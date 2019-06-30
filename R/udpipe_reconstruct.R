if(FALSE){
  library(udpipe)
  txt <- "  Maxime  y su mujer\\ hicieron que nuestra estancia 
    fuera lo mas comoda posible. \n  
    El primer dia Maxime nos espero hasta tarde para recibirnos y 
    darnos todas las indicaciones posibles del apartamento y 
    de la situacion de aparcamiento en el barrio ya que fuimos 
    desde Espana con el coche ( es todo zona azul de 9:00 a 18:00 pero 
    como saliamos pronto y llegabamos tarde no nos afectaba).\n  
    El apartamento es muy completo, la verdad es como aparece 
    en el anuncio, es mas, incluso tiene una barandilla 
    en la escaleras que dan a la habitacion que en la foto no sale.\n   
    El jardin esta muy bien para desayunar o cenar ya que 
    tiene una mesa grande para ello.\n   
    El barrio es muy tranquilo con bastantes tiendas y restaurantes.\n
       En general estuvimos muy comodos durante nuestra estancia, 
       repetiriamos ahora mismo.\n   Muchas gracias por todo Maxime.  "
  ud_model <- udpipe_download_model(language = "spanish")
  ud_model <- udpipe_load_model(ud_model$file_model)
  x <- udpipe_annotate(ud_model, x = txt)
  x <- as.data.frame(x, detailed = TRUE)
  original <- udpipe_reconstruct(sentence_id = x$sentence_id, token = x$token, token_id = x$token_id, misc = x$misc)
}


udpipe_reconstruct <- function(sentence_id, token, token_id, misc, only_from_to = FALSE){

  ##
  ## FROM THE UDPIPE DOCS: 
  ##
  
  # The markup uses the following MISC fields on tokens (not words in multi-word tokens):
  # SpacesBefore=content (by default empty): spaces/other content preceding the token
  # SpacesAfter=content (by default a space if SpaceAfter=No feature is not present, empty otherwise): spaces/other content following the token
  # SpacesInToken=content (by default equal to the FORM of the token): FORM of the token including original spaces (this is needed only if tokens are allowed to contain spaces and a token contains a tab or newline characters)
  
  # The content of all the three fields must be escaped to allow storing tabs and newlines. The following C-like schema is used:
  # \s: space
  # \t: tab
  # \r: CR character
  # \n: LF character
  # \p: | (pipe character)
  # \\: \ (backslash character)

  rawtxt <- token
  
  has_spacesafter_no <- grepl(pattern = "SpaceAfter=No", misc)
  has_spacesafter    <- grepl(pattern = "SpacesAfter=", misc)
  has_spacesbefore   <- grepl(pattern = "SpacesBefore=", misc)
  has_spacesintoken  <- grepl(pattern = "SpacesInToken=", misc)
  
  ##
  ## Spaces after
  ##
  after <- rep("", length(token))
  ## if no spaceafter feature, there is a space
  after[!has_spacesafter] <- " "
  ## if missing, there is a space after
  after[is.na(misc)] <- " "
  ## if contains SpaceAfter=No, there is nothing to add
  after[has_spacesafter_no] <- ""
  ## if contains SpacesAfter=, add the spaces to the after part
  idx <- which(has_spacesafter)
  #addme <- gsub(pattern = "(SpacesAfter=)(.+)($|Spaces)", "\\2", misc[idx])
  addme <- sapply(strsplit(misc[idx], split = "\\|"), FUN=function(x) grep(pattern = "SpacesAfter", x = x, value = TRUE))
  addme <- gsub(pattern = "SpacesAfter=", replacement = "", addme)
  addme <- gsub("\\\\s", " ", addme)
  addme <- gsub("\\\\n", "\n", addme)
  addme <- gsub("\\\\t", "\t", addme)
  addme <- gsub("\\\\r", "\r", addme)
  addme <- gsub("\\\\p", "|", addme)
  addme <- gsub("\\\\", "\\", addme)
  after[idx] <- addme
  ## Fix for using std::istringstream in udpipe_annotate as it always ends with a newline character
  after[length(after)] <- gsub("\n$", "", after[length(after)])
  
  ##
  ## Spaces before
  ##
  before <- rep("", length(token))
  ## if contains SpacesBefore=, add the spaces to the after part
  idx <- which(has_spacesbefore)
  #addme <- gsub(pattern = "(SpacesBefore=)(.+)($|Spaces)", "\\2", misc[idx])
  addme <- sapply(strsplit(misc[idx], split = "\\|"), FUN=function(x) grep(pattern = "SpacesBefore", x = x, value = TRUE))
  addme <- gsub(pattern = "SpacesBefore=", replacement = "", addme)
  addme <- gsub("\\\\s", " ", addme)
  addme <- gsub("\\\\n", "\n", addme)
  addme <- gsub("\\\\t", "\t", addme)
  addme <- gsub("\\\\r", "\r", addme)
  addme <- gsub("\\\\p", "|", addme)
  addme <- gsub("\\\\", "\\", addme)
  before[idx] <- addme
  
  ##
  ## SpacesInToken - MISC field stores form of the token including original spaces if there is a space in the token which can not be handled by FORM
  ##
  idx <- which(has_spacesintoken)
  #token[idx] <- gsub(pattern = "(SpacesInToken=)(.+)($|Spaces)", "\\2", misc[idx])
  addme <- sapply(strsplit(misc[idx], split = "\\|"), FUN=function(x) grep(pattern = "SpacesInToken", x = x, value = TRUE))
  addme <- gsub(pattern = "SpacesInToken=", replacement = "", addme)
  token[idx] <- addme
  
  ##
  ## Construct original text
  ##
  original_txt <- sprintf("%s%s%s", before, token, after)
  
  ##
  ## Multi-word tokens are not considered
  ##
  is_multi_word <- grepl("-", token_id)
  ids <- sprintf("%s.%s", sentence_id, token_id)
  ids_remove <- mapply(sentence_id = sentence_id[is_multi_word],
                       token_id = token_id[is_multi_word], 
                       FUN=function(sentence_id, token_id){
                         sprintf("%s.%s", sentence_id, unlist(strsplit(token_id, split = "-")))
                         }, SIMPLIFY = TRUE, USE.NAMES = FALSE)
  idx <- which(ids %in% ids_remove)
  original_txt[idx] <- ""
  
  ##
  ## Construct from-to
  ##
  before[idx] <- ""
  after[idx] <- ""
  
  nchars <- nchar(original_txt)
  original_to <- cumsum(nchars)
  original_from <- original_to - nchars + 1L
  from <- original_from + nchar(before)
  to <- original_to - nchar(after)
  from[idx] <- NA_integer_
  to[idx] <- NA_integer_

  
  if(only_from_to){
    return(list(from = from, to = to))  
  }else{
    return(list(text = paste(original_txt, collapse = ""),
                from = from,
                to = to))  
  }
}