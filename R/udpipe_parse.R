

#' @title Tokenising, Lemmatising, Tagging and Dependency Parsing Annotation of raw text
#' @description Tokenising, Lemmatising, Tagging and Dependency Parsing Annotation of raw text
#' @param object an object of class \code{udpipe_model} as returned by \code{\link{udpipe_load_model}}
#' @param x a character vector in UTF-8 encoding where each element of the character vector 
#' contains text which you like to tokenize, tag and perform dependency parsing.
#' @param doc_id an identifier of a document with the same length as \code{x}. This should be a character vector.
#' \code{doc_id[i]} corresponds to \code{x[i]}.
#' @param tokenizer a character string of length 1, which is either 'tokenizer' (default udpipe tokenisation)
#' or a character string with more complex tokenisation options 
#' as specified in \url{http://ufal.mff.cuni.cz/udpipe/1/users-manual} in which case \code{tokenizer} should be a character string where the options
#' are put after each other using the semicolon as separation.
#' @param tagger a character string of length 1, which is either 'default' (default udpipe POS tagging and lemmatisation)
#' or 'none' (no POS tagging and lemmatisation needed) or a character string with more complex tagging options 
#' as specified in \url{http://ufal.mff.cuni.cz/udpipe/1/users-manual} in which case \code{tagger} should be a character string where the options
#' are put after each other using the semicolon as separation.
#' @param parser a character string of length 1, which is either 'default' (default udpipe dependency parsing) or
#' 'none' (no dependency parsing needed) or a character string with more complex parsing options 
#' as specified in \url{http://ufal.mff.cuni.cz/udpipe/1/users-manual} in which case \code{parser} should be a character string where the options
#' are put after each other using the semicolon as separation.
#' @param trace A non-negative integer indicating to show progress on the annotation. 
#' If positive it prints out a message before each \code{trace} number of elements of \code{x} for which annotation is to be executed,
#' allowing you to see how much of the text is already annotated. Defaults to FALSE (no progress shown).
#' @param ... currently not used
#' @return a list with 3 elements
#' \itemize{
#'  \item{x: }{The \code{x} character vector with text.}
#'  \item{conllu: }{A character vector of length 1 containing the annotated result of the annotation flow in CONLL-U format.
#'  This format is explained at \url{https://universaldependencies.org/format.html}}
#'  \item{error: }{A vector with the same length of \code{x} containing possible errors when annotating \code{x}}
#' }
#' @seealso \code{\link{udpipe_load_model}}, \code{\link{as.data.frame.udpipe_connlu}}
#' @references \url{https://ufal.mff.cuni.cz/udpipe}, \url{https://lindat.mff.cuni.cz/repository/xmlui/handle/11234/1-2364}, 
#' \url{https://universaldependencies.org/format.html}
#' @export
#' @examples 
#' model    <- udpipe_download_model(language = "dutch-lassysmall")
#' if(!model$download_failed){
#' ud_dutch <- udpipe_load_model(model$file_model)
#' 
#' ## Tokenise, Tag and Dependency Parsing Annotation. Output is in CONLL-U format.
#' txt <- c("Dus. Godvermehoeren met pus in alle puisten, 
#'   zei die schele van Van Bukburg en hij had nog gelijk ook. 
#'   Er was toen dat liedje van tietenkonttieten kont tieten kontkontkont, 
#'   maar dat hoefden we geenseens niet te zingen. 
#'   Je kunt zeggen wat je wil van al die gesluierde poezenpas maar d'r kwam wel 
#'   een vleeswarenwinkel onder te voorschijn van heb je me daar nou.
#'   
#'   En zo gaat het maar door.",
#'   "Wat die ransaap van een academici nou weer in z'n botte pan heb gehaald mag 
#'   Joost in m'n schoen gooien, maar feit staat boven water dat het een gore 
#'   vieze vuile ransaap is.")
#' x <- udpipe_annotate(ud_dutch, x = txt)
#' cat(x$conllu)
#' as.data.frame(x)
#' 
#' ## Only tokenisation
#' x <- udpipe_annotate(ud_dutch, x = txt, tagger = "none", parser = "none")
#' as.data.frame(x)
#' 
#' ## Only tokenisation and POS tagging + lemmatisation, no dependency parsing
#' x <- udpipe_annotate(ud_dutch, x = txt, tagger = "default", parser = "none")
#' as.data.frame(x)
#' 
#' ## Only tokenisation and dependency parsing, no POS tagging nor lemmatisation
#' x <- udpipe_annotate(ud_dutch, x = txt, tagger = "none", parser = "default")
#' as.data.frame(x)
#' 
#' ## Provide doc_id for joining and identification purpose
#' x <- udpipe_annotate(ud_dutch, x = txt, doc_id = c("id1", "feedbackabc"),
#'                      tagger = "none", parser = "none", trace = TRUE)
#' as.data.frame(x)
#' 
#' ## Mark on encodings: if your data is not in UTF-8 encoding, make sure you convert it to UTF-8 
#' ## This can be done using iconv as follows for example
#' udpipe_annotate(ud_dutch, x = iconv('Ik drink melk bij mijn koffie.', to = "UTF-8"))
#' }
#' 
#' 
#' 
#' ## cleanup for CRAN only - you probably want to keep your model if you have downloaded it
#' if(file.exists(model$file_model)) file.remove(model$file_model)
udpipe_annotate <- function(object, x, doc_id = paste("doc", seq_along(x), sep=""), 
                            tokenizer = "tokenizer", 
                            tagger = c("default", "none"), 
                            parser = c("default", "none"), 
                            trace = FALSE, ...) {
  ## Some input checking
  if(!inherits(object, "udpipe_model")){
    stop("object should be of class udpipe_model as returned by the function ?udpipe_load_model")
  }
  check <- capture.output(object$model)
  if(check %in% "<pointer: (nil)>"){
    message(paste("This looks like you restarted your R session which has invalidated the model object, trying now to reload the model again from the file at", object$file, "in order to do the annotation."))
    object <- udpipe_load_model(object$file)
  }
  if(inherits(x, "factor")){
    x <- as.character(x)
  }
  stopifnot(inherits(x, "character"))
  if(!inherits(doc_id, "character")){
    doc_id <- as.character(doc_id)
  }
  stopifnot(length(x) == length(doc_id))
  stopifnot(length(tagger) > 0)
  stopifnot(length(parser) > 0)
  stopifnot(is.character(tagger))
  stopifnot(is.character(parser))
  tagger <- tagger[1]
  parser <- parser[1]
  log_every <- as.integer(trace)
  log_now <- function(){
    as.character(Sys.time())
  }
  ## Annotate
  x_conllu <- udp_tokenise_tag_parse(object$model, x, doc_id, tokenizer, tagger, parser, log_every, log_now)
  Encoding(x_conllu$conllu) <- "UTF-8"
  class(x_conllu) <- "udpipe_connlu"
  x_conllu
}



#' @title Convert the result of udpipe_annotate to a tidy data frame
#' @description Convert the result of \code{\link{udpipe_annotate}} to a tidy data frame
#' @param x an object of class \code{udpipe_connlu} as returned by \code{\link{udpipe_annotate}}
#' @param ... currently not used
#' @return a data.frame with columns doc_id, paragraph_id, sentence_id, sentence, 
#' token_id, token, lemma, upos, xpos, feats, head_token_id, dep_rel, deps, misc \cr
#' 
#' The columns paragraph_id, sentence_id are integers, the other fields
#' are character data in UTF-8 encoding. \cr
#' 
#' To get more information on these fields, visit \url{https://universaldependencies.org/format.html} 
#' or look at \code{\link{udpipe}}.
#' @seealso \code{\link{udpipe_annotate}}
#' @export
#' @examples 
#' model    <- udpipe_download_model(language = "dutch-lassysmall")
#' 
#' if(!model$download_failed){
#' 
#' ud_dutch <- udpipe_load_model(model$file_model)
#' txt <- c("Ik ben de weg kwijt, kunt u me zeggen waar de Lange Wapper ligt? Jazeker meneer", 
#'          "Het gaat vooruit, het gaat verbazend goed vooruit")
#' x <- udpipe_annotate(ud_dutch, x = txt)
#' x <- as.data.frame(x)
#' head(x)
#' 
#' }
#' 
#' ## cleanup for CRAN only - you probably want to keep your model if you have downloaded it
#' if(file.exists(model$file_model)) file.remove(model$file_model)
as.data.frame.udpipe_connlu <- function(x, ...){
  read_connlu(x, is_udpipe_annotation = TRUE, ...)
}

#' @export
#' @rdname as.data.table.udpipe_connlu
as.data.table.udpipe_connlu <- function(x, ...){
  out <- as.data.frame(x)
  out <- data.table::setDT(out)
  out
}


read_connlu <- function(x, is_udpipe_annotation = FALSE, ...){
  ## R CMD check happyness
  doc_id <- paragraph_id <- sentence_id <- token_id <- head_token_id <- token <- lemma <- upos <- xpos <- feats <- dep_rel <- deps <- misc <- term_id <- .N <- NULL
  
  output_fields <- c("doc_id", "paragraph_id", "sentence_id", "sentence", 
                     "token_id", "token", "lemma", "upos", "xpos", "feats", "head_token_id", "dep_rel", "deps", "misc")
  ## Undocumented feature
  ldots <- list(...)
  if(any(c("rich", "full", "enhanced", "detailed") %in% names(ldots))){
    ldots$term_id <- TRUE
    ldots$start_end <- TRUE
  }
  if("term_id" %in% names(ldots)){
    if(isTRUE(ldots$term_id)){
      output_fields <- append(output_fields, values = "term_id", after = 4)
    }
  }
  if("start_end" %in% names(ldots)){
    if(isTRUE(ldots$start_end)){
      output_fields <- append(output_fields, values = c("start", "end"), after = 4)
    }
  }
  ## Default output 
  default <- data.frame(doc_id = character(), 
                        paragraph_id = integer(), 
                        sentence_id = character(), 
                        sentence = character(),
                        start = integer(),
                        end = integer(),
                        term_id = integer(),
                        token_id = character(),
                        token = character(), 
                        lemma = character(), 
                        upos = character(), 
                        xpos = character(), 
                        feats = character(), 
                        head_token_id = character(), 
                        dep_rel = character(), 
                        deps = character(), 
                        misc = character(), stringsAsFactors = FALSE)
  default <- default[, output_fields]
  
  if(is_udpipe_annotation){
    default$sentence_id <- as.integer(default$sentence_id)
  }
  ## Check if there is data in x$conllu
  if(length(x$conllu) <= 1){
    if(all(x$conllu == "")){
      msg <- unique(x$error)
      if(length(msg) == 0){
        msg <- ""
      }else{
        msg <- paste(msg, collapse = ", ")
      }
      warning(sprintf("No parsed data in x$conllu, returning default empty data.frame. Error message at x$error indicates e.g.: %s", msg))
      
      return(default)
    }
  }
  
  ## Parse format of all lines in the CONLL-U format
  if(!exists("startsWith", envir = baseenv())){
    ## Function only from R version 3.3.0
    startsWith <- function(x, prefix){
      prefix <- paste("^", prefix, sep = "")
      grepl(pattern = prefix, x = x)
    }
  }
  txt <- strsplit(x$conllu, "\n")[[1]]
  is_sentence_boundary <- txt == ""
  is_comment <- startsWith(txt, "#")
  is_newdoc <- startsWith(txt, "# newdoc")
  is_newparagraph <- startsWith(txt, "# newpar")
  is_sentenceid <- startsWith(txt, "# sent_id")
  is_sentencetext <- startsWith(txt, "# text")
  is_taggeddata <- !is_sentence_boundary & !is_comment
  
  out <- data.table::data.table(txt = txt,
                    doc_id = na_locf(ifelse(is_newdoc, sub("^# newdoc id = *", "", txt), NA_character_)),
                    sentence_id = na_locf(ifelse(is_sentenceid, sub("^# sent_id = *", "", txt), NA_character_)),
                    sentence = na_locf(ifelse(is_sentencetext, sub("^# text = *", "", txt), NA_character_)),
                    is_newparagraph = is_newparagraph)
  if(is_udpipe_annotation){
    out$sentence_id <- as.integer(out$sentence_id)
  }
  underscore_as_na <- function(x, which_na = NA_character_){
    x[which(x == "_")] <- which_na
    x
  }
  out[, paragraph_id := cumsum(is_newparagraph), by = list(doc_id)]
  out <- out[is_taggeddata, ]
  if("term_id" %in% output_fields){
    out[, term_id := 1L:.N, by = list(doc_id)]
  }
  out <- out[,  c("token_id", "token", "lemma", "upos", "xpos", "feats", "head_token_id", "dep_rel", "deps", "misc") := data.table::tstrsplit(txt, "\t", fixed=TRUE)]
  out[, token_id := underscore_as_na(token_id)]
  out[, head_token_id := underscore_as_na(head_token_id)]
  out[, lemma := underscore_as_na(lemma)]
  out[, upos := underscore_as_na(upos)]
  out[, xpos := underscore_as_na(xpos)]
  out[, feats := underscore_as_na(feats)]
  out[, dep_rel := underscore_as_na(dep_rel)]
  out[, deps := underscore_as_na(deps)]
  out[, misc := underscore_as_na(misc)]
  if(all(c("start", "end") %in% output_fields)){
    out[, c("start", "end") := udpipe_reconstruct(sentence_id = sentence_id, token = token, token_id = token_id, misc = misc, only_from_to = TRUE), 
        by = list(doc_id)]
  }
  out <- out[, output_fields, with = FALSE]
  out <- data.table::setDF(out)
  out
}




#' @title Tokenising, Lemmatising, Tagging and Dependency Parsing of raw text in TIF format
#' @description Tokenising, Lemmatising, Tagging and Dependency Parsing of raw text in TIF format
#' @param x either
#' \itemize{
#'  \item{a character vector: }{The character vector contains the text you want to tokenize, lemmatise, tag and perform dependency parsing. The names of the character vector indicate the document identifier.}
#'  \item{a data.frame with columns doc_id and text: }{The text column contains the text you want to tokenize, lemmatise, tag and perform dependency parsing. The doc_id column indicate the document identifier.}
#'  \item{a list of tokens: }{If you have already a tokenised list of tokens and you want to enrich it by lemmatising, tagging and performing dependency parsing. The names of the list indicate the document identifier.}
#' }
#' All text data should be in UTF-8 encoding
#' @param object either an object of class \code{udpipe_model} as returned by \code{\link{udpipe_load_model}},
#' the path to the file on disk containing the udpipe model or the language as defined by \code{\link{udpipe_download_model}}. 
#' If the language is provided, it will download the model using \code{\link{udpipe_download_model}}.
#' @param parallel.cores integer indicating the number of parallel cores to use to speed up the annotation. Defaults to 1 (use only 1 single thread). \cr
#' If more than 1 is specified, it uses parallel::mclapply (unix) or parallel::clusterApply (windows) to run annotation in parallel. In order to do this on Windows it runs first parallel::makeCluster to set up a local socket cluster, on unix it just uses forking to parallelise the annotation.\cr
#' Only set this if you have more than 1 CPU at disposal and you have large amount of data to annotate as setting up a parallel backend also takes some time plus 
#' annotations will run in chunks set by \code{parallel.chunksize} and for each parallel chunk the udpipe model will be loaded which takes also some time.\cr
#' If \code{parallel.cores} is bigger than 1 and \code{object} is of class \code{udpipe_model}, it will load the corresponding file from the model again in each parallel chunk.
#' @param parallel.chunksize integer with the size of the chunks of text to be annotated in parallel. If not provided, defaults to the size of \code{x} divided by \code{parallel.cores}. Only used in case \code{parallel.cores} is bigger than 1.
#' @param ... other elements to pass on to \code{\link{udpipe_annotate}} and \code{\link{udpipe_download_model}}
#' @return a data.frame with one row per doc_id and term_id containing all the tokens in the data, the lemma, the part of speech tags,
#' the morphological features and the dependency relationship along the tokens. The data.frame has the following fields:
#' \itemize{
#'  \item{doc_id: }{The document identifier.}
#'  \item{paragraph_id: }{The paragraph identifier which is unique within each document.}
#'  \item{sentence_id: }{The sentence identifier which is unique within each document.}
#'  \item{sentence: }{The text of the sentence of the sentence_id.}
#'  \item{start: }{Integer index indicating in the original text where the token starts. Missing in case of tokens part of multi-word tokens which are not in the text.}
#'  \item{end: }{Integer index indicating in the original text where the token ends. Missing in case of tokens part of multi-word tokens which are not in the text.}
#'  \item{term_id: }{A row identifier which is unique within the doc_id identifier.}
#'  \item{token_id: }{Token index, integer starting at 1 for each new sentence. May be a range for multiword tokens or a decimal number for empty nodes.}
#'  \item{token: }{The token.}
#'  \item{lemma: }{The lemma of the token.}
#'  \item{upos: }{The universal parts of speech tag of the token. See \url{https://universaldependencies.org/format.html}}
#'  \item{xpos: }{The treebank-specific parts of speech tag of the token. See \url{https://universaldependencies.org/format.html}}
#'  \item{feats: }{The morphological features of the token, separated by |. See \url{https://universaldependencies.org/format.html}}
#'  \item{head_token_id: }{Indicating what is the token_id of the head of the token, indicating to which other token in the sentence it is related. See \url{https://universaldependencies.org/format.html}}
#'  \item{dep_rel: }{The type of relation the token has with the head_token_id. See \url{https://universaldependencies.org/format.html}}
#'  \item{deps: }{Enhanced dependency graph in the form of a list of head-deprel pairs. See \url{https://universaldependencies.org/format.html}}
#'  \item{misc: }{SpacesBefore/SpacesAfter/SpacesInToken spaces before/after/inside the token. Used to reconstruct the original text. See \url{http://ufal.mff.cuni.cz/udpipe/1/users-manual}}
#' }
#' The columns paragraph_id, sentence_id, term_id, start, end are integers, the other fields
#' are character data in UTF-8 encoding. \cr
#' 
#' @seealso \code{\link{udpipe_load_model}}, \code{\link{as.data.frame.udpipe_connlu}}, \code{\link{udpipe_download_model}}, \code{\link{udpipe_annotate}}
#' @references \url{https://ufal.mff.cuni.cz/udpipe}, \url{https://lindat.mff.cuni.cz/repository/xmlui/handle/11234/1-2364}, 
#' \url{https://universaldependencies.org/format.html}
#' @export
#' @examples 
#' model    <- udpipe_download_model(language = "dutch-lassysmall")
#' if(!model$download_failed){
#' ud_dutch <- udpipe_load_model(model)
#' 
#' ## Tokenise, Tag and Dependency Parsing Annotation. Output is in CONLL-U format.
#' txt <- c("Dus. Godvermehoeren met pus in alle puisten, 
#'   zei die schele van Van Bukburg en hij had nog gelijk ook. 
#'   Er was toen dat liedje van tietenkonttieten kont tieten kontkontkont, 
#'   maar dat hoefden we geenseens niet te zingen. 
#'   Je kunt zeggen wat je wil van al die gesluierde poezenpas maar d'r kwam wel 
#'   een vleeswarenwinkel onder te voorschijn van heb je me daar nou.
#'   
#'   En zo gaat het maar door.",
#'   "Wat die ransaap van een academici nou weer in z'n botte pan heb gehaald mag 
#'   Joost in m'n schoen gooien, maar feit staat boven water dat het een gore 
#'   vieze vuile ransaap is.")
#' names(txt) <- c("document_identifier_1", "we-like-ilya-leonard-pfeiffer")
#' 
#' ##
#' ## TIF tagging: tag if x is a character vector, a data frame or a token sequence
#' ##
#' x <- udpipe(txt, object = ud_dutch)
#' x <- udpipe(data.frame(doc_id = names(txt), text = txt, stringsAsFactors = FALSE), 
#'             object = ud_dutch)
#' x <- udpipe(strsplit(txt, "[[:space:][:punct:][:digit:]]+"), 
#'             object = ud_dutch)
#'        
#' ## You can also directly pass on the language in the call to udpipe
#' x <- udpipe("Dit werkt ook.", object = "dutch-lassysmall")
#' x <- udpipe(txt, object = "dutch-lassysmall")
#' x <- udpipe(data.frame(doc_id = names(txt), text = txt, stringsAsFactors = FALSE), 
#'             object = "dutch-lassysmall")
#' x <- udpipe(strsplit(txt, "[[:space:][:punct:][:digit:]]+"), 
#'             object = "dutch-lassysmall")
#' }
#'             
#' ## cleanup for CRAN only - you probably want to keep your model if you have downloaded it
#' if(file.exists(model$file_model)) file.remove(model$file_model)
udpipe <- function(x, object, parallel.cores = 1L, parallel.chunksize, ...) {
  parallel.cores <- as.integer(parallel.cores)
  if(is.data.frame(x)){
    size <- nrow(x)
  }else if(is.list(x) || is.character(x)){
    size <- length(x)
  }else{
    stop("x should either be a data.frame, a list or a character vector")
  }
  if(!.Platform$OS.type %in% c("unix", "windows")){
    parallel.cores <- 1L
  }
  ##
  ## RUN IN PARALLEL if we have data and parallel.cores is bigger than 1, else just run single-threaded
  ##
  if(parallel.cores > 1 && size > 0){
    if(is.list(x) || is.character(x)){
      if(is.null(names(x))){
        names(x) <- seq_len(size)
      }
    }
    #requireNamespace(package = "parallel")
    if(inherits(object, "udpipe_model")){
      stopifnot(file.exists(object$file))
      object <- object$file
    }
    if(missing(parallel.chunksize)){
      parallel.chunksize <- ceiling(size / parallel.cores)
    }
    parallel.chunksize <- as.integer(parallel.chunksize)
    stopifnot(parallel.chunksize >= 0)
    
    chunks <- rep(seq_len(ceiling(size / parallel.chunksize)), length.out = size)
    chunks <- sort(chunks, decreasing = FALSE)
    anno <- split(x, f = chunks)
    if(.Platform$OS.type %in% c("unix")) {
      anno <- parallel::mclapply(anno, 
                                 FUN = function(x, object, parallel.chunksize, ...){
                                   udpipe::udpipe(x, object, parallel.cores = 1, parallel.chunksize = parallel.chunksize, ...) 
                                 }, 
                                 object = object,
                                 parallel.chunksize = parallel.chunksize, 
                                 mc.cores = parallel.cores, 
                                 ...)
    }else if(.Platform$OS.type %in% c("windows")){
      cl   <- parallel::makeCluster(parallel.cores)
      on.exit(parallel::stopCluster(cl))
      anno <- parallel::parLapply(cl = cl, 
                                  X = anno, 
                                  fun = function(x, object, parallel.chunksize, ...){
                                    udpipe::udpipe(x, object, parallel.cores = 1, parallel.chunksize = parallel.chunksize,...) 
                                  }, object = object, parallel.chunksize = parallel.chunksize, ...)
      
    }else{
      stop(paste("unknown platform:", .Platform$OS.type))
    }
    anno <- data.table::rbindlist(anno)
    anno <- data.table::setDF(anno)
  }else{
    UseMethod("udpipe")
  }
}


getmodel <- function(object, ...){
  ## It already a loaded udpipe model
  if(inherits(object, "udpipe_model")){
    return(object)
  }
  ## It's a data.frame returned by udpipe_download_model
  ## Load the model, but only if it was not the already loaded
  if(is.data.frame(object) && nrow(object) == 1 && "file_model" %in% colnames(object)){
    if(object$file_model %in% names(.loaded_models)){
      return(getmodel(.loaded_models[[object$file_model]]))
    }else{
      return(udpipe_load_model(object))  
    }
  }
  ## It's just the path to the udpipe model
  if(file.exists(object)){
    if(object %in% names(.loaded_models)){
      return(getmodel(.loaded_models[[object]]))
    }else{
      return(udpipe_load_model(object))
    }
  }
  ## It's the language
  ## Download if needed, and check again if it is the same model which was already loaded
  getmodel(udpipe_download_model(object, overwrite = FALSE, ...))
}

#' @export
udpipe.character <- function(x, object, ...){
  udmodel <- getmodel(object, ...)
  if(length(names(x)) == 0){
    x <- udpipe_annotate(udmodel, x = x, ...)  
  }else{
    x <- udpipe_annotate(udmodel, x = x, doc_id = names(x), ...)
  }
  x <- as.data.frame(x, detailed = TRUE)
  x
}

#' @export
udpipe.data.frame <- function(x, object, ...){
  udmodel <- getmodel(object, ...)
  stopifnot(all(c("doc_id", "text") %in% colnames(x)))
  x <- udpipe_annotate(udmodel, x = x$text, doc_id = x$doc_id, ...)
  x <- as.data.frame(x, detailed = TRUE)
  x
}

#' @export
udpipe.list <- function(x, object, ...){
  udmodel <- getmodel(object, ...)
  x <- x[sapply(x, FUN=function(x) length(x) > 0)]
  x <- udpipe_annotate(udmodel, 
                       x = sapply(x, FUN=function(x) paste(x, collapse = "\n")), 
                       doc_id = names(x), tokenizer = "vertical", ...)
  x <- as.data.frame(x, detailed = TRUE)
}
