
#' @title Load an UDPipe model
#' @description Load an UDPipe model
#' @param file full path to the model
#' @return An object of class \code{udpipe_model} which is a list with 2 elements: file containing the file and model which is 
#' an Rcpp-generated pointer to the loaded model which can be used in \code{\link{udpipe_annotate}}
#' @seealso \code{\link{udpipe_annotate}}
#' @details Pre-trained Universal Dependencies 2.0 models on all UD treebanks are made available at 
#' \url{https://ufal.mff.cuni.cz/udpipe}, namely at \url{https://lindat.mff.cuni.cz/repository/xmlui/handle/11234/1-2364}.
#' At the time of writing this consists of models made available on 50 languages, namely: 
#' ancient_greek, arabic, basque, belarusian, bulgarian, catalan, chinese, coptic, croatian, czech, danish, dutch, 
#' english, estonian, finnish, french, galician, german, gothic, greek, hebrew, hindi, hungarian, indonesian, irish, 
#' italian, japanese, kazakh, korean, latin, latvian, lithuanian, norwegian, old_church_slavonic, persian, polish, 
#' portuguese, romanian, russian, sanskrit, slovak, slovenian, spanish, swedish, tamil, turkish, ukrainian, 
#' urdu, uyghur, vietnamese. Mark that these models are made available under the CC BY-NC-SA 4.0 license.
#' @references \url{https://ufal.mff.cuni.cz/udpipe}, \url{https://lindat.mff.cuni.cz/repository/xmlui/handle/11234/1-2364}
#' @export
#' @examples 
#' ## Download zipped folder with models build on UD version 2.0 + get one model for Dutch
#' url <- file.path("https://lindat.mff.cuni.cz",
#'  "repository/xmlui/bitstream/handle/11234/1-2364/udpipe-ud-2.0-170801.zip")
#' url
#' \dontrun{
#' download.file(url, "udpipe-ud-2.0-170801.zip")
#' unzip("udpipe-ud-2.0-170801.zip", list = TRUE)
#' unzip("udpipe-ud-2.0-170801.zip", 
#'       files = "udpipe-ud-2.0-170801/dutch-ud-2.0-170801.udpipe", exdir = "dev")
#' 
#' f <- file.path(getwd(), "dev/udpipe-ud-2.0-170801/dutch-ud-2.0-170801.udpipe")
#' ## Load model
#' ud_dutch <- udpipe_load_model(f)
#' }
udpipe_load_model <- function(file) {
  file = path.expand(file)
  if(!file.exists(file)){
    stop(sprintf("File %s containing the language model does not exist", file))
  }
  if(basename(file) == file){
    stop(sprintf("You should provide the full path to the file %s, as in %s", file, file.path(getwd(), file)))
  }
  ptr <- udp_load_model(file)
  structure(
    list(file = file, model = ptr), 
    class = "udpipe_model")
}


#' @title Tokenise, Tag and Dependency Parsing Annotation on text
#' @description Tokenise, Tag and Dependency Parsing Annotation on text
#' @param object an object of class \code{udpipe_model} as returned by \code{\link{udpipe_load_model}}
#' @param x a character vector in UTF-8 encoding where each element of the character vector 
#' contains text which you like to tokenize, tag and perform dependency parsing.
#' @param doc_id an identifier of a document with the same length as \code{x}.
#' @param tokenizer a character string of length 1, which is either 'tokenizer' (default udpipe tokenisation)
#' or a character string with more complex tokenisation options 
#' as specified in \url{http://ufal.mff.cuni.cz/udpipe/users-manual} in which case \code{tokenizer} should be a character string where the options
#' are put after each other using the semicolon as separation.
#' @param tagger a character string of length 1, which is either 'default' (default udpipe POS tagging and lemmatisation)
#' or 'none' (no POS tagging and lemmatisation needed) or a character string with more complex tagging options 
#' as specified in \url{http://ufal.mff.cuni.cz/udpipe/users-manual} in which case \code{tagger} should be a character string where the options
#' are put after each other using the semicolon as separation.
#' @param parser a character string of length 1, which is either 'default' (default udpipe dependency parsing) or
#' 'none' (no dependency parsing needed) or a character string with more complex parsing options 
#' as specified in \url{http://ufal.mff.cuni.cz/udpipe/users-manual} in which case \code{parser} should be a character string where the options
#' are put after each other using the semicolon as separation.
#' @param ... currently not used
#' @return a list with 3 elements
#' \itemize{
#'  \item{x: }{The \code{x} character vector with text.}
#'  \item{conllu: }{A character vector of length 1 containing the annotated result of the annotation flow in CONLL-U format.
#'  This format is explained at \url{http://universaldependencies.org/format.html}}
#'  \item{error: }{A vector with the same length of \code{x} containing possible errors when annotating \code{x}}
#' }
#' @seealso \code{\link{udpipe_load_model}}
#' @references \url{https://ufal.mff.cuni.cz/udpipe}, \url{https://lindat.mff.cuni.cz/repository/xmlui/handle/11234/1-2364}, 
#' \url{http://universaldependencies.org/format.html}
#' @export
#' @examples 
#' \dontrun{
#' ## Load the model
#' f <- file.path(getwd(), "dev/udpipe-ud-2.0-170801/dutch-ud-2.0-170801.udpipe")
#' ud_dutch <- udpipe_load_model(f)
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
#' }
udpipe_annotate <- function(object, x, doc_id = paste("doc", seq_along(x), sep=""), 
                            tokenizer = "tokenizer", 
                            tagger = c("default", "none"), 
                            parser = c("default", "none"), ...) {
  if(!inherits(object, "udpipe_model")){
    stop("object should be of class udpipe_model as returned by the function ?udpipe_load_model")
  }
  stopifnot(inherits(x, "character"))
  stopifnot(inherits(doc_id, "character"))
  stopifnot(length(x) == length(doc_id))
  stopifnot(length(tagger) > 0)
  stopifnot(length(parser) > 0)
  stopifnot(is.character(tagger))
  stopifnot(is.character(parser))
  tagger <- tagger[1]
  parser <- parser[1]
  tagger <- match.arg(tagger)
  parser <- match.arg(parser)
  
  x_conllu <- udp_tokenise_tag_parse(object$model, x, doc_id, tokenizer, tagger, parser)
  class(x_conllu) <- "udpipe_connlu"
  x_conllu
}



#' @title Convert the result of udpipe_annotate to a tidy data frame
#' @description Convert the result of udpipe_annotate to a tidy data frame
#' @param x an object of class \code{udpipe_connlu} as returned by \code{\link{udpipe_annotate}}
#' @param ... currently not used
#' @return a data.frame with columns 
#' doc_id, paragraph_id, sentence_id, sentence_text, 
#' id, form, lemma, upostag, xpostag, feats, head, deprel, deps, misc)
#' @seealso \code{\link{udpipe_annotate}}
#' @export
#' @examples 
#' \dontrun{
#' ## Load the model
#' f <- file.path(getwd(), "dev/udpipe-ud-2.0-170801/dutch-ud-2.0-170801.udpipe")
#' ud_dutch <- udpipe_load_model(f)
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
#' as.data.frame(x)
#' }
as.data.frame.udpipe_connlu <- function(x, ...){
  ## R CMD check happyness
  doc_id <- paragraph_id <- token_id <- head_token_id <- lemma <- upos <- xpos <- feats <- dep_rel <- misc <- NULL

  ## Parse format of all lines in the CONLL-U format
  txt <- strsplit(x$conllu, "\n")[[1]]
  is_sentence_boundary <- txt == ""
  is_comment <- startsWith(txt, "#")
  is_newdoc <- startsWith(txt, "# newdoc")
  is_newparagraph <- startsWith(txt, "# newpar")
  is_sentenceid = startsWith(txt, "# sent_id")
  is_sentencetext = startsWith(txt, "# text")
  is_taggeddata <- !is_sentence_boundary & !is_comment
  
  out <- data.table::data.table(txt = txt,
                    doc_id = na_locf(ifelse(is_newdoc, sub("^# newdoc id = *", "", txt), NA_character_)),
                    sentence_id = as.integer(na_locf(ifelse(is_sentenceid, sub("^# sent_id = *", "", txt), NA_character_))),
                    sentence = na_locf(ifelse(is_sentencetext, sub("^# text = *", "", txt), NA_character_)),
                    is_newparagraph = is_newparagraph)
  underscore_as_na <- function(x, which_na = NA_character_){
    x[which(x == "_")] <- which_na
    x
  }
  out[, paragraph_id := cumsum(is_newparagraph), by = list(doc_id)]
  out <- out[is_taggeddata, ]
  out <- out[,  c("token_id", "token", "lemma", "upos", "xpos", "feats", "head_token_id", "dep_rel", "deps", "misc") := data.table::tstrsplit(txt, "\t", fixed=TRUE)]
  out[, token_id := as.integer(underscore_as_na(token_id, which_na = NA_integer_))]
  out[, head_token_id := as.integer(underscore_as_na(head_token_id, which_na = NA_integer_))]
  out[, lemma := underscore_as_na(lemma)]
  out[, upos := underscore_as_na(upos)]
  out[, xpos := underscore_as_na(xpos)]
  out[, feats := underscore_as_na(feats)]
  out[, dep_rel := underscore_as_na(dep_rel)]
  out[, deps := underscore_as_na(deps)]
  out[, misc := underscore_as_na(misc)]
  out <- out[, c("doc_id", "paragraph_id", "sentence_id", "sentence", 
                 "token_id", "token", "lemma", "upos", "xpos", "feats", "head_token_id", "dep_rel", "deps", "misc")]
  data.table::setDF(out)
  out
}
