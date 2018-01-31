#' @title Train a UDPipe model
#' @description Train a UDPipe model which allows to do 
#' Tokenization, Parts of Speech Tagging, Lemmatization and Dependency Parsing or a combination of those. \cr
#' 
#' This function allows you to build models based on data in in CONLL-U format
#' as described at \url{http://universaldependencies.org/format.html}. At the time of writing open data in CONLL-U
#' format for 50 languages are available at \url{http://universaldependencies.org/#ud-treebanks}. 
#' Most of these are distributed under the CC-BY-SA licence or the CC-BY-NC-SA license. \cr
#' 
#' This function allows to build annotation tagger models based on these data in CONLL-U format, allowing you 
#' to have your own tagger model. This is relevant if you want to tune the tagger to your needs
#' or if you don't want to use ready-made models provided under the CC-BY-NC-SA license as shown at \code{\link{udpipe_load_model}}
#' @param file full path where the model will be saved. The model will be stored as a binary file which \code{\link{udpipe_load_model}}
#' can handle. Defaults to 'my_annotator.udpipe' in the current working directory. 
#' @param files_conllu_training a character vector of files in CONLL-U format used for training the model
#' @param files_conllu_holdout a character vector of files in CONLL-U format used for holdout evalution of the model. This argument is optional.
#' @param annotation_tokenizer a string containing options for the tokenizer. This can be either 'none' or 'default' or a list 
#' of options as mentioned in the UDPipe manual. See the vignette \code{vignette("udpipe-train", package = "udpipe")} or
#' go directly to \url{http://ufal.mff.cuni.cz/udpipe/users-manual#model_training_tokenizer} for a full description of the options
#' or see the examples below.
#' Defaults to 'default'. If you specify 'none', the model will not be able to perform tokenization.
#' @param annotation_tagger a string containing options for the pos tagger and lemmatiser. This can be either 'none' or 'default' or a list 
#' of options as mentioned in the UDPipe manual. See the vignette \code{vignette("udpipe-train", package = "udpipe")} or
#' go directly to \url{http://ufal.mff.cuni.cz/udpipe/users-manual#model_training_tagger} for a full description of the options
#' or see the examples below.
#' Defaults to 'default'. If you specify 'none', the model will not be able to perform POS tagging or lemmatization.
#' @param annotation_parser a string containing options for the dependency parser.  This can be either 'none' or 'default' or a list 
#' of options as mentioned in the UDPipe manual. See the vignette \code{vignette("udpipe-train", package = "udpipe")} or
#' go directly to \url{http://ufal.mff.cuni.cz/udpipe/users-manual#model_training_parser} for a full description of the options
#' or see the examples below.
#' Defaults to 'default'. If you specify 'none', the model will not be able to perform dependency parsing.
#' @return A list with elements 
#' \itemize{
#'  \item{file: }{The path to the model, which can be used in \code{udpipe_load_model}}
#'  \item{annotation_tokenizer: }{The input argument \code{annotation_tokenizer}}
#'  \item{annotation_tagger: }{The input argument \code{annotation_tagger}}
#'  \item{annotation_parser: }{The input argument \code{annotation_parser}}
#'  \item{errors: }{Messages from the UDPipe process indicating possible errors for example when passing the wrong arguments to the 
#'  annotation_tokenizer, annotation_tagger or annotation_parser}
#' }
#' @seealso \code{\link{udpipe_annotation_params}}, \code{\link{udpipe_annotate}}, \code{\link{udpipe_load_model}},
#' \code{\link{udpipe_accuracy}}
#' @references \url{http://ufal.mff.cuni.cz/udpipe/users-manual}
#' @details 
#' In order to train a model, you need to provide files which are in CONLL-U format in argument \code{files_conllu_training}. 
#' This can be a vector of files or just one file. If you do not have your own CONLL-U files, you can download files for your language of 
#' choice at \url{http://universaldependencies.org/#ud-treebanks}. \cr
#' 
#' At the time of writing open data in CONLL-U
#' format for 50 languages are available at \url{http://universaldependencies.org/#ud-treebanks}, namely for: 
#' ancient_greek, arabic, basque, belarusian, bulgarian, catalan, chinese, coptic, croatian, 
#' czech, danish, dutch, english, estonian, finnish, french, galician, german, gothic, greek, hebrew, hindi, hungarian, 
#' indonesian, irish, italian, japanese, kazakh, korean, latin, latvian, lithuanian, norwegian, 
#' old_church_slavonic, persian, polish, portuguese, romanian, russian, sanskrit, slovak, 
#' slovenian, spanish, swedish, tamil, turkish, ukrainian, urdu, uyghur, vietnamese. 
#' @export
#' @examples 
#' ## You need to have a file on disk in CONLL-U format, taking the toy example file put in the package
#' file_conllu <- system.file(package = "udpipe", "dummydata", "traindata.conllu")
#' file_conllu
#' cat(head(readLines(file_conllu), 3), sep="\n")
#' 
#' \dontrun{
#' ##
#' ## This is a toy example showing how to build a model, it is not a good model whatsoever, 
#' ##   because model building takes more than 5 seconds this model is saved also in 
#' ##   the file at system.file(package = "udpipe", "dummydata", "toymodel.udpipe")
#' ##
#' m <- udpipe_train(file = "toymodel.udpipe", files_conllu_training = file_conllu, 
#'   annotation_tokenizer = list(dimension = 16, epochs = 1, batch_size = 100, dropout = 0.7), 
#'   annotation_tagger = list(iterations = 1, models = 1, 
#'      provide_xpostag = 1, provide_lemma = 0, provide_feats = 0, 
#'      guesser_suffix_rules = 2, guesser_prefix_min_count = 2), 
#'   annotation_parser = list(iterations = 2, 
#'      embedding_upostag = 20, embedding_feats = 20, embedding_xpostag = 0, embedding_form = 50, 
#'      embedding_lemma = 0, embedding_deprel = 20, learning_rate = 0.01, 
#'      learning_rate_final = 0.001, l2 = 0.5, hidden_layer = 200, 
#'      batch_size = 10, transition_system = "projective", transition_oracle = "dynamic", 
#'      structured_interval = 10))
#' }
#' 
#' file_model <- system.file(package = "udpipe", "dummydata", "toymodel.udpipe")
#' ud_toymodel <- udpipe_load_model(file_model)
#' x <- udpipe_annotate(object = ud_toymodel, x = "Ik ging deze morgen naar de bakker brood halen.")
#' x <- as.data.frame(x)
#' 
#' ##
#' ## The above was a toy example showing how to build a model, if you want real-life scenario's
#' ## look at the training parameter examples given below and train it on your CONLL-U file
#' ##
#' ## Example training arguments used for the models available at udpipe_download_model
#' data(udpipe_annotation_params)
#' head(udpipe_annotation_params$tokenizer)
#' head(udpipe_annotation_params$tagger)
#' head(udpipe_annotation_params$parser)
#' \dontrun{
#' ## More details in the package vignette:
#' vignette("udpipe-train", package = "udpipe")
#' }
udpipe_train <- function(file = file.path(getwd(), "my_annotator.udpipe"), 
                         files_conllu_training, files_conllu_holdout = character(), 
                         annotation_tokenizer = "default", 
                         annotation_tagger = "default", 
                         annotation_parser = "default") {
  
  collapse_list <- function(x){
    if(is.list(x)){
      x <- mapply(names(x), x, FUN=function(key, value) sprintf("%s=%s", key, value), SIMPLIFY = TRUE, USE.NAMES = FALSE)
      x <- paste(x, collapse = ";")  
    }
    x
  }
  annotation_tokenizer <- collapse_list(annotation_tokenizer)
  annotation_tagger <- collapse_list(annotation_tagger)
  annotation_parser <- collapse_list(annotation_parser)
  
  file <- path.expand(file)
  files_conllu_training <- path.expand(files_conllu_training)
  files_conllu_holdout <- path.expand(files_conllu_holdout)
  if(!dir.exists(dirname(file))){
    dir.create(dirname(file), recursive = TRUE)
  }
  stopifnot(all(file.exists(files_conllu_training)))
  stopifnot(all(file.exists(files_conllu_holdout)))
  result <- udp_train(file, files_conllu_training, files_conllu_holdout, 
            annotation_tokenizer, annotation_tagger, annotation_parser)

  structure(
    list(file_model = result$file_model, 
         annotation_tokenizer = annotation_tokenizer,
         annotation_tagger = annotation_tagger,
         annotation_parser = annotation_parser,
         errors = result$errors,
    class = "udpipe_trained_model"))
}



#' @title Evaluate the accuracy of your UDPipe model on holdout data
#' @description Get precision, recall and F1 measures on finding words / sentences / upos / xpos / features
#' annotation as well as UAS and LAS dependency scores on holdout data in conllu format.
#' @param object an object of class \code{udpipe_model} as returned by \code{\link{udpipe_load_model}}
#' @param file_conllu the full path to a file on disk containing holdout data in conllu format
#' @param tokenizer a character string of length 1, which is either 'default' or 'none'
#' @param tagger a character string of length 1, which is either 'default' or 'none'
#' @param parser a character string of length 1, which is either 'default' or 'none'
#' @return a list with 3 elements
#' \itemize{
#'  \item{accuracy: }{A character vector with accuracy metrics.}
#'  \item{error: }{A character string with possible errors when calculating the accuracy metrics}
#' }
#' @seealso \code{\link{udpipe_load_model}}
#' @references \url{https://ufal.mff.cuni.cz/udpipe}, 
#' \url{http://universaldependencies.org/format.html}
#' @export
#' @examples 
#' x <- udpipe_download_model(language = "dutch-lassysmall")
#' ud_dutch <- udpipe_load_model(x$file_model)
#' 
#' file_conllu <- system.file(package = "udpipe", "dummydata", "traindata.conllu")
#' metrics <- udpipe_accuracy(ud_dutch, file_conllu)
#' metrics$accuracy
#' metrics <- udpipe_accuracy(ud_dutch, file_conllu, 
#'                            tokenizer = "none", tagger = "default", parser = "default")
#' metrics$accuracy
#' metrics <- udpipe_accuracy(ud_dutch, file_conllu, 
#'                            tokenizer = "none", tagger = "none", parser = "default")
#' metrics$accuracy
#' metrics <- udpipe_accuracy(ud_dutch, file_conllu, 
#'                            tokenizer = "default", tagger = "none", parser = "none")
#' metrics$accuracy
#' 
#' ## cleanup for CRAN only - you probably want to keep your model if you have downloaded it
#' file.remove("dutch-lassysmall-ud-2.0-170801.udpipe")
udpipe_accuracy <- function(object, 
                            file_conllu, 
                            tokenizer = c("default", "none"), 
                            tagger = c("default", "none"), 
                            parser = c("default", "none")) {
  if(!inherits(object, "udpipe_model")){
    stop("object should be of class udpipe_model as returned by the function ?udpipe_load_model")
  }
  stopifnot(file.exists(file_conllu))
  tokenizer <- match.arg(tokenizer)
  tagger <- match.arg(tagger)
  parser <- match.arg(parser)
  
  f <- tempfile()
  out <- udp_evaluate(object$model, file_conllu, f, tokenizer, tagger, parser)
  out$accuracy <- readLines(f)
  class(out) <- "udpipe_accuracy"
  out
}



#' @title Read in a CONLL-U file as a data.frame
#' @description Read in a CONLL-U file as a data.frame
#' @param file a connection object or a character string with the location of the file
#' @return a data.frame with columns doc_id, paragraph_id, sentence_id, sentence, 
#' token_id, token, lemma, upos, xpos, feats, head_token_id, deprel, dep_rel, misc
#' @export
#' @examples 
#' file_conllu <- system.file(package = "udpipe", "dummydata", "traindata.conllu")
#' x <- udpipe_read_conllu(file_conllu)
#' head(x)
udpipe_read_conllu <- function(file){
  x <- list(conllu = paste(readLines(file, encoding = "UTF-8"), collapse = "\n"),
            error = "")
  read_connlu(x, is_udpipe_annotation = FALSE)
}



#' @title Convert a data.frame to CONLL-U format 
#' @description If you have a data.frame with annotations containing 1 row per token, you can convert it to CONLL-U format with this function.
#' The data frame is required to have the following columns: doc_id, sentence_id, sentence, token_id, token and
#' optionally has the following columns: lemma, upos, xpos, feats, head_token_id, dep_rel, deps, misc. Where these fields
#' have the following meaning
#' \itemize{
#' \item doc_id: the identifier of the document
#' \item sentence_id: the identifier of the sentence
#' \item sentence: the text of the sentence for which this token is part of
#' \item token_id: Word index, integer starting at 1 for each new sentence; may be a range for multiword tokens; may be a decimal number for empty nodes.
#' \item token: Word form or punctuation symbol.
#' \item lemma: Lemma or stem of word form.
#' \item upos: Universal part-of-speech tag.
#' \item xpos: Language-specific part-of-speech tag; underscore if not available.
#' \item feats: List of morphological features from the universal feature inventory or from a defined language-specific extension; underscore if not available.
#' \item head_token_id: Head of the current word, which is either a value of token_id or zero (0).
#' \item dep_rel: Universal dependency relation to the HEAD (root iff HEAD = 0) or a defined language-specific subtype of one.
#' \item deps: Enhanced dependency graph in the form of a list of head-deprel pairs.
#' \item misc: Any other annotation.
#' }
#' The tokens in the data.frame should be ordered as they appear in the sentence.
#' @param x a data.frame with columns doc_id, sentence_id, sentence, 
#' token_id, token, lemma, upos, xpos, feats, head_token_id, deprel, dep_rel, misc
#' @return a character string of length 1 containing the data.frame in CONLL-U format. See the example. You can easily save this to disk for processing in other applications.
#' @export
#' @references \url{http://universaldependencies.org/format.html}
#' @examples 
#' file_conllu <- system.file(package = "udpipe", "dummydata", "traindata.conllu")
#' x <- udpipe_read_conllu(file_conllu)
#' str(x)
#' conllu <- as_conllu(x)
#' cat(conllu)
#' \dontrun{
#' ## Write it to file, making sure it is in UTF-8
#' cat(as_conllu(x), file = file("annotations.conllu", encoding = "UTF-8"))
#' }
#' 
#' ## Some fields are not mandatory, they will assummed to be NA
#' conllu <- as_conllu(x[, c('doc_id', 'sentence_id', 'sentence', 
#'                           'token_id', 'token', 'upos')])
#' cat(conllu)
as_conllu <- function(x){
  stopifnot(is.data.frame(x))
  stopifnot(all(c("doc_id", "sentence_id", "sentence", "token_id", "token") %in% colnames(x)))
  replaceNA <- function(x){
    x[is.na(x)] <- "_"
    x
  }
  required_fields <- c("token_id", "token", "lemma", "upos", "xpos", "feats", "head_token_id", "dep_rel", "deps", "misc")
  missing_fields <- setdiff(required_fields, colnames(x))
  if(length(missing_fields) > 0){
    message(sprintf("Missing columns %s => will set them to NA", paste(missing_fields, collapse = ", ")))
  }
  for(field in required_fields){
    if(!field %in% colnames(x)){
      x[[field]] <- NA
    }
    x[[field]] <- replaceNA(x[[field]])
  }
  ##
  ## Conllu files basically look like this:
  ##
  ## # newdoc id = document_identification
  ## # sent_id = idofthesentence
  ## # text = From the BlogFeed comes this story :
  ## 1	From	from	ADP	IN	_	3	case	3:case	_
  ## ...
  ##
  x$.newdoc <- !duplicated(x$doc_id)
  x$.first <- !duplicated(x[, c("doc_id", "sentence_id")])
  ## l
  x$.header <- ifelse(x$.first, sprintf("%s%s# sent_id = %s\n# text = %s\n", 
                                        ifelse(seq_len(nrow(x)) == 1, "", "\n"), 
                                        ifelse(x$.newdoc, sprintf("# newdoc id = %s\n", x$doc_id), ""),
                                        x$sentence_id, x$sentence), "")
  x$.core <- sprintf("%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n", 
                     x$token_id, 
                     x$token, 
                     x$lemma, 
                     x$upos, 
                     x$xpos, 
                     x$feats, 
                     x$head_token_id, 
                     x$dep_rel, 
                     x$deps, 
                     x$misc)
  txt <- paste(x$.header, x$.core, sep = "")
  txt <- paste(txt, collapse = "")
  sprintf("%s\n", txt)
}


#' @title Convert a matrix of word vectors to word2vec format 
#' @description The word2vec format provides in the first line the dimension of the word vectors and in the following lines one
#' has the elements of the wordvector where each line covers one word or token.\cr
#' 
#' The function is basically a utility function which allows one to write wordvectors created with other R packages in 
#' the well-known word2vec format which is used by \code{udpipe_train} to train the dependency parser.
#' @param x a matrix with word vectors where the rownames indicate the word or token and the number of columns
#' of the matrix indicate the side of the word vector
#' @return a character string of length 1 containing the word vectors in word2vec format which can be written to a file on disk
#' @export
#' @examples
#' wordvectors <- matrix(rnorm(1000), nrow = 100, ncol = 10)
#' rownames(wordvectors) <- sprintf("word%s", seq_len(nrow(wordvectors)))
#' wv <- as_word2vec(wordvectors)
#' cat(wv)
#' 
#' f <- file(tempfile(fileext = ".txt"), encoding = "UTF-8")
#' cat(wv, file = f)
#' close(f)
as_word2vec <- function(x){
  stopifnot(is.matrix(x))
  line1 <- sprintf("%s %s", nrow(x), ncol(x))
  lines_rest <- mapply(token = rownames(x), i = seq_len(nrow(x)), FUN=function(token, i){
    line <- sprintf("%s %s", token, paste(as.character(x[i, ]), collapse = " "))
    line
  })
  x <- c(line1, lines_rest)
  paste(x, collapse = "\n")
}
