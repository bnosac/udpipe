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
#'  \item{udpipe_log: }{The log of the udpipe process if you provided the environment variable UDPIPE_PROCESS_LOG as shown in the details}
#' }
#' @seealso \code{\link{udpipe_annotation_params}}, \code{\link{udpipe_annotate}}, \code{\link{udpipe_load_model}}
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
#' slovenian, spanish, swedish, tamil, turkish, ukrainian, urdu, uyghur, vietnamese. \cr
#' 
#' Mark that as training can take a while, you can set the environment variable UDPIPE_PROCESS_LOG to
#' a location of a file on disk. As in Sys.setenv(UDPIPE_PROCESS_LOG = "udpipe.log"). 
#' The evolution of the training will be put in that log. 
#' Mark that you need to do this before you load the udpipe package.
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
  if(!dir.exists(dirname(file))){
    dir.create(dirname(file), recursive = TRUE)
  }
  stopifnot(all(file.exists(files_conllu_training)))
  stopifnot(all(file.exists(files_conllu_holdout)))
  result <- udp_train(file, files_conllu_training, files_conllu_holdout, 
            annotation_tokenizer, annotation_tagger, annotation_parser)
  if(udpipe_env$log != ""){
    log <- readLines(udpipe_env$log)  
  }else{
    log <- "You need to set Sys.setenv(UDPIPE_PROCESS_LOG = 'udpipe.log') before loading the package to get the log of udpipe"
  }
  
  structure(
    list(file_model = result$file_model, 
         annotation_tokenizer = annotation_tokenizer,
         annotation_tagger = annotation_tagger,
         annotation_parser = annotation_parser,
         errors = result$errors,
         log = log,
    class = "udpipe_trained_model"))
}
