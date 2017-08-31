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
#' @param files_conllu_holdout a character vector of files in CONLL-U format used for holdout evalution of the model
#' @param annotation_tokenizer a string containing options for the tokenizer. This can be either 'none' or 'default' or any 
#' number of options as mentioned in the UDPipe manual. The options should be semicolon separated. In the future it will be possible to provide a list of options,
#' currently you need to provide a string as shown in the the examples.
#' See the example and \url{http://ufal.mff.cuni.cz/udpipe/users-manual#model_training_tokenizer} for the possible options. Defaults to 'default'. If you specify 'none',
#' the model will not be able to perform this annotation.
#' @param annotation_tagger a string containing options for the pos tagger and lemmatiser. This can be either 'none' or 'default' or any 
#' number of options as mentioned in the UDPipe manual. The options should be semicolon separated. I in the future it will be possible to provide a list of options,
#' currently you need to provide a string as shown in the the examples.
#' See the example and \url{http://ufal.mff.cuni.cz/udpipe/users-manual#model_training_tagger} for the possible options. Defaults to 'default'. If you specify 'none',
#' the model will not be able to perform this annotation.
#' @param annotation_parser a string containing options for the dependency parser. This can be either 'none' or 'default' or any 
#' number of options as mentioned in the UDPipe manual. The options should be semicolon separated. I in the future it will be possible to provide a list of options,
#' currently you need to provide a string as shown in the the examples.
#' See the example and \url{http://ufal.mff.cuni.cz/udpipe/users-manual#model_training_parser} for the possible options. Defaults to 'default'. If you specify 'none',
#' the model will not be able to perform this annotation.
#' @return A list with elements 
#' \itemize{
#'  \item{file: }{The path to the model, which can be used in \code{udpipe_load_model}}
#'  \item{annotation_tokenizer: }{The input argument \code{annotation_tokenizer}}
#'  \item{annotation_tagger: }{The input argument \code{annotation_tagger}}
#'  \item{annotation_parser: }{The input argument \code{annotation_parser}}
#'  \irem{errors: }{Messages from the UDPipe process indicating possible errors}
#'  \item{udpipe_log: }{The log of the udpipe process if you provided the environment variable UDPIPE_PROCESS_LOG as shown in the details}
#' }
#' @seealso \code{\link{udpipe_annotate}}, \code{\link{udpipe_load_model}}
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
#' file_conllu <- system.file(package = "udpipe", "dummydata", "traindata.conllu")
#' cat(readLines(file_conllu), sep="\n")
#' \dontrun{
#' m <- udpipe_train(file = "toyexample.udpipe", files_conllu_training = file_conllu)
#' 
#' m <- udpipe_train(file = "toyexample.udpipe", files_conllu_training = file_conllu,
#'   annotation_tokenizer = "dimension=16;epochs=1",
#'   annotation_tagger = "default", 
#'   annotation_parser = "none")
#' 
#' txt <- "Dit is een tokenizer zonder tagging mogelijkheden, noch laat die dependency parsing toe"
#' mymodel <- udpipe_load_model("toyexample.udpipe")
#' x_tagged <- udpipe_annotate(object = mymodel, x = txt, tagger = "default", parser = "none")
#' x_tagged <- as.data.frame(x_tagged)
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
