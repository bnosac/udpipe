#' @title Train a UDPipe model
#' @description Train a UDPipe model
#' @param file full path where the model will be saved
#' @param files_conllu_training a character vector of files in CONLL-U format used for training the model
#' @param files_conllu_holdout a character vector of files in CONLL-U format used for holdout evalution of the model
#' @param annotation_tokenizer a string containing options for the tokenizer. This can be either 'none' or 'default' or any 
#' number of options as mentioned in the UDPipe manuale. The options should be semicolon separated. 
#' See the example and \url{http://ufal.mff.cuni.cz/udpipe/users-manual} for the possible options.
#' @param annotation_tagger a string containing options for the pos tagger and lemmatiser. This can be either 'none' or 'default' or any 
#' number of options as mentioned in the UDPipe manuale. The options should be semicolon separated. 
#' See the example and \url{http://ufal.mff.cuni.cz/udpipe/users-manual} for the possible options.
#' @param annotation_parser a string containing options for the dependency parser. This can be either 'none' or 'default' or any 
#' number of options as mentioned in the UDPipe manuale. The options should be semicolon separated. 
#' See the example and \url{http://ufal.mff.cuni.cz/udpipe/users-manual} for the possible options.
#' @return A list with elements file (the path to the model, which can be used in \code{udpipe_load_model}), 
#' and the provided arguments annotation_tokenizer, annotation_tagger and annotation_parser
#' @seealso \code{\link{udpipe_annotate}}
#' @references \url{http://ufal.mff.cuni.cz/udpipe/users-manual}
#' @export
#' @examples 
#' \dontrun{
#' mymodel <- udpipe_train(
#'   file = "toymodel.udpipe", 
#'   files_conllu_training = "/home/bnosac/Desktop/ud-treebanks-v2.0/UD_Dutch/nl-ud-train.conllu",
#'   files_conllu_holdout = "/home/bnosac/Desktop/ud-treebanks-v2.0/UD_Dutch/nl-ud-dev.conllu",
#'   annotation_tokenizer = "dimension=64;epochs=2;initialization_range=0.1;batch_size=100", 
#'   annotation_tagger = "models=1;templates_1=tagger;guesser_suffix_rules_1=10;iterations_1=1", 
#'   annotation_parser = "none")
#' mymodel
#' }
udpipe_train <- function(file, files_conllu_training, files_conllu_holdout, 
                         annotation_tokenizer = "tokenizer", 
                         annotation_tagger = "default", 
                         annotation_parser = "default") {
  file <- path.expand(file)
  if(!dir.exists(dirname(file))){
    dir.create(dirname(file), recursive = TRUE)
  }
  stopifnot(all(file.exists(files_conllu_training)))
  stopifnot(all(file.exists(files_conllu_holdout)))
  model <- udp_train(file, files_conllu_training, files_conllu_holdout, 
            annotation_tokenizer, annotation_tagger, annotation_parser)
  structure(
    list(file = model, 
         annotation_tokenizer = annotation_tokenizer,
         annotation_tagger = annotation_tagger,
         annotation_parser = annotation_parser,
    class = "udpipe_trained_model"))
}
