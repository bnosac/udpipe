#' @name annotation_params
#' @title POS tags and definitions used in the Penn Treebank. 
#' @description In order to show the settings which were used by the UDPipe community when building 
#' the models made available when using \code{\link{udpipe_download_model}}, 
#' the tokenizer settings used for the different treebanks are shown below, 
#' so that you can easily use this to retrain your model directly on the corresponding 
#' UD treebank which you can download at \code{http://universaldependencies.org/#ud-treebanks}. \cr
#' 
#' More information on how the models provided by the UDPipe community have been built are available
#' at \url{https://lindat.mff.cuni.cz/repository/xmlui/handle/11234/1-2364}
#' @docType data
#' @references \url{https://lindat.mff.cuni.cz/repository/xmlui/handle/11234/1-2364}
#' @examples 
#' data(annotation_params)
#' str(annotation_params)
#' 
#' ## settings of the tokenizer
#' head(annotation_params$tokenizer)
#' 
#' ## settings of the tagger
#' subset(annotation_params$tagger, language_treebank == "nl")
#' 
#' ## settings of the parser
#' annotation_params$parser
NULL