#' @name udpipe_annotation_params
#' @title List with training options set by the UDPipe community when building models based on the Universal Dependencies data
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
#' data(udpipe_annotation_params)
#' str(udpipe_annotation_params)
#' 
#' ## settings of the tokenizer
#' head(udpipe_annotation_params$tokenizer)
#' 
#' ## settings of the tagger
#' subset(udpipe_annotation_params$tagger, language_treebank == "nl")
#' 
#' ## settings of the parser
#' udpipe_annotation_params$parser
NULL





#' @title Brussels AirBnB address locations available at www.insideairbnb.com
#' @description Brussels AirBnB address locations available at www.insideairbnb.com
#' More information: http://insideairbnb.com/get-the-data.html \cr
#' Data has been converted from UTF-8 to ASCII as in \code{iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")} in order
#' to be able to comply to CRAN policies.
#' @name brussels_listings
#' @docType data
#' @source \url{http://insideairbnb.com/brussels}: information of 2015-10-03
#' @seealso \code{\link{brussels_reviews}}, \code{\link{brussels_reviews_anno}}
#' @examples
#' data(brussels_listings)
#' head(brussels_listings)
NULL


#' @title Reviews of AirBnB customers on Brussels address locations available at www.insideairbnb.com
#' @description Reviews of AirBnB customers on Brussels address locations available at www.insideairbnb.com
#' More information: http://insideairbnb.com/get-the-data.html.
#' The data contains 500 reviews in Spanish, 500 reviews in French and 500 reviews in Dutch.\cr
#' The data frame contains the field id (unique), listing_id which corresponds to the listing_id of
#' the \code{\link{brussels_listings}} dataset and text fields feedback and language (identified with package cld2) \cr
#' Data has been converted from UTF-8 to ASCII as in \code{iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")} in order
#' to be able to comply to CRAN policies.
#' @name brussels_reviews
#' @docType data
#' @source \url{http://insideairbnb.com/brussels}: information of 2015-10-03
#' @seealso \code{\link{brussels_listings}}, \code{\link{brussels_reviews_anno}}
#' @examples
#' data(brussels_reviews)
#' str(brussels_reviews)
#' head(brussels_reviews)
NULL


#' @title Reviews of the AirBnB customers which are tokenised, POS tagged and lemmatised
#' @description Reviews of the AirBnB customerswhich are tokenised, POS tagged and lemmatised.
#' The data contains 1 row per document/token and contains the fields
#' doc_id, language, sentence_id, token_id, token, lemma, xpos. \cr
#' Data has been converted from UTF-8 to ASCII as in \code{iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")} in order
#' to be able to comply to CRAN policies.
#' @name brussels_reviews_anno
#' @docType data
#' @source \url{http://insideairbnb.com/brussels}: information of 2015-10-03
#' @seealso \code{\link{brussels_reviews}}, \code{\link{brussels_listings}}
#' @examples
#' ## brussels_reviews_anno
#' data(brussels_reviews_anno)
#' head(brussels_reviews_anno)
#' sort(table(brussels_reviews_anno$xpos))
#' 
#' \dontrun{
#' 
#' ##
#' ## If you want to construct a similar dataset as the 
#' ## brussels_reviews_anno dataset based on the udpipe library, do as follows
#' ##
#' 
#' library(udpipe)
#' library(data.table)
#' data(brussels_reviews)
#' 
#' ## The brussels_reviews contains comments on Airbnb sites in 3 languages: es, fr and nl
#' table(brussels_reviews$language)
#' bxl_anno <- split(brussels_reviews, brussels_reviews$language)
#' 
#' ## Annotate the Spanish comments
#' m <- udpipe_download_model(language = "spanish-ancora")
#' m <- udpipe_load_model(file = m$file_model)
#' bxl_anno$es <- udpipe_annotate(object = m, x = bxl_anno$es$feedback, doc_id = bxl_anno$es$id)
#' 
#' ## Annotate the French comments
#' m <- udpipe_download_model(language = "french-partut")
#' m <- udpipe_load_model(file = m$file_model)
#' bxl_anno$fr <- udpipe_annotate(object = m, x = bxl_anno$fr$feedback, doc_id = bxl_anno$fr$id)
#' 
#' ## Annotate the Dutch comments
#' m <- udpipe_download_model(language = "dutch-lassysmall")
#' m <- udpipe_load_model(file = m$file_model)
#' bxl_anno$nl <- udpipe_annotate(object = m, x = bxl_anno$nl$feedback, doc_id = bxl_anno$nl$id)
#' 
#' brussels_reviews_anno <- lapply(bxl_anno, as.data.frame)
#' brussels_reviews_anno <- rbindlist(brussels_reviews_anno)
#' str(brussels_reviews_anno)
#' }
NULL
