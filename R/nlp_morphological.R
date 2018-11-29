#' @title Add morphological features to an annotated dataset
#' @description The result of \code{\link{udpipe_annotate}} which is put into a data.frame 
#' returns a field called \code{feats} containing morphological features as defined at 
#' \url{http://universaldependencies.org/u/feat/index.html}. If there are several of these features,
#' these are concatenated with the \code{|} symbol. This function extracts each of these morphological 
#' features separately and adds these as extra columns to the data.frame
#' @param x a data.frame or data.table as returned by \code{as.data.frame(udpipe_annotate(...))}
#' @param term the name of the field in \code{x} which contains the morphological features. Defaults to 'feats'.
#' @return \code{x} in the same order with extra columns added (at least the column has_morph is added indicating
#' if any morphological features are present and as well extra columns for each possible morphological feature in the data)
#' @export
#' @examples 
#' \dontrun{
#' udmodel <- udpipe_download_model(language = "english-ewt")
#' udmodel <- udpipe_load_model(file = udmodel$file_model)
#' x <- udpipe_annotate(udmodel, 
#'                      x = "The economy is weak but the outlook is bright")
#' x <- as.data.frame(x)
#' x <- cbind_morphological(x, term = "feats")
#' }
#' 
#' f <- system.file(package = "udpipe", "dummydata", "traindata.conllu")
#' x <- udpipe_read_conllu(f)
#' x <- cbind_morphological(x, term = "feats")
cbind_morphological <- function(x, term = "feats"){
  stopifnot(inherits(x, "data.frame"))
  stopifnot(length(term) == 1)
  stopifnot(term %in% colnames(x))
  out <- txt_morphological(x[[term]])
  out <- data.table::setnames(out, 
                              old = colnames(out), 
                              new = c("has_morph", sprintf("morph_%s", setdiff(colnames(out), "has_morph"))))
  if(inherits(x, "data.table")){
    x[, colnames(out), with = FALSE] <- out  
  }else if(inherits(x, "data.frame")){
    x[, colnames(out)] <- out  
  }
  x
}



txt_morphological <- function(x){
  ## R CMD check happiness
  id <- has_morph <- NULL
  
  ## Split field according to |, next put it into data.frame and reshape it to wide format
  ## Add an indicator has_morph indicating if morphological features are uberhaupt present
  morpho <- strsplit(x, split = "\\|")
  morpho <- lapply(morpho, FUN=function(x) list(feats = x))
  morpho <- data.table::rbindlist(morpho, idcol = "id")
  ## NA values in morpohological features are set to None=None
  morpho$feats[is.na(morpho$feats)] <- "has_morph=FALSE"
  morpho$key <- gsub("^(.+)=(.+)$", "\\1", morpho$feats)
  morpho$key <- tolower(morpho$key) 
  morpho$value <- gsub("^(.+)=(.+)$", "\\2", morpho$feats)
  morpho <- data.table::dcast.data.table(data = morpho, formula = id ~ key, value.var = "value")
  if("has_morph" %in% colnames(morpho)){
    morpho <- morpho[, has_morph := as.logical(has_morph)]
    morpho$has_morph[is.na(morpho$has_morph)] <- TRUE
  }else{
    morpho <- morpho[, has_morph := TRUE]
  }
  morpho <- morpho[, id := NULL]
  morpho <- data.table::setcolorder(morpho, c("has_morph", setdiff(colnames(morpho), "has_morph")))
  morpho <- data.table::setDF(morpho)
  morpho
} 



