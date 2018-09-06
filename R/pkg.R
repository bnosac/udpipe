#' @importFrom Rcpp evalCpp
#' @importFrom data.table tstrsplit ":=" data.table as.data.table setDF setDT setnames setcolorder rbindlist setorder is.data.table frankv rleid uniqueN dcast.data.table
#' @importFrom utils download.file head
#' @importFrom methods cbind2 rbind2 as
#' @importFrom Matrix sparseMatrix summary crossprod tcrossprod diag 
#' @useDynLib udpipe
NULL


.current_model <- new.env()
.current_model$udpipe_model <- structure(list(file = character(0), model = NULL), class = "udpipe_model")