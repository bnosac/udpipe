#' @importFrom Rcpp evalCpp
#' @importFrom data.table tstrsplit ":=" data.table as.data.table setDF setDT setnames setcolorder rbindlist setorder is.data.table frankv rleid uniqueN dcast.data.table
#' @importFrom utils download.file head tail capture.output
#' @importFrom methods cbind2 rbind2 as
#' @importFrom Matrix sparseMatrix summary crossprod tcrossprod diag 
#' @importFrom stats chisq.test
#' @useDynLib udpipe
NULL


.loaded_models <- new.env()