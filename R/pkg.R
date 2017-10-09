#' @importFrom Rcpp evalCpp
#' @importFrom data.table tstrsplit ":=" data.table as.data.table setDF setDT setnames rbindlist setorder
#' @importFrom utils download.file head
#' @importFrom Matrix sparseMatrix summary crossprod tcrossprod diag 
#' @useDynLib udpipe
NULL

udpipe_env <- new.env()

.onLoad <- function(libname, pkgname){
  udpipe_env$log <- Sys.getenv("UDPIPE_PROCESS_LOG")
}
.onAttach <- function(libname, pkgname){
  udpipe_env$log <- Sys.getenv("UDPIPE_PROCESS_LOG")
}