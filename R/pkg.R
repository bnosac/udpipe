#' @importFrom Rcpp evalCpp
#' @importFrom data.table tstrsplit ":=" data.table setDF
#' @useDynLib udpipe
NULL

udpipe_env <- new.env()

.onLoad <- function(libname, pkgname){
  udpipe_env$log <- Sys.getenv("UDPIPE_PROCESS_LOG")
}
.onAttach <- function(libname, pkgname){
  udpipe_env$log <- Sys.getenv("UDPIPE_PROCESS_LOG")
}