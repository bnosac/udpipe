expr_list <- function(...){
  #expr <- substitute(list(...))
  #expr <- as.list(expr[-1])
  expr <- substitute(...())  
  expr
}

expr_labels = function(expr){
  labels <- names(expr)
  if(is.null(labels)){
    labels <- unlist(lapply(expr, FUN=function(e) paste(deparse(e, backtick = TRUE), collapse = "\n")))
  }
  idx <- which(labels == "")
  labels[idx] <- sapply(expr[idx], FUN=function(e) paste(deparse(e, backtick = TRUE), collapse = "\n"), USE.NAMES = FALSE)
  labels      <- unlist(labels)
  as.character(labels)
}

