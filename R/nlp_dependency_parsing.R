#' @title Add the dependency parsing information to an annotated dataset
#' @description Annotated results of \code{udpipe_annotate} contain dependency parsing results which indicate
#' how each word is linked to another word and the relation between these 2 words.\cr
#' This information is available in the fields token_id, head_token_id and dep_rel which indicates how each token
#' is linked to the parent. The type of relation (dep_rel) is defined at 
#' \url{http://universaldependencies.org/u/dep/index.html}. 
#' For example in the text 'The economy is weak but the outlook is bright', the term economy is linked to weak
#' as the term economy is the nominal subject of weak. \cr
#' This function adds the parent information to the annotated data.frame.
#' @param x a data.frame or data.table as returned by \code{as.data.frame(udpipe_annotate(...))}
#' @param type currently only possible value is 'parent', indicating to add the information of the head_token_id to the dataset
#' @return a data.frame/data.table in the same order of \code{x}
#' where the token/lemma/upos/xpos information of the parent (head dependency) is added to the data.frame. See the examples.
#' @details Mark that the output which this function provides might possibly change in subsequent releases and is experimental.
#' @export
#' @examples 
#' \dontrun{
#' udmodel <- udpipe_download_model(language = "english-ewt")
#' udmodel <- udpipe_load_model(file = udmodel$file_model)
#' x <- udpipe_annotate(udmodel, 
#'                      x = "The economy is weak but the outlook is bright")
#' x <- as.data.frame(x)
#' x[, c("token_id", "token", "head_token_id", "dep_rel")]
#' x <- cbind_dependencies(x, type = "parent")
#' nominalsubject <- subset(x, dep_rel %in% c("nsubj"))
#' nominalsubject <- nominalsubject[, c("dep_rel", "token", "token_parent")]
#' nominalsubject
#' }
cbind_dependencies <- function(x, type = c("parent", "child")){
  type <- match.arg(type)
  stopifnot(inherits(x, "data.frame"))
  fields <- colnames(x)
  
  ## convert to data.table as merge.data.table is so much faster than base::merge
  was_data_table <- inherits(x, "data.table")
  x <- data.table::setDT(x)
  if(type == "parent"){
    out <- merge(x, 
                 x[, c("doc_id", "paragraph_id", "sentence_id", "token_id", "token", "lemma", "upos", "xpos"), with = FALSE], 
                 by.x = c("doc_id", "paragraph_id", "sentence_id", "head_token_id"),
                 by.y = c("doc_id", "paragraph_id", "sentence_id", "token_id"),
                 all.x = TRUE, all.y = FALSE, 
                 suffixes = c("", "_parent"),
                 sort = FALSE)  
  }else if(type == "child"){
    .NotYetImplemented()
  }
  out <- data.table::setcolorder(out, neworder = c(fields, setdiff(colnames(out), fields)))
  if(!was_data_table){
    out <- data.table::setDF(out)
  }
  out
}



expressionlist <- function(...){
  #expr <- substitute(list(...))
  #expr <- as.list(expr[-1])
  expr <- substitute(...())  
  expr
}



dependency_locations <- function(data, type = c("parent", "children"), recursive = FALSE){
  type <- match.arg(type)
  stopifnot(inherits(data, c("data.frame", "rows_parent", "rows_child")))
  if(inherits(data, "data.frame") & !recursive){
    if(type == "parent"){
      # each term is linked to exactly 1 parent head, we could put it into a vector but for consistency, put it as a list
      #as.list(match(data$term_id_parent, data$term_id))
      #as.list(match(data$head_token_id,  data$token_id))
      # make sure NA values are not there but are elements of length 0
      result <- lapply(data$head_token_id, FUN = function(id) which(id ==  data$token_id)) 
      class(result) <- c("rows_parent")
    }else if(type == "children"){
      # a list of row numbers where to find the children
      #lapply(data$term_id,  FUN = function(id) which(id == data$term_id_parent))
      result <- lapply(data$token_id, FUN = function(id) which(id == data$head_token_id))
      class(result) <- c("rows_child")
    }  
    return(result)
  }else if(recursive && inherits(data, c("rows_parent", "rows_child"))){
    recursive_locations <- function(i, linksto){
      if(length(i)){
        new <- unlist(linksto[i])
        if(length(new)){
          new <- recursive_locations(new, linksto)
        }
        i <- c(i, new)
      }
      i
    }
    lapply(data, FUN=function(i) recursive_locations(i, data))
  }else{
    stop("data should be of type data.frame or of type rows_parent/rows_child in case recursive is set to TRUE")
  }
}
