#' @title Add the dependency parsing information to an annotated dataset
#' @description Annotated results of \code{udpipe_annotate} contain dependency parsing results which indicate
#' how each word is linked to another word and the relation between these 2 words.\cr
#' This information is available in the fields token_id, head_token_id and dep_rel which indicates how each token
#' is linked to the parent. The type of relation (dep_rel) is defined at 
#' \url{http://universaldependencies.org/u/dep/index.html}. 
#' For example in the text 'The economy is weak but the outlook is bright', the term economy is linked to weak
#' as the term economy is the nominal subject of weak. \cr
#' This function adds the parent or child information to the annotated data.frame.
#' @param x a data.frame or data.table as returned by \code{as.data.frame(udpipe_annotate(...))}
#' @param type either one of 'parent', 'child', 'parent_rowid', 'child_rowid'. Look to the return value section for more information on the difference in logic. 
#' Defaults to  'parent', indicating to add the information of the head_token_id to the dataset
#' @param recursive in case when \code{type} is set to 'parent_rowid' or 'child_rowid', do you want the parent of the parent of the parent, ... or the child of the child of the child ... included. Defaults to FALSE indicating to only have the direct parent or children.
#' @return a data.frame/data.table in the same order of \code{x} where extra information is added on top namely:
#' \itemize{
#'   \item In case \code{type} is set to \code{'parent'}: the token/lemma/upos/xpos information of the parent (head dependency) is added to the data.frame. See the examples.
#'   \item In case \code{type} is set to \code{'parent_rowid'}: a new list column is added to \code{x} containing the row numbers within each combination of \code{doc_id, paragraph_id, sentence_id} which are parents of the token. \cr
#'   In case recursive is set to \code{TRUE} the new column which is added to the data.frame is called \code{parent_rowids}, otherwise it is called \code{parent_rowid}. See the examples.
#'   \item In case \code{type} is set to \code{'child_rowid'}: a new list column is added to \code{x} containing the row numbers  within each combination of \code{doc_id, paragraph_id, sentence_id} which are children of the token. \cr
#'   In case recursive is set to \code{TRUE} the new column which is added to the data.frame is called \code{child_rowids}, otherwise it is called \code{child_rowid}. See the examples.
#' }
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
#' 
#' x <- cbind_dependencies(x, type = "parent_rowid")
#' x <- cbind_dependencies(x, type = "parent_rowid", recursive = TRUE)
#' x <- cbind_dependencies(x, type = "child_rowid")
#' x <- cbind_dependencies(x, type = "child_rowid", recursive = TRUE)
#' x
#' lapply(x$child_rowid, FUN=function(i) x[sort(i), ])
#' }
cbind_dependencies <- function(x, type = c("parent", "child", "parent_rowid", "child_rowid"), recursive = FALSE){
  ## R CMD check happiness
  .SD <- doc_id <- paragraph_id <- sentence_id <- parent_rowid <- parent_rowids <- child_rowid <- child_rowids <- NULL
  
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
  }else if(type == "parent_rowid"){
    if(recursive){
      out <- x[, parent_rowids := list(dependency_locations(data = .SD, type = "parent", recursive = recursive)), by = list(doc_id, paragraph_id, sentence_id)]
    }else{
      out <- x[, parent_rowid := list(dependency_locations(data = .SD, type = "parent", recursive = recursive)), by = list(doc_id, paragraph_id, sentence_id)]  
    }
    
  }else if(type == "child_rowid"){
    if(recursive){
      out <- x[, child_rowids := list(dependency_locations(data = .SD, type = "children", recursive = recursive)), by = list(doc_id, paragraph_id, sentence_id)]
    }else{
      out <- x[, child_rowid := list(dependency_locations(data = .SD, type = "children", recursive = recursive)), by = list(doc_id, paragraph_id, sentence_id)]
    }
    
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
  }else if(recursive){
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
    if(!inherits(data, c("rows_parent", "rows_child"))){
      data <- dependency_locations(data = data, type = type, recursive = FALSE)
    }
    lapply(data, FUN=function(i) recursive_locations(i, data))
  }else{
    stop("data should be of type data.frame or of type rows_parent/rows_child in case recursive is set to TRUE")
  }
}
