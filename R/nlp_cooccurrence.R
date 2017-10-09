#' @title Create a cooccurence data.frame
#' @description A cooccurence data.frame indicates how many times each term co-occurs with another term.
#' This type of dataset is a data.frame with fields term1, term2 and cooc where cooc indicates how many times
#' term1 and term2 co-occurred.\cr
#' 
#' The dataset can be constructed based upon a data frame where you look with a group if 2 terms occurred.\cr
#' It also can be constructed based upon a vector of words in which case we look how many times each word is 
#' followed by another word.
#' @param x either
#' \itemize{
#'   \item a data.frame where the data.frame contains 1 row per document/term (cooccurrenc.data.frame),
#'   in which case you need to provide \code{group} and \code{term}
#'   \item a character vector with terms (cooccurrence.character)
#'   \item an object of class \code{cooccurrence} (cooccurrence.cooccurrence)
#' }
#' @param order logical indicating if we need to sort the output from high cooccurrences to low coccurrences. Defaults to TRUE.
#' @param ... other arguments passed on to the methods
#' @return a data.frame with columns term1, term2 and cooc indicating
#' for the combination of term1 and term2 how many times this occurred
#' @export
#' @examples 
#' data(brussels_reviews_anno)
#' 
#' ## By document, which lemma's co-occur
#' x <- subset(brussels_reviews_anno, xpos %in% c("NN", "JJ") & language %in% "fr")
#' x <- cooccurrence(x, group = "doc_id", term = "lemma")
#' head(x)
#' 
#' ## Which words follow each other
#' x <- c("A", "B", "A", "B", "c")
#' cooccurrence(x)
#' 
#' data(brussels_reviews_anno)
#' x <- subset(brussels_reviews_anno, language == "es")
#' x <- cooccurrence(x$lemma)
#' head(x)
#' 
#' ## Which nouns follow each other in the same document
#' library(data.table)
#' x <- as.data.table(brussels_reviews_anno)
#' x <- subset(x, language == "nl" & xpos %in% c("NN"))
#' x <- x[, cooccurrence(lemma, order = FALSE), by = list(doc_id)]
#' head(x)
#' 
#' x_nodoc <- cooccurrence(x)
#' x_nodoc <- subset(x_nodoc, term1 != "appartement" & term2 != "appartement")
#' head(x_nodoc)
cooccurrence <- function(x, order = TRUE, ...){
  UseMethod("cooccurrence", x)
}

#' @describeIn cooccurrence Create a cooccurence data.frame based on a vector of terms
#' @export
cooccurrence.character <- function(x, order = TRUE, ...){
  cooc <- term1 <- term2 <- NULL
  
  result <- data.table(term1 = x,
             term2 = txt_next(x, n = 1),
             cooc = 1L)
  result <- subset(result, !is.na(term1) & !is.na(term2))
  result <- result[, list(cooc = sum(cooc)), by = list(term1, term2)]
  
  terminology <- c(unique(result$term1), unique(result$term2))
  terminology <- unique(terminology)
  if(order){
    setorder(result, -cooc)  
  }
  class(result) <- c("cooccurrence", "data.frame", "data.table")
  result
}

#' @describeIn cooccurrence Aggregate co-occurrence statistics by summing the cooc by term/term2 
#' @export
cooccurrence.cooccurrence <- function(x, order = TRUE, ...){
  cooc <- term1 <- term2 <- NULL
  
  result <- as.data.table(x)
  result <- result[, list(cooc = sum(cooc)), by = list(term1, term2)]
  if(order){
    setorder(result, -cooc)  
  }
  class(result) <- c("cooccurrence", "data.frame", "data.table")
  result
}


#' @describeIn cooccurrence Create a cooccurence data.frame based on a data.frame where you look within a document / sentence / paragraph / group 
#' if terms co-occur
#' @param group character string with a column in the data frame \code{x}
#' @param term character string with a column in the data frame \code{x}, containing 1 term per row
#' @export
cooccurrence.data.frame <- function(x, order = TRUE, ..., group, term) {
  if(missing(group) & missing(term) & all(c("term1", "term2", "cooc") %in% colnames(x))){
    return(cooccurrence.cooccurrence(x, ...))
  }
  ## make R CMD check happy
  .N <- NULL

  dtf <- setDT(x)[, list(freq = .N), by = c(group, term)]
  setnames(dtf, old = c(group, term), new = c("doc_id", "term"))
  dtf <- dtf[!is.na(term), ]
  
  x <- document_term_matrix(dtf)
  terms <- colnames(x)
  terms_idx <- seq_along(terms)
  
  cooc <- Matrix::t(x) %*% x
  termtermmatrix <- Matrix::summary(cooc)
  termtermmatrix <- termtermmatrix[termtermmatrix[, "i"] < termtermmatrix[, "j"], ]
  
  result <- data.table(
    term1 = terms[match(termtermmatrix[, "i"], terms_idx)], 
    term2 = terms[match(termtermmatrix[, "j"], terms_idx)], 
    cooc = termtermmatrix[, "x"])
  if(order){
    setorder(result, -cooc)  
  }
  setDF(result)
  class(result) <- c("cooccurrence", "data.frame", "data.table")
  result
}




