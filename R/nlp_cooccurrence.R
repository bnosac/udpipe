#' @title Create a cooccurence data.frame
#' @description A cooccurence data.frame indicates how many times each term co-occurs with another term.\cr
#' 
#' There are 3 types of cooccurrences:
#' \itemize{
#'   \item Looking at which words are located in the same document/sentence/paragraph.
#'   \item Looking at which words are followed by another word
#'   \item Looking at which words are in the neighbourhood of the word as in follows the word within \code{skipgram} number of words
#' }
#' The output of the function gives a cooccurrence data.frame which contains the fields term1, term2 and cooc where cooc indicates how many times
#' term1 and term2 co-occurred. This dataset can be constructed 
#' \itemize{
#'   \item based upon a data frame where you look within a group (column of the data.frame) if 2 terms occurred in that group.
#'   \item based upon a vector of words in which case we look how many times each word is followed by another word.
#'   \item based upon a vector of words in which case we look how many times each word is followed by another word or is followed by another word if we skip a number of words in between.
#' }
#' You can also aggregate cooccurrences if you decide to do any of these 3 by a certain group and next want to have
#' an overall aggregate.
#' @param x either
#' \itemize{
#'   \item a data.frame where the data.frame contains 1 row per document/term,
#'   in which case you need to provide \code{group} and \code{term} where \code{term} is the column containing 1 term per row
#'   and \code{group} indicates something like a document id or document + sentence id. This uses cooccurrence.data.frame.
#'   \item a character vector with terms where one element contains 1 term. This uses cooccurrence.character.
#'   \item an object of class \code{cooccurrence}. This uses cooccurrence.cooccurrence.
#' }
#' @param order logical indicating if we need to sort the output from high cooccurrences to low coccurrences. Defaults to TRUE.
#' @param ... other arguments passed on to the methods
#' @return a data.frame with columns term1, term2 and cooc indicating
#' for the combination of term1 and term2 how many times this combination occurred
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
#' x <- subset(brussels_reviews_anno, language == "es")
#' x <- cooccurrence(x$lemma, relevant = x$xpos %in% c("NN", "JJ"), skipgram = 4)
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
#' @param relevant a logical vector of the same length as \code{x}, indicating if the word in \code{x} is relevant or not.
#' This can be used to exclude stopwords from the cooccurrence calculation or selecting only nouns and adjectives to 
#' find cooccurrences along with each other 
#' (for example based on the Parts of Speech \code{upos} output from \code{udpipe_annotate}).\cr
#' Only used if calculating cooccurrences on \code{x} which is a character vector of words.
#' @param skipgram integer of length 1, indicating how far in the neighbourhood to look for words.\cr
#' \code{skipgram} is considered the maximum skip distance between words to calculate co-occurrences 
#' (where co-occurrences are of type skipgram-bigram, where a skipgram-bigram are 2 words which occur at a distance of at most \code{skipgram + 1} from each other). \cr
#' Only used if calculating cooccurrences on \code{x} which is a character vector of words.
#' @export
cooccurrence.character <- function(x, order = TRUE, ..., relevant = rep(TRUE, length(x)), skipgram = 0){
  stopifnot(all(skipgram >= 0))
  cooc <- term1 <- term2 <- NULL
  
  ## skipdistances if it is only 1 value, it is considered the maximum skip distance between words, compute all skip n-grams between 0 and skipgram
  ## if there are several values, consider them as such
  skipdistances <- as.integer(skipgram)
  if(length(skipdistances) == 1){
    skipdistances <- seq(0, skipdistances, by = 1)
  }else{
    skipdistances <- union(0L, skipdistances)
  }
  
  # look which word are followed with the next word, 
  # look which word is followed by the 2nd next word, 3rd next word andsoforth
  # but if the data is not considered relevant, do not use it
  irrelevant <- !relevant
  result <- lapply(skipdistances, FUN=function(n){
    result <- data.table(term1 = x, 
                         term2 = txt_next(x, n = n + 1L), 
                         cooc = 1L)
    not_relevant <- txt_next(relevant, n = n + 1L)
    not_relevant <- irrelevant | (not_relevant %in% FALSE)
    if(sum(not_relevant) > 0){
      result[not_relevant, term1 := NA_character_] 
      result[not_relevant, term2 := NA_character_]  
    }
    result <- subset(result, !is.na(term1) & !is.na(term2))
    result <- result[, list(cooc = sum(cooc)), by = list(term1, term2)]
    result
  })
  result <- rbindlist(result)
  result <- result[, list(cooc = sum(cooc)), by = list(term1, term2)]
  
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
#' @param group character string with a column in the data frame \code{x} indicating to calculate cooccurrences within this column. \cr
#' This is typically a field like document id or a sentence identifier. To be used if \code{x} is a data.frame.
#' @param term character string with a column in the data frame \code{x}, containing 1 term per row. To be used if \code{x} is a data.frame.
#' @export
cooccurrence.data.frame <- function(x, order = TRUE, ..., group, term) {
  if(missing(group) & missing(term) & all(c("term1", "term2", "cooc") %in% colnames(x))){
    return(cooccurrence.cooccurrence(x, ...))
  }
  ## make R CMD check happy
  .N <- NULL

  dtf <- setDT(x)[, list(freq = .N), by = c(group, term)]
  dtf <- setnames(dtf, old = c(group, term), new = c("doc_id", "term"))
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



#' @title Convert the result of cooccurrence to a sparse matrix
#' @description Convert the result of \code{\link{cooccurrence}} to a sparse matrix.
#' @param x an object of class \code{cooccurrence} as returned by  \code{\link{cooccurrence}}
#' @param ... not used
#' @return a sparse matrix with in the rows and columns the terms and in the cells how many times
#' the cooccurrence occurred
#' @S3method as.matrix cooccurrence
#' @usage \method{as.matrix}{cooccurrence}(x, ...)
#' @seealso \code{\link{cooccurrence}}
#' @examples 
#' data(brussels_reviews_anno)
#' ## By document, which lemma's co-occur
#' x <- subset(brussels_reviews_anno, xpos %in% c("NN", "JJ") & language %in% "fr")
#' x <- cooccurrence(x, group = "doc_id", term = "lemma")
#' x <- as.matrix(x)
#' dim(x)
#' x[1:3, 1:3]
as.matrix.cooccurrence <- function(x, ...){
  terms <- unique(c(unique(x$term1), unique(x$term2)))
  result <- Matrix::sparseMatrix(i=match(x=x$term1, table=terms), 
                              j = match(x=x$term2, table=terms), 
                              x = x$cooc, 
                              dims = c(length(terms), length(terms)),
                              dimnames = list(terms, terms))
  result
}

