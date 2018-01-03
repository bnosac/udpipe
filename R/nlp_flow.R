#' @title Aggregate a data.frame to the document/term level by calculating how many times a term occurs per document
#' @description Aggregate a data.frame to the document/term level by calculating how many times a term occurs per document
#' @param x a data.frame or data.table containing a field which can be considered 
#' as a document (defaults to the first column in \code{x}) and a field which can be considered as a term 
#' (defaults to the second column in \code{x}). If the dataset also contains a column called 'freq', this will be summed over instead of counting the number 
#' of rows occur by document/term combination.\cr
#' If \code{x} is a character vector containing several terms, the text will be split by the argument \code{split}
#' before doing the agregation at the document/term level.
#' @param document If \code{x} is a data.frame, the column in \code{x} which identifies a document. If \code{x}
#' is a character vector then \code{document} is a vector of the same length as \code{x} where \code{document[i]} is the
#' document id which corresponds to the text in \code{x[i]}.
#' @param term If \code{x} is a data.frame, the column in \code{x} which identifies a term. Defaults to the second column
#' in \code{x}.
#' @param ... further arguments passed on to the methods
#' @return a data.table with columns document, term and the summed freq. If freq is not in the dataset,
#' will assume that freq is 1 for each row in \code{x}.
#' @export
#' @examples 
#' ##
#' ## Calculate document_term_frequencies on a data.frame
#' ##
#' data(brussels_reviews_anno)
#' x <- document_term_frequencies(brussels_reviews_anno[, c("doc_id", "token")])
#' x <- document_term_frequencies(brussels_reviews_anno[, c("doc_id", "lemma")])
#' str(x)
#' 
#' brussels_reviews_anno$my_doc_id <- paste(brussels_reviews_anno$doc_id, 
#'                                          brussels_reviews_anno$sentence_id)
#' x <- document_term_frequencies(brussels_reviews_anno[, c("my_doc_id", "lemma")])
#' 
#' ##
#' ## Calculate document_term_frequencies on a character vector
#' ##
#' data(brussels_reviews)
#' x <- document_term_frequencies(x = brussels_reviews$feedback, document = brussels_reviews$id, 
#'                                split = " ")
#' x <- document_term_frequencies(x = brussels_reviews$feedback, document = brussels_reviews$id, 
#'                                split = "[[:space:][:punct:][:digit:]]+")
#'                                
#' ##
#' ## document-term-frequencies on several fields to easily include bigram and trigrams
#' ##
#' library(data.table)
#' x <- as.data.table(brussels_reviews_anno)
#' x <- x[, token_bigram := txt_nextgram(token, n = 2), by = list(doc_id, sentence_id)]
#' x <- x[, token_trigram := txt_nextgram(token, n = 3), by = list(doc_id, sentence_id)]
#' x <- document_term_frequencies(x = x, 
#'                                document = "doc_id", 
#'                                term = c("token", "token_bigram", "token_trigram"))
#' head(x)
document_term_frequencies <- function(x, document, ...){
  UseMethod("document_term_frequencies")
}

#' @describeIn document_term_frequencies Create a data.frame with one row per document/term combination indicating the frequency of the term in the document
#' @export
document_term_frequencies.data.frame <- function(x, document = colnames(x)[1], term = colnames(x)[2], ...){
  result <- list()
  for(field in term){
    result[[field]] <- dtf(x, document, term = field, ...)
  }
  result <- data.table::rbindlist(result)
  result <- dtf(result)
  result
}

dtf <- function(x, document = colnames(x)[1], term = colnames(x)[2], ...){
  # r cmd check happiness
  freq <- .N <- NULL
  
  x <- setDT(x)
  if("freq" %in% colnames(x)){
    result <- x[, list(freq = as.integer(sum(freq))), by = c(document, term)]  
  }else{
    result <- x[, list(freq = as.integer(.N)), by = c(document, term)]  
  }
  if(!"term" %in% colnames(result)){
    setnames(result, old = colnames(result)[2], new = c("term"))  
  }
  if(!"doc_id" %in% colnames(result)){
    setnames(result, old = colnames(result)[1], new = c("doc_id"))  
  }
  result<- result[!is.na(term), ]
  result
}


#' @describeIn document_term_frequencies Create a data.frame with one row per document/term combination indicating the frequency of the term in the document
#' @param split The regular expression to be used if \code{x} is a character vector. 
#' This will split the character vector \code{x} in pieces by the provides split argument. 
#' Defaults to splitting according to spaces/punctuations/digits.
#' @export
document_term_frequencies.character <- function(x, document=paste("doc", seq_along(x), sep=""), split = "[[:space:][:punct:][:digit:]]+", ...){
  txt <- NULL
  dt <- data.table(document = as.character(document), txt = x)
  dt <- dt[!is.na(txt), list(term = strsplit(txt, split = split)[[1]]), by = list(document)]
  dt <- document_term_frequencies(dt)
  dt
}





#' @title Create a document/term matrix from a data.frame with 1 row per document/term
#' @description Create a document/term matrix from a data.frame with 1 row per document/term as returned
#' by \code{\link{document_term_frequencies}}
#' @param x a data.frame with columns document, term and freq indicating how many times a term occurred in that specific document.
#' This is what \code{\link{document_term_frequencies}} returns.
#' @param vocabulary a character vector of terms which should be present in the document term matrix even if they did not occur in the \code{x}
#' @param ... further arguments currently not used
#' @return an sparse object of class dgCMatrix with in the rows the documents and in the columns the terms containing the frequencies
#' provided in \code{x} extended with terms which were not in \code{x} but were provided in \code{vocabulary}.
#' The rownames of this resulting object contain the doc_id from \code{x}
#' @export
#' @seealso \code{\link[Matrix]{sparseMatrix}}, \code{\link{document_term_frequencies}}
#' @examples 
#' x <- data.frame(doc_id = c(1, 1, 2, 3, 4), 
#'  term = c("A", "C", "Z", "X", "G"), 
#'  freq = c(1, 5, 7, 10, 0))
#' document_term_matrix(x)
#' document_term_matrix(x, vocabulary = LETTERS)
#' 
#' ## Example on larger dataset
#' data(brussels_reviews_anno)
#' x <- document_term_frequencies(brussels_reviews_anno[, c("doc_id", "lemma")])
#' dtm <- document_term_matrix(x)
#' dim(dtm)
#' 
#' ## example showing the vocubulary argument
#' ## allowing you to making sure terms which are not in the data are provided in the resulting dtm
#' allterms <- unique(x$term)
#' dtm <- document_term_matrix(head(x, 1000), vocabulary = allterms)
#' 
#' ##
#' ## Example adding bigrams/trigrams to the document term matrix
#' ## Mark that this can also be done using ?dtm_cbind
#' ##
#' library(data.table)
#' x <- as.data.table(brussels_reviews_anno)
#' x <- x[, token_bigram := txt_nextgram(token, n = 2), by = list(doc_id, sentence_id)]
#' x <- x[, token_trigram := txt_nextgram(token, n = 3), by = list(doc_id, sentence_id)]
#' x <- document_term_frequencies(x = x, 
#'                                document = "doc_id", 
#'                                term = c("token", "token_bigram", "token_trigram"))
#' dtm <- document_term_matrix(x)
document_term_matrix <- function(x, vocabulary, ...){
  UseMethod("document_term_matrix")
}

#' @describeIn document_term_matrix Construct a document term matrix from a data.frame with columns doc_id, term, freq
#' @export
document_term_matrix.data.frame <- function(x, vocabulary, ...){
  stopifnot(all(c("doc_id", "term", "freq") %in% colnames(x)))
  stopifnot(ncol(x) == 3)
  
  x$document <- factor(as.character(x$doc_id))
  
  term <- as.character(x$term)
  if(!missing(vocabulary)){
    x$term <- factor(term, levels = setdiff(unique(c(vocabulary, levels(factor(term)))), NA))
  }else{
    x$term <- factor(term)
  }
  
  doclabels <- levels(x$document)
  termlabels <- levels(x$term)
  
  x$document <- as.integer(x$document)
  x$term <- as.integer(x$term)
  
  dtm <- sparseMatrix(i=x$document, j = x$term, x = x$freq, dims = c(length(doclabels), length(termlabels)),
                      dimnames = list(doclabels, termlabels))
  dtm
}


#' @describeIn document_term_matrix Convert an object of class \code{DocumentTermMatrix} from the tm package to a sparseMatrix
#' @export
document_term_matrix.DocumentTermMatrix <- function(x, ...){
  dtm <- Matrix::sparseMatrix(i=x$i, j = x$j, x = x$v, 
                              dims = c(x$nrow, x$ncol),
                              dimnames = x$dimnames)
  dtm
}

#' @describeIn document_term_matrix Convert an object of class \code{TermDocumentMatrix} from the tm package to a sparseMatrix with
#' the documents in the rows and the terms in the columns
#' @export
document_term_matrix.TermDocumentMatrix <- function(x, ...){
  dtm <- document_term_matrix.DocumentTermMatrix(x)
  dtm <- Matrix::t(dtm)
  dtm
}

#' @describeIn document_term_matrix Convert an object of class \code{simple_triplet_matrix} from the slam package to a sparseMatrix
#' @export
document_term_matrix.simple_triplet_matrix <- function(x, ...){
  dtm <- Matrix::sparseMatrix(i=x$i, j = x$j, x = x$v, 
                              dims = c(x$nrow, x$ncol),
                              dimnames = x$dimnames)
  dtm
}




#' @title Inverse operation of the document_term_matrix function
#' @description Inverse operation of the \code{\link{document_term_matrix}} function. 
#' Creates frequency table which contains 1 row per document/term
#' @param x an object as returned by \code{\link{document_term_matrix}}
#' @return a data.frame with columns doc_id, term and freq where freq is just the value in each
#' cell of the \code{x}
#' @export
#' @seealso \code{\link{document_term_matrix}}
#' @examples 
#' x <- data.frame(
#'  doc_id = c(1, 1, 2, 3, 4), 
#'  term = c("A", "C", "Z", "X", "G"), 
#'  freq = c(1, 5, 7, 10, 0))
#' dtm <- document_term_matrix(x)
#' dtm_reverse(dtm)
dtm_reverse <- function(x){
  m <- Matrix::summary(x)
  data.frame(doc_id = rownames(x)[m$i],
             term = colnames(x)[m$j],
             freq = m$x,
             stringsAsFactors = FALSE)
}



#' @title Remove terms occurring with low frequency from a Document-Term-Matrix and documents with no terms
#' @description Remove terms occurring with low frequency from a Document-Term-Matrix and documents with no terms
#' @param dtm an object returned by \code{\link{document_term_matrix}} or an object of class DocumentTermMatrix
#' @param minfreq integer with the minimum number of times the term should occur in order to keep the term
#' @param maxterms integer indicating the maximum number of terms which should be kept in the \code{dtm}. The argument is optional. 
#' @return a sparse Matrix as returned by \code{sparseMatrix} or an object of class \code{DocumentTermMatrix} 
#' where terms with low occurrence are removed and documents without any terms are also removed
#' @export
#' @examples 
#' data(brussels_reviews_anno)
#' x <- subset(brussels_reviews_anno, xpos == "NN")
#' x <- x[, c("doc_id", "lemma")]
#' x <- document_term_frequencies(x)
#' dtm <- document_term_matrix(x)
#' 
#' 
#' ## Remove terms with low frequencies and documents with no terms
#' x <- dtm_remove_lowfreq(dtm, minfreq = 10)
#' dim(x)
#' x <- dtm_remove_lowfreq(dtm, minfreq = 10, maxterms = 25)
#' dim(x)
dtm_remove_lowfreq <- function(dtm, minfreq=5, maxterms){
  dtm <- dtm[, which(Matrix::colSums(dtm) > minfreq)]
  if(!missing(maxterms)){
    terms <- head(sort(Matrix::colSums(dtm), decreasing = TRUE), n = maxterms)
    dtm <- dtm[, which(colnames(dtm) %in% names(terms)), drop = FALSE]
  }
  dtm <- dtm[Matrix::rowSums(dtm) > 0, ]
  dtm
}


#' @title Term Frequency - Inverse Document Frequency calculation
#' @description Term Frequency - Inverse Document Frequency calculation.
#' Averaged by each term.
#' @param dtm an object returned by \code{\link{document_term_matrix}}
#' @return a vector with tfidf values, one for each term in the \code{dtm} matrix
#' @export
#' @examples 
#' data(brussels_reviews_anno)
#' x <- subset(brussels_reviews_anno, xpos == "NN")
#' x <- x[, c("doc_id", "lemma")]
#' x <- document_term_frequencies(x)
#' dtm <- document_term_matrix(x)
#' 
#' ## Calculate tfidf
#' tfidf <- dtm_tfidf(dtm)
#' hist(tfidf, breaks = "scott")
#' head(sort(tfidf, decreasing = TRUE))
#' head(sort(tfidf, decreasing = FALSE))
dtm_tfidf <- function(dtm){
  ## number of times word appears / number of words in document, on average if non-missing
  ## times log2(# documents / #documentsXwords)
  terms <- colnames(dtm)
  m <- Matrix::summary(dtm)
  term_tfidf <- tapply(m$x/Matrix::rowSums(dtm)[m$i], m$j, mean) * 
    log2(nrow(dtm)/Matrix::colSums(dtm > 0))
  names(term_tfidf) <- terms
  term_tfidf
}

#' @title Remove terms from a Document-Term-Matrix and documents with no terms based on the term frequency inverse document frequency
#' @description Remove terms from a Document-Term-Matrix and documents with no terms based on the term frequency inverse document frequency.
#' Either giving in the maximum number of terms (argument \code{top}), the tfidf cutoff (argument \code{cutoff})
#' or a quantile (argument \code{prob})
#' @param dtm an object returned by \code{\link{document_term_matrix}} or an object of class DocumentTermMatrix
#' @param top integer with the number of terms which should be kept as defined by the highest mean tfidf
#' @param cutoff numeric cutoff value to keep only terms in \code{dtm} where the tfidf obtained by \code{dtm_tfidf} is higher than this value
#' @param prob numeric quantile indicating to keep only terms in \code{dtm} where the tfidf obtained by \code{dtm_tfidf} is higher than 
#' the \code{prob} percent quantile
#' @return a sparse Matrix as returned by \code{sparseMatrix} or an object of class \code{DocumentTermMatrix} 
#' where terms with high tfidf are kept and documents without any remaining terms are removed
#' @export
#' @examples 
#' data(brussels_reviews_anno)
#' x <- subset(brussels_reviews_anno, xpos == "NN")
#' x <- x[, c("doc_id", "lemma")]
#' x <- document_term_frequencies(x)
#' dtm <- document_term_matrix(x)
#' dtm <- dtm_remove_lowfreq(dtm, minfreq = 10)
#' dim(dtm)
#' 
#' ## Keep only terms with high tfidf
#' x <- dtm_remove_tfidf(dtm, top=50)
#' dim(x)
#' 
#' ## Keep only terms with tfidf above 1.1
#' x <- dtm_remove_tfidf(dtm, cutoff=1.1)
#' dim(x)
#' 
#' ## Keep only terms with tfidf above the 60 percent quantile
#' x <- dtm_remove_tfidf(dtm, prob=0.6)
#' dim(x)
dtm_remove_tfidf <- function(dtm, top, cutoff, prob){
  tfidf <- dtm_tfidf(dtm)
  if(!missing(top)){
    terms <- head(sort(tfidf, decreasing = TRUE), n = top)
    terms <- names(terms)
  }else if(!missing(cutoff)){
    terms <- tfidf[tfidf >= cutoff]
    terms <- names(terms)
  }else if(!missing(prob)){
    cutoff <- stats::quantile(tfidf, prob)
    terms <- tfidf[tfidf >= cutoff]
    terms <- names(terms)
  }else{
    stop("either provide top, cutoff or prob")
  }
  if(length(terms) == 0){
    stop("no terms found in reducing based on tfidf, consider increasing top or decreasing cutoff/prob")
  }
  dtm <- dtm[, which(colnames(dtm) %in% terms)]
  dtm <- dtm[Matrix::rowSums(dtm) > 0, ]
  dtm
}


#' @title Remove terms from a Document-Term-Matrix and keep only documents which have a least some terms
#' @description Remove terms from a Document-Term-Matrix and keep only documents which have a least some terms
#' @param dtm an object returned by \code{\link{document_term_matrix}} or an object of class DocumentTermMatrix
#' @param terms a character vector of terms which are in \code{colnames(dtm)} and which should be removed
#' @return a sparse Matrix as returned by \code{sparseMatrix} or an object of class \code{DocumentTermMatrix} 
#' where the indicated terms are removed as well as documents with no terms whatsoever
#' @export
#' @examples 
#' data(brussels_reviews_anno)
#' x <- subset(brussels_reviews_anno, xpos == "NN")
#' x <- x[, c("doc_id", "lemma")]
#' x <- document_term_frequencies(x)
#' dtm <- document_term_matrix(x)
#' dim(dtm)
#' dtm <- dtm_remove_terms(dtm, terms = c("appartement", "casa", "centrum", "ciudad"))
#' dim(dtm)
dtm_remove_terms <- function(dtm, terms){
  dtm <- dtm[, which(!colnames(dtm) %in% terms)]
  dtm <- dtm[Matrix::rowSums(dtm) > 0, ]
  dtm
}

#' @title Pearson Correlation for Sparse Matrices
#' @description Pearson Correlation for Sparse Matrices. 
#' More memory and time-efficient than \code{cor(as.matrix(x))}. 
#' @param x A matrix, potentially a sparse matrix such as a "dgTMatrix" object 
#' which is returned by \code{\link{document_term_matrix}}
#' @return a correlation matrix 
#' @seealso \code{\link{document_term_matrix}}
#' @export
#' @examples 
#' x <- data.frame(
#'  doc_id = c(1, 1, 2, 3, 4), 
#'  term = c("A", "C", "Z", "X", "G"), 
#'  freq = c(1, 5, 7, 10, 0))
#' dtm <- document_term_matrix(x)
#' dtm_cor(dtm)
dtm_cor <- function(x) {
  n <- nrow(x)
  covmat <- (as.matrix(Matrix::crossprod(x)) - n * Matrix::tcrossprod(Matrix::colMeans(x))) / (n - 1)
  cormat <- covmat / Matrix::tcrossprod(sqrt(Matrix::diag(covmat)))
  cormat
}


#' @title Combine 2 document term matrices either by rows or by columns
#' @description These 2 methods provide \code{\link{cbind}} and \code{\link{rbind}} functionality 
#' for sparse matrix objects which are returned by \code{\link{document_term_matrix}}. \cr
#' 
#' In case of \code{dtm_cbind}, if the rows are not ordered in the same way in x and y, it will order them based on the rownames. 
#' If there are missing rows these will be filled with NA values. \cr
#' In case of \code{dtm_rbind}, if the columns are not ordered in the same way in x and y, it will order them based on the colnames. 
#' If there are missing columns these will be filled with NA values.
#' @param x a sparse matrix such as a "dgTMatrix" object which is returned by \code{\link{document_term_matrix}}
#' @param y a sparse matrix such as a "dgTMatrix" object which is returned by \code{\link{document_term_matrix}}
#' @return a sparse matrix where either rows are put below each other in case of \code{dtm_rbind}
#' or columns are put next to each other in case of \code{dtm_cbind}
#' @seealso \code{\link{document_term_matrix}}
#' @usage dtm_rbind(x, y) \cr
#' dtm_cbind(x, y) 
#' @name dtm_bind
#' @aliases dtm_rbind dtm_cbind
#' @export
#' @examples 
#' data(brussels_reviews_anno)
#' x <- brussels_reviews_anno
#' 
#' ## rbind
#' dtm1 <- document_term_frequencies(x = subset(x, doc_id %in% c("10049756", "10284782")))
#' dtm1 <- document_term_matrix(dtm1, document = "doc_id", term = c("token")))
#' dtm2 <- document_term_frequencies(x = subset(x, doc_id %in% c("10789408", "12285061", "35509091"))
#' dtm2 <- document_term_matrix(, document = "doc_id", term = c("token")))
#' m <- dtm_rbind(dtm1, dtm2)
#' dim(m)
#' 
#' ## cbind
#' library(data.table)
#' x <- as.data.table(brussels_reviews_anno)
#' x <- x[, token_bigram := txt_nextgram(token, n = 2), by = list(doc_id, sentence_id)]
#' dtm1 <- document_term_frequencies(x = x, document = "doc_id", term = c("token"))
#' dtm1 <- document_term_matrix(dtm1)
#' dtm2 <- document_term_frequencies(x = x, document = "doc_id", term = c("token_bigram"))
#' dtm2 <- document_term_matrix()
#' m <- dtm_cbind(dtm1, dtm2)
#' dim(m)
#' m <- dtm_cbind(dtm1[-c(100, 999), ], dtm2[-1000,])
#' dim(m)
dtm_cbind <- function(x, y){
  if(is.null(rownames(x))) stop("x needs to have rownames")
  if(is.null(rownames(y))) stop("y needs to have rownames")
  if(length(intersect(colnames(x), colnames(y))) > 0) stop("x and y should not have overlapping column names")
  rowsleft <- rownames(x)
  rowsright <- rownames(y)
  r <- union(rowsleft, rowsright)
  addleft <- setdiff(rowsright, rowsleft)
  if(length(addleft) > 0){
    addleft <- Matrix::sparseMatrix(i = integer(), j = integer(), x = NA,
                                    dims = c(length(addleft), ncol(x)), dimnames = list(addleft, colnames(x)))
    x <- methods::rbind2(x, addleft)
  }
  addright <- setdiff(rowsleft, rowsright)
  if(length(addright) > 0){
    addright <- Matrix::sparseMatrix(i = integer(), j = integer(), x = NA,
                                     dims = c(length(addright), ncol(y)), dimnames = list(addright, colnames(y)))
    y <- methods::rbind2(y, addright)
  }
  cbind2(x[r, ], y[r, ])
}

#' @export
dtm_rbind <- function(x, y){
  if(is.null(colnames(x))) stop("x needs to have colnames")
  if(is.null(colnames(y))) stop("y needs to have colnames")
  if(length(intersect(rownames(x), rownames(y))) > 0) stop("x and y should not have overlapping row names")
  colsleft <- colnames(x)
  colsright <- colnames(y)
  r <- union(colsleft, colsright)
  addleft <- setdiff(colsright, colsleft)
  if(length(addleft) > 0){
    addleft <- Matrix::sparseMatrix(i = integer(), j = integer(), x = NA,
                                    dims = c(nrow(x), length(addleft)), dimnames = list(rownames(x), addleft))
    x <- methods::cbind2(x, addleft)
  }
  addright <- setdiff(colsleft, colsright)
  if(length(addright) > 0){
    addright <- Matrix::sparseMatrix(i = integer(), j = integer(), x = NA,
                                     dims = c(nrow(y), length(addright)), dimnames = list(rownames(y), addright))
    y <- methods::cbind2(y, addright)
  }
  rbind2(x[, r], y[, r])
}