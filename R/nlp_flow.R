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
#' @return a data.table with columns doc_id, term, freq indicating how many times a term occurred in each document.
#' If freq occurred in the input dataset the resulting data will have summed the freq. If freq is not in the dataset,
#' will assume that freq is 1 for each row in the input dataset \code{x}.
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
  dt <- dt[!is.na(txt), list(term = unlist(strsplit(txt, split = split))), by = list(document)]
  dt <- document_term_frequencies(dt)
  dt
}


#' @title Add Term Frequency, Inverse Document Frequency and Okapi BM25 statistics to the output of document_term_frequencies
#' @description Term frequency Inverse Document Frequency (tfidf) is calculated as the multiplication of
#' \itemize{
#' \item Term Frequency (tf): how many times the word occurs in the document / how many words are in the document
#' \item Inverse Document Frequency (idf): log(number of documents / number of documents where the term appears)
#' }
#' The Okapi BM25 statistic is calculated as the multiplication of the inverse document frequency
#' and the weighted term frequency as defined at \url{https://en.wikipedia.org/wiki/Okapi_BM25}.
#' @param x a data.table as returned by \code{document_term_frequencies} containing the columns doc_id, term and freq.
#' @param k parameter k1 of the Okapi BM25 ranking function as defined at \url{https://en.wikipedia.org/wiki/Okapi_BM25}. Defaults to 1.2.
#' @param b parameter b of the Okapi BM25 ranking function as defined at \url{https://en.wikipedia.org/wiki/Okapi_BM25}. Defaults to 0.5.
#' @return a data.table with columns doc_id, term, freq and added to that the computed statistics
#' tf, idf, tfidf, tf_bm25 and bm25.
#' @export
#' @examples 
#' data(brussels_reviews_anno)
#' x <- document_term_frequencies(brussels_reviews_anno[, c("doc_id", "token")])
#' x <- document_term_frequencies_statistics(x)
#' head(x)
document_term_frequencies_statistics <- function(x, k = 1.2, b = 0.75){
  ## r cmd check happiness
  doc_id <- term <- freq <- tf <- idf <- tf_idf <- tf_bm25 <- bm25 <- NULL

  ##
  ## Term Frequency Inverse Document Frequency
  ##   - tf: how many times the word occurs in the document / how many words are in the document
  ##   - idf: number of documents / number of documents where the term appears
  ##
  nr_docs <- length(unique(x$doc_id))
  x <- x[, tf := freq / sum(freq), by = list(doc_id)]
  x <- x[, idf := log(nr_docs / uniqueN(doc_id)), by = list(term)]
  x <- x[, tf_idf := tf * idf, ]
  
  ##
  ## Okapi BM25
  ##   - See https://en.wikipedia.org/wiki/Okapi_BM25
  ##
  average_doc_length <- x[, list(nr_terms = sum(freq)), by = list(doc_id)]
  average_doc_length <- mean(average_doc_length$nr_terms)
  x <- x[, tf_bm25 := (freq * (k + 1)) / (freq + (k * (1 - b + b * (sum(freq) / average_doc_length)))), by = list(doc_id)]
  x <- x[, bm25 := tf_bm25 * idf, ]
  x
}



#' @title Create a document/term matrix from a data.frame with 1 row per document/term
#' @description Create a document/term matrix from a data.frame with 1 row per document/term as returned
#' by \code{\link{document_term_frequencies}}
#' @param x a data.frame with columns doc_id, term and freq indicating how many times a term occurred in that specific document. This is what \code{\link{document_term_frequencies}} returns.\cr
#' This data.frame will be reshaped to a matrix with 1 row per doc_id, the terms will be put 
#' in the columns and the freq in the matrix cells. Note that the column name to use for freq can be set in the \code{weight} argument.
#' @param vocabulary a character vector of terms which should be present in the document term matrix even if they did not occur in \code{x}
#' @param weight a column of \code{x} indicating what to put in the matrix cells. Defaults to 'freq' indicating to use column \code{freq} from \code{x} to put into the matrix cells
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
#' x <- document_term_frequencies(brussels_reviews_anno[, c("doc_id", "lemma")])
#' x <- document_term_frequencies_statistics(x)
#' dtm <- document_term_matrix(x)
#' dtm <- document_term_matrix(x, weight = "freq")
#' dtm <- document_term_matrix(x, weight = "tf_idf")
#' dtm <- document_term_matrix(x, weight = "bm25")
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
document_term_matrix <- function(x, vocabulary, weight = "freq", ...){
  UseMethod("document_term_matrix")
}

#' @describeIn document_term_matrix Construct a document term matrix from a data.frame with columns doc_id, term, freq
#' @export
document_term_matrix.data.frame <- function(x, vocabulary, weight = "freq", ...){
  stopifnot(all(c("doc_id", "term", weight) %in% colnames(x)))
  #stopifnot(ncol(x) == 3)
  
  x$document <- as.character(x$doc_id)
  x$document <- factor(x$document, levels = setdiff(unique(x$document), NA))
  
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
  
  dtm <- sparseMatrix(i=x$document, j = x$term, x = x[[weight]], dims = c(length(doclabels), length(termlabels)),
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
#' @param dtm an object returned by \code{\link{document_term_matrix}}
#' @param minfreq integer with the minimum number of times the term should occur in order to keep the term
#' @param maxterms integer indicating the maximum number of terms which should be kept in the \code{dtm}. The argument is optional. 
#' @param remove_emptydocs logical indicating to remove documents containing no more terms after the term removal is executed. Defaults to \code{TRUE}.
#' @return a sparse Matrix as returned by \code{sparseMatrix} 
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
#' x <- dtm_remove_lowfreq(dtm, minfreq = 10, maxterms = 25, remove_emptydocs = FALSE)
#' dim(x)
dtm_remove_lowfreq <- function(dtm, minfreq=5, maxterms, remove_emptydocs = TRUE){
  dtm <- dtm[, which(Matrix::colSums(dtm) >= minfreq), drop = FALSE]
  if(!missing(maxterms)){
    terms <- head(sort(Matrix::colSums(dtm), decreasing = TRUE), n = maxterms)
    dtm <- dtm[, which(colnames(dtm) %in% names(terms)), drop = FALSE]
  }
  if(remove_emptydocs){
    dtm <- dtm[Matrix::rowSums(dtm) > 0, , drop = FALSE]  
  }
  dtm
}


#' @title Remove terms with high sparsity from a Document-Term-Matrix
#' @description Remove terms with high sparsity from a Document-Term-Matrix and remove documents with no terms.\cr
#' Sparsity indicates in how many documents the term is not occurring.
#' @param dtm an object returned by \code{\link{document_term_matrix}}
#' @param sparsity numeric in 0-1 range indicating the sparsity percent. Defaults to 0.99 meaning drop terms which occur in less than 1 percent of the documents.
#' @param remove_emptydocs logical indicating to remove documents containing no more terms after the term removal is executed. Defaults to \code{TRUE}.
#' @return a sparse Matrix as returned by \code{sparseMatrix} 
#' where terms with high sparsity are removed and documents without any terms are also removed
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
#' x <- dtm_remove_sparseterms(dtm, sparsity = 0.99)
#' dim(x)
#' x <- dtm_remove_sparseterms(dtm, sparsity = 0.99, remove_emptydocs = FALSE)
#' dim(x)
dtm_remove_sparseterms <- function(dtm, sparsity = 0.99, remove_emptydocs = TRUE){
  colfreq <- diff(dtm@p)
  sparseness <- 1 - (colfreq / nrow(dtm))
  keep <- which(sparseness < sparsity)
  if(length(keep) == 0){
    warning(sprintf("No terms which occur more than %s percent of the documents", 100*(1-sparsity)))
    if(remove_emptydocs){
      dtm <- dtm[0, 0, drop = FALSE]
    }else{
      dtm <- dtm[, 0, drop = FALSE]
    }
  }else{
    dtm <- dtm[, keep, drop = FALSE]
    if(remove_emptydocs){
      dtm <- dtm[Matrix::rowSums(dtm) > 0, , drop = FALSE]  
    }  
  }
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
#' @param dtm an object returned by \code{\link{document_term_matrix}} 
#' @param top integer with the number of terms which should be kept as defined by the highest mean tfidf
#' @param cutoff numeric cutoff value to keep only terms in \code{dtm} where the tfidf obtained by \code{dtm_tfidf} is higher than this value
#' @param prob numeric quantile indicating to keep only terms in \code{dtm} where the tfidf obtained by \code{dtm_tfidf} is higher than 
#' the \code{prob} percent quantile
#' @param remove_emptydocs logical indicating to remove documents containing no more terms after the term removal is executed. Defaults to \code{TRUE}.
#' @return a sparse Matrix as returned by \code{sparseMatrix} 
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
#' x <- dtm_remove_tfidf(dtm, top=50, remove_emptydocs = FALSE)
#' dim(x)
#' 
#' ## Keep only terms with tfidf above 1.1
#' x <- dtm_remove_tfidf(dtm, cutoff=1.1)
#' dim(x)
#' 
#' ## Keep only terms with tfidf above the 60 percent quantile
#' x <- dtm_remove_tfidf(dtm, prob=0.6)
#' dim(x)
dtm_remove_tfidf <- function(dtm, top, cutoff, prob, remove_emptydocs = TRUE){
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
  dtm <- dtm[, which(colnames(dtm) %in% terms), drop = FALSE]
  if(remove_emptydocs){
    dtm <- dtm[Matrix::rowSums(dtm) > 0, , drop = FALSE]  
  }
  dtm
}


#' @title Remove terms from a Document-Term-Matrix and keep only documents which have a least some terms
#' @description Remove terms from a Document-Term-Matrix and keep only documents which have a least some terms
#' @param dtm an object returned by \code{\link{document_term_matrix}} 
#' @param terms a character vector of terms which are in \code{colnames(dtm)} and which should be removed
#' @param remove_emptydocs logical indicating to remove documents containing no more terms after the term removal is executed. Defaults to \code{TRUE}.
#' @return a sparse Matrix as returned by \code{sparseMatrix} 
#' where the indicated terms are removed as well as documents with no terms whatsoever
#' @export
#' @examples 
#' data(brussels_reviews_anno)
#' x <- subset(brussels_reviews_anno, xpos == "NN")
#' x <- x[, c("doc_id", "lemma")]
#' x <- document_term_frequencies(x)
#' dtm <- document_term_matrix(x)
#' dim(dtm)
#' x <- dtm_remove_terms(dtm, terms = c("appartement", "casa", "centrum", "ciudad"))
#' dim(x)
#' x <- dtm_remove_terms(dtm, terms = c("appartement", "casa", "centrum", "ciudad"), 
#'                       remove_emptydocs = FALSE)
#' dim(x)
dtm_remove_terms <- function(dtm, terms, remove_emptydocs = TRUE){
  dtm <- dtm[, which(!colnames(dtm) %in% terms), drop = FALSE]
  if(remove_emptydocs){
    dtm <- dtm[Matrix::rowSums(dtm) > 0, , drop = FALSE]  
  }
  dtm
}

#' @title Pearson Correlation for Sparse Matrices
#' @description Pearson Correlation for Sparse Matrices. 
#' More memory and time-efficient than \code{cor(as.matrix(x))}. 
#' @param x A matrix, potentially a sparse matrix such as a "dgCMatrix" object 
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
#' @param x a sparse matrix such as a "dgCMatrix" object which is returned by \code{\link{document_term_matrix}}
#' @param y a sparse matrix such as a "dgCMatrix" object which is returned by \code{\link{document_term_matrix}}
#' @return a sparse matrix where either rows are put below each other in case of \code{dtm_rbind}
#' or columns are put next to each other in case of \code{dtm_cbind}
#' @seealso \code{\link{document_term_matrix}}
#' @name dtm_bind
#' @aliases dtm_rbind dtm_cbind
#' @export
#' @examples 
#' data(brussels_reviews_anno)
#' x <- brussels_reviews_anno
#' 
#' ## rbind
#' dtm1 <- document_term_frequencies(x = subset(x, doc_id %in% c("10049756", "10284782")),
#'                                   document = "doc_id", term = "token")
#' dtm1 <- document_term_matrix(dtm1)
#' dtm2 <- document_term_frequencies(x = subset(x, doc_id %in% c("10789408", "12285061", "35509091")),
#'                                   document = "doc_id", term = "token")
#' dtm2 <- document_term_matrix(dtm2)
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
#' dtm2 <- document_term_matrix(dtm2)
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
  cbind2(x[r, , drop = FALSE], y[r, , drop = FALSE])
}

#' @export
#' @rdname dtm_bind
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
  rbind2(x[, r, drop = FALSE], y[, r, drop = FALSE])
}


#' @title Column sums and Row sums for document term matrices
#' @description Column sums and Row sums for document term matrices
#' @param dtm an object returned by \code{\link{document_term_matrix}}
#' @return a vector of row/column sums with corresponding names
#' @export
#' @aliases dtm_colsums dtm_rowsums
#' @examples 
#' x <- data.frame(
#'  doc_id = c(1, 1, 2, 3, 4), 
#'  term = c("A", "C", "Z", "X", "G"), 
#'  freq = c(1, 5, 7, 10, 0))
#' dtm <- document_term_matrix(x)
#' x <- dtm_colsums(dtm)
#' x
#' x <- dtm_rowsums(dtm)
#' head(x)
dtm_colsums <- function(dtm){
  Matrix::colSums(dtm)
}

#' @export
#' @rdname dtm_colsums
dtm_rowsums <- function(dtm){
  Matrix::rowSums(dtm)
}




#' @title Compare term usage across 2 document groups using the Chi-square Test for Count Data
#' @description Perform a \code{\link{chisq.test}} to compare if groups of documents have more prevalence of specific terms.\cr
#' The function looks to each term in the document term matrix and applies a \code{\link{chisq.test}} comparing the frequency 
#' of occurrence of each term compared to the other terms in the document group.
#' @param dtm a document term matrix: an object returned by \code{\link{document_term_matrix}}
#' @param groups a logical vector with 2 groups (TRUE / FALSE) where the size of the \code{groups} vector 
#' is the same as the number of rows of \code{dtm} and where element i corresponds row i of \code{dtm}
#' @param correct passed on to \code{\link{chisq.test}}
#' @param ... further arguments passed on to \code{\link{chisq.test}}
#' @export
#' @return a data.frame with columns term, chisq, p.value, freq, freq_true, freq_false indicating for each term in the \code{dtm},
#' how frequently it occurs in each group, the Chi-Square value and it's corresponding p-value.
#' @examples 
#' data(brussels_reviews_anno)
#' ##
#' ## Which nouns occur in text containing the term 'centre'
#' ##
#' x <- subset(brussels_reviews_anno, xpos == "NN" & language == "fr")
#' x <- x[, c("doc_id", "lemma")]
#' x <- document_term_frequencies(x)
#' dtm <- document_term_matrix(x)
#' relevant <- dtm_chisq(dtm, groups = dtm[, "centre"] > 0)
#' head(relevant, 10)
#' 
#' ##
#' ## Which adjectives occur in text containing the term 'hote'
#' ##
#' x <- subset(brussels_reviews_anno, xpos == "JJ" & language == "fr")
#' x <- x[, c("doc_id", "lemma")]
#' x <- document_term_frequencies(x)
#' dtm <- document_term_matrix(x)
#' 
#' group <- subset(brussels_reviews_anno, lemma %in% "hote")
#' group <- rownames(dtm) %in% group$doc_id
#' relevant <- dtm_chisq(dtm, groups = group)
#' head(relevant, 10)
#' 
#' 
#' \dontrun{
#' # do not show scientific notation of the p-values
#' options(scipen = 100)
#' head(relevant, 10)
#' }
dtm_chisq <- function(dtm, groups, correct = TRUE, ...){
  stopifnot(is.logical(groups))
  stopifnot(length(unique(groups)) == 2)
  stopifnot(length(groups) == nrow(dtm))
  recode <- function(x, from, to){
    to[match(x, from)]
  }
  
  DTM <- dtm_reverse(dtm)
  DTM <- data.frame(doc_id = recode(DTM$doc_id, from = rownames(dtm), to = groups), 
                    term = DTM$term, 
                    freq = DTM$freq, stringsAsFactors = FALSE)
  DTM <- document_term_frequencies(DTM)
  DTM <- document_term_matrix(DTM)
  
  freq_true  <- DTM["TRUE" , , drop = TRUE]
  freq_false <- DTM["FALSE", , drop = TRUE]
  
  contingencies <- data.frame(term = colnames(DTM), 
                              term_freq_true  = freq_true, 
                              term_freq_false = freq_false,
                              otherterms_freq_true  = sum(freq_true) - freq_true, 
                              otherterms_freq_flase = sum(freq_false) - freq_false, 
                              stringsAsFactors = FALSE)
  result <- mapply(term = contingencies$term, 
                   a = contingencies$term_freq_true, b = contingencies$term_freq_false, 
                   c = contingencies$otherterms_freq_true, d = contingencies$otherterms_freq_flase, 
                   FUN = function(term, a, b, c, d){
                     tab <- matrix(c(a, b, c, d), nrow = 2, ncol = 2, byrow = TRUE)
                     suppressWarnings(result <- chisq.test(tab, correct = correct, ...))
                     list(term = term,
                          chisq = result$statistic, 
                          p.value = result$p.value,
                          freq = a + b,
                          freq_true = a, 
                          freq_false = b)
                   }, SIMPLIFY = FALSE)
  result <- data.table::rbindlist(result)
  result <- result[order(result$chisq, decreasing = TRUE), ]
  result <- data.table::setDF(result)
  result
}



#' @title Make sure a document term matrix has exactly the specified rows and columns
#' @description Makes sure the document term matrix has exactly the rows and columns which you specify. If missing rows or columns
#' are occurring, the function fills these up either with empty cells or with the value that you provide. See the examples.
#' @param dtm a document term matrix: an object returned by \code{\link{document_term_matrix}}
#' @param rows a character vector of row names which \code{dtm} should have
#' @param columns a character vector of column names which \code{dtm} should have
#' @param fill a value to use to fill up missing rows / columns. Defaults to using an empty cell.
#' @return the sparse matrix \code{dtm} with exactly the specified rows and columns
#' @export
#' @seealso \code{\link{document_term_matrix}}
#' @examples 
#' x <- data.frame(doc_id = c("doc_1", "doc_1", "doc_1", "doc_2"), 
#'                 text = c("a", "a", "b", "c"), 
#'                 stringsAsFactors = FALSE)
#' dtm <- document_term_frequencies(x)
#' dtm <- document_term_matrix(dtm)
#' dtm
#' dtm_conform(dtm, 
#'             rows = c("doc_1", "doc_2", "doc_3"), columns = c("a", "b", "c", "Z", "Y"))
#' dtm_conform(dtm, 
#'             rows = c("doc_1", "doc_2", "doc_3"), columns = c("a", "b", "c", "Z", "Y"), 
#'             fill = 1)
#' dtm_conform(dtm, rows = c("doc_1", "doc_3"), columns = c("a", "b", "c", "Z", "Y"))
#' dtm_conform(dtm, columns = c("a", "b", "Z"))
#' dtm_conform(dtm, rows = c("doc_1"))
#' dtm_conform(dtm, rows = character())
#' dtm_conform(dtm, columns = character())
#' dtm_conform(dtm, rows = character(), columns = character())
#' 
#' ##
#' ## Some examples on border line cases
#' ##
#' special1 <- dtm[, character()]
#' special2 <- dtm[character(), character()]
#' special3 <- dtm[character(), ]
#' 
#' dtm_conform(special1, 
#'             rows = c("doc_1", "doc_2", "doc_3"), columns = c("a", "b", "c", "Z", "Y"))
#' dtm_conform(special1, 
#'             rows = c("doc_1", "doc_2", "doc_3"), columns = c("a", "b", "c", "Z", "Y"), 
#'             fill = 1)
#' dtm_conform(special1, rows = c("doc_1", "doc_3"), columns = c("a", "b", "c", "Z", "Y"))
#' dtm_conform(special1, columns = c("a", "b", "Z"))
#' dtm_conform(special1, rows = c("doc_1"))
#' dtm_conform(special1, rows = character())
#' dtm_conform(special1, columns = character())
#' dtm_conform(special1, rows = character(), columns = character())
#' 
#' dtm_conform(special2, 
#'             rows = c("doc_1", "doc_2", "doc_3"), columns = c("a", "b", "c", "Z", "Y"))
#' dtm_conform(special2, 
#'             rows = c("doc_1", "doc_2", "doc_3"), columns = c("a", "b", "c", "Z", "Y"), 
#'             fill = 1)
#' dtm_conform(special2, rows = c("doc_1", "doc_3"), columns = c("a", "b", "c", "Z", "Y"))
#' dtm_conform(special2, columns = c("a", "b", "Z"))
#' dtm_conform(special2, rows = c("doc_1"))
#' dtm_conform(special2, rows = character())
#' dtm_conform(special2, columns = character())
#' dtm_conform(special2, rows = character(), columns = character())
#' 
#' dtm_conform(special3, 
#'             rows = c("doc_1", "doc_2", "doc_3"), columns = c("a", "b", "c", "Z", "Y"))
#' dtm_conform(special3, 
#'             rows = c("doc_1", "doc_2", "doc_3"), columns = c("a", "b", "c", "Z", "Y"), 
#'             fill = 1)
#' dtm_conform(special3, rows = c("doc_1", "doc_3"), columns = c("a", "b", "c", "Z", "Y"))
#' dtm_conform(special3, columns = c("a", "b", "Z"))
#' dtm_conform(special3, rows = c("doc_1"))
#' dtm_conform(special3, rows = character())
#' dtm_conform(special3, columns = character())
#' dtm_conform(special3, rows = character(), columns = character())
dtm_conform <- function(dtm, rows, columns, fill){
  if(!missing(columns)){
    missing_terms <- setdiff(columns, colnames(dtm))
    if(length(missing_terms)){
      if(!missing(fill)){
        extra <- expand.grid(i = seq_len(nrow(dtm)), j = length(missing_terms))
        extra <- Matrix::sparseMatrix(dims = c(nrow(dtm), length(missing_terms)), dimnames = list(rownames(dtm), missing_terms), i = extra$i, j = extra$j, x = fill)
      }else{
        extra <- Matrix::sparseMatrix(dims = c(nrow(dtm), length(missing_terms)), dimnames = list(rownames(dtm), missing_terms), i = {}, j = {})  
      }
      if(nrow(dtm) == 0){
        dtm <- methods::cbind2(dtm, extra)  
      }else{
        dtm <- dtm_cbind(dtm, extra)
      }
    }  
  }
  if(!missing(rows)){
    missing_docs <- setdiff(rows, rownames(dtm))
    if(length(missing_docs) > 0){
      if(!missing(fill)){
        extra <- expand.grid(i = seq_len(length(missing_docs)), j = ncol(dtm))
        extra <- Matrix::sparseMatrix(dims = c(length(missing_docs), ncol(dtm)), dimnames = list(missing_docs, colnames(dtm)), i = extra$i, j = extra$j, x = fill)
      }else{
        extra <- Matrix::sparseMatrix(dims = c(length(missing_docs), ncol(dtm)), dimnames = list(missing_docs, colnames(dtm)), i = {}, j = {})
      }
      if(ncol(dtm) == 0){
        dtm <- methods::rbind2(dtm, extra)  
      }else{
        dtm <- udpipe::dtm_rbind(dtm, extra)  
      }
    }  
  }
  if(!missing(rows) & !missing(columns)){
    dtm <- dtm[rows, columns, drop = FALSE]  
  }else if(!missing(rows)){
    dtm <- dtm[rows, , drop = FALSE]  
  }else if(!missing(columns)){
    dtm <- dtm[, columns, drop = FALSE]  
  }
  dtm
}



#' @title Semantic Similarity to an Singular Value Decomposition
#' @description Calculate the similarity of a document term matrix to a set of terms based on 
#' an Singular Value Decomposition (SVD) embedding matrix.\cr
#' This can be used to easily construct a sentiment score based on the latent scale defined by a set of positive or negative terms.
#' @param dtm a sparse matrix such as a "dgCMatrix" object which is returned by \code{\link{document_term_matrix}} containing frequencies of terms for each document
#' @param embedding a matrix containing the \code{v} element from an singular value decomposition with the right singular vectors. 
#' The rownames of that matrix should contain terms which are available in the \code{colnames(dtm)}. See the examples.
#' @param weights a numeric vector with weights giving your definition of which terms are positive or negative, 
#' The names of this vector should be terms available in the rownames of the embedding matrix. See the examples.
#' @param terminology a character vector of terms to limit the calculation of the similarity for the \code{dtm} to the linear combination of the weights. 
#' Defaults to all terms from the \code{embedding} matrix.
#' @param type either 'cosine' or 'dot' indicating to respectively calculate cosine similarities or inner product similarities between the \code{dtm} and the SVD embedding space. Defaults to 'cosine'.
#' @export
#' @return an object of class 'svd_similarity' which is a list with elements
#' \itemize{
#' \item weights: The weights used. These are scaled to sum up to 1 as well on the positive as the negative side
#' \item type: The type of similarity calculated (either 'cosine' or 'dot')
#' \item terminology: A data.frame with columns term, freq and similarity where similarity indicates 
#' the similarity between the term and the SVD embedding space of the weights and freq is how frequently the term occurs in the \code{dtm}. 
#' This dataset is sorted in descending order by similarity.
#' \item similarity: A data.frame with columns doc_id and similarity indicating the similarity between
#' the \code{dtm} and the SVD embedding space of the weights. The doc_id is the identifier taken from the rownames of \code{dtm}.
#' \item scale: A list with elements terminology and weights 
#' indicating respectively the similarity in the SVD embedding space
#' between the \code{terminology} and each of the weights and between the weight terms itself
#' }
#' @seealso \url{https://en.wikipedia.org/wiki/Latent_semantic_analysis}
#' @examples
#' data("brussels_reviews_anno", package = "udpipe")
#' x <- subset(brussels_reviews_anno, language %in% "nl" & (upos %in% "ADJ" | lemma %in% "niet"))
#' dtm <- document_term_frequencies(x, document = "doc_id", term = "lemma")
#' dtm <- document_term_matrix(dtm)
#' dtm <- dtm_remove_lowfreq(dtm, minfreq = 3)
#' 
#' ## Function performing Singular Value Decomposition on sparse/dense data
#' dtm_svd <- function(dtm, dim = 5, type = c("RSpectra", "svd"), ...){
#'   type <- match.arg(type)
#'   if(type == "svd"){
#'     SVD <- svd(dtm, nu = 0, nv = dim, ...)
#'   }else if(type == "RSpectra"){
#'     #Uncomment this if you want to use the faster sparse SVD by RSpectra
#'     #SVD <- RSpectra::svds(dtm, nu = 0, k = dim, ...)
#'   }
#'   rownames(SVD$v) <- colnames(dtm)
#'   SVD$v
#' }
#' #embedding <- dtm_svd(dtm, dim = 5)
#' embedding <- dtm_svd(dtm, dim = 5, type = "svd")
#' 
#' ## Define positive / negative terms and calculate the similarity to these
#' weights <- setNames(c(1, 1, 1, 1, -1, -1, -1, -1),
#'                     c("fantastisch", "schoon", "vriendelijk", "net",
#'                       "lawaaiig", "lastig", "niet", "slecht"))
#' scores <- dtm_svd_similarity(dtm, embedding = embedding, weights = weights)
#' scores
#' str(scores$similarity)
#' hist(scores$similarity$similarity)
#' 
#' plot(scores$terminology$similarity_weight, log(scores$terminology$freq), 
#'      type = "n")
#' text(scores$terminology$similarity_weight, log(scores$terminology$freq), 
#'      labels = scores$terminology$term)
dtm_svd_similarity <- function(dtm, embedding, weights, terminology = rownames(embedding), type = c("cosine", "dot")){
  doc_id <- term <- prop <- in_terminology <- NULL
  embedding_similarity <- function(x, y, type = c("cosine", "dot")) {
    if(!is.matrix(x)){
      x <- matrix(x, nrow = 1)
    }
    if(!is.matrix(y)){
      y <- matrix(y, nrow = 1)
    }
    type <- match.arg(type)
    
    if(type == "dot"){
      similarities <- tcrossprod(x, y)
    }else if(type == "cosine"){
      normx <- sqrt(rowSums(x^2))
      normy <- sqrt(rowSums(y^2))
      similarities <- tcrossprod(x, y) / (normx %o% normy)
    }
    similarities
  }
  setNames <- function(x, labels){
    names(x) <- labels
    x
  }
  ##
  ## DATA CHECKS
  ##
  type <- match.arg(type)
  stopifnot(is.matrix(embedding))
  stopifnot(is.vector(weights))
  
  object <- list(dim = ncol(embedding),
                 terminology = rownames(embedding),
                 embedding = as.matrix(embedding))
  
  if(missing(terminology)){
    terminology <- object$terminology
  }else{
    terminology <- intersect(terminology, object$terminology)
  }
  not_known_weights <- setdiff(names(weights), object$terminology)
  if(length(not_known_weights) > 0){
    warning(sprintf("Removing '%s' from weights as these are not part of rownames(embedding)", paste(not_known_weights, collapse = ", ")))
    weights <- weights[!names(weights) %in% not_known_weights]
  }
  stopifnot(length(weights) > 0)
  if(length(unique(sign(weights))) != 2){
    stop("weights should contain positive as well as negative values")
  }
  
  ## rescale weights to make sure positive values sum to 1 and negative also sum to -1
  weights_scaled <- data.frame(term = names(weights), 
                               weight = weights, 
                               sign = sign(weights),
                               stringsAsFactors = FALSE)
  weights_scaled <- lapply(X = split(weights_scaled, weights_scaled$sign),
                           FUN = function(x){
                             x$weight_scaled = x$weight / abs(sum(x$weight))
                             x
                           })
  weights_scaled <- do.call(rbind, weights_scaled)
  weights_scaled <- setNames(weights_scaled$weight_scaled, weights_scaled$term)
  #weights_scaled <- setNames(weights_scaled$weight / nrow(weights_scaled), weights_scaled$term)
  
  
  ####################################################################################
  ## Calculate embedding similarity of terminology to weights, weights-to-weights
  ## and the embedding space of the linear combination of the weights
  ##
  weightsterms <- names(weights_scaled)
  
  similarity_terminology_to_weights <- embedding_similarity(
    object$embedding[terminology, , drop = FALSE],
    object$embedding[weightsterms, , drop = FALSE],
    type = type)

  similarity_weights_to_weights <- embedding_similarity(
    object$embedding[weightsterms, , drop = FALSE],
    object$embedding[weightsterms, , drop = FALSE],
    type = type)
  
  ## weighted term-to-weights similarity reflecting a scale on which -1 to +1 range lies
  weightspace <- similarity_terminology_to_weights %*% weights_scaled
  weightspace <- weightspace[, 1]
  
  ##
  ## Predict (for terms part of the terminology, get the weights embedding space)
  ##
  freq <- dtm_colsums(dtm)
  dtf <- dtm_reverse(dtm)
  dtf <- data.table::setDT(dtf)
  dtf <- dtf[, in_terminology := term %in% terminology, ]
  #dtf <- dtf[, prop := as.numeric(freq / sum(freq)), by = list(doc_id)]
  dtf <- dtf[, prop := as.numeric(ifelse(any(in_terminology), freq[in_terminology] / sum(freq[in_terminology]), 0)), by = list(doc_id)]
  dtm <- document_term_matrix(dtf, weight = "prop", terminology = terminology)
  dtm <- dtm[, terminology, drop = FALSE]
  
  scores <- dtm %*% weightspace
  scores <- scores[, 1]
  scores <- data.frame(doc_id = names(scores), 
                       similarity = as.numeric(scores), 
                       stringsAsFactors = FALSE)
  
  terminology_similarity <- sort(weightspace, decreasing = TRUE)
  terminology_similarity <- data.frame(
    term = names(terminology_similarity), 
    freq = txt_recode(names(terminology_similarity), from = names(freq), to = as.integer(freq)),
    similarity_weight = as.numeric(terminology_similarity),
    stringsAsFactors = FALSE)
  
  result <- list(weights = weights_scaled,
                 type = type,
                 terminology = terminology_similarity,
                 similarity = scores,
                 scale = list(
                   terminology = similarity_terminology_to_weights,
                   weights = similarity_weights_to_weights))
  class(result) <- "svd_similarity"
  result
}

#' @export
print.svd_similarity <- function(x, n = 7, digits = 2, ...){
  cat(sprintf("Latent Semantic Scaling using %s similarity on SVD matrix", x$type), sep = "\n")
  cat(sprintf("Weights: %s", paste(sprintf("%s %s", names(x$weights), round(x$weights, 4)), collapse = ", ")), sep = "\n")
  cat(sprintf("Top %s most similar terms on the high end of the scale", n), sep = "\n")
  elements <- head(x$terminology, n = n, sep = "\n")
  mapply(term = elements$term, freq = elements$freq, similarity = elements$similarity, FUN=function(term, freq, similarity){
    cat(sprintf(" - %s %s (frequency: %s)", term, round(similarity, digits = digits), freq), sep = "\n")
  })
  cat(sprintf("Top %s most similar terms on the low end of the scale", n), sep = "\n")
  elements <- tail(x$terminology, n = n, sep = "\n")
  elements <- elements[order(elements$similarity, decreasing = FALSE), ]
  mapply(term = elements$term, freq = elements$freq, similarity = elements$similarity, FUN=function(term, freq, similarity){
    cat(sprintf(" - %s %s (frequency: %s)", term, round(similarity, digits = digits), freq), sep = "\n")
  })
  invisible()
}