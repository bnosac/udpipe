#' @title Extract collocations - a sequence of terms which follow each other
#' @description Collocations are a sequence of words or terms that co-occur more often than would be expected by chance.
#' Common collocation are adjectives + nouns, nouns followed by nouns, verbs and nouns, adverbs and adjectives,
#' verbs and prepositional phrases or verbs and adverbs.\cr
#' This function extracts relevant collocations and computes the following statistics on them
#' which are indicators of how likely two terms are collocated compared to being independent.
#' \itemize{
#'   \item PMI (pointwise mutual information): log2(P(w1w2) / P(w1) P(w2))
#'   \item MD (mutual dependency): log2(P(w1w2)^2 / P(w1) P(w2))
#'   \item LFMD (log-frequency biased mutual dependency): MD + log2(P(w1w2))
#' }
#' As natural language is non random - otherwise you wouldn't understand what I'm saying, 
#' most of the combinations of terms are significant. That's why these indicators of collocation
#' are merely used to order the collocations.
#' @param x a data.frame with one row per term where the sequence of the terms correspond to 
#' the natural order of a text. The data frame \code{x} should also contain 
#' the columns provided in \code{term} and \code{group}
#' @param term a character vector with 1 column from \code{x} which indicates the term
#' @param group a character vector with 1 or several columns from \code{x} which indicates 
#' for example a document id or a sentence id. Collocations will be computed within this 
#' group in order not to find collocations across sentences or documents for example.
#' @param ngram_max integer indicating the size of the collocations. Defaults to 2, indicating
#' to compute bigrams. If set to 3, will find collocations of bigrams and trigrams.
#' @param n_min integer indicating the frequency of how many times a collocation should
#' at least occur in the data in order to be returned. Defaults to 2.
#' @param sep character string with the separator which will be used to \code{paste} together
#' terms which are collocated. Defaults to a space: ' '.
#' @return a data.frame with columns 
#' \itemize{
#' \item ngram: the number of terms which are combined
#' \item collocation: the terms which are combined
#' \item left: the left term of the collocation
#' \item right: the right term of the collocation
#' \item n: the number of times the collocation occurred in the data
#' \item n_left: the number of times the left element of the collocation occurred in the data
#' \item n_right: the number of times the right element of the collocation occurrend in the data
#' \item pmi: the pointwise mutual information
#' \item md: mutual dependency
#' \item lfmd: log-frequency biased mutual dependency
#' }
#' @export
#' @aliases keywords_collocation collocation
#' @examples 
#' data(brussels_reviews_anno)
#' x <- subset(brussels_reviews_anno, language %in% "fr")
#' colloc <- keywords_collocation(x, term = "lemma", group = c("doc_id", "sentence_id"), 
#'                                ngram_max = 3, n_min = 10)
#' head(colloc, 10)
#' 
#' ## Example on finding collocations of nouns preceded by an adjective
#' library(data.table)
#' x <- as.data.table(x)
#' x[, xpos_previous := txt_previous(xpos, n = 1), by = list(doc_id, sentence_id)]
#' x[, xpos_next := txt_next(xpos, n = 1), by = list(doc_id, sentence_id)]
#' x <- subset(x, (xpos %in% c("NN") & xpos_previous %in% c("JJ")) | 
#'                (xpos %in% c("JJ") & xpos_next %in% c("NN")))
#' colloc <- keywords_collocation(x, term = "lemma", group = c("doc_id", "sentence_id"), 
#'                                ngram_max = 2, n_min = 2)
#' head(colloc)
keywords_collocation <- function(x, term, group, ngram_max = 2, n_min = 2, sep = " "){
  ## R CMD check happiness
  txt.original <- .N <- bigram <- left <- right <- n <- n_left <- n_right <- pmi <- md <- lfmd <- NULL
  ngram_max <- as.integer(ngram_max)
  stopifnot(length(term) == 1)
  stopifnot(is.data.frame(x))
  cols <- setdiff(c(term, group), colnames(x))
  if(length(cols) > 0){
    stop(sprintf("%s are not columns of x", paste(cols, collapse = ", ")))
  }
  stopifnot(all(c(term, group) %in% colnames(x)))
  stopifnot(ngram_max >= 2)
  
  ## Start
  groupby <- sprintf("grp_%s", seq_along(group))
  if(is.data.table(x)){
    data <- as.list(x[, c(term, group), with=FALSE])
  }else{
    data <- as.list(x[, c(term, group)])  
  }
  names(data) <- c("txt.original", groupby)
  result <- list()
  for(i in seq_len(ngram_max-1)){
    if(i == 1){
      data$left <- data$txt.original  
    }
    nr_of_words <- sum(!is.na(data$left))
    dt <- as.data.table(data)
    dt <- dt[, right := txt_next(txt.original, n = i), by = groupby]
    dt <- dt[, bigram := ifelse(is.na(left) | is.na(right), NA_character_, paste(left, right, sep = sep)), by = groupby]
    wordfreqs <- dt[!is.na(bigram), list(n = .N), by = list(bigram, left, right)]
    wordfreqs <- merge(wordfreqs, dt[, list(n_left = .N), by = list(left)], by = "left", all.x=TRUE, all.y=FALSE)
    wordfreqs <- merge(wordfreqs, dt[, list(n_right = .N), by = list(right)], by = "right", all.x=TRUE, all.y=FALSE)
    wordfreqs[, pmi := log2((n/nr_of_words) / ((n_left/nr_of_words) * (n_right/nr_of_words)))]
    wordfreqs[, md := log2((n/nr_of_words)^2 / ((n_left/nr_of_words) * (n_right/nr_of_words)))]
    wordfreqs[, lfmd := log2((n/nr_of_words)^2 / ((n_left/nr_of_words) * (n_right/nr_of_words))) + log2(n/nr_of_words)]
    result[[i]] <- wordfreqs[n > n_min, ]
    data[["left"]] <- dt[["bigram"]]
  }
  result <- rbindlist(result, fill = TRUE, idcol = "ngram")
  result$collocation <- paste(result$left, result$right, sep = sep)
  result$ngram <- result$ngram + 1
  result <- result[order(result$pmi, decreasing = TRUE), c("ngram", "collocation", "left", "right", "n", "n_left", "n_right", "pmi", "md", "lfmd")]
  result <- data.table::setnames(result, old = c("n", "n_left", "n_right"), new = c("freq_collocation", "freq_left", "freq_right"))
  result <- setDF(result)
  result
}

#' @export
#' @rdname keywords_collocation
collocation <- keywords_collocation
