#' @title Keyword identification using Rapid Automatic Keyword Extraction (RAKE) 
#' @description RAKE is a basic algorithm which tries to identify keywords in text. Keywords are 
#' defined as a sequence of words following one another.\cr
#' The algorithm goes as follows.
#' \itemize{
#' \item candidate keywords are extracted by looking to a contiguous sequence of words which do not contain irrelevant words
#' \item a score is being calculated for each word which is part of any candidate keyword, this is done by
#'   \itemize{
#'   \item among the words of the candidate keywords, the algorithm looks how many times each word is occurring and how many times it co-occurs with other words
#'   \item each word gets a score which is the ratio of the word degree (how many times it co-occurs with other words) to the word frequency
#'   }
#' \item a RAKE score for the full candidate keyword is calculated by summing up the scores of each of the words which define the candidate keyword
#' }
#' The resulting keywords are returned as a data.frame together with their RAKE score.
#' @param x a data.frame with one row per term as returned by \code{as.data.frame(udpipe_annotate(...))}
#' @param group character string with a column in the data frame \code{x} indicating to calculate keywords within this column. This 
#' is typically a field like document id or a sentence identifier. To be used if \code{x} is a data.frame.
#' @param term character string with a column in the data frame \code{x}, containing 1 term per row. To be used if \code{x} is a data.frame.
#' @param relevant a logical vector of the same length as \code{nrow(x)}, indicating if the word in the corresponding row of \code{x} is relevant or not.
#' This can be used to exclude stopwords from the keywords calculation or for selecting only nouns and adjectives to 
#' find keywords (for example based on the Parts of Speech \code{upos} output from \code{udpipe_annotate}).\cr
#' @param ngram_max integer indicating the maximum number of words that there should be in each keyword
#' @param n_min integer indicating the frequency of how many times a keywords should
#' at least occur in the data in order to be returned. Defaults to 2.
#' @param sep character string with the separator which will be used to \code{paste} together
#' the terms which define the keywords. Defaults to a space: ' '.
#' @return a data.frame with columns keyword, ngram and rake which is ordered from low to high rake
#' \itemize{
#' \item keyword: the keyword
#' \item freq: how many times did the keyword occur
#' \item ngram: how many terms are in the keyword
#' \item rake: the ratio of the degree to the frequency as explained in the description, summed up for all words from the keyword
#' }
#' @references Rose, Stuart & Engel, Dave & Cramer, Nick & Cowley, Wendy. (2010). Automatic Keyword Extraction from Individual Documents. Text Mining: Applications and Theory. 1 - 20. 10.1002/9780470689646.ch1. 
#' @export
#' @examples 
#' data(brussels_reviews_anno)
#' x <- subset(brussels_reviews_anno, language == "nl")
#' keywords <- keywords_rake(x = x, term = "lemma", group = "doc_id", 
#'                           relevant = x$xpos %in% c("NN", "JJ"))
#' head(keywords)
#' 
#' x <- subset(brussels_reviews_anno, language == "fr")
#' x$id <- unique_identifier(x, fields = c("doc_id", "sentence_id"))
#' keywords <- keywords_rake(x = x, term = "lemma", group = "id", 
#'                           relevant = x$xpos %in% c("NN", "JJ"), 
#'                           ngram_max = 10, n_min = 2, sep = "-")
#' head(keywords)
keywords_rake <- function(x, term, group, relevant = rep(TRUE, nrow(x)), ngram_max = 2, n_min = 2, sep = " "){
  ## R CMD check happiness
  .relevant <- .N <- keyword_id <- keyword <- degree <- word <- freq <- ngram <- rake <- rake_word_score <- NULL
  stopifnot(is.data.frame(x))
  stopifnot(term %in% colnames(x))
  stopifnot(group %in% colnames(x))
  stopifnot(length(relevant) == nrow(x))
  
  ## Put the data in a data.frame
  x <- data.table(group = x[[group]], word = x[[term]], .relevant = relevant)
  
  ## Define a keyword which is a contiguous sequence of words + remove the non-candidates
  x$keyword_id <- data.table::rleid(x[["group"]], x[[".relevant"]])
  x <- subset(x, .relevant != FALSE, select = c("keyword_id", "word"))
  x <- x[, keyword := paste(word, collapse = sep), by = list(keyword_id)]
  
  ## Compute word scores degree/freq
  ##  - degree = co-occurence frequency
  ##  - frequency of each word 
  x <- x[, degree := .N-1L, by = list(keyword_id)]
  word_score <- list()
  word_score$degree <- x[, list(degree = sum(degree)), by = list(word)]
  word_score$freq <- txt_freq(x$word)
  word_score$freq <- setDT(word_score$freq)
  word_score <- merge(word_score$degree, word_score$freq, by.x = "word", by.y = "key", all.x=FALSE, all.y=TRUE)
  word_score$rake_word_score <- word_score$degree / word_score$freq
  
  ## Get keyword at the level of the word, add the word rake score and aggregate
  keywords <- x[, list(freq = length(unique(keyword_id))), by = list(keyword, word)]
  keywords <- merge(keywords, word_score[, c("word", "rake_word_score"), with = FALSE], by="word", all.x=TRUE, all.y=FALSE)
  keywords <- keywords[, list(ngram = .N,
                              rake = sum(rake_word_score)), by = list(keyword, freq)]
  keywords <- subset(keywords, ngram <= ngram_max & freq >= n_min)
  setorder(keywords, -rake)  
  keywords <- setDF(keywords)
  keywords
}


