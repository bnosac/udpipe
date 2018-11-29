#' @title Perform dictionary-based sentiment analysis on a tokenised data frame
#' @description This function identifies words which have a positive/negative meaning, with the addition of some basic logic regarding occurrences of amplifiers/deamplifiers and negators
#' in the neighbourhood of the word which has a positive/negative meaning.
#' \itemize{
#'  \item{If a negator is occurring in the neigbourhood, positive becomes negative or vice versa.}
#'  \item{If amplifiers/deamplifiers occur in the neigbourhood, these amplifier weight is added to the sentiment polarity score.}
#' }
#' This function took inspiration from qdap::polarity but was completely re-engineered to allow to calculate similar things on 
#' a udpipe-tokenised dataset. It works on a sentence level and the negator/amplification logic can not surpass a boundary defined
#' by the PUNCT upos parts of speech tag.\cr
#' 
#' Note that if you prefer to build a supervised model to perform sentiment scoring 
#' you might be interested in looking at the ruimtehol R package \url{https://github.com/bnosac/ruimtehol} instead.
#' @param x a data.frame with the columns doc_id, paragraph_id, sentence_id, upos and the column as indicated in \code{term}. This is exactly what \code{\link{udpipe}} returns.
#' @param term a character string with the name of a column of \code{x} where you want to apply to sentiment scoring upon
#' @param polarity_terms data.frame containing terms which have positive or negative meaning. This data frame should contain the columns
#' term and polarity where term is of type character and polarity can either be 1 or -1.
#' @param polarity_negators a character vector of words which will invert the meaning of the \code{polarity_terms} such that -1 becomes 1 and vice versa
#' @param polarity_amplifiers a character vector of words which amplify the \code{polarity_terms}
#' @param polarity_deamplifiers a character vector of words which deamplify the \code{polarity_terms}
#' @param amplifier_weight weight which is added to the polarity score if an amplifier occurs in the neighbourhood
#' @param n_before integer indicating how many words before the \code{polarity_terms} word one has to look to find negators/amplifiers/deamplifiers to apply its logic
#' @param n_after integer indicating how many words after the \code{polarity_terms} word one has to look to find negators/amplifiers/deamplifiers to apply its logic
#' @param constrain logical indicating to make sure the aggregated sentiment scores is between -1 and 1
#' @export
#' @return a list containing 
#' \itemize{
#'  \item{data: the \code{x} data.frame with 2 columns added: polarity and sentiment_polarity. 
#'  \itemize{
#'  \item{The column polarity being just the polarity column of the \code{polarity_terms} dataset corresponding to the polarity of the \code{term} you apply the sentiment scoring} 
#'  \item{The colummn sentiment_polarity is the value where the amplifier/de-amplifier/negator logic is applied on. }
#'  }
#'  }
#'  \item{overall: a data.frame with one row per doc_id containing the columns doc_id, sentences,
#'  terms, sentiment_polarity, terms_positive, terms_negative, terms_negation and terms_amplification 
#'  providing the aggregate sentiment_polarity score of the dataset \code{x} by doc_id as well as 
#'  the terminology causing the sentiment, the number of sentences and the number of non punctuation terms in the document.}
#' }
#' @examples 
#' x <- c("I do not like whatsoever when an R package has soo many dependencies.",
#'        "Making other people install java is somewhat annoying me, 
#'         as it is really painful.")
#' x <- udpipe(x, "english-gum")
#' txt_sentiment(x = x, 
#'               term = "lemma",
#'               polarity_terms = data.frame(term = c("annoy", "like", "painful"), 
#'                                           polarity = c(-1, 1, -1)), 
#'               polarity_negators = c("not", "neither"),
#'               polarity_amplifiers = c("pretty", "many", "really", "whatsoever"), 
#'               polarity_deamplifiers = c("slightly", "somewhat"),
#'               constrain = TRUE, n_before = 4,
#'               n_after = 2, amplifier_weight = .8)
txt_sentiment <- function(x,
                      term = "lemma",
                      polarity_terms, 
                      polarity_negators = character(), 
                      polarity_amplifiers = character(), 
                      polarity_deamplifiers = character(), 
                      amplifier_weight = .8, n_before = 4, n_after = 2,
                      constrain = FALSE) {
  
  ## R cmd check happiness
  .N <- .SD <- NULL
  .look_end <- .look_start <- doc_id <- paragraph_id <- sentence_id <- upos <- polarity <- sentiment_polarity <- .subsentence <-  .word_id <-  .word_id_max <- NULL
  
  stopifnot(inherits(x, "data.frame"))
  stopifnot(inherits(polarity_terms, "data.frame"))
  stopifnot(all(c("doc_id", "paragraph_id", "sentence_id", "upos", term) %in% colnames(x)))
  stopifnot(all(c("term", "polarity") %in% colnames(polarity_terms)))
  if(any(c("polarity", "sentiment_polarity") %in% colnames(x))){
    stop("x should not contain the columns polarity nor sentiment_polarity")
  }

  # Avoid duplicate information
  amplifiers   <- setdiff(polarity_amplifiers, polarity_terms$term)
  deamplifiers <- setdiff(polarity_deamplifiers, polarity_terms$term)
  negators     <- setdiff(polarity_negators, polarity_terms$term)
  deamplifier_weight <- amplifier_weight * -1
  
  within_sentence_separators <- c(",", ";")
  within_sentence_separators <- "PUNCT"
  x$polarity <- rep(NA_real_, nrow(x))
  x$sentiment_polarity <- rep(NA_real_, nrow(x))
  polarity_terms$term <- as.character(polarity_terms$term)
  idx <- which(x[[term]] %in% polarity_terms$term)
  if(length(idx) > 0){
    x$polarity[idx] <- polarity_terms$polarity[match(x[[term]][idx], polarity_terms$term)]
  }
  sentence_sentiment_scorer <- function(data){
    ## By default result of polarity is NA unless we have a polarised word
    ## Change the value of the polarised word if in the neighbourhood negators, amplifiers, deamplifiers occur
    result <- rep(NA_real_, nrow(data))
    idx <- which(!is.na(data$polarity))
    result[idx] <- sapply(idx, FUN = function(i){
      ## Get all the words in the neighbourhood of the word from the polarity frame
      word_locations     <- data$.look_start[i]:data$.look_end[i]
      word_locations     <- setdiff(word_locations, data$.word_id[i])
      words <- data[[term]][word_locations]
      ## Polarity of the word itself
      polarity_value     <- data$polarity[data$.word_id[i]]
      ## How many words are negators/deamplifiers/amplifiers
      nr_of_negators     <- sum(words %in% negators)
      nr_of_deamplifiers <- sum(words %in% deamplifiers)
      nr_of_amplifiers   <- sum(words %in% amplifiers)
      ## Calculate d which is the number of deamplifiers x their weight
      ##   - by counting the deamplifiers
      ##   - by counting the amplifiers if an uneven number of negator is also in the neigbourhood (e.g. 1, 3, 5, 7, ... negators around it)
      d <- (nr_of_deamplifiers * deamplifier_weight) + (nr_of_amplifiers * deamplifier_weight * ifelse(nr_of_negators %% 2 != 0, 1L, 0L))
      d <- ifelse(d < -1, -1, d)   
      ## Calculate a which is the number of amplifiers x their weight
      ##   - by counting the amplifiers 
      ##   - but only if an even number of negators are in the neighbourhood otherwise it was already counted in d
      a <- nr_of_amplifiers * amplifier_weight * ifelse(nr_of_negators %% 2 == 0, 1, 0)
      ## Final score for the word is either
      ##   - the polarity_value x 1
      ##   - the polarity value x (1 + the amplification caused by amplifiers/deamplifiers)
      ##   - the polarity value x -1 if there is an uneven number of negators
      ##   - the polarity value x 1  if there is an   even number of negators
      ##   - a combination of amplification and negation
      (1 + (d + a)) * (polarity_value * (-1)^(2 + nr_of_negators))
    })
    result
  }
  x <- data.table::setDT(x)
  if(sum(!is.na(x$polarity)) > 0){
    ## define a subsentence
    ## subsentence is a subpart within a sentence governed by a PUNCTUATION symbol, e.g. , or ;
    ## assumption is that a sentiment amplifiers/de-amplifiers/negators can not surpass such a boundary
    x[, .subsentence := cumsum(ifelse(upos %in% within_sentence_separators, 1, 0)), by = list(doc_id, paragraph_id, sentence_id)]
    ## Define some helper variables, namely what is the start/end position within the .subsentence of words in the neighbourhood
    ## of the word from the polarity frame
    x[, .word_id     := seq_len(.N),       by = list(doc_id, paragraph_id, sentence_id, .subsentence)]
    x[, .word_id_max := max(.word_id),      by = list(doc_id, paragraph_id, sentence_id, .subsentence)]
    x[, .look_start  := ifelse(.word_id - n_before < 1,                       1, .word_id - n_before)]
    x[, .look_end    := ifelse(.word_id + n_after  > .word_id_max, .word_id_max, .word_id + n_after)]
    x[, sentiment_polarity := sentence_sentiment_scorer(.SD), by = list(doc_id, paragraph_id, sentence_id, .subsentence), 
      .SDcols = c("polarity", term, ".word_id", ".look_start", ".look_end")]
    ## remove columns which were created for this and are no longer needed
    x[, .subsentence := NULL]
    x[, .word_id := NULL]
    x[, .word_id_max := NULL]
    x[, .look_start := NULL]
    x[, .look_end := NULL]
  }
  overall <- x[, list(sentiment_polarity = sum(sentiment_polarity, na.rm = TRUE), 
                      sentences = length(unique(sentence_id)),
                      terms = sum(!upos %in% within_sentence_separators),
                      terms_positive = paste(sort(unique(.SD[[term]][!is.na(polarity) & polarity > 0])), collapse = ", "), 
                      terms_negative = paste(sort(unique(.SD[[term]][!is.na(polarity) & polarity < 0])), collapse = ", "),
                      terms_negation = paste(sort(unique(intersect(.SD[[term]], negators))), collapse = ", "),
                      terms_amplification = paste(sort(unique(intersect(.SD[[term]], c(amplifiers, deamplifiers)))), collapse = ", ")),
               by = list(doc_id), .SDcols = c(term, "polarity")]
  if(constrain){
    constrain_01 <- function(x) ((1 - (1/(1 + exp(x)))) * 2) - 1
    overall$sentiment_polarity <- constrain_01(overall$sentiment_polarity)
  }
  x <- data.table::setDF(x)
  list(data = x, overall = overall)  
}
