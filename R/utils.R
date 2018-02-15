#' @title Collapse a character vector while removing missing data.
#' @description Collapse a character vector while removing missing data.
#' @param x a character vector
#' @param collapse a character string to be used to collapse the vector. Defaults to a space: ' '.
#' @return a character vector of length 1 with the content of x collapsed using paste
#' @export
#' @seealso \code{\link{paste}}
#' @examples 
#' txt_collapse(c(NA, "hello", "world", NA))
txt_collapse <- function(x, collapse=" "){
  x <- as.character(x)
  x <- x[!is.na(x)]
  if(length(x) == 0){
    return(NA_character_)
  }else if(length(x) > 1){
    x <- paste(x, collapse = collapse)
  }
  x
}




#' @title Boilerplate function to sample one element from a vector.
#' @description Boilerplate function to sample one element from a vector.
#' @param x a vector
#' @param na.exclude logical indicating to remove NA values before taking a sample
#' @param n integer indicating the number of items to sample from \code{x}
#' @return one element sampled from the vector x
#' @export
#' @seealso \code{\link{sample.int}}
#' @examples 
#' txt_sample(c(NA, "hello", "world", NA))
txt_sample <- function(x, na.exclude = TRUE, n = 1){
  if(na.exclude){
    x <- x[!is.na(x)]
  }
  x[sample.int(length(x), size = n)]
}

#' @title Boilerplate function to cat only 1 element of a character vector.
#' @description Boilerplate function to cat only 1 element of a character vector.
#' @param x a character vector
#' @return invisible
#' @export
#' @seealso \code{\link{txt_sample}}
#' @examples 
#' txt_show(c("hello \n\n\n world", "world \n\n\n hello"))
txt_show <- function(x){
  x <- txt_sample(x)
  if(length(x) == 1){
    cat(x, sep = "\n")  
  }else{
    cat(x, sep = "\n")
  }
  invisible()
}


#' @title Recode text to other categories
#' @description Recode text to other categories. 
#' Values of \code{x} which correspond to \code{from[i]} will be recoded to \code{to[i]}
#' @param x a character vector
#' @param from a character vector with values of \code{x} which you want to recode
#' @param to a character vector with values of you want to use to recode to where you
#' want to replace values of \code{x} which correspond to \code{from[i]} to \code{to[i]}
#' @return a character vector of the same length of \code{x} where values of \code{x}
#' which are given in \code{from} will be replaced by the corresponding element in \code{to}
#' @seealso \code{\link{match}}
#' @export
#' @examples 
#' x <- c("NOUN", "VERB", "NOUN", "ADV")
#' txt_recode(x = x,
#'            from = c("VERB", "ADV"),
#'            to = c("conjugated verb", "adverb"))
#'    
txt_recode <- function(x, from = c(), to = c()){
  stopifnot(length(from) == length(to))
  nongiven <- unique(x[!is.na(x)])
  nongiven <- setdiff(nongiven, from)
  if(length(nongiven) > 0) {
    from <- append(x = from, values = nongiven)
    to <- append(x = to, values = nongiven)
  }
  to[match(x, from)]
}


#' @title Based on a vector with a word sequence, get n-grams
#' @description If you have annotated your text using \code{\link{udpipe_annotate}},
#' your text is tokenised in a sequence of words. Based on this vector of words in sequence
#' getting n-grams comes down to looking at the next word and the subsequent word andsoforth.
#' These words can be \code{pasted} together to form an n-gram containing
#' the current word, the next word up, the subsequent word, ...
#' @param x a character vector where each element is just 1 term or word
#' @param n an integer indicating the ngram. Values of 1 will keep the x, a value of 2 will
#' append the next term to the current term, a value of 3 will append the subsequent
#' term and the term following that term to the current term
#' @param sep a character element indicating how to \code{\link{paste}} the subsequent words together
#' @return a character vector of the same length of \code{x} with the n-grams
#' @seealso \code{\link{paste}}, \code{\link[data.table]{shift}}
#' @export
#' @examples 
#' x <- sprintf("%s%s", LETTERS, 1:26)
#' txt_nextgram(x, n = 2)
#' 
#' data.frame(words = x,
#'            bigram = txt_nextgram(x, n = 2),
#'            trigram = txt_nextgram(x, n = 3, sep = "-"),
#'            quatrogram = txt_nextgram(x, n = 4, sep = ""),
#'            stringsAsFactors = FALSE)
txt_nextgram <- function(x, n = 2, sep = " "){
  n <- as.integer(n)
  stopifnot(n >= 1)
  if(n == 1){
    return(x)
  }
  nextel <- n - 1L
  out <- list()
  out[[1]] <- x
  for(i in 1:nextel){
    out[[i+1]] <- txt_next(x, n = i)
  }
  out$sep <- sep
  out <- do.call(paste, out)
  out[max((length(out)-(nextel-1L)), 1L):length(out)] <- NA
  out
}

#' @title Get the n-th previous element of a vector
#' @description Get the n-th previous element of a vector
#' @param x a character vector where each element is just 1 term or word
#' @param n an integer indicating how far to look back. Defaults to 1.
#' @return a character vector of the same length of \code{x} with the previous element
#' @seealso \code{\link[data.table]{shift}}
#' @export
#' @examples 
#' x <- sprintf("%s%s", LETTERS, 1:26)
#' txt_previous(x, n = 1)
#' 
#' data.frame(word = x,
#'            word_previous1 = txt_previous(x, n = 1),
#'            word_previous2 = txt_previous(x, n = 2),
#'            stringsAsFactors = FALSE)
txt_previous <- function(x, n = 1){
  n <- as.integer(n)
  stopifnot(n >= 0)
  data.table::shift(x, n = n, type = "lag")
}

#' @title Get the n-th next element of a vector
#' @description Get the n-th next element of a vector
#' @param x a character vector where each element is just 1 term or word
#' @param n an integer indicating how far to look next. Defaults to 1.
#' @return a character vector of the same length of \code{x} with the next element
#' @seealso \code{\link[data.table]{shift}}
#' @export
#' @examples 
#' x <- sprintf("%s%s", LETTERS, 1:26)
#' txt_next(x, n = 1)
#' 
#' data.frame(word = x,
#'            word_next1 = txt_next(x, n = 1),
#'            word_next2 = txt_next(x, n = 2),
#'            stringsAsFactors = FALSE)
txt_next <- function(x, n = 1){
  n <- as.integer(n)
  stopifnot(n >= 0)
  data.table::shift(x, n = n, type = "lead")
}



#' @title Frequency statistics of elements in a vector
#' @description Frequency statistics of elements in a vector
#' @param x a vector
#' @param exclude logical indicating to exclude values from the table. Defaults to NA and NaN.
#' @param order logical indicating to order the resulting dataset in order of frequency. Defaults to TRUE.
#' @return a data.frame with columns key, freq and freq_pct indicating the how 
#' many times each value in the vector \code{x} is occurring
#' @export
#' @examples 
#' x <- sample(LETTERS, 1000, replace = TRUE)
#' txt_freq(x)
#' x <- factor(x, levels = LETTERS)
#' txt_freq(x, order = FALSE)
txt_freq <- function(x, exclude = c(NA, NaN), order=TRUE){
  tab <- table(x, exclude = exclude)
  if(length(tab) == 0){
    return(data.frame(key = character(), freq = integer(), freq_pct = numeric(), stringsAsFactors = FALSE))
  }
  tab <- as.data.frame.table(tab, responseName = "freq")
  setnames(tab, old = colnames(tab)[1], new = "key")
  if(is.factor(tab$key)){
    tab$key <- as.character(tab$key)
  }
  tab$freq_pct <- 100 * tab$freq / sum(tab$freq)
  if(order){
    tab <- tab[order(tab$freq, decreasing=TRUE), ]  
  }
  rownames(tab) <- NULL
  tab
}


#' @title Highlight words in a character vector
#' @description Highlight words in a character vector. The words provided in \code{terms} are 
#' highlighted in the text by wrapping it around the following charater: |.
#' So 'I like milk and sugar in my coffee' would give 'I like |milk| and sugar in my coffee' if you 
#' want to highlight the word milk
#' @param x a character vector with text
#' @param terms a vector of words to highlight which appear in \code{x}
#' @return A character vector with the same length of \code{x} where the terms provided in \code{terms}
#' are put in between || to highlight them
#' @export
#' @examples 
#' x <- "I like milk and sugar in my coffee."
#' txt_highlight(x, terms = "sugar")
#' txt_highlight(x, terms = c("milk", "my"))
txt_highlight <- function(x, terms){
  terms <- paste(terms, collapse = "|")
  gsub(pattern = sprintf("(%s)", terms), replacement = "\\|\\1\\|", x = x, fixed = FALSE)
}



#' @title Recode words with compound multi-word expressions
#' @description Replace in a character vector of tokens, tokens with compound multi-word expressions.
#' So that \code{c("New", "York")} will be \code{c("New York", NA)}. 
#' @param x a character vector of words where you want to replace tokens with compound multi-word expressions.
#' This is generally a character vector as returned by the token column of \code{as.data.frame(udpipe_annotate(txt))}
#' @param compound a character vector of compound words multi-word expressions indicating terms which can be considered as one word. 
#' For example \code{c('New York', 'Brussels Hoofdstedelijk Gewest')}.
#' @param ngram a integer vector of the same length as \code{compound} indicating how many terms there are in the specific compound multi-word expressions
#' given by \code{compound}, where \code{compound[i]} contains \code{ngram[i]} words. 
#' So if \code{x} is \code{c('New York', 'Brussels Hoofdstedelijk Gewest')}, the ngram would be \code{c(2, 3)}
#' @param sep separator used when the compounds were constructed by combining the words together into a compound multi-word expression. Defaults to a space: ' '.
#' @return the same character vector \code{x} where elements in \code{x} will be replaced by compound multi-word expression. 
#' If will give preference to replacing with compounds with higher ngrams if these occur. See the examples.
#' @export
#' @seealso \code{\link{txt_nextgram}}
#' @examples 
#' x <- c("I", "went", "to", "New", "York", "City", "on", "holiday", ".")
#' y <- txt_recode_ngram(x, compound = "New York", ngram = 2, sep = " ")
#' data.frame(x, y)
#' 
#' keyw <- data.frame(keyword = c("New-York", "New-York-City"), ngram = c(2, 3))
#' y <- txt_recode_ngram(x, compound = keyw$keyword, ngram = keyw$ngram, sep = "-")
#' data.frame(x, y)
#' 
#' ## Example replacing adjectives followed by a noun with the full compound word
#' data(brussels_reviews_anno)
#' x <- subset(brussels_reviews_anno, language == "nl")
#' keyw <- keywords_phrases(x$xpos, term = x$token, pattern = "JJNN", 
#'                          is_regex = TRUE, detailed = FALSE)
#' head(keyw)
#' x$term <- txt_recode_ngram(x$token, compound = keyw$keyword, ngram = keyw$ngram)
#' head(x[, c("token", "term", "xpos")], 12)
txt_recode_ngram <- function(x, compound, ngram, sep = " "){
  ngram <- as.integer(ngram)
  
  if(length(ngram) != 1){
    stopifnot(length(ngram) == length(compound))
    keywords <- data.frame(keyword = compound, ngram = ngram, stringsAsFactors = FALSE)
    
    ## Loop over all ngrams, first replace the keywords with the most number of words in there
    ngrams <- unique(keywords$ngram)
    ngrams <- sort(ngrams, decreasing = TRUE)
    for(i in ngrams){
      x <- txt_recode_ngram(x, compound = keywords$keyword[keywords$ngram == i], ngram = i, sep = sep)
    }
  }else{
    keywords <- as.character(compound)
    if(length(keywords) == 0){
      return(x)
    }
    y <- txt_nextgram(x, n = ngram, sep = sep)
    idx <- which(y %in% keywords)
    ## Overwrite word with bigram/trigram/n-gram
    x[idx] <- y[idx]
    ## Set the next values to NA
    for(i in 1:(ngram - 1)){
      x[idx + i] <- NA_character_
    }  
  }
  x
}

#' @title Create a unique identifier for each combination of fields in a data frame
#' @description Create a unique identifier for each combination of fields in a data frame. 
#' This unique identifier is unique for each combination of the elements of the fields. 
#' The generated identifier is like a primary key or a secondary key on a table.
#' This is just a small wrapper around \code{\link[data.table]{frank}}
#' @param x a data.frame
#' @param fields a character vector of columns from \code{x}
#' @param start_from integer number indicating to start from that number onwards
#' @return an integer vector of the same length as the number of rows in \code{x} 
#' containing the unique identifier
#' @export
#' @examples 
#' data(brussels_reviews_anno)
#' x <- brussels_reviews_anno
#' x$doc_sent_id <- unique_identifier(x, fields = c("doc_id", "sentence_id"))
#' head(x, 15)
#' range(x$doc_sent_id)
#' x$doc_sent_id <- unique_identifier(x, fields = c("doc_id", "sentence_id"), start_from = 10)
#' head(x, 15)
#' range(x$doc_sent_id)
unique_identifier <- function(x, fields, start_from = 1L){
  id <- data.table::frankv(x, cols = fields, ties.method = "dense")
  if(!missing(start_from)){
    id <- id - 1L + as.integer(start_from)
  }
  id
}