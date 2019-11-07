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
txt_recode <- function(x, from = c(), to = c()){
  if(length(x) == 0){
    return(x)
  }
  stopifnot(length(from) == length(to))
  nongiven <- unique(x[!is.na(x)])
  nongiven <- setdiff(nongiven, from)
  if(length(nongiven) > 0) {
    from <- append(x = from, values = nongiven)
    to   <- append(x = to, values = nongiven)
  }
  to[match(x, from)]
}

recode <- function(x, from, to){
  to[match(x, from)]
}


#' @title Based on a vector with a word sequence, get n-grams (looking forward)
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
#' 
#' x <- c("A1", "A2", "A3", NA, "A4", "A5")
#' data.frame(x, 
#'            bigram = txt_nextgram(x, n = 2, sep = "_"),
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
  idx <- is.na(out[[1]])
  for(i in 1:nextel){
    out[[i+1]] <- txt_next(x, n = i)
    idx <- idx | is.na(out[[i+1]])
  }
  out$sep <- sep
  out <- do.call(paste, out)
  out[max((length(out)-(nextel-1L)), 1L):length(out)] <- NA
  out[idx] <- NA
  out
}


#' @title Based on a vector with a word sequence, get n-grams (looking backward)
#' @description If you have annotated your text using \code{\link{udpipe_annotate}},
#' your text is tokenised in a sequence of words. Based on this vector of words in sequence
#' getting n-grams comes down to looking at the previous word and the subsequent previous word andsoforth.
#' These words can be \code{pasted} together to form an n-gram containing
#' the second previous word, the previous word, the current word ...
#' @param x a character vector where each element is just 1 term or word
#' @param n an integer indicating the ngram. Values of 1 will keep the x, a value of 2 will
#' append the previous term to the current term, a value of 3 will append the second previous term
#' term and the previous term preceding the current term to the current term
#' @param sep a character element indicating how to \code{\link{paste}} the subsequent words together
#' @return a character vector of the same length of \code{x} with the n-grams
#' @seealso \code{\link{paste}}, \code{\link[data.table]{shift}}
#' @export
#' @examples 
#' x <- sprintf("%s%s", LETTERS, 1:26)
#' txt_previousgram(x, n = 2)
#' 
#' data.frame(words = x,
#'            bigram = txt_previousgram(x, n = 2),
#'            trigram = txt_previousgram(x, n = 3, sep = "-"),
#'            quatrogram = txt_previousgram(x, n = 4, sep = ""),
#'            stringsAsFactors = FALSE)
#' 
#' x <- c("A1", "A2", "A3", NA, "A4", "A5")
#' data.frame(x, 
#'            bigram = txt_previousgram(x, n = 2, sep = "_"),
#'            stringsAsFactors = FALSE)
txt_previousgram <- function(x, n = 2, sep = " "){
  n <- as.integer(n)
  stopifnot(n >= 1)
  if(n == 1){
    return(x)
  }
  nextel <- n - 1L
  out <- list()
  out[[n]] <- x
  idx <- is.na(out[[n]])
  for(i in 1:nextel){
    out[[n-i]] <- txt_previous(x, n = i)
    idx <- idx | is.na(out[[n-i]])
  }
  out$sep <- sep
  out <- do.call(paste, out)
  #out[max((length(out)-(nextel-1L)), 1L):length(out)] <- NA
  out[idx] <- NA
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
  if(inherits(x, "data.frame")){
    stop("x should be a vector")
  }
  default <- data.frame(key = character(), freq = integer(), freq_pct = numeric(), stringsAsFactors = FALSE)
  
  if(is.factor(x)){
    ## For factors, we want to keep all factor levels, even if they don't appear in the data
    tab <- table(x, exclude = exclude)
    if(length(tab) == 0){
      return(default)
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
    return(tab)
  }else{
    ## For other non-factor data like characters, just calculate how many times each key is occurring
    .N <- key <- freq <- freq_pct <- NULL
    if(length(x) == 0){
      return(default)
    }
    x <- data.table(content = x)
    x <- setnames(x, old = "content", new = "key")
    x <- x[, list(freq = .N), by = list(key)]
    x <- x[, freq_pct := 100 * freq / sum(freq)]
    x <- x[!(key %in% exclude), ]
    if(order){
      x <- setorder(x, -freq)
    }
    x <- setDF(x)
    return(x)
  }
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
    size <- length(x)
    if(ngram > 1){
      for (i in 1:(ngram - 1)) {
        loc <- idx + i
        loc <- loc[loc <= size]
        if(length(loc) > 0){
          x[loc] <- NA_character_
        }
      }
    }
  }
  x
}


#' @title Identify a contiguous sequence of tags as 1 being entity 
#' @description 
#' This function allows to identify contiguous sequences of text which have the same label or 
#' which follow the IOB scheme.\cr 
#' Named Entity Recognition or Chunking frequently follows the IOB tagging scheme 
#' where "B" means the token begins an entity, "I" means it is inside an entity,
#' "E" means it is the end of an entity and "O" means it is not part of an entity. 
#' An example of such an annotation would be 'New', 'York', 'City', 'District' which can be tagged as 
#' 'B-LOC', 'I-LOC', 'I-LOC', 'E-LOC'.\cr
#' The function looks for such sequences which start with 'B-LOC' and combines all subsequent 
#' labels of the same tagging group into 1 category. This sequence of words also gets a unique identifier such 
#' that the terms 'New', 'York', 'City', 'District' would get the same sequence identifier.
#' @param x a character vector of categories in the sequence of occurring (e.g. B-LOC, I-LOC, I-PER, B-PER, O, O, B-PER)
#' @param entities a list of groups, where each list element contains
#' \itemize{
#'  \item{start: }{A length 1 character string with the start element identifying a sequence start. E.g. 'B-LOC'}
#'  \item{labels: }{A character vector containing all the elements which are considered being part of a same labelling sequence, including the starting element. 
#'  E.g. \code{c('B-LOC', 'I-LOC', 'E-LOC')}}
#' }
#' The list name of the group defines the label that will be assigned to the entity. If \code{entities} is not provided each possible value of \code{x}
#' is considered an entity. See the examples.
#' @return a list with elements \code{entity_id} and \code{entity} where 
#' \itemize{
#'  \item{entity is a character vector of the same length as \code{x} containing entities , 
#'        constructed by recoding \code{x} to the names of \code{names(entities})}
#'  \item{entity_id is an integer vector of the same length as \code{x} containing unique identifiers identfying the compound label sequence such that 
#'        e.g. the sequence 'B-LOC', 'I-LOC', 'I-LOC', 'E-LOC' (New York City District) would get the same \code{entity_id} identifier.}
#' }
#' See the examples.
#' @export
#' @examples 
#' x <- data.frame(
#'   token = c("The", "chairman", "of", "the", "Nakitoma", "Corporation", 
#'            "Donald", "Duck", "went", "skiing", 
#'             "in", "the", "Niagara", "Falls"),
#'   upos = c("DET", "NOUN", "ADP", "DET", "PROPN", "PROPN", 
#'            "PROPN", "PROPN", "VERB", "VERB", 
#'            "ADP", "DET", "PROPN", "PROPN"),
#'   label = c("O", "O", "O", "O", "B-ORG", "I-ORG", 
#'             "B-PERSON", "I-PERSON", "O", "O", 
#'             "O", "O", "B-LOCATION", "I-LOCATION"), stringsAsFactors = FALSE)
#' x[, c("sequence_id", "group")] <- txt_tagsequence(x$upos)
#' x
#' 
#' ##
#' ## Define entity groups following the IOB scheme
#' ## and combine B-LOC I-LOC I-LOC sequences as 1 group (e.g. New York City) 
#' groups <- list(
#'  Location = list(start = "B-LOC", labels = c("B-LOC", "I-LOC", "E-LOC")),
#'  Organisation =  list(start = "B-ORG", labels = c("B-ORG", "I-ORG", "E-ORG")),
#'  Person = list(start = "B-PER", labels = c("B-PER", "I-PER", "E-PER")), 
#'  Misc = list(start = "B-MISC", labels = c("B-MISC", "I-MISC", "E-MISC")))
#' x[, c("entity_id", "entity")] <- txt_tagsequence(x$label, groups)
#' x
txt_tagsequence <- function(x, entities){
  stopifnot(is.character(x))
  if(missing(entities)){
    entities <- unique(x)
    names(entities) <- entities
    entities <- lapply(entities, FUN=function(x) list(start = x, labels = x))
    iob <- FALSE
  }else{
    iob <- TRUE
  }
  ## Data checks
  if(!all(sapply(entities, FUN=function(x) "start" %in% names(x) & "labels" %in% names(x)))){
    stop("entities should be list with elements start and labels")
  }
  
  ## Some data preparation on the entity groups
  starts_with <- sapply(entities, FUN=function(x) x$start)
  restentities <- lapply(entities, FUN=function(x) setdiff(x$labels, x$start))
  names(restentities) <- starts_with
  
  ## START
  newgroup <- rep(TRUE, length(x))
  x_prev <- txt_previous(x, n = 1)
  ## current group is same as previous group then we need to consider this together as 1 unless the previous one is part of the starting category
  if(iob){
    newgroup[which(x == x_prev & !x %in% starts_with)] <- FALSE  
  }else{
    newgroup[which(x == x_prev)] <- FALSE
  }
  ## current group is not part of start category but previous group is, look if current category part of the categories of the start
  idx <- x %in% unlist(restentities)
  if(sum(idx) > 0){
    is_same_grp <- mapply(x[idx], x_prev[idx], FUN=function(current, previous){
      current %in% restentities[[previous]]
    })
    newgroup[idx][is_same_grp] <- FALSE  
  }
  
  ## Create a new ID and a label
  out <- list()
  out$entity_id <- cumsum(newgroup)
  out$entity <- txt_recode(x = x, 
                           from = unlist(lapply(entities, FUN=function(x) x$labels)), 
                           to = rep(names(entities), sapply(entities, FUN=function(x) length(x$labels))))
  out
}


#' @title Check if text contains a certain pattern
#' @description Look up text which has a certain pattern. This pattern lookup is performed by executing a regular expression using \code{\link{grepl}}.
#' @param x a character vector with text
#' @param patterns a regular expression which might be contained in \code{x}, a vector of these
#' or a list of pattern elements where the list elements \code{include} and \code{exclude} indicate to find a pattern in \code{x}
#' while excluding elements which have another pattern
#' @param value logical, indicating to return the elements of \code{x} where the pattern was found or just a logical vector. Defaults to FALSE indicating to return a logical.
#' @param ignore.case logical, if set to \code{FALSE}, the pattern matching is case sensitive and if TRUE, case is ignored during matching. Passed on to \code{\link{grepl}}
#' @param ... other parameters which can be passed on to \code{\link{grepl}} e.g. fixed/perl/useBytes
#' @export
#' @seealso \code{\link{grepl}}
#' @return  
#' a logical vector of the same length as \code{x} indicating if one of the patterns was found in \code{x}.\cr 
#' Or the vector of elements of \code{x} where the pattern was found in case argument \code{value} is set to \code{TRUE}
#' @examples
#' x <- c("The cats are eating catfood", 
#'        "Our cat is eating the catfood", 
#'        "the dog eats catfood, he likes it")
#' txt_contains(x, patterns = c("cat", "dog")) 
#' txt_contains(x, patterns = c("cat", "dog"), value = TRUE) 
#' txt_contains(x, patterns = c("eats"), value = TRUE) 
#' txt_contains(x, patterns = c("^The"), ignore.case = FALSE, value = TRUE) 
#' txt_contains(x, patterns = list(include = c("cat"), exclude = c("dog")), 
#'              value = TRUE) 
#' txt_contains(x, "cat") & txt_contains(x, "dog")
txt_contains <- function(x, patterns, value = FALSE, ignore.case = TRUE, ...){
  if(is.list(patterns)){
    include <- rep_len(FALSE, length(x))
    exclude <- rep_len(FALSE, length(x))
    for(pattern in patterns$include){
      include <- include | grepl(pattern = pattern, x = x, ignore.case = ignore.case, ...)
    }
    for(pattern in patterns$exclude){
      exclude <- exclude | grepl(pattern = pattern, x = x, ignore.case = ignore.case, ...)
    }
    result <- include & !exclude
  }else{
    result <- rep_len(FALSE, length(x))
    for(pattern in patterns){
      result <- result | grepl(pattern = pattern, x = x, ignore.case = ignore.case, ...)
    }
  }
  if(value == TRUE){
    result <- x[result]
  }
  result
}


#' @title Count the number of times a pattern is occurring in text
#' @description Count the number of times a pattern is occurring in text. 
#' Pattern counting is performed by executing a regular expression using \code{\link{gregexpr}} and 
#' checking how many times the regular expression occurs.
#' @param x a character vector with text
#' @param pattern a text pattern which might be contained in \code{x}
#' @param ... other arguments, passed on to \code{\link{gregexpr}}
#' @return an integer vector of the same length as \code{x} indicating how many times the pattern is occurring in \code{x}
#' @export
#' @examples 
#' x <- c("abracadabra", "ababcdab")
#' txt_count(x, pattern = "ab")
#' txt_count(x, pattern = "AB", ignore.case = TRUE)
#' txt_count(x, pattern = "AB", ignore.case = FALSE)
txt_count <- function(x, pattern, ...){
  result <- gregexpr(pattern = pattern, text = x, ...)
  sapply(result, FUN = function(x){
    if(length(x) == 1 && x < 0){
      0L
    }else{
      length(x)
    }
  }, USE.NAMES = FALSE)
}

#' @title Get the overlap between 2 vectors
#' @description Get the overlap between 2 vectors
#' @param x a vector
#' @param y a vector
#' @return a vector with elements of \code{x} which are also found in \code{y}
#' @export
#' @examples 
#' x <- c("a", "b", "c")
#' y <- c("b", "c", "e", "z")
#' txt_overlap(x, y)
#' txt_overlap(y, x)
txt_overlap <- function(x, y){
  y[match(x, y, nomatch = 0L)]
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


#' @title Concatenate text of each group of data together
#' @description This function is similar to \code{\link{paste}}
#' but works on a data.frame, hence paste.data.frame. 
#' It concatenates text belonging to groups of data together in one string. 
#' The function is the inverse operation of \code{\link{strsplit.data.frame}}.
#' @param data a data.frame or data.table
#' @param term a string with a column name or a character vector of column names from \code{data} which you want to concatenate together using \code{\link{paste}}
#' @param group a string with a column name or a character vector of column names from \code{data} indicating identifiers of groups. 
#' The text in \code{term} will be concatenated by group.
#' @param collapse a character string that you want to use to collapse the text data together. 
#' Defaults to a single space.
#' @return A data.frame with 1 row per group containing the columns from \code{group} and \code{term} 
#' where all the text in \code{term} for each group will be \code{\link{paste}-d} together, separated by the \code{collapse} argument.
#' @seealso \code{\link{strsplit.data.frame}}, \code{\link{paste}}
#' @export
#' @examples 
#' data(brussels_reviews_anno, package = "udpipe")
#' head(brussels_reviews_anno)
#' x <- paste.data.frame(brussels_reviews_anno, 
#'                       term = "lemma", 
#'                       group = c("doc_id", "sentence_id"))
#' str(x)
#' x <- paste.data.frame(brussels_reviews_anno, 
#'                       term = c("lemma", "token"), 
#'                       group = c("doc_id", "sentence_id"), 
#'                       collapse = "-")
#' str(x)                       
paste.data.frame <- function(data, term, group, collapse=" "){
  .SDcols <- .SD <- NULL
  stopifnot(inherits(data, "data.frame"))
  stopifnot(inherits(term, "character"))
  stopifnot(inherits(group, "character"))
  stopifnot(all(c(term, group) %in% colnames(data)))
  if(inherits(data, "data.table")){
  }else{
    x <- data.table::as.data.table(data[, c(term, group)])  
  }
  x <- x[, lapply(.SD, FUN=function(x) paste(x, collapse = collapse)), by = group, .SDcols = term]
  x <- data.table::setDF(x)
  x
}

#' @title Obtain a tokenised data frame by splitting text alongside a regular expression 
#' @description Obtain a tokenised data frame by splitting text alongside a regular expression. 
#' This is the inverse operation of \code{\link{paste.data.frame}}.
#' @param data a data.frame or data.table
#' @param term a character with a column name from \code{data} which you want to split into tokens
#' @param group a string with a column name or a character vector of column names from \code{data} indicating identifiers of groups. 
#' The text in \code{term} will be split into tokens by group.
#' @param split a regular expression indicating how to split the \code{term} column. 
#' Defaults to splitting by spaces, punctuation symbols or digits. This will be passed on to \code{\link{strsplit}}.
#' @return A tokenised data frame containing one row per token.\cr
#' This data.frame has the columns from \code{group} and \code{term} where the text in column \code{term}
#' will be split by the provided regular expression into tokens. 
#' @seealso \code{\link{paste.data.frame}}, \code{\link{strsplit}}
#' @export
#' @examples 
#' data(brussels_reviews, package = "udpipe")
#' x <- strsplit.data.frame(brussels_reviews, term = "feedback", group = "id")
#' head(x)
#' x <- strsplit.data.frame(brussels_reviews, 
#'                          term = c("feedback"), 
#'                          group = c("listing_id", "language"))
#' head(x)                          
strsplit.data.frame <- function(data, term, group, split = "[[:space:][:punct:][:digit:]]+"){
  .SDcols <- .SD <- NULL
  stopifnot(inherits(data, "data.frame"))
  stopifnot(inherits(term, "character"))
  stopifnot(inherits(group, "character"))
  stopifnot(all(c(term, group) %in% colnames(data)))
  if(length(term) > 1){
    warning("strsplit.data.frame is not intended to be called with several columns in term")
  }
  if(inherits(data, "data.table")){
  }else{
    data <- data.table::as.data.table(data[, c(term, group)])  
  }
  x <- data[, lapply(.SD, FUN=function(txt){
    terms <- unlist(strsplit(txt, split = split))
    terms <- as.character(terms)
    terms <- terms[!is.na(terms)]
    terms <- terms[nchar(terms) > 0]
    terms
  }), by = group, .SDcols = term]
  x <- data.table::setDF(x)
  x
}