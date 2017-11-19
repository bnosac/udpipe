#' @title Extract phrases - a sequence of terms which follow each other based on a sequence of Parts of Speech tags
#' @description This function allows to extract phrases, like simple noun phrases, complex noun phrases
#' or any exact sequence of parts of speech tag patterns.\cr
#' An example use case of this is to get all text where an adjective is followed by a noun or
#' for example to get all phrases consisting of a preposition which is followed by a noun which is next followed by a verb.
#' More complex patterns are shown in the details below.
#' @param x a character vector of Parts of Speech tags where we want to locate a relevant sequence of POS tags as defined in \code{pattern}
#' @param term a character vector of the same length as \code{x} with the words or terms corresponding to the tags in \code{x}
#' @param pattern In case \code{is_regex} is set to FALSE, \code{pattern} should be a character vector with a sequence of POS tags 
#' to identify in \code{x}. The length of the character vector should be bigger than 1.\cr
#' In case \code{is_regex} is set to TRUE, this should be a regular expressions which will be used on a concatenated version 
#' of \code{x} to identify the locations where these regular expression occur. See the examples below.
#' @param is_regex logical indicating if \code{pattern} can be considered as a regular expression or if it is just
#' a character vector of POS tags. Defaults to FALSE, indicating \code{pattern} is not a regular expression.
#' @param sep character indicating how to collapse the phrase of terms which are found. Defaults to using a space.
#' @param ngram_max an integer indicating to allow phrases to be found up to \code{ngram} maximum number of terms following each other. Only 
#' used if is_regex is set to TRUE. Defaults to 8.
#' @return a data.frame with columns 
#' \itemize{
#' \item start: the starting index of \code{x} where the pattern was found
#' \item end: the ending index of \code{x} where the pattern was found
#' \item pattern: the pattern which was found
#' \item phrase: the phrase which corresponds to the collapsed terms of where the pattern was found
#' }
#' @export
#' @seealso \code{\link{as_phrasemachine}}
#' @details Common phrases which you might be interested in and which can be supplied to \code{pattern} are
#' \itemize{
#' \item Simple noun phrase: "(A|N)*N(P+D*(A|N)*N)*"
#' \item Simple verb Phrase: "((A|N)*N(P+D*(A|N)*N)*P*(M|V)*V(M|V)*|(M|V)*V(M|V)*D*(A|N)*N(P+D*(A|N)*N)*|(M|V)*V(M|V)*(P+D*(A|N)*N)+|(A|N)*N(P+D*(A|N)*N)*P*((M|V)*V(M|V)*D*(A|N)*N(P+D*(A|N)*N)*|(M|V)*V(M|V)*(P+D*(A|N)*N)+))"
#' \item Noun hrase with coordination conjuction: "((A(CA)*|N)*N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*(C(D(CD)*)*(A(CA)*|N)*N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*)*)"
#' \item Verb phrase with coordination conjuction: "(((A(CA)*|N)*N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*(C(D(CD)*)*(A(CA)*|N)*N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*)*)(P(CP)*)*(M(CM)*|V)*V(M(CM)*|V)*(C(M(CM)*|V)*V(M(CM)*|V)*)*|(M(CM)*|V)*V(M(CM)*|V)*(C(M(CM)*|V)*V(M(CM)*|V)*)*(D(CD)*)*((A(CA)*|N)*N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*(C(D(CD)*)*(A(CA)*|N)*N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*)*)|(M(CM)*|V)*V(M(CM)*|V)*(C(M(CM)*|V)*V(M(CM)*|V)*)*((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)+|((A(CA)*|N)*N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*(C(D(CD)*)*(A(CA)*|N)*N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*)*)(P(CP)*)*((M(CM)*|V)*V(M(CM)*|V)*(C(M(CM)*|V)*V(M(CM)*|V)*)*(D(CD)*)*((A(CA)*|N)*N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*(C(D(CD)*)*(A(CA)*|N)*N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*)*)|(M(CM)*|V)*V(M(CM)*|V)*(C(M(CM)*|V)*V(M(CM)*|V)*)*((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)+))"
#' }
#' See the examples.\cr
#' Mark that this functionality is also implemented in the phrasemachine package where it is implemented using plain R code, 
#' while the implementation in this package uses a more quick Rcpp implementation for 
#' extracting these kind of regular expression like phrases.
#' @examples 
#' data(brussels_reviews_anno, package = "udpipe")
#' x <- subset(brussels_reviews_anno, language %in% "fr")
#' 
#' ## Find exactly this sequence of POS tags
#' np <- phrases(x$xpos, pattern = c("DT", "NN", "VB", "RB", "JJ"), sep = "-")
#' head(np)
#' np <- phrases(x$xpos, pattern = c("DT", "NN", "VB", "RB", "JJ"), term = x$token)
#' head(np)
#' 
#' ## Find noun phrases with the following regular expression: (A|N)+N(P+D*(A|N)*N)*
#' x$phrase_tag <- as_phrasemachine(x$xpos, type = "penn-treebank")
#' nounphrases <- phrases(x$phrase_tag, term = x$token, 
#'                        pattern = "(A|N)+N(P+D*(A|N)*N)*", is_regex = TRUE, ngram_max = 4)
#' head(nounphrases, 10)
#' head(sort(table(nounphrases$phrase), decreasing=TRUE), 20)
#' 
#' ## Find frequent sequences of POS tags
#' library(data.table)
#' x <- as.data.table(x)
#' x <- x[, pos_sequence := txt_nextgram(x = xpos, n = 3), by = list(doc_id, sentence_id)]
#' tail(sort(table(x$pos_sequence)))
#' np <- phrases(x$xpos, term = x$token, pattern = c("IN", "DT", "NN"))
#' head(np)
phrases <- function(x, term = x, pattern, is_regex = FALSE, sep = " ", ngram_max = 8){
  stopifnot(length(x) == length(term))
  ngram_max <- as.integer(ngram_max)
  if(is_regex){
    stopifnot(length(pattern) == 1)
    stopifnot(ngram_max > 1L)
    i <- phrases_regex_locate(x, pattern = sprintf("^%s$", pattern), ngram_max = ngram_max)
    i <- data.table::setDF(i)
    if(nrow(i) == 0){
      out <- data.frame(start = integer(), end = integer(), pattern = character(), phrase = character(), stringsAsFactors = FALSE)
    }else{
      out <- data.frame(start = i$from, 
                        end = i$to, 
                        pattern = i$pattern, 
                        phrase = mapply(i$from, i$to, FUN=function(from, to) paste(term[from:to], collapse=sep)), stringsAsFactors = FALSE)  
    }
  }else{
    if(length(pattern) <= 1){
      warning("pattern is length <= 1. If you do not set is_regular_expression=TRUE, you should give a character vector of length > 1 in pattern. There are probably better ways than using this function to just extract the location where the pattern occurs. E.g. which(x %in% pattern)")
    }
    pattern_sequence <- txt_nextgram(x = x, n = length(pattern), sep = sep)
    p <- paste(pattern, collapse = sep)
    i <- which(pattern_sequence %in% p)
    if(length(i) == 0){
      out <- data.frame(start = integer(), end = integer(), pattern = character(), phrase = character(), stringsAsFactors = FALSE)
    }else{
      txt <- txt_nextgram(x = term, n = length(pattern), sep = sep)
      out <- data.frame(start = i, 
                        end = i + length(pattern) - 1L, 
                        pattern = p, 
                        phrase = txt[i], stringsAsFactors = FALSE)  
    }
  }
  out
}


#' @title Convert Parts of Speech tags to one-letter tags which can be used to identify phrases based on regular expressions
#' @description Noun phrases are of common interest when doing natural language processing. Extracting noun phrases
#' from text can be done easily by defining a sequence of Parts of Speech tags. For example this sequence of POS tags
#' can be seen as a noun phrase: Adjective, Noun, Preposition, Noun.\cr
#' This function recodes Universal POS tags to one of the following 1-letter tags, in order to simplify writing regular expressions
#' to find Parts of Speech sequences:
#' \itemize{
#' \item A: adjective
#' \item C: coordinating conjuction
#' \item D: determiner
#' \item M: modifier of verb
#' \item N: noun or proper noun
#' \item P: preposition
#' \item O: other elements
#' }
#' After which identifying a simple noun phrase can be just expressed by using the following 
#' regular expression (A|N)*N(P+D*(A|N)*N)* which basically says
#' start with adjective or noun, another noun, a preposition, determiner adjective or noun and next a noun again.
#' @param x a character vector of POS tags for example by using \code{\link{udpipe_annotate}}
#' @param type either 'upos' or 'penn-treebank' indicating to recode Universal Parts of Speech tags to the counterparts
#' as described in the description, or to recode Parts of Speech tags as known in the Penn Treebank to the counterparts
#' as described in the description
#' @return the character vector \code{x} where the respective POS tags are replaced with one-letter tags
#' @seealso \code{\link{phrases}}
#' @details For more information on extracting phrases see \url{http://brenocon.com/handler2016phrases.pdf}
#' @export
#' @examples 
#' x <- c("PROPN", "SCONJ", "ADJ", "NOUN", "VERB", "INTJ", "DET", "VERB", 
#'        "PROPN", "AUX", "NUM", "NUM", "X", "SCONJ", "PRON", "PUNCT", "ADP", 
#'        "X", "PUNCT", "AUX", "PROPN", "ADP", "X", "PROPN", "ADP", "DET", 
#'        "CCONJ", "INTJ", "NOUN", "PROPN")
#' as_phrasemachine(x)
as_phrasemachine <- function(x, type = c("upos", "penn-treebank")){
  stopifnot(is.character(x))
  type <- match.arg(type)
  if(type == "upos"){
    fromto <- list()
    fromto$original <- c("ADJ", "ADP", "ADV", "AUX", "CCONJ", "DET", "INTJ", "NOUN", "NUM", "PART", "PRON", "PROPN", "PUNCT", "SCONJ", "SYM", "VERB")
    fromto$tagset <- c("A", "P", "M", "V", "C", "D", "O", "N", "A", "M", "N", "N", "O", "C", "O", "V")
    fromto$other <- "X"
  }else if(type == "penn-treebank"){
    fromto <- list()
    fromto$original <- c("JJ", "JJR", "JJS", "CD", "PDT", "DT", "IN", "TO", "NN", "NNS", "NNP", "NNPS", "FW", "RB", "RBR", "RBS", "RP", "MD", "VB", "VBD", "VBG", "VBN", "VBP", "VBZ", "CC")
    fromto$tagset <- c("A", "A", "A", "A", "D", "D", "P", "P", "N", "N", "N", "N", "N", "M", "M", "M", "M", "M", "V", "V", "V", "V", "V", "V", "C")
    fromto$other <- c("EX", "LS", "POS", "PRP", "PRP$", "SYM", "UH", "WDT", "WP", "WP$", "WRB")
  }
  levs <- unique(x)
  levs <- setdiff(levs, NA)
  knowntagset <- c(fromto$original, fromto$other)
  if(!all(levs %in% knowntagset)){
    warning(sprintf("x should contain only these tags: %s, the following are not recognised tags: %s", 
                    paste(sort(knowntagset), collapse = ", "), paste(head(setdiff(levs, knowntagset)), collapse=" ")))
  }
  x <- ifelse(x %in% fromto$other, "O", x)
  txt_recode(x, 
             from = fromto$original,
             to = fromto$tagset)
}

