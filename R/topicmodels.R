#' @title Predict method for an object of class LDA_VEM or class LDA_Gibbs
#' @description Gives either the predictions to which topic a document belongs or 
#' the term posteriors by topic indicating which terms are emitted by each topic.\cr
#' If you provide in \code{newdata} a document term matrix 
#' for which a document does not contain any text and hence does not have any terms with nonzero entries,
#' the prediction will give as topic prediction NA values (see the examples).
#' @param object an object of class LDA_VEM or LDA_Gibbs as returned by \code{LDA} from the topicmodels package
#' @param newdata a document/term matrix containing data for which to make a prediction
#' @param type either 'topic' or 'terms' for the topic predictions or the term posteriors
#' @param labels a character vector of the same length as the number of topics in the topic model. 
#' Indicating how to label the topics. Only valid for type = 'topic'. 
#' Defaults to topic_prob_001 up to topic_prob_999.
#' @param min_posterior numeric in 0-1 range to output only terms emitted by each 
#' topic which have a posterior probability equal or higher than \code{min_posterior}. 
#' Only used if \code{type} is 'terms'. Provide -1 if you want to keep all values.
#' @param min_terms integer indicating the minimum number of terms to keep in the output when \code{type} is 'terms'. Defaults to 0.
#' @param ... further arguments passed on to topicmodels::posterior
#' @return 
#' \itemize{
#'  \item{in case of type = 'topic': }{a data.table with columns 
#'  doc_id, 
#'  topic (the topic number to which the document is assigned to), 
#'  topic_label (the topic label)
#'  topic_prob (the posterior probability score for that topic), 
#'  topic_probdiff_2nd (the probability score for that topic - the probability score for the 2nd highest topic) 
#'  and the probability scores for each topic as indicated by topic_labelyourownlabel}
#'  \item{n case of type = 'terms': }{a list of data.frames with columns term and prob, 
#'  giving the posterior probability that each term is emitted by the topic}
#' }
#' @export
#' @S3method predict LDA_VEM
#' @rdname predict.LDA
#' @aliases predict.LDA
#' @seealso \code{\link[topicmodels]{posterior-methods}}
#' @examples 
#' ## Build document/term matrix on dutch nouns
#' data(brussels_reviews_anno)
#' data(brussels_reviews)
#' x <- subset(brussels_reviews_anno, language == "nl")
#' x <- subset(x, xpos %in% c("JJ"))
#' x <- x[, c("doc_id", "lemma")]
#' x <- document_term_frequencies(x)
#' dtm <- document_term_matrix(x)
#' dtm <- dtm_remove_lowfreq(dtm, minfreq = 10)
#' dtm <- dtm_remove_tfidf(dtm, top = 100)
#' 
#' ## Fit a topicmodel using VEM
#' library(topicmodels)
#' mymodel <- LDA(x = dtm, k = 4, method = "VEM")
#' 
#' ## Get topic terminology
#' terminology <- predict(mymodel, type = "terms", min_posterior = 0.05, min_terms = 3)
#' terminology
#' 
#' ## Get scores alongside the topic model
#' dtm <- document_term_matrix(x, vocabulary = mymodel@terms)
#' scores <- predict(mymodel, newdata = dtm, type = "topics")
#' scores <- predict(mymodel, newdata = dtm, type = "topics", 
#'                   labels = c("mylabel1", "xyz", "app-location", "newlabel"))
#' head(scores)
#' table(scores$topic)
#' table(scores$topic_label)
#' table(scores$topic, exclude = c())
#' table(scores$topic_label, exclude = c())
#' 
#' ## Fit a topicmodel using Gibbs
#' library(topicmodels)
#' mymodel <- LDA(x = dtm, k = 4, method = "Gibbs")
#' terminology <- predict(mymodel, type = "terms", min_posterior = 0.05, min_terms = 3)
#' scores <- predict(mymodel, type = "topics", newdata = dtm)
predict.LDA_VEM <- function(object, newdata, type = c("topics", "terms"), min_posterior=-1, min_terms=0, labels, ...){
  requireNamespace("topicmodels")
  type <- match.arg(type)
  if(type == "topics"){
    newdata <- newdata[, object@terms]
    idx <- Matrix::rowSums(newdata)
    idx_ok <- which(idx > 0)
    idx_nok <- which(idx == 0)
    scores <- topicmodels::posterior(object, newdata = newdata[idx_ok, ], ...)$topics
    if(missing(labels)){
      labels <-  sprintf("topic_%03d", seq_len(ncol(scores)))
      colnames(scores) <- sprintf("topic_%03d", seq_len(ncol(scores)))
    }else{
      colnames(scores) <- sprintf("topic_%s", labels)
    }
    topic <- apply(scores, MARGIN = 1, FUN=which.max)
    topic <- apply(scores, MARGIN = 1, FUN=function(x){
      out <- list()
      out$topic <- which.max(x)
      out$topic_prob <- x[out$topic]
      out$topic_probdiff_2nd <- out$topic_prob - max(x[-out$topic])
      out
    })
    recoder <- function(x, from, to){
      idx <- match(x, from)
      to[idx]
    }
    doc_ids <- rownames(newdata)
    if(is.null(doc_ids)){
      doc_ids <- seq_len(nrow(newdata))
    }
    result <- list(ok = data.table(doc_id = doc_ids[idx_ok],
                                   topic = sapply(topic, FUN=function(x) x$topic),
                                   topic_label = recoder(sapply(topic, FUN=function(x) x$topic), from = seq_along(labels), to = labels),
                                   topic_prob = sapply(topic, FUN=function(x) x$topic_prob),
                                   topic_probdiff_2nd = sapply(topic, FUN=function(x) x$topic_probdiff_2nd),
                                   scores),
                   nok = data.table(doc_id = doc_ids[idx_nok]))
    result <- rbindlist(result, fill = TRUE) 
    result <- setDF(result)
    result <- result[match(doc_ids, result$doc_id), ]
    rownames(result) <- NULL
    return(result)
  }else{
    topicmodel_terms <- function(object, min_posterior, min_terms, labels){
      x <- t(topicmodels::posterior(object, ...)$terms)
      if(missing(labels)){
        labels <-  sprintf("topic_%03d", seq_len(ncol(x)))
      }
      terms <- object@terms
      topicterms <- list()
      for(topic in 1:ncol(x)){
        topicterms[[sprintf("topic_%03d", topic)]] <- x[, topic]
        names(topicterms[[sprintf("topic_%03d", topic)]]) <- terms
      }
      topicterms <- lapply(topicterms, FUN=function(x){
        x <- sort(x, decreasing = TRUE)
        x <- data.frame(term = names(x), prob = as.numeric(x), stringsAsFactors = FALSE)
        if(sum(x$prob >= min_posterior) <= min_terms){
          x <- head(x, n = min_terms)
        }else{
          x <- x[x$prob >= min_posterior, ]  
        }
      })
      topicterms
    }
    result <- topicmodel_terms(object = object, min_posterior = min_posterior, min_terms = min_terms, labels = labels)
  }
  result
}

#' @export
#' @S3method predict LDA_Gibbs
#' @rdname predict.LDA
predict.LDA_Gibbs <- predict.LDA_VEM