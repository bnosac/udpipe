% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nlp_flow.R
\name{dtm_remove_sparseterms}
\alias{dtm_remove_sparseterms}
\title{Remove terms with high sparsity from a Document-Term-Matrix}
\usage{
dtm_remove_sparseterms(dtm, sparsity = 0.99, remove_emptydocs = TRUE)
}
\arguments{
\item{dtm}{an object returned by \code{\link{document_term_matrix}}}

\item{sparsity}{numeric in 0-1 range indicating the sparsity percent. Defaults to 0.99 meaning drop terms which occur in less than 1 percent of the documents.}

\item{remove_emptydocs}{logical indicating to remove documents containing no more terms after the term removal is executed. Defaults to \code{TRUE}.}
}
\value{
a sparse Matrix as returned by \code{sparseMatrix} 
where terms with high sparsity are removed and documents without any terms are also removed
}
\description{
Remove terms with high sparsity from a Document-Term-Matrix and remove documents with no terms.\cr
Sparsity indicates in how many documents the term is not occurring.
}
\examples{
data(brussels_reviews_anno)
x <- subset(brussels_reviews_anno, xpos == "NN")
x <- x[, c("doc_id", "lemma")]
x <- document_term_frequencies(x)
dtm <- document_term_matrix(x)


## Remove terms with low frequencies and documents with no terms
x <- dtm_remove_sparseterms(dtm, sparsity = 0.99)
dim(x)
x <- dtm_remove_sparseterms(dtm, sparsity = 0.99, remove_emptydocs = FALSE)
dim(x)
}
