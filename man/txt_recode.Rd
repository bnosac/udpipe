% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{txt_recode}
\alias{txt_recode}
\title{Recode text to other categories}
\usage{
txt_recode(x, from = c(), to = c(), na.rm = FALSE)
}
\arguments{
\item{x}{a character vector}

\item{from}{a character vector with values of \code{x} which you want to recode}

\item{to}{a character vector with values of you want to use to recode to where you
want to replace values of \code{x} which correspond to \code{from[i]} to \code{to[i]}}

\item{na.rm}{logical, if set to TRUE, will put all values of \code{x} which have no
matching value in \code{from} to NA. Defaults to \code{FALSE}}
}
\value{
a character vector of the same length of \code{x} where values of \code{x}
which are given in \code{from} will be replaced by the corresponding element in \code{to}
}
\description{
Recode text to other categories. 
Values of \code{x} which correspond to \code{from[i]} will be recoded to \code{to[i]}
}
\examples{
x <- c("NOUN", "VERB", "NOUN", "ADV")
txt_recode(x = x,
           from = c("VERB", "ADV"),
           to = c("conjugated verb", "adverb"))
txt_recode(x = x,
           from = c("VERB", "ADV"),
           to = c("conjugated verb", "adverb"),
           na.rm = TRUE)
txt_recode(x = x,
           from = c("VERB", "ADV", "NOUN"),
           to = c("conjugated verb", "adverb", "noun"),
           na.rm = TRUE)
}
\seealso{
\code{\link{match}}
}
