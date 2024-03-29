% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{txt_grepl}
\alias{txt_grepl}
\title{Look up a multiple patterns and indicate their presence in text}
\usage{
txt_grepl(
  x,
  pattern,
  FUN = all,
  ignore.case = FALSE,
  perl = FALSE,
  fixed = FALSE,
  useBytes = FALSE,
  ...
)
}
\arguments{
\item{x}{a character vector}

\item{pattern}{a character vector containing one or several regular expressions}

\item{FUN}{a function to apply to combine the results ot the different regular expressions for each element of \code{x}. 
Defaults to \code{\link{all}}.}

\item{ignore.case}{passed on to \code{\link{grepl}}}

\item{perl}{passed on to \code{\link{grepl}}}

\item{fixed}{passed on to \code{\link{grepl}}}

\item{useBytes}{passed on to \code{\link{grepl}}}

\item{...}{further arguments passed on to \code{FUN}}
}
\value{
a logical vector with the same length as \code{x} with the result of the call to \code{FUN} applied elementwise to each result of grepl for each pattern
}
\description{
A variant of \code{\link{grepl}} which allows to specify multiple regular expressions
and allows to combine the result of these into one logical vector.\cr 
You can specify how to combine the results of the regular expressions by specifying
an aggregate function like \code{\link{all}}, \code{\link{any}}, \code{\link{sum}}.
}
\examples{
x <- c("--A--", "--B--", "--ABC--", "--AC--", "Z")
txt_grepl(x, pattern = c("A", "C"), FUN = all)
txt_grepl(x, pattern = c("A", "C"), FUN = any)
txt_grepl(x, pattern = c("A", "C"), FUN = sum)
data.frame(x = x, 
           A_and_C = txt_grepl(x, pattern = c("A", "C"), FUN = all),
           A_or_C  = txt_grepl(x, pattern = c("A", "C"), FUN = any),
           A_C_n   = txt_grepl(x, pattern = c("A", "C"), FUN = sum))
txt_grepl(x, pattern = "A|C")
}
\seealso{
\code{\link{grepl}}
}
