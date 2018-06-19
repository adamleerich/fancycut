% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fancycut.R
\name{wafflecut}
\alias{wafflecut}
\title{Like \code{cut}, turn a vector of numbers into a factor}
\usage{
wafflecut(x, intervals, buckets = intervals, na.bucket = NA,
  unmatched.bucket = NA, out.as.factor = TRUE)
}
\arguments{
\item{x}{a numeric vector}

\item{intervals}{a character vector of intervals}

\item{buckets}{a character vector of levels for the new factor
these have a 1-1 correspondence with \code{intervals}}

\item{na.bucket}{what level should NA values be given?}

\item{unmatched.bucket}{what level should numbers not covered by an interval be given?}

\item{out.as.factor}{default is TRUE
Should the resulting vector be a factor?
If FALSE will return a character vector.}
}
\description{
Like \code{cut}, turn a vector of numbers into a factor
}
\examples{
wafflecut(-10:10, c('(0,2]','(2,5)','[5,10]'), c('Small','Medium','Large'))
wafflecut(-10:10, c('[0,0]','(0,2]','(2,5)','[5,10]'), c('Zero','Small','Medium','Large'))
wafflecut(-10:10, c('(0,2]','(2,5)','[5,10]'), c('Small','Medium','Large'))
wafflecut(-10:10, c('[0,0]','(0,2]','(2,5)','[5,10]'), c('Zero','Small','Medium','Large'))
}