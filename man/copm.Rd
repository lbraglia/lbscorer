% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/copm.R
\name{copm}
\alias{copm}
\title{Canadian Occupational Performance Measure}
\usage{
copm(id, performance1, satisfaction1, performance2, satisfaction2)
}
\arguments{
\item{id}{patient id: there can be max 5 rows per id, or an error
is thrown}

\item{performance1}{performance self assessment at first evaluation}

\item{satisfaction1}{satisfaction self assessment at first
evaluation}

\item{performance2}{performance self assessment at re-evaluation}

\item{satisfaction2}{satisfaction self assessment at re-evaluation}
}
\description{
Canadian Occupational Performance Measure calculation given study variables
}
\examples{
set.seed(1)
id <- gl(2,5)
perf1 <- sample(1:10, replace = TRUE)
perf2 <- sample(1:10, replace = TRUE)
sat1 <- sample(1:10, replace = TRUE)
sat2 <- sample(1:10, replace = TRUE)

data.frame(id, perf1, sat1, perf2, sat2)
copm(id, perf1, sat1, perf2, sat2)

}
