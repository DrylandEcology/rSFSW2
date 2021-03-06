% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WorkDatabase.R
\name{dbWork_check_run}
\alias{dbWork_check_run}
\title{Check run status}
\usage{
dbWork_check_run(path, runIDs)
}
\arguments{
\item{path}{A character string. Path to the folder where the database will be
created.}

\item{runIDs}{An integer vector. The identification numbers of queried runs,
i.e., values out of \code{runIDs_total}, see \code{\link{indices}}.}
}
\value{
A data.frame with three columns \var{\dQuote{completed}},
  \var{\dQuote{failed}}, and \var{\dQuote{inwork}} and one row per
  \code{runIDs}.
}
\description{
Check run status
}
