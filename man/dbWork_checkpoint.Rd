% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WorkDatabase.R
\name{dbWork_checkpoint}
\alias{dbWork_checkpoint}
\title{Initiate a checkpoint operation on a SQLite-database \code{dbWork} of a rSFSW2 simulation project}
\usage{
dbWork_checkpoint(path = NULL, con = NULL, mode = c("PASSIVE", "FULL",
  "RESTART", "TRUNCATE", ""), failure = c("silent", "warning", "error"))
}
\description{
Initiate a checkpoint operation on a SQLite-database \code{dbWork} of a rSFSW2 simulation project
}
\references{
https://www.sqlite.org/pragma.html#pragma_wal_checkpoint
}