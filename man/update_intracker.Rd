% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Status_Trackers.R
\name{update_intracker}
\alias{update_intracker}
\title{Update input tracker status}
\usage{
update_intracker(
  ist,
  tracker,
  prepared = NULL,
  checked = NULL,
  clean_subsequent = FALSE
)
}
\arguments{
\item{ist}{A data.frame representing an input tracker as generated by
function \code{init_intracker}.}

\item{tracker}{A character string. One of the rownames of \code{ist}.}

\item{prepared}{A logical value or \code{NULL}. If not \code{NULL}, then the
requested tracker will be updated with this value and its time stamp set.}

\item{checked}{A logical value or \code{NULL}. If not \code{NULL}, then the
requested tracker will be updated with this value and its time stamp set.}

\item{clean_subsequent}{A logical value. If \code{TRUE} then trackers in rows
greater than \code{tracker} will be reset to \code{FALSE}.}
}
\value{
The updated data.frame \code{ist}.
}
\description{
Update input tracker status
}
\examples{
 # Create a new tracker object: if using 'demo' code to run a project,
 # then the object 'SFSW2_prj_meta' will contain a tracker object with
 # name 'input_status'
 SFSW2_prj_meta <- list(input_status = rSFSW2:::init_intracker())

 # Update the 'soil_data' trackers to indicate that data extraction has
 # already been carried out (e.g., soil data were entered separately)
 SFSW2_prj_meta[['input_status']] <- update_intracker(
   SFSW2_prj_meta[['input_status']], tracker = "soil_data", prepared = TRUE)

 # Reset the 'dbW_scenarios' trackers to so that both data
 # population/extraction and checking steps are carried out again during
 # the next run
 SFSW2_prj_meta[['input_status']] <- update_intracker(
   SFSW2_prj_meta[['input_status']], tracker = "dbW_scenarios",
   prepared = FALSE, checked = FALSE)

 # Save updated metadata with tracker object to project directory to prepare
 # for next run
 SFSW2_prj_meta[["fnames_in"]] <- list(fmeta = tempfile())
 saveRDS(SFSW2_prj_meta, file = SFSW2_prj_meta[["fnames_in"]][["fmeta"]])

 #Clean up from example
 unlink(SFSW2_prj_meta[["fnames_in"]][["fmeta"]])

}
