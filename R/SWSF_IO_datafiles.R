########################
#------ datafile-IO functions

#' Read a comma-separated value (csv) file
#'
#' Call function \code{\link[iotools]{read.csv.raw}}, if available, or else,
#' \code{\link[utils]{read.csv}}. \code{\link[iotools]{read.csv.raw}} can be much faster,
#'  particularly for large files. It reads, however, only \code{nrowsClasses} rows to
#'  determine the class of a column unlike \code{\link[utils]{read.csv}} which uses all
#'  rows to determine the column class.
#'
#' @param file A character string. The path to the file which is to be read.
#' @param stringsAsFactors A logical value. Should character vectors be converted to
#'  factors?
#' @param use_iotools A logical value. If \code{TRUE} and if \pkg{iotools} available,
#'  then \code{\link[iotools]{read.csv.raw}} instead of \code{\link[utils]{read.csv}} is
#'  used to read the \code{file}.
#' @param ... Further arguments to be passed to \code{\link[iotools]{read.csv.raw}} or
#'  \code{\link[utils]{read.csv}}.
#'
#' @return A data frame (\code{\link[base]{data.frame}}) containing a representation of
#'  the data in the file.
swsf_read_csv <- compiler::cmpfun(function(file, stringsAsFactors = FALSE,
  use_iotools = TRUE, ...) {

  dots <- list(...)
  dots[["file"]] <- file
  dots[["stringsAsFactors"]] <- stringsAsFactors
  use_iotools <- requireNamespace("iotools", quietly = TRUE) && use_iotools
  res <- NULL

  if (use_iotools) {
    # faster than utils::read.csv
    dots2 <- dots[names(dots) %in% names(formals(iotools::read.csv.raw))]
    if (!any(names(dots2) == "nrowsClasses"))
      dots2[["nrowsClasses"]] <- 1000L

    temp <- try(do.call(iotools::read.csv.raw, args = dots2), silent = TRUE)
    if (inherits(temp, "try-error")) {
      use_iotools <- FALSE
    } else {
      names(temp) <- gsub("\"", "", names(temp))
      res <- temp
    }
  }

  if (!use_iotools) {
    dots2 <- dots[names(dots) %in% names(formals(utils::read.table))]
    res <- try(do.call(utils::read.csv, args = dots2), silent = TRUE)
  }

  res
})


#' Read the data from a 'SWSF-inputfile'
#'
#' 'SWSF-inputfiles' are comma-separated value files with \itemize{
#'  \item First row: field names of which the first one is 'Label'
#'  \item Second row: flags indicating which column information is applied (1) or not (0);
#'    the first entry is the character string 'UseInformationToCreateSoilWatRuns'.
#'  \item Third - last row: values of the input file; first column: site labels.
#' }
#'
#' @inheritParams swsf_read_csv
#'
#' @return A list of length two with the elements \describe{
#'  \item{use}{A named logical vector. The names are from the first row of the \code{file}
#'    and the values are \code{FALSE} if the second row of the \code{file} contains a 0
#'    and \code{TRUE} otherwise. The first entry, corresponding to column 'Label' is
#'    always \code{FALSE}.
#'  \item{data}{A data frame (\code{\link[base]{data.frame}}) containing a representation
#'    of the values in the \code{file} with column names from the first row of the
#'    \code{file}.}
#' }
swsf_read_inputfile <- compiler::cmpfun(function(file, header_rows = 1, use_iotools = TRUE,
  ...) {

  sw_use <- tryCatch(swsf_read_csv(file, nrows = header_rows, use_iotools = use_iotools),
    error = function(e) print(paste("Failed to read file:", shQuote(basename(file)), "with", e)))
  sw <- swsf_read_csv(file, skip = header_rows, use_iotools = use_iotools, ...)
  names(sw) <- names(sw_use)
  sw_use <- c(FALSE, as.logical(as.numeric(sw_use[, -1])))
  sw_use[is.na(sw_use)] <- FALSE
  names(sw_use) <- names(sw)

  list(use = sw_use, data = sw)
})

#' Re-combine elements to create a 'SWSF-inputfile'
#'
#' Combines the output of \code{\link{swsf_read_inputfile}} to a data frame
#' (\code{\link[base]{data.frame}}) with proper 'SWSF-inputfile' format. This can be
#'  written back to disk.
#'
#' @param sw_use A named logical vector. See element \code{use} described under the
#'  section \code{Value} of \code{\link{swsf_read_inputfile}}.
#' @param data A named logical vector. See element \code{data} described under the
#'  section \code{Value} of \code{\link{swsf_read_inputfile}}.
#'
#' @return A data frame (\code{\link[base]{data.frame}}) with proper 'SWSF-inputfile'
#'  format.
reconstitute_inputfile <- compiler::cmpfun(function(sw_use, data) {
  temp <- as.data.frame(matrix(as.integer(sw_use), nrow = 1L))
  colnames(temp) <- names(sw_use)
  temp[1, 1] <- "UseInformationToCreateSoilWatRuns"
  rbind(temp, data)
})

#------ End of datafile-IO functions
########################
