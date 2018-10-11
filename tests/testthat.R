library("testthat")
library("rSFSW2")

# Make sure that environmental variable \code{NOT_CRAN} is set to either
# \var{\dQuote{true}} or \var{\dQuote{false}}
if (!identical(tolower(Sys.getenv("NOT_CRAN")), "false")) {
  # \code{testthat::skip_on_cran()} requires a value of \var{\dQuote{true}}
  # for the environmental variable \code{NOT_CRAN} to not skip.
  # However, only \pkg{devtools} sets \code{NOT_CRAN}. For instance,
  # `R CMD check *tar.gz` without `--as-cran`) does not set \code{NOT_CRAN},
  # thus, \code{testthat::skip_on_cran()} skips even though unintended
  Sys.setenv(NOT_CRAN = "true")
}

# \code{rSFSW2_test_verbose} is a hack to obtain unit test information
# even if run with `R CMD check .` which, otherwise, reports only the last
# 13 lines of output.
# If \code{rSFSW2_test_verbose} is \code{TRUE}, then instruct \pkg{testthat}
# to save \code{results} to file before returning after erroring out with
# \code{stop("Test failures", call. = FALSE)}
#
# If \pkg{testthat} will allow to store output to file, then this hack could be
# removed (see https://github.com/r-lib/testthat/issues/795)
rSFSW2_test_verbose <- !interactive()

if (rSFSW2_test_verbose) {
    trace(quote(test_files),
      exit = quote(saveRDS(results, file = "testthat_results.rds")),
      where = asNamespace("testthat"))
}


# Run package tests
test_check("rSFSW2", reporter = ListReporter, encoding = "UTF-8")

if (rSFSW2_test_verbose) {
  untrace(quote(test_files), where = asNamespace("testthat"))

  if (FALSE) {
    # Demo code to illustrate how to read in reporter output and display content
    # See also \code{\link[testthat]{find_reporter}} and
    # \code{\link[testthat]{with_reporter}}

    # If produced from running \code{R CMD check .}, then the file is located at
    #   \var{rSFSW2.Rcheck/tests/}
    utres <- readRDS("testthat_results.rds")

    r <- ListReporter$new()
    r$start_reporter()
    force(utres)
    r$end_reporter()
  }
}
