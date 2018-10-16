
# Post-mortem:
# Note: wrap in `try(.)` because running unit tests with `devtools::test()`
# isn't able here to locate the (hidden) function `testthat:::test_files`;
# however, `devtools` runs the unit tests in a clean and separate environment,
# thus, the traced copy of `testthat:::test_files` is not propagated to the
# global environment
try(untrace(quote(test_files), where = asNamespace("testthat")), silent = TRUE)

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


# Clean-up:
Sys.unsetenv("RSFSW2_ALLTESTS")
Sys.unsetenv("RSFSW2_SAVETESTS")
