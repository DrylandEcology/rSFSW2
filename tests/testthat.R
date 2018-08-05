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

test_check("rSFSW2", reporter = ListReporter, encoding = "UTF-8")
