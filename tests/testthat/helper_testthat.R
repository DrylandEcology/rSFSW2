# `helper_*.R`: executed before tests and during `devtools::load_all()`
# (see `?test_check`)


#' Locate copy of source package relative to working directory during unit tests
#
#' During testing the current/working path \code{getwd()} is set to
#' \code{tests/testthat/}.
#' \itemize{
#'   \item If tests/checks are run with \code{R CMD check .} or with
#'     \code{devtools::check()}, then (a copy of) the source of \pkg{rSFSW2}
#'     is located at \code{some_path/rSFSW2.Rcheck/00_pkg_src/rSFSW2/}.
#'   \item If tests/checks are run with \code{devtools::test()}, then the
#'     source code is located at \code{../../}.
#' @return A relative path.
pkg_temp_dir <- function() {
  path <- file.path("..", "..", "00_pkg_src", "rSFSW2")

  if (!dir.exists(path)) {
    path <- file.path("..", "..")
  }

  if (!dir.exists(path) && interactive() && requireNamespace("pkgload")) {
    path <- pkgload::pkg_path()
  }

  path
}
