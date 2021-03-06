context("Code style and good package practices")

#--- Inputs
pkg_path <- pkg_temp_dir()


#--- Code style
test_that("Package code style", {
  # Check locally and on travis
  skip_if_not(identical(tolower(Sys.getenv("RSFSW2_ALLTESTS")), "true"))
  skip_on_cran()
  skip_on_appveyor()
  skip_if_not_installed("lintr", minimum_version = "2.0.0")

  # Files that are not checked for lints
  files_not_tolint <- c(
    "ExtractData_ClimateDownscaling.R", # needs linting
    "Simulation_Run.R", # needs linting
    "OutputDatabase_Ensembles.R" # this is de-facto deprecated & defunct
  )

  # Note: working directory when these tests are run is at `tests/testthat/`
  if (FALSE) {
    # `expect_lint_free` as of v1.0.2.9000 (built 2018-05-27)
    #   - calls `testthat::skip_on_cran()`
    #   - argument `exclusions` is documented to take
    #     "[filenames] relative to the package path
    #     --> I find that only absolute paths works correctly
    lintr::expect_lint_free(
      # Files that should not be linted:
      exclusions = as.list(
        normalizePath(file.path(pkg_path, "R", files_not_tolint))))

  } else {
    # easier to work with interactively than `lintr::expect_lint_free`
    # additionally, `lintr::expect_lint_free` calls `lintr:::lint_package`
    # which only considers code in "R", "tests", "inst", but not in "data-raw",
    # "doc", or "demo"
    dir_code <- file.path(pkg_path,
      c("data-raw", "demo", "R", "tests", "inst"))
    pkg_code_files <- list.files(path = dir_code, pattern = "\\.R$",
      ignore.case = TRUE, full.names = TRUE, recursive = TRUE)
    pkg_code_files <- normalizePath(pkg_code_files)

    ids <- !(basename(pkg_code_files) %in% files_not_tolint)
    files_tolint <- pkg_code_files[ids]

    for (k in seq_along(files_tolint)) {
      badstyle <- lintr::lint(files_tolint[k])
      expect_identical(length(badstyle), 0L, info = print(badstyle))
    }
  }
})



#---
test_that("Package good practices", {
  skip(paste("rSFSW2 is not ready for 'good practices' ...;",
    "'goodpractice' should be run manually instead"))
  skip_if_not(identical(tolower(Sys.getenv("RSFSW2_ALLTESTS")), "true"))
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not_installed("goodpractice")

  gps <- goodpractice::gp(path = pkg_path)

  expect_identical(length(goodpractice::failed_checks(gps)), 0L,
    info = print(gps))
})
