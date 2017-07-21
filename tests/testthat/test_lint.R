context("lints")

test_that("Package Style", {
  skip("rSFSW2 is not ready for 'lint' testing...")
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not_installed("lintr")

  lintr::expect_lint_free()
})

