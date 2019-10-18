context("Spell checks")

#--- Inputs
pkg_path <- pkg_temp_dir()



#--- Spell check
test_that("Package spell checks", {
  # Check locally and on travis
  skip_if_not(identical(tolower(Sys.getenv("RSFSW2_ALLTESTS")), "true"))
  skip_on_cran()
  skip_on_appveyor()
  skip_if_not_installed("spelling", minimum_version = "1.1.0")

  # Spell check with `hunspell` as backend:
  # TODO: turn spell-checking on for vignettes
  misspelled <- spelling::spell_check_package(pkg_path, vignettes = FALSE)

  expect_identical(length(misspelled[["word"]]), 0L, info = print(misspelled))
})
