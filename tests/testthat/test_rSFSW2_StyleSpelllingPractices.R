context("Code style, spell checks, and good package practices")


#--- Code style
test_that("Package code style", {
  # Check locally and on travis
  skip_on_cran()
  skip_on_appveyor()
  # minimum version of lintr required for: empty commas in array subsetting
  skip_if_not_installed("lintr", minimum_version = "1.0.2.9000")


  # Once all files pass, then remove individual `lintr::lint()` and replace
  # with `lintr::expect_lint_free`
  if (TRUE) {
    files_ready_tolint <- c(
      "../../R/Aggregation_Functions.R",
#      "../../R/ExtractData_ClimateDownscaling.R",
      "../../R/ExtractData_Elevation.R",
      "../../R/ExtractData_MeanMonthlyClimate.R",
      "../../R/ExtractData_Soils.R",
      "../../R/GISSM.R",
      "../../R/Indices.R",
      "../../R/IO_databases.R",
      "../../R/IO_datafiles.R",
      "../../R/Mathematical_Functions.R",
      "../../R/Miscellaneous_Functions.R",
      "../../R/netCDF_prepare_climatedata_files.R",
#      "../../R/OutputDatabase_Ensembles.R",
#      "../../R/OutputDatabase.R",
      "../../R/Parallel.R",
      "../../R/Pedotransfer_Functions.R",
      "../../R/PriorCalculations.R",
      "../../R/RandomNumberGenerator.R",
#      "../../R/RcppExports.R",
      "../../R/rSFSW2-package.R",
      "../../R/rSOILWAT2_DataAccess.R",
#      "../../R/Simulation_Project.R",
#      "../../R/Simulation_Run.R",
#      "../../R/SoilMTRegimes.R",
#      "../../R/Soils_Functions.R",
#      "../../R/Spatial_Functions.R",
#      "../../R/Status_Trackers.R",
#      "../../R/Synchronicity.R",
#      "../../R/Testproject_Functions.R",
#      "../../R/Time_SimulationWorld.R",
#      "../../R/Timing_Calls.R",
#      "../../R/upgraders.R",
#      "../../R/Vegetation.R",
#      "../../R/WeatherDB_Check.R",
#      "../../R/WeatherDB.R",
#      "../../R/WorkDatabase.R",
      "../../R/zzz.R",
      NULL
    )

    for (f in files_ready_tolint) {
      badstyle <- lintr::lint(f)
      expect_identical(length(badstyle), 0L, info = print(badstyle))
    }

  } else {
    lintr::expect_lint_free()
  }
})



#--- Spell check documentation
test_that("Package spell checks", {
  # Check locally and on travis
  skip_on_cran()
  skip_on_appveyor()
  skip_if_not_installed("devtools")

  # ignore additional words (keep as short as possible)
  spell_ignores <- scan("../test_data/spell_words.txt", what = "character",
    comment.char = "#", quiet = TRUE)

  # against initial believe, we do not need to ignore R function names
  if (FALSE) {
    # ignore R functions of packages on the search path
    temp <- grep("package:", search(), value = TRUE)
    R_functions1 <- sort(unique(unlist(lapply(temp, function(pkg)
      as.character(lsf.str(pkg))))))

    # ignore R functions of packages imported, suggested, or linked to by rSFSW2
    temp <- utils::packageDescription("rSFSW2", fields = c("Depends",
      "Imports", "Suggests", "LinkingTo"))
    temp <- unname(unlist(sapply(temp,
      tools:::.extract_dependency_package_names)))
    R_functions2 <- sort(unique(unlist(lapply(temp, getNamespaceExports))))
  }

  # Spell check with `hunspell`:
  #   ignores text in roxygen2 content directives, e.g., \url{}, \var{},
  #   but checks text in roxygen2 formatting directives, e.g., \sQuote{}
  misspelled <- devtools::spell_check(ignore = spell_ignores)

  expect_identical(length(misspelled), 0L, info = print(misspelled))
})



#---
test_that("Package good practices", {
  skip(paste("rSFSW2 is not ready for 'good practices' ...;",
    "'goodpractice' should be run manually instead"))
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not_installed("goodpractice")

  # this path assumes that the current working directory is
  # 'rSFSW2/tests/testthat/'
  gps <- goodpractice::gp(path = "../..")

  expect_identical(length(goodpractice::failed_checks(gps)), 0L,
    info = print(gps))
})

