context("Daily weather data: DayMet")

# skip_on_travis() and skip_on_appveyor() are meant to be used within test_that() calls.
# Here, we need to skip preparation code outside of a test_that call. However, the skip_*
# functions cause CIs to error out, even when wrapped in try() statements, with
#   - "Error: On Appveyor", respectively
#   - "Error: On Travis"
# Check values of the ENV variables directly as a work-around:

do_skip <- c(
  identical(tolower(Sys.getenv("NOT_CRAN")), "false") # whereas skip_on_cran() skips if not "true", I believe it should skip only if "false" (i.e., not "" and not "true")
)

suppressWarnings(is_online <-
  !inherits(try(close(url(getOption("repos"), open = "r")), silent = TRUE),
  "try-error"))

if (!any(do_skip) && is_online) {

  #--- Inputs
  dm_path <- tempdir()
  exinfo <- list(GriddedDailyWeatherFromDayMet_NorthAmerica = TRUE)

  N <- 2L
  coords_WGS84 <- data.frame(
    X_WGS84 = c(-120, 0),
    Y_WGS84 = c(40, 0))
  site_dat <- cbind(Label = c(paste("DM_test", seq_len(N), sep = "_")), coords_WGS84)
  dw_source <- dw_names <- rep(NA, N)

  sim_testtimes <- list(
    t1 = c(simstartyr = 1980, endyr = 2016),
    t2 = c(simstartyr = 1960, endyr = 2016),
    t3 = c(simstartyr = 1980, endyr = 1900 + as.POSIXlt(Sys.time(), tz = "UTC")$year + 1)
  )

  # Expected outputs
  dw_source_exp <- list(t1 = c("DayMet_NorthAmerica", NA),
    t2 = rep(NA, N), t3 = rep(NA, N))
  dw_name_exp <- list(t1 = c("DM_test_1_DayMet-120.0000_40.0000", NA),
    t2 = rep(NA, N), t3 = rep(NA, N))
  dw_n_exp <- list(t1 = 1, t2 = 0, t3 = 0)

  #--- Tests
  test_that("DayMet weather data:", {
    for (k in seq_along(sim_testtimes)) {
      sim_time <- sim_testtimes[[k]]

      dw <- dw_DayMet_NorthAmerica(dw_source, dw_names, exinfo, site_dat, sim_time,
        path = dm_path)
      expect_named(dw, c("source", "name", "n"))
      expect_equal(dw[["source"]], dw_source_exp[[k]])
      expect_equal(dw[["name"]], dw_name_exp[[k]])
      expect_equal(dw[["n"]], dw_n_exp[[k]])

      dm <- get_DayMet_cellID(coords_WGS84)
      expect_named(dm, c("cellID", "dm_LCC", "dm_WGS84"))
      expect_equal(unname(sapply(dm, NROW)), rep(N, 3))

      for (i in seq_len(N)) {
        if (identical(dw[["source"]][i], "DayMet_NorthAmerica")) {
          x <- get_DayMet_NorthAmerica(dir_data = dm_path, cellID = dm[["cellID"]][i],
            Xdm_WGS84 = dm$dm_WGS84[i, 1], Ydm_WGS84 = dm$dm_WGS84[i, 2],
            start_year = sim_time[["simstartyr"]], end_year = sim_time[["endyr"]],
            dbW_digits = 2L)

          expect_equal(unique(sapply(x, class)), "swWeatherData")
          expect_equal(length(x), sim_time[["endyr"]] - sim_time[["simstartyr"]] + 1)
        }
      }
    }
  })


  #--- Clean up
  unlink(list.files(dm_path, "daymet", full.names = TRUE))
}
