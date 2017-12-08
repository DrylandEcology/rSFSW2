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

  coords_WGS84 <- data.frame(
    X_WGS84 = c(-105.5906, -72.595),
    Y_WGS84 = c(41.31139, 43.26278))
  N <- nrow(coords_WGS84)
  site_dat <- cbind(Label = c(paste("DM_test", seq_len(N), sep = "_")), coords_WGS84)
  dw_source <- dw_names <- rep(NA, N)

  avail_end_year <- as.integer(1900 + as.POSIXlt(Sys.Date())$year - 1)
  sim_testtimes <- list(
    t1 = c(overall_simstartyr = 1980, overall_endyr = 1985),
    t2 = c(overall_simstartyr = 1980, overall_endyr = avail_end_year),
    t3 = c(overall_simstartyr = avail_end_year - 1, overall_endyr = avail_end_year)
  )

  # Expected outputs
  temp <- rep("DayMet_NorthAmerica", N)
  dw_source_exp <- list(t1 = temp, t2 = temp, t3 = temp)
  temp <- c("DM_test_1_DayMet-105.5906_41.3114", "DM_test_2_DayMet-72.5950_43.2628")
  dw_name_exp <- list(t1 = temp, t2 = temp, t3 = temp)
  dw_n_exp <- list(t1 = N, t2 = N, t3 = N)

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
            start_year = sim_time[["overall_simstartyr"]], end_year = sim_time[["overall_endyr"]],
            dbW_digits = 2L)

          expect_equal(unique(sapply(x, class)), "swWeatherData")
          expect_equal(length(x), sim_time[["overall_endyr"]] - sim_time[["overall_simstartyr"]] + 1)
        }
      }
    }
  })


  #--- Clean up
  unlink(list.files(dm_path, "daymet", full.names = TRUE))
}
