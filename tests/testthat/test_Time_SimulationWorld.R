context("Time in the simulation world")

#--- INPUTS
SFSW2_prj_inputs <- list()

input_sim_time <- list(
  # test object 1: startyr is leap year
  startyr_leapyear = list(
    simstartyr = 1979,
    startyr = startyr <- 1980,
    endyr = endyr <- 2010,
    DScur_startyr = startyr,
    DScur_endyr = endyr,
    future_yrs = list(
      c(d <- 40, startyr + d, endyr + d),
      c(d <- 90, startyr + d, endyr + d - 1)
    )
  ),
  # test object 2: startyr is not leap year
  startyr_noleapyear = list(
    simstartyr = 1969,
    startyr = startyr <- 1970,
    endyr = endyr <- 2000,
    DScur_startyr = startyr,
    DScur_endyr = endyr,
    future_yrs = list(
      c(d <- 40, startyr + d, endyr + d),
      c(d <- 90, startyr + d, endyr + d - 1)
    )
  )
)

SFSW2_prj_meta[["opt_agg"]][["doy_ranges"]] <-  list(
  dailyFrostinSnowPeriod = c(1,250),
  default = c(1, 250),
  defaultWateryear = c(300, 30))
SFSW2_prj_meta[["opt_agg"]][["use_doy_range"]] <- FALSE


expected_sim_time_elements <- c("simstartyr", "startyr", "endyr", "DScur_startyr", "DScur_endyr",
"future_yrs", "spinup_N", "future_N", "useyrs", "no.useyr", "no.usemo",
"no.usedy", "index.useyr", "index.usemo", "index.usedy")

expected_sim_time_elements_expanded <- c(expected_sim_time_elements,
  "overall_simstartyr", "overall_endyr")

# Corrupted input: will produce error
input_sim_timeE <- input_sim_time[[1]]
input_sim_timeE[["future_yrs"]] <- list(
    c(d <- 40, startyr + d, endyr + d),
    c(d <- 90, startyr + d)
  )



#--- TESTS
test_that("Obtain time information", {
  # Spinup of simulation
  expect_equal(getStartYear(1980), 1981L)
  expect_equal(getStartYear(0), 1L)
  expect_equal(getStartYear(0, 10), 10L)


  # Leap years
  expect_true(isLeapYear(2000))
  expect_true(isLeapYear(2016))
  expect_false(isLeapYear(2100))
  expect_false(isLeapYear(2003))


  # Sequence of month numbers for each day in the period
  expect_equal(seq_month_ofeach_day(list(1980, 1, 1), list(2010, 12, 31), tz = "UTC"),
    as.POSIXlt(seq(from = ISOdate(1980, 1, 1, tz = "UTC"),
    to = ISOdate(2010, 12, 31, tz = "UTC"), by = "1 day"))$mon + 1)


  # Setup simulation time
  expect_error(setup_simulation_time(input_sim_timeE),
    regexp = "incorrect format of 'future_yrs'")
  sim_time <- list()
  for (k in seq_along(input_sim_time)) {
    info <- names(input_sim_time)[k]
    expect_silent(sim_time[[k]] <- setup_simulation_time(input_sim_time[[k]],
      add_st2 = TRUE, adjust_NS = TRUE))
    expect_equal(sim_time[[k]][["useyrs"]],
      sim_time[[k]][["startyr"]]:sim_time[[k]][["endyr"]], info = info)
    expect_true(all(expected_sim_time_elements %in% names(sim_time[[k]])), info = info)

    # Overall span of simulation period
    expect_silent(sim_time[[k]] <- determine_overall_simulation_time(sim_time[[k]],
      SFSW2_prj_inputs))
    expect_equal(sim_time[[k]][["overall_simstartyr"]], sim_time[[k]][["simstartyr"]])
    expect_equal(sim_time[[k]][["overall_endyr"]], sim_time[[k]][["endyr"]])
  }

  # Simulation time aggregation lists
  st2 <- list(N = list(), S = list())
  for (k in seq_along(sim_time)) {
    expect_silent(st2[["N"]] <- simTiming_ForEachUsedTimeUnit(sim_time[[k]],
      latitude = 90))
    expect_silent(st2[["S"]] <- simTiming_ForEachUsedTimeUnit(sim_time[[k]],
      latitude = -90))
    n_days <- sim_time[[k]][["no.usedy"]]
    n_months <- sim_time[[k]][["no.usemo"]]

    for (h in seq_along(st2)) {
      for (d in grep("ForEachUsedDay", names(st2[["N"]]), value = TRUE)) {
        info <- paste("For test =", names(input_sim_time)[k], "/ d =", shQuote(d),
          "/ hemisphere =", names(st2)[[h]])
        expect_equal(length(st2[[h]][[d]]), n_days, info = info)
      }
      for (d in grep("ForEachUsedMonth", names(st2[["N"]]), value = TRUE)) {
        info <- paste("For test =", names(input_sim_time)[k], "/ d =", shQuote(d),
          "/ hemisphere =", names(st2)[[h]])
        expect_equal(length(st2[[h]][[d]]), n_months, info = info)
      }
    }
  }
  
})


test_that("Check years", {
  expect_silent(x <- update_requested_years(2000, 2010, 1950, 2010, verbose = FALSE))
  expect_equal(x[["start_year"]], 2000L)
  expect_equal(x[["end_year"]], 2010L)

  expect_output(x <- update_requested_years(1940, 2010, 1950, 2010, verbose = TRUE),
    regexp = "requested start year")
  expect_equal(x[["start_year"]], 1950L)
  expect_equal(x[["end_year"]], 2010L)

  expect_output(x <- update_requested_years(2000, 2020, 1950, 2010, verbose = TRUE),
    regexp = "requested end year")
  expect_equal(x[["start_year"]], 2000L)
  expect_equal(x[["end_year"]], 2010L)
})
