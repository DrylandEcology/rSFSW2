context("Time in the simulation world")

#--- INPUTS
input_sim_time <- list(
  simstartyr = 1979,
  startyr = startyr <- 1980,
  endyr = endyr <- 2010,
  DScur_startyr = startyr,
  DScur_endyr = endyr,
  future_yrs = list(
    c(d <- 40, startyr + d, endyr + d),
    c(d <- 90, startyr + d, endyr + d - 1)
  )
)

expected_sim_time_elements <- c("simstartyr", "startyr", "endyr", "DScur_startyr", "DScur_endyr", 
"future_yrs", "spinup_N", "future_N", "useyrs", "no.useyr", "no.usemo", 
"no.usedy", "index.useyr", "index.usemo", "index.usedy")

input_sim_time2 <- input_sim_time
input_sim_time2[["future_yrs"]] <- list(
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
  expect_silent(sim_time <- setup_simulation_time(input_sim_time, 
    add_st2 = TRUE, adjust_NS = TRUE))
  expect_error(setup_simulation_time(input_sim_time2),
    regexp = "incorrect format of 'future_yrs'")
  expect_equal(sim_time[["useyrs"]], sim_time[["startyr"]]:sim_time[["endyr"]])
  expect_true(all(expected_sim_time_elements %in% names(sim_time)))

  # Simulation time aggregation lists
  expect_silent(sim_time2_North <- simTiming_ForEachUsedTimeUnit(sim_time, latitude = 90))
  expect_silent(sim_time2_South <- simTiming_ForEachUsedTimeUnit(sim_time, latitude = -90))
  n_days <- sim_time[["no.usedy"]]
  for (k in grep("ForEachUsedDay", names(sim_time2_North), value = TRUE)) {
    info <- paste("For k =", shQuote(k))
    expect_equal(length(sim_time2_North[[k]]), n_days, info = info)
    expect_equal(length(sim_time2_South[[k]]), n_days, info = info)
  }
  n_months <- sim_time[["no.usemo"]]
  for (k in grep("ForEachUsedMonth", names(sim_time2_North), value = TRUE)) {
    info <- paste("For k =", shQuote(k))
    expect_equal(length(sim_time2_North[[k]]), n_months, info = info)
    expect_equal(length(sim_time2_South[[k]]), n_months, info = info)
  }
})

