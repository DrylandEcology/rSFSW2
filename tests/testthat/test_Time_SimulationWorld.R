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

doy_ranges <- list(
  yearlyPPT = c(1, 250),
  periodicVWCmatric = c(1, 250),
  default = c(1, 250), #default doy_range aggregation period
  #water-years calcs - N & S option for each
  dailySnowpack_N = c(1, 250),
  dailySnowpack_S = c(200, 350),
  dailyFrostinSnowPeriod_N = c(1, 250),
  dailyFrostinSnowPeriod_S = c(200, 350),
  # default doy_range water-year aggregation in the N. Hemisphere:
  defaultWateryear_N = c(274, 60),
  # default doy_range water-year aggregation in the S. Hemisphere:
  defaultWateryear_S = c(92, 213)
)


req_simtime_elems <- c("simstartyr", "startyr", "endyr",
  "DScur_startyr", "DScur_endyr", "future_yrs", "spinup_N",
  "useyrs", "no.useyr", "no.usemo", "no.usedy", "index.useyr", "index.usemo",
  "index.usedy")

req_simtime_elems_expanded <- c(req_simtime_elems,
  "overall_simstartyr", "overall_endyr")

# Corrupted input: will produce error
input_sim_timeE <- input_sim_time[[1]]
input_sim_timeE[["future_yrs"]] <- list(
    c(d <- 40, startyr + d, endyr + d),
    c(d <- 90, startyr + d)
  )

#--- TESTS
test_that("Obtain time information", {

  # Setup simulation time
  expect_error(setup_time_simulation_project(input_sim_timeE),
    regexp = "incorrect format of 'future_yrs'")

  sim_time <- list()
  for (k in seq_along(input_sim_time)) {
    info <- names(input_sim_time)[k]
    expect_silent(sim_time[[k]] <- setup_time_simulation_project(
      input_sim_time[[k]], use_doy_range = TRUE, doy_ranges = doy_ranges,
      add_st2 = TRUE, adjust_NS = TRUE))

    N_names <- names(doy_ranges)[!grepl("_S", names(doy_ranges))]
    S_names <- names(doy_ranges)[!grepl("_N", names(doy_ranges))]

    # test if doy_range names were created when use_doy_range = TRUE
    expect_true(length(N_names) ==
      length(names(sim_time[[k]]$sim_time2_North)[
        grep(paste(N_names, collapse = "|"),
          names(sim_time[[k]]$sim_time2_North))]))

    expect_true(length(S_names) ==
      length(names(sim_time[[k]]$sim_time2_North)[
        grep(paste(S_names, collapse = "|"),
          names(sim_time[[k]]$sim_time2_South))]))

    expect_equal(sim_time[[k]][["useyrs"]],
      sim_time[[k]][["startyr"]]:sim_time[[k]][["endyr"]], info = info)
    expect_true(all(req_simtime_elems %in% names(sim_time[[k]])),
      info = info)

    expect_silent(sim_time[[k]] <- setup_time_simulation_project(
      input_sim_time[[k]], use_doy_range = FALSE, doy_ranges = doy_ranges,
      add_st2 = TRUE, adjust_NS = TRUE))

    # test if doy_range names were NOT created when use_doy_range = FALSE
    expect_true(length(N_names) !=
      length(names(sim_time[[k]]$sim_time2_North)[
        grep(paste(N_names, collapse = "|"),
          names(sim_time[[k]]$sim_time2_North))]))

    # Overall span of simulation period
    expect_silent(sim_time[[k]] <- get_simulation_time(sim_time[[k]],
      SFSW2_prj_inputs))
    expect_equal(sim_time[[k]][["overall_simstartyr"]],
      sim_time[[k]][["simstartyr"]])
    expect_equal(sim_time[[k]][["overall_endyr"]], sim_time[[k]][["endyr"]])
  }
})
