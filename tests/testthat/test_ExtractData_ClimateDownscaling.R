context("Extracting external climate scenario data and downscaling")

#---TESTS
# Test 'convert_temperature'
temp_C <- c(-50, 0, 50)
temp_F <- c(-58, 32, 122)
temp_K <- 273.15 + temp_C
# Test 'convert_precipitation'
dpm <- c(31, 28, 31)
temp_ppt <- c(1.5, 0.3, 0)

test_that("Unit conversion", {
  expect_equal(convert_temperature(temp_C, "C"), temp_C)
  expect_equal(convert_temperature(temp_F, "F"), temp_C, tolerance = 1e-6)
  expect_equal(convert_temperature(temp_K, "K"), temp_C)
  expect_error(convert_temperature(temp_F, "degree F"))
  expect_error(convert_temperature(temp_K, "K", unit_to = "F"))

  expect_equal(convert_precipitation(temp_ppt, dpm, "cm/month"), temp_ppt)
  expect_equal(convert_precipitation(temp_ppt, dpm, "mm/month"), temp_ppt / 10)
  expect_equal(convert_precipitation(temp_ppt, dpm, "mm/d"), temp_ppt * dpm / 10)
  expect_equal(convert_precipitation(temp_ppt, dpm, "cm/d"), temp_ppt * dpm)
  expect_equal(convert_precipitation(temp_ppt, dpm, "kg m-2 s-1"), temp_ppt * dpm * 8640)
  expect_error(convert_precipitation(temp_ppt, dpm, "L m-2"))
  expect_error(convert_precipitation(temp_ppt, dpm, "cm/month", unit_to = "L m-2"))
})

# Test 'get_time_unit'
temp <- c(day = 1, days = 1, d = 1, ds = NA,
  hour = 24, hours = 24, HOuR = 24, h = 24,
  minute = 1440, min = 1440, mins = 1440,
  second = 86400, s = 86400, S = 86400,
  fail = NA, year = NA)

test_that("Time units", {
  for (k in seq_along(temp)) {
    tunit <- names(temp)[k]
    exp_res <- temp[k]

    if (is.na(exp_res)) {
      expect_error(get_time_unit(tunit))
    } else {
      expect_equivalent(get_time_unit(tunit), exp_res, info = tunit)
    }
  }
})


# Test 'climscen_metadata'
req_metadata_fields1 <- c("bbox", "tbox", "var_desc", "sep_fname", "str_fname")
req_metadata_fields2 <- c("tag", "fileVarTags", "unit_given", "unit_real")

test_that("Check integrity of 'climscen_metadata'", {
  expect_silent(climDB_metas <- climscen_metadata())

  expect_type(climDB_metas, "list")
  expect_named(climDB_metas)

  for (k in seq_along(climDB_metas)) {
    expect_type(climDB_metas[[k]], "list")
    expect_true(all(req_metadata_fields1 %in% names(climDB_metas[[k]])))
    expect_named(climDB_metas[[k]][["var_desc"]], expected = req_metadata_fields2)
  }
})


# Test 'fill_bounding_box'
template_bbox <- data.frame(matrix(NA, nrow = 2, ncol = 2,
  dimnames = list(NULL, c("lat", "lon"))))
template_tbox <- data.frame(matrix(NA, nrow = 2, ncol = 2,
  dimnames = list(c("start", "end"), c("first", "second"))))
climDB_metas <- climscen_metadata()

test_that("Check 'fill_bounding_box'", {
  expect_silent(bbox <- fill_bounding_box(template_bbox,
    list(y = c(-90, 90), x = c(-180-0.25, 180+0.25))))
  expect_equal(bbox, climDB_metas[["CMIP5_ESGF_Global"]][["bbox"]])

  expect_silent(tbox <- fill_bounding_box(template_tbox,
    list(t1 = c(1950, 2005), t2 = c(2006, 2100))))
  expect_equal(tbox, climDB_metas[["CMIP5_ESGF_Global"]][["tbox"]])
})


# Test 'calc_timeSlices', 'unique_times', 'calc_getYears', 'useSlices', and
# 'calc_assocYears'
reqRCPs <- c("Current", "RCP45", "RCP85")
tbox <- fill_bounding_box(template_tbox, list(t1 = c(1950, 2005), t2 = c(2006, 2100)))
sim_time <- list(
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
sim_time <- setup_simulation_time(sim_time, add_st2 = TRUE, adjust_NS = TRUE)
names_sim_time <- c("Run", "Slice", "Time", "Year")
names_getYears <- c("n_first", "first", "n_second", "second", "first_dates",
  "second_dates", "first_dpm", "second_dpm")
names_assocYears <- c("historical", "d40yrs.Current", "d90yrs.Current", "d40yrs.RCP45",
"d90yrs.RCP45", "d40yrs.RCP85", "d90yrs.RCP85")


test_that("Check 'climate scenario simulation time slices'", {
  # Check 'calc_timeSlices'
  expect_silent(timeSlices <- calc_timeSlices(sim_time = sim_time, tbox = tbox))
  expect_named(timeSlices, names_sim_time)
  expect_equivalent(as.character(unique(timeSlices[, "Run"])),
    c("historical", "d40yrs", "d90yrs"))
  expect_equal(as.character(unique(timeSlices[, "Slice"])), c("first", "second"))
  expect_equal(as.character(unique(timeSlices[, "Time"])), c("start", "end"))
  expect_equal(nrow(timeSlices), 4L * length(unique(timeSlices[, "Run"])))

  # Check 'unique_times'
  expect_silent(slice1 <- unique_times(timeSlices, slice = "first"))
  expect_equal(as.vector(slice1), c(1980, 2005))
  expect_silent(slice2 <- unique_times(timeSlices, slice = "second"))
  expect_equal(slice2, structure(c(2006L, 2020L, 2070L, 2010L, 2050L, 2099L),
    .Dim = c(3L, 2L)))

  # Check 'calc_getYears'
  expect_silent(getYears <- calc_getYears(timeSlices))
  expect_named(getYears, names_getYears)

  # Check 'useSlices'
  expect_true(useSlices(getYears, timeSlices, run = "historical", slice = "first"))
  expect_false(useSlices(getYears, timeSlices, run = "d40yrs", slice = "first"))
  expect_false(useSlices(getYears, timeSlices, run = "d90yrs", slice = "first"))
  expect_equal(useSlices(getYears, timeSlices, run = "historical", slice = "second"),
    c(TRUE, FALSE, FALSE))
  expect_equal(useSlices(getYears, timeSlices, run = "d40yrs", slice = "second"),
    c(FALSE, TRUE, FALSE))
  expect_equal(useSlices(getYears, timeSlices, run = "d90yrs", slice = "second"),
    c(FALSE, FALSE, TRUE))

  # Check 'calc_assocYears'
  expect_silent(assocYears <- calc_assocYears(sim_time = sim_time, reqRCPs,
    getYears, timeSlices))
  expect_named(assocYears, names_assocYears)
  expect_type(assocYears, "list")
  expect_type(unlist(assocYears), "logical")
  expect_equal(length(unlist(assocYears)), 28L)
  expect_equal(sum(unlist(assocYears)), 8L)
})


# Test 'is_ClimateForecastConvention' and 'is_NEX'
test_that("Check convenction of requested climate data'", {
  expect_false(is_ClimateForecastConvention("CMIP3_ClimateWizardEnsembles_Global"))
  expect_false(is_NEX("CMIP3_ClimateWizardEnsembles_Global"))
  expect_false(is_ClimateForecastConvention("CMIP3_ClimateWizardEnsembles_USA"))
  expect_false(is_NEX("CMIP3_ClimateWizardEnsembles_USA"))
  expect_true(is_ClimateForecastConvention("CMIP5_BCSD_GDODCPUCLLNL_USA"))
  expect_false(is_NEX("CMIP5_BCSD_GDODCPUCLLNL_USA"))
  expect_true(is_ClimateForecastConvention("CMIP5_BCSD_GDODCPUCLLNL_Global"))
  expect_false(is_NEX("CMIP5_BCSD_GDODCPUCLLNL_Global"))
  expect_false(is_ClimateForecastConvention("CMIP5_BCSD_NEX_USA"))
  expect_true(is_NEX("CMIP5_BCSD_NEX_USA"))
  expect_true(is_ClimateForecastConvention("CMIP5_BCSD_SageSeer_USA"))
  expect_false(is_NEX("CMIP5_BCSD_SageSeer_USA"))
  expect_true(is_ClimateForecastConvention("CMIP5_ESGF_Global"))
  expect_false(is_NEX("CMIP5_ESGF_Global"))
})

