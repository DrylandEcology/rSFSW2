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
  climDB_metas <- climscen_metadata()

  expect_type(climDB_metas, "list")
  expect_named(climDB_metas)

  for (k in seq_along(climDB_metas)) {
    expect_type(climDB_metas[[k]], "list")
    expect_true(all(req_metadata_fields1 %in% names(climDB_metas[[k]])))
    expect_named(climDB_metas[[k]][["var_desc"]], expected = req_metadata_fields2)
  }
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

