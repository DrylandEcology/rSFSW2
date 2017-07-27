context("Extracting external climate scenario data and downscaling")

#---TESTS
# Test 'convert_temperature'
temp_C <- c(-50, 0, 50)
temp_F <- c(-58, 32, 122)
temp_K <- 273.15 + temp_C

test_that("Unit conversion", {
  expect_equal(convert_temperature(temp_C, "C"), temp_C)
  expect_equal(convert_temperature(temp_F, "F"), temp_C, tolerance = 1e-6)
  expect_equal(convert_temperature(temp_K, "K"), temp_C)
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

