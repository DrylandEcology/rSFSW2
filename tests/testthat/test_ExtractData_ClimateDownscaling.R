context("Extracting external climate scenario data and downscaling")

#---INPUTS
temp_C <- c(-50, 0, 50)
temp_F <- c(-58, 32, 122)
temp_K <- 273.15 + temp_C


#---TESTS
expect_that("Unit conversion", {
  expect_equal(convert_temperature(temp_C, "C"), temp_C)
  expect_equal(convert_temperature(temp_F, "F"), temp_C, tolerance = 1e-6)
  expect_equal(convert_temperature(temp_K, "K"), temp_C)
})

