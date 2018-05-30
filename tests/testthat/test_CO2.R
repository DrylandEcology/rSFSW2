context("Gathering of CO2 ppm data")

# Inputs
LookupCO2data <- file.path("..", "test_data", "TestPrj4", "1_Input",
  "treatments", "LookupCO2data", "AtmosCO2.csv")

# Tests
test_that("LookupCO2data: read_csv", {

  # Check that the CSV could be read in
  ppm_data <- SFSW2_read_csv(LookupCO2data)
  expect_is(ppm_data, "data.frame")

  # Check the columns...
  # 1) The first column must be "Year"
  expect_equal(colnames(ppm_data)[1], "Year")

  # 2) We need at least one scenario
  expect_gt(ncol(ppm_data), 1)

  # 3) The years must be non-NA integers, because SOILWAT2 expects it as such
  expect_is(ppm_data$Year, "integer")
  expect_false(any(is.na(ppm_data$Year)))

  # 4) The ppm data can be of type integer or double, because the values are
  # eventually coerced to doubles
  acceptable_data_types <- c("integer", "double")
  for (i in 2:ncol(ppm_data)) {
    expect_true(typeof(ppm_data[, i]) %in% acceptable_data_types)
    # 5) Check for empty columns with only NA values
    expect_false(all(is.na(ppm_data[, i])))
  }
})
