context("Gathering of CO2 ppm data")

# Inputs
LookupCarbonScenarios <- file.path("..", "test_data", "Test4_AllOverallAggregations_snow", "1_Data_SWInput", 
                                  "treatments", "LookupCarbonScenarios", "LookupCarbonScenarios.csv")

# Tests
test_that("LookupCarbonScenario: read_csv", {

  # Check that the CSV could be read in
  ppm_data <- SFSW2_read_csv(LookupCarbonScenarios)
  expect_is(ppm_data, 'data.frame')
  
  # Check the columns...
  # 1) The first column must be "Year"
  expect_equal(colnames(ppm_data)[1], "Year")
  # 2) We need at least one scenario
  expect_gt(ncol(ppm_data), 1)
  # 3) The years must be integers, because SOILWAT2 expects it as such
  expect_is(ppm_data$Year, 'integer')
  # 4) The ppm data can be of type integer or double, because the values are
  # eventually coerced to doubles
  acceptable_data_types <- c('integer', 'double')
  for (i in 1:ncol(ppm_data))
  {
    if (i > 1) expect_true(typeof(ppm_data[, i]) %in% acceptable_data_types)
    # 5) Check all columns for NA values
    expect_false(any(is.na(ppm_data[, i])))
  }
})
