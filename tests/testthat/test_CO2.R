context("Gathering of CO2 ppm data")

# Inputs
LookupCarbonScenarios <- file.path("..", "test_data", "Test4_AllOverallAggregations_snow", "1_Data_SWInput", 
                                  "treatments", "LookupCarbonScenarios", "LookupCarbonScenarios.csv")

# Tests
test_that("LookupCarbonScenario: read_csv", {

  # Check that the CSV could be read in
  ppm_data <- SFSW2_read_csv(LookupCarbonScenarios)
  expect_is(ppm_data, 'data.frame')
  
  # Check that there are enough rows
  expect_equal(length(ppm_data$Year), 2498)
  
  # Check that the columns are of the correct type
  columns_with_ppm_data <- 2:ncol(ppm_data)
  acceptable_data_types <- c('integer', 'double')
  expect_is(ppm_data$Year, 'integer')
  for (i in columns_with_ppm_data)
  {
    expect_true(typeof(ppm_data[, i]) %in% acceptable_data_types)
    expect_false(any(is.na(ppm_data[, i])))  # Check if any NA values exist
  }
})
