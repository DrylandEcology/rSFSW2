context("GISSM: setFALSE_SeedlingSurvival_1stSeason")

# Inputs
test_data <- list(
 test1 = list(
  ss1s = temp <- rep(TRUE, 10), ry_year_day = rep(1, 10),
  ry_useyrs = 1, y = 1, doy = itemp <- 1,
  ref = {ref <- temp; ref[0 + itemp] <- FALSE; ref}),

 test2 = list(
  ss1s = temp <- rep(TRUE, 10), ry_year_day = rep(1, 10),
  ry_useyrs = 1, y = 1, doy = itemp <- 10,
  ref = {ref <- temp; ref[0 + itemp] <- FALSE; ref}),

 test3 = list(
  ss1s = temp <- rep(TRUE, 30), ry_year_day = rep(1:3, each = 10),
  ry_useyrs = 1:3, y = 3, doy = itemp <- 10,
  ref = {ref <- temp; ref[20 + itemp] <- FALSE; ref}),

 test4 = list(
  ss1s = temp <- rep(FALSE, 30), ry_year_day = rep(1:3, each = 10),
  ry_useyrs = 1:3, y = 3, doy = itemp <- 10,
  ref = temp)
)


test_that("setFALSE_SeedlingSurvival_1stSeason", {
  for (k in seq_along(test_data))
    with(test_data[[k]],
      expect_equal(
        setFALSE_SeedlingSurvival_1stSeason(ss1s, ry_year_day, ry_useyrs, y, doy),
        ref,
        info = paste("Test dataset =", shQuote(names(test_data)[k]))))

  #--- Errors
  if (requireNamespace("Rcpp")) {
    expect_error(setFALSE_SeedlingSurvival_1stSeason(rep(TRUE, 7), rep(1, 10), 1, 1, 1))
    expect_error(setFALSE_SeedlingSurvival_1stSeason(rep(TRUE, 10), rep(1, 7), 1, 1, 1))
    expect_error(setFALSE_SeedlingSurvival_1stSeason(rep(TRUE, 10), rep(1, 10), 7, 1, 1))
    expect_error(setFALSE_SeedlingSurvival_1stSeason(rep(TRUE, 10), rep(1, 10), 1, 7, 1))
    expect_error(setFALSE_SeedlingSurvival_1stSeason(rep(TRUE, 10), rep(1, 10), 1, 1, 70))
  }
})
