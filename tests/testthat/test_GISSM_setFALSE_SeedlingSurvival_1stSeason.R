context("GISSM: kill_seedling")

# Inputs
calc_ref <- function(ss1s, offset, doy) {
  ref <- ss1s
  ref[offset + doy] <- FALSE
  ref
}

test_data <- list(
 test1 = list(
  ss1s = temp <- rep(TRUE, 10), ry_year_day = rep(1, 10),
  ry_useyrs = 1, y = 1, doy = itemp <- 1,
  ref = calc_ref(temp, 0, itemp)),

 test2 = list(
  ss1s = temp <- rep(TRUE, 10), ry_year_day = rep(1, 10),
  ry_useyrs = 1, y = 1, doy = itemp <- 10,
  ref = calc_ref(temp, 0, itemp)),

 test3 = list(
  ss1s = temp <- rep(TRUE, 30), ry_year_day = rep(1:3, each = 10),
  ry_useyrs = 1:3, y = 3, doy = itemp <- 10,
  ref = calc_ref(temp, 20, itemp)),

 test4 = list(
  ss1s = temp <- rep(FALSE, 30), ry_year_day = rep(1:3, each = 10),
  ry_useyrs = 1:3, y = 3, doy = itemp <- 10,
  ref = temp)
)



test_that("kill_seedling", {

  for (k in seq_along(test_data))
    with(test_data[[k]],
      expect_equal(
        kill_seedling(ss1s, ry_year_day, ry_useyrs, y, doy),
        ref,
        info = paste("Test dataset =", shQuote(names(test_data)[k]))))

  #--- Errors
  if (requireNamespace("Rcpp")) {
    expect_error(kill_seedling(rep(TRUE, 7), rep(1, 10), 1, 1, 1))
    expect_error(kill_seedling(rep(TRUE, 10), rep(1, 7), 1, 1, 1))
    expect_error(kill_seedling(rep(TRUE, 10), rep(1, 10), 7, 1, 1))
    expect_error(kill_seedling(rep(TRUE, 10), rep(1, 10), 1, 7, 1))
    expect_error(kill_seedling(rep(TRUE, 10), rep(1, 10), 1, 1, 70))
  }
})
