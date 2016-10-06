context("GISSM: germination_wait_times")

# Inputs
test_data <- list(
  real_data = list(
    dfc = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 132L, 131L,
    130L, 129L, 128L, 127L, 126L, 125L, 124L, 123L, 122L, 121L, 120L,
    119L, 118L, 117L, 116L, 115L, 114L, NA, 113L, 112L, 111L, 110L,
    109L, 108L, 107L, 106L, 105L, 104L, 103L, 102L, 101L, 100L, NA,
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 99L, 98L, 97L, 96L, 95L,
    94L, 93L, 92L, 91L, 90L, 89L, NA, NA, NA, NA, NA, NA, NA, 88L,
    87L, 86L, 85L, 84L, 83L, 82L, 81L, 80L, 79L, 78L, 77L, 76L, 75L,
    74L, 73L, 72L, 71L, 70L, 69L, 68L, 67L, 66L, 65L, 64L, 63L, 62L,
    61L, 60L, 59L, 58L, 57L, 56L, 55L, 54L, 53L, 52L, 51L, 50L, 49L,
    48L, 47L, 46L, 45L, 44L, 43L, 42L, 41L, 40L, 39L, 38L, 37L, 36L,
    35L, 34L, 33L, 32L, 31L, 30L, 29L, 28L, NA, 27L, 26L, 25L, 24L,
    23L, 22L, 21L, 20L, 19L, 18L, 17L, 16L, 15L, 14L, 13L, 12L, 11L,
    NA, 10L, 9L, NA, NA, NA, NA, 8L, 7L, 6L, NA, NA, NA, NA, NA,
    NA, NA, NA, NA, 5L, NA, NA, NA, 4L, NA, NA, 3L, 2L, 1L, NA, NA,
    NA, NA, NA, NA),

    ttg = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 6L, 6L, 5L,
    5L, 6L, 5L, 4L, 7L, 4L, 5L, 7L, 4L, 9L, 3L, 7L, 6L, 10L, 2L,
    5L, NA, 4L, 3L, 4L, 3L, 2L, 8L, 4L, 4L, 5L, 10L, 4L, 4L, 5L,
    3L, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 7L, 4L, 9L, 5L,
    3L, 5L, 5L, 4L, 8L, 7L, 6L, NA, NA, NA, NA, NA, NA, NA, 5L, 6L,
    5L, 5L, 6L, 7L, 5L, 5L, 4L, 9L, 5L, 4L, 3L, 5L, 4L, 6L, 5L, 3L,
    4L, 6L, 5L, 9L, 6L, 4L, 4L, 4L, 6L, 8L, 4L, 3L, 3L, 7L, 5L, 5L,
    5L, 4L, 5L, 3L, 3L, 4L, 6L, 5L, 6L, 2L, 7L, 7L, 5L, 6L, 4L, 5L,
    4L, 12L, 6L, 6L, 5L, 3L, 2L, 6L, 4L, 6L, 4L, NA, 11L, 7L, 7L,
    7L, 5L, 7L, 6L, 5L, 4L, 4L, 5L, 8L, 8L, 6L, 4L, 4L, 9L, NA, 4L,
    2L, NA, NA, NA, NA, 6L, 5L, 4L, NA, NA, NA, NA, NA, NA, NA, NA,
    NA, 5L, NA, NA, NA, 4L, NA, NA, 3L, 2L, 1L, NA, NA, NA, NA, NA,
    NA),

    ref = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 1L,
    1L, 1L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 27L, 0L,
    27L, 27L, 27L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 7L, 7L, 7L, 0L,
    0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
    0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
    0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
    0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L,
    0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 5L, 1L, 1L, 1L, 19L, 4L, 4L,
    14L, 14L, 14L, 5L, 2L, 0L, 0L, 0L)
  ),

  test1 = list(ttg = NA, dfc = NA, ref = integer(0)),
  test2 = list(ttg = 1, dfc = 1, ref = 0L),
  test3 = list(ttg = rep(1, 10), dfc = 10:1, ref = rep(0L, 10)),
  test4 = list(ttg = temp <- c(2, NA, 1), dfc = temp, ref = c(1L, 0L)),
  test5 = list(ttg = temp <- c(2, rep(NA, 10), 1), dfc = temp, ref = c(10L, 0L)),
  test6 = list(ttg = temp <- c(3, NA, NA, 2, NA, 1), dfc = temp, ref = c(3L, 1L, 0L)),
  test7 = list(ttg = c(3, 3, 3, NA, NA, 2, NA, 1),
              dfc = c(5, 4, 3, NA, NA, 2, NA, 1), ref = c(0L, 2L, 3L, 1L, 0L)),
  test8 = list(ttg = c(NA, 8, 1, 2, 1, NA, NA, NA, 3, 3, 3, NA, NA, 2, NA, 1),
              dfc = c(NA, 9, 8, 7, 6, NA, NA, NA, 5, 4, 3, NA, NA, 2, NA, 1),
              ref = c(5L, 0L, 0L, 0L, 0L, 2L, 3L, 1L, 0L))
)



test_that("germination_wait_times", {
  for (k in seq_along(test_data))
    with(test_data[[k]], expect_equal(as.integer(germination_wait_times(ttg, dfc)), ref,
      info = paste("Test dataset =", shQuote(names(test_data)[k]))))

  if (FALSE) {
    for (k in seq_along(test_data)[-1]) {
      print(paste("Test =", k, "with dataset =", shQuote(names(test_data)[k])))
      print(paste("ttg =", paste(test_data[[k]][["ttg"]], collapse = ", ")))
      print(paste("dfc =", paste(test_data[[k]][["dfc"]], collapse = ", ")))
      print(paste("ref =", paste(test_data[[k]][["ref"]], collapse = ", ")))
      out <- as.integer(germination_wait_times(test_data[[k]][["ttg"]], test_data[[k]][["dfc"]]))
      print(paste("out =", paste(out, collapse = ", ")))
      print("")
    }
}

  #--- Errors
  # time_to_germinate is not NA, but duration_fave_cond is NA
  expect_error(germination_wait_times(1, NA))
  # germination takes longer than available favorable condition
  expect_error(germination_wait_times(2, 1))
  expect_error(germination_wait_times(c(3, NA, 1), c(2, NA, 1)))
  # arguments not of identical length
  expect_error(germination_wait_times(rep(1, 10), 8:1))
})
