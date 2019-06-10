context("Output: dbOutput data access functionality")


#--- Inputs
utils::data(list = "iris", package = "datasets")

#--- Tests
test_that("dbOutput_add_calculated_field:", {
  # Prepare databse
  dbOut_tmp <- tempfile(fileext = ".sqlite")
  con <- dbConnect(SQLite(), dbOut_tmp)
  data(iris)
  x <- data.frame(P_id = seq_len(nrow(iris)), iris)
  dbWriteTable(con, "iris", x)

  # Test 1: 2 variables -> 1 variable, 1 additional parameter
  # Define function
  vars_orig <- c("Sepal.Length", "Sepal.Width")
  example_calc <- function(x, delta = 1, ...) {
    apply(x, MARGIN = 1, function(x) delta * prod(x))
  }

  # Create new field based on a calculation
  dbOutput_add_calculated_field(
    dbOut_fname = dbOut_tmp,
    table = "iris",
    vars_orig = vars_orig,
    vars_new = "calc1",
    FUN = example_calc, delta = 2)

  # Check the new field
  xout <- dbReadTable(con, "iris")
  res2 <- example_calc(x[, vars_orig], delta = 2)
  expect_equivalent(xout[, "calc1"], res2)

  # Cleanup
  dbDisconnect(con)
  unlink(dbOut_tmp)
})
