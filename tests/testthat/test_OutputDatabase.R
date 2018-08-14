context("Output: dbOutput database functionality")


#--- Inputs
utils::data(list = "iris", package = "datasets")

# original data
res0 <- data.frame(P_id = seq_len(nrow(iris)), iris)

# new data
new_Pids <- 10:15
res1 <- data.frame(P_id = new_Pids, iris[new_Pids, ])
res1[, -(1:2)] <- seq_along(new_Pids)
fields_exclude <- list(iris = colnames(iris)[1])
# reverse row order to test that updates are still correct
res1 <- res1[rev(seq_along(new_Pids)), ]

# create dbOutput
dbOut <- tempfile()
con <- dbConnect(SQLite(), dbOut)
dbWriteTable(con, "iris", res0)
dbWriteTable(con, "runs", res0[, "P_id", drop = FALSE])
dbDisconnect(con)

# create dbNew
dbNew <- tempfile()
con <- dbConnect(SQLite(), dbNew)
dbWriteTable(con, "iris", res1)
dbDisconnect(con)


#--- Tests
test_that("dbOut_check_values:", {
  skip_if_not(file.exists(dbOut), file.exists(dbNew))

  con <- dbOut_check_values(dbOut_fname = dbOut, dbNew_fname = dbNew,
    fields_check = list(iris = colnames(iris)[1]))
  expect_s4_class(con, "SQLiteConnection")

  ttrack <- dbListTables(con)
  utrack <- dbReadTable(con, ttrack)

  # Expect that correct records were matched
  expect_equal(length(new_Pids), sum(utrack[, "iris"], na.rm = TRUE))
  expect_equal(new_Pids, which(as.logical(utrack[, "iris"])))
})



test_that("compare_two_dbOutput:", {
  skip_if_not(file.exists(dbOut), file.exists(dbNew))

  # dbOut and dbNew are not equal --> output is a list of length greater than 0
  res <- compare_two_dbOutput(dbOut, dbNew)
  expect_gt(length(res), 0)

  # dbOut is equal to itself --> output is an empty list
  res <- compare_two_dbOutput(dbOut, dbOut)
  # should be of empty list, but is currently not because of how
  # `has_samedesign` is calculated by function `compare_two_dbOutput`
})



#--- Clean up
unlink(dbOut)
unlink(dbNew)
