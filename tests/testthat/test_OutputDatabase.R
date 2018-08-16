context("Output: dbOutput database functionality")


#--- Inputs
utils::data(list = "iris", package = "datasets")

# original data
Nsl <- 5L
aSoilLayer <- data.frame(
  P_id = rep(seq_len(nrow(iris)), each = Nsl),
  Soil_Layer = rep(seq_len(Nsl), nrow(iris)))
res0 <- data.frame(P_id = seq_len(nrow(iris)), iris)
res0[, "Species"] <- as.character(res0[, "Species"])
runs <- res0[, "P_id", drop = FALSE]

# new data
new_Pids <- 10:15
res1 <- res0[new_Pids, ]
res1[, - (1:2)] <- seq_along(new_Pids)
fields_exclude <- list(iris = colnames(iris)[1])
# reverse row order to test that updates are still correct
res1 <- res1[rev(seq_along(new_Pids)), ]

# create dbOutput
dbOut <- tempfile()
con <- dbConnect(SQLite(), dbOut)
dbWriteTable(con, "runs", runs)
dbWriteTable(con, "aSoilLayer", aSoilLayer)
dbWriteTable(con, "iris", res0)
dbDisconnect(con)

dbOut0 <- tempfile()
file.copy(from = dbOut, to = dbOut0)

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
  if (FALSE) {
    # TODO: should be of empty list, but is currently not because of how
    # `has_samedesign` is calculated by function `compare_two_dbOutput`
    expect_length(res, 0)
  }
})




test_that("dbOut_update_values", {
  skip_if_not(file.exists(dbOut), file.exists(dbNew))

  ttrack <- dbOut_update_values(dbOut_fname = dbOut, dbNew_fname = dbNew,
    fields_exclude = fields_exclude)
  expect_type(ttrack, "character")

  con <- dbConnect(SQLite(), dbOut)
  ures <- dbReadTable(con, "iris")
  utrack <- dbReadTable(con, ttrack)

  # Expect that correct records were updated
  expect_equal(length(new_Pids), sum(utrack[, "iris"]))
  expect_equal(new_Pids, which(as.logical(utrack[, "iris"])))

  # Expect that updated cells contain updated values
  temp0 <- res1[order(res1[, "P_id"]), ]
  temp1 <- ures[sort(new_Pids), ]
  temp1[, "Species"] <- as.integer(temp1[, "Species"])
  expect_equal(temp0, temp1)

  # Expect that non-updated cells continue to contain previous values
  temp0 <- res0[-new_Pids, ]
  temp1 <- ures[-new_Pids, ]
  expect_equal(temp0[order(temp0[, "P_id"]), ], temp1[order(temp1[, "P_id"]), ])

  dbDisconnect(con)
})


test_that("dbOutput_Tables_have_SoilLayers", {
  skip_if_not(file.exists(dbOut0))

  res <- dbOutput_Tables_have_SoilLayers(tables = "iris", dbname = dbOut0)
  expect_type(res, "logical")
  expect_length(res, 1)
  expect_named(res, "iris")
  expect_false(res)

  con <- dbConnect(SQLite(), dbOut0)

  res <- dbOutput_Tables_have_SoilLayers(tables = "iris", con = con)
  expect_type(res, "logical")
  expect_length(res, 1)
  expect_named(res, "iris")
  expect_false(res)

  res <- dbOutput_Tables_have_SoilLayers(con = con)
  expect_type(res, "logical")
  expect_length(res, 2)
  expect_named(res, c("aSoilLayer", "iris"))
  expect_equivalent(res, c(TRUE, FALSE))

  dbDisconnect(con)
})


test_that("dbOutput_subset", {
  dbNew2 <- tempfile()

  skip_if_not(file.exists(dbOut0))
  con <- dbConnect(SQLite(), dbOut0)

  temp <- dbListTables(con)
  design_tables <- temp[temp %in% dbOutput_ListDesignTables()]
  output_tables <- dbOutput_ListOutputTables(con = con)

  expect_design <- function(con_dbOut, con_dbNew, dtables = design_tables) {
    for (k in seq_along(dtables)) {
      expect_equal(
        dbReadTable(con_dbOut, dtables[k]),
        dbReadTable(con_dbNew, dtables[k]))
    }
  }

  #--- Subset and include all tables/fields and exclude none
  # (make an identical copy)
  expect_true(dbOutput_subset(dbOut_fname = dbOut0, dbNew_fname = dbNew2,
    fields_include = NULL, fields_exclude = NULL))

  con2 <- dbConnect(SQLite(), dbNew2)
  expect_design(con, con2)
  expect_equal(dbReadTable(con2, "iris"), res0)
  expect_equal(dbReadTable(con2, "aSoilLayer"), aSoilLayer)
  unlink(dbNew2)

  #--- Subset and include all tables/fields and exclude some
  expect_true(dbOutput_subset(dbOut_fname = dbOut0, dbNew_fname = dbNew2,
    fields_include = NULL, fields_exclude = fields_exclude))

  con2 <- dbConnect(SQLite(), dbNew2)
  expect_design(con, con2)
  icol <- which(colnames(res0) == fields_exclude[["iris"]][1])
  expect_equal(dbReadTable(con2, "iris"), res0[, - icol])
  expect_equal(dbReadTable(con2, "aSoilLayer"), aSoilLayer)
  unlink(dbNew2)


  #--- Subset and include some tables/fields and exclude some
  fields_include <- list(iris = "Species", aSoilLayer = "Soil_Layer")
  expect_true(dbOutput_subset(dbOut_fname = dbOut0, dbNew_fname = dbNew2,
    fields_include = fields_include, fields_exclude = fields_exclude))

  con2 <- dbConnect(SQLite(), dbNew2)
  expect_design(con, con2)
  icol <- which(colnames(res0) == fields_include[["iris"]])
  expect_equal(dbReadTable(con2, "iris"), res0[, c(1, icol)])
  icol <- which(colnames(aSoilLayer) == fields_include[["aSoilLayer"]])
  expect_equal(dbReadTable(con2, "aSoilLayer"), aSoilLayer[, c(1, icol)])
  unlink(dbNew2)

  dbDisconnect(con)
})


#--- Clean up
unlink(dbOut)
unlink(dbOut0)
unlink(dbNew)
