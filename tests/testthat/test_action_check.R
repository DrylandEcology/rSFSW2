context("dbOutput: action: check")

#--- Inputs
test_table <- "test"

init_testDB <- function(test_table. = test_table) {
  dbtest <- tempfile()
  con <- DBI::dbConnect(RSQLite::SQLite(), dbtest)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # A zero row data frame just creates a table definition
  temp <- matrix(NA, nrow = 0, ncol = 2, dimnames = list(NULL,
    c("P_id", "Include_YN")))
  RSQLite::dbWriteTable(con, "header", as.data.frame(temp))

  temp <- matrix(NA, nrow = 0, ncol = 1, dimnames = list(NULL, "P_id"))
  RSQLite::dbWriteTable(con, test_table., as.data.frame(temp))

  dbtest
}

testDB_add_to_header <- function(dbtest, P_id, Include_YN) {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbtest)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  dat <- data.frame(P_id = P_id, Include_YN = Include_YN)

  if ("header" %in% DBI::dbListTables(con)) {
    temp <- RSQLite::dbReadTable(con, "header")
    dat2 <- data.frame(P_id = sort(unique(c(temp$P_id, dat$P_id))))
    dat2[, "Include_YN"] <- NA
    irows <- match(temp$P_id, dat2$P_id, nomatch = 0)
    if (length(irows) > 0)
      dat2[dat2$P_id %in% temp$P_id, "Include_YN"] <- temp[irows, "Include_YN"]
    irows <- match(dat$P_id, dat2$P_id, nomatch = 0)
    if (length(irows) > 0)
      dat2[dat2$P_id %in% dat$P_id, "Include_YN"] <- dat[irows, "Include_YN"]

    temp <- RSQLite::dbWriteTable(con, "header", dat2, overwrite = TRUE)

  } else {
    temp <- RSQLite::dbWriteTable(con, "header", dat)
  }


  invisible(temp)
}

testDB_add_to_testtable <- function(dbtest, P_id, test_table. = test_table) {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbtest)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  dat <- data.frame(P_id = P_id)

  if (test_table. %in% DBI::dbListTables(con)) {
    temp <- RSQLite::dbReadTable(con, test_table.)
    dat2 <- data.frame(P_id = sort(unique(c(temp$P_id, dat$P_id))))

    temp <- RSQLite::dbWriteTable(con, test_table., dat2, overwrite = TRUE)

  } else {
    temp <- RSQLite::dbWriteTable(con, test_table., dat)
  }

  invisible(temp)
}

# Initialization
dbtest1 <- init_testDB()
dbtest2 <- init_testDB()

#--- Unit tests
test_that("missing_Pids_outputDB", {
  #--- Case where outputDB is missing
  expect_identical(missing_Pids_outputDB(test_table, file.path(tempdir(),
    "outputDB_nonexisting")), -1L)

  #--- Case where table is missing
  expect_identical(missing_Pids_outputDB("table_nonexisting", dbtest1), -1L)

  #--- Cases where no Pid is missing
  # no content
  expect_identical(missing_Pids_outputDB(test_table, dbtest1), integer(0))
  expect_identical(missing_Pids_outputDB(test_table, dbtest2), integer(0))

  # records only in test table
  testDB_add_to_testtable(dbtest1, P_id = seq_len(5))
  expect_identical(missing_Pids_outputDB(test_table, dbtest1), integer(0))

  # more records in test table than header
  testDB_add_to_header(dbtest1, P_id = seq_len(4), Include_YN = rep(1, 4))
  expect_identical(missing_Pids_outputDB(test_table, dbtest1), integer(0))

  # complete records
  testDB_add_to_header(dbtest1, P_id = 5, Include_YN = 1)
  expect_identical(missing_Pids_outputDB(test_table, dbtest1), integer(0))

  # missing records in test table, but header indicates not Include_YN
  testDB_add_to_header(dbtest1, P_id = 5, Include_YN = 0)
  expect_identical(missing_Pids_outputDB(test_table, dbtest1), integer(0))

  # no records in test table, but header indicates not Include_YN
  testDB_add_to_header(dbtest2, P_id = seq_len(4), Include_YN = rep(0, 4))
  expect_identical(missing_Pids_outputDB(test_table, dbtest2), integer(0))


  #--- Cases where Pids are missing
  # no records in test table and header indicates Include_YN
  testDB_add_to_header(dbtest2, P_id = seq_len(4), Include_YN = rep(1, 4))
  expect_identical(missing_Pids_outputDB(test_table, dbtest2), seq_len(4))

  # missing records in test table and header indicates Include_YN
  testDB_add_to_testtable(dbtest2, P_id = c(1, 4))
  expect_identical(missing_Pids_outputDB(test_table, dbtest2), c(2L, 3L))

  testDB_add_to_testtable(dbtest2, P_id = 2)
  expect_identical(missing_Pids_outputDB(test_table, dbtest2), 3L)

  # missing records in test table and header indicates Include_YN as well as
  # records in test table which are not in header
  testDB_add_to_testtable(dbtest2, P_id = 6)
  expect_identical(missing_Pids_outputDB(test_table, dbtest2), 3L)
})


# Clean-up
unlink(dbtest1)
unlink(dbtest2)
