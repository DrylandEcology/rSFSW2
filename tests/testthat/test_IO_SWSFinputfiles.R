context("Input/Output of rSFSW2 datafiles")

#---INPUTS
fname <- file.path("..", "test_data", "SWRuns_InputData_test.csv")

ftemp <- file.path("..", "test_data", "SWRuns_InputData_test1.rds")
if (!file.exists(ftemp)) {
  ref1 <- SFSW2_read_csv(fname)
  saveRDS(ref1, file = ftemp)
} else {
  ref1 <- readRDS(ftemp)
}

ftemp <- file.path("..", "test_data", "SWRuns_InputData_test2.rds")
if (!file.exists(ftemp)) {
  ref2 <- SFSW2_read_inputfile(fname, nrowsClasses = 10)
  saveRDS(ref2, file = ftemp)
} else {
  ref2 <- readRDS(ftemp)
}


#---TESTS
test_that("Read csv-file", {
  #--- With/without iotools
  expect_equal(SFSW2_read_csv(fname, iotools = TRUE), ref1)
  expect_equal(SFSW2_read_csv(fname, iotools = FALSE), ref1)
})

test_that("Read 'rSFSW2-inputfile'", {
  #--- With/without iotools
  expect_equal(SFSW2_read_inputfile(fname, iotools = TRUE), ref2)
  expect_equal(SFSW2_read_inputfile(fname, iotools = FALSE), ref2)
})

test_that("Additional arguments", {
  #--- Argument only applicable to 'iotools'
  expect_equal(SFSW2_read_inputfile(fname, iotools = TRUE,
    nrowsClasses = 10), ref2)
  expect_equal(SFSW2_read_inputfile(fname, iotools = FALSE,
    nrowsClasses = 10), ref2)

  #--- Skip all but one line
  expect_equivalent(SFSW2_read_csv(fname, iotools = TRUE,
    skip = 7), ref1[8, ])
  expect_equivalent(SFSW2_read_csv(fname, iotools = FALSE,
    skip = 7), ref1[8, ])

  #--- Non-existing argument
  expect_equal(SFSW2_read_inputfile(fname, iotools = TRUE,
    fake_argument = 10), ref2)
  expect_equal(SFSW2_read_inputfile(fname, iotools = FALSE,
    fake_argument = 10), ref2)
})

test_that("Reconstitute 'rSFSW2-inputfile'", {
  expect_equal(reconstitute_inputfile(ref2[["use"]], ref2[["data"]]), ref1)
})
