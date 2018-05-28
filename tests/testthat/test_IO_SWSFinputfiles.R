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

f_rds_orig <- tempfile(fileext = ".rds")
temp <- strsplit(basename(f_rds_orig), split = ".", fixed = TRUE)[[1]]
f_rds_backup <- file.path(dirname(f_rds_orig),
  paste0(paste(temp[-length(temp)], collapse = ""), "_backup.",
    temp[length(temp)]))


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


test_that("Save to disk with backup", {
  x1 <- Sys.time()
  expect_true(save_to_rds_with_backup(x1, f_rds_orig))
  expect_equal(readRDS(f_rds_orig), x1)
  x2 <- Sys.time()
  expect_true(save_to_rds_with_backup(x2, f_rds_orig))
  expect_equal(readRDS(f_rds_orig), x2)
  expect_equal(readRDS(f_rds_backup), x1)
})

test_that("rSOILWAT2 default inputs", {
  expect_s4_class(read_SOILWAT2_DefaultInputs(), "swInputData")
})

unlink(f_rds_orig)
unlink(f_rds_backup)
