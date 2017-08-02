context("dbWork: runIDs organization")

#--- Inputs
dbpath <- tempdir()
flock <- tempfile(pattern = "rSFSW2lock", tmpdir = normalizePath(dbpath))
runsN_master <- 25L
include_YN <- rep(TRUE, runsN_master)
include_YN[c(1, 10, 24:25)] <- FALSE
expN <- 4L
runsN_total <- runsN_master * expN
runIDs_total <- seq_len(runsN_total)
runIDs <- runIDs_total[rep(include_YN, times = expN)]
sim_size <- list(runsN_master = runsN_master, runsN_total = runsN_total, expN = expN)
time_set3 <- c(50, 75, 125)
#fname_log <- "log_dbWork.txt"
verbose <- FALSE

test_update <- function(i, dbpath, flock = NULL, verbose) {
  is_inwork <- dbWork_update_job(dbpath, i, "inwork", with_filelock = flock,
    verbose = verbose)

  if (is_inwork) {
    todos <- dbWork_todos(dbpath)

    if (any(i == todos)) {
      dbWork_update_job(dbpath, i, "failed", with_filelock = flock,
        verbose = verbose)

    } else {
      dbWork_update_job(dbpath, i, "completed", time_s = .node_id,
        with_filelock = flock, verbose = verbose)
    }

  } else {
    FALSE
  }
}

expect_dbWork_check <- function(x, len, sum) {
  expect_s3_class(x, "data.frame")
  expect_identical(dim(x), c(len, 3L))
  expect_identical(colnames(x), c("completed", "failed", "inwork"))
  if (prod(dim(x)) > 0)
    expect_equal(sum(x), sum)
  invisible()
}


#--- Unit tests
test_that("dbWork: mock simulation in parallel", {
  # Skip these tests because parallel code will likely fail on CIs and on CRAN
  #  - travis  on July 21, 2017: "Error in .check_ncores(length(names)) : 10 simultaneous
  #    processes spawned"
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  
  # Parallel setup
  pretend_sim <- function(cl, runIDs, dbpath, flock, verbose) {
    temp <- parallel::clusterApplyLB(cl, runIDs, test_update,
      dbpath = dbpath, flock = flock, verbose = verbose)
  
    # ELSE IF (not error)
    times <- dbWork_timing(dbpath)
    if (!(length(times) == length(runIDs))) {
      stop("not all runs completed or some failed")
    } else {
      table(times)
    }
  }
  temp <- max(2L, min(10L, parallel::detectCores() - 2L))
  ncores <- if (is.finite(temp)) temp else 2L
  #cl <- parallel::makePSOCKcluster(ncores, outfile = fname_log)
  cl <- parallel::makePSOCKcluster(ncores)
  .node_id <- 0L
  parallel::clusterApply(cl, seq_len(ncores),
    function(i) assign(".node_id", i, envir = globalenv()))
  parallel::clusterSetRNGStream(cl, iseed = 127)
  parallel::clusterExport(cl, varlist = c("create_dbWork", "setup_dbWork", "dbWork_todos",
    "dbWork_timing", "dbWork_update_job", "lock_access", "unlock_access", "lock_attempt",
    "lock_init", "check_lock_content", "remove_lock"))
  # End parallel setup

  # Init
  unlink(flock, recursive = TRUE)
  expect_true(setup_dbWork(dbpath, sim_size, include_YN))
  expect_identical(dbWork_todos(dbpath), runIDs)

  #--- Error due to locked database
  # This should fail with
    #Error in checkForRemoteErrors(val) :
    #  100 nodes produced errors; first error: rsqlite_query_send: could not execute1:
    #  database is locked
  expect_error(pretend_sim(cl, runIDs, dbpath, flock = NULL, verbose))

  #--- No error expected because dbWork is run with file locking
  # Init
  expect_true(setup_dbWork(dbpath, sim_size, include_YN))
  expect_identical(dbWork_todos(dbpath), runIDs)
  expect_s3_class(pretend_sim(cl, runIDs, dbpath, flock, verbose), "table")

  #--- Clean up
  parallel::stopCluster(cl)
  unlink(file.path(dbpath, "dbWork.sqlite3"))
  unlink(flock, recursive = TRUE)
  #unlink(fname_log)
})


test_that("dbWork: access and manipulation functions", {
  # Init
  expect_true(setup_dbWork(dbpath, sim_size, include_YN))

  # Testing 'dbWork_todos'
  expect_identical(dbWork_todos(dbpath), runIDs)

  # Testing 'dbWork_timing'
  #   - expect length 0 because no runID is completed and timed
  expect_length(dbWork_timing(dbpath), 0)
  #   - set runIDs along 'time_set3' as complete with specified timing
  for (k in seq_along(time_set3)) {
    expect_true(dbWork_update_job(dbpath, runIDs[k], "completed",
      time_s = time_set3[k], verbose = verbose))
    #   - expect timings for the completed runIDs
    expect_identical(dbWork_timing(dbpath), time_set3[seq_len(k)])
  }

  # Testing 'dbWork_check' (part 1 of 2)
  temp <- dbWork_check(dbpath, runIDs = runIDs[seq_along(time_set3)])
  expect_dbWork_check(temp, length(time_set3), length(time_set3))

  # Testing 'dbWork_redo'
  #   - incorrect runIDs arguments doesn't change dbWork
  expect_true(dbWork_redo(dbpath, runIDs = c(NULL, numeric(), -Inf, Inf, NA, NaN, FALSE,
    TRUE, "a", -1, runsN_total + 1)))
  expect_identical(dbWork_todos(dbpath), runIDs[-seq_along(time_set3)])
  #   - attempt to reset runIDs which haven't completed yet
  expect_true(dbWork_redo(dbpath, runIDs = runIDs[-seq_along(time_set3)]))
  expect_identical(dbWork_todos(dbpath), runIDs[-seq_along(time_set3)])
  #   - reset previously set runIDs along 'time_set3'
  expect_true(dbWork_redo(dbpath, runIDs = runIDs[seq_along(time_set3)]))
  expect_identical(dbWork_todos(dbpath), runIDs)

  # Testing 'dbWork_check' (part 2)
  temp <- dbWork_check(dbpath, runIDs = runIDs[seq_along(time_set3)])
  expect_dbWork_check(temp, length(time_set3), 0L)
  #   - incorrect runIDs arguments returns a 0-row data.frame
  temp <- dbWork_check(dbpath, runIDs = c(NULL, numeric(), -Inf, Inf, NA, NaN, FALSE,
    TRUE, "a", -1, runsN_total + 1))
  expect_dbWork_check(temp, 0L, 0L)
})

#--- Clean up
unlink(file.path(dbpath, "dbWork.sqlite3"))

