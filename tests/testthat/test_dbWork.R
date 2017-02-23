context("dbWork: runIDs organization")
# skip_on_cran()

#--- Inputs
#setwd("~/Dropbox (Personal)/Work_Stuff/2_Research/Software/GitHub_Projects/rSFSW2/tests/testthat")
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



# Parallel
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


#--- Unit tests
test_that("dbWork", {
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
})

#--- Clean up
parallel::stopCluster(cl)
unlink(file.path(dbpath, "dbWork.sqlite3"))
unlink(flock, recursive = TRUE)
#unlink(fname_log)
