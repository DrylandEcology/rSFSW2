context("dbWork: runIDs organization")

#--- Inputs
#setwd("~/Dropbox (Personal)/Work_Stuff/2_Research/Software/GitHub_Projects/SoilWat_R_Wrapper/tests/testthat")
dbpath <- tempdir()
flock_db <- tempfile()
runIDs <- seq_len(100)
#fname_log <- "log_dbWork.txt"

test_update <- function(i, dbpath, flock = NULL) {
  is_inwork <- dbWork_update_job(dbpath, i, "inwork", with_filelock = flock)
  if (is_inwork) {
    todos <- dbWork_todos(dbpath)
    if (any(i == todos)) {
      dbWork_update_job(dbpath, i, "failed", with_filelock = flock)
    } else {
      dbWork_update_job(dbpath, i, "completed", time_s = .node_id,
        with_filelock = flock)
    }
  } else {
    FALSE
  }
}

pretend_sim <- function(cl, runIDs, dbpath, flock) {
  temp <- parallel::clusterApplyLB(cl, runIDs, test_update, dbpath = dbpath, flock = flock)

  # ELSE IF (not error)
  res <- dbWork_todos(dbpath)
  if (length(res) > 0) {
    stop("not all runs completed or some failed")
  } else {
    "no error"
  }
}


# Parallel
temp <- max(2L, min(10L, parallel::detectCores() - 2L))
ncores <- if (is.finite(temp)) temp else 2L
cl <- parallel::makePSOCKcluster(ncores) #, outfile = fname_log)
.node_id <- 0L
parallel::clusterApply(cl, seq_len(ncores),
  function(i) assign(".node_id", i, envir = .GlobalEnv))
parallel::clusterSetRNGStream(cl, iseed = 127)
#TODO: replace next two lines when SWSF has become a package
parallel::clusterExport(cl, varlist = c("create_dbWork", "setup_dbWork", "dbWork_todos",
  "dbWork_timing", "dbWork_update_job", "lock_access", "unlock_access", "lock_attempt"))
parallel::clusterEvalQ(cl, require("RSQLite"))


#--- Unit tests
test_that("dbWork", {
  # Init
  expect_true(setup_dbWork(dbpath, runIDs))
  expect_identical(dbWork_todos(dbpath), runIDs)

  #--- Error due to locked database
  # This should fail with
    #Error in checkForRemoteErrors(val) :
    #  100 nodes produced errors; first error: rsqlite_query_send: could not execute1:
    #  database is locked
  expect_error(pretend_sim(cl, runIDs, dbpath, flock = NULL))

  #--- No error expected because dbWork run with file locking
  # Init
  expect_true(setup_dbWork(dbpath, runIDs))
  expect_identical(dbWork_todos(dbpath), runIDs)
  expect_identical(pretend_sim(cl, runIDs, dbpath, flock = flock_db),
    "no error")

})

#--- Clean up
parallel::stopCluster(cl)
unlink(dbpath, "dbWork.sqlite3"))
unlink(flock_db)
#unlink(fname_log)
