context("dbWork: runIDs organization")

#--- Inputs
dbpath <- tempdir()
runIDs <- seq_len(100)

test_update <- function(i, dbpath, with_lock = TRUE) {
  dbWork_update_job(dbpath, i, "inwork", with_lock = with_lock)
  todos <- dbWork_todos(dbpath)
  if (any(i == todos)) {
    dbWork_update_job(dbpath, i, "failed", with_lock = with_lock)
  } else {
    dbWork_update_job(dbpath, i, "completed", time_s = .node_id, with_lock = with_lock)
  }
  invisible()
}

pretend_sim <- function(with_lock) {
  parallel::clusterApplyLB(cl, runIDs, test_update, dbpath = dbpath, with_lock = with_lock)

  # ELSE
  res <- dbWork_todos(dbpath)
  if (length(res) > 0) {
    stop("not all runs completed or some failed")
  } else {
    "no error"
  }
}


# Parallel
temp <- min(4L, parallel::detectCores())
ncores <- if (is.finite(temp)) temp else 2L
cl <- parallel::makePSOCKcluster(ncores)
.node_id <- 0L
parallel::clusterApply(cl, seq_len(ncores),
  function(i) assign(".node_id", i, envir = .GlobalEnv))
parallel::clusterSetRNGStream(cl, iseed = 127)
#TODO: replace next two lines when SWSF has become a package
parallel::clusterExport(cl, varlist = c("create_dbWork", "setup_dbWork", "dbWork_todos",
  "dbWork_timing", "dbWork_update_job"))
parallel::clusterEvalQ(cl, require("RSQLite"))


#--- Unit tests
test_that("dbWork", {
  #--- Error due to locked database
  setup_dbWork(dbpath, runIDs)
  # This will most likely fail with
    #Error in checkForRemoteErrors(val) :
    #  1000 nodes produced errors; first error: rsqlite_query_send: could not execute1:
    #  database is locked
  expect_error(pretend_sim(with_lock = FALSE))

  #--- No error expected because dbWork run with shared locking
  expect_identical(pretend_sim(with_lock = TRUE), "no error")

})

#--- Clean up
parallel::stopCluster(cl)
unlink(file.path(dbpath, "dbWork.sqlite3"))
