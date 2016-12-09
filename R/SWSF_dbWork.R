########################
#------ dbWork functions

#' Create a SQLite-database \code{dbWork} to manage runs fo a SWSF simulation project
#'
#' @param dbWork A character string. Path to the folder where the database will be created.
#' @param runIDs An integer vector. Identification numbers of requested runs,
#'  i.e., all (or a subset) of \code{runIDs_total}, see \code{\link{iterators}}.
#' @return Invisibly \code{TRUE}
create_dbWork <- function(dbWork, runIDs) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbWork)
  DBI::dbExecute(con,
    paste("CREATE TABLE work(runID INTEGER PRIMARY KEY,",
    "completed INTEGER NOT NULL, failed INTEGER NOT NULL, inwork INTEGER NOT NULL,",
    "time_s REAL)"))

  jobs <- matrix(0L, nrow = length(runIDs), ncol = 5,
    dimnames = list(NULL, c("runID", "completed", "failed", "inwork", "time_s")))
  jobs[, "runID"] <- sort.int(as.integer(runIDs))

  RSQLite::dbWriteTable(con, "work", append = TRUE, value = as.data.frame(jobs))
  RSQLite::dbDisconnect(con)

  invisible(TRUE)
}

#' Setup or connect to SQLite-database \code{dbWork} to manage runs fo a SWSF simulation
#'  project
#'
#' @inheritParams create_dbWork
#' @param continueAfterAbort A logical value. If \code{TRUE} and \code{dbWork} exists,
#'  then function connects to the existing database. If \code{FALSE}, then a new database
#'  is created (possibly overwriting an existing one).
#' @return A logical value indicating success/failure of setting up/connecting to
#'  \code{dbWork} and initializing with \code{runIDs}.
setup_dbWork <- function(path, runIDs, continueAfterAbort = FALSE) {
  dbWork <- file.path(path, "dbWork.sqlite3")
  success <- create <- FALSE

  if (continueAfterAbort) {
    if (file.exists(dbWork)) {
      con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbWork)
      setup_runIDs <- RSQLite::dbGetQuery(con, "SELECT runID FROM work ORDER BY runID")

      success <- identical(as.integer(setup_runIDs$runID), sort.int(runIDs))

      if (success) {
        # clean-up potentially lingering 'inwork'
        DBI::dbExecute(con, "UPDATE work SET inwork = 0 WHERE inwork > 0")
      }
      RSQLite::dbDisconnect(con)

    } else {
      create <- TRUE
    }

  } else {
    unlink(dbWork)
    create <- TRUE
  }

  if (create) {
    temp <- create_dbWork(dbWork, runIDs)
    success <- !inherits(temp, "try-error")
  }

  success
}


#' Extract identification numbers of runs of a SWSF simulation project which are
#'  uncompleted and not \code{inwork}
#'
#' @inheritParams create_dbWork
#' @return An integer vector of \code{runIDs}.
dbWork_todos <- compiler::cmpfun(function(path) {
  dbWork <- file.path(path, "dbWork.sqlite3")
  stopifnot(file.exists(dbWork))

  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbWork, flags = RSQLite::SQLITE_RO)
  runIDs_todo <- RSQLite::dbGetQuery(con, paste("SELECT runID FROM work",
    "WHERE completed = 0 AND inwork = 0 ORDER BY runID"))
  RSQLite::dbDisconnect(con)

  runIDs_todo$runID
})

#' Extract stored execution times of completed runs of a SWSF simulation project
#'
#' @inheritParams create_dbWork
#' @return A numeric vector of execution time in seconds.
dbWork_timing <- compiler::cmpfun(function(path) {
  dbWork <- file.path(path, "dbWork.sqlite3")
  stopifnot(file.exists(dbWork))

  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbWork, flags = RSQLite::SQLITE_RO)
  times <- RSQLite::dbGetQuery(con, "SELECT time_s FROM work WHERE completed > 0")
  RSQLite::dbDisconnect(con)

  times$time_s
})


#' Update run information of a SWSF simulation project
#'
#' @inheritParams create_dbWork
#' @param runID An integer value. The identification number of the current run,
#'  i.e., a value out of \code{runIDs_total}, see \code{\link{iterators}}.
#' @param status A character string. One of "completed", "failed", "inwork".
#' @param time_s A numeric value. The execution time in seconds; used if \code{status} is one of
#'  "completed" and "failed".
#' @param with_filelock A character string. The file path for locking access to
#'  \code{dbWork} with a file lock, i.e., to provide
#'  synchronization during parallel processing. If \code{NULL}, no file locking is used.
#' @param verbose A logical value. If \code{TRUE}, status messages about file lock and
#'  database access are printed
#'
#' @return A logical value whether the status was successfully updated.
dbWork_update_job <- compiler::cmpfun(function(path, runID, status = c("completed", "failed", "inwork"),
  time_s = "NULL", with_filelock = NULL, verbose = FALSE) {

  status <- match.arg(status)
  dbWork <- file.path(path, "dbWork.sqlite3")
  stopifnot(file.exists(dbWork))

  success <- FALSE
  res <- 0L

  repeat {
    if (verbose)
      print(paste0("'dbWork_update_job': (", runID, "-", status, ") attempt to update"))

    lock <- if (!is.null(with_filelock)) {
        lock_access(with_filelock, runID, verbose)
      } else {
        list(confirmed_access = TRUE)
      }

    con <- try(RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbWork), silent = TRUE)

    if (inherits(con, "DBIConnection")) {
      res <- DBI::dbWithTransaction(con, {
        temp <- if (status == "completed") {
            DBI::dbExecute(con, paste("UPDATE work SET completed = 1, failed = 0,",
              "inwork = 0, time_s =", time_s, "WHERE runID =", runID))

          } else if (status == "failed") {
            DBI::dbExecute(con, paste("UPDATE work SET completed = 0, failed = 1,",
              "inwork = 0, time_s =", time_s, "WHERE runID =", runID))

          } else if (status == "inwork") {
            prev_status <- RSQLite::dbGetQuery(con,
              paste("SELECT inwork FROM work WHERE runID =", runID))$inwork
            if (prev_status == 0) {
              DBI::dbExecute(con, paste("UPDATE work SET completed = 0, failed = 0,",
                "inwork = 1, time_s = 0 WHERE runID =", runID))
            } else {
              if (verbose)
                print(paste("'dbWork_update_job':", runID, "is already in work"))
              0L
            }
          } else {
            0L
          }

        if (!is.null(with_filelock)) {
          lock <- unlock_access(lock)
        }

        if (!lock$confirmed_access) {
          if (verbose)
            print(paste0("'dbWork_update_job': (", runID, "-", status, ") access confirmation failed"))
          temp <- 0L
          DBI::dbBreak()
        } else if (verbose) {
          print(paste0("'dbWork_update_job': (", runID, "-", status, ") transaction confirmed"))
        }

        as.integer(temp)
      })

      RSQLite::dbDisconnect(con)
      success <- lock$confirmed_access
    } else if (verbose) {
      print(paste0("'dbWork_update_job': (", runID, "-", status, ") 'dbWork' is locked"))
    }

    if (success) break
  }

  !is.na(res) && res == 1L
})


#------ End of dbWork functions
########################
