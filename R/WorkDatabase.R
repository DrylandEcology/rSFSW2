########################
#------ dbWork functions

add_dbWork_index <- function(con) {
  stopifnot(DBI::dbIsValid(con))

  prev_indices <- DBI::dbGetQuery(con, "SELECT * FROM sqlite_master WHERE type = 'index'")

  if (NROW(prev_indices) == 0L || !("i_runIDs" %in% prev_indices[, "name"])) {
    DBI::dbExecute(con, paste("CREATE INDEX i_runIDs ON work (runID_total, runID_sites,",
      "include_YN)"))
  }
}


fname_dbWork <- function(path, dbname = "dbWork.sqlite3") {
  if (grepl(".sql", basename(path))) {
    path <- dirname(path)
  }

  file.path(path, dbname)
}


#' Create a SQLite-database \code{dbWork} to manage runs fo a rSFSW2 simulation project
#'
#' @param path A character string. Path to the folder where the database will be created.
#' @param jobs An integer matrix. Each row corresponds to one call of the simulation
#'  function \code{do_OneSite}, i.e., \code{runsN_master} x \code{expN}. The columns
#'  \code{runID_total}, \code{runID_sites}, \code{include_YN} represent a running ID,
#'  the site_id (row number in master input file), and a flag whether site is being
#'  simulated or not. See \code{\link{indices}}.
#' @return Invisibly \code{TRUE}
#' @export
create_dbWork <- function(path, jobs) {

  stopifnot(colnames_job_df() %in% dimnames(jobs)[[2]])
  dbWork <- fname_dbWork(path)

  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbWork, flags = RSQLite::SQLITE_RWC)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(con,
    paste("CREATE TABLE work(runID_total INTEGER PRIMARY KEY,",
    "runID_sites INTEGER NOT NULL, include_YN INTEGER NOT NULL,",
    "completed INTEGER NOT NULL, failed INTEGER NOT NULL, inwork INTEGER NOT NULL,",
    "time_s REAL)"))

  RSQLite::dbWriteTable(con, "work", append = TRUE, value = as.data.frame(jobs))
  add_dbWork_index(con)

  # Set WAL-mode (http://www.sqlite.org/wal.html)
  temp_jmode <- DBI::dbGetQuery(con, "PRAGMA journal_mode=WAL;")
  if (!(tolower(as.character(temp_jmode)) == "wal")) {
    print(paste("'create_dbWork': setting WAL mode for dbWork failed; journal mode is",
      "instead", shQuote(temp_jmode)))
  }

  # Set synchronous mode to OFF=0 (https://www.sqlite.org/pragma.html#pragma_synchronous)
  # Fast but data may be lost if power fails
  # DBI::dbExecute(con, "PRAGMA synchronous=OFF;")
  # no need to set this PRAGMA, because `RSQLite::dbConnect` does it by default

  # Turn off busy timeout (https://www.sqlite.org/pragma.html#pragma_busy_timeout)
  # temp_smode <- DBI::dbGetQuery(con, "PRAGMA busy_timeout=0;")
  # no need to set this PRAGMA, because `RSQLite::dbConnect` does it by default


  invisible(TRUE)
}

colnames_job_df <- function() {
  c("runID_total", "runID_sites", "include_YN", "completed", "failed", "inwork", "time_s")
}

create_job_df <- function(sim_size, include_YN) {
  temp <- colnames_job_df()
  jobs <- matrix(data = 0L, nrow = sim_size[["runsN_total"]], ncol = length(temp),
    dimnames = list(NULL, temp))

  jobs[, "runID_total"] <- seq_len(sim_size[["runsN_total"]])
  jobs[, "runID_sites"] <- rep(seq_len(sim_size[["runsN_master"]]),
    times = max(sim_size[["expN"]], 1L))
  temp <- rep(include_YN, times = max(sim_size[["expN"]], 1L))
  jobs[temp, "include_YN"] <- 1L

  jobs
}


#' Setup or connect to SQLite-database \code{dbWork} to manage runs fo a rSFSW2 simulation
#'  project
#'
#' @inheritParams create_dbWork
#' @param resume A logical value. If \code{TRUE} and \code{dbWork} exists,
#'  then function connects to the existing database. If \code{FALSE}, then a new database
#'  is created (possibly overwriting an existing one).
#' @return A logical value indicating success/failure of setting up/connecting to
#'  \code{dbWork} and initializing with \code{runIDs}.
#' @export
setup_dbWork <- function(path, sim_size, include_YN, resume = FALSE) {

  dbWork <- fname_dbWork(path)
  success <- create <- FALSE
  jobs <- create_job_df(sim_size, include_YN)

  if (resume) {
    if (file.exists(dbWork)) {
      con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbWork, flags = RSQLite::SQLITE_RW)
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      if (any(!(dimnames(jobs)[[2]] %in% DBI::dbListFields(con, "work")))) {
        stop("'setup_dbWork': dbWork is misspecified or outdated; you may fix ",
          "this by first calling the function 'recreate_dbWork'")
      }
      prev_work <- DBI::dbGetQuery(con, paste("SELECT runID_total, runID_sites,",
        "include_YN FROM work ORDER BY runID_total"))

      # check whether same design
      success <- all(sapply(c("runID_total", "runID_sites"), function(x)
        identical(prev_work[, x], as.integer(jobs[, x]))))

      if (success) {
        # clean-up potentially lingering 'inwork'
        DBI::dbExecute(con, "UPDATE work SET inwork = 0 WHERE inwork > 0")

        # check whether include_YN has been changed and update if necessary
        if (!identical(prev_work[, "include_YN"], jobs[, "include_YN"])) {
          # now excluded
          iwork <- which(prev_work[, "include_YN"] == 1L & jobs[, "include_YN"] == 0L)
          if (length(iwork) > 0) {
            rs <- DBI::dbSendStatement(con, paste("UPDATE work SET include_YN = 0",
              "WHERE runID_total = :x"))
            DBI::dbBind(rs, param = list(x = iwork))
          }
          # now included
          iwork <- which(prev_work[, "include_YN"] == 0L & jobs[, "include_YN"] == 1L)
          if (length(iwork) > 0) {
            rs <- DBI::dbSendStatement(con, paste("UPDATE work SET include_YN = 1",
              "WHERE runID_total = :x"))
            DBI::dbBind(rs, param = list(x = iwork))
          }

          DBI::dbClearResult(rs)
        }
      }

    } else {
      create <- TRUE
    }

  } else {
    unlink(dbWork)
    create <- TRUE
  }

  if (create) {
    temp <- create_dbWork(path, jobs)
    success <- !inherits(temp, "try-error")
  }

  success
}

#' Initiate a checkpoint operation on a SQLite-database \code{dbWork} of a rSFSW2 simulation project
#' @references https://www.sqlite.org/pragma.html#pragma_wal_checkpoint
dbWork_checkpoint <- function(path = NULL, con = NULL,
  mode = c("PASSIVE", "FULL", "RESTART", "TRUNCATE", ""),
  failure = c("silent", "warning", "error")) {

  mode <- match.arg(mode)
  failure <- match.arg(failure)

  if (is.null(con)) {
    dbWork <- fname_dbWork(path)
    stopifnot(file.exists(dbWork))

    # need write privilege to run wal_checkpoint
    con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbWork, flags = RSQLite::SQLITE_RW)
    on.exit(DBI::dbDisconnect(con), add = TRUE)

  } else {
    stopifnot(DBI::dbIsValid(con))
  }

  sql <- if (nchar(mode) > 0) {
      paste0("PRAGMA wal_checkpoint(", mode, ")")
    } else {
      "PRAGMA wal_checkpoint"
    }

  res <- try(DBI::dbGetQuery(con, sql), silent = TRUE)

  if (!identical(failure, "silent") && (inherits(res, "try-error") || res["busy"] > 0)) {
    msg <- paste("'dbWork_checkpoint': failed to run wal-checkpoint for",
      shQuote(basename(dbWork)), "with", shQuote(paste(res, collapse = "/")))
    if (identical(failure, "warning")) {
      warning(msg)
    } else {
      stop(msg)
    }
  }

  invisible(res)
}

#' Do maintenance work on a SQLite-database \code{dbWork} of a rSFSW2 simulation project
#'
#' Some code, power, and system failures may leave \code{dbWork} in an incomplete state,
#' e.g., a rollback journal is present, or runs are marked as 'inwork' even though no
#' runs are currently being worked on. This function cleans such situations up.
#'
#' @inheritParams create_dbWork
#' @return A logical value
#' @export
dbWork_clean <- function(path) {
  dbWork <- fname_dbWork(path)
  stopifnot(file.exists(dbWork))

  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbWork, flags = RSQLite::SQLITE_RW)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  temp_jmode <- tolower(as.character(DBI::dbGetQuery(con, "PRAGMA journal_mode;")))

  if (temp_jmode == "wal") {
    dbWork_checkpoint(con = con, mode = "TRUNCATE", failure = "error")

  } else {
    # Is there a rollback journal present and we need to perform a vacuum operation?
    frj <- paste0(dbWork, "-journal")
    if (file.exists(frj)) {
      DBI::dbExecute(con, "VACUUM")
      if (file.exists(frj)) {
        stop("'dbWork_clean': failed to vacuum ", shQuote(basename(dbWork)))
      }
    }
  }

  # Are there 'inwork' records?
  # - mark non-complete and non-failed records as incomplete
  rs <- DBI::dbExecute(con, paste("UPDATE work SET completed = 0, failed = 0,",
      "inwork = 0, time_s = 0 WHERE inwork = 1 AND completed != 1 AND failed != 1"))
  # - mark completed records as complete
  rs <- DBI::dbExecute(con, paste("UPDATE work SET completed = 1, failed = 0,",
      "inwork = 0 WHERE inwork = 1 AND completed = 1"))
  # - mark failed records as failed
  rs <- DBI::dbExecute(con, paste("UPDATE work SET completed = 0, failed = 1,",
      "inwork = 0 WHERE inwork = 1 AND failed = 1"))

  invisible(TRUE)
}


#' Extract identification numbers of runs of a rSFSW2 simulation project which are
#'  uncompleted and not \code{inwork}
#'
#' @inheritParams create_dbWork
#' @return An integer vector of \code{runIDs}.
#' @export
dbWork_todos <- function(path) {
  dbWork <- fname_dbWork(path)
  stopifnot(file.exists(dbWork))

  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbWork, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  x <- DBI::dbGetQuery(con, paste("SELECT runID_total FROM work ",
    "WHERE include_YN = 1 AND completed = 0 AND inwork = 0 ORDER BY runID_total"))
  as.integer(x[, 1])
}

#' Extract stored execution times of completed runs of a rSFSW2 simulation project
#'
#' @inheritParams create_dbWork
#' @return A numeric vector of execution time in seconds.
#' @export
dbWork_timing <- function(path) {
  dbWork <- fname_dbWork(path)
  stopifnot(file.exists(dbWork))

  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbWork, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  times <- DBI::dbGetQuery(con, paste("SELECT time_s FROM work WHERE include_YN = 1",
    "AND completed > 0"))
  as.numeric(times[, 1])
}



#' Set runs information that need to be redone / simulated (again)
#'
#' @inheritParams create_dbWork
#' @inheritParams dbWork_update_job
#' @param runIDs An integer vector. The identification numbers of queried runs,
#'  i.e., values out of \code{runIDs_total}, see \code{\link{indices}}.
#' @return A logical vector indicating success.
#' @export
dbWork_redo <- function(path, runIDs) {
  if (length(runIDs) > 0) {
    dbWork <- fname_dbWork(path)
    stopifnot(file.exists(dbWork))

    con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbWork, flags = RSQLite::SQLITE_RW)
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    rs <- DBI::dbSendStatement(con, paste("UPDATE work SET completed = 0, failed = 0,",
      "inwork = 0, time_s = 0 WHERE include_YN = 1 AND runID_total = :x"))
    DBI::dbBind(rs, param = list(x = runIDs))
    DBI::dbClearResult(rs)

  } else {
    TRUE
  }
}


#' Check run status
#'
#' @inheritParams create_dbWork
#' @inheritParams dbWork_update_job
#' @inheritParams dbWork_redo
#' @return A data.frame with three columns 'completed', 'failed', and 'inwork'
#' @export
dbWork_check <- function(path, runIDs) {
  if (length(runIDs) > 0) {
    dbWork <- fname_dbWork(path)
    stopifnot(file.exists(dbWork))

    con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbWork, flags = RSQLite::SQLITE_RO)
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    sql <- "SELECT completed, failed, inwork FROM work WHERE runID_total = :x"
    rs <- DBI::dbSendStatement(con, sql)
    DBI::dbBind(rs, param = list(x = runIDs))
    res <- DBI::dbFetch(rs)
    DBI::dbClearResult(rs)

  } else {
    res <- data.frame(completed = numeric(0), failed = numeric(0), inwork = numeric(0))
  }

  res
}



#' Re-create dbWork based on dbOutput
#'
#' @inheritParams create_dbWork
#' @param dbOutput A character string. Full name to the output database.
#' @param SFSW2_prj_meta An environment. If not \code{NULL}, then \code{path} and \code{dbOutput} may
#'  be missing. If not \code{NULL}, then code checks that no temporary output files remain
#'  unprocessed.
#'
#' @return A logical vector indicating success.
#'
#' @export
recreate_dbWork <- function(path, dbOutput, SFSW2_prj_meta = NULL) {
  if (missing(path)) {
    path <- if (!is.null(SFSW2_prj_meta)) {
        SFSW2_prj_meta[["project_paths"]][["dir_out"]]
      } else stop("'recreate_dbWork': argument 'path' is missing.")
  }

  if (missing(dbOutput)) {
    dbOutput <- if (!is.null(SFSW2_prj_meta)) {
        SFSW2_prj_meta[["fnames_out"]][["dbOutput"]]
      } else stop("'recreate_dbWork': argument 'dbOutput' is missing.")
  }

  if (!is.null(SFSW2_prj_meta)) {
    # check that no temporary output files remain unprocessed
    tempN_todo <- length(get_fnames_temporaryOutput(
      dir_out_temp = SFSW2_prj_meta[["project_paths"]][["dir_out_temp"]],
      concatFile = file.path(SFSW2_prj_meta[["project_paths"]][["dir_out_temp"]],
        "sqlFilesInserted.txt")))

    if (tempN_todo > 0) {
      stop("'recreate_dbWork' can only correctly re-create `dbWork` if",
        " all temporary output files have been moved to the database: \n",
        "Currently, n(unfinished temporary files) = ", tempN_todo, ".\n",
        "Use first, for instance, function `move_output_to_dbOutput` before trying again.")
    }
  }


  if (file.exists(dbOutput)) {
    dbWork <- fname_dbWork(path)

    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbOutput, flags = RSQLite::SQLITE_RO)
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    if (!all(sapply(c("runs", "sites"), function(x) DBI::dbExistsTable(con, x)))) {
      stop("'recreate_dbWork': OutputDB ", shQuote(dbOutput), " has ",
        "incomplete structure; dbWork cannot be recreated from it.")
    }

    table_runs <- DBI::dbReadTable(con, "runs")
    table_sites <- DBI::dbReadTable(con, "sites")

    # Infer design of simulation experiment
    infer_expN <- max(table_runs[, "treatment_id"])
    infer_scN <- max(table_runs[, "scenario_id"])
    infer_runIDs <- unique(it_sim2(table_runs[, "P_id"], infer_scN))
    infer_runsN_total <- max(infer_runIDs)

    infer_runsN_master <- dim(table_sites)[1]
    infer_include_YN <- as.logical(table_sites[, "Include_YN"])
    infer_runIDmax_sites <- max(table_sites[, "site_id"])

    do_new_dbWork <- TRUE

    if (file.exists(dbWork)) {
      # If dbWork present, check whether design is current
      con2 <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbWork, flags = RSQLite::SQLITE_RW)
      on.exit(DBI::dbDisconnect(con2), add = TRUE)

      if (DBI::dbExistsTable(con2, "work")) {
        has_work <- DBI::dbReadTable(con2, "work")

        if (all(c("runID_total", "runID_sites") %in% names(has_work))) {
          # Create new dbWork if number of total runs or number of sites does not match
          do_new_dbWork <- !(max(has_work[, "runID_total"]) == dim(has_work)[1] &&
            max(has_work[, "runID_total"]) == infer_runsN_total &&
            max(has_work[, "runID_sites"]) == infer_runIDmax_sites)
        }
      }
    }

    if (do_new_dbWork) {
      # Create new dbWork
      infer_sim_size <- list(runsN_master = infer_runsN_master,
        runsN_total = infer_runsN_total, expN = infer_expN)
      setup_dbWork(path, infer_sim_size, include_YN = infer_include_YN)

    } else {
      # Check whether include_YN should be updated
      infer_include_YN2 <- rep(as.integer(infer_include_YN), times = infer_expN)
      if (!identical(has_work[, "include_YN"], infer_include_YN2)) {
        con2 <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbWork, flags = RSQLite::SQLITE_RW)
        on.exit(DBI::dbDisconnect(con2), add = TRUE)

        rs <- DBI::dbSendStatement(con2, paste("UPDATE work SET include_YN = :x1",
          "WHERE runID_total = :x2"))
        DBI::dbBind(rs, param = list(x1 = infer_include_YN2, x2 = infer_runIDs))
        DBI::dbClearResult(rs)
      }
    }

    #--- Update completed runs
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbOutput, flags = RSQLite::SQLITE_RO)
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    #- Update completed runs based on dbOutput
    tables <- dbOutput_ListOutputTables(con)
    # get Pids for which simulation output is in the outputDB
    has_pids <- lapply(tables, function(x) DBI::dbGetQuery(con,
      paste0("SELECT P_id FROM \"", x, "\""))[, 1])
    has_complete_pids <- intersect2(has_pids)

    if (length(has_complete_pids) > 0) {
      con2 <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbWork, flags = RSQLite::SQLITE_RW)
      on.exit(DBI::dbDisconnect(con2), add = TRUE)

      has_complete_runIDs <- unique(it_sim2(has_complete_pids, infer_scN))

      rs <- DBI::dbSendStatement(con2, paste("UPDATE work SET completed = 1, failed = 0,",
        "inwork = 0 WHERE runID_total = :x"))
      DBI::dbBind(rs, param = list(x = has_complete_runIDs))
      DBI::dbClearResult(rs)
    }

  } else {
    stop("OutputDB ", shQuote(dbOutput), " not found on disk.")
  }
}



#' Update run information of a rSFSW2 simulation project
#'
#' @inheritParams create_dbWork
#' @param runID An integer value. The identification number of the current run,
#'  i.e., a value out of \code{runIDs_total}, see \code{\link{indices}}.
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
#' @export
dbWork_update_job <- function(path, runID, status = c("completed", "failed", "inwork"),
  time_s = "NULL", with_filelock = NULL, verbose = FALSE) {

  status <- match.arg(status)
  dbWork <- fname_dbWork(path)
  stopifnot(file.exists(dbWork))

  lock <- if (!is.null(with_filelock)) {
      lock_init(with_filelock, runID)
    } else {
      list(confirmed_access = TRUE)
    }

  success <- FALSE
  res <- 0L

  if (verbose) {
    t0 <- Sys.time()
  }

  repeat {
    if (verbose) {
      print(paste0("'dbWork_update_job': ", Sys.time(), " (", runID, "-", status,
        ") attempt to update after ", round(difftime(Sys.time(), t0, units = "secs"), 2),
        " s"))
    }

    if (!is.null(with_filelock)) {
      lock <- lock_access(lock, verbose, seed = NA)
    }

    con <- try(RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbWork,
      flags = RSQLite::SQLITE_RW), silent = TRUE)

    if (inherits(con, "SQLiteConnection")) {
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      res <- try(DBI::dbWithTransaction(con, {
        if (verbose) {
          print(paste0("'dbWork_update_job': ", Sys.time(), " (", runID, "-", status,
            ") start transaction after ", round(difftime(Sys.time(), t0, units = "secs"), 2),
            " s"))
        }

        temp <- if (status == "completed") {
            DBI::dbExecute(con, paste("UPDATE work SET completed = 1, failed = 0,",
              "inwork = 0, time_s =", time_s, "WHERE runID_total =", runID))

          } else if (status == "failed") {
            DBI::dbExecute(con, paste("UPDATE work SET completed = 0, failed = 1,",
              "inwork = 0, time_s =", time_s, "WHERE runID_total =", runID))

          } else if (status == "inwork") {
            prev_status <- DBI::dbGetQuery(con,
              paste("SELECT inwork FROM work WHERE runID_total =", runID))$inwork
            if (prev_status == 0) {
              DBI::dbExecute(con, paste("UPDATE work SET completed = 0, failed = 0,",
                "inwork = 1, time_s = 0 WHERE runID_total =", runID))
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
          if (verbose) {
            print(paste0("'dbWork_update_job': ", Sys.time(), " (", runID, "-", status,
              ") access confirmation failed after ",
              round(difftime(Sys.time(), t0, units = "secs"), 2), " s"))
          }
          temp <- 0L
          DBI::dbBreak()

        } else if (verbose) {
          print(paste0("'dbWork_update_job': ", Sys.time(), " (", runID, "-", status,
            ") transaction confirmed after ",
            round(difftime(Sys.time(), t0, units = "secs"), 2), " s"))
        }

        as.integer(temp)
      }), silent = !verbose)

      success <- if (inherits(res, "try-error")) FALSE else lock$confirmed_access
    }

    if (success) {
      break

    } else {
      if (verbose) {
        print(paste0("'dbWork_update_job': ", Sys.time(), " (", runID, "-", status,
          ") 'dbWork' is locked after ",
          round(difftime(Sys.time(), t0, units = "secs"), 2), " s"))
      }

      # Sys.sleep(stats::runif(1, 0.02, 0.1))
    }
  }

  identical(res, 1L)
}


#------ End of dbWork functions
########################
