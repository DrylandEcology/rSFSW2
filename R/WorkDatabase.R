########################
#------ dbWork functions

add_dbWork_index <- function(con) {
  stopifnot(DBI::dbIsValid(con))

  prev_indices <- DBI::dbGetQuery(con,
    "SELECT * FROM sqlite_master WHERE type = 'index'")

  if (DBI::dbExistsTable(con, "work") && (NROW(prev_indices) == 0L ||
    !("i_runIDs" %in% prev_indices[, "name"]))) {

    sql <- paste("CREATE INDEX i_runIDs ON work (runID_total, runID_sites,",
      "include_YN)")
    DBI::dbExecute(con, sql)
  }

  if (DBI::dbExistsTable(con, "need_outputs") && (NROW(prev_indices) == 0L ||
    !("i_needIDs" %in% prev_indices[, "name"]))) {

    sql <- paste("CREATE INDEX i_needIDs ON need_outputs (Pid, runID_total,",
      "include_YN)")
    DBI::dbExecute(con, sql)
  }
}


fname_dbWork <- function(path, dbname = "dbWork.sqlite3") {
  if (grepl(".sql", basename(path))) {
    path <- dirname(path)
  }

  file.path(path, dbname)
}



#' Create a \var{SQLite}-database \code{dbWork} to manage runs of a \pkg{rSFSW2}
#' simulation project
#'
#' @param path A character string. Path to the folder where the database will be
#'   created.
#' @param jobs An integer matrix. Each row corresponds to one call of the
#'   simulation function \code{do_OneSite}, i.e., \code{runsN_master} x
#'   \code{expN}. The columns \code{runID_total}, \code{runID_sites},
#'   \code{include_YN} represent a running ID, the \code{site_id} (row number in
#'   master input file), and a flag whether site is being simulated or not. See
#'   \code{\link{indices}}.
#'
#' @return Invisibly \code{TRUE}
create_dbWork <- function(path, jobs) {

  stopifnot(colnames_work_dbWork() %in% dimnames(jobs)[[2]])
  dbWork <- fname_dbWork(path)

  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbWork,
    flags = RSQLite::SQLITE_RWC)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(con,
    paste("CREATE TABLE work(runID_total INTEGER PRIMARY KEY,",
    "runID_sites INTEGER NOT NULL, include_YN INTEGER NOT NULL,",
    "completed INTEGER NOT NULL, failed INTEGER NOT NULL,",
    "inwork INTEGER NOT NULL, time_s REAL)"))

  RSQLite::dbWriteTable(con, "work", append = TRUE, value = as.data.frame(jobs))
  add_dbWork_index(con)

  add_status_dbWork(con)

  # Set WAL-mode (http://www.sqlite.org/wal.html)
  temp_jmode <- DBI::dbGetQuery(con, "PRAGMA journal_mode=WAL;")
  if (!(tolower(as.character(temp_jmode)) == "wal")) {
    print(paste("'create_dbWork': setting WAL mode for dbWork failed;",
      "journal mode is instead", shQuote(temp_jmode)))
  }

  # Set synchronous mode to OFF=0
  #   (https://www.sqlite.org/pragma.html#pragma_synchronous)
  # Fast but data may be lost if power fails
  # no need to set this PRAGMA, because `RSQLite::dbConnect` does it by default

  # Turn off busy timeout
  #   (https://www.sqlite.org/pragma.html#pragma_busy_timeout)
  # no need to set this PRAGMA, because `RSQLite::dbConnect` does it by default


  invisible(TRUE)
}


colnames_work_dbWork <- function() {
  c("runID_total", "runID_sites", "include_YN", "completed", "failed",
    "inwork", "time_s")
}

colnames_status_dbWork <- function() {
  c("status", "time_stamp")
}

colnames_need_outputs_dbWork <- function() {
  c("Pid", "runID_total", "include_YN")
}


create_job_df <- function(sim_size, include_YN) {
  temp <- colnames_work_dbWork()
  jobs <- matrix(data = 0L, nrow = sim_size[["runsN_total"]],
    ncol = length(temp), dimnames = list(NULL, temp))

  jobs[, "runID_total"] <- seq_len(sim_size[["runsN_total"]])
  jobs[, "runID_sites"] <- rep(seq_len(sim_size[["runsN_master"]]),
    times = max(sim_size[["expN"]], 1L))
  temp <- rep(include_YN, times = max(sim_size[["expN"]], 1L))
  jobs[temp, "include_YN"] <- 1L

  jobs
}


#' Create new table \code{need_outputs} for granular control of individual
#' output elements for each \code{Pid x table} combination
#'
#' The table \code{need_outputs} has one row/record for each \code{P_id = Pid}
#' and contains columns \code{Pid}, \code{runID_total} (that links to table
#' \code{work}), and one column for each output table of \code{dbOutput} with
#' the same names. Those fields contain values \itemize{ \item -1 No output is
#' requested, e.g., site is excluded from simulation project by input setting of
#' \code{include_YN == 0}, \item 0 FALSE, output is not needed (any more), i.e.,
#' output is already produced, \item 1 TRUE, output is needed, i.e., output has
#' not yet been generated. }
#'
#' @inheritParams create_dbWork
#' @return A logical value.
add_granular_dbWork <- function(SFSW2_prj_meta) {
  dbWork <- fname_dbWork(SFSW2_prj_meta[["project_paths"]][["dir_out"]])
  stopifnot(file.exists(dbWork))

  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbWork,
    flags = RSQLite::SQLITE_RW)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  if (DBI::dbExistsTable(con, "need_outputs")) {
    res <- TRUE

  } else {
    #--- Create new table 'need_outputs' for granular control;
    # with default -1 = no output requested (e.g., Include_YN == 0)
    stopifnot(file.exists(SFSW2_prj_meta[["fnames_out"]][["dbOutput"]]))

    out_tables <- dbOutput_ListOutputTables(
      dbname = SFSW2_prj_meta[["fnames_out"]][["dbOutput"]])
    temp_tables <- paste(DBI::dbQuoteIdentifier(con, out_tables),
      "INTEGER DEFAULT -1", collapse = ", ")

    sql <- paste0("CREATE TABLE need_outputs(Pid INTEGER PRIMARY KEY, ",
      "runID_total INTEGER, include_YN INTEGER, ", temp_tables, ")")
    DBI::dbExecute(con, sql)

    #--- Create table content: add one row per Pid
    temp_pids <- seq_len(SFSW2_prj_meta[["sim_size"]][["runsN_Pid"]])
    temp_runIDs <- it_sim2(temp_pids, SFSW2_prj_meta[["sim_scens"]][["N"]])

    sql <- paste("SELECT runID_total, include_YN FROM work",
      "WHERE runID_total IN (?)",
      "ORDER BY runID_total")
    rs <- DBI::dbSendStatement(con, sql)
    RSQLite::dbBind(rs, list(unique(temp_runIDs)))
    temp_include_YN <- RSQLite::dbFetch(rs)
    RSQLite::dbClearResult(rs)

    df_granular <- data.frame(matrix(NA, nrow = length(temp_pids),
      ncol = 3L + length(out_tables), dimnames = list(NULL,
        c("Pid", "runID_total", "include_YN", out_tables))))

    df_granular[, "Pid"] <- temp_pids
    df_granular[, "runID_total"] <- temp_runIDs
    ids <- match(temp_runIDs, temp_include_YN[, "runID_total"], nomatch = 0)
    df_granular[, "include_YN"] <- temp_include_YN[ids, "include_YN"]
    df_granular[, out_tables] <-
      ifelse(df_granular[, "include_YN"] > 0, 1L, -1L)

    res <- RSQLite::dbWriteTable(con, "need_outputs", value = df_granular,
      row.names = FALSE, append = TRUE)
  }

  add_dbWork_index(con)

  invisible(res)
}

#' Create a new table \code{modification_status} with one row for status control
#' of entire \code{dbWork}
#'
#' First check whether such a table with one row exists, if so, don't overwrite.
#'
#' The table \code{modification_status} consists of one row/record/entry and has
#' two fields:
#' \itemize{
#'   \item \code{status} with value FALSE/0 for \code{not modified} and
#'     TRUE/1 for \code{modified}.
#'   \item \code{time_stamp} with the \code{\link{POSIXct}} value of the
#'     last time the \code{status} was updated. }
#'
#' @param con A valid \code{SQLiteConnection} database connection to
#'   \code{dbWork}.
add_status_dbWork <- function(con) {
  has_table <- DBI::dbExistsTable(con, "modification_status")
  sql <- "SELECT COUNT(*) FROM modification_status"
  create_new <- !(has_table && length(DBI::dbGetQuery(con, sql)) == 1L)

  if (create_new) {
    if (has_table) {
      sql <- "DROP TABLE modification_status"
      DBI::dbExecute(con, sql)
    }

    sql <- paste("CREATE TABLE modification_status(status INTEGER,",
      "time_stamp INTEGER)")
    DBI::dbExecute(con, sql)

    # Set initial status and assume that dbWork is in sync,
    # i.e., dbOut is not modified
    sql <- paste0("INSERT INTO modification_status ",
      "VALUES (", 0L, ", ", as.integer(Sys.time()), ")")
    DBI::dbExecute(con, sql)
  }
}


#' Update \var{\sQuote{include_YN}}
dbWork_update_IncludeYN <- function(con, table, id_name, has_include_YN,
  should_include_YN) {

  if (!identical(has_include_YN, should_include_YN)) {
    Yes <- 1L
    No <- 0L

    sql <- paste("UPDATE", DBI::dbQuoteIdentifier(con, table),
      "SET include_YN = :yn WHERE", DBI::dbQuoteIdentifier(con, id_name),
      "= :x")
    rs <- DBI::dbSendStatement(con, sql)

    # now excluded and previously included
    iwork <- which(has_include_YN == Yes & should_include_YN == No)
    n <- length(iwork)
    if (n > 0) {
      DBI::dbBind(rs, param = list(yn = rep(No, n), x = iwork))
    }

    # now included and previously excluded
    iwork <- which(has_include_YN == No & should_include_YN == Yes)
    n <- length(iwork)
    if (n > 0) {
      DBI::dbBind(rs, param = list(yn = rep(Yes, n), x = iwork))
    }

    DBI::dbClearResult(rs)
  }
}


#' Setup or connect to \var{SQLite}-database \code{dbWork} to manage runs of a
#' \pkg{rSFSW2} simulation project
#'
#' \code{dbWork} tracks completion of each \code{runID} with table \code{work},
#' i.e., an entire call to \code{\link{do_OneSite}}. If your project requires a
#' finer granularity of output management, then set the
#' \code{use_granular_control} in the project description and pass
#' \code{SFSW2_prj_meta}; in that case, the function calls
#' \code{\link{add_granular_dbWork}} to add the table \code{need_outputs} to
#' \code{dbWork}.
#'
#' @inheritParams create_dbWork
#' @param resume A logical value. If \code{TRUE} and \code{dbWork} exists, then
#'   function connects to the existing database. If \code{FALSE}, then a new
#'   database is created (possibly overwriting an existing one).
#' @param SFSW2_prj_meta An environment. Required if \code{use_granular_control}
#'   is set or if \code{sim_size} or \code{path} are missing. If passed as
#'   argument and \code{resume} and \code{dbWork} exists, then
#'   \code{\link{recreate_dbWork}} is called.
#'
#' @return A logical value indicating success/failure of setting up/connecting
#'   to \code{dbWork} and initializing with \code{runIDs}.
#' @export
setup_dbWork <- function(path, sim_size, include_YN, resume = FALSE,
  SFSW2_prj_meta = NULL) {

  if (missing(path)) {
    path <- if (!is.null(SFSW2_prj_meta)) {
        SFSW2_prj_meta[["project_paths"]][["dir_out"]]
      } else stop("'setup_dbWork': argument 'path' is missing.")
  }

  dbWork <- fname_dbWork(path)
  success <- FALSE
  create <- TRUE

  if (resume) {
    if (file.exists(dbWork)) {
      create <- FALSE
      success <- if (!is.null(SFSW2_prj_meta)) {
          recreate_dbWork(SFSW2_prj_meta)
        } else TRUE
    }

  } else {
    unlink(dbWork)
  }

  if (create) {
    if (missing(sim_size)) {
      sim_size <- if (!is.null(SFSW2_prj_meta)) {
        SFSW2_prj_meta[["sim_size"]]
      } else stop("'setup_dbWork': argument 'sim_size' is missing.")
    }

    temp <- create_dbWork(path, jobs = create_job_df(sim_size, include_YN))
    success <- !inherits(temp, "try-error")

    if (success && !is.null(SFSW2_prj_meta) &&
      isTRUE(SFSW2_prj_meta[["opt_out_fix"]][["use_granular_control"]])) {
      success <- add_granular_dbWork(SFSW2_prj_meta)
    }
  }

  success
}


#' Initiate a checkpoint operation on a \var{SQLite}-database \code{dbWork} of a
#' \pkg{rSFSW2} simulation project
#'
#' @references \url{https://www.sqlite.org/pragma.html#pragma_wal_checkpoint}
dbWork_checkpoint <- function(path = NULL, con = NULL,
  mode = c("PASSIVE", "FULL", "RESTART", "TRUNCATE", ""),
  failure = c("silent", "warning", "error"), verbose = FALSE) {

  mode <- match.arg(mode)
  failure <- match.arg(failure)

  if (is.null(con)) {
    dbWork <- fname_dbWork(path)
    stopifnot(file.exists(dbWork))

    # need write privilege to run wal_checkpoint
    con <- dbConnect2(dbWork, verbose = verbose)

    stopifnot(inherits(con, "SQLiteConnection"))
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

  if (!identical(failure, "silent") && (inherits(res, "try-error") ||
      res["busy"] > 0)) {

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


#' Do maintenance work on a \var{SQLite}-database \code{dbWork} if it exists
#'
#' Some code, power, and system failures may leave \code{dbWork} in an
#' incomplete state, e.g., a rollback journal is present, or runs are marked as
#' \var{\dQuote{inwork}} even though no runs are currently being worked on. This
#' function cleans such situations up.
#'
#' @inheritParams create_dbWork
#' @return A logical value
#' @export
dbWork_clean <- function(path, verbose = FALSE) {
  res <- TRUE

  dbWork <- fname_dbWork(path)
  if (file.exists(dbWork)) {

    con <- dbConnect2(dbWork, verbose = verbose)
    res <- inherits(con, "SQLiteConnection")

    if (res) {
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      temp_jmode <- tolower(as.character(DBI::dbGetQuery(con,
        "PRAGMA journal_mode;")))

      if (temp_jmode == "wal") {
        dbWork_checkpoint(con = con, mode = "TRUNCATE", failure = "error")

      } else {
        dbVacuumRollack(con, dbWork)
      }

      # Are there 'inwork' records?
      # - mark non-complete and non-failed records as incomplete
      sql <- paste("UPDATE work SET completed = 0, failed = 0, inwork = 0,",
        "time_s = 0 WHERE inwork = 1 AND completed != 1 AND failed != 1")
      DBI::dbExecute(con, sql)
      # - mark completed records as complete
      sql <- paste("UPDATE work SET completed = 1, failed = 0, inwork = 0",
        "WHERE inwork = 1 AND completed = 1")
      DBI::dbExecute(con, sql)
      # - mark failed records as failed
      sql <- paste("UPDATE work SET completed = 0, failed = 1, inwork = 0",
        "WHERE inwork = 1 AND failed = 1")
      DBI::dbExecute(con, sql)
    }
  }

  invisible(res)
}


#' Extract identification numbers of runs of a \pkg{rSFSW2} simulation project
#' which are uncompleted and not \var{\dQuote{inwork}}
#'
#' @inheritParams create_dbWork
#' @return An integer vector of \code{runIDs}.
#' @export
dbWork_todos <- function(path) {
  dbWork <- fname_dbWork(path)
  stopifnot(file.exists(dbWork))

  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbWork,
    flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  x <- DBI::dbGetQuery(con, paste("SELECT runID_total FROM work ",
    "WHERE include_YN = 1 AND completed = 0 AND inwork = 0",
    "ORDER BY runID_total"))
  as.integer(x[, 1])
}


#' Numbers of runs of a \pkg{rSFSW2} simulation project which are uncompleted
#' and not \var{\dQuote{inwork}}
#'
#' @inheritParams create_dbWork
#' @return An integer value.
#' @export
dbWork_Ntodo <- function(path) {
  dbWork <- fname_dbWork(path)
  stopifnot(file.exists(dbWork))

  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbWork,
    flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  x <- DBI::dbGetQuery(con, paste("SELECT COUNT(*) FROM work",
    "WHERE include_YN = 1 AND completed = 0 AND inwork = 0",
    "ORDER BY runID_total"))
  as.integer(x[, 1])
}


#' Extract stored execution times of completed runs of a \pkg{rSFSW2}
#' simulation project
#'
#' @inheritParams create_dbWork
#' @return A numeric vector of execution time in seconds.
#' @export
dbWork_timing <- function(path) {
  dbWork <- fname_dbWork(path)
  stopifnot(file.exists(dbWork))

  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbWork,
    flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  times <- DBI::dbGetQuery(con, paste("SELECT time_s FROM work",
    "WHERE include_YN = 1 AND completed > 0"))
  as.numeric(times[, 1])
}


#' Calculate mean and standard deviation of stored execution times of
#' completed runs
#'
#' @inheritParams create_dbWork
#' @return A numeric vector with mean and standard deviation of execution
#'   time in seconds and number of completed runs.
#' @seealso \code{\link{dbWork_timing}}
#' @export
dbWork_agg_timing <- function(path) {
  dbWork <- fname_dbWork(path)
  stopifnot(file.exists(dbWork))

  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbWork,
    flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  sql <- paste(
    "WITH done(time_s) AS (SELECT time_s FROM work",
      "WHERE include_YN = 1 AND completed > 0)",
    "SELECT sub.mean AS mean, COUNT(*) AS n,",
      "SUM((time_s - sub.mean) * (time_s - sub.mean)) AS ss from done,",
      "(SELECT AVG(time_s) AS mean FROM done) AS sub")
  temp <- DBI::dbGetQuery(con, sql)

  c(mean = temp[, "mean"],
    sd = sqrt(temp[, "ss"] / (temp[, "n"] - 1)),
    n = temp[, "n"])
}




#' Set runs information that need to be redone / simulated (again)
#'
#' @inheritParams create_dbWork
#' @inheritParams dbWork_update_job
#' @param runIDs An integer vector. The identification numbers of queried runs,
#'  i.e., values out of \code{runIDs_total}, see \code{\link{indices}}.
#' @return A logical vector indicating success.
#' @export
dbWork_redo <- function(path, runIDs, verbose = FALSE) {
  res <- TRUE

  if (length(runIDs) > 0) {
    dbWork <- fname_dbWork(path)
    res <- file.exists(dbWork)

    if (res) {
      con <- dbConnect2(dbWork, verbose = verbose)
      res <- inherits(con, "SQLiteConnection")

      if (res) {
        on.exit(DBI::dbDisconnect(con), add = TRUE)

        sql <- paste("UPDATE work SET completed = 0, failed = 0,",
          "inwork = 0, time_s = 0 WHERE include_YN = 1 AND runID_total = :x")
        rs <- DBI::dbSendStatement(con, sql)
        DBI::dbBind(rs, param = list(x = runIDs))
        res <- DBI::dbClearResult(rs)
      }
    }
  }

  invisible(res)
}


#' Check run status
#'
#' @inheritParams create_dbWork
#' @inheritParams dbWork_update_job
#' @inheritParams dbWork_redo
#' @return A data.frame with three columns \var{\dQuote{completed}},
#'   \var{\dQuote{failed}}, and \var{\dQuote{inwork}} and one row per
#'   \code{runIDs}.
#' @export
dbWork_check_run <- function(path, runIDs) {
  if (length(runIDs) > 0) {
    dbWork <- fname_dbWork(path)
    stopifnot(file.exists(dbWork))

    con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbWork,
      flags = RSQLite::SQLITE_RO)
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    sql <- "SELECT completed, failed, inwork FROM work WHERE runID_total = :x"
    rs <- DBI::dbSendStatement(con, sql)
    DBI::dbBind(rs, param = list(x = runIDs))
    res <- DBI::dbFetch(rs)
    DBI::dbClearResult(rs)

  } else {
    res <- data.frame(completed = numeric(0), failed = numeric(0),
      inwork = numeric(0))
  }

  res
}


#' Estimate percentage of completed runs
#'
#' @inheritParams create_dbWork
#' @param use_granular_control A logical value. If \code{TRUE} and the granular
#'   table is present (see \code{\link{add_granular_dbWork}}), then calculate
#'   percentage of completed runs based on granular table instead of \code{work}
#'   table.
#' @param SFSW2_prj_meta An environment. If \code{use_granular_control}, then
#'   required as well as presence of \code{dbOutput}.
#'
#' @return A numeric value in \code{[0,100]} in percent. The proportion of
#'   output units that are reported as complete.
#' @export
dbWork_report_completion <- function(path, use_granular_control = FALSE,
  SFSW2_prj_meta = NULL) {

  dbWork <- fname_dbWork(path)
  stopifnot(file.exists(dbWork))

  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbWork,
    flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  if (use_granular_control && "need_outputs" %in% DBI::dbListTables(con) &&
    !is.null(SFSW2_prj_meta) &&
    file.exists(SFSW2_prj_meta[["fnames_out"]][["dbOutput"]])) {

    sql <- "SELECT COUNT(*) FROM need_outputs WHERE include_YN = 1"
    N1 <- DBI::dbGetQuery(con, sql)
    out_tables <- dbOutput_ListOutputTables(
      dbname = SFSW2_prj_meta[["fnames_out"]][["dbOutput"]])
    N <- N1 * length(out_tables)

    quoted_tables <- DBI::dbQuoteIdentifier(con, out_tables)
    n <- 0
    for (k in quoted_tables) {
      sql <- paste("SELECT COUNT(*) FROM need_outputs WHERE include_YN = 1 AND",
        k, "= 0")
      n <- n + DBI::dbGetQuery(con, sql)
    }

  } else {
    N <- DBI::dbGetQuery(con, "SELECT COUNT(*) FROM work WHERE include_YN = 1")
    n <- DBI::dbGetQuery(con,
      "SELECT COUNT(*) FROM work WHERE include_YN = 1 AND completed = 1")
  }

  as.numeric(100 * n / N)
}


#' Check granular run status
#'
#' @inheritParams create_dbWork
#' @inheritParams dbWork_update_job
#' @inheritParams dbWork_redo
#' @return A data.frame with columns as created by
#'   \code{\link{add_granular_dbWork}} and one row per \code{runIDs}.
#' @export
dbWork_check_granular <- function(path, runIDs) {
  dbWork <- fname_dbWork(path)
  stopifnot(file.exists(dbWork))

  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbWork,
    flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  if (length(runIDs) > 0) {
    sql <- "SELECT * FROM need_outputs WHERE runID_total = :x"
    rs <- DBI::dbSendStatement(con, sql)
    DBI::dbBind(rs, param = list(x = runIDs))
    res <- DBI::dbFetch(rs)
    DBI::dbClearResult(rs)

  } else {
    sql <- "SELECT * FROM need_outputs LIMIT 0"
    res <- DBI::dbGetQuery(con, sql)
  }

  res
}


#' Check modification status of \code{dbWork} against \code{dbOut} and
#' \code{dbTempOut}
#'
#' @inheritParams create_dbWork
#' @param SFSW2_prj_meta An environment.
#'
#' @return A logical value. \code{TRUE} if the status of \code{dbWork} claims to
#'   be modified (out of sync) or if its time stamp is older (and suggest to be
#'   out of sync) than any modification time of the output generated for
#'   \code{dbOut} and/or \code{dbTempOut} -- the later is only checked if
#'   \code{SFSW2_prj_meta} is not null; \code{FALSE} if \code{dbWork} is deemed
#'   to be in sync with output progress of project.
#' @export
dbWork_check_status <- function(path, SFSW2_prj_meta = NULL) {
  dbWork <- fname_dbWork(path)
  stopifnot(file.exists(dbWork))

  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbWork,
    flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Consider modified if status is set to 'modified'
  sql <- "SELECT * FROM modification_status LIMIT 1"
  ms <- DBI::dbGetQuery(con, sql)

  res <- as.logical(ms[1, "status"])

  # If status claims to be not modified, then check that time_stamp is also
  # younger than modification times of dbOut and dbTempOut(s)
  if (!res && !is.null(SFSW2_prj_meta)) {
    tstatus <- as.POSIXct(ms[1, "time_stamp"], origin = "1970-01-01")

    ftemps <- c(SFSW2_prj_meta[["fnames_out"]][["dbOutput"]],
      get_fnames_dbTempOut(SFSW2_prj_meta[["project_paths"]][["dir_out_temp"]]))
    mtimes <- file.mtime(ftemps) # returns NA if file doesn't exist

    res <- any(tstatus < mtimes, na.rm = TRUE)

    if (res) {
      # Status is determined to be modified -> update dbWork
      dbWork_update_status(path = path, status = TRUE)
    }
  }

  res
}




#' Check that \code{dbWork} has an up-to-date structure of tables and fields
#'
#' @inheritParams create_dbWork
#' @param use_granular_control A logical value. If \code{TRUE} and the granular
#'   table is present (see \code{\link{add_granular_dbWork}}), then include the
#'   optional table \code{needs_output} in check.
#' @return A logical value. \code{FALSE} if no \code{dbWork} can be located or
#'   if any required table is missing or if not all required fields are present.
#'
#' @export
dbWork_check_design <- function(path, use_granular_control = FALSE) {
  res <- FALSE

  dbWork <- fname_dbWork(path)

  if (file.exists(dbWork)) {
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbWork,
      flags = RSQLite::SQLITE_RO)
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    # Check that required tables exist
    need_tables <- c("work", "modification_status",
      if (use_granular_control) "need_outputs")

    has_tables <- need_tables %in% DBI::dbListTables(con)

    # If all tables exist, then check that each table has required fields
    res <- all(has_tables)
    if (res) for (k in seq_along(need_tables)) {
      has_fields <- DBI::dbListFields(con, need_tables[k])
      ftemp <- match.fun(paste("colnames", need_tables[k], "dbWork", sep = "_"))
      res <- res && all(ftemp() %in% has_fields)
    }
  }

  res
}



#' Re-create or update \var{\sQuote{dbWork}} based on \var{\sQuote{dbOutput}}
#'
#' @inheritParams create_dbWork
#' @param dbOutput A character string. Full name to the output database.
#' @param use_granular_control A logical vector.
#' @param SFSW2_prj_meta An environment. If not \code{NULL}, then \code{path},
#'  \code{dbOutput}, and/or \code{use_granular_control} may be missing.
#'  If not \code{NULL}, then code checks that no temporary output files remain
#'  unprocessed. This is because this function only checks output in
#'  the database \var{\sQuote{dbOutput}}, but not in the database(s) files
#'  \var{\sQuote{dbTempOut}}.
#'
#' @return A logical vector indicating success.
#'
#' @export
recreate_dbWork <- function(path, dbOutput, use_granular_control,
  SFSW2_prj_meta = NULL, verbose = FALSE, print.debug = FALSE) {

  res <- FALSE

  if (verbose) {
    t1 <- Sys.time()
    temp_call <- shQuote(match.call()[1])
    print(paste0("rSFSW2's ", temp_call, ": started at ", t1))

    on.exit(print(paste0("rSFSW2's ", temp_call, ": ended after ",
      round(difftime(Sys.time(), t1, units = "secs"), 2))), add = TRUE)
  }

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

  if (missing(use_granular_control)) {
    use_granular_control <- if (!is.null(SFSW2_prj_meta) &&
      !is.null(SFSW2_prj_meta[["opt_out_fix"]][["use_granular_control"]])) {
        SFSW2_prj_meta[["opt_out_fix"]][["use_granular_control"]]
      } else {
        stop("'recreate_dbWork': argument 'use_granular_control' is missing.")
      }
  }

  if (!is.null(SFSW2_prj_meta)) {
    # check that no temporary output files remain unprocessed
    temp1 <- get_fnames_temporaryOutput(
      dir_out_temp = SFSW2_prj_meta[["project_paths"]][["dir_out_temp"]],
      concatFile = file.path(
        SFSW2_prj_meta[["project_paths"]][["dir_out_temp"]],
        "sqlFilesInserted.txt"))
    temp2 <- get_fnames_dbTempOut(
      SFSW2_prj_meta[["project_paths"]][["dir_out_temp"]])

    tempN_todo <- length(temp1) + length(temp2)

    if (tempN_todo > 0) {
      stop("'recreate_dbWork' can only correctly re-create `dbWork` if",
        " all temporary output files have been moved to the database: \n",
        "Currently, n(unfinished temporary files) = ", tempN_todo, ".\n",
        "Use first, for instance, function `move_output_to_dbOutput` before ",
        "trying again.")
    }
  }


  if (file.exists(dbOutput)) {
    #--- Infer design of simulation experiment based on dbOutput
    con_dbOut <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbOutput,
      flags = RSQLite::SQLITE_RO)
    on.exit(DBI::dbDisconnect(con_dbOut), add = TRUE)

    if (!all(sapply(c("runs", "sites"), function(x)
      DBI::dbExistsTable(con_dbOut, x)))) {
      stop("'recreate_dbWork': OutputDB ", shQuote(dbOutput), " has ",
        "incomplete structure; dbWork cannot be recreated from it.")
    }

    # List of output 'aggregation' tables
    out_tables <- dbOutput_ListOutputTables(con = con_dbOut)

    # Extract information from dbOutput table 'runs'
    infer_expN <- as.integer(DBI::dbGetQuery(con_dbOut,
      "SELECT MAX(treatment_id) FROM runs"))
    infer_scN <- as.integer(DBI::dbGetQuery(con_dbOut,
      "SELECT MAX(scenario_id) FROM runs"))
    infer_runsN_total <- as.integer(DBI::dbGetQuery(con_dbOut,
      "SELECT MAX(label_id) FROM runs")) # TODO(drs): this field should really
      # be called 'runID'

    # Extract information from dbOutput table 'sites'
    infer_runsN_master <- as.integer(DBI::dbGetQuery(con_dbOut,
      "SELECT COUNT(*) FROM sites"))
    infer_include_YN <- as.logical(DBI::dbGetQuery(con_dbOut,
      "SELECT Include_YN FROM sites")[, 1])
    infer_runIDmax_sites <- as.integer(DBI::dbGetQuery(con_dbOut,
      "SELECT MAX(site_id) FROM sites"))


    #--- If dbWork exists, then copy timing information if design is current
    dbWork <- fname_dbWork(path)
    old_timing_s <- NULL
    dbWork_backup <- paste0(dbWork, "_backup")

    if (file.exists(dbWork)) {
      con_dbWork <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbWork,
        flags = RSQLite::SQLITE_RW)
      on.exit(DBI::dbDisconnect(con_dbWork), add = TRUE)

      if (DBI::dbExistsTable(con_dbWork, "work")) {
        # Check whether to create new dbWork: number of total runs,
        # number of sites
        has_runsN_total <- as.integer(DBI::dbGetQuery(con_dbWork,
          "SELECT MAX(runID_total) FROM work"))
        has_count_work <- as.integer(DBI::dbGetQuery(con_dbWork,
          "SELECT COUNT(*) FROM work"))
        has_runIDmax_sites <- as.integer(DBI::dbGetQuery(con_dbWork,
          "SELECT MAX(runID_sites) FROM work"))

        if (has_runsN_total == has_count_work &&
            has_runsN_total == infer_runsN_total &&
            has_runIDmax_sites == infer_runIDmax_sites) {

          if (verbose) {
            print(paste0(Sys.time(), ": dbWork is present and of ",
              "adequate design, backup run timing data"))
          }

          old_timing_s <- DBI::dbGetQuery(con_dbWork,
            "SELECT runID_total, time_s FROM work ORDER BY runID_total")
        }
      }

      DBI::dbDisconnect(con_dbWork)

      if (verbose) {
        print(paste0(Sys.time(), ": previous dbWork is backed-up."))
      }
      file.rename(from = dbWork, to = dbWork_backup)
    }


    #--- Create new dbWork
    infer_sim_size <- list(runsN_master = infer_runsN_master,
      runsN_total = infer_runsN_total, expN = infer_expN)

    stopifnot(create_dbWork(path,
      jobs = create_job_df(infer_sim_size, infer_include_YN)))

    con_dbWork <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbWork,
      flags = RSQLite::SQLITE_RW)
    on.exit(DBI::dbDisconnect(con_dbWork), add = TRUE)

    if (use_granular_control) {
      stopifnot(!is.null(SFSW2_prj_meta))

      if (verbose) {
        print(paste0(Sys.time(), ": add 'granular-level' table for ",
          "new dbWork."))
      }
      stopifnot(add_granular_dbWork(SFSW2_prj_meta))
    }

    # If dbWork existed previously and there was timing information, then insert
    ids_use <- old_timing_s[, "time_s"] > 0
    if (length(ids_use) > 0) {
      if (verbose) {
        print(paste0(Sys.time(), ": previous timing data is re-inserted ",
          "into new dbWork"))
      }
      sql <- paste("UPDATE work SET time_s = :t WHERE runID_total = :x")
      rs <- DBI::dbSendStatement(con_dbWork, sql)
      DBI::dbBind(rs, param = list(t = old_timing_s[ids_use, "time_s"],
        x = old_timing_s[ids_use, "runID_total"]))
      DBI::dbClearResult(rs)
    }


    #--- Update dbWork based on completed runs stored in dbOutput

    if (!use_granular_control) {
      # Note: below code is not memory-efficient because of large numbers of
      # Pids for large projects --> use `use_granular_control`
      if (verbose) {
        print(paste0(Sys.time(), ": update information on completed runs"))
      }

      # Get Pids for which simulation output is in each table of the dbOutput
      has_pids_per_table <- lapply(out_tables, function(x) {
          sql <- paste0("SELECT DISTINCT P_id FROM",
            DBI::dbQuoteIdentifier(con_dbOut, x), "ORDER BY P_id")
          DBI::dbGetQuery(con_dbOut, sql)[, 1]
        })

      #-- Update table 'work'
      has_pids_complete <- intersect2(has_pids_per_table)

      if (length(has_pids_complete) > 0) {
        # Get runID from Pid
        temp_runIDs <- it_sim2(has_pids_complete, infer_scN)

        # runID is complete if present and if
        # count(runID) == number of scenarios
        has_complete_runIDs <- which(tabulate(temp_runIDs) == infer_scN)

        if (length(has_complete_runIDs) > 0) {
          # set complete runIDs
          sql <- paste("UPDATE work SET completed = 1, failed = 0, inwork = 0",
            "WHERE runID_total = :x")
          rs <- DBI::dbSendStatement(con_dbWork, sql)
          DBI::dbBind(rs, param = list(x = has_complete_runIDs))
          DBI::dbClearResult(rs)
        }
      }

    } else {
      quoted_tables <- DBI::dbQuoteIdentifier(con_dbWork, out_tables)

      sql <- paste("ATTACH", DBI::dbQuoteIdentifier(con_dbWork, dbOutput),
        "AS dbOut")
      DBI::dbExecute(con_dbWork, sql)

      #-- Update table 'need_outputs'
      for (k in seq_along(out_tables)) {
        if (verbose) {
          print(paste0(Sys.time(), ": update granular-level information for",
            " output table ", quoted_tables[k]))
        }

        # don't need to generate output (anymore) where output is present in
        # dbOut
        # TODO(drs): we currently assume that one entry per Pid even
        # for mean daily tables with soil layers is enough
        sql <- paste0("UPDATE need_outputs SET ", quoted_tables[k], " = 0 ",
          "WHERE Pid IN ",
          "(SELECT DISTINCT P_id FROM dbOut.", quoted_tables[k], ")")
        DBI::dbExecute(con_dbWork, sql)
      }

      DBI::dbExecute(con_dbWork, "DETACH dbOut")

      #-- Update table 'work' based on updated 'need_outputs'
      # a runID is complete if all associated Pids are complete, i.e.,
      # count(Pids) == infer_scN, for each output table
      if (verbose) {
        print(paste0(Sys.time(), ": update information on completed runs"))
      }

      sql <- paste0("UPDATE work SET completed = 1, failed = 0, inwork = 0 ",
        "WHERE runID_total IN (",
        "SELECT oa.runID_total FROM ",
        "(SELECT runID_total, COUNT(*) FROM need_outputs ",
        "WHERE ", paste(quoted_tables, "= 0", collapse = " AND "), " ",
        "GROUP BY runID_total) AS oa ",
        "WHERE oa.`COUNT(*)` = ", infer_scN, ")")
      DBI::dbExecute(con_dbWork, sql)
    }

    # Set modification status: up-to-date
    dbWork_update_status(path, status = FALSE, verbose = print.debug)

    unlink(dbWork_backup)
    res <- file.exists(dbWork)

  } else {
    stop("OutputDB ", shQuote(dbOutput), " not found on disk.")
  }

  invisible(res)
}


#' Update run information of a \pkg{rSFSW2} simulation project
#'
#' @inheritParams create_dbWork
#' @param runID An integer value. The identification number of the current run,
#'   i.e., a value out of \code{runIDs_total}, see \code{\link{indices}}.
#' @param status A character string. One of \var{\dQuote{completed}},
#'   \var{\dQuote{failed}}, \var{\dQuote{inwork}}.
#' @param time_s A numeric value. The execution time in seconds; used if
#'   \code{status} is one of \var{\dQuote{completed}} and \var{\dQuote{failed}}.
#' @param verbose A logical value. If \code{TRUE}, status messages about file
#'   lock and database access are printed
#'
#' @return A logical value whether the status was successfully updated.
dbWork_update_job <- function(path, runID, status, time_s = "NULL",
  verbose = FALSE) {

  dbWork <- fname_dbWork(path)
  success <- FALSE
  res <- 0L

  # Connect to dbWork
  con <- dbConnect2(dbWork, verbose = verbose)

  if (inherits(con, "SQLiteConnection")) {
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    if (verbose) {
      t0 <- Sys.time()
    }

    # Update job record
    repeat {
      if (verbose) {
        print(paste0("'dbWork_update_job': ", Sys.time(), " (", runID, "-",
          status, ") attempt to update after ",
          round(difftime(Sys.time(), t0, units = "secs"), 2), " s"))
      }

      if (status == "completed") {
        res <- try(DBI::dbExecute(con, paste("UPDATE work SET completed = 1,",
          "failed = 0, inwork = 0, time_s =", time_s,
          "WHERE runID_total =", runID)), silent = !verbose)

      } else if (status == "failed") {
        res <- try(DBI::dbExecute(con, paste("UPDATE work SET completed = 0,",
          "failed = 1, inwork = 0, time_s =", time_s,
          "WHERE runID_total =", runID)), silent = !verbose)

      } else if (status == "inwork") {
        # https://sqlite.org/lang_transaction.html: "After a BEGIN IMMEDIATE,
        # no other database connection will be able to write to the database or
        # do a BEGIN IMMEDIATE or BEGIN EXCLUSIVE. Other processes can continue
        # to read from the database, however."
        res <- try(DBI::dbExecute(con, "BEGIN IMMEDIATE"), silent = !verbose)

        if (!inherits(res, "try-error")) {
          # Transaction with reserved lock established
          prev_status <- DBI::dbGetQuery(con,
            paste("SELECT inwork FROM work WHERE runID_total =", runID))$inwork

          res <- if (prev_status == 0) {
              try(DBI::dbExecute(con, paste("UPDATE work SET completed = 0,",
                "failed = 0, inwork = 1, time_s = 0",
                "WHERE runID_total =", runID)), silent = !verbose)
            } else {
              0L
            }
        }

        # End transaction
        if (inherits(res, "try-error")) {
          try(DBI::dbExecute(con, "ROLLBACK"), silent = !verbose)
        } else {
          try(DBI::dbExecute(con, "COMMIT"), silent = !verbose)

          if (verbose && prev_status != 0) {
            print(paste("'dbWork_update_job':", runID, "is already in work"))
          }
        }

      } else {
        if (verbose) {
          print(paste("'dbWork_update_job': value", shQuote(status),
            "of argument 'status' is not implemented."))
        }

        res <- 0L
      }

      success <- !inherits(res, "try-error")

      if (success) {
        if (verbose) {
          print(paste0("'dbWork_update_job': ", Sys.time(), " (", runID, "-",
            status, ") transaction confirmed after ",
            round(difftime(Sys.time(), t0, units = "secs"), 2), " s"))
        }
        break

      } else {
        if (verbose) {
          print(paste0("'dbWork_update_job': ", Sys.time(), " (", runID, "-",
            status, ") 'dbWork' is locked after ",
            round(difftime(Sys.time(), t0, units = "secs"), 2), " s"))
        }

        Sys.sleep(stats::runif(1, 0.02, 0.1))
      }
    }
  }

  identical(res, 1L)
}


#' Update granular run information of a \pkg{rSFSW2} simulation project
#'
#' @inheritParams create_dbWork
#' @param table A character string.
#' @param Pid An integer value. The identification number of the current output,
#'   i.e., see \code{\link{indices}}.
#' @param status A logical value. \code{FALSE} indicates "no longer needed",
#'   i.e., that the output for \code{table} has successfully been generated (see
#'   \code{\link{add_granular_dbWork}}).
#' @param verbose A logical value.
#'
#' @return A logical value whether the status was successfully updated.
dbWork_update_granular <- function(path, table, Pid, status, verbose = FALSE) {
  dbWork <- fname_dbWork(path)
  success <- FALSE
  res <- 0L

  # Connect to dbWork
  con <- dbConnect2(dbWork, verbose = verbose)

  if (inherits(con, "SQLiteConnection")) {
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    if (verbose) {
      t0 <- Sys.time()
    }

    # Update job record
    repeat {
      if (verbose) {
        print(paste0("'dbWork_update_granular': ", Sys.time(), " (", Pid, "-",
          status, ") attempt to update after ",
          round(difftime(Sys.time(), t0, units = "secs"), 2), " s"))
      }

      sql <- paste("UPDATE need_outputs SET", shQuote(table), "=",
        if (isTRUE(status)) "1" else "0",
        "WHERE Pid =", as.integer(Pid))
      res <- try(DBI::dbExecute(con, sql), silent = !verbose)

      success <- !inherits(res, "try-error")

      if (success) {
        if (verbose) {
          print(paste0("'dbWork_update_granular': ", Sys.time(), " (", Pid, "-",
            status, ") transaction confirmed after ",
            round(difftime(Sys.time(), t0, units = "secs"), 2), " s"))
        }
        break

      } else {
        if (verbose) {
          print(paste0("'dbWork_update_granular': ", Sys.time(), " (", Pid, "-",
            status, ") 'dbWork' is locked after ",
            round(difftime(Sys.time(), t0, units = "secs"), 2), " s"))
        }

        Sys.sleep(stats::runif(1, 0.02, 0.1))
      }
    }
  }

  identical(res, 1L)
}



#' Update modification status of a \pkg{rSFSW2} simulation project
#'
#' @inheritParams create_dbWork
#' @param status A logical value. \code{FALSE} indicates "not modified", i.e.,
#'   that \code{dbWork} and generated output \code{dbOutput} and
#'   \code{dbTempOut} is synchronized; \code{TRUE} indicates "modified".
#' @param verbose A logical value.
#'
#' @seealso \code{\link{add_status_dbWork}}
#' @return A logical value whether the status was successfully updated.
dbWork_update_status <- function(path, status, verbose = FALSE) {
  dbWork <- fname_dbWork(path)
  success <- FALSE
  res <- 0L

  # Connect to dbWork
  con <- dbConnect2(dbWork, verbose = verbose)

  if (inherits(con, "SQLiteConnection")) {
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    if (verbose) {
      t0 <- Sys.time()
    }

    # Update status
    repeat {
      if (verbose) {
        print(paste0("'dbWork_update_status': ", Sys.time(), " (", status,
          ") attempt to update after ",
          round(difftime(Sys.time(), t0, units = "secs"), 2), " s"))
      }

      sql <- paste("UPDATE modification_status SET ",
        "status =", if (isTRUE(status)) "1" else "0", ",",
        "time_stamp =", as.integer(Sys.time()))
      res <- try(DBI::dbExecute(con, sql), silent = !verbose)

      success <- !inherits(res, "try-error")

      if (success) {
        if (verbose) {
          print(paste0("'dbWork_update_status': ", Sys.time(), " (", status,
            ") transaction confirmed after ",
            round(difftime(Sys.time(), t0, units = "secs"), 2), " s"))
        }
        break

      } else {
        if (verbose) {
          print(paste0("'dbWork_update_status': ", Sys.time(), " (", status,
            ") 'dbWork' is locked after ",
            round(difftime(Sys.time(), t0, units = "secs"), 2), " s"))
        }

        Sys.sleep(stats::runif(1, 0.02, 0.1))
      }
    }
  }

  identical(res, 1L)
}

#------ End of dbWork functions
########################
