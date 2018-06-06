########################
#------ database-IO functions

#' Execute a SQL statement on a database connection with safeguards
#'
#' A wrapper around \code{\link[DBI]{dbExecute}} that catches errors and attempts
#' to execute the SQL statement up to \code{repeats} before giving up if the database
#' was locked.
#'
#' @param con A \linkS4class{DBIConnection} or \linkS4class{SQLiteConnection} object.
#' @param SQL A character string or vector of character strings. The SQL statement(s) to
#'   execute on \code{con}. If \code{SQL} is a vector of character strings, then the
#'   loop across individual executions is wrapped in a transaction.
#' @param verbose A logical value.
#' @param repeats An integer value. The maximum number of failed attempts to execute the
#'   \code{SQL} statement(s) if the database is locked.
#' @param sleep_s A numeric value. The average number to sleep before attempting to
#'   execute the \code{SQL} statement(s) if the database was locked.
#' @param seed An R object. Passed to \code{\link{set.seed}} if not \code{NA}.
#' @return A logical value. \code{TRUE} if the execution of the \code{SQL} statement(s)
#'   did not error out.
#'
#' @seealso \code{\link[DBI]{dbExecute}}
#' @export
dbExecute2 <- function(con, SQL, verbose = FALSE, repeats = 10L, sleep_s = 5, seed = NA) {
  if (!anyNA(seed)) set.seed(seed)

  N <- length(SQL)

  k <- 1L
  repeat {
    temp_try <- try(if (N > 1) {
        DBI::dbWithTransaction(con, for (k in seq_len(N)) DBI::dbExecute(con, SQL[k]))
      } else {
        DBI::dbExecute(con, SQL)
      }, silent = !verbose)
    success <- !inherits(temp_try, "try-error")

    do_repeat <- FALSE

    if (!success) {
      # Failed call; decide what to do about it
      try_msg <- attr(temp_try, "condition")[["message"]]

      if (grepl("database is locked", try_msg)) {
        # Sleep and retry the call
        do_repeat <- TRUE
      } else if (grepl("UNIQUE constraint failed", try_msg)) {
        # Ignore the error and pretend that all is fine
        success <- TRUE
      } else {
        # Report any other problem as failure
      }
    }

    if (do_repeat && k <= repeats) {
      k <- k + 1L

      # sleep on average shape * scale; and hope that afterwards, the db will be available
      temp_sleep <- round(stats::rgamma(1L, shape = sleep_s, scale = 1), 1L)
      if (verbose) {
        print(paste("'dbExecute2': sleeps for", temp_sleep, "sec before attempt", k))
      }

      Sys.sleep(temp_sleep)

    } else {
      break
    }
  }

  invisible(success)
}


#' Connect to \var{SQLite}-database
#'
#' A wrapper around \code{\link[RSQLite]{dbConnect}} that catches errors and attempts
#' to connect up to \code{repeats} before giving up.
#'
#' @param dbname A character string. The path including name to the database.
#' @param flags An integer value. See \code{\link[RSQLite]{dbConnect}}. Defaults to
#'   read/write mode.
#' @inheritParams dbExecute2
#' @return A \linkS4class{SQLiteConnection} object on success; an object of class
#'   \code{\link[base:try]{try-error}} on failure.
#'
#' @seealso \code{\link[RSQLite]{dbConnect}}
#' @export
dbConnect2 <- function(dbname, flags = RSQLite::SQLITE_RW, verbose = FALSE,
  repeats = 10L, sleep_s = 5, seed = NA) {

  if (!anyNA(seed)) set.seed(seed)

  if (verbose) {
    t0 <- Sys.time()
  }

  k <- 1L
  repeat {
    if (verbose) {
      print(paste("'dbConnect2':", Sys.time(), "attempt to connect after",
        round(difftime(Sys.time(), t0, units = "secs"), 2), "s"))
    }

    con <- try(RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbname, flags = flags),
      silent = !verbose)

    if (inherits(con, "SQLiteConnection") || k > repeats) {
      break

    } else {
      k <- k + 1L

      # sleep on average shape * scale; and hope that afterwards, the db will be available
      temp_sleep <- round(stats::rgamma(1L, shape = sleep_s, scale = 1), 1L)
      if (verbose) {
        print(paste("'dbConnect2': sleeps for", temp_sleep, "sec before attempt", k))
      }

      Sys.sleep(temp_sleep)
    }
  }

  con
}




# PRAGMA, see http://www.sqlite.org/pragma.html
PRAGMA_settings1 <- function() c(
  "PRAGMA cache_size = 400000;",
  "PRAGMA synchronous = FULL;", # ensures that an operating system crash or power failure will not corrupt the database
  "PRAGMA locking_mode = NORMAL;",
  "PRAGMA temp_store = MEMORY;",
  "PRAGMA auto_vacuum = NONE;")
PRAGMA_settings2 <- function() c(PRAGMA_settings1(),
  "PRAGMA page_size = 65536;", # no return value
  "PRAGMA max_page_count = 2147483646;", # returns the maximum page count
  "PRAGMA foreign_keys = ON;") #no return value

set_PRAGMAs <- function(con, settings) {
  temp <- lapply(force(settings), function(x) DBI::dbExecute(con, x))
  invisible(0)
}


#' Perform a vacuum operation if there is a rollback journal present
dbVacuumRollack <- function(con, dbname) {
  frj <- paste0(dbname, "-journal")

  if (file.exists(frj)) {
    DBI::dbExecute(con, "VACUUM")

    if (file.exists(frj)) {
      stop("'dbVacuumRollack': failed to vacuum rollback journal of ",
        shQuote(basename(dbname)))
    }
  }

  invisible(TRUE)
}

#' List tables and variables of a database
#' @export
list.dbTables <- function(dbName) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbName, flags = RSQLite::SQLITE_RO)
  res <- DBI::dbListTables(con)
  RSQLite::dbDisconnect(con)

  res
}

#' List variables of a database
#' @export
list.dbVariables <- function(dbName, dbTable) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbName, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbListFields(con, dbTable)
}

#' List tables and variables of a database
#' @export
list.dbVariablesOfAllTables <- function(dbName) {
  tables <- list.dbTables(dbName)
  sapply(tables, function(it) list.dbVariables(dbName, dbTable = it))
}


#------ End of database-IO functions
########################
