#------------------------------------------------------------------------------#

#------CODE developed and written by
# - Daniel R Schlaepfer (dschlaep@uwyo.edu, drs): 2009-2016
# for contact and further information see also:
# \url{sites.google.com/site/drschlaepfer}

#------DISCLAIMER: This program is distributed in the hope that it will be
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#------------------------------------------------------------------------------#

#' Add new field(s) to a table in \var{dbOutput} that is/are based on a
#' calculation of values from (an) existing field(s)
#'
#' @param dbOut_fname A character string. The path to the output database.
#' @param table A character string. The table name to which the new field(s)
#'   should be appended.
#' @param vars_orig A vector of character strings. The existing field names
#'   that are used by \code{FUN} to calculate \code{vars_new}.
#' @param vars_new A vector of character strings. The names of new fields.
#'   The number must match the number of columns returned by \code{FUN}.
#' @param FUN A function. See details.
#' @param ... Additional named arguments to \code{FUN}. See details.
#' @param verbose A logical value.
#' @param chunk_size An integer value.
#'
#' @section Details: The first argument of \code{FUN} must be a two-dimensional
#'   object. This object contains the extracted values from \code{dbOut_fname},
#'   i.e., it has up to \code{chunk_size} rows and the columns are
#'   \code{vars_orig}. Additional arguments can be passed via \code{...}.
#'   The function must return a value (or values) corresponding to
#'   \code{vars_new} for each row. These values are inserted into the new
#'   field(s).
#'
#' @return The function is called for its side-effects on \code{dbOut_fname}.
#'
#' @examples
#' # Prepare databse
#' dbOut_tmp <- tempfile(fileext = ".sqlite")
#' con <- dbConnect(SQLite(), dbOut_tmp)
#' data(iris)
#' x <- data.frame(P_id = seq_len(nrow(iris)), iris)
#' dbWriteTable(con, "iris", x)
#'
#' # Define calculation function
#' vars_orig <- c("Sepal.Length", "Sepal.Width")
#' example_calc <- function(x, delta = 1, ...) {
#'   apply(x, MARGIN = 1, function(x) delta * prod(x))
#' }
#'
#' # Create new field based on a calculation
#' dbOutput_add_calculated_field(
#'   dbOut_fname = dbOut_tmp,
#'   table = "iris",
#'   vars_orig = vars_orig,
#'   vars_new = "calc",
#'   FUN = example_calc, delta = 2)
#'
#' # Check the new field
#' xout <- dbReadTable(con, "iris")
#' res2 <- example_calc(x[, vars_orig], delta = 2)
#' all.equal(xout[, "calc"], res2)
#'
#' # Cleanup
#' dbDisconnect(con)
#' unlink(dbOut_tmp)
#'
#' @export
dbOutput_add_calculated_field <- function(dbOut_fname, table,
  vars_orig, vars_new, FUN, ..., verbose = FALSE, chunk_size = 1e5) {

  #--- Preparations
  con <- dbConnect(SQLite(), dbname = dbOut_fname)
  on.exit(dbDisconnect(con), add = TRUE)

  tableq <- dbQuoteIdentifier(con, table)
  vars_newq <- dbQuoteIdentifier(con, vars_new)
  vars_origq <- dbQuoteIdentifier(con, vars_orig)

  has_fields <- dbListFields(con, tableq)

  # Check that `vars_orig` are available
  stopifnot(vars_orig %in% has_fields)

  # Check new variable(s) don't already exist
  stopifnot(!(vars_new %in% has_fields))


  #--- Add new variables as empty fields
  sql <- paste(
    "ALTER TABLE", tableq,
    "ADD COLUMN", paste0(vars_newq, " REAL", collapse = ", "))

  dbExecute(con, sql)


  #--- Calculate new variable(s)

  # Loop over chunks, extract `vars_orig`, calculate `vars_new`, and insert
  sql <- paste("SELECT \"P_id\" FROM", tableq)
  pids <- as.integer(dbGetQuery(con, sql)[, 1])
  n_todos <- length(pids)
  do_chunks <- parallel::splitIndices(n_todos, ceiling(n_todos / chunk_size))

  # Prepare SQL statement to extract `vars_orig`
  sql_get <- paste(
    "SELECT", paste(vars_origq, collapse = ", "),
    "FROM", tableq,
    "WHERE \"P_id\" IN (:pids_chunk)",
    "ORDER BY \"P_id\"")

  # Prepare SQL statements to insert `vars_new`
  sql_put <- paste0(
    "UPDATE ", tableq, " ",
    "SET (", paste0(vars_newq, collapse = ", "), ") = (:res) ",
    "WHERE P_id = :pids_chunk")

  # Loop over chunks
  for (k in seq_along(do_chunks)) {
    if (verbose) {
      print(paste0(Sys.time(), ": step ", k, "/", length(do_chunks)))
    }

    dbWithTransaction(con, {
      # Extract data
      rs_get <- dbSendStatement(con, sql_get)
      dbBind(rs_get, params = list(pids_chunk = pids[do_chunks[[k]]]))
      x <- dbFetch(rs_get)
      dbClearResult(rs_get)

      # Calculate
      res <- do.call(FUN, args = list(x, ...))

      # Store in new variable(s)
      rs_put <- dbSendStatement(con, sql_put)
      dbBind(rs_put, params = list(
        res = res,
        pids_chunk = pids[do_chunks[[k]]]))
      dbClearResult(rs_put)
    })
  }
}
