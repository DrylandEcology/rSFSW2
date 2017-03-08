#---------------------------------------------------------------------------------------#

#------CODE developed and written by
# - Daniel R Schlaepfer (dschlaep@uwyo.edu, drs): 2009-2016
#for contact and further information see also: sites.google.com/site/drschlaepfer

#------DISCLAIMER: This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#---------------------------------------------------------------------------------------#

#' Function to produce an aggregating function
#'
#' @param agg_fun_defs A data.frame with two columns: 'id' of type integer and 'agg_fun'
#'  of type character. See \code{Details}.
#' @param circular A logical value. If \code{TRUE} then circular functions are used.
#'
#' @details
#'  The aggregation functions are determined based on the entries in the column 'agg_fun'.
#'  Currently, implemented functions are \code{mean}, \code{sd}, \code{quantile},
#'  \code{median}, \code{mad}, and \code{yearly}, respectively, their circular counterparts.
#'  The probability values, e.g., \code{X1, X2}, at which quantiles are calculated,
#'  are entered as "quantile_X1", and "quantile_X2". \code{yearly} will return the values
#'  of each year, i.e., \code{x} without aggregation (across years).
#'  The column 'id' is the identifier which connects the aggregated output to the table
#'  'aggregating_functions' of the output database.
#'
#' @return
#'  A function/closure with arguments \code{x, na.rm, omit_yearly, return_ids, ...} or,
#'  if \code{circular}, \code {x, int, na.rm, omit_yearly, return_ids, ...}.
#'  If \code{omit_yearly}, then the output of the \code{yearly} function is not returned.
#'  If \code{return_ids}, then the function returns a data.frame object
#'  with two columns \code{aggfun_id} and \code{x}. The column \code{aggfun_id} contains
#'  the values of the identifier 'id' and the column \code{x} contains the returned
#'  value(s) of the aggregating functions applied to the input argument \code{x}.
#'  If \code{return_ids} is \code{FALSE}, then the function returns what would be
#'  otherwise be the column \code{x} as numeric vector.
#'
#' @examples
#' d <- data.frame(id = 1:3, agg_fun = c("mean", "quantile_0.5", "median"))
#' f <- create_aggregation_function(d)
#' x <- c(1:10, 1, NA)
#' f(x, na.rm = TRUE)
create_aggregation_function <- function(agg_fun_defs, circular = FALSE) {
  envf <- new.env()

  #--- Create list of functions to be applied to data
  listf <- list()

  # Aggregation: mean
  itemp <- grepl("mean", agg_fun_defs[, "agg_fun"], ignore.case = TRUE)
  if (any(itemp))
    listf <- c(listf,
      list(mean = list(
        fun = if (circular) {
            function(x, int, na.rm = FALSE, ...) circ_mean(x, int = int, na.rm = na.rm)
          } else {
            function(x, na.rm = FALSE, ...) mean(x, na.rm = na.rm, ...)
          },
        aggfun_id = agg_fun_defs[itemp, "id"])))

  # Aggregation: sd
  itemp <- grepl("SD", agg_fun_defs[, "agg_fun"], ignore.case = TRUE)
  if (any(itemp))
    listf <- c(listf,
      list(SD = list(
        fun = if (circular) {
            function(x, int, na.rm = FALSE, ...) circ_sd(x, int = int, na.rm = na.rm)
          } else {
            function(x, na.rm = FALSE, ...) stats::sd(x, na.rm = na.rm)
          },
        aggfun_id = agg_fun_defs[itemp, "id"])))

  # Aggregation: quantiles
  itemp <- grepl("quantile", agg_fun_defs[, "agg_fun"], ignore.case = TRUE)
  if (any(itemp)) {
    probs <- grep("quantile", agg_fun_defs[, "agg_fun"], ignore.case = TRUE, value = TRUE)
    probs <- as.numeric(gsub("quantile_", "", probs, fixed = TRUE))
    assign("probs", probs, envir = envf)

    if (circular) {
      tempf <- function(x, int, na.rm = FALSE, ...) {}
      body(tempf) <- substitute({
        fargs <- list(x = x, int = int, na.rm = na.rm, probs = probs)
        dots <- list(...)
        if (!any("type" == names(dots)))
          dots[["type"]] <- 8
        fargs <- c(fargs, dots)
        do.call("circ_quantile", args = fargs)
      }, envf)
    } else {
      tempf <- function(x, na.rm = FALSE, ...) {}
      body(tempf) <- substitute({
        fargs <- list(x = x, na.rm = na.rm, probs = probs)
        dots <- list(...)
        if (!any("type" == names(dots)))
          dots[["type"]] <- 8
        fargs <- c(fargs, dots)
        do.call(stats::quantile, args = fargs)
      }, envf)
    }

    listf <- c(listf,
      list(quantile = list(
        fun = tempf,
        aggfun_id = agg_fun_defs[itemp, "id"])))
  }

  # Aggregation: median
  itemp <- grepl("median", agg_fun_defs[, "agg_fun"], ignore.case = TRUE)
  if (any(itemp))
    listf <- c(listf,
      list(median = list(
        fun = if (circular) {
            function(x, int, na.rm = FALSE, ...) circ_median(x, int = int, na.rm = na.rm)
          } else {
            function(x, na.rm = FALSE, ...) stats::median(x, na.rm = na.rm)
          },
        aggfun_id = agg_fun_defs[itemp, "id"])))

  # Aggregation: mad
  itemp <- grepl("mad", agg_fun_defs[, "agg_fun"], ignore.case = TRUE)
  if (any(itemp))
    listf <- c(listf,
      list(mad = list(
        fun = if (circular) {
            function(x, int, na.rm = FALSE, ...) circ_mad(x, int = int, na.rm = na.rm)
          } else {
            function(x, na.rm = FALSE, ...) stats::mad(x, na.rm = na.rm)
          },
        aggfun_id = agg_fun_defs[itemp, "id"])))

  # Aggregation: yearly
  yearly_id <- -1L
  itemp <- grepl("yearly", agg_fun_defs[, "agg_fun"], ignore.case = TRUE)
  if (any(itemp)) {
    yearly_id <- agg_fun_defs[itemp, "id"]

    listf <- c(listf,
      list(yearly = list(
        fun = function(x, na.rm = FALSE, ...) x,
        aggfun_id = yearly_id)))
  }
  assign("yearly_id", yearly_id, envir = envf)

  # Copy list of aggregation functions to environment used in substitute call to create closure
  assign("listf", listf, envir = envf)


  #---Create the function/closure
  # 1) part: formals, i.e. input arguments
  if (circular) {
    f <- function(x, int, na.rm = FALSE, omit_yearly = FALSE, return_ids = FALSE, ...) {}
    fargs <- quote(list(x = x, int = int, na.rm = na.rm, ...))
  } else {
    f <- function(x, na.rm = FALSE, omit_yearly = FALSE, return_ids = FALSE, ...) {}
    fargs <- quote(list(x = x, na.rm = na.rm, ...))
  }
  assign("fargs", fargs, envir = envf)
  # 2) part: environment; make it empty to have as little baggage as possible
  environment(f) <- new.env()
  # 3) part: body
  body(f) <- substitute({
    res <- lapply(listf, function(f) {
      agg <- do.call(f$fun, args = fargs)
      id <- if (length(f$aggfun_id) == 1) {
          rep(f$aggfun_id, length(agg))
        } else {
          f$aggfun_id
        }
      list(aggfun_id = id, x = agg)
    })

    out <- unlist(lapply(res, function(x) x$x), use.names = FALSE)
    if (omit_yearly || return_ids) {
      aggfun_ids <- unlist(lapply(res, function(x) x$aggfun_id))
    }

    if (omit_yearly && yearly_id > 0) {
      temp <- !(yearly_id == aggfun_ids)
      out <- out[temp]
      aggfun_ids <- aggfun_ids[temp]
    }

    if (return_ids) {
      cbind(aggfun_id = aggfun_ids, x = out)
    } else {
      out
    }

  }, envf)

  compiler::cmpfun(f)
}


setup_aggregations <- function(SFSW2_prj_meta) {
  agg_funs <- SFSW2_prj_meta[["req_out"]][["agg_funs"]]
  agg_fun_options <- SFSW2_prj_meta[["req_out"]][["agg_fun_options"]]
  agg_years <- SFSW2_prj_meta[["sim_time"]][["agg_years"]]

  aggs <- list()

  # Aggregation functions
  agg_fun_names1 <- names(agg_funs)[as.logical(agg_funs[sapply(agg_funs, is.logical)])]
  if (length(agg_fun_names1) == 0)
    stop("There must be at least one aggregating function included in 'agg_funs'")

  it <- which("quantile" == agg_fun_names1)
  if (length(it) > 0) {
    probs <- agg_fun_options[["quantile"]][["probs"]]
    if (length(probs) == 0 || any(probs < 0) || any(probs > 1) || !is.finite(probs))
      stop("If the aggregating function 'quantile' is selected, then the 'probs' entry ",
          "of the 'quantile' options in 'agg_fun_options' must be set correctly.")

    agg_fun_names <- c(
      if (it > 1) agg_fun_names1[1:(it - 1)],
      paste("quantile", format(probs), sep = "_"),
      if (it < length(agg_fun_names1)) agg_fun_names1[(it + 1):length(agg_fun_names1)]
    )
  } else {
    agg_fun_names <- agg_fun_names1
  }

  agg_fun_types <- rep(NA, length(agg_fun_names))
  agg_fun_types[grepl("yearly", agg_fun_names, ignore.case = TRUE)] <- "yearly"
  agg_fun_types[grepl("(mean)|(median)", agg_fun_names, ignore.case = TRUE)] <- "central"
  agg_fun_types[grepl("(sd)|(mad)|(quantile)", agg_fun_names, ignore.case = TRUE)] <- "variation"
  itemp <- grep("quantile", agg_fun_names, ignore.case = TRUE)
  if (length(itemp) > 0) {
    probs <- as.numeric(gsub("quantile_", "", agg_fun_names[itemp], fixed = TRUE))
    i50 <- which(abs(probs - 0.5) < tol)
    if (length(i50) > 0) {
      agg_fun_types[itemp[i50]] <- "central"
    }
  }

  aggs[["agg_fun_defs"]] <- data.frame(id = seq_len(agg_fun_names),
    agg_fun = agg_fun_names, type = agg_fun_types)

  # Prepare aggregation functions
  aggs[["agg_fun"]] <- create_aggregation_function(aggs[["agg_fun_defs"]],
    circular = FALSE)
  aggs[["agg_fun_circular"]] <- create_aggregation_function(aggs[["agg_fun_defs"]],
    circular = TRUE)

  # Aggregation time windows
  agg_years_bad <- sapply(agg_years, function(x) any(!(diff(x) == 1)))
  if (any(agg_years_bad))
    stop("Aggregation time windows must be continuous sequences of years; ",
        "check: ", paste0(names(agg_years)[agg_years_bad], collapse = ", "))
  agg_years_bad <- sapply(agg_years, function(x)
    all(sapply(SFSW2_prj_meta[["sim_time"]][["sim_windows"]], function(sw)
    length(setdiff(x, sw)) > 0)))
  if (any(agg_years_bad))
    stop("Aggregation time windows are set outside simulation time windows; ",
        "check: ", paste0(names(agg_years)[agg_years_bad], collapse = ", "))

  # column names of 'agg_windows' are used in part 2 to set up table
  # 'aggregation_timewindows' of outputDB
  aggs[["agg_windows"]] <- as.data.frame(matrix(NA, nrow = length(agg_years), ncol = 4,
    dimnames = list(NULL, c("id", "label", "agg_start", "agg_end"))))
  aggs[["agg_windows"]][, "id"] <- seq_len(agg_years)
  aggs[["agg_windows"]][, "label"] <- names(agg_years)
  aggs[["agg_windows"]][, c("agg_start", "agg_end")] <- t(sapply(agg_years, function(x)
    range(x)))

  aggs
}


