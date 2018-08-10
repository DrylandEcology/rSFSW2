#------ FUNCTIONS THAT DEAL WITH SIMULATION TIME


getStartYear <- function(simstartyr, spinup_N = 1L) {
  as.integer(simstartyr + spinup_N)
}



isLeapYear <- function(y) {
  #from package: tis
  y %% 4 == 0 & (y %% 100 != 0 | y %% 400 == 0)
}

#' The sequence of month numbers for each day in the period from - to
#'
#' @examples
#'  month1 <- function() as.POSIXlt(seq(from = ISOdate(1980, 1, 1, tz = "UTC"),
#'     to = ISOdate(2010, 12, 31, tz = "UTC"), by = "1 day"))$mon + 1
#'  month2 <- function() seq_month_ofeach_day(list(1980, 1, 1),
#'    list(2010, 12, 31), tz = "UTC")
#'
#' \dontrun{
#'    if (requireNamespace("microbenchmark", quietly = TRUE))
#'      # barely any difference
#'      microbenchmark::microbenchmark(month1(), month2())
#'  }
seq_month_ofeach_day <- function(from = list(year = 1900, month = 1, day = 1),
  to = list(year = 1900, month = 12, day = 31), tz = "UTC") {

  x <- paste(from[[1]], from[[2]], from[[3]], 12, 0, 0, sep = "-")
  from0 <- unclass(as.POSIXct.POSIXlt(strptime(x, "%Y-%m-%d-%H-%M-%OS",
    tz = tz)))
  x <- paste(to[[1]], to[[2]], to[[3]], 12, 0, 0, sep = "-")
  to0 <- unclass(as.POSIXct.POSIXlt(strptime(x, "%Y-%m-%d-%H-%M-%OS", tz = tz)))

  res <- seq.int(0, to0 - from0, by = 86400) + from0
  as.POSIXlt.POSIXct(.POSIXct(res, tz = tz))$mon + 1
}


#' Determine maximal span of simulation years across all experimental and design
#' treatments
#'
#' @param st An object as returned from the function
#'   \code{\link{setup_simulation_time}}.
#' @param SFSW2_prj_inputs An object as returned from function
#'   \code{\link{process_inputs}}.
#' @return The object \code{st} augmented with two named elements \itemize{
#'   \item \code{overall_simstartyr} which is the earliest year requested by any
#'   input \item \code{overall_endyr} which is the last year requested by any
#'   input }
get_simulation_time <- function(st, SFSW2_prj_inputs) {

  stopifnot(!is.null(st[["simstartyr"]]), !is.null(st[["endyr"]]))
  use_treat <- SFSW2_prj_inputs[["sw_input_treatments_use"]]
  use_exp <- SFSW2_prj_inputs[["sw_input_experimentals_use"]]

  if (any(SFSW2_prj_inputs[["create_treatments"]] == "YearStart")) {
    temp_tr <- if (use_treat["YearStart"]) {
        SFSW2_prj_inputs[["sw_input_treatments"]][, "YearStart"]
      } else NA

    temp_exp <- if (use_exp["YearStart"]) {
        SFSW2_prj_inputs[["sw_input_experimentals"]][, "YearStart"]
      } else NA

    st[["overall_simstartyr"]] <- min(st[["simstartyr"]], temp_tr, temp_exp,
      na.rm = TRUE)

  } else {
    st[["overall_simstartyr"]] <- st[["simstartyr"]]
  }

  if (any(SFSW2_prj_inputs[["create_treatments"]] == "YearEnd")) {
    temp_tr <- if (use_treat["YearEnd"]) {
        SFSW2_prj_inputs[["sw_input_treatments"]][, "YearEnd"]
      } else NA

    temp_exp <- if (use_exp["YearEnd"]) {
        SFSW2_prj_inputs[["sw_input_experimentals"]][, "YearEnd"]
      } else NA

    st[["overall_endyr"]] <- max(st[["endyr"]], temp_tr, temp_exp, na.rm = TRUE)

  } else {
    st[["overall_endyr"]] <- st[["endyr"]]
  }

  st
}

#' Describe the time of a simulation project
#'
#' @param sim_time A list with at least values for three named elements:
#'   \var{\dQuote{simstartyr}} and \var{\dQuote{endyr}} and one of the following
#'   two: \var{\dQuote{startyr}} or \var{\dQuote{spinup_N}}.
#' @param add_st2 A logical value. If \code{TRUE}, the output of calling the
#'   function \code{\link{simTiming_ForEachUsedTimeUnit}} is appended to the
#'   returned list.
#' @param use_doy_range A logical value. If \code{TRUE}, then the result is
#'   additional daily indices indicating whether the \var{DOY} is within the
#'   days indicated in the \code{doy_ranges}.
#' @param doy_ranges A named list. Aggregation output variables and the daily
#'   \code{c(min, max)} of days you wish to calculate the aggregation over.
#' @param adjust_NS A logical value. If \code{TRUE}, then the result is
#'   corrected for locations on the southern vs. northern hemisphere. Only used
#'   if \code{add_st2} is \code{TRUE}.
#' @param A named list, i.e., the updated version of \code{sim_time}.
setup_simulation_time <- function(sim_time, add_st2 = FALSE,
  adjust_NS = FALSE, use_doy_range = FALSE, doy_ranges = list()) {

  if (is.null(sim_time[["spinup_N"]])) {
    sim_time[["spinup_N"]] <- sim_time[["startyr"]] - sim_time[["simstartyr"]]

  } else {
    sim_time[["startyr"]] <- getStartYear(sim_time[["simstartyr"]],
      sim_time[["spinup_N"]])
  }

  stopifnot(!is.null(sim_time[["spinup_N"]]),
    !is.null(sim_time[["simstartyr"]]),
    !is.null(sim_time[["startyr"]]),
    !is.null(sim_time[["endyr"]]))
  if (is.matrix(sim_time[["future_yrs"]])) {
    stopifnot(dim(sim_time[["future_yrs"]])[2] == 3)

  } else if (is.list(sim_time[["future_yrs"]]) &&
    all(lengths(sim_time[["future_yrs"]]) == 3)) {

    ctemp <- c("delta", "DSfut_startyr", "DSfut_endyr")
    temp <- matrix(unlist(sim_time[["future_yrs"]]), ncol = length(ctemp),
      byrow = TRUE, dimnames = list(NULL, ctemp))
    rownames(temp) <- make.names(paste0("d", temp[, "delta"], "yrs"),
      unique = TRUE)
    sim_time[["future_yrs"]] <- temp

  } else {
    stop("'setup_simulation_time': incorrect format of 'future_yrs'")
  }

  sim_time[["future_N"]] <- dim(sim_time[["future_yrs"]])[1]

  temp <- ISOdate(sim_time[["startyr"]], 1, 1, tz = "UTC")
  discarddy <- as.numeric(temp - ISOdate(sim_time[["simstartyr"]], 1, 1,
    tz = "UTC"))

  sim_time[["useyrs"]] <- sim_time[["startyr"]]:sim_time[["endyr"]]

  sim_time[["no.useyr"]] <- sim_time[["endyr"]] - sim_time[["startyr"]] + 1
  sim_time[["no.usemo"]] <- sim_time[["no.useyr"]] * 12
  sim_time[["no.usedy"]] <- as.numeric(ISOdate(sim_time[["endyr"]], 12, 31,
    tz = "UTC") - temp) + 1

  sim_time[["index.useyr"]] <- sim_time[["spinup_N"]] +
    seq_len(sim_time[["no.useyr"]])
  sim_time[["index.usemo"]] <- sim_time[["spinup_N"]] * 12 +
    seq_len(sim_time[["no.usemo"]])
  sim_time[["index.usedy"]] <- discarddy + seq_len(sim_time[["no.usedy"]])

  if (add_st2) {
    sim_time[["sim_time2_North"]] <- simTiming_ForEachUsedTimeUnit(sim_time,
      sim_tscales = c("daily", "monthly", "yearly"),
      use_doy_range = use_doy_range,
      doy_ranges =  doy_ranges,
      latitude = 90, account_NorthSouth = adjust_NS)

    if (adjust_NS) {
      sim_time[["sim_time2_South"]] <- simTiming_ForEachUsedTimeUnit(sim_time,
        sim_tscales = c("daily", "monthly", "yearly"),
        use_doy_range = use_doy_range,
        doy_ranges = doy_ranges,
        latitude = -90,
        account_NorthSouth = TRUE)

    } else {
      sim_time[["sim_time2_South"]] <- sim_time[["sim_time2_North"]]
    }
  }

  sim_time
}


#' Calculate indices along different time steps for simulation time
#'
#'
#' @param st An object as returned from the function
#'   \code{\link{setup_simulation_time}}.
#' @param sim_tscales A vector of character strings. One or multiple of
#'   \code{c("daily", "weekly", "monthly", "yearly")}.
#' @param use_doy_range A logical value. If \code{TRUE}, then the result is
#'   additional daily indices indicating whether the \var{DOY} is within the
#'   days indicated in the \code{doy_ranges}.
#' @param doy_ranges A named list. Aggregation output variables and the daily
#'   \code{c(min, max)} of days you wish to calculate the aggregation over.
#' @param latitude A numeric value. The latitude in decimal degrees for which a
#'   hemispheric adjustment is requested; however, the code extracts only the
#'   sign. Positive values are interpreted as from the northern hemisphere;
#'   negative latitudes as from the southern hemisphere.
#' @param account_NorthSouth A logical value. If \code{TRUE}, then the result is
#'   corrected for locations on the southern vs. northern hemisphere.
#' @return A named list.
simTiming_ForEachUsedTimeUnit <- function(st,
  sim_tscales = c("daily", "weekly", "monthly", "yearly"),
  use_doy_range = FALSE,  doy_ranges = list(),
  latitude = 90, account_NorthSouth = TRUE) {

  res <- list()

  if (any(sim_tscales == "daily")) {
    temp <- as.POSIXlt(seq(from = ISOdate(min(st$useyrs), 1, 1, tz = "UTC"),
                           to = ISOdate(max(st$useyrs), 12, 31, tz = "UTC"),
                           by = "1 day"))

    res$doy_ForEachUsedDay <- temp$yday + 1
    res$month_ForEachUsedDay <- temp$mon + 1
    res$year_ForEachUsedDay <- res$year_ForEachUsedDay_NSadj <- temp$year + 1900

    if (latitude < 0 && account_NorthSouth) {
      #- Shift doys
      # New month either at end of year or in the middle because the two
      # halfs (6+6 months) of a year are of unequal length
      # (182 (183 if leap year) and 183 days): I chose to have a new month at
      # end of year (i.e., 1 July -> 1 Jan & 30 June -> 31 Dec;
      # but, 1 Jan -> July 3/4): and instead of a day with doy = 366,
      # there are two with doy = 182
      dshift <- as.POSIXlt(ISOdate(st$useyrs, 6, 30, tz = "UTC"))$yday + 1
      res$doy_ForEachUsedDay_NSadj <- unlist(lapply(seq_along(st$useyrs),
        function(x) {
          temp <- st$useyrs[x] == res$year_ForEachUsedDay
          temp1 <- res$doy_ForEachUsedDay[temp]
          temp2 <- 1:dshift[x]
          c(temp1[-temp2], temp1[temp2])
        }))

      #- Shift months
      temp <- paste(res$year_ForEachUsedDay, res$doy_ForEachUsedDay_NSadj,
        sep = "-")
      res$month_ForEachUsedDay_NSadj <- strptime(temp, format = "%Y-%j")$mon + 1

      #- Shift years
      temp1 <- length(res$year_ForEachUsedDay)
      delta <- if (dshift[1] == 182) 2 else 3
      temp2 <- dshift[1] + delta
      res$year_ForEachUsedDay_NSadj <- c(
        # add previous calendar year for shifted days of first simulation year
        rep(st$useyrs[1] - 1, times = temp2),
        # remove a corresponding number of days at end of simulation period
        res$year_ForEachUsedDay[- ((temp1 - temp2 + 1):temp1)]
      )
      res$useyrs_NSadj <- unique(res$year_ForEachUsedDay_NSadj)
      res$no.useyr_NSadj <- length(res$useyrs_NSadj)

    } else {
      res$doy_ForEachUsedDay_NSadj <- res$doy_ForEachUsedDay
      res$month_ForEachUsedDay_NSadj <- res$month_ForEachUsedDay
      res$year_ForEachUsedDay_NSadj <- res$year_ForEachUsedDay
      res$useyrs_NSadj <- st$useyrs
      res$no.useyr_NSadj <- st$no.useyr
    }

    #Adjust years to water-years
    # In North, Water year starting Oct 1 - Using DOY 274, which is Oct 1st in
    #   Leap Years, but Oct 2nd in typical years
    # In South, Water year starting April 1 - Using DOY 92, which is April 1st
    #   in Leap Years, but April 2nd in typical years

    temp <- res$doy_ForEachUsedDay[1] == res$doy_ForEachUsedDay_NSadj[1]
    FirstDOY_WaterYear <- if (temp) 274 else 92

    temp <- res$doy_ForEachUsedDay_NSadj > FirstDOY_WaterYear
    res$year_ForEachUsedDay_NSadj_WaterYearAdj <- # nolint
      res$year_ForEachUsedDay_NSadj + ifelse(temp, 1, 0)

    if (isTRUE(use_doy_range)) {
      # North or Southern hemisphere? eliminate unnecessary water years values
      if (latitude > 0) {
        Idx <- grep("_S", names(doy_ranges))
        doy_ranges[Idx] <- NULL
      } else {
        Idx <- grep("_N", names(doy_ranges))
        doy_ranges[Idx] <- NULL
      }

      for (dr in seq_along(doy_ranges)) {
        if (!is.null(doy_ranges[[dr]])) {
          # for all non-NULL doy_range values
          doy_range_values <- doy_ranges[[dr]]

          # Create daily logical vector indicating whether that doy is within
          # range or not
          res[[paste0("doy_NSadj_", names(doy_ranges[dr]), "_doyRange")]] <-
            if (doy_range_values[1] > doy_range_values[2]) {
              temp <- c(doy_range_values[1]:366, 1:doy_range_values[2])
              res$doy_ForEachUsedDay_NSadj %in% temp
            } else {
              temp <- doy_range_values[1]:doy_range_values[2]
              res$doy_ForEachUsedDay_NSadj %in% temp
            }
        }
      }
    }
  }



  if (any(sim_tscales == "weekly")) {

  }

  if (any(sim_tscales == "monthly")) {
    res$yearno_ForEachUsedMonth <- res$yearno_ForEachUsedMonth_NSadj <-
      rep(seq_len(st$no.useyr), each = 12)
    res$month_ForEachUsedMonth <- res$month_ForEachUsedMonth_NSadj <-
      rep(SFSW2_glovars[["st_mo"]], times = st$no.useyr)

    if (latitude < 0 && account_NorthSouth) {
      res$month_ForEachUsedMonth_NSadj <-
        (res$month_ForEachUsedMonth + 5) %% 12 + 1
    }
  }

  if (any(sim_tscales == "yearly")) {

  }

  res
}


#' Check requested years
#'
#' @param start_year An integer value. The requested first year to extract
#'   weather data.
#' @param end_year An integer value. The requested last year to extract weather
#'   data.
#' @param has_start_year An integer value. The available first year of the
#'   weather data.
#' @param has_end_year An integer value. The available last year of the weather
#'   data.
#' @param temp_call A character string. An identifier of the calling function
#'   used for printing.
#' @param verbose A logical value. If \code{TRUE} prints statements if first or
#'   last year were updated.
#'
#' @return A list with two named elements \itemize{ \item \code{start_year} to
#'   updated first year no smaller than \code{has_start_year} \item
#'   \code{end_year} to updated last year no larger than \code{has_end_year} }
update_requested_years <- function(start_year, end_year, has_start_year,
  has_end_year, temp_call = NULL, verbose = FALSE) {

  if (start_year < has_start_year) {
    if (verbose) {
      print(paste0(shQuote(temp_call), ": covers years ", has_start_year, "-",
        has_end_year, "; requested start year ", start_year, " was changed to ",
        has_start_year, "."))
    }
    start_year <- as.integer(has_start_year)

  } else {
    start_year <- as.integer(start_year)
  }

  if (end_year > has_end_year) {
    if (verbose) {
      print(paste0(shQuote(temp_call), ": covers years ", has_start_year, "-",
        has_end_year, "; requested end year ", end_year, " was changed to ",
        has_end_year, "."))
    }
    end_year <- as.integer(has_end_year)

  } else {
    end_year <- as.integer(end_year)
  }


  list(start_year = start_year, end_year = end_year)
}
