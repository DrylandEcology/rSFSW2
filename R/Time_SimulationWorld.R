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
#'      microbenchmark::microbenchmark(month1(), month2())    # barely any difference
#'  }
seq_month_ofeach_day <- function(from = list(year = 1900, month = 1, day = 1),
  to = list(year = 1900, month = 12, day = 31), tz = "UTC") {

  x <- paste(from[[1]], from[[2]], from[[3]], 12, 0, 0, sep = "-")
  from0 <- unclass(as.POSIXct.POSIXlt(strptime(x, "%Y-%m-%d-%H-%M-%OS", tz = tz)))
  x <- paste(to[[1]], to[[2]], to[[3]], 12, 0, 0, sep = "-")
  to0 <- unclass(as.POSIXct.POSIXlt(strptime(x, "%Y-%m-%d-%H-%M-%OS", tz = tz)))

  res <- seq.int(0, to0 - from0, by = 86400) + from0
  as.POSIXlt.POSIXct(.POSIXct(res, tz = tz))$mon + 1
}


setup_simulation_time <- function(sim_time, add_st2 = FALSE,
  adjust_NS = FALSE) {

  #simyrs <- simstartyr:endyr
  #no.simyr <- endyr - simstartyr + 1
  if (is.null(sim_time[["spinup_N"]])) {
    sim_time[["spinup_N"]] <- sim_time[["startyr"]] - sim_time[["simstartyr"]]

  } else {
    sim_time[["startyr"]] <- getStartYear(sim_time[["simstartyr"]], sim_time[["spinup_N"]])
  }

  stopifnot(!is.null(sim_time[["spinup_N"]]), !is.null(sim_time[["simstartyr"]]),
    !is.null(sim_time[["startyr"]]), !is.null(sim_time[["endyr"]]))

  if (is.matrix(sim_time[["future_yrs"]])) {
    stopifnot(dim(sim_time[["future_yrs"]])[2] == 3)

  } else if (is.list(sim_time[["future_yrs"]]) &&
    all(lengths(sim_time[["future_yrs"]]) == 3)) {

    ctemp <- c("delta", "DSfut_startyr", "DSfut_endyr")
    temp <- matrix(unlist(sim_time[["future_yrs"]]), ncol = length(ctemp), byrow = TRUE,
      dimnames = list(NULL, ctemp))
    rownames(temp) <- make.names(paste0("d", temp[, "delta"], "yrs"), unique = TRUE)
    sim_time[["future_yrs"]] <- temp

  } else {
    stop("'setup_simulation_time': incorrect format of 'future_yrs'")
  }

  sim_time[["future_N"]] <- dim(sim_time[["future_yrs"]])[1]

  temp <- ISOdate(sim_time[["startyr"]], 1, 1, tz = "UTC")
  discarddy <- as.numeric(temp - ISOdate(sim_time[["simstartyr"]], 1, 1, tz = "UTC"))

  sim_time[["useyrs"]] <- sim_time[["startyr"]]:sim_time[["endyr"]]

  sim_time[["no.useyr"]] <- sim_time[["endyr"]] - sim_time[["startyr"]] + 1
  sim_time[["no.usemo"]] <- sim_time[["no.useyr"]] * 12
  sim_time[["no.usedy"]] <- as.numeric(ISOdate(sim_time[["endyr"]], 12, 31, tz = "UTC") - temp) + 1

  sim_time[["index.useyr"]] <- sim_time[["spinup_N"]] + seq_len(sim_time[["no.useyr"]])
  sim_time[["index.usemo"]] <- sim_time[["spinup_N"]] * 12 + seq_len(sim_time[["no.usemo"]])
  sim_time[["index.usedy"]] <- discarddy + seq_len(sim_time[["no.usedy"]])

  if (add_st2) {
    sim_time[["sim_time2_North"]] <- simTiming_ForEachUsedTimeUnit(sim_time,
      sim_tscales = c("daily", "monthly", "yearly"), latitude = 90,
      account_NorthSouth = adjust_NS)

    if (adjust_NS) {
      sim_time[["sim_time2_South"]] <- simTiming_ForEachUsedTimeUnit(sim_time,
        sim_tscales = c("daily", "monthly", "yearly"), latitude = -90,
        account_NorthSouth = TRUE)

    } else {
      sim_time[["sim_time2_South"]] <- sim_time[["sim_time2_North"]]
    }
  }

  sim_time
}

simTiming_ForEachUsedTimeUnit <- function(st,
  sim_tscales = c("daily", "weekly", "monthly", "yearly"), latitude = 90,
  account_NorthSouth = TRUE) { #positive latitudes -> northern hemisphere; negative latitudes -> southern hemisphere

  res <- list()

  if (any(sim_tscales == "daily")) {
    temp <- as.POSIXlt(seq(from = ISOdate(min(st$useyrs), 1, 1, tz = "UTC"),
                           to = ISOdate(max(st$useyrs), 12, 31, tz = "UTC"),
                           by = "1 day"))

    res$doy_ForEachUsedDay <- res$doy_ForEachUsedDay_NSadj <- temp$yday + 1
    res$month_ForEachUsedDay <- res$month_ForEachUsedDay_NSadj <- temp$mon + 1
    res$year_ForEachUsedDay <- res$year_ForEachUsedDay_NSadj <- temp$year + 1900

    if (latitude < 0 && account_NorthSouth) {
      dshift <- as.POSIXlt(ISOdate(st$useyrs, 6, 30, tz = "UTC"))$yday + 1  #new month either at end of year or in the middle because the two halfs (6+6 months) of a year are of unequal length (182 (183 if leap year) and 183 days): I chose to have a new month at end of year (i.e., 1 July -> 1 Jan & 30 June -> 31 Dec; but, 1 Jan -> July 3/4): and instead of a day with doy = 366, there are two with doy = 182
      res$doy_ForEachUsedDay_NSadj <- unlist(lapply(seq_along(st$useyrs), function(x) {
        temp <- res$doy_ForEachUsedDay[st$useyrs[x] == res$year_ForEachUsedDay]
        c(temp[-(1:dshift[x])], temp[1:dshift[x]])
      }))
      res$month_ForEachUsedDay_NSadj <- strptime(paste(res$year_ForEachUsedDay, res$doy_ForEachUsedDay_NSadj, sep = "-"), format = "%Y-%j")$mon + 1
      temp <- length(res$year_ForEachUsedDay)
      delta <- if (dshift[1] == 182) 2 else 3
      res$year_ForEachUsedDay_NSadj <- c(
        rep(st$useyrs[1] - 1, times = dshift[1] + delta),
        res$year_ForEachUsedDay[-((temp - dshift[1] - delta):temp)]
      )
    }
  }

  if (any(sim_tscales == "weekly")) {

  }

  if (any(sim_tscales == "monthly")) {
    res$yearno_ForEachUsedMonth <- res$yearno_ForEachUsedMonth_NSadj <- rep(seq_len(st$no.useyr), each = 12)
    res$month_ForEachUsedMonth <- res$month_ForEachUsedMonth_NSadj <- rep(SFSW2_glovars[["st_mo"]], times = st$no.useyr)

    if (latitude < 0 && account_NorthSouth) {
      res$month_ForEachUsedMonth_NSadj <- (res$month_ForEachUsedMonth + 5) %% 12 + 1
    }
  }

  if (any(sim_tscales == "yearly")) {

  }

  res
}
