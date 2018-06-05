#' Setting global 'warn' and 'error' options
#'
#' @param debug.warn.level An integer value. Sets the \code{warn} option.
#' @param debug.dump.objects A logical value. Sets the \code{error} option.
#'  See \code{details} section.
#' @param dir_prj A character string. The path at which the \var{RData} file are saved if
#'  \code{debug.dump.objects} is turned on.
#' @param verbose A logical value.
#'
#' @return A list of length two with elements 'warn' and 'error' containing the status
#'  of these two global options before resetting them by this function.
#'
#' @section Details: Accepted values of \code{debug.warn.level} are \itemize{
#'  \item  warn < 0: warnings are ignored
#'  \item  warn = 0: warnings are stored until the top-level function returns
#'  \item  warn = 1: warnings are printed as they occur
#'  \item  warn = 2: all warnings are turned into errors.
#' }
#'  If \code{debug.dump.objects} is \code{TRUE}, then code will on error dump objects
#'  and frames to files at path \code{dir_prj}, and (if not in interactive mode) quit. To
#'  view the dumped frames first attach them with
#'      \code{load(file.path(dir_prj, "last.dump.rda"))}
#'  and then browse them with
#'      \code{debugger(`path/to/file/last.dump.rda`)}
#'
#' @export
print('Start Misc_Functions')

set_options_warn_error <- function(debug.warn.level = 1L, debug.dump.objects = FALSE,
  dir_prj = ".", verbose = FALSE) {

  ow_prev <- options("warn", "error")

  if (verbose) {
    temp_call <- shQuote(match.call()[1])
    print(paste0("rSFSW2's ", temp_call, ": set options ",
      "'warn' from ", ow_prev[["warn"]], " to ", debug.warn.level, " and 'error' to ",
      if (debug.dump.objects) "dump objects to file" else "'traceback'", "."))
  }

  options(warn = debug.warn.level)

  if (debug.dump.objects) {
    options(error = quote({
      dump_objs <- new.env()

      for (p in sys.parents()) {
        if (inherits(try(sys.frame(p), silent = TRUE), "try-error"))
          next

        items <- setdiff(ls(name = sys.frame(p)), ls(name = dump_objs))
        p_pos <- pos.to.env(sys.frame(p))

        for (it in items) {
          x <- get0(it, envir = p_pos)
          if (!is.null(x))
            assign(it, x, envir = dump_objs)
        }
      }

      save(list = ls(name = dump_objs), envir = dump_objs,
        file = file.path(dir_prj, "last.dump.save.RData"))

      dump.frames(dumpto = file.path(dir_prj, "last.dump"), to.file = TRUE)

      if (!interactive())
        q("no")
    }))

  } else {
    options(error = traceback)
  }

  ow_prev
}



#' Expression for dumping of objects from an evaluation stack
#'
#' Create an expression for functions 'f' to set \code{on.exit()} such that all objects
#' from the evaluation frame stack of function 'f' are collected and stored in a
#' \var{RData} file
#'
#' @param dir_out A character string. The path to where the \var{RData} file is dumped.
#' @param file_tag A character string. Will become final part of the \var{RData} file
#'   name.
#'
#' @return Expression.
#' @seealso \code{\link{set_options_warn_error}} with \code{debug.dump.objects = TRUE}
#'
#' @examples
#' \dontrun{
#' f2 <- function(x, cause_error = FALSE) {
#'   print(match.call())
#'   print(environment())
#'   # Enable debug dumping
#'   on.exit(enable_debug_dump(file_tag = match.call()[[1]]), add = TRUE)
#'   # Add to 'on.exit'
#'   on.exit(print(paste("exit from", match.call()[[1]])), add = TRUE)
#'
#'   res <- x + 100
#'   if (cause_error) stop("Create error and force debug dumping")
#'
#'   # Remove debug dumping but not other 'on.exit' expressions before returning without error
#'   oe <- sys.on.exit()
#'   oe <- remove_from_onexit_expression(oe, tag = "enable_debug_dump")
#'   on.exit(eval(oe), add = FALSE)
#'   # Add to 'on.exit'
#'   on.exit(print(paste("exit2 from", match.call()[[1]])), add = TRUE)
#'   res
#' }
#'
#' f1 <- function(x, cause_error) {
#'   print(paste(match.call()[[1]], x))
#'   print(environment())
#'   try(f2(x + 1, cause_error))
#' }
#'
#' f1(0, cause_error = FALSE)
#' f1(0, cause_error = TRUE)
#' x <- new.env()
#' load("last.dump.f2.RData", envir = x)
#' ls.str(x)
#'
#' # Clean up
#' unlink("last.dump.f2.RData")
#' }
#'
#' @export
enable_debug_dump <- function(dir_out = ".", file_tag = "debug") {
  {
    op_prev <- options("warn")
    options(warn = 0)
    env_tosave <- new.env()

    # Loop through evaluation frame stack, with global environment and
    # without 'enable_debug_dump', and collect objects
    ids_frame <- sys.parents()[-1]
    for (k in ids_frame) {
      list2env(as.list(sys.frame(sys.parent(k))), envir = env_tosave)
    }
    list2env(as.list(globalenv()), envir = env_tosave)

    save(list = ls(envir = env_tosave), envir = env_tosave,
      file = file.path(dir_out, paste0("last.dump.", as.character(file_tag), ".RData")))
    options(op_prev)
  }
}

#' Remove one of possibly several expressions recorded by \code{on.exit}
#'
#' @param sysonexit An expression. The returned value of a call to \code{sys.on.exit()}
#'   from inside the calling function.
#' @param tag A character string. An string identifying which of the recorded expressions
#'   should be removed.
#' @seealso \code{\link{enable_debug_dump}} for examples
#' @export
remove_from_onexit_expression <- function(sysonexit, tag) {
  if (!is.null(sysonexit) && nchar(tag) > 0) {
    sysonexit[regexpr(tag, sysonexit) < 0]
  } else {
    sysonexit
  }
}

has_nodata <- function(data, tag = NULL, MARGIN = 1) {
  if (is.null(tag)) {
    apply(data, MARGIN, function(x) all(is.na(x)))
  } else {
    apply(data[, grepl(tag, colnames(data)), drop = FALSE], MARGIN, function(x) all(is.na(x)))
  }
}

has_incompletedata <- function(data, tag = NULL, MARGIN = 1) {
  if (is.null(tag)) {
    apply(data, MARGIN, anyNA)
  } else {
    apply(data[, grepl(tag, colnames(data)), drop = FALSE], MARGIN, anyNA)
  }
}

#custom list.dirs function because the ones in 2.13 and 2.15 are different... this function will behave like the one in 2.15 no matter which version you are using...
#note: should work on any system where the directory seperator is .Platform$file.sep (ie Unix)
list.dirs2 <- function(path, full.names = TRUE, recursive = TRUE) {
  dir.list <- list.dirs(path, full.names)

  if (is.null(dir.list))
    return (dir.list)
  if (length(dir.list) == 0)
    return (dir.list)
  if (recursive == TRUE)
    return (dir.list)

  nSlash = length(strsplit(dir.list[1], .Platform$file.sep)[[1]]) + 1
  if (nSlash == 1)
    return(dir.list[-1])

  n = length(dir.list)
  for (i in n:1)
    if (length(strsplit(dir.list[i], .Platform$file.sep)[[1]]) != nSlash)
      dir.list <- dir.list[-i]

  dir.list
}
#custom file.copy2 function, b/c it was giving errors on JANUS when run with MPI
file.copy2 <- function(from = "", to = "", overwrite = TRUE, copy.mode = TRUE, times = 0) {
  file.copy(from, to, overwrite, FALSE, copy.mode)
  if (times < 24)
    if (file.exists(from))
      if (!file.exists(to)) {
        print("trying to copy the file again")
        Recall(from, to, overwrite, copy.mode, (times+1))  #recursively call the function again because when run with MPI the file copying doesn't seem to work everytime...
      }
  #else { #this commented out part copies the file via the system command cp
  #  if (any(grepl("/", to, fixed = TRUE))) { #this part makes the to directory if it doesn't exist... so pretty much this can copy files to places that don't exist, which generally isn't what you want to do but in this case it might help solve an error I keep getting.
  #    y <- to
  #    while (substr(y, nchar(y), nchar(y)) != '/')
  #      y <- substr(y, 1, nchar(y)-1)
  #    y <- substr(y, 1, nchar(y)-1)
  #    if (y != "")
  #      system(paste("mkdir -p", y), ignore.stdout = FALSE, ignore.stderr = FALSE)
  #  }
  #  command <- "cp" #this just calls the system command cp...
  #  if (overwrite == TRUE) command <- paste(command, "-f")
  #  if (copy.mode == TRUE) command <- paste(command, "-p")
  #  system(paste(command, from, to), ignore.stdout = FALSE, ignore.stderr = FALSE)
  #}
}
# made this function b/c dir.create wasn't always working correctly on JANUS for some
# reason...
dir.create2 <- function(path, showWarnings = TRUE, recursive = FALSE, mode = "0777") {
  k <- 0
  temp <- NULL
  temp_call <- shQuote(match.call()[1])

  repeat {
    temp <- dir.create(path, showWarnings, recursive, mode)

    if (k > 24 || dir.exists(path))
      break

    k <- k + 1
  }

  if (showWarnings && k > 0) {
    # Iteratively call the function b/c when run on JANUS with MPI it doesn't seem to
    # make the directories everytime... quite aggravating.
    print(paste0("rSFSW2's ", temp_call, ": failed to create ",
      shQuote(path), " during ", k, " attempt(s)"))
  }

  temp
}

#copy directory and content as in system(paste("cp -R", shQuote(from), shQuote(to)))
dir.copy <- function(dir.from, dir.to, overwrite = FALSE) {
  dir.create2(dir.to, recursive = TRUE)
  dir.list <- basename(list.dirs2(dir.from, full.names = FALSE, recursive = FALSE))
  file.list <- list.files(dir.from)
  if (length(dir.list) > 0) {
    sapply(dir.list, function(x) {
      dir.copy(dir.from = file.path(dir.from, x), dir.to = file.path(dir.to, x),
      overwrite = overwrite)
    })
    #file.list <- file.list[-match(dir.list, table = file.list)] #this line gives an error when run in R v. 2.13
    file.list <- file.list[file.list != dir.list] #this line does the same as the other line, but does not throw the error
  }
  if (length(file.list) > 0) {
    sapply(file.list, function(x) {file.copy2(from = file.path(dir.from, x), to = file.path(dir.to, x), overwrite = overwrite, copy.mode = TRUE)})
  }
  invisible(1)
}

#' Create the elements of paths
#'
#' This is a wrapper for the function \code{\link{dir.create}} using different default
#' settings and allowing multiple path names as input. The function checks if the
#' provided paths may be valid names and catches any other errors with \code{try}.
#'
#' @param paths A list or vector of strings. Path names to be created.
#' @inheritParams base::dir.create
#'
#' @return An invisible list of length equal to the length of \code{paths}. The elements
#'   are \code{NULL} for invalid elements of \code{paths}, a logical value for the
#'   elements of \code{paths} with a successful calls to \code{\link{dir.create}}, or
#'   an object of class \code{try-error} for a failed call to \code{\link{dir.create}}.
#'
#' @seealso \code{\link{dir.create}}
#' @export
dir_safe_create <- function(paths, showWarnings = FALSE, recursive = TRUE, mode = "0777") {
  temp <- lapply(paths, function(path) {
      if (!is.null(path) && !is.na(path) && is.character(path) && nchar(path) > 0 &&
        !dir.exists(path))

        try(dir.create2(path, showWarnings = showWarnings, recursive = recursive,
          mode = mode), silent = TRUE)
    })

  invisible(temp)
}

future_time_window_aggregation <- function(sim_time){
  # Add future time windows for aggregation of simulation output
  sim_time[["agg_years"]] <- c(sim_time[["agg_years.current1"]],
                               future = apply(future_yrs, 1, function(x) x["DSfut_startyr"]:x["DSfut_endyr"])
  )
  
  # Time windows of current and future simulation periods
  sim_time[["sim_windows"]] <- c(
    current = list(sim_time[["simstartyr"]]:sim_time[["endyr"]]),
    future = apply(future_yrs, 1, function(x) x["DSfut_startyr"]:x["DSfut_endyr"])
  )
}


degree_days <- function(temp_C, base_C = 0) {
  res <- temp_C - base_C
  res[res < 0] <- 0

  res
}

soil_status <- function(..., swp_crit, time_N, is_dry = TRUE) {
  swp <- list(...)
  if (is.null(names(swp)))
    names(swp) <- paste0("V", seq_along(swp))

  out <- list()
  mat_crits <- list()
  ncols <- vapply(swp, function(x) dim(x)[2], FUN.VALUE = NA_real_)

print("'soil_status' needs fixing")
#TODO(drs)
#  for (k in seq_along(swp)) {
#    if (k > 1 &&
#    mat_crit <- array(swp_crit, dim = c(length(swp_crit), time_N))
#
#
#  mat_crit1 <- matrix(swp_crit, nrow = time_N, ncol = length(swp_crit), byrow = TRUE)

  if (is_dry) {
    lapply(swp, function(x) x < mat_crit)
  } else {
    lapply(swp, function(x) x >= mat_crit)
  }
}

sw_dailyC4_TempVar <- function(dailyTempMin, dailyTempMean, simTime2, return_yearly = FALSE) {
  #Variables to estimate percent C4 species in North America: Teeri JA, Stowe LG (1976) Climatic patterns and the distribution of C4 grasses in North America. Oecologia, 23, 1-12.

  temp7 <- simTime2$month_ForEachUsedDay_NSadj == 7
  Month7th_MinTemp_C <- tapply(dailyTempMin[temp7], simTime2$year_ForEachUsedDay_NSadj[temp7], min)
  LengthFreezeFreeGrowingPeriod_Days <- tapply(dailyTempMin, simTime2$year_ForEachUsedDay_NSadj,
    function(x) {
      temp <- rle(x > 0)
      if (any(temp$values)) max(temp$lengths[temp$values], na.rm = TRUE) else 0
    })

  temp_base65F <- degree_days(dailyTempMean - 18.333, 0) # 18.333 C = 65 F with (65 - 32) * 5 / 9
  DegreeDaysAbove65F_DaysC <- tapply(temp_base65F, simTime2$year_ForEachUsedDay_NSadj, sum)

  nyrs <- seq_along(Month7th_MinTemp_C) #if southern Hemisphere, then 7th month of last year is not included
  ydat <- cbind(Month7th_MinTemp_C[nyrs],
                LengthFreezeFreeGrowingPeriod_Days[nyrs],
                DegreeDaysAbove65F_DaysC[nyrs])
  dimnames(ydat) <- list(NULL,
                        c("Month7th_NSadj_MinTemp_C",
                          "LengthFreezeFreeGrowingPeriod_NSadj_Days",
                          "DegreeDaysAbove65F_NSadj_DaysC")
                        )

  if (return_yearly) {
    yrs_have <- as.integer(names(Month7th_MinTemp_C[nyrs]))

    if (all(yrs_have == simTime$useyrs)) {
      res <- cbind(Years = yrs_have, ydat)

    } else {
      res <- matrix(NA, nrow = simTime$no.useyr, ncol = 4,
                    dimnames = list(NULL, c("Years", colnames(ydat))))
      res[, "Years"] <- simTime$useyrs
      res[, -1] <- ydat[match(simTime$useyrs, yrs_have), ]
    }

  } else {
    res <- c(apply(ydat, 2, mean), apply(ydat, 2, sd))
    names(res) <- c(colnames(ydat), paste0(colnames(ydat), ".sd"))
  }

  res
}

#' Calculate climate variables from daily weather
#'
#' @param weatherList A list. Each element is an object of class
#'   \linkS4class{swWeatherData} containing daily weather data of a specific year.
#' @param year.start An integer value. The first year of the range of years for which
#'   climate variables should be calculated.
#' @param year.end An integer value. The last year of the range of years for which
#'   climate variables should be calculated.
#' @param do.C4vars A logical value. If \code{TRUE} then additional output is returned.
#' @param simTime2 An object as returned from function
#'   \code{\link{simTiming_ForEachUsedTimeUnit}}. Only needed if \code{isTRUE(do.C4vars)}.
#'
#' @return A list with named elements \itemize{
#'   \item{\var{\dQuote{meanMonthlyTempC}}} {A numeric vector of length 12.
#'    Mean monthly mean daily air temperature in degree Celsius.}
#'   \item{\var{\dQuote{minMonthlyTempC}}} {A numeric vector of length 12.
#'     Mean monthly minimum daily air temperature in degree Celsius.}
#'   \item{\var{\dQuote{maxMonthlyTempC}}} {A numeric vector of length 12.
#'     Mean monthly maximum daily air temperature in degree Celsius.}
#'   \item{\var{\dQuote{meanMonthlyPPTcm}}} {A numeric vector of length 12.
#'     Mean monthly precipitation in centimeters.}
#'   \item{\var{\dQuote{MAP_cm}}} {A numeric value. Mean annual precipitation in
#'     centimeters.}
#'   \item{\var{\dQuote{MAT_C}}} {A numeric value. Mean annual air temperature in
#'     degree Celsius.}
#'   \item{\var{\dQuote{dailyTempMin}}} {A numeric vector. If \code{isTRUE(do.C4vars)},
#'     then minimum daily air temperature in degree Celsius for each day of time period
#'     between \code{year.start} and \code{year.end}. If \code{!isTRUE(do.C4vars)}, then
#'     \code{NA}.}
#'   \item{\var{\dQuote{dailyTempMean}}} {A numeric vector. Similar as for
#'     \code{dailyTempMin} but for mean daily air temperature.}
#'   \item{\var{\dQuote{dailyC4vars}}} {If \code{isTRUE(do.C4vars)}, then a named
#'     numeric vector containing the output of \code{\link{sw_dailyC4_TempVar}}, else
#'     \code{NA}.}
#' }
#' @export
calc_SiteClimate <- function(weatherList, year.start, year.end, do.C4vars = FALSE,
  simTime2 = NULL) {

  x <- rSOILWAT2::dbW_weatherData_to_dataframe(weatherList)

  # Trim to years
  years <- as.numeric(unlist(lapply(weatherList, function(x) x@year)))
  years <- years[year.start <= years & year.end >= years]

  x <- x[year.start <= x[, "Year"] & year.end >= x[, "Year"], ]
  xl <- list(
          months = as.POSIXlt(seq(from = ISOdate(years[1], 1, 1, tz = "UTC"),
                                 to = ISOdate(years[length(years)], 12, 31, tz = "UTC"),
                                 by = "1 day"))$mon + 1,
          Tmean_C = rowMeans(x[, c("Tmax_C", "Tmin_C")])
        )

  index <- xl[["months"]] + 100 * x[, "Year"]
  temp <- vapply(list(xl[["Tmean_C"]], x[, "Tmin_C"], x[, "Tmax_C"]), function(data)
    matrix(tapply(data, index, mean), nrow = 12),
    FUN.VALUE = matrix(NA_real_, nrow = 12, ncol = length(years)))
  tempPPT <- matrix(tapply(x[, "PPT_cm"], index, sum), nrow = 12)

  list(
    meanMonthlyTempC = apply(temp[, , 1, drop = FALSE], 1, mean),
    minMonthlyTempC = apply(temp[, , 2, drop = FALSE], 1, mean),
    maxMonthlyTempC = apply(temp[, , 3, drop = FALSE], 1, mean),
    meanMonthlyPPTcm = apply(tempPPT, 1, mean),

    MAP_cm = sum(tempPPT) / length(years),
    MAT_C = mean(xl[["Tmean_C"]]),

    dailyTempMin = if (do.C4vars) x[, "Tmin_C"] else NA,
    dailyTempMean = if (do.C4vars) xl[["Tmean_C"]] else NA,
    dailyC4vars = if (do.C4vars) {
        sw_dailyC4_TempVar(dailyTempMin = x[, "Tmin_C"], dailyTempMean = xl[["Tmean_C"]],
          simTime2)
      } else NA
  )
}



#functions wet and dry periods

#' Saturation vapor pressure
#'
#' @param T A numeric vector of temperature(s) (deg C)
#' @return A numeric vector of length \code{T} of saturation vapor pressure (\var{kPa}) at
#'    temperature T
#' @references Yoder, R. E., L. O. Odhiambo, and W. C. Wright. 2005. Effects of
#'    Vapor-Pressure Deficit and Net-Irradiance Calculation Methods on Accuracy of
#'    Standardized Penman-Monteith Equation in a Humid Climate Journal of Irrigation and
#'    Drainage Engineering 131:228-237.
vp0 <- function(T) {
  0.6108 * exp(17.27 * T / (T + 273.3))  # eq. 5 of Yoder et al. 2005
}


#' Vapor pressure deficit
#'
#' @param Tmin A numeric vector of daily minimum temperature(s) (deg C)
#' @param Tmax A numeric vector of daily maximum temperature(s) (deg C)
#' @param RHmean A numeric vector of daily mean relative humidity (percentage)
#' @return A numeric vector of length \code{T} of vapor pressure deficit (\var{kPa})
#' @references Yoder, R. E., L. O. Odhiambo, and W. C. Wright. 2005. Effects of
#'    Vapor-Pressure Deficit and Net-Irradiance Calculation Methods on Accuracy of
#'    Standardized Penman-Monteith Equation in a Humid Climate Journal of Irrigation and
#'    Drainage Engineering 131:228-237.
vpd <- function(Tmin, Tmax, RHmean = NULL) {
  if (is.null(RHmean)) {
    (vp0(Tmax) - vp0(Tmin)) / 2  # eq. 6 - eq. 13 of Yoder et al. 2005 (VPD6 in Table 4)
  } else {
    (vp0(Tmax) + vp0(Tmin)) / 2 * (1 - RHmean / 100)  # eq. 6 - eq. 11 of Yoder et al. 2005 (VPD4 in Table 4)
  }
}




max_duration <- function(x, target_val = 1L, return_doys = FALSE) {
  r <- rle(x)
  rgood <- r$values == target_val
  igood <- which(rgood)

  if (length(igood) > 0) {
    len <- max(r$lengths[igood])

    if (return_doys) {
      imax <- which(rgood & r$lengths == len)[1]

      rdoys <- cumsum(r$lengths)
      doys <- if (imax == 1L) {
          c(start = 1L, end = rdoys[1])
        } else {
          c(start = rdoys[imax - 1] + 1,
            end = rdoys[imax])
        }
    }

  } else {
    len <- 0L
    doys <- c(start = NA, end = NA)
  }

  if (return_doys)
    return(c(len, doys))

  len
}

startDoyOfDuration <- function(x, duration = 10) {
  r <- rle(x)
  if (length(r$lengths) == 1 | sum(r$values == 1 & r$lengths >= duration) == 0) {
    return (ifelse((length(r$lengths) == 1 & (r$values == 0 | r$lengths<duration)) | sum(r$values == 1 & r$lengths >= 10) == 0, NA, 1)[1])
  } else {
    first10dry <- r$lengths[which(r$values == 1 & r$lengths >= duration)][1] #pick first period
    if (!is.na(first10dry)) {
      ind <- which(r$lengths == first10dry & r$values == 1)[1] #always pick start of first suitable period
    } else {
      ind <- -1
    }
    if (ind == 1) {#start of period at beginning of year
      return(1)
    } else if (ind == -1) {#no period this year
      return(NA)
    } else {
      return(cumsum(r$lengths)[ind-1]+1)
    }
  }
}

endDoyAfterDuration <- function(x, duration = 10) {
  r <- rle(x)
  if (length(r$lengths) == 1 | sum(r$values == 1 & r$lengths >= duration) == 0) {
    return (ifelse((length(r$lengths) == 1 & (r$values == 0 | r$lengths<duration)) | sum(r$values == 1 & r$lengths >= duration) == 0, 365, NA)[1])
  } else {
    last10dry <- (rl <- r$lengths[which(r$values == 1 & r$lengths >= duration)])[length(rl)] #pick last period
    if (length(last10dry) > 0) {
      ind <- (temp <- which(r$lengths == last10dry & r$values == 1))[length(temp)]  #always pick end of last suitable period
    } else {
      ind <- -1
    }
    if (ind == -1) {#no period this year
      return(NA)
    } else {
      return(cumsum(r$lengths)[ind])
    }
  }
}

dailyRegeneration_bySWPSnow_ThisYear_YN <- function(x, opts) {
  # calculate season doys
  snowcover <- ifelse(x[,2] > 0, 1, 0)
  r <- rle(snowcover)
  rseries <- ifelse(r$values==0, 1:length(r$values), 0)
  then <- which(rseries==rseries[rseries>0][which.max(r$lengths[rseries>0])])

  sstart <- if (inherits(opts[["season.start"]], "character")) {
      # calculate last day of the longest snowpack
      if (then == 1) 1 else cumsum(r$lengths)[then - 1]
    } else {
      opts[["season.start"]]
    }
  send <- if (inherits(opts[["season.end"]], "character")) {
      # calculate first day of the longest snowpack
      min(c(cumsum(r$lengths)[then]+1, length(snowcover)))
    } else {
      opts[["season.end"]]
    }

  i_season <- sstart:send
  if(length(i_season) > 0){
    swp.season <- x[i_season,1]
    gs <- rle(ifelse(swp.season >= opts[["germination.swp.surface"]], 1, 0))
    es <- rle(ifelse(swp.season >= opts[["establishment.swp.surface"]] , 1, 0))

    reg <- 0
    # get vector of establishment starts and ends
    establishment.start.dos <- establishment.end.dos <- NULL
    for(esi in 1:length(es$lengths)){
      if(es$lengths[esi] >= opts[["establishment.duration"]] & es$values[esi] > 0){
        establishment.start.dos <- c(establishment.start.dos, ifelse(esi == 1, 1, cumsum(es$lengths)[esi-1]+1))
        establishment.end.dos <- c(establishment.end.dos, cumsum(es$lengths)[esi])
      }
    }

    # check if any germination period matches up with an establishment period
    if(length(establishment.end.dos) > 0){
      for(gsi in 1:length(gs$lengths)){
        if(gs$lengths[gsi] >= opts[["germination.duration"]] & gs$values[gsi] > 0){
          germination.start.dos <- ifelse(gsi == 1, 1, cumsum(gs$lengths)[gsi-1]+1)
          germination.end.dos <- cumsum(gs$lengths)[gsi]
          if( any( ((germination.start.dos + opts[["germination.duration"]] >= establishment.start.dos) &
                    (germination.start.dos + opts[["germination.duration"]] + opts[["establishment.duration"]] <= establishment.end.dos)) |
                  ((germination.end.dos + opts[["establishment.swp.surface"]]  >= establishment.start.dos) &
                    (germination.end.dos + opts[["establishment.swp.surface"]]  + opts[["establishment.duration"]] <= establishment.end.dos)) ) ){
            reg <- reg + 1
          }
        }
      }
    }

  } else {
    reg <- 0
  }

  reg > 0
}


#' Calculates temperate dryland criteria
#'
#' @param annualPPT A numeric vector. Annual precipitation values.
#' @param annualPET A numeric vector. Annual potential evapotranspiration values.
#'  The values must be in the same units as those of \code{annualPPT}, e.g., \code{mm}.
#' @param monthlyTemp A numeric vector. Monthly mean air temperature in degree Celsius
#'  for each year for which precipitation and PET values are provided.
#' @param ai_limit A numeric value. Used for return item \code{criteria_12}.
#'
#' @references
#' Deichmann, U. & L. Eklundh. 1991. Global digital datasets for land degradation
#'   studies: a GIS approach. Global Environment Monitoring System (GEMS), United Nations
#'   Environment Programme (UNEP), Nairobi, Kenya.
#' Trewartha G.T., Horn L.H. (1980) An introduction to climate. McGraw-Hill, New York,
#'   page 284: Temperate Areas
#'
#' @return
#'  A list with three items: UN-aridity index (numeric value), temperateness
#'  (logical value), and temperate drylands (logical value).
calc_drylandindices <- function(annualPPT, annualPET, monthlyTemp, ai_limit = 0.5) {
  ai <- annualPPT / annualPET  #Deichmann, U. & L. Eklundh. 1991. Global digital datasets for land degradation studies: a GIS approach. Global Environment Monitoring System (GEMS), United Nations Environment Programme (UNEP), Nairobi, Kenya.
  temp <- matrix(monthlyTemp >= 10, nrow = 12)
  temp <- .colSums(temp, nrow(temp), ncol(temp))
  TD <- temp >= 4 & temp < 8 #Trewartha & Horn 1980, page 284: temperate areas
  criteria12 <- as.integer(TD & ai < ai_limit)

  list(ai = ai, temperateness = TD, criteria12 = criteria12)
}


extreme_values_and_doys <- function(x, na.rm = FALSE) {
  tmax <- max(x, na.rm = na.rm)
  tmin <- min(x, na.rm = na.rm)

  c(tmax, tmin,
    circ_mean(which(abs(x - tmax) < SFSW2_glovars[["tol"]]), int = 365, na.rm = na.rm),
    circ_mean(which(abs(x - tmin) < SFSW2_glovars[["tol"]]), int = 365, na.rm = na.rm))
}


regenerationThisYear_YN <- function(x, params) {
  reg <- 0

  # calculate season doys
  snowcover <- ifelse(x[, 2] > 0, 1, 0)
  r <- rle(snowcover)
  rseries <- ifelse(r$values == 0, seq_along(r$values), 0)
  temp <- rseries > 0

  if (any(temp)) {
    # check that at least some days without snow
    then <- which(rseries == rseries[temp][which.max(r$lengths[temp])])

    if (typeof(params[["season.start"]]) == "character") {
      #calculate last day of the longest snowpack
      params[["season.start"]] <- if (then == 1) {
          1
        } else {
          cumsum(r$lengths)[then - 1]
        }
    }

    if (typeof(params[["season.end"]]) == "character") {
      #calculate first day of the longest snowpack
      params[["season.end"]] <- min(c(cumsum(r$lengths)[then] + 1, length(snowcover)))
    }

    ids <- params[["season.start"]]:params[["season.end"]]

    if (length(ids) > 0) {
      swp.season <- x[ids, 1]
      gs <- rle(as.integer(swp.season >= params[["germination.swp.surface"]]))
      es <- rle(as.integer(swp.season >= params[["establishment.swp.surface"]]))

      # get vector of establishment starts and ends
      establishment.start.dos <- establishment.end.dos <- NULL
      for (esi in seq_along(es$lengths)) {
        if (es$lengths[esi] >= params[["establishment.duration"]] && es$values[esi] > 0) {
          establishment.start.dos <- c(establishment.start.dos,
            if (esi == 1) 1 else {cumsum(es$lengths)[esi - 1] + 1})
          establishment.end.dos <- c(establishment.end.dos, cumsum(es$lengths)[esi])
        }
      }

      # check if any germination period matches up with an establishment period
      if (length(establishment.end.dos) > 0) {
        for (gsi in seq_along(gs$lengths)) {
          if (gs$lengths[gsi] >= params[["germination.duration"]] && gs$values[gsi] > 0) {
            germination.start.dos <- if (gsi == 1) 1 else {cumsum(gs$lengths)[gsi - 1] + 1}
            germination.end.dos <- cumsum(gs$lengths)[gsi]

            temp0 <- germination.start.dos + params[["germination.duration"]]
            temp1 <- germination.end.dos + params[["establishment.delay"]]
            temp <- (temp0 >= establishment.start.dos &
                temp0 + params[["establishment.duration"]] <= establishment.end.dos) |
              (temp1 >= establishment.start.dos &
                temp1 + params[["establishment.duration"]] <= establishment.end.dos)

            if (any(temp)) {
              reg <- reg + 1
            }
          }
        }
      }
    }
  }
  # else all(!temp) => no snow-free days: no regeneration this year


  if (reg > 0) 1 else 0
}




#' Function to extrapolate windspeeds measured at a height different from the
#' 2-m above ground that are assumed by \pkg{SOILWAT2}
#'
#' Based on equation 33 in Allen et al. 2005. Note: "For wind measurements above surfaces
#'  other than clipped grass, the user should apply the full logarithmic equation B.14".
#'
#' @param uz A numeric vector. Windspeed [m/s] at \code{height}.
#' @param height A numeric value. Height above ground at which \code{uz} windspeed was
#'  measured.
#'
#' @return Windspeed [m/s] at a height of 2 m above ground.
#'
#' @references Allen R.G., Walter I.A., Elliott R., Howell T., Itenfisu D., Jensen M.
#'  (2005) The ASCE standardized reference evapotranspiration equation. ASCE-EWRI Task
#'  Committee Report.
adjust.WindspeedHeight <- function(uz, height) {

  stopifnot(all(uz >= 0) && height >= 2)
  uz * 4.87 / log(67.8 * height - 5.42)
}


get.LookupFromTable <- function(pattern, trtype, tr_input, sw_input_use, sw_input, nvars) {
  nruns <- NROW(sw_input)
  if (length(trtype) == 1L && nruns > 1L)
    trtype <- rep(trtype, nruns)
  stopifnot(length(trtype) == nruns)

  # extract data from table by type
  ids <- match(trtype, rownames(tr_input), nomatch = NA)
  res <- tr_input[ids, seq_len(nvars), drop = FALSE]

  # identify columns with relevant data
  icols_in <- grep(pattern, names(sw_input_use))
  icols_res <- which(apply(!is.na(res), 2L, any))
  stopifnot(length(icols_in) >= max(icols_res))
  seq1_icols_in <- icols_in[icols_res]
  seq2_icols_in <- icols_in[-icols_res]

  # add data to datafiles and set the use flags
  sw_input[, seq1_icols_in] <- res[, icols_res]
  sw_input_use[seq1_icols_in] <- TRUE

  if (length(seq2_icols_in) > 0) {
    sw_input[, seq2_icols_in] <- NA
    sw_input_use[seq2_icols_in] <- FALSE
  }

  list(sw_input_use = sw_input_use,
       sw_input = sw_input)
}

fill_empty <- function(data, pattern, fill) {
  stopifnot(names(data) %in% c("sw_input", "sw_input_use"))

  icols <- sapply(data, function(x) grep(pattern, names(x)))
  stopifnot(identical(icols[, "sw_input_use"], icols[, "sw_input"]))
  icols <- icols[, "sw_input_use"]

  for (k in icols) {
    iempty <- is.na(data$sw_input[, k])
    if (any(iempty)) {
      data$sw_input[iempty, k] <- fill
      data$sw_input_use[k] <- TRUE
    }
  }

  data
}


#--put information from experimental design into appropriate input variables; create_treatments and the _use files were already adjusted for the experimental design when files were read in/created
transferExpDesignToInput <- function(x, i_exp, df_exp, df_exp_use) {
  temp <- match(names(df_exp)[df_exp_use], names(x), nomatch = 0)
  ctemp <- temp[!(temp == 0)]
  if (length(ctemp) > 0) {
    cexp <- match(names(x)[ctemp], names(df_exp), nomatch = 0)
    x[ctemp] <- df_exp[i_exp, cexp]
  }
  x
}


#data is the values for one year adj for SWPcrit_MPa; TRUE == dry
EventDistribution <- function(data, N, size) {
  bins <- rep(0, times = N)
  temp <- rle(data)
  temp <- temp$lengths[temp$values]
  if (length(temp) > 0) for (z in seq_along(temp)) {
    ix <- findInterval(temp[z], size)
    bins[ix] <- bins[ix] + 1
  }
  bins
}

daily_spells_permonth <- function(x, simTime2) {
  temp <- tapply(x,
    simTime2$month_ForEachUsedDay_NSadj + 100 * simTime2$year_ForEachUsedDay_NSadj,
    function(xm) {
      temp <- rle(xm)
      if (any(temp$values)) {
        mean(temp$lengths[temp$values], na.rm = TRUE)
      } else {
        NA
      }
    })

  matrix(temp, nrow = 12)
}

tabulate_values_in_bins <- function(x, method = c("duration", "values"),
  vcrit = NULL, bins, nbins, simTime, simTime2) {
  method <- match.arg(method)

  bins.summary <- (seq_len(nbins) - 1) * bins

  dat <- if (method == "duration" && is.logical(x)) {
      # bin duration of TRUE runs
      lapply(simTime$useyrs, function(y) {
        temp <- rle(x[simTime2$year_ForEachUsedDay == y])
        temp <- floor((temp$lengths[temp$values] - 1) / bins) * bins
        findInterval(temp, vec = bins.summary)
      })
    } else if (method == "values") {
      # bin values
      lapply(simTime$useyrs, function(y) {
        temp <- x[simTime2$year_ForEachUsedDay == y]
        if (!is.null(vcrit)) temp <- temp[temp > vcrit]
        floor(temp / bins) * bins
        findInterval(temp, vec = bins.summary)
      })
    } else {
      print("'tabulate_values_in_bins' cannot be calculated")
      NULL
    }

  if (length(unlist(dat)) > 0) {
    counts.summary <- sapply(dat, function(x)
      tabulate(x, nbins = length(bins.summary)))
    eventsPerYear <- apply(counts.summary, 2, sum)
    freq.summary <- sweep(counts.summary, 2, STATS = eventsPerYear, FUN = "/")
    rm(counts.summary)

  } else {
    freq.summary <- matrix(0, nrow = length(bins.summary), ncol = simTime$no.useyr)
    eventsPerYear <- rep(0, simTime$no.useyr)
  }

  list(eventsPerYear = eventsPerYear, freq.summary = freq.summary)
}



benchmark_BLAS <- function(platform, seed = NA) {
  if (grepl("darwin", platform)) { # apparently this works only on Mac OS X
    dir_r <- file.path(Sys.getenv()[["R_HOME"]], "R")
    blas <- system2(command = dir_r, args = "CMD config BLAS_LIBS", stdout = TRUE)
    blas <- sub("-L/", "/", strsplit(blas, split = " ")[[1]][1])
    lapack <- system2(command = dir_r, args = "CMD config LAPACK_LIBS", stdout = TRUE)
    lapack <- sub("-L/", "/", strsplit(lapack, split = " ")[[1]][1])
    get_ls <- if (identical(blas, lapack)) list(blas) else list(blas, lapack)
    temp <- lapply(get_ls, FUN = function(x) print(system2(command = "ls", args = paste("-l", x), stdout = TRUE)))

    print("Check linked BLAS library:") # http://simplystatistics.org/2016/01/21/parallel-blas-in-r/#
    if (!is.na(seed)) set.seed(seed)
    temp <- system.time({
      x <- replicate(5e3, stats::rnorm(5e3))
      tcrossprod(x)
    })
    print(temp)

    # Example values:
    # Apple's Accelerate framework:
    #   user  system elapsed
    # 14.755   0.268   3.423

    # ATLAS 3.10.2_2:
    #   user  system elapsed
    # 22.218   0.647   3.340

    # Built-in reference BLAS:
    #   user  system elapsed
    # 59.289   0.465  59.173

  } else {
    print(paste("'benchmark_BLAS' does not benchmark the linked BLAS library on platform",
      shQuote(platform)))
  }
}


#' Converts units of precipitation data
#'
#' @param x A numeric vector. Precipitation data as monthly series in units of
#'   \code{unit_from}.
#' @param dpm A numeric vector. Number of days per month in the time series \code{x}.
#' @param unit_from A character string. Units of data in \code{x}. Currently, supported
#'   units include "mm/month", "mm month-1", "mm/d", "mm d-1", "kg/m2/s", "kg m-2 s-1",
#'   "mm/s", "mm s-1", "cm/month", "cm month-1".
#' @param unit_to A character string. Units to which data are converted. Currently,
#'   supported unit is "cm month-1" respectively "cm/month".
#'
#' @return A numeric vector of the same size as \code{x} in units of \code{unit_to}.
#' @export
convert_precipitation <- function(x, dpm, unit_from, unit_to = "cm month-1") {
  if (!(unit_to %in% c("cm/month", "cm month-1"))) {
    stop("'convert_precipitation': only converts to units of 'cm month-1'")
  }

  if (unit_from %in% c("mm/month", "mm month-1")) {
    x / 10

  } else if (unit_from %in% c("mm/d", "mm d-1")) {
    x * dpm / 10

  } else if (unit_from %in% c("cm/d", "cm d-1")) {
    x * dpm

  } else if (unit_from %in% c("kg/m2/s", "kg m-2 s-1", "mm/s", "mm s-1")) {
    x * dpm * 8640

  } else if (unit_from %in% c("cm/month", "cm month-1")) {
    x

  } else {
    stop("Unknown precipitation unit: ", unit_from)
  }
}

#' Converts units of temperature data
#'
#' @param x A numeric vector. Temperature data as monthly series in units of
#'   \code{unit_from}.
#' @param unit_from A character string. Units of data in \code{x}. Currently, supported
#'   units include "K", "F", and "C".
#' @param unit_to A character string. Units to which data are converted. Currently,
#'   supported unit is "C".
#'
#' @return A numeric vector of the same size as \code{x} in units of \code{unit_to}.
#' @export
convert_temperature <- function(x, unit_from, unit_to = "C") {
  if (!identical(unit_to, "C")) {
    stop("'convert_temperature': only converts to units of degree Celsius")
  }

  if (identical(unit_from, "K")) {
    x - 273.15

  } else if (identical(unit_from, "F")) {
    (x - 32) * 0.5555556

  } else if (identical(unit_from, "C")) {
    x

  } else {
    stop("Unknown temperature unit: ", unit_from)
  }
}




convert_to_todo_list <- function(x) {
  temp <- matrix(x, ncol = 2, nrow = length(x) / 2, byrow = TRUE)
  todo <- lapply(temp[, 2], function(x) as.logical(as.numeric(x)))
  names(todo) <- temp[, 1]

  todo
}



setup_scenarios <- function(sim_scens, future_yrs) {
  #--- Create complete scenario names
  # make sure 'ambient' is not among models
  temp <- grep(sim_scens[["ambient"]], sim_scens[["models"]],
    invert = TRUE, value = TRUE)

  if (length(temp) > 0) {
    # add (multiple) future_yrs
    temp <- paste0(rownames(future_yrs), ".", rep(temp, each = nrow(future_yrs)))
    # add (multiple) downscaling.method
    temp <- paste0(sim_scens[["method_DS"]], ".",
      rep(temp, each = length(sim_scens[["method_DS"]])))
  }

  # make sure 'ambient' is first entry
  id <- c(sim_scens[["ambient"]], temp)
  N <- length(id)

  if (N > 1) {
    #--- Create table with scenario name parts for each scenario
    temp <- strsplit(id[-1], split = ".", fixed = TRUE)
    if (!all(lengths(temp) == 4L))
      stop("'climate.conditions' are mal-formed: they must contain 4 elements that are ",
        "concatenated by '.'")

    climScen <- data.frame(matrix(unlist(temp), nrow = N - 1, ncol = 4, byrow = TRUE),
      stringsAsFactors = FALSE)
    # ConcScen = concentration scenarios, e.g., SRESs, RCPs
    colnames(climScen) <- c("Downscaling", "DeltaStr_yrs", "ConcScen", "Model")
    # see 'setup_simulation_time' for how 'future_yrs' is created
    climScen[, "Delta_yrs"] <- as.integer(substr(climScen[, "DeltaStr_yrs"], 2,
      nchar(climScen[, "DeltaStr_yrs"]) - 3))

    #--- List unique sets of requested scenario name parts
    reqMs <- unique(climScen[, "Model"])
    reqCSs <- unique(climScen[, "ConcScen"])
    reqCSsPerM <- lapply(reqMs, function(x)
      unique(climScen[x == climScen[, "Model"], "ConcScen"]))
    reqDSsPerM <- lapply(reqMs, function(x)
      unique(climScen[x == climScen[, "Model"], "Downscaling"]))

  } else {
    # Only ambient scenario
    climScen <- reqMs <- reqCSs <- reqCSsPerM <- reqDSsPerM <- NULL
  }

  c(sim_scens, list(id = id, N = N, df = climScen, reqMs = reqMs,
    reqCSs = reqCSs, reqCSsPerM = reqCSsPerM, reqDSsPerM = reqDSsPerM))
}

setup_mean_daily_output_requests <- function(req_mean_daily, opt_agg) {
  N <- length(req_mean_daily)

  if (N > 0) {
    req_mean_daily <- sort(req_mean_daily)

    temp <- req_mean_daily == "SWAbulk"
    if (any(temp) && opt_agg[["SWPcrit_N"]] > 0) {
      req_mean_daily <- req_mean_daily[!temp]
      req_mean_daily <- c(req_mean_daily, paste0("SWAbulkatSWPcrit",
        abs(round(-1000 * opt_agg[["SWPcrit_MPa"]], 0)), "kPa"))

      N <- length(req_mean_daily)
    }
  }

  list(tag = req_mean_daily, N = N)
}

setup_aggregation_options <- function(opt_agg, ...) {
  more_options <- list(...)

  for (k in names(more_options)) {
    opt_agg[[k]] <- more_options[[k]]
  }

  opt_agg[["SWPcrit_N"]] <- length(opt_agg[["SWPcrit_MPa"]])

  opt_agg
}


get_datasource_includefield <- function(SWRunInformation, field_include, sim_size) {
  if (field_include %in% names(SWRunInformation)) {
    temp <- SWRunInformation[sim_size[["runIDs_sites"]], field_include]
    is.na(temp) | temp > 0
  } else {
    rep(TRUE, sim_size[["runsN_sites"]])
  }
}


get_datasource_masterfield <- function(SWRunInformation, field_sources, sim_size,
  how_determine_sources) {

  sites_source <- rep(NA, times = sim_size[["runsN_sites"]])
  i_cns_field <- which(field_sources == colnames(SWRunInformation))
  has_cns_field <- length(i_cns_field) > 0

  if (how_determine_sources == "SWRunInformation" && has_cns_field) {
    sites_source <- SWRunInformation[sim_size[["runIDs_sites"]], i_cns_field]
  } else if (how_determine_sources == "order" || !has_cns_field) {
  } else {
    message("Value of 'how_determine_sources'", how_determine_sources,
      " not implemented")
  }

  sites_source
}

update_datasource_masterfield <- function(MMC, sim_size, SWRunInformation, fnames_in,
  field_sources, field_include) {

  notDone <- NULL

  if (any(MMC[["idone"]])) {
    SWRunInformation[sim_size[["runIDs_sites"]], field_sources] <- as.character(MMC[["source"]])

    notDone <- is.na(MMC[["source"]])
    include_YN_data <- rep(0, sim_size[["runsN_master"]])
    include_YN_data[sim_size[["runIDs_sites"]][!notDone]] <- 1
    SWRunInformation[, field_include] <- include_YN_data

    #write data to disk
    utils::write.csv(SWRunInformation, file = fnames_in[["fmaster"]], row.names = FALSE)
    unlink(fnames_in[["fpreprocin"]])

    if (any(notDone))
      print(paste0(shQuote(field_sources), ": no data available for n = ", sum(notDone),
        " sites."))

  } else {
      print(paste0(shQuote(field_sources), ": no data extracted because already available"))
  }

  SWRunInformation
}

season_diff_NS <- function(simTime2, t_unit = "day") {
  switch(t_unit,
    day = ,
    days = simTime2$doy_ForEachUsedDay_NSadj[1] - simTime2$doy_ForEachUsedDay[1],
    month = ,
    months = simTime2$month_ForEachUsedMonth_NSadj[1] - simTime2$month_ForEachUsedMonth[1],

    stop("'season_diff_NS': unknown time unit"))
}


#' Check availability and version of a command-line tool
#'
#' The function throws an error if the command-line tool cannot be run and its version
#' queried by a call to \code{\link{system2}}. Otherwise, the function compares the
#' return version value with the argument \code{v_expected}. If it does not match and
#' the argument \code{stop_on_mismatch} has a \code{TRUE} value, then an error is thrown
#' with a suitable message; otherwise, a warning is issued.
#'
#' @param tool A character string. The name of the command-line tool.
#' @param v_expected A character string that is or can be converted to represent the
#'  expected version of the command-line tool.
#' @param stop_on_mismatch A logical value.
#' @seealso \code{\link{system2}}
#'
#' @return An invisible \code{TRUE}.
#' @export
check_cltool <- function(tool, v_expected, stop_on_mismatch = FALSE) {
  v_expected <- numeric_version(v_expected)
  fun_calling <- sys.call(sys.parent())[[1]]

  temp <- try(system2(tool, args = "--version", stdout = TRUE, stderr = TRUE))

  if (inherits(temp, "try-error")) {
    stop(shQuote(fun_calling), ": tool ", shQuote(tool), " cannot be found.")

  } else {
    v_has <- numeric_version(gsub("[^([:digit:][:punct:])]", "", temp[2]))

    txt <- if (v_has > v_expected) {
        .makeMessage(shQuote(fun_calling), ": expects ", shQuote(tool), " version ",
          shQuote(v_expected), " but found version ", shQuote(v_has), "; this may work, ",
          "but likely the code of ", shQuote(fun_calling), " must be updated to work ",
          "with newer versions of ", shQuote(tool), " properly.")

      } else if (v_has < v_expected) {
        .makeMessage(shQuote(fun_calling), ": expects ", shQuote(tool), " version ",
          shQuote(v_expected), " but found an older version ", shQuote(v_has), "; this ",
          "may work, but likely ", shQuote(tool), " must be updated to work with ",
          shQuote(fun_calling), "properly.")
      } else NULL

    if (!is.null(txt)) {
      if (stop_on_mismatch) {
        stop(txt)
      } else {
        warning(txt)
      }
    }
  }

  invisible(TRUE)
}

check_cltool <- memoise::memoise(check_cltool)
