
set_options_warn_error <- function(debug.warn.level = 1L, debug.dump.objects = FALSE,
  dir_prj = ".") {

  ow_prev <- options("warn", "error")
  #    - warn < 0: warnings are ignored
  #    - warn = 0: warnings are stored until the top–level function returns
  #    - warn = 1: warnings are printed as they occur
  #    - warn = 2: all warnings are turned into errors
  options(warn = debug.warn.level)

  if (debug.dump.objects) {
    # dumps objects and frames to files, and (if not interactive) quits
    # Note: view dumped frames with
    # load(file.path(dir_prj, "last.dump.rda"))
    # debugger(`path/to/file/last.dump.rda`)
    options(error = quote({
      dump_objs <- new.env()

      for (p in sys.parents()) {
        if (inherits(try(sys.frame(p), silent = TRUE), "try-error"))
          next
        items <- setdiff(ls(name = sys.frame(p)), ls(name = dump_objs))
        for (it in items)
          assign(it, get(it, pos = sys.frame(p)), envir = dump_objs)
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


getStartYear <- function(simstartyr, spinup_N = 1L) {
  as.integer(simstartyr + spinup_N)
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
list.dirs2 <- function(path, full.names=TRUE, recursive=TRUE) {
  dir.list <- list.dirs(path, full.names)

  if(is.null(dir.list))
    return (dir.list)
  if(length(dir.list) == 0)
    return (dir.list)
  if(recursive == TRUE)
    return (dir.list)

  nSlash = length(strsplit(dir.list[1], .Platform$file.sep)[[1]]) + 1
  if(nSlash == 1)
    return(dir.list[-1])

  n = length(dir.list)
  for(i in n:1)
    if(length(strsplit(dir.list[i], .Platform$file.sep)[[1]]) != nSlash)
      dir.list <- dir.list[-i]

  return (dir.list)
}
#custom file.copy2 function, b/c it was giving errors on JANUS when run with MPI
file.copy2 <- function(from="", to="", overwrite=TRUE, copy.mode=TRUE, times=0) {
  file.copy(from, to, overwrite, FALSE, copy.mode)
  if(times < 24)
    if(file.exists(from))
      if(!file.exists(to)) {
        print("trying to copy the file again")
        Recall(from, to, overwrite, copy.mode, (times+1))	#recursively call the function again because when run with MPI the file copying doesn't seem to work everytime...
      }
  #else { #this commented out part copies the file via the system command cp
  #	if(any(grepl("/", to, fixed=TRUE))) { #this part makes the to directory if it doesn't exist... so pretty much this can copy files to places that don't exist, which generally isn't what you want to do but in this case it might help solve an error I keep getting.
  #		y <- to
  #		while(substr(y, nchar(y), nchar(y)) != '/')
  #			y <- substr(y, 1, nchar(y)-1)
  #		y <- substr(y, 1, nchar(y)-1)
  #		if(y != "")
  #			system(paste("mkdir -p", y), ignore.stdout=FALSE, ignore.stderr=FALSE)
  #	}
  #	command <- "cp" #this just calls the system command cp...
  #	if(overwrite == TRUE) command <- paste(command, "-f")
  #	if(copy.mode == TRUE) command <- paste(command, "-p")
  #	system(paste(command, from, to), ignore.stdout=FALSE, ignore.stderr=FALSE)
  #}
}
#made this function b/c dir.create wasn't always working correctly on JANUS for some reason... so if the simulations are being run on JANUS then it uses the system mkdir call to make the directories.
dir.create2 <- function(path, showWarnings = TRUE, recursive = FALSE, mode = "0777", times = 0) {
  dir.create(path, showWarnings, recursive, mode)
  if(times < 24)
    if(!dir.exists(path)) {
      print("trying to make directory again")
      Recall(path, showWarnings, TRUE, mode, (times+1)) #recursively call the function b/c when run on JANUS with MPI it doesn't seem to make the directories everytime... quite aggravating.
    }
  #else if(recursive == TRUE) #this commented out part makes the directory via the system call mkdir
  #	system(paste("mkdir -p", path), ignore.stdout=TRUE, ignore.stderr=FALSE)
  #else
  #	system(paste("mkdir", path), ignore.stdout=TRUE, ignore.stderr=FALSE)
}
#copy directory and content as in system(paste("cp -R", shQuote(from), shQuote(to)))
dir.copy <- function(dir.from, dir.to, overwrite=FALSE){
  dir.create2(dir.to, recursive=TRUE)
  dir.list <- basename(list.dirs2(dir.from, full.names=FALSE, recursive=FALSE))
  file.list <- list.files(dir.from)
  if(length(dir.list) > 0) {
    sapply(dir.list, function(x) {Recall(dir.from=file.path(dir.from, x), dir.to=file.path(dir.to, x), overwrite=overwrite)})
    #file.list <- file.list[-match(dir.list, table=file.list)] #this line gives an error when run in R v. 2.13
    file.list <- file.list[file.list != dir.list] #this line does the same as the other line, but does not throw the error
  }
  if(length(file.list) > 0) {
    sapply(file.list, function(x) {file.copy2(from=file.path(dir.from, x), to=file.path(dir.to, x), overwrite=overwrite, copy.mode=TRUE)})
  }
  invisible(1)
}
#remove directory and content
dir.remove <- function(dir){
  file.list <- try(list.files(dir, all.files=TRUE))
  file.list <- file.list[-which(file.list %in% c(".", ".."))]
  dir.list <- basename(list.dirs2(dir, full.names=FALSE, recursive=FALSE))
  if(length(dir.list) > 0) {
    sapply(dir.list, function(x) Recall(dir=file.path(dir, x)))
    file.list <- file.list[-match(dir.list, table=file.list)]
  }
  if(length(file.list) > 0) {
    sapply(file.list, function(x) {file.remove(file.path(dir, x))})
  }
  file.remove(dir)
}


dir_safe_create <- function(paths, showWarnings = FALSE, recursive = TRUE, mode = "0777") {
  temp <- lapply(paths, function(path) {
      if (!is.null(path) && !is.na(path) && is.character(path) && nchar(path) > 0)
        try(dir.create2(path, showWarnings = showWarnings, recursive = recursive,
          mode = mode), silent = TRUE)
    })

  invisible(temp)
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
    sim_time["sim_time2_North"] <- list(simTiming_ForEachUsedTimeUnit(sim_time,
      sim_tscales = c("daily", "monthly", "yearly"), latitude = 90,
      account_NorthSouth = adjust_NS))

    if (adjust_NS) {
      sim_time["sim_time2_South"] <- list(simTiming_ForEachUsedTimeUnit(sim_time,
        sim_tscales = c("daily", "monthly", "yearly"), latitude = -90,
        account_NorthSouth = TRUE))

    } else {
      sim_time["sim_time2_South"] <- sim_time["sim_time2_North"]
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
      dshift <- as.POSIXlt(ISOdate(st$useyrs, 6, 30, tz = "UTC"))$yday + 1	#new month either at end of year or in the middle because the two halfs (6+6 months) of a year are of unequal length (182 (183 if leap year) and 183 days): I chose to have a new month at end of year (i.e., 1 July -> 1 Jan & 30 June -> 31 Dec; but, 1 Jan -> July 3/4): and instead of a day with doy=366, there are two with doy=182
      res$doy_ForEachUsedDay_NSadj <- unlist(lapply(seq_along(st$useyrs), function(x) {
        temp <- res$doy_ForEachUsedDay[st$useyrs[x] == res$year_ForEachUsedDay]
        c(temp[-(1:dshift[x])], temp[1:dshift[x]])
      }))
      res$month_ForEachUsedDay_NSadj <- strptime(paste(res$year_ForEachUsedDay, res$doy_ForEachUsedDay_NSadj, sep="-"), format="%Y-%j")$mon + 1
      temp <- length(res$year_ForEachUsedDay)
      delta <- if(dshift[1] == 182) 2 else 3
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
    res$month_ForEachUsedMonth <- res$month_ForEachUsedMonth_NSadj <- rep(swsf_glovars[["st_mo"]], times = st$no.useyr)

    if (latitude < 0 && account_NorthSouth) {
      res$month_ForEachUsedMonth_NSadj <- (res$month_ForEachUsedMonth + 5) %% 12 + 1
    }
  }

  if (any(sim_tscales == "yearly")) {

  }

  res
}


#------auxiliary functions

adjustLayersDepth <- function(layers_depth, d) round(layers_depth[seq_len(d)]) #The wrapper only handles 1-cm resolution of soil depths (maily because of the trco)
getLayersWidth <- function(layers_depth) diff(c(0, layers_depth))
setLayerSequence <- function(d) seq_len(d)

sw_dailyC4_TempVar <- function(dailyTempMin, dailyTempMean, simTime2) {
  #Variables to estimate percent C4 species in North America: Teeri JA, Stowe LG (1976) Climatic patterns and the distribution of C4 grasses in North America. Oecologia, 23, 1-12.

  temp7 <- simTime2$month_ForEachUsedDay_NSadj == 7
  Month7th_MinTemp_C <- tapply(dailyTempMin[temp7], simTime2$year_ForEachUsedDay_NSadj[temp7], min)
  LengthFreezeFreeGrowingPeriod_Days <- tapply(dailyTempMin, simTime2$year_ForEachUsedDay_NSadj,
    function(x) {
      temp <- rle(x > 0)
      if (any(temp$values)) max(temp$lengths[temp$values], na.rm = TRUE) else 0
    })
  temp_base65F <- dailyTempMean - 18.333  # 18.333 C = 65 F with (65 - 32) * 5 / 9
  temp_base65F[temp_base65F < 0] <- 0
  DegreeDaysAbove65F_DaysC <- tapply(temp_base65F, simTime2$year_ForEachUsedDay_NSadj, sum)

  nyrs <- seq_along(Month7th_MinTemp_C) #if southern Hemisphere, then 7th month of last year is not included
  temp <- cbind(Month7th_MinTemp_C[nyrs],
                LengthFreezeFreeGrowingPeriod_Days[nyrs],
                DegreeDaysAbove65F_DaysC[nyrs])
  res <- c(apply(temp, 2, mean), apply(temp, 2, stats::sd))
  temp <- c("Month7th_NSadj_MinTemp_C",
            "LengthFreezeFreeGrowingPeriod_NSadj_Days",
            "DegreeDaysAbove65F_NSadj_DaysC")
  names(res) <- c(temp, paste0(temp, ".sd"))

  res
}

#' Calculate several climate variables from daily weather
#' @export
calc_SiteClimate <- function(weatherList, year.start, year.end, do.C4vars = FALSE, simTime2 = NULL) {
  x <- Rsoilwat31::dbW_weatherData_to_dataframe(weatherList)

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
    meanMonthlyTempC = apply(temp[, , 1], 1, mean),
    minMonthlyTempC = apply(temp[, , 2], 1, mean),
    maxMonthlyTempC = apply(temp[, , 3], 1, mean),
    meanMonthlyPPTcm = apply(tempPPT, 1, mean),

    MAP_cm = sum(tempPPT) / length(years),
    MAT_C = mean(xl[["Tmean_C"]]),

    dailyTempMin = if (do.C4vars) x[, "Tmin_C"] else NA,
    dailyTempMean = if (do.C4vars) xl[["Tmean_C"]] else NA,
    dailyC4vars = if (do.C4vars) {
        sw_dailyC4_TempVar(dailyTempMin = x[, "Tmin_C"], dailyTempMean = xl[["Tmean_C"]], simTime2)
      } else NA
  )
}


check_soil_data <- function(data) {
    check_soil <- is.finite(data)
    check_soil[, "depth_cm"] <- check_soil[, "depth_cm"] & data[, "depth_cm"] > 0 &
      diff(c(0, data[, "depth_cm"])) > 0
    check_soil[, "matricd"] <- check_soil[, "matricd"] & data[, "matricd"] > 0.3 &
      data[, "matricd"] <= 2.65
    check_soil[, "gravel_content"] <- check_soil[, "gravel_content"] &
      data[, "gravel_content"] >= 0 & data[, "gravel_content"] < 1
    itemp <- c("sand", "clay")
    check_soil[, itemp] <- check_soil[, itemp] & data[, itemp] > 0 & data[, itemp] <= 1
    itemp <- c("EvapBareSoil_frac", "transpGrass_frac", "transpShrub_frac",
              "transpTree_frac", "transpForb_frac", "imperm")
    check_soil[, itemp] <- check_soil[, itemp] & data[, itemp] >= 0 & data[, itemp] <= 1

    check_soil
}


#functions wet and dry periods

#' Saturation vapor pressure
#'
#' @param T A numeric vector of temperature(s) (deg C)
#' @return A numeric vector of length \code{T} of saturation vapor pressure (kPa) at
#'    temperature T
#' @references Yoder, R. E., L. O. Odhiambo, and W. C. Wright. 2005. Effects of Vapor-Pressure Deficit and Net-Irradiance Calculation Methods on Accuracy of Standardized Penman-Monteith Equation in a Humid Climate Journal of Irrigation and Drainage Engineering 131:228-237.
vp0 <- function(T) {
  0.6108 * exp(17.27 * T / (T + 273.3))	# eq. 5 of Yoder et al. 2005
}


#' Vapor pressure deficit
#'
#' @param Tmin A numeric vector of daily minimum temperature(s) (deg C)
#' @param Tmax A numeric vector of daily maximum temperature(s) (deg C)
#' @param RHmean A numeric vector of daily mean relative humidity (percentage)
#' @return A numeric vector of length \code{T} of vapor pressure deficit (kPa)
#' @references Yoder, R. E., L. O. Odhiambo, and W. C. Wright. 2005. Effects of Vapor-Pressure Deficit and Net-Irradiance Calculation Methods on Accuracy of Standardized Penman-Monteith Equation in a Humid Climate Journal of Irrigation and Drainage Engineering 131:228-237.
vpd <- function(Tmin, Tmax, RHmean = NULL) {
  if (is.null(RHmean)) {
    (vp0(Tmax) - vp0(Tmin)) / 2	# eq. 6 - eq. 13 of Yoder et al. 2005 (VPD6 in Table 4)
  } else {
    (vp0(Tmax) + vp0(Tmin)) / 2 * (1 - RHmean / 100)	# eq. 6 - eq. 11 of Yoder et al. 2005 (VPD4 in Table 4)
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

startDoyOfDuration <- function(x, duration=10) {
  r <- rle(x)
  if(length(r$lengths)==1 | sum(r$values==1 & r$lengths>=duration)==0 ){
    return (ifelse((length(r$lengths)==1 & (r$values==0 | r$lengths<duration)) | sum(r$values==1 & r$lengths>=10)==0, NA, 1)[1])
  } else {
    first10dry <- r$lengths[which(r$values==1 & r$lengths>=duration)][1] #pick first period
    if( !is.na(first10dry) ){
      ind <- which(r$lengths==first10dry & r$values==1)[1] #always pick start of first suitable period
    } else {
      ind <- -1
    }
    if(ind==1) {#start of period at beginning of year
      return(1)
    } else if(ind==-1) {#no period this year
      return(NA)
    } else {
      return(cumsum(r$lengths)[ind-1]+1)
    }
  }
}

endDoyAfterDuration <- function(x, duration=10) {
  r <- rle(x)
  if(length(r$lengths)==1 | sum(r$values==1 & r$lengths>=duration)==0 ){
    return (ifelse((length(r$lengths)==1 & (r$values==0 | r$lengths<duration)) | sum(r$values==1 & r$lengths>=duration)==0, 365, NA)[1])
  } else {
    last10dry <- (rl <- r$lengths[which(r$values==1 & r$lengths>=duration)])[length(rl)] #pick last period
    if( length(last10dry) > 0 ){
      ind <- (temp <- which(r$lengths==last10dry & r$values==1))[length(temp)]	#always pick end of last suitable period
    } else {
      ind <- -1
    }
    if(ind==-1) {#no period this year
      return(NA)
    } else {
      return(cumsum(r$lengths)[ind])
    }
  }
}


#' Calculates temperate dryland criteria
#'
#' @param annualPPT A numeric vector. Annual precipitation values.
#' @param annualPET A numeric vector. Annual potential evapotranspiration values.
#'  The values must be in the same units as those of \code{annualPPT}, e.g., \code{mm}.
#' @param monthlyTemp A numeric vector. Monthly mean air temperature in degree Celsius for each
#'  year for which precipitation and PET values are provided.
#' @param ai_limit A numeric value. Used for return item \code{criteria_12}.
#'
#' @references
#' Deichmann, U. & L. Eklundh. 1991. Global digital datasets for land degradation studies: a GIS approach. Global Environment Monitoring System (GEMS), United Nations Environment Programme (UNEP), Nairobi, Kenya.
#' Trewartha GT, Horn LH (1980) An introduction to climate. McGraw-Hill, New York, page 284: Temperate Areas
#'
#' @return
#'  A list with three items: UN-aridity index (numeric value), temperateness (logical value),
#'  and temperate drylands (logical value).
calc_drylandindices <- function(annualPPT, annualPET, monthlyTemp, ai_limit = 0.5) {
  ai <- annualPPT / annualPET	#Deichmann, U. & L. Eklundh. 1991. Global digital datasets for land degradation studies: a GIS approach. Global Environment Monitoring System (GEMS), United Nations Environment Programme (UNEP), Nairobi, Kenya.
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
    circ_mean(which(abs(x - tmax) < swsf_glovars[["tol"]]), int = 365, na.rm = na.rm),
    circ_mean(which(abs(x - tmin) < swsf_glovars[["tol"]]), int = 365, na.rm = na.rm))
}





#two, three, or four layer aggregation for average daily aggregation output
setAggSoilLayerForAggDailyResponses <- function(layers_depth, daily_lyr_agg){
  d <- length(layers_depth)
  vals <- list()
  #first layer
  DeepestFirstDailyAggLayer <- findInterval(daily_lyr_agg[["first_cm"]], c(0, layers_depth) + swsf_glovars[["tol"]], all.inside=TRUE)
  vals[[1]] <- seq_len(DeepestFirstDailyAggLayer)
  #second layer
  if(!is.null(daily_lyr_agg[["second_cm"]])){
    DeepestSecondDailyAggLayer <- findInterval(daily_lyr_agg[["second_cm"]], c(0, layers_depth) + swsf_glovars[["tol"]], all.inside=TRUE)
  } else {
    DeepestSecondDailyAggLayer <- d
  }
  if(is.numeric(DeepestSecondDailyAggLayer) && is.numeric(DeepestFirstDailyAggLayer) && d > DeepestFirstDailyAggLayer){
    vals[[2]] <- (DeepestFirstDailyAggLayer+1):DeepestSecondDailyAggLayer
  }
  #third layer
  if(!is.null(daily_lyr_agg[["third_cm"]])){
    if(!is.na(daily_lyr_agg[["third_cm"]])){
      DeepestThirdDailyAggLayer <- findInterval(daily_lyr_agg[["third_cm"]], c(0, layers_depth) + swsf_glovars[["tol"]], all.inside=TRUE)
    } else {
      DeepestThirdDailyAggLayer <- NULL
    }
  } else {
    DeepestThirdDailyAggLayer <- d
  }
  if(is.numeric(DeepestThirdDailyAggLayer) && is.numeric(DeepestSecondDailyAggLayer) && d > DeepestSecondDailyAggLayer){
    vals[[3]] <- (DeepestSecondDailyAggLayer+1):DeepestThirdDailyAggLayer
  }
  #fourth layer
  if(!is.null(daily_lyr_agg[["fourth_cm"]])){
    if(!is.na(daily_lyr_agg[["fourth_cm"]])){
      DeepestFourthDailyAggLayer <- findInterval(daily_lyr_agg[["fourth_cm"]], c(0, layers_depth) + swsf_glovars[["tol"]], all.inside=TRUE)
    } else {
      DeepestFourthDailyAggLayer <- NULL
    }
  } else {
    DeepestFourthDailyAggLayer <- d
  }
  if(is.numeric(DeepestFourthDailyAggLayer) && is.numeric(DeepestThirdDailyAggLayer) && d > DeepestThirdDailyAggLayer){
    vals[[4]] <- ((DeepestThirdDailyAggLayer+1):DeepestFourthDailyAggLayer)
  }

  return(vals)
}


#function to extrapolate windspeeds measured at heights different than SOILWAT2 required 2-m above ground
adjust.WindspeedHeight <- function(uz, height){
  # Allen RG, Walter IA, Elliott R, Howell T, Itenfisu D, Jensen M (2005) In The ASCE standardized reference evapotranspiration equation, pp. 59. ASCE-EWRI Task Committee Report.
  # input: windspeed [m/s] at height x
  # output: windspeed [m/s] at height 2 m

  stopifnot(all(uz >= 0) && height >= 2 )
  return( uz * 4.87 / log(67.8 * height - 5.42) )	# eqn. 33 in Allen et al. (2005)
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

#' Split soil layer in two layers
#'
#' @param x A numeric data.frame or matrix. Columns are soil layers.
#' @param il An integer value. The column/soil layer number after which a new layer is added.
#' @param w A numeric vector of length two. The weights used to calculate the values of the new layer.
#' @param method A character string. See \code{Details}.
#'
#' @section Details: The method \code{interpolate} calculates the weighted mean of the
#'  columns/layers \code{il} and \code{il + 1}.
#'  The method \code{exhaust} distributes the value of \code{il + 1} according to the
#'  weights.
#'
#' @return An object like x with one column more at position \code{il + 1}.
add_layer_to_soil <- function(x, il, w, method = c("interpolate", "exhaust")) {
  method <- match.arg(method)
  if (!is.matrix(x))
    x <- as.matrix(x)
  ncols <- dim(x)[2]

  if (ncols > il) {
    x <- x[, c(seq_len(il), NA, (il + 1):ncols)]

    if (method == "interpolate") {
      x[, il + 1] <- if (il > 0) {
        (x[, il] * w[1] + x[, il + 2] * w[2]) / sum(w)
      } else {
        x[, il + 2]
      }

    } else if (method == "exhaust") {
      x[, il + 1] <- x[, il + 2] * w[1] / sum(w)
      x[, il + 2] <- x[, il + 2] * w[2] / sum(w)
    }

  } else if (ncols == il) {
    x <- x[, c(seq_len(ncols), NA)]

    if (method == "interpolate") {
      x[, il + 1] <- x[, il]

    } else if (method == "exhaust") {
      x[, il + 1] <- x[, il] * w[2] / sum(w)
      x[, il] <- x[, il] * w[1] / sum(w)
    }

  } else {
    stop("Object x has ", ncols, " columns; thus, a new ", il, "-th column cannot be created")
  }

  x
}

identify_soillayers <- function(depths, sdepth) {
  it <- findInterval(depths, sdepth)
  if (any(is.na(it))) {
    as.integer(stats::na.exclude(it))
  } else if (length(it) > 1 && diff(it) > 0) {
    (1 + it[1]):(it[2])
  } else {
    it[1]
  }
}

adjustLayer_byImp <- function(depths, imp_depth, sdepths) {
  if (any(imp_depth < depths[1])) {
    depths <- imp_depth
    if (length(sdepths) >= 2) {
      temp <- findInterval(imp_depth, sdepths)
      if (temp > 1) {
        depths <- c(sdepths[temp - 1], imp_depth)
      } else {
        depths <- c(imp_depth, sdepths[temp + 1])
      }
    }
  } else if(any(imp_depth < depths[2])){
    depths <- c(depths[1], imp_depth)
  }

  depths
}

EstimateInitialSoilTemperatureForEachSoilLayer <- function(layers_depth, lower.Tdepth, soilTupper, soilTlower){
  sl <- c(0, lower.Tdepth)
  st <- c(soilTupper, soilTlower)

  stats::predict(stats::lm(st ~ sl), data.frame(sl = layers_depth))
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

setDeepestTopLayer <- function(layers_depth, Depth_TopLayers_cm) {
  max(1, findInterval(Depth_TopLayers_cm, layers_depth))
}

setTopLayer <- function(d, DeepestTopLayer) {
  seq_len(if(d < DeepestTopLayer) d else DeepestTopLayer)
}

setBottomLayer <- function(d, DeepestTopLayer) {
  if (d <= DeepestTopLayer) {
    NULL
  } else {
    (DeepestTopLayer + 1L):d
  }
}


#data is the values for one year adj for SWPcrit_MPa; TRUE==dry
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




benchmark_BLAS <- function(platform) {
  if (grepl("darwin", platform)) { # apparently this works only on Mac OS X
    blas <- system2(command = file.path(Sys.getenv()[["R_HOME"]], "R"), args = "CMD config BLAS_LIBS", stdout = TRUE)
    blas <- sub("-L/", "/", (strsplit(blas, split=" ")[[1]][1]))
    lapack <- system2(command = file.path(Sys.getenv()[["R_HOME"]], "R"), args = "CMD config LAPACK_LIBS", stdout = TRUE)
    lapack <- sub("-L/", "/", (strsplit(lapack, split=" ")[[1]][1]))
    get_ls <- if(identical(blas, lapack)) list(blas) else list(blas, lapack)
    temp <- lapply(get_ls, FUN = function(x) print(system2(command = "ls", args = paste("-l", x), stdout = TRUE)))

    print("Check linked BLAS library:") # http://simplystatistics.org/2016/01/21/parallel-blas-in-r/#
    print(system.time({ x <- replicate(5e3, stats::rnorm(5e3)); tcrossprod(x) }))

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


#' Converts precipitation data to values in cm / month
convert_precipitation <- function(x, unit_conv, dpm) {
  if (unit_conv %in% c("mm/month", "mm month-1")) {
    x <- x / 10

  } else if (unit_conv %in% c("mm/d", "mm d-1")) {
    x <- x * dpm / 10

  } else if (unit_conv %in% c("kg/m2/s", "kg m-2 s-1", "mm/s", "mm s-1")) {
    x <- x * dpm * 8640

  } else if (unit_conv %in% c("cm/month", "cm month-1")) {

  } else stop("Unknown precipitation unit: ", unit_conv)

  x
}

#' Converts temperature data to values in degree Celsius
convert_temperature <- function(x, unit_conv) {
  if (unit_conv == "K") {
    x <- x - 273.15

  } else if (unit_conv == "F") {
    x <- (x - 32) * 0.5555556

  } else if (unit_conv == "C") {

  } else stop("Unknown temperature unit: ", unit_conv[1])

  x
}


#' Merge two soil input datafiles
#'
#' Merge datafiles from two soil data sources (source 1 overrides source 2)
#'  and choose some or none of the variables to come from one source only.
#'
#' @param fmaster A character string. Path to the target master file.
#' @param fmaster1 A character string. Path to master file derived from extracting from
#'  soil data source 1
#' @param fmaster2 A character string. Path to master file derived from extracting from
#'  soil data source 2
#' @param fslayer A character string. Path to the target soil layer structure file.
#' @param fslayer1 A character string. Path to soil layer file derived from data source 1
#' @param fslayer2 A character string. Path to soil layer file derived from data source 2
#' @param fstexture A character string. Path to the target soil texture input file.
#' @param fstexture1 A character string. Path to soil texture file derived from data source 1
#' @param fstexture2 A character string. Path to soil texture file derived from data source 2
#' @param var_from2 A vector of character strings. Variables of the soil texture file,
#'  which will be take values from source2 if available even if source1 is available
#'
#' @return A logical value. This function is called for its side effects, i.e., storing
#'  updated/new files to \code{fmaster}, \code{fslayer}, and \code{fstexture}.
merge_2soils <- function(fmaster, fmaster1, fmaster2, fslayer, fslayer1, fslayer2,
  fstexture, fstexture1, fstexture2, var_from2 = NULL) {

  #------ MASTER FILES
  master1 <- utils::read.csv(fmaster1)
  master2 <- utils::read.csv(fmaster2)
  master <- if (file.exists(fmaster)) utils::read.csv(fmaster) else master1

  source1 <- as.character(unique(stats::na.exclude(master1$SoilTexture_source)))
  source2 <- as.character(unique(stats::na.exclude(master2$SoilTexture_source)))

  stopifnot(length(source1) == 1, length(source2) == 1)

  print(paste("'merge_2soils': data from", shQuote(source1), "and", shQuote(source2),
    "will be merged, and values from", shQuote(source1), "will be used for sites which",
    "contain data from both sources.",
    if (length(var_from2) > 0) paste("However, data from", shQuote(source2), "for",
    "variables", paste(shQuote(var_from2), collapse = ", "), "will be",
    "used for all sites if available")))

  iuse_source <- ifelse(!is.na(master1$SoilTexture_source) &
    !is.na(master1$Include_YN_SoilSources) & master1$Include_YN_SoilSources > 0, 1,
    ifelse(!is.na(master2$SoilTexture_source) &
    !is.na(master2$Include_YN_SoilSources) & master2$Include_YN_SoilSources > 0, 2, NA))

  soiltally <- table(iuse_source, useNA = "ifany")
  print(soiltally)

  # Indices of soil datasets
  id1 <- id1c <- !is.na(master1$SoilTexture_source) & master1$SoilTexture_source == source1
  id2 <- !is.na(master2$SoilTexture_source) & master2$SoilTexture_source == source2
  id2c <- id2 & !id1
  id12 <- id1 & id2
  idnot <- !id1c & !id2c

  # Copy data
  master[idnot, "SoilTexture_source"] <- NA
  master[id1c, "SoilTexture_source"] <- source1
  master[id2c, "SoilTexture_source"] <- source2
  master[idnot, "Include_YN_SoilSources"] <- 0
  master[!idnot, "Include_YN_SoilSources"] <- 1

  # Save to disk
  utils::write.csv(master, file = fmaster, row.names = FALSE)


  #------SOIL LAYERS
  sl1 <- utils::read.csv(fslayer1)
  sl2 <- utils::read.csv(fslayer2)
  sl <- if (file.exists(fslayer)) utils::read.csv(fslayer) else sl1

  # Copy data
  sl[idnot, -1] <- NA
  sl[id1c, ] <- sl1[id1c, ]
  sl[id2c, ] <- sl2[id2c, ]

  # Save to disk
  utils::write.csv(sl, file = fslayer, row.names = FALSE)


  #------SOIL TEXTURE DATA
  st1_use <- utils::read.csv(fstexture1, nrows = 1)
  st1 <- utils::read.csv(fstexture1, skip = 1)
  st2_use <- utils::read.csv(fstexture2, nrows = 1)
  st2 <- utils::read.csv(fstexture2, skip = 1)
  st_use <- ifelse(st1_use == 1 | st2_use == 1, 1, 0)

  st <- if (file.exists(fstexture)) utils::read.csv(fstexture, skip = 1) else st1
  names(st1) <- names(st2) <- names(st) <- names(st_use) <- names(st1_use)

  # Copy data
  st[idnot, -1] <- NA
  st[id1c, ] <- st1[id1c, ]
  st[id2c, ] <- st2[id2c, ]

  # Replace content of variables 'var_from2' with values from source2
  if (sum(id12) > 0 && length(var_from2) > 0) {
    for (k in seq_along(var_from2)) {
      icol2 <- grep(var_from2[k], names(st), ignore.case = TRUE)

      if (length(icol2) > 0) {
        st[id12, icol2] <- st2[id12, icol2]
      } else {
        print(paste("'merge_2soils': no columns found for", shQuote(var_from2[k])))
      }
    }
  }

  # Reset transpiration regions
  st[, grep("TranspRegion", names(st))] <- NA
  print(paste("'merge_2soils': NOTE: transpiration regions have been reset. They require",
    "updated values before a simulation can be run successfully."))

  # Save to disk
  utils::write.csv(rbind(st_use, st), file = fstexture, row.names = FALSE)

  TRUE
}


convert_to_todo_list <- function(x) {
  temp <- matrix(x, ncol = 2, nrow = length(x) / 2, byrow = TRUE)
  todo <- lapply(temp[, 2], function(x) as.logical(as.numeric(x)))
  names(todo) <- temp[, 1]

  todo
}



setup_scenarios <- function(sim_scens, future_yrs) {
  # make sure 'ambient' is not among models
  sim_scens[["models"]] <- grep(sim_scens[["ambient"]], sim_scens[["models"]],
    invert = TRUE, value = TRUE)

  if (length(sim_scens[["models"]]) > 0) {
    # add (multiple) future_yrs
    sim_scens[["models"]] <- paste0(rownames(future_yrs), ".", rep(sim_scens[["models"]],
      each = nrow(future_yrs)))
    # add (multiple) downscaling.method
    sim_scens[["models"]] <- paste0(sim_scens[["method_DS"]], ".",
      rep(sim_scens[["models"]], each = length(sim_scens[["method_DS"]])))
  }

  # make sure 'ambient' is first entry
  temp <- c(sim_scens[["ambient"]], sim_scens[["models"]])

  c(sim_scens, list(id = temp, N = length(temp)))
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
