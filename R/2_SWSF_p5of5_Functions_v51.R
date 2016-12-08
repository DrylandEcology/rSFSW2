#------ Remove when this becomes a R package
# - compiler::cmpfun
# - move constants to package environment

slot <- methods::slot

#------ Constants
output_timescales_maxNo <- 4L
SoilLayer_MaxNo <- 20L
lmax <- seq_len(SoilLayer_MaxNo)
SoilWat.windspeedAtHeightAboveGround <- 2	#m
st_mo <- seq_len(12L)
tol <- sqrt(.Machine$double.eps)
toln <- sqrt(.Machine$double.neg.eps)

#------ Funtions

getStartYear <- compiler::cmpfun(function(simstartyr) simstartyr + 1)


has_nodata <- compiler::cmpfun(function(data, tag = NULL, MARGIN = 1) {
  if (is.null(tag)) {
    apply(data, MARGIN, function(x) all(is.na(x)))
  } else {
    apply(data[, grepl(tag, colnames(data)), drop = FALSE], MARGIN, function(x) all(is.na(x)))
  }
})

has_incompletedata <- compiler::cmpfun(function(data, tag = NULL, MARGIN = 1) {
  if (is.null(tag)) {
    apply(data, MARGIN, anyNA)
  } else {
    apply(data[, grepl(tag, colnames(data)), drop = FALSE], MARGIN, anyNA)
  }
})

#custom list.dirs function because the ones in 2.13 and 2.15 are different... this function will behave like the one in 2.15 no matter which version you are using...
#note: should work on any system where the directory seperator is .Platform$file.sep (ie Unix)
list.dirs2 <- compiler::cmpfun(function(path, full.names=TRUE, recursive=TRUE) {
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
})
#custom file.copy2 function, b/c it was giving errors on JANUS when run with MPI
file.copy2 <- compiler::cmpfun(function(from="", to="", overwrite=TRUE, copy.mode=TRUE, times=0) {
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
})
#made this function b/c dir.create wasn't always working correctly on JANUS for some reason... so if the simulations are being run on JANUS then it uses the system mkdir call to make the directories.
dir.create2 <- compiler::cmpfun(function(path, showWarnings = TRUE, recursive = FALSE, mode = "0777", times = 0) {
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
})
#copy directory and content as in system(paste("cp -R", shQuote(from), shQuote(to)))
dir.copy <- compiler::cmpfun(function(dir.from, dir.to, overwrite=FALSE){
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
})
#remove directory and content
dir.remove <- compiler::cmpfun(function(dir){
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
})

isLeapYear <- compiler::cmpfun(function(y) {
  #from package: tis
  y %% 4 == 0 & (y %% 100 != 0 | y %% 400 == 0)
})

#' Iterator functions
#'
#' @param isim An integer value. A value of \code{runIDs_todo} as subset of \code{runIDs_total}, i.e., a consecutive index across loops 1+2b
#' @param sc An integer value. The iterator position along \code{scenario_No}.
#' @param scN An integer value. The number of (climate) scenarios used in the project, i.e., \eqn{scN == scenario_No}.
#' @param runN An integer value. The number of runs/sites set up in the master input file, i.e., \eqn{runN == runsN_master}.
#' @param runIDs An integer vector. The identification IDs of rows in the master file that are included, i.e., \eqn{runIDs == runIDs_sites}.
#'
#' @section NOTE:
#'  Do not change the iterators without adjusting the design of the output databases!
#'
#' @section Simulation runs:
#'  * Simulations are run over three nested loops
#'    - loop1 (1...expN) nested in loop2b (1...runsN_sites) nested in loop3 (1...scenario_No)
#'        - Note: loop3 (along scenarios) occurs within the function 'do_OneSite'
#'        - Note: loop2b is a subset of loop2a (1...runsN_master)
#'    - column 'include_YN' reduces 'site_id' to 'runIDs_sites'
#'    - 'site_id' and 'P_id' are invariant to 'include_YN'
#'
#'  * Master input file: column 'include_YN' selects rows which are included in the simulation
#'    - Note: rows of the master input file correspond to rows of the treatment input file
#'    - column 'site_id' == consecutive identification numbers of all rows in the master file; this is treated as a unique (and stable) identifier of a site
#'    - runsN_master == number of rows in the master file
#'    - runsN_sites == number of rows in the master file that are included (runsN_sites <= max(site_id))
#'    - runIDs_sites == identification of rows in the master file that are included
#'
#'  * Experimental input file: each row defines a condition which is applied to every runIDs_sites
#'    - expN == number of experimental treatments
#'
#'  * The function 'do_OneSite' will be called n-times with n = runsN_total
#'    - runsN_total == (number of included sites) x (number of experimental treatments)
#'    - runIDs_total == consecutive identification numbers along runsN_total
#'    - runIDs_done == values of runIDs_total that have already been processed by 'do_OneSite'
#'    - runIDs_todo == values of runIDs_total that await simulation by 'do_OneSite'
#'    - runsN_todo == number of runIDs_total that await simulation by 'do_OneSite'
#'
#'  * The function 'do_OneSite' could be called n-times with n = runsN_incl if all 'include_YN' were on
#'    - runsN_incl == (number of sites) x (number of experimental treatments)
#'
#'  * The variable 'climate.conditions' defines climate conditions that are applied to each 'runIDs_total'
#'    - scenario_No == number of climate conditions
#'
#'  * A grand total of n = runsN_Pid SoilWat runs could be carried out (n == number of rows in the output database)
#'    - runsN_Pid == max(P_id) == runsN_incl x scenario_No
#'    - P_id == a consecutive identification number for each possible SoilWat simulation; used as the ID for the output database
#'
#' @aliases it_exp it_site it_Pid
#' @name iterators
NULL
#' @rdname iterators
#' @return An integer value of the iterator position in loop 1 based on the position across loops 1+2b
it_exp <- compiler::cmpfun(function(isim, runN) (isim - 1L) %/% runN + 1L)
#' @rdname iterators
#' @return An integer value of the iterator position across 'runIDs_sites', i.e., the position in loop 2 based on position across loops 1+2b
it_site <- compiler::cmpfun(function(isim, runN, runIDs) runIDs[(isim - 1L) %% runN + 1L])
#' @rdname iterators
#' @return An integer value of the iterator position across all loops 1+2a+3, dependent on \code{include_YN}.
it_Pid_old <- compiler::cmpfun(function(isim, sc, scN) (isim - 1L) * scN + sc)
#' @rdname iterators
#' @return An integer value of the iterator position across all loops 1+2a+3, invariant
#'  to \code{include_YN}. A consecutive identification number for each possible SoilWat
#'  simulation--used as the ID for the output database.
it_Pid <- compiler::cmpfun(function(isim, sc, scN, runN, runIDs) {
  ((it_exp(isim, runN) - 1L) * runN + it_site(isim, runN, runIDs) - 1L) * scN + sc
})


## Tests
#include_YN <- c(0, 0, 1, 0, 0, 1, 1, 0)
#include_YN <- rep(1, 8)
#t(sapply(runIDs_todo, function(isim) c(isim, it_site(isim, 8), it_exp(isim, 8), it_Pid(isim, 1, 3, 8), it_Pid_old(isim, 1))))
#t(sapply(runIDs_total, function(isim) c(isim, it_site(isim, 8), it_exp(isim, 8), it_Pid(isim, 1, 3, 8), it_Pid_old(isim, 1))))




#' @examples
#'  month1 <- function() as.POSIXlt(seq(from = ISOdate(1980, 1, 1, tz = "UTC"),
#'     to = ISOdate(2010, 12, 31, tz = "UTC"), by = "1 day"))$mon + 1
#'  month2 <- function() seq_month_ofeach_day(list(1980, 1, 1),
#'    list(2010, 12, 31), tz = "UTC")
#'
#'  if (requireNamespace("microbenchmark", quietly = TRUE))
#'    microbenchmark::microbenchmark(month1(), month2())    # barely any difference
#'
seq_month_ofeach_day <- compiler::cmpfun(function(from = list(year = 1900, month = 1, day = 1),
  to = list(year = 1900, month = 12, day = 31), tz = "UTC") {

  x <- paste(from[[1]], from[[2]], from[[3]], 12, 0, 0, sep = "-")
  from0 <- unclass(as.POSIXct.POSIXlt(strptime(x, "%Y-%m-%d-%H-%M-%OS", tz = tz)))
  x <- paste(to[[1]], to[[2]], to[[3]], 12, 0, 0, sep = "-")
  to0 <- unclass(as.POSIXct.POSIXlt(strptime(x, "%Y-%m-%d-%H-%M-%OS", tz = tz)))

  res <- seq.int(0, to0 - from0, by = 86400) + from0
  as.POSIXlt.POSIXct(.POSIXct(res, tz = tz))$mon + 1
})


simTiming <- compiler::cmpfun(function(startyr, simstartyr, endyr) {
  res <- list()
  #simyrs <- simstartyr:endyr
  #no.simyr <- endyr - simstartyr + 1
  temp <- ISOdate(startyr, 1, 1, tz = "UTC")

  res[["useyrs"]] <- startyr:endyr

  res[["no.useyr"]] <- endyr - startyr + 1
  res[["no.usemo"]] <- res[["no.useyr"]] * 12
  res[["no.usedy"]] <- as.numeric(ISOdate(endyr, 12, 31, tz = "UTC") - temp) + 1

  res[["discardyr"]] <- startyr - simstartyr
  res[["discardmo"]] <- res[["discardyr"]] * 12
  res[["discarddy"]] <- as.numeric(temp - ISOdate(simstartyr, 1, 1, tz = "UTC"))

  res[["index.useyr"]] <- res[["discardyr"]] + seq_len(res[["no.useyr"]])
  res[["index.usemo"]] <- res[["discardmo"]] + seq_len(res[["no.usemo"]])
  res[["index.usedy"]] <- res[["discarddy"]] + seq_len(res[["no.usedy"]])

  res
})

simTiming_ForEachUsedTimeUnit <- compiler::cmpfun(function(st, sim_tscales, latitude = 90, account_NorthSouth = TRUE) {	#positive latitudes -> northern hemisphere; negative latitudes -> southern hemisphere
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
    res$month_ForEachUsedMonth <- res$month_ForEachUsedMonth_NSadj <- rep(st_mo, times = st$no.useyr)

    if (latitude < 0 && account_NorthSouth) {
      res$month_ForEachUsedMonth_NSadj <- (res$month_ForEachUsedMonth + 5) %% 12 + 1
    }
  }

  if (any(sim_tscales == "yearly")) {

  }

  res
})


#------auxiliary functions

in_box <- compiler::cmpfun(function(xy, xbounds, ybounds, i_use) {
  !i_use &
  xy[, 1] >= xbounds[1] & xy[, 1] <= xbounds[2] &
  xy[, 2] >= ybounds[1] & xy[, 2] <= ybounds[2]
})

adjustLayersDepth <- compiler::cmpfun(function(layers_depth, d) round(layers_depth[seq_len(d)])) #The wrapper only handles 1-cm resolution of soil depths (maily because of the trco)
getLayersWidth <- compiler::cmpfun(function(layers_depth) diff(c(0, layers_depth)))
setLayerSequence <- compiler::cmpfun(function(d) seq_len(d))

sw_dailyC4_TempVar <- compiler::cmpfun(function(dailyTempMin, dailyTempMean, simTime2) {
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
  res <- c(apply(temp, 2, mean), apply(temp, 2, sd))
  temp <- c("Month7th_NSadj_MinTemp_C",
            "LengthFreezeFreeGrowingPeriod_NSadj_Days",
            "DegreeDaysAbove65F_NSadj_DaysC")
  names(res) <- c(temp, paste0(temp, ".sd"))

  res
})

sw_SiteClimate_Ambient <- compiler::cmpfun(function(weatherList, year.start, year.end, do.C4vars = FALSE, simTime2 = NULL) {
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
})

cut0Inf <- compiler::cmpfun(function(x, val = NA) {
  x[x < 0] <- val
  x
})
NAto0 <- compiler::cmpfun(function(x) {
  x[is.na(x)] <- 0
  x
})
finite01 <- compiler::cmpfun(function(x, val_low = 0, val_high = 1) {
  x[x < 0 | is.na(x)] <- val_low
  x[x > 1] <- val_high
  x
})

PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996 <- compiler::cmpfun(function(MAP_mm,MAT_C,monthly.ppt,monthly.temp,dailyC4vars,isNorth,shrub.fraction.limit,
    use_Annuals_Fraction,Annuals_Fraction,
    use_C4_Fraction,C4_Fraction,
    use_C3_Fraction,C3_Fraction,
    use_Shrubs_Fraction,Shrubs_Fraction,
    use_Forbs_Fraction, Forbs_Fraction,
    use_BareGround_Fraction, BareGround_Fraction) {

  f.digits <- 3
  tolerance <- 1.1*10^-f.digits

  #Get the user specified fractions, if column is false set to NA
  tree.fraction <- 0 #option 'PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996' doesn't estimate tree cover, i.e., assumed to be == 0
  forb.fraction <- 0
  bareGround.fraction <- 0
  AnnC4C3ShrubForbBareGroundFraction <- rep(NA, 6)
  if(use_Annuals_Fraction){
    AnnC4C3ShrubForbBareGroundFraction[1] <- finite01(Annuals_Fraction)
  } else {
    AnnC4C3ShrubForbBareGroundFraction[1] <- 0 #Annuals can not be NA
  }
  if(use_C4_Fraction)
    AnnC4C3ShrubForbBareGroundFraction[2] <- C4_Fraction
  if(use_C3_Fraction)
    AnnC4C3ShrubForbBareGroundFraction[3] <- C3_Fraction
  if(use_Shrubs_Fraction)
    AnnC4C3ShrubForbBareGroundFraction[4] <- Shrubs_Fraction

  if(use_Forbs_Fraction) {
    AnnC4C3ShrubForbBareGroundFraction[5] <- finite01(Forbs_Fraction)
  } else {
    AnnC4C3ShrubForbBareGroundFraction[5] <- forb.fraction
  }
  if(use_BareGround_Fraction) {
    AnnC4C3ShrubForbBareGroundFraction[6] <- finite01(BareGround_Fraction)
  } else {
    AnnC4C3ShrubForbBareGroundFraction[6] <- bareGround.fraction
  }
  AnnC4C3ShrubForbBareGroundFraction <- cut0Inf(AnnC4C3ShrubForbBareGroundFraction) #treat negatives as if NA
  TotalFraction <- sum(AnnC4C3ShrubForbBareGroundFraction, na.rm=TRUE)

  #Decide if all fractions are sufficiently defined or if they need to be calculated based on climate variables
  if(!isTRUE(all.equal(TotalFraction, 1, tolerance=tolerance)) && TotalFraction < 1 && sum(is.na(AnnC4C3ShrubForbBareGroundFraction)) == 0) {
    stop(print(paste(" run: User defined fractions of Shrub, C3, C4, Annuals are all set, but less than 1", sep=""))) #throw an error
  }

  if(isTRUE(all.equal(TotalFraction, 1, tolerance=tolerance)) || TotalFraction > 1 || sum(is.na(AnnC4C3ShrubForbBareGroundFraction)) == 1){

    if(sum(is.na(AnnC4C3ShrubForbBareGroundFraction)) == 1){ #if only one is NA, then this can be calculated
      AnnC4C3ShrubForbBareGroundFraction[which(is.na(AnnC4C3ShrubForbBareGroundFraction))] <- cut0Inf(1 - TotalFraction)
    } else {
      AnnC4C3ShrubForbBareGroundFraction <- finite01(AnnC4C3ShrubForbBareGroundFraction) #the composition is >= 1, so set eventually remaining NA to 0
    }

    TotalFraction <- sum(AnnC4C3ShrubForbBareGroundFraction, na.rm=TRUE)
    AnnC4C3ShrubForbBareGroundFraction <- AnnC4C3ShrubForbBareGroundFraction / TotalFraction #Rescale, in case it is needed

  } else { #i.e., (TotalFraction < 1 && sum(is.na(AnnC4C3ShrubForbBareGroundFraction)) > 1) is TRUE; thus, calculate some fractions based on climate variables
    if(isNorth){ #Northern hemisphere
      Months_WinterTF <- c(12, 1:2)
      Months_SummerTF <- c(6:8)
    } else {
      Months_WinterTF <- c(6:8)
      Months_SummerTF <- c(12, 1:2)
    }
    ppt.SummerToMAP <- sum(monthly.ppt[Months_SummerTF]) / MAP_mm
    ppt.WinterToMAP <- sum(monthly.ppt[Months_WinterTF]) / MAP_mm

    #---Potential natural vegetation
    #1. step: Paruelo JM, Lauenroth WK (1996) Relative abundance of plant functional types in grasslands and shrublands of North America. Ecological Applications, 6, 1212-1224.
    if(MAP_mm < 1){
      shrubs.fractionNA <- NA
    } else {
      shrubs.fractionNA <- cut0Inf(1.7105 - 0.2918 * log(MAP_mm) + 1.5451 * ppt.WinterToMAP) 								#if NA, then not enough winter precipitation above a given MAP
    }
    if(MAT_C <= 0){
      grass.c4.fractionNA <- 0
    } else {
      grass.c4.fractionNA <- cut0Inf(-0.9837 + 0.000594 * MAP_mm + 1.3528 * ppt.SummerToMAP + 0.2710 * log(MAT_C))			#if NA, then either MAT < 0 or not enough summer precipitation or too cold below a given MAP
    }
    if(ppt.WinterToMAP <= 0){
      grass.c3ingrasslands.fractionNA <- grass.c3inshrublands.fractionNA <- NA
    } else {
      grass.c3ingrasslands.fractionNA <- cut0Inf(1.1905 - 0.02909 * MAT_C + 0.1781 * log(ppt.WinterToMAP) - 0.2383 * 1)		#if NA, then not enough winter precipitation or too warm below a given MAP
      grass.c3inshrublands.fractionNA <- cut0Inf(1.1905 - 0.02909 * MAT_C + 0.1781 * log(ppt.WinterToMAP) - 0.2383 * 2)
    }
    grass.c3.fractionNA <- ifelse(shrubs.fractionNA >= shrub.fraction.limit && !is.na(shrubs.fractionNA), grass.c3inshrublands.fractionNA, grass.c3ingrasslands.fractionNA)

    grass.Annual.fraction <- AnnC4C3ShrubForbBareGroundFraction[1] #Ann will be 0 or something <= 1

    #2. step: Teeri JA, Stowe LG (1976) Climatic patterns and the distribution of C4 grasses in North America. Oecologia, 23, 1-12.
    #This equations give percent species/vegetation -> use to limit Paruelo's C4 equation, i.e., where no C4 species => there are no C4 abundance > 0
    if(dailyC4vars["LengthFreezeFreeGrowingPeriod_NSadj_Days"] <= 0){
      grass.c4.species <- 0
    } else {
      x10 <- dailyC4vars["Month7th_NSadj_MinTemp_C"] * 9/5 + 32
      x13 <- dailyC4vars["DegreeDaysAbove65F_NSadj_DaysC"] * 9/5
      x18 <- log(dailyC4vars["LengthFreezeFreeGrowingPeriod_NSadj_Days"])
      grass.c4.species <- as.numeric((1.60 * x10 + 0.0086 * x13 - 8.98 * x18 - 22.44) / 100)
    }
    grass.c4.fractionNA <- ifelse(grass.c4.species >= 0, grass.c4.fractionNA, NA)

    #3. step: Replacing missing values: If no or only one successful equation, then add 100% C3 if MAT < 10 C, 100% shrubs if MAP < 600 mm, and 100% C4 if MAT >= 10C & MAP >= 600 mm	[these rules are made up arbitrarily by drs, Nov 2012]
    if(sum(!is.na(shrubs.fractionNA), !is.na(grass.c4.fractionNA), !is.na(grass.c3.fractionNA)) <= 1){
      if(MAP_mm < 600) shrubs.fractionNA <- 1 + ifelse(is.na(shrubs.fractionNA), 0, shrubs.fractionNA)
      if(MAT_C < 10)  grass.c3.fractionNA <- 1 + ifelse(is.na(grass.c3.fractionNA), 0, grass.c3.fractionNA)
      if(MAT_C >= 10  & MAP_mm >= 600)  grass.c4.fractionNA <- 1 + ifelse(is.na(grass.c4.fractionNA), 0, grass.c4.fractionNA)
    }

    #4. step: Scale fractions to 0-1 with a sum of 1 including grass.Annual.fraction, but don't scale grass.Annual.fraction
    #if na then use calc fraction else use the user defined fraction
    shrubs.fraction <- NAto0(shrubs.fractionNA)
    grass.c4.fraction <- NAto0(grass.c4.fractionNA)
    grass.c3.fraction <- NAto0(grass.c3.fractionNA)

    sumVegWithoutAnnuals <- shrubs.fraction + grass.c4.fraction + grass.c3.fraction
    shrubs.fraction <- (shrubs.fraction / sumVegWithoutAnnuals) * (1 - grass.Annual.fraction) #scale these down to 1-annual fraction
    grass.c4.fraction <- (grass.c4.fraction / sumVegWithoutAnnuals) * (1 - grass.Annual.fraction)
    grass.c3.fraction <- (grass.c3.fraction / sumVegWithoutAnnuals) * (1 - grass.Annual.fraction)

    calcAnnC4C3ShrubForbBareGroundFraction <- c(grass.Annual.fraction, grass.c4.fraction, grass.c3.fraction, shrubs.fraction)
    naIndex <- which(is.na(AnnC4C3ShrubForbBareGroundFraction))
    #replace missing values
    if(isTRUE(all.equal(sum(calcAnnC4C3ShrubForbBareGroundFraction[naIndex]), 0)) && isTRUE(all.equal(temp <- sum(AnnC4C3ShrubForbBareGroundFraction[!naIndex]), 0))){ #there would be no vegetation, so force vegetation > 0
      AnnC4C3ShrubForbBareGroundFraction[naIndex] <- (1 - temp) / length(naIndex)
    } else {
      AnnC4C3ShrubForbBareGroundFraction[naIndex] <- calcAnnC4C3ShrubForbBareGroundFraction[naIndex]
    }
    #now we need to get the sum and scale the naIndex values accordingly
    AnnC4C3ShrubForbBareGroundFraction[naIndex] <- sapply(AnnC4C3ShrubForbBareGroundFraction[naIndex], function(x) (x/sum(AnnC4C3ShrubForbBareGroundFraction[naIndex])) * (1-sum(AnnC4C3ShrubForbBareGroundFraction[-naIndex])))
  }

  #Scale Grass components to one (or set to 0)
  if(!isTRUE(all.equal(sum(AnnC4C3ShrubForbBareGroundFraction[4:6]), 1))){
    grass.c4.fractionG <- AnnC4C3ShrubForbBareGroundFraction[2] / (1-sum(AnnC4C3ShrubForbBareGroundFraction[4:6]))
    grass.c3.fractionG <- AnnC4C3ShrubForbBareGroundFraction[3] / (1-sum(AnnC4C3ShrubForbBareGroundFraction[4:6]))
    grass.Annual.fractionG <- AnnC4C3ShrubForbBareGroundFraction[1] / (1-sum(AnnC4C3ShrubForbBareGroundFraction[4:6]))
  } else {
    grass.c4.fractionG <- grass.c3.fractionG <- grass.Annual.fractionG <- 0
  }
  grass.fraction <- sum(AnnC4C3ShrubForbBareGroundFraction[c(1:3)])

  list(Composition = c(Grasses = grass.fraction,
                         Shrubs = AnnC4C3ShrubForbBareGroundFraction[4],
                         Trees = tree.fraction,
                         Forbs = AnnC4C3ShrubForbBareGroundFraction[5],
                         BareGround = AnnC4C3ShrubForbBareGroundFraction[6]),
       grasses.c3c4ann.fractions = c(grass.c3.fractionG,
                                       grass.c4.fractionG,
                                       grass.Annual.fractionG))
})

calc.loess_coeff <- compiler::cmpfun(function(N, span) {
  #prevent call to loessc.c:ehg182(104): "span too small.   fewer data values than degrees of freedom"
  lcoef <- list(span = min(1, span), degree = 2)
  if (span <= 1) {
    nf <- floor(lcoef$span * N) - 1 #see R/trunk/src/library/stats/src/loessf.f:ehg136()
    if (nf > 2) {
      lcoef$degree <- 2
    } else if(nf > 1){
      lcoef$degree <- 1
    } else {
      lcoef <- Recall(N, lcoef$span + 0.1)
    }
  }
  lcoef
})

predict.season <- compiler::cmpfun(function(biomass_Standard, std.season.padded, std.season.seq, site.season.seq) {
  #length(std.season.seq) >= 3 because of padding and test that season duration > 0
  lcoef <- calc.loess_coeff(N = length(std.season.seq), span = 0.4)

  op <- options(c("warn", "error"))
  on.exit(options(op))
  options(warn = -1, error = traceback) #loess throws many warnings: 'pseudoinverse used', see calc.loess_coeff(), etc.

  sapply(apply(biomass_Standard, 2, function(x) {
      lf <- loess(x[std.season.padded] ~ std.season.seq, span = lcoef$span, degree = lcoef$degree)
      predict(lf, newdata = data.frame(std.season.seq = site.season.seq))
    }),
    FUN = function(x) max(0, x)) # guarantee that > 0
})

#Equations: Milchunas & Lauenroth 1993 (Fig. 2): Y [g/m2/yr] = c1 * MAP [mm/yr] + c2
Shrub_ANPP <- compiler::cmpfun(function(MAP_mm) 0.393 * MAP_mm - 10.2)
Grass_ANPP <- compiler::cmpfun(function(MAP_mm) 0.646 * MAP_mm - 102.5)

#' @section Default inputs:
#'    - shrubs (IM_USC00107648_Reynolds; 70% shrubs, 30% C3): biomass was estimated at MAP = 450 mm/yr
#'    - sgs-grassland (GP_SGSLTER; 12% shrubs, 22% C3, and 66% C4): biomass was estimated at MAP = 340 mm/yr
adjBiom_by_ppt <- compiler::cmpfun(function(biom_shrubs, biom_C3, biom_C4, biom_annuals, biom_maxs,
         map_mm_shrubs, map_mm_std_shrubs,
         map_mm_grasses, map_mm_std_grasses,
         vegcomp_std_shrubs, vegcomp_std_grass, tol. = tol) {

  #Intercepts to match outcomes of M & L 1993 equations under 'default' MAP with our previous default inputs for shrubs and sgs-grasslands
  #Whereas these intercepts were introduced artificially, they could also be interpreted as perennial storage, e.g., Lauenroth & Whitman (1977) found "Accumulation in the standing dead was 63% of inputs, in the litter 8%, and belowground 37%.". Lauenroth, W.K. & Whitman, W.C. (1977) Dynamics of dry matter production in a mixed-grass prairie in western North Dakota. Oecologia, 27, 339-351.
  Shrub_ANPPintercept <- (vegcomp_std_shrubs[1] * biom_maxs["Sh.Amount.Live"] +
                          vegcomp_std_shrubs[2] * biom_maxs["C3.Amount.Live"] +
                          vegcomp_std_shrubs[3] * biom_maxs["C4.Amount.Live"]) -
                        Shrub_ANPP(map_mm_std_shrubs)
  Grasses_ANPPintercept <- (vegcomp_std_grass[1] * biom_maxs["Sh.Amount.Live"] +
                            vegcomp_std_grass[2] * biom_maxs["C3.Amount.Live"] +
                            vegcomp_std_grass[3] * biom_maxs["C4.Amount.Live"]) -
                          Grass_ANPP(map_mm_std_grasses)

  #Get scaling values for scaled biomass; guarantee that > minimum.totalBiomass
  minimum.totalBiomass <- 0 #This is a SoilWat parameter
  Shrub_BiomassScaler <- max(minimum.totalBiomass, Shrub_ANPP(map_mm_shrubs) + Shrub_ANPPintercept)
  Grass_BiomassScaler <- max(minimum.totalBiomass, Grass_ANPP(map_mm_grasses) + Grasses_ANPPintercept)

  #Scale live biomass amount by productivity; assumption: ANPP = peak standing live biomass
  biom_shrubs$Sh.Amount.Live <- biom_shrubs$Sh.Amount.Live * Shrub_BiomassScaler
  biom_C3$C3.Amount.Live <- biom_C3$C3.Amount.Live * Grass_BiomassScaler
  biom_C4$C4.Amount.Live <- biom_C4$C4.Amount.Live * Grass_BiomassScaler
  biom_annuals$Annual.Amount.Live <- biom_annuals$Annual.Amount.Live * Grass_BiomassScaler

  #Scale litter amount by productivity and adjust for ratio of litter/live
  biom_shrubs$Sh.Litter <- biom_shrubs$Sh.Litter * Shrub_BiomassScaler * biom_maxs["Sh.Litter"] / biom_maxs["Sh.Amount.Live"]
  biom_C3$C3.Litter <- biom_C3$C3.Litter * Grass_BiomassScaler * biom_maxs["C3.Litter"] / biom_maxs["C3.Amount.Live"]
  biom_C4$C4.Litter <- biom_C4$C4.Litter * Grass_BiomassScaler * biom_maxs["C4.Litter"] / biom_maxs["C4.Amount.Live"]
  biom_annuals$Annual.Litter <- biom_annuals$Annual.Litter * Grass_BiomassScaler * biom_maxs["Annual.Litter"] / biom_maxs["Annual.Amount.Live"]

  #Guarantee that live fraction = ]0, 1]
  biom_shrubs$Sh.Perc.Live <- pmin(1, pmax(tol., biom_shrubs$Sh.Perc.Live))
  biom_C3$C3.Perc.Live <- pmin(1, pmax(tol., biom_C3$C3.Perc.Live))
  biom_C4$C4.Perc.Live <- pmin(1, pmax(tol., biom_C4$C4.Perc.Live))
  biom_annuals$Annual.Perc.Live <- pmin(1, pmax(tol., biom_annuals$Annual.Perc.Live))

  #Calculate total biomass based on scaled live biomass amount
  biom_shrubs$Sh.Biomass <- biom_shrubs$Sh.Amount.Live / biom_shrubs$Sh.Perc.Live
  biom_C3$C3.Biomass <- biom_C3$C3.Amount.Live / biom_C3$C3.Perc.Live
  biom_C4$C4.Biomass <- biom_C4$C4.Amount.Live / biom_C4$C4.Perc.Live
  biom_annuals$Annual.Biomass <- biom_annuals$Annual.Amount.Live / biom_annuals$Annual.Perc.Live

  list(biom_shrubs = biom_shrubs,
       biom_C3 = biom_C3,
       biom_C4 = biom_C4,
       biom_annuals = biom_annuals)
})

calc_starts <- compiler::cmpfun(function(x) {
  temp1 <- rle(as.logical(x))
  temp2 <- cumsum(c(0, temp1$lengths)) + 1
  temp2[-length(temp2)][temp1$values]
})

AdjMonthlyBioMass <- compiler::cmpfun(function(tr_VegBiom,
                do_adjBiom_by_temp = FALSE, do_adjBiom_by_ppt = FALSE,
                fgrass_c3c4ann, growing_limit_C = 4,
                isNorth = TRUE, MAP_mm = 450, monthly.temp) {

  #Default shrub biomass input is at MAP = 450 mm/yr, and default grass biomass input is at MAP = 340 mm/yr
  #Describe conditions for which the default vegetation biomass values are valid
  std.winter <- c(11:12, 1:2) #Assumes that the "growing season" (valid for growing_limit_C == 4) in 'tr_VegetationComposition' starts in March and ends after October, for all functional groups.
  std.growing <- seq_len(12L)[-std.winter] #Assumes that the "growing season" in 'tr_VegetationComposition' starts in March and ends after October, for all functional groups.
  #Default site for the grass description is SGS LTER
  StandardGrasses_MAP_mm <- 340
  StandardGrasses_VegComposition <- c(0.12, 0.22, 0.66) #Fraction of shrubs, C3, and C4
  #Default site for the shrub description is Reynolds Creek, ID
  StandardShrub_MAP_mm <- 250
  StandardShrub_VegComposition <- c(0.7, 0.3, 0) #Fraction of shrubs, C3, and C4

  #Calculate 'live biomass amount'
  tr_VegBiom$Sh.Amount.Live <- tr_VegBiom$Sh.Biomass * tr_VegBiom$Sh.Perc.Live
  tr_VegBiom$C3.Amount.Live <- tr_VegBiom$C3.Biomass * tr_VegBiom$C3.Perc.Live
  tr_VegBiom$C4.Amount.Live <- tr_VegBiom$C4.Biomass * tr_VegBiom$C4.Perc.Live
  tr_VegBiom$Annual.Amount.Live <- tr_VegBiom$Annual.Biomass * tr_VegBiom$Annual.Perc.Live

  #Scale monthly values of litter and live biomass amount by column-max; total biomass will be back calculated from 'live biomass amount' / 'percent live'
  itemp <- grepl("Litter", names(tr_VegBiom)) | grepl("Amount.Live", names(tr_VegBiom))
  colmax <- apply(tr_VegBiom[, itemp], MARGIN=2, FUN=max)
#  colmin <- apply(tr_VegBiom[, itemp], MARGIN=2, FUN=min)
  tr_VegBiom[, itemp] <- sweep(tr_VegBiom[, itemp], MARGIN=2, STATS=colmax, FUN="/")

  #Pull different vegetation types
  biom_shrubs <- biom_std_shrubs <- tr_VegBiom[, grepl("Sh", names(tr_VegBiom))]
  biom_C3 <- biom_std_C3 <- tr_VegBiom[, grepl("C3", names(tr_VegBiom))]
  biom_C4 <- biom_std_C4 <- tr_VegBiom[, grepl("C4", names(tr_VegBiom))]
  biom_annuals <- biom_std_annuals <- tr_VegBiom[, grepl("Annual", names(tr_VegBiom))]

  #adjust phenology for mean monthly temperatures
  if (do_adjBiom_by_temp) {
    growing.season <- as.vector(monthly.temp >= growing_limit_C)
    n_nonseason <- sum(!growing.season)
    n_season <- sum(growing.season)

    if (!isNorth)
      growing.season <- c(growing.season[7:12], growing.season[1:6]) #Standard growing season needs to be adjusted for southern Hemi

    #Adjust for timing and duration of non-growing season
    if (n_nonseason > 0) {
      if (n_nonseason < 12) {
        std.winter.padded <- (c(std.winter[1] - 1, std.winter, std.winter[length(std.winter)] + 1) - 1) %% 12 + 1
        std.winter.seq <- 0:(length(std.winter.padded) - 1)
        site.winter.seq <- seq(from = 1, to = length(std.winter), length = n_nonseason)
        starts <- calc_starts(!growing.season)
        site.winter.start <- starts[length(starts)] #Calculate first month of winter == last start of non-growing season
        site.winter.months <- (site.winter.start + seq_len(n_nonseason) - 2) %% 12 + 1

        biom_shrubs[site.winter.months,] <- predict.season(biom_std_shrubs, std.winter.padded, std.winter.seq, site.winter.seq)
        biom_C3[site.winter.months,] <- predict.season(biom_std_C3, std.winter.padded, std.winter.seq, site.winter.seq)
        biom_C4[site.winter.months,] <- predict.season(biom_std_C4, std.winter.padded, std.winter.seq, site.winter.seq)
        biom_annuals[site.winter.months,] <- predict.season(biom_std_annuals, std.winter.padded, std.winter.seq, site.winter.seq)

      } else { #if winter lasts 12 months
        #Take the mean of the winter months
        biom_shrubs[] <- matrix(apply(biom_std_shrubs[std.winter,], 2, mean), nrow=12, ncol=ncol(biom_shrubs), byrow=TRUE)
        biom_C3[] <- matrix(apply(biom_std_C3[std.winter,], 2, mean), nrow=12, ncol=ncol(biom_C3), byrow=TRUE)
        biom_C4[] <- matrix(apply(biom_std_C4[std.winter,], 2, mean), nrow=12, ncol=ncol(biom_C4), byrow=TRUE)
        biom_annuals[] <- matrix(apply(biom_std_annuals[std.winter,], 2, mean), nrow=12, ncol=ncol(biom_annuals), byrow=TRUE)
      }
    }

    #Adjust for timing and duration of growing season
    if (n_season > 0) {
      if (n_season < 12) {
        std.growing.padded <- (c(std.growing[1] - 1, std.growing, std.growing[length(std.growing)] + 1) - 1) %% 12 + 1
        std.growing.seq <- 0:(length(std.growing.padded) - 1)
        site.growing.seq <- seq(from = 1, to = length(std.growing), length = n_season)
        starts <- calc_starts(growing.season)
        site.growing.start <- starts[1] #Calculate first month of growing season == first start of growing season
        site.growing.months <- (site.growing.start + seq_len(n_season) - 2) %% 12 + 1

        biom_shrubs[site.growing.months,] <- predict.season(biom_std_shrubs, std.growing.padded, std.growing.seq, site.growing.seq)
        biom_C3[site.growing.months,] <- predict.season(biom_std_C3, std.growing.padded, std.growing.seq, site.growing.seq)
        biom_C4[site.growing.months,] <- predict.season(biom_std_C4, std.growing.padded, std.growing.seq, site.growing.seq)
        biom_annuals[site.growing.months,] <- predict.season(biom_std_annuals, std.growing.padded, std.growing.seq, site.growing.seq)

      } else { #if growing season lasts 12 months
        biom_shrubs[] <- matrix(apply(biom_std_shrubs[std.growing,], MARGIN=2, FUN=max), nrow=12, ncol=ncol(biom_shrubs), byrow=TRUE)
        biom_C3[] <- matrix(apply(biom_std_C3[std.growing,], MARGIN=2, FUN=max), nrow=12, ncol=ncol(biom_C3), byrow=TRUE)
        biom_C4[] <- matrix(apply(biom_std_C4[std.growing,], MARGIN=2, FUN=max), nrow=12, ncol=ncol(biom_C4), byrow=TRUE)
        biom_annuals[] <- matrix(apply(biom_std_annuals[std.growing,], MARGIN=2, FUN=max), nrow=12, ncol=ncol(biom_annuals), byrow=TRUE)
      }
    }

    if (!isNorth) { #Adjustements were done as if on northern hemisphere
      biom_shrubs <- rbind(biom_shrubs[7:12,], biom_shrubs[1:6,])
      biom_C3 <- rbind(biom_C3[7:12,], biom_C3[1:6,])
      biom_C4 <- rbind(biom_C4[7:12,], biom_C4[1:6,])
      biom_annuals <- rbind(biom_annuals[7:12,], biom_annuals[1:6,])
    }
  }

  # if (do_adjBiom_by_ppt) then adjust biomass amounts by productivity relationship with MAP
  temp <- adjBiom_by_ppt(
    biom_shrubs, biom_C3, biom_C4, biom_annuals,
    biom_maxs = colmax,
    map_mm_shrubs = if (do_adjBiom_by_ppt) MAP_mm else StandardShrub_MAP_mm,
    map_mm_std_shrubs = StandardShrub_MAP_mm,
    map_mm_grasses = if (do_adjBiom_by_ppt) MAP_mm else StandardGrasses_MAP_mm,
    map_mm_std_grasses = StandardGrasses_MAP_mm,
    vegcomp_std_shrubs = StandardShrub_VegComposition,
    vegcomp_std_grass = StandardGrasses_VegComposition)

  biom_shrubs <- temp$biom_shrubs
  biom_C3 <- temp$biom_C3
  biom_C4 <- temp$biom_C4
  biom_annuals <- temp$biom_annuals

  biom_grasses <- biom_C3 * fgrass_c3c4ann[1] +
                  biom_C4 * fgrass_c3c4ann[2] +
                  biom_annuals * fgrass_c3c4ann[3]

  list(grass = as.matrix(biom_grasses),
       shrub = as.matrix(biom_shrubs))
})

#' Lookup transpiration coefficients for grasses, shrubs, and trees per soil layer or per soil depth increment of 1 cm per distribution type for each simulation run and copy values to 'datafile.soils'
#'
#' first row of datafile is label for per soil layer 'Layer' or per soil depth increment of 1 cm 'DepthCM'
#' second row of datafile is source of data
#' the other rows contain the data for each distribution type = columns
#' @section Note:
#'  cannot write data from sw_input_soils to datafile.soils
TranspCoeffByVegType <- compiler::cmpfun(function(tr_input_code, tr_input_coeff,
  soillayer_no, trco_type, layers_depth,
  adjustType = c("positive", "inverse", "allToLast")) {

  #extract data from table by category
  trco.code <- as.character(tr_input_code[, which(colnames(tr_input_code) == trco_type)])
  trco <- rep(0, times = soillayer_no)
  trco.raw <- na.omit(tr_input_coeff[, which(colnames(tr_input_coeff) == trco_type)])

  if (trco.code == "DepthCM") {
    temp <- sum(trco.raw, na.rm = TRUE)
    trco_sum <- if (temp == 0 || is.na(temp)) 1L else temp
    lup <- 1
    for(l in 1:soillayer_no){
      llow <- as.numeric(layers_depth[l])
      if(is.na(llow) | lup > length(trco.raw))
      {
        l <- l - 1
        break
      }
      trco[l] <- sum(trco.raw[lup:llow], na.rm=TRUE) / trco_sum
      lup <- llow + 1
    }
    usel <- l
  } else if(trco.code == "Layer"){
    usel <- ifelse(length(trco.raw) < soillayer_no, length(trco.raw), soillayer_no)
    trco[1:usel] <- trco.raw[1:usel] / ifelse((temp <- sum(trco.raw[1:usel], na.rm=TRUE)) == 0 & is.na(temp), 1, temp)
  }

  if(identical(adjustType, "positive")){
    trco <- trco / sum(trco)	#equivalent to: trco + (1 - sum(trco)) * trco / sum(trco)
  } else if(identical(adjustType, "inverse")){
    irows <- 1:max(which(trco > 0))
    trco[irows] <- trco[irows] + rev(trco[irows]) * (1 / sum(trco[irows]) - 1)	#equivalent to: trco + (1 - sum(trco)) * rev(trco) / sum(trco)
  } else if(identical(adjustType, "allToLast")){
    irow <- max(which(trco > 0))
    if(irow > 1){
      trco[irow] <- 1 - sum(trco[1:(irow - 1)]) 	#adding all the missing roots because soil is too shallow to the deepest available layer
    } else {
      trco[1] <- 1
    }
  }

  trco
})


check_soil_data <- compiler::cmpfun(function(data) {
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
})


#Circular functions: int=number of units in circle, e.g., for days: int=365; for months: int=12
circ.mean <- compiler::cmpfun(function(x, int, na.rm = FALSE) {
  if (!all(is.na(x))) {
    circ <- 2 * pi / int
    x_circ <- circular::circular(x * circ, type = "angles", units = "radians", rotation = "clock", modulo = "2pi")
    x_int <- circular::mean.circular(x_circ, na.rm = na.rm) / circ

    round(as.numeric(x_int) - 1, 13) %% int + 1	# map 0 -> int; rounding to 13 digits: 13 was empirically derived for int={12, 365} and x=c((-1):2, seq(x-5, x+5, by=1), seq(2*x-5, 2*x+5, by=1)) assuming that this function will never need to calculate for x > t*int with t>2
  } else {
    NA
  }
})

circ.range <- compiler::cmpfun(function(x, int, na.rm = FALSE) {
  if (!all(is.na(x))) {
    circ <- 2 * pi / int
    x_circ <- circular::circular(x * circ, type = "angles", units = "radians", rotation = "clock", modulo = "2pi")
    x_int <- range(x_circ, na.rm = na.rm) / circ
    as.numeric(x_int)

  } else {
    NA
  }
})

circ.sd <- compiler::cmpfun(function(x, int, na.rm=FALSE){
  if (length(x) - sum(is.na(x)) > 1) {
    if (sd(x, na.rm = TRUE) > 0) {
      circ <- 2 * pi / int
      x_circ <- circular::circular(x * circ, type = "angles", units = "radians", rotation = "clock", modulo = "2pi")
      x_int <- circular::sd.circular(x_circ, na.rm = na.rm) / circ
      as.numeric(x_int)
    } else {
      0
    }
  } else {
    NA
  }
})


#functions wet and dry periods

#' Saturation vapor pressure
#'
#' @param T A numeric vector of temperature(s) (deg C)
#' @return A numeric vector of length \code{T} of saturation vapor pressure (kPa) at
#'    temperature T
#' @references Yoder, R. E., L. O. Odhiambo, and W. C. Wright. 2005. Effects of Vapor-Pressure Deficit and Net-Irradiance Calculation Methods on Accuracy of Standardized Penman-Monteith Equation in a Humid Climate Journal of Irrigation and Drainage Engineering 131:228-237.
vp0 <- compiler::cmpfun(function(T) {
  0.6108 * exp(17.27 * T / (T + 273.3))	# eq. 5 of Yoder et al. 2005
})


#' Vapor pressure deficit
#'
#' @param Tmin A numeric vector of daily minimum temperature(s) (deg C)
#' @param Tmax A numeric vector of daily maximum temperature(s) (deg C)
#' @param RHmean A numeric vector of daily mean relative humidity (percentage)
#' @return A numeric vector of length \code{T} of vapor pressure deficit (kPa)
#' @references Yoder, R. E., L. O. Odhiambo, and W. C. Wright. 2005. Effects of Vapor-Pressure Deficit and Net-Irradiance Calculation Methods on Accuracy of Standardized Penman-Monteith Equation in a Humid Climate Journal of Irrigation and Drainage Engineering 131:228-237.
vpd <- compiler::cmpfun(function(Tmin, Tmax, RHmean = NULL) {
  if (is.null(RHmean)) {
    (vp0(Tmax) - vp0(Tmin)) / 2	# eq. 6 - eq. 13 of Yoder et al. 2005 (VPD6 in Table 4)
  } else {
    (vp0(Tmax) + vp0(Tmin)) / 2 * (1 - RHmean / 100)	# eq. 6 - eq. 11 of Yoder et al. 2005 (VPD4 in Table 4)
  }
})


#' @param x A numeric vector
#' @param fun A function which requires one argument. \code{fun} will be applied to
#'    the k-largest values of \code{x}.
#' @param k An integer value. The k-largest value(s) of \code{x} will be used. The largest
#'    value will be used if 0 or negative.
#' @param na.rm A logical value
#' @param ... Optional arguments to be passed to \code{fun}
#'
#' @return A vector with the k-largest values of \code{x} if \code{is.null(fun)},
#'    otherwise the result of applying \code{fun} to the k-largest values.
fun_kLargest <- compiler::cmpfun(function(x, fun = NULL, k = 10L, na.rm = FALSE, ...) {
  if (na.rm)
    x <- na.exclude(x)
  x <- sort.int(x, decreasing = TRUE, na.last = !na.rm, method = if (getRversion() >= "3.3.0") "radix" else "quick")
  x <- x[seq_len(max(1L, min(length(x), as.integer(k))))]

  if (is.null(fun)) x else fun(x, ...)
})

max.duration <- compiler::cmpfun(function(x, target_val = 1L, return_doys = FALSE) {
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
})

startDoyOfDuration <- compiler::cmpfun(function(x, duration=10) {
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
})

endDoyAfterDuration <- compiler::cmpfun(function(x, duration=10) {
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
})


#' Calculates temperate dryland criteria
#'
#' @param annualPPT A numeric vector. Annual precipitation values.
#' @param annualPET A numeric vector. Annual potential evapotranspiration values.
#'  The values must be in the same units as those of \code{annualPPT}, e.g., \code{mm}.
#' @monthlyTemp A numeric vector. Monthly mean air temperature in degree Celsius for each
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
calc_drylandindices <- compiler::cmpfun(function(annualPPT, annualPET, monthlyTemp, ai_limit = 0.5) {
  ai <- annualPPT / annualPET	#Deichmann, U. & L. Eklundh. 1991. Global digital datasets for land degradation studies: a GIS approach. Global Environment Monitoring System (GEMS), United Nations Environment Programme (UNEP), Nairobi, Kenya.
  temp <- matrix(monthlyTemp >= 10, nrow = 12)
  temp <- .colSums(temp, nrow(temp), ncol(temp))
  TD <- temp >= 4 & temp < 8 #Trewartha & Horn 1980, page 284: temperate areas
  criteria12 <- as.integer(TD & ai < ai_limit)

  list(ai = ai, temperateness = TD, criteria12 = criteria12)
})


extreme_values_and_doys <- compiler::cmpfun(function(x, tol = sqrt(.Machine$double.eps), na.rm = FALSE) {
  tmax <- max(x, na.rm = na.rm)
  tmin <- min(x, na.rm = na.rm)

  c(tmax, tmin,
    circ.mean(which(abs(x - tmax) < tol), int = 365, na.rm = na.rm),
    circ.mean(which(abs(x - tmin) < tol), int = 365, na.rm = na.rm))
})


handle_NAs <- compiler::cmpfun(function(x, na.index, na.act) {
  if (length(na.index) > 0) {
    napredict(na.act, x)
  } else {
    x
  }
})

scale_by_sum <- compiler::cmpfun(function(x) {
  temp <- sum(x, na.rm = TRUE)
  if (temp > 0 && is.finite(temp)) {
    x / temp
  } else {
    x
  }
})




#two, three, or four layer aggregation for average daily aggregation output
setAggSoilLayerForAggDailyResponses <- compiler::cmpfun(function(layers_depth, daily_lyr_agg){
  d <- length(layers_depth)
  vals <- list()
  #first layer
  DeepestFirstDailyAggLayer <- findInterval(daily_lyr_agg[["first_cm"]], c(0, layers_depth) + tol, all.inside=TRUE)
  vals[[1]] <- seq_len(DeepestFirstDailyAggLayer)
  #second layer
  if(!is.null(daily_lyr_agg[["second_cm"]])){
    DeepestSecondDailyAggLayer <- findInterval(daily_lyr_agg[["second_cm"]], c(0, layers_depth) + tol, all.inside=TRUE)
  } else {
    DeepestSecondDailyAggLayer <- d
  }
  if(is.numeric(DeepestSecondDailyAggLayer) && is.numeric(DeepestFirstDailyAggLayer) && d > DeepestFirstDailyAggLayer){
    vals[[2]] <- (DeepestFirstDailyAggLayer+1):DeepestSecondDailyAggLayer
  }
  #third layer
  if(!is.null(daily_lyr_agg[["third_cm"]])){
    if(!is.na(daily_lyr_agg[["third_cm"]])){
      DeepestThirdDailyAggLayer <- findInterval(daily_lyr_agg[["third_cm"]], c(0, layers_depth) + tol, all.inside=TRUE)
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
      DeepestFourthDailyAggLayer <- findInterval(daily_lyr_agg[["fourth_cm"]], c(0, layers_depth) + tol, all.inside=TRUE)
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
})


#function to extrapolate windspeeds measured at heights different than SoilWat required 2-m above ground
adjust.WindspeedHeight <- compiler::cmpfun(function(uz, height){
  # Allen RG, Walter IA, Elliott R, Howell T, Itenfisu D, Jensen M (2005) In The ASCE standardized reference evapotranspiration equation, pp. 59. ASCE-EWRI Task Committee Report.
  # input: windspeed [m/s] at height x
  # output: windspeed [m/s] at height 2 m

  stopifnot(all(uz >= 0) && height >= 2 )
  return( uz * 4.87 / log(67.8 * height - 5.42) )	# eqn. 33 in Allen et al. (2005)
})


get.LookupFromTable <- compiler::cmpfun(function(pattern, trtype, tr_input, sw_input_use, sw_input, nvars) {
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
})

fill_empty <- compiler::cmpfun(function(data, pattern, fill, tol = tol) {
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
})

#' Split soil layer in two layers
#'
#' @details The method \code{interpolate} calculates the weighted mean of the columns/layers
#'  \code{il} and \code{il + 1}.
#'  The method \code{exhaust} distributes the value of \code{il + 1} according to the weights.
#'
#' @param x A numeric data.frame or matrix. Columns are soil layers.
#' @param il An integer value. The column/soil layer number after which a new layer is added.
#' @param w A numeric vector of length two. The weights used to calculate the values of the new layer.
#' @param method A character string. \link{@details}
#'
#' @return An object like x with one column more at position \code{il + 1}.
add_layer_to_soil <- compiler::cmpfun(function(x, il, w, method = c("interpolate", "exhaust")) {
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
})

identify_soillayers <- compiler::cmpfun(function(depths, sdepth) {
  it <- findInterval(depths, sdepth)
  if (any(is.na(it))) {
    as.integer(na.exclude(it))
  } else if (length(it) > 1 && diff(it) > 0) {
    (1 + it[1]):(it[2])
  } else {
    it[1]
  }
})

adjustLayer_byImp <- compiler::cmpfun(function(depths, imp_depth, sdepths) {
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
})

EstimateInitialSoilTemperatureForEachSoilLayer <- compiler::cmpfun(function(layers_depth, lower.Tdepth, soilTupper, soilTlower){
  sl <- c(0, lower.Tdepth)
  st <- c(soilTupper, soilTlower)
  return( predict(lm(st ~ sl), data.frame(sl=layers_depth)) )
})

#--put information from experimental design into appropriate input variables; create_treatments and the _use files were already adjusted for the experimental design when files were read in/created
transferExpDesignToInput <- compiler::cmpfun(function(x, i_exp, df_exp, df_exp_use) {
  temp <- match(names(df_exp)[df_exp_use], names(x), nomatch = 0)
  ctemp <- temp[!(temp == 0)]
  if (length(ctemp) > 0) {
    cexp <- match(names(x)[ctemp], names(df_exp), nomatch = 0)
    x[ctemp] <- df_exp[i_exp, cexp]
  }
  x
})

setDeepestTopLayer <- compiler::cmpfun(function(layers_depth, Depth_TopLayers) {
  max(1, findInterval(Depth_TopLayers, layers_depth))
})

setTopLayer <- compiler::cmpfun(function(d, DeepestTopLayer) {
  seq_len(if(d < DeepestTopLayer) d else DeepestTopLayer)
})

setBottomLayer <- compiler::cmpfun(function(d, DeepestTopLayer) {
  if (d <= DeepestTopLayer) {
    NULL
  } else {
    (DeepestTopLayer + 1L):d
  }
})

tempError <- compiler::cmpfun(function() .Call("tempError"))


cor2  <- compiler::cmpfun(function(y) {
	res <- try(cor(y[, 1], y[, 2]), silent = TRUE)
	if (inherits(res, "try-error")) NA else res
})

#data is the values for one year adj for SWPcrit_MPa; TRUE==dry
EventDistribution <- compiler::cmpfun(function(data, N, size) {
  bins <- rep(0, times = N)
  temp <- rle(data)
  temp <- temp$lengths[temp$values]
  if (length(temp) > 0) for (z in seq_along(temp)) {
    ix <- findInterval(temp[z], size)
    bins[ix] <- bins[ix] + 1
  }
  bins
})

daily_spells_permonth <- compiler::cmpfun(function(x, simTime2) {
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
})

tabulate_values_in_bins <- compiler::cmpfun(function(x, method = c("duration", "values"),
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
})



update_biomass <- compiler::cmpfun(function(funct_veg = c("Grass", "Shrub", "Tree", "Forb"), use, prod_input, prod_default) {
  funct_veg <- match.arg(funct_veg)

  comps <- c("_Litter", "_Biomass", "_FractionLive", "_LAIconv")
  veg_ids = lapply(comps, function(x)
    grep(paste0(funct_veg, x), names(use)))
  veg_incl = lapply(veg_ids, function(x) use[x])

  temp <- slot(prod_default, paste0("MonthlyProductionValues_", tolower(funct_veg)))
  if (any(unlist(veg_incl))) {
    for (k in seq_along(comps)) if (any(veg_incl[[k]]))
      temp[veg_incl[[k]], k] <- as.numeric(prod_input[, veg_ids[[k]][veg_incl[[k]]]])
  }

  temp
})

