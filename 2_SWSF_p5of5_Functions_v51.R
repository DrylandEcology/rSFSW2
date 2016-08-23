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

set_PRAGMAs <- compiler::cmpfun(function(con, settings) {
  temp <- lapply(settings, function(x) RSQLite::dbGetQuery(con, x))
  invisible(0)
})

getSiteIds <- compiler::cmpfun(function(con, folderNames) {
  wf_ids <- RSQLite::dbGetQuery(con, "SELECT id, folder FROM weatherfolders")
  wf_ids[match(folderNames, wf_ids[, "folder"], nomatch = NA), "id"]
})

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

.Last <- compiler::cmpfun(function() { #Properly end mpi slaves before quitting R (e.g., at a crash)
  if (is.loaded("mpi_initialize") && exists("mpi.comm.size")) {
    if (Rmpi::mpi.comm.size(1) > 0)
      Rmpi::mpi.close.Rslaves()
    .Call("mpi_finalize")
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
    if(!file.exists(path)) {
      print("trying to make directory again")
      dir.create2(path, showWarnings, TRUE, mode, (times+1)) #recursively call the function b/c when run on JANUS with MPI it doesn't seem to make the directories everytime... quite aggravating.
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
    sapply(dir.list, function(x) {dir.copy(dir.from=file.path(dir.from, x), dir.to=file.path(dir.to, x), overwrite=overwrite)})
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
    sapply(dir.list, function(x) {dir.remove(dir=file.path(dir, x))})
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

# iterator functions
#' @param isim An integer value. A value of \code{runIDs_todo} as subset of \code{runIDs_total}.
#' @details NOTE: Do not change the iterators without adjusting the design of the output databases!
it_exp <- compiler::cmpfun(function(isim, runN = runsN_master) (isim - 1L) %/% runN + 1L)
it_site <- compiler::cmpfun(function(isim, runN = runsN_master) runN[(isim - 1L) %% runN + 1L])
it_Pid_old <- compiler::cmpfun(function(isim, sc, scN = scenario_No) (isim - 1L) * scN + sc)
it_Pid <- compiler::cmpfun(function(isim, sc, scN = scenario_No, runN = runsN_master) {
  ((it_exp(isim, runN) - 1L) * runN + it_site(isim, runN) - 1L) * scN + sc
})

## Tests
#include_YN <- c(0, 0, 1, 0, 0, 1, 1, 0)
#include_YN <- rep(1, 8)
#t(sapply(runIDs_todo, function(isim) c(isim, it_site(isim), it_exp(isim), it_Pid(isim, 1), it_Pid_old(isim, 1))))
#t(sapply(runIDs_total, function(isim) c(isim, it_site(isim), it_exp(isim), it_Pid(isim, 1), it_Pid_old(isim, 1))))

exportObjects <- compiler::cmpfun(function(allObjects) {
  print("exporting objects from master node to slave nodes")
  t.bcast <- Sys.time()
  for(obj in 1:length(allObjects)) {
    bcast.tempString <- allObjects[obj]
    bcast.tempValue <- try(eval(as.name(allObjects[obj])))
    if(!inherits(bcast.tempValue, "try-error")){
      Rmpi::mpi.bcast.Robj2slave(bcast.tempString)
      Rmpi::mpi.bcast.Robj2slave(bcast.tempValue)
      Rmpi::mpi.bcast.cmd(cmd=try(assign(bcast.tempString, bcast.tempValue)))
    } else {
      print(paste(obj, bcast.tempString, "not successful"))
    }
  }
  print(paste("object export took", round(difftime(Sys.time(), t.bcast, units="secs"), 2), "secs"))
})
    
load_NCEPCFSR_shlib <- compiler::cmpfun(function(cfsr_so){
  if(!is.loaded("writeMonthlyClimate_R")) dyn.load(cfsr_so) # load because .so is available
  invisible(0)
})

prepare_NCEPCFSR_extraction <- compiler::cmpfun(function(dir.big, dir.cfsr.data, dir.cfsr.code = dir.cfsr.data) {
  dir.create(dir.in.cfsr <- file.path(dir.big, "ncepcfsr"), showWarnings=FALSE)
  fname_cfsr <- file.path(dir.in.cfsr, "cfsr_convert.so")

  .local <- function(){
    #Check for the shared object 'cfsr_convert.so' that contains the C functions accessible to R
    if(!file.exists(fname_cfsr)){ # compile
      dtemp <- getwd()
      setwd(dir.cfsr.code)
      stopifnot(file.exists("cfsr_convert.c", "generic2.c", "generic2.h", "filefuncs2.c", "filefuncs2.h", "mymemory2.c", "mymemory2.h"))
      unlink(c("cfsr_convert.o", "generic2.o", "filefuncs2.o", "mymemory2.o"))
      stopifnot(system2(command=file.path(Sys.getenv()[["R_HOME"]], "R"), args=paste("CMD SHLIB -o", fname_cfsr, "cfsr_convert.c generic2.c filefuncs2.c mymemory2.c"), wait=TRUE) == 0)
      setwd(dtemp)
    }
    load_NCEPCFSR_shlib(fname_cfsr)

    #Check for wgrib2 (http://www.cpc.ncep.noaa.gov/products/wesley/wgrib2/)
    if(!file.exists(wgrib2 <- file.path(dir.in.cfsr, "wgrib2"))){
      temp2 <- if(nchar(temp <- Sys.which("wgrib2")) > 0) temp else if(file.exists(temp <- "/opt/local/bin/wgrib2")) temp else ""
      stopifnot(nchar(temp2) > 0)
      file.copy(from=temp2, to=wgrib2)
    }

    #Soft link to gribbed data
    fname_gribDir <- "griblargeC2"
    dir.grib <- file.path(dir.in.cfsr, fname_gribDir)
    if(!file.exists(dir.grib)){ # value of gribDir defined in cfsr_convert.c
      stopifnot(system2(command="ln", args=paste("-s", file.path(dir.cfsr.data, fname_gribDir), dir.grib)) == 0)
    }

    #Set up temporary directory for C code to store objects
    if(file.exists(ftemp <- file.path(dir.in.cfsr, "temporary_dy"))) unlink(ftemp, recursive=TRUE)
    temp <- lapply(lapply(c("tmax", "tmin", "ppt"), FUN=function(x) file.path(ftemp, x)), FUN=function(x) dir.create(x, recursive=TRUE, showWarnings=FALSE))

    0L
  }

  temp <- .local()
  res <- if(!inherits(temp, "try-error")) list(dir.in.cfsr=dir.in.cfsr, cfsr_so=fname_cfsr)  else temp

  res
})

# Wrapper functions for C code to access NCEP/CFSR data and write out to temporary files
gribDailyWeatherData <- compiler::cmpfun(function(id, do_daily, nSites, latitudes, longitudes) {
  if(id %% 36 == 1) print(paste(Sys.time(), ": NCEP/CFSR extraction: year=", do_daily[id, "years"]))

  gribData <- .C("dailyWeather2_R",
            nSites = as.integer(nSites),
            latitudes = as.double(latitudes),
            longitudes = as.double(longitudes),
            year = as.integer(do_daily[id, "years"]),
            month = as.integer(do_daily[id, "months"]),
            type = as.integer(do_daily[id, "types"]))
  1L
})

writeDailyWeatherData <- compiler::cmpfun(function(year, nSites, siteNames, siteDirsC) {
  dataWrite <- .C("dailyWeather2Write_R",
            nSites = as.integer(nSites),
            siteNames = as.character(siteNames),
            siteDirs = as.character(siteDirsC),
            year = as.integer(year))
  1L
})

gribMonthlyClimate <- compiler::cmpfun(function(type, nSites, latitudes, longitudes, siteDirsC, yearLow, yearHigh) {
  gribData <- .C("monthlyClimate2_R",
            nSites = as.integer(nSites),
            latitudes = as.double(latitudes),
            longitudes = as.double(longitudes),
            siteDirs = as.character(siteDirsC),
            yearLow = as.integer(yearLow),
            yearHigh = as.integer(yearHigh),
            type = as.integer(type))
  1L
})

writeMonthlyClimate <- compiler::cmpfun(function(id, siteDirsC) {
  dataWrite <- .C("writeMonthlyClimate2_R", siteDir = as.character(siteDirsC[id]))
  1L
})

create_filename_for_Maurer2002_NorthAmerica <- function(X_WGS84, Y_WGS84){
  gsub("[[:space:]]", "", paste("data", formatC(28.8125+round((Y_WGS84-28.8125)/0.125,0)*0.125, digits=4, format="f"), formatC(28.8125+round((X_WGS84-28.8125)/0.125,0)*0.125, digits=4, format="f"), sep="_"))
}

simTiming <- compiler::cmpfun(function(startyr, simstartyr, endyr) {
  res <- list()
  #simyrs <- simstartyr:endyr
  #no.simyr <- endyr - simstartyr + 1
  temp <- as.POSIXlt(paste0(startyr, "-01-01"))
  
  res[["useyrs"]] <- startyr:endyr

  res[["no.useyr"]] <- endyr - startyr + 1
  res[["no.usemo"]] <- res[["no.useyr"]] * 12
  res[["no.usedy"]] <- as.numeric(as.POSIXlt(paste0(endyr, "-12-31")) - temp) + 1

  res[["discardyr"]] <- startyr - simstartyr
  res[["discardmo"]] <- res[["discardyr"]] * 12
  res[["discarddy"]] <- as.numeric(temp - as.POSIXlt(paste0(simstartyr, "-01-01")))

  res[["index.useyr"]] <- res[["discardyr"]] + seq_len(res[["no.useyr"]])
  res[["index.usemo"]] <- res[["discardmo"]] + seq_len(res[["no.usemo"]])
  res[["index.usedy"]] <- res[["discarddy"]] + seq_len(res[["no.usedy"]])

  res
})

simTiming_ForEachUsedTimeUnit <- compiler::cmpfun(function(st, sim_tscales, latitude = 90, account_NorthSouth = TRUE) {	#positive latitudes -> northern hemisphere; negative latitudes -> southern hemisphere
  res <- list()
  
  if (any(sim_tscales == "daily")) {
    temp <- as.POSIXlt(seq(from = as.POSIXlt(paste0(min(st$useyrs), "-01-01")),
                           to = as.POSIXlt(paste0(max(st$useyrs), "-12-31")),
                           by = "1 day"))

    res$doy_ForEachUsedDay <- res$doy_ForEachUsedDay_NSadj <- temp$yday + 1
    res$month_ForEachUsedDay <- res$month_ForEachUsedDay_NSadj <- temp$mon + 1
    res$year_ForEachUsedDay <- res$year_ForEachUsedDay_NSadj <- temp$year + 1900
    
    if (latitude < 0 && account_NorthSouth) {
      dshift <- as.POSIXlt(paste(st$useyrs, 6, 30, sep = "-"))$yday + 1	#new month either at end of year or in the middle because the two halfs (6+6 months) of a year are of unequal length (182 (183 if leap year) and 183 days): I chose to have a new month at end of year (i.e., 1 July -> 1 Jan & 30 June -> 31 Dec; but, 1 Jan -> July 3/4): and instead of a day with doy=366, there are two with doy=182
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
adjustLayersDepth <- compiler::cmpfun(function(layers_depth, d) round(layers_depth[seq_len(d)])) #The wrapper only handles 1-cm resolution of soil depths (maily because of the trco)
getLayersWidth <- compiler::cmpfun(function(layers_depth) diff(c(0, layers_depth)))
setLayerSequence <- compiler::cmpfun(function(d) seq_len(d))

sw_dailyC4_TempVar <- compiler::cmpfun(function(dailyTempMin, dailyTempMean, simTime2) {
  #Variables to estimate percent C4 species in North America: Teeri JA, Stowe LG (1976) Climatic patterns and the distribution of C4 grasses in North America. Oecologia, 23, 1-12.

  Month7th_MinTemp_C <- aggregate(dailyTempMin[simTime2$month_ForEachUsedDay_NSadj == 7], by=list(simTime2$year_ForEachUsedDay_NSadj[simTime2$month_ForEachUsedDay_NSadj == 7]), FUN=min)[, 2]
  LengthFreezeFreeGrowingPeriod_Days <- aggregate(dailyTempMin, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=function(x) {temp <- rle(x > 0); if(any(temp$values)) max(temp$lengths[temp$values], na.rm=TRUE) else 0})[, 2]
  DegreeDaysAbove65F_DaysC <- aggregate(dailyTempMean, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=function(x) sum(ifelse((temp <- x - ((65-32) * 5/9)) > 0, temp, 0)))[, 2]

  nyrs <- seq_along(Month7th_MinTemp_C) #if southern Hemisphere, then 7th month of last year is not included
  res <- c(apply(temp <- cbind(Month7th_MinTemp_C[nyrs], LengthFreezeFreeGrowingPeriod_Days[nyrs], DegreeDaysAbove65F_DaysC[nyrs]), MARGIN=2, FUN=mean), apply(temp, MARGIN=2, FUN=sd))
  names(res) <- c(temp <- c("Month7th_NSadj_MinTemp_C", "LengthFreezeFreeGrowingPeriod_NSadj_Days", "DegreeDaysAbove65F_NSadj_DaysC"), paste(temp, ".sd", sep=""))

  res
})

sw_SiteClimate_Ambient <- compiler::cmpfun(function(weatherList, year.start, year.end, do.C4vars=FALSE, simTime2=NULL) {
  sw.weather.suffices <- as.numeric(names(weatherList))
  itemp <- year.start <= sw.weather.suffices & year.end >= sw.weather.suffices
  years <- sw.weather.suffices[itemp]

  tempMean <- tempMin <- tempMax <- ppt <- rep(0, times=12)
  mat <- NULL
  if(do.C4vars){
    dailyTempMin <- NULL
    dailyTempMean <- NULL
  }
  if((no.yrs <- length(years)) > 0) for(y in 1:no.yrs){
      temp.dailyTempMean <- apply(get_swWeatherData(weatherList, years[y])@data[, 2:3], 1, mean)
      temp.dailyTempMin <- get_swWeatherData(weatherList, years[y])@data[, 3]
      temp.dailyTempMax <- get_swWeatherData(weatherList, years[y])@data[, 2]
      mat <- c(mat, mean(temp.dailyTempMean))
      if(do.C4vars){
        dailyTempMin <- c(dailyTempMin, get_swWeatherData(weatherList, years[y])@data[, 3])
        dailyTempMean <- c(dailyTempMean, temp.dailyTempMean)
      }
      month_forEachDoy <- as.POSIXlt(seq(from=as.POSIXlt(paste(years[y], "-01-01", sep="")), to=as.POSIXlt(paste(years[y], "-12-31", sep="")), by="1 day"))$mon + 1
      if (years[y] == 1942 ){
        month_forEachDoy<-c(month_forEachDoy,12)
      }
      tempMean <- tempMean + aggregate(temp.dailyTempMean, by=list(month_forEachDoy), FUN=mean)[, 2]
      tempMin <- tempMin + aggregate(temp.dailyTempMin, by=list(month_forEachDoy), FUN=mean)[, 2]
      tempMax <- tempMax + aggregate(temp.dailyTempMax, by=list(month_forEachDoy), FUN=mean)[, 2]
      ppt <- ppt + aggregate(get_swWeatherData(weatherList, years[y])@data[, 4], by=list(month_forEachDoy), FUN=sum)[, 2]
    }
  tempMean <- tempMean / no.yrs
  tempMin <- tempMin / no.yrs
  tempMax <- tempMax / no.yrs
  ppt <- ppt / no.yrs

  res <- list(meanMonthlyTempC=tempMean, minMonthlyTempC=tempMin, maxMonthlyTempC=tempMax,
        meanMonthlyPPTcm=ppt, MAP_cm=sum(ppt), MAT_C=mean(mat))

  if(do.C4vars){
    res$dailyTempMin <- dailyTempMin
    res$dailyTempMean <- dailyTempMean
    res$dailyC4vars <- sw_dailyC4_TempVar(dailyTempMin, dailyTempMean, simTime2)
  }
  res
})

cut0Inf <- compiler::cmpfun(function(x, val = NA) {
  x[x < 0] <- val
  x
})
NAto0 <- compiler::cmpfun(function(x) {
  x[is.na(x)] <- 0
  x
})
finite01 <- compiler::cmpfun(function(x) {
  x[x < 0 | is.na(x)] <- 0
  x[x > 1] <- 1
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


AdjMonthlyBioMass <- compiler::cmpfun(function(tr_VegetationComposition,AdjMonthlyBioMass_Temperature,AdjMonthlyBioMass_Precipitation,grasses.c3c4ann.fractions,growing.season.threshold.tempC,isNorth,MAP_mm,monthly.temp) {
  tr_VegComp_Adj <- tr_VegetationComposition	#Default shrub biomass input is at MAP = 450 mm/yr, and default grass biomass input is at MAP = 340 mm/yr
  #Describe conditions for which the default vegetation biomass values are valid
  std.winter <- c(11:12, 1:2) #Assumes that the "growing season" (valid for growing.season.threshold.tempC == 4) in 'tr_VegetationComposition' starts in March and ends after October, for all functional groups.
  std.growing <- st_mo[-std.winter] #Assumes that the "growing season" in 'tr_VegetationComposition' starts in March and ends after October, for all functional groups.
  #Default site for the grass description is SGS LTER
  StandardGrasses_MAP_mm <- 340
  StandardGrasses_VegComposition <- c(0.12, 0.22, 0.66) #Fraction of shrubs, C3, and C4
  #Default site for the shrub description is Reynolds Creek, ID
  StandardShrub_MAP_mm <- 250
  StandardShrub_VegComposition <- c(0.7, 0.3, 0) #Fraction of shrubs, C3, and C4

  #Calculate 'live biomass amount'
  tr_VegComp_Adj$Sh.Amount.Live <- tr_VegComp_Adj$Sh.Biomass * tr_VegComp_Adj$Sh.Perc.Live
  tr_VegComp_Adj$C3.Amount.Live <- tr_VegComp_Adj$C3.Biomass * tr_VegComp_Adj$C3.Perc.Live
  tr_VegComp_Adj$C4.Amount.Live <- tr_VegComp_Adj$C4.Biomass * tr_VegComp_Adj$C4.Perc.Live
  tr_VegComp_Adj$Annual.Amount.Live <- tr_VegComp_Adj$Annual.Biomass * tr_VegComp_Adj$Annual.Perc.Live

  #Scale monthly values of litter and live biomass amount by column-max; total biomass will be back calculated from 'live biomass amount' / 'percent live'
  colmax <- apply(tr_VegComp_Adj[, itemp <- grepl("Litter", names(tr_VegComp_Adj)) | grepl("Amount.Live", names(tr_VegComp_Adj))], MARGIN=2, FUN=max)
  colmin <- apply(tr_VegComp_Adj[, itemp], MARGIN=2, FUN=min)
  tr_VegComp_Adj[, itemp] <- sweep(tr_VegComp_Adj[, itemp], MARGIN=2, STATS=colmax, FUN="/")

  #Pull different composition types
  shrubs_Composition <- shrubs_Standard <- tr_VegComp_Adj[, grepl("Sh", names(tr_VegComp_Adj))]
  C3_Composition <- C3_Standard <- tr_VegComp_Adj[, grepl("C3", names(tr_VegComp_Adj))]
  C4_Composition <- C4_Standard <- tr_VegComp_Adj[, grepl("C4", names(tr_VegComp_Adj))]
  AnnGrass_Composition <- AnnGrass_Standard <- tr_VegComp_Adj[, grepl("Annual", names(tr_VegComp_Adj))]

  adjCompPPT <- function(shrubs_Composition, C3_Composition, C4_Composition, AnnGrass_Composition, ShrubsMAP_mm, GrassMAP_mm) {
    #Equations: Milchunas & Lauenroth 1993 (Fig. 2): Y [g/m2/yr] = c1 * MAP [mm/yr] + c2
    Shrub_ANPP <- function(MAP_mm) 0.393 * MAP_mm - 10.2
    Grass_ANPP <- function(MAP_mm) 0.646 * MAP_mm - 102.5

    #Intercepts to match outcomes of M & L 1993 equations under 'default' MAP with our previous default inputs for shrubs and sgs-grasslands
    #Whereas these intercepts were introduced artificially, they could also be interpreted as perennial storage, e.g., Lauenroth & Whitman (1977) found "Accumulation in the standing dead was 63% of inputs, in the litter 8%, and belowground 37%.". Lauenroth, W.K. & Whitman, W.C. (1977) Dynamics of dry matter production in a mixed-grass prairie in western North Dakota. Oecologia, 27, 339-351.
    Shrub_ANPPintercept <- (StandardShrub_VegComposition[1]*colmax["Sh.Amount.Live"] + StandardShrub_VegComposition[2]*colmax["C3.Amount.Live"] + StandardShrub_VegComposition[3]*colmax["C4.Amount.Live"]) - Shrub_ANPP(StandardShrub_MAP_mm)	#Default input for shrubs (IM_USC00107648_Reynolds; 70% shrubs, 30% C3): biomass was estimated at MAP = 450 mm/yr
    Grasses_ANPPintercept <- (StandardGrasses_VegComposition[1]*colmax["Sh.Amount.Live"] + StandardGrasses_VegComposition[2]*colmax["C3.Amount.Live"] + StandardGrasses_VegComposition[3]*colmax["C4.Amount.Live"]) - Grass_ANPP(StandardGrasses_MAP_mm)		#Default input for sgs-grassland (GP_SGSLTER; 12% shrubs, 22% C3, and 66% C4): biomass was estimated at MAP = 340 mm/yr

    #Get scaling values for scaled biomass; guarantee that > minimum.totalBiomass
    minimum.totalBiomass <- 0 #This is a SoilWat parameter
    Shrub_BiomassScaler <- max(minimum.totalBiomass, Shrub_ANPP(ShrubsMAP_mm) + Shrub_ANPPintercept)
    Grass_BiomassScaler <- max(minimum.totalBiomass, Grass_ANPP(GrassMAP_mm) + Grasses_ANPPintercept)

    #Scale live biomass amount by productivity; assumption: ANPP = peak standing live biomass
    shrubs_Composition$Sh.Amount.Live <- shrubs_Composition$Sh.Amount.Live * Shrub_BiomassScaler
    C3_Composition$C3.Amount.Live <- C3_Composition$C3.Amount.Live * Grass_BiomassScaler
    C4_Composition$C4.Amount.Live <- C4_Composition$C4.Amount.Live * Grass_BiomassScaler
    AnnGrass_Composition$Annual.Amount.Live <- AnnGrass_Composition$Annual.Amount.Live * Grass_BiomassScaler

    #Scale litter amount by productivity and adjust for ratio of litter/live
    shrubs_Composition$Sh.Litter <- shrubs_Composition$Sh.Litter * Shrub_BiomassScaler * colmax["Sh.Litter"] / colmax["Sh.Amount.Live"]
    C3_Composition$C3.Litter <- C3_Composition$C3.Litter * Grass_BiomassScaler * colmax["C3.Litter"] / colmax["C3.Amount.Live"]
    C4_Composition$C4.Litter <- C4_Composition$C4.Litter * Grass_BiomassScaler * colmax["C4.Litter"] / colmax["C4.Amount.Live"]
    AnnGrass_Composition$Annual.Litter <- AnnGrass_Composition$Annual.Litter * Grass_BiomassScaler * colmax["Annual.Litter"] / colmax["Annual.Amount.Live"]

    #Guarantee that live fraction = ]0, 1]
    shrubs_Composition$Sh.Perc.Live <- pmin(1, pmax(tol, shrubs_Composition$Sh.Perc.Live))
    C3_Composition$C3.Perc.Live <- pmin(1, pmax(tol, C3_Composition$C3.Perc.Live))
    C4_Composition$C4.Perc.Live <- pmin(1, pmax(tol, C4_Composition$C4.Perc.Live))
    AnnGrass_Composition$Annual.Perc.Live <- pmin(1, pmax(tol, AnnGrass_Composition$Annual.Perc.Live))

    #Calculate total biomass based on scaled live biomass amount
    shrubs_Composition$Sh.Biomass <- shrubs_Composition$Sh.Amount.Live / shrubs_Composition$Sh.Perc.Live
    C3_Composition$C3.Biomass <- C3_Composition$C3.Amount.Live / C3_Composition$C3.Perc.Live
    C4_Composition$C4.Biomass <- C4_Composition$C4.Amount.Live / C4_Composition$C4.Perc.Live
    AnnGrass_Composition$Annual.Biomass <- AnnGrass_Composition$Annual.Amount.Live / AnnGrass_Composition$Annual.Perc.Live

    list(shrubs_Composition = shrubs_Composition,
         C3_Composition = C3_Composition,
         C4_Composition = C4_Composition,
         AnnGrass_Composition = AnnGrass_Composition)
  }

  #adjust phenology for mean monthly temperatures
  if(AdjMonthlyBioMass_Temperature) {
    growing.season <- monthly.temp >= growing.season.threshold.tempC

    if(!isNorth) growing.season <- c(growing.season[7:12], growing.season[1:6]) #Standard growing season needs to be adjusted for southern Hemi

    predict.season <- function(biomass_Standard, std.season.padded, std.season.seq, site.season.seq){
      #length(std.season.seq) >= 3 because of padding and test that season duration > 0
      lcoef <- calc.loess_coeff(N=length(std.season.seq), span=0.4)

      op <- options(c("warn", "error"))
      options(warn=-1, error=traceback) #loess throws many warnings: 'pseudoinverse used', see calc.loess_coeff(), etc.
      res <- sapply(apply(biomass_Standard, MARGIN=2, function(x) {lf<-loess(x[std.season.padded] ~ std.season.seq, span=lcoef$span, degree=lcoef$degree); predict(lf, newdata=data.frame(std.season.seq=site.season.seq) ) }), FUN=function(x) max(0, x)) # guarantee that > 0
      options(op)
      return(res)
    }

    #Adjust for timing and duration of non-growing season
    if(sum(!growing.season) > 0) {
      if(sum(!growing.season) < 12) {
        std.winter.padded <- (c(std.winter[1] - 1, std.winter, std.winter[length(std.winter)] + 1) - 1) %% 12 + 1
        std.winter.seq <- 0:(length(std.winter.padded) - 1)
        site.winter.seq <- seq(from=1, to=length(std.winter), length=sum(!growing.season))
        site.winter.start <- (temp3 <- (temp2 <- cumsum(c(0, (rtemp <- rle(!growing.season))$lengths))+1)[-length(temp2)][rtemp$values])[length(temp3)] #Calculate first month of winter
        site.winter.months <- (site.winter.start + 1:sum(!growing.season) - 2) %% 12 + 1

        shrubs_Composition[site.winter.months,] <- predict.season(shrubs_Standard, std.winter.padded, std.winter.seq, site.winter.seq)
        C3_Composition[site.winter.months,] <- predict.season(C3_Standard, std.winter.padded, std.winter.seq, site.winter.seq)
        C4_Composition[site.winter.months,] <- predict.season(C4_Standard, std.winter.padded, std.winter.seq, site.winter.seq)
        AnnGrass_Composition[site.winter.months,] <- predict.season(AnnGrass_Standard, std.winter.padded, std.winter.seq, site.winter.seq)

      } else { #if winter lasts 12 months
        #Take the mean of the winter months
        shrubs_Composition[] <- matrix(apply(shrubs_Standard[std.winter,], 2, mean), nrow=12, ncol=ncol(shrubs_Composition), byrow=TRUE)
        C3_Composition[] <- matrix(apply(C3_Standard[std.winter,], 2, mean), nrow=12, ncol=ncol(C3_Composition), byrow=TRUE)
        C4_Composition[] <- matrix(apply(C4_Standard[std.winter,], 2, mean), nrow=12, ncol=ncol(C4_Composition), byrow=TRUE)
        AnnGrass_Composition[] <- matrix(apply(AnnGrass_Standard[std.winter,], 2, mean), nrow=12, ncol=ncol(AnnGrass_Composition), byrow=TRUE)
      }
    }
    #Adjust for timing and duration of growing season
    if(sum(growing.season)>0) {
      if(sum(growing.season) < 12) {
        std.growing.padded <- (c(std.growing[1] - 1, std.growing, std.growing[length(std.growing)] + 1) - 1) %% 12 + 1
        std.growing.seq <- 0:(length(std.growing.padded) - 1)
        site.growing.seq <- seq(from=1, to=length(std.growing), length=sum(growing.season))
        site.growing.start <- (temp3 <- (temp2 <- cumsum(c(0, (rtemp <- rle(growing.season))$lengths))+1)[-length(temp2)][rtemp$values])[1] #Calculate first month of growing season
        site.growing.months <- (site.growing.start + 1:sum(growing.season) - 2) %% 12 + 1

        shrubs_Composition[site.growing.months,] <- predict.season(shrubs_Standard, std.growing.padded, std.growing.seq, site.growing.seq)
        C3_Composition[site.growing.months,] <- predict.season(C3_Standard, std.growing.padded, std.growing.seq, site.growing.seq)
        C4_Composition[site.growing.months,] <- predict.season(C4_Standard, std.growing.padded, std.growing.seq, site.growing.seq)
        AnnGrass_Composition[site.growing.months,] <- predict.season(AnnGrass_Standard, std.growing.padded, std.growing.seq, site.growing.seq)

      } else { #if growing season lasts 12 months
        shrubs_Composition[] <- matrix(apply(shrubs_Standard[std.growing,], MARGIN=2, FUN=max), nrow=12, ncol=ncol(shrubs_Composition), byrow=TRUE)
        C3_Composition[] <- matrix(apply(C3_Standard[std.growing,], MARGIN=2, FUN=max), nrow=12, ncol=ncol(C3_Composition), byrow=TRUE)
        C4_Composition[] <- matrix(apply(C4_Standard[std.growing,], MARGIN=2, FUN=max), nrow=12, ncol=ncol(C4_Composition), byrow=TRUE)
        AnnGrass_Composition[] <- matrix(apply(AnnGrass_Standard[std.growing,], MARGIN=2, FUN=max), nrow=12, ncol=ncol(AnnGrass_Composition), byrow=TRUE)
      }
    }
    if(!isNorth) { #Adjustements were done as if on nothern hemisphere
      shrubs_Composition <- rbind(shrubs_Composition[7:12,], shrubs_Composition[1:6,])
      C3_Composition <- rbind(C3_Composition[7:12,], C3_Composition[1:6,])
      C4_Composition <- rbind(C4_Composition[7:12,], C4_Composition[1:6,])
      AnnGrass_Composition <- rbind(AnnGrass_Composition[7:12,], AnnGrass_Composition[1:6,])
    }
    if(!AdjMonthlyBioMass_Precipitation){
      temp<-adjCompPPT(shrubs_Composition,C3_Composition,C4_Composition,AnnGrass_Composition,ShrubsMAP_mm=StandardShrub_MAP_mm, GrassMAP_mm=StandardGrasses_MAP_mm)
      shrubs_Composition <- temp$shrubs_Composition
      C3_Composition <- temp$C3_Composition
      C4_Composition <- temp$C4_Composition
      AnnGrass_Composition <- temp$AnnGrass_Composition
    }
  }
  #Adjust biomass amounts by productivity relationship with MAP
  if(AdjMonthlyBioMass_Precipitation) {
    temp<-adjCompPPT(shrubs_Composition,C3_Composition,C4_Composition,AnnGrass_Composition,ShrubsMAP_mm=MAP_mm, GrassMAP_mm=MAP_mm)
    shrubs_Composition <- temp$shrubs_Composition
    C3_Composition <- temp$C3_Composition
    C4_Composition <- temp$C4_Composition
    AnnGrass_Composition <- temp$AnnGrass_Composition
  }

  Grass_Composition <- C3_Composition*grasses.c3c4ann.fractions[1] + C4_Composition*grasses.c3c4ann.fractions[2] + AnnGrass_Composition*grasses.c3c4ann.fractions[3]
  
  list(grass = as.matrix(Grass_Composition),
       shrub = as.matrix(shrubs_Composition))
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

#convert SWP(matric) to VWC(matric), e.g., to calculate field capacity and wilting point
SWPtoVWC <- compiler::cmpfun(function(swp, sand, clay) {
#Cosby, B. J., G. M. Hornberger, R. B. Clapp, and T. R. Ginn. 1984. A statistical exploration of the relationships of soil moisture characteristics to the physical properties of soils. Water Resources Research 20:682-690.

  #1. SWP in MPa [single value] + sand and clay in fraction [single values] --> VWC in fraction [single value]
  #2. SWP in MPa [single value] + sand and clay in fraction [vectors of length d] --> VWC in fraction [vector of length d]
  #3. SWP in MPa [vector of length l] + sand and clay in fraction [single values] --> VWC in fraction [vector of length l]
  #4. SWP in MPa [vector of length l] + sand and clay in fraction [vectors of length d] --> VWC in fraction [matrix with nrow=l and ncol=d, SWP vector repeated for each column]: probably not used
  #5. SWP in MPa [matrix with nrow=l and ncol=d] + sand and clay in fraction [single values] --> VWC in fraction [matrix with nrow=l and ncol=d]
  #6. SWP in MPa [matrix with nrow=l and ncol=d] + sand and clay in fraction [vectors of length d] --> VWC in fraction [matrix with nrow=l and ncol=d, sand/clay vector repeated for each row]

#input: sand and clay as fraction of matric volume, i.e, they don't need to be scaled with gravel

  stopifnot(length(sand) && length(sand) == length(clay))
  na.act <- na.action(na.exclude(apply(data.frame(sand, clay), MARGIN=1, FUN=sum)))

  if(length(sand) > length(na.act)){
    na.index <- as.vector(na.act)

    if(length(na.index) > 0){
      sand <- sand[-na.index]
      clay <- clay[-na.index]
    }

    thetas <- -14.2 * sand - 3.7 * clay + 50.5
    psis <- 10 ^ (-1.58 * sand - 0.63 * clay + 2.17)
    b <- -0.3 * sand + 15.7 * clay + 3.10
    if(any(b <= 0)) stop("b <= 0")

    bar_conversion <- 1024
    MPa_toBar <- -10

    get_vector <- function(swp, sand, clay, thetas=thetas, psis=psis, b=b, do.na=TRUE){#either swp or sand/clay needs be a single value
      vwc <- ifelse(!is.na(swp) & swp <= 0 & sand <= 1 & sand >= 0 & clay <= 1 & clay >= 0, thetas * (psis / (swp * MPa_toBar * bar_conversion))^(1/b) / 100, NA)
      if(do.na & length(na.index) > 0){
        vwc <- napredict(na.act, vwc)
      }
      return(vwc)
    }

    if(is.null(dim(swp))){
      if(length(swp) == 1 & length(sand) >= 1 | length(swp) >= 1 & length(sand) == 1){ #cases 1-3
        vwc <- get_vector(swp, sand, clay, thetas=thetas, psis=psis, b=b)
      } else if(length(swp) > 1 & length(sand) > 1){ #case 4
        vwc <- t(sapply(1:length(swp), FUN=function(d) get_vector(swp[d], sand, clay, thetas=thetas, psis=psis, b=b)))
      }
    } else {
      if(length(sand) == 1){ #case 5
        vwc <- sapply(1:ncol(swp), FUN=function(d) get_vector(swp[, d], sand, clay, thetas=thetas, psis=psis, b=b))
      } else { #case 6
        sand <- napredict(na.act, sand)
        clay <- napredict(na.act, clay)
        stopifnot(ncol(swp) == length(sand))
        psis <- napredict(na.act, psis)
        thetas <- napredict(na.act, thetas)
        b <- napredict(na.act, b)
        vwc <- sapply(1:ncol(swp), FUN=function(d) get_vector(swp[, d], sand[d], clay[d], thetas=thetas[d], psis=psis[d], b=b[d], do.na=FALSE))
      }
    }
  } else {
    vwc <- swp
    vwc[!is.na(vwc)] <- NA
  }
  return(vwc) #fraction m3/m3 [0, 1]
})

#convert VWC(matric) to SWP(matric)
VWCtoSWP <- compiler::cmpfun(function(vwc, sand, clay) {
#Cosby, B. J., G. M. Hornberger, R. B. Clapp, and T. R. Ginn. 1984. A statistical exploration of the relationships of soil moisture characteristics to the physical properties of soils. Water Resources Research 20:682-690.

  #1. VWC in fraction [single value] + sand and clay in fraction [single values] --> SWP in MPa [single value]
  #2. VWC in fraction [single value] + sand and clay in fraction [vectors of length d] --> SWP in MPa [vector of length d]
  #3. VWC in fraction [vector of length l] + sand and clay in fraction [single values] --> SWP in MPa [vector of length l]
  #4. VWC in fraction [vector of length l] + sand and clay in fraction [vectors of length d] --> SWP in MPa [matrix with nrow=l and ncol=d, VWC vector repeated for each column]: probably not used
  #5. VWC in fraction [matrix with nrow=l and ncol=d] + sand and clay in fraction [single values] --> SWP in MPa [matrix with nrow=l and ncol=d]
  #6. VWC in fraction [matrix with nrow=l and ncol=d] + sand and clay in fraction [vectors of length d] --> SWP in MPa [matrix with nrow=l and ncol=d, sand/clay vector repeated for each row]

#input: sand and clay as fraction of matric volume, i.e, they don't need to be scaled with gravel

  stopifnot(length(sand) && length(sand) == length(clay))
  na.act <- na.action(na.exclude(apply(data.frame(sand, clay), MARGIN=1, FUN=sum)))

  if(length(sand) > length(na.act)){
    na.index <- as.vector(na.act)

    if(length(na.index) > 0){
      sand <- sand[-na.index]
      clay <- clay[-na.index]
    }

    thetas <- -14.2 * sand - 3.7 * clay + 50.5
    psis <- 10 ^ (-1.58 * sand - 0.63 * clay + 2.17)
    b <- -0.3 * sand + 15.7 * clay + 3.10
    if(any(b <= 0)) stop("b <= 0")

    bar_conversion <- 1024
    bar_toMPa <- -1/10

    get_vector <- function(vwc, sand, clay, thetas=thetas, psis=psis, b=b, do.na=TRUE){#either vwc or sand/clay needs be a single value
      swp <- ifelse(!is.na(vwc) & vwc <= 1 & vwc >= 0 & sand <= 1 & sand >= 0 & clay <= 1 & clay >= 0, psis / ((vwc*100/thetas) ^ b * bar_conversion) * bar_toMPa, NA)
      if(do.na & length(na.index) > 0){
        swp <- napredict(na.act, swp)
      }
      return(swp)
    }

    if(is.null(dim(vwc))){
      if(length(vwc) == 1 & length(sand) >= 1 | length(vwc) >= 1 & length(sand) == 1){ #cases 1-3
        swp <- get_vector(vwc, sand, clay, thetas=thetas, psis=psis, b=b)
      } else if(length(vwc) > 1 & length(sand) > 1){ #case 4
        swp <- t(sapply(1:length(vwc), FUN=function(d) get_vector(vwc[d], sand, clay, thetas=thetas, psis=psis, b=b)))
      }
    } else {
      if(length(sand) == 1){ #case 5
        swp <- sapply(1:ncol(vwc), FUN=function(d) get_vector(vwc[, d], sand, clay, thetas=thetas, psis=psis, b=b))
      } else { #case 6
        sand <- napredict(na.act, sand)
        clay <- napredict(na.act, clay)
        stopifnot(ncol(vwc) == length(sand))
        psis <- napredict(na.act, psis)
        thetas <- napredict(na.act, thetas)
        b <- napredict(na.act, b)
        swp <- sapply(1:ncol(vwc), FUN=function(d) get_vector(vwc[, d], sand[d], clay[d], thetas=thetas[d], psis=psis[d], b=b[d], do.na=FALSE))
      }
    }
  } else {
    swp <- vwc
    swp[!is.na(swp)] <- NA
  }
  
  swp #MPa [-Inf, 0]
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
  sw_input_use[seq1_icols_in] <- 1L

  if (length(seq2_icols_in) > 0) {
    sw_input[, seq2_icols_in] <- NA
    sw_input_use[seq2_icols_in] <- 0L
  }

  list(sw_input_use = sw_input_use,
       sw_input = sw_input)
})

fill_empty <- compiler::cmpfun(function(data, pattern, fill, tol = tol) {
  stopifnot(names(data) %in% c("sw_input", "sw_input_use"))

  icols <- sapply(data, function(x) grep(pattern, colnames(x)))
  stopifnot(dim(icols)[2L] == 2L)

  for (k in seq_len(dim(icols)[1L])) {
    ic <- icols[k, "sw_input"]
    iempty <- is.na(data$sw_input[, ic]) | abs(data$sw_input[, ic]) < tol
    if (any(iempty)) {
      data$sw_input[iempty, ic] <- fill
      data$sw_input_use[icols[k, "sw_input_use"]] <- 1L
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
#' @param method A character string. \code{See details.}
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

EstimateInitialSoilTemperatureForEachSoilLayer <- compiler::cmpfun(function(layers_depth, lower.Tdepth, soilTupper, soilTlower){
  sl <- c(0, lower.Tdepth)
  st <- c(soilTupper, soilTlower)
  return( predict(lm(st ~ sl), data.frame(sl=layers_depth)) )
})

#--put information from experimental design into appropriate input variables; create_treatments and the _use files were already adjusted for the experimental design when files were read in/created
transferExpDesignToInput <- compiler::cmpfun(function(x, i_exp, df_exp, df_exp_use) {
  temp <- match(names(df_exp)[df_exp_use == 1], names(x), nomatch = 0)
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

.local_weatherDirName <- compiler::cmpfun(function(i_sim, name.OutputDB) {	# Get name of weather file from output database
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = name.OutputDB)
  temp <- DBI::dbGetQuery(con, paste("SELECT WeatherFolder FROM header WHERE P_id=", it_Pid(i_sim, 1)))[1,1]
  DBI::dbDisconnect(con)
  temp
})

tempError <- compiler::cmpfun(function() .Call("tempError"))

#data access functions
get_Response_aggL <- compiler::cmpfun(function(sc_i, response,
                              tscale = c("dy", "dyAll", "mo", "moAll", "yr", "yrAll"),
                              scaler = 10, FUN, weights = NULL,
                              x = runData, st = simTime, st2 = simTime2,
                              topL. = topL, bottomL. = bottomL) {
  FUN <- match.fun(FUN)
  tscale <- match.arg(tscale)
  responseRepeats <- if (response %in% c("TRANSP", "HYDRED")) {
      # divide by 5, because: each soil layer (cm): total, trees, shrubs, forbs, grasses
      5L
    } else {
      # c(sw_vwc, sw_evsoil, sw_soiltemp, sw_swc, sw_swa)
      1L
    }

  temp1 <- scaler * slot(slot(x[[sc_i]], response), 
    switch(tscale,
      dy = "Day", dyAll = "Day",
      mo = "Month", moAll = "Month",
      yr = "Year", yrAll = "Year"))

  if (inherits(temp1, "try-error"))
    stop("Necessary SoilWat output files are not present for aggregation of results")

  if (tscale == "dy") {
    index.col <- 2
    index.usetimestep <- st$index.usedy
    timestep_ForEachEntry <- st2$doy_ForEachUsedDay
  } else if (tscale == "dyAll") {
    index.col <- 2
    index.usetimestep <- seq_len(nrow(temp1))
    timestep_ForEachEntry <- NULL
  } else if (tscale == "mo") {
    index.col <- 2
    index.usetimestep <- st$index.usemo
    timestep_ForEachEntry <- st2$month_ForEachUsedMonth
  } else if (tscale == "moAll") {
    index.col <- 2
    index.usetimestep <- seq_len(nrow(temp1))
    timestep_ForEachEntry <- NULL
  } else if (tscale == "yr") {
    index.col <- 1
    index.usetimestep <- st$index.useyr
    timestep_ForEachEntry <- NULL
  } else if (tscale == "yrAll") {
    index.col <- 1
    index.usetimestep <- seq_len(nrow(temp1))
    timestep_ForEachEntry <- NULL
  }
  
  layers <- seq_len((ncol(temp1) - index.col) / responseRepeats)
  
  #adjust topL. and bottomL. locally in case temp1 doesn't contain information for every layer, e.g., soil evaporation
  if (max(layers) <= max(topL.)) {
    topL. <- layers
    bottomL. <- 0
  } else if (max(layers) < max(bottomL.)) {
    bottomL. <- min(bottomL.):max(layers)
  }
  
  res <- list()
  res[["top"]] <- if (length(topL.) > 1) {
      if (is.null(weights)) {
        apply(temp1[index.usetimestep, index.col + topL., drop = FALSE], 1, FUN)
      } else {
        apply(temp1[index.usetimestep, index.col + topL., drop = FALSE], 1, FUN, weights[topL.])
      }
    } else {
      temp1[index.usetimestep, index.col + topL.]
    }
  res[["bottom"]] <- if (length(bottomL.) > 1) {
      if(is.null(weights)){
        apply(temp1[index.usetimestep, index.col + bottomL., drop = FALSE], 1, FUN)
      } else {
        apply(temp1[index.usetimestep, index.col + bottomL., drop = FALSE], 1, FUN, weights[bottomL.])
      }
    } else if (is.null(bottomL.) || identical(bottomL., 0)) {
      matrix(data = 0, nrow = length(index.usetimestep), ncol = 1)
    } else {
      temp1[index.usetimestep, index.col + bottomL.]
    }

  if (!is.null(timestep_ForEachEntry)) {
    res[["aggMean.top"]] <- aggregate(res[["top"]], by = list(timestep_ForEachEntry), mean)[,2]
    res[["aggMean.bottom"]] <- aggregate(res[["bottom"]], by = list(timestep_ForEachEntry), mean)[,2]
  }
  
  if (tscale == "dyAll" || tscale == "moAll" || tscale == "yrAll") {
     res[["val"]] <- temp1
  }
  
  res
})

get_SWPmatric_aggL <- compiler::cmpfun(function(vwcmatric, texture. = texture, sand. = sand, clay. = clay) {
  res <- list()
  
  res[["top"]] <- VWCtoSWP(vwcmatric$top, texture.$sand.top, texture.$clay.top)
  res[["bottom"]] <- VWCtoSWP(vwcmatric$bottom, texture.$sand.bottom, texture.$clay.bottom)
  
  if (!is.null(vwcmatric$aggMean.top)) {
    res[["aggMean.top"]] <- VWCtoSWP(vwcmatric$aggMean.top, texture.$sand.top, texture.$clay.top)
    res[["aggMean.bottom"]] <- VWCtoSWP(vwcmatric$aggMean.bottom, texture.$sand.bottom, texture.$clay.bottom)
  }
  
  if (!is.null(vwcmatric$val)) {
    if (all(as.integer(vwcmatric$val[, 2]) == vwcmatric$val[, 2])) {
      index.header <- 1:2
    } else {
      index.header <- 1
    }
    res[["val"]] <- cbind(vwcmatric$val[, index.header], VWCtoSWP(vwcmatric$val[, -index.header], sand., clay.))
  }
  
  res
})

get_Temp_yr <- compiler::cmpfun(function(sc, x = runData, st = simTime) {
  list(mean = slot(slot(x[[sc]], "TEMP"), "Year")[st$index.useyr, 4])
})

get_Temp_mo <- compiler::cmpfun(function(sc, x = runData, st = simTime) {
  x <- slot(slot(x[[sc]], "TEMP"), "Month")[st$index.usemo, ]
  list(min =  x[, 4],
       mean = x[, 5],
       max =  x[, 3])
})

get_Temp_dy <- compiler::cmpfun(function(sc, x = runData, st = simTime) {
  x <- slot(slot(x[[sc]], "TEMP"), "Day")[st$index.usedy, ]
  list(min =  x[, 4],
       mean = x[, 5],
       max =  x[, 3])
})

get_VPD_mo <- compiler::cmpfun(function(sc, temp.mo, xin = swRunScenariosData, st2 = simTime2) {
  rH <- Rsoilwat31::swCloud_SkyCover(xin[[sc]])
  rH <- as.vector(rH[st2$month_ForEachUsedMonth])

  list(mean = vpd(temp.mo$min, temp.mo$max, rH))
})

get_VPD_dy <- compiler::cmpfun(function(sc, temp.dy, xin = swRunScenariosData, st2 = simTime2) {
  rH <- Rsoilwat31::swCloud_SkyCover(xin[[sc]])
  rH <- as.vector(rH[st2$month_ForEachUsedDay])
  
  list(mean = vpd(temp.dy$min, temp.dy$max, rH))
})

get_PPT_yr <- compiler::cmpfun(function(sc, x = runData, st = simTime) {
  x <- 10 * slot(slot(x[[sc]], "PRECIP"), "Year")[st$index.useyr, ]
  list(ppt = x[, 2], rain = x[, 3],
       snowfall = x[, 4], snowmelt = x[, 5], snowloss = x[, 6])
})

get_PPT_mo <- compiler::cmpfun(function(sc, x = runData, st = simTime) {
  x <- 10 * slot(slot(x[[sc]], "PRECIP"), "Month")[st$index.usemo, ]
  list(ppt = x[, 3], rain = x[, 4],
       snowmelt = x[, 6])
})

get_PPT_dy <- compiler::cmpfun(function(sc, x = runData, st = simTime) {
  x <- 10 * slot(slot(x[[sc]], "PRECIP"), "Day")[st$index.usedy, ]
  list(ppt = x[, 3], rain = x[, 4],
       snowfall = x[, 5], snowmelt = x[, 6], snowloss = x[, 7])
})

get_PET_yr <- compiler::cmpfun(function(sc, x = runData, st = simTime) {
  list(val = 10 * slot(slot(x[[sc]], "PET"), "Year")[st$index.useyr, 2])
})

get_PET_mo <- compiler::cmpfun(function(sc, x = runData, st = simTime) {
  list(val = 10 * slot(slot(x[[sc]], "PET"), "Month")[st$index.usemo, 3])
})

get_AET_yr <- compiler::cmpfun(function(sc, x = runData, st = simTime) {
  list(val = 10 * slot(slot(x[[sc]], "AET"), "Year")[st$index.useyr, 2])
})

get_AET_mo <- compiler::cmpfun(function(sc, x = runData, st = simTime) {
  list(val = 10 * slot(slot(x[[sc]], "AET"), "Month")[st$index.usemo, 3])
})

get_AET_dy <- compiler::cmpfun(function(sc, x = runData, st = simTime) {
  list(val = 10 * slot(slot(x[[sc]], "AET"), "Day")[st$index.usedy, 3])
})

get_SWE_mo <- compiler::cmpfun(function(sc, x = runData, st = simTime) {
  list(val = 10 * slot(slot(x[[sc]], "SNOWPACK"), "Month")[st$index.usemo, 3])
})

get_SWE_dy <- compiler::cmpfun(function(sc, x = runData, st = simTime) {
  list(val = 10 * slot(slot(x[[sc]], "SNOWPACK"), "Day")[st$index.usedy, 3])
})

get_Inf_yr <- compiler::cmpfun(function(sc, x = runData, st = simTime) {
  list(inf = 10 * slot(slot(x[[sc]], "SOILINFILT"), "Year")[st$index.useyr, 2])
})

get_Inf_mo <- compiler::cmpfun(function(sc, x = runData, st = simTime) {
  list(inf = 10 * slot(slot(x[[sc]], "SOILINFILT"), "Month")[st$index.usemo, 3])
})

get_Inf_dy <- compiler::cmpfun(function(sc, x = runData, st = simTime) {
  list(inf = 10 * slot(slot(x[[sc]], "SOILINFILT"), "Day")[st$index.usedy, 3])
})

get_Esurface_yr <- compiler::cmpfun(function(sc, x = runData, st = simTime) {
  x <- 10 * slot(slot(x[[sc]], "EVAPSURFACE"), "Year")[st$index.useyr, ]
  list(sum = x[, 2],
       veg = rowSums(x[, 3:6]),
       litter = x[, 7],
       surfacewater = x[, 8])
})

get_Esurface_dy <- compiler::cmpfun(function(sc, x = runData, st = simTime) {
  x <- 10 * slot(slot(x[[sc]], "EVAPSURFACE"), "Day")[st$index.usedy, ]
  list(sum = x[, 3],
       veg = rowSums(x[, 4:7]),
       litter = x[, 8],
       surfacewater = x[, 9])
})

get_Interception_yr <- compiler::cmpfun(function(sc, x = runData, st = simTime) {
  x <- 10 * slot(slot(x[[sc]], "INTERCEPTION"), "Year")[st$index.useyr, ]
  list(sum = x[, 2],
       veg = rowSums(x[, 3:6]),
       litter = x[, 7])
})

get_DeepDrain_yr <- compiler::cmpfun(function(sc, x = runData, st = simTime) {
  list(val = 10 * slot(slot(x[[sc]], "DEEPSWC"), "Year")[st$index.useyr, 2])
})

get_DeepDrain_mo <- compiler::cmpfun(function(sc, x = runData, st = simTime) {
  list(val = 10 * slot(slot(x[[sc]], "DEEPSWC"), "Month")[st$index.usemo, 3])
})

get_DeepDrain_dy <- compiler::cmpfun(function(sc, x = runData, st = simTime) {
  list(val = 10 * slot(slot(x[[sc]], "DEEPSWC"), "Day")[st$index.usedy, 3])
})

get_Runoff_mo <- compiler::cmpfun(function(sc, x = runData, st = simTime) {
  x <- 10 * slot(slot(x[[sc]], "RUNOFF"), "Month")[st$index.usemo, ]
  list(val = x[, 3],
       ponded = x[, 4],
       snowmelt = x[, 5])
})

get_Runoff_yr <- compiler::cmpfun(function(sc, x = runData, st = simTime) {
  x <- 10 * slot(slot(x[[sc]], "RUNOFF"), "Year")[st$index.useyr, ]
  list(val = x[, 2],
       ponded = x[, 3],
       snowmelt = x[, 4])
})

cor2  <- compiler::cmpfun(function(y) cor(y[,1], y[,2]))

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

