#------------------------DAILY WEATHER

make_dbW <- function(dbWeatherDataFile, runIDs_sites, SWRunInformation, simstartyr, endyr,
    climate.conditions, sites_dailyweather_source, dir.sw.in.tr, dir.out.temp,
    chunk_size.options, continueAfterAbort, deleteTmpSQLFiles, dbW_compression_type,
    parallel_init, parallel_runs, parallel_backend, num_cores, cl = NULL,
    dir.ex.maurer2002 = NULL, dir.ex.daymet = NULL, dir.ex.NRCan = NULL, prepd_CFSR = NULL,
    verbose = FALSE) {

  stopifnot(requireNamespace("Rsoilwat31"))

  # weather database contains rows for 1:max(SWRunInformation$site_id) (whether included or not)
  Rsoilwat31::dbW_createDatabase(dbFilePath = dbWeatherDataFile,
    site_data = data.frame(Site_id = SWRunInformation$site_id,
            Latitude = SWRunInformation$Y_WGS84,
            Longitude = SWRunInformation$X_WGS84,
            Label = SWRunInformation$WeatherFolder,
            stringsAsFactors = FALSE),
    site_subset = runIDs_sites,
    scenarios = data.frame(Scenario = climate.conditions),
    compression_type = dbW_compression_type)

  Time <- Sys.time()

  # Extract weather data and move to weather database based on inclusion-invariant 'site_id'
  # Extract weather data per site
  if (verbose)
    print(paste(Sys.time(), "started with moving single site weather data to database"))

  temp <- sites_dailyweather_source %in%
    c("LookupWeatherFolder", "Maurer2002_NorthAmerica")
  ids_single <- which(temp) ## position in 'runIDs_sites'

  if (length(ids_single) > 0) {
    if (any(sites_dailyweather_source == "Maurer2002_NorthAmerica"))
      Maurer <- with(SWRunInformation[runIDs_sites[ids_single], ],
        create_filename_for_Maurer2002_NorthAmerica(X_WGS84, Y_WGS84))

    for (i in seq_along(ids_single)) {
      i_idss <- ids_single[i]
      i_site <- runIDs_sites[i_idss]

      if (verbose && i %% 100 == 1)
        print(paste(Sys.time(), "storing weather data of site",
          SWRunInformation$Label[i_site], ":", i, "of", length(ids_single),
          "sites in database"))

      if (sites_dailyweather_source[i_idss] == "LookupWeatherFolder") {
        weatherData <- ExtractLookupWeatherFolder(dir.weather =
          file.path(dir.sw.in.tr, "LookupWeatherFolder"),
          weatherfoldername = SWRunInformation$WeatherFolder[i_site])

      } else if (sites_dailyweather_source[i_idss] == "Maurer2002_NorthAmerica") {
        weatherData <- ExtractGriddedDailyWeatherFromMaurer2002_NorthAmerica(
          dir_data = dir.ex.maurer2002, cellname = Maurer[i], startYear = simstartyr,
          endYear = endyr)

      } else {
        stop(paste(sites_dailyweather_source[i_idss], "not implemented"))
      }

      if (!is.null(weatherData)) {
        years <- as.integer(names(weatherData))
        data_blob <- Rsoilwat31::dbW_weatherData_to_blob(weatherData,
          type = dbW_compression_type)
        Rsoilwat31:::dbW_addWeatherDataNoCheck(Site_id = SWRunInformation$site_id[i_site],
          Scenario_id = 1, StartYear = years[1], EndYear = years[length(years)],
          weather_blob = data_blob)

      } else {
        print(paste("Moving daily weather data to database unsuccessful",
          SWRunInformation$Label[i_site]))
      }
    }
  }

  # Extract weather data for all sites based on inclusion-invariant 'site_id'
  temp <- sites_dailyweather_source == "DayMet_NorthAmerica"
  ids_DayMet_extraction <- runIDs_sites[which(temp)] ## position in 'runIDs_sites'

  if (length(ids_DayMet_extraction) > 0) {
    ExtractGriddedDailyWeatherFromDayMet_NorthAmerica_dbW(
      dir_data = dir.ex.daymet,
      site_ids = SWRunInformation$site_id[ids_DayMet_extraction],
      coords_WGS84 = SWRunInformation[ids_DayMet_extraction,
        c("X_WGS84", "Y_WGS84"), drop = FALSE],
      start_year = simstartyr,
      end_year = endyr,
      dir_temp = dir.out.temp,
      dbW_compression_type = dbW_compression_type)
  }

  temp <- sites_dailyweather_source == "NRCan_10km_Canada"
  ids_NRCan_extraction <- runIDs_sites[which(temp)]

  if (length(ids_NRCan_extraction) > 0) {
    ExtractGriddedDailyWeatherFromNRCan_10km_Canada(
      dir_data = dir.ex.NRCan,
      site_ids = SWRunInformation$site_id[ids_NRCan_extraction],
      coords_WGS84 = SWRunInformation[ids_NRCan_extraction,
        c("X_WGS84", "Y_WGS84"), drop = FALSE],
      start_year = simstartyr,
      end_year = endyr,
      dir_temp = dir.out.temp,
      dbW_compression_type = dbW_compression_type,
      do_parallel = parallel_init && parallel_runs && identical(parallel_backend, "cluster"),
      ncores = num_cores)
  }

  temp <- sites_dailyweather_source == "NCEPCFSR_Global"
  ids_NCEPCFSR_extraction <- runIDs_sites[which(temp)]
  if (length(ids_NCEPCFSR_extraction) > 0) {
    GriddedDailyWeatherFromNCEPCFSR_Global(
      site_ids = SWRunInformation$site_id[ids_NCEPCFSR_extraction],
      dat_sites = SWRunInformation[ids_NCEPCFSR_extraction,
        c("WeatherFolder", "X_WGS84", "Y_WGS84"), drop = FALSE],
      start_year = simstartyr,
      end_year = endyr,
      meta_cfsr = prepd_CFSR,
      n_site_per_core = chunk_size.options[["DailyWeatherFromNCEPCFSR_Global"]],
      do_parallel = parallel_runs && parallel_init,
      parallel_backend = parallel_backend,
      cl = cl,
      rm_temp = deleteTmpSQLFiles,
      continueAfterAbort = continueAfterAbort,
      dir_temp = dir.out.temp,
      dbW_compression_type = dbW_compression_type)
  }

  Rsoilwat31::dbW_disconnectConnection()
}



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

create_filename_for_Maurer2002_NorthAmerica <- compiler::cmpfun(function(X_WGS84, Y_WGS84){
  gsub("[[:space:]]", "", paste("data", formatC(28.8125+round((Y_WGS84-28.8125)/0.125,0)*0.125, digits=4, format="f"), formatC(28.8125+round((X_WGS84-28.8125)/0.125,0)*0.125, digits=4, format="f"), sep="_"))
})


#TODO replace with Rsoilwat31::getWeatherData_folders
ExtractLookupWeatherFolder <- compiler::cmpfun(function(dir.weather, weatherfoldername) {
  WeatherFolder <- file.path(dir.weather, weatherfoldername)
  weath <- list.files(WeatherFolder, pattern = "weath.")
  years <- as.numeric(sub(pattern = "weath.", replacement = "", weath))
  stopifnot(!anyNA(years))

  weatherData <- list()
  for (j in seq_along(weath)) {
    temp <- read.table(file.path(WeatherFolder, weath[j]), header = FALSE,
      comment.char = "#", blank.lines.skip = TRUE, sep = "\t")
    data_sw <- as.matrix(temp)
    data_sw[, -1] <- round(data_sw[, -1], 2) #weather.digits
    colnames(data_sw) <- c("DOY", "Tmax_C", "Tmin_C", "PPT_cm")
    weatherData[[j]] <- methods::new("swWeatherData",
                            year = years[j],
                            data = data.matrix(data_sw, rownames.force = FALSE))
  }

  names(weatherData) <- years
  weatherData
})

#' @return A list of which each element represents one year of daily weather data of class \linkS4class{swWeatherData}.
#' Units are [degree Celsius] for temperature and [cm / day] and for precipitation.
ExtractGriddedDailyWeatherFromMaurer2002_NorthAmerica <- compiler::cmpfun(function(dir_data, cellname, startYear = simstartyr, endYear = endyr) {
  #read data from Maurer et al. 2002
  weath.data <- try(read.table(file=file.path(dir_data, cellname), comment.char=""), silent=TRUE)
  weathDataList <- list()

  if(!inherits(weath.data, "try-error")){
    colnames(weath.data) <- c("year", "month", "day", "prcp_mm", "Tmax_C", "Tmin_C", "Wind_mPERs")

    #times
    doy <- 1 + as.POSIXlt(seq(from = with(weath.data[1, ], ISOdate(year, month, day, tz = "UTC")),
        to = with(weath.data[nrow(weath.data), ], ISOdate(year, month, day, tz = "UTC")),
        by = "1 day"))$yday

    # conversion precipitation: mm/day -> cm/day
    data_all <- with(weath.data, data.frame(
      DOY = doy, Tmax_C = Tmax_C, Tmin_C = Tmin_C, PPT_cm = prcp_mm / 10))

    years <- startYear:endYear
    n_years <- length(years)
    if(!all(years %in% unique(weath.data$year)))
      stop("simstartyr or endyr out of weather data range")
    for(y in seq_along(years)) {
      data_sw <- data_all[weath.data$year == years[y], ]
      data_sw[, -1] <- round(data_sw[, -1], 2) #weather.digits
      weathDataList[[y]] <- methods::new("swWeatherData",
                              year = years[y],
                              data = data.matrix(data_sw, rownames.force = FALSE)) #strip row.names, otherwise they consume about 60% of file size
    }
    names(weathDataList) <- as.character(years)
    weath.data <- weathDataList
  }

  weathDataList
})


get_DayMet_cellID <- compiler::cmpfun(function(coords_WGS84) {
  # Determine 1-km cell that contains requested location
  res_DayMet <- 1000L

  proj_LCC <- sp::CRS("+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  proj_WGS84 <- sp::CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

  xy_LCC <- sp::coordinates(sp::spTransform(sp::SpatialPoints(coords = coords_WGS84, proj4string = proj_WGS84), proj_LCC))
  dm_LCC <- floor(xy_LCC / res_DayMet) # Origin at lower-lef corner (-2015000, -3037000)
    ## ==> (0, 0)- cell includes xlim = [0, 1000[ and ylim = [0, 1000[
    ## ==> at 100-m and 1-m scale: ok; but some deviations at 0.5-m scale

  cellID <- apply(dm_LCC, 1, FUN = function(chr) paste0("daymet_pixel_",
                        if(chr[1] < 0) "-" else "+", formatC(abs(chr[1]), width=6, flag="0", format="d"), "_",
                        if(chr[2] < 0) "-" else "+", formatC(abs(chr[2]), width=6, flag="0", format="d")))

  dm_LCC <- res_DayMet * dm_LCC + 500 # center of 1-km cells to avoid projection errors at cell margins
  dm_WGS84 <- sp::coordinates(sp::spTransform(sp::SpatialPoints(coords = dm_LCC, proj4string = proj_LCC), proj_WGS84))

  list(cellID = cellID, dm_LCC = dm_LCC, dm_WGS84 = dm_WGS84)
})

#' @return A list of which each element represents one year of daily weather data of class \linkS4class{swWeatherData}.
#' Units are [degree Celsius] for temperature and [cm / day] and for precipitation.
#' @references
#'  \href{https://daymet.ornl.gov/}{daymet website}
#'  publication: Thornton, P.E., Running, S.W., White, M.A. 1997. Generating surfaces of daily meteorological variables over large regions of complex terrain. Journal of Hydrology 190: 214 - 251. http://dx.doi.org/10.1016/S0022-1694(96)03128-9
#'  dataset v3: Thornton, P.E., M.M. Thornton, B.W. Mayer, Y. Wei, R. Devarakonda, R.S. Vose, and R.B. Cook. 2016. Daymet: Daily Surface Weather Data on a 1-km Grid for North America, Version 3. ORNL DAAC, Oak Ridge, Tennessee, USA. Accessed Month DD, YYYY. Time period: YYYY-MM-DD to YYYY-MM-DD. Spatial Range: N=DD.DD, S=DD.DD, E=DDD.DD, W=DDD.DD. http://dx.doi.org/10.3334/ORNLDAAC/1328
#'  \hred{https://github.com/khufkens/daymetr}{DaymetR package}
get_DayMet_NorthAmerica <- compiler::cmpfun(function(dir_data, cellID, Xdm_WGS84, Ydm_WGS84, start_year = simstartyr, end_year = endyr) {
  # Filename for data of this 1-km cell
  ftemp <- file.path(dir_data, paste0(cellID, "_", start_year, "_", end_year, ".csv"))

  # Get data
  pwd <- getwd()
  get_from_ornl <- TRUE
  if(file.exists(ftemp)){
    dm_temp <- try(read.table(ftemp, sep = ",", skip = 6, header = TRUE), silent=TRUE)
    if(!inherits(dm_temp, "try-error")) get_from_ornl <- FALSE
  }
  if(get_from_ornl){
    setwd(dir_data)
    # DaymetR package: https://bitbucket.org/khufkens/daymetr
    dm_temp <- try(DaymetR::download.daymet(site=cellID, lat=Ydm_WGS84, lon=Xdm_WGS84, start_yr=start_year, end_yr=end_year, internal=TRUE, quiet=TRUE), silent=TRUE)
  }

  # Convert to Rsoilwat format
  if(!inherits(dm_temp, "try-error")){
    if(exists(cellID, envir=.GlobalEnv)){
      temp <- get(cellID, envir=.GlobalEnv)$data
    } else if(!get_from_ornl && inherits(dm_temp, "data.frame")){
      temp <- dm_temp
    } else stop(paste("Daymet data not successful", cellID))

    data_all <- with(temp, data.frame(year, yday, tmax..deg.c., tmin..deg.c., prcp..mm.day./10))
    stopifnot(!anyNA(data_all), sum(data_all == -9999L) == 0)
    template_sw <- data.frame(matrix(NA, nrow=366, ncol=4, dimnames=list(NULL, c("DOY", "Tmax_C", "Tmin_C", "PPT_cm"))))

    years <- start_year:end_year
    weathDataList <- list()
    for(y in seq_along(years)){
      data_sw <- template_sw
      # All Daymet years, including leap years, have 1 - 365 days. For leap years, the Daymet database includes leap day. Values for December 31 are discarded from leap years to maintain a 365-day year.
      data_sw[1:365, ] <- data_all[data_all$year == years[y], -1]
      if(isLeapYear(years[y])){
        data_sw[366, ] <- c(366, data_sw[365, -1])
      }
      data_sw[, -1] <- round(data_sw[, -1], 2) #weather.digits
      weathDataList[[y]] <- methods::new("swWeatherData",
                                year=years[y],
                                data = data.matrix(data_sw[if(isLeapYear(years[y])) 1:366 else 1:365, ], rownames.force=FALSE)) #strip row.names, otherwise they consume about 60% of file size
    }
    names(weathDataList) <- as.character(years)
  } else {
    weathDataList <- dm_temp
  }

  # Clean up
  if (exists(cellID, envir = .GlobalEnv))
    rm(list = cellID, envir = .GlobalEnv)
  setwd(pwd)

  weathDataList
})


ExtractGriddedDailyWeatherFromDayMet_NorthAmerica_swWeather <- compiler::cmpfun(function(dir_data, site_ids, coords_WGS84, start_year, end_year) {
  xy_WGS84 <- matrix(unlist(coords_WGS84), ncol = 2)[1, , drop = FALSE]
  dm <- get_DayMet_cellID(xy_WGS84)

  get_DayMet_NorthAmerica(
    dir_data = dir_data,
    cellID = dm$cellID[1],
    Xdm_WGS84 = dm$dm_WGS84[1, 1], Ydm_WGS84 = dm$dm_WGS84[1, 2],
    start_year, end_year)
})

# Function to be executed for all SoilWat-sites together
#' @return An invisible zero. A list of which each element represents one year of daily weather data of class \linkS4class{swWeatherData}. The list is copied to the weather database.
#' Units are [degree Celsius] for temperature and [cm / day] and for precipitation.
#' @references
#'  \href{https://daymet.ornl.gov/}{daymet website}
#'  publication: Thornton, P.E., Running, S.W., White, M.A. 1997. Generating surfaces of daily meteorological variables over large regions of complex terrain. Journal of Hydrology 190: 214 - 251. http://dx.doi.org/10.1016/S0022-1694(96)03128-9
#'  dataset v3: Thornton, P.E., M.M. Thornton, B.W. Mayer, Y. Wei, R. Devarakonda, R.S. Vose, and R.B. Cook. 2016. Daymet: Daily Surface Weather Data on a 1-km Grid for North America, Version 3. ORNL DAAC, Oak Ridge, Tennessee, USA. Accessed Month DD, YYYY. Time period: YYYY-MM-DD to YYYY-MM-DD. Spatial Range: N=DD.DD, S=DD.DD, E=DDD.DD, W=DDD.DD. http://dx.doi.org/10.3334/ORNLDAAC/1328
ExtractGriddedDailyWeatherFromDayMet_NorthAmerica_dbW <- compiler::cmpfun(function(dir_data, site_ids, coords_WGS84, start_year, end_year, dir_temp = tempdir(), dbW_compression_type = "gzip") {
  print(paste("Started 'ExtractGriddedDailyWeatherFromDayMet_NorthAmerica' at", Sys.time()))

  # Check if weather data was previously partially extracted
  wtemp_file <- file.path(dir_temp, "DayMet_weather_temp.rds")
  site_ids_done <- if (file.exists(wtemp_file)) readRDS(wtemp_file) else NULL
  iuse <- !(site_ids %in% site_ids_done)

  if (sum(iuse) > 0) {
    site_ids_todo <- site_ids[iuse]
    xy_WGS84 <- coords_WGS84[iuse, , drop=FALSE]
    dm <- get_DayMet_cellID(xy_WGS84)

    #TODO: re-write for parallel processing (does it make sense to download in parallel?)
    # Extract weather data sequentially for requested locations
    for (idm in seq_along(site_ids_todo)) {
      print(paste(Sys.time(), "DayMet data extraction of site", site_ids_todo[idm], "at", paste(round(coords_WGS84[idm, ], 4), collapse="/")))

      weatherData <- get_DayMet_NorthAmerica(
        dir_data = dir_data,
        cellID = dm$cellID[idm],
        Xdm_WGS84 = dm$dm_WGS84[idm, 1], Ydm_WGS84 = dm$dm_WGS84[idm, 2],
        start_year, end_year)

      if (!inherits(weatherData, "try-error")) {
        # Store site weather data in weather database
        data_blob <- Rsoilwat31::dbW_weatherData_to_blob(weatherData, type = dbW_compression_type)
        Rsoilwat31:::dbW_addWeatherDataNoCheck(Site_id = site_ids_todo[idm],
          Scenario_id = 1,
          StartYear = start_year,
          EndYear = end_year,
          weather_blob = data_blob)

        site_ids_done <- c(site_ids_done, site_ids_todo[idm])
        saveRDS(site_ids_done, file = wtemp_file)
      } else {
        print(paste(Sys.time(), "DayMet data extraction NOT successful for site", site_ids_todo[idm], weatherData))
      }
    }
  }

  print(paste("Finished 'ExtractGriddedDailyWeatherFromDayMet_NorthAmerica' at", Sys.time()))

  invisible(0)
})


# Function to be executed for all SoilWat-sites together
#' @return An invisible zero. A list of which each element represents one year of daily weather data of class \linkS4class{swWeatherData}. The list is copied to the weather database.
#' Units are [degree Celsius] for temperature and [cm / day] and for precipitation.
ExtractGriddedDailyWeatherFromNRCan_10km_Canada <- compiler::cmpfun(function(dir_data,
  site_ids, coords_WGS84, start_year, end_year,
  dir_temp = tempdir(), dbW_compression_type = "gzip", do_parallel = FALSE, ncores = 1L) {

  print(paste("Started 'ExtractGriddedDailyWeatherFromNRCan_10km_Canada' at", Sys.time()))

  NRC_years <- as.integer(list.dirs(path=dir_temp, recursive=FALSE, full.names=FALSE))
  NRC_target_years <- NRC_years[NRC_years %in% start_year:end_year]
  stopifnot(start_year:end_year %in% NRC_target_years)

  vars <- c("max", "min", "pcp") # units = C, C, mm/day
  prj_geographicWGS84 <- sp::CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
  prj_geographicNAD83 <- sp::CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")

  sp_locs <- sp::SpatialPoints(coords=coords_WGS84, proj4string=prj_geographicWGS84)
  sp_locs <- sp::spTransform(sp_locs, CRSobj=prj_geographicNAD83)

  if (do_parallel)
    raster::beginCluster(n = ncores, type = "SOCK")

  #TODO: re-write for a more memory friendly approach

  # Check if weather data was partially extracted already
  wtemp_file <- file.path(dir_temp, "NRCan_weather_temp.RData")
  if(file.exists(wtemp_file)){
    load(wtemp_file) # NRC_weather, iy
    yr_offset <- iy
    NRC_use_years <- NRC_target_years[-(1:iy)]
  } else {
    NRC_weather <- array(NA, dim=c(length(sp_locs), 366, length(NRC_target_years), 3), dimnames=list(NULL, NULL, NRC_target_years, c("Tmax(C)", "Tmin(C)", "PPT(mm)")))
    NRC_use_years <- NRC_target_years
    yr_offset <- 0
  }

  # Extract weather data for all locations together for each day of each year
  pwd <- getwd()
  for(iy in seq_along(NRC_use_years)){ # Loop through years
    print(paste(Sys.time(), "NRC data extraction of year", NRC_use_years[iy]))
    setwd(file.path(dir_temp, NRC_use_years[iy]))
    NRC_days <- list.files() #find all days for this year
    ndays <- length(NRC_days) / length(vars)
    stopifnot(ndays == if(isLeapYear(NRC_use_years[iy])) 366 else 365)

    # Stack rasters for each day and extract data
    NRC_stack <- raster::stack(NRC_days, RAT=FALSE, quick=TRUE)
    raster::projection(NRC_stack) <- prj_geographicNAD83
    temp <- round(raster::extract(NRC_stack, sp_locs), 2) #weather.digits; [sp_locs, NRC_days x vars]

    # Convert extraction information to array
    ivars <- substr(NRC_days, 1, 3) # sapply(vars, nchar) == 3
    for(iv in seq_along(vars)){
      idays <- as.integer(sapply(strsplit(NRC_days[vars[iv] == ivars], split="[_.]"), FUN=function(x) x[2]))
      NRC_weather[, 1:ndays, yr_offset + iy, iv] <- temp[, which(vars[iv] == ivars)[order(idays)][1:ndays]]
    }
    save(NRC_weather, iy, file=wtemp_file)
  }
  setwd(pwd)
  if (do_parallel)
    raster::endCluster()


  # Convert weather array to SoilWat weather objects for each sites
  NRC_weather[, , , "PPT(mm)"] <- NRC_weather[, , , "PPT(mm)"] / 10	# convert from mm/day to cm/day

  for (i in seq_along(site_ids)) {
    if (i %% 100 == 1)
      print(paste(Sys.time(), "storing NRC weather data of site_id", site_ids[i], i, "of", length(site_ids), "sites in database"))

    weatherData <- list()
    for (iy in seq_along(NRC_target_years)) {
      doys <- if (isLeapYear(NRC_use_years[iy])) 1:366 else 1:365
      data_sw <- cbind(doys, NRC_weather[i, doys, iy, ]) #DOY Tmax(C) Tmin(C) PPT(cm) [ppt was converted from mm to cm]
      colnames(data_sw) <- c("DOY", "Tmax_C", "Tmin_C", "PPT_cm")
      weatherData[[iy]] <- methods::new("swWeatherData",
                              year = NRC_target_years[iy],
                              data = data.matrix(data_sw, rownames.force = FALSE))
    }
    names(weatherData) <- as.character(NRC_target_years)

    # Store site weather data in weather database
    data_blob <- Rsoilwat31::dbW_weatherData_to_blob(weatherData, type = dbW_compression_type)
    Rsoilwat31:::dbW_addWeatherDataNoCheck(Site_id = site_ids[i],
      Scenario_id = 1,
      StartYear = start_year,
      EndYear = end_year,
      weather_blob = data_blob)
  }
  #unlink(file=wtemp_file)

  print(paste("Finished 'ExtractGriddedDailyWeatherFromNRCan_10km_Canada' at", Sys.time()))

  rm(NRC_weather, weatherData, data_blob)
  gc()

  invisible(0)
})

get_NCEPCFSR_data <- compiler::cmpfun(function(dat_sites, daily = FALSE, monthly = FALSE,
                cfsr_so,
                yearLow, yearHigh, dir.in.cfsr, dir_temp,
                n_site_per_core = 100,
                do_parallel = FALSE, parallel_backend = "cluster", cl = NULL,
                rm_mc_files = FALSE, continueAfterAbort = FALSE) {

#str(dat_sites): 'data.frame':	n_sites obs. of  3 variables:
# $ WeatherFolder: chr  ...
# $ X_WGS84      : num  -117 -117 -117 -117 -120 ...
# $ Y_WGS84      : num  32.8 32.8 32.8 32.8 38.9 ...

  years <- yearLow:yearHigh

  # directory paths
  dir_temp_cfsr <- file.path(dir_temp, "temp_NCEFCFSR")
  dir_temp_sites <- file.path(dir_temp_cfsr, dat_sites[, "WeatherFolder"])

  # determine previous efforts
  if (continueAfterAbort) {
    i_done <- file.exists(dir_temp_sites)
    if (sum(i_done) > 0) {
      for (i in which(i_done)) {
        i_done[i] <-
          if (monthly) {
            file.exists(file.path(dir_temp_sites[i], "mc.csv")) ||
            {file.exists(file.path(dir_temp_sites[i], "cc.txt")) &&
            file.exists(file.path(dir_temp_sites[i], "rh.txt")) &&
            file.exists(file.path(dir_temp_sites[i], "ws.txt"))}
          } else {
            TRUE
          } && if (daily) {
            d_files <- list.files(dir_temp_sites[i], pattern = "weath.")
            d_years <- as.integer(sapply(strsplit(d_files, ".", fixed = TRUE), function(x) x[2]))
            all(d_years %in% years)
          } else {
            TRUE
          }
        if (!i_done[i])
          unlink(dir_temp_sites[i], recursive = TRUE)
      }
    }
    i_todo <- !i_done

  } else {
    i_todo <- rep(TRUE, nrow(dat_sites))
  }

  # prepare tasks
  # do the extractions, loop over chunks of sites
  n_sites <- sum(i_todo)
  n_sites_all <- nrow(dat_sites)

  if (n_sites > 0) {
    dat_sites_todo <- dat_sites[i_todo, ]

    dir.create(dir_temp_cfsr, showWarnings = FALSE)
    temp <- lapply(dir_temp_sites, dir.create, showWarnings = FALSE)
    dir_temp.sitesC <- gsub("/", "//", normalizePath(dir_temp_sites)) # C-style paths; they cannot be relative to ~

    n_years <- length(years)
    n_climvars <- n_dailyvars <- 3
    do_sites <- parallel::splitIndices(n_sites, ceiling(n_sites / n_site_per_core))
    do_daily <- expand.grid(types = seq_len(n_dailyvars) - 1, months = st_mo, years = years)

    dtemp <- getwd()
    setwd(dir.in.cfsr)

    # set up parallel
    if (do_parallel) {
      obj2exp <- gather_objects_for_export(
        varlist = c("load_NCEPCFSR_shlib", "cfsr_so", "dir.in.cfsr"),
        list_envs = list(local = environment(), parent = parent.frame(), global = .GlobalEnv))

      if (identical(parallel_backend, "mpi")) {
        export_objects_to_workers(obj2exp, "mpi")
        Rmpi::mpi.bcast.cmd(load_NCEPCFSR_shlib(cfsr_so))
        Rmpi::mpi.bcast.cmd(setwd(dir.in.cfsr))

      } else if (identical(parallel_backend, "cluster")) {
        export_objects_to_workers(obj2exp, "cluster", cl)
        parallel::clusterEvalQ(cl, load_NCEPCFSR_shlib(cfsr_so))
        parallel::clusterEvalQ(cl, setwd(dir.in.cfsr))
      }
    }

    for (k in seq_along(do_sites)) {
      print(paste(Sys.time(), ": NCEP/CFSR extraction of",
        if(daily) "daily",
        if(daily && monthly) "and",
        if(monthly) "monthly",
        "data: chunk", k, "of", length(do_sites)))

      nDailyReads <- nDailyWrites <- nMonthlyReads <- nMonthlyWrites <- 0
      ntemp <- length(do_sites[[k]])
      irows <- do_sites[[k]]
      longs <- dat_sites_todo[irows, "X_WGS84"]
      lats <- dat_sites_todo[irows, "Y_WGS84"]
      dtemp <- dir_temp.sitesC[irows]

#      if (print.debug)
#        print(paste(Sys.time(), "cfsr chunk", k, ": # open R files", system2(command="lsof", args="-c R | wc -l", stdout=TRUE)))

      if (do_parallel) {
        if (identical(parallel_backend, "mpi")) {
          if (daily) {
            nDailyReads <- Rmpi::mpi.applyLB(X = seq_len(nrow(do_daily)),
              FUN = gribDailyWeatherData, do_daily = do_daily, nSites = ntemp,
              latitudes = lats, longitudes = longs)

            nDailyWrites <- Rmpi::mpi.applyLB(X = years, FUN = writeDailyWeatherData,
              nSites = ntemp, siteNames = dat_sites_todo[irows, "WeatherFolder"],
              siteDirsC = dtemp)
          }
          if (monthly) {
            nMonthlyReads <- Rmpi::mpi.applyLB(X = 0L:(n_climvars - 1L),
              FUN = gribMonthlyClimate, nSites = ntemp, latitudes = lats,
              longitudes = longs, siteDirsC = dtemp, yearLow = yearLow, yearHigh = yearHigh)
          }
          if (monthly && k == length(do_sites)) { # only do at the end
            nMonthlyWrites <- Rmpi::mpi.applyLB(X = seq_len(n_sites_all),
              FUN = writeMonthlyClimate, siteDirsC = dir_temp.sitesC)
          }

        } else if (identical(parallel_backend, "cluster")) {
          if (daily) {
            nDailyReads <- parallel::clusterApplyLB(cl, x = seq_len(nrow(do_daily)),
              fun = gribDailyWeatherData, do_daily = do_daily, nSites = ntemp,
              latitudes = lats, longitudes = longs)

            nDailyWrites <- parallel::clusterApplyLB(cl, x = years, fun = writeDailyWeatherData,
              nSites = ntemp, siteNames = dat_sites_todo[irows, "WeatherFolder"],
              siteDirsC = dtemp)
          }
          if (monthly) {
            nMonthlyReads <- parallel::clusterApplyLB(cl, x = 0L:(n_climvars - 1L),
              fun = gribMonthlyClimate, nSites = ntemp, latitudes = lats,
              longitudes = longs, siteDirsC = dtemp, yearLow = yearLow, yearHigh = yearHigh)
          }
          if (monthly && k == length(do_sites)) { # only do at the end
            nMonthlyWrites <- parallel::clusterApplyLB(cl, x = seq_len(n_sites_all),
              fun = writeMonthlyClimate, siteDirsC = dir_temp.sitesC)
          }
        }

      } else {
          if (daily) {
            nDailyReads <- lapply(X = seq_len(nrow(do_daily)),
              FUN = gribDailyWeatherData, do_daily = do_daily, nSites = ntemp,
              latitudes = lats, longitudes = longs)

            nDailyWrites <- lapply(X = years, FUN = writeDailyWeatherData,
              nSites = ntemp, siteNames = dat_sites_todo[irows, "WeatherFolder"],
              siteDirsC = dtemp)
          }
          if (monthly) {
            nMonthlyReads <- lapply(X = 0L:(n_climvars - 1L),
              FUN = gribMonthlyClimate, nSites = ntemp, latitudes = lats,
              longitudes = longs, siteDirsC = dtemp, yearLow = yearLow, yearHigh = yearHigh)
          }
          if (monthly && k == length(do_sites)) { # only do at the end
            nMonthlyWrites <- lapply(X = seq_len(n_sites_all),
              FUN = writeMonthlyClimate, siteDirsC = dir_temp.sitesC)
          }
      }

      # check that all was done
      if (daily) {
        nDailyReads <- do.call(sum, nDailyReads)
        nDailyWrites <- do.call(sum, nDailyWrites)
        stopifnot(nDailyReads == nrow(do_daily), nDailyWrites == n_years)
      }
      if (monthly) {
        nMonthlyReads <- do.call(sum, nMonthlyReads)
        stopifnot(nMonthlyReads == n_climvars)
      }
      if (monthly && k == length(do_sites)) { # only do at the end
        nMonthlyWrites <- do.call(sum, nMonthlyWrites)
        stopifnot(nMonthlyWrites == n_sites)
      }
    }

    # clean up parallel
    if (do_parallel) {
      if (identical(parallel_backend, "mpi")) {
        Rmpi::mpi.bcast.cmd(rm(list = ls()))
        Rmpi::mpi.bcast.cmd(gc())
      }
      if (identical(parallel_backend, "cluster")) {
        parallel::clusterEvalQ(cl, rm(list = ls()))
        parallel::clusterEvalQ(cl, gc())
      }
    }

    setwd(dtemp)
  }


  # concatenating the monthlyClimate csv files
  if (monthly) {
    res_clim <- data.frame(matrix(NA, nrow = n_sites_all, ncol = 1 + n_climvars * 12))
    colnames(res_clim) <- c("WeatherFolder", paste0("Cloud_m", st_mo), paste0("Wind_m", st_mo), paste0("RH_m", st_mo))
    res_clim[, "WeatherFolder"] <- dat_sites[, "WeatherFolder"]

    for (i in seq_len(n_sites_all)) {
      ftemp <- file.path(dir_temp_sites[i], "mc.csv")
      if (file.exists(ftemp)) {
        table.mc <- read.csv(file=ftemp, comment="", stringsAsFactors=FALSE)
        res_clim[i, 1 + st_mo] <- table.mc[, "Cloud_Cover"]
        res_clim[i, 1 + 12 + st_mo] <- table.mc[, "Surface_Wind"]
        res_clim[i, 1 + 24 + st_mo] <- table.mc[, "Rel_Humidity"]

        if (rm_mc_files == TRUE) unlink(ftemp)
      }
    }
  } else {
    res_clim <- NULL
  }

  list(dir_temp_cfsr = dir_temp_cfsr, res_clim = res_clim)
})


GriddedDailyWeatherFromNCEPCFSR_Global <- compiler::cmpfun(function(site_ids, dat_sites, start_year, end_year,
  meta_cfsr, n_site_per_core = 100, do_parallel = FALSE, parallel_backend = "cluster", cl = NULL,
  rm_temp = TRUE, continueAfterAbort = FALSE, dir_temp = tempdir(),
  dbW_compression_type = "gzip") {

  #Citations: Saha, S., et al. 2010. NCEP Climate Forecast System Reanalysis (CFSR) Selected Hourly Time-Series Products, January 1979 to December 2010. Research Data Archive at the National Center for Atmospheric Research, Computational and Information Systems Laboratory. http://dx.doi.org/10.5065/D6513W89.
  # http://rda.ucar.edu/datasets/ds093.1/. Accessed 8 March 2012.

  # do the extractions
  etemp <- get_NCEPCFSR_data(dat_sites = dat_sites,
    daily = TRUE, monthly =  FALSE,
    cfsr_so = meta_cfsr$cfsr_so,
    yearLow = start_year, yearHigh = end_year,
    dir.in.cfsr = meta_cfsr$dir.in.cfsr,
    dir_temp = dir_temp,
    n_site_per_core = n_site_per_core,
    do_parallel = do_parallel,
    parallel_backend = parallel_backend, cl = cl,
    rm_mc_files = TRUE,
    continueAfterAbort = continueAfterAbort)

  # move the weather data into the database
  for (i in seq_along(site_ids)) {
    weatherData <- Rsoilwat31::getWeatherData_folders(
      LookupWeatherFolder = etemp$dir_temp_cfsr,
      weatherDirName = dat_sites[i, "WeatherFolder"],
      filebasename = "weath",
      startYear = start_year,
      endYear = end_year)

    # Store site weather data in weather database
    data_blob <- Rsoilwat31::dbW_weatherData_to_blob(weatherData, type = dbW_compression_type)
    Rsoilwat31:::dbW_addWeatherDataNoCheck(Site_id = site_ids[i],
      Scenario_id = 1,
      StartYear = start_year,
      EndYear = end_year,
      weather_blob = data_blob)
  }

  if (rm_temp) {
    dir.remove(etemp$dir_temp_cfsr)
    temp <- lapply(c("ppt", "tmax", "tmin"), FUN=function(x) dir.remove(file.path(meta_cfsr$dir.in.cfsr, "temporary_dy", x)))
  }

  print(paste("Finished 'ExtractGriddedDailyWeatherFromNCEPCFSR_Global' at", Sys.time()))

  invisible(0)
})

