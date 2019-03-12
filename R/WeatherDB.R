#------------------------DAILY WEATHER

#' Lookup IDs of sites as found in a weather database
#'
#' @param sim_size A list with at least one named element \code{runIDs_sites}.
#' @param label_WeatherData A vector of character strings. The names of the
#'   weather data.
#' @param fdbWeather A character string. The path to the weather database. This
#'   is only used if there is no current valid connection.
#'
#' @return The list \code{sim_size} with an updated respectively newly added
#'   named element \code{runIDs_sites_by_dbW}. The element
#'   \code{runIDs_sites_by_dbW} is only added if there is a current valid
#'   connection to a weather database or if one can be established.
update_runIDs_sites_by_dbW <- function(sim_size, label_WeatherData,
  fdbWeather = NULL, verbose = FALSE) {

  # Check if there is a weather database
  do_get0 <- rSOILWAT2::dbW_IsValid() ||
    rSOILWAT2::dbW_setConnection(fdbWeather)

  if (do_get0) {
    # Check if 'runIDs_sites_by_dbW' should be updated
    do_get1 <- is.null(sim_size[["runIDs_sites_by_dbW"]])
    do_get2 <- anyNA(sim_size[["runIDs_sites_by_dbW"]])
    do_get3 <- !identical(length(sim_size[["runIDs_sites_by_dbW"]]),
      length(sim_size[["runIDs_sites"]]))

    if (do_get1 || do_get2 || do_get3) {
      if (verbose) {
        print(paste0("rSFSW2's 'update_runIDs_sites_by_dbW': is calling the ",
          "potentially time-consuming function 'rSOILWAT2::dbW_getSiteId'"))
      }
      sim_size[["runIDs_sites_by_dbW"]] <- rSOILWAT2::dbW_getSiteId(
        Labels = label_WeatherData[sim_size[["runIDs_sites"]]])
    }
  }

  sim_size
}


#' Create and populate a \pkg{rSOILWAT2} daily weather \var{SQLite} database
#' @export
make_dbW <- function(SFSW2_prj_meta, SWRunInformation, opt_parallel, opt_chunks,
  opt_behave, deleteTmpSQLFiles, verbose = FALSE, print.debug = FALSE) {

  if (verbose) {
    t1 <- Sys.time()
    temp_call <- shQuote(match.call()[1])
    print(paste0("rSFSW2's ", temp_call, ": started at ", t1))

    on.exit({
      print(paste0("rSFSW2's ", temp_call, ": ended after ",
      round(difftime(Sys.time(), t1, units = "secs"), 2), " s"))
      cat("\n")}, add = TRUE)
  }

  temp_runIDs_sites <- SFSW2_prj_meta[["sim_size"]][["runIDs_sites"]]
  site_data <- data.frame(Site_id = SWRunInformation$site_id,
    Latitude = SWRunInformation$Y_WGS84, Longitude = SWRunInformation$X_WGS84,
    Label = SWRunInformation$WeatherFolder, stringsAsFactors = FALSE)
  site_data[site_data == "NA"] <- NA

  do_new <- TRUE # flag to indicate if a new weather database should be created
  do_add <- FALSE # flag to indicate if ambient daily weather data will be added
  add_runIDs_sites <- NULL

  #--- Check if weather database exists and contains requested data
  if (file.exists(SFSW2_prj_meta[["fnames_in"]][["fdbWeather"]])) {
    if (opt_behave[["resume"]]) {
      if (verbose) {
        print(paste0("rSFSW2's ", temp_call, ": checks existing weather ",
        "database for complete location/sites and scenario tables."))
      }

      do_new <- FALSE
      rSOILWAT2::dbW_setConnection(
        SFSW2_prj_meta[["fnames_in"]][["fdbWeather"]])
      on.exit(rSOILWAT2::dbW_disconnectConnection(), add = TRUE)

      #-- Check if requested climate scenarios are listed in table;
      # if not add to database
      stopifnot(rSOILWAT2::dbW_addScenarios(
        SFSW2_prj_meta[["sim_scens"]][["id"]]))

      #-- Check if requested sites are complete
      # - Site is not in weather database: add to database
      i_new <- is.na(SFSW2_prj_meta[["sim_size"]][["runIDs_sites_by_dbW"]])
      if (any(i_new)) {
        i_new <- temp_runIDs_sites[i_new]
        stopifnot(rSOILWAT2::dbW_addSites(site_data[i_new, ]))

        add_runIDs_sites <- c(add_runIDs_sites, site_data[i_new, "Site_id"])
        do_add <- TRUE
      }

      # - Site is already in weather database but without ambient weather data
      #   (e.g., because a previous run was prematurely terminated)
      imiss <- find_sites_with_bad_weather(
        fdbWeather = SFSW2_prj_meta[["fnames_in"]][["fdbWeather"]],
        siteID_by_dbW = SFSW2_prj_meta[["sim_size"]][["runIDs_sites_by_dbW"]],
        scen_labels = SFSW2_prj_meta[["sim_scens"]][["ambient"]],
        chunk_size = opt_chunks[["ensembleCollectSize"]], verbose = verbose)

      if (any(imiss)) {
        do_add <- TRUE
        add_runIDs_sites <- c(add_runIDs_sites,
          site_data[temp_runIDs_sites[imiss], "Site_id"])
      }

    } else {
      print("Removing old weather database")
      unlink(SFSW2_prj_meta[["fnames_in"]][["fdbWeather"]])
    }
  }


  if (do_new) {
    #--- Create a new weather database
    # weather database contains rows for 1:max(SWRunInformation$site_id)
    # (whether included or not)
    stopifnot(rSOILWAT2::dbW_createDatabase(
      dbFilePath = SFSW2_prj_meta[["fnames_in"]][["fdbWeather"]],
      site_data = site_data,
      Scenarios = SFSW2_prj_meta[["sim_scens"]][["id"]],
      compression_type =
        SFSW2_prj_meta[["opt_input"]][["set_dbW_compresstype"]]))
    do_add <- TRUE
    add_runIDs_sites <- temp_runIDs_sites
  }

  rSOILWAT2::dbW_setConnection(SFSW2_prj_meta[["fnames_in"]][["fdbWeather"]])
  on.exit(rSOILWAT2::dbW_disconnectConnection(), add = TRUE)


  # Obtain siteIDs as seen by the weather database
  if (length(add_runIDs_sites) > 0) {
    add_runIDs_sites <- sort(unique(add_runIDs_sites))
    add_siteIDs_by_dbW <- rSOILWAT2::dbW_getSiteId(
      Labels = site_data[add_runIDs_sites, "Label"])
    if (anyNA(add_siteIDs_by_dbW)) {
      stop("Not all sites (labels) available in weather database.")
    }
  }

  #--- Extract weather data and move to database based on
  # inclusion-invariant 'site_id'
  if (do_add && length(add_runIDs_sites) > 0) {
    # Extract weather data per site
    if (verbose) {
      print(paste(Sys.time(), "started with moving single site weather data",
        "to database"))
    }

    dw_source <- SWRunInformation[add_runIDs_sites, "dailyweather_source"]
    temp <- dw_source %in% c("LookupWeatherFolder", "Maurer2002_NorthAmerica")
    ids_single <- which(temp) ## position in 'runIDs_sites'

    if (length(ids_single) > 0) {
      if (any(dw_source == "Maurer2002_NorthAmerica"))
        Maurer <- with(SWRunInformation[add_runIDs_sites[ids_single], ],
          create_filename_for_Maurer2002_NorthAmerica(X_WGS84, Y_WGS84))

      for (i in seq_along(ids_single)) {
        i_idss <- ids_single[i]
        i_site <- add_runIDs_sites[i_idss]

        if (verbose && i %% 100 == 1)
          print(paste(Sys.time(), "storing weather data of site",
            SWRunInformation$Label[i_site], ":", i, "of", length(ids_single),
            "sites in database"))

        if (dw_source[i_idss] == "LookupWeatherFolder") {
          weatherData <- ExtractLookupWeatherFolder(dir.weather =
            file.path(SFSW2_prj_meta[["project_paths"]][["dir_in_treat"]],
              "LookupWeatherFolder"),
            weatherfoldername = SWRunInformation$WeatherFolder[i_site],
            SFSW2_prj_meta[["opt_sim"]][["dbW_digits"]])

        } else if (dw_source[i_idss] == "Maurer2002_NorthAmerica") {
          weatherData <- ExtractGriddedDailyWeatherFromMaurer2002_NorthAmerica(
            dir_data = SFSW2_prj_meta[["project_paths"]][["dir_maurer2002"]],
            cellname = Maurer[i],
            start_year = SFSW2_prj_meta[["sim_time"]][["overall_simstartyr"]],
            end_year = SFSW2_prj_meta[["sim_time"]][["overall_endyr"]],
            SFSW2_prj_meta[["opt_sim"]][["dbW_digits"]],
            verbose = verbose)

        } else {
          stop(paste(dw_source[i_idss], "not implemented"))
        }

        if (!is.null(weatherData) && length(weatherData) > 0 &&
          !inherits(weatherData, "try-error")) {

          years <- as.integer(names(weatherData))
          data_blob <- rSOILWAT2::dbW_weatherData_to_blob(weatherData,
            type = SFSW2_prj_meta[["opt_input"]][["set_dbW_compresstype"]])
          rSOILWAT2:::dbW_addWeatherDataNoCheck(
            Site_id = add_siteIDs_by_dbW[i_idss], Scenario_id = 1,
            StartYear = years[1], EndYear = years[length(years)],
            weather_blob = data_blob)

        } else {
          print(paste("Moving daily weather data to database unsuccessful",
            SWRunInformation$Label[i_site]))
        }
      }
    }

    # Extract weather data for all sites based on inclusion-invariant 'site_id'
    if (verbose) {
      print(paste(Sys.time(), "started with extracting gridded weather",
        "data to database"))
    }

    ids_DayMet_extraction <- which(dw_source == "DayMet_NorthAmerica")
    ids_NRCan_extraction <- which(dw_source == "NRCan_10km_Canada")
    ids_NCEPCFSR_extraction <- which(dw_source == "NCEPCFSR_Global")
    ids_Livneh2013_extraction <- which(dw_source == "Livneh2013_NorthAmerica")
    ids_Livneh2016_extraction <- which(dw_source == "Livneh2016_NorthAmerica")


    # Weather extraction with parallel support
    if (length(ids_NRCan_extraction) > 0 ||
      length(ids_NCEPCFSR_extraction) > 0 ||
      length(ids_Livneh2013_extraction) > 0 ||
      length(ids_Livneh2016_extraction) > 0) {

      #--- Set up parallelization
      setup_SFSW2_cluster(opt_parallel,
        dir_out = SFSW2_prj_meta[["project_paths"]][["dir_prj"]],
        verbose = verbose)
      on.exit(exit_SFSW2_cluster(verbose), add = TRUE)

      on.exit(set_full_RNG(SFSW2_prj_meta[["rng_specs"]][["seed_prev"]],
        kind = SFSW2_prj_meta[["rng_specs"]][["RNGkind_prev"]][1],
        normal.kind = SFSW2_prj_meta[["rng_specs"]][["RNGkind_prev"]][2]),
        add = TRUE)
     }

    if (length(ids_DayMet_extraction) > 0) {
      irow <- add_runIDs_sites[ids_DayMet_extraction]
      ExtractGriddedDailyWeatherFromDayMet_NorthAmerica_dbW(
        dir_data = SFSW2_prj_meta[["project_paths"]][["dir_daymet"]],
        site_ids = SWRunInformation$site_id[irow],
        site_ids_by_dbW = add_siteIDs_by_dbW[ids_DayMet_extraction],
        coords_WGS84 = SWRunInformation[irow, c("X_WGS84", "Y_WGS84"),
          drop = FALSE],
        start_year = SFSW2_prj_meta[["sim_time"]][["overall_simstartyr"]],
        end_year = SFSW2_prj_meta[["sim_time"]][["overall_endyr"]],
        dir_temp = SFSW2_prj_meta[["project_paths"]][["dir_out_temp"]],
        dbW_compression_type =
          SFSW2_prj_meta[["opt_input"]][["set_dbW_compresstype"]],
        SFSW2_prj_meta[["opt_sim"]][["dbW_digits"]],
        verbose = verbose)
    }

    if (length(ids_NRCan_extraction) > 0) {
      irow <- add_runIDs_sites[ids_NRCan_extraction]
      ExtractGriddedDailyWeatherFromNRCan_10km_Canada(
        dir_data = SFSW2_prj_meta[["project_paths"]][["dir.ex.NRCan"]],
        site_ids = SWRunInformation$site_id[irow],
        site_ids_by_dbW = add_siteIDs_by_dbW[ids_NRCan_extraction],
        coords_WGS84 = SWRunInformation[irow, c("X_WGS84", "Y_WGS84"),
          drop = FALSE],
        start_year = SFSW2_prj_meta[["sim_time"]][["overall_simstartyr"]],
        end_year = SFSW2_prj_meta[["sim_time"]][["overall_endyr"]],
        dir_temp = SFSW2_prj_meta[["project_paths"]][["dir_out_temp"]],
        dbW_compression_type =
          SFSW2_prj_meta[["opt_input"]][["set_dbW_compresstype"]],
        SFSW2_prj_meta[["opt_sim"]][["dbW_digits"]],
        verbose = verbose)
    }

    if (length(ids_Livneh2013_extraction) > 0) {
      irow <- add_runIDs_sites[ids_Livneh2013_extraction]
      extract_daily_weather_from_livneh(
        version      = 'v2013',
        dir_data     = SFSW2_prj_meta[["project_paths"]][["dir.ex.Livneh2013"]],
        dir_temp     = SFSW2_prj_meta[["project_paths"]][["dir_out_temp"]],
        site_ids     = SWRunInformation$site_id[irow],
        site_ids_by_dbW = add_siteIDs_by_dbW[ids_Livneh2013_extraction],
        coords       = SWRunInformation[irow, c("X_WGS84", "Y_WGS84"),
          drop = FALSE],
        start_year   = SFSW2_prj_meta[["sim_time"]][["overall_simstartyr"]],
        end_year     = SFSW2_prj_meta[["sim_time"]][["overall_endyr"]],
        f_check      = TRUE,
        backup       = TRUE,
        comp_type    = SFSW2_prj_meta[["opt_input"]][["set_dbW_compresstype"]],
        dbW_digits   = SFSW2_prj_meta[["opt_sim"]][["dbW_digits"]],
        verbose     = verbose)
    }

    if (length(ids_Livneh2016_extraction) > 0) {
      irow <- add_runIDs_sites[ids_Livneh2016_extraction]
      extract_daily_weather_from_livneh(
        version      = 'v2016',
        dir_data     = SFSW2_prj_meta[["project_paths"]][["dir.ex.Livneh2016"]],
        dir_temp     = SFSW2_prj_meta[["project_paths"]][["dir_out_temp"]],
        site_ids     = SWRunInformation$site_id[irow],
        site_ids_by_dbW = add_siteIDs_by_dbW[ids_Livneh2016_extraction],
        coords       = SWRunInformation[irow, c("X_WGS84", "Y_WGS84"),
          drop = FALSE],
        start_year   = SFSW2_prj_meta[["sim_time"]][["overall_simstartyr"]],
        end_year     = SFSW2_prj_meta[["sim_time"]][["overall_endyr"]],
        f_check      = TRUE,
        backup       = TRUE,
        comp_type    = SFSW2_prj_meta[["opt_input"]][["set_dbW_compresstype"]],
        dbW_digits   = SFSW2_prj_meta[["opt_sim"]][["dbW_digits"]],
        verbose     = verbose)
    }

    if (length(ids_NCEPCFSR_extraction) > 0) {
      if (is.null(SFSW2_prj_meta[["prepd_CFSR"]]) ||
        inherits(SFSW2_prj_meta[["prepd_CFSR"]], "try-error") ||
        !dir.exists(SFSW2_prj_meta[["prepd_CFSR"]][["dir_ex_cfsr"]])) {

        SFSW2_prj_meta[["prepd_CFSR"]] <- try(prepare_NCEPCFSR_extraction(
          dir_in = SFSW2_prj_meta[["project_paths"]][["dir_in"]],
          dir.cfsr.data =
            SFSW2_prj_meta[["project_paths"]][["dir.ex.NCEPCFSR"]]))
      }
      stopifnot(!inherits(SFSW2_prj_meta[["prepd_CFSR"]], "try-error"))

      irow <- add_runIDs_sites[ids_NCEPCFSR_extraction]
      GriddedDailyWeatherFromNCEPCFSR_Global(
        site_ids = SWRunInformation$site_id[irow],
        site_ids_by_dbW = add_siteIDs_by_dbW[ids_NCEPCFSR_extraction],
        dat_sites = SWRunInformation[irow,
          c("WeatherFolder", "X_WGS84", "Y_WGS84"), drop = FALSE],
        tag_WeatherFolder = SFSW2_prj_meta[["opt_sim"]][["tag_WeatherFolder"]],
        start_year = SFSW2_prj_meta[["sim_time"]][["overall_simstartyr"]],
        end_year = SFSW2_prj_meta[["sim_time"]][["overall_endyr"]],
        meta_cfsr = SFSW2_prj_meta[["prepd_CFSR"]],
        n_site_per_core = opt_chunks[["DailyWeatherFromNCEPCFSR_Global"]],
        rm_temp = deleteTmpSQLFiles,
        resume = opt_behave[["resume"]],
        dir_temp = SFSW2_prj_meta[["project_paths"]][["dir_out_temp"]],
        dbW_compression_type =
          SFSW2_prj_meta[["opt_input"]][["set_dbW_compresstype"]],
        dbW_digits = SFSW2_prj_meta[["opt_sim"]][["dbW_digits"]],
        verbose = verbose, print.debug = print.debug)
    }

    oe <- sys.on.exit()
    oe <- remove_from_onexit_expression(oe, "exit_SFSW2_cluster")
    on.exit(eval(oe), add = FALSE)
  }

  invisible(TRUE)
}


#' Check that version of \var{\sQuote{dbWeather}} suffices
check_dbWeather_version <- function(fdbWeather) {
  rSOILWAT2::dbW_setConnection(fdbWeather)
  on.exit(rSOILWAT2::dbW_disconnectConnection(), add = TRUE)

  v_dbW <- rSOILWAT2::dbW_version()
  success <- v_dbW >= SFSW2_glovars[["minVersion_dbWeather"]]

  if (!success) {
    print(paste0("The version (", v_dbW, ") of the daily weather database ",
      "is outdated; min. version required: ",
      SFSW2_glovars[["minVersion_dbWeather"]]))
    if (v_dbW >= "1")
      print(paste("Use function 'rSOILWAT2:::dbW_upgrade_v1to2' etc. to ",
        "upgrade your version 1.y.z weather database to version >=",
        SFSW2_glovars[["minVersion_dbWeather"]]))
  }

  success
}


prepare_NCEPCFSR_extraction <- function(dir_in, dir.cfsr.data,
  dir.cfsr.code = dir.cfsr.data) {

  # nolint start
  writeLines(c("'NCEPCFSR' extractions: make sure the following conditions are met:",
    "  1) Compiled 'wgrib2' executable is located in '/opt/local/bin/' or in 'dir_in/ncepcfsr/'",
    "     Instructions for how to compile 'wgrib2' can be found in the 'ncepcfsr_convert.c'.",
    "     The code of wgrib2 is available from ",
    "         http://www.cpc.ncep.noaa.gov/products/wesley/wgrib2/",
    "  3) Appropriate grib files (the data) are located in directory 'dir.cfsr.data'. ",
    "     Info about the gribfiles is in 'ncepcfsr_convert.c'"))
  # nolint end

  dir.create(dir_ex_cfsr <- file.path(dir_in, "ncepcfsr"), showWarnings = FALSE)

  #Check for wgrib2 (http://www.cpc.ncep.noaa.gov/products/wesley/wgrib2/)
  if (!file.exists(wgrib2 <- file.path(dir_ex_cfsr, "wgrib2"))) {
    path_wgrib2 <- if (nchar(temp <- Sys.which("wgrib2")) > 0) {
        temp
      } else if (file.exists(temp <- "/opt/local/bin/wgrib2")) temp else ""
    stopifnot(nchar(path_wgrib2) > 0)
    file.copy(from = path_wgrib2, to = wgrib2)
  }

  #Soft link to gribbed data
  fname_gribDir <- "griblargeC2"
  dir.grib <- file.path(dir_ex_cfsr, fname_gribDir)
  if (!file.exists(dir.grib)) {
    # value of gribDir defined in cfsr_convert.c
    # If a previous soft link still exists at dir.grib, but cannot be found by
    # file.exists() because the link is 'dead' (but it is still listed by
    # list.files()) then use options -F -f to remove the link before creating
    # a new one -- otherwise, the command 'ln' reports an error
    stopifnot(system2(command = "ln", args = paste("-sFf",
      shQuote(file.path(dir.cfsr.data, fname_gribDir)),
      shQuote(dir.grib))) == 0)
  }

  #Set up temporary directory for C code to store objects
  if (file.exists(ftemp <- file.path(dir_ex_cfsr, "temporary_dy"))) {
    unlink(ftemp, recursive = TRUE)
  }
  temp <- lapply(lapply(c("tmax", "tmin", "ppt"), FUN = function(x)
    file.path(ftemp, x)), FUN = function(x)
      dir.create(x, recursive = TRUE, showWarnings = FALSE))

  list(dir_ex_cfsr = dir_ex_cfsr)
}


# Wrapper functions for C code to access NCEP/CFSR data and write out to
# temporary files
gribDailyWeatherData <- function(id, do_daily, nSites, latitudes, longitudes,
  print.debug = FALSE) {

  if (print.debug) {
    print(paste(Sys.time(), ": NCEP/CFSR daily extraction: year =",
      do_daily[id, "years"], "month =", do_daily[id, "months"], "variable =",
      do_daily[id, "types"]))
  }

  gribData <- .C(C_dailyWeather2_R,
            nSites = as.integer(nSites),
            latitudes = as.double(latitudes),
            longitudes = as.double(longitudes),
            year = as.integer(do_daily[id, "years"]),
            month = as.integer(do_daily[id, "months"]),
            type = as.integer(do_daily[id, "types"]),
            printdebug = if (print.debug) 1L else 0L)
  1L
}

writeDailyWeatherData <- function(year, nSites, siteNames, siteDirsC) {

  dataWrite <- .C(C_dailyWeather2Write_R,
            nSites = as.integer(nSites),
            siteNames = as.character(siteNames),
            siteDirs = as.character(siteDirsC),
            year = as.integer(year))
  1L
}

gribMonthlyClimate <- function(type, nSites, latitudes, longitudes, siteDirsC,
  yearLow, yearHigh, print.debug = FALSE) {

  if (print.debug) {
    print(paste(Sys.time(), ": monthly NCEP/CFSR extraction."))
  }

  gribData <- .C(C_monthlyClimate2_R,
            nSites = as.integer(nSites),
            latitudes = as.double(latitudes),
            longitudes = as.double(longitudes),
            siteDirs = as.character(siteDirsC),
            yearLow = as.integer(yearLow),
            yearHigh = as.integer(yearHigh),
            type = as.integer(type),
            printdebug = if (print.debug) 1L else 0L)
  1L
}

writeMonthlyClimate <- function(id, siteDirsC) {
  dataWrite <- .C(C_writeMonthlyClimate2_R,
    siteDir = as.character(siteDirsC[id]))
  1L
}

create_filename_for_Maurer2002_NorthAmerica <- function(X_WGS84, Y_WGS84) { # nolint
  origin <- 28.8125
  res <- 0.125
  xtemp <- origin + round((X_WGS84 - origin) / res, 0) * res
  ytemp <- origin + round((Y_WGS84 - origin) / res, 0) * res

  gsub("[[:space:]]", "", paste("data",
    formatC(ytemp, digits = 4, format = "f"),
    formatC(xtemp, digits = 4, format = "f"), sep = "_"))
}


# TODO replace with rSOILWAT2::getWeatherData_folders
ExtractLookupWeatherFolder <- function(dir.weather, weatherfoldername,
  dbW_digits) {

  weatherData <- list()
  WeatherFolder <- file.path(dir.weather, weatherfoldername)
  weath <- list.files(WeatherFolder, pattern = "weath.")

  if (length(weath) > 0) {
    years <- as.numeric(sub(pattern = "weath.", replacement = "", weath))
    stopifnot(!anyNA(years))

    for (j in seq_along(weath)) {
      temp <- utils::read.table(file.path(WeatherFolder, weath[j]),
        header = FALSE, comment.char = "#", blank.lines.skip = TRUE, sep = "\t")
      data_sw <- as.matrix(temp)
      data_sw[, -1] <- round(data_sw[, -1], dbW_digits)
      colnames(data_sw) <- c("DOY", "Tmax_C", "Tmin_C", "PPT_cm")
      weatherData[[j]] <- methods::new("swWeatherData", year = years[j],
        data = data.matrix(data_sw, rownames.force = FALSE))
    }

    names(weatherData) <- years
  }

  weatherData
}



#' Extract gridded daily weather from Maurer et al. 2002 (updated in 2010) for
#' North American sites
#'
#' Data are available for years 1949-2010 at a 1/12-degree resolution.
#'
#' @return An invisible zero. A list of which each element represents one year
#'   of daily weather data of class
#'   \code{\link[rSOILWAT2:swWeatherData-class]{rSOILWAT2::swWeatherData}}. The
#'   list is copied to the weather database. Units are [degree Celsius] for
#'   temperature and [cm / day] and for precipitation.
#'
#' @references Maurer, E. P., A. W. Wood, J. C. Adam, D. P. Lettenmaier, and B.
#'   Nijssen. 2002. A long-term hydrologically based dataset of land surface
#'   fluxes and states for the conterminous United States. Journal of Climate
#'   15:3237-3251.
#' @export
ExtractGriddedDailyWeatherFromMaurer2002_NorthAmerica <- function(dir_data, # nolint
  cellname, start_year, end_year, dbW_digits, verbose = FALSE) {

  if (verbose) {
    t1 <- Sys.time()
    temp_call <- shQuote(match.call()[1])
    print(paste0("rSFSW2's ", temp_call, ": started at ", t1))

    on.exit({
      print(paste0("rSFSW2's ", temp_call, ": ended after ",
      round(difftime(Sys.time(), t1, units = "secs"), 2), " s"))
      cat("\n")}, add = TRUE)
  } else {
    temp_call <- NULL
  }

  # Check requested years
  year_range <- update_requested_years(start_year, end_year,
    has_start_year = 1949, has_end_year = 2010, temp_call = temp_call,
    verbose = verbose)

  years <- year_range[["start_year"]]:year_range[["end_year"]]

  weathDataList <- list()

  #read data from Maurer et al. 2002
  weath.data <- try(utils::read.table(file.path(dir_data, cellname),
    comment.char = ""), silent = TRUE)

  if (!inherits(weath.data, "try-error")) {
    colnames(weath.data) <- c("year", "month", "day", "prcp_mm", "Tmax_C",
      "Tmin_C", "Wind_mPERs")

    #times
    doy <- 1 + as.POSIXlt(seq(
      from = with(weath.data[1, ], ISOdate(year, month, day, tz = "UTC")),
      to = with(weath.data[nrow(weath.data), ],
        ISOdate(year, month, day, tz = "UTC")),
      by = "1 day"))$yday

    # conversion precipitation: mm/day -> cm/day
    data_all <- with(weath.data, data.frame(
      DOY = doy, Tmax_C = Tmax_C, Tmin_C = Tmin_C, PPT_cm = prcp_mm / 10))

    if (!all(years %in% unique(weath.data$year)))
      stop("simstartyr or endyr out of weather data range")
    for (y in seq_along(years)) {
      data_sw <- data_all[weath.data$year == years[y], ]
      data_sw[, -1] <- round(data_sw[, -1], dbW_digits)
      weathDataList[[y]] <- methods::new("swWeatherData", year = years[y],
        data = data.matrix(data_sw, rownames.force = FALSE))
      # strip row.names, otherwise they consume about 60% of file size
    }
    names(weathDataList) <- as.character(years)
    weath.data <- weathDataList
  }

  weathDataList
}


get_DayMet_cellID <- function(coords_WGS84) {

  # Determine 1-km cell that contains requested location
  res_DayMet <- 1000L

  proj_LCC <- sp::CRS(paste("+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5",
    "+lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
    "+ellps=WGS84 +towgs84=0,0,0"))
  proj_WGS84 <- sp::CRS(paste("+init=epsg:4326 +proj=longlat +ellps=WGS84",
    "+datum=WGS84 +no_defs +towgs84=0,0,0"))

  xy_LCC <- sp::coordinates(sp::spTransform(sp::SpatialPoints(
    coords = coords_WGS84, proj4string = proj_WGS84), proj_LCC))

  dm_LCC <- floor(xy_LCC / res_DayMet)
  # Origin at lower-lef corner (-2015000, -3037000)
    ## ==> (0, 0)- cell includes xlim = [0, 1000[ and ylim = [0, 1000[
    ## ==> at 100-m and 1-m scale: ok; but some deviations at 0.5-m scale

  cellID <- apply(dm_LCC, 1, FUN = function(chr) paste0("daymet_pixel_",
    if (chr[1] < 0) "-" else "+",
    formatC(abs(chr[1]), width = 6, flag = "0", format = "d"), "_",
    if (chr[2] < 0) "-" else "+",
    formatC(abs(chr[2]), width = 6, flag = "0", format = "d")))

  # center of 1-km cells to avoid projection errors at cell margins
  dm_LCC <- res_DayMet * dm_LCC + 500
  dm_WGS84 <- sp::coordinates(sp::spTransform(sp::SpatialPoints(
    coords = dm_LCC, proj4string = proj_LCC), proj_WGS84))

  list(cellID = cellID, dm_LCC = dm_LCC, dm_WGS84 = dm_WGS84)
}

get_DayMet_NorthAmerica <- function(dir_data, cellID, Xdm_WGS84, Ydm_WGS84,
  start_year, end_year, dbW_digits) {

  # Filename for data of this 1-km cell
  ftemp <- file.path(dir_data, paste0(cellID, "_", start_year, "_",
    end_year, ".csv"))

  # Get data
  get_from_ornl <- TRUE
  if (file.exists(ftemp)) {
    dm_temp <- try(utils::read.table(ftemp, sep = ",", skip = 6, header = TRUE),
      silent = TRUE)
    if (!inherits(dm_temp, "try-error")) get_from_ornl <- FALSE
  }

  if (get_from_ornl) {
    stopifnot(requireNamespace("daymetr"))

    if (utils::packageVersion("daymetr") < "1.1") {
      # 'daymetr::download_daymet' saves downloaded file on disk in current
      # working directory
      wd_prev <- getwd()
      setwd(dir_data)
      on.exit(setwd(wd_prev), add = TRUE)
      on.exit(if (exists(cellID, envir = globalenv())) {
          rm(list = cellID, envir = globalenv())}, add = TRUE)

      dm_temp <- try(daymetr::download_daymet(site = cellID, lat = Ydm_WGS84,
        lon = Xdm_WGS84, start = start_year, end = end_year, internal = TRUE,
        silent = TRUE), silent = TRUE)

    } else {
      # 'daymetr::download_daymet' saves downloaded file on disk at `path`
      # daymetr returns either list with data.frame OR saves data on
      # specifiable path, but not both --> we choose to save on disk because
      # we want to store data for re-use by other projects
      dm_temp <- try(daymetr::download_daymet(site = cellID, lat = Ydm_WGS84,
        lon = Xdm_WGS84, start = start_year, end = end_year, path = dir_data,
        internal = FALSE, silent = TRUE), silent = TRUE)
    }

    if (inherits(dm_temp, "try-error")) {
      unlink(ftemp)
    }
  }

  # Convert to rSOILWAT2 format
  if (!inherits(dm_temp, "try-error")) {
    temp <- if (inherits(dm_temp, "list") &&
      inherits(dm_temp[["data"]], "data.frame")) {
        # 'daymetr' >= v1.2 returns a list with a named element 'data'
        # unless internal = FALSE
        dm_temp[["data"]]

      } else if (exists(cellID, envir = globalenv())) {
        # 'daymetr' < v1.2 created a variable 'cellID' in the global environment
        get(cellID, envir = globalenv())$data

      } else if (!get_from_ornl && inherits(dm_temp, "data.frame")) {
        # already read from file
        dm_temp

      } else {
        # not yet read from file
        temp <- try(utils::read.table(ftemp, sep = ",", skip = 6,
          header = TRUE), silent = TRUE)
        if (inherits(temp, "try-error") || !inherits(temp, "data.frame")) {
          stop(paste("Daymet data not successful", shQuote(cellID)))
        }
        temp
      }

    req_cols <- c("DOY", "Tmax_C", "Tmin_C", "PPT_cm")
    data_all <- with(temp, data.frame(Year = year, DOY = yday,
      Tmax_C = tmax..deg.c., Tmin_C = tmin..deg.c.,
      PPT_cm = prcp..mm.day. / 10))
    stopifnot(!anyNA(data_all), sum(data_all == -9999L) == 0)
    template_sw <- data.frame(matrix(NA, nrow = 366, ncol = 4,
      dimnames = list(NULL, req_cols)))

    years <- start_year:end_year
    weathDataList <- list()

    for (y in seq_along(years)) {
      data_sw <- template_sw
      # All Daymet years, including leap years, have 1 - 365 days. For
      # leap years, the Daymet database includes leap day. Values for
      # December 31 are discarded from leap years to maintain a 365-day year.
      irow <- data_all["Year"] == years[y]
      data_sw[1:365, req_cols] <- data_all[irow, req_cols]

      if (isLeapYear(years[y])) {
        doys <- 1:366
        data_sw[366, ] <- c(366, data_sw[365, -1])
      } else {
        doys <- 1:365
      }

      data_sw[, -1] <- round(data_sw[, -1], dbW_digits)
      weathDataList[[y]] <- methods::new("swWeatherData", year = years[y],
         #strip row.names, otherwise they consume about 60% of file size
        data = data.matrix(data_sw[doys, ], rownames.force = FALSE))
    }
    names(weathDataList) <- as.character(years)

  } else {
    # Return error object
    weathDataList <- dm_temp
  }

  weathDataList
}


#' @return A list of class
#'   \code{\link[rSOILWAT2:swWeatherData-class]{rSOILWAT2::swWeatherData}}
#'   objects.
#' @rdname ExtractDayMet
#' @export
ExtractGriddedDailyWeatherFromDayMet_NorthAmerica_swWeather <- function( # nolint
  dir_data, site_ids, coords_WGS84, start_year, end_year, dbW_digits) {

  # Check requested years
  avail_end_year <- as.integer(1900 + as.POSIXlt(Sys.Date())$year - 1)
  year_range <- update_requested_years(start_year, end_year,
    has_start_year = 1980, has_end_year = avail_end_year, temp_call = NULL,
    verbose = FALSE)

  xy_WGS84 <- matrix(unlist(coords_WGS84), ncol = 2)[1, , drop = FALSE]
  dm <- get_DayMet_cellID(xy_WGS84)

  get_DayMet_NorthAmerica(dir_data = dir_data, cellID = dm$cellID[1],
    Xdm_WGS84 = dm$dm_WGS84[1, 1], Ydm_WGS84 = dm$dm_WGS84[1, 2],
    start_year = year_range[["start_year"]],
    end_year = year_range[["end_year"]], dbW_digits)
}

#' Extract gridded daily weather from \var{\dQuote{DayMet}} for North American
#' sites
#'
#' Data are available for years 1980-(last full calendar year) at a 1-km
#' resolution.
#'
#' @return An invisible zero. A list of which each element represents one year
#'   of daily weather data of class
#'   \code{\link[rSOILWAT2:swWeatherData-class]{rSOILWAT2::swWeatherData}}. The
#'   list is copied to the weather database. Units are [degree Celsius] for
#'   temperature and [cm / day] and for precipitation.
#' @references \url{https://daymet.ornl.gov/}
#' @references publication: Thornton, P.E., Running, S.W., White, M.A. 1997.
#'   Generating surfaces of daily meteorological variables over large regions of
#'   complex terrain. Journal of Hydrology 190: 214 - 251.
#'   \url{http://dx.doi.org/10.1016/S0022-1694(96)03128-9}
#' @references Thornton, P.E., M.M. Thornton, B.W. Mayer, Y. Wei, R.
#'   Devarakonda, R.S. Vose, and R.B. Cook. 2016. \var{Daymet}: Daily Surface
#'   Weather Data on a 1-km Grid for North America, Version 3. ORNL DAAC, Oak
#'   Ridge, Tennessee, USA. Accessed \var{Month DD, YYYY}. Time period:
#'   \var{YYYY-MM-DD} to \var{YYYY-MM-DD}. Spatial Range: N = \var{DD.DD}, S =
#'   \var{DD.DD}, E = \var{DDD.DD}, W = \var{DDD.DD}.
#'   \url{http://dx.doi.org/10.3334/ORNLDAAC/1328}
#' @references \url{https://github.com/khufkens/daymetr}
#'
#' @name ExtractDayMet
#' @export
ExtractGriddedDailyWeatherFromDayMet_NorthAmerica_dbW <- function(dir_data, # nolint
  site_ids, site_ids_by_dbW, coords_WGS84, start_year, end_year,
  dir_temp = tempdir(), dbW_compression_type = "gzip", dbW_digits,
  verbose = FALSE) {

  if (verbose) {
    t1 <- Sys.time()
    temp_call <- shQuote(match.call()[1])
    print(paste0("rSFSW2's ", temp_call, ": started at ", t1))

    on.exit({
      print(paste0("rSFSW2's ", temp_call, ": ended after ",
      round(difftime(Sys.time(), t1, units = "secs"), 2), " s"))
      cat("\n")}, add = TRUE)
  } else {
    temp_call <- NULL
  }

  # Check requested years
  avail_end_year <- as.integer(1900 + as.POSIXlt(Sys.Date())$year - 1)
  year_range <- update_requested_years(start_year, end_year,
    has_start_year = 1980, has_end_year = avail_end_year, temp_call = temp_call,
    verbose = verbose)

  # Check if weather data was previously partially extracted
  wtemp_file <- file.path(dir_temp, "DayMet_weather_temp.rds")
  site_ids_done <- if (file.exists(wtemp_file)) readRDS(wtemp_file) else NULL
  iuse <- !(site_ids %in% site_ids_done)

  if (sum(iuse) > 0) {
    site_ids_todo <- site_ids[iuse]
    site_ids_by_dbW_todo <- site_ids_by_dbW[iuse]
    xy_WGS84 <- coords_WGS84[iuse, , drop = FALSE]
    dm <- get_DayMet_cellID(xy_WGS84)

    #TODO: re-write for parallel processing
    # (does it make sense to download in parallel?)
    # Extract weather data sequentially for requested locations
    for (idm in seq_along(site_ids_todo)) {
      print(paste(Sys.time(), "DayMet data extraction of site",
        site_ids_todo[idm], "at",
        paste(round(coords_WGS84[idm, ], 4), collapse = "/")))

      weatherData <- get_DayMet_NorthAmerica(dir_data = dir_data,
        cellID = dm$cellID[idm], Xdm_WGS84 = dm$dm_WGS84[idm, 1],
        Ydm_WGS84 = dm$dm_WGS84[idm, 2],
        start_year = year_range[["start_year"]],
        end_year = year_range[["end_year"]], dbW_digits)

      if (!is.null(weatherData) && length(weatherData) > 0 &&
        !inherits(weatherData, "try-error") &&
        inherits(weatherData[[1]], "swWeatherData")) {

        # Store site weather data in weather database
        data_blob <- rSOILWAT2::dbW_weatherData_to_blob(weatherData,
          type = dbW_compression_type)
        rSOILWAT2:::dbW_addWeatherDataNoCheck(
          Site_id = site_ids_by_dbW_todo[idm],
          Scenario_id = 1,
          StartYear = year_range[["start_year"]],
          EndYear = year_range[["end_year"]],
          weather_blob = data_blob)

        site_ids_done <- c(site_ids_done, site_ids_todo[idm])
        saveRDS(site_ids_done, file = wtemp_file)

      } else {
        print(paste(Sys.time(), "DayMet data extraction NOT successful",
          "for site", site_ids_todo[idm], weatherData))
      }
    }
  }

  invisible(0)
}


#' Extract gridded daily weather from NR Canada for Canadian sites
#'
#' Data are available for years 1950-2013 at a 10-km resolution.
#'
#' @references Hopkinson, R. F., D. W. McKenney, E. J. Milewska, M. F.
#'   Hutchinson, P. Papadopol, and L. A. Vincent. 2011. Impact of Aligning
#'   Climatological Day on Gridding Daily Maximum-Minimum Temperature and
#'   Precipitation over Canada. Journal of Applied Meteorology and Climatology
#'   50:1654-1665.
#' @references Hutchinson, M. F., D. W. McKenney, K. Lawrence, J. H. Pedlar, R.
#'   F. Hopkinson, E. Milewska, and P. Papadopol. 2009. Development and Testing
#'   of Canada-Wide Interpolated Spatial Models of Daily Minimum-Maximum
#'   Temperature and Precipitation for 1961-2003. Journal of Applied Meteorology
#'   and Climatology 48:725-741.
#' @references McKenney, D. W., M. F. Hutchinson, P. Papadopol, K. Lawrence, J.
#'   Pedlar, K. Campbell, E. Milewska, R. F. Hopkinson, D. Price, and T. Owen.
#'   2011. Customized Spatial Climate Models for North America. Bulletin of the
#'   American Meteorological Society 92:1611-1622.
#'
#' @return An invisible zero. A list of which each element represents one year
#'   of daily weather data of class
#'   \code{\link[rSOILWAT2:swWeatherData-class]{rSOILWAT2::swWeatherData}}. The
#'   list is copied to the weather database. Units are [degree Celsius] for
#'   temperature and [cm / day] and for precipitation.
#' @export
ExtractGriddedDailyWeatherFromNRCan_10km_Canada <- function(dir_data, site_ids, # nolint
  site_ids_by_dbW, coords_WGS84, start_year, end_year, dir_temp = tempdir(),
  dbW_compression_type = "gzip", dbW_digits, verbose = FALSE) {

  if (verbose) {
    t1 <- Sys.time()
    temp_call <- shQuote(match.call()[1])
    print(paste0("rSFSW2's ", temp_call, ": started at ", t1))

    on.exit({
      print(paste0("rSFSW2's ", temp_call, ": ended after ",
      round(difftime(Sys.time(), t1, units = "secs"), 2), " s"))
      cat("\n")}, add = TRUE)
  } else {
    temp_call <- NULL
  }

  # Check requested years
  year_range <- update_requested_years(start_year, end_year,
    has_start_year = 1950, has_end_year = 2013, temp_call = temp_call,
    verbose = verbose)

  years <- year_range[["start_year"]]:year_range[["end_year"]]
  NRC_years <- as.integer(list.dirs(path = dir_temp, recursive = FALSE,
    full.names = FALSE))
  NRC_target_years <- NRC_years[NRC_years %in% years]
  stopifnot(years %in% NRC_target_years)

  vars <- c("max", "min", "pcp") # units = C, C, mm/day
  prj_geographicWGS84 <- sp::CRS(paste("+init=epsg:4326 +proj=longlat",
    "+ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
  prj_geographicNAD83 <- sp::CRS(paste("+init=epsg:4269 +proj=longlat",
    "+ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"))

  sp_locs <- sp::SpatialPoints(coords = coords_WGS84,
    proj4string = prj_geographicWGS84)
  sp_locs <- sp::spTransform(sp_locs, CRSobj = prj_geographicNAD83)

  if (SFSW2_glovars[["p_has"]])
    raster::beginCluster(n = SFSW2_glovars[["p_workersN"]], type = "SOCK")

  #TODO: re-write for a more memory friendly approach

  # Check if weather data was partially extracted already
  wtemp_file <- file.path(dir_temp, "NRCan_weather_temp.RData")
  if (file.exists(wtemp_file)) {
    load(wtemp_file) # NRC_weather, iy
    yr_offset <- iy
    NRC_use_years <- NRC_target_years[- (1:iy)]
  } else {
    NRC_weather <- array(NA,
      dim = c(length(sp_locs), 366, length(NRC_target_years), 3),
      dimnames = list(NULL, NULL, NRC_target_years,
        c("Tmax(C)", "Tmin(C)", "PPT(mm)")))
    NRC_use_years <- NRC_target_years
    yr_offset <- 0
  }

  #--- Extract weather data for all locations together for each day of each year
  # Loop through years
  for (iy in seq_along(NRC_use_years)) {
    print(paste(Sys.time(), "NRC data extraction of year", NRC_use_years[iy]))

    # Locate files containing data for each day of this year
    NRC_days <- list.files(path = file.path(dir_temp, NRC_use_years[iy]),
      full.names = TRUE)
    ndays <- length(NRC_days) / length(vars)
    stopifnot(ndays == if (isLeapYear(NRC_use_years[iy])) 366 else 365)

    # Stack rasters for each day and extract data
    NRC_stack <- raster::stack(NRC_days, RAT = FALSE, quick = TRUE)
    raster::projection(NRC_stack) <- prj_geographicNAD83
    # temp has dimensions of [sp_locs, NRC_days x vars]:
    temp <- round(raster::extract(NRC_stack, sp_locs), dbW_digits)

    # Convert extraction information to array
    ivars <- substr(NRC_days, 1, 3)
    for (iv in seq_along(vars)) {
      idays <- as.integer(sapply(strsplit(NRC_days[vars[iv] == ivars],
        split = "[_.]"), FUN = function(x) x[2]))
      NRC_weather[, 1:ndays, yr_offset + iy, iv] <-
        temp[, which(vars[iv] == ivars)[order(idays)][1:ndays]]
    }
    save(NRC_weather, iy, file = wtemp_file)
  }

  if (SFSW2_glovars[["p_has"]])
    raster::endCluster()


  # Convert weather array to SOILWAT2 weather objects for each sites
  # convert from mm/day to cm/day
  NRC_weather[, , , "PPT(mm)"] <- NRC_weather[, , , "PPT(mm)"] / 10

  for (i in seq_along(site_ids)) {
    if (i %% 100 == 1) {
      print(paste(Sys.time(), "storing NRC weather data of site_id",
        site_ids[i], i, "of", length(site_ids), "sites in database"))
    }

    weatherData <- list()
    for (iy in seq_along(NRC_target_years)) {
      doys <- if (isLeapYear(NRC_use_years[iy])) 1:366 else 1:365
      #DOY Tmax(C) Tmin(C) PPT(cm) [ppt was converted from mm to cm]
      data_sw <- cbind(doys, NRC_weather[i, doys, iy, ])
      colnames(data_sw) <- c("DOY", "Tmax_C", "Tmin_C", "PPT_cm")
      weatherData[[iy]] <- methods::new("swWeatherData",
        year = NRC_target_years[iy],
        data = data.matrix(data_sw, rownames.force = FALSE))
    }
    names(weatherData) <- as.character(NRC_target_years)

    if (!is.null(weatherData) && length(weatherData) > 0 &&
      !inherits(weatherData, "try-error")) {

      # Store site weather data in weather database
      data_blob <- rSOILWAT2::dbW_weatherData_to_blob(weatherData,
        type = dbW_compression_type)
      rSOILWAT2:::dbW_addWeatherDataNoCheck(Site_id = site_ids_by_dbW[i],
        Scenario_id = 1,
        StartYear = year_range[["start_year"]],
        EndYear = year_range[["end_year"]],
        weather_blob = data_blob)

    } else {
      print(paste(Sys.time(), "NRC weather data extraction NOT successful",
        "for site", site_ids[i], weatherData))
    }
  }
  gc()

  invisible(0)
}


#TODO(drs): get rid of setwd()
get_NCEPCFSR_data <- function(dat_sites, daily = FALSE, monthly = FALSE,
  dbW_digits = 2, yearLow, yearHigh, dir_ex_cfsr, dir_temp,
  n_site_per_core = 100, rm_mc_files = FALSE, resume = FALSE,
  print.debug = FALSE) {

#str(dat_sites): 'data.frame':  n_sites obs. of  3 variables:
# $ WeatherFolder: chr  ...
# $ X_WGS84      : num  -117 -117 -117 -117 -120 ...
# $ Y_WGS84      : num  32.8 32.8 32.8 32.8 38.9 ...

  years <- yearLow:yearHigh

  # directory paths
  dir_temp_cfsr <- file.path(dir_temp, "temp_NCEFCFSR")
  dir_temp_sites <- file.path(dir_temp_cfsr, dat_sites[, "WeatherFolder"])

  # determine previous efforts
  if (resume) {
    i_done <- file.exists(dir_temp_sites)
    if (sum(i_done) > 0) {
      for (i in which(i_done)) {
        mtemp <- if (monthly) {
            file.exists(file.path(dir_temp_sites[i], "mc.csv")) ||
            (file.exists(file.path(dir_temp_sites[i], "cc.txt")) &&
                file.exists(file.path(dir_temp_sites[i], "rh.txt")) &&
                file.exists(file.path(dir_temp_sites[i], "ws.txt")))
          } else {
            TRUE
          }

        dtemp <- if (daily) {
            d_files <- list.files(dir_temp_sites[i], pattern = "weath.")
            d_years <- as.integer(sapply(strsplit(d_files, ".", fixed = TRUE),
              function(x) x[2]))
            all(d_years %in% years)
          } else {
            TRUE
          }

        i_done[i] <- mtemp && dtemp

        if (!i_done[i]) {
          unlink(dir_temp_sites[i], recursive = TRUE)
        }
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
    if (identical(.Platform$OS.type, "windows")) {
      stop("'get_NCEPCFSR_data' is currently not supported for 'windows' ",
        "platforms.")
    }

    dat_sites_todo <- dat_sites[i_todo, ]

    dir.create(dir_temp_cfsr, showWarnings = FALSE)
    lapply(dir_temp_sites, dir.create, showWarnings = FALSE)

    # C-style paths; they cannot be relative to ~
    dir_temp.sitesC <- gsub("/", "//", normalizePath(dir_temp_sites))

    n_years <- length(years)
    n_climvars <- n_dailyvars <- 3
    do_sites <- parallel::splitIndices(n_sites,
      ceiling(n_sites / n_site_per_core))
    do_daily <- expand.grid(types = seq_len(n_dailyvars) - 1,
      months = SFSW2_glovars[["st_mo"]], years = years)

    dir_prev <- getwd()
    setwd(dir_ex_cfsr)
    on.exit(setwd(dir_prev), add = TRUE)

    # set up parallel
    if (SFSW2_glovars[["p_has"]]) {
      if (identical(SFSW2_glovars[["p_type"]], "mpi")) {
        Rmpi::mpi.bcast.cmd(cmd = setwd, dir = dir_ex_cfsr)

      } else if (identical(SFSW2_glovars[["p_type"]], "socket")) {
        parallel::clusterCall(SFSW2_glovars[["p_cl"]], fun = setwd,
          dir = dir_ex_cfsr)
      }
    }

    for (k in seq_along(do_sites)) {
      print(paste(Sys.time(), ": NCEP/CFSR extraction of",
        if (daily) "daily",
        if (daily && monthly) "and",
        if (monthly) "monthly",
        "data: chunk", k, "of", length(do_sites)))

      nDailyReads <- nDailyWrites <- nMonthlyReads <- nMonthlyWrites <- 0
      ntemp <- length(do_sites[[k]])
      irows <- do_sites[[k]]
      longs <- dat_sites_todo[irows, "X_WGS84"]
      lats <- dat_sites_todo[irows, "Y_WGS84"]
      dtemp <- dir_temp.sitesC[irows]

      if (print.debug) {
        print(paste(Sys.time(), "cfsr chunk", k, ": # open R files",
          system2(command = "lsof", args = "-c R | wc -l", stdout = TRUE)))
      }

      if (SFSW2_glovars[["p_has"]]) {
        if (identical(SFSW2_glovars[["p_type"]], "mpi")) {
          if (daily) {
            nDailyReads <- Rmpi::mpi.applyLB(seq_len(nrow(do_daily)),
              gribDailyWeatherData, do_daily = do_daily, nSites = ntemp,
              latitudes = lats, longitudes = longs, print.debug = print.debug)

            nDailyWrites <- Rmpi::mpi.applyLB(years, writeDailyWeatherData,
              nSites = ntemp,
              siteNames = dat_sites_todo[irows, "WeatherFolder"],
              siteDirsC = dtemp)
          }

          if (monthly) {
            nMonthlyReads <- Rmpi::mpi.applyLB(0L:(n_climvars - 1L),
              gribMonthlyClimate, nSites = ntemp, latitudes = lats,
              longitudes = longs, siteDirsC = dtemp, yearLow = yearLow,
              yearHigh = yearHigh, print.debug = print.debug)
          }
          if (monthly && k == length(do_sites)) {
            # only do at the end
            nMonthlyWrites <- Rmpi::mpi.applyLB(seq_len(n_sites_all),
              writeMonthlyClimate, siteDirsC = dir_temp.sitesC)
          }

        } else if (identical(SFSW2_glovars[["p_type"]], "socket")) {
          if (daily) {
            nDailyReads <- parallel::clusterApplyLB(SFSW2_glovars[["p_cl"]],
              x = seq_len(nrow(do_daily)), fun = gribDailyWeatherData,
              do_daily = do_daily, nSites = ntemp, latitudes = lats,
              longitudes = longs, print.debug = print.debug)

            nDailyWrites <- parallel::clusterApplyLB(SFSW2_glovars[["p_cl"]],
              x = years, fun = writeDailyWeatherData, nSites = ntemp,
              siteNames = dat_sites_todo[irows, "WeatherFolder"],
              siteDirsC = dtemp)
          }

          if (monthly) {
            nMonthlyReads <- parallel::clusterApplyLB(SFSW2_glovars[["p_cl"]],
              x = 0L:(n_climvars - 1L), fun = gribMonthlyClimate,
              nSites = ntemp, latitudes = lats, longitudes = longs,
              siteDirsC = dtemp, yearLow = yearLow, yearHigh = yearHigh,
              print.debug = print.debug)
          }
          if (monthly && k == length(do_sites)) {
            # only do at the end
            nMonthlyWrites <- parallel::clusterApplyLB(SFSW2_glovars[["p_cl"]],
              x = seq_len(n_sites_all), fun = writeMonthlyClimate,
              siteDirsC = dir_temp.sitesC)
          }
        }

        clean_SFSW2_cluster()

      } else {
          if (daily) {
            nDailyReads <- lapply(X = seq_len(nrow(do_daily)),
              FUN = gribDailyWeatherData, do_daily = do_daily, nSites = ntemp,
              latitudes = lats, longitudes = longs, print.debug = print.debug)

            nDailyWrites <- lapply(X = years, FUN = writeDailyWeatherData,
              nSites = ntemp,
              siteNames = dat_sites_todo[irows, "WeatherFolder"],
              siteDirsC = dtemp)
          }

          if (monthly) {
            nMonthlyReads <- lapply(X = 0L:(n_climvars - 1L),
              FUN = gribMonthlyClimate, nSites = ntemp, latitudes = lats,
              longitudes = longs, siteDirsC = dtemp, yearLow = yearLow,
              yearHigh = yearHigh, print.debug = print.debug)
          }
          if (monthly && k == length(do_sites)) {
            # only do at the end
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
      if (monthly && k == length(do_sites)) {
        # only do at the end
        nMonthlyWrites <- do.call(sum, nMonthlyWrites)
        stopifnot(nMonthlyWrites == n_sites)
      }
    }

    setwd(dir_prev)
  }


  # concatenating the monthlyClimate csv files
  if (monthly) {
    res_clim <- data.frame(matrix(NA, nrow = n_sites_all,
      ncol = 1 + n_climvars * 12))
    colnames(res_clim) <- c("WeatherFolder",
      paste0("Cloud_m", SFSW2_glovars[["st_mo"]]),
      paste0("Wind_m", SFSW2_glovars[["st_mo"]]),
      paste0("RH_m", SFSW2_glovars[["st_mo"]]))
    res_clim[, "WeatherFolder"] <- dat_sites[, "WeatherFolder"]

    for (i in seq_len(n_sites_all)) {
      ftemp <- file.path(dir_temp_sites[i], "mc.csv")
      if (file.exists(ftemp)) {
        table.mc <- utils::read.csv(file = ftemp, comment = "",
          stringsAsFactors = FALSE)
        res_clim[i, 1 + SFSW2_glovars[["st_mo"]]] <-
          table.mc[, "Cloud_Cover"]
        res_clim[i, 1 + 12 + SFSW2_glovars[["st_mo"]]] <-
          table.mc[, "Surface_Wind"]
        res_clim[i, 1 + 24 + SFSW2_glovars[["st_mo"]]] <-
          table.mc[, "Rel_Humidity"]

        if (rm_mc_files == TRUE) unlink(ftemp)
      }
    }
  } else {
    res_clim <- NULL
  }

  list(dir_temp_cfsr = dir_temp_cfsr, res_clim = res_clim)
}


#' Extract gridded daily weather from NCEP/CFSR for sites globally
#'
#' @section Daily data: \url{http://rda.ucar.edu/datasets/ds093.1/}:
#'   \var{ds093.1} NCEP Climate Forecast System Reanalysis (CFSR) Selected
#'   Hourly Time-Series Products, January 1979 to December 2010, 0.313-deg:
#'   6-hourly.
#'   \describe{
#'     \item{maximum temperature}{2 m above ground (Kelvin):
#'       \var{tmax.gdas.yyyymm.grb2} --> max of 4 values per day}
#'     \item{minimum temperature}{2 m above ground (Kelvin):
#'       \var{tmin.gdas.yyyymm.grb2} --> max of 4 values per day}
#'     \item{precipitation rate}{ground or water surface (kg m-2 s-1):
#'       \var{prate.gdas.yyyymm.grb2} --> sum of 4 values per day which
#'       are converted to cm / 6-hour}
#'   }
#'
#' @return An invisible zero. A list of which each element represents one year
#'   of daily weather data of class
#'   \code{\link[rSOILWAT2:swWeatherData-class]{rSOILWAT2::swWeatherData}}. The
#'   list is copied to the weather database. Units are [degree Celsius] for
#'   temperature and [cm / day] and for precipitation.
#' @references \href{http://rda.ucar.edu/datasets/ds093.1/}{NCEP/CFSR website}
#' @references Saha, S., et al. 2010. NCEP Climate Forecast System Reanalysis
#'   (CFSR) Selected Hourly Time-Series Products, January 1979 to December 2010.
#'   Research Data Archive at the National Center for Atmospheric Research,
#'   Computational and Information Systems Laboratory.
#'   \url{http://dx.doi.org/10.5065/D6513W89}.
#' @export
GriddedDailyWeatherFromNCEPCFSR_Global <- function(site_ids, site_ids_by_dbW, # nolint
  dat_sites, tag_WeatherFolder, start_year, end_year, meta_cfsr,
  n_site_per_core = 100, rm_temp = TRUE, resume = FALSE, dir_temp = tempdir(),
  dbW_compression_type = "gzip", dbW_digits, verbose = FALSE,
  print.debug = FALSE) {

  if (verbose) {
    t1 <- Sys.time()
    temp_call <- shQuote(match.call()[1])
    print(paste0("rSFSW2's ", temp_call, ": started at ", t1))

    on.exit({
      print(paste0("rSFSW2's ", temp_call, ": ended after ",
      round(difftime(Sys.time(), t1, units = "secs"), 2), " s"))
      cat("\n")}, add = TRUE)
  } else {
    temp_call <- NULL
  }

  # Check requested years
  year_range <- update_requested_years(start_year, end_year,
    has_start_year = 1979, has_end_year = 2010, temp_call = temp_call,
    verbose = verbose)

  # do the extractions
  etemp <- get_NCEPCFSR_data(dat_sites = dat_sites,
    daily = TRUE, monthly =  FALSE, dbW_digits,
    yearLow = year_range[["start_year"]], yearHigh = year_range[["end_year"]],
    dir_ex_cfsr = meta_cfsr$dir_ex_cfsr,
    dir_temp = dir_temp,
    n_site_per_core = n_site_per_core,
    rm_mc_files = TRUE,
    resume = resume,
    print.debug = print.debug)

  # move the weather data into the database
  for (i in seq_along(site_ids)) {
    weatherData <- rSOILWAT2::getWeatherData_folders(
      LookupWeatherFolder = etemp$dir_temp_cfsr,
      weatherDirName = dat_sites[i, "WeatherFolder"],
      filebasename = tag_WeatherFolder,
      startYear = year_range[["start_year"]],
      endYear = year_range[["end_year"]])

    if (!is.null(weatherData) && length(weatherData) > 0 &&
      !inherits(weatherData, "try-error")) {

      # Store site weather data in weather database
      data_blob <- rSOILWAT2::dbW_weatherData_to_blob(weatherData,
        type = dbW_compression_type)
      rSOILWAT2:::dbW_addWeatherDataNoCheck(Site_id = site_ids_by_dbW[i],
        Scenario_id = 1,
        StartYear = year_range[["start_year"]],
        EndYear = year_range[["end_year"]],
        weather_blob = data_blob)

    } else {
      print(paste(Sys.time(), "NCEPCFSR weather data extraction NOT successful",
        "for site", site_ids[i], weatherData))
    }
  }

  if (rm_temp) {
    unlink(etemp$dir_temp_cfsr, recursive = TRUE)
    unlink(meta_cfsr$dir_ex_cfsr, recursive = TRUE)
  }

  invisible(0)
}

########################################################
# Livneh Gridded, Daily Weather Data Extraction
#
# Author - Charles Duso & Caitlin Andrews
########################################################

#' @title Extract daily gridded weather data from Livneh netcdf4 files.
#'
#' @description Extracts daily gridded weather data, including precipitation,
#'              maximum temperature and minimum temperature from Livneh
#'              files:
#'              Spatial resolution: 1/16 degree
#'              Spatial extent:
#'                2013 Version: CONUS & Canadian portion of Columbia River
#'                2016 Version: CONUS, Mexico, & Canada below 53 degrees N
#'              Temporal extend:
#'                2013 Version: 1915 - 2011
#'                2016 Version: 1915 - 2015
# nolint start
#' @references  \href{http://www.esrl.noaa.gov/psd/data/gridded/data.livneh.html}{Livneh2013 info}
#' @references  \href{ftp://livnehpublicstorage.colorado.edu/public/Livneh.2016.Dataset/}{Livneh2016 data}
#' @references  \href{https://www.nature.com/articles/sdata201542}{Livneh2016 reference}
# nolint end
#'
#' @param    version         version of Livneh data. v2013 or v2016.
#' @param    dir_data        directory containing Livneh data
#' @param    dir_temp        the database directory
#' @param    site_ids        the sites to gather weather data for
#' @param    coords          the coordinates for each site in \var{WGS84} format
#' @param    start_year      the start year in the sequence of data to gather
#' @param    end_year        the end year in the sequence of data to gather
#' @param    f_check         flag to check for errors in file structure -
#'  \code{TRUE}
#'                            for check else no integrity check
#' @param    backup          flag to create a backup of the weather data prior
#'                            to insertion in the database
#'                            (can create large files) - \code{TRUE} for backup
#'                            else no backup
#' @param    comp_type       the compression type of the data to
#'                            be inserted into the database
#' @param    run_parallel    whether the extraction should be ran in parallel
#' @param    num_cores       the number of cores to use if parallel
#'
#' @export
extract_daily_weather_from_livneh <- function(version, dir_data, dir_temp, # nolint
  site_ids, site_ids_by_dbW, coords, start_year, end_year, f_check = TRUE,
  backup = TRUE, comp_type = "gzip", dbW_digits = 2, verbose = FALSE) {

  if (verbose) {
    t1 <- Sys.time()
    temp_call <- shQuote(match.call()[1])
    print(paste0("rSFSW2's ", temp_call, ": started at ", t1))

    on.exit({
      print(paste0("rSFSW2's ", temp_call, ": ended after ",
      round(difftime(Sys.time(), t1, units = "secs"), 2), " s"))
      cat("\n")}, add = TRUE)
  } else {
    temp_call <- NULL
  }

  # Set up version based differences - years and tags
  if(version == 'v2013') {
    end_year_livneh = 2011
    tag_livneh <- "Meteorology_Livneh_CONUSExt_v.1.2_2013"
    tag_livneh_esc <- gsub(".", "\\.", tag_livneh, fixed = TRUE)
  }

  if(version == 'v2016') {
    end_year_livneh = 2015
    tag_livneh <- tag_livneh_esc <- "livneh_NAmerExt_1Sep2016"
  }

  # Check requested years
  year_range <- update_requested_years(start_year, end_year,
    has_start_year = 1915, has_end_year = end_year_livneh, temp_call = temp_call,
    verbose = verbose)


    ########################################
    # Ensure necessary packages are loaded
    ########################################
    stopifnot(requireNamespace("rgdal"), requireNamespace("ncdf4"))

    ###################################################################
    # Helper function to convert coordinates to the correct resolution
    ###################################################################
    conv_res <- function(x) {
      round((28.15625 + round((x - 28.15625) / 0.0625, 0) * 0.0625), digits = 5)
    }

    #########################
    # Configuration settings
    #########################
    # Start timer for timing the extraction process
    t_elapsed        <- proc.time()
    if (verbose) {
      print("Preparing to extract weather data from Livneh database.")
    }

    # Go to the directory for the weather database for extraction
    db_files <- list.files(dir_data,
      pattern = paste0("^(", tag_livneh_esc, ").+(\\.nc)$"))

    # Verify presence of every year-month combination
    if (f_check) {
      if (verbose) {
        print("Verifying data integrity.")
      }
      temp <- strsplit(db_files, ".", fixed = TRUE)
      # Months
      if (version == 'v2013') {
        temp <- unlist(lapply(temp, function(x)
        if (length(x) == 5 && x[5] == "nc") x[4]))

        db_months <- formatC(SFSW2_glovars[["st_mo"]], width = 2, flag = "0")
      }

      if (version == 'v2016') {
        temp <- unlist(lapply(temp, function(x)
        if (length(x) == 3 && x[3] == "nc") x[2]))

        db_months <- formatC(SFSW2_glovars[["st_mo"]], width = 2, flag = "0")
      }

      db_years <- unique(substr(temp, 1, 4))

      etemp <- as.vector(outer(db_years, db_months, paste0))
      ltemp <- sapply(etemp, function(tag) any(grepl(tag, db_files)))

      if (!all(ltemp)) {
        stop("ERROR: Monthly data file is missing for year-month: ",
          paste(etemp[!ltemp], collapse = ", "))
      }

      if (verbose) {
        print("Data integrity has been verified; no errors have been detected.")
      }
    }

    # Refine coordinates to resolution suitable for Livneh
    if (verbose) {
      print("Refining coordinates to match database resolution.")
    }
    xy_wgs84 <- apply(coords, 2, conv_res)

    # Create coordinates as spatial points for extraction with raster layers
    prj_geographicWGS84 <- sp::CRS(paste("+proj=longlat +ellps=WGS84",
      "+datum=WGS84 +no_defs +towgs84=0,0,0"))
    sp_locs  <- sp::SpatialPoints(coords = xy_wgs84,
      proj4string = prj_geographicWGS84)

    # Create necessary variables and containers for extraction
    seq_years <-  seq(year_range[["start_year"]], year_range[["end_year"]])
    len_years             <-  length(seq_years)
    site_length           <-  length(site_ids)
    data_sw               <-  array(NA, dim = c(site_length, 366, 3, len_years))

    # Backup RData in the event of an error with insertion
    if (backup) {
      on.exit({
        if (verbose) {
          print("Backing up data object.")
        }
        save(list = ls(environment()), envir = environment(),
          file = file.path(dir_temp, "weathData_Livneh2013.RData"))
        if (verbose) {
          print("Data object has been backed-up.")
        }
      }, add = TRUE)
    }

    if (verbose) {
      print("Extracting data for supplied sequence of years.")
    }

    #######################
    # Extract weather data
    #######################

    # Extract the data for each site for each year for each month
    for (i in seq_len(len_years)) {

      # Make data file names for respective year
      dfiles <- file.path(dir_data, paste0(tag_livneh, ".", seq_years[i],
        formatC(SFSW2_glovars[["st_mo"]], width = 2, format = "d", flag = 0),
        ".nc"))

      if (verbose) {
        print(paste0("Extracting data for year ", seq_years[i]))
      }

      # Extract Weather Data as Raster Stacks
      l_brick    <- lapply(dfiles, raster::brick, varname = "Prec")
#TODO(drs): the next line fails if run non-interactively, but succeeds if
# run interactively; the error message is
# "`names<-`(`*tmp*`, value = sapply(x@layers, names)) :
#      incorrect number of layer names".
# I don't understand this behavior.
if (!interactive()) {
  print(paste("The next function call 'stack(l_brick)' is expected to",
    "fail during this non-interactive run."))
}
      l_stack    <- raster::stack(l_brick, quick = TRUE)
      prec       <- raster::extract(x = l_stack, y = sp_locs, method = "simple")
      prec       <- round(prec / 10, dbW_digits) # mm day-1 to cm day-1

      l_brick    <- lapply(dfiles, raster::brick, varname = "Tmax")
      l_stack    <- raster::stack(l_brick, varname = "Tmax", quick = TRUE)
      tmax       <- raster::extract(x = l_stack, y = sp_locs, method = "simple")
      tmax       <- round(tmax, dbW_digits)

      l_brick    <- lapply(dfiles, raster::brick, varname = "Tmin")
      l_stack    <- raster::stack(l_brick, varname = "Tmin", quick = TRUE)
      tmin       <- raster::extract(x = l_stack, y = sp_locs, method = "simple")
      tmin       <- round(tmin, dbW_digits)

      # Add data to global data array
      ids <- if (isLeapYear(seq_years[i])) seq_len(366) else seq_len(365)
      data_sw[, ids, 1, i] <- tmax[, ids]
      data_sw[, ids, 2, i] <- tmin[, ids]
      data_sw[, ids, 3, i] <- prec[, ids]
    }

    # Format data and add it to the weather database
    if (verbose) {
      print("Inserting data into weather database.")
    }

    for (i in seq_len(site_length)) {
      weather_data <- list()
      for (k in seq_len(len_years)) {
        doys <- if (isLeapYear(seq_years[k])) seq_len(366) else seq_len(365)
        out  <- cbind(doys, data_sw[i, doys, , k])
        colnames(out) <- c("DOY", "Tmax_C", "Tmin_C", "PPT_cm")
        weather_data[[k]] <-
                    new("swWeatherData",
                         year = seq_years[k],
                         data = data.matrix(out, rownames.force = FALSE))
      }
      names(weather_data) <- as.character(seq_years)

      # Write out to data blob so that data is appropriate for database
      data_blob <- rSOILWAT2::dbW_weatherData_to_blob(weather_data,
        type = comp_type)

      # Store site weather data in weather database
      rSOILWAT2:::dbW_addWeatherDataNoCheck(Site_id = site_ids_by_dbW[i],
        Scenario_id  = 1,
        StartYear    = year_range[["start_year"]],
        EndYear      = year_range[["end_year"]],
        weather_blob = data_blob)
    }


    #######################
    # Clean-up environment
    #######################

    if (verbose) {
      print("Weather data has been successfully inserted.")
    }

    # Remove files & clean garbage to free-up RAM for executions that
    # don't just involve database creation
    if (verbose) {
      print("Cleaning up garbage.")
    }
    rm(weather_data, out, data_sw, data_blob, l_stack, l_brick, tmax, tmin,
       prec)
    gc()

    # End timer and notify user that extraction has finished
    if (verbose) {
      print("Data has been inserted.")
      print(proc.time() - t_elapsed)
    }
    invisible(0)
}

# End of Livneh extraction code
########################################################


#---Functions to determine sources of daily weather
dw_LookupWeatherFolder <- function(dw_source, dw_names, exinfo, site_dat,
  sim_time, path = NULL, MoreArgs = NULL) {

  if (!dir.exists(path))
    stop("'dw_LookupWeatherFolder': ", path, " does not exist.")

  lwf_cond1 <- MoreArgs[["it_use"]]["LookupWeatherFolder"] &&
    !anyNA(MoreArgs[["it_lwf"]])
  lwf_cond2 <- !anyNA(MoreArgs[["ri_lwf"]]) &&
    !any(grepl("GriddedDailyWeatherFrom", names(exinfo)[unlist(exinfo)]))
  lwf_cond3 <- MoreArgs[["ie_use"]]["LookupWeatherFolder"] &&
    !anyNA(MoreArgs[["it_lwf"]])
  lwf_cond4 <- any(MoreArgs[["create_treatments"]] == "LookupWeatherFolder")

  n <- length(MoreArgs[["runIDs_sites"]])
  there <- rep(FALSE, times = n)

  if (any(lwf_cond1, lwf_cond2, lwf_cond3, lwf_cond4)) {
    # Check which requested lookup weather folders are available
    if (lwf_cond1) {
      # Use weather folder names from treatment-design file
      temp <- sapply(MoreArgs[["it_lwf"]], function(ix)
        if (is.na(ix)) FALSE else file.exists(file.path(path, ix)))
      there <- there | temp
      ids <- temp & is.na(dw_names)
      dw_names[ids] <- MoreArgs[["it_lwf"]][ids]
    }

    if (lwf_cond2) {
      # Use weather folder names from master file
      temp <- sapply(MoreArgs[["ri_lwf"]], function(ix)
        if (is.na(ix)) FALSE else file.exists(file.path(path, ix)))
      there <- there | temp
      ids <- temp & is.na(dw_names)
      dw_names[ids] <- MoreArgs[["ri_lwf"]][ids]
    }

    if (lwf_cond3) {
      # Use weather folder name from experimental input file
      # TODO(drs): I don't believe that this currently works because
      # rows are not correctly lined up
      there <- there | rep(any(sapply(MoreArgs[["ie_lwf"]], function(ix)
        if (is.na(ix)) FALSE else file.exists(file.path(path, ix)))), times = n)
      there <- there | temp
      ids <- temp & is.na(dw_names)
      dw_names[ids] <- MoreArgs[["ie_lwf"]][ids]
    }

    if (any(there))
      dw_source[there] <- "LookupWeatherFolder"
  }

  list(source = dw_source, name = dw_names, n = sum(there))
}

dw_Maurer2002_NorthAmerica <- function(dw_source, dw_names, exinfo, site_dat,
  sim_time, path = NULL, MoreArgs = NULL) {

  if (!dir.exists(path))
    stop("'dw_Maurer2002_NorthAmerica': ", path, " does not exist.")

  there <- 0

  if (exinfo$GriddedDailyWeatherFromMaurer2002_NorthAmerica) {
    # Check which requested Maurer weather data are available
    there <- sim_time[["overall_simstartyr"]] <= 2010 &&
      sim_time[["overall_endyr"]] >= 1949

    if (any(there)) {
      Maurer <- with(site_dat,
        create_filename_for_Maurer2002_NorthAmerica(X_WGS84, Y_WGS84))
      there <- vapply(Maurer, function(im) file.exists(file.path(path, im)),
        FUN.VALUE = NA)

      if (any(there)) {
        dw_source[there] <- "Maurer2002_NorthAmerica"
        dw_names[there] <- paste0(site_dat[there, "Label"], "_", Maurer[there])
      }
    }
  }

  list(source = dw_source, name = dw_names, n = sum(there))
}


dw_DayMet_NorthAmerica <- function(dw_source, dw_names, exinfo, site_dat,
  sim_time, path = NULL, MoreArgs = NULL) {

  if (!dir.exists(path))
    stop("'dw_DayMet_NorthAmerica': ", path, " does not exist.")

  there <- 0
  if (exinfo$GriddedDailyWeatherFromDayMet_NorthAmerica) {
    # Check which of the DayMet weather data are available
    #  - Temperature: 2-meter air temperature in Celsius degrees
    #  - Precipitation: mm/day; Daily total precipitation in millimeters per
    #   day, sum of all forms converted to water-equivalent. Precipitation
    #   occurrence on any given day may be ascertained.
    #  - Grids domain v2: -131.104 -52.95  52.00 14.53
    #  - Grids domain v3: -179     -52     83    14
    #  - Grids: Geographic Coordinate Reference: WGS_1984;
    #    Projection: Lambert Conformal Conic
    #  - Cells size: 1000 x 1000 m
    #  - All Daymet years, including leap years, have 1 - 365 days.
    #   For leap years, the Daymet database includes leap day. Values for
    #   December 31 are discarded from leap years to maintain a 365-day year.
    there <- sim_time[["overall_simstartyr"]] <= 1900 +
      as.POSIXlt(Sys.time(), tz = "UTC")$year - 1 &&
      sim_time[["overall_endyr"]] >= 1980

    if (any(there)) {
      there <- site_dat[, "X_WGS84"] >= -179 & site_dat[, "X_WGS84"] <= -5 &
        site_dat[, "Y_WGS84"] >= 14 & site_dat[, "Y_WGS84"] <= 83

      if (any(there)) {
        dw_source[there] <- "DayMet_NorthAmerica"
        dw_names[there] <- with(site_dat[there, ], paste0(Label, "_DayMet",
          formatC(X_WGS84, digits = 4, format = "f"), "_",
          formatC(Y_WGS84, digits = 4, format = "f")))
      }
    }
  }

  list(source = dw_source, name = dw_names, n = sum(there))
}


dw_NRCan_10km_Canada <- function(dw_source, dw_names, exinfo, site_dat,
  sim_time, path = NULL, MoreArgs = NULL) {

  if (!dir.exists(path))
    stop("'dw_NRCan_10km_Canada': ", path, " does not exist.")

  there <- 0
  if (exinfo$GriddedDailyWeatherFromNRCan_10km_Canada) {
    # Check which of the NRCan weather data are available
    #  - Temperature: Celsius degrees
    #  - Precipitation: mm
    #  - Grids domain: 141.00 to 52.00 W, 41.00 to 83.00 N
    #  - Grids datum: geographic NAD83
    #  - Columns: 1068, Rows: 510, Cells size: 0.083333333
    there <- sim_time[["overall_simstartyr"]] <= 2013 &&
      sim_time[["overall_endyr"]] >= 1950
    ftemp <- file.path(path, "1950", "max1950_1.asc")

    if (any(there) && file.exists(ftemp)) {
      nrc_test <- raster::raster(ftemp)
      # see http://spatialreference.org/ref/epsg/4269/
      raster::crs(nrc_test) <- raster::crs(paste("+init=epsg:4269",
        " +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"))
      sp_locs <- sp::SpatialPoints(coords = site_dat[, c("X_WGS84", "Y_WGS84")],
        proj4string = raster::crs(paste("+init=epsg:4326 +proj=longlat",
        "+ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")))
      temp <- sp::spTransform(sp_locs, CRSobj = raster::crs(nrc_test))
      temp <- raster::extract(nrc_test, y = temp)
      there <- !is.na(temp)

      if (any(there)) {
        dw_source[there] <- "NRCan_10km_Canada"
        dw_names[there] <- with(site_dat[there, ], paste0(Label, "_NRCan",
          formatC(X_WGS84, digits = 4, format = "f"), "_",
          formatC(Y_WGS84, digits = 4, format = "f")))
      }
    }
  }

  list(source = dw_source, name = dw_names, n = sum(there))
}

dw_Livneh2013_NorthAmerica <- function(dw_source, dw_names, exinfo, site_dat,
  sim_time, path = NULL, MoreArgs = NULL) {

  if (!dir.exists(path))
    stop("'dw_Livneh2013_NorthAmerica': ", path, " does not exist.")

  there <- 0

  if (exinfo$GriddedDailyWeatherFromLivneh2013_NorthAmerica) {
    # Check which requested Livneh2013 weather data are available
    there <- sim_time[["overall_simstartyr"]] <= 2011 &&
      sim_time[["overall_endyr"]] >= 1915
    ftemp <- file.path(path, "Meteorology_Livneh_CONUSExt_v.1.2_2013.191501.nc")

    if (any(there) && file.exists(ftemp)) {
      livneh_test <- raster::raster(ftemp, varname = "Prec")
      sp_locs <- sp::SpatialPoints(coords = site_dat[, c("X_WGS84", "Y_WGS84")],
        proj4string = sp::CRS(paste("+proj=longlat +ellps=WGS84 +datum=WGS84",
          "+no_defs +towgs84=0,0,0")))
      there <- !is.na(raster::extract(livneh_test, y = sp_locs))

      if (any(there)) {
        dw_source[there] <- "Livneh2013_NorthAmerica"
        dw_names[there] <- with(site_dat[there, ], paste0(Label, "_Livneh2013_",
          formatC(X_WGS84, digits = 5, format = "f"), "_",
          formatC(Y_WGS84, digits = 5, format = "f")))
      }
    }
  }

  list(source = dw_source, name = dw_names, n = sum(there))
}

dw_Livneh2016_NorthAmerica <- function(dw_source, dw_names, exinfo, site_dat,
  sim_time, path = NULL, MoreArgs = NULL) {

  if (!dir.exists(path))
    stop("'dw_Livneh2016_NorthAmerica': ", path, " does not exist.")

  there <- 0

  if (exinfo$GriddedDailyWeatherFromLivneh2016_NorthAmerica) {
    # Check which requested Livneh2016 weather data are available
    there <- sim_time[["overall_simstartyr"]] <= 2015 &&
      sim_time[["overall_endyr"]] >= 1915
    ftemp <- file.path(path, "livneh_NAmerExt_1Sep2016.191501.nc")

    if (any(there) && file.exists(ftemp)) {
      livneh_test <- raster::raster(ftemp, varname = "Prec")
      sp_locs <- sp::SpatialPoints(coords = site_dat[, c("X_WGS84", "Y_WGS84")],
        proj4string = sp::CRS(paste("+proj=longlat +ellps=WGS84 +datum=WGS84",
          "+no_defs +towgs84=0,0,0")))
      there <- !is.na(raster::extract(livneh_test, y = sp_locs))

      if (any(there)) {
        dw_source[there] <- "Livneh2016_NorthAmerica"
        dw_names[there] <- with(site_dat[there, ], paste0(Label, "_Livneh2016_",
          formatC(X_WGS84, digits = 5, format = "f"), "_",
          formatC(Y_WGS84, digits = 5, format = "f")))
      }
    }
  }

  list(source = dw_source, name = dw_names, n = sum(there))
}

dw_NCEPCFSR_Global <- function(dw_source, dw_names, exinfo, site_dat, sim_time,
  path = NULL, MoreArgs = NULL) {

  if (!dir.exists(path))
    stop("'dw_NCEPCFSR_Global': ", path, " does not exist.")

  there <- 0
  if (exinfo$GriddedDailyWeatherFromNCEPCFSR_Global) {
    # Check which of the NCEPCFSR_Global weather data are available
    #  - Grids domain: 0E to 359.688E and 89.761N to 89.761S
    there <- sim_time[["overall_simstartyr"]] <= 2010 &&
      sim_time[["overall_endyr"]] >= 1979

    if (any(there)) {
      temp <- cbind(site_dat[, "X_WGS84"] >= -180, site_dat[, "X_WGS84"] <= 180,
        site_dat[, "Y_WGS84"] >= -89.761, site_dat[, "Y_WGS84"] <= 89.761)
      there <- apply(temp, 1, all)

      if (any(there)) {
        dw_source[there] <- "NCEPCFSR_Global"
        dw_names[there] <- with(site_dat[there, ], paste0(Label, "_CFSR",
          formatC(X_WGS84, digits = 4, format = "f"), "_",
          formatC(Y_WGS84, digits = 4, format = "f")))
      }
    }
  }

  list(source = dw_source, name = dw_names, n = sum(there))
}

#' Determine sources of daily weather
#'
#' Determine order of priorities (highest priority comes last): i.e., the
#' last entry is the one that will be used.
#'
dw_determine_sources <- function(dw_source, exinfo, dw_avail_sources,
  SFSW2_prj_inputs, SWRunInformation, sim_size, sim_time, fnames_in,
  project_paths, verbose = FALSE) {

  if (verbose) {
    t1 <- Sys.time()
    temp_call <- shQuote(match.call()[1])
    print(paste0("rSFSW2's ", temp_call, ": started at ", t1))

    on.exit({
      print(paste0("rSFSW2's ", temp_call, ": ended after ",
      round(difftime(Sys.time(), t1, units = "secs"), 2), " s"))
      cat("\n")}, add = TRUE)
  }

  dw_names <- rep(NA, times = length(dw_source))
  dw_avail_sources2 <- rev(dw_avail_sources)
  fun_dw_source <- paste("dw", dw_avail_sources2, sep = "_")

  path_dw_source <- list(
    NRCan_10km_Canada = project_paths[["dir.ex.NRCan"]],
    Maurer2002_NorthAmerica = project_paths[["dir_maurer2002"]],
    LookupWeatherFolder =
      file.path(project_paths[["dir_in_treat"]], "LookupWeatherFolder"),
    NCEPCFSR_Global = project_paths[["dir.ex.NCEPCFSR"]],
    Livneh2013_NorthAmerica = project_paths[["dir.ex.Livneh2013"]],
    Livneh2016_NorthAmerica = project_paths[["dir.ex.Livneh2016"]],
    DayMet_NorthAmerica = project_paths[["dir_daymet"]])

  MoreArgs <- list(LookupWeatherFolder = list(
    create_treatments = SFSW2_prj_inputs[["create_treatments"]],
    runIDs_sites = sim_size[["runIDs_sites"]],
    ri_lwf = SWRunInformation[sim_size[["runIDs_sites"]], "WeatherFolder"],
    it_use = SFSW2_prj_inputs[["sw_input_treatments_use"]],
    ie_use = SFSW2_prj_inputs[["sw_input_experimentals_use"]],
    it_lwf = SFSW2_prj_inputs[["sw_input_treatments"]][
      sim_size[["runIDs_sites"]], "LookupWeatherFolder"],
    ie_lwf = SFSW2_prj_inputs[["sw_input_experimentals"]][,
      "LookupWeatherFolder"]))

  site_dat <- SWRunInformation[sim_size[["runIDs_sites"]],
    c("Label", "X_WGS84", "Y_WGS84")]

  for (k in seq_along(fun_dw_source)) {
    ftemp <- get(fun_dw_source[k])
    temp <- try(ftemp(dw_source, dw_names, exinfo, site_dat, sim_time,
      path = path_dw_source[[dw_avail_sources2[k]]],
      MoreArgs = MoreArgs[[dw_avail_sources2[k]]]), silent = TRUE)

    if (!inherits(temp, "try-error")) {
      dw_source <- temp[["source"]]
      dw_names <- temp[["name"]]

      if (verbose)
        print(paste("Data for", temp[["n"]], "sites will come from",
          shQuote(dw_avail_sources2[k])))

    } else {
      if (verbose)
        print(paste("Data source", shQuote(dw_avail_sources2[k]),
          "is not available."))
    }
  }

  # Save information on weather source to disk file
  dw_names <- gsub("[[:space:]]", "", dw_names)
  SWRunInformation[sim_size[["runIDs_sites"]][!is.na(dw_names)],
    "WeatherFolder"] <- stats::na.exclude(dw_names)
  SWRunInformation[sim_size[["runIDs_sites"]], "dailyweather_source"] <-
    as.character(dw_source)
  include_YN_dw <- rep(0L, dim(SWRunInformation)[1])
  include_YN_dw[sim_size[["runIDs_sites"]]][!is.na(dw_source)] <- 1L
  SWRunInformation[, "Include_YN_DailyWeather"] <- include_YN_dw

  utils::write.csv(SWRunInformation, file = fnames_in[["fmaster"]],
    row.names = FALSE)
  unlink(fnames_in[["fpreprocin"]])

  SWRunInformation
}


set_paths_to_dailyweather_datasources <- function(SFSW2_prj_meta) { # nolint

  dir_dW <- SFSW2_prj_meta[["project_paths"]][["dir_ex_weather"]]

  SFSW2_prj_meta[["project_paths"]][["dir_maurer2002"]] <- file.path(dir_dW,
      "Maurer+_2002updated", "DAILY_FORCINGS")

  SFSW2_prj_meta[["project_paths"]][["dir_daymet"]] <- file.path(dir_dW,
    "DayMet_NorthAmerica", "DownloadedSingleCells_FromDayMetv3_NorthAmerica")

  SFSW2_prj_meta[["project_paths"]][["dir.ex.NRCan"]] <- file.path(dir_dW,
    "NRCan_10km_Canada", "DAILY_GRIDS")

  SFSW2_prj_meta[["project_paths"]][["dir.ex.Livneh2013"]] <- file.path(dir_dW,
    "Livneh_NA_2013", "MONTHLY_GRIDS")

  SFSW2_prj_meta[["project_paths"]][["dir.ex.Livneh2016"]] <- file.path(dir_dW,
    "Livneh_NA_2016")

  SFSW2_prj_meta[["project_paths"]][["dir.ex.NCEPCFSR"]] <- file.path(dir_dW,
    "NCEPCFSR_Global", "CFSR_weather_prog08032012")

  SFSW2_prj_meta
}
