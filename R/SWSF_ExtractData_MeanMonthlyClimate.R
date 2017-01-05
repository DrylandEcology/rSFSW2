
prepare_ExtractData_MeanMonthlyClimate <- function(SWRunInformation, runIDs_sites,
  extract_determine_database, sw_input_cloud_use, sw_input_cloud) {

  sites_monthlyclim_source <- rep(NA, times = runsN_sites)
  has_cns_field <- "ClimateNormals_source" %in% colnames(SWRunInformation)

  if (extract_determine_database == "SWRunInformation" && has_cns_field) {
    sites_monthlyclim_source <- SWRunInformation$ClimateNormals_source[runIDs_sites]
  } else if (extract_determine_database == "order" || !has_cns_field) {
  } else {
    message("Value of 'extract_determine_database'", extract_determine_database,
      " not implemented"))
  }

  temp <- rep(FALSE, times = runsN_sites)
  dtemp <- array(NA, dim = c(runsN_sites, 3, 12), dimnames = list(NULL,
      c("RH", "cover", "wind"), NULL))

  list(source = sites_monthlyclim_source, data = dtemp,
    itodo = list(NCDC1 = temp, NCEPCFSR1 = temp),
    idone = c(NCDC1 = temp, NCEPCFSR1 = temp),
    use = sw_input_cloud_use, input = sw_input_cloud)
}


copy_MMCdata_toInput <- function(MCC, runIDs_sites, digits = 2, st_mo, fcloud, fpreprocin) {
  #add data to MMC[["input"]] and set the use flags
  i.temp <- grep("RH", names(MMC[["use"]]))
  MMC[["use"]][i.temp] <- TRUE
  MMC[["input"]][runIDs_sites[i_good], i.temp][, st_mo] <- round(MMC[["data"]][i_good, "RH", ], digits)
  i.temp <- grep("SkyC", names(MMC[["use"]]))
  MMC[["use"]][i.temp] <- TRUE
  MMC[["input"]][runIDs_sites[i_good], i.temp][, st_mo] <- round(MMC[["data"]][i_good, "cover", ], digits)
  i.temp <- grep("wind", names(MMC[["use"]]))
  MMC[["use"]][i.temp] <- TRUE
  MMC[["input"]][runIDs_sites[i_good], i.temp][, st_mo] <- round(MMC[["data"]][i_good, "wind", ], digits)

  #write data to datafile.cloud
  write.csv(reconstitute_inputfile(MMC[["use"]], MMC[["input"]]), file = fcloud,
    row.names = FALSE)
  unlink(fpreprocin)

  MMC
}


#' @references National Climatic Data Center. 2005. Climate maps of the United States.
#'  Available online http://cdo.ncdc.noaa.gov/cgi-bin/climaps/climaps.pl. Last accessed
#'  May 2010.
do_ExtractSkyDataFromNOAAClimateAtlas_USA <- function(MMC, run_sites, runIDs_sites,
  st_mo, sim_cells_or_points, sim_res, sim_crs, crs_sites, dir.ex.weather, dir.out.temp,
  fcloud, fpreprocin, chunk_size.options,
  continueAfterAbort, verbose) {

  stopifnot(require(raster), require(sp), require(rgdal))

  if (verbose)
    print(paste("Started 'ExtractSkyDataFromNOAAClimateAtlas_USA' at", Sys.time()))

  todos <- MMC[["itodo"]][["NCDC1"]] | has_incompletedata(MMC[["data"]]) |
    is.na(MMC[["source"]]) | MMC[["source"]] == "ClimateNormals_NCDC2005_USA"

  if (continueAfterAbort) {
    todos <- todos & (
      has_nodata(MMC[["input"]][runIDs_sites, ], "RH") |
      has_nodata(MMC[["input"]][runIDs_sites, ], "SkyC") |
      has_nodata(MMC[["input"]][runIDs_sites, ], "wind"))
  }
  names(todos) <- NULL

  i_extract <- as.integer(which(todos))
  n_extract <- sum(todos)

  if (n_extract > 0) {
    if (verbose)
      print(paste("'ExtractSkyDataFromNOAAClimateAtlas_USA' will be extracted for n =",
      n_extract, "sites"))

    #NOAA Climate Atlas: provides no information on height above ground: assuming 2-m which is what is required by SoilWat
    dir.ex.dat <- file.path(dir.ex.weather, "ClimateAtlasUS")
    stopifnot(file.exists(dir.ex.dat))

    dir_noaaca <- list(
      RH = file.path(dir.ex.dat, "HumidityRelative_Percent"),
      cover = file.path(dir.ex.dat, "Sunshine_Percent"),
      # cover = file.path(dir.ex.dat, "SkyCoverDay_Percent"),
      wind = file.path(dir.ex.dat, "WindSpeed_mph"))

    files_shp <- list(
      RH = paste0("RH23", formatC(st_mo, width=2,format="d", flag="0")),
      cover = paste0("SUN52", formatC(st_mo, width=2,format="d", flag="0")),
      # cover = paste0("SKYC50", formatC(st_mo, width=2,format="d", flag="0")),
      wind = paste0("WND60B", formatC(st_mo, width=2,format="d", flag="0")))

    var_codes <- list(
      RH = c(10, 23, 31, 41, 51, 61, 71, 78, 90), #percent
      cover = c(11, 26, 36, 46, 56, 66, 76, 86, 96),	#percent
      # cover = c(11, 23, 31, 41, 51, 61, 71, 81, 93),	#percent
      wind = c(1.3, 2.9, 3.3, 3.8, 4.2, 4.7, 5.1, 5.6, 9.6))	#m/s; the last category is actually open '> 12.9 mph': I closed it arbitrarily with 30 mph
    stopifnot(colnames(MMC[["data"]]) == names(dir_noaaca),
      colnames(MMC[["data"]]) == names(files_shp),
      colnames(MMC[["data"]]) == names(var_codes))

    #locations of simulation runs
    sites_noaaca <- run_sites[todos, ]
    # Align with data crs
    noaaca <- rgdal::readOGR(dsn = dir_noaaca[["RH"]], layer = files_shp[["RH"]][1], verbose = FALSE)
    crs_data <- raster::crs(noaaca)
    if (!raster::compareCRS(crs_sites, crs_data)) {
      sites_noaaca <- sp::spTransform(sites_noaaca, CRS = crs_data)	#transform points to grid-coords
    }

    if (sim_cells_or_points == "point") {
      args_extract <- list(x = sites_noaaca)

    } else if (sim_cells_or_points == "cell") {
      cell_res_noaaca <- align_with_target_res(res_from = sim_res, crs_from = sim_crs,
        sp = sites_noaaca, crs_sp = crs_sites, crs_to = crs_data)
      args_extract <- list(x = cell_res_noaaca, coords = sites_noaaca, crs_data = crs_data)
    }

    # determine NOAA CA extractions to do
    do_chunks <- parallel::splitIndices(n_extract,
      ceiling(n_extract / chunk_size.options[["ExtractSkyDataFromNOAAClimateAtlas_USA"]]))

    n_vars <- ncol(MMC[["data"]])
    n_months <- 12L
    n_chunks <- length(do_chunks)
    iv <- m <- ic <- 1

    # determine start location based on interrupted data extraction
    ftemp_noaaca <- file.path(dir.out.temp, "NOAA_ClimateAtlas_extraction.rds")

    if (continueAfterAbort && file.exists(ftemp_noaaca)) {
      prev_noaaca <- readRDS(ftemp_noaaca)

      if (identical(todos, prev_noaaca[["do_extract"]])) { # only continue if same extractions
        MMC[["data"]][todos, , ] <- prev_noaaca[["monthlyclim"]][todos, , ]

        iv <- prev_noaaca[["iv"]]
        m <- prev_noaaca[["m"]]
        if (identical(do_chunks, prev_noaaca[["do_chunks"]])) {
          ic <- prev_noaaca[["ic"]]
        } else {
          itemp <- max(unlist(prev_noaaca[["do_chunks"]][seq_len(prev_noaaca[["ic"]])]))
          cnewmaxs <- sapply(do_chunks, function(x) max(x))
          ic <- findInterval(itemp, c(0, cnewmaxs))
        }
      }
    }

    #extract data for locations
    if (iv < n_vars ||
      (iv == n_vars && m < n_months) ||
      (iv == n_vars && m == n_months && ic < n_chunks)) repeat {

      if (verbose)
        print(paste0(Sys.time(), ": 'ExtractSkyDataFromNOAAClimateAtlas_USA' extracting for: ",
          paste(names(dir_noaaca)[iv], month.name[m], paste("chunk", ic, "of",
          n_chunks), sep = ", ")))

      iextr <- i_extract[do_chunks[[ic]]]
      args_chunk <- args_extract
      args_chunk[["x"]] <- args_chunk[["x"]][do_chunks[[ic]], ]
      if (!is.null(args_chunk[["coords"]]))
        args_chunk[["coords"]] <- args_chunk[["coords"]][do_chunks[[ic]], ]

      MMC[["data"]][iextr, iv, m] <- do.call("extract_from_external_shapefile",
        args = c(args_chunk, file_path = list(dir_noaaca[[iv]]),
        file_shp = list(files_shp[[iv]][m]), fields = list("GRIDCODE"),
        code = list(var_codes[[iv]])))

      if (ic < n_chunks) {
        ic <- ic + 1
      } else {
        ic <- 1
        m <- m + 1
      }
      if (m > n_months) {
        m <- 1
        iv <- iv + 1
      }

      if (continueAfterAbort)
        saveRDS(list(do_extract = todos, monthlyclim = MMC[["data"]],
          do_chunks = do_chunks, iv = iv, m = m, ic = ic), file = ftemp_noaaca)

      if (iv > n_vars) break
    }

    #subtract from 100% as we want cover and not no-cover
    MMC[["data"]][todos, "cover", ] <- 100 - MMC[["data"]][todos, "cover", ]


    # Save extracted data to disk
    i_good <- todos & !has_incompletedata(MMC[["data"]]) #length(i_good) == length(todos) == runsN_sites
    i_notgood <- todos & has_incompletedata(MMC[["data"]]) #length(i_good) == length(todos) == runsN_sites
    MMC[["source"]][i_notgood] <- NA

    if (any(i_good)) {
      MMC[["idone"]]["NCDC1"] <- TRUE
      MMC[["source"]][i_good] <- "ClimateNormals_NCDC2005_USA"
      if (verbose)
        print(paste("'ExtractSkyDataFromNOAAClimateAtlas_USA' was extracted for n =",
          sum(i_good), "out of", n_extract, "sites"))

      copy_MMCdata_toInput(MCC, runIDs_sites, digits = 2, st_mo, fcloud, fpreprocin)
    }
  }

  MMC[["itodo"]][["NCDC1"]] <- todos

  if (verbose)
    print(paste("Finished 'ExtractSkyDataFromNOAAClimateAtlas_USA' at", Sys.time()))

  MMC
}


#' @references Environmental Modeling Center/National Centers for Environmental
#'  Prediction/National Weather Service/NOAA/U.S. Department of Commerce. 2010. NCEP
#'  Climate Forecast System Reanalysis (CFSR) Monthly Products, January 1979 to December
#'  2010. Research Data Archive at the National Center for Atmospheric Research,
#'  Computational and Information Systems Laboratory.
#'  http://rda.ucar.edu/datasets/ds093.2/. Accessed 8 March 2012.
do_ExtractSkyDataFromNCEPCFSR_Global <- function(MMC, SWRunInformation, runIDs_sites,
  st_mo, prepd_CFSR, startyr, endyr, dir.out.temp, fcloud, fpreprocin, do_parallel,
  parallel_backend, cl, chunk_size.options, continueAfterAbort, verbose) {

  if (verbose)
    print(paste("Started 'ExtractSkyDataFromNCEPCFSR_Global' at", Sys.time()))

  todos <- MMC[["itodo"]][["NCEPCFSR1"]] | has_incompletedata(MMC[["data"]]) |
    is.na(MMC[["source"]]) | MMC[["source"]] == "ClimateNormals_NCEPCFSR_Global"

  if (continueAfterAbort) {
    todos <- todos & (
      has_nodata(MMC[["input"]][runIDs_sites, ], "RH") |
      has_nodata(MMC[["input"]][runIDs_sites, ], "SkyC") |
      has_nodata(MMC[["input"]][runIDs_sites, ], "wind"))
  }
  names(todos) <- NULL

  if (any(todos)) {
    if (verbose)
      print(paste("'ExtractSkyDataFromNCEPCFSR_Global' will be extracted for n =",
        sum(todos), "sites"))

    #locations of simulation runs
    locations <- SWRunInformation[runIDs_sites[todos], c("WeatherFolder", "X_WGS84", "Y_WGS84")]
    # do the extractions
    temp <- try(get_NCEPCFSR_data(dat_sites = locations, daily = FALSE, monthly = TRUE,
      yearLow = startyr, yearHigh = endyr, dir.in.cfsr = prepd_CFSR$dir.in.cfsr,
      dir_temp = dir.out.temp, cfsr_so = prepd_CFSR$cfsr_so,
      n_site_per_core = chunk_size.options[["ExtractSkyDataFromNCEPCFSR_Global"]],
      do_parallel = do_parallel, parallel_backend = parallel_backend, cl = cl,
      rm_mc_files = TRUE, continueAfterAbort = continueAfterAbort))

    if (inherits(temp, "try-error"))
      stop(temp)

    #match weather folder names in case of missing extractions
    res <- as.matrix(temp[["res_clim"]][, -1])
    irow <- match(locations[, "WeatherFolder"],
      table = temp[["res_clim"]][, "WeatherFolder"], nomatch = 0)
    irowL <- irow > 0
    MMC[["data"]][todos, "RH", ][irowL, ] <- res[irow, grepl("RH", colnames(res))]
    MMC[["data"]][todos, "cover", ][irowL, ] <- res[irow, grepl("Cloud", colnames(res))]
    MMC[["data"]][todos, "wind", ][irowL, ] <- res[irow, grepl("Wind", colnames(res))]

    # Save extracted data to disk
    i_good <- todos & !has_incompletedata(MMC[["data"]]) #length(i_good) == sum(todos) == runsN_sites
    i_notgood <- todos & has_incompletedata(MMC[["data"]]) #length(i_good) == sum(todos) == runsN_sites
    MMC[["source"]][i_notgood] <- NA

    if (any(i_good)) {
      MMC[["idone"]]["NCEPCFSR1"] <- TRUE
      MMC[["source"]][i_good] <- "ClimateNormals_NCEPCFSR_Global"
      if (verbose)
        print(paste("'ExtractSkyDataFromNCEPCFSR_Global' was extracted for n =",
          sum(i_good), "out of", sum(todos), "sites"))

      copy_MMCdata_toInput(MCC, runIDs_sites, digits = 2, st_mo, fcloud, fpreprocin)
    }
  }

  MMC[["itodo"]][["NCEPCFSR1"]] <- todos

  if (verbose)
    print(paste("Finished 'ExtractSkyDataFromNCEPCFSR_Global' at", Sys.time()))

  MMC
}

update_MeanMonthlyClimate_sources <- function(MMC, SWRunInformation, runIDs_sites,
  runsN_master, fmaster, fpreprocin) {

  notDone <- NULL

  if (any(MMC[["idone"]])) {
    #write data to datafile.SWRunInformation
    SWRunInformation$ClimateNormals_source[runIDs_sites] <- as.character(MMC[["source"]])

    notDone <- is.na(MMC[["source"]])
    include_YN_climnorm <- rep(0, runsN_master)
    include_YN_climnorm[runIDs_sites[!notDone]] <- 1
    SWRunInformation$Include_YN_ClimateNormalSources <- include_YN_climnorm

    write.csv(SWRunInformation, file = fmaster, row.names = FALSE)
    unlink(fpreprocin)

    if (any(notDone))
      print(paste("Climate normals weren't found for", sum(notDone), "sites"))

  } else {
      print("'ExtractClimateNormals': no data extracted because already available")
  }

  invisible(notDone)
}

#' @export
ExtractData_MeanMonthlyClimate <- function(SWRunInformation, runsN_master, runIDs_sites,
  run_sites, sw_input_cloud_use, sw_input_cloud, st_mo, extract_determine_database,
  sim_cells_or_points, sim_res, sim_crs, crs_sites, dir.ex.weather, dir.out.temp,
  fmaster, fcloud, fpreprocin, chunk_size.options, continueAfterAbort, be.quiet,
  prepd_CFSR, startyr, endyr, parallel_runs, parallel_init, parallel_backend, cl) {

  MMC <- prepare_ExtractData_MeanMonthlyClimate(SWRunInformation, runIDs_sites,
    extract_determine_database, sw_input_cloud_use, sw_input_cloud)

  if (exinfo$ExtractSkyDataFromNOAAClimateAtlas_USA) {
    MMC <- do_ExtractSkyDataFromNOAAClimateAtlas_USA(MMC, run_sites, runIDs_sites,
      st_mo, sim_cells_or_points, sim_res, sim_crs, crs_sites, dir.ex.weather,
      dir.out.temp, fcloud, fpreprocin, chunk_size.options, continueAfterAbort,
      verbose = !be.quiet)
  }

  if (exinfo$ExtractSkyDataFromNCEPCFSR_Global) {
    MMC <- do_ExtractSkyDataFromNCEPCFSR_Global(MMC, SWRunInformation, runIDs_sites,
      st_mo, prepd_CFSR, startyr, endyr, dir.out.temp, fcloud, fpreprocin,
      do_parallel = parallel_runs && parallel_init, parallel_backend, cl,
      chunk_size.options, continueAfterAbort, verbose = !be.quiet)
  }

  update_MeanMonthlyClimate_sources(MMC, SWRunInformation, runIDs_sites, runsN_master,
    fmaster, fpreprocin)
}

#--------------------------------------------------------------------------------------------------#
