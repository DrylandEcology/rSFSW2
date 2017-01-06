#---------------------------------------------------------------------------------------#
#------EXTRACT ELEVATION------

prepare_ExtractData_Elevation <- function(SWRunInformation, runsN_sites, runIDs_sites,
  extract_determine_database, sim_cells_or_points, elev_probs = c(0.025, 0.5, 0.975)) {

  sites_elevation_source <- rep(NA, times = runsN_sites)
  has_cns_field <- "Elevation_source" %in% colnames(SWRunInformation)

  if (extract_determine_database == "SWRunInformation" && has_cns_field) {
    sites_elevation_source <- SWRunInformation$Elevation_source[runIDs_sites]
  } else if (extract_determine_database == "order" || !has_cns_field) {
  } else {
    message("Value of 'extract_determine_database'", extract_determine_database,
      " not implemented")
  }

  dtemp <- matrix(NA, nrow = runsN_sites, ncol = 1 + length(elev_probs),
    dimnames = list(NULL, c("ELEV_m", if (sim_cells_or_points == "cell")
    paste0("ELEV_m_q", elev_probs))))

  list(source = sites_elevation_source, data = dtemp, idone = vector(),
    probs = if (sim_cells_or_points == "cell") elev_probs else NULL,
    input = SWRunInformation)
}


update_elevation_input <- function(MMC, runIDs_sites, digits = 0, fmaster, fpreprocin) {
  icolnew <- !(colnames(MMC[["data"]]) %in% colnames(MMC[["input"]]))
  if (any(icolnew)) {
    MMC[["input"]] <- cbind(MMC[["input"]],
      matrix(NA, nrow = nrow(MMC[["input"]]), ncol = sum(icolnew),
        dimnames = list(NULL, colnames(MMC[["data"]])[icolnew])))
  }

  i_good <- complete.cases(MMC[["data"]])
  MMC[["input"]][runIDs_sites[i_good], colnames(MMC[["data"]])] <-
    round(MMC[["data"]][i_good, ], digits)

  write.csv(MMC[["input"]], file = fmaster, row.names = FALSE)
  unlink(fpreprocin)

  MMC
}


#' @references
do_ExtractElevation_NED_USA <- function(MMC, run_sites, runIDs_sites,
  sim_cells_or_points, sim_res, sim_crs, crs_sites, dir.ex.dem, fmaster, fpreprocin,
  continueAfterAbort, verbose) {

  stopifnot(require(raster), require(sp), require(rgdal))

  if (verbose)
    print(paste("Started 'ExtractElevation_NED_USA' at", Sys.time()))

  MMC[["idone"]]["NEDUSA1"] <- FALSE
  todos <- has_incompletedata(MMC[["data"]]) | is.na(MMC[["source"]]) |
    MMC[["source"]] == "Elevation_NED_USA"

  if (continueAfterAbort) {
    todos <- todos & has_nodata(MMC[["input"]][runIDs_sites, ], "ELEV_m")
  }
  names(todos) <- NULL
  n_extract <- sum(todos)

  if (n_extract > 0) {
    if (verbose)
      print(paste("'ExtractElevation_NED_USA' will be extracted for n =",
      n_extract, "sites"))

    dir.ex.ned <- file.path(dir.ex.dem, 'NED_USA', "NED_1arcsec")

    #read raster data
    g.elev <- raster::raster(file.path(dir.ex.ned, "ned_1s_westernUS_GeogrNAD83.tif"))
    crs_data <- raster::crs(g.elev)

    #locations of simulation runs
    sites_ned <- run_sites[todos, ]
    # Align with data crs
    if (!raster::compareCRS(crs_sites, crs_data)) {
      sites_ned <- sp::spTransform(sites_ned, CRS = crs_data)	#transform points to grid-coords
    }

    if (sim_cells_or_points == "point") {
      args_extract <- list(x = sites_ned)

    } else if (sim_cells_or_points == "cell") {
      cell_res_ned <- align_with_target_res(res_from = sim_res, crs_from = sim_crs,
        sp = run_sites[todos, ], crs_sp = crs_sites, crs_to = crs_data)
      args_extract <- list(x = cell_res_ned, coords = sites_ned, method = "block",
        probs = MMC[["probs"]])
    }

    #extract data for locations
    temp <- do.call("extract_from_external_raster", args = c(args_extract,
      data = list(g.elev)))	# elevation in m a.s.l.
    if (is.vector(temp)) {
      MMC[["data"]][todos, "ELEV_m"] <- temp

    } else if (is.array(temp)) {
      MMC[["data"]][todos, ] <- temp[, 1, ]

    } else {
      stop("Unknown object returned from 'extract_from_external_raster' when extracting",
        "elevation data.")
    }

    i_good <- complete.cases(MMC[["data"]][todos, ]) #length(i_good) == sum(todos)
    MMC[["source"]][which(todos)[!i_good]] <- NA

    if (any(i_good)) {
      MMC[["idone"]]["NEDUSA1"] <- TRUE
      i_Done <- rep(FALSE, times = runsN_sites) #length(i_Done) == length(runIDs_sites) == runsN_sites
      i_Done[which(todos)[i_good]] <- TRUE #sum(i_Done) == sum(i_good)
      MMC[["source"]][i_Done] <- "Elevation_NED_USA"
      if (!be.quiet)
        print(paste("'ExtractElevation_NED_USA' was extracted for n =", sum(i_good),
          "out of", n_extract, "sites"))
    }

    # Save extracted data to disk
    i_good <- todos & !has_incompletedata(MMC[["data"]]) #length(i_good) == length(todos) == runsN_sites
    i_notgood <- todos & has_incompletedata(MMC[["data"]]) #length(i_good) == length(todos) == runsN_sites
    MMC[["source"]][i_notgood] <- NA

    if (any(i_good)) {
      MMC[["idone"]]["NEDUSA1"] <- TRUE
      MMC[["source"]][i_good] <- "Elevation_NED_USA"
      if (verbose)
        print(paste("'ExtractElevation_NED_USA' was extracted for n =",
          sum(i_good), "out of", n_extract, "sites"))

      update_elevation_input(MMC, runIDs_sites, digits = 0, fmaster, fpreprocin)
    }
  }

  if (verbose)
    print(paste("Finished 'ExtractElevation_NED_USA' at", Sys.time()))

  MMC
}


#' @references
do_ExtractElevation_HWSD_Global <- function(MMC, run_sites, runIDs_sites,
  sim_cells_or_points, sim_res, sim_crs, crs_sites, dir.ex.dem, fmaster, fpreprocin,
  continueAfterAbort, verbose) {

  if (verbose)
    print(paste("Started 'ExtractElevation_HWSD_Global' at", Sys.time()))

  stopifnot(require(raster), require(sp), require(rgdal))

  MMC[["idone"]]["HWSD1"] <- FALSE
  todos <- has_incompletedata(MMC[["data"]]) | is.na(MMC[["source"]]) |
    MMC[["source"]] == "Elevation_HWSD_Global"

  if (continueAfterAbort) {
    todos <- todos & has_nodata(MMC[["input"]][runIDs_sites, ], "ELEV_m")
  }
  names(todos) <- NULL
  n_extract <- sum(todos)

  if (n_extract > 0) {
    if (verbose)
      print(paste("'ExtractElevation_HWSD_Global' will be extracted for n =",
        n_extract, "sites"))

    dir.ex.hwsd <- file.path(dir.ex.dem, "HWSD")

    #read raster data
    g.elev <- raster(file.path(dir.ex.hwsd, "GloElev_30as.asc"))
    crs_data <- raster::crs(g.elev)

    #locations of simulation runs
    sites_hwsd <- run_sites[todos, ]
    # Align with data crs
    if (!raster::compareCRS(crs_sites, crs_data)) {
      sites_hwsd <- sp::spTransform(sites_hwsd, CRS = crs_data)	#transform points to grid-coords
    }

    if (sim_cells_or_points == "point") {
      args_extract <- list(x = sites_hwsd)

    } else if (sim_cells_or_points == "cell") {
      cell_res_hwsd <- align_with_target_res(res_from = sim_res, crs_from = sim_crs,
        sp = run_sites[todos, ], crs_sp = crs_sites, crs_to = crs_data)
      args_extract <- list(x = cell_res_hwsd, coords = sites_hwsd, method = "block",
        probs = MMC[["probs"]])
    }

    #extract data for locations
    temp <- do.call("extract_from_external_raster", args = c(args_extract,
      data = list(g.elev)))	# elevation in m a.s.l.

    if (is.vector(temp)) {
      MMC[["data"]][todos, "ELEV_m"] <- temp

    } else if (is.array(temp)) {
      MMC[["data"]][todos, ] <- temp[, 1, ]

    } else {
      stop("Unknown object returned from 'extract_from_external_raster' when",
        "extracting elevation data.")
    }

    i_good <- complete.cases(MMC[["data"]][todos, ]) #length(i_good) == sum(todos)
    MMC[["source"]][which(todos)[!i_good]] <- NA

    if (any(i_good)) {
      MMC[["idone"]]["HWSD1"] <- TRUE
      i_Done <- rep(FALSE, times = runsN_sites) #length(i_Done) == length(runIDs_sites) == runsN_sites
      i_Done[which(todos)[i_good]] <- TRUE #sum(i_Done) == sum(i_good)

      MMC[["source"]][i_Done] <- "Elevation_HWSD_Global"
      if (!be.quiet)
        print(paste("'Elevation_HWSD_Global' was extracted for n =", sum(i_good),
          "out of", n_extract, "sites"))
    }

    if (any(i_good)) {
      MMC[["idone"]]["HWSD1"] <- TRUE
      MMC[["source"]][i_good] <- "Elevation_HWSD_Global"
      if (verbose)
        print(paste("'ExtractElevation_HWSD_Global' was extracted for n =",
          sum(i_good), "out of", n_extract, "sites"))

      update_elevation_input(MMC, runIDs_sites, digits = 0, fmaster, fpreprocin)
    }
  }

  if (verbose)
    print(paste("Finished 'ExtractElevation_HWSD_Global' at", Sys.time()))

  MMC
}

update_Elevation_sources <- function(MMC, runIDs_sites, runsN_master, fmaster, fpreprocin) {
  notDone <- NULL

  if (any(MMC[["idone"]])) {
    #write data to datafile.SWRunInformation
    MMC[["input"]]$Elevation_source[runIDs_sites] <- as.character(MMC[["source"]])

    notDone <- is.na(MMC[["source"]])
    include_YN_elev <- rep(0, runsN_master)
    include_YN_elev[runIDs_sites[!notDone]] <- 1
    MMC[["input"]]$Include_YN_ElevationSources <- include_YN_elev

    write.csv(MMC[["input"]], file = fmaster, row.names = FALSE)
    unlink(fpreprocin)

    if (any(notDone))
      print(paste("Elevation data weren't found for", sum(notDone), "sites"))

  } else {
      print("'ExtractElevation': no data extracted because already available")
  }

  MMC[["input"]]
}

#' @export
ExtractData_Elevation <- function(SWRunInformation, runsN_master, runsN_sites,
  runIDs_sites, run_sites, extract_determine_database, sim_cells_or_points, sim_res,
  sim_crs, crs_sites, dir.ex.dem, fmaster, fpreprocin, continueAfterAbort, be.quiet) {

  MMC <- prepare_ExtractData_Elevation(SWRunInformation, runsN_sites, runIDs_sites,
    extract_determine_database, sim_cells_or_points)

  if (exinfo$ExtractElevation_NED_USA) {
    MMC <- do_ExtractElevation_NED_USA(MMC, run_sites, runIDs_sites,
      sim_cells_or_points, sim_res, sim_crs, crs_sites, dir.ex.dem, fmaster, fpreprocin,
      continueAfterAbort, verbose = !be.quiet)
  }

  if (exinfo$ExtractElevation_HWSD_Global) {
    MMC <- do_ExtractElevation_HWSD_Global(MMC, run_sites, runIDs_sites,
      sim_cells_or_points, sim_res, sim_crs, crs_sites, dir.ex.dem, fmaster, fpreprocin,
      continueAfterAbort, verbose = !be.quiet)
  }

  # returns 'SWRunInformation'
  temp <- update_Elevation_sources(MMC, runIDs_sites, runsN_master, fmaster, fpreprocin)

  list(SWRunInformation = temp)
}

#----------------------------------------------------------------------------------------#
