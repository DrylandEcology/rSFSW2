#---------------------------------------------------------------------------------------#
#------EXTRACT ELEVATION------

prepare_ExtractData_Elevation <- function(SWRunInformation, sim_size,
  how_determine_sources, scorp, elev_probs = c(0.025, 0.5, 0.975)) {

  sites_elevation_source <- rep(NA, times = sim_size[["runsN_sites"]])
  has_cns_field <- "Elevation_source" %in% colnames(SWRunInformation)

  if (how_determine_sources == "SWRunInformation" && has_cns_field) {
    sites_elevation_source <- SWRunInformation$Elevation_source[sim_size[["runIDs_sites"]]]
  } else if (how_determine_sources == "order" || !has_cns_field) {
  } else {
    message("Value of 'how_determine_sources'", how_determine_sources,
      " not implemented")
  }

  dtemp <- matrix(NA, nrow = sim_size[["runsN_sites"]], ncol = 1 + length(elev_probs),
    dimnames = list(NULL, c("ELEV_m", if (scorp == "cell")
    paste0("ELEV_m_q", elev_probs))))

  list(source = sites_elevation_source, data = dtemp, idone = vector(),
    probs = if (scorp == "cell") elev_probs else NULL,
    input = SWRunInformation)
}


update_elevation_input <- function(MMC, sim_size, digits = 0, fnames_in) {
  icolnew <- !(colnames(MMC[["data"]]) %in% colnames(MMC[["input"]]))
  if (any(icolnew)) {
    MMC[["input"]] <- cbind(MMC[["input"]],
      matrix(NA, nrow = nrow(MMC[["input"]]), ncol = sum(icolnew),
        dimnames = list(NULL, colnames(MMC[["data"]])[icolnew])))
  }

  i_good <- stats::complete.cases(MMC[["data"]])
  MMC[["input"]][sim_size[["runIDs_sites"]][i_good], colnames(MMC[["data"]])] <-
    round(MMC[["data"]][i_good, ], digits)

  utils::write.csv(MMC[["input"]], file = fnames_in[["fmaster"]], row.names = FALSE)
  unlink(fnames_in[["fpreprocin"]])

  MMC
}


#' @references National Elevation Dataset (ned.usgs.gov)
do_ExtractElevation_NED_USA <- function(MMC, sim_size, sim_space, dir_ex_dem, fnames_in,
  resume, verbose) {

  stopifnot(requireNamespace("raster"), requireNamespace("sp"), requireNamespace("rgdal"))

  if (verbose)
    print(paste("Started 'ExtractElevation_NED_USA' at", Sys.time()))

  MMC[["idone"]]["NEDUSA1"] <- FALSE
  todos <- has_incompletedata(MMC[["data"]]) | is.na(MMC[["source"]]) |
    MMC[["source"]] == "Elevation_NED_USA"

  if (resume) {
    todos <- todos & has_nodata(MMC[["input"]][sim_size[["runIDs_sites"]], ], "ELEV_m")
  }
  names(todos) <- NULL
  n_extract <- sum(todos)

  if (n_extract > 0) {
    if (verbose)
      print(paste("'ExtractElevation_NED_USA' will be extracted for n =",
      n_extract, "sites"))

    dir.ex.ned <- file.path(dir_ex_dem, 'NED_USA', "NED_1arcsec")

    #read raster data
    g.elev <- raster::raster(file.path(dir.ex.ned, "ned_1s_westernUS_GeogrNAD83.tif"))
    crs_data <- raster::crs(g.elev)

    #locations of simulation runs
    sites_ned <- sim_space[["run_sites"]][todos, ]
    # Align with data crs
    if (!raster::compareCRS(sim_space[["crs_sites"]], crs_data)) {
      sites_ned <- sp::spTransform(sites_ned, CRS = crs_data)	#transform graphics::points to grid-coords
    }

    if (sim_space[["scorp"]] == "point") {
      args_extract <- list(y = sites_ned, type = sim_space[["scorp"]])

    } else if (sim_space[["scorp"]] == "cell") {
      cell_res_ned <- align_with_target_res(res_from = sim_space[["sim_res"]],
        crs_from = sim_space[["sim_crs"]], sp = sim_space[["run_sites"]][todos, ],
        crs_sp = sim_space[["crs_sites"]], crs_to = crs_data)
      args_extract <- list(y = cell_res_ned, coords = sites_ned, method = "block",
        probs = MMC[["probs"]], type = sim_space[["scorp"]])
    }

    #extract data for locations
    temp <- do.call("extract_swsf", args = c(args_extract, x = list(g.elev)))	# elevation in m a.s.l.
    if (is.vector(temp)) {
      MMC[["data"]][todos, "ELEV_m"] <- temp

    } else if (is.array(temp)) {
      MMC[["data"]][todos, ] <- temp[, 1, ]

    } else {
      stop("Unknown object returned from 'extract_swsf' when extracting",
        "elevation data.")
    }

    i_good <- stats::complete.cases(MMC[["data"]][todos, ]) #length(i_good) == sum(todos)
    MMC[["source"]][which(todos)[!i_good]] <- NA

    if (any(i_good)) {
      MMC[["idone"]]["NEDUSA1"] <- TRUE
      i_Done <- rep(FALSE, times = sim_size[["runsN_sites"]]) #length(i_Done) == length(runIDs_sites) == runsN_sites
      i_Done[which(todos)[i_good]] <- TRUE #sum(i_Done) == sum(i_good)
      MMC[["source"]][i_Done] <- "Elevation_NED_USA"
      if (verbose)
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

      update_elevation_input(MMC, sim_size, digits = 0, fnames_in)
    }
  }

  if (verbose)
    print(paste("Finished 'ExtractElevation_NED_USA' at", Sys.time()))

  MMC
}


#' @references Harmonized World Soil Database
do_ExtractElevation_HWSD_Global <- function(MMC, sim_size, sim_space, dir_ex_dem,
  fnames_in, resume, verbose) {

  if (verbose)
    print(paste("Started 'ExtractElevation_HWSD_Global' at", Sys.time()))

  stopifnot(requireNamespace("raster"), requireNamespace("sp"), requireNamespace("rgdal"))

  MMC[["idone"]]["HWSD1"] <- FALSE
  todos <- has_incompletedata(MMC[["data"]]) | is.na(MMC[["source"]]) |
    MMC[["source"]] == "Elevation_HWSD_Global"

  if (resume) {
    todos <- todos & has_nodata(MMC[["input"]][sim_size[["runIDs_sites"]], ], "ELEV_m")
  }
  names(todos) <- NULL
  n_extract <- sum(todos)

  if (n_extract > 0) {
    if (verbose)
      print(paste("'ExtractElevation_HWSD_Global' will be extracted for n =",
        n_extract, "sites"))

    dir.ex.hwsd <- file.path(dir_ex_dem, "HWSD")

    #read raster data
    g.elev <- raster::raster(file.path(dir.ex.hwsd, "GloElev_30as.asc"))
    crs_data <- raster::crs(g.elev)

    #locations of simulation runs
    sites_hwsd <- sim_space[["run_sites"]][todos, ]
    # Align with data crs
    if (!raster::compareCRS(sim_space[["crs_sites"]], crs_data)) {
      sites_hwsd <- sp::spTransform(sites_hwsd, CRS = crs_data)	#transform graphics::points to grid-coords
    }

    if (sim_space[["scorp"]] == "point") {
      args_extract <- list(y = sites_hwsd, type = sim_space[["scorp"]])

    } else if (sim_space[["scorp"]] == "cell") {
      cell_res_hwsd <- align_with_target_res(res_from = sim_space[["sim_res"]],
        crs_from = sim_space[["sim_crs"]], sp = sim_space[["run_sites"]][todos, ],
        crs_sp = sim_space[["crs_sites"]], crs_to = crs_data)
      args_extract <- list(y = cell_res_hwsd, coords = sites_hwsd, method = "block",
        probs = MMC[["probs"]], type = sim_space[["scorp"]])
    }

    #extract data for locations
    temp <- do.call("extract_swsf", args = c(args_extract, x = list(g.elev)))	# elevation in m a.s.l.

    if (is.vector(temp)) {
      MMC[["data"]][todos, "ELEV_m"] <- temp

    } else if (is.array(temp)) {
      MMC[["data"]][todos, ] <- temp[, 1, ]

    } else {
      stop("Unknown object returned from 'extract_swsf' when",
        "extracting elevation data.")
    }

    i_good <- stats::complete.cases(MMC[["data"]][todos, ]) #length(i_good) == sum(todos)
    MMC[["source"]][which(todos)[!i_good]] <- NA

    if (any(i_good)) {
      MMC[["idone"]]["HWSD1"] <- TRUE
      i_Done <- rep(FALSE, times = sim_size[["runsN_sites"]]) #length(i_Done) == length(runIDs_sites) == runsN_sites
      i_Done[which(todos)[i_good]] <- TRUE #sum(i_Done) == sum(i_good)

      MMC[["source"]][i_Done] <- "Elevation_HWSD_Global"
      if (verbose)
        print(paste("'Elevation_HWSD_Global' was extracted for n =", sum(i_good),
          "out of", n_extract, "sites"))
    }

    if (any(i_good)) {
      MMC[["idone"]]["HWSD1"] <- TRUE
      MMC[["source"]][i_good] <- "Elevation_HWSD_Global"
      if (verbose)
        print(paste("'ExtractElevation_HWSD_Global' was extracted for n =",
          sum(i_good), "out of", n_extract, "sites"))

      update_elevation_input(MMC, sim_size, digits = 0, fnames_in)
    }
  }

  if (verbose)
    print(paste("Finished 'ExtractElevation_HWSD_Global' at", Sys.time()))

  MMC
}

update_Elevation_sources <- function(MMC, sim_size, fnames_in) {
  notDone <- NULL

  if (any(MMC[["idone"]])) {
    #write data to disk
    MMC[["input"]]$Elevation_source[sim_size[["runIDs_sites"]]] <- as.character(MMC[["source"]])

    notDone <- is.na(MMC[["source"]])
    include_YN_elev <- rep(0, sim_size[["runsN_master"]])
    include_YN_elev[sim_size[["runIDs_sites"]][!notDone]] <- 1
    MMC[["input"]]$Include_YN_ElevationSources <- include_YN_elev

    utils::write.csv(MMC[["input"]], file = fnames_in[["fmaster"]], row.names = FALSE)
    unlink(fnames_in[["fpreprocin"]])

    if (any(notDone))
      print(paste("Elevation data weren't found for", sum(notDone), "sites"))

  } else {
      print("'ExtractElevation': no data extracted because already available")
  }

  MMC[["input"]]
}

#' @export
ExtractData_Elevation <- function(exinfo, SWRunInformation, sim_size,
  how_determine_sources, sim_space, dir_ex_dem, fnames_in, resume, verbose) {

  MMC <- prepare_ExtractData_Elevation(SWRunInformation, sim_size, how_determine_sources,
    sim_space[["scorp"]])

  if (exinfo$ExtractElevation_NED_USA) {
    MMC <- do_ExtractElevation_NED_USA(MMC, sim_size, sim_space, dir_ex_dem, fnames_in,
      resume, verbose = verbose)
  }

  if (exinfo$ExtractElevation_HWSD_Global) {
    MMC <- do_ExtractElevation_HWSD_Global(MMC, sim_size, sim_space, dir_ex_dem,
      fnames_in, resume, verbose = verbose)
  }

  # returns 'SWRunInformation'
  temp <- update_Elevation_sources(MMC, sim_size, fnames_in)

  list(SWRunInformation = temp)
}

#----------------------------------------------------------------------------------------#
