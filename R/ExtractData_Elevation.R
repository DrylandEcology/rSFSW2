#---------------------------------------------------------------------------------------#
#------EXTRACT ELEVATION------

prepare_ExtractData_Elevation <- function(SWRunInformation, sim_size, field_sources,
  field_include, how_determine_sources, scorp, elev_probs = c(0.025, 0.5, 0.975)) {

  sites_elevation_source <- get_datasource_masterfield(SWRunInformation,
    field_sources, sim_size, how_determine_sources)

  probs <- if (scorp == "cell") elev_probs else NULL

  dtemp <- matrix(NA, nrow = sim_size[["runsN_sites"]], ncol = 1 + length(probs),
    dimnames = list(NULL, c("ELEV_m", if (scorp == "cell") paste0("ELEV_m_q", probs))))

  do_include <- if (field_include %in% names(SWRunInformation)) {
      SWRunInformation[sim_size[["runIDs_sites"]], field_include] > 0
    } else {
      rep(TRUE, sim_size[["runsN_sites"]])

  list(source = sites_elevation_source, data = dtemp, idone = vector(),
    probs = probs, input = SWRunInformation, do_include = do_include)
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

  if (verbose) {
    t1 <- Sys.time()
    temp_call <- shQuote(match.call()[1])
    print(paste0("rSFSW2's ", temp_call, ": started at ", t1))

    on.exit({print(paste0("rSFSW2's ", temp_call, ": ended after ",
      round(difftime(Sys.time(), t1, units = "secs"), 2), " s")); cat("\n")}, add = TRUE)
  }

  stopifnot(requireNamespace("raster"), requireNamespace("sp"), requireNamespace("rgdal"))

  MMC[["idone"]]["NEDUSA1"] <- FALSE
  todos <- has_incompletedata(MMC[["data"]]) | is.na(MMC[["source"]]) |
    MMC[["source"]] == "Elevation_NED_USA"

  if (resume) {
    todos <- todos & has_nodata(MMC[["input"]][sim_size[["runIDs_sites"]], ], "ELEV_m") &
      MMC[["do_include"]]
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
      sites_ned <- sp::spTransform(sites_ned, CRS = crs_data)  #transform graphics::points to grid-coords
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
    temp <- do.call("extract_rSFSW2", args = c(args_extract, x = list(g.elev)))  # elevation in m a.s.l.
    if (is.vector(temp)) {
      MMC[["data"]][todos, "ELEV_m"] <- temp

    } else if (is.array(temp)) {
      MMC[["data"]][todos, ] <- temp[, 1, ]

    } else {
      stop("Unknown object returned from 'extract_rSFSW2' when extracting",
        "elevation data.")
    }

    # Determine successful extractions
    MMC[["idone"]]["NEDUSA1"] <- TRUE
    i_good <- todos & !has_incompletedata(MMC[["data"]]) #length(i_good) == sum(todos) == runsN_sites
    i_notgood <- todos & has_incompletedata(MMC[["data"]]) #length(i_good) == sum(todos) == runsN_sites
    MMC[["source"]][i_notgood] <- NA

    if (any(i_good)) {
      MMC[["source"]][i_good] <- "Elevation_NED_USA"
      if (verbose)
        print(paste("'ExtractElevation_NED_USA' was extracted for n =",
          sum(i_good), "out of", n_extract, "sites"))

      MMC <- update_elevation_input(MMC, sim_size, digits = 0, fnames_in)
    }
  }

  MMC
}


#' @references Harmonized World Soil Database
do_ExtractElevation_HWSD_Global <- function(MMC, sim_size, sim_space, dir_ex_dem,
  fnames_in, resume, verbose) {

  if (verbose) {
    t1 <- Sys.time()
    temp_call <- shQuote(match.call()[1])
    print(paste0("rSFSW2's ", temp_call, ": started at ", t1))

    on.exit({print(paste0("rSFSW2's ", temp_call, ": ended after ",
      round(difftime(Sys.time(), t1, units = "secs"), 2), " s")); cat("\n")}, add = TRUE)
  }

  stopifnot(requireNamespace("raster"), requireNamespace("sp"), requireNamespace("rgdal"))

  MMC[["idone"]]["HWSD1"] <- FALSE
  todos <- has_incompletedata(MMC[["data"]]) | is.na(MMC[["source"]]) |
    MMC[["source"]] == "Elevation_HWSD_Global"

  if (resume) {
    todos <- todos & has_nodata(MMC[["input"]][sim_size[["runIDs_sites"]], ], "ELEV_m") &
      MMC[["do_include"]]
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
      sites_hwsd <- sp::spTransform(sites_hwsd, CRS = crs_data)  #transform graphics::points to grid-coords
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
    temp <- do.call("extract_rSFSW2", args = c(args_extract, x = list(g.elev)))  # elevation in m a.s.l.

    if (is.vector(temp)) {
      MMC[["data"]][todos, "ELEV_m"] <- temp

    } else if (is.array(temp)) {
      MMC[["data"]][todos, ] <- temp[, 1, ]

    } else {
      stop("Unknown object returned from 'extract_rSFSW2' when",
        "extracting elevation data.")
    }

    # Determine successful extractions
    MMC[["idone"]]["HWSD1"] <- TRUE
    i_good <- todos & !has_incompletedata(MMC[["data"]]) #length(i_good) == sum(todos) == runsN_sites
    i_notgood <- todos & has_incompletedata(MMC[["data"]]) #length(i_good) == sum(todos) == runsN_sites
    MMC[["source"]][i_notgood] <- NA

    if (any(i_good)) {
      MMC[["source"]][i_good] <- "Elevation_HWSD_Global"
      if (verbose)
        print(paste("'ExtractElevation_HWSD_Global' was extracted for n =",
          sum(i_good), "out of", n_extract, "sites"))

      MMC <- update_elevation_input(MMC, sim_size, digits = 0, fnames_in)
    }
  }

  MMC
}


#' Extract elevation data
#' @export
ExtractData_Elevation <- function(exinfo, SFSW2_prj_meta, SFSW2_prj_inputs, resume = FALSE,
  verbose = FALSE) {

  field_sources <- "Elevation_source"
  field_include <- "Include_YN_ElevationSources"

  MMC <- prepare_ExtractData_Elevation(SFSW2_prj_inputs[["SWRunInformation"]],
    sim_size = SFSW2_prj_meta[["sim_size"]], field_sources = field_sources,
    field_include = field_include,
    how_determine_sources = SFSW2_prj_meta[["opt_input"]][["how_determine_sources"]],
    SFSW2_prj_meta[["sim_space"]][["scorp"]])

  if (exinfo$ExtractElevation_NED_USA) {
    MMC <- do_ExtractElevation_NED_USA(MMC, sim_size = SFSW2_prj_meta[["sim_size"]],
      sim_space = SFSW2_prj_meta[["sim_space"]],
      dir_ex_dem = SFSW2_prj_meta[["project_paths"]][["dir_ex_dem"]],
      fnames_in = SFSW2_prj_meta[["fnames_in"]],
      resume, verbose)
  }

  if (exinfo$ExtractElevation_HWSD_Global) {
    MMC <- do_ExtractElevation_HWSD_Global(MMC, sim_size = SFSW2_prj_meta[["sim_size"]],
      sim_space = SFSW2_prj_meta[["sim_space"]],
      dir_ex_dem = SFSW2_prj_meta[["project_paths"]][["dir_ex_dem"]],
      fnames_in = SFSW2_prj_meta[["fnames_in"]], resume, verbose)
  }

  SFSW2_prj_inputs[["SWRunInformation"]] <- update_datasource_masterfield(MMC,
    sim_size = SFSW2_prj_meta[["sim_size"]], SFSW2_prj_inputs[["SWRunInformation"]],
    SFSW2_prj_meta[["fnames_in"]], field_sources, field_include)

  SFSW2_prj_inputs
}

#----------------------------------------------------------------------------------------#
