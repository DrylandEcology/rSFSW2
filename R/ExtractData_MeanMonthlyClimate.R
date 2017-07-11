
prepare_ExtractData_MeanMonthlyClimate <- function(SWRunInformation, sim_size,
  field_sources, how_determine_sources, sw_input_cloud_use, sw_input_cloud) {

  sites_monthlyclim_source <- get_datasource_masterfield(SWRunInformation,
    field_sources, sim_size, how_determine_sources)

  dtemp <- array(NA, dim = c(sim_size[["runsN_sites"]], 3, 12), dimnames = list(NULL,
      c("RH", "cover", "wind"), NULL))

  list(source = sites_monthlyclim_source, data = dtemp, idone = vector(),
    use = sw_input_cloud_use, input = sw_input_cloud)
}


update_meanmonthlyclimate_input <- function(MMC, use_site, sim_size, digits = 2,
  fnames_in) {

  #add data to MMC[["input"]] and set the use flags
  icol <- grep("RH", names(MMC[["use"]]))
  MMC[["use"]][icol] <- TRUE
  MMC[["input"]][sim_size[["runIDs_sites"]][use_site], icol][, SFSW2_glovars[["st_mo"]]] <-
    round(MMC[["data"]][use_site, "RH", ], digits)
  icol <- grep("SkyC", names(MMC[["use"]]))
  MMC[["use"]][icol] <- TRUE
  MMC[["input"]][sim_size[["runIDs_sites"]][use_site], icol][, SFSW2_glovars[["st_mo"]]] <-
    round(MMC[["data"]][use_site, "cover", ], digits)
  icol <- grep("wind", names(MMC[["use"]]))
  MMC[["use"]][icol] <- TRUE
  MMC[["input"]][sim_size[["runIDs_sites"]][use_site], icol][, SFSW2_glovars[["st_mo"]]] <-
    round(MMC[["data"]][use_site, "wind", ], digits)

  #write data to disk
  utils::write.csv(reconstitute_inputfile(MMC[["use"]], MMC[["input"]]),
    file = fnames_in[["fclimnorm"]], row.names = FALSE)
  unlink(fnames_in[["fpreprocin"]])

  MMC
}


#' @references National Climatic Data Center. 2005. Climate maps of the United States.
#'  Available online http://cdo.ncdc.noaa.gov/cgi-bin/climaps/climaps.pl. Last accessed
#'  May 2010.
do_ExtractSkyDataFromNOAAClimateAtlas_USA <- function(MMC, sim_size, sim_space,
  project_paths, fnames_in, opt_chunks, resume, verbose) {

  if (verbose) {
    t1 <- Sys.time()
    temp_call <- shQuote(match.call()[1])
    print(paste0("rSFSW2's ", temp_call, ": started at ", t1))

    on.exit({print(paste0("rSFSW2's ", temp_call, ": ended after ",
      round(difftime(Sys.time(), t1, units = "secs"), 2), " s")); cat("\n")}, add = TRUE)
  }

  stopifnot(requireNamespace("raster"), requireNamespace("sp"), requireNamespace("rgdal"))

  MMC[["idone"]]["NCDC1"] <- FALSE
  todos <- has_incompletedata(MMC[["data"]]) | is.na(MMC[["source"]]) |
    MMC[["source"]] == "ClimateNormals_NCDC2005_USA"

  if (resume) {
    todos <- todos & (
      has_nodata(MMC[["input"]][sim_size[["runIDs_sites"]], ], "RH") |
      has_nodata(MMC[["input"]][sim_size[["runIDs_sites"]], ], "SkyC") |
      has_nodata(MMC[["input"]][sim_size[["runIDs_sites"]], ], "wind"))
  }
  names(todos) <- NULL
  i_extract <- as.integer(which(todos))
  n_extract <- sum(todos)

  if (n_extract > 0) {
    if (verbose)
      print(paste("Data from 'NCDC2005_USA' will be extracted for n =", n_extract,
        "sites"))

    # NOAA Climate Atlas: provides no information on height above ground: assuming 2-m
    # which is what is required by SOILWAT2
    dir.ex.dat <- file.path(project_paths[["dir_ex_weather"]], "ClimateAtlasUS")
    stopifnot(file.exists(dir.ex.dat))

    dir_noaaca <- list(
      RH = file.path(dir.ex.dat, "HumidityRelative_Percent"),
      cover = file.path(dir.ex.dat, "Sunshine_Percent"),
      # cover = file.path(dir.ex.dat, "SkyCoverDay_Percent"),
      wind = file.path(dir.ex.dat, "WindSpeed_mph"))

    files_shp <- list(
      RH = paste0("RH23", formatC(SFSW2_glovars[["st_mo"]], width = 2, format = "d",
        flag = "0")),
      cover = paste0("SUN52", formatC(SFSW2_glovars[["st_mo"]], width = 2, format = "d",
        flag = "0")),
      # cover = paste0("SKYC50", formatC(SFSW2_glovars[["st_mo"]], width = 2,
      #  format = "d", flag = "0")),
      wind = paste0("WND60B", formatC(SFSW2_glovars[["st_mo"]], width = 2, format = "d",
        flag = "0")))

    var_codes <- list(
      RH = c(10, 23, 31, 41, 51, 61, 71, 78, 90), #percent
      cover = c(11, 26, 36, 46, 56, 66, 76, 86, 96),  #percent
      # cover = c(11, 23, 31, 41, 51, 61, 71, 81, 93),  #percent
      wind = c(1.3, 2.9, 3.3, 3.8, 4.2, 4.7, 5.1, 5.6, 9.6))  #m/s; the last category is
      # actually open '> 12.9 mph': I closed it arbitrarily with 30 mph

    stopifnot(colnames(MMC[["data"]]) == names(dir_noaaca),
      colnames(MMC[["data"]]) == names(files_shp),
      colnames(MMC[["data"]]) == names(var_codes))

    #locations of simulation runs
    sites_noaaca <- sim_space[["run_sites"]][todos, ]
    # Align with data crs
    noaaca <- rgdal::readOGR(dsn = dir_noaaca[["RH"]], layer = files_shp[["RH"]][1], verbose = FALSE)
    crs_data <- raster::crs(noaaca)
    if (!raster::compareCRS(sim_space[["crs_sites"]], crs_data)) {
      sites_noaaca <- sp::spTransform(sites_noaaca, CRS = crs_data)  #transform graphics::points to grid-coords
    }

    if (sim_space[["scorp"]] == "point") {
      args_extract <- list(y = sites_noaaca, type = sim_space[["scorp"]])

    } else if (sim_space[["scorp"]] == "cell") {
      cell_res_noaaca <- align_with_target_res(res_from = sim_space[["sim_res"]],
        crs_from = sim_space[["sim_crs"]], sp = sites_noaaca,
        crs_sp = sim_space[["crs_sites"]], crs_to = crs_data)
      args_extract <- list(y = cell_res_noaaca, coords = sites_noaaca,
        crs_data = crs_data, type = sim_space[["scorp"]])
    }

    # determine NOAA CA extractions to do
    do_chunks <- parallel::splitIndices(n_extract,
      ceiling(n_extract / opt_chunks[["ExtractSkyDataFromNOAAClimateAtlas_USA"]]))

    n_vars <- ncol(MMC[["data"]])
    n_months <- 12L
    n_chunks <- length(do_chunks)
    iv <- m <- ic <- 1

    # determine start location based on interrupted data extraction
    ftemp_noaaca <- file.path(project_paths[["dir_out_temp"]], "NOAA_ClimateAtlas_extraction.rds")

    if (resume && file.exists(ftemp_noaaca)) {
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
        print(paste0(Sys.time(), ": data from 'NCDC2005_USA' extracting for: ",
          paste(names(dir_noaaca)[iv], month.name[m], paste("chunk", ic, "of",
          n_chunks), sep = ", ")))

      iextr <- i_extract[do_chunks[[ic]]]
      args_chunk <- args_extract
      args_chunk[["y"]] <- args_chunk[["y"]][do_chunks[[ic]], ]
      if (!is.null(args_chunk[["coords"]]))
        args_chunk[["coords"]] <- args_chunk[["coords"]][do_chunks[[ic]], ]

      MMC[["data"]][iextr, iv, m] <- do.call("extract_rSFSW2", args = c(args_chunk,
        x = list(dir_noaaca[[iv]]), file_shp = list(files_shp[[iv]][m]),
        fields = list("GRIDCODE"), code = list(var_codes[[iv]])))

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

      if (resume)
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
        print(paste("Data from 'NCDC2005_USA' was extracted for n =", sum(i_good),
          "out of", n_extract, "sites"))

      MMC <- update_meanmonthlyclimate_input(MMC, i_good, sim_size, digits = 2, fnames_in)
    }
  }

  MMC
}


#' Extract gridded mean monthly data from NCEP/CFSR for sites globally
#'
#' @section Monthly data: (http://rda.ucar.edu/datasets/ds093.2/):
#'  ds093.2 - NCEP Climate Forecast System Reanalysis (CFSR) Monthly Products, January
#'  1979 to December 2010, 0.313-deg: monthly mean (4 per day) of forecasts of 6-hour
#'  average.
#'  \describe{
#'    \item{relative humidity}{percentage for entire atmosphere at 2 m above ground
#'        [0.5-deg]: 'pgbh06.gdas.R_H.2m.grb2' --> means for Jan-Dec}
#'    \item{wind (m s-1)}{u- and v-component at 10 m above ground in m s-1:
#'        'flxf06.gdas.WND.10m.grb2' (u- and v-component) --> means for Jan-Dec}
#'    \item{total cloud cover}{percentage of entire atmosphere as a single layer:
#'        'flxf06.gdas.T_CDC.EATM.grb2' --> means for Jan-Dec}
#'  }
#'
#' @references Environmental Modeling Center/National Centers for Environmental
#'  Prediction/National Weather Service/NOAA/U.S. Department of Commerce. 2010. NCEP
#'  Climate Forecast System Reanalysis (CFSR) Monthly Products, January 1979 to December
#'  2010. Research Data Archive at the National Center for Atmospheric Research,
#'  Computational and Information Systems Laboratory.
#'  http://rda.ucar.edu/datasets/ds093.2/. Accessed 8 March 2012.
#' @export
do_ExtractSkyDataFromNCEPCFSR_Global <- function(MMC, SWRunInformation, SFSW2_prj_meta,
  opt_parallel, opt_chunks, resume, verbose) {

  if (verbose) {
    t1 <- Sys.time()
    temp_call <- shQuote(match.call()[1])
    print(paste0("rSFSW2's ", temp_call, ": started at ", t1))

    on.exit({print(paste0("rSFSW2's ", temp_call, ": ended after ",
      round(difftime(Sys.time(), t1, units = "secs"), 2), " s")); cat("\n")}, add = TRUE)
  }

  #--- SET UP PARALLELIZATION
  setup_SFSW2_cluster(opt_parallel,
    dir_out = SFSW2_prj_meta[["project_paths"]][["dir_prj"]],
    verbose = opt_verbosity[["verbose"]])
  on.exit(exit_SFSW2_cluster(verbose = opt_verbosity[["verbose"]]),
    add = TRUE)
  on.exit(set_full_RNG(SFSW2_prj_meta[["rng_specs"]][["seed_prev"]],
    kind = SFSW2_prj_meta[["rng_specs"]][["RNGkind_prev"]][1],
    normal.kind = SFSW2_prj_meta[["rng_specs"]][["RNGkind_prev"]][2]),
    add = TRUE)


  if (is.null(SFSW2_prj_meta[["prepd_CFSR"]]) ||
    inherits(SFSW2_prj_meta[["prepd_CFSR"]], "try-error")) {

    SFSW2_prj_meta[["prepd_CFSR"]] <- try(prepare_NCEPCFSR_extraction(
      dir_in = SFSW2_prj_meta[["project_paths"]][["dir_in"]],
      dir.cfsr.data = SFSW2_prj_meta[["project_paths"]][["dir.ex.NCEPCFSR"]]))
  }

  stopifnot(!inherits(SFSW2_prj_meta[["prepd_CFSR"]], "try-error"))
  stopifnot(requireNamespace("rgdal"))

  MMC[["idone"]]["NCEPCFSR1"] <- FALSE
  todos <- has_incompletedata(MMC[["data"]]) | is.na(MMC[["source"]]) |
    MMC[["source"]] == "ClimateNormals_NCEPCFSR_Global"

  if (resume) {
    todos <- todos & (
      has_nodata(MMC[["input"]][SFSW2_prj_meta[["sim_size"]][["runIDs_sites"]], ], "RH") |
      has_nodata(MMC[["input"]][SFSW2_prj_meta[["sim_size"]][["runIDs_sites"]], ], "SkyC") |
      has_nodata(MMC[["input"]][SFSW2_prj_meta[["sim_size"]][["runIDs_sites"]], ], "wind"))
  }
  names(todos) <- NULL
  n_extract <- sum(todos)

  if (n_extract > 0) {
    if (verbose)
      print(paste("Data from 'NCEPCFSR_Global' will be extracted for n =", n_extract,
        "sites"))

    #locations of simulation runs
    locations <- SWRunInformation[SFSW2_prj_meta[["sim_size"]][["runIDs_sites"]][todos], c("WeatherFolder", "X_WGS84", "Y_WGS84")]
    # do the extractions
    temp <- try(get_NCEPCFSR_data(dat_sites = locations, daily = FALSE, monthly = TRUE,
      dbW_digits = SFSW2_prj_meta[["opt_sim"]][["dbW_digits"]],
      yearLow = SFSW2_prj_meta[["sim_time"]][["startyr"]],
      yearHigh = SFSW2_prj_meta[["sim_time"]][["endyr"]],
      dir_ex_cfsr = SFSW2_prj_meta[["prepd_CFSR"]]$dir_ex_cfsr,
      dir_temp = SFSW2_prj_meta[["project_paths"]][["dir_out_temp"]],
      n_site_per_core = opt_chunks[["ExtractSkyDataFromNCEPCFSR_Global"]],
      rm_mc_files = TRUE, resume = resume))

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
        print(paste("Data from 'NCEPCFSR_Global' was extracted for n =", sum(i_good),
          "out of", n_extract, "sites"))

      MMC <- update_meanmonthlyclimate_input(MMC, i_good, SFSW2_prj_meta[["sim_size"]],
        digits = SFSW2_prj_meta[["opt_sim"]][["dbW_digits"]], SFSW2_prj_meta[["fnames_in"]])
    }
  }

  oe <- sys.on.exit()
  oe <- remove_from_onexit_expression(oe, "exit_SFSW2_cluster")
  on.exit(eval(oe), add = FALSE)

  MMC
}


#' Extract mean monthly climate data: cloud cover, relative humidity, and wind speed
#' @export
ExtractData_MeanMonthlyClimate <- function(exinfo, SFSW2_prj_meta, SFSW2_prj_inputs,
  opt_parallel, opt_chunks, resume = FALSE, verbose = FALSE) {

  field_sources <- "ClimateNormals_source"
  field_include <- "Include_YN_ClimateNormalSources"

  MMC <- prepare_ExtractData_MeanMonthlyClimate(SFSW2_prj_inputs[["SWRunInformation"]],
    sim_size = SFSW2_prj_meta[["sim_size"]], field_sources = field_sources,
    how_determine_sources = SFSW2_prj_meta[["opt_input"]][["how_determine_sources"]],
    sw_input_cloud_use = SFSW2_prj_inputs[["sw_input_cloud_use"]],
    sw_input_cloud = SFSW2_prj_inputs[["sw_input_cloud"]])

  if (exinfo$ExtractSkyDataFromNOAAClimateAtlas_USA) {
    MMC <- do_ExtractSkyDataFromNOAAClimateAtlas_USA(MMC,
      sim_size = SFSW2_prj_meta[["sim_size"]], sim_space = SFSW2_prj_meta[["sim_space"]],
      project_paths = SFSW2_prj_meta[["project_paths"]],
      fnames_in = SFSW2_prj_meta[["fnames_in"]], opt_chunks, resume, verbose)
  }

  if (exinfo$ExtractSkyDataFromNCEPCFSR_Global) {
    MMC <- do_ExtractSkyDataFromNCEPCFSR_Global(MMC, SFSW2_prj_inputs[["SWRunInformation"]],
      SFSW2_prj_meta, opt_parallel, opt_chunks, resume, verbose)

  }

  SFSW2_prj_inputs[["SWRunInformation"]] <- update_datasource_masterfield(MMC,
    sim_size = SFSW2_prj_meta[["sim_size"]], SFSW2_prj_inputs[["SWRunInformation"]],
    SFSW2_prj_meta[["fnames_in"]], field_sources, field_include)

  SFSW2_prj_inputs[["sw_input_cloud_use"]] <- MMC[["use"]]
  SFSW2_prj_inputs[["sw_input_cloud"]] <- MMC[["input"]]


  SFSW2_prj_inputs
}

#----------------------------------------------------------------------------------------#
