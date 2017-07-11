#---------------------------------------------------------------------------------------#
#------EXTRACT SOIL CHARACTERISTICS------

#' Preparations for the extraction of external soil datasets
prepare_ExtractData_Soils <- function(SWRunInformation, sim_size, field_sources,
  how_determine_sources, sw_input_soillayers, sw_input_soils_use, sw_input_soils) {

  sites_soils_source <- get_datasource_masterfield(SWRunInformation,
    field_sources, sim_size, how_determine_sources)

  lvars <- c("density", "sand", "clay", "rock", "carbon")
  nvars <- length(lvars)
  coln <- c("i", "depth", paste0(rep(lvars, SFSW2_glovars[["slyrs_maxN"]]), "_L",
    rep(SFSW2_glovars[["slyrs_ids"]], each = nvars)))
  dtemp <- matrix(NA, nrow = sim_size[["runsN_sites"]],
    ncol = 2 + nvars * SFSW2_glovars[["slyrs_maxN"]], dimnames = list(NULL, coln))
  vars <- data.frame(input = c("SoilDepth_cm", "Matricd_L", "Sand_L",
                               "Clay_L", "GravelContent_L", "TOC_GperKG_L"),
                     intern = c("depth", lvars),
                     stringsAsFactors = FALSE)

  list(source = sites_soils_source, data = dtemp, idone = vector(),
    use = sw_input_soils_use, input = sw_input_soils, input2 = sw_input_soillayers,
    cn = coln, vars = vars, nvars = nvars)
}


update_soils_input <- function(MMC, sim_size, digits = 2, i_Done, ldepths_cm,
  lys, fnames_in) {

  #add data to MMC[["input"]] and set the use flags
  #set and save soil layer structure
  temp <- MMC[["data"]][i_Done, MMC[["vars"]][1, "intern"]]
  MMC[["input2"]][sim_size[["runIDs_sites"]][i_Done], MMC[["vars"]][1, "input"]] <-
    round(temp)
  icol <- grep("depth_L", colnames(MMC[["input2"]]))
  temp <- rep(ldepths_cm[lys], sum(i_Done))
  MMC[["input2"]][sim_size[["runIDs_sites"]][i_Done], icol[lys]] <- matrix(temp,
    nrow = sum(i_Done), ncol = length(lys), byrow = TRUE)
  MMC[["input2"]][sim_size[["runIDs_sites"]][i_Done], icol[-lys]] <- NA
  utils::write.csv(MMC[["input2"]], file = fnames_in[["fslayers"]], row.names = FALSE)
  unlink(fnames_in[["fpreprocin"]])

  #set and save soil texture
  for (k in 1 + seq_len(MMC[["nvars"]])) {
    icol <- grep(MMC[["vars"]][k, "input"], names(MMC[["use"]]))
    temp <- MMC[["data"]][i_Done, paste0(MMC[["vars"]][k, "intern"], "_L", lys)]
    if (!all(is.na(temp))) {
      MMC[["input"]][sim_size[["runIDs_sites"]][i_Done], icol[lys]] <- round(temp, digits)
      MMC[["use"]][icol[lys]] <- TRUE
      MMC[["use"]][icol[-lys]] <- FALSE
    }
  }

  #write data to disk
  utils::write.csv(reconstitute_inputfile(MMC[["use"]], MMC[["input"]]),
    file = fnames_in[["fsoils"]], row.names = FALSE)
  unlink(fnames_in[["fpreprocin"]])

  MMC
}

adjust_soils_todos <- function(todos, MMC, sim_size) {
  temp <- is.na(MMC[["input2"]][sim_size[["runIDs_sites"]], MMC[["vars"]][1, "input"]])

  for (k in 1 + seq_len(MMC[["nvars"]])) {
    temp <- temp | has_nodata(MMC[["input"]][sim_size[["runIDs_sites"]], ], MMC[["vars"]][k, "input"])
  }
  todos <- todos & temp
}



#' CONUS-SOIL is a rasterized and controlled STATSGO data set; information for 11 soil
#' are layers available.
#'
#' @param default_TOC_GperKG A numeric value. The default value is 0 g TOC per kg soil.
#'
#' @references Miller, D. A. and R. A. White. 1998. A conterminous United States
#'  multilayer soil characteristics dataset for regional climate and hydrology modeling.
#'  Earth Interactions 2:1-26.
#' @section Note(drs): it appears that NODATA values are recorded as 0
#'
do_ExtractSoilDataFromCONUSSOILFromSTATSGO_USA <- function(MMC, sim_size, sim_space,
  dir_ex_soil, fnames_in, resume, verbose, default_TOC_GperKG = 0) {

  if (verbose) {
    t1 <- Sys.time()
    temp_call <- shQuote(match.call()[1])
    print(paste0("rSFSW2's ", temp_call, ": started at ", t1))

    on.exit({print(paste0("rSFSW2's ", temp_call, ": ended after ",
      round(difftime(Sys.time(), t1, units = "secs"), 2), " s")); cat("\n")}, add = TRUE)
  }

  stopifnot(requireNamespace("raster"), requireNamespace("sp"), requireNamespace("rgdal"))

  MMC[["idone"]]["CONUSSOIL1"] <- FALSE
  todos <- is.na(MMC[["source"]]) | MMC[["source"]] == "CONUSSOILFromSTATSGO_USA"

  if (resume) {
    todos <- adjust_soils_todos(todos, MMC, sim_size)
  }
  names(todos) <- NULL
  n_extract <- sum(todos)

  if (n_extract > 0) {
    if (verbose)
      print(paste("Soil data from 'CONUSSOILFromSTATSGO_USA' will be extracted for n =",
        n_extract, "sites"))

    dir.ex.conus <- file.path(dir_ex_soil, "CONUSSoil", "output", "albers")
    stopifnot(file.exists(dir.ex.conus))

    ldepth_CONUS <- c(0, 5, 10, 20, 30, 40, 60, 80, 100, 150, 200, 250)  #in cm
    layer_N <- length(ldepth_CONUS) - 1
    ils <- seq_len(layer_N)

    g <- raster::brick(file.path(dir.ex.conus, "bd.tif"))
    crs_data <- raster::crs(g)

    #locations of simulation runs
    sites_conus <- sim_space[["run_sites"]][todos, ]
    # Align with data crs
    if (!raster::compareCRS(sim_space[["crs_sites"]], crs_data)) {
      sites_conus <- sp::spTransform(sites_conus, CRS = crs_data)  #transform graphics::points to grid-coords
    }

    if (sim_space[["scorp"]] == "point") {
      cell_res_conus <- NULL
      args_extract <- list(y = sites_conus, type = sim_space[["scorp"]])

    } else if (sim_space[["scorp"]] == "cell") {
      cell_res_conus <- align_with_target_res(res_from = sim_space[["sim_res"]],
        crs_from = sim_space[["sim_crs"]], sp = sim_space[["run_sites"]][todos, ],
        crs_sp = sim_space[["crs_sites"]], crs_to = crs_data)
      args_extract <- list(y = cell_res_conus, coords = sites_conus, method = "block",
        type = sim_space[["scorp"]])
    }

    #---extract data
    # bulk density -> matric density
    message("NOTE: soil density values extracted from CONUS-soil (gridded STATSGO) may ",
      "be too low!")
    cond30 <- compiler::cmpfun(function(v) ifelse(is.na(v) | v < 30, NA, v))
    ftemp <- file.path(dir.ex.conus, "bd_cond30.tif")
    g <- if (file.exists(ftemp)) {
        raster::brick(ftemp)
      } else {
        # bulk density of less than 0.3 g / cm3 should be treated as no soil
        raster::calc(g, fun = cond30, filename = ftemp)
      }
    temp <- do.call("extract_rSFSW2", args = c(args_extract, x = list(g)))
    MMC[["data"]][todos, grep("density", MMC[["cn"]])[ils]] <- temp / 100

    # Convert bulk density to matric density
    #  eqn. 20 from Saxton et al. 2006: bulkd <- matricd * (1 - rockvol) + rockvol * 2.65
    # This appears to be matric density, Miller et al. 1998 has labelled this as bulk. However, eq. 20 (Saxton et al. 2006) would give negative values if we assumed it to be bulk density
    #matricd <- ifelse(abs(1 - rockvol) > sqrt(.Machine$double.eps), (bulkd - rockvol * 2.65) / (1 - rockvol), 0)

    # soil depth
    # depth in cm >< bedrock from datafile.bedrock, but seems to make more sense?
    cond0 <- compiler::cmpfun(function(v) ifelse(!is.na(v) & v > 0, v, NA))
    ftemp <- file.path(dir.ex.conus, "rockdepm_cond0.tif")
    g <- if (file.exists(ftemp)) {
        raster::raster(ftemp)
      } else {
        # rockdepth of 0 cm should be treated as no soil
        raster::calc(raster::raster(file.path(dir.ex.conus, "rockdepm.tif")),
          fun = cond0, filename = ftemp)
      }
    rockdep_cm <- do.call("extract_rSFSW2", args = c(args_extract, x = list(g)))

    # rock volume
    g <- raster::brick(file.path(dir.ex.conus, "rockvol.tif")) #New with v31: rockvol -> gravel vol%
    temp <- do.call("extract_rSFSW2", args = c(args_extract, x = list(g)))
    temp <- ifelse(is.finite(temp), temp, NA)
    # eq. 7 of Miller et al. 1998
    temp <- pmax(pmin(temp / 100, 1), 0) # volume fraction of bulk = total soil

    # adjust soil depth by layers with 100% rock volume
    solid_rock_nl <- apply(temp >= 1 - SFSW2_glovars[["toln"]], 1, sum, na.rm = TRUE)
    solid_rock_nl <- 1 + layer_N - solid_rock_nl
    solid_rock_cm <- ldepth_CONUS[solid_rock_nl]
    MMC[["data"]][todos, grep("rock", MMC[["cn"]])[ils]] <- temp
    MMC[["data"]][todos, "depth"] <- pmin(rockdep_cm, solid_rock_cm)

    lys <- seq_len(max(findInterval(MMC[["data"]][todos, "depth"], ldepth_CONUS[-1]),
      na.rm = TRUE))

    # sand, silt, and clay
    ftemp <- file.path(dir.ex.conus, "sand_cond0.tif")
    g <- if (file.exists(ftemp)) {
        raster::brick(ftemp)
      } else {
        raster::calc(raster::brick(file.path(dir.ex.conus, "sand.tif")), fun = cond0,
          filename = ftemp)
      }
    sand <- do.call("extract_rSFSW2", args = c(args_extract, x = list(g)))

    ftemp <- file.path(dir.ex.conus, "clay_cond0.tif")
    g <- if (file.exists(ftemp)) {
        raster::brick(ftemp)
      } else {
        raster::calc(raster::brick(file.path(dir.ex.conus, "clay.tif")), fun = cond0,
          filename = ftemp)
      }
    clay <- do.call("extract_rSFSW2", args = c(args_extract, x = list(g)))

    ftemp <- file.path(dir.ex.conus, "silt_cond0.tif")
    g <- if (file.exists(ftemp)) {
        raster::brick(ftemp)
      } else {
        raster::calc(raster::brick(file.path(dir.ex.conus, "silt.tif")), fun = cond0,
          filename = ftemp)
      }
    silt <- do.call("extract_rSFSW2", args = c(args_extract, x = list(g)))

    #Normalize to 0-1
    total_matric <- sand + clay + silt # values between 0.99 and 1.01 (of the matric component)
    total_matric[!is.finite(total_matric)] <- NA
    MMC[["data"]][todos, grep("sand", MMC[["cn"]])[ils]] <- sand / total_matric
    MMC[["data"]][todos, grep("clay", MMC[["cn"]])[ils]] <- clay / total_matric

    # There is no organic carbon data, set all values to a default
    MMC[["data"]][todos, grep("carbon", MMC[["cn"]])[ils]] <- default_TOC_GperKG

    # Determine successful extractions
    i_good <- stats::complete.cases(MMC[["data"]][todos, "depth"]) #length(i_good) == sum(todos)
    MMC[["source"]][which(todos)[!i_good]] <- NA

    if (any(i_good)) {
      MMC[["idone"]]["CONUSSOIL1"] <- TRUE
      i_Done <- rep(FALSE, times = sim_size[["runsN_sites"]]) #length(i_Done) == length(runIDs_sites) == runsN_sites
      i_Done[which(todos)[i_good]] <- TRUE #sum(i_Done) == sum(i_good)

      MMC[["source"]][i_Done] <- "CONUSSOILFromSTATSGO_USA"
      MMC <- update_soils_input(MMC, sim_size, digits = 2, i_Done,
        ldepths_cm = ldepth_CONUS[-1], lys, fnames_in)
    }

    if (verbose)
      print(paste("Soil data from 'CONUSSOILFromSTATSGO_USA' was extracted for n =",
        sum(i_good), "out of", n_extract, "sites"))
  }

  MMC
}


#' A wrapper for \code{reaggregate_raster} designed to work with the rasters of the
#' ISRIC-WISE datasets versions 5-arcmin v1.2 and 30-arcsec v1.0
#'
#' @param i An integer value. The index to select a location from among \code{sp_sites}
#'  and the corresponding resolution \code{res}.
#' @param res A numeric vector of length two or a matrix with two columns. The x- and
#'  y-extent of the rectangle(s) for which to extract values.
#' @param grid A \linkS4class{RasterLayer} object with one layer. The raster from which
#'  values are extracted.
#' @param sp_sites A \linkS4class{SpatialPoints} object. This object is used to extract
#'  the coordinates of the i-th location.
#' @param att A character string. Which variable in the RAT table should be returned.
#'  If \code{NULL} then extracted values of \code{grid} are returned.
#'
#' @seealso \code{\link{reaggregate_raster}}
#'
#' @return A list with four elements
#'  \describe{
#'    \item{i}{An integer value. The location index.}
#'    \item{SUIDs_N}{An integer vector. The number of unique values within the rectangle
#'      of \code{x}.}
#'    \item{SUID}{A numeric vector or a vector of character strings. The unique soil
#'      soil identifier values or \code{NA}.}
#'    \item{fraction}{A numeric vector. The relative areas covered by \code{SUID}.}
#'  }
ISRICWISE_extract_SUIDs <- function(i, res = c(0, 0), grid, sp_sites, att = NULL) {
  stopifnot(requireNamespace("sp"))

  out <- try(reaggregate_raster(x = grid,
        coords = sp::coordinates(sp_sites[i, ]),
        to_res = if (is.null(dim(res))) res else res[i, ],
        with_weights = TRUE,
        method = "block"))

  if (inherits(out, "try-error")) {
    print(out)
    list(i = i, SUIDs_N = -1, SUID = NULL, fraction = NULL)

  } else {
    suids <- if (is.null(att)) {
        temp <- out[[1]][["values"]][[1]]
        ifelse(temp < SFSW2_glovars[["tol"]], NA, temp)
    } else {
        temp <- raster::factorValues(grid, out[[1]][["values"]][[1]], att = att)
        temp <- as.character(unlist(temp))
        ifelse(nchar(temp) == 0L, NA, temp)
    }
    list(i = i, SUIDs_N = out[[1]][["N"]][[1]],
          SUID = suids,
          fraction = out[[1]][["fraction"]][[1]])
  }
}

ISRICWISE_get_prids <- function(suid, dat_wise, layer_N, colname_suid) {
  # If is.na(suid) then 'soils' is a 0-row data.frame
  soils <- if (is.na(suid)) {
      dat_wise[0, ]
    } else {
      dat_wise[dat_wise[, colname_suid] == suid, ]
    }
  frac <- unique(soils[, c("PROP", "PRID")])
  depth <- tapply(soils[, "BotDep"], soils[, "PRID"], max)
  idepth <- depth[match(frac$PRID, names(depth))]

  list(PRIDs_N = nrow(soils) / layer_N,
     PRID = frac$PRID,
     fraction = frac$PROP / 100,
     depth = ifelse(idepth > 0, idepth, NA),
     soildat = soils)
}


ISRICWISE_get_SoilDatValuesForLayer <- function(dat, soildat_rows, frac) {
  dat_add <- ifelse(is.na(soildat_rows) | soildat_rows < 0, NA, soildat_rows)
  res_frac <- ifelse(is.na(dat_add) | is.na(frac), 0, frac)

  # NAs do not propagate because they are down-weighted by res_frac
  list(
    # weighted mean = sum of values x weights
    soil = sum(dat, dat_add * res_frac, na.rm = TRUE),
    frac = sum(res_frac, na.rm = TRUE))
}


max_depth_byPRIDs <- function(this_soil, var_ids, val_rocks) {
  if (is.null(val_rocks) || length(val_rocks) == 0L) {
    this_soil[["depth"]]

  } else {
    depths <- by(this_soil[["soildat"]][, c("BotDep", var_ids)],
        INDICES = this_soil[["soildat"]][, "PRID"],
        function(x1) {
          temp <- sapply(val_rocks, function(x2) {
            apply(x1[, -1] - x2, 1, function(x3) any(abs(x3) < SFSW2_glovars[["tol"]]))})
          itemp <- which(!apply(temp, 1, any))
          if (length(itemp) > 0) x1[itemp[length(itemp)], "BotDep"] else 0
      })
    depths[match(dimnames(depths)[[1]], this_soil[["PRID"]], nomatch = 0)]
  }
}

#' Calculate weighted mean soil variables from one of the ISRIC-WISE data bases for one
#'    simulation cell; areas with no soil are 'removed' and the remaining data 'scaled' to 1
#'
#' @param i An integer value. The number of the simulation site/cell location.
#' @param i_sim_cells_SUIDs A named numeric vector. A row of of the data.frame returned
#'    by \code{\link{ISRICWISE_extract_SUIDs}}.
#' @param sim_soils A named numeric vector. First element is 'i' and second is 'depth'.
#'    The following elements represent the soil variables (currently, "density", "sand",
#'    "clay", "rock", "carbon"; see \code{\link{prepare_ExtractData_Soils}}) for each of
#'    the maximally possible soil layers (see \code{SFSW2_glovars[["slyrs_maxN"]]}).
#'    Input is an empty template.
#' @param layer_N An integer value. The number of soil layers in the dataset (i.e., 5,
#'    see \code{\link{do_ExtractSoilDataFromISRICWISE_Global}}).
#' @param layer_Nsim An integer value. The number of soil layers of a \code{rSFSW2} project
#'    representing the dataset (i.e., 6, see
#'    \code{\link{do_ExtractSoilDataFromISRICWISE_Global}}).
#' @param ldepth An integer vector. The depth limits of the extracted \code{rSFSW2}
#'    project soil layers including zero where \code{layer_Nsim + 1 == length(ldepth)}.
#' @param dat_wise A data.frame representing the main ISRIC-WISE database.
#' @param nvars An integer value. The number of soil variables extracted from a ISRIC-WISE
#'    dataset (currently, 5; see \code{\link{prepare_ExtractData_Soils}}).
#' @param var_tags A vector of character string. The column names for 'suid', bulk
#'    density ('density'), 'sand', 'clay', coarse fragments ('rock'), and organic carbon
#'    content ('carbon').
#' @param val_rocks An integer vector. The (negative) values which correspond to rocky
#'    outcrops and/or rocky subsoils. Soil depth is limited to above layers that have
#'    values in \code{val_rocks}.
#'
ISRICWISE_calc_weightedMeanForSimulationCell <- function(i, i_sim_cells_SUIDs,
  sim_soils, layer_N, layer_Nsim, ldepth, dat_wise, nvars, var_tags, val_rocks = NULL) {

  sim_soils["i"] <- i

  #Do calculations if any soils in this simulation cell
  if (i_sim_cells_SUIDs["SUIDs_N"] > 0) {
    soil_labels <- names(sim_soils)
    temp <- grep("suid", names(var_tags), invert = TRUE)
    var_labels <- names(var_tags)[temp]
    var_ids <- unlist(var_tags)[temp]

    # Init
    # sim_frac = fraction of how much this simulation cell is covered with suids and prids
    #   that have a depth > 0 cm
    sim_frac <- 0
    # simlyr_frac = fraction of each soil layer x variable covering this simulation cell
    simlyr_frac <- rep(0, times = length(sim_soils))
    PRIDs_N <- 0
    PRIDs <- PRIDs_frac <- NULL
    sim_soils["depth"] <- 0

    this_simCell <- c(as.list(i_sim_cells_SUIDs),
      soils = list(t(sapply(i_sim_cells_SUIDs[["SUID"]], FUN = ISRICWISE_get_prids,
      dat_wise = dat_wise, layer_N = layer_N, colname_suid = var_tags[["suid"]]))))

    # loop through the suids within this simulation cell; each suid may be composed of
    #   several prids
    for (k in seq_len(this_simCell[["SUIDs_N"]])) {
      this_soil <- this_simCell[["soils"]][k, ]

      if (this_soil[["PRIDs_N"]] > 0) {
        # Vector of the fractions of each prid in relation to the simulation cell
        prids_frac <- this_soil[["fraction"]] * this_simCell[["fraction"]][k]
        PRIDs_frac <- c(PRIDs_frac, prids_frac)
        temp <- sum(ifelse(is.na(this_soil[["depth"]]), 0, prids_frac))
        sim_frac <- sim_frac + temp
        temp <- sum(max_depth_byPRIDs(this_soil, var_ids, val_rocks) * prids_frac,
          na.rm = TRUE)
        sim_soils["depth"] <- sim_soils["depth"] + temp

        if (!all(is.na(this_soil[["depth"]]))) for (ils in seq_len(layer_Nsim)) {
          # Split wise soil layer 0-20 cm into two layers, 0-10 and 10-20 cm, to account
          # for lithosols
          lwise <- if (ils == 1) 1 else {ils - 1}
          # Checks if for each prid, the soils are deeper than this layer. It also accounts
          # that soil depth for Rock outcrops (RK) is set to 0 instead of < 0 for such as
          # water and glaciers. Lithosols (Ix) have depth of 10 cm.
          layer.there <- this_soil[["depth"]] > ldepth[ils]
          pfracl <- prids_frac[layer.there]

          if (sum(layer.there, na.rm = TRUE) > 0) {
            irow <- this_soil[["soildat"]][, "Layer"] == paste0("D", lwise)

            if (sum(irow) > 0) for (iv in seq_along(var_labels)) {
              ids <- which(soil_labels == paste0(var_labels[iv], "_L", ils))
              temp <- ISRICWISE_get_SoilDatValuesForLayer(dat = sim_soils[ids],
                soildat_rows = this_soil[["soildat"]][irow, var_ids[iv]], frac = pfracl)
              sim_soils[ids] <- temp[["soil"]]
              simlyr_frac[ids] <- simlyr_frac[ids] + temp[["frac"]]
            }
          }
        }
      }
    }

    #Adjust values for area present
    fracs <- c(1, sim_frac, simlyr_frac[-(1:2)])
    sim_soils <- sim_soils / fracs
  }

  sim_soils
}


ISRICWISE_try_weightedMeanForSimulationCell <- function(i, sim_cells_SUIDs,
  template_simulationSoils, layer_N, layer_Nsim, ldepth, dat_wise = dat_wise,
  nvars, var_tags, val_rocks) {

  if (i %% 1000 == 0)
    print(paste(Sys.time(), "'ISRICWISE_try_weightedMeanForSimulationCell' done:", i))

  temp <- try(ISRICWISE_calc_weightedMeanForSimulationCell(i,
    i_sim_cells_SUIDs = sim_cells_SUIDs[i, ], sim_soils = template_simulationSoils,
    layer_N = layer_N, layer_Nsim = layer_Nsim, ldepth = ldepth, dat_wise = dat_wise,
    nvars = nvars, var_tags = var_tags, val_rocks = val_rocks))

  if (inherits(temp, "try-error")) template_simulationSoils else temp
}


#' Extract soil data from one of the ISRIC-WISE datasets
#'
#' @param dataset A character string. Identifies the ISRIC-WISE dataset from which
#'  soil data are extracted. See details.
#'
#' @section Details: \code{dataset} is current implemented for values: \describe{
#'    \item{"ISRICWISEv12"}{Dataset 'ISRIC-WISE 5-arcmin v1.2' (Batjes 2012)}
#'    \item{"ISRICWISE30secV1a"}{Dataset 'ISRIC-WISE 30-arcsec v1.0' (Batjes 2015, 2016)}
#'  }
#'
#' @references Batjes, N. H. 2012. ISRIC-WISE derived soil properties on a 5 by 5
#'  arc-minutes global grid (ver. 1.2). Report 2012/01 (with data set, available at
#'  www.isric.org). ISRIC-World Soil Information, Wageningen, The Netherlands.
#'  http://www.isric.org/data/isric-wise-derived-soil-properties-5-5-arc-minutes-global-grid-version-12
#' @references Batjes N.H. 2016. Harmonised soil property values for broad-scale
#'   modelling (WISE30sec) with estimates of global soil carbon stocks. Geoderma 269,
#'   61-68 (http://dx.doi.org/10.1016/j.geoderma.2016.01.034).
#' @references Batjes N.H. 2015. World soil property estimates for broad-scale modelling
#'   (WISE30sec, ver. 1.0). Report 2015/01, ISRIC—World Soil Information, Wageningen
#'   [available at ISRIC Soil Data Hub](http://geonode.isric.org/search/?title__icontains=World%20soil%20property%20estimates%20for%20broad-scale%20modelling%20(WISE30sec)&limit=100&offset=0),
#'   with addendum and corrigendum.
#'
#' @section Note: Cells with negative soil values indicate non-soils and are eliminated
#'   before aggregations: \enumerate{
#'   \item 1) ISRIC-WISE 5-arcmin v1.2 (Batjes 2012) \itemize{
#'       \item -1: Oceans and inland waters (SUIDS %in% c(0, 1972, 6997))
#'       \item -2: Glaciers and snow caps (SUIDS %in% 6998)
#'       \item -3: No/insufficient data
#'       \item -7: Rock outcrops (or shallow subsoils) (SUIDS %in% 6694)
#'     }
#'   \item 2) ISRIC-WISE 30-arcsec v1.0 (Batjes 2015) \itemize{
#'       \item -1: Oceans and inland waters
#'       \item -2: Glaciers and snow caps
#'       \item -3: Rock outcrops
#'       \item -4: Dunes/Shifting sands
#'       \item -5: Salt flats
#'       \item -7: 'Rocky' subsoils as for Leptosols
#'       \item -9: Remaining miscellaneous units
#'     }
#'   }
do_ExtractSoilDataFromISRICWISE_Global <- function(MMC, sim_size, sim_space,
  dir_ex_soil, fnames_in, dataset = c("ISRICWISEv12", "ISRICWISE30secV1a"), resume,
  verbose) {

  dataset <- match.arg(dataset)

  if (verbose) {
    t1 <- Sys.time()
    temp_call <- shQuote(match.call()[1])
    print(paste0("rSFSW2's ", temp_call, ": started at ", t1, " for dataset ",
      shQuote(dataset)))

    on.exit(enable_debug_dump(file_tag = match.call()[[1]]), add = FALSE)

    on.exit({print(paste0("rSFSW2's ", temp_call, ": ended after ",
      round(difftime(Sys.time(), t1, units = "secs"), 2), " s")); cat("\n")}, add = TRUE)
  }

  stopifnot(requireNamespace("raster"), requireNamespace("rgdal"))

  MMC[["idone"]][dataset] <- FALSE
  todos <- is.na(MMC[["source"]]) | MMC[["source"]] == paste0(dataset, "_Global")

  if (resume) {
    todos <- adjust_soils_todos(todos, MMC, sim_size)
  }
  names(todos) <- NULL
  n_extract <- sum(todos)

  if (n_extract > 0) {
    if (verbose)
      print(paste("Soil data from", shQuote(dataset), "will be extracted for n =",
        n_extract, "sites"))

    ldepth_WISE <- if (dataset == "ISRICWISEv12") {
        c(0, 10, 20, 40, 60, 80, 100)  #in cm
      } else if (dataset == "ISRICWISE30secV1a") {
        c(0, 10, 20, 40, 60, 80, 100, 150, 200) # in cm
      }
    layer_Nsim <- length(ldepth_WISE) - 1L  #WISE contains five soil layers for each prid; I added one layer to account for lithosols (Ix), which have a depth of 10 cm; for all other soil types, my layers 0-10 cm and 10-20 cm contain the same wise information
    layer_N <- layer_Nsim - 1L  #WISE contains five soil layers for each prid

    #run_sites_wise of simulation runs
    run_sites_wise <- sim_space[["run_sites"]][todos, ]
    is_ToDo <- seq_along(run_sites_wise)

    #---extract data
    if (dataset == "ISRICWISEv12") {
      rat_att <- NULL
      var_tags <- list(suid = "SUID", density = "BULK", sand = "SDTO", clay = "CLPC",
        rock = "CFRAG", carbon = "TOTC")
      val_rocks <- -7

      dir.ex.dat <- file.path(dir_ex_soil, "WISE", "wise5by5min_v1b")
      fwise_grid <- file.path(dir.ex.dat, "Grid", "smw5by5min")
      fwise_table <- file.path(dir.ex.dat, "WISEsummaryFile.csv")

    } else if (dataset == "ISRICWISE30secV1a") {
      rat_att <- "NEWSUID"
      var_tags <- list(suid = "NEWSUID", density = "BULK", sand = "SDTO", clay = "CLPC",
        rock = "CFRAG", carbon = "ORGC")
      val_rocks <- c(-3, -7)

      dir.ex.dat <- file.path(dir_ex_soil, "WISE", "WISE30sec_v1a")
      fwise_grid <- file.path(dir.ex.dat, "GISfiles", "wise30sec_fin")
      fwise_table <- file.path(dir.ex.dat, "Interchangeable_format", "HW30s_FULL.txt")
    }

    stopifnot(file.exists(fwise_grid), file.exists(fwise_table))
    grid_wise <- raster::raster(fwise_grid)
    dat_wise <- utils::read.csv(file = fwise_table, stringsAsFactors = FALSE)

    #- List all the wise cells that are covered by the grid cell or point location
    if (verbose) {
      print(paste0("rSFSW2's ", temp_call, " for dataset ", shQuote(dataset),
        ": extract WISE dataset identifiers for simulation cell/point locations"))
    }

    if (sim_space[["scorp"]] == "point") {
      cell_res_wise <- NULL
      suids <- raster::extract(grid_wise, run_sites_wise)
      if (!is.null(rat_att)) {
        suids <- raster::factorValues(grid_wise, temp, att = rat_att)
      }
      sim_cells_SUIDs <- data.frame(i = is_ToDo, SUIDs_N = 1, SUID = suids, fraction = 1)

    } else if (sim_space[["scorp"]] == "cell") {
      cell_res_wise <- align_with_target_res(res_from = sim_space[["sim_res"]],
        crs_from = sim_space[["sim_crs"]], sp = run_sites_wise,
        crs_sp = sim_space[["crs_sites"]], crs_to = raster::crs(grid_wise))

      if (SFSW2_glovars[["p_has"]]) {

        #call the simulations depending on parallel backend
        if (identical(SFSW2_glovars[["p_type"]], "mpi")) {

          sim_cells_SUIDs <- Rmpi::mpi.applyLB(X = is_ToDo,
            FUN = ISRICWISE_extract_SUIDs, res = cell_res_wise, grid = grid_wise,
            sp_sites = run_sites_wise, att = rat_att)

        } else if (identical(SFSW2_glovars[["p_type"]], "socket")) {
          sim_cells_SUIDs <- parallel::clusterApplyLB(SFSW2_glovars[["p_cl"]], x = is_ToDo,
            fun = ISRICWISE_extract_SUIDs, res = cell_res_wise, grid = grid_wise,
            sp_sites = run_sites_wise, att = rat_att)

        } else {
          sim_cells_SUIDs <- data.frame(i = is_ToDo, SUIDs_N = 0, SUID = NA, fraction = 1)
        }

        clean_SFSW2_cluster()

      } else {
        sim_cells_SUIDs <- lapply(is_ToDo, FUN = ISRICWISE_extract_SUIDs,
          res = cell_res_wise, grid = grid_wise, sp_sites = run_sites_wise,
          att = rat_att)
      }
    }

    sim_cells_SUIDs <- do.call(rbind, sim_cells_SUIDs)
    if ("i" %in% dimnames(sim_cells_SUIDs)[[1]]) {
      sim_cells_SUIDs <- t(sim_cells_SUIDs)
    }
    stopifnot("i" %in% dimnames(sim_cells_SUIDs)[[2]])
    sim_cells_SUIDs <- sim_cells_SUIDs[order(unlist(sim_cells_SUIDs[, "i"])), ]

    #- Calculate simulation cell wide weighted values based on each PRID weighted by
    #   SUID.fraction x PRIP.PROP
    if (verbose) {
      print(paste0("rSFSW2's ", temp_call, " for dataset ", shQuote(dataset),
        ": calculate simulation cell/point location weighted values of soil characteristics"))
    }

    template_simulationSoils <- unlist(MMC[["data"]][1, ])
    template_simulationSoils[] <- NA
    template_simulationSoils["depth"] <- 0

    if (SFSW2_glovars[["p_has"]]) {
      #call the simulations depending on parallel backend
      if (identical(SFSW2_glovars[["p_type"]], "mpi")) {

        ws <- Rmpi::mpi.applyLB(X = is_ToDo,
          FUN = ISRICWISE_try_weightedMeanForSimulationCell,
          sim_cells_SUIDs = sim_cells_SUIDs,
          template_simulationSoils = template_simulationSoils,
          layer_N = layer_N, layer_Nsim = layer_Nsim, ldepth = ldepth_WISE,
          dat_wise = dat_wise, nvars = MMC[["nvars"]], var_tags = var_tags,
          val_rocks = val_rocks)

      } else if (identical(SFSW2_glovars[["p_type"]], "socket")) {

        ws <- parallel::clusterApplyLB(SFSW2_glovars[["p_cl"]], x = is_ToDo,
          fun = ISRICWISE_try_weightedMeanForSimulationCell,
          sim_cells_SUIDs = sim_cells_SUIDs,
          template_simulationSoils = template_simulationSoils,
          layer_N = layer_N, layer_Nsim = layer_Nsim, ldepth = ldepth_WISE,
          dat_wise = dat_wise, nvars = MMC[["nvars"]], var_tags = var_tags,
          val_rocks = val_rocks)
      }

      clean_SFSW2_cluster()

    } else {
      ws <- lapply(is_ToDo, FUN = ISRICWISE_try_weightedMeanForSimulationCell,
        sim_cells_SUIDs = sim_cells_SUIDs,
        template_simulationSoils = template_simulationSoils,
        layer_N = layer_N, layer_Nsim = layer_Nsim, ldepth = ldepth_WISE,
        dat_wise = dat_wise, nvars = MMC[["nvars"]], var_tags = var_tags,
        val_rocks = val_rocks)
    }

    ws <- do.call(rbind, ws)
    ws <- ws[order(ws[, "i"]), ]
    # convert percent to fraction
    icol <- grepl("rock", colnames(ws)) |
      grepl("sand", colnames(ws)) | grepl("clay", colnames(ws))
    ws[, icol] <- ws[, icol] / 100

    #Convert bulk density to matric density
    #  eqn. 20 from Saxton et al. 2006: bulkd <- matricd * (1 - rockvol) + rockvol * 2.65
    # 'bulk density' here is of the matric component, i.e., what we call matric density
    #matricd <- (ws[, grep("bulk", colnames(ws))] - 2.65 * ws[, grep("rock", colnames(ws))]) / (1 - ws[, grep("rock", colnames(ws))])

    i_good <- rep(FALSE, n_extract)
    ids <- seq_len(2 + layer_Nsim * MMC[["nvars"]])
    i_good[ws[stats::complete.cases(ws[, ids]), "i"]] <- TRUE # i is index for todos
    MMC[["source"]][which(todos)[!i_good]] <- NA

    if (verbose) {
      print(paste0("rSFSW2's ", temp_call, " for dataset ", shQuote(dataset),
        ": soil data was extracted for n = ", sum(i_good), " out of ", n_extract, " sites"))
    }

    if (any(i_good)) {
      MMC[["idone"]][dataset] <- TRUE
      i_Done <- rep(FALSE, times = sim_size[["runsN_sites"]]) #length(i_Done) == length(runIDs_sites) == runsN_sites
      i_Done[which(todos)[i_good]] <- TRUE #sum(i_Done) == sum(i_good)
      MMC[["data"]][todos, seq_len(dim(ws)[2])] <- ws

      MMC[["source"]][i_Done] <- paste0(dataset, "_Global")

      if (verbose) {
        print(paste0("rSFSW2's ", temp_call, " for dataset ", shQuote(dataset),
          ": update soil input files"))
      }
      MMC <- update_soils_input(MMC, sim_size, digits = 2, i_Done,
        ldepths_cm = ldepth_WISE[-1], lys = seq_len(layer_Nsim), fnames_in)
    }
  }

  if (verbose) {
    # Remove debug dumping but not other 'on.exit' expressions before returning without error
    oe <- sys.on.exit()
    oe <- remove_from_onexit_expression(oe, "enable_debug_dump")
    on.exit(eval(oe), add = FALSE)
  }

  MMC
}



#' Extract soil characteristics
#' @export
ExtractData_Soils <- function(exinfo, SFSW2_prj_meta, SFSW2_prj_inputs, opt_parallel,
  resume, verbose = FALSE) {

  field_sources <- "SoilTexture_source"
  field_include <- "Include_YN_SoilSources"

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


  MMC <- prepare_ExtractData_Soils(SFSW2_prj_inputs[["SWRunInformation"]],
    sim_size = SFSW2_prj_meta[["sim_size"]], field_sources = field_sources,
    how_determine_sources = SFSW2_prj_meta[["opt_input"]][["how_determine_sources"]],
    sw_input_soillayers = SFSW2_prj_inputs[["sw_input_soillayers"]],
    sw_input_soils_use = SFSW2_prj_inputs[["sw_input_soils_use"]],
    sw_input_soils = SFSW2_prj_inputs[["sw_input_soils"]])

  if (exinfo$ExtractSoilDataFromCONUSSOILFromSTATSGO_USA) {
    MMC <- do_ExtractSoilDataFromCONUSSOILFromSTATSGO_USA(MMC,
      sim_size = SFSW2_prj_meta[["sim_size"]], sim_space = SFSW2_prj_meta[["sim_space"]],
      dir_ex_soil = SFSW2_prj_meta[["project_paths"]][["dir_ex_soil"]],
      fnames_in = SFSW2_prj_meta[["fnames_in"]], resume, verbose)
  }

  if (exinfo$ExtractSoilDataFromISRICWISE30secV1a_Global) {
    MMC <- do_ExtractSoilDataFromISRICWISE_Global(MMC,
      sim_size = SFSW2_prj_meta[["sim_size"]], sim_space = SFSW2_prj_meta[["sim_space"]],
      dir_ex_soil = SFSW2_prj_meta[["project_paths"]][["dir_ex_soil"]],
      fnames_in = SFSW2_prj_meta[["fnames_in"]], dataset = "ISRICWISE30secV1a",
      resume, verbose)
  }

  if (exinfo$ExtractSoilDataFromISRICWISEv12_Global) {
    MMC <- do_ExtractSoilDataFromISRICWISE_Global(MMC,
      sim_size = SFSW2_prj_meta[["sim_size"]], sim_space = SFSW2_prj_meta[["sim_space"]],
      dir_ex_soil = SFSW2_prj_meta[["project_paths"]][["dir_ex_soil"]],
      fnames_in = SFSW2_prj_meta[["fnames_in"]], dataset = "ISRICWISEv12",
      resume, verbose)
  }

  SFSW2_prj_inputs[["SWRunInformation"]] <- update_datasource_masterfield(MMC,
    sim_size = SFSW2_prj_meta[["sim_size"]], SFSW2_prj_inputs[["SWRunInformation"]],
    SFSW2_prj_meta[["fnames_in"]], field_sources, field_include)

  SFSW2_prj_inputs[["sw_input_soillayers"]] <- MMC[["input2"]]
  SFSW2_prj_inputs[["sw_input_soils_use"]] <- MMC[["use"]]
  SFSW2_prj_inputs[["sw_input_soils"]] <- MMC[["input"]]

  oe <- sys.on.exit()
  oe <- remove_from_onexit_expression(oe, "exit_SFSW2_cluster")
  on.exit(eval(oe), add = FALSE)

  SFSW2_prj_inputs
}


#------END OF SOIL CHARACTERISTICS------
#----------------------------------------------------------------------------------------#

