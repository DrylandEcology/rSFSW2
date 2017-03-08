#---------------------------------------------------------------------------------------#
#------EXTRACT SOIL CHARACTERISTICS------

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
  vars <- data.frame(input = c("SoilDepth_cm", "Matricd_L", "GravelContent_L", "Sand_L",
    "Clay_L", "TOC_GperKG_L"), intern = c("depth", lvars), stringsAsFactors = FALSE)

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
#' @references Miller, D. A. and R. A. White. 1998. A conterminous United States
#'  multilayer soil characteristics dataset for regional climate and hydrology modeling.
#'  Earth Interactions 2:1-26.
#' @section Note(drs): it appears that NODATA values are recorded as 0
#'
do_ExtractSoilDataFromCONUSSOILFromSTATSGO_USA <- function(MMC, sim_size, sim_space,
  dir_ex_soil, fnames_in, resume, verbose) {

  if (verbose) {
    t1 <- Sys.time()
    temp_call <- shQuote(match.call()[1])
    print(paste0("rSFSW2's ", temp_call, ": started at ", t1))

    on.exit({print(paste0("rSFSW2's ", temp_call, ": ended after ",
      round(difftime(Sys.time(), t1, units = "secs"), 2), " s")); cat("\n")}, add = TRUE)
  }

  stopifnot(requireNamespace("raster"), requireNamespace("sp"), requireNamespace("rgdal"))

  MMC[["idone"]]["CONUSSOIL1"] <- FALSE
  todos <- is.na(MMC[["source"]]) |  MMC[["source"]] == "CONUSSOILFromSTATSGO_USA"

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
    sand <- ifelse(is.finite(sand), sand, NA) / total_matric / 100 # mass fraction of matric component
    MMC[["data"]][todos, grep("sand", MMC[["cn"]])[ils]] <- sand
    clay <- ifelse(is.finite(clay), clay, NA) / total_matric / 100 # mass fraction of matric component
    MMC[["data"]][todos, grep("clay", MMC[["cn"]])[ils]] <- clay

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


#' A wrapper for \code{reaggregate_raster} design to work with raster data from ISRIC-WISE
#'
#' @param i An integer value. The index to select a location from among \code{sp_sites} and the corresponding resolution \code{res}.
#' @param res A numeric vector of length two or a matrix with two columns. The x- and y-extent of the rectangle(s) for which to extract values.
#' @param grid A \linkS4class{RasterLayer} object with one layer. The raster from which values are extracted.
#' @param sp_sites A \linkS4class{SpatialPoints} object. This object is used to extract the coordinates of the i-th location.
#'
#' @seealso \code{\link{reaggregate_raster}}
#'
#' @return A list with four elements
#'  \describe{
#'    \item{i}{An integer value. The location index.}
#'    \item{SUIDs_N}{An integer vector. The number of unique values within the rectangle of \code{x}.}
#'    \item{SUID}{A numeric vectors. The sorted unique values.}
#'    \item{fraction}{A numeric vector. The relative areas covered by \code{values}.}
#'  }
ISRICWISE12_extract_SUIDs <- function(i, res = c(0, 0), grid, sp_sites) {
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
    list(i = i, SUIDs_N = out[[1]][["N"]][[1]],
          SUID = out[[1]][["values"]][[1]],
          fraction = out[[1]][["fraction"]][[1]])
  }
}

ISRICWISE12_get_prids <- function(suid, dat_wise, layer_N) {
  soils <- dat_wise[dat_wise$SUID == suid, ]
  frac <- unique(soils[, c("PROP", "PRID")])
  depth <- tapply(soils$BotDep, soils$PRID, max)
  idepth <- depth[match(frac$PRID, names(depth))]

  list(PRIDs_N = nrow(soils) / layer_N,
     PRID = frac$PRID,
     fraction = frac$PROP / 100,
     depth = ifelse(idepth > 0, idepth, NA),
     soildat = soils)
}

ISRICWISE12_get_SoilDatValuesForLayer <- function(dat, soildat_rows, frac) {
  sum(soildat_rows * frac, dat, na.rm = TRUE) #weighted mean = sum of values x weights
}

#' Calculate the weighted mean soil variables from ISRIC-WISE v1.2 for one simulation cell
#'
#' @param i An integer value. The cell number.
#' @param i_sim_cells_SUIDs A named numeric vector. A row of of the data.frame returned
#'    by \code{\link{ISRICWISE12_extract_SUIDs}}.
#' @param sim_soils A named numeric vector. First element is 'i' and second is 'depth'.
#'    The following elements represent the soil variables (currently, "density", "sand",
#'    "clay", "rock", "carbon"; see \code{\link{prepare_ExtractData_Soils}}) for each of
#'    the maximally possible soil layers (see \code{SFSW2_glovars[["slyrs_maxN"]]}).
#'    Input is an empty template.
#' @param layer_N An integer value. The number of soil layers in the ISRIC-WISE v1.2
#'    dataset (i.e., 5, see \code{\link{do_ExtractSoilDataFromISRICWISEv12_Global}}).
#' @param layer_Nsim An integer value. The number of soil layers of a \code{rSFSW2} project
#'    representing the ISRIC-WISE v1.2 dataset (i.e., 6, see
#'    \code{\link{do_ExtractSoilDataFromISRICWISEv12_Global}}).
#' @param ldepth An integer vector. The depth limits of the extracted \code{rSFSW2}
#'    project soil layers including zero where \code{layer_Nsim + 1 == length(ldepth)}.
#' @param dat_wise A data.frame representing the disk file \code{WISEsummaryFile.csv}.
#' @param nvars An integer value. The number of soil variables extracted from ISRIC-WISE
#'    v1.2 (currently, 5; see \code{\link{prepare_ExtractData_Soils}}).
ISRICWISE12_calc_weightedMeanForSimulationCell <- function(i, i_sim_cells_SUIDs,
  sim_soils, layer_N, layer_Nsim, ldepth, dat_wise, nvars) {

  #Init
  sim_soils["i"] <- i
  sim_frac <- 0  #fraction of how much this simulation cell is covered with suids and prids that have a depth > 0 cm
  simlyr_frac <- rep(0, times = layer_Nsim) #fraction of each soil layer covering this simulation cell
  PRIDs_N <- 0
  PRIDs <- PRIDs_frac <- NULL

  #Do calculations if any soils in this simulation cell
  if (i_sim_cells_SUIDs["SUIDs_N"] > 0) {
    this_simCell <- c(as.list(i_sim_cells_SUIDs),
      soils = list(t(sapply(i_sim_cells_SUIDs["SUID"], FUN = ISRICWISE12_get_prids,
      dat_wise = dat_wise, layer_N = layer_N))))

    # loop through the suids within this simulation cell; each suid may be composed of
    # several prids
    for (k in seq_len(this_simCell$SUIDs_N)) {

      this_soil <- this_simCell$soils[k, ]
      # Vector of the fractions of each prid in relation to the simulation cell
      prids_frac <- this_soil$fraction * this_simCell$fraction[k]
      PRIDs_frac <- c(PRIDs_frac, prids_frac)
      temp <- sum(ifelse(is.na(this_soil$depth), 0, prids_frac))
      sim_frac <- sim_frac + temp
      temp <- sum(this_soil$depth * prids_frac, na.rm = TRUE)
      sim_soils["depth"] <- sim_soils["depth"] + temp

      if (!all(is.na(this_soil$depth))) for (ils in seq_len(layer_Nsim)) {
        # Split wise soil layer 0-20 cm into two layers, 0-10 and 10-20 cm, to account
        # for lithosols
        lwise <- if (ils == 1) 1 else {ils - 1}
        # Checks if for each prid, the soils are deeper than this layer. It also accounts
        # that soil depth for Rock outcrops (RK) is set to 0 instead of < 0 for such as
        # water and glaciers. Lithosols (Ix) have depth of 10 cm.
        layer.there <- this_soil$depth > ldepth[ils]
        pfracl <- prids_frac[layer.there]
        simlyr_frac[ils] <- simlyr_frac[ils] + sum(pfracl, na.rm = TRUE)

        if (sum(layer.there, na.rm = TRUE) > 0) {
          irow <- lwise + ((0:(this_soil$PRIDs_N - 1)) * layer_N)[layer.there]

          # bulk density (kg/dm3)
          ids <- paste0("density_L", ils)
          sim_soils[ids] <- ISRICWISE12_get_SoilDatValuesForLayer(dat = sim_soils[ids],
            soildat_rows = this_soil$soildat[irow, "BULK"], frac = pfracl)
          # Sand mass (%)
          ids <- paste0("sand_L", ils)
          sim_soils[ids] <- ISRICWISE12_get_SoilDatValuesForLayer(dat = sim_soils[ids],
            soildat_rows = this_soil$soildat[irow, "SDTO"], frac = pfracl)
          # clay mass (%)
          ids <- paste0("clay_L", ils)
          sim_soils[ids] <- ISRICWISE12_get_SoilDatValuesForLayer(dat = sim_soils[ids],
            soildat_rows = this_soil$soildat[irow, "CLPC"], frac = pfracl)
          # coarse fragments (vol % > 2 mm)
          ids <- paste0("rock_L", ils)
          sim_soils[ids] <- ISRICWISE12_get_SoilDatValuesForLayer(dat = sim_soils[ids],
            soildat_rows = this_soil$soildat[irow, "CFRAG"], frac = pfracl)
          # total organic carbon content (g C / kg)
          ids <- paste0("carbon_L", ils)
          sim_soils[ids] <- ISRICWISE12_get_SoilDatValuesForLayer(dat = sim_soils[ids],
            soildat_rows = this_soil$soildat[irow, "TOTC"], frac = pfracl)
        }
      }
    }

    #Adjust values for area present
    fracs <- c(1, sim_frac, rep(simlyr_frac, each = nvars))
    ids <- seq_along(fracs)
    sim_soils[ids] <- sim_soils[ids] / fracs
  }

  sim_soils
}


ISRICWISE12_try_weightedMeanForSimulationCell <- function(i, sim_cells_SUIDs,
  template_simulationSoils, layer_N, layer_Nsim, ldepth, dat_wise = dat_wise,
  nvars) {

  if (i %% 1000 == 0) print(paste(Sys.time(), "done:", i))

  temp <- try(ISRICWISE12_calc_weightedMeanForSimulationCell(i,
        i_sim_cells_SUIDs = sim_cells_SUIDs[i, ],
        sim_soils = template_simulationSoils,
        layer_N = layer_N, layer_Nsim = layer_Nsim, ldepth = ldepth, dat_wise = dat_wise,
        nvars = nvars))

  if (inherits(temp, "try-error")) template_simulationSoils else temp
}



#' @references Batjes, N. H. 2012. ISRIC-WISE derived soil properties on a 5 by 5
#'  arc-minutes global grid (ver. 1.2). Report 2012/01 (with data set, available at
#'  www.isric.org). ISRIC-World Soil Information, Wageningen, The Netherlands.
#'  http://www.isric.org/data/isric-wise-derived-soil-properties-5-5-arc-minutes-global-grid-version-12
#' @section Note: Cells with no soil values include those with \code{SUID = c(0 = Water,
#'  6997 = Water, 6694 = Rock, or 6998 = Glacier)}
do_ExtractSoilDataFromISRICWISEv12_Global <- function(MMC, sim_size, sim_space,
  dir_ex_soil, fnames_in, opt_parallel, resume, verbose) {

  if (verbose) {
    t1 <- Sys.time()
    temp_call <- shQuote(match.call()[1])
    print(paste0("rSFSW2's ", temp_call, ": started at ", t1))

    on.exit({print(paste0("rSFSW2's ", temp_call, ": ended after ",
      round(difftime(Sys.time(), t1, units = "secs"), 2), " s")); cat("\n")}, add = TRUE)
  }
  stopifnot(requireNamespace("raster"), requireNamespace("rgdal"))

  MMC[["idone"]]["ISRICWISEv12"] <- FALSE
  todos <- is.na(MMC[["source"]]) | MMC[["source"]] == "ISRICWISEv12_Global"

  if (resume) {
    todos <- adjust_soils_todos(todos, MMC, sim_size)
  }
  names(todos) <- NULL
  n_extract <- sum(todos)

  if (n_extract > 0) {
    if (verbose)
      print(paste("Soil data from 'ISRICWISEv12_Global' will be extracted for n =",
        n_extract, "sites"))

    ldepth_WISEv12 <- c(0, 10, 20, 40, 60, 80, 100)  #in cm
    layer_Nsim <- length(ldepth_WISEv12) - 1L  #WISE contains five soil layers for each prid; I added one layer to account for lithosols (Ix), which have a depth of 10 cm; for all other soil types, my layers 0-10 cm and 10-20 cm contain the same wise information
    layer_N <- layer_Nsim - 1L  #WISE contains five soil layers for each prid

    dir.ex.dat <- file.path(dir_ex_soil, "WISE", "wise5by5min_v1b")
    stopifnot(file.exists(dir.ex.dat))

    #run_sites_wise of simulation runs
    run_sites_wise <- sim_space[["run_sites"]][todos, ]
    is_ToDo <- seq_along(run_sites_wise)

    #---extract data
    grid_wise <- raster::raster(file.path(dir.ex.dat, "Grid", "smw5by5min"))

    #- List all the wise cells that are covered by the grid cell or point location
    if (sim_space[["scorp"]] == "point") {
      cell_res_wise <- NULL
      suids <- raster::extract(grid_wise, run_sites_wise)
      sim_cells_SUIDs <- data.frame(i = is_ToDo, SUIDs_N = 1, SUID = suids, fraction = 1)

    } else if (sim_space[["scorp"]] == "cell") {
      cell_res_wise <- align_with_target_res(res_from = sim_space[["sim_res"]],
        crs_from = sim_space[["sim_crs"]], sp = run_sites_wise,
        crs_sp = sim_space[["crs_sites"]], crs_to = raster::crs(grid_wise))

      if (opt_parallel[["has_parallel"]]) {

        #call the simulations depending on parallel backend
        if (identical(opt_parallel[["parallel_backend"]], "mpi")) {

          sim_cells_SUIDs <- Rmpi::mpi.applyLB(X = is_ToDo,
            FUN = ISRICWISE12_extract_SUIDs, res = cell_res_wise, grid = grid_wise,
            sp_sites = run_sites_wise)

          Rmpi::mpi.bcast.cmd(rm(list = ls()))
          Rmpi::mpi.bcast.cmd(gc())

        } else if (identical(opt_parallel[["parallel_backend"]], "cluster")) {

          sim_cells_SUIDs <- parallel::clusterApplyLB(opt_parallel[["cl"]], x = is_ToDo,
            fun = ISRICWISE12_extract_SUIDs, res = cell_res_wise, grid = grid_wise,
            sp_sites = run_sites_wise)

          parallel::clusterEvalQ(opt_parallel[["cl"]], rm(list = ls()))
          parallel::clusterEvalQ(opt_parallel[["cl"]], gc())

        } else {
          sim_cells_SUIDs <- NULL
        }

      } else {
        sim_cells_SUIDs <- lapply(is_ToDo, FUN = ISRICWISE12_extract_SUIDs,
          res = cell_res_wise, grid = grid_wise, sp_sites = run_sites_wise)
      }
    }

    sim_cells_SUIDs <- t(do.call(rbind, sim_cells_SUIDs))
    sim_cells_SUIDs <- sim_cells_SUIDs[order(unlist(sim_cells_SUIDs[, "i"])), ]

    #- Calculate simulation cell wide weighted values based on each PRID weighted by SUID.fraction x PRIP.PROP
    dat_wise <- utils::read.csv(file = file.path(dir.ex.dat, "WISEsummaryFile.csv"))

    template_simulationSoils <- unlist(MMC[["data"]][1, ])
    template_simulationSoils[] <- NA
    template_simulationSoils["depth"] <- 0

    if (opt_parallel[["has_parallel"]]) {
      #call the simulations depending on parallel backend
      if (identical(opt_parallel[["parallel_backend"]], "mpi")) {

        ws <- Rmpi::mpi.applyLB(X = is_ToDo,
          FUN = ISRICWISE12_try_weightedMeanForSimulationCell,
          sim_cells_SUIDs = sim_cells_SUIDs,
          template_simulationSoils = template_simulationSoils,
          layer_N = layer_N, layer_Nsim = layer_Nsim, ldepth = ldepth_WISEv12,
          dat_wise = dat_wise, nvars = MMC[["nvars"]])

        Rmpi::mpi.bcast.cmd(rm(list = ls()))
        Rmpi::mpi.bcast.cmd(gc())

      } else if (identical(opt_parallel[["parallel_backend"]], "cluster")) {

        ws <- parallel::clusterApplyLB(opt_parallel[["cl"]], x = is_ToDo,
          fun = ISRICWISE12_try_weightedMeanForSimulationCell,
          sim_cells_SUIDs = sim_cells_SUIDs,
          template_simulationSoils = template_simulationSoils,
          layer_N = layer_N, layer_Nsim = layer_Nsim, ldepth = ldepth_WISEv12,
          dat_wise = dat_wise, nvars = MMC[["nvars"]])

        parallel::clusterEvalQ(opt_parallel[["cl"]], rm(list = ls()))
        parallel::clusterEvalQ(opt_parallel[["cl"]], gc())
      }

    } else {
      ws <- lapply(is_ToDo, FUN = ISRICWISE12_try_weightedMeanForSimulationCell,
        sim_cells_SUIDs = sim_cells_SUIDs,
        template_simulationSoils = template_simulationSoils,
        layer_N = layer_N, layer_Nsim = layer_Nsim, ldepth = ldepth_WISEv12,
        dat_wise = dat_wise, nvars = MMC[["nvars"]])
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

    if (any(i_good)) {
      MMC[["idone"]]["ISRICWISEv12"] <- TRUE
      i_Done <- rep(FALSE, times = sim_size[["runsN_sites"]]) #length(i_Done) == length(runIDs_sites) == runsN_sites
      i_Done[which(todos)[i_good]] <- TRUE #sum(i_Done) == sum(i_good)
      MMC[["data"]][todos, seq_len(dim(ws)[2])] <- ws

      MMC[["source"]][i_Done] <- "ISRICWISEv12_Global"
      MMC <- update_soils_input(MMC, sim_size, digits = 2, i_Done,
        ldepths_cm = ldepth_WISEv12[-1], lys = seq_len(layer_Nsim), fnames_in)
    }

    if (verbose)
      print(paste("Soil data from 'ISRICWISEv12_Global' was extracted for n =",
        sum(i_good), "out of", n_extract, "sites"))
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
  opt_parallel <- setup_SFSW2_cluster(opt_parallel,
    dir_out = SFSW2_prj_meta[["project_paths"]][["dir_prj"]],
    verbose = opt_verbosity[["verbose"]])
  on.exit(clean_SFSW2_cluster(opt_parallel, verbose = opt_verbosity[["verbose"]]),
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

  if (exinfo$ExtractSoilDataFromISRICWISEv12_Global) {
    MMC <- do_ExtractSoilDataFromISRICWISEv12_Global(MMC,
      sim_size = SFSW2_prj_meta[["sim_size"]], sim_space = SFSW2_prj_meta[["sim_space"]],
      dir_ex_soil = SFSW2_prj_meta[["project_paths"]][["dir_ex_soil"]],
      fnames_in = SFSW2_prj_meta[["fnames_in"]], opt_parallel, resume, verbose)
  }

  SFSW2_prj_inputs[["SWRunInformation"]] <- update_datasource_masterfield(MMC,
    sim_size = SFSW2_prj_meta[["sim_size"]], SFSW2_prj_inputs[["SWRunInformation"]],
    SFSW2_prj_meta[["fnames_in"]], field_sources, field_include)

  SFSW2_prj_inputs[["sw_input_soillayers"]] <- MMC[["input2"]]
  SFSW2_prj_inputs[["sw_input_soils_use"]] <- MMC[["use"]]
  SFSW2_prj_inputs[["sw_input_soils"]] <- MMC[["input"]]


  SFSW2_prj_inputs
}


#------END OF SOIL CHARACTERISTICS------
#----------------------------------------------------------------------------------------#

