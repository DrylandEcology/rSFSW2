
#' Add soil layers (by interpolation) if not already present
#' @export
calc_ExtendSoilDatafileToRequestedSoilLayers <- function(SFSW2_prj_meta, SFSW2_prj_inputs,
  runIDs_adjust, verbose = FALSE) {

  requested_soil_layers <- SFSW2_prj_meta[["opt_input"]][["requested_soil_layers"]]

  if (verbose) {
    t1 <- Sys.time()
    temp_call <- shQuote(match.call()[1])
    print(paste0("rSFSW2's ", temp_call, ": started at ", t1))

    on.exit({print(paste0("rSFSW2's ", temp_call, ": ended after ",
      round(difftime(Sys.time(), t1, units = "secs"), 2), " s")); cat("\n")}, add = TRUE)
  }

  # How to add different soil variables
  sl_vars_mean <- c("Matricd", "GravelContent", "Sand", "Clay", "SoilTemp") # values will be interpolated
  sl_vars_sub <- c("EvapCoeff", "TranspCoeff", "Imperm") # values will be exhausted

  # Requested layers
  requested_soil_layers <- as.integer(round(requested_soil_layers))
  stopifnot(requested_soil_layers > 0, diff(requested_soil_layers) > 0)
  req_sl_ids <- paste0(requested_soil_layers, collapse = "x")

  # Available layers
  ids_depth <- strsplit(names(SFSW2_prj_inputs[["sw_input_soils_use"]])[SFSW2_prj_inputs[["sw_input_soils_use"]]], "_", fixed = TRUE)
  stopifnot(length(ids_depth) > 0)
  var_layers <- unique(sapply(ids_depth, function(x) paste0(x[-length(x)], collapse = "_")))
  ids_depth2 <- unique(sapply(ids_depth, function(x) x[length(x)]))
  use_layers <- paste0("depth_", ids_depth2)

  layers_depth <- round(as.matrix(SFSW2_prj_inputs[["sw_input_soillayers"]][runIDs_adjust, use_layers, drop = FALSE]))
  i_nodata <- apply(is.na(layers_depth), 1, all)
  if (any(i_nodata)) {
    layers_depth <- layers_depth[!i_nodata, ]
    runIDs_adjust_ws <- runIDs_adjust[!i_nodata]
  } else {
    runIDs_adjust_ws <- runIDs_adjust
  }
  i_nodata <- apply(is.na(layers_depth), 2, all)
  if (any(i_nodata))
    layers_depth <- layers_depth[, !i_nodata]
  ids_layers <- seq_len(dim(layers_depth)[2])
  avail_sl_ids <- apply(layers_depth, 1, paste0, collapse = "x")

  # Loop through runs with same layer profile and adjust
  layer_sets <- unique(avail_sl_ids)
  if (length(layer_sets) > 0) {
    has_changed <- FALSE
    sw_input_soils_data <- lapply(var_layers, function(x)
      as.matrix(SFSW2_prj_inputs[["sw_input_soils"]][runIDs_adjust_ws,
      grep(x, names(SFSW2_prj_inputs[["sw_input_soils"]]))[ids_layers]]))
    sw_input_soils_data2 <- NULL

    for (ils in seq_along(layer_sets)) {
      il_set <- avail_sl_ids == layer_sets[ils]
      if (sum(il_set, na.rm = TRUE) == 0) next

      # Identify which requested layers to add
      ldset <- stats::na.exclude(layers_depth[which(il_set)[1], ])
      req_sl_toadd <- setdiff(requested_soil_layers, ldset)
      req_sd_toadd <- req_sl_toadd[req_sl_toadd < max(ldset)]
      if (length(req_sd_toadd) == 0) next

      # Add identified layers
      sw_input_soils_data2 <- lapply(seq_along(var_layers), function(iv)
        sw_input_soils_data[[iv]][il_set, ])
      for (lnew in req_sd_toadd) {
        ilnew <- findInterval(lnew, ldset)
        il_weight <- calc_weights_from_depths(ilnew, lnew, ldset)
        sw_input_soils_data2 <- lapply(seq_along(var_layers), function(iv)
          add_layer_to_soil(sw_input_soils_data2[[iv]], il = ilnew, w = il_weight,
            method = if (var_layers[iv] %in% sl_vars_sub) "exhaust" else "interpolate"))
        ldset <- sort(c(ldset, lnew))
      }

      # Update soil datafiles
      lyrs <- seq_along(ldset)
      for (iv in seq_along(var_layers)) {
        i.temp <- grep(var_layers[iv], names(SFSW2_prj_inputs[["sw_input_soils_use"]]))[lyrs]
        SFSW2_prj_inputs[["sw_input_soils"]][runIDs_adjust_ws[il_set], i.temp] <-
          round(sw_input_soils_data2[[iv]][, lyrs], if (var_layers[iv] %in% sl_vars_sub) 4L else 2L)
        SFSW2_prj_inputs[["sw_input_soils_use"]][i.temp] <- TRUE
      }

      SFSW2_prj_inputs[["sw_input_soillayers"]][runIDs_adjust_ws[il_set],
        grep("depth_", names(SFSW2_prj_inputs[["sw_input_soillayers"]]))[lyrs]] <- matrix(ldset,
        nrow = sum(il_set), ncol = length(ldset), byrow = TRUE)
      has_changed <- TRUE
    }

    if (has_changed) {
      #write data to disk
      utils::write.csv(SFSW2_prj_inputs[["sw_input_soillayers"]],
        file = SFSW2_prj_meta[["fnames_in"]][["fslayers"]], row.names = FALSE)
      utils::write.csv(reconstitute_inputfile(SFSW2_prj_inputs[["sw_input_soils_use"]],
        SFSW2_prj_inputs[["sw_input_soils"]]),
        file = SFSW2_prj_meta[["fnames_in"]][["fsoils"]], row.names = FALSE)
      unlink(SFSW2_prj_meta[["fnames_in"]][["fpreprocin"]])

      print(paste("'InterpolateSoilDatafileToRequestedSoilLayers': don't forget to",
        "adjust lookup tables with per-layer values if applicable for this project"))
    }

   SFSW2_prj_meta[["opt_input"]][["requested_soil_layers"]] <- requested_soil_layers
  }

  list(SFSW2_prj_meta = SFSW2_prj_meta, SFSW2_prj_inputs = SFSW2_prj_inputs)
}


#' Calculate potential bare-soil evaporation coefficients
#'
#' Soil texture influence based on re-analysis of data from Wythers et al. 1999.
#' Default of \code{depth_max_bs_evap} = 15 cm from Torres et al. 2010.
#'
#' @references Torres EA, Calera A (2010) Bare soil evaporation under high evaporation
#'  demand: a proposed modification to the FAO-56 model. Hydrological Sciences Journal-
#'  Journal Des Sciences Hydrologiques, 55, 303-315.
#'
#' @references Wythers KR, Lauenroth WK, Paruelo JM (1999) Bare-Soil Evaporation Under
#'  Semiarid Field Conditions. Soil Science Society of America Journal, 63, 1341-1349.
#'
#' @param layers_depth A numeric vector, matrix, or data.frame. Values describe the lower
#'  soil layer depths in units of centimeters.
#' @param sand A numeric vector, matrix, or data.frame. Values are sand contents in units
#'  of mass-percentage / 100.
#' @param clay A numeric vector, matrix, or data.frame. Values are clay contents in units
#'  of mass-percentage / 100.
#' @param depth_max_bs_evap_cm A numeric value. The maximal soil depth in centimeters from
#'  which bare-soil evaporation is potentially drawing moisture.
#'
#' @section Notes: Rows of soil input arguments \code{layers_depth}, \code{sand}, and
#'  \code{clay} correspond to sites and columns to soil layers. If \code{sand} and/or
#'  \code{clay} are vectors, then they are converted to 1-row matrices.
#'  If \code{layers_depth} is a vector, then it is converted to a matrix with as many
#'  sites/rows as \code{sand} and \code{clay} have. That is the code assumes identical
#'  soil layer depths for each site. All soil input arguments must have a the same number
#'  of sites and of soil layers, i.e., identical matrix dimensions.
#'
#' @return A numeric matrix with potential bare-soil evaporation coefficients where rows
#'  correspond to sites and columns to soil layers.
#'
#' @export
calc_BareSoilEvaporationCoefficientsFromSoilTexture <- function(layers_depth, sand, clay,
  depth_max_bs_evap_cm = 15) {

  #--- If inputs are not site x layers, then convert them into 1 site x layers table
  if (is.null(dim(sand))) {
    sand <- matrix(sand, nrow = 1, ncol = length(sand))
  }
  if (is.null(dim(clay))) {
    clay <- matrix(clay, nrow = 1, ncol = length(clay))
  }
  if (is.null(dim(layers_depth))) {
    layers_depth <- matrix(layers_depth, nrow = dim(sand)[1], ncol = length(layers_depth),
      byrow = TRUE)
  }

  #--- Test inputs
  # - sand and clay have identical number of sites and layers
  # - all soil inputs have identical number of sites and at least as many layers as depths
  # - soil layer depths are numeric and positive -- or NA, if all deeper layers are NA
  # - sand and clay are numeric and values between 0 and 1 -- or NA, if all deeper
  #   layers are NA as well
  # - the sum of sand and clay is less or equal to 1
  sand_and_clay <- sand + clay
  stopifnot(
    identical(dim(sand), dim(clay)),
    identical(dim(sand)[1], dim(layers_depth)[1]), dim(sand)[2] >= dim(layers_depth)[2],
    is.numeric(layers_depth), layers_depth > 0 | has_NAs_pooled_at_depth(layers_depth),
    is.numeric(unlist(sand)), sand >= 0 & sand <= 1 | has_NAs_pooled_at_depth(sand),
    is.numeric(unlist(clay)), clay >= 0 & clay <= 1 | has_NAs_pooled_at_depth(clay),
    sand_and_clay <= 1 | has_NAs_pooled_at_depth(sand_and_clay),
    is.finite(depth_max_bs_evap_cm) & depth_max_bs_evap_cm >= 0)


  #--- Calculate

  depth_min_bs_evap <- min(layers_depth[, 1], na.rm = TRUE)
  if (depth_min_bs_evap > depth_max_bs_evap_cm) {
    # all sites have first layer with coeff = 1
    res <- array(1, dim = dim(sand))
    res[, -1] <- 0
    return(res)
  }

  lyrs_max_bs_evap <- t(apply(layers_depth, 1, function(x) {
    xdm <- depth_max_bs_evap_cm - x
    i0 <- abs(xdm) < SFSW2_glovars[["tol"]]
    ld <- if (any(i0, na.rm = TRUE)) {
      which(i0)
    } else {
      temp <- which(xdm < 0)
      if (length(temp) > 0) temp[1] else length(x)
    }
    c(diff(c(0, x))[seq_len(ld)], rep(0L, length(x) - ld))
  }))
  ldepth_max_bs_evap <- rowSums(lyrs_max_bs_evap)

  sand_mean <- rowSums(lyrs_max_bs_evap * sand, na.rm = TRUE) / ldepth_max_bs_evap
  clay_mean <- rowSums(lyrs_max_bs_evap * clay, na.rm = TRUE) / ldepth_max_bs_evap

  temp_depth <- 4.1984 + 0.6695 * sand_mean ^ 2 + 168.7603 * clay_mean ^ 2 # equation from re-analysis
  depth_bs_evap <- pmin(pmax(temp_depth, depth_min_bs_evap, na.rm = TRUE),
    depth_max_bs_evap_cm, na.rm = TRUE)
  lyrs_bs_evap0 <- t(apply(depth_bs_evap - layers_depth, 1, function(x) {
    i0 <- abs(x) < SFSW2_glovars[["tol"]]
    ld <- if (any(i0, na.rm = TRUE)) {
      which(i0)
    } else {
      temp <- which(x < 0)
      if (length(temp) > 0) temp[1] else sum(!is.na(x))
    }
    ld0 <- max(0, ld - 1)

    c(rep(TRUE, ld0), rep(FALSE, length(x) - ld0))
  }))

  # function made up to match previous cummulative distributions
  temp_coeff <- 1 - exp(- 5 * layers_depth / depth_bs_evap)
  temp_coeff[!lyrs_bs_evap0 | is.na(temp_coeff)] <- 1
  coeff_bs_evap <- round(t(apply(cbind(0, temp_coeff), 1, diff)), 4)
  coeff_bs_evap / rowSums(coeff_bs_evap, na.rm = TRUE)
}


get_BareSoilEvaporationCoefficientsForSoilInputFile <- function(SFSW2_prj_meta,
  SFSW2_prj_inputs, runIDs_adjust, resume = TRUE, verbose = FALSE) {

  if (verbose) {
    t1 <- Sys.time()
    temp_call <- shQuote(match.call()[1])
    print(paste0("rSFSW2's ", temp_call, ": started at ", t1))

    on.exit({print(paste0("rSFSW2's ", temp_call, ": ended after ",
      round(difftime(Sys.time(), t1, units = "secs"), 2), " s")); cat("\n")}, add = TRUE)
  }

  icol_bsE <- grep("EvapCoeff", names(SFSW2_prj_inputs[["sw_input_soils_use"]]))
  icol_sand <- grep("Sand_L", names(SFSW2_prj_inputs[["sw_input_soils_use"]]))
  icol_clay <- grep("Clay_L", names(SFSW2_prj_inputs[["sw_input_soils_use"]]))
  use_layers <- which(SFSW2_prj_inputs[["sw_input_soils_use"]][icol_sand] &
    SFSW2_prj_inputs[["sw_input_soils_use"]][icol_clay])
  stopifnot(length(use_layers) > 0)

  do_calc <- TRUE
  if (resume) {
    temp <- icol_bsE[use_layers]
    icols <- temp[SFSW2_prj_inputs[["sw_input_soils_use"]][temp]]
    if (length(icols) > 0L) {
      x <- SFSW2_prj_inputs[["sw_input_soils"]][runIDs_adjust, icols, drop = FALSE]
      do_calc <- anyNA(x) || !all(rowSums(x, na.rm = TRUE) > 0)
      rm(x)
    }
  }

  if (do_calc) {
    temp <- SFSW2_prj_inputs[["sw_input_soillayers"]][runIDs_adjust, grep("depth_L",
      names(SFSW2_prj_inputs[["sw_input_soillayers"]]))[use_layers], drop = FALSE]
    layers_depth <- as.matrix(temp)

    #TODO: add influence of gravel
    sand <- SFSW2_prj_inputs[["sw_input_soils"]][runIDs_adjust, icol_sand, drop = FALSE]
    clay <- SFSW2_prj_inputs[["sw_input_soils"]][runIDs_adjust, icol_clay, drop = FALSE]

    coeff_bs_evap <- calc_BareSoilEvaporationCoefficientsFromSoilTexture(layers_depth, sand, clay,
      depth_max_bs_evap_cm = SFSW2_prj_meta[["opt_sim"]][["depth_max_bs_evap_cm"]])


    #add data to sw_input_soils and set the use flags
    icol <- seq_len(sum(apply(coeff_bs_evap, 2, function(x) any(x > SFSW2_glovars[["tol"]]))))
    icols_bsE_used <- icol_bsE[icol]
    icols_bse_notused <- icol_bsE[-icol]

    SFSW2_prj_inputs[["sw_input_soils_use"]][icols_bsE_used] <- TRUE
    SFSW2_prj_inputs[["sw_input_soils"]][runIDs_adjust, icols_bsE_used] <- round(coeff_bs_evap[, icol], 4)

    SFSW2_prj_inputs[["sw_input_soils_use"]][icols_bse_notused] <- FALSE
    SFSW2_prj_inputs[["sw_input_soils"]][runIDs_adjust, icols_bse_notused] <- 0

    stopifnot(!is.na(SFSW2_prj_inputs[["sw_input_soils"]][runIDs_adjust, icols_bsE_used]))

    #write data to disk
    utils::write.csv(reconstitute_inputfile(SFSW2_prj_inputs[["sw_input_soils_use"]],
      SFSW2_prj_inputs[["sw_input_soils"]]),
      file = SFSW2_prj_meta[["fnames_in"]][["fsoils"]], row.names = FALSE)
    unlink(SFSW2_prj_meta[["fnames_in"]][["fpreprocin"]])
  }

  SFSW2_prj_inputs
}


#' Look-up input values from spreadsheet tables
#' @export
do_prior_TableLookups <- function(SFSW2_prj_meta, SFSW2_prj_inputs, resume = TRUE,
  verbose = FALSE) {

  if (verbose) {
    t1 <- Sys.time()
    temp_call <- shQuote(match.call()[1])
    print(paste0("rSFSW2's ", temp_call, ": started at ", t1))

    on.exit({print(paste0("rSFSW2's ", temp_call, ": ended after ",
      round(difftime(Sys.time(), t1, units = "secs"), 2), " s")); cat("\n")}, add = TRUE)
  }
  do_prior_lookup <- list(
    LookupEvapCoeffFromTable = list(),
    LookupTranspRegionsFromTable = list(),
    LookupSnowDensityFromTable = list()
  )

  done_prior <- rep(FALSE, length(do_prior_lookup))
  names(done_prior) <- names(do_prior_lookup)

  if (any(SFSW2_prj_inputs[["create_treatments"]] %in% names(do_prior_lookup))) {

    do_prior_lookup[["LookupEvapCoeffFromTable"]] <- list(
      flag = "LookupEvapCoeffFromTable",
      pattern = "EvapCoeff",
      tr_input = SFSW2_prj_inputs[["tr_input_EvapCoeff"]],
      sw_input_use = "sw_input_soils_use",
      sw_input = "sw_input_soils",
      nvars = SFSW2_glovars[["slyrs_maxN"]],
      do_fill = FALSE,
      datafile = SFSW2_prj_meta[["fnames_in"]][["fsoils"]])

    do_prior_lookup[["LookupTranspRegionsFromTable"]] <- list(
      flag = "LookupTranspRegionsFromTable",
      pattern = "TranspRegion",
      tr_input = SFSW2_prj_inputs[["tr_input_TranspRegions"]],
      sw_input_use = "sw_input_soils_use",
      sw_input = "sw_input_soils",
      nvars = SFSW2_glovars[["slyrs_maxN"]],
      do_fill = FALSE,
      datafile = SFSW2_prj_meta[["fnames_in"]][["fsoils"]])

    do_prior_lookup[["LookupSnowDensityFromTable"]] <- list(
      flag = "LookupSnowDensityFromTable",
      pattern = "(snowd)|(SnowD_Hemisphere)",
      tr_input = SFSW2_prj_inputs[["tr_input_SnowD"]],
      sw_input_use = "sw_input_cloud_use",
      sw_input = "sw_input_cloud",
      nvars = 12 + 1,
      do_fill = TRUE,
      fill_pattern = "snowd",
      fill_value = 76,    # 76 kg/m3 = median of medians over 6 sites in Colorado and Wyoming: Judson, A. & Doesken, N. (2000) Density of Freshly Fallen Snow in the Central Rocky Mountains. Bulletin of the American Meteorological Society, 81, 1577-1587.
      datafile = SFSW2_prj_meta[["fnames_in"]][["fclimnorm"]])

    for (pc in do_prior_lookup) {
      if (any(SFSW2_prj_inputs[["create_treatments"]] == pc$flag)) {
        #lookup values per category for each simulation run and copy values to datafile
        temp1 <- SFSW2_prj_inputs[["sw_input_experimentals_use"]][pc$flag]
        temp2 <- length(unique(SFSW2_prj_inputs[["sw_input_experimentals"]][, pc$flag])) == 1L

        # Lookup prior to do_OneSite() only if option is off in sw_input_experimentals or constant
        if (!temp1 || (temp1 && temp2)) {

          if (resume) {
            # Determine whether lookup already carried out and stored to file
            sw_input_use <- SFSW2_prj_inputs[[pc$sw_input_use]]

            icols <- grep(pc$pattern, names(sw_input_use))
            icols <- icols[sw_input_use[icols]]
            temp <- SFSW2_prj_inputs[[pc$sw_input]][, icols, drop = FALSE]

            if (all(!apply(is.na(temp), 2, all))) {
              # if no layer has only NAs for which the _use flag is on, then consider as completed
              done_prior[pc$flag] <- TRUE
              next
            }
          }

          if (verbose)
            print(paste(Sys.time(), ": performing", shQuote(pc$flag)))

          trtype <- if (SFSW2_prj_inputs[["sw_input_experimentals_use"]][pc$flag]) {
              unique(SFSW2_prj_inputs[["sw_input_experimentals"]][, pc$flag])
            } else {
              SFSW2_prj_inputs[["sw_input_treatments"]][, pc$flag]
            }

          if (any(is.na(trtype)))
            stop("ERROR: ", pc$flag, " column cannot have any NAs.")
          if (!all(unique(trtype) %in% rownames(pc$tr_input)))
            stop("ERROR: ", pc$flag, " column values do not match up with trfile. ",
              pc$flag, " row names.")

          tempdat <- try(get.LookupFromTable(
            pattern = pc$pattern,
            trtype = trtype,
            tr_input = pc$tr_input,
            sw_input_use = SFSW2_prj_inputs[[pc$sw_input_use]],
            sw_input = SFSW2_prj_inputs[[pc$sw_input]],
            nvars = pc$nvars))

          done_prior[pc$flag] <- !inherits(tempdat, "try-error")
          if (done_prior[pc$flag]) {
            if (!is.null(pc$do_fill) && pc$do_fill)
              tempdat <- fill_empty(tempdat, pattern = pc$fill_pattern, fill = pc$fill_value)

            SFSW2_prj_inputs[[pc$sw_input_use]] <- tempdat$sw_input_use
            SFSW2_prj_inputs[[pc$sw_input]] <- tempdat$sw_input

            #write data to datafile
            utils::write.csv(reconstitute_inputfile(SFSW2_prj_inputs[[pc$sw_input_use]],
              SFSW2_prj_inputs[[pc$sw_input]]), file = pc$datafile, row.names = FALSE)
            unlink(SFSW2_prj_meta[["fnames_in"]][["fpreprocin"]])
          }

        } else {
          done_prior[pc$flag] <- FALSE
        }
      }
    }

  }

  SFSW2_prj_inputs[["done_prior"]] <- done_prior

  SFSW2_prj_inputs
}
