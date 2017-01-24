calc_ExtendSoilDatafileToRequestedSoilLayers <- function(requested_soil_layers,
  runIDs_adjust, sw_input_soillayers, sw_input_soils_use, sw_input_soils,
  fpreprocin, fslayers, fsoils, verbose = FALSE) {

  if (verbose)
    print(paste(Sys.time(), "'InterpolateSoilDatafileToRequestedSoilLayers' of",
      paste0(requested_soil_layers, collapse = ", "), "cm"))

  # How to add different soil variables
  sl_vars_mean <- c("Matricd", "GravelContent", "Sand", "Clay", "SoilTemp") # values will be interpolated
  sl_vars_sub <- c("EvapCoeff", "TranspCoeff", "Imperm") # values will be exhausted

  # Requested layers
  requested_soil_layers <- as.integer(round(requested_soil_layers))
  stopifnot(requested_soil_layers > 0, diff(requested_soil_layers) > 0)
  req_sl_ids <- paste0(requested_soil_layers, collapse = "x")

  # Available layers
  ids_depth <- strsplit(names(sw_input_soils_use)[sw_input_soils_use], "_", fixed = TRUE)
  stopifnot(length(ids_depth) > 0)
  var_layers <- unique(sapply(ids_depth, function(x) paste0(x[-length(x)], collapse = "_")))
  ids_depth2 <- unique(sapply(ids_depth, function(x) x[length(x)]))
  use_layers <- paste0("depth_", ids_depth2)

  layers_depth <- round(as.matrix(sw_input_soillayers[runIDs_adjust, use_layers, drop = FALSE]))
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
      as.matrix(sw_input_soils[runIDs_adjust_ws, grep(x, names(sw_input_soils))[ids_layers]]))
    sw_input_soils_data2 <- NULL

    for (ils in seq_along(layer_sets)) {
      il_set <- avail_sl_ids == layer_sets[ils]
      if (sum(il_set, na.rm = TRUE) == 0) next

      # Identify which requested layers to add
      ldset <- na.exclude(layers_depth[which(il_set)[1], ])
      req_sl_toadd <- setdiff(requested_soil_layers, ldset)
      req_sd_toadd <- req_sl_toadd[req_sl_toadd < max(ldset)]
      if (length(req_sd_toadd) == 0) next

      # Add identified layers
      sw_input_soils_data2 <- lapply(seq_along(var_layers), function(iv)
        sw_input_soils_data[[iv]][il_set, ])

      for (lnew in req_sd_toadd) {
        ilnew <- findInterval(lnew, ldset)
        il_weight <- abs(lnew - ldset[ilnew + 1:0])
        sw_input_soils_data2 <- lapply(seq_along(var_layers), function(iv)
          add_layer_to_soil(sw_input_soils_data2[[iv]], il = ilnew, w = il_weight,
            method = if (var_layers[iv] %in% sl_vars_sub) "exhaust" else "interpolate"))
        ldset <- sort(c(ldset, lnew))
      }

      # Update soil datafiles
      lyrs <- seq_along(ldset)
      for (iv in seq_along(var_layers)) {
        i.temp <- grep(var_layers[iv], names(sw_input_soils_use))[lyrs]
        sw_input_soils[runIDs_adjust_ws[il_set], i.temp] <-
          round(sw_input_soils_data2[[iv]][, lyrs], if (var_layers[iv] %in% sl_vars_sub) 4L else 2L)
        sw_input_soils_use[i.temp] <- TRUE
      }

      sw_input_soillayers[runIDs_adjust_ws[il_set],
        grep("depth_", names(sw_input_soillayers))[lyrs]] <- matrix(ldset,
        nrow = sum(il_set), ncol = length(ldset), byrow = TRUE)
      has_changed <- TRUE
    }

    if (has_changed) {
      #write data to disk
      write.csv(sw_input_soillayers, file = fslayers, row.names = FALSE)
      write.csv(reconstitute_inputfile(sw_input_soils_use, sw_input_soils),
        file = fsoils, row.names = FALSE)
      unlink(fpreprocin)

      print(paste0("'InterpolateSoilDatafileToRequestedSoilLayers': don't forget to",
        "adjust lookup tables with per-layer values if applicable for this project"))
    }
  }

  if (verbose)
    print(paste(Sys.time(), "completed 'InterpolateSoilDatafileToRequestedSoilLayers'"))

  list(sw_input_soillayers = sw_input_soillayers,
    sw_input_soils_use = sw_input_soils_use, sw_input_soils = sw_input_soils)
}


calc_CalculateBareSoilEvaporationCoefficientsFromSoilTexture <- function(runIDs_adjust,
  sw_input_soils_use, sw_input_soils, fpreprocin, fsoils, continueAfterAbort = TRUE,
  verbose = FALSE) {

 if (verbose)
    print(paste(Sys.time(), "'CalculateBareSoilEvaporationCoefficientsFromSoilTexture'"))

  # max = 15 cm: Torres EA, Calera A (2010) Bare soil evaporation under high evaporation
  #   demand: a proposed modification to the FAO-56 model. Hydrological Sciences Journal-
  #   Journal Des Sciences Hydrologiques, 55, 303-315.
  depth_max_bs_evap <- 15

  icol_bsE <- grep("EvapCoeff", names(sw_input_soils_use))
  icol_sand <- grep("Sand_L", names(sw_input_soils_use))
  icol_clay <- grep("Clay_L", names(sw_input_soils_use))
  use_layers <- which(sw_input_soils_use[icol_sand] & sw_input_soils_use[icol_clay])
  stopifnot(length(use_layers) > 0)

  do_calc <- TRUE
  if (continueAfterAbort) {
    temp <- icol_bsE[use_layers]
    icols <- temp[sw_input_soils_use[temp]]
    if (length(icols) > 0L) {
      x <- sw_input_soils[runIDs_adjust, icols, drop = FALSE]
      do_calc <- anyNA(x) || !all(rowSums(x, na.rm = TRUE) > 0)
      rm(x)
    }
  }

  if (do_calc) {
    temp <- sw_input_soillayers[runIDs_adjust, grep("depth_L",
      names(sw_input_soillayers))[use_layers], drop = FALSE]
    layers_depth <- as.matrix(temp)
    depth_min_bs_evap <- min(layers_depth[, 1])
    stopifnot(na.exclude(depth_min_bs_evap < depth_max_bs_evap))

    lyrs_max_bs_evap <- t(apply(layers_depth, 1, function(x) {
      xdm <- depth_max_bs_evap - x
      i0 <- abs(xdm) < tol
      ld <- if (any(i0, na.rm = TRUE)) {
        which(i0)
      } else {
        temp <- which(xdm < 0)
        if (length(temp) > 0) temp[1] else length(x)
      }
      c(diff(c(0, x))[seq_len(ld)], rep(0L, length(x) - ld))
    }))
    ldepth_max_bs_evap <- rowSums(lyrs_max_bs_evap)

    #TODO: add influence of gravel
    sand <- sw_input_soils[runIDs_adjust, icol_sand, drop = FALSE]
    clay <- sw_input_soils[runIDs_adjust, icol_clay, drop = FALSE]
    sand_mean <- rowSums(lyrs_max_bs_evap * sand, na.rm = TRUE) / ldepth_max_bs_evap
    clay_mean <- rowSums(lyrs_max_bs_evap * clay, na.rm = TRUE) / ldepth_max_bs_evap

    temp_depth <- 4.1984 + 0.6695 * sand_mean ^ 2 + 168.7603 * clay_mean ^ 2 # equation from re-analysis
    depth_bs_evap <- pmin(pmax(temp_depth, depth_min_bs_evap, na.rm = TRUE),
      depth_max_bs_evap, na.rm = TRUE)
    lyrs_bs_evap <- t(apply(depth_bs_evap - layers_depth, 1, function(x) {
      i0 <- abs(x) < tol
      ld <- if (any(i0, na.rm = TRUE)) {
        which(i0)
      } else {
        temp <- which(x < 0)
        if (length(temp) > 0) temp[1] else sum(!is.na(x))
      }
      c(rep(TRUE, ld), rep(FALSE, length(x) - ld))
    }))

    # function made up to match previous cummulative distributions
    temp_coeff <- 1 - exp(- 5 * layers_depth / depth_bs_evap)
    temp_coeff[!lyrs_bs_evap | is.na(temp_coeff)] <- 1
    coeff_bs_evap <- round(t(apply(cbind(0, temp_coeff), 1, diff)), 4)
    coeff_bs_evap <- coeff_bs_evap / rowSums(coeff_bs_evap, na.rm = TRUE)

    #add data to sw_input_soils and set the use flags
    icol <- seq_len(sum(apply(coeff_bs_evap, 2, function(x) any(x > tol))))
    icols_bsE_used <- icol_bsE[icol]
    icols_bse_notused <- icol_bsE[-icol]

    sw_input_soils_use[icols_bsE_used] <- TRUE
    sw_input_soils[runIDs_adjust, icols_bsE_used] <- round(coeff_bs_evap[, icol], 4)

    sw_input_soils_use[icols_bse_notused] <- FALSE
    sw_input_soils[runIDs_adjust, icols_bse_notused] <- 0

    stopifnot(!is.na(sw_input_soils[runIDs_adjust, icols_bsE_used]))

    #write data to disk
    write.csv(reconstitute_inputfile(sw_input_soils_use, sw_input_soils),
      file = fsoils, row.names = FALSE)
    unlink(fpreprocin)
  }

  if (verbose)
    print(paste(Sys.time(), "completed",
      "'CalculateBareSoilEvaporationCoefficientsFromSoilTexture'"))

  list(sw_input_soils_use = sw_input_soils_use, sw_input_soils = sw_input_soils)
}


do_prior_TableLookups <- function(tr_input_EvapCoeff, tr_input_TranspRegions, tr_input_SnowD,
  create_treatments, SoilLayer_MaxNo, sw_input_experimentals_use, sw_input_experimentals,
  sw_input_soils_use, sw_input_soils, sw_input_cloud_use, sw_input_cloud,
  fpreprocin, fsoils, fcloud, continueAfterAbort = TRUE, verbose = FALSE) {

  if (verbose)
    print(paste("SWSF obtains information prior to simulation runs: started at",
      t1 <- Sys.time()))

  if (any(create_treatments %in% c("LookupEvapCoeffFromTable",
    "LookupTranspRegionsFromTable", "LookupSnowDensityFromTable"))) {

    do_prior_lookup <- list(
      LookupEvapCoeffFromTable = list(
        flag = "LookupEvapCoeffFromTable",
        pattern = "EvapCoeff",
        tr_input = tr_input_EvapCoeff,
        sw_input_use = "sw_input_soils_use",
        sw_input = "sw_input_soils",
        nvars = SoilLayer_MaxNo,
        do_fill = FALSE,
        datafile = fsoils),

      LookupTranspRegionsFromTable = list(
        flag = "LookupTranspRegionsFromTable",
        pattern = "TranspRegion",
        tr_input = tr_input_TranspRegions,
        sw_input_use = "sw_input_soils_use",
        sw_input = "sw_input_soils",
        nvars = SoilLayer_MaxNo,
        do_fill = FALSE,
        datafile = fsoils),

      LookupSnowDensityFromTable = list(
        flag = "LookupSnowDensityFromTable",
        pattern = "(snowd)|(SnowD_Hemisphere)",
        tr_input = tr_input_SnowD,
        sw_input_use = "sw_input_cloud_use",
        sw_input = "sw_input_cloud",
        nvars = 12 + 1,
        do_fill = TRUE,
        fill_pattern = "snowd",
        fill_value = 76,  	# 76 kg/m3 = median of medians over 6 sites in Colorado and Wyoming: Judson, A. & Doesken, N. (2000) Density of Freshly Fallen Snow in the Central Rocky Mountains. Bulletin of the American Meteorological Society, 81, 1577-1587.
        datafile = fcloud)
    )

    done_prior <- rep(FALSE, length(do_prior_lookup))
    names(done_prior) <- names(do_prior_lookup)

    for (pc in do_prior_lookup) {
      if (any(create_treatments == pc$flag)) {
        #lookup values per category for each simulation run and copy values to datafile
        temp <- sw_input_experimentals_use[pc$flag]
        if (!temp || (temp && (length(unique(sw_input_experimentals[, pc$flag])) == 1L))) {
          # Lookup prior to do_OneSite() only if option is off in sw_input_experimentals or constant

          if (continueAfterAbort) {
            # Determine whether lookup already carried out and stored to file
            sw_input_use <- get(pc$sw_input_use)

            icols <- grep(pc$pattern, names(sw_input_use))
            icols <- icols[sw_input_use[icols]]
            temp <- get(pc$sw_input)[, icols, drop = FALSE]

            if (all(!apply(is.na(temp), 2, all))) {
              # if no layer has only NAs for which the _use flag is on, then consider as completed
              done_prior[pc$flag] <- TRUE
              next
            }
          }

          if (verbose)
            print(paste(Sys.time(), ": performing", shQuote(pc$flag)))

          trtype <- if (sw_input_experimentals_use[pc$flag]) {
              unique(sw_input_experimentals[, pc$flag])
            } else {
              sw_input_treatments[, pc$flag]
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
            sw_input_use = get(pc$sw_input_use),
            sw_input = get(pc$sw_input),
            nvars = pc$nvars))

          done_prior[pc$flag] <- !inherits(tempdat, "try-error")
          if (done_prior[pc$flag]) {
            if (!is.null(pc$do_fill) && pc$do_fill)
              tempdat <- fill_empty(tempdat, pattern = pc$fill_pattern, fill = pc$fill_value)

            assign(pc$sw_input_use, tempdat$sw_input_use)
            assign(pc$sw_input, tempdat$sw_input)

            #write data to datafile
            write.csv(reconstitute_inputfile(tempdat$sw_input_use, tempdat$sw_input),
              file = pc$datafile, row.names = FALSE)
            unlink(fpreprocin)
          }

        } else {
          done_prior[pc$flag] <- FALSE
        }
      }
    }

  }

  if (verbose)
    print(paste("SWSF obtains information prior to simulation runs: ended after",
      round(difftime(Sys.time(), t1, units = "secs"), 2), "s"))

  list(sw_input_soils_use = sw_input_soils_use, sw_input_soils = sw_input_soils,
    sw_input_cloud_use = sw_input_cloud_use, sw_input_cloud = sw_input_cloud,
    done_prior = done_prior)
}
