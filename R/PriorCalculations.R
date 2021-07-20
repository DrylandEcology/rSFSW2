
#' Add soil layers (by interpolation) if not already present and store in
#' soil data input file
#'
#' @section Notes: Splitting existing soil layers into two new layers will
#'   "exhaust" the values of fields \var{\dQuote{EvapCoeff}},
#'   \var{\dQuote{TranspCoeff}}, and \var{\dQuote{Imperm}}, i.e., sum remains
#'   constant; and will "interpolate" the values of the fields
#'   \var{\dQuote{Matricd}}, \var{\dQuote{GravelContent}}, \var{\dQuote{Sand}},
#'   \var{\dQuote{Clay}}, \var{\dQuote{SoilTemp}}.
#'
#' @export
calc_RequestedSoilLayers <- function(
  SFSW2_prj_meta,
  SFSW2_prj_inputs,
  runIDs_adjust,
  keep_prev_soildepth = TRUE,
  keep_prev_soillayers = TRUE,
  verbose = FALSE
) {

  if (verbose) {
    t1 <- Sys.time()
    temp_call <- shQuote(match.call()[1])
    print(paste0("rSFSW2's ", temp_call, ": started at ", t1))

    on.exit({
      print(paste0("rSFSW2's ", temp_call, ": ended after ",
        round(difftime(Sys.time(), t1, units = "secs"), 2), " s"))
      cat("\n")}, add = TRUE)
  }

  vars_exhaust <- c("EvapCoeff", "TranspCoeff", "Imperm")

  #--- Available variables
  tmp <- colnames(SFSW2_prj_inputs[["sw_input_soils"]])[-1]
  vars_all <- unique(sapply(
    strsplit(tmp, split = "_", fixed = TRUE),
    function(x) paste0(x[-length(x)], collapse = "_")
  ))

  stopifnot(nchar(vars_all) > 0)
  n_vars <- length(vars_all)


  tmp <- SFSW2_prj_inputs[["sw_input_soils_use"]]
  ids_depth <- strsplit(names(tmp)[tmp], split = "_", fixed = TRUE)
  stopifnot(length(ids_depth) > 0)

  var_layers <- unique(sapply(
    X = ids_depth,
    FUN = function(x) paste0(x[-length(x)], collapse = "_")
  ))

  #--- Available layers
  ids_depth2 <- unique(sapply(ids_depth, function(x) x[length(x)]))
  use_layers <- paste0("depth_", ids_depth2)

  layers_depth <- round(
    as.matrix(SFSW2_prj_inputs[["sw_input_soillayers"]]
      [runIDs_adjust, use_layers, drop = FALSE])
  )
  has_nodata <- apply(is.na(layers_depth), 1, all)

  if (any(has_nodata)) {
    layers_depth <- layers_depth[!has_nodata, , drop = FALSE]
    ids_updated <- runIDs_adjust[!has_nodata]
  } else {
    ids_updated <- runIDs_adjust
  }

  has_nodata <- apply(is.na(layers_depth), 2, all)
  if (any(has_nodata)) {
    layers_depth <- layers_depth[, !has_nodata, drop = FALSE]
    use_layers <- use_layers[!has_nodata]
  }


  new_soils <- rSW2data::update_soil_profile(
    soil_layers = layers_depth,
    requested_soil_layers =
      SFSW2_prj_meta[["opt_input"]][["requested_soil_layers"]],
    soil_data =
      SFSW2_prj_inputs[["sw_input_soils"]][ids_updated, -1, drop = FALSE],
    variables = var_layers,
    vars_exhaust = vars_exhaust,
    keep_prev_soildepth = keep_prev_soildepth,
    keep_prev_soillayers = keep_prev_soillayers,
    verbose = verbose
  )


  if (new_soils[["updated"]]) {
    n_req <- length(SFSW2_glovars[["slyrs_ids"]])

    #--- Round soil data
    cn_tmp <- colnames(new_soils[["soil_data"]])

    for (k in seq_along(var_layers)) {
      kvar <- grep(var_layers[k], cn_tmp, value = TRUE)

      new_soils[["soil_data"]][, kvar] <- round(
        new_soils[["soil_data"]][, kvar],
        digits = if (var_layers[k] %in% vars_exhaust) 4L else 3L
      )
    }

    #--- transfer updated soils
    n_new_sd <- ncol(new_soils[["soil_data"]]) %/% n_vars
    n_has_sd <- (ncol(SFSW2_prj_inputs[["sw_input_soils"]]) - 1) %/% n_vars

    n_tmp <- max(n_req, n_has_sd, n_new_sd)

    if (n_req < n_new_sd || n_req < n_has_sd) {
      stop(
        "Downstream code requires exactly ", n_req, " soil layers; ",
        "we have now n = ", n_tmp
      )
    }

    cn_tmp <- paste0(
      rep(vars_all, n_tmp),
      "_L",
      rep(seq_len(n_tmp), each = n_vars)
    )

    tmp_sprop <- data.frame(
      Label = SFSW2_prj_inputs[["sw_input_soils"]][, "Label"],
      matrix(
        data = NA,
        nrow = nrow(SFSW2_prj_inputs[["sw_input_soils"]]),
        ncol = n_tmp * n_vars,
        dimnames = list(NULL, cn_tmp)
      )
    )

    cn_tmp <- paste0(
      rep(var_layers, n_new_sd),
      "_L",
      rep(seq_len(n_new_sd), each = length(var_layers))
    )

    tmp_sprop[ids_updated, cn_tmp] <- new_soils[["soil_data"]][, cn_tmp]


    #--- transfer usage
    n_new_sl <- sum(
      apply(
        X = new_soils[["soil_layers"]],
        MARGIN = 2,
        FUN = function(x) any(is.finite(x))
      )
    )

    cn_tmp <- paste0(
      rep(var_layers, n_new_sl),
      "_L",
      rep(seq_len(n_new_sl), each = length(var_layers))
    )

    SFSW2_prj_inputs[["sw_input_soils_use"]][cn_tmp] <- TRUE


    #--- transfer updated soil layer depths
    is_depth <- grepl(
      "depth_L",
      colnames(SFSW2_prj_inputs[["sw_input_soillayers"]])
    )

    n_has_sd <- sum(is_depth)
    n_new_sd <- ncol(new_soils[["soil_layers"]])
    n_tmp <- max(n_req, n_has_sd, n_new_sd)

    if (n_req < n_new_sd || n_req < n_has_sd) {
      stop(
        "Downstream code requires exactly ", n_req, " soil layers; ",
        "we have now n = ", n_tmp
      )
    }

    cn_tmp <- paste0("depth_L", seq_len(n_tmp))

    x_tmp <- SFSW2_prj_inputs[["sw_input_soillayers"]][, !is_depth, drop = FALSE] #nolint
    tmp_slyrs <- data.frame(
      x_tmp,
      matrix(
        data = NA,
        nrow = nrow(SFSW2_prj_inputs[["sw_input_soillayers"]]),
        ncol = n_tmp,
        dimnames = list(NULL, cn_tmp)
      )
    )

    ids <- seq_len(n_new_sd)
    tmp_slyrs[ids_updated, ncol(x_tmp) + ids] <-
      new_soils[["soil_layers"]][, ids]


    #--- write updated data to disk
    SFSW2_prj_inputs[["sw_input_soils"]] <- tmp_sprop
    SFSW2_prj_inputs[["sw_input_soillayers"]] <- tmp_slyrs

    utils::write.csv(
      SFSW2_prj_inputs[["sw_input_soillayers"]],
      file = SFSW2_prj_meta[["fnames_in"]][["fslayers"]],
      row.names = FALSE
    )

    utils::write.csv(
      reconstitute_inputfile(
        SFSW2_prj_inputs[["sw_input_soils_use"]],
        SFSW2_prj_inputs[["sw_input_soils"]]
      ),
      file = SFSW2_prj_meta[["fnames_in"]][["fsoils"]],
      row.names = FALSE
    )

    unlink(SFSW2_prj_meta[["fnames_in"]][["fpreprocin"]])

    if (verbose) {
      print(paste(
        "'InterpolateSoilDatafileToRequestedSoilLayers':",
        "don't forget to adjust lookup tables with per-layer values if",
        "applicable for this project"
      ))
    }
  }

  list(SFSW2_prj_meta = SFSW2_prj_meta, SFSW2_prj_inputs = SFSW2_prj_inputs)
}



#' Calculate bare-soil evaporation coefficients based on soil texture and store
#' in soil input file
get_BareSoilEvapCoefs <- function(
  SFSW2_prj_meta,
  SFSW2_prj_inputs,
  runIDs_adjust,
  resume = TRUE,
  verbose = FALSE
) {

  if (verbose) {
    t1 <- Sys.time()
    temp_call <- shQuote(match.call()[1])
    print(paste0("rSFSW2's ", temp_call, ": started at ", t1))

    on.exit({
      print(paste0("rSFSW2's ", temp_call, ": ended after ",
      round(difftime(Sys.time(), t1, units = "secs"), 2), " s"))
      cat("\n")},
      add = TRUE
    )
  }

  icol_bsE <- grep("EvapCoeff", names(SFSW2_prj_inputs[["sw_input_soils_use"]]))
  icol_sand <- grep("Sand_L", names(SFSW2_prj_inputs[["sw_input_soils_use"]]))
  icol_clay <- grep("Clay_L", names(SFSW2_prj_inputs[["sw_input_soils_use"]]))
  use_layers <- which(
    SFSW2_prj_inputs[["sw_input_soils_use"]][icol_sand] &
    SFSW2_prj_inputs[["sw_input_soils_use"]][icol_clay]
  )
  stopifnot(length(use_layers) > 0)

  do_calc <- TRUE
  if (resume) {
    tmp <- icol_bsE[use_layers]
    icols <- tmp[SFSW2_prj_inputs[["sw_input_soils_use"]][tmp]]
    if (length(icols) > 0L) {
      x <-
        SFSW2_prj_inputs[["sw_input_soils"]][runIDs_adjust, icols, drop = FALSE]
      do_calc <- anyNA(x) || !all(rowSums(x, na.rm = TRUE) > 0)
      rm(x)
    }
  }

  if (do_calc) {
    icols <- grep(
      "depth_L",
      names(SFSW2_prj_inputs[["sw_input_soillayers"]])
    )[use_layers]
    tmp <-
      SFSW2_prj_inputs[["sw_input_soillayers"]][runIDs_adjust, icols,
        drop = FALSE]
    layers_depth <- as.matrix(tmp)

    sand <-
      SFSW2_prj_inputs[["sw_input_soils"]][runIDs_adjust, icol_sand,
        drop = FALSE]
    clay <-
      SFSW2_prj_inputs[["sw_input_soils"]][runIDs_adjust, icol_clay,
        drop = FALSE]

    coeff_bs_evap <- rSW2data::calc_BareSoilEvapCoefs(
      layers_depth,
      sand,
      clay,
      depth_max_bs_evap_cm =
        SFSW2_prj_meta[["opt_sim"]][["depth_max_bs_evap_cm"]],
      method_bad_soils = "pass"
    )

    # warn if any used runs returned with NA
    has_bad_evco <- anyNA(
      coeff_bs_evap[SFSW2_prj_meta[["sim_size"]][["runIDs_sites"]], 1]
    )
    if (has_bad_evco) {
      stop(
        "Not able to estimate bare-soil evaporation coefficients ",
        "for some active sites because of poor soil inputs."
      )
    }


    #add data to sw_input_soils and set the use flags
    icol <- seq_len(sum(
      apply(
        coeff_bs_evap,
        MARGIN = 2,
        FUN = function(x) any(x > SFSW2_glovars[["tol"]], na.rm = TRUE)
      )
    ))
    icols_bsE_used <- icol_bsE[icol]
    icols_bse_notused <- icol_bsE[-icol]

    SFSW2_prj_inputs[["sw_input_soils_use"]][icols_bsE_used] <- TRUE
    SFSW2_prj_inputs[["sw_input_soils"]][runIDs_adjust, icols_bsE_used] <-
      round(coeff_bs_evap[, icol], 4)

    SFSW2_prj_inputs[["sw_input_soils_use"]][icols_bse_notused] <- FALSE
    SFSW2_prj_inputs[["sw_input_soils"]][runIDs_adjust, icols_bse_notused] <- 0

    #write data to disk
    utils::write.csv(
      reconstitute_inputfile(
        SFSW2_prj_inputs[["sw_input_soils_use"]],
        SFSW2_prj_inputs[["sw_input_soils"]]
      ),
      file = SFSW2_prj_meta[["fnames_in"]][["fsoils"]],
      row.names = FALSE
    )
    unlink(SFSW2_prj_meta[["fnames_in"]][["fpreprocin"]])
  }

  SFSW2_prj_inputs
}


#' Look-up input values from spreadsheet tables
#' @export
do_prior_TableLookups <- function(SFSW2_prj_meta, SFSW2_prj_inputs,
  resume = TRUE, verbose = FALSE) {

  if (verbose) {
    t1 <- Sys.time()
    temp_call <- shQuote(match.call()[1])
    print(paste0("rSFSW2's ", temp_call, ": started at ", t1))

    on.exit({
      print(paste0("rSFSW2's ", temp_call, ": ended after ",
      round(difftime(Sys.time(), t1, units = "secs"), 2), " s"))
      cat("\n")}, add = TRUE)
  }

  do_prior_lookup <- list(
    LookupEvapCoefs = list(),
    LookupTranspRegions = list(),
    LookupSnowDensity = list()
  )

  done_prior <- rep(FALSE, length(do_prior_lookup))
  names(done_prior) <- names(do_prior_lookup)

  temp <- SFSW2_prj_inputs[["create_treatments"]] %in% names(do_prior_lookup)
  if (any(temp)) {

    do_prior_lookup[["LookupEvapCoefs"]] <- list(
      flag = "LookupEvapCoefs",
      pattern = "EvapCoeff",
      tr_input = SFSW2_prj_inputs[["tr_input_EvapCoeff"]],
      sw_input_use = "sw_input_soils_use",
      sw_input = "sw_input_soils",
      nvars = SFSW2_glovars[["slyrs_maxN"]],
      do_fill = FALSE,
      datafile = SFSW2_prj_meta[["fnames_in"]][["fsoils"]])

    do_prior_lookup[["LookupTranspRegions"]] <- list(
      flag = "LookupTranspRegions",
      pattern = "TranspRegion",
      tr_input = SFSW2_prj_inputs[["tr_input_TranspRegions"]],
      sw_input_use = "sw_input_soils_use",
      sw_input = "sw_input_soils",
      nvars = SFSW2_glovars[["slyrs_maxN"]],
      do_fill = FALSE,
      datafile = SFSW2_prj_meta[["fnames_in"]][["fsoils"]])

    do_prior_lookup[["LookupSnowDensity"]] <- list(
      flag = "LookupSnowDensity",
      pattern = "(snowd)|(SnowD_Hemisphere)",
      tr_input = SFSW2_prj_inputs[["tr_input_SnowD"]],
      sw_input_use = "sw_input_cloud_use",
      sw_input = "sw_input_cloud",
      nvars = 12 + 1,
      do_fill = TRUE,
      fill_pattern = "snowd",
      fill_value = 76,    # 76 kg/m3 = median of medians over 6 sites in
      # Colorado and Wyoming: Judson, A. & Doesken, N. (2000) Density of
      # Freshly Fallen Snow in the Central Rocky Mountains. Bulletin of the
      # American Meteorological Society, 81, 1577-1587.
      datafile = SFSW2_prj_meta[["fnames_in"]][["fclimnorm"]])

    for (pc in do_prior_lookup) {
      if (any(SFSW2_prj_inputs[["create_treatments"]] == pc$flag)) {
        # lookup values per category for each simulation run and copy values
        # to datafile
        temp1 <- SFSW2_prj_inputs[["sw_input_experimentals_use"]][pc$flag]
        temp <- unique(SFSW2_prj_inputs[["sw_input_experimentals"]][, pc$flag])
        temp2 <- length(temp) == 1L

        # Lookup prior to do_OneSite() only if option is off in
        # sw_input_experimentals or constant
        if (!temp1 || (temp1 && temp2)) {

          if (resume) {
            # Determine whether lookup already carried out and stored to file
            sw_input_use <- SFSW2_prj_inputs[[pc$sw_input_use]]

            icols <- grep(pc$pattern, names(sw_input_use))
            icols <- icols[sw_input_use[icols]]
            temp <- SFSW2_prj_inputs[[pc$sw_input]][, icols, drop = FALSE]

            if (all(!apply(is.na(temp), 2, all))) {
              # if no layer has only NAs for which the _use flag is on, then
              # consider as completed
              done_prior[pc$flag] <- TRUE
              next
            }
          }

          if (verbose)
            print(paste(Sys.time(), ": performing", shQuote(pc$flag)))

          temp <- SFSW2_prj_inputs[["sw_input_experimentals_use"]][pc$flag]
          trtype <- if (temp) {
              unique(SFSW2_prj_inputs[["sw_input_experimentals"]][, pc$flag])
            } else {
              SFSW2_prj_inputs[["sw_input_treatments"]][, pc$flag]
            }

          if (any(is.na(trtype)))
            stop("ERROR: ", pc$flag, " column cannot have any NAs.")
          if (!all(unique(trtype) %in% rownames(pc$tr_input)))
            stop("ERROR: ", pc$flag, " column values do not match up with ",
              "trfile. ", pc$flag, " row names.")

          tempdat <- try(get.LookupFromTable(
            pattern = pc$pattern,
            trtype = trtype,
            tr_input = pc$tr_input,
            sw_input_use = SFSW2_prj_inputs[[pc$sw_input_use]],
            sw_input = SFSW2_prj_inputs[[pc$sw_input]],
            nvars = pc$nvars))

          done_prior[pc$flag] <- !inherits(tempdat, "try-error")
          if (done_prior[pc$flag]) {
            if (!is.null(pc$do_fill) && pc$do_fill) {
              tempdat <- fill_empty(tempdat, pattern = pc$fill_pattern,
                fill = pc$fill_value)
            }

            SFSW2_prj_inputs[[pc$sw_input_use]] <- tempdat$sw_input_use
            SFSW2_prj_inputs[[pc$sw_input]] <- tempdat$sw_input

            #write data to datafile
            utils::write.csv(reconstitute_inputfile(
              SFSW2_prj_inputs[[pc$sw_input_use]],
              SFSW2_prj_inputs[[pc$sw_input]]), file = pc$datafile,
              row.names = FALSE)
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
