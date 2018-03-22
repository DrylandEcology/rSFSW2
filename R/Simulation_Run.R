#---------------------------------------------------------------------------------------#
#------------------------FUNCTIONS FOR SOILWAT2 SIMULATIONS

#' List of objects to export which are required by do_OneSite and are not in rSFSW2
#'  (sorted alphabetically)
#' @export
global_args_do_OneSite <- function() {
  c("create_experimentals", "create_treatments", "done_prior", "fnames_in", "fnames_out",
    "opt_agg", "opt_behave", "opt_out_fix", "opt_out_run", "opt_parallel",
    "opt_sim", "opt_verbosity", "prj_todos", "project_paths", "rng_specs", "sim_scens",
    "sim_size", "sim_time", "sw_input_climscen_use", "sw_input_climscen_values_use",
    "sw_input_cloud_use", "sw_input_experimentals_use", "sw_input_experimentals",
    "sw_input_prod_use", "sw_input_site_use", "sw_input_soils_use",
    "sw_input_weather_use", "swDataFromFiles", "swof", "t_job_start", "tr_cloud",
    "tr_files", "tr_input_CarbonScenario", "tr_input_climPPT", "tr_input_climTemp", "tr_input_EvapCoeff",
    "tr_input_shiftedPPT", "tr_input_SnowD", "tr_input_TranspCoeff_Code",
    "tr_input_TranspCoeff", "tr_input_TranspRegions", "tr_prod", "tr_site", "tr_soil",
    "tr_VegetationComposition", "tr_weather")
}

if (getRversion() >= "2.15.1")
  utils::globalVariables(c("MoreArgs", global_args_do_OneSite()))

gather_args_do_OneSite <- function(meta, inputs) {
  gather_objects_for_export(varlist = global_args_do_OneSite(),
    list_envs = list(meta = meta, inputs = inputs, local = environment(),
    parent = parent.frame(), global = globalenv()))
}

print_debug <- function(opt_verbosity, tag_id, tag_action, tag_section = NULL) {
  if (opt_verbosity[["print.debug"]]) {
    print(paste0(tag_id, ": ", tag_action,
      if (!is.null(tag_section)) paste0(" ", shQuote(tag_section))))
  }
}


print_debugN <- function(opt_verbosity, tag_id, prj_todos, n, tag_section) {
  if (opt_verbosity[["print.debug"]] && n != prj_todos[["aon_fields"]][tag_section, "N"]) {

    stop(tag_id, ": ", shQuote(tag_section), " aggregation produced n = ", n,
      " but dbOutput expects n' = ", prj_todos[["aon_fields"]][tag_section, "N"])
  }
}


#' The main simulation function which does all the heavy lifting
#'
#' @details For contributors only: This function cannot return prematurely because
#'  (i.e., don't use \code{return}); otherwise the management of simulation runs will
#'  fail. If a condition is met that prevents proper continuation/execution of a
#'  simulation, then the appropriate element in the variable \code{tasks} should be set
#'  to 0. The variable \code{tasks} contains the elements \code{create}, \code{execute},
#'  and \code{aggregate} with the values: \itemize{
#'    \item -1 indicates "don't do" a task element
#'    \item 0 indicates that the task element has/is "failed"
#'    \item 1 indicates "to do" a task element
#'    \item 2 indicates that a task element had "success" in executing relevant code
#'  }
#' @export
do_OneSite <- function(i_sim, i_SWRunInformation, i_sw_input_soillayers,
  i_sw_input_treatments, i_sw_input_cloud, i_sw_input_prod, i_sw_input_site,
  i_sw_input_soils, i_sw_input_weather, i_sw_input_climscen, i_sw_input_climscen_values,
  SimParams) {

  # i_sim =   a value of runIDs_total, i.e., index for each simulation run
  # i_xxx =   the i_site-row of xxx for the i-th simulation run; if expN > 0 then these
  #           will eventually be repeated, and below replaced with experimental values
  # i_exp =   the row of sw_input_experimentals for the i_sim-th simulation run
  # P_id  =   is a unique id number for each scenario in each run

  t.do_OneSite <- Sys.time()

  # ID of worker
  fid <- if (SFSW2_glovars[["p_has"]]) {
      if (SFSW2_glovars[["p_type"]] == "mpi") {
        Rmpi::mpi.comm.rank()
      } else if (SFSW2_glovars[["p_type"]] == "socket") {
        get(SFSW2_glovars[["p_wtag"]], envir = globalenv())
      }
    } else {
      0L
    }

  # temporary output database
  dbTempFile <- DBI::dbConnect(RSQLite::SQLite(), dbname =
    file.path(project_paths[["dir_out_temp"]], paste0("SQL_Node_", fid, ".sqlite3")))
  on.exit(DBI::dbDisconnect(dbTempFile), add = TRUE)

  # Print/tag for function call
  tag_simfid <- paste0("[run", i_sim, "/work", fid, "]")
  temp_call <- shQuote("do_OneSite") # match.call()[1] doesn't work when called via parallel-backend
  tag_funid <- paste0("rSFSW2's ", temp_call, ": ", tag_simfid)

  if (SimParams[["opt_verbosity"]][["verbose"]]) {
    print(paste0(tag_funid, ": started at ", t.do_OneSite))

    on.exit({print(paste0(tag_funid, ": ended prematurely")); cat("\n")}, add = TRUE)
  }

  temp <- difftime(t.do_OneSite, SimParams[["t_job_start"]], units = "secs")
  temp <- temp + SimParams[["opt_parallel"]][["opt_job_time"]][["one_sim_s"]]
  has_time_to_simulate <- temp < SimParams[["opt_parallel"]][["opt_job_time"]][["wall_time_s"]]

  if (!has_time_to_simulate)
    stop(tag_funid, ": not enough time to simulate.")

  list2env(as.list(SimParams), envir = environment())

  if (!(SFSW2_glovars[["p_has"]] && SFSW2_glovars[["p_type"]] == "mpi")) {
    stopifnot(dbWork_update_job(project_paths[["dir_out"]], i_sim, status = "inwork",
      verbose = opt_verbosity[["print.debug"]]))
  }

  flag.icounter <- formatC(i_sim, width = sim_size[["digitsN_total"]], format = "d",
    flag = "0")

  if (opt_verbosity[["debug.dump.objects"]]) {
    print(paste0(tag_funid, ": 'last.dump.do_OneSite_", i_sim, ".RData' on error."))

    on.exit({
      op_prev <- options("warn")
      options(warn = 0)
      env_tosave <- new.env()
      list2env(as.list(globalenv()), envir = env_tosave)
      list2env(as.list(parent.frame()), envir = env_tosave)
      list2env(as.list(environment()), envir = env_tosave)
      save(list = ls(envir = env_tosave), envir = env_tosave,
        file = file.path(project_paths[["dir_prj"]], paste0("last.dump.do_OneSite_",
        i_sim, ".RData")))
      options(op_prev)
    }, add = TRUE)
  }

  # Set RNG seed for random number use by functions
  #   - Aggregation GISSM: calculate_TimeToGerminate_modifiedHardegree2006NLR
  set_RNG_stream(seed = rng_specs[["seeds_runN"]][[it_site(i_sim, sim_size[["runsN_master"]])]])

  if (opt_verbosity[["print.debug"]] && identical(fid, 0L)) {
    temp <- sapply(grep("p_", ls(envir = SFSW2_glovars), value = TRUE),
      function(x) paste(shQuote(x), "=", paste(SFSW2_glovars[[x]], collapse = " / ")))
    temp <- paste(temp, collapse = "; ")

    print(paste0(tag_funid, ": worker ID is 0 with global variables: ", temp))
  }

#-----------------------Check for experimentals
  if (sim_size[["expN"]] > 0 && length(create_experimentals) > 0) {
    i_exp <- it_exp(i_sim, sim_size[["runsN_master"]])
    i_label <- paste(flag.icounter, sw_input_experimentals[i_exp, 1],
      i_SWRunInformation["Label"], sep = "_")

    #--put information from experimental design into appropriate input variables; create_treatments and the _use files were already adjusted for the experimental design when files were read in/created
    i_sw_input_treatments <- transferExpDesignToInput(i_sw_input_treatments, i_exp,
      df_exp = sw_input_experimentals, df_exp_use = sw_input_experimentals_use)
    i_sw_input_soils <- transferExpDesignToInput(i_sw_input_soils, i_exp,
      df_exp = sw_input_experimentals, df_exp_use = sw_input_experimentals_use)
    i_sw_input_site <- transferExpDesignToInput(i_sw_input_site, i_exp,
      df_exp = sw_input_experimentals, df_exp_use = sw_input_experimentals_use)
    i_sw_input_prod <- transferExpDesignToInput(i_sw_input_prod, i_exp,
      df_exp = sw_input_experimentals, df_exp_use = sw_input_experimentals_use)
  }


#------------------------Preparations for simulation run
  #Check what needs to be done
  #TODO this currently doesn't work in the database setup
  isdone.overallAggs <- rep(FALSE, sim_scens[["N"]])
  if (prj_todos[["adaily"]][["N"]] > 0) {
    isdone.dailyAggs <- matrix(FALSE, nrow = prj_todos[["adaily"]][["N"]], ncol = sim_scens[["N"]])
  } else {
    isdone.dailyAggs <- TRUE
  }

  #set up task list: code: -1, don't do; 0, failed; 1, to do; 2, success
  #   for now: ignoring to check time-series aggregations, i.e., assuming that if
  #   overallAggs is done, then time-series output was also completed
  tasks <- list(
    create = if (prj_todos[["actions"]][["sim_create"]]) 1L else -1L,
    execute = rep(if (prj_todos[["actions"]][["sim_execute"]]) 1L else -1L, sim_scens[["N"]]),
    aggregate = rep(if (prj_todos[["actions"]][["sim_aggregate"]]) 1L else -1L,
      sim_scens[["N"]]))

  #Prepare directory structure in case SOILWAT2 input/output is requested to be stored on disk
  temp <- file.path(project_paths[["dir_out_sw"]], i_label)
  f_sw_input <- file.path(temp, "sw_input.RData")
  f_sw_output <- file.path(temp, paste0("sw_output_sc", seq_len(sim_scens[["N"]]),
    ".RData"))

  if (opt_out_run[["saveRsoilwatInput"]] || opt_out_run[["saveRsoilwatOutput"]]) {
    dir.create2(temp, showWarnings = FALSE)
  }

  #--- Load previously created rSOILWAT2 run objets
  if (file.exists(f_sw_input) && ((tasks$create == 1L && opt_behave[["resume"]]) ||
    (tasks$create == -1L && any(tasks$execute == 1L, tasks$aggregate == 1L)))) {

    # load objects: swRunScenariosData, i_sw_weatherList, grasses.c3c4ann.fractions,
    #   ClimatePerturbationsVals, isim_time, simTime2
    load(f_sw_input)
    tasks$create <- 2L
  }


  #----Get preparations done
  if (all(unlist(tasks) %in% c(-1L, 1L))) {
    #------Learn about soil layer structure
    soil_source <- NULL

    #determine number of soil layers = soilLayers_N and soildepth
    if (tasks$create == 1L && (!any(create_treatments == "soilsin") ||
        any(create_treatments == "soilsin") && (is.na(i_sw_input_treatments$soilsin) ||
            identical(i_sw_input_treatments$soilsin, "NA")))) {

      soil_source <- "datafile"
      soildepth <- i_sw_input_soillayers$SoilDepth_cm
      itemp <- 2L + SFSW2_glovars[["slyrs_ids"]]
      layers_depth <- stats::na.omit(as.numeric(i_sw_input_soillayers[itemp]))
      soilLayers_N <- which(soildepth == layers_depth)
      if (length(soilLayers_N) == 0) {
        # soildepth is one of the lower layer boundaries
        # soildepth is not one of the lower layer boundaries, the next deeper layer
        #   boundary is used
        soilLayers_N <- min(length(layers_depth), findInterval(soildepth, layers_depth) + 1)
      }

    } else {
      layers_depth <- if (any(create_treatments == "soilsin") &&
        !is.na(i_sw_input_treatments$soilsin) &&
        !identical(i_sw_input_treatments$soilsin, "NA")) {
          soil_source <- "tr_soilsin"
          slot(tr_soil[[i_sw_input_treatments$soilsin]], "Layers")[, 1]
        } else {
          soil_source <- "default_run"
          unname(rSOILWAT2::swSoils_Layers(swDataFromFiles)[, 1])
        }
      soilLayers_N <- length(layers_depth)
      soildepth <- max(layers_depth)
    }

    #functions to obtain soil layer structures
    #layer sequence
    ld <- setLayerSequence(soilLayers_N)
    layers_depth <- adjustLayersDepth(layers_depth, soilLayers_N)
    layers_width <- getLayersWidth(layers_depth)

    #top and bottom layer aggregation
    DeepestTopLayer <- setDeepestTopLayer(layers_depth, opt_agg[["aon_toplayer_cm"]])
    topL <- setTopLayer(soilLayers_N, DeepestTopLayer)
    bottomL <- setBottomLayer(soilLayers_N, DeepestTopLayer)


    #------Learn about simulation time
    isim_time <- sim_time

    if (any(create_treatments == "YearStart") || any(create_treatments == "YearEnd")) {
      #------time frame of simulation
      if (any(create_treatments == "YearStart")) {
        #year when SOILWAT2 starts the simulation
        isim_time[["simstartyr"]] <- i_sw_input_treatments$YearStart
        #first year that is used for output aggregation, e.g., simstartyr + 1
        isim_time[["startyr"]] <- getStartYear(isim_time[["simstartyr"]], isim_time[["spinup_N"]])
      }
      if (any(create_treatments == "YearEnd")) {
        #year when SOILWAT2 ends the simulation
        isim_time[["endyr"]] <- i_sw_input_treatments$YearEnd
      }

      #------simulation timing needs to be adjusted
      isim_time <- setup_simulation_time(isim_time, add_st2 = FALSE)

      simTime2 <- simTiming_ForEachUsedTimeUnit(isim_time,
        sim_tscales = c("daily", "monthly", "yearly"),
        latitude = i_SWRunInformation$Y_WGS84,
        account_NorthSouth = opt_agg[["adjust_NorthSouth"]],
        use_doy_range = SFSW2_prj_meta[["opt_agg"]][["use_doy_range"]],
        doy_ranges = SFSW2_prj_meta[["opt_agg"]][["doy_ranges"]])

    } else {
      simTime2 <- if (i_SWRunInformation$Y_WGS84 >= 0) {
          isim_time[["sim_time2_North"]]
        } else {
          isim_time[["sim_time2_South"]]
        }
    }

    isim_time[["sim_time2_North"]] <- NULL
    isim_time[["sim_time2_South"]] <- NULL
  }



#------------------------CREATE RUNS
  if (tasks$create == 1L) {
    print_debug(opt_verbosity, tag_simfid, "section", "create simulation")

    EVCO_done <- TRCO_done <- FALSE  #to check whether we get information for evaporation and transpiration coefficients
    TRRG_done <- FALSE #to check whether we get information for transpiration regions

    # Data objects used also during aggregation
    grasses.c3c4ann.fractions <- rep(list(rep(NA, 3)), sim_scens[["N"]]) #Init fractions of C3, C4, and annual grasses of grass-vegetation type fraction; used in create and aggregate
    ClimatePerturbationsVals <- matrix(c(rep(1, 12), rep(0, 24)),
      nrow = sim_scens[["N"]], ncol = 12 * 3, byrow = TRUE) #, dimnames = list(NULL, paste0(rep(paste0("ClimatePerturbations.", c("PrcpMultiplier.m", "TmaxAddand.m", "TminAddand.m")), each = 12), SFSW2_glovars[["st_mo"]], rep(c("_none", "_C", "_C"), each = 12), "_const"))

    #------1. Step: Information for this SOILWAT2-run from prepared SOILWAT2-run stored in dir_in_sw
    #Make a local copy of the swInput object do not want to destroy orignal
    swRunScenariosData <- list()
    swRunScenariosData[[1]] <- swDataFromFiles

    #adjust simulation years
    rSOILWAT2::swYears_StartYear(swRunScenariosData[[1]]) <- as.integer(isim_time[["simstartyr"]])
    rSOILWAT2::swYears_EndYear(swRunScenariosData[[1]]) <- as.integer(isim_time[["endyr"]])

    #------2. Step: a) Information for this SOILWAT2-run from treatment SOILWAT2 input files stored in dir_in_treat
    if (any(create_treatments == "sw"))
      print(paste0(tag_simfid, ": SW treatment is not used because 'rSOILWAT2' package only uses one version of SOILWAT2. Sorry"))
    if (any(create_treatments == "filesin"))
      rSOILWAT2::set_swFiles(swRunScenariosData[[1]]) <- tr_files[[i_sw_input_treatments$filesin]]
    if (any(create_treatments == "prodin"))
      rSOILWAT2::set_swProd(swRunScenariosData[[1]]) <- tr_prod[[i_sw_input_treatments$prodin]]
    if (any(create_treatments == "siteparamin")) {
      rSOILWAT2::set_swSite(swRunScenariosData[[1]]) <- tr_site[[i_sw_input_treatments$siteparamin]]
      TRRG_done <- TRUE
    }
    if (identical(soil_source, "tr_soilsin")) {
      rSOILWAT2::set_swSoils(swRunScenariosData[[1]]) <- tr_soil[[i_sw_input_treatments$soilsin]]
      EVCO_done <- TRCO_done <- TRUE
    }
    if (any(create_treatments == "weathersetupin"))
      rSOILWAT2::set_swWeather(swRunScenariosData[[1]]) <- tr_weather[[i_sw_input_treatments$weathersetupin]]
    if (any(create_treatments == "cloudin"))
      rSOILWAT2::set_swCloud(swRunScenariosData[[1]]) <- tr_cloud[[i_sw_input_treatments$cloudin]]

    #------2. Step: b) Information for this SOILWAT2-run from treatment chunks stored in dir_in_treat
    #Do the lookup stuff for experimental design that was done for the treatment design before the call to call_OneSite, but couldn't for the experimental design because at that time information was unkown

    #----- Begin carbon effects
    if (!is.na(i_sw_input_treatments$UseCO2BiomassMultiplier) &&
      i_sw_input_treatments$UseCO2BiomassMultiplier == 1) {
      rSOILWAT2::swCarbon_Use_Bio(swRunScenariosData[[1]]) <- 1L
    } else {
      rSOILWAT2::swCarbon_Use_Bio(swRunScenariosData[[1]]) <- 0L
    }

    if (!is.na(i_sw_input_treatments$UseCO2WUEMultiplier) &&
      i_sw_input_treatments$UseCO2WUEMultiplier == 1) {
      rSOILWAT2::swCarbon_Use_WUE(swRunScenariosData[[1]]) <- 1L
    } else {
      rSOILWAT2::swCarbon_Use_WUE(swRunScenariosData[[1]]) <- 0L
    }
    # End carbon effects -----

    if (any(sw_input_experimentals_use[c("LookupEvapCoeffFromTable",
                                     "LookupTranspRegionsFromTable",
                                     "LookupSnowDensityFromTable")]) &&
        any(!done_prior)) {

      do_lookup <- list(
        LookupEvapCoeffFromTable = list(
          flag = "LookupEvapCoeffFromTable",
          pattern = "EvapCoeff",
          tr_input = tr_input_EvapCoeff,
          sw_input_use = sw_input_soils_use,
          sw_input = i_sw_input_soils,
          nvars = SFSW2_glovars[["slyrs_maxN"]],
          do_fill = FALSE),

        LookupTranspRegionsFromTable = list(
          flag = "LookupTranspRegionsFromTable",
          pattern = "TranspRegion",
          tr_input = tr_input_TranspRegions,
          sw_input_use = sw_input_soils_use,
          sw_input = i_sw_input_soils,
          nvars = SFSW2_glovars[["slyrs_maxN"]],
          do_fill = FALSE),

        LookupSnowDensityFromTable = list(
          flag = "LookupSnowDensityFromTable",
          pattern = "(snowd)|(SnowD_Hemisphere)",
          tr_input = tr_input_SnowD,
          sw_input_use = sw_input_cloud_use,
          sw_input = i_sw_input_cloud,
          nvars = 12 + 1,
          do_fill = TRUE,
          fill_pattern = "snowd",
          fill_value = 76)  # 76 kg/m3 = median of medians over 6 sites in Colorado and Wyoming: Judson, A. & Doesken, N. (2000) Density of Freshly Fallen Snow in the Central Rocky Mountains. Bulletin of the American Meteorological Society, 81, 1577-1587.
      )

      for (pc in do_lookup) {
        if (sw_input_experimentals_use[pc$flag] && !done_prior[pc$flag]) {
          if (any(is.na(i_sw_input_treatments[[pc$flag]])) ||
             !all(unique(i_sw_input_treatments[[pc$flag]]) %in% rownames(pc$tr_input))) {

            print(paste0(tag_simfid, ": ERROR: ", shQuote(pc$flag), " column in ",
              "experimental table cannot have any NAs or name is not in tr_input table."))

            tasks$create <- 0L
            break

          } else {
            tempdat <- try(get.LookupFromTable(
              pattern = pc$flag,
              trtype = i_sw_input_treatments[pc$flag],
              tr_input = pc$tr_input,
              sw_input_use = pc$sw_input_use,
              sw_input = pc$sw_input,
              nvars = pc$nvars))

            if (!inherits(tempdat, "try-error")) {
              if (!is.null(pc$do_fill) && pc$do_fill)
                tempdat <- fill_empty(tempdat, pattern = pc$fill_pattern, fill = pc$fill_value)

              assign(pc$sw_input_use, tempdat$sw_input_use)
              assign(pc$sw_input, tempdat$sw_input)

            } else {
              tasks$create <- 0L
              break
            }
          }
        }
      }
    }

    #Treatment chunks
    print_debug(opt_verbosity, tag_simfid, "creating", "LookupTranspCoeff")
    do_vegs <- list(
      veg = c("Grass", "Shrub", "Tree", "Forb"),
      flag = c("LookupTranspCoeffFromTable_Grass", "LookupTranspCoeffFromTable_Shrub",
                "LookupTranspCoeffFromTable_Tree", "LookupTranspCoeffFromTable_Forb"),
      adjustType = c("positive", "inverse", "inverse", "inverse"))

    for (k in seq_along(do_vegs[["veg"]])) {
      print_debug(opt_verbosity, tag_simfid, "creating", do_vegs[["veg"]][k])

      if (any(create_treatments == do_vegs[["flag"]][k])) {
        temp <- is.na(i_sw_input_treatments[1, do_vegs[["flag"]][k]])
        temp1 <- !all(i_sw_input_treatments[1, do_vegs[["flag"]][k]] %in% colnames(tr_input_TranspCoeff))
        if (temp || temp1) {
          if (temp)
            print(paste0(tag_simfid, ": ", do_vegs[["flag"]][k], " cannot be NA"))
          if (temp1)
            print(paste0(tag_simfid, ": ", do_vegs[["flag"]][k], " name(s) are not in ",
              "'tr_input_TranspCoeff' table column names"))
          tasks$create <- 0L
          break

        } else {
          trco <- TranspCoeffByVegType(
            tr_input_code = tr_input_TranspCoeff_Code, tr_input_coeff = tr_input_TranspCoeff,
            soillayer_no = soilLayers_N,
            trco_type = i_sw_input_treatments[1, do_vegs[["flag"]][k]],
            layers_depth = layers_depth,
            adjustType = do_vegs[["adjustType"]][k])

          if (!any(is.na(trco)) || sum(trco, na.rm = TRUE) > 0) {#trco does not have NA and sum is greater than 0.
            #set the use flags
            i.temp <- grep(paste0(do_vegs[["veg"]][k], "_TranspCoeff"), names(sw_input_soils_use))
            sw_input_soils_use[i.temp[seq_along(trco)]] <- TRUE
            if (length(i.temp) > length(trco))
              sw_input_soils_use[i.temp[(length(trco) + 1):length(i.temp)]] <- FALSE
            #add data to sw_input_soils
            i_sw_input_soils[i.temp[seq_along(trco)]] <- trco
          } else {
            print(paste0(tag_simfid, ": the function 'TranspCoeffByVegType' returned NA ",
              "or does not sum to greater than 0 for type", do_vegs[["adjustType"]][k]))
            tasks$create <- 0L
            break
          }
        }
      }
    }

    #the monthly ppt-shifts are extracted, but written to the weathersetup input file only at the end of the create section 'copy and make climate scenarios from datafiles', because they are multiplied with any climate change factors
    ppt_scShift <- rep(1, times = 12)
    if (any(create_treatments == "LookupShiftedPPTScenarios")) {
      ppt_scShift <- tr_input_shiftedPPT[which(rownames(tr_input_shiftedPPT) == i_sw_input_treatments[1, "LookupShiftedPPTCategory"]), (ts <- which(colnames(tr_input_shiftedPPT) == paste0(i_sw_input_treatments$LookupShiftedPPTScenarios, "_m1"))):(ts+11)][SFSW2_glovars[["st_mo"]]]
    }

    if (any(create_treatments == "LookupClimatePPTScenarios") | any(create_treatments == "LookupClimateTempScenarios")) {
      clim_scale <- rSOILWAT2::swWeather_MonScalingParams(swRunScenariosData[[1]])[, 1:3]

      #Treatment chunk = climate precipitation scenarios
      if (any(create_treatments == "LookupClimatePPTScenarios")) {
        clim_scale[, 1] <- tr_input_climPPT[SFSW2_glovars[["st_mo"]], which(colnames(tr_input_climPPT) == i_sw_input_treatments$LookupClimatePPTScenarios)]
      }
      #Treatment chunk = climate temperature scenarios
      if (any(create_treatments == "LookupClimateTempScenarios")) {
        clim_scale[, 2] <- clim_scale[, 3] <- tr_input_climTemp[SFSW2_glovars[["st_mo"]], which(colnames(tr_input_climTemp) == i_sw_input_treatments$LookupClimateTempScenarios)]
      }

      rSOILWAT2::swWeather_MonScalingParams(swRunScenariosData[[1]])[, 1:3] <- clim_scale

      rm(clim_scale)
    }


    #------4. Step: Information from datafiles are added if flagged 'use' to SOILWAT2 input files
    #add information from datafile to cloudin
    print_debug(opt_verbosity, tag_simfid, "creating", "cloudin")

    wind <- with(i_sw_input_cloud, data.frame(wind_ms_1, wind_ms_2, wind_ms_3, wind_ms_4,
      wind_ms_5, wind_ms_6, wind_ms_7, wind_ms_8, wind_ms_9, wind_ms_10, wind_ms_11,
      wind_ms_12))
    do_wind <- opt_sim[["windspeed_obs_height_m"]] != SFSW2_glovars[["windspeed_height_m"]]
    if (do_wind)
      wind <- adjust.WindspeedHeight(uz = wind, height = opt_sim[["windspeed_obs_height_m"]])

    if (any(sw_input_cloud_use) || do_wind) {
      #sky cover
      if (any(sw_input_cloud_use[grepl("SkyC", names(sw_input_cloud_use))])) {
        sky <- with(i_sw_input_cloud, data.frame(SkyC_1, SkyC_2, SkyC_3, SkyC_4, SkyC_5, SkyC_6, SkyC_7, SkyC_8, SkyC_9, SkyC_10, SkyC_11, SkyC_12))
        rSOILWAT2::swCloud_SkyCover(swRunScenariosData[[1]]) <- round(as.double(sky), 0)
      }
      #wind speed
      if (any(sw_input_cloud_use[grepl("wind", names(sw_input_cloud_use))]) | do_wind) {
        rSOILWAT2::swCloud_WindSpeed(swRunScenariosData[[1]]) <- round(as.double(wind), 2)
      }
      #relative humidity
      if (any(sw_input_cloud_use[grepl("RH", names(sw_input_cloud_use))])) {
        rh <- with(i_sw_input_cloud, data.frame(RH_1, RH_2, RH_3, RH_4, RH_5, RH_6, RH_7, RH_8, RH_9, RH_10, RH_11, RH_12))
        rSOILWAT2::swCloud_Humidity(swRunScenariosData[[1]]) <- round(as.double(rh), 0)
      }
      #snow density
      if (any(sw_input_cloud_use[grepl("snowd", names(sw_input_cloud_use))])) {
        snowd <- with(i_sw_input_cloud, data.frame(snowd_1, snowd_2, snowd_3, snowd_4, snowd_5, snowd_6, snowd_7, snowd_8, snowd_9, snowd_10, snowd_11, snowd_12))
        if (i_SWRunInformation$Y_WGS84 < 0 && i_sw_input_cloud$SnowD_Hemisphere == "N" || i_SWRunInformation$Y_WGS84 > 0 && i_sw_input_cloud$SnowD_Hemisphere == "S") {  #adjust for hemisphere only if location and data are opposite
          snowd <- c(snowd[7:12], snowd[1:6])
        }
        rSOILWAT2::swCloud_SnowDensity(swRunScenariosData[[1]]) <- round(as.double(snowd), 1)
      }
    }

    #add vegetation information  from datafile to prodin
    print_debug(opt_verbosity, tag_simfid, "creating", "vegetation")

    if (any(sw_input_prod_use)) {
      #constant canopy height
      ids <- grepl("CanopyHeight_Constant", names(sw_input_prod_use))
      use <- sw_input_prod_use[ids]
      if (any(use)) {
        def <- rSOILWAT2::swProd_CanopyHeight(swRunScenariosData[[1]])
        temp <- colnames(def)
        def_names <- substr(temp, 1, nchar(temp) - 2)
        for (k in seq_along(def_names)) {
          itemp <- grep(def_names[k], names(use))
          if (length(itemp) == 1 && use[itemp]) {
            def["height_cm", k] <- as.numeric(i_sw_input_prod[ids][itemp])
          }
        }
        rSOILWAT2::swProd_CanopyHeight(swRunScenariosData[[1]]) <- def
      }

      #composition
      temp <- try(rSOILWAT2::set_requested_flags(swIn = swRunScenariosData[[1]],
        tag = "Composition", use = sw_input_prod_use, values = i_sw_input_prod,
        fun = "swProd_Composition", reset = TRUE, default = 0))
      if (inherits(temp, "try-error")) {
        tasks$create <- 0L
      } else {
        swRunScenariosData[[1]] <- temp
      }

      #albedo
      temp <- try(rSOILWAT2::set_requested_flags(swIn = swRunScenariosData[[1]],
        tag = "Albedo", use = sw_input_prod_use, values = i_sw_input_prod,
        fun = "swProd_Albedo", reset = FALSE))
      if (inherits(temp, "try-error")) {
        tasks$create <- 0L
      } else {
        swRunScenariosData[[1]] <- temp
      }

      #flag for hydraulic redistribution
      temp <- try(rSOILWAT2::set_requested_flags(swIn = swRunScenariosData[[1]],
        tag = "HydRed", use = sw_input_prod_use, values = i_sw_input_prod,
        fun = "swProd_HydrRedstro_use", reset = FALSE))
      if (inherits(temp, "try-error")) {
        tasks$create <- 0L
      } else {
        swRunScenariosData[[1]] <- temp
      }

      #flag for transpiration-critical SWP (MPa)
      temp <- try(rSOILWAT2::set_requested_flags(swIn = swRunScenariosData[[1]],
        tag = "SWPcrit_MPa", use = sw_input_prod_use, values = i_sw_input_prod,
        fun = "swProd_CritSoilWaterPotential", reset = FALSE))
      if (inherits(temp, "try-error")) {
        tasks$create <- 0L
      } else {
        swRunScenariosData[[1]] <- temp
      }

      for (k in c("Grass", "Shrub", "Tree", "Forb")) {
        rSOILWAT2::swProd_MonProd_veg(swRunScenariosData[[1]], k) <- update_biomass(
        fg = k, use = sw_input_prod_use, prod_input = i_sw_input_prod,
        prod_default = swRunScenariosData[[1]]@prod)
      }
    }

    #add site information to siteparamin
    print_debug(opt_verbosity, tag_simfid, "creating", "site parameters")

    if (any(sw_input_site_use)) {
      flags <- c("SWC_min", "SWC_init", "SWC_wet")
      site_use <- sw_input_site_use[flags]
      if (any(site_use))
        rSOILWAT2::swSite_SWClimits(swRunScenariosData[[1]])[site_use] <-
          as.numeric(i_sw_input_site[flags][site_use])

      flags <- c("SWC_YearlyReset", "SWC_Deepdrain")
      site_use <- sw_input_site_use[flags]
      if (any(site_use))
        rSOILWAT2::swSite_ModelFlags(swRunScenariosData[[1]])[site_use] <-
          as.logical(i_sw_input_site[flags][site_use])

      flags <- c("PET_multiplier", "RunoffPercent_fromPondedWater", "RunonPercent_fromPondedWater")
      site_use <- sw_input_site_use[flags]
      if (any(site_use))
        rSOILWAT2::swSite_ModelCoefficients(swRunScenariosData[[1]])[site_use] <-
          as.numeric(i_sw_input_site[flags][site_use])

      if (sw_input_site_use["Param_UnsaturatedPercolation"]) {
        rSOILWAT2::swSite_DrainageCoefficient(swRunScenariosData[[1]]) <-
          as.numeric(i_sw_input_site$Param_UnsaturatedPercolation)
      }

      flags <- c("Latitude", "Altitude", "Slope", "Aspect")
      site_use <- sw_input_site_use[flags]
      if (any(site_use))
        rSOILWAT2::swSite_IntrinsicSiteParams(swRunScenariosData[[1]])[site_use] <-
          as.numeric(i_sw_input_site[flags][site_use])

      if (sw_input_site_use["SoilTemp_Flag"]) {
        rSOILWAT2::swSite_SoilTemperatureFlag(swRunScenariosData[[1]]) <-
          as.logical(i_sw_input_site$SoilTemp_Flag)
      }

      flagsIn <- c("SoilTemp_BiomassLimiter_gPERm2", "SoilTemp_T1constant_a",
        "SoilTemp_T1constant_b", "SoilTemp_T1constant_c", "SoilTemp_SoilThermCondct",
        "SoilTemp_cs_constant", "SoilTemp_SpecificHeatCapacity",
        "SoilTemp_deltaX_cm", "SoilTemp_MaxDepth_cm")
      flagsSW <- c("BiomassLimiter_g/m^2", "T1constant_a", "T1constant_b", "T1constant_c",
        "cs_constant_SoilThermCondct", "cs_constant", "sh_constant_SpecificHeatCapacity",
        "ConstMeanAirTemp", "deltaX_Param", "MaxDepth")[c(1:7, 9:10)]
      site_use <- sw_input_site_use[flagsIn]
      if (any(site_use))
        rSOILWAT2::swSite_SoilTemperatureConsts(swRunScenariosData[[1]])[flagsSW][site_use] <-
          as.numeric(i_sw_input_site[flagsIn][site_use])
    }

    rSOILWAT2::swSite_IntrinsicSiteParams(swRunScenariosData[[1]])[1] <-
      as.numeric(i_SWRunInformation$Y_WGS84 * pi / 180)

    if (is.finite(i_SWRunInformation$ELEV_m))
      rSOILWAT2::swSite_IntrinsicSiteParams(swRunScenariosData[[1]])[2] <-
        as.numeric(i_SWRunInformation$ELEV_m)

    #add soil information to soilsin
    print_debug(opt_verbosity, tag_simfid, "creating", "soils")

    # Use fixed column names
    soil_cols <- c("depth_cm", "matricd", "gravel_content", "EvapBareSoil_frac",
                    "transpGrass_frac", "transpShrub_frac", "transpTree_frac",
                    "transpForb_frac", "sand", "clay", "imperm", "soilTemp_c")
    soil_swdat <- rSOILWAT2::swSoils_Layers(swRunScenariosData[[1]])
    dimnames(soil_swdat)[[2]] <- soil_cols

    done.Imperm_L1 <- FALSE
    if (sw_input_soils_use["Imperm_L1"] && any(create_treatments == "soilsin")) {
      soil_swdat[1, "imperm"] <- i_sw_input_soils$Imperm_L1
      done.Imperm_L1 <- TRUE
    }

    use_transpregion <- sw_input_soils_use[paste0("TranspRegion_L", ld)]
    if (!identical(soil_source, "tr_soilsin") &&
      sum(sw_input_soils_use) + {if (done.Imperm_L1) -1 else 0} - sum(use_transpregion) > 0) {

      # Calculate soil layer structure, because any(create_treatments == "soilsin") and soilsin may have a different soil layer structure than the datafiles
      temp <- as.numeric(stats::na.omit(unlist(i_sw_input_soillayers[paste0("depth_L", SFSW2_glovars[["slyrs_ids"]])])))
      layers_depth.datafile <- temp[temp <= as.numeric(i_sw_input_soillayers["SoilDepth_cm"])]
      if (length(layers_depth.datafile) == 0) {
        # this condition arises if i_sw_input_soillayers["SoilDepth_cm"] < i_sw_input_soillayers["depth_L1"]
        layers_depth.datafile <- temp[1]
      }

      if (!identical(layers_depth.datafile, soil_swdat[, "depth_cm"])) {
        # different soil layer structure in soilsin and datafile AND since variables are
        # flagged in sw_input_soils_use => use only datafile values
        soilLayers_N <- findInterval(i_sw_input_soillayers["SoilDepth_cm"] - SFSW2_glovars[["toln"]],
          c(0, layers_depth.datafile))
        soilLayers_N <- min(length(layers_depth.datafile), soilLayers_N, na.rm = TRUE)
        soilLayers_N <- max(1, soilLayers_N, na.rm = TRUE)
        layers_depth <- adjustLayersDepth(layers_depth.datafile, soilLayers_N)
        layers_width <- getLayersWidth(layers_depth)
        ld <- setLayerSequence(soilLayers_N)

        DeepestTopLayer <- setDeepestTopLayer(layers_depth, opt_agg[["aon_toplayer_cm"]])
        topL <- setTopLayer(soilLayers_N, DeepestTopLayer)
        bottomL <- setBottomLayer(soilLayers_N, DeepestTopLayer)
      }

      #compile soil information from both sources
      soildat <- matrix(0, nrow = soilLayers_N, ncol = length(soil_cols),
                        dimnames = list(NULL, soil_cols))
      soildat[, "depth_cm"] <- layers_depth.datafile[ld]
      infile_cols <- names(sw_input_soils_use)

      coefs <- list(infile = c("Matricd", "GravelContent", "EvapCoeff", "Grass_TranspCoeff",
                                "Shrub_TranspCoeff", "Tree_TranspCoeff", "Forb_TranspCoeff",
                                "Sand", "Clay", "Imperm", "SoilTemp"),
                    sw = soil_cols[-1])
      for (iv in seq_along(coefs[[1]])) {
        icol <- grep(coefs[["infile"]][iv], infile_cols, ignore.case = TRUE, value = TRUE)
        if (length(icol) > soilLayers_N)
          icol <- icol[ld]

        if (length(icol) > 0) {
          luse <- list(use = which(sw_input_soils_use[icol]),
                        other = intersect(
                                  which(!sw_input_soils_use[icol]),
                                  seq_len(dim(soil_swdat)[1])))
          for (k in 1:2) if (any(luse[[k]])) {
            temp <- if (k == 1L) {
                as.numeric(i_sw_input_soils[, icol[luse[[k]]]])
              } else {
                soil_swdat[luse[[k]], coefs[["sw"]][iv]]
              }
            if (isTRUE(grepl("coeff", coefs[["infile"]][iv], ignore.case = TRUE)))
              temp <- scale_by_sum(temp)
            soildat[luse[[k]], coefs[["sw"]][iv]] <- temp
          }
        }
      }

      # Adjust deepest soil layer if there is no soil information
      if (opt_sim[["fix_depth_to_layers"]]) {
        for (k in soilLayers_N:1) {
          temp <- soildat[k, c("matricd", "sand", "clay")]
          if (any(!is.na(temp)))
            break
        }
        if (soilLayers_N != k) {
          soilLayers_N <- k
          layers_depth <- adjustLayersDepth(layers_depth, soilLayers_N)
          layers_width <- getLayersWidth(layers_depth)
          ld <- setLayerSequence(soilLayers_N)

          DeepestTopLayer <- setDeepestTopLayer(layers_depth, opt_agg[["aon_toplayer_cm"]])
          topL <- setTopLayer(soilLayers_N, DeepestTopLayer)
          bottomL <- setBottomLayer(soilLayers_N, DeepestTopLayer)

          soildat <- soildat[ld, , drop = FALSE]
        }
      }

      # Impute missing/bad soil data from previous layer
      icol_excl <- which(soil_cols %in% "soilTemp_c")
      icols <- seq_along(soil_cols)[-icol_excl]
      bad_data <- !check_soil_data(soildat[, -icol_excl, drop = FALSE])

      if (any(bad_data)) for (l in ld) {
        lbad <- bad_data[l, ]
        if (any(lbad)) {
          if (l > 1L) {
            soildat[l, icols[lbad]] <- soildat[l - 1L, icols[lbad]]
            print(paste0(tag_simfid, ": layer ", l, " filled in with data imputed from ",
              "previous layer: ", paste(names(lbad)[lbad], collapse = ", ")))

          } else {
            print(paste0(tag_simfid, ": data missing for 1st layer -> no data to impute: ",
              "simulation will fail"))
            print(soildat[l, icols])
            tasks$create <- 0L
            break
          }
        }
      }

      soil_swdat <- soildat

    } else {
      # Check soil
      check_soil <- check_soil_data(soil_swdat)

      if (!all(check_soil)) {
        print(paste0(tag_simfid, ": soil data didn't pass quality checks for:",
          paste(soil_cols[colSums(!check_soil) > 0], collapse = ", ")))
        print(soil_swdat)
        tasks$create <- 0L
      }

    }

    rSOILWAT2::swSoils_Layers(swRunScenariosData[[1]]) <- soil_swdat

    #add transpiration regions information to siteparamin
    print_debug(opt_verbosity, tag_simfid, "creating", "transpiration regions")

    if (sum(use_transpregion) > 0) {
      tr <- max(tr.layers <- stats::na.omit(as.numeric(i_sw_input_soils[paste0("TranspRegion_L", ld)]))) # max transpiration region

      TranspirationRegions <- matrix(data = NA, nrow = 4, ncol = 2)
      colnames(TranspirationRegions) <- c("ndx", "layer")

      ltreg.last <- 0
      for (tri in 1:4) {
        ltreg <- ifelse(length(ind <- which(tr.layers == tri)) > 0, max(ind), -1)
        ltreg <- ifelse(ltreg>ltreg.last, ltreg, ltreg.last+1)
        ltreg <- ifelse(ltreg>soilLayers_N & tri == 1, soilLayers_N, ltreg)

        if (tri <= tr & tri <= soilLayers_N & ltreg <= soilLayers_N | tri == 1) TranspirationRegions[tri, ] <- as.integer(c(tri, ltreg))
        ltreg.last <- ltreg
      }
      tr_rows <- rowSums(is.na(TranspirationRegions)) != 2 #used to get rid of NA rows
      if (sum(tr_rows) == 0) {
        print(paste0(tag_simfid, ": 'transpiration regions' cannot be empty."))
      } else if (sum(tr_rows) == 1) {
        rSOILWAT2::swSite_TranspirationRegions(swRunScenariosData[[1]]) <- matrix(data = TranspirationRegions[tr_rows, ], nrow = 1, ncol = 2, byrow = T, dimnames = list(numeric(), c("ndx", "layer")))
        TRRG_done <- TRUE
      } else {
        rSOILWAT2::swSite_TranspirationRegions(swRunScenariosData[[1]]) <- TranspirationRegions[tr_rows, ]
        TRRG_done <- TRUE
      }
    }

    #add weather setup information to weatherin
    if (sw_input_weather_use["SnowFlag"])
      rSOILWAT2::swWeather_UseSnow(swRunScenariosData[[1]]) <- as.logical(i_sw_input_weather$SnowFlag)
    if (sw_input_weather_use["SnowDrift_Percent"])
      rSOILWAT2::swWeather_pct_SnowDrift(swRunScenariosData[[1]]) <- i_sw_input_weather$SnowDrift_Percent
    if (sw_input_weather_use["RunOffOnPerSnowmelt_Percent"])
      rSOILWAT2::swWeather_pct_SnowRunoff(swRunScenariosData[[1]]) <- i_sw_input_weather$RunOffOnPerSnowmelt_Percent
    rSOILWAT2::swWeather_FirstYearHistorical(swRunScenariosData[[1]]) <- isim_time[["simstartyr"]]

    # Set simulation_timescales fix to daily, monthly, and yearly
    rSOILWAT2::swOUT_TimeStepsForEveryKey(swRunScenariosData[[1]]) <- c(daily = 0, monthly = 2, yearly = 3)

    #############Get Weather Data################
    print_debug(opt_verbosity, tag_simfid, "creating", "daily weather")
    i_sw_weatherList <- list()

    if (!opt_sim[["use_dbW_current"]]) {
      if (i_SWRunInformation$dailyweather_source == "Maurer2002_NorthAmerica") {
        i_sw_weatherList[[1]] <- ExtractGriddedDailyWeatherFromMaurer2002_NorthAmerica(
                  dir_data = project_paths[["dir_maurer2002"]],
                  cellname = with(i_SWRunInformation,
                    create_filename_for_Maurer2002_NorthAmerica(X_WGS84, Y_WGS84)),
                  start_year = isim_time[["simstartyr"]],
                  end_year = isim_time[["endyr"]],
                  verbose = opt_verbosity[["verbose"]])

      } else if (i_SWRunInformation$dailyweather_source == "DayMet_NorthAmerica") {
        i_sw_weatherList[[1]] <- with(i_SWRunInformation,
          ExtractGriddedDailyWeatherFromDayMet_NorthAmerica_swWeather(
            dir_data = dir_daymet,
            site_ids = NULL,
            coords_WGS84 = c(X_WGS84, Y_WGS84),
            start_year = isim_time[["simstartyr"]], end_year = isim_time[["endyr"]]))

      } else if (i_SWRunInformation$dailyweather_source == "LookupWeatherFolder") {
        # Read weather data from folder
        i_sw_weatherList[[1]] <- try(rSOILWAT2::getWeatherData_folders(
          LookupWeatherFolder = file.path(project_paths[["dir_in_treat"]], "LookupWeatherFolder"),
          weatherDirName = local_weatherDirName(i_sim, sim_size[["runsN_master"]], sim_scens[["N"]],
            fnames_out[["dbOutput"]]), filebasename = opt_sim[["tag_WeatherFolder"]],
          startYear = isim_time[["simstartyr"]], endYear = isim_time[["endyr"]]),
          silent = TRUE)
      }

    } else {
      #---Extract weather data
      weather_label_cur <- try(local_weatherDirName(i_sim, sim_size[["runsN_master"]], sim_scens[["N"]],
        fnames_out[["dbOutput"]]), silent = TRUE)

      if (is.na(weather_label_cur))
        weather_label_cur <- try({function() stop(tag_simfid, ": Output DB ",
          basename(fnames_out[["dbOutput"]]), " has no information about weather data")}(),
          silent = TRUE)

      if (inherits(weather_label_cur, "try-error")) {
        i_sw_weatherList <- weather_label_cur

      } else {
        temp <- if (opt_sim[["use_dbW_future"]]) {
            seq_len(sim_scens[["N"]])
          } else 1L

        i_sw_weatherList <- try(lapply(sim_scens[["id"]][temp], function(scen)
          rSOILWAT2::dbW_getWeatherData(Label = weather_label_cur,
            startYear = isim_time[["simstartyr"]], endYear = isim_time[["endyr"]],
            Scenario = scen)), silent = TRUE)
      }
    }

    #Check that extraction of weather data was successful
    if (inherits(i_sw_weatherList, "try-error") || length(i_sw_weatherList) == 0) {
      tasks$create <- 0L
      print(paste0(tag_simfid, ": i_sw_weatherList ERROR: ", i_sw_weatherList))
    }

    #copy and make climate scenarios from datafiles
    if (tasks$create > 0L) for (sc in seq_len(sim_scens[["N"]])) {
      P_id <- it_Pid(i_sim, sim_size[["runsN_master"]], sc, sim_scens[["N"]])
      tag_simpidfid <- paste0("[run", i_sim, "/PID", P_id, "/sc", sc, "/work", fid, "]")

      if (sc > 1) {
        swRunScenariosData[[sc]] <- swRunScenariosData[[1]]

        # How many years in the future is this simulation?
        # The delta year was originaly designed to only be used by swCarbon to grab the correct ppm values,
        # but has since been used to also display the correct years in runDataSC, so this information is
        # extracted regardless of whether or not CO2 effects are being used
        delta_yr <- sim_scens[["df"]][sc - 1, "Delta_yrs"]
        if (!is.na(delta_yr))
          rSOILWAT2::swCarbon_DeltaYear(swRunScenariosData[[sc]]) <- as.integer(delta_yr)

      } else {
        if (prj_todos[["need_cli_means"]]) {
          print_debug(opt_verbosity, tag_simpidfid, "creating", "climate")

          do.C4vars <- any(create_treatments == "PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996") || isTRUE(prj_todos[["aon"]][["dailyC4_TempVar"]])
          #redo SiteClimate_Ambient
          SiteClimate_Ambient <- calc_SiteClimate(weatherList = i_sw_weatherList[[1]],
            year.start = min(isim_time$useyrs), year.end = max(isim_time$useyrs),
            do.C4vars = do.C4vars, simTime2 = simTime2)
        }
      }

      #----- Begin CO2 effects
      # CO2 effects rely on the information of the current scenario, so the extraction of its Lookup data
      # doesn't occur until now
      if (sw_input_experimentals_use["LookupCarbonScenarios"]) {
        if (is.na(i_sw_input_treatments$LookupCarbonScenarios)) {
          tasks$create <- 0L
          print(paste0(tag_simfid, ": ERROR: An empty value was provided for LookupCarbonScenarios"))
          break
        }

        scenario_CO2 <- "Default"

        # Are we modelling a scenario?
        if (sc > 1) {
          # Did the user request to use the built-in scenario information?
          if (toupper(i_sw_input_treatments$LookupCarbonScenarios) == "FILL")
            scenario_CO2 <- sim_scens[["df"]][sc - 1, "ConcScen"]
        }

        # Did the user override the scenario name?
        if (toupper(i_sw_input_treatments$LookupCarbonScenarios) != "FILL")
          scenario_CO2 <- i_sw_input_treatments$LookupCarbonScenarios

        # Save the scenario to the input object just so that the user can see it
        rSOILWAT2::swCarbon_Scenario(swRunScenariosData[[sc]]) <- scenario_CO2

        scenario_index <- which(toupper(colnames(tr_input_CarbonScenario)) == toupper(scenario_CO2))

        # Was a scenario found?
        if (length(scenario_index) == 0) {
          tasks$create <- 0L
          print(paste0(tag_simfid, ": ERROR: Scenario ", scenario_CO2,
            " was not found in LookupCarbonScenarios.csv"))
          break
        }

        # Normally, we would also check for duplicate scenarios, but when the CSV is read in, duplicate column headers
        # are already accounted for by incrementing the name. For instance, having two RCP85 scenarios result in these
        # headers: RCP85, RCP85.1

        # Extract CO2 concentration values in units of ppm into swCarbon
        ids_years <- match(isim_time$simstartyr:isim_time$endyr + rSOILWAT2::swCarbon_DeltaYear(swRunScenariosData[[sc]]),
          tr_input_CarbonScenario[, "Year"], nomatch = 0)
        # Convert possible integers to numeric
        tr_input_CarbonScenario[ids_years, scenario_index] <- as.numeric(unlist(tr_input_CarbonScenario[ids_years, scenario_index]))
        scenarioCO2_ppm <- tr_input_CarbonScenario[ids_years, c(1, scenario_index)]
        colnames(scenarioCO2_ppm) <- c("Year", "CO2ppm")

        rSOILWAT2::swCarbon_CO2ppm(swRunScenariosData[[sc]]) <- as.matrix(scenarioCO2_ppm,
          rownames.force = TRUE)
      }
      # End CO2 effects -----

      if (!opt_sim[["use_dbW_future"]]) {
        #get climate change information
        cols_climscen_val <- lapply(c("PPTmm_m", "TempC_min_m", "TempC_max_m"), function(flag)
          paste0(flag, SFSW2_glovars[["st_mo"]], "_sc", formatC(sc - 1, width = 2, format = "d", flag = "0")))
        use_climscen_val <- any(unlist(sw_input_climscen_values_use[unlist(cols_climscen_val)]))

        cols_climscen_delta <- lapply(c("PPTfactor_m", "deltaTempC_min_m", "deltaTempC_max_m"), function(flag)
          paste0(flag, SFSW2_glovars[["st_mo"]], "_sc", formatC(sc - 1, width = 2, format = "d", flag = "0")))
        use_climscen_delta <- any(unlist(sw_input_climscen_use[unlist(cols_climscen_delta)]))

        if (any(use_climscen_val)) {
          #convert climate change values to factors
          #read values from datafile
          pptVal_sc <- unlist(i_sw_input_climscen_values[, cols_climscen_val[[1]]])
          tVal_min_sc <- unlist(i_sw_input_climscen_values[, cols_climscen_val[[2]]])
          tVal_max_sc <- unlist(i_sw_input_climscen_values[, cols_climscen_val[[3]]])
          #calculate change factors
          ppt_sc <- pptVal_sc / (10 * SiteClimate_Ambient$meanMonthlyPPTcm)
          if (sum(abs(tVal_max_sc - tVal_min_sc)) > SFSW2_glovars[["tol"]]) {
            t_min_sc <- tVal_min_sc - SiteClimate_Ambient$minMonthlyTempC
            t_max_sc <- tVal_max_sc - SiteClimate_Ambient$maxMonthlyTempC
          } else { #no information for tmin, tmax by GCM -> tmin = tmax = tmean
            t_min_sc <- t_max_sc <- tVal_min_sc - SiteClimate_Ambient$meanMonthlyTempC
          }
        } else if (any(use_climscen_delta)) {
          #read climate change factors from datafile
          ppt_sc <- unlist(i_sw_input_climscen[, cols_climscen_delta[[1]]])
          t_min_sc <- unlist(i_sw_input_climscen[, cols_climscen_delta[[2]]])
          t_max_sc <- unlist(i_sw_input_climscen[, cols_climscen_delta[[3]]])
        } else {
          ppt_sc <- rep(1, times = 12)
          t_min_sc <- t_max_sc <- rep(0, times = 12)
        }
        #guarantee that all entries are finite: this may not be the case for instance if any(meanMonthlyClimate$meanMonthlyPPTcm == 0)
        ppt_sc <- temp_ppt_sc <- ifelse(is.finite(ppt_sc), ppt_sc, 1)
        t_min_sc <- ifelse(is.finite(t_min_sc), t_min_sc, 0)
        t_max_sc <- ifelse(is.finite(t_max_sc), t_max_sc, 0)

        if (sc > 1) {
          if (any(create_treatments == "ClimateScenario_Temp_PerturbationInMeanSeasonalityBothOrNone") && !grepl("Both", i_sw_input_treatments$ClimateScenario_Temp_PerturbationInMeanSeasonalityBothOrNone, ignore.case = T)) {
            if (grepl("Mean", i_sw_input_treatments$ClimateScenario_Temp_PerturbationInMeanSeasonalityBothOrNone, ignore.case = T)) {
              t_min_sc <- rep(mean(t_min_sc), times = 12)
              t_max_sc <- rep(mean(t_max_sc), times = 12)
            }
            if (grepl("Seasonality", i_sw_input_treatments$ClimateScenario_Temp_PerturbationInMeanSeasonalityBothOrNone, ignore.case = T)) {
              t_min_sc <- t_min_sc - mean(t_min_sc)
              t_max_sc <- t_max_sc - mean(t_max_sc)
            }
            if (grepl("None", i_sw_input_treatments$ClimateScenario_Temp_PerturbationInMeanSeasonalityBothOrNone, ignore.case = T)) {
              t_min_sc <- t_max_sc <- rep(0, times = 12)
            }
          }
          if (any(create_treatments == "ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone") && !grepl("Both", i_sw_input_treatments$ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone, ignore.case = T)) {
            temp_map_sc <- sum(SiteClimate_Ambient$meanMonthlyPPTcm * temp_ppt_sc)
            if (grepl("Mean", i_sw_input_treatments$ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone, ignore.case = T)) ppt_sc = rep(temp_map_sc / SiteClimate_Ambient$MAP_cm, times = 12)
            if (grepl("Seasonality", i_sw_input_treatments$ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone, ignore.case = T)) ppt_sc = ppt_sc * SiteClimate_Ambient$MAP_cm / temp_map_sc
            if (grepl("None", i_sw_input_treatments$ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone, ignore.case = T)) ppt_sc = rep(1, times = 12)
          }
        }

        temp <- rSOILWAT2::swWeather_MonScalingParams(swRunScenariosData[[sc]])
        ppt_old <- temp[, 1]
        t_max_old <- temp[, 2]
        t_min_old <- temp[, 3]

        #write information into weatherin
        if (any(use_climscen_val, use_climscen_delta)) {
          ppt_f <- ppt_sc
          t_min_f <- t_min_sc
          t_max_f <- t_max_sc
        } else {
          ppt_f <- ppt_old
          t_min_f <- t_min_old
          t_max_f <- t_max_old
        }

        MonthlyScalingParams <- matrix(data = c(ppt_f, t_max_f, t_min_f), nrow = 12, ncol = 3)
        colnames(MonthlyScalingParams) <- c("PPT", "MaxT", "MinT")
        rownames(MonthlyScalingParams) <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

        rSOILWAT2::swWeather_MonScalingParams(swRunScenariosData[[sc]])[, 1:3] <- MonthlyScalingParams
        ClimatePerturbationsVals[sc, SFSW2_glovars[["st_mo"]]] <- MonthlyScalingParams[, 1]
        ClimatePerturbationsVals[sc, 12 + SFSW2_glovars[["st_mo"]]] <- MonthlyScalingParams[, 2]
        ClimatePerturbationsVals[sc, 2 * 12 + SFSW2_glovars[["st_mo"]]] <- MonthlyScalingParams[, 3]

        #Update climate data with climate scenario information
        if (prj_todos[["need_cli_means"]]) {
          SiteClimate_Scenario <- list()
          SiteClimate_Scenario$meanMonthlyPPTcm <- SiteClimate_Ambient$meanMonthlyPPTcm * ppt_f
          tmean_f <- apply(cbind(t_min_f, t_max_f), MARGIN = 1, FUN = mean)
          SiteClimate_Scenario$meanMonthlyTempC <- SiteClimate_Ambient$meanMonthlyTempC + tmean_f
          SiteClimate_Scenario$minMonthlyTempC <- SiteClimate_Ambient$minMonthlyTempC + t_min_f
          SiteClimate_Scenario$maxMonthlyTempC <- SiteClimate_Ambient$maxMonthlyTempC + t_max_f
          SiteClimate_Scenario$MAP_cm <- sum(SiteClimate_Scenario$meanMonthlyPPTcm)
          SiteClimate_Scenario$MAT_C <- mean(SiteClimate_Scenario$meanMonthlyTempC)
          if (do.C4vars) {
            SiteClimate_Scenario$dailyTempMin <- SiteClimate_Ambient$dailyTempMin + t_min_f[simTime2$month_ForEachUsedDay]
            SiteClimate_Scenario$dailyTempMean <- SiteClimate_Ambient$dailyTempMean + tmean_f[simTime2$month_ForEachUsedDay]
            SiteClimate_Scenario$dailyC4vars <- sw_dailyC4_TempVar(SiteClimate_Scenario$dailyTempMin, SiteClimate_Scenario$dailyTempMean, simTime2)
          }
        }

      } else {
        SiteClimate_Scenario <- calc_SiteClimate(weatherList = i_sw_weatherList[[sc]],
          year.start = min(isim_time$useyrs), year.end = max(isim_time$useyrs),
          do.C4vars = do.C4vars, simTime2 = simTime2)

        if (sc > 1) {
          ppt_sc <- (temp <- rSOILWAT2::swWeather_MonScalingParams(swRunScenariosData[[sc]]))[, 1]
          t_max <- temp[, 2]
          t_min <- temp[, 3]

          if (any(create_treatments == "ClimateScenario_Temp_PerturbationInMeanSeasonalityBothOrNone") && !grepl("Both", i_sw_input_treatments$ClimateScenario_Temp_PerturbationInMeanSeasonalityBothOrNone, ignore.case = T)) {
            if (grepl("Mean", i_sw_input_treatments$ClimateScenario_Temp_PerturbationInMeanSeasonalityBothOrNone, ignore.case = T)) {
              # -(mean monthly of scenario - mean monthly of current) + (mean annual of scenario - mean annual of current)
              t_min <- -(SiteClimate_Scenario$minMonthlyTempC - SiteClimate_Ambient$minMonthlyTempC) + (SiteClimate_Scenario$MAT_C - SiteClimate_Ambient$MAT_C)
              t_max <- -(SiteClimate_Scenario$maxMonthlyTempC - SiteClimate_Ambient$maxMonthlyTempC) + (SiteClimate_Scenario$MAT_C - SiteClimate_Ambient$MAT_C)
            }
            if (grepl("Seasonality", i_sw_input_treatments$ClimateScenario_Temp_PerturbationInMeanSeasonalityBothOrNone, ignore.case = T)) {
              # -(mean annual of scenario - mean annual of current)
              t_min <- rep(-(SiteClimate_Scenario$MAT_C - SiteClimate_Ambient$MAT_C), 12)
              t_max <- rep(-(SiteClimate_Scenario$MAT_C - SiteClimate_Ambient$MAT_C), 12)
            }
            if (grepl("None", i_sw_input_treatments$ClimateScenario_Temp_PerturbationInMeanSeasonalityBothOrNone, ignore.case = T)) {
              # -(mean monthly of scenario - mean monthly of current)
              t_min <- -(SiteClimate_Scenario$minMonthlyTempC - SiteClimate_Ambient$minMonthlyTempC)
              t_max <- -(SiteClimate_Scenario$maxMonthlyTempC - SiteClimate_Ambient$maxMonthlyTempC)
            }
          }
          if (any(create_treatments == "ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone") && !grepl("Both", i_sw_input_treatments$ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone, ignore.case = T)) {
            if (grepl("Mean", i_sw_input_treatments$ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone, ignore.case = T)) {
              #Mean of weather == mean of scenario, seasonality of weather = seasonality of ambient
              if (isTRUE(all.equal(SiteClimate_Ambient$MAP_cm, 0))) {
                SiteClimate_Ambient$MAP_cm <- SFSW2_glovars[["tol"]]
                if (isTRUE(all.equal(SiteClimate_Scenario$MAP_cm, 0))) {
                  SiteClimate_Scenario$MAP_cm <- SFSW2_glovars[["tol"]]
                  ppt_sc <- rep(0, times = 12)
                } else {
                  warning("Problem with scaling to 'mean' for ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone because of zero precipitation periods")
                }
              }
              if (sum(ppt_sc) > 0) {
                if (sum(temp <- sapply(SiteClimate_Scenario$meanMonthlyPPTcm, FUN = function(x) isTRUE(all.equal(x, 0)))) > 0) {
                  warning("Problem with scaling to 'mean' for ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone because of zero precipitation periods")
                  SiteClimate_Scenario$meanMonthlyPPTcm[temp] <- SFSW2_glovars[["tol"]]
                }
                ppt_sc <- (SiteClimate_Ambient$meanMonthlyPPTcm / SiteClimate_Scenario$meanMonthlyPPTcm) * (SiteClimate_Scenario$MAP_cm / SiteClimate_Ambient$MAP_cm)
              }
            }
            if (grepl("Seasonality", i_sw_input_treatments$ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone, ignore.case = T)) {
              #Mean of weather == mean of ambient, seasonality of weather = seasonality of scenario
              if (isTRUE(all.equal(SiteClimate_Scenario$MAP_cm, 0))) {
                SiteClimate_Scenario$MAP_cm <- SFSW2_glovars[["tol"]]
                if (isTRUE(all.equal(SiteClimate_Ambient$MAP_cm, 0))) {
                  SiteClimate_Ambient$MAP_cm <- SFSW2_glovars[["tol"]]
                  ppt_sc <- rep(0, times = 12)
                } else {
                  warning("Problem with scaling to 'mean' for ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone because of zero precipitation periods")
                }
              }
              if (sum(ppt_sc) > 0) {
                ppt_sc <- rep((SiteClimate_Ambient$MAP_cm / SiteClimate_Scenario$MAP_cm), 12)
              }
            }
            if (grepl("None", i_sw_input_treatments$ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone, ignore.case = TRUE)) {
              #Mean of weather == mean of ambient, seasonality of weather = seasonality of ambient
              if (isTRUE(all.equal(SiteClimate_Ambient$MAP_cm, 0)) && isTRUE(all.equal(SiteClimate_Scenario$MAP_cm, 0))) {
                SiteClimate_Ambient$MAP_cm <- SiteClimate_Scenario$MAP_cm <- SFSW2_glovars[["tol"]]
                ppt_sc <- rep(0, times = 12)
              }
              if (sum(ppt_sc) > 0) {
                if (sum(temp <- sapply(SiteClimate_Scenario$meanMonthlyPPTcm, FUN = function(x) isTRUE(all.equal(x, 0)))) > 0) {
                  warning("Problem with scaling to 'mean' for ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone because of zero precipitation periods")
                  SiteClimate_Scenario$meanMonthlyPPTcm[temp] <- SFSW2_glovars[["tol"]]
                }
                ppt_sc <- (SiteClimate_Ambient$meanMonthlyPPTcm / SiteClimate_Scenario$meanMonthlyPPTcm)
              }
            }
          }
          if (sum(temp <- sapply(SiteClimate_Ambient$meanMonthlyPPTcm, FUN = function(x) isTRUE(all.equal(x, 0)))) > 0) {
            warning("Problem with scaling to 'mean' for ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone because of zero precipitation periods")
            SiteClimate_Ambient$meanMonthlyPPTcm[temp] <- SFSW2_glovars[["tol"]]
          }

          rSOILWAT2::swWeather_MonScalingParams(swRunScenariosData[[sc]])[, 1] <- ppt_sc
          rSOILWAT2::swWeather_MonScalingParams(swRunScenariosData[[sc]])[, 2] <- t_max
          rSOILWAT2::swWeather_MonScalingParams(swRunScenariosData[[sc]])[, 3] <- t_min
          ClimatePerturbationsVals[sc, SFSW2_glovars[["st_mo"]]] <- ppt_sc * SiteClimate_Scenario$meanMonthlyPPTcm / SiteClimate_Ambient$meanMonthlyPPTcm
          ClimatePerturbationsVals[sc, 12 + SFSW2_glovars[["st_mo"]]] <- t_max + (SiteClimate_Scenario$maxMonthlyTempC - SiteClimate_Ambient$maxMonthlyTempC)
          ClimatePerturbationsVals[sc, 2 * 12 + SFSW2_glovars[["st_mo"]]] <- t_min + (SiteClimate_Scenario$minMonthlyTempC - SiteClimate_Ambient$minMonthlyTempC)
        }
      }

      if (any(create_treatments == "LookupShiftedPPTScenarios")) {
        ppt_f <- rSOILWAT2::swWeather_MonScalingParams(swRunScenariosData[[sc]])[, 1]
        ppt_f <- ppt_f * as.numeric(ppt_scShift)
        rSOILWAT2::swWeather_MonScalingParams(swRunScenariosData[[sc]])[, 1] <- ppt_f
        if (opt_sim[["use_dbW_future"]]) {
          ClimatePerturbationsVals[sc, SFSW2_glovars[["st_mo"]]] <- ppt_f * ClimatePerturbationsVals[sc, SFSW2_glovars[["st_mo"]]]
        } else {
          ClimatePerturbationsVals[sc, SFSW2_glovars[["st_mo"]]] <- ppt_f
        }
      }

      #anything that depends on weather
      #------3. Step: Lookup or extract external information that needs to be executed for each run

      #- Initial soil temperatures adjusted to climatic conditions
      print_debug(opt_verbosity, tag_simpidfid, "creating", "soil temperature")

      if (exists("soilTUpper"))
        rm(soilTUpper)

      if (prj_todos[["EstimateConstantSoilTemperatureAtUpperAndLowerBoundaryAsMeanAnnualAirTemperature"]]) {
        rSOILWAT2::swSite_SoilTemperatureConsts(swRunScenariosData[[sc]])["ConstMeanAirTemp"] <- mean(SiteClimate_Scenario$meanMonthlyTempC)
        soilTUpper <- max(-1, mean(SiteClimate_Scenario$meanMonthlyTempC[c(1, 12)]))
        #TODO get this working LOW PR: save data
        #out.temp <- data.frame(i_sim, i_label, soilTUpper, soilTlower)
        #utils::write.csv(out.temp, file = file.path(project_paths[["dir_out_temp"]], flag.icounter, "_", "SoilTempC_atLowerBoundary.csv"), quote = FALSE, row.names = FALSE)

      } else {
        if (sw_input_site_use["SoilTempC_atUpperBoundary"]) {
          soilTUpper <- i_sw_input_site$SoilTempC_atUpperBoundary
        }

        if (sw_input_site_use["SoilTempC_atLowerBoundary"]) {
          rSOILWAT2::swSite_SoilTemperatureConsts(swRunScenariosData[[sc]])["ConstMeanAirTemp"] <- i_sw_input_site$SoilTempC_atLowerBoundary
        }
      }

      if (prj_todos[["EstimateInitialSoilTemperatureForEachSoilLayer"]]) {
        stopifnot(exists("soilTUpper"))

        init.soilTprofile <- EstimateInitialSoilTemperatureForEachSoilLayer(
          layers_depth = layers_depth,
          lower.Tdepth = as.numeric(rSOILWAT2::swSite_SoilTemperatureConsts(swRunScenariosData[[sc]])["MaxDepth"]),
          soilTupper = soilTUpper,
          soilTlower = as.numeric(rSOILWAT2::swSite_SoilTemperatureConsts(swRunScenariosData[[sc]])["ConstMeanAirTemp"]))
        #temporaly save data #TODO get this working
        #out.temp <- data.frame(i_sim, i_label, t(c(init.soilTprofile, rep(NA, times = SFSW2_glovars[["slyrs_maxN"]]-length(init.soilTprofile)))))
        #utils::write.csv(out.temp, file = file.path(project_paths[["dir_out_temp"]], .Platform$file.sep, flag.icounter, "_", "SoilTempC_InitProfile.csv"), quote = FALSE, row.names = FALSE)
      }

      stemp <- paste0("SoilTemp_L", ld)
      use_soil_temp <- sw_input_soils_use[stemp]
      if (any(use_soil_temp)) {
        if (exists("init.soilTprofile")) {
          rSOILWAT2::swSoils_Layers(swRunScenariosData[[sc]])[ld, 12] <-
            init.soilTprofile
        } else {
          rSOILWAT2::swSoils_Layers(swRunScenariosData[[sc]])[use_soil_temp, 12] <-
            as.numeric(i_sw_input_soils[stemp[use_soil_temp]])
        }
      }

      #- Calculate relative composition based on equations
      print_debug(opt_verbosity, tag_simpidfid, "creating", "potential vegetation")

      if (any(create_treatments == "PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996") && i_sw_input_treatments$PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996) {
        #Climate variables
        if (any(create_treatments == "PotentialNaturalVegetation_Composition_basedOnReferenceOrScenarioClimate") && i_sw_input_treatments$PotentialNaturalVegetation_Composition_basedOnReferenceOrScenarioClimate == "Reference") {
          MAP_mm <- SiteClimate_Ambient$MAP_cm*10
          MAT_C <- SiteClimate_Ambient$MAT_C
          monthly.ppt <- SiteClimate_Ambient$meanMonthlyPPTcm*10
          monthly.temp <- SiteClimate_Ambient$meanMonthlyTempC
          dailyC4vars <- SiteClimate_Ambient$dailyC4vars
        } else {
          MAP_mm <- SiteClimate_Scenario$MAP_cm*10
          MAT_C <- SiteClimate_Scenario$MAT_C
          monthly.ppt <- SiteClimate_Scenario$meanMonthlyPPTcm*10
          monthly.temp <- SiteClimate_Scenario$meanMonthlyTempC
          dailyC4vars <- SiteClimate_Scenario$dailyC4vars
        }

        isNorth <- i_SWRunInformation$Y_WGS84 >= 0

        #TODO: Include forbs and bareground in PotNatVeg_Composition_Estimate_ShrubsC3C4_Fraction
        temp <- try(PotNatVeg_Composition_Estimate_ShrubsC3C4_Fraction(MAP_mm, MAT_C,
          mean_monthly_ppt_mm = monthly.ppt, dailyC4vars, isNorth = isNorth,
          shrub_limit = opt_sim[["shrub_limit"]],
          fix_annuals = any(create_treatments == "PotentialNaturalVegetation_CompositionAnnuals_Fraction"),
          Annuals_Fraction = i_sw_input_treatments$PotentialNaturalVegetation_CompositionAnnuals_Fraction,
          fix_C4grasses = any(create_treatments == "PotentialNaturalVegetation_CompositionC4_Fraction"),
          C4_Fraction = i_sw_input_treatments$PotentialNaturalVegetation_CompositionC4_Fraction,
          fix_C3grasses = any(create_treatments == "PotentialNaturalVegetation_CompositionC3_Fraction"),
          C3_Fraction = i_sw_input_treatments$PotentialNaturalVegetation_CompositionC3_Fraction,
          fix_shrubs = any(create_treatments == "PotentialNaturalVegetation_CompositionShrubs_Fraction"),
          Shrubs_Fraction = i_sw_input_treatments$PotentialNaturalVegetation_CompositionShrubs_Fraction,
          fix_forbs = any(create_treatments == "PotentialNaturalVegetation_CompositionForb_Fraction"),
          Forbs_Fraction = i_sw_input_treatments$PotentialNaturalVegetation_CompositionForb_Fraction,
          fix_BareGround = any(create_treatments == "PotentialNaturalVegetation_CompositionBareGround_Fraction"),
          BareGround_Fraction = i_sw_input_treatments$PotentialNaturalVegetation_CompositionBareGround_Fraction))

        if (inherits(temp, "try-error")) {
          tasks$create <- 0L
          break

        } else {
          grass.fraction <- temp$Composition[1]
          rSOILWAT2::swProd_Composition(swRunScenariosData[[sc]]) <- temp$Composition
          grasses.c3c4ann.fractions[[sc]] <- temp$grasses.c3c4ann.fractions
        }
      }

      print_debug(opt_verbosity, tag_simpidfid, "creating", "potential vegetation")

      if (any(create_treatments == "PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996") &&
        i_sw_input_treatments$PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996 &&
        ((any(create_treatments == "AdjMonthlyBioMass_Temperature") &&
        i_sw_input_treatments$AdjMonthlyBioMass_Temperature) |
        (any(create_treatments == "AdjMonthlyBioMass_Precipitation") &&
        i_sw_input_treatments$AdjMonthlyBioMass_Precipitation))) {

        temp <- PotNatVeg_MonthlyBiomassPhenology_from_Climate(
          tr_VegBiom = tr_VegetationComposition,
          do_adjBiom_by_temp = any(create_treatments == "AdjMonthlyBioMass_Temperature") && i_sw_input_treatments$AdjMonthlyBioMass_Temperature,
          do_adjBiom_by_ppt = any(create_treatments == "AdjMonthlyBioMass_Precipitation") & i_sw_input_treatments$AdjMonthlyBioMass_Precipitation,
          fgrass_c3c4ann = grasses.c3c4ann.fractions[[sc]],
          growing_limit_C = opt_sim[["growseason_Tlimit_C"]],
          isNorth = isNorth, MAP_mm = MAP_mm, mean_monthly_temp_C = monthly.temp)

        rSOILWAT2::swProd_MonProd_grass(swRunScenariosData[[sc]])[, 1:3] <- temp$grass[, 1:3]
        rSOILWAT2::swProd_MonProd_shrub(swRunScenariosData[[sc]])[, 1:3] <- temp$shrub[, 1:3]
      }

      #adjust Root Profile - need composition fractions set above
      print_debug(opt_verbosity, tag_simpidfid, "creating", "AdjRootProfile")

      if (any(create_treatments == "AdjRootProfile") &&
          i_sw_input_treatments$AdjRootProfile &&
          any(create_treatments == "PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996") &&
          i_sw_input_treatments$PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996) {

        trco_type_C3 <- if (any(create_treatments == "RootProfile_C3") &&
          any(colnames(tr_input_TranspCoeff) == i_sw_input_treatments$RootProfile_C3)) {
            i_sw_input_treatments$RootProfile_C3
          } else {
            "SchenkJackson2003_PCdry_grasses"
          }

        trco_type_C4 <- if (any(create_treatments == "RootProfile_C4") &&
          any(colnames(tr_input_TranspCoeff) == i_sw_input_treatments$RootProfile_C4)) {
            i_sw_input_treatments$RootProfile_C4
          } else {
            "SchenkJackson2003_PCdry_grasses"
          }

        trco_type_annuals <- if (any(create_treatments == "RootProfile_Annuals") &&
          any(colnames(tr_input_TranspCoeff) == i_sw_input_treatments$RootProfile_Annuals)) {
            i_sw_input_treatments$RootProfile_Annuals
          } else {
            "Jacksonetal1996_crops"
          }

        trco_type_shrubs <- if (any(create_treatments == "RootProfile_Shrubs") &&
          any(colnames(tr_input_TranspCoeff) == i_sw_input_treatments$RootProfile_Shrubs)) {
            i_sw_input_treatments$RootProfile_Shrubs
          } else {
            "SchenkJackson2003_PCdry_shrubs"
          }

        tro_type_forb <- if (any(create_treatments == "RootProfile_Forbs") &&
          any(colnames(tr_input_TranspCoeff) == i_sw_input_treatments$RootProfile_Forbs)) {
            i_sw_input_treatments$RootProfile_Forbs
          } else {
            "SchenkJackson2003_PCdry_forbs"
          }

        tro_type_tree <- if (any(create_treatments == "LookupTranspCoeffFromTable_Tree") &&
          is.finite(i_sw_input_treatments$LookupTranspCoeffFromTable_Tree) &&
          any(colnames(tr_input_TranspCoeff) == i_sw_input_treatments$LookupTranspCoeffFromTable_Tree)) {
            i_sw_input_treatments$LookupTranspCoeffFromTable_Tree
          } else {
            "FILL"
          }

        if (rSOILWAT2::swProd_Composition(swRunScenariosData[[sc]])[1] > 0) {
          C3.trco <- TranspCoeffByVegType(
            tr_input_code = tr_input_TranspCoeff_Code, tr_input_coeff = tr_input_TranspCoeff,
            soillayer_no = soilLayers_N,
            trco_type = trco_type_C3,
            layers_depth = layers_depth,
            adjustType = "positive")

          C4.trco <- TranspCoeffByVegType(
            tr_input_code = tr_input_TranspCoeff_Code, tr_input_coeff = tr_input_TranspCoeff,
            soillayer_no = soilLayers_N,
            trco_type = trco_type_C4,
            layers_depth = layers_depth,
            adjustType = "positive")

          Annuals.trco <- TranspCoeffByVegType(
            tr_input_code = tr_input_TranspCoeff_Code, tr_input_coeff = tr_input_TranspCoeff,
            soillayer_no = soilLayers_N,
            trco_type = trco_type_annuals,
            layers_depth = layers_depth,
            adjustType = "positive")

          Grass.trco <- C3.trco * grasses.c3c4ann.fractions[[sc]][1] +
                        C4.trco * grasses.c3c4ann.fractions[[sc]][2] +
                        Annuals.trco * grasses.c3c4ann.fractions[[sc]][3]

        } else {
          Grass.trco <- TranspCoeffByVegType(
            tr_input_code = tr_input_TranspCoeff_Code, tr_input_coeff = tr_input_TranspCoeff,
            soillayer_no = soilLayers_N,
            trco_type = "FILL",
            layers_depth = layers_depth,
            adjustType = "positive")
        }

        if (anyNA(Grass.trco))
          Grass.trco <- rep(0, soilLayers_N)

        Shrub.trco <- TranspCoeffByVegType(
          tr_input_code = tr_input_TranspCoeff_Code, tr_input_coeff = tr_input_TranspCoeff,
          soillayer_no = soilLayers_N,
          trco_type = trco_type_shrubs,
          layers_depth = layers_depth,
          adjustType = "inverse")
        Tree.trco <- TranspCoeffByVegType(
          tr_input_code = tr_input_TranspCoeff_Code, tr_input_coeff = tr_input_TranspCoeff,
          soillayer_no = soilLayers_N,
          trco_type = tro_type_tree,
          layers_depth = layers_depth,
          adjustType = "inverse")
        Forb.trco <- TranspCoeffByVegType(
          tr_input_code = tr_input_TranspCoeff_Code, tr_input_coeff = tr_input_TranspCoeff,
          soillayer_no = soilLayers_N,
          trco_type = tro_type_forb,
          layers_depth = layers_depth,
          adjustType = "inverse")

        rSOILWAT2::swSoils_Layers(swRunScenariosData[[sc]])[, 5] <- Grass.trco
        rSOILWAT2::swSoils_Layers(swRunScenariosData[[sc]])[, 6] <- Shrub.trco
        rSOILWAT2::swSoils_Layers(swRunScenariosData[[sc]])[, 7] <- Tree.trco
        rSOILWAT2::swSoils_Layers(swRunScenariosData[[sc]])[, 8] <- Forb.trco

        TRCO_done <- TRUE
      }

      print_debug(opt_verbosity, tag_simpidfid, "creating", "vegetation scaling")

      Grass_Scaling_use <- c("Grass_TotalBiomass_ScalingFactor", "Grass_LiveBiomass_ScalingFactor", "Grass_Litter_ScalingFactor")
      Shrub_Scaling_use <- c("Shrub_TotalBiomass_ScalingFactor", "Shrub_LiveBiomass_ScalingFactor", "Shrub_Litter_ScalingFactor")
      Tree_Scaling_use <- c("Tree_TotalBiomass_ScalingFactor", "Tree_LiveBiomass_ScalingFactor", "Tree_Litter_ScalingFactor")
      Forb_Scaling_use <- c("Forb_TotalBiomass_ScalingFactor", "Forb_LiveBiomass_ScalingFactor", "Forb_Litter_ScalingFactor")
      if (any(create_treatments %in% c(Grass_Scaling_use, Shrub_Scaling_use, Tree_Scaling_use, Forb_Scaling_use))) {
        grass_LitterTotalLiveScalingFactors <- rep(1, 3)
        if (any(create_treatments == "Grass_Litter_ScalingFactor") && is.finite(i_sw_input_treatments$Grass_Litter_ScalingFactor))
          grass_LitterTotalLiveScalingFactors[1] <- i_sw_input_treatments$Grass_Litter_ScalingFactor
        if (any(create_treatments == "Grass_TotalBiomass_ScalingFactor") && is.finite(i_sw_input_treatments$Grass_TotalBiomass_ScalingFactor))
          grass_LitterTotalLiveScalingFactors[2] <- i_sw_input_treatments$Grass_TotalBiomass_ScalingFactor
        if (any(create_treatments == "Grass_LiveBiomass_ScalingFactor") && is.finite(i_sw_input_treatments$Grass_LiveBiomass_ScalingFactor))
          grass_LitterTotalLiveScalingFactors[3] <- i_sw_input_treatments$Grass_LiveBiomass_ScalingFactor

        shrub_LitterTotalLiveScalingFactors <- rep(1, 3)
        if (any(create_treatments == "Shrub_Litter_ScalingFactor") && is.finite(i_sw_input_treatments$Shrub_Litter_ScalingFactor))
          shrub_LitterTotalLiveScalingFactors[1] <- i_sw_input_treatments$Shrub_Litter_ScalingFactor
        if (any(create_treatments == "Shrub_TotalBiomass_ScalingFactor") && is.finite(i_sw_input_treatments$Shrub_TotalBiomass_ScalingFactor))
          shrub_LitterTotalLiveScalingFactors[2] <- i_sw_input_treatments$Shrub_TotalBiomass_ScalingFactor
        if (any(create_treatments == "Shrub_LiveBiomass_ScalingFactor") && is.finite(i_sw_input_treatments$Shrub_LiveBiomass_ScalingFactor))
          shrub_LitterTotalLiveScalingFactors[3] <- i_sw_input_treatments$Shrub_LiveBiomass_ScalingFactor

        tree_LitterTotalLiveScalingFactors <- rep(1, 3)
        if (any(create_treatments == "Tree_Litter_ScalingFactor") && is.finite(i_sw_input_treatments$Tree_Litter_ScalingFactor))
          tree_LitterTotalLiveScalingFactors[1] <- i_sw_input_treatments$Tree_Litter_ScalingFactor
        if (any(create_treatments == "Tree_TotalBiomass_ScalingFactor") && is.finite(i_sw_input_treatments$Tree_TotalBiomass_ScalingFactor))
          tree_LitterTotalLiveScalingFactors[2] <- i_sw_input_treatments$Tree_TotalBiomass_ScalingFactor
        if (any(create_treatments == "Tree_LiveBiomass_ScalingFactor") && is.finite(i_sw_input_treatments$Tree_LiveBiomass_ScalingFactor))
          tree_LitterTotalLiveScalingFactors[3] <- i_sw_input_treatments$Tree_LiveBiomass_ScalingFactor

        forb_LitterTotalLiveScalingFactors <- rep(1, 3)
        if (any(create_treatments == "Forb_Litter_ScalingFactor") && is.finite(i_sw_input_treatments$Forb_Litter_ScalingFactor))
          forb_LitterTotalLiveScalingFactors[1] <- i_sw_input_treatments$Forb_Litter_ScalingFactor
        if (any(create_treatments == "Forb_TotalBiomass_ScalingFactor") && is.finite(i_sw_input_treatments$Forb_TotalBiomass_ScalingFactor))
          forb_LitterTotalLiveScalingFactors[2] <- i_sw_input_treatments$Forb_TotalBiomass_ScalingFactor
        if (any(create_treatments == "Forb_LiveBiomass_ScalingFactor") && is.finite(i_sw_input_treatments$Forb_LiveBiomass_ScalingFactor))
          forb_LitterTotalLiveScalingFactors[3] <- i_sw_input_treatments$Forb_LiveBiomass_ScalingFactor

        ScalingSeason <- i_sw_input_treatments$Vegetation_Biomass_ScalingSeason_AllGrowingORNongrowing
        if (is.na(ScalingSeason) || !any(c("All", "Growing", "Nongrowing") == ScalingSeason)) #set to All for default
          ScalingSeason <- "All"

        if (any(create_treatments == "Vegetation_Biomass_ScalingSeason_AllGrowingORNongrowing") && !is.na(ScalingSeason) && !(any(create_treatments == "Vegetation_Biomass_ScalingSeason_AllGrowingORNongrowing") && ScalingSeason == "All")) {
          if (ScalingSeason == "Growing") { #Growing: apply 'Vegetation_Biomass_ScalingFactor' only to those months that have MAT > growseason_Tlimit_C
            temp <- SiteClimate_Scenario$meanMonthlyTempC > opt_sim[["growseason_Tlimit_C"]]
            templength <- sum(temp)
            if (templength > 1) {
              rSOILWAT2::swProd_MonProd_grass(swRunScenariosData[[sc]])[temp, 1:3] <- sweep(rSOILWAT2::swProd_MonProd_grass(swRunScenariosData[[sc]])[temp, 1:3], MARGIN = 2, FUN = "*", grass_LitterTotalLiveScalingFactors)
              rSOILWAT2::swProd_MonProd_shrub(swRunScenariosData[[sc]])[temp, 1:3] <- sweep(rSOILWAT2::swProd_MonProd_shrub(swRunScenariosData[[sc]])[temp, 1:3], MARGIN = 2, FUN = "*", shrub_LitterTotalLiveScalingFactors)
              rSOILWAT2::swProd_MonProd_tree(swRunScenariosData[[sc]])[temp, 1:3] <- sweep(rSOILWAT2::swProd_MonProd_tree(swRunScenariosData[[sc]])[temp, 1:3], MARGIN = 2, FUN = "*", tree_LitterTotalLiveScalingFactors)
              rSOILWAT2::swProd_MonProd_forb(swRunScenariosData[[sc]])[temp, 1:3] <- sweep(rSOILWAT2::swProd_MonProd_forb(swRunScenariosData[[sc]])[temp, 1:3], MARGIN = 2, FUN = "*", forb_LitterTotalLiveScalingFactors)
            } else if (templength == 1) {
              rSOILWAT2::swProd_MonProd_grass(swRunScenariosData[[sc]])[temp, 1:3] <- rSOILWAT2::swProd_MonProd_grass(swRunScenariosData[[sc]])[temp, 1:3]*grass_LitterTotalLiveScalingFactors
              rSOILWAT2::swProd_MonProd_shrub(swRunScenariosData[[sc]])[temp, 1:3] <- rSOILWAT2::swProd_MonProd_shrub(swRunScenariosData[[sc]])[temp, 1:3]*shrub_LitterTotalLiveScalingFactors
              rSOILWAT2::swProd_MonProd_tree(swRunScenariosData[[sc]])[temp, 1:3] <- rSOILWAT2::swProd_MonProd_tree(swRunScenariosData[[sc]])[temp, 1:3]*tree_LitterTotalLiveScalingFactors
              rSOILWAT2::swProd_MonProd_forb(swRunScenariosData[[sc]])[temp, 1:3] <-rSOILWAT2::swProd_MonProd_forb(swRunScenariosData[[sc]])[temp, 1:3]*forb_LitterTotalLiveScalingFactors
            } else {
              print(paste0(tag_simfid, ": to Cold to do Vegetation Scaling Season for Growing"))
            }
          } else if (ScalingSeason == "Nongrowing") {# Nongrowing: apply 'Vegetation_Biomass_ScalingFactor' only to those months that have MAT <= growseason_Tlimit_C
            temp <- SiteClimate_Scenario$meanMonthlyTempC <= opt_sim[["growseason_Tlimit_C"]]
            templength <- sum(temp)
            if (templength > 1) {
              rSOILWAT2::swProd_MonProd_grass(swRunScenariosData[[sc]])[temp, 1:3] <- sweep(rSOILWAT2::swProd_MonProd_grass(swRunScenariosData[[sc]])[temp, 1:3], MARGIN = 2, FUN = "*", grass_LitterTotalLiveScalingFactors)
              rSOILWAT2::swProd_MonProd_shrub(swRunScenariosData[[sc]])[temp, 1:3] <- sweep(rSOILWAT2::swProd_MonProd_shrub(swRunScenariosData[[sc]])[temp, 1:3], MARGIN = 2, FUN = "*", shrub_LitterTotalLiveScalingFactors)
              rSOILWAT2::swProd_MonProd_tree(swRunScenariosData[[sc]])[temp, 1:3] <- sweep(rSOILWAT2::swProd_MonProd_tree(swRunScenariosData[[sc]])[temp, 1:3], MARGIN = 2, FUN = "*", tree_LitterTotalLiveScalingFactors)
              rSOILWAT2::swProd_MonProd_forb(swRunScenariosData[[sc]])[temp, 1:3] <- sweep(rSOILWAT2::swProd_MonProd_forb(swRunScenariosData[[sc]])[temp, 1:3], MARGIN = 2, FUN = "*", forb_LitterTotalLiveScalingFactors)
            } else if (templength == 1) {
              rSOILWAT2::swProd_MonProd_grass(swRunScenariosData[[sc]])[temp, 1:3] <- rSOILWAT2::swProd_MonProd_grass(swRunScenariosData[[sc]])[temp, 1:3]*grass_LitterTotalLiveScalingFactors
              rSOILWAT2::swProd_MonProd_shrub(swRunScenariosData[[sc]])[temp, 1:3] <- rSOILWAT2::swProd_MonProd_shrub(swRunScenariosData[[sc]])[temp, 1:3]*shrub_LitterTotalLiveScalingFactors
              rSOILWAT2::swProd_MonProd_tree(swRunScenariosData[[sc]])[temp, 1:3] <- rSOILWAT2::swProd_MonProd_tree(swRunScenariosData[[sc]])[temp, 1:3]*tree_LitterTotalLiveScalingFactors
              rSOILWAT2::swProd_MonProd_forb(swRunScenariosData[[sc]])[temp, 1:3] <- rSOILWAT2::swProd_MonProd_forb(swRunScenariosData[[sc]])[temp, 1:3]*forb_LitterTotalLiveScalingFactors
            } else {
              print(paste0(tag_simfid, ": to Hot to do Vegetation Scaling Season for NonGrowing"))
            }
          }
        } else {
          rSOILWAT2::swProd_MonProd_grass(swRunScenariosData[[sc]])[, 1:3] <- sweep(rSOILWAT2::swProd_MonProd_grass(swRunScenariosData[[sc]])[, 1:3], MARGIN = 2, FUN = "*", grass_LitterTotalLiveScalingFactors)
          rSOILWAT2::swProd_MonProd_shrub(swRunScenariosData[[sc]])[, 1:3] <- sweep(rSOILWAT2::swProd_MonProd_shrub(swRunScenariosData[[sc]])[, 1:3], MARGIN = 2, FUN = "*", shrub_LitterTotalLiveScalingFactors)
          rSOILWAT2::swProd_MonProd_tree(swRunScenariosData[[sc]])[, 1:3] <- sweep(rSOILWAT2::swProd_MonProd_tree(swRunScenariosData[[sc]])[, 1:3], MARGIN = 2, FUN = "*", tree_LitterTotalLiveScalingFactors)
          rSOILWAT2::swProd_MonProd_forb(swRunScenariosData[[sc]])[, 1:3] <- sweep(rSOILWAT2::swProd_MonProd_forb(swRunScenariosData[[sc]])[, 1:3], MARGIN = 2, FUN = "*", forb_LitterTotalLiveScalingFactors)
        }
        rSOILWAT2::swProd_MonProd_grass(swRunScenariosData[[sc]])[, 3] <- finite01(rSOILWAT2::swProd_MonProd_grass(swRunScenariosData[[sc]])[, 3])  #Check that live biomass fraction <= 1 & >= 0
        rSOILWAT2::swProd_MonProd_shrub(swRunScenariosData[[sc]])[, 3] <- finite01(rSOILWAT2::swProd_MonProd_shrub(swRunScenariosData[[sc]])[, 3])  #Check that live biomass fraction <= 1 & >= 0
        rSOILWAT2::swProd_MonProd_tree(swRunScenariosData[[sc]])[, 3] <- finite01(rSOILWAT2::swProd_MonProd_tree(swRunScenariosData[[sc]])[, 3])  #Check that live biomass fraction <= 1 & >= 0
        rSOILWAT2::swProd_MonProd_forb(swRunScenariosData[[sc]])[, 3] <- finite01(rSOILWAT2::swProd_MonProd_forb(swRunScenariosData[[sc]])[, 3])  #Check that live biomass fraction <= 1 & >= 0
      }

      if (any(create_treatments == "Vegetation_Height_ScalingFactor")) {
        #scale constant height
        rSOILWAT2::swProd_CanopyHeight(swRunScenariosData[[sc]])[5, ] <- pmax(0, rSOILWAT2::swProd_CanopyHeight(swRunScenariosData[[sc]])[5, ] * i_sw_input_treatments$Vegetation_Height_ScalingFactor)
        #scale tanfunc parameters: scale yinflec and range, leave xinflec and slope as is
        rSOILWAT2::swProd_CanopyHeight(swRunScenariosData[[sc]])[2:3, ] <- pmax(0, rSOILWAT2::swProd_CanopyHeight(swRunScenariosData[[sc]])[2:3, ] * i_sw_input_treatments$Vegetation_Height_ScalingFactor)
      }

      #if southern hemisphere adjust if set, but not when already adjusted by, e.g., growing season
      print_debug(opt_verbosity, tag_simpidfid, "creating", "hemisphere adjustment")

      if (opt_sim[["adjust_veg_input_NS"]] && i_SWRunInformation$Y_WGS84 < 0 && !any(create_treatments == "AdjMonthlyBioMass_Temperature")) {
        rSOILWAT2::swProd_MonProd_grass(swRunScenariosData[[sc]])[, 3] <- rbind(rSOILWAT2::swProd_MonProd_grass(swRunScenariosData[[sc]])[7:12, ], rSOILWAT2::swProd_MonProd_grass(swRunScenariosData[[sc]])[1:6, ])
        rSOILWAT2::swProd_MonProd_shrub(swRunScenariosData[[sc]])[, 3] <- rbind(rSOILWAT2::swProd_MonProd_shrub(swRunScenariosData[[sc]])[7:12, ], rSOILWAT2::swProd_MonProd_shrub(swRunScenariosData[[sc]])[1:6, ])
        rSOILWAT2::swProd_MonProd_tree(swRunScenariosData[[sc]])[, 3] <- rbind(rSOILWAT2::swProd_MonProd_tree(swRunScenariosData[[sc]])[7:12, ], rSOILWAT2::swProd_MonProd_tree(swRunScenariosData[[sc]])[1:6, ])
        rSOILWAT2::swProd_MonProd_forb(swRunScenariosData[[sc]])[, 3] <- rbind(rSOILWAT2::swProd_MonProd_forb(swRunScenariosData[[sc]])[7:12, ], rSOILWAT2::swProd_MonProd_forb(swRunScenariosData[[sc]])[1:6, ])
      }

      # check that vegetation has no NAs
      is_bad_veg <- any(anyNA(rSOILWAT2::swProd_MonProd_grass(swRunScenariosData[[sc]])),
        anyNA(rSOILWAT2::swProd_MonProd_shrub(swRunScenariosData[[sc]])),
        anyNA(rSOILWAT2::swProd_MonProd_tree(swRunScenariosData[[sc]])),
        anyNA(rSOILWAT2::swProd_MonProd_forb(swRunScenariosData[[sc]])))
      if (is_bad_veg) {
        print(paste0(tag_simpidfid, ": ERROR: vegetation values contain NA."))
        tasks$create <- 0L
        break
      }

      #--control transpiration regions for adjusted soil depth and rooting depth
      print_debug(opt_verbosity, tag_simpidfid, "creating", "control transpiration regions")

      tri.file <- matrix(NA, nrow = 4, ncol = 2, dimnames = list(NULL, c("Used_TF", "DeepestLayer")))
      for (tri in 1:4) {
        if (tri <= nrow(rSOILWAT2::swSite_TranspirationRegions(swRunScenariosData[[sc]]))) {
          tri.file[tri, 2] <- rSOILWAT2::swSite_TranspirationRegions(swRunScenariosData[[sc]])[tri, 2]
          tri.file[tri, 1] <- 1
        } else {
          tri.file[tri, 2] <- NA#rSOILWAT2::swSite_TranspirationRegions(swRunScenariosData[[sc]])[tri-1, 2]+1
          tri.file[tri, 1] <- 0
        }
      }

      #get soil depth
      max.tri.soil <- length(layers_depth)

      #get rooting depth
      if (nrow(rSOILWAT2::swSoils_Layers(swRunScenariosData[[sc]])) > 1) {
        max.tri.root <- min(apply(rSOILWAT2::swSoils_Layers(swRunScenariosData[[sc]])[, c(6, 7, 8), drop = FALSE], MARGIN = 2, FUN = function(x) sum(x > 0)))
      } else {
        max.tri.root <- 1
      }
      #adjust maximum transpiration region for minimum soil depth and rooting depth
      if (max(tri.file[tri.file[, 1] > 0, 2], na.rm = TRUE) > (max.tri <- min(max.tri.soil, max.tri.root))) {
        for (tri in 4:1) if (tri.file[tri, 1] > 0) {
            if (tri.file[tri, 2] > max.tri)
              tri.file[tri, 2] <- rSOILWAT2::swSite_TranspirationRegions(swRunScenariosData[[sc]])[tri, 2] <- max.tri
            if (tri > 1 && tri.file[tri, 2] <= tri.file[tri-1, 2])
              rSOILWAT2::swSite_TranspirationRegions(swRunScenariosData[[sc]]) <- matrix(rSOILWAT2::swSite_TranspirationRegions(swRunScenariosData[[sc]])[-tri, ], ncol = 2)
          }
      }
      #check transpiration regions once more and set TRRG_done
      temp <- rSOILWAT2::swSite_TranspirationRegions(swRunScenariosData[[sc]])
      if (nrow(temp) > 0 && temp[1, 2] >= 1 ||
        max(temp[, 2]) <= max.tri.root) TRRG_done <- TRUE

      # Check evaporation- and transpiration coefficients
      soil_swdat <- rSOILWAT2::swSoils_Layers(swRunScenariosData[[sc]])
      dimnames(soil_swdat)[[2]] <- soil_cols
      EVCO_done <- check_soilco(soil_swdat[, "EvapBareSoil_frac"])
      temp_trco <- soil_swdat[, c("transpGrass_frac", "transpShrub_frac",
        "transpTree_frac", "transpForb_frac"), drop = FALSE]
      TRCO_done <- all(apply(temp_trco, 2, check_soilco))


      print_debug(opt_verbosity, tag_simpidfid, "tasks =",
        paste(paste(tasks, collapse = ", "), ", evco = ", EVCO_done, ", trco = ",
        TRCO_done, ", trrg = ", TRRG_done))
    }#end do scenario creations

    if (!EVCO_done) {
      print(paste0(tag_simfid, ": evaporation coefficients not set for this run."))
    } else if (!TRCO_done) {
      print(paste0(tag_simfid, ": transpiration coefficients not set for this run."))
    } else if (!TRRG_done) {
      print(paste0(tag_simfid, ": transpiration regions not set for this run."))
    }

    if (tasks$create <= 0L || !EVCO_done || !TRCO_done || !TRRG_done) {
      tasks$create <- 0L
      tasks$execute[] <- tasks$aggregate[] <- -1L
    } else {
      tasks$create <- 2L
    }

    if (opt_out_run[["saveRsoilwatInput"]])
      save(swRunScenariosData, i_sw_weatherList, grasses.c3c4ann.fractions,
      ClimatePerturbationsVals, isim_time, simTime2, file = f_sw_input)
  }#end if do create runs

  if (opt_out_run[["makeInputForExperimentalDesign"]] && sim_size[["expN"]] > 0 &&
    length(create_experimentals) > 0) {

    #This file will be used to remake the input files for experimentals
    infiletext <- c(paste(i_label, paste(i_SWRunInformation[-1],
      collapse = opt_out_fix[["ExpInput_Seperator"]]),
      sep = opt_out_fix[["ExpInput_Seperator"]]))
    infiletext <- c(infiletext, paste(i_label, paste(i_sw_input_soillayers[-1],
      collapse = opt_out_fix[["ExpInput_Seperator"]]),
      sep = opt_out_fix[["ExpInput_Seperator"]]))
    infiletext <- c(infiletext, paste(i_label, paste(i_sw_input_treatments[-1],
      collapse = opt_out_fix[["ExpInput_Seperator"]]),
      sep = opt_out_fix[["ExpInput_Seperator"]]))
    infiletext <- c(infiletext, paste(i_label, paste(i_sw_input_cloud[-1],
      collapse = opt_out_fix[["ExpInput_Seperator"]]),
      sep = opt_out_fix[["ExpInput_Seperator"]]))
    infiletext <- c(infiletext, paste(i_label, paste(i_sw_input_prod[-1],
      collapse = opt_out_fix[["ExpInput_Seperator"]]),
      sep = opt_out_fix[["ExpInput_Seperator"]]))
    infiletext <- c(infiletext, paste(i_label, paste(i_sw_input_site[-1],
      collapse = opt_out_fix[["ExpInput_Seperator"]]),
      sep = opt_out_fix[["ExpInput_Seperator"]]))
    infiletext <- c(infiletext, paste(i_label, paste(i_sw_input_soils[-1],
      collapse = opt_out_fix[["ExpInput_Seperator"]]),
      sep = opt_out_fix[["ExpInput_Seperator"]]))
    infiletext <- c(infiletext, paste(i_label, paste(i_sw_input_weather[-1],
      collapse = opt_out_fix[["ExpInput_Seperator"]]),
      sep = opt_out_fix[["ExpInput_Seperator"]]))
    infiletext <- c(infiletext, paste(i_label, paste(i_sw_input_climscen[-1],
      collapse = opt_out_fix[["ExpInput_Seperator"]]),
      sep = opt_out_fix[["ExpInput_Seperator"]]))
    infiletext <- c(infiletext, paste(i_label, paste(i_sw_input_climscen_values[-1],
      collapse = opt_out_fix[["ExpInput_Seperator"]]),
      sep = opt_out_fix[["ExpInput_Seperator"]]))

    infilename <- file.path(project_paths[["dir_out_expDesign"]],
      paste0(flag.icounter, "_", "Experimental_InputData_All.csv"))
    infile <- file(infilename, "w+b")
    writeLines(text = infiletext, con = infile, sep = "\n")
    close(infile)
  }



#------------------------EXECUTE & AGGREGATE SOILWAT2
  if (!exists("swRunScenariosData") || !exists("i_sw_weatherList")) {
    tasks$execute[] <- -1L

  } else {
    #get soil texture data for each layer
    stemp <- rSOILWAT2::swSoils_Layers(swRunScenariosData[[1]])
    layers_depth <- stemp[, 1]
    layers_width <- getLayersWidth(layers_depth)
    soilDepth_cm <- max(stemp[, 1])
    soilLayers_N <- length(stemp[, 1])
    ld <- setLayerSequence(soilLayers_N)
    DeepestTopLayer <- setDeepestTopLayer(layers_depth, opt_agg[["aon_toplayer_cm"]])
    topL <- setTopLayer(soilLayers_N, DeepestTopLayer)
    bottomL <- setBottomLayer(soilLayers_N, DeepestTopLayer)

    gravel <- stemp[, 3]
    sand <- stemp[, 9]
    clay <- stemp[, 10]

    #TODO: adjust this once TOC is incorporated into rSOILWAT2
    soil_TOC <- rep(NA, soilLayers_N)
    if (exists("i_sw_input_soils") && exists("sw_input_soils_use")) {
      temp <- grep("TOC_GperKG_L", names(sw_input_soils_use))
      if (length(temp) > 0)
        soil_TOC <- as.numeric(i_sw_input_soils[, temp[ld]])
    }

    #get soil aggregation layer for daily aggregations
    if (opt_agg[["doy_slyrs"]][["do"]]) {
      aggLs <- setAggSoilLayerForAggDailyResponses(layers_depth, opt_agg[["doy_slyrs"]])
    } else {
      aggLs <- as.list(ld)
    }
    aggLs_no <- length(aggLs)

    texture <- list(
      sand.top = stats::weighted.mean(sand[topL], layers_width[topL]),
      sand.bottom = stats::weighted.mean(sand[bottomL], layers_width[bottomL]),
      clay.top = stats::weighted.mean(clay[topL], layers_width[topL]),
      clay.bottom = stats::weighted.mean(clay[bottomL], layers_width[bottomL]),
      gravel.top = stats::weighted.mean(gravel[topL], layers_width[topL]),
      gravel.bottom = stats::weighted.mean(gravel[bottomL], layers_width[bottomL]))

    if (prj_todos[["adaily"]][["N"]] > 0) {
      temp <- seq_along(aggLs)

      textureDAgg <- list(
        gravel = sapply(temp,
          function(x) stats::weighted.mean(gravel[aggLs[[x]]], layers_width[aggLs[[x]]])),
        sand = sapply(temp,
          function(x) stats::weighted.mean(sand[aggLs[[x]]], layers_width[aggLs[[x]]])),
        clay = sapply(temp,
          function(x) stats::weighted.mean(clay[aggLs[[x]]], layers_width[aggLs[[x]]])))
    }

    #prepare SQL result container
    SQL <- character(0)
  }

  if (is.na(i_sw_input_treatments$Exclude_ClimateAmbient))
    i_sw_input_treatments$Exclude_ClimateAmbient <- FALSE

  sc1 <- if (any(create_treatments == "Exclude_ClimateAmbient") &&
      i_sw_input_treatments$Exclude_ClimateAmbient && i_sim != 1L) 2L else 1L

#  #' Width of layer used to simulate soil temperature
#  #'
#  #' @param DeltaX An integer vector of length two.
#  #'  \code{DeltaX[1]}: determined deltaX_Param value; will be used for all >= sc
#  #'  \code{DeltaX[2]}: -1 == failed; 0 == no run yet;
#  #'    1 == deltaX_Param successfully approved; 2 == deltaX_Param successfully modified
  DeltaX <- c(NA, 0L)

  for (sc in sc1:sim_scens[["N"]]) {

    P_id <- it_Pid(i_sim, sim_size[["runsN_master"]], sc, sim_scens[["N"]])
    tag_simpidfid <- paste0("[run", i_sim, "/PID", P_id, "/sc", sc, "/work", fid, "]")

    print_debug(opt_verbosity, tag_simpidfid, "executing", "SOILWAT2")

    if (file.exists(f_sw_output[sc]) && ((tasks$execute[sc] == 1L && opt_behave[["resume"]]) ||
      (tasks$execute[sc] == -1L && any(tasks$aggregate == 1L)))) {

      load(f_sw_output[sc])  # load object: runDataSC
      if (exists("runDataSC"))
        tasks$execute[sc] <- 2L
    }

    if (tasks$execute[sc] == 1L) {
      runDataSC <- NULL
      is_SOILTEMP_INSTABLE <- rep(NA, sim_scens[["N"]])

      scw <- if (opt_sim[["use_dbW_future"]]) sc else 1L
      mDepth <- rSOILWAT2::swSite_SoilTemperatureConsts(swRunScenariosData[[sc]])["MaxDepth"]

      if (DeltaX[2] > 0) {
        print_debug(opt_verbosity, tag_simpidfid, "using pre-determined DeltaX", DeltaX[1])

        if (DeltaX[2] == 2L)
          rSOILWAT2::swSite_SoilTemperatureConsts(swRunScenariosData[[sc]])["deltaX_Param"] <- DeltaX[1]
      }

      runDataSC <- try(rSOILWAT2::sw_exec(inputData = swRunScenariosData[[sc]],
                     weatherList = i_sw_weatherList[[scw]],
                echo = FALSE, quiet = TRUE),
              silent = TRUE)

      # Testing for error in soil temperature module
      is_SOILTEMP_INSTABLE[sc] <- rSOILWAT2::has_soilTemp_failed()

      if (is_SOILTEMP_INSTABLE[sc]) {
        ## Incrementing deltaX and recalling SOILWAT2 until the temperature is at least normal or the loop executes ten times
        i_soil_rep <- 0
        DeltaX[1] <- rSOILWAT2::swSite_SoilTemperatureConsts(swRunScenariosData[[sc]])["deltaX_Param"]

        while (!inherits(runDataSC, "try-error") && is_SOILTEMP_INSTABLE[sc] &&
          DeltaX[1] <= mDepth && i_soil_rep < 10) {

          ## Make sure that the increment for the soil layers is a multiple of the MaxDepth,
          #   modulus of 0 means no remainder and thus a multiple of the MaxDepth
          repeat {
            DeltaX[1] <- DeltaX[1] + opt_sim[["increment_soiltemperature_deltaX_cm"]]
            if (mDepth %% DeltaX[1] == 0) break
          }

          ## recall Soilwat with the new deltaX parameter and continue to do so with increasing deltax until resolved or executed 10 times
          rSOILWAT2::swSite_SoilTemperatureConsts(swRunScenariosData[[sc]])["deltaX_Param"] <- min(DeltaX[1], mDepth)
          print_debug(opt_verbosity, tag_simpidfid, "SOILWAT2 called again with deltaX (cm) =",
            rSOILWAT2::swSite_SoilTemperatureConsts(swRunScenariosData[[sc]])["deltaX_Param"])

          runDataSC <- try(rSOILWAT2::sw_exec(inputData = swRunScenariosData[[sc]],
                     weatherList = i_sw_weatherList[[scw]],
                echo = FALSE, quiet = TRUE),
              silent = TRUE)

          ## Test to check and see if SOILTEMP is stable so that the loop can break - this will be based on parts being > 1.0
          is_SOILTEMP_INSTABLE[sc] <- rSOILWAT2::has_soilTemp_failed()
          i_soil_rep <- i_soil_rep + 1
        }

        DeltaX[2] <- if (!inherits(runDataSC, "try-error") && !is_SOILTEMP_INSTABLE[sc]) 2L else -1L

        #TODO: change deltaX_Param for all [> sc] as well
        if (opt_out_run[["saveRsoilwatInput"]])
          save(swRunScenariosData, i_sw_weatherList, grasses.c3c4ann.fractions,
            ClimatePerturbationsVals, file = f_sw_input)

      } else {
        DeltaX <- c(rSOILWAT2::swSite_SoilTemperatureConsts(swRunScenariosData[[sc]])["deltaX_Param"], 1L)
      }

      if (inherits(runDataSC, "try-error") || DeltaX[2] < 0) {
        tasks$execute[sc] <- 0L
      }

      if (opt_out_run[["saveRsoilwatOutput"]]) {
        save(runDataSC, is_SOILTEMP_INSTABLE, file = f_sw_output[sc])
      }
    }

    if (tasks$execute[sc] > 0L && exists("runDataSC"))
      tasks$execute[sc] <- 2L


#------------------------AGGREGATE SOILWAT2 OUTPUT
    if (tasks$execute[sc] != 2L && !exists("swRunScenariosData") || !exists("runDataSC") ||
      !exists("grasses.c3c4ann.fractions") || !exists("ClimatePerturbationsVals") ||
      !exists("is_SOILTEMP_INSTABLE") || !inherits(runDataSC, "swOutput")) {

      tasks$aggregate[sc] <- -1L
    }


    if (tasks$aggregate[sc] == 1L) {
      print_debug(opt_verbosity, tag_simpidfid, "section", "overall aggregation")

      #HEADER GENERATION REMOVED#
      #only exclude if
      #   1.) Exclude_ClimateAmbient is true in treatments
      #   2.) That Run is set to Exclude_ClimateAmbient
      #   3.) Our current Scenario is Current
      if (any(create_treatments == "Exclude_ClimateAmbient") &&
        i_sw_input_treatments$Exclude_ClimateAmbient && sc == 1 && i_sim != 1) {

        Exclude_ClimateAmbient <- TRUE

        #ncol_dbOut_overall comes from database creation
        temp <- paste(c(P_id, if (sim_size[["ncol_dbOut_overall"]] > 0)
          paste0(rep("NULL", sim_size[["ncol_dbOut_overall"]]), collapse = ",")),
          collapse = ", ")

        SQL <- paste0("INSERT INTO \"aggregation_overall_mean\" VALUES (", temp, ");")
        try(DBI::dbExecute(dbTempFile, SQL), silent = !opt_verbosity[["verbose"]])

        SQL <- paste0("INSERT INTO \"aggregation_overall_sd\" VALUES (", temp, ");")
        try(DBI::dbExecute(dbTempFile, SQL), silent = !opt_verbosity[["verbose"]])

      } else {
        Exclude_ClimateAmbient <- FALSE
      }

      #overall aggregation. If Exclude_ClimateAmbient == TRUE then skip
      if (!opt_behave[["resume"]] || (opt_behave[["resume"]] &&
        !isdone.overallAggs[sc]) && !Exclude_ClimateAmbient) {

        # delete data so that they are read anew for each scenario; each variable is
        # checked that datafile is read in only once per scenario
        to_del <- c("temp.yr", "temp.mo", "temp.dy",
              "prcp.yr", "prcp.mo", "prcp.dy",
              "PET.yr", "PET.mo", "PET.dy",
              "vpd.yr", "vpd.mo", "vpd.dy",
              "AET.yr", "AET.mo", "AET.dy",
              "soiltemp.yr", "soiltemp.mo", "soiltemp.dy",
              "soiltemp.yr.all", "soiltemp.mo.all", "soiltemp.dy.all",
              "swcbulk.yr", "swcbulk.mo", "swcbulk.dy",
              "swabulk.yr", "swabulk.mo", "swabulk.dy",
              "swamatric.yr", "swamatric.mo", "swamatric.dy",
              "vwcbulk.yr", "vwcbulk.mo", "vwcbulk.dy", "vwcbulk.dy.all",
              "vwcmatric.yr", "vwcmatric.mo", "vwcmatric.dy", "vwcmatric.dy.all",
              "swpmatric.yr", "swpmatric.mo", "swpmatric.dy", "swpmatric.dy.all",
              "transp.yr", "transp.mo", "transp.dy", "transp.dy.all",
              "Esoil.yr", "Esoil.mo", "Esoil.dy", "Esoil.dy.all",
              "Esurface.yr", "Esurface.mo", "Esurface.dy",
              "hydred.yr", "hydred.mo", "hydred.dy",
              "inf.yr", "inf.mo", "inf.dy",
              "runonoff.yr", "runonoff.mo", "runonoff.dy",
              "intercept.yr", "intercept.mo", "intercept.dy",
              "deepDrain.yr", "deepDrain.mo", "deepDrain.dy",
              "veg.yr", "veg.mo", "veg.dy",
              "co2effects.yr", "co2effects.mo", "co2effects.dy")
        to_del <- to_del[to_del %in% ls()]
        if (length(to_del) > 0) try(rm(list = to_del), silent = TRUE)

        #result vector column index indicating variable within set of n_variables per scenario
        resMeans <- resSDs <- rep(NA, length = sim_size[["ncol_dbOut_overall"]])
        nv <- 1


        #---Aggregation: SOILWAT2 inputs
      #0
        if (isTRUE(prj_todos[["aon"]][["input_SoilProfile"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "input_SoilProfile")

          resMeans[nv:(nv+7)] <- c(soilDepth_cm, soilLayers_N, unlist(texture))
          nv <- nv+8
          resMeans[nv] <- swRunScenariosData[[1]]@site@SoilTemperatureConstants[9]
          nv <- nv+1

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "input_SoilProfile")
        }

      #1
        if (isTRUE(prj_todos[["aon"]][["input_FractionVegetationComposition"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "input_FractionVegetationComposition")

          resMeans[nv:(nv+7)] <- c(rSOILWAT2::swProd_Composition(swRunScenariosData[[sc]]),
            grasses.c3c4ann.fractions[[sc]])
          nv <- nv+8

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "input_FractionVegetationComposition")
        }

      #2
        if (isTRUE(prj_todos[["aon"]][["input_VegetationBiomassMonthly"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "input_VegetationBiomassMonthly")

          temp <- lapply(c("swProd_MonProd_grass", "swProd_MonProd_shrub",
            "swProd_MonProd_tree", "swProd_MonProd_forb"),
            function(x) utils::getFromNamespace(x, "rSOILWAT2")(swRunScenariosData[[sc]]))

          for (k in seq_along(temp)) {
            resMeans[nv:(nv + 11)] <- temp[[k]][, 1]
            nv <- nv + 12
            resMeans[nv:(nv + 11)] <- temp[[k]][, 2]
            nv <- nv + 12
            resMeans[nv:(nv + 11)] <- temp[[k]][, 2] * temp[[k]][, 3]
            nv <- nv + 12
          }

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "input_VegetationBiomassMonthly")
        }

        if (isTRUE(prj_todos[["aon"]][["input_VegetationBiomassTrends"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "input_VegetationBiomassMonthly")

          if (!exists("veg.yr")) veg.yr <- get_Vegetation_yr(runDataSC, isim_time)

          nv_add <- ncol(veg.yr[["val"]])
          nv_new <- nv + nv_add
          resMeans[nv:(nv_new - 1)] <- .colMeans(veg.yr[["val"]], isim_time$no.useyr, nv_add)
          resSDs[nv:(nv_new - 1)] <- apply(veg.yr[["val"]], 2, stats::sd)
          nv <- nv_new

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "input_VegetationBiomassTrends")
        }

      #3
        if (isTRUE(prj_todos[["aon"]][["input_VegetationPeak"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "input_VegetationPeak")

          fracs <- rSOILWAT2::swProd_Composition(swRunScenariosData[[sc]])[1:4] #get the fractional Composition of grasses, shrubs, and trees
          tempdat <- matrix(data = NA, nrow = 12, ncol = 4)#matrix to hold biomass * percLive for grass, shrubs, trees
          colnames(tempdat) <- c("grass", "shrub", "tree", "forb")
          tempdat[, 1] <- rSOILWAT2::swProd_MonProd_grass(swRunScenariosData[[sc]])[, 2]*rSOILWAT2::swProd_MonProd_grass(swRunScenariosData[[sc]])[, 3]
          tempdat[, 2] <- rSOILWAT2::swProd_MonProd_shrub(swRunScenariosData[[sc]])[, 2]*rSOILWAT2::swProd_MonProd_shrub(swRunScenariosData[[sc]])[, 3]
          tempdat[, 3] <- rSOILWAT2::swProd_MonProd_tree(swRunScenariosData[[sc]])[, 2]*rSOILWAT2::swProd_MonProd_tree(swRunScenariosData[[sc]])[, 3]
          tempdat[, 4] <- rSOILWAT2::swProd_MonProd_forb(swRunScenariosData[[sc]])[, 2]*rSOILWAT2::swProd_MonProd_forb(swRunScenariosData[[sc]])[, 3]

          sumWeightedLiveBiomassByMonth <- apply(sweep(tempdat, MARGIN = 2, fracs, FUN = "*"), MARGIN = 1, sum) #sweep out fractionals, and sum over rows
          maxMonth <- which(sumWeightedLiveBiomassByMonth == max(sumWeightedLiveBiomassByMonth)) #returns index, which is the month, of max bio
          meanPeakMonth <- circ_mean(maxMonth, 12)
          duration <- circ_range(maxMonth, 12)+1

          resMeans[nv:(nv+1)] <- c(meanPeakMonth, duration) #just in case we get more then one month
          nv <- nv+2

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "input_VegetationPeak")
        }

      #4
        if (isTRUE(prj_todos[["aon"]][["input_Phenology"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "input_Phenology")

          if (!exists("temp.mo")) temp.mo <- get_Temp_mo(runDataSC, isim_time)
          monthly.temp <- tapply(temp.mo$mean, simTime2$month_ForEachUsedMonth, mean) #get mean monthly temp
          Months_Above_Threshold <- which(monthly.temp > opt_sim[["growseason_Tlimit_C"]]) #get months above threshold
          if (i_SWRunInformation$Y_WGS84 < 0) { #check for Southern Hemi
            monthly.temp <- c(monthly.temp[7:12], monthly.temp[1:6]) #rearrange temp
            Months_Above_Threshold <- c(7:12, 1:6)[Months_Above_Threshold] #get months above threshold, then map back to real months.
          }
          Input_PhenologyStart_Month <- Months_Above_Threshold[1] #get the first month
          Input_PhenologyEnd_Month <- Months_Above_Threshold[length(Months_Above_Threshold)] #get the last month

          resMeans[nv:(nv+1)] <- c(Input_PhenologyStart_Month, Input_PhenologyEnd_Month)
          nv <- nv+2

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "input_Phenology")
        }

      #5
        if (isTRUE(prj_todos[["aon"]][["input_TranspirationCoeff"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "input_TranspirationCoeff")

          Tcoeff <- rSOILWAT2::swSoils_Layers(swRunScenariosData[[1]])[, 5:8, drop = FALSE]
          if (is.null(dim(Tcoeff))) Tcoeff <- matrix(Tcoeff, nrow = 1)

          TaggLs <- sapply(aggLs, FUN = function(l) apply(Tcoeff[l, , drop = FALSE], 2, sum))
          if (length(bottomL) > 0 && !identical(bottomL, 0)) {
            Ttb <- sapply(list(topL, bottomL), FUN = function(l) apply(Tcoeff[l, , drop = FALSE], 2, sum))
          } else {
            Ttb <- sapply(list(topL), FUN = function(l) apply(Tcoeff[l, , drop = FALSE], 2, sum))
          }

          iinv <- inv <- nv
          for (iv in 1:4) {
            nv <- nv+SFSW2_glovars[["slyrs_maxN"]] #We don't know the max number of soil layers (aggLs_no) among all simulations, i.e., set to the maximum
            resMeans[(inv+(iv-1)*SFSW2_glovars[["slyrs_maxN"]]):(nv-1)] <- c(TaggLs[iv, ], rep(NA, times = SFSW2_glovars[["slyrs_maxN"]]-aggLs_no))
          }
          inv <- nv
          for (iv in 1:4) {
            nv <- nv+2
            resMeans[(inv+(iv-1)*2):(nv-1)] <- Ttb[iv, ]
          }

          rm(Tcoeff, TaggLs, Ttb)

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "input_TranspirationCoeff")
        }

      #6
        if (isTRUE(prj_todos[["aon"]][["input_ClimatePerturbations"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "input_ClimatePerturbations")

          resMeans[nv:(nv+35)] <- as.vector(as.numeric(ClimatePerturbationsVals[sc, ]))
          nv <- nv+36

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "input_ClimatePerturbations")
        }

        if (isTRUE(prj_todos[["aon"]][["input_CO2Effects"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "input_CO2Effects")

          if (!exists("co2effects.yr")) co2effects.yr <- get_CO2effects_yr(runDataSC, isim_time)

          nv_add <- ncol(co2effects.yr[["val"]])
          nv_new <- nv + nv_add
          resMeans[nv:(nv_new - 1)] <- .colMeans(co2effects.yr[["val"]], isim_time$no.useyr, nv_add)
          resSDs[nv:(nv_new - 1)] <- apply(co2effects.yr[["val"]], 2, stats::sd)
          nv <- nv_new

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "input_CO2Effects")
        }

        #---Aggregation: Climate and weather
      #7
        if (isTRUE(prj_todos[["aon"]][["yearlyTemp"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "yearlyTemp")
          if (!exists("temp.yr"))  temp.yr <- get_Temp_yr(runDataSC, isim_time)

          resMeans[nv] <- mean(temp.yr$mean)
          resSDs[nv] <- stats::sd(temp.yr$mean)
          nv <- nv+1

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "yearlyTemp")
        }

      #8
        if (isTRUE(prj_todos[["aon"]][["yearlyPPT"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "yearlyPPT")
          if (!exists("prcp.yr")) prcp.yr <- get_PPT_yr(runDataSC, isim_time)
          if (!exists("prcp.dy")) prcp.dy <- get_PPT_dy(runDataSC, isim_time)

          resMeans[nv] <- mean(prcp.yr$ppt)
          resSDs[nv] <- stats::sd(prcp.yr$ppt)
          resMeans[nv+1] <- mean(snowofppt <- prcp.yr$snowfall/prcp.yr$ppt, na.rm = TRUE)
          resSDs[nv+1] <- stats::sd(snowofppt, na.rm = TRUE)
          nv <- nv+2

          rm(snowofppt)

          if (isTRUE(opt_agg[["use_doy_range"]])) {

            dailyrange <- if(length(idx <- grep("yearlyPPT", names(simTime2))) > 1) {
              simTime2[[idx]]
            } else {
              simTime2[[pmatch("doy_NSadj_default_", names(simTime2))]]
            }

            yearlyPPT_doyRange <- tapply(prcp.dy$ppt[dailyrange], simTime2$year_ForEachUsedDay_NSadj[dailyrange], sum)
            snowofppt_doyRange<- prcp.dy$snowfall[dailyrange]/prcp.dy$ppt[dailyrange]
            snowofppt_doyRange <- tapply(snowofppt_doyRange, simTime2$year_ForEachUsedDay_NSadj[dailyrange], mean, na.rm=TRUE)

            resMeans[nv] <- mean(yearlyPPT_doyRange)
            resSDs[nv] <- stats::sd(yearlyPPT_doyRange)
            resMeans[nv+1] <- mean(snowofppt_doyRange, na.rm = TRUE)
            resSDs[nv+1] <- stats::sd(snowofppt_doyRange, na.rm = TRUE)
            nv <- nv+2

            rm(dailyrange, yearlyPPT_doyRange, snowofppt_doyRange)
          }

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "yearlyPPT")
        }

      #9
        if (isTRUE(prj_todos[["aon"]][["dailySnowpack"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "dailySnowpack")
          if (!exists("prcp.yr")) prcp.yr <- get_PPT_yr(runDataSC, isim_time)
          if (!exists("prcp.dy")) prcp.dy <- get_PPT_dy(runDataSC, isim_time)
          if (!exists("SWE.dy")) SWE.dy <- get_SWE_dy(runDataSC, isim_time)

          # Fraction of rain that falls on snow
          rainOnSnow <- ifelse(SWE.dy$val > 0, prcp.dy$rain, 0)
          rainOnSnow <- as.matrix(tapply(rainOnSnow, simTime2$year_ForEachUsedDay, sum))
          rainOnSnow <- rainOnSnow / prcp.yr$ppt

          resMeans[nv] <- mean(rainOnSnow, na.rm = TRUE)
          resSDs[nv] <- stats::sd(rainOnSnow, na.rm = TRUE)
          nv <- nv+1

          rm(rainOnSnow)

      #10
          #daily snowpack: adjust_NorthSouth
          if (!exists("SWE.dy")) SWE.dy <- get_SWE_dy(runDataSC, isim_time)
          if (!exists("wateryears")) wateryears <- simTime2$year_ForEachUsedDay_NSadj_WaterYearAdj

          wateryearsN <- length(unique(wateryears))
          if (sum(SWE.dy$val) > 0 && wateryearsN - 2 > 0) {
            temp <- simTime2$doy_ForEachUsedDay[1] == simTime2$doy_ForEachUsedDay_NSadj[1]
            adjDays <- if (temp) {365 - 273} else -91

            res.snow  <- matrix(data = 0, nrow = wateryearsN - 2, ncol = 9, byrow = TRUE)
            # 1. snowyear
            res.snow[, 1]  <- unique(wateryears)[2:(wateryearsN - 1)]
            wateryear.trim <- !is.na(pmatch(wateryears, res.snow[, 1],
              duplicates.ok = TRUE))
            # 2. doy of peak snowpack water-equivalent (mm)
            res.snow[, 2] <- tapply(SWE.dy$val[wateryear.trim],
              wateryears[wateryear.trim], which.max) - adjDays
            # 6. total number of days of snow cover
            res.snow[, 6] <- tapply(SWE.dy$val[wateryear.trim],
              wateryears[wateryear.trim], function(s) sum(s > 0))
            # 7. peak snowpack water-equivalent (mm)
            res.snow[, 7] <- tapply(SWE.dy$val[wateryear.trim],
              wateryears[wateryear.trim], max)

            syi <- 1
            for (sy in res.snow[, 1]) {
              r <- rle(ifelse(SWE.dy$val[which(wateryears == sy)] > 0, 1, 0))
              temp1 <- which(r$values == 1)
              # 5. number of continous days of snow cover
              res.snow[syi, 5] <- r$lengths[temp1][order(r$lengths[temp1],
                decreasing = TRUE)[1]]
              ind <- which(r$lengths == res.snow[syi, 5])
              # 4. last day of continous snow cover
              res.snow[syi, 4] <- cumsum(r$lengths)[ifelse(length(ind) > 1,
                ind[which.max(r$values[ind])], ind)] - adjDays
              # 3. first day of continuous snow cover
              res.snow[syi, 3] <- res.snow[syi, 4] - res.snow[syi, 5]
              # 8. first day of any snow cover
              res.snow[syi, 8] <- ifelse(length(ind) > 0,
                cumsum(r$lengths)[min(temp1)] - (r$lengths[min(temp1)] - 1), ind) - adjDays
              # 9. last day of any snow cover
              res.snow[syi, 9] <- ifelse(length(ind) > 0,
                cumsum(r$lengths)[max(temp1)], ind) - adjDays
              syi <- syi + 1
            }

            nvnew <- nv + 7
            if (nrow(res.snow) > 1) {
              resMeans[nv:nvnew] <- c(
                apply(res.snow[, 2:4], 2, circ_mean, int = 365, na.rm = TRUE),
                apply(res.snow[, 5:7], 2, mean, na.rm = TRUE),
                apply(res.snow[, 8:9], 2, circ_mean, int = 365, na.rm = TRUE))
              resSDs[nv:nvnew] <- c(
                apply(res.snow[, 2:4], 2, circ_sd, int = 365, na.rm = TRUE),
                apply(res.snow[, 5:7], 2, stats::sd, na.rm = TRUE),
                apply(res.snow[, 8:9], 2, circ_sd, int = 365, na.rm = TRUE))

            } else {
              resMeans[nv:nvnew] <- res.snow[1, -1]
              resSDs[nv:nvnew] <- 0
            }

            nv <- nvnew + 1
            rm(res.snow)

            if (isTRUE(opt_agg[["use_doy_range"]])) {
              #daily options
              idx <- grep("doy_NSadj_dailySnowpack", names(simTime2))
              dailyrange <- if (length(idx) > 1) {
                  simTime2[[idx]]
                } else {
                  simTime2[[pmatch("doy_NSadj_defaultWateryear", names(simTime2))]]
                }

              wateryears.doy <- wateryears[wateryear.trim][dailyrange[wateryear.trim]]
              SWE.doy <- SWE.dy$val[wateryear.trim][dailyrange[wateryear.trim]]
              res.snow.doy <- matrix(data = 0, nrow = wateryearsN-2, ncol = 4, byrow = TRUE)
              res.snow.doy[, 1] <- unique(wateryears)[2:(wateryearsN-1)]  # 1. water year
              res.snow.doy[, 2] <- tapply(SWE.doy, wateryears.doy, which.max) - adjDays # 2. doy of peak snowpack water-equivalent (mm)
              res.snow.doy[, 3] <- tapply(SWE.doy, wateryears.doy, function(s) sum(s > 0)) # 3. total number of days of snow cover
              res.snow.doy[, 4] <- tapply(SWE.doy, wateryears.doy, max) # 4. peak snowpack water-equivalent (mm)

              nvnew <- nv + 2
              if (nrow(res.snow.doy) > 1) {
                resMeans[nv:nvnew] <- c(
                  circ_mean(res.snow.doy[, 2], int = 365, na.rm = TRUE),
                  apply(res.snow.doy[, 3:4], 2, mean, na.rm = TRUE))
                resSDs[nv:nvnew] <- c(
                  circ_sd(res.snow.doy[, 2], int = 365, na.rm = TRUE),
                  apply(res.snow.doy[, 3:4], 2, stats::sd, na.rm = TRUE))

              } else {
                resMeans[nv:nvnew] <- res.snow.doy[1, -1]
                resSDs[nv:nvnew] <- 0
              }

              nv <- nvnew + 1
              rm(res.snow.doy, wateryears.doy, SWE.doy)
            }
            rm(wateryears, wateryear.trim, adjDays)

          } else {
            # No snow or too short a simulation period
            resMeans[nv:(nv + 7)] <- resSDs[nv:(nv + 7)] <- 0
            nv <- nv + 8

            if (isTRUE(opt_agg[["use_doy_range"]])) {
              resMeans[nv:(nv + 2)] <- resSDs[nv:(nv + 2)] <- 0
              nv <- nv + 3
            }
          }

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "dailySnowpack")
        }


      #11
        if (isTRUE(prj_todos[["aon"]][["dailyFrostInSnowfreePeriod"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "dailyFrostInSnowfreePeriod")
          if (!exists("temp.dy")) temp.dy <- get_Temp_dy(runDataSC, isim_time)
          if (!exists("SWE.dy")) SWE.dy <- get_SWE_dy(runDataSC, isim_time)
          if (!exists("wateryears")) wateryears <- simTime2$year_ForEachUsedDay_NSadj_WaterYearAdj

          # 1. Trimmed water years -- the first simulation year must be ignored
          wateryear.unique <- unique(wateryears)
          wateryear.trim <- !is.na(pmatch(wateryears, wateryear.unique[2:(length(wateryear.unique)-1)], duplicates.ok = TRUE))
          res.frost <- matrix(data = 0, nrow = length(wateryear.unique)-2, ncol = 4, byrow = TRUE)
          res.frost[, 1] <- wateryear.unique[2:(length(wateryear.unique)-1)]
          rm(wateryear.unique)

          for (iTmin in opt_agg[["Tmin_crit_C"]]) {

            # 2. Number of days with min. temp < 0 and snow == 0 for the trimmed water years
            ifelse(any(is.na(temp.dy$surface)), temps <- temp.dy$min[wateryear.trim], temps <- temp.dy$surface[wateryear.trim])
            frostWithoutSnow <- SWE.dy$val[wateryear.trim] == 0 & temps < iTmin
            res.frost[, 2] <- tapply(frostWithoutSnow, wateryears[wateryear.trim], sum)

            # Find the last day of continuous snow pack for the first water year
            # For the other years, they will just utilize the previous year's data
            daysOfThisYear <- which(wateryears == res.frost[1, 1]) # Use the full water years to retain accurate indicies for snow / temp data
            r <- rle(ifelse(SWE.dy$val[daysOfThisYear] > 0, 1, 0))
            x <- r$lengths[which(r$values == 1)][order(r$lengths[which(r$values == 1)], decreasing = TRUE)[1]] # Number of days in largest continuous snow period
            ind <- which(r$lengths == x)
            # If there is no snow, the best we can do is mark the beginning of the first water year as the end of the last continuous snow period
            thisSnowPeriodEndDay <- ifelse(length(ind) != 0, cumsum(r$lengths)[ifelse(length(ind)>1, ind[which.max(r$values[ind])], ind)], 1)

            # Now go through the first water year until the second to last year.
            # Calculate the number of days with min. temp < 0 and snow == 0 between the end of
            # this year's longest continuous snow pack and the beginning of next year's largest continuous snow pack.
            # Additionally, split these results into the first half of the period (Spring) and the second half (Fall).
            for (syi in 1:(length(res.frost[, 1]) - 1)) {
              # Find the beginning and end of continuous snow pack for the next water year
              daysOfNextYear <- which(wateryears == (res.frost[syi, 1] + 1))
              r <- rle(ifelse(SWE.dy$val[daysOfNextYear] > 0, 1, 0))
              x <- r$lengths[which(r$values == 1)][order(r$lengths[which(r$values == 1)], decreasing = TRUE)[1]]
              ind <- which(r$lengths == x)

              # If next year has no snow, just keep track of its days and it will become a part of the period between snow packs
              if (length(ind) == 0) {
                # If it is the last year, we must mark the last day of the year as the beginning of the next continuous
                # snow pack, so that we can analyze the data that we have
                if (syi == length(res.frost[, 1])) {
                  nextSnowPeriodStartDay <- length(daysOfNextYear)
                } else {
                  daysOfThisYear <- c(daysOfThisYear, daysOfNextYear)
                  next
                }
              }
              # If next year has snow, calculate the start and end of its longest continuous snow pack
              else {
                nextSnowPeriodEndDay <- cumsum(r$lengths)[ifelse(length(ind)>1, ind[which.max(r$values[ind])], ind)]
                nextSnowPeriodStartDay <- nextSnowPeriodEndDay - x
              }

              # Calculate the half-way point - half of the days between end and start of longest continuous snowpack -
              # so that there is no double counting of frost events.
              Y <- floor((nextSnowPeriodStartDay + length(daysOfThisYear) - thisSnowPeriodEndDay) / 2)

              # 3. Spring - first half of days between continuous snow periods with min. temp < 0 and snow == 0
              spring <- (thisSnowPeriodEndDay + daysOfThisYear[1]):(thisSnowPeriodEndDay + daysOfThisYear[1] + Y)
              ifelse(any(is.na(temp.dy$surface[spring])), temps <- temp.dy$min[spring], temps <- temp.dy$surface[spring])
              res.frost[syi, 3] <- sum(SWE.dy$val[spring] == 0 & temps < iTmin)

              # 4. Fall - second half of days between continuous snow periods with min. temp < 0 and snow == 0
              fall <- (spring[length(spring)] + 1):(nextSnowPeriodStartDay + daysOfNextYear[1])
              ifelse(any(is.na(temp.dy$surface[fall])), temps <- temp.dy$min[fall], temps <- temp.dy$surface[fall])
              res.frost[syi, 4] <- sum(SWE.dy$val[fall] == 0 & temps < iTmin)

              # Keep track of this year's data so that we do not have to re-calculate the previous year
              thisSnowPeriodEndDay <- nextSnowPeriodEndDay
              daysOfThisYear <- daysOfNextYear
            }

              resMeans[nv:(nv+2)] <- apply(res.frost[, 2:4], 2, mean, na.rm = TRUE)
              resSDs[nv:(nv+2)] <- apply(res.frost[, 2:4], 2, stats::sd, na.rm = TRUE)
              nv <- nv+3
            }

          rm(frostWithoutSnow)

          if (isTRUE(opt_agg[["use_doy_range"]])) {

            dailyrange <- if (length(idx <- grep("doy_NSadj_dailyFrostinSnowPeriod", names(simTime2))) > 1) {
                          simTime2[[idx]]
                        } else {
                          simTime2[[pmatch("doy_NSadj_defaultWateryear", names(simTime2))]]
                        }

            for (iTmin in opt_agg[["Tmin_crit_C"]]) {

              ifelse(any(is.na(temp.dy$surface)), temps <- temp.dy$min[wateryear.trim], temps <- temp.dy$surface[wateryear.trim])
              frostWithoutSnowDailyRange <- SWE.dy$val[wateryear.trim] == 0 & temps < iTmin & dailyrange[wateryear.trim]
              frostWithoutSnowDailyRange <- tapply(frostWithoutSnowDailyRange,  wateryears[wateryear.trim], sum)  #Numbers of days with min.temp < 0 and snow == 0 within daily range

              resMeans[nv] <- mean(frostWithoutSnowDailyRange, na.rm = TRUE)
              resSDs[nv] <- stats::sd(frostWithoutSnowDailyRange, na.rm = TRUE)
              nv <- nv+1
            }

            rm(frostWithoutSnowDailyRange, dailyrange)
          }

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "dailyFrostInSnowfreePeriod")
        }


      #12
        if (isTRUE(prj_todos[["aon"]][["dailyHotDays"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "dailyHotDays")
          if (!exists("temp.dy")) temp.dy <- get_Temp_dy(runDataSC, isim_time)

          nv_add <- length(opt_agg[["Tmax_crit_C"]])

          dailyExcess <- temp.dy$max >
            matrix(rep.int(opt_agg[["Tmax_crit_C"]], length(temp.dy$max)),
              ncol = nv_add, byrow = TRUE)

          HotDays <- matrix(NA, nrow = isim_time$no.useyr, ncol = nv_add)
          for (k in seq_len(nv_add))
            HotDays[, k] <- tapply(dailyExcess[, k],
              INDEX = simTime2$year_ForEachUsedDay,
              FUN = sum)

          nv_new <- nv + nv_add
          resMeans[nv:(nv_new - 1)] <- .colMeans(HotDays, isim_time$no.useyr, nv_add)
          resSDs[nv:(nv_new - 1)] <- apply(HotDays, 2, stats::sd)
          nv <- nv_new

          rm(HotDays, dailyExcess)

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "dailyHotDays")
        }

      #12b
        if (isTRUE(prj_todos[["aon"]][["dailyWarmDays"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "dailyWarmDays")
          if (!exists("temp.dy")) temp.dy <- get_Temp_dy(runDataSC, isim_time)

          nv_add <- length(opt_agg[["Tmean_crit_C"]])

          dailyExcess <- temp.dy$mean >
            matrix(rep.int(opt_agg[["Tmean_crit_C"]], length(temp.dy$mean)),
              ncol = nv_add, byrow = TRUE)

          WarmDays <- matrix(NA, nrow = isim_time$no.useyr, ncol = nv_add)
          for (k in seq_len(nv_add))
            WarmDays[, k] <- tapply(dailyExcess[, k],
              INDEX = simTime2$year_ForEachUsedDay,
              FUN = sum)

          nv_new <- nv + nv_add
          resMeans[nv:(nv_new - 1)] <- .colMeans(WarmDays, isim_time$no.useyr, nv_add)
          resSDs[nv:(nv_new - 1)] <- apply(WarmDays, 2, stats::sd)
          nv <- nv_new

          rm(WarmDays, dailyExcess)

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "dailyWarmDays")
        }

      #12c
        if (isTRUE(prj_todos[["aon"]][["dailyColdDays"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "dailyColdDays")
          if (!exists("temp.dy")) temp.dy <- get_Temp_dy(runDataSC, isim_time)

          nv_add <- length(opt_agg[["Tmin_crit_C"]])

          dailyExcess <- temp.dy$surface <
            matrix(rep.int(opt_agg[["Tmin_crit_C"]], length(temp.dy$surface)),
                   ncol = nv_add, byrow = TRUE)

          ColdDays <- matrix(NA, nrow = isim_time$no.useyr, ncol = nv_add)
          for (k in seq_len(nv_add))
            ColdDays[, k] <- tapply(dailyExcess[, k],
                                    INDEX = simTime2$year_ForEachUsedDay,
                                    FUN = sum)

          nv_new <- nv + nv_add
          resMeans[nv:(nv_new - 1)] <- .colMeans(ColdDays, isim_time$no.useyr, nv_add)
          resSDs[nv:(nv_new - 1)] <- apply(ColdDays, 2, stats::sd)
          nv <- nv_new

          rm(ColdDays, dailyExcess)

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "dailyColdDays")
        }

      #12d
        if (isTRUE(prj_todos[["aon"]][["dailyCoolDays"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "dailyCoolDays")
          if (!exists("temp.dy")) temp.dy <- get_Temp_dy(runDataSC, isim_time)

          nv_add <- length(opt_agg[["Tmean_crit_C"]])

          dailyExcess <- temp.dy$surface <
            matrix(rep.int(opt_agg[["Tmean_crit_C"]], length(temp.dy$surface)),
                   ncol = nv_add, byrow = TRUE)

          CoolDays <- matrix(NA, nrow = isim_time$no.useyr, ncol = nv_add)
          for (k in seq_len(nv_add))
            CoolDays[, k] <- tapply(dailyExcess[, k],
                                    INDEX = simTime2$year_ForEachUsedDay,
                                    FUN = sum)

          nv_new <- nv + nv_add
          resMeans[nv:(nv_new - 1)] <- .colMeans(CoolDays, isim_time$no.useyr, nv_add)
          resSDs[nv:(nv_new - 1)] <- apply(CoolDays, 2, stats::sd)
          nv <- nv_new

          rm(CoolDays, dailyExcess)

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "dailyCoolDays")
        }

      #13  #daily weather frequency distributions
        if (isTRUE(prj_todos[["aon"]][["dailyPrecipitationEventSizeDistribution"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "dailyPrecipitationEventSizeDistribution")
          if (!exists("prcp.dy")) prcp.dy <- get_PPT_dy(runDataSC, isim_time)

          #prcp-event sizes in bins
          ppt_sizes <- tabulate_values_in_bins(
            x = prcp.dy$ppt, method = "values", vcrit = 0,
            bins = opt_agg[["bin_prcp_mm"]], nbins = 7,
            simTime = isim_time, simTime2 = simTime2)

          resMeans[nv] <- mean(ppt_sizes$eventsPerYear)
          resSDs[nv] <- stats::sd(ppt_sizes$eventsPerYear)
          resMeans[(nv+1):(nv+7)] <- apply(ppt_sizes$freq.summary, 1, mean)
          resSDs[(nv+1):(nv+7)] <- apply(ppt_sizes$freq.summary, 1, stats::sd)
          nv <- nv+8

          rm(ppt_sizes)

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "dailyPrecipitationEventSizeDistribution")
        }

      #15
        if (isTRUE(prj_todos[["aon"]][["yearlyPET"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "yearlyPET")
          if (!exists("PET.yr")) PET.yr <- get_PET_yr(runDataSC, isim_time)

          resMeans[nv] <- mean(PET.yr$val)
          resSDs[nv] <- stats::sd(PET.yr$val)
          nv <- nv+1

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "yearlyPET")
        }

      #16
        #correl monthly swp (top and bottom) vs. pet and ppt vs. temp, use product moment correlation coefficient {eqn. 11.6, \Sala, 1997 #45}
        if (isTRUE(prj_todos[["aon"]][["monthlySeasonalityIndices"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "monthlySeasonalityIndices")
          if (!exists("vwcmatric.mo")) vwcmatric.mo <- get_Response_aggL(swof["sw_vwcmatric"], tscale = "mo", scaler = 1, FUN = stats::weighted.mean, weights = layers_width, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)
          if (!exists("swpmatric.mo")) swpmatric.mo <- get_SWPmatric_aggL(vwcmatric.mo, texture, sand, clay)
          if (!exists("temp.mo")) temp.mo <- get_Temp_mo(runDataSC, isim_time)
          if (!exists("prcp.mo")) prcp.mo <- get_PPT_mo(runDataSC, isim_time)
          if (!exists("PET.mo")) PET.mo <- get_PET_mo(runDataSC, isim_time)

          #in case var(ppt or swp) == 0 => cor is undefined: exclude those years
          temp <- by(data.frame(PET.mo$val, swpmatric.mo$top), simTime2$yearno_ForEachUsedMonth, cor2)
          resMeans[nv] <- mean(temp, na.rm = TRUE)
          resSDs[nv] <- stats::sd(temp, na.rm = TRUE)

          if (length(bottomL) > 0 && !identical(bottomL, 0)) {
            temp <- by(data.frame(PET.mo$val, swpmatric.mo$bottom), simTime2$yearno_ForEachUsedMonth, cor2)
            resMeans[nv+1] <- mean(temp, na.rm = TRUE)
            resSDs[nv+1] <- stats::sd(temp, na.rm = TRUE)
          }

          temp <- by(data.frame(temp.mo$mean, prcp.mo$ppt), simTime2$yearno_ForEachUsedMonth, cor2)
          resMeans[nv+2] <- mean(temp, na.rm = TRUE)
          resSDs[nv+2] <- stats::sd(temp, na.rm = TRUE)

          nv <- nv+3

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "monthlySeasonalityIndices")
        }


        #---Aggregation: Climatic dryness
      #17
        if (isTRUE(prj_todos[["aon"]][["yearlymonthlyTemperateDrylandIndices"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "yearlymonthlyTemperateDrylandIndices")
          if (!exists("prcp.yr")) prcp.yr <- get_PPT_yr(runDataSC, isim_time)
          if (!exists("PET.yr")) PET.yr <- get_PET_yr(runDataSC, isim_time)
          if (!exists("temp.mo")) temp.mo <- get_Temp_mo(runDataSC, isim_time)

          di.ts <- calc_drylandindices(annualPPT = prcp.yr$ppt, annualPET = PET.yr$val,
                                      monthlyTemp = temp.mo$mean)

          meanmonthlyTemp <- tapply(temp.mo$mean, simTime2$month_ForEachUsedMonth, mean)
          di.normals <- calc_drylandindices(annualPPT = mean(prcp.yr$ppt),
                                      annualPET = mean(PET.yr$val),
                                      monthlyTemp = meanmonthlyTemp)

          resMeans[nv:(nv+2)] <- unlist(di.normals)
          resMeans[(nv+3):(nv+5)] <- sapply(di.ts, mean, na.rm = TRUE)
          resSDs[(nv+3):(nv+5)] <- sapply(di.ts, stats::sd, na.rm = TRUE)
          nv <- nv+6

          rm(di.ts, di.normals)

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "yearlymonthlyTemperateDrylandIndices")
        }

      #18
        if (isTRUE(prj_todos[["aon"]][["yearlyDryWetPeriods"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "yearlyDryWetPeriods")
          if (!exists("prcp.yr")) prcp.yr <- get_PPT_yr(runDataSC, isim_time)
          temp.rle <- rle(as.vector(sign(prcp.yr$ppt - mean(prcp.yr$ppt))))

          resMeans[nv:(nv+1)] <- c(stats::quantile(temp.rle$lengths[temp.rle$values == -1], probs = 0.9, type = 7), stats::quantile(temp.rle$lengths[temp.rle$values == 1], probs = 0.9, type = 7))
          nv <- nv+2

          rm(temp.rle)

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "yearlyDryWetPeriods")
        }

      #19 #daily response to weather generator treatments
        if (isTRUE(prj_todos[["aon"]][["dailyWeatherGeneratorCharacteristics"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "dailyWeatherGeneratorCharacteristics")
          if (!exists("prcp.dy")) prcp.dy <- get_PPT_dy(runDataSC, isim_time)
          if (!exists("temp.dy")) temp.dy <- get_Temp_dy(runDataSC, isim_time)

          # until rSFSW2 v1.4.4: dws, dds, and tv were calculated as mean of all months
          # pooled across years
          # now: they are aggregated across years on the means for each month x year
          dws <- daily_spells_permonth(prcp.dy$ppt > 0, simTime2) # wet spells
          dds <- daily_spells_permonth(prcp.dy$ppt < SFSW2_glovars[["tol"]], simTime2) # dry spells

          temp <- tapply(temp.dy$mean,
            simTime2$month_ForEachUsedDay_NSadj + 100 * simTime2$year_ForEachUsedDay_NSadj,
            stats::sd)
          tv <- matrix(temp, nrow = 12)

          resMeans[nv+SFSW2_glovars[["st_mo"]]-1] <- apply(dws, 1, mean, na.rm = TRUE)
          resSDs[nv+SFSW2_glovars[["st_mo"]]-1] <- apply(dws, 1, stats::sd, na.rm = TRUE)
          resMeans[nv+SFSW2_glovars[["st_mo"]]-1+12] <- apply(dds, 1, mean, na.rm = TRUE)
          resSDs[nv+SFSW2_glovars[["st_mo"]]-1+12] <- apply(dds, 1, stats::sd, na.rm = TRUE)
          resMeans[nv+SFSW2_glovars[["st_mo"]]-1+24] <- apply(tv, 1, mean, na.rm = TRUE)
          resSDs[nv+SFSW2_glovars[["st_mo"]]-1+24] <- apply(tv, 1, stats::sd, na.rm = TRUE)
          nv <- nv+36

          rm(dws, dds, tv)

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "dailyWeatherGeneratorCharacteristics")
        }

      #20  #daily weather frequency distributions
        if (isTRUE(prj_todos[["aon"]][["dailyPrecipitationFreeEventDistribution"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "dailyPrecipitationFreeEventDistribution")
          if (!exists("prcp.dy")) prcp.dy <- get_PPT_dy(runDataSC, isim_time)

          #duration of prcp-free days in bins
          ppt_free <- tabulate_values_in_bins(
            x = prcp.dy$ppt <= SFSW2_glovars[["tol"]], method = "duration",
            bins = opt_agg[["bin_prcpfree_days"]], nbins = 4,
            simTime = isim_time, simTime2 = simTime2)

          resMeans[nv] <- mean(ppt_free$eventsPerYear)
          resSDs[nv] <- stats::sd(ppt_free$eventsPerYear)
          resMeans[(nv+1):(nv+4)] <- apply(ppt_free$freq.summary, 1, mean)
          resSDs[(nv+1):(nv+4)] <- apply(ppt_free$freq.summary, 1, stats::sd)
          nv <- nv+5

          rm(ppt_free)

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "dailyPrecipitationFreeEventDistribution")
        }

      #21
        if (isTRUE(prj_todos[["aon"]][["monthlySPEIEvents"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "monthlySPEIEvents")
          #standardized precipitation-evapotranspiration index, SPEI: Vicente-Serrano, S.M., Beguer, S., Lorenzo-Lacruz, J., Camarero, J.s.J., Lopez-Moreno, J.I., Azorin-Molina, C., Revuelto, J.s., Morn-Tejeda, E. & Sanchez-Lorenzo, A. (2012) Performance of Drought Indices for Ecological, Agricultural, and Hydrological Applications. Earth Interactions, 16, 1-27.
          if (!exists("PET.mo")) PET.mo <- get_PET_mo(runDataSC, isim_time)
          if (!exists("prcp.mo")) prcp.mo <- get_PPT_mo(runDataSC, isim_time)

          #n_variables is set for 4*4*3 with length(binSPEI_m) == 4 && length(probs) == 3
          binSPEI_m <- c(1, 12, 24, 48) #months
          probs <- c(0.025, 0.5, 0.975)
          iresp <- rep(1:4, each = length(probs))

          for (iscale in seq_along(binSPEI_m)) {
            rvec <- rep(NA, times = 4 * length(probs))
            if (binSPEI_m[iscale] < length(prcp.mo$ppt) && requireNamespace("SPEI")) {
              spei_m <- as.numeric(SPEI::spei(prcp.mo$ppt - PET.mo$val, scale = binSPEI_m[iscale])$fitted)
              spei_m <- spei_m[!is.na(spei_m)]
              runs <- rle(spei_m >= 0)

              if (sum(runs$values) > 0) {
                rvec[iresp == 1] <- stats::quantile(runs$lengths[runs$values], probs = probs, type = 7) #duration of positive spells
                rvec[iresp == 2] <- stats::quantile(spei_m[spei_m >= 0], probs = probs, type = 7) #intensity of positive spells
              }
              if (sum(!runs$values) > 0) {
                rvec[iresp == 3] <- stats::quantile(runs$lengths[!runs$values], probs = probs, type = 7) #duration of negative spells
                rvec[iresp == 4] <- stats::quantile(spei_m[spei_m < 0], probs = probs, type = 7) #intensity of positive spells
              }

            } else {
              print(paste0(tag_simpidfid, ": package 'SPEI' missing",
                "or simulation period shorter than ", binSPEI_m[iscale], " months. ",
                "'monthlySPEIEvents' are not calculated."))
            }

            resMeans[nv:(nv+length(rvec)-1)] <- rvec
            nv <- nv+length(rvec)
          }

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "monthlySPEIEvents")
        }


        #---Aggregation: Climatic control
      #22  #Nemani RR, Keeling CD, Hashimoto H et al. (2003) Climate-Driven Increases in Global Terrestrial Net Primary Production from 1982 to 1999. Science, 300, 1560-1563.
        if (isTRUE(prj_todos[["aon"]][["monthlyPlantGrowthControls"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "monthlyPlantGrowthControls")
          if (!exists("temp.mo")) temp.mo <- get_Temp_mo(runDataSC, isim_time)
          if (!exists("PET.mo")) PET.mo <- get_PET_mo(runDataSC, isim_time)
          if (!exists("prcp.mo")) prcp.mo <- get_PPT_mo(runDataSC, isim_time)

          DayNumber_ForEachUsedMonth <- rle(simTime2$month_ForEachUsedDay)$lengths
          DayNumber_ForEachUsedYear <- rle(simTime2$year_ForEachUsedDay)$lengths

          #temperature control
          temp <- ifelse(temp.mo$min > 5, 1,
                  ifelse(temp.mo$min < -5, 0,
                  (5 + temp.mo$min) / 10)) * DayNumber_ForEachUsedMonth
          control_temp <- tapply(temp, simTime2$yearno_ForEachUsedMonth, sum) / DayNumber_ForEachUsedYear

          #moisture control
          aridity <- (prcp.mo$rain + prcp.mo$snowmelt) / PET.mo$val
          temp <- ifelse(aridity > 0.75, 1,
                  ifelse(aridity < 0, 0, aridity / 0.75)) * DayNumber_ForEachUsedMonth
          control_water <- tapply(temp, simTime2$yearno_ForEachUsedMonth, sum) / DayNumber_ForEachUsedYear

          #radiation control
          cloudiness <- rSOILWAT2::swCloud_SkyCover(swRunScenariosData[[sc]])
          cloudiness <- rep(cloudiness, times = isim_time$no.useyr)

          temp <- (1 - ifelse(cloudiness < 10, 0, (cloudiness - 10) / 100 * 0.5)) * DayNumber_ForEachUsedMonth
          control_radiation <- tapply(temp, simTime2$yearno_ForEachUsedMonth, sum) / DayNumber_ForEachUsedYear

          temp <- data.frame(control_temp, control_water, control_radiation)
          resMeans[nv:(nv+2)] <- apply(temp, 2, mean, na.rm = TRUE)
          resSDs[nv:(nv+2)] <- apply(temp, 2, stats::sd, na.rm = TRUE)
          nv <- nv+3

          rm(DayNumber_ForEachUsedMonth, DayNumber_ForEachUsedYear, control_temp, control_water, control_radiation, aridity, temp, cloudiness)

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "monthlyPlantGrowthControls")
        }

      #23 #Variables to estimate percent C4 species in North America: Teeri JA, Stowe LG (1976) Climatic patterns and the distribution of C4 grasses in North America. Oecologia, 23, 1-12.
        if (isTRUE(prj_todos[["aon"]][["dailyC4_TempVar"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "dailyC4_TempVar")
          if (!exists("temp.dy")) temp.dy <- get_Temp_dy(runDataSC, isim_time)

          resMeans[nv:(nv+2)] <- (temp <- as.numeric(sw_dailyC4_TempVar(dailyTempMin = temp.dy$min, dailyTempMean = temp.dy$mean, simTime2)))[1:3]  #adjust_NorthSouth
          resSDs[nv:(nv+2)] <- temp[4:6]
          nv <- nv+3

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "dailyC4_TempVar")
        }

      #24  #Degree days based on daily temp
        if (isTRUE(prj_todos[["aon"]][["dailyDegreeDays"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "dailyDegreeDays")
          if (!exists("temp.dy")) temp.dy <- get_Temp_dy(runDataSC, isim_time)

          degday <- ifelse(temp.dy$mean > opt_agg[["Tbase_DD_C"]],
            temp.dy$mean - opt_agg[["Tbase_DD_C"]], 0) #degree days
          temp <- tapply(degday, simTime2$year_ForEachUsedDay, sum)

          resMeans[nv] <- mean(temp)
          resSDs[nv] <- stats::sd(temp)
          nv <- nv+1

          rm(degday)

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "dailyDegreeDays")
        }

      #25 # Cold-degree days based on temperature
        if (isTRUE(prj_todos[["aon"]][["dailyColdDegreeDays"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "dailyColdDegreeDays")
          if (!exists("temp.dy")) temp.dy <- get_Temp_dy(runDataSC, isim_time)
          if (!exists("SWE.dy")) SWE.dy <- get_SWE_dy(runDataSC, isim_time)

          # Cold-degree daily mean temperatures (degree C) with snow
          ids <- temp.dy$mean < opt_agg[["Tbase_coldDD_C"]]
          colddegday <- ifelse(ids, temp.dy$mean - opt_agg[["Tbase_coldDD_C"]], 0)

          # Cold-degree daily mean temperatures (degree C) without snow
          ids_snowfree <- ids & SWE.dy$val <= SFSW2_glovars[["tol"]]
          colddegday_snowfree <- ifelse(ids_snowfree, temp.dy$mean - opt_agg[["Tbase_coldDD_C"]], 0)

          # Sum of daily mean temperatures for snow/snow-free
          temp <- data.frame(tapply(colddegday, simTime2$year_ForEachUsedDay, sum),
                             tapply(colddegday_snowfree, simTime2$year_ForEachUsedDay, sum))

          resMeans[nv:(nv+1)] <- apply(temp, 2, mean, na.rm = TRUE)
          resSDs[nv:(nv+1)] <- apply(temp, 2, stats::sd, na.rm = TRUE)
          nv <- nv + 2

          rm(colddegday, colddegday_snowfree, ids, ids_snowfree)

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "dailyColdDegreeDays")
        }


        #---Aggregation: Yearly water balance
      #27.0
        if (isTRUE(prj_todos[["aon"]][["yearlyAET"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "yearlyAET")
          if (!exists("AET.yr")) AET.yr <- get_AET_yr(runDataSC, isim_time)

          resMeans[nv] <- mean(AET.yr$val)
          resSDs[nv] <- stats::sd(AET.yr$val)
          nv <- nv+1

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "yearlyAET")
        }

      #27
        if (isTRUE(prj_todos[["aon"]][["yearlyWaterBalanceFluxes"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "yearlyWaterBalanceFluxes")
          if (!exists("prcp.yr")) prcp.yr <- get_PPT_yr(runDataSC, isim_time)
          if (!exists("Esurface.yr")) Esurface.yr <- get_Esurface_yr(runDataSC, isim_time)
          if (!exists("intercept.yr")) intercept.yr <- get_Interception_yr(runDataSC, isim_time)
          if (!exists("inf.yr")) inf.yr <- get_Inf_yr(runDataSC, isim_time)
          if (!exists("runonoff.yr")) runonoff.yr <- get_RunOnOff_yr(runDataSC, isim_time)
          if (!exists("transp.yr")) transp.yr <- get_Response_aggL(swof["sw_transp"], tscale = "yr", scaler = 10, FUN = sum, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)
          if (!exists("AET.yr")) AET.yr <- get_AET_yr(runDataSC, isim_time)
          if (!exists("PET.yr")) PET.yr <- get_PET_yr(runDataSC, isim_time)
          if (!exists("Esoil.yr")) Esoil.yr <- get_Response_aggL(swof["sw_evsoil"], tscale = "yr", scaler = 10, FUN = sum, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)
          if (!exists("deepDrain.yr")) deepDrain.yr <- get_DeepDrain_yr(runDataSC, isim_time)

          rain_toSoil <- prcp.yr$rain - intercept.yr$sum
          transp.tot <- transp.yr$top + transp.yr$bottom

          evap_soil.tot <- as.vector(Esoil.yr$top + Esoil.yr$bottom)
          evap.tot <- evap_soil.tot + Esurface.yr$sum + prcp.yr$snowloss

          temp1 <- 10 * slot(slot(runDataSC, swof["sw_percolation"]), "Year")
          drain.topTobottom <- if (length(topL) > 1 && length(bottomL) > 0 && !identical(bottomL, 0)) {
              temp1[isim_time$index.useyr, 1+DeepestTopLayer, drop = FALSE]
            } else NA

          temp1 <- 10 * slot(slot(runDataSC, swof["sw_hd"]), "Year")
          hydred.topTobottom <- if (length(topL) > 1) {
              apply(temp1[isim_time$index.useyr, 1+topL, drop = FALSE], 1, sum)
            } else {
              temp1[isim_time$index.useyr, 1+topL, drop = FALSE]
            }

          temp1 <- 10 * slot(slot(runDataSC, swof["sw_swcbulk"]), "Day")
          index.usedyPlusOne <- if (isim_time$index.usedy[1] == 1) { #simstartyr == startyr, then (isim_time$index.usedy-1) misses first value
              isim_time$index.usedy[-length(isim_time$index.usedy)]+1
            } else {
              isim_time$index.usedy
            }
          swcdyflux <- if (length(ld) > 1) {
              apply(temp1[index.usedyPlusOne, 2+ld], 1, sum) -
                apply(temp1[index.usedyPlusOne-1, 2+ld], 1, sum)
            } else {
              temp1[index.usedyPlusOne, 2+ld] - temp1[index.usedyPlusOne-1, 2+ld]
            }
          swc.flux <- tapply(swcdyflux, temp1[index.usedyPlusOne, 1], sum)

          fluxtemp <- cbind(prcp.yr$rain, rain_toSoil, prcp.yr$snowfall, prcp.yr$snowmelt,
            prcp.yr$snowloss, intercept.yr$sum, intercept.yr$veg, intercept.yr$litter,
            inf.yr$inf, runonoff.yr$total_runoff, runonoff.yr$total_runon,
            evap.tot, Esurface.yr$surfacewater, Esurface.yr$veg, Esurface.yr$litter,
            evap_soil.tot, Esoil.yr$top, Esoil.yr$bottom, transp.tot, transp.yr$top,
            transp.yr$bottom, hydred.topTobottom, drain.topTobottom, deepDrain.yr$val,
            swc.flux)

          nv1 <- nv + ncol(fluxtemp) - 1

          #mean fluxes
          resMeans[nv:nv1] <- apply(fluxtemp, 2, mean)
          resMeans[nv1 + 1] <- if (sum(transp.tot) > 0) mean(transp.yr$bottom/transp.tot) else 0
          resMeans[nv1 + 2] <- if (sum(AET.yr$val) > 0) mean(transp.tot/AET.yr$val) else 0
          resMeans[nv1 + 3] <- if (sum(AET.yr$val) > 0) mean(evap_soil.tot/AET.yr$val) else 0
          resMeans[nv1 + 4] <- if (sum(PET.yr$val) > 0) mean(AET.yr$val/PET.yr$val) else 0
          resMeans[nv1 + 5] <- if (sum(PET.yr$val) > 0) mean(transp.tot/PET.yr$val) else 0
          resMeans[nv1 + 6] <- if (sum(PET.yr$val) > 0) mean(evap_soil.tot/PET.yr$val) else 0

          #stats::sd of fluxes
          resSDs[nv:nv1] <- apply(fluxtemp, 2, stats::sd)
          resSDs[nv1 + 1] <- if (sum(transp.tot) > 0) stats::sd(transp.yr$bottom/transp.tot) else 0
          resSDs[nv1 + 2] <- if (sum(AET.yr$val) > 0) stats::sd(transp.tot/AET.yr$val) else 0
          resSDs[nv1 + 3] <- if (sum(AET.yr$val) > 0) stats::sd(evap_soil.tot/AET.yr$val) else 0
          resSDs[nv1 + 4] <- if (sum(PET.yr$val) > 0) stats::sd(AET.yr$val/PET.yr$val) else 0
          resSDs[nv1 + 5] <- if (sum(PET.yr$val) > 0) stats::sd(transp.tot/PET.yr$val) else 0
          resSDs[nv1 + 6] <- if (sum(PET.yr$val) > 0) stats::sd(evap_soil.tot/PET.yr$val) else 0

          nv <- nv1 + 7

          rm(rain_toSoil, transp.tot, evap_soil.tot, drain.topTobottom, hydred.topTobottom, index.usedyPlusOne, swcdyflux, swc.flux)

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "yearlyWaterBalanceFluxes")
        }

      #27.1
        if (isTRUE(prj_todos[["aon"]][["yearlyTranspirationBySoilLayer"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "yearlyTranspirationBySoilLayer")
          if (!exists("transp.yr.all")) transp.yr.all <- get_Response_aggL(swof["sw_transp"], tscale = "yrAll", scaler = 10, FUN = sum, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)

          # aggregate across years for each soil layer and vegetation type
          vegtypes <- c("total", "tree", "shrub", "forb", "grass")
          coln <- colnames(transp.yr.all[["val"]])

          for (k in vegtypes) {
            temp <- transp.yr.all[["val"]][, grep(k, coln)[ld], drop = FALSE]
            nv1 <- nv + soilLayers_N - 1
            resMeans[nv:nv1] <- colMeans(temp)
            resSDs[nv:nv1] <- apply(temp, 2, stats::sd)
            nv <- nv + SFSW2_glovars[["slyrs_maxN"]]
          }

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "yearlyTranspirationBySoilLayer")
        }

      #27.2
        if (isTRUE(prj_todos[["aon"]][["dailySoilWaterPulseVsStorage"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "dailySoilWaterPulseVsStorage")
          if (!exists("inf.dy")) inf.dy <- get_Inf_dy(runDataSC, isim_time)
          if (!exists("transp.dy.all")) transp.dy.all <- get_Response_aggL(swof["sw_transp"], tscale = "dyAll", scaler = 10, FUN = sum, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)
          if (!exists("Esoil.dy.all")) Esoil.dy.all <- get_Response_aggL(swof["sw_evsoil"], tscale = "dyAll", scaler = 10, FUN = sum, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)
          if (!exists("deepDrain.dy")) deepDrain.dy <- get_DeepDrain_dy(runDataSC, isim_time)

          percolation <- if (soilLayers_N > 1) {
              10 * slot(slot(runDataSC, swof["sw_percolation"]), "Day")[isim_time$index.usedy, 2 + ld[-soilLayers_N]]
            } else {
              rep(0, isim_time$no.usedy)
            }
          hydred <- 10 * slot(slot(runDataSC, swof["sw_hd"]), "Day")[isim_time$index.usedy, 2 + ld]

          # Water balance
          outputs_by_layer <- inputs_by_layer <- matrix(0, nrow = isim_time$no.usedy, ncol = soilLayers_N,
            dimnames = list(NULL, paste0("total_Lyr_", ld)))
          # Inputs: infiltration + received hydraulic redistribution + received percolation
          inputs_by_layer[, 1] <- inputs_by_layer[, 1] + inf.dy$inf
          inputs_by_layer <- inputs_by_layer + ifelse(hydred > 0, hydred, 0)
          if (soilLayers_N > 1) {
            inputs_by_layer[, -1] <- inputs_by_layer[, -1] + ifelse(percolation > 0, percolation, 0)
          }

          # Outputs: soil evaporation + transpiration + deep drainage + hydraulic redistribution donor + percolation donor
          if (ncol(Esoil.dy.all$val) > 2) {
            itemp <- seq_len(ncol(Esoil.dy.all$val) - 2)
            outputs_by_layer[, itemp] <- outputs_by_layer[, itemp] +
              Esoil.dy.all$val[isim_time$index.usedy, -(1:2)]
          }
          itemp <- grepl("transp_total", colnames(transp.dy.all$val))
          if (any(itemp)) {
            itemp <- seq_len(sum(itemp))
            outputs_by_layer[, itemp] <- outputs_by_layer[, itemp] +
              transp.dy.all$val[isim_time$index.usedy, itemp]
          }
          itemp <- ncol(outputs_by_layer)
          outputs_by_layer[, itemp] <- outputs_by_layer[, itemp] + deepDrain.dy$val
          if (itemp > 1) {
            outputs_by_layer[, -itemp] <- outputs_by_layer[, -itemp] +
              ifelse(percolation < 0, -percolation, 0)
          }
          outputs_by_layer <- outputs_by_layer + ifelse(hydred < 0, -hydred, 0)

          # balance
          balance <- inputs_by_layer - outputs_by_layer
          extraction <- balance < 0
          storage_use <- by(cbind(extraction, outputs_by_layer), INDICES = simTime2$year_ForEachUsedDay_NSadj, FUN = function(x) {
            res1 <- apply(x[, ld, drop = FALSE], MARGIN = 2, FUN = rle)
            res2 <- apply(x[, soilLayers_N + ld, drop = FALSE], MARGIN = 2, FUN = function(y) list(out = y))
            utils::modifyList(res1, res2)
          }, simplify = FALSE)

          # median duration among extracting spells for each layer and each year
          extraction_duration_days <- sapply(storage_use, function(x)
              sapply(x, function(dat) {
                if (is.null(dat$out) || is.null(dat$values)) {
                  NA
                } else {
                  temp <- as.logical(dat$values)
                  if (any(temp)) mean(dat$lengths[as.logical(dat$values)]) else NA
                }
              }))
          if (!is.matrix(extraction_duration_days)) {
            extraction_duration_days <- matrix(extraction_duration_days, nrow = soilLayers_N, ncol = isim_time$no.useyr)
          }

          # median annual sum of all extracted water during extracting spells for each layer and each year
          extraction_summed_mm <- sapply(storage_use, function(x) sapply(x, function(dat) {
              if (is.null(dat$out) || is.null(dat$values)) {
                NA
              } else {
                dat$values <- as.logical(dat$values)
                temp <- dat
                if (any(dat$values))
                  temp$values[dat$values] <- seq_len(sum(dat$values)) # give unique ID to each extraction spell
                if (any(!dat$values)) {
                  temp$values[!dat$values] <- 0 # we are not interested in positive spells
                  has_zero <- TRUE
                } else {
                  has_zero <- FALSE
                }
                storage_ids <- inverse.rle(temp)
                x <- tapply(dat$out, INDEX = storage_ids, sum) # sum up extracted water for each extraction spell
                if (has_zero && length(x) > 0)
                  x <- x[-1] # remove first element because this represents the positive spells (id = 0)

                sum(x)
              }
            }))
          if (!is.matrix(extraction_summed_mm)) {
            extraction_summed_mm <- matrix(extraction_summed_mm, nrow = soilLayers_N, ncol = isim_time$no.useyr)
          }

          # aggregate across years for each soil layer
          resMeans[nv:(nv+soilLayers_N-1)] <- round(apply(extraction_duration_days, 1, mean), 1)
          resSDs[nv:(nv+soilLayers_N-1)] <- round(apply(extraction_duration_days, 1, stats::sd), 1)
          nv <- nv+SFSW2_glovars[["slyrs_maxN"]]
          resMeans[nv:(nv+soilLayers_N-1)] <- round(apply(extraction_summed_mm, 1, mean), 2)
          resSDs[nv:(nv+soilLayers_N-1)] <- round(apply(extraction_summed_mm, 1, stats::sd), 2)
          nv <- nv+SFSW2_glovars[["slyrs_maxN"]]

          rm(percolation, hydred, inputs_by_layer, outputs_by_layer, balance, extraction, storage_use, extraction_duration_days, extraction_summed_mm)

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "dailySoilWaterPulseVsStorage")
        }


        #---Aggregation: Daily extreme values
      #28 #mean and stats::sd of DOY and value of minimum/maximum
        if (isTRUE(prj_todos[["aon"]][["dailyTranspirationExtremes"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "dailyTranspirationExtremes")
          if (!exists("transp.dy")) transp.dy <- get_Response_aggL(swof["sw_transp"], tscale = "dy", scaler = 10, FUN = sum, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)

          temp <- transp.dy$top + transp.dy$bottom
          temp <- tapply(temp, simTime2$year_ForEachUsedDay, extreme_values_and_doys)
          extremes <- matrix(unlist(temp), ncol = 4, byrow = TRUE)

          temp <- extremes[, 1:2, drop = FALSE]
          resMeans[nv:(nv+1)] <- apply(temp, MARGIN = 2, FUN = mean)
          resSDs[nv:(nv+1)] <- apply(temp, MARGIN = 2, FUN = stats::sd)
          nv <- nv+2

          temp <- extremes[, 3:4, drop = FALSE]
          resMeans[nv:(nv+1)] <- apply(temp, MARGIN = 2, circ_mean, int = 365)
          resSDs[nv:(nv+1)] <- apply(temp, MARGIN = 2, circ_sd, int = 365)
          nv <- nv+2

          rm(extremes)

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "dailyTranspirationExtremes")
        }

      #29
        if (isTRUE(prj_todos[["aon"]][["dailyTotalEvaporationExtremes"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "dailyTotalEvaporationExtremes")
          if (!exists("Esoil.dy")) Esoil.dy <- get_Response_aggL(swof["sw_evsoil"], tscale = "dy", scaler = 10, FUN = sum, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)
          if (!exists("Esurface.dy")) Esurface.dy <- get_Esurface_dy(runDataSC, isim_time)

          temp <- Esoil.dy$top + Esoil.dy$bottom + Esurface.dy$sum
          temp <- tapply(temp, simTime2$year_ForEachUsedDay, extreme_values_and_doys)
          extremes <- matrix(unlist(temp), ncol = 4, byrow = TRUE)

          temp <- extremes[, 1:2, drop = FALSE]
          resMeans[nv:(nv+1)] <- apply(temp, MARGIN = 2, FUN = mean)
          resSDs[nv:(nv+1)] <- apply(temp, MARGIN = 2, FUN = stats::sd)
          nv <- nv+2

          temp <- extremes[, 3:4, drop = FALSE]
          resMeans[nv:(nv+1)] <- apply(temp, MARGIN = 2, circ_mean, int = 365)
          resSDs[nv:(nv+1)] <- apply(temp, MARGIN = 2, circ_sd, int = 365)
          nv <- nv+2

          rm(extremes)

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "dailyTotalEvaporationExtremes")
        }

      #30
        if (isTRUE(prj_todos[["aon"]][["dailyDrainageExtremes"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "dailyDrainageExtremes")
          if (!exists("deepDrain.dy")) deepDrain.dy <- get_DeepDrain_dy(runDataSC, isim_time)

          temp <- tapply(deepDrain.dy$val, simTime2$year_ForEachUsedDay, extreme_values_and_doys)
          extremes <- matrix(unlist(temp), ncol = 4, byrow = TRUE)

          temp <- extremes[, 1:2, drop = FALSE]
          resMeans[nv:(nv+1)] <- apply(temp, MARGIN = 2, FUN = mean)
          resSDs[nv:(nv+1)] <- apply(temp, MARGIN = 2, FUN = stats::sd)
          nv <- nv+2

          temp <- extremes[, 3:4, drop = FALSE]
          resMeans[nv:(nv+1)] <- apply(temp, MARGIN = 2, circ_mean, int = 365)
          resSDs[nv:(nv+1)] <- apply(temp, MARGIN = 2, circ_sd, int = 365)
          nv <- nv+2

          rm(extremes)

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "dailyDrainageExtremes")
        }

      #31
        if (isTRUE(prj_todos[["aon"]][["dailyInfiltrationExtremes"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "dailyInfiltrationExtremes")
          if (!exists("inf.dy")) inf.dy <- get_Inf_dy(runDataSC, isim_time)

          temp <- tapply(inf.dy$inf, simTime2$year_ForEachUsedDay, extreme_values_and_doys)
          extremes <- matrix(unlist(temp), ncol = 4, byrow = TRUE)

          temp <- extremes[, 1:2, drop = FALSE]
          resMeans[nv:(nv+1)] <- apply(temp, MARGIN = 2, FUN = mean)
          resSDs[nv:(nv+1)] <- apply(temp, MARGIN = 2, FUN = stats::sd)
          nv <- nv+2

          temp <- extremes[, 3:4, drop = FALSE]
          resMeans[nv:(nv+1)] <- apply(temp, MARGIN = 2, circ_mean, int = 365)
          resSDs[nv:(nv+1)] <- apply(temp, MARGIN = 2, circ_sd, int = 365)
          nv <- nv+2

          rm(extremes)

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "dailyInfiltrationExtremes")
        }

      #32
        if (isTRUE(prj_todos[["aon"]][["dailyAETExtremes"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "dailyAETExtremes")
          if (!exists("AET.dy")) AET.dy <- get_AET_dy(runDataSC, isim_time)

          temp <- tapply(AET.dy$val, simTime2$year_ForEachUsedDay, extreme_values_and_doys)
          extremes <- matrix(unlist(temp), ncol = 4, byrow = TRUE)

          temp <- extremes[, 1:2, drop = FALSE]
          resMeans[nv:(nv+1)] <- apply(temp, MARGIN = 2, FUN = mean)
          resSDs[nv:(nv+1)] <- apply(temp, MARGIN = 2, FUN = stats::sd)
          nv <- nv+2

          temp <- extremes[, 3:4, drop = FALSE]
          resMeans[nv:(nv+1)] <- apply(temp, MARGIN = 2, circ_mean, int = 365)
          resSDs[nv:(nv+1)] <- apply(temp, MARGIN = 2, circ_sd, int = 365)
          nv <- nv+2

          rm(extremes)

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "dailyAETExtremes")
        }

      #33
        if (isTRUE(prj_todos[["aon"]][["dailySWPextremes"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "dailySWPextremes")
          if (!exists("vwcmatric.dy")) vwcmatric.dy <- get_Response_aggL(swof["sw_vwcmatric"], tscale = "dy", scaler = 1, FUN = stats::weighted.mean, weights = layers_width, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)
          if (!exists("swpmatric.dy")) swpmatric.dy <- get_SWPmatric_aggL(vwcmatric.dy, texture, sand, clay)

          extremes <- matrix(NA, nrow = isim_time$no.useyr, ncol = 2 * 4)
          temp <- tapply(swpmatric.dy$top, simTime2$year_ForEachUsedDay, extreme_values_and_doys)
          extremes[, 1:4] <- matrix(unlist(temp), ncol = 4, byrow = TRUE)
          if (length(bottomL) > 0 && !identical(bottomL, 0)) {
            temp <- tapply(swpmatric.dy$bottom, simTime2$year_ForEachUsedDay, extreme_values_and_doys)
            extremes[, 5:8] <- matrix(unlist(temp), ncol = 4, byrow = TRUE)
          }

          temp <- extremes[, c(1:2, 5:6), drop = FALSE]
          resMeans[nv:(nv+3)] <- apply(temp, MARGIN = 2, FUN = mean, na.rm = TRUE)
          resSDs[nv:(nv+3)] <- apply(temp, MARGIN = 2, FUN = stats::sd, na.rm = TRUE)
          nv <- nv+4

          temp <- extremes[, c(3:4, 7:8), drop = FALSE]
          resMeans[nv:(nv+3)] <- apply(temp, MARGIN = 2, circ_mean, int = 365, na.rm = TRUE)
          resSDs[nv:(nv+3)] <- apply(temp, MARGIN = 2, circ_sd, int = 365, na.rm = TRUE)
          nv <- nv+4

          rm(extremes)

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "dailySWPextremes")
        }

      #34
        if (isTRUE(prj_todos[["aon"]][["dailyRechargeExtremes"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "dailyRechargeExtremes")
          if (!exists("swcbulk.dy")) swcbulk.dy <- get_Response_aggL(swof["sw_swcbulk"], tscale = "dy", scaler = 10, FUN = sum, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)

          recharge.dy <- NULL
          recharge.dy$top <- swcbulk.dy$top / (SWPtoVWC(-0.033, texture$sand.top, texture$clay.top) * 10 * sum(layers_width[topL]))
          extremes <- matrix(NA, nrow = isim_time$no.useyr, ncol = 2 * 4)
          temp <- tapply(recharge.dy$top, simTime2$year_ForEachUsedDay, extreme_values_and_doys)
          extremes[, 1:4] <- matrix(unlist(temp), ncol = 4, byrow = TRUE)

          if (length(bottomL) > 0 && !identical(bottomL, 0)) {
            recharge.dy$bottom <- swcbulk.dy$bottom / (SWPtoVWC(-0.033, texture$sand.bottom, texture$clay.bottom) * 10 * sum(layers_width[bottomL]))
            temp <- tapply(recharge.dy$bottom, simTime2$year_ForEachUsedDay, extreme_values_and_doys)
            extremes[, 5:8] <- matrix(unlist(temp), ncol = 4, byrow = TRUE)
          }

          temp <- extremes[, c(1:2, 5:6), drop = FALSE]
          resMeans[nv:(nv+3)] <- apply(temp, MARGIN = 2, FUN = function(x) mean(pmin(1, x), na.rm = TRUE))
          resSDs[nv:(nv+3)] <- apply(temp, MARGIN = 2, FUN = function(x) stats::sd(pmin(1, x), na.rm = TRUE))
          nv <- nv+4

          temp <- extremes[, c(3:4, 7:8), drop = FALSE]
          resMeans[nv:(nv+3)] <- apply(temp, MARGIN = 2, circ_mean, int = 365, na.rm = TRUE)
          resSDs[nv:(nv+3)] <- apply(temp, MARGIN = 2, circ_sd, int = 365, na.rm = TRUE)
          nv <- nv+4

          rm(recharge.dy, extremes)

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "dailyRechargeExtremes")
        }


        #---Aggregation: Ecological dryness
      #35a
        regimes_done <- FALSE
        if (isTRUE(prj_todos[["aon"]][["dailyNRCS_SoilMoistureTemperatureRegimes"]]) ||
          isTRUE(prj_todos[["aon"]][["dailyNRCS_SoilMoistureTemperatureRegimes_Intermediates"]])) {

          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "dailyNRCS_SoilMoistureTemperatureRegimes")
          #Based on references provided by Chambers, J. C., D. A. Pyke, J. D. Maestas, M. Pellant, C. S. Boyd, S. B. Campbell, S. Espinosa, D. W. Havlina, K. E. Mayer, and A. Wuenschel. 2014. Using Resistance and Resilience Concepts to Reduce Impacts of Invasive Annual Grasses and Altered Fire Regimes on the Sagebrush Ecosystem and Greater Sage-Grouse: A Strategic Multi-Scale Approach. Gen. Tech. Rep. RMRS-GTR-326. U.S. Department of Agriculture, Forest Service, Rocky Mountain Research Station, Fort Collins, CO.
          #Soil Survey Staff. 2014. Keys to soil taxonomy, 12th ed., USDA Natural Resources Conservation Service, Washington, DC.

          stopifnot(any(opt_agg[["NRCS_SMTRs"]][["aggregate_at"]] == c("data", "conditions", "regime")))

          #Result containers
          has_simulated_SoilTemp <- has_realistic_SoilTemp <- NA
          SMTR <- list()
          temp <- STR_names()
          SMTR[["STR"]] <- matrix(0, nrow = 1, ncol = length(temp), dimnames = list(NULL, temp))
          temp <- c(SMR_names(), SMRq_names())
          SMTR[["SMR"]] <- matrix(0, nrow = 1, ncol = length(temp), dimnames = list(NULL, temp))

          MCS_depth <- Lanh_depth <- rep(NA, 2)
          Fifty_depth <- permafrost_yrs <- has_Ohorizon <- NA
          SMR_normalyears_N <- 0
          temp_annual <- matrix(NA, nrow = isim_time$no.useyr, ncol = 45, dimnames =
            list(NULL, c("MATLanh", "MAT50", "T50jja", "T50djf", "CSPartSummer",
            "meanTair_Tsoil50_offset_C", paste0("V", 7:45))))

          if (rSOILWAT2::swSite_SoilTemperatureFlag(swRunScenariosData[[sc]]) &&
            isTRUE(!is_SOILTEMP_INSTABLE[sc])) { #we need soil temperature

            has_simulated_SoilTemp <- 1
            if (!exists("soiltemp.dy.all")) soiltemp.dy.all <- get_Response_aggL(swof["sw_soiltemp"], tscale = "dyAll", scaler = 1, FUN = stats::weighted.mean, weights = layers_width, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)

            if (!anyNA(soiltemp.dy.all$val) && all(soiltemp.dy.all$val[, -(1:2)] < 100)) {
              # 100 C as upper realistic limit from Garratt, J.R. (1992). Extreme maximum land surface temperatures. Journal of Applied Meteorology, 31, 1096-1105.
              has_realistic_SoilTemp <- 1
              if (!exists("soiltemp.yr.all")) soiltemp.yr.all <- get_Response_aggL(swof["sw_soiltemp"], tscale = "yrAll", scaler = 1, FUN = stats::weighted.mean, weights = layers_width, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)
              if (!exists("soiltemp.mo.all")) soiltemp.mo.all <- get_Response_aggL(swof["sw_soiltemp"], tscale = "moAll", scaler = 1, FUN = stats::weighted.mean, weights = layers_width, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)
              if (!exists("vwcmatric.dy.all")) vwcmatric.dy.all <- get_Response_aggL(swof["sw_vwcmatric"], tscale = "dyAll", scaler = 1, FUN = stats::weighted.mean, weights = layers_width, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)
              if (!exists("swpmatric.dy.all")) swpmatric.dy.all <- get_SWPmatric_aggL(vwcmatric.dy.all, texture, sand, clay)
              if (!exists("prcp.yr")) prcp.yr <- get_PPT_yr(runDataSC, isim_time)
              if (!exists("prcp.mo")) prcp.mo <- get_PPT_mo(runDataSC, isim_time)
              if (!exists("pet.mo")) pet.mo <- get_PET_mo(runDataSC, isim_time)
              if (!exists("temp.mo")) temp.mo <- get_Temp_mo(runDataSC, isim_time)

              # Prepare data
              #Water year starting Oct 1
              # 1. water-year: N-hemisphere: October 1st = 1 day of water year; S-hemisphere: April 1st = 1 day of water year
              wateryears <- simTime2$year_ForEachUsedDay_NSadj +
                ifelse(simTime2$doy_ForEachUsedDay_NSadj > 273, 1, 0)
              wyears <- (temp <- unique(wateryears))[-length(temp)]#eliminate last year

              if (opt_agg[["NRCS_SMTRs"]][["use_normal"]]) {
                # Normal years for soil moisture regimes (Soil Survey Staff 2014: p.29)
                # Should have a time period of 30 years to determine normal years
                if (isim_time$no.useyr < 30)
                  print(paste0(tag_simpidfid, ": has only", isim_time$no.useyr, "years ",
                    "of data; determination of normal years for NRCS soil moisture ",
                    "regimes should be based on >= 30 years."))

                #   - Annual precipitation that is plus or minus one standard precipitation
                #   - and Mean monthly precipitation that is plus or minus one standard deviation of the long-term monthly precipitation for 8 of the 12 months
                MAP <- c(mean(prcp.yr$ppt), stats::sd(prcp.yr$ppt))
                normal1 <- as.vector((prcp.yr$ppt >= MAP[1] - MAP[2]) &
                    (prcp.yr$ppt <= MAP[1] + MAP[2]))
                MMP <- tapply(prcp.mo$ppt, simTime2$month_ForEachUsedMonth_NSadj,
                  function(x) c(mean(x), stats::sd(x)))
                MMP <- matrix(unlist(MMP), nrow = 2, ncol = 12)
                normal2 <- tapply(prcp.mo$ppt, simTime2$yearno_ForEachUsedMonth_NSadj,
                  function(x) sum((x >= MMP[1, ] - MMP[2, ]) & (x <= MMP[1, ] + MMP[2, ])) >= 8)

                st_NRCS <- list(
                  yr_used = yr_used <- wyears[normal1 & normal2],
                  i_yr_used = findInterval(yr_used, wyears))

                rm(list = c("MAP", "MMP", "normal1", "normal2"))

              } else {
                st_NRCS <- list(
                  yr_used = isim_time$useyrs,
                  i_yr_used = findInterval(isim_time$useyrs, wyears))
              }

              st_NRCS <- c(st_NRCS, list(
                  N_yr_used = length(st_NRCS[["yr_used"]]),
                  i_dy_used = i_dy_used <- wateryears %in% st_NRCS[["yr_used"]],
                  N_dy_used = sum(i_dy_used),
                  i_mo_used = seq_len(isim_time$no.usemo)[rep(wyears, each = 12) %in% st_NRCS[["yr_used"]]],
                  days_per_yr_used = as.integer(table(wateryears[i_dy_used], dnn = FALSE))))

              SMR_normalyears_N <- st_NRCS[["N_yr_used"]]

              soiltemp_nrsc <- list(
                yr = list(data = {temp <- isim_time$index.useyr[st_NRCS[["i_yr_used"]]]
                  soiltemp.yr.all$val[temp, , drop = FALSE]}, nheader = 1),
                mo = list(data = {temp <- isim_time$index.usemo[st_NRCS[["i_mo_used"]]]
                  soiltemp.mo.all$val[temp, , drop = FALSE]}, nheader = 2),
                dy = list(data = {temp <- isim_time$index.usedy[st_NRCS[["i_dy_used"]]]
                  soiltemp.dy.all$val[temp, , drop = FALSE]}, nheader = 2)
              )
              vwc_dy_nrsc <- vwcmatric.dy.all

              if (opt_agg[["NRCS_SMTRs"]][["aggregate_at"]] == "data") {
                # Aggregate SOILWAT2 output to mean conditions before determining conditions and regimes
                soiltemp_nrsc <- list(
                  yr = list(data = matrix(colMeans(soiltemp_nrsc[["yr"]][["data"]]), nrow = 1),
                    nheader = soiltemp_nrsc[["yr"]][["nheader"]]),
                  mo = list(data = stats::aggregate(soiltemp_nrsc[["mo"]][["data"]],
                    by = list(simTime2$month_ForEachUsedMonth[st_NRCS[["i_mo_used"]]]), mean)[, -1],
                    nheader = soiltemp_nrsc[["mo"]][["nheader"]]),
                  dy = list(data = stats::aggregate(soiltemp_nrsc[["dy"]][["data"]],
                    by = list(simTime2$doy_ForEachUsedDay[st_NRCS[["i_dy_used"]]]), mean)[, -1],
                    nheader = soiltemp_nrsc[["dy"]][["nheader"]])
                )
                vwc_dy_nrsc <- lapply(vwcmatric.dy.all, function(x)
                  stats::aggregate(as.matrix(x)[isim_time$index.usedy[st_NRCS[["i_dy_used"]]], ],
                    list(simTime2$doy_ForEachUsedDay[st_NRCS[["i_dy_used"]]]), mean)[, -1])

                temp <- dim(vwc_dy_nrsc$val)[1]
                st_NRCS <- c(st_NRCS, list(
                    index_usedy = seq_len(temp),
                    month_ForMonth = SFSW2_glovars[["st_mo"]],
                    yearno_ForMonth = rep(1, 12),
                    doy_ForDay = seq_len(temp)
                  ))
                # adjust st_NRCS for the aggregation
                st_NRCS <- utils::modifyList(st_NRCS, list(
                  yr_used = 1,
                  N_yr_used = 1,
                  i_yr_used = 1,
                  i_mo_used = SFSW2_glovars[["st_mo"]],
                  i_dy_used = rep(TRUE, temp),
                  N_dy_used = temp,
                  days_per_yr_used = temp))

                wateryears <- rep(1, temp)
                wyears <- 1

              } else {
                # Determine regimes based on time-series output and then determine conditions and regime
                st_NRCS <- c(st_NRCS, list(
                    index_usedy = isim_time$index.usedy[st_NRCS[["i_dy_used"]]],
                    month_ForMonth = simTime2$month_ForEachUsedMonth_NSadj[st_NRCS[["i_mo_used"]]],
                    yearno_ForMonth = simTime2$yearno_ForEachUsedMonth_NSadj[st_NRCS[["i_mo_used"]]],
                    doy_ForDay = simTime2$doy_ForEachUsedDay_NSadj[st_NRCS[["i_dy_used"]]]
                  ))
              }

              #Required soil layers
              soildat <- rSOILWAT2::swSoils_Layers(swRunScenariosData[[sc]])[, c("depth_cm", "sand_frac", "clay_frac", "impermeability_frac"), drop = FALSE]
              #TODO: adjust this once TOC is incorporated into rSOILWAT2
              soildat <- cbind(soildat, soil_TOC)
              #50cm soil depth or impermeable layer (whichever is shallower; Soil Survey Staff 2014: p.31)
              imp_depth <- which(soildat[, "impermeability_frac"] >= opt_agg[["NRCS_SMTRs"]][["impermeability"]])
              imp_depth <- min(imp_depth, max(soildat[, "depth_cm"]))  #Interpret maximum soil depth as possible impermeable layer
              Fifty_depth <- min(50, imp_depth)

              #Definition of MCS (Soil Survey Staff 2014: p.29): The moisture control section (MCS) of a soil: the depth to which a dry (tension of more than 1500 kPa, but not air-dry) soil will be moistened by 2.5 cm of water within 24 hours. The lower boundary is the depth to which a dry soil will be moistened by 7.5 cm of water within 48 hours.
              sand_temp <- stats::weighted.mean(sand, layers_width)
              clay_temp <- stats::weighted.mean(clay, layers_width)
              #Practical depth definition of MCS
              #  - 10 to 30 cm below the soil surface if the particle-size class of the soil is fine-loamy, coarse-silty, fine-silty, or clayey
              #  - 20 to 60 cm if the particle-size class is coarse-loamy
              #  - 30 to 90 cm if the particle-size class is sandy.
              MCS_depth <- if (clay_temp >= 0.18) { c(10, 30)
                } else if (sand_temp < 0.15) { c(10, 30)
                } else if (sand_temp >= 0.50) { c(30, 90)
                } else c(20, 60)
              #If 7.5 cm of water moistens the soil to a densic, lithic, paralithic, or petroferric contact or to a petrocalcic or petrogypsic horizon or a duripan, the contact or the upper boundary of the cemented horizon constitutes the lower boundary of the soil moisture control section. If a soil is moistened to one of these contacts or horizons by 2.5 cm of water, the soil moisture control section is the boundary of the contact itself. The control section of such a soil is considered moist if the contact or upper boundary of the cemented horizon has a thin film of water. If that upper boundary is dry, the control section is considered dry.

              MCS_depth <- adjustLayer_byImp(depths = MCS_depth, imp_depth = imp_depth,
                sdepths = soildat[, "depth_cm"])

              #Soil layer 10-70 cm used for anhydrous layer definition; adjusted for impermeable layer
              Lanh_depth <- adjustLayer_byImp(depths = c(10, 70), imp_depth = imp_depth,
                sdepths = soildat[, "depth_cm"])

              #Permafrost (Soil Survey Staff 2014: p.28) is defined as a thermal condition in which a material (including soil material) remains below 0 C for 2 or more years in succession
              permafrost_yrs <- max(apply(soiltemp.yr.all$val[isim_time$index.useyr, -1, drop = FALSE], 2, function(x) {
                temp <- rle(x < 0)
                if (any(temp$values)) max(temp$lengths[temp$values]) else 0L
              }))

              has_notenough_normalyears <- FALSE
              if (SMR_normalyears_N > 0) {
                temp_annual <- temp_annual[st_NRCS[["i_yr_used"]], , drop = FALSE]

                #Set soil depths and intervals accounting for shallow soil profiles: Soil Survey Staff 2014: p.31)
                ##Calculate soil temperature at necessary depths using a weighted mean
                i_depth50 <- findInterval(Fifty_depth, soildat[, "depth_cm"])
                calc50 <- !(Fifty_depth == soildat[i_depth50, "depth_cm"])
                if (calc50) {
                  weights50 <- calc_weights_from_depths(i_depth50, Fifty_depth, soildat[, "depth_cm"])
                  soildat <- t(add_layer_to_soil(t(soildat), i_depth50, weights50))
                  i_depth50 <- findInterval(Fifty_depth, soildat[, "depth_cm"])

                  soiltemp_nrsc <- lapply(soiltemp_nrsc, function(st)
                    list(data = add_layer_to_soil(st[["data"]], st[["nheader"]] + i_depth50, weights50),
                         nheader = st[["nheader"]]))
                  vwc_dy_nrsc$val <- add_layer_to_soil(vwc_dy_nrsc$val, 2 + i_depth50, weights50)
                  rm(weights50)
                }

                i_MCS <- findInterval(MCS_depth, soildat[, "depth_cm"])
                calcMCS <- !(MCS_depth == soildat[i_MCS, "depth_cm"])
                if (any(calcMCS)) for (k in which(calcMCS)) {
                  weightsMCS <- calc_weights_from_depths(i_MCS[k], MCS_depth[k], soildat[, "depth_cm"])
                  soildat <- t(add_layer_to_soil(t(soildat), i_MCS[k], weightsMCS))
                  i_MCS <- findInterval(MCS_depth, soildat[, "depth_cm"])

                  soiltemp_nrsc <- lapply(soiltemp_nrsc, function(st)
                    list(data = add_layer_to_soil(st[["data"]], st[["nheader"]] + i_MCS[k], weightsMCS),
                         nheader = st[["nheader"]]))
                  vwc_dy_nrsc$val <- add_layer_to_soil(vwc_dy_nrsc$val, 2 + i_MCS[k], weightsMCS)
                  rm(weightsMCS)
                }

                i_Lanh <- findInterval(Lanh_depth, soildat[, "depth_cm"])
                calcLanh <- !(Lanh_depth == soildat[i_Lanh, "depth_cm"])
                if (any(calcLanh)) for (k in which(calcLanh)) {
                  weightsLanh <- calc_weights_from_depths(i_Lanh[k], Lanh_depth[k], soildat[, "depth_cm"])
                  soildat <- t(add_layer_to_soil(t(soildat), i_Lanh[k], weightsLanh))
                  i_Lanh <- findInterval(Lanh_depth, soildat[, "depth_cm"])

                  soiltemp_nrsc <- lapply(soiltemp_nrsc, function(st)
                    list(data = add_layer_to_soil(st[["data"]], st[["nheader"]] + i_Lanh[k], weightsLanh),
                         nheader = st[["nheader"]]))
                  vwc_dy_nrsc$val <- add_layer_to_soil(vwc_dy_nrsc$val, 2 + i_Lanh[k], weightsLanh)
                  rm(weightsLanh)
                }

                soiltemp_nrsc <- lapply(soiltemp_nrsc, function(st) st[["data"]])

                swp_recalculate <- calc50 || any(calcMCS) || any(calcLanh)
                if (swp_recalculate) {
                  soilLayers_N_NRCS <- dim(soildat)[1]

                  if (opt_verbosity[["verbose"]])
                    print(paste0(tag_simpidfid, ": interpolated soil layers for NRCS soil ",
                      "regimes because of insufficient soil layers: required would be {",
                      paste(sort(unique(c(Fifty_depth, MCS_depth, Lanh_depth))),
                      collapse = ", "), "} and available are {",
                      paste(layers_depth, collapse = ", "), "}"))
                } else {
                  soilLayers_N_NRCS <- soilLayers_N
                }

                swp_dy_nrsc <- if (swp_recalculate || opt_agg[["NRCS_SMTRs"]][["aggregate_at"]] == "data") {
                    get_SWPmatric_aggL(vwc_dy_nrsc, texture = texture,
                      sand = soildat[, "sand_frac"], clay = soildat[, "clay_frac"])
                  } else {
                    swpmatric.dy.all
                  }
                swp_dy_nrsc <- swp_dy_nrsc$val[st_NRCS[["index_usedy"]], -(1:2), drop = FALSE]

                #MCS (Soil Survey Staff 2014: p.29)
                #What soil layer info used for MCS
                i_MCS <- identify_soillayers(MCS_depth, soildat[, "depth_cm"])
                #Repeat for Anhydrous soil layer moisture delineation
                i_Lanh <- identify_soillayers(Lanh_depth, soildat[, "depth_cm"])

                #mean soil temperature in Lahn depths (10 - 70 cm)
                temp_annual[, "MATLanh"] <- apply(soiltemp_nrsc[["yr"]][, 1 + i_Lanh, drop = FALSE], 1,
                  stats::weighted.mean, w = soildat[i_Lanh, "depth_cm"])

                #---Calculate variables
                crit_agree <- opt_agg[["NRCS_SMTRs"]][["crit_agree_frac"]] * st_NRCS[["N_yr_used"]]

                #mean soil temperatures at 50cm depth
                temp_annual[, "MAT50"] <- soiltemp_nrsc[["yr"]][, 1 + i_depth50]
                temp <- soiltemp_nrsc[["mo"]][, 2 + i_depth50][st_NRCS[["month_ForMonth"]] %in% 6:8]
                temp_annual[, "T50jja"] <- apply(matrix(temp, ncol = st_NRCS[["N_yr_used"]]), 2, mean)
                temp <- soiltemp_nrsc[["mo"]][, 2 + i_depth50][st_NRCS[["month_ForMonth"]] %in% c(12, 1:2)]
                temp_annual[, "T50djf"] <- apply(matrix(temp, ncol = st_NRCS[["N_yr_used"]]), 2, mean)
                T50 <- soiltemp_nrsc[["dy"]][, 2 + i_depth50]
                # offset between soil and air temperature
                fc <- temp.mo$mean[st_NRCS[["i_mo_used"]]] - soiltemp_nrsc[["mo"]][, 2 + i_depth50]
                temp_annual[, "meanTair_Tsoil50_offset_C"] <- tapply(fc,
                  st_NRCS[["yearno_ForMonth"]], mean)

                #CSPartSummer: Is the soil saturated with water during some part of the summer June1 ( = regular doy 244) - Aug31 ( = regular doy 335)
                isummer <- st_NRCS[["doy_ForDay"]] >= 244 & st_NRCS[["doy_ForDay"]] <= 335
                temp_annual[, "CSPartSummer"] <- vapply(st_NRCS[["yr_used"]], function(yr) {
                  temp <- apply(swp_dy_nrsc[wateryears[st_NRCS[["i_dy_used"]]] == yr & isummer, , drop = FALSE], 1,
                    function(x) all(x >= opt_agg[["NRCS_SMTRs"]][["SWP_sat"]]))
                  rtemp <- rle(temp)
                  if (any(rtemp$values)) max(rtemp$lengths[rtemp$values]) else 0
                }, FUN.VALUE = NA_real_)

                # "saturated with water for X cumulative days in normal years"
                days_saturated_layers <- vapply(st_NRCS[["yr_used"]], function(yr) {
                  apply(swp_dy_nrsc[wateryears[st_NRCS[["i_dy_used"]]] == yr, , drop = FALSE], 2,
                    function(x) sum(x >= opt_agg[["NRCS_SMTRs"]][["SWP_sat"]]))
                }, FUN.VALUE = rep(NA_real_, soilLayers_N_NRCS))
                if (!is.matrix(days_saturated_layers)) {
                  days_saturated_layers <- matrix(days_saturated_layers,
                    nrow = soilLayers_N_NRCS, ncol = st_NRCS[["N_yr_used"]])
                }

                somCOND0 <- t(days_saturated_layers) >= 30
                #if (opt_agg[["NRCS_SMTRs"]][["aggregate_at"]] == "conditions") {
                  somCOND0 <- matrix(colSums(somCOND0), nrow = 1, ncol = soilLayers_N_NRCS) >=
                    crit_agree
                #}

                # Organic versus mineral soil material per layer
                organic_carbon_wfraction <- soildat[, "soil_TOC"] / 1000 # units(TOC) = g C / kg soil

                is_mineral_layer <- (!somCOND0 & organic_carbon_wfraction < 0.2) |
                  (somCOND0 &
                  (soildat[, "clay_frac"] >= 0.6 & organic_carbon_wfraction < 0.18) |
                  (organic_carbon_wfraction < 0.12 + 0.1 * soildat[, "clay_frac"]))

                # determine presence of O horizon
                # TODO: guess (critical levels 'crit_Oh' are made up and not based on data):
                #       O-horizon if 50% trees or 75% shrubs or lots of litter
                crit_Oh <- c(0.5, 0.75, 0.8)
                veg_comp <- rSOILWAT2::swProd_Composition(swRunScenariosData[[sc]])[1:4]

                temp <- cbind(rSOILWAT2::swProd_MonProd_grass(swRunScenariosData[[sc]])[, "Litter"],
                  rSOILWAT2::swProd_MonProd_shrub(swRunScenariosData[[sc]])[, "Litter"],
                  rSOILWAT2::swProd_MonProd_tree(swRunScenariosData[[sc]])[, "Litter"],
                  rSOILWAT2::swProd_MonProd_forb(swRunScenariosData[[sc]])[, "Litter"])

                veg_litter <- mean(apply(sweep(temp, 2, veg_comp, "*"), 1, sum))
                crit_litter <- crit_Oh[3] *
                  sum(rSOILWAT2::swProd_Es_param_limit(swRunScenariosData[[sc]]) * veg_comp)

                has_Ohorizon <- (veg_litter >= crit_litter) &&
                  if (!is.finite(is_mineral_layer[1])) {
                    veg_comp["Trees"] > crit_Oh[1] || veg_comp["Shrubs"] > crit_Oh[2]
                  } else {
                    !is_mineral_layer[1]
                  }

                #---Soil temperature regime: based on Soil Survey Staff 2014 (Key to Soil Taxonomy): p.31
                #we ignore distinction between iso- and not iso-
                icol <- c("MAT50", "T50jja", "CSPartSummer")
                stCONDs <- temp_annual[, icol, drop = FALSE]
                if (opt_agg[["NRCS_SMTRs"]][["aggregate_at"]] == "conditions") {
                  temp <- colMeans(stCONDs)
                  temp["CSPartSummer"] <- temp["CSPartSummer"] >
                    opt_agg[["NRCS_SMTRs"]][["crit_agree_frac"]]
                  stCONDs <- matrix(temp, nrow = 1, ncol = length(icol),
                    dimnames = list(NULL, icol))
                }
                has_permafrost <- permafrost_yrs >= 2

                SMTR[["STR"]] <- t(apply(stCONDs, 1, function(x)
                  STR_logic(MAST = x["MAT50"], MSST = x["T50jja"],
                    SatSoilSummer_days = x["CSPartSummer"],
                    has_permafrost = has_permafrost, has_Ohorizon = has_Ohorizon)))


                if (SMR_normalyears_N > 2) {
                  #Structures used Lanh delineation
                  #Days are moists in half of the Lanh soil depth (and not soil layers!)
                  n_Lanh <- length(i_Lanh)
                  width_Lanh <- diff(c(0, soildat[, "depth_cm"]))[i_Lanh] # stopifnot(sum(width_Lanh) == Lanh_depth[2] - Lanh_depth[1])
                  temp <- swp_dy_nrsc[, i_Lanh, drop = FALSE] > opt_agg[["NRCS_SMTRs"]][["SWP_dry"]]
                  temp <- temp * matrix(width_Lanh, nrow = st_NRCS[["N_dy_used"]], ncol = length(i_Lanh), byrow = TRUE)
                  Lanh_Dry_Half <- .rowSums(temp, m = st_NRCS[["N_dy_used"]], n = n_Lanh) <= sum(width_Lanh) / 2

                  #Conditions for Anhydrous soil delineation
                  ACS_CondsDF_day <- data.frame(
                    Years = rep(st_NRCS[["yr_used"]], st_NRCS[["days_per_yr_used"]]),
                    T50_at0C = T50 > 0, # days where T @ 50 is > 0 C
                    Lanh_Dry_Half = Lanh_Dry_Half
                  )
                  ACS_CondsDF_yrs <- data.frame(
                    Years = st_NRCS[["yr_used"]],
                    MAT50 = temp_annual[, "MAT50"],
                    MATLanh = temp_annual[, "MATLanh"]
                  )

                  #Mean Annual soil temperature is less than or equal to 0C
                  ACS_CondsDF_yrs$COND1 <- ACS_CondsDF_yrs$MAT50 <= 0
                  #Soil temperature in the Lahn Depth is never greater than 5
                  ACS_CondsDF_day$COND2_Test <- apply(soiltemp_nrsc[["dy"]][, 1 + i_Lanh, drop = FALSE],
                    1, function(st) all(st < 5))
                  ACS_CondsDF_yrs$COND2 <- with(ACS_CondsDF_day,
                    tapply(COND2_Test, Years, all))
                  #In the Lahn Depth, 1/2 of soil dry > 1/2 CUMULATIVE days when Mean Annual ST > 0C
                  ACS_CondsDF_day$COND3_Test <- with(ACS_CondsDF_day,
                    Lanh_Dry_Half == T50_at0C) #TRUE = where are both these conditions met
                  ACS_CondsDF_yrs$HalfDryDaysCumAbove0C <- with(ACS_CondsDF_day,
                    tapply(COND3_Test, Years, sum))
                  ACS_CondsDF_yrs$SoilAbove0C <- with(ACS_CondsDF_day,
                    tapply(T50_at0C, Years, sum))
                  ACS_CondsDF_yrs$COND3 <- with(ACS_CondsDF_yrs,
                    HalfDryDaysCumAbove0C > .5 * SoilAbove0C) #TRUE = Half of soil layers are dry greater than half the days where MAST >0c

                  icol <- c('COND1', 'COND2', 'COND3')
                  icol_new <- paste0("ACS_", icol)
                  ACS_CondsDF3 <- as.matrix(ACS_CondsDF_yrs[, icol, drop = FALSE])
                  if (opt_agg[["NRCS_SMTRs"]][["aggregate_at"]] == "conditions") {
                    temp <- matrix(colSums(ACS_CondsDF3, na.rm = TRUE), nrow = 1,
                      ncol = length(icol), dimnames = list(NULL, icol_new))
                    ACS_CondsDF3 <- temp >= crit_agree
                  } else {
                    dimnames(ACS_CondsDF3)[[2]] <- icol_new
                  }

                  #Structures used for MCS delineation
                  MCS_CondsDF_day <- data.frame(
                    Years = rep(st_NRCS[["yr_used"]], st_NRCS[["days_per_yr_used"]]),
                    DOY = st_NRCS[["doy_ForDay"]],
                    T50_at5C = T50 > 5, # days where T @ 50cm exceeds 5C
                    T50_at8C = T50 > 8, # days where T @ 50cm exceeds 8C
                    MCS_Moist_All = apply(swp_dy_nrsc[, i_MCS, drop = FALSE] > opt_agg[["NRCS_SMTRs"]][["SWP_dry"]], 1, all),
                    MCS_Dry_All = apply(swp_dy_nrsc[, i_MCS, drop = FALSE] < opt_agg[["NRCS_SMTRs"]][["SWP_dry"]], 1, all)
                  )
                  MCS_CondsDF_yrs <- data.frame(
                    Years = st_NRCS[["yr_used"]],
                    MAT50 = temp_annual[, "MAT50"],
                    T50jja = temp_annual[, "T50jja"],
                    T50djf = temp_annual[, "T50djf"]
                  )

                  #COND0 - monthly PET < PPT
                  MCS_CondsDF_yrs$COND0 <- if (opt_agg[["NRCS_SMTRs"]][["aggregate_at"]] == "data") {
                      all(tapply(prcp.mo$ppt - pet.mo$val,
                        simTime2$month_ForEachUsedMonth, mean) > 0)
                    } else {
                      tapply(prcp.mo$ppt > pet.mo$val,
                        simTime2$yearno_ForEachUsedMonth, all)[st_NRCS[["i_yr_used"]]]
                    }

                  #COND1 - Dry in ALL parts for more than half of the CUMULATIVE days per year when the soil temperature at a depth of 50cm is above 5C
                  MCS_CondsDF_day$COND1_Test <- with(MCS_CondsDF_day,
                    MCS_Dry_All & T50_at5C)  #TRUE = where are both these conditions met
                  MCS_CondsDF_yrs$DryDaysCumAbove5C <- with(MCS_CondsDF_day,
                    tapply(COND1_Test, Years, sum))
                  MCS_CondsDF_yrs$SoilAbove5C <- with(MCS_CondsDF_day,
                    tapply(T50_at5C, Years, sum))
                  MCS_CondsDF_yrs$COND1 <- with(MCS_CondsDF_yrs,
                    DryDaysCumAbove5C > .5 * SoilAbove5C) #TRUE =Soils are dry greater than 1/2 cumulative days/year

                  #Cond2 - Moist in SOME or all parts for less than 90 CONSECUTIVE days when the the soil temperature at a depth of 50cm is above 8C
                  MCS_CondsDF_day$COND2_Test <- with(MCS_CondsDF_day,
                    !MCS_Dry_All & T50_at8C)  #TRUE = where are both these conditions met
                  MCS_CondsDF_yrs$MaxContDaysAnyMoistCumAbove8 <- with(MCS_CondsDF_day,
                    tapply(COND2_Test, Years, max_duration)) # Maximum consecutive days
                  MCS_CondsDF_yrs$COND2 <- MCS_CondsDF_yrs$MaxContDaysAnyMoistCumAbove8 < 90 # TRUE = moist less than 90 consecutive days during >8 C soils, FALSE = moist more than 90 consecutive days
                  MCS_CondsDF_yrs$COND2_1 <- MCS_CondsDF_yrs$MaxContDaysAnyMoistCumAbove8 < 180
                  MCS_CondsDF_yrs$COND2_2 <- MCS_CondsDF_yrs$MaxContDaysAnyMoistCumAbove8 < 270
                  MCS_CondsDF_yrs$COND2_3 <- MCS_CondsDF_yrs$MaxContDaysAnyMoistCumAbove8 <= 45

                  #COND3 - MCS is Not dry in ANY part as long as 90 CUMULATIVE days - Can't be dry longer than 90 cum days
                  MCS_CondsDF_yrs$DryDaysCumAny <- with(MCS_CondsDF_day,
                    tapply(!MCS_Moist_All, Years, sum)) #Number of days where any soils are dry
                  MCS_CondsDF_yrs$COND3 <- MCS_CondsDF_yrs$DryDaysCumAny < 90 #TRUE = Not Dry for as long 90 cumlative days, FALSE = Dry as long as as 90 Cumlative days
                  MCS_CondsDF_yrs$COND3_1 <- MCS_CondsDF_yrs$DryDaysCumAny < 30

                  #COND4 - The means annual soil temperature at 50cm is < or > 22C
                  MCS_CondsDF_yrs$COND4 <- MCS_CondsDF_yrs$MAT50 >= 22 #TRUE - Greater than 22, False - Less than 22

                  #COND5 - The absolute difference between the temperature in winter @ 50cm and the temperature in summer @ 50cm is > or < 6
                  MCS_CondsDF_yrs$AbsDiffSoilTemp_DJFvsJJA <- with(MCS_CondsDF_yrs,
                    abs(T50djf - T50jja))
                  MCS_CondsDF_yrs$COND5 <- MCS_CondsDF_yrs$AbsDiffSoilTemp_DJFvsJJA >= 6 #TRUE - Greater than 6, FALSE - Less than 6

                  #COND6 - Dry in ALL parts LESS than 45 CONSECUTIVE days in the 4 months following the summer solstice
                  temp <- with(MCS_CondsDF_day[MCS_CondsDF_day$DOY %in% c(172:293), ],
                    tapply(MCS_Dry_All, Years, max_duration))  #Consecutive days of dry soil after summer solsitice
                  ids <- match( MCS_CondsDF_yrs[, "Years"], as.integer(names(temp)),
                    nomatch = 0)
                  MCS_CondsDF_yrs[ids > 0, "DryDaysConsecSummer"] <- temp[ids]
                  MCS_CondsDF_yrs$COND6 <- MCS_CondsDF_yrs$DryDaysConsecSummer < 45 # TRUE = dry less than 45 consecutive days
                  MCS_CondsDF_yrs$COND6_1 <- MCS_CondsDF_yrs$DryDaysConsecSummer > 90

                  #COND7 - MCS is MOIST in SOME parts for more than 180 CUMULATIVE days
                  MCS_CondsDF_yrs$MoistDaysCumAny <- with(MCS_CondsDF_day,
                    tapply(!MCS_Dry_All, Years, sum))#Number of days where any soils are moist
                  MCS_CondsDF_yrs$COND7 <- MCS_CondsDF_yrs$MoistDaysCumAny > 180 #TRUE = Not Dry or Moist for as long 180 cumlative days

                  #Cond8 - MCS is MOIST in SOME parts for more than 90 CONSECUTIVE days
                  MCS_CondsDF_yrs$MoistDaysConsecAny <- with(MCS_CondsDF_day, tapply(!MCS_Dry_All, Years, max_duration)) #Consecutive days of Moist soil
                  MCS_CondsDF_yrs$COND8 <- MCS_CondsDF_yrs$MoistDaysConsecAny > 90 # TRUE = Moist more than 90 Consecutive Days

                  #COND9 - Moist in ALL parts MORE than 45 CONSECUTIVE days in the 4 months following the winter solstice
                  temp <- with(MCS_CondsDF_day[MCS_CondsDF_day$DOY %in% c(355:365, 1:111), ],
                    tapply(MCS_Moist_All, Years, max_duration))#Consecutive days of moist soil after winter solsitice
                  ids <- match( MCS_CondsDF_yrs[, "Years"], as.integer(names(temp)),
                    nomatch = 0)
                  MCS_CondsDF_yrs[ids > 0, "MoistDaysConsecWinter"] <- temp[ids]
                  MCS_CondsDF_yrs$COND9 <- MCS_CondsDF_yrs$MoistDaysConsecWinter > 45 # TRUE = moist more than 45 consecutive days

                  #COND10 - MCS is Dry in ALL layers for more or equal to 360 days
                  MCS_CondsDF_yrs$AllDryDaysCumAny <- with(MCS_CondsDF_day,
                    tapply(MCS_Dry_All, Years, sum)) #Number of days where all soils are dry
                  MCS_CondsDF_yrs$COND10 <- MCS_CondsDF_yrs$AllDryDaysCumAny >= 360

                  icol <- c('COND0', 'COND1', 'COND2', 'COND2_1', 'COND2_2', 'COND2_3',
                    'COND3', 'COND3_1', 'COND4', 'COND5', 'COND6', 'COND6_1', 'COND7',
                    'COND8', 'COND9', 'COND10')
                  icol_new <- paste0("MCS_", icol)
                  MCS_CondsDF3 <- as.matrix(MCS_CondsDF_yrs[, icol, drop = FALSE])
                  if (opt_agg[["NRCS_SMTRs"]][["aggregate_at"]] == "conditions") {
                    temp <- matrix(colSums(MCS_CondsDF3, na.rm = TRUE),
                      nrow = 1, ncol = length(icol), dimnames = list(NULL, icol_new))
                    MCS_CondsDF3 <- temp >= crit_agree
                  } else {
                    dimnames(MCS_CondsDF3)[[2]] <- icol_new
                  }


                  #---Soil moisture regime: Soil Survey Staff 2014 (Key to Soil Taxonomy): p.28-31
                  SMTR[["SMR"]] <- t(apply(cbind(ACS_CondsDF3, MCS_CondsDF3), 1,
                    function(x) do.call(SMR_logic, args = c(as.list(x),
                    list(has_permafrost = has_permafrost)))))

                  temp_annual[, 7:14] <- as.matrix(cbind(ACS_CondsDF_yrs
                    [, c("COND1", "COND2", "COND3", "HalfDryDaysCumAbove0C", "SoilAbove0C")],
                    stats::aggregate(ACS_CondsDF_day[, c('T50_at0C', 'Lanh_Dry_Half', 'COND3_Test')],
                      by = list(ACS_CondsDF_day$Years), mean)[, -1]))

                  dtemp <- stats::aggregate(MCS_CondsDF_day[, c("T50_at5C", "T50_at8C",
                      "MCS_Moist_All", "COND1_Test", "COND2_Test")],
                    by = list(MCS_CondsDF_day$Years), mean)[, -1]
                  icols_conds <- c("COND0",
                      "DryDaysCumAbove5C", "SoilAbove5C", "COND1",
                      "MaxContDaysAnyMoistCumAbove8", "COND2", "COND2_1", "COND2_2", "COND2_3",
                      "DryDaysCumAny", "COND3", "COND3_1",
                      "COND4",
                      "AbsDiffSoilTemp_DJFvsJJA", "COND5",
                      "DryDaysConsecSummer", "COND6", "COND6_1",
                      "MoistDaysCumAny", "COND7",
                      "MoistDaysConsecAny", "COND8",
                      "MoistDaysConsecWinter", "COND9",
                      "AllDryDaysCumAny", "COND10")
                  temp_annual[, 15:45] <- as.matrix(cbind(MCS_CondsDF_yrs[, icols_conds],
                    dtemp))

                  regimes_done <- TRUE

                  to_del <- c("n_Lanh", "width_Lanh", "Lanh_Dry_Half", "ACS_CondsDF_day",
                    "ACS_CondsDF_yrs", "ACS_CondsDF3", "MCS_CondsDF_day", "MCS_CondsDF_yrs",
                    "MCS_CondsDF3")
                  #to_del <- to_del[to_del %in% ls()]
                  if (length(to_del) > 0)
                    try(rm(list = to_del), silent = TRUE)

                } else {
                  has_notenough_normalyears <- TRUE
                  SMTR[["SMR"]][] <- NA
                }

                to_del <- c("calc50", "calcLanh", "calcMCS", "clay_temp",
                  "i_depth50", "i_Lanh", "i_MCS", "imp_depth", "isummer",
                  "sand_temp", "soildat", "soiltemp_nrsc", "st_NRCS", "swp_dy_nrsc",
                  "vwc_dy_nrsc", "wateryears", "wyears")
                #to_del <- to_del[to_del %in% ls()]
                if (length(to_del) > 0) {
                  try(rm(list = to_del), silent = TRUE)
                }

              } else {
                SMTR[["STR"]][] <- SMTR[["SMR"]][] <- NA
                has_notenough_normalyears <- TRUE
              }

              if (has_notenough_normalyears) {
                if (opt_verbosity[["verbose"]]) {
                  print(paste0(tag_simpidfid, ": number of normal years is ",
                    SMR_normalyears_N, " which is insufficient to calculate ",
                    "NRCS soil moisture", if (SMR_normalyears_N <= 0) "/temperature",
                    " regimes."))
                }
              }

            } else {
              if (opt_verbosity[["verbose"]]) {
                print(paste0(tag_simpidfid, ": has unrealistic soil temperature values: ",
                  "NRCS soil moisture/temperature regimes not calculated."))
              }
              SMTR[["STR"]][] <- SMTR[["SMR"]][] <- NA
              has_realistic_SoilTemp <- 0
            }

          } else {
            if (opt_verbosity[["verbose"]]) {
              print(paste0(tag_simpidfid, ": soil temperature module turned off but ",
                "required for NRCS Soil Moisture/Temperature Regimes."))
            }
            SMTR[["STR"]][] <- SMTR[["SMR"]][] <- NA
            has_simulated_SoilTemp <- 0
          }

          if (isTRUE(prj_todos[["aon"]][["dailyNRCS_SoilMoistureTemperatureRegimes_Intermediates"]])) {
            nv01 <- nv

            nv_new <- nv + 10
            resMeans[nv:(nv_new - 1)] <- c(has_simulated_SoilTemp, has_realistic_SoilTemp,
              Fifty_depth, MCS_depth[1:2], Lanh_depth[1:2],
              permafrost_yrs, SMR_normalyears_N, as.integer(has_Ohorizon))
            nv <- nv_new
            nv_new <- nv + dim(temp_annual)[2]
            resMeans[nv:(nv_new - 1)] <- t(apply(temp_annual, 2, mean, na.rm = TRUE))
            resSDs[nv:(nv_new - 1)] <- t(apply(temp_annual, 2, stats::sd, na.rm = TRUE))
            nv <- nv_new

            print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv01, "dailyNRCS_SoilMoistureTemperatureRegimes_Intermediates")
          }

          Tregime <- colMeans(SMTR[["STR"]])
          Sregime <- colMeans(SMTR[["SMR"]])

          if (isTRUE(prj_todos[["aon"]][["dailyNRCS_SoilMoistureTemperatureRegimes"]])) {
            nv02 <- nv

            nv_new <- nv + length(Tregime)
            resMeans[nv:(nv_new - 1)] <- Tregime
            nv <- nv_new
            nv_new <- nv + length(Sregime)
            resMeans[nv:(nv_new - 1)] <- Sregime
            nv <- nv_new

            print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv02, "dailyNRCS_SoilMoistureTemperatureRegimes")
          }

          Tregime <- Tregime >= opt_agg[["NRCS_SMTRs"]][["crit_agree_frac"]]
          Sregime <- Sregime >= opt_agg[["NRCS_SMTRs"]][["crit_agree_frac"]]

          to_del <- c("MCS_depth", "Lanh_depth", "Fifty_depth", "permafrost_yrs",
            "SMTR", "SMR_normalyears_N", "temp_annual")
          #to_del <- to_del[to_del %in% ls()]
          if (length(to_del) > 0)
            try(rm(list = to_del), silent = TRUE)
        }

        #35b #Based on Table 1 in Chambers, J. C., D. A. Pyke, J. D. Maestas, M. Pellant, C. S. Boyd, S. B. Campbell, S. Espinosa, D. W. Havlina, K. E. Mayer, and A. Wuenschel. 2014. Using Resistance and Resilience Concepts to Reduce Impacts of Invasive Annual Grasses and Altered Fire Regimes on the Sagebrush Ecosystem and Greater Sage-Grouse: A Strategic Multi-Scale Approach. Gen. Tech. Rep. RMRS-GTR-326. U.S. Department of Agriculture, Forest Service, Rocky Mountain Research Station, Fort Collins, CO.
        if (isTRUE(prj_todos[["aon"]][["dailyNRCS_Chambers2014_ResilienceResistance"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "dailyNRCS_Chambers2014_ResilienceResistance")
          if (!exists("prcp.yr")) prcp.yr <- get_PPT_yr(runDataSC, isim_time)

          #Result containers
          cats <- c("Low", "ModeratelyLow", "Moderate", "ModeratelyHigh", "High")
          resilience <- resistance <- rep(0, times = length(cats))
          names(resilience) <- names(resistance) <- cats

          if (regimes_done && (isTRUE(prj_todos[["aon"]][["dailyNRCS_SoilMoistureTemperatureRegimes"]]) ||
            isTRUE(prj_todos[["aon"]][["dailyNRCS_SoilMoistureTemperatureRegimes_Intermediates"]])) &&
            any(!is.na(Tregime)) && any(!is.na(Sregime))) {
            #---Table 1 in Chambers et al. 2014
            rows_resilience <- c("ModeratelyHigh", "ModeratelyHigh", "Moderate", "Low",
              "Low")
            rows_resistance <- c("High", "Moderate", "ModeratelyLow", "Moderate", "Low")
            #Ecological type
            Table1_EcologicalType <- matrix(c("Cryic", "Xeric", "Frigid", "Xeric",
              "Mesic", "Xeric", "Frigid", "Aridic", "Mesic", "Aridic"),
              ncol = 2, byrow = TRUE)
            Type <- as.logical(Tregime[Table1_EcologicalType[, 1]]) &
              as.logical(Sregime[Table1_EcologicalType[, 2]])

            #Characteristics
            MAP <- mean(prcp.yr$ppt)
            Table1_Characteristics_mm <- matrix(c(14, Inf, 12, 22, 12, 16, 6, 12, 8, 12),
              ncol = 2, byrow = TRUE) * 2.54 * 10
            Characteristics <- MAP >= Table1_Characteristics_mm[, 1] &
              MAP <= Table1_Characteristics_mm[, 2]

            #Resilience and Resistance
            is_notRR <- which(!is.na(Type) & !Type & Characteristics)
            for (ir in is_notRR) {
              resilience[rows_resilience[ir]] <- 0
              resistance[rows_resistance[ir]] <- 0
            }
            is_RR <- which(!is.na(Type) & Type & Characteristics)
            for (ir in is_RR) {
              resilience[rows_resilience[ir]] <- 1
              resistance[rows_resistance[ir]] <- 1
            }

            rm(rows_resilience, rows_resistance, Table1_EcologicalType, Type,
              MAP, Table1_Characteristics_mm, Characteristics, is_RR, is_notRR)
          } else {
            resilience <- resistance <- rep(NA, times = length(cats))
          }

          resMeans[nv:(nv+2*length(cats)-1)] <- c(resilience, resistance)
          nv <- nv + 2*length(cats)

          rm(cats, resilience, resistance)

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "dailyNRCS_Chambers2014_ResilienceResistance")
        }

        #35c   #Requires "dailyNRCS_SoilMoistureTemperatureRegimes"
        #Based on Maestas, J.D., Campbell, S.B., Chambers, J.C., Pellant, M. & Miller, R.F. (2016). Tapping Soil Survey Information for Rapid Assessment of Sagebrush Ecosystem Resilience and Resistance. Rangelands, 38, 120-128.
        if (isTRUE(prj_todos[["aon"]][["dailyNRCS_Maestas2016_ResilienceResistance"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "dailyNRCS_Maestas2016_ResilienceResistance")

          RR <- c(Low = NA, Moderate = NA, High = NA)

          if (regimes_done && (isTRUE(prj_todos[["aon"]][["dailyNRCS_SoilMoistureTemperatureRegimes"]]) ||
            isTRUE(prj_todos[["aon"]][["dailyNRCS_SoilMoistureTemperatureRegimes_Intermediates"]])) &&
            any(!is.na(Tregime)) && any(!is.na(Sregime))) {
            #---Table 1 in Maestas et al. 2016
            # assumptions
            #   - "Dry-Xeric" == "Xeric bordering on Aridic"
            #   - "Weak-Aridic" == "Aridic bordering on Xeric"
            Table1 <- matrix(c(
                "Cryic", "Typic-Xeric", "High",
                "Cryic", "Dry-Xeric", "High",
                "Frigid", "Typic-Xeric", "High",
                "Cryic", "Weak-Aridic", "High",

                "Cryic", "Typic-Aridic", "Moderate",
                "Frigid", "Dry-Xeric", "Moderate",
                "Frigid", "Typic-Aridic", "Moderate",
                "Frigid", "Weak-Aridic", "Moderate",
                "Mesic", "Typic-Xeric", "Moderate",

                "Mesic", "Dry-Xeric", "Low",
                "Mesic", "Weak-Aridic", "Low",
                "Mesic", "Typic-Aridic", "Low"),
              ncol = 3, byrow = TRUE)

            temp <- as.logical(Tregime[Table1[, 1]]) & as.logical(Sregime[Table1[, 2]])
            is_notRR <- !is.na(temp) & !temp
            if (any(is_notRR)) {
              RR[Table1[is_notRR, 3]] <- 0
            }
            is_RR <- !is.na(temp) & temp
            if (any(is_RR)) {
              RR[Table1[is_RR, 3]] <- 1
            }
            rm(Table1, is_RR, is_notRR)
          }

          nv_new <- nv + 3
          resMeans[nv:(nv_new - 1)] <- RR
          nv <- nv_new

          rm(RR)

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "dailyNRCS_Maestas2016_ResilienceResistance")
        }

        rm(regimes_done)
        if (isTRUE(prj_todos[["aon"]][["dailyNRCS_SoilMoistureTemperatureRegimes"]]))
          rm(Tregime, Sregime)

      #35.2   #Wet degree days on daily temp and swp
        if (isTRUE(prj_todos[["aon"]][["dailyWetDegreeDays"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "dailyWetDegreeDays")
          if (!exists("vwcmatric.dy")) vwcmatric.dy <- get_Response_aggL(swof["sw_vwcmatric"], tscale = "dy", scaler = 1, FUN = stats::weighted.mean, weights = layers_width, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)
          if (!exists("swpmatric.dy")) swpmatric.dy <- get_SWPmatric_aggL(vwcmatric.dy, texture, sand, clay)
          if (!exists("temp.dy")) temp.dy <- get_Temp_dy(runDataSC, isim_time)

          degday <- ifelse(temp.dy$mean > opt_agg[["Tbase_DD_C"]],
            temp.dy$mean - opt_agg[["Tbase_DD_C"]], 0) #degree days

          for (icrit in seq_along(opt_agg[["SWPcrit_MPa"]])) {

            wet.top <- swpmatric.dy$top >= opt_agg[["SWPcrit_MPa"]][icrit]

            if (length(bottomL) > 0 && !identical(bottomL, 0)) {
              wet.bottom <- swpmatric.dy$bottom >= opt_agg[["SWPcrit_MPa"]][icrit]
            } else {
              wet.bottom <- matrix(data = NA, nrow = length(swpmatric.dy$bottom), ncol = 1)
            }

            wetdegday.top <- ifelse(wet.top > 0, degday, 0)
            wetdegday.bottom <- ifelse(wet.bottom > 0, degday, 0)
            wetdegday.any <- ifelse(wet.top + wet.bottom > 0, degday, 0)

            temp <- lapply(list(wetdegday.top, wetdegday.bottom, wetdegday.any),
                            function(x) tapply(x, simTime2$year_ForEachUsedDay, sum))

            resMeans[(nv+3*(icrit-1)):(nv+3*(icrit-1)+2)] <- vapply(temp, mean, 1)
            resSDs[(nv+3*(icrit-1)):(nv+3*(icrit-1)+2)] <- vapply(temp, stats::sd, 1)
          }
          nv <- nv+3*opt_agg[["SWPcrit_N"]]

          rm(degday, wet.top, wet.bottom, wetdegday.top, wetdegday.bottom, wetdegday.any)

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "dailyWetDegreeDays")
        }

      #35.3
        if (isTRUE(prj_todos[["aon"]][["dailyThermalDrynessStartEnd"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "dailyThermalDrynessStartEnd")
          if (!exists("temp.dy")) temp.dy <- get_Temp_dy(runDataSC, isim_time)
          if (!exists("vwcmatric.dy")) vwcmatric.dy <- get_Response_aggL(swof["sw_vwcmatric"], tscale = "dy", scaler = 1, FUN = stats::weighted.mean, weights = layers_width, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)
          if (!exists("swpmatric.dy")) swpmatric.dy <- get_SWPmatric_aggL(vwcmatric.dy, texture, sand, clay)
          adjDays <- simTime2$doy_ForEachUsedDay_NSadj[1] - simTime2$doy_ForEachUsedDay[1]

          thermal <- temp.dy$mean > 0

          for (icrit in seq_along(opt_agg[["SWPcrit_MPa"]])) {
            thermaldry.top <- thermal & swpmatric.dy$top < opt_agg[["SWPcrit_MPa"]][icrit]
            thermaldry.bottom <- if (length(bottomL) > 0 && !identical(bottomL, 0)) {
                thermal & swpmatric.dy$bottom < opt_agg[["SWPcrit_MPa"]][icrit]
              } else {
                rep(FALSE, length(thermaldry.top))
              }

            temp <- stats::aggregate(cbind(thermaldry.top, thermaldry.bottom),
                  by = list(simTime2$year_ForEachUsedDay_NSadj),
                  FUN = function(x) max_duration(x, return_doys = TRUE))

            resMeans[nv:(nv+3)] <- c(
              apply(temp$thermaldry.top[, 2:3, drop = FALSE], 2, circ_mean, int = 365),
              apply(temp$thermaldry.bottom[, 2:3, drop = FALSE], 2, circ_mean, int = 365)) - adjDays
            resSDs[nv:(nv+3)] <- c(
              apply(temp$thermaldry.top[, 2:3, drop = FALSE], 2, circ_sd, int = 365),
              apply(temp$thermaldry.bottom[, 2:3, drop = FALSE], 2, circ_sd, int = 365))
            nv <- nv+4
          }

          rm(thermal, adjDays, thermaldry.top)
          if (length(bottomL) > 0 && !identical(bottomL, 0))
            rm(thermaldry.bottom)

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "dailyThermalDrynessStartEnd")
        }

      #35.4
        if (isTRUE(prj_todos[["aon"]][["dailyThermalSWPConditionCount"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "dailyThermalSWPConditionCount")
          if (!exists("vwcmatric.dy.all")) vwcmatric.dy.all <- get_Response_aggL(swof["sw_vwcmatric"], tscale = "dyAll", scaler = 1, FUN = stats::weighted.mean, weights = layers_width, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)
          if (!exists("swpmatric.dy.all")) swpmatric.dy.all <- get_SWPmatric_aggL(vwcmatric.dy.all, texture, sand, clay)
          if (!exists("vwcmatric.dy")) vwcmatric.dy <- get_Response_aggL(swof["sw_vwcmatric"], tscale = "dy", scaler = 1, FUN = stats::weighted.mean, weights = layers_width, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)
          if (!exists("swpmatric.dy")) swpmatric.dy <- get_SWPmatric_aggL(vwcmatric.dy, texture, sand, clay)
          if (!exists("temp.dy")) temp.dy <- get_Temp_dy(runDataSC, isim_time)

          Tcrit_N <- length(opt_agg[["Tmean_crit_C"]])

          thermal <- temp.dy$mean >
            matrix(rep.int(opt_agg[["Tmean_crit_C"]], length(temp.dy$mean)),
              ncol = Tcrit_N, byrow = TRUE)

          dryness <- matrix(rep.int(opt_agg[["SWPcrit_MPa"]], length(temp.dy$mean)),
              ncol = opt_agg[["SWPcrit_N"]], byrow = TRUE)
          n_conds <- 6L
          conds <- list() # max length(conds) == n_conds
          conds[["DryAll"]] <- apply(swpmatric.dy.all$val[isim_time$index.usedy, -(1:2), drop = FALSE], 1, max) < dryness
          conds[["WetAll"]] <- apply(swpmatric.dy.all$val[isim_time$index.usedy, -(1:2), drop = FALSE], 1, min) >= dryness
          conds[["DryTop"]] <- swpmatric.dy$top < dryness
          conds[["WetTop"]] <- !conds[["DryTop"]]
          if (length(bottomL) > 0 && !identical(bottomL, 0)) {
              conds[["DryBottom"]] <- swpmatric.dy$bottom < dryness
              conds[["WetBottom"]] <- !conds[["DryBottom"]]
          }

          day_count <- array(NA,
              dim = c(isim_time$no.useyr, Tcrit_N, opt_agg[["SWPcrit_N"]], n_conds))
          for (d2 in seq_len(Tcrit_N))
              for (d4 in seq_along(conds))
                  for (d3 in seq_along(opt_agg[["SWPcrit_MPa"]]))
                      day_count[, d2, d3, d4] <- tapply(thermal[, d2] & conds[[d4]][, d3],
                          INDEX = simTime2$year_ForEachUsedDay,
                          FUN = sum)
          nv_new <- nv + Tcrit_N * opt_agg[["SWPcrit_N"]] * n_conds
          resMeans[nv:(nv_new - 1)] <- as.vector(colMeans(day_count))
          resSDs[nv:(nv_new - 1)] <- as.vector(apply(day_count, 2:4, stats::sd))
          nv <- nv_new

          rm(thermal, dryness, conds, day_count)

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "dailyThermalSWPConditionCount")
        }

      #36 #dry periods based on monthly swp data: adjust_NorthSouth
        if (isTRUE(prj_todos[["aon"]][["monthlySWPdryness"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "monthlySWPdryness")
          if (!exists("vwcmatric.mo")) vwcmatric.mo <- get_Response_aggL(swof["sw_vwcmatric"], tscale = "mo", scaler = 1, FUN = stats::weighted.mean, weights = layers_width, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)
          if (!exists("swpmatric.mo")) swpmatric.mo <- get_SWPmatric_aggL(vwcmatric.mo, texture, sand, clay)

          adjMonths <- ifelse(simTime2$month_ForEachUsedMonth[1] == simTime2$month_ForEachUsedMonth_NSadj[1], 0, 6)

          drymonths.top <- drymonths.bottom <- array(data = 0, dim = c(opt_agg[["SWPcrit_N"]], isim_time$no.useyr, 12))
          for (icrit in seq_along(opt_agg[["SWPcrit_MPa"]])) {
            temp <- tapply(swpmatric.mo$top, simTime2$month_ForEachUsedMonth_NSadj, function(x) x <= opt_agg[["SWPcrit_MPa"]][icrit])
            drymonths.top[icrit, , ] <- matrix(unlist(temp), nrow = isim_time$no.useyr)
            temp <- tapply(swpmatric.mo$bottom, simTime2$month_ForEachUsedMonth_NSadj, function(x) x <= opt_agg[["SWPcrit_MPa"]][icrit])
            drymonths.bottom[icrit, , ] <- matrix(unlist(temp), nrow = isim_time$no.useyr)
          }

          years.top <- apply(drymonths.top, MARGIN = 1:2, FUN = sum)
          years.bottom <- apply(drymonths.bottom, MARGIN = 1:2, FUN = sum)

          resMeans[nv:(nv+2*opt_agg[["SWPcrit_N"]]-1)] <- c(apply(years.top, MARGIN = 1, FUN = mean), apply(years.bottom, MARGIN = 1, FUN = mean))
          resSDs[nv:(nv+2*opt_agg[["SWPcrit_N"]]-1)] <- c(apply(years.top, MARGIN = 1, FUN = stats::sd), apply(years.bottom, MARGIN = 1, FUN = stats::sd))

          nv <- nv+2*opt_agg[["SWPcrit_N"]]

          start.top <- apply(drymonths.top, MARGIN = 1:2, FUN = match, x = 1, nomatch = 0)
          start.top[start.top != 0] <- ifelse((temp <- (start.top[start.top != 0] + adjMonths) %% 12) == 0, 12, temp)
          start.bottom <- apply(drymonths.bottom, MARGIN = 1:2, FUN = match, x = 1, nomatch = 0)
          start.bottom[start.bottom != 0] <- ifelse((temp <- (start.bottom[start.bottom != 0] + adjMonths) %% 12) == 0, 12, temp)

          resMeans[nv:(nv+2*opt_agg[["SWPcrit_N"]]-1)] <- c(apply(start.top, MARGIN = 1, circ_mean, int = 12),
                                                         apply(start.bottom, MARGIN = 1, circ_mean, int = 12))
          resSDs[nv:(nv+2*opt_agg[["SWPcrit_N"]]-1)] <- c(apply(start.top, MARGIN = 1, circ_sd, int = 12),
                                                       apply(start.bottom, MARGIN = 1, circ_sd, int = 12))

          nv <- nv+2*opt_agg[["SWPcrit_N"]]

          rm(drymonths.top, drymonths.bottom, years.top, start.top, years.bottom, start.bottom, adjMonths)

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "monthlySWPdryness")
        }

      #37 #Dry and wet periods based on daily swp: adjust_NorthSouth
        if (isTRUE(prj_todos[["aon"]][["dailySWPdrynessANDwetness"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "dailySWPdrynessANDwetness")
          if (!exists("vwcmatric.dy.all")) vwcmatric.dy.all <- get_Response_aggL(swof["sw_vwcmatric"], tscale = "dyAll", scaler = 1, FUN = stats::weighted.mean, weights = layers_width, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)
          if (!exists("swpmatric.dy.all")) swpmatric.dy.all <- get_SWPmatric_aggL(vwcmatric.dy.all, texture, sand, clay) #swp.dy.all is required to get all layers

          adjDays <- simTime2$doy_ForEachUsedDay_NSadj[1] - simTime2$doy_ForEachUsedDay[1]
          durationDryPeriods.min <- 10 # days

          for (icrit in seq_along(opt_agg[["SWPcrit_MPa"]])) {

            wet_crit <- swpmatric.dy.all$val >= opt_agg[["SWPcrit_MPa"]][icrit]
            wet <- list()
            wet$top <- apply(wet_crit[isim_time$index.usedy, 2+topL, drop = FALSE], 1, sum)
            if (length(bottomL) > 0 && !identical(bottomL, 0)) {
              wet$bottom <- apply(wet_crit[isim_time$index.usedy, 2+bottomL, drop = FALSE], 1, sum)
            } else {
              wet$bottom <- rep(NA, isim_time$no.usedy)
            }

            AtLeastOneWet <- lapply(wet, function(x) x > 0)
            AllDry <- lapply(AtLeastOneWet, `!`)
            AllWet <- list(top = wet$top == length(topL),
                           bottom = wet$bottom == length(bottomL))
            AtLeastOneDry <- lapply(AllWet, `!`)

            #wet periods
            res.wet <- matrix(0, nrow = simTime2$no.useyr_NSadj, ncol = 8)
            res.wet[, 1] <- tapply(AtLeastOneWet$top, simTime2$year_ForEachUsedDay_NSadj, sum) # total number of days per year when at least one top layer is wet
            res.wet[, 2] <- tapply(AtLeastOneWet$bottom, simTime2$year_ForEachUsedDay_NSadj, sum) # total number of days per year when at least one top layer is wet
            res.wet[, 3] <- tapply(AtLeastOneWet$top, simTime2$year_ForEachUsedDay_NSadj, max_duration) # maximum number of continous days when at least one top layers is wet
            res.wet[, 4] <- tapply(AtLeastOneWet$bottom, simTime2$year_ForEachUsedDay_NSadj, max_duration) # maximum number of continous days when at least one top layers is wet
            res.wet[, 5] <- tapply(AllWet$top, simTime2$year_ForEachUsedDay_NSadj, sum) # total number of days per year when all top layer are wet
            res.wet[, 6] <- tapply(AllWet$bottom, simTime2$year_ForEachUsedDay_NSadj, sum) # total number of days per year when all top layer are wet
            res.wet[, 7] <- tapply(AllWet$top, simTime2$year_ForEachUsedDay_NSadj, max_duration) # maximum number of continous days when all top layers are wet
            res.wet[, 8] <- tapply(AllWet$bottom, simTime2$year_ForEachUsedDay_NSadj, max_duration) # maximum number of continous days when all top layers are wet

            #dry periods
            res.dry <- matrix(0, nrow = simTime2$no.useyr_NSadj, ncol = 8)
            res.dry[, 3] <- tapply(AllDry$top, simTime2$year_ForEachUsedDay_NSadj, sum) #total number of days/year when all top layers are dry
            res.dry[, 7] <- tapply(AllDry$bottom, simTime2$year_ForEachUsedDay_NSadj, sum) #total number of days/year when all bottom layers are dry
            res.dry[, 4] <- tapply(AllDry$top, simTime2$year_ForEachUsedDay_NSadj, max_duration) #maximum number of continous days when all top layers are dry
            res.dry[, 8] <- tapply(AllDry$bottom, simTime2$year_ForEachUsedDay_NSadj, max_duration) #maximum number of continous days when all bottom layers are dry
            res.dry[, 1] <- tapply(AtLeastOneDry$top, simTime2$year_ForEachUsedDay_NSadj, startDoyOfDuration, duration = durationDryPeriods.min)  # start days/year when at least one of top layers are dry for at least ten days
            res.dry[, 5] <- tapply(AtLeastOneDry$bottom, simTime2$year_ForEachUsedDay_NSadj, startDoyOfDuration, duration = durationDryPeriods.min)  # start days/year when at least one of bottom layers are dry for at least ten days
            res.dry[, 2] <- tapply(AtLeastOneDry$top, simTime2$year_ForEachUsedDay_NSadj, endDoyAfterDuration, duration = durationDryPeriods.min)  # end days/year when at least one of top layers have been dry for at least ten days
            res.dry[, 6] <- tapply(AtLeastOneDry$bottom, simTime2$year_ForEachUsedDay_NSadj, endDoyAfterDuration, duration = durationDryPeriods.min) # end days/year when at least one of bottom layers have been dry for at least ten days
            res.dry[, c(1:2, 5:5)] <- res.dry[, c(1:2, 5:5)] - adjDays
            res.dry[res.dry[, 1] > res.dry[, 2], 3] <- 0 #correct [, c(3, 7)] for years when start<end otherwise set 0
            res.dry[res.dry[, 5] > res.dry[, 6], 7] <- 0 #correct [, c(3, 7)] for years when start<end otherwise set 0

            #aggregate results
            temp <- data.frame(res.wet, res.dry[, -c(1:2, 5:6)])
            resMeans[(nv+16*(icrit-1)):(nv+16*icrit-1)] <- c(colMeans(temp, na.rm = TRUE),
                apply(res.dry[, c(1:2, 5:6), drop = FALSE], 2, circ_mean, int = 365, na.rm = TRUE))
            resSDs[(nv+16*(icrit-1)):(nv+16*icrit-1)] <- c(apply(temp, 2, stats::sd, na.rm = TRUE),
                apply(res.dry[, c(1:2, 5:6), drop = FALSE], 2, circ_sd, int = 365, na.rm = TRUE))
          }
          nv <- nv+16*opt_agg[["SWPcrit_N"]]

          rm(res.dry, wet, wet_crit, AtLeastOneWet, AllWet, AllDry)

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "dailySWPdrynessANDwetness")
        }

      #38
        if (isTRUE(prj_todos[["aon"]][["dailySuitablePeriodsDuration"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "dailySuitablePeriodsDuration")
          if (!exists("vwcmatric.dy")) vwcmatric.dy <- get_Response_aggL(swof["sw_vwcmatric"], tscale = "dy", scaler = 1, FUN = stats::weighted.mean, weights = layers_width, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)
          if (!exists("swpmatric.dy")) swpmatric.dy <- get_SWPmatric_aggL(vwcmatric.dy, texture, sand, clay)
          if (!exists("temp.dy")) temp.dy <- get_Temp_dy(runDataSC, isim_time)
          if (!exists("SWE.dy")) SWE.dy <- get_SWE_dy(runDataSC, isim_time)

          quantiles <- c(0.05, 0.5, 0.95)
          snowfree <- SWE.dy$val == 0
          niceTemp <- temp.dy$mean >= opt_agg[["Tbase_DD_C"]]

          for (icrit in seq(along = opt_agg[["SWPcrit_MPa"]])) {
            wet.top <- swpmatric.dy$top >= opt_agg[["SWPcrit_MPa"]][icrit]

            if (length(bottomL) > 0 && !identical(bottomL, 0)) {
              wet.bottom <- swpmatric.dy$bottom >= opt_agg[["SWPcrit_MPa"]][icrit]
            } else {
              wet.bottom <- rep(FALSE, length(wet.top))
            }

            durations.top <- sapply(isim_time$useyrs, FUN = function(y) {if (length(temp <- (temp <- rle((snowfree & niceTemp & wet.top)[simTime2$year_ForEachUsedDay == y]))$lengths[temp$values]) > 0) return(max(temp)) else return(0)})
            durations.bottom <- sapply(isim_time$useyrs, FUN = function(y) {if (length(temp <- (temp <- rle((snowfree & niceTemp & wet.bottom)[simTime2$year_ForEachUsedDay == y]))$lengths[temp$values]) > 0) return(max(temp)) else return(0)})

            resMeans[nv:(nv+2*length(quantiles)-1)] <- c(stats::quantile(durations.top, probs = quantiles, type = 8), stats::quantile(durations.bottom, probs = quantiles, type = 8))

            nv <- nv+2*length(quantiles)
          }

          rm(wet.top, wet.bottom, durations.top, snowfree, niceTemp)

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "dailySuitablePeriodsDuration")
        }

      #39
        if (isTRUE(prj_todos[["aon"]][["dailySuitablePeriodsAvailableWater"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "dailySuitablePeriodsAvailableWater")
          if (!exists("swcbulk.dy")) swcbulk.dy <- get_Response_aggL(swof["sw_swcbulk"], tscale = "dy", scaler = 10, FUN = sum, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)
          if (!exists("temp.dy")) temp.dy <- get_Temp_dy(runDataSC, isim_time)
          if (!exists("SWE.dy")) SWE.dy <- get_SWE_dy(runDataSC, isim_time)

          suitable <- (SWE.dy$val == 0) & (temp.dy$mean >= opt_agg[["Tbase_DD_C"]])

          for (icrit in seq(along = opt_agg[["SWPcrit_MPa"]])) {
            SWCcritT <- SWPtoVWC(opt_agg[["SWPcrit_MPa"]][icrit], texture$sand.top, texture$clay.top) * 10 * sum(layers_width[topL])
            swa.top <- ifelse(suitable, cut0Inf(swcbulk.dy$top - SWCcritT, val = 0), 0)

            if (length(bottomL) > 0 && !identical(bottomL, 0)) {
              SWCcritB <- SWPtoVWC(opt_agg[["SWPcrit_MPa"]][icrit], texture$sand.bottom, texture$clay.bottom) * 10 * sum(layers_width[bottomL])
              swa.bottom <- ifelse(suitable, cut0Inf(swcbulk.dy$bottom - SWCcritB, val = 0), 0)
            } else {
              swa.bottom <- rep(0, length(swa.top))
            }

            temp <- list(t = tapply(swa.top, simTime2$year_ForEachUsedDay_NSadj, sum),
                                    b = tapply(swa.bottom, simTime2$year_ForEachUsedDay_NSadj, sum))
            resMeans[nv:(nv+1)] <- sapply(temp, mean)
            resSDs[nv:(nv+1)] <- sapply(temp, stats::sd)
            nv <- nv+2
          }

          rm(swa.top, swa.bottom, suitable)

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "dailySuitablePeriodsAvailableWater")
        }

      #40
        if (isTRUE(prj_todos[["aon"]][["dailySuitablePeriodsDrySpells"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "dailySuitablePeriodsDrySpells")
          if (!exists("vwcmatric.dy.all")) vwcmatric.dy.all <- get_Response_aggL(swof["sw_vwcmatric"], tscale = "dyAll", scaler = 1, FUN = stats::weighted.mean, weights = layers_width, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)
          if (!exists("swpmatric.dy.all")) swpmatric.dy.all <- get_SWPmatric_aggL(vwcmatric.dy.all, texture, sand, clay) #swp.dy.all is required to get all layers
          if (!exists("temp.dy")) temp.dy <- get_Temp_dy(runDataSC, isim_time)
          if (!exists("SWE.dy")) SWE.dy <- get_SWE_dy(runDataSC, isim_time)

          suitable <- (SWE.dy$val == 0) & (temp.dy$mean >= opt_agg[["Tbase_DD_C"]])

          adjDays <- simTime2$doy_ForEachUsedDay_NSadj[1] - simTime2$doy_ForEachUsedDay[1]
          durationDryPeriods.min <- 10 # days

          for (icrit in seq(along = opt_agg[["SWPcrit_MPa"]])) {
            dry_crit <- swpmatric.dy.all$val < opt_agg[["SWPcrit_MPa"]][icrit]
            if (length(topL) > 1) {
              dry.top <- apply(dry_crit[isim_time$index.usedy, 2+topL], 1, sum)
            } else {
              dry.top <- dry_crit[isim_time$index.usedy, 2+topL]
            }
            dry.top <- (suitable & dry.top >= length(topL))
            if (length(bottomL) > 1) {
              dry.bottom <- apply(dry_crit[isim_time$index.usedy, 2+bottomL], 1, sum)
            } else if (length(bottomL) > 0 && !identical(bottomL, 0)) {
              dry.bottom <- ifelse(dry_crit[isim_time$index.usedy, 2+bottomL], 1, 0)
            }
            if (length(bottomL) > 0 && !identical(bottomL, 0)) {
              dry.bottom <- (suitable & dry.bottom >= length(bottomL))
            } else {
              dry.bottom <- rep(FALSE, length(dry.top))
            }

            temp <- stats::aggregate(cbind(dry.top, dry.bottom), by = list(simTime2$year_ForEachUsedDay_NSadj), FUN = function(x) c(if (any((temp <- rle(x))$values)) c(mean(temp$lengths[temp$values]), max(temp$lengths[temp$values])) else c(0, 0), sum(x), startDoyOfDuration(x, duration = durationDryPeriods.min) - adjDays))
            resMeans[nv:(nv+7)] <- c(apply(temp$dry.top[, 1:3, drop = FALSE], 2, mean), circ_mean(x = temp$dry.top[, 4], int = 365), apply(temp$dry.bottom[, 1:3, drop = FALSE], 2, mean), circ_mean(x = temp$dry.bottom[, 4], int = 365))
            resSDs[nv:(nv+7)] <- c(apply(temp$dry.top[, 1:3, drop = FALSE], 2, stats::sd), circ_sd(x = temp$dry.top[, 4], int = 365), apply(temp$dry.bottom[, 1:3, drop = FALSE], 2, stats::sd), circ_sd(x = temp$dry.bottom[, 4], int = 365))
            nv <- nv+8
          }

          rm(dry.top, dry.bottom, suitable, dry_crit, adjDays, durationDryPeriods.min)

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "dailySuitablePeriodsDrySpells")
        }

      #41 #cummulative frequency distribution of durations of dry soils in each of the four seasons and for each of the SWP.crit
        if (isTRUE(prj_todos[["aon"]][["dailySWPdrynessDurationDistribution"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "dailySWPdrynessDurationDistribution")
          if (!exists("vwcmatric.dy")) vwcmatric.dy <- get_Response_aggL(swof["sw_vwcmatric"], tscale = "dy", scaler = 1, FUN = stats::weighted.mean, weights = layers_width, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)
          if (!exists("swpmatric.dy")) swpmatric.dy <- get_SWPmatric_aggL(vwcmatric.dy, texture, sand, clay)

          deciles <- (0:10)*10/100
          quantiles <- (0:4)/4
          mo_seasons <- matrix(data = c(12, 1:11), ncol = 3, nrow = 4, byrow = TRUE)
          season.flag <- c("DJF", "MAM", "JJA", "SON")
          seasonal.years <- c(simTime2$year_ForEachUsedDay[-(1:31)], rep(-9999, times = 31))  #shift beginning of year to Dec 1

          for (icrit in seq(along = opt_agg[["SWPcrit_MPa"]])) {

            wet.top <- swpmatric.dy$top >= opt_agg[["SWPcrit_MPa"]][icrit]

            if (length(bottomL) > 0 && !identical(bottomL, 0)) wet.bottom <- swpmatric.dy$bottom >= opt_agg[["SWPcrit_MPa"]][icrit]

            for (season in 1:nrow(mo_seasons)) {
              durations.top <- sapply(isim_time$useyrs, FUN = function(y) {if (length(temp <- (temp <- rle(wet.top[seasonal.years == y & (simTime2$month_ForEachUsedDay %in% mo_seasons[season, ])] == 0))$lengths[temp$values]) > 0) return(max(temp)) else return(0)})
              if (length(bottomL) > 0 && !identical(bottomL, 0)) durations.bottom <- sapply(isim_time$useyrs, FUN = function(y) {if (length(temp <- (temp <- rle(wet.bottom[seasonal.years == y & (simTime2$month_ForEachUsedDay %in% mo_seasons[season, ])] == 0))$lengths[temp$values]) > 0) return(max(temp)) else return(0)})

              resMeans[nv:(nv+length(quantiles)-1)] <- stats::quantile(durations.top, probs = quantiles, type = 7)
              resMeans[(nv+length(quantiles)):(nv+2*length(quantiles)-1)] <- if (length(bottomL) > 0 && !identical(bottomL, 0)) stats::quantile(durations.bottom, probs = quantiles, type = 7) else 0

              nv <- nv+2*length(quantiles)
            }
          }

          rm(wet.top, durations.top)
          if (length(bottomL) > 0 && !identical(bottomL, 0)) rm(wet.bottom)

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "dailySWPdrynessDurationDistribution")
        }

      #42
        if (isTRUE(prj_todos[["aon"]][["dailySWPdrynessEventSizeDistribution"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "dailySWPdrynessEventSizeDistribution")
          if (!exists("vwcmatric.dy")) vwcmatric.dy <- get_Response_aggL(swof["sw_vwcmatric"], tscale = "dy", scaler = 1, FUN = stats::weighted.mean, weights = layers_width, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)
          if (!exists("swpmatric.dy")) swpmatric.dy <- get_SWPmatric_aggL(vwcmatric.dy, texture, sand, clay)
          binSize <- c(1, 8, 15, 29, 57, 183, 367) #closed interval lengths in [days] within a year; NOTE: n_variables is set for binsN == 6
          binsN <- length(binSize) - 1

          for (icrit in seq_along(opt_agg[["SWPcrit_MPa"]])) {

            dry.top <- swpmatric.dy$top[isim_time$index.usedy] < opt_agg[["SWPcrit_MPa"]][icrit]

            if (length(bottomL) > 0 && !identical(bottomL, 0)) {
              dry.bottom <- swpmatric.dy$bottom[isim_time$index.usedy] < opt_agg[["SWPcrit_MPa"]][icrit]
            }

            #apply over each year, rle just on selected year store runs in vec, if that is greater than 0 then add to that years bins else return 0s for that year. Will result in a matrix of 4 by Years
            binsYears.top <- stats::aggregate(dry.top, by = list(simTime2$year_ForEachUsedDay_NSadj), FUN = EventDistribution, N = binsN, size = binSize)$x
            eventsPerYear <- apply(binsYears.top, MARGIN = 1, FUN = sum)
            freqBins <- sweep(binsYears.top, MARGIN = 1, STATS = eventsPerYear, FUN = "/")
            events.top <- c(mean(eventsPerYear, na.rm = TRUE), stats::sd(eventsPerYear, na.rm = TRUE))
            bin_top_mean <- apply(freqBins, MARGIN = 2, mean, na.rm = TRUE) #mean of each bin size across a year - vector of binsN
            bin_top_sd <- apply(freqBins, MARGIN = 2, stats::sd, na.rm = TRUE) # stats::sd of each bin size across a year - vector of binsN

            resMeans[nv] <- events.top[1]
            resSDs[nv] <- events.top[2]
            resMeans[(nv+1):(nv+binsN)] <- bin_top_mean
            resSDs[(nv+1):(nv+binsN)] <- bin_top_sd

            if (length(bottomL) > 0 && !identical(bottomL, 0)) {
              binsYears.bottom <- stats::aggregate(dry.bottom, by = list(simTime2$year_ForEachUsedDay_NSadj), FUN = EventDistribution, N = binsN, size = binSize)$x
              eventsPerYear <- apply(binsYears.bottom, MARGIN = 1, FUN = sum)
              freqBins <- sweep(binsYears.bottom, MARGIN = 1, STATS = eventsPerYear, FUN = "/")
              events.bottom <- c(mean(eventsPerYear, na.rm = TRUE), stats::sd(eventsPerYear, na.rm = TRUE))
              bin_bottom_mean <- apply(freqBins, MARGIN = 2, mean, na.rm = TRUE)
              bin_bottom_sd <- apply(freqBins, MARGIN = 2, stats::sd, na.rm = TRUE)

              resMeans[nv+binsN+1] <- events.bottom[1]
              resSDs[nv+binsN+1] <- events.bottom[2]
              resMeans[(nv+binsN+2):(nv+2*binsN+1)] <- bin_bottom_mean
              resSDs[(nv+binsN+2):(nv+2*binsN+1)] <- bin_bottom_sd
            }


            nv <- nv+2+2*binsN
          }
          rm(dry.top, binsN, binSize, events.top, eventsPerYear, freqBins)
          if (length(bottomL) > 0 && !identical(bottomL, 0)) rm(dry.bottom, events.bottom)

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "dailySWPdrynessEventSizeDistribution")
        }

      #43
        if (isTRUE(prj_todos[["aon"]][["dailySWPdrynessIntensity"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "dailySWPdrynessIntensity")
          if (!exists("vwcmatric.dy")) vwcmatric.dy <- get_Response_aggL(swof["sw_vwcmatric"], tscale = "dy", scaler = 1, FUN = stats::weighted.mean, weights = layers_width, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)

          SWCtop <- vwcmatric.dy$top * sum(layers_width[topL])*10
          if (length(bottomL) > 0 && !identical(bottomL, 0)) SWCbottom <- vwcmatric.dy$bottom * sum(layers_width[bottomL])*10

          for (icrit in seq(along = opt_agg[["SWPcrit_MPa"]])) {
            #amount of SWC required so that layer wouldn't be dry
            SWCcritT <- SWPtoVWC(opt_agg[["SWPcrit_MPa"]][icrit], texture$sand.top, texture$clay.top) * sum(layers_width[topL])*10
            missingSWCtop <- cut0Inf(SWCcritT - SWCtop, val = 0)
            IntensitySum_top <- c(mean(temp <- sapply(isim_time$useyrs, FUN = function(y) sum(missingSWCtop[simTime2$year_ForEachUsedDay == y])), na.rm = TRUE), stats::sd(temp, na.rm = TRUE))
            IntensityMean_top <- c(mean(temp <- sapply(isim_time$useyrs, FUN = function(y) mean((temp <- missingSWCtop[simTime2$year_ForEachUsedDay == y])[temp > 0], na.rm = TRUE)), na.rm = TRUE), stats::sd(temp, na.rm = TRUE))
            IntensityDurationAndNumber_top <- c(apply(temp <- sapply(isim_time$useyrs, FUN = function(y) c(mean(temp <- (temp <- rle(missingSWCtop[simTime2$year_ForEachUsedDay == y] > 0))$lengths[temp$values]), length(temp))), 1, mean), apply(temp, 1, stats::sd))[c(1, 3, 2, 4)]

            if (length(bottomL) > 0 && !identical(bottomL, 0)) {
              SWCcritB <- SWPtoVWC(opt_agg[["SWPcrit_MPa"]][icrit], texture$sand.bottom, texture$clay.bottom) * sum(layers_width[bottomL])*10
              missingSWCbottom <- cut0Inf(SWCcritB - SWCbottom, val = 0)
              IntensitySum_bottom <- c(mean(temp <- sapply(isim_time$useyrs, FUN = function(y) sum(missingSWCbottom[simTime2$year_ForEachUsedDay == y])), na.rm = TRUE), stats::sd(temp, na.rm = TRUE))
              IntensityMean_bottom <- c(mean(temp <- sapply(isim_time$useyrs, FUN = function(y) mean((temp <- missingSWCbottom[simTime2$year_ForEachUsedDay == y])[temp > 0], na.rm = TRUE)), na.rm = TRUE), stats::sd(temp, na.rm = TRUE))
              IntensityDurationAndNumber_bottom <- c(apply(temp <- sapply(isim_time$useyrs, FUN = function(y) c(mean(temp <- (temp <- rle(missingSWCbottom[simTime2$year_ForEachUsedDay == y] > 0))$lengths[temp$values]), length(temp))), 1, mean), apply(temp, 1, stats::sd))[c(1, 3, 2, 4)]
            }

            resMeans[nv:(nv+3)] <- c(IntensitySum_top[1], IntensityMean_top[1], IntensityDurationAndNumber_top[c(1, 3)])
            resSDs[nv:(nv+3)] <- c(IntensitySum_top[2], IntensityMean_top[2], IntensityDurationAndNumber_top[c(2, 4)])
            resMeans[(nv+4):(nv+7)] <- if (length(bottomL) > 0 && !identical(bottomL, 0)) c(IntensitySum_bottom[1], IntensityMean_bottom[1], IntensityDurationAndNumber_bottom[c(1, 3)]) else rep(0, 4)
            resSDs[(nv+4):(nv+7)] <- if (length(bottomL) > 0 && !identical(bottomL, 0)) c(IntensitySum_bottom[2], IntensityMean_bottom[2], IntensityDurationAndNumber_bottom[c(2, 4)]) else rep(0, 4)

            nv <- nv+8
          }
          rm(  SWCcritT, missingSWCtop, IntensitySum_top, IntensityMean_top, IntensityDurationAndNumber_top)
          if (length(bottomL) > 0 && !identical(bottomL, 0)) rm(SWCcritB, missingSWCbottom, IntensitySum_bottom, IntensityMean_bottom, IntensityDurationAndNumber_bottom)

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "dailySWPdrynessIntensity")
        }

      #43.2
        if (isTRUE(prj_todos[["aon"]][["dailyThermalDrynessStress"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "dailyThermalDrynessStress")
          if (!exists("vwcmatric.dy.all")) vwcmatric.dy.all <- get_Response_aggL(swof["sw_vwcmatric"], tscale = "dyAll", scaler = 1, FUN = stats::weighted.mean, weights = layers_width, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)
          if (!exists("swpmatric.dy.all")) swpmatric.dy.all <- get_SWPmatric_aggL(vwcmatric.dy.all, texture, sand, clay) #swp.dy.all is required to get all layers
          if (!exists("vwcmatric.dy")) vwcmatric.dy <- get_Response_aggL(swof["sw_vwcmatric"], tscale = "dy", scaler = 1, FUN = stats::weighted.mean, weights = layers_width, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)
          if (!exists("swpmatric.dy")) swpmatric.dy <- get_SWPmatric_aggL(vwcmatric.dy, texture, sand, clay)
          if (!exists("temp.dy")) temp.dy <- get_Temp_dy(runDataSC, isim_time)
          if (!exists("vpd.dy")) vpd.dy <- get_VPD_dy(sc, temp.dy, xin = swRunScenariosData, st2 = simTime2)

          # Aggregate for hottest and for coldest conditions
          extreme <- c(hottest = TRUE, coldest = FALSE)

          # Set up soil moisture stress conditions
          dryness <- matrix(rep.int(opt_agg[["SWPcrit_MPa"]], isim_time$no.usedy),
              ncol = opt_agg[["SWPcrit_N"]], byrow = TRUE)
          snowfree <- SWE.dy$val <= SFSW2_glovars[["tol"]]

          n_conds <- 4L
          conds <- list() # max length(conds) == n_conds
          conds[["Always"]] <- matrix(TRUE, nrow = isim_time$no.usedy, ncol = 1)
          temp <- swpmatric.dy.all$val[isim_time$index.usedy, -(1:2), drop = FALSE]
          conds[["DryAll"]] <- apply(temp, 1, max) < dryness
          conds[["DryTop"]] <- swpmatric.dy$top < dryness
          conds[["DryBottom"]] <- if (length(bottomL) > 0 && !identical(bottomL, 0)) {
              swpmatric.dy$bottom < dryness
            } else{
              matrix(FALSE, nrow = isim_time$no.usedy, ncol = opt_agg[["SWPcrit_N"]])
            }

          for (d3 in seq_len(n_conds)) {
            #--- Moisture/temperature stress during hot/cold and soil-dry periods
            N <- ncol(conds[[d3]]) # either 1 or opt_agg[["SWPcrit_N"]]
            Ns <- seq_len(N)

            # Daily VPD on soil-dry days (for 1 column or for each opt_agg[["SWPcrit_N"]])
            VPD_during_Stress <- ifelse(conds[[d3]], vpd.dy$mean, NA)
            # Daily air temperature on soil-dry days
            Temp_during_Stress1 <- ifelse(conds[[d3]], temp.dy$mean, NA)
            # Daily air temperature on snowfree, soil-dry days
            Temp_during_Stress2 <- ifelse(conds[[d3]] & snowfree, temp.dy$mean, NA)

            # Output container for VPD and Temp on 10 hottest/coldest, soil-dry days
            # and for Temp on 10 hottest/coldest, snowfree, soil-dry days
            out_during_Stress <- array(NA, dim = c(isim_time$no.useyr, 3 * N))

            for (ihot in seq_along(extreme)) {
              for (d2 in Ns) {
                # indices (=doy) of k-largest/smallest temperature values per year given soil is dry
                ids_hotcold <- tapply(Temp_during_Stress1[, d2],
                  INDEX = simTime2$year_ForEachUsedDay, FUN = fun_kLargest,
                  largest = extreme[ihot], fun = "index", k = 10L, na.rm = TRUE)

                # values of mean VPD and of mean temperature during k-indices per year
                out_during_Stress[, c(d2, N + d2)] <- t(sapply(seq_len(isim_time$no.useyr),
                  function(j) {
                    ids <- simTime2$doy_ForEachUsedDay %in% ids_hotcold[[j]] &
                      simTime2$year_ForEachUsedDay == isim_time$useyrs[j]
                    c(mean(VPD_during_Stress[ids, d2]), mean(Temp_during_Stress1[ids, d2]))
                  }))

                # mean temperature during 10 hottest/coldest, snowfree, soil-dry days
                out_during_Stress[, 2 * N + d2] <- tapply(Temp_during_Stress2[, d2],
                  INDEX = simTime2$year_ForEachUsedDay, FUN = fun_kLargest,
                  largest = extreme[ihot], fun = mean, k = 10L, na.rm = TRUE)
              }

              nv_add <- ncol(out_during_Stress)
              nv_new <- nv + nv_add

              resMeans[nv:(nv_new - 1)] <- .colMeans(out_during_Stress,
                isim_time$no.useyr, nv_add)
              resSDs[nv:(nv_new - 1)] <- apply(out_during_Stress, 2, stats::sd)
              nv <- nv_new

              nv_new <- nv + N
              resMeans[nv:(nv_new - 1)] <-
                apply(out_during_Stress[, Ns, drop = FALSE], 2, max)
              nv <- nv_new

              nv_new <- nv + 2 * N
              resMeans[nv:(nv_new - 1)] <-
                apply(out_during_Stress[, c(N + Ns, 2 * N + Ns), drop = FALSE], 2, min)
              nv <- nv_new
            }
          }

          rm(dryness, conds, VPD_during_Stress, Temp_during_Stress1,
            Temp_during_Stress2, out_during_Stress)

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "dailyThermalDrynessStress")
        }

      #43.3
        if (isTRUE(prj_todos[["aon"]][["periodicVWCmatricFirstLayer"]])) {
          nv0 <- nv

          if (isTRUE(opt_agg$use_doy_range)) {

            print_debug(opt_verbosity, tag_simpidfid, "aggregating", "periodicVWCmatricFirstLayer")
            if (!exists("vwcmatric.dy.all")) vwcmatric.dy.all <- get_Response_aggL(swof["sw_vwcmatric"], tscale = "dyAll", scaler = 1, FUN = stats::weighted.mean, weights = layers_width, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)

            doy.trim <- if (!is.null(opt_agg[["doy_ranges"]][["periodicVWCmatric"]])){
              c(opt_agg[["doy_ranges"]][["periodicVWCmatric"]][1]:opt_agg[["doy_ranges"]][["periodicVWCmatric"]][2])
            } else {
              c(opt_agg[["doy_ranges"]][["default"]][1]:opt_agg[["doy_ranges"]][["default"]][2])
            }

            years <- unique(vwcmatric.dy.all$val[,1])
            year.trim <- years[2:length(years)]

            vwclayervals <- vwcmatric.dy.all$val[vwcmatric.dy.all$val[,1] %in% year.trim,]
            vwclayervals <-vwclayervals[vwclayervals[,2] %in% doy.trim,]

            periodicVWCmeans <- tapply(vwclayervals[,3], vwclayervals[,1], mean)
            periodicVWCsums <- tapply(vwclayervals[,3], vwclayervals[,1], sum)

            resMeans[nv:(nv+1)] <- mean(periodicVWCmeans, na.rm = TRUE)
            resSDs[nv:(nv+1)] <- sd(periodicVWCmeans, na.rm = TRUE)
            nv <- nv + 1

            resMeans[nv:(nv+1)] <- mean(periodicVWCsums, na.rm = TRUE)
            resSDs[nv:(nv+1)] <- sd(periodicVWCsums, na.rm = TRUE)
            nv <- nv + 1

            rm(vwclayervals, doy.trim, year.trim, periodicVWCmeans, periodicVWCsums)
          }

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "periodicVWCmatricFirstLayer")
        }


        #---Aggregation: Mean monthly values
      #44
        if (isTRUE(prj_todos[["aon"]][["monthlyTemp"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "monthlyTemp")
          if (!exists("temp.mo")) temp.mo <- get_Temp_mo(runDataSC, isim_time)

          resMeans[nv+SFSW2_glovars[["st_mo"]]-1] <- tapply(temp.mo$mean, simTime2$month_ForEachUsedMonth, mean)
          resSDs[nv+SFSW2_glovars[["st_mo"]]-1] <- tapply(temp.mo$mean, simTime2$month_ForEachUsedMonth, stats::sd)
          nv <- nv+12

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "monthlyTemp")
        }

      #45
        if (isTRUE(prj_todos[["aon"]][["monthlyPPT"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "monthlyPPT")
          if (!exists("prcp.mo")) prcp.mo <- get_PPT_mo(runDataSC, isim_time)

          resMeans[nv+SFSW2_glovars[["st_mo"]]-1] <- tapply(prcp.mo$ppt, simTime2$month_ForEachUsedMonth, mean)
          resSDs[nv+SFSW2_glovars[["st_mo"]]-1] <- tapply(prcp.mo$ppt, simTime2$month_ForEachUsedMonth, stats::sd)
          nv <- nv+12

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "monthlyPPT")
        }

      #46
        if (isTRUE(prj_todos[["aon"]][["monthlySnowpack"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "monthlySnowpack")
          if (!exists("SWE.mo")) SWE.mo <- get_SWE_mo(runDataSC, isim_time)

          resMeans[nv+SFSW2_glovars[["st_mo"]]-1] <- tapply(SWE.mo$val, simTime2$month_ForEachUsedMonth, mean)
          resSDs[nv+SFSW2_glovars[["st_mo"]]-1] <- tapply(SWE.mo$val, simTime2$month_ForEachUsedMonth, stats::sd)
          nv <- nv+12

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "monthlySnowpack")
        }

      #47
        if (isTRUE(prj_todos[["aon"]][["monthlySoilTemp"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "monthlySoilTemp")
          if (!exists("soiltemp.mo")) soiltemp.mo <- get_Response_aggL(swof["sw_soiltemp"], tscale = "mo", scaler = 1, FUN = stats::weighted.mean, weights = layers_width, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)

          resMeans[nv+SFSW2_glovars[["st_mo"]]-1] <- tapply(soiltemp.mo$top, simTime2$month_ForEachUsedMonth, mean)
          resSDs[nv+SFSW2_glovars[["st_mo"]]-1] <- tapply(soiltemp.mo$top, simTime2$month_ForEachUsedMonth, stats::sd)
          resMeans[nv+SFSW2_glovars[["st_mo"]]-1+12] <- tapply(soiltemp.mo$bottom, simTime2$month_ForEachUsedMonth, mean)
          resSDs[nv+SFSW2_glovars[["st_mo"]]-1+12] <- tapply(soiltemp.mo$bottom, simTime2$month_ForEachUsedMonth, stats::sd)
          nv <- nv+24

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "monthlySoilTemp")
        }

      #48
        if (isTRUE(prj_todos[["aon"]][["monthlyRunoff"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "monthlyRunoff")
          if (!exists("runonoff.mo")) runonoff.mo <- get_RunOnOff_mo(runDataSC, isim_time)

          resMeans[nv+SFSW2_glovars[["st_mo"]]-1] <- tapply(runonoff.mo$total_runoff,
            simTime2$month_ForEachUsedMonth, mean)
          resSDs[nv+SFSW2_glovars[["st_mo"]]-1] <- tapply(runonoff.mo$total_runoff,
            simTime2$month_ForEachUsedMonth, stats::sd)
          nv <- nv+12

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "monthlyRunoff")
        }

        if (isTRUE(prj_todos[["aon"]][["monthlyRunon"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "monthlyRunon")
          if (!exists("runonoff.mo")) runonoff.mo <- get_RunOnOff_mo(runDataSC, isim_time)

          resMeans[nv+SFSW2_glovars[["st_mo"]]-1] <- tapply(runonoff.mo$total_runon,
            simTime2$month_ForEachUsedMonth, mean)
          resSDs[nv+SFSW2_glovars[["st_mo"]]-1] <- tapply(runonoff.mo$total_runon,
            simTime2$month_ForEachUsedMonth, stats::sd)
          nv <- nv+12

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "monthlyRunon")
        }

      #49
        if (isTRUE(prj_todos[["aon"]][["monthlyHydraulicRedistribution"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "monthlyHydraulicRedistribution")
          if (!exists("hydred.mo")) hydred.mo <- get_Response_aggL(swof["sw_hd"], tscale = "mo", scaler = 10, FUN = sum, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)

          resMeans[nv+SFSW2_glovars[["st_mo"]]-1] <- tapply(hydred.mo$top, simTime2$month_ForEachUsedMonth, mean)
          resSDs[nv+SFSW2_glovars[["st_mo"]]-1] <- tapply(hydred.mo$top, simTime2$month_ForEachUsedMonth, stats::sd)
          resMeans[nv+SFSW2_glovars[["st_mo"]]-1+12] <- tapply(hydred.mo$bottom, simTime2$month_ForEachUsedMonth, mean)
          resSDs[nv+SFSW2_glovars[["st_mo"]]-1+12] <- tapply(hydred.mo$bottom, simTime2$month_ForEachUsedMonth, stats::sd)
          nv <- nv+24

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "monthlyHydraulicRedistribution")
        }

      #50
        if (isTRUE(prj_todos[["aon"]][["monthlyInfiltration"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "monthlyInfiltration")
          if (!exists("inf.mo")) inf.mo <- get_Inf_mo(runDataSC, isim_time)

          resMeans[nv+SFSW2_glovars[["st_mo"]]-1] <- tapply(inf.mo$inf, simTime2$month_ForEachUsedMonth, mean)
          resSDs[nv+SFSW2_glovars[["st_mo"]]-1] <- tapply(inf.mo$inf, simTime2$month_ForEachUsedMonth, stats::sd)
          nv <- nv+12

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "monthlyInfiltration")
        }

      #51
        if (isTRUE(prj_todos[["aon"]][["monthlyDeepDrainage"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "monthlyDeepDrainage")
          if (!exists("deepDrain.mo")) deepDrain.mo <- get_DeepDrain_mo(runDataSC, isim_time)

          resMeans[nv+SFSW2_glovars[["st_mo"]]-1] <- tapply(deepDrain.mo$val, simTime2$month_ForEachUsedMonth, mean)
          resSDs[nv+SFSW2_glovars[["st_mo"]]-1] <- tapply(deepDrain.mo$val, simTime2$month_ForEachUsedMonth, stats::sd)
          nv <- nv+12

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "monthlyDeepDrainage")
        }

      #52
        if (isTRUE(prj_todos[["aon"]][["monthlySWPmatric"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "monthlySWPmatric")
          if (!exists("vwcmatric.mo")) vwcmatric.mo <- get_Response_aggL(swof["sw_vwcmatric"], tscale = "mo", scaler = 1, FUN = stats::weighted.mean, weights = layers_width, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)
          if (!exists("swpmatric.mo")) swpmatric.mo <- get_SWPmatric_aggL(vwcmatric.mo, texture, sand, clay)

          resMeans[nv+SFSW2_glovars[["st_mo"]]-1] <- swpmatric.mo$aggMean.top
          resMeans[nv+SFSW2_glovars[["st_mo"]]-1+12] <- swpmatric.mo$aggMean.bottom
          nv <- nv+24

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "monthlySWPmatric")
        }

      #53 a.)
        if (isTRUE(prj_todos[["aon"]][["monthlyVWCbulk"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "monthlyVWCbulk")
          if (!exists("vwcbulk.mo")) vwcbulk.mo <- get_Response_aggL(swof["sw_vwcbulk"], tscale = "mo", scaler = 1, FUN = stats::weighted.mean, weights = layers_width, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)

          resMeans[nv+SFSW2_glovars[["st_mo"]]-1] <- tapply(vwcbulk.mo$top, simTime2$month_ForEachUsedMonth, mean)
          resSDs[nv+SFSW2_glovars[["st_mo"]]-1] <- tapply(vwcbulk.mo$top, simTime2$month_ForEachUsedMonth, stats::sd)
          resMeans[nv+SFSW2_glovars[["st_mo"]]-1+12] <- tapply(vwcbulk.mo$bottom, simTime2$month_ForEachUsedMonth, mean)
          resSDs[nv+SFSW2_glovars[["st_mo"]]-1+12] <- tapply(vwcbulk.mo$bottom, simTime2$month_ForEachUsedMonth, stats::sd)
          nv <- nv+24

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "monthlyVWCbulk")
        }

      #53 b.)
        if (isTRUE(prj_todos[["aon"]][["monthlyVWCmatric"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "monthlyVWCmatric")
          if (!exists("vwcmatric.mo")) vwcmatric.mo <- get_Response_aggL(swof["sw_vwcmatric"], tscale = "mo", scaler = 1, FUN = stats::weighted.mean, weights = layers_width, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)

          resMeans[nv+SFSW2_glovars[["st_mo"]]-1] <- tapply(vwcmatric.mo$top, simTime2$month_ForEachUsedMonth, mean)
          resSDs[nv+SFSW2_glovars[["st_mo"]]-1] <- tapply(vwcmatric.mo$top, simTime2$month_ForEachUsedMonth, stats::sd)
          resMeans[nv+SFSW2_glovars[["st_mo"]]-1+12] <- tapply(vwcmatric.mo$bottom, simTime2$month_ForEachUsedMonth, mean)
          resSDs[nv+SFSW2_glovars[["st_mo"]]-1+12] <- tapply(vwcmatric.mo$bottom, simTime2$month_ForEachUsedMonth, stats::sd)
          nv <- nv+24

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "monthlyVWCmatric")
        }

      #54
        if (isTRUE(prj_todos[["aon"]][["monthlySWCbulk"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "monthlySWCbulk")
          if (!exists("swcbulk.mo")) swcbulk.mo <- get_Response_aggL(swof["sw_swcbulk"], tscale = "mo", scaler = 10, FUN = sum, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)

          resMeans[nv+SFSW2_glovars[["st_mo"]]-1] <- tapply(swcbulk.mo$top, simTime2$month_ForEachUsedMonth, mean)
          resSDs[nv+SFSW2_glovars[["st_mo"]]-1] <- tapply(swcbulk.mo$top, simTime2$month_ForEachUsedMonth, stats::sd)
          resMeans[nv+SFSW2_glovars[["st_mo"]]-1+12] <- tapply(swcbulk.mo$bottom, simTime2$month_ForEachUsedMonth, mean)
          resSDs[nv+SFSW2_glovars[["st_mo"]]-1+12] <- tapply(swcbulk.mo$bottom, simTime2$month_ForEachUsedMonth, stats::sd)
          nv <- nv+24

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "monthlySWCbulk")
        }

      #55
        if (isTRUE(prj_todos[["aon"]][["monthlySWAbulk"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "monthlySWAbulk")
          if (!exists("vwcmatric.mo")) vwcmatric.mo <- get_Response_aggL(swof["sw_vwcmatric"], tscale = "mo", scaler = 1, FUN = stats::weighted.mean, weights = layers_width, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)

          VWCcritsT <- SWPtoVWC(opt_agg[["SWPcrit_MPa"]], texture$sand.top, texture$clay.top)
          VWCcritsB <- if (length(bottomL) > 0 && !identical(bottomL, 0)) {
              SWPtoVWC(opt_agg[["SWPcrit_MPa"]], texture$sand.bottom, texture$clay.bottom)
            } else {
              rep(NA, opt_agg[["SWPcrit_N"]])
            }

          for (icrit in opt_agg[["SWPcrit_MPa"]]) {
            temp_top_mo <- 10 * sum(layers_width[topL]) * (vwcmatric.mo$top - VWCcritsT[icrit])
            temp_top_mean <- tapply(temp_top_mo, simTime2$month_ForEachUsedMonth, mean)
            temp_top_sd <- tapply(temp_top_mo, simTime2$month_ForEachUsedMonth, mean)

            if (length(bottomL) > 0 && !identical(bottomL, 0)) {
              temp_bottom_mo <- 10 * sum(layers_width[bottomL]) * (vwcmatric.mo$bottom - VWCcritsB[icrit])
              temp_bottom_mean <- tapply(temp_bottom_mo, simTime2$month_ForEachUsedMonth, mean)
              temp_bottom_sd <- tapply(temp_bottom_mo, simTime2$month_ForEachUsedMonth, mean)
            } else {
              temp_bottom_mo <- temp_bottom_mean <- temp_bottom_sd <- rep(NA, 12)
            }

            resMeans[nv+SFSW2_glovars[["st_mo"]]-1] <- ifelse(temp_top_mean > 0, temp_top_mean, 0)
            resSDs[nv+SFSW2_glovars[["st_mo"]]-1] <- ifelse(temp_top_mean > 0, temp_top_sd, 0)
            resMeans[nv+SFSW2_glovars[["st_mo"]]-1+12] <- ifelse(is.na(temp_bottom_mean) | temp_bottom_mean > 0, temp_bottom_mean, 0)
            resSDs[nv+SFSW2_glovars[["st_mo"]]-1+12] <- ifelse(is.na(temp_bottom_sd) | temp_bottom_sd > 0, temp_bottom_sd, 0)
            nv <- nv+24
          }

          rm(VWCcritsT, VWCcritsB, temp_top_mo, temp_top_mean, temp_top_sd, temp_bottom_mo, temp_bottom_mean, temp_bottom_sd)

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "monthlySWAbulk")
        }

      #56
        if (isTRUE(prj_todos[["aon"]][["monthlyTranspiration"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "monthlyTranspiration")
          if (!exists("transp.mo")) transp.mo <- get_Response_aggL(swof["sw_transp"], tscale = "mo", scaler = 10, FUN = sum, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)

          resMeans[nv+SFSW2_glovars[["st_mo"]]-1] <- tapply(transp.mo$top, simTime2$month_ForEachUsedMonth, mean)
          resSDs[nv+SFSW2_glovars[["st_mo"]]-1] <- tapply(transp.mo$top, simTime2$month_ForEachUsedMonth, stats::sd)
          resMeans[nv+SFSW2_glovars[["st_mo"]]-1+12] <- tapply(transp.mo$bottom, simTime2$month_ForEachUsedMonth, mean)
          resSDs[nv+SFSW2_glovars[["st_mo"]]-1+12] <- tapply(transp.mo$bottom, simTime2$month_ForEachUsedMonth, stats::sd)
          nv <- nv+24

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "monthlyTranspiration")
        }

      #57
        if (isTRUE(prj_todos[["aon"]][["monthlySoilEvaporation"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "monthlySoilEvaporation")
          if (!exists("Esoil.mo")) Esoil.mo <- get_Response_aggL(swof["sw_evsoil"], tscale = "mo", scaler = 10, FUN = sum, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)

          temp <- Esoil.mo$top + Esoil.mo$bottom
          resMeans[nv+SFSW2_glovars[["st_mo"]]-1] <- tapply(temp, simTime2$month_ForEachUsedMonth, mean)
          resSDs[nv+SFSW2_glovars[["st_mo"]]-1] <- tapply(temp, simTime2$month_ForEachUsedMonth, stats::sd)
          nv <- nv+12

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "monthlySoilEvaporation")
        }

      #58
        if (isTRUE(prj_todos[["aon"]][["monthlyAET"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "monthlyAET")
          if (!exists("AET.mo")) AET.mo <- get_AET_mo(runDataSC, isim_time)

          resMeans[nv+SFSW2_glovars[["st_mo"]]-1] <- tapply(AET.mo$val, simTime2$month_ForEachUsedMonth, mean)
          resSDs[nv+SFSW2_glovars[["st_mo"]]-1] <- tapply(AET.mo$val, simTime2$month_ForEachUsedMonth, stats::sd)
          nv <- nv+12

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "monthlyAET")
        }

      #59
        if (isTRUE(prj_todos[["aon"]][["monthlyPET"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "monthlyPET")
          if (!exists("PET.mo")) PET.mo <- get_PET_mo(runDataSC, isim_time)

          resMeans[nv+SFSW2_glovars[["st_mo"]]-1] <- tapply(PET.mo$val, simTime2$month_ForEachUsedMonth, mean)
          resSDs[nv+SFSW2_glovars[["st_mo"]]-1] <- tapply(PET.mo$val, simTime2$month_ForEachUsedMonth, stats::sd)
          nv <- nv+12

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "monthlyPET")
        }

      #59.2
        if (isTRUE(prj_todos[["aon"]][["monthlyVPD"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "monthlyVPD")
          if (!exists("temp.mo")) temp.mo <- get_Temp_mo(runDataSC, isim_time)
          if (!exists("vpd.mo")) vpd.mo <- get_VPD_mo(sc, temp.mo, xin = swRunScenariosData, st2 = simTime2)

          nv_new <- nv + 12
          resMeans[nv:(nv_new - 1)] <- tapply(vpd.mo$mean, simTime2$month_ForEachUsedMonth, mean)
          resSDs[nv:(nv_new - 1)] <- tapply(vpd.mo$mean, simTime2$month_ForEachUsedMonth, stats::sd)
          nv <- nv_new

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "monthlyVPD")
        }

      #60
        if (isTRUE(prj_todos[["aon"]][["monthlyAETratios"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "monthlyAETratios")
          if (!exists("AET.mo")) AET.mo <- get_AET_mo(runDataSC, isim_time)
          if (!exists("Esoil.mo")) Esoil.mo <- get_Response_aggL(swof["sw_evsoil"], tscale = "mo", scaler = 10, FUN = sum, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)
          if (!exists("transp.mo")) transp.mo <- get_Response_aggL(swof["sw_transp"], tscale = "mo", scaler = 10, FUN = sum, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)

          temp <- ifelse(AET.mo$val < SFSW2_glovars[["tol"]], 0, (transp.mo$top + transp.mo$bottom) / AET.mo$val)
          resMeans[nv+SFSW2_glovars[["st_mo"]]-1] <- tapply(temp, simTime2$month_ForEachUsedMonth, mean)
          resSDs[nv+SFSW2_glovars[["st_mo"]]-1] <- tapply(temp, simTime2$month_ForEachUsedMonth, stats::sd)

          temp <- ifelse(AET.mo$val < SFSW2_glovars[["tol"]], 0, (Esoil.mo$top + Esoil.mo$bottom) / AET.mo$val)
          resMeans[nv+SFSW2_glovars[["st_mo"]]-1+12] <- tapply(temp, simTime2$month_ForEachUsedMonth, mean)
          resSDs[nv+SFSW2_glovars[["st_mo"]]-1+12] <- tapply(temp, simTime2$month_ForEachUsedMonth, stats::sd)
          nv <- nv+24

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "monthlyAETratios")
        }

      #61
        if (isTRUE(prj_todos[["aon"]][["monthlyPETratios"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "monthlyPETratios")
          if (!exists("PET.mo")) PET.mo <- get_PET_mo(runDataSC, isim_time)
          if (!exists("Esoil.mo")) Esoil.mo <- get_Response_aggL(swof["sw_evsoil"], tscale = "mo", scaler = 10, FUN = sum, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)
          if (!exists("transp.mo")) transp.mo <- get_Response_aggL(swof["sw_transp"], tscale = "mo", scaler = 10, FUN = sum, x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)

          temp <- ifelse(PET.mo$val < SFSW2_glovars[["tol"]], 0, (transp.mo$top + transp.mo$bottom) / PET.mo$val)
          resMeans[nv+SFSW2_glovars[["st_mo"]]-1] <- tapply(temp, simTime2$month_ForEachUsedMonth, mean)
          resSDs[nv+SFSW2_glovars[["st_mo"]]-1] <- tapply(temp, simTime2$month_ForEachUsedMonth, stats::sd)

          temp <- ifelse(PET.mo$val < SFSW2_glovars[["tol"]], 0, (Esoil.mo$top + Esoil.mo$bottom) / PET.mo$val)
          resMeans[nv+SFSW2_glovars[["st_mo"]]-1+12] <- tapply(temp, simTime2$month_ForEachUsedMonth, mean)
          resSDs[nv+SFSW2_glovars[["st_mo"]]-1+12] <- tapply(temp, simTime2$month_ForEachUsedMonth, stats::sd)
          nv <- nv+24

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "monthlyPETratios")
        }


        #---Aggregation: Potential regeneration
        #regeneration: adjust_NorthSouth
      #62
        if (isTRUE(prj_todos[["aon"]][["dailyRegeneration_bySWPSnow"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "dailyRegeneration_bySWPSnow")
          if (!exists("swpmatric.dy.all")) swpmatric.dy.all <- list(val = -1/10*slot(slot(runDataSC, swof["sw_swp"]), "Day"))  #no vwcdy available!
          if (!exists("SWE.dy")) SWE.dy <- get_SWE_dy(runDataSC, isim_time)

          swp.surface <- swpmatric.dy.all$val[isim_time$index.usedy, 3]
          temp <- c(by(data = data.frame(swp.surface, SWE.dy$val),
            INDICES = simTime2$year_ForEachUsedDay_NSadj, FUN = regenerationThisYear_YN,
            params = opt_agg[["dailyRegeneration_bySWPSnow"]]))

          resMeans[nv] <- mean(temp)
          resSDs[nv] <- stats::sd(temp)
          nv <- nv + 1

          rm(swp.surface)

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "dailyRegeneration_bySWPSnow")
        }

        #Artemisia tridentata regeneration according to factor model (2012-02-15, drs), call for every regeneration species
        #adjust_NorthSouth: param$Doy_SeedDispersalStart0 must be set correctly\
      #63
        if (isTRUE(prj_todos[["aon"]][["dailyRegeneration_GISSM"]])) {
          nv0 <- nv

          # Schlaepfer, D.R., Lauenroth, W.K. & Bradford, J.B. (2014). Modeling regeneration responses of big sagebrush (Artemisia tridentata) to abiotic conditions. Ecol Model, 286, 66-77.
          print_debug(opt_verbosity, tag_simpidfid, "aggregating", "dailyRegeneration_GISSM")

          #---Access daily data, which do not depend on specific species parameters, i.e., start of season

          if (!exists("swpmatric.dy.all")) swpmatric.dy.all <- list(val = -1/10*slot(slot(runDataSC, swof["sw_swp"]), "Day"))  #no vwcdy available!
          temp.snow <- slot(slot(runDataSC, swof["sw_snow"]), "Day")
          temp.temp <- slot(slot(runDataSC, swof["sw_temp"]), "Day")
          TmeanJan <- mean(temp.temp[isim_time$index.usedy, 5][simTime2$month_ForEachUsedDay_NSadj == 1], na.rm = TRUE)  #mean January (N-hemisphere)/July (S-hemisphere) air temperature based on normal 'doy'
          temp.soiltemp <- slot(slot(runDataSC, swof["sw_soiltemp"]), "Day")
          if (inherits(temp.soiltemp, "try-error") || anyNA(temp.soiltemp[, -(1:2)]) || all(temp.soiltemp[, -(1:2)] == 0)) {
            use.soiltemp <- FALSE  #flag whether soil temperature output is available or not (and then air temperature is used instead of top soil temperature)
          } else {
            use.soiltemp <- TRUE  #currently we have only mean daily soil temperatures and not min/max which we need fo the model
          }

          #Loop through each species
          prev.Doy_SeedDispersalStart <- 0
          for (sp in seq_len(opt_agg[["GISSM_species_No"]])) {
            param <- data.frame(t(opt_agg[["GISSM_params"]][, sp]))

            #Regeneration year = RY: RYdoy = 1 == start of seed dispersal = start of 'regeneration year'
            temp <- param$Doy_SeedDispersalStart0 +
              param$SeedDispersalStart_DependencyOnMeanTempJanuary * TmeanJan
            Doy_SeedDispersalStart <- as.integer(max(round(temp, 0) %% 365, 1))

            moveByDays <- if (Doy_SeedDispersalStart > 1) {
                temp <- ISOdate(isim_time$useyrs[1] - 1, 12, 31, tz = "UTC") -
                        ISOdate(isim_time$useyrs[1] - 1, 1, 1, tz = "UTC") + 1 -
                        (Doy_SeedDispersalStart - 1)
                as.integer(max(c(as.numeric(temp) %% 365, 1)))
              } else {
                1L
              }

            #Calculate regeneration year dates
            et <- isim_time$no.usedy
            itail <- (et - moveByDays + 1):et
            if (isim_time[["startyr"]] > isim_time[["simstartyr"]]) {
              #start earlier to complete RY
              st <- isim_time$index.usedy[1]
              RY.index.usedy <- c((st - moveByDays):(st - 1), isim_time$index.usedy[-itail]) #index indicating which rows of the daily SOILWAT2 output is used
              RYyear_ForEachUsedDay <- simTime2$year_ForEachUsedDay  #'regeneration year' for each used day
              RYdoy_ForEachUsedDay <- simTime2$doy_ForEachUsedDay  #'doy of the regeneration year' for each used day

            } else {
              #start later to get a complete RY
              RY.index.usedy <- isim_time$index.usedy[-c(1:(Doy_SeedDispersalStart - 1), itail)]
              temp <- which(simTime2$year_ForEachUsedDay == simTime2$year_ForEachUsedDay[1])
              RYyear_ForEachUsedDay <- simTime2$year_ForEachUsedDay[-temp]
              RYdoy_ForEachUsedDay <- simTime2$doy_ForEachUsedDay[-temp]
            }
            RY.useyrs <- unique(RYyear_ForEachUsedDay)  #list of 'regeneration years' that are used for aggregation

            # normal year for each used 'doy of the regeneration year'
            RY_N_usedy <- length(RY.index.usedy)
            itail <- (RY_N_usedy - moveByDays + 1):RY_N_usedy
            year_ForEachUsedRYDay <- c(rep(isim_time$useyrs[1] - 1, moveByDays),
                                        RYyear_ForEachUsedDay[-itail])
            # normal doy for each used 'doy of the regeneration year'
            st <- isim_time$index.usedy[1]
            doy_ForEachUsedRYDay <- c((st - moveByDays):(st - 1),
                                      RYdoy_ForEachUsedDay[-itail])

            #Access daily data, the first time and afterwards only if Doy_SeedDispersalStart is different from value of previous species
            if (sp == 1 || Doy_SeedDispersalStart != prev.Doy_SeedDispersalStart) {
              swp <- swpmatric.dy.all$val[RY.index.usedy, 2 + ld, drop = FALSE]
              snow <- temp.snow[RY.index.usedy, 3]*10 #mm swe in snowpack
              airTminSnow <- ifelse(snow > 0, param$Temp_ExperiencedUnderneathSnowcover, temp.temp[RY.index.usedy, 4])
              airTmax <- temp.temp[RY.index.usedy, 3]
              if (use.soiltemp) {
                soilTmeanSnow <- ifelse(snow > 0, param$Temp_ExperiencedUnderneathSnowcover, temp.soiltemp[RY.index.usedy, 3])
                soilTminSnow <- ifelse(snow > 0, param$Temp_ExperiencedUnderneathSnowcover, temp.soiltemp[RY.index.usedy, 3])
                soilTmax <- temp.soiltemp[RY.index.usedy, 3]

              } else {
                soilTmeanSnow <- ifelse(snow > 0, param$Temp_ExperiencedUnderneathSnowcover, temp.temp[RY.index.usedy, 5])
                soilTminSnow <- airTminSnow
                soilTmax <- airTmax
              }
            }

            #----GERMINATION

            #---1. Germination periods: sequence of days with favorable conditions for germination defined by upper/lower limits
            #Maximal temperature for germination
            Germination_AtBelowTmax <- soilTmax <= param$Temp_MaximumForGermination

            #Minimal temperature for germination
            Germination_AtAboveTmin <- soilTminSnow >= param$Temp_MinimumForGermination

            #Minimum soil water for germination in relevant soil layer
            SoilLayers_RelevantToGermination <- SoilLayer_at_SoilDepth(param$SoilDepth_RelevantToGermination, layers_depth)
            if (length(SoilLayers_RelevantToGermination) == 1) {
              Germination_AtMoreThanTopSWPmin <- swp[, SoilLayers_RelevantToGermination] >= param$SWP_MinimumForGermination
              swp.TopMean <- swp[, SoilLayers_RelevantToGermination]
            } else {
              Germination_AtMoreThanTopSWPmin <- apply(swp[, SoilLayers_RelevantToGermination], MARGIN = 1, FUN = function(x) all(x >= param$SWP_MinimumForGermination))
              swp.TopMean <- apply(swp[, SoilLayers_RelevantToGermination], MARGIN = 1, FUN = mean, na.rm = TRUE)
            }

            #Put all limits together
            Germination_DuringFavorableConditions <- Germination_AtBelowTmax & Germination_AtAboveTmin & Germination_AtMoreThanTopSWPmin

            #---2. Time to germinate
            #for each day with favorable conditions, determine whether period of favorable conditions (resumed or reset if broken) is long enough for successful completion of germination under current mean conditions
            LengthDays_FavorableConditions <- unlist(lapply(RY.useyrs, FUN = calculate_DurationFavorableConditions,
                consequences.unfavorable = param$GerminationPeriods_0ResetOr1Resume,
                Germination_DuringFavorableConditions = Germination_DuringFavorableConditions,
                RYyear_ForEachUsedDay = RYyear_ForEachUsedDay))
            Germination_TimeToGerminate <- unlist(lapply(RY.useyrs, FUN = calculate_TimeToGerminate_modifiedHardegree2006NLR,
                Germination_DuringFavorableConditions = Germination_DuringFavorableConditions,
                LengthDays_FavorableConditions = LengthDays_FavorableConditions,
                RYyear_ForEachUsedDay = RYyear_ForEachUsedDay,
                soilTmeanSnow = soilTmeanSnow,
                swp.TopMean = swp.TopMean,
                TmeanJan = TmeanJan, param = param))

            Germination_RestrictedByTimeToGerminate <- rep(FALSE, RY_N_usedy)
            Germination_RestrictedByTimeToGerminate[Germination_DuringFavorableConditions & is.na(Germination_TimeToGerminate)] <- TRUE

            #---3. Successful germinations
            GerminationSuccess_Initiated <- !is.na(Germination_TimeToGerminate)
            germ.starts <- which(GerminationSuccess_Initiated)
            germ.durs <- Germination_TimeToGerminate[germ.starts] - 1
            if (param$GerminationPeriods_0ResetOr1Resume == 1) {
              germ.durs <- germ.durs + germination_wait_times(Germination_TimeToGerminate,
                LengthDays_FavorableConditions)
            }
            emergence.doys <- germ.starts + germ.durs #index of start of successful germinations + time to germinate (including wait time during unfavorable conditions if 'resume')
            Germination_Emergence <- rep(FALSE, RY_N_usedy)
            Germination_Emergence[emergence.doys] <- TRUE
            Germination_Emergence.doys <- rep(NA, RY_N_usedy)
            Germination_Emergence.doys[GerminationSuccess_Initiated] <- emergence.doys


            #----SEEDLING SURVIVAL

            #---1. Seedling survival periods:
            #  mortality = !survival: days with conditions which kill a seedling, defined by upper/lower limits
            #  growth: days with conditions which allows a seedling to grow (here, roots), defined by upper/lower limits
            SeedlingMortality_UnderneathSnowCover <- calculate_SeedlingMortality_ByCondition(kill.conditions = (snow > param$SWE_MaximumForSeedlingGrowth), max.duration.before.kill = param$Days_SnowCover_MaximumForSeedlingSurvival)
            SeedlingMortality_ByTmin <- calculate_SeedlingMortality_ByCondition(kill.conditions = (airTminSnow < param$Temp_MinimumForSeedlingSurvival), max.duration.before.kill = 0)
            SeedlingMortality_ByTmax <- calculate_SeedlingMortality_ByCondition(kill.conditions = (airTmax > param$Temp_MaximumForSeedlingSurvival), max.duration.before.kill = 0)
            SeedlingMortality_ByChronicSWPMax <- calculate_SeedlingMortality_ByCondition(kill.conditions = (swp > param$SWP_ChronicMaximumForSeedlingSurvival), max.duration.before.kill = param$Days_ChronicMaximumForSeedlingSurvival)
            SeedlingMortality_ByChronicSWPMin <- calculate_SeedlingMortality_ByCondition(kill.conditions = (swp < param$SWP_ChronicMinimumForSeedlingSurvival), max.duration.before.kill = param$Days_ChronicMinimumForSeedlingSurvival)
            SeedlingMortality_ByAcuteSWPMin <- calculate_SeedlingMortality_ByCondition(kill.conditions = (swp < param$SWP_AcuteMinimumForSeedlingSurvival), max.duration.before.kill = 0)

            SeedlingGrowth_AbsenceOfSnowCover <- (snow <= param$SWE_MaximumForSeedlingGrowth)
            SeedlingGrowth_AtAboveTmin <- (airTminSnow >= param$Temp_MinimumForSeedlingGrowth)
            SeedlingGrowth_AtBelowTmax <- (airTmax <= param$Temp_MaximumForSeedlingGrowth)

            #---2. Grow and kill the seedlings
            SeedlingSurvival_1stSeason <- Seedling_Starts <- Germination_Emergence #TRUE = seedling that germinated on that day and survives until end of season; FALSE = no germination or seedling dies during the first season
            SeedlingSurvival_1stSeason[] <- SeedlingSurvival_1stSeason # deep copy because Rcpp-version of get_KilledBySoilLayers changes in place which has otherwise side effects on Seedling_Starts and Germination_Emergence
            SeedlingMortality_CausesByYear <- matrix(0, nrow = length(RY.useyrs), ncol = 9)
            colnames(SeedlingMortality_CausesByYear) <- paste0("Seedlings1stSeason.Mortality.", c("UnderneathSnowCover", "ByTmin", "ByTmax", "ByChronicSWPMax", "ByChronicSWPMin", "ByAcuteSWPMin",
                    "DuringStoppedGrowth.DueSnowCover", "DuringStoppedGrowth.DueTmin", "DuringStoppedGrowth.DueTmax"))
            for (y in seq_along(RY.useyrs)) {#for each year
              index.thisYear <- RYyear_ForEachUsedDay == RY.useyrs[y]
              RYDoys_SeedlingStarts_ThisYear <- which(Seedling_Starts[index.thisYear])
              if (length(RYDoys_SeedlingStarts_ThisYear) > 0) {#if there are any germinations
                #init values for this year
                no.days <- sum(index.thisYear)
                thisYear_SeedlingMortality_UnderneathSnowCover <- SeedlingMortality_UnderneathSnowCover[index.thisYear]
                thisYear_SeedlingMortality_ByTmin <- SeedlingMortality_ByTmin[index.thisYear]
                thisYear_SeedlingMortality_ByTmax <- SeedlingMortality_ByTmax[index.thisYear]
                thisYear_SeedlingMortality_ByChronicSWPMax <- SeedlingMortality_ByChronicSWPMax[index.thisYear, , drop = FALSE]
                thisYear_SeedlingMortality_ByChronicSWPMin <- SeedlingMortality_ByChronicSWPMin[index.thisYear, , drop = FALSE]
                thisYear_SeedlingMortality_ByAcuteSWPMin <- SeedlingMortality_ByAcuteSWPMin[index.thisYear, , drop = FALSE]
                thisYear_SeedlingGrowth_AbsenceOfSnowCover <- SeedlingGrowth_AbsenceOfSnowCover[index.thisYear]
                thisYear_SeedlingGrowth_AtAboveTmin <- SeedlingGrowth_AtAboveTmin[index.thisYear]
                thisYear_SeedlingGrowth_AtBelowTmax <- SeedlingGrowth_AtBelowTmax[index.thisYear]

                for (sg_RYdoy in RYDoys_SeedlingStarts_ThisYear) {#for each seedling indexed by day of germination
                  #init values for this seedling and season
                  temp <- seq_len(no.days)
                  index.thisSeedlingSeason <- temp[temp > sg_RYdoy]
                  killed_byCauses_onRYdoy <- rep(NA, times = 6)  #book-keeping of mortality causes
                  names(killed_byCauses_onRYdoy) <- colnames(SeedlingMortality_CausesByYear)[1:6]
                  stopped_byCauses_onRYdoy <- rep(NA, times = 3)  #book-keeping of causes why growth stopped
                  names(stopped_byCauses_onRYdoy) <- colnames(SeedlingMortality_CausesByYear)[7:9]

                  #Establish days of growth ( = TRUE) and surviving, but no growth ( = FALSE)
                  thisSeedlingGrowing <- rep(TRUE, no.days)
                  if (sg_RYdoy > 1)
                    thisSeedlingGrowing[seq_len(sg_RYdoy - 1)] <- FALSE  #seedling germinated on sg_RYdoy, hence it cannot grow before germination day

                  #Check growth under above-ground conditions
                  #Snow cover
                  thisSeedlingGrowth_AbsenceOfSnowCover <- calculate_SuitableGrowthThisYear_UnderCondition(favorable.conditions = thisSeedlingGrowing & thisYear_SeedlingGrowth_AbsenceOfSnowCover, consequences.unfavorable = param$SeedlingGrowth_0StopOr1Resume)
                  temp <- !thisSeedlingGrowth_AbsenceOfSnowCover[index.thisSeedlingSeason]
                  if (any(temp))
                    stopped_byCauses_onRYdoy["Seedlings1stSeason.Mortality.DuringStoppedGrowth.DueSnowCover"] <- sg_RYdoy + which(temp)[1]
                  #Minimum temperature
                  thisSeedlingGrowth_AtAboveTmin <- calculate_SuitableGrowthThisYear_UnderCondition(favorable.conditions = thisSeedlingGrowing & thisYear_SeedlingGrowth_AtAboveTmin, consequences.unfavorable = param$SeedlingGrowth_0StopOr1Resume)
                  temp <- !thisSeedlingGrowth_AtAboveTmin[index.thisSeedlingSeason]
                  if (any(temp))
                    stopped_byCauses_onRYdoy["Seedlings1stSeason.Mortality.DuringStoppedGrowth.DueTmin"] <- sg_RYdoy + which(temp)[1]
                  #Maximum temperature
                  thisSeedlingGrowth_AtBelowTmax <- calculate_SuitableGrowthThisYear_UnderCondition(favorable.conditions = thisSeedlingGrowing & thisYear_SeedlingGrowth_AtBelowTmax, consequences.unfavorable = param$SeedlingGrowth_0StopOr1Resume)
                  temp <- !thisSeedlingGrowth_AtBelowTmax[index.thisSeedlingSeason]
                  if (any(temp))
                    stopped_byCauses_onRYdoy["Seedlings1stSeason.Mortality.DuringStoppedGrowth.DueTmax"] <- sg_RYdoy + which(temp)[1]
                  #Updated days of growth or surviving
                  thisSeedlingGrowing <- thisSeedlingGrowing & thisSeedlingGrowth_AbsenceOfSnowCover & thisSeedlingGrowth_AtAboveTmin & thisSeedlingGrowth_AtBelowTmax
                  thisSeedlingLivingButNotGrowing <- !thisSeedlingGrowing
                  if (sg_RYdoy > 1)
                    thisSeedlingLivingButNotGrowing[seq_len(sg_RYdoy - 1)] <- FALSE  #seedling germinated on sg_RYdoy, hence it cannot live before germination day

                  #Book-keeping survival under above-ground conditions
                  temp <- thisYear_SeedlingMortality_UnderneathSnowCover[index.thisSeedlingSeason]
                  if (any(temp))
                    killed_byCauses_onRYdoy["Seedlings1stSeason.Mortality.UnderneathSnowCover"] <- sg_RYdoy + which(temp)[1] - 1
                  temp <- thisYear_SeedlingMortality_ByTmin[index.thisSeedlingSeason]
                  if (any(temp))
                    killed_byCauses_onRYdoy["Seedlings1stSeason.Mortality.ByTmin"] <- sg_RYdoy + which(temp)[1] - 1
                  temp <- thisYear_SeedlingMortality_ByTmax[index.thisSeedlingSeason]
                  if (any(temp))
                    killed_byCauses_onRYdoy["Seedlings1stSeason.Mortality.ByTmax"] <- sg_RYdoy + which(temp)[1] - 1

                  #If not killed (yet) then grow and check survival below-ground
                  if (all(is.na(killed_byCauses_onRYdoy))) {
                    #Grow: estimate rooting depth for this seedling for each day of this year
                    thisSeedling_thisYear_RootingDepth <- rep(NA, times = no.days)
                    temp <- sum(thisSeedlingGrowing)
                    if (temp > 0) {
                      thisSeedlingGrowing_AgeDays <- seq_len(temp)
                      thisSeedlingGrowing_RootingDepth <- SeedlingRootingDepth(thisSeedlingGrowing_AgeDays, param$Seedling_SoilDepth.PO, param$Seedling_SoilDepth.K, param$Seedling_SoilDepth.r)
                      thisSeedling_thisYear_RootingDepth[thisSeedlingGrowing] <- thisSeedlingGrowing_RootingDepth
                      if (any(thisSeedlingLivingButNotGrowing, na.rm = TRUE)) {
                        #for days when growth stopped then copy relevant soil depth
                        stopg <- addDepths <- rle(thisSeedlingLivingButNotGrowing)
                        RYDoys_stopg <- c(1, cumsum(stopg$lengths))
                        for (p in seq_along(stopg$values)[stopg$values]) {
                          addDepths$values[p] <- if (is.na(thisSeedling_thisYear_RootingDepth[RYDoys_stopg[p]])) {
                              if (is.na(thisSeedling_thisYear_RootingDepth[1 + RYDoys_stopg[p+1]])) {
                                  param$Seedling_SoilDepth.K
                                } else {
                                  thisSeedling_thisYear_RootingDepth[1 + RYDoys_stopg[p+1]]
                                }
                            } else {
                              thisSeedling_thisYear_RootingDepth[RYDoys_stopg[p]]
                            }
                        }
                        RYDoys_addDepths <- inverse.rle(addDepths)
                        thisSeedling_thisYear_RootingDepth <- ifelse(RYDoys_addDepths > 0, RYDoys_addDepths, thisSeedling_thisYear_RootingDepth)
                      }

                    } else {
                      thisSeedling_thisYear_RootingDepth[thisSeedlingLivingButNotGrowing] <- param$Seedling_SoilDepth.PO/10
                    }
                    thisSeedling_thisYear_RootingSoilLayers <- SoilLayer_at_SoilDepth(thisSeedling_thisYear_RootingDepth, layers_depth)

                    #Check survival under chronic SWPMax
                    thisSeedling_thisYear_SeedlingMortality_ByChronicSWPMax <- get_KilledBySoilLayers(thisSeedling_thisYear_RootingSoilLayers, thisYear_SeedlingMortality_ByChronicSWPMax)
                    temp <- thisSeedling_thisYear_SeedlingMortality_ByChronicSWPMax[index.thisSeedlingSeason]
                    if (any(temp))
                      killed_byCauses_onRYdoy["Seedlings1stSeason.Mortality.ByChronicSWPMax"] <- sg_RYdoy + which(temp)[1] - 1
                    #Check survival under chronic SWPMin
                    thisSeedling_thisYear_SeedlingMortality_ByChronicSWPMin <- get_KilledBySoilLayers(thisSeedling_thisYear_RootingSoilLayers, thisYear_SeedlingMortality_ByChronicSWPMin)
                    temp <- thisSeedling_thisYear_SeedlingMortality_ByChronicSWPMin[index.thisSeedlingSeason]
                    if (any(temp))
                      killed_byCauses_onRYdoy["Seedlings1stSeason.Mortality.ByChronicSWPMin"] <- sg_RYdoy + which(temp)[1] - 1
                    #Check survival under acute SWPMin
                    thisSeedling_thisYear_SeedlingMortality_ByAcuteSWPMin <- get_KilledBySoilLayers(thisSeedling_thisYear_RootingSoilLayers, thisYear_SeedlingMortality_ByAcuteSWPMin)
                    temp <- thisSeedling_thisYear_SeedlingMortality_ByAcuteSWPMin[index.thisSeedlingSeason]
                    if (any(temp))
                      killed_byCauses_onRYdoy["Seedlings1stSeason.Mortality.ByAcuteSWPMin"] <- sg_RYdoy + which(temp)[1] - 1
                  }

                  #If killed then establish which factor killed first and if and how growth was stopped before kill
                  if (any(!is.na(killed_byCauses_onRYdoy))) {
                    kill.factor <- which.min(killed_byCauses_onRYdoy)
                    SeedlingMortality_CausesByYear[y, kill.factor] <- SeedlingMortality_CausesByYear[y, kill.factor] + 1
                    stop.factor <- which.min(stopped_byCauses_onRYdoy)
                    if (any(!is.na(stopped_byCauses_onRYdoy)) &&
                        killed_byCauses_onRYdoy[kill.factor] > stopped_byCauses_onRYdoy[stop.factor]) {
                      SeedlingMortality_CausesByYear[y, 6+stop.factor] <- SeedlingMortality_CausesByYear[y, 6+stop.factor] + 1
                    }

                    SeedlingSurvival_1stSeason <- setFALSE_SeedlingSurvival_1stSeason(
                      SeedlingSurvival_1stSeason, RYyear_ForEachUsedDay,
                      RY.useyrs, y, sg_RYdoy)
                  }
                }
              } else {#no germination during this year -> no seedlings to grow or die
                SeedlingMortality_CausesByYear[y, ] <- NA
              }
            }#end of year loop of seedling growth

            #---Aggregate output
            dat_gissm1 <- cbind(Germination_Emergence, SeedlingSurvival_1stSeason)
            dat_gissm2 <- cbind(!Germination_AtBelowTmax, !Germination_AtAboveTmin,
              !Germination_AtMoreThanTopSWPmin, !Germination_DuringFavorableConditions,
              Germination_RestrictedByTimeToGerminate)

            #Fraction of years with success
            index_RYuseyr <- unique(year_ForEachUsedRYDay) %in% isim_time$useyr
            res1.yr_v0 <- stats::aggregate(dat_gissm1, by = list(year_ForEachUsedRYDay), FUN = sum)
            res1.yr <- res1.yr_v0[index_RYuseyr, -1]
            stemp <- res1.yr > 0
            resMeans[nv:(nv+1)] <- apply(stemp, 2, mean, na.rm = TRUE)
            resSDs[nv:(nv+1)] <- apply(stemp, 2, stats::sd, na.rm = TRUE)
            #Periods with no successes
            rleGerm <- rle(stemp[, 1])
            if (any(!rleGerm$values))
              resMeans[(nv+2):(nv+4)] <- stats::quantile(rleGerm$lengths[!rleGerm$values],
                                                  probs = c(0.05, 0.5, 0.95), type = 7)
            rleSling <- rle(stemp[, 2])
            if (any(!rleSling$values))
              resMeans[(nv+5):(nv+7)] <- stats::quantile(rleSling$lengths[!rleSling$values],
                                                  probs = c(0.05, 0.5, 0.95), type = 7)
            #Mean number of days per year with success
            resMeans[(nv+8):(nv+9)] <- apply(res1.yr, 2, mean)
            resSDs[(nv+8):(nv+9)] <- apply(res1.yr, 2, stats::sd)
            #Days of year (in normal count) of most frequent successes among years: #toDoy <- function(x) sort(ifelse((temp <- x+Doy_SeedDispersalStart-1) > 365, temp-365, temp)) #convert to normal doys
            res1.dy <- stats::aggregate(dat_gissm1, by = list(doy_ForEachUsedRYDay), FUN = sum)
            resMeans[(nv+10):(nv+15)] <- get.DoyMostFrequentSuccesses(res1.dy, dat_gissm1)
            #Mean number of days when germination is restricted due to conditions
            res2.yr_v0 <- stats::aggregate(dat_gissm2, by = list(year_ForEachUsedRYDay), sum)
            res2.yr <- res2.yr_v0[index_RYuseyr, -1]
            resMeans[(nv+16):(nv+20)] <- apply(res2.yr, 2, mean)
            resSDs[(nv+16):(nv+20)] <- apply(res2.yr, 2, stats::sd)
            #Mean time to germinate in days
            res3.yr_v0 <- tapply(Germination_TimeToGerminate, year_ForEachUsedRYDay, mean, na.rm = TRUE)
            res3.yr <- res3.yr_v0[index_RYuseyr]
            resMeans[nv+21] <- mean(res3.yr, na.rm = TRUE)
            resSDs[nv+21] <- stats::sd(res3.yr, na.rm = TRUE)
            #Mean number of days per year of different types of mortalities
            resMeans[(nv+22):(nv+30)] <- apply(SeedlingMortality_CausesByYear, 2, mean, na.rm = TRUE) #if value == NA, then no germinations that year
            resSDs[(nv+22):(nv+30)] <- apply(SeedlingMortality_CausesByYear, 2, stats::sd, na.rm = TRUE) #if value == NA, then no germinations that year

            nv <- nv+31

            #---Aggregate time series output
            if (any(prj_todos[["otrace"]] == "dailyRegeneration_GISSM")) {
              #Table with data for every year
              res1.yr.doy <- t(simplify2array(by(dat_gissm1, INDICES = year_ForEachUsedRYDay,
                FUN = function(x) get.DoyMostFrequentSuccesses(x, dat_gissm1))))[isim_time$index.useyr, , drop = FALSE]

              res.yr <- data.frame(data.frame(res1.yr_v0, res2.yr_v0[, -1], res3.yr_v0)[index_RYuseyr, ], SeedlingMortality_CausesByYear, res1.yr.doy)
              temp.header2 <- c("DaysWith_GerminationSuccess", "DaysWith_SeedlingSurvival1stSeason",
                  "Days_GerminationRestrictedByTmax", "Days_GerminationRestrictedByTmin",
                  "Days_GerminationRestrictedBySWPmin", "Days_GerminationRestrictedByAnyCondition",
                  "Days_GerminationRestrictedByTimeToGerminate", "MeanDays_TimeToGerminate",
                  paste("Days", colnames(SeedlingMortality_CausesByYear), sep = "_"),
                  paste(rep(c("Start90%", "Median", "End90%"), times = 2),
                  rep(c("DoyMostFrequent_GerminationSuccess", "DoyMostFrequent_SeedlingSurvival1stSeason"),
                  each = 3), sep = "_"))
              colnames(res.yr) <- c("Year", temp.header2)
              utils::write.csv(res.yr, file = file.path(project_paths[["dir_out_traces"]],
                paste0("Scenario", formatC(sc-1, width = 2, format = "d", flag = "0"), "_",
                sim_scens[["id"]][sc], "_", i_label, "_", colnames(opt_agg[["GISSM_params"]])[sp],
                "_Regeneration.csv")))

              #Plot with data for every day
              grDevices::pdf(file = file.path(project_paths[["dir_out_traces"]], paste0("Scenario",
                formatC(sc-1, width = 2, format = "d", flag = "0"), "_", sim_scens[["id"]][sc],
                "_", i_label, "_", colnames(opt_agg[["GISSM_params"]])[sp],
                "_Regeneration.pdf")),
                width = max(4, 2*length(isim_time$index.useyr)), height = 4.5)

              op <- graphics::par(mar = c(1, 3, 0.1, 0.1), mgp = c(2, 0.5, 0), las = 1)
              ylim <- c(-17.5, max(max(snow, na.rm = TRUE), max(Germination_TimeToGerminate, na.rm = TRUE)))
              p.cex <- max(0.5, min(1, exp(-0.01 * ylim[2]) + 0.5))
              xp <- 1:length(snow) + Doy_SeedDispersalStart-1

              graphics::plot(xp, snow, type = "l", ylim = ylim, xlab = "Year", ylab = "SWE (mm), Time to germinate (days)", axes = FALSE)
              graphics::axis(1, pos = ylim[1], at = 365*(1:(length(isim_time$index.useyr))), labels = isim_time$useyr)
              graphics::axis(2, pos = graphics::par("usr")[1], at = (temp <- graphics::axTicks(2))[temp >= 0])
              graphics::lines(xp, Germination_TimeToGerminate, col = "red", type = "b", pch = 19, cex = p.cex/5)
              graphics::points(xp, ifelse(SeedlingSurvival_1stSeason, 0, NA), col = "green", pch = 19)
              x0.temp <- (temp <- data.frame(xp, ifelse(GerminationSuccess_Initiated, -7.5, NA)))[stats::complete.cases(temp), ]
              x1.temp <- (temp <- data.frame(Germination_Emergence.doys + Doy_SeedDispersalStart-1, ifelse(GerminationSuccess_Initiated, -2.5, NA)))[stats::complete.cases(temp), ]
              graphics::segments(x0 = x0.temp[, 1], y0 = x0.temp[, 2], x1 = x1.temp[, 1], y1 = x1.temp[, 2], col = "blue")
              graphics::points(xp, ifelse(Germination_RestrictedByTimeToGerminate, -10, NA), col = "black", pch = 4, cex = p.cex)
              graphics::points(xp, ifelse(!Germination_AtAboveTmin, -12.5, NA), col = grDevices::gray(0.3), pch = 4, cex = p.cex)
              graphics::points(xp, ifelse(!Germination_AtMoreThanTopSWPmin, -15, NA), col = grDevices::gray(0.7), pch = 4, cex = p.cex)
              graphics::mtext(i_label)
              graphics::legend("topright", legend = c("SWE", "Time to germinate", "Seedling survival", "Emergence", "Too short favorable conditions", "Too cold", "Too dry"),
                  bty = "n", lty = c(1, 1, -1, 1, -1, -1, -1), pch = c(-1, -1, 19, -1, 4, 4, 4), col = c("black", "red", "green", "blue", "black", grDevices::gray(0.3), grDevices::gray(0.7)), merge = TRUE)
              graphics::par(op)
              grDevices::dev.off()
            }

            #Prepare next species
            prev.Doy_SeedDispersalStart <- Doy_SeedDispersalStart
          }#end of species loop

          print_debugN(opt_verbosity, tag_simpidfid, prj_todos, nv - nv0, "dailyRegeneration_GISSM")
        }

        #---Aggregation: done with options

        #temporaly save aggregate data
        nv1 <- nv - 1
        if ((sim_size[["ncol_dbOut_overall"]] > 0 &&
          sim_size[["ncol_dbOut_overall"]] == nv1) ||
          sim_size[["ncol_dbOut_overall"]] == 0L) {

          resMeans[!is.finite(resMeans)] <- "NULL"
          resSDs[!is.finite(resSDs)] <- "NULL"
          temp1 <- paste0(c(P_id, resMeans[seq_len(nv1)]), collapse = ",")
          temp2 <- paste0(c(P_id, resSDs[seq_len(nv1)]), collapse = ",")

          print_debug(opt_verbosity, tag_simpidfid, "aggregations successful!")

        } else {
          print(paste0(tag_simpidfid, ": aggregation unsuccessful:",
            " incorrect number of aggregated variables: n = ", nv1,
            " instead of ", sim_size[["ncol_dbOut_overall"]]))
          tasks$aggregate[sc] <- 0L
          temp1 <- temp2 <- P_id
        }

        SQL <- paste0("INSERT INTO \"aggregation_overall_mean\" VALUES (", temp1, ");")
        try(DBI::dbExecute(dbTempFile, SQL), silent = !opt_verbosity[["verbose"]])

        SQL <- paste0("INSERT INTO \"aggregation_overall_sd\" VALUES (", temp2, ");")
        try(DBI::dbExecute(dbTempFile, SQL), silent = !opt_verbosity[["verbose"]])
      }

      #Daily Output
      if (prj_todos[["adaily"]][["N"]] > 0 && tasks$aggregate[sc] > 0L) {
        dailyList <- list()
        SQLc <- ""
        #aggregate for each response variable
        for (doi in seq_len(prj_todos[["adaily"]][["N"]])) {
          nv0 <- nv
          print_debug(opt_verbosity, tag_simpidfid, "daily aggregation", doi)

          if (!opt_behave[["resume"]] | (opt_behave[["resume"]] & !isdone.dailyAggs[doi, sc])) {
            #check to see if we are on SWA
            if (regexpr("SWAbulk", prj_todos[["adaily"]][["tag"]][doi]) > 0) {
              agg.resp <- "SWAbulk"
              temp <- sub("SWAbulkatSWPcrit", "", prj_todos[["adaily"]][["tag"]][doi])
              temp <- sub("kPa", "", temp)
              index.SWPcrit <- -as.numeric(temp) / 1000
            } else {
              agg.resp <- prj_todos[["adaily"]][["tag"]][doi]
            }

            agg.analysis <- switch(EXPR = agg.resp, AET = 1, Transpiration = 2,
              EvaporationSoil = 1, EvaporationSurface = 1, EvaporationTotal = 1,
              VWCbulk = 2, VWCmatric = 2, SWCbulk = 2, SWPmatric = 2, SWAbulk = 2,
              Snowpack = 1, Rain = 1, Snowfall = 1, Snowmelt = 1, SnowLoss = 1,
              Infiltration = 1, DeepDrainage = 1, PET = 1, TotalPrecipitation = 1,
              TemperatureMin = 1, TemperatureMax = 1, SoilTemperature = 2, Runoff = 1,
              Runon = 1)
            agg.no <- if (agg.analysis > 1) aggLs_no else 1L

            temp <- if (agg.analysis == 1) 1L else {
              if (opt_agg[["doy_slyrs"]][["do"]]) agg.no else SFSW2_glovars[["slyrs_maxN"]]}
            res.dailyMean <- res.dailySD <- rep(NA, temp * 366)

            # Unit scaling:
            #   - SWP: -bar => MPa (but, since calculated via VWC, needs be same as VWC)
            #   - VWC: # cm/cm -> m3/m3
            #   - default: cm => mm
            scaler <- switch(EXPR = prj_todos[["adaily"]][["tag"]][doi], SWPmatric = 1,
              VWCbulk = 1, VWCmatric = 1, TemperatureMin = 1, TemperatureMax = 1,
              SoilTemperature = 1, 10)

            #read in data unless Exclude_ClimateAmbient
            if (!Exclude_ClimateAmbient) {
              if (agg.resp == "EvaporationTotal") {
                temp1 <- slot(slot(runDataSC, swof["sw_evsoil"]), "Day")
                temp2 <- slot(slot(runDataSC, swof["sw_evapsurface"]), "Day")
              } else {#"VWCbulk", "VWCmatric", "SWCbulk", "SWPmatric", "SWAbulk"
                agg.file <- switch(EXPR = agg.resp,
                    AET = swof["sw_aet"],
                    Transpiration = swof["sw_transp"],
                    EvaporationSoil = swof["sw_evsoil"],
                    EvaporationSurface = swof["sw_evapsurface"],
                    VWCbulk = swof["sw_vwcbulk"],
                    VWCmatric = swof["sw_vwcmatric"],
                    SWCbulk = swof["sw_swcbulk"],
                    SWPmatric = swof["sw_vwcmatric"], #TODO: this was sw_vwc so can we just do sw_swpmatric?
                    SWAbulk = swof["sw_swcbulk"], #TODO: this was sw_swc so can we just do sw_swa?
                    Snowpack = swof["sw_snow"],
                    Rain = swof["sw_precip"],
                    Snowfall = swof["sw_precip"],
                    Snowmelt = swof["sw_precip"],
                    SnowLoss = swof["sw_precip"],
                    Infiltration = swof["sw_inf_soil"],
                    DeepDrainage = swof["sw_deepdrain"],
                    PET = swof["sw_pet"],
                    TotalPrecipitation = swof["sw_precip"],
                    TemperatureMin = swof["sw_temp"],
                    TemperatureMax = swof["sw_temp"],
                    SoilTemperature = swof["sw_soiltemp"],
                    Runoff = swof["sw_runoff"],
                    Runon = swof["sw_runoff"])
                temp1 <- slot(slot(runDataSC, agg.file), "Day")
              }

              #extract data and aggregate into layers if requested
              agg.dat <- NULL
              if (agg.analysis > 1) {
                #deal with soil layers: either each or 1-4 aggregated soil layers
                if (any(!is.na(match(agg.resp, c("VWCbulk", "VWCmatric", "SWPmatric", "SoilTemperature"))))) { #aggregate by functions that are weighted by depths of soil layers
                  agg.agg <- stats::weighted.mean
                  agg.w <- layers_width
                } else if (any(!is.na(match(agg.resp, c("Transpiration", "SWCbulk", "SWAbulk"))))) {#aggregate by simple functions
                  agg.agg <- sum
                  agg.w <- rep(0, times = length(layers_width))
                }
                for (al in 1:aggLs_no) {
                  if (length(aggLs[[al]]) > 1) {
                    agg.dat[[al]] <- apply(temp1[isim_time$index.usedy, 2 + aggLs[[al]]], 1, agg.agg, w = agg.w[aggLs[[al]]])
                  } else {
                    if (!(is.null(aggLs[[al]]) | length(aggLs[[al]]) == 0)) {
                      agg.dat[[al]]  <- temp1[isim_time$index.usedy, 2 + aggLs[[al]]]
                    }
                  }
                }

              } else {
                #no layers
                if (agg.resp %in% c("AET", "EvaporationSoil", "EvaporationSurface", "Snowpack",
                  "Rain", "Snowfall", "Snowmelt", "SnowLoss", "Infiltration", "DeepDrainage",
                  "PET", "TotalPrecipitation", "TemperatureMin", "TemperatureMax",
                  "Runoff", "Runon")) {
                  agg.column <- switch(EXPR = agg.resp, AET = 3,
                    EvaporationSoil = if ((colN <- ncol(temp1)) > 3) 3:colN else 3,
                    EvaporationSurface = 3, Snowpack = 3, Rain = 4, Snowfall = 5,
                    Snowmelt = 6, SnowLoss = 7, Infiltration = 3, DeepDrainage = 3,
                    PET = 3, TotalPrecipitation = 3, TemperatureMin = 4,
                    TemperatureMax = 3, Runoff = 4:5, Runon = 6)

                  agg.dat[[1]] <- if (length(agg.column) > 1) {
                      apply(temp1[isim_time$index.usedy, agg.column], 1, sum)
                    } else {
                      temp1[isim_time$index.usedy, agg.column]
                    }
                }
                if (agg.resp == "EvaporationTotal") {
                  if ((colN <- ncol(temp1)) > 3) {
                    agg.dat[[1]] <- apply(temp1[isim_time$index.usedy, 3:colN], 1, sum) + temp2[isim_time$index.usedy, 3]
                  } else {
                    agg.dat[[1]] <- temp1[isim_time$index.usedy, 3] + temp2[isim_time$index.usedy, 3]
                  }
                }
              }


              #calculate mean/stats::sd daily values
              for (al in 1:agg.no) {
                ir <- (al - 1) * 366 + 1:366
                res.dailyMean[ir] <- stats::aggregate(scaler * agg.dat[[al]], by = list(simTime2$doy_ForEachUsedDay), FUN = mean)[, 2]
                if (agg.resp == "SWPmatric") { ##post-aggregate calculation of SWP: convert VWC to SWP
                  res.dailyMean[ir] <- VWCtoSWP(res.dailyMean[ir], textureDAgg$sand[al], textureDAgg$clay[al])
                  res.dailySD[ir] <- 0 #was NA now 0
                } else {
                  res.dailySD[ir] <- stats::aggregate(scaler * agg.dat[[al]], by = list(simTime2$doy_ForEachUsedDay), FUN = stats::sd)[, 2]
                }
              }

              #post-aggregate calculation of SWA based on SWC for each SWPcrit
              if (agg.resp == "SWAbulk") {
                swc.swpcrit.layers <- layers_width * 10 * SWPtoVWC(index.SWPcrit, sand, clay)

                for (al in 1:agg.no) {
                  ir <- (al - 1) * 366 + 1:366

                  if (length(aggLs[[al]]) > 1) {
                    swc.swpcrit <- sum(swc.swpcrit.layers[aggLs[[al]]])
                  } else {
                    swc.swpcrit <- swc.swpcrit.layers[aggLs[[al]]]
                  }
                  res.dailyMean[ir] <- ifelse((temp.res <- res.dailyMean[ir] - swc.swpcrit) > 0, temp.res, 0)  #stats::sd is same as for SWC
                }
              }
            }

            #temporary save daily data
            res.dailyMean[!is.finite(res.dailyMean)] <- "NULL"
            res.dailySD[!is.finite(res.dailySD)] <- "NULL"

            if (agg.analysis > 1) {
              temp1 <- paste0("(", sapply(seq_len(agg.no), function(x) {
                  ids <- seq_len(366) + (x - 1) * 366
                  paste0(P_id, ", ", x, ", ", paste0(res.dailyMean[ids], collapse = ","))
                }), ")")

              temp2 <- paste0("(", sapply(seq_len(agg.no), function(x) {
                  ids <- seq_len(366) + (x - 1) * 366
                  paste0(P_id, ", ", x, ", ", paste0(res.dailySD[ids], collapse = ","))
                }), ")")

            } else { #no layers
              temp1 <- paste0("(", P_id, ", ", paste(res.dailyMean, collapse = ","), ")")
              temp2 <- paste0("(", P_id, ", ", paste(res.dailySD, collapse = ","), ")")
            }

            SQL <- paste0("INSERT INTO \"aggregation_doy_",
              prj_todos[["adaily"]][["tag"]][doi], "_Mean\" VALUES ", temp1, ";")
            try(DBI::dbExecute(dbTempFile, SQL), silent = !opt_verbosity[["verbose"]])

            SQL <- paste0("INSERT INTO \"aggregation_doy_",
              prj_todos[["adaily"]][["tag"]][doi], "_SD\" VALUES ", temp2, ";")
            try(DBI::dbExecute(dbTempFile, SQL), silent = !opt_verbosity[["verbose"]])

          }#end if resume
        }#doi loop
      }#end if daily output

      # Determine success of 'aggregate' section
      if (tasks$aggregate[sc] > 0L && length(SQL) > 0) {
        tasks$aggregate[sc] <- 2L
      }

    } #end if do aggregate

  } #end loop through scenarios

  if (any(tasks$aggregate == 0L)) {
    print(paste0(tag_simfid, ": not all aggregation results successful with",
      paste(tasks$aggregate, collapse = "-")))
  }

  delta.do_OneSite <- round(difftime(Sys.time(), t.do_OneSite, units = "secs"), 2)
  status <- all(unlist(tasks) != 0)

  if (!(SFSW2_glovars[["p_has"]] && SFSW2_glovars[["p_type"]] == "mpi")) {
    temp <- dbWork_update_job(project_paths[["dir_out"]], i_sim,
      status = if (status) "completed" else "failed", time_s = delta.do_OneSite,
      verbose = opt_verbosity[["print.debug"]])
  }

  if (status) {
    #ETA estimation
    times <- dbWork_timing(project_paths[["dir_out"]])

    if (opt_verbosity[["verbose"]]) {
      n <- length(times) - 1

      temp <- paste0("rSFSW2's ", temp_call, ": ", tag_simfid, ": completed in ",
        delta.do_OneSite, " ", units(delta.do_OneSite), "; simulation project is ",
        round(n / sim_size[["runsN_job"]] * 100, 2), "% complete")

      if (opt_verbosity[["print.eta"]]) {
        deta <- round(ceiling((sim_size[["runsN_job"]] - n) / SFSW2_glovars[["p_workersN"]]) *
          sapply(list(mean, stats::sd), function(f) f(times, na.rm = TRUE)))
        pi95 <- deta[2] * sqrt(1 + 1 / n) * {if (n > 1) stats::qt(0.975, n) else NA}# 95% prediction interval
        pi95 <- if (is.na(pi95)) "NA" else if (pi95 > 3600) {
            paste(round(pi95 / 3600), "h")
          } else if (pi95 > 60) {
            paste(round(pi95 / 60), "min")
          } else {
            paste(round(pi95), "s")
          }
        temp <- paste0(temp, " with ETA (mean plus/minus 95%-PI) = ",
                      Sys.time() + deta[1], " +/- ", pi95)
      }

      print(temp)
    }

  } else {
    print(paste0(tag_funid, ": unsuccessful after ", delta.do_OneSite, " ",
      units(delta.do_OneSite), " with status of tasks = "))
    print(unlist(sapply(tasks, table)))
  }

  on.exit()

  as.integer(status)
} #end do_OneSite()


#' Run a rSFSW2 simulation experiment
#' @export
run_simulation_experiment <- function(sim_size, SFSW2_prj_inputs, MoreArgs) {

  runs.completed <- 0

  if (MoreArgs[["opt_verbosity"]][["verbose"]]) {
    t1 <- Sys.time()
    temp_call <- shQuote(match.call()[1])
    print(paste0("rSFSW2's ", temp_call, ": started at ", t1, " for ",
      MoreArgs[["sim_size"]][["runsN_todo"]], " out of ",
      MoreArgs[["sim_size"]][["runsN_job"]], " runs on ",
      SFSW2_glovars[["p_workersN"]], " cores"))

    on.exit({print(paste0("rSFSW2's ", temp_call, ": ended after ",
      round(difftime(Sys.time(), t1, units = "secs"), 2), " s for ", runs.completed,
      " runs")); cat("\n")}, add = TRUE)
  }

  i_sites <- it_site(MoreArgs[["sim_size"]][["runIDs_todo"]],
    MoreArgs[["sim_size"]][["runsN_master"]])

  #--- call the simulations depending on parallel backend
  if (SFSW2_glovars[["p_has"]]) {
    unlink(SFSW2_glovars[["lockfile"]], recursive = TRUE)


    if (identical(SFSW2_glovars[["p_type"]], "mpi")) {

      Rmpi::mpi.remote.exec(cmd = dbW_setConnection_local,
        dbFilePath = MoreArgs[["fnames_in"]][["fdbWeather"]])
      on.exit(Rmpi::mpi.bcast.cmd(cmd = dbW_disconnectConnection_local), add = TRUE)

      Rmpi::mpi.bcast.cmd(cmd = mpi_work,
        verbose = MoreArgs[["opt_verbosity"]][["print.debug"]])

      junk <- 0L
      closed_workers <- 0L
      runs.completed <- 1L

      while (closed_workers < SFSW2_glovars[["p_workersN"]]) {

      tryCatch({

        if (MoreArgs[["opt_verbosity"]][["print.debug"]]) {
          print(paste(Sys.time(), ": MPI-master is waiting for workers to communicate"))
        }

        complete <- Rmpi::mpi.recv.Robj(Rmpi::mpi.any.source(), Rmpi::mpi.any.tag())
        complete_info <- Rmpi::mpi.get.sourcetag()
        worker_id <- complete_info[1]
        tag_from_worker <- complete_info[2] # see ?mpi_work for interpretation of tags

        if (MoreArgs[["opt_verbosity"]][["print.debug"]]) {
          print(paste(Sys.time(), ": MPI-master has received communication from worker",
            worker_id, "with tag", tag_from_worker))
        }

        if (tag_from_worker == 1L) {
          has_time_to_simulate <- (difftime(Sys.time(), MoreArgs[["t_job_start"]], units = "secs") +
            MoreArgs[["opt_parallel"]][["opt_job_time"]][["one_sim_s"]]) <
            MoreArgs[["opt_parallel"]][["opt_job_time"]][["wall_time_s"]]

          # worker is ready for a task. Give it the next task, or tell it tasks
          # are done if there are none.
          if ((runs.completed <= MoreArgs[["sim_size"]][["runsN_todo"]]) && has_time_to_simulate) {
            # Send a task, and then remove it from the task list
            t.do_OneSite <- Sys.time()

            i_sim <- MoreArgs[["sim_size"]][["runIDs_todo"]][runs.completed]
            success <- dbWork_update_job(MoreArgs[["project_paths"]][["dir_out"]],
              runID = i_sim, status = "inwork",
              verbose = MoreArgs[["opt_verbosity"]][["print.debug"]])
            # TODO: do something on failure of 'dbWork_update_job'

            i_site <- i_sites[runs.completed]
            dataForRun <- list(do_OneSite = TRUE, i_sim = i_sim,
              i_SWRunInformation = SFSW2_prj_inputs[["SWRunInformation"]][i_site, ],
              i_sw_input_soillayers = SFSW2_prj_inputs[["sw_input_soillayers"]][i_site, ],
              i_sw_input_treatments = SFSW2_prj_inputs[["sw_input_treatments"]][i_site, ],
              i_sw_input_cloud = SFSW2_prj_inputs[["sw_input_cloud"]][i_site, ],
              i_sw_input_prod = SFSW2_prj_inputs[["sw_input_prod"]][i_site, ],
              i_sw_input_site = SFSW2_prj_inputs[["sw_input_site"]][i_site, ],
              i_sw_input_soils = SFSW2_prj_inputs[["sw_input_soils"]][i_site, ],
              i_sw_input_weather = SFSW2_prj_inputs[["sw_input_weather"]][i_site, ],
              i_sw_input_climscen = SFSW2_prj_inputs[["sw_input_climscen"]][i_site, ],
              i_sw_input_climscen_values = SFSW2_prj_inputs[["sw_input_climscen_values"]][i_site, ],
              SimParams = MoreArgs)

            if (MoreArgs[["opt_verbosity"]][["print.debug"]]) {
              print(paste(Sys.time(), ": MPI-master is sending worker", worker_id, "task",
                MoreArgs[["sim_size"]][["runIDs_todo"]][runs.completed]))
            }

            # Tell worker that this communication contains a task
            Rmpi::mpi.send.Robj(dataForRun, dest = worker_id, tag = 1L)
            runs.completed <- runs.completed + 1L

          } else {
            # Tell worker to shut down because all work completed or run out of walltime
            Rmpi::mpi.send.Robj(junk, dest = worker_id, tag = 2L)
          }

        } else if (tag_from_worker == 2L) {
          # Worker has sent results back to master
          if (MoreArgs[["opt_verbosity"]][["print.debug"]]) {
            print(paste(Sys.time(), ": MPI-master received results from worker", worker_id,
              paste(complete, collapse = ", ")))
          }

          # Invoke checkpoint on dbWork in an attempt to avoid checkpoint starvation
          #dbWork_checkpoint(path = MoreArgs[["project_paths"]][["dir_out"]],
          #  mode = "PASSIVE", failure = "silent")

        } else if (tag_from_worker == 3L) {
          # A worker has closed down.
          closed_workers <- closed_workers + 1L
          if (MoreArgs[["opt_verbosity"]][["print.debug"]]) {
            print(paste(Sys.time(), ": MPI-master was notified that worker", worker_id,
              "shut down."))
          }

        } else if (tag_from_worker == 4L) {
          #The worker had a problem
          print(paste(Sys.time(), ": MPI-master was notified that worker", worker_id,
            "failed with task:", paste(complete, collapse = ", "), "-- storing info",
            "in file 'MPI_ProblemRuns.tab'."))

          ftemp <- file.path(MoreArgs[["project_paths"]][["dir_out"]],
            "MPI_ProblemRuns.tab")
          if (!file.exists(ftemp)) {
            cat("Worker, Run, Error", file = ftemp, sep = "\n")
          }
          cat(paste(worker_id, complete$i, complete$r, sep = "\t"), file = ftemp,
            append = TRUE, sep = "\n")

        } else {
          # We'll just ignore any unknown message from worker
          print(paste(Sys.time(), ": MPI-master received tag =", tag_from_worker,
            "from worker", worker_id, "but doesn't know what this means."))
        }

        if (tag_from_worker %in% c(2L, 4L)) {
          temp <- dbWork_update_job(MoreArgs[["project_paths"]][["dir_out"]],
            runID = complete[["i"]],
            status = if (complete[["status"]]) "completed" else "failed",
            time_s = complete[["time_s"]],
            verbose = MoreArgs[["opt_verbosity"]][["print.debug"]])
        }

      }, interrupt = function(interrupt) {
        print(paste(Sys.time(), ": MPI-master received user interruption 'ctrl-c' and",
          "is shutting down workers -- this may take a short while."))
        print(interrupt)
      })
      }
    }


    if (identical(SFSW2_glovars[["p_type"]], "socket")) {

      parallel::clusterCall(SFSW2_glovars[["p_cl"]],
        fun = rSOILWAT2::dbW_setConnection,
        dbFilePath = MoreArgs[["fnames_in"]][["fdbWeather"]])
      on.exit(parallel::clusterEvalQ(SFSW2_glovars[["p_cl"]],
        rSOILWAT2::dbW_disconnectConnection()), add = TRUE)

#TODO: It seems like a bad hack to make this work without exporting the full data.frames
# (e.g., SFSW2_prj_inputs[["SWRunInformation"]], SFSW2_prj_inputs[["sw_input_soillayers"]],
# ...) to the workers. clusterLapplyLB does not work because do_OneSite has two indices
# (i.e., i_sim and i_site). clusterMap operates on elements (i.e., columns of data.frames);
# hence, I use split() to convert the data.frames to lists where the elements correspond
# to the rows.

      temp_ids <- cbind(i_sim = MoreArgs[["sim_size"]][["runIDs_todo"]], i_site = i_sites)
      temp_seqs <- seq_along(MoreArgs[["sim_size"]][["runIDs_todo"]])

      runs.completed <- parallel::clusterMap(SFSW2_glovars[["p_cl"]],
        fun = do_OneSite,
        i_sim = temp_ids[, "i_sim"],
        i_SWRunInformation = split(SFSW2_prj_inputs[["SWRunInformation"]][temp_ids[, "i_site"], ], temp_seqs),
        i_sw_input_soillayers = split(SFSW2_prj_inputs[["sw_input_soillayers"]][temp_ids[, "i_site"], ], temp_seqs),
        i_sw_input_treatments = split(SFSW2_prj_inputs[["sw_input_treatments"]][temp_ids[, "i_site"], ], temp_seqs),
        i_sw_input_cloud = split(SFSW2_prj_inputs[["sw_input_cloud"]][temp_ids[, "i_site"], ], temp_seqs),
        i_sw_input_prod = split(SFSW2_prj_inputs[["sw_input_prod"]][temp_ids[, "i_site"], ], temp_seqs),
        i_sw_input_site = split(SFSW2_prj_inputs[["sw_input_site"]][temp_ids[, "i_site"], ], temp_seqs),
        i_sw_input_soils = split(SFSW2_prj_inputs[["sw_input_soils"]][temp_ids[, "i_site"], ], temp_seqs),
        i_sw_input_weather = split(SFSW2_prj_inputs[["sw_input_weather"]][temp_ids[, "i_site"], ], temp_seqs),
        i_sw_input_climscen = split(SFSW2_prj_inputs[["sw_input_climscen"]][temp_ids[, "i_site"], ], temp_seqs),
        i_sw_input_climscen_values = split(SFSW2_prj_inputs[["sw_input_climscen_values"]][temp_ids[, "i_site"], ], temp_seqs),
        MoreArgs = list(SimParams = MoreArgs),
        RECYCLE = FALSE, SIMPLIFY = FALSE, USE.NAMES = FALSE, .scheduling = "dynamic")

      runs.completed <- length(unlist(runs.completed))
    }

    clean_SFSW2_cluster()


  } else { #call the simulations in serial

    rSOILWAT2::dbW_setConnection(MoreArgs[["fnames_in"]][["fdbWeather"]])
    on.exit(rSOILWAT2::dbW_disconnectConnection(), add = TRUE)

    runs.completed <- lapply(seq_along(MoreArgs[["sim_size"]][["runIDs_todo"]]),
      function(i) {
        i_site <- i_sites[i]
        do_OneSite(i_sim = MoreArgs[["sim_size"]][["runIDs_todo"]][i],
          i_SWRunInformation = SFSW2_prj_inputs[["SWRunInformation"]][i_site, ],
          i_sw_input_soillayers = SFSW2_prj_inputs[["sw_input_soillayers"]][i_site, ],
          i_sw_input_treatments = SFSW2_prj_inputs[["sw_input_treatments"]][i_site, ],
          i_sw_input_cloud = SFSW2_prj_inputs[["sw_input_cloud"]][i_site, ],
          i_sw_input_prod = SFSW2_prj_inputs[["sw_input_prod"]][i_site, ],
          i_sw_input_site = SFSW2_prj_inputs[["sw_input_site"]][i_site, ],
          i_sw_input_soils = SFSW2_prj_inputs[["sw_input_soils"]][i_site, ],
          i_sw_input_weather = SFSW2_prj_inputs[["sw_input_weather"]][i_site, ],
          i_sw_input_climscen = SFSW2_prj_inputs[["sw_input_climscen"]][i_site, ],
          i_sw_input_climscen_values = SFSW2_prj_inputs[["sw_input_climscen_values"]][i_site, ],
          SimParams = MoreArgs)
      }
    )
    runs.completed <- length(unlist(runs.completed))
  }

  runs.completed
}
