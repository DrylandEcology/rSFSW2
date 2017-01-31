#' @export
simulate_SOILWAT2_experiment <- function(actions, opt_behave, opt_prepare, opt_sim,
  req_scens, req_out, opt_agg, project_paths, fnames_in, fnames_out, sim_space,
  opt_parallel, opt_chunks, opt_job_time, opt_verbosity) {

#---------------------------------------------------------------------------------------#
#------------------------PREPARE SOILWAT2 SIMULATIONS
  t_job_start <- Sys.time()

  if (opt_verbosity[["verbose"]])
    print(paste("SWSF is executed for:", sQuote(basename(project_paths[["dir_prj"]])),
      "and started at", t_job_start))

  ow_prev <- set_options_warn_error(opt_verbosity[["debug.warn.level"]],
    opt_verbosity[["debug.dump.objects"]], project_paths[["dir_prj"]])

  dir_safe_create(project_paths)

  init_timer(fnames_out[["timerfile"]])


  #--- Update simulation time
  sim_time <- setup_simulation_time(sim_time, add_st2 = TRUE,
    adjust_NS = opt_agg[["adjust_NorthSouth"]])

  #--- Determine scenario names
  sim_scens <- setup_scenarios(req_scens, sim_time[["future_yrs"]])

  #--- Determine todos for simulation project
  prj_todos <- list(
    actions = actions,
    use_SOILWAT2 = any(c("create", "execute", "aggregate") %in% actions),
    # output maps of input variables
    map_input = req_out[["map_vars"]],
    # output aggregate overall
    aon = convert_to_todo_list(req_out[["overall_out"]]),
    # output aggregate daily
    adaily = setup_mean_daily_output_requests(req_out[["mean_daily"]], opt_agg),
    # output daily traces
    otrace = req_out[["traces"]],
    # prior calculations
    pcalcs = convert_to_todo_list(opt_prepare[["prior_calculations"]]),
    # external data extraction
    exinfo = convert_to_todo_list(opt_prepare[["req_data"]])
  )

  #--- Import data
  fnames_in <- complete_with_defaultpaths(project_paths, fnames_in)
  do_check_include <- process_inputs(project_paths, fnames_in, opt_behave[["use_preprocin"]],
    verbose = opt_verbosity[["verbose"]])
  #TODO (drs): do not load into globalenv() - bad practice for a package:
  # 'load' annihilates all objects in globalenv() with the same names !
  load(file = fnames_in[["fpreprocin"]], envir = globalenv())

  #--- Update todo list
  prj_todos <- c(prj_todos, list(
    ex_besides_weather = {
      temp <- !grepl("GriddedDailyWeather", names(prj_todos[["exinfo"]]))
      any(as.logical(prj_todos[["exinfo"]])[temp])},

    need_cli_means = prj_todos[["use_SOILWAT2"]] && (
      any(sw_input_climscen_values_use) ||
      prj_todos[["pcalcs"]][["EstimateConstantSoilTemperatureAtUpperAndLowerBoundaryAsMeanAnnualAirTemperature"]] ||
      sw_input_site_use["SoilTempC_atLowerBoundary"] ||
      sw_input_site_use["SoilTempC_atUpperBoundary"] ||
      prj_todos[["pcalcs"]][["EstimateInitialSoilTemperatureForEachSoilLayer"]] ||
      any(create_treatments == "PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996") ||
      any(create_treatments == "AdjMonthlyBioMass_Temperature") ||
      any(create_treatments == "AdjMonthlyBioMass_Precipitation") ||
      any(create_treatments == "Vegetation_Biomass_ScalingSeason_AllGrowingORNongrowing")),

    use_sim_spatial = any(prj_todos[["actions"]] == "map_input") ||
      prj_todos[["ExtractSoilDataFromCONUSSOILFromSTATSGO_USA"]] ||
      prj_todos[["ExtractSoilDataFromISRICWISEv12_Global"]] ||
      prj_todos[["ExtractElevation_NED_USA"]] ||
      prj_todos[["ExtractElevation_HWSD_Global"]] ||
      prj_todos[["ExtractSkyDataFromNOAAClimateAtlas_USA"]] ||
      prj_todos[["ExtractSkyDataFromNCEPCFSR_Global"]],

    wipe_dbOut = opt_out[["wipe_dbOutput"]] &&
      !(length(prj_todos[["actions"]]) == 1 && prj_todos[["actions"]] == "ensemble")
  ))

  #--- Update output aggregation options
  opt_agg <- setup_aggregation_options(opt_agg, GISSM_species_No = GISSM_species_No,
    GISSM_params = GISSM_params)

  #--- Determine size of simulation runs
  sim_size <- determine_simulation_size(SWRunInformation, include_YN,
    sw_input_experimentals, sim_scens)

  #--- Setup/connect to dbWork and determine which runs need to be done
  temp <- setup_dbWork(project_paths[["dir_out"]], sim_size, include_YN,
    opt_behave[["resume"]])
  if (!temp)
    stop("Work database failed to setup or an existing one is from a different",
      "simulation design")

  sim_size[["runIDs_todo"]] <- dbWork_todos(project_paths[["dir_out"]]) # elements of runIDs_total
  sim_size[["runsN_todo"]] <- length(sim_size[["runIDs_todo"]])

  #--- Spatial setup of simulations
  sim_space <- setup_spatial_simulation(SWRunInformation, sim_space, sim_size,
    fnames_in[["fsimraster"]], prj_todos[["use_sim_spatial"]])

  #--- Determine requested ensembles across climate scenarios
  temp <- update_scenarios_with_ensembles(sim_scens, sim_time, prj_todos)
  prj_todos[["do_ensembles"]] <- temp[["do_ensembles"]]
  sim_scens <- temp[["sim_scens"]]


############################################


#append treatment information to the aggregated output in addition to selected Index_RunInformation
Index_RunInformation_Treatments <- NULL
if (length(create_treatments) > 0) {
	Index_RunInformation_Treatments <- match(create_treatments, names(sw_input_treatments))
}





#---------------------------------------------------------------------------------------#
#------------------------SET UP PARALLELIZATION
#used in: GriddedDailyWeatherFromNCEPCFSR_Global, external dataset extractions, loop calling do_OneSite, and ensembles

opt_parallel <- setup_SWSF_cluster(opt_parallel, opt_verbosity, dir_out = project_paths[["dir_prj"]])

if (!identical(opt_parallel[["parallel_backend"]], "mpi")) {
  # Only enforce wall-time on MPI systems
  opt_job_time[["wall_time_s"]] <- Inf
}
#---------------------------------------------------------------------------------------#


#------------------------FUNCTIONS FOR NCEP/CFSR DATA
if (prj_todos[["GriddedDailyWeatherFromNCEPCFSR_Global"]] ||
  prj_todos[["ExtractSkyDataFromNCEPCFSR_Global"]]) {

	writeLines(c("'NCEPCFSR' extractions: make sure the following conditions are met:",
		" 	1) C code for 'cfsr_convert' is located in directory 'dir.cfsr.code'",
		"	2) Compiled 'wgrib2' executable is located in directory 'dir.cfsr.code' or '/opt/local/bin/' & have it located in the same directory as cfsr_convert.  Instructions for how to compile 'wgrib2' can be found in the 'cfsr_convert.c'. The code of wgrib2 is available from http://www.cpc.ncep.noaa.gov/products/wesley/wgrib2/",
		"	3) Appropriate grib files (the data) are located in directory 'dir.cfsr.data'.  Info about the gribfiles is in 'cfsr_convert.c'"))

	#daily data (http://rda.ucar.edu/datasets/ds093.1/): ds093.1 NCEP Climate Forecast System Reanalysis (CFSR) Selected Hourly Time-Series Products, January 1979 to December 2010, 0.313-deg: 6-hourly
	#- maximum temperature: 2m above ground (Kelvin): 6-hour period
	#	-> tmax.gdas.yyyymm.grb2 --> max of 4 values per day
	#- minimum temperature: 2m above ground (Kelvin): 6-hour period
	#	-> tmin.gdas.yyyymm.grb2 --> max of 4 values per day
	#- precipitation rate: ground or water surface (kg m-2 s-1): 6-hour average
	#	-> prate.gdas.yyyymm.grb2 --> sum of 4 values per day which are converted to cm/6-hour


	#monthly data (http://rda.ucar.edu/datasets/ds093.2/): ds093.2 - NCEP Climate Forecast System Reanalysis (CFSR) Monthly Products, January 1979 to December 2010, 0.313-deg: monthly mean (4 per day) of forecasts of 6-hour average
	#- relative humidity (%): entire atmosphere --> 2m above ground
	#	-> [0.5-deg] pgbh06.gdas.R_H.2m.grb2 --> means for Jan-Dec
	#- wind (m s-1): u- and v-component at 10m above ground
	#	-> flxf06.gdas.WND.10m.grb2 (u- and v-component) --> means for Jan-Dec
	#- total cloud cover (%): entire atmosphere as a single layer
	#	-> flxf06.gdas.T_CDC.EATM.grb2 --> means for Jan-Dec


	temp <- file.path(project_paths[["dir_ex_weather"]], "NCEPCFSR_Global", "CFSR_weather_prog08032012")
	stopifnot(file.exists(temp))

	prepd_CFSR <- prepare_NCEPCFSR_extraction(project_paths[["dir_in"]], temp)
	stopifnot(!inherits(prepd_CFSR, "try-error"))
} else {
  prepd_CFSR <- NULL
}


#------------------------DAILY WEATHER
if (opt_prepare[["how_determine_sources"]] == "SWRunInformation" &&
  "dailyweather_source" %in% colnames(SWRunInformation)) {

  dw_source <- factor(SWRunInformation$dailyweather_source[sim_size[["runIDs_sites"]]],
    levels = opt_prepare[["dw_source_priority"]])
  do_weather_source <- anyNA(dw_source)

} else {
  dw_source <- factor(rep(NA, sim_size[["runsN_sites"]]),
    levels = opt_prepare[["dw_source_priority"]])
  do_weather_source <- TRUE
}

weather.digits <- 2


if (prj_todos[["GriddedDailyWeatherFromMaurer2002_NorthAmerica"]]) {
	#extract daily weather information for the grid cell coded by latitude/longitude for each simulation run
	#Citation: Maurer, E. P., A. W. Wood, J. C. Adam, D. P. Lettenmaier, and B. Nijssen. 2002. A long-term hydrologically based dataset of land surface fluxes and states for the conterminous United States. Journal of Climate 15:3237-3251.

	project_paths[["dir_maurer2002"]] <- file.path(project_paths[["dir_ex_weather"]],
	  "Maurer+_2002updated", "DAILY_FORCINGS")
	stopifnot(file.exists(project_paths[["dir_maurer2002"]]))

} else {
  project_paths[["dir_maurer2002"]] <- NA
}

if (prj_todos[["GriddedDailyWeatherFromDayMet_NorthAmerica"]]) {
	# https://daymet.ornl.gov/
	#extract daily weather information for the grid cell coded by latitude/longitude for each simulation run
	#Citation
	#	- article: Thornton, P.E., Running, S.W., White, M.A. 1997. Generating surfaces of daily meteorological variables over large regions of complex terrain. Journal of Hydrology 190: 214 - 251. http://dx.doi.org/10.1016/S0022-1694(96)03128-9
	#	- dataset v2: Thornton, P.E., M.M. Thornton, B.W. Mayer, N. Wilhelmi, Y. Wei, R. Devarakonda, and R.B. Cook. 2014. Daymet: Daily Surface Weather Data on a 1-km Grid for North America, Version 2. ORNL DAAC, Oak Ridge, Tennessee, USA. Accessed Month DD, YYYY. Time period: YYYY-MM-DD to YYYY-MM-DD. Spatial range: N=DD.DD, S=DD.DD, E=DDD.DD, W=DDD.DD. http://dx.doi.org/10.3334/ORNLDAAC/1219
  # - dataset v3: Thornton, P.E., M.M. Thornton, B.W. Mayer, Y. Wei, R. Devarakonda, R.S. Vose, and R.B. Cook. 2016. Daymet: Daily Surface Weather Data on a 1-km Grid for North America, Version 3. ORNL DAAC, Oak Ridge, Tennessee, USA. Accessed Month DD, YYYY. Time period: YYYY-MM-DD to YYYY-MM-DD. Spatial Range: N=DD.DD, S=DD.DD, E=DDD.DD, W=DDD.DD. http://dx.doi.org/10.3334/ORNLDAAC/1328

#  project_paths[["dir_daymet"]] <- file.path(project_paths[["dir_ex_weather"]],
#    "DayMet_NorthAmerica", "DownloadedSingleCells_FromDayMetv2_NorthAmerica")
  project_paths[["dir_daymet"]] <- file.path(project_paths[["dir_ex_weather"]],
    "DayMet_NorthAmerica", "DownloadedSingleCells_FromDayMetv3_NorthAmerica")
	if (!file.exists(project_paths[["dir_daymet"]]))
	  stop("Directory for external dataset 'DayMet' does not exist:",
	    shQuote(project_paths[["dir_daymet"]]))
	stopifnot(requireNamespace(DaymetR)) #https://github.com/khufkens/daymetr

} else {
  project_paths[["dir_daymet"]] <- NULL
}

if (prj_todos[["GriddedDailyWeatherFromNRCan_10km_Canada"]] &&
  prj_todos[["actions"]]["dbW"]) {
	#Citations:
	#	- Hopkinson, R. F., D. W. McKenney, E. J. Milewska, M. F. Hutchinson, P. Papadopol, and L. A. Vincent. 2011. Impact of Aligning Climatological Day on Gridding Daily Maximum–Minimum Temperature and Precipitation over Canada. Journal of Applied Meteorology and Climatology 50:1654-1665.
	#	- Hutchinson, M. F., D. W. McKenney, K. Lawrence, J. H. Pedlar, R. F. Hopkinson, E. Milewska, and P. Papadopol. 2009. Development and Testing of Canada-Wide Interpolated Spatial Models of Daily Minimum–Maximum Temperature and Precipitation for 1961–2003. Journal of Applied Meteorology and Climatology 48:725-741.
	#	- McKenney, D. W., M. F. Hutchinson, P. Papadopol, K. Lawrence, J. Pedlar, K. Campbell, E. Milewska, R. F. Hopkinson, D. Price, and T. Owen. 2011. Customized Spatial Climate Models for North America. Bulletin of the American Meteorological Society 92:1611-1622.
	project_paths[["dir.ex.NRCan"]] <- file.path(project_paths[["dir_ex_weather"]],
	  "NRCan_10km_Canada", "DAILY_GRIDS")
	stopifnot(file.exists(project_paths[["dir.ex.NRCan"]]), requireNamespace(raster),
	  requireNamespace(sp), requireNamespace(rgdal))

} else {
  project_paths[["dir.ex.NRCan"]] <- NULL
}



#------------------------CHECK THAT DAILY WEATHER DATA IS AVAILABLE
if (do_weather_source) {
  #--- Determine sources of daily weather
  SWRunInformation <- dw_determine_sources(dw_source, prj_todos[["exinfo"]],
    opt_prepare[["dw_source_priority"]], create_treatments, sim_size, SWRunInformation,
    sw_input_treatments_use, sw_input_treatments, sw_input_experimentals_use,
    sw_input_experimentals, sim_time, fnames_in, project_paths,
    verbose = opt_verbosity[["verbose"]])
}

if (anyNA(SWRunInformation[sim_size[["runIDs_sites"]], "dailyweather_source"])) {
  stop("There are sites without daily weather. Provide data for all runs")
}

if (prj_todos[["ExtractClimateChangeScenarios"]]) {
  opt_sim[["use_dbW_future"]] <- TRUE
  opt_sim[["use_dbW_current"]] <- TRUE
}
if (opt_sim[["use_dbW_future"]])
  opt_sim[["use_dbW_current"]] <- TRUE

if (opt_sim[["use_dbW_current"]]) {
  if (!(prj_todos[["actions"]]["dbW"] || file.exists(fnames_in[["fdbWeather"]]))) {
    stop("Create or use existing Weather database with Scenario data inside.")
  }

} else {
  if (!any(create_treatments == "LookupWeatherFolder",
    prj_todos[["GriddedDailyWeatherFromMaurer2002_NorthAmerica"]],
    prj_todos[["GriddedDailyWeatherFromDayMet_NorthAmerica"]])) {
    stop("Daily weather data must be provided through 'LookupWeatherFolder', ",
      "'Maurer2002_NorthAmerica', or 'DayMet_NorthAmerica' since no weather database is ",
      "used")
  }
}


  #----------------------------------------------------------------------------------------#
  #------------ORGANIZE DATABASES FOR DAILY WEATHER AND FOR SIMULATION OUTPUT

  #--- Create weather database and populate with weather for current conditions
  if (prj_todos[["actions"]]["dbW"]) {
    make_dbW(fnames_in[["fdbWeather"]], sim_size, SWRunInformation, sim_time,
      sim_scens, project_paths, opt_chunks, opt_behave[["resume"]],
      opt_out[["deleteTmpSQLFiles"]], opt_prepare[["set_dbW_compresstype"]],
      tag_WeatherFolder = project_paths[["tag_WeatherFolder"]], opt_parallel,
      prepd_CFSR, verbose = opt_verbosity[["verbose"]])
  }

  if (opt_sim[["use_dbW_current"]] || opt_sim[["use_dbW_future"]])
    stopifnot(check_dbWeather_version(fnames_in[["fdbWeather"]]))



  #--- Prepare output database
  temp <- make_dbOutput(fnames_out[["dbOutput"]], prj_todos, opt_agg,
    SWRunInformation, Index_RunInformation, sim_size, create_treatments,
    create_experimentals, sw_input_treatments, sw_input_treatments_use,
    sw_input_experimentals, sim_scens, sim_time, verbose = opt_verbosity[["verbose"]])

  sim_size[["ncol_dbOut_overall"]] <- temp


if (opt_verbosity[["verbose"]])
  print(paste("SWSF sets up the database: ended after",
    round(difftime(Sys.time(), t1, units = "secs"), 2), "s"))




#---------------------------------------------------------------------------------------#
#------------------------OBTAIN INFORMATION FROM EXTERNAL DATASETS PRIOR TO SIMULATION RUNS TO CREATE THEM
if (any(prj_todos[["actions"]] == "external") && prj_todos[["ex_besides_weather"]]) {
  if (opt_verbosity[["verbose"]])
    print(paste("SWSF extracts information from external datasets prior to simulation",
      "runs: started at", t1 <- Sys.time()))

  stopifnot(file.exists(project_paths[["dir_external"]]))

  if (prj_todos[["exinfo"]][["ExtractSoilDataFromCONUSSOILFromSTATSGO_USA"]] ||
    prj_todos[["exinfo"]][["ExtractSoilDataFromISRICWISEv12_Global"]]) {

    temp <- ExtractData_Soils(prj_todos[["exinfo"]], SWRunInformation, sim_size,
      sw_input_soillayers, sw_input_soils_use, sw_input_soils,
      opt_prepare[["how_determine_sources"]], sim_space, project_paths[["dir_ex_soil"]],
      fnames_in, opt_behave[["resume"]], opt_verbosity[["verbose"]], opt_parallel)

    SWRunInformation <- temp[["SWRunInformation"]]
    sw_input_soillayers <- temp[["sw_input_soillayers"]]
    sw_input_soils_use <- temp[["sw_input_soils_use"]]
    sw_input_soils <- temp[["sw_input_soils"]]
  }

  if (prj_todos[["exinfo"]][["ExtractSkyDataFromNOAAClimateAtlas_USA"]] ||
    prj_todos[["exinfo"]][["ExtractSkyDataFromNCEPCFSR_Global"]]) {

    temp <- ExtractData_MeanMonthlyClimate(prj_todos[["exinfo"]], SWRunInformation,
      sim_size, sw_input_cloud_use, sw_input_cloud,
      opt_prepare[["how_determine_sources"]], sim_space, project_paths, fnames_in,
      opt_chunks,opt_behave[["resume"]], opt_verbosity[["verbose"]], prepd_CFSR, sim_time,
      opt_parallel)

    SWRunInformation <- temp[["SWRunInformation"]]
    sw_input_cloud_use <- temp[["sw_input_cloud_use"]]
    sw_input_cloud <- temp[["sw_input_cloud"]]
  }

  if (prj_todos[["exinfo"]][["ExtractElevation_NED_USA"]] ||
    prj_todos[["exinfo"]][["ExtractElevation_HWSD_Global"]]) {
    SWRunInformation <- ExtractData_Elevation(prj_todos[["exinfo"]], SWRunInformation,
      sim_size, opt_prepare[["how_determine_sources"]], sim_space,
      project_paths[["dir_ex_dem"]], fnames_in, opt_behave[["resume"]],
      opt_verbosity[["verbose"]])
  }

  if (prj_todos[["exinfo"]][["ExtractClimateChangeScenarios"]]) {
    climDB_metas <- climscen_metadata()

    SWRunInformation <- climscen_determine_sources(climDB_metas,
      opt_prepare[["how_determine_sources"]], sim_scens[["sources"]], sim_size,
      SWRunInformation, fnames_in)

    which_NEX <- grepl("NEX", sim_scens[["sources"]])
    which_netCDF <- grepl("(GDODCPUCLLNL)|(SageSeer)", sim_scens[["sources"]])
    which_ClimateWizard <- grepl("ClimateWizardEnsembles", sim_scens[["sources"]])

    if (any(which_NEX) || any(which_netCDF)) {
      SWRunInformation <- ExtractClimateChangeScenarios(sim_scens, climDB_metas, sim_time,
        sim_size, SWRunInformation, sw_input_treatments, fnames_in, project_paths,
        opt_parallel, verbose = opt_verbosity[["verbose"]],
        print.debug = opt_verbosity[["print.debug"]])
    }

    if (any(which_ClimateWizard)) {
      temp <- ExtractClimateWizard(sim_scens, SWRunInformation, fnames_in,
        sw_input_climscen_use, sw_input_climscen,
        sw_input_climscen_values_use, sw_input_climscen_values,
        project_paths[["dir_ex_fut"]], sim_size, verbose = opt_verbosity[["verbose"]])

      SWRunInformation <- temp[["SWRunInformation"]]
      sw_input_climscen_use <- temp[["sw_input_climscen_use"]]
      sw_input_climscen <- temp[["sw_input_climscen"]]
      sw_input_climscen_values_use <- temp[["sw_input_climscen_values_use"]]
      sw_input_climscen_values <- temp[["sw_input_climscen_values"]]
    }
  }

  do_check_include <- TRUE

  if (opt_verbosity[["verbose"]])
    print(paste("SWSF extracts information from external datasets prior to simulation",
      "runs: ended after",  round(difftime(Sys.time(), t1, units = "secs"), 2), "s"))
}



#---------------------------------------------------------------------------------------#
#------------------------CHECK THAT INCLUDE_YN* ARE INCLUSIVE
if (do_check_include) {
  SWRunInformation <- check_requested_sites(include_YN, SWRunInformation, fnames_in,
    verbose = opt_verbosity[["verbose"]])
}


#---------------------------------------------------------------------------------------#
#------------------------CALCULATIONS PRIOR TO SIMULATION RUNS TO CREATE THEM

if (any(unlist(prj_todos[["pcalcs"]]))) {
  if (opt_verbosity[["verbose"]])
    print(paste("SWSF makes calculations prior to simulation runs: started at",
      t1 <- Sys.time()))

    runIDs_adjust <- seq_len(sim_size[["runsN_master"]])  # if not all, then runIDs_sites

  if (prj_todos[["pcalcs"]][["AddRequestedSoilLayers"]]) {
    temp <- calc_ExtendSoilDatafileToRequestedSoilLayers(opt_prepare[["requested_soil_layers"]],
      runIDs_adjust, sw_input_soillayers, sw_input_soils_use, sw_input_soils,
      fnames_in, verbose = opt_verbosity[["verbose"]])

    sw_input_soillayers <- temp[["sw_input_soillayers"]]
    sw_input_soils_use <- temp[["sw_input_soils_use"]]
    sw_input_soils <- temp[["sw_input_soils"]]
  }

  if (prj_todos[["pcalcs"]][["CalculateBareSoilEvaporationCoefficientsFromSoilTexture"]]) {
    #calculate bare soil evaporation coefficients per soil layer for each simulation run and copy values to 'datafile.soils'
    # soil texture influence based on re-analysis of data from Wythers KR, Lauenroth WK, Paruelo JM (1999) Bare-Soil Evaporation Under Semiarid Field Conditions. Soil Science Society of America Journal, 63, 1341-1349.

    temp <- calc_CalculateBareSoilEvaporationCoefficientsFromSoilTexture(runIDs_adjust,
      sw_input_soils_use, sw_input_soils, sw_input_soillayers, fnames_in,
      opt_behave[["resume"]], verbose = opt_verbosity[["verbose"]])

    sw_input_soils_use <- temp[["sw_input_soils_use"]]
    sw_input_soils <- temp[["sw_input_soils"]]
  }

	#------used during each simulation run: define functions here
	if (any(prj_todos[["actions"]] == "create") &&
	  prj_todos[["pcalcs"]][["EstimateConstantSoilTemperatureAtUpperAndLowerBoundaryAsMeanAnnualAirTemperature"]]) {

		sw_input_site_use["SoilTempC_atLowerBoundary"] <- TRUE #set use flag
		sw_input_site_use["SoilTempC_atUpperBoundary"] <- TRUE
		#call function 'SiteClimate' in each SOILWAT2-run
	}

	if (any(prj_todos[["actions"]] == "create") &&
		prj_todos[["pcalcs"]][["EstimateInitialSoilTemperatureForEachSoilLayer"]]) {

		#set use flags
		use.layers <- which(sw_input_soils_use[paste0("Sand_L", swsf_glovars[["slyrs_ids"]])])
		index.soilTemp <- paste0("SoilTemp_L", swsf_glovars[["slyrs_ids"]])[use.layers]
		soilTemp <- sw_input_soils[runIDs_adjust, index.soilTemp, drop = FALSE]
		sw_input_soils_use[index.soilTemp] <- TRUE
	}

	if (opt_verbosity[["verbose"]])
	  print(paste("SWSF makes calculations prior to simulation runs: ended after",
	    round(difftime(Sys.time(), t1, units = "secs"), 2), "s"))
}


#---------------------------------------------------------------------------------------#
#------------------------OBTAIN INFORMATION FROM TABLES PRIOR TO SIMULATION RUNS TO CREATE THEM

if (any(prj_todos[["actions"]] == "create")) {
  temp <- do_prior_TableLookups(tr_input_EvapCoeff, tr_input_TranspRegions, tr_input_SnowD,
    create_treatments, sw_input_treatments, sw_input_experimentals_use, sw_input_experimentals,
    sw_input_soils_use, sw_input_soils, sw_input_cloud_use, sw_input_cloud,
    fnames_in, opt_behave[["resume"]], verbose = opt_verbosity[["verbose"]])

  done_prior <- temp[["done_prior"]]
  sw_input_soils_use <- temp[["sw_input_soils_use"]]
  sw_input_soils <- temp[["sw_input_soils"]]
  sw_input_cloud_use <- temp[["sw_input_cloud_use"]]
  sw_input_cloud <- temp[["sw_input_cloud"]]
}


#---------------------------------------------------------------------------------------#
#------------------------MAP INPUT VARIABLES (FOR QUALITY CONTROL)
if (any(prj_todos[["actions"]] == "map_input") && length(prj_todos[["map_input"]]) > 0) {
  map_input_variables(prj_todos[["map_input"]], SWRunInformation, sw_input_soillayers,
    sw_input_cloud_use,
    sw_input_cloud, sw_input_prod_use, sw_input_prod, sw_input_site_use, sw_input_site,
    sw_input_soils_use, sw_input_soils, sw_input_weather_use, sw_input_weather,
    sw_input_climscen_use, sw_input_climscen, sw_input_climscen_values_use,
    sw_input_climscen_values, sim_size, sim_space, project_paths[["dir_out"]],
    verbose = opt_verbosity[["verbose"]])
}






#---------------------------------------------------------------------------------------#
#------------------------RUN RSOILWAT

  # print system information
  print(temp <- utils::sessionInfo())
  if (opt_behave[["check_blas"]])
    benchmark_BLAS(temp$platform)


# run the simulation experiment
if (prj_todos[["use_SOILWAT2"]] && sim_size[["runsN_todo"]] > 0) {
  swof <- sw_out_flags()
  swDataFromFiles <- read_SOILWAT2_FileDefaults(project_paths[["dir_in_sw"]])
  args_do_OneSite <- gather_args_do_OneSite()

  runs.completed <- run_simulation_experiment(rSWSF, sim_size, SWRunInformation,
    sw_input_soillayers, sw_input_treatments, sw_input_cloud, sw_input_prod,
    sw_input_site, sw_input_soils, sw_input_weather, sw_input_climscen,
    sw_input_climscen_values, MoreArgs = args_do_OneSite)


} else {
  runs.completed <- 0
}
#------------------------

#------------------------
# NOTE(drs): 'concatenation' may be much faster if temporary text files are not constructed
# around SQL insert statements, but instead as data.frames. Text files containing
# data.frames may be much faster with checks for duplicate P_id entries and could be
# inserted at once (instead of line by line) with the command
#   RSQLite::dbWriteTable(con, name = table, value = "path/to/db-file", append = TRUE)

# NOTE: The variables 'pids_inserted' and 'pids2_inserted' become quickly very large and
#   may then be too large for available memory

t.outputDB <- Sys.time()

if (any(prj_todos[["actions"]] == "concatenate")) {
  if (opt_verbosity[["verbose"]])
    print(paste("SWSF inserting temporary data to outputDB: started at", t.outputDB))

  has_time_to_concat <- (difftime(t.outputDB, t_job_start, units = "secs") +
    opt_job_time[["one_concat_s"]]) < opt_job_time[["wall_time_s"]]

  if (has_time_to_concat) {
    move_temporary_to_outputDB(project_paths[["dir_out_temp"]], fnames_out[["dbOutput"]],
      fnames_out[["dbOutput_current"]], t_job_start, opt_job_time,
      opt_out[["dbOutCurrent_from_tempTXT"]] && !opt_out[["dbOutCurrent_from_dbOut"]],
      opt_out[["wipe_dbOutput"]], opt_out[["deleteTmpSQLFiles"]], opt_behave[["resume"]],
      print.debug = opt_verbosity[["print.debug"]], verbose = opt_verbosity[["verbose"]])

  } else {
    print(paste("Need at least", opt_job_time[["one_concat_s"]], "seconds to put SQL in",
      "output DB."))
  }


  if (opt_out[["dbOutCurrent_from_dbOut"]] && !opt_out[["dbOutCurrent_from_tempTXT"]]) {
    has_time_to_concat <- {difftime(Sys.time(), t_job_start, units = "secs") +
      opt_job_time[["one_concat_s"]]} < opt_job_time[["wall_time_s"]]

    if (has_time_to_concat) {
      do_copyCurrentConditionsFromDatabase(fnames_out[["dbOutput"]],
        fnames_out[["dbOutput_current"]], verbose = opt_verbosity[["verbose"]])

    } else {
      print(paste("Need at least", opt_job_time[["one_concat_s"]], "seconds to put SQL",
        "in output DB."))
    }
  }
}

#timing of outputDB
delta.outputDB <- as.double(difftime(Sys.time(), t.outputDB, units = "secs"))
if (opt_verbosity[["verbose"]] && any(prj_todos[["actions"]] == "concatenate")) {
  print(paste("SWSF inserting temporary data to outputDB: ended after",
    round(delta.outputDB, 2), "s"))
}


#---------------------------------------------------------------------------------------#
#------------------------CHECK COMPLETENESS OF OUTPUT DATABASE AND SIMULATION
t.check <- Sys.time()

if (any(prj_todos[["actions"]] == "check")) {
  if (opt_verbosity[["verbose"]])
    print(paste("SWSF checks simulations and output: started at", t.check))

  check_outputDB_completeness(fnames_out[["dbOutput"]], fnames_out[["dbOutput_current"]],
    update_workDB = opt_behave[["check_updates_dbWork"]] || opt_out[["deleteTmpSQLFiles"]],
    do_DBcurrent = opt_out[["dbOutCurrent_from_dbOut"]] || opt_out[["dbOutCurrent_from_tempTXT"]],
    opt_parallel, project_paths[["dir_out"]])
}

#timing of check
delta.check <- difftime(Sys.time(), t.check, units = "secs")
if (opt_verbosity[["verbose"]] && any(prj_todos[["actions"]] == "check"))
  print(paste("SWSF checks simulations and output: ended after", round(delta.check, 2),
    "s"))

#---------------------------------------------------------------------------------------#
#------------------------ENSEMBLE GENERATION
t.ensembles <- Sys.time()	#timing of ensemble calculation

if (prj_todos[["do_ensembles"]]) {

	if (opt_verbosity[["verbose"]])
	  print(paste("SWSF calculates ensembles: started at", t.ensembles))


	con <- DBI::dbConnect(RSQLite::SQLite(), dbname = fnames_out[["dbOutput"]])

	Tables <- dbOutput_ListOutputTables(con)
	Tables <- Tables[-grep(pattern="_sd", Tables, ignore.case = T)]

	if (opt_parallel[["do_parallel"]]) {
		#call the simulations depending on parallel backend

		if(identical(opt_parallel[["parallel_backend"]], "mpi")) {

			ensembles.completed <- Rmpi::mpi.applyLB(X = Tables,
			  FUN = collect_EnsembleFromScenarios,
			  name.OutputDB = fnames_out[["dbOutput"]], t.overall = t_job_start,
			  opt_job_time = opt_job_time, opt_parallel = opt_parallel,
			  dir_out = project_paths[["dir_out"]], sim_scens = sim_scens,
			  opt_chunks = opt_chunks)

		} else if(identical(opt_parallel[["parallel_backend"]], "cluster")) {

      ensembles.completed <- parallel::clusterApplyLB(opt_parallel[["cl"]],
        x = Tables, fun = collect_EnsembleFromScenarios,
			  name.OutputDB = fnames_out[["dbOutput"]], t.overall = t_job_start,
			  opt_job_time = opt_job_time, opt_parallel = opt_parallel,
			  dir_out = project_paths[["dir_out"]], sim_scens = sim_scens,
			  opt_chunks = opt_chunks)
		}

	} else {
    ensembles.completed <- lapply(Tables, FUN = collect_EnsembleFromScenarios,
			  name.OutputDB = fnames_out[["dbOutput"]], t.overall = t_job_start,
			  opt_job_time = opt_job_time, opt_parallel = opt_parallel,
			  dir_out = project_paths[["dir_out"]], sim_scens = sim_scens,
			  opt_chunks = opt_chunks)
	}

  ensembles.completed <- sum(unlist(ensembles.completed))

  temp <- {if (sim_scens[["save.scenario.ranks"]]) 3 else 2} * length(Tables) *
    length(sim_scens[["ensemble.families"]]) * length(sim_scens[["ensemble.levels"]])

	if (ensembles.completed != temp)
	  print(paste("SWSF calculates ensembles: something went wrong with ensemble output:",
	    "ensembles.completed = ", ensembles.completed, " instead of ", temp, "."))

} else {
  ensembles.completed <- 0
}


#timing of ensemble calculation
delta.ensembles <- difftime(Sys.time(), t.ensembles, units="secs")
if (opt_verbosity[["verbose"]] && prj_todos[["do_ensembles"]])
  print(paste("SWSF calculates ensembles: ended after", round(delta.ensembles, 2), "s"))


#---------------------------------------------------------------------------------------#
#------------------------OVERALL TIMING
delta.overall <- difftime(Sys.time(), t_job_start, units = "secs")
if (opt_verbosity[["verbose"]])
 print(paste("SWSF: ended after", round(delta.overall, 2), "s"))

compile_overall_timer(fnames_out[["timerfile"]], project_paths[["dir_out"]],
  opt_parallel[["workersN"]], runs.completed, sim_scens[["N"]], ensembles.completed,
  delta.overall, delta.outputDB, delta.check, delta.ensembles)


if (opt_verbosity[["verbose"]])
  print(paste("SWSF: ended with actions =", paste(prj_todos[["actions"]],
    collapse = ", "), "at", Sys.time()))


#---------------------------------------------------------------------------------------#
#------------------------CODE CLEANUP

options(ow_prev) #sets the warning option to its previous value

if (opt_parallel[["do_parallel"]]) {
  clean_SWSF_cluster(opt_parallel[["parallel_backend"]], cl = opt_parallel[["cl"]],
    verbose = opt_verbosity[["print.debug"]])
}


#---------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------#
}
