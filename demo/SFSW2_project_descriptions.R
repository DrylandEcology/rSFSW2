#----------------------------------------------------------------------------------------#
# rSFSW2: FRAMEWORK FOR SOILWAT2 SIMULATIONS: CREATING SIMULATION RUNS, EXECUTING
#        SIMULATIONS, AND AGGREGATING OUTPUTS
#
# See demo/SFSW2_project_code.R for details
#----------------------------------------------------------------------------------------#


##############################################################################
#----------------------- DESCRIPTION OF SIMULATION PROJECT ---------------------

# NOTE: The values cannot be changed once a rSFSW2 simulation project is set up. The
#  values of settings (file demo/SFSW2_project_settings.R) may be changed from run to run.

#----- Metainformation about computing platform
opt_platform <- list(
  host = c("local", "hpc")[1],
  no_parallel = any(
    identical(tolower(Sys.getenv("NOT_CRAN")), "false"),
    identical(tolower(Sys.getenv("TRAVIS")), "true"),
    identical(tolower(Sys.getenv("APPVEYOR")), "true"))
)


#------ Paths to simulation framework project folders
project_paths <- list(
  dir_prj = dir_prj <- {# path to simulation project
    temp <- if (identical(opt_platform[["host"]], "local")) {
        "SFSW2_default_project" # "~/YOURPROJECT"
      } else if (identical(opt_platform[["host"]], "hpc")) {
        getwd()
      }

    if (dir.exists(temp)) {
      if (interactive()) setwd(temp)
    } else {
      print(paste("'project_paths[['dir_prj']]' =", shQuote(temp), "does not exist. Code",
        "uses", shQuote(getwd()), "instead."))
    }
    getwd()
  },

  # Path to inputs
  dir_in = dir_in <- file.path(dir_prj, "1_Data_SWInput"),
  # Folder with default standalone SOILWAT2 input files
  dir_in_sw = file.path(dir_in, "SoilWat2_defaults"),
  # Folder with data input files
  dir_in_dat = file.path(dir_in, "datafiles"),
  # Folder with treatment input files according to treatment instructions
  dir_in_treat = file.path(dir_in, "treatments"),
  # Folder with GISSM regeneration parameters (will contain one file per species)
  dir_in_gissm = file.path(dir_in, "regeneration"),

  # Path to where large outputs are saved to disk
  dir_big = dir_big <- if (identical(opt_platform[["host"]], "local")) {
      dir_prj
    } else if (identical(opt_platform[["host"]], "hpc")) {
      dir_prj
    },
  # Path to where rSOILWAT2 objects are saved to disk
  #   if saveRsoilwatInput and/or saveRsoilwatOutput
  dir_out_sw = file.path(dir_big, "3_Runs"),
  # Path to outputs produced by rSFSW2
  dir_out = dir_out <- file.path(dir_big, "4_Data_SWOutputAggregated"),
  # Path to where rSFSW2 will store temporary files
  dir_out_temp = file.path(dir_out, "temp"),
  # Path to various other output
  dir_out_expDesign = file.path(dir_out, "Experimentals_Input_Data"),
  dir_out_traces = file.path(dir_out, "Time_Traces"),

  # Path from where external data are extraced
  dir_external = dir_ex <- if (identical(opt_platform[["host"]], "local")) {
      file.path("/Volumes", "YOURDRIVE", "BigData", "GIS", "Data")
    } else if (identical(opt_platform[["host"]], "hpc")) {
      file.path("/home", "YOURDRIVE", "BigData", "GIS", "Data")
    },
  # Path to historic weather and climate data including
  #   Livneh, Maurer, ClimateAtlas, and NCEPCFSR data
  dir_ex_weather = file.path(dir_ex, "Weather_Past"),
  # Path to future scenario data
  dir_ex_fut = file.path(dir_ex, "Weather_Future"),
  # Path to soil data
  dir_ex_soil = file.path(dir_ex, "Soils"),
  # Path to topographic data
  dir_ex_dem = file.path(dir_ex, "Topography")
)


#------ Base names or full names of input files
fnames_in <- list(
  fmaster = "SWRuns_InputMaster_YOURPROJECT_v11.csv",

  fslayers = "SWRuns_InputData_SoilLayers_v9.csv",
  ftreatDesign = "SWRuns_InputData_TreatmentDesign_v15.csv",
  fexpDesign = "SWRuns_InputData_ExperimentalDesign_v07.csv",

  fclimnorm = "SWRuns_InputData_cloud_v10.csv",
  fvegetation = "SWRuns_InputData_prod_v11.csv",
  fsite = "SWRuns_InputData_siteparam_v14.csv",
  fsoils = "SWRuns_InputData_soils_v12.csv",
  fweathersetup = "SWRuns_InputData_weathersetup_v10.csv",
  fclimscen_delta = "SWRuns_InputData_ClimateScenarios_Change_v11.csv",
  fclimscen_values = "SWRuns_InputData_ClimateScenarios_Values_v11.csv",

  LookupClimatePPTScenarios = "climate.ppt.csv",
  LookupClimateTempScenarios = "climate.temp.csv",
  LookupShiftedPPTScenarios = "shifted.ppt.csv",
  LookupEvapCoeffFromTable = "BareSoilEvaporationCoefficientsPerSoilLayer.csv",
  LookupTranspCoeffFromTable = "TranspirationCoefficients_v2.csv",
  LookupTranspRegionsFromTable = "TranspirationRegionsPerSoilLayer.csv",
  LookupSnowDensityFromTable = "MeanMonthlySnowDensities_v2.csv",
  LookupVegetationComposition = "VegetationComposition_MeanMonthly_v5.csv",
  LookupCarbonScenarios = "LookupCarbonScenarios.csv",

  # Pre-processed input: storage file of input data for repeated access (faster) instead
  #   of re-reading from (slower) csv files if flag 'use_preprocin' is TRUE
  fpreprocin = "SWRuns_InputAll_PreProcessed.rds",

  # Database with daily weather data
  fdbWeather = if (identical(opt_platform[["host"]], "local")) {
      file.path(project_paths[["dir_in"]], "dbWeatherData.sqlite3")
    } else if (identical(opt_platform[["host"]], "hpc")) {
      file.path(project_paths[["dir_prj"]], "..", "dbWeatherData.sqlite3")
    },

  # Raster describing spatial interpretation of simulation experiment if scorp == "cell"
  fsimraster = file.path(project_paths[["dir_in"]], "sim_raster.grd")
)


#------ Full names of output files
fnames_out <- list(
  dbOutput = file.path(project_paths[["dir_out"]], "dbTables.sqlite3"),
  dbOutput_current = file.path(project_paths[["dir_out"]], "dbTables_current.sqlite3"),
  timerfile = file.path(project_paths[["dir_out"]], "Timing_Simulation.csv")
)



#------ Input data sources and options for data preparation
opt_input <- list(
  prior_calculations = c(
      "AddRequestedSoilLayers", 0,
      "EstimateConstantSoilTemperatureAtUpperAndLowerBoundaryAsMeanAnnualAirTemperature", 1,
      "EstimateInitialSoilTemperatureForEachSoilLayer", 1,
      "CalculateBareSoilEvaporationCoefficientsFromSoilTexture", 1
  ),

  # Interpolate and add soil layers if not available if 'AddRequestedSoilLayers'
  requested_soil_layers = c(5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150),

  # Request data from datasets ('external' to a rSFSW2-project)
  req_data = c(
      # Daily weather data for current conditions
      #   - Maurer et al. 2002: 1/8-degree res. for 1949-2010; data expected at file.path(
      #     project_paths[["dir_ex_weather"]], "Maurer+_2002updated", "DAILY_FORCINGS")
      "GriddedDailyWeatherFromMaurer2002_NorthAmerica", 0,
      #   - Thornton et al. 1997: 1-km res. for 1980-2016; data expected at file.path(
      #     project_paths[["dir_ex_weather"]], "DayMet_NorthAmerica",
      #     "DownloadedSingleCells_FromDayMetv3_NorthAmerica")
      "GriddedDailyWeatherFromDayMet_NorthAmerica", 0,
      #   - McKenney et al. 2011: 10-km res. for 1950-2013; use with dbW; data expected at
      #     file.path(project_paths[["dir_ex_weather"]], "NRCan_10km_Canada", "DAILY_GRIDS")
      "GriddedDailyWeatherFromNRCan_10km_Canada", 0,
      #   - Saha et al. 2010: 0.3125-deg res. for 1979-2010; use with dbW; data expected at file.path(
      #     project_paths[["dir_ex_weather"]], "NCEPCFSR_Global", "CFSR_weather_prog08032012")
      "GriddedDailyWeatherFromNCEPCFSR_Global", 0,
      #   - Livneh et al. 2013: 1/16 degree res. for 1915-2011; data expected at file.path(
      #     project_paths[["dir_ex_weather"]], "Livneh_NA_2013", "MONTHLY_GRIDS")
      "GriddedDailyWeatherFromLivneh2013_NorthAmerica", 0,

      # Monthly PPT, Tmin, Tmax conditions: if using NEX or GDO-DCP-UC-LLNL,
      #   climate condition names must be of the form SCENARIO.GCM with SCENARIO being
      #   used for ensembles; if using climatewizard, climate condition names must be
      #   equal to what is in the respective directories
      #   - data expected at file.path(project_paths[["dir_ex_fut"]], "ClimateScenarios")
      "ExtractClimateChangeScenarios", 0,

      # Mean monthly wind, relative humidity, and 100% - sunshine
      #   - NCDC 2005: data expected at file.path(project_paths[["dir_ex_weather"]],
      #     "ClimateAtlasUS")
      "ExtractSkyDataFromNOAAClimateAtlas_USA", 0,
      #   - Saha et al. 2010: project_paths[["dir_ex_weather"]], "NCEPCFSR_Global",
      #     "CFSR_weather_prog08032012")
      "ExtractSkyDataFromNCEPCFSR_Global", 0,

      # Topography
      #   - NED, National Elevation Dataset (ned.usgs.gov): 1-arcsec res; data expected
      #     at project_paths[["dir_ex_dem"]], "NED_USA", "NED_1arcsec")
      "ExtractElevation_NED_USA", 0,
      #   - Harmonized World Soil Database: 30-arcsec res; data expected
      #     at project_paths[["dir_ex_dem"]], "HWSD")
      "ExtractElevation_HWSD_Global", 0,

      # Soil texture
      #   - Harmonized World Soil Database: 1-km re-gridded; data expected
      #     at project_paths[["dir_ex_soil"]], "CONUSSoil", "output", "albers")
      "ExtractSoilDataFromCONUSSOILFromSTATSGO_USA", 0,
      #   - ISRIC-WISE 5-arcmin v1.2 (2012): 5-arcmin re-gridded; data expected
      #     at project_paths[["dir_ex_soil"]], "WISE", "wise5by5min_v1b", "Grid", "smw5by5min")
      "ExtractSoilDataFromISRICWISEv12_Global", 0,
      #   - ISRIC-WISE 30-arsec v1.0 (2016): 30-arcsec re-gridded; data expected
      #     at project_paths[["dir_ex_soil"]], "WISE", "WISE30sec_v1a")
      "ExtractSoilDataFromISRICWISE30secV1a_Global", 0
  ),

  # Approach to determine prioprities of external data source extractions
  # - If how_determine_sources == "order", then
  #   - Elevation: 'ExtractElevation_NED_USA' has priority over
  #     'ExtractElevation_HWSD_Global' on a per site basis if both are requested and data
  #     is available for both
  #   - Soil texture: 'ExtractSoilDataFromCONUSSOILFromSTATSGO_USA' has first priority,
  #     then 'ExtractSoilDataFromISRICWISE30secV1a_Global' has second priority, and
  #     'ExtractSoilDataFromISRICWISEv12_Global' has third priority on a per site basis
  #     if more than one are requested and data are available for multiple sources
  #   - Climate normals: 'ExtractSkyDataFromNOAAClimateAtlas_USA' has priority over
  #     'ExtractSkyDataFromNCEPCFSR_Global' on a per site basis if both are requested and
  #     data is available for both
  # - If how_determine_sources == "SWRunInformation", then use information in suitable
  #   columns of spreadsheet 'SWRunInformation' if available; if not available, then fall
  #   back to option 'order'
  how_determine_sources = "SWRunInformation",

  # If a run has multiple sources for daily weather, then take the one in the first
  #   position of 'dw_source_priority' if available, if not then second etc.
  # Do not change/remove/add entries; only re-order to set different priorities
  dw_source_priority = c("DayMet_NorthAmerica", "LookupWeatherFolder",
    "Maurer2002_NorthAmerica", "Livneh2013_NorthAmerica", "NRCan_10km_Canada",
    "NCEPCFSR_Global"),

  # Creation of dbWeather
  # Compression type of dbWeather; one value of eval(formals(memCompress)[[2]])
  set_dbW_compresstype = "gzip"
)



#------ Options for simulation and meta-information of input data
opt_sim <- list(
  # Set the random number generator for each task so that repeating runs with the same
  # inputs results in the same outputs even under load-balanced parallel computations and
  # under re-starts of partially finished runs
  reproducible = TRUE,
  global_seed = 1235L,

  # Daily weather either from database 'dbWeather' or specified via 'WeatherFolder' in
  #   MasterInput.csv, treatmentDesign.csv, or experimentalDesign.csv
  # Use daily weather from dbWeather for current condition
  use_dbW_current = TRUE,
  # Use daily weather from dbWeather for future scenario conditions
  use_dbW_future = TRUE,
  # Number of decimal places to which weather data is rounded
  dbW_digits = 2,
  # Identifying tag of folder names for site weather data if 'LookupWeatherFolder'
  tag_WeatherFolder = "weath",

  # Approach if there is no soil texture information for the deepest layer(s)
  #   - [TRUE] adjust soil depth
  #   - [FALSE] fill soil layer structure from shallower layer(s)
  fix_depth_to_layers = FALSE,

  # SOILWAT2 requires windspeed input data observed at a height of 2 m above ground
  #   - NCEP/CRSF data are at 10 m
  windspeed_obs_height_m = 2,

  # SOILWAT2 simulations are repeated with incrementally increased soil temperature
  #   profile layer width until a stable soil temperature solution is found or total
  #   failure is determined
  increment_soiltemperature_deltaX_cm = 5,

  # Maximal soil depth for which bare-soil evaporation coefficients are calculated
  #   if 'CalculateBareSoilEvaporationCoefficientsFromSoilTexture' is TRUE
  depth_max_bs_evap_cm = 15,

  # Shift monthly vegetation/production values in prod.in file by six months
  #   if TRUE and latitude < 0 (i.e., southern hemisphere)
  adjust_veg_input_NS = TRUE,

  # Potential natural vegetation based on climate data (Jose Paruelo et al. 1996, 1998)
  #  - default value: shrub_limit = 0.2 on page 1213 in Paruelo JM,
  #     Lauenroth WK (1996) Relative abundance of plant functional types in grasslands
  #     and shrublands of North America. Ecological Applications, 6, 1212-1224.
  shrub_limit = 0.2,
  # Growing season threshold
  #   - 10 C based on Trewartha's D temperateness definition:
  #     temperate climate := has >=4 & < 8 months with > 10C
  #   - 4 C based standard input of mean monthly biomass values described in
  #   Bradford et al. 2014 Journal of Ecology
  growseason_Tlimit_C = 4
)


#------ Output options
opt_out_fix <- list(
  # Column numbers of master input file 'SWRunInformation', e.g, c(3, 7:9), or NULL:
  #   Selected columns will be part of 'header' table in dbOutput in addition to those of
  #   create_treatments, experimental_treatments, and climate scenario
  Index_RunInformation = NULL,

  # Text separator if 'makeInputForExperimentalDesign'
  ExpInput_Seperator = "X!X",

  # Current subset of dbOutput
  #   - Create from a subset of temporary text files (fast)
  dbOutCurrent_from_tempTXT = FALSE,
  #   - Subset scenarios to climate.ambient (slow)
  dbOutCurrent_from_dbOut = FALSE
)



#----- Spatial setup of simulations
# scorp := one of c("point", "cell"), whether to interpret the simulation locations
#   provided in 'SWRunInformation' as point locations (1D-sites) or as means of 2D-cells
# If scorp == "cell" then provide either valid path to 'fsimraster' (takes precedence) or
#   (grid resolution and grid crs)
# Currently, implemented for
# - actions[["map_inputs"]]
# - external extractions:
#  - soils: "ExtractSoilDataFromISRICWISEv12_Global",
#     "ExtractSoilDataFromISRICWISE30secV1a_Global",
#     "ExtractSoilDataFromCONUSSOILFromSTATSGO_USA",
#  - elevation: "ExtractElevation_NED_USA", "ExtractElevation_HWSD_Global",
#  - climate normals: "ExtractSkyDataFromNOAAClimateAtlas_USA"
#     NOTE: not implemented for 'ExtractSkyDataFromNCEPCFSR_Global'
in_space <- list(
  scorp = scorp <- "point",

  # Resolution of raster cells
  sim_res = if (scorp == "cell") c(1e4, 1e4) else NA,
  # Coordinate reference system (CRS)
  sim_crs = if (scorp == "cell") {
      "+init=epsg:5072" # NAD83(HARN) / Conus Albers
    } else {
      "+init=epsg:4326" # WGS84
    }
)


#------ Time frames of simulation (may be modified by treatments)
sim_time <- list(
  # current simulation years = simstartyr:endyr
  # spinup_N = startyr - simstartyr
  # years used for results = startyr:endyr
  simstartyr = 1979,
  startyr = startyr <- 1980,
  endyr = endyr <- 2010,

  #Future time period(s):
  # Each list element of 'future_yrs' will be applied to every climate.conditions
  # Each list element of 'future_yrs' is a vector with three elements
  # c(delta, DSfut_startyr, DSfut_endyr)
  # future simulation years = delta + simstartyr:endyr
  # future simulation years downscaled based on
  #   - current conditions = DScur_startyr:DScur_endyr
  #   - future conditions = DSfut_startyr:DSfut_endyr
  # NOTE: Multiple time periods doesn't work with external type 'ClimateWizardEnsembles'
  DScur_startyr = startyr,
  DScur_endyr = endyr,

  future_yrs = list(
    c(d <- 40, startyr + d, endyr + d),
    c(d <- 90, startyr + d, endyr + d - 1) # most GCMs don't have data for 2100
  )
)


#------ Requested climate conditions
req_scens <- list(
  # Name of climatic conditions of the daily weather input when monthly climate
  #   perturbations are all off
  ambient = "Current",

  # Names of climate scenarios
  #   - If a simulation project does not include future climate conditions, then set
  #     models = NULL
  #   - If climate datafiles used, then in the order of data in the those datafiles
  #   - This is a list of all GCMs for CMIP5 provided by GDO-DCP-UC-LLNL: 37 RCP4.5, 35 RCP8.5
  #     Excluded: 'HadCM3' and 'MIROC4h' because data only available until 2035
  models = c("RCP45.ACCESS1-0", "RCP45.ACCESS1-3", "RCP45.bcc-csm1-1",
    "RCP45.bcc-csm1-1-m", "RCP45.BNU-ESM", "RCP45.CanESM2", "RCP45.CCSM4",
    "RCP45.CESM1-BGC", "RCP45.CESM1-CAM5", "RCP45.CMCC-CM", "RCP45.CNRM-CM5",
    "RCP45.CSIRO-Mk3-6-0", "RCP45.EC-EARTH", "RCP45.FGOALS-g2", "RCP45.FGOALS-s2",
    "RCP45.FIO-ESM", "RCP45.GFDL-CM3", "RCP45.GFDL-ESM2G", "RCP45.GFDL-ESM2M",
    "RCP45.GISS-E2-H-CC", "RCP45.GISS-E2-R", "RCP45.GISS-E2-R-CC", "RCP45.HadGEM2-AO",
    "RCP45.HadGEM2-CC", "RCP45.HadGEM2-ES", "RCP45.inmcm4", "RCP45.IPSL-CM5A-LR",
    "RCP45.IPSL-CM5A-MR", "RCP45.IPSL-CM5B-LR", "RCP45.MIROC-ESM", "RCP45.MIROC-ESM-CHEM",
    "RCP45.MIROC5", "RCP45.MPI-ESM-LR", "RCP45.MPI-ESM-MR", "RCP45.MRI-CGCM3",
    "RCP45.NorESM1-M", "RCP45.NorESM1-ME",

             "RCP85.ACCESS1-0", "RCP85.ACCESS1-3", "RCP85.bcc-csm1-1",
    "RCP85.bcc-csm1-1-m", "RCP85.BNU-ESM", "RCP85.CanESM2", "RCP85.CCSM4",
    "RCP85.CESM1-BGC", "RCP85.CESM1-CAM5", "RCP85.CMCC-CM", "RCP85.CNRM-CM5",
    "RCP85.CSIRO-Mk3-6-0", "RCP85.EC-EARTH", "RCP85.FGOALS-g2", "RCP85.FGOALS-s2",
    "RCP85.FIO-ESM", "RCP85.GFDL-CM3", "RCP85.GFDL-ESM2G", "RCP85.GFDL-ESM2M",
                          "RCP85.GISS-E2-R",                       "RCP85.HadGEM2-AO",
    "RCP85.HadGEM2-CC", "RCP85.HadGEM2-ES", "RCP85.inmcm4", "RCP85.IPSL-CM5A-LR",
    "RCP85.IPSL-CM5A-MR", "RCP85.IPSL-CM5B-LR", "RCP85.MIROC-ESM", "RCP85.MIROC-ESM-CHEM",
    "RCP85.MIROC5", "RCP85.MPI-ESM-LR", "RCP85.MPI-ESM-MR", "RCP85.MRI-CGCM3",
    "RCP85.NorESM1-M", "RCP85.NorESM1-ME"
  ),

  sources = c(
    # For each climate data set from which to extract, add an element like 'dataset1'
    # Priority of extraction: dataset1, dataset2, ... if multiple sources provide data
    #   for a location
    # Dataset = 'project_source' with
    #   - project = one string out of c("CMIP3", "CMIP5")
    #   - source = one string out of:
    #     - "ClimateWizardEnsembles_Global": mean monthly values at 50-km resolution for 2070-2099
    #     - "ClimateWizardEnsembles_USA": mean monthly change at 12-km resolution between 2070-2099 and 1971-2000
    #     - "BCSD_GDODCPUCLLNL_USA": monthly time series at 1/8-degree resolution
    #     - "BCSD_GDODCPUCLLNL_Global": monthly time series at 1/2-degree resolution
    #     - "BCSD_NEX_USA": monthly time series at 30-arcsec resolution; requires live internet access
    #     - "BCSD_SageSeer_USA": monthly time-series at 1-km resolution for the western US prepared by Katie Renwick
    #     - "ESGF_Global": monthly time-series at varying resolution
      dataset1 = "CMIP5_BCSD_GDODCPUCLLNL_USA"
  ),

  # Downscaling method (applied to each each climate.conditions)
  #   Monthly scenario -> daily forcing variables
  #   One or multiple elements of
  #   - "raw"
  #   - "delta" (Hay et al. 2002)
  #   - "hybrid-delta" (Hamlet et al. 2010), "hybrid-delta-3mod"
  #   - "wgen-package" (Steinschneider & Brown 2013 WRR, doi:10.1002/wrcr.20528
  method_DS = c("hybrid-delta-3mod"),

  # Downscaling parameters
  opt_DS = list(
    daily_ppt_limit = 1.5,
    monthly_limit = 1.5,

    # Method to apply precipitation changes: either "detailed" or "simple"
    ppt_type = "detailed",

    # Method to fix spline predictions: one of "fail", "none" or "attempt";
    #   only used if extrapol_type is using splines
    #  - "fail": downscaling fails if spline extrapolations fall outside estimated
    #     monthly extremes
    #  - "none": no correction for extrapolated monthly extreme values, but this will
    #     likely fail during correction of extreme daily PPT events
    #  - "attempt": repeated attempts with jittering data to fit spline extrapolations
    #     within estimated monthly extreme values
    fix_spline = "attempt",

    # Method to extrapolate beyond observed data
    #   Options: one of "linear_Boe", "linear_Thermessl2012CC.QMv1b", "linear_none",
    #     "tricub_fmm", "tricub_monoH.FC", "tricub_natural", "normal_anomalies"
    #  - "linear": Gudmundsson et al. 2012: "If new model values (e.g. from climate
    #     projections) are larger than the training values used to estimate the empirical
    #     CDF, the correction found for the highest quantile of the training period is
    #     used (Boe ?? et al., 2007; Theme??l et al., 2012)."
    #  - "tricub": I got really large output values, e.g., obs.hist = 54 cm,
    #       scen.fut = 64 cm, sbc.fut = 88 cm, hd.fut = 89 cm
    #  - "linear" (i.e., using Boe et al.'s correction) resulted for the same site to:
    #       obs.hist = 54 cm, scen.fut = 64 cm, sbc.fut = 75 cm, hd.fut = 75 cm
    #  - "normal", but no implemented in qmap: Tohver et al. 2014, Appendix A, p. 6:
    #     "... values that are outside the observed quantile map (e.g. in the early parts
    #     of the 20th century) are interpolated using standard anomalies (i.e. number of
    #     standard deviations from the mean) calculated for observed data and GCM data.
    #     Although this approach ostensibly assumes a normal distribution, it was found
    #     during testing to be much more stable than attempts to use more sophisticated
    #     approaches. In particular, the use of Extreme Value Type I or Generalized
    #     Extreme Value distributions for extending the tail of the probability
    #     distributions were both found to be highly unstable in practice and introduced
    #     unacceptable daily extremes in isolated grid cells. These errors occur because
    #     of irregularities in the shapes of the CDFs for observed and GCM data, which
    #     relates in part to the relatively small sample size used to construct the
    #     monthly CDFs (i.e. n = 30)."
    extrapol_type = "linear_Thermessl2012CC.QMv1b",

    # Test whether data distributions are within sigmaN * stats::sd of mean
    sigmaN = 6,

    # Additive instead of multiplicative adjustments for precipitation if precipitation
    #   is above or below 'PPTratioCutoff'; 3 was too small -> resulting in too many
    #   medium-sized ppt-event
    PPTratioCutoff = 10
  ),

  # Climate ensembles created across scenarios
  # Ensemble families: NULL or from c("SRESA2", "SRESA1B", "SRESB1")
  # This defines the groups for which ensembles of climate scenarios are calculated;
  #   corresponds to first part of scenario name
  ensemble.families = NULL,
  # If(!is.null(ensemble.families)) then this needs to have at least one value; this
  #   variable defines which ranked climate.conditions the ensembles are representing
  #   for each ensemble.families
  ensemble.levels = c(2, 8, 15),
  # If TRUE then for each ensemble.levels a file is saved with the scenario numbers
  #   corresponding to the ensemble.levels
  save.scenario.ranks = TRUE
)


#------ Requested output
# Turn aggregation for variable groups on (1) or off (0), don't delete any names
req_out <- list(
  # Overall aggregated output table
  overall_out = c(
  #---Aggregation: SOILWAT2 inputs
    "input_SoilProfile", 1,
    "input_FractionVegetationComposition", 1,
    "input_VegetationBiomassMonthly", 1,
    "input_VegetationBiomassTrends", 1,
    "input_VegetationPeak", 1,
    "input_Phenology", 1,
    "input_TranspirationCoeff", 1,
    "input_ClimatePerturbations", 1,
    "input_CO2Effects", 1,
  #---Aggregation: Climate and weather
    "yearlyTemp", 1,
    "yearlyPPT", 1,
    "dailySnowpack", 1,
    "dailyFrostInSnowfreePeriod", 1,
    "dailyHotDays", 1,
    "dailyWarmDays", 1,
    "dailyColdDays", 1,
    "dailyCoolDays", 1,
    "dailyPrecipitationEventSizeDistribution", 1,
    "yearlyPET", 1,
    "monthlySeasonalityIndices", 1,
  #---Aggregation: Climatic dryness
    "yearlymonthlyTemperateDrylandIndices", 1,
    "yearlyDryWetPeriods", 1,
    "dailyWeatherGeneratorCharacteristics", 1,
    "dailyPrecipitationFreeEventDistribution", 1,
    "monthlySPEIEvents", 1,
  #---Aggregation: Climatic control
    "monthlyPlantGrowthControls", 1,
    "dailyC4_TempVar", 1,
    "dailyDegreeDays", 1,
    "dailyColdDegreeDays", 1,
  #---Aggregation: Yearly water balance
    "yearlyAET", 1,
    "yearlyWaterBalanceFluxes", 1,
    "dailySoilWaterPulseVsStorage", 1,
  #---Aggregation: Daily extreme values
    "dailyTranspirationExtremes", 1,
    "dailyTotalEvaporationExtremes", 1,
    "dailyDrainageExtremes", 1,
    "dailyInfiltrationExtremes", 1,
    "dailyAETExtremes", 1,
    "dailySWPextremes", 1,
    "dailyRechargeExtremes", 1,
  #---Aggregation: Ecological dryness
    # Note: 'dailyNRCS_SoilMoistureTemperatureRegimes*' require at least soil layers at
    #   10, 20, 30, 50, 60, 90 cm
    "dailyNRCS_SoilMoistureTemperatureRegimes_Intermediates", 1,
    "dailyNRCS_SoilMoistureTemperatureRegimes", 1,
    "dailyNRCS_Chambers2014_ResilienceResistance", 1,
    "dailyNRCS_Maestas2016_ResilienceResistance", 1,
    "dailyWetDegreeDays", 1,
    "dailyThermalDrynessStartEnd", 1,
    "dailyThermalSWPConditionCount", 1,
    "monthlySWPdryness", 1,
    "dailySWPdrynessANDwetness", 1,
    "dailySuitablePeriodsDuration", 1,
    "dailySuitablePeriodsAvailableWater", 1,
    "dailySuitablePeriodsDrySpells", 1,
    "dailySWPdrynessDurationDistribution", 1,
    "dailySWPdrynessEventSizeDistribution", 1,
    "dailySWPdrynessIntensity", 1,
    "dailyThermalDrynessStress", 1,
  #---Aggregation: Mean monthly values
    "monthlyTemp", 1,
    "monthlyPPT", 1,
    "monthlySnowpack", 1,
    "monthlySoilTemp", 1,
    "monthlyRunoff", 1,
    "monthlyRunon", 1,
    "monthlyHydraulicRedistribution", 1,
    "monthlyInfiltration", 1,
    "monthlyDeepDrainage", 1,
    "monthlySWPmatric", 1,
    "monthlyVWCbulk", 1,
    "monthlyVWCmatric", 1,
    "monthlySWCbulk", 1,
    "monthlySWAbulk", 1,
    "monthlyTranspiration", 1,
    "monthlySoilEvaporation", 1,
    "monthlyAET", 1,
    "monthlyPET", 1,
    "monthlyVPD", 1,
    "monthlyAETratios", 1,
    "monthlyPETratios", 1,
  #---Aggregation: Potential regeneration
    "dailyRegeneration_bySWPSnow", 1,
    "dailyRegeneration_GISSM", 1
  ),

  # Select variables to aggregate daily means and stats::sd (one per day of year, DOY)
  #  options: NULL or a selection of c("AET", "Transpiration", "EvaporationSoil",
  #   "EvaporationSurface", "EvaporationTotal", "VWCbulk", "VWCmatric", "SWCbulk",
  #   "SWPmatric", "Snowpack", "SWAbulk", "Rain", "Snowfall", "Snowmelt", "SnowLoss",
  #   "Runoff", "Runon", "Infiltration", "DeepDrainage", "PET", "TotalPrecipitation",
  #   "TemperatureMin", "TemperatureMax", "SoilTemperature")
  mean_daily = NULL,
  # Select variables to output as aggregated yearly time series
  #  options: NULL or a selection of c("dailyRegeneration_GISSM")
  traces = NULL
)

#------ Parameters for output aggregations
opt_agg <- list(
  # Aggregate overall simulation output across soil layers with separate values for
  #   shallow/top (soil layers < aon_toplayer_cm) and deep/bottom soil layers
  aon_toplayer_cm = 20,

  # Aggregate mean daily simulation output across soil layers
  doy_slyrs = list(
    # Do [no] aggregate soil layers
    #   - TRUE, aggregate into 1-4 layers for mean/stats::sd
    #   - FALSE, output values for every simulated soil layer
    do = FALSE,
    # Depth of aggregated soil layers
    #   Options: depth in centimeters or
    #   - NULL is interpreted as deepest soil layer (not available for first)
    #   - NA indicates that no third/fourth aggregated layer is calculated
    #       (not available for first and second)
    # Depth of first aggregated soil layer
    first_cm = 10,
    # Depth of second aggregated soil layer
    second_cm = 20,
    # Depth of third aggregated soil layer
    third_cm = 60,
    # Depth of fourth aggregated soil layer
    fourth_cm = NULL
  ),

  # The ccounting of timing variables is shifted by 6 months (e.g., July becomes 1st
  #   month, etc.) if TRUE and latitude < 0 (i.e., southern hemisphere)
  adjust_NorthSouth = TRUE,

  # Critical soil water potential(s) [MPa] to calculate 'dry' and 'wet' soils
  #   (cf. wilting point) and available soil water
  SWPcrit_MPa = c(-1.5, -3.0, -3.5, -3.9),

  # Critical temperatures [Celsius degrees]
  Tmin_crit_C = c(-15, -9, 0),
  Tmax_crit_C = c(34, 40),
  Tmean_crit_C = c(5, 15, 25, 35),

  # Base temperature (degree C) above which degree-days are accumulated
  Tbase_DD_C = 0,

  # Base temperature (degree C) below which cold-degree-days are accumulated
  Tbase_coldDD_C = 0,

  # Options for calculating daily aggregation options over a specific range of days
  use_doy_range = TRUE,
  doy_ranges = list(
    dailyFrostinSnowPeriod = c(1,250), #water year
    default = c(1, 250),
    defaultWateryear_N = c(274, 273), # default water year aggregation in the N. Hemisphere -  a full year Oct1st - Sept31st
    defaultWateryear_S = c(92, 91) # default water year aggregation in the S. Hemisphere
  ),

  # Daily weather frequency distributions
  # Bins of x mm precipitation event sizes
  bin_prcp_mm = 5,
  # Bins of x consecutive days without precipitation
  bin_prcpfree_days = 10,

  # Parameters for 'dailyRegeneration_bySWPSnow'
  dailyRegeneration_bySWPSnow = list(
    season.start = "LastSnow", # either doy or "LastSnow"
    season.end = "FirstSnow", # either doy or "FirstSnow"
    germination.duration = 7, # in days
    germination.swp.surface = -0.2, # in MPa, duration must have at least x MPa
    establishment.duration = 14, # in days
    establishment.swp.surface = -0.4, # in MPa, duration must have at least x MPa
    establishment.delay = 1 # establishment starts latest x days after end of germination
  ),

  # NRCS soil moisture regimes (SMR) and soil temperature regimes (STR) settings
  NRCS_SMTRs = list(
    # Approach for regime determination ('data' -> 'conditions' -> 'regime')
    aggregate_at = "conditions",
    # Aggregation agreement level (e.g., 0.5 = majority; 1 = all)
    crit_agree_frac = 0.9,
    # Restrict data to normal years (as defined by SSS 2014) if TRUE; if FALSE, all years
    use_normal = TRUE,
    SWP_dry = -1.5,       #dry means SWP below -1.5 MPa (Soil Survey Staff 2014: p.29)
    SWP_sat = -0.033,     #saturated means SWP above -0.033 MPa
    impermeability = 0.9  #impermeable layer
  )
)

##############################################################################
