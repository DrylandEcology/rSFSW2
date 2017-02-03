#----------------------------------------------------------------------------------------#
# rSWSF: FRAMEWORK FOR SOILWAT2 SIMULATIONS: CREATING SIMULATION RUNS, EXECUTING
#        SIMULATIONS, AND AGGREGATING OUTPUTS
#
# See demo/SWSF_project_code.R for details
#----------------------------------------------------------------------------------------#


##############################################################################
#------ DESCRIPTION OF EXTERNAL DATA AND DATA PREPARATIONS FOR A SIMULATION PROJECT ------


#------ Options for data preparation of a project for a simulation experiment
opt_prepare <- list(
  prior_calculations = c(
      "AddRequestedSoilLayers", 0,
      "EstimateConstantSoilTemperatureAtUpperAndLowerBoundaryAsMeanAnnualAirTemperature", 1,
      "EstimateInitialSoilTemperatureForEachSoilLayer", 1,
      "CalculateBareSoilEvaporationCoefficientsFromSoilTexture", 1
  ),

  # Interpolate and add soil layers if not available if 'AddRequestedSoilLayers'
  requested_soil_layers = c(5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150),

  # Request data from datasets ('external' to a rSWSF-project)
  req_data = c(
      # Daily weather data for current conditions
      "GriddedDailyWeatherFromMaurer2002_NorthAmerica", 0,  # 1/8-degree resolution
      "GriddedDailyWeatherFromDayMet_NorthAmerica", 0,  # 1-km resolution
      "GriddedDailyWeatherFromNRCan_10km_Canada", 0,  # must be used with dbW
      "GriddedDailyWeatherFromNCEPCFSR_Global", 0, # must be used with dbW

      # Monthly PPT, Tmin, Tmax conditions: if using NEX or GDO-DCP-UC-LLNL,
      #   climate condition names must be of the form SCENARIO.GCM with SCENARIO being
      #   used for ensembles; if using climatewizard, climate condition names must be
      #   equal to what is in the respective directories
      "ExtractClimateChangeScenarios", 0,

      # Mean monthly wind, relative humidity, and 100% - sunshine
      "ExtractSkyDataFromNOAAClimateAtlas_USA", 0,
      "ExtractSkyDataFromNCEPCFSR_Global", 0,

      # Topography
      "ExtractElevation_NED_USA", 0,  #1-arcsec res: National Elevation Dataset
        # (ned.usgs.gov), currently downloaded only for western US
      "ExtractElevation_HWSD_Global", 0, #30-arcsec res: Harmonized World Soil Database

      # Soil texture
      "ExtractSoilDataFromCONUSSOILFromSTATSGO_USA", 0,
      "ExtractSoilDataFromISRICWISEv12_Global", 0
  ),

  # Approach to determine prioprities of external data source extractions
  # - If how_determine_sources == "order", then
  #   - Elevation: 'ExtractElevation_NED_USA' has priority over
  #     'ExtractElevation_HWSD_Global' on a per site basis if both are requested and data
  #     is available for both
  #   - Soil texture: 'ExtractSoilDataFromCONUSSOILFromSTATSGO_USA' has priority over
  #     'ExtractSoilDataFromISRICWISEv12_Global' on a per site basis if both are requested
  #     and data is available for both
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
    "Maurer2002_NorthAmerica", "NRCan_10km_Canada", "NCEPCFSR_Global"),

  # Creation of dbWeather
  # Compression type of dbWeather; one value of eval(formals(memCompress)[[2]])
  set_dbW_compresstype = "gzip"
)

##############################################################################
