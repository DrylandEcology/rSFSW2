#--------------------------------------------------------------------------------------------------#

#------------------------FRAMEWORK FOR SOILWAT SIMULATIONS: CREATING SIMULATION RUNS, EXECUTING SIMULATIONS, AND AGGREGATING OUTPUTS

#--------------------------------------------------------------------------------------------------#

#------CODE developed and written by
# - Daniel R Schlaepfer (daniel.schlaepfer@unibas.ch, drs): 2009-2016
# - Donovan Miller (dlm): 2012
# - Ryan Murphy (rjm): 2012-2015
# - Charlie Duso (cd): 2016
# - Caitlin Andrews (ca): 2016
# - Alexander Reeder (ar): 2016
#for contact and further information see also: sites.google.com/site/drschlaepfer

#The R code below was tested on R version 3.3.0

#------DISCLAIMER: This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#------NOTES:
#	- the code performs only rudimentary error checking and handling
#	- SoilWat is forced by:
#		- daily: rainfall (cm), maximum and minimum air temperature at 2-m height (C)
#		- mean monthly: wind speed at 2-m height (miles/h before v24, m/s starting with v24), relative humidity at 2-m height (%), and cloud cover (%)

#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#
#---------------------------------------------SETUP------------------------------------------------#

#------Clean the working environment
# rm(list=ls(all=TRUE))

#------Overall timing
t.overall <- Sys.time()
be.quiet <- FALSE
eta.estimate <- interactive()
print.debug <- interactive()
debug.warn.level <- sum(c(print.debug, interactive()))
debug.dump.objects <- interactive()

#------Mode of framework
minVersionRsoilwat <- "1.1.0"
minVersion_dbWeather <- "3.1.0"
use_rcpp <- TRUE
num_cores <- 4
parallel_backend <- "snow" #"snow" or "multicore" or "mpi"
parallel_runs <- !interactive()

#------Rmpi Jobs finish within Wall Time------#
MaxRunDurationTime <- 1.5 * 60 *60 #Set the time duration for this job [in seconds], i.e. Wall time. As time runs out Rmpi will not send more work. Effects Insert into database and ensembles.
MaxDoOneSiteTime <- (MaxRunDurationTime - 11*60) #This will stop new Rmpi jobs at 'x' seconds before MaxRunDuration expires.
MinTimeConcat <- 10 * 60 * 60 #This is the minimum time remaining after execution needed to begin concat
MaxConcatTime <- 35 * 60 #This will stop any new sql file concat job at 'x' seconds before MaxRunDuration expires.

#------Repository in case installation of additional R packages is required
url.Rrepos <- "https://cran.us.r-project.org"

#--------------------------------------------------------------------------------------------------#
#------------------------USER INPUT

#------Set paths to simulation framework folders
#parent folder of simulation project
# for test projects:
#	if interactive: current working directory must be SoilWat_R_Wrapper/
#	if !interactive: current working directory must be folder of test projects,
#		e.g., SoilWat_R_Wrapper/tests/Test_projects/Test4_AllOverallAggregations
if(interactive()) {
	dir.prj <- normalizePath(file.path(".", "tests", "Test_projects", "Test2_LookupWeatherFolders"))
	setwd(dir.prj)
}
dir.prj <- dir.big <- getwd()
dir.code <- normalizePath(file.path("..", "..", "..")) # "github/SoilWat_R_Wrapper/R"

#parent folder containing external data
#drs dir.external <- "/Volumes/YOURBIGDATA/BigData/GIS/Data"
dir.external <- "/Volumes/BookDuo_12TB/BigData/GIS/Data"

#paths to external subfolder
dir.ex.weather <- file.path(dir.external,"Weather_Past")#historic weather data. Used with Livneh and Maurer Data and ClimateAtlas and NCEPCFSR data.
dir.ex.fut <- file.path(dir.external,"Weather_Future")#future scenario data.
dir.ex.soil <- file.path(dir.external,"Soils")
dir.ex.dem <- file.path(dir.external,"Topography")

#paths to sub-folder hierarchy
dir.in <- file.path(dir.prj, "1_Data_SWInput")	#path to input data of SoilWat-runs)
dir.sw.dat <- file.path(dir.in, "datafiles")	#folder with datafiles to add information to SoilWat input files
dir.sw.in <- file.path(dir.in, "swrun")	#folder with complete SoilWat run setup (without yearly weather files, cloudin is in 'Input' folder and not in weather-folder: needs to be moved appropiately)
dir.sw.in.tr <- file.path(dir.in, "treatments")	#folder with treatment input files according to treatment instructions
dir.sw.in.reg <- file.path(dir.in, "regeneration")	#folder with regeneration files, one for each species = run of 'dailyRegeneration_byTempSWPSnow'
dir.sw.runs <- file.path(dir.big, "3_Runs")	#path to SoilWat-runs
dir.out <- file.path(dir.big, "4_Data_SWOutputAggregated")	#path to aggregated output


#------Define actions to be carried out by simulation framework
#actions are at least one of c("external", "map_input", "create", "execute", "aggregate", "concatenate", "ensemble")
#	- data preparation
#		- "external": pulls data from 'external' data sources from 'dir.external' as specified by 'do.ExtractExternalDatasets'
#		- "map_input": creates maps of input data as specified by 'map_vars'
#	- simulation runs ('create', 'execute', and 'aggregate' can be used individually if 'saveRsoilwatInput' and/or 'saveRsoilwatOutput')
#		- "create": puts information and files together for each simulation run
#		- "execute": executes the SoilWat simulation
#		- "aggregate": calculates aggregated response variables from the SoilWat output and writes results to temporary text files
#	- output handling
#		- "concatenate": moves results from the simulation runs (temporary text files) to a SQL-database
#		- "ensemble": calculates 'ensembles' across climate scenarios and stores the results in additional SQL-databases as specified by 'ensemble.families' and 'ensemble.levels'
actions <- c("external", "map_input", "create", "execute", "aggregate", "concatenate")
#continues with unfinished part of simulation after abort if TRUE, i.e.,
#	- it doesn't delete an existing weather database, if a new one is requested
#	- it doesn't re-extract external information (soils, elevation, climate normals, NCEPCFSR) if already extracted
# - it doesn't lookup values from tables if already available in input datafiles, i.e., 'LookupEvapCoeffFromTable', 'LookupTranspRegionsFromTable', and 'LookupSnowDensityFromTable'
#	- it doesn't repeat calls to 'do_OneSite' that are listed in 'runIDs_done'
continueAfterAbort <- TRUE
#use preprocessed input data if available
usePreProcessedInput <- TRUE
#stores for each SoilWat simulation a folder with inputs and outputs if TRUE
saveRsoilwatInput <- TRUE
saveRsoilwatOutput <- TRUE
#store data in big input files for experimental design x treatment design
makeInputForExperimentalDesign <- FALSE
# fields/variables of input data for which to create maps if any(actions == "map_input")
map_vars <- c("ELEV_m", "SoilDepth", "Matricd", "GravelContent", "Sand", "Clay", "EvapCoeff", "RH", "SkyC", "Wind", "snowd")
#check completeness of SoilWat simulation directories and of temporary output aggregation files; create a list with missing directories and files
checkCompleteness <- FALSE
# check linked BLAS library before simulation runs
check.blas <- FALSE

#---Load functions
rSWSF <- file.path(dir.code, "R", "2_SWSF_p5of5_Functions_v51.RData")
if (!file.exists(rSWSF) || !continueAfterAbort) {
  sys.source(sub(".RData", ".R", rSWSF), envir = attach(NULL, name = "swsf_funs"))
  save(list = ls(name = "swsf_funs"), file = rSWSF)
  detach("swsf_funs")
}
load(rSWSF)
print("The following warning can be safely ignored: ''package:stats' may not be available when loading'. It will disappear once the wrapper has been transformed to a package")


#------Define how aggregated output should be handled:
cleanDB <- TRUE #This will wipe all the Tables at the begining of a run. Becareful not to wipe your data.
deleteTmpSQLFiles <- FALSE
copyCurrentConditionsFromTempSQL <- TRUE
copyCurrentConditionsFromDatabase <- FALSE #Creates a copy of the main database containing the scenario==climate.ambient subset
ensembleCollectSize <- 500 #This value is the chunk size for reads of 'runID' from the database, i.e., chunk size = ensembleCollectSize * scenario_No. Yellowstone 500 seems to work. Balance between available memory, cores, read/write times, etc..

#------Define type of simulations and source of input data
#Daily weather data: must be one of dailyweather_options; WeatherFolder in MasterInput.csv, treatmentDesign.csv, or experimentalDesign.csv
# If a run has multiple sources for daily weather, then take the one in the first position of dailyweather_options if availble, if not then second etc.
#	do not change/remove/add entries; only re-order to set different priorities
dailyweather_options <- c("Maurer2002_NorthAmerica", "DayMet_NorthAmerica", "LookupWeatherFolder", "NRCan_10km_Canada", "NCEPCFSR_Global")
#Daily weather database
getCurrentWeatherDataFromDatabase <- TRUE
getScenarioWeatherDataFromDatabase <- TRUE
dbWeatherDataFile <- file.path(dir.big, "1_Data_SWInput", "dbWeatherData_test.sqlite3")
createAndPopulateWeatherDatabase <- TRUE #TRUE, will create a new(!) database and populate with current data
dbW_compression_type <- "gzip" # one of eval(formals(memCompress)[[2]]); this only affects dbWeather if createAndPopulateWeatherDatabase

#-Spatial setup of simulations
# Should the locations of 'SWRunInformation' interpreted as 2D-cells of a raster/grid or as 1D-sites
# sim_cells_or_points: currently, implemented for
# - actions == "map_inputs"
# - external extractions:
#	- soils: "ExtractSoilDataFromISRICWISEv12_Global", "ExtractSoilDataFromCONUSSOILFromSTATSGO_USA",
#	- elevation: "ExtractElevation_NED_USA", "ExtractElevation_HWSD_Global",
#	- climate normals: "ExtractSkyDataFromNOAAClimateAtlas_USA" (NOTE: not implemented for 'ExtractSkyDataFromNCEPCFSR_Global')
sim_cells_or_points <- "point" # one of c("point", "cell"), whether to extract for point locations or averaged over a cell area
if (sim_cells_or_points == "cell") {
	# provide either path to raster file (takes precedence) or (grid resolution and grid crs)
	fname_sim_raster <- file.path(dir.in, "YOURRASTER.FILE")
	sim_res <- c(1e4, 1e4)
	sim_crs <- sp::CRS("+init=epsg:5072") # NAD83(HARN) / Conus Albers
} else {
	sim_crs <- sp::CRS("+init=epsg:4326") # WGS84
}

#Indicate if actions contains "external" which external information (1/0) to obtain from dir.external, don't delete any labels; GIS extractions not supported on JANUS
# if extract_determine_database == "order", then
# - Elevation: 'ExtractElevation_NED_USA' has priority over 'ExtractElevation_HWSD_Global' on a per site basis if both are requested and data is available for both
# - Soil texture: 'ExtractSoilDataFromCONUSSOILFromSTATSGO_USA' has priority over 'ExtractSoilDataFromISRICWISEv12_Global' on a per site basis if both are requested and data is available for both
# - Climate normals: 'ExtractSkyDataFromNOAAClimateAtlas_USA' has priority over 'ExtractSkyDataFromNCEPCFSR_Global' on a per site basis if both are requested and data is available for both
# if extract_determine_database == "SWRunInformation", then use information in suitable columns of spreadsheet 'SWRunInformation' if available; if not available, then fall back to option 'order'
extract_determine_database <- "SWRunInformation" # one of c("order", "SWRunInformation")

# External datasets
do.ExtractExternalDatasets <- c(
		#Daily weather data for current conditions
		"GriddedDailyWeatherFromMaurer2002_NorthAmerica", 0,	#1/8-degree resolution
		"GriddedDailyWeatherFromDayMet_NorthAmerica", 0,	#1-km resolution
		"GriddedDailyWeatherFromNRCan_10km_Canada", 0,	# can only be used together with database
		"GriddedDailyWeatherFromNCEPCFSR_Global", 0, # can only be used together with database

		#Monthly PPT, Tmin, Tmax conditions: if using NEX or GDO-DCP-UC-LLNL, climate condition names must be of the form SCENARIO.GCM with SCENARIO being used for ensembles; if using climatewizard, climate condition names must be equal to what is in the respective directories
		"ExtractClimateChangeScenarios", 1,

		#Mean monthly wind, relative humidity, and 100% - sunshine
		"ExtractSkyDataFromNOAAClimateAtlas_USA", 0,
		"ExtractSkyDataFromNCEPCFSR_Global", 0,

		#Topography
		"ExtractElevation_NED_USA", 0,	#1-arcsec resolution, National Elevation Dataset (ned.usgs.gov), currently downloaded only for western US
		"ExtractElevation_HWSD_Global", 0, #30-arcsec resolution, Harmonized World Soil Database

		#Soil texture
		"ExtractSoilDataFromCONUSSOILFromSTATSGO_USA", 0,
		"ExtractSoilDataFromISRICWISEv12_Global", 0
)

chunk_size.options <- list(
		ExtractSkyDataFromNOAAClimateAtlas_USA = 10000,	# chunk_size == 1e4 && n_extract 6e4 will use about 30 GB of memory
		ExtractSkyDataFromNCEPCFSR_Global = 100,	# this is also OS-limited by the number of concurrently open files (on 'unix' platforms, check with 'ulimit -a')
		DailyWeatherFromNCEPCFSR_Global = 100	# this is also OS-limited by the number of concurrently open files (on 'unix' platforms, check with 'ulimit -a')
)

opt_climsc_extr <- c(
  # for each climate data set from which to extract, add an element like 'dataset1'
  # priority of extraction: dataset1, dataset2, ... if multiple sources provide data for a location
  # dataset = 'project_source' with
  #   - project = one string out of c("CMIP3", "CMIP5", "GeoMIP")
  #   - source = one string out of:
  #     - "ClimateWizardEnsembles_Global": mean monthly values at 50-km resolution for 2070-2099
  #     - "ClimateWizardEnsembles_USA": mean monthly change at 12-km resolution between 2070-2099 and 1971-2000
  #     - "BCSD_GDODCPUCLLNL_USA": monthly time series at 1/8-degree resolution
  #     - "BCSD_GDODCPUCLLNL_Global": monthly time series at 1/2-degree resolution
  #     - "BCSD_NEX_USA": monthly time series at 30-arcsec resolution; requires live internet access
    dataset1 = "CMIP5_BCSD_GDODCPUCLLNL_USA"
)

do.PriorCalculations <- c(
		"ExtendSoilDatafileToRequestedSoilLayers", 0,
		"EstimateConstantSoilTemperatureAtUpperAndLowerBoundaryAsMeanAnnualAirTemperature", 1,
		"EstimateInitialSoilTemperatureForEachSoilLayer", 1,
		"CalculateBareSoilEvaporationCoefficientsFromSoilTexture", 1
)

#------Time frames of simulation (if not specified in the treatment datafile)
#	current simulation years = simstartyr:endyr
#	years used for results = startyr:endyr
simstartyr  <- 1979
startyr <- getStartYear(simstartyr)
endyr <- 2010

#Future time period(s):
#	future simulation years = delta + simstartyr:endyr
#	future simulation years downscaled based on
#		- current conditions = DScur_startyr:DScur_endyr
#		- future conditions = DSfut_startyr:DSfut_endyr
# NOTE: Multiple time periods doesn't work with external type 'ClimateWizardEnsembles'
# Each row of 'future_yrs' will be applied to every climate.conditions
DScur_startyr <- startyr
DScur_endyr <- endyr

ctemp <- c("delta", "DSfut_startyr", "DSfut_endyr")
future_yrs <- matrix(c(c(d <- 40, startyr + d, endyr + d),
						c(d <- 90, startyr + d, endyr + d - 1)), # most GCMs don't have data for 2100
					ncol = length(ctemp), byrow = TRUE, dimnames = list(NULL, ctemp))
rownames(future_yrs) <- make.names(paste0("d", future_yrs[, "delta"], "yrs"), unique = TRUE)

#------Meta-information of input data
datafile.windspeedAtHeightAboveGround <- 2 #SoilWat requires 2 m, but some datasets are at 10 m, e.g., NCEP/CRSF: this value checks windspeed height and if necessary converts to u2
adjust.soilDepth <- FALSE # [FALSE] fill soil layer structure from shallower layer(s) or [TRUE] adjust soil depth if there is no soil texture information for the lowest layers
requested_soil_layers <- c(5, 10, 20, 30, 40, 50, 60, 70, 80, 100, 150)
increment_soiltemperature_deltaX_cm <- 5	# If SOILWAT soil temperature is simulated and the solution instable, then the soil profile layer width is increased by this value until a stable solution can be found or total failure is determined

#Climate conditions
climate.ambient <- "Current"	#Name of climatic conditions of the daily weather input when monthly climate perturbations are all off
#names of climate conditions/scenarios in the order of data in the climate scenarios datafile; this must have at least one entry (e.g., climate.ambient) and climate.ambient is forced to be the first entry
#All GCMs for CMIP5 by GDO-DCP-UC-LLNL: 37 RCP4.5, 35 RCP8.5
#Excluded: 'HadCM3' and 'MIROC4h' because data only available until 2035
climate.conditions <- c(climate.ambient,	"RCP45.CanESM2", "RCP45.CESM1-CAM5", "RCP45.HadGEM2-CC",
											"RCP85.CanESM2", "RCP85.CESM1-CAM5", "RCP85.HadGEM2-CC")

#Downscaling method: monthly scenario -> daily forcing variables
#Will be applied to each climate.conditions
downscaling.method			<- c("raw", "delta", "hybrid-delta-3mod")				#one or multiple of "raw", "delta" (Hay et al. 2002), "hybrid-delta" (Hamlet et al. 2010), or "hybrid-delta-3mod"

opt_DS <- list(
  daily_ppt_limit = 1.5,							#
  monthly_limit = 1.5,							#
  ppt_type = "detailed",							# either "detailed" or "simple"
  fix_spline = "attempt",						# one of "fail", "none" or "attempt"; only used if extrapol_type is using splines
    #	- "fail": downscaling fails if spline extrapolations fall outside estimated monthly extremes
    #	- "none": no correction for extrapolated monthly extreme values, but this will likely fail during correction of extreme daily PPT events
    #	- "attempt": repeated attempts with jittering data to fit spline extrapolations within estimated monthly extreme values
  extrapol_type = "linear_Thermessl2012CC.QMv1b",	# one of "linear_Boe", "linear_Thermessl2012CC.QMv1b", "linear_none", "tricub_fmm", "tricub_monoH.FC", "tricub_natural", "normal_anomalies"
    #	- "linear": Gudmundsson et al. 2012: "If new model values (e.g. from climate projections) are larger than the training values used to estimate the empirical CDF, the correction found for the highest quantile of the training period is used (Boe ?? et al., 2007; Theme??l et al., 2012)."
    #	- "tricub": I got really large output values, e.g., obs.hist = 54 cm, scen.fut = 64 cm, sbc.fut = 88 cm, hd.fut = 89 cm
    #	- "linear" (i.e., using Boe et al.'s correction) resulted for the same site to: obs.hist = 54 cm, scen.fut = 64 cm, sbc.fut = 75 cm, hd.fut = 75 cm
    # 	- "normal", but no implemented in qmap: Tohver et al. 2014, Appendix A, p. 6: "... values that are outside the observed quantile map (e.g. in the early parts of the 20th century) are interpolated using standard anomalies (i.e. number of standard deviations from the mean) calculated for the observed data and GCM data. Although this approach ostensibly assumes a normal distribution, it was found during testing to be much more stable than attempts to use more sophisticated approaches. In particular, the use of Extreme Value Type I or Generalized Extreme Value distributions for extending the tail of the probability distributions were both found to be highly unstable in practice and introduced unacceptable daily extremes in isolated grid cells. These errors occur because of irregularities in the shapes of the CDFs for observed and GCM data, which relates in part to the relatively small sample size used to construct the monthly CDFs (i.e. n = 30)."
  sigmaN = 6,										# test whether data distributions are within sigmaN * sd of mean
  PPTratioCutoff = 10								# above and below that value use additive instead of multiplicative adjustments for precipitation; 3 was too small -> resulting in too many medium-sized ppt-event
)

#Climate ensembles created across scenarios
ensemble.families <- NULL #c("RCP45", "RCP85") # NULL or from c("SRESA2", "SRESA1B", "SRESB1"); this variable defines the groups for which ensembles of climate scenarios are calculated; corresponds to first part of scenario name
ensemble.levels <- c(2, 8, 15)  #if(!is.null(ensemble.families)) then this needs to have at least one value; this variable defines which ranked climate.conditions the ensembles are representing for each ensemble.families
save.scenario.ranks <- TRUE #if TRUE then for each ensemble.levels a file is saved with the scenario numbers corresponding to the ensemble.levels

#------Names of files that contain input data or treatment codes
datafile.SWRunInformation <- "SWRuns_InputMaster_Test_v11.csv"

datafile.soillayers <- "SWRuns_InputData_SoilLayers_v9.csv"
datafile.treatments <- "SWRuns_InputData_TreatmentDesign_v14.csv"
datafile.Experimentals <- "SWRuns_InputData_ExperimentalDesign_v04.csv"

if ((any(actions == "external") || any(actions == "create") || any(actions == "execute") || any(actions == "aggregate")) ) {	#input datafiles in the folder ./datafiles
	datafile.climatescenarios <- "SWRuns_InputData_ClimateScenarios_Change_v11.csv"
	datafile.climatescenarios_values <- "SWRuns_InputData_ClimateScenarios_Values_v11.csv"
	datafile.cloud <- "SWRuns_InputData_cloud_v10.csv"
	datafile.prod <- "SWRuns_InputData_prod_v10.csv"
	datafile.siteparam <- "SWRuns_InputData_siteparam_v13.csv"
	datafile.soils <- "SWRuns_InputData_soils_v11.csv"
	datafile.weathersetup <- "SWRuns_InputData_weathersetup_v10.csv"
}
if (( any(actions == "external") || any(actions == "create") || any(actions == "execute") || any(actions == "aggregate")) ) {	#input files in sub-folders ./treatments
	trfile.LookupClimatePPTScenarios <- "climate.ppt.csv"
	trfile.LookupClimateTempScenarios <- "climate.temp.csv"
	trfile.LookupShiftedPPTScenarios <- "shifted.ppt.csv"
	trfile.LookupEvapCoeffFromTable <- "BareSoilEvaporationCoefficientsPerSoilLayer.csv"
	trfile.LookupTranspCoeffFromTable <- "TranspirationCoefficients_v2.csv"
	trfile.LookupTranspRegionsFromTable <- "TranspirationRegionsPerSoilLayer.csv"
	trfile.LookupSnowDensityFromTable <- "MeanMonthlySnowDensities_v2.csv"
	trfile.LookupVegetationComposition <- "VegetationComposition_MeanMonthly_v5.csv"
}

datafile.SWRWinputs_preprocessed <- "SWRuns_InputAll_PreProcessed.RData" # Storage file of input data for repeated access (faster) instead of re-reading from (slower) csv files if flag 'usePreProcessedInput' is TRUE

#------Northern/Southern Hemisphere adjustments
accountNSHemispheres_agg <- TRUE	#if TRUE and latitude < 0 (i.e., southern hemisphere) then the counting of timing variables is shifted by 6 months (e.g., July becomes 1st month, etc.)
accountNSHemispheres_veg <- TRUE 	#if TRUE and latitude < 0 (i.e., southern hemisphere) then shift monthly production values in prod.in file by six months

#------Output Header Columns------#
Index_RunInformation <- NULL #indices of columns of 'SWRunInformation', e.g, c(3, 7:9), or NULL, used for outputting SoilWat-run information in addition to create_treatments and climate scenario

#------Select aggregated output: time scale and variable groups
#simulation_timescales is at least one of c("daily", "weekly", "monthly", "yearly")
simulation_timescales <- c("daily", "monthly", "yearly")
#turn aggregation for variable groups on (1) or off (0), don't delete any variable group labels
output_aggregates <- c(
					#---Aggregation: SoilWat inputs
						"input_SoilProfile", 1,
            "input_FractionVegetationComposition", 1,
						"input_VegetationBiomassMonthly", 1,
						"input_VegetationPeak", 1,
						"input_Phenology", 1,
						"input_TranspirationCoeff", 1,
						"input_ClimatePerturbations", 1,
					#---Aggregation: Climate and weather
						"yearlyTemp", 1,
						"yearlyPPT", 1,
						"dailySnowpack", 1,
						"dailyFrostInSnowfreePeriod", 1,
						"dailyHotDays", 1,
						"dailyWarmDays", 1,
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
						"dailyNRCS_SoilMoistureTemperatureRegimes", 0, #Requires at least soil layers at 10, 20, 30, 50, 60, 90 cm
						"dailyNRCS_Chambers2014_ResilienceResistance", 0, #Requires "dailyNRCS_SoilMoistureTemperatureRegimes"
					  "dailyNRCS_Maestas2016_ResilienceResistance", 0,
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
						"dailyRegeneration_bySWPSnow", 0,
						"dailyRegeneration_GISSM", 0
)

#select variables to aggregate daily mean and SD, if "daily" is in simulation_timescales

#options: NULL or at least one of c("AET", "Transpiration", "EvaporationSoil", "EvaporationSurface", "EvaporationTotal", "VWCbulk", "VWCmatric", "SWCbulk", "SWPmatric", "Snowpack", "SWAbulk", "Rain", "Snowfall", "Snowmelt", "SnowLoss", "Runoff", "Infiltration", "DeepDrainage", "PET", "TotalPrecipitation", "TemperatureMin", "TemperatureMax", "SoilTemperature")
output_aggregate_daily <- c("AET", "Transpiration", "EvaporationSoil", "EvaporationSurface", "EvaporationTotal", "VWCbulk", "VWCmatric", "SWCbulk", "SWPmatric", "Snowpack", "SWAbulk", "Rain", "Snowfall", "Snowmelt", "SnowLoss", "Runoff", "Infiltration", "DeepDrainage", "PET", "TotalPrecipitation", "TemperatureMin", "TemperatureMax", "SoilTemperature")
#select variables to output as aggregated yearly time series
ouput_aggregated_ts <- NULL #c("Regeneration")


#------Parameters used in output aggregation
#critical soil water potential
SWPcrit_MPa <- c(-1.5, -3.0, -3.5, -3.9) #e.g., -1.5 or c(-3.0, -3.9, -4.9); critical soil water potential(s) to calculate 'dry' and 'wet' soils (aka wilting point) and available soil water

#critical temperatures
Tmin_crit_C <- c(-15, -9, 0)	#e.g., 0 or c(-15, -9, 0)
Tmax_crit_C <- c(34, 40)	#e.g., 34 or c(34, 40)
Tmean_crit_C <- c(5, 15, 25, 35)

#degree-days and suitable temperature
DegreeDayBase <- 0 # (degree C) base temperature above which degree-days are accumulated

#soil layers
Depth_TopLayers  <- 20 				#cm, distinguishes between top and bottom soil layer for overall data aggregation
daily_lyr_agg <- list(
      do = TRUE,				# if TRUE, then aggregate soil layers into 1-4 layers for mean/SD daily values; if FALSE, then use each soil layer
      first_cm = 10, 	  # cm, distinguishes between first and second soil layer for average daily data aggregation
      second_cm = 20, 	# cm or NULL(=deepest soil layer), distinguishes between first and second soil layer for average daily data aggregation
      third_cm = 60, 	  # cm, NULL(=deepest soil layer), or NA(=only two aggregation layers), distinguishes between second and third soil layer for average daily data aggregation
      fourth_cm = NULL) # cm, NULL(=deepest soil layer), or NA(=only three aggregation layers), distinguishes between third and fourth soil layer for average daily data aggregation

#regeneration: germination and establishment
season.start <- "LastSnow" # either doy or "LastSnow"
season.end <- "FirstSnow" # either doy or "FirstSnow"
germination.duration <- 7 # in days
germination.swp.surface <- -0.2 # in MPa, duration must have at least x MPa
establishment.duration <- 14 # in days
establishment.swp.surface <- -0.4 # in MPa, duration must have at least x MPa
establishment.delay <- 1 # start of establishment needs to occur latest x days after end of germination

#daily weather frequency distributions
bin.prcpSizes <- 5	#bins of x mm precipitation event sizes
bin.prcpfreeDurations <- 10	#bins of x consecutive days without precipitation

#coefficients: potential natural vegetation based on climate data (Jose Paruelo et al. 1996, 1998)
shrub.fraction.limit <- 0.2 	#page 1213: 0.2 in Paruelo JM, Lauenroth WK (1996) Relative abundance of plant functional types in grasslands and shrublands of North America. Ecological Applications, 6, 1212-1224.
growing.season.threshold.tempC <- 10 # based on Trewartha's D temperateness definition (with >=4 & < 8 months with > 10C)
growing.season.threshold.tempC <- 4 # based on standard input of mean monthly biomass values for vegetation composition


#------SoilWat files
sw <- "sw_v31"
sw.inputs <- "Input"	#must be string of length > 0; i.e. not compatible with SoilWat versions < 21
sw.outputs <- "Output"	#sw_v20+: "Output", earlier versions ""
swFilesIn <- "files_v30.in"

if(any(actions == "create") || any(actions == "execute") || any(actions == "aggregate") ) {
	#sw input file names
	swOutSetupIn <- "outsetup_v20.in"
	swcsetupin <- "swcsetup.in"
	soilsin <- "soils_v30.in"
	yearsin <- "years.in"
	estabin <- "estab.in"
	weatherin <- "weathsetup_v20.in"
	cloudin <- "cloud_v20.in"
	prodin <- "sbe_prod_v31.in"
	siteparamin <- "siteparam_v26.in"
	filebasename.WeatherDataYear <- "weath"

	#characteristics of sw input files
	soilsin.firstDataLine <- 18	# 18, if soilsin >= v23; 17, if soilsin < v23

	sw_aet			<- "AET"
	sw_deepdrain	<- "DEEPSWC"
	sw_estabs		<- "ESTABL"
	sw_evsoil		<- "EVAPSOIL"
	sw_evapsurface	<- "EVAPSURFACE"
	sw_hd			<- "HYDRED"
	sw_inf_soil		<- "SOILINFILT"
	sw_interception	<- "INTERCEPTION"
	sw_percolation	<- "LYRDRAIN"
	sw_pet			<- "PET"
	sw_precip		<- "PRECIP"
	sw_runoff		<- "RUNOFF"
	sw_snow			<- "SNOWPACK"
	sw_soiltemp		<- "SOILTEMP"
	sw_surfaceWater	<- "SURFACEWATER"
	sw_swp			<- "SWPMATRIC"
	sw_swabulk		<- "SWABULK"
	sw_swcbulk		<- "SWCBULK"
	sw_temp			<- "TEMP"
	sw_transp		<- "TRANSP"
	sw_vwcbulk		<- "VWCBULK"
	sw_vwcmatric	<- "VWCMATRIC"
	sw_wetdays		<- "WETDAY"
	sw_logfile		<- "LOG"
}

##############################################################################
########################Source of the code base###############################

#if (!interactive())
  source(file.path(dir.code, "R", "2_SWSF_p4of5_Code_v51.R"), verbose = FALSE, chdir = FALSE)
