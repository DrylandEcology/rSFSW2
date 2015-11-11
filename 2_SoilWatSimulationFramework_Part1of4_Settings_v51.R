#--------------------------------------------------------------------------------------------------#

#------------------------FRAMEWORK FOR SOILWAT SIMULATIONS: CREATING SIMULATION RUNS, EXECUTING SIMULATIONS, AND AGGREGATING OUTPUTS

#--------------------------------------------------------------------------------------------------#

#------CODE developed and written by
# - Daniel R Schlaepfer (dschlaep@uwyo.edu, drs): 2009-2014
# - Donovan Miller (dlm): 2012
# - Ryan Murphy (rjm): 2012-2015
#for contact and further information see also: sites.google.com/site/drschlaepfer

#The R code below was tested on R version 3.1.1

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
rm(list=ls(all=TRUE))

#------Overall timing
t.overall <- Sys.time()
be.quiet <- FALSE
print.debug <- if(interactive()) TRUE else FALSE

#------Mode of framework
minVersionRsoilwat <- "0.31.10"
num_cores <- 2
parallel_backend <- "mpi" #"snow" or "multicore" or "mpi"
parallel_runs <- if(interactive()) FALSE else TRUE

#------Rmpi Jobs finish within Wall Time------#
MaxRunDurationTime <- 1.5 * 60 *60 #Set the time duration for this job [in seconds], i.e. Wall time. As time runs out Rmpi will not send more work. Effects Insert into database and ensembles.
MaxDoOneSiteTime <- (MaxRunDurationTime - 11*60) #This will stop new Rmpi jobs at 'x' seconds before MaxRunDuration expires.
MinTimeConcat <- 10 * 60 * 60 #This is the minimum time remaining after execution needed to begin concat
MaxConcatTime <- 35 * 60 #This will stop any new sql file concat job at 'x' seconds before MaxRunDuration expires.

#------Repository in case installation of additional R packages is required
url.Rrepos <- "http://cran.us.r-project.org"

#--------------------------------------------------------------------------------------------------#
#------------------------USER INPUT

#------Set paths to simulation framework folders
#parent folder of simulation project
dir.prj <- "~/Documents/drschlaepfer/2_Research/200907_UofWyoming_PostDoc/Projects_My/Product_PowellCenter/6_Projects_Year1/Prj01_Texture/1_PC_TempDry_Simulations_Prj01_r2mini"
if(interactive()) setwd(dir.prj)
dir.prj <- dir.runs <- getwd()
	
#parent folder containing external data
dir.external <- "/Users/drschlaep/Documents/drschlaepfer/2_Research/200907_UofWyoming_PostDoc/Projects_My/Software/SoilWat/SoilWat_SimulationFrameworks/SoilWat_DataSet_External"

#paths to sub-folder hierarchy
dir.in <- file.path(dir.prj, "1_Data_SWInput")	#path to input data of SoilWat-runs)
dir.sw.dat <- file.path(dir.in, "datafiles")	#folder with datafiles to add information to SoilWat input files
dir.sw.in <- file.path(dir.in, "swrun")	#folder with complete SoilWat run setup (without yearly weather files, cloudin is in 'Input' folder and not in weather-folder: needs to be moved appropiately)
dir.sw.in.tr <- file.path(dir.in, "treatments")	#folder with treatment input files according to treatment instructions
dir.sw.in.reg <- file.path(dir.in, "regeneration")	#folder with regeneration files, one for each species = run of 'dailyRegeneration_byTempSWPSnow'
dir.sw.runs <- file.path(dir.runs, "3_Runs")	#path to SoilWat-runs 
dir.out <- file.path(dir.prj, "4_Data_SWOutputAggregated")	#path to aggregated output


#------Define actions to be carried out by simulation framework
#actions are at least one of c("external", "create", "execute", "aggregate", "concatenate", "ensemble")
actions <- c("external", "create", "execute", "aggregate", "concatenate", "ensemble")#
#continues with unfinished part of simulation after abort if TRUE
continueAfterAbort <- TRUE
#stores for each SoilWat simulation a folder with inputs and outputs if TRUE
saveSoilWatInputOutput <- FALSE
#store data in big input files for experimental design x treatment design
makeInputForExperimentalDesign <- FALSE
#check completeness of SoilWat simulation directories and of temporary output aggregation files; create a list with missing directories and files
checkCompleteness <- FALSE

#------Define how aggregated output should be handled:
cleanDB <- FALSE #This will wipe all the Tables at the begining of a run. Becareful not to wipe your data.
deleteTmpSQLFiles <- TRUE
copyCurrentConditionsFromTempSQL <- TRUE
copyCurrentConditionsFromDatabase <- FALSE #Creates a copy of the main database containing the scenario==climate.ambient subset
ensembleCollectSize <- 500 #This value is the chunk size for reads of 'runID' from the database, i.e., chunk size = ensembleCollectSize * scenario_No. Yellowstone 500 seems to work. Balance between available memory, cores, read/write times, etc..

#------Define type of simulations and source of input data
#Daily weather data: must be one of dailyweather_options; WeatherFolder in MasterInput.csv, treatmentDesign.csv, or experimentalDesign.csv
# If a run has multiple sources for daily weather, then take the one in the first position of dailyweather_options if availble, if not then second etc.
#	do not change/remove/add entries; only re-order to set different priorities
dailyweather_options <- c("LookupWeatherFolder", "NRCan_10km_Canada", "Maurer2002_NorthAmerica", "NCEPCFSR_Global")
#Daily weather database
getCurrentWeatherDataFromDatabase <- TRUE
getScenarioWeatherDataFromDatabase <- TRUE
dbWeatherDataFile <- file.path(dir.in, "dbWeatherData.sqlite3")
createAndPopulateWeatherDatabase <- FALSE #TRUE, will create a new(!) database and populate with data

#Indicate if actions contains "external" which external information (1/0) to obtain from dir.external, don't delete any labels; GIS extractions not supported on JANUS
# - Elevation: 'ExtractElevation_NED_USA' has priority over 'ExtractElevation_HWSD_Global' on a per site basis if both are requested and data is available for both
# - Soil texture: 'ExtractSoilDataFromCONUSSOILFromSTATSGO_USA' has priority over 'ExtractSoilDataFromISRICWISEv12_Global' on a per site basis if both are requested and data is available for both
#extract_gridcell_or_point: currently, only implemented for "ExtractSoilDataFromISRICWISEv12_Global"
extract_gridcell_or_point <- "point" # one of c("point", "gridcell"), whether to extract for point locations or averaged over a cell area
gridcell_resolution <- 1/8
do.ExtractExternalDatasets <- c(
		#Daily weather data for current conditions
		"GriddedDailyWeatherFromMaurer2002_NorthAmerica", 0,	#1/8-degree resolution
		"GriddedDailyWeatherFromNRCan_10km_Canada", 0,	# can only be used together with database
		"GriddedDailyWeatherFromNCEPCFSR_Global", 0, # can only be used together with database
		
		#Mean monthly PPT, Tmin, Tmax conditions: if using NEX or GDO-DCP-UC-LLNL, climate condition names must be of the form SCENARIO.GCM with SCENARIO being used for ensembles; if using climatewizard, climate condition names must be equal to what is in the respective directories
		#CMIP3
		"ExtractClimateChangeScenarios_CMIP3_ClimateWizardEnsembles_Global", 0, #50-km resolution for mean of 2070-2099
		"ExtractClimateChangeScenarios_CMIP3_ClimateWizardEnsembles_USA", 0, #12-km resolution for mean change between 2070-2099 and 1971-2000
		"ExtractClimateChangeScenarios_CMIP3_BCSD_GDODCPUCLLNL_USA", 0,	#1/8-degree resolution
		"ExtractClimateChangeScenarios_CMIP3_BCSD_GDODCPUCLLNL_Global", 0,	#1/2-degree resolution
		#CMIP5
		"ExtractClimateChangeScenarios_CMIP5_BCSD_GDODCPUCLLNL_USA", 0,	#1/8-degree resolution
		"ExtractClimateChangeScenarios_CMIP5_BCSD_GDODCPUCLLNL_Global", 0,	#1/2-degree resolution
		"ExtractClimateChangeScenarios_CMIP5_BCSD_NEX_USA", 0,	#30-arcsec resolution; requires live internet access
		
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

do.PriorCalculations <- c(
		"EstimateConstantSoilTemperatureAtUpperAndLowerBoundaryAsMeanAnnualAirTemperature", 0,
		"EstimateInitialSoilTemperatureForEachSoilLayer", 0,
		"CalculateBareSoilEvaporationCoefficientsFromSoilTexture", 0
)

#------Time frame of simulation: if not specified in the treatment datafile
#year when SoilWat starts the simulation
simstartyr  <- 1979
#first year that is used for output aggregation, e.g., simstartyr + 1
getStartYear <- function(simstartyr){
	return(simstartyr + 1)
}
startyr <- getStartYear(simstartyr)
#year when SoilWat ends the simulation
endyr <- 2010

#------Meta-information of input data
datafile.windspeedAtHeightAboveGround <- 2 #SoilWat requires 2 m, but some datasets are at 10 m, e.g., NCEP/CRSF: this value checks windspeed height and if necessary converts to u2
adjust.soilDepth <- FALSE # [FALSE] fill soil layer structure from shallower layer(s) or [TRUE] adjust soil depth if there is no soil texture information for the lowest layers

#Climate conditions
climate.ambient <- "Current"	#Name of climatic conditions of the daily weather input when monthly climate perturbations are all off
#names of climate conditions/scenarios in the order of data in the climate scenarios datafile; this must have at least one entry (e.g., climate.ambient) and climate.ambient is forced to be the first entry
#All GCMs for CMIP5 by GDO-DCP-UC-LLNL: 37 RCP4.5, 35 RCP8.5
#Excluded: 'HadCM3' and 'MIROC4h' because data only available until 2035
climate.conditions <- c(climate.ambient,	"RCP45.ACCESS1-0", "RCP45.ACCESS1-3", "RCP45.bcc-csm1-1", "RCP45.bcc-csm1-1-m", "RCP45.BNU-ESM", "RCP45.CanESM2", "RCP45.CCSM4", "RCP45.CESM1-BGC", "RCP45.CESM1-CAM5", "RCP45.CMCC-CM", "RCP45.CNRM-CM5", "RCP45.CSIRO-Mk3-6-0", "RCP45.EC-EARTH", "RCP45.FGOALS-g2", "RCP45.FGOALS-s2", "RCP45.FIO-ESM", "RCP45.GFDL-CM3", "RCP45.GFDL-ESM2G", "RCP45.GFDL-ESM2M", "RCP45.GISS-E2-H-CC",	"RCP45.GISS-E2-R", "RCP45.GISS-E2-R-CC",	"RCP45.HadGEM2-AO", "RCP45.HadGEM2-CC", "RCP45.HadGEM2-ES", "RCP45.inmcm4", "RCP45.IPSL-CM5A-LR", "RCP45.IPSL-CM5A-MR", "RCP45.IPSL-CM5B-LR", "RCP45.MIROC-ESM", "RCP45.MIROC-ESM-CHEM", "RCP45.MIROC5", "RCP45.MPI-ESM-LR", "RCP45.MPI-ESM-MR", "RCP45.MRI-CGCM3", "RCP45.NorESM1-M", "RCP45.NorESM1-ME",
											"RCP85.ACCESS1-0", "RCP85.ACCESS1-3", "RCP85.bcc-csm1-1", "RCP85.bcc-csm1-1-m", "RCP85.BNU-ESM", "RCP85.CanESM2", "RCP85.CCSM4", "RCP85.CESM1-BGC", "RCP85.CESM1-CAM5", "RCP85.CMCC-CM", "RCP85.CNRM-CM5", "RCP85.CSIRO-Mk3-6-0", "RCP85.EC-EARTH", "RCP85.FGOALS-g2", "RCP85.FGOALS-s2", "RCP85.FIO-ESM", "RCP85.GFDL-CM3", "RCP85.GFDL-ESM2G", "RCP85.GFDL-ESM2M", 						"RCP85.GISS-E2-R", 							"RCP85.HadGEM2-AO", "RCP85.HadGEM2-CC", "RCP85.HadGEM2-ES", "RCP85.inmcm4", "RCP85.IPSL-CM5A-LR", "RCP85.IPSL-CM5A-MR", "RCP85.IPSL-CM5B-LR", "RCP85.MIROC-ESM", "RCP85.MIROC-ESM-CHEM", "RCP85.MIROC5", "RCP85.MPI-ESM-LR", "RCP85.MPI-ESM-MR", "RCP85.MRI-CGCM3", "RCP85.NorESM1-M", "RCP85.NorESM1-ME")
#Future time period(s) simulated = delta + simstartyr:endyr; also used to extract external climate conditions
#Will be applied to each climate.conditions
#Multiple time periods doesn't work with external type 'ClimateWizardEnsembles'
deltaFutureToSimStart_yr <- c(50, 90)

#Downscaling method: monthly scenario -> daily forcing variables
#Will be applied to each climate.conditions
downscaling.method <- c("hybrid-delta")	#one or multiple of "raw", "delta" (Hay et al. 2002), or "hybrid-delta" (Hamlet et al. 2010)

#Climate ensembles created across scenarios
ensemble.families <- c("RCP45", "RCP85") # NULL or from c("SRESA2", "SRESA1B", "SRESB1"); this variable defines the groups for which ensembles of climate scenarios are calculated; corresponds to first part of scenario name
ensemble.levels <- c(2, 8, 15)  #if(!is.null(ensemble.families)) then this needs to have at least one value; this variable defines which ranked climate.conditions the ensembles are representing for each ensemble.families
save.scenario.ranks <- TRUE #if TRUE then for each ensemble.levels a file is saved with the scenario numbers corresponding to the ensemble.levels

#------Names of files that contain input data or treatment codes
datafile.SWRunInformation <- "SWRuns_InputMaster_TemperateArid_v11.csv"

datafile.soillayers <- "SWRuns_InputData_SoilLayers_WISE_v9.csv"	
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

#------Northern/Southern Hemisphere adjustments
accountNSHemispheres_agg <- TRUE	#if TRUE and latitude < 0 (i.e., southern hemisphere) then the counting of timing variables is shifted by 6 months (e.g., July becomes 1st month, etc.)
accountNSHemispheres_veg <- TRUE 	#if TRUE and latitude < 0 (i.e., southern hemisphere) then shift monthly production values in prod.in file by six months

#------Output Header Columns------#
Index_RunInformation <- c(2:3, 5:11, 16:17) #indices of columns of 'SWRunInformation', e.g, c(3, 7:9), or NULL, used for outputting SoilWat-run information in addition to create_treatments and climate scenario

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
						"dailyPrecipitationEventSizeDistribution", 1,
						"yearlyAET", 1,
						"yearlyPET", 1,
						"monthlySeasonalityIndices", 1,
					#---Aggregation: Climatic dryness
						"yearlymonthlyTemperateDrylandIndices", 1,
						"yearlyDryWetPeriods", 1,
						"dailyWeatherGeneratorCharacteristics", 1,	#Takes about .5120 seconds for 33 scenarios is about 
						"dailyPrecipitationFreeEventDistribution", 1,
						"monthlySPEIEvents", 1,
					#---Aggregation: Climatic control
						"monthlyPlantGrowthControls", 1,
						"dailyC4_TempVar", 1,
						"dailyDegreeDays", 1,
						"dailyNRCS_SoilMoistureTemperatureRegimes", 1, #Requires at least soil layers at 10, 20, 30, 50, 60, 90 cm
						"dailyNRCS_Chambers2014_ResilienceResistance", 1, #Requires "dailyNRCS_SoilMoistureTemperatureRegimes"
					#---Aggregation: Yearly water balance
						"yearlyWaterBalanceFluxes", 1,
						"dailySoilWaterPulseVsStorage", 1,
					#---Aggregation: Daily extreme values
						"dailyTranspirationExtremes", 1,
						"dailyTotalEvaporationExtremes", 1,
						"dailyDrainageExtremes", 1,
						"dailyInfiltrationExtremes", 1,
						"dailyAETExtremes", 1,
						"dailySWPextremes", 1,						#Takes about .7630 seconds for 33 scenarios is about .419 minutes
						"dailyRechargeExtremes", 1,
					#---Aggregation: Ecological dryness
						"dailyWetDegreeDays", 1,
						"monthlySWPdryness", 1,
						"dailySWPdrynessANDwetness", 1, 			#Takes about 3.200 seconds for 33 scenarios is about 1.76 minutes
						"dailySuitablePeriodsDuration", 1,
						"dailySuitablePeriodsAvailableWater", 1,
						"dailySuitablePeriodsDrySpells", 1,
						"dailySWPdrynessDurationDistribution", 1,	#Takes about .8132 seconds for 33 scenarios is about .447 minutes
						"dailySWPdrynessEventSizeDistribution", 1,	#Takes about .5120 seconds for 33 scenarios is about .2819334
						"dailySWPdrynessIntensity", 1,
					#---Aggregation: Mean monthly values
						"monthlyTemp", 1,
						"monthlyPPT", 1,
						"monthlySnowpack", 1,
						"monthlySoilTemp", 0,
						"monthlyRunoff", 1,
						"monthlyHydraulicRedistribution", 1,
						"monthlyInfiltration", 1,
						"monthlyDeepDrainage", 1,
						"monthlySWPmatric", 1,
						"monthlyVWCbulk", 1,
						"monthlyVWCmatric", 1,
						"monthlySWCbulk", 1,
						"monthlySWAbulk", 1,
						"monthlySWAmatric", 1,
						"monthlyTranspiration", 1,
						"monthlySoilEvaporation", 1,
						"monthlyAET", 1,
						"monthlyPET", 1,
						"monthlyAETratios", 1,
						"monthlyPETratios", 1,
					#---Aggregation: Potential regeneration
						"dailyRegeneration_bySWPSnow", 0,
						"dailyRegeneration_GISSM", 0
)

#select variables to aggregate daily mean and SD, if "daily" is in simulation_timescales 

#options: NULL or at least one of c("AET", "Transpiration", "EvaporationSoil", "EvaporationSurface", "EvaporationTotal", "VWCbulk", "VWCmatric", "SWCbulk", "SWPmatric", "Snowpack", "SWAbulk", "Rain", "Snowfall", "Snowmelt", "SnowLoss", "Runoff", "Infiltration", "DeepDrainage", "PET", "TotalPrecipitation", "TemperatureMin", "TemperatureMax", "SoilTemperature")
output_aggregate_daily <- c("SWPmatric")
#select variables to output as aggregated yearly time series
ouput_aggregated_ts <- NULL #c("Regeneration")


#------Parameters used in output aggregation
#critical soil water potential
SWPcrit_MPa <- c(-1.5, -3.0, -3.5, -3.9) #e.g., -1.5 or c(-3.0, -3.9, -4.9); critical soil water potential(s) to calculate 'dry' and 'wet' soils (aka wilting point) and available soil water

#critical temperatures
Tmin_crit_C <- c(-15, -9, 0)	#e.g., 0 or c(-15, -9, 0)
Tmax_crit_C <- c(34, 40)	#e.g., 34 or c(34, 40)

#degree-days and suitable temperature
DegreeDayBase <- 0 # (degree C) base temperature above which degree-days are accumulated

#soil layers
Depth_TopLayers  <- 20 				#cm, distinguishes between top and bottom soil layer for overall data aggregation
AggLayer.daily <- TRUE				#if TRUE, then aggregate soil layers into 1-4 layers for mean/SD daily values; if FALSE, then use each soil layer
Depth_FirstAggLayer.daily  <- 10 	#cm, distinguishes between first and second soil layer for average daily data aggregation
Depth_SecondAggLayer.daily  <- 20 	#cm or NULL(=deepest soil layer), distinguishes between first and second soil layer for average daily data aggregation
Depth_ThirdAggLayer.daily  <- 60 	#cm, NULL(=deepest soil layer), or NA(=only two aggregation layers), distinguishes between second and third soil layer for average daily data aggregation
Depth_FourthAggLayer.daily  <- NULL	#cm, NULL(=deepest soil layer), or NA(=only three aggregation layers), distinguishes between third and fourth soil layer for average daily data aggregation

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
	sw_swamatric	<- "SWAMATRIC"
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

if(!interactive()) source("2_SoilWatSimulationFramework_Part4of4_Code_v51.R", echo=FALSE, keep.source=FALSE)

