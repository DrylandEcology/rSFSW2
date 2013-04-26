#--------------------------------------------------------------------------------------------------#

#------------------------FRAMEWORK FOR SOILWAT SIMULATIONS: CREATING SIMULATION RUNS, EXECUTING SIMULATIONS, AND AGGREGATING OUTPUTS

#--------------------------------------------------------------------------------------------------#

#------CODE developed and written by
# - Daniel R Schlaepfer (dschlaep@uwyo.edu, drs): 2009-2013
# - Donovan Miller (dlm): 2012
# - Ryan Murphy (rjm): 2012-2013
#for contact and further information see also: sites.google.com/site/drschlaepfer

#The R code below was tested on R versions 2.13.0 and 2.15.0-2.15.2

#------DISCLAIMER: This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#------SoilWat Simulation Framework, SWSF

#------NOTES:
#	- the code performs only rudimentary error checking and handling
#	- SoilWat is forced by:
#		- daily: rainfall (cm), maximum and minimum air temperature at 2-m height (C)
#		- mean monthly: wind speed at 2-m height (miles/h before v24, m/s starting with v24), relative humidity at 2-m height (%), and cloud cover (%)

#------VERSIONING
#	v37 (20111213):
#	v38	(20111227-20120104):
#		- added to datafile.treatments: YearStart and YearEnd to adjust simulation period for each SoilWat-run individually
#		- adjusted for treatment = 'LookupWeatherFolder' so that if the weather folder does not contain cloudin and treatment != 'cloudin', then cloudin is copied from 'dir.sw.in'
#		- fixed bug in function 'TranspCoeffByVegType' where if 'DepthCM', the coefficients for the deepest soil layer were not added up if the soil layer was deeper than the coefficients
#		- fixed bug in 'ExtractSkyDataFromNOAAClimateAtlasUS': codes for cover and wind were erroneous
#		- fixed bug in 'ExtractSoilDataFromCONUSSOILFromSTATSGO': extraction of values was erroneous
#		- fixed bug in 'add soil information to soilsin': re-adjustment of deepest soil layer when multiple lowest soil layer data was empty, was erroneous
#		- fixed bug in function 'setSecondLayer' and 'setThirdLayer': DeepestSecondDailyAggLayer and DeepestThirdDailyAggLayer were calculated erroneously if soil layer re-adjustment decreased number of soil layers below desired value
#		- fixed bug in 'determine soil layer structure': if not create, then re-adjustment of deepest soil layer when multiple lowest soil layer data was empty, was erroneous
#		- converted header information from levels of factors to characters if necessary
#		- function 'SWPtoVWC': can now handle NAs in sand or clay
#		- improved dir.remove: removes now also files in recursive directories
#	v39 (20120104-20120113):
#		- aggregated output files contain only selected output variables and no longer all
#		- added aggregated output:
#			- sd for climate and weather, for climate correlations, and for water balance fluxes
#			- group 'dailyWeatherEventSizeDistribution' including frequency distribution of prcp-event size and of duration of dry days (no prcp); also added two constants to determine bin sizes
#			- group 'dailySWPdrynessDurationDistribution' including cummulative frequency distribution values (i.e., sampled at deciles plus at extremes) of duration of dry soils in top and bottom soils for each of the four seasons for each of the SWP.crit
#		- improved 'LookupEvapCoeffFromTable' and 'LookupTranspRegionsFromTable': if datafile has from previous run more soil layers than current, these old data are now deleted
#		- introduced function 'simTiming' which produces a list 'simTime': to better handle run-specific timing
#		- fixed bug in 'dailySnowpack': checks now that there are enough snow years available (i.e., 2) to calculate response variables
#	v40 (20120113-20120123):
#		- added flag 'deleteSoilWatOutputAfterAggregation': deletes all SoilWat simulation output after aggregation for run is complete, but maintains the SoilWat input file structure; this safes space (e.g., for 1000 years of complete output = 490 MB); if other response variables are needed, then only actions==c("execute", "aggregate") are needed, but not "create"
#		- fixed bug in column names of daily aggregations 
#		- added vector 'delete.exceptions': if 'deleteSoilWatOutputAfterAggregation' is turned on, then it doesn't delete files listed
#		- fixed bug in 'dailyWeatherEventSizeDistribution': incorrect result or subscript out of bounds if no data present for a summary bin
#	v41 (20120123-20120217):
#		- added flag 'continueAfterAbort': if true, then performs actions only if not already completed, i.e., SoilWat-run folder present for 'create', output folder not empty unless deleteSoilWatOutputAfterAggregation==TRUE & delete.exceptions==NULL for 'execute', and appropriate temporary output files present for 'aggregate'
#		- fixed bug in calculation of 'doy_ForEachUsedDay', 'year_ForEachUsedDay', and 'month_ForEachUsedDay': required output file may not have been available if 'deleteSoilWatOutputAfterAggregation' was on and actions != execute
#		- fixed bug in daily aggregation: rain, snowfall, snowmelt
#		- added daily aggregation response variables: PET, TotalPrecipitation
#		- fixed bug in 'dailyWeatherEventSizeDistribution': subscript out of bounds if data present for only one bin larger than the largest summary bin
#		- changed daily aggregation 'SWA' from daily means of SWA (truncated at 0) to daily SWA based on daily means of SWC: otherwise if a single day had SWA > 0 then daily mean for that doy > 0 even though daily average of SWP could be < SWPcrit
#		- added external data source option for 'CalculateBareSoilEvaporationCoefficientsFromSoilTexture'
#		- added code to delete duplicated input files originating by copying 'swruns' and then adding treatment files
#		- fixed bug in calculation of number of layers if soildepth was exactly a lower layer boundary
#		- column names of daily aggregation 'SWA' include now value of SWPcrit instead an index
#		- added daily aggregation response variables: minimum and maximum temperature
#		- fixed bug in ETA calculation: didn't ignore excluded runs
#		- fixed bug in calculation of ifirst: if first row was not included then no column headers were generated
#		- fixed bug in concatenating output files: if error writing file then all temp files still deleted instead of saved for later attempts
#		- fixed simTime
#	v42	(20120217-20120503)
#		- added aggregation: Artemisia tridentata germination and first-year seedling survival
#		- added option for 4 aggregated soil layers for daily aggregation responses
#		- fixed bug in calculation of daily aggregation of SWA
#		- unified simulation output file access: data is read only once
#		- temporary variables are now removed for each aggregation group
#		- fixed bug in aggregation of 'dailySnowpack': apply failed if nrow(matrix) == 1
#		- added aggregation 'dailyWeatherGeneratorCharacteristics': duration of wet spells (days with precipitation), duration of dry spells (days without precipitation), daily temperature standard deviation
#		- fixed bug in daily aggregation of SWA: soiltexture
#		- fixed bug in column names for aggregation of 'dailyWeatherEventSizeDistribution'
#		- fixed bug in aggregation 'monthlyCorrelations' when there was no precipitation during a given year
#		- fixed bug in creation of soils.in: if use_imperm not set and soilsin contained no data, then set impermeability to 0
#		- fixed bug in aggregation of 'dailyWeatherEventSizeDistribution': if only 1 or 2 years of simulation data
#	v43 (20120503-20120604)
#		- added option for aggregated time series output
#		- added option for 'ArtemisiaTridentataRegeneration' yearly time series output of germination and seedling survival successes
#		- fixed bug in aggregation of 'ArtemisiaTridentataRegeneration': number of years was mis-calculated
#		- fixed bug in determination of layers_depth if adjusted during soilsin creation: added function: layers_depth <- adjustLayersDepth(layers_depth, d)
#		- fixed bug in creation of soils.in: if no data for impermeability in datafile and datafile different soil layer structure than soilsin, then set impermeability to 0
#		- added overall timing measurements and output
#		- fixed bug in determination of 'layers_depth.datafile': if more depths values available in datafile than column 'SoilDepth_cm' indicated, then values for soilsin taken from datafile (which may be empty) instead of input file
#	v44 (20120604-20120612)
#		- (DLM) began updating the framework for use with SOILWAT v_23 (now includes: soil temperature)
#		- (DLM) updated the adding of site information to siteparamin to account for changes in the siteparamin file... (IE: the inclusion of the soil temperature constants).  updated the line numbers for the transp regions part.
#		- (DLM) updated the adding of soil information to soilsin to include the initial soil temperature for each soil layer.  also changed the SW_Runs_InputData_Soils.csv file to include columns for the initial soil temperature for each soil layer.
#		- (DLM) added new aggregation response variable "monthlySoilTemp" to output monthly soil temperature
#		- (DLM) added new daily response variable "SoilTemperature" to output daily soil temperature
#		- added to datafile 'SWRuns_InputData_siteparam_v10.csv': option to turn on/off soil temperature calculations, and option to adjust constant soil temperature at lower boundary (estimated as mean annual air temperature)
#		- added option to extract data externally 'EstimateConstantSoilTemperatureAtUpperAndLowerBoundaryAsMeanAnnualAirTemperature': calculates mean annual air temperature from daily weather input previous to runs; data per run needs to be stored temporarilly and afterwards concatenated and stored into datafile.siteparam
#		- added option to extract data externally 'EstimateInitialSoilTemperatureForEachSoilLayer': estimate initial soil temperature values for each layer as linear regression = f(soil layer depth) between (0 cm, 0C) and (lower bound, lower bound constant temperature); data per run needs to be stored temporarilly and afterwards concatenated and stored into datafile.soils
#		- fixed bug in 'collect_ResultsWithTemporaryDataFrame' if there was only one run to aggregate
#		- fixed bug in creation of soils.in: if datafile.soils used and not data for initial soil temperatures, but these calculated in EstimateInitialSoilTemperatureForEachSoilLayer, then values were not taken from the calculated ones
#	v45 (20120612-20120705)
#		- updated 'ArtemisiaTridentataRegeneration' and made it on average 15% faster
#		- renamed 'ArtemisiaTridentataRegeneration' to 'dailyRegeneration_byTempSWPSnow'
#		- parameter values for 'dailyRegeneration_byTempSWPSnow' are read in from a species.csv file from folder input/regeneration
#		- added option for multiple species in 'dailyRegeneration_byTempSWPSnow': for each file in folder input/regeneration, 'dailyRegeneration_byTempSWPSnow' is called 
#		- added option 'be.quiet': if TRUE, no reports on timing and runs is printed
#	v46 (20120705-20120725)
#		- (DLM) added a few changes for running on JANUS, made a R script to run the framework (rToCallR.R) that sets a global variable "use_janus" to make a few minor changes for specifically running the code on JANUS.  (ie. doesn't load libraries since they're not installed on JANUS, sets the project directory differently, sets the number of cores to use differently, & calls the C code slightly differently).
#		- (DLM) made a few small changes to facilitate running the code on R version 2.13 (ie. the version on JANUS) mainly wrote a function list.dirs2 to handle list.dir calls, since the list.dir function is different in 2.13 and 2.15.  also made a small edit to the dir.copy function (commented out 1 line that was giving an error & added a new one that does the same thing and doesn't throw an error).  Code is running on both version 2.13 & 2.15 now!
#		- framework is no longer compatible with SoilWat versions prior to v20 due to requirements for sw.inputs and sw.output folders
#		- added options to extract data from global datasets: "ExtractGriddedDailyWeatherFromNCEPCFSR_Global", "ExtractClimateChangeScenariosMaurer2009_Global", "ExtractSkyDataFromNCEPCFSR_Global"
#		- added input datafile 'SWRuns_InputData_ClimateScenarios_Values_v10.csv': SoilWat climate scenario factors (delta temperature, precipitation multiplier) are not taken from 'SWRuns_InputData_ClimateScenarios_Change_v10.csv', but are calculated as difference between climate scenario values and current climate calculated from daily input data (goal: match timeframe of model and change)
#		- added option to 'actions': concatenate temporarily stored files after simulation runs or not (e.g., run everything on JANUS and then concatenate locally because it's not parallelized)
#		- fixed bug in aggregation of mean or median aggregated soil layer response variables: previously, aggregated mean or medians were simple averages across soil layers thereby values from wide soil layers were underrepresented; now, aggregated means are weighted by soil layer width (note, SWP aggregated across soil layers is now done by first aggregating VWC and then converting to SWP)
#		- added function 'VWCtoSWP': converting VWC for layers with known sand and clay contents to SWP
#		- aggregation of SWP across soil layers is now done by first aggregating VWC (weighted by soil layer widths) and then converting to SWP because SWP is not linear
#		- improved 'continueAfterAbort': now checks first for each temporary output file and then decides whether it needs to re-execute SoilWat and aggregate or not
#		- improved handling of 'include_YN': the work-horse function is now only called for runs/sites with include_YN > 0 
#	v47 (20120725-20120829)
#		- implemented parallelization with doSNOW/snow as an option to doMC/multicore - JANUS: only use snow, because multicore cannot access cores outside master node
#		- calculation of 'calculate doy_ForEachUsedDay, month_ForEachUsedDay, and year_ForEachUsedDay' is expensive for many years: creation of these vectors once before each simulation is started if possible, i.e., already executed and all simulations have same simulation years
#		- temporary files written to a run-specific sub-directory in dir.out.temp instead of all files to dir.out.temp: linear increase in computation time per run if too many files in dir.out.temp (limit ca. 600,000 files)
#		- (DLM) added function dir.create2: made this function b/c dir.create wasn't always working correctly on JANUS for some reason... so if the simulations are being run on JANUS then it uses the system mkdir call to make the directories.
#		- added option 'deleteTemporaryAggregationFiles' to delete all temporary aggregation files in folder dir.out.temp after successful concatenation (or not if FALSE)
#		- (DLM) concatenation made much faster: concatenation step only creates the file list once instead of each time
#		- concatenation of temporary aggregated output is now performed in parallel
#		- added option to check completeness of SoilWat simulation directories and of temporary output aggregation files 'checkCompleteness'; creates a list with missing directories and files
#		- (DLM) added option to use Rmpi for the multi-threading, it's performing better on JANUS and is slightly easier to debug then using SNOW (as it will print out an error message for every site that fails, unlike SNOW which will only print out the first error).  The only bad thing is that the output is harder to read as all the print statements that are made inside the do_OneSite() function are seperated into log files for each thread by Rmpi.  To keep all the logfiles change the call to mpi.close.Rslaves(dellog = TRUE) to mpi.close.Rslaves(dellog = FALSE).  It's fairly easy to check if it ran right because if any of them do not it will still print the error to the main logfile (or stdout if running locally).
#		- (DLM) changed dir.create2 & file.copy2 functions to recursively call themselves until the file/dir is copied/created.  This is because for some strange reason when using MPI on JANUS, the functions both don't work everytime and fail to throw an error when they don't work.  It's quite aggravating really, but this is the only way I got it actually working.
#		- (DLM) program is still called the same on JANUS when using Rmpi, ex: "R --no-save --slave < 2_SoilWatSimulationFramework_PC_GlobalDrylandEcoHydro_PrelimSim_v47.R", and is running correctly on JANUS again
#		- (DLM) fixed bug in daily aggregation if no daily aggregation was selected
#		- fixed bug in "CalculateBareSoilEvaporationCoefficientsFromSoilTexture"
#		- fixed bug in "CalculateFieldCapacityANDWiltingPointFromSoilTexture": call to SWPtoVWC was not designed for matrices of sand and clay
#	v48 (20120829-20121018)
#		- replaced variable in 'ExtractSkyDataFromNOAAClimateAtlas_USA': replaced 'Sky Cover' with 'Percent Sunshine' as SoilWat's PET function estimates clearsky = 1 - cloudcover (hence, percent sunshine fits SoilWat's interpretation better than sky cover; PET values were too low generally as sky cover values are generally high in the NOAA US Climate Atlas)
#		- added option in overall aggregations: 'yearlyUNAridityIndex' = mean(PPT/PET)
#		- added option 'deleteSoilWatFolderAfterAggregation': to completely delete each SoilWat run folder after completion of all required 'actions'; calculations of 'todo' actions (picking up after stop) accounts for 'deleteSoilWatFolderAfterAggregation'
#		- added option in overall aggregations: 'monthlyPlantGrowthControls', see Nemani RR, Keeling CD, Hashimoto H et al. (2003) Climate-Driven Increases in Global Terrestrial Net Primary Production from 1982 to 1999. Science, 300, 1560-1563.
#		- fixed bug: guaranteed that all climate change entries in 'weathersetup' are finite: this was not the case for instance if any(meanMonthlyClimate$meanMonthlyPPTcm == 0)
#		- added code for storing to disk/reading from disk/creating the list of temporary output files to concatenate (because creating this list can be quite costly), and checking whether enough temporary files are on disk for successful concatenations
#		- fixed bug: in 'dailySWPbasedRegeneration' swp and SWE were not of the same time frame
#		- introduced sw_v24: main change: unit of wind speed at 2-m height (miles/h before v24, m/s starting with v24)
#		- added code to check if input datafile measurement height above ground of windspeed is SoilWat required 2-m, if not convert with function 'adjust.WindspeedHeight'
#		- new vegetation interception coefficients for shrubs
#	v49 (20121102-)
#		- (rjm) adjusted line numbers for prod.in file
#		- (rjm) Fixed sim timing to have Start and End not Start and Start
#		- (rjm) Aggregate for each senario, has a header that includes YearStart and YearEnd
#		- (rjm) created variables 	shrub.fraction.limit 			- potential natural vegetation based on climate data (Jose Paruelo et al. 1996, 1998)
#		- (rjm) 					growing.season.threshold.tempC 	- used to define threshold temp of a growing month
#		- (rjm)						grass.c4.fractionG, grass.c3.fractionG, grass.Annual.fraction - represent percentage of Grass based on various values.
#		- (rjm) added code to find relative composition of vegetation based on climate
#		- (rjm) added code to adjust composition based on mean temp and mean ppt. 
#		- (rjm) added code to calc rooting fraction based on those relative composition
#		- (rjm) Fixed the code so that all 'create' sections based on climate is carried out after the climate change scenario are made
#		- more verbose during preparation stages of code
#		- streamlined code for parallelization
#		- added option in overall aggregations: 'dailyC4_TempVar' = c('mean July minimum temperature', 'mean length of annual freeze-free periods (days)', 'mean annual degree-days above 65F (day C)'
#		- replaced simTime$mo == 1:12 with global variable st_mo: functions to extract external information don't need to import simTime any more
#		- added function 'simTiming_ForEachUsedTimeUnit' to calculate necessary timing variables such as 'doy_ForEachUsedDay' without readin from SoilWat files (which was quite costly) 
#		- accounting for North/South hemispheres:
#			- added global option flag 'accountNSHemispheres_agg': if TRUE then add a set of counting of the timing variables for sites with latitude < 0 (i.e., southern hemisphere), which have time shifted by 6 months (e.g., July becomes 1st month, etc.); i.e., introduced simTime2 as calculated by function 'simTiming_ForEachUsedTimeUnit'
#				- this affects aggregation of: monthlySWPdryness, dailySWPdrynessANDwetness, dailySnowpack, dailyC4_TempVar, dailySWPbasedRegeneration, dailyRegeneration_byTempSWPSnow (param$Doy_SeedDispersalStart0 must be set correctly)
#			- (rjm) added global option flag 'accountNSHemispheres_veg' to shift monthly production values in prod.in file by six months
#		- moved scenarios from master datafile to script because it should be the same for every simulation run
#		- fixed bug in "CalculateBareSoilEvaporationCoefficientsFromSoilTexture": affected layers were incorrectly estimated when limit was a soil layer boundary
#		- fixed bug in "CalculateFieldCapacityANDWiltingPointFromSoilTexture": data was incorrectly assigned to simulation runs
#		- changed aggregated output names for SWPcrit from 1, 2, 3, ... to actual values, e.g., 1500kPa
#		- efforts made to reduce memory load for forked processes:
#			(i) deleted all global variables when not needed any more,
#			(ii) 'large' variables such as SWRunInformation or sw_input_climscen_values only passed row-wise to function do_OneSite()
#				- problems: works on multicore, works on snow (but each process utilizes 2x memory than if run with multicore), does not work on mpi
#		- added global option for daily aggregations: output for every soil layer instead for aggregated soil layers
#		- added option to treatment datafile: 'Vegetation_Biomass_ScalingFactor': litter and total biomass values are multiplied by scaling factor
#		- added option to treatment datafile: 'Vegetation_Height_ScalingFactor': height of vegetation (either constant or as described by a tangens-function(biomass)) is multiplied by scaling factor
#		- added overall output option: replaced 'yearlyUNAridityIndex' with 'yearlymonthlyTemperateDrylandIndices': includes UN Aridity Index, Trewartha's D, and the combination of both
#		- added name of climate scenarios to output filenames
#		- (rjm) introduced sw_v25: main change: topography (slope, aspect as inputs to siteparam.in) affects PET; datafile siteparam adjusted
#		- (rjm)	added code to handle ensemble.families & ensemble.quantiles, new function collect_EnsembleFromScenarios
#		- (rjm) tested relative composition based on climate, adjust composition based on mean temp and mean ppt, rooting profile
#		- (rjm) fixed line numbers to prod file in my code, added infile<-file(infilename,"w+b") in my code, fixed mpi bug, replaced TranspirationCoefficients.csv, removed soillayers_depth<-NULL from my code, removed double tab when writing to prodin file line 6 in my code
#		- (rjm) 'input_FractionVegetationComposition' in output_aggregates: add three columns ("SWinput_Fraction_{Grasses, Shrubs, Trees}") to overall aggregation file: with content of line 6 of file prodin
#		- (rjm) 'input_VegetationPeak' in output_aggregates:add one column ("SWinput_PeakLiveBiomass_Month") to overall aggregation file: with content of the month corresponding to the maximal value, weighted by fractional composition of grasses, shrubs, and trees, of biomass * percLive
#		- (rjm) 'input_ClimatePerturbations' in output_aggregates:' add 36 columns ("SWinput_ClimatePerturbations_{PrcpMultiplier, TminAddand, TmaxAddand}_m1:12") to overall aggregation file: with content of the mean monthly climate perturbations from file weathsetupin lines 17-28
#		- (rjm) add column to treatment datafile 'PotentialNaturalVegetation_Composition_basedOnReferenceOrScenarioClimate" with values "Reference" or "Scenario" : calculate C3, C4, and shrub potential compositions using 'SiteClimate_Ambient', if "Reference", and 'SiteClimate_Scenario', if "Scenario" or not set
#		- (rjm) introduced sw_v26: main change: a set fraction of ponded water can be runoff
#		- (drs) updated 'PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996': fraction of C4 grasses accounts now limits set by Teeri JA, Stowe LG (1976); If no or only one successful equation, then 	add 100% C3 if MAT < 10 C, 100% shrubs if MAP < 600 mm, and 100% C4 if MAT >= 10C & MAP >= 600 mm
#		- (drs) updated 'max.duration': added handling of durations of length 0
#		- (drs) added SD to output: 'dailySWPextremes', 'dailySnowpack'
#		- (drs)	incorporated circular calculations for time output (via library 'circular' accessed with wrapper functions 'circ.mean' and 'circ.sd'): 'dailySWPextremes', 'dailySnowpack', 'monthlySWPdryness', 'dailySWPdrynessANDwetness'
#		- (drs) fixed code after introduction of 'experimental design' so that do_OneSite is called with proper inputs only if include_YN > 0
#		- (drs) in 'create' at the end of each scenario-loop: control that transpiration regions are within limits of adjusted soil depth and rooting depth
#		- (drs) fixed 'makeInputForExperimentalDesign' & !makeOutputDB: parsing of the files was incorrect if separator was already used by text; added a (hopefully) unique 'ExpInput_Seperator'
#		- (drs) generalized experimental design: structure of 'datafile.Experimentals' does no longer need to be a copy of the structure of 'datafile.treatments'; currently, experimental design could now cover any changes to sw_input_treatments, sw_input_site, and sw_input_soils
#		- (drs) fixed concatenation: 'tempFiles_N' wasn't exported to nodes
#		- (drs) fixed aggregation of 'input_VegetationPeak': 2nd column wasn't labelled correctly
#		- (drs) output of functions circ.xxx (e.g., xxx = {mean, range, sd}) give now numeric result, instead of class circular; i.e., this caused some inadverted problems converting results to vectors
#		- (drs) added to aggregation of 'yearlymonthlyTemperateDrylandIndices': indices based on climate normals in addition to mean±SD of time series
#		- (drs) added columns for source information to datafile.cloud, which will be promoted to the cloudin file
#		- (drs) function 'get.LookupSnowDensityFromTable' replaces months with NA or 0 with an estimate of density for freshly fallen snow
#		- (drs) snow density values from datafile.cloud are now tagged with hemisphere, and if different than location adjusted
#		- (drs) fixed bug that failed daily aggregation of 'EvaporationTotal' and 'EvaporationSoil' if soil evaporation was only one layer deep
#		- (drs) fixed bug in calculation of 'count.AllConcats': if makeInputForExperimentalDesign was TRUE but trowExperimentals not used, then count.AllConcats was too large
#		- (drs) fixed bug in ensembles.maker$outputs (wrong dimensions) and created a own section for do.ensembles with timing, pulling together the version for temporary files and the one with the MPI-based sql-database
#		- (drs) fixed bug in 'get.LookupSnowDensityFromTable': wrong dimension of references; added support of references for 'ExtractSkyDataFromNOAAClimateAtlas_USA'
#		- (drs) fixed bug in 'AdjRootProfile': formatting of very small numbers was incorrect while writing to soilsin file
#		- (drs) fixed bug in daily aggregation of response variables with many soil layers: dimension of weight factors was incorrect for weighted means

#--------------------------------------------------------------------------------------------------#
#------------------------PREPARE SOILWAT SIMULATIONS
#------
ow <- options(c("warn", "error"))
options(warn=-1, error=traceback)	#turns all warnings off and on error returns a traceback()

#made this function b/c dir.create wasn't always working correctly on JANUS for some reason... so if the simulations are being run on JANUS then it uses the system mkdir call to make the directories.
dir.create2 <- function(path, showWarnings = TRUE, recursive = FALSE, mode = "0777", times = 0) {
	#print(paste("a path:", path))
	#if(!exists("use_janus")) 
	dir.create(path, showWarnings, recursive, mode)
	if(times < 24)
		if(!file.exists(path)) {
			print("trying to make directory again")
			dir.create2(path, showWarnings, TRUE, mode, (times+1)) #recursively call the function b/c when run on JANUS with MPI it doesn't seem to make the directories everytime... quite aggravating.
		}
	#else if(recursive == TRUE) #this commented out part makes the directory via the system call mkdir
	#	system(paste("mkdir -p", path), ignore.stdout=TRUE, ignore.stderr=FALSE)
	#else
	#	system(paste("mkdir", path), ignore.stdout=TRUE, ignore.stderr=FALSE)
}

#create simulation directory structure
dir.out.experimentalInput <- file.path(dir.out, "Experimentals_Input_Data")
dir.out.temp <- file.path(dir.out, "temp")
dir.create2(dir.out, showWarnings=FALSE, recursive=TRUE)
dir.create2(dir.runs, showWarnings=FALSE, recursive=TRUE)
dir.create2(dir.sw.runs, showWarnings=FALSE, recursive=TRUE)
dir.create2(dir.out.temp, showWarnings=FALSE, recursive=TRUE)
dir.create2(dir.out.experimentalInput, showWarnings=FALSE, recursive=TRUE)

#timing: basis for estimated time of arrival, ETA
timerfile <- "temp_timer.csv"
if(!continueAfterAbort || !identical(actions, "concatenate")) try(file.remove(file.path(dir.out, timerfile)), silent=TRUE)
write.table(NA, file=file.path(dir.out, timerfile), append=TRUE, sep=",", dec=".", col.names=FALSE)
#timing: output for overall timing information
timerfile2 <- "Timing_Simulation.csv"
try(file.remove(file.path(dir.out, timerfile2)), silent=TRUE)
write.table(t(c("", "Time_s")), file=file.path(dir.out, timerfile2), append=TRUE, sep=",", dec=".", col.names=FALSE, row.names=FALSE)

#file: file list of all temporary output files
filename.theFileList <- "List_TemporaryOutput.csv"


#------flagging which external information
exinfo.help <- matrix(data=do.ExternalInformation, ncol=2, nrow=length(do.ExternalInformation)/2, byrow=TRUE)
exinfo <- data.frame(t(as.numeric(exinfo.help[,-1])))
names(exinfo) <- exinfo.help[,1]

#------load libraries
if(identical(parallel_backend, "mpi")) { 
	library(Rmpi)
}
if(identical(parallel_backend, "snow")) {	
	library(doSNOW)		#requires: foreach, iterators, snow
	#library(snow)
}
if(identical(parallel_backend, "multicore")){
	library(doMC)	#requires: foreach, iterators, codetools, and attaches: multicore
}
if(!exists("use_janus") & sum(exinfo) > 0) library(rgdal)	#requires: sp; used for extracting external GIS information

#Node_Libraries <- c("circular")

#------prepare output
aon.help <- matrix(data=output_aggregates, ncol=2, nrow=length(output_aggregates)/2, byrow=TRUE)
aon <- data.frame(t(as.numeric(aon.help[,-1])))
names(aon) <- aon.help[,1]

#------import regeneration data
if(!be.quiet) print(paste("SWSF reads input data: started at", t1 <- Sys.time()))

if(any(actions == "aggregate") & any(simulation_timescales=="daily") & aon$dailyRegeneration_byTempSWPSnow) {
	list.species_regeneration <- list.files(dir.sw.in.reg, pattern=".csv")
	no.species_regeneration <- length(list.species_regeneration)
	if(no.species_regeneration > 0){
		f.temp <- read.csv(file.path(dir.sw.in.reg, list.species_regeneration[1]))
		param.species_regeneration <- matrix(NA, ncol=no.species_regeneration, nrow=nrow(f.temp))
		colnames(param.species_regeneration) <- sub(".csv", "", list.species_regeneration)
		rownames(param.species_regeneration) <- f.temp[, 1]
		param.species_regeneration[, 1] <- f.temp[, 2]
		if(no.species_regeneration > 1) for(f in 2:no.species_regeneration){
				f.temp <- read.csv(file.path(dir.sw.in.reg, list.species_regeneration[f]))
				param.species_regeneration[, f] <- f.temp[, 2]
			}
		rm(f.temp)
	}
} else {
	no.species_regeneration <- 0
}


#------constants
n_variables <- 615 + (135*max(length(SWPcrit_MPa), 1)) + (50*no.species_regeneration) #number of variables in aggregated dataset
output_timescales_maxNo <- 4
SoilLayer_MaxNo <- 20
lmax <- 1:SoilLayer_MaxNo
dirname.sw.runs.weather <- "WeatherData"
dirname.AggDaily <- "Aggregation_Seasons_DailyValues"
dirname.AggTS <- "Aggregation_TimeSeries"
dirname.Overall <- "Aggregation_Overall"
dirname.Scenarios <- "Scenarios"
dirname.Ensembles <- "Ensembles"
SoilWat.windspeedAtHeightAboveGround <- 2	#m

#------ignore action == create if source_input == "folders"
if(source_input == "folders" & any(actions == "create")){
	actions <- actions[-which(actions == "create")]
}

#------import data
if(source_input == "folders"){
	SWRunInformation <- Index_RunInformation <- NULL
	labels <- try(list.files(dir.sw.runs))
	include_YN <- rep(1, times=length(labels))
} else {
	SWRunInformation <- read.csv(file.path(dir.in, datafile.SWRunInformation), as.is=TRUE)
	include_YN <- SWRunInformation$Include_YN
	labels <- SWRunInformation$Label
}

if (source_input == "datafiles&treatments") {
	sw_input_soillayers <- read.csv(file.path(dir.in, datafile.soillayers))
	
	sw_input_treatments_use <- read.csv(temp <- file.path(dir.in, datafile.treatments), nrows=1)
	sw_input_treatments <- read.csv(temp, skip=1, as.is=TRUE)
	colnames(sw_input_treatments) <- colnames(sw_input_treatments_use)
	
	sw_input_experimentals_use <- read.csv(temp <- file.path(dir.in, datafile.Experimentals), nrows=1)
	sw_input_experimentals <- read.csv(temp, skip=1, as.is=TRUE)
	colnames(sw_input_experimentals) <- colnames(sw_input_experimentals_use)
	create_experimentals <- names(sw_input_experimentals_use[-1][which(sw_input_experimentals_use[-1] > 0 & is.finite(as.numeric(sw_input_experimentals_use[-1])))])
	
	#update treatment specifications based on experimental design
	sw_input_treatments_use[-1] <- ifelse(sw_input_treatments_use[-1] == 1 | names(sw_input_treatments_use[-1]) %in% create_experimentals, 1, 0)
	create_treatments <- names(sw_input_treatments_use[-1][which(sw_input_treatments_use[-1] > 0 & is.finite(as.numeric(sw_input_treatments_use[-1])))])
	
} else {
	sw_input_soillayers <- NULL
	sw_input_treatments <- NULL
	create_treatments <- NULL
}

if (source_input == "datafiles&treatments" & any(actions == "create")) {
	sw_input_cloud_use <- read.csv(temp <- file.path(dir.sw.dat, datafile.cloud), nrows=1)
	sw_input_cloud <- read.csv(temp, skip=1)
	colnames(sw_input_cloud) <- colnames(sw_input_cloud_use)
	
	sw_input_prod_use <- read.csv(temp <- file.path(dir.sw.dat, datafile.prod), nrows=1)
	sw_input_prod <- read.csv(temp, skip=1)
	colnames(sw_input_prod) <- colnames(sw_input_prod_use)
	
	sw_input_site_use <- read.csv(temp <- file.path(dir.sw.dat, datafile.siteparam), nrows=1)
	sw_input_site <- read.csv(temp, skip=1)
	colnames(sw_input_site) <- colnames(sw_input_site_use)
	sw_input_site_use[-1] <- ifelse(sw_input_site_use[-1] == 1 | names(sw_input_site_use[-1]) %in% create_experimentals, 1, 0)	#update specifications based on experimental design
	
	sw_input_soils_use <- read.csv(temp <- file.path(dir.sw.dat, datafile.soils), nrows=1)
	sw_input_soils <- read.csv(temp, skip=1)
	colnames(sw_input_soils) <- colnames(sw_input_soils_use)
	sw_input_soils_use[-1] <- ifelse(sw_input_soils_use[-1] == 1 | names(sw_input_soils_use[-1]) %in% create_experimentals, 1, 0)	#update specifications based on experimental design
	
	sw_input_weather_use <- read.csv(temp <- file.path(dir.sw.dat, datafile.weathersetup), nrows=1)
	sw_input_weather <- read.csv(temp, skip=1)
	colnames(sw_input_weather) <- colnames(sw_input_weather_use)
	
	sw_input_climscen_use <- read.csv(temp <- file.path(dir.sw.dat, datafile.climatescenarios), nrows=1)
	sw_input_climscen <- read.csv(temp, skip=1)
	colnames(sw_input_climscen) <- colnames(sw_input_climscen_use)
	
	sw_input_climscen_values_use <- read.csv(temp <- file.path(dir.sw.dat, datafile.climatescenarios_values), nrows=1)
	sw_input_climscen_values <- read.csv(temp, skip=1)
	colnames(sw_input_climscen_values) <- colnames(sw_input_climscen_values_use)
	
	if(any(create_treatments == "LookupClimatePPTScenarios")) tr_input_climPPT <- read.csv( file.path(dir.sw.in.tr, "LookupClimatePPTScenarios", trfile.LookupClimatePPTScenarios))
	if(any(create_treatments == "LookupClimateTempScenarios")) tr_input_climTemp <- read.csv( file.path(dir.sw.in.tr, "LookupClimateTempScenarios", trfile.LookupClimateTempScenarios))
	if(any(create_treatments == "LookupShiftedPPTScenarios")) tr_input_shiftedPPT <- read.csv( file.path(dir.sw.in.tr, "LookupShiftedPPTScenarios", trfile.LookupShiftedPPTScenarios), row.names=1)
	if(any(create_treatments == "LookupEvapCoeffFromTable")) tr_input_EvapCoeff <- read.csv( file.path(dir.sw.in.tr, "LookupEvapCoeffFromTable", trfile.LookupEvapCoeffFromTable), row.names=1)
	if(any(create_treatments == "LookupTranspCoeffFromTable_Grass", create_treatments == "LookupTranspCoeffFromTable_Shrub", create_treatments == "LookupTranspCoeffFromTable_Tree", create_treatments == "AdjRootProfile"))
		tr_input_TranspCoeff <- read.csv( file.path(dir.sw.in.tr, "LookupTranspCoeffFromTable", trfile.LookupTranspCoeffFromTable))
	if(any(create_treatments == "LookupTranspRegionsFromTable")) tr_input_TranspRegions <- read.csv( file.path(dir.sw.in.tr, "LookupTranspRegionsFromTable", trfile.LookupTranspRegionsFromTable), row.names=1)
	if(any(create_treatments == "LookupSnowDensityFromTable")) tr_input_SnowD <- read.csv( file.path(dir.sw.in.tr, "LookupSnowDensityFromTable", trfile.LookupSnowDensityFromTable), row.names=1)
	if(any(create_treatments == "AdjMonthlyBioMass_Temperature")) tr_VegetationComposition <- read.csv(file.path(dir.sw.in.tr, "LookupVegetationComposition", trfile.LookupVegetationComposition), skip=1, row.names=1)
}

if(!be.quiet) print(paste("SWSF reads input data: ended after",  round(difftime(Sys.time(), t1, units="secs"), 2), "s"))


#------determine number of runs
runs <- sum(include_YN>0, na.rm=TRUE)
trow  <- length(include_YN)
temp.counter.width <- 1 + ceiling(log10(trow))	#max index digits for temporary output file
if(!(length(runs) > 0)) stop(paste("at least 1 SoilWat-run needed for simulation, but", runs, "found"))
trowExperimentals <- ifelse(length(create_experimentals) > 0, nrow(sw_input_experimentals), 0)


#------create scenario names
climate.conditions <- c(climate.ambient, climate.conditions[!grepl(climate.ambient, climate.conditions)])
if (source_input == "datafiles&treatments") {
	scenario_No <- length(climate.conditions)
	scenario <- climate.conditions
} else if (source_input == "folders") {
	scenario_No  <- 1
	scenario <- climate.conditions[1]
}

#------create ensembles
do.ensembles <- !is.null(ensemble.families) && length(ensemble.quantiles) > 0 && is.numeric(ensemble.quantiles)
if(do.ensembles){
	scenarios.ineach.ensemble <- sapply(ensemble.families, function(x) grepl(pattern=x, scenario, ignore.case=TRUE), simplify=TRUE)
	ensemble.families <- ensemble.families[temp <- apply(scenarios.ineach.ensemble, MARGIN=2, FUN=any)]
	scenarios.ineach.ensemble <- scenarios.ineach.ensemble[, temp]
	families_N <- length(ensemble.families)
	if(families_N > 1){
		scenariosPERensemble_N <- max(apply(scenarios.ineach.ensemble, MARGIN=2, FUN=sum))
	} else{
		scenariosPERensemble_N <- sum(scenarios.ineach.ensemble)
	}
}

#------outputing data
if(makeInputForExperimentalDesign) ExpInput_Seperator <- "X!X"

#create folder structure
dir.create2(path=dir.out.scenarios <- file.path(dir.out, dirname.Overall, dirname.Scenarios), showWarnings=FALSE, recursive=TRUE)
if(do.ensembles) dir.create2(path=dir.out.ensembles <- file.path(dir.out, dirname.Overall, dirname.Ensembles), showWarnings=FALSE, recursive=TRUE)

#append treatment information to the aggregated output in addition to selected Index_RunInformation
Index_RunInformation_Treatments <- NULL
if(source_input == "datafiles&treatments" & length(create_treatments) > 0) {
	Index_RunInformation_Treatments <- match(create_treatments, names(sw_input_treatments))
}

#make filenames for aggregated output
resultfiles.Aggregates <- matrix(data=NA, ncol=scenario_No, nrow=1)
resultfiles.Aggregates[1, ] <- paste(dir.out.scenarios, .Platform$file.sep, "Scenario", formatC(0:(scenario_No-1), width=2, format="d", flag="0"), "_", climate.conditions, "_", filename.aggregatedResults, sep="")
resultfiles.Aggregates.header <- vector(mode="character", length=n_variables)


#prepare for aggregated time series output
dir.out.AggTS <- file.path(dir.out, dirname.AggTS)
dir.create2(dir.out.AggTS, showWarnings=FALSE, recursive=TRUE)
if(any(ouput_aggregated_ts=="Regeneration")) dir.create2(dir.at <- file.path(dir.out.AggTS, "dailyRegeneration_byTempSWPSnow"))


#create seasonal daily output files
filename.aggregatedResults.dailyMean <- paste(unlist(strsplit(filename.aggregatedResults, ".csv")), "_DailyMean.csv", sep="")
filename.aggregatedResults.dailySD <- paste(unlist(strsplit(filename.aggregatedResults, ".csv")), "_DailySD.csv", sep="")

daily_no <- length(output_aggregate_daily)
if(any(simulation_timescales=="daily")){
	if(any(output_aggregate_daily == "SWA") & length(SWPcrit_MPa) > 0){
		output_aggregate_daily <- output_aggregate_daily[-which(output_aggregate_daily == "SWA")]
		for(icrit in seq(along=SWPcrit_MPa)){
			output_aggregate_daily <- c(output_aggregate_daily, paste("SWAatSWPcrit", abs(round(-1000*SWPcrit_MPa[icrit], 0)), "kPa", sep=""))
		}
	}
	
	if(AggLayer.daily){
		aggLs_no <- 2 + ifelse(is.null(Depth_ThirdAggLayer.daily), 1, ifelse(!is.na(Depth_ThirdAggLayer.daily), 1, 0)) + ifelse(is.null(Depth_FourthAggLayer.daily), 1, ifelse(!is.na(Depth_FourthAggLayer.daily), 1, 0))
		dir.out.daily <- paste(dir.out, .Platform$file.sep, dirname.AggDaily, .Platform$file.sep, dirname.Scenarios, "_", aggLs_no, "AggSoilLayers", sep="")
		if(do.ensembles) dir.out.daily.ens <- paste(dir.out, .Platform$file.sep, dirname.AggDaily, .Platform$file.sep, dirname.Ensembles, "_", aggLs_no, "AggSoilLayers", sep="")
	} else {#at this stage we don't know how many soil layers we will have among the SoilWat runs; so just prepare for the maximum
		if(!any(create_treatments == "soilsin") & !is.null(sw_input_soillayers)){
			aggLs_no <- max(apply(sw_input_soillayers[, -1], MARGIN=1, FUN=function(x) ifelse(is.na(x[1]), NA, findInterval(x[1] - sqrt(.Machine$double.neg.eps), c(0, na.exclude(unlist(x[-1]))))) ), na.rm=TRUE)
		} else {
			aggLs_no <- SoilLayer_MaxNo
		}
		dir.out.daily <- paste(dir.out, .Platform$file.sep, dirname.AggDaily, .Platform$file.sep, dirname.Scenarios, "_AllSoilLayers", sep="")
		if(do.ensembles) dir.out.daily.ens <- paste(dir.out, .Platform$file.sep, dirname.AggDaily, .Platform$file.sep, dirname.Ensembles, "_AllSoilLayers", sep="")
	}
	dir.create2(dir.out.daily, showWarnings=FALSE, recursive=TRUE)
	if(do.ensembles) dir.create2(dir.out.daily.ens, showWarnings=FALSE, recursive=TRUE)
	
	resultfiles.daily.labelsOne <- paste("Var_doy", formatC(1:366, width=3, format="d", flag="0"), sep="")
	if(AggLayer.daily){
		resultfiles.daily.labelsLayers <- paste("L0to", Depth_FirstAggLayer.daily, "cm_doy", formatC(1:366, width=3, format="d", flag="0"), sep="")
		if(is.null(Depth_SecondAggLayer.daily)) {
			resultfiles.daily.labelsLayers <- c(resultfiles.daily.labelsLayers, paste("L", Depth_FirstAggLayer.daily, "toSoilDepth", "_doy", formatC(1:366, width=3, format="d", flag="0"), sep=""))
		} else if(is.numeric(Depth_SecondAggLayer.daily)){
			resultfiles.daily.labelsLayers <- c(resultfiles.daily.labelsLayers, paste("L", Depth_FirstAggLayer.daily, "to", Depth_SecondAggLayer.daily, "cm_doy", formatC(1:366, width=3, format="d", flag="0"), sep=""))
		}
		if(is.null(Depth_ThirdAggLayer.daily)) {
			resultfiles.daily.labelsLayers <- c(resultfiles.daily.labelsLayers, paste("L", Depth_SecondAggLayer.daily, "toSoilDepth", "_doy", formatC(1:366, width=3, format="d", flag="0"), sep=""))
		} else if(is.na(Depth_ThirdAggLayer.daily)){
		} else if(is.numeric(Depth_ThirdAggLayer.daily)){
			resultfiles.daily.labelsLayers <- c(resultfiles.daily.labelsLayers, paste("L", Depth_SecondAggLayer.daily, "to", Depth_ThirdAggLayer.daily, "cm_doy", formatC(1:366, width=3, format="d", flag="0"), sep=""))
		}
		if(is.null(Depth_FourthAggLayer.daily)) {
			resultfiles.daily.labelsLayers <- c(resultfiles.daily.labelsLayers, paste("L", Depth_ThirdAggLayer.daily, "toSoilDepth", "_doy", formatC(1:366, width=3, format="d", flag="0"), sep=""))
		} else if(is.na(Depth_FourthAggLayer.daily)){
		} else if(is.numeric(Depth_FourthAggLayer.daily)){
			resultfiles.daily.labelsLayers <- c(resultfiles.daily.labelsLayers, paste("L", Depth_ThirdAggLayer.daily, "to", Depth_FourthAggLayer.daily, "cm_doy", formatC(1:366, width=3, format="d", flag="0"), sep=""))
		}
	} else {
		resultfiles.daily.labelsLayers <- paste(rep(paste("L", formatC(lmax, width=2, format="d", flag="0"), "_doy", sep=""), each=366), rep(formatC(1:366, width=3, format="d", flag="0"), times=SoilLayer_MaxNo), sep="")
	}
	
	resultfiles.dailyMean <- resultfiles.dailySD <- matrix(data=NA, ncol=scenario_No, nrow=daily_no)
	if(daily_no > 0){
		for(doi in 1:daily_no){
			resultfiles.dailyMean[doi,] <- paste(dir.out.daily, .Platform$file.sep, "Scenario", formatC(0:(scenario_No-1), width=2, format="d", flag="0"), "_", climate.conditions, "_", output_aggregate_daily[doi], "_", aggLs_no, "AggL", "_", filename.aggregatedResults.dailyMean, sep="")
		}
		resultfiles.dailySD <- gsub(filename.aggregatedResults.dailyMean, filename.aggregatedResults.dailySD, resultfiles.dailyMean)
	}
	resultfiles.daily_no <- 2 * daily_no * scenario_No
}

#prepare for ensembles
if(do.ensembles){
	ensembles.maker <- list(outputs=array(data="", dim=c(1 + 2 * daily_no, families_N, length(ensemble.quantiles))), scenarioFiles=array(data="", dim=c(1 + 2 * daily_no, families_N, scenariosPERensemble_N)))
	dimnames(ensembles.maker$outputs) <- list(NULL, ensemble.families, paste(round(100*ensemble.quantiles), "Percentile", sep=""))
	dimnames(ensembles.maker$scenarioFiles) <- list(NULL, ensemble.families, NULL)
	#overall
	ensembles.maker$outputs[1,,] <- t(sapply(ensemble.families, FUN=function(x) paste(dir.out.ensembles, .Platform$file.sep, "Ensemble_", x, "_", formatC(round(100*ensemble.quantiles), width=3, flag="0"), "Percentile_", filename.aggregatedResults, sep="")))
	if(families_N > 1){
		ensembles.maker$scenarioFiles[1,,] <- t(apply(scenarios.ineach.ensemble, MARGIN=2, function(x) resultfiles.Aggregates[x]))
	} else {
		ensembles.maker$scenarioFiles[1,,] <- resultfiles.Aggregates[scenarios.ineach.ensemble]
	}
	#mean daily output
	if(daily_no > 0){
		for(doi in 1:daily_no){
			ensembles.maker$outputs[irow <- 2 * doi,,] <- t(sapply(ensemble.families, FUN=function(x) paste(dir.out.daily.ens, .Platform$file.sep, "Ensemble_", x, "_", formatC(round(100*ensemble.quantiles), width=3, flag="0"), "Percentile_", output_aggregate_daily[doi], "_", aggLs_no, "AggL", "_", filename.aggregatedResults.dailyMean, sep="")))
			if(families_N > 1){
				ensembles.maker$scenarioFiles[irow,,] <- t(apply(scenarios.ineach.ensemble, MARGIN=2, function(x) resultfiles.dailyMean[doi, x]))
			} else {
				ensembles.maker$scenarioFiles[irow,,] <- resultfiles.dailyMean[doi, scenarios.ineach.ensemble]
			}
		}
		ensembles.maker$outputs[2 * (1:daily_no) + 1,,] <- gsub(filename.aggregatedResults.dailyMean, filename.aggregatedResults.dailySD, ensembles.maker$outputs[2 * (1:daily_no),,])
		ensembles.maker$scenarioFiles[2 * (1:daily_no) + 1,,] <- gsub(filename.aggregatedResults.dailyMean, filename.aggregatedResults.dailySD, ensembles.maker$scenarioFiles[2 * (1:daily_no),,])
	}
}

#------simulation timing
output_timescales_shortest <- ifelse(any(simulation_timescales=="daily"), 1, ifelse(any(simulation_timescales=="weekly"), 2, ifelse(any(simulation_timescales=="monthly"), 3, 4)))
st_mo <- 1:12

simTiming <- function(startyr, simstartyr, endyr){
	#simyrs <- simstartyr:endyr
	#no.simyr <- endyr - simstartyr + 1
	useyrs <- startyr:endyr
	no.useyr <- endyr - startyr + 1
	no.usemo <- no.useyr * 12
	no.usedy <- as.numeric(as.POSIXlt(paste(endyr, "-12-31", sep="")) - as.POSIXlt(paste(startyr, "-01-01", sep=""))) + 1
	discardyr <- startyr - simstartyr
	discardmo <- discardyr * 12
	discarddy <- as.numeric(as.POSIXlt(paste(startyr, "-01-01", sep="")) - as.POSIXlt(paste(simstartyr, "-01-01", sep="")))
	index.useyr <- (discardyr+1):(discardyr+no.useyr)
	index.usemo <- (discardmo+1):(discardmo+no.usemo)
	index.usedy <- (discarddy+1):(discarddy+no.usedy)
	
	return(list(useyrs=useyrs, no.useyr=no.useyr, index.useyr=index.useyr, index.usemo=index.usemo, index.usedy=index.usedy))
}

isLeapYear <- function(y) y %% 4 == 0 & (y %% 100 != 0 | y %% 400 == 0)	#from package: tis

simTiming_ForEachUsedTimeUnit <- function(simTime, latitude=90){	#positive latitudes -> northern hemisphere; negative latitudes -> southern hemisphere
	res <- NULL
	if(any(simulation_timescales=="daily")){
		temp <- as.POSIXlt(seq(from=as.POSIXlt(paste(min(simTime$useyrs), "-01-01", sep="")), to=as.POSIXlt(paste(max(simTime$useyrs), "-12-31", sep="")), by="1 day"))
		
		res$doy_ForEachUsedDay <- res$doy_ForEachUsedDay_NSadj <- temp$yday + 1
		res$month_ForEachUsedDay <- res$month_ForEachUsedDay_NSadj <- temp$mon + 1
		res$year_ForEachUsedDay <- res$year_ForEachUsedDay_NSadj <- temp$year + 1900
		if(latitude < 0 && accountNSHemispheres_agg){
			dshift <- as.POSIXlt(paste(simTime$useyrs, 6, 30, sep="-"))$yday+1	#new month either at end of year or in the middle because the two halfs (6+6 months) of a year are of unequal length (182 (183 if leap year) and 183 days): I chose to have a new month at end of year (i.e., 1 July -> 1 Jan & 30 June -> 31 Dec; but, 1 Jan -> July 3/4): and instead of a day with doy=366, there are two with doy=182
			res$doy_ForEachUsedDay_NSadj <- unlist(lapply(seq(along=simTime$useyrs), FUN=function(x) c((temp <- res$doy_ForEachUsedDay[simTime$useyrs[x] == res$year_ForEachUsedDay])[-(1:dshift[x])], temp[1:dshift[x]])))
			res$month_ForEachUsedDay_NSadj <- strptime(paste(res$year_ForEachUsedDay, res$doy_ForEachUsedDay_NSadj, sep="-"), format="%Y-%j")$mon + 1
			res$year_ForEachUsedDay_NSadj <- c(rep(simTime$useyrs[1]-1, times=dshift[1] + ifelse(dshift[1] == 182, 2, 3)), res$year_ForEachUsedDay[-(((temp <- length(res$year_ForEachUsedDay)) - dshift[1] - ifelse(dshift[1] == 182, 1, 2)):temp)])
		}
	}
	if(any(simulation_timescales=="weekly")){
		
	}
	if(any(simulation_timescales=="monthly")){
		res$yearno_ForEachUsedMonth <- res$yearno_ForEachUsedMonth_NSadj <- rep(1:simTime$no.useyr, each=12)
		res$month_ForEachUsedMonth <- res$month_ForEachUsedMonth_NSadj <- rep(st_mo, times=simTime$no.useyr)
		if(latitude < 0 && accountNSHemispheres_agg){
			res$month_ForEachUsedMonth_NSadj <- (res$month_ForEachUsedMonth + 5) %% 12 + 1
		}
	}
	if(any(simulation_timescales=="yearly")){
		
	}
	
	return(res)
}

simTime <- simTiming(startyr, simstartyr, endyr)
simTime_ForEachUsedTimeUnit_North <- simTiming_ForEachUsedTimeUnit(simTime, latitude=90)
if(accountNSHemispheres_agg){
	simTime_ForEachUsedTimeUnit_South <- simTiming_ForEachUsedTimeUnit(simTime, latitude=-90)
} else {
	simTime_ForEachUsedTimeUnit_South <- simTime_ForEachUsedTimeUnit_North
}


#------auxiliary functions
#custom list.dirs function because the ones in 2.13 and 2.15 are different... this function will behave like the one in 2.15 no matter which version you are using...
#note: should work on any system where the directory seperator is .Platform$file.sep (ie Unix)
list.dirs2 <- function(path, full.names=TRUE, recursive=TRUE) {
	dir.list <- list.dirs(path, full.names)
	
	if(is.null(dir.list)) 
		return (dir.list)
	if(length(dir.list) == 0) 
		return (dir.list)
	if(recursive == TRUE)
		return (dir.list)
	
	nSlash = length(strsplit(dir.list[1], .Platform$file.sep)[[1]]) + 1
	if(nSlash == 1) 
		return(dir.list[-1])
	
	n = length(dir.list)
	for(i in n:1) 
		if(length(strsplit(dir.list[i], .Platform$file.sep)[[1]]) != nSlash)
			dir.list <- dir.list[-i]
	
	return (dir.list)
}
#custom file.copy2 function, b/c it was giving errors on JANUS when run with MPI
file.copy2 <- function(from="", to="", overwrite=TRUE, copy.mode=TRUE, times=0) {
	#if(!exists("use_janus"))
	file.copy(from, to, overwrite, FALSE, copy.mode)
	if(times < 24)
		if(file.exists(from))
			if(!file.exists(to)) {
				print("trying to copy the file again")
				file.copy2(from, to, overwrite, copy.mode, (times+1))	#recursively call the function again because when run with MPI the file copying doesn't seem to work everytime...
			}
	#else { #this commented out part copies the file via the system command cp
	#	if(any(grepl("/", to, fixed=TRUE))) { #this part makes the to directory if it doesn't exist... so pretty much this can copy files to places that don't exist, which generally isn't what you want to do but in this case it might help solve an error I keep getting.
	#		y <- to
	#		while(substr(y, nchar(y), nchar(y)) != '/')
	#			y <- substr(y, 1, nchar(y)-1)
	#		y <- substr(y, 1, nchar(y)-1)
	#		if(y != "")
	#			system(paste("mkdir -p", y), ignore.stdout=FALSE, ignore.stderr=FALSE)
	#	}
	#	command <- "cp" #this just calls the system command cp...
	#	if(overwrite == TRUE) command <- paste(command, "-f")
	#	if(copy.mode == TRUE) command <- paste(command, "-p")
	#	system(paste(command, from, to), ignore.stdout=FALSE, ignore.stderr=FALSE)
	#}
}
#copy directory and content as in system(paste("cp -R", shQuote(from), shQuote(to)))
dir.copy <- function(dir.from, dir.to, overwrite=FALSE){
	dir.create2(dir.to, recursive=TRUE)
	dir.list <- basename(list.dirs2(dir.from, full.names=FALSE, recursive=FALSE))
	file.list <- list.files(dir.from)
	if(length(dir.list) > 0) {
		sapply(dir.list, function(x) {dir.copy(dir.from=file.path(dir.from, x), dir.to=file.path(dir.to, x), overwrite=overwrite)})
		#file.list <- file.list[-match(dir.list, table=file.list)] #this line gives an error when run in R v. 2.13
		file.list <- file.list[file.list != dir.list] #this line does the same as the other line, but does not throw the error	
	}
	if(length(file.list) > 0) {
		sapply(file.list, function(x) {file.copy2(from=file.path(dir.from, x), to=file.path(dir.to, x), overwrite=overwrite, copy.mode=TRUE)})
	}
	invisible(1)
}
#remove directory and content
dir.remove <- function(dir){
	file.list <- try(list.files(dir, all.files=TRUE))
	file.list <- file.list[-which(file.list %in% c(".", ".."))]
	dir.list <- basename(list.dirs2(dir, full.names=FALSE, recursive=FALSE))
	if(length(dir.list) > 0) {
		sapply(dir.list, function(x) {dir.remove(dir=file.path(dir, x))})
		file.list <- file.list[-match(dir.list, table=file.list)]
	}
	if(length(file.list) > 0) {
		sapply(file.list, function(x) {file.remove(file.path(dir, x))})
	}
	return(file.remove(dir))
}

#Circular functions: int=number of units in circle, e.g., for days: int=365; for months: int=12
circ.mean = function(x, int, na.rm=FALSE){
	require(circular)
	
	circ <- 2 * pi / int
	x.circ <- circular(x * circ, type="angles", units="radians", rotation="clock", modulo="2pi")
	x.int <- mean.circular(x.circ, na.rm=na.rm) / circ
	rm(circ, x.circ)
	return(as.numeric(x.int))
}
circ.range = function(x, int, na.rm=FALSE) {
	require(circular)
	
	circ <- 2 * pi / int
	x.circ <- circular(x * circ, type="angles", units="radians", rotation="clock", modulo="2pi")
	x.int <- range(x.circ, na.rm=na.rm) / circ
	rm(circ, x.circ)
	return(as.numeric(x.int))
}
circ.sd = function(x, int, na.rm=FALSE){
	require(circular)
	
	circ <- 2 * pi / int
	x.circ <- circular(x * circ, type="angles", units="radians", rotation="clock", modulo="2pi")
	x.int <- sd.circular(x.circ, na.rm=na.rm) / circ
	rm(circ, x.circ)
	return(as.numeric(x.int))
}


#functions wet and dry periods
max.duration <- function(x) {
	r <- rle(x)
	rmax <- max(r$lengths[which(r$values==1)])
	return(ifelse(is.finite(rmax), rmax, 0)[1])
}
start10days <- function(x) {
	r <- rle(x)
	if(length(r$lengths)==1 | sum(r$values==1 & r$lengths>=10)==0 ){
		return (ifelse((length(r$lengths)==1 & (r$values==0 | r$lengths<=90)) | sum(r$values==1 & r$lengths>=10)==0, 365, 1)[1])
	} else {
		s <- cumsum(r$lengths)
		first10dry <- r$lengths[which(r$values==1 & r$lengths>=10 & s>90)][1]
		if( !is.na(first10dry) ){
			ind <- which(r$lengths==first10dry)
		} else {
			ind <- -1
		}
		if(ind[1]==1 | ind[1]==-1) {#start at beginning of year
			return(ifelse(s[1]>90 & ind[1]==1, 1, 365))
		} else {
			return(s[ifelse(length(ind)>1, ind[which.max(r$values[ind])], ind)-1]+1)
		}
	}
}
end10days <- function(x) {
	r <- rle(x)
	if(length(r$lengths)==1 | sum(r$values==1 & r$lengths>=10)==0 ){
		return (ifelse((length(r$lengths)==1 & (r$values==0 | r$lengths<=90)) | sum(r$values==1 & r$lengths>=10)==0, 365, 1)[1])
	} else {
		s <- cumsum(r$lengths)
		last10dry <- (rl <- r$lengths[which(r$values==1 & r$lengths>=10 & s>90)])[length(rl)]
		if( length(last10dry) > 0 ){
			ind <- which(r$lengths==last10dry)
		} else {
			ind <- -1
		}
		if(ind[1]==1 | ind[1]==-1) {#start at beginning of year
			return(ifelse(s[1]>90 & ind[1]==1, 1, 365))
		} else {
			return(s[ifelse(length(ind)>1, ind[which.max(r$values[ind])], ind)])
		}
	}
}
start10days2 <- function(x) {
	r <- rle(x)
	if(length(r$lengths)==1 | sum(r$values==1 & r$lengths>=10)==0 ){
		return (ifelse((length(r$lengths)==1 & (r$values==0 | r$lengths<=90)) | sum(r$values==1 & r$lengths>=10)==0, 365, 1)[1])
	} else {
		s <- cumsum(r$lengths)
		firstMax10dry <- max(r$lengths[which(r$values==1 & r$lengths>=10)])
		if( !is.na(firstMax10dry) ){
			ind <- which(r$lengths==firstMax10dry)
		} else {
			ind <- -1
		}
		if(ind[1]==1 | ind[1]==-1) {#start at beginning of year
			return(ifelse(ind[1]==1,1,365))
		} else {
			return(s[ifelse(length(ind)>1, ind[which.max(r$values[ind])], ind)-1]+1)
		}
	}
}
end10days2 <- function(x) {
	r <- rle(x)
	if(length(r$lengths)==1 | sum(r$values==1 & r$lengths>=10)==0 ){
		return (ifelse((length(r$lengths)==1 & (r$values==0 | r$lengths<=90)) | sum(r$values==1 & r$lengths>=10)==0, 365, 1)[1])
	} else {
		s <- cumsum(r$lengths)
		last10dry <- max(r$lengths[which(r$values==1 & r$lengths>=10)])
		if( length(last10dry) > 0 ){
			ind <- which(r$lengths==last10dry)
		} else {
			ind <- -1
		}
		if(ind[1]==-1) {#start at beginning of year
			return(365)
		} else {
			return(s[ifelse(length(ind)>1, ind[which.max(r$values[ind])], ind)])
		}
	}
}
#convert SWP to VWC, e.g., to calculate field capacity and wilting point
SWPtoVWC <- function(swp, sand, clay) {
#Cosby, B. J., G. M. Hornberger, R. B. Clapp, and T. R. Ginn. 1984. A statistical exploration of the relationships of soil moisture characteristics to the physical properties of soils. Water Resources Research 20:682-690.
	
	#1. SWP in MPa [single value] + sand and clay in fraction [single values] --> VWC in fraction [single value]
	#2. SWP in MPa [single value] + sand and clay in fraction [vectors of length d] --> VWC in fraction [vector of length d]
	#3. SWP in MPa [vector of length l] + sand and clay in fraction [single values] --> VWC in fraction [vector of length l]
	#4. SWP in MPa [vector of length l] + sand and clay in fraction [vectors of length d] --> VWC in fraction [matrix with nrow=l and ncol=d, SWP vector repeated for each column]: probably not used
	#5. SWP in MPa [matrix with nrow=l and ncol=d] + sand and clay in fraction [single values] --> VWC in fraction [matrix with nrow=l and ncol=d]
	#6. SWP in MPa [matrix with nrow=l and ncol=d] + sand and clay in fraction [vectors of length d] --> VWC in fraction [matrix with nrow=l and ncol=d, sand/clay vector repeated for each row]
	
	stopifnot(length(sand) == length(clay))
	na.act <- na.action(na.exclude(apply(data.frame(sand, clay), MARGIN=1, FUN=sum)))
	
	if(length(sand) > length(na.act)){
		na.index <- as.vector(na.act)
		
		if(length(na.index) > 0){
			sand <- sand[-na.index]
			clay <- clay[-na.index]
		}
		
		thetas <- -14.2 * sand - 3.7 * clay + 50.5
		psis <- 10 ^ (-1.58 * sand - 0.63 * clay + 2.17)
		b <- -0.3 * sand + 15.7 * clay + 3.10
		if(any(b <= 0)) stop("b <= 0")
		
		bar_conversion <- 1024
		MPa_toBar <- -10
		
		get_vector <- function(swp, sand, clay, thetas=thetas, psis=psis, b=b, do.na=TRUE){#either swp or sand/clay needs be a single value
			vwc <- ifelse(!is.na(swp) & swp <= 0 & sand <= 1 & sand >= 0 & clay <= 1 & clay >= 0, thetas * (psis / (swp * MPa_toBar * bar_conversion))^(1/b) / 100, NA)
			if(do.na & length(na.index) > 0){
				vwc <- napredict(na.act, vwc)
			}
			return(vwc)
		}
		
		if(is.null(dim(swp))){
			if(length(swp) == 1 & length(sand) >= 1 | length(swp) >= 1 & length(sand) == 1){ #cases 1-3		
				vwc <- get_vector(swp, sand, clay, thetas=thetas, psis=psis, b=b)
			} else if(length(swp) > 1 & length(sand) > 1){ #case 4
				vwc <- t(sapply(1:length(swp), FUN=function(d) get_vector(swp[d], sand, clay, thetas=thetas, psis=psis, b=b)))
			}
		} else {
			if(length(sand) == 1){ #case 5
				vwc <- sapply(1:ncol(swp), FUN=function(d) get_vector(swp[, d], sand, clay, thetas=thetas, psis=psis, b=b))
			} else { #case 6
				sand <- napredict(na.act, sand)
				clay <- napredict(na.act, clay)
				stopifnot(ncol(swp) == length(sand))
				psis <- napredict(na.act, psis)
				thetas <- napredict(na.act, thetas)
				b <- napredict(na.act, b)
				vwc <- sapply(1:ncol(swp), FUN=function(d) get_vector(swp[, d], sand[d], clay[d], thetas=thetas[d], psis=psis[d], b=b[d], do.na=FALSE))
			}
		}
	} else {
		vwc <- swp
		vwc[!is.na(vwc)] <- NA
	}
	return(vwc) #fraction m3/m3 [0, 1]
}
#convert VWC to SWP
VWCtoSWP <- function(vwc, sand, clay) {
#Cosby, B. J., G. M. Hornberger, R. B. Clapp, and T. R. Ginn. 1984. A statistical exploration of the relationships of soil moisture characteristics to the physical properties of soils. Water Resources Research 20:682-690.
	
	#1. VWC in fraction [single value] + sand and clay in fraction [single values] --> SWP in MPa [single value]
	#2. VWC in fraction [single value] + sand and clay in fraction [vectors of length d] --> SWP in MPa [vector of length d]
	#3. VWC in fraction [vector of length l] + sand and clay in fraction [single values] --> SWP in MPa [vector of length l]
	#4. VWC in fraction [vector of length l] + sand and clay in fraction [vectors of length d] --> SWP in MPa [matrix with nrow=l and ncol=d, VWC vector repeated for each column]: probably not used
	#5. VWC in fraction [matrix with nrow=l and ncol=d] + sand and clay in fraction [single values] --> SWP in MPa [matrix with nrow=l and ncol=d]
	#6. VWC in fraction [matrix with nrow=l and ncol=d] + sand and clay in fraction [vectors of length d] --> SWP in MPa [matrix with nrow=l and ncol=d, sand/clay vector repeated for each row]
	
	stopifnot(length(sand) == length(clay))
	na.act <- na.action(na.exclude(apply(data.frame(sand, clay), MARGIN=1, FUN=sum)))
	
	if(length(sand) > length(na.act)){
		na.index <- as.vector(na.act)
		
		if(length(na.index) > 0){
			sand <- sand[-na.index]
			clay <- clay[-na.index]
		}
		
		thetas <- -14.2 * sand - 3.7 * clay + 50.5
		psis <- 10 ^ (-1.58 * sand - 0.63 * clay + 2.17)
		b <- -0.3 * sand + 15.7 * clay + 3.10
		if(any(b <= 0)) stop("b <= 0")
		
		bar_conversion <- 1024
		bar_toMPa <- -1/10
		
		get_vector <- function(vwc, sand, clay, thetas=thetas, psis=psis, b=b, do.na=TRUE){#either vwc or sand/clay needs be a single value
			swp <- ifelse(!is.na(vwc) & vwc <= 1 & vwc >= 0 & sand <= 1 & sand >= 0 & clay <= 1 & clay >= 0, psis / ((vwc*100/thetas) ^ b * bar_conversion) * bar_toMPa, NA)
			if(do.na & length(na.index) > 0){
				swp <- napredict(na.act, swp)
			}
			return(swp)
		}
		
		if(is.null(dim(vwc))){
			if(length(vwc) == 1 & length(sand) >= 1 | length(vwc) >= 1 & length(sand) == 1){ #cases 1-3		
				swp <- get_vector(vwc, sand, clay, thetas=thetas, psis=psis, b=b)
			} else if(length(vwc) > 1 & length(sand) > 1){ #case 4
				swp <- t(sapply(1:length(vwc), FUN=function(d) get_vector(vwc[d], sand, clay, thetas=thetas, psis=psis, b=b)))
			}
		} else {
			if(length(sand) == 1){ #case 5
				swp <- sapply(1:ncol(vwc), FUN=function(d) get_vector(vwc[, d], sand, clay, thetas=thetas, psis=psis, b=b))
			} else { #case 6
				sand <- napredict(na.act, sand)
				clay <- napredict(na.act, clay)
				stopifnot(ncol(vwc) == length(sand))
				psis <- napredict(na.act, psis)
				thetas <- napredict(na.act, thetas)
				b <- napredict(na.act, b)
				swp <- sapply(1:ncol(vwc), FUN=function(d) get_vector(vwc[, d], sand[d], clay[d], thetas=thetas[d], psis=psis[d], b=b[d], do.na=FALSE))
			}
		}
	} else {
		swp <- vwc
		swp[!is.na(swp)] <- NA
	}
	return(swp) #MPa [-Inf, 0]
}


#two, three, or four layer aggregation for average daily aggregation output
setAggSoilLayerForAggDailyResponses <- function(layers_depth){
	d <- length(layers_depth)
	vals <- NULL
	#first layer
	DeepestFirstDailyAggLayer <- findInterval(Depth_FirstAggLayer.daily, c(0, layers_depth) + sqrt(.Machine$double.eps), all.inside=TRUE)
	vals[[1]] <- 1:DeepestFirstDailyAggLayer
	#second layer
	if(!is.null(Depth_SecondAggLayer.daily)){
		DeepestSecondDailyAggLayer <- findInterval(Depth_SecondAggLayer.daily, c(0, layers_depth) + sqrt(.Machine$double.eps), all.inside=TRUE)
	} else {
		DeepestSecondDailyAggLayer <- d
	}
	if(is.numeric(DeepestSecondDailyAggLayer) && is.numeric(DeepestFirstDailyAggLayer) && d > DeepestFirstDailyAggLayer){
		vals[[2]] <- (DeepestFirstDailyAggLayer+1):DeepestSecondDailyAggLayer
	}
	#third layer
	if(!is.null(Depth_ThirdAggLayer.daily)){
		if(!is.na(Depth_ThirdAggLayer.daily)){
			DeepestThirdDailyAggLayer <- findInterval(Depth_ThirdAggLayer.daily, c(0, layers_depth) + sqrt(.Machine$double.eps), all.inside=TRUE)
		} else {
			DeepestThirdDailyAggLayer <- NULL
		}
	} else {
		DeepestThirdDailyAggLayer <- d
	}
	if(is.numeric(DeepestThirdDailyAggLayer) && is.numeric(DeepestSecondDailyAggLayer) && d > DeepestSecondDailyAggLayer){
		vals[[3]] <- (DeepestSecondDailyAggLayer+1):DeepestThirdDailyAggLayer
	}
	#fourth layer
	if(!is.null(Depth_FourthAggLayer.daily)){
		if(!is.na(Depth_FourthAggLayer.daily)){
			DeepestFourthDailyAggLayer <- findInterval(Depth_FourthAggLayer.daily, c(0, layers_depth) + sqrt(.Machine$double.eps), all.inside=TRUE)
		} else {
			DeepestFourthDailyAggLayer <- NULL
		}
	} else {
		DeepestFourthDailyAggLayer <- d
	}
	if(is.numeric(DeepestFourthDailyAggLayer) && is.numeric(DeepestThirdDailyAggLayer) && d > DeepestThirdDailyAggLayer){
		vals[[4]] <- ((DeepestThirdDailyAggLayer+1):DeepestFourthDailyAggLayer)
	}
	
	return(vals)
}


#function extracting climate information for one SoilWat-run from SoilWat weather files
do.GetClimateMeans <- 	(sum(sw_input_climscen_values_use[-1]) > 0) |
		exinfo$EstimateConstantSoilTemperatureAtUpperAndLowerBoundaryAsMeanAnnualAirTemperature |
		sw_input_site_use$SoilTempC_atLowerBoundary |
		sw_input_site_use$SoilTempC_atUpperBoundary |
		exinfo$EstimateInitialSoilTemperatureForEachSoilLayer |
		any(create_treatments == "PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996") |
		any(create_treatments == "AdjMonthlyBioMass_Temperature") |
		any(create_treatments == "AdjMonthlyBioMass_Precipitation") |
		any(create_treatments == "Vegetation_Biomass_ScalingSeason_AllGrowingORNongrowing")


dailyC4_TempVar <- function(dailyTempMin, dailyTempMean, simTime2){
	#Variables to estimate percent C4 species in North America: Teeri JA, Stowe LG (1976) Climatic patterns and the distribution of C4 grasses in North America. Oecologia, 23, 1-12.
	
	Month7th_MinTemp_C <- aggregate(dailyTempMin[simTime2$month_ForEachUsedDay_NSadj == 7], by=list(simTime2$year_ForEachUsedDay_NSadj[simTime2$month_ForEachUsedDay_NSadj == 7]), FUN=min)[, 2]
	LengthFreezeFreeGrowingPeriod_Days <- aggregate(dailyTempMin, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=function(x) max(rle(x > 0)$lengths, na.rm=TRUE))[, 2]
	DegreeDaysAbove65F_DaysC <- aggregate(dailyTempMean, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=function(x) sum(ifelse((temp <- x - ((65-32) * 5/9)) > 0, temp, 0)))[, 2]
	
	res <- c(apply(temp <- cbind(Month7th_MinTemp_C, LengthFreezeFreeGrowingPeriod_Days, DegreeDaysAbove65F_DaysC), MARGIN=2, FUN=mean), apply(temp, MARGIN=2, FUN=sd))
	names(res) <- c(temp <- c("Month7th_NSadj_MinTemp_C", "LengthFreezeFreeGrowingPeriod_NSadj_Days", "DegreeDaysAbove65F_NSadj_DaysC"), paste(temp, ".sd", sep=""))
	
	return(res)
}

if(do.GetClimateMeans){
	SiteClimate <- function(dir.weather, sw.weather.praefix, year.start, year.end, do.C4vars=FALSE, simTime2=NULL){
		files.weath <- list.files(dir.weather, pattern=sw.weather.praefix)
		sw.weather.suffices <- as.numeric(sapply(files.weath, FUN=function(x) strsplit(x, "\\.")[[1]][2]))
		files.weath <- files.weath[itemp <- year.start <= sw.weather.suffices & year.end >= sw.weather.suffices]
		years <- sw.weather.suffices[itemp]
		
		temp <- ppt <- rep(0, times=12)
		if(do.C4vars){
			dailyTempMin <- NULL
			dailyTempMean <- NULL
		}
		if((no.yrs <- length(files.weath)) > 0) for(y in 1:no.yrs){
				ftemp <- read.table(file.path(dir.weather, files.weath[y]))
				temp.dailyTempMean <- apply(ftemp[, 2:3], 1, mean)
				if(do.C4vars){
					dailyTempMin <- c(dailyTempMin, ftemp[, 3])
					dailyTempMean <- c(dailyTempMean, temp.dailyTempMean)
				}
				month_forEachDoy <- as.POSIXlt(seq(from=as.POSIXlt(paste(years[y], "-01-01", sep="")), to=as.POSIXlt(paste(years[y], "-12-31", sep="")), by="1 day"))$mon + 1
				temp <- temp + aggregate(temp.dailyTempMean, by=list(month_forEachDoy), FUN=mean)[, 2]
				ppt <- ppt + aggregate(ftemp[, 4], by=list(month_forEachDoy), FUN=sum)[, 2]
			}
		temp <- temp / no.yrs
		ppt <- ppt / no.yrs
		
		res <- list(meanMonthlyTempC=temp, meanMonthlyPPTcm=ppt, MAP_cm=sum(ppt), MAT_C=mean(temp))
		
		if(do.C4vars){
			res$dailyTempMin <- dailyTempMin
			res$dailyTempMean <- dailyTempMean
			res$dailyC4vars <- dailyC4_TempVar(dailyTempMin, dailyTempMean, simTime2)
		}
		
		return(res)
	}
}

#function to extrapolate windspeeds measured at heights different than SoilWat required 2-m above ground
adjust.WindspeedHeight <- function(uz, height){
	# Allen RG, Walter IA, Elliott R, Howell T, Itenfisu D, Jensen M (2005) In The ASCE standardized reference evapotranspiration equation, pp. 59. ASCE-EWRI Task Committee Report.
	# input: windspeed [m/s] at height x
	# output: windspeed [m/s] at height 2 m
	
	stopifnot(all(uz >= 0) && height >= 2 )
	return( uz * 4.87 / log(67.8 * height - 5.42) )	# eqn. 33 in Allen et al. (2005)
}


#------------------------



#--------------------------------------------------------------------------------------------------#
#------------------------OBTAIN INFORMATION PRIOR TO SIMULATION RUNS TO CREATE THEM

#------obtain information prior to simulation runs
if(any(actions == "create")){
	if(!be.quiet) print(paste("SWSF obtains information prior to simulation runs: started at", t1 <- Sys.time()))
	
	if(any(create_treatments == "LookupEvapCoeffFromTable")){
		#lookup bare soil evaporation coefficients per soil layer per distribution type for each simulation run and copy values to 'datafile.soils'
		get.LookupEvapCoeffFromTable <- function(evco_type, sw_input_soils_use, sw_input_soils){
			#extract data from table by category
			table.EvapCoeff <- matrix(data=unlist(sapply(evco_type, FUN=function(i) {tr_input_EvapCoeff[which(rownames(tr_input_EvapCoeff) == as.character(i)), 1:SoilLayer_MaxNo]})), ncol=SoilLayer_MaxNo, byrow=TRUE)
		
			#add data to sw_input_soils and set the use flags
			i.temp <- grepl(pattern="EvapCoeff", x=names(sw_input_soils_use))
			tr.col.max <- max(rowSums(!is.na(table.EvapCoeff)))
			sw_input_soils[, i.temp][1:tr.col.max] <- ifelse(!is.na(table.EvapCoeff[, 1:tr.col.max]), table.EvapCoeff[, 1:tr.col.max], 0)
			sw_input_soils[, i.temp][(tr.col.max+1):SoilLayer_MaxNo] <- NA
			sw_input_soils_use[i.temp][1:tr.col.max] <- 1
			sw_input_soils_use[i.temp][(tr.col.max+1):SoilLayer_MaxNo] <- 0
		
			return(list(sw_input_soils_use=sw_input_soils_use, sw_input_soils=sw_input_soils))
		} 
		
		if(!all(is.na(sw_input_treatments$LookupEvapCoeffFromTable))){#if not then data is in sw_input_experimentals
			tempdat <- get.LookupEvapCoeffFromTable(evco_type=sw_input_treatments$LookupEvapCoeffFromTable, sw_input_soils_use=sw_input_soils_use, sw_input_soils=sw_input_soils)
			sw_input_soils_use <- tempdat$sw_input_soils_use
			sw_input_soils <- tempdat$sw_input_soils
			
			#write data to datafile.soils
			write.csv(rbind(sw_input_soils_use, sw_input_soils), file=file.path(dir.sw.dat, datafile.soils), row.names=FALSE)
		}
	}
	if(any(create_treatments == "LookupTranspRegionsFromTable")){
		#lookup transpiration region per soil layer per distribution type for each simulation run and copy values to 'datafile.soils'
		get.LookupTranspRegionsFromTable <- function(trtype, sw_input_soils_use, sw_input_soils){
			#extract data from table by type
			table.TranspReg <- matrix(data=unlist(sapply(trtype, FUN=function(i) {tr_input_TranspRegions[which(rownames(tr_input_TranspRegions) == as.character(i)), 1:SoilLayer_MaxNo]})), ncol=SoilLayer_MaxNo, byrow=TRUE)
		
			#add data to sw_input_soils and set the use flags
			i.temp <- grepl(pattern="TranspRegion", x=names(sw_input_soils_use))
			tr.col.max <- max(rowSums(!is.na(table.TranspReg)))
			sw_input_soils[, i.temp][1:tr.col.max] <- table.TranspReg[, 1:tr.col.max]
			sw_input_soils[, i.temp][(tr.col.max+1):SoilLayer_MaxNo] <- NA
			sw_input_soils_use[i.temp][1:tr.col.max] <- 1
			sw_input_soils_use[i.temp][(tr.col.max+1):SoilLayer_MaxNo] <- 0
			
			return(list(sw_input_soils_use=sw_input_soils_use, sw_input_soils=sw_input_soils))
		}
		
		if(!all(is.na(sw_input_treatments$LookupTranspRegionsFromTable))){#if not then data is in sw_input_experimentals
			tempdat <- get.LookupTranspRegionsFromTable(trtype=sw_input_treatments$LookupTranspRegionsFromTable, sw_input_soils_use=sw_input_soils_use, sw_input_soils=sw_input_soils)
			sw_input_soils_use <- tempdat$sw_input_soils_use
			sw_input_soils <- tempdat$sw_input_soils
				
			#write data to datafile.soils
			write.csv(rbind(sw_input_soils_use, sw_input_soils), file=file.path(dir.sw.dat, datafile.soils), row.names=FALSE)
		}
	}
	
	if(any(create_treatments == "LookupSnowDensityFromTable")){
		#lookup monthly snow density values per category for each simulation run and copy values to 'datafile.cloud'
		get.LookupSnowDensityFromTable <- function(sdcategories, sw_input_cloud_use, sw_input_cloud){
			#extract data from table by category
			snowd <- matrix(data=unlist(sapply(sdcategories, FUN=function(i) {tr_input_SnowD[which(rownames(tr_input_SnowD) == as.character(i)), st_mo]})), ncol=12, byrow=TRUE)
			notes <- data.frame(matrix(data=unlist(sapply(sdcategories, FUN=function(i) {tr_input_SnowD[which(rownames(tr_input_SnowD) == as.character(i)), 13:14]})), ncol=2, byrow=TRUE), stringsAsFactors=FALSE)
			
			#add fresh snow density during month of no or zero data
			if(sum(itemp <- (is.na(snowd) | snowd == 0)) > 0) snowd[itemp] <- 76 #76 kg/m3 = median of medians over 6 sites in Colorado and Wyoming: Judson, A. & Doesken, N. (2000) Density of Freshly Fallen Snow in the Central Rocky Mountains. Bulletin of the American Meteorological Society, 81, 1577-1587.
			
			#add data to sw_input_cloud and set the use flags
			sw_input_cloud_use[i.temp <- grepl(pattern="snowd", x=names(sw_input_cloud_use))] <- 1
			sw_input_cloud[, i.temp][st_mo] <- snowd
			sw_input_cloud[, grepl(pattern="(SnowD_Hemisphere)|(SnowD_Source)", x=names(sw_input_cloud))] <- cbind(notes[1], apply(notes[2], MARGIN=2, FUN=function(x) paste("Type", sdcategories, "from", x)))
			
			return(list(sw_input_cloud_use=sw_input_cloud_use, sw_input_cloud=sw_input_cloud))
		}
		
		if(!all(is.na(sw_input_treatments$LookupSnowDensityFromTable))){#if not then data is in sw_input_experimentals
			tempdat <- get.LookupSnowDensityFromTable(sdcategories=sw_input_treatments$LookupSnowDensityFromTable, sw_input_cloud_use=sw_input_cloud_use, sw_input_cloud=sw_input_cloud)
			sw_input_cloud_use <- tempdat$sw_input_cloud_use
			sw_input_cloud <- tempdat$sw_input_cloud
				
			#write data to datafile.cloud
			write.csv(rbind(sw_input_cloud_use, sw_input_cloud), file=file.path(dir.sw.dat, datafile.cloud), row.names=FALSE)
		}
	}
	
	if(any(create_treatments == "LookupTranspCoeffFromTable_Grass", create_treatments == "LookupTranspCoeffFromTable_Shrub", create_treatments == "LookupTranspCoeffFromTable_Tree", create_treatments == "AdjRootProfile"))
	{
		#lookup transpiration coefficients for grasses, shrubs, and trees per soil layer or per soil depth increment of 1 cm per distribution type for each simulation run and copy values to 'datafile.soils'
		#first row of datafile is label for per soil layer 'Layer' or per soil depth increment of 1 cm 'DepthCM'
		#second row of datafile is source of data
		#the other rows contain the data for each distribution type = columns
		TranspCoeffByVegType <- function(soillayer_no, trco_type, layers_depth)
		{
			#extract data from table by category
			#trco_type <- eval(parse(text=paste("LookupTranspCoeffFromTable_", veg, sep="")), envir=sw_input_treatments[index, ])
			if(nchar(trco_type) > 0){
				trco.code <- as.character((temp <- tr_input_TranspCoeff[, which(colnames(tr_input_TranspCoeff) == trco_type)])[1])	#Soil 'Layers' or per cm 'DepthCM'
				trco <- rep(0, times=soillayer_no)
				trco.raw <- na.omit(as.numeric(levels(temp <- temp[-(1:2)]))[temp])
				
				if(trco.code == "DepthCM"){
					trco_sum <- ifelse((temp <- sum(trco.raw, na.rm=TRUE)) == 0 & is.na(temp), 1, temp)
					lup <- 1
					for(l in 1:soillayer_no){
						llow <- as.numeric(layers_depth[l])
						#eval(parse(text=paste("depth_L", l, sep="")), envir=sw_input_soillayers[index, ])
						if(is.na(llow) | lup > length(trco.raw))
						{
							l <- l - 1
							break
						}
						trco[l] <- sum(trco.raw[lup:llow], na.rm=TRUE) / trco_sum
						lup <- llow + 1
					}
					usel <- l
				} else if(trco.code == "Layer"){
					usel <- ifelse(length(trco.raw) < soillayer_no, length(trco.raw), soillayer_no)
					trco[1:usel] <- trco.raw[1:usel] / ifelse((temp <- sum(trco.raw[1:usel], na.rm=TRUE)) == 0 & is.na(temp), 1, temp)
				}
				
				return(trco)
			} else {
				return(NA)
			}
		}
		#cannot write data from sw_input_soils to datafile.soils
	}
	
	if(!be.quiet) print(paste("SWSF obtains information prior to simulation runs: ended after",  round(difftime(Sys.time(), t1, units="secs"), 2), "s"))
}


#--------------------------------------------------------------------------------------------------#
#------------------------OBTAIN EXTERNAL INFORMATION PRIOR TO SIMULATION RUNS TO CREATE THEM


#------obtain external information prior to simulation runs
if(exists("use_janus") & (	exinfo$ExtractClimateChangeScenarios_NorthAmerica |
			exinfo$ExtractSoilDataFromCONUSSOILFromSTATSGO_NorthAmerica |
			exinfo$ExtractSkyDataFromNOAAClimateAtlas_USA |
			exinfo$ExtractTopographyANDElevation_USA |
			exinfo$ExtractClimateChangeScenariosMaurer2009_Global)){
	print("GIS extractions not supported on JANUS") #reason: RGDAL and sp libraries not installed
}

if(any(actions == "create")){
	if(!be.quiet) print(paste("SWSF obtains external information prior to simulation runs: started at", t1 <- Sys.time()))
	
	if(exinfo$ExtractClimateChangeScenariosMaurer2009_Global & !exists("use_janus")){
		#Maurer EP, Adam JC, Wood AW (2009) Climate model based consensus on the hydrologic impacts of climate change to the Rio Lempa basin of Central America. Hydrology and Earth System Sciences, 13, 183-194.
		#accessed via climatewizard.org on July 10, 2012
		if(!be.quiet) print(paste("Started 'ExtractClimateChangeScenariosMaurer2009_Global' at", Sys.time()))
		
		list.scenarios.datafile <- climate.conditions[!grepl(climate.ambient, climate.conditions)]
		if(length(list.scenarios.datafile) > 0){ #extracts only information requested in the 'datafile.SWRunInformation'
			dir.ex.dat <- file.path(dir.external, "ExtractClimateChangeScenarios", "ClimateWizard_CMIP3")
			
			list.scenarios.external <- basename(list.dirs2(path=dir.ex.dat, full.names=FALSE, recursive=FALSE))
			
			if(all(list.scenarios.datafile %in% list.scenarios.external)){
				#locations of simulation runs
				locations <- SpatialPoints(coords=with(SWRunInformation, data.frame(X_WGS84, Y_WGS84)), proj4string=CRS("+proj=longlat +datum=WGS84"))
				
				for(sc in 1:length(list.scenarios.datafile)){
					dir.ex.dat.sc <- file.path(dir.ex.dat, list.scenarios.datafile[sc])
					temp <- basename(list.dirs2(path=dir.ex.dat.sc, full.names=FALSE, recursive=FALSE))
					dir.ex.dat.sc.ppt <- file.path(dir.ex.dat.sc, temp[grepl(pattern="Precipitation_Value", x=temp)])
					dir.ex.dat.sc.temp <- file.path(dir.ex.dat.sc, temp[grepl(pattern="Tmean_Value", x=temp)])
					
					list.temp.asc <- list.files(dir.ex.dat.sc.temp, pattern=".asc")
					list.ppt.asc <- list.files(dir.ex.dat.sc.ppt, pattern=".asc")
					
					#extract data
					get.month <- function(path, grid){
						g <- readGDAL(fname=file.path(path, grid), silent=TRUE)
						locations.CoordG <- spTransform(locations, CRS=CRS(proj4string(g)))	#transform points to grid-coords
						val <- unlist(sp::over(x=locations.CoordG, y=g))
					}
					sc.temp <- sapply(st_mo, FUN=function(m) get.month(path=dir.ex.dat.sc.temp, grid=list.temp.asc[grepl(pattern=paste("_", m, "_", sep=""), x=list.temp.asc)]))	#temp value in C
					
					sc.ppt <- sapply(st_mo, FUN=function(m) get.month(path=dir.ex.dat.sc.ppt, grid=list.ppt.asc[grepl(pattern=paste("_", m, "_", sep=""), x=list.temp.asc)]))	#ppt value in mm
					
					#add data to sw_input_climscen and set the use flags
					sw_input_climscen_values_use[i.temp <- match(paste("PPTmm_m", st_mo, "_sc", formatC(sc, width=2,format="d", flag="0"), sep=""), colnames(sw_input_climscen_values_use))] <- 1
					sw_input_climscen_values[, i.temp] <- sc.ppt
					sw_input_climscen_values_use[i.temp <- match(paste("TempC_m", st_mo, "_sc", formatC(sc, width=2,format="d", flag="0"), sep=""), colnames(sw_input_climscen_values_use))] <- 1
					sw_input_climscen_values[, i.temp] <- sc.temp
				}
				
				res <- nrow(sw_input_climscen_values[, i.temp]) - sum(complete.cases(sw_input_climscen_values[, i.temp]))
				if(res > 0) print(paste(res, "sites didn't extract climate scenario information by 'ExtractClimateChangeScenariosMaurer2009_Global'"))
				
				#write data to datafile.climatescenarios_values
				tempdat <- rbind(sw_input_climscen_values_use, sw_input_climscen_values)
				write.csv(tempdat, file=file.path(dir.sw.dat, datafile.climatescenarios_values), row.names=FALSE)
				
				rm(list.scenarios.datafile, list.scenarios.external, tempdat, sc.temp, sc.ppt, res, locations)
			} else {
				print("Not all scenarios requested in 'datafile.SWRunInformation' are available in dir.external/ExtractClimateChangeScenarios")
			}
		}
		if(!be.quiet) print(paste("Finished 'ExtractClimateChangeScenariosMaurer2009_Global' at", Sys.time()))
	}
	
	if(exinfo$ExtractClimateChangeScenarios_NorthAmerica & !exists("use_janus")){
		#Maurer, E. P., L. Brekke, T. Pruitt, and P. B. Duffy. 2007. Fine-resolution climate projections enhance regional climate change impact studies. Eos Transactions AGU 88:504.
		#accessed via climatewizard.org
		if(!be.quiet) print(paste("Started 'ExtractClimateChangeScenarios_NorthAmerica' at", Sys.time()))
		
		list.scenarios.datafile <- climate.conditions[!grepl(climate.ambient, climate.conditions)]
		if(length(list.scenarios.datafile) > 0){ #extracts only information requested in the 'datafile.SWRunInformation'
			dir.ex.dat <- file.path(dir.external, "ExtractClimateChangeScenarios")
			
			list.scenarios.external <- basename(list.dirs2(path=dir.ex.dat, full.names=FALSE, recursive=FALSE))
			
			if(all(list.scenarios.datafile %in% list.scenarios.external)){
				#locations of simulation runs
				locations <- SpatialPoints(coords=with(SWRunInformation, data.frame(X_WGS84, Y_WGS84)), proj4string=CRS("+proj=longlat +datum=WGS84"))
				
				for(sc in 1:length(list.scenarios.datafile)){
					dir.ex.dat.sc <- file.path(dir.ex.dat, list.scenarios.datafile[sc])
					temp <- basename(list.dirs2(path=dir.ex.dat.sc, full.names=FALSE, recursive=FALSE))
					dir.ex.dat.sc.ppt <- file.path(dir.ex.dat.sc, temp[grepl(pattern="Precipitation_Change", x=temp)])
					dir.ex.dat.sc.temp <- file.path(dir.ex.dat.sc, temp[grepl(pattern="Tmean_Change", x=temp)])
					
					list.temp.asc <- list.files(dir.ex.dat.sc.temp, pattern=".asc")
					list.ppt.asc <- list.files(dir.ex.dat.sc.ppt, pattern=".asc")
					
					#extract data
					get.month <- function(path, grid){
						g <- readGDAL(fname=file.path(path, grid), silent=TRUE)
						locations.CoordG <- spTransform(locations, CRS=CRS(proj4string(g)))	#transform points to grid-coords
						val <- unlist(sp::over(x=locations.CoordG, y=g))
					}
					sc.temp <- sapply(st_mo, FUN=function(m) get.month(path=dir.ex.dat.sc.temp, grid=list.temp.asc[grepl(pattern=paste("_", m, "_", sep=""), x=list.temp.asc)]))	#temp change in F
					sc.temp <- sc.temp * 5/9	#temp addand in C
					
					sc.ppt <- sapply(st_mo, FUN=function(m) get.month(path=dir.ex.dat.sc.ppt, grid=list.ppt.asc[grepl(pattern=paste("_", m, "_", sep=""), x=list.temp.asc)]))	#ppt change in %
					sc.ppt <- 1 + sc.ppt/100	#ppt change as factor
					
					#add data to sw_input_climscen and set the use flags
					sw_input_climscen_use[i.temp <- match(paste("PPTfactor_m", st_mo, "_sc", formatC(sc, width=2,format="d", flag="0"), sep=""), colnames(sw_input_climscen_use))] <- 1
					sw_input_climscen[, i.temp] <- sc.ppt
					sw_input_climscen_use[i.temp <- match(paste("deltaTempC_m", st_mo, "_sc", formatC(sc, width=2,format="d", flag="0"), sep=""), colnames(sw_input_climscen_use))] <- 1
					sw_input_climscen[, i.temp] <- sc.temp
				}
				
				#write data to datafile.climatescenarios
				tempdat <- rbind(sw_input_climscen_use, sw_input_climscen)
				write.csv(tempdat, file=file.path(dir.sw.dat, datafile.climatescenarios), row.names=FALSE)
				
				rm(list.scenarios.datafile, list.scenarios.external, tempdat, sc.temp, sc.ppt, res, locations)
			} else {
				print("Not all scenarios requested in 'datafile.SWRunInformation' are available in dir.external/ExtractClimateChangeScenarios")
			}
			
		}
		if(!be.quiet) print(paste("Finished 'ExtractClimateChangeScenarios_NorthAmerica' at", Sys.time()))
	}
	
	if(exinfo$CalculateBareSoilEvaporationCoefficientsFromSoilTexture){
		#calculate bare soil evaporation coefficients per soil layer for each simulation run and copy values to 'datafile.soils'
		bsEvap.depth.max <- 15	# max = 15 cm: Torres EA, Calera A (2010) Bare soil evaporation under high evaporation demand: a proposed modification to the FAO-56 model. Hydrological Sciences Journal-Journal Des Sciences Hydrologiques, 55, 303-315.
		
		ld <- 1:SoilLayer_MaxNo
		use.layers <- which(sw_input_soils_use[match(paste("Sand_L", ld, sep=""), colnames(sw_input_soils_use))] == 1)
		stopifnot(length(use.layers) > 0)
		layers.depth <- as.matrix(sw_input_soillayers[, match(paste("depth_L", use.layers, sep=""), colnames(sw_input_soillayers))])
		if(length(dim(layers.depth)) > 0){
			layers.width <- t(apply(layers.depth, MARGIN=1, FUN=function(x) diff(c(0, x))))
		} else {
			layers.width <- diff(c(0, layers.depth))
		}
		bsEvap.ld <- t(lapply(1:nrow(layers.depth), FUN=function(l) 1:(1+findInterval(bsEvap.depth.max - sqrt(.Machine$double.neg.eps), na.exclude(as.numeric(layers.depth[l, ]))))))
		
		sand <- sw_input_soils[, match(paste("Sand_L", ld, sep=""), colnames(sw_input_soils_use))]
		clay <- sw_input_soils[, match(paste("Clay_L", ld, sep=""), colnames(sw_input_soils_use))]
		sand.mean <- sapply(1:nrow(layers.depth), FUN=function(l) weighted.mean(as.numeric(sand[l, bsEvap.ld[[l]]]), w=layers.width[bsEvap.ld[[l]]], na.rm=TRUE))
		clay.mean <- sapply(1:nrow(layers.depth), FUN=function(l) weighted.mean(as.numeric(clay[l, bsEvap.ld[[l]]]), w=layers.width[bsEvap.ld[[l]]], na.rm=TRUE))
		
		temp <- 4.1984+0.6695*sand.mean^2+168.7603*clay.mean^2	# soil texture influence: Wythers KR, Lauenroth WK, Paruelo JM (1999) Bare-Soil Evaporation Under Semiarid Field Conditions. Soil Science Society of America Journal, 63, 1341-1349.
		bsEvap.depth.min <- ifelse(length(dim(layers.depth)) > 0, min(layers.width[, 1]), min(layers.width[1]))
		stopifnot(bsEvap.depth.min < bsEvap.depth.max)
		temp <- matrix(data=c(temp, rep(bsEvap.depth.min, times=nrow(layers.depth)), rep(bsEvap.depth.max, times=nrow(layers.depth))), ncol=3, byrow=FALSE)
		bsEvap.depth <- apply(temp, MARGIN=1, FUN=function(x) min(c(x[3], max(x[1:2], na.rm=TRUE)), na.rm=TRUE))
		bsEvap.ld <- t(lapply(1:nrow(layers.depth), FUN=function(l) 1:(1+findInterval(bsEvap.depth[l] - sqrt(.Machine$double.neg.eps), na.exclude(as.numeric(layers.depth[l, ]))))))
		
		bsEvap.coeff <-  t(sapply(1:nrow(layers.depth), FUN=function(i) {
							temp <- rep(NA, times=SoilLayer_MaxNo);
							temp[bsEvap.ld[[i]]] <- 1 - exp(1 - layers.depth[i, bsEvap.ld[[i]]] * 5 / bsEvap.depth[i]) / exp(1);	#function made up to match previous cummulative distributions
							return( (temp <- (c(temp <- as.numeric(temp), 1)-c(0, temp))[ld])/sum(temp, na.rm=TRUE) )	#garuantee that sum is 1
						} ))
		
		i.bsE <- grepl(pattern="EvapCoeff", x=names(sw_input_soils_use))
		
		#add data to sw_input_cloud and set the use flags
		sw_input_soils_use[i.bsE] <- 0
		sw_input_soils_use[i.bsE][1:max(unlist(bsEvap.ld))] <- 1
		
		sw_input_soils[, i.bsE] <- 0
		sw_input_soils[, i.bsE] <- bsEvap.coeff
		
		#write data to datafile.soils
		tempdat <- rbind(sw_input_soils_use, sw_input_soils)
		write.csv(tempdat, file=file.path(dir.sw.dat, datafile.soils), row.names=FALSE)
		
		rm(tempdat, i.bsE, bsEvap.coeff, bsEvap.depth, clay.mean, sand.mean, sand, clayuse.layers, layers.depth, layers.width)
		
	}
	
	
	if(exinfo$ExtractSoilDataFromCONUSSOILFromSTATSGO_NorthAmerica & !exists("use_janus")){
		if(!be.quiet) print(paste("Started 'ExtractSoilDataFromCONUSSOILFromSTATSGO_NorthAmerica' at", Sys.time()))
		#Miller, D. A. and R. A. White. 1998. A conterminous United States multilayer soil characteristics dataset for regional climate and hydrology modeling. Earth Interactions 2:1-26.
		#CONUS-SOIL: rasterized and controlled STATSGO data; information for 11 soil layers available
		cl <- 1:11
		ldepth <- c(5, 10, 20, 30, 40, 60, 80, 100, 150, 200, 250)	#in cm
		
		dir.ex.dat <- file.path(dir.external, "ExtractSoilDataFromCONUSSOILFromSTATSGO")
		datafile.bedrock <- "cs_bedrock"
		datafile.bulkd <- "cs_bulkd"
		datafile.sandsilt <- "cs_sandsilt"
		
		#locations of simulation runs
		locations <- SpatialPoints(coords=with(SWRunInformation, data.frame(X_WGS84, Y_WGS84)), proj4string=CRS("+proj=longlat +datum=WGS84"))
		
		#extract data
		g <- readGDAL(fname=temp <- file.path(dir.ex.dat, datafile.bulkd), silent=TRUE)
		locations.CoordG <- spTransform(locations, CRS=CRS(proj4string(g)))	#transform points to grid-coords
		rat <- attr(GDALinfo(temp, silent=TRUE, returnRAT=TRUE), "RATlist")[[1]]
		val <- unlist(sp::over(x=locations.CoordG, y=g))
		bedrock <- unlist(rat$ROCKDEPM[match(val, rat$VALUE)])	#depth in cm >< bedrock from datafile.bedrock, but seems to make more sense?
		cl <- 1:(max(findInterval(bedrock, ldepth), na.rm=TRUE))
		bulkd <- sapply(cl, FUN=function(l) eval(parse(text=paste("L", l, "_BD", sep="")), envir=rat)[match(val, rat$VALUE)])	#bulk density in g/cm3
		
		#Layer specific for bedrock, but it differs from bedrock in bulkd and sandsilt grids, which seem to make more sense
		#g <- readGDAL(fname=paste(dir.ex.dat, .Platform$file.sep, datafile.bedrock, sep=""), silent=TRUE)
		#bedrock <- sp::over(x=locations.CoordG, y=g)	#depth in cm
		
		g <- readGDAL(fname=temp <- file.path(dir.ex.dat, datafile.sandsilt), silent=TRUE)
		rat <- attr(GDALinfo(temp, silent=TRUE, returnRAT=TRUE), "RATlist")[[1]]
		val <- unlist(sp::over(x=locations.CoordG, y=g))
		sand <- sapply(cl, FUN=function(l) eval(parse(text=paste("SAND_L", l, sep="")), envir=rat)[match(val, rat$VALUE)])/100	#sand fraction
		clay <- sapply(cl, FUN=function(l) eval(parse(text=paste("CLAY_L", l, sep="")), envir=rat)[match(val, rat$VALUE)])/100	#sand fraction
		
		#set and save soil layer structure
		sw_input_soillayers$SoilDepth_cm <- bedrock
		sw_input_soillayers[, 2+cl] <- matrix(data=rep(ldepth[cl], times=nrow(sw_input_soillayers)), ncol=length(cl), byrow=TRUE)
		write.csv(sw_input_soillayers, file=file.path(dir.in, datafile.soillayers), row.names=FALSE)
		
		#set and save soil texture
		#add data to sw_input_soils and set the use flags
		i.temp <- grepl(pattern="BD_L", x=names(sw_input_soils_use))
		sw_input_soils[, i.temp][cl] <- bulkd
		sw_input_soils_use[i.temp][cl] <- 1
		i.temp <- grepl(pattern="Sand", x=names(sw_input_soils_use))
		sw_input_soils[, i.temp][cl] <- sand
		sw_input_soils_use[i.temp][cl] <- 1
		i.temp <- grepl(pattern="Clay", x=names(sw_input_soils_use))
		sw_input_soils[, i.temp][cl] <- clay
		sw_input_soils_use[i.temp][cl] <- 1
		
		#write data to datafile.soils
		tempdat <- rbind(sw_input_soils_use, sw_input_soils)
		write.csv(tempdat, file=file.path(dir.sw.dat, datafile.soils), row.names=FALSE)
		
		rm(tempdat, i.temp, cl, bedrock, bulkd, sand, clay, val, rat, g, locations)
		
		if(!be.quiet) print(paste("Finished 'ExtractSoilDataFromCONUSSOILFromSTATSGO_NorthAmerica' at", Sys.time()))
	}
	
	if(exinfo$CalculateFieldCapacityANDWiltingPointFromSoilTexture){
		#lookup soil texture data from 'datafile.soils' for those that the use flag of sand and clay is set, and calculate field capacity and wilting point
		ld <- 1:SoilLayer_MaxNo
		use.layers <- which(sw_input_soils_use[match(paste("Sand_L", ld, sep=""), colnames(sw_input_soils_use))] == 1)
		sand <- sw_input_soils[, match(paste("Sand_L", use.layers, sep=""), colnames(sw_input_soils_use))]
		clay <- sw_input_soils[, match(paste("Clay_L", use.layers, sep=""), colnames(sw_input_soils_use))]
		
		fieldc <- sapply(1:ncol(sand), FUN=function(i) SWPtoVWC(-0.033, sand[, i], clay[, i]))
		wiltp <- sapply(1:ncol(sand), FUN=function(i) SWPtoVWC(-1.5, sand[, i], clay[, i]))
		
		#add data to sw_input_cloud and set the use flags
		sw_input_soils_use[i.fieldc <- match(paste("FieldC_L", use.layers, sep=""), colnames(sw_input_soils_use))] <- 1
		sw_input_soils_use[i.wiltp <- match(paste("WiltP_L", use.layers, sep=""), colnames(sw_input_soils_use))] <- 1
		sw_input_soils[, i.fieldc] <- fieldc
		sw_input_soils[, i.wiltp] <- wiltp
		
		#write data to datafile.soils
		tempdat <- rbind(sw_input_soils_use, sw_input_soils)
		write.csv(tempdat, file=file.path(dir.sw.dat, datafile.soils), row.names=FALSE)
		
		rm(use.layers, sand, clay, fieldc, wiltp, tempdat, i.fieldc, i.wiltp)
	}
	
	if(exinfo$ExtractTopographyANDElevation_USA & !exists("use_janus")){
		#LANDFIRE data
		if(!be.quiet) print(paste("Started 'ExtractTopographyANDElevation_USA' at", Sys.time()))
		
		dir.ex.dat <- file.path(dir.external, "ExtractTopographyANDElevation", "LANDFIRE_30m")
		datafile.elev <- "lf_elevation"
		datafile.aspect <- "lf_aspect"
		datafile.slope <- "lf_slope"
		
		#read raster data
		g.elev <- try(readGDAL(fname=file.path(dir.ex.dat, datafile.elev), silent=TRUE), silent=TRUE)
		g.aspect <- try(readGDAL(fname=file.path(dir.ex.dat, datafile.aspect), silent=TRUE), silent=TRUE)
		g.slope <- try(readGDAL(fname=file.path(dir.ex.dat, datafile.slope), silent=TRUE), silent=TRUE)
		
		if(any(identical(class(g.elev), "try-error"), identical(class(g.aspect), "try-error"), identical(class(g.slope), "try-error"))){
			print("'ExtractTopographyANDElevation_USA': 30m-grid rasters too big to handle correctly by GDAL driver AIG: attempt to read instead 10km-grid")
			
			dir.ex.dat <- file.path(dir.external, "ExtractTopographyANDElevation", "LANDFIRE_10km")
			datafile.elev <- "elev10km"
			datafile.aspect <- "aspect10km"
			datafile.slope <- "slope10km"
			
			g.elev <- try(readGDAL(fname=file.path(dir.ex.dat, datafile.elev), silent=TRUE), silent=TRUE)
			g.aspect <- try(readGDAL(fname=file.path(dir.ex.dat, datafile.aspect), silent=TRUE), silent=TRUE)
			g.slope <- try(readGDAL(fname=file.path(dir.ex.dat, datafile.slope), silent=TRUE), silent=TRUE)
		}
		
		#locations of simulation runs
		locations <- SpatialPoints(coords=with(SWRunInformation, data.frame(X_WGS84, Y_WGS84)), proj4string=CRS("+proj=longlat +datum=WGS84"))
		locations.CoordG <- spTransform(locations, CRS=CRS(proj4string(g.elev)))	#transform points to grid-coords
		
		#extract data for locations
		SWRunInformation$ELEV_m <- unlist(sp::over(x=locations.CoordG, y=g.elev))	# elevation in m a.s.l.
		SWRunInformation$ASPECT <- unlist(sp::over(x=locations.CoordG, y=g.aspect))	# aspect in degrees
		SWRunInformation$SLOPE <- unlist(sp::over(x=locations.CoordG, y=g.slope))	# slope in degrees
		
		#write data to datafile.SWRunInformation
		write.csv(SWRunInformation, file=file.path(dir.in, datafile.SWRunInformation), row.names=FALSE)
		
		rm(g.elev, g.aspect, g.slope, locations, locations.CoordG)
		
		if(!be.quiet) print(paste("Finished 'ExtractTopographyANDElevation_USA' at", Sys.time()))
	}
	
	if(exinfo$ExtractSkyDataFromNOAAClimateAtlas_USA & !exists("use_janus")){
		if(!be.quiet) print(paste("Started 'ExtractSkyDataFromNOAAClimateAtlas_USA' at", Sys.time()))
		
		reference <- "National Climatic Data Center. 2005. Climate maps of the United States. Available online http://cdo.ncdc.noaa.gov/cgi-bin/climaps/climaps.pl. Last accessed May 2010."
		
		#NOAA Climate Atlas: provides no information on height above ground: assuming 2-m which is what is required by SoilWat
		dir.ex.dat <- file.path(dir.external, "ExtractSkyDataFromNOAAClimateAtlasUS")
		dir.ex.dat.RH <- file.path(dir.ex.dat, "HumidityRelative_Percent")
		dir.ex.dat.cover <- file.path(dir.ex.dat, "Sunshine_Percent")
#		dir.ex.dat.cover <- file.path(dir.ex.dat, "SkyCoverDay_Percent")
		dir.ex.dat.wind <- file.path(dir.ex.dat, "WindSpeed_mph")
		
		datafile.RH <- paste("RH23", formatC(st_mo, width=2,format="d", flag="0"), sep="")
		datafile.cover <- paste("SUN52", formatC(st_mo, width=2,format="d", flag="0"), sep="")
#		datafile.cover <- paste("SKYC50", formatC(st_mo, width=2,format="d", flag="0"), sep="")
		datafile.wind <- paste("WND60B", formatC(st_mo, width=2,format="d", flag="0"), sep="")
		
		code.RH <- c(10, 23, 31, 41, 51, 61, 71, 78, 90) #percent
		code.cover <- c(11, 26, 36, 46, 56, 66, 76, 86, 96)	#percent
#		code.cover <- c(11, 23, 31, 41, 51, 61, 71, 81, 93)	#percent
		code.wind <- c(1.3, 2.9, 3.3, 3.8, 4.2, 4.7, 5.1, 5.6, 9.6)	#m/s; the last category is actually open '> 12.9 mph': I closed it arbitrarily with 30 mph
		
		#locations of simulation runs
		locations <- SpatialPoints(coords=with(SWRunInformation, data.frame(X_WGS84, Y_WGS84)), proj4string=CRS("+proj=longlat +datum=WGS84"))
		projStringWGS84 <- proj4string(locations)
		
		#extract data
		get.month <- function(path, shp, month){
			s <- readOGR(dsn=path, layer=shp[month], verbose=FALSE)
			s.wgs84 <- spTransform(s, CRS=CRS(projStringWGS84))	#transform to wgs84
			val <- sp::over(x=locations, y=s.wgs84)$GRIDCODE
		}
		rh <- sapply(st_mo, FUN=function(m) code.RH[get.month(path=dir.ex.dat.RH, shp=datafile.RH, month=m)])
		cover <- sapply(st_mo, FUN=function(m) 100 - code.cover[get.month(path=dir.ex.dat.cover, shp=datafile.cover, month=m)]) #subtract from 100% as we want cover not no-cover
#		cover <- sapply(st_mo, FUN=function(m) code.cover[get.month(path=dir.ex.dat.cover, shp=datafile.cover, month=m)])
		wind <- sapply(st_mo, FUN=function(m) code.wind[get.month(path=dir.ex.dat.wind, shp=datafile.wind, month=m)])
		
		if(sum(c(is.na(rh), is.na(cover), is.na(wind))) > 0) print(paste("Missing data in 'ExtractSkyDataFromNOAAClimateAtlas_USA'"))
		
		#add data to sw_input_cloud and set the use flags
		sw_input_cloud_use[i.temp <- grepl(pattern="RH", x=names(sw_input_cloud_use))] <- 1
		sw_input_cloud[, i.temp][st_mo] <- rh
		sw_input_cloud_use[i.temp <- grepl(pattern="SkyC", x=names(sw_input_cloud_use))] <- 1
		sw_input_cloud[, i.temp][st_mo] <- cover
		sw_input_cloud_use[i.temp <- grepl(pattern="wind", x=names(sw_input_cloud_use))] <- 1
		sw_input_cloud[, i.temp][st_mo] <- wind

		sw_input_cloud[, grepl(pattern="RH_Source", x=names(sw_input_cloud))] <- paste("Variable RH23 from", reference)
		sw_input_cloud[, grepl(pattern="SkyC_Source", x=names(sw_input_cloud))] <- paste("'100% - Variable SUN52' from", reference)
		sw_input_cloud[, grepl(pattern="Wind_Source", x=names(sw_input_cloud))] <- paste("Variable WND60B from", reference)
		

		#write data to datafile.cloud
		tempdat <- rbind(sw_input_cloud_use, sw_input_cloud)
		write.csv(tempdat, file=file.path(dir.sw.dat, datafile.cloud), row.names=FALSE)
		
		rm(tempdat, i.temp, rh, cover, wind, locations, reference)
		
		if(!be.quiet) print(paste("Finished 'ExtractSkyDataFromNOAAClimateAtlas_USA' at", Sys.time()))
	}
	
	#------obtain external information during each simulation run: define functions here	
	if(exinfo$ExtractGriddedDailyWeatherFromMaurer2002_NorthAmerica){
		#extract daily weather information for the grid cell coded by latitude/longitude for each simulation run
		#Maurer, E. P., A. W. Wood, J. C. Adam, D. P. Lettenmaier, and B. Nijssen. 2002. A long-term hydrologically based dataset of land surface fluxes and states for the conterminous United States. Journal of Climate 15:3237-3251.
		
		dir.ex.maurer2002 <- file.path(dir.external, "ExtractGriddedDailyWeatherFromMaurer2002/DAILY_FORCINGS")
		
		#function to be executed for each SoilWat-run
		ExtractGriddedDailyWeatherFromMaurer2002_NorthAmerica <- function(site, cellname, dir.weath, sw.weather.praefix){
			#read data from Maurer et al. 2002
			weath.data <- try(read.table(file=file.path(dir.ex.maurer2002, cellname), comment.char=""), silent=TRUE)
			if(!identical(class(weath.data), "try-error")){
				colnames(weath.data) <- c("year", "month", "day", "prcp_mm", "Tmax_C", "Tmin_C", "Wind_mPERs")
				
				#times
				date <- strptime(with(weath.data, paste(year, month, day, sep="-")), format="%Y-%m-%d")
				doy <- 1 + as.POSIXlt(date)$yday
				
				#weather maker
				Write_Weather <- function(path, site.label, years, doy, Tmax_C, Tmin_C, PPT_cm) {
					n_years <- max(years) - min(years) + 1
					for(y in 1:n_years) {
						data.sw <- data.frame(doy, Tmax_C, Tmin_C, PPT_cm)[years == min(years) + y - 1, ]
						sw.filename <- paste(path, .Platform$file.sep, sw.weather.praefix, ".", min(years) + y - 1, sep="")
						sw.comments <- c(paste("# weather for site", site.label, "year = ", min(years) + y - 1), "# DOY Tmax(C) Tmin(C) PPT(cm)")
						write.table(sw.comments, file=sw.filename, sep="\t", eol="\r\n", quote=FALSE, row.names=FALSE, col.names=FALSE)
						write.table(data.frame(data.sw[,1], formatC(data.sw[, 2], digits=2, format="f"), formatC(data.sw[, 3], digits=2, format="f"), formatC(data.sw[, 4], digits=2, format="f")), file=sw.filename, append=TRUE, sep="\t", eol="\r\n", quote=FALSE, row.names=FALSE, col.names=FALSE)							
					}
				}
				
				Write_Weather(path=dir.weath, site.label=site, years=weath.data$year, doy=doy, Tmax_C=weath.data$Tmax_C, Tmin_C=weath.data$Tmin_C, PPT_cm=weath.data$prcp_mm/10)
				return(1)
			} else {
				return(0)
			}
		}
	}
	
	if(exinfo$EstimateConstantSoilTemperatureAtUpperAndLowerBoundaryAsMeanAnnualAirTemperature){
		sw_input_site_use$SoilTempC_atLowerBoundary <- 1 #set use flag
		sw_input_site_use$SoilTempC_atUpperBoundary <- 1
		#call function 'SiteClimate' in each SoilWat-run
	}
	
	if(exinfo$EstimateInitialSoilTemperatureForEachSoilLayer){
		#set use flags
		ld <- 1:SoilLayer_MaxNo
		use.layers <- which(sw_input_soils_use[match(paste("Sand_L", ld, sep=""), colnames(sw_input_soils_use))] == 1)
		soilTemp <- sw_input_soils[, index.soilTemp <- match(paste("SoilTemp_L", ld, sep=""), colnames(sw_input_soils_use))[use.layers]]
		sw_input_soils_use[index.soilTemp] <- 1
		
		#function to be executed for each SoilWat-run
		EstimateInitialSoilTemperatureForEachSoilLayer <- function(layers_depth, lower.Tdepth, soilTupper, soilTlower){
			sl <- c(0, lower.Tdepth)
			st <- c(soilTupper, soilTlower)
			return( predict(lm(st ~ sl), data.frame(sl=layers_depth)) )
		}
	}
	
	if(!be.quiet) print(paste("SWSF obtains external information prior to simulation runs: ended after",  round(difftime(Sys.time(), t1, units="secs"), 2), "s"))
}

#--------------------------------------------------------------------------------------------------#



#--------------------------------------------------------------------------------------------------#
#------------------------FUNCTION FOR A SOILWAT SIMULATION
do_OneSite <- function(i, i_labels, i_SWRunInformation, i_sw_input_soillayers, i_sw_input_treatments, i_sw_input_cloud, i_sw_input_prod, i_sw_input_site, i_sw_input_soils, i_sw_input_weather, i_sw_input_climscen, i_sw_input_climscen_values) {
#i = i_sim: consecutive number of seq.todo, i.e., counting the simulation runs
#i_xxx = the i_tr-row of xxx for the i-th simulation run; if trowExperimentals > 0 then these will eventually be repeated, and below replaced with experimental values
#i_exp = the row of sw_input_experimentals for the i-th simulation run

	time.sys <- Sys.time()

#-----------------------Check for experimentals
	if(trowExperimentals > 0 && length(create_experimentals) > 0) {
		i_exp <- (i - 1) %/% runs + 1	#first cycle through all sites (seq.tr), then repeat sites and cycle through trowExperimentals
		i_labels <- paste(formatC(i, width=ceiling(log10(runsN.todo + 1)), format = "d", flag="0"), sw_input_experimentals[i_exp,1], i_labels, sep="_")
				
		#--put information from experimental design into appropriate input variables; create_treatments and the _use files were already adjusted for the experimental design when files were read in/created
		#these columns of the experimental design replace information in the treatment design
		ctemp <- match(names(sw_input_experimentals)[sw_input_experimentals_use == 1], names(i_sw_input_treatments), nomatch=0)
		if(sum(ctemp) > 0){
			cexp <- match(names(i_sw_input_treatments)[ctemp], names(sw_input_experimentals), nomatch=0)
			i_sw_input_treatments[ctemp] <- sw_input_experimentals[i_exp, cexp]
		}
		#these columns of the experimental design replace information from the soilsin datafile
		ctemp <- match(names(sw_input_experimentals)[sw_input_experimentals_use == 1], names(i_sw_input_soils), nomatch=0)
		if(sum(ctemp) > 0){
			cexp <- match(names(i_sw_input_soils)[ctemp], names(sw_input_experimentals), nomatch=0)
			i_sw_input_soils[ctemp] <- sw_input_experimentals[i_exp, cexp]
		}
		#these columns of the experimental design replace information from the siteparam datafile
		ctemp <- match(names(sw_input_experimentals)[sw_input_experimentals_use == 1], names(i_sw_input_site), nomatch=0)
		if(sum(ctemp) > 0){
			cexp <- match(names(i_sw_input_site)[ctemp], names(sw_input_experimentals), nomatch=0)
			i_sw_input_site[ctemp] <- sw_input_experimentals[i_exp, cexp]
		}
	}
	
#	if (i_include_YN > 0){
		
		if(!be.quiet) print(paste(i, ":", i_labels, "started at ", time.sys))
		
#------------------------Preparations for simulation run
		#------Get folder and file names
		#create folder-names, temporary file-names, and checks existence of temporary files
		dir.sw.runs.sc <- dir.sw.runs.sc.in <- dir.sw.runs.sc.out <- filename.out.temp <- vector(mode="character", length=scenario_No)
		isdone.overallAggs <- vector(mode="logical", length=scenario_No)
		if(any(simulation_timescales=="daily") && daily_no > 0){
			filename.out.temp.dailyMean <- filename.out.temp.dailySD <- isdone.dailyAggs <- matrix(data=NA, nrow=daily_no, ncol=scenario_No)
		} else {
			isdone.dailyAggs <- TRUE
		}
		
		flag.icounter <- formatC(i, width=temp.counter.width, flag="0")
		dir.out.temp <- file.path(dir.out.temp, i_labels) #local temp folder
		dir.create2(dir.out.temp, showWarnings=FALSE)
		filenames.out.temp <- list.files(dir.out.temp, pattern=flag.icounter)
		
		for (sc in 1:scenario_No){
			dir.sw.runs.sc[sc] <- paste(dir.sw.runs, .Platform$file.sep, i_labels, ifelse(scenario_No > 1, paste("_", scenario[sc], sep=""), ""), sep="")
			dir.sw.runs.sc.in[sc] <- file.path(dir.sw.runs.sc[sc], ifelse(nchar(sw.inputs) > 0, sw.inputs, "Input"))
			dir.sw.runs.sc.out[sc] <- file.path(dir.sw.runs.sc[sc], ifelse(nchar(sw.outputs) > 0, sw.outputs, "Output"))
			
			filename.out.temp[sc] <- paste(dir.out.temp, .Platform$file.sep, flag.icounter, "_", basename(resultfiles.Aggregates[1, sc]), sep="")
			isdone.overallAggs[sc] <- basename(filename.out.temp[sc]) %in% filenames.out.temp
			
			if(any(simulation_timescales=="daily") && daily_no > 0){
				for(doi in 1:daily_no){
					filename.out.temp.dailyMean[doi, sc] <- paste(dir.out.temp, .Platform$file.sep, flag.icounter, "_", basename(resultfiles.dailyMean[doi, sc]), sep="")
					filename.out.temp.dailySD[doi, sc] <- paste(dir.out.temp, .Platform$file.sep, flag.icounter, "_", basename(resultfiles.dailySD[doi, sc]), sep="")
					isdone.dailyAggs[doi, sc] <- all(basename(c(filename.out.temp.dailyMean[doi, sc], filename.out.temp.dailySD[doi, sc])) %in% filenames.out.temp)
				}
			}
		}
		
		#establish what needs to be done for this SoilWat run (accounting for 'deleteSoilWatFolderAfterAggregation' and 'deleteSoilWatOutputAfterAggregation')
		#Final outputs:
		#	- deleteSoilWatFolderAfterAggregation == FALSE:
		#		- if create then SoilWat-folder
		#		- if execute then SoilWat-folder with output except if deleteSoilWatOutputAfterAggregation except for delete.exceptions
		#		- if aggregate then temporary aggregations
		#	- deleteSoilWatFolderAfterAggregation == TRUE:
		#		- if aggregate then temporary aggregations
		
		todo <- list(aggregate=	atemp <- (any(actions=="aggregate") & !continueAfterAbort) |
						(any(actions=="aggregate") & continueAfterAbort & !(all(isdone.overallAggs) & all(isdone.dailyAggs))), #for now: ignoring to check time-series aggregations, i.e., assuming that if overallAggs is done, then time-series output was also completed
				
				create= !deleteSoilWatFolderAfterAggregation & (
							(any(actions=="create") & !continueAfterAbort) |
							(any(actions=="create") & continueAfterAbort & !all(basename(dir.sw.runs.sc) %in% list.files(dir.sw.runs, pattern=as.character(i_labels))) ))
						| deleteSoilWatFolderAfterAggregation & atemp,
				
				execute= !deleteSoilWatFolderAfterAggregation & (
							(any(actions=="execute") & !continueAfterAbort) |
							(any(actions=="execute") & continueAfterAbort & !all(sapply(1:scenario_No, FUN=function(sc) {f_no <- length(list.files(dir.sw.runs.sc.out[sc])); return(f_no > 0 | (f_no == 0 & deleteSoilWatOutputAfterAggregation & is.null(delete.exceptions)) ) } )) ))
						| deleteSoilWatFolderAfterAggregation & atemp
		)
		
		if( any(todo) ){
			#get treatment sw.input.filenames for this run
			filesin <- swFilesIn
			
			oldInputFiles.toDelete <- NULL
			if(source_input == "datafiles&treatments" & !is.null(create_treatments) & todo$create){	
				if(any(create_treatments == "sw")){
					if(!identical(sw, i_sw_input_treatments$sw)) oldInputFiles.toDelete <- c(oldInputFiles.toDelete, file.path(dir.sw.runs.sc[1], sw))
					sw <- i_sw_input_treatments$sw
				}
				if(any(create_treatments == "filesin")){
					if(!identical(filesin, i_sw_input_treatments$filesin)) oldInputFiles.toDelete <- c(oldInputFiles.toDelete, file.path(dir.sw.runs.sc[1], filesin))
					filesin <- i_sw_input_treatments$filesin
				}
				if(any(create_treatments == "prodin")){
					if(!identical(prodin, i_sw_input_treatments$prodin)) oldInputFiles.toDelete <- c(oldInputFiles.toDelete, file.path(dir.sw.runs.sc.in[1], prodin))
					prodin <- i_sw_input_treatments$prodin
				}
				if(any(create_treatments == "siteparamin")){
					if(!identical(siteparamin, i_sw_input_treatments$siteparamin)) oldInputFiles.toDelete <- c(oldInputFiles.toDelete, file.path(dir.sw.runs.sc.in[1], siteparamin))
					siteparamin <- i_sw_input_treatments$siteparamin
				}
				if(any(create_treatments == "soilsin")){
					if(!identical(soilsin, i_sw_input_treatments$soilsin)) oldInputFiles.toDelete <- c(oldInputFiles.toDelete, file.path(dir.sw.runs.sc.in[1], soilsin))
					soilsin <- i_sw_input_treatments$soilsin
				}
				if(any(create_treatments == "weathersetupin")){
					if(!identical(weatherin, i_sw_input_treatments$weathersetupin)) oldInputFiles.toDelete <- c(oldInputFiles.toDelete, file.path(dir.sw.runs.sc.in[1], weatherin))
					weatherin <- i_sw_input_treatments$weathersetupin
				}
				if(any(create_treatments == "cloudin")){
					if(!identical(cloudin, i_sw_input_treatments$cloudin)) oldInputFiles.toDelete <- c(oldInputFiles.toDelete, file.path(dir.sw.runs.sc.in[1], cloudin))
					cloudin <- i_sw_input_treatments$cloudin
				}
			}
			
			#if source_input is "folders" or action is not create then get sw.input.filenames from filesin for this run
			if(source_input == "folders" | !todo$create){
				infilename <- file.path(dir.sw.runs.sc[1], filesin)
				infiletext <- readLines(con = infilename)
				soilsin <- basename(unlist(strsplit(infiletext[10], split="[[:space:]]"))[1])
			}
			
			#------Learn about soil layer structure
			#determine number of soil layers = d and soildepth
			if(source_input == "datafiles&treatments" & !any(create_treatments=="soilsin") & todo$create ) {
				soildepth <- i_sw_input_soillayers$SoilDepth_cm
				layers_depth <- na.omit(as.numeric(i_sw_input_soillayers[2 + lmax]))
				if(!(length(d <- which(soildepth == layers_depth)) > 0)){	#soildepth is one of the lower layer boundaries
					d <- min(length(layers_depth), findInterval(soildepth, layers_depth)+1)	#soildepth is not one of the lower layer boundaries, the next deeper layer boundary is used
				}
			} else {# needs to be read from soilsin file
				if(source_input == "folders" | !todo$create) {
					infilename <- file.path(dir.sw.runs.sc.in[1], soilsin)
				} else { # case treatments & soilsin turned on, file still in input directory
					infilename <- file.path(dir.sw.in.tr, "soilsin", soilsin)
				}
				infiletext <- readLines(con = infilename)
				layers_depth <- NULL
				for (l in soilsin.firstDataLine:(length(infiletext))){
					vec <- (unlist(strsplit(infiletext[l], split="[[:space:]]")))
					layers_depth  <- na.exclude(c(layers_depth, as.numeric(vec[grep("[[:digit:]]", vec)[1]]) ))
				}
				d <- length(layers_depth)
				soildepth <- max(layers_depth)
			}
			
			#functions to obtain soil layer structures
			#layer sequence
			adjustLayersDepth <- function(layers_depth, d) return(layers_depth[1:d])
			getLayersWidth <- function(layers_depth) return(diff(c(0, layers_depth)))
			setLayerSequence <- function(d) return(1:d)
			ld <- setLayerSequence(d)
			layers_depth <- adjustLayersDepth(layers_depth, d)
			layers_width <- getLayersWidth(layers_depth)
			
			#top and bottom layer aggregation
			setDeepestTopLayer <- function(d){
				return(max(1, findInterval(Depth_TopLayers, layers_depth) ))
			}
			setTopLayer <- function(d){
				return(1:(ifelse(d<DeepestTopLayer, d, DeepestTopLayer)))
			}
			setBottomLayer <- function(d){
				if(d <= DeepestTopLayer){
					val  <- 0
				} else {
					val  <- ((DeepestTopLayer+1):d)
				}
				return(val)
			}
			DeepestTopLayer <- setDeepestTopLayer(d)
			topL <- setTopLayer(d)
			bottomL <- setBottomLayer(d)	
			
			
			#------Learn about simulation time
			if(any(create_treatments == "YearStart") | any(create_treatments == "YearEnd")){
				#------time frame of simulation
				if(any(create_treatments == "YearStart")){
					#year when SoilWat starts the simulation
					simstartyr  <- i_sw_input_treatments$YearStart
					#first year that is used for output aggregation, e.g., simstartyr + 1
					startyr <- getStartYear(simstartyr)
				}
				if(any(create_treatments == "YearEnd")){
					#year when SoilWat ends the simulation
					endyr <- i_sw_input_treatments$YearEnd
				}
				
				#------simulation timing needs to be adjusted
				simTime <- simTiming(startyr, simstartyr, endyr)
				simTime2 <- simTiming_ForEachUsedTimeUnit(simTime, latitude=i_SWRunInformation$Y_WGS84)
			} else {
				if(i_SWRunInformation$Y_WGS84 >= 0){
					simTime2 <- simTime_ForEachUsedTimeUnit_North
				} else {
					simTime2 <- simTime_ForEachUsedTimeUnit_South
				}
			}
		}
		
		
#------------------------CREATE RUNS
		if( todo$create ){	
			#------1. Step: Information for this SoilWat-run from prepared SoilWat-run stored in dir.sw.in
			dir.copy(dir.from=dir.sw.in, dir.to=dir.sw.runs.sc[1], overwrite=TRUE)
			
			#get folder and file names
			outsetupin <- swOutSetupIn
			
			#weather folder name and structure
			if(exinfo$ExtractGriddedDailyWeatherFromMaurer2002_NorthAmerica){ #obtain external weather information that needs to be executed for each run
				dirname.sw.runs.weather <- paste("data", format(28.8125+round((i_SWRunInformation$Y_WGS84-28.8125)/0.125,0)*0.125, nsmall=4), format(28.8125+round((i_SWRunInformation$X_WGS84-28.8125)/0.125,0)*0.125, nsmall=4), sep="_")
			}
			if(any(create_treatments == "LookupWeatherFolder")){ #establish names from weather-treatment
				dirname.sw.runs.weather <- i_sw_input_treatments$LookupWeatherFolder
				#obtain 'filebasename.WeatherDataYear' from weather folder
				temp <- try(list.files(file.path(dir.sw.in.tr, "LookupWeatherFolder", dirname.sw.runs.weather), pattern=paste("\\.", as.character(simstartyr), sep="")))
				if(length(temp) > 0){
					filebasename.WeatherDataYear <- strsplit(temp[1], split=".", fixed=TRUE)[[1]][1]
				}
				#obtain name of cloudin from weather folder, except if create_treatments contains cloudin
				if(!any(create_treatments == "cloudin")){
					temp <- c(	try(list.files(temp <- file.path(dir.sw.in.tr, "LookupWeatherFolder", dirname.sw.runs.weather), pattern="cloud")),
							try(list.files(temp, pattern="Cloud")),
							try(list.files(temp, pattern="sky")),
							try(list.files(temp, pattern="Sky")) )
					if(length(temp) > 0){
						cloudin <- temp[1]
					}
				}
			}
			dir.sw.runs.weather <- file.path(dir.sw.runs.sc.in[1], dirname.sw.runs.weather)
			if(!any(create_treatments == "LookupWeatherFolder")){	#create weather folder and move cloud into it
				dir.create2(dir.sw.runs.weather, recursive=TRUE)
				file.copy2(from=file.path(dir.sw.runs.sc.in[1], cloudin), to=file.path(dir.sw.runs.weather, cloudin), overwrite=TRUE, copy.mode = TRUE)
			} else {	#2. Step: b) The entire weather folder is replaced and the paths in filesin will be adjusted
				dir.remove(dir.sw.runs.weather)
				dir.copy(dir.from=file.path(dir.sw.in.tr, "LookupWeatherFolder", dirname.sw.runs.weather), dir.to=dir.sw.runs.weather, overwrite=TRUE)
				#if there is no cloudin in the weather folder and cloudin is not a treatment, then copy cloudin from dir.sw.in
				if(!any(create_treatments == "cloudin") & length(list.files(dir.sw.runs.weather, pattern=cloudin)) == 0){
					file.copy2(from=file.path(dir.sw.runs.sc.in[1], cloudin), to=file.path(dir.sw.runs.weather, cloudin), copy.mode = TRUE)
				}
			}
			try(file.remove(file.path(dir.sw.runs.sc.in[1], cloudin)), silent=TRUE) #delete cloudin in SoilWat-input folder after existing in the weather folder
			
			#write input file names and paths to first file, unless filesin is a treatment
			if(!any(create_treatments=="filesin")){
				infilename <- file.path(dir.sw.runs.sc[1], filesin)
				infiletext <- readLines(con = infilename)
				infiletext[2] <- paste("# This is the first file read. Simulation information = ", paste(i_SWRunInformation[Index_RunInformation], collapse=", "))
				
				infiletext[5] <- paste(sw.inputs, .Platform$file.sep, yearsin, "\t# years for model operation", sep="")   
				infiletext[6] <- paste(ifelse(sw.outputs == "", "", paste(sw.outputs, .Platform$file.sep, sep="")), "logfile.log", "\t# errors or important info (can also be stdout)", sep="")   
				
				infiletext[9] <- paste(sw.inputs, .Platform$file.sep, siteparamin, "\t# site parameters", sep="")   
				infiletext[10] <- paste(sw.inputs, .Platform$file.sep, soilsin, "\t# soil layer definitions", sep="")
				
				infiletext[13] <- paste(sw.inputs, .Platform$file.sep, weatherin, "\t# weather parameters", sep="")
				infiletext[14] <- paste(sw.inputs, .Platform$file.sep, dirname.sw.runs.weather, .Platform$file.sep, filebasename.WeatherDataYear, "\t\t# data file containing historical weather (can include path)", sep="")
				infiletext[15] <- paste(sw.inputs, .Platform$file.sep, dirname.sw.runs.weather, .Platform$file.sep, "mkv_prob.in\t\t# precip probs; required for markov weather", sep="")
				infiletext[16] <- paste(sw.inputs, .Platform$file.sep, dirname.sw.runs.weather, .Platform$file.sep, "mkv_covar.in\t\t# covariance table required for markov weather", sep="")
				infiletext[17] <- paste(sw.inputs, .Platform$file.sep, dirname.sw.runs.weather, .Platform$file.sep, cloudin, "\t# general atmospheric params", sep="")
				
				infiletext[20] <- paste(sw.inputs, .Platform$file.sep, prodin, "\t# productivity values", sep="")
				infiletext[21] <- paste(sw.inputs, .Platform$file.sep, estabin, "\t# plant establishment start file", sep="")
				
				infiletext[24] <- paste(sw.inputs, .Platform$file.sep, swcsetupin, "\t# params for handling measured swc", sep="")
				
				infiletext[27] <- paste(ifelse(sw.outputs == "", "", paste(sw.outputs, .Platform$file.sep, sep="")), "\t# 'relative' path for output files: / for same directory, or e.g., Output/", sep="")   
				infiletext[28] <- paste(sw.inputs, .Platform$file.sep, outsetupin, "\t# define output quantities", sep="")
				
				infile <- file(infilename, "w+b")
				writeLines(text = infiletext, con = infile, sep = "\n")
				close(infile)
			}
			
			#adjust simulation years
			infilename <- file.path(dir.sw.runs.sc.in[1], yearsin)
			infiletext <- readLines(con = infilename)
			infiletext[4] <- paste(simstartyr, "\t# starting year (but see weather and swc inputs)", sep="")
			infiletext[5] <- paste(endyr, "\t# ending year", sep="")
			infile <- file(infilename, "w+b")
			writeLines(text = infiletext, con = infile, sep = "\n")
			close(infile)
			
			
			
			#------2. Step: a) Information for this SoilWat-run from treatment SoilWat input files stored in dir.sw.in.tr
			if(any(create_treatments=="sw"))
				file.copy2(from=file.path(dir.sw.in.tr, "sw"), to=file.path(dir.sw.runs.sc[1], sw), overwrite=TRUE, copy.mode = TRUE)
			if(any(create_treatments=="filesin"))
				file.copy2(from=file.path(dir.sw.in.tr, "filesin", filesin), to=file.path(dir.sw.runs.sc[1], filesin), overwrite=TRUE, copy.mode = TRUE)
			if(any(create_treatments=="prodin"))
				file.copy2(from=file.path(dir.sw.in.tr, "prodin", prodin), to=file.path(dir.sw.runs.sc.in[1], prodin), overwrite=TRUE, copy.mode = TRUE)
			if(any(create_treatments=="siteparamin"))
				file.copy2(from=file.path(dir.sw.in.tr, "siteparamin", siteparamin), to=file.path(dir.sw.runs.sc.in[1], siteparamin), overwrite=TRUE, copy.mode = TRUE)
			if(any(create_treatments=="soilsin"))
				file.copy2(from=file.path(dir.sw.in.tr, "soilsin", soilsin), to=file.path(dir.sw.runs.sc.in[1], soilsin), overwrite=TRUE, copy.mode = TRUE)
			if(any(create_treatments=="weathersetupin"))
				file.copy2(from=file.path(dir.sw.in.tr, "weatherin", weatherin), to=file.path(dir.sw.runs.sc.in[1], weatherin), overwrite=TRUE, copy.mode = TRUE)
			if(any(create_treatments=="cloudin"))
				file.copy2(from=file.path(dir.sw.in.tr, "cloudin", cloudin), to=file.path(dir.sw.runs.weather, cloudin), overwrite=TRUE, copy.mode = TRUE)
			
			
			
			#------2. Step: b) Information for this SoilWat-run from treatment chunks stored in dir.sw.in.tr			
			#Do the lookup stuff for experimental design that was done for the treatment design before the call to call_OneSite, but couldn't for the experimental design because at that time information was unkown
			if(any(names(sw_input_experimentals)[sw_input_experimentals_use == 1] == "LookupEvapCoeffFromTable")) {
				tempdat <- get.LookupEvapCoeffFromTable(evco_type=i_sw_input_treatments$LookupEvapCoeffFromTable, sw_input_soils_use=sw_input_soils_use, sw_input_soils=i_sw_input_soils)
				sw_input_soils_use <- tempdat$sw_input_soils_use
				i_sw_input_soils <- tempdat$sw_input_soils
			}
			if(any(names(sw_input_experimentals)[sw_input_experimentals_use == 1] == "LookupTranspRegionsFromTable")) {
				tempdat <- get.LookupTranspRegionsFromTable(trtype=i_sw_input_treatments$LookupTranspRegionsFromTable, sw_input_soils_use=sw_input_soils_use, sw_input_soils=i_sw_input_soils)
				sw_input_soils_use <- tempdat$sw_input_soils_use
				i_sw_input_soils <- tempdat$sw_input_soils
			}
			if(any(names(sw_input_experimentals)[sw_input_experimentals_use == 1] == "LookupSnowDensityFromTable")) {
				tempdat <- get.LookupSnowDensityFromTable(sdcategories=i_sw_input_treatments$LookupSnowDensityFromTable, sw_input_cloud_use=sw_input_cloud_use, sw_input_cloud=i_sw_input_cloud)
				sw_input_cloud_use <- tempdat$sw_input_cloud_use
				i_sw_input_cloud <- tempdat$sw_input_cloud
			} 
				
			#Treatment chunks
			if(any(create_treatments == "LookupTranspCoeffFromTable_Grass")){
				trco <- TranspCoeffByVegType(soillayer_no=d, trco_type=eval(parse(text=paste("LookupTranspCoeffFromTable_", "Grass", sep="")), envir=i_sw_input_treatments), layers_depth=layers_depth)
				if(!is.na(trco)){
					#set the use flags
					i.temp <- grepl(pattern=paste("Grass", "_TranspCoeff", sep=""), x=names(sw_input_soils_use))
					sw_input_soils_use[i.temp][1:length(trco)] <- rep(1, times=length(trco))
					#add data to sw_input_soils
					i_sw_input_soils[i.temp][1:length(trco)] <- trco
				}
			}
			if(any(create_treatments == "LookupTranspCoeffFromTable_Shrub")){
				trco <- TranspCoeffByVegType(soillayer_no=d, trco_type=eval(parse(text=paste("LookupTranspCoeffFromTable_", "Shrub", sep="")), envir=i_sw_input_treatments), layers_depth=layers_depth)
				#set the use flags
				if(!is.na(trco)){
					i.temp <- grepl(pattern=paste("Shrub", "_TranspCoeff", sep=""), x=names(sw_input_soils_use))
					sw_input_soils_use[i.temp][1:length(trco)] <- rep(1, times=length(trco))
					#add data to sw_input_soils
					i_sw_input_soils[i.temp][1:length(trco)] <- trco
				}
			}
			if(any(create_treatments == "LookupTranspCoeffFromTable_Tree")){
				trco <- TranspCoeffByVegType(soillayer_no=d, trco_type=eval(parse(text=paste("LookupTranspCoeffFromTable_", "Tree", sep="")), envir=i_sw_input_treatments), layers_depth=layers_depth)
				if(!is.na(trco)){				
					i.temp <- grepl(pattern=paste("Tree", "_TranspCoeff", sep=""), x=names(sw_input_soils_use))
					sw_input_soils_use[i.temp][1:length(trco)] <- rep(1, times=length(trco))
					#add data to sw_input_soils
					i_sw_input_soils[i.temp][1:length(trco)] <- trco
				}
			}
			
			#the monthly ppt-shifts are extracted, but written to the weathersetup input file only at the end of the create section 'copy and make climate scenarios from datafiles', because they are multiplied with any climate change factors
			ppt_scShift <- rep(1, times=12)
			if(any(create_treatments=="LookupShiftedPPTScenarios")){
				ppt_scShift <- tr_input_shiftedPPT[which(rownames(tr_input_shiftedPPT) == i_sw_input_treatments["LookupShiftedPPTCategory"]),(ts <- which(colnames(tr_input_shiftedPPT) == paste(i_sw_input_treatments$LookupShiftedPPTScenarios, "_m1", sep=""))):(ts+11)][st_mo]
			}
			if(any(create_treatments=="LookupClimatePPTScenarios") ) {
				ppt_sc <- tr_input_climPPT[st_mo, which(colnames(tr_input_climPPT) == i_sw_input_treatments$LookupClimatePPTScenarios)]
			} else {
				ppt_sc <- rep(NA, times=12)
			}
			#Treatment chunk = climate scenarios
			if(any(create_treatments=="LookupClimateTempScenarios") ) {
				t_sc <- tr_input_climTemp[st_mo, which(colnames(tr_input_climTemp) == i_sw_input_treatments$LookupClimateTempScenarios)]
			} else {
				t_sc <- rep(NA, times=12)
			}
			if(any(create_treatments=="LookupClimatePPTScenarios") | any(create_treatments=="LookupClimateTempScenarios") ) {
				infilename <- file.path(dir.sw.runs.sc.in[1], weatherin)
				infiletext <- readLines(con = infilename)
				
				#information from input file
				ppt_old <- rep(1, times=12)
				t1_old <- t2_old <- rep(0, times=12)
				for (m in st_mo){
					wt <- na.omit(as.double(unlist(strsplit(infiletext[16+m], split="[[:space:]]"))))
					ppt_old[m] <- wt[2]
					t1_old[m] <- wt[3]
					t2_old[m] <- wt[4]
				}
				
				for (m in st_mo)
					infiletext[16+m] <- paste(m, formatC(ifelse(!is.na(ppt_sc[m]), ppt_sc[m], ppt_old[m]), digits=3, format="f"), formatC(ifelse(!is.na(t_sc[m]), t_sc[m], t1_old[m]), digits=2, format="f"), formatC(ifelse(!is.na(t_sc[m]), t_sc[m], t2_old[m]), digits=2, format="f"), sep="\t")
				
				infile <- file(infilename, "w+b")
				writeLines(text = infiletext, con = infile, sep = "\n")
				close(infile)
			}
			
			
			
			#------3. Step: Lookup or extract external information that needs to be executed for each run
			if(exinfo$ExtractGriddedDailyWeatherFromMaurer2002_NorthAmerica & !any(create_treatments == "LookupWeatherFolder")){
				ExtractGriddedDailyWeatherFromMaurer2002_NorthAmerica(site=i_labels, cellname=dirname.sw.runs.weather, dir.weath=dir.sw.runs.weather, sw.weather.praefix=filebasename.WeatherDataYear)
			}
			#EstimateConstantSoilTemperatureAtUpperAndLowerBoundaryAsMeanAnnualAirTemperature Taken from here
			
			
			#------4. Step: Information from datafiles are added if flagged 'use' to SoilWat input files
			#add information from datafile to cloudin
			wind <- with(i_sw_input_cloud, data.frame(wind_ms_1, wind_ms_2, wind_ms_3, wind_ms_4, wind_ms_5, wind_ms_6, wind_ms_7, wind_ms_8, wind_ms_9, wind_ms_10, wind_ms_11, wind_ms_12))
			if(do.wind <- datafile.windspeedAtHeightAboveGround != SoilWat.windspeedAtHeightAboveGround)
				wind <- adjust.WindspeedHeight(uz=wind, height=datafile.windspeedAtHeightAboveGround)
			
			if(sum(sw_input_cloud_use[-1]) > 0 | do.wind){
				infilename <- file.path(dir.sw.runs.weather, cloudin)
				infiletext <- readLines(con = infilename)
				
				#sky cover
				if(sum(sw_input_cloud_use[grepl(pattern="SkyC", x=names(sw_input_cloud_use))]) > 0) {
					sky <- with(i_sw_input_cloud, data.frame(SkyC_1, SkyC_2, SkyC_3, SkyC_4, SkyC_5, SkyC_6, SkyC_7, SkyC_8, SkyC_9, SkyC_10, SkyC_11, SkyC_12))
					infiletext[1] <- paste(do.call("paste", as.list(round(as.double(sky), 0), sep="\t")), "\t# (site:", i_labels, "), percentage of sky cover (sunrise-sunset).", i_sw_input_cloud$SkyC_Source, sep="\t")
				}
				#wind speed
				if(sum(sw_input_cloud_use[grepl(pattern="wind", x=names(sw_input_cloud_use))]) > 0 | do.wind) {
					infiletext[2] <- paste(do.call("paste", as.list(round(as.double(wind), 2), sep="\t")), "\t# Wind speed (m/s) at 2-m above ground.", ifelse(do.wind, paste("converted from", datafile.windspeedAtHeightAboveGround, "m."), ""), i_sw_input_cloud$Wind_Source, sep="\t")
				}
				#relative humidity
				if(sum(sw_input_cloud_use[grepl(pattern="RH", x=names(sw_input_cloud_use))]) > 0) {
					rh <- with(i_sw_input_cloud, data.frame(RH_1, RH_2, RH_3, RH_4, RH_5, RH_6, RH_7, RH_8, RH_9, RH_10, RH_11, RH_12))
					infiletext[3] <- paste(do.call("paste", as.list(round(as.double(rh), 0), sep="\t")), "\t# rel. Humidity (%).", i_sw_input_cloud$RH_Source, sep="\t")
				}
				#snow density
				if(sum(sw_input_cloud_use[grepl(pattern="snowd", x=names(sw_input_cloud_use))]) > 0) {
					snowd <- with(i_sw_input_cloud, data.frame(snowd_1, snowd_2, snowd_3, snowd_4, snowd_5, snowd_6, snowd_7, snowd_8, snowd_9, snowd_10, snowd_11, snowd_12))
					if(i_SWRunInformation$Y_WGS84 < 0 && i_sw_input_cloud$SnowD_Hemisphere == "N" || i_SWRunInformation$Y_WGS84 > 0 && i_sw_input_cloud$SnowD_Hemisphere == "S"){	#adjust for hemisphere only if location and data are opposite
						snowd <- c(snowd[7:12], snowd[1:6])
					}
					infiletext[5] <- paste(do.call("paste", as.list(round(as.double(snowd), 1), sep="\t")), "\t# snow density (kg/m3). ", i_sw_input_cloud$SnowD_Source, sep="\t")
				}
				
				infile <- file(infilename, "w+b")
				writeLines(text = infiletext, con = infile, sep = "\n")
				close(infile)
			}
			
			#add vegetation information	from datafile to prodin
			if(sum(sw_input_prod_use[-1]) > 0){
				infilename <- file.path(dir.sw.runs.sc.in[1], prodin)
				infiletext <- readLines(con = infilename)
				
				#composition
				if(sum(use_comp <- unlist(sw_input_prod_use[grepl(pattern="Composition", x=names(sw_input_prod_use))])) > 0) {
					comp.file  <- as.numeric((vec <- (unlist(strsplit(infiletext[6], split="[[:space:]]"))))[grep("[[:digit:]]", vec)])[1:3]
					comp.datfile <- with(i_sw_input_prod, data.frame(Composition_GrassFraction, Composition_ShrubFraction, Composition_TreeFraction))
					infiletext[6] <- paste(format(ifelse(use_comp, comp.datfile, comp.file), nsmall=0), collapse="\t\t\t")
				}
				#albedo
				if(sum(use_albedo <- unlist(sw_input_prod_use[grepl(pattern="Albedo", x=names(sw_input_prod_use))])) > 0) {
					albedo.file  <- as.numeric((vec <- (unlist(strsplit(infiletext[11], split="[[:space:]]"))))[grep("[[:digit:]]", vec)])[1:3]
					albedo.datfile <- with(i_sw_input_prod, data.frame(Grass_Albedo, Shrub_Albedo, Tree_Albedo))
					infiletext[11] <- paste(paste(format(ifelse(use_albedo, albedo.datfile, albedo.file), nsmall=2), collapse="\t\t\t"), "\t# albedo:\t(Houldcroft et al. 2009) MODIS snowfree 'grassland', 'open shrub', ‘evergreen needle forest’ with MODIS albedo aggregated over pure IGBP cells where NDVI is greater than the 98th percentile NDVI", sep="\t")
					
				}
				#constant canopy height
				if(sum(use_height <- unlist(sw_input_prod_use[grepl(pattern="CanopyHeight_Constant", x=names(sw_input_prod_use))])) > 0) {
					height.file  <- as.numeric((vec <- (unlist(strsplit(infiletext[25], split="[[:space:]]"))))[grep("[[:digit:]]", vec)])[1:3]
					height.datfile <- with(i_sw_input_prod, data.frame(Grass_CanopyHeight_Constant_cm, Shrub_CanopyHeight_Constant_cm, Tree_CanopyHeight_Constant_cm))
					infiletext[25] <- paste(paste(format(ifelse(use_height, height.datfile, height.file), nsmall=0), collapse="\t\t\t"), "\t# if > 0 then constant canopy height (cm)", sep="\t")
				}
				#flag for hydraulic redistribution
				if(sum(use_HD <- unlist(sw_input_prod_use[grepl(pattern="HydRed", x=names(sw_input_prod_use))])) > 0) {
					HD.file  <- as.numeric((vec <- (unlist(strsplit(infiletext[66], split="[[:space:]]"))))[grep("[[:digit:]]", vec)])[1:3]
					HD.datfile <- with(i_sw_input_prod, data.frame(Grass_HydRed_OnOff, Shrub_HydRed_OnOff, Tree_HydRed_OnOff))
					infiletext[66] <- paste(paste(format(ifelse(use_HD, HD.datfile, HD.file), nsmall=0), collapse="\t\t\t"), "\t# flag to turn on/off (1/0) hydraulic redistribution", sep="\t")
				}
				#biomass components
				biomassComponents <- function(FunctGroup, startline, infiletext){
					if(	sum(litt <- sw_input_prod_use[grepl(pattern=paste(FunctGroup, "_Litter", sep=""), x=names(sw_input_prod_use))]) + 
							sum(biom <- sw_input_prod_use[grepl(pattern=paste(FunctGroup, "_Biomass", sep=""), x=names(sw_input_prod_use))]) +
							sum(live <- sw_input_prod_use[grepl(pattern=paste(FunctGroup, "_FractionLive", sep=""), x=names(sw_input_prod_use))]) +
							sum(laiconv <- sw_input_prod_use[grepl(pattern=paste(FunctGroup, "_LAIconv", sep=""), x=names(sw_input_prod_use))])			> 0) {
						
						tempdat <- matrix(data=NA, nrow=12, ncol=4)
						colnames(tempdat) <- c("litt", "biom", "live", "laiconv")
						if(sum(litt) + sum(biom) + sum(live) + sum(laiconv) < 48) {	#information comes from part datafile, part input file -> read input file
							for (m in st_mo){
								vec <- (unlist(strsplit(infiletext[startline + m], split="[[:space:]]")))
								tempdat[m, ]  <- as.numeric(vec[grep("[[:digit:]]", vec)])
							}
						}
						for (m in st_mo){
							mo.dat <- with(i_sw_input_prod, c(	ifelse(litt[m], eval(parse(text=paste(FunctGroup, "_Litter_m", m, sep=""))), tempdat[m, "litt"]),
											ifelse(biom[m], eval(parse(text=paste(FunctGroup, "_Biomass_m", m, sep=""))), tempdat[m, "biom"]),
											ifelse(live[m], eval(parse(text=paste(FunctGroup, "_FractionLive_m", m, sep=""))), tempdat[m, "live"]),
											ifelse(laiconv[m], eval(parse(text=paste(FunctGroup, "_LAIconv_m", m, sep=""))), tempdat[m, "laiconv"])))
							infiletext[startline + m] <- paste(format(mo.dat[1], nsmall=1), format(mo.dat[2], nsmall=1), format(mo.dat[3], nsmall=2), format(mo.dat[4], nsmall=0), "\t# ", month.name[m], sep="\t")
						}
					}
					return(infiletext)
				}
				infiletext <- biomassComponents(FunctGroup="Grass", startline=85, infiletext=infiletext)
				infiletext <- biomassComponents(FunctGroup="Shrub", startline=100, infiletext=infiletext)
				infiletext <- biomassComponents(FunctGroup="Tree", startline=115, infiletext=infiletext)
				
				infile <- file(infilename, "w+b")
				writeLines(text = infiletext, con = infile, sep = "\n")
				close(infile)
			}
			#Moved adjust to southern Hemi
			
			#add site information to siteparamin
			infilename <- file.path(dir.sw.runs.sc.in[1], siteparamin)
			infiletext <- readLines(con = infilename)
			if(sum(sw_input_site_use[-1]) > 0){
				if(sw_input_site_use$SWC_min){
					infiletext[2] <- paste(format(i_sw_input_site$SWC_min, nsmall=ifelse(i_sw_input_site$SWC_min < 1, 4, 1)) , "\t# swc_min : cm/cm if < 1.0, -bars if >= 1.0.", sep="")
				}
				if(sw_input_site_use$SWC_init){
					infiletext[3] <- paste(format(i_sw_input_site$SWC_init, nsmall=ifelse(i_sw_input_site$SWC_init < 1, 4, 1)) , "\t# swc_init: cm/cm if < 1.0, -bars if >= 1.0.", sep="")
				}
				if(sw_input_site_use$SWC_wet){
					infiletext[4] <- paste(format(i_sw_input_site$SWC_wet, nsmall=ifelse(i_sw_input_site$SWC_wet < 1, 4, 1)) , "\t# swc_wet : cm/cm if < 1.0, -bars if >= 1.0.", sep="")
				}
				if(sw_input_site_use$SWC_YearlyReset){
					infiletext[7] <- paste(format(i_sw_input_site$SWC_YearlyReset, nsmall=0) , "\t# reset (1/0): reset/don't reset swc each new year", sep="")
				}
				if(sw_input_site_use$SWC_Deepdrain){
					infiletext[8] <- paste(format(i_sw_input_site$SWC_Deepdrain, nsmall=0) , "\t# deepdrain (1/0): allow/disallow deep drainage function.", sep="")
				}
				if(sw_input_site_use$PET_multiplier){
					infiletext[10] <- paste(format(i_sw_input_site$PET_multiplier, nsmall=2) , "\t# multiplier for PET (eg for climate change).", sep="")
				}
				if(sw_input_site_use$RunoffPercent_fromPondedWater){
					infiletext[11] <- paste(format(i_sw_input_site$RunoffPercent_fromPondedWater, nsmall=3), "\t# proportion of ponded surface water removed as runoff daily (value ranges between 0 and 1; 0=no loss of surface water, 1=all ponded water lost via runoff)", sep="")
				}#replace if in treatment file
				if(sw_input_site_use$Param_UnsaturatedPercolation){
					infiletext[22] <- paste(format(i_sw_input_site$Param_UnsaturatedPercolation, nsmall=4) , "\t\t# slow-drain coefficient per layer (cm/day).  See Eqn 2.9 in ELM doc.", sep="")
				}
				if(sw_input_site_use$Slope){
					infiletext[48] <- paste(format(i_sw_input_site$Slope, nsmall=2) , "\t# slope at site (degrees): no slope = 0", sep="")
				}
				if(sw_input_site_use$Aspect){
					infiletext[49] <- paste(format(i_sw_input_site$Aspect, nsmall=0) , "\t# aspect at site (degrees): N=0, E=90, S=180, W=270, no slope:-1", sep="")
				}
				#Moved sw_input_site_use$SoilTempC_atLowerBoundary
				if(sw_input_site_use$SoilTemp_Flag){
					infiletext[63] <- paste(format(i_sw_input_site$SoilTemp_Flag, nsmall=0) , "\t\t\t# flag, 1 to calculate soil_temperature, 0 to not calculate soil_temperature", sep="")
				}
			}
			infiletext[46] <- paste(formatC(i_SWRunInformation$Y_WGS84 * pi / 180, digits=3, format="f"), "\t# latitude of the site in radians, site = ", i_labels, sep="")
			if(is.finite(i_SWRunInformation$ELEV_m))	infiletext[47] <- paste(i_SWRunInformation$ELEV_m, "\t# altitude of site (m a.s.l.)", sep="")
			infile <- file(infilename, "w+b")
			writeLines(text = infiletext, con = infile, sep = "\n")
			close(infile)
			
			#add soil information to soilsin
			if(sum(sw_input_soils_use[-1]) - sum(use_transpregion <- unlist(lapply(parse(text=paste("TranspRegion_L", ld, sep="")), FUN=eval, envir=sw_input_soils_use))) > 0){
				infilename <- file.path(dir.sw.runs.sc.in[1], soilsin)
				infiletext <- readLines(con = infilename)
				
				tempdat <- matrix(data=NA, nrow=SoilLayer_MaxNo, ncol=12)
				colnames(tempdat) <- c("depth", "bulkd", "fieldc", "wiltp", "evco", "trco_grass", "trco_shrub", "trco_tree", "sand", "clay", "imperm", "soiltemp")
				
				#recalculate soil layer structure, because any(create_treatments=="soilsin") and soilsin may have a different soil layer structure than the datafiles
				layers_depth.datafile <- (temp <- as.numeric(na.omit(unlist(i_sw_input_soillayers[match(paste("depth_L", 1:SoilLayer_MaxNo, sep=""), colnames(i_sw_input_soillayers))]))))[temp <= as.numeric(i_sw_input_soillayers["SoilDepth_cm"])]
				
				if(all(layers_depth.datafile == layers_depth)){	#same soil layer structure in soilsin and datafile => combine data
					#soil texture data from SoilWat input file
					for (l in soilsin.firstDataLine:length(infiletext)){
						vec <- (unlist(strsplit(infiletext[l], split="[[:space:]]")))
						vec <- as.numeric(vec[grep("[[:digit:]]", vec)])
						if(length(vec) == ncol(tempdat)){
							tempdat[l-soilsin.firstDataLine+1, ]  <- vec
						} else {
							break
						}
					}
					
					#flags for use of texture data from datafile
					use_bulkd <- unlist(lapply(parse(text=paste("BD_L", ld, sep="")), FUN=eval, envir=sw_input_soils_use))
					use_fieldc <- unlist(lapply(parse(text=paste("FieldC_L", ld, sep="")), FUN=eval, envir=sw_input_soils_use))
					use_pwp <- unlist(lapply(parse(text=paste("WiltP_L", ld, sep="")), FUN=eval, envir=sw_input_soils_use))
					sum_use_evco <- sum(unlist(lapply(parse(text=paste("EvapCoeff_L", ld, sep="")), FUN=eval, envir=sw_input_soils_use)))
					sum_use_trco_grass <- sum(unlist(lapply(parse(text=paste("Grass_TranspCoeff_L", ld, sep="")), FUN=eval, envir=sw_input_soils_use)))
					sum_use_trco_shrub <- sum(unlist(lapply(parse(text=paste("Shrub_TranspCoeff_L", ld, sep="")), FUN=eval, envir=sw_input_soils_use)))
					sum_use_trco_tree <- sum(unlist(lapply(parse(text=paste("Tree_TranspCoeff_L", ld, sep="")), FUN=eval, envir=sw_input_soils_use)))
					use_sand <- unlist(lapply(parse(text=paste("Sand_L", ld, sep="")), FUN=eval, envir=sw_input_soils_use))
					use_clay <- unlist(lapply(parse(text=paste("Clay_L", ld, sep="")), FUN=eval, envir=sw_input_soils_use))
					use_imperm <- unlist(lapply(parse(text=paste("Imperm_L", ld, sep="")), FUN=eval, envir=sw_input_soils_use))
					
				} else { #different soil layer structure in soilsin and datafile AND since variables are flagged in sw_input_soils_use => use only datafile values
					d <- max(1, min(length(layers_depth.datafile), findInterval(i_sw_input_soillayers$SoilDepth_cm - sqrt(.Machine$double.neg.eps), c(0, layers_depth.datafile)), na.rm=TRUE), na.rm=TRUE)
					layers_depth <- adjustLayersDepth(layers_depth.datafile, d)
					layers_width <- getLayersWidth(layers_depth)
					ld <- setLayerSequence(d)
					
					DeepestTopLayer <- setDeepestTopLayer(d)
					topL <- setTopLayer(d)
					bottomL <- setBottomLayer(d)
					
					#set all flags to use=1
					use_bulkd <- use_fieldc <- use_pwp <- use_sand <- use_clay <- use_imperm <- rep(1, times=d)
					sum_use_evco <- sum_use_trco_grass <- sum_use_trco_shrub <- sum_use_trco_tree <- 1
				}
				
				#tr and ev coefficients data from datafile
				evco <- unlist(lapply(parse(text=paste("EvapCoeff_L", ld, sep="")), FUN=eval, envir=i_sw_input_soils))
				trco_grass <- unlist(lapply(parse(text=paste("Grass_TranspCoeff_L", ld, sep="")), FUN=eval, envir=i_sw_input_soils))
				trco_shrub <- unlist(lapply(parse(text=paste("Shrub_TranspCoeff_L", ld, sep="")), FUN=eval, envir=i_sw_input_soils))
				trco_tree <- unlist(lapply(parse(text=paste("Tree_TranspCoeff_L", ld, sep="")), FUN=eval, envir=i_sw_input_soils))
				
				#normalize transpiration and evaporation coefficients from datafile
				if(sum_use_evco) evco <- evco / ifelse((temp <- sum(evco, na.rm=TRUE)) == 0 & is.na(temp), 1, temp)
				if(sum_use_trco_grass) trco_grass <- trco_grass / ifelse((temp <- sum(trco_grass, na.rm=TRUE)) == 0 & is.na(temp), 1, temp)
				if(sum_use_trco_shrub) trco_shrub <- trco_shrub / ifelse((temp <- sum(trco_shrub, na.rm=TRUE)) == 0 & is.na(temp), 1, temp)
				if(sum_use_trco_tree) trco_tree <- trco_tree / ifelse((temp <- sum(trco_tree, na.rm=TRUE)) == 0 & is.na(temp), 1, temp)
				
				#compile soil information from both sources
				#changed ncol to 12, and added "soil_temp" to colnames...
				soildat <- matrix(data=NA, nrow=d, ncol=12)
				colnames(soildat) <- c("depth", "bulkd", "fieldc", "wiltp", "evco", "trco_grass", "trco_shrub", "trco_tree", "sand", "clay", "imperm", "soiltemp")
				for (l in ld){
					soildat[l, ] <- c(	layers_depth.datafile[l],
							ifelse(use_bulkd[l], eval(parse(text=paste("BD_L", l, sep="")), envir=i_sw_input_soils), tempdat[l, "bulkd"]),
							ifelse(use_fieldc[l], eval(parse(text=paste("FieldC_L", l, sep="")), envir=i_sw_input_soils), tempdat[l, "fieldc"]),
							ifelse(use_pwp[l], eval(parse(text=paste("WiltP_L", l, sep="")), envir=i_sw_input_soils), tempdat[l, "wiltp"]),
							ifelse(!is.na(temp <- ifelse(sum_use_evco, evco[l], tempdat[l, "evco"])), temp, 0),
							ifelse(!is.na(temp <- ifelse(sum_use_trco_grass, trco_grass[l], tempdat[l, "trco_grass"])), temp, 0),
							ifelse(!is.na(temp <- ifelse(sum_use_trco_shrub, trco_shrub[l], tempdat[l, "trco_shrub"])), temp, 0),
							ifelse(!is.na(temp <- ifelse(sum_use_trco_tree, trco_tree[l], tempdat[l, "trco_tree"])), temp, 0),
							ifelse(use_sand[l], eval(parse(text=paste("Sand_L", l, sep="")), envir=i_sw_input_soils), tempdat[l, "sand"]),
							ifelse(use_clay[l], eval(parse(text=paste("Clay_L", l, sep="")), envir=i_sw_input_soils), tempdat[l, "clay"]),
							ifelse(!is.na(temp <- ifelse(use_imperm[l], eval(parse(text=paste("Imperm_L", l, sep="")), envir=i_sw_input_soils), tempdat[l, "imperm"])), temp, 0),
							0	#soiltemp information may depend on climatic conditions; it will be only added after weather/climate scenarios are completed
					)
				}
				
				#adjust deepest soil layer if there is no soil information for the lowest layers, but needs to recalculate soil layer structure
				for(temp in d:1){
					if(all(!is.na(soildat[temp, "bulkd"]), soildat[temp, "bulkd"] > 0, !is.na(soildat[temp, "sand"]), soildat[temp, "sand"] > 0, !is.na(soildat[temp, "clay"]), soildat[temp, "clay"] > 0)) break
				}
				if(d != temp){
					d <- temp	
					layers_depth <- adjustLayersDepth(layers_depth, d)
					layers_width <- getLayersWidth(layers_depth)
					ld <- setLayerSequence(d)
					
					DeepestTopLayer <- setDeepestTopLayer(d)
					topL <- setTopLayer(d)
					bottomL <- setBottomLayer(d)
				}
				
				# changed all 16's to 17 to update # of lines in soils_v23.in file
				infiletext.nrow <- length(infiletext)
				this_soil <- soildat[1, ]
				for (l in ld){
					missingtext <- ifelse(soildat[l, "bulkd"] > 0 & soildat[l, "fieldc"] > 0 & soildat[l, "wiltp"] > 0 & soildat[l, "sand"] > 0 & soildat[l, "clay"] > 0,"", "\t/* soil data missing for this layer -> data used from previous layer */")
					if(nchar(missingtext)==0){
						this_soil <- soildat[l, ]
					} else {
						this_soil <- c(soildat[l, "depth"], this_soil[2:4], soildat[l, "evco"], soildat[l, "trco_grass"], soildat[l, "trco_shrub"], soildat[l, "trco_tree"], this_soil[9:10], soildat[l, "imperm"], soildat[l, "soiltemp"])
					}
					soilline <- paste(formatC(this_soil[1], digits=0, format="f"), formatC(this_soil[2], digits=2, format="f"), formatC(this_soil[3], digits=4, format="f"), formatC(this_soil[4], digits=4, format="f"), formatC(this_soil[5], digits=4, format="f"), formatC(this_soil[6], digits=4, format="f"), formatC(this_soil[7], digits=4, format="f"), formatC(this_soil[8], digits=4, format="f"), formatC(this_soil[9], digits=4, format="f"), formatC(this_soil[10], digits=4, format="f"), formatC(this_soil[11], digits=0, format="f"), formatC(this_soil[12], digits=4, format="f"), missingtext, sep="\t")
					if(soilsin.firstDataLine-1 + l > infiletext.nrow){
						infiletext <- c(infiletext, soilline)
					} else {
						infiletext[soilsin.firstDataLine-1 + l] <- soilline
					}
				}
				if(soilsin.firstDataLine-1 + max(ld) + 1 > infiletext.nrow){
					infiletext <- c(infiletext, "")
				} else {
					infiletext[soilsin.firstDataLine + max(ld)] <- ""	#SoilWat is happy with a empty last line
					infiletext <- infiletext[1:(soilsin.firstDataLine-1 + max(ld))]	#delete unused additional lines
				}
				infile <- file(infilename, "w+b")
				writeLines(text = infiletext, con = infile, sep = "\n")
				close(infile)
			}
			
			
			#add transpiration regions information to siteparamin
			if(sum(use_transpregion) > 0){
				tr <- max(tr.layers <- na.omit(unlist(lapply(parse(text=paste("TranspRegion_L", ld, sep="")), FUN=eval, envir=i_sw_input_soils)))) # max transpiration region
				
				infilename <- file.path(dir.sw.runs.sc.in[1], siteparamin)
				infiletext <- readLines(con = infilename)
				
				ltreg.last <- 0
				for(tri in 1:4){
					ltreg <- ifelse(length(ind <- which(tr.layers==tri)) > 0, max(ind), -1)
					ltreg <- ifelse(ltreg>ltreg.last, ltreg, ltreg.last+1)
					ltreg <- ifelse(ltreg>d & tri==1, d, ltreg)
					
					# was 53 for siteparam_v21.in, now 67 for siteparam_v23.in, 70 for siteparam_v26.in
					infiletext[70+tri] <- paste(ifelse(tri <= tr & tri <= d & ltreg <= d | tri == 1, "\t", "# "), tri, "\t\t", ltreg , sep="")
					ltreg.last <- ltreg
				}
				
				infile <- file(infilename, "w+b")
				writeLines(text = infiletext, con = infile, sep = "\n")
				close(infile)
			}
			
			#add weather setup information to weatherin
			infilename <- file.path(dir.sw.runs.sc.in[1], weatherin)
			infiletext <- readLines(con = infilename)
			if(sw_input_weather_use$SnowFlag)
				infiletext[4] <- paste(i_sw_input_weather$SnowFlag, "\t# 1=split ppt into rain and snow,   0=no snow effects.", sep="")
			if(sw_input_weather_use$SnowDrift_Percent)
				infiletext[5] <- paste(i_sw_input_weather$SnowDrift_Percent, "\t# % of snow drift per snow event (+ indicates snow addition, - indicates snow taken away from site)", sep="")
			if(sw_input_weather_use$RunOffOnPerSnowmelt_Percent)
				infiletext[6] <- paste(i_sw_input_weather$RunOffOnPerSnowmelt_Percent, "\t# % of snowmelt water as runoff/on per event (>0 indicates runoff, <0 indicates runon)", sep="")
			infiletext[8] <- paste(simstartyr, "\t# first year to begin historical weather.", sep="")
			infile <- file(infilename, "w+b")
			writeLines(text = infiletext, con = infile, sep = "\n")
			close(infile)
			
			#------Add different time-steps and create duplicate runs for different climate scenarios
			#copy outsetup/filesin from shortest to others among dy/we/mo/yr
			#for (r in 1:output_timescales_maxNo){ #daily, weekly, monthly, yearly output
			#	rt <- switch(EXPR=r, "daily", "weekly", "monthly", "yearly")
			#	if(output_timescales_shortest != r & any(simulation_timescales==rt)){
			#		fin <- swFilesIn
			#		oin <- swOutSetupIn
			#		
			#		file.copy2(from=file.path(dir.sw.in, oin), to=file.path(dir.sw.runs.sc[1], oin), overwrite=TRUE, copy.mode = TRUE)
			#		
			#		infilename <- file.path(dir.sw.runs.sc[1], fin)
			#		file.copy2(from=file.path(dir.sw.runs.sc[1], filesin), to=infilename, overwrite=TRUE, copy.mode = TRUE)
			#
			#		infiletext <- readLines(con = infilename)
			#		infiletext[28] <- paste(sw.inputs, .Platform$file.sep, oin, "\t# define output quantities", sep="")
			#		infile <- file(infilename, "w+b")
			#		writeLines(text = infiletext, con = infile, sep = "\n")
			#		close(infile)
			#	}
			#}
			
			infilename <- file.path(dir.sw.runs.sc.in[1], swOutSetupIn)			
			infiletext <- readLines(con = infilename)
			timeSteps <- sapply(simulation_timescales, function(x) ifelse(x=="daily", "dy", ifelse(x=="weekly", "wk", ifelse(x=="monthly", "mo", ifelse(x=="yearly","yr","trouble")))) )
			infiletext[42] <- paste("TIMESTEP", paste(timeSteps, collapse=" "), sep=" ")
			infile <- file(infilename, "w+b")
			writeLines(text = infiletext, con = infile, sep = "\n")
			close(infile)
			
			
			#delete superfluous files through copying swruns, but then adding treatment files
			if(length(oldInputFiles.toDelete)) file.remove(oldInputFiles.toDelete)
			
			#copy and make climate scenarios from datafiles	
			for (sc in 1:scenario_No){
				if(sc > 1){
					dir.copy(dir.from=dir.sw.runs.sc[1], dir.to=dir.sw.runs.sc[sc], overwrite=TRUE)	#system(paste("cp -R", shQuote(dir.sw.runs.sc[1]), shQuote(dir.sw.runs.sc[sc])))
				} else {
					if(do.GetClimateMeans){
						do.C4vars <- any(create_treatments == "PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996") || aon$dailyC4_TempVar
						SiteClimate_Ambient <- SiteClimate(dir.weather=dir.sw.runs.weather, sw.weather.praefix=filebasename.WeatherDataYear, year.start=min(simTime$useyrs), year.end=max(simTime$useyrs), do.C4vars=do.C4vars, simTime2=simTime2)
					}
				}
				
				infilename <- file.path(dir.sw.runs.sc.in[sc], weatherin)
				infiletext <- readLines(con = infilename)
				
				#get climate change information
				use_pptValscen <- unlist(lapply(parse(text=pptVal.colnames <- paste("PPTmm_m", st_mo, "_sc", formatC(sc-1, width=2, format="d", flag="0"), sep="")), FUN=eval, envir=sw_input_climscen_values_use)) 
				use_tempValscen <- unlist(lapply(parse(text=tempVal.colnames <- paste("TempC_m", st_mo, "_sc", formatC(sc-1, width=2, format="d", flag="0"), sep="")), FUN=eval, envir=sw_input_climscen_values_use)) 
				use_pptscen <- unlist(lapply(parse(text=ppt.colnames <- paste("PPTfactor_m", st_mo, "_sc", formatC(sc-1, width=2, format="d", flag="0"), sep="")), FUN=eval, envir=sw_input_climscen_use)) 
				use_tempscen <- unlist(lapply(parse(text=temp.colnames <- paste("deltaTempC_m", st_mo, "_sc", formatC(sc-1, width=2, format="d", flag="0"), sep="")), FUN=eval, envir=sw_input_climscen_use)) 
				
				if(	sum(use_pptValscen) + sum(use_tempValscen) > 0){
					#convert climate change values to factors
					#read values from datafile
					pptVal_sc <- unlist(lapply(parse(text=pptVal.colnames), FUN=eval, envir=i_sw_input_climscen_values))
					tVal_sc <-  unlist(lapply(parse(text=tempVal.colnames), FUN=eval, envir=i_sw_input_climscen_values))
					#calculate change factors
					ppt_sc <- pptVal_sc / (10 * SiteClimate_Ambient$meanMonthlyPPTcm)
					t_sc <- tVal_sc - SiteClimate_Ambient$meanMonthlyTempC
				} else if(	sum(use_pptscen) + sum(use_tempscen) > 0){
					#read climate change factors from datafile
					ppt_sc <- unlist(lapply(parse(text=ppt.colnames), FUN=eval, envir=i_sw_input_climscen))
					t_sc <-  unlist(lapply(parse(text=temp.colnames), FUN=eval, envir=i_sw_input_climscen))
				} else {
					ppt_sc <- rep(1, times=12)
					t_sc <- rep(0, times=12)
				}
				#guarantee that all entries are finite: this may not be the case for instance if any(meanMonthlyClimate$meanMonthlyPPTcm == 0)
				ppt_sc <- temp_ppt_sc <- ifelse(is.finite(ppt_sc), ppt_sc, 1)
				t_sc <- ifelse(is.finite(t_sc), t_sc, 0)
				
				
				if(sc > 1){
					if(any(create_treatments=="ClimateScenario_Temp_PerturbationInMeanSeasonalityBothOrNone")){
						if(grepl("Mean", i_sw_input_treatments$ClimateScenario_Temp_PerturbationInMeanSeasonalityBothOrNone, ignore.case=T)) t_sc <- rep(mean(t_sc), times=12)
						if(grepl("Seasonality", i_sw_input_treatments$ClimateScenario_Temp_PerturbationInMeanSeasonalityBothOrNone, ignore.case=T)) t_sc <- t_sc - mean(t_sc)
						if(grepl("None", i_sw_input_treatments$ClimateScenario_Temp_PerturbationInMeanSeasonalityBothOrNone, ignore.case=T)) t_sc <- 0
					}
					if(any(create_treatments=="ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone")){
						temp_map_sc <- sum(SiteClimate_Ambient$meanMonthlyPPTcm * temp_ppt_sc)
						if(grepl("Mean", i_sw_input_treatments$ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone, ignore.case=T)) ppt_sc = rep(temp_map_sc / SiteClimate_Ambient$MAP_cm, times=12)
						if(grepl("Seasonality", i_sw_input_treatments$ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone, ignore.case=T)) ppt_sc = ppt_sc * SiteClimate_Ambient$MAP_cm / temp_map_sc
						if(grepl("None", i_sw_input_treatments$ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone, ignore.case=T)) ppt_sc = 1;
					}
				}				
				
				#information from input file
				ppt_old <- rep(1, times=12)
				t1_old <- t2_old <- rep(0, times=12)
				for (m in st_mo){
					wt <- na.omit(as.double(unlist(strsplit(infiletext[16+m], split="[[:space:]]"))))
					ppt_old[m] <- wt[2]
					t1_old[m] <- wt[3]
					t2_old[m] <- wt[4]
				}
				
				#write information into weatherin
				if(use_pptscen || use_pptValscen){
					ppt_f <- ppt_sc
					t1_f <- t2_f <- t_sc
				} else {
					ppt_f <- ppt_old
					t1_f <- t1_old
					t2_f <- t2_old
					
				}
				ppt_f <- ppt_f * as.numeric(ppt_scShift)
				
				for (m in st_mo)
					infiletext[16+m] <- paste(m, formatC(ppt_f[m], digits=3, format="f"), formatC(t1_f[m], digits=2, format="f"), formatC(t2_f[m], digits=2, format="f"), sep="\t")
				
				infile <- file(infilename, "w+b")
				writeLines(text = infiletext, con = infile, sep = "\n")
				close(infile)
				
				#Update climate data with climate scenario information
				if(do.GetClimateMeans){
					SiteClimate_Scenario <- list()
					SiteClimate_Scenario$meanMonthlyPPTcm <- SiteClimate_Ambient$meanMonthlyPPTcm * ppt_f
					tmean_f <- apply(cbind(t1_f, t2_f), MARGIN=1, FUN=mean)
					SiteClimate_Scenario$meanMonthlyTempC <- SiteClimate_Ambient$meanMonthlyTempC + tmean_f
					SiteClimate_Scenario$MAP_cm <- sum(SiteClimate_Scenario$meanMonthlyPPTcm)
					SiteClimate_Scenario$MAT_C <- mean(SiteClimate_Scenario$meanMonthlyTempC)
					if(do.C4vars){
						SiteClimate_Scenario$dailyTempMin <- SiteClimate_Ambient$dailyTempMin + t2_f[simTime2$month_ForEachUsedDay]
						SiteClimate_Scenario$dailyTempMean <- SiteClimate_Ambient$dailyTempMean + tmean_f[simTime2$month_ForEachUsedDay]
						SiteClimate_Scenario$dailyC4vars <- dailyC4_TempVar(SiteClimate_Scenario$dailyTempMin, SiteClimate_Scenario$dailyTempMean, simTime2)
					}
				}
				
				#anything that depends on weather
				#------3. Step: Lookup or extract external information that needs to be executed for each run
				if(exinfo$EstimateConstantSoilTemperatureAtUpperAndLowerBoundaryAsMeanAnnualAirTemperature){
					soilTlower <- mean(SiteClimate_Scenario$meanMonthlyTempC)
					soilTUpper <- max(-1, mean(SiteClimate_Scenario$meanMonthlyTempC[c(1,12)]))
					#temporaly save data
					out.temp <- data.frame(i, i_labels, soilTUpper, soilTlower)
					write.csv(out.temp, file=paste(dir.out.temp, .Platform$file.sep, flag.icounter, "_", "SoilTempC_atLowerBoundary.csv", sep=""), quote=FALSE, row.names=FALSE)
				}
				if(sw_input_site_use$SoilTempC_atUpperBoundary) {
					soilTUpper <- ifelse(exists("soilTUpper"), soilTUpper, i_sw_input_site$SoilTempC_atUpperBoundary)
				}
				if(sw_input_site_use$SoilTempC_atLowerBoundary){
					soilTlower <- ifelse(exists("soilTlower"), soilTlower, i_sw_input_site$SoilTempC_atLowerBoundary)
					
					infilename <- file.path(dir.sw.runs.sc.in[sc], siteparamin)
					infiletext <- readLines(con = infilename)
					infiletext[60] <- paste(format(soilTlower, digits=3) , "		# constant mean air temperature (the soil temperature at the lower boundary, 180 cm) in celsius", sep="")
					
					infile <- file(infilename, "w+b")
					writeLines(text = infiletext, con = infile, sep = "\n")
					close(infile)
				}
				if(exinfo$EstimateInitialSoilTemperatureForEachSoilLayer){
					init.soilTprofile <- EstimateInitialSoilTemperatureForEachSoilLayer(layers_depth=layers_depth, lower.Tdepth=180, soilTupper=soilTUpper, soilTlower=soilTlower)	#lower.Tdepth needs to be adjusted if it changes in soilparam.in
					#temporaly save data
					out.temp <- data.frame(i, i_labels, t(c(init.soilTprofile, rep(NA, times=SoilLayer_MaxNo-length(init.soilTprofile)))))
					write.csv(out.temp, file=paste(dir.out.temp, .Platform$file.sep, flag.icounter, "_", "SoilTempC_InitProfile.csv", sep=""), quote=FALSE, row.names=FALSE)
				}
				
				#adjust init soil temperatures to climatic conditions
				if(use_soil_temp <- unlist(lapply(parse(text=paste("SoilTemp_L", ld, sep="")), FUN=eval, envir=sw_input_soils_use))){
					infilename <- file.path(dir.sw.runs.sc.in[sc], soilsin)
					infiletext <- readLines(con = infilename)
					
					tempdat <- matrix(data=NA, nrow=SoilLayer_MaxNo, ncol=12)
					colnames(tempdat) <- c("depth", "bulkd", "fieldc", "wiltp", "evco", "trco_grass", "trco_shrub", "trco_tree", "sand", "clay", "imperm", "soiltemp")
					
					#soil texture data from SoilWat input file and soil temperature data from sources
					for (l in soilsin.firstDataLine:length(infiletext)){
						vec <- (unlist(strsplit(infiletext[l], split="[[:space:]]")))
						vec <- as.numeric(vec[grep("[[:digit:]]", vec)])
						if(length(vec) == ncol(tempdat)){
							tempdat[l-soilsin.firstDataLine+1, ]  <- vec
							tempdat[l-soilsin.firstDataLine+1, "soiltemp"] <- ifelse(!is.na(temp <- ifelse(use_soil_temp[l-soilsin.firstDataLine+1], ifelse(exists("init.soilTprofile"), init.soilTprofile[l-soilsin.firstDataLine+1], eval(parse(text=paste("SoilTemp_L", l-soilsin.firstDataLine+1, sep="")), envir=i_sw_input_soils)), tempdat[l-soilsin.firstDataLine+1, "soiltemp"])), temp, 0)
							
							infiletext[l] <- paste(formatC(tempdat[l-soilsin.firstDataLine+1,1], digits=0, format="f"), formatC(tempdat[l-soilsin.firstDataLine+1,2], digits=2, format="f"), formatC(tempdat[l-soilsin.firstDataLine+1,3], digits=4, format="f"), formatC(tempdat[l-soilsin.firstDataLine+1,4], digits=4, format="f"), formatC(tempdat[l-soilsin.firstDataLine+1,5], digits=4, format="f"), formatC(tempdat[l-soilsin.firstDataLine+1,6], digits=4, format="f"), formatC(tempdat[l-soilsin.firstDataLine+1,7], digits=4, format="f"), formatC(tempdat[l-soilsin.firstDataLine+1,8], digits=4, format="f"), formatC(tempdat[l-soilsin.firstDataLine+1,9], digits=4, format="f"), formatC(tempdat[l-soilsin.firstDataLine+1,10], digits=4, format="f"), formatC(tempdat[l-soilsin.firstDataLine+1,11], digits=4, format="f"), formatC(tempdat[l-soilsin.firstDataLine+1,12], digits=4, format="f"), sep="\t")
						} else {
							break
						}
					}
					
					infile <- file(infilename, "w+b")
					writeLines(text = infiletext, con = infile, sep = "\n")
					close(infile)
				}
				
				
				#- Calculate relative composition based on equations
				if(any(create_treatments == "PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996") && i_sw_input_treatments$PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996){
					#Climate variables
					if(any(create_treatments == "PotentialNaturalVegetation_Composition_basedOnReferenceOrScenarioClimate") && i_sw_input_treatments$PotentialNaturalVegetation_Composition_basedOnReferenceOrScenarioClimate=="Reference"){
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
					#Functions
					cut0Inf <- function(x) {x[x < 0] <- NA; return(x)}
					finite01 <- function(x) {x[x < 0 | is.na(x)] <- 0; x[x > 1] <- 1; return(x)}
					#to file
					FractionsToFile <- function(shrubs, grass, trees) {
						infilename <- file.path(dir.sw.runs.sc.in[sc], prodin)
						infiletext <- readLines(con = infilename)
						infiletext[6] <- paste(formatC(grass, format="f", digits=3), formatC(shrubs, format="f", digits=3), formatC(trees, format="f", digits=3), sep="\t")	#Trees are set to 0
						infile <- file(infilename, "w+b")
						writeLines(text = infiletext, con = infile, sep = "\n")
						close(infile)
					}
					#Get the user specified fractions, if column is false set to NA
					AnnC4C3ShrubFraction <- c(NA, NA, NA, NA)
					ifelse(any(create_treatments == "PotentialNaturalVegetation_CompositionAnnuals_Fraction"), AnnC4C3ShrubFraction[1] <- finite01(i_sw_input_treatments$PotentialNaturalVegetation_CompositionAnnuals_Fraction), AnnC4C3ShrubFraction[1] <- 0) #Annual can not be NA
					ifelse(any(create_treatments == "PotentialNaturalVegetation_CompositionC4_Fraction"), AnnC4C3ShrubFraction[2] <- i_sw_input_treatments$PotentialNaturalVegetation_CompositionC4_Fraction, AnnC4C3ShrubFraction[2] <- NA)
					ifelse(any(create_treatments == "PotentialNaturalVegetation_CompositionC3_Fraction"), AnnC4C3ShrubFraction[3] <- i_sw_input_treatments$PotentialNaturalVegetation_CompositionC3_Fraction, AnnC4C3ShrubFraction[3] <-NA)
					ifelse(any(create_treatments == "PotentialNaturalVegetation_CompositionShrubs_Fraction"), AnnC4C3ShrubFraction[4] <- i_sw_input_treatments$PotentialNaturalVegetation_CompositionShrubs_Fraction, AnnC4C3ShrubFraction[4] <-NA)
					AnnC4C3ShrubFraction <- cut0Inf(AnnC4C3ShrubFraction) #some reason we have negatives
					TotalFraction <- sum(AnnC4C3ShrubFraction, na.rm=TRUE)
					
					if(TotalFraction <= 1+sqrt(.Machine$double.eps) && TotalFraction >= 1-sqrt(.Machine$double.neg.eps)) { #no more work just use what we set
						#the total is one so set NA to 0
						AnnC4C3ShrubFraction <- finite01(AnnC4C3ShrubFraction)
						
						grass.c4.fractionG <- finite01(AnnC4C3ShrubFraction[2] * 1/(1-AnnC4C3ShrubFraction[4]))
						grass.c3.fractionG <- finite01(AnnC4C3ShrubFraction[3] * 1/(1-AnnC4C3ShrubFraction[4]))
						grass.Annual.fractionG <- finite01(AnnC4C3ShrubFraction[1] * 1/(1-AnnC4C3ShrubFraction[4]))
						
						grass.fraction <- sum(AnnC4C3ShrubFraction[c(1:3)])
						tree.fraction <- 0
						#write to file
						FractionsToFile(shrubs=AnnC4C3ShrubFraction[4], grass=grass.fraction, trees=tree.fraction)					
						
					} else if(TotalFraction > 1) { #error scale down to one
						#set NA to 0
						AnnC4C3ShrubFraction <- finite01(AnnC4C3ShrubFraction) # this might fix our problem
						#Total is high scale down to 1
						if(sum(AnnC4C3ShrubFraction) > 1)
							AnnC4C3ShrubFraction <- AnnC4C3ShrubFraction/TotalFraction
						
						grass.c4.fractionG <- finite01(AnnC4C3ShrubFraction[2] * 1/(1-AnnC4C3ShrubFraction[4]))
						grass.c3.fractionG <- finite01(AnnC4C3ShrubFraction[3] * 1/(1-AnnC4C3ShrubFraction[4]))
						grass.Annual.fractionG <- finite01(AnnC4C3ShrubFraction[1] * 1/(1-AnnC4C3ShrubFraction[4]))
						
						grass.fraction <- sum(AnnC4C3ShrubFraction[c(1:3)])
						tree.fraction <- 0
						#write to file
						FractionsToFile(shrubs=AnnC4C3ShrubFraction[4], grass=grass.fraction, trees=tree.fraction)	
						
					} else if(TotalFraction < 1) {  #need to calc some fractions
						if(sum(is.na(AnnC4C3ShrubFraction)) == 0) {
							#throw an error
							print(paste("User Defined Shrub, C3, C4, Annuals do not Add to 1. Run ", i, sep=""))
							stop()							
						}
						#do a quick check to make sure we have at least 2 NA
						if(sum(is.na(AnnC4C3ShrubFraction)) == 1) {
							#missing only one value, just find its fraction and plug in.
							AnnC4C3ShrubFraction[is.na(AnnC4C3ShrubFraction)] <- 1-sum(AnnC4C3ShrubFraction, na.rm=TRUE)
							
							grass.c4.fractionG <- finite01(AnnC4C3ShrubFraction[2] * 1/(1-AnnC4C3ShrubFraction[4]))
							grass.c3.fractionG <- finite01(AnnC4C3ShrubFraction[3] * 1/(1-AnnC4C3ShrubFraction[4]))
							grass.Annual.fractionG <- finite01(AnnC4C3ShrubFraction[1] * 1/(1-AnnC4C3ShrubFraction[4]))
							
							grass.fraction <- sum(AnnC4C3ShrubFraction[c(1:3)])
							tree.fraction <- 0
							#write to file
							FractionsToFile(shrubs=AnnC4C3ShrubFraction[4], grass=grass.fraction, trees=tree.fraction)
						} else if (sum(is.na(AnnC4C3ShrubFraction)) > 1) {
							if(i_SWRunInformation$Y_WGS84 >= 0){ #Northern hemisphere
								Months_WinterTF <- c(12, 1:2)
								Months_SummerTF <- c(6:8)
							} else {
								Months_WinterTF <- c(6:8)
								Months_SummerTF <- c(12, 1:2)
							}
							ppt.SummerToMAP <- sum(monthly.ppt[Months_SummerTF]) / MAP_mm
							ppt.WinterToMAP <- sum(monthly.ppt[Months_WinterTF]) / MAP_mm
							
							#---Potential natural vegetation
							#1. step: Paruelo JM, Lauenroth WK (1996) Relative abundance of plant functional types in grasslands and shrublands of North America. Ecological Applications, 6, 1212-1224.
							shrubs.fractionNA <- cut0Inf(1.7105 - 0.2918 * log(MAP_mm) + 1.5451 * ppt.WinterToMAP) 								#if NA, then not enough winter precipitation above a given MAP
							grass.c4.fractionNA <- cut0Inf(-0.9837 + 0.000594 * MAP_mm + 1.3528 * ppt.SummerToMAP + 0.2710 * log(MAT_C))			#if NA, then either MAT < 0 or not enough summer precipitation or too cold below a given MAP
							grass.c3ingrasslands.fractionNA <- cut0Inf(1.1905 - 0.02909 * MAT_C + 0.1781 * log(ppt.WinterToMAP) - 0.2383 * 1)		#if NA, then not enough winter precipitation or too warm below a given MAP
							grass.c3inshrublands.fractionNA <- cut0Inf(1.1905 - 0.02909 * MAT_C + 0.1781 * log(ppt.WinterToMAP) - 0.2383 * 2)
							grass.c3.fractionNA <- ifelse(shrubs.fractionNA >= shrub.fraction.limit && !is.na(shrubs.fractionNA), grass.c3inshrublands.fractionNA, grass.c3ingrasslands.fractionNA)
							
							grass.Annual.fraction <- AnnC4C3ShrubFraction[1] #Ann will be 0 or something <= 1
							
							
							#2. step: Teeri JA, Stowe LG (1976) Climatic patterns and the distribution of C4 grasses in North America. Oecologia, 23, 1-12.
							#This equations give percent species/vegetation -> use to limit Paruelo's C4 equation, i.e., where no C4 species => there are no C4 abundance > 0
							x10 <- dailyC4vars["Month7th_NSadj_MinTemp_C"] * 9/5 + 32
							x13 <- dailyC4vars["DegreeDaysAbove65F_NSadj_DaysC"] * 9/5
							x18 <- log(dailyC4vars["LengthFreezeFreeGrowingPeriod_NSadj_Days"])
							grass.c4.species <- as.numeric((1.60 * x10 + 0.0086 * x13 - 8.98 * x18 - 22.44) / 100)
							grass.c4.fractionNA <- ifelse(grass.c4.species > 0, grass.c4.fractionNA, NA)
							
							#3. step: Replacing missing values: If no or only one successful equation, then add 100% C3 if MAT < 10 C, 100% shrubs if MAP < 600 mm, and 100% C4 if MAT >= 10C & MAP >= 600 mm	[these rules are made up arbitrarily by drs, Nov 2012]
							if(sum(!is.na(shrubs.fractionNA), !is.na(grass.c4.fractionNA), !is.na(grass.c3.fractionNA)) <= 1){
								if(MAP_mm < 600) shrubs.fractionNA <- 1 + ifelse(is.na(shrubs.fractionNA), 0, shrubs.fractionNA)
								if(MAT_C < 10)  grass.c3.fractionNA <- 1 + ifelse(is.na(grass.c3.fractionNA), 0, grass.c3.fractionNA)
								if(MAT_C >= 10  & MAP_mm >= 600)  grass.c4.fractionNA <- 1 + ifelse(is.na(grass.c4.fractionNA), 0, grass.c4.fractionNA)
							}
							
							#4. step: Scale fractions to 0-1 with a sum of 1 including grass.Annual.fraction, but don't scale grass.Annual.fraction
							#if na then use calc fraction else use the user defined fraction
							shrubs.fraction <- finite01(shrubs.fractionNA)
							grass.c4.fraction <- finite01(grass.c4.fractionNA)
							grass.c3.fraction <- finite01(grass.c3.fractionNA)
							
							sumVegWithoutAnnuals <- shrubs.fraction + grass.c4.fraction + grass.c3.fraction
							shrubs.fraction <- (shrubs.fraction / sumVegWithoutAnnuals) * (1 - grass.Annual.fraction) #scale these down to 1-annual fraction
							grass.c4.fraction <- (grass.c4.fraction / sumVegWithoutAnnuals) * (1 - grass.Annual.fraction)
							grass.c3.fraction <- (grass.c3.fraction / sumVegWithoutAnnuals) * (1 - grass.Annual.fraction)
							
							calcAnnC4C3ShrubFraction <- c(grass.Annual.fraction, grass.c4.fraction, grass.c3.fraction, shrubs.fraction)
							naIndex <- which(is.na(AnnC4C3ShrubFraction))
							#replace missing values
							AnnC4C3ShrubFraction[naIndex] <- calcAnnC4C3ShrubFraction[naIndex]
							#now we need to get the sum and scale the naIndex values accordingly
							AnnC4C3ShrubFraction[naIndex] <- sapply(AnnC4C3ShrubFraction[naIndex], function(x) (x/sum(AnnC4C3ShrubFraction[naIndex])) * (1-sum(AnnC4C3ShrubFraction[-naIndex])))
							
							#Scale Grass components to one
							grass.c4.fractionG <- finite01(AnnC4C3ShrubFraction[2] * 1/(1-AnnC4C3ShrubFraction[4]))
							grass.c3.fractionG <- finite01(AnnC4C3ShrubFraction[3] * 1/(1-AnnC4C3ShrubFraction[4]))
							grass.Annual.fractionG <- finite01(AnnC4C3ShrubFraction[1] * 1/(1-AnnC4C3ShrubFraction[4]))
							
							grass.fraction <- sum(AnnC4C3ShrubFraction[c(1:3)])
							tree.fraction <- 0
							#write to file
							FractionsToFile(shrubs=AnnC4C3ShrubFraction[4], grass=grass.fraction, trees=tree.fraction)
						}
					}
				}
				
				if(any(create_treatments == "PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996") && i_sw_input_treatments$PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996 && ((any(create_treatments == "AdjMonthlyBioMass_Temperature") && i_sw_input_treatments$AdjMonthlyBioMass_Temperature) | (any(create_treatments == "AdjMonthlyBioMass_Precipitation") &&  i_sw_input_treatments$AdjMonthlyBioMass_Precipitation) )){
					tr_VegComp_Adj <- tr_VegetationComposition	#Default shrub biomass input is at MAP = 450 mm/yr, and default grass biomass input is at MAP = 340 mm/yr
					#Describe conditions for which the default vegetation biomass values are valid
					std.winter <- c(11:12, 1:2) #Assumes that the "growing season" (valid for growing.season.threshold.tempC == 4) in 'tr_VegetationComposition' starts in March and ends after October, for all functional groups.
					std.growing <- st_mo[-std.winter] #Assumes that the "growing season" in 'tr_VegetationComposition' starts in March and ends after October, for all functional groups.
					#Default site for the grass description is SGS LTER
					StandardGrasses_MAP_mm <- 340
					StandardGrasses_VegComposition <- c(0.12, 0.22, 0.66) #Fraction of shrubs, C3, and C4
					#Default site for the shrub description is Reynolds Creek, ID
					StandardShrub_MAP_mm <- 250
					StandardShrub_VegComposition <- c(0.7, 0.3, 0) #Fraction of shrubs, C3, and C4

					#Calculate 'live biomass amount'
					tr_VegComp_Adj$Sh.Amount.Live <- tr_VegComp_Adj$Sh.Biomass * tr_VegComp_Adj$Sh.Perc.Live
					tr_VegComp_Adj$C3.Amount.Live <- tr_VegComp_Adj$C3.Biomass * tr_VegComp_Adj$C3.Perc.Live
					tr_VegComp_Adj$C4.Amount.Live <- tr_VegComp_Adj$C4.Biomass * tr_VegComp_Adj$C4.Perc.Live
					tr_VegComp_Adj$Annual.Amount.Live <- tr_VegComp_Adj$Annual.Biomass * tr_VegComp_Adj$Annual.Perc.Live
					
					#Scale monthly values of litter and live biomass amount by column-max; total biomass will be back calculated from 'live biomass amount' / 'percent live'
					colmax <- apply(tr_VegComp_Adj[, itemp <- grepl("Litter", names(tr_VegComp_Adj)) | grepl("Amount.Live", names(tr_VegComp_Adj))], MARGIN=2, FUN=max)
					colmin <- apply(tr_VegComp_Adj[, itemp], MARGIN=2, FUN=min)
					tr_VegComp_Adj[, itemp] <- sweep(tr_VegComp_Adj[, itemp], MARGIN=2, STATS=colmax, FUN="/")
					
					#Pull different composition types
					shrubs_Composition <- shrubs_Standard <- tr_VegComp_Adj[, grepl("Sh", names(tr_VegComp_Adj))]
					C3_Composition <- C3_Standard <- tr_VegComp_Adj[, grepl("C3", names(tr_VegComp_Adj))]
					C4_Composition <- C4_Standard <- tr_VegComp_Adj[, grepl("C4", names(tr_VegComp_Adj))]
					AnnGrass_Composition <- AnnGrass_Standard <- tr_VegComp_Adj[, grepl("Annual", names(tr_VegComp_Adj))]
			
					adjCompPPT <- function(ShrubsMAP_mm, GrassMAP_mm) {
						#Equations: Milchunas & Lauenroth 1993 (Fig. 2): Y [g/m2/yr] = c1 * MAP [mm/yr] + c2
						Shrub_ANPP <- function(MAP_mm) 0.393 * MAP_mm - 10.2
						Grass_ANPP <- function(MAP_mm) 0.646 * MAP_mm - 102.5
						
						#Intercepts to match outcomes of M & L 1993 equations under 'default' MAP with our previous default inputs for shrubs and sgs-grasslands
						#Whereas these intercepts were introduced artificially, they could also be interpreted as perennial storage, e.g., Lauenroth & Whitman (1977) found "Accumulation in the standing dead was 63% of inputs, in the litter 8%, and belowground 37%.". Lauenroth, W.K. & Whitman, W.C. (1977) Dynamics of dry matter production in a mixed-grass prairie in western North Dakota. Oecologia, 27, 339-351.
						Shrub_ANPPintercept <- (StandardShrub_VegComposition[1]*colmax["Sh.Amount.Live"] + StandardShrub_VegComposition[2]*colmax["C3.Amount.Live"] + StandardShrub_VegComposition[3]*colmax["C4.Amount.Live"]) - Shrub_ANPP(StandardShrub_MAP_mm)	#Default input for shrubs (IM_USC00107648_Reynolds; 70% shrubs, 30% C3): biomass was estimated at MAP = 450 mm/yr
						Grasses_ANPPintercept <- (StandardGrasses_VegComposition[1]*colmax["Sh.Amount.Live"] + StandardGrasses_VegComposition[2]*colmax["C3.Amount.Live"] + StandardGrasses_VegComposition[3]*colmax["C4.Amount.Live"]) - Grass_ANPP(StandardGrasses_MAP_mm)		#Default input for sgs-grassland (GP_SGSLTER; 12% shrubs, 22% C3, and 66% C4): biomass was estimated at MAP = 340 mm/yr
						
						#Get scaling values for scaled biomass; guarantee that > minimum.totalBiomass
						minimum.totalBiomass <- 0 #This is a SoilWat parameter
						Shrub_BiomassScaler <- max(minimum.totalBiomass, Shrub_ANPP(ShrubsMAP_mm) + Shrub_ANPPintercept)
						Grass_BiomassScaler <- max(minimum.totalBiomass, Grass_ANPP(GrassMAP_mm) + Grasses_ANPPintercept)
						
						#Scale live biomass amount by productivity; assumption: ANPP = peak standing live biomass
						shrubs_Composition$Sh.Amount.Live <<- shrubs_Composition$Sh.Amount.Live * Shrub_BiomassScaler					
						C3_Composition$C3.Amount.Live <<- C3_Composition$C3.Amount.Live * Grass_BiomassScaler					
						C4_Composition$C4.Amount.Live <<- C4_Composition$C4.Amount.Live * Grass_BiomassScaler					
						AnnGrass_Composition$Annual.Amount.Live <<- AnnGrass_Composition$Annual.Amount.Live * Grass_BiomassScaler					

						#Scale litter amount by productivity and adjust for ratio of litter/live
						shrubs_Composition$Sh.Litter <<- shrubs_Composition$Sh.Litter * Shrub_BiomassScaler * colmax["Sh.Litter"] / colmax["Sh.Amount.Live"]	
						C3_Composition$C3.Litter <<- C3_Composition$C3.Litter * Grass_BiomassScaler * colmax["C3.Litter"] / colmax["C3.Amount.Live"]	
						C4_Composition$C4.Litter <<- C4_Composition$C4.Litter * Grass_BiomassScaler * colmax["C4.Litter"] / colmax["C4.Amount.Live"]	
						AnnGrass_Composition$Annual.Litter <<- AnnGrass_Composition$Annual.Litter * Grass_BiomassScaler * colmax["Annual.Litter"] / colmax["Annual.Amount.Live"]	
						
						#Guarantee that live fraction = ]0, 1]
						shrubs_Composition$Sh.Perc.Live <<- pmin(1, pmax(sqrt(.Machine$double.eps), shrubs_Composition$Sh.Perc.Live))
						C3_Composition$C3.Perc.Live <<- pmin(1, pmax(sqrt(.Machine$double.eps), C3_Composition$C3.Perc.Live))
						C4_Composition$C4.Perc.Live <<- pmin(1, pmax(sqrt(.Machine$double.eps), C4_Composition$C4.Perc.Live))
						AnnGrass_Composition$Annual.Perc.Live <<- pmin(1, pmax(sqrt(.Machine$double.eps), AnnGrass_Composition$Annual.Perc.Live))
						
						#Calculate total biomass based on scaled live biomass amount
						shrubs_Composition$Sh.Biomass <<- shrubs_Composition$Sh.Amount.Live / shrubs_Composition$Sh.Perc.Live
						C3_Composition$C3.Biomass <<- C3_Composition$C3.Amount.Live / C3_Composition$C3.Perc.Live
						C4_Composition$C4.Biomass <<- C4_Composition$C4.Amount.Live / C4_Composition$C4.Perc.Live
						AnnGrass_Composition$Annual.Biomass <<- AnnGrass_Composition$Annual.Amount.Live / AnnGrass_Composition$Annual.Perc.Live
					}

					#adjust phenology for mean monthly temperatures
					if(any(create_treatments == "AdjMonthlyBioMass_Temperature") && i_sw_input_treatments$AdjMonthlyBioMass_Temperature) {
						growing.season <- monthly.temp >= growing.season.threshold.tempC
						
						if(i_SWRunInformation$Y_WGS84 < 0) growing.season <- c(growing.season[7:12], growing.season[1:6]) #Standard growing season needs to be adjusted for southern Hemi
						
						predict.season <- function(biomass_Standard, std.season.padded, std.season.seq, site.season.seq){
							sapply(apply(biomass_Standard, MARGIN=2, function(x) {lf<-loess(x[std.season.padded] ~ std.season.seq, span=0.4); predict(lf, newdata=data.frame(std.season.seq=site.season.seq) ) }), FUN=function(x) max(0, x)) # guarantee that > 0
						}
						
						#Adjust for timing and duration of non-growing season
						if(sum(!growing.season) > 0) {
							if(sum(!growing.season) < 12) {
								std.winter.padded <- (c(std.winter[1] - 1, std.winter, std.winter[length(std.winter)] + 1) - 1) %% 12 + 1
								std.winter.seq <- 0:(length(std.winter.padded) - 1)
								site.winter.seq <- seq(from=1, to=length(std.winter), length=sum(!growing.season))
								site.winter.start <- (temp3 <- (temp2 <- cumsum(c(0, (rtemp <- rle(!growing.season))$lengths))+1)[-length(temp2)][rtemp$values])[length(temp3)] #Calculate first month of winter
								site.winter.months <- (site.winter.start + 1:sum(!growing.season) - 2) %% 12 + 1
								
								shrubs_Composition[site.winter.months,] <- predict.season(shrubs_Standard, std.winter.padded, std.winter.seq, site.winter.seq)
								C3_Composition[site.winter.months,] <- predict.season(C3_Standard, std.winter.padded, std.winter.seq, site.winter.seq)
								C4_Composition[site.winter.months,] <- predict.season(C4_Standard, std.winter.padded, std.winter.seq, site.winter.seq)
								AnnGrass_Composition[site.winter.months,] <- predict.season(AnnGrass_Standard, std.winter.padded, std.winter.seq, site.winter.seq)
							
							} else { #if winter lasts 12 months
								#Take the mean of the winter months
								shrubs_Composition[] <- matrix(mean(shrubs_Standard[std.winter,]), nrow=12, ncol=ncol(shrubs_Composition), byrow=TRUE)
								C3_Composition[] <- matrix(mean(C3_Standard[std.winter,]), nrow=12, ncol=ncol(C3_Composition), byrow=TRUE)
								C4_Composition[] <- matrix(mean(C4_Standard[std.winter,]), nrow=12, ncol=ncol(C4_Composition), byrow=TRUE)
								AnnGrass_Composition[] <- matrix(mean(AnnGrass_Standard[std.winter,]), nrow=12, ncol=ncol(AnnGrass_Composition), byrow=TRUE)
							}
						}
						
						#Adjust for timing and duration of growing season
						if(sum(growing.season)>0) {
							if(sum(growing.season) < 12) {
								std.growing.padded <- (c(std.growing[1] - 1, std.growing, std.growing[length(std.growing)] + 1) - 1) %% 12 + 1
								std.growing.seq <- 0:(length(std.growing.padded) - 1)
								site.growing.seq <- seq(from=1, to=length(std.growing), length=sum(growing.season))
								site.growing.start <- (temp3 <- (temp2 <- cumsum(c(0, (rtemp <- rle(growing.season))$lengths))+1)[-length(temp2)][rtemp$values])[1] #Calculate first month of growing season
								site.growing.months <- (site.growing.start + 1:sum(growing.season) - 2) %% 12 + 1
								
								shrubs_Composition[site.growing.months,] <- predict.season(shrubs_Standard, std.growing.padded, std.growing.seq, site.growing.seq)
								C3_Composition[site.growing.months,] <- predict.season(C3_Standard, std.growing.padded, std.growing.seq, site.growing.seq)
								C4_Composition[site.growing.months,] <- predict.season(C4_Standard, std.growing.padded, std.growing.seq, site.growing.seq)
								AnnGrass_Composition[site.growing.months,] <- predict.season(AnnGrass_Standard, std.growing.padded, std.growing.seq, site.growing.seq)
								
							} else { #if growing season lasts 12 months
								shrubs_Composition[] <- matrix(apply(shrubs_Standard[3:9,], MARGIN=2, FUN=max), nrow=12, ncol=ncol(shrubs_Composition), byrow=TRUE)
								C3_Composition[] <- matrix(apply(C3_Standard[3:9,], MARGIN=2, FUN=max), nrow=12, ncol=ncol(C3_Composition), byrow=TRUE)
								C4_Composition[] <- matrix(apply(C4_Standard[3:9,], MARGIN=2, FUN=max), nrow=12, ncol=ncol(C4_Composition), byrow=TRUE)
								AnnGrass_Composition[] <- matrix(apply(AnnGrass_Standard[3:9,], MARGIN=2, FUN=max), nrow=12, ncol=ncol(AnnGrass_Composition), byrow=TRUE)
							}
						}
						if(i_SWRunInformation$Y_WGS84 < 0) { #Adjustements were done as if on nothern hemisphere
							shrubs_Composition <- rbind(shrubs_Composition[7:12,], shrubs_Composition[1:6,])
							C3_Composition <- rbind(C3_Composition[7:12,], C3_Composition[1:6,])
							C4_Composition <- rbind(C4_Composition[7:12,], C4_Composition[1:6,])
							AnnGrass_Composition <- rbind(AnnGrass_Composition[7:12,], AnnGrass_Composition[1:6,])
						}
						if(!(any(create_treatments == "AdjMonthlyBioMass_Precipitation") & i_sw_input_treatments$AdjMonthlyBioMass_Precipitation)){
							adjCompPPT(ShrubsMAP_mm=StandardShrub_MAP_mm, GrassMAP_mm=StandardGrasses_MAP_mm)
						}
					}
					
					#Adjust biomass amounts by productivity relationship with MAP
					if(any(create_treatments == "AdjMonthlyBioMass_Precipitation") & i_sw_input_treatments$AdjMonthlyBioMass_Precipitation) {
						adjCompPPT(ShrubsMAP_mm=MAP_mm, GrassMAP_mm=MAP_mm)
					}
					
					#Write to prodin
					infilename <- file.path(dir.sw.runs.sc.in[sc], prodin)
					infiletext <- readLines(con = infilename)
					
					put.toProdinWithLaiconv <- function(biomass_Composition, startline, infiletext){
						for (m in st_mo) {
							vec <- (unlist(strsplit(infiletext[startline + m], split="[[:space:]]")))
							laiconv  <- as.numeric(vec[grep("[[:digit:]]", vec)])[4] #Get laiconv from file
							infiletext[startline + m] <- paste(paste(	format(biomass_Composition[m, grepl("Litter", names(biomass_Composition))], digits=1, nsmall=1),
																		format(biomass_Composition[m, grepl("Biomass", names(biomass_Composition))], digits=1, nsmall=1),
																		format(biomass_Composition[m, grepl("Perc.Live", names(biomass_Composition))], digits=1, nsmall=3),
																		format(laiconv, digits=1, nsmall=0), sep="\t"), "\t# ", month.name[m], sep="")
						}
						return(infiletext)
					}
					
					Grass_Composition <- C3_Composition*grass.c3.fractionG + C4_Composition*grass.c4.fractionG + AnnGrass_Composition*grass.Annual.fractionG	#Assumed grass.c3.fractionG, grass.c4.fractionG, grass.Annual.fractionG are set
					infiletext <- put.toProdinWithLaiconv(biomass_Composition=Grass_Composition, startline=85, infiletext)
					infiletext <- put.toProdinWithLaiconv(biomass_Composition=shrubs_Composition, startline=100, infiletext)

					infile <- file(infilename, "w+b")
					writeLines(text = infiletext, con = infile, sep = "\n")
					close(infile)
				}
				
				#adjust Root Profile - need composition fractions set above
				if(any(create_treatments == "AdjRootProfile") && i_sw_input_treatments$AdjRootProfile && any(create_treatments == "PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996") && i_sw_input_treatments$PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996) {
					
					trco_type_C3 <- ifelse(any(create_treatments == "RootProfile_C3") && any(colnames(tr_input_TranspCoeff) == i_sw_input_treatments$RootProfile_C3), i_sw_input_treatments$RootProfile_C3, "SchenkJackson2003_PCdry_grasses")
					trco_type_C4 <- ifelse(any(create_treatments == "RootProfile_C4") && any(colnames(tr_input_TranspCoeff) == i_sw_input_treatments$RootProfile_C4), i_sw_input_treatments$RootProfile_C4, "SchenkJackson2003_PCdry_grasses")
					trco_type_annuals <- ifelse(any(create_treatments == "RootProfile_Annuals") && any(colnames(tr_input_TranspCoeff) == i_sw_input_treatments$RootProfile_Annuals), i_sw_input_treatments$RootProfile_Annuals, "Jacksonetal1996_crops")
					trco_type_shrubs <- ifelse(any(create_treatments == "RootProfile_Shrubs") && any(colnames(tr_input_TranspCoeff) == i_sw_input_treatments$RootProfile_Shrubs), i_sw_input_treatments$RootProfile_Shrubs, "SchenkJackson2003_PCdry_shrubs")
					tro_type_tree <- ifelse(any(create_treatments == "LookupTranspCoeffFromTable_Tree") && is.finite(i_sw_input_treatments$LookupTranspCoeffFromTable_Tree) && any(colnames(tr_input_TranspCoeff) == i_sw_input_treatments$LookupTranspCoeffFromTable_Tree), i_sw_input_treatments$LookupTranspCoeffFromTable_Tree, "FILL")
					
					if(grass.fraction==0) { #if grass.fraction is 0 then Grass.trco will be 0
						Grass.trco <- TranspCoeffByVegType(soillayer_no=d, trco_type="FILL", layers_depth=layers_depth)
					} else {
						C3.trco <- TranspCoeffByVegType(soillayer_no=d, trco_type=trco_type_C3, layers_depth=layers_depth)
						C4.trco <- TranspCoeffByVegType(soillayer_no=d, trco_type=trco_type_C4, layers_depth=layers_depth)
						Annuals.trco <- TranspCoeffByVegType(soillayer_no=d, trco_type=trco_type_annuals, layers_depth=layers_depth)
						Grass.trco <- C3.trco * grass.c3.fractionG + C4.trco * grass.c4.fractionG + Annuals.trco * grass.Annual.fractionG
					}
					
					Shrub.trco <- TranspCoeffByVegType(soillayer_no=d, trco_type=trco_type_shrubs, layers_depth=layers_depth)
					Tree.trco <- TranspCoeffByVegType(soillayer_no=d, trco_type=tro_type_tree, layers_depth=layers_depth)
					
					#write to table
					infilename <- file.path(dir.sw.runs.sc.in[sc], soilsin)
					infiletext <- readLines(con = infilename)
					if(is.na(Grass.trco)) Grass.trco <- rep(0, soilsin.firstDataLine:length(infiletext))
					#get the values and sub new grass trco values in
					tempdat <- matrix(data=0, nrow=SoilLayer_MaxNo, ncol=12)
					colnames(tempdat) <- c("depth", "bulkd",   "fieldc",   "wiltpt",  "evco",  "trco_grass",	"trco_shrub",  "trco_tree", "Perc.sand",  "Perc.clay", "imperm", "soiltemp")
					for (l in soilsin.firstDataLine:(length(infiletext))){
						tempdat[(l+1)-soilsin.firstDataLine,] <- na.exclude(as.numeric((unlist(strsplit(infiletext[l], split="[[:space:]]")))))
						tempdat[(l+1)-soilsin.firstDataLine,6] <- Grass.trco[(l+1)-soilsin.firstDataLine]
						tempdat[(l+1)-soilsin.firstDataLine,7] <- Shrub.trco[(l+1)-soilsin.firstDataLine]
						tempdat[(l+1)-soilsin.firstDataLine,8] <- Tree.trco[(l+1)-soilsin.firstDataLine]
						infiletext[l] <- paste(formatC(tempdat[l-soilsin.firstDataLine+1,1], digits=0, format="f"), formatC(tempdat[l-soilsin.firstDataLine+1,2], digits=2, format="f"), formatC(tempdat[l-soilsin.firstDataLine+1,3], digits=4, format="f"), formatC(tempdat[l-soilsin.firstDataLine+1,4], digits=4, format="f"), formatC(tempdat[l-soilsin.firstDataLine+1,5], digits=4, format="f"), formatC(tempdat[l-soilsin.firstDataLine+1,6], digits=4, format="f"), formatC(tempdat[l-soilsin.firstDataLine+1,7], digits=4, format="f"), formatC(tempdat[l-soilsin.firstDataLine+1,8], digits=4, format="f"), formatC(tempdat[l-soilsin.firstDataLine+1,9], digits=4, format="f"), formatC(tempdat[l-soilsin.firstDataLine+1,10], digits=4, format="f"), formatC(tempdat[l-soilsin.firstDataLine+1,11], digits=4, format="f"), formatC(tempdat[l-soilsin.firstDataLine+1,12], digits=4, format="f"), sep="\t")
					}
					#write results back out
					infile <- file(infilename, "w+b")
					writeLines(text = infiletext, con = infile, sep = "\n")
					close(infile)				
				}
				
				if(any(create_treatments %in% c("Vegetation_TotalBiomass_ScalingFactor", "Vegetation_LiveBiomass_ScalingFactor", "Vegetation_Litter_ScalingFactor"))){
					finite01 <- function(x) {x[x < 0 | is.na(x)] <- 0; x[x > 1] <- 1; return(x)}
					
					infilename <- file.path(dir.sw.runs.sc.in[sc], prodin)
					infiletext <- readLines(con = infilename)
					LitterTotalLiveScalingFactors <- rep(1, 3)
					
					if(any(create_treatments == "Vegetation_Litter_ScalingFactor") && is.finite(i_sw_input_treatments$Vegetation_Litter_ScalingFactor))
						LitterTotalLiveScalingFactors[1] <- i_sw_input_treatments$Vegetation_Litter_ScalingFactor
					if(any(create_treatments == "Vegetation_TotalBiomass_ScalingFactor") && is.finite(i_sw_input_treatments$Vegetation_TotalBiomass_ScalingFactor))
						LitterTotalLiveScalingFactors[2] <- i_sw_input_treatments$Vegetation_TotalBiomass_ScalingFactor
					if(any(create_treatments == "Vegetation_LiveBiomass_ScalingFactor") && is.finite(i_sw_input_treatments$Vegetation_LiveBiomass_ScalingFactor))
						LitterTotalLiveScalingFactors[3] <- i_sw_input_treatments$Vegetation_LiveBiomass_ScalingFactor
					
					adjustTables <- function(bscale, ScalingSeason, startline, infiletext){
						#collect data and put into a table
						if(is.na(ScalingSeason) || !any(c("All", "Growing", "Nongrowing") == ScalingSeason)) #set to All for default
							ScalingSeason <- "All"
						tempdat <- matrix(data=NA, nrow=12, ncol=4)
						colnames(tempdat) <- c("litt", "biom", "live", "laiconv")
						for(m in st_mo){
							vec <- (unlist(strsplit(infiletext[startline + m], split="[[:space:]]")))
							tempdat[m, ]  <- as.numeric(vec[grep("[[:digit:]]", vec)])
						}
						if(any(create_treatments == "Vegetation_Biomass_ScalingSeason_AllGrowingORNongrowing")) {
							if(ScalingSeason == "Growing") { #Growing: apply 'Vegetation_Biomass_ScalingFactor' only to those months that have MAT > growing.season.threshold.tempC
								ifelse((templength<-length((temp<-SiteClimate_Scenario$meanMonthlyTempC>growing.season.threshold.tempC)[temp==TRUE]))>1, tempdat[temp, 1:3] <- sweep(tempdat[temp, 1:3], MARGIN=2, FUN="*", LitterTotalLiveScalingFactors), ifelse(templength==1, tempdat[temp,1:3]<-tempdat[temp,1:3]*LitterTotalLiveScalingFactors, print("To Cold to do Vegetation Scaling Season for Growing")))
								#tempdat[SiteClimate_Scenario$meanMonthlyTempC>growing.season.threshold.tempC, 1:3] <- sweep(tempdat[SiteClimate_Scenario$meanMonthlyTempC>growing.season.threshold.tempC, 1:3], MARGIN=2, FUN="*", LitterTotalLiveScalingFactors)
							} else if(ScalingSeason == "Nongrowing") {# Nongrowing: apply 'Vegetation_Biomass_ScalingFactor' only to those months that have MAT <= growing.season.threshold.tempC
								ifelse((templength<-length((temp<-SiteClimate_Scenario$meanMonthlyTempC<=growing.season.threshold.tempC)[temp==TRUE]))>1, tempdat[temp, 1:3] <- sweep(tempdat[temp, 1:3], MARGIN=2, FUN="*", LitterTotalLiveScalingFactors), ifelse(templength==1, tempdat[temp,1:3]<-tempdat[temp,1:3]*LitterTotalLiveScalingFactors, print("To Hot to do Vegetation Scaling Season for NonGrowing")))
								#tempdat[SiteClimate_Scenario$meanMonthlyTempC<=growing.season.threshold.tempC, 1:3] <- sweep(tempdat[SiteClimate_Scenario$meanMonthlyTempC<=growing.season.threshold.tempC, 1:3], MARGIN=2,  FUN="*", LitterTotalLiveScalingFactors)
							}
						} else {
							tempdat[, 1:3] <- sweep(tempdat[, 1:3], MARGIN=2, FUN="*", LitterTotalLiveScalingFactors)
						}
						tempdat[, 3] <- finite01(tempdat[, 3])  #Check that live biomass fraction <= 1 & >= 0
						for(m in st_mo){
							infiletext[startline + m] <- paste(paste(format(tempdat[m, 1], nsmall=1), format(tempdat[m, 2], nsmall=1), format(tempdat[m, 3], nsmall=2), format(tempdat[m, 4], nsmall=0), sep="\t"), "\t# ", month.name[m], sep="")
						}
						return(infiletext)
					}
					infiletext <- adjustTables(bscale=LitterTotalLiveScalingFactors, ScalingSeason=i_sw_input_treatments$Vegetation_Biomass_ScalingSeason_AllGrowingORNongrowing, startline=85, infiletext)
					infiletext <- adjustTables(bscale=LitterTotalLiveScalingFactors, ScalingSeason=i_sw_input_treatments$Vegetation_Biomass_ScalingSeason_AllGrowingORNongrowing, startline=100, infiletext)
					infiletext <- adjustTables(bscale=LitterTotalLiveScalingFactors, ScalingSeason=i_sw_input_treatments$Vegetation_Biomass_ScalingSeason_AllGrowingORNongrowing, startline=115, infiletext)
					
					#write results back out
					infile <- file(infilename, "w+b")
					writeLines(text = infiletext, con = infile, sep = "\n")
					close(infile)
				}			
				
				if(any(create_treatments == "Vegetation_Height_ScalingFactor")) {
					infilename <- file.path(dir.sw.runs.sc.in[sc], prodin)
					infiletext <- readLines(con = infilename)
					
					#collect data and put into a table
					tempdat <- matrix(data=NA, nrow=5, ncol=3)
					colnames(tempdat) <- c("Grasses", "Shrubs", "Trees")
					rownames(tempdat) <- c("xinflec", "yinflec", "range", "slope", "constantHeight")
					comments <- vector("character", length=5)
					for(m in 1:5){
						vec <- (unlist(strsplit(infiletext[20 + m], split="[[:space:]]")))
						comments[m] <- paste(vec[which(grepl("#", vec)):length(vec)], collapse=" ")
						tempdat[m, ]  <- as.numeric(vec[grep("[[:digit:]]", vec)])[1:3]
					}
					
					#scale constant height
					tempdat[5, ] <- pmax(0, tempdat[5, ] * i_sw_input_treatments$Vegetation_Height_ScalingFactor)
					
					#scale tanfunc parameters: scale yinflec and range, leave xinflec and slope as is
					tempdat[2:3, ] <- pmax(0, tempdat[2:3, ] * i_sw_input_treatments$Vegetation_Height_ScalingFactor)
					
					#write results back out
					line.digits <- c(0, 1, 0, 5, 0)
					for(m in 1:5){
						infiletext[20 + m] <- paste(paste(formatC(tempdat[m, 1], format="f", digits=line.digits[m]), formatC(tempdat[m, 2], format="f", digits=line.digits[m]), formatC(tempdat[m, 3], format="f", digits=line.digits[m]), sep="\t"), "\t", comments[m], sep="")
					}
					
					infile <- file(infilename, "w+b")
					writeLines(text = infiletext, con = infile, sep = "\n")
					close(infile)				
				}
				
				#if southern hemisphere adjust if set, but not when already adjusted by, e.g., growing season
				if(accountNSHemispheres_veg && i_SWRunInformation$Y_WGS84 < 0 && !any(create_treatments == "AdjMonthlyBioMass_Temperature")){
					infilename <- file.path(dir.sw.runs.sc.in[sc], prodin)
					infiletext <- readLines(con = infilename)
					
					adjustTables <- function(startline, infiletext){
						#collect data and put into a table
						tempdat <- matrix(data=NA, nrow=12, ncol=4)
						colnames(tempdat) <- c("litt", "biom", "live", "laiconv")
						for (m in st_mo){
							vec <- (unlist(strsplit(infiletext[startline + m], split="[[:space:]]")))
							tempdat[m, ]  <- as.numeric(vec[grep("[[:digit:]]", vec)])
						}
						tempdat <- rbind(tempdat[7:12,], tempdat[1:6,])
						for(m in st_mo){
							infiletext[startline + m] <- paste(paste(format(tempdat[m, 1], nsmall=1), format(tempdat[m, 2], nsmall=1), format(tempdat[m, 3], nsmall=2), format(tempdat[m, 4], nsmall=0), sep="\t"), "\t# ", month.name[m], sep="")
						}
						return(infiletext)
					}
					infiletext <- adjustTables(85, infiletext)
					infiletext <- adjustTables(100, infiletext)
					infiletext <- adjustTables(115, infiletext)
					
					infile <- file(infilename, "w+b")
					writeLines(text = infiletext, con = infile, sep = "\n")
					close(infile)
				}
				
				
				#--control transpiration regions for adjusted soil depth and rooting depth
				#get transpiration regions from sw-input file
				infilename <- file.path(dir.sw.runs.sc.in[sc], siteparamin)
				infiletext <- readLines(con = infilename)
				
				tri.file <- matrix(NA, nrow=4, ncol=2, dimnames=list(NULL, c("Used_TF", "DeepestLayer")))
				for(tri in 1:4){ 
					tri.file[tri, 2] <- na.exclude(as.numeric(temp <- unlist(strsplit(infiletext[70 + tri], split="[[:space:]]"))))[2]
					tri.file[tri, 1] <- !identical(temp[nchar(temp) > 0][1], "#")
				}
				
				#get soil depth
				max.tri.soil <- length(layers_depth)
				
				#get rooting depth
				soiltext <- readLines(con = file.path(dir.sw.runs.sc.in[sc], soilsin))
				vec <- matrix(data=0, nrow=length(soiltext)-soilsin.firstDataLine+1, ncol=12)
				colnames(vec) <- c("depth", "bulkd",   "fieldc",   "wiltpt",  "evco",  "trco_grass",	"trco_shrub",  "trco_tree", "Perc.sand",  "Perc.clay", "imperm", "soiltemp")
				for (l in soilsin.firstDataLine:(length(soiltext))) vec[(l+1)-soilsin.firstDataLine,] <- na.exclude(as.numeric((unlist(strsplit(soiltext[l], split="[[:space:]]")))))
				max.tri.root <- min(apply(vec[, grepl("trco", colnames(vec))], MARGIN=2, FUN=function(x) sum(x > 0)))
				
				#adjust maximum transpiration region for minimum soil depth and rooting depth
				if(max(tri.file[tri.file[, 1] > 0, 2], na.rm=TRUE) > (max.tri <- min(max.tri.soil, max.tri.root))){ 
					for(tri in 4:1) if(tri.file[tri, 1] > 0){
						if(tri.file[tri, 2] > max.tri) tri.file[tri, 2] <- max.tri
						if(tri > 1 && tri.file[tri, 2] <= tri.file[tri-1, 2]) tri.file[tri, 1] <- 0
					}
				}

				#write adjusted transpiration region values back to file
				for(tri in 1:4) infiletext[70+tri] <- paste(ifelse(tri.file[tri, 1] > 0, "\t", "# "), tri, "\t\t", tri.file[tri, 2] , sep="")
				infile <- file(infilename, "w+b")
				writeLines(text = infiletext, con = infile, sep = "\n")
				close(infile)
				
			}#end do scenario creations
			
		}#end if do create runs
		
		if(makeInputForExperimentalDesign && trowExperimentals > 0 && length(create_experimentals) > 0) {
			#This file will be used to remake the input files for experimentals
			if(makeOutputDB) {
				mpi.send.Robj(list(	SWRunInformation=c(paste(i_labels, paste(i_SWRunInformation[-1], collapse = "\t"), sep="\t")),
								sw_input_soillayers=c(paste(i_labels, paste(i_sw_input_soillayers[-1], collapse = "\t"), sep="\t")),
								sw_input_treatments=c(paste(i_labels, paste(i_sw_input_treatments[-1], collapse = "\t"), sep="\t")),
								sw_input_cloud=c(paste(i_labels, paste(i_sw_input_cloud[-1], collapse = "\t"), sep="\t")),
								sw_input_prod=c(paste(i_labels, paste(i_sw_input_prod[-1], collapse = "\t"), sep="\t")),
								sw_input_site=c(paste(i_labels, paste(i_sw_input_site[-1], collapse = "\t"), sep="\t")),
								sw_input_soils=c(paste(i_labels, paste(i_sw_input_soils[-1], collapse = "\t"), sep="\t")),
								sw_input_weather=c(paste(i_labels, paste(i_sw_input_weather[-1], collapse = "\t"), sep="\t")),
								sw_input_climscen=c(paste(i_labels, paste(i_sw_input_climscen[-1], collapse = "\t"), sep="\t")),
								sw_input_climscen_values=c(paste(i_labels, paste(i_sw_input_climscen_values[-1], collapse = "\t"), sep="\t")) ), 1, 4, 1)
			} else {
				infiletext <- c(paste(i_labels, paste(i_SWRunInformation[-1], collapse = ExpInput_Seperator), sep=ExpInput_Seperator))
				infiletext <- c(infiletext, paste(i_labels, paste(i_sw_input_soillayers[-1], collapse = ExpInput_Seperator), sep=ExpInput_Seperator))
				infiletext <- c(infiletext, paste(i_labels, paste(i_sw_input_treatments[-1], collapse = ExpInput_Seperator), sep=ExpInput_Seperator))
				infiletext <- c(infiletext, paste(i_labels, paste(i_sw_input_cloud[-1], collapse = ExpInput_Seperator), sep=ExpInput_Seperator))
				infiletext <- c(infiletext, paste(i_labels, paste(i_sw_input_prod[-1], collapse = ExpInput_Seperator), sep=ExpInput_Seperator))
				infiletext <- c(infiletext, paste(i_labels, paste(i_sw_input_site[-1], collapse = ExpInput_Seperator), sep=ExpInput_Seperator))
				infiletext <- c(infiletext, paste(i_labels, paste(i_sw_input_soils[-1], collapse = ExpInput_Seperator), sep=ExpInput_Seperator))
				infiletext <- c(infiletext, paste(i_labels, paste(i_sw_input_weather[-1], collapse = ExpInput_Seperator), sep=ExpInput_Seperator))
				infiletext <- c(infiletext, paste(i_labels, paste(i_sw_input_climscen[-1], collapse = ExpInput_Seperator), sep=ExpInput_Seperator))
				infiletext <- c(infiletext, paste(i_labels, paste(i_sw_input_climscen_values[-1], collapse = ExpInput_Seperator), sep=ExpInput_Seperator))
				
				infilename <- paste(dir.out.temp, .Platform$file.sep, flag.icounter, "_", "Experimental_InputData_All.csv", sep="")
				infile <- file(infilename, "w+b")
				writeLines(text = infiletext, con = infile, sep = "\n")
				close(infile)
			}	
		}
		
#------------------------EXECUTE SOILWAT
		if( todo$execute ){
			if(any(create_treatments == "Exclude_ClimateAmbient") && i_sw_input_treatments$Exclude_ClimateAmbient) {
				Exclude_ClimateAmbient <- 2
			} else {
				Exclude_ClimateAmbient <- 1
			}
			
			for (sc in Exclude_ClimateAmbient:scenario_No){
				
				setwd(dir.sw.runs.sc[sc])
				if(!exists("use_janus")){
					try(system(paste("./", shQuote(sw), " -f ", filesin, " -e -q", sep="")))
				} else {
					try(system(paste(exec_c_prefix, "./", shQuote(sw), " -f ", filesin, " -e -q", sep="")))
				}
				
				if(sw.outputs == ""){
					outputfiles <- try((ll <- list.files(dir.sw.runs.sc[sc]))[grepl(pattern=c(".dy", ".wk", ".mo", ".yr")[r], x=ll, fixed=TRUE)])
					dir.create2(dir.sw.runs.sc.out[sc])
					try(file.rename(from=file.path(dir.sw.runs.sc[sc], outputfiles), to=file.path(dir.sw.runs.sc.out[sc], outputfiles)))
					sw.outputs.moved <- TRUE
				}
				
#				for (r in 1:output_timescales_maxNo){ #daily, weekly, monthly, yearly output
#					rt <- switch(EXPR=r, "daily", "weekly", "monthly", "yearly")
#					if(any(simulation_timescales==rt)){
#						#system(paste("chmod a+x", shQuote(sw))) #giving sw executable permission...
#						if(!exists("use_janus")) 
#							try(system(paste("./", shQuote(sw), " ", cmd[r], sep="")))
#						else
#							try(system(paste(exec_c_prefix, "./", shQuote(sw), " ", cmd[r], sep="")))
#
#						if(sw.outputs == ""){
#							outputfiles <- try((ll <- list.files(dir.sw.runs.sc[sc]))[grepl(pattern=c(".dy", ".wk", ".mo", ".yr")[r], x=ll, fixed=TRUE)])
#							dir.create2(dir.sw.runs.sc.out[sc])
#							try(file.rename(from=file.path(dir.sw.runs.sc[sc], outputfiles), to=file.path(dir.sw.runs.sc.out[sc], outputfiles)))
#							sw.outputs.moved <- TRUE
#						}
#					}
#				}
				
			}
		}#end if do execute
		
		
#------------------------AGGREGATE SOILWAT OUTPUT
		if( todo$aggregate ){
			#get soil aggregation layer for daily aggregations
			if(AggLayer.daily){
				aggLs <- setAggSoilLayerForAggDailyResponses(layers_depth)
			} else {
				aggLs <- as.list(ld)
			}
			aggLs_no <- length(aggLs)
			
			#get soil texture data for each layer
			if(exists("soildat") && all(!is.na(soildat[, c("sand", "clay")]))){
				sand <- soildat[ld, "sand"]
				clay <- soildat[ld, "clay"]
			} else {
				sand <- clay <- vector(length=d)
				infilename <- file.path(dir.sw.runs.sc.in[sc], soilsin)
				infiletext <- readLines(con = infilename)
				for (l in soilsin.firstDataLine:(soilsin.firstDataLine+d-1)){
					vec <- na.omit(as.double(unlist(strsplit(infiletext[l], split="[[:space:]]"))))
					sand[l+1-soilsin.firstDataLine] <- vec[9]
					clay[l+1-soilsin.firstDataLine] <- vec[10]
				}
			}
			texture <- list(sand.top=weighted.mean(sand[topL], layers_width[topL]),
					sand.bottom=weighted.mean(sand[bottomL], layers_width[bottomL]),
					clay.top=weighted.mean(clay[topL], layers_width[topL]),
					clay.bottom=weighted.mean(clay[bottomL], layers_width[bottomL]))
			
			if(any(simulation_timescales=="daily") && daily_no > 0){
				textureDAgg <- list(sand=sapply(1:aggLs_no, FUN=function(x) weighted.mean(sand[aggLs[[x]]], layers_width[aggLs[[x]]])),
						clay=sapply(1:aggLs_no, FUN=function(x) weighted.mean(clay[aggLs[[x]]], layers_width[aggLs[[x]]])))
			}
			
			#data access functions
			get_Response_aggL <- function(dir.current, response, tscale=c("dy", "dyAll", "mo", "yr"), scaler=10, FUN, weights=NULL){
				FUN <- match.fun(FUN)
				tscale <- match.arg(tscale, choices=c("dy", "dyAll", "mo", "yr"))
				temp1 <- try(scaler * read.table(file = file.path(dir.current, response), header = FALSE, sep = "", fill = TRUE, comment.char=""), silent=TRUE)
				if(identical(class(temp1), "try-error")) stop("Necessary SoilWat output files are not present for aggregation of results")
				if(tscale == "dy"){
					index.col <- 2
					index.usetimestep <- simTime$index.usedy
					timestep_ForEachEntry <- simTime2$doy_ForEachUsedDay
				} else if(tscale == "dyAll"){
					index.col <- 2
					index.usetimestep <- 1:nrow(temp1)
					timestep_ForEachEntry <- NULL
				} else if(tscale == "mo"){
					index.col <- 2
					index.usetimestep <- simTime$index.usemo
					timestep_ForEachEntry <- simTime2$month_ForEachUsedMonth
				} else if(tscale == "yr"){
					index.col <- 1
					index.usetimestep <- simTime$index.useyr
					timestep_ForEachEntry <- NULL
				}
				layers <- 1:(ncol(temp1) - index.col)
				if(max(layers) <= max(topL)){ #adjust topL and bottomL locally in case temp1 doesn't contain information for every layer, e.g., soil evaporation
					topL <- layers
					bottomL <- 0
				} else if(max(layers) < max(bottomL)){
					bottomL <- min(bottomL):max(layers)
				}					
				if(length(topL) > 1) {
					if(is.null(weights)){
						val.top <- apply(temp1[index.usetimestep, index.col + topL], 1, FUN)
					} else {
						val.top <- apply(temp1[index.usetimestep, index.col + topL], 1, FUN, weights[topL])
					}
				} else {
					val.top <- temp1[index.usetimestep, index.col + topL]
				}
				if(length(bottomL) > 1) {
					if(is.null(weights)){
						val.bottom <- apply(temp1[index.usetimestep, index.col + bottomL], 1, FUN)
					} else {
						val.bottom <- apply(temp1[index.usetimestep, index.col + bottomL], 1, FUN, weights[bottomL])
					}
				} else {
					if(bottomL == 0) {
						val.bottom <- matrix(data=0, nrow=length(index.usetimestep), ncol=1)
					} else {
						val.bottom  <- temp1[index.usetimestep, index.col + bottomL]
					}
				}
				if(!is.null(timestep_ForEachEntry)){
					aggMean.top <- aggregate(val.top, by=list(timestep_ForEachEntry), FUN=mean)[,2]
					aggMean.bottom <- aggregate(val.bottom, by=list(timestep_ForEachEntry), FUN=mean)[,2]
					return(list(top=val.top, bottom=val.bottom, aggMean.top=aggMean.top, aggMean.bottom=aggMean.bottom))
				} else {
					if(tscale == "dyAll"){
						return(list(val=temp1, top=val.top, bottom=val.bottom))
					} else {
						return(list(top=val.top, bottom=val.bottom))
					}
				}
			}
			get_SWP_aggL <- function(vwc){
				val.top <- VWCtoSWP(vwc$top, texture$sand.top, texture$clay.top)
				val.bottom <- VWCtoSWP(vwc$bottom, texture$sand.bottom, texture$clay.bottom)				
				if(!is.null(vwc$aggMean.top)){
					aggMean.top <- VWCtoSWP(vwc$aggMean.top, texture$sand.top, texture$clay.top)
					aggMean.bottom <- VWCtoSWP(vwc$aggMean.bottom, texture$sand.bottom, texture$clay.bottom)
					return(list(top=val.top, bottom=val.bottom, aggMean.top=aggMean.top, aggMean.bottom=aggMean.bottom))
				}
				if(!is.null(vwc$val)){
					if(all(as.integer(vwc$val[,2])==vwc$val[,2])){
						index.header <- 1:2
					} else {
						index.header <- 1
					}
					val <- cbind(vwc$val[, index.header], VWCtoSWP(vwc$val[, -index.header], sand, clay))
					return(list(val=val, top=val.top, bottom=val.bottom))
				} else {
					return(list(top=val.top, bottom=val.bottom))
				}
			}
			get_Temp_yr <- function(dir.current){
				temp1 <- try(read.table(file = file.path(dir.current, tempyr), header = FALSE, sep = "", fill = TRUE, comment.char=""))
				return(list(mean=temp1[simTime$index.useyr, 4]))
			}
			get_Temp_mo <- function(dir.current){
				temp1 <- try(read.table(file = file.path(dir.current, tempmo), header = FALSE, sep = "", fill = TRUE, comment.char=""))
				return(list(min=temp1[simTime$index.usemo, 4], mean=temp1[simTime$index.usemo, 5]))
			}
			get_Temp_dy <- function(dir.current){
				temp1 <- try(read.table(file = file.path(dir.current, tempdy), header = FALSE, sep = "", fill = TRUE, comment.char=""))
				return(list(min=temp1[simTime$index.usedy, 4], mean=temp1[simTime$index.usedy, 5]))
			}
			
			get_PPT_yr <- function(dir.current){
				temp1 <- 10 * read.table(file = file.path(dir.current, precipyr), header = FALSE, sep = "", fill = TRUE, comment.char="")
				ppt <- temp1[simTime$index.useyr, 2]
				rain <- temp1[simTime$index.useyr, 3]
				snowfall <- temp1[simTime$index.useyr, 4]
				snowmelt <- temp1[simTime$index.useyr, 5]
				snowloss <- temp1[simTime$index.useyr, 6]
				return(list(ppt=ppt, rain=rain, snowfall=snowfall, snowmelt=snowmelt, snowloss=snowloss))
			}
			get_PPT_mo <- function(dir.current){
				temp1 <- 10 * read.table(file = file.path(dir.current, precipmo), header = FALSE, sep = "", fill = TRUE, comment.char="")
				return(list(ppt=temp1[simTime$index.usemo, 3], rain=temp1[simTime$index.usemo, 4], snowmelt=temp1[simTime$index.usemo, 6]))
			}
			get_PPT_dy <- function(dir.current){
				temp1 <- 10 * try(read.table(file = file.path(dir.current, precipdy), header = FALSE, sep = "", fill = TRUE, comment.char=""))
				return(list(ppt=temp1[simTime$index.usedy, 3], rain=temp1[simTime$index.usedy, 4]))
			}
			
			get_PET_yr <- function(dir.current){
				temp1 <- 10 * read.table(file = file.path(dir.current, petyr), header = FALSE, sep = "", fill = TRUE, comment.char="")
				return(list(val=temp1[simTime$index.useyr, 2]))
			}
			get_PET_mo <- function(dir.current){
				temp1 <- 10 * read.table(file = file.path(dir.current, petmo), header = FALSE, sep = "", fill = TRUE, comment.char="")
				return(list(val=temp1[simTime$index.usemo, 3]))
			}
			
			get_AET_yr <- function(dir.current){
				temp1 <- 10 * read.table(file = file.path(dir.current, aetyr), header = FALSE, sep = "", fill = TRUE, comment.char="")
				return(list(val=temp1[simTime$index.useyr, 2]))
			}
			get_AET_mo <- function(dir.current){
				temp1 <- 10 * read.table(file = file.path(dir.current, aetmo), header = FALSE, sep = "", fill = TRUE, comment.char="")
				return(list(val=temp1[simTime$index.usemo, 3]))
			}
			
			get_SWE_mo <- function(dir.current){
				temp1 <- 10 * read.table(file = file.path(dir.current, snowmo), header = FALSE, sep = "", fill = TRUE, comment.char="")
				return(list(val=temp1[simTime$index.usemo, 3]))
			}
			get_SWE_dy <- function(dir.current){
				temp1 <- 10 * read.table(file = file.path(dir.current, snowdy), header = FALSE, sep = "", fill = TRUE, comment.char="")
				return(list(val=temp1[simTime$index.usedy, 3]))
			}
			
			get_Inf_yr <- function(dir.current){
				temp1 <- 10 * read.table(file = file.path(dir.current, inf_soilyr), header = FALSE, sep = "", fill = TRUE, comment.char="")
				return(list(inf=temp1[simTime$index.useyr, 2]))
			}
			get_Inf_mo <- function(dir.current){
				temp1 <- 10 * read.table(file = file.path(dir.current, inf_soilmo), header = FALSE, sep = "", fill = TRUE, comment.char="")
				return(list(inf=temp1[simTime$index.usemo, 3]))
			}
			
			get_Esurface_yr <- function(dir.current){
				temp1 <- 10 * read.table(file = file.path(dir.current, evapsurfaceyr), header = FALSE, sep = "", fill = TRUE, comment.char="")
				veg <- apply(temp1[simTime$index.useyr, 3:5], 1, sum)
				return(list(sum=temp1[simTime$index.useyr, 2], veg=veg, litter=temp1[simTime$index.useyr, 6], surfacewater=temp1[simTime$index.useyr, 7]))
			}
			
			get_Interception_yr <- function(dir.current){
				temp1 <- 10 * read.table(file = file.path(dir.current, interceptionyr), header = FALSE, sep = "", fill = TRUE, comment.char="")
				veg <- apply(temp1[simTime$index.useyr, 3:5], 1, sum)
				return(list(sum=temp1[simTime$index.useyr, 2], veg=veg, litter=temp1[simTime$index.useyr, 6]))
			}
			
			get_DeepDrain_yr <- function(dir.current){
				temp1 <- 10 * read.table(file = file.path(dir.current, deepdrainyr), header = FALSE, sep = "", fill = TRUE, comment.char="")
				return(list(val=temp1[simTime$index.useyr, 2]))
			}
			
			get_Runoff_mo <- function(dir.current){
				temp1 <- 10 * read.table(file = file.path(dir.current, runoffmo), header = FALSE, sep = "", fill=TRUE, comment.char="")
				return(list(val=temp1[simTime$index.usemo, 3], ponded=temp1[simTime$index.usemo, 4], snowmelt=temp1[simTime$index.usemo, 5]))
			}
			get_Runoff_yr <- function(dir.current){
				temp1 <- 10 * read.table(file = file.path(dir.current, runoffyr), header = FALSE, sep = "", fill=TRUE, comment.char="")
				return(list(val=temp1[simTime$index.useyr, 3], ponded=temp1[simTime$index.useyr, 4], snowmelt=temp1[simTime$index.useyr, 5]))
			}
			
			#aggregate for each scenario
			for (sc in 1:scenario_No){
				#create header information for output files
				if((i1 <- length(Index_RunInformation)) + (i2 <- length(Index_RunInformation_Treatments)) > 0 ) {
					treatment_header1 <- treatment_header2 <- NULL
					if(i1 > 0){
						treatment_header1 <- i_SWRunInformation[Index_RunInformation]
					}
					if(i2 > 0){
						if(trowExperimentals > 0 && length(create_experimentals) > 0) treatment_header2 <- c(treatment_header2,  sw_input_experimentals[i_exp, 1])
						for(h in Index_RunInformation_Treatments){
							treatment_header2 <- c(treatment_header2, as.character(i_sw_input_treatments[h]))
						}
						if(trowExperimentals > 0 && length(create_experimentals) > 0) names(treatment_header2) <- c("Experimental_Label", colnames(i_sw_input_treatments)[Index_RunInformation_Treatments])
						else names(treatment_header2) <- colnames(i_sw_input_treatments)[Index_RunInformation_Treatments]
						if(any(create_treatments == "YearStart")) if(is.na(treatment_header2["YearStart"])) treatment_header2["YearStart"] <- startyr
						if(any(create_treatments == "YearEnd")) if(is.na(treatment_header2["YearEnd"])) treatment_header2["YearEnd"] <- endyr
						
					}
					treatment_header <- c(treatment_header1, treatment_header2)
					#check to see if the header.names includes YearStart, SimStartYear ,YearEnd
					if(any(names(treatment_header) == "YearStart") && any(names(treatment_header) == "YearEnd")) {#find index of YearStart and Add Sim
						index.startYear <- match("YearStart",names(treatment_header))
						treatment_header <- c(treatment_header[1:index.startYear], "SimStartYear"=simstartyr, treatment_header[(index.startYear+1):length(treatment_header)]);
					} else if(any(names(treatment_header) == "YearStart") && !any(names(treatment_header) == "YearEnd")){
						index.startYear <- match("YearStart",names(treatment_header))
						treatment_header <- c(treatment_header[1:index.startYear], "SimStartYear"=simstartyr, "YearEnd"=endyr, treatment_header[(index.startYear+1):length(treatment_header)]);
					} else if(!any(names(treatment_header) == "YearStart") && any(names(treatment_header) == "YearEnd")){
						index.YearEnd <- match("YearEnd",names(treatment_header))
						treatment_header <- c(treatment_header[1:index.YearEnd-1], "YearStart"=startyr, "SimStartYear"=simstartyr, treatment_header[index.YearEnd:length(treatment_header)]);
					} else {
						treatment_header <- c(treatment_header[1:3], "YearStart"=startyr, "SimStartYear"=simstartyr, "YearEnd"=endyr, treatment_header[4:length(treatment_header)]);
					}
					if(i==ifirst || makeOutputDB) treatment_header.names <- names(treatment_header)
					header <- c(i, as.character(i_labels), treatment_header, scenario[sc])
					if(i==ifirst || makeOutputDB) header.names <-  c("RunID", "Labels", treatment_header.names, "Scenario")	#'Scenario' column has to be the last in the header
				} else {
					if(trowExperimentals > 0 && length(create_experimentals) > 0) {
						header <- c(i, as.character(i_labels),sw_input_experimentals[i_exp,1] ,startyr, simstartyr, endyr, scenario[sc])
						if(i==ifirst || makeOutputDB) header.names <- c("RunID", "Labels", "Experimental Design Label", "YearStart", "SimStartYear", "YearEnd","Scenario")	#'Scenario' column has to be the last in the header
					} else {
						header <- c(i, as.character(i_labels) ,startyr, simstartyr, endyr, scenario[sc])
						if(i==ifirst || makeOutputDB) header.names <- c("RunID", "Labels", "YearStart", "SimStartYear", "YearEnd","Scenario")	#'Scenario' column has to be the last in the header
					}
				}
				names(header) <- NULL
				#convert factor labels to characters
				for(ih in 1:length(header)){
					if(nlevels(header[[ih]]) > 0){
						header[[ih]] <- as.character(header[[ih]])
					}
				}
				
				#only exclude if 1.) Exclude_ClimateAmbient is true in header 2.) That Run is set to Exclude_ClimateAmbient 3.) Our current Scenario is Current
				if(any(create_treatments == "Exclude_ClimateAmbient") && i_sw_input_treatments$Exclude_ClimateAmbient && sc==1) {
					Exclude_ClimateAmbient <- TRUE
					#Send to the database or write to file
					#temporaly save aggregate data
					out.temp <- cbind(header, data.frame(matrix(NA, nrow = 1, ncol = n_variables)))
	
					if(i==ifirst || makeOutputDB){
						colnames(out.temp)[1:length(header)] <- header.names
					}
	
					if(!makeOutputDB) write.csv(out.temp, file=filename.out.temp[sc], quote=FALSE, row.names=FALSE )
					if(makeOutputDB) mpi.send.Robj(out.temp[,1:length(header)], 1, 2, 1) #only send header info when 
					
				} else {
					Exclude_ClimateAmbient <- FALSE
				}
				#overall aggregation. If Exclude_ClimateAmbient == TRUE then skip
				if(!continueAfterAbort | (continueAfterAbort & !isdone.overallAggs[sc]) && !Exclude_ClimateAmbient){
					
					#delete data so that they are read if anew for each scenario; each variable is checked that datafile is read in only once per scenario			
					try(rm(	temp.yr, temp.mo, temp.dy, 
									prcp.yr, prcp.mo, prcp.dy, 
									PET.yr, PET.mo, 
									AET.yr, AET.mo, AET.dy,
									SWE.mo, SWE.dy,
									soiltemp.mo,
									swc.mo, 
									swa.mo, 
									vwc.mo, vwc.dy, vwc.dy.all, 
									swp.mo, swp.dy, swp.dy.all, 
									transp.yr, transp.mo, transp.dy,
									Esoil.yr, evsoil.mo, evsoil.dy,
									hydred.mo, 
									inf.mo,
									runoff.mo, runoff.yr,
									Esurface.yr, evapsurface.dy,
									intercept.yr, 
									inf.yr, 
									deepDrain.yr , deepdrain.dy
							), silent=TRUE)
					
					#result vector column index indicating variable within set of n_variables per scenario
					res <- vector(mode="numeric", length=n_variables)
					res <- data.frame(t(res), stringsAsFactors = FALSE)
					nv <- 1
					
					#Fractional Vegetation Composition
					if(aon$input_FractionVegetationComposition) {
						infilename <- file.path(dir.sw.runs.sc.in[sc], prodin)
						infiletext <- readLines(con = infilename, n=6)
						infiletext <- infiletext[6]
						vec <- (unlist(strsplit(infiletext, split="[[:space:]]")))
						vec<-vec[vec!=""] #remove any empty strings
						vec <- as.numeric(vec)
						res[nv:(nv+2)] <- vec
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+2)] <- c("SWinput_Fraction_Grasses", "SWinput_Fraction_Shrubs", "SWinput_Fraction_Trees")
						nv <- nv+3
					}
					
					#Vegetation Peak
					if(aon$input_VegetationPeak) {
						infilename <- file.path(dir.sw.runs.sc.in[sc], prodin)
						infiletext <- readLines(con = infilename)
						fracs <- na.omit(as.numeric((unlist(strsplit(infiletext[6], split="[[:space:]]"))))) #get the fractional Composition of grasses, shrubs, and trees
						startLines <- c(85, 100, 115)
						tempdat <- matrix(data=NA, nrow=12, ncol=3)#matrix to hold biomass * percLive for grass,shrubs,trees
						colnames(tempdat) <- c("grass", "shrubs", "tree")
						for(j in 1:3) {#get each of the three tables
							for (m in st_mo) {
								vec <- (unlist(strsplit(infiletext[startLines[j] + m], split="[[:space:]]"))) #only get biomass and percLive
								vec <- vec[vec!=""][2:3] #was getting an extra space some how so this is needed
								vec <- as.numeric(vec[grep("[[:digit:]]", vec)]) #store as numeric
								tempdat[m, j]  <- vec[1] * vec[2]
							}
						}
						sumWeightedLiveBiomassByMonth <- apply(sweep(tempdat, MARGIN=2, fracs, FUN="*"), MARGIN=1, function(x) sum(x)) #sweep out fractionals, and sum over rows
						maxMonth <- which(sumWeightedLiveBiomassByMonth==max(sumWeightedLiveBiomassByMonth)) #returns index, which is the month, of max bio
						meanPeakMonth <- circ.mean(maxMonth, 12)
						duration <- circ.range(maxMonth, 12)+1
						res[nv:(nv+1)] <- c(meanPeakMonth, duration) #just in case we get more then one month
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+1)] <- c("SWinput_PeakLiveBiomass_Month_mean","SWinput_PeakLiveBiomass_Month_duration")
						nv <- nv+2
					}
					
					#Climate Perturbations
					if(aon$input_ClimatePerturbations) {
						infilename <- file.path(dir.sw.runs.sc.in[sc], weatherin) #path to weatherin
						infiletext <- readLines(con = infilename) #read all the lines in file
						tempdat <- matrix(data=NA, nrow=12, ncol=3) #matrix to hold the data
						colnames(tempdat) <- c("PrcpMultiplier", "TmaxAddand", "TminAddand")
						for(m in st_mo){
							vec <- (unlist(strsplit(infiletext[16 + m], split="[[:space:]]")))[-1]
							tempdat[m,] <- vec
						}
						res[nv:(nv+35)] <- as.vector(as.numeric(tempdat))
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+35)] <- c(paste("SWinput_ClimatePerturbations_PrcpMultiplier_m",st_mo,sep=""), paste("SWinput_ClimatePerturbations_TmaxAddand_m",st_mo,sep=""), paste("SWinput_ClimatePerturbations_TminAddand_m",st_mo,sep=""))
						nv <- nv+36
					}
					#input Phenology # all growing?
					if(aon$input_Phenology & any(simulation_timescales=="monthly")) {
						if(!exists("temp.mo")) temp.mo <- get_Temp_mo(dir.sw.runs.sc.out[sc]) #see if we have data
						monthly.temp <- aggregate(temp.mo$mean, by=list(simTime2$month_ForEachUsedMonth), FUN=mean)[,2] #get mean monthly temp
						if(i_SWRunInformation$Y_WGS84 < 0) { #check for Southern Hemi
							monthly.temp <- c(monthly.temp[7:12], monthly.temp[1:6]) #rearrange temp
							Months_Above_Threshold <- c(7:12, 1:6)[which(monthly.temp > growing.season.threshold.tempC)] #get months above threshold, then map back to real months.
						} else {
							Months_Above_Threshold <- which(monthly.temp > growing.season.threshold.tempC) #get months above threshold
						}
						Input_PhenologyStart_Month <- Months_Above_Threshold[1] #get the first month
						Input_PhenologyEnd_Month <- tail(Months_Above_Threshold, n=1) #get the last month
						
						res[nv:(nv+1)] <- c(Input_PhenologyStart_Month, Input_PhenologyEnd_Month)
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+1)] <- c("Input_PhenologyStart_Month", "Input_PhenologyEnd_Month")
						nv <- nv+2
					}
					
					#climate & weather
					if(any(simulation_timescales=="yearly") & aon$yearlyTemp){
						if(!exists("temp.yr"))	temp.yr <- get_Temp_yr(dir.sw.runs.sc.out[sc])
						res[nv:(nv+1)] <- c(mean(temp.yr$mean), sd(temp.yr$mean))
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+1)] <- c("MAT_C", "MAT.sd")
						nv <- nv+2
					}
					
					
					if(any(simulation_timescales=="yearly") & aon$yearlyPPT){			
						if(!exists("prcp.yr")) prcp.yr <- get_PPT_yr(dir.sw.runs.sc.out[sc])
						
						res[nv:(nv+1)] <- c(mean(prcp.yr$ppt), sd(prcp.yr$ppt))
						res[(nv+2):(nv+4)] <- quantile(prcp.yr$ppt, probs = c(0.25, 0.5, 0.75))
						res[(nv+5):(nv+6)] <- c(mean(snowofppt <- prcp.yr$snowfall/prcp.yr$ppt, na.rm=TRUE), sd(snowofppt, na.rm=TRUE))
						
						rm(snowofppt)
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+6)] <- c("MAP_mm", "MAP.sd", "MAP.Q25", "MAP.Q50", "MAP.Q75", "SnowofPPT", "SnowofPPT.sd")
						nv <- nv+7
					}
					
					
					if(any(simulation_timescales=="yearly") & aon$yearlyDryWetPeriods){			
						if(!exists("prcp.yr")) prcp.yr <- get_PPT_yr(dir.sw.runs.sc.out[sc])
						
						temp.rle <- rle(sign(prcp.yr$ppt - mean(prcp.yr$ppt)))
						
						res[nv:(nv+1)] <- c(quantile(temp.rle$lengths[temp.rle$values==-1], probs=0.9), quantile(temp.rle$lengths[temp.rle$values==1], probs=0.9))
						
						rm(temp.rle)
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+1)] <- c("Duration90PercentDryYearSpells_yrs", "Duration90PercentWetYearSpells_yrs")
						nv <- nv+2
					}
					
					if(any(simulation_timescales=="yearly") & aon$yearlyPET){
						if(!exists("PET.yr")) PET.yr <- get_PET_yr(dir.sw.runs.sc.out[sc])
						res[nv:(nv+1)] <- c(mean(PET.yr$val), sd(PET.yr$val))
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+1)] <- c("PET_mm", "PET.sd")
						nv <- nv+2
					}
					
					if(any(simulation_timescales=="yearly") & any(simulation_timescales=="monthly") & aon$yearlymonthlyTemperateDrylandIndices){
						if(!exists("prcp.yr")) prcp.yr <- get_PPT_yr(dir.sw.runs.sc.out[sc])
						if(!exists("PET.yr")) PET.yr <- get_PET_yr(dir.sw.runs.sc.out[sc])
						if(!exists("temp.mo")) temp.mo <- get_Temp_mo(dir.sw.runs.sc.out[sc])
						
						get.drylandindices <- function(annualPPT, annualPET, monthlyTemp){
							ai <- annualPPT/annualPET	#Deichmann, U. & L. Eklundh. 1991. Global digital datasets for land degradation studies: a GIS approach. Global Environment Monitoring System (GEMS), United Nations Environment Programme (UNEP), Nairobi, Kenya.
							TD <- ifelse((temp <- apply(matrix(data=monthlyTemp, ncol=12, byrow=TRUE), MARGIN=1, FUN=function(x) sum(x >= 10))) >= 4 & temp < 8, 1, 0) #Trewartha & Horn 1980, page 284: temperate areas
							criteria12 <- ifelse(TD == 1 & ai < 0.5, 1, 0)
							
							return(list(ai=ai, TD=TD, criteria12=criteria12))
						}
						
						di.ts <- get.drylandindices(annualPPT=prcp.yr$ppt, annualPET=PET.yr$val, monthlyTemp=temp.mo$mean)
						di.normals <- get.drylandindices(annualPPT=mean(prcp.yr$ppt), annualPET=mean(PET.yr$val), monthlyTemp=aggregate(temp.mo$mean, by=list(simTime2$month_ForEachUsedMonth), FUN=mean)$x)
						
						res[nv:(nv+8)] <- c(unlist(di.normals), apply(temp <- cbind(di.ts$ai, di.ts$TD, di.ts$criteria12), MARGIN=2, FUN=mean, na.rm=TRUE), apply(temp, MARGIN=2, FUN=sd, na.rm=TRUE))
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+8)] <- c(paste(temp <- c("UNAridityIndex", "TrewarthaD", "TemperateDryland12"), ".normals", sep=""), paste(temp, "_Annual.mean", sep=""), paste(temp, "_Annual.sd", sep=""))
						nv <- nv+9
						
						rm(di.ts, di.normals)
					}
					
					if(any(simulation_timescales=="monthly") & aon$monthlyPlantGrowthControls){	#Nemani RR, Keeling CD, Hashimoto H et al. (2003) Climate-Driven Increases in Global Terrestrial Net Primary Production from 1982 to 1999. Science, 300, 1560-1563.
						if(!exists("temp.mo")) temp.mo <- get_Temp_mo(dir.sw.runs.sc.out[sc])
						if(!exists("PET.mo")) PET.mo <- get_PET_mo(dir.sw.runs.sc.out[sc])
						if(!exists("prcp.mo")) prcp.mo <- get_PPT_mo(dir.sw.runs.sc.out[sc])
						
						DayNumber_ForEachUsedMonth <- rle(simTime2$month_ForEachUsedDay)$lengths
						DayNumber_ForEachUsedYear <- rle(simTime2$year_ForEachUsedDay)$lengths
						
						#temperature control
						control_temp <- aggregate(ifelse(temp.mo$min > 5, 1, ifelse(temp.mo$min < -5, 0, (5 + temp.mo$min) / 10)) * DayNumber_ForEachUsedMonth, by=list(simTime2$yearno_ForEachUsedMonth), FUN=sum)[, 2] / DayNumber_ForEachUsedYear
						
						#moisture control
						aridity <- (prcp.mo$rain + prcp.mo$snowmelt) / PET.mo$val
						control_water <- aggregate(ifelse(aridity > 0.75, 1, ifelse(aridity < 0, 0, aridity/0.75)) * DayNumber_ForEachUsedMonth, by=list(simTime2$yearno_ForEachUsedMonth), FUN=sum)[, 2] / DayNumber_ForEachUsedYear
						
						#radiation control
						infilename <- file.path(dir.sw.runs.sc[sc], filesin)	#in lieu of monthly cloudiness, here use mean monthly sky cover
						infiletext <- readLines(con = infilename)
						dirname.cloud <- unlist(strsplit(infiletext[17], split="[[:space:]]"))[1]
						
						infilename <- file.path(dir.sw.runs.sc[sc], dirname.cloud)
						infiletext <- readLines(con = infilename)
						cloudiness <- as.numeric(unlist(strsplit(infiletext[1], split="[[:space:]]"))[st_mo])
						cloudiness <- rep(cloudiness, times=simTime$no.useyr)
						
						control_radiation <- aggregate((1 - ifelse(cloudiness < 10, 0, (cloudiness - 10) / 100 * 0.5 )) * DayNumber_ForEachUsedMonth, by=list(simTime2$yearno_ForEachUsedMonth), FUN=sum)[, 2] / DayNumber_ForEachUsedYear
						
						temp <- data.frame(control_temp, control_water, control_radiation)
						res[nv:(nv+5)] <- c(apply(temp, 2, mean, na.rm=TRUE), apply(temp, 2, sd, na.rm=TRUE))
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+5)] <- c(temp <- c("Control_Temperature", "Control_Water", "Control_Radiation"), paste(temp, ".sd", sep=""))
						nv <- nv+6
						
						rm(DayNumber_ForEachUsedMonth, DayNumber_ForEachUsedYear, control_temp, control_water, control_radiation, aridity, temp, cloudiness)
					}
					
					if(any(simulation_timescales=="yearly") & aon$yearlyAET){
						if(!exists("AET.yr")) AET.yr <- get_AET_yr(dir.sw.runs.sc.out[sc])
						res[nv:(nv+1)] <- c(mean(AET.yr$val), sd(AET.yr$val))
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+1)] <- c("AET_mm", "AET.sd")
						nv <- nv+2
					}	
					
					if(any(simulation_timescales=="monthly") & aon$monthlyTemp){
						if(!exists("temp.mo")) temp.mo <- get_Temp_mo(dir.sw.runs.sc.out[sc])
						res[nv+st_mo-1] <- aggregate(temp.mo$mean, by=list(simTime2$month_ForEachUsedMonth), FUN=mean)[,2]
						res[nv+st_mo-1+12] <- aggregate(temp.mo$mean, by=list(simTime2$month_ForEachUsedMonth), FUN=sd)[,2]
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+23)] <- c(paste("TempC_m", st_mo, sep=""), paste("TempC_m", st_mo, ".sd", sep=""))
						nv <- nv+24
					}
					
					if(any(simulation_timescales=="monthly") & aon$monthlyPPT){
						if(!exists("prcp.mo")) prcp.mo <- get_PPT_mo(dir.sw.runs.sc.out[sc])
						res[nv+st_mo-1] <- aggregate(prcp.mo$ppt, by=list(simTime2$month_ForEachUsedMonth), FUN=mean)[,2]
						res[nv+st_mo-1+12] <- aggregate(prcp.mo$ppt, by=list(simTime2$month_ForEachUsedMonth), FUN=sd)[,2]
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+23)] <- c(paste("PPT_mm_m", st_mo, sep=""), paste("PPT_mm_m", st_mo, ".sd", sep=""))
						nv <- nv+24
					}
					
					
					
					#daily response to weather generator treatments
					if(any(simulation_timescales=="daily") & aon$dailyWeatherGeneratorCharacteristics){
						if(!exists("prcp.dy")) prcp.dy <- get_PPT_dy(dir.sw.runs.sc.out[sc])
						if(!exists("temp.dy")) temp.dy <- get_Temp_dy(dir.sw.runs.sc.out[sc])
						
						dws <- sapply(st_mo, FUN=function(m)
									return(list(mean=mean(temp <- unlist(sapply(simTime$useyrs, FUN=function(y) ((temp <- rle(prcp.dy$ppt[simTime2$month_ForEachUsedDay == m & simTime2$year_ForEachUsedDay == y] > 0))$lengths[temp$values]) )), na.rm=TRUE),
													sd=sd(temp, na.rm=TRUE))))
						
						dds <- sapply(st_mo, FUN=function(m)
									return(list(mean=mean(temp <- unlist(sapply(simTime$useyrs, FUN=function(y) ((temp <- rle(prcp.dy$ppt[simTime2$month_ForEachUsedDay == m & simTime2$year_ForEachUsedDay == y] == 0))$lengths[temp$values]) )), na.rm=TRUE),
													sd=sd(temp, na.rm=TRUE))))
						
						tv <- sapply(st_mo, FUN=function(m) sd(temp.dy$mean[simTime2$month_ForEachUsedDay == m], na.rm=TRUE) )
						
						res[nv+st_mo-1] <- unlist(dws[1, ])
						res[nv+st_mo-1+12] <- unlist(dws[2, ])
						res[nv+st_mo-1+24] <- unlist(dds[1, ])
						res[nv+st_mo-1+36] <- unlist(dds[2, ])
						res[nv+st_mo-1+48] <- tv
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+59)] <- c(paste("meanDWSdays_m", st_mo, sep=""), paste("meanDWSdays_m", st_mo, ".sd", sep=""), paste("meanDDSdays_m", st_mo, sep=""), paste("meanDDSdays_m", st_mo, ".sd", sep=""), paste("StDevDailyTempC_m", st_mo, sep=""))
						nv <- nv+60
						
						rm(dws, dws.sd, dds, dds.sd, tv)
					}
					
					
					#daily weather frequency distributions
					if(any(simulation_timescales=="daily") & aon$dailyWeatherEventSizeDistribution){
						if(!exists("prcp.dy")) prcp.dy <- get_PPT_dy(dir.sw.runs.sc.out[sc])
						#prcp-event sizes in bins
						events <- lapply(simTime$useyrs, FUN=function(y) floor((temp <- prcp.dy$ppt[simTime2$year_ForEachUsedDay == y])[temp>0]/bin.prcpSizes)*bin.prcpSizes)
						bins.summary <- (0:6) * bin.prcpSizes	#aggregate to maximal 7 bins
						bins.available <- sort(unique(unlist(events))) #bins present
						counts.available <- as.matrix(sapply(simTime$useyrs - startyr + 1, FUN=function(y) sapply(bins.available, FUN=function(b) sum(events[[y]] == b))))
						counts.summary <- matrix(data=0, ncol=simTime$no.useyr, nrow=length(bins.summary))
						counts.summary[bins.summary %in% bins.available, ] <- counts.available[bins.available %in% bins.summary, ]
						if(max(bins.summary) < max(bins.available)){ #if bins available that are outside the summary bins, then sum them up into the highest summary bin
							if(length(bins.suming <- which(bins.available >= max(bins.summary))) > 1){
								counts.summary[length(bins.summary), ] <- apply(counts.available[bins.suming, ], MARGIN=2, FUN=sum)
							} else {
								counts.summary[length(bins.summary), ] <- counts.available[bins.suming, ]
							}
						}
						res[nv:(nv+6)] <- apply(counts.summary, MARGIN=1, FUN=mean)
						res[(nv+7):(nv+7+6)] <- apply(counts.summary, MARGIN=1, FUN=sd)
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+13)] <- c(temp <- paste("PrcpEventSizes_NoPerYearIn", bins.summary, "+mm", sep=""), paste(temp, ".sd", sep=""))
						
						#duration of prcp-free days in bins
						durations <- lapply(simTime$useyrs, FUN=function(y) floor(((temp <- rle(prcp.dy$ppt[simTime2$year_ForEachUsedDay == y] == 0))$lengths[temp$values]-1)/bin.prcpfreeDurations)*bin.prcpfreeDurations )
						bins.summary <- (0:3) * bin.prcpfreeDurations	#aggregate to maximal 4 bins
						bins.available <- sort(unique(unlist(durations)))	#bins present
						counts.available <- as.matrix(sapply(simTime$useyrs - startyr + 1, FUN=function(y) sapply(bins.available, FUN=function(b) sum(durations[[y]] == b))))
						counts.summary <- matrix(data=0, ncol=simTime$no.useyr, nrow=length(bins.summary))
						counts.summary[bins.summary %in% bins.available, ] <- counts.available[bins.available %in% bins.summary, ]
						if(max(bins.summary) < max(bins.available)){ #if bins available that are outside the summary bins, then sum them up into the highest summary bin
							if(length(bins.suming <- which(bins.available >= max(bins.summary))) > 1){
								counts.summary[length(bins.summary), ] <- apply(counts.available[bins.suming, ], MARGIN=2, FUN=sum)
							} else {
								counts.summary[length(bins.summary), ] <- counts.available[bins.suming, ]
							}
						}
						res[(nv+14):(nv+14+3)] <- apply(counts.summary, MARGIN=1, FUN=mean)
						res[(nv+14+4):(nv+14+4+3)] <- apply(counts.summary, MARGIN=1, FUN=sd)
						
						rm(events, durations, counts.available, counts.summary)
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[(nv+14):(nv+21)] <- c(temp <- paste("PrcpFreeDurations_NoPerYearIn", bins.summary+1, "+days", sep=""), paste(temp, ".sd", sep=""))
						nv <- nv+22
					}
					
					
					#Variables to estimate percent C4 species in North America: Teeri JA, Stowe LG (1976) Climatic patterns and the distribution of C4 grasses in North America. Oecologia, 23, 1-12.
					#accountNSHemispheres_agg
					if(any(simulation_timescales=="daily") & aon$dailyC4_TempVar){
						if(!exists("temp.dy")) temp.dy <- get_Temp_dy(dir.sw.runs.sc.out[sc])
						
						res[nv:(nv+5)] <- as.numeric(temp <- dailyC4_TempVar(dailyTempMin=temp.dy$min, dailyTempMean=temp.dy$mean, simTime2))
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+5)] <- names(temp)
						nv <- nv+6
						
					}
					#mean and SD of DOY and value of minimum/maximum of transpiration as in'dailySWPextremes' (don't distinguish between top/bottom soil layers)
					if(any(simulation_timescales=="daily") & aon$dailyTranspirationExtremes) {
						if(!exists("transp.dy")) transp.dy <- get_Response_aggL(dir.sw.runs.sc.out[sc], transpdy, "dy", 10, FUN=sum)
						
						extremes <- as.matrix(aggregate(cbind(transp.dy$top + transp.dy$bottom), by=list(simTime2$year_ForEachUsedDay), FUN=function(x) c(max(x), min(x), which.max(x), which.min(x))))
						res[nv:(nv+3)] <- c(apply(temp <- extremes[, c(2:3)], MARGIN=2, FUN=mean), apply(temp, MARGIN=2, FUN=sd))
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+7)] <- c(temp <- c("transp.dy.max_mm", "transp.dy.min_mm"), paste(temp, "sd", sep="."))
						nv <- nv+4
						
						res[nv:(nv+3)] <- c(apply(extremes[, 4:5], MARGIN=2, FUN=function(x) circ.mean(x, int=365)), apply(extremes[, 4:5], MARGIN=2, FUN=function(x) circ.sd(x, int=365)))
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+7)] <- c(temp <- c("transp.max_doy", "transp.min_doy"), paste(temp, "sd", sep="."))
						nv <- nv+4
						
						rm(extremes)
					}
					#mean and SD of DOY and value of minimum/maximum of total evaporation (surface + soil) as in'dailySWPextremes' (don't distinguish between top/bottom soil layers)
					if(any(simulation_timescales=="daily") & aon$dailyTotalEvaporationExtremes) {
						if(!exists("evsoil.dy")) evsoil.dy <- get_Response_aggL(dir.sw.runs.sc.out[sc], evsoildy, "dy", 10, FUN=sum)
						if(!exists("evapsurface.dy")) evapsurface.dy <- get_Response_aggL(dir.sw.runs.sc.out[sc], evapsurfacedy, "dy", 10, FUN=sum)
						
						
						extremes <- as.matrix(aggregate(cbind(evsoil.dy$top + evsoil.dy$bottom + evapsurface.dy$top + evapsurface.dy$bottom), by=list(simTime2$year_ForEachUsedDay), FUN=function(x) c(max(x), min(x), which.max(x), which.min(x))))
						res[nv:(nv+3)] <- c(apply(temp <- extremes[, c(2:3)], MARGIN=2, FUN=mean), apply(temp, MARGIN=2, FUN=sd))
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+7)] <- c(temp <- c("evap.dy.max_mm", "evap.dy.min_mm"), paste(temp, "sd", sep="."))
						nv <- nv+4
						
						res[nv:(nv+3)] <- c(apply(extremes[, 4:5], MARGIN=2, FUN=function(x) circ.mean(x, int=365)), apply(extremes[, 4:5], MARGIN=2, FUN=function(x) circ.sd(x, int=365)))
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+7)] <- c(temp <- c("evap.max_doy", "evap.min_doy"), paste(temp, "sd", sep="."))
						nv <- nv+4
						
						rm(extremes)
					}
					#mean and SD of DOY and value of minimum/maximum of deep drainage as in'dailySWPextremes' (don't distinguish between top/bottom soil layers
					if(any(simulation_timescales=="daily") & aon$dailyDrainageExtremes) {
						if(!exists("deepdrain.dy")) deepdrain.dy <- get_Response_aggL(dir.sw.runs.sc.out[sc], deepdraindy, "dy", 10, FUN=sum)
						
						extremes <- as.matrix(aggregate(cbind(deepdrain.dy$top + deepdrain.dy$bottom), by=list(simTime2$year_ForEachUsedDay), FUN=function(x) c(max(x), min(x), which.max(x), which.min(x))))
						res[nv:(nv+3)] <- c(apply(temp <- extremes[, c(2:3)], MARGIN=2, FUN=mean), apply(temp, MARGIN=2, FUN=sd))
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+7)] <- c(temp <- c("deepDrainage.dy.max_mm", "deepDrainage.dy.min_mm"), paste(temp, "sd", sep="."))
						nv <- nv+4
						
						res[nv:(nv+3)] <- c(apply(extremes[, 4:5], MARGIN=2, FUN=function(x) circ.mean(x, int=365)), apply(extremes[, 4:5], MARGIN=2, FUN=function(x) circ.sd(x, int=365)))
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+7)] <- c(temp <- c("deepDrainage.max_doy", "deepDrainage.min_doy"), paste(temp, "sd", sep="."))
						nv <- nv+4
						
						rm(extremes)
					}
					#mean and SD of DOY and value of minimum/maximum of AET as in'dailySWPextremes' (don't distinguish between top/bottom soil layers
					if(any(simulation_timescales=="daily") & aon$dailyAETExtremes) {						
						if(!exists("AET.dy")) AET.dy <- get_Response_aggL(dir.sw.runs.sc.out[sc], aetdy, "dy", 10, FUN=sum)
						
						extremes <- as.matrix(aggregate(cbind(AET.dy$top + AET.dy$bottom), by=list(simTime2$year_ForEachUsedDay), FUN=function(x) c(max(x), min(x), which.max(x), which.min(x))))
						res[nv:(nv+3)] <- c(apply(temp <- extremes[, c(2:3)], MARGIN=2, FUN=mean), apply(temp, MARGIN=2, FUN=sd))
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+7)] <- c(temp <- c("AET.dy.max_mm", "AET.dy.min_mm"), paste(temp, "sd", sep="."))
						nv <- nv+4
						
						res[nv:(nv+3)] <- c(apply(extremes[, 4:5], MARGIN=2, FUN=function(x) circ.mean(x, int=365)), apply(extremes[, 4:5], MARGIN=2, FUN=function(x) circ.sd(x, int=365)))
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+7)] <- c(temp <- c("AET.max_doy", "AET.min_doy"), paste(temp, "sd", sep="."))
						nv <- nv+4
						
						rm(extremes)
					}
					
					#snowpack
					if(any(simulation_timescales=="monthly") & aon$monthlySnowpack){
						if(!exists("SWE.mo")) SWE.mo <- get_SWE_mo(dir.sw.runs.sc.out[sc])
						res[nv+st_mo-1] <- aggregate(SWE.mo$val, by=list(simTime2$month_ForEachUsedMonth), FUN=mean)[,2]
						res[nv+st_mo-1+12] <- aggregate(SWE.mo$val, by=list(simTime2$month_ForEachUsedMonth), FUN=sd)[,2]
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+23)] <- c(paste("snowpack_mm_m", st_mo, sep=""), paste("snowpack_mm_m", st_mo, ".sd", sep=""))
						nv <- nv+24
					}
					
					#daily snowpack: accountNSHemispheres_agg
					if(any(simulation_timescales=="daily") & aon$dailySnowpack){
						if(!exists("SWE.dy")) SWE.dy <- get_SWE_dy(dir.sw.runs.sc.out[sc])
						
						if(sum(SWE.dy$val) > 0){
							snowyears <- simTime2$year_ForEachUsedDay_NSadj + ifelse(simTime2$doy_ForEachUsedDay_NSadj > 273, 1, 0)	# 1. snow-year: N-hemisphere: October 1st = 1 day of snow year; S-hemisphere: April 1st = 1 day of snow year
							adjDays <- ifelse(simTime2$doy_ForEachUsedDay[1] == simTime2$doy_ForEachUsedDay_NSadj[1], 365 - 273, -91)
							
							if(length(unique(snowyears))-2 > 0){
								res.snow  <- matrix(data=0, nrow=length(unique(snowyears))-2, ncol=6, byrow=TRUE)
								res.snow[,1]  <- unique(snowyears)[2:(length(unique(snowyears))-1)]  # 1. snowyear
								snowyear.trim <- !is.na(pmatch(snowyears, res.snow[, 1], duplicates.ok=TRUE))
								res.snow[,2] <- unlist(aggregate(SWE.dy$val[snowyear.trim], by=list(snowyears[snowyear.trim]), FUN=which.max)[, 2]) - adjDays # 2. doy of peak snowpack water-equivalent (mm)
								res.snow[,5] <- unlist(aggregate(SWE.dy$val[snowyear.trim], by=list(snowyears[snowyear.trim]), FUN=function(s) sum(s>0))[, 2]) # 5. total number of days of snow cover
								res.snow[,6] <- unlist(aggregate(SWE.dy$val[snowyear.trim], by=list(snowyears[snowyear.trim]), FUN=max)[, 2]) # 6. peak snowpack water-equivalent (mm)
								
								#attempt to get rid of for loop
								#r.lengths <- aggregate(SWE.dy$val, by=list(snowyears), FUN=function(s) rle(ifelse(s>0,1,0))$lengths )[-c(1,30),]
								#r.values <- aggregate(SWE.dy$val, by=list(snowyears), FUN=function(s) rle(ifelse(s>0,1,0))$values )[-c(1,30),]
								
								syi <- 1
								for (sy in res.snow[,1]){
									r <- rle(ifelse(SWE.dy$val[which(snowyears == sy)]>0,1,0))
									res.snow[syi,4] <- r$lengths[which(r$values==1)][order(r$lengths[which(r$values==1)], decreasing=TRUE)[1]] # 4. number of continous days of snow cover
									ind <- which(r$lengths==res.snow[syi,4])
									res.snow[syi,3] <- cumsum(r$lengths)[ifelse(length(ind)>1, ind[which.max(r$values[ind])], ind)] - adjDays # 3. last day of continous snow cover
									syi <- syi + 1
								}
								if(nrow(res.snow) > 1){
									res[nv:(nv+9)] <- c(apply(res.snow[, 2:3], 2, FUN=function(x) circ.mean(x, int=365, na.rm=TRUE)), apply(res.snow[,-(1:3)], 2, mean, na.rm=TRUE),  apply(res.snow[, 2:3], 2, FUN=function(x) circ.sd(x, int=365, na.rm=TRUE)), apply(res.snow[,-(1:3)], 2, sd, na.rm=TRUE))
								} else {
									res[nv:(nv+9)] <- c(res.snow[,-1], rep(0, times=5))
								}
							} else {
								res[nv:(nv+9)] <- 0					
							}
						} else {
							res[nv:(nv+9)] <- 0					
						}
						
						rm(snowyears, snowyear.trim, res.snow, adjDays)
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+9)] <- c(temp <- c("Snowcover_NSadj_Peak_doy", "Snowcover_NSadj_ContLastDay_doy", "Snowcover_NSadj_Peak_mm", "Snowcover_NSadj_ContDays", "Snowcover_NSadj_TotDays"), paste(temp, "sd", sep="."))
						nv <- nv+10
					}		
					
					
					if(any(simulation_timescales=="daily") & any(simulation_timescales=="yearly") & aon$dailySnowpack){			
						if(!exists("prcp.yr")) prcp.yr <- get_PPT_yr(dir.sw.runs.sc.out[sc])
						if(!exists("prcp.dy")) prcp.dy <- get_PPT_dy(dir.sw.runs.sc.out[sc])
						if(!exists("SWE.dy")) SWE.dy <- get_SWE_dy(dir.sw.runs.sc.out[sc])
						
						#Portion of rain that falls on snow
						res[nv] <- mean(aggregate(ifelse(SWE.dy$val > 0, prcp.dy$rain, 0), by=list(simTime2$year_ForEachUsedDay), FUN=sum)[, 2], na.rm=TRUE) / mean(prcp.yr$ppt, na.rm=TRUE)
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv] <- c("RainOnSnowOfMAP")
						nv <- nv+1
					}
					
					#soil temp
					if(any(simulation_timescales == "monthly") & aon$monthlySoilTemp) {
						if(!exists("soiltemp.mo")) soiltemp.mo <- get_Response_aggL(dir.sw.runs.sc.out[sc], soiltempmo, "mo", scaler=1, FUN=weighted.mean, weights=layers_width)
						
						res[nv+st_mo-1] <- soiltemp.mo$aggMean.top
						res[nv+st_mo-1+12] <- soiltemp.mo$aggMean.bottom
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+23)] <- c(paste("soiltemp.top_m", st_mo, sep=""), paste("soiltemp.bot_m", st_mo, sep=""))
						nv <- nv+24
					}
					
					#soil water potential
					if(any(simulation_timescales=="monthly") & aon$monthlySWP){
						if(!exists("vwc.mo")) vwc.mo <- get_Response_aggL(dir.sw.runs.sc.out[sc], vwcmo, "mo", 1, FUN=weighted.mean, weights=layers_width)
						if(!exists("swp.mo")) swp.mo <- get_SWP_aggL(vwc.mo)
						
						res[nv+st_mo-1] <- swp.mo$aggMean.top
						res[nv+st_mo-1+12] <- swp.mo$aggMean.bottom
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+23)] <- c(paste("swp.top_MPa_m", st_mo, sep=""), paste("swp.bottom_MPa_m", st_mo, sep=""))
						nv <- nv+24
					}
					
					
					#dry periods based on monthly swp data: accountNSHemispheres_agg
					if(any(simulation_timescales=="monthly") & aon$monthlySWPdryness){
						if(!exists("vwc.mo")) vwc.mo <- get_Response_aggL(dir.sw.runs.sc.out[sc], vwcmo, "mo", 1, FUN=weighted.mean, weights=layers_width)
						if(!exists("swp.mo")) swp.mo <- get_SWP_aggL(vwc.mo)
						
						adjMonths <- ifelse(simTime2$month_ForEachUsedMonth[1] == simTime2$month_ForEachUsedMonth_NSadj[1], 0, 6)
						
						drymonths.top <- drymonths.bottom <- array(data=0, dim=c(length(SWPcrit_MPa), 12, simTime$no.useyr))
						for(icrit in seq(along=SWPcrit_MPa)){
							drymonths.top[icrit, , ] <- aggregate(swp.mo$top, by=list(simTime2$month_ForEachUsedMonth_NSadj), FUN=function(x) ifelse(x <= SWPcrit_MPa[icrit], 1, 0))[, -1]
							drymonths.bottom[icrit, , ] <- aggregate(swp.mo$bottom, by=list(simTime2$month_ForEachUsedMonth_NSadj), FUN=function(x) ifelse(x <= SWPcrit_MPa[icrit], 1, 0))[, -1]
						}
						
						years.top <- apply(drymonths.top, MARGIN=c(1, 3), FUN=sum)
						years.bottom <- apply(drymonths.bottom, MARGIN=c(1, 3), FUN=sum)
						
						res[nv:(nv+2*length(SWPcrit_MPa)-1)] <- c(apply(years.top, MARGIN=1, FUN=mean), apply(years.bottom, MARGIN=1, FUN=mean))
						res[(nv+2*length(SWPcrit_MPa)):(nv+4*length(SWPcrit_MPa)-1)] <- c(apply(years.top, MARGIN=1, FUN=sd), apply(years.bottom, MARGIN=1, FUN=sd))
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+4*length(SWPcrit_MPa)-1)] <- c(temp <- paste(rep(c("DryPeriod_NSadj.Top.TotMonthsMean_SWPcrit", "DryPeriod_NSadj.Bottom.TotMonthsMean_SWPcrit"), each=length(SWPcrit_MPa)), paste(abs(round(-1000*SWPcrit_MPa, 0)), "kPa", sep=""), sep=""), gsub("Mean", "SD", temp))
						nv <- nv+4*length(SWPcrit_MPa)
						
						start.top <- apply(drymonths.top, MARGIN=c(1, 3), FUN=match, x=1, nomatch=0)
						start.top[start.top != 0] <- ifelse((temp <- (start.top[start.top != 0] + adjMonths) %% 12) == 0, 12, temp)
						start.bottom <- apply(drymonths.bottom, MARGIN=c(1, 3), FUN=match, x=1, nomatch=0)
						start.bottom[start.bottom != 0] <- ifelse((temp <- (start.bottom[start.bottom != 0] + adjMonths) %% 12) == 0, 12, temp)
						
						res[nv:(nv+2*length(SWPcrit_MPa)-1)] <- c(apply(start.top, MARGIN=1, FUN=function(x) circ.mean(x, int=12)), apply(start.bottom, MARGIN=1, FUN=function(x) circ.mean(x, int=12)))
						res[(nv+2*length(SWPcrit_MPa)):(nv+4*length(SWPcrit_MPa)-1)] <- c(apply(start.top, MARGIN=1, FUN=function(x) circ.sd(x, int=12)), apply(start.bottom, MARGIN=1, FUN=function(x) circ.sd(x, int=12)))
						
						rm(drymonths.top, drymonths.bottom, years.top, start.top, years.bottom, start.bottom, adjMonths)
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+4*length(SWPcrit_MPa)-1)] <- c(temp <- paste(rep(c("DryPeriod_NSadj.Top.MeanStartMonth_SWPcrit", "DryPeriod_NSadj.Bottom.MeanStartMonth_SWPcrit"), each=length(SWPcrit_MPa)), paste(abs(round(-1000*SWPcrit_MPa, 0)), "kPa", sep=""), sep=""), gsub("Mean", "SD", temp))
						nv <- nv+4*length(SWPcrit_MPa)
					}
					
					
					#extremes and timing of swp on a monthly basis
					if(any(simulation_timescales=="monthly") & aon$monthlySWPextremes){
						if(!exists("vwc.mo")) vwc.mo <- get_Response_aggL(dir.sw.runs.sc.out[sc], vwcmo, "mo", 1, FUN=weighted.mean, weights=layers_width)
						if(!exists("swp.mo")) swp.mo <- get_SWP_aggL(vwc.mo)
						
						res[nv] <- max(swp.mo$aggMean.top)
						res[nv+1] <- min(swp.mo$aggMean.top)
						res[nv+2] <- max(swp.mo$aggMean.bottom)
						res[nv+3] <- min(swp.mo$aggMean.bottom)
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+3)] <- c("swp.top.mo.max_MPa", "swp.top.mo.min_MPa", "swp.bottom.mo.max_MPa", "swp.bottom.mo.min_MPa")
						nv <- nv+4
						
						res[nv] <- match(res[nv-4], swp.mo$aggMean.top)
						res[nv+1] <- match(res[nv-3], swp.mo$aggMean.top)
						res[nv+2] <- match(res[nv-2], swp.mo$aggMean.bottom)
						res[nv+3] <- match(res[nv-1], swp.mo$aggMean.bottom)
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+3)] <- c("swp.top.max_Month", "swp.top.min_Month", "swp.bottom.max_Month", "swp.bottom.min_Month")
						nv <- nv+4	
					}
					
					
					#extremes and timing of swp on a daily basis
					if(any(simulation_timescales=="daily") & aon$dailySWPextremes){
						if(!exists("vwc.dy")) vwc.dy <- get_Response_aggL(dir.sw.runs.sc.out[sc], vwcdy, "dy", 1, FUN=weighted.mean, weights=layers_width)
						if(!exists("swp.dy")) swp.dy <- get_SWP_aggL(vwc.dy)
						
						extremes <- as.matrix(aggregate(cbind(swp.dy$top, swp.dy$bottom), by=list(simTime2$year_ForEachUsedDay), FUN=function(x) c(max(x), min(x), which.max(x), which.min(x))))
						res[nv:(nv+7)] <- c(apply(temp <- extremes[, c(2:3, 6:7)], MARGIN=2, FUN=mean), apply(temp, MARGIN=2, FUN=sd))
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+7)] <- c(temp <- c("swp.top.dy.max_MPa", "swp.top.dy.min_MPa", "swp.bottom.dy.max_MPa", "swp.bottom.dy.min_MPa"), paste(temp, "sd", sep="."))
						nv <- nv+8
						
						res[nv:(nv+7)] <- c(apply(extremes[, c(4:5, 8:9)], MARGIN=2, FUN=function(x) circ.mean(x, int=365)), apply(extremes[, c(4:5, 8:9)], MARGIN=2, FUN=function(x) circ.sd(x, int=365)))
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+7)] <- c(temp <- c("swp.top.max_doy", "swp.top.min_doy", "swp.bottom.max_doy", "swp.bottom.min_doy"), paste(temp, "sd", sep="."))
						nv <- nv+8
						
						rm(extremes)
					}
					
					
					#correl monthly swp (top and bottom) vs. pet and ppt vs. temp, use product moment correlation coefficient {eqn. 11.6, \Sala, 1997 #45}
					if(any(simulation_timescales=="monthly") & aon$monthlyCorrelations){
						if(!exists("vwc.mo")) vwc.mo <- get_Response_aggL(dir.sw.runs.sc.out[sc], vwcmo, "mo", 1, FUN=weighted.mean, weights=layers_width)
						if(!exists("swp.mo")) swp.mo <- get_SWP_aggL(vwc.mo)
						if(!exists("temp.mo")) temp.mo <- get_Temp_mo(dir.sw.runs.sc.out[sc])
						if(!exists("prcp.mo")) prcp.mo <- get_PPT_mo(dir.sw.runs.sc.out[sc])
						if(!exists("PET.mo")) PET.mo <- get_PET_mo(dir.sw.runs.sc.out[sc])
						
						cor2  <- function(y) cor(y[,1], y[,2])
						
						#in case var(ppt or swp)==0 => cor is undefined: exclude those years
						res[nv] <- mean( temp <- by(data.frame(PET.mo$val, swp.mo$top), INDICES=simTime2$yearno_ForEachUsedMonth, FUN=cor2), na.rm=TRUE )
						res[nv+1] <- sd(temp, na.rm=TRUE)
						res[nv+2] <- mean( temp <- by(data.frame(PET.mo$val, swp.mo$bottom), INDICES=simTime2$yearno_ForEachUsedMonth, FUN=cor2), na.rm=TRUE )
						res[nv+3] <- sd(temp, na.rm=TRUE)
						
						res[nv+4] <- mean( temp <- by(data.frame(temp.mo$mean, prcp.mo$ppt), INDICES=simTime2$yearno_ForEachUsedMonth, FUN=cor2), na.rm=TRUE )
						res[nv+5] <- sd(temp, na.rm=TRUE)
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+5)] <- paste(rep(c("CorPETSWPtop", "CorPETSWPbottom", "CorTempPPT"), each=2), c("", ".sd"), sep="")
						nv <- nv+6
					}
					
					#cummulative frequency distribution of durations of dry soils in each of the four seasons and for each of the SWP.crit
					if(any(simulation_timescales=="daily") & aon$dailySWPdrynessDurationDistribution){
						if(!exists("vwc.dy")) vwc.dy <- get_Response_aggL(dir.sw.runs.sc.out[sc], vwcdy, "dy", 1, FUN=weighted.mean, weights=layers_width)
						if(!exists("swp.dy")) swp.dy <- get_SWP_aggL(vwc.dy)
						
						deciles <- (0:10)*10/100
						mo_seasons <- matrix(data=c(12,1:11), ncol=3, nrow=4, byrow=TRUE)
						season.flag <- c("DJF", "MAM", "JJA", "SON")
						seasonal.years <- c(simTime2$year_ForEachUsedDay[-(1:31)], rep(-9999, times=31))	#shift beginning of year to Dec 1
						
						for(icrit in seq(along=SWPcrit_MPa)){
							
							wet.top <- swp.dy$top >= SWPcrit_MPa[icrit]
							
							if(length(bottomL) >= 1) wet.bottom <- swp.dy$bottom >= SWPcrit_MPa[icrit]
							
							for(season in 1:nrow(mo_seasons)){
								durations.top <- sapply(simTime$useyrs, FUN=function(y) {if(length(temp <- (temp <- rle(wet.top[seasonal.years == y & (simTime2$month_ForEachUsedDay %in% mo_seasons[season,])] == 0))$lengths[temp$values]) > 0) {return(max(temp))} else {return(0)}} )
								if(length(bottomL) > 0) durations.bottom <- sapply(simTime$useyrs, FUN=function(y) {if(length(temp <- (temp <- rle(wet.bottom[seasonal.years == y & (simTime2$month_ForEachUsedDay %in% mo_seasons[season,])] == 0))$lengths[temp$values]) > 0) {return(max(temp))} else {return(0)}} )
								
								res[nv:(nv+length(deciles)-1)] <- quantile(durations.top, probs=deciles, type=4)
								res[(nv+length(deciles)):(nv+2*length(deciles)-1)] <- if(length(bottomL) > 0) quantile(durations.bottom, probs=deciles, type=4) else 0
								
								if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+2*length(deciles)-1)] <- c(paste("Quantile", formatC(deciles*100, flag="0", width=3, format="d"), "%_DurationDryTopSoilsInDays_Season", season.flag[season], "_SWPcrit", paste(abs(round(-1000*SWPcrit_MPa[icrit], 0)), "kPa", sep=""), sep=""), paste("Quantile", formatC(deciles*100, flag="0", width=3, format="d"), "%_DurationDryBottomSoilsInDays_Season", season.flag[season], "_SWPcrit", paste(abs(round(-1000*SWPcrit_MPa[icrit], 0)), "kPa", sep=""), sep=""))
								nv <- nv+2*length(deciles)
							}
						}
						rm(wet.top, wet.bottom)
					}
					
					if(any(simulation_timescales=="daily") && aon$dailySWPdrynessEventSizeDistribution)
					{
						if(!exists("vwc.dy.all")) vwc.dy.all <- get_Response_aggL(dir.sw.runs.sc.out[sc], vwcdy, "dyAll", 1, FUN=weighted.mean, weights=layers_width)
						if(!exists("swp.dy.all")) swp.dy.all <- get_SWP_aggL(vwc.dy.all)
						binSize <- c(1,8,15,29,367)#use for interval
						
						EventDistribution <- function(data) {#data is the values for one year adj for SWPcrit_MPa
							if(length(temp <- (temp <- rle(data == 0))$lengths[temp$values]) > 0) { 
								bins <- c(0,0,0,0)
								for(z in 1:length(temp)) {
									bins[findInterval(temp[z],binSize)] <- (bins[findInterval(temp[z],binSize)]+1) 
								}
								return(bins)
							}  else {
								return(c(0,0,0,0))
							}
						}
						#if(i_SWRunInformation$Y_WGS84 < 0) {
						#for(k in simTime$useyrs) {#go through the years and adjust each day of year for South
						#	swp.dy$top[simTime2$year_ForEachUsedDay == k] <- swp.dy$top[simTime2$year_ForEachUsedDay == k][simTime2$doy_ForEachUsedDay_NSadj[simTime2$year_ForEachUsedDay == k]]
						#	swp.dy$bottom[simTime2$year_ForEachUsedDay == k] <- swp.dy$bottom[simTime2$year_ForEachUsedDay == k][simTime2$doy_ForEachUsedDay_NSadj[simTime2$year_ForEachUsedDay == k]]
						#}
						#}
						for(icrit in seq(along=SWPcrit_MPa)){
							
							wet.top <- swp.dy.all$top[simTime$index.usedy] >= SWPcrit_MPa[icrit]
							
							if(length(bottomL) >= 1) {
								wet.bottom <- swp.dy.all$bottom[simTime$index.usedy] >= SWPcrit_MPa[icrit]
							} else {
								wet.bottom <- matrix(data=NA, nrow=length(swp.dy$bottom), ncol=1)
							}
							
							#apply over each year, rle just on selected year store runs in vec, if that is greater than 0 then add to that years bins else return 0s for that year. Will result in a matrix of 4 by Years
							#binsYears.top <- sapply(simTime$useyrs, FUN=function(y) {if(length(temp <- (temp <- rle(wet.top[simTime2$year_ForEachUsedDay == y] == 0))$lengths[temp$values]) > 0) { bins <- c(0,0,0,0); for(z in 1:length(temp)) { bins[findInterval(temp[z],binSize)] <- (bins[findInterval(temp[z],binSize)]+1) }; return(bins) } else {return(c(0,0,0,0))}} )
							binsYears.top <- aggregate(wet.top, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=EventDistribution)$x
							bin_top_mean <- apply(binsYears.top, MARGIN = 2, mean) #mean of each bin size across a year - vector of 4
							bin_top_sd <- apply(binsYears.top, MARGIN = 2, sd) # sd of each bin size across a year - vector of 4
							
							if(length(bottomL) > 0) {
								#binsYears.bottom <- sapply(simTime$useyrs, FUN=function(y) {if(length(temp <- (temp <- rle(wet.bottom[simTime2$year_ForEachUsedDay == y] == 0))$lengths[temp$values]) > 0) { bins <- c(0,0,0,0); for(z in 1:length(temp)) { bins[findInterval(temp[z],binSize)] <- (bins[findInterval(temp[z],binSize)]+1) }; return(bins) } else {return(c(0,0,0,0))}} )
								binsYears.bottom <- aggregate(wet.bottom, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=EventDistribution)$x
								bin_bottom_mean <- apply(binsYears.bottom, MARGIN = 2, mean)
								bin_bottom_sd <- apply(binsYears.bottom, MARGIN = 2, sd)
							}
							bottom <- if(length(bottomL) > 0) c(bin_bottom_mean, bin_bottom_sd) else rep(0,8)
							res[nv:(nv+15)] <- c(bin_top_mean, bin_top_sd, bottom)
							
							if(i==ifirst || makeOutputDB) {
								baseTitle <- paste(paste("SWPdryness_Length",c("1-7days","8-14days","15-28days", "29+days") ,"_SWPcrit", paste(abs(round(-1000*SWPcrit_MPa[icrit], 0)), "kPa", sep=""), sep=""), sep="")
								resultfiles.Aggregates.header[nv:(nv+15)] <- c(paste(baseTitle, c(rep("_top_mean", 4), rep("_top_sd",4)), sep=""),	 paste(baseTitle, c(rep("_bottom_mean", 4), rep("_bottom_sd",4)), sep=""))
							}
							nv <- nv+16
						}
						rm(wet.top, wet.bottom)
					}
					
					if(any(simulation_timescales=="daily") && aon$dailySWPdrynessIntensity)
					{
						if(!exists("vwc.dy.all")) vwc.dy.all <- get_Response_aggL(dir.sw.runs.sc.out[sc], vwcdy, "dyAll", 1, FUN=weighted.mean, weights=layers_width)
						
						cut0 <- function(x) {x[x < 0] <- 0; return(x)}
						SWCtop <- vwc.dy.all$top * sum(layers_width[topL])*10
						if(length(bottomL) > 0) SWCbottom <- vwc.dy.all$bottom * sum(layers_width[bottomL])*10
						
						for(icrit in seq(along=SWPcrit_MPa)){
							
							SWCcritT <- SWPtoVWC(SWPcrit_MPa[icrit], texture$sand.top, texture$clay.top) * sum(layers_width[topL])*10
							if(length(bottomL) > 0) SWCcritB <- SWPtoVWC(SWPcrit_MPa[icrit], texture$sand.bottom, texture$clay.bottom) * sum(layers_width[bottomL])*10
							
							SWCtopAdjCrit <- cut0(SWCcritT - SWCtop)
							SWCbottomAdjCrit <- cut0(SWCcritB - SWCbottom)
							
							Intensity_top_mean <- mean( sumSWCtopAdjCritYears <- sapply(simTime$useyrs, FUN=function(y) sum(SWCtopAdjCrit[simTime2$year_ForEachUsedDay == y])) )
							Intensity_top_sd <- sd( sumSWCtopAdjCritYears )
							
							if(length(bottomL) > 0) {
								Intensity_bottom_mean <- mean( sumSWCbottomAdjCritYears <- sapply(simTime$useyrs, FUN=function(y) sum(SWCbottomAdjCrit[simTime2$year_ForEachUsedDay == y])) )
								Intensity_bottom_sd <- sd( sumSWCbottomAdjCritYears )
							}
							
							bottom <- if(length(bottomL) > 0) c(Intensity_bottom_mean, Intensity_bottom_sd) else rep(0,2)
							res[nv:(nv+3)] <- c(Intensity_top_mean, Intensity_top_sd, bottom)
							
							if(i==ifirst || makeOutputDB) {
								baseTitle <- paste("IntensityDryPeriod_", paste(abs(round(-1000*SWPcrit_MPa[icrit], 0)), "kPa", sep=""), sep="")
								resultfiles.Aggregates.header[nv:(nv+3)] <- c(paste(baseTitle, c(rep("_top_mean_mm", 1), rep("_top_sd",1)), sep=""), paste(baseTitle, c(rep("_bottom_mean_mm", 1), rep("_bottom_sd_mm",1)), sep=""))
							}
							nv <- nv+4
						}
					}
					
					#Degree days based on daily temp, and wet degree days on daily temp and swp
					if(any(simulation_timescales=="daily") & aon$dailyDegreeDays){
						if(!exists("vwc.dy")) vwc.dy <- get_Response_aggL(dir.sw.runs.sc.out[sc], vwcdy, "dy", 1, FUN=weighted.mean, weights=layers_width)
						if(!exists("swp.dy")) swp.dy <- get_SWP_aggL(vwc.dy)
						if(!exists("temp.dy")) temp.dy <- get_Temp_dy(dir.sw.runs.sc.out[sc])
						
						degday <- ifelse(temp.dy$mean > DegreeDayBase, temp.dy$mean - DegreeDayBase, 0) #degree days
						temp <- aggregate(degday, by=list(simTime2$year_ForEachUsedDay), FUN=sum)[, 2]
						res[nv:(nv+1)] <- c(mean(temp), sd(temp))
						
						for(icrit in seq(along=SWPcrit_MPa)){
							
							wet.top <- swp.dy$top >= SWPcrit_MPa[icrit]
							
							if(length(bottomL) >= 1) {
								wet.bottom <- swp.dy$bottom >= SWPcrit_MPa[icrit]
							} else {
								wet.bottom <- matrix(data=NA, nrow=length(swp.dy$bottom), ncol=1)
							}
							
							wetdegday.top <- ifelse(wet.top > 0, degday, 0)
							wetdegday.bottom <- ifelse(wet.bottom > 0, degday, 0)
							wetdegday.any <- ifelse(wet.top + wet.bottom > 0, degday, 0)
							
							temp <- aggregate(data.frame(wetdegday.top, wetdegday.bottom, wetdegday.any), by=list(simTime2$year_ForEachUsedDay), FUN=sum)[, 2:4]
							res[(nv+2+6*(icrit-1)):(nv+2+6*(icrit-1)+5)] <- c(apply(temp, MARGIN=2, FUN=mean), apply(temp, MARGIN=2, FUN=sd))
						}
						rm(degday, wet.top, wet.bottom, temp)
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+2+6*length(SWPcrit_MPa)-1)] <- c("DegreeDaysMean_Cdays", "DegreeDaysSD_Cdays", temp <- paste(rep(c("WetDegreeDaysMean.top_Cdays_SWPcrit", "WetDegreeDaysMean.bottom_Cdays_SWPcrit", "WetDegreeDaysMean.any_Cdays_SWPcrit"), times=length(SWPcrit_MPa)), rep(paste(abs(round(-1000*SWPcrit_MPa, 0)), "kPa", sep=""), each=3), sep=""), gsub("Mean", "SD", temp))
						nv <- nv+2+6*length(SWPcrit_MPa)	
					}	
					
					
					
					#Dry and wet periods based on daily swp: accountNSHemispheres_agg
					if(any(simulation_timescales=="daily") & aon$dailySWPdrynessANDwetness){
						if(!exists("vwc.dy.all")) vwc.dy.all <- get_Response_aggL(dir.sw.runs.sc.out[sc], vwcdy, "dyAll", 1, FUN=weighted.mean, weights=layers_width)
						if(!exists("swp.dy.all")) swp.dy.all <- get_SWP_aggL(vwc.dy.all)
						
						adjDays <- simTime2$doy_ForEachUsedDay_NSadj[1] - simTime2$doy_ForEachUsedDay[1]
						
						for(icrit in seq(along=SWPcrit_MPa)){
							
							wet_crit <- swp.dy.all$val >= SWPcrit_MPa[icrit]
							if(length(topL) > 1) {
								wet.top <- apply(wet_crit[simTime$index.usedy,2+topL], 1, sum)
							} else {
								wet.top <- wet_crit[simTime$index.usedy,2+topL]
							}
							if(length(bottomL)>1) {
								wet.bottom <- apply(wet_crit[simTime$index.usedy,2+bottomL], 1, sum)
							} else {
								if(bottomL==0) {
									wet.bottom <- matrix(data=NA, nrow=length(simTime$index.usedy), ncol=1)
								} else {
									wet.bottom  <- ifelse(wet_crit[simTime$index.usedy,2+bottomL], 1, 0)
								}
							}
							
							AtLeastOneWet.top <- ifelse(wet.top>0,1,0)
							AllWet.top <- ifelse(wet.top==length(topL),1,0)
							AllDry.top <- ifelse(wet.top==0,1,0)
							AtLeastOneDry.top <- ifelse(wet.top<length(topL),1,0)
							
							AtLeastOneWet.bottom <- ifelse(wet.bottom>0,1,0)
							AllWet.bottom <- ifelse(wet.bottom==length(bottomL),1,0)
							AllDry.bottom <- ifelse(wet.bottom==0,1,0)
							AtLeastOneDry.bottom <- ifelse(wet.bottom<length(bottomL),1,0)
							
							
							#wet periods
							res.wet <- matrix(data=0, nrow=length(unique(simTime2$year_ForEachUsedDay_NSadj)), ncol=8)
							res.wet[, 1] <- aggregate(AtLeastOneWet.top, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=sum)[,2] # total number of days per year when at least one top/bottom layer is wet
							res.wet[, 2] <- aggregate(AtLeastOneWet.bottom, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=sum)[,2]
							
							res.wet[, 3] <- aggregate(AtLeastOneWet.top, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=max.duration )[,2] # maximum number of continous days when at least one top/bottom layers is wet
							res.wet[, 4] <- aggregate(AtLeastOneWet.bottom, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=max.duration )[,2]
							
							res.wet[, 5] <- aggregate(AllWet.top, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=sum)[,2] # total number of days per year when all top/bottom layer are wet
							res.wet[, 6] <- aggregate(AllWet.bottom, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=sum)[,2]
							
							res.wet[, 7] <- aggregate(AllWet.top, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=max.duration )[,2] # maximum number of continous days when all top/bottom layers are wet
							res.wet[, 8] <- aggregate(AllWet.bottom, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=max.duration )[,2]
							
							
							#dry periods
							res.dry <- matrix(data=0, nrow=length(unique(simTime2$year_ForEachUsedDay_NSadj)), ncol=8)
							# 3/7. total number of days/year when all top/bottom layers are dry
							#correct [,c(3,7)] for years when start<end otherwise set 0
							res.dry[,3] <- aggregate(AllDry.top, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=sum)[,2] 
							res.dry[,7] <- aggregate(AllDry.bottom, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=sum)[,2]
							
							#4/8. maximum number of continous days when all top/bottom layers are dry
							res.dry[,4] <- aggregate(AllDry.top, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=max.duration )[,2]
							res.dry[,8] <- aggregate(AllDry.bottom, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=max.duration )[,2]
							
							# 1/5. start days/year when at least one of top/bottom layers are dry for at least ten days
							res.dry[,1] <- aggregate(AtLeastOneDry.top, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=start10days )[,2] - adjDays
							res.dry[,5] <- aggregate(AtLeastOneDry.bottom, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=start10days )[,2] - adjDays
							
							# 2/6. end days/year when at least one of top/bottom layers have been dry for at least ten days
							res.dry[,2] <- aggregate(AtLeastOneDry.top, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=end10days )[,2] - adjDays
							res.dry[,6] <- aggregate(AtLeastOneDry.bottom, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=end10days )[,2] - adjDays
							
							#correct [,c(3,7)] for years when start<end otherwise set 0
							res.dry[,3] <- ifelse(res.dry[,2]-res.dry[,1]>0, res.dry[,3], 0) 
							res.dry[,7] <- ifelse(res.dry[,6]-res.dry[,5]>0, res.dry[,7], 0)
							
							
							#aggregate results
							res[(nv+2*16*(icrit-1)):(nv+2*16*icrit-1)] <- c(apply(temp <- data.frame(res.wet, res.dry[, -c(1:2, 5:6)]), MARGIN=2, FUN=mean, na.rm=TRUE),
									apply(res.dry[, c(1:2, 5:6)], MARGIN=2, FUN=function(x) circ.mean(x, int=365, na.rm=TRUE)),
									apply(temp, MARGIN=2, FUN=sd, na.rm=TRUE),
									apply(res.dry[, c(1:2, 5:6)], MARGIN=2, FUN=function(x) circ.sd(x, int=365, na.rm=TRUE)))
						}
						rm(res.dry, wet.top, wet.bottom, wet_crit, AtLeastOneWet.top, AtLeastOneWet.bottom, AllWet.top, AllWet.bottom, AllDry.top, AllDry.bottom)
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+2*16*length(SWPcrit_MPa)-1)] <- paste(rep(c(temp <- c("WetPeriod_NSadj.top.MeanDaysAnyWet_SWPcrit", "WetPeriod_NSadj.bottom.MeanDaysAnyWet_SWPcrit", "WetPeriod_NSadj.top.MeanLongestPeriodAnyWet_SWPcrit", "WetPeriod_NSadj.bottom.MeanLongestPeriodAnyWet_SWPcrit", "WetPeriod_NSadj.top.MeanDaysAllWet_SWPcrit", "WetPeriod_NSadj.bottom.MeanDaysAllWet_SWPcrit", "WetPeriod_NSadj.top.MeanLongestPeriodAllWet_SWPcrit", "WetPeriod_NSadj.bottom.MeanLongestPeriodAllWet_SWPcrit",
															"DryPeriod_NSadj.top.MeanDaysAllDry_SWPcrit", "DryPeriod_NSadj.top.MeanLongestPeriodAllDry_SWPcrit", "DryPeriod_NSadj.bottom.MeanDaysAllDry_SWPcrit", "DryPeriod_NSadj.bottom.MeanLongestPeriodAllDry_SWPcrit", "DryPeriod_NSadj.top.MeanStartDay_SWPcrit", "DryPeriod_NSadj.top.MeanEndDay_SWPcrit", "DryPeriod_NSadj.bottom.MeanStartDay_SWPcrit", "DryPeriod_NSadj.bottom.MeanEndDay_SWPcrit"), gsub("Mean", "SD", temp)), times=length(SWPcrit_MPa)), rep(paste(abs(round(-1000*SWPcrit_MPa, 0)), "kPa", sep=""), each=2*16), sep="")
						nv <- nv+2*16*length(SWPcrit_MPa)	
					}	
					
					
					#volumetric soil water content top and bottom for each month
					#monthly data
					if(any(simulation_timescales=="monthly") & aon$monthlySWCvol){
						if(!exists("vwc.mo")) vwc.mo <- get_Response_aggL(dir.sw.runs.sc.out[sc], vwcmo, "mo", 1, FUN=weighted.mean, weights=layers_width)
						
						res[nv+st_mo-1] <- vwc.mo$aggMean.top
						res[nv+st_mo-1+12] <- vwc.mo$aggMean.bottom
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+23)] <- c(paste("swcvol.top_mm_m", st_mo, sep=""), paste("swcvol.bottom_mm_m", st_mo, sep=""))
						nv <- nv+24
					}				
					
					
					#total soil water content top and bottom for each month
					#monthly data
					if(any(simulation_timescales=="monthly") & aon$monthlySWCtot){
						if(!exists("swc.mo")) swc.mo <- get_Response_aggL(dir.sw.runs.sc.out[sc], swcmo, "mo", 10, FUN=sum)
						
						res[nv+st_mo-1] <- swc.mo$aggMean.top
						res[nv+st_mo-1+12] <- swc.mo$aggMean.bottom
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+23)] <- c(paste("swctot.top_mm_m", st_mo, sep=""), paste("swctot.bottom_mm_m", st_mo, sep=""))
						nv <- nv+24
					}				
					
					
					#available soil water top and bottom for each month
					#monthly data
					if(any(simulation_timescales=="monthly") & aon$monthlySWA){
						if(!exists("swa.mo")) swa.mo <- get_Response_aggL(dir.sw.runs.sc.out[sc], swamo, "mo", 10, FUN=sum)
						
						res[nv+st_mo-1] <- swa.mo$aggMean.top
						res[nv+st_mo-1+12] <- swa.mo$aggMean.bottom
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+23)] <- c(paste("swa.top_mm_m", st_mo, sep=""), paste("swa.bottom_mm_m", st_mo, sep=""))
						nv <- nv+24
					}	
					
					#extremes and timing of swa
					if(any(simulation_timescales=="monthly") & aon$monthlySWAextremes){
						if(!exists("swa.mo")) swa.mo <- get_Response_aggL(dir.sw.runs.sc.out[sc], swamo, "mo", 10, FUN=sum)
						
						res[nv] <- max(swa.mo$aggMean.top)
						res[nv+1] <- min(swa.mo$aggMean.top)
						res[nv+2] <- max(swa.mo$aggMean.bottom)
						res[nv+3] <- min(swa.mo$aggMean.bottom)
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+3)] <- c("swa.top.max_mm", "swa.top.min_mm", "swa.bottom.max_mm", "swa.bottom.min_mm")
						nv <- nv+4
						
						res[nv] <- match(res[nv-4], swa.mo$aggMean.top)
						res[nv+1] <- match(res[nv-3], swa.mo$aggMean.top)
						res[nv+2] <- match(res[nv-2], swa.mo$aggMean.bottom)
						res[nv+3] <- match(res[nv-1], swa.mo$aggMean.bottom)
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+3)] <- c("swa.top.Monthmax", "swa.top.Monthmin", "swa.bottom.Monthmax", "swa.bottom.Monthmin")
						nv <- nv+4	
					}
					
					#transpiration top and bottom for each month
					#monthly data
					if(any(simulation_timescales=="monthly") & aon$monthlyTranspiration){
						if(!exists("transp.mo")) transp.mo <- get_Response_aggL(dir.sw.runs.sc.out[sc], transpmo, "mo", 10, FUN=sum)
						
						res[nv+st_mo-1] <- transp.mo$aggMean.top
						res[nv+st_mo-1+12] <- transp.mo$aggMean.bottom
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+23)] <- c(paste("transp.top_mm_m", st_mo, sep=""), paste("transp.bottom_mm_m", st_mo, sep=""))
						nv <- nv+24
					}	
					
					
					#soil evaporation for each month
					#monthly data
					if(any(simulation_timescales=="monthly") & aon$monthlySoilEvaporation){
						if(!exists("evsoil.mo")) evsoil.mo <- get_Response_aggL(dir.sw.runs.sc.out[sc], evsoilmo, "mo", 10, FUN=sum)
						
						res[nv+st_mo-1] <- aggregate(evsoil.mo$top + evsoil.mo$bottom, by=list(simTime2$month_ForEachUsedMonth), FUN=mean)[,2]
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+11)] <- c(paste("evapsoil_mm_m", st_mo, sep=""))
						nv <- nv+12
					}	
					
					
					#monthly AET and PET	
					if(any(simulation_timescales=="monthly") & aon$monthlyAET){
						if(!exists("AET.mo")) AET.mo <- get_AET_mo(dir.sw.runs.sc.out[sc])
						res[nv+st_mo-1] <- aggregate( AET.mo$val , by=list(simTime2$month_ForEachUsedMonth), FUN=mean)[,2]
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+11)] <- paste("AET_mm_m", st_mo, sep="")
						nv <- nv+12
					}	
					
					if(any(simulation_timescales=="monthly") & aon$monthlyPET){
						if(!exists("PET.mo")) PET.mo <- get_PET_mo(dir.sw.runs.sc.out[sc])
						res[nv+st_mo-1] <- aggregate( PET.mo$val , by=list(simTime2$month_ForEachUsedMonth), FUN=mean)[,2]
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+11)] <- paste("PET_mm_m", st_mo, sep="")
						nv <- nv+12
					}	
					
					
					#transpiration:PET and soil evaporation:PET for each month
					#monthly data
					if(any(simulation_timescales=="monthly") & aon$monthlyPETratios){
						if(!exists("PET.mo")) PET.mo <- get_PET_mo(dir.sw.runs.sc.out[sc])
						if(!exists("evsoil.mo")) evsoil.mo <- get_Response_aggL(dir.sw.runs.sc.out[sc], evsoilmo, "mo", 10, FUN=sum)
						if(!exists("transp.mo")) transp.mo <- get_Response_aggL(dir.sw.runs.sc.out[sc], transpmo, "mo", 10, FUN=sum)
						
						res[nv+st_mo-1] <- aggregate( ifelse( PET.mo$val == 0, 0, (transp.mo$top + transp.mo$bottom) / PET.mo$val) , by=list(simTime2$month_ForEachUsedMonth), FUN=mean)[,2]
						res[nv+st_mo-1+12] <- aggregate( ifelse( PET.mo$val == 0, 0, (evsoil.mo$top + evsoil.mo$bottom) / PET.mo$val) , by=list(simTime2$month_ForEachUsedMonth), FUN=mean)[,2]
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+23)] <- c(paste("TranspToPET_m", st_mo, sep=""), paste("EvapSoilToPET_m", st_mo, sep=""))
						nv <- nv+24
					}	
					
					
					#transpiration:AET and soil evaporation:AET for each month
					#monthly data
					if(any(simulation_timescales=="monthly") & aon$monthlyAETratios){
						if(!exists("AET.mo")) AET.mo <- get_AET_mo(dir.sw.runs.sc.out[sc])
						if(!exists("evsoil.mo")) evsoil.mo <- get_Response_aggL(dir.sw.runs.sc.out[sc], evsoilmo, "mo", 10, FUN=sum)
						if(!exists("transp.mo")) transp.mo <- get_Response_aggL(dir.sw.runs.sc.out[sc], transpmo, "mo", 10, FUN=sum)
						
						res[nv+st_mo-1] <- aggregate( ifelse( AET.mo$val == 0, 0, (transp.mo$top + transp.mo$bottom) / AET.mo$val) , by=list(simTime2$month_ForEachUsedMonth), FUN=mean)[,2]
						res[nv+st_mo-1+12] <- aggregate( ifelse( AET.mo$val == 0, 0, (evsoil.mo$top + evsoil.mo$bottom) / AET.mo$val) , by=list(simTime2$month_ForEachUsedMonth), FUN=mean)[,2]
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+23)] <- c(paste("TranspToAET_m", st_mo, sep=""), paste("EvapSoilToAET_m", st_mo, sep=""))
						nv <- nv+24
					}	
					
					if(any(simulation_timescales=="monthly") & aon$monthlyRunoff){
						if(!exists("runoff.mo")) runoff.mo <- get_Runoff_mo(dir.sw.runs.sc.out[sc])
						res[nv+st_mo-1] <- aggregate(runoff.mo$val, by=list(simTime2$month_ForEachUsedMonth), FUN=mean)[,2]
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+11)] <- c(paste("Runoff_m", st_mo, sep=""))
						nv <- nv+12
					}
					
					#hydraulic redistribution top and bottom for each month
					#monthly data
					if(any(simulation_timescales=="monthly") & aon$monthlyHydraulicRedistribution){
						if(!exists("hydred.mo")) hydred.mo <- get_Response_aggL(dir.sw.runs.sc.out[sc], hdmo, "mo", 10, FUN=sum)
						
						res[nv+st_mo-1] <- hydred.mo$aggMean.top
						res[nv+st_mo-1+12] <- hydred.mo$aggMean.bottom
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+23)] <- c(paste("hydred.top_mm_m", st_mo, sep=""), paste("hydred.bottom_mm_m", st_mo, sep=""))
						nv <- nv+24
					}	
					
					
					#monthly Infiltration
					if(any(simulation_timescales=="monthly") & aon$monthlyInfiltration){
						if(!exists("inf.mo")) inf.mo <- get_Inf_mo(dir.sw.runs.sc.out[sc])
						res[nv+st_mo-1] <- aggregate( inf.mo$inf , by=list(simTime2$month_ForEachUsedMonth), FUN=mean)[,2]
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+11)] <- c(paste("Infiltration_mm_m", st_mo, sep="") )
						nv <- nv+12
					}	
					
					
					#Fluxes
					#yearly data
					if(any(simulation_timescales=="yearly")  & aon$yearlyWaterBalanceFluxes) {
						if(!exists("prcp.yr")) prcp.yr <- get_PPT_yr(dir.sw.runs.sc.out[sc])
						if(!exists("Esurface.yr")) Esurface.yr <- get_Esurface_yr(dir.sw.runs.sc.out[sc])
						if(!exists("intercept.yr")) intercept.yr <- get_Interception_yr(dir.sw.runs.sc.out[sc])
						if(!exists("inf.yr")) inf.yr <- get_Inf_yr(dir.sw.runs.sc.out[sc])
						if(!exists("runoff.yr")) runoff.yr <- get_Runoff_yr(dir.sw.runs.sc.out[sc])
						if(!exists("transp.yr")) transp.yr <- get_Response_aggL(dir.sw.runs.sc.out[sc], transpyr, "yr", 10, sum)
						if(!exists("AET.yr")) AET.yr <- get_AET_yr(dir.sw.runs.sc.out[sc])
						if(!exists("PET.yr")) PET.yr <- get_PET_yr(dir.sw.runs.sc.out[sc])
						if(!exists("Esoil.yr")) Esoil.yr <- get_Response_aggL(dir.sw.runs.sc.out[sc], evsoilyr, "yr", 10, sum)
						if(!exists("deepDrain.yr")) deepDrain.yr <- get_DeepDrain_yr(dir.sw.runs.sc.out[sc])
						
						rain_toSoil <- prcp.yr$rain - intercept.yr$sum
						transp.tot <- transp.yr$top + transp.yr$bottom
						
						evap_soil.tot <- Esoil.yr$top + Esoil.yr$bottom
						evap.tot <- evap_soil.tot + Esurface.yr$sum + prcp.yr$snowloss
						
						temp1 <- 10 * read.table(file = file.path(dir.sw.runs.sc.out[sc], percolationyr), header = FALSE, sep = "", fill = TRUE, comment.char="")
						if(length(topL) > 1 & length(bottomL)>1) {
							drain.topTobottom <- temp1[simTime$index.useyr, 1+DeepestTopLayer]
						} else {
							drain.topTobottom <- NA
						}
						temp1 <- 10 * read.table(file = file.path(dir.sw.runs.sc.out[sc], hdyr), header = FALSE, sep = "", fill = TRUE, comment.char="")
						if(length(topL) > 1) {
							hydred.topTobottom <- apply(temp1[simTime$index.useyr,1+topL], 1, sum)
						} else {
							hydred.topTobottom <- temp1[simTime$index.useyr,1+topL]
						}					
						
						if( any(simulation_timescales=="daily")) {
							temp1 <- 10 * read.table(file = file.path(dir.sw.runs.sc.out[sc], swcdy), header = FALSE, sep = "", fill = TRUE, comment.char="")
							if(simTime$index.usedy[1] == 1){ #simstartyr == startyr, then (simTime$index.usedy-1) misses first value
								index.usedyPlusOne <- simTime$index.usedy[-length(simTime$index.usedy)]+1
							} else {
								index.usedyPlusOne <- simTime$index.usedy
							}
							swcdyflux <- apply(temp1[index.usedyPlusOne,2+ld], 1, sum)-apply(temp1[index.usedyPlusOne-1,2+ld], 1, sum)
							swc.flux <- aggregate(swcdyflux, by=list(temp1[index.usedyPlusOne,1]), FUN=sum)[,2]							
						} else {
							swc.flux <- NA
						}
						
						#mean fluxes ?use runoff.yr$val
						res[nv:(nv+22)] <- apply(fluxtemp <- cbind(prcp.yr$rain, rain_toSoil, prcp.yr$snowfall, prcp.yr$snowmelt, prcp.yr$snowloss, intercept.yr$sum, intercept.yr$veg, intercept.yr$litter, Esurface.yr$veg, Esurface.yr$litter, inf.yr$inf, runoff.yr$val, evap.tot, evap_soil.tot, Esoil.yr$top, Esoil.yr$bottom, transp.tot, transp.yr$top, transp.yr$bottom, hydred.topTobottom, drain.topTobottom, deepDrain.yr$val, swc.flux), 2, mean)
						res[nv+23] <- ifelse(sum(transp.tot)==0, 0, mean(transp.yr$bottom/transp.tot))
						res[nv+24] <- ifelse(sum(AET.yr$val)==0, 0, mean(transp.tot/AET.yr$val))
						res[nv+25] <- ifelse(sum(AET.yr$val)==0, 0, mean(evap_soil.tot/AET.yr$val))
						res[nv+26] <- ifelse(sum(PET.yr$val)==0, 0, mean(AET.yr$val/PET.yr$val))
						res[nv+27] <- ifelse(sum(PET.yr$val)==0, 0, mean(transp.tot/PET.yr$val))
						res[nv+28] <- ifelse(sum(PET.yr$val)==0, 0, mean(evap_soil.tot/PET.yr$val))
						
						#sd of fluxes
						res[(nv+29):(nv+51)] <- apply(fluxtemp, 2, sd)
						res[nv+52] <- ifelse(sum(transp.tot)==0, 0, sd(transp.yr$bottom/transp.tot))
						res[nv+53] <- ifelse(sum(AET.yr$val)==0, 0, sd(transp.tot/AET.yr$val))
						res[nv+54] <- ifelse(sum(AET.yr$val)==0, 0, sd(evap_soil.tot/AET.yr$val))
						res[nv+55] <- ifelse(sum(PET.yr$val)==0, 0, sd(AET.yr$val/PET.yr$val))
						res[nv+56] <- ifelse(sum(PET.yr$val)==0, 0, sd(transp.tot/PET.yr$val))
						res[nv+57] <- ifelse(sum(PET.yr$val)==0, 0, sd(evap_soil.tot/PET.yr$val))
						
						rm(rain_toSoil, transp.tot, evap_soil.tot, drain.topTobottom, hydred.topTobottom, index.usedyPlusOne, swcdyflux, swc.flux)
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+57)] <- c(htemp <- c("RainTot_mm", "RainToSoil_mm", "Snowfall_mm", "Snowmelt_mm", "Snowloss_mm", "InterceptionTot_mm", "InterceptionVeg_mm", "InterceptionLitter_mm", "EvapVeg_mm", "EvapLitter_mm", "Infiltration_mm", "Runoff_mm", "EvapTot_mm", "EvapSoilTot_mm", "EvapSoilTop_mm", "EvapSoilBottom_mm", "TranspTot_mm", "TranspTop_mm", "TranspBottom_mm", "HydRed_TopToBottom_mm", "PercolationTopToBottom_mm", "DrainDeep_mm", "SWCFlux_mm", "TbottomToT", "TtoAET", "EStoAET", "AETtoPET", "TtoPET", "EStoPET"), paste(htemp, ".sd", sep=""))
						nv <- nv+58
					}
					
					
					#regeneration: accountNSHemispheres_agg
					if(any(simulation_timescales=="daily")  & aon$dailySWPbasedRegeneration) {
						if(!exists("swp.dy.all")) swp.dy.all <- list(val=-1/10*read.table(file = file.path(dir.sw.runs.sc.out[sc], swpdy), header = FALSE, sep = "", fill = TRUE, comment.char=""))	#no vwcdy available!
						swp.surface <- swp.dy.all$val[simTime$index.usedy, 3]
						if(!exists("SWE.dy")) SWE.dy <- get_SWE_dy(dir.sw.runs.sc.out[sc])
						
						regenerationThisYear_YN <- function(x){
							# calculate season doys
							snowcover <- ifelse(x[,2]>0, 1, 0)
							r <- rle(snowcover)
							rseries <- ifelse(r$values==0, 1:length(r$values), 0)
							then <- which(rseries==rseries[rseries>0][which.max(r$lengths[rseries>0])])
							if(typeof(season.start) == "character"){ #calculate last day of the longest snowpack
								if(then==1){
									season.start <- 1
								} else {
									season.start <- cumsum(r$lengths)[then-1]
								}
							}
							if(typeof(season.end) == "character"){ #calculate first day of the longest snowpack
								season.end <- min(c(cumsum(r$lengths)[then]+1, length(snowcover)))
							}
							if(length(season.start:season.end) > 0){
								swp.season <- x[season.start:season.end,1]
								gs <- rle(ifelse(swp.season>=germination.swp.surface, 1, 0))
								es <- rle(ifelse(swp.season>=establishment.swp.surface, 1, 0))
								
								reg <- 0
								# get vector of establishment starts and ends
								establishment.start.dos <- establishment.end.dos <- NULL
								for(esi in 1:length(es$lengths)){
									if(es$lengths[esi] >= establishment.duration & es$values[esi] > 0){
										establishment.start.dos <- c(establishment.start.dos, ifelse(esi == 1, 1, cumsum(es$lengths)[esi-1]+1))
										establishment.end.dos <- c(establishment.end.dos, cumsum(es$lengths)[esi])
									}
								}
								
								# check if any germination period matches up with an establishment period
								if(length(establishment.end.dos) > 0){
									for(gsi in 1:length(gs$lengths)){
										if(gs$lengths[gsi] >= germination.duration & gs$values[gsi] > 0){
											germination.start.dos <- ifelse(gsi == 1, 1, cumsum(gs$lengths)[gsi-1]+1)
											germination.end.dos <- cumsum(gs$lengths)[gsi]
											if( any( ((germination.start.dos + germination.duration >= establishment.start.dos) &
																(germination.start.dos + germination.duration + establishment.duration <= establishment.end.dos)) |
															((germination.end.dos + establishment.delay >= establishment.start.dos) &
																(germination.end.dos + establishment.delay + establishment.duration <= establishment.end.dos)) ) ){
												reg <- reg + 1
											}
										}
									}
								}
								
							} else {
								reg <- 0
							}
							return (ifelse(reg>0, 1, 0))
						}
						
						res[nv:(nv+1)] <- c(mean(temp <- c(by(data=data.frame(swp.surface, SWE.dy$val), INDICES=simTime2$year_ForEachUsedDay_NSadj, FUN=regenerationThisYear_YN ))), sd(temp))
						
						rm(swp.surface)
						
						if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+1)] <- c("FractionYearsWithRegenerationMean_NSadj", "FractionYearsWithRegenerationSD_NSadj")
						nv <- nv+2
					}
					
					
					#Artemisia tridentata regeneration according to factor model (2012-02-15, drs), call for every regeneration species
					#accountNSHemispheres_agg: param$Doy_SeedDispersalStart0 must be set correctly
					if(any(simulation_timescales=="daily")  & aon$dailyRegeneration_byTempSWPSnow & no.species_regeneration > 0){
						#---Access daily data, which do not depend on specific species parameters, i.e., start of season
						if(!exists("swp.dy.all")) swp.dy.all <- list(val=-1/10*read.table(file = file.path(dir.sw.runs.sc.out[sc], swpdy), header = FALSE, sep = "", fill = TRUE, comment.char=""))	#no vwcdy available!
						temp.snow <- read.table(file = file.path(dir.sw.runs.sc.out[sc], snowdy), header = FALSE, sep = "", fill = TRUE, comment.char="")
						temp.temp <- read.table(file = file.path(dir.sw.runs.sc.out[sc], tempdy), header = FALSE, sep = "", fill = TRUE, comment.char="")
						TmeanJan <- mean(temp.temp[simTime$index.usedy, 5][simTime2$month_ForEachUsedDay_NSadj==1], na.rm=TRUE)	#mean January (N-hemisphere)/July (S-hemisphere) air temperature based on normal 'doy'
						temp.soiltemp <- try(read.table(file = file.path(dir.sw.runs.sc.out[sc], soiltempdy), header = FALSE, sep = "", fill = TRUE, comment.char=""), silent=TRUE)
						if(identical(class(temp.soiltemp), "try-error") || all(temp.soiltemp[, -(1:2)] == 0)){
							use.soiltemp <- FALSE	#flag whether soil temperature output is available or not (and then air temperature is used instead of top soil temperature)
						} else {
							use.soiltemp <- TRUE	#currently we have only mean daily soil temperatures and not min/max which we need fo the model
						}
						
						#---Functional relationships
						#Function to convert soil depth to soil layer
						SoilLayer_at_SoilDepth <- function(depth_cm){
							return( pmax(1, pmin(length(layers_depth), 1+findInterval(depth_cm-0.01, layers_depth))) )
						}						
						
						
						#Function to calculate for each day of the year, duration in days of upcoming favorable conditions accounting for consequences.unfavorable=0 (if conditions become unfavorable, then restart the count), =1 (resume)
						calculate_DurationFavorableConditions <- function(RYyear, consequences.unfavorable){ 
							conditions <- Germination_DuringFavorableConditions[index.year <- RYyear_ForEachUsedDay==RYyear]
							doys <- 1:sum(index.year)
							doys[!conditions] <- NA	#calculate only for favorable days
							out <- rep(NA, times=sum(index.year))
							if(consequences.unfavorable == 0){#if conditions become unfavorable, then restart the count afterwards
								temp.rle <- rle(conditions)
								if(sum(!temp.rle$values) > 0){
									temp.unfavorable_startdoy <- c((1 + c(0, cumsum(temp.rle$lengths)))[!temp.rle$values], 1 + sum(index.year)) #add starts for odd- and even-lengthed rle
									if(temp.rle$values[1]){#first rle period is favorable
										temp.rle$values <- rep(temp.unfavorable_startdoy, each=2)
									} else {#first rle period is unfavorable
										temp.rle$values <- rep(temp.unfavorable_startdoy[-1], each=2)
									}
									temp.rle$values <- temp.rle$values[1:length(temp.rle$lengths)]
								} else {#every day is favorable
									temp.rle$values <- length(conditions)+1
								}
								out <- inverse.rle(temp.rle) - doys	#difference to next following start of a period of unfavorable conditions
							} else if(consequences.unfavorable == 1){#if conditions become unfavorable, then resume the count afterwards
								if((temp <- sum(conditions)) > 0){
									count <- temp:1
								} else {#every day is unfavorable
									count <- vector("numeric", length=0)
								}
								out <- napredict(na.action(na.exclude(doys)), count)	#sum of following favorable conditions in this year
							}
							return(out)
						}
						
						
						#Function to estimate time to germinate for each day of a given year and conditions (temperature, top soil SWP)
						calculate_TimeToGerminate_modifiedHardegree2006NLR <- function(RYyear){
							#values for current year
							conditions <- Germination_DuringFavorableConditions[index.year <- RYyear_ForEachUsedDay==RYyear]
							Tgerm.year <- soilTmeanSnow[index.year]
							SWPgerm.year <- swp.TopMean[index.year]
							durations <- LengthDays_FavorableConditions[index.year]	#consequences of unfavorable conditions coded in here
							doys.favorable <- (doys.padded <- 1:sum(index.year))[conditions]
							doys.padded[!conditions] <- NA
							
							#function determining time to germinate for a given day
							nrec.max <- 10
							rec.delta <- 1
							
							a <- max(sqrt(.Machine$double.eps), param$Hardegree_a)
							b <- param$Hardegree_b
							d <- max(sqrt(.Machine$double.eps), ifelse((temp <- param$Hardegree_d) == 1, ifelse(runif(1) > 0.5, 1 + .Machine$double.eps, 1 - .Machine$double.neg.eps), temp))
							temp.c <- ifelse(param$Hardegree_c != 0, param$Hardegree_c, sign(runif(1)-0.5) * sqrt(.Machine$double.eps))
							
							get_modifiedHardegree2006NLR <- function(RYdoy, Estimate_TimeToGerminate){
								for(nrec in 1:nrec.max){
									Estimate_TimeToGerminate <- Estimate_TimeToGerminate.oldEstimate <- max(0, round(Estimate_TimeToGerminate, 0))
									
									Tgerm <- mean(Tgerm.year[RYdoy:(RYdoy + Estimate_TimeToGerminate - 1)], na.rm=TRUE)
									SWPgerm <- mean(SWPgerm.year[RYdoy:(RYdoy + Estimate_TimeToGerminate - 1)], na.rm=TRUE)
									
									temp.c.lim <- -(Tgerm-b)*(d^2-1)/d
									if(temp.c > 0){
										c <- ifelse(temp.c > temp.c.lim, temp.c, temp.c.lim + sqrt(.Machine$double.eps))
									}
									if(temp.c < 0){
										c <- ifelse(temp.c < temp.c.lim, temp.c, temp.c.lim - sqrt(.Machine$double.eps))
									}
									
									#NLR model (eq.5) in Hardegree SP (2006) Predicting Germination Response to Temperature. I. Cardinal-temperature Models and Subpopulation-specific Regression. Annals of Botany, 97, 1115-1125.
									temp <- a * exp(-log(2)/log(d)^2 * log(1 + (Tgerm - b)*(d^2 - 1)/(c * d))^2)
									#drs addition to time to germinate dependent on mean January temperature and soil water potential
									temp <- 1/temp + param$TimeToGerminate_k1_meanJanTemp * TmeanJan + param$TimeToGerminate_k2_meanJanTempXIncubationTemp * TmeanJan * Tgerm + param$TimeToGerminate_k3_IncubationSWP * SWPgerm
									Estimate_TimeToGerminate <- max(1, round(temp, 0) )
									
									#break if convergence or not enough time in this year
									if(abs(Estimate_TimeToGerminate - Estimate_TimeToGerminate.oldEstimate) <= rec.delta | RYdoy + Estimate_TimeToGerminate - 1 > 365) break
								}
								
								if(nrec >= nrec.max){
									out <- round(mean(c(Estimate_TimeToGerminate, Estimate_TimeToGerminate.oldEstimate)), 0)
								} else {
									out <- Estimate_TimeToGerminate
								}
								out <- ifelse(out <= durations[RYdoy] & RYdoy + out <= 365, out, NA) #test whether enough time to germinate
								return(out)
							}
							
							TimeToGerminate.favorable <- sapply(doys.favorable, FUN=function(fd) get_modifiedHardegree2006NLR(RYdoy=fd, Estimate_TimeToGerminate=1))
							if(length(TimeToGerminate.favorable) == 0){
								TimeToGerminate.favorable <- vector("numeric", length=0)
							}
							return(napredict(na.action(na.exclude(doys.padded)), TimeToGerminate.favorable))
						}
						
						
						#Function to calculate mortality under conditions and checks survival limit
						calculate_SeedlingMortality_ByCondition <- function(kill.conditions, max.duration.before.kill){
							do.vector <- function(kill.vector, max.duration.before.kill){
								doys <- 1:length(kill.vector)
								doys[!kill.vector] <- NA	#calculate only for kill days
								temp.rle <- rle(kill.vector)
								if(sum(!temp.rle$values) > 0){
									temp.startdoy <- (1 + c(0, cumsum(temp.rle$lengths)))[!temp.rle$values]
									if(temp.rle$values[1]){
										temp.rle$values <- rep(temp.startdoy, each=2)
									} else {
										temp.rle$values <- rep(temp.startdoy[-1], each=2)
									}
									temp.rle$values <- temp.rle$values[1:length(temp.rle$lengths)]
								} else {#every day is kill free
									temp.rle$values <- length(kill.vector)+1
								}
								kill.durations <- inverse.rle(temp.rle) - doys
								mortality <- rep(FALSE, times=length(kill.vector))
								mortality[kill.durations > max.duration.before.kill] <- TRUE
								return(mortality)
							}
							if(length(dim(kill.conditions)) > 0){ #i.e., is.matrix, columns=soil layers
								out <- apply(kill.conditions, MARGIN=2, FUN=function(x) do.vector(kill.vector=x, max.duration.before.kill))
							} else {
								out <- do.vector(kill.conditions, max.duration.before.kill)
							}
							return(out)
						}
						
						
						
						#Function to calculate favorable conditions for seedling growth for each day of a given year
						calculate_SuitableGrowthThisYear_UnderCondition <- function(favorable.conditions, consequences.unfavorable){
							out <- rep(NA, times=length(favorable.conditions))
							if(consequences.unfavorable == 0){#if conditions become unfavorable, then stop growth for rest of season
								temp.rle <- rle(favorable.conditions)
								temp.firstFavorable.index <- which(temp.rle$values)[1]
								if(!is.na(temp.firstFavorable.index) && temp.firstFavorable.index < length(temp.rle$values)){
									temp.rle$values[(temp.firstFavorable.index+1):length(temp.rle$values)] <- FALSE
									out <- inverse.rle(temp.rle)
								} else { #nothing changed, either because all days are either favorable or unfavorable or because first favorable period is also the last in the season
									out <- favorable.conditions
								}
							} else if(consequences.unfavorable == 1){#if conditions become unfavorable, then resume growth afterwards
								out <- favorable.conditions
							}
							return(out)
						}
						
						
						#Function to calculate rooting depth at given age
						SeedlingRootingDepth <- function(age, P0, K, r){
							depth <- K * P0 * exp(r * age) / (K + P0 * (exp(r * age) - 1))	#[age] = days, [P0, K, r] = mm
							depth <- pmax(0, depth)
							return(depth/10)	#cm
						}
						
						
						#Function that checks whether all relevant (those with roots) soil layers are under conditions of mortality (kill.conditions) for each day of a given year  
						get_KilledBySoilLayers <- function(relevantLayers, kill.conditions){
							temp <- data.frame(relevantLayers, kill.conditions)
							return( apply(temp, MARGIN=1, FUN=function(x) {if(!is.na(x[1])){return(all(x[2:(2 + x[1] - 1)]))} else {return(NA)} } ) )
						}
						
						
						#Loop through each species
						prev.Doy_SeedDispersalStart <- 0
						for(sp in 1:no.species_regeneration){
							param <- data.frame(t(param.species_regeneration[,sp]))
							
							#Regeneration year=RY: RYdoy=1 == start of seed dispersal = start of 'regeneration year'
							Doy_SeedDispersalStart <- max(round(param$Doy_SeedDispersalStart0 + param$SeedDispersalStart_DependencyOnMeanTempJanuary * TmeanJan, 0) %% 365, 1)
							moveByDays <- ifelse(Doy_SeedDispersalStart ==  1, 1, max(as.numeric(as.POSIXlt(paste(simTime$useyrs[1] - 1, "-12-31", sep="")) - as.POSIXlt(paste(simTime$useyrs[1] - 1, "-01-01", sep=""))) + 1 - (Doy_SeedDispersalStart - 1) %% 365, 1))
							#Calculate regeneration year dates
							if(startyr > simstartyr){#start earlier to complete RY
								RY.index.usedy <- c(((st <- simTime$index.usedy[1])-moveByDays):(st-1), simTime$index.usedy[-(((et <- length(simTime$index.usedy))-moveByDays+1):et)]) #index indicating which rows of the daily SoilWat output is used
								RYyear_ForEachUsedDay <- simTime2$year_ForEachUsedDay	#'regeneration year' for each used day
								RYdoy_ForEachUsedDay <- simTime2$doy_ForEachUsedDay	#'doy of the regeneration year' for each used day
							} else if(!(startyr > simstartyr)){#start later to get a complete RY
								RY.index.usedy <- simTime$index.usedy[-c(1:(Doy_SeedDispersalStart - 1), (((et <- length(simTime$index.usedy))-moveByDays+1):et))]
								RYyear_ForEachUsedDay <- simTime2$year_ForEachUsedDay[-which(simTime2$year_ForEachUsedDay == simTime2$year_ForEachUsedDay[1])]
								RYdoy_ForEachUsedDay <- simTime2$doy_ForEachUsedDay[-which(simTime2$year_ForEachUsedDay == simTime2$year_ForEachUsedDay[1])]
							}
							year_ForEachUsedRYDay <- c(rep(simTime$useyrs[1] - 1, times=moveByDays), RYyear_ForEachUsedDay[-(((et <- length(RYyear_ForEachUsedDay))-moveByDays+1):et)])	#normal year for each used 'doy of the regeneration year'
							doy_ForEachUsedRYDay <- c(((st <- simTime$index.usedy[1])-moveByDays):(st-1), RYdoy_ForEachUsedDay[-(((et <- length(RYdoy_ForEachUsedDay))-moveByDays+1):et)])	#normal doy for each used 'doy of the regeneration year'
							RY.useyrs <- unique(RYyear_ForEachUsedDay)	#list of 'regeneration years' that are used for aggregation
							
							#Access daily data, the first time and afterwards only if Doy_SeedDispersalStart is different from value of previous species
							if(sp == 1 || Doy_SeedDispersalStart != prev.Doy_SeedDispersalStart){
								swp <- swp.dy.all$val[RY.index.usedy, 2 + ld]
								snow <- temp.snow[RY.index.usedy, 3]*10 #mm swe in snowpack
								airTminSnow <- ifelse(snow > 0, param$Temp_ExperiencedUnderneathSnowcover, temp.temp[RY.index.usedy, 4])
								airTmax <- temp.temp[RY.index.usedy, 3]
								if(use.soiltemp){
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
							SoilLayers_RelevantToGermination <- SoilLayer_at_SoilDepth(param$SoilDepth_RelevantToGermination)
							if(length(SoilLayers_RelevantToGermination) == 1){
								Germination_AtMoreThanTopSWPmin <- swp[, SoilLayers_RelevantToGermination] >= param$SWP_MinimumForGermination
								swp.TopMean <- swp[, SoilLayers_RelevantToGermination]
							} else {
								Germination_AtMoreThanTopSWPmin <- apply(swp[, SoilLayers_RelevantToGermination], MARGIN=1, FUN=function(x) all(x >= param$SWP_MinimumForGermination))
								swp.TopMean <- apply(swp[, SoilLayers_RelevantToGermination], MARGIN=1, FUN=mean, na.rm=TRUE)
							}
							
							#Put all limits together
							Germination_DuringFavorableConditions <- Germination_AtBelowTmax & Germination_AtAboveTmin & Germination_AtMoreThanTopSWPmin
							
							#---2. Time to germinate
							#for each day with favorable conditions, determine whether period of favorable conditions (resumed or reset if broken) is long enough for successful completion of germination under current mean conditions
							LengthDays_FavorableConditions <- unlist(lapply(RY.useyrs, FUN=function(y) calculate_DurationFavorableConditions(RYyear=y, consequences.unfavorable=param$GerminationPeriods_0ResetOr1Resume)))
							Germination_TimeToGerminate <- unlist(lapply(RY.useyrs, FUN=function(y) calculate_TimeToGerminate_modifiedHardegree2006NLR(RYyear=y)))
							
							Germination_RestrictedByTimeToGerminate <- rep(FALSE, times=length(Germination_TimeToGerminate))
							Germination_RestrictedByTimeToGerminate[Germination_DuringFavorableConditions & is.na(Germination_TimeToGerminate)] <- TRUE
							
							#---3. Successful germinations
							GerminationSuccess_Initiated <- !is.na(Germination_TimeToGerminate)
							temp <- padded <- rep(FALSE, times=length(GerminationSuccess_Initiated))
							germ.starts <- (1:length(temp))[GerminationSuccess_Initiated]
							germ.durs <- Germination_TimeToGerminate[GerminationSuccess_Initiated] - 1
							if(param$GerminationPeriods_0ResetOr1Resume == 1){
								temp.wait <- na.exclude(unlist(lapply(1:length(temp), FUN=function(t)
														{
															if(!is.na(Germination_TimeToGerminate[t])){
																t3 <- which((t2 <- na.exclude(t1 <- LengthDays_FavorableConditions[t:length(LengthDays_FavorableConditions)]))[Germination_TimeToGerminate[t]] == t1)[1]
																out <- sum(is.na(t1[1:t3]))
															} else {
																out <- NA
															}
															return(out)
														})))
								germ.durs <- germ.durs + temp.wait
							}
							emergence.doys <- germ.starts + germ.durs #index of start of successful germinations + time to germinate (including wait time during unfavorable conditions if 'resume')
							temp[emergence.doys] <- TRUE
							padded[!GerminationSuccess_Initiated] <- NA
							Germination_Emergence.doys <- napredict(na.action(na.exclude(padded)), emergence.doys)
							Germination_Emergence <- temp
							
							
							#----SEEDLING SURVIVAL
							
							#---1. Seedling survival periods:
							#	mortality = !survival: days with conditions which kill a seedling, defined by upper/lower limits
							#	growth: days with conditions which allows a seedling to grow (here, roots), defined by upper/lower limits
							SeedlingMortality_UnderneathSnowCover <- calculate_SeedlingMortality_ByCondition(kill.conditions=(snow > param$SWE_MaximumForSeedlingGrowth), max.duration.before.kill=param$Days_SnowCover_MaximumForSeedlingSurvival)
							SeedlingMortality_ByTmin <- calculate_SeedlingMortality_ByCondition(kill.conditions=(airTminSnow < param$Temp_MinimumForSeedlingSurvival), max.duration.before.kill=0)
							SeedlingMortality_ByTmax <- calculate_SeedlingMortality_ByCondition(kill.conditions=(airTmax > param$Temp_MaximumForSeedlingSurvival), max.duration.before.kill=0)
							SeedlingMortality_ByChronicSWPMax <- calculate_SeedlingMortality_ByCondition(kill.conditions=(swp > param$SWP_ChronicMaximumForSeedlingSurvival), max.duration.before.kill=param$Days_ChronicMaximumForSeedlingSurvival)
							SeedlingMortality_ByChronicSWPMin <- calculate_SeedlingMortality_ByCondition(kill.conditions=(swp < param$SWP_ChronicMinimumForSeedlingSurvival), max.duration.before.kill=param$Days_ChronicMinimumForSeedlingSurvival)
							SeedlingMortality_ByAcuteSWPMin <- calculate_SeedlingMortality_ByCondition(kill.conditions=(swp < param$SWP_AcuteMinimumForSeedlingSurvival), max.duration.before.kill=0)														
							
							SeedlingGrowth_AbsenceOfSnowCover <- (snow <= param$SWE_MaximumForSeedlingGrowth)
							SeedlingGrowth_AtAboveTmin <- (airTminSnow >= param$Temp_MinimumForSeedlingGrowth)
							SeedlingGrowth_AtBelowTmax <- (airTmax <= param$Temp_MaximumForSeedlingGrowth)
							
							#---2. Grow and kill the seedlings
							SeedlingSurvival_1stSeason <- Seedling_Starts <- Germination_Emergence #TRUE=seedling that germinated on that day and survives until end of season; FALSE=no germination or seedling dies during the first season
							SeedlingMortality_CausesByYear <- matrix(data=0, nrow=length(RY.useyrs), ncol=9)
							colnames(SeedlingMortality_CausesByYear) <- c(	"SeedlingMortality_UnderneathSnowCover", "SeedlingMortality_ByTmin", "SeedlingMortality_ByTmax", "SeedlingMortality_ByChronicSWPMax", "SeedlingMortality_ByChronicSWPMin", "SeedlingMortality_ByAcuteSWPMin",
									"SeedlingMortality_DuringStoppedGrowth_DueSnowCover", "SeedlingMortality_DuringStoppedGrowth_DueTmin", "SeedlingMortality_DuringStoppedGrowth_DueTmax")
							for(y in seq(along=RY.useyrs)){#for each year
								RYDoys_SeedlingStarts_ThisYear <- which(Seedling_Starts[index.thisYear <- RYyear_ForEachUsedDay == RY.useyrs[y]])
								if(length(RYDoys_SeedlingStarts_ThisYear) > 0){#if there are any germinations
									#init values for this year
									no.days <- sum(index.thisYear)
									thisYear_SeedlingMortality_UnderneathSnowCover <- SeedlingMortality_UnderneathSnowCover[index.thisYear]
									thisYear_SeedlingMortality_ByTmin <- SeedlingMortality_ByTmin[index.thisYear]
									thisYear_SeedlingMortality_ByTmax <- SeedlingMortality_ByTmax[index.thisYear]
									thisYear_SeedlingMortality_ByChronicSWPMax <- SeedlingMortality_ByChronicSWPMax[index.thisYear, ]
									thisYear_SeedlingMortality_ByChronicSWPMin <- SeedlingMortality_ByChronicSWPMin[index.thisYear, ]
									thisYear_SeedlingMortality_ByAcuteSWPMin <- SeedlingMortality_ByAcuteSWPMin[index.thisYear, ]
									thisYear_SeedlingGrowth_AbsenceOfSnowCover <- SeedlingGrowth_AbsenceOfSnowCover[index.thisYear]
									thisYear_SeedlingGrowth_AtAboveTmin <- SeedlingGrowth_AtAboveTmin[index.thisYear]
									thisYear_SeedlingGrowth_AtBelowTmax <- SeedlingGrowth_AtBelowTmax[index.thisYear]
									
									for(sg_RYdoy in RYDoys_SeedlingStarts_ThisYear){#for each seedling indexed by day of germination
										#init values for this seedling and season
										index.thisSeedlingSeason <- (temp <- (1:no.days))[temp > sg_RYdoy]
										killed_byCauses_onRYdoy <- rep(NA, times=6)	#book-keeping of mortality causes
										names(killed_byCauses_onRYdoy) <- colnames(SeedlingMortality_CausesByYear)[1:6]
										stopped_byCauses_onRYdoy <- rep(NA, times=3)	#book-keeping of causes why growth stopped
										names(stopped_byCauses_onRYdoy) <- colnames(SeedlingMortality_CausesByYear)[7:9]
										
										#Establish days of growth (=TRUE) and surviving, but no growth (=FALSE)
										thisSeedlingGrowing <- rep(TRUE, no.days)
										if(sg_RYdoy > 1) thisSeedlingGrowing[1:(sg_RYdoy-1)] <- FALSE	#seedling germinated on sg_RYdoy, hence it cannot grow before germination day
										
										#Check growth under above-ground conditions
										#Snow cover
										thisSeedlingGrowth_AbsenceOfSnowCover <- calculate_SuitableGrowthThisYear_UnderCondition(favorable.conditions=thisSeedlingGrowing & thisYear_SeedlingGrowth_AbsenceOfSnowCover, consequences.unfavorable=param$SeedlingGrowth_0StopOr1Resume)
										if(sum(temp <- !thisSeedlingGrowth_AbsenceOfSnowCover[index.thisSeedlingSeason]) > 0) stopped_byCauses_onRYdoy["SeedlingMortality_DuringStoppedGrowth_DueSnowCover"] <- sg_RYdoy + which(temp)[1]
										#Minimum temperature
										thisSeedlingGrowth_AtAboveTmin <- calculate_SuitableGrowthThisYear_UnderCondition(favorable.conditions=thisSeedlingGrowing & thisYear_SeedlingGrowth_AtAboveTmin, consequences.unfavorable=param$SeedlingGrowth_0StopOr1Resume)
										if(sum(temp <- !thisSeedlingGrowth_AtAboveTmin[index.thisSeedlingSeason]) > 0) stopped_byCauses_onRYdoy["SeedlingMortality_DuringStoppedGrowth_DueTmin"] <- sg_RYdoy + which(temp)[1]
										#Maximum temperature
										thisSeedlingGrowth_AtBelowTmax <- calculate_SuitableGrowthThisYear_UnderCondition(favorable.conditions=thisSeedlingGrowing & thisYear_SeedlingGrowth_AtBelowTmax, consequences.unfavorable=param$SeedlingGrowth_0StopOr1Resume)
										if(sum(temp <- !thisSeedlingGrowth_AtBelowTmax[index.thisSeedlingSeason]) > 0) stopped_byCauses_onRYdoy["SeedlingMortality_DuringStoppedGrowth_DueTmax"] <- sg_RYdoy + which(temp)[1]
										#Updated days of growth or surviving
										thisSeedlingGrowing <- thisSeedlingGrowing & thisSeedlingGrowth_AbsenceOfSnowCover & thisSeedlingGrowth_AtAboveTmin & thisSeedlingGrowth_AtBelowTmax
										thisSeedlingLivingButNotGrowing <- !thisSeedlingGrowing
										if(sg_RYdoy > 1) thisSeedlingLivingButNotGrowing[1:(sg_RYdoy-1)] <- FALSE	#seedling germinated on sg_RYdoy, hence it cannot live before germination day
										
										#Book-keeping survival under above-ground conditions
										if(sum(temp <- thisYear_SeedlingMortality_UnderneathSnowCover[index.thisSeedlingSeason]) > 0) killed_byCauses_onRYdoy["SeedlingMortality_UnderneathSnowCover"] <- sg_RYdoy + which(temp)[1] - 1
										if(sum(temp <- thisYear_SeedlingMortality_ByTmin[index.thisSeedlingSeason]) > 0) killed_byCauses_onRYdoy["SeedlingMortality_ByTmin"] <- sg_RYdoy + which(temp)[1] - 1
										if(sum(temp <- thisYear_SeedlingMortality_ByTmax[index.thisSeedlingSeason]) > 0) killed_byCauses_onRYdoy["SeedlingMortality_ByTmax"] <- sg_RYdoy + which(temp)[1] - 1
										
										#If not killed (yet) then grow and check survival below-ground
										if(all(is.na(killed_byCauses_onRYdoy))){
											#Grow: estimate rooting depth for this seedling for each day of this year
											thisSeedling_thisYear_RootingDepth <- rep(NA, times=no.days)
											if((temp <- sum(thisSeedlingGrowing)) > 0){
												thisSeedlingGrowing_AgeDays <- 1:temp
												thisSeedlingGrowing_RootingDepth <- SeedlingRootingDepth(thisSeedlingGrowing_AgeDays, param$Seedling_SoilDepth.PO, param$Seedling_SoilDepth.K, param$Seedling_SoilDepth.r)
												thisSeedling_thisYear_RootingDepth[thisSeedlingGrowing] <- thisSeedlingGrowing_RootingDepth
												if(sum(thisSeedlingLivingButNotGrowing, na.rm=TRUE) > 0){ #for days when growth stopped then copy relevant soil depth
													stopg <- addDepths <- rle(thisSeedlingLivingButNotGrowing)
													RYDoys_stopg <- c(1, cumsum(stopg$lengths))
													for(p in seq(along=stopg$values)[stopg$values]){
														if(is.na(thisSeedling_thisYear_RootingDepth[RYDoys_stopg[p]])){
															if(is.na(thisSeedling_thisYear_RootingDepth[1 + RYDoys_stopg[p+1]])){
																add.values <- param$Seedling_SoilDepth.K
															} else {
																add.values <- thisSeedling_thisYear_RootingDepth[1 + RYDoys_stopg[p+1]]
															}
														} else {
															add.values <- thisSeedling_thisYear_RootingDepth[RYDoys_stopg[p]]
														}
														addDepths$values[p] <- add.values
													}
													RYDoys_addDepths <- inverse.rle(addDepths)
													thisSeedling_thisYear_RootingDepth <- ifelse(RYDoys_addDepths > 0, RYDoys_addDepths, thisSeedling_thisYear_RootingDepth)
												}
											} else {
												thisSeedling_thisYear_RootingDepth[thisSeedlingLivingButNotGrowing] <- param$Seedling_SoilDepth.PO/10
											}
											thisSeedling_thisYear_RootingSoilLayers <- SoilLayer_at_SoilDepth(thisSeedling_thisYear_RootingDepth)
											
											#Check survival under chronic SWPMax
											thisSeedling_thisYear_SeedlingMortality_ByChronicSWPMax <- get_KilledBySoilLayers(relevantLayers=thisSeedling_thisYear_RootingSoilLayers, kill.conditions=thisYear_SeedlingMortality_ByChronicSWPMax)
											if(sum(temp <- thisSeedling_thisYear_SeedlingMortality_ByChronicSWPMax[index.thisSeedlingSeason]) > 0) killed_byCauses_onRYdoy["SeedlingMortality_ByChronicSWPMax"] <- sg_RYdoy + which(temp)[1] - 1
											#Check survival under chronic SWPMin
											thisSeedling_thisYear_SeedlingMortality_ByChronicSWPMin <- get_KilledBySoilLayers(relevantLayers=thisSeedling_thisYear_RootingSoilLayers, kill.conditions=thisYear_SeedlingMortality_ByChronicSWPMin)
											if(sum(temp <- thisSeedling_thisYear_SeedlingMortality_ByChronicSWPMin[index.thisSeedlingSeason]) > 0) killed_byCauses_onRYdoy["SeedlingMortality_ByChronicSWPMin"] <- sg_RYdoy + which(temp)[1] - 1
											#Check survival under acute SWPMin
											thisSeedling_thisYear_SeedlingMortality_ByAcuteSWPMin <- get_KilledBySoilLayers(relevantLayers=thisSeedling_thisYear_RootingSoilLayers, kill.conditions=thisYear_SeedlingMortality_ByAcuteSWPMin)
											if(sum(temp <- thisSeedling_thisYear_SeedlingMortality_ByAcuteSWPMin[index.thisSeedlingSeason]) > 0) killed_byCauses_onRYdoy["SeedlingMortality_ByAcuteSWPMin"] <- sg_RYdoy + which(temp)[1] - 1
										}
										
										#If killed then establish which factor killed first and if and how growth was stopped before kill
										if(any(!is.na(killed_byCauses_onRYdoy))){
											kill.factor <- which.min(killed_byCauses_onRYdoy)
											SeedlingMortality_CausesByYear[y, kill.factor] <- SeedlingMortality_CausesByYear[y, kill.factor] + 1
											if(any(!is.na(stopped_byCauses_onRYdoy)) && (killed_byCauses_onRYdoy[kill.factor] > stopped_byCauses_onRYdoy[(stop.factor <- which.min(stopped_byCauses_onRYdoy))])){
												SeedlingMortality_CausesByYear[y, 6+stop.factor] <- SeedlingMortality_CausesByYear[y, 6+stop.factor] + 1
											}
											SeedlingSurvival_1stSeason[RYyear_ForEachUsedDay == RY.useyrs[y]][sg_RYdoy] <- FALSE
										}
									}
								} else {#no germination during this year -> no seedlings to grow or die
									SeedlingMortality_CausesByYear[y, ] <- NA
								}	
							}#end of year loop of seedling growth													
							
							#---Aggregate output
							temp1 <- data.frame(Germination_Emergence, SeedlingSurvival_1stSeason)
							temp2 <- data.frame(!Germination_AtBelowTmax, !Germination_AtAboveTmin, !Germination_AtMoreThanTopSWPmin, !Germination_DuringFavorableConditions, Germination_RestrictedByTimeToGerminate)
							
							#Fraction of years with success
							res[nv:(nv+1)] <- apply(temp <- (res1.yr <- aggregate(temp1, by=list(year_ForEachUsedRYDay), FUN=sum))[simTime$index.useyr, -1] > 0, MARGIN=2, FUN=mean)
							res[(nv+2):(nv+3)] <- apply(temp, MARGIN=2, FUN=sd)
							#Periods with no successes
							res[(nv+4):(nv+6)] <- quantile((rleGerm <- rle(temp[, 1]))$lengths[!rleGerm$values], probs=c(0.05, 0.5, 0.95))
							res[(nv+7):(nv+9)] <- quantile((rleSling <- rle(temp[, 2]))$lengths[!rleSling$values], probs=c(0.05, 0.5, 0.95))
							#Mean number of days per year with success
							res[(nv+10):(nv+11)] <- apply(res1.yr[simTime$index.useyr, -1], MARGIN=2, FUN=mean)
							res[(nv+12):(nv+13)] <- apply(res1.yr[simTime$index.useyr, -1], MARGIN=2, FUN=sd)
							#Days of year (in normal count) of most frequent successes among years: #toDoy <- function(x) sort(ifelse((temp <- x+Doy_SeedDispersalStart-1) > 365, temp-365,temp)) #convert to normal doys							
							res1.dy <- aggregate(temp1, by=list(doy_ForEachUsedRYDay), FUN=sum)[, -1]
							get.DoyMostFrequentSuccesses <- function(doys){
								res1.max <- sapply(1:2, FUN=function(x) quantile(doys[doys[, x]>0, x], probs=c(0.1, 1)))
								get.DoyAtLevel <- function(x, level) which(x == level & x > 0)
								if(all(!temp1[, 1])){#no successful germination
									germ.doy <- list(NA, NA)
								} else {
									germ.doy <- lapply(1:2, FUN=function(x) get.DoyAtLevel(doys[, 1], res1.max[x, 1]))
								}
								if(all(!temp1[, 2])){#no successful germination
									sling.doy <- list(NA, NA)
								} else {
									sling.doy <- lapply(1:2, FUN=function(x) get.DoyAtLevel(doys[, 2], res1.max[x, 2]))
								}
								res1.max <- list(germ.doy, sling.doy)
								return( unlist(lapply(res1.max, FUN=function(x) c(min(x[[1]]), median(x[[2]]), max(x[[1]])))) )
							}
							res[(nv+14):(nv+19)] <- get.DoyMostFrequentSuccesses(res1.dy)
							#Mean number of days when germination is restricted due to conditions
							res[(nv+20):(nv+24)] <- apply((res2.yr <- aggregate(temp2, by=list(year_ForEachUsedRYDay), FUN=sum))[simTime$index.useyr, -1], MARGIN=2, FUN=mean)
							res[(nv+25):(nv+29)] <- apply(res2.yr[simTime$index.useyr, -1], MARGIN=2, FUN=sd)
							#Mean time to germinate in days
							res[nv+30] <- mean((res3.yr <- aggregate(Germination_TimeToGerminate, by=list(year_ForEachUsedRYDay), FUN=mean, na.rm=TRUE))[simTime$index.useyr, -1], na.rm=TRUE)
							res[nv+31] <- sd(res3.yr[simTime$index.useyr, -1], na.rm=TRUE)
							#Mean number of days per year of different types of mortalities
							res[(nv+32):(nv+40)] <- apply(SeedlingMortality_CausesByYear, MARGIN=2, FUN=mean, na.rm=TRUE) #if value==NA, then no germinations that year
							res[(nv+41):(nv+49)] <- apply(SeedlingMortality_CausesByYear, MARGIN=2, FUN=sd, na.rm=TRUE) #if value==NA, then no germinations that year
							
							temp.header1 <- c(paste(rep(c("FractionYearsWith_GerminationSuccess", "FractionYearsWith_SeedlingSurvival1stSeason"), times=2), rep(c("", ".sd"), each=2), sep=""),
									paste(rep(c("SuccessiveYearsWith_NoGerminationSuccess", "SuccessiveYearsWith_NoSeedlingSurvival"), each=3), rep(c("5%", "50%", "95%"), times=2), "Quantile", sep="_"),
									paste(rep(c("MeanDaysWith_GerminationSuccess", "MeanDaysWith_SeedlingSurvival1stSeason"), times=2), rep(c("", ".sd"), each=2), sep=""),
									paste(rep(c("Start90%", "Median", "End90%"), times=2), rep(c("DoyMostFrequent_GerminationSuccess", "DoyMostFrequent_SeedlingSurvival1stSeason"), each=3), sep="_"),
									paste(rep(c("MeanDays_GerminationRestrictedByTmax", "MeanDays_GerminationRestrictedByTmin", "MeanDays_GerminationRestrictedBySWPmin", "MeanDays_GerminationRestrictedByAnyCondition", "MeanDays_GerminationRestrictedByTimeToGerminate"), times=2), rep(c("", ".sd"), each=5), sep=""),
									paste("MeanDays_TimeToGerminate", c("", ".sd"), sep=""),
									paste(rep(paste("MeanDays", colnames(SeedlingMortality_CausesByYear), sep="_"), times=2), rep(c("", ".sd"), each=9), sep=""))
							if(i==ifirst || makeOutputDB) resultfiles.Aggregates.header[nv:(nv+49)] <- paste(colnames(param.species_regeneration)[sp], temp.header1, sep="_")
							nv <- nv+50
							
							#---Aggregate time series output
							if(any(ouput_aggregated_ts=="Regeneration")){
								#Table with data for every year
								res1.yr.doy <- t(simplify2array(by(temp1, INDICES=year_ForEachUsedRYDay, FUN=function(x) get.DoyMostFrequentSuccesses(x))))[simTime$index.useyr, ]
								
								res.yr <- data.frame(data.frame(res1.yr, res2.yr[, -1], res3.yr[, -1])[simTime$index.useyr, ], SeedlingMortality_CausesByYear, res1.yr.doy)
								temp.header2 <- c("DaysWith_GerminationSuccess", "DaysWith_SeedlingSurvival1stSeason",
										"Days_GerminationRestrictedByTmax", "Days_GerminationRestrictedByTmin", "Days_GerminationRestrictedBySWPmin", "Days_GerminationRestrictedByAnyCondition", "Days_GerminationRestrictedByTimeToGerminate",
										"MeanDays_TimeToGerminate",
										paste("Days", colnames(SeedlingMortality_CausesByYear), sep="_"),
										paste(rep(c("Start90%", "Median", "End90%"), times=2), rep(c("DoyMostFrequent_GerminationSuccess", "DoyMostFrequent_SeedlingSurvival1stSeason"), each=3), sep="_"))
								colnames(res.yr) <- c("Year", temp.header2)
								write.csv(res.yr, file=paste(dir.at, .Platform$file.sep, "Scenario", formatC(sc-1, width=2, format="d", flag="0"), "_", climate.conditions[sc], "_", i_labels, "_", colnames(param.species_regeneration)[sp], "_Regeneration.csv", sep=""))
								
								#Plot with data for every day
								if(!exists("use_janus")){ #quartz() would need to plot differently on JANUS, i.e., without using a screen device, but I'm too lazy now to change this: changed to pdf(), now need to test on JANUS
									pdf(file=paste(dir.at, .Platform$file.sep, "Scenario", formatC(sc-1, width=2, format="d", flag="0"), "_", climate.conditions[sc], "_", i_labels, "_", colnames(param.species_regeneration)[sp], "_Regeneration.pdf", sep=""),
											width=max(4, 2*length(simTime$index.useyr)), height=4)
									op <- par(mar=c(1, 3, 0.1, 0.1), mgp=c(2, 0.5, 0), las=1)
									ylim <- c(-20, max(max(snow, na.rm=TRUE), max(Germination_TimeToGerminate, na.rm=TRUE)))
									p.cex <- max(0.5, min(1, exp(-0.01 * ylim[2]) + 0.5))
									xp <- 1:length(snow) + Doy_SeedDispersalStart-1
									plot(xp, snow, type="l", ylim=ylim, xlab="Year", ylab="SWE (mm), Time to germinate (days)", axes=FALSE)
									axis(1, pos=0, at=365*(1:(length(simTime$index.useyr))), labels=simTime$useyr)
									axis(2, pos=0, at=(temp <- axTicks(2))[temp>=0])
									lines(xp, Germination_TimeToGerminate, col="red", type="b", pch=19, cex=p.cex/10)
									points(xp, ifelse(SeedlingSurvival_1stSeason, 0, NA), col="green", pch=19)		
									x0.temp <- (temp <- data.frame(xp, ifelse(GerminationSuccess_Initiated, -10, NA)))[complete.cases(temp), ]
									x1.temp <- (temp <- data.frame(Germination_Emergence.doys + Doy_SeedDispersalStart-1, ifelse(GerminationSuccess_Initiated, -5, NA)))[complete.cases(temp), ]
									segments(x0=x0.temp[, 1], y0=x0.temp[, 2], x1=x1.temp[, 1], y1=x1.temp[, 2], col="blue")
									points(xp, ifelse(Germination_RestrictedByTimeToGerminate, -15, NA), col="black", pch=4, cex=p.cex)		
									points(xp, ifelse(!Germination_AtAboveTmin, -17.5, NA), col=gray(0.3), pch=4, cex=p.cex)		
									points(xp, ifelse(!Germination_AtMoreThanTopSWPmin, -20, NA), col=gray(0.7), pch=4, cex=p.cex)		
									mtext(i_labels)
									legend("topright", legend=c("SWE", "Time to germinate", "Seedling survival", "Emergence", "Too short favorable conditions", "Too cold", "Too dry"),
											bty="n", lty=c(1, 1, -1, 1, -1, -1, -1), pch=c(-1, -1, 19, -1, 4, 4, 4), col=c("black", "red", "green", "blue", "black", gray(0.3), gray(0.7)), merge=TRUE)
									par(op)
									dev.off()
								}
							}
							
							#Prepare next species
							prev.Doy_SeedDispersalStart <- Doy_SeedDispersalStart
						}#end of species loop
					}
					
					
					#temporaly save aggregate data
					out.temp <- cbind(header, res[1:(nv-1)])
					
					if(i==ifirst || makeOutputDB){
						colnames(out.temp) <- c(header.names, resultfiles.Aggregates.header[1:(nv-1)])
					}
					
					if(!makeOutputDB) write.csv(out.temp, file=filename.out.temp[sc], quote=FALSE, row.names=FALSE )
					
					if(makeOutputDB) mpi.send.Robj(out.temp, 1, 1, 1)
				} #end overall aggregation
				
				
				#Daily Output
				if(any(simulation_timescales=="daily") && daily_no > 0){
					
					#aggregate for each response variable
					for (doi in 1:daily_no) {
						
						if(!continueAfterAbort | (continueAfterAbort & !isdone.dailyAggs[doi, sc])){
							#check to see if we are on SWA
							if(regexpr("SWA", output_aggregate_daily[doi]) > 0){
								agg.resp <- "SWA"
								index.SWPcrit <- -as.numeric(sub("kPa", "", sub("SWAatSWPcrit", "", output_aggregate_daily[doi])))/1000
							} else {
								agg.resp <- output_aggregate_daily[doi]
							}
							
							agg.analysis <- switch(EXPR=agg.resp, AET=1, Transpiration=2, EvaporationSoil=1, EvaporationSurface=1, EvaporationTotal=1, VWC=2, SWC=2, SWP=2, SWA=2, Snowpack=1, Rain=1, Snowfall=1, Snowmelt=1, Infiltration=1, DeepDrainage=1, PET=1, TotalPrecipitation=1, TemperatureMin=1, TemperatureMax=1, SoilTemperature=2, Runoff=1)
							agg.no <- ifelse(agg.analysis == 1, 1, aggLs_no)
							
							res.dailyMean <- res.dailySD <- rep(NA, times=ifelse(agg.analysis == 1, 1, ifelse(AggLayer.daily, agg.no, SoilLayer_MaxNo)) * 366)
							
							scaler <- switch(EXPR=output_aggregate_daily[doi], SWP=1, VWC=1, TemperatureMin=1, TemperatureMax=1, SoilTemperature=1, 10) 	# SWP: -bar => MPa (but, since calculated via VWC, needs be same as VWC); VWC: # cm/cm -> m3/m3; default: cm => mm
							
							#read in data unless Exclude_ClimateAmbient
							if(!Exclude_ClimateAmbient) {
								if(agg.resp == "EvaporationTotal"){
									temp1 <- read.table(file = file.path(dir.sw.runs.sc.out[sc], evsoildy), header = FALSE, sep = "", fill = TRUE, comment.char="")
									temp2 <- read.table(file = file.path(dir.sw.runs.sc.out[sc], evapsurfacedy), header = FALSE, sep = "", fill = TRUE, comment.char="")
								} else {
									agg.file <- switch(EXPR=agg.resp,
											AET=aetdy,
											Transpiration=transpdy,
											EvaporationSoil=evsoildy,
											EvaporationSurface=evapsurfacedy,
											VWC=vwcdy,
											SWC=swcdy,
											SWP=vwcdy,
											SWA=swcdy,
											Snowpack=snowdy,
											Rain=precipdy,
											Snowfall=precipdy,
											Snowmelt=precipdy,
											Infiltration=inf_soildy,
											DeepDrainage=deepdraindy,
											PET=petdy,
											TotalPrecipitation=precipdy,
											TemperatureMin=tempdy,
											TemperatureMax=tempdy,
											SoilTemperature=soiltempdy,
											Runoff=runoffdy)
									temp1 <- read.table(file = file.path(dir.sw.runs.sc.out[sc], agg.file), header = FALSE, sep = "", fill = TRUE, comment.char="")
								}
								
								#extract data and aggregate into layers if requested
								agg.dat <- NULL
								if(agg.analysis == 1){ #no layers
									if( any(!is.na(match(agg.resp, c("AET", "EvaporationSurface", "Snowpack", "Rain", "Snowfall", "Snowmelt", "Infiltration", "DeepDrainage", "PET", "TotalPrecipitation", "TemperatureMin", "TemperatureMax","Runoff")))) ){
										agg.column <- switch(EXPR=agg.resp, AET=3, EvaporationSurface=3, Snowpack=3, Rain=4, Snowfall=5, Snowmelt=6, Infiltration=3, DeepDrainage=3, PET=3, TotalPrecipitation=3, TemperatureMin=4, TemperatureMax=3,Runoff=3)
										agg.dat[[1]] <- temp1[simTime$index.usedy, agg.column]
									}
									if(agg.resp == "EvaporationTotal"){
										if((colN <- ncol(temp1)) > 3){
											agg.dat[[1]] <- apply(temp1[simTime$index.usedy, 3:colN], 1, sum) + temp2[simTime$index.usedy, 3]
										} else {
											agg.dat[[1]] <- temp1[simTime$index.usedy, 3] + temp2[simTime$index.usedy, 3]
										}
									}
									if(agg.resp == "EvaporationSoil"){
										if((colN <- ncol(temp1)) > 3){
											agg.dat[[1]] <- apply(temp1[simTime$index.usedy, 3:colN], 1, sum)
										} else {
											agg.dat[[1]] <- temp1[simTime$index.usedy, 3]
										}
									}
								} else {#deal with soil layers: either each or 1-4 aggregated soil layers
									if( any(!is.na(match(agg.resp, c("VWC", "SWP", "SoilTemperature")))) ){ #aggregate by functions that are weighted by depths of soil layers
										agg.agg <- weighted.mean
										agg.w <- layers_width
									} else if( any(!is.na(match(agg.resp, c("Transpiration", "SWC", "SWA")))) ){#aggregate by simple functions
										agg.agg <- sum
										agg.w <- rep(0, times=length(layers_width))
									}
									for(al in 1:aggLs_no){
										if(length(aggLs[[al]]) > 1) {
											agg.dat[[al]] <- apply(temp1[simTime$index.usedy, 2 + aggLs[[al]]], 1, agg.agg, w=agg.w[aggLs[[al]]])
										} else {
											if(!(is.null(aggLs[[al]]) | length(aggLs[[al]]) == 0)) {
												agg.dat[[al]]  <- temp1[simTime$index.usedy, 2 + aggLs[[al]]]
											}
										}					
									}
								}
							}
						
							#calculate mean/SD daily values
							for(al in 1:agg.no){
								ir <- (al - 1) * 366 + 1:366
								res.dailyMean[ir] <- aggregate(scaler * agg.dat[[al]], by=list(simTime2$doy_ForEachUsedDay), FUN=mean)[, 2]
								if(agg.resp == "SWP"){ ##post-aggregate calculation of SWP: convert VWC to SWP
									res.dailyMean[ir] <- VWCtoSWP(res.dailyMean[ir], textureDAgg$sand[al], textureDAgg$clay[al])
									res.dailySD[ir] <- 0 #was NA now 0
								} else {
									res.dailySD[ir] <- aggregate(scaler * agg.dat[[al]], by=list(simTime2$doy_ForEachUsedDay), FUN=sd)[, 2]
								}
							}
							
							#post-aggregate calculation of SWA based on SWC for each SWPcrit
							if(agg.resp == "SWA"){								
								swc.swpcrit.layers <- layers_width * 10 * SWPtoVWC(index.SWPcrit, sand, clay)
								
								for(al in 1:agg.no){
									ir <- (al - 1) * 366 + 1:366
									
									if(length(aggLs[[al]]) > 1){
										swc.swpcrit <- sum(swc.swpcrit.layers[aggLs[[al]]])
									} else {
										swc.swpcrit <- swc.swpcrit.layers[aggLs[[al]]]
									}
									res.dailyMean[ir] <- ifelse((temp.res <- res.dailyMean[ir] - swc.swpcrit) > 0, temp.res, 0)	#SD is same as for SWC
								}
							}
							
							#temporary save daily data
							out.tempM <- t(c(header, res.dailyMean))
							out.tempSD <- t(c(header, res.dailySD))
							if(i==ifirst || makeOutputDB){
								if(agg.analysis == 1){
									colnames(out.tempM) <- colnames(out.tempSD) <- c(header.names, resultfiles.daily.labelsOne)
								} else {
									if(agg.resp == "SWA"){
										agg.labels <- paste(resultfiles.daily.labelsLayers, "_", sub("SWA", "", output_aggregate_daily[doi]), sep="")
									} else {
										agg.labels <- resultfiles.daily.labelsLayers
									}
									colnames(out.tempM) <- colnames(out.tempSD) <- c(header.names, agg.labels)
								}
							} else {
								colnames(out.tempM) <- colnames(out.tempSD) <- NULL
							}
							
							if(!makeOutputDB) write.csv(out.tempM, file=filename.out.temp.dailyMean[doi, sc], quote=FALSE, row.names=FALSE )
							#out.tempM <- c(agg.resp, "Mean", out.tempM)
							#colnames(out.tempM)[1:2] <- c("DailyOutput", "SD_or_Mean")
							if(!makeOutputDB) write.csv(out.tempSD, file=filename.out.temp.dailySD[doi, sc], quote=FALSE, row.names=FALSE )
							#out.tempSD <- c(agg.resp, "SD", out.tempSD)
							#colnames(out.tempSD)[1:2] <- c("DailyOutput", "SD_or_Mean")
							if(makeOutputDB) mpi.send.Robj(list(M=out.tempM, SD=out.tempSD, name=agg.resp, aggLs_no=aggLs_no), 1, 3, 1)
						}#end if continueAfterAbort
					}#doi loop
				}#end if daily output
			} #end loop through scenarios
		} #end if do aggregate
		
		
		#Remove output if requested
		if(deleteSoilWatFolderAfterAggregation | deleteSoilWatOutputAfterAggregation){
			for(sc in 1:scenario_No){
				
				if(deleteSoilWatFolderAfterAggregation) dir.remove(dir.sw.runs.sc[sc])
				
				if(deleteSoilWatOutputAfterAggregation & !deleteSoilWatFolderAfterAggregation){
					rm.files <- list.files(dir.sw.runs.sc.out[sc], full.names=TRUE)
					if(length(rm.files) > 0){
						if(!is.null(delete.exceptions))
							rm.files <- rm.files[!(basename(rm.files) %in% delete.exceptions)]
						file.remove(rm.files)
					}
				}
			}
		}
		
		#ETA estimation
		dt <- difftime(Sys.time(), time.sys, units="secs")
		times <- read.csv(file=file.path(dir.out, timerfile), header=FALSE, colClasses=c("NULL", "numeric"))
		if(!be.quiet) print(paste(i, ":", i_labels, "done in", round(dt, 2), units(dt), ":", round(nrow(times)/runsN.todo*100, 2), "% complete, ETA =", Sys.time()+ceiling((runsN.todo-(nrow(times)-1))/workersN)*mean(unlist(c(times, dt)), na.rm=TRUE) ))	
		write.table(dt, file=file.path(dir.out, timerfile), append=TRUE, sep=",", dec=".", col.names=FALSE)
		
#	} #end if Include_YN
	#return 1 to count total number of runs
	return(1)	
	
} #end do_OneSite()
#------------------------

FileHandler <- function(workers) {
	
	library(RSQLite)
	#Keep track of stuff
	WorkersDone <- 0
	dailyTableNames <- character()
	First <- TRUE
	
	#Create the Database
	drv <- dbDriver("SQLite")
	tfile <- file.path(dir.out, "dbTables.sql")
	con <- dbConnect(drv, dbname = tfile)
	if(makeInputForExperimentalDesign) {
		conExpiremtalInput <- dbConnect(drv, dbname = file.path(dir.out, "dbExperimentalInputDataFiles.sql"))
	}
	#data <- merge(data.frame(fileName=file), data, all.y=TRUE)
	AggOverallDataCols <- -1
	
	while(!(WorkersDone == workers)) {
		dataToWrite <- mpi.recv.Robj(mpi.any.source(), mpi.any.tag())
		task_info <- mpi.get.sourcetag()
		print(task_info[1])
		tag <- task_info[2]
		
		if(tag==1) { #Data to write to file
			if(First) {
				dataToWrite <- data.frame(dataToWrite[1,])
				print(dbWriteTable(con, "Aggregation_Overall", dataToWrite, row.names=FALSE))
				AggOverallDataCols <- dim(dataToWrite)[2]
				First<-FALSE
			} else {
				dataToWrite<-data.frame(dataToWrite[1,])
				print(dbWriteTable(con, "Aggregation_Overall", dataToWrite, row.names=FALSE, append=TRUE))
				#dbSendPreparedQuery(con, paste("INSERT INTO Aggregation_Overall (", paste(columnNames, sep="", collapse = ", "), ") VALUES (", paste(dataToWrite, collapse=",", sep=""), ")", sep=""))
			}
		} else if (tag==2) { #tag 2 is for excluded current scenario with NA values
			if(First) { #crap this is not good
				#put in tempary variable until a real first is done
				if(!exists("AggOverTemp")) {
					AggOverTemp <- dataToWrite #should just include info columns 1:(scenarios)
				} else {
					AggOverTemp <- rbind(AggOverTemp,dataToWrite)
				}
			} else { #perfect just put in table with NA for data past scenario col
				if(exists("AggOverTemp")) {#We need to write out what we have to file already
					temp<-data.frame(matrix(NA, nrow=dim(AggOverTemp)[1], ncol=AggOverallDataCols-dim(AggOverTemp)[2]))
					print(dbWriteTable(con, "Aggregation_Overall", cbind(AggOverTemp, temp), row.names=FALSE, append=TRUE))
					rm(AggOverTemp, temp) #make sure we remove these
				}
				#now write out the data
				print(dbWriteTable(con, "Aggregation_Overall", cbind(dataToWrite, data.frame(matrix(NA, nrow=1, ncol=AggOverallDataCols-dim(dataToWrite)[2])) ), row.names=FALSE, append=TRUE))
			}
		} else if (tag == 3) {
			if(any(dataToWrite$name == dailyTableNames)) {
				MdataToWrite <- data.frame(dataToWrite$M[1,])
				SDdataToWrite <- data.frame(dataToWrite$SD[1,])
				
				print(dbWriteTable(con, paste("Aggregation_Seasons_DailyValues_Mean_", dataToWrite$name, sep=""), MdataToWrite, row.names=FALSE, append=TRUE))
				print(dbWriteTable(con, paste("Aggregation_Seasons_DailyValues_SD_", dataToWrite$name, sep=""), SDdataToWrite, row.names=FALSE, append=TRUE))
			} else {
				dailyTableNames <- c(dailyTableNames, dataToWrite$name)
				MdataToWrite <- data.frame(dataToWrite$M[1,])
				SDdataToWrite <- data.frame(dataToWrite$SD[1,])
				print(dbWriteTable(con, paste("Aggregation_Seasons_DailyValues_Mean_", dataToWrite$name, sep=""), MdataToWrite, row.names=FALSE))
				print(dbWriteTable(con, paste("Aggregation_Seasons_DailyValues_SD_", dataToWrite$name, sep=""), SDdataToWrite, row.names=FALSE))
			}
		} else if (tag == 4) {
			if( length(dbListTables(conExpiremtalInput)) > 0 ) {
				print(dbWriteTable(conExpiremtalInput, "SWRunInformation", dataToWrite$SWRunInformation, row.names=FALSE, append=TRUE))
				print(dbWriteTable(conExpiremtalInput, "sw_input_soillayers", dataToWrite$sw_input_soillayers, row.names=FALSE, append=TRUE))
				print(dbWriteTable(conExpiremtalInput, "sw_input_treatments", dataToWrite$sw_input_treatments, row.names=FALSE, append=TRUE))
				print(dbWriteTable(conExpiremtalInput, "sw_input_cloud", dataToWrite$sw_input_cloud, row.names=FALSE, append=TRUE))
				print(dbWriteTable(conExpiremtalInput, "sw_input_prod", dataToWrite$sw_input_prod, row.names=FALSE, append=TRUE))
				print(dbWriteTable(conExpiremtalInput, "sw_input_site", dataToWrite$sw_input_site, row.names=FALSE, append=TRUE))
				print(dbWriteTable(conExpiremtalInput, "sw_input_soils", dataToWrite$sw_input_soils, row.names=FALSE, append=TRUE))
				print(dbWriteTable(conExpiremtalInput, "sw_input_weather", dataToWrite$sw_input_weather, row.names=FALSE, append=TRUE))
				print(dbWriteTable(conExpiremtalInput, "sw_input_climscen", dataToWrite$sw_input_climscen, row.names=FALSE, append=TRUE))
				print(dbWriteTable(conExpiremtalInput, "sw_input_climscen_values", dataToWrite$sw_input_climscen_values, row.names=FALSE, append=TRUE))
			} else { #need to create the tables
				#tables <- names(dataToWrite)
				print(dbWriteTable(conExpiremtalInput, "SWRunInformation", dataToWrite$SWRunInformation, row.names=FALSE))
				print(dbWriteTable(conExpiremtalInput, "sw_input_soillayers", dataToWrite$sw_input_soillayers, row.names=FALSE))
				print(dbWriteTable(conExpiremtalInput, "sw_input_treatments", dataToWrite$sw_input_treatments, row.names=FALSE))
				print(dbWriteTable(conExpiremtalInput, "sw_input_cloud", dataToWrite$sw_input_cloud, row.names=FALSE))
				print(dbWriteTable(conExpiremtalInput, "sw_input_prod", dataToWrite$sw_input_prod, row.names=FALSE))
				print(dbWriteTable(conExpiremtalInput, "sw_input_site", dataToWrite$sw_input_site, row.names=FALSE))
				print(dbWriteTable(conExpiremtalInput, "sw_input_soils", dataToWrite$sw_input_soils, row.names=FALSE))
				print(dbWriteTable(conExpiremtalInput, "sw_input_weather", dataToWrite$sw_input_weather, row.names=FALSE))
				print(dbWriteTable(conExpiremtalInput, "sw_input_climscen", dataToWrite$sw_input_climscen, row.names=FALSE))
				print(dbWriteTable(conExpiremtalInput, "sw_input_climscen_values", dataToWrite$sw_input_climscen_values, row.names=FALSE))
			}
		} else if (tag == 5) {
			print("Worker Done")
			WorkersDone <- WorkersDone + 1
		}
	}
	#cleanup Remove unused soil layers
	for(i in seq(dailyTableNames)) {
		agg.analysis <- switch(EXPR=dailyTableNames[i], AET=1, Transpiration=2, EvaporationSoil=1, EvaporationSurface=1, EvaporationTotal=1, VWC=2, SWC=2, SWP=2, SWA=2, Snowpack=1, Rain=1, Snowfall=1, Snowmelt=1, Infiltration=1, DeepDrainage=1, PET=1, TotalPrecipitation=1, TemperatureMin=1, TemperatureMax=1, SoilTemperature=2, Runoff=1)
		if(agg.analysis == 2) {
			temp1 <- dbReadTable(con, paste("Aggregation_Seasons_DailyValues_Mean_", dailyTableNames[i], sep=""))
			temp2 <- dbReadTable(con, paste("Aggregation_Seasons_DailyValues_SD_", dailyTableNames[i], sep=""))
			temp1 <- temp1[,colSums(is.na(temp1))<nrow(temp1)]
			temp2 <- temp2[,colSums(is.na(temp2))<nrow(temp2)]
			dbRemoveTable(con, paste("Aggregation_Seasons_DailyValues_Mean_", dailyTableNames[i], sep=""))
			dbRemoveTable(con, paste("Aggregation_Seasons_DailyValues_SD_", dailyTableNames[i], sep=""))
			print(dbWriteTable(con, paste("Aggregation_Seasons_DailyValues_Mean_", dailyTableNames[i], sep=""), temp1, row.names=FALSE))
			print(dbWriteTable(con, paste("Aggregation_Seasons_DailyValues_SD_", dailyTableNames[i], sep=""), temp2, row.names=FALSE))
		}
	}
	
	dbDisconnect(con)
	mpi.send.Robj(0,0,3)
}

work <- function(parallel_backend, Data) {
	# Note the use of the tag for sent messages:
	#     1=ready_for_task, 2=done_task, 3=exiting
	# Note the use of the tag for received messages:
	#     1=task, 2=done_tasks
	
	#The First slave needs to get the data and write to file
	if(!makeOutputDB) { #each slave will test this condition and only the first slave will do something else
		junk <- 0
		done <- 0
		while (done != 1) {
			# Signal being ready to receive a new task
			mpi.send.Robj(junk,0,1)
			
			# Receive a task
			dataForRun <- mpi.recv.Robj(mpi.any.source(),mpi.any.tag())
			task_info <- mpi.get.sourcetag()
			tag <- task_info[2]
			
			if (tag == 1) {
				print(dataForRun$i)
				if(dataForRun$do_OneSite) results <- do_OneSite(i=dataForRun$i, i_labels=dataForRun$labels, i_SWRunInformation=dataForRun$SWRunInformation, i_sw_input_soillayers=dataForRun$sw_input_soillayers, i_sw_input_treatments=dataForRun$sw_input_treatments, i_sw_input_cloud=dataForRun$sw_input_cloud, i_sw_input_prod=dataForRun$sw_input_prod, i_sw_input_site=dataForRun$sw_input_site, i_sw_input_soils=dataForRun$sw_input_soils, i_sw_input_weather=dataForRun$sw_input_weather, i_sw_input_climscen=dataForRun$sw_input_climscen, i_sw_input_climscen_values=dataForRun$sw_input_climscen_values)
				if(dataForRun$collect_ResultsWithTemporaryDataFrame) collect_ResultsWithTemporaryDataFrame(resultfile=dataForRun$resultfiles.toConcatenate, filelist=dataForRun$theFileList, col.names=dataForRun$col.names, cleanup=dataForRun$deleteTemporaryAggregationFiles)
				# Send a results message back to the master
				#print(results)
				mpi.send.Robj(dataForRun$i,0,2)
			} else if (tag == 2) {
				done <- 1
			}
			# We'll just ignore any unknown messages
		}
		mpi.send.Robj(junk,0,3)
		#mpi.send.Robj(junk,1,3)
	} else {
		if(mpi.comm.rank() != 1) {
			junk <- 0
			done <- 0
			while (done != 1) {
				# Signal being ready to receive a new task
				mpi.send.Robj(junk,0,1)
				
				# Receive a task
				dataForRun <- mpi.recv.Robj(mpi.any.source(),mpi.any.tag())
				task_info <- mpi.get.sourcetag()
				tag <- task_info[2]
				
				if (tag == 1) {
					print(dataForRun$i)
					if(dataForRun$do_OneSite) results <- do_OneSite(i=dataForRun$i, i_labels=dataForRun$labels, i_SWRunInformation=dataForRun$SWRunInformation, i_sw_input_soillayers=dataForRun$sw_input_soillayers, i_sw_input_treatments=dataForRun$sw_input_treatments, i_sw_input_cloud=dataForRun$sw_input_cloud, i_sw_input_prod=dataForRun$sw_input_prod, i_sw_input_site=dataForRun$sw_input_site, i_sw_input_soils=dataForRun$sw_input_soils, i_sw_input_weather=dataForRun$sw_input_weather, i_sw_input_climscen=dataForRun$sw_input_climscen, i_sw_input_climscen_values=dataForRun$sw_input_climscen_values)
					if(dataForRun$collect_ResultsWithTemporaryDataFrame) collect_ResultsWithTemporaryDataFrame(resultfile=dataForRun$resultfiles.toConcatenate, filelist=dataForRun$theFileList, col.names=dataForRun$col.names, cleanup=dataForRun$deleteTemporaryAggregationFiles)
					# Send a results message back to the master
					#print(results)
					mpi.send.Robj(dataForRun$i,0,2)
				} else if (tag == 2) {
					done <- 1
				}
				# We'll just ignore any unknown messages
			}
			mpi.send.Robj(junk,0,3)
			mpi.send.Robj(junk,1,5)
		} else {
			FileHandler(workers = (mpi.comm.size() - 2))
		}
		
	}
}

#--------------------------------------------------------------------------------------------------#
#------------------------RUN THE FRAMEWORK TASKS IN PARALLEL OR SERIAL

#identify which SoilWat-runs = rows are to be carried out
seq.tr <- (1:trow)[include_YN > 0]	# sequence of row numbers in the master and treatment input files that are included
seq.todo <- 1:(runs * ifelse(trowExperimentals > 0, trowExperimentals, 1)) # consecutive number of all (tr x exp) simulations to be executed
runsN.todo <- length(seq.todo)

#parallelization
if(runsN.todo > 0){
	if(parallel_runs){
		if(!be.quiet) print(paste("SWSF prepares parallelization: started at", t1 <- Sys.time()))
		if(identical(parallel_backend, "mpi")) {
			mpi.spawn.Rslaves(nslaves=num_cores)
			
			exportObjects <- function(allObjects) {
				print("exporting objects from master node to slave nodes")
				t.bcast <- Sys.time()
				for(obj in 1:length(allObjects)) {
					bcast.tempString <- allObjects[obj]
					bcast.tempValue <- try(eval(as.name(allObjects[obj])))
					mpi.bcast.Robj2slave(bcast.tempString)
					mpi.bcast.Robj2slave(bcast.tempValue)
					mpi.bcast.cmd(cmd=try(assign(bcast.tempString, bcast.tempValue)))
				}
				print(paste("object export took", round(difftime(Sys.time(), t.bcast, units="secs"), 2), "secs"))
			}
		}
		
		if(identical(parallel_backend, "snow")){
			if(exists("use_janus")){
				print("janus exists")
				cl <-  makeCluster(num_cores, type="MPI", outfile="")
				print("cluster made")
			} else if(!exists("use_janus")){
				if(!be.quiet) setDefaultClusterOptions(outfile="")
				#cl <-  makeCluster(num_cores, type="MPI", outfile="")
				cl <- snow::makeSOCKcluster(num_cores)
				#snow::clusterSetupRNG(cl) #random numbers setup
			}
			doSNOW::registerDoSNOW(cl) 	# register foreach backend
			snow::clusterEvalQ(cl, library(circular)) 	#load any packages necessary for do_OneSite(): none as of July 24, 2012
		}
		
		if(identical(parallel_backend, "multicore")) {
			#stop("Only use snow on JANUS, because multicore cannot access cores outside master node")
			registerDoMC(num_cores)
		}
		
		if(identical(parallel_backend, "mpi")){
			workersN <- (mpi.comm.size() - 1)
		} else {
			workersN <- foreach::getDoParWorkers()
		}
		if(!be.quiet) print(paste("SWSF prepares parallelization: ended after",  round(difftime(Sys.time(), t1, units="secs"), 2), "s"))
	} else {
		workersN <- 1
	}
}

if(!identical(actions, "concatenate") & runsN.todo > 0){
	ifirst <- seq.todo[1]
	
	#objects to export
	list.noexport <- c("include_YN", "labels", "SWRunInformation", "sw_input_soillayers", "sw_input_treatments", "sw_input_cloud", "sw_input_prod", "sw_input_site", "sw_input_soils", "sw_input_weather", "sw_input_climscen", "sw_input_climscen_values")
	list.export <- (temp <- ls())[-match(list.noexport, temp, nomatch=0)]
	
	#ETA calculation
	if(!be.quiet) print(paste("SWSF simulation runs:", runsN.todo, "out of", trow * ifelse(trowExperimentals==0, 1, trowExperimentals), " runs will be carried out on", workersN, "cores: started at", t1 <- Sys.time()))
	
	if(parallel_runs){
		#call the simulations depending on parallel backend
		if(identical(parallel_backend, "mpi")) {
			workersN <- (mpi.comm.size() - 1)
			exportObjects(list.export)
			#mpi.bcast.cmd(library(Node_Libraries))
			mpi.bcast.cmd(work(parallel_backend = "mpi", Data = 0))
			junk <- 0
			closed_slaves <- 0
			runs.completed <- 0
			#sTag <- c("Ready for task", "Done with Task", "Exiting")
			while(closed_slaves < workersN) {
				complete <- mpi.recv.Robj(mpi.any.source(),mpi.any.tag())
				complete_info <- mpi.get.sourcetag()
				slave_id <- complete_info[1]
				tag <- complete_info[2]
				#print(paste("From:", slave_id, "tag:", sTag[tag], "Message:", complete))
				
				if (tag == 1) {
					# slave is ready for a task. Give it the next task, or tell it tasks
					# are done if there are none.
					if (runs.completed < runsN.todo) {
						# Send a task, and then remove it from the task list
						i_tr <- seq.tr[(1+runs.completed - 1) %% runs + 1]
						i_labels <- labels[i_tr]
						i_SWRunInformation <- SWRunInformation[i_tr, ]
						i_sw_input_soillayers <- sw_input_soillayers[i_tr, ]
						i_sw_input_treatments <- sw_input_treatments[i_tr, ]
						i_sw_input_cloud <- sw_input_cloud[i_tr, ]
						i_sw_input_prod <- sw_input_prod[i_tr, ]
						i_sw_input_site <- sw_input_site[i_tr, ]
						i_sw_input_soils <- sw_input_soils[i_tr, ]
						i_sw_input_weather <- sw_input_weather[i_tr, ]
						i_sw_input_climscen <- sw_input_climscen[i_tr, ]
						i_sw_input_climscen_values <- sw_input_climscen_values[i_tr, ]
						dataForRun <- list(do_OneSite=TRUE, collect_ResultsWithTemporaryDataFrame=FALSE, i=(1+runs.completed), labels=i_labels, SWRunInformation=i_SWRunInformation, sw_input_soillayers=i_sw_input_soillayers, sw_input_treatments=i_sw_input_treatments, sw_input_cloud=i_sw_input_cloud, sw_input_prod=i_sw_input_prod, sw_input_site=i_sw_input_site, sw_input_soils=i_sw_input_soils, sw_input_weather=i_sw_input_weather, sw_input_climscen=i_sw_input_climscen, sw_input_climscen_values=i_sw_input_climscen_values)
						mpi.send.Robj(dataForRun, slave_id, 1);
						print(paste("Slave:", slave_id, "Run:", (runs.completed+1), "started at", Sys.time()))
						runs.completed <- runs.completed + 1
					} else {
						mpi.send.Robj(junk, slave_id, 2)
					}
				}
				else if (tag == 2) {
					# The message contains results. Do something with the results.
					# Store them in the data structure
					#print(paste("Run: ", complete, "at", Sys.time()))
				}
				else if (tag == 3) {
					# A slave has closed down.
					closed_slaves <- closed_slaves + 1
					print(paste("Slave Closed:", slave_id))
				}
			}
			print(runs.completed)
		}
		if(identical(parallel_backend, "snow")){
			snow::clusterExport(cl, list.export)
			runs.completed <- foreach(i_sim=seq.todo, .combine="+", .inorder=FALSE) %dopar% {
				i_tr <- seq.tr[(i_sim - 1) %% runs + 1]
				do_OneSite(i=i_sim, i_labels=labels[i_tr], i_SWRunInformation=SWRunInformation[i_tr, ], i_sw_input_soillayers=sw_input_soillayers[i_tr, ], i_sw_input_treatments=sw_input_treatments[i_tr, ], i_sw_input_cloud=sw_input_cloud[i_tr, ], i_sw_input_prod=sw_input_prod[i_tr, ], i_sw_input_site=sw_input_site[i_tr, ], i_sw_input_soils=sw_input_soils[i_tr, ], i_sw_input_weather=sw_input_weather[i_tr, ], i_sw_input_climscen=sw_input_climscen[i_tr, ], i_sw_input_climscen_values=sw_input_climscen_values[i_tr, ])
			}
		}
		if(identical(parallel_backend, "multicore")){
			runs.completed <- foreach(i_sim=seq.todo, .combine="+", .inorder=FALSE, .noexport=list.noexport) %dopar% {
				i_tr <- seq.tr[(i_sim - 1) %% runs + 1]
				do_OneSite(i=i_sim, i_labels=labels[i_tr], i_SWRunInformation=SWRunInformation[i_tr, ], i_sw_input_soillayers=sw_input_soillayers[i_tr, ], i_sw_input_treatments=sw_input_treatments[i_tr, ], i_sw_input_cloud=sw_input_cloud[i_tr, ], i_sw_input_prod=sw_input_prod[i_tr, ], i_sw_input_site=sw_input_site[i_tr, ], i_sw_input_soils=sw_input_soils[i_tr, ], i_sw_input_weather=sw_input_weather[i_tr, ], i_sw_input_climscen=sw_input_climscen[i_tr, ], i_sw_input_climscen_values=sw_input_climscen_values[i_tr, ])
			}
		}
		
	} else { #call the simulations in seriel
		runs.completed <- foreach(i_sim=seq.todo, .combine="+", .inorder=FALSE, .noexport=list.noexport) %do% {
			i_tr <- seq.tr[(i_sim - 1) %% runs + 1]
			do_OneSite(i=i_sim, i_labels=labels[i_tr], i_SWRunInformation=SWRunInformation[i_tr, ], i_sw_input_soillayers=sw_input_soillayers[i_tr, ], i_sw_input_treatments=sw_input_treatments[i_tr, ], i_sw_input_cloud=sw_input_cloud[i_tr, ], i_sw_input_prod=sw_input_prod[i_tr, ], i_sw_input_site=sw_input_site[i_tr, ], i_sw_input_soils=sw_input_soils[i_tr, ], i_sw_input_weather=sw_input_weather[i_tr, ], i_sw_input_climscen=sw_input_climscen[i_tr, ], i_sw_input_climscen_values=sw_input_climscen_values[i_tr, ])
		}
	}
	if(!be.quiet) print(paste("SWSF simulation runs: completed with", runs.completed, "runs: ended after",  round(difftime(Sys.time(), t1, units="secs"), 2), "s"))
} else {
	runs.completed <- 0
}
#------------------------
#print(runs.completed)

#--------------------------------------------------------------------------------------------------#
#------------------------CHECK COMPLETENESS OF OUTPUT FILES AND SIMULATIONS
t.check <- Sys.time()

if(checkCompleteness){
	if(!be.quiet) print(paste("SWSF checks simulations and output: started at", t.check))
	
	if(exists("theredone")){
		
		res <- matrix(NA, ncol=2, nrow=0)
		vec <- vector(length=2)
		
		dirt <- "/Volumes/DRS4_BigData2/3_20120720_PC_GlobalDrylandEcoHydro_PrelimSim"
		
		for(d in dirlist){
			vec <- c(basename(d), length(list.files(file.path(dirt, d), recursive=TRUE, include.dirs=TRUE)))
			res <- rbind(res, vec)
		}
		
		
		
		target<-paste(rep(t$Label, each=7), c("Current", as.character(unlist(t[1, 13:18]))), sep="_")
		index<-match(target, as.character(s[,1]))
		target[which(is.na(index))]
	}
	
	
	#check SoilWat simulation directories: number of runs and number of files in each directory
	if(any(actions=="create") | any(actions=="execute")){
		
	} else {
		complete.simulations <- TRUE	#assume all is good
	}
	
	#check temporary output aggregation file number
	if(any(actions=="aggregate")){
		
	} else {
		complete.aggregations <- TRUE	#assume all is good
	}
	
	
	all.complete <- complete.aggregations & complete.simulations	#do check here: still need to implement
	if(!all.complete) print("Not all simulations or aggregations were successful: please, check logs")
} else {
	all.complete <- TRUE	#assume all is good
}

#timing of check
delta.check <- difftime(Sys.time(), t.check, units="secs")
if(!be.quiet & checkCompleteness) print(paste("SWSF checks simulations and output: ended after", round(delta.check, 2), "s"))



#--------------------------------------------------------------------------------------------------#
#------------------------COLLECT AND CONCATENATE SINGLE RESULT FILES INTO FINAL OUTPUT FILES
t.concatenation <- Sys.time()	#timing of file concatenation
if(any(actions=="concatenate") & all.complete & runs.completed == runsN.todo & !makeOutputDB){
	if(!be.quiet) print(paste("SWSF concatenates temporary results: started at", t.concatenation))
	
	#collect and concatenate results into files: read, temporarily store in data.frame, and write to file at end (potentially big data.frame generation, but not much disk writing)
	collect_ResultsWithTemporaryDataFrame <- function(resultfile, filelist, col.names=TRUE, cleanup=FALSE){
		if(cleanup) try(file.remove(resultfile), silent=TRUE)
		
		if(length(filelist) == 0 || !file.exists(filelist[1])) {
			print(paste(basename(resultfile), ": no or not enough temporary files to collect results from"))
			return(0)
		} else {
			print(paste(basename(resultfile), ": concatenation started at", Sys.time()))
		}
		
		f.temp <- read.csv(filelist[1])	#option: row.names=1
		maxCol <- dim(f.temp)[2]
		data.temp <- matrix(data=NA, nrow=length(filelist), ncol=ncol(f.temp))
		if(all(col.names == TRUE)){
			colnames(data.temp) <- colnames(f.temp)
		} else {
			colnames(data.temp) <- col.names
		}
		data.temp[1, ] <- t(f.temp)
		
		if((no.files <- length(filelist)) > 1) for(f in 2:no.files) {
			f.temp <- read.csv(filelist[f])
			data.temp[f, ] <- t(f.temp[,1:maxCol])
		}
		
		icol.allEmpty <- apply(data.temp, MARGIN=2, FUN=function(x) sum(is.na(x)) == nrow(data.temp))
		if(sum(icol.allEmpty) > 0) data.temp <- data.temp[, !icol.allEmpty]	#delete all empty columns, e.g. for the daily aggregates if all soil layers are selected --> code cannot know how many soil layers there will be, so use all, and then (here) delete superfluous columns
		
		written <- try( write.csv(data.temp, file=resultfile, row.names=FALSE) )
		
		if(!identical(class(written), "try-error")){
			if(cleanup) try(file.remove(filelist), silent=TRUE)
			return(1)
		} else {
			return(0)
		}
	}
	getMatches <- function(filelist, pattern, targetN) {
		temp <- filelist[grepl(pattern, filelist, fixed=TRUE)]
		if(length(temp) != targetN) temp <- NULL
		return(temp)
	}
		
	#Determine how many concatenations needs to be done
	resultfiles.toConcatenate <- c(unlist(resultfiles.Aggregates))
	if(any(simulation_timescales=="daily") & daily_no > 0) resultfiles.toConcatenate <- c(resultfiles.toConcatenate, unlist(resultfiles.dailyMean), unlist(resultfiles.dailySD))
	seq.concats <- 1:length(resultfiles.toConcatenate)
	count.AllConcats <- length(resultfiles.toConcatenate) + exinfo$EstimateConstantSoilTemperatureAtUpperAndLowerBoundaryAsMeanAnnualAirTemperature + exinfo$EstimateInitialSoilTemperatureForEachSoilLayer + ifelse(trowExperimentals > 0, makeInputForExperimentalDesign, 0)
	
	#Compile list of temporary output files to concatenate
	checkList.theFileList <- list.files(path=dir.out, recursive=FALSE)
	do.theFileList.create <- TRUE
	if(filename.theFileList %in% checkList.theFileList){
		theFileList <- read.csv(file=file.path(dir.out, filename.theFileList), colClasses="character")[, 1]
		if(length(theFileList) != runsN.todo * count.AllConcats){#file on disk is incorrect
			if(!be.quiet) print("TheFileList on disk contains an unexpected number of files. TheFileList will be recreated.")
			file.rename(from=file.path(dir.out, filename.theFileList), to=file.path(dir.out, sub(".csv", "_old.csv", filename.theFileList)))
		} else {
			do.theFileList.create <- FALSE
		}
	}
	if(do.theFileList.create){
		theFileList <- list.files(path=dir.out.temp, full.names=TRUE, recursive=TRUE, include.dirs=FALSE)
		write.csv(theFileList, file=file.path(dir.out, filename.theFileList), row.names=FALSE)
	}
	if(length(theFileList) != runsN.todo * count.AllConcats)#theFileList is incorrect
		if(!be.quiet) print("TheFileList contains an unexpected number of files. Not all concatenations will be successful.")
	
	#Concatenate
	if(makeInputForExperimentalDesign && trowExperimentals > 0 && length(create_experimentals) > 0) {
		ExpInputFiles <- getMatches(filelist=theFileList, pattern="Experimental_InputData_All.csv", targetN=runsN.todo)
		print(paste("Experimental Input Data: concatenation started at", Sys.time()))
		
		SWRunInformation <- matrix(data="", nrow=runsN.todo, ncol=ncol(SWRunInformation), dimnames = list(NULL, colnames(SWRunInformation)))
		sw_input_soillayers <-  matrix(data="", nrow=runsN.todo, ncol=ncol(sw_input_soillayers), dimnames = list(NULL, colnames(sw_input_soillayers)))
		sw_input_treatments <-  matrix(data="", nrow=runsN.todo, ncol=ncol(sw_input_treatments), dimnames = list(NULL, colnames(sw_input_treatments)))
		sw_input_cloud <-  matrix(data="", nrow=runsN.todo, ncol=ncol(sw_input_cloud), dimnames = list(NULL, colnames(sw_input_cloud)))
		sw_input_prod <-  matrix(data="", nrow=runsN.todo, ncol=ncol(sw_input_prod), dimnames = list(NULL, colnames(sw_input_prod)))
		sw_input_site <-  matrix(data="", nrow=runsN.todo, ncol=ncol(sw_input_site), dimnames = list(NULL, colnames(sw_input_site)))
		sw_input_soils <-  matrix(data="", nrow=runsN.todo, ncol=ncol(sw_input_soils), dimnames = list(NULL, colnames(sw_input_soils)))
		sw_input_weather <-  matrix(data="", nrow=runsN.todo, ncol=ncol(sw_input_weather), dimnames = list(NULL, colnames(sw_input_weather)))
		sw_input_climscen <-  matrix(data="", nrow=runsN.todo, ncol=ncol(sw_input_climscen), dimnames = list(NULL, colnames(sw_input_climscen)))
		sw_input_climscen_values <-  matrix(data="", nrow=runsN.todo, ncol=ncol(sw_input_climscen_values), dimnames = list(NULL, colnames(sw_input_climscen_values)))
		
		for(i in 1:length(ExpInputFiles)) {
			infilename <- file.path(ExpInputFiles[i])
			infiletext <- readLines(con = infilename)
			
			SWRunInformation[i,] <- (unlist(strsplit(infiletext[1], split=ExpInput_Seperator)))
			sw_input_soillayers[i,] <- (unlist(strsplit(infiletext[2], split=ExpInput_Seperator)))
			sw_input_treatments[i,] <- (unlist(strsplit(infiletext[3], split=ExpInput_Seperator)))
			sw_input_cloud[i,] <- (unlist(strsplit(infiletext[4], split=ExpInput_Seperator)))
			sw_input_prod[i,] <- (unlist(strsplit(infiletext[5], split=ExpInput_Seperator)))
			sw_input_site[i,] <- (unlist(strsplit(infiletext[6], split=ExpInput_Seperator)))
			sw_input_soils[i,] <- (unlist(strsplit(infiletext[7], split=ExpInput_Seperator)))
			sw_input_weather[i,] <- (unlist(strsplit(infiletext[8], split=ExpInput_Seperator)))
			sw_input_climscen[i,] <- (unlist(strsplit(infiletext[9], split=ExpInput_Seperator)))
			sw_input_climscen_values[i,] <- (unlist(strsplit(infiletext[10], split=ExpInput_Seperator)))
			
			if(deleteTemporaryAggregationFiles) try(file.remove(infilename), silent=TRUE)
		}
		write.csv(SWRunInformation, file=file.path(dir.out.experimentalInput, paste("EXP_", datafile.SWRunInformation, sep="")), row.names=FALSE)
		write.csv(sw_input_soillayers, file=file.path(dir.out.experimentalInput, paste("EXP_", datafile.soillayers, sep="")), row.names=FALSE)
		write.csv(rbind(sw_input_treatments_use, sw_input_treatments), file=file.path(dir.out.experimentalInput, paste("EXP_", datafile.treatments, sep="")), row.names=FALSE)
		write.csv(rbind(sw_input_prod_use, sw_input_prod), file=file.path(dir.out.experimentalInput, paste("EXP_", datafile.prod, sep="")), row.names=FALSE)
		write.csv(rbind(sw_input_site_use, sw_input_site), file=file.path(dir.out.experimentalInput, paste("EXP_", datafile.siteparam, sep="")), row.names=FALSE)
		write.csv(rbind(sw_input_soils_use, sw_input_soils), file=file.path(dir.out.experimentalInput, paste("EXP_", datafile.soils, sep="")), row.names=FALSE)
		write.csv(rbind(sw_input_weather_use, sw_input_weather), file=file.path(dir.out.experimentalInput, paste("EXP_", datafile.weathersetup, sep="")), row.names=FALSE)
		write.csv(rbind(sw_input_climscen_use, sw_input_climscen), file=file.path(dir.out.experimentalInput, paste("EXP_", datafile.climatescenarios, sep="")), row.names=FALSE)
		write.csv(rbind(sw_input_climscen_values_use, sw_input_climscen_values), file=file.path(dir.out.experimentalInput, paste("EXP_", datafile.climatescenarios_values, sep="")), row.names=FALSE)
	}
#	if(!makeInputForExperimentalDesign && trowExperimentals > 0 && length(create_experimentals) > 0) {
#		if(any(create_experimentals == "LookupEvapCoeffFromTable") | any(create_experimentals == "LookupTranspRegionsFromTable")) {
#			try.cat <- collect_ResultsWithTemporaryDataFrame(resultfile=(temp.file <- file.path(dir.out, "Experimental Input Data",  paste("EXP_", datafile.soils, sep=""))), filelist=getMatches(filelist=theFileList, pattern=paste("EXP_", datafile.soils, sep=""), targetN=runsN.todo), col.names=TRUE, cleanup=deleteTemporaryAggregationFiles)
#			if(try.cat > 0){
#				sw_input_soils <- read.csv(temp)
#				tempdat <- rbind(sw_input_soils_use, sw_input_soils)
#				write.csv(tempdat, file=file.path(dir.out, "Experimental Input Data", paste("EXP_", datafile.soils, sep="")), row.names=FALSE)
#				rm(tempdat)
#			}
#		}
#		if(any(create_experimentals == "LookupSnowDensityFromTable")) {
#			try.cat <- collect_ResultsWithTemporaryDataFrame(resultfile=(temp.file <- file.path(dir.out, "Experimental Input Data",  paste("EXP_", datafile.cloud, sep=""))), filelist=getMatches(filelist=theFileList, pattern=paste("EXP_", datafile.cloud, sep=""), targetN=runsN.todo), col.names=TRUE, cleanup=deleteTemporaryAggregationFiles)
#			if(try.cat > 0){
#				sw_input_cloud <- read.csv(temp)
#				tempdat <- rbind(sw_input_cloud_use, sw_input_cloud)
#				write.csv(tempdat, file=file.path(dir.out, "Experimental Input Data", paste("EXP_", datafile.cloud, sep="")), row.names=FALSE)
#				rm(tempdat)
#			}
#		}
#	}
	if(trowExperimentals == 0 && runs.completed == runsN.todo && exinfo$EstimateConstantSoilTemperatureAtUpperAndLowerBoundaryAsMeanAnnualAirTemperature){#store soil temperature at lower boundary into datafile
		try.cat <- collect_ResultsWithTemporaryDataFrame(resultfile=(temp.file <- file.path(dir.out.temp, "SoilTempC_atLowerBoundary.csv")), filelist=getMatches(filelist=theFileList, pattern="SoilTempC_atLowerBoundary", targetN=runsN.todo), col.names=TRUE, cleanup=deleteTemporaryAggregationFiles)
		if(try.cat > 0){
			temp.soilT <- read.csv(temp.file)
			try(file.remove(temp.file), silent=TRUE)
			#write data to datafile.siteparam
			temp.index <- match(temp.soilT[, 1], seq.tr)	#this doesn't work with trowExperimentals > 0
			sw_input_site[, "SoilTempC_atUpperBoundary"][temp.index] <- temp.soilT[, 3]
			sw_input_site[, "SoilTempC_atLowerBoundary"][temp.index] <- temp.soilT[, 4]
			tempdat <- rbind(sw_input_site_use, sw_input_site)
			if(makeInputForExperimentalDesign && trowExperimentals > 0 && length(create_experimentals) > 0) write.csv(tempdat, file=file.path(dir.out.experimentalInput, paste("EXP_", datafile.siteparam, sep="")), row.names=FALSE)
			else write.csv(tempdat, file=file.path(dir.sw.dat, datafile.siteparam), row.names=FALSE)
			
			rm(try.cat, temp.soilT, temp.index, tempdat)
		}
	}
	
	if(trowExperimentals == 0 && runs.completed == runsN.todo && exinfo$EstimateInitialSoilTemperatureForEachSoilLayer){#store initial soil temperature into datafile
		try.cat <- collect_ResultsWithTemporaryDataFrame(resultfile=(temp.file <- file.path(dir.out.temp, "SoilTempC_InitProfile.csv")), filelist=getMatches(filelist=theFileList, pattern="SoilTempC_InitProfile", targetN=runsN.todo), col.names=TRUE, cleanup=deleteTemporaryAggregationFiles)
		if(try.cat > 0){
			temp.soilT <- read.csv(temp.file)
			try(file.remove(temp.file), silent=TRUE)
			#write data to datafile.soils
			temp.index <- match(temp.soilT[, 1], seq.tr)	#this doesn't work with trowExperimentals > 0
			ld <- 1:SoilLayer_MaxNo
			
			use.layers <- which(sw_input_soils_use[match(paste("SoilTemp_L", ld, sep=""), colnames(sw_input_soils_use))] == 1)[1:(ncol(temp.soilT)-2)]
			index.soilTemp <- match(paste("SoilTemp_L", ld, sep=""), colnames(sw_input_soils_use))[use.layers]
			sw_input_soils[temp.index, index.soilTemp] <- ifelse(makeInputForExperimentalDesign, as.matrix(temp.soilT[, 3:ncol(temp.soilT)]), temp.soilT[, 3:ncol(temp.soilT)])
			tempdat <- rbind(sw_input_soils_use, sw_input_soils)
			if(makeInputForExperimentalDesign && trowExperimentals > 0 && length(create_experimentals) > 0) write.csv(tempdat, file=file.path(dir.out.experimentalInput, paste("EXP_", datafile.soils, sep="")), row.names=FALSE)
			else write.csv(tempdat, file=file.path(dir.sw.dat, datafile.soils), row.names=FALSE)
			rm(try.cat, temp.soilT, temp.index, ld, use.layers, index.soilTemp, tempdat)
		}
	}
	
	if(runs.completed == runsN.todo & any(actions=="aggregate") | identical(actions, "concatenate")){
		if(parallel_runs){
			#objects to export
			list.export <- c("collect_ResultsWithTemporaryDataFrame", "resultfiles.toConcatenate", "getMatches", "theFileList", "deleteTemporaryAggregationFiles", "seq.concats", "runsN.todo")
			list.noexport <- (temp <- ls())[-match(list.export, temp, nomatch=0)]			
			
			if(identical(parallel_backend, "mpi")) {
				exportObjects(list.export)
				concats.completed <- mpi.parLapply(seq.concats, function(i) collect_ResultsWithTemporaryDataFrame(resultfile=resultfiles.toConcatenate[i], filelist=getMatches(filelist=theFileList, pattern=basename(resultfiles.toConcatenate[i]), targetN=runsN.todo), col.names=TRUE, cleanup=deleteTemporaryAggregationFiles))
				concats.completed <- sum(as.numeric(unlist(concats.completed)), na.rm=TRUE)
			}
			if(identical(parallel_backend, "snow")){
				snow::clusterExport(cl, list.export)
				concats.completed <- foreach(i = seq.concats, .combine="+", .inorder=FALSE) %dopar% collect_ResultsWithTemporaryDataFrame(resultfile=resultfiles.toConcatenate[i], filelist=getMatches(filelist=theFileList, pattern=basename(resultfiles.toConcatenate[i]), targetN=runsN.todo), col.names=TRUE, cleanup=deleteTemporaryAggregationFiles)
			}
			if(identical(parallel_backend, "multicore")){
				concats.completed <- foreach(i = seq.concats, .combine="+", .inorder=FALSE, .noexport=list.noexport) %dopar% collect_ResultsWithTemporaryDataFrame(resultfile=resultfiles.toConcatenate[i], filelist=getMatches(filelist=theFileList, pattern=basename(resultfiles.toConcatenate[i]), targetN=runsN.todo), col.names=TRUE, cleanup=deleteTemporaryAggregationFiles)
			}
		} else {
			concats.completed <- foreach(i = seq.concats, .combine="+", .inorder=FALSE, .noexport=list.noexport) %do% collect_ResultsWithTemporaryDataFrame(resultfile=resultfiles.toConcatenate[i], filelist=getMatches(filelist=theFileList, pattern=basename(resultfiles.toConcatenate[i]), targetN=runsN.todo), col.names=TRUE, cleanup=deleteTemporaryAggregationFiles)
		}
		
		if(concats.completed != length(resultfiles.toConcatenate)) print(paste("Not all concatenations were successful:", concats.completed, "instead of", length(resultfiles.toConcatenate)))
		
				
	} else if(runs.completed != runsN.todo){
		print(paste("The 'foreach' simulation loop ran not often enough for concatenation:", runs.completed, "instead of", runsN.todo))
	}
}

#timing of file concatenation
delta.concatenation <- difftime(Sys.time(), t.concatenation, units="secs")
if(!be.quiet & any(actions=="concatenate")) print(paste("SWSF concatenates temporary results: ended after", round(delta.concatenation, 2), "s"))


#--------------------------------------------------------------------------------------------------#
#------------------------ENSEMBLE GENERATION
t.ensembles <- Sys.time()	#timing of ensemble calculation

if(do.ensembles && any(actions=="concatenate") && all.complete && runs.completed == runsN.todo &&
	((concats.completed == length(resultfiles.toConcatenate) && !makeOutputDB) || makeOutputDB)){
	
	if(!be.quiet) print(paste("SWSF calculates ensembles: started at", t.ensembles))

	#Function to calculate the quantiles for an array of the data for each scenario (stacked in 3rd dim)
	doEnsembleQuantilesOverData <- function(data.temp, probs){
		doQuantile <- function(x, probs){
			quantile(x, prob=probs, type=3, na.rm=TRUE)
		}
		
		if(parallel_runs){
			if(identical(parallel_backend, "mpi")) {
				mpi.bcast.Robj2slave(obj=data.temp)
				mpi.bcast.Robj2slave(obj=doQuantile)
				mpi.bcast.Robj2slave(obj=probs)
				return(list <- mpi.parApply(data.temp[,,], MARGIN = c(1,2), doQuantile, probs=probs))
			}
			if(identical(parallel_backend, "snow")){
				return(parApply(cl, data.temp[,,], MARGIN = c(1,2), doQuantile, probs=probs)) #snow library
			}
			if(identical(parallel_backend, "multicore")){
				ifelse(dim(data.temp)[3] == 1, list <- foreach(i = 1:nrow(data.temp)) %dopar% lapply(data.temp[i,,], doQuantile, probs=probs) ,list <- foreach(i = 1:nrow(data.temp)) %dopar% apply(data.temp[i,,], MARGIN=1, doQuantile, probs=probs))
				return(array(data=unlist(list), dim=c(length(x), dim(data.temp)[2], length(list))))
			}
		} else {
			return(apply(data.temp[,,], MARGIN = c(1,2), doQuantile, probs=probs))
		}
	}


	if(!makeOutputDB) {
			#Define ensemble function
			collect_EnsembleFromScenarios <- function(outputs, probs, filelist){
				countWrite <- 1
				
				doWrite <- function(x){
					#add info to 
					name<-ensemble.families[sapply(ensemble.families, function(d) grepl(pattern=d, x=outputs[countWrite]))]
					cAdd<-data.frame(matrix(data=c(rep(name,nrow(f.temp)), rep(ensemble.quantiles[countWrite],nrow(f.temp))), nrow=nrow(f.temp), ncol = 2 ))
					names(cAdd)<- c("EnsembleName", "Quantile")
					headerInfo <- cbind(headerInfo, cAdd)
					if(parallel_runs && identical(parallel_backend, "multicore")){
						qData <- t(x) #this is needed for MC
						colnames(qData) <- col.names
						data <- cbind(headerInfo, qData)
					}else
						data <- cbind(headerInfo, x)
					
					written <- try(write.csv(data, file=outputs[countWrite], row.names=FALSE))
					countWrite <<- countWrite + 1
					if(!identical(class(written), "try-error")){
						return(1)
					} else {
						return(0)
					}
				}
				
				if(length(filelist) == 0 || !file.exists(filelist[1])) {
					print(": not enough scenario files to collect results from or bad path")
					return(0)
				}
				
				f.temp <- read.csv(filelist[1])
				#store scenario information for just the first one
				columnCutoff <- match("Scenario", colnames(f.temp))
				columns <- ncol(f.temp)
				#want to remove scenario column replace with one for ensemble family name and quantile
				headerInfo <- f.temp[,1:(columnCutoff-1)]
				col.names <- colnames(f.temp[,-(1:columnCutoff)])
				data.temp <- list()
				data.temp[[1]] <- f.temp[,-(1:columnCutoff)]
				
				if((no.files <- length(filelist)) > 1) for(f in 2:no.files){
						f.temp <- read.csv(filelist[f], colClasses=c(rep("NULL", columnCutoff), rep("character", columns-columnCutoff)))
						data.temp[[f]] <- f.temp
					}
				#put list into array
				data.temp <- array(data=unlist(data.temp), dim=c(nrow(data.temp[[1]]), ncol(data.temp[[1]]), length(data.temp)) )
				colnames(data.temp) <- col.names
				class(data.temp) <- "numeric"
				#check quantiles
				nfiles <- sum(unlist(apply(doEnsembleQuantilesOverData(data.temp, probs=probs), MARGIN=1, doWrite)))
				
				return(nfiles)
			}
			
			#Get ready for doing the ensembles
			nfiles <- total.files <- 0
			for(ios in 1:nrow(ensembles.maker$outputs)){
				for(ifs in 1:ncol(ensembles.maker$outputs)){
					nfiles <- collect_EnsembleFromScenarios(outputs=ensembles.maker$outputs[ios, ifs, ], probs=ensemble.quantiles, filelist=ensembles.maker$scenarioFiles[ios, ifs, ])
					total.files <- total.files + nfiles
					if(nfiles != length(ensemble.quantiles)) print(paste("Something went wrong with ensemble output:", basename(ensembles.maker$outputs[ios, ifs, 1])))
				}
			}
			if(total.files != length(unlist(ensembles.maker$outputs))) print("Something went wrong with ensemble output.")

	} else {# if makeOutputDB == TRUE
	
		collect_EnsembleFromScenarios <- function(Table, ensemble.family, probs, conEnsembleDB){
			countWrite <- 1
		
			doWrite <- function(x){
				#add info to 
				name<-ensemble.family
				cAdd<-data.frame(matrix(data=c(rep(name,nrow(data.temp)), rep(ensemble.quantiles[countWrite],nrow(data.temp))), nrow=nrow(data.temp), ncol = 2 ))
				names(cAdd)<- c("EnsembleName", "Quantile")
				headerInfo <- cbind(header, cAdd)
				if(parallel_runs && identical(parallel_backend, "multicore")){
					qData <- t(x) #this is needed for MC
					colnames(qData) <- col.names
					data <- cbind(headerInfo, qData)
				}else
					data <- cbind(headerInfo, x)
			
				written <- dbWriteTable(conEnsembleDB, name=paste(name, "_", formatC(ensemble.quantiles[countWrite]*100, digits = 0, format = "f"), "_", Table, sep=""), data, row.names=FALSE)
				countWrite <<- countWrite + 1
				if(written)
					return(1)
				else
					return(0)
			}
		
			sqlString <- paste("SELECT * FROM ", Table, " WHERE Scenario LIKE '", ensemble.family, "%'", sep="")
			res <- dbSendQuery(con, sqlString)
			dataToQuantilize <- fetch(res, n=-1) #get the data from the query n=-1 to get all rows
			dbClearResult(res)
			columnCutoff <- match("Scenario", colnames(dataToQuantilize))
			header <- lapply(unique(dataToQuantilize$Scenario), function (x) dataToQuantilize[dataToQuantilize$Scenario == x,(1:columnCutoff)])
			header <- header[[1]]
			#We have all the scenarios in the family. We need to get unique scenario names and group them by that
			data.temp <- lapply(unique(dataToQuantilize$Scenario), function (x) dataToQuantilize[dataToQuantilize$Scenario == x,-(1:columnCutoff)])
			data.temp <- array(data=unlist(data.temp), dim=c(nrow(data.temp[[1]]), ncol(data.temp[[1]]), length(data.temp)) )
			colnames(data.temp) <- colnames(dataToQuantilize[,-(1:columnCutoff)])
			class(data.temp) <- "numeric"
			#check quantiles
			nfiles <- sum(unlist(apply(doEnsembleQuantilesOverData(data.temp, probs), MARGIN=1, doWrite)))
		
			return(nfiles)
		}
		library(RSQLite)
		drv <- dbDriver("SQLite")
		tfile <- file.path(dir.out, "dbTables.sql")
		con <- dbConnect(drv, dbname = tfile)
	
		Tables <- dbListTables(con) #get a list of tables
	
		dir.out.ensemble.sql <- dir.out
		tfile <- file.path(dir.out.ensemble.sql, "dbTablesEnsembles.sql")
		conEnsembleDB <- dbConnect(drv, dbname=tfile)
	
		for(i in 1:length(Tables)) {
			for(j in 1:length(ensemble.families)) {
				collect_EnsembleFromScenarios(Tables[i], ensemble.families[j], ensemble.quantiles, conEnsembleDB)
			}
		}
	}
}

#timing of ensemble calculation
delta.ensembles <- difftime(Sys.time(), t.ensembles, units="secs")
if(!be.quiet && any(actions=="concatenate") && do.ensembles) print(paste("SWSF calculates ensembles: ended after", round(delta.ensembles, 2), "s"))


#--------------------------------------------------------------------------------------------------#
#------------------------OVERALL TIMING
delta.overall <- difftime(Sys.time(), t.overall, units="secs")
if(!be.quiet) print(paste("SWSF: ended after", round(delta.overall, 2), "s"))

write.timer <- function(label, time_sec){ write.table(t(c(label, time_sec)), file=file.path(dir.out, timerfile2), append=TRUE, sep=",", dec=".", col.names=FALSE, row.names=FALSE) }

write.timer("Time_Total", delta.overall)
write.timer("Time_Check", delta.check)
write.timer("Time_FileConcatenation", delta.concatenation)
write.timer("Time_Ensembles", delta.ensembles)

if(!identical(actions, "concatenate")){
	times <- as.numeric(unlist(read.csv(file=file.path(dir.out, timerfile), header=FALSE, colClasses=c("NULL", "numeric"), skip=1)))
	write.timer("Time_OneRun_Mean", mean(times))
	write.timer("Time_OneRun_SD", sd(times))
	write.timer("Time_OneRun_Median", median(times))
	write.timer("Time_OneRun_Min", min(times))
	write.timer("Time_OneRun_Max", max(times))
}

if(!be.quiet) print(paste("SWSF: ended with actions =", paste(actions, collapse=", "), "at", Sys.time()))


#--------------------------------------------------------------------------------------------------#
#------------------------CODE CLEANUP

options(ow)	#sets the warning option to its previous value

if(parallel_runs & identical(parallel_backend, "mpi")) {	#clean up mpi slaves
	#mpi.close.Rslaves(dellog=FALSE)
	mpi.quit()
}
if(parallel_runs & identical(parallel_backend, "snow")) snow::stopCluster(cl)	#clean up snow cluster


#rm(list=ls(all=TRUE))	#optional

#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#
