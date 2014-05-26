#--------------------------------------------------------------------------------------------------#

#------------------------FRAMEWORK FOR SOILWAT SIMULATIONS: CREATING SIMULATION RUNS, EXECUTING SIMULATIONS, AND AGGREGATING OUTPUTS

#--------------------------------------------------------------------------------------------------#

#------CODE developed and written by
# - Daniel R Schlaepfer (dschlaep@uwyo.edu, drs): 2009-2013
# - Donovan Miller (dlm): 2012
# - Ryan Murphy (rjm): 2012-2013
#for contact and further information see also: sites.google.com/site/drschlaepfer

#The R code below was tested on R version 3.0.2

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
#	v49 (20121102-20130801)
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
#		- (drs) increased precision of climate change values: changed number of digits of monthly PPT and T scalers written to weatherin from 2 to 4 (differences between input and output could be larger than 0.5%)
#		- (drs) fixed bug in how 'experimental design' is assigned to the underlaying inputs: subsetting threw error if multiple columns didn't match beside at least a matching one. (Bug in R? that data.frame[i, c(0,0,3,0)]  <- 3 shows error of 'duplicate subscripts for columns')
#		- (drs) fixed bug in climate scenario creation: error due to wrong dimensions in scalors if treatments of ClimateScenario_{Temp, PPT}_PerturbationInMeanSeasonalityBothOrNone were 'None'
#		- (drs) fixed bug and numerical instability in circ.mean: wrong cycle for x=int+1 yielded 0 instead of int; rounding error for x=366 if int=365 yielded 366 instead of 365
#		- (drs) fixed bug in treatment option 'Vegetation_Biomass_Scaling': no scaling occured if 'Vegetation_Biomass_ScalingSeason_AllGrowingORNongrowing' was 'All'
#		- (drs) fixed bug in creation of soilsin from input datafile: writing to file of impermeability was done after rounding to 0 digits, thereby missing all content except 0 and 1
#		- (drs) aggregation options 'dailySWPextremes', 'dailyTranspirationExtremes', 'dailyTotalEvaporationExtremes', and 'dailyDrainageExtremes', 'dailyAETExtremes': adapted that the mean instead of the first of multiple extreme days is used
#		- (drs) aggregation options 'dailyTranspirationExtremes', 'dailyTotalEvaporationExtremes', 'dailyDrainageExtremes', and 'dailyAETExtremes': fixed a index bug in the header
#		- (drs) aggregation option 'RainOnSnowOfMAP': added SD
#		- (drs) aggregation option 'dailySWPdrynessIntensity': added mean of the amount of dry-period missing water, as well as the duration and the number
#		- (drs) fixed bug in aggregation options 'dailySWPdrynessEventSizeDistribution' and 'dailySWPdrynessIntensity': used xx.dy.all instead of xx.dy (yearly aggregations are based on xx.dy-formatted data)
#		- (drs) generalized functions 'start10days' and 'end10days' (only used in aggregation option 'dailySWPdrynessANDwetness'), i.e., removed exlcusion of dry periods in first 90 days, generalized from fix 10 days to n days periods; replaced with 'startDoyOfDuration', 'endDoyAfterDuration'
#		- (drs) added aggregation option 'monthlySPEIEvents': duration and intensity of the standardized precipitation-evapotranspiration index at different scales
#		- (drs) renamed 'SWCtot' -> 'SWC' and 'SWCvol' -> 'VWC'
#		- (drs) updated n_variables to represent actual numbers of aggregations: apparently, in the past, when aggregation options were added, the update of n_variables was forgotten
#		- (drs) fixed bug in 'PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996': if all fractions set, but didn't quite sum up to 1 (due to that fractions in prodin are only written with 3 digits), then error was thrown
#		- (drs) added information on number of runs, scenarios, concatenations, ensembles, and workers to the overall timing file
#		- (drs) adjusted 'adjustLayersDepth': included a round(, 0) because the wrapper only handles 1-cm resolution of soil depths (maily because of the trco)
#		- (drs) aggregation option 'dailyWeatherEventSizeDistribution": re-formulated counts per year to frequencies per year, added mean count per year and SD to output (adjusted n_variables by +4)
#		- (drs) aggregation option 'dailySWPdrynessEventSizeDistribution": re-formulated counts per year to frequencies per year, added mean count per year and SD to output (adjusted n_variables by +4 per icrit)
#		- (drs) added action 'ensemble': as separate from concatenation
#		- (drs) re-organized 'ensemble's:
#					- ensemble levels are now taken by ranks instead of quantiles (type 3, even order statistics); they are taken individually for each variable=column
#					- ensembles are taken directly for all response variables except for SD which are the ones associated at the scenario-level with the selected ensemble variable
#					- i.e., all output must be organized in two files: one for the means, etc., and one file for the SDs organized in the same columns --> overall aggregated output is now split up in two files like the daily aggregations already were
#					- option 'save.scenario.ranks' if TRUE then for each ensemble.levels an additional file is generated with the scenario numbers corresponding to the ensemble.levels
#		- (drs) re-arranged overall aggregated output: it is now split up in two files (means and others, optional SDs), like the daily aggregations already were
#					- Column names always consist of three parts:
#						1. variable name: sortable,
#							i.e., most common name parts go first,
#							if monthly then at the end m1 ... m12, separated by "."
#							if for top and bottom layers, then ".topLayers", ".bottomLayers"
#						2. units: "_mm", "_fraction", "_TF" (true or false), etc. if unitless then "_none"
#						3. type of aggregation: "_mean", "_sd", "_const", "_median", "_quantilePROBS", etc.
#		- (drs) fixed bug in some of the aggregations if bottomL == 0
#		- (drs) fixed bug in create 'control transpiration regions for adjusted soil depth and rooting depth' if number of soil layers == 1
#		- (drs) fixed bug in some of the aggregations if topL == 1
#		- (drs) changed functions 'circ.' {mean, range, sd}: if all elements of x are NA and na.rm==TRUE, then output was 'numeric(0)' which caused aggregated output variables from numeric vectors into a list; changed that these functions now put out NA instead of 'numeric(0)'
#		- (drs) faster version of 'ExtractGriddedDailyWeatherFromMaurer2002_NorthAmerica'
#		- (drs) deleted empty line(s) in soilsin -> r wrapper hangs with empty last line
#		- (drs) added option 'print.debug' to print statements about code advancement (may be useful for debugging)
#		- (drs) fixed bug in 'dailyRegeneration_byTempSWPSnow': if only one soil layer, then variable 'swp' needs to be forced to be a matrix
#		- (drs) fixed bug in 'SiteClimate': MAT was taken as mean of monthly values which biased the result; instead the function is now correctly taking the mean of the mean annual temperature values
#		- (drs) added code that reports if SoilWat execution or 'ExtractGriddedDailyWeatherFromMaurer2002_NorthAmerica' failed
#		- (drs) fixed bug in 'PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996': if no vegetation was estimated for the components to be estimated and the fixed ones summed up to 0, then the result contained NA: now, vegetation is forced > 0
#		- (drs) fixed bug in section of concatenation: if actions included concatenation and ensemble, but not aggregation, then no concatenation occurred mistakenly
#		- (drs) added option 'concatenation.inMemory': #concatenation.inMemory: all temp output is loaded into a giant data.frame, then written to final file; if !concatenation.inMemory, temp output is read and immediately appended to final file
#		- (drs) renamed function 'collect_ResultsWithTemporaryDataFrame' to 'concatenate_TemporaryResultFiles'
#		- (drs) added two functions 'concatenate_TemporaryResultFiles': one to concatenate in memory (as before), one to concatenate via append (new); these are selected via option 'concatenation.inMemory'
#		- (drs) fixed bug in 'concatenate_TemporaryResultFiles': if cleanup and concatenation resumed, then already final files deleted
#	v50 (20130801-20130909)
#		- (rjm) fixed database creation so page size is max and using REAL instead of double.
#		- (rjm) Each node writes out a sql file with all the data from run. This is loaded into the dbTables.db database after runs are finished
#		- (rjm) Ensembles via database now works in with Rmpi. A database for each scenario table is generated with its ensembles.
#		- (rjm) The weather is loaded from a database by the master node and gives it to slave node via memory OR it can look it up in files
#		- (rjm) The soilwat setup files are loaded in memory via Rsoilwat structures objects and passed to the slaves who then work with that instead of files.
#		- (rjm) daily values are sorted to match database.
#		- (rjm) fixed SWA output only outputing for one crit
#		- (drs) 'deleteSoilWatFolderAfterAggregation' set to FALSE: requests SoilWat input/output to be stored on disk
#		- (drs) 'yearlyWaterBalanceFluxes': fixed column names (percolation and hydred were switched)
#		- (drs) experimental design can replace information from the prodin datafile
#		- (drs) added overall aggregation option 'dailyFrostInSnowfreePeriod'
#		- (drs) fixed temporary db output if create_treatments == "Exclude_ClimateAmbient": superfluous end of line and header was factor values instead of characters
#		- (drs) fixed adding treatment/experimental information to siteparam: flags were vectors of 0/1 and thus not suitable to subset
#		- (drs) added overall aggregation option 'input_TranspirationCoeff'
#		- (drs) fixed TranspCoeffByVegType(): read in .csv table in two pieces: tr_input_TranspCoeff and tr_input_TranspCoeff_Code, so that tr_input_TranspCoeff is numeric and there can be no errors translating levels into numbers
#		- (drs) fixed max.duration(): circumvented that 'no non-missing arguments to max'
#		- (drs) fixed aggregation of 'input_TranspirationCoeff': if there is only one aggLs or no bottomL
#		- (drs) fixed get_Response_aggL(): transp and hydred output is for each soil layer for total and 3 vegtypes; # soil layer calculation was incorrect
#		- (drs) added to aggregation 'input_FractionVegetationComposition': C3, C4, and annual-grass fractions
#		- (drs) fixed create:soils: comparison of soil layer structure
#		- (drs) adjusted wrapper function circ.sd() because circular::sd.circular() can return NaN instead of 0 [in packageVersion("circular") <= "0.4.7"]
#		- (drs) scale TranspCoeffByVegType() to 1 as SoilWat does: co/sum(co)
#		- (drs) added output option 'input_SoilProfile'
#		- (drs) added 'adjustType' option to function TranspCoeffByVegType(): with 'positive' as recommended for grasses (built-in Soilwat) and 'inverse' as recommended for woody plants and forbs
#		- (drs) added output options 'dailyRechargeExtremes' and 'dailyHotDays'; added option for multiple Tmin values in 'dailyFrostInSnowfreePeriod'
#		- (drs) added output options 'dailySuitablePeriodsDuration', 'dailySuitablePeriodsAvailableWater', and 'dailySuitablePeriodsDrySpells'
#		- (drs) fixed 'startDoyOfDuration' and 'endDoyOfDuration': picked first/last period of length of first suitable period instead of picking first/last suitable period of length of first/last suitable period

#--------------------------------------------------------------------------------------------------#
#------------------------PREPARE SOILWAT SIMULATIONS

if(!be.quiet) print(paste("SWSF is executed for:", sQuote(basename(dir.prj)), "and started at", Sys.time()))

.Last <- function() { #Properly end mpi slaves before quitting R (e.g., at a crash)
	if (is.loaded("mpi_initialize") && exists("mpi.comm.size")){
		if (mpi.comm.size(1) > 0) mpi.close.Rslaves()
		.Call("mpi_finalize")
	}
}

#------
actionWithSoilWat <- any(actions == "create") || any(actions == "execute") || any(actions == "aggregate")
actionWithSWSFOutput <- any(actions == "concatenate") || any(actions == "ensemble")
#--order output_aggregate_daily--#
if(length(output_aggregate_daily) > 0) output_aggregate_daily <- output_aggregate_daily[order(output_aggregate_daily)]
#------
ow <- options("warn", "error")
if(print.debug){
	options(warn=2, error=quote({dump.frames(to.file=TRUE); q()}))	#turns all warnings into errors, dumps all to a file, and quits
} else {
	options(warn=0, error=traceback)	#catches all warnings and on error returns a traceback()
}

#made this function b/c dir.create wasn't always working correctly on JANUS for some reason... so if the simulations are being run on JANUS then it uses the system mkdir call to make the directories.
dir.create2 <- function(path, showWarnings = TRUE, recursive = FALSE, mode = "0777", times = 0) { 
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
dir.sw.in <- normalizePath(dir.sw.in)
if(makeInputForExperimentalDesign) dir.out.experimentalInput <- file.path(dir.out, "Experimentals_Input_Data")
dir.out.temp <- file.path(dir.out, "temp")
dir.create2(dir.out, showWarnings=FALSE, recursive=TRUE)
dir.create2(dir.runs, showWarnings=FALSE, recursive=TRUE)
if(saveSoilWatInputOutput) dir.create2(dir.sw.runs, showWarnings=FALSE, recursive=TRUE)
dir.create2(dir.out.temp, showWarnings=FALSE, recursive=TRUE)
if(makeInputForExperimentalDesign) dir.create2(dir.out.experimentalInput, showWarnings=FALSE, recursive=TRUE)

#timing: basis for estimated time of arrival, ETA
timerfile <- "temp_timer.csv"
if(file.exists(temp <- file.path(dir.out, timerfile)) && (!continueAfterAbort || (actionWithSWSFOutput && !actionWithSoilWat))) try(file.remove(temp), silent=TRUE)
if(!file.exists(temp)){
	write.table(t(c(0,NA)), file=temp, append=TRUE, sep=",", dec=".", col.names=FALSE, row.names=FALSE)
} else {
	ttemp <- read.csv(file=temp, header=FALSE)
	if(nrow(ttemp) > 1) todo.done <- sort(ttemp[-1, 1])
}
#timing: output for overall timing information
timerfile2 <- "Timing_Simulation.csv"
if(file.exists(temp <- file.path(dir.out, timerfile2))) try(file.remove(temp), silent=TRUE)
write.table(t(c("", "Time_s", "Number")), file=temp, append=TRUE, sep=",", dec=".", col.names=FALSE, row.names=FALSE)

#concatenate file keeps track of sql files inserted into data
concatFile <- "sqlFilesInserted.csv"
concatFileProblemLines <- "sqlFilesProblemLines.csv"

#------load libraries
dir.libraries <- .libPaths()[1]
if (.Platform$OS.type == "windows") {
	#test if user has write permission to standard library path
	err <- try(write.table(1, file=ftemp <- file.path(dir.libraries, "testPermission.txt")))
	if(inherits(err, "try-error")){
		print(paste("User has no write permission for:", dir.libraries, ". A local path is attempted instead, but this is known to likely fail for the setup of 'snow' under Windows XP"))
		dir.create2(path=dir.libraries <- file.path(dir.in, "RLibrary"),showWarnings=FALSE,recursive=FALSE)
		if(!any(.libPaths() == dir.libraries)) .libPaths(dir.libraries)
	} else {
		file.remove(ftemp)
	}
}

if(!require(Rsoilwat,quietly = TRUE) || (require(Rsoilwat,quietly = TRUE) && packageVersion("Rsoilwat") < minVersionRsoilwat)) {
	print("Going to try to install Rsoilwat library")
	installed <- FALSE
	if(.Platform$OS.type == "unix" && Sys.info()[1] == "Darwin" && sessionInfo()$R.version$major == 3){
		#try to install mac binary for R version 3
		installed<-tryCatch(install.packages(file.path(dir.in, "Rsoilwat", "Rsoilwat_osx_r3.zip"),repos=NULL, type="mac.binary",lib=dir.libraries), warning=function(w) { print(w); print("FAILED"); return(FALSE) })
		installed<-is.null(installed)
	} else if (.Platform$OS.type == "windows" && sessionInfo()$R.version$major == 3){
		#try to install windows binary for R version 3
		installed<-tryCatch(install.packages(file.path(dir.in, "Rsoilwat", "Rsoilwat_windows_r3.zip"),repos=NULL, type="win.binary",lib=dir.libraries), warning=function(w) { print(w); print("FAILED"); return(FALSE) })
		installed<-is.null(installed)
	}
	if(!installed){#attempt to compile package from source because so far neither mac or windows binary attempted to install or successfully installed
		installed <- tryCatch(install.packages(file.path(dir.in, "Rsoilwat", "SoilWat_v27_R.tar.gz"),repos=NULL, type="source",lib=dir.libraries), warning=function(w) { print(w); print("FAILED"); return(FALSE) })
		installed <- is.null(installed)
	}
	if(!installed) stop("Could not install package Rsoilwat please contact admin.")
	stopifnot(require(Rsoilwat,quietly = TRUE) && packageVersion("Rsoilwat") >= minVersionRsoilwat)
}
if(!require(circular, quietly=TRUE)) {
	tryCatch(install.packages("circular",repos=url.Rrepos,lib=dir.libraries), warning=function(w) { print(w); print("circular failed to install"); stop("Stopping") })
	stopifnot(require(circular, quietly=TRUE))
}
if(!require(SPEI, quietly=TRUE)) {
	tryCatch(install.packages("SPEI",repos=url.Rrepos,lib=dir.libraries), warning=function(w) { print(w); print("circular failed to install"); stop("Stopping") })
	stopifnot(require(SPEI, quietly=TRUE))
}
if(!require(RSQLite,quietly = TRUE)) {
	tryCatch(install.packages("RSQLite",repos=url.Rrepos,lib=dir.libraries), warning=function(w) { print(w); print("RSQLite failed to install"); stop("Stopping") })
	stopifnot(require(RSQLite, quietly = TRUE))
}

if(parallel_runs && identical(parallel_backend, "mpi")) { 
	if(!require(Rmpi,quietly = TRUE)) {
		tryCatch(install.packages("Rmpi",repos=url.Rrepos,lib=dir.libraries), warning=function(w) { print(w); print("Rmpi failed to install"); stop("Stopping") })
		stopifnot(require(Rmpi, quietly = TRUE))
	}
}
if(parallel_runs && identical(parallel_backend, "snow")) {	
	if(!require(doSNOW,quietly = TRUE)) {#requires: foreach, iterators, snow
		tryCatch(install.packages("doSNOW",repos=url.Rrepos,lib=dir.libraries), warning=function(w) { print(w); print("doSNOW failed to install"); stop("Stopping") })
		stopifnot(require(doSNOW, quietly = TRUE))
	}
}
if(parallel_runs && identical(parallel_backend, "multicore")) {
	if(!require(doMC,quietly = TRUE)) {	#requires: foreach, iterators, codetools, and attaches: multicore
		tryCatch(install.packages("doMC",repos=url.Rrepos,lib=dir.libraries), warning=function(w) { print(w); print("doMC failed to install"); stop("Stopping") })
		stopifnot(require(doMC, quietly = TRUE))
	}
}	
if(!parallel_runs) {
	if(!require(foreach,quietly = TRUE)) {
		tryCatch(install.packages("foreach",repos=url.Rrepos,lib=dir.libraries), warning=function(w) { print(w); print("foreach failed to install"); stop("Stopping") })
		stopifnot(require(foreach, quietly = TRUE))
	}
}

#if(print.debug) trace(what=circular:::SdCircularRad, tracer=quote({print(x); print(sys.calls()[[6]]); print(paste(rbar, circsd))}), at=4)

#------prepare output
aon.help <- matrix(data=output_aggregates, ncol=2, nrow=length(output_aggregates)/2, byrow=TRUE)
aon <- data.frame(t(as.numeric(aon.help[,-1])))
names(aon) <- aon.help[,1]

#------import regeneration data
if(!be.quiet) print(paste("SWSF reads input data: started at", t1 <- Sys.time()))

if(any(simulation_timescales=="daily") & aon$dailyRegeneration_GISSM) {
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
output_timescales_maxNo <- 4
SoilLayer_MaxNo <- 20
lmax <- 1:SoilLayer_MaxNo
dirname.sw.runs.weather <- "WeatherData"
SoilWat.windspeedAtHeightAboveGround <- 2	#m
st_mo <- 1:12

#------import data
SWRunInformation <- tryCatch(read.csv(file.path(dir.in, datafile.SWRunInformation), as.is=TRUE),error=function(e) { print("datafile.SWRunInformation: Bad Path"); print(e)})
include_YN <- SWRunInformation$Include_YN
labels <- SWRunInformation$Label

sw_input_soillayers <- tryCatch(read.csv(file.path(dir.in, datafile.soillayers)),error=function(e) { print("datafile.soillayers: Bad Path"); print(e)})

sw_input_treatments_use <- tryCatch(read.csv(temp <- file.path(dir.in, datafile.treatments), nrows=1),error=function(e) { print("datafile.treatments: Bad Path"); print(e)})
sw_input_treatments <- read.csv(temp, skip=1, as.is=TRUE)
colnames(sw_input_treatments) <- colnames(sw_input_treatments_use)

sw_input_experimentals_use <- tryCatch(read.csv(temp <- file.path(dir.in, datafile.Experimentals), nrows=1),error=function(e) { print("datafile.Experimentals: Bad Path"); print(e)})
sw_input_experimentals <- read.csv(temp, skip=1, as.is=TRUE)
colnames(sw_input_experimentals) <- colnames(sw_input_experimentals_use)
create_experimentals <- names(sw_input_experimentals_use[-1][which(sw_input_experimentals_use[-1] > 0 & is.finite(as.numeric(sw_input_experimentals_use[-1])))])

#update treatment specifications based on experimental design
sw_input_treatments_use_combined <- ifelse(sw_input_treatments_use[-1] == 1 | names(sw_input_treatments_use[-1]) %in% create_experimentals, 1, 0)
temp<-which(!(create_experimentals %in% names(sw_input_treatments_use[-1])))
if(length(temp) != 0) sw_input_treatments_use_combined <- cbind(sw_input_treatments_use_combined, matrix(data=1,nrow=1,ncol=length(temp),dimnames=list(NA, c(create_experimentals[temp]))))
create_treatments <- names(sw_input_treatments_use_combined[,which(sw_input_treatments_use_combined > 0 & is.finite(as.numeric(sw_input_treatments_use_combined)))])

if(dim(SWRunInformation)[2] == 1) stop("SWRunInformation might be tab separated instead of comma.")
if(dim(sw_input_soillayers)[2] == 1) stop("SoilLayers might be tab separated instead of comma.")
if(dim(sw_input_treatments_use)[2] == 1) stop("Treatments might be tab separated instead of comma.")
if(dim(sw_input_experimentals_use)[2] == 1) stop("Experimentals might be tab separated instead of comma.")

if (actionWithSoilWat || any(actions == "external")) {
	sw_input_cloud_use <- tryCatch(read.csv(temp <- file.path(dir.sw.dat, datafile.cloud), nrows=1),error=function(e) { print("datafile.cloud: Bad Path"); print(e)})
	sw_input_cloud <- read.csv(temp, skip=1)
	colnames(sw_input_cloud) <- colnames(sw_input_cloud_use)
	
	sw_input_prod_use <- tryCatch(read.csv(temp <- file.path(dir.sw.dat, datafile.prod), nrows=1),error=function(e) { print("datafile.prod: Bad Path"); print(e)})
	sw_input_prod <- read.csv(temp, skip=1)
	colnames(sw_input_prod) <- colnames(sw_input_prod_use)
	sw_input_prod_use[-1] <- ifelse(sw_input_prod_use[-1] == 1 | names(sw_input_prod_use[-1]) %in% create_experimentals, 1, 0)	#update specifications based on experimental design
	
	sw_input_site_use <- tryCatch(read.csv(temp <- file.path(dir.sw.dat, datafile.siteparam), nrows=1),error=function(e) { print("datafile.siteparam: Bad Path"); print(e)})
	sw_input_site <- read.csv(temp, skip=1)
	colnames(sw_input_site) <- colnames(sw_input_site_use)
	sw_input_site_use[-1] <- ifelse(sw_input_site_use[-1] == 1 | names(sw_input_site_use[-1]) %in% create_experimentals, 1, 0)	#update specifications based on experimental design
	
	sw_input_soils_use <- tryCatch(read.csv(temp <- file.path(dir.sw.dat, datafile.soils), nrows=1),error=function(e) { print("datafile.soils: Bad Path"); print(e)})
	sw_input_soils <- read.csv(temp, skip=1)
	colnames(sw_input_soils) <- colnames(sw_input_soils_use)
	sw_input_soils_use[-1] <- ifelse(sw_input_soils_use[-1] == 1 | names(sw_input_soils_use[-1]) %in% create_experimentals, 1, 0)	#update specifications based on experimental design
	
	sw_input_weather_use <- tryCatch(read.csv(temp <- file.path(dir.sw.dat, datafile.weathersetup), nrows=1),error=function(e) { print("datafile.weathersetup: Bad Path"); print(e)})
	sw_input_weather <- read.csv(temp, skip=1)
	colnames(sw_input_weather) <- colnames(sw_input_weather_use)
	
	sw_input_climscen_use <- tryCatch(read.csv(temp <- file.path(dir.sw.dat, datafile.climatescenarios), nrows=1),error=function(e) { print("datafile.climatescenarios: Bad Path"); print(e)})
	sw_input_climscen <- read.csv(temp, skip=1)
	colnames(sw_input_climscen) <- colnames(sw_input_climscen_use)
	
	sw_input_climscen_values_use <- tryCatch(read.csv(temp <- file.path(dir.sw.dat, datafile.climatescenarios_values), nrows=1),error=function(e) { print("datafile.climatescenarios_values: Bad Path"); print(e)})
	sw_input_climscen_values <- read.csv(temp, skip=1)
	colnames(sw_input_climscen_values) <- colnames(sw_input_climscen_values_use)
	
	if(dim(sw_input_cloud_use)[2] == 1) stop("Cloud datafile might be tab separated instead of comma.")
	if(dim(sw_input_prod_use)[2] == 1) stop("Prod datafile might be tab separated instead of comma.")
	if(dim(sw_input_site_use)[2] == 1) stop("Site datafile might be tab separated instead of comma.")
	if(dim(sw_input_soils_use)[2] == 1) stop("Soils datafile might be tab separated instead of comma.")
	if(dim(sw_input_weather_use)[2] == 1) stop("Weather datafile might be tab separated instead of comma.")
	if(dim(sw_input_climscen_use)[2] == 1) stop("Climate Use datafile datafile might be tab separated instead of comma.")
	if(dim(sw_input_climscen_values_use)[2] == 1) stop("Climate Values datafile datafile might be tab separated instead of comma.")
	
	#Create a list of possible treatment files with data.
	if(any(create_treatments=="sw"))
		print("SW treatment is not used because library Rsoilwat only uses one version of soilwat. Sorry")
	if(any(create_treatments=="filesin")) {
		tr_files <- list()
		temp<-list.files(path=file.path(dir.sw.in.tr, "filesin"),pattern="in",include.dirs=FALSE,recursive=TRUE,full.names=TRUE)
		tr_files[basename(temp)] <-unlist(lapply(temp,FUN=function(x) return(swReadLines(swClear(new("swFiles")),x))))
	}
	if(any(create_treatments=="prodin")) {
		tr_prod <- list()
		temp<-list.files(path=file.path(dir.sw.in.tr, "prodin"),pattern="in",include.dirs=FALSE,recursive=TRUE,full.names=TRUE)
		tr_prod[basename(temp)] <-unlist(lapply(temp,FUN=function(x) return(swReadLines(swClear(new("swProd")),x))))
	}
	if(any(create_treatments=="siteparamin")) {
		tr_site <- list()
		temp<-list.files(path=file.path(dir.sw.in.tr, "siteparamin"),pattern="in",include.dirs=FALSE,recursive=TRUE,full.names=TRUE)
		tr_site[basename(temp)] <-unlist(lapply(temp,FUN=function(x) return(swReadLines(swClear(new("swSite")),x))))
	}
	if(any(create_treatments=="soilsin")) {
		tr_soil <- list()
		temp<-list.files(path=file.path(dir.sw.in.tr, "soilsin"),pattern="in",include.dirs=FALSE,recursive=TRUE,full.names=TRUE)
		tr_soil[basename(temp)] <-unlist(lapply(temp,FUN=function(x) return(swReadLines(swClear(new("swSoils")),x))))
	}
	if(any(create_treatments=="weathersetupin")) {
		tr_weather <- list()
		temp<-list.files(path=file.path(dir.sw.in.tr, "weatherin"),pattern="in",include.dirs=FALSE,recursive=TRUE,full.names=TRUE)
		tr_weather[basename(temp)] <-unlist(lapply(temp,FUN=function(x) return(swReadLines(swClear(new("swWeather")),x))))
	}
	if(any(create_treatments=="cloudin")) {
		tr_cloud <- list()
		temp<-list.files(path=file.path(dir.sw.in.tr, "cloudin"),pattern="in",include.dirs=FALSE,recursive=TRUE,full.names=TRUE)
		tr_cloud[basename(temp)] <-unlist(lapply(temp,FUN=function(x) return(swReadLines(swClear(new("swCloud")),x))))
	}
	
	if(any(create_treatments == "LookupClimatePPTScenarios")) tr_input_climPPT <- read.csv( file.path(dir.sw.in.tr, "LookupClimatePPTScenarios", trfile.LookupClimatePPTScenarios))
	if(any(create_treatments == "LookupClimateTempScenarios")) tr_input_climTemp <- read.csv( file.path(dir.sw.in.tr, "LookupClimateTempScenarios", trfile.LookupClimateTempScenarios))
	if(any(create_treatments == "LookupShiftedPPTScenarios")) tr_input_shiftedPPT <- read.csv( file.path(dir.sw.in.tr, "LookupShiftedPPTScenarios", trfile.LookupShiftedPPTScenarios), row.names=1)
	if(any(create_treatments == "LookupEvapCoeffFromTable")) tr_input_EvapCoeff <- read.csv( file.path(dir.sw.in.tr, "LookupEvapCoeffFromTable", trfile.LookupEvapCoeffFromTable), row.names=1)
	if(any(create_treatments == "LookupTranspCoeffFromTable_Grass", create_treatments == "LookupTranspCoeffFromTable_Shrub", create_treatments == "LookupTranspCoeffFromTable_Tree", create_treatments == "AdjRootProfile")){
		tr_input_TranspCoeff_Code <- tryCatch(read.csv(temp <- file.path(dir.sw.in.tr, "LookupTranspCoeffFromTable", trfile.LookupTranspCoeffFromTable), nrows=2), error=function(e) { print("LookupTranspCoeffFromTable.csv: Bad Path"); print(e)})
		tr_input_TranspCoeff_Code <- tr_input_TranspCoeff_Code[-2,]
		tr_input_TranspCoeff <- read.csv(temp, skip=2)
		colnames(tr_input_TranspCoeff) <- colnames(tr_input_TranspCoeff_Code)
	}
	if(any(create_treatments == "LookupTranspRegionsFromTable")) tr_input_TranspRegions <- read.csv( file.path(dir.sw.in.tr, "LookupTranspRegionsFromTable", trfile.LookupTranspRegionsFromTable), row.names=1)
	if(any(create_treatments == "LookupSnowDensityFromTable")) tr_input_SnowD <- read.csv( file.path(dir.sw.in.tr, "LookupSnowDensityFromTable", trfile.LookupSnowDensityFromTable), row.names=1)
	if(any(create_treatments == "AdjMonthlyBioMass_Temperature")) tr_VegetationComposition <- read.csv(file.path(dir.sw.in.tr, "LookupVegetationComposition", trfile.LookupVegetationComposition), skip=1, row.names=1)
}

if(!be.quiet) print(paste("SWSF reads input data: ended after",  round(difftime(Sys.time(), t1, units="secs"), 2), "s"))


#------determine number of runs
runs <- sum(include_YN>0, na.rm=TRUE)
trow  <- length(include_YN)
if(!(length(runs) > 0)) stop(paste("at least 1 SoilWat-run needed for simulation, but", runs, "found"))
trowExperimentals <- ifelse(length(create_experimentals) > 0, nrow(sw_input_experimentals), 0)
#identify which SoilWat-runs = rows are to be carried out
seq.tr <- which(include_YN > 0)	# sequence of row numbers in the master and treatment input files that are included
seq.todo <- (1:(runs * ifelse(trowExperimentals > 0, trowExperimentals, 1))) # consecutive number of all (tr x exp) simulations to be executed
runsN.total <- length(seq.todo)
counter.digitsN <- 1 + ceiling(log10(runsN.total))	#max index digits
if(exists("todo.done")) { #adjust for already completed runs
	seq.todo <- seq.todo[-todo.done]
}
runsN.todo <- length(seq.todo)
	

#------create scenario names
climate.conditions <- c(climate.ambient, climate.conditions[!grepl(climate.ambient, climate.conditions)])
scenario_No <- length(climate.conditions)
scenario <- climate.conditions

#------create ensembles
if(length(ensemble.levels) > 0) ensemble.levels <- sort(ensemble.levels)
do.ensembles <- any(actions=="ensemble") && !is.null(ensemble.families) && length(ensemble.levels) > 0 && is.numeric(ensemble.levels) && length(climate.conditions) > 1

if(do.ensembles){
	scenarios.ineach.ensemble <- sapply(ensemble.families, function(x) grepl(pattern=x, scenario, ignore.case=TRUE), simplify=TRUE)
	ensemble.families <- ensemble.families[temp <- apply(scenarios.ineach.ensemble, MARGIN=2, FUN=any)]
	scenarios.ineach.ensemble <- scenarios.ineach.ensemble[, temp]
	families_N <- length(ensemble.families)
	if(families_N > 1){
		scenariosPERensemble_N <- max(temp <- apply(scenarios.ineach.ensemble, MARGIN=2, FUN=sum))
		stopifnot(any(ensemble.levels <= min(temp)))
	} else{
		scenariosPERensemble_N <- sum(scenarios.ineach.ensemble)
		stopifnot(any(ensemble.levels <= scenariosPERensemble_N))
	}
}

#------outputing data
if(makeInputForExperimentalDesign) ExpInput_Seperator <- "X!X"

#append treatment information to the aggregated output in addition to selected Index_RunInformation
Index_RunInformation_Treatments <- NULL
if(length(create_treatments) > 0) {
	Index_RunInformation_Treatments <- match(create_treatments, names(sw_input_treatments))
}

daily_no <- length(output_aggregate_daily)
if(any(simulation_timescales=="daily")){
	if(any(output_aggregate_daily == "SWA") & length(SWPcrit_MPa) > 0){
		output_aggregate_daily <- output_aggregate_daily[-which(output_aggregate_daily == "SWA")]
		for(icrit in seq(along=SWPcrit_MPa)){
			output_aggregate_daily <- c(output_aggregate_daily, paste("SWAatSWPcrit", abs(round(-1000*SWPcrit_MPa[icrit], 0)), "kPa", sep=""))
		}
		daily_no <- length(output_aggregate_daily)
	}
	
	if(AggLayer.daily){
		aggLs_no <- 2 + ifelse(is.null(Depth_ThirdAggLayer.daily), 1, ifelse(!is.na(Depth_ThirdAggLayer.daily), 1, 0)) + ifelse(is.null(Depth_FourthAggLayer.daily), 1, ifelse(!is.na(Depth_FourthAggLayer.daily), 1, 0))
	} else {#at this stage we don't know how many soil layers we will have among the SoilWat runs; so just prepare for the maximum
		if(!any(create_treatments == "soilsin") & !is.null(sw_input_soillayers)){
			aggLs_no <- max(apply(sw_input_soillayers[, -1], MARGIN=1, FUN=function(x) ifelse(is.na(x[1]), NA, findInterval(x[1] - sqrt(.Machine$double.neg.eps), c(0, na.exclude(unlist(x[-1]))))) ), na.rm=TRUE)
		} else {
			aggLs_no <- SoilLayer_MaxNo
		}
	}
}
#------flags for external------#
temp <- matrix(data=do.ExtractExternalDatasets, ncol=2, nrow=length(do.ExtractExternalDatasets)/2, byrow=TRUE)
exinfo <- data.frame(t(as.numeric(temp[,-1])))
names(exinfo) <- temp[,1]

if(exinfo$ExtractClimateChangeScenarios_CMIP3_BCSD_GDODCPUCLLNL_USA || exinfo$ExtractClimateChangeScenarios_CMIP3_BCSD_GDODCPUCLLNL_Global || exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_GDODCPUCLLNL_USA || exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_GDODCPUCLLNL_Global || exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_NEX_USA) {
	getScenarioWeatherDataFromDatabase <- TRUE
	getCurrentWeatherDataFromDatabase <- TRUE
	#TODO: downscaling.method 
}
if(getScenarioWeatherDataFromDatabase) {
	getCurrentWeatherDataFromDatabase<-TRUE
	if(!createWeatherDatabaseFromLookupWeatherFolder && !file.exists(dbWeatherDataFile))
		stop("Create or use existing Weather database with Scenario data inside.")
}
#------ Create the Database and Tables within
if(!be.quiet) print(paste("SWSF sets up the database: started at", t1 <- Sys.time()))
drv <- dbDriver("SQLite")

name.OutputDB <- file.path(dir.out, "dbTables.sqlite3")
if(copyCurrentConditionsFromDatabase | copyCurrentConditionsFromTempSQL) name.OutputDBCurrent <- file.path(dir.out, "dbTables_current.sqlite3")
source("2_SoilWatSimulationFramework_Part2of4_CreateDB_Tables_v50.R", echo=F, keep.source=F)
con <- dbConnect(drv, dbname=name.OutputDB)

if(getCurrentWeatherDataFromDatabase && !GriddedDailyWeatherFromMaurer2002_NorthAmerica)
	conWeather <- dbConnect(drv, dbname=dbWeatherDataFile)

if(!be.quiet) print(paste("SWSF sets up the database: ended after",  round(difftime(Sys.time(), t1, units="secs"), 2), "s"))


#------simulation timing
output_timescales_shortest <- ifelse(any(simulation_timescales=="daily"), 1, ifelse(any(simulation_timescales=="weekly"), 2, ifelse(any(simulation_timescales=="monthly"), 3, 4)))

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
adjustLayersDepth <- function(layers_depth, d) return(round(layers_depth[1:d])) #The wrapper only handles 1-cm resolution of soil depths (maily because of the trco)
getLayersWidth <- function(layers_depth) return(diff(c(0, layers_depth)))
setLayerSequence <- function(d) return(1:d)

sw_dailyC4_TempVar <- function(dailyTempMin, dailyTempMean, simTime2){
	#Variables to estimate percent C4 species in North America: Teeri JA, Stowe LG (1976) Climatic patterns and the distribution of C4 grasses in North America. Oecologia, 23, 1-12.
	
	Month7th_MinTemp_C <- aggregate(dailyTempMin[simTime2$month_ForEachUsedDay_NSadj == 7], by=list(simTime2$year_ForEachUsedDay_NSadj[simTime2$month_ForEachUsedDay_NSadj == 7]), FUN=min)[, 2]
	LengthFreezeFreeGrowingPeriod_Days <- aggregate(dailyTempMin, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=function(x) {temp <- rle(x > 0); if(any(temp$values)) max(temp$lengths[temp$values], na.rm=TRUE) else 0})[, 2]
	DegreeDaysAbove65F_DaysC <- aggregate(dailyTempMean, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=function(x) sum(ifelse((temp <- x - ((65-32) * 5/9)) > 0, temp, 0)))[, 2]
	
	nyrs <- seq_along(Month7th_MinTemp_C) #if southern Hemisphere, then 7th month of last year is not included
	res <- c(apply(temp <- cbind(Month7th_MinTemp_C[nyrs], LengthFreezeFreeGrowingPeriod_Days[nyrs], DegreeDaysAbove65F_DaysC[nyrs]), MARGIN=2, FUN=mean), apply(temp, MARGIN=2, FUN=sd))
	names(res) <- c(temp <- c("Month7th_NSadj_MinTemp_C", "LengthFreezeFreeGrowingPeriod_NSadj_Days", "DegreeDaysAbove65F_NSadj_DaysC"), paste(temp, ".sd", sep=""))
	
	return(res)
}

sw_SiteClimate_Ambient <- function(weatherList, year.start, year.end, do.C4vars=FALSE, simTime2=NULL) {
	sw.weather.suffices <- as.numeric(names(weatherList))
	itemp <- year.start <= sw.weather.suffices & year.end >= sw.weather.suffices
	years <- sw.weather.suffices[itemp]
	
	tempMean <- tempMin <- tempMax <- ppt <- rep(0, times=12)
	mat <- NULL
	if(do.C4vars){
		dailyTempMin <- NULL
		dailyTempMean <- NULL
	}
	if((no.yrs <- length(years)) > 0) for(y in 1:no.yrs){
			temp.dailyTempMean <- apply(get_swWeatherData(weatherList, years[y])@data[, 2:3], 1, mean)
			temp.dailyTempMin <- get_swWeatherData(weatherList, years[y])@data[, 3]
			temp.dailyTempMax <- get_swWeatherData(weatherList, years[y])@data[, 2]
			mat <- c(mat, mean(temp.dailyTempMean))
			if(do.C4vars){
				dailyTempMin <- c(dailyTempMin, get_swWeatherData(weatherList, years[y])@data[, 3])
				dailyTempMean <- c(dailyTempMean, temp.dailyTempMean)
			}
			month_forEachDoy <- as.POSIXlt(seq(from=as.POSIXlt(paste(years[y], "-01-01", sep="")), to=as.POSIXlt(paste(years[y], "-12-31", sep="")), by="1 day"))$mon + 1
			tempMean <- tempMean + aggregate(temp.dailyTempMean, by=list(month_forEachDoy), FUN=mean)[, 2]
			tempMin <- tempMin + aggregate(temp.dailyTempMin, by=list(month_forEachDoy), FUN=mean)[, 2]
			tempMax <- tempMax + aggregate(temp.dailyTempMax, by=list(month_forEachDoy), FUN=mean)[, 2]
			ppt <- ppt + aggregate(get_swWeatherData(weatherList, years[y])@data[, 4], by=list(month_forEachDoy), FUN=sum)[, 2]
		}
	tempMean <- tempMean / no.yrs
	tempMin <- tempMin / no.yrs
	tempMax <- tempMax / no.yrs
	ppt <- ppt / no.yrs
	
	res <- list(meanMonthlyTempC=tempMean, minMonthlyTempC=tempMin, maxMonthlyTempC=tempMax, 
				meanMonthlyPPTcm=ppt, MAP_cm=sum(ppt), MAT_C=mean(mat))
	
	if(do.C4vars){
		res$dailyTempMin <- dailyTempMin
		res$dailyTempMean <- dailyTempMean
		res$dailyC4vars <- sw_dailyC4_TempVar(dailyTempMin, dailyTempMean, simTime2)
	}
	return(res)
}

PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996 <- function(MAP_mm,MAT_C,monthly.ppt,monthly.temp,dailyC4vars,isNorth,shrub.fraction.limit,
		use_Annuals_Fraction,Annuals_Fraction,
		use_C4_Fraction,C4_Fraction,
		use_C3_Fraction,C3_Fraction,
		use_Shrubs_Fraction,Shrubs_Fraction) {

	cut0Inf <- function(x) {x[x < 0] <- NA; return(x)}
	NAto0 <- function(x) {x[is.na(x)] <- 0; return(x)}
	finite01 <- function(x) {x[x < 0 | is.na(x)] <- 0; x[x > 1] <- 1; return(x)}
	f.digits <- 3
	tolerance <- 1.1*10^-f.digits

	#Get the user specified fractions, if column is false set to NA
	tree.fraction <- 0 #option 'PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996' doesn't estimate tree cover, i.e., assumed to be == 0
	AnnC4C3ShrubFraction <- rep(NA, 4)
	if(use_Annuals_Fraction){
		AnnC4C3ShrubFraction[1] <- finite01(Annuals_Fraction)
	} else {
		AnnC4C3ShrubFraction[1] <- 0 #Annuals can not be NA
	}
	if(use_C4_Fraction)
		AnnC4C3ShrubFraction[2] <- C4_Fraction
	if(use_C3_Fraction)
		AnnC4C3ShrubFraction[3] <- C3_Fraction
	if(use_Shrubs_Fraction)
		AnnC4C3ShrubFraction[4] <- Shrubs_Fraction
	AnnC4C3ShrubFraction <- cut0Inf(AnnC4C3ShrubFraction) #treat negatives as if NA
	TotalFraction <- sum(AnnC4C3ShrubFraction, na.rm=TRUE)
	
	#Decide if all fractions are sufficiently defined or if they need to be calculated based on climate variables
	if(!isTRUE(all.equal(TotalFraction, 1, tolerance=tolerance)) && TotalFraction < 1 && sum(is.na(AnnC4C3ShrubFraction)) == 0) {
		stop(print(paste(i, " run: User defined fractions of Shrub, C3, C4, Annuals are all set, but less than 1", sep=""))) #throw an error
	}
	
	if(isTRUE(all.equal(TotalFraction, 1, tolerance=tolerance)) || TotalFraction > 1 || sum(is.na(AnnC4C3ShrubFraction)) == 1){
		
		if(sum(is.na(AnnC4C3ShrubFraction)) == 1){ #if only one is NA, then this can be calculated
			AnnC4C3ShrubFraction[which(is.na(AnnC4C3ShrubFraction))] <- cut0Inf(1 - TotalFraction)
		} else {					
			AnnC4C3ShrubFraction <- finite01(AnnC4C3ShrubFraction) #the composition is >= 1, so set eventually remaining NA to 0
		}
		
		TotalFraction <- sum(AnnC4C3ShrubFraction, na.rm=TRUE)
		AnnC4C3ShrubFraction <- AnnC4C3ShrubFraction / TotalFraction #Rescale, in case it is needed	
		
	} else { #i.e., (TotalFraction < 1 && sum(is.na(AnnC4C3ShrubFraction)) > 1) is TRUE; thus, calculate some fractions based on climate variables
		if(isNorth){ #Northern hemisphere
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
		if(MAP_mm < 1){
			shrubs.fractionNA <- NA
		} else {
			shrubs.fractionNA <- cut0Inf(1.7105 - 0.2918 * log(MAP_mm) + 1.5451 * ppt.WinterToMAP) 								#if NA, then not enough winter precipitation above a given MAP
		}
		if(MAT_C <= 0){
			grass.c4.fractionNA <- 0
		} else {
			grass.c4.fractionNA <- cut0Inf(-0.9837 + 0.000594 * MAP_mm + 1.3528 * ppt.SummerToMAP + 0.2710 * log(MAT_C))			#if NA, then either MAT < 0 or not enough summer precipitation or too cold below a given MAP
		}
		if(ppt.WinterToMAP <= 0){
			grass.c3ingrasslands.fractionNA <- grass.c3inshrublands.fractionNA <- NA
		} else {
			grass.c3ingrasslands.fractionNA <- cut0Inf(1.1905 - 0.02909 * MAT_C + 0.1781 * log(ppt.WinterToMAP) - 0.2383 * 1)		#if NA, then not enough winter precipitation or too warm below a given MAP
			grass.c3inshrublands.fractionNA <- cut0Inf(1.1905 - 0.02909 * MAT_C + 0.1781 * log(ppt.WinterToMAP) - 0.2383 * 2)
		}
		grass.c3.fractionNA <- ifelse(shrubs.fractionNA >= shrub.fraction.limit && !is.na(shrubs.fractionNA), grass.c3inshrublands.fractionNA, grass.c3ingrasslands.fractionNA)
		
		grass.Annual.fraction <- AnnC4C3ShrubFraction[1] #Ann will be 0 or something <= 1
		
		#2. step: Teeri JA, Stowe LG (1976) Climatic patterns and the distribution of C4 grasses in North America. Oecologia, 23, 1-12.
		#This equations give percent species/vegetation -> use to limit Paruelo's C4 equation, i.e., where no C4 species => there are no C4 abundance > 0
		if(dailyC4vars["LengthFreezeFreeGrowingPeriod_NSadj_Days"] <= 0){
			grass.c4.species <- 0
		} else {
			x10 <- dailyC4vars["Month7th_NSadj_MinTemp_C"] * 9/5 + 32
			x13 <- dailyC4vars["DegreeDaysAbove65F_NSadj_DaysC"] * 9/5
			x18 <- log(dailyC4vars["LengthFreezeFreeGrowingPeriod_NSadj_Days"])
			grass.c4.species <- as.numeric((1.60 * x10 + 0.0086 * x13 - 8.98 * x18 - 22.44) / 100)
		}
		grass.c4.fractionNA <- ifelse(grass.c4.species >= 0, grass.c4.fractionNA, NA)
		
		#3. step: Replacing missing values: If no or only one successful equation, then add 100% C3 if MAT < 10 C, 100% shrubs if MAP < 600 mm, and 100% C4 if MAT >= 10C & MAP >= 600 mm	[these rules are made up arbitrarily by drs, Nov 2012]
		if(sum(!is.na(shrubs.fractionNA), !is.na(grass.c4.fractionNA), !is.na(grass.c3.fractionNA)) <= 1){
			if(MAP_mm < 600) shrubs.fractionNA <- 1 + ifelse(is.na(shrubs.fractionNA), 0, shrubs.fractionNA)
			if(MAT_C < 10)  grass.c3.fractionNA <- 1 + ifelse(is.na(grass.c3.fractionNA), 0, grass.c3.fractionNA)
			if(MAT_C >= 10  & MAP_mm >= 600)  grass.c4.fractionNA <- 1 + ifelse(is.na(grass.c4.fractionNA), 0, grass.c4.fractionNA)
		}
		
		#4. step: Scale fractions to 0-1 with a sum of 1 including grass.Annual.fraction, but don't scale grass.Annual.fraction
		#if na then use calc fraction else use the user defined fraction
		shrubs.fraction <- NAto0(shrubs.fractionNA)
		grass.c4.fraction <- NAto0(grass.c4.fractionNA)
		grass.c3.fraction <- NAto0(grass.c3.fractionNA)
		
		sumVegWithoutAnnuals <- shrubs.fraction + grass.c4.fraction + grass.c3.fraction
		shrubs.fraction <- (shrubs.fraction / sumVegWithoutAnnuals) * (1 - grass.Annual.fraction) #scale these down to 1-annual fraction
		grass.c4.fraction <- (grass.c4.fraction / sumVegWithoutAnnuals) * (1 - grass.Annual.fraction)
		grass.c3.fraction <- (grass.c3.fraction / sumVegWithoutAnnuals) * (1 - grass.Annual.fraction)
		
		calcAnnC4C3ShrubFraction <- c(grass.Annual.fraction, grass.c4.fraction, grass.c3.fraction, shrubs.fraction)
		naIndex <- which(is.na(AnnC4C3ShrubFraction))
		#replace missing values
		if(isTRUE(all.equal(sum(calcAnnC4C3ShrubFraction[naIndex]), 0)) && isTRUE(all.equal(temp <- sum(AnnC4C3ShrubFraction[!naIndex]), 0))){ #there would be no vegetation, so force vegetation > 0
			AnnC4C3ShrubFraction[naIndex] <- (1 - temp) / length(naIndex)
		} else {
			AnnC4C3ShrubFraction[naIndex] <- calcAnnC4C3ShrubFraction[naIndex]
		}
		#now we need to get the sum and scale the naIndex values accordingly
		AnnC4C3ShrubFraction[naIndex] <- sapply(AnnC4C3ShrubFraction[naIndex], function(x) (x/sum(AnnC4C3ShrubFraction[naIndex])) * (1-sum(AnnC4C3ShrubFraction[-naIndex])))
	}
	
	#Scale Grass components to one (or set to 0)
	if(!isTRUE(all.equal(AnnC4C3ShrubFraction[4], 1))){
		grass.c4.fractionG <- AnnC4C3ShrubFraction[2] / (1-AnnC4C3ShrubFraction[4])
		grass.c3.fractionG <- AnnC4C3ShrubFraction[3] / (1-AnnC4C3ShrubFraction[4])
		grass.Annual.fractionG <- AnnC4C3ShrubFraction[1] / (1-AnnC4C3ShrubFraction[4])
	} else {
		grass.c4.fractionG <- grass.c3.fractionG <- grass.Annual.fractionG <- 0
	}
	grass.fraction <- sum(AnnC4C3ShrubFraction[c(1:3)])
	
	return(list("Composition"=c("Grasses"=grass.fraction, "Shrubs"=AnnC4C3ShrubFraction[4], "Trees"=tree.fraction),"grasses.c3c4ann.fractions"=c(grass.c3.fractionG,grass.c4.fractionG,grass.Annual.fractionG)))
}

AdjMonthlyBioMass <- function(tr_VegetationComposition,AdjMonthlyBioMass_Temperature,AdjMonthlyBioMass_Precipitation,grasses.c3c4ann.fractions,growing.season.threshold.tempC,isNorth,MAP_mm,monthly.temp) {
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
	
	adjCompPPT <- function(shrubs_Composition, C3_Composition, C4_Composition, AnnGrass_Composition, ShrubsMAP_mm, GrassMAP_mm) {
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
		shrubs_Composition$Sh.Amount.Live <- shrubs_Composition$Sh.Amount.Live * Shrub_BiomassScaler					
		C3_Composition$C3.Amount.Live <- C3_Composition$C3.Amount.Live * Grass_BiomassScaler					
		C4_Composition$C4.Amount.Live <- C4_Composition$C4.Amount.Live * Grass_BiomassScaler					
		AnnGrass_Composition$Annual.Amount.Live <- AnnGrass_Composition$Annual.Amount.Live * Grass_BiomassScaler					
		
		#Scale litter amount by productivity and adjust for ratio of litter/live
		shrubs_Composition$Sh.Litter <- shrubs_Composition$Sh.Litter * Shrub_BiomassScaler * colmax["Sh.Litter"] / colmax["Sh.Amount.Live"]	
		C3_Composition$C3.Litter <- C3_Composition$C3.Litter * Grass_BiomassScaler * colmax["C3.Litter"] / colmax["C3.Amount.Live"]	
		C4_Composition$C4.Litter <- C4_Composition$C4.Litter * Grass_BiomassScaler * colmax["C4.Litter"] / colmax["C4.Amount.Live"]	
		AnnGrass_Composition$Annual.Litter <- AnnGrass_Composition$Annual.Litter * Grass_BiomassScaler * colmax["Annual.Litter"] / colmax["Annual.Amount.Live"]	
		
		#Guarantee that live fraction = ]0, 1]
		shrubs_Composition$Sh.Perc.Live <- pmin(1, pmax(sqrt(.Machine$double.eps), shrubs_Composition$Sh.Perc.Live))
		C3_Composition$C3.Perc.Live <- pmin(1, pmax(sqrt(.Machine$double.eps), C3_Composition$C3.Perc.Live))
		C4_Composition$C4.Perc.Live <- pmin(1, pmax(sqrt(.Machine$double.eps), C4_Composition$C4.Perc.Live))
		AnnGrass_Composition$Annual.Perc.Live <- pmin(1, pmax(sqrt(.Machine$double.eps), AnnGrass_Composition$Annual.Perc.Live))
		
		#Calculate total biomass based on scaled live biomass amount
		shrubs_Composition$Sh.Biomass <- shrubs_Composition$Sh.Amount.Live / shrubs_Composition$Sh.Perc.Live
		C3_Composition$C3.Biomass <- C3_Composition$C3.Amount.Live / C3_Composition$C3.Perc.Live
		C4_Composition$C4.Biomass <- C4_Composition$C4.Amount.Live / C4_Composition$C4.Perc.Live
		AnnGrass_Composition$Annual.Biomass <- AnnGrass_Composition$Annual.Amount.Live / AnnGrass_Composition$Annual.Perc.Live
		
		return(list("shrubs_Composition"=shrubs_Composition,"C3_Composition"=C3_Composition,"C4_Composition"=C4_Composition,"AnnGrass_Composition"=AnnGrass_Composition))
	}
	
	#adjust phenology for mean monthly temperatures
	if(AdjMonthlyBioMass_Temperature) {
		growing.season <- monthly.temp >= growing.season.threshold.tempC
		
		if(!isNorth) growing.season <- c(growing.season[7:12], growing.season[1:6]) #Standard growing season needs to be adjusted for southern Hemi
		
		predict.season <- function(biomass_Standard, std.season.padded, std.season.seq, site.season.seq){
			#length(std.season.seq) >= 3 because of padding and test that season duration > 0
			calc.loess_coeff <- function(N, span){
				#prevent call to loessc.c:ehg182(104): "span too small.   fewer data values than degrees of freedom"
				lcoef <- list(span=min(1, span), degree=2)
				if(span > 1) return(lcoef)
				nf <- floor(lcoef$span * N) - 1 #see R/trunk/src/library/stats/src/loessf.f:ehg136()
				if(nf > 2){
					lcoef$degree <- 2
				} else if(nf > 1){
					lcoef$degree <- 1
				} else {
					lcoef <- calc.loess_coeff(N, lcoef$span+0.1)
				}
				return(lcoef)		
			}
			lcoef <- calc.loess_coeff(N=length(std.season.seq), span=0.4)
			
			op <- options(c("warn", "error"))
			options(warn=-1, error=traceback) #loess throws many warnings: 'pseudoinverse used', see calc.loess_coeff(), etc.
			res <- sapply(apply(biomass_Standard, MARGIN=2, function(x) {lf<-loess(x[std.season.padded] ~ std.season.seq, span=lcoef$span, degree=lcoef$degree); predict(lf, newdata=data.frame(std.season.seq=site.season.seq) ) }), FUN=function(x) max(0, x)) # guarantee that > 0
			options(op)
			return(res)
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
				shrubs_Composition[] <- matrix(apply(shrubs_Standard[std.winter,], 2, mean), nrow=12, ncol=ncol(shrubs_Composition), byrow=TRUE)
				C3_Composition[] <- matrix(apply(C3_Standard[std.winter,], 2, mean), nrow=12, ncol=ncol(C3_Composition), byrow=TRUE)
				C4_Composition[] <- matrix(apply(C4_Standard[std.winter,], 2, mean), nrow=12, ncol=ncol(C4_Composition), byrow=TRUE)
				AnnGrass_Composition[] <- matrix(apply(AnnGrass_Standard[std.winter,], 2, mean), nrow=12, ncol=ncol(AnnGrass_Composition), byrow=TRUE)
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
				shrubs_Composition[] <- matrix(apply(shrubs_Standard[std.growing,], MARGIN=2, FUN=max), nrow=12, ncol=ncol(shrubs_Composition), byrow=TRUE)
				C3_Composition[] <- matrix(apply(C3_Standard[std.growing,], MARGIN=2, FUN=max), nrow=12, ncol=ncol(C3_Composition), byrow=TRUE)
				C4_Composition[] <- matrix(apply(C4_Standard[std.growing,], MARGIN=2, FUN=max), nrow=12, ncol=ncol(C4_Composition), byrow=TRUE)
				AnnGrass_Composition[] <- matrix(apply(AnnGrass_Standard[std.growing,], MARGIN=2, FUN=max), nrow=12, ncol=ncol(AnnGrass_Composition), byrow=TRUE)
			}
		}
		if(!isNorth) { #Adjustements were done as if on nothern hemisphere
			shrubs_Composition <- rbind(shrubs_Composition[7:12,], shrubs_Composition[1:6,])
			C3_Composition <- rbind(C3_Composition[7:12,], C3_Composition[1:6,])
			C4_Composition <- rbind(C4_Composition[7:12,], C4_Composition[1:6,])
			AnnGrass_Composition <- rbind(AnnGrass_Composition[7:12,], AnnGrass_Composition[1:6,])
		}
		if(!AdjMonthlyBioMass_Precipitation){
			temp<-adjCompPPT(shrubs_Composition,C3_Composition,C4_Composition,AnnGrass_Composition,ShrubsMAP_mm=StandardShrub_MAP_mm, GrassMAP_mm=StandardGrasses_MAP_mm)
			shrubs_Composition <- temp$shrubs_Composition
			C3_Composition <- temp$C3_Composition
			C4_Composition <- temp$C4_Composition
			AnnGrass_Composition <- temp$AnnGrass_Composition
		}
	}
	#Adjust biomass amounts by productivity relationship with MAP
	if(AdjMonthlyBioMass_Precipitation) {
		temp<-adjCompPPT(shrubs_Composition,C3_Composition,C4_Composition,AnnGrass_Composition,ShrubsMAP_mm=MAP_mm, GrassMAP_mm=MAP_mm)
		shrubs_Composition <- temp$shrubs_Composition
		C3_Composition <- temp$C3_Composition
		C4_Composition <- temp$C4_Composition
		AnnGrass_Composition <- temp$AnnGrass_Composition
	}
	
	Grass_Composition <- C3_Composition*grasses.c3c4ann.fractions[1] + C4_Composition*grasses.c3c4ann.fractions[2] + AnnGrass_Composition*grasses.c3c4ann.fractions[3]
	return(list("grass"=as.matrix(Grass_Composition),"shrub"=as.matrix(shrubs_Composition)))
}

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
circ.mean <- function(x, int, na.rm=FALSE){
	if(length(x) == sum(is.na(x))){
		return(NA)
	} else {
		require(circular)
		
		circ <- 2 * pi / int
		x.circ <- circular(x * circ, type="angles", units="radians", rotation="clock", modulo="2pi")
		x.int <- mean.circular(x.circ, na.rm=na.rm) / circ
		rm(circ, x.circ)
		return(round(as.numeric(x.int) - 1, 13) %% int + 1)	# map 0 -> int; rounding to 13 digits: 13 was empirically derived for int={12, 365} and x=c((-1):2, seq(x-5, x+5, by=1), seq(2*x-5, 2*x+5, by=1)) assuming that this function will never need to calculate for x > t*int with t>2
	}
}
circ.range <- function(x, int, na.rm=FALSE) {
	if(length(x) == sum(is.na(x))){
		return(NA)
	} else {
		require(circular)
		
		circ <- 2 * pi / int
		x.circ <- circular(x * circ, type="angles", units="radians", rotation="clock", modulo="2pi")
		x.int <- range(x.circ, na.rm=na.rm) / circ
		rm(circ, x.circ)
		return(as.numeric(x.int))
	}
}
circ.sd <- function(x, int, na.rm=FALSE){
	if(length(x) == sum(is.na(x)) || sum(!is.na(x)) == 1){
		return(NA)
	} else if(sd(x, na.rm=TRUE) == 0){
		return(0)
	} else {
		require(circular)
		circ <- 2 * pi / int
		x.circ <- circular(x * circ, type="angles", units="radians", rotation="clock", modulo="2pi")
		x.int <- sd.circular(x.circ, na.rm=na.rm) / circ
		rm(circ, x.circ)
		return(as.numeric(x.int))
	}
}


#functions wet and dry periods
max.duration <- function(x) {
	r <- rle(x)
	if(length(temp <- which(r$values==1)) > 0){
		rmax <- max(r$lengths[temp])
	} else {
		rmax <- 0
	}
	return(rmax)
}
startDoyOfDuration <- function(x, duration=10) {
	r <- rle(x)
	if(length(r$lengths)==1 | sum(r$values==1 & r$lengths>=duration)==0 ){
		return (ifelse((length(r$lengths)==1 & (r$values==0 | r$lengths<duration)) | sum(r$values==1 & r$lengths>=10)==0, NA, 1)[1])
	} else {
		first10dry <- r$lengths[which(r$values==1 & r$lengths>=duration)][1] #pick first period
		if( !is.na(first10dry) ){
			ind <- which(r$lengths==first10dry & r$values==1)[1] #always pick start of first suitable period
		} else {
			ind <- -1
		}
		if(ind==1) {#start of period at beginning of year
			return(1)
		} else if(ind==-1) {#no period this year
			return(NA)
		} else {
			return(cumsum(r$lengths)[ind-1]+1)
		}
	}
}
endDoyAfterDuration <- function(x, duration=10) {
	r <- rle(x)
	if(length(r$lengths)==1 | sum(r$values==1 & r$lengths>=duration)==0 ){
		return (ifelse((length(r$lengths)==1 & (r$values==0 | r$lengths<duration)) | sum(r$values==1 & r$lengths>=duration)==0, 365, NA)[1])
	} else {
		last10dry <- (rl <- r$lengths[which(r$values==1 & r$lengths>=duration)])[length(rl)] #pick last period
		if( length(last10dry) > 0 ){
			ind <- (temp <- which(r$lengths==last10dry & r$values==1))[length(temp)]	#always pick end of last suitable period
		} else {
			ind <- -1
		}
		if(ind==-1) {#no period this year
			return(NA)
		} else {
			return(cumsum(r$lengths)[ind])
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
	vals <- list()
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


#function to extrapolate windspeeds measured at heights different than SoilWat required 2-m above ground
adjust.WindspeedHeight <- function(uz, height){
	# Allen RG, Walter IA, Elliott R, Howell T, Itenfisu D, Jensen M (2005) In The ASCE standardized reference evapotranspiration equation, pp. 59. ASCE-EWRI Task Committee Report.
	# input: windspeed [m/s] at height x
	# output: windspeed [m/s] at height 2 m
	
	stopifnot(all(uz >= 0) && height >= 2 )
	return( uz * 4.87 / log(67.8 * height - 5.42) )	# eqn. 33 in Allen et al. (2005)
}


#------------------------DAILY WEATHER
if(GriddedDailyWeatherFromMaurer2002_NorthAmerica){
	#extract daily weather information for the grid cell coded by latitude/longitude for each simulation run
	#Maurer, E. P., A. W. Wood, J. C. Adam, D. P. Lettenmaier, and B. Nijssen. 2002. A long-term hydrologically based dataset of land surface fluxes and states for the conterminous United States. Journal of Climate 15:3237-3251.
	
	dir.ex.maurer2002 <- file.path(dir.external, "ExtractGriddedDailyWeatherFromMaurer2002/DAILY_FORCINGS")
	stopifnot(file.exists(dir.ex.maurer2002))
	
	#function to be executed for each SoilWat-run
	ExtractGriddedDailyWeatherFromMaurer2002_NorthAmerica <- function(cellname,startYear=NULL, endYear=NULL){
		if(is.null(startYear))
			startYear <- simstartyr
		if(is.null(endYear))
			endYear <- endyr
		#read data from Maurer et al. 2002
		weath.data <- try(read.table(file=file.path(dir.ex.maurer2002, cellname), comment.char=""), silent=TRUE)
		if(!inherits(weath.data, "try-error")){
			colnames(weath.data) <- c("year", "month", "day", "prcp_mm", "Tmax_C", "Tmin_C", "Wind_mPERs")
			
			#times
			date <- seq(from=as.Date(with(weath.data[1, ], paste(year, month, day, sep="-")), format="%Y-%m-%d"),
					to=as.Date(with(weath.data[nrow(weath.data), ], paste(year, month, day, sep="-")), format="%Y-%m-%d"),
					by="1 day")
			doy <- 1 + as.POSIXlt(date)$yday
			
			years <- startYear:endYear
			n_years <- length(years)
			if(!all(years %in% unique(weath.data$year)))
				stop("simstartyr or endyr out of weather data range")
			weathDataList <- list()
			for(y in 1:n_years) {
				data.sw <- data.frame(doy, weath.data$Tmax_C, weath.data$Tmin_C, weath.data$prcp_mm/10)[weath.data$year == years[y], ]
				weathDataList[[y]]<-new("swWeatherData", data=data.matrix(data.sw),year=years[y])
			}
			names(weathDataList) <- as.character(years)
			return(weathDataList)
		} else {
			return(NULL)
		}
	}
}


#--------------------------------------------------------------------------------------------------#
#------------------------SET UP PARALLELIZATION
#used in: external dataset extractions, loop calling do_OneSite, and ensembles

workersN <- 1
parallel_init <- FALSE
if(any(actions == "external") || (actionWithSoilWat && runsN.todo > 0) || do.ensembles){
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
					if(!inherits(bcast.tempValue, "try-error")){
						mpi.bcast.Robj2slave(bcast.tempString)
						mpi.bcast.Robj2slave(bcast.tempValue)
						mpi.bcast.cmd(cmd=try(assign(bcast.tempString, bcast.tempValue)))
					} else {
						print(paste(obj, bcast.tempString, "not successful"))
					}
				}
				print(paste("object export took", round(difftime(Sys.time(), t.bcast, units="secs"), 2), "secs"))
			}
		}
	
		if(identical(parallel_backend, "snow")){
			if(!be.quiet) setDefaultClusterOptions(outfile="")
			#cl <-  makeCluster(num_cores, type="MPI", outfile="")
			cl <- snow::makeSOCKcluster(num_cores)
			clusterApply(cl, 1:num_cores, function(x) nodeNumber<<-x)
			#snow::clusterSetupRNG(cl) #random numbers setup
			doSNOW::registerDoSNOW(cl) 	# register foreach backend
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
		
		parallel_init <- TRUE
		if(!be.quiet) print(paste("SWSF prepares parallelization: ended after",  round(difftime(Sys.time(), t1, units="secs"), 2), "s"))
	}
}


#--------------------------------------------------------------------------------------------------#
#------------------------OBTAIN INFORMATION FROM EXTERNAL DATASETS PRIOR TO SIMULATION RUNS TO CREATE THEM
if(any(actions == "external") && any(exinfo > 0)){
	if(!be.quiet) print(paste("SWSF extracts information from external datasets prior to simulation runs: started at", t1 <- Sys.time()))
	stopifnot(file.exists(dir.external))
	
	source("2_SoilWatSimulationFramework_Part3of4_ExternalDataExtractions_v50.R", echo=FALSE, keep.source=FALSE)
	
	if(!be.quiet) print(paste("SWSF extracts information from external datasets prior to simulation runs: ended after",  round(difftime(Sys.time(), t1, units="secs"), 2), "s"))
}


#--------------------------------------------------------------------------------------------------#
#------------------------OBTAIN INFORMATION FROM TABLES PRIOR TO SIMULATION RUNS TO CREATE THEM

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
		
		if( !(any(names(sw_input_experimentals)[sw_input_experimentals_use == 1] == "LookupEvapCoeffFromTable")) ){#Use only if option is off in sw_input_experimentals and on in treatments
			if(any(is.na(sw_input_treatments$LookupEvapCoeffFromTable))) stop("ERROR: LookupEvapCoeffFromTable column in treatments cannot have any NAs.")
			if(!all(unique(sw_input_treatments$LookupEvapCoeffFromTable) %in% rownames(tr_input_EvapCoeff))) stop("ERROR: LookupEvapCoeffFromTable column values in treatments do not match up with trfile.LookupEvapCoeffFromTable row names.")
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
		
		if( !(any(names(sw_input_experimentals)[sw_input_experimentals_use == 1] == "LookupTranspRegionsFromTable")) ){#Use only if option is off in sw_input_experimentals
			if(any(is.na(sw_input_treatments$LookupTranspRegionsFromTable))) stop("ERROR: LookupTranspRegionsFromTable column in treatments cannot have any NAs.")
			if(!all(unique(sw_input_treatments$LookupTranspRegionsFromTable) %in% rownames(tr_input_TranspRegions))) stop("ERROR: LookupTranspRegionsFromTable column values in treatments do not match up with trfile.LookupTranspRegionsFromTable row names.")
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
		
		if( !(any(names(sw_input_experimentals)[sw_input_experimentals_use == 1] == "LookupSnowDensityFromTable")) ){#Use only if option is off in sw_input_experimentals
			if(any(is.na(sw_input_treatments$LookupSnowDensityFromTable))) stop("ERROR: LookupSnowDensityFromTable column in treatments cannot have any NAs.")
			if(!all(unique(sw_input_treatments$LookupSnowDensityFromTable) %in% rownames(tr_input_TranspRegions))) stop("ERROR: LookupSnowDensityFromTable column values in treatments do not match up with trfile.LookupSnowDensityFromTable row names.")
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
		TranspCoeffByVegType <- function(soillayer_no, trco_type, layers_depth, adjustType=c("positive", "inverse", "allToLast"))
		{
			#extract data from table by category
			trco.code <- as.character(tr_input_TranspCoeff_Code[, which(colnames(tr_input_TranspCoeff_Code) == trco_type)])
			trco <- rep(0, times=soillayer_no)
			trco.raw <- na.omit(tr_input_TranspCoeff[, which(colnames(tr_input_TranspCoeff) == trco_type)])
			
			if(trco.code == "DepthCM"){
				trco_sum <- ifelse((temp <- sum(trco.raw, na.rm=TRUE)) == 0 & is.na(temp), 1, temp)
				lup <- 1
				for(l in 1:soillayer_no){
					llow <- as.numeric(layers_depth[l])
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
			
			if(identical(adjustType, "positive")){
				trco <- trco / sum(trco)	#equivalent to: trco + (1 - sum(trco)) * trco / sum(trco)
			} else if(identical(adjustType, "inverse")){
				irows <- 1:max(which(trco > 0))
				trco[irows] <- trco[irows] + rev(trco[irows]) * (1 / sum(trco[irows]) - 1)	#equivalent to: trco + (1 - sum(trco)) * rev(trco) / sum(trco)
			} else if(identical(adjustType, "allToLast")){
				irow <- max(which(trco > 0))
				if(irow > 1){
					trco[irow] <- 1 - sum(trco[1:(irow - 1)]) 	#adding all the missing roots because soil is too shallow to the deepest available layer
				} else {
					trco[1] <- 1
				}
			}
			
			return(trco)
		}
		#cannot write data from sw_input_soils to datafile.soils
	}
	
	if(!be.quiet) print(paste("SWSF obtains information prior to simulation runs: ended after",  round(difftime(Sys.time(), t1, units="secs"), 2), "s"))
}


#--------------------------------------------------------------------------------------------------#
#------------------------CALCULATIONS PRIOR TO SIMULATION RUNS TO CREATE THEM
#------flags
temp <- matrix(data=do.PriorCalculations, ncol=2, nrow=length(do.PriorCalculations)/2, byrow=TRUE)
pcalcs <- data.frame(t(as.numeric(temp[,-1])))
names(pcalcs) <- temp[,1]

if(actionWithSoilWat) {
	do.GetClimateMeans <- 	(sum(sw_input_climscen_values_use[-1]) > 0) |
			pcalcs$EstimateConstantSoilTemperatureAtUpperAndLowerBoundaryAsMeanAnnualAirTemperature |
			sw_input_site_use$SoilTempC_atLowerBoundary |
			sw_input_site_use$SoilTempC_atUpperBoundary |
			pcalcs$EstimateInitialSoilTemperatureForEachSoilLayer |
			any(create_treatments == "PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996") |
			any(create_treatments == "AdjMonthlyBioMass_Temperature") |
			any(create_treatments == "AdjMonthlyBioMass_Precipitation") |
			any(create_treatments == "Vegetation_Biomass_ScalingSeason_AllGrowingORNongrowing")
}

if(any(actions == "create") && any(pcalcs > 0)){
	if(!be.quiet) print(paste("SWSF makes calculations prior to simulation runs: started at", t1 <- Sys.time()))

	if(pcalcs$CalculateBareSoilEvaporationCoefficientsFromSoilTexture){
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
	
	if(pcalcs$CalculateFieldCapacityANDWiltingPointFromSoilTexture){
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
	
	#------used during each simulation run: define functions here	
	if(pcalcs$EstimateConstantSoilTemperatureAtUpperAndLowerBoundaryAsMeanAnnualAirTemperature){
		sw_input_site_use$SoilTempC_atLowerBoundary <- 1 #set use flag
		sw_input_site_use$SoilTempC_atUpperBoundary <- 1
		#call function 'SiteClimate' in each SoilWat-run
	}
	
	if(pcalcs$EstimateInitialSoilTemperatureForEachSoilLayer){
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
	
	if(!be.quiet) print(paste("SWSF makes calculations prior to simulation runs: ended after",  round(difftime(Sys.time(), t1, units="secs"), 2), "s"))
}



#--------------------------------------------------------------------------------------------------#
#------------------------FUNCTION FOR A SOILWAT SIMULATION
if(actionWithSoilWat){
do_OneSite <- function(i, i_labels, i_SWRunInformation, i_sw_input_soillayers, i_sw_input_treatments, i_sw_input_cloud, i_sw_input_prod, i_sw_input_site, i_sw_input_soils, i_sw_input_weather, i_sw_input_climscen, i_sw_input_climscen_values) {
#i = i_sim: consecutive number of seq.todo, i.e., counting the simulation runs
#i_xxx = the i_tr-row of xxx for the i-th simulation run; if trowExperimentals > 0 then these will eventually be repeated, and below replaced with experimental values
#i_exp = the row of sw_input_experimentals for the i-th simulation run
#P_id is a unique id number for each scenario in each run
	
	time.sys <- Sys.time()
	
	if(!be.quiet) print(paste(i, ":", i_labels, "started at ", time.sys))
	flag.icounter <- formatC(i, width=counter.digitsN, format = "d", flag="0")
	
#-----------------------Check for experimentals
	if(trowExperimentals > 0 && length(create_experimentals) > 0) {
		i_exp <- (i - 1) %/% runs + 1	#first cycle through all sites (seq.tr), then repeat sites and cycle through trowExperimentals
		i_labels <- paste(flag.icounter, sw_input_experimentals[i_exp,1], i_labels, sep="_")
		
		#--put information from experimental design into appropriate input variables; create_treatments and the _use files were already adjusted for the experimental design when files were read in/created
		transferExpDesignToInput <- function(i_sw_input){
			ctemp <- (temp <- match(names(sw_input_experimentals)[sw_input_experimentals_use == 1], names(i_sw_input), nomatch=0))[!temp == 0]
			if(length(ctemp) > 0){
				cexp <- match(names(i_sw_input)[ctemp], names(sw_input_experimentals), nomatch=0)
				i_sw_input[ctemp] <- sw_input_experimentals[i_exp, cexp]
			}
			return(i_sw_input)
		}
		
		i_sw_input_treatments <- transferExpDesignToInput(i_sw_input_treatments)
		i_sw_input_soils <- transferExpDesignToInput(i_sw_input_soils)
		i_sw_input_site <- transferExpDesignToInput(i_sw_input_site)
		i_sw_input_prod <- transferExpDesignToInput(i_sw_input_prod)
	}
	
	
#------------------------Preparations for simulation run
	
	#Check what needs to be done
	#TODO this currently doesn't work in the database setup
	isdone.overallAggs <- rep(FALSE, scenario_No)
	if(any(simulation_timescales=="daily") && daily_no > 0){
		isdone.dailyAggs <- matrix(data=FALSE, nrow=daily_no, ncol=scenario_No)
	} else {
		isdone.dailyAggs <- TRUE
	}
	
	#set up task list: code: -1, don't do; 0, failed; 1, to do; 2, success
	tasks <- list(aggregate=1, #for now: ignoring to check time-series aggregations, i.e., assuming that if overallAggs is done, then time-series output was also completed
					create=1,
					execute=1)
	
	#----Get preparations done
	if( all(unlist(tasks) %in% c(-1, 1)) ){
		#get treatment sw.input.filenames for this run
		filesin <- swFilesIn
		
		if(!is.null(create_treatments) & tasks$create == 1){	
			if(any(create_treatments == "sw")){
				sw <- i_sw_input_treatments$sw
			}
			if(any(create_treatments == "filesin")){
				filesin <- i_sw_input_treatments$filesin
			}
			if(any(create_treatments == "prodin")){
				prodin <- i_sw_input_treatments$prodin
			}
			if(any(create_treatments == "siteparamin")){
				siteparamin <- i_sw_input_treatments$siteparamin
			}
			if(any(create_treatments == "soilsin")){
				soilsin <- i_sw_input_treatments$soilsin
			}
			if(any(create_treatments == "weathersetupin")){
				weatherin <- i_sw_input_treatments$weathersetupin
			}
			if(any(create_treatments == "cloudin")){
				cloudin <- i_sw_input_treatments$cloudin
			}
		}
		
		#if action is not create then get sw.input.filenames from filesin for this run
		if(tasks$create == -1){
			stop("This currently doesn't work") #TODO make it work low PR
			soilsin <- basename(unlist(strsplit(infiletext[10], split="[[:space:]]"))[1])
		}
		
		#------Learn about soil layer structure
		#determine number of soil layers = d and soildepth
		if(!any(create_treatments=="soilsin") & tasks$create == 1) {
			soildepth <- i_sw_input_soillayers$SoilDepth_cm
			layers_depth <- na.omit(as.numeric(i_sw_input_soillayers[2 + lmax]))
			if(!(length(d <- which(soildepth == layers_depth)) > 0)){	#soildepth is one of the lower layer boundaries
				d <- min(length(layers_depth), findInterval(soildepth, layers_depth)+1)	#soildepth is not one of the lower layer boundaries, the next deeper layer boundary is used
			}
		} else {# needs to be read from soilsin file
			if(tasks$create == -1) stop("This currently doesn't work") #TODO make it work low PR
			layers_depth <- swSoils_Layers(tr_soil[[soilsin]])[,1]
			d <- length(layers_depth)
			soildepth <- max(layers_depth)
		}
		
		#functions to obtain soil layer structures
		#layer sequence
		#########################Moved to Rsoilwat###########################
		#adjustLayersDepth <- function(layers_depth, d) return(round(layers_depth[1:d])) #The wrapper only handles 1-cm resolution of soil depths (maily because of the trco)
		#getLayersWidth <- function(layers_depth) return(diff(c(0, layers_depth)))
		#setLayerSequence <- function(d) return(1:d)
		#####################################################################
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
				val  <- NULL
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
		#Prepare directory structure in case SoilWat input/output is requested to be stored on disk
		if(saveSoilWatInputOutput) dir.create2(dir.sw.runs.sim <- file.path(dir.sw.runs, i_labels))
	}
	
	
#------------------------CREATE RUNS
	if( tasks$create == 1 ){	
		if(print.debug) print("Start of section 'create'")
		EVCO_done <- TRCO_done <- FALSE	#to check whether we get information for evaporation and transpiration coefficients
		TRRG_done <- FALSE #to check whether we get information for transpiration regions
		
		#------1. Step: Information for this SoilWat-run from prepared SoilWat-run stored in dir.sw.in
		#Make a local copy of the swInput object do not want to destroy orignal
		swRunScenariosData<-list(scenario_No)
		swRunScenariosData[[1]]<-swDataFromFiles
		
		#get folder and file names
		outsetupin <- swOutSetupIn
		dir.sw.runs.weather <- i_sw_input_treatments$LookupWeatherFolder
		
		if(print.debug) print("Start of LookupWeatherFolder")
		
		#write input file names and paths to first file, unless filesin is a treatment
		if(!any(create_treatments=="filesin")){
			swFiles_Years(swRunScenariosData[[1]]) <- paste(sw.inputs, .Platform$file.sep, yearsin, sep="")   
			swFiles_LogFile(swRunScenariosData[[1]]) <- paste(ifelse(sw.outputs == "", "", paste(sw.outputs, .Platform$file.sep, sep="")), sep="")   
			swFiles_SiteParams(swRunScenariosData[[1]]) <- paste(sw.inputs, .Platform$file.sep, siteparamin, sep="")   
			swFiles_Soils(swRunScenariosData[[1]]) <- paste(sw.inputs, .Platform$file.sep, soilsin, sep="")
			swFiles_WeatherSetup(swRunScenariosData[[1]]) <- paste(sw.inputs, .Platform$file.sep, weatherin, sep="")
			swFiles_WeatherPrefix(swRunScenariosData[[1]]) <- paste(sw.inputs, .Platform$file.sep, dirname.sw.runs.weather, .Platform$file.sep, filebasename.WeatherDataYear, sep="")
			swFiles_MarkovProbs(swRunScenariosData[[1]]) <- paste(sw.inputs, .Platform$file.sep, dirname.sw.runs.weather, .Platform$file.sep, sep="")
			swFiles_MarkovCov(swRunScenariosData[[1]]) <- paste(sw.inputs, .Platform$file.sep, dirname.sw.runs.weather, .Platform$file.sep, sep="")
			swFiles_Cloud(swRunScenariosData[[1]]) <- paste(sw.inputs, .Platform$file.sep, dirname.sw.runs.weather, .Platform$file.sep, cloudin, sep="")
			swFiles_Prod(swRunScenariosData[[1]]) <- paste(sw.inputs, .Platform$file.sep, prodin, sep="")
			swFiles_Estab(swRunScenariosData[[1]]) <- paste(sw.inputs, .Platform$file.sep, estabin, sep="")
			swFiles_SWCsetup(swRunScenariosData[[1]]) <- paste(sw.inputs, .Platform$file.sep, swcsetupin, sep="")
			swFiles_OutputPrefix(swRunScenariosData[[1]]) <- paste(ifelse(sw.outputs == "", "", paste(sw.outputs, .Platform$file.sep, sep="")), sep="")   
			swFiles_Output(swRunScenariosData[[1]]) <- paste(sw.inputs, .Platform$file.sep, outsetupin, sep="")
		}
		
		#adjust simulation years
		swYears_StartYear(swRunScenariosData[[1]]) <- as.integer(simstartyr)
		swYears_EndYear(swRunScenariosData[[1]]) <- as.integer(endyr)
		
		#------2. Step: a) Information for this SoilWat-run from treatment SoilWat input files stored in dir.sw.in.tr
		if(any(create_treatments=="sw"))
			print("SW treatment is not used because library Rsoilwat only uses one version of soilwat. Sorry")
		if(any(create_treatments=="filesin"))
			set_swFiles(swRunScenariosData[[1]]) <- tr_files[[filesin]]
		if(any(create_treatments=="prodin"))
			set_swProd(swRunScenariosData[[1]]) <- tr_prod[[prodin]]
		if(any(create_treatments=="siteparamin")){
			set_swSite(swRunScenariosData[[1]]) <- tr_site[[siteparamin]]
			TRRG_done <- TRUE
		}
		if(any(create_treatments=="soilsin")){
			set_swSoils(swRunScenariosData[[1]]) <- tr_soil[[soilsin]]
			EVCO_done <- TRCO_done <- TRUE
		}	
		if(any(create_treatments=="weathersetupin"))
			set_swWeather(swRunScenariosData[[1]]) <- tr_weather[[weatherin]]
		if(any(create_treatments=="cloudin"))
			set_swCloud(swRunScenariosData[[1]]) <- tr_cloud[[cloudin]]
		
		#------2. Step: b) Information for this SoilWat-run from treatment chunks stored in dir.sw.in.tr			
		#Do the lookup stuff for experimental design that was done for the treatment design before the call to call_OneSite, but couldn't for the experimental design because at that time information was unkown
		if(any(names(sw_input_experimentals)[sw_input_experimentals_use == 1] == "LookupEvapCoeffFromTable")) {
			if(any(is.na(i_sw_input_treatments$LookupEvapCoeffFromTable)) || !all(unique(i_sw_input_treatments$LookupEvapCoeffFromTable) %in% rownames(tr_input_EvapCoeff))) {
				print("ERROR: LookupEvapCoeffFromTable column in expirementals cannot have any NAs or name is not in tr_input_EvapCoeff table.")
				tasks$create <- 0
			} else {
				tempdat <- get.LookupEvapCoeffFromTable(evco_type=i_sw_input_treatments$LookupEvapCoeffFromTable, sw_input_soils_use=sw_input_soils_use, sw_input_soils=i_sw_input_soils)
				if(all(colSums(tempdat$sw_input_soils,na.rm=T)>0) && !any(is.na(colSums(tempdat$sw_input_soils))) ) {
					sw_input_soils_use <- tempdat$sw_input_soils_use
					i_sw_input_soils <- tempdat$sw_input_soils
				} else {
					print("ERROR: get.LookupEvapCoeffFromTable returned a Layer that didn't have a sum greater then 0 or had a NA.")
					tasks$create <- 0
				}
			}
		}
		if(any(names(sw_input_experimentals)[sw_input_experimentals_use == 1] == "LookupTranspRegionsFromTable")) {
			if(any(is.na(i_sw_input_treatments$LookupTranspRegionsFromTable)) || !all(unique(i_sw_input_treatments$LookupTranspRegionsFromTable) %in% rownames(tr_input_TranspRegions))) {
				print("ERROR: LookupTranspRegionsFromTable column in expirementals cannot have any NAs or name is not in LookupTranspRegionsFromTable data table.")
				tasks$create <- 0
			} else {
				tempdat <- get.LookupTranspRegionsFromTable(trtype=i_sw_input_treatments$LookupTranspRegionsFromTable, sw_input_soils_use=sw_input_soils_use, sw_input_soils=i_sw_input_soils)
				sw_input_soils_use <- tempdat$sw_input_soils_use
				i_sw_input_soils <- tempdat$sw_input_soils
			}
		}
		if(any(names(sw_input_experimentals)[sw_input_experimentals_use == 1] == "LookupSnowDensityFromTable")) {
			if(any(is.na(i_sw_input_treatments$LookupSnowDensityFromTable)) || !all(unique(i_sw_input_treatments$LookupSnowDensityFromTable) %in% rownames(tr_input_SnowD))) {
				print("ERROR: LookupSnowDensityFromTable column in expirementals cannot have any NAs or name is not in tr_input_SnowD data table.")
				tasks$create <- 0 
			} else {
				tempdat <- get.LookupSnowDensityFromTable(sdcategories=i_sw_input_treatments$LookupSnowDensityFromTable, sw_input_cloud_use=sw_input_cloud_use, sw_input_cloud=i_sw_input_cloud)
				sw_input_cloud_use <- tempdat$sw_input_cloud_use
				i_sw_input_cloud <- tempdat$sw_input_cloud
			}
		} 
		
		#Treatment chunks
		if(print.debug) print("Start of LookupTranspCoeff")
		if(any(create_treatments == "LookupTranspCoeffFromTable_Grass")){
			if(temp<-is.na(i_sw_input_treatments$LookupTranspCoeffFromTable_Grass)) print("LookupTranspCoeffFromTable_Grass for this run cannot be NA.")
			if(temp1<-!all(i_sw_input_treatments$LookupTranspCoeffFromTable_Grass %in% colnames(tr_input_TranspCoeff))) print("LookupTranspCoeffFromTable_Grass name for this run are not in tr_input_TranspCoeff table column names.")
			if(temp || temp1) {
				tasks$create <- 0
			} else {
				trco <- TranspCoeffByVegType(soillayer_no=d, trco_type=i_sw_input_treatments["LookupTranspCoeffFromTable_Grass"], layers_depth=layers_depth, adjustType="positive")
				if(!any(is.na(trco)) || sum(trco,na.rm=T) > 0){#trco does not have NA and sum is greater than 0.
					#set the use flags
					i.temp <- grepl(pattern=paste("Grass", "_TranspCoeff", sep=""), x=names(sw_input_soils_use))
					sw_input_soils_use[i.temp][1:length(trco)] <- rep(1, times=length(trco))
					#add data to sw_input_soils
					i_sw_input_soils[i.temp][1:length(trco)] <- trco
				} else {
					print("The function TranspCoeffByVegType returned NA or does not sum to greater than 0 for this run for type grass.")
					tasks$create <- 0
				}
			}
		}
		if(any(create_treatments == "LookupTranspCoeffFromTable_Shrub")){
			if(temp<-is.na(i_sw_input_treatments$LookupTranspCoeffFromTable_Shrub)) print("LookupTranspCoeffFromTable_Shrub for this run cannot be NA.")
			if(temp1<-!all(i_sw_input_treatments$LookupTranspCoeffFromTable_Shrub %in% colnames(tr_input_TranspCoeff))) print("LookupTranspCoeffFromTable_Shrub name for this run are not in tr_input_TranspCoeff table column names.")
			if(temp || temp1) {
				tasks$create <- 0
			} else {
				trco <- TranspCoeffByVegType(soillayer_no=d, trco_type=i_sw_input_treatments["LookupTranspCoeffFromTable_Shrub"], layers_depth=layers_depth, adjustType="inverse")
				#set the use flags
				if(!any(is.na(trco)) || sum(trco,na.rm=T) > 0){
					i.temp <- grepl(pattern=paste("Shrub", "_TranspCoeff", sep=""), x=names(sw_input_soils_use))
					sw_input_soils_use[i.temp][1:length(trco)] <- rep(1, times=length(trco))
					#add data to sw_input_soils
					i_sw_input_soils[i.temp][1:length(trco)] <- trco
				}  else {
					print("The function TranspCoeffByVegType returned NA or does not sum to greater than 0 for this run for type shrub.")
					tasks$create <- 0
				}
			}
		}
		if(any(create_treatments == "LookupTranspCoeffFromTable_Tree")){
			if(temp<-is.na(i_sw_input_treatments$LookupTranspCoeffFromTable_Tree)) print("LookupTranspCoeffFromTable_Tree for this run cannot be NA.")
			if(temp1<-!all(i_sw_input_treatments$LookupTranspCoeffFromTable_Tree %in% colnames(tr_input_TranspCoeff))) print("LookupTranspCoeffFromTable_Tree name for this run are not in tr_input_TranspCoeff table column names.")
			if(temp || temp1) {
				tasks$create <- 0
			} else {
				trco <- TranspCoeffByVegType(soillayer_no=d, trco_type=i_sw_input_treatments["LookupTranspCoeffFromTable_Tree"], layers_depth=layers_depth, adjustType="inverse")
				if(!is.na(trco)){				
					i.temp <- grepl(pattern=paste("Tree", "_TranspCoeff", sep=""), x=names(sw_input_soils_use))
					sw_input_soils_use[i.temp][1:length(trco)] <- rep(1, times=length(trco))
					#add data to sw_input_soils
					i_sw_input_soils[i.temp][1:length(trco)] <- trco
				} else {
					print("The function TranspCoeffByVegType returned NA or does not sum to greater than 0 for this run for type Tree.")
					tasks$create <- 0
				}
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
			ppt_old <- swWeather_MonScalingParams(swRunScenariosData[[1]])[,1]
			t1_old <- swWeather_MonScalingParams(swRunScenariosData[[1]])[,2]
			t2_old <- swWeather_MonScalingParams(swRunScenariosData[[1]])[,3]
			
			for (m in st_mo)
				swWeather_MonScalingParams(swRunScenariosData[[1]])[m,] <- c(ifelse(!is.na(ppt_sc[m]), ppt_sc[m], ppt_old[m]), ifelse(!is.na(t_sc[m]), t_sc[m], t1_old[m]), ifelse(!is.na(t_sc[m]), t_sc[m], t2_old[m]))
		}
		

		#------4. Step: Information from datafiles are added if flagged 'use' to SoilWat input files
		#add information from datafile to cloudin
		if(print.debug) print("Start of cloudin")
		wind <- with(i_sw_input_cloud, data.frame(wind_ms_1, wind_ms_2, wind_ms_3, wind_ms_4, wind_ms_5, wind_ms_6, wind_ms_7, wind_ms_8, wind_ms_9, wind_ms_10, wind_ms_11, wind_ms_12))
		if(do.wind <- datafile.windspeedAtHeightAboveGround != SoilWat.windspeedAtHeightAboveGround)
			wind <- adjust.WindspeedHeight(uz=wind, height=datafile.windspeedAtHeightAboveGround)
		
		if(sum(sw_input_cloud_use[-1]) > 0 | do.wind){
			#sky cover
			if(sum(sw_input_cloud_use[grepl(pattern="SkyC", x=names(sw_input_cloud_use))]) > 0) {
				sky <- with(i_sw_input_cloud, data.frame(SkyC_1, SkyC_2, SkyC_3, SkyC_4, SkyC_5, SkyC_6, SkyC_7, SkyC_8, SkyC_9, SkyC_10, SkyC_11, SkyC_12))
				swCloud_SkyCover(swRunScenariosData[[1]]) <- round(as.double(sky), 0)
			}
			#wind speed
			if(sum(sw_input_cloud_use[grepl(pattern="wind", x=names(sw_input_cloud_use))]) > 0 | do.wind) {
				swCloud_WindSpeed(swRunScenariosData[[1]]) <- round(as.double(wind), 2)
			}
			#relative humidity
			if(sum(sw_input_cloud_use[grepl(pattern="RH", x=names(sw_input_cloud_use))]) > 0) {
				rh <- with(i_sw_input_cloud, data.frame(RH_1, RH_2, RH_3, RH_4, RH_5, RH_6, RH_7, RH_8, RH_9, RH_10, RH_11, RH_12))
				swCloud_Humidity(swRunScenariosData[[1]]) <- round(as.double(rh), 0)
			}
			#snow density
			if(sum(sw_input_cloud_use[grepl(pattern="snowd", x=names(sw_input_cloud_use))]) > 0) {
				snowd <- with(i_sw_input_cloud, data.frame(snowd_1, snowd_2, snowd_3, snowd_4, snowd_5, snowd_6, snowd_7, snowd_8, snowd_9, snowd_10, snowd_11, snowd_12))
				if(i_SWRunInformation$Y_WGS84 < 0 && i_sw_input_cloud$SnowD_Hemisphere == "N" || i_SWRunInformation$Y_WGS84 > 0 && i_sw_input_cloud$SnowD_Hemisphere == "S"){	#adjust for hemisphere only if location and data are opposite
					snowd <- c(snowd[7:12], snowd[1:6])
				}
				swCloud_SnowDensity(swRunScenariosData[[1]]) <- round(as.double(snowd), 1)
			}
		}
		
		#add vegetation information	from datafile to prodin
		if(print.debug) print("Start of prodin")
		if(sum(sw_input_prod_use[-1]) > 0){
			#composition
			if(sum(use_comp <- unlist(sw_input_prod_use[grepl(pattern="Composition", x=names(sw_input_prod_use))])) > 0) {
				comp.datfile <- with(i_sw_input_prod, data.frame(Composition_GrassFraction, Composition_ShrubFraction, Composition_TreeFraction))
				swProd_Composition(swRunScenariosData[[1]])[use_comp] <- comp.datfile[use_comp]
			}
			#albedo
			if(sum(use_albedo <- unlist(sw_input_prod_use[grepl(pattern="Albedo", x=names(sw_input_prod_use))])) > 0) {
				albedo.datfile <- with(i_sw_input_prod, data.frame(Grass_Albedo, Shrub_Albedo, Tree_Albedo))
				swProd_Albedo(swRunScenariosData[[1]])[use_albedo] <- albedo.datfile[use_albedo]
			}
			#constant canopy height
			if(sum(use_height <- unlist(sw_input_prod_use[grepl(pattern="CanopyHeight_Constant", x=names(sw_input_prod_use))])) > 0) {
				height.datfile <- with(i_sw_input_prod, data.frame(Grass_CanopyHeight_Constant_cm, Shrub_CanopyHeight_Constant_cm, Tree_CanopyHeight_Constant_cm))
				swProd_CanopyHeight(swRunScenariosData[[1]])[5,][use_height] <- height.datfile[use_height]
			}
			#flag for hydraulic redistribution
			if(sum(use_HD <- unlist(sw_input_prod_use[grepl(pattern="HydRed", x=names(sw_input_prod_use))])) > 0) {
				HD.datfile <- with(i_sw_input_prod, data.frame(Grass_HydRed_OnOff, Shrub_HydRed_OnOff, Tree_HydRed_OnOff))
				swProd_HydrRedstro_use(swRunScenariosData[[1]])[use_HD] <- as.logical(HD.datfile[use_HD])
			}
			#biomass components TODO Check This
			biomassComponents <- function(FunctGroup){
				if(	sum(litt <- sw_input_prod_use[grepl(pattern=paste(FunctGroup, "_Litter", sep=""), x=names(sw_input_prod_use))]) + 
						sum(biom <- sw_input_prod_use[grepl(pattern=paste(FunctGroup, "_Biomass", sep=""), x=names(sw_input_prod_use))]) +
						sum(live <- sw_input_prod_use[grepl(pattern=paste(FunctGroup, "_FractionLive", sep=""), x=names(sw_input_prod_use))]) +
						sum(laiconv <- sw_input_prod_use[grepl(pattern=paste(FunctGroup, "_LAIconv", sep=""), x=names(sw_input_prod_use))])			> 0) {
					
					for (m in st_mo){
						mo.dat <- with(i_sw_input_prod, c(	ifelse(litt[m], eval(parse(text=paste(FunctGroup, "_Litter_m", m, sep=""))), NA),
										ifelse(biom[m], eval(parse(text=paste(FunctGroup, "_Biomass_m", m, sep=""))), NA),
										ifelse(live[m], eval(parse(text=paste(FunctGroup, "_FractionLive_m", m, sep=""))), NA),
										ifelse(laiconv[m], eval(parse(text=paste(FunctGroup, "_LAIconv_m", m, sep=""))), NA)))
						if(FunctGroup=="Grass")
							swProd_MonProd_grass(swRunScenariosData[[1]])[m,c(litt[m],biom[m],live[m],laiconv[m])]  <- mo.dat[!is.na(mo.dat)]
						if(FunctGroup=="Shrub")
							swProd_MonProd_shrub(swRunScenariosData[[1]])[m,c(litt[m],biom[m],live[m],laiconv[m])]  <- mo.dat[!is.na(mo.dat)]
						if(FunctGroup=="Tree")
							swProd_MonProd_tree(swRunScenariosData[[1]])[m,c(litt[m],biom[m],live[m],laiconv[m])]  <- mo.dat[!is.na(mo.dat)]
					}
				}
				if(FunctGroup=="Grass")
					return(swProd_MonProd_grass(swRunScenariosData[[1]]))
				if(FunctGroup=="Shrub")
					return(swProd_MonProd_shrub(swRunScenariosData[[1]]))
				if(FunctGroup=="Tree")
					return(swProd_MonProd_tree(swRunScenariosData[[1]]))
			}
			swProd_MonProd_grass(swRunScenariosData[[1]]) <- biomassComponents(FunctGroup="Grass")
			swProd_MonProd_shrub(swRunScenariosData[[1]]) <- biomassComponents(FunctGroup="Shrub")
			swProd_MonProd_tree(swRunScenariosData[[1]])  <- biomassComponents(FunctGroup="Tree")
		}
		#Moved adjust to southern Hemi
		
		#add site information to siteparamin
		if(print.debug) print("Start of siteparamin")
		if(sum(sw_input_site_use[-1]) > 0){
			site_swc_use <- as.logical(c(sw_input_site_use$SWC_min,sw_input_site_use$SWC_init,sw_input_site_use$SWC_wet))
			if(any(site_swc_use)){
				swSite_SWClimits(swRunScenariosData[[1]])[temp] <- c(i_sw_input_site$SWC_min,i_sw_input_site$SWC_init,i_sw_input_site$SWC_wet)[temp]
			}
			site_modelflag_use <- as.logical(c(sw_input_site_use$SWC_YearlyReset,sw_input_site_use$SWC_Deepdrain))
			if(any(site_modelflag_use)){
				swSite_ModelFlags(swRunScenariosData[[1]])[site_modelflag_use] <- c(i_sw_input_site$SWC_YearlyReset,i_sw_input_site$SWC_Deepdrain)[site_modelflag_use]
			}
			site_modelcoef_use <- as.logical(c(sw_input_site_use$PET_multiplier,sw_input_site_use$RunoffPercent_fromPondedWater))
			if(any(site_modelcoef_use)){
				swSite_ModelCoefficients(swRunScenariosData[[1]])[site_modelcoef_use] <- c(i_sw_input_site$PET_multiplier, i_sw_input_site$RunoffPercent_fromPondedWater)[site_modelcoef_use]
			}
			#replace if in treatment file
			if(sw_input_site_use$Param_UnsaturatedPercolation){
				swSite_DrainageCoefficient(swRunScenariosData[[1]]) <- i_sw_input_site$Param_UnsaturatedPercolation
			}
			if(sw_input_site_use$Slope){
				swSite_IntrinsicSiteParams(swRunScenariosData[[1]])[3] <- i_sw_input_site$Slope
			}
			if(sw_input_site_use$Aspect){
				swSite_IntrinsicSiteParams(swRunScenariosData[[1]])[4] <- i_sw_input_site$Aspect
			}
			#Moved sw_input_site_use$SoilTempC_atLowerBoundary
			if(sw_input_site_use$SoilTemp_Flag){
				swSite_SoilTemperatureFlag(swRunScenariosData[[1]]) <- i_sw_input_site$SoilTemp_Flag
			}
			rm(site_swc_use,site_modelflag_use,site_modelcoef_use)
		}
		swSite_IntrinsicSiteParams(swRunScenariosData[[1]])[1] <- i_SWRunInformation$Y_WGS84 * pi / 180
		if(is.finite(i_SWRunInformation$ELEV_m))	swSite_IntrinsicSiteParams(swRunScenariosData[[1]])[2] <- i_SWRunInformation$ELEV_m
		
		#add soil information to soilsin
		if(print.debug) print("Start of soilsin")
		done.Imperm_L1 <- FALSE
		if(sw_input_soils_use$Imperm_L1 == 1 && any(create_treatments == "soilsin")){
			tempdat <- swSoils_Layers(swRunScenariosData[[1]])
			tempdat[1, "impermeability_frac"] <- i_sw_input_soils$Imperm_L1
			swSoils_Layers(swRunScenariosData[[1]]) <- tempdat
			done.Imperm_L1 <- TRUE
		}
		if(sum(sw_input_soils_use[-1] + ifelse(done.Imperm_L1, -1, 0)) - sum(use_transpregion <- as.numeric(sw_input_soils_use[paste("TranspRegion_L", ld, sep="")])) > 0){
			tempdat <- matrix(data=NA, nrow=SoilLayer_MaxNo, ncol=12)
			colnames(tempdat) <- c("depth", "bulkd", "fieldc", "wiltp", "evco", "trco_grass", "trco_shrub", "trco_tree", "sand", "clay", "imperm", "soiltemp")
			
			#recalculate soil layer structure, because any(create_treatments=="soilsin") and soilsin may have a different soil layer structure than the datafiles
			layers_depth.datafile <- (temp <- as.numeric(na.omit(unlist(i_sw_input_soillayers[match(paste("depth_L", 1:SoilLayer_MaxNo, sep=""), colnames(i_sw_input_soillayers))]))))[temp <= as.numeric(i_sw_input_soillayers["SoilDepth_cm"])]
			layers_depth.soilsin <- swSoils_Layers(swRunScenariosData[[1]])[,1]
			mergeDatafileWithSoilsin <- FALSE
			
			if(identical(layers_depth.datafile, layers_depth.soilsin)){	#same soil layer structure in soilsin and datafile => combine data
				#soil texture data from SoilWat input file
				tempdat <- swSoils_Layers(swRunScenariosData[[1]])
				colnames(tempdat) <- c("depth", "bulkd", "fieldc", "wiltp", "evco", "trco_grass", "trco_shrub", "trco_tree", "sand", "clay", "imperm", "soiltemp")#names might be diff
				mergeDatafileWithSoilsin <- TRUE
				
			} else { #different soil layer structure in soilsin and datafile AND since variables are flagged in sw_input_soils_use => use only datafile values
				d <- max(1, min(length(layers_depth.datafile), findInterval(i_sw_input_soillayers$SoilDepth_cm - sqrt(.Machine$double.neg.eps), c(0, layers_depth.datafile)), na.rm=TRUE), na.rm=TRUE)
				layers_depth <- adjustLayersDepth(layers_depth.datafile, d)
				layers_width <- getLayersWidth(layers_depth)
				ld <- setLayerSequence(d)
				
				DeepestTopLayer <- setDeepestTopLayer(d)
				topL <- setTopLayer(d)
				bottomL <- setBottomLayer(d)
			}

			#flags for use of texture data from datafile
			use_bulkd <- as.numeric(sw_input_soils_use[paste("BD_L", ld, sep="")])
			use_fieldc <- as.numeric(sw_input_soils_use[paste("FieldC_L", ld, sep="")])
			use_pwp <- as.numeric(sw_input_soils_use[paste("WiltP_L", ld, sep="")])
			sum_use_evco <- sum(sw_input_soils_use[paste("EvapCoeff_L", ld, sep="")])
			sum_use_trco_grass <- sum(sw_input_soils_use[paste("Grass_TranspCoeff_L", ld, sep="")])
			sum_use_trco_shrub <- sum(sw_input_soils_use[paste("Shrub_TranspCoeff_L", ld, sep="")])
			sum_use_trco_tree <- sum(sw_input_soils_use[paste("Tree_TranspCoeff_L", ld, sep="")])
			use_sand <- as.numeric(sw_input_soils_use[paste("Sand_L", ld, sep="")])
			use_clay <- as.numeric(sw_input_soils_use[paste("Clay_L", ld, sep="")])
			use_imperm <- as.numeric(sw_input_soils_use[paste("Imperm_L", ld, sep="")])
			
			if(mergeDatafileWithSoilsin || sum_use_evco > 0) EVCO_done <- TRUE
			if(mergeDatafileWithSoilsin || (sum_use_trco_grass > 0 && sum_use_trco_shrub > 0 && sum_use_trco_tree > 0)) TRCO_done <- TRUE
			
			#tr and ev coefficients data from datafile
			evco <- as.numeric(i_sw_input_soils[paste("EvapCoeff_L", ld, sep="")])
			trco_grass <- as.numeric(i_sw_input_soils[paste("Grass_TranspCoeff_L", ld, sep="")])
			trco_shrub <- as.numeric(i_sw_input_soils[paste("Shrub_TranspCoeff_L", ld, sep="")])
			trco_tree <- as.numeric(i_sw_input_soils[paste("Tree_TranspCoeff_L", ld, sep="")])
			
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
						ifelse(use_bulkd[l], as.numeric(i_sw_input_soils[paste("BD_L", l, sep="")]), tempdat[l, "bulkd"]),
						ifelse(use_fieldc[l], as.numeric(i_sw_input_soils[paste("FieldC_L", l, sep="")]), tempdat[l, "fieldc"]),
						ifelse(use_pwp[l], as.numeric(i_sw_input_soils[paste("WiltP_L", l, sep="")]), tempdat[l, "wiltp"]),
						ifelse(!is.na(temp <- ifelse(sum_use_evco, evco[l], tempdat[l, "evco"])), temp, 0),
						ifelse(!is.na(temp <- ifelse(sum_use_trco_grass, trco_grass[l], tempdat[l, "trco_grass"])), temp, 0),
						ifelse(!is.na(temp <- ifelse(sum_use_trco_shrub, trco_shrub[l], tempdat[l, "trco_shrub"])), temp, 0),
						ifelse(!is.na(temp <- ifelse(sum_use_trco_tree, trco_tree[l], tempdat[l, "trco_tree"])), temp, 0),
						ifelse(use_sand[l], as.numeric(i_sw_input_soils[paste("Sand_L", l, sep="")]), tempdat[l, "sand"]),
						ifelse(use_clay[l], as.numeric(i_sw_input_soils[paste("Clay_L", l, sep="")]), tempdat[l, "clay"]),
						ifelse(!is.na(temp <- ifelse(use_imperm[l], as.numeric(i_sw_input_soils[paste("Imperm_L", l, sep="")]), tempdat[l, "imperm"])), temp, 0),
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
			
			# Resize swSoils Layers to proper size
			if(length(ld)==1) {
				swSoils_Layers(swRunScenariosData[[1]]) <- matrix(data=swSoils_Layers(swRunScenariosData[[1]])[ld,], nrow=length(ld), ncol=12, byrow=TRUE, dimnames=list(numeric(),c("depth", "bulkd", "fieldc", "wiltp", "evco", "trco_grass", "trco_shrub", "trco_tree", "sand", "clay", "imperm", "soiltemp")))
			} else {
				if(nrow(swSoils_Layers(swRunScenariosData[[1]])) != d) {
					if(nrow(swSoils_Layers(swRunScenariosData[[1]])) > d) {
						swSoils_Layers(swRunScenariosData[[1]]) <- swSoils_Layers(swRunScenariosData[[1]])[ld,]
					} else {
						swSoils_Layers(swRunScenariosData[[1]]) <- rbind( swSoils_Layers(swRunScenariosData[[1]]), matrix(NA,nrow=d-nrow(swSoils_Layers(swRunScenariosData[[1]])), ncol=ncol(swSoils_Layers(swRunScenariosData[[1]]))) )
					}
				}
			}
			this_soil <- soildat[1, ]
			for (l in ld){
				missingtext <- ifelse(soildat[l, "bulkd"] > 0 & soildat[l, "fieldc"] > 0 & soildat[l, "wiltp"] > 0 & soildat[l, "sand"] > 0 & soildat[l, "clay"] > 0,"", paste("Layer ",l,": soil data missing for this layer -> data used from previous layer */",sep=""))
				if(nchar(missingtext)==0){
					this_soil <- soildat[l, ]
				} else {
					swLog_setLine(swRunScenariosData[[1]]) <- missingtext
					this_soil <- c(soildat[l, "depth"], this_soil[2:4], soildat[l, "evco"], soildat[l, "trco_grass"], soildat[l, "trco_shrub"], soildat[l, "trco_tree"], this_soil[9:10], soildat[l, "imperm"], soildat[l, "soiltemp"])
				}
				swSoils_Layers(swRunScenariosData[[1]])[l,] <- this_soil
			}
			
			#SoilWat needs positive values for sand and clay contents
			temp <- colnames(swSoils_Layers(swRunScenariosData[[1]]))
			if(!all(swSoils_Layers(swRunScenariosData[[1]])[, grep("sand", temp)] > 0, swSoils_Layers(swRunScenariosData[[1]])[, grep("clay", temp)] > 0)){
				warning(paste("Run:", i, ", no or zero sand or clay content: SoilWat will likely crash"))
				tasks$create <- 0
			}		
		}
		
		
		#add transpiration regions information to siteparamin
		if(print.debug) print("Start of transpregion")
		if(sum(use_transpregion) > 0){
			tr <- max(tr.layers <- na.omit(as.numeric(i_sw_input_soils[paste("TranspRegion_L", ld, sep="")]))) # max transpiration region
			
			TranspirationRegions <- matrix(data=NA,nrow=4,ncol=2)
			colnames(TranspirationRegions)<-c("ndx","layer")
			
			ltreg.last <- 0
			for(tri in 1:4){
				ltreg <- ifelse(length(ind <- which(tr.layers==tri)) > 0, max(ind), -1)
				ltreg <- ifelse(ltreg>ltreg.last, ltreg, ltreg.last+1)
				ltreg <- ifelse(ltreg>d & tri==1, d, ltreg)
				
				if(tri <= tr & tri <= d & ltreg <= d | tri == 1) TranspirationRegions[tri,] <- as.integer(c(tri,ltreg))
				ltreg.last <- ltreg
			}
			tr_rows<-rowSums(is.na(TranspirationRegions))!=2 #used to get rid of NA rows
			if(sum(tr_rows) == 0) {
				stop("Transpiration Regions in Site can not be empty")
			} else if(sum(tr_rows) == 1) {
				swSite_TranspirationRegions(swRunScenariosData[[1]]) <- matrix(data=TranspirationRegions[tr_rows,],nrow=1,ncol=2,byrow=T,dimnames=list(numeric(),c("ndx","layer")))
				TRRG_done <- TRUE
			} else {
				swSite_TranspirationRegions(swRunScenariosData[[1]]) <- TranspirationRegions[tr_rows,]
				TRRG_done <- TRUE
			}
		}
		
		#add weather setup information to weatherin
		if(sw_input_weather_use$SnowFlag)
			swWeather_UseSnow(swRunScenariosData[[1]]) <- as.logical(i_sw_input_weather$SnowFlag)
		if(sw_input_weather_use$SnowDrift_Percent)
			swWeather_pct_SnowDrift(swRunScenariosData[[1]]) <- i_sw_input_weather$SnowDrift_Percent
		if(sw_input_weather_use$RunOffOnPerSnowmelt_Percent)
			swWeather_pct_SnowRunoff(swRunScenariosData[[1]]) <- i_sw_input_weather$RunOffOnPerSnowmelt_Percent
		swWeather_FirstYearHistorical(swRunScenariosData[[1]]) <- simstartyr
				
		
		swOUT_TimeStep(swRunScenariosData[[1]]) <- sapply(simulation_timescales, function(x) ifelse(x=="daily", 1, ifelse(x=="weekly", 2, ifelse(x=="monthly", 3, ifelse(x=="yearly",4,5)))) )-1

		#############TODO Get Weather Data################
		i_sw_weatherList <- list()
		if(GriddedDailyWeatherFromMaurer2002_NorthAmerica & !any(create_treatments == "LookupWeatherFolder")){ #obtain external weather information that needs to be executed for each run
			dirname.sw.runs.weather <- paste("data", format(28.8125+round((i_SWRunInformation$Y_WGS84-28.8125)/0.125,0)*0.125, nsmall=4), format(28.8125+round((i_SWRunInformation$X_WGS84-28.8125)/0.125,0)*0.125, nsmall=4), sep="_")
			i_sw_weatherList[[1]] <- ExtractGriddedDailyWeatherFromMaurer2002_NorthAmerica(cellname=dirname.sw.runs.weather,startYear=ifelse(any(create_treatments=="YearStart"), i_sw_input_treatments$YearStart, simstartyr), endYear=ifelse(any(create_treatments=="YearEnd"), i_sw_input_treatments$YearEnd, endyr))
			if(is.null(i_sw_weatherList[[1]])) stop("ExtractGriddedDailyWeatherFromMaurer2002_NorthAmerica failed")
		} else {
			if(getCurrentWeatherDataFromDatabase) {
				.local <- function(i){
					dbW_setConnection(dbFilePath=dbWeatherDataFile, FALSE)
					if(!exists("con") || !isIdCurrent(con) || !parallel_runs) {
						drv <<- dbDriver("SQLite")
						con <<- dbConnect(drv, dbname=name.OutputDB)
					}
					temp <- dbGetQuery(con, paste("SELECT WeatherFolder FROM header WHERE P_id=",((i-1)*scenario_No+1)))[1,1]
					dbDisconnect(con)
					i_sw_weatherList <- list()
					for(k in 1:ifelse(getScenarioWeatherDataFromDatabase, length(climate.conditions), 1))
						i_sw_weatherList[[k]] <- dbW_getWeatherData(Label=temp,startYear=ifelse(any(create_treatments=="YearStart"), i_sw_input_treatments$YearStart, simstartyr), endYear=ifelse(any(create_treatments=="YearEnd"), i_sw_input_treatments$YearEnd, endyr), Scenario=climate.conditions[k])
					return(i_sw_weatherList)
				}
				i_sw_weatherList <- try(.local(i), silent=TRUE)
				if(inherits(i_sw_weatherList, "try-error")) tasks$create <- 0
			} else {
				i_sw_weatherList[[1]] <- getWeatherData_folders(LookupWeatherFolder=file.path(dir.sw.in.tr, "LookupWeatherFolder"),weatherDirName=temp,filebasename=filebasename,startYear=ifelse(any(create_treatments=="YearStart"), i_sw_input_treatments$YearStart, simstartyr), endYear=ifelse(any(create_treatments=="YearEnd"), i_sw_input_treatments$YearEnd, endyr))
			}
		}
		
		#copy and make climate scenarios from datafiles
		grasses.c3c4ann.fractions <- rep(list(rep(NA, 3)), scenario_No) #Init fractions of C3, C4, and annual grasses of grass-vegetation type fraction; used in create and aggregate
		ClimatePerturbationsVals <- matrix(data=c(rep(1,12),rep(0,24)),nrow=scenario_No, ncol=12*3) #, dimnames=list(NULL,paste(rep(paste("ClimatePerturbations.", c("PrcpMultiplier.m", "TmaxAddand.m", "TminAddand.m"), sep=""), each=12), st_mo, rep(c("_none", "_C", "_C"), each=12), "_const", sep=""))
		if(tasks$create > 0) for(sc in 1:scenario_No){
			if(sc > 1){
				swRunScenariosData[[sc]] <- swRunScenariosData[[1]]
			} else {
				if(do.GetClimateMeans){
					if(print.debug) print("Start of get SiteClimate")
					do.C4vars <- any(create_treatments == "PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996") || aon$dailyC4_TempVar
					#redo SiteClimate_Ambient
					SiteClimate_Ambient <- sw_SiteClimate_Ambient(weatherList=i_sw_weatherList[[1]], year.start=min(simTime$useyrs), year.end=max(simTime$useyrs), do.C4vars=do.C4vars, simTime2=simTime2)
				}
			}
			
			if(!getScenarioWeatherDataFromDatabase) {
				#get climate change information
				use_pptValscen <- sw_input_climscen_values_use[, pptVal.colnames <- paste("PPTmm_m", st_mo, "_sc", formatC(sc-1, width=2, format="d", flag="0"), sep="")]
				use_tempValMinScen <- sw_input_climscen_values_use[, tempValMin.colnames <- paste("TempC_min_m", st_mo, "_sc", formatC(sc-1, width=2, format="d", flag="0"), sep="")]
				use_tempValMaxScen <- sw_input_climscen_values_use[, tempValMax.colnames <- paste("TempC_max_m", st_mo, "_sc", formatC(sc-1, width=2, format="d", flag="0"), sep="")]
				
				use_pptscen <- sw_input_climscen_use[, ppt.colnames <- paste("PPTfactor_m", st_mo, "_sc", formatC(sc-1, width=2, format="d", flag="0"), sep="")]
				use_tempMinScen <- sw_input_climscen_use[, tempMin.colnames <- paste("deltaTempC_min_m", st_mo, "_sc", formatC(sc-1, width=2, format="d", flag="0"), sep="")]
				use_tempMaxScen <- sw_input_climscen_use[, tempMax.colnames <- paste("deltaTempC_max_m", st_mo, "_sc", formatC(sc-1, width=2, format="d", flag="0"), sep="")]
				
				if(	sum(use_pptValscen) + sum(use_tempValMinScen) + sum(use_tempValMaxScen) > 0){
					#convert climate change values to factors
					#read values from datafile
					pptVal_sc <- unlist(i_sw_input_climscen_values[, pptVal.colnames])
					tVal_min_sc <- unlist(i_sw_input_climscen_values[, tempValMin.colnames])
					tVal_max_sc <- unlist(i_sw_input_climscen_values[, tempValMax.colnames])
					#calculate change factors
					ppt_sc <- pptVal_sc / (10 * SiteClimate_Ambient$meanMonthlyPPTcm)
					if(sum(abs(tVal_max_sc - tVal_min_sc)) > sqrt(.Machine$double.eps)){
						t_min_sc <- tVal_min_sc - SiteClimate_Ambient$minMonthlyTempC
						t_max_sc <- tVal_max_sc - SiteClimate_Ambient$maxMonthlyTempC
					} else { #no information for tmin, tmax by GCM -> tmin=tmax=tmean
						t_min_sc <- t_max_sc <- tVal_min_sc - SiteClimate_Ambient$meanMonthlyTempC
					}
				} else if(	sum(use_pptscen) + sum(use_tempMinScen) + sum(use_tempMaxScen) > 0){
					#read climate change factors from datafile
					ppt_sc <- unlist(i_sw_input_climscen[, ppt.colnames])
					t_min_sc <- unlist(i_sw_input_climscen[, tempMin.colnames])
					t_max_sc <- unlist(i_sw_input_climscen[, tempMax.colnames])
				} else {
					ppt_sc <- rep(1, times=12)
					t_min_sc <- rep(0, times=12)
					t_max_sc <- rep(0, times=12)
				}
				#guarantee that all entries are finite: this may not be the case for instance if any(meanMonthlyClimate$meanMonthlyPPTcm == 0)
				ppt_sc <- temp_ppt_sc <- ifelse(is.finite(ppt_sc), ppt_sc, 1)
				t_min_sc <- ifelse(is.finite(t_min_sc), t_min_sc, 0)
				t_max_sc <- ifelse(is.finite(t_max_sc), t_max_sc, 0)
				
				if(sc > 1){
					if(any(create_treatments=="ClimateScenario_Temp_PerturbationInMeanSeasonalityBothOrNone") && !grepl("Both", i_sw_input_treatments$ClimateScenario_Temp_PerturbationInMeanSeasonalityBothOrNone, ignore.case=T)){
						if(grepl("Mean", i_sw_input_treatments$ClimateScenario_Temp_PerturbationInMeanSeasonalityBothOrNone, ignore.case=T)) {
							t_min_sc <- rep(mean(t_min_sc), times=12)
							t_max_sc <- rep(mean(t_max_sc), times=12)
						}
						if(grepl("Seasonality", i_sw_input_treatments$ClimateScenario_Temp_PerturbationInMeanSeasonalityBothOrNone, ignore.case=T)) {
							t_min_sc <- t_min_sc - mean(t_min_sc)
							t_max_sc <- t_max_sc - mean(t_max_sc)
						} 
						if(grepl("None", i_sw_input_treatments$ClimateScenario_Temp_PerturbationInMeanSeasonalityBothOrNone, ignore.case=T)) {
							t_min_sc <- rep(0, times=12)
							t_max_sc <- rep(0, times=12)
						}
					}
					if(any(create_treatments=="ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone") && !grepl("Both", i_sw_input_treatments$ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone, ignore.case=T)){
						temp_map_sc <- sum(SiteClimate_Ambient$meanMonthlyPPTcm * temp_ppt_sc)
						if(grepl("Mean", i_sw_input_treatments$ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone, ignore.case=T)) ppt_sc = rep(temp_map_sc / SiteClimate_Ambient$MAP_cm, times=12)
						if(grepl("Seasonality", i_sw_input_treatments$ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone, ignore.case=T)) ppt_sc = ppt_sc * SiteClimate_Ambient$MAP_cm / temp_map_sc
						if(grepl("None", i_sw_input_treatments$ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone, ignore.case=T)) ppt_sc = rep(1, times=12)
					}
				}				
				
				ppt_old <- (temp <- swWeather_MonScalingParams(swRunScenariosData[[sc]]))[,1]
				t_max_old <- temp[,2]
				t_min_old <- temp[,3]
				
				#write information into weatherin
				if(sum(use_pptValscen) + sum(use_tempValMinScen) + sum(use_tempValMaxScen) + sum(use_pptscen) + sum(use_tempMinScen) + sum(use_tempMaxScen) > 0){
					ppt_f <- ppt_sc
					t_min_f <- t_min_sc
					t_max_f <- t_max_sc
				} else {
					ppt_f <- ppt_old
					t_min_f <- t_min_old
					t_max_f <- t_max_old
				}
				
				MonthlyScalingParams<-matrix(data=c(ppt_f,t_max_f,t_min_f),nrow=12,ncol=3)
				colnames(MonthlyScalingParams)<-c("PPT","MaxT","MinT")
				rownames(MonthlyScalingParams)<-c("January","February","March","April","May","June","July","August","September","October","November","December")
				
				swWeather_MonScalingParams(swRunScenariosData[[sc]]) <- MonthlyScalingParams
				ClimatePerturbationsVals[sc,1:12] <- MonthlyScalingParams[,1]
				ClimatePerturbationsVals[sc,13:24] <- MonthlyScalingParams[,2]
				ClimatePerturbationsVals[sc,25:36] <- MonthlyScalingParams[,3]

				#Update climate data with climate scenario information
				if(do.GetClimateMeans){
					SiteClimate_Scenario <- list()
					SiteClimate_Scenario$meanMonthlyPPTcm <- SiteClimate_Ambient$meanMonthlyPPTcm * ppt_f
					tmean_f <- apply(cbind(t_min_f, t_max_f), MARGIN=1, FUN=mean)
					SiteClimate_Scenario$meanMonthlyTempC <- SiteClimate_Ambient$meanMonthlyTempC + tmean_f
					SiteClimate_Scenario$minMonthlyTempC <- SiteClimate_Ambient$minMonthlyTempC + t_min_f
					SiteClimate_Scenario$maxMonthlyTempC <- SiteClimate_Ambient$maxMonthlyTempC + t_max_f
					SiteClimate_Scenario$MAP_cm <- sum(SiteClimate_Scenario$meanMonthlyPPTcm)
					SiteClimate_Scenario$MAT_C <- mean(SiteClimate_Scenario$meanMonthlyTempC)
					if(do.C4vars){
						SiteClimate_Scenario$dailyTempMin <- SiteClimate_Ambient$dailyTempMin + t_min_f[simTime2$month_ForEachUsedDay]
						SiteClimate_Scenario$dailyTempMean <- SiteClimate_Ambient$dailyTempMean + tmean_f[simTime2$month_ForEachUsedDay]
						SiteClimate_Scenario$dailyC4vars <- sw_dailyC4_TempVar(SiteClimate_Scenario$dailyTempMin, SiteClimate_Scenario$dailyTempMean, simTime2)
					}
				}
			} else {
				SiteClimate_Scenario <- sw_SiteClimate_Ambient(weatherList=i_sw_weatherList[[sc]], year.start=min(simTime$useyrs), year.end=max(simTime$useyrs), do.C4vars=do.C4vars, simTime2=simTime2)
				if(sc > 1){
					ppt_sc <- (temp <- swWeather_MonScalingParams(swRunScenariosData[[sc]]))[,1]
					t_max <- temp[,2]
					t_min <- temp[,3]
				
					if(any(create_treatments=="ClimateScenario_Temp_PerturbationInMeanSeasonalityBothOrNone") && !grepl("Both", i_sw_input_treatments$ClimateScenario_Temp_PerturbationInMeanSeasonalityBothOrNone, ignore.case=T)){
						if(grepl("Mean", i_sw_input_treatments$ClimateScenario_Temp_PerturbationInMeanSeasonalityBothOrNone, ignore.case=T)) {
							# -(mean monthly of scenario - mean monthly of current) + (mean annual of scenario - mean annual of current)
							t_min <- -(SiteClimate_Scenario$minMonthlyTempC - SiteClimate_Ambient$minMonthlyTempC) + (SiteClimate_Scenario$MAT_C - SiteClimate_Ambient$MAT_C)
							t_max <- -(SiteClimate_Scenario$maxMonthlyTempC - SiteClimate_Ambient$maxMonthlyTempC) + (SiteClimate_Scenario$MAT_C - SiteClimate_Ambient$MAT_C)
						}
						if(grepl("Seasonality", i_sw_input_treatments$ClimateScenario_Temp_PerturbationInMeanSeasonalityBothOrNone, ignore.case=T)) {
							# -(mean annual of scenario - mean annual of current)
							t_min <- rep(-(SiteClimate_Scenario$MAT_C - SiteClimate_Ambient$MAT_C),12)
							t_max <- rep(-(SiteClimate_Scenario$MAT_C - SiteClimate_Ambient$MAT_C),12)
						} 
						if(grepl("None", i_sw_input_treatments$ClimateScenario_Temp_PerturbationInMeanSeasonalityBothOrNone, ignore.case=T)) {
							# -(mean monthly of scenario - mean monthly of current)
							t_min <- -(SiteClimate_Scenario$minMonthlyTempC - SiteClimate_Ambient$minMonthlyTempC)
							t_max <- -(SiteClimate_Scenario$maxMonthlyTempC - SiteClimate_Ambient$maxMonthlyTempC)
						}
					}
					if(any(create_treatments=="ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone") && !grepl("Both", i_sw_input_treatments$ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone, ignore.case=T)){
						if(grepl("Mean", i_sw_input_treatments$ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone, ignore.case=T)) {
							#Mean of weather == mean of scenario, seasonality of weather = seasonality of ambient
							if(isTRUE(all.equal(SiteClimate_Ambient$MAP_cm, 0))){
								SiteClimate_Ambient$MAP_cm <- sqrt(.Machine$double.eps)
								if(isTRUE(all.equal(SiteClimate_Scenario$MAP_cm, 0))){
									SiteClimate_Scenario$MAP_cm <- sqrt(.Machine$double.eps)
									ppt_sc <- rep(0, times=12)
								} else {
									warning("Problem with scaling to 'mean' for ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone because of zero precipitation periods")
								}
							}
							if(sum(ppt_sc) > 0){
								if(sum(temp <- sapply(SiteClimate_Scenario$meanMonthlyPPTcm, FUN=function(x) isTRUE(all.equal(x, 0)))) > 0){
									warning("Problem with scaling to 'mean' for ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone because of zero precipitation periods")
									SiteClimate_Scenario$meanMonthlyPPTcm[temp] <- sqrt(.Machine$double.eps)
								}
								ppt_sc <- (SiteClimate_Ambient$meanMonthlyPPTcm / SiteClimate_Scenario$meanMonthlyPPTcm) * (SiteClimate_Scenario$MAP_cm / SiteClimate_Ambient$MAP_cm)
							}
						}
						if(grepl("Seasonality", i_sw_input_treatments$ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone, ignore.case=T)) {
							#Mean of weather == mean of ambient, seasonality of weather = seasonality of scenario
							if(isTRUE(all.equal(SiteClimate_Scenario$MAP_cm, 0))){
								SiteClimate_Scenario$MAP_cm <- sqrt(.Machine$double.eps)
								if(isTRUE(all.equal(SiteClimate_Ambient$MAP_cm, 0))){
									SiteClimate_Ambient$MAP_cm <- sqrt(.Machine$double.eps)
									ppt_sc <- rep(0, times=12)
								} else {
									warning("Problem with scaling to 'mean' for ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone because of zero precipitation periods")
								}
							}
							if(sum(ppt_sc) > 0){
								ppt_sc <- rep((SiteClimate_Ambient$MAP_cm / SiteClimate_Scenario$MAP_cm),12)
							}
						}
						if(grepl("None", i_sw_input_treatments$ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone, ignore.case=T)) {
							#Mean of weather == mean of ambient, seasonality of weather = seasonality of ambient
							if(isTRUE(all.equal(SiteClimate_Ambient$MAP_cm, 0)) && isTRUE(all.equal(SiteClimate_Scenario$MAP_cm, 0))){
								SiteClimate_Ambient$MAP_cm <- SiteClimate_Scenario$MAP_cm <- sqrt(.Machine$double.eps)
								ppt_sc <- rep(0, times=12)
							}
							if(sum(ppt_sc) > 0){
								if(sum(temp <- sapply(SiteClimate_Scenario$meanMonthlyPPTcm, FUN=function(x) isTRUE(all.equal(x, 0)))) > 0){
									warning("Problem with scaling to 'mean' for ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone because of zero precipitation periods")
									SiteClimate_Scenario$meanMonthlyPPTcm[temp] <- sqrt(.Machine$double.eps)
								}
								ppt_sc <- (SiteClimate_Ambient$meanMonthlyPPTcm / SiteClimate_Scenario$meanMonthlyPPTcm)
							}
						}
					}
					if(sum(temp <- sapply(SiteClimate_Ambient$meanMonthlyPPTcm, FUN=function(x) isTRUE(all.equal(x, 0)))) > 0){
						warning("Problem with scaling to 'mean' for ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone because of zero precipitation periods")
						SiteClimate_Ambient$meanMonthlyPPTcm[temp] <- sqrt(.Machine$double.eps)
					}
					
					swWeather_MonScalingParams(swRunScenariosData[[sc]])[,1] <- ppt_sc
					swWeather_MonScalingParams(swRunScenariosData[[sc]])[,2] <- t_max
					swWeather_MonScalingParams(swRunScenariosData[[sc]])[,3] <- t_min
					ClimatePerturbationsVals[sc,1:12] <- ppt_sc * SiteClimate_Scenario$meanMonthlyPPTcm / SiteClimate_Ambient$meanMonthlyPPTcm
					ClimatePerturbationsVals[sc,13:24] <- t_max + (SiteClimate_Scenario$maxMonthlyTempC - SiteClimate_Ambient$maxMonthlyTempC)
					ClimatePerturbationsVals[sc,25:36] <- t_min + (SiteClimate_Scenario$minMonthlyTempC - SiteClimate_Ambient$minMonthlyTempC)
				}
			}
			
			if(any(create_treatments=="LookupShiftedPPTScenarios")){
				ppt_f <- swWeather_MonScalingParams(swRunScenariosData[[sc]])[,1]
				ppt_f <- ppt_f * as.numeric(ppt_scShift)
				swWeather_MonScalingParams(swRunScenariosData[[sc]])[,1] <- ppt_f
				if(getScenarioWeatherDataFromDatabase){
					ClimatePerturbationsVals[sc,1:12] <- ppt_f * ClimatePerturbationsVals[sc,1:12]
				} else {
					ClimatePerturbationsVals[sc,1:12] <- ppt_f
				}
			}
				
			#anything that depends on weather
			#------3. Step: Lookup or extract external information that needs to be executed for each run
			if(print.debug) print("Start of set soil temperature")
			#TODO get this working LOW PR
			if(pcalcs$EstimateConstantSoilTemperatureAtUpperAndLowerBoundaryAsMeanAnnualAirTemperature){
				soilTlower <- mean(SiteClimate_Scenario$meanMonthlyTempC)
				soilTUpper <- max(-1, mean(SiteClimate_Scenario$meanMonthlyTempC[c(1,12)]))
				#temporaly save data 
				#out.temp <- data.frame(i, i_labels, soilTUpper, soilTlower)
				#write.csv(out.temp, file=paste(dir.out.temp, .Platform$file.sep, flag.icounter, "_", "SoilTempC_atLowerBoundary.csv", sep=""), quote=FALSE, row.names=FALSE)
			}
			if(sw_input_site_use$SoilTempC_atUpperBoundary) {
				soilTUpper <- ifelse(exists("soilTUpper"), soilTUpper, i_sw_input_site$SoilTempC_atUpperBoundary)
			}
			if(sw_input_site_use$SoilTempC_atLowerBoundary){
				soilTlower <- ifelse(exists("soilTlower"), soilTlower, i_sw_input_site$SoilTempC_atLowerBoundary)
				swSite_SoilTemperatureConsts(swRunScenariosData[[sc]])[8] <- soilTlower
			}
			if(pcalcs$EstimateInitialSoilTemperatureForEachSoilLayer){
				init.soilTprofile <- EstimateInitialSoilTemperatureForEachSoilLayer(layers_depth=layers_depth, lower.Tdepth=180, soilTupper=soilTUpper, soilTlower=soilTlower)	#lower.Tdepth needs to be adjusted if it changes in soilparam.in
				#temporaly save data #TODO get this working
				#out.temp <- data.frame(i, i_labels, t(c(init.soilTprofile, rep(NA, times=SoilLayer_MaxNo-length(init.soilTprofile)))))
				#write.csv(out.temp, file=paste(dir.out.temp, .Platform$file.sep, flag.icounter, "_", "SoilTempC_InitProfile.csv", sep=""), quote=FALSE, row.names=FALSE)
			}
			
			#adjust init soil temperatures to climatic conditions
			if(any(use_soil_temp <- as.logical(sw_input_soils_use[paste("SoilTemp_L", ld, sep="")]))){
				
				temp <- 1:nrow(swSoils_Layers(swRunScenariosData[[sc]]))
				if(exists("init.soilTprofile")) {
					swSoils_Layers(swRunScenariosData[[sc]])[,12][use_soil_temp] <- init.soilTprofile
				} else {
					swSoils_Layers(swRunScenariosData[[sc]])[,12][use_soil_temp] <- as.numeric(i_sw_input_soils[paste("SoilTemp_L", temp, sep="")])
				}
			}
			
			#- Calculate relative composition based on equations
			if(print.debug) print("Start of PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996")
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
				#Rsoilwat
				isNorth <-i_SWRunInformation$Y_WGS84 >= 0
				use_Annuals_Fraction <- any(create_treatments == "PotentialNaturalVegetation_CompositionAnnuals_Fraction")
				Annuals_Fraction <- i_sw_input_treatments$PotentialNaturalVegetation_CompositionAnnuals_Fraction
				use_C4_Fraction <- any(create_treatments == "PotentialNaturalVegetation_CompositionC4_Fraction")
				C4_Fraction <- i_sw_input_treatments$PotentialNaturalVegetation_CompositionC4_Fraction
				use_C3_Fraction <- any(create_treatments == "PotentialNaturalVegetation_CompositionC3_Fraction")
				C3_Fraction <- i_sw_input_treatments$PotentialNaturalVegetation_CompositionC3_Fraction
				use_Shrubs_Fraction <- any(create_treatments == "PotentialNaturalVegetation_CompositionShrubs_Fraction")
				Shrubs_Fraction <- i_sw_input_treatments$PotentialNaturalVegetation_CompositionShrubs_Fraction
				
				#save(SiteClimate_Ambient,SiteClimate_Scenario,MAP_mm,MAT_C,monthly.ppt,monthly.temp,dailyC4vars,isNorth,use_Annuals_Fraction, Annuals_Fraction,use_C4_Fraction, C4_Fraction,use_C3_Fraction, C3_Fraction,use_Shrubs_Fraction, Shrubs_Fraction,shrub.fraction.limit,file=file.path(dir.sw.runs, paste("Rsoilwat_composition_",i,"_",sc,sep="")))
				temp <- try(PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996(MAP_mm,MAT_C,monthly.ppt,monthly.temp,dailyC4vars,isNorth,shrub.fraction.limit,
						use_Annuals_Fraction, Annuals_Fraction,
						use_C4_Fraction, C4_Fraction,
						use_C3_Fraction, C3_Fraction,
						use_Shrubs_Fraction, Shrubs_Fraction), silent=TRUE)
				if(inherits(temp, "try-error")){
					tasks$create <- 0
				} else {
					grass.fraction <- temp$Composition[1]
					swProd_Composition(swRunScenariosData[[sc]]) <- temp$Composition
					grasses.c3c4ann.fractions[[sc]] <- temp$grasses.c3c4ann.fractions
				}
			}
			
			if(print.debug) print("Start of biomass adjustments")
			if(any(create_treatments == "PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996") && i_sw_input_treatments$PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996 && ((any(create_treatments == "AdjMonthlyBioMass_Temperature") && i_sw_input_treatments$AdjMonthlyBioMass_Temperature) | (any(create_treatments == "AdjMonthlyBioMass_Precipitation") &&  i_sw_input_treatments$AdjMonthlyBioMass_Precipitation) )){
				adjTemp <- any(create_treatments == "AdjMonthlyBioMass_Temperature") && i_sw_input_treatments$AdjMonthlyBioMass_Temperature
				adjPrep <- any(create_treatments == "AdjMonthlyBioMass_Precipitation") & i_sw_input_treatments$AdjMonthlyBioMass_Precipitation
				
				temp<-AdjMonthlyBioMass(tr_VegetationComposition=tr_VegetationComposition, AdjMonthlyBioMass_Temperature=adjTemp, AdjMonthlyBioMass_Precipitation=adjPrep, grasses.c3c4ann.fractions=grasses.c3c4ann.fractions[[sc]],growing.season.threshold.tempC=growing.season.threshold.tempC,isNorth=isNorth,MAP_mm=MAP_mm,monthly.temp=monthly.temp)
				swProd_MonProd_grass(swRunScenariosData[[sc]])[,1:3] <- temp$grass[,1:3]
				swProd_MonProd_shrub(swRunScenariosData[[sc]])[,1:3] <- temp$shrub[,1:3]
				rm(adjTemp,adjPrep)
			}
			
			#adjust Root Profile - need composition fractions set above
			if(print.debug) print("Start of AdjRootProfile")
			if(any(create_treatments == "AdjRootProfile") && i_sw_input_treatments$AdjRootProfile && any(create_treatments == "PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996") && i_sw_input_treatments$PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996) {
				
				trco_type_C3 <- ifelse(any(create_treatments == "RootProfile_C3") && any(colnames(tr_input_TranspCoeff) == i_sw_input_treatments$RootProfile_C3), i_sw_input_treatments$RootProfile_C3, "SchenkJackson2003_PCdry_grasses")
				trco_type_C4 <- ifelse(any(create_treatments == "RootProfile_C4") && any(colnames(tr_input_TranspCoeff) == i_sw_input_treatments$RootProfile_C4), i_sw_input_treatments$RootProfile_C4, "SchenkJackson2003_PCdry_grasses")
				trco_type_annuals <- ifelse(any(create_treatments == "RootProfile_Annuals") && any(colnames(tr_input_TranspCoeff) == i_sw_input_treatments$RootProfile_Annuals), i_sw_input_treatments$RootProfile_Annuals, "Jacksonetal1996_crops")
				trco_type_shrubs <- ifelse(any(create_treatments == "RootProfile_Shrubs") && any(colnames(tr_input_TranspCoeff) == i_sw_input_treatments$RootProfile_Shrubs), i_sw_input_treatments$RootProfile_Shrubs, "SchenkJackson2003_PCdry_shrubs")
				tro_type_tree <- ifelse(any(create_treatments == "LookupTranspCoeffFromTable_Tree") && is.finite(i_sw_input_treatments$LookupTranspCoeffFromTable_Tree) && any(colnames(tr_input_TranspCoeff) == i_sw_input_treatments$LookupTranspCoeffFromTable_Tree), i_sw_input_treatments$LookupTranspCoeffFromTable_Tree, "FILL")
				
				if(grass.fraction==0) { #if grass.fraction is 0 then Grass.trco will be 0
					Grass.trco <- TranspCoeffByVegType(soillayer_no=d, trco_type="FILL", layers_depth=layers_depth, adjustType="positive")
				} else {
					C3.trco <- TranspCoeffByVegType(soillayer_no=d, trco_type=trco_type_C3, layers_depth=layers_depth, adjustType="positive")
					C4.trco <- TranspCoeffByVegType(soillayer_no=d, trco_type=trco_type_C4, layers_depth=layers_depth, adjustType="positive")
					Annuals.trco <- TranspCoeffByVegType(soillayer_no=d, trco_type=trco_type_annuals, layers_depth=layers_depth, adjustType="positive")
					Grass.trco <- C3.trco * grasses.c3c4ann.fractions[[sc]][1] + C4.trco * grasses.c3c4ann.fractions[[sc]][2] + Annuals.trco * grasses.c3c4ann.fractions[[sc]][3]
				}
				if(is.na(sum(Grass.trco))) Grass.trco <- rep(0, d)
				
				Shrub.trco <- TranspCoeffByVegType(soillayer_no=d, trco_type=trco_type_shrubs, layers_depth=layers_depth, adjustType="inverse")
				Tree.trco <- TranspCoeffByVegType(soillayer_no=d, trco_type=tro_type_tree, layers_depth=layers_depth, adjustType="inverse")
				swSoils_Layers(swRunScenariosData[[sc]])[,6] <- Grass.trco
				swSoils_Layers(swRunScenariosData[[sc]])[,7] <- Shrub.trco
				swSoils_Layers(swRunScenariosData[[sc]])[,8] <- Tree.trco
				
				TRCO_done <- TRUE			
			}
			
			if(print.debug) print("Start of vegetation scaling")
			Grass_Scaling_use <- c("Grass_TotalBiomass_ScalingFactor", "Grass_LiveBiomass_ScalingFactor", "Grass_Litter_ScalingFactor")
			Shrub_Scaling_use <- c("Shrub_TotalBiomass_ScalingFactor", "Shrub_LiveBiomass_ScalingFactor", "Shrub_Litter_ScalingFactor")
			Tree_Scaling_use <- c("Tree_TotalBiomass_ScalingFactor", "Tree_LiveBiomass_ScalingFactor", "Tree_Litter_ScalingFactor")
			if(any(create_treatments %in% c(Grass_Scaling_use, Shrub_Scaling_use, Tree_Scaling_use))){
				finite01 <- function(x) {x[x < 0 | is.na(x)] <- 0; x[x > 1] <- 1; return(x)}
			
				grass_LitterTotalLiveScalingFactors <- rep(1, 3)
				if(any(create_treatments == "Grass_Litter_ScalingFactor") && is.finite(i_sw_input_treatments$Grass_Litter_ScalingFactor))
					grass_LitterTotalLiveScalingFactors[1] <- i_sw_input_treatments$Grass_Litter_ScalingFactor
				if(any(create_treatments == "Grass_TotalBiomass_ScalingFactor") && is.finite(i_sw_input_treatments$Grass_TotalBiomass_ScalingFactor))
					grass_LitterTotalLiveScalingFactors[2] <- i_sw_input_treatments$Grass_TotalBiomass_ScalingFactor
				if(any(create_treatments == "Grass_LiveBiomass_ScalingFactor") && is.finite(i_sw_input_treatments$Grass_LiveBiomass_ScalingFactor))
					grass_LitterTotalLiveScalingFactors[3] <- i_sw_input_treatments$Grass_LiveBiomass_ScalingFactor
				
				shrub_LitterTotalLiveScalingFactors <- rep(1, 3)
				if(any(create_treatments == "Shrub_Litter_ScalingFactor") && is.finite(i_sw_input_treatments$Shrub_Litter_ScalingFactor))
					shrub_LitterTotalLiveScalingFactors[1] <- i_sw_input_treatments$Shrub_Litter_ScalingFactor
				if(any(create_treatments == "Shrub_TotalBiomass_ScalingFactor") && is.finite(i_sw_input_treatments$Shrub_TotalBiomass_ScalingFactor))
					shrub_LitterTotalLiveScalingFactors[2] <- i_sw_input_treatments$Shrub_TotalBiomass_ScalingFactor
				if(any(create_treatments == "Shrub_LiveBiomass_ScalingFactor") && is.finite(i_sw_input_treatments$Shrub_LiveBiomass_ScalingFactor))
					shrub_LitterTotalLiveScalingFactors[3] <- i_sw_input_treatments$Shrub_LiveBiomass_ScalingFactor
				
				tree_LitterTotalLiveScalingFactors <- rep(1, 3)
				if(any(create_treatments == "Tree_Litter_ScalingFactor") && is.finite(i_sw_input_treatments$Tree_Litter_ScalingFactor))
					tree_LitterTotalLiveScalingFactors[1] <- i_sw_input_treatments$Tree_Litter_ScalingFactor
				if(any(create_treatments == "Tree_TotalBiomass_ScalingFactor") && is.finite(i_sw_input_treatments$Tree_TotalBiomass_ScalingFactor))
					tree_LitterTotalLiveScalingFactors[2] <- i_sw_input_treatments$Tree_TotalBiomass_ScalingFactor
				if(any(create_treatments == "Tree_LiveBiomass_ScalingFactor") && is.finite(i_sw_input_treatments$Tree_LiveBiomass_ScalingFactor))
					tree_LitterTotalLiveScalingFactors[3] <- i_sw_input_treatments$Tree_LiveBiomass_ScalingFactor
				
				ScalingSeason <- i_sw_input_treatments$Vegetation_Biomass_ScalingSeason_AllGrowingORNongrowing
				if(is.na(ScalingSeason) || !any(c("All", "Growing", "Nongrowing") == ScalingSeason)) #set to All for default
					ScalingSeason <- "All"
				
				if(any(create_treatments == "Vegetation_Biomass_ScalingSeason_AllGrowingORNongrowing") && !is.na(ScalingSeason) && !(any(create_treatments == "Vegetation_Biomass_ScalingSeason_AllGrowingORNongrowing") && ScalingSeason == "All")) {
					if(ScalingSeason == "Growing") { #Growing: apply 'Vegetation_Biomass_ScalingFactor' only to those months that have MAT > growing.season.threshold.tempC
						if((templength<-length((temp<-SiteClimate_Scenario$meanMonthlyTempC>growing.season.threshold.tempC)[temp==TRUE]))>1) {
							swProd_MonProd_grass(swRunScenariosData[[sc]])[temp, 1:3] <- sweep(swProd_MonProd_grass(swRunScenariosData[[sc]])[temp, 1:3], MARGIN=2, FUN="*", grass_LitterTotalLiveScalingFactors)
							swProd_MonProd_shrub(swRunScenariosData[[sc]])[temp, 1:3] <- sweep(swProd_MonProd_shrub(swRunScenariosData[[sc]])[temp, 1:3], MARGIN=2, FUN="*", shrub_LitterTotalLiveScalingFactors)
							swProd_MonProd_tree(swRunScenariosData[[sc]])[temp, 1:3] <- sweep(swProd_MonProd_tree(swRunScenariosData[[sc]])[temp, 1:3], MARGIN=2, FUN="*", tree_LitterTotalLiveScalingFactors)
						} else if(templength==1) {
							swProd_MonProd_grass(swRunScenariosData[[sc]])[temp,1:3]<-swProd_MonProd_grass(swRunScenariosData[[sc]])[temp,1:3]*grass_LitterTotalLiveScalingFactors
							swProd_MonProd_shrub(swRunScenariosData[[sc]])[temp,1:3]<-swProd_MonProd_shrub(swRunScenariosData[[sc]])[temp,1:3]*shrub_LitterTotalLiveScalingFactors
							swProd_MonProd_tree(swRunScenariosData[[sc]])[temp,1:3]<-swProd_MonProd_tree(swRunScenariosData[[sc]])[temp,1:3]*tree_LitterTotalLiveScalingFactors
						} else {
							print("To Cold to do Vegetation Scaling Season for Growing")
						}
					} else if(ScalingSeason == "Nongrowing") {# Nongrowing: apply 'Vegetation_Biomass_ScalingFactor' only to those months that have MAT <= growing.season.threshold.tempC
						if((templength<-length((temp<-SiteClimate_Scenario$meanMonthlyTempC<=growing.season.threshold.tempC)[temp==TRUE]))>1) {
							swProd_MonProd_grass(swRunScenariosData[[sc]])[temp, 1:3] <- sweep(swProd_MonProd_grass(swRunScenariosData[[sc]])[temp, 1:3], MARGIN=2, FUN="*", grass_LitterTotalLiveScalingFactors)
							swProd_MonProd_shrub(swRunScenariosData[[sc]])[temp, 1:3] <- sweep(swProd_MonProd_shrub(swRunScenariosData[[sc]])[temp, 1:3], MARGIN=2, FUN="*", shrub_LitterTotalLiveScalingFactors)
							swProd_MonProd_tree(swRunScenariosData[[sc]])[temp, 1:3] <- sweep(swProd_MonProd_tree(swRunScenariosData[[sc]])[temp, 1:3], MARGIN=2, FUN="*", tree_LitterTotalLiveScalingFactors)
						} else if (templength==1) {
							swProd_MonProd_grass(swRunScenariosData[[sc]])[temp,1:3]<-swProd_MonProd_grass(swRunScenariosData[[sc]])[temp,1:3]*grass_LitterTotalLiveScalingFactors
							swProd_MonProd_shrub(swRunScenariosData[[sc]])[temp,1:3]<-swProd_MonProd_shrub(swRunScenariosData[[sc]])[temp,1:3]*shrub_LitterTotalLiveScalingFactors
							swProd_MonProd_tree(swRunScenariosData[[sc]])[temp,1:3]<-swProd_MonProd_tree(swRunScenariosData[[sc]])[temp,1:3]*tree_LitterTotalLiveScalingFactors
						} else {
							print("To Hot to do Vegetation Scaling Season for NonGrowing")
						}
					}
				} else {
					swProd_MonProd_grass(swRunScenariosData[[sc]])[, 1:3] <- sweep(swProd_MonProd_grass(swRunScenariosData[[sc]])[, 1:3], MARGIN=2, FUN="*", grass_LitterTotalLiveScalingFactors)
					swProd_MonProd_shrub(swRunScenariosData[[sc]])[, 1:3] <- sweep(swProd_MonProd_shrub(swRunScenariosData[[sc]])[, 1:3], MARGIN=2, FUN="*", shrub_LitterTotalLiveScalingFactors)
					swProd_MonProd_tree(swRunScenariosData[[sc]])[, 1:3] <- sweep(swProd_MonProd_tree(swRunScenariosData[[sc]])[, 1:3], MARGIN=2, FUN="*", tree_LitterTotalLiveScalingFactors)
				}
				swProd_MonProd_grass(swRunScenariosData[[sc]])[, 3] <- finite01(swProd_MonProd_grass(swRunScenariosData[[sc]])[, 3])  #Check that live biomass fraction <= 1 & >= 0
				swProd_MonProd_shrub(swRunScenariosData[[sc]])[, 3] <- finite01(swProd_MonProd_shrub(swRunScenariosData[[sc]])[, 3])  #Check that live biomass fraction <= 1 & >= 0
				swProd_MonProd_tree(swRunScenariosData[[sc]])[, 3] <- finite01(swProd_MonProd_tree(swRunScenariosData[[sc]])[, 3])  #Check that live biomass fraction <= 1 & >= 0
			}			
			
			if(any(create_treatments == "Vegetation_Height_ScalingFactor")) {
				#scale constant height
				swProd_CanopyHeight(swRunScenariosData[[sc]])[5, ] <- pmax(0, swProd_CanopyHeight(swRunScenariosData[[sc]])[5, ] * i_sw_input_treatments$Vegetation_Height_ScalingFactor)
				#scale tanfunc parameters: scale yinflec and range, leave xinflec and slope as is
				swProd_CanopyHeight(swRunScenariosData[[sc]])[2:3, ] <- pmax(0, swProd_CanopyHeight(swRunScenariosData[[sc]])[2:3, ] * i_sw_input_treatments$Vegetation_Height_ScalingFactor)			
			}
			
			#if southern hemisphere adjust if set, but not when already adjusted by, e.g., growing season
			if(print.debug) print("Start of hemisphere adjustment")
			if(accountNSHemispheres_veg && i_SWRunInformation$Y_WGS84 < 0 && !any(create_treatments == "AdjMonthlyBioMass_Temperature")){
				swProd_MonProd_grass(swRunScenariosData[[sc]])[, 3] <- rbind(swProd_MonProd_grass(swRunScenariosData[[sc]])[7:12,], swProd_MonProd_grass(swRunScenariosData[[sc]])[1:6,])
				swProd_MonProd_shrub(swRunScenariosData[[sc]])[, 3] <- rbind(swProd_MonProd_shrub(swRunScenariosData[[sc]])[7:12,], swProd_MonProd_shrub(swRunScenariosData[[sc]])[1:6,])
				swProd_MonProd_tree(swRunScenariosData[[sc]])[, 3] <- rbind(swProd_MonProd_tree(swRunScenariosData[[sc]])[7:12,], swProd_MonProd_tree(swRunScenariosData[[sc]])[1:6,])
			}

			#--control transpiration regions for adjusted soil depth and rooting depth
			if(print.debug) print("Start of control transpiration regions")
			
			tri.file <- matrix(NA, nrow=4, ncol=2, dimnames=list(NULL, c("Used_TF", "DeepestLayer")))
			for(tri in 1:4){
				if(tri <= nrow(swSite_TranspirationRegions(swRunScenariosData[[sc]]))) {
					tri.file[tri, 2] <- swSite_TranspirationRegions(swRunScenariosData[[sc]])[tri,2]
					tri.file[tri, 1] <- 1
				} else {
					tri.file[tri, 2] <- NA#swSite_TranspirationRegions(swRunScenariosData[[sc]])[tri-1,2]+1
					tri.file[tri, 1] <- 0
				}
			}
			
			#get soil depth
			max.tri.soil <- length(layers_depth)
			
			#get rooting depth
			if(nrow(swSoils_Layers(swRunScenariosData[[sc]])) > 1){
				max.tri.root <- min(apply(swSoils_Layers(swRunScenariosData[[sc]])[, c(6,7,8)], MARGIN=2, FUN=function(x) sum(x > 0)))
			} else {
				max.tri.root <- 1
			}
			#adjust maximum transpiration region for minimum soil depth and rooting depth
			if(max(tri.file[tri.file[, 1] > 0, 2], na.rm=TRUE) > (max.tri <- min(max.tri.soil, max.tri.root))){ 
				for(tri in 4:1) if(tri.file[tri, 1] > 0){
						if(tri.file[tri, 2] > max.tri)
							tri.file[tri, 2] <- swSite_TranspirationRegions(swRunScenariosData[[sc]])[tri,2] <- max.tri
						if(tri > 1 && tri.file[tri, 2] <= tri.file[tri-1, 2])
							swSite_TranspirationRegions(swRunScenariosData[[sc]]) <- matrix(swSite_TranspirationRegions(swRunScenariosData[[sc]])[-tri,], ncol=2)
					}
			}
			#check transpiration regions once more and set TRRG_done
			temp <- swSite_TranspirationRegions(swRunScenariosData[[sc]])
			if(nrow(temp) > 0 && temp[1, 2] >= 1 ||
				max(temp[, 2]) <= max.tri.root ) TRRG_done <- TRUE
		}#end do scenario creations
		
		if(!EVCO_done){
			print("Evaporation coefficients not set for this run.")
		} else if(!TRCO_done){
			print("Transpiration coefficients not set for this run.")
		} else if(!TRRG_done){
			print("Transpiration regions not set for this run.")
		}
			
		if(tasks$create <= 0 || !EVCO_done || !TRCO_done || !TRRG_done){
			tasks$create <- 0
			tasks$execute <- tasks$aggregate <- -1
		} else {
			tasks$create <- 2
		}
			
		if(saveSoilWatInputOutput) save(swRunScenariosData, i_sw_weatherList, file=file.path(dir.sw.runs.sim, "sw_input.RData"))
	}#end if do create runs
	
	if(makeInputForExperimentalDesign && trowExperimentals > 0 && length(create_experimentals) > 0) {
		#This file will be used to remake the input files for experimentals
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
	
	
#------------------------EXECUTE SOILWAT
	if( tasks$execute == 1 ){
		runData <- list()
		
		if(is.na(i_sw_input_treatments$Exclude_ClimateAmbient)) i_sw_input_treatments$Exclude_ClimateAmbient <- FALSE
		if(any(create_treatments == "Exclude_ClimateAmbient") && i_sw_input_treatments$Exclude_ClimateAmbient && i!=1) {
			Exclude_ClimateAmbient <- 2
		} else {
			Exclude_ClimateAmbient <- 1
		}
		
		for (sc in Exclude_ClimateAmbient:scenario_No){
			if(print.debug) print(paste("Start of SoilWat execution for scenario:", sc))
#			runData[[sc]]<-tryCatch({ sw_exec(data=swRunScenariosData[[sc]],weatherList=i_sw_weatherList, echo=F, quiet=F,colNames=saveSoilWatInputOutput)
#					}, warning = function(w) {
#						print("------------Warning----------")
#						print(w)
#						print("-----------------------------")
#						#assign("todo$aggregate", FALSE, pos=2)
#						#mpi.send.Robj(i,0,4)
#					}, error = function(e) {
#						print("-------------Error-----------")
#						print(e)
#						print("-----------------------------")
#						if(parallel_runs && identical(parallel_backend,"mpi"))
#							mpi.send.Robj(i,0,4)
#						return(NA)
#					})
#			if(isTRUE(is.na(runData[[sc]]))){
#				todo$execute <- todo$aggregate <- FALSE
#				break
#			}
			runData[[sc]] <- try(sw_exec(data=swRunScenariosData[[sc]],weatherList=i_sw_weatherList[[ifelse(getScenarioWeatherDataFromDatabase, sc, 1)]], echo=F, quiet=F,colNames=saveSoilWatInputOutput), silent=TRUE)
			if(inherits(runData[[sc]], "try-error")){
				tasks$execute <- 0
				break
			}
		}
		if(saveSoilWatInputOutput) save(runData, file=file.path(dir.sw.runs.sim, "sw_output.RData"))
		if(tasks$execute > 0){
			tasks$execute <- 2
		} else {
			tasks$aggregate <- -1
		}
	}#end if do execute
	
#------------------------AGGREGATE SOILWAT OUTPUT
	
	if( tasks$aggregate == 1 ){
		if(print.debug) print("Preparations for aggregation")
    
		#get soil aggregation layer for daily aggregations
		if(AggLayer.daily){
			aggLs <- setAggSoilLayerForAggDailyResponses(layers_depth)
		} else {
			aggLs <- as.list(ld)
		}
		aggLs_no <- length(aggLs)
		
		#get soil texture data for each layer
		stemp <- swSoils_Layers(swRunScenariosData[[1]])
		soilDepth_cm <- max(stemp[, 1])
		soilLayers_N <- length(stemp[, 1])
    
		sand <- stemp[,9]
		clay <- stemp[,10]
		
		texture <- list(sand.top=weighted.mean(sand[topL], layers_width[topL]),
				sand.bottom=weighted.mean(sand[bottomL], layers_width[bottomL]),
				clay.top=weighted.mean(clay[topL], layers_width[topL]),
				clay.bottom=weighted.mean(clay[bottomL], layers_width[bottomL]))
		
		if(any(simulation_timescales=="daily") && daily_no > 0){
			textureDAgg <- list(sand=sapply(1:aggLs_no, FUN=function(x) weighted.mean(sand[aggLs[[x]]], layers_width[aggLs[[x]]])),
					clay=sapply(1:aggLs_no, FUN=function(x) weighted.mean(clay[aggLs[[x]]], layers_width[aggLs[[x]]])))
		}
		
		#data access functions
		get_Response_aggL <- function(sc_i, response, tscale=c("dy", "dyAll", "mo", "yr"), scaler=10, FUN, weights=NULL){
			FUN <- match.fun(FUN)
			tscale <- match.arg(tscale, choices=c("dy", "dyAll", "mo", "yr"))
			if(response %in% c(sw_transp, sw_hd)){#divide by 4, because: each soil layer (cm): total, trees, shrubs, grasses
				responseRepeats <- 4
			} else { #c(sw_vwc, sw_evsoil, sw_soiltemp, sw_swc, sw_swa)
				responseRepeats <- 1
			}

			if((tscale == "dy") || (tscale == "dyAll"))
				temp1 <- scaler * runData[[sc_i]][[response]][[4]]
			if(tscale == "mo")
				temp1 <- scaler * runData[[sc_i]][[response]][[2]]
			if(tscale == "yr")
				temp1 <- scaler * runData[[sc_i]][[response]][[1]]
			
			if(inherits(temp1, "try-error")) stop("Necessary SoilWat output files are not present for aggregation of results")
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
			layers <- 1:((ncol(temp1) - index.col) / responseRepeats)
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
			} else if(is.null(bottomL) || identical(bottomL, 0)) {
				val.bottom <- matrix(data=0, nrow=length(index.usetimestep), ncol=1)
			} else {
				val.bottom  <- temp1[index.usetimestep, index.col + bottomL]
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
		get_Temp_yr <- function(sc){
			return(list(mean=runData[[sc]][[sw_temp]][[sw_yr]][simTime$index.useyr, 4]))
		}
		get_Temp_mo <- function(sc){
			return(list(min=runData[[sc]][[sw_temp]][[sw_mo]][simTime$index.usemo, 4], mean=runData[[sc]][[sw_temp]][[sw_mo]][simTime$index.usemo, 5]))
		}
		get_Temp_dy <- function(sc){
			return(list(min=runData[[sc]][[sw_temp]][[sw_dy]][simTime$index.usedy, 4],
						mean=runData[[sc]][[sw_temp]][[sw_dy]][simTime$index.usedy, 5],
						max=runData[[sc]][[sw_temp]][[sw_dy]][simTime$index.usedy, 3]))
		}
		
		get_PPT_yr <- function(sc){
			ppt <- 10 * runData[[sc]][[sw_precip]][[sw_yr]][simTime$index.useyr, 2]
			rain <- 10 * runData[[sc]][[sw_precip]][[sw_yr]][simTime$index.useyr, 3]
			snowfall <- 10 * runData[[sc]][[sw_precip]][[sw_yr]][simTime$index.useyr, 4]
			snowmelt <- 10 * runData[[sc]][[sw_precip]][[sw_yr]][simTime$index.useyr, 5]
			snowloss <- 10 * runData[[sc]][[sw_precip]][[sw_yr]][simTime$index.useyr, 6]
			return(list(ppt=ppt, rain=rain, snowfall=snowfall, snowmelt=snowmelt, snowloss=snowloss))
		}
		get_PPT_mo <- function(sc){
			return(list(ppt=10 * runData[[sc]][[sw_precip]][[sw_mo]][simTime$index.usemo, 3], rain=10 * runData[[sc]][[sw_precip]][[sw_mo]][simTime$index.usemo, 4], snowmelt=10 * runData[[sc]][[sw_precip]][[sw_mo]][simTime$index.usemo, 6]))
		}
		get_PPT_dy <- function(sc){
			return(list(ppt=10 * runData[[sc]][[sw_precip]][[sw_dy]][simTime$index.usedy, 3], rain=10 * runData[[sc]][[sw_precip]][[sw_dy]][simTime$index.usedy, 4]))
		}
		
		get_PET_yr <- function(sc){
			return(list(val=10 * runData[[sc]][[sw_pet]][[sw_yr]][simTime$index.useyr, 2]))
		}
		get_PET_mo <- function(sc){
			return(list(val=10 * runData[[sc]][[sw_pet]][[sw_mo]][simTime$index.usemo, 3]))
		}
		
		get_AET_yr <- function(sc){
			return(list(val=10 * runData[[sc]][[sw_aet]][[sw_yr]][simTime$index.useyr, 2]))
		}
		get_AET_mo <- function(sc){
			return(list(val=10 * runData[[sc]][[sw_aet]][[sw_mo]][simTime$index.usemo, 3]))
		}
		get_AET_dy <- function(sc){
			return(list(val=10 * runData[[sc]][[sw_aet]][[sw_dy]][simTime$index.usedy, 3]))
		}
		
		get_SWE_mo <- function(sc){
			return(list(val=10 * runData[[sc]][[sw_snow]][[sw_mo]][simTime$index.usemo, 3]))
		}
		get_SWE_dy <- function(sc){
			return(list(val=10 * runData[[sc]][[sw_snow]][[sw_dy]][simTime$index.usedy, 3]))
		}
		
		get_Inf_yr <- function(sc){
			return(list(inf=10 * runData[[sc]][[sw_inf_soil]][[sw_yr]][simTime$index.useyr, 2]))
		}
		get_Inf_mo <- function(sc){
			return(list(inf=10 * runData[[sc]][[sw_inf_soil]][[sw_mo]][simTime$index.usemo, 3]))
		}
		get_Inf_dy <- function(sc){
			return(list(inf=10 * runData[[sc]][[sw_inf_soil]][[sw_dy]][simTime$index.usedy, 3]))
		}
		
		get_Esurface_yr <- function(sc){
			return(list(sum=10 * runData[[sc]][[sw_evapsurface]][[sw_yr]][simTime$index.useyr, 2], veg=apply(10 * runData[[sc]][[sw_evapsurface]][[sw_yr]][simTime$index.useyr, 3:5], 1, sum), litter=10 * runData[[sc]][[sw_evapsurface]][[sw_yr]][simTime$index.useyr, 6], surfacewater=10 * runData[[sc]][[sw_evapsurface]][[sw_yr]][simTime$index.useyr, 7]))
		}
		get_Esurface_dy <- function(sc){
			return(list(sum=10 * runData[[sc]][[sw_evapsurface]][[sw_dy]][simTime$index.usedy, 3], veg=apply(10 * runData[[sc]][[sw_evapsurface]][[sw_dy]][simTime$index.usedy, 4:6], 1, sum), litter=10 * runData[[sc]][[sw_evapsurface]][[sw_dy]][simTime$index.usedy, 7], surfacewater=10 * runData[[sc]][[sw_evapsurface]][[sw_dy]][simTime$index.usedy, 8]))
		}
		
		get_Interception_yr <- function(sc){
			return(list(sum=10 * runData[[sc]][[sw_interception]][[sw_yr]][simTime$index.useyr, 2], veg=apply(10 * runData[[sc]][[sw_interception]][[sw_yr]][simTime$index.useyr, 3:5], 1, sum), litter=10 * runData[[sc]][[sw_interception]][[sw_yr]][simTime$index.useyr, 6]))
		}
		
		get_DeepDrain_yr <- function(sc){
			return(list(val=10 * runData[[sc]][[sw_deepdrain]][[sw_yr]][simTime$index.useyr, 2]))
		}
		get_DeepDrain_mo <- function(sc){
			return(list(val=10 * runData[[sc]][[sw_deepdrain]][[sw_mo]][simTime$index.usemo, 3]))
		}
		get_DeepDrain_dy <- function(sc){
			return(list(val=10 * runData[[sc]][[sw_deepdrain]][[sw_dy]][simTime$index.usedy, 3]))
		}
		
		get_Runoff_mo <- function(sc){
			return(list(val=10 * runData[[sc]][[sw_runoff]][[sw_mo]][simTime$index.usemo, 3], ponded=10 * runData[[sc]][[sw_runoff]][[sw_mo]][simTime$index.usemo, 4], snowmelt=10 * runData[[sc]][[sw_runoff]][[sw_mo]][simTime$index.usemo, 5]))
		}
		get_Runoff_yr <- function(sc){
			return(list(val=10 * runData[[sc]][[sw_runoff]][[sw_yr]][simTime$index.useyr, 2], ponded=10 * runData[[sc]][[sw_runoff]][[sw_yr]][simTime$index.useyr, 3], snowmelt=10 * runData[[sc]][[sw_runoff]][[sw_yr]][simTime$index.useyr, 4]))
		}
		
		SQL <- character(0)
		if(parallel_runs && parallel_backend == "mpi") {
			dbTempFileName <- paste("SQL_Node_",mpi.comm.rank(),".sql",sep="")
			dbTempFileNameCurrent <- paste("SQL_Current_Node_",mpi.comm.rank(),".sql",sep="")
		} else if(parallel_runs && parallel_backend == "snow") {
			dbTempFileName <- paste("SQL_Node_",nodeNumber,".sql",sep="")
			dbTempFileNameCurrent <- paste("SQL_Current_Node_",nodeNumber,".sql",sep="")
		} else if (parallel_runs && parallel_backend == "multicore") {
			#TODO Get proper node number.
			dbTempFileName <- paste("SQL_Node_",sample(1:500,1),".sql",sep="")
			dbTempFileNameCurrent <- paste("SQL_Current_Node_",sample(1:500,1),".sql",sep="")
		} else {
			dbTempFileName <- "SQL.sql"
			dbTempFileNameCurrent <- "SQL_Current.sql"
		}
		dbTempFile <- file.path(dir.out, "temp", dbTempFileName)
		dbTempFileCurrent <- file.path(dir.out, "temp", dbTempFileNameCurrent)
		
		#Performance Measuring
		#if(aggregate.timing) OutputTiming <- list()
		#if(aggregate.timing) GeneralOutputTiming <- matrix(NA,nrow=scenario_No,ncol=2)
		#aggregate for each scenario
		for (sc in 1:scenario_No){
			if(print.debug) print(paste("Start of overall aggregation for scenario:", sc))
			#HEADER GENERATION REMOVED#	
			#only exclude if 1.) Exclude_ClimateAmbient is true in treatments 2.) That Run is set to Exclude_ClimateAmbient 3.) Our current Scenario is Current
			if(any(create_treatments == "Exclude_ClimateAmbient") && i_sw_input_treatments$Exclude_ClimateAmbient && sc==1 && i!=1) {
				Exclude_ClimateAmbient <- TRUE
				
				#dbOverallColumns comes from database creation
				P_id <- ((i-1)*scenario_No+sc)
				SQL1 <- paste0("INSERT INTO \"aggregation_overall_mean\" VALUES (",paste0(P_id,",",paste0(temp <- rep("NULL", times=dbOverallColumns),collapse=","),sep=""),");", sep="")
				SQL2 <- paste0("INSERT INTO \"aggregation_overall_sd\" VALUES (",paste0(P_id,",",paste0(temp,collapse=","),sep=""),");", sep="")
				if(length(SQL) == 0) {
					SQL <- paste(SQL1, SQL2, sep="\n")
				} else {
					SQL <- paste(SQL, SQL1, SQL2, sep="\n")
				}
				
			} else {
				Exclude_ClimateAmbient <- FALSE
			}
			
			#overall aggregation. If Exclude_ClimateAmbient == TRUE then skip
			if(!continueAfterAbort | (continueAfterAbort & !isdone.overallAggs[sc]) && !Exclude_ClimateAmbient){
				
				#delete data so that they are read if anew for each scenario; each variable is checked that datafile is read in only once per scenario			
				try(rm(			temp.yr, temp.mo, temp.dy, 
								prcp.yr, prcp.mo, prcp.dy, 
								PET.yr, PET.mo, PET.dy,
								AET.yr, AET.mo, AET.dy,
								SWE.yr, SWE.mo, SWE.dy,
								soiltemp.yr, soiltemp.mo, soiltemp.dy,
								swc.yr, swc.mo, swc.dy,
								swa.yr, swa.mo, swa.dy,
								vwc.yr, vwc.mo, vwc.dy, vwc.dy.all, 
								swp.yr, swp.mo, swp.dy, swp.dy.all, 
								transp.yr, transp.mo, transp.dy,
								Esoil.yr, Esoil.mo, Esoil.dy,
								Esurface.yr, Esurface.mo, Esurface.dy,
								hydred.yr, hydred.mo, hydred.dy,
								inf.yr, inf.mo, inf.dy, 
								runoff.yr, runoff.mo, runoff.dy,
								intercept.yr, intercept.mo, intercept.dy,
								deepDrain.yr, deepDrain.mo, deepDrain.dy
						), silent=TRUE)
				
				#result vector column index indicating variable within set of n_variables per scenario
				resMeans <- resSDs <- rep(NA, length=dbOverallColumns)
#check that db still works:
#					res <- vector(mode="numeric", length=n_variables)
#					res <- data.frame(t(res), stringsAsFactors = FALSE)
				nv <- 1
				
				
				#---Aggregation: SoilWat inputs
			#0
				if(aon$input_SoilProfile){
					if(print.debug) print("Aggregation of input_SoilProfile")
					resMeans[nv:(nv+5)] <- c(soilDepth_cm, soilLayers_N, unlist(texture))
					nv <- nv+6
				}
			#1
				if(aon$input_FractionVegetationComposition) {
					if(print.debug) print("Aggregation of input_FractionVegetationComposition")
					resMeans[nv:(nv+5)] <- c(swProd_Composition(swRunScenariosData[[sc]]), grasses.c3c4ann.fractions[[sc]])
					nv <- nv+6
				}
				if(aon$input_VegetationBiomassMonthly) {
					if(print.debug) print("Aggregation of input_VegetationBiomassMonthly")
					resMeans[nv:(nv+11)] <- swProd_MonProd_grass(swRunScenariosData[[sc]])[,1]
					nv <- nv+12
					resMeans[nv:(nv+11)] <- swProd_MonProd_grass(swRunScenariosData[[sc]])[,2]
					nv <- nv+12
					resMeans[nv:(nv+11)] <- swProd_MonProd_grass(swRunScenariosData[[sc]])[,2]*swProd_MonProd_grass(swRunScenariosData[[sc]])[,3]
					nv <- nv+12
					resMeans[nv:(nv+11)] <- swProd_MonProd_shrub(swRunScenariosData[[sc]])[,1]
					nv <- nv+12
					resMeans[nv:(nv+11)] <- swProd_MonProd_shrub(swRunScenariosData[[sc]])[,2]
					nv <- nv+12
					resMeans[nv:(nv+11)] <- swProd_MonProd_shrub(swRunScenariosData[[sc]])[,2]*swProd_MonProd_shrub(swRunScenariosData[[sc]])[,3]
					nv <- nv+12
					resMeans[nv:(nv+11)] <- swProd_MonProd_tree(swRunScenariosData[[sc]])[,1]
					nv <- nv+12
					resMeans[nv:(nv+11)] <- swProd_MonProd_tree(swRunScenariosData[[sc]])[,2]
					nv <- nv+12
					resMeans[nv:(nv+11)] <- swProd_MonProd_tree(swRunScenariosData[[sc]])[,2]*swProd_MonProd_tree(swRunScenariosData[[sc]])[,3]
					nv <- nv+12
				}
			#3
				if(aon$input_VegetationPeak) {
					if(print.debug) print("Aggregation of input_VegetationPeak")
					fracs <- swProd_Composition(swRunScenariosData[[sc]]) #get the fractional Composition of grasses, shrubs, and trees
					tempdat <- matrix(data=NA, nrow=12, ncol=3)#matrix to hold biomass * percLive for grass,shrubs,trees
					colnames(tempdat) <- c("grass", "shrubs", "tree")
					tempdat[,1] <- swProd_MonProd_grass(swRunScenariosData[[sc]])[,2]*swProd_MonProd_grass(swRunScenariosData[[sc]])[,3]
					tempdat[,2] <- swProd_MonProd_shrub(swRunScenariosData[[sc]])[,2]*swProd_MonProd_shrub(swRunScenariosData[[sc]])[,3]
					tempdat[,3] <- swProd_MonProd_tree(swRunScenariosData[[sc]])[,2]*swProd_MonProd_tree(swRunScenariosData[[sc]])[,3]
					
					sumWeightedLiveBiomassByMonth <- apply(sweep(tempdat, MARGIN=2, fracs, FUN="*"), MARGIN=1, function(x) sum(x)) #sweep out fractionals, and sum over rows
					maxMonth <- which(sumWeightedLiveBiomassByMonth==max(sumWeightedLiveBiomassByMonth)) #returns index, which is the month, of max bio
					meanPeakMonth <- circ.mean(maxMonth, 12)
					duration <- circ.range(maxMonth, 12)+1
					
					resMeans[nv:(nv+1)] <- c(meanPeakMonth, duration) #just in case we get more then one month
					nv <- nv+2
				}
			#4
				if(any(simulation_timescales=="monthly") && aon$input_Phenology) {
					if(print.debug) print("Aggregation of input_Phenology")
					if(!exists("temp.mo")) temp.mo <- get_Temp_mo(sc) #see if we have data
					monthly.temp <- aggregate(temp.mo$mean, by=list(simTime2$month_ForEachUsedMonth), FUN=mean)[,2] #get mean monthly temp
					if(i_SWRunInformation$Y_WGS84 < 0) { #check for Southern Hemi
						monthly.temp <- c(monthly.temp[7:12], monthly.temp[1:6]) #rearrange temp
						Months_Above_Threshold <- c(7:12, 1:6)[which(monthly.temp > growing.season.threshold.tempC)] #get months above threshold, then map back to real months.
					} else {
						Months_Above_Threshold <- which(monthly.temp > growing.season.threshold.tempC) #get months above threshold
					}
					Input_PhenologyStart_Month <- Months_Above_Threshold[1] #get the first month
					Input_PhenologyEnd_Month <- tail(Months_Above_Threshold, n=1) #get the last month
					
					resMeans[nv:(nv+1)] <- c(Input_PhenologyStart_Month, Input_PhenologyEnd_Month)
					nv <- nv+2
				}
			#5
				if(aon$input_TranspirationCoeff){
					if(print.debug) print("Aggregation of input_TranspirationCoeff")
					Tcoeff <- swSoils_Layers(swRunScenariosData[[1]])[, 6:8]
					if(is.null(dim(Tcoeff))) Tcoeff <- matrix(Tcoeff, nrow=1)

					TaggLs <- sapply(aggLs, FUN=function(l) apply(Tcoeff[l,, drop=FALSE], 2, sum))
					if(length(bottomL) > 0 && !identical(bottomL, 0)){
						Ttb <- sapply(list(topL, bottomL), FUN=function(l) apply(Tcoeff[l,, drop=FALSE], 2, sum))
					} else {
						Ttb <- sapply(list(topL), FUN=function(l) apply(Tcoeff[l,, drop=FALSE], 2, sum))
					}
					
					iinv <- inv <- nv
					for(iv in 1:3){
						nv <- nv+SoilLayer_MaxNo #We don't know the max number of soil layers (aggLs_no) among all simulations, i.e., set to the maximum
						resMeans[(inv+(iv-1)*SoilLayer_MaxNo):(nv-1)] <- c(TaggLs[iv, ], rep(NA, times=SoilLayer_MaxNo-aggLs_no))
					}
					inv <- nv
					for(iv in 1:3){
						nv <- nv+2
						resMeans[(inv+(iv-1)*2):(nv-1)] <- Ttb[iv, ]
					}
					
					rm(Tcoeff, TaggLs, Ttb)
				}
			#6
				if(aon$input_ClimatePerturbations) {
					if(print.debug) print("Aggregation of input_ClimatePerturbations")
					resMeans[nv:(nv+35)] <- as.vector(as.numeric(ClimatePerturbationsVals[sc,]))
					nv <- nv+36
				}			
				
				#---Aggregation: Climate and weather
			#7
				if(any(simulation_timescales=="yearly") & aon$yearlyTemp){
					if(print.debug) print("Aggregation of yearlyTemp")
					if(!exists("temp.yr"))	temp.yr <- get_Temp_yr(sc)
					
					resMeans[nv] <- mean(temp.yr$mean)
					resSDs[nv] <- sd(temp.yr$mean)
					nv <- nv+1
				}
			#8
				if(any(simulation_timescales=="yearly") & aon$yearlyPPT){			
					if(print.debug) print("Aggregation of yearlyPPT")
					if(!exists("prcp.yr")) prcp.yr <- get_PPT_yr(sc)
					
					resMeans[nv] <- mean(prcp.yr$ppt)
					resSDs[nv] <- sd(prcp.yr$ppt)
					resMeans[nv+1] <- mean(snowofppt <- prcp.yr$snowfall/prcp.yr$ppt, na.rm=TRUE)
					resSDs[nv+1] <- sd(snowofppt, na.rm=TRUE)
					nv <- nv+2
					
					rm(snowofppt)
				}
			#9
				if(any(simulation_timescales=="daily") & any(simulation_timescales=="yearly") & aon$dailySnowpack){			
					if(print.debug) print("Aggregation of dailySnowpack")
					if(!exists("prcp.yr")) prcp.yr <- get_PPT_yr(sc)
					if(!exists("prcp.dy")) prcp.dy <- get_PPT_dy(sc)
					if(!exists("SWE.dy")) SWE.dy <- get_SWE_dy(sc)
					
					rainOnSnow <- aggregate(ifelse(SWE.dy$val > 0, prcp.dy$rain, 0), by=list(simTime2$year_ForEachUsedDay), FUN=sum)[, 2]#Fraction of rain that falls on snow
					
					resMeans[nv] <- mean(temp <- rainOnSnow / prcp.yr$ppt, na.rm=TRUE)
					resSDs[nv] <- sd(temp, na.rm=TRUE)
					nv <- nv+1
					
					rm(rainOnSnow)
				}
			#10	
				if(any(simulation_timescales=="daily") & aon$dailySnowpack){#daily snowpack: accountNSHemispheres_agg
					if(print.debug) print("Aggregation of dailySnowpack2")
					if(!exists("SWE.dy")) SWE.dy <- get_SWE_dy(sc)
					
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
								resMeans[nv:(nv+4)] <- c(apply(res.snow[, 2:3], 2, FUN=function(x) circ.mean(x, int=365, na.rm=TRUE)), apply(res.snow[,-(1:3)], 2, mean, na.rm=TRUE))
								resSDs[nv:(nv+4)] <- c(apply(res.snow[, 2:3], 2, FUN=function(x) circ.sd(x, int=365, na.rm=TRUE)), apply(res.snow[,-(1:3)], 2, sd, na.rm=TRUE))
							} else {
								resMeans[nv:(nv+4)] <- res.snow[1,-1]
								resSDs[nv:(nv+4)] <- 0
							}
							
							rm(snowyears, snowyear.trim, res.snow, adjDays)
						} else {
							resMeans[nv:(nv+4)] <- resSDs[nv:(nv+4)] <- 0					
						}
					} else {
						resMeans[nv:(nv+4)] <- resSDs[nv:(nv+4)] <- 0					
					}
					nv <- nv+5
				}
			#11
				if(any(simulation_timescales=="daily") & aon$dailyFrostInSnowfreePeriod){			
					if(print.debug) print("Aggregation of dailyFrostInSnowfreePeriod")
					if(!exists("temp.dy")) temp.dy <- get_Temp_dy(sc)
					if(!exists("SWE.dy")) SWE.dy <- get_SWE_dy(sc)
					
					for(iTmin in Tmin_crit_C){
						frostWithoutSnow <- aggregate(SWE.dy$val == 0 & temp.dy$min < iTmin, by=list(simTime2$year_ForEachUsedDay), FUN=sum)[, 2]#Numbers of days with min.temp < 0 and snow == 0
					
						resMeans[nv] <- mean(frostWithoutSnow, na.rm=TRUE)
						resSDs[nv] <- sd(frostWithoutSnow, na.rm=TRUE)
						nv <- nv+1
					}
					
					rm(frostWithoutSnow)
				}
				if(any(simulation_timescales=="daily") & aon$dailyHotDays){			
					if(print.debug) print("Aggregation of dailyHotDays")
					if(!exists("temp.dy")) temp.dy <- get_Temp_dy(sc)
					
					for(iTmax in Tmax_crit_C){
						HotDays <- aggregate(temp.dy$max > iTmax, by=list(simTime2$year_ForEachUsedDay), FUN=sum)[, 2]#Numbers of days with max.temp > 0
					
						resMeans[nv] <- mean(HotDays, na.rm=TRUE)
						resSDs[nv] <- sd(HotDays, na.rm=TRUE)
						nv <- nv+1
					}
					
					rm(HotDays)
				}
			#12
				if(any(simulation_timescales=="daily") & aon$dailyPrecipitationEventSizeDistribution){	#daily weather frequency distributions
					if(print.debug) print("Aggregation of dailyPrecipitationEventSizeDistribution")
					if(!exists("prcp.dy")) prcp.dy <- get_PPT_dy(sc)
					
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
					eventsPerYear <- apply(counts.summary, MARGIN=2, FUN=sum)
					freq.summary <- sweep(counts.summary, MARGIN=2, STATS=eventsPerYear, FUN="/")
					
					resMeans[nv] <- mean(eventsPerYear)
					resSDs[nv] <- sd(eventsPerYear)
					resMeans[(nv+1):(nv+7)] <- apply(freq.summary, MARGIN=1, FUN=mean)
					resSDs[(nv+1):(nv+7)] <- apply(freq.summary, MARGIN=1, FUN=sd)
					nv <- nv+8
					
					rm(events, counts.available, counts.summary, freq.summary, eventsPerYear)
				}
			#13
				if(any(simulation_timescales=="yearly") & aon$yearlyAET){
					if(print.debug) print("Aggregation of yearlyAET")
					if(!exists("AET.yr")) AET.yr <- get_AET_yr(sc)
					
					resMeans[nv] <- mean(AET.yr$val)
					resSDs[nv] <- sd(AET.yr$val)
					nv <- nv+1
				}
			#14
				if(any(simulation_timescales=="yearly") & aon$yearlyPET){
					if(print.debug) print("Aggregation of yearlyPET")
					if(!exists("PET.yr")) PET.yr <- get_PET_yr(sc)
					
					resMeans[nv] <- mean(PET.yr$val)
					resSDs[nv] <- sd(PET.yr$val)
					nv <- nv+1
				}
			#15
				#correl monthly swp (top and bottom) vs. pet and ppt vs. temp, use product moment correlation coefficient {eqn. 11.6, \Sala, 1997 #45}
				if(any(simulation_timescales=="monthly") & aon$monthlySeasonalityIndices){
					if(print.debug) print("Aggregation of monthlySeasonalityIndices")
					if(!exists("vwc.mo")) vwc.mo <- get_Response_aggL(sc, sw_vwc, "mo", 1, FUN=weighted.mean, weights=layers_width)
					if(!exists("swp.mo")) swp.mo <- get_SWP_aggL(vwc.mo)
					if(!exists("temp.mo")) temp.mo <- get_Temp_mo(sc)
					if(!exists("prcp.mo")) prcp.mo <- get_PPT_mo(sc)
					if(!exists("PET.mo")) PET.mo <- get_PET_mo(sc)
					
					cor2  <- function(y) cor(y[,1], y[,2])
					
					#in case var(ppt or swp)==0 => cor is undefined: exclude those years
					resMeans[nv] <- mean( temp <- by(data.frame(PET.mo$val, swp.mo$top), INDICES=simTime2$yearno_ForEachUsedMonth, FUN=cor2), na.rm=TRUE )
					resSDs[nv] <- sd(temp, na.rm=TRUE)
					if(length(bottomL) > 0 && !identical(bottomL, 0)){
						resMeans[nv+1] <- mean( temp <- by(data.frame(PET.mo$val, swp.mo$bottom), INDICES=simTime2$yearno_ForEachUsedMonth, FUN=cor2), na.rm=TRUE )
						resSDs[nv+1] <- sd(temp, na.rm=TRUE)
					}
					resMeans[nv+2] <- mean( temp <- by(data.frame(temp.mo$mean, prcp.mo$ppt), INDICES=simTime2$yearno_ForEachUsedMonth, FUN=cor2), na.rm=TRUE )
					resSDs[nv+2] <- sd(temp, na.rm=TRUE)
					
					nv <- nv+3
				}
				
				
				#---Aggregation: Climatic dryness
			#16
				if(any(simulation_timescales=="yearly") & any(simulation_timescales=="monthly") & aon$yearlymonthlyTemperateDrylandIndices){
					if(print.debug) print("Aggregation of yearlymonthlyTemperateDrylandIndices")
					if(!exists("prcp.yr")) prcp.yr <- get_PPT_yr(sc)
					if(!exists("PET.yr")) PET.yr <- get_PET_yr(sc)
					if(!exists("temp.mo")) temp.mo <- get_Temp_mo(sc)
					
					get.drylandindices <- function(annualPPT, annualPET, monthlyTemp){
						ai <- annualPPT/annualPET	#Deichmann, U. & L. Eklundh. 1991. Global digital datasets for land degradation studies: a GIS approach. Global Environment Monitoring System (GEMS), United Nations Environment Programme (UNEP), Nairobi, Kenya.
						TD <- ifelse((temp <- apply(matrix(data=monthlyTemp, ncol=12, byrow=TRUE), MARGIN=1, FUN=function(x) sum(x >= 10))) >= 4 & temp < 8, 1, 0) #Trewartha & Horn 1980, page 284: temperate areas
						criteria12 <- ifelse(TD == 1 & ai < 0.5, 1, 0)
						
						return(list(ai=ai, TD=TD, criteria12=criteria12))
					}
					
					di.ts <- get.drylandindices(annualPPT=prcp.yr$ppt, annualPET=PET.yr$val, monthlyTemp=temp.mo$mean)
					di.normals <- get.drylandindices(annualPPT=mean(prcp.yr$ppt), annualPET=mean(PET.yr$val), monthlyTemp=aggregate(temp.mo$mean, by=list(simTime2$month_ForEachUsedMonth), FUN=mean)$x)
					
					resMeans[nv:(nv+2)] <- unlist(di.normals)
					resMeans[(nv+3):(nv+5)] <- apply(temp <- cbind(di.ts$ai, di.ts$TD, di.ts$criteria12), MARGIN=2, FUN=mean, na.rm=TRUE)
					resSDs[(nv+3):(nv+5)] <- apply(temp, MARGIN=2, FUN=sd, na.rm=TRUE)
					nv <- nv+6
					
					rm(di.ts, di.normals)
				}
			#17
				if(any(simulation_timescales=="yearly") & aon$yearlyDryWetPeriods){			
					if(print.debug) print("Aggregation of yearlyDryWetPeriods")
					if(!exists("prcp.yr")) prcp.yr <- get_PPT_yr(sc)
					temp.rle <- rle(sign(prcp.yr$ppt - mean(prcp.yr$ppt)))
					
					resMeans[nv:(nv+1)] <- c(quantile(temp.rle$lengths[temp.rle$values==-1], probs=0.9, type=7), quantile(temp.rle$lengths[temp.rle$values==1], probs=0.9, type=7))
					nv <- nv+2
					
					rm(temp.rle)
				}
			#18
				if(any(simulation_timescales=="daily") & aon$dailyWeatherGeneratorCharacteristics){#daily response to weather generator treatments
					if(print.debug) print("Aggregation of dailyWeatherGeneratorCharacteristics")
					if(!exists("prcp.dy")) prcp.dy <- get_PPT_dy(sc)
					if(!exists("temp.dy")) temp.dy <- get_Temp_dy(sc)
					
					dws <- sapply(st_mo, FUN=function(m)
								return(list(mean=mean(temp <- unlist(sapply(simTime$useyrs, FUN=function(y) ((temp <- rle(prcp.dy$ppt[simTime2$month_ForEachUsedDay == m & simTime2$year_ForEachUsedDay == y] > 0))$lengths[temp$values]) )), na.rm=TRUE),
												sd=sd(temp, na.rm=TRUE))))
					
					dds <- sapply(st_mo, FUN=function(m)
								return(list(mean=mean(temp <- unlist(sapply(simTime$useyrs, FUN=function(y) ((temp <- rle(prcp.dy$ppt[simTime2$month_ForEachUsedDay == m & simTime2$year_ForEachUsedDay == y] == 0))$lengths[temp$values]) )), na.rm=TRUE),
												sd=sd(temp, na.rm=TRUE))))
					
					#tv <- sapply(st_mo, FUN=function(m) sd(temp.dy$mean[simTime2$month_ForEachUsedDay == m], na.rm=TRUE) )
					tv <- sapply(st_mo, FUN=function(m)
								return(list(mean=mean(temp <- unlist(sapply(simTime$useyrs, FUN=function(y) sd(temp.dy$mean[simTime2$month_ForEachUsedDay == m & simTime2$year_ForEachUsedDay == y], na.rm=TRUE) )), na.rm=TRUE),
												sd=sd(temp, na.rm=TRUE))))
					
					resMeans[nv+st_mo-1] <- unlist(dws[1, ])
					resSDs[nv+st_mo-1] <- unlist(dws[2, ])
					resMeans[nv+st_mo-1+12] <- unlist(dds[1, ])
					resSDs[nv+st_mo-1+12] <- unlist(dds[2, ])
					resMeans[nv+st_mo-1+24] <- unlist(tv[1, ])
					resSDs[nv+st_mo-1+24] <- unlist(tv[2, ])
					nv <- nv+36
					
					rm(dws, dds, tv)
				}
			#19
				if(any(simulation_timescales=="daily") & aon$dailyPrecipitationFreeEventDistribution){	#daily weather frequency distributions
					if(print.debug) print("Aggregation of dailyPrecipitationFreeEventDistribution")
					if(!exists("prcp.dy")) prcp.dy <- get_PPT_dy(sc)
					
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
					eventsPerYear <- apply(counts.summary, MARGIN=2, FUN=sum)
					freq.summary <- sweep(counts.summary, MARGIN=2, STATS=eventsPerYear, FUN="/")
					
					resMeans[nv] <- mean(eventsPerYear)
					resSDs[nv] <- sd(eventsPerYear)
					resMeans[(nv+1):(nv+4)] <- apply(freq.summary, MARGIN=1, FUN=mean)
					resSDs[(nv+1):(nv+4)] <- apply(freq.summary, MARGIN=1, FUN=sd)
					nv <- nv+5
					
					rm(durations, counts.available, counts.summary, freq.summary, eventsPerYear)
				}
			#20
				if(any(simulation_timescales=="monthly") & aon$monthlySPEIEvents){
					if(print.debug) print("Aggregation of monthlySPEIEvents")
					require(SPEI)
					#standardized precipitation-evapotranspiration index, SPEI: Vicente-Serrano, S.M., Beguer√≠a, S., Lorenzo-Lacruz, J., Camarero, J.s.J., L√≥pez-Moreno, J.I., Azorin-Molina, C., Revuelto, J.s., Mor√°n-Tejeda, E. & Sanchez-Lorenzo, A. (2012) Performance of Drought Indices for Ecological, Agricultural, and Hydrological Applications. Earth Interactions, 16, 1-27.
					if(!exists("PET.mo")) PET.mo <- get_PET_mo(sc)
					if(!exists("prcp.mo")) prcp.mo <- get_PPT_mo(sc)
					
					#n_variables is set for 4*4*3 with length(binSPEI_m) == 4 && length(probs) == 3
					binSPEI_m <- c(1, 12, 24, 48) #months
					probs <- c(0.025, 0.5, 0.975)	
					iresp <- rep(1:4, each=length(probs))
					
					for(iscale in seq_along(binSPEI_m)){
						rvec <- rep(NA, times=4 * length(probs))
						if(binSPEI_m[iscale] < length(prcp.mo$ppt)){
							spei_m <- as.numeric(spei(prcp.mo$ppt - PET.mo$val, scale=binSPEI_m[iscale])$fitted)
							spei_m <- spei_m[!is.na(spei_m)]
							runs <- rle(spei_m >= 0)
							
							if(sum(runs$values) > 0){
								rvec[iresp==1] <- quantile(runs$lengths[runs$values], probs=probs, type=7) #duration of positive spells
								rvec[iresp==2] <- quantile(spei_m[spei_m >= 0], probs=probs, type=7) #intensity of positive spells
							}
							if(sum(!runs$values) > 0){
								rvec[iresp==3] <- quantile(runs$lengths[!runs$values], probs=probs, type=7) #duration of negative spells
								rvec[iresp==4] <- quantile(spei_m[spei_m < 0], probs=probs, type=7) #intensity of positive spells
							}
						}
						
						resMeans[nv:(nv+length(rvec)-1)] <- rvec
						nv <- nv+length(rvec)
					}
				}
				#---Aggregation: Climatic control
			#21
				if(any(simulation_timescales=="monthly") & aon$monthlyPlantGrowthControls){	#Nemani RR, Keeling CD, Hashimoto H et al. (2003) Climate-Driven Increases in Global Terrestrial Net Primary Production from 1982 to 1999. Science, 300, 1560-1563.
					if(print.debug) print("Aggregation of monthlyPlantGrowthControls")
					if(!exists("temp.mo")) temp.mo <- get_Temp_mo(sc)
					if(!exists("PET.mo")) PET.mo <- get_PET_mo(sc)
					if(!exists("prcp.mo")) prcp.mo <- get_PPT_mo(sc)
					
					DayNumber_ForEachUsedMonth <- rle(simTime2$month_ForEachUsedDay)$lengths
					DayNumber_ForEachUsedYear <- rle(simTime2$year_ForEachUsedDay)$lengths
					
					#temperature control
					control_temp <- aggregate(ifelse(temp.mo$min > 5, 1, ifelse(temp.mo$min < -5, 0, (5 + temp.mo$min) / 10)) * DayNumber_ForEachUsedMonth, by=list(simTime2$yearno_ForEachUsedMonth), FUN=sum)[, 2] / DayNumber_ForEachUsedYear
					
					#moisture control
					aridity <- (prcp.mo$rain + prcp.mo$snowmelt) / PET.mo$val
					control_water <- aggregate(ifelse(aridity > 0.75, 1, ifelse(aridity < 0, 0, aridity/0.75)) * DayNumber_ForEachUsedMonth, by=list(simTime2$yearno_ForEachUsedMonth), FUN=sum)[, 2] / DayNumber_ForEachUsedYear
					
					#radiation control
					cloudiness <- swCloud_SkyCover(swRunScenariosData[[sc]])
					cloudiness <- rep(cloudiness, times=simTime$no.useyr)
					
					control_radiation <- aggregate((1 - ifelse(cloudiness < 10, 0, (cloudiness - 10) / 100 * 0.5 )) * DayNumber_ForEachUsedMonth, by=list(simTime2$yearno_ForEachUsedMonth), FUN=sum)[, 2] / DayNumber_ForEachUsedYear
					
					temp <- data.frame(control_temp, control_water, control_radiation)
					resMeans[nv:(nv+2)] <- apply(temp, 2, mean, na.rm=TRUE)
					resSDs[nv:(nv+2)] <- apply(temp, 2, sd, na.rm=TRUE)
					nv <- nv+3
					
					rm(DayNumber_ForEachUsedMonth, DayNumber_ForEachUsedYear, control_temp, control_water, control_radiation, aridity, temp, cloudiness)
				}
			#22
				if(any(simulation_timescales=="daily") & aon$dailyC4_TempVar){#Variables to estimate percent C4 species in North America: Teeri JA, Stowe LG (1976) Climatic patterns and the distribution of C4 grasses in North America. Oecologia, 23, 1-12.
					if(print.debug) print("Aggregation of dailyC4_TempVar")
					if(!exists("temp.dy")) temp.dy <- get_Temp_dy(sc)
					
					resMeans[nv:(nv+2)] <- (temp <- as.numeric(sw_dailyC4_TempVar(dailyTempMin=temp.dy$min, dailyTempMean=temp.dy$mean, simTime2)))[1:3]	#accountNSHemispheres_agg
					resSDs[nv:(nv+2)] <- temp[4:6]
					nv <- nv+3
				}
			#23
				if(any(simulation_timescales=="daily") & aon$dailyDegreeDays){	#Degree days based on daily temp
					if(print.debug) print("Aggregation of dailyDegreeDays")
					if(!exists("temp.dy")) temp.dy <- get_Temp_dy(sc)
					
					degday <- ifelse(temp.dy$mean > DegreeDayBase, temp.dy$mean - DegreeDayBase, 0) #degree days
					temp <- aggregate(degday, by=list(simTime2$year_ForEachUsedDay), FUN=sum)[, 2]
					
					resMeans[nv] <- mean(temp)
					resSDs[nv] <- sd(temp)
					nv <- nv+1	
					
					rm(degday)
				}
		
				#---Aggregation: Yearly water balance
			#24
				if(any(simulation_timescales=="yearly") & aon$yearlyWaterBalanceFluxes) {
					if(print.debug) print("Aggregation of yearlyWaterBalanceFluxes")
					if(!exists("prcp.yr")) prcp.yr <- get_PPT_yr(sc)
					if(!exists("Esurface.yr")) Esurface.yr <- get_Esurface_yr(sc)
					if(!exists("intercept.yr")) intercept.yr <- get_Interception_yr(sc)
					if(!exists("inf.yr")) inf.yr <- get_Inf_yr(sc)
					if(!exists("runoff.yr")) runoff.yr <- get_Runoff_yr(sc)
					if(!exists("transp.yr")) transp.yr <- get_Response_aggL(sc, sw_transp, "yr", 10, sum)
					if(!exists("AET.yr")) AET.yr <- get_AET_yr(sc)
					if(!exists("PET.yr")) PET.yr <- get_PET_yr(sc)
					if(!exists("Esoil.yr")) Esoil.yr <- get_Response_aggL(sc, sw_evsoil, "yr", 10, sum)
					if(!exists("deepDrain.yr")) deepDrain.yr <- get_DeepDrain_yr(sc)

					rain_toSoil <- prcp.yr$rain - intercept.yr$sum
					transp.tot <- transp.yr$top + transp.yr$bottom
					
					evap_soil.tot <- as.vector(Esoil.yr$top + Esoil.yr$bottom)
					evap.tot <- evap_soil.tot + Esurface.yr$sum + prcp.yr$snowloss
					
					temp1 <- 10 * runData[[sc]][[sw_percolation]][[sw_yr]]
					if(length(topL) > 1 && length(bottomL) > 0 && !identical(bottomL, 0)) {
						drain.topTobottom <- temp1[simTime$index.useyr, 1+DeepestTopLayer]
					} else {
						drain.topTobottom <- NA
					}
					temp1 <- 10 * runData[[sc]][[sw_hd]][[sw_yr]]
					if(length(topL) > 1) {
						hydred.topTobottom <- apply(temp1[simTime$index.useyr,1+topL], 1, sum)
					} else {
						hydred.topTobottom <- temp1[simTime$index.useyr,1+topL]
					}					
					
					if( any(simulation_timescales=="daily")) {
						temp1 <- 10 * runData[[sc]][[sw_swc]][[sw_dy]]
						if(simTime$index.usedy[1] == 1){ #simstartyr == startyr, then (simTime$index.usedy-1) misses first value
							index.usedyPlusOne <- simTime$index.usedy[-length(simTime$index.usedy)]+1
						} else {
							index.usedyPlusOne <- simTime$index.usedy
						}
						if(length(topL) > 1) {
							swcdyflux <- apply(temp1[index.usedyPlusOne,2+ld], 1, sum) - apply(temp1[index.usedyPlusOne-1,2+ld], 1, sum)
						} else {
							swcdyflux <- temp1[index.usedyPlusOne,2+ld] - temp1[index.usedyPlusOne-1,2+ld]
						}
						swc.flux <- aggregate(swcdyflux, by=list(temp1[index.usedyPlusOne,1]), FUN=sum)[,2]							
					} else {
						swc.flux <- NA
					}
					
					#mean fluxes
					resMeans[nv:(nv+22)] <- apply(fluxtemp <- cbind(prcp.yr$rain, rain_toSoil, prcp.yr$snowfall, prcp.yr$snowmelt, prcp.yr$snowloss, intercept.yr$sum, intercept.yr$veg, intercept.yr$litter, Esurface.yr$veg, Esurface.yr$litter, inf.yr$inf, runoff.yr$val, evap.tot, evap_soil.tot, Esoil.yr$top, Esoil.yr$bottom, transp.tot, transp.yr$top, transp.yr$bottom, hydred.topTobottom, drain.topTobottom, deepDrain.yr$val, swc.flux), 2, mean)
					resMeans[nv+23] <- ifelse(sum(transp.tot)==0, 0, mean(transp.yr$bottom/transp.tot))
					resMeans[nv+24] <- ifelse(sum(AET.yr$val)==0, 0, mean(transp.tot/AET.yr$val))
					resMeans[nv+25] <- ifelse(sum(AET.yr$val)==0, 0, mean(evap_soil.tot/AET.yr$val))
					resMeans[nv+26] <- ifelse(sum(PET.yr$val)==0, 0, mean(AET.yr$val/PET.yr$val))
					resMeans[nv+27] <- ifelse(sum(PET.yr$val)==0, 0, mean(transp.tot/PET.yr$val))
					resMeans[nv+28] <- ifelse(sum(PET.yr$val)==0, 0, mean(evap_soil.tot/PET.yr$val))
					
					#sd of fluxes
					resSDs[nv:(nv+22)] <- apply(fluxtemp, 2, sd)
					resSDs[nv+23] <- ifelse(sum(transp.tot)==0, 0, sd(transp.yr$bottom/transp.tot))
					resSDs[nv+24] <- ifelse(sum(AET.yr$val)==0, 0, sd(transp.tot/AET.yr$val))
					resSDs[nv+25] <- ifelse(sum(AET.yr$val)==0, 0, sd(evap_soil.tot/AET.yr$val))
					resSDs[nv+26] <- ifelse(sum(PET.yr$val)==0, 0, sd(AET.yr$val/PET.yr$val))
					resSDs[nv+27] <- ifelse(sum(PET.yr$val)==0, 0, sd(transp.tot/PET.yr$val))
					resSDs[nv+28] <- ifelse(sum(PET.yr$val)==0, 0, sd(evap_soil.tot/PET.yr$val))
					
					nv <- nv+29
					
					rm(rain_toSoil, transp.tot, evap_soil.tot, drain.topTobottom, hydred.topTobottom, index.usedyPlusOne, swcdyflux, swc.flux)
				}			
				#---Aggregation: Daily extreme values
			#25
				if(any(simulation_timescales=="daily") & aon$dailyTranspirationExtremes) {#mean and SD of DOY and value of minimum/maximum
					if(print.debug) print("Aggregation of dailyTranspirationExtremes")
					if(!exists("transp.dy")) transp.dy <- get_Response_aggL(sc, sw_transp, "dy", 10, FUN=sum)
					
					extremes <- as.matrix(aggregate(cbind(transp.dy$top + transp.dy$bottom), by=list(simTime2$year_ForEachUsedDay), FUN=function(x) c(max(x), min(x), circ.mean(which(x==max(x)), int=365), circ.mean(which(x==min(x)), int=365))))
					
					resMeans[nv:(nv+1)] <- apply(temp <- extremes[, c(2:3)], MARGIN=2, FUN=mean)
					resSDs[nv:(nv+1)] <- apply(temp, MARGIN=2, FUN=sd)						
					nv <- nv+2
					
					resMeans[nv:(nv+1)] <- apply(extremes[, 4:5], MARGIN=2, FUN=function(x) circ.mean(x, int=365))
					resSDs[nv:(nv+1)] <- apply(extremes[, 4:5], MARGIN=2, FUN=function(x) circ.sd(x, int=365))
					nv <- nv+2
					
					rm(extremes)
				}
			#26
				if(any(simulation_timescales=="daily") & aon$dailyTotalEvaporationExtremes) {
					if(print.debug) print("Aggregation of dailyTotalEvaporationExtremes")
					if(!exists("Esoil.dy")) Esoil.dy <- get_Response_aggL(sc, sw_evsoil, "dy", 10, FUN=sum)
					if(!exists("Esurface.dy")) Esurface.dy <- get_Esurface_dy(sc)
					
					extremes <- as.matrix(aggregate(cbind(Esoil.dy$top + Esoil.dy$bottom + Esurface.dy$sum), by=list(simTime2$year_ForEachUsedDay), FUN=function(x) c(max(x), min(x), circ.mean(which(x==max(x)), int=365), circ.mean(which(x==min(x)), int=365))))
					
					resMeans[nv:(nv+1)] <- apply(temp <- extremes[, c(2:3)], MARGIN=2, FUN=mean)
					resSDs[nv:(nv+1)] <- apply(temp, MARGIN=2, FUN=sd)						
					nv <- nv+2
					
					resMeans[nv:(nv+1)] <- apply(extremes[, 4:5], MARGIN=2, FUN=function(x) circ.mean(x, int=365))
					resSDs[nv:(nv+1)] <- apply(extremes[, 4:5], MARGIN=2, FUN=function(x) circ.sd(x, int=365))
					nv <- nv+2
					
					rm(extremes)
				}
			#27
				if(any(simulation_timescales=="daily") & aon$dailyDrainageExtremes) {
					if(print.debug) print("Aggregation of dailyDrainageExtremes")
					if(!exists("deepDrain.dy")) deepDrain.dy <- get_DeepDrain_dy(sc)
					
					extremes <- as.matrix(aggregate(deepDrain.dy$val, by=list(simTime2$year_ForEachUsedDay), FUN=function(x) c(max(x), min(x), circ.mean(which(x==max(x)), int=365), circ.mean(which(x==min(x)), int=365))))
					
					resMeans[nv:(nv+1)] <- apply(temp <- extremes[, c(2:3)], MARGIN=2, FUN=mean)
					resSDs[nv:(nv+1)] <- apply(temp, MARGIN=2, FUN=sd)						
					nv <- nv+2
					
					resMeans[nv:(nv+1)] <- apply(extremes[, 4:5], MARGIN=2, FUN=function(x) circ.mean(x, int=365))
					resSDs[nv:(nv+1)] <- apply(extremes[, 4:5], MARGIN=2, FUN=function(x) circ.sd(x, int=365))
					nv <- nv+2
					
					rm(extremes)
				}
			#28
				if(any(simulation_timescales=="daily") & aon$dailyInfiltrationExtremes) {
					if(print.debug) print("Aggregation of dailyInfiltrationExtremes")
					if(!exists("inf.dy")) inf.dy <- get_Inf_dy(sc)
					
					extremes <- as.matrix(aggregate(inf.dy$inf, by=list(simTime2$year_ForEachUsedDay), FUN=function(x) c(max(x), min(x), circ.mean(which(x==max(x)), int=365), circ.mean(which(x==min(x)), int=365))))
					
					resMeans[nv:(nv+1)] <- apply(temp <- extremes[, c(2:3)], MARGIN=2, FUN=mean)
					resSDs[nv:(nv+1)] <- apply(temp, MARGIN=2, FUN=sd)						
					nv <- nv+2
					
					resMeans[nv:(nv+1)] <- apply(extremes[, 4:5], MARGIN=2, FUN=function(x) circ.mean(x, int=365))
					resSDs[nv:(nv+1)] <- apply(extremes[, 4:5], MARGIN=2, FUN=function(x) circ.sd(x, int=365))
					nv <- nv+2
					
					rm(extremes)
				}
			#29
				if(any(simulation_timescales=="daily") & aon$dailyAETExtremes) {						
					if(print.debug) print("Aggregation of dailyAETExtremes")
					if(!exists("AET.dy")) AET.dy <- get_AET_dy(sc)
					
					extremes <- as.matrix(aggregate(AET.dy$val, by=list(simTime2$year_ForEachUsedDay), FUN=function(x) c(max(x), min(x), circ.mean(which(x==max(x)), int=365), circ.mean(which(x==min(x)), int=365))))
					
					resMeans[nv:(nv+1)] <- apply(temp <- extremes[, c(2:3)], MARGIN=2, FUN=mean)
					resSDs[nv:(nv+1)] <- apply(temp, MARGIN=2, FUN=sd)						
					nv <- nv+2
					
					resMeans[nv:(nv+1)] <- apply(extremes[, 4:5], MARGIN=2, FUN=function(x) circ.mean(x, int=365))
					resSDs[nv:(nv+1)] <- apply(extremes[, 4:5], MARGIN=2, FUN=function(x) circ.sd(x, int=365))
					nv <- nv+2
					
					rm(extremes)
				}
			#30
				if(any(simulation_timescales=="daily") & aon$dailySWPextremes){
					if(print.debug) print("Aggregation of dailySWPextremes")
					if(!exists("vwc.dy")) vwc.dy <- get_Response_aggL(sc, sw_vwc, "dy", 1, FUN=weighted.mean, weights=layers_width)
					if(!exists("swp.dy")) swp.dy <- get_SWP_aggL(vwc.dy)
					
					if(length(bottomL) > 0 && !identical(bottomL, 0)) {
						extremes <- as.matrix(aggregate(cbind(swp.dy$top, swp.dy$bottom), by=list(simTime2$year_ForEachUsedDay), FUN=function(x) c(max(x), min(x), circ.mean(which(x==max(x)), int=365), circ.mean(which(x==min(x)), int=365))))
					} else {
						extremes <- cbind(temp <- as.matrix(aggregate(swp.dy$top, by=list(simTime2$year_ForEachUsedDay), FUN=function(x) c(max(x), min(x), circ.mean(which(x==max(x)), int=365), circ.mean(which(x==min(x)), int=365)))), matrix(NA, nrow=nrow(temp), ncol=ncol(temp)-1))
					}
					
					resMeans[nv:(nv+3)] <- apply(extremes[, c(2:3, 6:7)], MARGIN=2, FUN=mean, na.rm=TRUE)
					resSDs[nv:(nv+3)] <- apply(extremes[, c(2:3, 6:7)], MARGIN=2, FUN=sd, na.rm=TRUE)
					nv <- nv+4

					resMeans[nv:(nv+3)] <- apply(extremes[, c(4:5, 8:9)], MARGIN=2, FUN=function(x) circ.mean(x, int=365))
					resSDs[nv:(nv+3)] <- apply(extremes[, c(4:5, 8:9)], MARGIN=2, FUN=function(x) circ.sd(x, int=365))
					nv <- nv+4
					
					rm(extremes)
				}
				if(any(simulation_timescales=="daily") & aon$dailyRechargeExtremes){
					if(print.debug) print("Aggregation of dailyRechargeExtremes")
					if(!exists("swc.dy")) swc.dy <- get_Response_aggL(sc, sw_swc, "dy", 10, FUN=sum)
					
					recharge.dy <- NULL
					recharge.dy$top <- swc.dy$top / (SWPtoVWC(-0.033, texture$sand.top, texture$clay.top) * 10 * sum(layers_width[topL]))
					
					if(length(bottomL) > 0 && !identical(bottomL, 0)) {
						recharge.dy$bottom <- swc.dy$bottom / (SWPtoVWC(-0.033, texture$sand.bottom, texture$clay.bottom) * 10 * sum(layers_width[bottomL])) 
						extremes <- as.matrix(aggregate(cbind(recharge.dy$top, recharge.dy$bottom), by=list(simTime2$year_ForEachUsedDay), FUN=function(x) c(max(x), min(x), circ.mean(which(x==max(x)), int=365), circ.mean(which(x==min(x)), int=365))))
					} else {
						extremes <- cbind(temp <- as.matrix(aggregate(recharge.dy$top, by=list(simTime2$year_ForEachUsedDay), FUN=function(x) c(max(x), min(x), circ.mean(which(x==max(x)), int=365), circ.mean(which(x==min(x)), int=365)))), matrix(NA, nrow=nrow(temp), ncol=ncol(temp)-1))
					}
					
					resMeans[nv:(nv+3)] <- apply(extremes[, c(2:3, 6:7)], MARGIN=2, FUN=function(x) mean(pmin(1, x), na.rm=TRUE))
					resSDs[nv:(nv+3)] <- apply(extremes[, c(2:3, 6:7)], MARGIN=2, FUN=function(x) sd(pmin(1, x), na.rm=TRUE))
					nv <- nv+4

					resMeans[nv:(nv+3)] <- apply(extremes[, c(4:5, 8:9)], MARGIN=2, FUN=function(x) circ.mean(x, int=365))
					resSDs[nv:(nv+3)] <- apply(extremes[, c(4:5, 8:9)], MARGIN=2, FUN=function(x) circ.sd(x, int=365))
					nv <- nv+4
					
					rm(recharge.dy, extremes)
				}
				
				#---Aggregation: Ecological dryness
			#31
				if(any(simulation_timescales=="daily") & aon$dailyWetDegreeDays){	#Wet degree days on daily temp and swp
					if(print.debug) print("Aggregation of dailyWetDegreeDays")
					if(!exists("vwc.dy")) vwc.dy <- get_Response_aggL(sc, sw_vwc, "dy", 1, FUN=weighted.mean, weights=layers_width)
					if(!exists("swp.dy")) swp.dy <- get_SWP_aggL(vwc.dy)
					if(!exists("temp.dy")) temp.dy <- get_Temp_dy(sc)
					
					degday <- ifelse(temp.dy$mean > DegreeDayBase, temp.dy$mean - DegreeDayBase, 0) #degree days
					
					for(icrit in seq(along=SWPcrit_MPa)){
						
						wet.top <- swp.dy$top >= SWPcrit_MPa[icrit]
						
						if(length(bottomL) > 0 && !identical(bottomL, 0)) {
							wet.bottom <- swp.dy$bottom >= SWPcrit_MPa[icrit]
						} else {
							wet.bottom <- matrix(data=NA, nrow=length(swp.dy$bottom), ncol=1)
						}
						
						wetdegday.top <- ifelse(wet.top > 0, degday, 0)
						wetdegday.bottom <- ifelse(wet.bottom > 0, degday, 0)
						wetdegday.any <- ifelse(wet.top + wet.bottom > 0, degday, 0)
						
						temp <- aggregate(data.frame(wetdegday.top, wetdegday.bottom, wetdegday.any), by=list(simTime2$year_ForEachUsedDay), FUN=sum)[, 2:4]
						resMeans[(nv+3*(icrit-1)):(nv+3*(icrit-1)+2)] <- apply(temp, MARGIN=2, FUN=mean)
						resSDs[(nv+3*(icrit-1)):(nv+3*(icrit-1)+2)] <- apply(temp, MARGIN=2, FUN=sd)
					}
					nv <- nv+3*length(SWPcrit_MPa)	
					
					rm(degday, wet.top, wet.bottom, wetdegday.top, wetdegday.bottom, wetdegday.any)
				}
			#32
				if(any(simulation_timescales=="monthly") & aon$monthlySWPdryness){#dry periods based on monthly swp data: accountNSHemispheres_agg
					if(print.debug) print("Aggregation of monthlySWPdryness")
					if(!exists("vwc.mo")) vwc.mo <- get_Response_aggL(sc, sw_vwc, "mo", 1, FUN=weighted.mean, weights=layers_width)
					if(!exists("swp.mo")) swp.mo <- get_SWP_aggL(vwc.mo)
					
					adjMonths <- ifelse(simTime2$month_ForEachUsedMonth[1] == simTime2$month_ForEachUsedMonth_NSadj[1], 0, 6)
					
					drymonths.top <- drymonths.bottom <- array(data=0, dim=c(length(SWPcrit_MPa), 12, simTime$no.useyr))
					for(icrit in seq(along=SWPcrit_MPa)){
						drymonths.top[icrit, , ] <- aggregate(swp.mo$top, by=list(simTime2$month_ForEachUsedMonth_NSadj), FUN=function(x) ifelse(x <= SWPcrit_MPa[icrit], 1, 0))[, -1]
						drymonths.bottom[icrit, , ] <- aggregate(swp.mo$bottom, by=list(simTime2$month_ForEachUsedMonth_NSadj), FUN=function(x) ifelse(x <= SWPcrit_MPa[icrit], 1, 0))[, -1]
					}
					
					years.top <- apply(drymonths.top, MARGIN=c(1, 3), FUN=sum)
					years.bottom <- apply(drymonths.bottom, MARGIN=c(1, 3), FUN=sum)
					
					resMeans[nv:(nv+2*length(SWPcrit_MPa)-1)] <- c(apply(years.top, MARGIN=1, FUN=mean), apply(years.bottom, MARGIN=1, FUN=mean))
					resSDs[nv:(nv+2*length(SWPcrit_MPa)-1)] <- c(apply(years.top, MARGIN=1, FUN=sd), apply(years.bottom, MARGIN=1, FUN=sd))
					
					nv <- nv+2*length(SWPcrit_MPa)
					
					start.top <- apply(drymonths.top, MARGIN=c(1, 3), FUN=match, x=1, nomatch=0)
					start.top[start.top != 0] <- ifelse((temp <- (start.top[start.top != 0] + adjMonths) %% 12) == 0, 12, temp)
					start.bottom <- apply(drymonths.bottom, MARGIN=c(1, 3), FUN=match, x=1, nomatch=0)
					start.bottom[start.bottom != 0] <- ifelse((temp <- (start.bottom[start.bottom != 0] + adjMonths) %% 12) == 0, 12, temp)
					
					resMeans[nv:(nv+2*length(SWPcrit_MPa)-1)] <- c(apply(start.top, MARGIN=1, FUN=function(x) circ.mean(x, int=12)), apply(start.bottom, MARGIN=1, FUN=function(x) circ.mean(x, int=12)))
					resSDs[nv:(nv+2*length(SWPcrit_MPa)-1)] <- c(apply(start.top, MARGIN=1, FUN=function(x) circ.sd(x, int=12)), apply(start.bottom, MARGIN=1, FUN=function(x) circ.sd(x, int=12)))
					
					nv <- nv+2*length(SWPcrit_MPa)
					
					rm(drymonths.top, drymonths.bottom, years.top, start.top, years.bottom, start.bottom, adjMonths)
				}
			#33
				if(any(simulation_timescales=="daily") & aon$dailySWPdrynessANDwetness){#Dry and wet periods based on daily swp: accountNSHemispheres_agg
					if(print.debug) print("Aggregation of dailySWPdrynessANDwetness")
					if(!exists("vwc.dy.all")) vwc.dy.all <- get_Response_aggL(sc, sw_vwc, "dyAll", 1, FUN=weighted.mean, weights=layers_width)
					if(!exists("swp.dy.all")) swp.dy.all <- get_SWP_aggL(vwc.dy.all) #swp.dy.all is required to get all layers
					
					adjDays <- simTime2$doy_ForEachUsedDay_NSadj[1] - simTime2$doy_ForEachUsedDay[1]
					durationDryPeriods.min <- 10 # days
					
					for(icrit in seq(along=SWPcrit_MPa)){
						
						wet_crit <- swp.dy.all$val >= SWPcrit_MPa[icrit]
						if(length(topL) > 1) {
							wet.top <- apply(wet_crit[simTime$index.usedy,2+topL], 1, sum)
						} else {
							wet.top <- wet_crit[simTime$index.usedy,2+topL]
						}
						if(length(bottomL) > 1) {
							wet.bottom <- apply(wet_crit[simTime$index.usedy,2+bottomL], 1, sum)
						} else if(length(bottomL) > 0 && !identical(bottomL, 0)) {
							wet.bottom  <- ifelse(wet_crit[simTime$index.usedy,2+bottomL], 1, 0)
						}
						
						AtLeastOneWet.top <- ifelse(wet.top>0,1,0)
						AllWet.top <- ifelse(wet.top==length(topL),1,0)
						AllDry.top <- ifelse(wet.top==0,1,0)
						AtLeastOneDry.top <- ifelse(wet.top<length(topL),1,0)
						
						if(length(bottomL) > 0 && !identical(bottomL, 0)){
							AtLeastOneWet.bottom <- ifelse(wet.bottom>0,1,0)
							AllWet.bottom <- ifelse(wet.bottom==length(bottomL),1,0)
							AllDry.bottom <- ifelse(wet.bottom==0,1,0)
							AtLeastOneDry.bottom <- ifelse(wet.bottom<length(bottomL),1,0)
						}
						
						#wet periods
						res.wet <- matrix(data=0, nrow=length(unique(simTime2$year_ForEachUsedDay_NSadj)), ncol=8)
						res.wet[, 1] <- aggregate(AtLeastOneWet.top, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=sum)[,2] # total number of days per year when at least one top layer is wet
						res.wet[, 3] <- aggregate(AtLeastOneWet.top, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=max.duration )[,2] # maximum number of continous days when at least one top layers is wet
						res.wet[, 5] <- aggregate(AllWet.top, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=sum)[,2] # total number of days per year when all top layer are wet
						res.wet[, 7] <- aggregate(AllWet.top, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=max.duration )[,2] # maximum number of continous days when all top layers are wet
						
						if(length(bottomL) > 0 && !identical(bottomL, 0)){
							res.wet[, 2] <- aggregate(AtLeastOneWet.bottom, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=sum)[,2]	# total number of days per year when at least one bottom layer is wet
							res.wet[, 4] <- aggregate(AtLeastOneWet.bottom, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=max.duration )[,2] # maximum number of continous days when at least one bottom layers is wet
							res.wet[, 6] <- aggregate(AllWet.bottom, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=sum)[,2]  # total number of days per year when all bottom layer are wet
							res.wet[, 8] <- aggregate(AllWet.bottom, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=max.duration )[,2] # maximum number of continous days when all bottom layers are wet
						}
						
						#dry periods
						res.dry <- matrix(data=0, nrow=length(unique(simTime2$year_ForEachUsedDay_NSadj)), ncol=8)
						res.dry[,3] <- aggregate(AllDry.top, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=sum)[,2] #total number of days/year when all top layers are dry
						res.dry[,4] <- aggregate(AllDry.top, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=max.duration )[,2] #maximum number of continous days when all top layers are dry
						res.dry[,1] <- aggregate(AtLeastOneDry.top, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=startDoyOfDuration, duration=durationDryPeriods.min)[,2] - adjDays	# start days/year when at least one of top layers are dry for at least ten days
						res.dry[,2] <- aggregate(AtLeastOneDry.top, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=endDoyAfterDuration, duration=durationDryPeriods.min)[,2] - adjDays	# end days/year when at least one of top layers have been dry for at least ten days
						res.dry[,3] <- ifelse(res.dry[,2]-res.dry[,1]>0, res.dry[,3], 0) #correct [,c(3,7)] for years when start<end otherwise set 0							
						
						if(length(bottomL) > 0 && !identical(bottomL, 0)){
							res.dry[,7] <- aggregate(AllDry.bottom, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=sum)[,2]#total number of days/year when all bottom layers are dry
							res.dry[,8] <- aggregate(AllDry.bottom, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=max.duration )[,2] #maximum number of continous days when all bottom layers are dry
							res.dry[,5] <- aggregate(AtLeastOneDry.bottom, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=startDoyOfDuration, duration=durationDryPeriods.min)[,2] - adjDays	# start days/year when at least one of bottom layers are dry for at least ten days
							res.dry[,6] <- aggregate(AtLeastOneDry.bottom, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=endDoyAfterDuration, duration=durationDryPeriods.min)[,2] - adjDays # end days/year when at least one of bottom layers have been dry for at least ten days
							res.dry[,7] <- ifelse(res.dry[,6]-res.dry[,5]>0, res.dry[,7], 0) #correct [,c(3,7)] for years when start<end otherwise set 0
						}
						
						#aggregate results
						resMeans[(nv+16*(icrit-1)):(nv+16*icrit-1)] <- c(apply(temp <- data.frame(res.wet, res.dry[, -c(1:2, 5:6)]), MARGIN=2, FUN=mean, na.rm=TRUE),
								apply(res.dry[, c(1:2, 5:6)], MARGIN=2, FUN=function(x) circ.mean(x, int=365, na.rm=TRUE)))
						resSDs[(nv+16*(icrit-1)):(nv+16*icrit-1)] <- c(apply(temp, MARGIN=2, FUN=sd, na.rm=TRUE),
								apply(res.dry[, c(1:2, 5:6)], MARGIN=2, FUN=function(x) circ.sd(x, int=365, na.rm=TRUE)))
					}
					nv <- nv+16*length(SWPcrit_MPa)	
					
					rm(res.dry, wet.top, wet_crit, AtLeastOneWet.top, AllWet.top, AllDry.top)
					if(length(bottomL) > 0 && !identical(bottomL, 0)) rm(wet.bottom, AtLeastOneWet.bottom, AllWet.bottom, AllDry.bottom)
				}
			#34
				if(any(simulation_timescales=="daily") & aon$dailySuitablePeriodsDuration){
					if(print.debug) print("Aggregation of dailySuitablePeriodsDuration")
					if(!exists("vwc.dy")) vwc.dy <- get_Response_aggL(sc, sw_vwc, "dy", 1, FUN=weighted.mean, weights=layers_width)
					if(!exists("swp.dy")) swp.dy <- get_SWP_aggL(vwc.dy)
					if(!exists("temp.dy")) temp.dy <- get_Temp_dy(sc)
					if(!exists("SWE.dy")) SWE.dy <- get_SWE_dy(sc)
					
					quantiles <- c(0.05, 0.5, 0.95)
					snowfree <- SWE.dy$val == 0
					niceTemp <- temp.dy$mean >= DegreeDayBase
					
					for(icrit in seq(along=SWPcrit_MPa)){
						wet.top <- swp.dy$top >= SWPcrit_MPa[icrit]
						
						if(length(bottomL) > 0 && !identical(bottomL, 0)){
							wet.bottom <- swp.dy$bottom >= SWPcrit_MPa[icrit]
						} else {
							wet.bottom <- rep(FALSE, length(wet.top))
						}

						durations.top <- sapply(simTime$useyrs, FUN=function(y) {if(length(temp <- (temp <- rle((snowfree & niceTemp & wet.top)[simTime2$year_ForEachUsedDay == y]))$lengths[temp$values]) > 0) {return(max(temp))} else {return(0)}} )
						durations.bottom <- sapply(simTime$useyrs, FUN=function(y) {if(length(temp <- (temp <- rle((snowfree & niceTemp & wet.bottom)[simTime2$year_ForEachUsedDay == y]))$lengths[temp$values]) > 0) {return(max(temp))} else {return(0)}} )
							
						resMeans[nv:(nv+2*length(quantiles)-1)] <- c(quantile(durations.top, probs=quantiles, type=8), quantile(durations.bottom, probs=quantiles, type=8))
							
						nv <- nv+2*length(quantiles)
					}
					
					rm(wet.top, wet.bottom, durations.top, snowfree, niceTemp)
				}
				
				if(any(simulation_timescales=="daily") & aon$dailySuitablePeriodsAvailableWater){
					if(print.debug) print("Aggregation of dailySuitablePeriodsAvailableWater")
					if(!exists("swc.dy")) swc.dy <- get_Response_aggL(sc, sw_swc, "dy", 10, FUN=sum)
					if(!exists("temp.dy")) temp.dy <- get_Temp_dy(sc)
					if(!exists("SWE.dy")) SWE.dy <- get_SWE_dy(sc)
					
					suitable <- (SWE.dy$val == 0) & (temp.dy$mean >= DegreeDayBase)
					
					cut0 <- function(x) {x[x < 0] <- 0; return(x)}
					for(icrit in seq(along=SWPcrit_MPa)){
						SWCcritT <- SWPtoVWC(SWPcrit_MPa[icrit], texture$sand.top, texture$clay.top) * 10 * sum(layers_width[topL])
						swa.top <- ifelse(suitable, cut0(swc.dy$top - SWCcritT), 0)
						
						if(length(bottomL) > 0 && !identical(bottomL, 0)){
							SWCcritB <- SWPtoVWC(SWPcrit_MPa[icrit], texture$sand.bottom, texture$clay.bottom) * 10 * sum(layers_width[bottomL])
							swa.bottom <- ifelse(suitable, cut0(swc.dy$bottom - SWCcritB), 0)
						} else {
							swa.bottom <- rep(0, length(swa.top))
						}

						temp <- aggregate(cbind(swa.top, swa.bottom), by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=sum)
						resMeans[nv:(nv+1)] <- apply(temp[, -1], 2, mean)
						resSDs[nv:(nv+1)] <- apply(temp[, -1], 2, sd)
						nv <- nv+2
					}
					
					rm(swa.top, swa.bottom, suitable)
				}
				
				if(any(simulation_timescales=="daily") & aon$dailySuitablePeriodsDrySpells){
					if(print.debug) print("Aggregation of dailySuitablePeriodsDrySpells")
					if(!exists("vwc.dy.all")) vwc.dy.all <- get_Response_aggL(sc, sw_vwc, "dyAll", 1, FUN=weighted.mean, weights=layers_width)
					if(!exists("swp.dy.all")) swp.dy.all <- get_SWP_aggL(vwc.dy.all) #swp.dy.all is required to get all layers
					if(!exists("temp.dy")) temp.dy <- get_Temp_dy(sc)
					if(!exists("SWE.dy")) SWE.dy <- get_SWE_dy(sc)
					
					suitable <- (SWE.dy$val == 0) & (temp.dy$mean >= DegreeDayBase)
					
					adjDays <- simTime2$doy_ForEachUsedDay_NSadj[1] - simTime2$doy_ForEachUsedDay[1]
					durationDryPeriods.min <- 10 # days
					
					for(icrit in seq(along=SWPcrit_MPa)){
						dry_crit <- swp.dy.all$val < SWPcrit_MPa[icrit]
						if(length(topL) > 1) {
							dry.top <- apply(dry_crit[simTime$index.usedy,2+topL], 1, sum)
						} else {
							dry.top <- dry_crit[simTime$index.usedy,2+topL]
						}
						dry.top <- (suitable & dry.top >= length(topL))
						if(length(bottomL) > 1) {
							dry.bottom <- apply(dry_crit[simTime$index.usedy,2+bottomL], 1, sum)
						} else if(length(bottomL) > 0 && !identical(bottomL, 0)) {
							dry.bottom <- ifelse(dry_crit[simTime$index.usedy,2+bottomL], 1, 0)
						}
						if(length(bottomL) > 0 && !identical(bottomL, 0)){
							dry.bottom <- (suitable & dry.bottom >= length(bottomL))
						} else {
							dry.bottom <- rep(FALSE, length(dry.top))
						}
						
						temp <- aggregate(cbind(dry.top, dry.bottom), by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=function(x) c(if(any((temp <- rle(x))$values)) c(mean(temp$lengths[temp$values]), max(temp$lengths[temp$values])) else c(0, 0), sum(x), startDoyOfDuration(x, duration=durationDryPeriods.min) - adjDays))
						resMeans[nv:(nv+7)] <- c(apply(temp$dry.top[, 1:3], 2, mean), circ.mean(x=temp$dry.top[, 4], int=365), apply(temp$dry.bottom[, 1:3], 2, mean), circ.mean(x=temp$dry.bottom[, 4], int=365))
						resSDs[nv:(nv+7)] <- c(apply(temp$dry.top[, 1:3], 2, sd), circ.sd(x=temp$dry.top[, 4], int=365), apply(temp$dry.bottom[, 1:3], 2, sd), circ.sd(x=temp$dry.bottom[, 4], int=365))
						nv <- nv+8
					}
					
					rm(dry.top, dry.bottom, suitable, dry_crit, adjDays, durationDryPeriods.min)
				}
				
				if(any(simulation_timescales=="daily") & aon$dailySWPdrynessDurationDistribution){#cummulative frequency distribution of durations of dry soils in each of the four seasons and for each of the SWP.crit
					if(print.debug) print("Aggregation of dailySWPdrynessDurationDistribution")
					if(!exists("vwc.dy")) vwc.dy <- get_Response_aggL(sc, sw_vwc, "dy", 1, FUN=weighted.mean, weights=layers_width)
					if(!exists("swp.dy")) swp.dy <- get_SWP_aggL(vwc.dy)
					
					deciles <- (0:10)*10/100
					quantiles <- (0:4)/4
					mo_seasons <- matrix(data=c(12,1:11), ncol=3, nrow=4, byrow=TRUE)
					season.flag <- c("DJF", "MAM", "JJA", "SON")
					seasonal.years <- c(simTime2$year_ForEachUsedDay[-(1:31)], rep(-9999, times=31))	#shift beginning of year to Dec 1
					
					for(icrit in seq(along=SWPcrit_MPa)){
						
						wet.top <- swp.dy$top >= SWPcrit_MPa[icrit]
						
						if(length(bottomL) > 0 && !identical(bottomL, 0)) wet.bottom <- swp.dy$bottom >= SWPcrit_MPa[icrit]
						
						for(season in 1:nrow(mo_seasons)){
							durations.top <- sapply(simTime$useyrs, FUN=function(y) {if(length(temp <- (temp <- rle(wet.top[seasonal.years == y & (simTime2$month_ForEachUsedDay %in% mo_seasons[season,])] == 0))$lengths[temp$values]) > 0) {return(max(temp))} else {return(0)}} )
							if(length(bottomL) > 0 && !identical(bottomL, 0)) durations.bottom <- sapply(simTime$useyrs, FUN=function(y) {if(length(temp <- (temp <- rle(wet.bottom[seasonal.years == y & (simTime2$month_ForEachUsedDay %in% mo_seasons[season,])] == 0))$lengths[temp$values]) > 0) {return(max(temp))} else {return(0)}} )
							
							resMeans[nv:(nv+length(quantiles)-1)] <- quantile(durations.top, probs=quantiles, type=7)
							resMeans[(nv+length(quantiles)):(nv+2*length(quantiles)-1)] <- if(length(bottomL) > 0 && !identical(bottomL, 0)) quantile(durations.bottom, probs=quantiles, type=7) else 0
							
							nv <- nv+2*length(quantiles)
						}
					}
					
					rm(wet.top, durations.top)
					if(length(bottomL) > 0 && !identical(bottomL, 0)) rm(wet.bottom)
				}
			#35
				if(any(simulation_timescales=="daily") && aon$dailySWPdrynessEventSizeDistribution){
					if(print.debug) print("Aggregation of dailySWPdrynessEventSizeDistribution")
					if(!exists("vwc.dy")) vwc.dy <- get_Response_aggL(sc, sw_vwc, "dy", 1, FUN=weighted.mean, weights=layers_width)
					if(!exists("swp.dy")) swp.dy <- get_SWP_aggL(vwc.dy)
					binSize <- c(1, 8, 15, 29, 57, 183, 367) #closed interval lengths in [days] within a year; NOTE: n_variables is set for binsN == 6
					binsN <- length(binSize) - 1
					
					EventDistribution <- function(data) {#data is the values for one year adj for SWPcrit_MPa; TRUE==dry
						bins <- rep(0, times=binsN)
						if(length(temp <- (temp <- rle(data))$lengths[temp$values]) > 0) { 	
							for(z in 1:length(temp)) {
								bins[findInterval(temp[z],binSize)] <- bins[findInterval(temp[z],binSize)] + 1 
							}
						}
						return(bins)
					}
					
					for(icrit in seq(along=SWPcrit_MPa)){
						
						dry.top <- swp.dy$top[simTime$index.usedy] < SWPcrit_MPa[icrit]
						
						if(length(bottomL) > 0 && !identical(bottomL, 0)) {
							dry.bottom <- swp.dy$bottom[simTime$index.usedy] < SWPcrit_MPa[icrit]
						}
						
						#apply over each year, rle just on selected year store runs in vec, if that is greater than 0 then add to that years bins else return 0s for that year. Will result in a matrix of 4 by Years
						binsYears.top <- aggregate(dry.top, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=EventDistribution)$x
						eventsPerYear <- apply(binsYears.top, MARGIN=1, FUN=sum)
						freqBins <- sweep(binsYears.top, MARGIN=1, STATS=eventsPerYear, FUN="/")
						events.top <- c(mean(eventsPerYear, na.rm=TRUE), sd(eventsPerYear, na.rm=TRUE))
						bin_top_mean <- apply(freqBins, MARGIN = 2, mean, na.rm=TRUE) #mean of each bin size across a year - vector of binsN
						bin_top_sd <- apply(freqBins, MARGIN = 2, sd, na.rm=TRUE) # sd of each bin size across a year - vector of binsN
						
						resMeans[nv] <- events.top[1]
						resSDs[nv] <- events.top[2]
						resMeans[(nv+1):(nv+binsN)] <- bin_top_mean
						resSDs[(nv+1):(nv+binsN)] <- bin_top_sd
						
						if(length(bottomL) > 0 && !identical(bottomL, 0)) {
							binsYears.bottom <- aggregate(dry.bottom, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=EventDistribution)$x
							eventsPerYear <- apply(binsYears.bottom, MARGIN=1, FUN=sum)
							freqBins <- sweep(binsYears.bottom, MARGIN=1, STATS=eventsPerYear, FUN="/")
							events.bottom <- c(mean(eventsPerYear, na.rm=TRUE), sd(eventsPerYear, na.rm=TRUE))
							bin_bottom_mean <- apply(freqBins, MARGIN = 2, mean, na.rm=TRUE)
							bin_bottom_sd <- apply(freqBins, MARGIN = 2, sd, na.rm=TRUE)
							
							resMeans[nv+binsN+1] <- events.bottom[1]
							resSDs[nv+binsN+1] <- events.bottom[2]
							resMeans[(nv+binsN+2):(nv+2*binsN+1)] <- bin_bottom_mean
							resSDs[(nv+binsN+2):(nv+2*binsN+1)] <- bin_bottom_sd
						}
						
						
						nv <- nv+2+2*binsN
					}
					rm(dry.top, binsN, binSize, events.top, eventsPerYear, freqBins)
					if(length(bottomL) > 0 && !identical(bottomL, 0)) rm(dry.bottom, events.bottom)
				}
			#36
				if(any(simulation_timescales=="daily") && aon$dailySWPdrynessIntensity) {
					if(print.debug) print("Aggregation of dailySWPdrynessIntensity")
					if(!exists("vwc.dy")) vwc.dy <- get_Response_aggL(sc, sw_vwc, "dy", 1, FUN=weighted.mean, weights=layers_width)
					
					cut0 <- function(x) {x[x < 0] <- 0; return(x)}
					SWCtop <- vwc.dy$top * sum(layers_width[topL])*10
					if(length(bottomL) > 0 && !identical(bottomL, 0)) SWCbottom <- vwc.dy$bottom * sum(layers_width[bottomL])*10
					
					for(icrit in seq(along=SWPcrit_MPa)){
						#amount of SWC required so that layer wouldn't be dry
						SWCcritT <- SWPtoVWC(SWPcrit_MPa[icrit], texture$sand.top, texture$clay.top) * sum(layers_width[topL])*10							
						missingSWCtop <- cut0(SWCcritT - SWCtop) 
						IntensitySum_top <- c(mean(temp <- sapply(simTime$useyrs, FUN=function(y) sum(missingSWCtop[simTime2$year_ForEachUsedDay == y])), na.rm=TRUE), sd(temp, na.rm=TRUE))
						IntensityMean_top <- c(mean(temp <- sapply(simTime$useyrs, FUN=function(y) mean((temp <- missingSWCtop[simTime2$year_ForEachUsedDay == y])[temp > 0], na.rm=TRUE)), na.rm=TRUE), sd(temp, na.rm=TRUE))
						IntensityDurationAndNumber_top <- c(apply(temp <- sapply(simTime$useyrs, FUN=function(y) c(mean(temp <- (temp <- rle(missingSWCtop[simTime2$year_ForEachUsedDay == y] > 0))$lengths[temp$values]), length(temp))), 1, mean), apply(temp, 1, sd))[c(1, 3, 2, 4)]
						
						if(length(bottomL) > 0 && !identical(bottomL, 0)) {
							SWCcritB <- SWPtoVWC(SWPcrit_MPa[icrit], texture$sand.bottom, texture$clay.bottom) * sum(layers_width[bottomL])*10
							missingSWCbottom <- cut0(SWCcritB - SWCbottom)
							IntensitySum_bottom <- c(mean(temp <- sapply(simTime$useyrs, FUN=function(y) sum(missingSWCbottom[simTime2$year_ForEachUsedDay == y])), na.rm=TRUE), sd(temp, na.rm=TRUE))
							IntensityMean_bottom <- c(mean(temp <- sapply(simTime$useyrs, FUN=function(y) mean((temp <- missingSWCbottom[simTime2$year_ForEachUsedDay == y])[temp > 0], na.rm=TRUE)), na.rm=TRUE), sd(temp, na.rm=TRUE))
							IntensityDurationAndNumber_bottom <- c(apply(temp <- sapply(simTime$useyrs, FUN=function(y) c(mean(temp <- (temp <- rle(missingSWCbottom[simTime2$year_ForEachUsedDay == y] > 0))$lengths[temp$values]), length(temp))), 1, mean), apply(temp, 1, sd))[c(1, 3, 2, 4)]
						}
						
						resMeans[nv:(nv+3)] <- c(IntensitySum_top[1], IntensityMean_top[1], IntensityDurationAndNumber_top[c(1, 3)])
						resSDs[nv:(nv+3)] <- c(IntensitySum_top[2], IntensityMean_top[2], IntensityDurationAndNumber_top[c(2, 4)])
						resMeans[(nv+4):(nv+7)] <- if(length(bottomL) > 0 && !identical(bottomL, 0)) c(IntensitySum_bottom[1], IntensityMean_bottom[1], IntensityDurationAndNumber_bottom[c(1, 3)]) else rep(0, 4)
						resSDs[(nv+4):(nv+7)] <- if(length(bottomL) > 0 && !identical(bottomL, 0)) c(IntensitySum_bottom[2], IntensityMean_bottom[2], IntensityDurationAndNumber_bottom[c(2, 4)]) else rep(0, 4)
						
						nv <- nv+8
					}
					rm(	SWCcritT, missingSWCtop, IntensitySum_top, IntensityMean_top, IntensityDurationAndNumber_top)
					if(length(bottomL) > 0 && !identical(bottomL, 0)) rm(SWCcritB, missingSWCbottom, IntensitySum_bottom, IntensityMean_bottom, IntensityDurationAndNumber_bottom)
				}
				#---Aggregation: Mean monthly values
			#37
				if(any(simulation_timescales=="monthly") & aon$monthlyTemp){
					if(print.debug) print("Aggregation of monthlyTemp")
					if(!exists("temp.mo")) temp.mo <- get_Temp_mo(sc)
					
					resMeans[nv+st_mo-1] <- aggregate(temp.mo$mean, by=list(simTime2$month_ForEachUsedMonth), FUN=mean)[,2]
					resSDs[nv+st_mo-1] <- aggregate(temp.mo$mean, by=list(simTime2$month_ForEachUsedMonth), FUN=sd)[,2]
					nv <- nv+12
				}
			#38
				if(any(simulation_timescales=="monthly") & aon$monthlyPPT){
					if(print.debug) print("Aggregation of monthlyPPT")
					if(!exists("prcp.mo")) prcp.mo <- get_PPT_mo(sc)
					
					resMeans[nv+st_mo-1] <- aggregate(prcp.mo$ppt, by=list(simTime2$month_ForEachUsedMonth), FUN=mean)[,2]
					resSDs[nv+st_mo-1] <- aggregate(prcp.mo$ppt, by=list(simTime2$month_ForEachUsedMonth), FUN=sd)[,2]
					nv <- nv+12
				}
			#39
				if(any(simulation_timescales=="monthly") & aon$monthlySnowpack){
					if(print.debug) print("Aggregation of monthlySnowpack")
					if(!exists("SWE.mo")) SWE.mo <- get_SWE_mo(sc)
					
					resMeans[nv+st_mo-1] <- aggregate(SWE.mo$val, by=list(simTime2$month_ForEachUsedMonth), FUN=mean)[,2]
					resSDs[nv+st_mo-1] <- aggregate(SWE.mo$val, by=list(simTime2$month_ForEachUsedMonth), FUN=sd)[,2]
					nv <- nv+12
				}
			#40
				if(any(simulation_timescales == "monthly") & aon$monthlySoilTemp) {
					if(print.debug) print("Aggregation of monthlySoilTemp")
					if(!exists("soiltemp.mo")) soiltemp.mo <- get_Response_aggL(sc, sw_soiltemp, "mo", scaler=1, FUN=weighted.mean, weights=layers_width)
					
					resMeans[nv+st_mo-1] <- soiltemp.mo$aggMean.top
					resMeans[nv+st_mo-1+12] <- soiltemp.mo$aggMean.bottom
					nv <- nv+24
				}
			#41
				if(any(simulation_timescales=="monthly") & aon$monthlyRunoff){
					if(print.debug) print("Aggregation of monthlyRunoff")
					if(!exists("runoff.mo")) runoff.mo <- get_Runoff_mo(sc)
					
					resMeans[nv+st_mo-1] <- aggregate(runoff.mo$val, by=list(simTime2$month_ForEachUsedMonth), FUN=mean)[,2]
					resSDs[nv+st_mo-1] <- aggregate(runoff.mo$val, by=list(simTime2$month_ForEachUsedMonth), FUN=sd)[,2]
					nv <- nv+12
				}
			#42
				if(any(simulation_timescales=="monthly") & aon$monthlyHydraulicRedistribution){
					if(print.debug) print("Aggregation of monthlyHydraulicRedistribution")
					if(!exists("hydred.mo")) hydred.mo <- get_Response_aggL(sc, sw_hd, "mo", 10, FUN=sum)
					
					resMeans[nv+st_mo-1] <- hydred.mo$aggMean.top
					resMeans[nv+st_mo-1+12] <- hydred.mo$aggMean.bottom
					nv <- nv+24
				}
			#43
				if(any(simulation_timescales=="monthly") & aon$monthlyInfiltration){
					if(print.debug) print("Aggregation of monthlyInfiltration")
					if(!exists("inf.mo")) inf.mo <- get_Inf_mo(sc)
					
					resMeans[nv+st_mo-1] <- aggregate( inf.mo$inf , by=list(simTime2$month_ForEachUsedMonth), FUN=mean)[,2]
					resSDs[nv+st_mo-1] <- aggregate( inf.mo$inf , by=list(simTime2$month_ForEachUsedMonth), FUN=sd)[,2]
					nv <- nv+12
				}
				if(any(simulation_timescales=="monthly") & aon$monthlyDeepDrainage){
					if(print.debug) print("Aggregation of monthlyDeepDrainage")
					if(!exists("deepDrain.mo")) deepDrain.mo <- get_DeepDrain_mo(sc)
					
					resMeans[nv+st_mo-1] <- aggregate( deepDrain.mo$val , by=list(simTime2$month_ForEachUsedMonth), FUN=mean)[,2]
					resSDs[nv+st_mo-1] <- aggregate( deepDrain.mo$val , by=list(simTime2$month_ForEachUsedMonth), FUN=sd)[,2]
					nv <- nv+12
				}
			#44
				if(any(simulation_timescales=="monthly") & aon$monthlySWP){
					if(print.debug) print("Aggregation of monthlySWP")
					if(!exists("vwc.mo")) vwc.mo <- get_Response_aggL(sc, sw_vwc, "mo", 1, FUN=weighted.mean, weights=layers_width)
					if(!exists("swp.mo")) swp.mo <- get_SWP_aggL(vwc.mo)
					
					resMeans[nv+st_mo-1] <- swp.mo$aggMean.top
					resMeans[nv+st_mo-1+12] <- swp.mo$aggMean.bottom
					nv <- nv+24
				}
			#45
				if(any(simulation_timescales=="monthly") & aon$monthlyVWC){
					if(print.debug) print("Aggregation of monthlyVWC")
					if(!exists("vwc.mo")) vwc.mo <- get_Response_aggL(sc, sw_vwc, "mo", 1, FUN=weighted.mean, weights=layers_width)
					
					resMeans[nv+st_mo-1] <- vwc.mo$aggMean.top
					resMeans[nv+st_mo-1+12] <- vwc.mo$aggMean.bottom
					nv <- nv+24
				}		
			#46
				if(any(simulation_timescales=="monthly") & aon$monthlySWC){
					if(print.debug) print("Aggregation of monthlySWC")
					if(!exists("swc.mo")) swc.mo <- get_Response_aggL(sc, sw_swc, "mo", 10, FUN=sum)
					
					resMeans[nv+st_mo-1] <- swc.mo$aggMean.top
					resMeans[nv+st_mo-1+12] <- swc.mo$aggMean.bottom
					nv <- nv+24
				}
			#47
				if(any(simulation_timescales=="monthly") & aon$monthlySWA){
					if(print.debug) print("Aggregation of monthlySWA")
					if(!exists("swa.mo")) swa.mo <- get_Response_aggL(sc, sw_swa, "mo", 10, FUN=sum)
					
					resMeans[nv+st_mo-1] <- swa.mo$aggMean.top
					resMeans[nv+st_mo-1+12] <- swa.mo$aggMean.bottom
					nv <- nv+24
				}
			#48
				if(any(simulation_timescales=="monthly") & aon$monthlyTranspiration){
					if(print.debug) print("Aggregation of monthlyTranspiration")
					if(!exists("transp.mo")) transp.mo <- get_Response_aggL(sc, sw_transp, "mo", 10, FUN=sum)
					
					resMeans[nv+st_mo-1] <- transp.mo$aggMean.top
					resMeans[nv+st_mo-1+12] <- transp.mo$aggMean.bottom
					nv <- nv+24
				}
			#49
				if(any(simulation_timescales=="monthly") & aon$monthlySoilEvaporation){
					if(print.debug) print("Aggregation of monthlySoilEvaporation")
					if(!exists("Esoil.mo")) Esoil.mo <- get_Response_aggL(sc, sw_evsoil, "mo", 10, FUN=sum)
					
					resMeans[nv+st_mo-1] <- aggregate(temp <- Esoil.mo$top + Esoil.mo$bottom, by=list(simTime2$month_ForEachUsedMonth), FUN=mean)[,2]
					resSDs[nv+st_mo-1] <- aggregate(temp, by=list(simTime2$month_ForEachUsedMonth), FUN=sd)[,2]
					nv <- nv+12
				}
			#50
				if(any(simulation_timescales=="monthly") & aon$monthlyAET){
					if(print.debug) print("Aggregation of monthlyAET")
					if(!exists("AET.mo")) AET.mo <- get_AET_mo(sc)
					
					resMeans[nv+st_mo-1] <- aggregate( AET.mo$val , by=list(simTime2$month_ForEachUsedMonth), FUN=mean)[,2]
					resSDs[nv+st_mo-1] <- aggregate( AET.mo$val , by=list(simTime2$month_ForEachUsedMonth), FUN=sd)[,2]
					nv <- nv+12
				}
			#51
				if(any(simulation_timescales=="monthly") & aon$monthlyPET){
					if(print.debug) print("Aggregation of monthlyPET")
					if(!exists("PET.mo")) PET.mo <- get_PET_mo(sc)
					
					resMeans[nv+st_mo-1] <- aggregate( PET.mo$val , by=list(simTime2$month_ForEachUsedMonth), FUN=mean)[,2]
					resSDs[nv+st_mo-1] <- aggregate( PET.mo$val , by=list(simTime2$month_ForEachUsedMonth), FUN=sd)[,2]
					nv <- nv+12
				}
			#52
				if(any(simulation_timescales=="monthly") & aon$monthlyAETratios){
					if(print.debug) print("Aggregation of monthlyAETratios")
					if(!exists("AET.mo")) AET.mo <- get_AET_mo(sc)
					if(!exists("Esoil.mo")) Esoil.mo <- get_Response_aggL(sc, sw_evsoil, "mo", 10, FUN=sum)
					if(!exists("transp.mo")) transp.mo <- get_Response_aggL(sc, sw_transp, "mo", 10, FUN=sum)
					
					resMeans[nv+st_mo-1] <- aggregate( temp <- ifelse( AET.mo$val == 0, 0, (transp.mo$top + transp.mo$bottom) / AET.mo$val) , by=list(simTime2$month_ForEachUsedMonth), FUN=mean)[,2]
					resSDs[nv+st_mo-1] <- aggregate( temp, by=list(simTime2$month_ForEachUsedMonth), FUN=sd)[,2]
					resMeans[nv+st_mo-1+12] <- aggregate( temp <- ifelse( AET.mo$val == 0, 0, (Esoil.mo$top + Esoil.mo$bottom) / AET.mo$val) , by=list(simTime2$month_ForEachUsedMonth), FUN=mean)[,2]
					resSDs[nv+st_mo-1+12] <- aggregate( temp, by=list(simTime2$month_ForEachUsedMonth), FUN=sd)[,2]
					nv <- nv+24
				}
			#53
				if(any(simulation_timescales=="monthly") & aon$monthlyPETratios){
					if(print.debug) print("Aggregation of monthlyPETratios")
					if(!exists("PET.mo")) PET.mo <- get_PET_mo(sc)
					if(!exists("Esoil.mo")) Esoil.mo <- get_Response_aggL(sc, sw_evsoil, "mo", 10, FUN=sum)
					if(!exists("transp.mo")) transp.mo <- get_Response_aggL(sc, sw_transp, "mo", 10, FUN=sum)
					
					resMeans[nv+st_mo-1] <- aggregate( temp <- ifelse( PET.mo$val == 0, 0, (transp.mo$top + transp.mo$bottom) / PET.mo$val) , by=list(simTime2$month_ForEachUsedMonth), FUN=mean)[,2]
					resSDs[nv+st_mo-1] <- aggregate( temp, by=list(simTime2$month_ForEachUsedMonth), FUN=sd)[,2]
					resMeans[nv+st_mo-1+12] <- aggregate( temp <- ifelse( PET.mo$val == 0, 0, (Esoil.mo$top + Esoil.mo$bottom) / PET.mo$val) , by=list(simTime2$month_ForEachUsedMonth), FUN=mean)[,2]
					resSDs[nv+st_mo-1+12] <- aggregate( temp, by=list(simTime2$month_ForEachUsedMonth), FUN=sd)[,2]
					nv <- nv+24
				}
				
				#---Aggregation: Potential regeneration
				#regeneration: accountNSHemispheres_agg
			#54
				if(any(simulation_timescales=="daily")  & aon$dailyRegeneration_bySWPSnow) {
					if(print.debug) print("Aggregation of dailyRegeneration_bySWPSnow")
					if(!exists("swp.dy.all")) swp.dy.all <- list(val=-1/10*runData[[sc]][[sw_swp]][[dy]])	#no vwcdy available!
					swp.surface <- swp.dy.all$val[simTime$index.usedy, 3]
					if(!exists("SWE.dy")) SWE.dy <- get_SWE_dy(sc)
					
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
					
					resMeans[nv] <- mean(temp <- c(by(data=data.frame(swp.surface, SWE.dy$val), INDICES=simTime2$year_ForEachUsedDay_NSadj, FUN=regenerationThisYear_YN )))
					resSDs[nv] <- sd(temp)
					nv <- nv+1
					
					rm(swp.surface)
				}
				
				#Artemisia tridentata regeneration according to factor model (2012-02-15, drs), call for every regeneration species
				#accountNSHemispheres_agg: param$Doy_SeedDispersalStart0 must be set correctly\
			#55
				if(any(simulation_timescales=="daily")  & aon$dailyRegeneration_GISSM & no.species_regeneration > 0){
					if(print.debug) print("Aggregation of dailyRegeneration_GISSM")
					#---Access daily data, which do not depend on specific species parameters, i.e., start of season
					if(!exists("swp.dy.all")) swp.dy.all <- list(val=-1/10*runData[[sc]][[sw_swp]][[sw_dy]])	#no vwcdy available!
					temp.snow <- runData[[sc]][[sw_snow]][[sw_dy]]
					temp.temp <- runData[[sc]][[sw_temp]][[sw_dy]]
					TmeanJan <- mean(temp.temp[simTime$index.usedy, 5][simTime2$month_ForEachUsedDay_NSadj==1], na.rm=TRUE)	#mean January (N-hemisphere)/July (S-hemisphere) air temperature based on normal 'doy'
					temp.soiltemp <- runData[[sc]][[sw_soiltemp]][[sw_dy]]
					if(inherits(temp.soiltemp, "try-error") || any(is.na(temp.soiltemp[, -(1:2)])) || all(temp.soiltemp[, -(1:2)] == 0)){
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
								} else if(temp.c < 0){
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
						return( apply(temp, MARGIN=1, FUN=function(x) {if(!is.na(x[1])){return(all(as.logical(x[2:(2 + x[1] - 1)])))} else {return(NA)} } ) )
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
							if(length(ld) == 1) swp <- matrix(swp, ncol=1)
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
						colnames(SeedlingMortality_CausesByYear) <- paste("Seedlings1stSeason.Mortality.", c("UnderneathSnowCover", "ByTmin", "ByTmax", "ByChronicSWPMax", "ByChronicSWPMin", "ByAcuteSWPMin",
										"DuringStoppedGrowth.DueSnowCover", "DuringStoppedGrowth.DueTmin", "DuringStoppedGrowth.DueTmax"), sep="")
						for(y in seq_along(RY.useyrs)){#for each year
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
									if(sum(temp <- !thisSeedlingGrowth_AbsenceOfSnowCover[index.thisSeedlingSeason]) > 0) stopped_byCauses_onRYdoy["Seedlings1stSeason.Mortality.DuringStoppedGrowth.DueSnowCover"] <- sg_RYdoy + which(temp)[1]
									#Minimum temperature
									thisSeedlingGrowth_AtAboveTmin <- calculate_SuitableGrowthThisYear_UnderCondition(favorable.conditions=thisSeedlingGrowing & thisYear_SeedlingGrowth_AtAboveTmin, consequences.unfavorable=param$SeedlingGrowth_0StopOr1Resume)
									if(sum(temp <- !thisSeedlingGrowth_AtAboveTmin[index.thisSeedlingSeason]) > 0) stopped_byCauses_onRYdoy["Seedlings1stSeason.Mortality.DuringStoppedGrowth.DueTmin"] <- sg_RYdoy + which(temp)[1]
									#Maximum temperature
									thisSeedlingGrowth_AtBelowTmax <- calculate_SuitableGrowthThisYear_UnderCondition(favorable.conditions=thisSeedlingGrowing & thisYear_SeedlingGrowth_AtBelowTmax, consequences.unfavorable=param$SeedlingGrowth_0StopOr1Resume)
									if(sum(temp <- !thisSeedlingGrowth_AtBelowTmax[index.thisSeedlingSeason]) > 0) stopped_byCauses_onRYdoy["Seedlings1stSeason.Mortality.DuringStoppedGrowth.DueTmax"] <- sg_RYdoy + which(temp)[1]
									#Updated days of growth or surviving
									thisSeedlingGrowing <- thisSeedlingGrowing & thisSeedlingGrowth_AbsenceOfSnowCover & thisSeedlingGrowth_AtAboveTmin & thisSeedlingGrowth_AtBelowTmax
									thisSeedlingLivingButNotGrowing <- !thisSeedlingGrowing
									if(sg_RYdoy > 1) thisSeedlingLivingButNotGrowing[1:(sg_RYdoy-1)] <- FALSE	#seedling germinated on sg_RYdoy, hence it cannot live before germination day
									
									#Book-keeping survival under above-ground conditions
									if(sum(temp <- thisYear_SeedlingMortality_UnderneathSnowCover[index.thisSeedlingSeason]) > 0) killed_byCauses_onRYdoy["Seedlings1stSeason.Mortality.UnderneathSnowCover"] <- sg_RYdoy + which(temp)[1] - 1
									if(sum(temp <- thisYear_SeedlingMortality_ByTmin[index.thisSeedlingSeason]) > 0) killed_byCauses_onRYdoy["Seedlings1stSeason.Mortality.ByTmin"] <- sg_RYdoy + which(temp)[1] - 1
									if(sum(temp <- thisYear_SeedlingMortality_ByTmax[index.thisSeedlingSeason]) > 0) killed_byCauses_onRYdoy["Seedlings1stSeason.Mortality.ByTmax"] <- sg_RYdoy + which(temp)[1] - 1
									
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
										if(sum(temp <- thisSeedling_thisYear_SeedlingMortality_ByChronicSWPMax[index.thisSeedlingSeason]) > 0) killed_byCauses_onRYdoy["Seedlings1stSeason.Mortality.ByChronicSWPMax"] <- sg_RYdoy + which(temp)[1] - 1
										#Check survival under chronic SWPMin
										thisSeedling_thisYear_SeedlingMortality_ByChronicSWPMin <- get_KilledBySoilLayers(relevantLayers=thisSeedling_thisYear_RootingSoilLayers, kill.conditions=thisYear_SeedlingMortality_ByChronicSWPMin)
										if(sum(temp <- thisSeedling_thisYear_SeedlingMortality_ByChronicSWPMin[index.thisSeedlingSeason]) > 0) killed_byCauses_onRYdoy["Seedlings1stSeason.Mortality.ByChronicSWPMin"] <- sg_RYdoy + which(temp)[1] - 1
										#Check survival under acute SWPMin
										thisSeedling_thisYear_SeedlingMortality_ByAcuteSWPMin <- get_KilledBySoilLayers(relevantLayers=thisSeedling_thisYear_RootingSoilLayers, kill.conditions=thisYear_SeedlingMortality_ByAcuteSWPMin)
										if(sum(temp <- thisSeedling_thisYear_SeedlingMortality_ByAcuteSWPMin[index.thisSeedlingSeason]) > 0) killed_byCauses_onRYdoy["Seedlings1stSeason.Mortality.ByAcuteSWPMin"] <- sg_RYdoy + which(temp)[1] - 1
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
						resMeans[nv:(nv+1)] <- apply(temp <- (res1.yr <- aggregate(temp1, by=list(year_ForEachUsedRYDay), FUN=sum))[simTime$index.useyr, -1] > 0, MARGIN=2, FUN=mean)
						resSDs[nv:(nv+1)] <- apply(temp, MARGIN=2, FUN=sd)
						#Periods with no successes
						resMeans[(nv+2):(nv+4)] <- quantile((rleGerm <- rle(temp[, 1]))$lengths[!rleGerm$values], probs=c(0.05, 0.5, 0.95), type=7)
						resMeans[(nv+5):(nv+7)] <- quantile((rleSling <- rle(temp[, 2]))$lengths[!rleSling$values], probs=c(0.05, 0.5, 0.95), type=7)
						#Mean number of days per year with success
						resMeans[(nv+8):(nv+9)] <- apply(res1.yr[simTime$index.useyr, -1], MARGIN=2, FUN=mean)
						resSDs[(nv+8):(nv+9)] <- apply(res1.yr[simTime$index.useyr, -1], MARGIN=2, FUN=sd)
						#Days of year (in normal count) of most frequent successes among years: #toDoy <- function(x) sort(ifelse((temp <- x+Doy_SeedDispersalStart-1) > 365, temp-365,temp)) #convert to normal doys							
						res1.dy <- aggregate(temp1, by=list(doy_ForEachUsedRYDay), FUN=sum)[, -1]
						get.DoyMostFrequentSuccesses <- function(doys){
							res1.max <- sapply(1:2, FUN=function(x) quantile(doys[doys[, x]>0, x], probs=c(0.1, 1), type=7))
							get.DoyAtLevel <- function(x, level) which(x == level & x > 0)
							if(all(!temp1[, 1])){#no successful germination
								germ.doy <- list(NA, NA)
							} else {
								germ.doy <- lapply(1:2, FUN=function(x) get.DoyAtLevel(doys[, 1], res1.max[x, 1]))
							}
							if(all(!temp1[, 2])){#no successful seedlings
								sling.doy <- list(NA, NA)
							} else {
								sling.doy <- lapply(1:2, FUN=function(x) get.DoyAtLevel(doys[, 2], res1.max[x, 2]))
							}
							res1.max <- list(germ.doy, sling.doy)
							return( unlist(lapply(res1.max, FUN=function(x) c(min(x[[1]]), median(x[[2]]), max(x[[1]])))) )
						}
						resMeans[(nv+10):(nv+15)] <- get.DoyMostFrequentSuccesses(res1.dy)
						#Mean number of days when germination is restricted due to conditions
						resMeans[(nv+16):(nv+20)] <- apply((res2.yr <- aggregate(temp2, by=list(year_ForEachUsedRYDay), FUN=sum))[simTime$index.useyr, -1], MARGIN=2, FUN=mean)
						resSDs[(nv+16):(nv+20)] <- apply(res2.yr[simTime$index.useyr, -1], MARGIN=2, FUN=sd)
						#Mean time to germinate in days
						resMeans[nv+21] <- mean((res3.yr <- aggregate(Germination_TimeToGerminate, by=list(year_ForEachUsedRYDay), FUN=mean, na.rm=TRUE))[simTime$index.useyr, -1], na.rm=TRUE)
						resSDs[nv+21] <- sd(res3.yr[simTime$index.useyr, -1], na.rm=TRUE)
						#Mean number of days per year of different types of mortalities
						resMeans[(nv+22):(nv+30)] <- apply(SeedlingMortality_CausesByYear, MARGIN=2, FUN=mean, na.rm=TRUE) #if value==NA, then no germinations that year
						resSDs[(nv+22):(nv+30)] <- apply(SeedlingMortality_CausesByYear, MARGIN=2, FUN=sd, na.rm=TRUE) #if value==NA, then no germinations that year
						
						nv <- nv+31
						
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
							write.csv(res.yr, file=file.path(dir.at, paste("Scenario", formatC(sc-1, width=2, format="d", flag="0"), "_", climate.conditions[sc], "_", i_labels, "_", colnames(param.species_regeneration)[sp], "_Regeneration.csv", sep="")))
							
							#Plot with data for every day
							pdf(file=file.path(dir.at, paste("Scenario", formatC(sc-1, width=2, format="d", flag="0"), "_", climate.conditions[sc], "_", i_labels, "_", colnames(param.species_regeneration)[sp], "_Regeneration.pdf", sep="")),
									width=max(4, 2*length(simTime$index.useyr)), height=4.5)
							op <- par(mar=c(1, 3, 0.1, 0.1), mgp=c(2, 0.5, 0), las=1)
							ylim <- c(-17.5, max(max(snow, na.rm=TRUE), max(Germination_TimeToGerminate, na.rm=TRUE)))
							p.cex <- max(0.5, min(1, exp(-0.01 * ylim[2]) + 0.5))
							xp <- 1:length(snow) + Doy_SeedDispersalStart-1
							plot(xp, snow, type="l", ylim=ylim, xlab="Year", ylab="SWE (mm), Time to germinate (days)", axes=FALSE)
							axis(1, pos=ylim[1], at=365*(1:(length(simTime$index.useyr))), labels=simTime$useyr)
							axis(2, pos=par("usr")[1], at=(temp <- axTicks(2))[temp>=0])
							lines(xp, Germination_TimeToGerminate, col="red", type="b", pch=19, cex=p.cex/5)
							points(xp, ifelse(SeedlingSurvival_1stSeason, 0, NA), col="green", pch=19)		
							x0.temp <- (temp <- data.frame(xp, ifelse(GerminationSuccess_Initiated, -7.5, NA)))[complete.cases(temp), ]
							x1.temp <- (temp <- data.frame(Germination_Emergence.doys + Doy_SeedDispersalStart-1, ifelse(GerminationSuccess_Initiated, -2.5, NA)))[complete.cases(temp), ]
							segments(x0=x0.temp[, 1], y0=x0.temp[, 2], x1=x1.temp[, 1], y1=x1.temp[, 2], col="blue")
							points(xp, ifelse(Germination_RestrictedByTimeToGerminate, -10, NA), col="black", pch=4, cex=p.cex)		
							points(xp, ifelse(!Germination_AtAboveTmin, -12.5, NA), col=gray(0.3), pch=4, cex=p.cex)		
							points(xp, ifelse(!Germination_AtMoreThanTopSWPmin, -15, NA), col=gray(0.7), pch=4, cex=p.cex)		
							mtext(i_labels)
							legend("topright", legend=c("SWE", "Time to germinate", "Seedling survival", "Emergence", "Too short favorable conditions", "Too cold", "Too dry"),
									bty="n", lty=c(1, 1, -1, 1, -1, -1, -1), pch=c(-1, -1, 19, -1, 4, 4, 4), col=c("black", "red", "green", "blue", "black", gray(0.3), gray(0.7)), merge=TRUE)
							par(op)
							dev.off()
						}
						
						#Prepare next species
						prev.Doy_SeedDispersalStart <- Doy_SeedDispersalStart
					}#end of species loop
				}

				#---Aggregation: done with options
				
				#temporaly save aggregate data					
				
				P_id <- ((i-1)*scenario_No+sc)
				
				resMeans[!is.finite(resMeans)] <- "NULL"
				resSDs[!is.finite(resSDs)] <- "NULL"
				SQL1 <- paste0("INSERT INTO \"aggregation_overall_mean\" VALUES (",paste0(P_id,",",paste0(resMeans[1:(nv-1)],collapse=","),sep=""),");", sep="")
				SQL2 <- paste0("INSERT INTO \"aggregation_overall_sd\" VALUES (",paste0(P_id,",",paste0(resSDs[1:(nv-1)],collapse=","),sep=""),");", sep="")
				if(length(SQL) == 0) {
					SQL <- paste(SQL1, SQL2, sep="\n")
				} else {
					SQL <- paste(SQL, SQL1, SQL2, sep="\n")
				}
			}
			
			#Daily Output
			if(any(simulation_timescales=="daily") && daily_no > 0){
				dailyList <- list()
				SQLc <- ""
				#aggregate for each response variable
				for (doi in 1:daily_no) {
					if(print.debug) print(paste("Aggregation of mean daily outputs:", doi))
					
					if(!continueAfterAbort | (continueAfterAbort & !isdone.dailyAggs[doi, sc])){
						#check to see if we are on SWA
						if(regexpr("SWA", output_aggregate_daily[doi]) > 0){
							agg.resp <- "SWA"
							index.SWPcrit <- -as.numeric(sub("kPa", "", sub("SWAatSWPcrit", "", output_aggregate_daily[doi])))/1000
						} else {
							agg.resp <- output_aggregate_daily[doi]
						}
						
						agg.analysis <- switch(EXPR=agg.resp, AET=1, Transpiration=2, EvaporationSoil=1, EvaporationSurface=1, EvaporationTotal=1, VWC=2, SWC=2, SWP=2, SWA=2, Snowpack=1, Rain=1, Snowfall=1, Snowmelt=1, SnowLoss=1, Infiltration=1, DeepDrainage=1, PET=1, TotalPrecipitation=1, TemperatureMin=1, TemperatureMax=1, SoilTemperature=2, Runoff=1)
						agg.no <- ifelse(agg.analysis == 1, 1, aggLs_no)
						
						res.dailyMean <- res.dailySD <- rep(NA, times=ifelse(agg.analysis == 1, 1, ifelse(AggLayer.daily, agg.no, SoilLayer_MaxNo)) * 366)
						
						scaler <- switch(EXPR=output_aggregate_daily[doi], SWP=1, VWC=1, TemperatureMin=1, TemperatureMax=1, SoilTemperature=1, 10) 	# SWP: -bar => MPa (but, since calculated via VWC, needs be same as VWC); VWC: # cm/cm -> m3/m3; default: cm => mm
						
						#read in data unless Exclude_ClimateAmbient
						if(!Exclude_ClimateAmbient) {
							if(agg.resp == "EvaporationTotal"){
								temp1 <- runData[[sc]][[sw_evsoil]][[sw_dy]]
								temp2 <- runData[[sc]][[sw_evapsurface]][[sw_dy]]
							} else {
								agg.file <- switch(EXPR=agg.resp,
										AET=sw_aet,
										Transpiration=sw_transp,
										EvaporationSoil=sw_evsoil,
										EvaporationSurface=sw_evapsurface,
										VWC=sw_vwc,
										SWC=sw_swc,
										SWP=sw_vwc,
										SWA=sw_swc,
										Snowpack=sw_snow,
										Rain=sw_precip,
										Snowfall=sw_precip,
										Snowmelt=sw_precip,
										SnowLoss=sw_precip,
										Infiltration=sw_inf_soil,
										DeepDrainage=sw_deepdrain,
										PET=sw_pet,
										TotalPrecipitation=sw_precip,
										TemperatureMin=sw_temp,
										TemperatureMax=sw_temp,
										SoilTemperature=sw_soiltemp,
										Runoff=sw_runoff)
								temp1 <- runData[[sc]][[agg.file]][[sw_dy]]
							}
							
							#extract data and aggregate into layers if requested
							agg.dat <- NULL
							if(agg.analysis == 1){ #no layers
								if( any(!is.na(match(agg.resp, c("AET", "EvaporationSurface", "Snowpack", "Rain", "Snowfall", "Snowmelt", "SnowLoss", "Infiltration", "DeepDrainage", "PET", "TotalPrecipitation", "TemperatureMin", "TemperatureMax","Runoff")))) ){
									agg.column <- switch(EXPR=agg.resp, AET=3, EvaporationSurface=3, Snowpack=3, Rain=4, Snowfall=5, Snowmelt=6, SnowLoss=7, Infiltration=3, DeepDrainage=3, PET=3, TotalPrecipitation=3, TemperatureMin=4, TemperatureMax=3,Runoff=3)
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
						}
						#temporary save daily data
						P_id <- ((i-1)*scenario_No+sc)
						
						#save(agg.analysis, aggLs_no, P_id, header, sc, agg.resp, res.dailyMean, res.dailySD, file=file.path(dir.out, paste(mpi.comm.rank(),"of",mpi.comm.size(),"_sc_",sc,"_doi_",doi,".r",sep="")))
						if(agg.analysis == 1){
							res.dailyMean[!is.finite(res.dailyMean)] <- "NULL"
							res.dailySD[!is.finite(res.dailySD)] <- "NULL"
							SQL1 <- paste0("INSERT INTO \"",paste0("aggregation_doy_", output_aggregate_daily[doi], "_Mean", sep=""),"\" VALUES (",paste0(P_id,",",paste0(res.dailyMean,collapse=",")),");", sep="")
							SQL2 <- paste0("INSERT INTO \"",paste0("aggregation_doy_", output_aggregate_daily[doi], "_SD", sep=""),"\" VALUES (",paste0(P_id,",",paste0(res.dailySD,collapse=",")),");", sep="")
							SQL <- paste(SQL, SQL1, SQL2, sep="\n")
						} else {
							#save(res.dailyMean,agg.no,header,header.names,P_id, res.dailySD,agg.analysis, aggLs_no,aggLs,agg.resp,layers_width,file=file.path(dir.out, "readThis.r"))
							res.dailyMean[!is.finite(res.dailyMean)] <- "NULL"
							res.dailySD[!is.finite(res.dailySD)] <- "NULL"
							SQL1 <- paste0("INSERT INTO \"",paste("aggregation_doy_", output_aggregate_daily[doi], "_Mean", sep=""),"\" VALUES", paste0("(",sapply(1:agg.no, FUN=function(x) {paste0(P_id,",", x,",",paste0(res.dailyMean[((x*366)-365):(x*366)],collapse=","))}), ")", sep="", collapse = ","), ";", sep="") 
							SQL2 <- paste0("INSERT INTO \"",paste("aggregation_doy_", output_aggregate_daily[doi], "_SD", sep=""),"\" VALUES", paste0("(",sapply(1:agg.no, FUN=function(x) {paste0(P_id,",", x,",",paste0(res.dailySD[((x*366)-365):(x*366)],collapse=","))}), ")", sep="", collapse = ","), ";", sep="")
							SQL <- paste(SQL, SQL1, SQL2, sep="\n")
						}
						
					}#end if continueAfterAbort
				}#doi loop
			}#end if daily output
			if(tasks$aggregate > 0 && nchar(SQL) > 0 && sc==1){
				write(SQL, dbTempFileCurrent, append=TRUE)
				#Clear SQL
				SQL <- character(0)
			}
		} #end loop through scenarios
		
	} #end if do aggregate
	
	if(tasks$aggregate > 0 && nchar(SQL) > 0){
		tasks$aggregate <- 2
		write(SQL, dbTempFile, append=TRUE)
	}
	
	
	if(all(unlist(tasks) != 0)){
		#ETA estimation
		dt <- difftime(Sys.time(), time.sys, units="secs")
		times <- read.csv(file=file.path(dir.out, timerfile), header=FALSE, colClasses=c("NULL", "numeric"))
		if(!be.quiet) print(paste(i, ":", i_labels, "done in", round(dt, 2), units(dt), ":", round(nrow(times)/runsN.total*100, 2), "% complete, ETA =", Sys.time()+ceiling((runsN.total-(nrow(times)-1))/workersN)*mean(unlist(c(times, dt)), na.rm=TRUE) ))	
		write.table(data.frame(i=i,dt=dt), file=file.path(dir.out, timerfile), append=TRUE, sep=",", dec=".", col.names=FALSE,row.names=FALSE)
	} else {
		print(paste(i, ":", i_labels, " unsuccessful:", paste(names(tasks), "=", tasks, collapse=", ")))	
	}
	
	return(1)	
} #end do_OneSite()

#------------------------

	work <- function() {
		# Note the use of the tag for sent messages:
		#     1=ready_for_task, 2=done_task, 3=exiting
		# Note the use of the tag for received messages:
		#     1=task, 2=done_tasks
	
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
				if(dataForRun$do_OneSite) result <- do_OneSite(i=dataForRun$i, i_labels=dataForRun$labels, i_SWRunInformation=dataForRun$SWRunInformation, i_sw_input_soillayers=dataForRun$sw_input_soillayers, i_sw_input_treatments=dataForRun$sw_input_treatments, i_sw_input_cloud=dataForRun$sw_input_cloud, i_sw_input_prod=dataForRun$sw_input_prod, i_sw_input_site=dataForRun$sw_input_site, i_sw_input_soils=dataForRun$sw_input_soils, i_sw_input_weather=dataForRun$sw_input_weather, i_sw_input_climscen=dataForRun$sw_input_climscen, i_sw_input_climscen_values=dataForRun$sw_input_climscen_values)
				# Send a results message back to the master
				#print(results)
				mpi.send.Robj(list(i=dataForRun$i,r=result),0,2)
			} else if (tag == 2) {
				done <- 1
			}
			# We'll just ignore any unknown messages
		}
		mpi.send.Robj(junk,0,3)
	}
}
#--------------------------------------------------------------------------------------------------#
#------------------------RUN RSOILWAT


if(actionWithSoilWat && runsN.todo > 0){
		
	swDataFromFiles <- sw_inputDataFromFiles(dir=dir.sw.in,files.in=swFilesIn) #This acts for the basis for all runs.
	#Used for weather from files
	filebasename <- basename(swFiles_WeatherPrefix(swDataFromFiles))
	#objects to export
	list.export <- c("Tmax_crit_C","Tmin_crit_C","name.OutputDB","getScenarioWeatherDataFromDatabase","getCurrentWeatherDataFromDatabase","GriddedDailyWeatherFromMaurer2002_NorthAmerica","ExtractGriddedDailyWeatherFromMaurer2002_NorthAmerica","climate.conditions","dir.sw.in.tr","dbWeatherDataFile","dir.ex.maurer2002","AggLayer.daily","Depth_TopLayers","Depth_FirstAggLayer.daily","Depth_SecondAggLayer.daily","Depth_ThirdAggLayer.daily","Depth_FourthAggLayer.daily","adjustLayersDepth", "getLayersWidth", "setLayerSequence", "sw_dailyC4_TempVar","sw_SiteClimate_Ambient","PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996", "AdjMonthlyBioMass","siteparamin","soilsin","weatherin","cloudin","prodin","estabin","tr_input_TranspCoeff_Code","transferExpDesignToInput","sw_input_experimentals","getStartYear","get.month","adjust.WindspeedHeight","circ.mean","circ.range","circ.sd","dir.create2","do_OneSite","endDoyAfterDuration","EstimateInitialSoilTemperatureForEachSoilLayer","get.LookupEvapCoeffFromTable","get.LookupSnowDensityFromTable","get.LookupTranspRegionsFromTable","max.duration","setAggSoilLayerForAggDailyResponses","simTiming","simTiming_ForEachUsedTimeUnit","startDoyOfDuration","SWPtoVWC","TranspCoeffByVegType","VWCtoSWP",
			"work", "do_OneSite", "accountNSHemispheres_veg","AggLayer.daily","be.quiet","bin.prcpfreeDurations","bin.prcpSizes","climate.conditions","continueAfterAbort","datafile.windspeedAtHeightAboveGround","DegreeDayBase","Depth_TopLayers","dir.out","dir.sw.runs","endyr","estabin","establishment.delay","establishment.duration","establishment.swp.surface","exec_c_prefix","filebasename.WeatherDataYear","germination.duration","germination.swp.surface","growing.season.threshold.tempC","makeInputForExperimentalDesign","ouput_aggregated_ts","output_aggregate_daily","parallel_backend","parallel_runs","print.debug","saveSoilWatInputOutput","season.end","season.start","shrub.fraction.limit","simstartyr","simulation_timescales","startyr","sw_aet","sw_deepdrain","sw_dy","sw_evapsurface","sw_evsoil","sw_hd","sw_inf_soil","sw_interception","sw_mo","sw_percolation","sw_pet","sw_precip","sw_runoff","sw_snow","sw_soiltemp","sw_swa","sw_swc","sw_swp","sw_temp","sw_transp","sw_vwc","sw_yr","sw.inputs","sw.outputs","swcsetupin","swFilesIn","swOutSetupIn","SWPcrit_MPa","yearsin","dbOverallColumns","aon","create_experimentals","create_treatments","daily_no","dir.out.temp","dirname.sw.runs.weather","do.GetClimateMeans","ExpInput_Seperator","lmax","no.species_regeneration","param.species_regeneration","pcalcs","runs","runsN.todo","runsN.total", "scenario_No","simTime","simTime_ForEachUsedTimeUnit_North","simTime_ForEachUsedTimeUnit_South","SoilLayer_MaxNo","SoilWat.windspeedAtHeightAboveGround","st_mo","sw_input_climscen_use","sw_input_climscen_values_use","sw_input_cloud_use","sw_input_experimentals_use","sw_input_prod_use","sw_input_site_use","sw_input_soils_use","sw_input_weather_use","swDataFromFiles","counter.digitsN","timerfile","tr_cloud","tr_files","tr_input_climPPT","tr_input_climTemp","tr_input_EvapCoeff","tr_input_shiftedPPT","tr_input_SnowD","tr_input_TranspCoeff","tr_input_TranspRegions","tr_prod","tr_site","tr_soil","tr_VegetationComposition","tr_weather","trowExperimentals","workersN")
	list.export <- ls()[ls() %in% list.export]
	#ETA calculation
	if(!be.quiet) print(paste("SWSF simulation runs:", runsN.todo, "out of", trow * ifelse(trowExperimentals==0, 1, trowExperimentals), " runs will be carried out on", workersN, "cores: started at", t1 <- Sys.time()))
	
	inputDataToSave <- list()
	
	if(parallel_runs && parallel_init){
		#call the simulations depending on parallel backend
		if(identical(parallel_backend, "mpi")) {
			workersN <- (mpi.comm.size() - 1)
			exportObjects(list.export)
			
			mpi.bcast.cmd(library(Rsoilwat,quietly = TRUE))
			mpi.bcast.cmd(library(circular,quietly = TRUE))
			mpi.bcast.cmd(library(SPEI,quietly = TRUE))
			mpi.bcast.cmd(library(RSQLite,quietly = TRUE))
			mpi.bcast.cmd(drv<-dbDriver("SQLite"))
			#mpi.bcast.cmd(con<-dbConnect(drv,dbWeatherDataFile))
			
			mpi.bcast.cmd(work())
	
			junk <- 0
			closed_slaves <- 0
			runs.completed <- 1
			#sTag <- c("Ready for task", "Done with Task", "Exiting")
			while(closed_slaves < workersN) {
tryCatch({
				complete <- mpi.recv.Robj(mpi.any.source(),mpi.any.tag())
				complete_info <- mpi.get.sourcetag()
				slave_id <- complete_info[1]
				tag <- complete_info[2]
				#print(paste("From:", slave_id, "tag:", tag, "Message:", complete))
				
				if (tag == 1) {
					temp <- Sys.time() - t.overall
					units(temp) <- "secs"
					temp <- as.double(temp)
					
					# slave is ready for a task. Give it the next task, or tell it tasks
					# are done if there are none.
					if ((runs.completed <= length(seq.todo)) & (temp < MaxDoOneSiteTime)) {
						# Send a task, and then remove it from the task list
						i_tr <- seq.tr[(seq.todo[runs.completed]-1) %% runs + 1]
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
						
						dataForRun <- list(do_OneSite=TRUE, i=(seq.todo[runs.completed]), labels=i_labels, SWRunInformation=i_SWRunInformation, sw_input_soillayers=i_sw_input_soillayers, sw_input_treatments=i_sw_input_treatments, sw_input_cloud=i_sw_input_cloud, sw_input_prod=i_sw_input_prod, sw_input_site=i_sw_input_site, sw_input_soils=i_sw_input_soils, sw_input_weather=i_sw_input_weather, sw_input_climscen=i_sw_input_climscen, sw_input_climscen_values=i_sw_input_climscen_values)
						mpi.send.Robj(dataForRun, slave_id, 1);
						print(paste("Slave:", slave_id, "Run:", (seq.todo[runs.completed]), "started at", Sys.time()))
						runs.completed <- runs.completed + 1
					} else {
						mpi.send.Robj(junk, slave_id, 2)
					}
				} else if (tag == 2) {
					# The message contains results. Do something with the results.
					# Store them in the data structure
					inputDataToSave[[complete$i]] <- complete$r
					#print(paste("Run: ", complete, "at", Sys.time()))
				} else if (tag == 3) {
					# A slave has closed down.
					closed_slaves <- closed_slaves + 1
					print(paste("Slave Closed:", slave_id))
				} else if (tag == 4) {
					#The slave had a problem with Soilwat record Slave number and the Run number.
					print("Problem with run")
					write.csv(x=data.frame(Slave=slave_id,Run=complete), file=file.path(dir.out, "ProblemRuns.csv"), append=TRUE,row.names<-FALSE,col.names=TRUE)
				}
}, interrupt=function(interrupt) {
	print("Ctrl-C caught bringing work to an end.")
	print(interrupt)
	MaxDoOneSiteTime <<- 0
})
			}
			mpi.bcast.cmd(rm(list=ls())) #do not remove 'ls(all=TRUE)' because there are important .XXX objects that are important for proper slave functioning!
			mpi.bcast.cmd(gc())
			print(runs.completed)
		}
		if(identical(parallel_backend, "snow")){
			snow::clusterEvalQ(cl, library(circular,quietly=TRUE)) 	#load any packages necessary for do_OneSite(): none as of July 24, 2012
			snow::clusterEvalQ(cl, library(SPEI,quietly=TRUE))
			snow::clusterEvalQ(cl, library(RSQLite,quietly=TRUE))
			snow::clusterEvalQ(cl, library(Rsoilwat,quietly=TRUE))

			snow::clusterExport(cl, list.export)
			snow::clusterEvalQ(cl, dbConnected <- FALSE)

			runs.completed <- foreach(i_sim=seq.todo, .combine="+", .inorder=FALSE) %dopar% {				
				i_tr <- seq.tr[(i_sim - 1) %% runs + 1]
				do_OneSite(i=i_sim, i_labels=labels[i_tr], i_SWRunInformation=SWRunInformation[i_tr, ], i_sw_input_soillayers=sw_input_soillayers[i_tr, ], i_sw_input_treatments=sw_input_treatments[i_tr, ], i_sw_input_cloud=sw_input_cloud[i_tr, ], i_sw_input_prod=sw_input_prod[i_tr, ], i_sw_input_site=sw_input_site[i_tr, ], i_sw_input_soils=sw_input_soils[i_tr, ], i_sw_input_weather=sw_input_weather[i_tr, ], i_sw_input_climscen=sw_input_climscen[i_tr, ], i_sw_input_climscen_values=sw_input_climscen_values[i_tr, ])
			}
			snow::clusterEvalQ(cl, rm(list=ls(all=TRUE)))
			snow::clusterEvalQ(cl, gc())
		}
		if(identical(parallel_backend, "multicore")){
			runs.completed <- foreach(i_sim=seq.todo, .combine="+", .inorder=FALSE, .noexport=list.noexport) %dopar% {
				i_tr <- seq.tr[(i_sim - 1) %% runs + 1]
				
				do_OneSite(i=i_sim, i_labels=labels[i_tr], i_SWRunInformation=SWRunInformation[i_tr, ], i_sw_input_soillayers=sw_input_soillayers[i_tr, ], i_sw_input_treatments=sw_input_treatments[i_tr, ], i_sw_input_cloud=sw_input_cloud[i_tr, ], i_sw_input_prod=sw_input_prod[i_tr, ], i_sw_input_site=sw_input_site[i_tr, ], i_sw_input_soils=sw_input_soils[i_tr, ], i_sw_input_weather=sw_input_weather[i_tr, ], i_sw_input_climscen=sw_input_climscen[i_tr, ], i_sw_input_climscen_values=sw_input_climscen_values[i_tr, ])
			}
		}
		
	} else { #call the simulations in seriel
		runs.completed <- 0
#		runs.completed <- foreach(i_sim=seq.todo, .combine="+", .inorder=FALSE, .noexport=list.noexport) %do% {
#			i_tr <- seq.tr[(i_sim - 1) %% runs + 1]
#			do_OneSite(i=i_sim, i_labels=labels[i_tr], i_SWRunInformation=SWRunInformation[i_tr, ], i_sw_input_soillayers=sw_input_soillayers[i_tr, ], i_sw_input_treatments=sw_input_treatments[i_tr, ], i_sw_input_cloud=sw_input_cloud[i_tr, ], i_sw_input_prod=sw_input_prod[i_tr, ], i_sw_input_site=sw_input_site[i_tr, ], i_sw_input_soils=sw_input_soils[i_tr, ], i_sw_input_weather=sw_input_weather[i_tr, ], i_sw_input_climscen=sw_input_climscen[i_tr, ], i_sw_input_climscen_values=sw_input_climscen_values[i_tr, ])
#		}
		#Best for debugging
		setwd(dir.prj)
		exeEnv <- new.env()
		for(n in list.export) assign(x=n,value=get(n,globalenv()), envir=exeEnv)
		
		for(i_sim in seq.todo) {
			i_tr <- seq.tr[(i_sim - 1) %% runs + 1]
			
			assign(x="i",value=i_sim,envir=exeEnv)
			assign(x="i_labels",value=labels[i_tr],envir=exeEnv)
			assign(x="i_SWRunInformation",value=SWRunInformation[i_tr, ],envir=exeEnv)
			assign(x="i_sw_input_soillayers",value=sw_input_soillayers[i_tr, ],envir=exeEnv)
			assign(x="i_sw_input_treatments",value=sw_input_treatments[i_tr, ],envir=exeEnv)
			assign(x="i_sw_input_cloud",value=sw_input_cloud[i_tr, ],envir=exeEnv)
			assign(x="i_sw_input_prod",value=sw_input_prod[i_tr, ],envir=exeEnv)
			assign(x="i_sw_input_site",value=sw_input_site[i_tr, ],envir=exeEnv)
			assign(x="i_sw_input_soils",value=sw_input_soils[i_tr, ],envir=exeEnv)
			assign(x="i_sw_input_weather",value=sw_input_weather[i_tr, ],envir=exeEnv)
			assign(x="i_sw_input_climscen",value=sw_input_climscen[i_tr, ],envir=exeEnv)
			assign(x="i_sw_input_climscen_values",value=sw_input_climscen_values[i_tr, ],envir=exeEnv)
			
			save(list=ls(exeEnv),file="test.Rdata", envir=exeEnv)
			rm(list=ls(all=TRUE))
			load("test.Rdata")
			
			do_OneSite(i=i, i_labels=i_labels, i_SWRunInformation=i_SWRunInformation, i_sw_input_soillayers=i_sw_input_soillayers,
							i_sw_input_treatments=i_sw_input_treatments, i_sw_input_cloud=i_sw_input_cloud, i_sw_input_prod=i_sw_input_prod, i_sw_input_site=i_sw_input_site, i_sw_input_soils=i_sw_input_soils,
							i_sw_input_weather=i_sw_input_weather, i_sw_input_climscen=i_sw_input_climscen, i_sw_input_climscen_values=i_sw_input_climscen_values)
			runs.completed <- runs.completed + do_OneSite(i=i_sim, i_labels=labels[i_tr], i_SWRunInformation=SWRunInformation[i_tr, ], i_sw_input_soillayers=sw_input_soillayers[i_tr, ], i_sw_input_treatments=sw_input_treatments[i_tr, ], i_sw_input_cloud=sw_input_cloud[i_tr, ], i_sw_input_prod=sw_input_prod[i_tr, ], i_sw_input_site=sw_input_site[i_tr, ], i_sw_input_soils=sw_input_soils[i_tr, ], i_sw_input_weather=sw_input_weather[i_tr, ], i_sw_input_climscen=sw_input_climscen[i_tr, ], i_sw_input_climscen_values=sw_input_climscen_values[i_tr, ])
		}
	}
	#save(inputDataToSave,file=file.path(dir.out,paste("swInputData_",head(seq.todo,n=1),"_",head(seq.todo,n=1)+runs.completed,".R",sep="")),compress=TRUE)
	if(!be.quiet) print(paste("SWSF simulation runs: completed with", runs.completed, "runs: ended after",  round(difftime(Sys.time(), t1, units="secs"), 2), "s"))
} else {
	runs.completed <- 0
}
#------------------------

#------------------------
if(any(actions=="concatenate")) {
	if(!be.quiet) print(paste("Inserting Data from Temp SQL files into Database", ": started at", t1 <- Sys.time()))
	settings <- c("PRAGMA cache_size = 400000;","PRAGMA synchronous = OFF;","PRAGMA journal_mode = OFF;","PRAGMA locking_mode = EXCLUSIVE;","PRAGMA count_changes = OFF;","PRAGMA temp_store = MEMORY;","PRAGMA auto_vacuum = NONE;")
	
	temp <- Sys.time() - t.overall
	units(temp) <- "secs"
	temp <- as.double(temp)
	if(temp <= (MinTimeConcat-36000) | !parallel_runs | !identical(parallel_backend,"mpi")) {#need at least 10 hours for anything useful
		library(RSQLite)
		#Connect to the Database
		drv <- dbDriver("SQLite")
		con <- dbConnect(drv, dbname = name.OutputDB)
		if(copyCurrentConditionsFromTempSQL) {
			file.copy(from=name.OutputDB, to=name.OutputDBCurrent, overwrite=TRUE)
			con2 <- dbConnect(drv, dbname = name.OutputDBCurrent)
			NumberTables <- length(dbListTables(con2)[!(dbListTables(con2) %in% headerTables)])
			##DROP ALL ROWS THAT ARE NOT CURRENT FROM HEADER##
			dbGetQuery(con2,"DELETE FROM runs WHERE scenario_id != 1;")
		}
		if(file.exists(file.path(dir.out.temp,concatFile))) {
			completedFiles <- basename(readLines(file.path(dir.out.temp,concatFile)))
		} else {
			completedFiles <- character(0)
		}
		theFileList <- list.files(path=dir.out.temp, pattern="SQL", full.names=FALSE, recursive=TRUE, include.dirs=FALSE, ignore.case=FALSE)
		if(any(theFileList %in% completedFiles)) {
			theFileList <- theFileList[-which(theFileList %in% completedFiles)]#remove any already inserted files from list
		}
		
		for(j in 1:length(theFileList)) {
			FAIL <- FALSE
			
			temp <- Sys.time() - t.overall
			units(temp) <- "secs"
			temp <- as.double(temp)
			if( temp > (MaxRunDurationTime-MaxConcatTime) ) {#figure need at least 8 minutes for big ones  (& parallel_runs & identical(parallel_backend,"mpi") not run in parallel
				break
			}
			if(print.debug) print(paste(j,": started at ",temp<-Sys.time(),sep=""))
			
			command<-paste(paste(settings,collapse="\n"),"BEGIN;",paste(".read ",file.path(dir.out.temp,theFileList[j]),sep=""),"COMMIT;",sep="\n")
			system(paste("echo ",shQuote(command)," | sqlite3 ", shQuote(name.OutputDB)))
			if(copyCurrentConditionsFromTempSQL && grepl("SQL_Current_Node", theFileList[j])) {
				system(paste("echo ",shQuote(command)," | sqlite3 ", shQuote(name.OutputDBCurrent)))
			}
			
			write(file.path(dir.out.temp, theFileList[j]), file=file.path(dir.out.temp,concatFile), append = TRUE)
			if(!FAIL && deleteTmpSQLFiles) try(file.remove(file.path(dir.out.temp, theFileList[j])), silent=TRUE)
			if(print.debug) {
				temp2<-Sys.time() - temp
				units(temp2) <- "secs"
				temp2 <- as.double(temp2)
				print(paste("    ended at ",Sys.time(),", after ",temp2," seconds.",sep=""))
			}
		}
		
		if(!be.quiet) print(paste("Database complete in :",  round(difftime(Sys.time(), t1, units="secs"), 2), "s"))
		
		if(copyCurrentConditionsFromDatabase & !copyCurrentConditionsFromTempSQL) {
			if(!be.quiet) print(paste("Database is copied and subset to ambient condition: start at ",  Sys.time()))
			#Get sql for tables and index
			resSQL<-dbSendQuery(con, "SELECT sql FROM sqlite_master WHERE type='table' ORDER BY name;")
			sqlTables <- fetch(resSQL,n=-1)
			sqlTables <- unlist(sqlTables)
			sqlTables <- sqlTables[-grep(pattern="sqlite_sequence",sqlTables)]
			dbClearResult(resSQL)
			resIndex<-dbSendQuery(con, "SELECT sql FROM sqlite_master WHERE type='view' ORDER BY name;")
			sqlView <- fetch(resIndex,n=-1)
			dbClearResult(resIndex)
			sqlView<-unlist(sqlView)
			sqlView <- sqlView[!is.na(sqlView)]
			Tables <- dbListTables(con)
			Tables <- Tables[-grep(pattern="sqlite_sequence",Tables)]
			
			con <- dbConnect(drv, name.OutputDBCurrent)
			for(i in 1:length(sqlTables)) {#Create the tables
				res<-dbSendQuery(con, sqlTables[i])
				dbClearResult(res)
			}
			dbGetQuery(con, sqlView)
			
			con <- dbConnect(drv, dbname = name.OutputDB)
			#Get Tables minus ones we do not want
			Tables <- dbListTables(con)
			Tables <- Tables[-grep(pattern="sqlite_sequence",Tables)]
			Tables <- Tables[-(which(Tables %in% headerTables))]
			
			writeLines(text=paste(".mode insert ", Tables, "\n.out ", Tables,".sql\nSELECT * FROM ",Tables," WHERE P_id IN (SELECT P_id FROM runs WHERE scenario_id = 1 ORDER BY P_id);",sep=""),con="dump.txt")
			lines <- c("PRAGMA cache_size = 400000;","PRAGMA synchronous = OFF;","PRAGMA journal_mode = OFF;","PRAGMA locking_mode = EXCLUSIVE;","PRAGMA count_changes = OFF;","PRAGMA temp_store = MEMORY;","PRAGMA auto_vacuum = NONE;")
			writeLines(text=c(lines,paste(".read ",Tables,".sql",sep="")),con="insert.txt")
			
			system(paste("cat dump.txt | sqlite3 ", shQuote(name.OutputDB)))
			system(paste("cat insert.txt | sqlite3 ", shQuote(name.OutputDBCurrent)))
			
			unlink(paste(Tables,".sql",sep=""))
			
			Tables <- dbListTables(con)
			Tables <- Tables[-grep(pattern="sqlite_sequence",Tables)]
			Tables <- Tables[(which(Tables %in% headerTables[-1]))]
			
			writeLines(text=paste(".mode insert ", Tables, "\n.out ", Tables,".sql\nSELECT * FROM ",Tables,";",sep=""),con="dump.txt")
			lines <- c("PRAGMA cache_size = 400000;","PRAGMA synchronous = OFF;","PRAGMA journal_mode = OFF;","PRAGMA locking_mode = EXCLUSIVE;","PRAGMA count_changes = OFF;","PRAGMA temp_store = MEMORY;","PRAGMA auto_vacuum = NONE;")
			writeLines(text=c(lines,paste(".read ",Tables,".sql",sep="")),con="insert.txt")
			
			system(paste("cat dump.txt | sqlite3 ", shQuote(name.OutputDB)))
			system(paste("cat insert.txt | sqlite3 ", shQuote(name.OutputDBCurrent)))
			
			unlink(paste(Tables,".sql",sep=""))
			unlink(c("dump.txt","insert.txt"))
		}
	} else {
		print(paste("Need more than ", MinTimeConcat," seconds to put SQL in Database.",sep=""))
	}
}


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
		
		target<-paste(rep(t$Label, each=7), c(climate.ambient, as.character(unlist(t[1, 13:18]))), sep="_")
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
#------------------------ENSEMBLE GENERATION
t.ensembles <- Sys.time()	#timing of ensemble calculation

if(do.ensembles && all.complete && (actionWithSoilWat && runs.completed == runsN.todo || actionWithSWSFOutput && !actionWithSoilWat) ){
	
	if(!be.quiet) print(paste("SWSF calculates ensembles: started at", t.ensembles))
	
	#save(ensembles.maker,ensemble.levels, ensemble.families, file=file.path(dir.out, "ensembleObjects.r"))
	
	calc.ensembles <- function(dat, elevels){ #dat must be three-dimensional object with dims=(runs, outputs, scenarios); runs and/or scenarios can be 1 or larger
		doRanks <- function(x){
			temp <- sort.int(x, na.last=NA, index.return=TRUE)
			return(c(temp$x[elevels], temp$ix[elevels]))
		}
		
		res <- NULL
		col.names <- colnames(dat)
		if(dim(dat)[3] == 1){ #only one scenario; i.e., all levels are identical to the scenario
			temp <- array(data=rep(unlist(dat), each=3), dim=c(length(elevels), dim(dat)[1], dim(dat)[2]))
			res <- array(data=1, dim=c(2*length(elevels), dim(dat)[1], dim(dat)[2]))
			res[1:length(elevels),,] <- temp
		} else {
			if(dim(dat)[1] > 1){
				res <- apply(dat[,,], MARGIN = c(1,2), doRanks)
			} else { #only one run=site
				res <- apply(dat[,,], MARGIN = 1, doRanks)
				res <- array(data=res, dim=c(2*length(elevels), 1, dim(dat)[2]))
			}
		}
		#returned object: array with 3 dims: 1. dim = 1:length(elevels) are the ensembles at the ensemble.levels; the second set of rows are the ranked GCMs; 2. dim = runs/sites; 3. dim = aggregated variables
		dimnames(res) <- list(NULL, NULL, col.names)
		return(res)
	}
	
	collect_EnsembleFromScenarios <- function(Table){
		drv <- dbDriver("SQLite")
		con <- dbConnect(drv, dbname = name.OutputDB)
		#########TIMING#########
		TableTimeStop <- Sys.time() - t.overall
		units(TableTimeStop) <- "secs"
		TableTimeStop <- as.double(TableTimeStop)
		print(paste("Table: ",Table,": started at ",TableTime<-Sys.time(),sep=""))
		
		#########FUNCTIONS######
		doWrite <- function(dat, headerInfo, elevel, outfile){
			#add info to 
			name <- ensemble.family
			if(is.vector(dat)) {
				dat <- cbind(headerInfo, t(dat))
			} else {
				dat <- cbind(headerInfo, dat)
			}
			#dbGetPreparedQuery(conEnsembleDB, paste("INSERT INTO ",outfile," VALUES(",paste(paste(":",colnames(dat),sep=""),collapse=", "),");",sep=""), bind.data=dat)
			#written<-1
			written <- dbWriteTable(conEnsembleDB, name=outfile, dat, row.names=FALSE,append=TRUE)#
			if(written)
				return(1)
			else
				return(0)
		}
		read.scenarios <- function(Table, start, stop, ensemble.family, export.header=TRUE){
			#Read first file
			columns<-dbListFields(con,Table)[-1]
			if(Layers<-any(temp<-grepl(pattern = "Soil_Layer",x=columns))) columns<-columns[-temp]
			columns<-paste("\"",columns,"\"", sep="",collapse = ", ")
			sqlString <- paste("SELECT ",Table,".P_id AS P_id, header.Scenario AS Scenario, ",columns," FROM ", Table, " INNER JOIN header ON ",Table,".P_id=header.P_id WHERE header.P_id BETWEEN ",start," AND ",stop," AND header.Scenario LIKE '%", tolower(ensemble.family), "%'", " ORDER BY P_id;", sep="")
			res <- dbSendQuery(con, sqlString)
			dataScen.Mean <- fetch(res, n=-1) #dataToQuantilize get the data from the query n=-1 to get all rows
			dbClearResult(res)
			
			columnCutoff <- match("Scenario", colnames(dataScen.Mean))
			if(export.header) {
				sqlString <- paste("SELECT ", Table,".P_id AS P_id ",if(Layers) ", Soil_Layer ","FROM ",Table,",header WHERE ",Table,".P_id=header.P_id AND header.P_id BETWEEN ",start," AND ",stop," AND header.Scenario = 'Current' ORDER BY P_id;",sep="")
				res <- dbSendQuery(con, sqlString)
				headerInfo <- fetch(res, n=-1) #dataToQuantilize get the data from the query n=-1 to get all rows
				dbClearResult(res)
			}
			col.names <- colnames(dataScen.Mean[,-(1:columnCutoff)])
			#We have all the scenarios in the family. We need to get unique scenario names and group them by that
			data.temp <- lapply(unique(dataScen.Mean$Scenario), function (x) dataScen.Mean[dataScen.Mean$Scenario == x,-(1:columnCutoff)])
			data.temp <- array(data=unlist(data.temp), dim=c(nrow(data.temp[[1]]), ncol(data.temp[[1]]), length(data.temp)) )
			colnames(data.temp) <- col.names
			class(data.temp) <- "numeric"
			
			if(export.header) {
				return(list(headerInfo=headerInfo, data.scen=data.temp))
			} else {
				return(list(data.scen=data.temp))
			}
		}
		
		if(!(TableTimeStop > (MaxRunDurationTime-1*60)) | !parallel_runs | !identical(parallel_backend,"mpi")) {#figure need at least 3 hours for big ones
			tfile <- file.path(dir.out, paste("dbEnsemble_",sub(pattern="_Mean", replacement="", Table, ignore.case=TRUE),".sqlite3",sep=""))
			conEnsembleDB <- dbConnect(drv, dbname=tfile)
			
			nfiles <- 0
			#Grab x rows at a time
			SQL <- paste("SELECT MAX(P_id) FROM ",Table,";",sep="")
			maxP_id <- as.integer(dbGetQuery(con,SQL))
			maxRun_id <- (maxP_id/scenario_No)
			
			for(j in 1:length(ensemble.families)) {
				EnsembleTimeStop <- Sys.time() - t.overall
				units(EnsembleTimeStop) <- "secs"
				EnsembleTimeStop <- as.double(EnsembleTimeStop)
				if((EnsembleTimeStop > (MaxRunDurationTime-1*60)) & parallel_runs & identical(parallel_backend,"mpi")) {#figure need at least 4 hours for a ensemble
					break
				}
				print(paste("Table: ",Table,", Ensemble: ",ensemble.families[j]," started at ",EnsembleTime <- Sys.time(),sep=""))
				
				ensemble.family=ensemble.families[j]
				#########################
				for(i in seq(1,maxRun_id,ensembleCollectSize)) {
					start <- (i-1)*scenario_No+1
					stop <- (min(i+ensembleCollectSize-1,maxRun_id)-1)*scenario_No+scenario_No
					dataScen.Mean <- read.scenarios(Table=Table,start=start,stop=stop, ensemble.family=ensemble.family, export.header=TRUE)
					Table <- sub(pattern="Mean", replacement="SD", Table)
					dataScen.SD <- read.scenarios(Table=Table,start=start,stop=stop, ensemble.family=ensemble.family, export.header=FALSE)			
					Table <- sub(pattern="SD", replacement="Mean", Table)
					#get ensembles for non-SD file
					dataEns.Mean <- calc.ensembles(dat=dataScen.Mean$data.scen, elevels=ensemble.levels)
					#Lookup SD values from scenarios based on ranks determined from taking ensembles of the means
					if(length(dim(dataEns.Mean[(length(ensemble.levels) + 1):(2*length(ensemble.levels)),,])) == 2) {
						lookup <- aperm(dataEns.Mean[(length(ensemble.levels) + 1):(2*length(ensemble.levels)),,], perm=c(2,1))
						make <- array(c(lookup, dataScen.SD$data.scen), dim=c(nrow(lookup), length(ensemble.levels) + dim(dataScen.SD$data.scen)[3]))
						dataEns.SD <- apply(make, MARGIN=1, FUN=function(lookANDscen) lookANDscen[(length(ensemble.levels)+1):dim(make)[2]][lookANDscen[1:length(ensemble.levels)]])
						dimnames(dataEns.SD)[2] <- dimnames(dataScen.SD$data.scen)[2]
					} else {
						lookup <- aperm(dataEns.Mean[(length(ensemble.levels) + 1):(2*length(ensemble.levels)),,], perm=c(2,3,1))
						make <- array(c(lookup, dataScen.SD$data.scen), dim=c(nrow(lookup), ncol(lookup), length(ensemble.levels) + dim(dataScen.SD$data.scen)[3]))
						dataEns.SD <- apply(make, MARGIN=c(1,2), FUN=function(lookANDscen) lookANDscen[(length(ensemble.levels)+1):dim(make)[3]][lookANDscen[1:length(ensemble.levels)]])
						dimnames(dataEns.SD)[3] <- dimnames(dataScen.SD$data.scen)[2]
					}
					#write ensemble files
					ntemp <- 0
					for(k in 1:length(ensemble.levels)){
						outputs <- paste(ensemble.family,"_rank_",formatC(ensemble.levels[k], width=2, flag="0"),"_",c("means","sds","scenarioranks"),sep="")
						ntemp <- ntemp + doWrite(dat=dataEns.Mean[k,,], headerInfo=dataScen.Mean$headerInfo, elevel=ensemble.levels[k], outfile=outputs[1])
						if(length(dim(dataEns.Mean[(length(ensemble.levels) + 1):(2*length(ensemble.levels)),,])) == 2) {
							ntemp <- ntemp + doWrite(dat=dataEns.SD[k,], headerInfo=dataScen.Mean$headerInfo, elevel=ensemble.levels[k], outfile=outputs[2])
						} else {
							ntemp <- ntemp + doWrite(dat=dataEns.SD[k,,], headerInfo=dataScen.Mean$headerInfo, elevel=ensemble.levels[k], outfile=outputs[2])
						}
						if(save.scenario.ranks) ntemp <- ntemp + doWrite(dat=dataEns.Mean[length(ensemble.levels) + k,,], headerInfo=dataScen.Mean$headerInfo, elevel=ensemble.levels[k], outfile=outputs[3])
					}
					if(i == 1) nfiles <- nfiles + ntemp
					print(paste("          ",i,":",min(i+ensembleCollectSize-1,maxRun_id)," of ",maxRun_id," done.",sep=""))
				}
				#########################
				temp2<-Sys.time() - EnsembleTime
				units(temp2) <- "secs"
				temp2 <- as.double(temp2)
				print(paste("Table: ", Table, ", Ensemble: ", ensemble.families[j], " ended at ",Sys.time(),", after ", round(temp2)," s.",sep=""))
			}
		}
		temp2<-Sys.time() - TableTime
		units(temp2) <- "secs"
		temp2 <- as.double(temp2)
		print(paste("Table: ", Table, " ended at ",Sys.time(),", after ",round(temp2)," s.",sep=""))
		
		return(nfiles)
	}
	
	library(RSQLite,quietly = TRUE)
	drv <- dbDriver("SQLite")
	con <- dbConnect(drv, dbname = name.OutputDB)
	
	Tables <- dbListTables(con) #get a list of tables
	Tables <- Tables[-which(Tables %in% headerTables)]
	Tables <- Tables[-grep(pattern="_sd", Tables, ignore.case = T)]
	
	if(parallel_runs && parallel_init){
		#call the simulations depending on parallel backend
		list.export <- c("ensembleCollectSize","Tables","save.scenario.ranks","ensemble.levels","calc.ensembles","scenario_No","MaxRunDurationTime", "collect_EnsembleFromScenarios","dir.out","ensemble.families","t.overall","parallel_runs","parallel_backend","name.OutputDB")
		if(identical(parallel_backend, "mpi")) {
			workersN <- (mpi.comm.size() - 1)
			exportObjects(list.export)
			
			mpi.bcast.cmd(library(RSQLite,quietly = TRUE))
			mpi.bcast.cmd(drv<-dbDriver("SQLite"))
			
			ensembles.completed <- mpi.applyLB(x=Tables, fun=collect_EnsembleFromScenarios)
			ensembles.completed <- sum(unlist(ensembles.completed))
		} else if(identical(parallel_backend, "snow")) {
			snow::clusterExport(cl, list.export)
			snow::clusterEvalQ(cl, library(RSQLite,quietly = TRUE))
			snow::clusterEvalQ(cl, drv<-dbDriver("SQLite"))
			
			ensembles.completed <- foreach(i = 1:length(Tables), .combine="+", .inorder=FALSE) %dopar% collect_EnsembleFromScenarios(Tables[i])
		}
	} else {
		ensembles.completed <- foreach(table=Tables, .combine="+", .inorder=FALSE) %do% {
			collect_EnsembleFromScenarios(table)
		}
	}

	if(ensembles.completed != (temp <- length(Tables)*ifelse(save.scenario.ranks, 3, 2)*length(ensemble.families)*length(ensemble.levels))) print("SWSF calculates ensembles: something went wrong with ensemble output: ensembles.completed = ", ensembles.completed, " instead of ", temp,".")
}


#timing of ensemble calculation
delta.ensembles <- difftime(Sys.time(), t.ensembles, units="secs")
if(!be.quiet && do.ensembles) print(paste("SWSF calculates ensembles: ended after", round(delta.ensembles, 2), "s"))


#--------------------------------------------------------------------------------------------------#
#------------------------OVERALL TIMING
delta.overall <- difftime(Sys.time(), t.overall, units="secs")
if(!be.quiet) print(paste("SWSF: ended after", round(delta.overall, 2), "s"))

write.timer <- function(label, time_sec="", number=""){ write.table(t(c(label, time_sec, number)), file=file.path(dir.out, timerfile2), append=TRUE, sep=",", dec=".", col.names=FALSE, row.names=FALSE) }

write.timer("Time_Total", time_sec=delta.overall)
write.timer("Time_Check", time_sec=delta.check)
write.timer("Time_Ensembles", time_sec=delta.ensembles)

if(actionWithSoilWat){
	times <- as.numeric(unlist(read.csv(file=file.path(dir.out, timerfile), header=FALSE, colClasses=c("NULL", "numeric"), skip=1)))
	write.timer("Time_OneRun_Mean", time_sec=mean(times))
	write.timer("Time_OneRun_SD", time_sec=sd(times))
	write.timer("Time_OneRun_Median", time_sec=median(times))
	write.timer("Time_OneRun_Min", time_sec=min(times))
	write.timer("Time_OneRun_Max", time_sec=max(times))
}

write.timer("N_cores", number=workersN)
write.timer("N_Runs", number=runs.completed)
write.timer("N_SWruns", number=runs.completed * scenario_No)
write.timer("N_EnsembleFiles", number=ifelse(exists("ensembles.completed"), ensembles.completed, 0))


if(!be.quiet) print(paste("SWSF: ended with actions =", paste(actions, collapse=", "), "at", Sys.time()))


#--------------------------------------------------------------------------------------------------#
#------------------------CODE CLEANUP

options(ow)	#sets the warning option to its previous value

if(parallel_runs && parallel_init){
	if(identical(parallel_backend, "mpi")) {	#clean up mpi slaves
		mpi.close.Rslaves(dellog=FALSE)
		mpi.exit()
	}
	if(identical(parallel_backend, "snow")){
		snow::stopCluster(cl)	#clean up snow cluster
	}
}


#rm(list=ls(all=TRUE))	#optional

#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#


