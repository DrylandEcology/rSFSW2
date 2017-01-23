#--------------------------------------------------------------------------------------------------#
#------------------------PREPARE SOILWAT SIMULATIONS

if (!be.quiet)
  print(paste("SWSF is executed for:", sQuote(basename(dir.prj)), "and started at",
    Sys.time()))

#------
actionWithSoilWat <- any(actions == "create") || any(actions == "execute") ||
  any(actions == "aggregate")
actionWithSWSFOutput <- any(actions == "concatenate") || any(actions == "ensemble")

#--order output_aggregate_daily--#
daily_no <- length(output_aggregate_daily)
if (daily_no > 0)
  output_aggregate_daily <- sort(output_aggregate_daily)

#------
ow_prev <- set_options_warn_error(debug.warn.level, debug.dump.objects, dir.prj)


#create simulation directory structure
dir.sw.in <- normalizePath(dir.sw.in)
if (makeInputForExperimentalDesign)
  dir.out.experimentalInput <- file.path(dir.out, "Experimentals_Input_Data")
dir.out.temp <- file.path(dir.out, "temp")
dir.create2(dir.out, showWarnings = FALSE, recursive = TRUE)
dir.create2(dir.big, showWarnings = FALSE, recursive = TRUE)
if (saveRsoilwatInput || saveRsoilwatOutput)
  dir.create2(dir.sw.runs, showWarnings = FALSE, recursive = TRUE)
dir.create2(dir.out.temp, showWarnings = FALSE, recursive = TRUE)
if (makeInputForExperimentalDesign)
  dir.create2(dir.out.experimentalInput, showWarnings = FALSE, recursive = TRUE)
dirname.sw.runs.weather <- "WeatherData"

#timing: output for overall timing information
temp <- "Timing_Simulation.csv"
timerfile2 <- file.path(dir.out, temp)
init_timer(timerfile2)

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

if (!require(Rsoilwat31, quietly = TRUE) || (require(Rsoilwat31, quietly = TRUE) &&
  packageVersion("Rsoilwat31") < minVersionRsoilwat)) {

  print("Going to try to install Rsoilwat library")

  temp <- getwd()
  setwd(dir.in)
  system2(command = "git", args = "clone -b master --single-branch --recursive https://github.com/Burke-Lauenroth-Lab/Rsoilwat.git Rsoilwat")
  tools::Rcmd(args = paste("INSTALL Rsoilwat"))
  setwd(temp)

  stopifnot(require(Rsoilwat31) && packageVersion("Rsoilwat31") >= minVersionRsoilwat)
}

if(!require(circular, quietly=TRUE)) {
  # loads via a namespace: boot
	tryCatch(install.packages("circular",repos=url.Rrepos,lib=dir.libraries), warning=function(w) { print(w); print("circular failed to install"); stop("Stopping") })
	stopifnot(require(circular, quietly=TRUE))
}
if(!require(SPEI, quietly=TRUE)) {
  # attaches: lmomco; loads via a namespace: MASS, Lmoments, goftest
	tryCatch(install.packages("SPEI",repos=url.Rrepos,lib=dir.libraries), warning=function(w) { print(w); print("SPEI failed to install"); stop("Stopping") })
	stopifnot(require(SPEI, quietly=TRUE))
}
if(!require(RSQLite,quietly = TRUE)) {
  # loads via a namespace: DBI, memoise, Rcpp, digest
	tryCatch(install.packages("RSQLite",repos=url.Rrepos,lib=dir.libraries), warning=function(w) { print(w); print("RSQLite failed to install"); stop("Stopping") })
	stopifnot(require(RSQLite, quietly = TRUE))
}

if (parallel_runs && identical(parallel_backend, "mpi")) {
  if (!require(Rmpi, quietly = TRUE)) {
    # loads via a namespace: parallel
    print(paste("'Rmpi' requires a MPI backend, e.g., OpenMPI is available from",
      shQuote("https://www.open-mpi.org/software/ompi/"), "with install instructions at",
      shQuote("https://www.open-mpi.org/faq/?category=building#easy-build")))
    print(paste("If no MPI is available, installation of 'Rmpi' will fail and may print",
      "the error message: 'Cannot find mpi.h header file'"))
    tryCatch(install.packages("Rmpi", repos = url.Rrepos, lib = dir.libraries),
      warning = function(w) {
        print(w)
        print("Rmpi failed to install")
        stop("Stopping")
    })
    stopifnot(require(Rmpi, quietly = TRUE))
  }

} else if(parallel_runs && identical(parallel_backend, "cluster")) {
  stopifnot(require("parallel")) # parallel is a base-package since R v2.14.0

} else if(parallel_runs) {
  stop("Parallel backend ", shQuote(parallel_backend), " is not available to SWSF")
}

#if(print.debug) trace(what=circular:::SdCircularRad, tracer=quote({print(x); print(sys.calls()[[6]]); print(paste(rbar, circsd))}), at=4)

#------prepare output
temp <- matrix(output_aggregates, ncol = 2, nrow = length(output_aggregates) / 2,
  byrow = TRUE)
aon <- lapply(temp[, 2], function(x) as.logical(as.numeric(x)))
names(aon) <- temp[, 1]

#------import data
if (!be.quiet)
  print(paste("SWSF reads input data: started at", t1 <- Sys.time()))

# Read data from files
do_check_include <- FALSE
fpreprocin <- file.path(dir.in, datafile.SWRWinputs_preprocessed)

if (usePreProcessedInput && file.exists(fpreprocin)) {
  # This however annihilates all objects in globalenv() with the same names !
  load(file = fpreprocin, envir = globalenv())

} else {
  fmaster <- file.path(dir.in, datafile.SWRunInformation)
  SWRunInformation <- tryCatch(swsf_read_csv(fmaster), error = print)
  stopifnot(sapply(required_colnames_SWRunInformation(),
      function(x) x %in% names(SWRunInformation)),		# required columns
    all(SWRunInformation$site_id == seq_len(nrow(SWRunInformation))),	# consecutive site_id
    !grepl("[[:space:]]", SWRunInformation$Label),	# no space-characters in label
    !grepl("[[:space:]]", SWRunInformation$WeatherFolder)	# no space-characters in weather-data names
  )
  include_YN <- as.logical(SWRunInformation$Include_YN)
  nrowsClasses <- max(dim(SWRunInformation)[1], 25L, na.rm = TRUE)

  fslayers <- file.path(dir.in, datafile.soillayers)
  sw_input_soillayers <- tryCatch(swsf_read_csv(fslayers, nrowsClasses = nrowsClasses),
    error = print)

  temp <- tryCatch(swsf_read_inputfile(file.path(dir.in, datafile.treatments),
    nrowsClasses = nrowsClasses), error = print)
  sw_input_treatments_use <- temp[["use"]]
  sw_input_treatments <- temp[["data"]]
  stopifnot(
    !grepl("[[:space:]]", sw_input_treatments$LookupWeatherFolder)	# no space-characters in weather-data names
  )

  temp <- tryCatch(swsf_read_inputfile(file.path(dir.in, datafile.Experimentals),
    nrowsClasses = nrowsClasses), error = print)
  sw_input_experimentals_use <- temp[["use"]]
  sw_input_experimentals <- temp[["data"]]
  create_experimentals <- names(sw_input_experimentals_use[sw_input_experimentals_use])
  stopifnot(
    !grepl("[[:space:]]", sw_input_experimentals$LookupWeatherFolder)	# no space-characters in weather-data names
  )

  #update treatment specifications based on experimental design
  create_treatments <- union(names(sw_input_treatments_use)[sw_input_treatments_use],
    create_experimentals)

  if (dim(SWRunInformation)[2] < 2)
    stop("SWRunInformation might be tab separated instead of comma.")
  if (dim(sw_input_soillayers)[2] < 2)
    stop("SoilLayers might be tab separated instead of comma.")
  if (dim(sw_input_treatments)[2] < 2)
    stop("Treatments might be tab separated instead of comma.")
  if (dim(sw_input_experimentals)[2] < 2)
    stop("Experimentals might be tab separated instead of comma.")

  fcloud <- file.path(dir.sw.dat, datafile.cloud)
  temp <- tryCatch(swsf_read_inputfile(fcloud, nrowsClasses = nrowsClasses), error = print)
  sw_input_cloud_use <- temp[["use"]]
  sw_input_cloud <- temp[["data"]]

  temp <- tryCatch(swsf_read_inputfile(file.path(dir.sw.dat, datafile.prod),
    nrowsClasses = nrowsClasses), error = print)
  sw_input_prod <- temp[["data"]]
  sw_input_prod_use <- temp[["use"]]
  sw_input_prod_use <- sw_input_prod_use | names(sw_input_prod_use) %in% create_experimentals	#update specifications based on experimental design

  temp <- tryCatch(swsf_read_inputfile(file.path(dir.sw.dat, datafile.siteparam),
    nrowsClasses = nrowsClasses), error = print)
  sw_input_site <- temp[["data"]]
  sw_input_site_use <- temp[["use"]]
  sw_input_site_use <- sw_input_site_use | names(sw_input_site_use) %in% create_experimentals	#update specifications based on experimental design

  fsoils <- file.path(dir.sw.dat, datafile.soils)
  temp <- tryCatch(swsf_read_inputfile(fsoils, nrowsClasses = nrowsClasses), error = print)
  sw_input_soils <- temp[["data"]]
  sw_input_soils_use <- temp[["use"]]
  sw_input_soils_use <- sw_input_soils_use | names(sw_input_soils_use) %in% create_experimentals	#update specifications based on experimental design

  temp <- tryCatch(swsf_read_inputfile(file.path(dir.sw.dat, datafile.weathersetup),
    nrowsClasses = nrowsClasses), error = print)
  sw_input_weather_use <- temp[["use"]]
  sw_input_weather <- temp[["data"]]

  fclimscen <- file.path(dir.sw.dat, datafile.climatescenarios)
  temp <- tryCatch(swsf_read_inputfile(fclimscen, nrowsClasses = nrowsClasses),
    error = print)
  sw_input_climscen_use <- temp[["use"]]
  sw_input_climscen <- temp[["data"]]

  fclimscen_values <- file.path(dir.sw.dat, datafile.climatescenarios_values)
  temp <- tryCatch(swsf_read_inputfile(fclimscen_values, nrowsClasses = nrowsClasses),
    error = print)
  sw_input_climscen_values_use <- temp[["use"]]
  sw_input_climscen_values <- temp[["data"]]

  if (dim(sw_input_cloud)[2] == 1)
    stop("Cloud datafile might be tab separated instead of comma.")
  if (dim(sw_input_prod)[2] == 1)
    stop("Prod datafile might be tab separated instead of comma.")
  if (dim(sw_input_site)[2] == 1)
    stop("Site datafile might be tab separated instead of comma.")
  if (dim(sw_input_soils)[2] == 1)
    stop("Soils datafile might be tab separated instead of comma.")
  if (dim(sw_input_weather)[2] == 1)
    stop("Weather datafile might be tab separated instead of comma.")
  if (dim(sw_input_climscen)[2] == 1)
    stop("Climate Use datafile datafile might be tab separated instead of comma.")
  if (dim(sw_input_climscen_values)[2] == 1)
    stop("Climate Values datafile datafile might be tab separated instead of comma.")

  #Create a list of possible treatment files with data.
  tr_files <- tr_prod <- tr_site <- tr_soil <- tr_weather <- tr_cloud <- list()
  tr_input_climPPT <- tr_input_climTemp <- tr_input_shiftedPPT <- tr_input_EvapCoeff <- tr_input_TranspCoeff_Code <- tr_input_TranspCoeff <- tr_input_TranspRegions <- tr_input_SnowD <- tr_VegetationComposition <- list()

	if(any(create_treatments=="sw"))
		print("SW treatment is not used because library Rsoilwat only uses one version of soilwat. Sorry")
	if(any(create_treatments=="filesin")) {
		temp<-list.files(path=file.path(dir.sw.in.tr, "filesin"),pattern="in",include.dirs=FALSE,recursive=TRUE,full.names=TRUE)
		tr_files[basename(temp)] <-unlist(lapply(temp,FUN=function(x) return(swReadLines(swClear(new("swFiles")),x))))
	}
	if(any(create_treatments=="prodin")) {
		temp<-list.files(path=file.path(dir.sw.in.tr, "prodin"),pattern="in",include.dirs=FALSE,recursive=TRUE,full.names=TRUE)
		tr_prod[basename(temp)] <-unlist(lapply(temp,FUN=function(x) return(swReadLines(swClear(new("swProd")),x))))
	}
	if(any(create_treatments=="siteparamin")) {
		temp<-list.files(path=file.path(dir.sw.in.tr, "siteparamin"),pattern="in",include.dirs=FALSE,recursive=TRUE,full.names=TRUE)
		tr_site[basename(temp)] <-unlist(lapply(temp,FUN=function(x) return(swReadLines(swClear(new("swSite")),x))))
	}
	if(any(create_treatments=="soilsin")) {
		temp<-list.files(path=file.path(dir.sw.in.tr, "soilsin"),pattern="in",include.dirs=FALSE,recursive=TRUE,full.names=TRUE)
		tr_soil[basename(temp)] <-unlist(lapply(temp,FUN=function(x) return(swReadLines(swClear(new("swSoils")),x))))
	}
	if(any(create_treatments=="weathersetupin")) {
		temp<-list.files(path=file.path(dir.sw.in.tr, "weatherin"),pattern="in",include.dirs=FALSE,recursive=TRUE,full.names=TRUE)
		tr_weather[basename(temp)] <-unlist(lapply(temp,FUN=function(x) return(swReadLines(swClear(new("swWeather")),x))))
	}
	if(any(create_treatments=="cloudin")) {
		temp<-list.files(path=file.path(dir.sw.in.tr, "cloudin"),pattern="in",include.dirs=FALSE,recursive=TRUE,full.names=TRUE)
		tr_cloud[basename(temp)] <-unlist(lapply(temp,FUN=function(x) return(swReadLines(swClear(new("swCloud")),x))))
	}

	if (any(create_treatments == "LookupClimatePPTScenarios")) {
	  temp <- file.path(dir.sw.in.tr, "LookupClimatePPTScenarios", trfile.LookupClimatePPTScenarios)
	  tr_input_climPPT <- swsf_read_csv(temp)
	}
	if (any(create_treatments == "LookupClimateTempScenarios")) {
	  temp <- file.path(dir.sw.in.tr, "LookupClimateTempScenarios", trfile.LookupClimateTempScenarios)
	  tr_input_climTemp <- swsf_read_csv(temp)
	}
	if (any(create_treatments == "LookupShiftedPPTScenarios")) {
	  temp <- file.path(dir.sw.in.tr, "LookupShiftedPPTScenarios", trfile.LookupShiftedPPTScenarios)
	  tr_input_shiftedPPT <- swsf_read_csv(temp, row.names = 1)
	}
	if (any(create_treatments == "LookupEvapCoeffFromTable")) {
	  temp <- file.path(dir.sw.in.tr, "LookupEvapCoeffFromTable", trfile.LookupEvapCoeffFromTable)
	  tr_input_EvapCoeff <- swsf_read_csv(temp, row.names = 1)
	}

	if (any(grepl("LookupTranspCoeffFromTable_", create_treatments),
	    create_treatments == "AdjRootProfile")) {
	  temp <- file.path(dir.sw.in.tr, "LookupTranspCoeffFromTable", trfile.LookupTranspCoeffFromTable)
		tr_input_TranspCoeff_Code <- tryCatch(read.csv(temp, nrows = 2, stringsAsFactors = FALSE), error = print)
		tr_input_TranspCoeff_Code <- tr_input_TranspCoeff_Code[-2,]
		tr_input_TranspCoeff <- read.csv(temp, skip = 2, stringsAsFactors = FALSE)
		colnames(tr_input_TranspCoeff) <- colnames(tr_input_TranspCoeff_Code)
	}

	if (any(create_treatments == "LookupTranspRegionsFromTable")) {
	  temp <- file.path(dir.sw.in.tr, "LookupTranspRegionsFromTable", trfile.LookupTranspRegionsFromTable)
	  tr_input_TranspRegions <- read.csv(temp, row.names = 1, stringsAsFactors = FALSE)
	}
	if (any(create_treatments == "LookupSnowDensityFromTable")) {
	  temp <- file.path(dir.sw.in.tr, "LookupSnowDensityFromTable", trfile.LookupSnowDensityFromTable)
	  tr_input_SnowD <- read.csv(temp, row.names = 1, stringsAsFactors = FALSE)
	}
	if (any(create_treatments == "AdjMonthlyBioMass_Temperature")) {
	  temp <- file.path(dir.sw.in.tr, "LookupVegetationComposition", trfile.LookupVegetationComposition)
	  tr_VegetationComposition <- read.csv(temp, skip = 1, row.names = 1, stringsAsFactors = FALSE)
	}

	#-import regeneration data
	param.species_regeneration <- list()
	if(aon$dailyRegeneration_GISSM) {
		list.species_regeneration <- list.files(dir.sw.in.reg, pattern=".csv")
		no.species_regeneration <- length(list.species_regeneration)
		if(no.species_regeneration > 0){
			f.temp <- read.csv(file.path(dir.sw.in.reg, list.species_regeneration[1]), stringsAsFactors = FALSE)
			param.species_regeneration <- matrix(NA, ncol=no.species_regeneration, nrow=nrow(f.temp))
			colnames(param.species_regeneration) <- sub(".csv", "", list.species_regeneration)
			rownames(param.species_regeneration) <- f.temp[, 1]
			param.species_regeneration[, 1] <- f.temp[, 2]
			if(no.species_regeneration > 1) for(f in 2:no.species_regeneration){
					f.temp <- read.csv(file.path(dir.sw.in.reg, list.species_regeneration[f]), stringsAsFactors = FALSE)
					param.species_regeneration[, f] <- f.temp[, 2]
				}
			rm(f.temp)
		}
	} else {
		no.species_regeneration <- 0
	}

  do_check_include <- TRUE
  # No compression for fast access; RDS may be slightly faster, but would require loop
  # over assign(, envir = globalenv())
  save(fmaster, SWRunInformation, include_YN, create_experimentals, create_treatments,
    fslayers, sw_input_soillayers,
    sw_input_treatments_use, sw_input_treatments,
    sw_input_experimentals_use, sw_input_experimentals,
    fcloud, sw_input_cloud_use, sw_input_cloud,
    sw_input_prod_use, sw_input_prod,
    sw_input_site_use, sw_input_site,
    fsoils, sw_input_soils_use, sw_input_soils,
    sw_input_weather_use, sw_input_weather,
    sw_input_climscen_use, sw_input_climscen,
    fclimscen, fclimscen_values, sw_input_climscen_values_use, sw_input_climscen_values,
    tr_files, tr_prod, tr_site, tr_soil, tr_weather, tr_cloud, tr_input_climPPT,
    tr_input_climTemp, tr_input_shiftedPPT, tr_input_EvapCoeff, tr_input_TranspCoeff_Code,
    tr_input_TranspCoeff, tr_input_TranspRegions, tr_input_SnowD,
    tr_VegetationComposition,
    param.species_regeneration, no.species_regeneration,
    file = fpreprocin, compress = FALSE)
}

if (!be.quiet)
  print(paste("SWSF reads input data: ended after",
    round(difftime(Sys.time(), t1, units = "secs"), 2), "s"))


#------create scenario names
temp <- climate.conditions[!grepl(climate.ambient, climate.conditions)] #make sure 'climate.ambient' is first entry
if(length(temp) > 0){
	temp <- paste0(rownames(future_yrs), ".", rep(temp, each = nrow(future_yrs)))	#add (multiple) future_yrs
	temp <- paste0(downscaling.method, ".", rep(temp, each=length(downscaling.method))) #add (multiple) downscaling.method
}
climate.conditions <- c(climate.ambient, temp)
scenario_No <- length(climate.conditions)

#------create ensembles
if(length(ensemble.levels) > 0) ensemble.levels <- sort(ensemble.levels)
do.ensembles <- any(actions=="ensemble") && !is.null(ensemble.families) && length(ensemble.levels) > 0 && is.numeric(ensemble.levels) && scenario_No > 1

if(do.ensembles){
	ensemble.families <- paste0(rownames(future_yrs), ".", rep(ensemble.families, each = nrow(future_yrs)))	#add (multiple) future_yrs
	scenarios.ineach.ensemble <- sapply(ensemble.families, function(x) grepl(pattern=x, climate.conditions, ignore.case=TRUE), simplify=TRUE)
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


#------Determine simulation runs
# see ?indices, ?it_exp, ?it_site, ?it_Pid
runsN_master <- nrow(SWRunInformation)
runIDs_sites <- which(include_YN)
runsN_sites <- length(runIDs_sites)
if (!(runsN_sites > 0))
	stop(paste("at least 1 SoilWat-run needed for simulation, but", runsN_sites, "found"))

# identify how many SoilWat-runs = rows are to be carried out
expN <- NROW(sw_input_experimentals)
runsN_total <- runsN_master * max(expN, 1L)
runIDs_total <- seq_len(runsN_total) # consecutive number of all possible (tr x exp) simulations
digitsN_total <- 1 + ceiling(log10(runsN_total))  #max index digits
runsN_job <- runsN_sites * max(expN, 1L)
runsN_Pid <- runsN_total * scenario_No
nextn(runsN_incl, 10)

success <- setup_dbWork(dir.out, runsN_master, runsN_total, expN, include_YN, continueAfterAbort)
if (!success)
  stop("Work database failed to setup or an existing one is from a different simulation design")

runIDs_todo <- dbWork_todos(dir.out) # elements of runIDs_total
runsN_todo <- length(runIDs_todo)


#------outputing data
ExpInput_Seperator <- "X!X"

#append treatment information to the aggregated output in addition to selected Index_RunInformation
Index_RunInformation_Treatments <- NULL
if (length(create_treatments) > 0) {
	Index_RunInformation_Treatments <- match(create_treatments, names(sw_input_treatments))
}

temp <- output_aggregate_daily == "SWAbulk"
if (any(temp) && length(SWPcrit_MPa) > 0) {
  output_aggregate_daily <- output_aggregate_daily[!temp]
  output_aggregate_daily <- c(output_aggregate_daily,
    paste0("SWAbulkatSWPcrit", abs(round(-1000 * SWPcrit_MPa, 0)), "kPa"))
  daily_no <- length(output_aggregate_daily)
}


#------------------------FLAGS FOR EXTERNAL DATA
temp <- matrix(do.ExtractExternalDatasets, nrow = length(do.ExtractExternalDatasets) / 2,
  ncol = 2, byrow = TRUE)
exinfo <- lapply(temp[, 2], function(x) as.logical(as.numeric(x)))
names(exinfo) <- temp[, 1]


exinfo$use_sim_spatial <-
	exinfo$ExtractSoilDataFromCONUSSOILFromSTATSGO_USA	||
	exinfo$ExtractSoilDataFromISRICWISEv12_Global		||
	exinfo$ExtractElevation_NED_USA						||
	exinfo$ExtractElevation_HWSD_Global					||
	exinfo$ExtractSkyDataFromNOAAClimateAtlas_USA		||
	exinfo$ExtractSkyDataFromNCEPCFSR_Global

exinfo$which_NEX <- grepl("NEX", opt_climsc_extr)
exinfo$which_netCDF <- grepl("(GDODCPUCLLNL)|(SageSeer)", opt_climsc_extr)
exinfo$which_ClimateWizard <- grepl("ClimateWizardEnsembles", opt_climsc_extr)


#------------------------SPATIAL SETUP OF SIMULATIONS
if (exinfo$use_sim_spatial || any(actions == "map_input")) {
	if (any(!requireNamespace("rgdal"), !requireNamespace("sp"), !require("raster"))) {
		stop("The packages 'rgdal', 'sp', and 'raster' are required, but one or multiple of them are not installed.")
	}

	# make sure that flag 'sim_cells_or_points' has a valid option
	sim_cells_or_points <- match.arg(sim_cells_or_points, c("point", "cell"))

	# make sure sim_raster agrees with sim_res and sim_crs; sim_raster takes priority
	if (sim_cells_or_points == "cell") {
		if (file.exists(fname_sim_raster)) {
			sim_raster <- raster::raster(fname_sim_raster)
			sim_res <- raster::res(sim_raster)
			sim_crs <- raster::crs(sim_raster)
		}

		# make sure that sim_res is valid
		stopifnot(is.finite(sim_res), length(sim_res) == 2L, sim_res > 0)
	}

	# make sure that sim_crs is valid
  #   - package 'raster' must be loaded so that method 'CRS' for 'as.character' is available
	stopifnot((temp <- rgdal::checkCRSArgs(as.character(sim_crs)))[[1]])
	sim_crs <- sp::CRS(temp[[2]])

	# SpatialPoints of simulation cell centers/sites in WGS84
	crs_sites <- sp::CRS("+init=epsg:4326")	# sp::CRS("+proj=longlat +datum=WGS84 +no_defs")
	run_sites <- sp::SpatialPoints(coords = with(SWRunInformation[runIDs_sites, ],
    data.frame(X_WGS84, Y_WGS84)), proj4string = crs_sites)

} else {
  run_sites <- sim_raster <- crs_sites <- sim_res <- NULL
}



#--------------------------------------------------------------------------------------------------#
#------------------------SET UP PARALLELIZATION
#used in: GriddedDailyWeatherFromNCEPCFSR_Global, external dataset extractions, loop calling do_OneSite, and ensembles

workersN <- 1
do_parallel <- FALSE
cl <- NULL
lockfile <- NULL

if (any(actions == "external") || (actionWithSoilWat && runsN_todo > 0) || do.ensembles) {
  if (parallel_runs) {
    if (!be.quiet)
      print(paste("SWSF prepares parallelization: started at", t1 <- Sys.time()))

    lockfile <- tempfile(pattern = "swsflock", tmpdir = normalizePath(tempdir()))

    if (identical(parallel_backend, "mpi")) {
      Rmpi::mpi.spawn.Rslaves(nslaves = num_cores)

      .Last <- function() { #Properly end mpi slaves before quitting R (e.g., at a crash)
        # based on http://acmmac.acadiau.ca/tl_files/sites/acmmac/resources/examples/task_pull.R.txt
        if (is.loaded("mpi_initialize")) {
          if (requireNamespace("Rmpi") && Rmpi::mpi.comm.size(1) > 0)
            Rmpi::mpi.close.Rslaves()
          .Call("mpi_finalize")
        }
      }

    } else if (identical(parallel_backend, "cluster")) {
      cl <- parallel::makePSOCKcluster(num_cores, outfile = if (!be.quiet) "" else {
        file.path(dir.prj, paste0(format(Sys.time(), "%Y%m%d-%H%M"), "_olog_cluster.txt"))})
      # Worker ID: this needs to be a .x object that does not get deleted with rm(list = ls())
      parallel::clusterApplyLB(cl, seq_len(num_cores), function(x) .nodeNumber <<- x)
      #parallel::clusterSetRNGStream(cl, seed) #random numbers setup
    }

    workersN <- if (identical(parallel_backend, "mpi")) {
        Rmpi::mpi.comm.size() - 1
      } else {
        num_cores #parallel::detectCores(all.tests = TRUE)
      }

    do_parallel <- TRUE
    if (!be.quiet)
      print(paste("SWSF prepares parallelization: initialization of", workersN,
        "workers ended after",  round(difftime(Sys.time(), t1, units = "secs"), 2), "s"))
  }
}

if (!identical(parallel_backend, "mpi")) {
  # Only enforce wall-time on MPI systems
  opt_comp_time[["wall_time_s"]] <- Inf
}
#--------------------------------------------------------------------------------------------------#


#------------------------FUNCTIONS FOR NCEP/CFSR DATA
if(exinfo$GriddedDailyWeatherFromNCEPCFSR_Global || exinfo$ExtractSkyDataFromNCEPCFSR_Global){
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


	dir.ex.CFSR <- file.path(dir.ex.weather, "NCEPCFSR_Global", "CFSR_weather_prog08032012")
	stopifnot(file.exists(dir.ex.CFSR))

	prepd_CFSR <- prepare_NCEPCFSR_extraction(dir.big, dir.ex.CFSR)
	stopifnot(!inherits(prepd_CFSR, "try-error"))
} else {
  prepd_CFSR <- NULL
}


#------------------------DAILY WEATHER
if (extract_determine_database == "SWRunInformation" &&
  "dailyweather_source" %in% colnames(SWRunInformation)) {

  dw_source <- factor(SWRunInformation$dailyweather_source[runIDs_sites],
    levels = dailyweather_options)
  do_weather_source <- anyNA(dw_source)

} else {
  dw_source <- factor(rep(NA, runsN_sites), levels = dailyweather_options)
  do_weather_source <- TRUE
}

weather.digits <- 2


if(exinfo$GriddedDailyWeatherFromMaurer2002_NorthAmerica){
	#extract daily weather information for the grid cell coded by latitude/longitude for each simulation run
	#Citation: Maurer, E. P., A. W. Wood, J. C. Adam, D. P. Lettenmaier, and B. Nijssen. 2002. A long-term hydrologically based dataset of land surface fluxes and states for the conterminous United States. Journal of Climate 15:3237-3251.

	dir.ex.maurer2002 <- file.path(dir.ex.weather, "Maurer+_2002updated", "DAILY_FORCINGS")
	stopifnot(file.exists(dir.ex.maurer2002))

} else {
  dir.ex.maurer2002 <- NULL
}

if(exinfo$GriddedDailyWeatherFromDayMet_NorthAmerica){
	# https://daymet.ornl.gov/
	#extract daily weather information for the grid cell coded by latitude/longitude for each simulation run
	#Citation
	#	- article: Thornton, P.E., Running, S.W., White, M.A. 1997. Generating surfaces of daily meteorological variables over large regions of complex terrain. Journal of Hydrology 190: 214 - 251. http://dx.doi.org/10.1016/S0022-1694(96)03128-9
	#	- dataset v2: Thornton, P.E., M.M. Thornton, B.W. Mayer, N. Wilhelmi, Y. Wei, R. Devarakonda, and R.B. Cook. 2014. Daymet: Daily Surface Weather Data on a 1-km Grid for North America, Version 2. ORNL DAAC, Oak Ridge, Tennessee, USA. Accessed Month DD, YYYY. Time period: YYYY-MM-DD to YYYY-MM-DD. Spatial range: N=DD.DD, S=DD.DD, E=DDD.DD, W=DDD.DD. http://dx.doi.org/10.3334/ORNLDAAC/1219
  # - dataset v3: Thornton, P.E., M.M. Thornton, B.W. Mayer, Y. Wei, R. Devarakonda, R.S. Vose, and R.B. Cook. 2016. Daymet: Daily Surface Weather Data on a 1-km Grid for North America, Version 3. ORNL DAAC, Oak Ridge, Tennessee, USA. Accessed Month DD, YYYY. Time period: YYYY-MM-DD to YYYY-MM-DD. Spatial Range: N=DD.DD, S=DD.DD, E=DDD.DD, W=DDD.DD. http://dx.doi.org/10.3334/ORNLDAAC/1328

  #dir.ex.daymet <- file.path(dir.ex.weather, "DayMet_NorthAmerica", "DownloadedSingleCells_FromDayMetv2_NorthAmerica")
  dir.ex.daymet <- file.path(dir.ex.weather, "DayMet_NorthAmerica", "DownloadedSingleCells_FromDayMetv3_NorthAmerica")
	if (!file.exists(dir.ex.daymet))
	  stop("Directory for external dataset 'DayMet' does not exist:", shQuote(dir.ex.daymet))
	stopifnot(require(DaymetR)) #https://github.com/khufkens/daymetr

} else {
  dir.ex.daymet <- NULL
}

if(exinfo$GriddedDailyWeatherFromNRCan_10km_Canada && createAndPopulateWeatherDatabase){
	#Citations:
	#	- Hopkinson, R. F., D. W. McKenney, E. J. Milewska, M. F. Hutchinson, P. Papadopol, and L. A. Vincent. 2011. Impact of Aligning Climatological Day on Gridding Daily Maximum–Minimum Temperature and Precipitation over Canada. Journal of Applied Meteorology and Climatology 50:1654-1665.
	#	- Hutchinson, M. F., D. W. McKenney, K. Lawrence, J. H. Pedlar, R. F. Hopkinson, E. Milewska, and P. Papadopol. 2009. Development and Testing of Canada-Wide Interpolated Spatial Models of Daily Minimum–Maximum Temperature and Precipitation for 1961–2003. Journal of Applied Meteorology and Climatology 48:725-741.
	#	- McKenney, D. W., M. F. Hutchinson, P. Papadopol, K. Lawrence, J. Pedlar, K. Campbell, E. Milewska, R. F. Hopkinson, D. Price, and T. Owen. 2011. Customized Spatial Climate Models for North America. Bulletin of the American Meteorological Society 92:1611-1622.
	dir.ex.NRCan <- file.path(dir.ex.weather, "NRCan_10km_Canada", "DAILY_GRIDS")
	stopifnot(file.exists(dir.ex.NRCan), require(raster), require(sp), require(rgdal))

} else {
  dir.ex.NRCan <- NULL
}



#------------------------CHECK THAT DAILY WEATHER DATA IS AVAILABLE
if (do_weather_source) {
  #--- Determine sources of daily weather
  SWRunInformation <- dw_determine_sources(dw_source, exinfo, dailyweather_options,
    create_treatments, runIDs_sites, SWRunInformation, sw_input_treatments_use,
    sw_input_treatments, sw_input_experimentals_use, sw_input_experimentals, simstartyr,
    endyr, fmaster, fpreprocin, dir.ex.NRCan, dir.ex.maurer2002, dir.sw.in.tr,
    verbose = !be.quiet)
}

if (anyNA(SWRunInformation[runIDs_sites, "dailyweather_source"])) {
  stop("There are sites without daily weather. Provide data for all runs")
}

if (exinfo$ExtractClimateChangeScenarios) {
  getScenarioWeatherDataFromDatabase <- TRUE
  getCurrentWeatherDataFromDatabase <- TRUE
}
if (getScenarioWeatherDataFromDatabase)
  getCurrentWeatherDataFromDatabase <- TRUE

if (getCurrentWeatherDataFromDatabase){
  if (!(createAndPopulateWeatherDatabase || file.exists(dbWeatherDataFile))) {
    stop("Create or use existing Weather database with Scenario data inside.")
  }

} else {
  if (!any(create_treatments == "LookupWeatherFolder",
    exinfo$GriddedDailyWeatherFromMaurer2002_NorthAmerica,
    exinfo$GriddedDailyWeatherFromDayMet_NorthAmerica)) {
    stop("Daily weather data must be provided through 'LookupWeatherFolder', ",
      "'Maurer2002_NorthAmerica', or 'DayMet_NorthAmerica' since no weather database is ",
      "used")
  }
}


#----------------------------------------------------------------------------------------#
#------------ORGANIZE DATABASES FOR DAILY WEATHER AND FOR SIMULATION OUTPUT
if (!be.quiet)
  print(paste("SWSF sets up the database: started at", t1 <- Sys.time()))

#--- Create weather database and populate with weather for current conditions
if (createAndPopulateWeatherDatabase) {
	if (file.exists(dbWeatherDataFile)) {
		if (continueAfterAbort) {
			stop("Weather database exists, 'continueAfterAbort' is TRUE, and ",
			  "'createAndPopulateWeatherDatabase' is TRUE: a maximum of two of these three ",
			  "conditions may simultaneously be TRUE: adjust inputs and restart")

		} else {
			print("Removing old weather database")
			unlink(dbWeatherDataFile)
		}
	}

  make_dbW(dbWeatherDataFile, runIDs_sites, SWRunInformation, simstartyr, endyr,
    climate.conditions, dir.sw.in.tr, dir.out.temp,
    chunk_size.options, continueAfterAbort, deleteTmpSQLFiles, dbW_compression_type,
    do_parallel, parallel_backend, num_cores, cl,
    dir.ex.maurer2002 = dir.ex.maurer2002, dir.ex.daymet = dir.ex.daymet,
    dir.ex.NRCan = dir.ex.NRCan, prepd_CFSR = prepd_CFSR,
    verbose = !be.quiet)
}

if (getCurrentWeatherDataFromDatabase || getScenarioWeatherDataFromDatabase) {
	# Check that version of dbWeather suffices
	dbW_setConnection(dbWeatherDataFile)
	v_dbW <- dbW_version()

	if (v_dbW < minVersion_dbWeather) {
		print(paste0("The version (", v_dbW, ") of the daily weather database is outdated; ",
		  "min. version required: ", minVersion_dbWeather))
		if (v_dbW >= "1")
		  print(paste("Use function 'Rsoilwat31:::dbW_upgrade_v1to2' etc. to upgrade your",
		    "version 1.y.z weather database to version >=", minVersion_dbWeather))
		stop("Outdated weather database")
	}

}


#--- Prepare output database
name.OutputDB <- file.path(dir.out, "dbTables.sqlite3")
if (copyCurrentConditionsFromDatabase || copyCurrentConditionsFromTempSQL)
  name.OutputDBCurrent <- file.path(dir.out, "dbTables_current.sqlite3")

do.clean <- (cleanDB && !(length(actions) == 1 && actions == "ensemble"))

if (!file.exists(name.OutputDB) || do.clean) {
  temp <- try(make_dbOutput(name.OutputDB, SWRunInformation, Index_RunInformation,
      runsN_master, runIDs_sites, runsN_Pid, runsN_total, scenario_No, expN,
      create_treatments, create_experimentals, sw_input_treatments, sw_input_treatments_use,
      sw_input_experimentals, climate.conditions, simstartyr, startyr, endyr, digitsN_total,
      aon, daily_no, daily_lyr_agg, output_aggregate_daily, SoilLayer_MaxNo, SWPcrit_MPa,
      Tmin_crit_C, Tmax_crit_C, Tmean_crit_C, bin.prcpSizes, bin.prcpfreeDurations,
      DegreeDayBase, st_mo, no.species_regeneration, param.species_regeneration,
      do_clean = do.clean))

  if (inherits(temp, "try-error")) {
    unlink(name.OutputDB)
    stop(paste("Creation of output database failed:", temp, collapse = ", "))
  }
}

con_dbOut <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = name.OutputDB)
set_PRAGMAs(con_dbOut, PRAGMA_settings2())

temp <- RSQLite::dbListFields(con_dbOut, "aggregation_overall_mean")
dbOverallColumns <- length(temp) - 1L

RSQLite::dbDisconnect(con_dbOut)


if (!be.quiet)
  print(paste("SWSF sets up the database: ended after",
    round(difftime(Sys.time(), t1, units = "secs"), 2), "s"))


#----------------------------------------------------------------------------------------#
#------simulation timing
simTime <- simTiming(startyr, simstartyr, endyr)
simTime_ForEachUsedTimeUnit_North <- simTiming_ForEachUsedTimeUnit(simTime,
  sim_tscales = c("daily", "monthly", "yearly"),
  latitude = 90,
	account_NorthSouth = accountNSHemispheres_agg)

if(accountNSHemispheres_agg){
	simTime_ForEachUsedTimeUnit_South <- simTiming_ForEachUsedTimeUnit(simTime,
	  sim_tscales = c("daily", "monthly", "yearly"),
	  latitude = -90,
	  account_NorthSouth = accountNSHemispheres_agg)

} else {
	simTime_ForEachUsedTimeUnit_South <- simTime_ForEachUsedTimeUnit_North
}


#--------------------------------------------------------------------------------------------------#
#------------------------OBTAIN INFORMATION FROM EXTERNAL DATASETS PRIOR TO SIMULATION RUNS TO CREATE THEM
if(any(actions == "external") && any(exinfo[!grepl("GriddedDailyWeather", names(exinfo))] > 0)){
  if (!be.quiet)
    print(paste("SWSF extracts information from external datasets prior to simulation",
      "runs: started at", t1 <- Sys.time()))

  stopifnot(file.exists(dir.external))

  if (exinfo$ExtractSoilDataFromCONUSSOILFromSTATSGO_USA ||
    exinfo$ExtractSoilDataFromISRICWISEv12_Global) {

    temp <- ExtractData_Soils(SWRunInformation, runsN_master, runsN_sites,
      runIDs_sites, run_sites, sw_input_soillayers, sw_input_soils_use, sw_input_soils,
      extract_determine_database, sim_cells_or_points, sim_res, sim_crs, crs_sites,
      dir.ex.soil, fmaster, fslayers, fsoils, fpreprocin, continueAfterAbort, be.quiet,
      do_parallel, parallel_backend, cl)

    SWRunInformation <- temp[["SWRunInformation"]]
    sw_input_soillayers <- temp[["sw_input_soillayers"]]
    sw_input_soils_use <- temp[["sw_input_soils_use"]]
    sw_input_soils <- temp[["sw_input_soils"]]
  }

  if (exinfo$ExtractSkyDataFromNOAAClimateAtlas_USA ||
    exinfo$ExtractSkyDataFromNCEPCFSR_Global) {

    temp <- ExtractData_MeanMonthlyClimate(SWRunInformation, runsN_master, runsN_sites,
      runIDs_sites, run_sites, sw_input_cloud_use, sw_input_cloud, st_mo,
      extract_determine_database, sim_cells_or_points, sim_res, sim_crs, crs_sites,
      dir.ex.weather, dir.out.temp, fmaster, fcloud, fpreprocin, chunk_size.options,
      continueAfterAbort, be.quiet, prepd_CFSR, startyr, endyr, do_parallel,
      parallel_backend, cl)

    SWRunInformation <- temp[["SWRunInformation"]]
    sw_input_cloud_use <- temp[["sw_input_cloud_use"]]
    sw_input_cloud <- temp[["sw_input_cloud"]]
  }

  if (exinfo$ExtractElevation_NED_USA || exinfo$ExtractElevation_HWSD_Global) {
    SWRunInformation <- ExtractData_Elevation(SWRunInformation, runsN_master, runsN_sites,
      runIDs_sites, run_sites, extract_determine_database, sim_cells_or_points, sim_res,
      sim_crs, crs_sites, dir.ex.dem, fmaster, fpreprocin, continueAfterAbort, be.quiet)
  }

  if (exinfo$ExtractClimateChangeScenarios) {
    climDB_metas <- climscen_metadata()

    SWRunInformation <- climscen_determine_sources(extract_determine_database,
      opt_climsc_extr, runIDs_sites, runsN_sites, SWRunInformation, fmaster, fpreprocin)

    if (any(exinfo$which_NEX) || any(exinfo$which_netCDF)) {
      SWRunInformation <- ExtractClimateChangeScenarios(opt_climsc_extr, climDB_metas,
        opt_DS, simstartyr, endyr, DScur_startyr, DScur_endyr, future_yrs,
        dbWeatherDataFile, climate.ambient, climate.conditions, runsN_master, runsN_sites,
        runIDs_sites, SWRunInformation, fmaster, fpreprocin, dir.out.temp,
        verbose = !be.quiet, print.debug, cl)
    }

    if (any(exinfo$which_ClimateWizard)) {
      temp <- ExtractClimateWizard(opt_climsc_extr, SWRunInformation, fmaster, fpreprocin,
        sw_input_climscen_use, sw_input_climscen, fclimscen,
        sw_input_climscen_values_use, sw_input_climscen_values, fclimscen_values,
        dir.ex.fut, runsN_master, verbose = !be.quiet)

      SWRunInformation <- temp[["SWRunInformation"]]
      sw_input_climscen_use <- temp[["sw_input_climscen_use"]]
      sw_input_climscen <- temp[["sw_input_climscen"]]
      sw_input_climscen_values_use <- temp[["sw_input_climscen_values_use"]]
      sw_input_climscen_values <- temp[["sw_input_climscen_values"]]
    }
  }

  do_check_include <- TRUE

  if (!be.quiet)
    print(paste("SWSF extracts information from external datasets prior to simulation",
      "runs: ended after",  round(difftime(Sys.time(), t1, units = "secs"), 2), "s"))
}



#--------------------------------------------------------------------------------------------------#
#------------------------CHECK THAT INCLUDE_YN* ARE INCLUSIVE
if (do_check_include) {
  SWRunInformation <- check_requested_sites(include_YN, SWRunInformation, fmaster,
    fpreprocin, verbose = !be.quiet)
}


#--------------------------------------------------------------------------------------------------#
#------------------------CALCULATIONS PRIOR TO SIMULATION RUNS TO CREATE THEM

temp <- matrix(do.PriorCalculations, nrow = length(do.PriorCalculations) / 2, ncol = 2,
  byrow = TRUE)
pcalcs <- lapply(temp[, 2], function(x) as.logical(as.numeric(x)))
names(pcalcs) <- temp[, 1]

if (actionWithSoilWat) {
	do.GetClimateMeans <- any(sw_input_climscen_values_use) ||
			pcalcs$EstimateConstantSoilTemperatureAtUpperAndLowerBoundaryAsMeanAnnualAirTemperature ||
			sw_input_site_use["SoilTempC_atLowerBoundary"] ||
			sw_input_site_use["SoilTempC_atUpperBoundary"] ||
			pcalcs$EstimateInitialSoilTemperatureForEachSoilLayer ||
			any(create_treatments == "PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996") ||
			any(create_treatments == "AdjMonthlyBioMass_Temperature") ||
			any(create_treatments == "AdjMonthlyBioMass_Precipitation") ||
			any(create_treatments == "Vegetation_Biomass_ScalingSeason_AllGrowingORNongrowing")
}

if (any(unlist(pcalcs))) {
  if (!be.quiet)
    print(paste("SWSF makes calculations prior to simulation runs: started at",
      t1 <- Sys.time()))

    runIDs_adjust <- seq_len(runsN_master)  # if not all, then runIDs_sites

  if (pcalcs$ExtendSoilDatafileToRequestedSoilLayers) {
    temp <- calc_ExtendSoilDatafileToRequestedSoilLayers(requested_soil_layers,
      runIDs_adjust, sw_input_soillayers, sw_input_soils_use, sw_input_soils,
      fpreprocin, fslayers, fsoils, verbose = !be.quiet)

    sw_input_soillayers <- temp[["sw_input_soillayers"]]
    sw_input_soils_use <- temp[["sw_input_soils_use"]]
    sw_input_soils <- temp[["sw_input_soils"]]
  }

  if (pcalcs$CalculateBareSoilEvaporationCoefficientsFromSoilTexture) {
    #calculate bare soil evaporation coefficients per soil layer for each simulation run and copy values to 'datafile.soils'
    # soil texture influence based on re-analysis of data from Wythers KR, Lauenroth WK, Paruelo JM (1999) Bare-Soil Evaporation Under Semiarid Field Conditions. Soil Science Society of America Journal, 63, 1341-1349.

    temp <- calc_CalculateBareSoilEvaporationCoefficientsFromSoilTexture(runIDs_adjust,
      sw_input_soils_use, sw_input_soils, fpreprocin, fsoils, continueAfterAbort,
      verbose = !be.quiet)

    sw_input_soils_use <- temp[["sw_input_soils_use"]]
    sw_input_soils <- temp[["sw_input_soils"]]
  }

	#------used during each simulation run: define functions here
	if (any(actions == "create") &&
	  pcalcs$EstimateConstantSoilTemperatureAtUpperAndLowerBoundaryAsMeanAnnualAirTemperature) {

		sw_input_site_use["SoilTempC_atLowerBoundary"] <- TRUE #set use flag
		sw_input_site_use["SoilTempC_atUpperBoundary"] <- TRUE
		#call function 'SiteClimate' in each SoilWat-run
	}

	if (any(actions == "create") && pcalcs$EstimateInitialSoilTemperatureForEachSoilLayer) {
		#set use flags
		ld <- seq_len(SoilLayer_MaxNo)
		use.layers <- which(sw_input_soils_use[paste0("Sand_L", ld)])
		index.soilTemp <- paste0("SoilTemp_L", ld)[use.layers]
		soilTemp <- sw_input_soils[runIDs_adjust, index.soilTemp, drop = FALSE]
		sw_input_soils_use[index.soilTemp] <- TRUE
	}

	if (!be.quiet)
	  print(paste("SWSF makes calculations prior to simulation runs: ended after",
	    round(difftime(Sys.time(), t1, units = "secs"), 2), "s"))
}


#--------------------------------------------------------------------------------------------------#
#------------------------OBTAIN INFORMATION FROM TABLES PRIOR TO SIMULATION RUNS TO CREATE THEM

if (any(actions == "create")) {
  temp <- do_prior_TableLookups(tr_input_EvapCoeff, tr_input_TranspRegions, tr_input_SnowD,
    create_treatments, SoilLayer_MaxNo, sw_input_experimentals_use, sw_input_experimentals,
    sw_input_soils_use, sw_input_soils, sw_input_cloud_use, sw_input_cloud,
    fpreprocin, fsoils, fcloud, continueAfterAbort, verbose = !be.quiet)

  done_prior <- temp[["done_prior"]]
  sw_input_soils_use <- temp[["sw_input_soils_use"]]
  sw_input_soils <- temp[["sw_input_soils"]]
  sw_input_cloud_use <- temp[["sw_input_cloud_use"]]
  sw_input_cloud <- temp[["sw_input_cloud"]]
}


#--------------------------------------------------------------------------------------------------#
#------------------------MAP INPUT VARIABLES (FOR QUALITY CONTROL)
if (any(actions == "map_input") && length(map_vars) > 0) {
  map_input_variables(map_vars, SWRunInformation, sw_input_soillayers, sw_input_cloud_use,
    sw_input_cloud, sw_input_prod_use, sw_input_prod, sw_input_site_use, sw_input_site,
    sw_input_soils_use, sw_input_soils, sw_input_weather_use, sw_input_weather,
    sw_input_climscen_use, sw_input_climscen, sw_input_climscen_values_use,
    sw_input_climscen_values, runIDs_sites, sim_cells_or_points, run_sites, crs_sites,
    sim_crs, sim_raster, dir.out, verbose = !be.quiet)
}






#--------------------------------------------------------------------------------------------------#
#------------------------RUN RSOILWAT

# print system information
print(temp <- sessionInfo())
if (check.blas)
  benchmark_BLAS(temp$platform)


# run the simulation experiment
if (actionWithSoilWat && runsN_todo > 0) {
  swof <- sw_out_flags()
  swDataFromFiles <- read_SoilWat_FileDefaults(dir.sw.in, swFilesIn)
  args_do_OneSite <- gather_args_do_OneSite()

  runs.completed <- run_simulation_experiment(rSWSF, cl, runIDs_todo, use_rcpp,
    SWRunInformation, sw_input_soillayers, sw_input_treatments, sw_input_cloud,
    sw_input_prod, sw_input_site, sw_input_soils, sw_input_weather, sw_input_climscen,
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

if (any(actions == "concatenate")) {
  if (!be.quiet)
    print(paste("SWSF inserting temporary data to outputDB: started at", t.outputDB))

  has_time_to_concat <- (difftime(t.outputDB, t.overall, units = "secs") +
    opt_comp_time[["one_concat_s"]]) < opt_comp_time[["wall_time_s"]]

  if (has_time_to_concat) {
    move_temporary_to_outputDB(dir.out.temp, name.OutputDB,
      name.OutputDBCurrent, t.overall, opt_comp_time,
      copyCurrentConditionsFromTempSQL && !copyCurrentConditionsFromDatabase,
      cleanDB, deleteTmpSQLFiles, continueAfterAbort, print.debug, verbose = !be.quiet)

  } else {
    print(paste("Need at least", opt_comp_time[["one_concat_s"]], "seconds to put SQL in output DB."))
  }


  if (copyCurrentConditionsFromDatabase && !copyCurrentConditionsFromTempSQL) {
    has_time_to_concat <- {difftime(Sys.time(), t.overall, units = "secs") +
      opt_comp_time[["one_concat_s"]]} < opt_comp_time[["wall_time_s"]]

    if (has_time_to_concat) {
      do_copyCurrentConditionsFromDatabase(name.OutputDB, name.OutputDBCurrent,
        verbose = !be.quiet)

    } else {
      print(paste("Need at least", opt_comp_time[["one_concat_s"]], "seconds to put SQL in output DB."))
    }
  }
}

#timing of outputDB
delta.outputDB <- as.double(difftime(Sys.time(), t.outputDB, units = "secs"))
if (!be.quiet & any(actions == "concatenate")) {
  print(paste("SWSF inserting temporary data to outputDB: ended after",
    round(delta.outputDB, 2), "s"))
}


#--------------------------------------------------------------------------------------------------#
#------------------------CHECK COMPLETENESS OF OUTPUT DATABASE AND SIMULATION
t.check <- Sys.time()

if (any(actions == "check")) {
  if (!be.quiet)
    print(paste("SWSF checks simulations and output: started at", t.check))

  #------ Remove when this becomes a R package
  if (!exists("swsf_env")) {
    swsf_env <- new.env(parent = emptyenv())
    suppressWarnings(load(rSWSF, envir = swsf_env))
  }

  check_outputDB_completeness(name.OutputDB, name.OutputDBCurrent,
    update_workDB = check_updates_workDB || deleteTmpSQLFiles,
    do_DBcurrent = copyCurrentConditionsFromDatabase || copyCurrentConditionsFromTempSQL,
    do_parallel, parallel_backend, cl, dir.out, swsf_env)
}

#timing of check
delta.check <- difftime(Sys.time(), t.check, units = "secs")
if (!be.quiet & any(actions == "check"))
  print(paste("SWSF checks simulations and output: ended after", round(delta.check, 2), "s"))

#--------------------------------------------------------------------------------------------------#
#------------------------ENSEMBLE GENERATION
t.ensembles <- Sys.time()	#timing of ensemble calculation

if(do.ensembles && all.complete && (actionWithSoilWat && runs.completed == runsN_todo || actionWithSWSFOutput && !actionWithSoilWat) ){

	if(!be.quiet) print(paste("SWSF calculates ensembles: started at", t.ensembles))

	#save(ensembles.maker,ensemble.levels, ensemble.families, file=file.path(dir.out, "ensembleObjects.r"))


	library(RSQLite,quietly = TRUE)
	con <- DBI::dbConnect(RSQLite::SQLite(), dbname = name.OutputDB)

	Tables <- dbOutput_ListOutputTables(con)
	Tables <- Tables[-grep(pattern="_sd", Tables, ignore.case = T)]

	if (do_parallel) {
		#call the simulations depending on parallel backend
		list.export <- c("ensembleCollectSize","Tables","save.scenario.ranks","ensemble.levels","calc.ensembles","scenario_No","opt_comp_time", "collect_EnsembleFromScenarios","dir.out","ensemble.families","t.overall","do_parallel","parallel_backend","name.OutputDB")
		if(identical(parallel_backend, "mpi")) {
			export_objects_to_workers(list.export, list(global = globalenv()), "mpi")

			Rmpi::mpi.bcast.cmd(library(RSQLite,quietly = TRUE))

			ensembles.completed <- Rmpi::mpi.applyLB(X = Tables, FUN = collect_EnsembleFromScenarios)

		} else if(identical(parallel_backend, "cluster")) {
			export_obj_local <- list.export[list.export %in% ls(name=environment())]
			export_obj_in_parent <- list.export[list.export %in% ls(name=parent.frame())]
			export_obj_in_parent <- export_obj_in_parent[!(export_obj_in_parent %in% export_obj_local)]
			export_obj_in_globenv <- list.export[list.export %in% ls(name=globalenv())]
			export_obj_in_globenv <- export_obj_in_globenv[!(export_obj_in_globenv %in% c(export_obj_local, export_obj_in_parent))]
			stopifnot(c(export_obj_local, export_obj_in_parent, export_obj_in_globenv) %in% list.export)

			if(length(export_obj_local) > 0) parallel::clusterExport(cl, export_obj_local, envir=environment())
			if(length(export_obj_in_parent) > 0) parallel::clusterExport(cl, export_obj_in_parent, envir=parent.frame())
			if(length(export_obj_in_globenv) > 0) parallel::clusterExport(cl, export_obj_in_globenv, envir=globalenv())

			parallel::clusterEvalQ(cl, library(RSQLite,quietly = TRUE))

      ensembles.completed <- parallel::clusterApplyLB(cl, x = seq_along(Tables), function(i)
        collect_EnsembleFromScenarios(Tables[i]))
		}

	} else {
    ensembles.completed <- lapply(seq_along(Tables), function(i)
      collect_EnsembleFromScenarios(Tables[i]))
	}

  ensembles.completed <- sum(unlist(ensembles.completed))

	if(ensembles.completed != (temp <- length(Tables)*ifelse(save.scenario.ranks, 3, 2)*length(ensemble.families)*length(ensemble.levels))) print("SWSF calculates ensembles: something went wrong with ensemble output: ensembles.completed = ", ensembles.completed, " instead of ", temp,".")
} else {
  ensembles.completed <- 0
}


#timing of ensemble calculation
delta.ensembles <- difftime(Sys.time(), t.ensembles, units="secs")
if(!be.quiet && do.ensembles) print(paste("SWSF calculates ensembles: ended after", round(delta.ensembles, 2), "s"))


#--------------------------------------------------------------------------------------------------#
#------------------------OVERALL TIMING
delta.overall <- difftime(Sys.time(), t.overall, units = "secs")
if (!be.quiet)
 print(paste("SWSF: ended after", round(delta.overall, 2), "s"))

compile_overall_timer(timerfile2, dir.out, workersN, runs.completed, scenario_No,
  ensembles.completed, delta.overall, delta.outputDB, delta.check, delta.ensembles)


if (!be.quiet)
  print(paste("SWSF: ended with actions =", paste(actions, collapse = ", "), "at",
    Sys.time()))


#--------------------------------------------------------------------------------------------------#
#------------------------CODE CLEANUP

options(ow_prev) #sets the warning option to its previous value

if (do_parallel) {
  clean_parallel_workers(parallel_backend, cl, verbose = print.debug)
}


#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#
