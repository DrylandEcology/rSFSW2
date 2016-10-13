#--------------------------------------------------------------------------------------------------#
#------------------------PREPARE SOILWAT SIMULATIONS

if(!be.quiet) print(paste("SWSF is executed for:", sQuote(basename(dir.prj)), "and started at", Sys.time()))

#------
actionWithSoilWat <- any(actions == "create") || any(actions == "execute") || any(actions == "aggregate")
actionWithSWSFOutput <- any(actions == "concatenate") || any(actions == "ensemble")
#--order output_aggregate_daily--#
if(length(output_aggregate_daily) > 0) output_aggregate_daily <- output_aggregate_daily[order(output_aggregate_daily)]
#------
ow <- options("warn", "error")
#    - warn < 0: warnings are ignored
#    - warn = 0: warnings are stored until the top–level function returns
#    - warn = 1: warnings are printed as they occur
#    - warn = 2: all warnings are turned into errors
options(warn = debug.warn.level)

if (debug.dump.objects) {
	# dumps objects and frames to files, and (if not interactive) quits
	# Note: view dumped frames with
	# load(file.path(dir.prj, "last.dump.rda"))
	# debugger(`path/to/file/last.dump.rda`)
	options(error = quote({
    dump_objs <- new.env()

    for (p in sys.parents()) {
      if (inherits(try(sys.frame(p), silent = TRUE), "try-error"))
        next
      items <- setdiff(ls(name = sys.frame(p)), ls(name = dump_objs))
      for (it in items)
        assign(it, get(it, pos = sys.frame(p)), envir = dump_objs)
    }

    save(list = ls(name = dump_objs), envir = dump_objs,
      file = file.path(dir.prj, "last.dump.save.RData"))

		dump.frames(dumpto = file.path(dir.prj, "last.dump"), to.file = TRUE)

		if (!interactive())
			q("no")
	}))
} else {
	options(error = traceback)
}


#create simulation directory structure
dir.sw.in <- normalizePath(dir.sw.in)
if(makeInputForExperimentalDesign) dir.out.experimentalInput <- file.path(dir.out, "Experimentals_Input_Data")
dir.out.temp <- file.path(dir.out, "temp")
dir.create2(dir.out, showWarnings=FALSE, recursive=TRUE)
dir.create2(dir.big, showWarnings=FALSE, recursive=TRUE)
if(saveRsoilwatInput || saveRsoilwatOutput) dir.create2(dir.sw.runs, showWarnings=FALSE, recursive=TRUE)
dir.create2(dir.out.temp, showWarnings=FALSE, recursive=TRUE)
if(makeInputForExperimentalDesign) dir.create2(dir.out.experimentalInput, showWarnings=FALSE, recursive=TRUE)
dirname.sw.runs.weather <- "WeatherData"

#timing: basis for estimated time of arrival, ETA
timerfile <- "temp_timer.csv"
ftemp <- file.path(dir.out, timerfile)
times <- if (!file.exists(ftemp) || !continueAfterAbort) {
    cat("0,NA", file = ftemp, sep = "\n")
  } else {
   swsf_read_csv(file = ftemp, header = FALSE, colClasses = c("integer", "NULL"), skip = 1)[[1]]
  }
runIDs_done <- if (length(times) > 0) sort(times) else NULL

#timing: output for overall timing information
timerfile2 <- "Timing_Simulation.csv"
ftemp <- file.path(dir.out, timerfile2)
cat(",Time_s,Number", file = ftemp, sep = "\n")

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

if(!require(Rsoilwat31,quietly = TRUE) || (require(Rsoilwat31,quietly = TRUE) && packageVersion("Rsoilwat31") < minVersionRsoilwat)) {
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
	stopifnot(require(Rsoilwat31,quietly = TRUE) && packageVersion("Rsoilwat31") >= minVersionRsoilwat)
}
if(!require(circular, quietly=TRUE)) {
	tryCatch(install.packages("circular",repos=url.Rrepos,lib=dir.libraries), warning=function(w) { print(w); print("circular failed to install"); stop("Stopping") })
	stopifnot(require(circular, quietly=TRUE))
}
if(!require(SPEI, quietly=TRUE)) {
	tryCatch(install.packages("SPEI",repos=url.Rrepos,lib=dir.libraries), warning=function(w) { print(w); print("SPEI failed to install"); stop("Stopping") })
	stopifnot(require(SPEI, quietly=TRUE))
}
if(!require(RSQLite,quietly = TRUE)) {
	tryCatch(install.packages("RSQLite",repos=url.Rrepos,lib=dir.libraries), warning=function(w) { print(w); print("RSQLite failed to install"); stop("Stopping") })
	stopifnot(require(RSQLite, quietly = TRUE))
}

if(parallel_runs && identical(parallel_backend, "mpi")) {
	if(!require(Rmpi, quietly = TRUE)) {
	  print(paste("'Rmpi' requires a MPI backend, e.g., OpenMPI available from",
	              "https://www.open-mpi.org/software/ompi/",
	              "with install instructions at https://www.open-mpi.org/faq/?category=building#easy-build"))
	  print(paste("If no MPI is available, installation of 'Rmpi' will fail and may print the",
	              "error message: 'Cannot find mpi.h header file'"))
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
temp <- matrix(data=output_aggregates, ncol=2, nrow=length(output_aggregates)/2, byrow=TRUE)
aon <- lapply(temp[, 2], function(x) as.logical(as.numeric(x)))
names(aon) <- temp[, 1]

#------import data
if(!be.quiet) print(paste("SWSF reads input data: started at", t1 <- Sys.time()))

# Read data from files
do_check_include <- FALSE
if (usePreProcessedInput && file.exists(file.path(dir.in, datafile.SWRWinputs_preprocessed))) {
	load(file = file.path(dir.in, datafile.SWRWinputs_preprocessed),
		envir = .GlobalEnv) # This however annihilates all objects in .GlobalEnv with the same names !

} else {
  SWRunInformation <- tryCatch(swsf_read_csv(file.path(dir.in, datafile.SWRunInformation)),
    error = print)
  stopifnot(sapply(c("Label", "site_id", "WeatherFolder", "X_WGS84", "Y_WGS84", "ELEV_m", "Include_YN"),
    function(x) x %in% names(SWRunInformation)),		# required columns
    all(SWRunInformation$site_id == seq_len(nrow(SWRunInformation))),	# consecutive site_id
    !grepl("[[:space:]]", SWRunInformation$Label),	# no space-characters in label
    !grepl("[[:space:]]", SWRunInformation$WeatherFolder)	# no space-characters in weather-data names
  )
  include_YN <- SWRunInformation$Include_YN
  labels <- SWRunInformation$Label

  sw_input_soillayers <- tryCatch(swsf_read_csv(file.path(dir.in, datafile.soillayers)),
    error = print)

  temp <- tryCatch(swsf_read_inputfile(file.path(dir.in, datafile.treatments)),
    error = print)
  sw_input_treatments_use <- temp[["use"]]
  sw_input_treatments <- temp[["data"]]
  stopifnot(
    !grepl("[[:space:]]", sw_input_treatments$LookupWeatherFolder)	# no space-characters in weather-data names
  )

  temp <- tryCatch(swsf_read_inputfile(file.path(dir.in, datafile.Experimentals)),
    error = print)
  sw_input_experimentals_use <- temp[["use"]]
  sw_input_experimentals <- temp[["data"]]
  create_experimentals <- names(sw_input_experimentals_use[sw_input_experimentals_use])
  stopifnot(
    !grepl("[[:space:]]", sw_input_experimentals$LookupWeatherFolder)	# no space-characters in weather-data names
  )

  #update treatment specifications based on experimental design
  sw_input_treatments_use_combined <- sw_input_treatments_use |
  	names(sw_input_treatments_use) %in% create_experimentals
  temp <- which(!(create_experimentals %in% names(sw_input_treatments_use)))
  if (length(temp) > 0) {
    sw_input_treatments_use_combined <- cbind(sw_input_treatments_use_combined,
      matrix(1, nrow = 1, ncol = length(temp), dimnames = list(NA, c(create_experimentals[temp]))))
  }
  create_treatments <- names(sw_input_treatments_use_combined)[sw_input_treatments_use_combined]

  if (dim(SWRunInformation)[2] == 1)
    stop("SWRunInformation might be tab separated instead of comma.")
  if (dim(sw_input_soillayers)[2] == 1)
    stop("SoilLayers might be tab separated instead of comma.")
  if (dim(sw_input_treatments)[2] == 1)
    stop("Treatments might be tab separated instead of comma.")
  if (dim(sw_input_experimentals)[2] == 1)
    stop("Experimentals might be tab separated instead of comma.")

  temp <- tryCatch(swsf_read_inputfile(file.path(dir.sw.dat, datafile.cloud)),
    error = print)
  sw_input_cloud_use <- temp[["use"]]
  sw_input_cloud <- temp[["data"]]

  temp <- tryCatch(swsf_read_inputfile(file.path(dir.sw.dat, datafile.prod)),
    error = print)
  sw_input_prod <- temp[["data"]]
  sw_input_prod_use <- temp[["use"]]
  sw_input_prod_use <- sw_input_prod_use | names(sw_input_prod_use) %in% create_experimentals	#update specifications based on experimental design

  temp <- tryCatch(swsf_read_inputfile(file.path(dir.sw.dat, datafile.siteparam)),
    error = print)
  sw_input_site <- temp[["data"]]
  sw_input_site_use <- temp[["use"]]
  sw_input_site_use <- sw_input_site_use | names(sw_input_site_use) %in% create_experimentals	#update specifications based on experimental design

  temp <- tryCatch(swsf_read_inputfile(file.path(dir.sw.dat, datafile.soils)),
    error = print)
  sw_input_soils <- temp[["data"]]
  sw_input_soils_use <- temp[["use"]]
  sw_input_soils_use <- sw_input_soils_use | names(sw_input_soils_use) %in% create_experimentals	#update specifications based on experimental design

  temp <- tryCatch(swsf_read_inputfile(file.path(dir.sw.dat, datafile.weathersetup)),
    error = print)
  sw_input_weather_use <- temp[["use"]]
  sw_input_weather <- temp[["data"]]

  temp <- tryCatch(swsf_read_inputfile(file.path(dir.sw.dat, datafile.climatescenarios)),
    error = print)
  sw_input_climscen_use <- temp[["use"]]
  sw_input_climscen <- temp[["data"]]

  temp <- tryCatch(swsf_read_inputfile(file.path(dir.sw.dat, datafile.climatescenarios_values)),
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
	save(SWRunInformation, include_YN, labels, sw_input_soillayers, sw_input_treatments_use, sw_input_treatments, sw_input_experimentals_use, sw_input_experimentals, create_experimentals, create_treatments, sw_input_cloud_use, sw_input_cloud, sw_input_prod_use, sw_input_prod, sw_input_site_use, sw_input_site, sw_input_soils_use, sw_input_soils, sw_input_weather_use, sw_input_weather, sw_input_climscen_use, sw_input_climscen, sw_input_climscen_values_use, sw_input_climscen_values, tr_files, tr_prod, tr_site, tr_soil, tr_weather, tr_cloud, tr_input_climPPT, tr_input_climTemp, tr_input_shiftedPPT, tr_input_EvapCoeff, tr_input_TranspCoeff_Code, tr_input_TranspCoeff, tr_input_TranspRegions, tr_input_SnowD, tr_VegetationComposition, param.species_regeneration, no.species_regeneration,
		file = file.path(dir.in, datafile.SWRWinputs_preprocessed),
		compress = FALSE) # No compression for fast access; RDS may be slightly faster, but would require loop over assign(, envir = .GlobalEnv)
}

if(!be.quiet) print(paste("SWSF reads input data: ended after",  round(difftime(Sys.time(), t1, units="secs"), 2), "s"))


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
# see ?iterators, ?it_exp, ?it_site, ?it_Pid
runsN_master <- nrow(SWRunInformation)
runIDs_sites <- which(include_YN > 0)
runsN_sites <- length(runIDs_sites)
if (!(runsN_sites > 0))
	stop(paste("at least 1 SoilWat-run needed for simulation, but", runsN_sites, "found"))

# identify how many SoilWat-runs = rows are to be carried out
expN <- NROW(sw_input_experimentals)
runsN_total <- runsN_sites * max(expN, 1L)
runsN_incl <- runsN_master * max(expN, 1L)
runsN_Pid <- runsN_incl * scenario_No

runIDs_total <- seq_len(runsN_total) # consecutive number of all (tr x exp) simulations to be executed
counter.digitsN <- 1 + ceiling(log10(runsN_master * max(expN, 1L)))	#max index digits
runIDs_todo <- runIDs_total[!(runIDs_total %in% runIDs_done)] # remove already completed runs from todo list
runsN_todo <- length(runIDs_todo)


#------outputing data
if(makeInputForExperimentalDesign) ExpInput_Seperator <- "X!X"

#append treatment information to the aggregated output in addition to selected Index_RunInformation
Index_RunInformation_Treatments <- NULL
if(length(create_treatments) > 0) {
	Index_RunInformation_Treatments <- match(create_treatments, names(sw_input_treatments))
}

daily_no <- length(output_aggregate_daily)
if(any(output_aggregate_daily == "SWAbulk") & length(SWPcrit_MPa) > 0){
  output_aggregate_daily <- output_aggregate_daily[-which(output_aggregate_daily == "SWAbulk")]
  for(icrit in seq(along=SWPcrit_MPa)){
    output_aggregate_daily <- c(output_aggregate_daily, paste("SWAbulkatSWPcrit", abs(round(-1000*SWPcrit_MPa[icrit], 0)), "kPa", sep=""))
  }
  daily_no <- length(output_aggregate_daily)

#	if(daily_lyr_agg[["do"]]){
#		aggLs_no <- 2 + ifelse(is.null(daily_lyr_agg[["third_cm"]]), 1, ifelse(!is.na(daily_lyr_agg[["third_cm"]]), 1, 0)) + ifelse(is.null(daily_lyr_agg[["fourth_cm"]]), 1, ifelse(!is.na(daily_lyr_agg[["fourth_cm"]]), 1, 0))
#	} else {#at this stage we don't know how many soil layers we will have among the SoilWat runs; so just prepare for the maximum
#		if(!any(create_treatments == "soilsin") & !is.null(sw_input_soillayers)){
#			aggLs_no <- max(apply(sw_input_soillayers[, -1], MARGIN=1, FUN=function(x) ifelse(is.na(x[1]), NA, findInterval(x[1] - sqrt(.Machine$double.neg.eps), c(0, na.exclude(unlist(x[-1]))))) ), na.rm=TRUE)
#		} else {
#			aggLs_no <- SoilLayer_MaxNo
#		}
#	}
}


#------------------------FLAGS FOR EXTERNAL DATA
temp <- matrix(data=do.ExtractExternalDatasets, ncol=2, nrow=length(do.ExtractExternalDatasets)/2, byrow=TRUE)
exinfo <- lapply(temp[, 2], function(x) as.logical(as.numeric(x)))
names(exinfo) <- temp[, 1]


exinfo$use_sim_spatial <-
	exinfo$ExtractSoilDataFromCONUSSOILFromSTATSGO_USA	||
	exinfo$ExtractSoilDataFromISRICWISEv12_Global		||
	exinfo$ExtractElevation_NED_USA						||
	exinfo$ExtractElevation_HWSD_Global					||
	exinfo$ExtractSkyDataFromNOAAClimateAtlas_USA		||
	exinfo$ExtractSkyDataFromNCEPCFSR_Global


#------------------------SPATIAL SETUP OF SIMULATIONS
if (exinfo$use_sim_spatial || any(actions == "map_input")) {
	if (any(!requireNamespace("rgdal"), !requireNamespace("sp"), !requireNamespace("raster"))) {
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
	stopifnot((temp <- rgdal::checkCRSArgs(as.character(sim_crs)))[[1]])
	sim_crs <- sp::CRS(temp[[2]])

	# SpatialPoints of simulation cell centers/sites in WGS84
	crs_sites <- sp::CRS("+init=epsg:4326")	# sp::CRS("+proj=longlat +datum=WGS84 +no_defs")
	run_sites <- sp::SpatialPoints(coords = with(SWRunInformation[runIDs_sites,], data.frame(X_WGS84, Y_WGS84)), proj4string = crs_sites)
}



#--------------------------------------------------------------------------------------------------#
#------------------------SET UP PARALLELIZATION
#used in: GriddedDailyWeatherFromNCEPCFSR_Global, external dataset extractions, loop calling do_OneSite, and ensembles

workersN <- 1
parallel_init <- FALSE
if(any(actions == "external") || (actionWithSoilWat && runsN_todo > 0) || do.ensembles){
	if(parallel_runs){
		if(!be.quiet) print(paste("SWSF prepares parallelization: started at", t1 <- Sys.time()))

		if(identical(parallel_backend, "mpi")) {
      Rmpi::mpi.spawn.Rslaves(nslaves = num_cores)

      .Last <- function() { #Properly end mpi slaves before quitting R (e.g., at a crash)
        # based on http://acmmac.acadiau.ca/tl_files/sites/acmmac/resources/examples/task_pull.R.txt
        if (is.loaded("mpi_initialize")) {
          if (isNamespaceLoaded("Rmpi") && Rmpi::mpi.comm.size(1) > 0)
            Rmpi::mpi.close.Rslaves()
          .Call("mpi_finalize")
        }
      }
		}

		if(identical(parallel_backend, "snow")){
			if(!be.quiet) setDefaultClusterOptions(outfile="")
			#cl <-  makeCluster(num_cores, type="MPI", outfile="")
			cl <- snow::makeSOCKcluster(num_cores)
			# Worker ID: this needs to be a .x object that does not get deleted with rm(list = ls())
			clusterApply(cl, seq_len(num_cores), function(x) .nodeNumber <<- x)
			#snow::clusterSetupRNG(cl) #random numbers setup
			doSNOW::registerDoSNOW(cl) 	# register foreach backend
		}

		if(identical(parallel_backend, "multicore")) {
			#stop("Only use snow on JANUS, because multicore cannot access cores outside master node")
			registerDoMC(num_cores)
		}

    workersN <- if (identical(parallel_backend, "mpi")) {
        Rmpi::mpi.comm.size() - 1
      } else {
        foreach::getDoParWorkers()
      }

		parallel_init <- TRUE
		if(!be.quiet) print(paste("SWSF prepares parallelization: initialization of", workersN, "workers ended after",  round(difftime(Sys.time(), t1, units="secs"), 2), "s"))
	}
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
}


#------------------------DAILY WEATHER
if (extract_determine_database == "SWRunInformation" && "dailyweather_source" %in% colnames(SWRunInformation)) {
	sites_dailyweather_source <- factor(SWRunInformation$dailyweather_source[runIDs_sites], levels=dailyweather_options)
	do_weather_source <- anyNA(sites_dailyweather_source)
} else {
	sites_dailyweather_source <- factor(rep(NA, runsN_sites), levels=dailyweather_options)
	do_weather_source <- TRUE
}

weather.digits <- 2


if(exinfo$GriddedDailyWeatherFromMaurer2002_NorthAmerica){
	#extract daily weather information for the grid cell coded by latitude/longitude for each simulation run
	#Citation: Maurer, E. P., A. W. Wood, J. C. Adam, D. P. Lettenmaier, and B. Nijssen. 2002. A long-term hydrologically based dataset of land surface fluxes and states for the conterminous United States. Journal of Climate 15:3237-3251.

	dir.ex.maurer2002 <- file.path(dir.ex.weather, "Maurer+_2002updated", "DAILY_FORCINGS")
	stopifnot(file.exists(dir.ex.maurer2002))
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
}

if(exinfo$GriddedDailyWeatherFromNRCan_10km_Canada && createAndPopulateWeatherDatabase){
	#Citations:
	#	- Hopkinson, R. F., D. W. McKenney, E. J. Milewska, M. F. Hutchinson, P. Papadopol, and L. A. Vincent. 2011. Impact of Aligning Climatological Day on Gridding Daily Maximum–Minimum Temperature and Precipitation over Canada. Journal of Applied Meteorology and Climatology 50:1654-1665.
	#	- Hutchinson, M. F., D. W. McKenney, K. Lawrence, J. H. Pedlar, R. F. Hopkinson, E. Milewska, and P. Papadopol. 2009. Development and Testing of Canada-Wide Interpolated Spatial Models of Daily Minimum–Maximum Temperature and Precipitation for 1961–2003. Journal of Applied Meteorology and Climatology 48:725-741.
	#	- McKenney, D. W., M. F. Hutchinson, P. Papadopol, K. Lawrence, J. Pedlar, K. Campbell, E. Milewska, R. F. Hopkinson, D. Price, and T. Owen. 2011. Customized Spatial Climate Models for North America. Bulletin of the American Meteorological Society 92:1611-1622.
	dir.ex.NRCan <- file.path(dir.ex.weather, "NRCan_10km_Canada", "DAILY_GRIDS")
	stopifnot(file.exists(dir.ex.NRCan), require(raster), require(sp), require(rgdal))
}



if (do_weather_source) {
  lwf_cond1 <- sw_input_treatments_use["LookupWeatherFolder"] &&
                !anyNA(sw_input_treatments$LookupWeatherFolder[runIDs_sites])
  lwf_cond2 <- !anyNA(SWRunInformation$WeatherFolder[runIDs_sites]) &&
                !any(grepl("GriddedDailyWeatherFrom", names(exinfo)[unlist(exinfo)]))
  lwf_cond3 <- sw_input_experimentals_use["LookupWeatherFolder"] &&
                !anyNA(sw_input_treatments$LookupWeatherFolder)
  lwf_cond4 <- any(create_treatments == "LookupWeatherFolder")

	#Functions to determine sources of daily weather; they write to global 'sites_dailyweather_source' and 'sites_dailyweather_names', i.e., the last entry is the one that will be used
	dw_LookupWeatherFolder <- function(sites_dailyweather_source) {
		if (any(lwf_cond1, lwf_cond2, lwf_cond3, lwf_cond4)) {
			# Check which requested lookup weather folders are available
			pwd <- getwd()
			setwd(file.path(dir.sw.in.tr, "LookupWeatherFolder"))
			there <- rep(FALSE, times = runsN_sites)
			if (lwf_cond1)
				there <- there | sapply(runIDs_sites, FUN=function(ix) if(!is.na(sw_input_treatments$LookupWeatherFolder[ix])) file.exists(sw_input_treatments$LookupWeatherFolder[ix]) else FALSE)
			if (lwf_cond2)
				there <- there | sapply(runIDs_sites, FUN=function(ix) if(!is.na(SWRunInformation$WeatherFolder[ix])) file.exists(SWRunInformation$WeatherFolder[ix]) else FALSE)
			if (lwf_cond3)
				there <- there | rep(any(sapply(sw_input_experimentals$LookupWeatherFolder, FUN=function(ix) file.exists(sw_input_experimentals$LookupWeatherFolder))), times = runsN_sites)
			setwd(pwd)
			if (any(there))
				sites_dailyweather_source[there] <<- "LookupWeatherFolder"

			if (!be.quiet)
			  print(paste("Data for", sum(there), "sites will come from 'LookupWeatherFolder'"))
		}

		sites_dailyweather_source
	}

	dw_Maurer2002_NorthAmerica <- function(sites_dailyweather_source) {
		if(exinfo$GriddedDailyWeatherFromMaurer2002_NorthAmerica){
			# Check which requested Maurer weather data are available
			there <- simstartyr >= 1949 && endyr <= 2010
			if (any(there)) {
			  Maurer <- with(SWRunInformation[runIDs_sites, ], create_filename_for_Maurer2002_NorthAmerica(X_WGS84, Y_WGS84))
			  there <- vapply(Maurer, function(im) file.exists(file.path(dir.ex.maurer2002, im)), FUN.VALUE = NA)
        if (any(there)) {
          sites_dailyweather_source[there] <- "Maurer2002_NorthAmerica"
          sites_dailyweather_names[there] <- paste0(SWRunInformation$Label[runIDs_sites][there], "_", Maurer[there])
        }
      }
			if (!be.quiet)
			  print(paste("Data for", sum(there), "sites will come from 'Maurer2002_NorthAmerica'"))
		}

		sites_dailyweather_source
	}

	dw_DayMet_NorthAmerica <- function(sites_dailyweather_source) {
		if (exinfo$GriddedDailyWeatherFromDayMet_NorthAmerica) {
			# Check which of the DayMet weather data are available
			#	- Temperature: 2-meter air temperature in Celsius degrees
			#	- Precipitation: mm/day; Daily total precipitation in millimeters per day, sum of all forms converted to water-equivalent. Precipitation occurrence on any given day may be ascertained.
			#	- Grids domain v2: -131.104 -52.95  52.00 14.53
			#	- Grids domain v3: -179     -52     83    14
			#	- Grids: Geographic Coordinate Reference: WGS_1984; Projection: Lambert Conformal Conic
			#	- Cells size: 1000 x 1000 m
			#	- All Daymet years, including leap years, have 1 - 365 days. For leap years, the Daymet database includes leap day. Values for December 31 are discarded from leap years to maintain a 365-day year.
			there <- simstartyr >= 1980 && endyr <= as.POSIXlt(Sys.time(), tz = "UTC")$year+1900 - 1
			if (any(there)) {
        there <- (SWRunInformation[runIDs_sites, "X_WGS84"] >= -179 &
                  SWRunInformation[runIDs_sites, "X_WGS84"] <= -5) &
                (SWRunInformation[runIDs_sites, "Y_WGS84"] >= 14 &
                  SWRunInformation[runIDs_sites, "Y_WGS84"] <= 83)
        if (any(there)) {
          sites_dailyweather_source[there] <- "DayMet_NorthAmerica"
          sites_dailyweather_names[there] <- with(SWRunInformation[runIDs_sites[there], ], paste0(Label, "_DayMet", formatC(X_WGS84, digits=4, format="f"), "_", formatC(Y_WGS84, digits=4, format="f")))
        }
      }
			if (!be.quiet)
			  print(paste("Data for", sum(there), "sites will come from 'DayMet_NorthAmerica'"))
		}

		sites_dailyweather_source
	}

	dw_NRCan_10km_Canada <- function(sites_dailyweather_source) {
		if (exinfo$GriddedDailyWeatherFromNRCan_10km_Canada) {
			# Check which of the NRCan weather data are available
			#	- Temperature: Celsius degrees
			#	- Precipitation: mm
			#	- Grids domain: 141.00 to 52.00 W, 41.00 to 83.00 N
			#	- Grids datum: geographic NAD83
			#	- Columns: 1068, Rows: 510, Cells size: 0.083333333
			there <- simstartyr >= 1950 && endyr <= 2013
			if (any(there)) {
        nrc_test <- raster::raster(file.path(dir.ex.NRCan, "1950", "max1950_1.asc"))
        raster::CRS(nrc_test) <- raster::CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0") #	see http://spatialreference.org/ref/epsg/4269/
        sp_locs <- sp::SpatialPoints(coords = SWRunInformation[runIDs_sites, c("X_WGS84", "Y_WGS84")], proj4string = CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
        there <- !is.na(raster::extract(nrc_test, y = spTransform(sp_locs, CRSobj = raster::CRS(projection(nrc_test)))))
        if (any(there)) {
          sites_dailyweather_source[there] <<- "NRCan_10km_Canada"
          sites_dailyweather_names[there] <<- with(SWRunInformation[runIDs_sites[there], ], paste0(Label, "_NRCan", formatC(X_WGS84, digits=4, format="f"), "_", formatC(Y_WGS84, digits=4, format="f")))
        }
      }
			if(!be.quiet) print(paste("Data for", sum(there), "sites will come from 'NRCan_10km_Canada'"))
		}

		sites_dailyweather_source
	}

	dw_NCEPCFSR_Global <- function(sites_dailyweather_source) {
		if(exinfo$GriddedDailyWeatherFromNCEPCFSR_Global){
			# Check which of the NCEPCFSR_Global weather data are available
			#	- Grids domain: 0E to 359.688E and 89.761N to 89.761S
			there <- simstartyr >= 1979 && endyr <= 2010
			if (any(there)) {
        there <- (SWRunInformation[runIDs_sites, "X_WGS84"] >= 0 - 180 & SWRunInformation[runIDs_sites, "X_WGS84"] <= 360 - 180) & (SWRunInformation[runIDs_sites, "Y_WGS84"] >= -89.761 & SWRunInformation[runIDs_sites, "Y_WGS84"] <= 89.761)
        if (any(there)) {
          sites_dailyweather_source[there] <- "NCEPCFSR_Global"
          sites_dailyweather_names[there] <- with(SWRunInformation[runIDs_sites[there], ], paste0(Label, "_CFSR", formatC(X_WGS84, digits=4, format="f"), "_", formatC(Y_WGS84, digits=4, format="f")))
        }
      }
			if (!be.quiet)
			  print(paste("Data for", sum(there), "sites will come from 'NCEPCFSR_Global'"))
		}

		sites_dailyweather_source
	}

	#Determine order of priorities (highest priority comes last)
	sites_dailyweather_names <- rep(NA, times = length(sites_dailyweather_source))
	dailyweather_priorities <- rev(paste("dw", dailyweather_options, sep = "_"))
	for (idw in dailyweather_priorities)
	  sites_dailyweather_source <- get(idw)(sites_dailyweather_source)


	if(anyNA(sites_dailyweather_source)){
		if(FALSE){# remove sites with no weather; code to run by hand
			xy <- SpatialPoints(coords=t(sapply(strsplit(list.files(dir.ex.maurer2002), "_"), FUN=function(x) as.numeric(x[3:2]))), proj4string=CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
			sp_locs2 <- spTransform(sp_locs, CRSobj=CRS(projection(nrc_test)))

			plot(sp_locs2, pch=19, cex=0.5, col=c("blue", "red", "green", "purple", "black")[ifelse(is.na(sites_dailyweather_source),5,sites_dailyweather_source)])
			plot(nrc_test, col=adjustcolor("orange", alpha.f=0.5), add=TRUE)
			if(require(maps)) map("state", add=TRUE)
			plot(xy, col=adjustcolor("darkgray", alpha.f=0.5), lwd=1, add=TRUE)

			id_remove <- which(is.na(sites_dailyweather_source))
			if(!be.quiet) print(paste("There are no daily weather data for", length(id_remove), "sites"))
			SWRunInformation$Include_YN[runIDs_sites][id_remove] <- 0
			write.csv(SWRunInformation, file=file.path(dir.in, datafile.SWRunInformation), row.names=FALSE)
			unlink(file.path(dir.in, datafile.SWRWinputs_preprocessed))

			stop(paste("Restart code because master file", datafile.SWRunInformation, "has changed"))
		}
	}

	# Save information about weather source to disk file
	sites_dailyweather_names <- gsub("[[:space:]]", "", sites_dailyweather_names)
	SWRunInformation$WeatherFolder[runIDs_sites][!is.na(sites_dailyweather_names)] <- na.exclude(sites_dailyweather_names)
	SWRunInformation$dailyweather_source[runIDs_sites] <- as.character(sites_dailyweather_source)
	write.csv(SWRunInformation, file=file.path(dir.in, datafile.SWRunInformation), row.names=FALSE)
	unlink(file.path(dir.in, datafile.SWRWinputs_preprocessed))
}


#------------------------CHECK THAT DAILY WEATHER DATA IS AVAILABLE
if(anyNA(sites_dailyweather_source)){
	stop("There are sites without daily weather. Provide data for all runs")
}

if (exinfo$ExtractClimateChangeScenarios) {
	getScenarioWeatherDataFromDatabase <- TRUE
	getCurrentWeatherDataFromDatabase <- TRUE
}
if(getScenarioWeatherDataFromDatabase)
	getCurrentWeatherDataFromDatabase <- TRUE

if(getCurrentWeatherDataFromDatabase){
	if(!(createAndPopulateWeatherDatabase || file.exists(dbWeatherDataFile)))
		stop("Create or use existing Weather database with Scenario data inside.")
} else {
	if(!any(create_treatments == "LookupWeatherFolder", exinfo$GriddedDailyWeatherFromMaurer2002_NorthAmerica, exinfo$GriddedDailyWeatherFromDayMet_NorthAmerica))
		stop("Daily weather data must be provided through 'LookupWeatherFolder', 'Maurer2002_NorthAmerica', or 'DayMet_NorthAmerica' since no weather database is used")
}


#------ Create the Database and Tables within
if(!be.quiet) print(paste("SWSF sets up the database: started at", t1 <- Sys.time()))

name.OutputDB <- file.path(dir.out, "dbTables.sqlite3")
if(copyCurrentConditionsFromDatabase | copyCurrentConditionsFromTempSQL) name.OutputDBCurrent <- file.path(dir.out, "dbTables_current.sqlite3")
setwd(dir.prj)
source(file.path(dir.code, "R", "2_SWSF_p2of5_CreateDB_Tables_v51.R"), verbose = FALSE, chdir = FALSE)

con <- DBI::dbConnect(RSQLite::SQLite(), dbname=name.OutputDB)

if (getCurrentWeatherDataFromDatabase || getScenarioWeatherDataFromDatabase) {
	# Check that version of dbWeather suffices
	dbW_setConnection(dbWeatherDataFile)
	v_dbW <- dbW_version()

	if (v_dbW < minVersion_dbWeather) {
		print(paste0("The version (", v_dbW, ") of the daily weather database is outdated; min. version required: ", minVersion_dbWeather))
		if (v_dbW >= "1") print("Use function 'Rsoilwat31:::dbW_upgrade_v1to2' to upgrade your version 1.y.z weather database to version 2.0.0")
		stop("Outdated weather database")
	}

}

if(!be.quiet) print(paste("SWSF sets up the database: ended after",  round(difftime(Sys.time(), t1, units="secs"), 2), "s"))

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
	setwd(dir.prj)

	if(!be.quiet) print(paste("SWSF extracts information from external datasets prior to simulation runs: started at", t1 <- Sys.time()))
	stopifnot(file.exists(dir.external))

	source(file.path(dir.code, "R", "2_SWSF_p3of5_ExternalDataExtractions_v51.R"), verbose = FALSE, chdir = FALSE)
  do_check_include <- TRUE

	if(!be.quiet) print(paste("SWSF extracts information from external datasets prior to simulation runs: ended after",  round(difftime(Sys.time(), t1, units="secs"), 2), "s"))
}



#--------------------------------------------------------------------------------------------------#
#------------------------CHECK THAT INCLUDE_YN* ARE INCLUSIVE
if (do_check_include) {
	includes_all_sources <- grep("Include_YN", colnames(SWRunInformation), ignore.case = TRUE, value = TRUE)
	includes_sources <- includes_all_sources[-which(includes_all_sources == "Include_YN")]
	if (length(includes_sources) > 0L) {
		include_YN_sources <- apply(SWRunInformation[, includes_sources, drop = FALSE], 1, function(x) all(x > 0L))

		if (all(include_YN_sources[include_YN > 0L])) {
			if(!be.quiet) print(paste("Data sources available for all requested SWSF simulation runs"))

		} else {
			include_YN_available <- rep(0, runsN_master)
			include_YN_available[include_YN_sources] <- 1
			SWRunInformation$include_YN_available <- include_YN_available

			write.csv(SWRunInformation, file = file.path(dir.in, datafile.SWRunInformation), row.names = FALSE)
			unlink(file.path(dir.in, datafile.SWRWinputs_preprocessed))
			rm(include_YN_available)

			stop("Data sources not available for every requested SWSF simulation run. ",
				"New column 'include_YN_available' with updated information stored to MasterInput file 'SWRunInformation' on disk. ",
				"SWSF is stopped so that you can bring 'include_YN' and 'include_YN_available' in agreement before running the simulations.")
		}

		rm(include_YN_sources)
	}

	rm(includes_all_sources, includes_sources)
}
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
#------------------------CALCULATIONS PRIOR TO SIMULATION RUNS TO CREATE THEM

temp <- matrix(data=do.PriorCalculations, ncol=2, nrow=length(do.PriorCalculations)/2, byrow=TRUE)
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
    print(paste("SWSF makes calculations prior to simulation runs: started at", t1 <- Sys.time()))

    runIDs_adjust <- seq_len(runsN_master)  # if not all, then runIDs_sites

  if (pcalcs$ExtendSoilDatafileToRequestedSoilLayers) {
    if (!be.quiet)
      print(paste(Sys.time(), "'InterpolateSoilDatafileToRequestedSoilLayers' of", paste0(requested_soil_layers, collapse = ", "), "cm"))
    # How to add different soil variables
    sl_vars_mean <- c("Matricd", "GravelContent", "Sand", "Clay", "SoilTemp") # values will be interpolated
    sl_vars_sub <- c("EvapCoeff", "TranspCoeff", "Imperm") # values will be exhausted

    # Requested layers
    requested_soil_layers <- as.integer(round(requested_soil_layers))
    stopifnot(requested_soil_layers > 0, diff(requested_soil_layers) > 0)
    req_sl_ids <- paste0(requested_soil_layers, collapse = "x")

    # Available layers
    ids_depth <- strsplit(names(sw_input_soils_use)[sw_input_soils_use], "_", fixed = TRUE)
    stopifnot(length(ids_depth) > 0)
    var_layers <- unique(sapply(ids_depth, function(x) paste0(x[-length(x)], collapse = "_")))
    ids_depth2 <- unique(sapply(ids_depth, function(x) x[length(x)]))
    use_layers <- paste0("depth_", ids_depth2)

    layers_depth <- round(as.matrix(sw_input_soillayers[runIDs_adjust, use_layers, drop = FALSE]))
    i_nodata <- apply(is.na(layers_depth), 1, all)
    if (any(i_nodata)) {
      layers_depth <- layers_depth[!i_nodata, ]
      runIDs_adjust_ws <- runIDs_adjust[!i_nodata]
    } else {
      runIDs_adjust_ws <- runIDs_adjust
    }
    i_nodata <- apply(is.na(layers_depth), 2, all)
    if (any(i_nodata))
      layers_depth <- layers_depth[, !i_nodata]
    ids_layers <- seq_len(dim(layers_depth)[2])
    avail_sl_ids <- apply(layers_depth, 1, paste0, collapse = "x")

    # Loop through runs with same layer profile and adjust
    layer_sets <- unique(avail_sl_ids)
    if (length(layer_sets) > 0) {
      has_changed <- FALSE
      sw_input_soils_data <- lapply(var_layers, function(x)
        as.matrix(sw_input_soils[runIDs_adjust_ws, grep(x, names(sw_input_soils))[ids_layers]]))

      for (ils in seq_along(layer_sets)) {
        il_set <- avail_sl_ids == layer_sets[ils]
        if (sum(il_set, na.rm = TRUE) == 0) next

        # Identify which requested layers to add
        ldset <- na.exclude(layers_depth[which(il_set)[1], ])
        req_sl_toadd <- setdiff(requested_soil_layers, ldset)
        req_sd_toadd <- req_sl_toadd[req_sl_toadd < max(ldset)]
        if (length(req_sd_toadd) == 0) next

        # Add identified layers
        sw_input_soils_data2 <- lapply(seq_along(var_layers), function(iv)
          sw_input_soils_data[[iv]][il_set, ])

        for (lnew in req_sd_toadd) {
          ilnew <- findInterval(lnew, ldset)
          il_weight <- abs(lnew - ldset[ilnew + 1:0])
          sw_input_soils_data2 <- lapply(seq_along(var_layers), function(iv)
            add_layer_to_soil(sw_input_soils_data2[[iv]], il = ilnew, w = il_weight,
              method = if (var_layers[iv] %in% sl_vars_sub) "exhaust" else "interpolate"))
          ldset <- sort(c(ldset, lnew))
        }

        # Update soil datafiles
        lyrs <- seq_along(ldset)
        for (iv in seq_along(var_layers)) {
          i.temp <- grep(var_layers[iv], names(sw_input_soils_use))[lyrs]
          sw_input_soils[runIDs_adjust_ws[il_set], i.temp] <-
            round(sw_input_soils_data2[[iv]][, lyrs], if (var_layers[iv] %in% sl_vars_sub) 4L else 2L)
          sw_input_soils_use[i.temp] <- TRUE
        }

        sw_input_soillayers[runIDs_adjust_ws[il_set],
          grep("depth_", names(sw_input_soillayers))[lyrs]] <- matrix(ldset, nrow = sum(il_set), ncol = length(ldset), byrow = TRUE)
        has_changed <- TRUE
      }

      if (has_changed) {
        #write data to datafile.soillayers
        write.csv(sw_input_soillayers, file = file.path(dir.in, datafile.soillayers), row.names = FALSE)
        #write data to datafile.soils
        write.csv(reconstitute_inputfile(sw_input_soils_use, sw_input_soils),
          file = file.path(dir.sw.dat, datafile.soils), row.names = FALSE)
        unlink(file.path(dir.in, datafile.SWRWinputs_preprocessed))

        print("'InterpolateSoilDatafileToRequestedSoilLayers': don't forget to adjust lookup tables with per-layer values if applicable for this project")
      }

      rm(sw_input_soils_data, sw_input_soils_data2)
    }

    if(!be.quiet) print(paste(Sys.time(), "completed 'InterpolateSoilDatafileToRequestedSoilLayers'"))
  }

  if (pcalcs$CalculateBareSoilEvaporationCoefficientsFromSoilTexture) {
    #calculate bare soil evaporation coefficients per soil layer for each simulation run and copy values to 'datafile.soils'
    # soil texture influence based on re-analysis of data from Wythers KR, Lauenroth WK, Paruelo JM (1999) Bare-Soil Evaporation Under Semiarid Field Conditions. Soil Science Society of America Journal, 63, 1341-1349.

    if (!be.quiet)
      print(paste(Sys.time(), "'CalculateBareSoilEvaporationCoefficientsFromSoilTexture'"))

    depth_max_bs_evap <- 15	# max = 15 cm: Torres EA, Calera A (2010) Bare soil evaporation under high evaporation demand: a proposed modification to the FAO-56 model. Hydrological Sciences Journal-Journal Des Sciences Hydrologiques, 55, 303-315.

    icol_bsE <- grep("EvapCoeff", names(sw_input_soils_use))
    icol_sand <- grep("Sand_L", names(sw_input_soils_use))
    icol_clay <- grep("Clay_L", names(sw_input_soils_use))
    use_layers <- which(sw_input_soils_use[icol_sand] & sw_input_soils_use[icol_clay])
    stopifnot(length(use_layers) > 0)

    do_calc <- TRUE
    if (continueAfterAbort) {
      temp <- icol_bsE[use_layers]
      icols <- temp[sw_input_soils_use[temp]]
      if (length(icols) > 0L) {
        do_calc <- !all(rowSums(sw_input_soils[runIDs_adjust, icols, drop = FALSE], na.rm = TRUE) > 0)
      }
    }

    if (do_calc) {
      layers_depth <- as.matrix(sw_input_soillayers[runIDs_adjust, grep("depth_L", names(sw_input_soillayers))[use_layers], drop = FALSE])
      depth_min_bs_evap <- min(layers_depth[, 1])
      stopifnot(na.exclude(depth_min_bs_evap < depth_max_bs_evap))

      lyrs_max_bs_evap <- t(apply(layers_depth, 1, function(x) {
        xdm <- depth_max_bs_evap - x
        i0 <- abs(xdm) < tol
        ld <- if (any(i0, na.rm = TRUE)) {
          which(i0)
        } else {
          temp <- which(xdm < 0)
          if (length(temp) > 0) temp[1] else length(x)
        }
        c(diff(c(0, x))[seq_len(ld)], rep(0L, length(x) - ld))
      }))
      ldepth_max_bs_evap <- rowSums(lyrs_max_bs_evap)

      #TODO: add influence of gravel
      sand <- sw_input_soils[runIDs_adjust, icol_sand, drop = FALSE]
      clay <- sw_input_soils[runIDs_adjust, icol_clay, drop = FALSE]
      sand_mean <- rowSums(lyrs_max_bs_evap * sand, na.rm = TRUE) / ldepth_max_bs_evap
      clay_mean <- rowSums(lyrs_max_bs_evap * clay, na.rm = TRUE) / ldepth_max_bs_evap

      temp_depth <- 4.1984 + 0.6695 * sand_mean ^ 2 + 168.7603 * clay_mean ^ 2 # equation from re-analysis
      depth_bs_evap <- pmin(pmax(temp_depth, depth_min_bs_evap, na.rm = TRUE), depth_max_bs_evap, na.rm = TRUE)
      lyrs_bs_evap <- t(apply(depth_bs_evap - layers_depth, 1, function(x) {
        i0 <- abs(x) < tol
        ld <- if (any(i0, na.rm = TRUE)) {
          which(i0)
        } else {
          temp <- which(x < 0)
          if (length(temp) > 0) temp[1] else sum(!is.na(x))
        }
        c(rep(TRUE, ld), rep(FALSE, length(x) - ld))
      }))

      temp_coeff <- 1 - exp(- 5 * layers_depth / depth_bs_evap)	# function made up to match previous cummulative distributions
      temp_coeff[!lyrs_bs_evap | is.na(temp_coeff)] <- 1
      coeff_bs_evap <- round(t(apply(cbind(0, temp_coeff), 1, diff)), 4)
      coeff_bs_evap <- coeff_bs_evap / rowSums(coeff_bs_evap, na.rm = TRUE)

      #add data to sw_input_soils and set the use flags
      icol <- seq_len(sum(apply(coeff_bs_evap, 2, function(x) any(x > tol))))
      icols_bsE_used <- icol_bsE[icol]
      icols_bse_notused <- icol_bsE[-icol]

      sw_input_soils_use[icols_bsE_used] <- TRUE
      sw_input_soils[runIDs_adjust, icols_bsE_used] <- round(coeff_bs_evap[, icol], 4)

      sw_input_soils_use[icols_bse_notused] <- FALSE
      sw_input_soils[runIDs_adjust, icols_bse_notused] <- 0

      #write data to datafile.soils
      write.csv(reconstitute_inputfile(sw_input_soils_use, sw_input_soils),
        file = file.path(dir.sw.dat, datafile.soils), row.names = FALSE)
      unlink(file.path(dir.in, datafile.SWRWinputs_preprocessed))

      rm(icol, icols_bsE_used, icols_bse_notused, coeff_bs_evap, temp_coeff,
        lyrs_bs_evap, depth_bs_evap, temp_depth, ldepth_max_bs_evap, sand, clay, sand_mean,
        clay_mean, depth_min_bs_evap, layers_depth)
    }

    rm(depth_max_bs_evap, icol_bsE, icol_sand, icol_clay, use_layers, do_calc)

    if (!be.quiet)
      print(paste(Sys.time(), "completed 'CalculateBareSoilEvaporationCoefficientsFromSoilTexture'"))
  }

	#------used during each simulation run: define functions here
	if(any(actions == "create") && pcalcs$EstimateConstantSoilTemperatureAtUpperAndLowerBoundaryAsMeanAnnualAirTemperature){
		sw_input_site_use["SoilTempC_atLowerBoundary"] <- TRUE #set use flag
		sw_input_site_use["SoilTempC_atUpperBoundary"] <- TRUE
		#call function 'SiteClimate' in each SoilWat-run
	}

	if(any(actions == "create") && pcalcs$EstimateInitialSoilTemperatureForEachSoilLayer){
		#set use flags
		ld <- seq_len(SoilLayer_MaxNo)
		use.layers <- which(sw_input_soils_use[paste0("Sand_L", ld)])
		index.soilTemp <- paste0("SoilTemp_L", ld)[use.layers]
		soilTemp <- sw_input_soils[runIDs_adjust, index.soilTemp, drop = FALSE]
		sw_input_soils_use[index.soilTemp] <- TRUE
	}

	if(!be.quiet) print(paste("SWSF makes calculations prior to simulation runs: ended after",  round(difftime(Sys.time(), t1, units="secs"), 2), "s"))
}


#--------------------------------------------------------------------------------------------------#
#------------------------OBTAIN INFORMATION FROM TABLES PRIOR TO SIMULATION RUNS TO CREATE THEM

if (any(actions == "create")) {
  if (!be.quiet)
    print(paste("SWSF obtains information prior to simulation runs: started at", t1 <- Sys.time()))

  if (any(create_treatments %in% c("LookupEvapCoeffFromTable",
                                   "LookupTranspRegionsFromTable",
                                   "LookupSnowDensityFromTable"))) {

    do_prior_lookup <- list(
      LookupEvapCoeffFromTable = list(
        flag = "LookupEvapCoeffFromTable",
        pattern = "EvapCoeff",
        tr_input = tr_input_EvapCoeff,
        sw_input_use = "sw_input_soils_use",
        sw_input = "sw_input_soils",
        nvars = SoilLayer_MaxNo,
        do_fill = FALSE,
        datafile = file.path(dir.sw.dat, datafile.soils)),

      LookupTranspRegionsFromTable = list(
        flag = "LookupTranspRegionsFromTable",
        pattern = "TranspRegion",
        tr_input = tr_input_TranspRegions,
        sw_input_use = "sw_input_soils_use",
        sw_input = "sw_input_soils",
        nvars = SoilLayer_MaxNo,
        do_fill = FALSE,
        datafile = file.path(dir.sw.dat, datafile.soils)),

      LookupSnowDensityFromTable = list(
        flag = "LookupSnowDensityFromTable",
        pattern = "(snowd)|(SnowD_Hemisphere)",
        tr_input = tr_input_SnowD,
        sw_input_use = "sw_input_cloud_use",
        sw_input = "sw_input_cloud",
        nvars = 12 + 1,
        do_fill = TRUE,
        fill_pattern = "snowd",
        fill_value = 76,  	# 76 kg/m3 = median of medians over 6 sites in Colorado and Wyoming: Judson, A. & Doesken, N. (2000) Density of Freshly Fallen Snow in the Central Rocky Mountains. Bulletin of the American Meteorological Society, 81, 1577-1587.
        datafile = file.path(dir.sw.dat, datafile.cloud))
    )

    done_prior <- rep(FALSE, length(do_prior_lookup))
    names(done_prior) <- names(do_prior_lookup)

    for (pc in do_prior_lookup) {
      if (any(create_treatments == pc$flag)) {
        #lookup values per category for each simulation run and copy values to datafile
        temp <- sw_input_experimentals_use[pc$flag]
        if (!temp || (temp && (length(unique(sw_input_experimentals[, pc$flag])) == 1L))) {
          # Lookup prior to do_OneSite() only if option is off in sw_input_experimentals or constant

          if (continueAfterAbort) {
            # Determine whether lookup already carried out and stored to file
            sw_input_use <- get(pc$sw_input_use)

            icols <- grep(pc$pattern, names(sw_input_use))
            icols <- icols[sw_input_use[icols]]
            temp <- get(pc$sw_input)[, icols, drop = FALSE]

            if (all(!apply(is.na(temp), 2, all))) {
              # if no layer has only NAs for which the _use flag is on, then consider as completed
              done_prior[pc$flag] <- TRUE
              next
            }
          }

          if (!be.quiet)
            print(paste(Sys.time(), ": performing", shQuote(pc$flag)))

          trtype <- if (sw_input_experimentals_use[pc$flag]) {
            unique(sw_input_experimentals[, pc$flag])
          } else {
            sw_input_treatments[, pc$flag]
          }

          if (any(is.na(trtype)))
            stop("ERROR: ", pc$flag, " column cannot have any NAs.")
          if (!all(unique(trtype) %in% rownames(pc$tr_input)))
            stop("ERROR: ", pc$flag, " column values do not match up with trfile.", pc$flag, " row names.")

          tempdat <- try(get.LookupFromTable(
            pattern = pc$pattern,
            trtype = trtype,
            tr_input = pc$tr_input,
            sw_input_use = get(pc$sw_input_use),
            sw_input = get(pc$sw_input),
            nvars = pc$nvars))

          done_prior[pc$flag] <- !inherits(tempdat, "try-error")
          if (done_prior[pc$flag]) {
            if (!is.null(pc$do_fill) && pc$do_fill)
              tempdat <- fill_empty(tempdat, pattern = pc$fill_pattern, fill = pc$fill_value)

            assign(pc$sw_input_use, tempdat$sw_input_use, envir = .GlobalEnv)
            assign(pc$sw_input, tempdat$sw_input, envir = .GlobalEnv)

            #write data to datafile
            write.csv(reconstitute_inputfile(tempdat$sw_input_use, tempdat$sw_input),
              file = pc$datafile, row.names = FALSE)
            unlink(file.path(dir.in, datafile.SWRWinputs_preprocessed))
          }

        } else {
          done_prior[pc$flag] <- FALSE
        }
      }
    }

    rm(do_prior_lookup)
  }

	if(!be.quiet) print(paste("SWSF obtains information prior to simulation runs: ended after",  round(difftime(Sys.time(), t1, units="secs"), 2), "s"))
}



#--------------------------------------------------------------------------------------------------#
#------------------------MAP INPUT VARIABLES (FOR QUALITY CONTROL)
if (any(actions == "map_input") && length(map_vars) > 0) {
	if (!be.quiet) print(paste("SWSF generates maps of input variables for quality control: started at", t1 <- Sys.time()))
	dir.create(dir.inmap <- file.path(dir.out, "Input_maps"), showWarnings = FALSE)

	input_avail <- list(SWRunInformation = list(cols = names(SWRunInformation), use = rep(TRUE, ncol(SWRunInformation))),
						sw_input_soillayers = list(cols = names(sw_input_soillayers), use = rep(TRUE, ncol(sw_input_soillayers))),
						sw_input_cloud = list(cols = names(sw_input_cloud), use = sw_input_cloud_use),
						sw_input_prod = list(cols = names(sw_input_prod), use = sw_input_prod_use),
						sw_input_site = list(cols = names(sw_input_site), use = sw_input_site_use),
						sw_input_soils = list(cols = names(sw_input_soils), use = sw_input_soils_use),
						sw_input_weather = list(cols = names(sw_input_weather), use = sw_input_weather_use),
						sw_input_climscen = list(cols = names(sw_input_climscen), use = sw_input_climscen_use),
						sw_input_climscen_values = list(cols = names(sw_input_climscen_values), use = sw_input_climscen_use)
					)

	for (iv in seq_along(map_vars)) {
		iv_locs <- lapply(input_avail, function(ina) grep(map_vars[iv], ina$cols[ina$use], ignore.case = TRUE, value = TRUE))
		iv_locs <- iv_locs[lengths(iv_locs) > 0]

		if (length(iv_locs) > 0) {
			dir.create(dir.inmapvar <- file.path(dir.inmap, map_vars[iv]), showWarnings = FALSE)

			for (it1 in seq_along(iv_locs)) for (it2 in seq_along(iv_locs[[it1]])) {
				dat <- get(names(iv_locs)[it1])[runIDs_sites, iv_locs[[it1]][it2]]
				dat <- try(as.numeric(dat), silent = TRUE) # e.g., sw_input_cloud[, "SnowD_Hemisphere"] contains only strings for which as.numeric() issues a warning

        # this code plots only numeric maps
				if (any(is.finite(dat)) && !inherits(dat, "try-error")) {
					names(dat) <- iv_locs[[it1]][it2]

					map_flag <- paste(names(iv_locs)[it1], iv_locs[[it1]][it2], sim_cells_or_points, sep = "_")

					# Convert data to spatial object
					if (sim_cells_or_points == "point") {
						sp_dat <- as(run_sites, "SpatialPointsDataFrame")
						temp <- as.data.frame(dat)
						colnames(temp) <-  iv_locs[[it1]][it2]
						slot(sp_dat, "data") <- temp

						if (!raster::compareCRS(crs_sites, sim_crs)) {
							sp_dat <- sp::spTransform(sp_dat, CRS = sim_crs)
						}

					} else if (sim_cells_or_points == "cell") {
						sp_dat <- sim_raster
						stopifnot(raster::canProcessInMemory(sp_dat)) # if failing, then need a more sophisticated assignment of values than implemented below

						temp <- run_sites
						if (!raster::compareCRS(crs_sites, sim_crs)) {
							temp <- sp::spTransform(temp, CRS = sim_crs)
						}

						sp_dat[raster::cellFromXY(sp_dat, sp::coordinates(temp))] <- dat
					}

					# Save to disk
					saveRDS(sp_dat, file = file.path(dir.inmapvar, paste0(map_flag, ".rds")))

					# Figure
					png(height = 10, width = 6, units = "in", res = 200, file = file.path(dir.inmapvar, paste0(map_flag, ".png")))
					par_old <- par(mfrow = c(2, 1), mar = c(2.5, 2.5, 0.5, 0.5), mgp = c(1.25, 0.25, 0), tcl = 0.5, cex = 1)

					# panel a: map
					n_cols <- 255
					cols <- rev(terrain.colors(7))
					cols[1] <- "gray"
					cols <- colorRampPalette(c(cols, "dodgerblue3"))(n_cols)
					if (sim_cells_or_points == "point") {
						par1 <- par(mar = c(2.5, 2.5, 0.5, 8.5))
						cdat <- cut(dat, n_cols)
						p_size <- function(x) max(0.25, min(2, 100 / x))
						sp::plot(sp_dat, col = cols[as.integer(cdat)], pch = 15, cex = p_size(length(dat)), axes = TRUE, asp = 1)
						# legend
						ids <- round(seq(1, n_cols, length.out = 12))
						lusr <- par("usr")
						lxy <- cbind(rep(lusr[2] + (lusr[2] - lusr[1]) / 15, 12),
									 lusr[3] + (lusr[4] - lusr[3]) / 4 + seq(0, 1, length.out = 12) * (lusr[4] - lusr[3]) / 2)
						points(lxy, col = cols[ids], pch = 15, cex = 2, xpd = NA)
						text(lxy, pos = 4, labels = levels(cdat)[ids], xpd = NA)
						par(par1)

					} else if (sim_cells_or_points == "cell") {
						raster::plot(sp_dat, col = cols, asp = 1)
					}
					mtext(side = 3, line = -1, adj = 0.03, text = paste0("(", letters[1], ")"), font = 2)

					# panel b: histogram
					hist(dat, xlab = paste(names(iv_locs)[it1], iv_locs[[it1]][it2]), main = "")
					mtext(side = 3, line = -1, adj = 0.03, text = paste0("(", letters[2], ")"), font = 2)

					par(par_old)
					dev.off()
				}
			}
		}
	}

	if (!be.quiet) print(paste("SWSF input maps: ended after",  round(difftime(Sys.time(), t1, units = "secs"), 2), "s"))
}



#--------------------------------------------------------------------------------------------------#
#------------------------FUNCTION FOR A SOILWAT SIMULATION
if(actionWithSoilWat){
do_OneSite <- function(i_sim, i_labels, i_SWRunInformation, i_sw_input_soillayers, i_sw_input_treatments, i_sw_input_cloud, i_sw_input_prod, i_sw_input_site, i_sw_input_soils, i_sw_input_weather, i_sw_input_climscen, i_sw_input_climscen_values) {
#i_sim: a value of runIDs_total, i.e., counting the simulation runs
#i_xxx = the i_site-row of xxx for the i-th simulation run; if expN > 0 then these will eventually be repeated, and below replaced with experimental values
#i_exp = the row of sw_input_experimentals for the i_sim-th simulation run
#P_id is a unique id number for each scenario in each run

	time.sys <- Sys.time()

	flag.icounter <- formatC(i_sim, width=counter.digitsN, format = "d", flag="0")

  if (debug.dump.objects)
    print(paste0("'last.dump.do_OneSite_", i_sim, ".RData' will be produced if 'do_OneSite' fails"))
    on.exit({
      op_prev <- options("warn")
      options(warn = 0)
      env_tosave <- new.env()
      list2env(as.list(.GlobalEnv), envir = env_tosave)
      list2env(as.list(parent.frame()), envir = env_tosave)
      list2env(as.list(environment()), envir = env_tosave)
      save(list = ls(envir = env_tosave), envir = env_tosave,
          file = file.path(dir.prj, paste0("last.dump.do_OneSite_", i_sim, ".RData")))
      options(op_prev)
    })

#-----------------------Check for experimentals
	if(expN > 0 && length(create_experimentals) > 0) {
		i_exp <- it_exp(i_sim, runsN_master)
		i_label <- paste(flag.icounter, sw_input_experimentals[i_exp,1], i_labels, sep="_")

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
  if (!be.quiet)
    print(paste(i_sim, ":", i_label, "started at ", time.sys))

	#Check what needs to be done
	#TODO this currently doesn't work in the database setup
	isdone.overallAggs <- rep(FALSE, scenario_No)
	if(daily_no > 0){
		isdone.dailyAggs <- matrix(data=FALSE, nrow=daily_no, ncol=scenario_No)
	} else {
		isdone.dailyAggs <- TRUE
	}

	#set up task list: code: -1, don't do; 0, failed; 1, to do; 2, success
	tasks <- list(aggregate = 1L, #for now: ignoring to check time-series aggregations, i.e., assuming that if overallAggs is done, then time-series output was also completed
					create = 1L,
					execute = 1L)

	#Prepare directory structure in case SoilWat input/output is requested to be stored on disk
	if (saveRsoilwatInput || saveRsoilwatOutput) {
		temp <- file.path(dir.sw.runs, i_label)
		dir.create2(temp, showWarnings = FALSE)
		f_sw_input <- file.path(temp, "sw_input.RData")
		f_sw_output <- file.path(temp, "sw_output.RData")
	}

	#----Get preparations done
	if (all(unlist(tasks) %in% c(-1L, 1L))) {
		#get treatment sw.input.filenames for this run
		filesin <- swFilesIn

		if(!is.null(create_treatments) & tasks$create == 1L){
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
		if(tasks$create == -1L){
			stop("This currently doesn't work") #TODO make it work low PR
			soilsin <- basename(unlist(strsplit(infiletext[10], split="[[:space:]]"))[1])
		}

		#------Learn about soil layer structure
		#determine number of soil layers = d and soildepth
		if(!any(create_treatments=="soilsin") & tasks$create == 1L) {
			soildepth <- i_sw_input_soillayers$SoilDepth_cm
			layers_depth <- na.omit(as.numeric(i_sw_input_soillayers[2 + lmax]))
			d <- which(soildepth == layers_depth)
			if (length(d) == 0) {	#soildepth is one of the lower layer boundaries
				d <- min(length(layers_depth), findInterval(soildepth, layers_depth)+1)	#soildepth is not one of the lower layer boundaries, the next deeper layer boundary is used
			}
		} else {# needs to be read from soilsin file
			if(tasks$create == -1L) stop("This currently doesn't work") #TODO make it work low PR
			layers_depth <- swSoils_Layers(tr_soil[[soilsin]])[, 1]
			d <- length(layers_depth)
			soildepth <- max(layers_depth)
		}

		#functions to obtain soil layer structures
		#layer sequence
		ld <- setLayerSequence(d)
		layers_depth <- adjustLayersDepth(layers_depth, d)
		layers_width <- getLayersWidth(layers_depth)

		#top and bottom layer aggregation
		DeepestTopLayer <- setDeepestTopLayer(layers_depth, Depth_TopLayers)
		topL <- setTopLayer(d, DeepestTopLayer)
		bottomL <- setBottomLayer(d, DeepestTopLayer)


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
			simTime2 <- simTiming_ForEachUsedTimeUnit(simTime,
			  sim_tscales = c("daily", "monthly", "yearly"),
			  latitude = i_SWRunInformation$Y_WGS84,
			  account_NorthSouth = accountNSHemispheres_agg)

		} else {
			if(i_SWRunInformation$Y_WGS84 >= 0){
				simTime2 <- simTime_ForEachUsedTimeUnit_North
			} else {
				simTime2 <- simTime_ForEachUsedTimeUnit_South
			}
		}
	}



#------------------------CREATE RUNS
	if (tasks$create == 1L && continueAfterAbort && saveRsoilwatInput && file.exists(f_sw_input)) {
		load(f_sw_input)	# load objects: swRunScenariosData, i_sw_weatherList, grasses.c3c4ann.fractions, ClimatePerturbationsVals
		tasks$create <- 2L
	}

	if (tasks$create == 1L) {
		if(print.debug) print("Start of section 'create'")
		EVCO_done <- TRCO_done <- FALSE	#to check whether we get information for evaporation and transpiration coefficients
		TRRG_done <- FALSE #to check whether we get information for transpiration regions

		# Data objects used also during aggregation
		grasses.c3c4ann.fractions <- rep(list(rep(NA, 3)), scenario_No) #Init fractions of C3, C4, and annual grasses of grass-vegetation type fraction; used in create and aggregate
		ClimatePerturbationsVals <- matrix(data=c(rep(1,12),rep(0,24)),nrow=scenario_No, ncol=12*3, byrow=TRUE) #, dimnames=list(NULL,paste(rep(paste("ClimatePerturbations.", c("PrcpMultiplier.m", "TmaxAddand.m", "TminAddand.m"), sep=""), each=12), st_mo, rep(c("_none", "_C", "_C"), each=12), "_const", sep=""))

		#------1. Step: Information for this SoilWat-run from prepared SoilWat-run stored in dir.sw.in
		#Make a local copy of the swInput object do not want to destroy orignal
		swRunScenariosData <- list()
		swRunScenariosData[[1]] <- swDataFromFiles

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

		##adjust soil temp equation parameters
		if(any(create_treatments=="MaxTempDepth"))
		  swRunScenariosData[[1]]@site@SoilTemperatureConstants[[10]] <- i_sw_input_treatments$MaxTempDepth

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
          nvars = SoilLayer_MaxNo,
          do_fill = FALSE),

        LookupTranspRegionsFromTable = list(
          flag = "LookupTranspRegionsFromTable",
          pattern = "TranspRegion",
          tr_input = tr_input_TranspRegions,
          sw_input_use = sw_input_soils_use,
          sw_input = i_sw_input_soils,
          nvars = SoilLayer_MaxNo,
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
          fill_value = 76)	# 76 kg/m3 = median of medians over 6 sites in Colorado and Wyoming: Judson, A. & Doesken, N. (2000) Density of Freshly Fallen Snow in the Central Rocky Mountains. Bulletin of the American Meteorological Society, 81, 1577-1587.
      )

      for (pc in do_lookup) {
        if (sw_input_experimentals_use[pc$flag] && !done_prior[pc$flag]) {
          if (any(is.na(i_sw_input_treatments[[pc$flag]])) ||
             !all(unique(i_sw_input_treatments[[pc$flag]]) %in% rownames(pc$tr_input))) {
            print(paste("ERROR:", shQuote(pc$flag), "column in expirementals cannot have any NAs or name is not in tr_input table."))
            tasks$create <- 0L

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
            }
          }
        }
      }
    }

		#Treatment chunks
    if (print.debug)
      print("Start of LookupTranspCoeff")

    do_vegs <- list(
      veg = c("Grass", "Shrub", "Tree", "Forb"),
      flag = c("LookupTranspCoeffFromTable_Grass", "LookupTranspCoeffFromTable_Shrub",
                "LookupTranspCoeffFromTable_Tree", "LookupTranspCoeffFromTable_Forb"),
      adjustType = c("positive", "inverse", "inverse", "inverse"))

		for (k in seq_along(do_vegs[["veg"]])) {
      if (print.debug)
        print(paste0(".........", do_vegs[["veg"]][k]))

      if (any(create_treatments == do_vegs[["flag"]][k])) {
        temp <- is.na(i_sw_input_treatments[1, do_vegs[["flag"]][k]])
        temp1 <- !all(i_sw_input_treatments[1, do_vegs[["flag"]][k]] %in% colnames(tr_input_TranspCoeff))
        if (temp || temp1) {
          if (temp)
            print(paste(do_vegs[["flag"]][k], "for this run cannot be NA."))
          if (temp1)
            print(paste(do_vegs[["flag"]][k], "name for this run are not in tr_input_TranspCoeff table column names."))
          tasks$create <- 0L
        } else {
          trco <- TranspCoeffByVegType(
            tr_input_code = tr_input_TranspCoeff_Code, tr_input_coeff = tr_input_TranspCoeff,
            soillayer_no = d,
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
            print(paste("The function TranspCoeffByVegType returned NA or does not sum to greater than 0 for this run for type", do_vegs[["adjustType"]][k]))
            tasks$create <- 0L
          }
        }
      }
    }

		#the monthly ppt-shifts are extracted, but written to the weathersetup input file only at the end of the create section 'copy and make climate scenarios from datafiles', because they are multiplied with any climate change factors
		ppt_scShift <- rep(1, times=12)
		if(any(create_treatments=="LookupShiftedPPTScenarios")){
			ppt_scShift <- tr_input_shiftedPPT[which(rownames(tr_input_shiftedPPT) == i_sw_input_treatments[1,"LookupShiftedPPTCategory"]),(ts <- which(colnames(tr_input_shiftedPPT) == paste(i_sw_input_treatments$LookupShiftedPPTScenarios, "_m1", sep=""))):(ts+11)][st_mo]
		}

		if(any(create_treatments=="LookupClimatePPTScenarios") | any(create_treatments=="LookupClimateTempScenarios")){
			clim_scale <- swWeather_MonScalingParams(swRunScenariosData[[1]])[, 1:3]

			#Treatment chunk = climate precipitation scenarios
			if(any(create_treatments=="LookupClimatePPTScenarios") ) {
				clim_scale[, 1] <- tr_input_climPPT[st_mo, which(colnames(tr_input_climPPT) == i_sw_input_treatments$LookupClimatePPTScenarios)]
			}
			#Treatment chunk = climate temperature scenarios
			if(any(create_treatments=="LookupClimateTempScenarios") ) {
				clim_scale[, 2] <- clim_scale[, 3] <- tr_input_climTemp[st_mo, which(colnames(tr_input_climTemp) == i_sw_input_treatments$LookupClimateTempScenarios)]
			}

			swWeather_MonScalingParams(swRunScenariosData[[1]])[, 1:3] <- clim_scale

			rm(clim_scale)
		}


		#------4. Step: Information from datafiles are added if flagged 'use' to SoilWat input files
		#add information from datafile to cloudin
		if(print.debug) print("Start of cloudin")
		wind <- with(i_sw_input_cloud, data.frame(wind_ms_1, wind_ms_2, wind_ms_3, wind_ms_4, wind_ms_5, wind_ms_6, wind_ms_7, wind_ms_8, wind_ms_9, wind_ms_10, wind_ms_11, wind_ms_12))
		if(do.wind <- datafile.windspeedAtHeightAboveGround != SoilWat.windspeedAtHeightAboveGround)
			wind <- adjust.WindspeedHeight(uz=wind, height=datafile.windspeedAtHeightAboveGround)

		if (any(sw_input_cloud_use) | do.wind) {
			#sky cover
			if (any(sw_input_cloud_use[grepl("SkyC", names(sw_input_cloud_use))])) {
				sky <- with(i_sw_input_cloud, data.frame(SkyC_1, SkyC_2, SkyC_3, SkyC_4, SkyC_5, SkyC_6, SkyC_7, SkyC_8, SkyC_9, SkyC_10, SkyC_11, SkyC_12))
				swCloud_SkyCover(swRunScenariosData[[1]]) <- round(as.double(sky), 0)
			}
			#wind speed
			if (any(sw_input_cloud_use[grepl("wind", names(sw_input_cloud_use))]) | do.wind) {
				swCloud_WindSpeed(swRunScenariosData[[1]]) <- round(as.double(wind), 2)
			}
			#relative humidity
			if (any(sw_input_cloud_use[grepl("RH", names(sw_input_cloud_use))])) {
				rh <- with(i_sw_input_cloud, data.frame(RH_1, RH_2, RH_3, RH_4, RH_5, RH_6, RH_7, RH_8, RH_9, RH_10, RH_11, RH_12))
				swCloud_Humidity(swRunScenariosData[[1]]) <- round(as.double(rh), 0)
			}
			#snow density
			if (any(sw_input_cloud_use[grepl("snowd", names(sw_input_cloud_use))])) {
				snowd <- with(i_sw_input_cloud, data.frame(snowd_1, snowd_2, snowd_3, snowd_4, snowd_5, snowd_6, snowd_7, snowd_8, snowd_9, snowd_10, snowd_11, snowd_12))
				if(i_SWRunInformation$Y_WGS84 < 0 && i_sw_input_cloud$SnowD_Hemisphere == "N" || i_SWRunInformation$Y_WGS84 > 0 && i_sw_input_cloud$SnowD_Hemisphere == "S"){	#adjust for hemisphere only if location and data are opposite
					snowd <- c(snowd[7:12], snowd[1:6])
				}
				swCloud_SnowDensity(swRunScenariosData[[1]]) <- round(as.double(snowd), 1)
			}
		}

		#add vegetation information	from datafile to prodin
		if(print.debug) print("Start of prodin")

		if (any(sw_input_prod_use)) {
			#composition
			use_it <- sw_input_prod_use[grepl("Composition", names(sw_input_prod_use))]
			if (any(use_it)) {
        temp <- with(i_sw_input_prod, cbind(
          Composition_GrassFraction, Composition_ShrubFraction, Composition_TreeFraction,
          Composition_ForbFraction, Composition_BareGround))
        temp[is.finite(temp) | !use_it] <- 0 # if one is is requested, then put others to 0
				swRunScenariosData[[1]]@prod@Composition <- temp
			}
			#albedo
			use_it <- sw_input_prod_use[grepl("Albedo", names(sw_input_prod_use))]
			if (any(use_it)) {
        temp <- with(i_sw_input_prod, cbind(Grass_Albedo, Shrub_Albedo, Tree_Albedo,
          Forb_Albedo, BareGround_Albedo))
        temp[is.finite(temp)] <- 0
				swProd_Albedo(swRunScenariosData[[1]])[use_it] <- temp[use_it]
			}
			#constant canopy height
			use_it <- sw_input_prod_use[grepl("CanopyHeight_Constant", names(sw_input_prod_use))]
			if (any(use_it)) {
        temp <- with(i_sw_input_prod, cbind(Grass_CanopyHeight_Constant_cm,
          Shrub_CanopyHeight_Constant_cm, Tree_CanopyHeight_Constant_cm,
          Forb_CanopyHeight_Constant_cm))
        temp[is.finite(temp)] <- 0
				swRunScenariosData[[1]]@prod@CanopyHeight[5, use_it] <- height.datfile[use_it]
			}
			#flag for hydraulic redistribution
			use_it <- sw_input_prod_use[grepl("HydRed", names(sw_input_prod_use))]
			if (any(use_it)) {
				temp <- with(i_sw_input_prod, cbind(Grass_HydRed_OnOff, Shrub_HydRed_OnOff,
				  Tree_HydRed_OnOff, Forb_HydRed_OnOff))
        temp[is.finite(temp)] <- 0
				swProd_HydrRedstro_use(swRunScenariosData[[1]])[use_it] <- temp[use_it]
			}

      swProd_MonProd_grass(swRunScenariosData[[1]]) <- update_biomass(
        funct_veg = "Grass", use = sw_input_prod_use, prod_input = i_sw_input_prod,
        prod_default = swRunScenariosData[[1]]@prod)
      swProd_MonProd_shrub(swRunScenariosData[[1]]) <- update_biomass(
        funct_veg = "Shrub", use = sw_input_prod_use, prod_input = i_sw_input_prod,
        prod_default = swRunScenariosData[[1]]@prod)
      swProd_MonProd_tree(swRunScenariosData[[1]]) <- update_biomass(
        funct_veg = "Tree", use = sw_input_prod_use, prod_input = i_sw_input_prod,
        prod_default = swRunScenariosData[[1]]@prod)
      swProd_MonProd_forb(swRunScenariosData[[1]]) <- update_biomass(
        funct_veg = "Forb", use = sw_input_prod_use, prod_input = i_sw_input_prod,
        prod_default = swRunScenariosData[[1]]@prod)
		}
		#Moved adjust to southern Hemi

		#add site information to siteparamin
		if(print.debug) print("Start of siteparamin")
		if (any(sw_input_site_use)) {
      flags <- c("SWC_min", "SWC_init", "SWC_wet")
      site_use <- sw_input_site_use[flags]
      if (any(site_use))
        swSite_SWClimits(swRunScenariosData[[1]])[site_use] <-
          as.numeric(i_sw_input_site[flags][site_use])

      flags <- c("SWC_YearlyReset", "SWC_Deepdrain")
      site_use <- sw_input_site_use[flags]
      if (any(site_use))
        swSite_ModelFlags(swRunScenariosData[[1]])[site_use] <-
          as.numeric(i_sw_input_site[flags][site_use])

      flags <- c("PET_multiplier", "RunoffPercent_fromPondedWater")
      site_use <- sw_input_site_use[flags]
      if (any(site_use))
        swSite_ModelCoefficients(swRunScenariosData[[1]])[site_use] <-
          as.numeric(i_sw_input_site[flags][site_use])

      if (sw_input_site_use["Param_UnsaturatedPercolation"]) {
        swSite_DrainageCoefficient(swRunScenariosData[[1]]) <- i_sw_input_site$Param_UnsaturatedPercolation
      }

      flags <- c("Latitude", "Altitude", "Slope", "Aspect")
      site_use <- sw_input_site_use[flags]
      if (any(site_use))
        swSite_IntrinsicSiteParams(swRunScenariosData[[1]])[site_use] <-
          as.numeric(i_sw_input_site[flags][site_use])

      if (sw_input_site_use["SoilTemp_Flag"]) {
        swSite_SoilTemperatureFlag(swRunScenariosData[[1]]) <- i_sw_input_site$SoilTemp_Flag
      }
    }
    swSite_IntrinsicSiteParams(swRunScenariosData[[1]])[1] <- i_SWRunInformation$Y_WGS84 * pi / 180
    if (is.finite(i_SWRunInformation$ELEV_m))
      swSite_IntrinsicSiteParams(swRunScenariosData[[1]])[2] <- i_SWRunInformation$ELEV_m

    #add soil information to soilsin
    if (print.debug)
      print("Start of soilsin")
    # Use fixed column names
    soil_cols <- c("depth_cm", "matricd", "gravel_content", "EvapBareSoil_frac",
                    "transpGrass_frac", "transpShrub_frac", "transpTree_frac",
                    "transpForb_frac", "sand", "clay", "imperm", "soilTemp_c")
    soil_swdat <- swSoils_Layers(swRunScenariosData[[1]])
    dimnames(soil_swdat)[[2]] <- soil_cols

    done.Imperm_L1 <- FALSE
    if (sw_input_soils_use["Imperm_L1"] && any(create_treatments == "soilsin")) {
      soil_swdat[1, "imperm"] <- i_sw_input_soils$Imperm_L1
      done.Imperm_L1 <- TRUE
    }

    use_transpregion <- sw_input_soils_use[paste0("TranspRegion_L", ld)]
    if (sum(sw_input_soils_use) + {if (done.Imperm_L1) -1 else 0} - sum(use_transpregion) > 0) {

      # Calculate soil layer structure, because any(create_treatments=="soilsin") and soilsin may have a different soil layer structure than the datafiles
      temp <- as.numeric(na.omit(unlist(i_sw_input_soillayers[paste0("depth_L", seq_len(SoilLayer_MaxNo))])))
      layers_depth.datafile <- temp[temp <= as.numeric(i_sw_input_soillayers["SoilDepth_cm"])]
      if (length(layers_depth.datafile) == 0) {
        # this condition arises if i_sw_input_soillayers["SoilDepth_cm"] < i_sw_input_soillayers["depth_L1"]
        layers_depth.datafile <- temp[1]
      }

      if (!identical(layers_depth.datafile, soil_swdat[, "depth_cm"])) {
        # different soil layer structure in soilsin and datafile AND since variables are flagged in sw_input_soils_use => use only datafile values
        d <- max(1, min(length(layers_depth.datafile),
          findInterval(i_sw_input_soillayers["SoilDepth_cm"] - toln,
                        c(0, layers_depth.datafile)),
          na.rm = TRUE), na.rm = TRUE)
        layers_depth <- adjustLayersDepth(layers_depth.datafile, d)
        layers_width <- getLayersWidth(layers_depth)
        ld <- setLayerSequence(d)

        DeepestTopLayer <- setDeepestTopLayer(layers_depth, Depth_TopLayers)
        topL <- setTopLayer(d, DeepestTopLayer)
        bottomL <- setBottomLayer(d, DeepestTopLayer)
      }

      #compile soil information from both sources
      soildat <- matrix(0, nrow = d, ncol = length(soil_cols),
                        dimnames = list(NULL, soil_cols))
      soildat[, "depth_cm"] <- layers_depth.datafile[ld]
      infile_cols <- names(sw_input_soils_use)

      coefs <- list(infile = c("Matricd", "GravelContent", "EvapCoeff", "Grass_TranspCoeff",
                                "Shrub_TranspCoeff", "Tree_TranspCoeff", "Forb_TranspCoeff",
                                "Sand", "Clay", "Imperm", "SoilTemp"),
                    sw = soil_cols[-1])
      for (iv in seq_along(coefs[[1]])) {
        icol <- grep(coefs[["infile"]][iv], infile_cols, ignore.case = TRUE, value = TRUE)
        if (length(icol) > d)
          icol <- icol[ld]

        if (length(temp) > 0) {
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
      if (adjust.soilDepth) {
        for (k in d:1) {
          temp <- soildat[k, c("matricd", "sand", "clay")]
          if (any(!is.na(temp)))
            break
        }
        if (d != k) {
          d <- k
          layers_depth <- adjustLayersDepth(layers_depth, d)
          layers_width <- getLayersWidth(layers_depth)
          ld <- setLayerSequence(d)

          DeepestTopLayer <- setDeepestTopLayer(layers_depth, Depth_TopLayers)
          topL <- setTopLayer(d, DeepestTopLayer)
          bottomL <- setBottomLayer(d, DeepestTopLayer)

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
            print(paste("Site", i_sim, shQuote(i_label), "Layer", l,
                        ": data imputed from previous layer:",
                        paste(names(lbad)[lbad], collapse = ", ")))

          } else {
            print(paste("Site", i_sim, shQuote(i_label), ": data missing for 1st layer",
                        "-> no data to impute: simulation will fail"))
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
        print(paste("Run:", i_sim, shQuote(i_label), ": soil data didn't pass quality checks for:",
                    paste(soil_cols[colSums(!check_soil) > 0], collapse = ", ")))
        print(soil_swdat)
        tasks$create <- 0L
      }

    }

    EVCO_done <- sum(soil_swdat[, "EvapBareSoil_frac"]) > 0
    TRCO_done <- all(colSums(soil_swdat[, c("transpGrass_frac", "transpShrub_frac",
                                            "transpTree_frac", "transpForb_frac"), drop = FALSE]) > 0)

    swSoils_Layers(swRunScenariosData[[1]]) <- soil_swdat

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
				print("Transpiration Regions in Site can not be empty")
			} else if(sum(tr_rows) == 1) {
				swSite_TranspirationRegions(swRunScenariosData[[1]]) <- matrix(data=TranspirationRegions[tr_rows,],nrow=1,ncol=2,byrow=T,dimnames=list(numeric(),c("ndx","layer")))
				TRRG_done <- TRUE
			} else {
				swSite_TranspirationRegions(swRunScenariosData[[1]]) <- TranspirationRegions[tr_rows,]
				TRRG_done <- TRUE
			}
		}

		#add weather setup information to weatherin
		if (sw_input_weather_use["SnowFlag"])
			swWeather_UseSnow(swRunScenariosData[[1]]) <- as.logical(i_sw_input_weather$SnowFlag)
		if (sw_input_weather_use["SnowDrift_Percent"])
			swWeather_pct_SnowDrift(swRunScenariosData[[1]]) <- i_sw_input_weather$SnowDrift_Percent
		if (sw_input_weather_use["RunOffOnPerSnowmelt_Percent"])
			swWeather_pct_SnowRunoff(swRunScenariosData[[1]]) <- i_sw_input_weather$RunOffOnPerSnowmelt_Percent
		swWeather_FirstYearHistorical(swRunScenariosData[[1]]) <- simstartyr

    # Set simulation_timescales fix to daily, monthly, and yearly
		swOUT_TimeStep(swRunScenariosData[[1]]) <- c(daily = 0, monthly = 2, yearly = 3)

		#############Get Weather Data################
		if (print.debug) print("Start of daily weather")
		i_sw_weatherList <- list()

		if (!getCurrentWeatherDataFromDatabase) {
			if (i_SWRunInformation$dailyweather_source == "Maurer2002_NorthAmerica") {
				dirname.sw.runs.weather <- with(i_SWRunInformation, create_filename_for_Maurer2002_NorthAmerica(X_WGS84, Y_WGS84))
				i_sw_weatherList[[1]] <- ExtractGriddedDailyWeatherFromMaurer2002_NorthAmerica(
				          dir_data = dir.ex.maurer2002,
				          cellname = dirname.sw.runs.weather,
									startYear = simstartyr,
									endYear = endyr)

			} else if (i_SWRunInformation$dailyweather_source == "DayMet_NorthAmerica") {
				i_sw_weatherList[[1]] <- with(i_SWRunInformation,
				  ExtractGriddedDailyWeatherFromDayMet_NorthAmerica_swWeather(
				    dir_data = dir.ex.daymet,
				    site_ids = NULL,
				    coords_WGS84 = c(X_WGS84, Y_WGS84),
				    start_year = simstartyr, end_year = endyr))

			} else if (i_SWRunInformation$dailyweather_source == "LookupWeatherFolder") {	# Read weather data from folder
				i_sw_weatherList[[1]] <- try(getWeatherData_folders(
						LookupWeatherFolder = file.path(dir.sw.in.tr, "LookupWeatherFolder"),
						weatherDirName = local_weatherDirName(i_sim, scenario_No, runsN_master, runIDs_sites, name.OutputDB),
						filebasename = filebasename,
						startYear = simstartyr,
						endYear = endyr),
					silent = TRUE)
			}

		} else {
			#---Extract weather data
			weather_label_cur <- try(local_weatherDirName(i_sim, scenario_No, runsN_master, runIDs_sites, name.OutputDB), silent = TRUE)
			if (is.na(weather_label_cur))
				weather_label_cur <- try({function() stop("Output DB ", basename(name.OutputDB), " has no information about weather data for run ", i_sim)}(), silent = TRUE)

			if (inherits(weather_label_cur, "try-error")) {
				i_sw_weatherList <- weather_label_cur

			} else {
				i_sw_weatherList <- try(
				  lapply(climate.conditions[if (getScenarioWeatherDataFromDatabase) seq_along(climate.conditions) else 1L],
					  function(scen) Rsoilwat31::dbW_getWeatherData(Label = weather_label_cur,
                                                          startYear = simstartyr,
                                                          endYear = endyr,
                                                          Scenario = scen)),
				  silent = TRUE)
			}
		}

		#Check that extraction of weather data was successful
		if (inherits(i_sw_weatherList, "try-error") || length(i_sw_weatherList) == 0) {
			if(!be.quiet) print(paste(i_sim, i_label, "i_sw_weatherList ERROR:", i_sw_weatherList))
			tasks$create <- 0L
		}

		#copy and make climate scenarios from datafiles
		if(tasks$create > 0L) for(sc in 1:scenario_No){
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
			if (!getScenarioWeatherDataFromDatabase) {
        #get climate change information
        cols_climscen_val <- lapply(c("PPTmm_m", "TempC_min_m", "TempC_max_m"), function(flag)
          paste0(flag, st_mo, "_sc", formatC(sc - 1, width = 2, format = "d", flag = "0")))
        use_climscen_val <- any(unlist(sw_input_climscen_values_use[unlist(cols_climscen_val)]))

        cols_climscen_delta <- lapply(c("PPTfactor_m", "deltaTempC_min_m", "deltaTempC_max_m"), function(flag)
          paste0(flag, st_mo, "_sc", formatC(sc - 1, width = 2, format = "d", flag = "0")))
        use_climscen_delta <- any(unlist(sw_input_climscen_use[unlist(cols_climscen_delta)]))

				if (any(use_climscen_val)) {
					#convert climate change values to factors
					#read values from datafile
					pptVal_sc <- unlist(i_sw_input_climscen_values[, cols_climscen_val[[1]]])
					tVal_min_sc <- unlist(i_sw_input_climscen_values[, cols_climscen_val[[2]]])
					tVal_max_sc <- unlist(i_sw_input_climscen_values[, cols_climscen_val[[3]]])
					#calculate change factors
					ppt_sc <- pptVal_sc / (10 * SiteClimate_Ambient$meanMonthlyPPTcm)
					if(sum(abs(tVal_max_sc - tVal_min_sc)) > tol){
						t_min_sc <- tVal_min_sc - SiteClimate_Ambient$minMonthlyTempC
						t_max_sc <- tVal_max_sc - SiteClimate_Ambient$maxMonthlyTempC
					} else { #no information for tmin, tmax by GCM -> tmin=tmax=tmean
						t_min_sc <- t_max_sc <- tVal_min_sc - SiteClimate_Ambient$meanMonthlyTempC
					}
				} else if (any(use_climscen_delta)) {
					#read climate change factors from datafile
					ppt_sc <- unlist(i_sw_input_climscen[, cols_climscen_delta[[1]]])
					t_min_sc <- unlist(i_sw_input_climscen[, cols_climscen_delta[[2]]])
					t_max_sc <- unlist(i_sw_input_climscen[, cols_climscen_delta[[3]]])
				} else {
					ppt_sc <- rep(1, times = 12)
					t_min_sc <- t_max_sc <- rep(0, times=12)
				}
				#guarantee that all entries are finite: this may not be the case for instance if any(meanMonthlyClimate$meanMonthlyPPTcm == 0)
				ppt_sc <- temp_ppt_sc <- ifelse(is.finite(ppt_sc), ppt_sc, 1)
				t_min_sc <- ifelse(is.finite(t_min_sc), t_min_sc, 0)
				t_max_sc <- ifelse(is.finite(t_max_sc), t_max_sc, 0)

				if (sc > 1) {
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
							t_min_sc <- t_max_sc <- rep(0, times=12)
						}
					}
					if(any(create_treatments=="ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone") && !grepl("Both", i_sw_input_treatments$ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone, ignore.case=T)){
						temp_map_sc <- sum(SiteClimate_Ambient$meanMonthlyPPTcm * temp_ppt_sc)
						if(grepl("Mean", i_sw_input_treatments$ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone, ignore.case=T)) ppt_sc = rep(temp_map_sc / SiteClimate_Ambient$MAP_cm, times=12)
						if(grepl("Seasonality", i_sw_input_treatments$ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone, ignore.case=T)) ppt_sc = ppt_sc * SiteClimate_Ambient$MAP_cm / temp_map_sc
						if(grepl("None", i_sw_input_treatments$ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone, ignore.case=T)) ppt_sc = rep(1, times=12)
					}
				}

				temp <- swWeather_MonScalingParams(swRunScenariosData[[sc]])
				ppt_old <- temp[, 1]
				t_max_old <- temp[,2]
				t_min_old <- temp[,3]

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

				MonthlyScalingParams<-matrix(data=c(ppt_f,t_max_f,t_min_f),nrow=12,ncol=3)
				colnames(MonthlyScalingParams)<-c("PPT","MaxT","MinT")
				rownames(MonthlyScalingParams)<-c("January","February","March","April","May","June","July","August","September","October","November","December")

				swWeather_MonScalingParams(swRunScenariosData[[sc]])[, 1:3] <- MonthlyScalingParams
				ClimatePerturbationsVals[sc,1:12] <- MonthlyScalingParams[, 1]
				ClimatePerturbationsVals[sc,13:24] <- MonthlyScalingParams[, 2]
				ClimatePerturbationsVals[sc,25:36] <- MonthlyScalingParams[, 3]

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
								SiteClimate_Ambient$MAP_cm <- tol
								if(isTRUE(all.equal(SiteClimate_Scenario$MAP_cm, 0))){
									SiteClimate_Scenario$MAP_cm <- tol
									ppt_sc <- rep(0, times=12)
								} else {
									warning("Problem with scaling to 'mean' for ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone because of zero precipitation periods")
								}
							}
							if(sum(ppt_sc) > 0){
								if(sum(temp <- sapply(SiteClimate_Scenario$meanMonthlyPPTcm, FUN=function(x) isTRUE(all.equal(x, 0)))) > 0){
									warning("Problem with scaling to 'mean' for ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone because of zero precipitation periods")
									SiteClimate_Scenario$meanMonthlyPPTcm[temp] <- tol
								}
								ppt_sc <- (SiteClimate_Ambient$meanMonthlyPPTcm / SiteClimate_Scenario$meanMonthlyPPTcm) * (SiteClimate_Scenario$MAP_cm / SiteClimate_Ambient$MAP_cm)
							}
						}
						if(grepl("Seasonality", i_sw_input_treatments$ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone, ignore.case=T)) {
							#Mean of weather == mean of ambient, seasonality of weather = seasonality of scenario
							if(isTRUE(all.equal(SiteClimate_Scenario$MAP_cm, 0))){
								SiteClimate_Scenario$MAP_cm <- tol
								if(isTRUE(all.equal(SiteClimate_Ambient$MAP_cm, 0))){
									SiteClimate_Ambient$MAP_cm <- tol
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
								SiteClimate_Ambient$MAP_cm <- SiteClimate_Scenario$MAP_cm <- tol
								ppt_sc <- rep(0, times=12)
							}
							if(sum(ppt_sc) > 0){
								if(sum(temp <- sapply(SiteClimate_Scenario$meanMonthlyPPTcm, FUN=function(x) isTRUE(all.equal(x, 0)))) > 0){
									warning("Problem with scaling to 'mean' for ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone because of zero precipitation periods")
									SiteClimate_Scenario$meanMonthlyPPTcm[temp] <- tol
								}
								ppt_sc <- (SiteClimate_Ambient$meanMonthlyPPTcm / SiteClimate_Scenario$meanMonthlyPPTcm)
							}
						}
					}
					if(sum(temp <- sapply(SiteClimate_Ambient$meanMonthlyPPTcm, FUN=function(x) isTRUE(all.equal(x, 0)))) > 0){
						warning("Problem with scaling to 'mean' for ClimateScenario_PPT_PerturbationInMeanSeasonalityBothOrNone because of zero precipitation periods")
						SiteClimate_Ambient$meanMonthlyPPTcm[temp] <- tol
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
				#out.temp <- data.frame(i_sim, i_label, soilTUpper, soilTlower)
				#write.csv(out.temp, file=paste(dir.out.temp, .Platform$file.sep, flag.icounter, "_", "SoilTempC_atLowerBoundary.csv", sep=""), quote=FALSE, row.names=FALSE)
			}
			if (sw_input_site_use["SoilTempC_atUpperBoundary"]) {
				soilTUpper <- if (exists("soilTUpper")) soilTUpper else i_sw_input_site$SoilTempC_atUpperBoundary
			}
			if (sw_input_site_use["SoilTempC_atLowerBoundary"]) {
				soilTlower <- if (exists("soilTlower")) soilTlower else i_sw_input_site$SoilTempC_atLowerBoundary
				swSite_SoilTemperatureConsts(swRunScenariosData[[sc]])[8] <- soilTlower
			}
			if (pcalcs$EstimateInitialSoilTemperatureForEachSoilLayer) {
				init.soilTprofile <- EstimateInitialSoilTemperatureForEachSoilLayer(layers_depth=layers_depth, lower.Tdepth=as.numeric(swRunScenariosData[[sc]]@site@SoilTemperatureConstants[10]), soilTupper=soilTUpper, soilTlower=soilTlower)	#lower.Tdepth needs to be adjusted if it changes in soilparam.in
				#temporaly save data #TODO get this working
				#out.temp <- data.frame(i_sim, i_label, t(c(init.soilTprofile, rep(NA, times=SoilLayer_MaxNo-length(init.soilTprofile)))))
				#write.csv(out.temp, file=paste(dir.out.temp, .Platform$file.sep, flag.icounter, "_", "SoilTempC_InitProfile.csv", sep=""), quote=FALSE, row.names=FALSE)
			}

			#adjust init soil temperatures to climatic conditions
			use_soil_temp <- sw_input_soils_use[paste0("SoilTemp_L", ld)]
			if (any(use_soil_temp)) {
				temp <- seq_len(nrow(swSoils_Layers(swRunScenariosData[[sc]])))
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
				use_Forbs_Fraction <- any(create_treatments == "PotentialNaturalVegetation_CompositionForb_Fraction")
				Forbs_Fraction <- i_sw_input_treatments$PotentialNaturalVegetation_CompositionForb_Fraction
				use_BareGround_Fraction <- any(create_treatments == "PotentialNaturalVegetation_CompositionBareGround_Fraction")
				BareGround_Fraction <- i_sw_input_treatments$PotentialNaturalVegetation_CompositionBareGround_Fraction

				#TODO: Include forbs or bareground in PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996
				#save(SiteClimate_Ambient,SiteClimate_Scenario,MAP_mm,MAT_C,monthly.ppt,monthly.temp,dailyC4vars,isNorth,use_Annuals_Fraction, Annuals_Fraction,use_C4_Fraction, C4_Fraction,use_C3_Fraction, C3_Fraction,use_Shrubs_Fraction, Shrubs_Fraction,shrub.fraction.limit,file=file.path(dir.sw.runs, paste("Rsoilwat_composition_",i_sim,"_",sc,sep="")))
				temp <- try(PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996(MAP_mm,MAT_C,monthly.ppt,monthly.temp,dailyC4vars,isNorth,shrub.fraction.limit,
						use_Annuals_Fraction, Annuals_Fraction,
						use_C4_Fraction, C4_Fraction,
						use_C3_Fraction, C3_Fraction,
						use_Shrubs_Fraction, Shrubs_Fraction,
						use_Forbs_Fraction, Forbs_Fraction,
						use_BareGround_Fraction, BareGround_Fraction), silent=TRUE)
				if(inherits(temp, "try-error")){
					tasks$create <- 0L
				} else {
					grass.fraction <- temp$Composition[1]
					swProd_Composition(swRunScenariosData[[sc]]) <- temp$Composition
					grasses.c3c4ann.fractions[[sc]] <- temp$grasses.c3c4ann.fractions
				}
			}

			if(print.debug) print("Start of biomass adjustments")
			if(any(create_treatments == "PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996") && i_sw_input_treatments$PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996 && ((any(create_treatments == "AdjMonthlyBioMass_Temperature") && i_sw_input_treatments$AdjMonthlyBioMass_Temperature) | (any(create_treatments == "AdjMonthlyBioMass_Precipitation") &&  i_sw_input_treatments$AdjMonthlyBioMass_Precipitation) )){

				temp <- AdjMonthlyBioMass(tr_VegBiom = tr_VegetationComposition,
				  do_adjBiom_by_temp = any(create_treatments == "AdjMonthlyBioMass_Temperature") && i_sw_input_treatments$AdjMonthlyBioMass_Temperature,
				  do_adjBiom_by_ppt = any(create_treatments == "AdjMonthlyBioMass_Precipitation") & i_sw_input_treatments$AdjMonthlyBioMass_Precipitation,
				  fgrass_c3c4ann = grasses.c3c4ann.fractions[[sc]],
				  growing_limit_C = growing.season.threshold.tempC,
				  isNorth = isNorth,
				  MAP_mm = MAP_mm,
				  monthly.temp = monthly.temp)

				swProd_MonProd_grass(swRunScenariosData[[sc]])[,1:3] <- temp$grass[,1:3]
				swProd_MonProd_shrub(swRunScenariosData[[sc]])[,1:3] <- temp$shrub[,1:3]
			}

			#adjust Root Profile - need composition fractions set above
			if (print.debug)
			  print("Start of AdjRootProfile")

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

				if (grass.fraction == 0) { #if grass.fraction is 0 then Grass.trco will be 0
					Grass.trco <- TranspCoeffByVegType(
            tr_input_code = tr_input_TranspCoeff_Code, tr_input_coeff = tr_input_TranspCoeff,
            soillayer_no = d,
            trco_type = "FILL",
            layers_depth = layers_depth,
            adjustType = "positive")

				} else {
					C3.trco <- TranspCoeffByVegType(
            tr_input_code = tr_input_TranspCoeff_Code, tr_input_coeff = tr_input_TranspCoeff,
            soillayer_no = d,
            trco_type = trco_type_C3,
            layers_depth = layers_depth,
            adjustType = "positive")

					C4.trco <- TranspCoeffByVegType(
            tr_input_code = tr_input_TranspCoeff_Code, tr_input_coeff = tr_input_TranspCoeff,
            soillayer_no = d,
            trco_type = trco_type_C4,
            layers_depth = layers_depth,
            adjustType = "positive")

					Annuals.trco <- TranspCoeffByVegType(
            tr_input_code = tr_input_TranspCoeff_Code, tr_input_coeff = tr_input_TranspCoeff,
            soillayer_no = d,
            trco_type = trco_type_annuals,
            layers_depth = layers_depth,
            adjustType = "positive")

					Grass.trco <- C3.trco * grasses.c3c4ann.fractions[[sc]][1] +
					              C4.trco * grasses.c3c4ann.fractions[[sc]][2] +
					              Annuals.trco * grasses.c3c4ann.fractions[[sc]][3]
				}
				if(is.na(sum(Grass.trco)))
				  Grass.trco <- rep(0, d)

        Shrub.trco <- TranspCoeffByVegType(
          tr_input_code = tr_input_TranspCoeff_Code, tr_input_coeff = tr_input_TranspCoeff,
          soillayer_no = d,
          trco_type = trco_type_shrubs,
          layers_depth = layers_depth,
          adjustType = "inverse")
        Tree.trco <- TranspCoeffByVegType(
          tr_input_code = tr_input_TranspCoeff_Code, tr_input_coeff = tr_input_TranspCoeff,
          soillayer_no = d,
          trco_type = tro_type_tree,
          layers_depth = layers_depth,
          adjustType = "inverse")
        Forb.trco <- TranspCoeffByVegType(
          tr_input_code = tr_input_TranspCoeff_Code, tr_input_coeff = tr_input_TranspCoeff,
          soillayer_no = d,
          trco_type = tro_type_forb,
          layers_depth = layers_depth,
          adjustType = "inverse")

				swSoils_Layers(swRunScenariosData[[sc]])[,5] <- Grass.trco
				swSoils_Layers(swRunScenariosData[[sc]])[,6] <- Shrub.trco
				swSoils_Layers(swRunScenariosData[[sc]])[,7] <- Tree.trco
				swSoils_Layers(swRunScenariosData[[sc]])[,8] <- Forb.trco

				TRCO_done <- TRUE
			}

			if(print.debug) print("Start of vegetation scaling")
			Grass_Scaling_use <- c("Grass_TotalBiomass_ScalingFactor", "Grass_LiveBiomass_ScalingFactor", "Grass_Litter_ScalingFactor")
			Shrub_Scaling_use <- c("Shrub_TotalBiomass_ScalingFactor", "Shrub_LiveBiomass_ScalingFactor", "Shrub_Litter_ScalingFactor")
			Tree_Scaling_use <- c("Tree_TotalBiomass_ScalingFactor", "Tree_LiveBiomass_ScalingFactor", "Tree_Litter_ScalingFactor")
			Forb_Scaling_use <- c("Forb_TotalBiomass_ScalingFactor", "Forb_LiveBiomass_ScalingFactor", "Forb_Litter_ScalingFactor")
			if(any(create_treatments %in% c(Grass_Scaling_use, Shrub_Scaling_use, Tree_Scaling_use, Forb_Scaling_use))){
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

				forb_LitterTotalLiveScalingFactors <- rep(1, 3)
				if(any(create_treatments == "Forb_Litter_ScalingFactor") && is.finite(i_sw_input_treatments$Forb_Litter_ScalingFactor))
					forb_LitterTotalLiveScalingFactors[1] <- i_sw_input_treatments$Forb_Litter_ScalingFactor
				if(any(create_treatments == "Forb_TotalBiomass_ScalingFactor") && is.finite(i_sw_input_treatments$Forb_TotalBiomass_ScalingFactor))
					forb_LitterTotalLiveScalingFactors[2] <- i_sw_input_treatments$Forb_TotalBiomass_ScalingFactor
				if(any(create_treatments == "Forb_LiveBiomass_ScalingFactor") && is.finite(i_sw_input_treatments$Forb_LiveBiomass_ScalingFactor))
					forb_LitterTotalLiveScalingFactors[3] <- i_sw_input_treatments$Forb_LiveBiomass_ScalingFactor

				ScalingSeason <- i_sw_input_treatments$Vegetation_Biomass_ScalingSeason_AllGrowingORNongrowing
				if(is.na(ScalingSeason) || !any(c("All", "Growing", "Nongrowing") == ScalingSeason)) #set to All for default
					ScalingSeason <- "All"

				if(any(create_treatments == "Vegetation_Biomass_ScalingSeason_AllGrowingORNongrowing") && !is.na(ScalingSeason) && !(any(create_treatments == "Vegetation_Biomass_ScalingSeason_AllGrowingORNongrowing") && ScalingSeason == "All")) {
					if(ScalingSeason == "Growing") { #Growing: apply 'Vegetation_Biomass_ScalingFactor' only to those months that have MAT > growing.season.threshold.tempC
						if((templength<-length((temp<-SiteClimate_Scenario$meanMonthlyTempC>growing.season.threshold.tempC)[temp==TRUE]))>1) {
							swProd_MonProd_grass(swRunScenariosData[[sc]])[temp, 1:3] <- sweep(swProd_MonProd_grass(swRunScenariosData[[sc]])[temp, 1:3], MARGIN=2, FUN="*", grass_LitterTotalLiveScalingFactors)
							swProd_MonProd_shrub(swRunScenariosData[[sc]])[temp, 1:3] <- sweep(swProd_MonProd_shrub(swRunScenariosData[[sc]])[temp, 1:3], MARGIN=2, FUN="*", shrub_LitterTotalLiveScalingFactors)
							swProd_MonProd_tree(swRunScenariosData[[sc]])[temp, 1:3] <- sweep(swProd_MonProd_tree(swRunScenariosData[[sc]])[temp, 1:3], MARGIN=2, FUN="*", tree_LitterTotalLiveScalingFactors)
							swProd_MonProd_forb(swRunScenariosData[[sc]])[temp, 1:3] <- sweep(swProd_MonProd_forb(swRunScenariosData[[sc]])[temp, 1:3], MARGIN=2, FUN="*", forb_LitterTotalLiveScalingFactors)
						} else if(templength==1) {
							swProd_MonProd_grass(swRunScenariosData[[sc]])[temp,1:3]<-swProd_MonProd_grass(swRunScenariosData[[sc]])[temp,1:3]*grass_LitterTotalLiveScalingFactors
							swProd_MonProd_shrub(swRunScenariosData[[sc]])[temp,1:3]<-swProd_MonProd_shrub(swRunScenariosData[[sc]])[temp,1:3]*shrub_LitterTotalLiveScalingFactors
							swProd_MonProd_tree(swRunScenariosData[[sc]])[temp,1:3]<-swProd_MonProd_tree(swRunScenariosData[[sc]])[temp,1:3]*tree_LitterTotalLiveScalingFactors
							swProd_MonProd_forb(swRunScenariosData[[sc]])[temp, 1:3] <-swProd_MonProd_forb(swRunScenariosData[[sc]])[temp, 1:3]*forb_LitterTotalLiveScalingFactors
						} else {
							print("To Cold to do Vegetation Scaling Season for Growing")
						}
					} else if(ScalingSeason == "Nongrowing") {# Nongrowing: apply 'Vegetation_Biomass_ScalingFactor' only to those months that have MAT <= growing.season.threshold.tempC
						if((templength<-length((temp<-SiteClimate_Scenario$meanMonthlyTempC<=growing.season.threshold.tempC)[temp==TRUE]))>1) {
							swProd_MonProd_grass(swRunScenariosData[[sc]])[temp, 1:3] <- sweep(swProd_MonProd_grass(swRunScenariosData[[sc]])[temp, 1:3], MARGIN=2, FUN="*", grass_LitterTotalLiveScalingFactors)
							swProd_MonProd_shrub(swRunScenariosData[[sc]])[temp, 1:3] <- sweep(swProd_MonProd_shrub(swRunScenariosData[[sc]])[temp, 1:3], MARGIN=2, FUN="*", shrub_LitterTotalLiveScalingFactors)
							swProd_MonProd_tree(swRunScenariosData[[sc]])[temp, 1:3] <- sweep(swProd_MonProd_tree(swRunScenariosData[[sc]])[temp, 1:3], MARGIN=2, FUN="*", tree_LitterTotalLiveScalingFactors)
							swProd_MonProd_forb(swRunScenariosData[[sc]])[temp, 1:3] <- sweep(swProd_MonProd_forb(swRunScenariosData[[sc]])[temp, 1:3], MARGIN=2, FUN="*", forb_LitterTotalLiveScalingFactors)
						} else if (templength==1) {
							swProd_MonProd_grass(swRunScenariosData[[sc]])[temp,1:3]<-swProd_MonProd_grass(swRunScenariosData[[sc]])[temp,1:3]*grass_LitterTotalLiveScalingFactors
							swProd_MonProd_shrub(swRunScenariosData[[sc]])[temp,1:3]<-swProd_MonProd_shrub(swRunScenariosData[[sc]])[temp,1:3]*shrub_LitterTotalLiveScalingFactors
							swProd_MonProd_tree(swRunScenariosData[[sc]])[temp,1:3]<-swProd_MonProd_tree(swRunScenariosData[[sc]])[temp,1:3]*tree_LitterTotalLiveScalingFactors
							swProd_MonProd_forb(swRunScenariosData[[sc]])[temp,1:3]<-swProd_MonProd_forb(swRunScenariosData[[sc]])[temp,1:3]*forb_LitterTotalLiveScalingFactors
						} else {
							print("To Hot to do Vegetation Scaling Season for NonGrowing")
						}
					}
				} else {
					swProd_MonProd_grass(swRunScenariosData[[sc]])[, 1:3] <- sweep(swProd_MonProd_grass(swRunScenariosData[[sc]])[, 1:3], MARGIN=2, FUN="*", grass_LitterTotalLiveScalingFactors)
					swProd_MonProd_shrub(swRunScenariosData[[sc]])[, 1:3] <- sweep(swProd_MonProd_shrub(swRunScenariosData[[sc]])[, 1:3], MARGIN=2, FUN="*", shrub_LitterTotalLiveScalingFactors)
					swProd_MonProd_tree(swRunScenariosData[[sc]])[, 1:3] <- sweep(swProd_MonProd_tree(swRunScenariosData[[sc]])[, 1:3], MARGIN=2, FUN="*", tree_LitterTotalLiveScalingFactors)
					swProd_MonProd_forb(swRunScenariosData[[sc]])[, 1:3] <- sweep(swProd_MonProd_forb(swRunScenariosData[[sc]])[, 1:3], MARGIN=2, FUN="*", forb_LitterTotalLiveScalingFactors)
				}
				swProd_MonProd_grass(swRunScenariosData[[sc]])[, 3] <- finite01(swProd_MonProd_grass(swRunScenariosData[[sc]])[, 3])  #Check that live biomass fraction <= 1 & >= 0
				swProd_MonProd_shrub(swRunScenariosData[[sc]])[, 3] <- finite01(swProd_MonProd_shrub(swRunScenariosData[[sc]])[, 3])  #Check that live biomass fraction <= 1 & >= 0
				swProd_MonProd_tree(swRunScenariosData[[sc]])[, 3] <- finite01(swProd_MonProd_tree(swRunScenariosData[[sc]])[, 3])  #Check that live biomass fraction <= 1 & >= 0
				swProd_MonProd_forb(swRunScenariosData[[sc]])[, 3] <- finite01(swProd_MonProd_forb(swRunScenariosData[[sc]])[, 3])  #Check that live biomass fraction <= 1 & >= 0
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
				swProd_MonProd_forb(swRunScenariosData[[sc]])[, 3] <- rbind(swProd_MonProd_forb(swRunScenariosData[[sc]])[7:12,], swProd_MonProd_forb(swRunScenariosData[[sc]])[1:6,])
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
				max.tri.root <- min(apply(swSoils_Layers(swRunScenariosData[[sc]])[, c(6,7,8), drop=FALSE], MARGIN=2, FUN=function(x) sum(x > 0)))
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

			if(print.debug) print(paste0(i_label, " created scenario ", sc, ": tasks = ", paste(tasks, collapse=", "), ", evco = ", EVCO_done, ", trco = ", TRCO_done, ", trrg = ", TRRG_done))
		}#end do scenario creations

		if(!EVCO_done){
			print("Evaporation coefficients not set for this run.")
		} else if(!TRCO_done){
			print("Transpiration coefficients not set for this run.")
		} else if(!TRRG_done){
			print("Transpiration regions not set for this run.")
		}

		if(tasks$create <= 0L || !EVCO_done || !TRCO_done || !TRRG_done){
			tasks$create <- 0L
			tasks$execute <- tasks$aggregate <- -1L
		} else {
			tasks$create <- 2L
		}

		if (saveRsoilwatInput)
			save(swRunScenariosData, i_sw_weatherList, grasses.c3c4ann.fractions, ClimatePerturbationsVals, file = f_sw_input)
	}#end if do create runs

	if(makeInputForExperimentalDesign && expN > 0 && length(create_experimentals) > 0) {
		#This file will be used to remake the input files for experimentals
		infiletext <- c(paste(i_label, paste(i_SWRunInformation[-1], collapse = ExpInput_Seperator), sep=ExpInput_Seperator))
		infiletext <- c(infiletext, paste(i_label, paste(i_sw_input_soillayers[-1], collapse = ExpInput_Seperator), sep=ExpInput_Seperator))
		infiletext <- c(infiletext, paste(i_label, paste(i_sw_input_treatments[-1], collapse = ExpInput_Seperator), sep=ExpInput_Seperator))
		infiletext <- c(infiletext, paste(i_label, paste(i_sw_input_cloud[-1], collapse = ExpInput_Seperator), sep=ExpInput_Seperator))
		infiletext <- c(infiletext, paste(i_label, paste(i_sw_input_prod[-1], collapse = ExpInput_Seperator), sep=ExpInput_Seperator))
		infiletext <- c(infiletext, paste(i_label, paste(i_sw_input_site[-1], collapse = ExpInput_Seperator), sep=ExpInput_Seperator))
		infiletext <- c(infiletext, paste(i_label, paste(i_sw_input_soils[-1], collapse = ExpInput_Seperator), sep=ExpInput_Seperator))
		infiletext <- c(infiletext, paste(i_label, paste(i_sw_input_weather[-1], collapse = ExpInput_Seperator), sep=ExpInput_Seperator))
		infiletext <- c(infiletext, paste(i_label, paste(i_sw_input_climscen[-1], collapse = ExpInput_Seperator), sep=ExpInput_Seperator))
		infiletext <- c(infiletext, paste(i_label, paste(i_sw_input_climscen_values[-1], collapse = ExpInput_Seperator), sep=ExpInput_Seperator))

		infilename <- paste(dir.out.temp, .Platform$file.sep, flag.icounter, "_", "Experimental_InputData_All.csv", sep="")
		infile <- file(infilename, "w+b")
		writeLines(text = infiletext, con = infile, sep = "\n")
		close(infile)
	}


#------------------------EXECUTE SOILWAT
	if (tasks$execute == 1L && continueAfterAbort && saveRsoilwatOutput && file.exists(f_sw_output)) {
		load(f_sw_output)	# load objects: runData
		tasks$execute <- 2L
	}

	if (!exists("swRunScenariosData") || !exists("i_sw_weatherList"))
		tasks$execute <- -1L

	if (tasks$execute == 1L) {
		runData <- list()

		if (is.na(i_sw_input_treatments$Exclude_ClimateAmbient)) i_sw_input_treatments$Exclude_ClimateAmbient <- FALSE
		sc1 <- if (any(create_treatments == "Exclude_ClimateAmbient") && i_sw_input_treatments$Exclude_ClimateAmbient && i_sim != 1L) 2L else 1L

		DeltaX <- c(NA, 0L)
			# first element: determined deltaX_Param value; will be used for all scenarios >= sc if modified
			# second element: -1 == failed; 0 == no run yet; 1 == deltaX_Param successfully approved; 2 == deltaX_Param successfully modified

		for (sc in sc1:scenario_No) {
			if (print.debug) print(paste("Start of SoilWat execution for scenario:", sc))

			scw <- if (getScenarioWeatherDataFromDatabase) sc else 1L
			mDepth <- swSite_SoilTemperatureConsts(swRunScenariosData[[sc]])["MaxDepth"]

			if (DeltaX[2] > 0) {
				if (print.debug) print(paste("Using pre-determined DeltaX =", DeltaX[1]))
				if (DeltaX[2] == 2L) swSite_SoilTemperatureConsts(swRunScenariosData[[sc]])["deltaX_Param"] <- DeltaX[1]

				runData[[sc]] <- try(sw_exec(inputData = swRunScenariosData[[sc]],
											 weatherList = i_sw_weatherList[[scw]],
									echo = FALSE, quiet = FALSE),
								silent = TRUE)

			} else {
				runData[[sc]] <- try(sw_exec(inputData = swRunScenariosData[[sc]],
											 weatherList = i_sw_weatherList[[scw]],
									echo = FALSE, quiet = FALSE),
								silent = TRUE)

				## Testing for Error in Soil Layers and then repeating the SW run with a modified deltaX
				is_SOILTEMP_INSTABLE <- tempError()

				if (is_SOILTEMP_INSTABLE) {
					## Incrementing deltaX and recalling SOILWAT until the temperature is at least normal or the loop executes ten times
					i_soil_rep <- 0
					DeltaX[1] <- swSite_SoilTemperatureConsts(swRunScenariosData[[sc]])["deltaX_Param"]

					while (!inherits(runData[[sc]], "try-error") && is_SOILTEMP_INSTABLE && DeltaX[1] <= mDepth && i_soil_rep < 10) {
						## Make sure that the increment for the soil layers is a multiple of the MaxDepth, modulus of 0 means no remainder and thus a multiple of the MaxDepth
						repeat {
							DeltaX[1] <- DeltaX[1] + increment_soiltemperature_deltaX_cm
							if (mDepth %% DeltaX[1] == 0) break
						}

						## recall Soilwat with the new deltaX parameter and continue to do so with increasing deltax until resolved or executed 10 times
						swSite_SoilTemperatureConsts(swRunScenariosData[[sc]])["deltaX_Param"] <- min(DeltaX[1], mDepth)
						if (print.debug) print(paste("Site", i_sim, i_label, "SOILWAT called again with deltaX = ", swSite_SoilTemperatureConsts(swRunScenariosData[[sc]])["deltaX_Param"], "cm because soil temperature stability criterion was not met." ))

						runData[[sc]] <- try(sw_exec(inputData = swRunScenariosData[[sc]],
											 weatherList = i_sw_weatherList[[scw]],
									echo = FALSE, quiet = FALSE),
								silent = TRUE)

						## Test to check and see if SOILTEMP is stable so that the loop can break - this will be based on parts being > 1.0
						is_SOILTEMP_INSTABLE <- tempError()
						i_soil_rep <- i_soil_rep + 1
					}

					DeltaX[2] <- if (!inherits(runData[[sc]], "try-error") && !is_SOILTEMP_INSTABLE) 2L else -1L

					#TODO: change deltaX_Param for all [> sc] as well
					if (saveRsoilwatInput)
						save(swRunScenariosData, i_sw_weatherList, grasses.c3c4ann.fractions, ClimatePerturbationsVals, file = f_sw_input)

				} else {
					DeltaX <- c(swSite_SoilTemperatureConsts(swRunScenariosData[[sc]])["deltaX_Param"], 1L)
				}
			}

			if (inherits(runData[[sc]], "try-error") || DeltaX[2] < 0) {
				tasks$execute <- 0L
				break
			}
		}

		if (saveRsoilwatOutput)
			save(runData, file = f_sw_output)

	}#end if do execute

	if (tasks$execute > 0L && exists("runData"))
		tasks$execute <- 2L

#------------------------AGGREGATE SOILWAT OUTPUT
	if (!exists("swRunScenariosData") || !exists("runData"))
		tasks$aggregate <- -1L

	if (tasks$aggregate == 1L) {
		if(print.debug) print("Preparations for aggregation")

		#get soil texture data for each layer
		stemp <- swSoils_Layers(swRunScenariosData[[1]])
		layers_depth <- stemp[, 1]
		layers_width <- getLayersWidth(layers_depth)
		soilDepth_cm <- max(stemp[, 1])
		soilLayers_N <- length(stemp[, 1])

		gravel <- stemp[,3]
		sand <- stemp[,9]
		clay <- stemp[,10]

		#get soil aggregation layer for daily aggregations
		if(daily_lyr_agg[["do"]]){
			aggLs <- setAggSoilLayerForAggDailyResponses(layers_depth, daily_lyr_agg)
		} else {
			aggLs <- as.list(ld)
		}
		aggLs_no <- length(aggLs)

		texture <- list(sand.top=weighted.mean(sand[topL], layers_width[topL]),
						sand.bottom=weighted.mean(sand[bottomL], layers_width[bottomL]),
						clay.top=weighted.mean(clay[topL], layers_width[topL]),
						clay.bottom=weighted.mean(clay[bottomL], layers_width[bottomL]),
						gravel.top=weighted.mean(gravel[topL], layers_width[topL]),
						gravel.bottom=weighted.mean(gravel[bottomL], layers_width[bottomL])
					)

		if(daily_no > 0){
			textureDAgg <- list(	gravel=sapply(1:aggLs_no, FUN=function(x) weighted.mean(gravel[aggLs[[x]]], layers_width[aggLs[[x]]])),
									sand=sapply(1:aggLs_no, FUN=function(x) weighted.mean(sand[aggLs[[x]]], layers_width[aggLs[[x]]])),
									clay=sapply(1:aggLs_no, FUN=function(x) weighted.mean(clay[aggLs[[x]]], layers_width[aggLs[[x]]]))
								)
		}

		#prepare SQL result container
		SQL <- SQLcurrent <- character(0)
		fid <- if (parallel_runs && parallel_backend == "mpi") {
			mpi.comm.rank()
		} else if (parallel_runs && parallel_backend == "snow") {
			.nodeNumber
		} else if (parallel_runs && parallel_backend == "multicore") {
			sample.int(50000, 1)
		} else {
			0L
		}
		dbTempFile <- file.path(dir.out, "temp", paste("SQL_Node_", fid, ".sql", sep = ""))
		dbTempFileCurrent <- file.path(dir.out, "temp", paste("SQL_Current_Node_", fid, ".sql", sep = ""))

		#Performance Measuring
		#if(aggregate.timing) OutputTiming <- list()
		#if(aggregate.timing) GeneralOutputTiming <- matrix(NA,nrow=scenario_No,ncol=2)
		#aggregate for each scenario
		for (sc in 1:scenario_No){
			if(print.debug) print(paste("Start of overall aggregation for scenario:", sc))
			#HEADER GENERATION REMOVED#
			#only exclude if 1.) Exclude_ClimateAmbient is true in treatments 2.) That Run is set to Exclude_ClimateAmbient 3.) Our current Scenario is Current
			if(any(create_treatments == "Exclude_ClimateAmbient") && i_sw_input_treatments$Exclude_ClimateAmbient && sc==1 && i_sim!=1) {
				Exclude_ClimateAmbient <- TRUE

				#dbOverallColumns comes from database creation
				P_id <- it_Pid(i_sim, sc, scenario_No, runsN_master, runIDs_sites)
				temp <- paste(c(P_id, if (dbOverallColumns > 0) paste0(rep("NULL", dbOverallColumns), collapse = ",")), collapse = ",")
				SQL1 <- paste0("INSERT INTO \"aggregation_overall_mean\" VALUES (", temp, ");")
				SQL2 <- paste0("INSERT INTO \"aggregation_overall_sd\" VALUES (", temp, ");")
				if(length(SQL) == 0) {
					SQL <- paste(SQL1, SQL2, sep="\n")
				} else {
					SQL <- paste(SQL, SQL1, SQL2, sep="\n")
				}

			} else {
				Exclude_ClimateAmbient <- FALSE
			}

			#overall aggregation. If Exclude_ClimateAmbient == TRUE then skip
			if (!continueAfterAbort | (continueAfterAbort & !isdone.overallAggs[sc]) && !Exclude_ClimateAmbient) {

				#delete data so that they are read anew for each scenario; each variable is checked that datafile is read in only once per scenario
				to_del <- c("temp.yr", "temp.mo", "temp.dy",
							"prcp.yr", "prcp.mo", "prcp.dy",
							"PET.yr", "PET.mo", "PET.dy",
							"vpd.yr", "vpd.mo", "vpd.dy",
							"AET.yr", "AET.mo", "AET.dy",
							"soiltemp.yr", "soiltemp.mo", "soiltemp.dy",
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
							"runoff.yr", "runoff.mo", "runoff.dy",
							"intercept.yr", "intercept.mo", "intercept.dy",
							"deepDrain.yr", "deepDrain.mo", "deepDrain.dy")
				to_del <- to_del[to_del %in% ls()]
				if(length(to_del) > 0) try(rm(list=to_del), silent=TRUE)

				#result vector column index indicating variable within set of n_variables per scenario
				resMeans <- resSDs <- rep(NA, length=dbOverallColumns)
				nv <- 1


				#---Aggregation: SoilWat inputs
			#0
				if(aon$input_SoilProfile){
					if(print.debug) print("Aggregation of input_SoilProfile")
					resMeans[nv:(nv+7)] <- c(soilDepth_cm, soilLayers_N, unlist(texture))
					nv <- nv+8
					resMeans[nv]<-swRunScenariosData[[1]]@site@SoilTemperatureConstants[9]
					nv <- nv+1
				}
			#1
				if(aon$input_FractionVegetationComposition) {
					if(print.debug) print("Aggregation of input_FractionVegetationComposition")
					resMeans[nv:(nv+7)] <- c(swProd_Composition(swRunScenariosData[[sc]]), grasses.c3c4ann.fractions[[sc]])
					nv <- nv+8
				}
			#2
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

					resMeans[nv:(nv+11)] <- swProd_MonProd_forb(swRunScenariosData[[sc]])[,1]
					nv <- nv+12
					resMeans[nv:(nv+11)] <- swProd_MonProd_forb(swRunScenariosData[[sc]])[,2]
					nv <- nv+12
					resMeans[nv:(nv+11)] <- swProd_MonProd_forb(swRunScenariosData[[sc]])[,2]*swProd_MonProd_forb(swRunScenariosData[[sc]])[,3]
					nv <- nv+12
				}
			#3
				if(aon$input_VegetationPeak) {
					if(print.debug) print("Aggregation of input_VegetationPeak")
					fracs <- swProd_Composition(swRunScenariosData[[sc]])[1:4] #get the fractional Composition of grasses, shrubs, and trees
					tempdat <- matrix(data=NA, nrow=12, ncol=4)#matrix to hold biomass * percLive for grass,shrubs,trees
					colnames(tempdat) <- c("grass", "shrub", "tree", "forb")
					tempdat[,1] <- swProd_MonProd_grass(swRunScenariosData[[sc]])[,2]*swProd_MonProd_grass(swRunScenariosData[[sc]])[,3]
					tempdat[,2] <- swProd_MonProd_shrub(swRunScenariosData[[sc]])[,2]*swProd_MonProd_shrub(swRunScenariosData[[sc]])[,3]
					tempdat[,3] <- swProd_MonProd_tree(swRunScenariosData[[sc]])[,2]*swProd_MonProd_tree(swRunScenariosData[[sc]])[,3]
					tempdat[,4] <- swProd_MonProd_forb(swRunScenariosData[[sc]])[,2]*swProd_MonProd_forb(swRunScenariosData[[sc]])[,3]

					sumWeightedLiveBiomassByMonth <- apply(sweep(tempdat, MARGIN=2, fracs, FUN="*"), MARGIN=1, sum) #sweep out fractionals, and sum over rows
					maxMonth <- which(sumWeightedLiveBiomassByMonth==max(sumWeightedLiveBiomassByMonth)) #returns index, which is the month, of max bio
					meanPeakMonth <- circ.mean(maxMonth, 12)
					duration <- circ.range(maxMonth, 12)+1

					resMeans[nv:(nv+1)] <- c(meanPeakMonth, duration) #just in case we get more then one month
					nv <- nv+2
				}
			#4
				if(aon$input_Phenology) {
					if(print.debug) print("Aggregation of input_Phenology")
					if(!exists("temp.mo")) temp.mo <- get_Temp_mo(sc, runData, simTime)
					monthly.temp <- tapply(temp.mo$mean, simTime2$month_ForEachUsedMonth, mean) #get mean monthly temp
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
					Tcoeff <- swSoils_Layers(swRunScenariosData[[1]])[, 5:8, drop = FALSE]
					if(is.null(dim(Tcoeff))) Tcoeff <- matrix(Tcoeff, nrow=1)

					TaggLs <- sapply(aggLs, FUN=function(l) apply(Tcoeff[l,, drop=FALSE], 2, sum))
					if(length(bottomL) > 0 && !identical(bottomL, 0)){
						Ttb <- sapply(list(topL, bottomL), FUN=function(l) apply(Tcoeff[l,, drop=FALSE], 2, sum))
					} else {
						Ttb <- sapply(list(topL), FUN=function(l) apply(Tcoeff[l,, drop=FALSE], 2, sum))
					}

					iinv <- inv <- nv
					for(iv in 1:4){
						nv <- nv+SoilLayer_MaxNo #We don't know the max number of soil layers (aggLs_no) among all simulations, i.e., set to the maximum
						resMeans[(inv+(iv-1)*SoilLayer_MaxNo):(nv-1)] <- c(TaggLs[iv, ], rep(NA, times=SoilLayer_MaxNo-aggLs_no))
					}
					inv <- nv
					for(iv in 1:4){
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
				if(aon$yearlyTemp){
					if(print.debug) print("Aggregation of yearlyTemp")
					if(!exists("temp.yr"))	temp.yr <- get_Temp_yr(sc, runData, simTime)

					resMeans[nv] <- mean(temp.yr$mean)
					resSDs[nv] <- sd(temp.yr$mean)
					nv <- nv+1
				}
			#8
				if(aon$yearlyPPT){
					if(print.debug) print("Aggregation of yearlyPPT")
					if(!exists("prcp.yr")) prcp.yr <- get_PPT_yr(sc, runData, simTime)

					resMeans[nv] <- mean(prcp.yr$ppt)
					resSDs[nv] <- sd(prcp.yr$ppt)
					resMeans[nv+1] <- mean(snowofppt <- prcp.yr$snowfall/prcp.yr$ppt, na.rm=TRUE)
					resSDs[nv+1] <- sd(snowofppt, na.rm=TRUE)
					nv <- nv+2

					rm(snowofppt)
				}
			#9
				if(aon$dailySnowpack){
					if(print.debug) print("Aggregation of dailySnowpack")
					if(!exists("prcp.yr")) prcp.yr <- get_PPT_yr(sc, runData, simTime)
					if(!exists("prcp.dy")) prcp.dy <- get_PPT_dy(sc, runData, simTime)
					if(!exists("SWE.dy")) SWE.dy <- get_SWE_dy(sc, runData, simTime)

          # Fraction of rain that falls on snow
          rainOnSnow <- ifelse(SWE.dy$val > 0, prcp.dy$rain, 0)
					rainOnSnow <- tapply(rainOnSnow, simTime2$year_ForEachUsedDay, sum)
					rainOnSnow <- rainOnSnow / prcp.yr$ppt

					resMeans[nv] <- mean(rainOnSnow, na.rm = TRUE)
					resSDs[nv] <- sd(rainOnSnow, na.rm = TRUE)
					nv <- nv+1

					rm(rainOnSnow)
				}
			#10
				if(aon$dailySnowpack){#daily snowpack: accountNSHemispheres_agg
					if(print.debug) print("Aggregation of dailySnowpack2")
					if(!exists("SWE.dy")) SWE.dy <- get_SWE_dy(sc, runData, simTime)

					if(sum(SWE.dy$val) > 0){
						snowyears <- simTime2$year_ForEachUsedDay_NSadj + ifelse(simTime2$doy_ForEachUsedDay_NSadj > 273, 1, 0)	# 1. snow-year: N-hemisphere: October 1st = 1 day of snow year; S-hemisphere: April 1st = 1 day of snow year
						adjDays <- ifelse(simTime2$doy_ForEachUsedDay[1] == simTime2$doy_ForEachUsedDay_NSadj[1], 365 - 273, -91)

						if(length(unique(snowyears))-2 > 0){
							res.snow  <- matrix(data=0, nrow=length(unique(snowyears))-2, ncol=6, byrow=TRUE)
							res.snow[,1]  <- unique(snowyears)[2:(length(unique(snowyears))-1)]  # 1. snowyear
							snowyear.trim <- !is.na(pmatch(snowyears, res.snow[, 1], duplicates.ok=TRUE))
							res.snow[,2] <- tapply(SWE.dy$val[snowyear.trim], snowyears[snowyear.trim], which.max) - adjDays # 2. doy of peak snowpack water-equivalent (mm)
							res.snow[,5] <- tapply(SWE.dy$val[snowyear.trim], snowyears[snowyear.trim], function(s) sum(s > 0)) # 5. total number of days of snow cover
							res.snow[,6] <- tapply(SWE.dy$val[snowyear.trim], snowyears[snowyear.trim], max) # 6. peak snowpack water-equivalent (mm)

							syi <- 1
							for (sy in res.snow[,1]){
								r <- rle(ifelse(SWE.dy$val[which(snowyears == sy)]>0,1,0))
								res.snow[syi,4] <- r$lengths[which(r$values==1)][order(r$lengths[which(r$values==1)], decreasing=TRUE)[1]] # 4. number of continous days of snow cover
								ind <- which(r$lengths==res.snow[syi,4])
								res.snow[syi,3] <- cumsum(r$lengths)[ifelse(length(ind)>1, ind[which.max(r$values[ind])], ind)] - adjDays # 3. last day of continous snow cover
								syi <- syi + 1
							}
							if(nrow(res.snow) > 1){
								resMeans[nv:(nv+4)] <- c(apply(res.snow[, 2:3], 2, circ.mean, int = 365, na.rm = TRUE),
								                         apply(res.snow[,-(1:3)], 2, mean, na.rm = TRUE))
								resSDs[nv:(nv+4)] <- c(apply(res.snow[, 2:3], 2, circ.sd, int = 365, na.rm = TRUE),
								                       apply(res.snow[,-(1:3)], 2, sd, na.rm = TRUE))
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
				if(aon$dailyFrostInSnowfreePeriod){
					if(print.debug) print("Aggregation of dailyFrostInSnowfreePeriod")
					if(!exists("temp.dy")) temp.dy <- get_Temp_dy(sc, runData, simTime)
					if(!exists("SWE.dy")) SWE.dy <- get_SWE_dy(sc, runData, simTime)

					for(iTmin in Tmin_crit_C){
						frostWithoutSnow <- SWE.dy$val == 0 & temp.dy$min < iTmin
						frostWithoutSnow <- tapply(frostWithoutSnow, simTime2$year_ForEachUsedDay, sum)  #Numbers of days with min.temp < 0 and snow == 0

						resMeans[nv] <- mean(frostWithoutSnow, na.rm=TRUE)
						resSDs[nv] <- sd(frostWithoutSnow, na.rm=TRUE)
						nv <- nv+1
					}

					rm(frostWithoutSnow)
				}
			#12
				if(aon$dailyHotDays){
					if(print.debug) print("Aggregation of dailyHotDays")
					if(!exists("temp.dy")) temp.dy <- get_Temp_dy(sc, runData, simTime)

					nv_add <- length(Tmax_crit_C)

					dailyExcess <- temp.dy$max >
						matrix(rep.int(Tmax_crit_C, length(temp.dy$max)),
							ncol = nv_add, byrow = TRUE)

					HotDays <- matrix(NA, nrow = simTime$no.useyr, ncol = nv_add)
					for (k in seq_along(Tmax_crit_C))
						HotDays[, k] <- tapply(dailyExcess[, k],
							INDEX = simTime2$year_ForEachUsedDay,
							FUN = sum)

					nv_new <- nv + nv_add
					resMeans[nv:(nv_new - 1)] <- .colMeans(HotDays, simTime$no.useyr, nv_add)
					resSDs[nv:(nv_new - 1)] <- apply(HotDays, 2, sd)
					nv <- nv_new

					rm(HotDays, dailyExcess)
				}
			#12b
				if(aon$dailyWarmDays){
					if(print.debug) print("Aggregation of dailyWarmDays")
					if(!exists("temp.dy")) temp.dy <- get_Temp_dy(sc, runData, simTime)

					nv_add <- length(Tmean_crit_C)

					dailyExcess <- temp.dy$mean >
						matrix(rep.int(Tmean_crit_C, length(temp.dy$mean)),
							ncol = nv_add, byrow = TRUE)

					WarmDays <- matrix(NA, nrow = simTime$no.useyr, ncol = nv_add)
					for (k in seq_along(Tmean_crit_C))
						WarmDays[, k] <- tapply(dailyExcess[, k],
							INDEX = simTime2$year_ForEachUsedDay,
							FUN = sum)

					nv_new <- nv + nv_add
					resMeans[nv:(nv_new - 1)] <- .colMeans(WarmDays, simTime$no.useyr, nv_add)
					resSDs[nv:(nv_new - 1)] <- apply(WarmDays, 2, sd)
					nv <- nv_new

					rm(WarmDays, dailyExcess)
				}

			#13
				if(aon$dailyPrecipitationEventSizeDistribution){	#daily weather frequency distributions
					if(print.debug) print("Aggregation of dailyPrecipitationEventSizeDistribution")
					if(!exists("prcp.dy")) prcp.dy <- get_PPT_dy(sc, runData, simTime)

          #prcp-event sizes in bins
          ppt_sizes <- tabulate_values_in_bins(
            x = prcp.dy$ppt, method = "values", vcrit = 0,
            bins = bin.prcpSizes, nbins = 7,
            simTime = simTime, simTime2 = simTime2)

					resMeans[nv] <- mean(ppt_sizes$eventsPerYear)
					resSDs[nv] <- sd(ppt_sizes$eventsPerYear)
					resMeans[(nv+1):(nv+7)] <- apply(ppt_sizes$freq.summary, 1, mean)
					resSDs[(nv+1):(nv+7)] <- apply(ppt_sizes$freq.summary, 1, sd)
					nv <- nv+8

					rm(ppt_sizes)
				}
			#15
				if(aon$yearlyPET){
					if(print.debug) print("Aggregation of yearlyPET")
					if(!exists("PET.yr")) PET.yr <- get_PET_yr(sc, runData, simTime)

					resMeans[nv] <- mean(PET.yr$val)
					resSDs[nv] <- sd(PET.yr$val)
					nv <- nv+1
				}
			#16
				#correl monthly swp (top and bottom) vs. pet and ppt vs. temp, use product moment correlation coefficient {eqn. 11.6, \Sala, 1997 #45}
				if(aon$monthlySeasonalityIndices){
					if(print.debug) print("Aggregation of monthlySeasonalityIndices")
					if(!exists("vwcmatric.mo")) vwcmatric.mo <- get_Response_aggL(sc, sw_vwcmatric, tscale = "mo", scaler = 1, FUN = weighted.mean, weights = layers_width, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)
					if(!exists("swpmatric.mo")) swpmatric.mo <- get_SWPmatric_aggL(vwcmatric.mo, texture, sand, clay)
					if(!exists("temp.mo")) temp.mo <- get_Temp_mo(sc, runData, simTime)
					if(!exists("prcp.mo")) prcp.mo <- get_PPT_mo(sc, runData, simTime)
					if(!exists("PET.mo")) PET.mo <- get_PET_mo(sc, runData, simTime)

					#in case var(ppt or swp)==0 => cor is undefined: exclude those years
					temp <- by(data.frame(PET.mo$val, swpmatric.mo$top), simTime2$yearno_ForEachUsedMonth, cor2)
					resMeans[nv] <- mean(temp, na.rm = TRUE)
					resSDs[nv] <- sd(temp, na.rm = TRUE)

					if (length(bottomL) > 0 && !identical(bottomL, 0)) {
						temp <- by(data.frame(PET.mo$val, swpmatric.mo$bottom), simTime2$yearno_ForEachUsedMonth, cor2)
						resMeans[nv+1] <- mean(temp, na.rm = TRUE)
						resSDs[nv+1] <- sd(temp, na.rm = TRUE)
					}

					temp <- by(data.frame(temp.mo$mean, prcp.mo$ppt), simTime2$yearno_ForEachUsedMonth, cor2)
					resMeans[nv+2] <- mean(temp, na.rm = TRUE)
					resSDs[nv+2] <- sd(temp, na.rm = TRUE)

					nv <- nv+3
				}


				#---Aggregation: Climatic dryness
			#17
				if(aon$yearlymonthlyTemperateDrylandIndices){
					if(print.debug) print("Aggregation of yearlymonthlyTemperateDrylandIndices")
					if(!exists("prcp.yr")) prcp.yr <- get_PPT_yr(sc, runData, simTime)
					if(!exists("PET.yr")) PET.yr <- get_PET_yr(sc, runData, simTime)
					if(!exists("temp.mo")) temp.mo <- get_Temp_mo(sc, runData, simTime)

          di.ts <- calc_drylandindices(annualPPT = prcp.yr$ppt, annualPET = PET.yr$val,
                                      monthlyTemp = temp.mo$mean)

          meanmonthlyTemp <- tapply(temp.mo$mean, simTime2$month_ForEachUsedMonth, mean)
          di.normals <- calc_drylandindices(annualPPT = mean(prcp.yr$ppt),
                                      annualPET = mean(PET.yr$val),
                                      monthlyTemp = meanmonthlyTemp)

					resMeans[nv:(nv+2)] <- unlist(di.normals)
					resMeans[(nv+3):(nv+5)] <- sapply(di.ts, mean, na.rm = TRUE)
					resSDs[(nv+3):(nv+5)] <- sapply(di.ts, sd, na.rm = TRUE)
					nv <- nv+6

					rm(di.ts, di.normals)
				}
			#18
				if(aon$yearlyDryWetPeriods){
					if(print.debug) print("Aggregation of yearlyDryWetPeriods")
					if(!exists("prcp.yr")) prcp.yr <- get_PPT_yr(sc, runData, simTime)
					temp.rle <- rle(sign(prcp.yr$ppt - mean(prcp.yr$ppt)))

					resMeans[nv:(nv+1)] <- c(quantile(temp.rle$lengths[temp.rle$values==-1], probs=0.9, type=7), quantile(temp.rle$lengths[temp.rle$values==1], probs=0.9, type=7))
					nv <- nv+2

					rm(temp.rle)
				}
			#19
				if(aon$dailyWeatherGeneratorCharacteristics){#daily response to weather generator treatments
					if(print.debug) print("Aggregation of dailyWeatherGeneratorCharacteristics")
					if(!exists("prcp.dy")) prcp.dy <- get_PPT_dy(sc, runData, simTime)
					if(!exists("temp.dy")) temp.dy <- get_Temp_dy(sc, runData, simTime)

          # until SWSF v1.4.4: dws, dds, and tv were calculated as mean of all months
          # pooled across years
          # now: they are aggregated across years on the means for each month x year
          dws <- daily_spells_permonth(prcp.dy$ppt > 0, simTime2) # wet spells
          dds <- daily_spells_permonth(prcp.dy$ppt < tol, simTime2) # dry spells

          temp <- tapply(temp.dy$mean,
            simTime2$month_ForEachUsedDay_NSadj + 100 * simTime2$year_ForEachUsedDay_NSadj,
            sd)
          tv <- matrix(temp, nrow = 12)

					resMeans[nv+st_mo-1] <- apply(dws, 1, mean, na.rm = TRUE)
					resSDs[nv+st_mo-1] <- apply(dws, 1, sd, na.rm = TRUE)
					resMeans[nv+st_mo-1+12] <- apply(dds, 1, mean, na.rm = TRUE)
					resSDs[nv+st_mo-1+12] <- apply(dds, 1, sd, na.rm = TRUE)
					resMeans[nv+st_mo-1+24] <- apply(tv, 1, mean, na.rm = TRUE)
					resSDs[nv+st_mo-1+24] <- apply(tv, 1, sd, na.rm = TRUE)
					nv <- nv+36

					rm(dws, dds, tv)
				}
			#20
				if(aon$dailyPrecipitationFreeEventDistribution){	#daily weather frequency distributions
					if(print.debug) print("Aggregation of dailyPrecipitationFreeEventDistribution")
					if(!exists("prcp.dy")) prcp.dy <- get_PPT_dy(sc, runData, simTime)

          #duration of prcp-free days in bins
          ppt_free <- tabulate_values_in_bins(
            x = prcp.dy$ppt <= tol, method = "duration",
            bins = bin.prcpfreeDurations, nbins = 4,
            simTime = simTime, simTime2 = simTime2)

					resMeans[nv] <- mean(ppt_free$eventsPerYear)
					resSDs[nv] <- sd(ppt_free$eventsPerYear)
					resMeans[(nv+1):(nv+4)] <- apply(ppt_free$freq.summary, 1, mean)
					resSDs[(nv+1):(nv+4)] <- apply(ppt_free$freq.summary, 1, sd)
					nv <- nv+5

					rm(ppt_free)
				}
			#21
				if(aon$monthlySPEIEvents){
					if(print.debug) print("Aggregation of monthlySPEIEvents")
					require(SPEI)
					#standardized precipitation-evapotranspiration index, SPEI: Vicente-Serrano, S.M., Beguer, S., Lorenzo-Lacruz, J., Camarero, J.s.J., Lopez-Moreno, J.I., Azorin-Molina, C., Revuelto, J.s., Morn-Tejeda, E. & Sanchez-Lorenzo, A. (2012) Performance of Drought Indices for Ecological, Agricultural, and Hydrological Applications. Earth Interactions, 16, 1-27.
					if(!exists("PET.mo")) PET.mo <- get_PET_mo(sc, runData, simTime)
					if(!exists("prcp.mo")) prcp.mo <- get_PPT_mo(sc, runData, simTime)

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
			#22
				if(aon$monthlyPlantGrowthControls){	#Nemani RR, Keeling CD, Hashimoto H et al. (2003) Climate-Driven Increases in Global Terrestrial Net Primary Production from 1982 to 1999. Science, 300, 1560-1563.
					if(print.debug) print("Aggregation of monthlyPlantGrowthControls")
					if(!exists("temp.mo")) temp.mo <- get_Temp_mo(sc, runData, simTime)
					if(!exists("PET.mo")) PET.mo <- get_PET_mo(sc, runData, simTime)
					if(!exists("prcp.mo")) prcp.mo <- get_PPT_mo(sc, runData, simTime)

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
					cloudiness <- swCloud_SkyCover(swRunScenariosData[[sc]])
					cloudiness <- rep(cloudiness, times=simTime$no.useyr)

          temp <- (1 - ifelse(cloudiness < 10, 0, (cloudiness - 10) / 100 * 0.5 )) * DayNumber_ForEachUsedMonth
					control_radiation <- tapply(temp, simTime2$yearno_ForEachUsedMonth, sum) / DayNumber_ForEachUsedYear

					temp <- data.frame(control_temp, control_water, control_radiation)
					resMeans[nv:(nv+2)] <- apply(temp, 2, mean, na.rm=TRUE)
					resSDs[nv:(nv+2)] <- apply(temp, 2, sd, na.rm=TRUE)
					nv <- nv+3

					rm(DayNumber_ForEachUsedMonth, DayNumber_ForEachUsedYear, control_temp, control_water, control_radiation, aridity, temp, cloudiness)
				}
			#23
				if(aon$dailyC4_TempVar){#Variables to estimate percent C4 species in North America: Teeri JA, Stowe LG (1976) Climatic patterns and the distribution of C4 grasses in North America. Oecologia, 23, 1-12.
					if(print.debug) print("Aggregation of dailyC4_TempVar")
					if(!exists("temp.dy")) temp.dy <- get_Temp_dy(sc, runData, simTime)

					resMeans[nv:(nv+2)] <- (temp <- as.numeric(sw_dailyC4_TempVar(dailyTempMin=temp.dy$min, dailyTempMean=temp.dy$mean, simTime2)))[1:3]	#accountNSHemispheres_agg
					resSDs[nv:(nv+2)] <- temp[4:6]
					nv <- nv+3
				}
			#24
				if(aon$dailyDegreeDays){	#Degree days based on daily temp
					if(print.debug) print("Aggregation of dailyDegreeDays")
					if(!exists("temp.dy")) temp.dy <- get_Temp_dy(sc, runData, simTime)

					degday <- ifelse(temp.dy$mean > DegreeDayBase, temp.dy$mean - DegreeDayBase, 0) #degree days
					temp <- tapply(degday, simTime2$year_ForEachUsedDay, sum)

					resMeans[nv] <- mean(temp)
					resSDs[nv] <- sd(temp)
					nv <- nv+1

					rm(degday)
				}


				#---Aggregation: Yearly water balance
			#27.0
				if(aon$yearlyAET){
					if(print.debug) print("Aggregation of yearlyAET")
					if(!exists("AET.yr")) AET.yr <- get_AET_yr(sc, runData, simTime)

					resMeans[nv] <- mean(AET.yr$val)
					resSDs[nv] <- sd(AET.yr$val)
					nv <- nv+1
				}

			#27
				if(aon$yearlyWaterBalanceFluxes) {
					if(print.debug) print("Aggregation of yearlyWaterBalanceFluxes")
					if(!exists("prcp.yr")) prcp.yr <- get_PPT_yr(sc, runData, simTime)
					if(!exists("Esurface.yr")) Esurface.yr <- get_Esurface_yr(sc, runData, simTime)
					if(!exists("intercept.yr")) intercept.yr <- get_Interception_yr(sc, runData, simTime)
					if(!exists("inf.yr")) inf.yr <- get_Inf_yr(sc, runData, simTime)
					if(!exists("runoff.yr")) runoff.yr <- get_Runoff_yr(sc, runData, simTime)
					if(!exists("transp.yr")) transp.yr <- get_Response_aggL(sc, sw_transp, tscale = "yr", scaler = 10, FUN = sum, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)
					if(!exists("AET.yr")) AET.yr <- get_AET_yr(sc, runData, simTime)
					if(!exists("PET.yr")) PET.yr <- get_PET_yr(sc, runData, simTime)
					if(!exists("Esoil.yr")) Esoil.yr <- get_Response_aggL(sc, sw_evsoil, tscale = "yr", scaler = 10, FUN = sum, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)
					if(!exists("deepDrain.yr")) deepDrain.yr <- get_DeepDrain_yr(sc, runData, simTime)

					rain_toSoil <- prcp.yr$rain - intercept.yr$sum
					transp.tot <- transp.yr$top + transp.yr$bottom

					evap_soil.tot <- as.vector(Esoil.yr$top + Esoil.yr$bottom)
					evap.tot <- evap_soil.tot + Esurface.yr$sum + prcp.yr$snowloss

					temp1 <- 10 * slot(slot(runData[[sc]],sw_percolation),"Year")
					if(length(topL) > 1 && length(bottomL) > 0 && !identical(bottomL, 0)) {
						drain.topTobottom <- temp1[simTime$index.useyr, 1+DeepestTopLayer]
					} else {
						drain.topTobottom <- NA
					}
					temp1 <- 10 * slot(slot(runData[[sc]],sw_hd),"Year")
					if(length(topL) > 1) {
						hydred.topTobottom <- apply(temp1[simTime$index.useyr, 1+topL, drop=FALSE], 1, sum)
					} else {
						hydred.topTobottom <- temp1[simTime$index.useyr,1+topL]
					}

          temp1 <- 10 * slot(slot(runData[[sc]],sw_swcbulk),"Day")
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
          swc.flux <- tapply(swcdyflux, temp1[index.usedyPlusOne, 1], sum)

					#mean fluxes
					resMeans[nv:(nv+22)] <- apply(fluxtemp <- cbind(prcp.yr$rain, rain_toSoil, prcp.yr$snowfall, prcp.yr$snowmelt, prcp.yr$snowloss, intercept.yr$sum, intercept.yr$veg, intercept.yr$litter, Esurface.yr$veg, Esurface.yr$litter, inf.yr$inf, runoff.yr$val, evap.tot, evap_soil.tot, Esoil.yr$top, Esoil.yr$bottom, transp.tot, transp.yr$top, transp.yr$bottom, hydred.topTobottom, drain.topTobottom, deepDrain.yr$val, swc.flux), 2, mean)
					resMeans[nv+23] <- if (sum(transp.tot) > 0) mean(transp.yr$bottom/transp.tot) else 0
					resMeans[nv+24] <- if (sum(AET.yr$val) > 0) mean(transp.tot/AET.yr$val) else 0
					resMeans[nv+25] <- if (sum(AET.yr$val) > 0) mean(evap_soil.tot/AET.yr$val) else 0
					resMeans[nv+26] <- if (sum(PET.yr$val) > 0) mean(AET.yr$val/PET.yr$val) else 0
					resMeans[nv+27] <- if (sum(PET.yr$val) > 0) mean(transp.tot/PET.yr$val) else 0
					resMeans[nv+28] <- if (sum(PET.yr$val) > 0) mean(evap_soil.tot/PET.yr$val) else 0

					#sd of fluxes
					resSDs[nv:(nv+22)] <- apply(fluxtemp, 2, sd)
					resSDs[nv+23] <- if (sum(transp.tot) > 0) sd(transp.yr$bottom/transp.tot) else 0
					resSDs[nv+24] <- if (sum(AET.yr$val) > 0) sd(transp.tot/AET.yr$val) else 0
					resSDs[nv+25] <- if (sum(AET.yr$val) > 0) sd(evap_soil.tot/AET.yr$val) else 0
					resSDs[nv+26] <- if (sum(PET.yr$val) > 0) sd(AET.yr$val/PET.yr$val) else 0
					resSDs[nv+27] <- if (sum(PET.yr$val) > 0) sd(transp.tot/PET.yr$val) else 0
					resSDs[nv+28] <- if (sum(PET.yr$val) > 0) sd(evap_soil.tot/PET.yr$val) else 0

					nv <- nv+29

					rm(rain_toSoil, transp.tot, evap_soil.tot, drain.topTobottom, hydred.topTobottom, index.usedyPlusOne, swcdyflux, swc.flux)
				}

			#27.2
				if(aon$dailySoilWaterPulseVsStorage){
					if(print.debug) print("Aggregation of dailySoilWaterPulseVsStorage")
					if(!exists("inf.dy")) inf.dy <- get_Inf_dy(sc, runData, simTime)
					if(!exists("transp.dy.all")) transp.dy.all <- get_Response_aggL(sc, sw_transp, tscale = "dyAll", scaler = 10, FUN = sum, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)
					if(!exists("Esoil.dy.all")) Esoil.dy.all <- get_Response_aggL(sc, sw_evsoil, tscale = "dyAll", scaler = 10, FUN = sum, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)
					if(!exists("deepDrain.dy")) deepDrain.dy <- get_DeepDrain_dy(sc, runData, simTime)

          percolation <- if (d > 1) {
              10 * slot(slot(runData[[sc]],sw_percolation), "Day")[simTime$index.usedy, 2 + ld[-d]]
            } else {
              rep(0, simTime$no.usedy)
            }
          hydred <- 10 * slot(slot(runData[[sc]],sw_hd), "Day")[simTime$index.usedy, 2 + ld]

					# Water balance
					outputs_by_layer <- inputs_by_layer <- matrix(0, nrow = simTime$no.usedy, ncol = d,
					  dimnames = list(NULL, paste0("total_Lyr_", ld)))
					# Inputs: infiltration + received hydraulic redistribution + received percolation
					inputs_by_layer[, 1] <- inputs_by_layer[, 1] + inf.dy$inf
					inputs_by_layer <- inputs_by_layer + ifelse(hydred > 0, hydred, 0)
          if (d > 1) {
            inputs_by_layer[, -1] <- inputs_by_layer[, -1] + ifelse(percolation > 0, percolation, 0)
          }

					# Outputs: soil evaporation + transpiration + deep drainage + hydraulic redistribution donor + percolation donor
          if (ncol(Esoil.dy.all$val) > 2) {
            itemp <- seq_len(ncol(Esoil.dy.all$val) - 2)
            outputs_by_layer[, itemp] <- outputs_by_layer[, itemp] +
              Esoil.dy.all$val[simTime$index.usedy, -(1:2)]
          }
          itemp <- grepl("transp_total", colnames(transp.dy.all$val))
          if (any(itemp)) {
            itemp <- seq_len(sum(itemp))
            outputs_by_layer[, itemp] <- outputs_by_layer[, itemp] +
              transp.dy.all$val[simTime$index.usedy, itemp]
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
					storage_use <- by(cbind(extraction, outputs_by_layer), INDICES=simTime2$year_ForEachUsedDay_NSadj, FUN=function(x) {
            res1 <- apply(x[, ld, drop = FALSE], MARGIN=2, FUN=rle)
            res2 <- apply(x[, soilLayers_N + ld, drop = FALSE], MARGIN=2, FUN=function(y) list(out=y))
            modifyList(res1, res2)
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
            extraction_duration_days <- matrix(extraction_duration_days, nrow = d, ncol = simTime$no.useyr)
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
                x <- tapply(dat$out, INDEX=storage_ids, sum) # sum up extracted water for each extraction spell
                if (has_zero && length(x) > 0)
                  x <- x[-1] # remove first element because this represents the positive spells (id = 0)

                sum(x)
              }
						}))
          if (!is.matrix(extraction_summed_mm)) {
            extraction_summed_mm <- matrix(extraction_summed_mm, nrow = d, ncol = simTime$no.useyr)
          }

					# aggregate across years for each soil layer
					resMeans[nv:(nv+soilLayers_N-1)] <- round(apply(extraction_duration_days, 1, mean), 1)
					resSDs[nv:(nv+soilLayers_N-1)] <- round(apply(extraction_duration_days, 1, sd), 1)
					nv <- nv+SoilLayer_MaxNo
					resMeans[nv:(nv+soilLayers_N-1)] <- round(apply(extraction_summed_mm, 1, mean), 2)
					resSDs[nv:(nv+soilLayers_N-1)] <- round(apply(extraction_summed_mm, 1, sd), 2)
					nv <- nv+SoilLayer_MaxNo

					rm(percolation, hydred, inputs_by_layer, outputs_by_layer, balance, extraction, storage_use, extraction_duration_days, extraction_summed_mm)
				}


				#---Aggregation: Daily extreme values
			#28
				if(aon$dailyTranspirationExtremes) {#mean and SD of DOY and value of minimum/maximum
					if(print.debug) print("Aggregation of dailyTranspirationExtremes")
					if(!exists("transp.dy")) transp.dy <- get_Response_aggL(sc, sw_transp, tscale = "dy", scaler = 10, FUN = sum, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)

          temp <- transp.dy$top + transp.dy$bottom
          temp <- tapply(temp, simTime2$year_ForEachUsedDay, extreme_values_and_doys)
					extremes <- matrix(unlist(temp), ncol = 4, byrow = TRUE)

          temp <- extremes[, 1:2, drop = FALSE]
					resMeans[nv:(nv+1)] <- apply(temp, MARGIN=2, FUN=mean)
					resSDs[nv:(nv+1)] <- apply(temp, MARGIN=2, FUN=sd)
					nv <- nv+2

          temp <- extremes[, 3:4, drop = FALSE]
					resMeans[nv:(nv+1)] <- apply(temp, MARGIN=2, circ.mean, int = 365)
					resSDs[nv:(nv+1)] <- apply(temp, MARGIN=2, circ.sd, int = 365)
					nv <- nv+2

					rm(extremes)
				}
			#29
				if(aon$dailyTotalEvaporationExtremes) {
					if(print.debug) print("Aggregation of dailyTotalEvaporationExtremes")
					if(!exists("Esoil.dy")) Esoil.dy <- get_Response_aggL(sc, sw_evsoil, tscale = "dy", scaler = 10, FUN = sum, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)
					if(!exists("Esurface.dy")) Esurface.dy <- get_Esurface_dy(sc, runData, simTime)

          temp <- Esoil.dy$top + Esoil.dy$bottom + Esurface.dy$sum
          temp <- tapply(temp, simTime2$year_ForEachUsedDay, extreme_values_and_doys)
					extremes <- matrix(unlist(temp), ncol = 4, byrow = TRUE)

          temp <- extremes[, 1:2, drop = FALSE]
					resMeans[nv:(nv+1)] <- apply(temp, MARGIN=2, FUN=mean)
					resSDs[nv:(nv+1)] <- apply(temp, MARGIN=2, FUN=sd)
					nv <- nv+2

          temp <- extremes[, 3:4, drop = FALSE]
					resMeans[nv:(nv+1)] <- apply(temp, MARGIN=2, circ.mean, int = 365)
					resSDs[nv:(nv+1)] <- apply(temp, MARGIN=2, circ.sd, int = 365)
					nv <- nv+2

					rm(extremes)
				}
			#30
				if(aon$dailyDrainageExtremes) {
					if(print.debug) print("Aggregation of dailyDrainageExtremes")
					if(!exists("deepDrain.dy")) deepDrain.dy <- get_DeepDrain_dy(sc, runData, simTime)

          temp <- tapply(deepDrain.dy$val, simTime2$year_ForEachUsedDay, extreme_values_and_doys)
					extremes <- matrix(unlist(temp), ncol = 4, byrow = TRUE)

          temp <- extremes[, 1:2, drop = FALSE]
					resMeans[nv:(nv+1)] <- apply(temp, MARGIN=2, FUN=mean)
					resSDs[nv:(nv+1)] <- apply(temp, MARGIN=2, FUN=sd)
					nv <- nv+2

          temp <- extremes[, 3:4, drop = FALSE]
					resMeans[nv:(nv+1)] <- apply(temp, MARGIN=2, circ.mean, int = 365)
					resSDs[nv:(nv+1)] <- apply(temp, MARGIN=2, circ.sd, int = 365)
					nv <- nv+2

					rm(extremes)
				}
			#31
				if(aon$dailyInfiltrationExtremes) {
					if(print.debug) print("Aggregation of dailyInfiltrationExtremes")
					if(!exists("inf.dy")) inf.dy <- get_Inf_dy(sc, runData, simTime)

          temp <- tapply(inf.dy$inf, simTime2$year_ForEachUsedDay, extreme_values_and_doys)
					extremes <- matrix(unlist(temp), ncol = 4, byrow = TRUE)

          temp <- extremes[, 1:2, drop = FALSE]
					resMeans[nv:(nv+1)] <- apply(temp, MARGIN=2, FUN=mean)
					resSDs[nv:(nv+1)] <- apply(temp, MARGIN=2, FUN=sd)
					nv <- nv+2

          temp <- extremes[, 3:4, drop = FALSE]
					resMeans[nv:(nv+1)] <- apply(temp, MARGIN=2, circ.mean, int = 365)
					resSDs[nv:(nv+1)] <- apply(temp, MARGIN=2, circ.sd, int = 365)
					nv <- nv+2

					rm(extremes)
				}
			#32
				if(aon$dailyAETExtremes) {
					if(print.debug) print("Aggregation of dailyAETExtremes")
					if(!exists("AET.dy")) AET.dy <- get_AET_dy(sc, runData, simTime)

          temp <- tapply(AET.dy$val, simTime2$year_ForEachUsedDay, extreme_values_and_doys)
					extremes <- matrix(unlist(temp), ncol = 4, byrow = TRUE)

          temp <- extremes[, 1:2, drop = FALSE]
					resMeans[nv:(nv+1)] <- apply(temp, MARGIN=2, FUN=mean)
					resSDs[nv:(nv+1)] <- apply(temp, MARGIN=2, FUN=sd)
					nv <- nv+2

          temp <- extremes[, 3:4, drop = FALSE]
					resMeans[nv:(nv+1)] <- apply(temp, MARGIN=2, circ.mean, int = 365)
					resSDs[nv:(nv+1)] <- apply(temp, MARGIN=2, circ.sd, int = 365)
					nv <- nv+2

					rm(extremes)
				}
			#33
				if(aon$dailySWPextremes){
					if(print.debug) print("Aggregation of dailySWPextremes")
					if(!exists("vwcmatric.dy")) vwcmatric.dy <- get_Response_aggL(sc, sw_vwcmatric, tscale = "dy", scaler = 1, FUN = weighted.mean, weights = layers_width, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)
					if(!exists("swpmatric.dy")) swpmatric.dy <- get_SWPmatric_aggL(vwcmatric.dy, texture, sand, clay)

          extremes <- matrix(NA, nrow = simTime$no.useyr, ncol = 2 * 4)
          temp <- tapply(swpmatric.dy$top, simTime2$year_ForEachUsedDay, extreme_values_and_doys)
          extremes[, 1:4] <- matrix(unlist(temp), ncol = 4, byrow = TRUE)
          if(length(bottomL) > 0 && !identical(bottomL, 0)) {
            temp <- tapply(swpmatric.dy$bottom, simTime2$year_ForEachUsedDay, extreme_values_and_doys)
            extremes[, 5:8] <- matrix(unlist(temp), ncol = 4, byrow = TRUE)
          }

          temp <- extremes[, c(1:2, 5:6), drop = FALSE]
					resMeans[nv:(nv+3)] <- apply(temp, MARGIN=2, FUN=mean, na.rm=TRUE)
					resSDs[nv:(nv+3)] <- apply(temp, MARGIN=2, FUN=sd, na.rm=TRUE)
					nv <- nv+4

          temp <- extremes[, c(3:4, 7:8), drop = FALSE]
					resMeans[nv:(nv+3)] <- apply(temp, MARGIN=2, circ.mean, int = 365, na.rm = TRUE)
					resSDs[nv:(nv+3)] <- apply(temp, MARGIN=2, circ.sd, int = 365, na.rm = TRUE)
					nv <- nv+4

					rm(extremes)
				}
			#34
				if(aon$dailyRechargeExtremes){
					if(print.debug) print("Aggregation of dailyRechargeExtremes")
					if(!exists("swcbulk.dy")) swcbulk.dy <- get_Response_aggL(sc, sw_swcbulk, tscale = "dy", scaler = 10, FUN = sum, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)

					recharge.dy <- NULL
					recharge.dy$top <- swcbulk.dy$top / (SWPtoVWC(-0.033, texture$sand.top, texture$clay.top) * 10 * sum(layers_width[topL]))
          extremes <- matrix(NA, nrow = simTime$no.useyr, ncol = 2 * 4)
          temp <- tapply(recharge.dy$top, simTime2$year_ForEachUsedDay, extreme_values_and_doys)
          extremes[, 1:4] <- matrix(unlist(temp), ncol = 4, byrow = TRUE)

					if(length(bottomL) > 0 && !identical(bottomL, 0)) {
						recharge.dy$bottom <- swcbulk.dy$bottom / (SWPtoVWC(-0.033, texture$sand.bottom, texture$clay.bottom) * 10 * sum(layers_width[bottomL]))
            temp <- tapply(recharge.dy$bottom, simTime2$year_ForEachUsedDay, extreme_values_and_doys)
            extremes[, 5:8] <- matrix(unlist(temp), ncol = 4, byrow = TRUE)
					}

          temp <- extremes[, c(1:2, 5:6), drop = FALSE]
					resMeans[nv:(nv+3)] <- apply(temp, MARGIN=2, FUN=function(x) mean(pmin(1, x), na.rm=TRUE))
					resSDs[nv:(nv+3)] <- apply(temp, MARGIN=2, FUN=function(x) sd(pmin(1, x), na.rm=TRUE))
					nv <- nv+4

          temp <- extremes[, c(3:4, 7:8), drop = FALSE]
					resMeans[nv:(nv+3)] <- apply(temp, MARGIN=2, circ.mean, int = 365, na.rm=TRUE)
					resSDs[nv:(nv+3)] <- apply(temp, MARGIN=2, circ.sd, int = 365, na.rm=TRUE)
					nv <- nv+4

					rm(recharge.dy, extremes)
				}

				#---Aggregation: Ecological dryness
			#35a
        regimes_done <- FALSE
        if (aon$dailyNRCS_SoilMoistureTemperatureRegimes) {
          if (print.debug) print("Aggregation of dailyNRCS_SoilMoistureTemperatureRegimes")
          #Based on references provided by Chambers, J. C., D. A. Pyke, J. D. Maestas, M. Pellant, C. S. Boyd, S. B. Campbell, S. Espinosa, D. W. Havlina, K. E. Mayer, and A. Wuenschel. 2014. Using Resistance and Resilience Concepts to Reduce Impacts of Invasive Annual Grasses and Altered Fire Regimes on the Sagebrush Ecosystem and Greater Sage-Grouse: A Strategic Multi-Scale Approach. Gen. Tech. Rep. RMRS-GTR-326. U.S. Department of Agriculture, Forest Service, Rocky Mountain Research Station, Fort Collins, CO.
          #Soil Survey Staff. 2014. Keys to soil taxonomy, 12th ed., USDA Natural Resources Conservation Service, Washington, DC.
          #Soil Survey Staff. 2010. Keys to soil taxonomy, 11th ed., USDA Natural Resources Conservation Service, Washington, DC.

          #Result containers
          temp <- c("Hyperthermic", "Thermic", "Mesic", "Frigid", "Cryic", "Gelic")
          Tregime <- rep(0, length(temp))
          names(Tregime) <- temp
          temp <- c("Anyhydrous", "Aridic", "Udic", "Ustic", "Xeric")
          Sregime <- rep(0, length(temp))
          names(Sregime) <- temp

          MCS_depth <- Lanh_depth <- rep(NA, 2)
          Fifty_depth <- permafrost <- CSPartSummer <- NA
          MATLanh <- MAT50 <- T50jja <- T50djf <- NA
          Lanh_annual_means <- rep(NA, 3)
          Cond_annual_means <- rep(NA, 17)

          if (swSite_SoilTemperatureFlag(swRunScenariosData[[sc]])) { #we need soil temperature
            if (!exists("soiltemp.dy.all")) soiltemp.dy.all <- get_Response_aggL(sc, sw_soiltemp, tscale = "dyAll", scaler = 1, FUN = weighted.mean, weights = layers_width, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)

            if (!anyNA(soiltemp.dy.all$val) && all(soiltemp.dy.all$val[, -(1:2)] < 100)) {
              # 100 C as upper realistic limit from Garratt, J.R. (1992). Extreme maximum land surface temperatures. Journal of Applied Meteorology, 31, 1096-1105.
              if (!exists("soiltemp.yr.all")) soiltemp.yr.all <- get_Response_aggL(sc, sw_soiltemp, tscale = "yrAll", scaler = 1, FUN = weighted.mean, weights = layers_width, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)
              if (!exists("soiltemp.mo.all")) soiltemp.mo.all <- get_Response_aggL(sc, sw_soiltemp, tscale = "moAll", scaler = 1, FUN = weighted.mean, weights = layers_width, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)
              if (!exists("vwcmatric.dy.all")) vwcmatric.dy.all <- get_Response_aggL(sc, sw_vwcmatric, tscale = "dyAll", scaler = 1, FUN = weighted.mean, weights = layers_width, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)
              if (!exists("swpmatric.dy.all")) swpmatric.dy.all <- get_SWPmatric_aggL(vwcmatric.dy.all, texture, sand, clay)
              if (!exists("prcp.yr")) prcp.yr <- get_PPT_yr(sc, runData, simTime)
              if (!exists("prcp.mo")) prcp.mo <- get_PPT_mo(sc, runData, simTime)

              #Parameters
              SWP_dry <- -1.5	#dry means SWP below -1.5 MPa (Soil Survey Staff 2014: p.29)
              SWP_sat <- -0.033	#saturated means SWP above -0.033 MPa
              impermeability <- 0.9 #impermeable layer

              #Required soil layers
              soildat <- swSoils_Layers(swRunScenariosData[[sc]])[, c("depth_cm", "sand", "clay", "imperm"), drop = FALSE]
              #50cm soil depth or impermeable layer (whichever is shallower; Soil Survey Staff 2014: p.31)
              imp_depth <- which(soildat[, "imperm"] >= impermeability)
              imp_depth <- min(imp_depth, max(soildat[, "depth_cm"]))	#Interpret maximum soil depth as possible impermeable layer
              Fifty_depth <- min(50, imp_depth)

              #Definition of MCS (Soil Survey Staff 2014: p.29): The moisture control section (MCS) of a soil: the depth to which a dry (tension of more than 1500 kPa, but not air-dry) soil will be moistened by 2.5 cm of water within 24 hours. The lower boundary is the depth to which a dry soil will be moistened by 7.5 cm of water within 48 hours.
              sand_temp <- weighted.mean(sand, layers_width)
              clay_temp <- weighted.mean(clay, layers_width)
              #Practical depth definition of MCS
              #	- 10 to 30 cm below the soil surface if the particle-size class of the soil is fine-loamy, coarse-silty, fine-silty, or clayey
              #	- 20 to 60 cm if the particle-size class is coarse-loamy
              #	- 30 to 90 cm if the particle-size class is sandy.
              MCS_depth <- if(clay_temp >= 0.18) { c(10, 30)
                } else if(sand_temp < 0.15){ c(10, 30)
                } else if(sand_temp >= 0.50){ c(30, 90)
                } else c(20, 60)
              #If 7.5 cm of water moistens the soil to a densic, lithic, paralithic, or petroferric contact or to a petrocalcic or petrogypsic horizon or a duripan, the contact or the upper boundary of the cemented horizon constitutes the lower boundary of the soil moisture control section. If a soil is moistened to one of these contacts or horizons by 2.5 cm of water, the soil moisture control section is the boundary of the contact itself. The control section of such a soil is considered moist if the contact or upper boundary of the cemented horizon has a thin film of water. If that upper boundary is dry, the control section is considered dry.

              MCS_depth <- adjustLayer_byImp(depths = MCS_depth, imp_depth = imp_depth, sdepths = soildat[, "depth_cm"])

              #Soil layer 10-70 cm used for anhydrous layer definition; adjusted for impermeable layer
              Lanh_depth <- adjustLayer_byImp(depths = c(10, 70), imp_depth = imp_depth, sdepths = soildat[, "depth_cm"])

              #Permafrost (Soil Survey Staff 2014: p.28) is defined as a thermal condition in which a material (including soil material) remains below 0 C for 2 or more years in succession
              permafrost <- any(apply(soiltemp.yr.all$val[simTime$index.useyr, -1, drop = FALSE], 2, function(x) {
                temp <- rle(x < 0)
                any(temp$values) && any(temp$lengths[temp$values] >= 2)
              }))

              #Set soil depths and intervals accounting for shallow soil profiles: Soil Survey Staff 2014: p.31)
              soiltemp_nrsc <- list(
                yr = list(data = soiltemp.yr.all$val, nheader = 1),
                mo = list(data = soiltemp.mo.all$val, nheader = 2),
                dy = list(data = soiltemp.dy.all$val, nheader = 2)
              )
              vwc_dy_nrsc <- vwcmatric.dy.all

              ##Calculate soil temperature at necessary depths using a weighted mean
              i_depth50 <- findInterval(Fifty_depth, soildat[, "depth_cm"])
              calc50 <- !(Fifty_depth == soildat[i_depth50, "depth_cm"])
              if (calc50) {
                weights50 <- abs(Fifty_depth - soildat[i_depth50 + c(1, 0), "depth_cm"])
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
                weightsMCS <- abs(MCS_depth[k] - soildat[i_MCS[k] + c(1, 0), "depth_cm"])
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
                weightsLanh <- abs(Lanh_depth[k] - soildat[i_Lanh[k] + c(1, 0), "depth_cm"])
                soildat <- t(add_layer_to_soil(t(soildat), i_Lanh[k], weightsLanh))
                i_Lanh <- findInterval(Lanh_depth, soildat[, "depth_cm"])

                soiltemp_nrsc <- lapply(soiltemp_nrsc, function(st)
                  list(data = add_layer_to_soil(st[["data"]], st[["nheader"]] + i_Lanh[k], weightsLanh),
                       nheader = st[["nheader"]]))
                vwc_dy_nrsc$val <- add_layer_to_soil(vwc_dy_nrsc$val, 2 + i_Lanh[k], weightsLanh)
                rm(weightsLanh)
              }

              if (calc50 || any(calcMCS) || any(calcLanh)) {
                if (!be.quiet) print(paste0(i_label, " interpolated soil layers for NRCS soil regimes",
                    " because of insufficient soil layers: required would be {",
                      paste(sort(unique(c(Fifty_depth, MCS_depth, Lanh_depth))), collapse = ", "),
                      "} and available are {",
                      paste(layers_depth, collapse = ", "), "}"))

                swp_dy_nrsc <- get_SWPmatric_aggL(vwc_dy_nrsc, texture = texture,
                  sand = soildat[, "sand"], clay = soildat[, "clay"])

              } else {
                swp_dy_nrsc <- swpmatric.dy.all
              }

              soiltemp_nrsc <- lapply(soiltemp_nrsc, function(st) st[["data"]])
              swp_dy_nrsc <- swp_dy_nrsc$val[simTime$index.usedy, -(1:2), drop = FALSE]

              #MCS (Soil Survey Staff 2014: p.29)
              #What soil layer info used for MCS
              i_MCS <- identify_soillayers(MCS_depth, soildat[, "depth_cm"])
              #Repeat for Anhydrous soil layer moisture delineation
              i_Lanh <- identify_soillayers(Lanh_depth, soildat[, "depth_cm"])

              #mean soil temperature in Lahn depths (10 - 70 cm)
              MATLanh <- apply(soiltemp_nrsc[["yr"]][simTime$index.useyr, 1 + i_Lanh, drop = FALSE], 1,
                weighted.mean, w = soildat[i_Lanh, "depth_cm"])

              #---Calculate variables
              #Water year starting Oct 1
              wateryears <- simTime2$year_ForEachUsedDay_NSadj + ifelse(simTime2$doy_ForEachUsedDay_NSadj > 273, 1, 0)	# 1. water-year: N-hemisphere: October 1st = 1 day of water year; S-hemisphere: April 1st = 1 day of water year
              wyears <- (temp <- unique(wateryears))[-length(temp)]#eliminate last year

              #mean soil temperatures at 50cm depth
              MAT50 <- soiltemp_nrsc[["yr"]][simTime$index.useyr, 1 + i_depth50]
              T50jja <- soiltemp_nrsc[["mo"]][simTime$index.usemo, 2 + i_depth50][simTime2$month_ForEachUsedMonth_NSadj %in% 6:8]
              T50jja <- apply(matrix(T50jja, ncol = simTime$no.useyr), 2, mean)
              T50djf <- soiltemp_nrsc[["mo"]][simTime$index.usemo, 2 + i_depth50][simTime2$month_ForEachUsedMonth_NSadj %in% c(12, 1:2)]
              T50djf <- apply(matrix(T50djf, ncol = simTime$no.useyr), 2, mean)
              T50 <- soiltemp_nrsc[["dy"]][simTime$index.usedy, 2 + i_depth50]
              #Moist and dry at 50cm depth for MCS and Lahn calcs

              #CSPartSummer: Is the soil saturated with water during some part of the summer June1 (=regular doy 244) - Aug31 (=regular doy 335)
              isummer <- simTime2$doy_ForEachUsedDay_NSadj >= 244 & simTime2$doy_ForEachUsedDay_NSadj <= 335
              CSPartSummer <- mean(vapply(wyears, function(yr) {
                temp <- apply(swp_dy_nrsc[wateryears == yr & isummer, , drop = FALSE], 1,
                  function(x) all(x >= SWP_sat))
                rtemp <- rle(temp)
                if(any(rtemp$values)) max(rtemp$lengths[rtemp$values]) else 0
              }, FUN.VALUE = NA_real_))

              #---Soil temperature regime: based on Chambers et al. 2014: Appendix 3 and on Soil Survey Staff 2010: p.28/Soil Survey Staff 2014: p.31
              #we ignore distinction between iso- and not iso-
              if (mean(MAT50) >= 22) {
                Tregime["Hyperthermic"] <- 1L
              } else if (mean(MAT50) >= 15){
                Tregime["Thermic"] <- 1L
              } else if (mean(MAT50) >= 8){
                Tregime["Mesic"] <- 1L
              } else if (mean(MAT50) < 8 && mean(MAT50) > 0) {
                if(CSPartSummer > 0) { #ignoring organic soils, Saturated with water
                  if(mean(T50jja) > 0 && mean(T50jja) < 13)  { #ignoring O-horizon
                    Tregime["Cryic"] <- 1L
                  } else {
                    Tregime["Frigid"] <- 1L
                  }
                } else {#ignoring O-horizon and histic epipedon, Not saturated with water
                  if (mean(T50jja) > 0 && mean(T50jja) < 15) {
                    Tregime["Cryic"] <- 1L
                  } else {
                    Tregime["Frigid"] <- 1L
                  }
                }
              } else if (mean(MAT50) <= 0 || permafrost) { #limit should be 1 C for Gelisols
                Tregime["Gelic"] <- 1L
              }

              #Normal years for soil moisture regimes (Soil Survey Staff 2014: p.29)
              #Should have a time period of 30 years to determine normal years
              MAP <- c(mean(prcp.yr$ppt), sd(prcp.yr$ppt))
              normal1 <- (prcp.yr$ppt >= MAP[1] - MAP[2]) & (prcp.yr$ppt <= MAP[1] + MAP[2])
              MMP <- tapply(prcp.mo$ppt,
                simTime2$month_ForEachUsedMonth_NSadj,
                function(x) c(mean(x), sd(x)))
              MMP <- matrix(unlist(MMP), nrow = 2, ncol = 12)
              normal2 <- tapply(prcp.mo$ppt, simTime2$yearno_ForEachUsedMonth_NSadj,
                function(x) sum((x >= MMP[1, ] - MMP[2, ]) & (x <= MMP[1, ] + MMP[2, ])) >= 8)
              # Normal years =
              #   - Annual precipitation that is plus or minus one standard precipitation
              #   - and Mean monthly precipitation that is plus or minus one standard deviation of the long-term monthly precipitation for 8 of the 12 months
              wyears_normal <- wyears[normal1 & normal2]
              wyears_index <- findInterval(wyears_normal, wyears)
              wdays_index <- wateryears %in% wyears_normal
              days_per_wyear <- as.integer(table(wateryears[wdays_index], dnn = FALSE))

              if (length(wyears_normal) > 2) {
                #Structures used Lanh delinieation
                #Days are moists in half of the Lanh soil depth (and not soil layers!)
                n_Lanh <- length(i_Lanh)
                width_Lanh <- diff(c(0, soildat[, "depth_cm"]))[i_Lanh] # stopifnot(sum(width_Lanh) == Lanh_depth[2] - Lanh_depth[1])
                temp <- swp_dy_nrsc[wdays_index, i_Lanh, drop = FALSE] > SWP_dry
                temp <- temp * matrix(width_Lanh, nrow = sum(wdays_index), ncol = length(i_Lanh), byrow = TRUE)
                Lanh_Dry_Half <- .rowSums(temp, m = sum(wdays_index), n = n_Lanh) <= sum(width_Lanh) / 2

                #Conditions for Anhydrous soil delineation
                LanhConditionalDF <- data.frame(
                  Years = rep(wyears_normal, days_per_wyear),
                  T50_at0C = T50[wdays_index] > 0, # days where T @ 50 is > 0 C
                  Lanh_Dry_Half = Lanh_Dry_Half,
                  MAT50 = rep(MAT50[wyears_index], days_per_wyear),
                  MATLanh = rep(MATLanh[wyears_index], days_per_wyear)
                )
                #Mean Annual soil temperature is less than or equal to 0C
                LanhConditionalDF$COND1 <- LanhConditionalDF$MAT50 <= 0
                #Soil temperature in the Lahn Depth is never greater than 5
                LanhConditionalDF$COND2 <- LanhConditionalDF$MATLanh <= 5
                #In the Lahn Depth, 1/2 of soil dry > 1/2 CUMULATIVE days when Mean Annual ST > 0C
                LanhConditionalDF$COND3_Test <- LanhConditionalDF$Lanh_Dry_Half == LanhConditionalDF$T50_at0C #TRUE = where are both these conditions met
                temp <- with(LanhConditionalDF, tapply(COND3_Test, Years, sum))
                LanhConditionalDF$HalfDryDaysCumAbove0C <- rep(temp, days_per_wyear)
                temp <- with(LanhConditionalDF, tapply(T50_at0C, Years, sum))
                LanhConditionalDF$SoilAbove0C <- rep(temp, days_per_wyear)
                LanhConditionalDF$COND3 <- LanhConditionalDF$HalfDryDaysCumAbove0C > .5 * LanhConditionalDF$SoilAbove0C #TRUE = Half of soil layers are dry greater than half the days where MAST >0c
                LanhConditionalDF3 <- apply(aggregate(LanhConditionalDF[, c('COND1', 'COND2', 'COND3')],
                                                      by = list(LanhConditionalDF$Years),
                                                      function(x) sum(x) >= sum(!x)),
                                            2, function(x) sum(x) >= sum(!x))

                #Structures used for MCS delineation
                ConditionalDF <- data.frame(
                  Years = rep(wyears_normal, days_per_wyear),
                  DOY = simTime2$doy_ForEachUsedDay_NSadj[wdays_index],
                  MAT50 = rep(MAT50[wyears_index], days_per_wyear),
                  T50_at5C = T50[wdays_index] > 5, # days where T @ 50cm exceeds 5C
                  T50_at8C = T50[wdays_index] > 8, # days where T @ 50cm exceeds 8C
                  MCS_Moist_All = apply(swp_dy_nrsc[wdays_index, i_MCS, drop = FALSE] > SWP_dry, 1, all),
                  MCS_Dry_All = apply(swp_dy_nrsc[wdays_index, i_MCS, drop = FALSE] < SWP_dry, 1, all),
                  T50jja = rep(T50jja[wyears_index], days_per_wyear),
                  T50djf = rep(T50djf[wyears_index], days_per_wyear)
                )

                #COND1 - Dry in ALL parts for more than half of the CUMULATIVE days per year when the soil temperature at a depth of 50cm is above 5C
                ConditionalDF$COND1_Test <- ConditionalDF$MCS_Dry_All & ConditionalDF$T50_at5C	#TRUE = where are both these conditions met
                temp <- with(ConditionalDF, tapply(COND1_Test, Years, sum))
                ConditionalDF$DryDaysCumAbove5C <- rep(temp, days_per_wyear)
                temp <- with(ConditionalDF, tapply(T50_at5C, Years, sum))
                ConditionalDF$SoilAbove5C <- rep(temp, days_per_wyear)
                ConditionalDF$COND1 <- ConditionalDF$DryDaysCumAbove5C > .5 * ConditionalDF$SoilAbove5C #TRUE =Soils are dry greater than 1/2 cumulative days/year

                #COND1.1 - Moist in SOME or ALL parts for more than half of the CUMMULATIVE days per year when the soil temperature at a depth of 50cm is above 5
                #!MCS_Dry_All = MCS_ Moist Any; !MCS_Moist_All = MCS_Dry Any
                #This Test is kind of redundant basically if COND1 is TRUE than
                ConditionalDF$COND1_1_Test <- !ConditionalDF$MCS_Dry_All & ConditionalDF$T50_at5C	#TRUE = where are both these conditions met
                temp <-  with(ConditionalDF, tapply(COND1_1_Test, Years, sum))
                ConditionalDF$AnyMoistDaysCumAbove5C <- rep(temp, days_per_wyear)
                ConditionalDF$COND1_1 <- ConditionalDF$AnyMoistDaysCumAbove5C > .5 * ConditionalDF$SoilAbove5C
                #Cond2 - Moist in SOME or all parts for less than 90 CONSECUTIVE days when the the soil temperature at a depth of 50cm is above 8C
                ConditionalDF$COND2_Test <- !ConditionalDF$MCS_Dry_All & ConditionalDF$T50_at8C	#TRUE = where are both these conditions met
                temp <- with(ConditionalDF, tapply(COND2_Test, Years, max.duration)) # Maximum consecutive days
                ConditionalDF$COND2 <- rep(temp < 90, days_per_wyear) # TRUE = moist less than 90 consecutive days during >8 C soils, FALSE = moist more than 90 consecutive days

                #COND3 - MCS is Not dry in ANY part as long as 90 CUMULATIVE days - Can't be dry longer than 90 cum days
                temp <- with(ConditionalDF, tapply(!MCS_Moist_All, Years, sum)) #Number of days where any soils are dry
                ConditionalDF$DryDaysCumAny <- rep(temp, days_per_wyear)
                ConditionalDF$COND3 <- ConditionalDF$DryDaysCumAny < 90 #TRUE = Not Dry for as long 90 cumlative days,FALSE = Dry as long as as 90 Cumlative days

                #COND4 - The means annual soil temperature at 50cm is < or > 22C
                ConditionalDF$COND4 <- ConditionalDF$MAT50 > 22 #TRUE - Greater than 22, False - Less than 22

                #COND5 - The absolute difference between the temperature in winter @ 50cm and the temperature in summer @ 50cm is > or < 6
                ConditionalDF$COND5 <- abs(ConditionalDF$T50djf - ConditionalDF$T50jja) > 6 #TRUE - Greater than 6, FALSE - Less than 6

                #COND6 - Dry in ALL parts LESS than 45 CONSECUTIVE days in the 4 months following the summer solstice
                temp <- with(ConditionalDF[ConditionalDF$DOY %in% c(172:293),], tapply(MCS_Dry_All, Years, max.duration))  #Consecutive days of dry soil after summer solsitice
                ConditionalDF$DryDaysConsecSummer <- rep(temp, days_per_wyear)
                ConditionalDF$COND6 <- ConditionalDF$DryDaysConsecSummer < 45 # TRUE = dry less than 45 consecutive days

                #COND7 - MCS is MOIST in SOME parts for more than 180 CUMULATIVE days
                temp <- with(ConditionalDF, tapply(!MCS_Dry_All, Years, function(x) sum(x)))#Number of days where any soils are moist
                ConditionalDF$MoistDaysCumAny <- rep(temp, days_per_wyear)
                ConditionalDF$COND7 <- ConditionalDF$MoistDaysCumAny > 180 #TRUE = Not Dry or Moist for as long 180 cumlative days

                #Cond8 - MCS is MOIST in SOME parts for more than 90 CONSECUTIVE days
                temp <- with(ConditionalDF, tapply(!MCS_Dry_All,Years, max.duration)) #Consecutive days of Moist soil
                ConditionalDF$MoistDaysConsecAny <- rep(temp, days_per_wyear)
                ConditionalDF$COND8 <- ConditionalDF$MoistDaysConsecAny > 90 # TRUE = Moist more than 90 Consecutive Days

                #COND9 - Moist in ALL parts MORE than 45 CONSECUTIVE days in the 4 months following the winter solstice
                temp <- with(ConditionalDF[ConditionalDF$DOY %in% c(355:365, 1:111), ], tapply(MCS_Moist_All, Years, max.duration))#Consecutive days of moist soil after winter solsitice
                ConditionalDF$MoistDaysConsecWinter <- rep(temp, days_per_wyear)
                ConditionalDF$COND9 <- ConditionalDF$MoistDaysConsecWinter > 45 # TRUE = moist more than 45 consecutive days

                ConditionalDF3 <- apply(aggregate(ConditionalDF[, c('COND1','COND1_1','COND2','COND3','COND4','COND5','COND6','COND7','COND8','COND9')],
                                                  by=list(Year=ConditionalDF$Years),
                                                  function(x) sum(x) > sum(!x)),
                  2, function(x) sum(x) > sum(!x))

                #---Soil moisture regime: based on Chambers et al. 2014: Appendix 3 and on Soil Survey Staff 2010: p.26-28/Soil Survey Staff 2014: p.28-31
                #we ignore 'Aquic'

                #Anhydrous condition: Soil Survey Staff 2010: p.16/Soil Survey Staff 2014: p.18
                #we ignore test for 'ice-cemented permafrost' and 'rupture-resistance class'
                if (LanhConditionalDF3['COND1'] && LanhConditionalDF3['COND2'] && LanhConditionalDF3['COND3'])
                  Sregime["Anhydrous"] <- 1L

                #Aridic soil moisture regime; The limits set for soil temperature exclude from these soil moisture regimes soils in the very cold and dry polar regions and in areas at high elevations. Such soils are considered to have anhydrous condition
                if (ConditionalDF3['COND1'] && ConditionalDF3['COND2'] && !ConditionalDF3['COND3'])
                  Sregime["Aridic"] <- 1L

                #Udic soil moisture regime - #we ignore test for 'three- phase system' during T50 > 5
                if (ConditionalDF3['COND3']) {
                  if (!ConditionalDF3['COND4'] && ConditionalDF3['COND5']) {
                    if (ConditionalDF3['COND6'])
                      Sregime["Udic"] <- 1L
                  } else {
                    Sregime["Udic"] <- 1L
                  }
                }

                #Ustic soil moisture regime
                if (!permafrost) {
                  if ((ConditionalDF3['COND4'] || !ConditionalDF3['COND5']) &&
                      !ConditionalDF3['COND3'] && (ConditionalDF3['COND7'] || ConditionalDF3['COND8'])) {
                      Sregime["Ustic"] <- 1L
                  }
                  if (!ConditionalDF3['COND4'] && ConditionalDF3['COND5'] &&
                      !ConditionalDF3['COND3'] && !ConditionalDF3['COND1']) {
                      if (ConditionalDF3['COND9']) {
                        if (ConditionalDF3['COND6'])
                          Sregime["Ustic"] <- 1L
                      } else {
                          Sregime["Ustic"] <- 1L
                      }
                  }
                }

                 #Xeric soil moisture regime
                if (!ConditionalDF3['COND6'] && ConditionalDF3['COND9'] &&
                    !ConditionalDF3['COND4'] && ConditionalDF3['COND5'] &&
                    (ConditionalDF3['COND1_1'] || !ConditionalDF3['COND2'])) {
                    Sregime["Xeric"] <- 1L
                }

                Lanh_annual_means <- .colMeans(as.matrix(
                    aggregate(LanhConditionalDF[, c('T50_at0C', 'Lanh_Dry_Half',
                                                    'HalfDryDaysCumAbove0C')],
                              by = list(LanhConditionalDF$Years), mean)[, -1]),
                  length(wyears_normal), 3)
                Cond_annual_means <- .colMeans(as.matrix(
                    aggregate(ConditionalDF[, c("T50_at5C", "T50_at8C", "MCS_Moist_All",
                                                "MCS_Dry_All", "COND1_Test", "COND1_1_Test",
                                                "COND2", "COND3", "COND4", "COND5",
                                                "DryDaysConsecSummer", "COND6", "COND7",
                                                "MoistDaysConsecAny", "COND8",
                                                "MoistDaysConsecWinter", "COND9")],
                              by = list(ConditionalDF$Years), mean)[, -1]),
                  length(wyears_normal), 17)

                regimes_done <- TRUE

                to_del <- c("n_Lanh", "width_Lanh", "Lanh_Dry_Half", "LanhConditionalDF",
                  "LanhConditionalDF3", "ConditionalDF", "ConditionalDF3")
                to_del <- to_del[to_del %in% ls()]
                if (length(to_del) > 0)
                  try(rm(list = to_del), silent = TRUE)

              } else {
                if (!be.quiet)
                  print(paste(i_label, "Number of normal years not long enough to calculate NRCS soil moisture regimes. Try increasing length of simulation"))

                Sregime[] <- NA

                to_del <- c("calc50", "calcLanh", "calcMCS", "clay_temp", "days_per_wyear",
                  "i_depth50", "i_Lanh", "i_MCS", "imp_depth", "impermeability",
                  "isummer", "Lanh_depth", "MAP", "MMP", "normal1", "normal2",
                  "sand_temp", "soildat", "soiltemp_nrsc", "SWP_dry", "swp_dy_nrsc",
                  "SWP_sat", "T50", "vwc_dy_nrsc", "wateryears", "wdays_index",
                  "wyears", "wyears_index", "wyears_normal")
                to_del <- to_del[to_del %in% ls()]
                if (length(to_del) > 0)
                  try(rm(list = to_del), silent = TRUE)
              }

            } else {
              if (!be.quiet)
                print(paste(i_label, "has unrealistic soil temperature values: NRCS soil moisture/temperature regimes not calculated."))
                Tregime[] <- Sregime[] <- NA
            }

          } else {
            if (!be.quiet)
              print(paste(i_label, "soil temperature module turned off but required for NRCS Soil Moisture/Temperature Regimes."))
              Tregime[] <- Sregime[] <- NA
          }

          nv_new <- nv + 11 + 3 + 17 + length(Tregime) + length(Sregime)
          resMeans[nv:(nv_new - 1)] <- c(Fifty_depth,
            MCS_depth[1:2], Lanh_depth[1:2], as.integer(permafrost),
            mean(MATLanh, na.rm = TRUE), mean(MAT50, na.rm = TRUE),
            mean(T50jja, na.rm = TRUE), mean(T50djf, na.rm = TRUE), CSPartSummer,
            Lanh_annual_means, Cond_annual_means, Tregime, Sregime)
          nv <- nv_new


          to_del <- c("MCS_depth", "Fifty_depth", "permafrost", "CSPartSummer", "MATLanh",
            "MAT50", "T50jja", "T50djf", "Lanh_annual_means", "Cond_annual_means")
          to_del <- to_del[to_del %in% ls()]
          if (length(to_del) > 0)
            try(rm(list = to_del), silent = TRUE)
        }

			#35b
				if(aon$dailyNRCS_Chambers2014_ResilienceResistance){
					#Based on Table 1 in Chambers, J. C., D. A. Pyke, J. D. Maestas, M. Pellant, C. S. Boyd, S. B. Campbell, S. Espinosa, D. W. Havlina, K. E. Mayer, and A. Wuenschel. 2014. Using Resistance and Resilience Concepts to Reduce Impacts of Invasive Annual Grasses and Altered Fire Regimes on the Sagebrush Ecosystem and Greater Sage-Grouse: A Strategic Multi-Scale Approach. Gen. Tech. Rep. RMRS-GTR-326. U.S. Department of Agriculture, Forest Service, Rocky Mountain Research Station, Fort Collins, CO.
					if(print.debug) print("Aggregation of dailyNRCS_Chambers2014_ResilienceResistance")
					if(!exists("prcp.yr")) prcp.yr <- get_PPT_yr(sc, runData, simTime)

					#Result containers
					cats <- c("Low", "ModeratelyLow", "Moderate", "ModeratelyHigh", "High")
					resilience <- resistance <- rep(0, times=length(cats))
					names(resilience) <- names(resistance) <- cats

					if (regimes_done && aon$dailyNRCS_SoilMoistureTemperatureRegimes) {
						#---Table 1 in Chambers et al. 2014
						rows_resilience <- c("ModeratelyHigh", "ModeratelyHigh", "Moderate", "Low", "Low")
						rows_resistance <- c("High", "Moderate", "ModeratelyLow", "Moderate", "Low")
						#Ecological type
						Table1_EcologicalType <- matrix(c("Cryic", "Xeric", "Frigid", "Xeric", "Mesic", "Xeric", "Frigid", "Aridic", "Mesic", "Aridic"), ncol=2, byrow=TRUE)
						Type <- as.logical(Tregime[Table1_EcologicalType[, 1]]) & as.logical(Sregime[Table1_EcologicalType[, 2]])

						#Characteristics
						MAP <- mean(prcp.yr$ppt)
						Table1_Characteristics_mm <- matrix(c(14, Inf, 12, 22, 12, 16, 6, 12, 8, 12), ncol=2, byrow=TRUE) * 2.54 * 10
						Characteristics <- MAP >= Table1_Characteristics_mm[, 1] & MAP <= Table1_Characteristics_mm[, 2]

						#Resilience and Resistance
						RR <- which(Type & Characteristics)
						for(ir in RR){
							resilience[rows_resilience[ir]] <- 1
							resistance[rows_resistance[ir]] <- 1
						}

						rm(rows_resilience, rows_resistance, Table1_EcologicalType, Type,
							MAP, Table1_Characteristics_mm, Characteristics, RR)
					} else {
						resilience <- resistance <- rep(NA, times=length(cats))
					}

					resMeans[nv:(nv+2*length(cats)-1)] <- c(resilience, resistance)
					nv <- nv + 2*length(cats)

					rm(cats, resilience, resistance)

				}

        #35c
        if(aon$dailyNRCS_Maestas2016_ResilienceResistance){	#Requires "dailyNRCS_SoilMoistureTemperatureRegimes"
          #Based on Maestas, J.D., Campbell, S.B., Chambers, J.C., Pellant, M. & Miller, R.F. (2016). Tapping Soil Survey Information for Rapid Assessment of Sagebrush Ecosystem Resilience and Resistance. Rangelands, 38, 120-128.
          if (print.debug)
            print("Aggregation of dailyNRCS_Maestas2016_ResilienceResistance")

          RR <- c(Low = 0, Moderate = 0, High = 0)

          if (regimes_done && aon$dailyNRCS_SoilMoistureTemperatureRegimes) {
            #---Table 1 in Maestas et al. 2016
            Table1 <- matrix(c(
                "Cryic", "Xeric", "High",
                "Frigid", "Xeric", "High",
                "Cryic", "Aridic", "Moderate",
                "Frigid", "Aridic", "Moderate",
                "Mesic", "Xeric", "Moderate",
                "Mesic", "Aridic", "Low"),
              ncol = 3, byrow = TRUE)

            temp <- Table1[as.logical(Tregime[Table1[, 1]]) & as.logical(Sregime[Table1[, 2]]), 3]
            RR[temp] <- 1

            rm(Table1)
          }

          nv_new <- nv + length(RR)
          resMeans[nv:(nv_new - 1)] <- RR
          nv <- nv_new

          rm(RR)
        }

        rm(regimes_done)
        if (aon$dailyNRCS_SoilMoistureTemperatureRegimes)
          rm(Tregime, Sregime)

			#35.2
				if(aon$dailyWetDegreeDays){	#Wet degree days on daily temp and swp
					if(print.debug) print("Aggregation of dailyWetDegreeDays")
					if(!exists("vwcmatric.dy")) vwcmatric.dy <- get_Response_aggL(sc, sw_vwcmatric, tscale = "dy", scaler = 1, FUN = weighted.mean, weights = layers_width, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)
					if(!exists("swpmatric.dy")) swpmatric.dy <- get_SWPmatric_aggL(vwcmatric.dy, texture, sand, clay)
					if(!exists("temp.dy")) temp.dy <- get_Temp_dy(sc, runData, simTime)

					degday <- ifelse(temp.dy$mean > DegreeDayBase, temp.dy$mean - DegreeDayBase, 0) #degree days

					for(icrit in seq_along(SWPcrit_MPa)){

						wet.top <- swpmatric.dy$top >= SWPcrit_MPa[icrit]

						if(length(bottomL) > 0 && !identical(bottomL, 0)) {
							wet.bottom <- swpmatric.dy$bottom >= SWPcrit_MPa[icrit]
						} else {
							wet.bottom <- matrix(data=NA, nrow=length(swpmatric.dy$bottom), ncol=1)
						}

						wetdegday.top <- ifelse(wet.top > 0, degday, 0)
						wetdegday.bottom <- ifelse(wet.bottom > 0, degday, 0)
						wetdegday.any <- ifelse(wet.top + wet.bottom > 0, degday, 0)

            temp <- lapply(list(wetdegday.top, wetdegday.bottom, wetdegday.any),
                            function(x) tapply(x, simTime2$year_ForEachUsedDay, sum))

						resMeans[(nv+3*(icrit-1)):(nv+3*(icrit-1)+2)] <- vapply(temp, mean, 1)
						resSDs[(nv+3*(icrit-1)):(nv+3*(icrit-1)+2)] <- vapply(temp, sd, 1)
					}
					nv <- nv+3*length(SWPcrit_MPa)

					rm(degday, wet.top, wet.bottom, wetdegday.top, wetdegday.bottom, wetdegday.any)
				}

			#35.3
				if(aon$dailyThermalDrynessStartEnd){
					if (print.debug) print("Aggregation of dailyThermalDrynessStartEnd")
					if (!exists("temp.dy")) temp.dy <- get_Temp_dy(sc, runData, simTime)
					if(!exists("vwcmatric.dy")) vwcmatric.dy <- get_Response_aggL(sc, sw_vwcmatric, tscale = "dy", scaler = 1, FUN = weighted.mean, weights = layers_width, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)
					if(!exists("swpmatric.dy")) swpmatric.dy <- get_SWPmatric_aggL(vwcmatric.dy, texture, sand, clay)
					adjDays <- simTime2$doy_ForEachUsedDay_NSadj[1] - simTime2$doy_ForEachUsedDay[1]

					thermal <- temp.dy$mean > 0

					for (icrit in seq_along(SWPcrit_MPa)) {
						thermaldry.top <- thermal & swpmatric.dy$top < SWPcrit_MPa[icrit]
						thermaldry.bottom <- if (length(bottomL) > 0 && !identical(bottomL, 0)) {
								thermal & swpmatric.dy$bottom < SWPcrit_MPa[icrit]
							} else {
								rep(FALSE, length(thermaldry.top))
							}

						temp <- aggregate(cbind(thermaldry.top, thermaldry.bottom),
									by = list(simTime2$year_ForEachUsedDay_NSadj),
									FUN = function(x) max.duration(x, return_doys = TRUE))

						resMeans[nv:(nv+3)] <- c(
							apply(temp$thermaldry.top[, 2:3, drop = FALSE], 2, circ.mean, int = 365),
							apply(temp$thermaldry.bottom[, 2:3, drop = FALSE], 2, circ.mean, int = 365)) - adjDays
						resSDs[nv:(nv+3)] <- c(
							apply(temp$thermaldry.top[, 2:3, drop = FALSE], 2, circ.sd, int = 365),
							apply(temp$thermaldry.bottom[, 2:3, drop = FALSE], 2, circ.sd, int = 365))
						nv <- nv+4
					}

					rm(thermal, adjDays, thermaldry.top)
					if (length(bottomL) > 0 && !identical(bottomL, 0))
						rm(thermaldry.bottom)
				}

			#35.4
				if(aon$dailyThermalSWPConditionCount){
				  if(print.debug) print("Aggregation of dailyThermalSWPConditionCount")
				  if(!exists("vwcmatric.dy.all")) vwcmatric.dy.all <- get_Response_aggL(sc, sw_vwcmatric, tscale = "dyAll", scaler = 1, FUN = weighted.mean, weights = layers_width, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)
				  if(!exists("swpmatric.dy.all")) swpmatric.dy.all <- get_SWPmatric_aggL(vwcmatric.dy.all, texture, sand, clay)
				  if(!exists("vwcmatric.dy")) vwcmatric.dy <- get_Response_aggL(sc, sw_vwcmatric, tscale = "dy", scaler = 1, FUN = weighted.mean, weights = layers_width, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)
				  if(!exists("swpmatric.dy")) swpmatric.dy <- get_SWPmatric_aggL(vwcmatric.dy, texture, sand, clay)
				  if(!exists("temp.dy")) temp.dy <- get_Temp_dy(sc, runData, simTime)

				  thermal <- temp.dy$mean >
						matrix(rep.int(Tmean_crit_C, length(temp.dy$mean)),
							ncol = length(Tmean_crit_C), byrow = TRUE)

				  dryness <- matrix(rep.int(SWPcrit_MPa, length(temp.dy$mean)),
				      ncol = length(SWPcrit_MPa), byrow = TRUE)
				  n_conds <- 6L
				  conds <- list() # max length(conds) == n_conds
				  conds[["DryAll"]] <- apply(swpmatric.dy.all$val[simTime$index.usedy, -(1:2), drop = FALSE], 1, max) < dryness
				  conds[["WetAll"]] <- apply(swpmatric.dy.all$val[simTime$index.usedy, -(1:2), drop = FALSE], 1, min) >= dryness
				  conds[["DryTop"]] <- swpmatric.dy$top < dryness
				  conds[["WetTop"]] <- !conds[["DryTop"]]
				  if (length(bottomL) > 0 && !identical(bottomL, 0)) {
				      conds[["DryBottom"]] <- swpmatric.dy$bottom < dryness
				      conds[["WetBottom"]] <- !conds[["DryBottom"]]
				  }

				  day_count <- array(NA,
				      dim = c(simTime$no.useyr, length(Tmean_crit_C), length(SWPcrit_MPa), n_conds))
				  for (d2 in seq_along(Tmean_crit_C))
				      for (d4 in seq_along(conds))
				          for (d3 in seq_along(SWPcrit_MPa))
				              day_count[, d2, d3, d4] <- tapply(thermal[, d2] & conds[[d4]][, d3],
				                  INDEX = simTime2$year_ForEachUsedDay,
				                  FUN = sum)
				  nv_new <- nv + length(Tmean_crit_C) * length(SWPcrit_MPa) * n_conds
				  resMeans[nv:(nv_new - 1)] <- as.vector(colMeans(day_count))
				  resSDs[nv:(nv_new - 1)] <- as.vector(apply(day_count, 2:4, sd))
				  nv <- nv_new

				  rm(thermal, dryness, conds, day_count)
				}

			#36
				if(aon$monthlySWPdryness){#dry periods based on monthly swp data: accountNSHemispheres_agg
					if(print.debug) print("Aggregation of monthlySWPdryness")
					if(!exists("vwcmatric.mo")) vwcmatric.mo <- get_Response_aggL(sc, sw_vwcmatric, tscale = "mo", scaler = 1, FUN = weighted.mean, weights = layers_width, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)
					if(!exists("swpmatric.mo")) swpmatric.mo <- get_SWPmatric_aggL(vwcmatric.mo, texture, sand, clay)

					adjMonths <- ifelse(simTime2$month_ForEachUsedMonth[1] == simTime2$month_ForEachUsedMonth_NSadj[1], 0, 6)

					drymonths.top <- drymonths.bottom <- array(data=0, dim=c(length(SWPcrit_MPa), simTime$no.useyr, 12))
					for(icrit in seq(along=SWPcrit_MPa)){
            temp <- tapply(swpmatric.mo$top, simTime2$month_ForEachUsedMonth_NSadj, function(x) x <= SWPcrit_MPa[icrit])
						drymonths.top[icrit, , ] <- matrix(unlist(temp), nrow = simTime$no.useyr)
            temp <- tapply(swpmatric.mo$bottom, simTime2$month_ForEachUsedMonth_NSadj, function(x) x <= SWPcrit_MPa[icrit])
						drymonths.bottom[icrit, , ] <- matrix(unlist(temp), nrow = simTime$no.useyr)
					}

					years.top <- apply(drymonths.top, MARGIN=1:2, FUN=sum)
					years.bottom <- apply(drymonths.bottom, MARGIN=1:2, FUN=sum)

					resMeans[nv:(nv+2*length(SWPcrit_MPa)-1)] <- c(apply(years.top, MARGIN=1, FUN=mean), apply(years.bottom, MARGIN=1, FUN=mean))
					resSDs[nv:(nv+2*length(SWPcrit_MPa)-1)] <- c(apply(years.top, MARGIN=1, FUN=sd), apply(years.bottom, MARGIN=1, FUN=sd))

					nv <- nv+2*length(SWPcrit_MPa)

					start.top <- apply(drymonths.top, MARGIN=1:2, FUN=match, x=1, nomatch=0)
					start.top[start.top != 0] <- ifelse((temp <- (start.top[start.top != 0] + adjMonths) %% 12) == 0, 12, temp)
					start.bottom <- apply(drymonths.bottom, MARGIN=1:2, FUN=match, x=1, nomatch=0)
					start.bottom[start.bottom != 0] <- ifelse((temp <- (start.bottom[start.bottom != 0] + adjMonths) %% 12) == 0, 12, temp)

					resMeans[nv:(nv+2*length(SWPcrit_MPa)-1)] <- c(apply(start.top, MARGIN=1, circ.mean, int = 12),
					                                               apply(start.bottom, MARGIN=1, circ.mean, int = 12))
					resSDs[nv:(nv+2*length(SWPcrit_MPa)-1)] <- c(apply(start.top, MARGIN=1, circ.sd, int = 12),
					                                             apply(start.bottom, MARGIN=1, circ.sd, int = 12))

					nv <- nv+2*length(SWPcrit_MPa)

					rm(drymonths.top, drymonths.bottom, years.top, start.top, years.bottom, start.bottom, adjMonths)
				}
			#37
				if(aon$dailySWPdrynessANDwetness){#Dry and wet periods based on daily swp: accountNSHemispheres_agg
					if(print.debug) print("Aggregation of dailySWPdrynessANDwetness")
					if(!exists("vwcmatric.dy.all")) vwcmatric.dy.all <- get_Response_aggL(sc, sw_vwcmatric, tscale = "dyAll", scaler = 1, FUN = weighted.mean, weights = layers_width, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)
					if(!exists("swpmatric.dy.all")) swpmatric.dy.all <- get_SWPmatric_aggL(vwcmatric.dy.all, texture, sand, clay) #swp.dy.all is required to get all layers

					adjDays <- simTime2$doy_ForEachUsedDay_NSadj[1] - simTime2$doy_ForEachUsedDay[1]
					durationDryPeriods.min <- 10 # days

					for(icrit in seq_along(SWPcrit_MPa)){

						wet_crit <- swpmatric.dy.all$val >= SWPcrit_MPa[icrit]
						wet <- list()
						wet$top <- apply(wet_crit[simTime$index.usedy, 2+topL, drop = FALSE], 1, sum)
            if(length(bottomL) > 0 && !identical(bottomL, 0)) {
							wet$bottom <- apply(wet_crit[simTime$index.usedy,2+bottomL, drop = FALSE], 1, sum)
						} else {
						  wet$bottom <- rep(NA, simTime$no.usedy)
						}

						AtLeastOneWet <- lapply(wet, function(x) x > 0)
						AllDry <- lapply(AtLeastOneWet, `!`)
						AllWet <- list(top = wet$top == length(topL),
						               bottom = wet$bottom == length(bottomL))
						AtLeastOneDry <- lapply(AllWet, `!`)

						#wet periods
						res.wet <- matrix(0, nrow = simTime$no.useyr, ncol = 8)
						res.wet[, 1] <- tapply(AtLeastOneWet$top, simTime2$year_ForEachUsedDay_NSadj, sum) # total number of days per year when at least one top layer is wet
						res.wet[, 2] <- tapply(AtLeastOneWet$bottom, simTime2$year_ForEachUsedDay_NSadj, sum) # total number of days per year when at least one top layer is wet
						res.wet[, 3] <- tapply(AtLeastOneWet$top, simTime2$year_ForEachUsedDay_NSadj, max.duration) # maximum number of continous days when at least one top layers is wet
						res.wet[, 4] <- tapply(AtLeastOneWet$bottom, simTime2$year_ForEachUsedDay_NSadj, max.duration) # maximum number of continous days when at least one top layers is wet
						res.wet[, 5] <- tapply(AllWet$top, simTime2$year_ForEachUsedDay_NSadj, sum) # total number of days per year when all top layer are wet
						res.wet[, 6] <- tapply(AllWet$bottom, simTime2$year_ForEachUsedDay_NSadj, sum) # total number of days per year when all top layer are wet
						res.wet[, 7] <- tapply(AllWet$top, simTime2$year_ForEachUsedDay_NSadj, max.duration) # maximum number of continous days when all top layers are wet
						res.wet[, 8] <- tapply(AllWet$bottom, simTime2$year_ForEachUsedDay_NSadj, max.duration) # maximum number of continous days when all top layers are wet

						#dry periods
						res.dry <- matrix(0, nrow = simTime$no.useyr, ncol = 8)
						res.dry[,3] <- tapply(AllDry$top, simTime2$year_ForEachUsedDay_NSadj, sum) #total number of days/year when all top layers are dry
            res.dry[,7] <- tapply(AllDry$bottom, simTime2$year_ForEachUsedDay_NSadj, sum) #total number of days/year when all bottom layers are dry
						res.dry[,4] <- tapply(AllDry$top, simTime2$year_ForEachUsedDay_NSadj, max.duration) #maximum number of continous days when all top layers are dry
            res.dry[,8] <- tapply(AllDry$bottom, simTime2$year_ForEachUsedDay_NSadj, max.duration) #maximum number of continous days when all bottom layers are dry
						res.dry[,1] <- tapply(AtLeastOneDry$top, simTime2$year_ForEachUsedDay_NSadj, startDoyOfDuration, duration = durationDryPeriods.min)	# start days/year when at least one of top layers are dry for at least ten days
            res.dry[,5] <- tapply(AtLeastOneDry$bottom, simTime2$year_ForEachUsedDay_NSadj, startDoyOfDuration, duration = durationDryPeriods.min)	# start days/year when at least one of bottom layers are dry for at least ten days
						res.dry[,2] <- tapply(AtLeastOneDry$top, simTime2$year_ForEachUsedDay_NSadj, endDoyAfterDuration, duration = durationDryPeriods.min)	# end days/year when at least one of top layers have been dry for at least ten days
            res.dry[,6] <- tapply(AtLeastOneDry$bottom, simTime2$year_ForEachUsedDay_NSadj, endDoyAfterDuration, duration = durationDryPeriods.min) # end days/year when at least one of bottom layers have been dry for at least ten days
						res.dry[, c(1:2, 5:5)] <- res.dry[, c(1:2, 5:5)] - adjDays
						res.dry[res.dry[, 1] > res.dry[, 2], 3] <- 0 #correct [,c(3,7)] for years when start<end otherwise set 0
            res.dry[res.dry[, 5] > res.dry[, 6], 7] <- 0 #correct [,c(3,7)] for years when start<end otherwise set 0

						#aggregate results
						temp <- data.frame(res.wet, res.dry[, -c(1:2, 5:6)])
						resMeans[(nv+16*(icrit-1)):(nv+16*icrit-1)] <- c(colMeans(temp, na.rm = TRUE),
								apply(res.dry[, c(1:2, 5:6), drop=FALSE], 2, circ.mean, int = 365, na.rm = TRUE))
						resSDs[(nv+16*(icrit-1)):(nv+16*icrit-1)] <- c(apply(temp, 2, sd, na.rm = TRUE),
								apply(res.dry[, c(1:2, 5:6), drop=FALSE], 2, circ.sd, int = 365, na.rm = TRUE))
					}
					nv <- nv+16*length(SWPcrit_MPa)

					rm(res.dry, wet, wet_crit, AtLeastOneWet, AllWet, AllDry)
				}
			#38
				if(aon$dailySuitablePeriodsDuration){
					if(print.debug) print("Aggregation of dailySuitablePeriodsDuration")
					if(!exists("vwcmatric.dy")) vwcmatric.dy <- get_Response_aggL(sc, sw_vwcmatric, tscale = "dy", scaler = 1, FUN = weighted.mean, weights = layers_width, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)
					if(!exists("swpmatric.dy")) swpmatric.dy <- get_SWPmatric_aggL(vwcmatric.dy, texture, sand, clay)
					if(!exists("temp.dy")) temp.dy <- get_Temp_dy(sc, runData, simTime)
					if(!exists("SWE.dy")) SWE.dy <- get_SWE_dy(sc, runData, simTime)

					quantiles <- c(0.05, 0.5, 0.95)
					snowfree <- SWE.dy$val == 0
					niceTemp <- temp.dy$mean >= DegreeDayBase

					for(icrit in seq(along=SWPcrit_MPa)){
						wet.top <- swpmatric.dy$top >= SWPcrit_MPa[icrit]

						if(length(bottomL) > 0 && !identical(bottomL, 0)){
							wet.bottom <- swpmatric.dy$bottom >= SWPcrit_MPa[icrit]
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
			#39
				if(aon$dailySuitablePeriodsAvailableWater){
					if(print.debug) print("Aggregation of dailySuitablePeriodsAvailableWater")
					if(!exists("swcbulk.dy")) swcbulk.dy <- get_Response_aggL(sc, sw_swcbulk, tscale = "dy", scaler = 10, FUN = sum, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)
					if(!exists("temp.dy")) temp.dy <- get_Temp_dy(sc, runData, simTime)
					if(!exists("SWE.dy")) SWE.dy <- get_SWE_dy(sc, runData, simTime)

					suitable <- (SWE.dy$val == 0) & (temp.dy$mean >= DegreeDayBase)

					for(icrit in seq(along=SWPcrit_MPa)){
						SWCcritT <- SWPtoVWC(SWPcrit_MPa[icrit], texture$sand.top, texture$clay.top) * 10 * sum(layers_width[topL])
						swa.top <- ifelse(suitable, cut0Inf(swcbulk.dy$top - SWCcritT, val = 0), 0)

						if(length(bottomL) > 0 && !identical(bottomL, 0)){
							SWCcritB <- SWPtoVWC(SWPcrit_MPa[icrit], texture$sand.bottom, texture$clay.bottom) * 10 * sum(layers_width[bottomL])
							swa.bottom <- ifelse(suitable, cut0Inf(swcbulk.dy$bottom - SWCcritB, val = 0), 0)
						} else {
							swa.bottom <- rep(0, length(swa.top))
						}

						temp <- list(t = tapply(swa.top, simTime2$year_ForEachUsedDay_NSadj, sum),
						                        b = tapply(swa.bottom, simTime2$year_ForEachUsedDay_NSadj, sum))
						resMeans[nv:(nv+1)] <- sapply(temp, mean)
						resSDs[nv:(nv+1)] <- sapply(temp, sd)
						nv <- nv+2
					}

					rm(swa.top, swa.bottom, suitable)
				}
			#40
				if(aon$dailySuitablePeriodsDrySpells){
					if(print.debug) print("Aggregation of dailySuitablePeriodsDrySpells")
					if(!exists("vwcmatric.dy.all")) vwcmatric.dy.all <- get_Response_aggL(sc, sw_vwcmatric, tscale = "dyAll", scaler = 1, FUN = weighted.mean, weights = layers_width, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)
					if(!exists("swpmatric.dy.all")) swpmatric.dy.all <- get_SWPmatric_aggL(vwcmatric.dy.all, texture, sand, clay) #swp.dy.all is required to get all layers
					if(!exists("temp.dy")) temp.dy <- get_Temp_dy(sc, runData, simTime)
					if(!exists("SWE.dy")) SWE.dy <- get_SWE_dy(sc, runData, simTime)

					suitable <- (SWE.dy$val == 0) & (temp.dy$mean >= DegreeDayBase)

					adjDays <- simTime2$doy_ForEachUsedDay_NSadj[1] - simTime2$doy_ForEachUsedDay[1]
					durationDryPeriods.min <- 10 # days

					for(icrit in seq(along=SWPcrit_MPa)){
						dry_crit <- swpmatric.dy.all$val < SWPcrit_MPa[icrit]
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
						resMeans[nv:(nv+7)] <- c(apply(temp$dry.top[, 1:3, drop=FALSE], 2, mean), circ.mean(x=temp$dry.top[, 4], int=365), apply(temp$dry.bottom[, 1:3, drop=FALSE], 2, mean), circ.mean(x=temp$dry.bottom[, 4], int=365))
						resSDs[nv:(nv+7)] <- c(apply(temp$dry.top[, 1:3, drop=FALSE], 2, sd), circ.sd(x=temp$dry.top[, 4], int=365), apply(temp$dry.bottom[, 1:3, drop=FALSE], 2, sd), circ.sd(x=temp$dry.bottom[, 4], int=365))
						nv <- nv+8
					}

					rm(dry.top, dry.bottom, suitable, dry_crit, adjDays, durationDryPeriods.min)
				}
			#41
				if(aon$dailySWPdrynessDurationDistribution){#cummulative frequency distribution of durations of dry soils in each of the four seasons and for each of the SWP.crit
					if(print.debug) print("Aggregation of dailySWPdrynessDurationDistribution")
					if(!exists("vwcmatric.dy")) vwcmatric.dy <- get_Response_aggL(sc, sw_vwcmatric, tscale = "dy", scaler = 1, FUN = weighted.mean, weights = layers_width, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)
					if(!exists("swpmatric.dy")) swpmatric.dy <- get_SWPmatric_aggL(vwcmatric.dy, texture, sand, clay)

					deciles <- (0:10)*10/100
					quantiles <- (0:4)/4
					mo_seasons <- matrix(data=c(12,1:11), ncol=3, nrow=4, byrow=TRUE)
					season.flag <- c("DJF", "MAM", "JJA", "SON")
					seasonal.years <- c(simTime2$year_ForEachUsedDay[-(1:31)], rep(-9999, times=31))	#shift beginning of year to Dec 1

					for(icrit in seq(along=SWPcrit_MPa)){

						wet.top <- swpmatric.dy$top >= SWPcrit_MPa[icrit]

						if(length(bottomL) > 0 && !identical(bottomL, 0)) wet.bottom <- swpmatric.dy$bottom >= SWPcrit_MPa[icrit]

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
			#42
				if(aon$dailySWPdrynessEventSizeDistribution){
					if(print.debug) print("Aggregation of dailySWPdrynessEventSizeDistribution")
					if(!exists("vwcmatric.dy")) vwcmatric.dy <- get_Response_aggL(sc, sw_vwcmatric, tscale = "dy", scaler = 1, FUN = weighted.mean, weights = layers_width, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)
					if(!exists("swpmatric.dy")) swpmatric.dy <- get_SWPmatric_aggL(vwcmatric.dy, texture, sand, clay)
					binSize <- c(1, 8, 15, 29, 57, 183, 367) #closed interval lengths in [days] within a year; NOTE: n_variables is set for binsN == 6
					binsN <- length(binSize) - 1

					for(icrit in seq_along(SWPcrit_MPa)){

						dry.top <- swpmatric.dy$top[simTime$index.usedy] < SWPcrit_MPa[icrit]

						if(length(bottomL) > 0 && !identical(bottomL, 0)) {
							dry.bottom <- swpmatric.dy$bottom[simTime$index.usedy] < SWPcrit_MPa[icrit]
						}

						#apply over each year, rle just on selected year store runs in vec, if that is greater than 0 then add to that years bins else return 0s for that year. Will result in a matrix of 4 by Years
						binsYears.top <- aggregate(dry.top, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=EventDistribution, N = binsN, size = binSize)$x
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
							binsYears.bottom <- aggregate(dry.bottom, by=list(simTime2$year_ForEachUsedDay_NSadj), FUN=EventDistribution, N = binsN, size = binSize)$x
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
			#43
				if(aon$dailySWPdrynessIntensity) {
					if(print.debug) print("Aggregation of dailySWPdrynessIntensity")
					if(!exists("vwcmatric.dy")) vwcmatric.dy <- get_Response_aggL(sc, sw_vwcmatric, tscale = "dy", scaler = 1, FUN = weighted.mean, weights = layers_width, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)

					SWCtop <- vwcmatric.dy$top * sum(layers_width[topL])*10
					if(length(bottomL) > 0 && !identical(bottomL, 0)) SWCbottom <- vwcmatric.dy$bottom * sum(layers_width[bottomL])*10

					for(icrit in seq(along=SWPcrit_MPa)){
						#amount of SWC required so that layer wouldn't be dry
						SWCcritT <- SWPtoVWC(SWPcrit_MPa[icrit], texture$sand.top, texture$clay.top) * sum(layers_width[topL])*10
						missingSWCtop <- cut0Inf(SWCcritT - SWCtop, val = 0)
						IntensitySum_top <- c(mean(temp <- sapply(simTime$useyrs, FUN=function(y) sum(missingSWCtop[simTime2$year_ForEachUsedDay == y])), na.rm=TRUE), sd(temp, na.rm=TRUE))
						IntensityMean_top <- c(mean(temp <- sapply(simTime$useyrs, FUN=function(y) mean((temp <- missingSWCtop[simTime2$year_ForEachUsedDay == y])[temp > 0], na.rm=TRUE)), na.rm=TRUE), sd(temp, na.rm=TRUE))
						IntensityDurationAndNumber_top <- c(apply(temp <- sapply(simTime$useyrs, FUN=function(y) c(mean(temp <- (temp <- rle(missingSWCtop[simTime2$year_ForEachUsedDay == y] > 0))$lengths[temp$values]), length(temp))), 1, mean), apply(temp, 1, sd))[c(1, 3, 2, 4)]

						if(length(bottomL) > 0 && !identical(bottomL, 0)) {
							SWCcritB <- SWPtoVWC(SWPcrit_MPa[icrit], texture$sand.bottom, texture$clay.bottom) * sum(layers_width[bottomL])*10
							missingSWCbottom <- cut0Inf(SWCcritB - SWCbottom, val = 0)
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

			#43.2
				if(aon$dailyThermalDrynessStress){
				  if(print.debug) print("Aggregation of dailyThermalDrynessStress")
				  if(!exists("vwcmatric.dy.all")) vwcmatric.dy.all <- get_Response_aggL(sc, sw_vwcmatric, tscale = "dyAll", scaler = 1, FUN = weighted.mean, weights = layers_width, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)
				  if(!exists("swpmatric.dy.all")) swpmatric.dy.all <- get_SWPmatric_aggL(vwcmatric.dy.all, texture, sand, clay) #swp.dy.all is required to get all layers
				  if(!exists("vwcmatric.dy")) vwcmatric.dy <- get_Response_aggL(sc, sw_vwcmatric, tscale = "dy", scaler = 1, FUN = weighted.mean, weights = layers_width, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)
				  if(!exists("swpmatric.dy")) swpmatric.dy <- get_SWPmatric_aggL(vwcmatric.dy, texture, sand, clay)
				  if(!exists("temp.dy")) temp.dy <- get_Temp_dy(sc, runData, simTime)
				  if(!exists("vpd.dy")) vpd.dy <- get_VPD_dy(sc, temp.dy, xin = swRunScenariosData, st2 = simTime2)

				  # Moisture stress during hot and dry periods
				  nv_add <- length(SWPcrit_MPa)
				  dryness <- matrix(rep.int(SWPcrit_MPa, length(vpd.dy$mean)),
				      ncol = nv_add, byrow = TRUE)
				  n_conds <- 4L
				  conds <- list() # max length(conds) == n_conds
				  conds[["Always"]] <- matrix(TRUE, nrow = length(vpd.dy$mean), ncol = 1)
				  conds[["DryAll"]] <- apply(swpmatric.dy.all$val[simTime$index.usedy, -(1:2), drop = FALSE], 1, max) < dryness
				  conds[["DryTop"]] <- swpmatric.dy$top < dryness
				  conds[["DryBottom"]] <- if (length(bottomL) > 0 && !identical(bottomL, 0)) {
						  swpmatric.dy$bottom < dryness
					  } else{
						  matrix(FALSE, nrow = length(vpd.dy$mean), ncol = nv_add)
					  }

				  for (d3 in seq_len(n_conds)) {
					temp <- ifelse(conds[[d3]], vpd.dy$mean, NA)
				    nv_add <- ncol(temp)
					stress <- array(NA, dim = c(simTime$no.useyr, nv_add))
					for (d2 in seq_len(nv_add)) {
						stress[, d2] <- tapply(temp[, d2],
							INDEX = simTime2$year_ForEachUsedDay,
							FUN = fun_kLargest, fun = mean, k = 10L, na.rm = TRUE)
					}
					nv_new <- nv + nv_add
					resMeans[nv:(nv_new - 1)] <- apply(stress, 2, mean)
					resMeans[nv_new:(nv + 2 * nv_add - 1)] <- apply(stress, 2, max)
					resSDs[nv:(nv_new - 1)] <- apply(stress, 2, sd)
				  	nv <- nv + 2 * nv_add
				  }

				  rm(dryness, conds, stress)
				}


				#---Aggregation: Mean monthly values
			#44
				if(aon$monthlyTemp){
					if(print.debug) print("Aggregation of monthlyTemp")
					if(!exists("temp.mo")) temp.mo <- get_Temp_mo(sc, runData, simTime)

					resMeans[nv+st_mo-1] <- tapply(temp.mo$mean, simTime2$month_ForEachUsedMonth, mean)
					resSDs[nv+st_mo-1] <- tapply(temp.mo$mean, simTime2$month_ForEachUsedMonth, sd)
					nv <- nv+12
				}
			#45
				if(aon$monthlyPPT){
					if(print.debug) print("Aggregation of monthlyPPT")
					if(!exists("prcp.mo")) prcp.mo <- get_PPT_mo(sc, runData, simTime)

					resMeans[nv+st_mo-1] <- tapply(prcp.mo$ppt, simTime2$month_ForEachUsedMonth, mean)
					resSDs[nv+st_mo-1] <- tapply(prcp.mo$ppt, simTime2$month_ForEachUsedMonth, sd)
					nv <- nv+12
				}
			#46
				if(aon$monthlySnowpack){
					if(print.debug) print("Aggregation of monthlySnowpack")
					if(!exists("SWE.mo")) SWE.mo <- get_SWE_mo(sc, runData, simTime)

					resMeans[nv+st_mo-1] <- tapply(SWE.mo$val, simTime2$month_ForEachUsedMonth, mean)
					resSDs[nv+st_mo-1] <- tapply(SWE.mo$val, simTime2$month_ForEachUsedMonth, sd)
					nv <- nv+12
				}
			#47
				if(aon$monthlySoilTemp) {
					if(print.debug) print("Aggregation of monthlySoilTemp")
					if(!exists("soiltemp.mo")) soiltemp.mo <- get_Response_aggL(sc, sw_soiltemp, tscale = "mo", scaler = 1, FUN = weighted.mean, weights = layers_width, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)

					resMeans[nv+st_mo-1] <- tapply(soiltemp.mo$top, simTime2$month_ForEachUsedMonth, mean)
					resSDs[nv+st_mo-1] <- tapply(soiltemp.mo$top, simTime2$month_ForEachUsedMonth, sd)
					resMeans[nv+st_mo-1+12] <- tapply(soiltemp.mo$bottom, simTime2$month_ForEachUsedMonth, mean)
					resSDs[nv+st_mo-1+12] <- tapply(soiltemp.mo$bottom, simTime2$month_ForEachUsedMonth, sd)
					nv <- nv+24
				}
			#48
				if(aon$monthlyRunoff){
					if(print.debug) print("Aggregation of monthlyRunoff")
					if(!exists("runoff.mo")) runoff.mo <- get_Runoff_mo(sc, runData, simTime)

					resMeans[nv+st_mo-1] <- tapply(runoff.mo$val, simTime2$month_ForEachUsedMonth, mean)
					resSDs[nv+st_mo-1] <- tapply(runoff.mo$val, simTime2$month_ForEachUsedMonth, sd)
					nv <- nv+12
				}
			#49
				if(aon$monthlyHydraulicRedistribution){
					if(print.debug) print("Aggregation of monthlyHydraulicRedistribution")
					if(!exists("hydred.mo")) hydred.mo <- get_Response_aggL(sc, sw_hd, tscale = "mo", scaler = 10, FUN = sum, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)

					resMeans[nv+st_mo-1] <- tapply(hydred.mo$top, simTime2$month_ForEachUsedMonth, mean)
					resSDs[nv+st_mo-1] <- tapply(hydred.mo$top, simTime2$month_ForEachUsedMonth, sd)
					resMeans[nv+st_mo-1+12] <- tapply(hydred.mo$bottom, simTime2$month_ForEachUsedMonth, mean)
					resSDs[nv+st_mo-1+12] <- tapply(hydred.mo$bottom, simTime2$month_ForEachUsedMonth, sd)
					nv <- nv+24
				}
			#50
				if(aon$monthlyInfiltration){
					if(print.debug) print("Aggregation of monthlyInfiltration")
					if(!exists("inf.mo")) inf.mo <- get_Inf_mo(sc, runData, simTime)

					resMeans[nv+st_mo-1] <- tapply(inf.mo$inf, simTime2$month_ForEachUsedMonth, mean)
					resSDs[nv+st_mo-1] <- tapply(inf.mo$inf, simTime2$month_ForEachUsedMonth, sd)
					nv <- nv+12
				}
			#51
				if(aon$monthlyDeepDrainage){
					if(print.debug) print("Aggregation of monthlyDeepDrainage")
					if(!exists("deepDrain.mo")) deepDrain.mo <- get_DeepDrain_mo(sc, runData, simTime)

					resMeans[nv+st_mo-1] <- tapply(deepDrain.mo$val, simTime2$month_ForEachUsedMonth, mean)
					resSDs[nv+st_mo-1] <- tapply(deepDrain.mo$val, simTime2$month_ForEachUsedMonth, sd)
					nv <- nv+12
				}
			#52
				if(aon$monthlySWPmatric){
					if(print.debug) print("Aggregation of monthlySWPmatric")
					if(!exists("vwcmatric.mo")) vwcmatric.mo <- get_Response_aggL(sc, sw_vwcmatric, tscale = "mo", scaler = 1, FUN = weighted.mean, weights = layers_width, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)
					if(!exists("swpmatric.mo")) swpmatric.mo <- get_SWPmatric_aggL(vwcmatric.mo, texture, sand, clay)

					resMeans[nv+st_mo-1] <- swpmatric.mo$aggMean.top
					resMeans[nv+st_mo-1+12] <- swpmatric.mo$aggMean.bottom
					nv <- nv+24
				}
			#53 a.)
				if(aon$monthlyVWCbulk){
					if(print.debug) print("Aggregation of monthlyVWC")
					if(!exists("vwcbulk.mo")) vwcbulk.mo <- get_Response_aggL(sc, sw_vwcbulk, tscale = "mo", scaler = 1, FUN = weighted.mean, weights = layers_width, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)

					resMeans[nv+st_mo-1] <- tapply(vwcbulk.mo$top, simTime2$month_ForEachUsedMonth, mean)
					resSDs[nv+st_mo-1] <- tapply(vwcbulk.mo$top, simTime2$month_ForEachUsedMonth, sd)
					resMeans[nv+st_mo-1+12] <- tapply(vwcbulk.mo$bottom, simTime2$month_ForEachUsedMonth, mean)
					resSDs[nv+st_mo-1+12] <- tapply(vwcbulk.mo$bottom, simTime2$month_ForEachUsedMonth, sd)
					nv <- nv+24
				}
			#53 b.)
				if(aon$monthlyVWCmatric){
					if(print.debug) print("Aggregation of monthlyVWCmatric")
					if(!exists("vwcmatric.mo")) vwcmatric.mo <- get_Response_aggL(sc, sw_vwcmatric, tscale = "mo", scaler = 1, FUN = weighted.mean, weights = layers_width, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)

					resMeans[nv+st_mo-1] <- tapply(vwcmatric.mo$top, simTime2$month_ForEachUsedMonth, mean)
					resSDs[nv+st_mo-1] <- tapply(vwcmatric.mo$top, simTime2$month_ForEachUsedMonth, sd)
					resMeans[nv+st_mo-1+12] <- tapply(vwcmatric.mo$bottom, simTime2$month_ForEachUsedMonth, mean)
					resSDs[nv+st_mo-1+12] <- tapply(vwcmatric.mo$bottom, simTime2$month_ForEachUsedMonth, sd)
					nv <- nv+24
				}
			#54
				if(aon$monthlySWCbulk){
					if(print.debug) print("Aggregation of monthlySWCbulk")
					if(!exists("swcbulk.mo")) swcbulk.mo <- get_Response_aggL(sc, sw_swcbulk, tscale = "mo", scaler = 10, FUN = sum, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)

					resMeans[nv+st_mo-1] <- tapply(swcbulk.mo$top, simTime2$month_ForEachUsedMonth, mean)
					resSDs[nv+st_mo-1] <- tapply(swcbulk.mo$top, simTime2$month_ForEachUsedMonth, sd)
					resMeans[nv+st_mo-1+12] <- tapply(swcbulk.mo$bottom, simTime2$month_ForEachUsedMonth, mean)
					resSDs[nv+st_mo-1+12] <- tapply(swcbulk.mo$bottom, simTime2$month_ForEachUsedMonth, sd)
					nv <- nv+24
				}
			#55
				if(aon$monthlySWAbulk){
					if(print.debug) print("Aggregation of monthlySWA")
					if(!exists("vwcmatric.mo")) vwcmatric.mo <- get_Response_aggL(sc, sw_vwcmatric, tscale = "mo", scaler = 1, FUN = weighted.mean, weights = layers_width, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)

					VWCcritsT <- SWPtoVWC(SWPcrit_MPa, texture$sand.top, texture$clay.top)
					VWCcritsB <- if (length(bottomL) > 0 && !identical(bottomL, 0)) {
							SWPtoVWC(SWPcrit_MPa, texture$sand.bottom, texture$clay.bottom)
						} else {
							rep(NA, length(SWPcrit_MPa))
						}

					for (icrit in SWPcrit_MPa) {
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

						resMeans[nv+st_mo-1] <- ifelse(temp_top_mean > 0, temp_top_mean, 0)
						resSDs[nv+st_mo-1] <- ifelse(temp_top_mean > 0, temp_top_sd, 0)
						resMeans[nv+st_mo-1+12] <- ifelse(is.na(temp_bottom_mean) | temp_bottom_mean > 0, temp_bottom_mean, 0)
						resSDs[nv+st_mo-1+12] <- ifelse(is.na(temp_bottom_sd) | temp_bottom_sd > 0, temp_bottom_sd, 0)
						nv <- nv+24
					}

					rm(VWCcritsT, VWCcritsB, temp_top_mo, temp_top_mean, temp_top_sd, temp_bottom_mo, temp_bottom_mean, temp_bottom_sd)
				}
			#56
				if(aon$monthlyTranspiration){
					if(print.debug) print("Aggregation of monthlyTranspiration")
					if(!exists("transp.mo")) transp.mo <- get_Response_aggL(sc, sw_transp, tscale = "mo", scaler = 10, FUN = sum, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)

					resMeans[nv+st_mo-1] <- tapply(transp.mo$top, simTime2$month_ForEachUsedMonth, mean)
					resSDs[nv+st_mo-1] <- tapply(transp.mo$top, simTime2$month_ForEachUsedMonth, sd)
					resMeans[nv+st_mo-1+12] <- tapply(transp.mo$bottom, simTime2$month_ForEachUsedMonth, mean)
					resSDs[nv+st_mo-1+12] <- tapply(transp.mo$bottom, simTime2$month_ForEachUsedMonth, sd)
					nv <- nv+24
				}
			#57
				if(aon$monthlySoilEvaporation){
					if(print.debug) print("Aggregation of monthlySoilEvaporation")
					if(!exists("Esoil.mo")) Esoil.mo <- get_Response_aggL(sc, sw_evsoil, tscale = "mo", scaler = 10, FUN = sum, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)

					temp <- Esoil.mo$top + Esoil.mo$bottom
					resMeans[nv+st_mo-1] <- tapply(temp, simTime2$month_ForEachUsedMonth, mean)
					resSDs[nv+st_mo-1] <- tapply(temp, simTime2$month_ForEachUsedMonth, sd)
					nv <- nv+12
				}
			#58
				if(aon$monthlyAET){
					if(print.debug) print("Aggregation of monthlyAET")
					if(!exists("AET.mo")) AET.mo <- get_AET_mo(sc, runData, simTime)

					resMeans[nv+st_mo-1] <- tapply(AET.mo$val, simTime2$month_ForEachUsedMonth, mean)
					resSDs[nv+st_mo-1] <- tapply(AET.mo$val, simTime2$month_ForEachUsedMonth, sd)
					nv <- nv+12
				}
			#59
				if(aon$monthlyPET){
					if(print.debug) print("Aggregation of monthlyPET")
					if(!exists("PET.mo")) PET.mo <- get_PET_mo(sc, runData, simTime)

					resMeans[nv+st_mo-1] <- tapply(PET.mo$val, simTime2$month_ForEachUsedMonth, mean)
					resSDs[nv+st_mo-1] <- tapply(PET.mo$val, simTime2$month_ForEachUsedMonth, sd)
					nv <- nv+12
				}
			#59.2
				if (aon$monthlyVPD) {
					if (print.debug) print("Aggregation of monthlyVPD")
					if (!exists("temp.mo")) temp.mo <- get_Temp_mo(sc, runData, simTime)
					if (!exists("vpd.mo")) vpd.mo <- get_VPD_mo(sc, temp.mo, xin = swRunScenariosData, st2 = simTime2)

					nv_new <- nv + 12
					resMeans[nv:(nv_new - 1)] <- tapply(vpd.mo$mean, simTime2$month_ForEachUsedMonth, mean)
					resSDs[nv:(nv_new - 1)] <- tapply(vpd.mo$mean, simTime2$month_ForEachUsedMonth, sd)
					nv <- nv_new
				}
			#60
				if(aon$monthlyAETratios){
					if(print.debug) print("Aggregation of monthlyAETratios")
					if(!exists("AET.mo")) AET.mo <- get_AET_mo(sc, runData, simTime)
					if(!exists("Esoil.mo")) Esoil.mo <- get_Response_aggL(sc, sw_evsoil, tscale = "mo", scaler = 10, FUN = sum, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)
					if(!exists("transp.mo")) transp.mo <- get_Response_aggL(sc, sw_transp, tscale = "mo", scaler = 10, FUN = sum, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)

          temp <- ifelse(AET.mo$val < tol, 0, (transp.mo$top + transp.mo$bottom) / AET.mo$val)
					resMeans[nv+st_mo-1] <- tapply(temp, simTime2$month_ForEachUsedMonth, mean)
					resSDs[nv+st_mo-1] <- tapply(temp, simTime2$month_ForEachUsedMonth, sd)

					temp <- ifelse(AET.mo$val < tol, 0, (Esoil.mo$top + Esoil.mo$bottom) / AET.mo$val)
					resMeans[nv+st_mo-1+12] <- tapply(temp, simTime2$month_ForEachUsedMonth, mean)
					resSDs[nv+st_mo-1+12] <- tapply(temp, simTime2$month_ForEachUsedMonth, sd)
					nv <- nv+24
				}
			#61
				if(aon$monthlyPETratios){
					if(print.debug) print("Aggregation of monthlyPETratios")
					if(!exists("PET.mo")) PET.mo <- get_PET_mo(sc, runData, simTime)
					if(!exists("Esoil.mo")) Esoil.mo <- get_Response_aggL(sc, sw_evsoil, tscale = "mo", scaler = 10, FUN = sum, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)
					if(!exists("transp.mo")) transp.mo <- get_Response_aggL(sc, sw_transp, tscale = "mo", scaler = 10, FUN = sum, x = runData, st = simTime, st2 = simTime2, topL = topL, bottomL = bottomL)

          temp <- ifelse(PET.mo$val < tol, 0, (transp.mo$top + transp.mo$bottom) / PET.mo$val)
					resMeans[nv+st_mo-1] <- tapply(temp, simTime2$month_ForEachUsedMonth, mean)
					resSDs[nv+st_mo-1] <- tapply(temp, simTime2$month_ForEachUsedMonth, sd)

					temp <- ifelse(PET.mo$val < tol, 0, (Esoil.mo$top + Esoil.mo$bottom) / PET.mo$val)
					resMeans[nv+st_mo-1+12] <- tapply(temp, simTime2$month_ForEachUsedMonth, mean)
					resSDs[nv+st_mo-1+12] <- tapply(temp, simTime2$month_ForEachUsedMonth, sd)
					nv <- nv+24
				}

				#---Aggregation: Potential regeneration
				#regeneration: accountNSHemispheres_agg
			#62
				if(aon$dailyRegeneration_bySWPSnow) {
					if(print.debug) print("Aggregation of dailyRegeneration_bySWPSnow")
					if(!exists("swpmatric.dy.all")) swpmatric.dy.all <- list(val=-1/10*slot(slot(runData[[sc]],sw_swp),"Day"))	#no vwcdy available!
					swp.surface <- swpmatric.dy.all$val[simTime$index.usedy, 3]
					if(!exists("SWE.dy")) SWE.dy <- get_SWE_dy(sc, runData, simTime)

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
			#63
				if(aon$dailyRegeneration_GISSM & no.species_regeneration > 0){
					# Schlaepfer, D.R., Lauenroth, W.K. & Bradford, J.B. (2014). Modeling regeneration responses of big sagebrush (Artemisia tridentata) to abiotic conditions. Ecol Model, 286, 66-77.
					if(print.debug) print("Aggregation of dailyRegeneration_GISSM")
					#---Access daily data, which do not depend on specific species parameters, i.e., start of season

					if(!exists("swpmatric.dy.all")) swpmatric.dy.all <- list(val=-1/10*slot(slot(runData[[sc]],sw_swp),"Day"))	#no vwcdy available!
					temp.snow <- slot(slot(runData[[sc]],sw_snow),"Day")
					temp.temp <- slot(slot(runData[[sc]],sw_temp),"Day")
					TmeanJan <- mean(temp.temp[simTime$index.usedy, 5][simTime2$month_ForEachUsedDay_NSadj==1], na.rm=TRUE)	#mean January (N-hemisphere)/July (S-hemisphere) air temperature based on normal 'doy'
					temp.soiltemp <- slot(slot(runData[[sc]],sw_soiltemp),"Day")
					if(inherits(temp.soiltemp, "try-error") || anyNA(temp.soiltemp[, -(1:2)]) || all(temp.soiltemp[, -(1:2)] == 0)){
						use.soiltemp <- FALSE	#flag whether soil temperature output is available or not (and then air temperature is used instead of top soil temperature)
					} else {
						use.soiltemp <- TRUE	#currently we have only mean daily soil temperatures and not min/max which we need fo the model
					}

					#Loop through each species
					prev.Doy_SeedDispersalStart <- 0
					for (sp in seq_len(no.species_regeneration)) {
						param <- data.frame(t(param.species_regeneration[,sp]))

            #Regeneration year=RY: RYdoy=1 == start of seed dispersal = start of 'regeneration year'
            temp <- param$Doy_SeedDispersalStart0 +
              param$SeedDispersalStart_DependencyOnMeanTempJanuary * TmeanJan
            Doy_SeedDispersalStart <- as.integer(max(round(temp, 0) %% 365, 1))

            moveByDays <- if (Doy_SeedDispersalStart > 1) {
                temp <- ISOdate(simTime$useyrs[1] - 1, 12, 31, tz = "UTC") -
                        ISOdate(simTime$useyrs[1] - 1, 1, 1, tz = "UTC") + 1 -
                        (Doy_SeedDispersalStart - 1)
                as.integer(max(c(as.numeric(temp) %% 365, 1)))
              } else {
                1L
              }

						#Calculate regeneration year dates
            et <- simTime$no.usedy
						itail <- (et - moveByDays + 1):et
						if (startyr > simstartyr) {
						  #start earlier to complete RY
						  st <- simTime$index.usedy[1]
							RY.index.usedy <- c((st - moveByDays):(st - 1), simTime$index.usedy[-itail]) #index indicating which rows of the daily SoilWat output is used
							RYyear_ForEachUsedDay <- simTime2$year_ForEachUsedDay	#'regeneration year' for each used day
							RYdoy_ForEachUsedDay <- simTime2$doy_ForEachUsedDay	#'doy of the regeneration year' for each used day

						} else if (!(startyr > simstartyr)) {
						  #start later to get a complete RY
							RY.index.usedy <- simTime$index.usedy[-c(1:(Doy_SeedDispersalStart - 1), itail)]
							temp <- which(simTime2$year_ForEachUsedDay == simTime2$year_ForEachUsedDay[1])
							RYyear_ForEachUsedDay <- simTime2$year_ForEachUsedDay[-temp]
							RYdoy_ForEachUsedDay <- simTime2$doy_ForEachUsedDay[-temp]
						}
						RY.useyrs <- unique(RYyear_ForEachUsedDay)	#list of 'regeneration years' that are used for aggregation

						# normal year for each used 'doy of the regeneration year'
						RY_N_usedy <- length(RY.index.usedy)
						itail <- (RY_N_usedy - moveByDays + 1):RY_N_usedy
						year_ForEachUsedRYDay <- c(rep(simTime$useyrs[1] - 1, moveByDays),
						                            RYyear_ForEachUsedDay[-itail])
            # normal doy for each used 'doy of the regeneration year'
						st <- simTime$index.usedy[1]
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
							Germination_AtMoreThanTopSWPmin <- apply(swp[, SoilLayers_RelevantToGermination], MARGIN=1, FUN=function(x) all(x >= param$SWP_MinimumForGermination))
							swp.TopMean <- apply(swp[, SoilLayers_RelevantToGermination], MARGIN=1, FUN=mean, na.rm=TRUE)
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
									killed_byCauses_onRYdoy <- rep(NA, times = 6)	#book-keeping of mortality causes
									names(killed_byCauses_onRYdoy) <- colnames(SeedlingMortality_CausesByYear)[1:6]
									stopped_byCauses_onRYdoy <- rep(NA, times = 3)	#book-keeping of causes why growth stopped
									names(stopped_byCauses_onRYdoy) <- colnames(SeedlingMortality_CausesByYear)[7:9]

									#Establish days of growth (=TRUE) and surviving, but no growth (=FALSE)
									thisSeedlingGrowing <- rep(TRUE, no.days)
									if (sg_RYdoy > 1)
									  thisSeedlingGrowing[seq_len(sg_RYdoy - 1)] <- FALSE	#seedling germinated on sg_RYdoy, hence it cannot grow before germination day

									#Check growth under above-ground conditions
									#Snow cover
									thisSeedlingGrowth_AbsenceOfSnowCover <- calculate_SuitableGrowthThisYear_UnderCondition(favorable.conditions=thisSeedlingGrowing & thisYear_SeedlingGrowth_AbsenceOfSnowCover, consequences.unfavorable=param$SeedlingGrowth_0StopOr1Resume)
									temp <- !thisSeedlingGrowth_AbsenceOfSnowCover[index.thisSeedlingSeason]
									if (any(temp))
									  stopped_byCauses_onRYdoy["Seedlings1stSeason.Mortality.DuringStoppedGrowth.DueSnowCover"] <- sg_RYdoy + which(temp)[1]
									#Minimum temperature
									thisSeedlingGrowth_AtAboveTmin <- calculate_SuitableGrowthThisYear_UnderCondition(favorable.conditions=thisSeedlingGrowing & thisYear_SeedlingGrowth_AtAboveTmin, consequences.unfavorable=param$SeedlingGrowth_0StopOr1Resume)
									temp <- !thisSeedlingGrowth_AtAboveTmin[index.thisSeedlingSeason]
									if (any(temp))
									  stopped_byCauses_onRYdoy["Seedlings1stSeason.Mortality.DuringStoppedGrowth.DueTmin"] <- sg_RYdoy + which(temp)[1]
									#Maximum temperature
									thisSeedlingGrowth_AtBelowTmax <- calculate_SuitableGrowthThisYear_UnderCondition(favorable.conditions=thisSeedlingGrowing & thisYear_SeedlingGrowth_AtBelowTmax, consequences.unfavorable=param$SeedlingGrowth_0StopOr1Resume)
									temp <- !thisSeedlingGrowth_AtBelowTmax[index.thisSeedlingSeason]
									if (any(temp))
									  stopped_byCauses_onRYdoy["Seedlings1stSeason.Mortality.DuringStoppedGrowth.DueTmax"] <- sg_RYdoy + which(temp)[1]
									#Updated days of growth or surviving
									thisSeedlingGrowing <- thisSeedlingGrowing & thisSeedlingGrowth_AbsenceOfSnowCover & thisSeedlingGrowth_AtAboveTmin & thisSeedlingGrowth_AtBelowTmax
									thisSeedlingLivingButNotGrowing <- !thisSeedlingGrowing
									if (sg_RYdoy > 1)
									  thisSeedlingLivingButNotGrowing[seq_len(sg_RYdoy - 1)] <- FALSE	#seedling germinated on sg_RYdoy, hence it cannot live before germination day

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
						index_RYuseyr <- unique(year_ForEachUsedRYDay) %in% simTime$useyr
						res1.yr_v0 <- aggregate(dat_gissm1, by = list(year_ForEachUsedRYDay), FUN = sum)
						res1.yr <- res1.yr_v0[index_RYuseyr, -1]
						stemp <- res1.yr > 0
						resMeans[nv:(nv+1)] <- apply(stemp, 2, mean, na.rm = TRUE)
						resSDs[nv:(nv+1)] <- apply(stemp, 2, sd, na.rm = TRUE)
						#Periods with no successes
						rleGerm <- rle(stemp[, 1])
						if (any(!rleGerm$values))
						  resMeans[(nv+2):(nv+4)] <- quantile(rleGerm$lengths[!rleGerm$values],
						                                      probs = c(0.05, 0.5, 0.95), type = 7)
						rleSling <- rle(stemp[, 2])
						if (any(!rleSling$values))
						  resMeans[(nv+5):(nv+7)] <- quantile(rleSling$lengths[!rleSling$values],
						                                      probs = c(0.05, 0.5, 0.95), type = 7)
						#Mean number of days per year with success
						resMeans[(nv+8):(nv+9)] <- apply(res1.yr, 2, mean)
						resSDs[(nv+8):(nv+9)] <- apply(res1.yr, 2, sd)
						#Days of year (in normal count) of most frequent successes among years: #toDoy <- function(x) sort(ifelse((temp <- x+Doy_SeedDispersalStart-1) > 365, temp-365,temp)) #convert to normal doys
						res1.dy <- aggregate(dat_gissm1, by = list(doy_ForEachUsedRYDay), FUN = sum)
						resMeans[(nv+10):(nv+15)] <- get.DoyMostFrequentSuccesses(res1.dy, dat_gissm1)
						#Mean number of days when germination is restricted due to conditions
						res2.yr_v0 <- aggregate(dat_gissm2, by = list(year_ForEachUsedRYDay), sum)
						res2.yr <- res2.yr_v0[index_RYuseyr, -1]
						resMeans[(nv+16):(nv+20)] <- apply(res2.yr, 2, mean)
						resSDs[(nv+16):(nv+20)] <- apply(res2.yr, 2, sd)
						#Mean time to germinate in days
						res3.yr_v0 <- tapply(Germination_TimeToGerminate, year_ForEachUsedRYDay, mean, na.rm = TRUE)
						res3.yr <- res3.yr_v0[index_RYuseyr]
						resMeans[nv+21] <- mean(res3.yr, na.rm = TRUE)
						resSDs[nv+21] <- sd(res3.yr, na.rm = TRUE)
						#Mean number of days per year of different types of mortalities
						resMeans[(nv+22):(nv+30)] <- apply(SeedlingMortality_CausesByYear, 2, mean, na.rm = TRUE) #if value==NA, then no germinations that year
						resSDs[(nv+22):(nv+30)] <- apply(SeedlingMortality_CausesByYear, 2, sd, na.rm = TRUE) #if value==NA, then no germinations that year

						nv <- nv+31

						#---Aggregate time series output
						if(any(ouput_aggregated_ts=="Regeneration")){
							#Table with data for every year
							res1.yr.doy <- t(simplify2array(by(dat_gissm1, INDICES=year_ForEachUsedRYDay, FUN=function(x) get.DoyMostFrequentSuccesses(x, dat_gissm1))))[simTime$index.useyr, ]

							res.yr <- data.frame(data.frame(res1.yr_v0, res2.yr_v0[, -1], res3.yr_v0)[index_RYuseyr, ], SeedlingMortality_CausesByYear, res1.yr.doy)
							temp.header2 <- c("DaysWith_GerminationSuccess", "DaysWith_SeedlingSurvival1stSeason",
									"Days_GerminationRestrictedByTmax", "Days_GerminationRestrictedByTmin", "Days_GerminationRestrictedBySWPmin", "Days_GerminationRestrictedByAnyCondition", "Days_GerminationRestrictedByTimeToGerminate",
									"MeanDays_TimeToGerminate",
									paste("Days", colnames(SeedlingMortality_CausesByYear), sep="_"),
									paste(rep(c("Start90%", "Median", "End90%"), times=2), rep(c("DoyMostFrequent_GerminationSuccess", "DoyMostFrequent_SeedlingSurvival1stSeason"), each=3), sep="_"))
							colnames(res.yr) <- c("Year", temp.header2)
							write.csv(res.yr, file=file.path(dir.at, paste("Scenario", formatC(sc-1, width=2, format="d", flag="0"), "_", climate.conditions[sc], "_", i_label, "_", colnames(param.species_regeneration)[sp], "_Regeneration.csv", sep="")))

							#Plot with data for every day
							pdf(file=file.path(dir.at, paste("Scenario", formatC(sc-1, width=2, format="d", flag="0"), "_", climate.conditions[sc], "_", i_label, "_", colnames(param.species_regeneration)[sp], "_Regeneration.pdf", sep="")),
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
							mtext(i_label)
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
				P_id <- it_Pid(i_sim, sc, scenario_No, runsN_master, runIDs_sites)

				if (dbOverallColumns > 0 && dbOverallColumns == (nv - 1)) {
					resMeans[!is.finite(resMeans)] <- "NULL"
					resSDs[!is.finite(resSDs)] <- "NULL"
					temp1 <- paste0(c(P_id, resMeans[1:(nv-1)]), collapse = ",")
					temp2 <- paste0(c(P_id, resSDs[1:(nv-1)]), collapse = ",")
				} else {
					temp1 <- temp2 <- P_id
				}
				SQL1 <- paste0("INSERT INTO \"aggregation_overall_mean\" VALUES (", temp1, ");")
				SQL2 <- paste0("INSERT INTO \"aggregation_overall_sd\" VALUES (", temp2, ");")

				if(length(SQL) == 0) {
					SQL <- paste(SQL1, SQL2, sep="\n")
				} else {
					SQL <- paste(SQL, SQL1, SQL2, sep="\n")
				}
			}

			#Daily Output
			if(daily_no > 0){
				dailyList <- list()
				SQLc <- ""
				#aggregate for each response variable
				for (doi in 1:daily_no) {
					if(print.debug) print(paste("Aggregation of mean daily outputs:", doi))

					if(!continueAfterAbort | (continueAfterAbort & !isdone.dailyAggs[doi, sc])){
						#check to see if we are on SWA
						if(regexpr("SWAbulk", output_aggregate_daily[doi]) > 0){
							agg.resp <- "SWAbulk"
							index.SWPcrit <- -as.numeric(sub("kPa", "", sub("SWAbulkatSWPcrit", "", output_aggregate_daily[doi])))/1000
						} else {
							agg.resp <- output_aggregate_daily[doi]
						}

						agg.analysis <- switch(EXPR=agg.resp, AET=1, Transpiration=2, EvaporationSoil=1, EvaporationSurface=1, EvaporationTotal=1, VWCbulk=2, VWCmatric=2, SWCbulk=2, SWPmatric=2, SWAbulk=2, Snowpack=1, Rain=1, Snowfall=1, Snowmelt=1, SnowLoss=1, Infiltration=1, DeepDrainage=1, PET=1, TotalPrecipitation=1, TemperatureMin=1, TemperatureMax=1, SoilTemperature=2, Runoff=1)
						agg.no <- ifelse(agg.analysis == 1, 1, aggLs_no)

						res.dailyMean <- res.dailySD <- rep(NA, times=ifelse(agg.analysis == 1, 1, ifelse(daily_lyr_agg[["do"]], agg.no, SoilLayer_MaxNo)) * 366)

						scaler <- switch(EXPR=output_aggregate_daily[doi], SWPmatric=1, VWCbulk=1, VWCmatric=1, TemperatureMin=1, TemperatureMax=1, SoilTemperature=1, 10) 	# SWP: -bar => MPa (but, since calculated via VWC, needs be same as VWC); VWC: # cm/cm -> m3/m3; default: cm => mm

						#read in data unless Exclude_ClimateAmbient
						if(!Exclude_ClimateAmbient) {
							if(agg.resp == "EvaporationTotal"){
								temp1 <- slot(slot(runData[[sc]],sw_evsoil),"Day")
								temp2 <- slot(slot(runData[[sc]],sw_evapsurface),"Day")
							} else {#"VWCbulk","VWCmatric", "SWCbulk", "SWPmatric","SWAbulk"
								agg.file <- switch(EXPR=agg.resp,
										AET=sw_aet,
										Transpiration=sw_transp,
										EvaporationSoil=sw_evsoil,
										EvaporationSurface=sw_evapsurface,
										VWCbulk=sw_vwcbulk,
										VWCmatric=sw_vwcmatric,
										SWCbulk=sw_swcbulk,
										SWPmatric=sw_vwcmatric,#TODO: this was sw_vwc so can we just do sw_swpmatric?
										SWAbulk=sw_swcbulk,#TODO: this was sw_swc so can we just do sw_swa?
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
								temp1 <- slot(slot(runData[[sc]],agg.file),"Day")
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
								if( any(!is.na(match(agg.resp, c("VWCbulk", "VWCmatric", "SWPmatric", "SoilTemperature")))) ){ #aggregate by functions that are weighted by depths of soil layers
									agg.agg <- weighted.mean
									agg.w <- layers_width
								} else if( any(!is.na(match(agg.resp, c("Transpiration", "SWCbulk", "SWAbulk")))) ){#aggregate by simple functions
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
								if(agg.resp == "SWPmatric"){ ##post-aggregate calculation of SWP: convert VWC to SWP
									res.dailyMean[ir] <- VWCtoSWP(res.dailyMean[ir], textureDAgg$sand[al], textureDAgg$clay[al])
									res.dailySD[ir] <- 0 #was NA now 0
								} else {
									res.dailySD[ir] <- aggregate(scaler * agg.dat[[al]], by=list(simTime2$doy_ForEachUsedDay), FUN=sd)[, 2]
								}
							}

							#post-aggregate calculation of SWA based on SWC for each SWPcrit
							if(agg.resp == "SWAbulk"){
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
						P_id <- it_Pid(i_sim, sc, scenario_No, runsN_master, runIDs_sites)

						#save(agg.analysis, aggLs_no, P_id, header, sc, agg.resp, res.dailyMean, res.dailySD, file=file.path(dir.out, paste(mpi.comm.rank(),"of",mpi.comm.size(),"_sc_",sc,"_doi_",doi,".r",sep="")))
						if(agg.analysis == 1){
							res.dailyMean[!is.finite(res.dailyMean)] <- "NULL"
							res.dailySD[!is.finite(res.dailySD)] <- "NULL"
							SQL1 <- paste0("INSERT INTO ",paste("aggregation_doy_", output_aggregate_daily[doi], "_Mean", sep=""), " VALUES ", paste0("(",sapply(1:agg.no, FUN=function(x) {paste0(P_id,",",paste0(res.dailyMean[((x*366)-365):(x*366)],collapse=","))}), ")", sep="", collapse = ","), ";", sep="")
							SQL2 <- paste0("INSERT INTO ",paste("aggregation_doy_", output_aggregate_daily[doi], "_SD", sep=""),   " VALUES ", paste0("(",sapply(1:agg.no, FUN=function(x) {paste0(P_id,",",paste0(res.dailySD[((x*366)-365):(x*366)],collapse=","))}), ")", sep="", collapse = ","), ";", sep="")
							SQL <- paste(SQL, SQL1, SQL2, sep="\n")

						} else {
							#save(res.dailyMean,agg.no,header,header.names,P_id, res.dailySD,agg.analysis, aggLs_no,aggLs,agg.resp,layers_width,file=file.path(dir.out, "readThis.r"))
							res.dailyMean[!is.finite(res.dailyMean)] <- "NULL"
							res.dailySD[!is.finite(res.dailySD)] <- "NULL"
							SQL1 <- paste0("INSERT INTO ",paste("aggregation_doy_", output_aggregate_daily[doi], "_Mean", sep=""), " VALUES ", paste0("(",sapply(1:agg.no, FUN=function(x) {paste0(P_id,",", x,",",paste0(res.dailyMean[((x*366)-365):(x*366)],collapse=","))}), ")", sep="", collapse = ","), ";", sep="")
							SQL2 <- paste0("INSERT INTO ",paste("aggregation_doy_", output_aggregate_daily[doi], "_SD", sep=""),   " VALUES ", paste0("(",sapply(1:agg.no, FUN=function(x) {paste0(P_id,",", x,",",paste0(res.dailySD[((x*366)-365):(x*366)],collapse=","))}), ")", sep="", collapse = ","), ";", sep="")
							SQL <- paste(SQL, SQL1, SQL2, sep="\n")
						}

					}#end if continueAfterAbort
				}#doi loop
			}#end if daily output

			if(tasks$aggregate > 0L && length(SQL) > 0 && sc == 1){
				#Clear SQL
				SQLcurrent <- SQL
				SQL <- character(0)
			}
		} #end loop through scenarios

	} #end if do aggregate

  if (tasks$aggregate > 0L && (length(SQL) > 0 || length(SQLcurrent) > 0)) {
    tasks$aggregate <- 2L
    if (length(SQLcurrent) > 0)
      cat(SQLcurrent, file = dbTempFileCurrent, append = TRUE, sep = "\n")
    if (length(SQL) > 0)
      cat(SQL, file = dbTempFile, append = TRUE, sep = "\n")
  }

  if (all(unlist(tasks) != 0)) {
    #ETA estimation
    dt <- round(difftime(Sys.time(), time.sys, units = "secs"), 2)
    ftemp <- file.path(dir.out, timerfile)
    cat(paste(i_sim, dt, sep = ","), file = ftemp, append = TRUE, sep = "\n")
    times <- swsf_read_csv(file = ftemp, header = FALSE,
          nrows = runsN_total + 1, colClasses = c("NULL", "numeric"))[[1]]

    if (!be.quiet) {
      n <- length(times) - 1
      temp <- paste0(i_sim, ": ", i_label, " done in ", dt, " ", units(dt), ": ",
                    round(n / runsN_total * 100, 2), "% complete")

      if (eta.estimate) {
        deta <- round(ceiling((runsN_total - n) / workersN) *
          sapply(list(mean, sd), function(f) f(times, na.rm = TRUE)))
        pi95 <- deta[2] * sqrt(1 + 1 / n) * qt(0.975, n) # 95% prediction interval
        pi95 <- if (is.na(pi95)) "NA" else if (pi95 > 3600) {
            paste(round(pi95 / 3600), "h")
          } else if (pi95 > 60) {
            paste(round(pi95 / 60), "min")
          } else {
            paste(round(pi95), "s")
          }
        temp <- paste0(temp, ", ETA (mean ± 95%-PI) = ",
                      Sys.time() + deta[1], " ± ", pi95)
      }

      print(temp)
    }

  } else {
    print(paste0(i_sim, ": ", i_label, " unsuccessful: ",
          paste(names(tasks), "=", tasks, collapse = ", ")))
  }

	on.exit()

	return(1)
} #end do_OneSite()
}
#--------------------------------------------------------------------------------------------------#
#------------------------RUN RSOILWAT

# print system information
print(temp <- sessionInfo())
if (check.blas && grepl("darwin", temp$platform)) { # apparently this works only on Mac OS X
	blas <- system2(command = file.path(Sys.getenv()[["R_HOME"]], "R"), args = "CMD config BLAS_LIBS", stdout = TRUE)
	blas <- sub("-L/", "/", (strsplit(blas, split=" ")[[1]][1]))
	lapack <- system2(command = file.path(Sys.getenv()[["R_HOME"]], "R"), args = "CMD config LAPACK_LIBS", stdout = TRUE)
	lapack <- sub("-L/", "/", (strsplit(lapack, split=" ")[[1]][1]))
	get_ls <- if(identical(blas, lapack)) list(blas) else list(blas, lapack)
	temp <- lapply(get_ls, FUN = function(x) print(system2(command = "ls", args = paste("-l", x), stdout = TRUE)))

	print("Check linked BLAS library:") # http://simplystatistics.org/2016/01/21/parallel-blas-in-r/#
	print(system.time({ x <- replicate(5e3, rnorm(5e3)); tcrossprod(x) }))

	# Example values:
	# Apple's Accelerate framework:
	#   user  system elapsed
	# 14.755   0.268   3.423

	# ATLAS 3.10.2_2:
	#   user  system elapsed
	# 22.218   0.647   3.340

	# Built-in reference BLAS:
	#   user  system elapsed
	# 59.289   0.465  59.173
}

# run the simulation experiment
if(actionWithSoilWat && runsN_todo > 0){

	swDataFromFiles <- sw_inputDataFromFiles(dir=dir.sw.in,files.in=swFilesIn) #This acts for the basis for all runs.
	if (length(swDataFromFiles@weatherHistory) > 0)
		swDataFromFiles@weatherHistory <- list(swClear(swDataFromFiles@weatherHistory[[1]])) # we don't need the example weather data; the code will get weather data separately
	#Used for weather from files
	filebasename <- basename(swFiles_WeatherPrefix(swDataFromFiles))

	# List of objects to export which are required by do_OneSite and are not in rSWSF (sorted alphabetically)
	list.export <- c("accountNSHemispheres_agg", "accountNSHemispheres_veg",
    "adjust.soilDepth", "aon", "be.quiet", "bin.prcpfreeDurations", "bin.prcpSizes",
    "climate.conditions", "cloudin", "continueAfterAbort", "counter.digitsN",
    "create_experimentals", "create_treatments", "daily_lyr_agg",
    "daily_no", "datafile.windspeedAtHeightAboveGround", "dbOverallColumns",
    "dbWeatherDataFile", "debug.dump.objects", "DegreeDayBase", "Depth_TopLayers",
    "dir.code", "dir.ex.daymet", "dir.ex.maurer2002", "dir.out", "dir.out.temp",
    "dir.prj", "dir.sw.in.tr", "dir.sw.runs", "dirname.sw.runs.weather",
    "do_OneSite", "do.GetClimateMeans", "done_prior", "endyr", "estabin",
    "establishment.delay", "establishment.duration", "establishment.swp.surface",
    "eta.estimate", "exec_c_prefix", "ExpInput_Seperator", "expN", "filebasename",
    "filebasename.WeatherDataYear", "germination.duration", "germination.swp.surface",
    "get.month", "getCurrentWeatherDataFromDatabase", "getScenarioWeatherDataFromDatabase",
    "growing.season.threshold.tempC", "increment_soiltemperature_deltaX_cm",
    "makeInputForExperimentalDesign", "name.OutputDB", "no.species_regeneration",
    "ouput_aggregated_ts", "output_aggregate_daily", "parallel_backend",
    "parallel_runs", "param.species_regeneration", "pcalcs", "print.debug",
    "prodin", "runIDs_sites", "runsN_master", "runsN_sites", "runsN_todo",
    "runsN_total", "saveRsoilwatInput", "saveRsoilwatOutput", "scenario_No",
    "season.end", "season.start", "shrub.fraction.limit", "simstartyr",
    "simTime", "simTime_ForEachUsedTimeUnit_North", "simTime_ForEachUsedTimeUnit_South",
    "siteparamin", "soilsin", "startyr",
    "sw_aet", "sw_deepdrain", "sw_evapsurface", "sw_evsoil", "sw_hd",
    "sw_inf_soil", "sw_input_climscen_use", "sw_input_climscen_values_use",
    "sw_input_cloud_use", "sw_input_experimentals", "sw_input_experimentals_use",
    "sw_input_prod_use", "sw_input_site_use", "sw_input_soils_use",
    "sw_input_weather_use", "sw_interception", "sw_percolation",
    "sw_pet", "sw_precip", "sw_runoff", "sw_snow", "sw_soiltemp",
    "sw_swcbulk", "sw_swpmatric", "sw_temp", "sw_transp", "sw_vwcbulk",
    "sw_vwcmatric", "sw.inputs", "sw.outputs", "swcsetupin", "swDataFromFiles",
    "swFilesIn", "swOutSetupIn", "SWPcrit_MPa", "timerfile", "Tmean_crit_C", "Tmax_crit_C",
    "Tmin_crit_C", "tr_cloud", "tr_files", "tr_input_climPPT", "tr_input_climTemp",
    "tr_input_EvapCoeff", "tr_input_shiftedPPT", "tr_input_SnowD",
    "tr_input_TranspCoeff", "tr_input_TranspCoeff_Code", "tr_input_TranspRegions",
    "tr_prod", "tr_site", "tr_soil", "tr_VegetationComposition",
    "tr_weather", "use_rcpp", "weatherin", "workersN", "yearsin")
  #list.export <- list.export[!duplicated(list.export)]

  swsf_env <- new.env(parent = emptyenv())
  load(rSWSF, envir = swsf_env)
  list.export <- unique(c(ls(envir = swsf_env), list.export))

  list_envs <- list(rSWSF = swsf_env,
                    local = environment(),
                    parent = parent.frame(),
                    global = .GlobalEnv)

	#ETA calculation
	if (!be.quiet)
	  print(paste("SWSF simulation runs:", runsN_todo, "out of", runsN_total, "runs will be carried out on", workersN, "cores: started at", t1 <- Sys.time()))

	inputDataToSave <- list()

	if (parallel_runs && parallel_init) {
		#call the simulations depending on parallel backend
		if (identical(parallel_backend, "mpi")) {
			mpi.bcast.cmd(library(Rsoilwat31, quietly = TRUE))
			mpi.bcast.cmd(library(circular, quietly = TRUE))
			mpi.bcast.cmd(library(SPEI, quietly = TRUE))
			mpi.bcast.cmd(library(RSQLite, quietly = TRUE))

      export_objects_to_workers(list.export, list_envs, "mpi")
      mpi.bcast.cmd(source(file.path(dir.code, "R", "SWSF_cpp_functions.R")))
      if (print.debug) {
        mpi.bcast.cmd(print(paste("Slave", mpi.comm.rank(), "has", length(ls()), "objects")))
      }
      mpi.bcast.cmd(Rsoilwat31::dbW_setConnection(dbFilePath = dbWeatherDataFile))
			mpi.bcast.cmd(mpi_work())

			junk <- 0L
			closed_slaves <- 0L
			runs.completed <- 1L
			#sTag <- c("Ready for task", "Done with Task", "Exiting")
			while (closed_slaves < workersN) {
tryCatch({
        if (print.debug) {
          print(paste(Sys.time(), ": master is waiting for slaves to communicate"))
        }
				complete <- mpi.recv.Robj(mpi.any.source(), mpi.any.tag())
				complete_info <- mpi.get.sourcetag()
				slave_id <- complete_info[1]
				tag <- complete_info[2]
        if (print.debug) {
          print(paste(Sys.time(),
                      ": master has received communication from slave", slave_id,
                      "with tag", tag,
                      "and message", paste(complete, collapse = ", ")))
        }

				if (tag == 1L) {
					temp <- Sys.time() - t.overall
					units(temp) <- "secs"
					temp <- as.double(temp)

					# slave is ready for a task. Give it the next task, or tell it tasks
					# are done if there are none.
          if ((runs.completed <= length(runIDs_todo)) & (temp < MaxDoOneSiteTime)) {
						# Send a task, and then remove it from the task list
						i_site <- it_site(runIDs_todo[runs.completed], runsN_master, runIDs_sites)
						i_labels <- labels[i_site]
						i_SWRunInformation <- SWRunInformation[i_site, ]
						i_sw_input_soillayers <- sw_input_soillayers[i_site, ]
						i_sw_input_treatments <- sw_input_treatments[i_site, ]
						i_sw_input_cloud <- sw_input_cloud[i_site, ]
						i_sw_input_prod <- sw_input_prod[i_site, ]
						i_sw_input_site <- sw_input_site[i_site, ]
						i_sw_input_soils <- sw_input_soils[i_site, ]
						i_sw_input_weather <- sw_input_weather[i_site, ]
						i_sw_input_climscen <- sw_input_climscen[i_site, ]
						i_sw_input_climscen_values <- sw_input_climscen_values[i_site, ]

						dataForRun <- list(do_OneSite=TRUE, i_sim=runIDs_todo[runs.completed], labels=i_labels, SWRunInformation=i_SWRunInformation, sw_input_soillayers=i_sw_input_soillayers, sw_input_treatments=i_sw_input_treatments, sw_input_cloud=i_sw_input_cloud, sw_input_prod=i_sw_input_prod, sw_input_site=i_sw_input_site, sw_input_soils=i_sw_input_soils, sw_input_weather=i_sw_input_weather, sw_input_climscen=i_sw_input_climscen, sw_input_climscen_values=i_sw_input_climscen_values)
						mpi.send.Robj(dataForRun, slave_id, 1)
						if (print.debug) {
						  print(paste("Slave:", slave_id,
						              "Run:", runIDs_todo[runs.completed],
						              "started at", Sys.time()))
						}
						runs.completed <- runs.completed + 1L

					} else {
						mpi.send.Robj(junk, slave_id, 2)
					}

				} else if (tag == 2L) {
					# The message contains results: store result in a data structure
					inputDataToSave[[complete$i]] <- complete$r
					if (print.debug) {
					  print(paste("Run:", complete, "at", Sys.time()))
          }

				} else if (tag == 3L) {
					# A slave has closed down.
					closed_slaves <- closed_slaves + 1L
					if (print.debug) {
					  print(paste("Slave:", slave_id, "closed at", Sys.time()))
          }

				} else if (tag == 4L) {
					#The slave had a problem with Soilwat record Slave number and the Run number.
					print("Problem with run:", complete, "on slave:", save_id, "at", Sys.time())
					ftemp <- file.path(dir.out, "ProblemRuns.csv")
					if (!file.exists(ftemp))
            cat("Slave,Run", file = ftemp, sep = "\n")
          cat(paste(slave_id, complete, sep = ","), file = ftemp, append = TRUE, sep = "\n")
				}

}, interrupt=function(interrupt) {
	print("Ctrl-C caught bringing work to an end.")
	print(interrupt)
	MaxDoOneSiteTime <<- 0
})
			}

			mpi.bcast.cmd(Rsoilwat31::dbW_disconnectConnection())
			mpi.bcast.cmd(rm(list=ls())) #do not remove 'ls(all=TRUE)' because there are important .XXX objects that are important for proper slave functioning!
			mpi.bcast.cmd(gc())
			print(runs.completed)
		}

		if (identical(parallel_backend, "snow")) {
			snow::clusterEvalQ(cl, library(Rsoilwat31, quietly = TRUE))
			snow::clusterEvalQ(cl, library(circular, quietly = TRUE))
			snow::clusterEvalQ(cl, library(SPEI, quietly = TRUE))
			snow::clusterEvalQ(cl, library(RSQLite, quietly = TRUE))

      export_objects_to_workers(list.export, list_envs, "snow", cl)
      snow::clusterEvalQ(cl, source(file.path(dir.code, "R", "SWSF_cpp_functions.R")))
			snow::clusterEvalQ(cl, Rsoilwat31::dbW_setConnection(dbFilePath = dbWeatherDataFile))

			runs.completed <- foreach(i_sim=runIDs_todo, .combine="+", .inorder=FALSE) %dopar% {
				i_site <- it_site(i_sim, runsN_master, runIDs_sites)
				do_OneSite(i_sim=i_sim, i_labels=labels[i_site], i_SWRunInformation=SWRunInformation[i_site, ], i_sw_input_soillayers=sw_input_soillayers[i_site, ], i_sw_input_treatments=sw_input_treatments[i_site, ], i_sw_input_cloud=sw_input_cloud[i_site, ], i_sw_input_prod=sw_input_prod[i_site, ], i_sw_input_site=sw_input_site[i_site, ], i_sw_input_soils=sw_input_soils[i_site, ], i_sw_input_weather=sw_input_weather[i_site, ], i_sw_input_climscen=sw_input_climscen[i_site, ], i_sw_input_climscen_values=sw_input_climscen_values[i_site, ])
			}

			snow::clusterEvalQ(cl, Rsoilwat31::dbW_disconnectConnection())
			snow::clusterEvalQ(cl, rm(list=ls()))
			snow::clusterEvalQ(cl, gc())
		}

		if (identical(parallel_backend, "multicore")) {
      source(file.path(dir.code, "R", "SWSF_cpp_functions.R"))
			Rsoilwat31::dbW_setConnection(dbFilePath = dbWeatherDataFile)

			runs.completed <- foreach(i_sim=runIDs_todo, .combine="+", .inorder=FALSE, .noexport=list.noexport) %dopar% {
				i_site <- it_site(i_sim, runsN_master, runIDs_sites)

				do_OneSite(i_sim=i_sim, i_labels=labels[i_site], i_SWRunInformation=SWRunInformation[i_site, ], i_sw_input_soillayers=sw_input_soillayers[i_site, ], i_sw_input_treatments=sw_input_treatments[i_site, ], i_sw_input_cloud=sw_input_cloud[i_site, ], i_sw_input_prod=sw_input_prod[i_site, ], i_sw_input_site=sw_input_site[i_site, ], i_sw_input_soils=sw_input_soils[i_site, ], i_sw_input_weather=sw_input_weather[i_site, ], i_sw_input_climscen=sw_input_climscen[i_site, ], i_sw_input_climscen_values=sw_input_climscen_values[i_site, ])
			}

			Rsoilwat31::dbW_disconnectConnection()
		}

	} else { #call the simulations in serial
		source(file.path(dir.code, "R", "SWSF_cpp_functions.R"))
		Rsoilwat31::dbW_setConnection(dbFilePath = dbWeatherDataFile)
		runs.completed <- 0

		runs.completed <- foreach(i_sim=runIDs_todo, .combine="+", .inorder=FALSE) %do% {
			i_site <- it_site(i_sim, runsN_master, runIDs_sites)
			do_OneSite(i_sim=i_sim, i_labels=labels[i_site], i_SWRunInformation=SWRunInformation[i_site, ], i_sw_input_soillayers=sw_input_soillayers[i_site, ], i_sw_input_treatments=sw_input_treatments[i_site, ], i_sw_input_cloud=sw_input_cloud[i_site, ], i_sw_input_prod=sw_input_prod[i_site, ], i_sw_input_site=sw_input_site[i_site, ], i_sw_input_soils=sw_input_soils[i_site, ], i_sw_input_weather=sw_input_weather[i_site, ], i_sw_input_climscen=sw_input_climscen[i_site, ], i_sw_input_climscen_values=sw_input_climscen_values[i_site, ])
		}

		Rsoilwat31::dbW_disconnectConnection()

		#Best for debugging
#		setwd(dir.prj)
#		exeEnv <- new.env()
#		for(n in list.export) assign(x=n,value=get(n,globalenv()), envir=exeEnv)
#
#		for(i_sim in runIDs_todo) {
#			i_site <- it_site(i_sim, runsN_master, runIDs_sites)
#
#			assign(x="runs.completed", value=0, envir=exeEnv)
#			assign(x="i_sim",value=i_sim,envir=exeEnv)
#			assign(x="i_labels",value=labels[i_site],envir=exeEnv)
#			assign(x="i_SWRunInformation",value=SWRunInformation[i_site, ],envir=exeEnv)
#			assign(x="i_sw_input_soillayers",value=sw_input_soillayers[i_site, ],envir=exeEnv)
#			assign(x="i_sw_input_treatments",value=sw_input_treatments[i_site, ],envir=exeEnv)
#			assign(x="i_sw_input_cloud",value=sw_input_cloud[i_site, ],envir=exeEnv)
#			assign(x="i_sw_input_prod",value=sw_input_prod[i_site, ],envir=exeEnv)
#			assign(x="i_sw_input_site",value=sw_input_site[i_site, ],envir=exeEnv)
#			assign(x="i_sw_input_soils",value=sw_input_soils[i_site, ],envir=exeEnv)
#			assign(x="i_sw_input_weather",value=sw_input_weather[i_site, ],envir=exeEnv)
#			assign(x="i_sw_input_climscen",value=sw_input_climscen[i_site, ],envir=exeEnv)
#			assign(x="i_sw_input_climscen_values",value=sw_input_climscen_values[i_site, ],envir=exeEnv)
#
#			save(list=ls(exeEnv),file="test.Rdata", envir=exeEnv)
#			rm(list=ls(all=TRUE))
#			load("test.Rdata")
#
#			do_OneSite(i_sim=i_sim, i_labels=i_labels, i_SWRunInformation=i_SWRunInformation, i_sw_input_soillayers=i_sw_input_soillayers,
#							i_sw_input_treatments=i_sw_input_treatments, i_sw_input_cloud=i_sw_input_cloud, i_sw_input_prod=i_sw_input_prod, i_sw_input_site=i_sw_input_site, i_sw_input_soils=i_sw_input_soils,
#							i_sw_input_weather=i_sw_input_weather, i_sw_input_climscen=i_sw_input_climscen, i_sw_input_climscen_values=i_sw_input_climscen_values)
#			runs.completed <- runs.completed + do_OneSite(i_sim=i_sim, i_labels=labels[i_site], i_SWRunInformation=SWRunInformation[i_site, ], i_sw_input_soillayers=sw_input_soillayers[i_site, ], i_sw_input_treatments=sw_input_treatments[i_site, ], i_sw_input_cloud=sw_input_cloud[i_site, ], i_sw_input_prod=sw_input_prod[i_site, ], i_sw_input_site=sw_input_site[i_site, ], i_sw_input_soils=sw_input_soils[i_site, ], i_sw_input_weather=sw_input_weather[i_site, ], i_sw_input_climscen=sw_input_climscen[i_site, ], i_sw_input_climscen_values=sw_input_climscen_values[i_site, ])
#		}
	}
	#save(inputDataToSave,file=file.path(dir.out,paste("swInputData_",head(runIDs_todo,n=1),"_",head(runIDs_todo,n=1)+runs.completed,".R",sep="")),compress=TRUE)
	if(!be.quiet) print(paste("SWSF simulation runs: completed with", runs.completed, "runs: ended after",  round(difftime(Sys.time(), t1, units="secs"), 2), "s"))
} else {
	runs.completed <- 0
}
#------------------------

#------------------------
if (any(actions == "concatenate")) {
	t1 <- Sys.time()
	if(!be.quiet) print(paste("Inserting Data from Temp SQL files into Database", ": started at", t1))

	temp <- as.double(difftime(t1, t.overall, units = "secs"))

	if (temp <= (MinTimeConcat - 36000) || !parallel_runs || !identical(parallel_backend, "mpi")) {#need at least 10 hours for anything useful
		#Connect to the Database
		con <- DBI::dbConnect(RSQLite::SQLite(), dbname = name.OutputDB)

		do_DBCurrent <- copyCurrentConditionsFromTempSQL || copyCurrentConditionsFromDatabase
		reset_DBCurrent <- copyCurrentConditionsFromTempSQL && (cleanDB || !file.exists(name.OutputDBCurrent))
		if (reset_DBCurrent) file.copy(from = name.OutputDB, to = name.OutputDBCurrent)
		if (do_DBCurrent) con2 <- DBI::dbConnect(RSQLite::SQLite(), dbname = name.OutputDBCurrent)
		if (reset_DBCurrent) dbGetQuery(con2, "DELETE FROM runs WHERE scenario_id != 1;") # DROP ALL ROWS THAT ARE NOT CURRENT FROM HEADER
		out_tables <- DBI::dbListTables(con)
		out_tables_aggr <- grep("aggregation_", out_tables, value = TRUE)

		# Prepare output databases
		set_PRAGMAs(con, PRAGMA_settings1)
		if (do_DBCurrent) set_PRAGMAs(con2, PRAGMA_settings1)

		# Locate temporary SQL files
		theFileList <- list.files(path = dir.out.temp, pattern = "SQL",
			full.names = FALSE, recursive = TRUE, include.dirs = FALSE, ignore.case = FALSE)
		completedFiles <- if (file.exists(file.path(dir.out.temp,concatFile))) {
				basename(readLines(file.path(dir.out.temp, concatFile)))
			} else {
				character(0)
			}
		if (any(theFileList %in% completedFiles)) {
			theFileList <- theFileList[-which(theFileList %in% completedFiles)]#remove any already inserted files from list
		}

		# Add data to SQL databases
		for	(j in seq_along(theFileList)) {
			tDB1 <- Sys.time()
			if (print.debug) {
				print(paste(j,": started at ", tDB1, sep = ""))
			}

			tDB <- as.double(difftime(tDB1, t.overall, units = "secs"))
			if (tDB > (MaxRunDurationTime - MaxConcatTime) && parallel_runs && identical(parallel_backend, "mpi")) {#figure need at least 8 minutes for big ones  ( not run in parallel
				break
			}

			OK <- TRUE
			# Read SQL statements from temporary file
			sql_cmds <- readLines(file.path(dir.out.temp, theFileList[j]))
			add_to_DBCurrent <- copyCurrentConditionsFromTempSQL && grepl("SQL_Current", theFileList[j])

			# Check what has already been inserted to the tables
# NOTE(drs): assuming that every table has the same set of P_id inserted!
			pids_inserted <- DBI::dbGetQuery(con, paste0("SELECT P_id FROM ", out_tables_aggr[1], ";"))[, 1]
			if (add_to_DBCurrent)
				pids2_inserted <- DBI::dbGetQuery(con2, paste0("SELECT P_id FROM ", out_tables_aggr[1], ";"))[, 1]

			# Send SQL statements to database
			OK <- OK && DBI::dbBegin(con)
			if (add_to_DBCurrent) OK <- OK && DBI::dbBegin(con2)

# NOTE(drs): 'concatenation' may be much faster if temporary text files are not constructed
# around SQL insert statements, but instead as data.frames. Data.frames could be much faster
# checked for duplicate P_id entries and could be inserted all at once with RSQLite::dbWriteTable()
			for (k in seq_along(sql_cmds)) {
				# Determine P_id
				id_start <- as.integer(regexpr(" VALUES (", sql_cmds[k], fixed = TRUE))
				id_end <- as.integer(regexpr(",", sql_cmds[k], fixed = TRUE))
				if (id_end < 0)
					id_end <- as.integer(regexpr(")", sql_cmds[k], fixed = TRUE))

				if (any(id_start < 1, id_end <= id_start)) {
					print(paste0("P_id not found in file ", sQuote(theFileList[j]), " on line ", k, ": ", substr(sql_cmds[k], 1, 100)))
					next()
				}

				id <- as.integer(substr(sql_cmds[k], 9 + id_start, -1 + id_end))

# NOTE(drs): assuming duplicate pids do not show up in the same temporary file
				do_insert <- !(id %in% pids_inserted)
				do_insert2 <- if (add_to_DBCurrent) !(id %in% pids2_inserted) else FALSE

				if (do_insert) {
					res <- try(DBI::dbSendQuery(con, sql_cmds[k]))
					OK <- OK && !inherits(res, "try-error")
				}
				if (do_insert2) {
					res <- try(DBI::dbSendQuery(con2, sql_cmds[k]))
					OK <- OK && !inherits(res, "try-error")
				}

				if (!OK) break
			}

			if (OK) {
				OK <- OK && DBI::dbCommit(con)
				if (add_to_DBCurrent) OK <- OK && DBI::dbCommit(con2)
			} else {
				DBI::dbRollback(con)
				if (add_to_DBCurrent) DBI::dbRollback(con2)
			}

			# Clean up and report
			if (OK) {
				cat(file.path(dir.out.temp, theFileList[j]),
				    file = file.path(dir.out.temp,concatFile), append = TRUE, sep = "\n")
				if (deleteTmpSQLFiles)
					try(file.remove(file.path(dir.out.temp, theFileList[j])), silent = TRUE)
			}
			if (print.debug) {
				tDB <- round(difftime(Sys.time(), tDB1, units = "secs"), 2)
				print(paste("    ended at", Sys.time(), "after", tDB, "s"))
			}
		}

		if (!be.quiet) print(paste("Database complete in :",  round(difftime(Sys.time(), t1, units="secs"), 2), "s"))

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

			con <- DBI::dbConnect(RSQLite::SQLite(), name.OutputDBCurrent)
			for(i in 1:length(sqlTables)) {#Create the tables
				res<-dbSendQuery(con, sqlTables[i])
				dbClearResult(res)
			}
			dbGetQuery(con, sqlView)

			con <- DBI::dbConnect(RSQLite::SQLite(), dbname = name.OutputDB)
			#Get Tables minus ones we do not want
			Tables <- dbListTables(con)
			Tables <- Tables[-grep(pattern="sqlite_sequence",Tables)]
			Tables <- Tables[-(which(Tables %in% headerTables))]

			writeLines(text=paste(".mode insert ", Tables, "\n.out ", Tables,".sql\nSELECT * FROM ",Tables," WHERE P_id IN (SELECT P_id FROM runs WHERE scenario_id = 1 ORDER BY P_id);",sep=""),con="dump.txt")
			lines <- c("PRAGMA cache_size = 400000;","PRAGMA synchronous = 1;","PRAGMA locking_mode = EXCLUSIVE;","PRAGMA temp_store = MEMORY;","PRAGMA auto_vacuum = NONE;")
			writeLines(text=c(lines,paste(".read ",Tables,".sql",sep="")),con="insert.txt")

			system(paste("cat dump.txt | sqlite3 ", shQuote(name.OutputDB)))
			system(paste("cat insert.txt | sqlite3 ", shQuote(name.OutputDBCurrent)))

			unlink(paste(Tables,".sql",sep=""))

			Tables <- dbListTables(con)
			Tables <- Tables[-grep(pattern="sqlite_sequence",Tables)]
			Tables <- Tables[(which(Tables %in% headerTables[-1]))]

			writeLines(text=paste(".mode insert ", Tables, "\n.out ", Tables,".sql\nSELECT * FROM ",Tables,";",sep=""),con="dump.txt")
			lines <- c("PRAGMA cache_size = 400000;","PRAGMA synchronous = 1;","PRAGMA locking_mode = EXCLUSIVE;","PRAGMA temp_store = MEMORY;","PRAGMA auto_vacuum = NONE;")
			writeLines(text=c(lines,paste(".read ",Tables,".sql",sep="")),con="insert.txt")

			system(paste("cat dump.txt | sqlite3 ", shQuote(name.OutputDB)))
			system(paste("cat insert.txt | sqlite3 ", shQuote(name.OutputDBCurrent)))

			unlink(paste(Tables,".sql",sep=""))
			unlink(c("dump.txt","insert.txt"))
		}

		DBI::dbDisconnect(con)
		if (do_DBCurrent) DBI::dbDisconnect(con2)
	} else {
		print(paste("Need more than ", MinTimeConcat," seconds to put SQL in Database.",sep=""))
	}
}


#--------------------------------------------------------------------------------------------------#
#------------------------CHECK COMPLETENESS OF OUTPUT FILES AND SIMULATIONS
t.check <- Sys.time()

if(checkCompleteness){

	if(!be.quiet) print(paste("SWSF checks simulations and output: started at", t.check))
	headerTables <- c("runs","sqlite_sequence","header","run_labels","scenario_labels","sites","experimental_labels","treatments","simulation_years","weatherfolders")

	checkForMissing <- function(Table, database){
		con <- DBI::dbConnect(RSQLite::SQLite(), dbname = database)

		sql <- paste("SELECT runs.P_id FROM runs LEFT JOIN ",Table," ON (runs.P_id=",Table,".P_id) WHERE ",Table,".P_id is NULL ORDER BY runs.P_id",sep="")

		mP_ids <- dbGetQuery(con, sql)$P_id

		if(length(mP_ids) > 0)
			save(mP_ids,file=paste(database,"_",Table,"_missing_P_ids.RData"))

		return(length(mP_ids))
	}

	library(RSQLite,quietly = TRUE)
	con <- DBI::dbConnect(RSQLite::SQLite(), dbname = name.OutputDB)

	Tables <- dbListTables(con) #get a list of tables
	Tables <- Tables[-which(Tables %in% headerTables)]


	if(parallel_runs && parallel_init){
		#call the simulations depending on parallel backend
		if(identical(parallel_backend, "mpi")) {
			mpi.bcast.cmd(library(RSQLite,quietly = TRUE))

			numberMissing <- mpi.applyLB(x=Tables, fun=checkForMissing, database=name.OutputDB)
			TotalMissing <- sum(unlist(numberMissing))

			print(paste("dbTables.sqlite3 is missing : ",TotalMissing,"",sep=""))

			numberMissing <- mpi.applyLB(x=Tables, fun=checkForMissing, database=name.OutputDBCurrent)
			TotalMissing <- sum(unlist(numberMissing))

			print(paste("dbTables_current.sqlite3 is missing : ",TotalMissing,"",sep=""))
		} else if(identical(parallel_backend, "snow")) {
			snow::clusterEvalQ(cl, library(RSQLite,quietly = TRUE))

			numberMissing <- foreach(i = 1:length(Tables), .combine="+", .inorder=FALSE) %dopar% checkForMissing(Tables[i], database=name.OutputDB)
			print(paste("dbTables.sqlite3 is missing : ",numberMissing,"",sep=""))
			numberMissing <- foreach(i = 1:length(Tables), .combine="+", .inorder=FALSE) %dopar% checkForMissing(Tables[i], database=name.OutputDBCurrent)
			print(paste("dbTables_current.sqlite3 is missing : ",numberMissing,"",sep=""))
		}
	} else {
		numberMissing <- foreach(table=Tables, .combine="+", .inorder=FALSE) %do% {
			checkForMissing(table, database=name.OutputDB)
		}
		print(paste("dbTables.sqlite3 is missing : ",numberMissing,"",sep=""))

		numberMissing <- foreach(table=Tables, .combine="+", .inorder=FALSE) %do% {
			checkForMissing(table, database=name.OutputDBCurrent)
		}
		print(paste("dbTables.sqlite3 is missing : ",numberMissing,"",sep=""))
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

if(do.ensembles && all.complete && (actionWithSoilWat && runs.completed == runsN_todo || actionWithSWSFOutput && !actionWithSoilWat) ){

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
		con <- DBI::dbConnect(RSQLite::SQLite(), dbname = name.OutputDB)
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
			dbBegin(conn=conEnsembleDB)
			dbGetPreparedQuery(conEnsembleDB, paste("INSERT INTO ",outfile," VALUES (",paste(paste(":",colnames(dat),sep=""),collapse=", "),");",sep=""), bind.data=dat)
			dbCommit(conn=conEnsembleDB)
			written<-1
			#written <- dbWriteTable(conEnsembleDB, name=outfile, dat, row.names=FALSE,append=TRUE)#
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
			sqlString <- paste("SELECT '",Table,"'.P_id AS P_id, header.Scenario AS Scenario, ",columns," FROM '", Table, "' INNER JOIN header ON '",Table,"'.P_id=header.P_id WHERE header.P_id BETWEEN ",start," AND ",stop," AND header.Scenario LIKE '%", tolower(ensemble.family), "%'", " ORDER BY P_id;", sep="")
			res <- dbSendQuery(con, sqlString)
			dataScen.Mean <- fetch(res, n=-1) #dataToQuantilize get the data from the query n=-1 to get all rows
			dbClearResult(res)

			columnCutoff <- match("Scenario", colnames(dataScen.Mean))
			if(export.header) {
				sqlString <- paste("SELECT '", Table,"'.P_id AS P_id ",if(Layers) ", Soil_Layer ","FROM '",Table,"',header WHERE '",Table,"'.P_id=header.P_id AND header.P_id BETWEEN ",start," AND ",stop," AND header.Scenario = 'Current' ORDER BY P_id;",sep="")
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
			conEnsembleDB <- DBI::dbConnect(RSQLite::SQLite(), dbname=tfile)

			nfiles <- 0
			#Grab x rows at a time
			SQL <- paste("SELECT MAX(P_id) FROM '",Table,"';",sep="")
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
				EnsembleFamilyLevelTables<-paste(ensemble.family,"_rank_",formatC(ensemble.levels, width=2, flag="0"),"_",c("means","sds",if(save.scenario.ranks) "scenarioranks"),sep="")
				LastPid <- integer(length=length(EnsembleFamilyLevelTables))
				for(i in 1:length(LastPid)) {
					SQL <- paste("SELECT MAX(P_id) FROM '",EnsembleFamilyLevelTables[i],"';",sep="")
					LastPid[i] <- as.integer(dbGetQuery(conEnsembleDB,SQL))+(scenario_No-1)#Need to add all the scenarios because last P_id will always be Current
				}
				if(any(is.na(LastPid))) { #If any of the tables are empty we need to start at the beginning
					minRun_id <- 1
				} else {
					minRun_id <- (min(LastPid)/scenario_No)+1 #This is already done so we add one
				}
				#########################
				for(i in seq(minRun_id,maxRun_id,ensembleCollectSize)) {
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
						ntemp <- ntemp + doWrite(dat=dataEns.Mean[k,,], headerInfo=dataScen.Mean$headerInfo, elevel=ensemble.levels[k], outfile=paste("'",outputs[1],"'",sep=""))
						if(length(dim(dataEns.Mean[(length(ensemble.levels) + 1):(2*length(ensemble.levels)),,])) == 2) {
							ntemp <- ntemp + doWrite(dat=dataEns.SD[k,], headerInfo=dataScen.Mean$headerInfo, elevel=ensemble.levels[k], outfile=paste("'",outputs[2],"'",sep=""))
						} else {
							ntemp <- ntemp + doWrite(dat=dataEns.SD[k,,], headerInfo=dataScen.Mean$headerInfo, elevel=ensemble.levels[k], outfile=paste("'",outputs[2],"'",sep=""))
						}
						if(save.scenario.ranks) ntemp <- ntemp + doWrite(dat=dataEns.Mean[length(ensemble.levels) + k,,], headerInfo=dataScen.Mean$headerInfo, elevel=ensemble.levels[k], outfile=paste("'",outputs[3],"'",sep=""))
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
	con <- DBI::dbConnect(RSQLite::SQLite(), dbname = name.OutputDB)

	Tables <- dbListTables(con) #get a list of tables
	Tables <- Tables[-which(Tables %in% headerTables)]
	Tables <- Tables[-grep(pattern="_sd", Tables, ignore.case = T)]

	if(parallel_runs && parallel_init){
		#call the simulations depending on parallel backend
		list.export <- c("ensembleCollectSize","Tables","save.scenario.ranks","ensemble.levels","calc.ensembles","scenario_No","MaxRunDurationTime", "collect_EnsembleFromScenarios","dir.out","ensemble.families","t.overall","parallel_runs","parallel_backend","name.OutputDB")
		if(identical(parallel_backend, "mpi")) {
			export_objects_to_workers(list.export, list(global = .GlobalEnv), "mpi")

			mpi.bcast.cmd(library(RSQLite,quietly = TRUE))

			ensembles.completed <- mpi.applyLB(x=Tables, fun=collect_EnsembleFromScenarios)
			ensembles.completed <- sum(unlist(ensembles.completed))
		} else if(identical(parallel_backend, "snow")) {
			export_obj_local <- list.export[list.export %in% ls(name=environment())]
			export_obj_in_parent <- list.export[list.export %in% ls(name=parent.frame())]
			export_obj_in_parent <- export_obj_in_parent[!(export_obj_in_parent %in% export_obj_local)]
			export_obj_in_globenv <- list.export[list.export %in% ls(name=.GlobalEnv)]
			export_obj_in_globenv <- export_obj_in_globenv[!(export_obj_in_globenv %in% c(export_obj_local, export_obj_in_parent))]
			stopifnot(c(export_obj_local, export_obj_in_parent, export_obj_in_globenv) %in% list.export)

			if(length(export_obj_local) > 0) snow::clusterExport(cl, export_obj_local, envir=environment())
			if(length(export_obj_in_parent) > 0) snow::clusterExport(cl, export_obj_in_parent, envir=parent.frame())
			if(length(export_obj_in_globenv) > 0) snow::clusterExport(cl, export_obj_in_globenv, envir=.GlobalEnv)

			snow::clusterEvalQ(cl, library(RSQLite,quietly = TRUE))

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
delta.overall <- difftime(Sys.time(), t.overall, units = "secs")
if (!be.quiet)
 print(paste("SWSF: ended after", round(delta.overall, 2), "s"))

write.timer <- function(label, time_sec = "", number = "") {
  cat(paste(label, time_sec, number, sep = ","), file = file.path(dir.out, timerfile2),
    append = TRUE, sep = "\n")
}

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
