#--------------------------------------------------------------------------------------------------#
#------------------------OBTAIN INFORMATION FROM EXTERNAL DATASETS PRIOR TO SIMULATION RUNS TO CREATE THEM

#------Load required packages
if(exinfo$ExtractSkyDataFromNOAAClimateAtlas_USA){
	if(!require(rgdal, quietly=TRUE)) {
		tryCatch(install.packages("rgdal", repos=url.Rrepos, lib=dir.libraries), warning=function(w) { print(w); print("rgdal failed to install"); stop("Stopping") })
		stopifnot(require(rgdal, quietly=TRUE))
	}
}

if(	exinfo$ExtractClimateChangeScenarios_CMIP3_ClimateWizardEnsembles_Global ||
	exinfo$ExtractClimateChangeScenarios_CMIP3_ClimateWizardEnsembles_USA ||
	exinfo$ExtractSoilDataFromCONUSSOILFromSTATSGO_USA ||
	exinfo$ExtractElevation_NED_USA ||
	exinfo$ExtractElevation_HWSD_Global){

	if(!require(raster, quietly=TRUE)) {
		tryCatch(install.packages("raster", repos=url.Rrepos, lib=dir.libraries), warning=function(w) { print(w); print("raster failed to install"); stop("Stopping") })
		stopifnot(require(raster, quietly=TRUE))
	}
}

if(	exinfo$ExtractClimateChangeScenarios_CMIP3_BCSD_GDODCPUCLLNL_USA ||
	exinfo$ExtractClimateChangeScenarios_CMIP3_BCSD_GDODCPUCLLNL_Global ||
	exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_GDODCPUCLLNL_USA ||
	exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_GDODCPUCLLNL_Global){
	
	if(!require(ncdf4, quietly=TRUE)) {
		tryCatch(install.packages("ncdf4", repos=url.Rrepos, lib=dir.libraries), warning=function(w) { print(w); print("ncdf4 failed to install"); stop("Stopping") })
		stopifnot(require(ncdf4, quietly=TRUE))
	}
}



#--------------------------------------------------------------------------------------------------#

if(exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_NEX_USA){
	if(!be.quiet) print(paste("Started 'ExtractClimateChangeScenarios_CMIP5_BCSD_NEX_USA' at", Sys.time()))
	
	temp <- unlist(strsplit(climate.conditions[!grepl(climate.ambient, climate.conditions)], split=".", fixed=TRUE))
	
	if(!is.null(temp) && sum(include_YN) * length(temp) < 5000){
		reqGets <- matrix(temp, ncol=2, byrow=TRUE)
		reqGets[, 1] <- tolower(reqGets[, 1])
		
		##https://portal.nccs.nasa.gov/portal_home/published/NEX.html
		gcmsNEX <- c("inmcm4", "bcc-csm1-1", "bcc-csm1-1-m", "NorESM1-M", "MRI-CGCM3", "MPI-ESM-MR", "MPI-ESM-LR", "MIROC5", "MIROC-ESM", "MIROC-ESM-CHEM", "IPSL-CM5B-LR", "IPSL-CM5A-MR", "IPSL-CM5A-LR", "HadGEM2-ES", "HadGEM2-CC", "HadGEM2-AO", " GISS-E2-R", "GFDL-ESM2M", "GFDL-ESM2G", "GFDL-CM3", "FIO-ESM", "FGOALS-g2", "CanESM2", "CSIRO-Mk3-6-0", "CNRM-CM5", "CMCC-CM", "CESM1-CAM5", "CESM1-BGC", "CCSM4", "BNU-ESM", "ACCESS1-0")
		scenariosNEX <- c("historical", "rcp26", "rcp45", "rcp60", "rcp85")
		
		if(all(reqGets[, 1] %in% scenariosNEX) && all(reqGets[, 2] %in% gcmsNEX)){
			#locations of simulation runs
			locations <- with(SWRunInformation[include_YN > 0, ], data.frame(X_WGS84, Y_WGS84))
			
			#put requests together
			requestN <- nrow(reqGets) * nrow(locations)
			if(!be.quiet) print(paste("'ExtractClimateChangeScenarios_CMIP5_BCSD_NEX_USA' will run", requestN, "times"))
			
			startNEX <- max(2006, deltaFutureToSimStart_yr + simstartyr)
			endNEX <- min(2099, deltaFutureToSimStart_yr + endyr)
			
			#function communicating with NEX
			url.nex.ncss <- "http://dataserver.nccs.nasa.gov/thredds/ncss/grid/bypass/NEX-DCP30"
			downscaling <- "bcsd"
			gcmrun <- "r1i1p1"
			variables <- c("pr", "tasmin", "tasmax") #units c("kg/m2/s", "K", "K") --> SoilWat required units c("cm/day", "C", "C")
		
			get.NEX <- function(i, startyear=NULL, endyear=NULL){
				scen <- reqGets[ig <- (i - 1) %% nrow(reqGets) + 1, 1]
				gcm <- reqGets[ig, 2]
				lon <- locations[il <- (i - 1) %/% nrow(reqGets) + 1, 1]
				lat <- locations[il, 2]
				if(!be.quiet) print(paste(i, "th extraction of NEX at", Sys.time(), "for", gcm, scen, "at", lon, lat))

				mmPerSecond_to_cmPerMonth <- function(prcp_mmPerSecond, yearStart, yearEnd){
					DaysPerMonths <- rle(as.POSIXlt(seq(from=as.POSIXlt(paste0(yearStart, "-01-01")), to=as.POSIXlt(paste0(yearEnd, "-12-31")), by="1 day"))$mon)$lengths			
					return(prcp_mmPerSecond / 10 * DaysPerMonths * 24 * 60 * 60)
				}
				
				if(is.null(startyear)) startyear <- ifelse(identical(scen, "historical"), 1950, 2006)
				if(is.null(endyear)) endyear <- ifelse(identical(scen, "historical"), 2005, 2099)
				
				get.NEXvariable <- function(var, scen, gcm, lon, lat){
					request <- paste0(paste(url.nex.ncss, downscaling, scen, gcmrun,
									paste0(gcm, "_", var, ".ncml"), sep="/"), "?var=", paste0(gcm, "_", var), 
							"&latitude=", lat, "&longitude=", ifelse(lon > 180, lon - 360, lon),
							paste0("&time_start=", startyear, "-01-16T12%3A00%3A00Z&time_end=", endyear, "-12-16T12%3A00%3A00Z&timeStride=1"),
							"&accept=csv")
					
					success <- try(download.file(url=request, destfile=ftemp <- file.path(dir.out.temp, paste0("NEX_", gcm, "_", scen, "_", var, "_", round(lat, 5), "&", round(lon, 5), ".csv")), quiet=TRUE), silent=TRUE)
					
					if(!inherits(success, "try-error") && success == 0){
						dat <- read.csv(ftemp, colClasses=c("NULL", "NULL", "NULL", "numeric"))[, 1] #colnames = Time, Lat, Long, Variable
					} else {
						dat <- rep(NA, times=12)
					}
					if(file.exists(ftemp)) unlink(ftemp)
					
					return(dat)
				}
				
				res <- matrix(NA, nrow=1, ncol=2 + 3*12, dimnames=list(paste(sapply(list(scen, gcm), as.character), collapse="."), NULL))
				res[1, 1:2] <- c(lon, lat)
				for(iv in seq_along(variables)){
					temp <- get.NEXvariable(var=variables[iv], scen=scen, gcm=gcm, lon=lon, lat=lat)
					if(variables[iv] == "pr"){
						temp <- mmPerSecond_to_cmPerMonth(temp, yearStart=startyear, yearEnd=endyear) #convert kg/m2/s -> cm/month
					} else if(grepl("tas", variables[iv])){
						temp <- temp - 273.15	#convert K -> C
					}
					res[2 + (iv - 1)*12 + 1:12] <- aggregate(temp, by=list(rep(1:12, times=length(temp) %/% 12)), FUN=mean)$x
				}
				
				return(res)
			}
			
			#get data from NEX
			if(parallel_runs && parallel_init){
				#call the simulations depending on parallel backend
				list.export <- c("get.NEX", "reqGets", "locations", "url.nex.ncss", "downscaling", "gcmrun", "variables", "dir.out.temp", "print.debug")
				if(identical(parallel_backend, "mpi")) {
					exportObjects(list.export)
					
					res <- mpi.applyLB(x=1:requestN, fun=get.NEX, startyear=startNEX, endyear=endNEX)

					mpi.bcast.cmd(rm(list=ls(all=TRUE)))
					mpi.bcast.cmd(gc())

					temp <- sapply(res, FUN=rownames)
					res <- matrix(unlist(res), ncol=2 + 3*12, dimnames=list(temp, NULL))
				} else if(identical(parallel_backend, "snow")) {
					snow::clusterExport(cl, list.export)
					
					res <- foreach(i=1:requestN, .combine="rbind", .inorder=FALSE) %dopar% get.NEX(i=i, startyear=startNEX, endyear=endNEX)

					snow::clusterEvalQ(cl, rm(list=ls(all=TRUE)))
					snow::clusterEvalQ(cl, gc())
				} else if(identical(parallel_backend, "multicore")) {
					res <- foreach(i=1:requestN, .combine="rbind", .inorder=FALSE) %dopar% get.NEX(i=i, startyear=startNEX, endyear=endNEX)
				}
			} else {
					res <- foreach(i=1:requestN, .combine="rbind", .inorder=FALSE) %do% get.NEX(i=i, startyear=startNEX, endyear=endNEX)
			}
			save(res, file=file.path(dir.sw.dat, "extractionNEX.RData"))

			#prepare data for SoilWat wrapper format
			#reshape from long to wide format for climate conditions sorted as in 'climate.conditions'
			i_climCond <- match(rownames(res), climate.conditions[!grepl(climate.ambient, climate.conditions)])
			res <- cbind(i_climCond, res)
			dimnames(res) <- list(NULL, c("CC", "Lon", "Lat", paste0(rep(c("pr", "tmin", "tmax"), each=12), rep(1:12, times=2))))
			res <- reshape(data=data.frame(res), timevar="CC", idvar=c("Lon","Lat"), direction="wide")
			
			#sort sites
			idLocs <- apply(locations, 1, paste0, collapse="_")
			idLocRes <- apply(res[, 1:2], 1, paste0, collapse="_")
			res <- res[match(idLocRes, idLocs, nomatch=0), ]
			
			if((temp <- nrow(res) - sum(complete.cases(res))) > 0){
				warning(paste(temp, "sites didn't extract climate scenario information by 'ExtractClimateChangeScenarios_CMIP5_BCSD_NEX_USA'"))
				failedLocations_NEX <- locations[temp <- which(apply(res, 1, FUN=function(x) any(is.na(x)))), ]
				include_YN_updateFailed <- include_YN
				include_YN_updateFailed[include_YN > 0][temp] <- 0
				save(failedLocations_NEX, include_YN_updateFailed, file=file.path(dir.in, "failedLocations_NEX.RData"))
				SWRunInformation_updateFailed <- cbind(SWRunInformation, include_YN_updateFailed=include_YN_updateFailed)
				write.csv(SWRunInformation_updateFailed, file=file.path(dir.in, paste0("failedLocationsUpdated_NEX_", datafile.SWRunInformation)))
			}
			
			#add data to sw_input_climscen and set the use flags
			icols <- 1 + 12 * 3 + 1:(nrow(reqGets) * 12 * 3)
			sw_input_climscen_values_use[icols] <- 1
			sw_input_climscen_values[include_YN > 0, icols] <- res[, -(1:2)]
			
			#write data to datafile.climatescenarios_values
			write.csv(rbind(sw_input_climscen_values_use, sw_input_climscen_values), file=file.path(dir.sw.dat, datafile.climatescenarios_values), row.names=FALSE)
			
			rm(reqGets, icols, res, idLocs, idLocRes, locations, i_climCond, get.NEX, url.nex.ncss, downscaling, gcmrun, variables, requestN, startNEX, endNEX)
		
			#make sure no lingering temp files are left on the hard drive
			if(length(flist <- list.files(dir.out.temp, pattern="NEX_", full.names=TRUE)) > 0) sapply(flist, unlink)
		} else {
			warning("Not all requested RCPs and/or GCMs requested are available in 'ExtractClimateChangeScenarios_CMIP5_BCSD_NEX_USA'")
		}
		rm(gcmsNEX, scenariosNEX)
	} else if(sum(include_YN) * length(temp) >= 5000){
		warning(paste0("This implementation of 'ExtractClimateChangeScenarios_CMIP5_BCSD_NEX_USA' gets data for single locations; for many locations x climate conditions (here, ", sum(include_YN) * length(), "), a different approach downloading a spatial bounding box may be more appropriate"))
	}

	if(!be.quiet) print(paste("Finished 'ExtractClimateChangeScenarios_CMIP5_BCSD_NEX_USA' at", Sys.time()))
}


if(	exinfo$ExtractClimateChangeScenarios_CMIP3_BCSD_GDODCPUCLLNL_USA ||
	exinfo$ExtractClimateChangeScenarios_CMIP3_BCSD_GDODCPUCLLNL_Global ||
	exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_GDODCPUCLLNL_USA ||
	exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_GDODCPUCLLNL_Global){

	if(!be.quiet) print(paste("Started 'ExtractClimateChangeScenarios/BCSD_GDODCPUCLLNL' at", Sys.time()))

	temp <- unlist(strsplit(climate.conditions[!grepl(climate.ambient, climate.conditions)], split=".", fixed=TRUE))
	
	if(!is.null(temp)){
		reqGets <- matrix(temp, ncol=2, byrow=TRUE)
		
		##gdo-dcp.ucllnl.org/downscaled_cmip_projections
		dir.ex.dat <- file.path(dir.external, "Extract_GDO_DCP_UCLLNL_DownscaledClimateData")
		if(exinfo$ExtractClimateChangeScenarios_CMIP3_BCSD_GDODCPUCLLNL_USA) dir.ex.dat <- file.path(dir.ex.dat, "CMIP3_BCSD", "CONUS_0.125degree")
		if(exinfo$ExtractClimateChangeScenarios_CMIP3_BCSD_GDODCPUCLLNL_Global) dir.ex.dat <- file.path(dir.ex.dat, "CMIP3_BCSD", "Global_0.5degree_MaurerEd")
		if(exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_GDODCPUCLLNL_USA) dir.ex.dat <- file.path(dir.ex.dat, "CMIP5_BCSD", "CONUS_0.125degree_r1i1p1")
		if(exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_GDODCPUCLLNL_Global) dir.ex.dat <- file.path(dir.ex.dat, "CMIP5_BCSD", "Global_0.5degree_r1i1p1")
		
		scenariosGDODCPUCLLNL <- list.dirs(dir.ex.dat, full.names=FALSE, recursive=FALSE)
		if(any((temp <- sapply(scenariosGDODCPUCLLNL, FUN=function(x) length(list.files(file.path(dir.ex.dat, x))))) == 0)) scenariosGDODCPUCLLNL <- scenariosGDODCPUCLLNL[temp > 0]
		
		gcmsGDODCPUCLLNL <- unique(unlist(sapply(scenariosGDODCPUCLLNL, FUN=function(x) sapply(strsplit(list.files(file.path(dir.ex.dat, x)), split="_", fixed=TRUE), FUN=function(x) x[5]))))
		
		if(all(reqGets[, 1] %in% scenariosGDODCPUCLLNL) && all(sapply(1:nrow(reqGets), FUN=function(i) reqGets[i, 2] %in% gcmsGDODCPUCLLNL[, reqGets[i, 1]]))){
			#put requests together
			locations <- with(SWRunInformation[include_YN > 0, ], data.frame(X_WGS84, Y_WGS84))	#locations of simulation runs
			requestN <- nrow(reqGets) * nrow(locations)
			if(!be.quiet) print(paste("'ExtractClimateChangeScenarios/BCSD_GDODCPUCLLNL' will run", requestN, "times"))
			
			startGDODCPUCLLNL <- max(2006, deltaFutureToSimStart_yr + simstartyr)
			endGDODCPUCLLNL <- min(2099, deltaFutureToSimStart_yr + endyr)
			
			if(exinfo$ExtractClimateChangeScenarios_CMIP3_BCSD_GDODCPUCLLNL_USA || exinfo$ExtractClimateChangeScenarios_CMIP3_BCSD_GDODCPUCLLNL_Global){
				fileVarTags <- c("monthly.Prcp", "monthly.Tavg", "monthly.Tmin", "monthly.Tmax")
				varTags <- c("Prcp", "Tavg", "Tmin", "Tmax")
			}
			if(exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_GDODCPUCLLNL_USA || exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_GDODCPUCLLNL_Global){
				fileVarTags <- c("_pr_", "_tas_", "_tasmin_", "_tasmax_")
				varTags <- c("pr", "tas", "tasmin", "tasmax")
			}
			
			#functions
			whereNearest <- function(val, matrix) {
				#this returns the index of the closest value in the matrix to the passed in value.
				dist <- abs(matrix-val)
				index <- which.min(dist)
				return (index)
			}
			
			nc_getByCoords <- function(nc, varid, lats, lons, lat, lon, timeStartIndex, timeCount){ 
				#this function gets values from the netCDF files, it is specially written so that it can be passed a lat/lon value instead of the lat/lon indices the function in the ncdf4 library is looking for
				ix <- whereNearest(val=lon, matrix=lons)
				iy <- whereNearest(val=lat, matrix=lats)
				return(ncvar_get(nc, varid, start=c(ix, iy, timeStartIndex), count=c(1, 1, timeCount)))
			} 
			
			mmPerDay_to_cmPerMonth <- function(prcp_mmPerDay, yearStart, yearEnd){
				prcp <- prcp_mmPerDay / 10	# mm -> cm
				DaysPerMonths <- rle(as.POSIXlt(seq(from=as.POSIXlt(paste0(yearStart, "-01-01")), to=as.POSIXlt(paste0(yearEnd, "-12-31")), by="1 day"))$mon)$lengths
				prcp <- prcp * DaysPerMonths
				
				return(prcp)
			}
			
			get.TimeIndices <- function(nc, startyear, endyear){
				utemp <- nc$dim$time$units
				N <- length(temp <- nc$dim$time$vals)
				firstMonth <- temp[1]
				lastMonth <- temp[N]
				baseYear <- (temp <- lapply(strsplit(utemp, split=" ", fixed=TRUE)[[1]], FUN=function(x) as.Date(x, format="%Y-%m-%d")))[sapply(temp, FUN=function(x) !is.na(x))][[1]]
				stopifnot(length(baseYear) == 1)
				
				startYear <- as.POSIXlt(baseYear + firstMonth)$year + 1900
				startMonth <- as.POSIXlt(baseYear + firstMonth)$mon + 1
				endYear <- as.POSIXlt(baseYear + lastMonth)$year + 1900
				endMonth <- as.POSIXlt(baseYear + lastMonth)$mon + 1
				
				stopifnot(startYear < startyear || (startMonth == 1 && startYear == startyear)) 	#we only extract full years
				timeStartIndex <- ((startyear - startYear - 1) * 12) + (12 - startMonth + 2)
				
				stopifnot(endYear >= endyear)	#account for missing months in last year, but don't allow missing years; e.g., precipitation of 'HadGEM2-ES' has values only until Nov 2099
				addMissingMonthAtEnd <- 12 - endMonth
				timeCount <- (endyear - startyear + 1) * 12 - addMissingMonthAtEnd
				
				return( list(timeStartIndex=timeStartIndex, timeCount=timeCount, addMissingMonthAtEnd=addMissingMonthAtEnd) )
			}
			
			get.GDODCPUCLLNL <- function(i, startyear, endyear){
				scen <- reqGets[ig <- (i - 1) %% nrow(reqGets) + 1, 1]
				gcm <- reqGets[ig, 2]
				lon <- locations[il <- (i - 1) %/% nrow(reqGets) + 1, 1]
				lat <- locations[il, 2]
				if(!be.quiet && i %% 1000 == 1) print(paste(i, "th extraction of GDO-DCP-UC-LLNL at", Sys.time(), "for", gcm, scen, "at", lon, lat))

				get.netCDFcontent <- function(filepath, variable, unit, startyear, endyear){
					nc <- nc_open(filename=filepath, write=FALSE, readunlim=TRUE, verbose=FALSE)
					stopifnot(grepl(unit, nc$var[[variable]]$units, fixed=TRUE))
					
					#Time index
					nct <- get.TimeIndices(nc=nc, startyear=startyear, endyear=endyear)
					
					#matrices containing the latitudes/longitudes in the netCDF files... these are used to get the correct indices in the whereNearest function in the nc_getByCoords function
					lats <- nc$dim$lat$vals   # NOTE: dim values are CACHED, don't read them
					lons <- nc$dim$lon$vals
					if(any(lons > 180)) lons <- ifelse(lons > 180, lons - 360, lons)
					
					# getting the values from the netCDF files...
					res <- nc_getByCoords(nc=nc, varid=variable, lats=lats, lons=lons, lat=lat, lon=lon, timeStartIndex=nct$timeStartIndex, timeCount=nct$timeCount)
					if(nct$addMissingMonthAtEnd > 0) res <- c(res, rep(NA, times=nct$addMissingMonthAtEnd))
					
					nc_close(nc) #close the netCDF file
					return(res)
				}
				
				nYears <- endyear - startyear + 1
				gcmFiles <- list.files(file.path(dir.ex.dat, scen), pattern=gcm, full.names=TRUE)
				
				#Get precipitation data
				prcp <- get.netCDFcontent(filepath=gcmFiles[grepl(fileVarTags[1], gcmFiles)], variable=varTags[1], unit="mm/d", startyear=startyear, endyear=endyear)
				prcp <- mmPerDay_to_cmPerMonth(prcp, startyear, endyear)
				#Get temperature data
				if(any(grepl(fileVarTags[3], gcmFiles)) && any(grepl(fileVarTags[4], gcmFiles))){
					tmin <- get.netCDFcontent(filepath=gcmFiles[grepl(fileVarTags[3], gcmFiles)], variable=varTags[3], unit="C", startyear=startyear, endyear=endyear)
					tmax <- get.netCDFcontent(filepath=gcmFiles[grepl(fileVarTags[4], gcmFiles)], variable=varTags[4], unit="C", startyear=startyear, endyear=endyear)
				} else if(any(grepl(fileVarTags[2], gcmFiles))){
					tmin <- tmax <- get.netCDFcontent(filepath=gcmFiles[grepl(fileVarTags[2], gcmFiles)], variable=varTags[2], unit="C", startyear=startyear, endyear=endyear)
				}
					
				#calculate monthly averages for precipitation and temperature
				res <- matrix(NA, nrow=1, ncol=2 + 3*12, dimnames=list(paste(sapply(list(scen, gcm), as.character), collapse="."), NULL))
				res[1, 1:2] <- c(lon, lat)
				
				res[1, 2 + 1:12] <- aggregate(prcp, by=list(rep(1:12, times=nYears)), FUN=mean, na.rm=TRUE)[, 2]
				res[1, 2 + 12 + 1:12] <- aggregate(tmin, by=list(rep(1:12, times=nYears)), FUN=mean, na.rm=TRUE)[, 2]
				res[1, 2 + 2*12 + 1:12] <- aggregate(tmax, by=list(rep(1:12, times=nYears)), FUN=mean, na.rm=TRUE)[, 2]
				
				return(res)
			}
			
			
			#get data from netCDF files
			if(parallel_runs && parallel_init){
				#call the simulations depending on parallel backend
				list.export <- c("dir.ex.dat", "get.GDODCPUCLLNL", "reqGets", "locations", "get.TimeIndices", "mmPerDay_to_cmPerMonth", "nc_getByCoords", "whereNearest", "fileVarTags", "varTags", "be.quiet")
				if(identical(parallel_backend, "mpi")) {
					exportObjects(list.export)
					mpi.bcast.cmd(library(ncdf4, quietly = TRUE))

					res <- mpi.applyLB(x=1:requestN, fun=get.GDODCPUCLLNL, startyear=startGDODCPUCLLNL, endyear=endGDODCPUCLLNL)

					mpi.bcast.cmd(rm(list=ls(all=TRUE)))
					mpi.bcast.cmd(gc())

					temp <- sapply(res, FUN=rownames)
					res <- matrix(unlist(res), ncol=2 + 3*12, dimnames=list(temp, NULL))
				} else if(identical(parallel_backend, "snow")) {
					snow::clusterExport(cl, list.export)
					snow::clusterEvalQ(cl, library(ncdf4, quietly = TRUE))

					res <- foreach(i=1:requestN, .combine="rbind", .inorder=FALSE) %dopar% get.GDODCPUCLLNL(i=i, startyear=startGDODCPUCLLNL, endyear=endGDODCPUCLLNL)
					
					snow::clusterEvalQ(cl, rm(list=ls(all=TRUE)))
					snow::clusterEvalQ(cl, gc())
				} else if(identical(parallel_backend, "multicore")) {
					res <- foreach(i=1:requestN, .combine="rbind", .inorder=FALSE) %dopar% get.GDODCPUCLLNL(i=i, startyear=startGDODCPUCLLNL, endyear=endGDODCPUCLLNL)
				}
			} else {
				res <- foreach(i=1:requestN, .combine="rbind", .inorder=FALSE) %do% get.GDODCPUCLLNL(i=i, startyear=startGDODCPUCLLNL, endyear=endGDODCPUCLLNL)
			}
			save(res, file=file.path(dir.sw.dat, "extractionGDODCPUCLLNL.RData"))
			
			#prepare data for SoilWat wrapper format
			#reshape from long to wide format for climate conditions sorted as in 'climate.conditions'
			i_climCond <- match(rownames(res), climate.conditions[!grepl(climate.ambient, climate.conditions)])
			res <- cbind(i_climCond, res)
			dimnames(res) <- list(NULL, c("CC", "Lon", "Lat", paste0(rep(c("pr", "tmin", "tmax"), each=12), rep(1:12, times=2))))
			res <- reshape(data=data.frame(res), timevar="CC", idvar=c("Lon","Lat"), direction="wide")
			
			#sort sites
			idLocs <- apply(locations, 1, paste0, collapse="_")
			idLocRes <- apply(res[, 1:2], 1, paste0, collapse="_")
			res <- res[match(idLocRes, idLocs, nomatch=0), ]
			
			if((temp <- nrow(res) - sum(complete.cases(res))) > 0){
				warning(paste(temp, "sites didn't extract climate scenario information by 'ExtractClimateChangeScenarios_CMIP3/5_BCSD_GDODCPUCLLNL'"))
				failedLocations_GDODCPUCLLNL <- locations[temp <- which(apply(res, 1, FUN=function(x) any(is.na(x)))), ]
				include_YN_updateFailed <- include_YN
				include_YN_updateFailed[include_YN > 0][temp] <- 0
				save(failedLocations_GDODCPUCLLNL, include_YN_updateFailed, file=file.path(dir.in, "failedLocations_GDODCPUCLLNL.RData"))
				SWRunInformation_updateFailed <- cbind(SWRunInformation, include_YN_updateFailed=include_YN_updateFailed)
				write.csv(SWRunInformation_updateFailed, file=file.path(dir.in, paste0("failedLocationsUpdated_GDODCPUCLLNL_", datafile.SWRunInformation)))
			}
			
			#add data to sw_input_climscen and set the use flags
			icols <- 1 + 12 * 3 + 1:(nrow(reqGets) * 12 * 3)
			sw_input_climscen_values_use[icols] <- 1
			sw_input_climscen_values[include_YN > 0, icols] <- res[, -(1:2)]
			
			#write data to datafile.climatescenarios_values
			write.csv(rbind(sw_input_climscen_values_use, sw_input_climscen_values), file=file.path(dir.sw.dat, datafile.climatescenarios_values), row.names=FALSE)
			
			rm(locations, requestN, startGDODCPUCLLNL, endGDODCPUCLLNL, i_climCond, res, idLocs, idLocRes, icols, fileVarTags, varTags)
		} else {
			warning("Not all requested RCPs and/or GCMs requested are available in 'ExtractClimateChangeScenarios_CMIP3/5_BCSD_GDODCPUCLLNL'")
		}
		rm(dir.ex.dat, gcmsGDODCPUCLLNL, scenariosGDODCPUCLLNL)
	}

	if(!be.quiet) print(paste("Finished 'ExtractClimateChangeScenarios_CMIP3/5_BCSD_GDODCPUCLLNL' at", Sys.time()))
	
}

if(	exinfo$ExtractClimateChangeScenarios_CMIP3_ClimateWizardEnsembles_Global ||
	exinfo$ExtractClimateChangeScenarios_CMIP3_ClimateWizardEnsembles_USA){

	if(!be.quiet) print(paste("Started 'ExtractClimateChangeScenarios_CMIP3_ClimateWizardEnsembles' at", Sys.time()))
	
	list.scenarios.datafile <- climate.conditions[!grepl(climate.ambient, climate.conditions)]
	if(length(list.scenarios.datafile) > 0){ #extracts only information requested in the 'datafile.SWRunInformation'
		
		if(exinfo$ExtractClimateChangeScenarios_CMIP3_ClimateWizardEnsembles_Global){
			#Maurer EP, Adam JC, Wood AW (2009) Climate model based consensus on the hydrologic impacts of climate change to the Rio Lempa basin of Central America. Hydrology and Earth System Sciences, 13, 183-194.
			#accessed via climatewizard.org on July 10, 2012
			dir.ex.dat <- file.path(dir.external, "ExtractClimateChangeScenarios", "ClimateWizard_CMIP3", "Global")
		}
		if(exinfo$ExtractClimateChangeScenarios_CMIP3_ClimateWizardEnsembles_USA){
			#Maurer, E. P., L. Brekke, T. Pruitt, and P. B. Duffy. 2007. Fine-resolution climate projections enhance regional climate change impact studies. Eos Transactions AGU 88:504.
			#accessed via climatewizard.org
			dir.ex.dat <- file.path(dir.external, "ExtractClimateChangeScenarios", "ClimateWizard_CMIP3", "USA")
		}
		
		list.scenarios.external <- basename(list.dirs2(path=dir.ex.dat, full.names=FALSE, recursive=FALSE))
		
		if(all(list.scenarios.datafile %in% list.scenarios.external)){
			#locations of simulation runs
			locations <- SpatialPoints(coords=with(SWRunInformation, data.frame(X_WGS84, Y_WGS84)), proj4string=CRS("+proj=longlat +datum=WGS84"))
			
			for(sc in 1:length(list.scenarios.datafile)){
				dir.ex.dat.sc <- file.path(dir.ex.dat, list.scenarios.datafile[sc])
				temp <- basename(list.dirs2(path=dir.ex.dat.sc, full.names=FALSE, recursive=FALSE))
				if(exinfo$ExtractClimateChangeScenarios_CMIP3_ClimateWizardEnsembles_Global){
					dir.ex.dat.sc.ppt <- file.path(dir.ex.dat.sc, temp[grepl(pattern="Precipitation_Value", x=temp)])
					dir.ex.dat.sc.temp <- file.path(dir.ex.dat.sc, temp[grepl(pattern="Tmean_Value", x=temp)])
				}		
				if(exinfo$ExtractClimateChangeScenarios_CMIP3_ClimateWizardEnsembles_USA){
					dir.ex.dat.sc.ppt <- file.path(dir.ex.dat.sc, temp[grepl(pattern="Precipitation_Change", x=temp)])
					dir.ex.dat.sc.temp <- file.path(dir.ex.dat.sc, temp[grepl(pattern="Tmean_Change", x=temp)])
				}		
				list.temp.asc <- list.files(dir.ex.dat.sc.temp, pattern=".asc")
				list.ppt.asc <- list.files(dir.ex.dat.sc.ppt, pattern=".asc")
				
				#extract data
				get.month <- function(path, grid){
					g <- raster(file.path(path, grid))
					locations.CoordG <- spTransform(locations, CRS=CRS(proj4string(g)))	#transform points to grid-coords
					vals <- extract(g, locations.CoordG)
				}
				sc.temp <- sapply(st_mo, FUN=function(m) get.month(path=dir.ex.dat.sc.temp, grid=list.temp.asc[grepl(pattern=paste("_", m, "_", sep=""), x=list.temp.asc)]))
				sc.ppt <- sapply(st_mo, FUN=function(m) get.month(path=dir.ex.dat.sc.ppt, grid=list.ppt.asc[grepl(pattern=paste("_", m, "_", sep=""), x=list.temp.asc)]))
				
				if(exinfo$ExtractClimateChangeScenarios_CMIP3_ClimateWizardEnsembles_Global){
					#temp value in C
					#ppt value in mm
					#add data to sw_input_climscen and set the use flags
					sw_input_climscen_values_use[i.temp <- match(paste("PPTmm_m", st_mo, "_sc", formatC(sc, width=2,format="d", flag="0"), sep=""), colnames(sw_input_climscen_values_use))] <- 1
					sw_input_climscen_values[, i.temp] <- sc.ppt
					sw_input_climscen_values_use[i.temp <- match(paste("TempC_m", st_mo, "_sc", formatC(sc, width=2,format="d", flag="0"), sep=""), colnames(sw_input_climscen_values_use))] <- 1
					sw_input_climscen_values[, i.temp] <- sc.temp
				}
				if(exinfo$ExtractClimateChangeScenarios_CMIP3_ClimateWizardEnsembles_USA){
					sc.temp <- sc.temp * 5/9	#temp addand in C
					sc.ppt <- 1 + sc.ppt/100	#ppt change as factor
					#add data to sw_input_climscen and set the use flags
					sw_input_climscen_use[i.temp <- match(paste("PPTfactor_m", st_mo, "_sc", formatC(sc, width=2,format="d", flag="0"), sep=""), colnames(sw_input_climscen_use))] <- 1
					sw_input_climscen[, i.temp] <- sc.ppt
					sw_input_climscen_use[i.temp <- match(paste("deltaTempC_m", st_mo, "_sc", formatC(sc, width=2,format="d", flag="0"), sep=""), colnames(sw_input_climscen_use))] <- 1
					sw_input_climscen[, i.temp] <- sc.temp
				}
			}
			
			res <- nrow(sw_input_climscen_values[, i.temp]) - sum(complete.cases(sw_input_climscen_values[, i.temp]))
			if(res > 0) print(paste(res, "sites didn't extract climate scenario information by 'ExtractClimateChangeScenarios_CMIP3_ClimateWizardEnsembles'"))
			
			#write data to datafile.climatescenarios_values
			tempdat <- rbind(sw_input_climscen_values_use, sw_input_climscen_values)
			write.csv(tempdat, file=file.path(dir.sw.dat, datafile.climatescenarios_values), row.names=FALSE)
			
			rm(list.scenarios.datafile, list.scenarios.external, tempdat, sc.temp, sc.ppt, res, locations)
		} else {
			warning("Not all scenarios requested in 'datafile.SWRunInformation' are available in with 'ExtractClimateChangeScenarios_CMIP3_ClimateWizardEnsembles'")
		}
	}
	if(!be.quiet) print(paste("Finished 'ExtractClimateChangeScenarios_CMIP3_ClimateWizardEnsembles' at", Sys.time()))
}


if(exinfo$ExtractSoilDataFromCONUSSOILFromSTATSGO_USA){
	if(!be.quiet) print(paste("Started 'ExtractSoilDataFromCONUSSOILFromSTATSGO_USA' at", Sys.time()))
	#Miller, D. A. and R. A. White. 1998. A conterminous United States multilayer soil characteristics dataset for regional climate and hydrology modeling. Earth Interactions 2:1-26.
	#CONUS-SOIL: rasterized and controlled STATSGO data; information for 11 soil layers available
	ldepth <- c(5, 10, 20, 30, 40, 60, 80, 100, 150, 200, 250)	#in cm
	
	dir.ex.dat <- file.path(dir.external, "ExtractSoilDataFromCONUSSOILFromSTATSGO", "CONUSSoil")
	
	#locations of simulation runs
	locations <- SpatialPoints(coords=with(SWRunInformation, data.frame(X_WGS84, Y_WGS84)), proj4string=CRS("+proj=longlat +datum=WGS84"))
	
	#extract data
	locations.CoordG <- spTransform(locations, CRS=CRS(proj4string(g)))	#transform points to grid-coords
	
	g <- raster(file.path(dir.ex.dat, "cs_bulkd"), RAT=TRUE)
	val <- extract(g, locations.CoordG)
	temp <- factorValues(g, val)
	bedrock <- temp[, "ROCKDEPM"]	#depth in cm >< bedrock from datafile.bedrock, but seems to make more sense?
	cl <- 1:max(findInterval(bedrock, ldepth), na.rm=TRUE)
	bulkd <- temp[, paste0("L", cl, "_BD")]
		
	g <- raster(file.path(dir.ex.dat, "cs_sandsilt"), RAT=TRUE)
	val <- extract(g, locations.CoordG)
	temp <- factorValues(g, val)
	sand <- temp[, paste0("SAND_L", cl)] / 100
	clay <- temp[, paste0("CLAY_L", cl)] / 100	
	
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
	
	if(any(sand == 0, clay == 0)) warning(paste("'ExtractSoilDataFromCONUSSOILFromSTATSGO_USA': no soil information for one or several sites (e.g., sand or clay is 0): this will likely lead to crashes of SoilWat"))
	
	rm(tempdat, i.temp, cl, bedrock, bulkd, sand, clay, val, temp, g, locations)
	
	if(!be.quiet) print(paste("Finished 'ExtractSoilDataFromCONUSSOILFromSTATSGO_USA' at", Sys.time()))
}


if(exinfo$ExtractElevation_NED_USA){
	if(!be.quiet) print(paste("Started 'ExtractElevation_NED_USA' at", Sys.time()))
	#ned.usgs.gov
	
	dir.ex.dat <- file.path(dir.external, "ExtractTopographyANDElevation", "NED_1arcsec")
	
	#read raster data
	g.elev <- raster(file.path(dir.ex.dat, "ned_1s_westernUS_GeogrNAD83.tif"))

	#locations of simulation runs
	locations <- SpatialPoints(coords=with(SWRunInformation, data.frame(X_WGS84, Y_WGS84)), proj4string=CRS("+proj=longlat +datum=WGS84"))
	locations.CoordG <- spTransform(locations, CRS=CRS(proj4string(g.elev)))	#transform points to grid-coords
	
	#extract data for locations
	SWRunInformation$ELEV_m <- extract(g.elev, locations.CoordG)	# elevation in m a.s.l.

	#write data to datafile.SWRunInformation
	write.csv(SWRunInformation, file=file.path(dir.in, datafile.SWRunInformation), row.names=FALSE)
	
	rm(g.elev, locations, locations.CoordG)
	
	if(!be.quiet) print(paste("Finished 'ExtractElevation_NED_USA' at", Sys.time()))
}


if(exinfo$ExtractElevation_HWSD_Global){
	if(!be.quiet) print(paste("Started 'ExtractElevation_HWSD_Global' at", Sys.time()))
	
	dir.ex.dat <- file.path(dir.external, "ExtractTopographyANDElevation", "HWSD")
	
	#read raster data
	g.elev <- raster(file.path(dir.ex.dat, "GloElev_30as.asc"))
	
	#locations of simulation runs
	locations <- SpatialPoints(coords=with(SWRunInformation, data.frame(X_WGS84, Y_WGS84)), proj4string=CRS("+proj=longlat +datum=WGS84"))
	locations.CoordG <- spTransform(locations, CRS=CRS(proj4string(g.elev)))	#transform points to grid-coords
	
	#extract data for locations
	SWRunInformation$ELEV_m <- extract(g.elev, locations.CoordG)	# elevation in m a.s.l.
	
	#write data to datafile.SWRunInformation
	write.csv(SWRunInformation, file=file.path(dir.in, datafile.SWRunInformation), row.names=FALSE)
	
	rm(g.elev, locations, locations.CoordG)
	
	if(!be.quiet) print(paste("Finished 'ExtractElevation_HWSD_Global' at", Sys.time()))
}



if(exinfo$ExtractSkyDataFromNOAAClimateAtlas_USA){
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
	
	if(sum(c(is.na(rh), is.na(cover), is.na(wind))) > 0) warning(paste("Missing data in 'ExtractSkyDataFromNOAAClimateAtlas_USA'"))
	
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

#--------------------------------------------------------------------------------------------------#
