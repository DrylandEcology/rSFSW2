#--------------------------------------------------------------------------------------------------#
#------------------------OBTAIN INFORMATION FROM EXTERNAL DATASETS PRIOR TO SIMULATION RUNS TO CREATE THEM

exinfo$GDODCPUCLLNL <-  exinfo$ExtractClimateChangeScenarios_CMIP3_BCSD_GDODCPUCLLNL_USA ||
		exinfo$ExtractClimateChangeScenarios_CMIP3_BCSD_GDODCPUCLLNL_Global ||
		exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_GDODCPUCLLNL_USA ||
		exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_GDODCPUCLLNL_Global

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

if(exinfo$GDODCPUCLLNL){
	if(!require(ncdf4, quietly=TRUE)) {
		tryCatch(install.packages("ncdf4", repos=url.Rrepos, lib=dir.libraries), warning=function(w) { print(w); print("ncdf4 failed to install"); stop("Stopping") })
		stopifnot(require(ncdf4, quietly=TRUE))
	}
}

if(exinfo$GDODCPUCLLNL || exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_NEX_USA){
	
	if(!require(reshape2, quietly=TRUE)) {
		tryCatch(install.packages("reshape2", repos=url.Rrepos, lib=dir.libraries), warning=function(w) { print(w); print("reshape2 failed to install"); stop("Stopping") })
		stopifnot(require(reshape2, quietly=TRUE))
	}
}

if(exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_NEX_USA){
	useRCurl <- FALSE
	if(!require(RCurl, quietly=TRUE)) {
		tryCatch(install.packages("RCurl", repos=url.Rrepos, lib=dir.libraries), warning=function(w) { print(w); print("RCurl failed to install, using 'download.files' instead") })
	}
	if(require(RCurl, quietly=TRUE)) useRCurl <- TRUE
}



#--------------------------------------------------------------------------------------------------#
if(	exinfo$GDODCPUCLLNL || exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_NEX_USA){
	
	dbW_setConnection(dbFilePath=dbWeatherDataFile)
	dbW_iSiteTable <- dbW_getSiteTable()
	dbW_iScenarioTable <- dbW_getScenariosTable()
	
	climScen <- data.frame(matrix(unlist(strsplit(temp <- climate.conditions[!grepl(climate.ambient, climate.conditions)], split=".", fixed=TRUE)), ncol=3, byrow=TRUE), stringsAsFactors=FALSE)
	climScen$imap_todbW <- match(temp, table=dbW_iScenarioTable$Scenario, nomatch=0)
	reqGCMs <- unique(climScen[, 3])
	reqRCPs <- unique(climScen[, 2])
	reqRCPsPerGCM <- lapply(reqGCMs, FUN=function(x) unique(climScen[x == climScen[, 3], 2]))
	reqDownscalingsPerGCM <- lapply(reqGCMs, FUN=function(x) unique(climScen[x == climScen[, 3], 1]))
	
	for(i in 1:length(reqGCMs)) {
		dir.create2(file.path(dir.out,"temp",reqGCMs[i]), showWarnings=FALSE, recursive=TRUE)
	}
	
#	reqGets <- unique(climScen[, -1])
#	imap_GetsToScen <- match(apply(climScen[, -1], 1, paste0, collapse="."), table=apply(reqGets, 1, paste0, collapse="."), nomatch=0)
	
	#Downscaling/bias-correction functions
	downscale.raw <- function(obs.hist.daily, obs.hist.monthly, scen.fut.monthly){
		#cf 'direct' approach in Lenderink, G., A. Buishand, and W. van Deursen. 2007. Estimates of future discharges of the river Rhine using two scenario methodologies: direct versus delta approach. Hydrology and Earth System Sciences 11:1145-1159.
		
		# 1. Calculate mean monthly values in historic and future scenario values
		temp <- rep(1:12, times=nrow(scen.fut.monthly) / 12)
		scen.fut.mean <- aggregate(scen.fut.monthly[, 2+1:3], by=list(temp), FUN=function(x) mean(x, na.rm=TRUE))[, 2:4]
		temp <- rep(1:12, times=nrow(obs.hist.monthly) / 12)
		obs.hist.mean <- aggregate(obs.hist.monthly[, 2+1:3], by=list(temp), FUN=function(x) mean(x, na.rm=TRUE))[, 2:4]
		
		# 2. Calculate deltas between observed historic and future mean scenario values
		deltas <- cbind(scen.fut.mean[, 1:2] - obs.hist.mean[, 1:2], scen.fut.mean[, 3] / ifelse(obs.hist.mean[, 3] > 0, obs.hist.mean[, 3], 1))
		
		# 3. Apply deltas to historic daily weather
		scen.fut.daily <- lapply(obs.hist.daily, FUN=function(obs) {
					month <- as.POSIXlt(paste(obs@year, obs@data[, "DOY"], sep="-"), format="%Y-%j")$mon + 1
					new("swWeatherData", data=data.matrix(cbind(obs@data[, "DOY"], obs@data[, c("Tmax_C", "Tmin_C")] + deltas[month, 1:2], obs@data[, "PPT_cm"] * deltas[month, 3]), rownames.force=FALSE), year=obs@year)
				})
		return(scen.fut.daily)
	}
	
	downscale.delta <- function(obs.hist.daily, scen.hist.monthly, scen.fut.monthly){
		#Hay, L. E., R. L. Wilby, and G. H. Leavesley. 2000. A comparison of delta change and downscaled gcm scenarios for three mountainous basins in the United States. Journal of the American Water Resources Association 36:387-397.
		#Hamlet, A. F., E. P. Salathé, and P. Carrasco. 2010. Statistical downscaling techniques for global climate model simulations of temperature and precipitation with application to water resources planning studies. Chapter 4. Final Report for the Columbia Basin Climate Change Scenarios Project. Climate Impacts Group, Center for Science in the Earth System, Joint Institute for the Study of the Atmosphere and Ocean, University of Washington, Seattle, WA.
		
		#obs.hist.daily: list of years with soilwat weather
		#scen.hist.monthly: matrix with monthly time-series of scenario weather under historic time period
		#scen.fut.monthly: matrix with monthly time-series of scenario weather under projected time period
		
		# 1. Calculate mean monthly values in historic and future scenario values
		temp <- rep(1:12, times=nrow(scen.fut.monthly) / 12)
		scen.fut.mean <- aggregate(scen.fut.monthly[, 2+1:3], by=list(temp), FUN=function(x) mean(x, na.rm=TRUE))[, 2:4]
		temp <- rep(1:12, times=nrow(scen.hist.monthly) / 12)
		scen.hist.mean <- aggregate(scen.hist.monthly[, 2+1:3], by=list(temp), FUN=function(x) mean(x, na.rm=TRUE))[, 2:4]
		
		# 2. Calculate deltas between historic and future mean scenario values
		deltas <- cbind(scen.fut.mean[, 1:2] - scen.hist.mean[, 1:2], scen.fut.mean[, 3] / ifelse(scen.hist.mean[, 3] > 0, scen.hist.mean[, 3], 1))
		
		# 3. Apply deltas to historic daily weather
		scen.fut.daily <- lapply(obs.hist.daily, FUN=function(obs) {
					month <- as.POSIXlt(paste(obs@year, obs@data[, "DOY"], sep="-"), format="%Y-%j")$mon + 1
					new("swWeatherData", data=data.matrix(cbind(obs@data[, "DOY"], obs@data[, c("Tmax_C", "Tmin_C")] + deltas[month, 1:2], obs@data[, "PPT_cm"] * deltas[month, 3]), rownames.force=FALSE), year=obs@year)
				})
		return(scen.fut.daily)
	}
	
	downscale.deltahybrid <- function(obs.hist.daily, obs.hist.monthly, scen.hist.monthly, scen.fut.monthly){
		#Hamlet, A. F., E. P. Salathé, and P. Carrasco. 2010. Statistical downscaling techniques for global climate model simulations of temperature and precipitation with application to water resources planning studies. Chapter 4. Final Report for the Columbia Basin Climate Change Scenarios Project. Climate Impacts Group, Center for Science in the Earth System, Joint Institute for the Study of the Atmosphere and Ocean, University of Washington, Seattle, WA.
		#Dickerson-Lange, S. E., and R. Mitchell. 2014. Modeling the effects of climate change projections on streamflow in the Nooksack River basin, Northwest Washington. Hydrological Processes:doi: 10.1002/hyp.10012.
		#Wang, L., and W. Chen. 2014. Equiratio cumulative distribution function matching as an improvement to the equidistant approach in bias correction of precipitation. Atmospheric Science Letters 15:1-6.
		
		#obs.hist.daily: list of years with soilwat weather
		#scen.hist.monthly: matrix with monthly time-series of scenario weather under historic time period
		#scen.fut.monthly: matrix with monthly time-series of scenario weather under projected time period
		
		eCDF.Cunnane <- function(x){
			x <- sort(x)
			n <- length(x)
			q <- (1:n - 0.4) / (n + 0.2) #Cunnane (1978)
			f <- splinefun(x=q, y=x, method="monoH.FC", ties=mean) #'hyman' produces too extreme large values
			return(list(x=x, q=q, fun=f))
		}
		
		delta_ts <- matrix(NA, ncol=5, nrow=nrow(obs.hist.monthly), dimnames=list(NULL, colnames(obs.hist.monthly)))
		delta_ts[, 1:2] <- obs.hist.monthly[, 1:2]
		for(iv in 1:3){
			for(m in 1:12){
				# 1. Calculate eCDF of historic weather and of scenario using Cunnane plotting position (Cunnane 1978) following Hamlet et al. 2010
				#		- interpolation and extrapolation by splines (Dickerson-Lange et al. 2014) instead of standard anomalies (Hamlet et al. 2010)
				obs.hist.x <- obs.hist.monthly[rep(1:12, times=nrow(obs.hist.monthly) / 12) == m, 2 + iv]
				scen.hist.x <- scen.hist.monthly[rep(1:12, times=nrow(scen.hist.monthly) / 12) == m, 2 + iv]
				scen.fut.x <- scen.fut.monthly[rep(1:12, times=nrow(scen.fut.monthly) / 12) == m, 2 + iv]
				
				scen.hist.ecdf <- eCDF.Cunnane(scen.hist.x)
				obs.hist.ecdf <- eCDF.Cunnane(obs.hist.x)
				scen.fut.ecdf <- eCDF.Cunnane(scen.fut.x)
				
				# 2. Adjust future scenario with quantile-based deltas from historic comparison for future scenario values with linear extrapolation
				if(iv <= 2){ #Temp is additive
					scen.fut.xadj <- scen.fut.x + obs.hist.ecdf$fun(scen.fut.ecdf$q, extrapol="linear") - scen.hist.ecdf$fun(scen.fut.ecdf$q, extrapol="linear")
				} else { #PPT is treated multiplicative: Wang et al. 2014
					scen.fut.xadj <- scen.fut.x * obs.hist.ecdf$fun(scen.fut.ecdf$q, extrapol="linear") / scen.hist.ecdf$fun(scen.fut.ecdf$q, extrapol="linear")
				}
				
				# 3. Calculate eCDF of future adjusted scenario
				scen.fut2.ecdf <- eCDF.Cunnane(scen.fut.xadj)
				
				# 5. Quantile map observed historic to adjusted future scenario
				if(iv <= 2){ #Temp is additive
					mapFut <- scen.fut2.ecdf$fun(obs.hist.ecdf$q, extrapol="linear") - obs.hist.ecdf$fun(obs.hist.ecdf$q, extrapol="linear")
				} else {
					mapFut <- scen.fut2.ecdf$fun(obs.hist.ecdf$q, extrapol="linear") / obs.hist.ecdf$fun(obs.hist.ecdf$q, extrapol="linear")
				}
				delta_ts[delta_ts[, "Month"] == m, 2 + iv] <- mapFut[rank(obs.hist.x)]
			}
		}
		
		# 6. Apply deltas to historic daily weather
		scen.fut.daily <- lapply(obs.hist.daily, FUN=function(obs) {
					month <- as.POSIXlt(paste(obs@year, obs@data[, "DOY"], sep="-"), format="%Y-%j")$mon + 1
					ydelta <- delta_ts[delta_ts[, "Year"] == obs@year, -(1:2)]
					new("swWeatherData", data=data.matrix(cbind(obs@data[, "DOY"], obs@data[, c("Tmax_C", "Tmin_C")] + ydelta[month, 1:2], obs@data[, "PPT_cm"] * ydelta[month, 3]), rownames.force=FALSE), year=obs@year)
				})
		return(scen.fut.daily)
	}
	
	get_monthlyTimeSeriesFromDaily <- function(dailySW){
		monthly <- matrix(NA, nrow=length(dailySW) * 12, ncol=5, dimnames=list(NULL, c("Year", "Month", "Tmax_C", "Tmin_C", "PPT_cm")))
		for(y in seq_along(dailySW)){
			weath <- dailySW[[y]]
			month <- as.POSIXlt(paste(weath@year, weath@data[, "DOY"], sep="-"), format="%Y-%j")$mon + 1
			monthly[1:12 + 12*(y - 1), ] <- data.matrix(cbind(Year=weath@year, Month=1:12, aggregate(weath@data[, c("Tmax_C", "Tmin_C")], by=list(month), FUN=mean)[, 2:3], PPT_cm=aggregate(weath@data[, "PPT_cm"], by=list(month), FUN=sum)[, 2]))
		}
		return(monthly)
	}
	
}

#--------------------------------------------------------------------------------------------------#

if(exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_NEX_USA){
	saveNEXtempfiles <- FALSE
	repeatUntilComplete <- TRUE
	if(!be.quiet) print(paste("Started 'ExtractClimateChangeScenarios_CMIP5_BCSD_NEX_USA' at", Sys.time()))
	opt <- options("timeout")
	options(timeout=5*60)
	
	tempN <- sum(include_YN) * length(reqGCMs)
	
	if(length(reqGCMs) > 0 && tempN < 6500){
		reqRCPs <- tolower(reqRCPs)
		
		##https://portal.nccs.nasa.gov/portal_home/published/NEX.html
		gcmsNEX <- c("inmcm4", "bcc-csm1-1", "bcc-csm1-1-m", "NorESM1-M", "MRI-CGCM3", "MPI-ESM-MR", "MPI-ESM-LR", "MIROC5", "MIROC-ESM", "MIROC-ESM-CHEM", "IPSL-CM5B-LR", "IPSL-CM5A-MR", "IPSL-CM5A-LR", "HadGEM2-ES", "HadGEM2-CC", "HadGEM2-AO", "GISS-E2-R", "GFDL-ESM2M", "GFDL-ESM2G", "GFDL-CM3", "FIO-ESM", "FGOALS-g2", "CanESM2", "CSIRO-Mk3-6-0", "CNRM-CM5", "CMCC-CM", "CESM1-CAM5", "CESM1-BGC", "CCSM4", "BNU-ESM", "ACCESS1-0")
		scenariosNEX <- c("historical", "rcp26", "rcp45", "rcp60", "rcp85")
		stopifnot(any(grepl("historical", scenariosNEX)))
		
		if(all(reqRCPs %in% scenariosNEX) && all(reqGCMs %in% gcmsNEX)){
			#locations of simulation runs
			locations <- with(SWRunInformation[include_YN > 0, ], data.frame(X_WGS84, Y_WGS84, WeatherFolder))
			
			#put requests together
			requestN <- length(reqGCMs) * nrow(locations)
			if(!be.quiet) print(paste("'ExtractClimateChangeScenarios_CMIP5_BCSD_NEX_USA' will run", requestN, "times"))
			
			#timing: time slices
			timeNEX <- data.frame(matrix(NA, ncol=4, nrow=8, dimnames=list(NULL, c("Run", "Slice", "Time", "Year"))))
			timeNEX[, 1:3] <- expand.grid(c("start", "end"), c("first", "second"), c("hist", "fut"))[, 3:1]
			
			timeNEX[1, 4] <- max(1950, simstartyr)
			timeNEX[2, 4] <- min(2005, endyr)
			if(endyr > 2005){
				timeNEX[3, 4] <- 2006
				timeNEX[4, 4] <- min(2099, endyr)
			}
			timeNEX[7, 4] <- max(2006, deltaFutureToSimStart_yr + simstartyr)
			timeNEX[8, 4] <- min(2099, deltaFutureToSimStart_yr + endyr)
			if(deltaFutureToSimStart_yr + simstartyr < 2006){
				timeNEX[5, 4] <- max(1950, deltaFutureToSimStart_yr + simstartyr)
				timeNEX[6, 4] <- 2006
			}
			
			#function communicating with NEX
			nasa.dataserver <- "http://dataserver.nccs.nasa.gov"
			if(useRCurl) stopifnot(url.exists(nasa.dataserver)) #check whether we are online
			url.nex.ncss <- paste0(nasa.dataserver, "/thredds/ncss/grid/bypass/NEX-DCP30")
			downscaling <- "bcsd"
			gcmrun <- "r1i1p1"
			variables <- c("pr", "tasmin", "tasmax") #units c("kg/m2/s", "K", "K") --> SoilWat required units c("cm/day", "C", "C")
			
			mmPerSecond_to_cmPerMonth <- function(prcp_mmPerSecond, startyear, endyear){
				DaysPerMonths <- rle(as.POSIXlt(seq(from=as.POSIXlt(paste0(startyear, "-01-01")), to=as.POSIXlt(paste0(endyear, "-12-31")), by="1 day"))$mon)$lengths			
				return(prcp_mmPerSecond / 10 * DaysPerMonths * 24 * 60 * 60)
			}
			
			get.NEXvariable <- function(i, var, scen, gcm, lon, lat, startyear, endyear){
				request <- paste0(paste(url.nex.ncss, downscaling, scen, gcmrun,
								paste0(gcm, "_", var, ".ncml"), sep="/"), "?var=", paste0(gcm, "_", var), 
						"&latitude=", lat, "&longitude=", ifelse(lon > 180, lon - 360, lon),
						paste0("&time_start=", startyear, "-01-01T00%3A00%3A00Z&time_end=", endyear, "-12-31T23%3A59%3A59Z&timeStride=1"),
						"&accept=csv")
				
				if(useRCurl && !saveNEXtempfiles){
					success <- try(getURL(request, .opts=list(timeout=5*60, connecttimeout=60)), silent=TRUE)
					if(!inherits(success, "try-error")){
						if(grepl("Not Found", success, ignore.case=TRUE)){
							class(success) <- "try-error"
						} else {
							ftemp <- textConnection(success)
							success <- 0
						}
					}
				} else {
					success <- try(download.file(url=request, destfile=ftemp <- file.path(dir.out.temp, paste0("NEX_", gcm, "_", scen, "_", var, "_", round(lat, 5), "&", round(lon, 5), ".csv")), quiet=TRUE), silent=TRUE)
				}
				
				yearsN <- endyear - startyear + 1
				dat <- rep(NA, times=12*yearsN)
				if(!inherits(success, "try-error") && success == 0){
					temp <- read.csv(ftemp, colClasses=c("POSIXct", "NULL", "NULL", "numeric")) #colnames = Time, Lat, Long, Variable
					if(!saveNEXtempfiles && !useRCurl && file.exists(ftemp)) unlink(ftemp)
					if(nrow(temp) < 12*yearsN){ #some GCMs only have values up to Nov 2099
						tempYearMonth <- paste((temp2 <- as.POSIXlt(temp[, 1]))$year + 1900, temp2$mo + 1, sep="_")
						targetYearMonth <- paste(rep(startyear:endyear, each=12), rep(1:12, times=yearsN), sep="_")
						dat[match(tempYearMonth, targetYearMonth, nomatch=0)] <- temp[match(targetYearMonth, tempYearMonth, nomatch=0), 2]
					} else {
						dat <- temp[, 2]
					}
				} else {
					stop(paste(i, "th extraction of NEX at", Sys.time(), "for", gcm, scen, "at", lon, lat, ": not successful"))
				}
				
				return(dat)
			}
			
			get_GCMdata <- function(i, gcm, scen, lon, lat, startyear, endyear){
				clim <- vector("list", length=3)
				names(clim) <- variables
				for(iv in seq_along(variables)){
					#Extract data
					clim[[iv]] <- get.NEXvariable(i, var=variables[iv], scen=scen, gcm=gcm, lon=lon, lat=lat, startyear=startyear, endyear=endyear)
					#Adjust units
					if(variables[iv] == "pr"){#convert kg/m2/s -> cm/month
						clim[[iv]] <- mmPerSecond_to_cmPerMonth(prcp_mmPerSecond=clim[[iv]], startyear=startyear, endyear=endyear)
					} else if(grepl("tas", variables[iv])){	#convert K -> C
						clim[[iv]] <- clim[[iv]] - 273.15
					}
				}
				
				#Monthly weather time-series
				date <- as.POSIXlt(seq(from=as.POSIXlt(paste0(startyear, "-01-01")), to=as.POSIXlt(paste0(endyear, "-12-31")), by="1 month"))
				return(list(cbind(year=date$year + 1900, month=date$mon + 1, clim[["tasmax"]], clim[["tasmin"]], clim[["pr"]])))
			}
			
			get.NEX <- function(i){
				.local <- function(i){
					#Identify index for site and scenario
					gcm <- reqGCMs[ig <- (i - 1) %% length(reqGCMs) + 1]
					rcps <- reqRCPsPerGCM[[ig]]
					downs <- reqDownscalingsPerGCM[[ig]]
					lon <- locations[il <- (i - 1) %/% length(reqGCMs) + 1, 1]
					lat <- locations[il, 2]
					site_id <- dbW_iSiteTable[dbW_iSiteTable[, "Label"] == locations[il, 3], "Site_id"]		
					if(!be.quiet && i %% 1000 == 1) print(paste(i, "th extraction of NEX at", Sys.time(), "for", gcm, "(", paste(rcps, collapse=", "), ") at", lon, lat))
		
					#Data Bounding Box = lat= [24.06,49.92] lon= [-125.02,-66.48]
					if(lat >= 24.06 && lat <= 49.92 && lon >= -125.02 && lon <= -66.48){
						#Scenario monthly weather time-series
						scen.monthly <- matrix(data=vector("list", length=2*(1 + length(rcps))), ncol=2, dimnames=list(c("Current", as.character(rcps)), c("hist", "fut")))
						for(isc in 1:nrow(scen.monthly)){ #Get GCM data for each scenario and time slice
							scen <- c("historical", as.character(rcps))[isc]
							
							if(isc == 1){ #Slice past: 1950-2005
								if(!is.na(temp <- timeNEX[1, 4]) && !all(downs == "raw")) {# past slice for 'historic' data; 'raw' downscaling doesn't need "historical" scenario
									scen.monthly[isc, 1] <- get_GCMdata(i=i, gcm=gcm, scen=scen, lon=lon, lat=lat, startyear=temp, endyear=timeNEX[2, 4])
								}
								if(!is.na(temp <- timeNEX[5, 4])){ #past slice for 'future' scenario
									scen.monthly[isc, 2] <- get_GCMdata(i=i, gcm=gcm, scen=scen, lon=lon, lat=lat, startyear=temp, endyear=timeNEX[6, 4])
								}
							} else { #Slice future: 2006-2099
								if(!is.na(temp <- timeNEX[3, 4])){ #future slice for 'historic' data
									scen.monthly[isc, 1] <- get_GCMdata(i=i, gcm=gcm, scen=scen, lon=lon, lat=lat, startyear=temp, endyear=timeNEX[4, 4])
								}
								if(!is.na(temp <- timeNEX[7, 4])){ #future slice for 'future' scenario
									scen.monthly[isc, 2] <- get_GCMdata(i=i, gcm=gcm, scen=scen, lon=lon, lat=lat, startyear=temp, endyear=timeNEX[8, 4])
								}
							}
						}
						
						#Observed historic weather from weather database
						obs.hist.daily <- Rsoilwat::dbW_getWeatherData(Site_id=site_id, startYear=simstartyr, endYear=endYear, Scenario=climate.ambient)
						obs.hist.monthly <- get_monthlyTimeSeriesFromDaily(dailySW=obs.hist.daily)
						
						wdataOut <- list()
						for(ir in seq_along(rcps)){ #Apply downscaling for each RCP
							if(!all(downs == "raw")){
								scen.hist.monthly <- scen.monthly[1, 1][[1]]
								if(!is.null(scen.monthly[1 + ir, 1][[1]])) scen.hist.monthly <- rbind(scen.hist.monthly, scen.monthly[1 + ir, 1][[1]])
							}
							scen.fut.monthly <- scen.monthly[1 + ir, 2][[1]]
							if(!is.null(scen.monthly[1, 2][[1]])) scen.fut.monthly <- rbind(scen.monthly[1, 2][[1]], scen.fut.monthly)
							types <- list()
							if("raw" %in% downs){
								scen.fut.daily <- downscale.raw(obs.hist.daily, obs.hist.monthly, scen.fut.monthly)
								scenario_id <- dbW_iScenarioTable[dbW_iScenarioTable[, "Scenario"] == paste("raw", rcps[ir], gcm, sep="."), "id"]
								data_blob <- paste0("x'",paste0(memCompress(serialize(scen.fut.daily,NULL),type="gzip"),collapse = ""),"'",sep="")
								types[[length(types)+1]] <- list(Site_id=site_id, Scenario_id=scenario_id, weatherData=data_blob)
							}
							if("delta" %in% downs){
								scen.fut.daily <- downscale.delta(obs.hist.daily, scen.hist.monthly, scen.fut.monthly)
								scenario_id <- dbW_iScenarioTable[dbW_iScenarioTable[, "Scenario"] == paste("delta", rcps[ir], gcm, sep="."), "id"]
								data_blob <- paste0("x'",paste0(memCompress(serialize(scen.fut.daily,NULL),type="gzip"),collapse = ""),"'",sep="")
								types[[length(types)+1]] <- list(Site_id=site_id, Scenario_id=scenario_id, weatherData=data_blob)
							}
							if("hybrid-delta" %in% downs){
								scen.fut.daily <- downscale.deltahybrid(obs.hist.daily, obs.hist.monthly, scen.hist.monthly, scen.fut.monthly)
								scenario_id <- dbW_iScenarioTable[dbW_iScenarioTable[, "Scenario"] == paste("hybrid-delta", rcps[ir], gcm, sep="."), "id"]
								data_blob <- paste0("x'",paste0(memCompress(serialize(scen.fut.daily,NULL),type="gzip"),collapse = ""),"'",sep="")
								types[[length(types)+1]] <- list(Site_id=site_id, Scenario_id=scenario_id, weatherData=data_blob)
							}
							wdataOut[[ir]] <- types
						}
						save(wdataOut,file=file.path(dir.out,"temp",gcm,paste("NEX_",i,".Rdata",sep="")))
						res <- i
					} else {
						stop(paste(i, "th extraction of NEX at", Sys.time(), "at", lon, lat, ": outside of bounding data box"))
						break
					}
					
					return(res)
				}
				
				res <- if(inherits(try(.local(i), silent=FALSE), "try-error")) NULL else i	
				return(res)
			}
			
			#get data from NEX
			tryToGet_NEX <- function(is_ToDo){
				if(parallel_runs && parallel_init){
					#call the simulations depending on parallel backend
					list.export <- c("dir.out", "get.NEX", "get_GCMdata", "reqGCMs", "reqRCPsPerGCM", "reqDownscalingsPerGCM", "locations", "climScen", "url.nex.ncss", "downscaling", "gcmrun", "variables", "dir.out.temp", "be.quiet", "saveNEXtempfiles", "useRCurl", "mmPerSecond_to_cmPerMonth", "get.NEXvariable", "dbWeatherDataFile", "climate.ambient", "dbW_iSiteTable", "dbW_iScenarioTable",
							"timeNEX", "simstartyr", "endyr", "get_monthlyTimeSeriesFromDaily", "downscale.raw", "downscale.delta", "downscale.deltahybrid")
					if(identical(parallel_backend, "mpi")) {
						exportObjects(list.export)
						if(useRCurl && !saveNEXtempfiles) mpi.bcast.cmd(library(RCurl, quietly = TRUE))
						
						i_Done <- mpi.applyLB(x=is_ToDo, fun=get.NEX)
						
						mpi.bcast.cmd(rm(list=ls(all=TRUE)))
						mpi.bcast.cmd(gc())
					} else if(identical(parallel_backend, "snow")) {
						snow::clusterExport(cl, list.export)
						if(useRCurl && !saveNEXtempfiles) snow::clusterEvalQ(cl, library(RCurl, quietly = TRUE))
						
						i_Done <- snow::clusterApplyLB(cl, x=is_ToDo, fun=get.NEX)
						
						snow::clusterEvalQ(cl, rm(list=ls(all=TRUE)))
						snow::clusterEvalQ(cl, gc())
					} else if(identical(parallel_backend, "multicore")) {
						i_Done <- foreach(i=is_ToDo, .combine="c", .errorhandling="remove", .inorder=FALSE) %dopar% {
							Rsoilwat::dbW_setConnection(dbFilePath=dbWeatherDataFile)
							temp <- get.NEX(i)
							Rsoilwat::dbW_disconnectConnection()
							return(temp)
						}
					} else {
						i_Done <- NULL
					}
				} else {
					i_Done <- foreach(i=is_ToDo, .combine="rbind", .errorhandling="remove", .inorder=FALSE) %do% get.NEX(i)
				}
				
				require(Rsoilwat)
				Rsoilwat::dbW_setConnection(dbFilePath=dbWeatherDataFile)
				files <- list.files(path=file.path(dir.out, "temp"), pattern="NEX", recursive=TRUE, include.dirs = FALSE, no..=TRUE)
				if(length(files) != 0) {
					for (i in 1:length(files)) {
						wdata <- load(file.path(dir.out, "temp",files[i]))
						for(j in 1:length(wdata)) {
							for(l in 1:length(wdata[[j]])) {
								Rsoilwat:::dbW_addWeatherDataNoCheck(Site_id=wdata[[j]][[l]]$Site_id, Scenario_id=wdata[[j]][[l]]$Scenario_id, weatherData=wdata[[j]][[l]]$weatherData)
							}
						}
					}
				}
				Rsoilwat::dbW_disconnectConnection()
				
				return(sort(unlist(i_Done)))
			}
			
			repeatN <- 1
			i_AllToDo <- 1:requestN
			if(file.exists(temp <- file.path(dir.sw.dat, paste0("extractionNEX.rds")))){
				i_Done <- readRDS(file=temp)
			} else {
				i_Done <- tryToGet_NEX(is_ToDo=i_AllToDo)
				saveRDS(i_Done, file=file.path(dir.sw.dat, paste0("extractionNEX.rds")))
			}
			if(repeatUntilComplete){
				while(length(i_ToDo <- if(length(i_Done) > 0) i_AllToDo[-i_Done] else i_AllToDo) > 0){
					repeatN <- repeatN + 1
					if(!be.quiet) print(paste("'ExtractClimateChangeScenarios_CMIP5_BCSD_NEX_USA' will run a", repeatN, ". time to extract an additional", length(i_ToDo), "requests" ))
					i_Done <- sort(unique(c(i_Done, tryToGet_NEX(is_ToDo=i_ToDo))))
					saveRDS(i_Done, file=file.path(dir.sw.dat, paste0("extractionNEX.rds")))
				}
				rm(i_ToDo)
			}
			
			if(length(i_ToDo <- if(length(i_Done) > 0) i_AllToDo[-i_Done] else i_AllToDo) > 0){
				warning(paste(length(i_ToDo), "sites didn't extract climate scenario information by 'ExtractClimateChangeScenarios_CMIP5_BCSD_NEX_USA'"))
				failedLocations_NEX <- locations[temp <- unique((i_ToDo - 1) %/% length(reqGCMs) + 1), ]
				include_YN_updateFailed <- include_YN
				include_YN_updateFailed[include_YN > 0][temp] <- 0
				save(failedLocations_NEX, include_YN_updateFailed, file=file.path(dir.in, "failedLocations_NEX.RData"))
				SWRunInformation_updateFailed <- cbind(SWRunInformation, include_YN_updateFailed=include_YN_updateFailed)
				write.csv(SWRunInformation_updateFailed, file=file.path(dir.in, paste0("failedLocationsUpdated_NEX_", datafile.SWRunInformation)))
			}
			
			rm(icols, i_Done, i_ToDo, i_AllToDo, idLocs, locations, repeatN, i_climCond, get.NEX, url.nex.ncss, downscaling, gcmrun, variables, requestN, timeNEX)
			
			#make sure no lingering temp files are left on the hard drive
			if(!saveNEXtempfiles && length(flist <- list.files(dir.out.temp, pattern="NEX_", full.names=TRUE)) > 0) sapply(flist, unlink)
		} else {
			warning("Not all requested RCPs and/or GCMs requested are available in 'ExtractClimateChangeScenarios_CMIP5_BCSD_NEX_USA'")
		}
		rm(gcmsNEX, scenariosNEX)
	} else if(tempN >= 6500){
		warning(paste0("This implementation of 'ExtractClimateChangeScenarios_CMIP5_BCSD_NEX_USA' gets data for single locations; for many locations x climate conditions (here, ", sum(include_YN) * length(), "), a different approach downloading a spatial bounding box may be more appropriate"))
	}
	options(opt)
	rm(repeatUntilComplete, saveNEXtempfiles, tempN, opt)
	
	if(!be.quiet) print(paste("Finished 'ExtractClimateChangeScenarios_CMIP5_BCSD_NEX_USA' at", Sys.time()))
}


if(exinfo$GDODCPUCLLNL){
	
	if(!be.quiet) print(paste("Started 'ExtractClimateChangeScenarios/BCSD_GDODCPUCLLNL' at", Sys.time()))
	
	if(length(reqGCMs) > 0){
		##gdo-dcp.ucllnl.org/downscaled_cmip_projections
		dir.ex.dat <- file.path(dir.external, "Extract_GDO_DCP_UCLLNL_DownscaledClimateData")
		if(exinfo$ExtractClimateChangeScenarios_CMIP3_BCSD_GDODCPUCLLNL_USA) dir.ex.dat <- file.path(dir.ex.dat, "CMIP3_BCSD", "CONUS_0.125degree")
		if(exinfo$ExtractClimateChangeScenarios_CMIP3_BCSD_GDODCPUCLLNL_Global) dir.ex.dat <- file.path(dir.ex.dat, "CMIP3_BCSD", "Global_0.5degree_MaurerEd")
		if(exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_GDODCPUCLLNL_USA) dir.ex.dat <- file.path(dir.ex.dat, "CMIP5_BCSD", "CONUS_0.125degree_r1i1p1")
		if(exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_GDODCPUCLLNL_Global) dir.ex.dat <- file.path(dir.ex.dat, "CMIP5_BCSD", "Global_0.5degree_r1i1p1")
		
		scenariosGDODCPUCLLNL <- list.dirs(dir.ex.dat, full.names=FALSE, recursive=FALSE)
		stopifnot(any(grepl("Historical", scenariosGDODCPUCLLNL)))
		if(any((temp <- sapply(scenariosGDODCPUCLLNL, FUN=function(x) length(list.files(file.path(dir.ex.dat, x))))) == 0)) scenariosGDODCPUCLLNL <- scenariosGDODCPUCLLNL[temp > 0]
		
		gcmsGDODCPUCLLNL <- unique(unlist(sapply(scenariosGDODCPUCLLNL, FUN=function(x) sapply(strsplit(list.files(file.path(dir.ex.dat, x)), split="_", fixed=TRUE), FUN=function(x) x[5]))))
		
		if(all(reqRCPs %in% scenariosGDODCPUCLLNL) && all(sapply(1:length(reqGCMs), FUN=function(i) sum(gcmsGDODCPUCLLNL %in% reqGCMs[i]) == 1 + length(reqRCPs)))){
			#put requests together
			locations <- with(SWRunInformation[include_YN > 0, ], data.frame(X_WGS84, Y_WGS84, WeatherFolder))	#locations of simulation runs
			requestN <- length(reqGCMs) * nrow(locations)
			if(!be.quiet) print(paste("'ExtractClimateChangeScenarios/BCSD_GDODCPUCLLNL' will run", requestN, "times"))
			
			#bounding box
			bbox <- data.frame(matrix(NA, nrow=2, ncol=2, dimnames=list(NULL, c("lat", "lon"))))
			if(exinfo$ExtractClimateChangeScenarios_CMIP3_BCSD_GDODCPUCLLNL_USA || exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_GDODCPUCLLNL_USA){
				bbox$lat <- c(25.125, 52.875)
				bbox$lon <- c(-124.625, -67)
			}
			if(exinfo$ExtractClimateChangeScenarios_CMIP3_BCSD_GDODCPUCLLNL_Global || exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_GDODCPUCLLNL_Global){
				bbox$lat <- c(-55.25-0.25, 83.25+0.25)
				bbox$lon <- c(-179.75-0.25, 179.75+0.25)
			}
			
			#timing: time slices
			timeLLNL <- data.frame(matrix(NA, ncol=4, nrow=8, dimnames=list(NULL, c("Run", "Slice", "Time", "Year"))))
			timeLLNL[, 1:3] <- expand.grid(c("start", "end"), c("first", "second"), c("hist", "fut"))[, 3:1]
			
			timeLLNL[1, 4] <- max(1950, simstartyr)
			timeLLNL[2, 4] <- min(2005, endyr)
			if(endyr > 2005){
				timeLLNL[3, 4] <- 2006
				timeLLNL[4, 4] <- min(2099, endyr)
			}
			timeLLNL[7, 4] <- max(2006, deltaFutureToSimStart_yr + simstartyr)
			timeLLNL[8, 4] <- min(2099, deltaFutureToSimStart_yr + endyr)
			if(deltaFutureToSimStart_yr + simstartyr < 2006){
				timeLLNL[5, 4] <- max(1950, deltaFutureToSimStart_yr + simstartyr)
				timeLLNL[6, 4] <- 2006
			}
			
			#tags
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
			
			mmPerDay_to_cmPerMonth <- function(prcp_mmPerDay, startyear, endyear){
				DaysPerMonths <- rle(as.POSIXlt(seq(from=as.POSIXlt(paste0(startyear, "-01-01")), to=as.POSIXlt(paste0(endyear, "-12-31")), by="1 day"))$mon)$lengths
				return(prcp_mmPerDay / 10 * DaysPerMonths)
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
			
			get.netCDFcontent <- function(filepath, variable, unit, lat, lon, startyear, endyear){
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
			
			get_GCMdata <- function(gcm, scen, lat=lat, lon=lon, startyear, endyear){
				gcmFiles <- list.files(file.path(dir.ex.dat, as.character(scen)), pattern=as.character(gcm), full.names=TRUE)
				
				#Get precipitation data
				prcp <- mmPerDay_to_cmPerMonth(get.netCDFcontent(filepath=gcmFiles[grepl(fileVarTags[1], gcmFiles)], variable=varTags[1], unit="mm/d", lat=lat, lon=lon, startyear=startyear, endyear=endyear), startyear, endyear)
				
				#Get temperature data
				if(any(temp3 <- grepl(fileVarTags[3], gcmFiles)) && any(temp4 <- grepl(fileVarTags[4], gcmFiles))){
					tmin <- get.netCDFcontent(filepath=gcmFiles[temp3], variable=varTags[3], unit="C", lat=lat, lon=lon, startyear=startyear, endyear=endyear)
					tmax <- get.netCDFcontent(filepath=gcmFiles[temp4], variable=varTags[4], unit="C", lat=lat, lon=lon, startyear=startyear, endyear=endyear)
				} else if(any(temp2 <- grepl(fileVarTags[2], gcmFiles))){
					tmean <- get.netCDFcontent(filepath=gcmFiles[temp2], variable=varTags[2], unit="C", lat=lat, lon=lon, startyear=startyear, endyear=endyear)
					tmin <- tmax <- tmean
				}
				
				#Monthly weather time-series
				date <- as.POSIXlt(seq(from=as.POSIXlt(paste0(startyear, "-01-01")), to=as.POSIXlt(paste0(endyear, "-12-31")), by="1 month"))
				return(list(cbind(year=date$year + 1900, month=date$mon + 1, tmax, tmin, prcp)))
			}
			
			get.GDODCPUCLLNL <- function(i){
				.local <- function(i){
					#Identify index for site and scenario
					gcm <- reqGCMs[ig <- (i - 1) %% length(reqGCMs) + 1]
					rcps <- reqRCPsPerGCM[[ig]]
					downs <- reqDownscalingsPerGCM[[ig]]
					lon <- locations[il <- (i - 1) %/% length(reqGCMs) + 1, 1]
					lat <- locations[il, 2]
					site_id <- dbW_iSiteTable[dbW_iSiteTable[, "Label"] == locations[il, 3], "Site_id"]		
					if(!be.quiet && i %% 1000 == 1) print(paste(i, "th extraction of GDO-DCP-UC-LLNL at", Sys.time(), "for", gcm, "(", paste(rcps, collapse=", "), ") at", lon, lat))
					
					if(lat >= bbox$lat[1] && lat <= bbox$lat[2] && lon >= bbox$lon[1] && lon <= bbox$lon[2]){#Data Bounding Box
						#Scenario monthly weather time-series
						scen.monthly <- matrix(data=vector("list", length=2*(1 + length(rcps))), ncol=2, dimnames=list(c("Current", as.character(rcps)), c("hist", "fut")))
						for(isc in 1:nrow(scen.monthly)){ #Get GCM data for each scenario and time slice
							scen <- c("historical", as.character(rcps))[isc]
							
							if(isc == 1){ #Slice past: 1950-2005
								if(!is.na(temp <- timeLLNL[1, 4]) && !all(downs == "raw")) {# past slice for 'historic' data; 'raw' downscaling doesn't need "historical" scenario
									scen.monthly[isc, 1] <- get_GCMdata(gcm=gcm, scen=scen, lat=lat, lon=lon, startyear=temp, endyear=timeLLNL[2, 4])
								}
								if(!is.na(temp <- timeLLNL[5, 4])){ #past slice for 'future' scenario
									scen.monthly[isc, 2] <- get_GCMdata(gcm=gcm, scen=scen, lat=lat, lon=lon, startyear=temp, endyear=timeLLNL[6, 4])
								}
							} else { #Slice future: 2006-2099
								if(!is.na(temp <- timeLLNL[3, 4])){ #future slice for 'historic' data
									scen.monthly[isc, 1] <- get_GCMdata(gcm=gcm, scen=scen, lat=lat, lon=lon, startyear=temp, endyear=timeLLNL[4, 4])
								}
								if(!is.na(temp <- timeLLNL[7, 4])){ #future slice for 'future' scenario
									scen.monthly[isc, 2] <- get_GCMdata(gcm=gcm, scen=scen, lat=lat, lon=lon, startyear=temp, endyear=timeLLNL[8, 4])
								}
							}
						}
						
						#Observed historic daily weather from weather database
						obs.hist.daily <- Rsoilwat::dbW_getWeatherData(Site_id=site_id, startYear=simstartyr, endYear=endyr, Scenario=climate.ambient)
						obs.hist.monthly <- get_monthlyTimeSeriesFromDaily(dailySW=obs.hist.daily)
						
						wdataOut <- list()
						for(ir in seq_along(rcps)){ #Apply downscaling for each RCP
							if(!all(downs == "raw")){
								scen.hist.monthly <- scen.monthly[1, 1][[1]]
								if(!is.null(scen.monthly[1 + ir, 1][[1]])) scen.hist.monthly <- rbind(scen.hist.monthly, scen.monthly[1 + ir, 1][[1]])
							}
							scen.fut.monthly <- scen.monthly[1 + ir, 2][[1]]
							if(!is.null(scen.monthly[1, 2][[1]])) scen.fut.monthly <- rbind(scen.monthly[1, 2][[1]], scen.fut.monthly)
							
							types <- list()
							if("raw" %in% downs){
								scen.fut.daily <- downscale.raw(obs.hist.daily, obs.hist.monthly, scen.fut.monthly)
								scenario_id <- dbW_iScenarioTable[dbW_iScenarioTable[, "Scenario"] == paste("raw", rcps[ir], gcm, sep="."), "id"]
								data_blob <- paste0("x'",paste0(memCompress(serialize(scen.fut.daily,NULL),type="gzip"),collapse = ""),"'",sep="")
								types[[length(types)+1]] <- list(Site_id=site_id, Scenario_id=scenario_id, weatherData=data_blob)
							}
							if("delta" %in% downs){
								scen.fut.daily <- downscale.delta(obs.hist.daily, scen.hist.monthly, scen.fut.monthly)
								scenario_id <- dbW_iScenarioTable[dbW_iScenarioTable[, "Scenario"] == paste("delta", rcps[ir], gcm, sep="."), "id"]
								data_blob <- paste0("x'",paste0(memCompress(serialize(scen.fut.daily,NULL),type="gzip"),collapse = ""),"'",sep="")
								types[[length(types)+1]] <- list(Site_id=site_id, Scenario_id=scenario_id, weatherData=scen.fut.daily)
							}
							if("hybrid-delta" %in% downs){
								scen.fut.daily <- downscale.deltahybrid(obs.hist.daily, obs.hist.monthly, scen.hist.monthly, scen.fut.monthly)
								scenario_id <- dbW_iScenarioTable[dbW_iScenarioTable[, "Scenario"] == paste("hybrid-delta", rcps[ir], gcm, sep="."), "id"]
								data_blob <- paste0("x'",paste0(memCompress(serialize(scen.fut.daily,NULL),type="gzip"),collapse = ""),"'",sep="")
								types[[length(types)+1]] <- list(Site_id=site_id, Scenario_id=scenario_id, weatherData=scen.fut.daily)
							}
							wdataOut[[ir]] <- types
						}
						save(wdataOut, file=file.path(dir.out, "temp", gcm, paste("GDO-DCP-UC-LLNL_", i, ".Rdata", sep="")))
						res <- i
					} else {
						stop(paste(i, "th extraction of GDO-DCP-UC-LLNL at", Sys.time(), "at", lon, lat, ": outside of bounding data box"))
						break
					}
					
					return(res)
				}
				
				res <- if(inherits(try(.local(i), silent=FALSE), "try-error")) NULL else i	
				return(res)
			}
			
			#get data from netCDF files
			if(parallel_runs && parallel_init){
				#call the simulations depending on parallel backend
				list.export <- c("dir.ex.dat", "get.GDODCPUCLLNL", "reqGCMs", "reqRCPsPerGCM", "reqDownscalingsPerGCM", "locations", "get.netCDFcontent", "get_GCMdata", "get.TimeIndices", "mmPerDay_to_cmPerMonth", "nc_getByCoords", "whereNearest", "fileVarTags", "varTags", "be.quiet", "dbWeatherDataFile", "climate.ambient",  "dbW_iSiteTable", "dbW_iScenarioTable",
						"bbox", "timeLLNL", "simstartyr", "endyr", "get_monthlyTimeSeriesFromDaily", "downscale.raw", "downscale.delta", "downscale.deltahybrid")
				if(identical(parallel_backend, "mpi")) {
					exportObjects(list.export)
					mpi.bcast.cmd(library(ncdf4, quietly = TRUE))
					
					i_Done <- mpi.applyLB(x=1:requestN, fun=get.GDODCPUCLLNL)
					
					mpi.bcast.cmd(rm(list=ls(all=TRUE)))
					mpi.bcast.cmd(gc())
				} else if(identical(parallel_backend, "snow")) {
					snow::clusterExport(cl, list.export)
					snow::clusterEvalQ(cl, library(ncdf4, quietly = TRUE))
					
					i_Done <- snow::clusterApplyLB(cl, x=1:requestN, fun=get.GDODCPUCLLNL)
					
					snow::clusterEvalQ(cl, rm(list=ls(all=TRUE)))
					snow::clusterEvalQ(cl, gc())
				} else if(identical(parallel_backend, "multicore")) {
					i_Done <- foreach(i=1:requestN, .combine="c", .inorder=FALSE, .errorhandling="remove") %dopar% {
						Rsoilwat::dbW_setConnection(dbFilePath=dbWeatherDataFile)
						temp <- get.GDODCPUCLLNL(i)
						Rsoilwat::dbW_disconnectConnection()
						return(temp)
					}
				}
			} else {
				i_Done <- foreach(i=1:requestN, .combine="c", .inorder=FALSE, .errorhandling="remove") %do% get.GDODCPUCLLNL(i)
			}
			
			require(Rsoilwat)
			Rsoilwat::dbW_setConnection(dbFilePath=dbWeatherDataFile)
			files <- list.files(path=file.path(dir.out, "temp"), pattern="GDO-DCP-UC-LLNL", recursive=TRUE, include.dirs = FALSE, no..=TRUE)
			if(length(files) != 0) {
				for (i in 1:length(files)) {
					wdata <- load(file.path(dir.out, "temp",files[i]))
					for(j in 1:length(wdata)) {
						for(l in 1:length(wdata[[j]])) {
							Rsoilwat:::dbW_addWeatherDataNoCheck(Site_id=wdata[[j]][[l]]$Site_id, Scenario_id=wdata[[j]][[l]]$Scenario_id, weatherData=wdata[[j]][[l]]$weatherData)
						}
					}
				}
			}
			Rsoilwat::dbW_disconnectConnection()
			
			i_Done <- sort(unlist(i_Done))
			save(i_Done, file=file.path(dir.sw.dat, "extractionGDODCPUCLLNL.RData"))
			
			if(length(i_ToDo <- if(length(i_Done) > 0) i_AllToDo[-i_Done] else i_AllToDo) > 0){
				warning(paste(length(i_ToDo), "sites didn't extract climate scenario information by 'ExtractClimateChangeScenarios_CMIP3/5_BCSD_GDODCPUCLLNL'"))
				failedLocations_GDODCPUCLLNL <- locations[temp <- unique((i_ToDo - 1) %/% length(reqGCMs) + 1), ]
				include_YN_updateFailed <- include_YN
				include_YN_updateFailed[include_YN > 0][temp] <- 0
				save(failedLocations_GDODCPUCLLNL, include_YN_updateFailed, file=file.path(dir.in, "failedLocations_GDODCPUCLLNL.RData"))
				SWRunInformation_updateFailed <- cbind(SWRunInformation, include_YN_updateFailed=include_YN_updateFailed)
				write.csv(SWRunInformation_updateFailed, file=file.path(dir.in, paste0("failedLocationsUpdated_GDODCPUCLLNL_", datafile.SWRunInformation)))
			}
			
			rm(locations, requestN, i_climCond, i_Done, idLocs, icols, fileVarTags, varTags, timeLLNL)
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
