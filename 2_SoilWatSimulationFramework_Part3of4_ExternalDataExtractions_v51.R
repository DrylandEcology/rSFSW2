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

	if(!require("compiler", quietly=TRUE)) {
		tryCatch(install.packages("compiler", repos=url.Rrepos, lib=dir.libraries), warning=function(w) { print(w); print("'compiler' failed to install"); stop("Stopping") })
		stopifnot(require("compiler", quietly=TRUE))
	}
	
	useRCurl <- FALSE
}

if(exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_NEX_USA){
	useRCurl <- FALSE
	if (!require(RCurl, quietly=TRUE)) {
		tryCatch(install.packages("RCurl", repos=url.Rrepos, lib=dir.libraries), warning=function(w) { print(w); print("RCurl failed to install, using 'download.files' instead") })
	}
	if (require(RCurl, quietly=TRUE)) useRCurl <- TRUE
}



#--------------------------------------------------------------------------------------------------#
#------Load additional parameters and functions for data from the Lawrence Livermore National Lab and from USGS NEX

if(	exinfo$GDODCPUCLLNL || exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_NEX_USA){
	stopifnot(getCurrentWeatherDataFromDatabase, getScenarioWeatherDataFromDatabase)
	
	dbW_setConnection(dbFilePath=dbWeatherDataFile)
	dbW_iSiteTable <- dbW_getSiteTable()
	dbW_iScenarioTable <- dbW_getScenariosTable()
	
	climScen <- data.frame(matrix(unlist(strsplit(temp <- climate.conditions[!grepl(climate.ambient, climate.conditions)], split=".", fixed=TRUE)), ncol=4, byrow=TRUE), stringsAsFactors=FALSE)
	climScen$imap_todbW <- match(temp, table=dbW_iScenarioTable$Scenario, nomatch=0)
	dbW_iScenarioTable[, "Scenario"] <- tolower(dbW_iScenarioTable[, "Scenario"])
	reqGCMs <- unique(climScen[, 4])
	reqRCPs <- unique(climScen[, 3])
	reqRCPsPerGCM <- lapply(reqGCMs, FUN=function(x) unique(climScen[x == climScen[, 4], 3]))
	reqDownscalingsPerGCM <- lapply(reqGCMs, FUN=function(x) unique(climScen[x == climScen[, 4], 1]))
	
	for(i in seq_along(reqGCMs)) {
		dir.create2(file.path(dir.out.temp, reqGCMs[i]), showWarnings=FALSE, recursive=TRUE)
	}


	#---Downscaling/bias-correction functions	
	#Helper functions
	in_GMC_box <- compiler::cmpfun(function(xy, lats, longs, i_use) {
		!i_use & (xy[, 1] >= longs[1] & xy[, 1] <= longs[2]) & (xy[, 2] >= lats[1] & xy[, 2] <= lats[2])
	})

	unique_times <- compiler::cmpfun(function(timeSlices, slice) {
		starts <- na.exclude(timeSlices$Year[timeSlices$Slice == slice & timeSlices$Time == "start"])
		ends <- na.exclude(timeSlices$Year[timeSlices$Slice == slice & timeSlices$Time == "end"])
		temp <- lapply(seq_along(starts), function(x) starts[x]:ends[x])
		temp1 <- vector("integer", length = 0)
		for (it in seq_along(temp)) temp1 <- union(temp1, temp[[it]])
		n <- 1 + length(temp2 <- which(diff(temp1) > 1))
		temp2 <- c(1, 1 + temp2, length(temp1) + 1)
		res <- matrix(NA, nrow = n, ncol = 2)
		for (it in 1:n) res[it, ] <- c(temp1[temp2[it]], temp1[temp2[it+1] - 1])
	
		res
	})

	useSlices <- compiler::cmpfun(function(getYears, timeSlices, run, slice) {
		res <- rep(FALSE, length = nrow(getYears[[slice]]))
		temp <- timeSlices$Year[timeSlices$Run == run & timeSlices$Slice == slice]
		if (!anyNA(temp)) {
			istart <- findInterval(temp[1], getYears[[slice]][, 1], rightmost.closed = FALSE, all.inside = FALSE)
			iend <- findInterval(temp[2], getYears[[slice]][, 2], rightmost.closed = FALSE, all.inside = FALSE)
			res[istart:iend] <- TRUE
		}
		
		res
	})


	#' Error function
	#' @references See among examples of ?Normal
	erf <- compiler::cmpfun(function(x) 2 * pnorm(x * sqrt(2)) - 1)
	
	#' Stretch the values
	#'
	#' Values above the mean of \code{x} are made larger and 
	#' values below the mean are made smaller - each by \code{lambda * dist(x, mean(x))}.
	stretch_values <- compiler::cmpfun(function(x, lambda = 0) {
		(1 + lambda) * x - lambda * mean(x)
	})

	
	#' Additive Precipitation adjustment by a delta value
	#' 
	#' Additive Precipitation adjustment by a delta value
	#'
	#' Provide one of the arguments \code{addDelta} or \code{deltaPerEvent}, but not both.
	#' The value of \code{addDelta} will be evenly split up among \code{ind_events}.
	#'
	#' @param data A numeric vector. Daily values of precipitation.
	#' @param ind_events A logical or integer vector of the same length as \code{data} or \code{NULL}. If logical, then TRUE/FALSE for each element/day of \code{data} whether it precipitates or not. If integer, then the indices of \code{data} on which it precipitates. If \code{NULL}, then it will be calculated as \code{data > 0}.
	#' @param addDelta A numeric value or \code{NULL}. The total amount of precipitation that is to be added/removed from \code{data[ind_events]} together (divided among events), i.e., it requires the same unit as \code{data}.
	#' @param deltaPerEvent A numeric vector of the length equal to \code{sum(ind_events)} or \code{NULL}. The daily amount of precipitation that is to be added/removed from each \code{data[ind_events]}, i.e., it requires the same unit as \code{data}.
	#'
	#' @return A list with two elements
	#'	\describe{
	#'		\item{data}{A copy of \code{data} with adjusted values.}
	#'		\item{PPT_to_remove}{The total amount of precipitation that couldn't be removed from \code{data} due to lack of precipitation.}
	#'	}
	add_delta_to_PPT <- compiler::cmpfun(function(data, ind_events = NULL, addDelta = NULL, deltaPerEvent = NULL) {
		stopifnot(xor(is.null(deltaPerEvent), is.null(addDelta)))
		
		if (is.null(ind_events)) ind_events <- data > 0
		if (!is.null(addDelta)) {
			elems_N <- if(is.logical(ind_events)) sum(ind_events) else length(ind_events)
			deltaPerEvent <- rep(addDelta[1] / elems_N, elems_N)
		}
		StillToSubtract <- 0
		
		if (all(deltaPerEvent > 0)) { # All deltas are additions -> no problem
			data[ind_events] <- data[ind_events] + deltaPerEvent
		} else { # There are subtractions -> check that all adjusted precipitation days > 0
			newRainyValues <- data[ind_events] + deltaPerEvent
			posRainyDays <- newRainyValues >= 0
			
			if (all(posRainyDays)) { # all ok
				data[ind_events] <- newRainyValues
			} else { # Some rainy days would now be negative; this is not ok
				negRainyDays <- !posRainyDays
				data[ind_events][posRainyDays] <- newRainyValues[posRainyDays]
				data[ind_events][negRainyDays] <- 0
				StillToSubtract <- sum(newRainyValues[negRainyDays]) # 'StillToSubtract' is negative
				ppt_avail <- sum(data[ind_events])
				
				if (ppt_avail > 0) {
					# There is precipitation of the already adjusted rainy days from which to subtract
					temp <- Recall(data = data, ind_events = data > 0, addDelta = StillToSubtract)
					data <- temp$data
					StillToSubtract <- temp$PPT_to_remove
				}
			}
		}
		
		list(data = data, PPT_to_remove = StillToSubtract)
	})
	
	#' Adds/removes elements of data
	#'
	#' Adds/removes elements of data until \code{identical(length(data), targetLength)}
	#'
	#' Removal of elements is by random sampling. Addition of elements is by randomly
	#' placing them within the sequence of days of \code{data} and them randomly
	#' assigning values of the original \code{data} with replacement.
	#'
	#' @param data A numeric vector. Daily values of precipitation.
	#' @param targetLength An integer value.
	#'
	#' @return A copy of \code{data} with adjusted length.
	fix_PPTdata_length <- compiler::cmpfun(function(data, targetLength) {
		targetLength <- as.integer(targetLength)
		stopifnot(targetLength >= 0)
		
		Ndiff <- length(data) - targetLength
		absNdiff <- abs(Ndiff)
		
		if (absNdiff > 0) {
			if (Ndiff > 0) {
				# remove random days
				ids <- sample(x = length(data), size = absNdiff, replace = FALSE)
				data <- data[-ids]
			} else {
				# Imputation
				# add random days with randomly sampled precipitation
				ids <- sample(x = targetLength, size = absNdiff, replace = FALSE)
				temp <- rep(0, targetLength)
				temp[-ids] <- data
				temp[ids] <- sample(x = data, size = absNdiff, replace = TRUE)
				data <- temp
			}
		}
		
		data
	})
	
	#' @param data_N An integer value. The number of days of the precipitation record to consider.
	#' @param this_newPPTevent_N An integer value. The number of days which will received 'spill-over' precipitation.
	#' @param sigmaN An integer value. The multiplicator of \code{this_newPPTevent_N} to determine the range of days to consider.
	#' @param this_i_extreme. An integer value. The index indicating for which day in the precipitation record, precipitation is removed and redistributed.
	#' @param this_pptToDistribute. An integer value. The amount of precipitation that was removed from day \code{this_i_extreme} and is now redistributed to other days.
	calc_Days_withLoweredPPT <- compiler::cmpfun(function(data_N, this_newPPTevent_N, sigmaN, this_i_extreme, this_pptToDistribute) {
		this_newPPTevent_N <- max(0L, as.integer(this_newPPTevent_N))
		
		#Randomly select days within ± sigmaN days from a normal distribution		
		temp <- (-sigmaN * this_newPPTevent_N):(sigmaN * this_newPPTevent_N)
		xt <- temp[this_i_extreme + temp > 0]
		this_xt <- this_i_extreme + xt
		xt <- xt[1 <= this_xt & this_xt <= data_N] #do not select days from previous or next year
		probs <- dnorm(x = xt, mean = 0, sd = this_newPPTevent_N)
		probs[which.max(probs)] <- 0
		temp <- sample(x = xt, size = this_newPPTevent_N, replace = FALSE, prob = probs)
		dayDelta <- temp[.Internal(order(na.last = TRUE, decreasing = FALSE, abs(temp)))]
		
		#Distribute PPT to add among the selected days with a linear decay function
		newDays <- this_i_extreme + dayDelta
		temp <- 1 / abs(dayDelta)
		newPPT <- this_pptToDistribute * temp / sum(temp)
		
		list(newDays = newDays, newPPT = newPPT)
	})

	#' Check that data are within range of normal distribution
	#'
	#' @param data A numeric vector. Daily values of temperature.
	#' @param sigmaN An integer value. A multiplicator of \code{sd}.
	test_sigmaNormal <- compiler::cmpfun(function(data, sigmaN = 6){
		md <- mean(data)
		sdd <- sd(data) * sigmaN
		stopifnot(data < md + sdd, data > md - sdd)
	})

	
	#' Check that data are within range of an approximated gamma distribution
	#'
	#' @param data A numeric vector. Daily values of precipitation.
	#' @param sigmaN An integer value. A multiplicator of \code{sd}.
	#' @references Choi, S. C., and R. Wette. 1969. Maximum Likelihood Estimation of the Parameters of the Gamma Distribution and Their Bias. Technometrics 11:683-690.
	#' @references http://en.wikipedia.org/wiki/Gamma_distribution#Maximum_likelihood_estimation
	test_sigmaGamma <- compiler::cmpfun(function(data, sigmaN = 6){
		tempD <- data[data > 0]
		
		if (length(tempD) >= 2 && sd(tempD) > 0) {
			tempM <- mean(tempD)
			temp <- log(tempM) - mean(log(tempD))
			# Approximate shape and scale instead of very slow call: g <- MASS::fitdistr(data, "gamma")
			gshape <- (3 - temp + sqrt((temp - 3)^2 + 24 * temp)) / (12 * temp)
			gscale <- tempM / gshape
			stopifnot(data < qgamma(erf(sigmaN / sqrt(2)), shape = gshape, scale = gscale))
		}
	})
	
	
	#' @param data A numeric vector. Daily values of precipitation.
	#' @param dailyPPTceiling A numeric value. The maximum value of daily precipitation. Values above this limit will be removed and redistributed to other days.
	#' @param do_checks A logical value. See details.
	#' @param sigmaN An integer value. A multiplicator of \code{sd} for data checks.
	#' @param mfact A numeric value. See details.
	#' @details IF \code{TRUE} and any daily precipitation is equal or larger than \code{mfact * dailyPPTceiling}, then the code will error out.
	controlExtremePPTevents <- compiler::cmpfun(function(data, dailyPPTceiling, sigmaN, do_checks = FALSE, mfact = 10) {
		if (do_checks) stopifnot(data < mfact * dailyPPTceiling) #something went wrong, e.g., GCM data is off; (10 / 1.5 * dailyPPTceiling) -> dailyPPTtoExtremeToBeReal  #if more than 1000% of observed value then assume that something went wrong and error out
		
		data_N <- length(data)
		i_extreme <- which(data > dailyPPTceiling)
		irep <- 0
		
		while (length(i_extreme) > 0 && irep < 30) {
			# limit calls: if too many wet days, then not possible to distribute all the water!
			newValues <- dailyPPTceiling * runif(n = length(i_extreme), min = 0.9, max = 1)
			pptToDistribute <- data[i_extreme] - newValues
			data[i_extreme] <- newValues
			newPPTevent_N <- ceiling(pptToDistribute / dailyPPTceiling)
			stopifnot(sum(newPPTevent_N) <= data_N) 	# more days with dailyPPTceiling precipitation would be necessary than are available
			
			for (i in seq_along(i_extreme)) {
				newPPTevents <- calc_Days_withLoweredPPT(
					data_N = data_N, this_newPPTevent_N = newPPTevent_N[i], sigmaN = sigmaN,
					this_i_extreme = i_extreme[i], this_pptToDistribute = pptToDistribute[i])
						
				data[newPPTevents$newDays] <- data[newPPTevents$newDays] + newPPTevents$newPPT
			}
			
			# prepare for next iteration in case a day with previous ppt got too much ppt
			i_extreme <- which(data > dailyPPTceiling)
			irep <- irep + 1
		}
		
		if (do_checks) test_sigmaGamma(data = data, sigmaN)
		
		data
	})

	#' Add/multiply deltas to historic daily data to generate future daily SoilWat-formatted weather.
	#' Used by \code{downscale.raw}, \code{downscale.delta}, and \code{downscale.deltahybrid}
	applyDeltas <- compiler::cmpfun(function(obs.hist.daily, obs.hist.monthly, delta_ts, ppt_fun, sigmaN = 6, do_checks=FALSE){
		dailyPPTceiling <- 1.5 * max(sapply(obs.hist.daily, FUN=function(obs) max(obs@data[,4]))) #Hamlet et al. 2010: "an arbitrary ceiling of 150% of the observed maximum precipitation value for each cell is also imposed by “spreading out” very large daily precipitation values into one or more adjacent days"

		res <- try(lapply(obs.hist.daily, FUN=function(obs) {
					month <- as.POSIXlt(paste(obs@year, obs@data[, "DOY"], sep="-"), format="%Y-%j")$mon + 1
					ydelta <- delta_ts[delta_ts[, "Year"] == obs@year, -(1:2)]
					tmax <- obs@data[, "Tmax_C"] + ydelta[month, "Tmax_C"]

					if(do_checks) test_sigmaNormal(data=tmax, sigmaN)

					tmin <- obs@data[, "Tmin_C"] + ydelta[month, "Tmin_C"]
					if(do_checks) test_sigmaNormal(data=tmin, sigmaN)

					ppt_data <- unlist(lapply(1:12, FUN=function(m) {
														im_month <- month == m
														m_ydelta <- ydelta[m, 3]
														m_data <- obs@data[im_month, "PPT_cm"]
														if(ppt_fun[m] == "*"){# multiply ppt
															res <- m_data * m_ydelta
														} else if(m_ydelta == 0){ #add ppt here and below: nothing to add
															res <- m_data
														} else if(any(i_rainyDays <- m_data > 0)){ #there are rainy days in the historic record: add to those
															res <- add_delta_to_PPT(data=m_data, ind_events=i_rainyDays, addDelta=m_ydelta)$data
														} else { #there are no rainy days in the historic record
															if(m_ydelta > 0){ #we need rainy days in the historic record to add precipitation
																if(any(i_rainyMYears <- obs.hist.monthly[obs.hist.monthly[, "Month"] == m, "PPT_cm"] > 0)){ #sample from the same historic month in an other with rainy days instead
																	#Locate data of same month in other year
																	i_newYear <- which(i_rainyMYears)[which.min(abs(obs.hist.monthly[obs.hist.monthly[, "Month"] == m, "PPT_cm"][i_rainyMYears] - m_ydelta))]
																	newMonth <- as.POSIXlt(paste((newObs <- obs.hist.daily[i_newYear][[1]])@year, newObs@data[, "DOY"], sep="-"), format="%Y-%j")$mon + 1
																	newMonthData <- newObs@data[, "PPT_cm"][newMonth == m]
																	#Adjust data
																	newMonthData <- fix_PPTdata_length(data=newMonthData, targetLength=sum(im_month)) #adjust number of days in case we got a leap year February issue
																	res <- add_delta_to_PPT(newMonthData, newMonthData > 0, addDelta = m_ydelta)$data
																} else if(any(i_rainyMonth <- obs.hist.monthly[, "PPT_cm"] > 0)){ #no rainy day for this month in historic record: locate rainy days in any months from other years
																	#Locate data of any month in any year
																	i_newMYear <- which(i_rainyMonth)[which.min(abs(obs.hist.monthly[i_rainyMonth, "PPT_cm"] - m_ydelta))]
																	i_newYear <- which(obs.hist.monthly[i_newMYear, "Year"] == sort(unique(obs.hist.monthly[, "Year"])))
																	newMonth <- as.POSIXlt(paste((newObs <- obs.hist.daily[i_newYear][[1]])@year, newObs@data[, "DOY"], sep="-"), format="%Y-%j")$mon + 1
																	newMonthData <- newObs@data[, "PPT_cm"][newMonth == obs.hist.monthly[i_newMYear, "Month"]]
																	#Adjust data
																	newMonthData <- fix_PPTdata_length(data=newMonthData, targetLength=sum(im_month)) #adjust number of days in case we got a month with a different number of days
																	res <- add_delta_to_PPT(newMonthData, newMonthData > 0, addDelta = m_ydelta)$data
																} else {
																	stop(paste("no rainy day in historic record, but requested for the future prediction"))
																}
															} else {#there is no rain in the historic record, so we cannot remove any
																res <- rep(0, length(m_data))
															}
														}
														return(res)
													}))
					
					ppt <- controlExtremePPTevents(data=ppt_data, dailyPPTceiling, do_checks=do_checks, sigmaN = sigmaN)			

					new("swWeatherData", data=round(data.matrix(cbind(obs@data[, "DOY"], tmax, tmin, ppt), rownames.force=FALSE), 2), year=obs@year)
				}), silent=TRUE)

		return(res)
	})

	

	#' Add/multiply deltas to historic daily precipitation to generate future daily precipitation without checks
	#' 
	#' @param m An integer vector. Each element corresponds to a day (i.e., length(m) is 365 or 366 days) and the values are the number of the month.
	#' @param data A numeric vector. Precipitation of each day.
	#' @param ydelta A numeric vector. Delta values for each day. If computed deltas are monthly, then they must be repeated for each day before passed as argument to this function.
	#' @param add_days A logical vector. \code{TRUE} for each day for which \code{ydelta} is applied additively.
	#' @param mult_days A logical vector. \code{TRUE} for each day for which \code{ydelta} is applied multiplicatively.
	#' @return A copy of \code{data} with adjusted values.
	applyPPTdelta_simple <- compiler::cmpfun(function(m, data, ydelta, add_days, mult_days) {
		ppt <- rep(0, length(data))
		ievents <- data > 0

		# additive delta
		if (any(add_days)) {
			# Spread monthly 'delta' amount of PPT to each daily precip event
			events_per_month <- tapply(as.integer(ievents), m, sum)[m]
			itemp <- add_days & ievents
			ppt[itemp] <- data[itemp] + ydelta[itemp] / events_per_month[itemp]
			# Simple correction for negative precipitation that can arise from subtractive deltas
			negppt <- ppt[itemp] < 0
			if (any(negppt)) ppt[itemp][negppt] <- 0
		}
		# multiplicative delta
		if (any(mult_days)) {
			itemp <- mult_days & ievents
			ppt[itemp] <- data[itemp] * ydelta[itemp]
		}

		ppt
	})

	#' Add/multiply deltas to historic daily precipitation to generate future daily precipitation with checks
	#'
	#' @inheritParams applyPPTdelta_simple
	#' @inheritParams downscale
	#' @return A list with two elements
	#'	\describe{
	#'		\item{data}{A copy of \code{data} with adjusted values.}
	#'		\item{PPT_to_remove}{The total amount of precipitation that couldn't be removed from \code{data} due to lack of precipitation.}
	#'	}
	applyPPTdelta_detailed <- compiler::cmpfun(function(m, data, ydelta, add_days, mult_days, daily, monthly) {
		ppt <- rep(0, length(data))
		PPT_to_remove <- 0
		
		# multiplicative delta
		if (any(mult_days)) {
			ppt[mult_days] <- data[mult_days] * ydelta[mult_days]
		}

		# additive delta
		if (any(add_days) && sum(abs(ydelta[add_days])) > 0) {
			ievents <- data > 0
			ievents_add <- ievents & add_days
			
			if (any(ievents_add)) {
				# there is precipitation among the days with additive delta: add to/subtract from those
				eventsN_add_per_month <- tapply(as.integer(ievents_add), m, sum)[m]
				temp <- add_delta_to_PPT(data = data[add_days],
									ind_events = ievents_add[add_days],
									deltaPerEvent = (ydelta / eventsN_add_per_month)[ievents_add])
				ppt[add_days] <- temp[["data"]]
				
				if (temp[["PPT_to_remove"]] < 0) {
					# there was not enough precipitation among the additive days; attempt to remove precipitation from all days
					temp <- add_delta_to_PPT(data = ppt,
										ind_events = ievents,
										addDelta = temp[["PPT_to_remove"]])
					ppt <- temp[["data"]]
					PPT_to_remove <- PPT_to_remove + temp[["PPT_to_remove"]]
				}
			} else {
				# there are no precip days in the data with additive delta
				idelta_pos <- ydelta > 0 & ievents_add
				idelta_neg <- ydelta < 0 & ievents_add

				if (any(idelta_pos)) {
					# there are positive deltas: sample days with precipitation events from the historic record (to preserve precipitation event distribution) and adjust the amounts
					ipos_months <- m[idelta_pos]
					
					for (im in unique(ipos_months)) {
						this_month_im <- monthly[, "Month"] == im
						month_precip <- monthly[, "PPT_cm"] > 0 
						this_month_precip <- month_precip & this_month_im
						precip_target <- (temp <- ydelta[idelta_pos][ipos_months == im])[1] / length(temp)

						if (any(this_month_precip)) {
							# locate data from the same historic month 'im' from a different year with the most similar monthly PPT
							itemp <- this_month_precip
						} else {
							# this month 'im' has no precipitation in all years
							if (any(month_precip)) {
								# locate data from any month from any year with the most similar monthly PPT
								itemp <- month_precip
							} else {
								stop(paste("Historic record has no days with precipitation, but requested for the future."))
							}
						}
						
						i_newYear <- monthly[itemp, "Year"][which.min(abs(monthly[itemp, "PPT_cm"] - precip_target))]
						newMonthData <- daily[[as.character(i_newYear)]]@data[m == im, "PPT_cm"]
						
						# Adjust data for this month
						these_days <- m == im
						newMonthData <- fix_PPTdata_length(data = newMonthData, targetLength = sum(these_days)) #adjust number of days in case we got a leap year February issue
						temp <- add_delta_to_PPT(data = newMonthData, ind_events = newMonthData > 0,
									addDelta = precip_target - sum(newMonthData))
						ppt[these_days] <- temp[["data"]]
						PPT_to_remove <- PPT_to_remove + temp[["PPT_to_remove"]]
					}
				}
				
				if (any(idelta_neg)) {
					# there are negative deltas and no precipitation among additive days: attempt to remove from all days otherwise return and attempt to remove from all years
					StillToSubtract <- sum(ydelta[idelta_neg]) # 'StillToSubtract' is negative

					# attempt to remove precipitation from all days
					temp <- add_delta_to_PPT(data = ppt,
										ind_events = ievents,
										addDelta = StillToSubtract)
					ppt <- temp[["data"]]
					PPT_to_remove <- PPT_to_remove + temp[["PPT_to_remove"]]
				}
			}
		}
		
		list(data = ppt, PPT_to_remove = PPT_to_remove)
	})
	

	applyDelta_oneYear <- compiler::cmpfun(function(obs, delta_ts, ppt_fun, daily, monthly, ppt_type = c("simple", "detailed"), dailyPPTceiling, sigmaN, do_checks) {
		ppt_type <- match.arg(ppt_type)

		month <- 1 + as.POSIXlt(seq(ISOdate(obs@year, 1, 1), ISOdate(obs@year, 12, 31), by = "day"))$mon
		ydeltas <- delta_ts[delta_ts[, "Year"] == obs@year, -(1:2)]
		add_days <- ppt_fun[month] == "+"
		mult_days <- !add_days
		PPT_to_remove <- 0
		
		tmax <- obs@data[, "Tmax_C"] + ydeltas[month, "Tmax_C"]
		if(do_checks) test_sigmaNormal(data=tmax, sigmaN)

		tmin <- obs@data[, "Tmin_C"] + ydeltas[month, "Tmin_C"]
		if(do_checks) test_sigmaNormal(data=tmin, sigmaN)
		
		if (ppt_type == "simple") {
			ppt <- applyPPTdelta_simple(m = month,
						data = obs@data[, "PPT_cm"],
						ydelta = ydeltas[month, "PPT_cm"],
						add_days = add_days, mult_days = mult_days)
						
		} else if (ppt_type == "detailed") {
			temp <- applyPPTdelta_detailed(m = month,
						data = obs@data[, "PPT_cm"],
						ydelta = ydeltas[month, "PPT_cm"],
						add_days = add_days, mult_days = mult_days,
						daily = daily, monthly = monthly)
			ppt <- temp[["data"]]
			PPT_to_remove <- temp[["PPT_to_remove"]]
			
			if(dailyPPTceiling > 0)
				ppt <- controlExtremePPTevents(data = ppt, 
							do_checks = do_checks,
							dailyPPTceiling = dailyPPTceiling,
							sigmaN = sigmaN)			
		}
	

		sw <- new("swWeatherData",
				data = round(data.matrix(cbind(obs@data[, "DOY"], tmax, tmin, ppt),
										rownames.force = FALSE),
							2),
				year = obs@year)
		list(sw = sw, PPT_to_remove = PPT_to_remove)
	})

	
	applyDeltas2 <- compiler::cmpfun(function(daily, monthly, years, delta_ts, ppt_fun, ppt_type = c("simple", "detailed"), dailyPPTceiling, sigmaN, do_checks = FALSE) {
		ppt_type <- match.arg(ppt_type)
		# daily_months <- 1 + as.POSIXlt(seq(ISOdate(years[1], 1, 1), ISOdate(years[length(years)], 12, 31), by = "day"))$mon

		sw_list <- list()
		totalPPT_to_remove <- 0
		
		for (i in seq_along(daily)) {
			temp <- applyDelta_oneYear(obs = daily[[i]],
						delta_ts = delta_ts, ppt_fun = ppt_fun, daily = daily, monthly = monthly,
						ppt_type = ppt_type, dailyPPTceiling = dailyPPTceiling, sigmaN = sigmaN, do_checks = do_checks)
			sw_list[[i]] <- temp[["sw"]]
			totalPPT_to_remove <- totalPPT_to_remove + temp[["PPT_to_remove"]]
		}
		
		if (totalPPT_to_remove < 0) {
			# Some years didn't have sufficient precipitation for the removal of the requested delta precipitation
			# Here, crude approach to remove this additional quantity by spreading it across all years
			
			daily2 <- dbW_weatherData_to_dataframe(sw_list)
			totalPPT <- sum(daily2[, "PPT_cm"])
			
			if (totalPPT > abs(totalPPT_to_remove)) {
				daily2[, "PPT_cm"] <- daily2[, "PPT_cm"] * (1 - abs(totalPPT_to_remove) / totalPPT)
			} else {
				warning(paste("Total site precipitation should be reduced on average by a further", round((abs(totalPPT_to_remove) - totalPPT) / length(daily), 2), "cm / year"))
				daily2[, "PPT_cm"] <- 0
			}
			
			sw_list <- dbW_dataframe_to_weatherData(daily2, years)
		}
	  	names(sw_list) <- years
		
		sw_list
	})


	
	#' Downscale and temporal disaggregation
	#' 
	#' @details Units are [degree Celsius] for temperature and [cm / day] or [cm / month], respectively, for precipitation
	#' 
	#' @param obs.hist.daily A list. Each element corresponds to one year of simstartyr:endyris and is an object of class \linkS4class{swWeatherData}.
	#' @param daily A list. Each element corresponds to one year of simstartyr:endyris and is an object of class \linkS4class{swWeatherData}.
	#' @param obs.hist.monthly A numeric matrix. Monthly time-series of observed weather calculated from \code{obs.hist.daily} for the years simstartyr:endyr.
	#' @param monthly A numeric matrix. Monthly time-series of observed weather calculated from \code{daily} for the years simstartyr:endyr.
	#' @param scen.hist.monthly A numeric matrix. Monthly time-series of scenario weather during the historic time period DScur_startyr:DScur_endyr
	#' @param scen.fut.monthly A numeric matrix. Monthly time-series of scenario weather during the projected time period DSfut_startyr:DSfut_endyr
	#' @param downscaling.options A named list.
	#' @param do_checks A logical value. If \code{TRUE} perform several sanity checks on the data.
	downscale <- function(obs.hist.daily, obs.hist.monthly, scen.fut.monthly, downscaling.options, do_checks = TRUE) {}

	

	#' Time periods for downscaling functions
	#' @inheritParams downscale
	downscale.periods <- compiler::cmpfun(function(obs.hist.daily, obs.hist.monthly, scen.hist.monthly = NULL, scen.fut.monthly = NULL, years = NULL, DScur_startyear = NULL, DScur_endyear = NULL, DSfut_startyear = NULL, DSfut_endyear = NULL) {
		# Time periods
		#	- historic observed period: simstartyr:endyr
		dyears <- sapply(obs.hist.daily, function(obs) obs@year)
		if (is.null(years)) years <- dyears
		startyear <- years[1]
		endyear <- years[length(years)]
		iuse_obs_hist_d <- dyears >= startyear & dyears <= endyear
		iuse_obs_hist_m <- obs.hist.monthly[, 1] >= startyear & obs.hist.monthly[, 1] <= endyear
		stopifnot(sum(iuse_obs_hist_m) == (endyear - startyear + 1) * 12)

		#	- historic training period: DScur_startyear:DScur_endyear
		if (!is.null(scen.hist.monthly)) {
			if (is.null(DScur_startyear)) DScur_startyear <- scen.hist.monthly[1, 1]
			if (is.null(DScur_endyear)) DScur_endyear <- scen.hist.monthly[nrow(scen.hist.monthly), 1]
			iuse_scen_hist_m <- scen.hist.monthly[, 1] >= DScur_startyear & scen.hist.monthly[, 1] <= DScur_endyear
			if (!(sum(iuse_scen_hist_m) == (DScur_endyear - DScur_startyear + 1) * 12)) {
				warning("downscale.periods: resulting record of 'scen.hist.monthly' covers only the years ", paste(range(scen.hist.monthly[iuse_scen_hist_m, 1]), collapse = "-"), " instead of the requested ", DScur_startyear, "-", DScur_endyear, immediate. = TRUE)
			}
		} else {
			DScur_startyear <- DScur_endyear <- iuse_scen_hist_m <- NULL
		}
		#	- future training period: DSfut_startyear:DSfut_endyear
		if (!is.null(scen.fut.monthly)) {
			if (is.null(DSfut_startyear)) DSfut_startyear <- scen.fut.monthly[1, 1]
			if (is.null(DSfut_endyear)) DSfut_endyear <- scen.fut.monthly[nrow(scen.fut.monthly), 1]
			iuse_scen_fut_m <- scen.fut.monthly[, 1] >= DSfut_startyear & scen.fut.monthly[, 1] <= DSfut_endyear
			if (!(sum(iuse_scen_fut_m) == (DSfut_endyear - DSfut_startyear + 1) * 12)) {
				warning("downscale.periods: resulting record of 'scen.fut.monthly' covers only the years ", paste(range(scen.fut.monthly[iuse_scen_fut_m, 1]), collapse = "-"), " instead of the requested ", DSfut_startyear, "-", DSfut_endyear, immediate. = TRUE)
			}
		} else {
			DSfut_startyear <- DSfut_endyear <- iuse_scen_fut_m <- NULL
		}

	
		# Return
		list(years = years, startyear = startyear, endyear = endyear,
			DScur_startyear = DScur_startyear, DScur_endyear = DScur_endyear,
			DSfut_startyear = DSfut_startyear, DSfut_endyear = DSfut_endyear,
			iuse_obs_hist_d = iuse_obs_hist_d, iuse_obs_hist_m = iuse_obs_hist_m,
			iuse_scen_hist_m = iuse_scen_hist_m, iuse_scen_fut_m = iuse_scen_fut_m)
	})

	#' Downscale with the 'direct approach'
	#' 
	#' See 'direct' approach in Lenderink et al. (2007)
	#'
	#' @inheritParams downscale
	#'
	#' @references Lenderink, G., A. Buishand, and W. van Deursen. 2007. Estimates of future discharges of the river Rhine using two scenario methodologies: direct versus delta approach. Hydrology and Earth System Sciences 11:1145-1159.
	#' @export
	downscale.raw <- compiler::cmpfun(function(obs.hist.daily, obs.hist.monthly, scen.fut.monthly, years = NULL, DScur_startyear = NULL, DScur_endyear = NULL, DSfut_startyear = NULL, DSfut_endyear = NULL, downscaling.options = list(applyPPT_type = "detailed", sigmaN = 6, PPTratioCutoff = 10), dailyPPTceiling, do_checks=TRUE) {
		# Time periods
		tp <- downscale.periods(obs.hist.daily, obs.hist.monthly, scen.hist.monthly = NULL, scen.fut.monthly, years, DScur_startyear, DScur_endyear, DSfut_startyear, DSfut_endyear)
		if (any(!tp$iuse_obs_hist_d)) obs.hist.daily <- obs.hist.daily[tp$iuse_obs_hist_d]
		if (any(!tp$iuse_obs_hist_m)) obs.hist.monthly <- obs.hist.monthly[tp$iuse_obs_hist_m, ]
		if (any(!tp$iuse_scen_fut_m)) scen.fut.monthly <- scen.fut.monthly[tp$iuse_scen_fut_m, ]
	
		# 1. Calculate mean monthly values in historic and future scenario values
		scen.fut.mean_tmax <- tapply(scen.fut.monthly[, "tmax"], INDEX = scen.fut.monthly[, "month"], mean, na.rm = TRUE)
		scen.fut.mean_tmin <- tapply(scen.fut.monthly[, "tmin"], INDEX = scen.fut.monthly[, "month"], mean, na.rm = TRUE)
		scen.fut.mean_ppt <- tapply(scen.fut.monthly[, "prcp"], INDEX = scen.fut.monthly[, "month"], sum, na.rm = TRUE)
		
		obs.hist.mean_tmax <- tapply(obs.hist.monthly[, "Tmax_C"], INDEX = obs.hist.monthly[, "Month"], mean, na.rm = TRUE)
		obs.hist.mean_tmin <- tapply(obs.hist.monthly[, "Tmin_C"], INDEX = obs.hist.monthly[, "Month"], mean, na.rm = TRUE)
		obs.hist.mean_ppt <- tapply(obs.hist.monthly[, "PPT_cm"], INDEX = obs.hist.monthly[, "Month"], sum, na.rm = TRUE)
		
		# 2. Calculate deltas between observed historic and future mean scenario values
				#	- Additive approach (Anandhi et al. 2011): Temp, close-to-zero PPT, small or very large PPT ratios
				#	- Multiplicative approach (Wang et al. 2014): PPT otherwise
		delta_ts <- matrix(NA, ncol=5, nrow=nrow(obs.hist.monthly), dimnames=list(NULL, c("Year", "Month", "Tmax_C", "Tmin_C", "PPT_cm")))
		delta_ts[, 1:2] <- obs.hist.monthly[, 1:2]
		ppt_fun <- rep("*", 12)

		# Deltas of monthly means
		delta_ts[, "Tmax_C"] <- scen.fut.mean_tmax - obs.hist.mean_tmax
		delta_ts[, "Tmin_C"] <- scen.fut.mean_tmin - obs.hist.mean_tmin
		delta_ppts <- scen.fut.mean_ppt / obs.hist.mean_ppt
		temp_add <- delta_ppts < 1 / (10 * downscaling.options[["PPTratioCutoff"]]) | delta_ppts > downscaling.options[["PPTratioCutoff"]]
		if (any(temp_add)) {
			ppt_fun[temp_add] <- "+"
			delta_ppts[temp_add] <- scen.fut.mean_ppt - obs.hist.mean_ppt
		}
		delta_ts[, "PPT_cm"] <- delta_ppts
		
		
		# 3. Apply deltas to historic daily weather
		try(applyDeltas2(daily = obs.hist.daily, monthly = obs.hist.monthly, 
				years = tp$years, delta_ts = delta_ts, ppt_fun = ppt_fun,
				ppt_type = downscaling.options[["applyPPT_type"]], dailyPPTceiling = dailyPPTceiling, sigmaN = downscaling.options[["sigmaN"]], do_checks = do_checks), silent = TRUE)
	})
	
	#' Downscale with the 'delta approach'
	#'
	#' @inheritParams downscale
	#'
	#' @references Hay, L. E., R. L. Wilby, and G. H. Leavesley. 2000. A comparison of delta change and downscaled gcm scenarios for three mountainous basins in the United States. Journal of the American Water Resources Association 36:387-397.
	#' @references Hamlet, A. F., E. P. Salathé, and P. Carrasco. 2010. Statistical downscaling techniques for global climate model simulations of temperature and precipitation with application to water resources planning studies. Chapter 4. Final Report for the Columbia Basin Climate Change Scenarios Project. Climate Impacts Group, Center for Science in the Earth System, Joint Institute for the Study of the Atmosphere and Ocean, University of Washington, Seattle, WA.
	#' @export
	downscale.delta <- compiler::cmpfun(function(obs.hist.daily, obs.hist.monthly, scen.hist.monthly, scen.fut.monthly, years = NULL, DScur_startyear = NULL, DScur_endyear = NULL, DSfut_startyear = NULL, DSfut_endyear = NULL, downscaling.options = list(applyPPT_type = "detailed", sigmaN = 6, PPTratioCutoff = 10), dailyPPTceiling, do_checks = TRUE) {
		# Time periods
		tp <- downscale.periods(obs.hist.daily, obs.hist.monthly, scen.hist.monthly, scen.fut.monthly, years, DScur_startyear, DScur_endyear, DSfut_startyear, DSfut_endyear)
		if (any(!tp$iuse_obs_hist_d)) obs.hist.daily <- obs.hist.daily[tp$iuse_obs_hist_d]
		if (any(!tp$iuse_obs_hist_m)) obs.hist.monthly <- obs.hist.monthly[tp$iuse_obs_hist_m, ]
		if (any(!tp$iuse_scen_hist_m)) scen.hist.monthly <- scen.hist.monthly[tp$iuse_scen_hist_m, ]
		if (any(!tp$iuse_scen_fut_m)) scen.fut.monthly <- scen.fut.monthly[tp$iuse_scen_fut_m, ]
		
		# 1. Calculate mean monthly values in historic and future scenario values
		scen.fut.mean_tmax <- tapply(scen.fut.monthly[, "tmax"], INDEX = scen.fut.monthly[, "month"], mean, na.rm = TRUE)
		scen.fut.mean_tmin <- tapply(scen.fut.monthly[, "tmin"], INDEX = scen.fut.monthly[, "month"], mean, na.rm = TRUE)
		scen.fut.mean_ppt <- tapply(scen.fut.monthly[, "prcp"], INDEX = scen.fut.monthly[, "month"], sum, na.rm = TRUE)

		scen.hist.mean_tmax <- tapply(scen.hist.monthly[, "tmax"], INDEX = scen.hist.monthly[, "month"], mean, na.rm = TRUE)
		scen.hist.mean_tmin <- tapply(scen.hist.monthly[, "tmin"], INDEX = scen.hist.monthly[, "month"], mean, na.rm = TRUE)
		scen.hist.mean_ppt <- tapply(scen.hist.monthly[, "prcp"], INDEX = scen.hist.monthly[, "month"], sum, na.rm = TRUE)

		
		# 2. Calculate deltas between historic and future mean scenario values
				#	- Additive approach (Anandhi et al. 2011): Temp, close-to-zero PPT, small or very large PPT ratios
				#	- Multiplicative approach (Wang et al. 2014): PPT otherwise
		delta_ts <- matrix(NA, ncol=5, nrow=nrow(obs.hist.monthly), dimnames=list(NULL, c("Year", "Month", "Tmax_C", "Tmin_C", "PPT_cm")))
		delta_ts[, 1:2] <- obs.hist.monthly[, 1:2]
		ppt_fun <- rep("*", 12)
		
		# Deltas of monthly means
		delta_ts[, "Tmax_C"] <- scen.fut.mean_tmax - scen.hist.mean_tmax
		delta_ts[, "Tmin_C"] <- scen.fut.mean_tmin - scen.hist.mean_tmin
		delta_ppts <- scen.fut.mean_ppt / scen.hist.mean_ppt
		temp_add <- delta_ppts < 1 / (10 * downscaling.options[["PPTratioCutoff"]]) | delta_ppts > downscaling.options[["PPTratioCutoff"]]

		if (any(temp_add)) {
			ppt_fun[temp_add] <- "+"
			delta_ppts[temp_add] <- scen.fut.mean_ppt - scen.hist.mean_ppt
		}
		delta_ts[, "PPT_cm"] <- delta_ppts
		
		
		# 3. Apply deltas to historic daily weather
		try(applyDeltas2(daily = obs.hist.daily, monthly = obs.hist.monthly, 
				years = tp$years, delta_ts = delta_ts, ppt_fun = ppt_fun,
				ppt_type = downscaling.options[["applyPPT_type"]], dailyPPTceiling = dailyPPTceiling, sigmaN = downscaling.options[["sigmaN"]], do_checks = do_checks), silent = TRUE)
	})
	
	#' Downscale with the 'delta-hybrid approach' old version (prior to May 2016)
	#' 
	#' Hybrid-delta downscaling developed by Hamlet et al. 2010 and Tohver et al. 2014.
	#' Applied, e.g., by Dickerson-Lange et al. 2014
	#'
	#' @inheritParams downscale
	#'
	#' @references Hamlet, A. F., E. P. Salathé, and P. Carrasco. 2010. Statistical downscaling techniques for global climate model simulations of temperature and precipitation with application to water resources planning studies. Chapter 4. Final Report for the Columbia Basin Climate Change Scenarios Project. Climate Impacts Group, Center for Science in the Earth System, Joint Institute for the Study of the Atmosphere and Ocean, University of Washington, Seattle, WA.
	#' @references Tohver, I.M., Hamlet, A.F. & Lee, S.-Y. (2014) Impacts of 21st-Century Climate Change on Hydrologic Extremes in the Pacific Northwest Region of North America. Journal of the American Water Resources Association, 50, 1461-1476.
	#' @references Anandhi, A., A. Frei, D. C. Pierson, E. M. Schneiderman, M. S. Zion, D. Lounsbury, and A. H. Matonse. 2011. Examination of change factor methodologies for climate change impact assessment. Water Resources Research 47:W03501.
	#' @references Dickerson-Lange, S. E., and R. Mitchell. 2014. Modeling the effects of climate change projections on streamflow in the Nooksack River basin, Northwest Washington. Hydrological Processes:doi: 10.1002/hyp.10012.
	#' @references Wang, L., and W. Chen. 2014. Equiratio cumulative distribution function matching as an improvement to the equidistant approach in bias correction of precipitation. Atmospheric Science Letters 15:1-6.
	#' @export
	downscale.deltahybrid <- compiler::cmpfun(function(obs.hist.daily, obs.hist.monthly, scen.hist.monthly, scen.fut.monthly, years = NULL, DScur_startyear = NULL, DScur_endyear = NULL, DSfut_startyear = NULL, DSfut_endyear = NULL, downscaling.options = list(sigmaN = 6, PPTratioCutoff = 10), do_checks=TRUE){
		#Functions
		eCDF.Cunnane <- function(x){
			na_N <- sum(is.na(x))
			x <- sort(x, na.last=NA)
			if(na_N > 0){#if there are NAs in the data, add them in the middle assuming missing values represent median conditions
				i_center <- ceiling(length(x)/2)
				x <- c(x[1:i_center], rep(NA, na_N), x[(i_center+1):length(x)])
			}
			n <- length(x)
			q <- (1:n - 0.4) / (n + 0.2) #Cunnane (1978)
			f <- splinefun(x=q, y=x, method="monoH.FC", ties=mean) #'hyman' produces too extreme large values
			return(list(x=x, q=q, fun=f))
		}

		# Time periods
		tp <- downscale.periods(obs.hist.daily, obs.hist.monthly, scen.hist.monthly, scen.fut.monthly, years, DScur_startyear, DScur_endyear, DSfut_startyear, DSfut_endyear)
		if (any(!tp$iuse_obs_hist_d)) obs.hist.daily <- obs.hist.daily[tp$iuse_obs_hist_d]
		if (any(!tp$iuse_obs_hist_m)) obs.hist.monthly <- obs.hist.monthly[tp$iuse_obs_hist_m, ]
		if (any(!tp$iuse_scen_hist_m)) scen.hist.monthly <- scen.hist.monthly[tp$iuse_scen_hist_m, ]
		if (any(!tp$iuse_scen_fut_m)) scen.fut.monthly <- scen.fut.monthly[tp$iuse_scen_fut_m, ]

 		#Delta time series values									
		delta_ts <- matrix(NA, ncol=5, nrow=nrow(obs.hist.monthly), dimnames=list(NULL, c("Year", "Month", "Tmax_C", "Tmin_C", "PPT_cm")))
		delta_ts[, 1:2] <- obs.hist.monthly[, 1:2]
		ppt_fun <- rep("*", 12)
		for(iv in 1:3){
			for(m in 1:12){
				# 1. Calculate eCDF of historic weather and of scenario using Cunnane plotting position (Cunnane 1978) following Hamlet et al. 2010
				#		- interpolation and extrapolation by splines (Dickerson-Lange et al. 2014) instead of standard anomalies (Hamlet et al. 2010)
				obs.hist.x <- obs.hist.monthly[rep(1:12, times=nrow(obs.hist.monthly) / 12) == m, 2 + iv]
				scen.hist.x <- scen.hist.monthly[rep(1:12, times=nrow(scen.hist.monthly) / 12) == m, 2 + iv]
				scen.fut.x <- scen.fut.monthly[rep(1:12, times=nrow(scen.fut.monthly) / 12) == m, 2 + iv]
				
				#NA values are assumed to represent median conditions
				if(any(i_na <- is.na(obs.hist.x))) obs.hist.x[i_na] <- median(obs.hist.x, na.rm=TRUE)
				if(any(i_na <- is.na(scen.hist.x))) scen.hist.x[i_na] <- median(scen.hist.x, na.rm=TRUE)
				if(any(i_na <- is.na(scen.fut.x))) scen.fut.x[i_na] <- median(scen.fut.x, na.rm=TRUE)
				
				#eCDFs
				obs.hist.ecdf <- eCDF.Cunnane(obs.hist.x)
				scen.hist.ecdf <- eCDF.Cunnane(scen.hist.x)
				scen.fut.ecdf <- eCDF.Cunnane(scen.fut.x)
				
				# 2. Adjust future scenario with quantile-based deltas from historic comparison for future scenario values with linear extrapolation
				#	- Additive approach (Anandhi et al. 2011): Temp, close-to-zero PPT, small or very large PPT ratios
				#	- Multiplicative approach (Wang et al. 2014): PPT otherwise
				scHistToFut <- scen.hist.ecdf$fun(scen.fut.ecdf$q, extrapol="linear")
				scHistToFutRatio <- obs.hist.ecdf$fun(scen.fut.ecdf$q, extrapol="linear") / scHistToFut
				if(any(iv <= 2, any(scHistToFut < 1/(10*downscaling.options[["PPTratioCutoff"]])), any(scHistToFutRatio > downscaling.options[["PPTratioCutoff"]]), any(scHistToFutRatio < 1/downscaling.options[["PPTratioCutoff"]]))){
					scen.fut.xadj <- scen.fut.x + obs.hist.ecdf$fun(scen.fut.ecdf$q, extrapol="linear") - scHistToFut
					if(all(iv == 3, sum(temp0 <- (scen.fut.xadj < 0)) > 0)) scen.fut.xadj[temp0] <- 0
				} else {
					scen.fut.xadj <- scen.fut.x * scHistToFutRatio
				}
				stopifnot(is.finite(scen.fut.xadj))
				if(do_checks){
					if(iv <= 2) test_sigmaNormal(data=scen.fut.xadj, downscaling.options[["sigmaN"]])
					if(iv == 3) test_sigmaGamma(data=scen.fut.xadj, downscaling.options[["sigmaN"]])
				}

				# 3. Calculate eCDF of future adjusted scenario
				scen.fut2.ecdf <- eCDF.Cunnane(scen.fut.xadj)
				
				# 5. Quantile map observed historic to adjusted future scenario
				#	- Additive approach (Anandhi et al. 2011): Temp, close-to-zero PPT, small or very large PPT ratios
				#	- Multiplicative approach (Wang et al. 2014): PPT otherwise
				scHistToHist <- obs.hist.ecdf$fun(obs.hist.ecdf$q, extrapol="linear")
				scHistToFutRatio <- scen.fut2.ecdf$fun(obs.hist.ecdf$q, extrapol="linear") / scHistToHist
				if(any(iv <= 2, any(scHistToHist < 1/(10*downscaling.options[["PPTratioCutoff"]])), any(scHistToFutRatio > downscaling.options[["PPTratioCutoff"]]), any(scHistToFutRatio < 1/downscaling.options[["PPTratioCutoff"]]))){
					mapFut <- scen.fut2.ecdf$fun(obs.hist.ecdf$q, extrapol="linear") - scHistToHist
					if(iv == 3) ppt_fun[m] <- "+"
				} else {
					mapFut <- scHistToFutRatio
					stopifnot(all(!is.infinite(mapFut)), all(!is.nan(mapFut))) #if(sum(temp <- is.nan(mapFut)) > 0) mapFut[temp] <- 0
				}
				delta_ts[delta_ts[, "Month"] == m, 2 + iv] <- mapFut[rank(obs.hist.x, ties.method="random")]
			}
		}
		
		# 6. Apply deltas to historic daily weather
		applyDeltas(obs.hist.daily, obs.hist.monthly, delta_ts, ppt_fun, downscaling.options[["sigmaN"]], do_checks=do_checks)
	})
   
	#------------------------
	doQmapQUANT.default_drs <- compiler::cmpfun(function(x, fobj, type = c("linear", "tricub"), linear_extrapolation=c("Boe", "Thermessl2012CC.QMv1b", "none"), spline_method=c("monoH.FC", "fmm", "natural"), monthly_extremes=NULL, correctSplineFun_type=c("fail", "none", "attempt"), ...) {
	  # Note: differs from call to call if jitter correction is used
	  
	  type <- match.arg(type)
	  linear_extrapolation <- match.arg(linear_extrapolation)
	  spline_method <- match.arg(spline_method)
	  correctSplineFun_type <- match.arg(correctSplineFun_type)
	  
	  wet <- if (!is.null(fobj$wet.day)) {
	    x >= fobj$wet.day
	  } else {
	    rep(TRUE, length(x))
	  }
	  out <- rep(NA, length.out = length(x))
	  
	  if (type == "linear") {
	    out[wet] <- approx(x = fobj$par$modq[, 1], y = fobj$par$fitq[, 1], xout = x[wet], method = "linear", rule = 2, ties = mean)$y
	    if(!(linear_extrapolation == "none")){
	      # "same extrapolation as Boe et al. (2007), but neglecting the three highest/lowest correction terms" Thermessl et al. 2011 Climatic Change
	      qid <- switch(linear_extrapolation, Boe=0, Thermessl2012CC.QMv1b=3)
	      nq <- nrow(fobj$par$modq)
	      largex <- x > fobj$par$modq[nq, 1] + sqrt(.Machine$double.eps)
	      if (any(largex)) {
	        max.delta <- fobj$par$modq[nq - qid, 1] - fobj$par$fitq[nq - qid, 1]
	        out[largex] <- x[largex] - max.delta
	      }
	      smallx <- x < fobj$par$modq[1, 1] - sqrt(.Machine$double.eps)
	      if (any(smallx)) {
	        min.delta <- fobj$par$modq[1 + qid, 1] - fobj$par$fitq[1 + qid, 1]
	        out[smallx] <- x[smallx] - min.delta
	      }
	    }
	  } else if (type == "tricub") {
	    sfun <- splinefun(x = fobj$par$modq[, 1], y = fobj$par$fitq[, 1], method = spline_method) #only "monoH.FC" would be appropriate here because we would want a monotone function if possible
	    temp <- sfun(x[wet])
	    
	    #There seem to be at least two causes for abnormally high values from sfun()
	    #	1) extrapolation error
	    #	2) huge oscillations
	    #		2a) arising from non-monotone splines ('fmm' and 'natural')
	    #		2b) arising from numerical instabilities in the exact monotonicity for 'monoH.FC'
	    
	    if(!is.null(monthly_extremes) && !(correctSplineFun_type == "none")){
	      # version previous to 20150705 didn't catch several bad cases, e.g., ix = 180099
	      # to prevent huge oscillation in 'fmm' and 'natural', we need to bound values between some small and some not-too large number
	      # apparently 'monoH.FC' does also show huge oscillations, e.g., ix=82529 because of numerical instabilities in the exact monotonicity in fobj$par$modq[, 1]
			icount <- 1
			while((itemp <- sum((temp < monthly_extremes[1]) | (temp > monthly_extremes[2]))) > 0 && icount < 10){
			  if(correctSplineFun_type == "fail") stop("Out-of-range splinefun values and 'correctSplineFun_type' set to fail")
			  sfun <- splinefun(x=jitter(fobj$par$modq[, 1]), y=jitter(fobj$par$fitq[, 1]), method=spline_method)
			  temp <- sfun(x[wet])
			  icount <- icount + 1
			}
			if(itemp > 0) stop("Jitter failed to fix out-of-range splinefun values")
	    }
	    
	    out[wet] <- temp
	  }
	  out[!wet] <- 0
	  if (!is.null(fobj$wet.day))  out[out < 0] <- 0
	  
	  out
	})
	
	doQmapQUANT_drs <- compiler::cmpfun(function(x, fobj, type=c("linear_Boe", "linear_Thermessl2012CC.QMv1b", "linear_none", "tricub_fmm", "tricub_monoH.FC", "tricub_natural", "normal_anomalies"), montly_obs_base=NULL, monthly_extremes=NULL, correctSplineFun_type=c("fail", "none", "attempt"), ...){
		correctSplineFun_type <- match.arg(correctSplineFun_type)
		type <- match.arg(type)
		temp <- strsplit(type, "_", fixed=TRUE)[[1]]
		type <- temp[1]
		type_mod <- temp[2]

		if (type == "linear") {
			out <- try(doQmapQUANT.default_drs(x, fobj, type="linear", linear_extrapolation=type_mod, monthly_extremes=monthly_extremes, correctSplineFun_type=correctSplineFun_type, ...), silent=TRUE)
		} else if (type == "tricub") {
			out <- try(doQmapQUANT.default_drs(x, fobj, type="tricub", spline_method=type_mod, monthly_extremes=monthly_extremes, correctSplineFun_type=correctSplineFun_type, ...), silent=TRUE)
		} else if (type == "normal") {
			# Tohver, I. M., A. F. Hamlet, and S.-Y. Lee. 2014. Impacts of 21st-Century Climate Change on Hydrologic Extremes in the Pacific Northwest Region of North America. Journal of the American Water Resources Association 50:1461-1476.
			# Appendix A, p. 6: "... values that are outside the observed quantile map (e.g. in the early parts of the 20th century) are interpolated using standard anomalies (i.e. number of standard deviations from the mean) calculated for the observed data and GCM data. Although this approach ostensibly assumes a normal distribution, it was found during testing to be much more stable than attempts to use more sophisticated approaches. In particular, the use of Extreme Value Type I or Generalized Extreme Value distributions for extending the tail of the probability distributions were both found to be highly unstable in practice and introduced unacceptable daily extremes in isolated grid cells. These errors occur because of irregularities in the shapes of the CDFs for observed and GCM data, which relates in part to the relatively small sample size used to construct the monthly CDFs (i.e. n = 30)."

			out <- doQmapQUANT.default_drs(x, fobj, type="linear", linear_extrapolation="Boe", monthly_extremes=monthly_extremes, correctSplineFun_type=correctSplineFun_type, ...)

			target_range <- c(-Inf, fobj$par$modq[1, 1] -  sqrt(.Machine$double.eps), max(fobj$par$modq[, 1]) + sqrt(.Machine$double.eps), Inf) # -Inf, smallest observed value, largest observed value, Inf
			out_of_range <- !(findInterval(x, target_range) == 2)

			if (any(out_of_range)) {
				tscore_x <- (x[out_of_range] - mean(montly_obs_base)) / sd(montly_obs_base)
				out[out_of_range] <- mean(out[!out_of_range]) + sd(out[!out_of_range]) * tscore_x 
			}
		}

		out
	})

	
	#------------------------
	
	#' Downscale with the 'delta-hybrid approach' new version (post to May 2016)
	#' 
	#' Hybrid-delta downscaling developed by Hamlet et al. 2010 and Tohver et al. 2014.
	#' Applied, e.g., by Dickerson-Lange et al. 2014.
	#' Quantile mapping performed by functions modified from Gudmundsson et al. 2012.
	#'
	#' @inheritParams downscale
	#'
	#' @references Hamlet, A. F., E. P. Salathé, and P. Carrasco. 2010. Statistical downscaling techniques for global climate model simulations of temperature and precipitation with application to water resources planning studies. Chapter 4. Final Report for the Columbia Basin Climate Change Scenarios Project. Climate Impacts Group, Center for Science in the Earth System, Joint Institute for the Study of the Atmosphere and Ocean, University of Washington, Seattle, WA.
	#' @references Tohver, I.M., Hamlet, A.F. & Lee, S.-Y. (2014) Impacts of 21st-Century Climate Change on Hydrologic Extremes in the Pacific Northwest Region of North America. Journal of the American Water Resources Association, 50, 1461-1476.
	#' @references Anandhi, A., A. Frei, D. C. Pierson, E. M. Schneiderman, M. S. Zion, D. Lounsbury, and A. H. Matonse. 2011. Examination of change factor methodologies for climate change impact assessment. Water Resources Research 47:W03501.
	#' @references Dickerson-Lange, S. E., and R. Mitchell. 2014. Modeling the effects of climate change projections on streamflow in the Nooksack River basin, Northwest Washington. Hydrological Processes:doi: 10.1002/hyp.10012.
	#' @references Wang, L., and W. Chen. 2014. Equiratio cumulative distribution function matching as an improvement to the equidistant approach in bias correction of precipitation. Atmospheric Science Letters 15:1-6.
	#' @references Gudmundsson, L., Bremnes, J.B., Haugen, J.E. & Engen-Skaugen, T. (2012). Technical Note: Downscaling RCM precipitation to the station scale using statistical transformations - a comparison of methods. Hydrol Earth Syst Sci, 16, 3383-3390.
	#' @export
	downscale.deltahybrid3mod <- compiler::cmpfun(function(obs.hist.daily, obs.hist.monthly, scen.hist.monthly, scen.fut.monthly,
	                                      deltaFuture_yr, years = NULL, 
	                                      DScur_startyear = NULL, DScur_endyear = NULL,
	                                      DSfut_startyear = NULL, DSfut_endyear = NULL,
	                                      downscaling.options = list(extrapol_type = "linear_Thermessl2012CC.QMv1b",
	                                      		applyPPT_type = "detailed",
	                                      		sigmaN = 6,
												PPTratioCutoff = 10,
												correctSplineFun_type = "attempt"),
										  dailyPPTceiling, monthly_extremes,
	                                      do_checks = TRUE) {

		stopifnot(requireNamespace("qmap"))
		qstep <- 0.01
		nboot <- 1

		# Time periods
		tp <- downscale.periods(obs.hist.daily, obs.hist.monthly, scen.hist.monthly, scen.fut.monthly, years, DScur_startyear, DScur_endyear, DSfut_startyear, DSfut_endyear)
		if (any(!tp$iuse_obs_hist_d)) obs.hist.daily <- obs.hist.daily[tp$iuse_obs_hist_d]
		if (any(!tp$iuse_obs_hist_m)) obs.hist.monthly <- obs.hist.monthly[tp$iuse_obs_hist_m, ]
		if (any(!tp$iuse_scen_hist_m)) scen.hist.monthly <- scen.hist.monthly[tp$iuse_scen_hist_m, ]
		if (any(!tp$iuse_scen_fut_m)) scen.fut.monthly <- scen.fut.monthly[tp$iuse_scen_fut_m, ]

		# Data objects
		sbc.hist.monthly <- matrix(NA, ncol=5, nrow=nrow(scen.hist.monthly), dimnames=list(NULL, colnames(obs.hist.monthly)))
		sbc.hist.monthly[, 1:2] <- scen.hist.monthly[, 1:2]
		sbc.fut.monthly <- matrix(NA, ncol=5, nrow=nrow(scen.fut.monthly), dimnames=list(NULL, colnames(obs.hist.monthly)))
		sbc.fut.monthly[, 1:2] <- scen.fut.monthly[, 1:2]
		#	future simulation years = delta + simstartyr:endyr
		hd.fut.monthly <- delta_ts <- matrix(NA, ncol=5, nrow=nrow(obs.hist.monthly), dimnames=list(NULL, colnames(obs.hist.monthly)))
		hd.fut.monthly[, 1:2] <- delta_ts[, 1:2] <- obs.hist.monthly[, 1:2]
		hd.fut.monthly[, 1] <- hd.fut.monthly[, 1] + deltaFuture_yr

		#------STEPS 1-4 based on the appendix of Tohver et al. 2014
		for (iv in 1:3) {	# for each variable separately: Tmax, Tmin, PPT
			# NAs in scenario data: impute with median conditions (TODO(drs): implement a more sophisticated imputation scheme; this one biases variation downwards)
			if (anyNA(scen.hist.monthly[, 2 + iv])) {
				id_nas <- is.na(scen.hist.monthly[, 2 + iv])
				scen.hist.monthly[id_nas, 2 + iv] <- median(scen.hist.monthly[, 2 + iv], na.rm=TRUE)
			}
			if (anyNA(scen.fut.monthly[, 2 + iv])) {
				id_nas <- is.na(scen.fut.monthly[, 2 + iv])
				scen.fut.monthly[id_nas, 2 + iv] <- median(scen.fut.monthly[, 2 + iv], na.rm=TRUE)
			}
	    
			#---STEP 1: Statistical bias correction of GCM data
			# 1st part of this step is NOT carried out here because our GCM data is already BCSD downscaled: "first aggregating the gridded T and P observations to the GCM grid scale (at the time of this writing typically about 200km resolution)"

			# fit quantile map based on training data of same historic time period
			qm_fit <- qmap::fitQmapQUANT.default(obs=obs.hist.monthly[, 2 + iv], mod=scen.hist.monthly[, 2 + iv], qstep=qstep, nboot=nboot, wet.day=FALSE)
	    
			# 2nd part: bias correcting historic data ("then using quantile mapping techniques to remove the systematic bias in the GCM simulations relative to the observed probability distributions")
			temp <- try(doQmapQUANT_drs(x=scen.hist.monthly[, 2 + iv], fobj=qm_fit, type=downscaling.options[["extrapol_type"]], montly_obs_base=obs.hist.monthly[, 2 + iv], monthly_extremes=monthly_extremes[[iv]], correctSplineFun_type=downscaling.options[["correctSplineFun_type"]]), silent = TRUE)
			if (inherits(temp, "try-error")) return(temp)
			sbc.hist.monthly[, 2 + iv] <- temp
	    
			# 3rd part: bias correcting future data ("the same quantile map between simulations and observations is used to transform the future simulations from the GCM")
			temp <- try(doQmapQUANT_drs(x=scen.fut.monthly[, 2 + iv], fobj=qm_fit, type=downscaling.options[["extrapol_type"]], montly_obs_base=obs.hist.monthly[, 2 + iv], monthly_extremes=monthly_extremes[[iv]], correctSplineFun_type=downscaling.options[["correctSplineFun_type"]]), silent = TRUE)
			if (inherits(temp, "try-error")) return(temp)
			sbc.fut.monthly[, 2 + iv] <- temp


			#---STEP 2: Spatial downscaling
			# 	- "the monthly T and P values at the GCM grid scale are interpolated to the fine scale grid"
			# 	-> not done here because spatial aggregation (step 1, 1st part) not carried out

			for (im in 1:12) { # for each month separately
				#---STEP 3: Remapping the Historical Record to Interpolated GCM data
				id_sim_months <- obs.hist.monthly[, "Month"] == im	#identical(obs.hist.monthly[, 2], hd.fut.monthly[, 2])

				qm_fitm <- qmap::fitQmapQUANT.default(obs=sbc.fut.monthly[sbc.fut.monthly[, 2] == im, 2 + iv], mod=obs.hist.monthly[id_sim_months, 2 + iv], qstep=qstep, nboot=nboot, wet.day=FALSE)
				temp <- try(doQmapQUANT_drs(x=obs.hist.monthly[id_sim_months, 2 + iv], fobj=qm_fitm, type=downscaling.options[["extrapol_type"]], montly_obs_base=obs.hist.monthly[, 2 + iv], monthly_extremes=monthly_extremes[[iv]], correctSplineFun_type=downscaling.options[["correctSplineFun_type"]]), silent = TRUE)
				if (inherits(temp, "try-error")) return(temp)
				hd.fut.monthly[id_sim_months, 2 + iv] <- temp
			}
		}

		#---STEP 4: Daily Time Step Disaggregation of Monthly Data
		delta_ts[, c("Tmax_C", "Tmin_C")] <- hd.fut.monthly[, c("Tmax_C", "Tmin_C")] - obs.hist.monthly[, c("Tmax_C", "Tmin_C")] # equation 8

		ppt_fun <- rep("*", 12)
		delta_ppts <- hd.fut.monthly[, "PPT_cm"] / obs.hist.monthly[, "PPT_cm"] # equation 7
		temp_add <- is.infinite(delta_ppts) | is.nan(delta_ppts) | delta_ppts > downscaling.options[["PPTratioCutoff"]] | delta_ppts < 1 / downscaling.options[["PPTratioCutoff"]]
		if (any(temp_add)) {
			ids_m <- unique(delta_ts[temp_add, "Month"])
			ppt_fun[ids_m] <- "+"
			temp_m <- delta_ts[, "Month"] %in% ids_m # all calendar month for which at least one instance qualifies for additive PPT
			delta_ppts[temp_m] <- hd.fut.monthly[temp_m, "PPT_cm"] - obs.hist.monthly[temp_m, "PPT_cm"]
		}
		delta_ts[, "PPT_cm"] <- delta_ppts
      
	    # Apply deltas to historic daily weather
	    # Note: PPT differs from call to call to applyDeltas() because of controlExtremePPTevents (if dailyPPTceiling > 0)
		try(applyDeltas2(daily = obs.hist.daily, monthly = obs.hist.monthly,
	    						years = tp$years, delta_ts, ppt_fun, ppt_type = downscaling.options[["applyPPT_type"]], dailyPPTceiling, sigmaN = downscaling.options[["sigmaN"]], do_checks = do_checks), silent = FALSE)
	})



	#-------DB access functions
	if (exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_NEX_USA) {	
	
		mmPerSecond_to_cmPerMonth <- compiler::cmpfun(function(prcp_mmPerSecond, dpm) {
			prcp_mmPerSecond * dpm * 8640	# 24 * 60 * 60 / 10 == 8640
		})

		get.request <- compiler::cmpfun(function(service, request, i, variable, scen, gcm, lon, lat, startyear, endyear, dir.out.temp, useRCurl, saveNEXtempfiles) {
			if (useRCurl && !saveNEXtempfiles) {
				success <- try(getURL(request, .opts=list(timeout=5*60, connecttimeout=60)), silent=TRUE)
				if (!inherits(success, "try-error")) {
					if (isTRUE(grepl("Not Found", success, ignore.case=TRUE))) {
						class(success) <- "try-error"
					} else {
						if (service == "ncss") {
							ftemp <- textConnection(success)
						} else if (service == "opendap") {
							ftemp <- textConnection((temp <- strsplit(success, split="\n\n", fixed=TRUE))[[1]][3])
							ttemp <- as.POSIXlt("1950-01-01") + 86400 * as.numeric(scan(text=sub("\n", ",", temp[[1]][4], fixed=TRUE), what="character", sep=",", quiet=TRUE)[-1])
						}
						success <- 0
					}
				}
			} else {
				if (service == "opendap") stop("Curl must be present to access NEX-DCP30 data via thredds/dodsC (opendap)")
				ftemp <- file.path(dir.out.temp, paste0("NEX_", gcm, "_", scen, "_", variable, "_", round(lat, 5), "&", round(lon, 5), ".csv"))
				success <- try(download.file(url=request, destfile=ftemp, quiet=TRUE), silent=TRUE)
			}
	
			yearsN <- endyear - startyear + 1
			dat <- rep(NA, times=12*yearsN)
			if (!inherits(success, "try-error") && success == 0){
				if (service == "ncss") {
					temp <- read.csv(ftemp, colClasses=c("POSIXct", "NULL", "NULL", "numeric")) #colnames = Time, Lat, Long, Variable
					vtemp <- temp[, 2]
					ttemp <- as.POSIXlt(temp[, 1])
				} else if (service == "opendap") {
					vtemp <- read.csv(ftemp, colClasses=c("NULL", "numeric"), header=FALSE)[-1, ] #columns = Index, Variable
				}
				if (!saveNEXtempfiles && !useRCurl && file.exists(ftemp)) unlink(ftemp)
				if (length(vtemp) < 12*yearsN) { #some GCMs only have values up to Nov 2099
					tempYearMonth <- paste(ttemp$year + 1900, ttemp$mo + 1, sep="_")
					targetYearMonth <- paste(rep(startyear:endyear, each=12), rep(1:12, times=yearsN), sep="_")
					dat[match(tempYearMonth, targetYearMonth, nomatch=0)] <- vtemp[match(targetYearMonth, tempYearMonth, nomatch=0)]
				} else {
					dat <- vtemp
				}
			} else {
				stop(paste(i, "th extraction of NEX at", Sys.time(), "for", gcm, scen, "at", lon, lat, ": not successful"))
			}
		
			dat
		})

	
		get.DBvariable <- compiler::cmpfun(function(i, variable, scen, gcm, lon, lat, bbox, startyear, endyear, dir.out.temp, useRCurl, saveNEXtempfiles) {
			gcmrun <- "r1i1p1"
			#1st attempt: TRHEDDS ncss/netCDF subsetting service
			request <- paste0(paste("http://dataserver.nccs.nasa.gov", "thredds/ncss/grid/bypass/NEX-DCP30/bcsd", scen, gcmrun,
								paste0(gcm, "_", variable, ".ncml"), sep="/"), "?var=", paste0(gcm, "_", variable), 
								"&latitude=", lat, "&longitude=", ifelse(lon > 180, lon - 360, lon),
								paste0("&time_start=", startyear, "-01-01T00%3A00%3A00Z&time_end=", endyear, "-12-31T23%3A59%3A59Z&timeStride=1"),
								"&accept=csv")
			dat <- get.request(service="ncss", request, i, variable, scen, gcm, lon, lat, startyear, endyear, dir.out.temp, useRCurl, saveNEXtempfiles)
			if(inherits(dat, "try-error") || any(dat > 1e5 | dat < -1e5, na.rm=TRUE)){ #thredds/ncss/ returns for some GCMs/RCPs/locations unrealistic large values, e.g., 9.969210e+36 and sometimes 2.670153e+42 for pr, tasmin, and tasmax for the month of May in every fifth year (2071, 2076, ...): bug report to NASA NCCS Support Team on June 2, 2014 - confirmed on June 8, 2014 by Yingshuo Shen (issue=48932)
				#2nd attempt: TRHEDDS opendap/dodsC
				lat.index <- round((lat - bbox$lat[1]) / 0.0083333333, 0)
				lon.index <- round((lon - bbox$lon[1]) / 0.0083333333, 0)
				if(startyear < 2006 && scen == "historical"){
					index.time.start <- (startyear - 1950) * 12
					index.time.end <- (endyear + 1 - 1950) * 12 - 1
				} else {
					index.time.start <- (startyear - 2006) * 12
					index.time.end <- (endyear + 1 - 2006) * 12 - 1
				}
				request <- paste0(paste("http://dataserver.nccs.nasa.gov", "thredds/dodsC/bypass/NEX-DCP30/bcsd", scen, gcmrun,
								paste0(gcm, "_", variable, ".ncml.ascii"), sep="/"),
								"?lat[", lat.index, "],lon[", lon.index, "],",
								gcm, "_", variable, "[", index.time.start, ":1:", index.time.end, "][", lat.index, "][", lon.index, "]")

				dat <- get.request(service="opendap", request, i, variable, scen, gcm, lon, lat, startyear, endyear, dir.out.temp, useRCurl, saveNEXtempfiles)
				stopifnot(!inherits(dat, "try-error"), all(dat < 1e5 & dat > -1e5, na.rm=TRUE))
			}
		
			dat
		})
		
		#' @return A list of one data.frame object with 5 columns and names of
		#' "year", "month", "tmax", "tmin", and "prcp". Each row is one day.
		#' Units are [degree Celsius] for temperature and [cm / day] and [cm / month], respectively, for precipitation.
		get_GCMdata <- compiler::cmpfun(function(i, ts_mons, dpm, gcm, scen, lon, lat, startyear, endyear, varTags, ...) {
			dots <- list(...) # bbox, dir.out.temp, useRCurl, saveNEXtempfiles
			
			clim <- vector("list", length=3)
			names(clim) <- varTags
			
			for (iv in seq_along(varTags)) {
				#Extract data
				clim[[iv]] <- get.DBvariable(i, variable=varTags[iv], scen=scen, gcm=gcm, lon=lon, lat=lat, bbox = dots[["bbox"]], startyear=startyear, endyear=endyear, dir.out.temp = dots[["dir.out.temp"]], useRCurl = dots[["useRCurl"]], saveNEXtempfiles = dots[["saveNEXtempfiles"]])
				#Adjust units
				if (varTags[iv] == "pr") {#convert kg/m2/s -> cm/month
					clim[[iv]] <- mmPerSecond_to_cmPerMonth(prcp_mmPerSecond=clim[[iv]], dpm)
				} else if (grepl("tas", varTags[iv])) {	#convert K -> C
					clim[[iv]] <- clim[[iv]] - 273.15
				}
			}
		
			#Monthly weather time-series
			list(cbind(year=ts_mons$year + 1900, month=ts_mons$mon + 1, tmax = clim[["tasmax"]], tmin = clim[["tasmin"]], prcp = clim[["pr"]]))
		})
	}		

	if (exinfo$GDODCPUCLLNL) {
		whereNearest <- compiler::cmpfun(function(val, matrix) {
			#this returns the index of the closest value in the matrix to the passed in value.
			which.min(abs(matrix - val))
		})

		get.SpatialIndices <- compiler::cmpfun(function(filename, lon, lat) {
			nc <- nc_open(filename=filename, write=FALSE, readunlim=TRUE, verbose=FALSE)
		
			#Get latitudes/longitudes from the netCDF files...; they are the same for each CMIP x extent
			#	- these are used to get the correct indices in the whereNearest function
			lats <- nc$dim$lat$vals
			lons <- nc$dim$lon$vals
			#close the netCDF file
			nc_close(nc)
		
			if (any(lons > 180)) lons <- ifelse(lons > 180, lons - 360, lons)
			#Calculate the spatial indices
			ncg <- NULL
			ncg$ix <- whereNearest(val=lon, matrix=lons)
			ncg$iy <- whereNearest(val=lat, matrix=lats)

			ncg
		})

		get.TimeIndices <- compiler::cmpfun(function(filename, startyear, endyear) {
			nc <- nc_open(filename=filename, write=FALSE, readunlim=TRUE, verbose=FALSE)

			utemp <- nc$dim$time$units
			tvals <- nc$dim$time$vals
			nc_close(nc)
		
			N <- length(tvals)
			temp <- lapply(strsplit(utemp, split=" ", fixed=TRUE)[[1]], function(x) as.Date(x, format="%Y-%m-%d"))
			baseYear <- temp[sapply(temp, function(x) !is.na(x))][[1]]
			stopifnot(length(baseYear) == 1)

			firstDate <- as.POSIXlt(baseYear + tvals[1])
			lastDate <- as.POSIXlt(baseYear + tvals[N])
		
			startYear <- firstDate$year + 1900
			startMonth <- firstDate$mon + 1
			endYear <- lastDate$year + 1900
			endMonth <- lastDate$mon + 1
		
			stopifnot(startYear <= startyear || (startMonth == 1 && startYear == startyear)) 	#we only extract full years and require data from the startyear on
			timeStartIndex <- (startyear - startYear) * 12 + 2 - startMonth #we extract beginning with January of startyear
		
			#account for missing months: assume all are at the end; e.g., precipitation of 'HadGEM2-ES' has values only until Nov 2099 instead Dec 2100
			timeCount_should <- (endyear - startyear + 1) * 12 #timeCount must include a count at timeStartIndex; to extract two values at 1:2, have timeStartIndex=1 and timeCount=2
			N_should <- timeStartIndex + timeCount_should - 1
			if (N >= N_should) {
				timeCount <- timeCount_should
				addMissingMonthAtEnd <- 0
			} else {
				timeCount <- N - timeStartIndex + 1
				addMissingMonthAtEnd <- N_should - N
			}
		
			list(timeStartIndex = timeStartIndex,
				timeCount = timeCount,
				addMissingMonthAtEnd = addMissingMonthAtEnd)
		})
	
		do_ncvar_get <- compiler::cmpfun(function(nc, nc_perm, variable, ncg, nct){
			index <- which("time" == nc_perm)

			if (index == 3L) { #if file is in order of (lat, lon, time)
				ncvar_get(nc, variable, start=c(ncg$ix, ncg$iy, nct$timeStartIndex), count=c(1, 1, nct$timeCount))
			} else if (index == 1L){ #if file is optimized for time series extraction and permutated to order (time, lat, lon)
				ncvar_get(nc, variable, start=c(nct$timeStartIndex, ncg$ix, ncg$iy), count=c(nct$timeCount, 1, 1))
			} else {
				stop("do_ncvar_get: dimension 'time' must be either in first or third place, but is instead at ", index)
			}
		})
	
		mmPerDay_to_cmPerMonth <- compiler::cmpfun(function(prcp_mmPerDay, dpm) {
			prcp_mmPerDay / 10 * dpm
		})
	
		get.DBvariable <- compiler::cmpfun(function(filepath, variable, unit, ncg, nct, lon, lat, startyear, endyear) {
			# the 'raster' package (version <= '2.5.2') cannot handle non-equally spaced cells
			nc <- nc_open(filename = filepath, write = FALSE, readunlim = TRUE, verbose = FALSE)
			stopifnot(grepl(unit, nc$var[[variable]]$units, fixed = TRUE))
		
			# getting the values from the netCDF files...
			nc_perm <- sapply(nc$var[[variable]]$dim, function(x) x$name)
			res <- try(do_ncvar_get(nc, nc_perm, variable, ncg, nct), silent = TRUE)
			if (inherits(res, "try-error")) { #in case of 'HadGEM2-ES x RCP45' where pr and tasmax/tasmin have different timings
				ncg <- get.SpatialIndices(filename = filepath, lon, lat)
				nct <- get.TimeIndices(filename = filepath, startyear, endyear)
				res <- try(do_ncvar_get(nc, nc_perm, variable, ncg, nct), silent = TRUE)
			}
			nc_close(nc) #close the netCDF file
		
			#adjust for missing months
			if (nct$addMissingMonthAtEnd > 0) res <- c(res, rep(NA, times = nct$addMissingMonthAtEnd))
			
			#no data: most likely cell location is not terrestrial
			if (all(is.na(res))) stop("get.DBvariable: all data are NAs; cell location (", round(lon, 5), ", ", round(lat, 5), ") is likely not terrestrial")
			
			res
		})

	
		#' @return A list of one data.frame object with 5 columns and names of
		#' "year", "month", "tmax", "tmin", and "prcp". Each row is one day.
		#' Units are [degree Celsius] for temperature and [cm / day] and [cm / month], respectively, for precipitation.
		get_GCMdata <- compiler::cmpfun(function(i, ts_mons, dpm, gcm, scen, lon, lat, startyear, endyear, varTags, ...) {
			dots <- list(...) # fileVarTags, ncFiles, ncg, nct

			#Get precipitation data
			temp1 <- grepl(dots[["fileVarTags"]]["prcp"], dots[["ncFiles"]])
			if (any(temp1)) {
				prcp <- get.DBvariable(filepath = dots[["ncFiles"]][temp1][1], variable = varTags["prcp"], unit = "mm/d", ncg = dots[["ncg"]], nct = dots[["ncg"]], lon = lon, lat = lat, startyear = startyear, endyear = endyear)
				prcp <- mmPerDay_to_cmPerMonth(prcp, dpm)
			} else {
				stop("No suitable netCDF file with precipitation data found for ", i, gcm, scen, varTags["prcp"])
			}
			#Get temperature data
			temp3 <- grepl(dots[["fileVarTags"]]["tmin"], dots[["ncFiles"]])
			temp4 <- grepl(dots[["fileVarTags"]]["tmax"], dots[["ncFiles"]])
			if (any(temp3) && any(temp4)) {
				tmin <- get.DBvariable(filepath = dots[["ncFiles"]][temp3][1], variable = varTags["tmin"], unit = "C", ncg = dots[["ncg"]], nct = dots[["ncg"]], lon = lon, lat = lat, startyear = startyear, endyear = endyear)
				tmax <- get.DBvariable(filepath = dots[["ncFiles"]][temp4][1], variable = varTags["tmax"], unit = "C", ncg = dots[["ncg"]], nct = dots[["ncg"]], lon = lon, lat = lat, startyear = startyear, endyear = endyear)
			} else {
				temp2 <- grepl(dots[["fileVarTags"]]["tmean"], dots[["ncFiles"]])
				if (any(temp2)) {
					tmean <- get.DBvariable(filepath = dots[["ncFiles"]][temp2][1], variable = varTags["tmean"], unit = "C", ncg = dots[["ncg"]], nct = dots[["ncg"]], lon = lon, lat = lat, startyear = startyear, endyear = endyear)
					tmin <- tmax <- tmean
				} else {
					stop("No suitable netCDF file with temperature data found for ", i, gcm, scen, varTags["prcp"])
				}
			}
		
			list(cbind(year = ts_mons$year + 1900, month = ts_mons$mon + 1, tmax = tmax, tmin = tmin, prcp = prcp))
		})
	}
	
	#----Extraction function
	calc.ScenarioWeather <- compiler::cmpfun(function(i, GCM_source, is_GDODCPUCLLNL, is_NEX, gcmFiles, reqGCMs, reqRCPsPerGCM, reqDownscalingsPerGCM, climate.ambient, bbox, locations, dbW_iSiteTable, varTags, fileVarTags, getYears, assocYears, future_yrs, simstartyr, endyr, DScur_startyr, DScur_endyr, downscaling.options, dir.out.temp, be.quiet, useRCurl, saveNEXtempfiles) { 
		#Identify index for site and scenario
		ig <- (i - 1) %% length(reqGCMs) + 1
		gcm <- reqGCMs[ig]
		rcps <- reqRCPsPerGCM[[ig]]
		downs <- reqDownscalingsPerGCM[[ig]]
		il <- (i - 1) %/% length(reqGCMs) + 1
		lon <- locations[il, 1]
		lat <- locations[il, 2]
		site_id <- dbW_iSiteTable[dbW_iSiteTable[, "Label"] == locations[il, 3], "Site_id"]		
		ncFiles_gcm <- if (is_GDODCPUCLLNL) gcmFiles[grepl(paste0("_", as.character(gcm), "_"), gcmFiles)] else NULL
		if (!be.quiet) print(paste(i, "th extraction of '", GCM_source, "' at", Sys.time(), "for", gcm, "(", paste(rcps, collapse=", "), ") at", lon, lat))
#			if(!be.quiet && (i-1) %% print_int == 0) print(paste(i, "th extraction of '", GCM_source, "' at", Sys.time(), "for", gcm, "(", paste(rcps, collapse=", "), ") at", lon, lat))
#			if(!be.quiet && (i-1) %% 10000 == 0) saveRDS(i, file=file.path(dir.out.temp, paste0("iteration_", i, ".rds")))
		
		# Data Bounding Box
		if (!(lat >= bbox$lat[1] && lat <= bbox$lat[2] && lon >= bbox$lon[1] && lon <= bbox$lon[2])) {
			stop(paste(i, "th extraction of '", GCM_source, "' at", Sys.time(), "at", lon, lat, ": outside of bounding data box"))
		}

		#Scenario monthly weather time-series: Get GCM data for each scenario and time slice
		scen.monthly <- matrix(data=vector("list", length=(getYears$n_first+getYears$n_second)*(1 + length(rcps))), ncol=getYears$n_first+getYears$n_second, dimnames=list(c("Current", as.character(rcps)), c(paste0("first", 1:getYears$n_first), paste0("second", 1:getYears$n_second))))
		#First slice ('historical'): 1950-2005
		args_template1 <- list(i = i, gcm = gcm, scen = "historical", lon = lon, lat = lat, varTags = varTags)
		if (is_GDODCPUCLLNL) {
			ncFiles <- ncFiles_gcm[grepl(args_template1[["scen"]], ncFiles_gcm)]
			ncg <- get.SpatialIndices(filename = ncFiles[1], lon = lon, lat = lat)
		}
		if (is_GDODCPUCLLNL) {
			args_template1 <- c(args_template1, fileVarTags = list(fileVarTags), ncFiles = list(ncFiles), ncg = list(ncg))
		} else if (is_NEX) {
			args_template1 <- c(args_template1, bbox = list(bbox), dir.out.temp = dir.out.temp, useRCurl = useRCurl, saveNEXtempfiles = saveNEXtempfiles)
		}
		for (it in 1:getYears$n_first) {
			args_first <- c(args_template1, 
								ts_mons = list(getYears$first_dates[[it]]),
								dpm = list(getYears$first_dpm[[it]]),
								startyear = getYears$first[it, 1],
								endyear = getYears$first[it, 2])
			if (is_GDODCPUCLLNL) {
				# Time index: differs among variables from the same GCMxRCP: in only once case: HadGEM2-ES x RCP45
				args_first <- c(args_first, nct = list(get.TimeIndices(filename = ncFiles[1], startyear = getYears$first[it, 1], endyear = getYears$first[it, 2])))
			}
			scen.monthly[1, it] <- do.call(get_GCMdata, args = args_first)
		}
		
		#Second slice ('future scenarios'): 2006-2099
		for (it in 1:getYears$n_second) {
			args_template2 <- c(args_template1, 
								ts_mons = list(getYears$second_dates[[it]]),
								dpm = list(getYears$second_dpm[[it]]),
								startyear = getYears$second[it, 1],
								endyear = getYears$second[it, 2])
			# Time index: differs among variables from the same GCMxRCP: in only once case: HadGEM2-ES x RCP45
			if (is_GDODCPUCLLNL) {
				args_template2[["nct"]] <- get.TimeIndices(filename=ncFiles_gcm[grep(as.character(rcps)[1], ncFiles_gcm)[1]], startyear=getYears$second[it, 1], endyear=getYears$second[it, 2])
			}
			for (isc in 2:nrow(scen.monthly)) { 
				args_second <- args_template2
				args_second[["scen"]] <- as.character(rcps[isc - 1])
				if (is_GDODCPUCLLNL) {
					args_second[["ncFiles"]] <- ncFiles_gcm[grepl(args_second[["scen"]], ncFiles_gcm)]
				}
				scen.monthly[isc, getYears$n_first + it] <- do.call(get_GCMdata, args = args_second)
			}
		}

	
		#Observed historic daily weather from weather database
		obs.hist.daily <- Rsoilwat31::dbW_getWeatherData(Site_id=site_id, startYear=simstartyr, endYear=endyr, Scenario=climate.ambient)
		if (obs.hist.daily[[1]]@year < 1950){ #TODO(drs): I don't know where the hard coded value of 1950 comes from; it doesn't make sense to me
			print("Note: subsetting years 'obs.hist.daily' because 'simstartyr < 1950'")
			start_yr <- obs.hist.daily[[length(obs.hist.daily)]]@year - 1950
			obs.hist.daily <- obs.hist.daily[(length(obs.hist.daily)-start_yr):length(obs.hist.daily)]
		}

		sim_years <- as.integer(names(obs.hist.daily))
		obs.hist.monthly <- dbW_weatherData_to_monthly(dailySW = obs.hist.daily)

		#Hamlet et al. 2010: "an arbitrary ceiling of 150% of the observed maximum precipitation value for each cell is also imposed by ???spreading out??? very large daily precipitation values into one or more adjacent days"
		dailyPPTceiling <- downscaling.options[["daily_ppt_limit"]] * max(unlist(lapply(obs.hist.daily, function(obs) max(obs@data[, "PPT_cm"]))))
		#Monthly extremes are used to cut the most extreme spline oscillations; these limits are ad hoc; monthly temperature extremes based on expanded daily extremes
		temp <- stretch_values(x = range(sapply(obs.hist.daily, function(obs) obs@data[, c("Tmax_C", "Tmin_C")])), lambda = downscaling.options[["monthly_limit"]])
		monthly_extremes <- list(Tmax = temp, Tmin = temp, PPT = c(0, downscaling.options[["monthly_limit"]] * max(aggregate(obs.hist.monthly[, "PPT_cm"], by=list(obs.hist.monthly[, 1]), FUN=sum)$x)))

	
		wdataOut <- list()
		for (ir in seq_along(rcps)) { #Apply downscaling for each RCP
			#Put historical data together
			#NOTE: both scen.hist.monthly and scen.fut.monthly may have NAs because some GCMs do not provide data for the last month of a time slice (e.g. December 2005 may be NA)
			if (!all(downs == "raw")) {
				scen.hist.monthly <- NULL
				for(itt in which(assocYears[["historical"]]$first)) scen.hist.monthly <- rbind(scen.hist.monthly, scen.monthly[1, itt][[1]])
				for(itt in which(assocYears[["historical"]]$second)) scen.hist.monthly <- rbind(scen.hist.monthly, scen.monthly[1 + ir, getYears$n_first + itt][[1]])
			}
		
			types <- list()
			for (it in 1:nrow(future_yrs)) {
				tag <- paste0(rownames(future_yrs)[it], ".", rcps[ir])
			
				#Put future data together
				scen.fut.monthly <- NULL
				for(itt in which(assocYears[[tag]]$first)) scen.fut.monthly <- rbind(scen.fut.monthly, scen.monthly[1, itt][[1]])
				for(itt in which(assocYears[[tag]]$second)) scen.fut.monthly <- rbind(scen.fut.monthly, scen.monthly[1 + ir, getYears$n_first + itt][[1]])
				
				# Comment: The variables are expected to cover the following time periods
				# 'obs.hist.daily' = simstartyr:endyr
				# 'obs.hist.monthly' = simstartyr:endyr
				# 'scen.hist.monthly' = DScur_startyr:DScur_endyr
				# 'scen.fut.monthly' = DSfut_startyr:DSfut_endyr
				# 'scen.fut.daily' will cover: delta + simstartyr:endyr
				# Units are [degree Celsius] for temperature and [cm / day] and [cm / month], respectively, for precipitation
				
				#Apply downscaling					
				if ("raw" %in% downs) {
					scenario_id <- dbW_iScenarioTable[dbW_iScenarioTable[, "Scenario"] == tolower(paste("raw", tag, gcm, sep=".")), "id"]
					scen.fut.daily <- downscale.raw(obs.hist.daily, obs.hist.monthly, scen.fut.monthly,
											years = sim_years,
											DScur_startyear = DScur_startyr, DScur_endyear = DScur_endyr,
											DSfut_startyear = future_yrs[it, "DSfut_startyr"], DSfut_endyear = future_yrs[it, "DSfut_endyr"],
											downscaling.options = downscaling.options,
											dailyPPTceiling = dailyPPTceiling,
											do_checks = TRUE)
					
					if (inherits(scen.fut.daily, "try-error")) {#raw unsuccessful, replace with raw without checks
						scen.fut.daily <- downscale.raw(obs.hist.daily, obs.hist.monthly, scen.fut.monthly,
												years = sim_years,
												DScur_startyear = DScur_startyr, DScur_endyear = DScur_endyr,
												DSfut_startyear = future_yrs[it, "DSfut_startyr"], DSfut_endyear = future_yrs[it, "DSfut_endyr"],
												downscaling.options = downscaling.options,
												dailyPPTceiling = dailyPPTceiling,
												do_checks = FALSE)
						stopifnot(!inherits(scen.fut.daily, "try-error"))
						print(paste0(i, ", site_id = ", site_id, ", scenario_id = ", scenario_id, ", ", tolower(paste(tag, gcm, sep=".")), ", timeslice = ", rownames(future_yrs)[it], ": raw method: checks turned off for monthly->daily"))
					}
					data_blob <- dbW_weatherData_to_blob(scen.fut.daily)
					years <- as.integer(names(scen.fut.daily))
					types[[length(types)+1]] <- list(Site_id=site_id, Scenario_id=scenario_id, StartYear=years[1], EndYear=years[length(years)], weatherData=data_blob)
				}
				
				if ("delta" %in% downs) {
					scenario_id <- dbW_iScenarioTable[dbW_iScenarioTable[, "Scenario"] == tolower(paste("delta", tag, gcm, sep=".")), "id"]
					scen.fut.daily <- downscale.delta(obs.hist.daily, obs.hist.monthly, scen.hist.monthly, scen.fut.monthly,
											years = sim_years,
											DScur_startyear = DScur_startyr, DScur_endyear = DScur_endyr,
											DSfut_startyear = future_yrs[it, "DSfut_startyr"], DSfut_endyear = future_yrs[it, "DSfut_endyr"],
											downscaling.options = downscaling.options,
											dailyPPTceiling = dailyPPTceiling,
											do_checks = TRUE)
					if (inherits(scen.fut.daily, "try-error")) {#delta unsuccessful, replace with delta without checks
						scen.fut.daily <- downscale.delta(obs.hist.daily, obs.hist.monthly, scen.hist.monthly, scen.fut.monthly,
												years = sim_years,
												DScur_startyear = DScur_startyr, DScur_endyear = DScur_endyr,
												DSfut_startyear = future_yrs[it, "DSfut_startyr"], DSfut_endyear = future_yrs[it, "DSfut_endyr"],
												downscaling.options = downscaling.options,
												dailyPPTceiling = dailyPPTceiling,
												do_checks = FALSE)
						stopifnot(!inherits(scen.fut.daily, "try-error"))
						print(paste0(i, ", site_id = ", site_id, ", scenario_id = ", scenario_id, ", ", tolower(paste(tag, gcm, sep=".")), ", timeslice = ", rownames(future_yrs)[it], ": delta method: checks turned off for monthly->daily"))
					}
					data_blob <- dbW_weatherData_to_blob(scen.fut.daily)
					years <- as.integer(names(scen.fut.daily))
					types[[length(types)+1]] <- list(Site_id=site_id, Scenario_id=scenario_id, StartYear=years[1], EndYear=years[length(years)], weatherData=data_blob)
				}
				
				if ("hybrid-delta" %in% downs) {
					scenario_id <- dbW_iScenarioTable[dbW_iScenarioTable[, "Scenario"] == tolower(paste("hybrid-delta", tag, gcm, sep=".")), "id"]
					scen.fut.daily <- downscale.deltahybrid(obs.hist.daily, obs.hist.monthly, scen.hist.monthly, scen.fut.monthly,
											years = sim_years,
											DScur_startyear = DScur_startyr, DScur_endyear = DScur_endyr,
											DSfut_startyear = future_yrs[it, "DSfut_startyr"], DSfut_endyear = future_yrs[it, "DSfut_endyr"],
											downscaling.options = downscaling.options,
											do_checks = TRUE)
					if(inherits(scen.fut.daily, "try-error")){#delta-hybrid unsuccessful, replace with delta method
						scen.fut.daily <- downscale.delta(obs.hist.daily, obs.hist.monthly, scen.hist.monthly, scen.fut.monthly,
												years = sim_years,
												DScur_startyear = DScur_startyr, DScur_endyear = DScur_endyr,
												DSfut_startyear = future_yrs[it, "DSfut_startyr"], DSfut_endyear = future_yrs[it, "DSfut_endyr"],
												downscaling.options = downscaling.options,
												dailyPPTceiling = dailyPPTceiling,
												do_checks = FALSE)
						stopifnot(!inherits(scen.fut.daily, "try-error"))
						print(paste0(i, ", site_id = ", site_id, ", scenario_id = ", scenario_id, ", ", tolower(paste(tag, gcm, sep=".")), ", timeslice = ", rownames(future_yrs)[it], ": delta-hybrid replaced by delta method for monthly->daily"))
					}
					data_blob <- dbW_weatherData_to_blob(scen.fut.daily)
					years <- as.integer(names(scen.fut.daily))
					types[[length(types)+1]] <- list(Site_id=site_id, Scenario_id=scenario_id, StartYear=years[1], EndYear=years[length(years)], weatherData=data_blob)
				}
				
				if ("hybrid-delta-3mod" %in% downs) {
					scenario_id <- dbW_iScenarioTable[dbW_iScenarioTable[, "Scenario"] == tolower(paste("hybrid-delta-3mod", tag, gcm, sep=".")), "id"]                          

					scen.fut.daily <- downscale.deltahybrid3mod(obs.hist.daily, obs.hist.monthly, scen.hist.monthly, scen.fut.monthly,
											deltaFuture_yr = future_yrs[it, "delta"], years = sim_years,
											DScur_startyear = DScur_startyr, DScur_endyear = DScur_endyr,
											DSfut_startyear = future_yrs[it, "DSfut_startyr"], DSfut_endyear = future_yrs[it, "DSfut_endyr"],
											downscaling.options = downscaling.options,
											dailyPPTceiling = dailyPPTceiling,
											monthly_extremes = monthly_extremes,
											do_checks = TRUE)
					
					if (inherits(scen.fut.daily, "try-error")) {#delta-hybrid-3mod unsuccessful, replace with delta method
						scen.fut.daily <- downscale.delta(obs.hist.daily, obs.hist.monthly, scen.hist.monthly, scen.fut.monthly,
												years = sim_years,
												DScur_startyear = DScur_startyr, DScur_endyear = DScur_endyr,
												DSfut_startyear = future_yrs[it, "DSfut_startyr"], DSfut_endyear = future_yrs[it, "DSfut_endyr"],
												downscaling.options = downscaling.options,
												dailyPPTceiling = dailyPPTceiling,
												do_checks = FALSE)
						stopifnot(!inherits(scen.fut.daily, "try-error"))
						print(paste0(i, ", site_id = ", site_id, ", scenario_id = ", scenario_id, ", ", tolower(paste(tag, gcm, sep=".")), ", timeslice = ", rownames(future_yrs)[it], ": delta-hybrid-3mod replaced by delta method for monthly->daily"))
					}
					data_blob <- dbW_weatherData_to_blob(scen.fut.daily)
					years <- as.integer(names(scen.fut.daily))
					types[[length(types)+1]] <- list(Site_id=site_id, Scenario_id=scenario_id, StartYear=years[1], EndYear=years[length(years)], weatherData=data_blob)
				}
			}
			wdataOut[[ir]] <- types
		}
		saveRDS(wdataOut, file=file.path(dir.out.temp, gcm, paste(GCM_source, "_", i, ".rds", sep="")))
		res <- i
	
		res
	})


}


#--------------------------------------------------------------------------------------------------#
#------EXTRACT CLIMATE CHANGE DATA------
# Allow for multiple data sources
#	- among sites but not multiple sources per site (for that you need a new row in the MasterInput spreadsheet)
#	- NEX USA data has highest priority, then LLNL USA products, then global LLNL products

if(	exinfo$GDODCPUCLLNL || exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_NEX_USA){

	sites_GCM_source <- rep(NA, times=length(seq.tr))
	xy <- with(SWRunInformation[seq.tr,], data.frame(X_WGS84, Y_WGS84))

	i_use <- rep(FALSE, times=length(seq.tr))

	# determine which data product to use for each site based on bounding boxes of datasets
	if(exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_NEX_USA){
		i_use <- in_GMC_box(xy = xy, lats=c(24.0625, 49.9375), longs=c(-125.02083333, -66.47916667), i_use=i_use)
		sites_GCM_source[i_use] <- "CMIP5_BCSD_NEX_USA"
	}
	if(exinfo$ExtractClimateChangeScenarios_CMIP3_BCSD_GDODCPUCLLNL_USA){
		i_use <- in_GMC_box(xy = xy, lats=c(25.125, 52.875), longs=c(-124.625, -67), i_use=i_use)
		sites_GCM_source[i_use] <- "CMIP3_BCSD_GDODCPUCLLNL_USA"
	}
	if(exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_GDODCPUCLLNL_USA){
		i_use <- in_GMC_box(xy = xy, lats=c(25.125, 52.875), longs=c(-124.625, -67), i_use=i_use)
		sites_GCM_source[i_use] <- "CMIP5_BCSD_GDODCPUCLLNL_USA"
	}
	if(exinfo$ExtractClimateChangeScenarios_CMIP3_BCSD_GDODCPUCLLNL_Global){
		i_use <- in_GMC_box(xy = xy, lats=c(-55.25-0.25, 83.25+0.25), longs=c(-179.75-0.25, 179.75+0.25), i_use=i_use)
		sites_GCM_source[i_use] <- "CMIP3_BCSD_GDODCPUCLLNL_Global"
	}
	if(exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_GDODCPUCLLNL_Global){
		i_use <- in_GMC_box(xy = xy, lats=c(-55.25-0.25, 83.25+0.25), longs=c(-179.75-0.25, 179.75+0.25), i_use=i_use)
		sites_GCM_source[i_use] <- "CMIP5_BCSD_GDODCPUCLLNL_Global"
	}
	
	#write data to datafile.SWRunInformation
	SWRunInformation$GCM_sources[seq.tr] <- as.character(sites_GCM_source)
	write.csv(SWRunInformation, file=file.path(dir.in, datafile.SWRunInformation), row.names=FALSE)

	if(anyNA(sites_GCM_source)) warning("No climate change data available for ", sum(is.na(sites_GCM_source)), " sites")

	#access climate change data
	get_climatechange_data <- compiler::cmpfun(function(GCM_source, is_GDODCPUCLLNL, is_NEX, do_SWRun_sites) {
	#	GCM_source <- if(GCM_source == "CMIP5_BCSD_NEX_USA"){
	#					"CMIP5-BCSD-NEX-USA"
	#				} else if(grepl("CMIP3_BCSD_GDODCPUCLLNL", GCM_source)){
	#					"CMIP3-BCSD-GDO-DCP-UC-LLNL"
	#				} else if(grepl("CMIP5_BCSD_GDODCPUCLLNL", GCM_source)){
	#					"CMIP5-BCSD-GDO-DCP-UC-LLNL"
	#				} else {
	#					stop("No dataset for climate change conditions set")
	#				}
		if(!be.quiet) print(paste0("Started '", GCM_source, "' at ", Sys.time()))
	
		#Global flags
		repeatExtractionLoops_maxN <- 3
		#Specific flags
		if (is_GDODCPUCLLNL) {
			##gdo-dcp.ucllnl.org/downscaled_cmip_projections
			dir.ex.dat <- file.path(dir.ex.fut, "GDO_DCP_UCLLNL_DownscaledClimateData")
			if(GCM_source == "CMIP3_BCSD_GDODCPUCLLNL_USA") dir.ex.dat <- file.path(dir.ex.dat, "CMIP3_BCSD", "CONUS_0.125degree")
			if(GCM_source == "CMIP3_BCSD_GDODCPUCLLNL_Global") dir.ex.dat <- file.path(dir.ex.dat, "CMIP3_BCSD", "Global_0.5degree_MaurerEd")
			if(GCM_source == "CMIP5_BCSD_GDODCPUCLLNL_USA") dir.ex.dat <- file.path(dir.ex.dat, "CMIP5_BCSD", "CONUS_0.125degree_r1i1p1")
			if(GCM_source == "CMIP5_BCSD_GDODCPUCLLNL_Global") dir.ex.dat <- file.path(dir.ex.dat, "CMIP5_BCSD", "Global_0.5degree_r1i1p1")
		
			#CMIP3 Global and USA
			#	- obs: 1950 Jan to 1999 Dec
			#	- SRES: 1950 Jan to 2099 Dec
			#	- all same time + spatial coordinates
			#CMIP5 Global and USA
			#	- historical: 1950 Jan to 2005 Dec (except: HadGEM2-CC and HadGEM2-ES, to 2005 Nov)
			#	- RCPs:
			#		- in general: 2006 Jan to 2099 Dec or 2100 Dec
			#		- HadGEM2-CC and HadGEM2-ES: 2005 Dec to 2099 Dec
			#		- RCP45 & Globus: HadGEM2-ES: 2005 Dec to 2099 Nov
			#		- RCP45: HadCM2 and MIROC4h: 2006 Jan to 2035 Dec
			#		- no RCP85: GISS-E2-H-CC, GISS-E2-R-CC
			#	=> ignore missing Dec value; ignore 2005 Dec value if that is the start
			#	- all same spatial coordinates
		
			scenariosDB <- list.dirs(dir.ex.dat, full.names=FALSE, recursive=FALSE)
			if(any((temp <- sapply(scenariosDB, FUN=function(x) length(list.files(file.path(dir.ex.dat, x))))) == 0)) scenariosDB <- scenariosDB[temp > 0]
		
			gcmsDB <- unique(unlist(sapply(scenariosDB, FUN=function(x) sapply(strsplit(list.files(file.path(dir.ex.dat, x)), split="_", fixed=TRUE), FUN=function(x) x[5]))))	
			gcmFiles <- list.files(dir.ex.dat, recursive=TRUE, full.names=TRUE)
		
			print_int <- 100
			saveNEXtempfiles <- FALSE
		}
		if(is_NEX){
			##https://portal.nccs.nasa.gov/portal_home/published/NEX.html
			opt <- options("timeout")
			options(timeout=5*60)

			dir.ex.dat <- NULL
			if (useRCurl) {
				stopifnot(RCurl::url.exists("https://portal.nccs.nasa.gov/portal_home/published/NEX.html")) #check whether we are online
				saveNEXtempfiles <- FALSE
			} else {
				saveNEXtempfiles <- TRUE
			}

			gcmsDB <- c("inmcm4", "bcc-csm1-1", "bcc-csm1-1-m", "NorESM1-M", "MRI-CGCM3", "MPI-ESM-MR", "MPI-ESM-LR", "MIROC5", "MIROC-ESM", "MIROC-ESM-CHEM", "IPSL-CM5B-LR", "IPSL-CM5A-MR", "IPSL-CM5A-LR", "HadGEM2-ES", "HadGEM2-CC", "HadGEM2-AO", "GISS-E2-R", "GFDL-ESM2M", "GFDL-ESM2G", "GFDL-CM3", "FIO-ESM", "FGOALS-g2", "CanESM2", "CSIRO-Mk3-6-0", "CNRM-CM5", "CMCC-CM", "CESM1-CAM5", "CESM1-BGC", "CCSM4", "BNU-ESM", "ACCESS1-0")
			scenariosDB <- c("historical", "rcp26", "rcp45", "rcp60", "rcp85")
			gcmFiles <- NULL
			
			print_int <- 1
		}
	
		#Force dataset specific lower/uper case for GCMs and RCPs
		reqGCMs <- gcmsDB[match(tolower(reqGCMs), tolower(gcmsDB), nomatch=NA)]
		reqRCPs <- scenariosDB[match(tolower(reqRCPs), tolower(scenariosDB), nomatch=NA)]
		reqRCPsPerGCM <- lapply(reqRCPsPerGCM, FUN=function(r) scenariosDB[match(tolower(r), tolower(scenariosDB), nomatch=NA)])
	
		#Tests that all requested conditions will be extracted
		stopifnot(length(reqGCMs) > 0, all(!is.na(reqGCMs)))
		stopifnot(length(reqRCPs) > 0, all(!is.na(reqRCPs)), any(grepl("historical", scenariosDB, ignore.case=TRUE)))

		#put requests together
		locations <- with(SWRunInformation[do_SWRun_sites, ], data.frame(X_WGS84, Y_WGS84, WeatherFolder))	#locations of simulation runs
		requestN <- length(reqGCMs) * nrow(locations)
		if(!be.quiet) print(paste("'", GCM_source, "' will run", requestN, "times"))
	
		#bounding box
		bbox <- data.frame(matrix(NA, nrow=2, ncol=2, dimnames=list(NULL, c("lat", "lon"))))
		if(is_NEX){
			bbox$lat <- c(24.0625, 49.9375)
			bbox$lon <- c(-125.02083333, -66.47916667)
		}
		if(GCM_source == "CMIP3_BCSD_GDODCPUCLLNL_USA" || GCM_source == "CMIP5_BCSD_GDODCPUCLLNL_USA"){
			bbox$lat <- c(25.125, 52.875)
			bbox$lon <- c(-124.625, -67)
		}
		if(GCM_source == "CMIP3_BCSD_GDODCPUCLLNL_Global" || GCM_source == "CMIP5_BCSD_GDODCPUCLLNL_Global"){
			bbox$lat <- c(-55.25-0.25, 83.25+0.25)
			bbox$lon <- c(-179.75-0.25, 179.75+0.25)
		}
	
		#time box
		tbox <- data.frame(matrix(NA, nrow=2, ncol=2, dimnames=list(c("start", "end"), c("first", "second"))))
		if(is_NEX || GCM_source == "CMIP5_BCSD_GDODCPUCLLNL_USA" || GCM_source == "CMIP5_BCSD_GDODCPUCLLNL_Global"){
			tbox$first <- c(1950, 2005)
			tbox$second <- c(2006, 2099)
		}
		if(GCM_source == "CMIP3_BCSD_GDODCPUCLLNL_USA" || GCM_source == "CMIP3_BCSD_GDODCPUCLLNL_Global"){
			tbox$first <- c(1950, 1999)
			tbox$second <- c(2000, 2099)
		}
		#timing: time slices: data is organized into 'historical' runs 1950-2005 (="first") and future 'rcp' runs 2006-2099 (="second")
		timeSlices <- data.frame(matrix(NA, ncol=4, nrow = 4 + 4 * nrow(future_yrs), dimnames = list(NULL, c("Run", "Slice", "Time", "Year"))))
		timeSlices[, 1:3] <- expand.grid(c("start", "end"), c("first", "second"), c("historical", rownames(future_yrs)))[, 3:1]
		#historic conditions for downscaling
		timeSlices[1, 4] <- max(tbox$first[1], DScur_startyr)
		timeSlices[2, 4] <- min(tbox$first[2], DScur_endyr)
		if(DScur_endyr > tbox$first[2]){
			timeSlices[3, 4] <- tbox$second[1]
			timeSlices[4, 4] <- min(tbox$second[2], DScur_endyr)
		}
		#future conditions for downscaling
		for(it in 1:nrow(future_yrs)){
			timeSlices[3 + 4*it, 4] <- max(tbox$second[1], future_yrs[it, "DSfut_startyr"])
			timeSlices[4 + 4*it, 4] <- min(tbox$second[2], future_yrs[it, "DSfut_endyr"])	#limits timeSlices to 2099
			if (DScur_startyr < 1950){ #TODO(drs): I don't know where the hard coded value of 1950 comes from; it doesn't make sense to me
			  print("Note: adjustment to 'timeSlices' because 'DScur_startyr < 1950'")
			  timeSlices[4 + 4*it, 4] <- min(timeSlices[4 + 4*it, 4], timeSlices[4 + 3*it, 4]+(timeSlices[4,4]-timeSlices[1,4]))
			}
			if(future_yrs[it, "DSfut_startyr"] < tbox$second[1]){
				timeSlices[1 + 4*it, 4] <- max(tbox$first[1], future_yrs[it, "DSfut_startyr"])
				timeSlices[2 + 4*it, 4] <- tbox$second[1]
			}
		}
		#get unique time slices
		temp1 <- unique_times(timeSlices, slice = "first")
		temp2 <- unique_times(timeSlices, slice = "second")
		getYears <- list(n_first = nrow(temp1), first = temp1, n_second = nrow(temp2), second = temp2)
		
		#Monthly time-series
		getYears$first_dates <- lapply(1:getYears$n_first, FUN=function(it) as.POSIXlt(seq(from=as.POSIXlt(paste0(getYears$first[it, 1], "-01-01")), to=as.POSIXlt(paste0(getYears$first[it, 2], "-12-31")), by="1 month")))
		getYears$second_dates <- lapply(1:getYears$n_second, FUN=function(it) as.POSIXlt(seq(from=as.POSIXlt(paste0(getYears$second[it, 1], "-01-01")), to=as.POSIXlt(paste0(getYears$second[it, 2], "-12-31")), by="1 month")))
		#Days per month
		getYears$first_dpm <- lapply(1:getYears$n_first, FUN=function(it) rle(as.POSIXlt(seq(from=as.POSIXlt(paste0(getYears$first[it, 1], "-01-01")), to=as.POSIXlt(paste0(getYears$first[it, 2], "-12-31")), by="1 day"))$mon)$lengths)
		getYears$second_dpm <- lapply(1:getYears$n_second, FUN=function(it) rle(as.POSIXlt(seq(from=as.POSIXlt(paste0(getYears$second[it, 1], "-01-01")), to=as.POSIXlt(paste0(getYears$second[it, 2], "-12-31")), by="1 day"))$mon)$lengths)	

		#Logical on how to select from getYears
		assocYears <- vector("list", length = 1 + length(reqRCPs) * nrow(future_yrs))
		names_assocYears <- c("historical", paste0(rownames(future_yrs), ".", rep(reqRCPs, each = nrow(future_yrs))))
		for(it in seq_along(assocYears)){
			temp <- strsplit(names_assocYears[it], ".", fixed=TRUE)[[1]][[1]]
			assocYears[[it]] <- list(first = useSlices(getYears, timeSlices, run = temp, slice = "first"),
									second = useSlices(getYears, timeSlices, run = temp, slice = "second"))
		}
		names(assocYears) <- names_assocYears
		
		print(paste("Future scenario data will be extracted for a time period spanning ", timeSlices[7,4], "through",  max(na.omit(timeSlices[,4]))))
		#Variable tags
		if(is_NEX){
			varTags <- c(prcp = "pr", tmin = "tasmin", tmax = "tasmax") #units c("kg/m2/s", "K", "K") --> SoilWat required units c("cm/day", "C", "C")
			fileVarTags <- NULL
		}
		if(GCM_source == "CMIP3_BCSD_GDODCPUCLLNL_USA" || GCM_source == "CMIP3_BCSD_GDODCPUCLLNL_Global"){
			varTags <- c(prcp = "Prcp", tmean = "Tavg", tmin = "Tmin", tmax = "Tmax")
			fileVarTags <- paste("monthly", varTags, sep=".")
			names(fileVarTags) <- names(varTags)
		}
		if(GCM_source == "CMIP5_BCSD_GDODCPUCLLNL_USA" || GCM_source == "CMIP5_BCSD_GDODCPUCLLNL_Global"){
			varTags <- c(prcp = "pr", tmean = "tas", tmin = "tasmin", tmax = "tasmax")
			fileVarTags <- paste0("_", varTags, "_")
			names(fileVarTags) <- names(varTags)
		}

	
		try.ScenarioWeather <- function(i) {
			temp <- try(calc.ScenarioWeather(i = i,
							GCM_source = GCM_source, is_GDODCPUCLLNL = is_GDODCPUCLLNL, is_NEX = is_NEX,
							gcmFiles = gcmFiles,
							reqGCMs = reqGCMs, reqRCPsPerGCM = reqRCPsPerGCM, reqDownscalingsPerGCM = reqDownscalingsPerGCM,
							climate.ambient = climate.ambient,
							bbox = bbox,
							locations = locations,
							dbW_iSiteTable = dbW_iSiteTable,
							varTags = varTags, fileVarTags = fileVarTags,
							getYears = getYears, assocYears = assocYears, future_yrs = future_yrs,
							simstartyr = simstartyr, endyr = endyr,
							DScur_startyr = DScur_startyr, DScur_endyr = DScur_endyr,
							downscaling.options = downscaling.options,
							dir.out.temp = dir.out.temp,
							be.quiet = be.quiet,
							useRCurl = useRCurl, saveNEXtempfiles = saveNEXtempfiles),
						silent = FALSE)
			
			if (inherits(temp, "try-error")) {
				print(paste(Sys.time(), temp))
				save(i, temp, file = file.path(dir.out.temp, paste0("failed_", i, ".RData")))
				res <- NULL
			} else {
				res <- i
			}
	
			res
		}

		#objects that need exporting to workers
		list.export <- c("dir.out.temp", "dir.ex.dat", "GCM_source", "is_GDODCPUCLLNL", "is_NEX", "reqGCMs", "reqRCPsPerGCM", "reqDownscalingsPerGCM", "locations", "climScen", "varTags", "be.quiet", "getYears", "assocYears", "future_yrs", "DScur_startyr", "DScur_endyr", "simstartyr", "endyr", "dbWeatherDataFile", "climate.ambient", "dbW_iSiteTable", "dbW_iScenarioTable", "bbox", "GCM_source", "print_int",
				"calc.ScenarioWeather", "get_GCMdata", "get.DBvariable",
				"downscale.raw", "downscale.delta", "downscale.deltahybrid", "downscale.deltahybrid3mod", "downscale.periods",
				"in_GMC_box", "unique_times", "useSlices", "erf", "add_delta_to_PPT", "fix_PPTdata_length", "calc_Days_withLoweredPPT", "controlExtremePPTevents", "test_sigmaNormal", "test_sigmaGamma", "stretch_values", 
				"applyDeltas", "applyPPTdelta_simple", "applyPPTdelta_detailed", "applyDelta_oneYear", "applyDeltas2",
				"doQmapQUANT.default_drs", "doQmapQUANT_drs", "applyDeltas2", "downscaling.options")
		if (is_NEX) {
			list.export <- c(list.export, "saveNEXtempfiles", "useRCurl",  
				"mmPerSecond_to_cmPerMonth", "get.request")
		}
		if (is_GDODCPUCLLNL) {
			list.export <- c(list.export, "fileVarTags", "gcmFiles",  
				"mmPerDay_to_cmPerMonth", "whereNearest", "get.TimeIndices", "get.SpatialIndices", "do_ncvar_get")
		}

		tryToGet_ClimDB <- function(is_ToDo) {
			#requests is_ToDo: fastest if nc file is
			#	- DONE: permutated to (lat, lon, time) instead (time, lat, lon)
			#	- TODO: many sites are extracted from one nc-read instead of one site per nc-read (see benchmarking_GDODCPUCLLNL_extractions.R)
			#TODO: create chunks for is_ToDo of size sites_per_chunk_N that use the same access to a nc file and distribute among workersN
	
			if(parallel_runs && parallel_init){
				is_ToDo <- sample(x=is_ToDo, size=length(is_ToDo)) #attempt to prevent reading from same .nc at the same time

				# extract the GCM data depending on parallel backend
				if (identical(parallel_backend, "mpi")) {
					exportObjects(list.export)
					if(is_NEX && useRCurl && !saveNEXtempfiles)
						mpi.bcast.cmd(library("RCurl", quietly=TRUE))
					if(is_GDODCPUCLLNL)
						mpi.bcast.cmd(library("ncdf4", quietly=TRUE))
					mpi.bcast.cmd(library("Rsoilwat31", quietly=TRUE))
					mpi.bcast.cmd(Rsoilwat31::dbW_setConnection(dbFilePath=dbWeatherDataFile))
				
					i_Done <- mpi.applyLB(x = is_ToDo, fun = try.ScenarioWeather)
				
					mpi.bcast.cmd(Rsoilwat31::dbW_disconnectConnection())
					mpi.bcast.cmd(rm(list=ls()))
					mpi.bcast.cmd(gc())
					
				} else if(identical(parallel_backend, "snow")) {
					snow::clusterExport(cl, list.export, envir=parent.frame())
					if(is_NEX && useRCurl && !saveNEXtempfiles)
						snow::clusterEvalQ(cl, library("RCurl", quietly = TRUE))
					if(is_GDODCPUCLLNL)
						snow::clusterEvalQ(cl, library("ncdf4", quietly=TRUE))
					snow::clusterEvalQ(cl, library("Rsoilwat31", quietly=TRUE))
					snow::clusterEvalQ(cl, Rsoilwat31::dbW_setConnection(dbFilePath=dbWeatherDataFile))
				
					i_Done <- snow::clusterApplyLB(cl, x = is_ToDo, fun = try.ScenarioWeather)
				
					snow::clusterEvalQ(cl, Rsoilwat31::dbW_disconnectConnection())
					snow::clusterEvalQ(cl, rm(list=ls()))
					snow::clusterEvalQ(cl, gc())
					
				} else if(identical(parallel_backend, "multicore")) {
					packages.export <- "Rsoilwat31"
					if(is_NEX && useRCurl && !saveNEXtempfiles)
						packages.export <- c(packages.export, "RCurl")
					if(is_GDODCPUCLLNL)
						packages.export <- c(packages.export, "ncdf4")
					i_Done <- foreach(i=is_ToDo, .combine="c", .errorhandling="remove", .inorder=FALSE, .export=list.export, .packages=packages.export) %dopar% {
						Rsoilwat31::dbW_setConnection(dbFilePath=dbWeatherDataFile)
						temp <- try.ScenarioWeather(i)
						Rsoilwat31::dbW_disconnectConnection()
						return(temp)
					}
				} else {
					i_Done <- NULL
				}
				
			} else {
				Rsoilwat31::dbW_setConnection(dbFilePath=dbWeatherDataFile)
				i_Done <- foreach(i=is_ToDo, .combine="c", .errorhandling="remove", .inorder=FALSE) %do% try.ScenarioWeather(i)
				Rsoilwat31::dbW_disconnectConnection()
			}
		

			if(!be.quiet) print(paste("Started adding temporary files into database '", GCM_source, "' at", Sys.time()))
			Rsoilwat31::dbW_setConnection(dbFilePath=dbWeatherDataFile)
			temp.files <- list.files(path=dir.out.temp, pattern=GCM_source, recursive=TRUE, include.dirs=FALSE, no..=TRUE)
			if (length(temp.files) > 0) {
				for (k in seq_along(temp.files)) {
					wdataOut <- readRDS(file=ftemp <- file.path(dir.out.temp, temp.files[k]))
					for (j in seq_along(wdataOut)) {
						for (l in seq_along(wdataOut[[j]])) {
							res <- try(Rsoilwat31:::dbW_addWeatherDataNoCheck(Site_id=wdataOut[[j]][[l]]$Site_id, Scenario_id=wdataOut[[j]][[l]]$Scenario_id, StartYear=wdataOut[[j]][[l]]$StartYear, EndYear=wdataOut[[j]][[l]]$EndYear, weatherData=wdataOut[[j]][[l]]$weatherData), silent=TRUE)
							if(inherits(res, "try-error")) break
						}
						if(inherits(res, "try-error")) break
					}
					if(!inherits(res, "try-error")) unlink(ftemp)
				}
			}
			Rsoilwat31::dbW_disconnectConnection()
		
			sort(unlist(i_Done))
		}

		#Repeat call to get climate data for all requests until complete
		repeatN <- 0
		i_AllToDo <- 1:requestN
		i_Done <- NULL
	
		logFile <- file.path(dir.out, paste0("extractionsDone_", GCM_source, ".rds"))
		if(file.exists(logFile)){
			i_Done <- sort(unique(c(i_Done, readRDS(file=logFile))))
		}
		temp.files <- list.files(path=dir.out.temp, pattern=GCM_source, recursive=TRUE, include.dirs=FALSE, no..=TRUE)
		if(length(temp.files) > 0){
			i_Done <- sort(unique(c(i_Done, as.integer(unlist(strsplit(unlist(strsplit(temp.files, split="_", fixed=TRUE))[c(FALSE, TRUE)], split=".", fixed=TRUE))[c(TRUE, FALSE)]))))
		}
		while(repeatExtractionLoops_maxN > repeatN && length(i_ToDo <- if(length(i_Done) > 0) i_AllToDo[-i_Done] else i_AllToDo) > 0){
			repeatN <- repeatN + 1
			if(!be.quiet) print(paste("'", GCM_source, "' will run the", repeatN, ". time to extract", length(i_ToDo), "requests" ))
			i_Done <- sort(unique(c(i_Done, tryToGet_ClimDB(is_ToDo=i_ToDo))))
			saveRDS(i_Done, file=logFile)
		}
		rm(i_ToDo, logFile)

		#Clean up: report unfinished locations, etc.
		if(length(i_ToDo <- if(length(i_Done) > 0) i_AllToDo[-i_Done] else i_AllToDo) > 0){
			warning(paste(length(i_ToDo), "sites didn't extract climate scenario information by '", GCM_source, "'"))
			failedLocations_DB <- locations[temp <- unique((i_ToDo - 1) %/% length(reqGCMs) + 1), ]
			include_YN_updateFailed <- include_YN
			include_YN_updateFailed[do_SWRun_sites][temp] <- 0
			save(failedLocations_DB, include_YN_updateFailed, file=file.path(dir.in, "failedLocations_ClimDB.RData"))
			SWRunInformation_updateFailed <- cbind(SWRunInformation, include_YN_updateFailed=include_YN_updateFailed)
			write.csv(SWRunInformation_updateFailed, file=file.path(dir.in, paste0("failedLocationsUpdated_ClimDB_", GCM_source, "_", datafile.SWRunInformation)))
			rm(failedLocations_DB, include_YN_updateFailed, SWRunInformation_updateFailed)
		}
	
		if(!be.quiet) print(paste("Finished '", GCM_source, "' at", Sys.time()))
	
		rm(locations, requestN, i_Done, i_ToDo, i_AllToDo, varTags, timeSlices, getYears, assocYears, gcmsDB, scenariosDB)

		invisible(0)
	})
	
	for (isource in sort(unique(sites_GCM_source))) {
		get_climatechange_data(GCM_source = isource,
								is_GDODCPUCLLNL = grepl("BCSD_GDODCPUCLLNL", isource),
								is_NEX = isource == "CMIP5_BCSD_NEX_USA",
								do_SWRun_sites = seq.tr[sites_GCM_source == isource])
	}

	rm(sites_GCM_source, xy, i_use, id_sites, get_climatechange_data)
}


#-CMIP3_ClimateWizardEnsembles does not allow for multiple sources
if(	exinfo$ExtractClimateChangeScenarios_CMIP3_ClimateWizardEnsembles_Global || exinfo$ExtractClimateChangeScenarios_CMIP3_ClimateWizardEnsembles_USA){
	
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
			
			for(sc in seq_along(list.scenarios.datafile)){
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

#------END CLIMATE CHANGE DATA------


#--------------------------------------------------------------------------------------------------#
#------EXTRACT SOIL CHARACTERISTICS------
if(exinfo$ExtractSoilDataFromCONUSSOILFromSTATSGO_USA || exinfo$ExtractSoilDataFromISRICWISEv12_Global){
	#allow for multiple sources
	if(extract_determine_database == "order"){
		sites_externalsoils_source <- rep(NA, times=length(seq.tr))
	} else if(extract_determine_database == "SWRunInformation"){
		sites_externalsoils_source <- SWRunInformation$SoilTexture_source[seq.tr]
	} else {
		stop(paste("Value of 'extract_determine_database'", extract_determine_database, "not implemented"))
	}
	
	if(exinfo$ExtractSoilDataFromCONUSSOILFromSTATSGO_USA){
		if(!be.quiet) print(paste("Started 'ExtractSoilDataFromCONUSSOILFromSTATSGO_USA' at", Sys.time()))
		#Miller, D. A. and R. A. White. 1998. A conterminous United States multilayer soil characteristics dataset for regional climate and hydrology modeling. Earth Interactions 2:1-26.
		#CONUS-SOIL: rasterized and controlled STATSGO data; information for 11 soil layers available
		do_extract <- is.na(sites_externalsoils_source) | (sites_externalsoils_source == "CONUSSOILFromSTATSGO_USA")
		if(sum(do_extract) > 0){
			ldepth <- c(5, 10, 20, 30, 40, 60, 80, 100, 150, 200, 250)	#in cm
	
			dir.ex.dat <- file.path(dir.ex.soil, "CONUSSoil", "output", "albers")
	
			#locations of simulation runs
			locations <- SpatialPoints(coords=with(SWRunInformation[seq.tr[do_extract],], data.frame(X_WGS84, Y_WGS84)), proj4string=CRS("+proj=longlat +datum=WGS84"))
	
			#extract data
			soil_data <- array(NA, dim=c(sum(do_extract), 11, 5), dimnames=list(NULL, paste0("L", 1:11), c("bedrock", "matricd", "rockvol", "sand", "clay")))
			g <- brick(file.path(dir.ex.dat, "bd.tif"))
			locations.CoordG <- spTransform(locations, CRS=CRS(proj4string(g)))	#transform points to grid-coords
			soil_data[, , "matricd"] <- extract(g, locations.CoordG) / 100
	
			g <- raster(file.path(dir.ex.dat, "rockdepm.tif"))
			soil_data[, 1, "bedrock"] <- extract(g, locations.CoordG) #depth in cm >< bedrock from datafile.bedrock, but seems to make more sense?
			lys <- 1:max(findInterval(soil_data[, 1, "bedrock"], ldepth), na.rm=TRUE)

			#New with v31: rockvol -> gravel vol%
			g <- brick(file.path(dir.ex.dat, "rockvol.tif"))
			rockvol <- extract(g, locations.CoordG) / 100
	
			g <- brick(file.path(dir.ex.dat, "sand.tif"))
			sand <- extract(g, locations.CoordG) / 100
			g <- brick(file.path(dir.ex.dat, "clay.tif"))
			clay <- extract(g, locations.CoordG) / 100
			g <- brick(file.path(dir.ex.dat, "silt.tif"))
			silt <- extract(g, locations.CoordG) / 100

			if(FALSE){#visualize in interactive sessions
				temp <- sand
				cats <- addNA(cut(temp[, 1], breaks=seq(0, to=max(1, max(temp, na.rm=TRUE)), length.out=11)))
				cols <- c(head(rainbow(n=nlevels(cats)), n=-1), "gray")
				plot(locations, pch=15, cex=0.5, col=cols[cats])
				legend(x="bottomleft", legend=sQuote(levels(cats)), pch=19, col=cols)
				if(require("maps")) map("state", add=TRUE)
			}

			#Normalize to 0-1
			total_matric <- sand + clay + silt
			total_bulk <- total_matric + rockvol
			sand <- round(sand / total_matric, 2) # mass fraction of matric component
			soil_data[, , "sand"] <- ifelse(is.finite(sand), sand, NA)
			clay <- round(clay / total_matric, 2) # mass fraction of matric component
			soil_data[, , "clay"] <- ifelse(is.finite(clay), clay, NA)
			rockvol <- round(rockvol / total_bulk, 2) # volume fraction of bulk=total soil
			soil_data[, , "rockvol"] <- ifelse(is.finite(rockvol), rockvol, NA)

			#Convert bulk density to matric density
			#	eqn. 20 from Saxton et al. 2006: bulkd <- matricd * (1 - rockvol) + rockvol * 2.65
	# TODO: unclear whether this is bulk or matric density, Miller et al. 1998 has labelled this as bulk, but it appears as if it is matric because eq. 20 (Saxton et al. 2006) would give otherwise negative values for matricd (bulkd <- matricd * (1 - rockvol) + rockvol * 2.65)
			#matricd <- ifelse(abs(1 - rockvol) > sqrt(.Machine$double.eps), (bulkd - rockvol * 2.65) / (1 - rockvol), 0)
		
			i_good <- complete.cases(soil_data[, 1, ]) #length(i_good) == sum(do_extract)
		
			if(sum(i_good) > 0){
				i_Done <- rep(FALSE, times=length(seq.tr)) #length(i_Done) == length(seq.tr)
				i_Done[which(do_extract)[i_good]] <- TRUE #sum(i_Done) == sum(i_good)

				if(extract_determine_database == "order"){
					sites_externalsoils_source[i_Done] <- "CONUSSOILFromSTATSGO_USA"
				} else if(extract_determine_database == "SWRunInformation"){
					sites_externalsoils_source[which(do_extract)[!i_good]] <- NA
				}
						
				#set and save soil layer structure
				sw_input_soillayers[seq.tr[i_Done], "SoilDepth_cm"] <- soil_data[i_good, 1, "bedrock"]
				sw_input_soillayers[seq.tr[i_Done], 2+lys] <- matrix(data=rep(ldepth[lys], times=sum(i_good)), ncol=length(lys), byrow=TRUE)
				write.csv(sw_input_soillayers, file=file.path(dir.in, datafile.soillayers), row.names=FALSE)

				#set and save soil texture
				#add data to sw_input_soils and set the use flags
				i.temp <- grepl(pattern="Matricd_L", x=names(sw_input_soils_use))
				sw_input_soils[seq.tr[i_Done], i.temp][, lys] <- soil_data[i_good, lys, "matricd"]
				sw_input_soils_use[i.temp][lys] <- 1
				i.temp <- grepl(pattern="GravelContent_L", x=names(sw_input_soils_use))
				sw_input_soils[seq.tr[i_Done], i.temp][, lys] <- soil_data[i_good, lys, "rockvol"]
				sw_input_soils_use[i.temp][lys] <- 1
				i.temp <- grepl(pattern="Sand_L", x=names(sw_input_soils_use))
				sw_input_soils[seq.tr[i_Done], i.temp][, lys] <- soil_data[i_good, lys, "sand"]
				sw_input_soils_use[i.temp][lys] <- 1
				i.temp <- grepl(pattern="Clay_L", x=names(sw_input_soils_use))
				sw_input_soils[seq.tr[i_Done], i.temp][, lys] <- soil_data[i_good, lys, "clay"]
				sw_input_soils_use[i.temp][lys] <- 1

				#write data to datafile.soils
				tempdat <- rbind(sw_input_soils_use, sw_input_soils)
				write.csv(tempdat, file=file.path(dir.sw.dat, datafile.soils), row.names=FALSE)
		
				rm(tempdat, i.temp)
			}
			rm(lys, total_bulk, total_matric, rockvol, sand, clay, silt, g, locations, soil_data)
		}
		
		if(!be.quiet) print(paste("Finished 'ExtractSoilDataFromCONUSSOILFromSTATSGO_USA' at", Sys.time()))
	}

	if(exinfo$ExtractSoilDataFromISRICWISEv12_Global){
		if(!be.quiet) print(paste("Started 'ExtractSoilDataFromISRICWISEv12_Global' at", Sys.time()))
		#Batjes, N. H. 2012. ISRIC-WISE derived soil properties on a 5 by 5 arc-minutes global grid (ver. 1.2). Report 2012/01 (with data set, available at www.isric.org). ISRIC-World Soil Information, Wageningen, The Netherlands.
		#http://www.isric.org/data/isric-wise-derived-soil-properties-5-5-arc-minutes-global-grid-version-12
		#cells with no soil values have SUID=c(0=Water, 6997=Water, 6694=Rock, or 6998=Glacier)
		do_extract <- is.na(sites_externalsoils_source) | (sites_externalsoils_source == "ISRICWISEv12_Global")
		if(sum(do_extract) > 0){
		
			layer_N <- 5	#WISE contains five soil layers for each prid
			layer_Nsim <- 6	#WISE contains five soil layers for each prid; I added one layer to account for lithosols (Ix), which have a soildepth of 10 cm; for all other soil types, my layers 0-10 cm and 10-20 cm contain the same wise information
			layer_TopDep <- c(0, 10, 20, 40, 60, 80)	#in cm
			layer_BotDep <- c(10, 20, 40, 60, 80)	#in cm
	
			dir.ex.dat <- file.path(dir.ex.soil, "wise5by5min_v1b")
			stopifnot(file.exists(dir.ex.dat), require(raster), require(sp), require(rgdal))
	
			#locations of simulation runs
			locations <- SpatialPoints(coords=with(SWRunInformation[seq.tr[do_extract],], data.frame(X_WGS84, Y_WGS84)), proj4string=CRS("+proj=longlat +datum=WGS84"))
			is_ToDo <- seq_along(locations)
		
			#---extract data
			grid_wise <- raster(x=file.path(dir.ex.dat, "Grid", "smw5by5min"))

			#- List all the wise cells that are covered by the grid cell or point location
			if(extract_gridcell_or_point == "point"){
				suids <- extract(grid_wise, locations)
				sim_cells_SUIDs <- data.frame(i=is_ToDo, SUIDs_N=1, SUID=suids, fraction=1)
			
			} else if(extract_gridcell_or_point == "gridcell"){
				extract_SUIDs <- function(i, res=0){
					# coord: X, Y coordinates of point or cell center
					# res: grid cell resolution
					.local <- function(i){
						coord <- coordinates(locations[i, ])
						resp <- list(i=i, SUIDs_N=0, SUID=NULL, fraction=NULL)

						cells_wise <- cellsFromExtent(object=grid_wise, extent=extent(coord[1] - res/2, coord[1] + res/2, coord[2] - res/2, coord[2] + res/2), expand=FALSE)
						if(!is.null(cells_wise)){
							minmax_cells_wise <- range(cells_wise, na.rm=TRUE)
							xy_minmax_cells_wise <- xyFromCell(object=grid_wise, cell=minmax_cells_wise)

							icol <- colFromX(object=grid_wise, x=xy_minmax_cells_wise[, 1])
							irow <- rowFromY(object=grid_wise, y=xy_minmax_cells_wise[, 2])
		
							#extract values
							sval <- sort(getValuesBlock(x=grid_wise, row=irow[1], nrow=1 + diff(irow), col=icol[1], ncol=1 + diff(icol))) #getValuesBlock() faster than extract() for one location
							if(length(sval) != length(cells_wise)) sval <- sval[sample(x=length(sval), size=length(cells_wise))]

							#calculate proportions of each value
							temp <- rle(sval)

							resp$SUIDs_N <- length(temp$values)
							resp$SUID <- temp$values
							resp$fraction <- temp$lengths / length(cells_wise)
						}
				
						return(resp)
					}
			
					temp <- try(.local(i), silent=TRUE)
					return(if(!inherits(temp, "try-error")) temp else list(i=i, SUIDs_N=-1, SUID=NULL, fraction=NULL))
				}

				if(parallel_runs && parallel_init){
					#objects that need exporting to slaves
					list.export <- c("grid_wise", "locations")
					#call the simulations depending on parallel backend
					if(identical(parallel_backend, "mpi")) {
						exportObjects(list.export)
						mpi.bcast.cmd(library(raster, quietly=TRUE))
				
						sim_cells_SUIDs <- mpi.applyLB(x=is_ToDo, fun=extract_SUIDs, res=gridcell_resolution)
						sim_cells_SUIDs <- do.call(rbind, sim_cells_SUIDs)
				
						mpi.bcast.cmd(rm(list=ls()))
						mpi.bcast.cmd(gc())
					} else if(identical(parallel_backend, "snow")) {
						snow::clusterExport(cl, list.export, envir=parent.frame())
						snow::clusterEvalQ(cl, library(raster, quietly = TRUE))
				
						sim_cells_SUIDs <- snow::clusterApplyLB(cl, x=is_ToDo, fun=extract_SUIDs, res=gridcell_resolution)
						sim_cells_SUIDs <- do.call(rbind, sim_cells_SUIDs)
				
						snow::clusterEvalQ(cl, rm(list=ls()))
						snow::clusterEvalQ(cl, gc())
					} else if(identical(parallel_backend, "multicore")) {
						packages.export <- "raster"
						sim_cells_SUIDs <- foreach(i=is_ToDo, .combine="rbind", .errorhandling="remove", .inorder=FALSE, .export=list.export, .packages=packages.export) %dopar% extract_SUIDs(i, res=gridcell_resolution)
					} else {
						sim_cells_SUIDs <- NULL
					}
				} else {
					sim_cells_SUIDs <- foreach(i=is_ToDo, .combine="rbind", .errorhandling="remove", .inorder=FALSE) %do% extract_SUIDs(i, res=gridcell_resolution)
				}
			} else {
				stop("Flag 'extract_gridcell_or_point' has no accepted value, i.e., 'point' or 'gridcell'")
			}
			rm(grid_wise)

			sim_cells_SUIDs <- sim_cells_SUIDs[order(unlist(sim_cells_SUIDs[,"i"])),]

			#- Calculate simulation cell wide weighted values based on each PRID weighted by SUID.fraction x PRIP.PROP
			dat_wise <- read.csv(file=file.path(dir.ex.dat, "WISEsummaryFile.csv"))
		
			get_prids <- function(suid){
				soils <- dat_wise[dat_wise$SUID == suid, ]
				frac <- unique(soils[, c("PROP", "PRID")])
				depth <- aggregate(soils$BotDep, by=list(soils$PRID), FUN=max)
				return(list(PRIDs_N=nrow(soils)/layer_N, PRID=frac$PRID, fraction=frac$PROP/100, soildepth=ifelse((temp <- depth[match(frac$PRID, depth[, 1]), 2]) > 0, temp, NA), soildat=soils))
			}

			get_SoilDatValuesForLayer <- function(dat, soildat_rows, frac){
				return(sum(soildat_rows * frac, dat, na.rm=TRUE)) #weighted mean = sum of values x weights
			}

			template_simulationSoils <- rep(NA, times=2 + 4 * layer_Nsim)
			names(template_simulationSoils) <- c("i", "soildepth", paste0(rep(c("bulk", "sand", "clay", "cfrag"), times=layer_Nsim), "_L", rep(1:layer_Nsim, each=4)))
			template_simulationSoils["soildepth"] <- 0
		
			#cells with no soil values have SUID=c(0=Water, 6997=Water, 6694=Rock, or 6998=Glacier)
			calc_weightedMeanForSimulationCell <- function(i){
				.local <- function(i){
					#Init
					simulation_frac <- 0	#fraction of how much this simulation cell is covered with suids and prids that have a soildepth > 0 cm
					simulation_layer_frac <- rep(0, times=layer_Nsim) #fraction of each soil layer covering this simulation cell
					simulationSoils <- template_simulationSoils
					simulationSoils["i"] <- i
					PRIDs_N <- 0
					PRIDs <- PRIDs_frac <- NULL
					#Do calculations if any soils in this simulation cell
					if(sim_cells_SUIDs[i, ]$SUIDs_N > 0){
						this_simCell <- c(sim_cells_SUIDs[i, ], soils=list(t(sapply(sim_cells_SUIDs[i, ]$SUID, FUN=get_prids))))
						for(is in 1:this_simCell$SUIDs_N){	#loop through the suids within this simulation cell; each suid may be composed of several prids
							prids_frac <- this_simCell$soils[is,]$fraction * this_simCell$fraction[is]	#vector of the fractions of each prid in relation to the simulation cell
							PRIDs_frac <- c(PRIDs_frac, prids_frac)
							simulation_frac <- simulation_frac + sum(ifelse(!is.na(this_simCell$soils[is,]$soildepth), prids_frac, 0))
							simulationSoils["soildepth"] <- simulationSoils["soildepth"] + sum(this_simCell$soils[is,]$soildepth * prids_frac, na.rm=TRUE)
							if(!all(is.na(this_simCell$soils[is,]$soildepth))) for(ils in 1:layer_Nsim){
								lwise <- if(ils == 1) 1 else ils - 1	# I split wise soil layer 0-20 cm into two layers, 0-10 and 10-20 cm, to account for lithosols
								layer.there <- this_simCell$soils[is,]$soildepth > layer_TopDep[ils]	#checks if for each prid, there soils are deeper than this layer. It also accounts that soil depth for Rock outcrops (RK) is set to 0 instead of < 0 for such as water and glaciers. Lithosols (Ix) have soildepth of 10 cm.
								pfracl <- prids_frac[layer.there]
								simulation_layer_frac[ils] <- simulation_layer_frac[ils] + sum(pfracl, na.rm=TRUE)
								if(sum(layer.there, na.rm=TRUE) > 0){
									irow <- lwise + ((0:(this_simCell$soils[is,]$PRIDs_N-1))*layer_N)[layer.there]
									simulationSoils[paste0("bulk_L", ils)] <- get_SoilDatValuesForLayer(dat=simulationSoils[paste0("bulk_L", ils)], soildat_rows=this_simCell$soils[is,]$soildat[irow, "BULK"], frac=pfracl)	# bulk density (kg/dm3)
									simulationSoils[paste0("sand_L", ils)] <- get_SoilDatValuesForLayer(dat=simulationSoils[paste0("sand_L", ils)], soildat_rows=this_simCell$soils[is,]$soildat[irow, "SDTO"], frac=pfracl)	# Sand mass (%)
									simulationSoils[paste0("clay_L", ils)] <- get_SoilDatValuesForLayer(dat=simulationSoils[paste0("clay_L", ils)], soildat_rows=this_simCell$soils[is,]$soildat[irow, "CLPC"], frac=pfracl)	 # clay mass (%)
									simulationSoils[paste0("cfrag_L", ils)] <- get_SoilDatValuesForLayer(dat=simulationSoils[paste0("cfrag_L", ils)], soildat_rows=this_simCell$soils[is,]$soildat[irow, "CFRAG"], frac=pfracl)	# coarse fragments (vol % > 2 mm)
								}
							}
						}
					
						#Adjust values for area present
						simulationSoils <- simulationSoils / c(1, simulation_frac, rep(simulation_layer_frac, each=4))
					}
					return(simulationSoils)
				}
		
				if(i %% 1000 == 0) print(paste(Sys.time(), "done:", i))

				temp <- .local(i)
				return(if(!inherits(temp, "try-error")) temp else template_simulationSoils)
			}
	
			if(parallel_runs && parallel_init){
				#objects that need exporting to slaves
				list.export <- c("get_prids", "dat_wise", "layer_TopDep", "layer_N", "get_SoilDatValuesForLayer", "layer_Nsim", "calc_weightedMeanForSimulationCell", "template_simulationSoils", "sim_cells_SUIDs")
				#call the simulations depending on parallel backend
				if(identical(parallel_backend, "mpi")) {
					exportObjects(list.export)
			
					sim_cells_soils <- mpi.applyLB(x=is_ToDo, fun=calc_weightedMeanForSimulationCell)
					sim_cells_soils <- do.call(rbind, sim_cells_soils)
			
					mpi.bcast.cmd(rm(list=ls()))
					mpi.bcast.cmd(gc())
				} else if(identical(parallel_backend, "snow")) {
					snow::clusterExport(cl, list.export, envir=parent.frame())
			
					sim_cells_soils <- snow::clusterApplyLB(cl, x=is_ToDo, fun=calc_weightedMeanForSimulationCell)
					sim_cells_soils <- do.call(rbind, sim_cells_soils)
				
					snow::clusterEvalQ(cl, rm(list=ls()))
					snow::clusterEvalQ(cl, gc())
				} else if(identical(parallel_backend, "multicore")) {
					sim_cells_soils <- foreach(i=is_ToDo, .combine="rbind", .errorhandling="remove", .inorder=FALSE, .export=list.export) %dopar% calc_weightedMeanForSimulationCell(i)
				}
			} else {
				sim_cells_soils <- foreach(i=is_ToDo, .combine="rbind", .errorhandling="remove", .inorder=FALSE) %do% calc_weightedMeanForSimulationCell(i)
			}
			rm(dat_wise)

			sim_cells_soils <- round(sim_cells_soils[order(sim_cells_soils[, "i"]), ], 2)

			if(FALSE){#visualize in interactive sessions
				temp <- sim_cells_soils[, grep("sand", colnames(sim_cells_soils))]
				cats <- addNA(cut(temp[, 1], breaks=seq(0, to=max(1, max(temp, na.rm=TRUE)), length.out=11)))
				cols <- c(head(rainbow(n=nlevels(cats)), n=-1), "gray")
				plot(locations, pch=15, cex=0.5, col=cols[cats])
				legend(x="bottomleft", legend=sQuote(levels(cats)), pch=19, col=cols)
				if(require("maps")) map("state", add=TRUE)
			}

			#Convert bulk density to matric density
			#	eqn. 20 from Saxton et al. 2006: bulkd <- matricd * (1 - rockvol) + rockvol * 2.65
	#TODO: why so many negative values?
			#matricd <- (sim_cells_soils[, grep("bulk", colnames(sim_cells_soils))] - 2.65 * sim_cells_soils[, grep("cfrag", colnames(sim_cells_soils))]) / (1 - sim_cells_soils[, grep("cfrag", colnames(sim_cells_soils))])
	
			i_good <- complete.cases(sim_cells_soils) #length(i_good) == sum(do_extract)
		
			if(sum(i_good) > 0){
				i_Done <- rep(FALSE, times=length(seq.tr)) #length(i_Done) == length(seq.tr)
				i_Done[which(do_extract)[i_good]] <- TRUE #sum(i_Done) == sum(i_good)

				if(extract_determine_database == "order"){
					sites_externalsoils_source[i_Done] <- "ISRICWISEv12_Global"
				} else if(extract_determine_database == "SWRunInformation"){
					sites_externalsoils_source[which(do_extract)[!i_good]] <- NA
				}
		
				#set and save soil layer structure
				lys <- 1:layer_Nsim
				sw_input_soillayers[seq.tr[i_Done], "SoilDepth_cm"] <- sim_cells_soils[i_good, "soildepth"]
				sw_input_soillayers[seq.tr[i_Done], 2+lys] <- matrix(data=rep(layer_BotDep[lys], times=sum(i_good)), ncol=length(lys), byrow=TRUE)
				sw_input_soillayers[seq.tr[i_Done], 2+(1:20)[-lys]] <- NA
				write.csv(sw_input_soillayers, file=file.path(dir.in, datafile.soillayers), row.names=FALSE)

				#set and save soil texture
				#add data to sw_input_soils and set the use flags
				i.temp <- grep(pattern="Matricd_L", x=names(sw_input_soils_use))
				sw_input_soils[seq.tr[i_Done], i.temp[lys]] <- sim_cells_soils[i_good, paste0("bulk_L", lys)]
				sw_input_soils_use[i.temp][lys] <- 1
				i.temp <- grep(pattern="GravelContent_L", x=names(sw_input_soils_use))
				sw_input_soils[seq.tr[i_Done], i.temp[lys]] <- sim_cells_soils[i_good, paste0("cfrag_L", lys)] / 100
				sw_input_soils_use[i.temp][lys] <- 1
				i.temp <- grep(pattern="Sand_L", x=names(sw_input_soils_use))
				sw_input_soils[seq.tr[i_Done], i.temp[lys]] <- sim_cells_soils[i_good, paste0("sand_L", lys)] / 100
				sw_input_soils_use[i.temp][lys] <- 1
				i.temp <- grep(pattern="Clay_L", x=names(sw_input_soils_use))
				sw_input_soils[seq.tr[i_Done], i.temp[lys]] <- sim_cells_soils[i_good, paste0("clay_L", lys)] / 100
				sw_input_soils_use[i.temp][lys] <- 1

				#write data to datafile.soils
				tempdat <- rbind(sw_input_soils_use, sw_input_soils)
				write.csv(tempdat, file=file.path(dir.sw.dat, datafile.soils), row.names=FALSE)
			}
		
			rm(sim_cells_soils, tempdat, i.temp, lys, locations)
		}
		
		if(!be.quiet) print(paste("Finished 'ExtractSoilDataFromISRICWISEv12_Global' at", Sys.time()))
	}

	
	#write data to datafile.SWRunInformation
	SWRunInformation$SoilTexture_source[seq.tr] <- as.character(sites_externalsoils_source)
	notDone <- is.na(sites_externalsoils_source)
	include_YN[seq.tr[notDone]] <- 0
	SWRunInformation$Include_YN[seq.tr[notDone]] <- 0
	write.csv(SWRunInformation, file=file.path(dir.in, datafile.SWRunInformation), row.names=FALSE)

	if(sum(notDone) > 0) warning(paste("'ExtractSoilDataFromCONUSSOILFromSTATSGO_USA': no soil information for one or several sites (e.g., sand or clay is 0): this will likely lead to crashes of SoilWat"))
}

#------END OF SOIL CHARACTERISTICS------
#--------------------------------------------------------------------------------------------------#




#--------------------------------------------------------------------------------------------------#
#------EXTRACT ELEVATION------
if(exinfo$ExtractElevation_NED_USA || exinfo$ExtractElevation_HWSD_Global){
	#allow for multiple sources
	if(extract_determine_database == "order"){
		sites_elevation_source <- rep(NA, times=length(seq.tr))
	} else if(extract_determine_database == "SWRunInformation"){
		sites_elevation_source <- SWRunInformation$Elevation_source[seq.tr]
	} else {
		stop(paste("Value of 'extract_determine_database'", extract_determine_database, "not implemented"))
	}
	elevation_m <- rep(NA, times=length(seq.tr))
	
	#	- extract NED data where available
	if(exinfo$ExtractElevation_NED_USA){
		if(!be.quiet) print(paste("Started 'ExtractElevation_NED_USA' at", Sys.time()))
		#ned.usgs.gov
	
		do_extract <- is.na(elevation_m) | is.na(sites_elevation_source) | (sites_elevation_source == "Elevation_NED_USA")
		if(sum(do_extract) > 0){
			dir.ex.dat <- file.path(dir.ex.dem, 'NED_USA', "NED_1arcsec")
	
			#read raster data
			g.elev <- raster(file.path(dir.ex.dat, "ned_1s_westernUS_GeogrNAD83.tif"))
	
			#locations of simulation runs
			locations <- SpatialPoints(coords=with(SWRunInformation[seq.tr[do_extract],], data.frame(X_WGS84, Y_WGS84)), proj4string=CRS("+proj=longlat +datum=WGS84"))
			locations.CoordG <- spTransform(locations, CRS=CRS(proj4string(g.elev)))	#transform points to grid-coords
	
			#extract data for locations
			elevation_m[do_extract] <- round(extract(g.elev, locations.CoordG))	# elevation in m a.s.l.

			i_good <- !is.na(elevation_m[do_extract]) #length(i_good) == sum(do_extract)
			if(sum(i_good) > 0){
				i_Done <- rep(FALSE, times=length(seq.tr)) #length(i_Done) == length(seq.tr)
				i_Done[which(do_extract)[i_good]] <- TRUE #sum(i_Done) == sum(i_good)

				if(extract_determine_database == "order"){
					sites_elevation_source[i_Done] <- "Elevation_NED_USA"
				} else if(extract_determine_database == "SWRunInformation"){
					sites_elevation_source[which(do_extract)[!i_good]] <- NA
				}
			}
	
			rm(g.elev, locations, locations.CoordG)
		}
		
		if(!be.quiet) print(paste("Finished 'ExtractElevation_NED_USA' at", Sys.time()))
	}

	#	- extract HWSD elevation data for sites with no elevation data
	if(exinfo$ExtractElevation_HWSD_Global){
		if(!be.quiet) print(paste("Started 'ExtractElevation_HWSD_Global' at", Sys.time()))
	
		do_extract <- is.na(elevation_m) | is.na(sites_elevation_source) | (sites_elevation_source == "Elevation_HWSD_Global")
		if(sum(do_extract) > 0){
			dir.ex.dat <- file.path(dir.ex.dem, "HWSD")
	
			#read raster data
			g.elev <- raster(file.path(dir.ex.dat, "GloElev_30as.asc"))
	
			#locations of simulation runs
			locations <- SpatialPoints(coords=with(SWRunInformation[seq.tr[do_extract],], data.frame(X_WGS84, Y_WGS84)), proj4string=CRS("+proj=longlat +datum=WGS84"))
			locations.CoordG <- spTransform(locations, CRS=CRS(proj4string(g.elev)))	#transform points to grid-coords
	
			#extract data for locations
			elevation_m[do_extract] <- round(extract(g.elev, locations.CoordG))	# elevation in m a.s.l.

			i_good <- !is.na(elevation_m[do_extract]) #length(i_good) == sum(do_extract)
			if(sum(i_good) > 0){
				i_Done <- rep(FALSE, times=length(seq.tr)) #length(i_Done) == length(seq.tr)
				i_Done[which(do_extract)[i_good]] <- TRUE #sum(i_Done) == sum(i_good)

				if(extract_determine_database == "order"){
					sites_elevation_source[i_Done] <- "Elevation_HWSD_Global"
				} else if(extract_determine_database == "SWRunInformation"){
					sites_elevation_source[which(do_extract)[!i_good]] <- NA
				}
			}
			rm(g.elev, locations, locations.CoordG)
		}
		
		if(!be.quiet) print(paste("Finished 'ExtractElevation_HWSD_Global' at", Sys.time()))
	}


	#write data to datafile.SWRunInformation
	SWRunInformation$ELEV_m[seq.tr] <- elevation_m
	SWRunInformation$Elevation_source[seq.tr] <- as.character(sites_elevation_source)
	write.csv(SWRunInformation, file=file.path(dir.in, datafile.SWRunInformation), row.names=FALSE)
	
	if(anyNA(elevation_m)) warning("Elevation wasn't found for ", sum(is.na(elevation_m)), " sites")
	
	rm(elevation_m, sites_elevation_source)
}

#------END OF ELEVATION------
#--------------------------------------------------------------------------------------------------#


if(exinfo$ExtractSkyDataFromNOAAClimateAtlas_USA || exinfo$ExtractSkyDataFromNCEPCFSR_Global){
	#allow for multiple sources
	if(extract_determine_database == "order"){
		sites_monthlyclim_source <- rep(NA, times=length(seq.tr))
	} else if(extract_determine_database == "SWRunInformation"){
		sites_monthlyclim_source <- SWRunInformation$ClimateNormals_source[seq.tr]
	} else {
		stop(paste("Value of 'extract_determine_database'", extract_determine_database, "not implemented"))
	}

	monthlyclim <- array(NA, dim=c(length(seq.tr), 3, 12), dimnames=list(NULL, c("rh", "cover", "wind"), NULL))
	get_NA_byrow <- function(dat) apply(dat, 1, anyNA)
	
	if(exinfo$ExtractSkyDataFromNOAAClimateAtlas_USA){
		if(!be.quiet) print(paste("Started 'ExtractSkyDataFromNOAAClimateAtlas_USA' at", Sys.time()))
	
		do_extract <- get_NA_byrow(monthlyclim) | is.na(sites_monthlyclim_source) | (sites_monthlyclim_source == "ClimateNormals_NCDC2005_USA")
		if(sum(do_extract) > 0){
			reference <- "National Climatic Data Center. 2005. Climate maps of the United States. Available online http://cdo.ncdc.noaa.gov/cgi-bin/climaps/climaps.pl. Last accessed May 2010."
	
			#NOAA Climate Atlas: provides no information on height above ground: assuming 2-m which is what is required by SoilWat
			dir.ex.dat <- file.path(dir.ex.weather, "ClimateAtlasUS")
			stopifnot(file.exists(dir.ex.dat), require(raster), require(sp), require(rgdal))

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
			locations <- SpatialPoints(coords=with(SWRunInformation[seq.tr[do_extract],], data.frame(X_WGS84, Y_WGS84)), proj4string=CRS("+proj=longlat +datum=WGS84"))
			projStringWGS84 <- proj4string(locations)
	
			#extract data for locations
			get.month <- function(path, shp, month){
				s <- readOGR(dsn=path, layer=shp[month], verbose=FALSE)
				s.wgs84 <- spTransform(s, CRS=CRS(projStringWGS84))	#transform to wgs84
				val <- sp::over(x=locations, y=s.wgs84)$GRIDCODE
			}
			monthlyclim[do_extract, "rh", ] <- sapply(st_mo, FUN=function(m) code.RH[get.month(path=dir.ex.dat.RH, shp=datafile.RH, month=m)])
			monthlyclim[do_extract, "cover", ] <- sapply(st_mo, FUN=function(m) 100 - code.cover[get.month(path=dir.ex.dat.cover, shp=datafile.cover, month=m)]) #subtract from 100% as we want cover not no-cover
		#		cover <- sapply(st_mo, FUN=function(m) code.cover[get.month(path=dir.ex.dat.cover, shp=datafile.cover, month=m)])
			monthlyclim[do_extract, "wind", ] <- sapply(st_mo, FUN=function(m) code.wind[get.month(path=dir.ex.dat.wind, shp=datafile.wind, month=m)])
	
			# Save extracted data to disk
			i_good <- do_extract & !get_NA_byrow(monthlyclim) #length(i_good) == sum(do_extract)
			if(sum(i_good) > 0){
				i_Done <- rep(FALSE, times=length(seq.tr)) #length(i_Done) == length(seq.tr)
				i_Done[which(do_extract)[i_good]] <- TRUE #sum(i_Done) == sum(i_good)

				if(extract_determine_database == "order"){
					sites_monthlyclim_source[i_Done] <- "ClimateNormals_NCDC2005_USA"
				} else if(extract_determine_database == "SWRunInformation"){
					sites_monthlyclim_source[which(do_extract)[!i_good]] <- NA
				}
	
				#add data to sw_input_cloud and set the use flags
				sw_input_cloud_use[i.temp <- grepl(pattern="RH", x=names(sw_input_cloud_use))] <- 1
				sw_input_cloud[seq.tr[i_Done], i.temp][, st_mo] <- round(monthlyclim[i_good, "rh", ], 2)
				sw_input_cloud_use[i.temp <- grepl(pattern="SkyC", x=names(sw_input_cloud_use))] <- 1
				sw_input_cloud[seq.tr[i_Done], i.temp][, st_mo] <- round(monthlyclim[i_good, "cover", ], 2)
				sw_input_cloud_use[i.temp <- grepl(pattern="wind", x=names(sw_input_cloud_use))] <- 1
				sw_input_cloud[seq.tr[i_Done], i.temp][, st_mo] <- round(monthlyclim[i_good, "wind", ], 2)
	
				#write data to datafile.cloud
				write.csv(rbind(sw_input_cloud_use, sw_input_cloud), file=file.path(dir.sw.dat, datafile.cloud), row.names=FALSE)
			
				rm(i.temp)
			}
			rm(locations, reference)
		}
	
		if(!be.quiet) print(paste("Finished 'ExtractSkyDataFromNOAAClimateAtlas_USA' at", Sys.time()))
	}
	
	if(exinfo$ExtractSkyDataFromNCEPCFSR_Global){
		#Citations: Environmental Modeling Center/National Centers for Environmental Prediction/National Weather Service/NOAA/U.S. Department of Commerce. 2010. NCEP Climate Forecast System Reanalysis (CFSR) Monthly Products, January 1979 to December 2010. Research Data Archive at the National Center for Atmospheric Research, Computational and Information Systems Laboratory.
		# http://rda.ucar.edu/datasets/ds093.2/. Accessed 8 March 2012.
		
		if(!be.quiet) writeLines(c(paste("Started 'ExtractSkyDataFromNCEPCFSR_Global' at", Sys.time()), "NOTE: Do not interrupt - this process cannot resume."))

		do_extract <- get_NA_byrow(monthlyclim) | is.na(sites_monthlyclim_source) | (sites_monthlyclim_source == "ClimateNormals_NCEPCFSR_Global")
		if(sum(do_extract) > 0){		
			# preparations
			dir.ex.dat <- file.path(dir.ex.weather, "NCEPCFSR", "CFSR_weather_prog08032012")
			stopifnot(file.exists(dir.ex.dat))
		
			prepd_CFSR <- prepare_NCEPCFSR_extraction(dir.cfsr=dir.ex.dat)
			stopifnot(!inherits(prepd_CFSR, "try-error"))

			#locations of simulation runs
			locations <- SWRunInformation[seq.tr[do_extract], c("WeatherFolder", "X_WGS84", "Y_WGS84")]
			# do the extractions
			temp <- try(get_NCEPCFSR_data(dat_sites=locations, daily=FALSE, monthly=TRUE, yearLow=startyr, yearHigh=endyr, n_site_per_core=100, cfsr_so=prepd_CFSR$cfsr_so, dir.in.cfsr=prepd_CFSR$dir.in.cfsr, dir.temp=dir.out.temp, rm_mc_files=TRUE), silent=TRUE)
			stopifnot(!inherits(temp, "try-error"))

			#match weather folder names in case of missing extractions
			res <- as.matrix(temp[["res_clim"]][, -1])
			irow <- match(temp[["res_clim"]][, "WeatherFolder"], table=locations[, "WeatherFolder"], nomatch=0)
			irowL <- (irow > 0)
			monthlyclim[do_extract, "rh", ][irowL, ] <- res[irow, grepl("RH", colnames(res))]
			monthlyclim[do_extract, "cover", ][irowL, ] <- res[irow, grepl("Cloud", colnames(res))]
			monthlyclim[do_extract, "wind", ][irowL, ] <- res[irow, grepl("Wind", colnames(res))]
		
			# Save extracted data to disk
			i_good <- do_extract & !get_NA_byrow(monthlyclim) #length(i_good) == sum(do_extract)
			if(sum(i_good) > 0){
				i_Done <- rep(FALSE, times=length(seq.tr)) #length(i_Done) == length(seq.tr)
				i_Done[which(do_extract)[i_good]] <- TRUE #sum(i_Done) == sum(i_good)

				if(extract_determine_database == "order"){
					sites_monthlyclim_source[i_Done] <- "ClimateNormals_NCEPCFSR_Global"
				} else if(extract_determine_database == "SWRunInformation"){
					sites_monthlyclim_source[which(do_extract)[!i_good]] <- NA
				}

				#add data to sw_input_cloud and set the use flags
				sw_input_cloud_use[i.temp <- grepl(pattern="RH", x=names(sw_input_cloud_use))] <- 1
				sw_input_cloud[seq.tr[i_Done], i.temp][, st_mo] <- round(monthlyclim[i_good, "rh", ], 2)
				sw_input_cloud_use[i.temp <- grepl(pattern="SkyC", x=names(sw_input_cloud_use))] <- 1
				sw_input_cloud[seq.tr[i_Done], i.temp][, st_mo] <- round(monthlyclim[i_good, "cover", ], 2)
				sw_input_cloud_use[i.temp <- grepl(pattern="wind", x=names(sw_input_cloud_use))] <- 1
				sw_input_cloud[seq.tr[i_Done], i.temp][, st_mo] <- round(monthlyclim[i_good, "wind", ], 2)

				#write data to datafile.cloud
				write.csv(rbind(sw_input_cloud_use, sw_input_cloud), file=file.path(dir.sw.dat, datafile.cloud), row.names=FALSE)
		
				rm(i_Done, i.temp)
			}
			rm(locations, temp)
		}
		rm(do_extract)
	
		if(!be.quiet) print(paste("Finished 'ExtractSkyDataFromNCEPCFSR_Global' at", Sys.time()))
	}

	#write data to datafile.SWRunInformation
	SWRunInformation$ClimateNormals_source[seq.tr] <- as.character(sites_monthlyclim_source)
	write.csv(SWRunInformation, file=file.path(dir.in, datafile.SWRunInformation), row.names=FALSE)
	
	if(anyNA(sites_monthlyclim_source)) warning("Climate normals weren't found for ", sum(is.na(sites_monthlyclim_source)), " sites")

	rm(monthlyclim, sites_monthlyclim_source)
}

#--------------------------------------------------------------------------------------------------#
