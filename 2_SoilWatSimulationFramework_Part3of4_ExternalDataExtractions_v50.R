#--------------------------------------------------------------------------------------------------#
#------------------------OBTAIN INFORMATION FROM EXTERNAL DATASETS PRIOR TO SIMULATION RUNS TO CREATE THEM

#------Load required packages
if(!require(sp, quietly=TRUE)) {
	tryCatch(install.packages("sp", repos=url.Rrepos, lib=dir.libraries), warning=function(w) { print(w); print("sp failed to install"); stop("Stopping") })
	stopifnot(require(sp, quietly=TRUE))
}
if(!require(rgdal, quietly=TRUE)) {
	tryCatch(install.packages("rgdal", repos=url.Rrepos, lib=dir.libraries), warning=function(w) { print(w); print("rgdal failed to install"); stop("Stopping") })
	stopifnot(require(rgdal, quietly=TRUE))
}
if(!require(ncdf4, quietly=TRUE)) {
	tryCatch(install.packages("ncdf4", repos=url.Rrepos, lib=dir.libraries), warning=function(w) { print(w); print("ncdf4 failed to install"); stop("Stopping") })
	stopifnot(require(ncdf4, quietly=TRUE))
}
if(parallel_runs && identical(parallel_backend, "multicore")) {
	if(!require(doMC,quietly = TRUE)) {	#requires: foreach, iterators, codetools, and attaches: multicore
		tryCatch(install.packages("doMC",repos=url.Rrepos,lib=dir.libraries), warning=function(w) { print(w); print("doMC failed to install"); stop("Stopping") })
		stopifnot(require(doMC, quietly = TRUE))
	}
}	

if(exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_NEX_USA){
}

if(	exinfo$ExtractClimateChangeScenarios_CMIP3_BCSD_GDODCPUCLLNL_USA ||
	exinfo$ExtractClimateChangeScenarios_CMIP3_BCSD_GDODCPUCLLNL_Global ||
	exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_GDODCPUCLLNL_USA ||
	exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_GDODCPUCLLNL_Global){
}

if(exinfo$ExtractClimateChangeScenariosMaurer2009_Global){
	#Maurer EP, Adam JC, Wood AW (2009) Climate model based consensus on the hydrologic impacts of climate change to the Rio Lempa basin of Central America. Hydrology and Earth System Sciences, 13, 183-194.
	#accessed via climatewizard.org on July 10, 2012
	if(!be.quiet) print(paste("Started 'ExtractClimateChangeScenariosMaurer2009_Global' at", Sys.time()))
	
	list.scenarios.datafile <- climate.conditions[!grepl(climate.ambient, climate.conditions)]
	if(length(list.scenarios.datafile) > 0){ #extracts only information requested in the 'datafile.SWRunInformation'
		dir.ex.dat <- file.path(dir.external, "ExtractClimateChangeScenarios", "ClimateWizard_CMIP3", "Global")
		
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

if(exinfo$ExtractClimateChangeScenarios_NorthAmerica){
	#Maurer, E. P., L. Brekke, T. Pruitt, and P. B. Duffy. 2007. Fine-resolution climate projections enhance regional climate change impact studies. Eos Transactions AGU 88:504.
	#accessed via climatewizard.org
	if(!be.quiet) print(paste("Started 'ExtractClimateChangeScenarios_NorthAmerica' at", Sys.time()))
	
	list.scenarios.datafile <- climate.conditions[!grepl(climate.ambient, climate.conditions)]
	if(length(list.scenarios.datafile) > 0){ #extracts only information requested in the 'datafile.SWRunInformation'
		dir.ex.dat <- file.path(dir.external, "ExtractClimateChangeScenarios", "ClimateWizard_CMIP3", "USA")
		
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

if(exinfo$ExtractSoilDataFromCONUSSOILFromSTATSGO_NorthAmerica){
	if(!be.quiet) print(paste("Started 'ExtractSoilDataFromCONUSSOILFromSTATSGO_NorthAmerica' at", Sys.time()))
	#Miller, D. A. and R. A. White. 1998. A conterminous United States multilayer soil characteristics dataset for regional climate and hydrology modeling. Earth Interactions 2:1-26.
	#CONUS-SOIL: rasterized and controlled STATSGO data; information for 11 soil layers available
	cl <- 1:11
	ldepth <- c(5, 10, 20, 30, 40, 60, 80, 100, 150, 200, 250)	#in cm
	
	dir.ex.dat <- file.path(dir.external, "ExtractSoilDataFromCONUSSOILFromSTATSGO", "CONUSSoil")
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
	
	if(any(sand == 0, clay == 0)) print(paste("'ExtractSoilDataFromCONUSSOILFromSTATSGO_NorthAmerica': no soil information for one or several sites (e.g., sand or clay is 0): this will likely lead to crashes of SoilWat"))
	
	rm(tempdat, i.temp, cl, bedrock, bulkd, sand, clay, val, rat, g, locations)
	
	if(!be.quiet) print(paste("Finished 'ExtractSoilDataFromCONUSSOILFromSTATSGO_NorthAmerica' at", Sys.time()))
}

if(exinfo$ExtractTopographyANDElevation_USA){
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

#--------------------------------------------------------------------------------------------------#
