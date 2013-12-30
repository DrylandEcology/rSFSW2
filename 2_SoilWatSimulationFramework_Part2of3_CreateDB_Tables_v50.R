# TODO: Add comment
# 
# Author: Ryan Murphy
#
#
# This will generate all the SQL table definitions.
# 
#
###############################################################################
library(RSQLite)
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname = name.OutputDB)

Tables <- dbListTables(con)
if(length(Tables) == 0) {
	rs<-dbGetQuery(con,"PRAGMA page_size=65536;") #no return value (http://www.sqlite.org/pragma.html)
	rs<-dbGetQuery(con,"PRAGMA max_page_count=2147483646;") #returns the maximum page count
	rs<-dbGetQuery(con,"PRAGMA temp_store=2;") #no return value
	rs<-dbGetQuery(con,"PRAGMA foreign_keys = ON;") #no return value
	
	rm(rs)
}

#Only do this if the database is empty
#number of tables without ensembles (daily_no*2 + 2)
if((length(Tables) == 0) || (cleanDB && !(length(actions) == 1 && actions == "ensemble"))) {
	#0. Aggregation_Overall
	if((i1 <- length(Index_RunInformation)) + (i2 <- length(Index_RunInformation_Treatments)) > 0 ) {
		treatment_header1 <- treatment_header2 <- NULL
		if(i1 > 0){
			treatment_header1 <- SWRunInformation[1,Index_RunInformation]
		}
		if(i2 > 0){
			if(trowExperimentals > 0 && length(create_experimentals) > 0) treatment_header2 <- c(treatment_header2,  sw_input_experimentals[1, 1])
			for(h in Index_RunInformation_Treatments){
				treatment_header2 <- c(treatment_header2, as.character(sw_input_treatments[1,h]))
			}
			if(trowExperimentals > 0 && length(create_experimentals) > 0) names(treatment_header2) <- c("Experimental_Label", colnames(sw_input_treatments)[Index_RunInformation_Treatments])
			else names(treatment_header2) <- colnames(sw_input_treatments)[Index_RunInformation_Treatments]
			if(any(create_treatments == "YearStart")) if(is.na(treatment_header2["YearStart"])) treatment_header2["YearStart"] <- startyr
			if(any(create_treatments == "YearEnd")) if(is.na(treatment_header2["YearEnd"])) treatment_header2["YearEnd"] <- endyr
			
		}
		treatment_header <- c(treatment_header1, treatment_header2)
		#check to see if the header.names includes YearStart, SimStartYear ,YearEnd
		if(any(names(treatment_header) == "YearStart") && any(names(treatment_header) == "YearEnd")) {#find index of YearStart and Add Sim
			index.startYear <- match("YearStart",names(treatment_header))
			treatment_header <- c(treatment_header[1:index.startYear], "SimStartYear"=simstartyr, treatment_header[(index.startYear+1):length(treatment_header)])
		} else if(any(names(treatment_header) == "YearStart") && !any(names(treatment_header) == "YearEnd")){
			index.startYear <- match("YearStart",names(treatment_header))
			treatment_header <- c(treatment_header[1:index.startYear], "SimStartYear"=simstartyr, "YearEnd"=endyr, treatment_header[(index.startYear+1):length(treatment_header)])
		} else if(!any(names(treatment_header) == "YearStart") && any(names(treatment_header) == "YearEnd")){
			index.YearEnd <- match("YearEnd",names(treatment_header))
			treatment_header <- c(treatment_header[1:index.YearEnd-1], "YearStart"=startyr, "SimStartYear"=simstartyr, treatment_header[index.YearEnd:length(treatment_header)])
		} else {
			treatment_header <- c(treatment_header, "YearStart"=startyr, "SimStartYear"=simstartyr, "YearEnd"=endyr)
		}
		treatment_header.names <- names(treatment_header)
		header <- c(1, as.character("temp"), treatment_header, scenario[1])
		header.names <-  c("RunID", "Labels", treatment_header.names, "Scenario")	#'Scenario' column has to be the last in the header
	} else {
		if(trowExperimentals > 0 && length(create_experimentals) > 0) {
			header <- c(1, as.character("temp"),sw_input_experimentals[1,1] ,startyr, simstartyr, endyr, scenario[1])
			header.names <- c("RunID", "Labels", "Experimental Design Label", "YearStart", "SimStartYear", "YearEnd","Scenario")	#'Scenario' column has to be the last in the header
		} else {
			header <- c(1, as.character("temp") ,startyr, simstartyr, endyr, scenario[1])
			header.names <- c("RunID", "Labels", "YearStart", "SimStartYear", "YearEnd","Scenario")	#'Scenario' column has to be the last in the header
		}
	}
	header[[1]] <- as.integer(header[[1]])
	names(header) <- header.names
	mapType <- function(type) {
		if(type =="double")
			return("REAL")
		else if(type=="character")
			return("TEXT")
		else if(type=="logical")
			return("INTEGER")
		else if(type=="integer")
			return("INTEGER")
	}
	temp <- "\"P_id\" INTEGER PRIMARY KEY"
	for(i in 1:length(header)) {
		temp <- c(temp, paste(c("\"", header.names[i], "\"", " ", mapType(typeof(header[[i]]))), sep=",", collapse = ""))
	}
	header_vector <- temp
#SQL_Table_Definitions <- c(SQL_Table_Definitions, Aggregation_Overall=paste("CREATE TABLE \"Aggregation_Overall\" (\"P_Id\" integer PRIMARY KEY,", temp, ");", sep=""))
	
	##############################################################---Aggregation: SoilWat inputs---##############################################################
	
#1. 
	if(aon$input_FractionVegetationComposition) {
		temp <- paste("SWinput.Composition.", c("Grasses", "Shrubs", "Trees", "C3ofGrasses", "C4ofGrasses", "AnnualsofGrasses"), "_fraction_const", sep="")
	}
#2.
	if(aon$input_VegetationBiomassMonthly) {
		temp <- c(temp, paste(c(rep("Grass",36),rep("Shrub",36),rep("Tree",36)),"_",c(rep("Litter",12),rep("TotalBiomass",12),rep("LiveBiomass",12)),"_m", st_mo,"_gPERm2",sep=""))
	}
#3. 
	if(aon$input_VegetationPeak) {
		temp <- c(temp, paste("SWinput.PeakLiveBiomass_", c("month_mean","months_duration"), sep=""))
	}
	
#4.
	if(any(simulation_timescales=="monthly") && aon$input_Phenology) {
		temp <- c(temp, paste("SWinput.GrowingSeason.", c("Start", "End"), "_month_const", sep=""))
	}
	
	if(aon$input_TranspirationCoeff){
		if(AggLayer.daily){
			ltemp <- paste("L0to", Depth_FirstAggLayer.daily, "cm", sep="")
			if(is.null(Depth_SecondAggLayer.daily)) {
				ltemp <- c(ltemp, paste("L", Depth_FirstAggLayer.daily, "toSoilDepth", sep=""))
			} else if(is.numeric(Depth_SecondAggLayer.daily)){
				ltemp <- c(ltemp, paste("L", Depth_FirstAggLayer.daily, "to", Depth_SecondAggLayer.daily, "cm", sep=""))
			}
			if(is.null(Depth_ThirdAggLayer.daily)) {
				ltemp <- c(ltemp, paste("L", Depth_SecondAggLayer.daily, "toSoilDepth", sep=""))
			} else if(is.na(Depth_ThirdAggLayer.daily)){
			} else if(is.numeric(Depth_ThirdAggLayer.daily)){
				ltemp <- c(ltemp, paste("L", Depth_SecondAggLayer.daily, "to", Depth_ThirdAggLayer.daily, "cm", sep=""))
			}
			if(is.null(Depth_FourthAggLayer.daily)) {
				ltemp <- c(ltemp, paste("L", Depth_ThirdAggLayer.daily, "toSoilDepth", sep=""))
			} else if(is.na(Depth_FourthAggLayer.daily)){
			} else if(is.numeric(Depth_FourthAggLayer.daily)){
				ltemp <- c(ltemp, paste("L", Depth_ThirdAggLayer.daily, "to", Depth_FourthAggLayer.daily, "cm", sep=""))
			}
			ltemp <- c(ltemp, rep("NA", times=SoilLayer_MaxNo-length(ltemp)))
		} else {
			ltemp <- paste("L", formatC(lmax, width=2, format="d", flag="0"), sep="")
		}
	
		temp <- c(temp, c(paste("SWinput.", rep(vtemp <- c("Grass", "Shrub", "Tree"), each=SoilLayer_MaxNo), ".TranspirationCoefficients.", rep(ltemp, times=3), "_fraction", sep=""), paste("SWinput.", rep(vtemp, each=2), ".TranspirationCoefficients.", rep(c("topLayer", "bottomLayer"), times=3), "_fraction", sep="")))

	}
	
#5.
	if(aon$input_ClimatePerturbations) {
		temp <- c(temp, paste(rep(paste("SWinput.ClimatePerturbations.", c("PrcpMultiplier.m", "TmaxAddand.m", "TminAddand.m"), sep=""), each=12), st_mo, rep(c("_none", "_C", "_C"), each=12), "_const", sep=""))
	}
	
	##############################################################---Aggregation: Climate and weather---##############################################################
	
#6.
	if(any(simulation_timescales=="yearly") & aon$yearlyTemp){
		temp <- c(temp, "MAT_C_mean")
	}
	
#7.
	if(any(simulation_timescales=="yearly") & aon$yearlyPPT){
		temp <- c(temp, c("MAP_mm_mean", "SnowOfPPT_fraction_mean"))
	}
	
#8.
	if(any(simulation_timescales=="daily") & any(simulation_timescales=="yearly") & aon$dailySnowpack){
		temp <- c(temp, "RainOnSnowOfMAP_fraction_mean")
	}
	
#9.
	if(any(simulation_timescales=="daily") & aon$dailySnowpack){
		temp <- c(temp, paste("Snowcover.NSadj.", c("Peak_doy", "LongestContinuous.LastDay_doy", "Peak_mmSWE", "LongestContinuous.Duration_days", "Total_days"), "_mean", sep=""))
	}
#
	if(any(simulation_timescales=="daily") & aon$dailyFrostInSnowfreePeriod){			
		temp <- c(temp, "FreezingWithoutSnowpack_days_mean")
	}
#10.
	if(any(simulation_timescales=="daily") & aon$dailyPrecipitationEventSizeDistribution){
		bins.summary <- (0:6) * bin.prcpSizes
		temp <- c(temp, paste("PrcpEvents.Annual", c("_count", paste(".SizeClass", bins.summary, "to", c(bins.summary[-1], "Inf"), "mm_fraction", sep="")), "_mean", sep=""))
		rm(bins.summary)
	}
	
#11.
	if(any(simulation_timescales=="yearly") & aon$yearlyAET){
		temp <- c(temp, "AET_mm_mean")
	}
	
#12.
	if(any(simulation_timescales=="yearly") & aon$yearlyPET){
		temp <- c(temp, "PET_mm_mean")
	}
	
#13.
	if(any(simulation_timescales=="monthly") & aon$monthlySeasonalityIndices){
		temp <- c(temp, paste("Seasonality.monthly", c("PETandSWPtopLayers", "PETandSWPbottomLayers", "TandPPT"), "_PearsonCor_mean", sep=""))
	}
	
#14.
	if(any(simulation_timescales=="yearly") & any(simulation_timescales=="monthly") & aon$yearlymonthlyTemperateDrylandIndices){
		temp <- c(temp, paste(c(paste(temp <- c("UNAridityIndex", "TrewarthaD", "TemperateDryland12"), ".Normals", sep=""), paste(temp, ".Annual", sep="")), rep(c("_none", "_TF", "_TF"), times=2), "_mean", sep=""))
	}
	
#15.
	if(any(simulation_timescales=="yearly") & aon$yearlyDryWetPeriods){
		temp <- c(temp, paste(c("Dry", "Wet"), "SpellDuration.90PercentEvents.ShorterThan_years_quantile0.9", sep=""))
	}
	
#16.
	if(any(simulation_timescales=="daily") & aon$dailyWeatherGeneratorCharacteristics){
		temp <- c(temp, paste(rep(c("WetSpellDuration", "DrySpellDuration", "TempAir.StDevOfDailyValues"), each=12), ".m", st_mo, rep(c("_days", "_days", "_C"), each=12), "_mean", sep=""))
	}
	
#17.
	if(any(simulation_timescales=="daily") & aon$dailyPrecipitationFreeEventDistribution){
		bins.summary <- (0:3) * bin.prcpfreeDurations
		temp <- c(temp, paste("DrySpells.Annual", c("_count", paste(".SizeClass", bins.summary+1, "to", c(bins.summary[-1], "365"), "days_fraction", sep="")), "_mean", sep=""))
		rm(bins.summary)
	}
	
#18.
	if(any(simulation_timescales=="monthly") & aon$monthlySPEIEvents){
		binSPEI_m <- c(1, 12, 24, 48) #months
		probs <- c(0.025, 0.5, 0.975)
		for(iscale in seq_along(binSPEI_m)) {
			rvec <- rep(NA, times=4 * length(probs))
			temp <- c(temp, paste(rep(paste("SPEI.", binSPEI_m[iscale], "monthsScale.", sep=""), length(rvec)), "Spell", rep(c("Pos.", "Neg."), each=2*length(probs)), rep(rep(c("Duration_months", "Value_none"), each=length(probs)), times=2), "_quantile", rep(probs, times=4), sep=""))
			
		}
		rm(binSPEI_m, probs)
	}
	
#19.
	if(any(simulation_timescales=="monthly") & aon$monthlyPlantGrowthControls){
		temp <- c(temp, paste("NemaniEtAl2003.NPPControl.", c("Temperature", "Water", "Radiation"), "_none_mean", sep=""))
	}
	
#20.
	if(any(simulation_timescales=="daily") & aon$dailyC4_TempVar){
		temp <- c(temp, paste("TeeriEtAl1976.NSadj.", c("TempAirMin.7thMonth_C", "FreezeFreeGrowingPeriod_days", "AccumDegreeDaysAbove65F_daysC"), "_mean", sep=""))
	}
	
#21.
	if(any(simulation_timescales=="daily") & aon$dailyDegreeDays){
		temp <- c(temp, paste("DegreeDays.Base", DegreeDayBase, "C.dailyTmean_Cdays_mean", sep=""))
	}
	
	##############################################################---Aggregation: Yearly water balance---##############################################################
	
#22.
	if(any(simulation_timescales=="yearly") & aon$yearlyWaterBalanceFluxes) {
		temp <- c(temp, paste(c("Rain_mm", "Rain.ReachingSoil_mm", "Snowfall_mm", "Snowmelt_mm", "Snowloss_mm", "Interception.Total_mm", "Interception.Vegetation_mm", "Interception.Litter_mm", "Evaporation.InterceptedByVegetation_mm", "Evaporation.InterceptedByLitter_mm", "Infiltration_mm", "Runoff_mm", "Evaporation.Total_mm", "Evaporation.Soil.Total_mm", "Evaporation.Soil.topLayers_mm",
								"Evaporation.Soil.bottomLayers_mm", "Transpiration.Total_mm", "Transpiration.topLayers_mm", "Transpiration.bottomLayers_mm", "HydraulicRedistribution.TopToBottom_mm", "Percolation.TopToBottom_mm", "DeepDrainage_mm", "SWC.StorageChange_mm", "TranspirationBottomToTranspirationTotal_fraction", "TtoAET", "EStoAET", "AETtoPET", "TtoPET", "EStoPET"), "_mean", sep=""))
	}
	
	##############################################################---Aggregation: Daily extreme values---##############################################################
	
#23.
	if(any(simulation_timescales=="daily") & aon$dailyTranspirationExtremes) {
		temp <- c(temp, paste("Transpiration.", c("DailyMax", "DailyMin"), "_mm_mean", sep=""), paste("Transpiration.", c("DailyMax", "DailyMin"), "_doy_mean", sep=""))
	}
	
#24.
	if(any(simulation_timescales=="daily") & aon$dailyTotalEvaporationExtremes) {
		temp <- c(temp, paste("Evaporation.Total.", c("DailyMax", "DailyMin"), "_mm_mean", sep=""), paste("Evaporation.Total.", c("DailyMax", "DailyMin"), "_doy_mean", sep=""))
	}
	
#25.
	if(any(simulation_timescales=="daily") & aon$dailyDrainageExtremes) {
		temp <- c(temp, paste("DeepDrainage.", c("DailyMax", "DailyMin"), "_mm_mean", sep=""), paste("DeepDrainage.", c("DailyMax", "DailyMin"), "_doy_mean", sep=""))
	}
	
#26.
	if(any(simulation_timescales=="daily") & aon$dailyInfiltrationExtremes) {
		temp <- c(temp, paste("Infiltration.", c("DailyMax", "DailyMin"), "_mm_mean", sep=""), paste("Infiltration.", c("DailyMax", "DailyMin"), "_doy_mean", sep=""))
	}
	
#27.
	if(any(simulation_timescales=="daily") & aon$dailyAETExtremes) {
		temp <- c(temp, paste("AET.", c("DailyMax", "DailyMin"), "_mm_mean", sep=""), paste("AET.", c("DailyMax", "DailyMin"), "_doy_mean", sep=""))
	}
	
#28.
	if(any(simulation_timescales=="daily") & aon$dailySWPextremes){
		temp <- c(temp, paste("SWP.", rep(c("topLayers.", "bottomLayers."), each=2), rep(c("DailyMax", "DailyMin"), times=2), "_doy_mean", sep=""))
	}
	
	##############################################################---Aggregation: Ecological dryness---##############################################################
	
#29.
	if(any(simulation_timescales=="daily") & aon$dailyWetDegreeDays){
		temp <- c(temp, paste("WetDegreeDays.SWPcrit", rep(paste(abs(round(-1000*SWPcrit_MPa, 0)), "kPa", sep=""), each=3), rep(c(".topLayers", ".bottomLayers", ".anyLayer"), times=length(SWPcrit_MPa)), "_Cdays_mean", sep=""))
	}
	
#30.
	if(any(simulation_timescales=="monthly") & aon$monthlySWPdryness){
		temp <- c(temp, paste("DrySoilPeriods.SWPcrit", rep(paste(abs(round(-1000*SWPcrit_MPa, 0)), "kPa", sep=""), times=2), ".NSadj.", rep(c("topLayers", "bottomLayers"), each=length(SWPcrit_MPa)), ".Duration.Total_months_mean", sep=""), 
				paste("DrySoilPeriods.SWPcrit", rep(paste(abs(round(-1000*SWPcrit_MPa, 0)), "kPa", sep=""), times=2), ".NSadj.", rep(c("topLayers", "bottomLayers"), each=length(SWPcrit_MPa)), ".Start_month_mean", sep=""))
	}
	
#31.
	if(any(simulation_timescales=="daily") & aon$dailySWPdrynessANDwetness){
		temp <- c(temp, paste(rep(c("WetSoilPeriods", "DrySoilPeriods"), each=8), ".SWPcrit", rep(paste(abs(round(-1000*SWPcrit_MPa, 0)), "kPa", sep=""), each=16), ".NSadj.", c(rep(c("topLayers", "bottomLayers"), times=4), rep(rep(c("topLayers", "bottomLayers"), each=2), times=2)), 
						rep(c(".AnyLayerWet.", ".AllLayersWet.", ".AllLayersDry.", ""), each=4), c(rep(rep(c("Duration.Total_days", "Duration.LongestContinuous_days"), each=2), times=2), rep(c("Duration.Total_days", "Duration.LongestContinuous_days"), times=2), rep(c(".PeriodsForAtLeast10Days.Start_doy", ".PeriodsForAtLeast10Days.End_doy"), times=2)), "_mean", sep=""))
	}
	
#32.
	if(any(simulation_timescales=="daily") & aon$dailySWPdrynessDurationDistribution){
		deciles <- (0:10)*10/100
		quantiles <- (0:4)/4
		mo_seasons <- matrix(data=c(12,1:11), ncol=3, nrow=4, byrow=TRUE)
		season.flag <- c("DJF", "MAM", "JJA", "SON")
		for(icrit in seq(along=SWPcrit_MPa)) {
			for(season in 1:nrow(mo_seasons)){
				temp <- c(temp, paste("DrySoilPeriods.SWPcrit", paste(abs(round(-1000*SWPcrit_MPa[icrit], 0)), "kPa", sep=""), ".Month", season.flag[season], ".", rep(c("topLayers", "bottomLayers"), each=length(quantiles)), ".Duration_days_quantile", rep(quantiles, times=2), sep=""))
			}
		}
		rm(deciles, quantiles, mo_seasons, season.flag)
	}
	
#33.
	if(any(simulation_timescales=="daily") && aon$dailySWPdrynessEventSizeDistribution) {
		binSize <- c(1, 8, 15, 29, 57, 183, 367) #closed interval lengths in [days] within a year; NOTE: n_variables is set for binsN == 4
		binsN <- length(binSize) - 1
		binTitle <- paste("SizeClass", paste(binSize[-length(binSize)], binSize[-1]-1, sep="to") ,"days", sep="")
		for(icrit in seq(along=SWPcrit_MPa)) {
			temp <- c(temp, paste("DrySoilPeriods.SWPcrit", paste(abs(round(-1000*SWPcrit_MPa[icrit], 0)), "kPa", sep=""), ".Annual.", rep(c("topLayers", "bottomLayers"), each=binsN+1), rep(c("_count", paste(".", binTitle, "_fraction", sep="")), times=2), "_mean", sep=""))
		}
		rm(binSize, binsN, binTitle)
	}
	
#34.
	if(any(simulation_timescales=="daily") && aon$dailySWPdrynessIntensity) {
		for(icrit in seq(along=SWPcrit_MPa)){
			temp <- c(temp, paste("DrySoilPeriods.SWPcrit", paste(abs(round(-1000*SWPcrit_MPa[icrit], 0)), "kPa", sep=""), ".MissingWater.", rep(c("topLayers", "bottomLayers"), each=4), ".", rep(c("AnnualSum_mmH2O", "PerEventPerDay_mmH2O", "Duration.Event_days", "Events_count"), times=2), "_mean", sep=""))
		}
	}
	
	##############################################################---Aggregation: Mean monthly values---##############################################################
	
#35.
	if(any(simulation_timescales=="monthly") & aon$monthlyTemp){
		temp <- c(temp, paste("TempAir.m", st_mo, "_C_mean", sep=""))
	}
	
#36.
	if(any(simulation_timescales=="monthly") & aon$monthlyPPT){
		temp <- c(temp, paste("Precip.m", st_mo, "_mm_mean", sep=""))
	}
	
#37.
	if(any(simulation_timescales=="monthly") & aon$monthlySnowpack){
		temp <- c(temp, paste("Snowpack.m", st_mo, "_mmSWE_mean", sep=""))
	}
	
#38.
	if(any(simulation_timescales == "monthly") & aon$monthlySoilTemp) {
		temp <- c(temp, paste("TempSoil.", c(paste("topLayers.m", st_mo, sep=""), paste("bottomLayers.m", st_mo, sep="")), "_C_mean", sep=""))
	}
	
#39.
	if(any(simulation_timescales=="monthly") & aon$monthlyRunoff){
		temp <- c(temp, paste("Runoff.Total.m", st_mo, "_mm_mean", sep=""))
	}
	
#40.
	if(any(simulation_timescales=="monthly") & aon$monthlyHydraulicRedistribution){
		temp <- c(temp, paste("HydraulicRedistribution.", c(paste("topLayers.m", st_mo, sep=""), paste("bottomLayers.m", st_mo, sep="")), "_mm_mean", sep=""))
	}
	
#41.
	if(any(simulation_timescales=="monthly") & aon$monthlyInfiltration){
		temp <- c(temp, paste("Infiltration.m", st_mo, "_mm_mean", sep=""))
	}
	
#42.
	if(any(simulation_timescales=="monthly") & aon$monthlySWP){
		temp <- c(temp, paste("SWP.", c(paste("topLayers.m", st_mo, sep=""), paste("bottomLayers.m", st_mo, sep="")), "_MPa_FromVWCmean", sep=""))
	}
	
#43.
	if(any(simulation_timescales=="monthly") & aon$monthlyVWC){
		temp <- c(temp, paste("VWC.", c(paste("topLayers.m", st_mo, sep=""), paste("bottomLayers.m", st_mo, sep="")), "_mPERm_mean", sep=""))
	}
	
#44.
	if(any(simulation_timescales=="monthly") & aon$monthlySWC){
		temp <- c(temp, paste("SWC.", c(paste("topLayers.m", st_mo, sep=""), paste("bottomLayers.m", st_mo, sep="")), "_mm_mean", sep=""))
	}
	
#45.
	if(any(simulation_timescales=="monthly") & aon$monthlySWA){
		temp <- c(temp, paste("AWC.", c(paste("topLayers.m", st_mo, sep=""), paste("bottomLayers.m", st_mo, sep="")), "_mm_mean", sep=""))
	}
	
#46.
	if(any(simulation_timescales=="monthly") & aon$monthlyTranspiration){
		temp <- c(temp, paste("Transpiration.", c(paste("topLayers.m", st_mo, sep=""), paste("bottomLayers.m", st_mo, sep="")), "_mm_mean", sep=""))
	}
	
#47.
	if(any(simulation_timescales=="monthly") & aon$monthlySoilEvaporation){
		temp <- c(temp, paste("Evaporation.Soil.m", st_mo, "_mm_mean", sep=""))
	}
	
#48.
	if(any(simulation_timescales=="monthly") & aon$monthlyAET){
		temp <- c(temp, paste("AET.m", st_mo, "_mm_mean", sep=""))
	}
	
#49.
	if(any(simulation_timescales=="monthly") & aon$monthlyPET){
		temp <- c(temp, paste("PET.m", st_mo, "_mm_mean", sep=""))
	}
	
#50.
	if(any(simulation_timescales=="monthly") & aon$monthlyAETratios){
		temp <- c(temp, paste(rep(c("TranspToAET.m", "EvapSoilToAET.m"), each=12), st_mo, "_fraction_mean", sep=""))
	}
	
#51.
	if(any(simulation_timescales=="monthly") & aon$monthlyPETratios){
		temp <- c(temp, paste(rep(c("TranspToPET.m", "EvapSoilToPET.m"), each=12), st_mo, "_fraction_mean", sep=""))
	}
	
	##############################################################---Aggregation: Potential regeneration---##############################################################
	
#52.
	if(any(simulation_timescales=="daily")  & aon$dailyRegeneration_bySWPSnow) {
		temp <- c(temp, "Regeneration.Potential.SuitableYears.NSadj_fraction_mean")
	}
	
#53.
	if(any(simulation_timescales=="daily")  & aon$dailyRegeneration_GISSM & no.species_regeneration > 0){
		for(sp in 1:no.species_regeneration){
			SeedlingMortality_CausesByYear_colnames <- paste("Seedlings1stSeason.Mortality.", c("UnderneathSnowCover", "ByTmin", "ByTmax", "ByChronicSWPMax", "ByChronicSWPMin", "ByAcuteSWPMin",
					"DuringStoppedGrowth.DueSnowCover", "DuringStoppedGrowth.DueTmin", "DuringStoppedGrowth.DueTmax"), sep="")
										
			temp.header1 <- c(paste(temp1 <- c("Germination", "Seedlings1stSeason"), ".SuitableYears_fraction_mean", sep=""),
					paste(rep(temp1, each=3), ".UnsuitableYears.Successive_years_quantile", rep(c(0.05, 0.5, 0.95), times=2), sep=""),
					paste(temp1, ".SuitableDaysPerYear_days_mean", sep=""),
					paste(paste(rep(temp1, each=3), ".", c("Start", "Middle", "End"), sep=""), "_doy_quantile", rep(c(0.9, 0.5, 0.9), times=2), sep=""),
					paste("Germination.RestrictedDays.By", c("Tmax", "Tmin", "SWPmin", "AnyCondition", "TimeToGerminate"), "_days_mean", sep=""),
					"Germination.TimeToGerminate_days_mean",
					paste(SeedlingMortality_CausesByYear_colnames, "_days_mean", sep=""))
			
			temp <- c(temp, paste(colnames(param.species_regeneration)[sp], temp.header1, sep="."))
			
			#Output for time series: not yet implemented for db			
		}
	}
	
	AggOverallDataCols <- length(temp)
	
	dbOverallColumns <- length(temp)
	for(i in 1:dbOverallColumns) {
		temp[i] <- paste(c("\"", temp[i], "\"", " REAL"), collapse = "")
	}
	
	sdString <- gsub("_mean", "_sd", temp)
	meanString <- paste(c(header_vector, temp), collapse = ", ")
	sdString <-paste(c(header_vector, sdString), collapse = ", ")
	
	
	SQL_Table_Definitions1 <- paste("CREATE TABLE \"Aggregation_Overall_Mean\" (", meanString, ");", sep="")
	SQL_Table_Definitions2 <- paste("CREATE TABLE \"Aggregation_Overall_SD\" (", sdString, ");", sep="")
		
	empty <- ifelse(length(dbListTables(con))==0, TRUE, FALSE)
	if(cleanDB && !empty) for(i in 1:length(Tables)) { res <- dbSendQuery(con,paste("DROP TABLE ", '"',Tables[i], '"', sep="")); dbClearResult(res) }
	rs <- dbSendQuery(con, paste(SQL_Table_Definitions1, collapse = "\n"))
	dbClearResult(rs)
	rs <- dbSendQuery(con, paste(SQL_Table_Definitions2, collapse = "\n"))
	dbClearResult(rs)
	
	
	temp <- resultfiles.daily.labelsOne
	for(i in 1:366) {
		temp[i] <- paste(c("\"", temp[i], "\"", " REAL"), collapse = "")
	}
	temp <-paste(c(header_vector, temp), collapse = ", ")
	
	temp1 <- paste("doy", formatC(1:366, width=3, format="d", flag="0"), sep="")
	for(i in 1:366) {
		temp1[i] <- paste(c("\"", temp1[i], "\"", " REAL"), collapse = "")
	}
	temp1 <-paste(c(header_vector[1], "\"Layer\" INTEGER", header_vector[-1], temp1), collapse = ", ")
	temp1 <- sub(" PRIMARY KEY", "", temp1)
	
	if(any(simulation_timescales=="daily") && daily_no > 0) {
		for(doi in 1:daily_no) {
			if(regexpr("SWA", output_aggregate_daily[doi]) > 0){
				agg.resp <- "SWA"
				index.SWPcrit <- -as.numeric(sub("kPa", "", sub("SWAatSWPcrit", "", output_aggregate_daily[doi])))/1000
			} else {
				agg.resp <- output_aggregate_daily[doi]
			}
			agg.analysis <- switch(EXPR=agg.resp, AET=1, Transpiration=2, EvaporationSoil=1, EvaporationSurface=1, EvaporationTotal=1, VWC=2, SWC=2, SWP=2, SWA=2, Snowpack=1, Rain=1, Snowfall=1, Snowmelt=1, SnowLoss=1, Infiltration=1, DeepDrainage=1, PET=1, TotalPrecipitation=1, TemperatureMin=1, TemperatureMax=1, SoilTemperature=2, Runoff=1)
			
			tableName <- paste("Aggregation_Seasons_DailyValues_", output_aggregate_daily[doi], sep="")
			
			if(agg.analysis == 1){
					SQL_Table_Definitions1 <- paste("CREATE TABLE \"",tableName,"_Mean\" (", temp, ");", sep="")
					SQL_Table_Definitions2 <- paste("CREATE TABLE \"",tableName,"_SD\" (", temp, ");", sep="")
					rs <- dbSendQuery(con, paste(SQL_Table_Definitions1, collapse = "\n"))
					dbClearResult(rs)
					rs <- dbSendQuery(con, paste(SQL_Table_Definitions2, collapse = "\n"))
					dbClearResult(rs)
			} else {
					SQL_Table_Definitions1 <- paste("CREATE TABLE \"",tableName,"_Mean\" (", temp1, ");", sep="")
					SQL_Table_Definitions2 <- paste("CREATE TABLE \"",tableName,"_SD\" (", temp1, ");", sep="")
					rs <- dbSendQuery(con, paste(SQL_Table_Definitions1, collapse = "\n"))
					dbClearResult(rs)
					rs <- dbSendQuery(con, paste(SQL_Table_Definitions2, collapse = "\n"))
					dbClearResult(rs)
			}
			
		}
	}
	Tables<-dbListTables(con)
	sqlLines<-paste("CREATE INDEX idx_",as.character(1:length(Tables)), " ON \"",Tables,"\"(P_id,RunID,Scenario);",sep="")
	sqlLines<-c(sqlLines,paste("CREATE INDEX idx_",as.character((length(Tables)+1):(2*length(Tables))), " ON \"",Tables,"\"(RunID,Scenario,Labels);",sep=""))
	sqlLines<-c(sqlLines,paste("CREATE INDEX idx_",as.character((2*length(Tables)+1):(3*length(Tables))), " ON \"",Tables,"\"(Labels);",sep=""))
	
	for(j in 1:length(sqlLines)) rs<-dbGetQuery(con,sqlLines[j])
	
	rm(sqlLines, rs, sdString, meanString, temp, temp1, tableName, agg.analysis, agg.resp, SQL_Table_Definitions1, SQL_Table_Definitions2, header_vector, header, header.names, treatment_header1, treatment_header2, i1, i2)
}
rm(Tables) 
