# Settings
library(Rsoilwat31)
library(parallel)
library(compiler)

n_cores <- 20
startyear <- 1979
endyear <- 2010


# Paths
dir_prj <- "/PATH/TO/PROJECT"
dir_big <- "/PATH/TO/WEATHERDATABASE"
dir.create(dir_out <- file.path(dir_prj, "6_Results", "Weather_summary"), recursive = TRUE, showWarnings = FALSE)
ftemp <- file.path(dir_out, "Summary_climate_dbWeatherData_temp.csv")
fout <- file.path(dir_out, "Summary_climate_dbWeatherData.csv")


# Connect to weather database
dbWeatherDataFile <- file.path(dir_big, "1_DATA_SWInput", "dbWeatherData_decadalSDM_Projections_1979-2010.sqlite3")


dbW_setConnection(dbFilePath = dbWeatherDataFile, FALSE)
dbW_iSiteTable <- dbW_getSiteTable()
write.csv(dbW_iSiteTable, file = file.path(file.path(dir_prj, "6_Results", "Weather_summary", "Sites.cvs")), row.names = FALSE)
dbW_iScenarioTable <- dbW_getScenariosTable()
write.csv(dbW_iScenarioTable, file = file.path(file.path(dir_prj, "6_Results", "Weather_summary", "Scenarios.cvs")), row.names = FALSE)


# Define output
vars <- c("MAP_mm", "aPPT_mm_sd", "MAT_C", "MATmax_C", "MATmin_C")

climate <- as.data.frame(matrix(NA, nrow = nrow(dbW_iSiteTable) * nrow(dbW_iScenarioTable),
									ncol = 3 + length(vars),
									dimnames = list(NULL, c("Site_id", "Scenario_id", "Status", vars))))
climate[, "Site_id"] <- dbW_iSiteTable[, "Site_id"]
climate[, "Scenario_id"] <- rep(dbW_iScenarioTable[, "id"], each = nrow(dbW_iSiteTable))

ids_todo <- seq_len(nrow(climate))


# Check on progress
if (file.exists(ftemp)) {
	temp <- read.csv(ftemp, header = TRUE)
	if (nrow(temp) > 0) {
		i_duplics <- duplicated(temp[, c("Site_id", "Scenario_id", "Status")])
		if (any(i_duplics)) temp <- temp[!i_duplics, ]
		
		# Transfer to output
		ids_clim <- apply(climate[, c("Site_id", "Scenario_id")], 1, paste0, collapse = "_")
		ids_temp <- as.vector(apply(temp[, c("Site_id", "Scenario_id")], 1, paste0, collapse = "_"))
		
		imc <- match(ids_temp, ids_clim, nomatch = 0)
		imt <- match(ids_clim, ids_temp, nomatch = 0)
		
		climate[imc, vars] <- temp[imt, vars]
		
		ids_todo <- ids_todo[-imc]
	}	

} else {
	write.table(climate[0, ], file = ftemp, append = FALSE, sep = ",", dec = ".", qmethod = "double", row.names = FALSE, col.names = TRUE)
}

print(paste(Sys.time(), ": # run =", length(ids_todo), "out of", nrow(climate)))


# Go through the weather database

summarize_weather <- compiler::cmpfun(function(i, iclimate, scen_table, startyear, endyear, con_temp) {
	if (i %% 1000 == 1) print(paste(Sys.time(), ": run =", i))
	
	# Access data from database
	wtemp <- try(dbW_getWeatherData(Site_id = iclimate["Site_id"],
					startYear = startyear, endYear = endyear,
					Scenario = scen_table[as.integer(iclimate["Scenario_id"]), "Scenario"]),
				silent = TRUE)

	# Avoiding the Rsoilwat convenience function is at most 1% faster 
#	id_sc <- dbGetQuery(dbW_con, paste0("SELECT id FROM Scenarios WHERE Scenario='", iclimate["Scenario"], "';"))[1, 1]
#	temp <- dbGetQuery(dbW_con, paste0("SELECT StartYear,EndYear,data FROM WeatherData WHERE Site_id=", 
#            			iclimate["Site_id"], " AND Scenario=", id_sc, ";"))
#	wtemp <- try(dbW_blob_to_weatherData(temp$StartYear, temp$EndYear, temp$data))

	
	if (inherits(wtemp, "try-error")) {
		print(paste(Sys.time(), ": run =", i, "failed:", wtemp))
		iclimate["Status"] <- 0

	} else {
		iclimate["Status"] <- 1
		wd <- dbW_weatherData_to_dataframe(wtemp)

		# Calculate climate variables (this is the slow part of the function)
		wy_ppt <- tapply(wd[, "PPT_cm"], wd[, "Year"], sum)
		iclimate["MAP_mm"] <- round(mean(wy_ppt))
		iclimate["aPPT_mm_sd"] <- round(sd(wy_ppt), 2)

		wy_tmax <- tapply(wd[, "Tmax_C"], wd[, "Year"], mean)
		iclimate["MATmax_C"] <- round(mean(wy_tmax), 2)

		wy_tmin <- tapply(wd[, "Tmin_C"], wd[, "Year"], mean)
		iclimate["MATmin_C"] <- round(mean(wy_tmin), 2)

		#wd_tmean <- apply(wd[, c("Tmax_C", "Tmin_C")], 1, mean)
		#wy_tmean <- tapply(wd_tmean, wd[, "Year"], mean)
		wy_tmean <- apply(cbind(wy_tmax, wy_tmin), 1, mean)
		iclimate["MAT_C"] <- round(mean(wy_tmean), 2)
	}

	# Temporary output
	write.table(iclimate, file = con_temp, append = TRUE, sep = ",", dec = ".", qmethod = "double", row.names = FALSE, col.names = FALSE)

	iclimate
})

# Calculate in parallel
cl <- makeCluster(n_cores, type = "PSOCK", outfile = "workers_log.txt")
clusterExport(cl, c("climate", "ftemp", "summarize_weather", "dbWeatherDataFile", "dbW_iScenarioTable", "startyear", "endyear"))
clusterEvalQ(cl, {require(Rsoilwat31); dbW_setConnection(dbFilePath = dbWeatherDataFile, FALSE)})
#clusterEvalQ(cl, {
#	require(Rsoilwat31)
#	drv <- dbDriver("SQLite")
#	dbW_con <- dbConnect(drv, dbname = dbWeatherDataFile)
#})


print(paste(Sys.time(), ": start with the parallel loop"))
climate2 <- parSapply(cl, X = ids_todo, FUN = function(i)
						summarize_weather(i, iclimate = climate[i, ],
											scen_table = dbW_iScenarioTable,
											startyear = startyear,
											endyear = endyear,
											con_temp = ftemp))

clusterEvalQ(cl, dbDisconnect(dbW_con))
stopCluster(cl)

# Final save
write.csv(climate2, file = fout, row.names = FALSE)
