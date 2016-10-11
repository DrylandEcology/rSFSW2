# TODO: Add comment
#
# Author: Ryan Murphy
#
#
# This will generate all the SQL table definitions.
#
#
###############################################################################
suppressMessages(library(RSQLite))

#--------------------------------------------------------------------------------------------------#
#------------------------CREATE WEATHER DATABASE AND POPULATION WITH DAILY WEATHER FOR CURRENT CONDITIONS
if (createAndPopulateWeatherDatabase) {
	if (file.exists(dbWeatherDataFile)) {
		if (continueAfterAbort) {
			stop("Weather database exists, 'continueAfterAbort' is TRUE, and 'createAndPopulateWeatherDatabase' is TRUE: a maximum of two of these three conditions may simultaneously be TRUE: adjust inputs and restart")
		} else {
			print("Removing old database")
			file.remove(dbWeatherDataFile)
		}
	}

	# weather database contains rows for 1:max(SWRunInformation$site_id) (whether included or not)
	dbW_createDatabase(dbFilePath = dbWeatherDataFile,
		site_data = data.frame(Site_id = SWRunInformation$site_id,
						Latitude = SWRunInformation$Y_WGS84,
						Longitude = SWRunInformation$X_WGS84,
						Label = SWRunInformation$WeatherFolder,
						stringsAsFactors = FALSE),
		site_subset = runIDs_sites,
		scenarios = data.frame(Scenario = climate.conditions),
		compression_type = dbW_compression_type)

	Time <- Sys.time()

	# Extract weather data and move to weather database based on inclusion-invariant 'site_id'
	# Extract weather data per site
	if (!be.quiet) print(paste(Sys.time(), "started with moving single site weather data to database"))

	ids_single <- which(sites_dailyweather_source %in% c("LookupWeatherFolder", "Maurer2002_NorthAmerica")) ## position in 'runIDs_sites'
	if (length(ids_single) > 0) {
		if (any(sites_dailyweather_source == "Maurer2002_NorthAmerica"))
			Maurer <- with(SWRunInformation[runIDs_sites[ids_single], ], create_filename_for_Maurer2002_NorthAmerica(X_WGS84, Y_WGS84))

		for (i in seq_along(ids_single)) {
			i_idss <- ids_single[i]
			i_site <- runIDs_sites[i_idss]

			if (!be.quiet && i %% 100 == 1)
				print(paste(Sys.time(), "storing weather data of site", SWRunInformation$Label[i_site], i, "of", length(ids_single), "sites in database"))

			if (sites_dailyweather_source[i_idss] == "LookupWeatherFolder") {
				weatherData <- ExtractLookupWeatherFolder(dir.weather = file.path(dir.sw.in.tr, "LookupWeatherFolder"),
									weatherfoldername = SWRunInformation$WeatherFolder[i_site])

			} else if (sites_dailyweather_source[i_idss] == "Maurer2002_NorthAmerica") {
				weatherData <- ExtractGriddedDailyWeatherFromMaurer2002_NorthAmerica(
				          dir_data = dir.ex.maurer2002,
				          cellname = Maurer[i],
									startYear = simstartyr,
									endYear = endyr)

			} else {
				stop(paste(sites_dailyweather_source[i_idss], "not implemented"))
			}

			if (!is.null(weatherData)) {
				years <- as.integer(names(weatherData))
				data_blob <- dbW_weatherData_to_blob(weatherData, type = dbW_compression_type)
				Rsoilwat31:::dbW_addWeatherDataNoCheck(Site_id = SWRunInformation$site_id[i_site],
					Scenario_id = 1,
					StartYear = years[1],
					EndYear = years[length(years)],
					weather_blob = data_blob)
			} else {
				print(paste("Moving daily weather data to database unsuccessful", SWRunInformation$Label[i_site]))
			}
		}
		rm(ids_single, i_idss, i_site, weatherData, years, data_blob)
	}

	# Extract weather data for all sites based on inclusion-invariant 'site_id'
	ids_DayMet_extraction <- runIDs_sites[which(sites_dailyweather_source == "DayMet_NorthAmerica")] ## position in 'runIDs_sites'
	if (length(ids_DayMet_extraction) > 0) {
		ExtractGriddedDailyWeatherFromDayMet_NorthAmerica_dbW(
		  dir_data = dir.ex.daymet,
		  site_ids = SWRunInformation$site_id[ids_DayMet_extraction],
			coords_WGS84 = SWRunInformation[ids_DayMet_extraction, c("X_WGS84", "Y_WGS84"), drop = FALSE],
			start_year = simstartyr,
			end_year = endyr,
			dir_temp = dir.out.temp,
			dbW_compression_type = dbW_compression_type)
	}
	rm(ids_DayMet_extraction)

	ids_NRCan_extraction <- runIDs_sites[which(sites_dailyweather_source == "NRCan_10km_Canada")]
	if (length(ids_NRCan_extraction) > 0) {
		ExtractGriddedDailyWeatherFromNRCan_10km_Canada(
		  dir_data = dir.ex.NRCan,
		  site_ids = SWRunInformation$site_id[ids_NRCan_extraction],
			coords_WGS84 = SWRunInformation[ids_NRCan_extraction, c("X_WGS84", "Y_WGS84"), drop = FALSE],
			start_year = simstartyr,
			end_year = endyr,
			dir_temp = dir.out.temp,
			dbW_compression_type = dbW_compression_type,
			do_parallel = parallel_runs && identical(parallel_backend, "snow"),
			ncores = num_cores)
	}
	rm(ids_NRCan_extraction)

	ids_NCEPCFSR_extraction <- runIDs_sites[which(sites_dailyweather_source == "NCEPCFSR_Global")]
	if (length(ids_NCEPCFSR_extraction) > 0) {
		GriddedDailyWeatherFromNCEPCFSR_Global(
		  site_ids = SWRunInformation$site_id[ids_NCEPCFSR_extraction],
			dat_sites = SWRunInformation[ids_NCEPCFSR_extraction, c("WeatherFolder", "X_WGS84", "Y_WGS84"), drop = FALSE],
			start_year = simstartyr,
			end_year = endyr,
			meta_cfsr = prepd_CFSR,
			n_site_per_core = chunk_size.options[["DailyWeatherFromNCEPCFSR_Global"]],
			do_parallel = parallel_runs && parallel_init,
			parallel_backend = parallel_backend,
			cl = if (identical(parallel_backend, "snow")) cl else NULL,
			rm_temp = deleteTmpSQLFiles,
			continueAfterAbort = continueAfterAbort,
			dir_temp = dir.out.temp,
			dbW_compression_type = dbW_compression_type)
	}
	rm(ids_NCEPCFSR_extraction)

	dbW_disconnectConnection()
}


#--------------------------------------------------------------------------------------------------#
#------------------------PREPARE OUTPUT DATABASES

# NOTE: Do not change the design of the output database without adjusting the iterator functions 'it_Pid', 'it_exp', and 'it_site' (see part 4)

con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = name.OutputDB)
Tables <- RSQLite::dbListTables(con)

# PRAGMA, see http://www.sqlite.org/pragma.html
PRAGMA_settings1 <- c("PRAGMA cache_size = 400000;",
					  "PRAGMA synchronous = 1;",
					  "PRAGMA locking_mode = EXCLUSIVE;",
					  "PRAGMA temp_store = MEMORY;",
					  "PRAGMA auto_vacuum = NONE;")
PRAGMA_settings2 <- c(PRAGMA_settings1,
					  "PRAGMA page_size=65536;", # no return value
					  "PRAGMA max_page_count=2147483646;", # returns the maximum page count
					  "PRAGMA foreign_keys = ON;") #no return value

if (length(Tables) == 0) set_PRAGMAs(con, PRAGMA_settings2)
headerTables <- c("runs", "sqlite_sequence", "header", "run_labels", "scenario_labels",
                  "sites", "experimental_labels", "treatments", "simulation_years",
                  "weatherfolders", "aggregating_functions", "aggregating_timewindows",
                  "Meta")


if (length(Tables) == 0 || cleanDB) {

	.local <- function() {

		if (cleanDB && length(dbListTables(con)) > 0){
			unlink(name.OutputDB)
			con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = name.OutputDB)
			set_PRAGMAs(con, PRAGMA_settings2)
		}

		#############meta-data table#########################
    # Meta information
    stopifnot(DBI::dbIsValid(con))
    DBI::dbGetQuery(con, "CREATE TABLE \"Meta\" (\"Desc\" TEXT PRIMARY KEY, \"Value\" TEXT);")
    RSQLite::dbGetPreparedQuery(con, "INSERT INTO Meta VALUES(:Desc, :Value)",
      bind.data = data.frame(Desc = c("Version", "DateTime_Creation"),
                  Value = c("2.0.0", format(Sys.time(), usetz = TRUE))))
		##################################################

		######################

		RSQLite::dbGetQuery(con, "CREATE TABLE weatherfolders(id INTEGER PRIMARY KEY AUTOINCREMENT, folder TEXT UNIQUE NOT NULL);")

		if (!(all(any((SWRunInformation$dailyweather_source[runIDs_sites] == "LookupWeatherFolder")),
				  any(create_treatments == "LookupWeatherFolder")))) {
			if (any(!is.na(SWRunInformation$WeatherFolder))) {
				RSQLite::dbBegin(con)
				RSQLite::dbGetPreparedQuery(con, "INSERT INTO weatherfolders VALUES(NULL, :folder)",
					bind.data = data.frame(folder = unique(na.exclude(SWRunInformation$WeatherFolder)), stringsAsFactors = FALSE))
				RSQLite::dbCommit(con)

				# Slightly slower alternative to RSQLite::dbGetPreparedQuery()
#				temp <- unique(na.exclude(SWRunInformation$WeatherFolder))
#				RSQLite::dbWriteTable(con, "weatherfolders", append = TRUE,
#					value = data.frame(id = rep(NA, length(temp)), folder = temp), row.names = FALSE)

			} else {
				stop("All WeatherFolder names in master input file are NAs.")
			}
		}


		#############Site Table############################
		# Note: invariant to 'include_YN', i.e., do not subset rows of 'SWRunInformation'
		index_sites <- sort(unique(c(sapply(c("Label", "site_id", "WeatherFolder", "X_WGS84", "Y_WGS84", "ELEV_m", "Include_YN"),
				function(x) which(x == colnames(SWRunInformation))),
			Index_RunInformation)))
		sites_data <- data.frame(SWRunInformation[, index_sites], row.names = NULL, check.rows = FALSE, check.names = FALSE, stringsAsFactors = FALSE)
		# Get WeatherFolder_id from table weatherfolders
		sites_data$WeatherFolder <- getSiteIds(con, sites_data$WeatherFolder)
		colnames(sites_data) <- sub(pattern = "WeatherFolder", replacement = "WeatherFolder_id", colnames(sites_data))
		site_col_types <- sapply(sites_data, function(x) RSQLite::dbDataType(con, x))

		RSQLite::dbGetQuery(con,
			paste0("CREATE TABLE sites(\"id\" INTEGER PRIMARY KEY AUTOINCREMENT, ",
				paste0('\"', colnames(sites_data), '\" ', site_col_types, collapse = ", "),
				", FOREIGN KEY(WeatherFolder_id) REFERENCES weatherfolders(id));"))

		RSQLite::dbWriteTable(con, "sites", append = TRUE,
			value = cbind(id = NA, sites_data), row.names = FALSE)

		rm(site_col_types, sites_data)

		useExperimentals <- expN > 0 && length(create_experimentals) > 0
		useTreatments <- !(length(create_treatments[!(create_treatments %in% create_experimentals)])==0)

		#############simulation_years table#########################
		RSQLite::dbGetQuery(con, "CREATE TABLE simulation_years(id INTEGER PRIMARY KEY AUTOINCREMENT, simulationStartYear INTEGER NOT NULL, StartYear INTEGER NOT NULL, EndYear INTEGER NOT NULL);")
		##################################################


		##########Create table experimental_labels only if using experimentals
		if (useExperimentals) {
			RSQLite::dbGetQuery(con, "CREATE TABLE experimental_labels(id INTEGER PRIMARY KEY AUTOINCREMENT, label TEXT UNIQUE NOT NULL);")
			RSQLite::dbBegin(con)
			RSQLite::dbGetPreparedQuery(con, "INSERT INTO experimental_labels VALUES(NULL, :label);",
				bind.data = data.frame(label = sw_input_experimentals[,1], stringsAsFactors = FALSE))
			RSQLite::dbCommit(con)
		}
		################################

		#If LookupWeatherFolder is ON we need to make sure all of the weather folders are in weatherfolders table
#TODO: WeatherFolder update
		if (any(create_treatments=="LookupWeatherFolder")) {
			#which ones are not in SWRunInformation$WeatherFolder

			#make a combined list of experimentals and treatments LookupWeatherFolder List
			#first add any from the experimentals table if its turned on
			#next add any from the treatments table if its turned on
			treatments_lookupweatherfolders <- character(0)
			if (any(names(sw_input_treatments_use[sw_input_treatments_use])=="LookupWeatherFolder")) {
				treatments_lookupweatherfolders <- c(treatments_lookupweatherfolders, sw_input_treatments$LookupWeatherFolder[runIDs_sites])
			}
			if (any(create_experimentals=="LookupWeatherFolder")) {
				treatments_lookupweatherfolders <- c(treatments_lookupweatherfolders, sw_input_experimentals$LookupWeatherFolder[runIDs_sites])
			}
			#Remove NA because that defaults to sites default weatherFolder also make sure each folder is unique
			treatments_lookupweatherfolders <- treatments_lookupweatherfolders[!is.na(treatments_lookupweatherfolders)]
			treatments_lookupweatherfolders <- unique(treatments_lookupweatherfolders)
			if (length(treatments_lookupweatherfolders) == 0) {
				print("LookupWeatherFolder is turned on in treatments or experimentals or both but is not used")
			} else {
				#make a temp data.frame of a column NA's and a column of folder names
				LookupWeatherFolder_index <- data.frame(id=rep(NA,length(treatments_lookupweatherfolders)), folder=treatments_lookupweatherfolders, stringsAsFactors = F)
				#Get the id from sites table if the folder is in it
				LookupWeatherFolder_index$id <- getSiteIds(con, LookupWeatherFolder_index$folder)
				#if there are any NA's we need to add those to the weatherfolder db table and update its id in our lookuptable for weatherfolder
				if (any(is.na(LookupWeatherFolder_index$id))) {
					#get max id from weatherfolders table
					temp<-is.na(LookupWeatherFolder_index$id)
					weatherfolders_index <- as.numeric(RSQLite::dbGetQuery(con,"SELECT MAX(id) FROM weatherfolders;"))+1
					LookupWeatherFolder_index$id[temp] <- weatherfolders_index:(weatherfolders_index+length(LookupWeatherFolder_index$id[temp])-1)
					#Write those in
					RSQLite::dbBegin(con)
					RSQLite::dbGetPreparedQuery(con, "INSERT INTO weatherfolders VALUES(:id,:folder)", bind.data = LookupWeatherFolder_index[temp,])
					RSQLite::dbCommit(con)
				}
			}
		}

		# get unique rows from both treatments and experimentals
		if (useExperimentals) {#Only use experimentals if there is something in it
			#Are all the columns NA
			if (all(temp<-is.na(sw_input_experimentals[,create_experimentals]))) stop("All Columns in experimentals table are NA")
			if (any(apply(temp,MARGIN=2, function(x) all(x)))) warning("One ore more columns in experimentals table are turned on with no values or only with NA.")
			db_experimentals <- unique(sw_input_experimentals[,create_experimentals])
			#note experimentals should be unique if we have less rows then the original then lets throw an Error
			stopifnot(nrow(db_experimentals) == nrow(sw_input_experimentals))
		} else {
			#experimentals does not have any rows. Are any of the create_experimentals turned on
			if (length(create_experimentals) > 0 && expN == 0) stop("No rows in experimentals table but columns are turned on")
			if (expN > 0 && length(create_experimentals)==0) warning("Rows in experimentals are not being used.")
		}

		if (useTreatments) {
			# Note: invariant to 'include_YN', i.e., do not subset 'SWRunInformation'
			# we only need the columns that are turned on and not in experimentals. Experimentals over write.
			db_treatments <- unique(df<-sw_input_treatments[, create_treatments[!(create_treatments %in% create_experimentals)], drop=FALSE])
			db_treatments_rows <- nrow(db_treatments)
			#this maps locations from reduced
			temp<-duplicated(df)
			treatments_unique_map<-rep(NA,nrow(df))
			temp2 <- data.frame(t(df))
			treatments_unique_map[temp]<-match(data.frame(t(df[temp,])),temp2)
			treatments_unique_map[!temp] <- match(data.frame(t(df[!temp,])),temp2)
			db_treatments_map<-unique(treatments_unique_map)
			treatments_unique_map <- sapply(treatments_unique_map, function(x) which(db_treatments_map == x))
			rm(db_treatments_map)
		} else {
			db_treatments_rows <- 1
		}

		#Replace the LookupWeatherFolder with the LookupWeatherFolder_id in either db_experimentals or db_treatments
		if (any(create_treatments=="LookupWeatherFolder")) {
			if (any(create_experimentals=="LookupWeatherFolder")) {
				#rename the column
				colnames(db_experimentals)[where(create_experimentals=="LookupWeatherFolder")] <- "LookupWeatherFolder_id"
				#get the id numbers for those columns and replace text
				db_experimentals$LookupWeatherFolder_id <- sapply(db_experimentals$LookupWeatherFolder_id,function(x) LookupWeatherFolder_index$id[LookupWeatherFolder_index$folder==x])
			} else {
				#rename the column
				colnames(db_treatments)[which(colnames(db_treatments)=="LookupWeatherFolder")] <- "LookupWeatherFolder_id"
				#get the id numbers for those columns and replace text
				db_treatments$LookupWeatherFolder_id <- sapply(db_treatments$LookupWeatherFolder_id,function(x) LookupWeatherFolder_index$id[LookupWeatherFolder_index$folder==x])
			}
		}
		useTreatmentWeatherFolder <- FALSE
		if (useExperimentals | useTreatments) {
			#Create a table to hold the values going into the database
			temp_numberRows <- ifelse(useExperimentals,nrow(db_experimentals)*db_treatments_rows,nrow(db_treatments))
			temp_numberColumns <- ifelse(useExperimentals,3,2)+length(create_treatments)
			temp_columnNames <- c("id",if (useExperimentals) c("experimental_id"),"simulation_years_id",create_treatments)
			db_combined_exp_treatments <- data.frame(matrix(data=NA, nrow=temp_numberRows, ncol=temp_numberColumns,dimnames=list(NULL, temp_columnNames)),stringsAsFactors = FALSE)
			rm(temp_numberColumns,temp_columnNames,temp_numberRows)

			#fill in the id column.
			db_combined_exp_treatments$id <- 1:nrow(db_combined_exp_treatments)

			#column types are listed in this data.frame along with what table it is from
			db_treatments_column_types <- data.frame(column=create_treatments, type=character(length(create_treatments)),table=numeric(length(create_treatments)), stringsAsFactors = FALSE)
			#0 for teatments 1 for experimentals
			db_treatments_column_types[db_treatments_column_types[,1] %in% create_experimentals,3] <- 1

			######################
			#Get the column types from the proper tables
			db_treatments_column_types[,2] <- sapply(db_treatments_column_types[,1], function(columnName) {
					if (columnName %in% create_experimentals) {
						RSQLite::dbDataType(con, sw_input_experimentals[,columnName])
					} else if (columnName %in% create_treatments[!(create_treatments %in% create_experimentals)]) {
						RSQLite::dbDataType(con, sw_input_treatments[,columnName])
					}
				})

			#Finalize db_treatments_column_types
			#remove YearStart or YearEnd
			db_treatments_years <- NULL
			if (any(db_treatments_column_types$column == "YearStart")) {
				db_treatments_years <- rbind(db_treatments_years, db_treatments_column_types[which(db_treatments_column_types$column == "YearStart"),])
				db_treatments_column_types <- db_treatments_column_types[-which(db_treatments_column_types$column == "YearStart"),]
			}
			if (any(db_treatments_column_types$column == "YearEnd")) {
				db_treatments_years <- rbind(db_treatments_years, db_treatments_column_types[which(db_treatments_column_types$column == "YearEnd"),])
				db_treatments_column_types <- db_treatments_column_types[-which(db_treatments_column_types$column == "YearEnd"),]
			}

			#rename weather folder column name and create the fk
			fk_LookupWeatherFolder <- ""
			if (any(create_treatments=="LookupWeatherFolder")) {
				useTreatmentWeatherFolder <- TRUE
				db_treatments_column_types[which(db_treatments_column_types[,1] == "LookupWeatherFolder"),1:2] <- c("LookupWeatherFolder_id","INTEGER")
				colnames(db_combined_exp_treatments)[-(1:2)] <- db_treatments_column_types[,1]
				fk_LookupWeatherFolder <- ", FOREIGN KEY(LookupWeatherFolder_id) REFERENCES weatherfolders(id)"
			}
			#Create the table
			RSQLite::dbGetQuery(con, paste("CREATE TABLE treatments(id INTEGER PRIMARY KEY AUTOINCREMENT, ",if (useExperimentals) "experimental_id INTEGER,", " simulation_years_id INTEGER, ", paste(db_treatments_column_types[,1], " ", db_treatments_column_types[,2], sep="", collapse =", "), if (useExperimentals || fk_LookupWeatherFolder!="") ", ", if (useExperimentals) "FOREIGN KEY(experimental_id) REFERENCES experimental_labels(id)",if (fk_LookupWeatherFolder != "") ", ",fk_LookupWeatherFolder,");", sep=""))

			#Lets put in the treatments into combined. This will repeat the reduced rows of treatments into combined
			if (useTreatments) {
				i_start <- which(colnames(db_treatments) == "YearStart")
				i_end <- which(colnames(db_treatments) == "YearEnd")
				i_use <- 1:ncol(db_treatments)
				if (length(i_start) > 0) i_use <- i_use[-i_start]
				if (length(i_end) > 0) i_use <- i_use[-i_end]
				db_combined_exp_treatments[,db_treatments_column_types[db_treatments_column_types[,3]==0,1]] <- db_treatments[, i_use]
				#Handle StartYear and EndYear separately
				if (length(i_start) > 0 && !is.null(db_treatments_years) && db_treatments_years[db_treatments_years$column == "YearStart", "table"] == 0) db_combined_exp_treatments[, colnames(db_combined_exp_treatments) == "YearStart"] <- db_treatments[, i_start]
				if (length(i_end) > 0 && !is.null(db_treatments_years) && db_treatments_years[db_treatments_years$column == "YearEnd", "table"] == 0) db_combined_exp_treatments[, colnames(db_combined_exp_treatments) == "YearEnd"] <- db_treatments[, i_end]
			}

			if (useExperimentals) {
				exp_start_rows<-seq(from=1,to=db_treatments_rows*nrow(db_experimentals),by=db_treatments_rows)
				#Insert data into our new data.frame
				for (start in exp_start_rows) {
					#Get experimental_label_id
					db_combined_exp_treatments[start:(start+db_treatments_rows-1),2] <- which(exp_start_rows==start)
					#insert all of the rows from experimentals
					db_combined_exp_treatments[start:(start+db_treatments_rows-1),db_treatments_column_types[db_treatments_column_types[,3]==1,1]] <- db_experimentals[which(exp_start_rows==start),]
				}
			}
		} else {
			db_combined_exp_treatments <- data.frame(matrix(data=c(1,1), nrow=1, ncol=2,dimnames=list(NULL, c("id","simulation_years_id"))),stringsAsFactors = FALSE)
			RSQLite::dbGetQuery(con, paste("CREATE TABLE treatments(id INTEGER PRIMARY KEY AUTOINCREMENT, simulation_years_id INTEGER);", sep=""))
		}

		#if the column startYear or endYear are present move over to simulation_years
		if (any(colnames(db_combined_exp_treatments) == "YearStart") || any(colnames(db_combined_exp_treatments) == "YearEnd")) {
			simulation_years<-matrix(data=NA, nrow=nrow(db_combined_exp_treatments), ncol = 4, dimnames=list(NULL,c("id","simulationStartYear","StartYear","EndYear")))
			#Get from treatments or get from settings
			if (any(colnames(db_combined_exp_treatments) == "YearStart")) {
				simulation_years[, "simulationStartYear"] <- db_combined_exp_treatments$YearStart
				db_combined_exp_treatments <- db_combined_exp_treatments[,-which(colnames(db_combined_exp_treatments) == "YearStart")]
			} else {
				simulation_years[, "simulationStartYear"] <- simstartyr
			}
			if (any(colnames(db_combined_exp_treatments) == "YearEnd")) {
				simulation_years[, "EndYear"] <- db_combined_exp_treatments$YearEnd
				db_combined_exp_treatments <- db_combined_exp_treatments[,-which(colnames(db_combined_exp_treatments) == "YearEnd")]
			} else {
				simulation_years[, "EndYear"] <- endyr
			}
			simulation_years[, "StartYear"] <- getStartYear(simulation_years[, "simulationStartYear"])

			unique_simulation_years <- unique(simulation_years)
			#each row is unique so add id to db_combined
			if (nrow(unique_simulation_years)==nrow(simulation_years)) {
				id <- seq_len(nrow(unique_simulation_years))
			  	unique_simulation_years<-cbind(id,unique_simulation_years[,2:4])
				db_combined_exp_treatments$simulation_years_id <- unique_simulation_years[,1]
			} else {#treatment table has a map to reduced rows in simulation_years
				temp<-duplicated(simulation_years)
				sim_years_unique_map<-rep(NA,nrow(simulation_years))
				temp2 <- data.frame(t(simulation_years))
				sim_years_unique_map[temp]<-match(data.frame(t(simulation_years[temp,])),temp2)
				sim_years_unique_map[!temp] <- match(data.frame(t(simulation_years[!temp,])),temp2)
				treatments_toYears_map<-unique(sim_years_unique_map)
				sim_years_unique_map <- sapply(sim_years_unique_map, function(x) which(treatments_toYears_map == x))
				rm(treatments_toYears_map)
				db_combined_exp_treatments$simulation_years_id <- sim_years_unique_map
			}
			#write to the database
			RSQLite::dbBegin(con)
			RSQLite::dbGetPreparedQuery(con, "INSERT INTO simulation_years VALUES(NULL, :simulationStartYear, :StartYear, :EndYear);", bind.data = data.frame(unique_simulation_years))
			RSQLite::dbCommit(con)
		} else {#Treatment option for simulation Years is turned off. Get the default one from settings.
			db_combined_exp_treatments$simulation_years_id <- 1
			RSQLite::dbBegin(con)
			RSQLite::dbGetPreparedQuery(con, "INSERT INTO simulation_years VALUES(NULL, :simulationStartYear, :StartYear, :EndYear);", bind.data = data.frame(simulationStartYear=simstartyr, StartYear=startyr, EndYear=endyr))
			RSQLite::dbCommit(con)
		}

		#Insert the data into the treatments table
		RSQLite::dbBegin(con)
		RSQLite::dbGetPreparedQuery(con, paste("INSERT INTO treatments VALUES(",paste(":",colnames(db_combined_exp_treatments),sep="",collapse=", "),")",sep=""), bind.data = db_combined_exp_treatments)
		RSQLite::dbCommit(con)

		##############scenario_labels table###############
		RSQLite::dbGetQuery(con, "CREATE TABLE scenario_labels(id INTEGER PRIMARY KEY AUTOINCREMENT, label TEXT UNIQUE NOT NULL);")
		RSQLite::dbBegin(con)
		RSQLite::dbGetPreparedQuery(con, "INSERT INTO scenario_labels VALUES(NULL, :label);", bind.data = data.frame(label=climate.conditions,stringsAsFactors = FALSE))
		RSQLite::dbCommit(con)
		##################################################

		#############run_labels table#########################
		# Note: invariant to 'include_YN', i.e., do not subset 'SWRunInformation'
		RSQLite::dbGetQuery(con, "CREATE TABLE run_labels(id INTEGER PRIMARY KEY AUTOINCREMENT, label TEXT UNIQUE NOT NULL);")
		RSQLite::dbBegin(con)
		if (useExperimentals) {
			RSQLite::dbGetPreparedQuery(con, "INSERT INTO run_labels VALUES(NULL, :label);",
				bind.data = data.frame(label = paste(formatC(SWRunInformation$site_id, width = counter.digitsN, format = "d", flag = "0"),
													rep(sw_input_experimentals[, 1], each = runsN_master), labels,
													sep = "_"),
										stringsAsFactors = FALSE))
		} else {
			RSQLite::dbGetPreparedQuery(con, "INSERT INTO run_labels VALUES(NULL, :label);",
				bind.data = data.frame(label = labels, stringsAsFactors = FALSE))
		}
		RSQLite::dbCommit(con)
		##################################################

		##############agg_fun table###############
		stopifnot(c("agg_fun", "type") %in% names(agg_fun_defs))
		RSQLite::dbGetQuery(con, "CREATE TABLE aggregating_functions(id INTEGER PRIMARY KEY AUTOINCREMENT, agg_fun TEXT UNIQUE NOT NULL, type TEXT);")
		RSQLite::dbBegin(con)
		RSQLite::dbGetPreparedQuery(con, "INSERT INTO aggregating_functions VALUES(NULL, :agg_fun, :type);",
			bind.data = agg_fun_defs)
		RSQLite::dbCommit(con)
		##################################################

		##############aggregating time windows table###############
		stopifnot(c("label", "agg_start", "agg_end") %in% names(agg_windows))
		RSQLite::dbGetQuery(con, "CREATE TABLE aggregating_timewindows(id INTEGER PRIMARY KEY AUTOINCREMENT, label TEXT UNIQUE NOT NULL, agg_start INTEGER, agg_end INTEGER);")
		RSQLite::dbBegin(con)
		RSQLite::dbGetPreparedQuery(con, "INSERT INTO aggregating_timewindows VALUES(NULL, :label, :agg_start, :agg_end);",
			bind.data = agg_windows)
		RSQLite::dbCommit(con)
		##################################################


		#####################runs table###################
		# Note: invariant to 'include_YN', i.e., do not subset 'SWRunInformation'
		RSQLite::dbGetQuery(con, "CREATE TABLE runs(P_id INTEGER PRIMARY KEY, label_id INTEGER NOT NULL, site_id INTEGER NOT NULL, treatment_id INTEGER NOT NULL, scenario_id INTEGER NOT NULL, FOREIGN KEY(label_id) REFERENCES run_labels(id), FOREIGN KEY(site_id) REFERENCES sites(id), FOREIGN KEY(treatment_id) REFERENCES treatments(id), FOREIGN KEY(scenario_id) REFERENCES scenario_labels(id));")
		db_runs <- data.frame(matrix(data = 0,
									 nrow = runsN_Pid,
									 ncol = 5,
									 dimnames = list(NULL, c("P_id", "label_id", "site_id", "treatment_id", "scenario_id"))))

		db_runs$P_id <- seq_len(runsN_Pid)
		db_runs$label_id <- rep(seq_len(runsN_incl), each = scenario_No)
		db_runs$site_id <- rep(rep(SWRunInformation$site_id, times = max(expN, 1L)), each = scenario_No)
		db_runs$scenario_id <- rep(seq_len(scenario_No), times = runsN_incl)

		if (useTreatments) {
			if (useExperimentals) {
				i_exp<-as.vector(matrix(data=exp_start_rows,nrow=runsN_master,ncol=expN,byrow=T))
				db_runs$treatment_id <- rep(i_exp+(treatments_unique_map-1),each=scenario_No)
			} else {
				db_runs$treatment_id <- rep(treatments_unique_map,each=scenario_No)
			}
		} else {
			if (useExperimentals) {
				i_exp<-as.vector(matrix(data=exp_start_rows,nrow=runsN_master,ncol=expN,byrow=T))
				db_runs$treatment_id <- rep(i_exp,each=scenario_No)
			} else {
				db_runs$treatment_id <- 1
			}
		}
		RSQLite::dbBegin(con)
		RSQLite::dbGetPreparedQuery(con, "INSERT INTO runs VALUES(:P_id, :label_id, :site_id, :treatment_id, :scenario_id);", bind.data=db_runs)
		RSQLite::dbCommit(con)
		##################################################

		################CREATE VIEW########################
		if (length(Index_RunInformation) > 0) {
			sites_columns <- colnames(SWRunInformation)[Index_RunInformation]
			icol <- grep("WeatherFolder", sites_columns)
			if (length(icol) > 0)
				sites_columns <- sites_columns[-icol]
		} else {
			sites_columns <- NULL
		}
		treatment_columns <- colnames(db_combined_exp_treatments)[-(1:3)]
		if (useTreatmentWeatherFolder)
			treatment_columns <- treatment_columns[-grep("WeatherFolder", treatment_columns)]
		header_columns <- paste(c(
				"runs.P_id",
				"run_labels.label AS Labels",
				if (!is.null(sites_columns))
					paste0("sites.\"", sites_columns, "\"", collapse = ", "),
				if (useExperimentals)
					"experimental_labels.label AS Experimental_Label",
				"weatherfolders.folder AS WeatherFolder",
				if (useExperimentals || useTreatments)
					paste("treatments", treatment_columns, sep = ".", collapse = ", "),
				"simulation_years.StartYear",
				"simulation_years.simulationStartYear AS SimStartYear",
				"simulation_years.EndYear",
				"scenario_labels.label AS Scenario"),
			collapse = ", ")

		RSQLite::dbGetQuery(con, paste0(
			"CREATE VIEW header AS SELECT ", header_columns, " FROM runs, run_labels, sites, ",
			if (useExperimentals)
				"experimental_labels, ",
			"treatments, scenario_labels, simulation_years, weatherfolders",
			" WHERE runs.label_id=run_labels.id AND runs.site_id=sites.id AND",
			" runs.treatment_id=treatments.id AND runs.scenario_id=scenario_labels.id AND ",
			if (useTreatmentWeatherFolder) {
				"treatments.LookupWeatherFolder_id=weatherfolders.id AND "
			} else {
				"sites.WeatherFolder_id=weatherfolders.id AND "
			},
			if (useExperimentals)
				"treatments.experimental_id=experimental_labels.id AND ",
			"treatments.simulation_years_id=simulation_years.id;"
		))
		##################################################


	#B. Aggregation_Overall

		##############################################################---Aggregation: SoilWat inputs---##############################################################
		## Note: All '.' will be translated to "_" because of sqlite field name constraints
		temp <- character(0)

		fieldtag_SWPcrit_MPa <- paste0(abs(round(-1000 * SWPcrit_MPa, 0)), "kPa")
		fieldtag_Tmin_crit_C <- paste0(ifelse(Tmin_crit_C < 0, "Neg", ifelse(Tmin_crit_C > 0, "Pos", "")), abs(Tmin_crit_C), "C")
		fieldtag_Tmax_crit_C <- paste0(ifelse(Tmax_crit_C < 0, "Neg", ifelse(Tmax_crit_C > 0, "Pos", "")), abs(Tmax_crit_C), "C")
		fieldtag_Tmean_crit_C <- paste0(ifelse(Tmean_crit_C < 0, "Neg", ifelse(Tmean_crit_C > 0, "Pos", "")), abs(Tmean_crit_C), "C")
    fieldtag_drysoils <- paste0("AtLeast", duration_min_drysoils_days, "Days")

	#0.
		if (aon$input_SoilProfile) {
			temp <- paste0("SWinput.Soil.",
			              c("maxDepth_cm", "soilLayers_N",
			                "topLayers.Sand_fraction", "bottomLayers.Sand_fraction",
			                "topLayers.Clay_fraction", "bottomLayers.Clay_fraction",
			                "topLayers.Gravel_fraction", "bottomLayers.Gravel_fraction",
			                "deltaX"))
		}

	#1.
		if (aon$input_FractionVegetationComposition) {
			temp <- c(temp, paste0("SWinput.Composition.",
			                      c("Grasses", "Shrubs", "Trees", "Forbs", "BareGround",
			                      "C3ofGrasses", "C4ofGrasses", "AnnualsofGrasses"),
			                      "_fraction_const"))
		}
	#2.
		if (aon$input_VegetationBiomassMonthly) {
			temp <- c(temp, paste0(c(rep("Grass", 36), rep("Shrub", 36), rep("Tree", 36), rep("Forb", 36)),
			                      "_",
			                      c(rep("Litter", 12), rep("TotalBiomass", 12), rep("LiveBiomass", 12)),
			                      "_m", st_mo, "_gPERm2"))
		}
	#3.
		if (aon$input_VegetationPeak) {
			temp <- c(temp, paste0("SWinput.PeakLiveBiomass_",
			                        c("month_mean","months_duration")))
		}

	#4.
		if (aon$input_Phenology) {
			temp <- c(temp, paste0("SWinput.GrowingSeason.",
			                      c("Start", "End"),
			                      "_month_const"))
		}
	#5.
		if (aon$input_TranspirationCoeff) {
			if (daily_lyr_agg[["do"]]) {
				ltemp <- paste("L0to", daily_lyr_agg[["first_cm"]], "cm", sep="")
				if (is.null(daily_lyr_agg[["second_cm"]])) {
					ltemp <- c(ltemp, paste("L", daily_lyr_agg[["first_cm"]], "toSoilDepth", sep=""))
				} else if (is.numeric(daily_lyr_agg[["second_cm"]])) {
					ltemp <- c(ltemp, paste("L", daily_lyr_agg[["first_cm"]], "to", daily_lyr_agg[["second_cm"]], "cm", sep=""))
				}
				if (is.null(daily_lyr_agg[["third_cm"]])) {
					ltemp <- c(ltemp, paste("L", daily_lyr_agg[["second_cm"]], "toSoilDepth", sep=""))
				} else if (is.na(daily_lyr_agg[["third_cm"]])) {
				} else if (is.numeric(daily_lyr_agg[["third_cm"]])) {
					ltemp <- c(ltemp, paste("L", daily_lyr_agg[["second_cm"]], "to", daily_lyr_agg[["third_cm"]], "cm", sep=""))
				}
				if (is.null(daily_lyr_agg[["fourth_cm"]])) {
					ltemp <- c(ltemp, paste("L", daily_lyr_agg[["third_cm"]], "toSoilDepth", sep=""))
				} else if (is.na(daily_lyr_agg[["fourth_cm"]])) {
				} else if (is.numeric(daily_lyr_agg[["fourth_cm"]])) {
					ltemp <- c(ltemp, paste("L", daily_lyr_agg[["third_cm"]], "to", daily_lyr_agg[["fourth_cm"]], "cm", sep=""))
				}
				ltemp <- c(ltemp, paste("NA", (length(ltemp)+1):SoilLayer_MaxNo, sep=""))
			} else {
				ltemp <- paste("L", formatC(lmax, width=2, format="d", flag="0"), sep="")
			}

      vtemp <- c("Grass", "Shrub", "Tree", "Forb")
			temp <- c(temp, c(paste0("SWinput.",
                              rep(vtemp, each = SoilLayer_MaxNo),
                              ".TranspirationCoefficients.",
                              rep(ltemp, times = length(vtemp)),
                              "_fraction"),
                        paste0("SWinput.",
                              rep(vtemp, each = 2),
                              ".TranspirationCoefficients.",
                              rep(c("topLayer", "bottomLayer"), times = length(vtemp)),
                              "_fraction")))

		}

	#6.
		if (aon$input_ClimatePerturbations) {
			temp <- c(temp, paste0(rep(paste0("SWinput.ClimatePerturbations.",
			                                c("PrcpMultiplier.m", "TmaxAddand.m", "TminAddand.m")),
			                          each = 12),
			                      st_mo,
			                      rep(c("_none", "_C", "_C"), each = 12),
			                      "_const"))
		}

		##############################################################---Aggregation: Climate and weather---##############################################################

	#7.
		if (aon$yearlyTemp) {
			temp <- c(temp, "MAT_C")
		}

	#8.
		if (aon$yearlyPPT) {
			temp <- c(temp, c("MAP_mm", "SnowOfPPT_fraction"))
		}

	#9.
		if (aon$dailySnowpack) {
			temp <- c(temp, "RainOnSnowOfMAP_fraction")
		}

	#10.
		if (aon$dailySnowpack) {
			temp <- c(temp, paste0("Snowcover.NSadj.",
			                      c("Peak_doy", "LongestContinuous.LastDay_doy",
			                        "LongestContinuous.Duration_days", "Total_days",
			                        "Peak_mmSWE")))
		}
	#11
		if (aon$dailyFrostInSnowfreePeriod) {
			temp <- c(temp, paste0("TminBelow", fieldtag_Tmin_crit_C, "withoutSnowpack_days"))
		}
	#12
		if (aon$dailyHotDays) {
			temp <- c(temp, paste0("TmaxAbove", fieldtag_Tmax_crit_C, "_days"))
		}
	#12b
		if (aon$dailyWarmDays) {
		  temp <- c(temp, paste0("TmeanAbove", fieldtag_Tmean_crit_C, "_days"))
		}
	#13
		if (aon$dailyPrecipitationEventSizeDistribution) {
			bins.summary <- (0:6) * bin.prcpSizes
			temp <- c(temp, paste0("PrcpEvents.Annual",
			                      c("_count",
			                        paste0(".SizeClass", bins.summary, "to",
			                              c(bins.summary[-1], "Inf"),
			                              "mm_fraction"))))
			rm(bins.summary)
		}

	#15
		if (aon$yearlyPET) {
			temp <- c(temp, "PET_mm")
		}

	#16
		if (aon$monthlySeasonalityIndices) {
			temp <- c(temp, paste0("Seasonality.monthly",
                            c("PETandSWPtopLayers", "PETandSWPbottomLayers", "TandPPT"),
                            "_PearsonCor_mean"))
		}


				#---Aggregation: Climatic dryness
	#17
		if (aon$yearlymonthlyTemperateDrylandIndices) {
			temp2 <- c("UNAridityIndex", "TrewarthaD", "TemperateDryland12")
			temp <- c(temp, paste0(c(paste0(temp2, ".Normals"),
                               paste0(temp2, ".Annual")),
                            "_",
                            rep(c("none", "TF", "TF"), times = 2)))
		}

	#18
		if (aon$yearlyDryWetPeriods) {
      temp <- c(temp, paste0("SpellsOfYears_",
                            c("Below", "Above"),
                            "MeanAnnualPrecip_Duration_years"))
		}

	#19
		if (aon$dailyWeatherGeneratorCharacteristics) {
		  temp2 <- c("WetSpellDuration", "DrySpellDuration", "TempAir.StDevOfDailyValues")
			temp <- c(temp, paste0(rep(temp2, each = 12),
			                      ".m", st_mo, "_",
			                      rep(c("days", "days", "C"), each = 12)))
		}

	#20
		if (aon$dailyPrecipitationFreeEventDistribution) {
			bins.summary <- (0:3) * bin.prcpfreeDurations
			temp <- c(temp, paste0("DrySpells.Annual",
			                      c("_count",
			                        paste0(".SizeClass", bins.summary + 1, "to",
			                              c(bins.summary[-1], "365"),
			                              "days_fraction"))))
			rm(bins.summary)
		}

	#21
		if (aon$monthlySPEIEvents) {
      temp <- c(temp, paste0(paste0("SPEI.",
                                    rep(SPEI_tscales_months, each = 4), "monthsScale.",
                                    "Spell", rep(c("Pos.", "Neg."), each = 2)),
                            c("Duration_months", "IntensityValue_none")))
		}

	#---Aggregation: Climatic control
	#22
		if (aon$monthlyPlantGrowthControls) {
			temp <- c(temp, paste0("NemaniEtAl2003.NPPControl.",
			                      c("Temperature", "Water", "Radiation"),
			                      "_fraction"))
		}

	#23
		if (aon$dailyC4_TempVar) {
			temp <- c(temp, paste0("TeeriEtAl1976.NSadj.",
			                      c("TempAirMin.7thMonth_C",
			                        "FreezeFreeGrowingPeriod_days",
			                        "AccumDegreeDaysAbove65F_daysC")))
		}

	#24
		if (aon$dailyDegreeDays) {
			temp <- c(temp, paste0("DegreeDays.Base", DegreeDayBase, "C.dailyTmean_Cdays"))
		}

		##############################################################---Aggregation: Yearly water balance---##############################################################

	#27.0
		if (aon$yearlyAET) {
			temp <- c(temp, "AET_mm")
		}

	#27
		if (aon$yearlyWaterBalanceFluxes) {
			temp <- c(temp,
				c("Rain_mm", "Rain.ReachingSoil_mm", "Snowfall_mm", "Snowmelt_mm", "Snowloss_mm",
					"Interception.Total_mm", "Interception.Vegetation_mm", "Interception.Litter_mm",
					"Evaporation.InterceptedByVegetation_mm", "Evaporation.InterceptedByLitter_mm",
					"Infiltration_mm", "Runoff_mm", "Evaporation.Total_mm",
					"Evaporation.Soil.Total_mm", "Evaporation.Soil.topLayers_mm",
					"Evaporation.Soil.bottomLayers_mm", "Transpiration.Total_mm",
					"Transpiration.topLayers_mm", "Transpiration.bottomLayers_mm",
					"HydraulicRedistribution.TopToBottom_mm", "Percolation.TopToBottom_mm",
					"DeepDrainage_mm", "SWC.StorageChange_mm",
					"TranspirationBottomToTranspirationTotal_fraction", "TtoAET", "EStoAET",
					"AETtoPET", "TtoPET", "EStoPET"))
		}


	#27.2
		if (aon$dailySoilWaterPulseVsStorage) {
			temp <- c(temp,
								paste0("WaterExtractionSpell_MeanContinuousDuration_L", lmax, "_days"),
								paste0("WaterExtractionSpell_AnnualSummedExtraction_L", lmax, "_mm"))
		}

		##############################################################---Aggregation: Daily extreme values---##############################################################
	#28
		if (aon$dailyTranspirationExtremes) {
      temp <- c(temp, paste0("Transpiration.", c("DailyMax", "DailyMin"), "_mm"),
                      paste0("Transpiration.", c("DailyMax", "DailyMin"), "_doy"))
		}

	#29
		if (aon$dailyTotalEvaporationExtremes) {
			temp <- c(temp, paste0("Evaporation.Total.", c("DailyMax", "DailyMin"), "_mm"),
			                paste0("Evaporation.Total.", c("DailyMax", "DailyMin"), "_doy"))
		}

	#30
		if (aon$dailyDrainageExtremes) {
			temp <- c(temp, paste0("DeepDrainage.", c("DailyMax", "DailyMin"), "_mm"),
			                paste0("DeepDrainage.", c("DailyMax", "DailyMin"), "_doy"))
		}

	#31
		if (aon$dailyInfiltrationExtremes) {
			temp <- c(temp, paste0("Infiltration.", c("DailyMax", "DailyMin"), "_mm"),
			                paste0("Infiltration.", c("DailyMax", "DailyMin"), "_doy"))
		}

	#32
		if (aon$dailyAETExtremes) {
			temp <- c(temp, paste0("AET.", c("DailyMax", "DailyMin"), "_mm"),
			                paste0("AET.", c("DailyMax", "DailyMin"), "_doy"))
		}

	#33
		if (aon$dailySWPextremes) {
			temp <- c(temp, paste0("SWP.",
                              rep(c("topLayers.", "bottomLayers."), each = 2),
			                        rep(c("DailyMax", "DailyMin"), times = 2),
			                        rep(c("_MPa", "_doy"), each = 4)))
		}
	#34
		if (aon$dailyRechargeExtremes) {
			temp <- c(temp, paste0("RelRecharge.",
                              rep(c("topLayers.", "bottomLayers."), each = 2),
                              rep(c("DailyMax", "DailyMin"), times = 2),
                              rep(c("_Fraction", "_doy"), each = 4)))
		}

		##############################################################---Aggregation: Ecological dryness---##############################################################

	#35a
  if (aon$dailyNRCS_SoilMoistureTemperatureRegimes) {
      # abbreviations:
      #     - GT = greater than; LT = less than; EQ = equal
      #     - MCS = MoistureControlSection; ACS = AnhydrousControlSection
      #     - consec = consecutive
      temp <- c(temp,
        paste0("NRCS_",
          c("Depth50cmOrImpermeable_cm",
              "MCS_Upper_cm", "MCS_Lower_cm",
              "ACS_Upper_cm", "ACS_Lower_cm",
              "Permafrost_TF",
              "SoilTemp_ACS_Annual_C", "SoilTemp_at50cm_Annual_C",
              "SoilTemp_at50cm_JJA_C", "SoilTemp_at50cm_DJF_C",
              "Saturation_ConsecutiveMaxDuration_JJA_days",
              # Lanh_annual_means:
              "Days_at50cm_GT0C_prob", "Days_ACS_MoreThanHalfDry_prob",
              "Days_ACS_MoreThanHalfDry_and_at50cm_GT0C_prob",
              # Cond_annual_means:
              "Days_at50cm_GT5C_prob", "Days_at50cm_GT8C_prob",
              "Days_MCS_AllWet_prob", "Days_MCS_AllDry_prob",
              "MCS_AllDry_and_at50cm_GT5C_prob", # COND1_Test
              "MCS_AnyWet_and_at50cm_GT5C_prob", # COND1_1_Test
              "MCS_AnyWetConsec_LT90Days_at50cm_GT8C_prob", # COND2
              "MCS_AnyDryTotal_LT90Days_prob", # COND3
              "MCS_at50cm_GT22C_prob", # COND4
              "MCS_at50cm_DiffJJAtoDJF_GT6C_prob", # COND5
              "Days_MCS_AllDry_Summer_days",
              "MCS_AllDry_Summer_LT45Days_prob", # COND6
              "MCS_AnyMoist_GT180Days_prob", # COND7
              "Days_MCS_AnyWetConsec_days",
              "MCS_AnyWetConsec_GT90Days_prob", # COND8
              "Days_MCS_AllWet_Winter_days",
              "MCS_AllWet_Winter_GT45days_prob"), # COND9
              paste0("SoilTemperatureRegime_",
                    c("Hyperthermic", "Thermic", "Mesic", "Frigid", "Cryic", "Gelic")),
              paste0("SoilMoistureRegime_",
                    c("Anhydrous", "Aridic", "Udic", "Ustic", "Xeric"))))
    }
	#35b
    if (aon$dailyNRCS_Chambers2014_ResilienceResistance) {
      cats <- c("Low", "ModeratelyLow", "Moderate", "ModeratelyHigh", "High")
			temp <- c(temp, paste0("NRCS_Chambers2014_Sagebrush",
			                      rep(c("Resilience", "Resistance"), each = length(cats)),
			                      "_", cats))
      rm(cats)
    }

    #35c
    if (aon$dailyNRCS_Maestas2016_ResilienceResistance) {
      temp <- c(temp, paste0("NRCS_Maestas2016_SagebrushRR_", c("Low", "Moderate", "High")))
    }

	#35.2
		if (aon$dailyWetDegreeDays) {
      temp <- c(temp, paste0("WetDegreeDays.SWPcrit",
                            rep(fieldtag_SWPcrit_MPa, times = 3),
                            rep(c(".topLayers", ".bottomLayers", ".anyLayer"),
                                each = length(SWPcrit_MPa)), "_Cdays"))
		}

	#35.3
    if(aon$dailyThermalDrynessStartEnd){
      temp <- c(temp, paste0("ThermalDrySoilPeriods_SWPcrit",
                            rep(fieldtag_SWPcrit_MPa, each = 2),
                            "_NSadj_",
                            rep(c("topLayers", "bottomLayers"),
                                each = length(SWPcrit_MPa) * 2), "_",
                            c("Start", "End"),
                            "_LongestContinuous_days"))
		}

	#35.4
		if (aon$dailyThermalSWPConditionCount) {
		  temp <- c(temp, paste0("SoilPeriods_Warm",
                      rep(paste0(rep(c("Dry", "Wet"), times = 3), "_",
                        rep(c("allLayers", "topLayer", "bottomLayer"), each = 2)),
                        each = length(Tmean_crit_C) * length(SWPcrit_MPa)),
                      "_Tcrit", rep(fieldtag_Tmean_crit_C, times = length(SWPcrit_MPa)),
                      "_SWPcrit", rep(fieldtag_SWPcrit_MPa, each = length(Tmean_crit_C)),
                      "_Count_days"))
		}

	#36
		if (aon$monthlySWPdryness) {
			temp <- c(temp, paste0("DrySoilPeriods.SWPcrit",
                            rep(fieldtag_SWPcrit_MPa, times = 2), ".NSadj.",
                            rep(c("topLayers", "bottomLayers"), each = length(SWPcrit_MPa)),
                            ".Duration.Total_months"),
                      paste0("DrySoilPeriods.SWPcrit",
                            rep(fieldtag_SWPcrit_MPa, times = 2), ".NSadj.",
                            rep(c("topLayers", "bottomLayers"), each = length(SWPcrit_MPa)),
                            ".Start_month"))
		}

	#37
		if (aon$dailySWPdrynessANDwetness) {
			temp <- c(temp, paste0(rep(c("WetSoilPeriods", "DrySoilPeriods"), each = 8),
                            ".SWPcrit",
                            rep(fieldtag_SWPcrit_MPa, each = 16),
                            ".NSadj.",
                            c(rep(c("topLayers", "bottomLayers"), times = 4),
                              rep(rep(c("topLayers", "bottomLayers"), each = 2), times = 2)),
                            rep(c(".AnyLayerWet", ".AllLayersWet", ".AllLayersDry", ""),
                                each = 4),
                            ".",
                            c(rep(rep(c("Duration.Total_days",
                                        "Duration.LongestContinuous_days"), each = 2),
                                  times = 2),
                              rep(c("Duration.Total_days",
                                    "Duration.LongestContinuous_days"), times = 2),
                              paste0("PeriodsFor", fieldtag_drysoils, ".",
                                rep(c("Start_doy", "End_doy"), times = 2)))
                            ))
		}

#TODO(drs): progress state
	#38
		if (aon$dailySuitablePeriodsDuration) {
			quantiles <- c(0.05, 0.5, 0.95)
			temp <- c(temp, paste("ThermalSnowfreeWetPeriods.SWPcrit", rep(paste(rep(fieldtag_SWPcrit_MPa, each=2), rep(c(".topLayers", ".bottomLayers"), times=length(SWPcrit_MPa)), sep=""), each=length(quantiles)), "_Duration_days_quantile", rep(quantiles, times=2), sep=""))
			rm(quantiles)
		}
	#39
		if (aon$dailySuitablePeriodsAvailableWater) {
			temp <- c(temp, paste("ThermalSnowfreeWetPeriods.SWPcrit", rep(fieldtag_SWPcrit_MPa, each=2), rep(c(".topLayers", ".bottomLayers"), times=length(SWPcrit_MPa)), "_AvailableWater_mm_mean", sep=""))
		}
	#40
		if (aon$dailySuitablePeriodsDrySpells) {
			temp <- c(temp, paste0("ThermalSnowfreeDryPeriods.SWPcrit",
			                      rep(paste0(rep(fieldtag_SWPcrit_MPa, each = 2),
			                                rep(c(".topLayers", ".bottomLayers"),
			                                    times=length(SWPcrit_MPa))),
			                          each=4),
			                      "_DrySpells",
			                      c(rep("", 3), fieldtag_drysoils),
			                      "AllLayers_",
			                      c("meanDuration_days", "maxDuration_days", "Total_days",
			                        "Start_doy")))
		}
	#41
		if (aon$dailySWPdrynessDurationDistribution) {
			deciles <- (0:10)*10/100
			quantiles <- (0:4)/4
			mo_seasons <- matrix(data=c(12,1:11), ncol=3, nrow=4, byrow=TRUE)
			season.flag <- c("DJF", "MAM", "JJA", "SON")

			temp <- c(temp, paste0("DrySoilPeriods.SWPcrit",
								rep(rep(fieldtag_SWPcrit_MPa, each = 2 * length(quantiles)), times = length(season.flag)),
								".Month",
								rep(season.flag, each = 2 * length(quantiles) * length(SWPcrit_MPa)), ".",
								rep(rep(paste0(rep(c("topLayers", "bottomLayers"), each = length(quantiles)),
									".Duration_days_quantile",
									rep(quantiles, times = 2)), times = length(SWPcrit_MPa)),
								times = length(season.flag))))

			rm(deciles, quantiles, mo_seasons, season.flag)
		}

	#42
		if (aon$dailySWPdrynessEventSizeDistribution) {
			binSize <- c(1, 8, 15, 29, 57, 183, 367) #closed interval lengths in [days] within a year; NOTE: n_variables is set for binsN == 4
			binsN <- length(binSize) - 1
			binTitle <- paste("SizeClass", paste(binSize[-length(binSize)], binSize[-1]-1, sep="to") ,"days", sep="")

			temp <- c(temp, paste0("DrySoilPeriods.SWPcrit",
								rep(fieldtag_SWPcrit_MPa, each = 2 * (binsN + 1)),
								".Annual.",
								rep(c("topLayers", "bottomLayers"), each = binsN + 1),
								rep(c("_count", paste0(".", binTitle, "_fraction")), times = 2),
								"_mean"))

			rm(binSize, binsN, binTitle)
		}

	#43
		if (aon$dailySWPdrynessIntensity) {
			temp <- c(temp, paste0("DrySoilPeriods.SWPcrit",
								rep(fieldtag_SWPcrit_MPa, each = 4 * 2),
								".MissingWater.",
								rep(c("topLayers", "bottomLayers"), each = 4), ".",
								rep(c("AnnualSum_mmH2O", "PerEventPerDay_mmH2O", "Duration.Event_days", "Events_count"), times = 2),
								"_mean"))
		}

	#43.2
		if (aon$dailyThermalDrynessStress) {
			temp <- c(temp,
						paste0("Mean10HottestDays_VPD_kPa",
							c("_mean", "_max",
							paste0(paste0("_MoistureStress_",
									"SWPcrit", rep(fieldtag_SWPcrit_MPa, times = 3), "_",
									rep(rep(c("allLayers", "topLayer", "bottomLayer"), each = length(SWPcrit_MPa)), each = 2)
								),
								rep(c("_mean", "_max"), each = length(SWPcrit_MPa))))))

		}

		##############################################################---Aggregation: Mean monthly values---##############################################################

	#44
		if (aon$monthlyTemp) {
			temp <- c(temp, paste0("TempAir.m", st_mo, "_C"))
		}

	#45
		if (aon$monthlyPPT) {
			temp <- c(temp, paste0("Precip.m", st_mo, "_mm"))
		}

	#46
		if (aon$monthlySnowpack) {
			temp <- c(temp, paste0("Snowpack.m", st_mo, "_mmSWE"))
		}

	#47
		if (aon$monthlySoilTemp) {
      temp <- c(temp, paste0("TempSoil.",
                            paste0(rep(c("top", "bottom"), each = 12), "Layers.m", st_mo),
                            "_C"))
		}

	#48
		if (aon$monthlyRunoff) {
			temp <- c(temp, paste0("Runoff.Total.m", st_mo, "_mm"))
		}

	#49
		if (aon$monthlyHydraulicRedistribution) {
      temp <- c(temp, paste0("HydraulicRedistribution.",
                            paste0(rep(c("top", "bottom"), each = 12), "Layers.m", st_mo),
                            "_mm"))
		}

	#50
		if (aon$monthlyInfiltration) {
			temp <- c(temp, paste0("Infiltration.m", st_mo, "_mm"))
		}

	#51
		if (aon$monthlyDeepDrainage) {
			temp <- c(temp, paste0("DeepDrainage.m", st_mo, "_mm"))
		}

	#52
		if (aon$monthlySWPmatric) {
      temp <- c(temp, paste0("SWPmatric.",
                            paste0(rep(c("top", "bottom"), each = 12), "Layers.m", st_mo),
                            "_MPa_FromVWCmean"))
		}

	#53 a.)
		if (aon$monthlyVWCbulk) {
      temp <- c(temp, paste0("VWCbulk.",
                            paste0(rep(c("top", "bottom"), each = 12), "Layers.m", st_mo),
                            "_mPERm"))
		}
	#53 b.)
		if (aon$monthlyVWCmatric) {
      temp <- c(temp, paste0("VWCmatric.",
                            paste0(rep(c("top", "bottom"), each = 12), "Layers.m", st_mo),
                            "_mPERm"))
		}

	#54
		if (aon$monthlySWCbulk) {
      temp <- c(temp, paste0("SWCbulk.",
                            paste0(rep(c("top", "bottom"), each = 12), "Layers.m", st_mo),
                            "_mm"))
		}

	#55
		if (aon$monthlySWAbulk) {
      temp <- c(temp, paste0("SWAbulk_",
                            "SWPcrit", rep(fieldtag_SWPcrit_MPa, each = 24), "_",
                            paste0(rep(c("top", "bottom"), each = 12), "Layers.m", st_mo),
                            "_mm"))
		}

	#56
		if (aon$monthlyTranspiration) {
      temp <- c(temp, paste0("Transpiration.",
                            paste0(rep(c("top", "bottom"), each = 12), "Layers.m", st_mo),
                            "_mm"))
		}

	#57
		if (aon$monthlySoilEvaporation) {
			temp <- c(temp, paste0("Evaporation.Soil.m", st_mo, "_mm"))
		}

	#58
		if (aon$monthlyAET) {
			temp <- c(temp, paste0("AET.m", st_mo, "_mm"))
		}

	#59
		if (aon$monthlyPET) {
			temp <- c(temp, paste0("PET.m", st_mo, "_mm"))
		}

	#59.2
		if (aon$monthlyVPD) {
			temp <- c(temp, paste0("VPD_m", st_mo, "_kPa"))
		}

	#60
		if (aon$monthlyAETratios) {
      temp <- c(temp, paste0(rep(c("TranspToAET.m", "EvapSoilToAET.m"), each = 12),
                              st_mo, "_fraction"))
		}

	#61
		if (aon$monthlyPETratios) {
      temp <- c(temp, paste0(rep(c("TranspToPET.m", "EvapSoilToPET.m"), each = 12),
                              st_mo, "_fraction"))
		}

		##############################################################---Aggregation: Potential regeneration---##############################################################

	#62
		if (aon$dailyRegeneration_bySWPSnow) {
			temp <- c(temp, "Regeneration.Potential.SuitableYears.NSadj_fraction")
		}

	#63
		if (aon$dailyRegeneration_GISSM & no.species_regeneration > 0) {
			for (sp in 1:no.species_regeneration) {
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

		#---Aggregation: done with options

		#---Overall aggregation table
		#Convert '.' to "_"
		temp <- gsub(".", "_", temp, fixed=TRUE)
		dbOverallColumns <- length(temp)

		if (dbOverallColumns > 0) {
			temp <- paste(paste0("\"", temp, "\""), "REAL", collapse = ", ")

      overallSQL <- paste0("CREATE TABLE \"aggregation_overall\" (",
                      paste(c("\"P_id\" INTEGER",
                            "\"aggfun_id\" INTEGER",
                            "\"aggwindow_id\" INTEGER",
                            temp,
                            "PRIMARY KEY (\"P_id\", \"aggfun_id\",  \"aggwindow_id\")"),
                          collapse = ", "),
                      ");")

      rs <- RSQLite::dbGetQuery(con, overallSQL)
    }

		#---Daily aggregation table(s)
		if (!is.null(output_aggregate_daily)) {
			doy_colnames <- paste0("doy", formatC(seq_len(366), width = 3, format = "d", flag = "0"))
			doy_colnames <- paste(paste0("\"", doy_colnames, "\""), "REAL", collapse = ", ")

			dailySQL <- paste(c("\"P_id\" INTEGER",
													"\"aggfun_id\" INTEGER",
													"\"aggwindow_id\" INTEGER",
													doy_colnames,
													"PRIMARY KEY (\"P_id\", \"aggfun_id\",  \"aggwindow_id\")"),
												collapse = ", ")
			dailyLayersSQL <- paste(c("\"P_id\" INTEGER",
													"\"aggfun_id\" INTEGER",
													"\"aggwindow_id\" INTEGER",
													"\"Soil_Layer\" INTEGER",
													doy_colnames,
													"PRIMARY KEY (\"P_id\", \"aggfun_id\",  \"aggwindow_id\", \"Soil_Layer\")"),
												collapse = ", ")

			if (daily_no > 0) {
				for (doi in seq_len(daily_no)) {
					if (regexpr("SWAbulk", output_aggregate_daily[doi]) > 0) {
						agg.resp <- "SWAbulk"
						#index.SWPcrit <- -as.numeric(sub("kPa", "", sub("SWAatSWPcrit", "", output_aggregate_daily[doi])))/1000
					} else {
						agg.resp <- output_aggregate_daily[doi]
					}

          def_dailySQL <- paste0("CREATE TABLE \"",
            paste0("aggregation_doy_", output_aggregate_daily[doi]),
            " (",
            if (agg.resp %in% c("Transpiration", "SoilTemperature", "VWCbulk",
                                "VWCmatric", "SWCbulk", "SWPmatric", "SWAbulk")) {
              dailyLayersSQL
            } else {
              dailySQL
            },
            ");")

          rs <- RSQLite::dbGetQuery(con, def_dailySQL)
				}
			}
		}

		RSQLite::dbDisconnect(con)

		dbOverallColumns
	}

	dbOverallColumns <- try(.local(), silent=FALSE)

	if (inherits(dbOverallColumns, "try-error")) {
		temp <- list.files(dir.out, pattern = ".sqlite3", full.names = TRUE)
		temp <- lapply(temp, unlink)
		stop(paste("Creation of databases failed:", dbOverallColumns, collapse = ", "))
	}

} else {
	dbOverallColumns <- length(dbListFields(con, "aggregation_overall")) - 3L # c("P_id", "aggfun_id",  "aggwindow_id")
}


rm(Tables)
