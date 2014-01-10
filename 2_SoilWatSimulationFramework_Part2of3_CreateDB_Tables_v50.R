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
#A. Header Tables
	
	if(!exinfo$ExtractGriddedDailyWeatherFromMaurer2002_NorthAmerica && any(is.na(SWRunInformation$WeatherFolder[seq.tr])) && !any(create_treatments=="LookupWeatherFolder")) stop("No WeatherData For Runs")
	
	####FUNCTIONS CONSIDER MOVING####
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
	columnType <- function(columnName) {
		if(columnName %in% create_experimentals) {
			mapType(typeof(sw_input_experimentals[,columnName]))
		} else if(columnName %in% create_treatments[!(create_treatments %in% create_experimentals)]) {
			mapType(typeof(sw_input_treatments[,columnName]))
		}
	}
	######################
	
	dbGetQuery(con, "CREATE TABLE weatherfolders(id INTEGER PRIMARY KEY AUTOINCREMENT, folder TEXT UNIQUE NOT NULL);")
	dbBeginTransaction(con)
	if(all(!is.na(SWRunInformation$WeatherFolder[seq.tr]))) {
		dbGetPreparedQuery(con, "INSERT INTO weatherfolders VALUES(NULL, :folder)", bind.data = data.frame(folder=SWRunInformation$WeatherFolder[seq.tr],stringsAsFactors=FALSE))
	} else {
		if(!exinfo$ExtractGriddedDailyWeatherFromMaurer2002_NorthAmerica && !any(create_treatments=="LookupWeatherFolder")) stop("Weather Data in Master has NA's.") 
	}
	dbCommit(con)
	
	#############Site Table############################
	#This returns the SQLite Types of the columns
	site_col_types <-sapply(X=1:ncol(SWRunInformation), function(x) mapType(typeof(SWRunInformation[[x]])))
	site_columns <- colnames(SWRunInformation)
	site_columns <- sub(pattern="WeatherFolder",replacement="WeatherFolder_id",site_columns)
	site_col_types[which(colnames(SWRunInformation) == "WeatherFolder")] <- "INTEGER"
	dbGetQuery(con, paste("CREATE TABLE sites(id INTEGER PRIMARY KEY AUTOINCREMENT,", paste(site_columns, site_col_types, collapse=","), ", FOREIGN KEY(WeatherFolder_id) REFERENCES weatherfolders(id));", sep=""))
	
	
	sites_data<-data.frame(SWRunInformation[seq.tr,],row.names=NULL, check.rows=FALSE, check.names=FALSE, stringsAsFactors=FALSE)
	
	colnames(sites_data) <- site_columns
	sites_data$WeatherFolder_id <- if(all(!is.na(SWRunInformation$WeatherFolder[seq.tr]))) 1:length(sites_data$site_id) else NA
	#This works fast
	dbBeginTransaction(con)
	dbGetPreparedQuery(con, paste("INSERT INTO sites VALUES(NULL,",paste(":",site_columns,collapse=",",sep=""),")",sep=""), bind.data=sites_data)
	dbCommit(con)
	
	rm(site_col_types,site_columns,sites_data)
	
	useExperimentals <- trowExperimentals > 0 && length(create_experimentals) > 0
	useTreatments <- !(length(create_treatments[!(create_treatments %in% create_experimentals)])==0)
	
	#############simulation_years table#########################
	dbGetQuery(con, "CREATE TABLE simulation_years(id INTEGER PRIMARY KEY AUTOINCREMENT, simulationStartYear INTEGER NOT NULL, StartYear INTEGER NOT NULL, EndYear INTEGER NOT NULL);")
	##################################################
	
	
	##########Create table experimental_labels only if using experimentals
	if(useExperimentals) {
		dbGetQuery(con, "CREATE TABLE experimental_labels(id INTEGER PRIMARY KEY AUTOINCREMENT, label TEXT UNIQUE NOT NULL);")
		dbBeginTransaction(con)
		dbGetPreparedQuery(con, "INSERT INTO experimental_labels VALUES(NULL, :label);", bind.data = data.frame(label=sw_input_experimentals[,1],stringsAsFactors=FALSE))
		dbCommit(con)
	}
	################################
	
	#If LookupWeatherFolder is ON we need to make sure all of the weather folders are in weatherfolders table
	if(any(create_treatments=="LookupWeatherFolder")) {
		#which ones are not in SWRunInformation$WeatherFolder
		
		#make a combined list of experimentals and treatments LookupWeatherFolder List
		#first add any from the experimentals table if its turned on
		#next add any from the treatments table if its turned on
		treatments_lookupweatherfolders <- character(0)
		if(any(names(sw_input_treatments_use[-1][which(sw_input_treatments_use[-1] > 0 & is.finite(as.numeric(sw_input_treatments_use[-1])))])=="LookupWeatherFolder")) {
			treatments_lookupweatherfolders <- c(treatments_lookupweatherfolders, sw_input_treatments$LookupWeatherFolder[seq.tr])
		}
		if(any(create_experimentals=="LookupWeatherFolder")) {
			treatments_lookupweatherfolders <- c(treatments_lookupweatherfolders, sw_input_experimentals$LookupWeatherFolder[seq.tr])
		}
		#Remove NA because that defaults to sites default weatherFolder also make sure each folder is unique
		treatments_lookupweatherfolders <- treatments_lookupweatherfolders[!is.na(treatments_lookupweatherfolders)]
		treatments_lookupweatherfolders <- unique(treatments_lookupweatherfolders)
		if(length(treatments_lookupweatherfolders) == 0){
			print("LookupWeatherFolder is turned on in treatments or experimentals or both but is not used")
		} else {
			#make a temp data.frame of a column NA's and a column of folder names
			LookupWeatherFolder_index <- data.frame(id=rep(NA,length(treatments_lookupweatherfolders)), folder=treatments_lookupweatherfolders, stringsAsFactors = F)
			#Get the id from sites table if the folder is in it
			temp <- LookupWeatherFolder_index$folder %in% SWRunInformation$WeatherFolder[seq.tr]
			LookupWeatherFolder_index$id[temp] <- sapply(which(temp), function(x) which(SWRunInformation$WeatherFolder[seq.tr] == LookupWeatherFolder_index$folder[x]))
			#if there are any NA's we need to add those to the weatherfolder db table and update its id in our lookuptable for weatherfolder
			if(any(is.na(LookupWeatherFolder_index$id))) {
				#get max id from weatherfolders table
				weatherfolders_index <- as.numeric(dbGetQuery(con,"SELECT MAX(id) FROM weatherfolders;"))+1
				LookupWeatherFolder_index$id[!temp] <- weatherfolders_index:(weatherfolders_index+length(LookupWeatherFolder_index$id[!temp])-1)
				#Write those in
				dbBeginTransaction(con)
				dbGetPreparedQuery(con, "INSERT INTO weatherfolders VALUES(:id,:folder)", bind.data = LookupWeatherFolder_index[!temp,])
				dbCommit(con)
			}
		}
	}
	
	# get unique rows from both treatments and experimentals
	if(useExperimentals) {#Only use experimentals if there is something in it
		#Are all the columns NA
		if(all(temp<-is.na(sw_input_experimentals[,create_experimentals]))) stop("All Columns in experimentals table are NA")
		if(any(apply(temp,MARGIN=2, function(x) all(x)))) warning("One ore more columns in experimentals table are turned on with no value.")
		db_experimentals <- unique(sw_input_experimentals[,create_experimentals])
		#note experimentals should be unique if we have less rows then the original then lets throw an Error
		stopifnot(nrow(db_experimentals) == nrow(sw_input_experimentals))
	} else {
		#experimentals does not have any rows. Are any of the create_experimentals turned on
		if(length(create_experimentals) > 0 && trowExperimentals == 0) stop("No rows in experimentals table but columns are turned on")
		if(trowExperimentals > 0 && length(create_experimentals)==0) warning("Rows in experimentals are not being used.")
	}
	
	if(useTreatments) {
		# we only need the columns that are turned on and not in experimentals. Experimentals over write. Only rows that are going to be used
		db_treatments <- unique(df<-sw_input_treatments[seq.tr,create_treatments[!(create_treatments %in% create_experimentals)]])
		db_treatments_rows <- nrow(db_treatments)
		#this maps locations from reduced
		temp<-duplicated(df)
		treatments_unique_map<-rep(NA,length(df))
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
	if(any(create_treatments=="LookupWeatherFolder")) {
		if(any(create_experimentals=="LookupWeatherFolder")) {
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
	if(useExperimentals | useTreatments) {
		#Create a table to hold the values going into the database
		temp_numberRows <- ifelse(useExperimentals,nrow(db_experimentals)*db_treatments_rows,nrow(db_treatments))
		temp_numberColumns <- ifelse(useExperimentals,3,2)+length(create_treatments)
		temp_columnNames <- c("id",if(useExperimentals) c("experimental_id"),"simulation_years_id",create_treatments)
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
		db_treatments_column_types[,2] <- sapply(db_treatments_column_types[,1], function(x) columnType(x))
		
		#Finalize db_treatments_column_types
		#remove YearStart or YearEnd
		if(any(db_treatments_column_types$column == "YearStart")) {
			db_treatments_column_types <- db_treatments_column_types[-which(db_treatments_column_types$column == "YearStart"),]
		}
		if(any(db_treatments_column_types$column == "YearEnd")) {
			db_treatments_column_types <- db_treatments_column_types[-which(db_treatments_column_types$column == "YearEnd"),]
		}
		
		#rename weather folder column name and create the fk
		fk_LookupWeatherFolder <- ""
		useTreatmentWeatherFolder <- FALSE
		if(any(create_treatments=="LookupWeatherFolder")) {
			useTreatmentWeatherFolder <- TRUE
			db_treatments_column_types[which(db_treatments_column_types[,1] == "LookupWeatherFolder"),1:2] <- c("LookupWeatherFolder_id","INTEGER")
			colnames(db_combined_exp_treatments)[-(1:2)] <- db_treatments_column_types[,1]
			fk_LookupWeatherFolder <- ", FOREIGN KEY(LookupWeatherFolder_id) REFERENCES weatherfolders(id)"
		}
		#Create the table
		dbGetQuery(con, paste("CREATE TABLE treatments(id INTEGER PRIMARY KEY AUTOINCREMENT, ",if(useExperimentals) "experimental_id INTEGER,", " simulation_years_id INTEGER, ", paste(db_treatments_column_types[,1], " ", db_treatments_column_types[,2], sep="", collapse =", "), ", ", if(useExperimentals) "FOREIGN KEY(experimental_id) REFERENCES experimental_labels(id)",fk_LookupWeatherFolder,");", sep=""))
		
		#Lets put in the treatments into combined. This will repeat the reduced rows of treatments into combined
		if(useTreatments) {
			db_combined_exp_treatments[,db_treatments_column_types[db_treatments_column_types[,3]==0,1]] <- db_treatments
		}
		
		if(useExperimentals) {
			exp_start_rows<-seq(from=1,to=db_treatments_rows*nrow(db_experimentals),by=db_treatments_rows)
			#Insert data into our new data.frame
			for(start in exp_start_rows) {
				#Get experimental_label_id
				db_combined_exp_treatments[start:(start+db_treatments_rows-1),2] <- which(exp_start_rows==start)
				#insert all of the rows from experimentals
				db_combined_exp_treatments[start:(start+db_treatments_rows-1),db_treatments_column_types[db_treatments_column_types[,3]==1,1]] <- db_experimentals[which(exp_start_rows==start),]
			}
		}
	} else {
		db_combined_exp_treatments <- data.frame(matrix(data=c(1,1), nrow=1, ncol=2,dimnames=list(NULL, c("id","simulation_years_id"))),stringsAsFactors = FALSE)
		dbGetQuery(con, paste("CREATE TABLE treatments(id INTEGER PRIMARY KEY AUTOINCREMENT, simulation_years_id INTEGER);", sep=""))
	}
	
	#if the column startYear or endYear are present move over to simulation_years
	if(any(colnames(db_combined_exp_treatments) == "YearStart") || any(colnames(db_combined_exp_treatments) == "YearEnd")) {
		simulation_years<-matrix(data=NA, nrow=nrow(db_combined_exp_treatments), ncol = 4, dimnames=list(NULL,c("id","simulationStartYear","StartYear","EndYear")))
		#Get from treatments or get from settings
		if(any(colnames(db_combined_exp_treatments) == "YearStart")) {
			simulation_years$simulationStartYear <- db_combined_exp_treatments$YearStart
			db_combined_exp_treatments <- db_combined_exp_treatments[,-which(colnames(db_combined_exp_treatments) == "YearStart")]
		} else {
			simulation_years$simulationStartYear <- simstartyr
		}
		if(any(colnames(db_combined_exp_treatments) == "YearEnd")) {
			simulation_years$EndYear <- db_combined_exp_treatments$YearEnd
			db_combined_exp_treatments <- db_combined_exp_treatments[,-which(colnames(db_combined_exp_treatments) == "YearEnd")]
		} else {
			simulation_years$EndYear <- endyr
		}
		simulation_years$StartYear <- getStartYear(simulation_years$simulationStartYear)
		
		unique_simulation_years <- unique(simulation_years)
		#each row is unique so add id to db_combined
		if(nrow(unique_simulation_years)==nrow(simulation_years)) {
			unique_simulation_years$id <- 1:nrow(unique_simulation_years)
			db_combined_exp_treatments$simulation_years_id <- unique_simulation_years$id
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
		dbBeginTransaction(con)
		dbGetPreparedQuery(con, "INSERT INTO simulation_years VALUES(NULL, :simulationStartYear, :StartYear, :EndYear);", bind.data = data.frame(unique_simulation_years))
		dbCommit(con)
	} else {#Treatment option for simulation Years is turned off. Get the default one from settings.
		db_combined_exp_treatments$simulation_years_id <- 1
		dbBeginTransaction(con)
		dbGetPreparedQuery(con, "INSERT INTO simulation_years VALUES(NULL, :simulationStartYear, :StartYear, :EndYear);", bind.data = data.frame(simulationStartYear=simstartyr, StartYear=startyr, EndYear=endyr))
		dbCommit(con)
	}
	
	#Insert the data into the treatments table
	dbBeginTransaction(con)
	dbGetPreparedQuery(con, paste("INSERT INTO treatments VALUES(",paste(":",colnames(db_combined_exp_treatments),sep="",collapse=", "),")",sep=""), bind.data = db_combined_exp_treatments)
	dbCommit(con)
	
	##############scenario_labels table###############
	dbGetQuery(con, "CREATE TABLE scenario_labels(id INTEGER PRIMARY KEY AUTOINCREMENT, label TEXT UNIQUE NOT NULL);")
	dbBeginTransaction(con)
	dbGetPreparedQuery(con, "INSERT INTO scenario_labels VALUES(NULL, :label);", bind.data = data.frame(label=climate.conditions,stringsAsFactors = FALSE))
	dbCommit(con)
	##################################################
	
	#############run_labels table#########################
	dbGetQuery(con, "CREATE TABLE run_labels(id INTEGER PRIMARY KEY AUTOINCREMENT, label TEXT UNIQUE NOT NULL);")
	dbBeginTransaction(con)
	if(useExperimentals) {
		dbGetPreparedQuery(con, "INSERT INTO run_labels VALUES(NULL, :label);", bind.data = data.frame(label=paste(formatC(seq.todo, width=ceiling(log10(runsN.todo + 1)), format = "d", flag="0"), rep(sw_input_experimentals[,1],each=runs), labels[seq.tr], sep="_"),stringsAsFactors = FALSE))
	} else {
		dbGetPreparedQuery(con, "INSERT INTO run_labels VALUES(NULL, :label);", bind.data = data.frame(label=labels[seq.tr],stringsAsFactors = FALSE))
	}
	dbCommit(con)
	##################################################
	
	
	#####################runs table###################
	dbGetQuery(con, "CREATE TABLE runs(P_id INTEGER PRIMARY KEY AUTOINCREMENT, label_id INTEGER NOT NULL, site_id INTEGER NOT NULL, treatment_id INTEGER NOT NULL, scenario_id INTEGER NOT NULL, FOREIGN KEY(label_id) REFERENCES run_labels(id), FOREIGN KEY(site_id) REFERENCES sites(id), FOREIGN KEY(treatment_id) REFERENCES treatments(id), FOREIGN KEY(scenario_id) REFERENCES scenario_labels(id));")
	db_runs <- data.frame(matrix(data=0, nrow=runsN.todo*scenario_No, ncol=5,dimnames=list(NULL,c("P_id","label_id","site_id","treatment_id","scenario_id"))))
	
	db_runs$P_id <- 1:nrow(db_runs)
	db_runs$scenario_id <- rep(1:scenario_No, times=runsN.todo)
	
	if(useTreatments) {
		if(useExperimentals) {
			db_runs$label_id <- rep(seq.todo,each=scenario_No)
			db_runs$site_id <- rep(rep(1:runs,times=trowExperimentals),each=scenario_No)
			i_exp<-as.vector(matrix(data=exp_start_rows,nrow=runs,ncol=trowExperimentals,byrow=T))
			db_runs$treatment_id <- rep(i_exp+(treatments_unique_map-1),each=scenario_No)
		} else {
			db_runs$label_id <- rep(seq.todo,each=scenario_No)
			db_runs$site_id <- rep(rep(1:runs,times=trowExperimentals),each=scenario_No)
			db_runs$treatment_id <- rep(treatments_unique_map,each=scenario_No)
		}
	} else {
		if(useExperimentals) {
			db_runs$label_id <- rep(seq.todo,each=scenario_No)
			db_runs$site_id <- rep(rep(1:runs,times=trowExperimentals),each=scenario_No)
			i_exp<-as.vector(matrix(data=exp_start_rows,nrow=runs,ncol=trowExperimentals,byrow=T))
			db_runs$treatment_id <- rep(i_exp,each=scenario_No)
		} else {
			db_runs$label_id <- rep(seq.todo,each=scenario_No)
			db_runs$site_id <- rep(1:runs,each=scenario_No)
			db_runs$treatment_id <- 1
		}
	}
	#dbWriteTable(con,name="runs",db_runs,append=TRUE)
	dbGetPreparedQuery(con, "INSERT INTO runs VALUES(NULL, :label_id, :site_id, :treatment_id, :scenario_id);", bind.data=db_runs)
	
	##################################################
	
	################CREATE VIEW########################
	sites_columns <- colnames(SWRunInformation[Index_RunInformation])
	sites_columns <- sub("ID","site_id AS ID",sites_columns)
	if(length(icol <- grep(pattern="WeatherFolder", sites_columns)) > 0) sites_columns <- sites_columns[-icol]
	treatment_columns <- colnames(db_combined_exp_treatments)[-(1:3)]
	if(useTreatmentWeatherFolder) treatment_columns <- treatment_columns[-grep(pattern="WeatherFolder",treatment_columns)]
	header_columns<-c("runs.P_id","run_labels.label AS Labels", paste("sites",sites_columns,sep=".",collapse = ", "), if(useExperimentals) "experimental_labels.label AS Experimental_Label",if(!exinfo$ExtractGriddedDailyWeatherFromMaurer2002_NorthAmerica) "weatherfolders.folder AS WeatherFolder", if(useExperimentals | useTreatments) paste("treatments",treatment_columns, sep=".", collapse=", "), "simulation_years.StartYear", "simulation_years.simulationStartYear AS SimStartYear", "simulation_years.EndYear", "scenario_labels.label AS Scenario")
	header_columns<-paste(header_columns,collapse = ", ")
	
	dbGetQuery(con, paste("CREATE VIEW header AS SELECT ",header_columns, " FROM runs, run_labels, sites, ", if(useExperimentals) "experimental_labels, ","treatments, scenario_labels, simulation_years, weatherfolders WHERE runs.label_id=run_labels.id AND runs.site_id=sites.id AND runs.treatment_id=treatments.id AND runs.scenario_id=scenario_labels.id AND ",if(!exinfo$ExtractGriddedDailyWeatherFromMaurer2002_NorthAmerica) { if(useTreatmentWeatherFolder) "treatments.LookupWeatherFolder_id=weatherfolders.id AND " else "sites.WeatherFolder_id=weatherfolders.id AND " }, if(useExperimentals) "treatments.experimental_id=experimental_labels.id AND ","treatments.simulation_years_id=simulation_years.id;",sep=""))
	##################################################
	
#B. Aggregation_Overall
	
	##############################################################---Aggregation: SoilWat inputs---##############################################################
#0.
	if(aon$input_SoilProfile){
		temp <- paste("SWinput.Soil.", c("maxDepth_cm", "soilLayers_N", "topLayers.Sand_fraction", "bottomLayers.Sand_fraction", "topLayers.Clay_fraction", "bottomLayers.Clay_fraction"), sep="")
	}

#1. 
	if(aon$input_FractionVegetationComposition) {
		temp <- c(temp, paste("SWinput.Composition.", c("Grasses", "Shrubs", "Trees", "C3ofGrasses", "C4ofGrasses", "AnnualsofGrasses"), "_fraction_const", sep=""))
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
#5.
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
			ltemp <- c(ltemp, paste("NA", (length(ltemp)+1):SoilLayer_MaxNo, sep=""))
		} else {
			ltemp <- paste("L", formatC(lmax, width=2, format="d", flag="0"), sep="")
		}

		temp <- c(temp, c(paste("SWinput.", rep(vtemp <- c("Grass", "Shrub", "Tree"), each=SoilLayer_MaxNo), ".TranspirationCoefficients.", rep(ltemp, times=3), "_fraction", sep=""), paste("SWinput.", rep(vtemp, each=2), ".TranspirationCoefficients.", rep(c("topLayer", "bottomLayer"), times=3), "_fraction", sep="")))

	}
	
#6.
	if(aon$input_ClimatePerturbations) {
		temp <- c(temp, paste(rep(paste("SWinput.ClimatePerturbations.", c("PrcpMultiplier.m", "TmaxAddand.m", "TminAddand.m"), sep=""), each=12), st_mo, rep(c("_none", "_C", "_C"), each=12), "_const", sep=""))
	}
	
	##############################################################---Aggregation: Climate and weather---##############################################################
	
#7.
	if(any(simulation_timescales=="yearly") & aon$yearlyTemp){
		temp <- c(temp, "MAT_C_mean")
	}
	
#8.
	if(any(simulation_timescales=="yearly") & aon$yearlyPPT){
		temp <- c(temp, c("MAP_mm_mean", "SnowOfPPT_fraction_mean"))
	}
	
#9.
	if(any(simulation_timescales=="daily") & any(simulation_timescales=="yearly") & aon$dailySnowpack){
		temp <- c(temp, "RainOnSnowOfMAP_fraction_mean")
	}
	
#10.
	if(any(simulation_timescales=="daily") & aon$dailySnowpack){
		temp <- c(temp, paste("Snowcover.NSadj.", c("Peak_doy", "LongestContinuous.LastDay_doy", "Peak_mmSWE", "LongestContinuous.Duration_days", "Total_days"), "_mean", sep=""))
	}
#
	if(any(simulation_timescales=="daily") & aon$dailyFrostInSnowfreePeriod){			
		temp <- c(temp, "FreezingWithoutSnowpack_days_mean")
	}
#11
	if(any(simulation_timescales=="daily") & aon$dailyPrecipitationEventSizeDistribution){
		bins.summary <- (0:6) * bin.prcpSizes
		temp <- c(temp, paste("PrcpEvents.Annual", c("_count", paste(".SizeClass", bins.summary, "to", c(bins.summary[-1], "Inf"), "mm_fraction", sep="")), "_mean", sep=""))
		rm(bins.summary)
	}
	
#12
	if(any(simulation_timescales=="yearly") & aon$yearlyAET){
		temp <- c(temp, "AET_mm_mean")
	}
	
#13
	if(any(simulation_timescales=="yearly") & aon$yearlyPET){
		temp <- c(temp, "PET_mm_mean")
	}
	
#14
	if(any(simulation_timescales=="monthly") & aon$monthlySeasonalityIndices){
		temp <- c(temp, paste("Seasonality.monthly", c("PETandSWPtopLayers", "PETandSWPbottomLayers", "TandPPT"), "_PearsonCor_mean", sep=""))
	}
	
#15
	if(any(simulation_timescales=="yearly") & any(simulation_timescales=="monthly") & aon$yearlymonthlyTemperateDrylandIndices){
		temp <- c(temp, paste(c(paste(temp <- c("UNAridityIndex", "TrewarthaD", "TemperateDryland12"), ".Normals", sep=""), paste(temp, ".Annual", sep="")), rep(c("_none", "_TF", "_TF"), times=2), "_mean", sep=""))
	}
	
#16
	if(any(simulation_timescales=="yearly") & aon$yearlyDryWetPeriods){
		temp <- c(temp, paste(c("Dry", "Wet"), "SpellDuration.90PercentEvents.ShorterThan_years_quantile0.9", sep=""))
	}
	
#17
	if(any(simulation_timescales=="daily") & aon$dailyWeatherGeneratorCharacteristics){
		temp <- c(temp, paste(rep(c("WetSpellDuration", "DrySpellDuration", "TempAir.StDevOfDailyValues"), each=12), ".m", st_mo, rep(c("_days", "_days", "_C"), each=12), "_mean", sep=""))
	}
	
#18
	if(any(simulation_timescales=="daily") & aon$dailyPrecipitationFreeEventDistribution){
		bins.summary <- (0:3) * bin.prcpfreeDurations
		temp <- c(temp, paste("DrySpells.Annual", c("_count", paste(".SizeClass", bins.summary+1, "to", c(bins.summary[-1], "365"), "days_fraction", sep="")), "_mean", sep=""))
		rm(bins.summary)
	}
	
#19
	if(any(simulation_timescales=="monthly") & aon$monthlySPEIEvents){
		binSPEI_m <- c(1, 12, 24, 48) #months
		probs <- c(0.025, 0.5, 0.975)
		for(iscale in seq_along(binSPEI_m)) {
			rvec <- rep(NA, times=4 * length(probs))
			temp <- c(temp, paste(rep(paste("SPEI.", binSPEI_m[iscale], "monthsScale.", sep=""), length(rvec)), "Spell", rep(c("Pos.", "Neg."), each=2*length(probs)), rep(rep(c("Duration_months", "Value_none"), each=length(probs)), times=2), "_quantile", rep(probs, times=4), sep=""))
			
		}
		rm(binSPEI_m, probs)
	}
	
#20
	if(any(simulation_timescales=="monthly") & aon$monthlyPlantGrowthControls){
		temp <- c(temp, paste("NemaniEtAl2003.NPPControl.", c("Temperature", "Water", "Radiation"), "_none_mean", sep=""))
	}
	
#21
	if(any(simulation_timescales=="daily") & aon$dailyC4_TempVar){
		temp <- c(temp, paste("TeeriEtAl1976.NSadj.", c("TempAirMin.7thMonth_C", "FreezeFreeGrowingPeriod_days", "AccumDegreeDaysAbove65F_daysC"), "_mean", sep=""))
	}
	
#22
	if(any(simulation_timescales=="daily") & aon$dailyDegreeDays){
		temp <- c(temp, paste("DegreeDays.Base", DegreeDayBase, "C.dailyTmean_Cdays_mean", sep=""))
	}
	
	##############################################################---Aggregation: Yearly water balance---##############################################################
	
#23
	if(any(simulation_timescales=="yearly") & aon$yearlyWaterBalanceFluxes) {
		temp <- c(temp, paste(c("Rain_mm", "Rain.ReachingSoil_mm", "Snowfall_mm", "Snowmelt_mm", "Snowloss_mm", "Interception.Total_mm", "Interception.Vegetation_mm", "Interception.Litter_mm", "Evaporation.InterceptedByVegetation_mm", "Evaporation.InterceptedByLitter_mm", "Infiltration_mm", "Runoff_mm", "Evaporation.Total_mm", "Evaporation.Soil.Total_mm", "Evaporation.Soil.topLayers_mm",
								"Evaporation.Soil.bottomLayers_mm", "Transpiration.Total_mm", "Transpiration.topLayers_mm", "Transpiration.bottomLayers_mm", "HydraulicRedistribution.TopToBottom_mm", "Percolation.TopToBottom_mm", "DeepDrainage_mm", "SWC.StorageChange_mm", "TranspirationBottomToTranspirationTotal_fraction", "TtoAET", "EStoAET", "AETtoPET", "TtoPET", "EStoPET"), "_mean", sep=""))
	}
	
	##############################################################---Aggregation: Daily extreme values---##############################################################
	
#24
	if(any(simulation_timescales=="daily") & aon$dailyTranspirationExtremes) {
		temp <- c(temp, paste("Transpiration.", c("DailyMax", "DailyMin"), "_mm_mean", sep=""), paste("Transpiration.", c("DailyMax", "DailyMin"), "_doy_mean", sep=""))
	}
	
#25
	if(any(simulation_timescales=="daily") & aon$dailyTotalEvaporationExtremes) {
		temp <- c(temp, paste("Evaporation.Total.", c("DailyMax", "DailyMin"), "_mm_mean", sep=""), paste("Evaporation.Total.", c("DailyMax", "DailyMin"), "_doy_mean", sep=""))
	}
	
#26
	if(any(simulation_timescales=="daily") & aon$dailyDrainageExtremes) {
		temp <- c(temp, paste("DeepDrainage.", c("DailyMax", "DailyMin"), "_mm_mean", sep=""), paste("DeepDrainage.", c("DailyMax", "DailyMin"), "_doy_mean", sep=""))
	}
	
#27
	if(any(simulation_timescales=="daily") & aon$dailyInfiltrationExtremes) {
		temp <- c(temp, paste("Infiltration.", c("DailyMax", "DailyMin"), "_mm_mean", sep=""), paste("Infiltration.", c("DailyMax", "DailyMin"), "_doy_mean", sep=""))
	}
	
#28
	if(any(simulation_timescales=="daily") & aon$dailyAETExtremes) {
		temp <- c(temp, paste("AET.", c("DailyMax", "DailyMin"), "_mm_mean", sep=""), paste("AET.", c("DailyMax", "DailyMin"), "_doy_mean", sep=""))
	}
	
#29
	if(any(simulation_timescales=="daily") & aon$dailySWPextremes){
		temp <- c(temp, paste("SWP.", rep(c("topLayers.", "bottomLayers."), each=2), rep(c("DailyMax", "DailyMin"), times=2), "_doy_mean", sep=""))
	}
	
	##############################################################---Aggregation: Ecological dryness---##############################################################
	
#30
	if(any(simulation_timescales=="daily") & aon$dailyWetDegreeDays){
		temp <- c(temp, paste("WetDegreeDays.SWPcrit", rep(paste(abs(round(-1000*SWPcrit_MPa, 0)), "kPa", sep=""), each=3), rep(c(".topLayers", ".bottomLayers", ".anyLayer"), times=length(SWPcrit_MPa)), "_Cdays_mean", sep=""))
	}
	
#31
	if(any(simulation_timescales=="monthly") & aon$monthlySWPdryness){
		temp <- c(temp, paste("DrySoilPeriods.SWPcrit", rep(paste(abs(round(-1000*SWPcrit_MPa, 0)), "kPa", sep=""), times=2), ".NSadj.", rep(c("topLayers", "bottomLayers"), each=length(SWPcrit_MPa)), ".Duration.Total_months_mean", sep=""), 
				paste("DrySoilPeriods.SWPcrit", rep(paste(abs(round(-1000*SWPcrit_MPa, 0)), "kPa", sep=""), times=2), ".NSadj.", rep(c("topLayers", "bottomLayers"), each=length(SWPcrit_MPa)), ".Start_month_mean", sep=""))
	}
	
#32
	if(any(simulation_timescales=="daily") & aon$dailySWPdrynessANDwetness){
		temp <- c(temp, paste(rep(c("WetSoilPeriods", "DrySoilPeriods"), each=8), ".SWPcrit", rep(paste(abs(round(-1000*SWPcrit_MPa, 0)), "kPa", sep=""), each=16), ".NSadj.", c(rep(c("topLayers", "bottomLayers"), times=4), rep(rep(c("topLayers", "bottomLayers"), each=2), times=2)), 
						rep(c(".AnyLayerWet.", ".AllLayersWet.", ".AllLayersDry.", ""), each=4), c(rep(rep(c("Duration.Total_days", "Duration.LongestContinuous_days"), each=2), times=2), rep(c("Duration.Total_days", "Duration.LongestContinuous_days"), times=2), rep(c(".PeriodsForAtLeast10Days.Start_doy", ".PeriodsForAtLeast10Days.End_doy"), times=2)), "_mean", sep=""))
	}
	
#33
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
	
#34
	if(any(simulation_timescales=="daily") && aon$dailySWPdrynessEventSizeDistribution) {
		binSize <- c(1, 8, 15, 29, 57, 183, 367) #closed interval lengths in [days] within a year; NOTE: n_variables is set for binsN == 4
		binsN <- length(binSize) - 1
		binTitle <- paste("SizeClass", paste(binSize[-length(binSize)], binSize[-1]-1, sep="to") ,"days", sep="")
		for(icrit in seq(along=SWPcrit_MPa)) {
			temp <- c(temp, paste("DrySoilPeriods.SWPcrit", paste(abs(round(-1000*SWPcrit_MPa[icrit], 0)), "kPa", sep=""), ".Annual.", rep(c("topLayers", "bottomLayers"), each=binsN+1), rep(c("_count", paste(".", binTitle, "_fraction", sep="")), times=2), "_mean", sep=""))
		}
		rm(binSize, binsN, binTitle)
	}
	
#35
	if(any(simulation_timescales=="daily") && aon$dailySWPdrynessIntensity) {
		for(icrit in seq(along=SWPcrit_MPa)){
			temp <- c(temp, paste("DrySoilPeriods.SWPcrit", paste(abs(round(-1000*SWPcrit_MPa[icrit], 0)), "kPa", sep=""), ".MissingWater.", rep(c("topLayers", "bottomLayers"), each=4), ".", rep(c("AnnualSum_mmH2O", "PerEventPerDay_mmH2O", "Duration.Event_days", "Events_count"), times=2), "_mean", sep=""))
		}
	}
	
	##############################################################---Aggregation: Mean monthly values---##############################################################
	
#36
	if(any(simulation_timescales=="monthly") & aon$monthlyTemp){
		temp <- c(temp, paste("TempAir.m", st_mo, "_C_mean", sep=""))
	}
	
#37
	if(any(simulation_timescales=="monthly") & aon$monthlyPPT){
		temp <- c(temp, paste("Precip.m", st_mo, "_mm_mean", sep=""))
	}
	
#38
	if(any(simulation_timescales=="monthly") & aon$monthlySnowpack){
		temp <- c(temp, paste("Snowpack.m", st_mo, "_mmSWE_mean", sep=""))
	}
	
#39
	if(any(simulation_timescales == "monthly") & aon$monthlySoilTemp) {
		temp <- c(temp, paste("TempSoil.", c(paste("topLayers.m", st_mo, sep=""), paste("bottomLayers.m", st_mo, sep="")), "_C_mean", sep=""))
	}
	
#40
	if(any(simulation_timescales=="monthly") & aon$monthlyRunoff){
		temp <- c(temp, paste("Runoff.Total.m", st_mo, "_mm_mean", sep=""))
	}
	
#41
	if(any(simulation_timescales=="monthly") & aon$monthlyHydraulicRedistribution){
		temp <- c(temp, paste("HydraulicRedistribution.", c(paste("topLayers.m", st_mo, sep=""), paste("bottomLayers.m", st_mo, sep="")), "_mm_mean", sep=""))
	}
	
#42
	if(any(simulation_timescales=="monthly") & aon$monthlyInfiltration){
		temp <- c(temp, paste("Infiltration.m", st_mo, "_mm_mean", sep=""))
	}
	
#43
	if(any(simulation_timescales=="monthly") & aon$monthlySWP){
		temp <- c(temp, paste("SWP.", c(paste("topLayers.m", st_mo, sep=""), paste("bottomLayers.m", st_mo, sep="")), "_MPa_FromVWCmean", sep=""))
	}
	
#44
	if(any(simulation_timescales=="monthly") & aon$monthlyVWC){
		temp <- c(temp, paste("VWC.", c(paste("topLayers.m", st_mo, sep=""), paste("bottomLayers.m", st_mo, sep="")), "_mPERm_mean", sep=""))
	}
	
#45
	if(any(simulation_timescales=="monthly") & aon$monthlySWC){
		temp <- c(temp, paste("SWC.", c(paste("topLayers.m", st_mo, sep=""), paste("bottomLayers.m", st_mo, sep="")), "_mm_mean", sep=""))
	}
	
#46
	if(any(simulation_timescales=="monthly") & aon$monthlySWA){
		temp <- c(temp, paste("AWC.", c(paste("topLayers.m", st_mo, sep=""), paste("bottomLayers.m", st_mo, sep="")), "_mm_mean", sep=""))
	}
	
#47
	if(any(simulation_timescales=="monthly") & aon$monthlyTranspiration){
		temp <- c(temp, paste("Transpiration.", c(paste("topLayers.m", st_mo, sep=""), paste("bottomLayers.m", st_mo, sep="")), "_mm_mean", sep=""))
	}
	
#48
	if(any(simulation_timescales=="monthly") & aon$monthlySoilEvaporation){
		temp <- c(temp, paste("Evaporation.Soil.m", st_mo, "_mm_mean", sep=""))
	}
	
#49
	if(any(simulation_timescales=="monthly") & aon$monthlyAET){
		temp <- c(temp, paste("AET.m", st_mo, "_mm_mean", sep=""))
	}
	
#50
	if(any(simulation_timescales=="monthly") & aon$monthlyPET){
		temp <- c(temp, paste("PET.m", st_mo, "_mm_mean", sep=""))
	}
	
#51
	if(any(simulation_timescales=="monthly") & aon$monthlyAETratios){
		temp <- c(temp, paste(rep(c("TranspToAET.m", "EvapSoilToAET.m"), each=12), st_mo, "_fraction_mean", sep=""))
	}
	
#52
	if(any(simulation_timescales=="monthly") & aon$monthlyPETratios){
		temp <- c(temp, paste(rep(c("TranspToPET.m", "EvapSoilToPET.m"), each=12), st_mo, "_fraction_mean", sep=""))
	}
	
	##############################################################---Aggregation: Potential regeneration---##############################################################
	
#53
	if(any(simulation_timescales=="daily")  & aon$dailyRegeneration_bySWPSnow) {
		temp <- c(temp, "Regeneration.Potential.SuitableYears.NSadj_fraction_mean")
	}
	
#54
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

	dbOverallColumns <- length(temp)
	
	temp <- paste(paste("\"", temp, "\"",sep=""), " REAL", collapse = ", ")
	
	sdString <- gsub("_mean", "_sd", temp)
	meanString <- paste(c("\"P_id\" INTEGER PRIMARY KEY",temp), collapse = ", ")
	sdString <-paste(c("\"P_id\" INTEGER PRIMARY KEY",sdString), collapse = ", ")
	
	SQL_Table_Definitions1 <- paste("CREATE TABLE \"aggregation_overall_mean\" (", meanString, ");", sep="")
	SQL_Table_Definitions2 <- paste("CREATE TABLE \"aggregation_overall_sd\" (", sdString, ");", sep="")
		
	empty <- ifelse(length(dbListTables(con))==0, TRUE, FALSE)
	if(cleanDB && !empty) for(i in 1:length(Tables)) { res <- dbSendQuery(con,paste("DROP TABLE ", '"',Tables[i], '"', sep="")); dbClearResult(res) }
	rs <- dbSendQuery(con, paste(SQL_Table_Definitions1, collapse = "\n"))
	dbClearResult(rs)
	rs <- dbSendQuery(con, paste(SQL_Table_Definitions2, collapse = "\n"))
	dbClearResult(rs)
	
	doy_colnames <- paste("doy", formatC(1:366, width=3, format="d", flag="0"), sep="")
	doy_colnames <- paste(paste("\"", doy_colnames, "\"",sep=""), " REAL", collapse = ", ")
	
	temp <-paste(c("\"P_id\" INTEGER PRIMARY KEY", doy_colnames), collapse = ", ")
	temp1 <-paste(c("\"P_id\" INTEGER", "\"Soil_Layer\" INTEGER", doy_colnames,"PRIMARY KEY (\"P_id\",\"Soil_Layer\")"), collapse = ", ")
	
	if(any(simulation_timescales=="daily") && daily_no > 0) {
		for(doi in 1:daily_no) {
			if(regexpr("SWA", output_aggregate_daily[doi]) > 0){
				agg.resp <- "SWA"
				index.SWPcrit <- -as.numeric(sub("kPa", "", sub("SWAatSWPcrit", "", output_aggregate_daily[doi])))/1000
			} else {
				agg.resp <- output_aggregate_daily[doi]
			}
			agg.analysis <- switch(EXPR=agg.resp, AET=1, Transpiration=2, EvaporationSoil=1, EvaporationSurface=1, EvaporationTotal=1, VWC=2, SWC=2, SWP=2, SWA=2, Snowpack=1, Rain=1, Snowfall=1, Snowmelt=1, SnowLoss=1, Infiltration=1, DeepDrainage=1, PET=1, TotalPrecipitation=1, TemperatureMin=1, TemperatureMax=1, SoilTemperature=2, Runoff=1)
			
			tableName <- paste("aggregation_doy_", output_aggregate_daily[doi], sep="")
			
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
		rm(tableName, agg.analysis, agg.resp)
	}
	rm(rs, sdString, meanString, temp,temp1, doy_colnames)
} else {
	dbOverallColumns <- length(dbListFields(con,"aggregation_overall_mean"))-1
}
rm(Tables)
