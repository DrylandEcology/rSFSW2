# TODO: Add comment
# 
# Author: Ryan Murphy
#
#	
#	This file will construct and fill the treatments table from the experimentals and treatment excel files.
#	If experimentals are not used, then it will leave out the experimental_labels table
#
#	Tables Created
#		* treatments
#		* simulation_years
#		* experimental_labels
#
#
###############################################################################

###################FOR TESTING
#library(RSQLite)
#drv <- dbDriver("SQLite")
#setwd("/home/ryan/Documents/Work/1_PC_TempDry_Simulations_Prj04_r1_Rsoilwat/")
#con <- dbConnect(drv, dbname = "test.db")
#
#res<-dbSendQuery(con,"PRAGMA foreign_keys = ON;")
#fetch(res)
#dbClearResult(res)
#
#dir.in<-"/home/ryan/Documents/Work/1_PC_TempDry_Simulations_Prj04_r1_Rsoilwat/1_Data_SWInput"
#datafile.treatments <- "SWRuns_InputData_TreatmentDesign_v14.csv"
#datafile.Experimentals <- "SWRuns_InputData_ExperimentalDesign_Prj04_v01.csv"
#
#sw_input_treatments_use <- tryCatch(read.csv(temp <- file.path(dir.in, datafile.treatments), nrows=1),error=function(e) { print("datafile.treatments: Bad Path"); print(e)})
#sw_input_treatments <- read.csv(temp, skip=1, as.is=TRUE)
#colnames(sw_input_treatments) <- colnames(sw_input_treatments_use)
#
#sw_input_experimentals_use <- tryCatch(read.csv(temp <- file.path(dir.in, datafile.Experimentals), nrows=1),error=function(e) { print("datafile.Experimentals: Bad Path"); print(e)})
#sw_input_experimentals <- read.csv(temp, skip=1, as.is=TRUE)
#colnames(sw_input_experimentals) <- colnames(sw_input_experimentals_use)
#create_experimentals <- names(sw_input_experimentals_use[-1][which(sw_input_experimentals_use[-1] > 0 & is.finite(as.numeric(sw_input_experimentals_use[-1])))])
#
##update treatment specifications based on experimental design
#sw_input_treatments_use_combined <- ifelse(sw_input_treatments_use[-1] == 1 | names(sw_input_treatments_use[-1]) %in% create_experimentals, 1, 0)
#create_treatments <- names(sw_input_treatments_use_combined[,which(sw_input_treatments_use_combined > 0 & is.finite(as.numeric(sw_input_treatments_use_combined)))])
###################END TESTING

useExperimentals <- trowExperimentals > 0 && length(create_experimentals) > 0

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
##########

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

# we only need the columns that are turned on and not in experimentals. Experimentals over write. Only rows that are going to be used
db_treatments <- unique(df<-sw_input_treatments[seq.tr,create_treatments[!(create_treatments %in% create_experimentals)]])

#
temp<-duplicated(df)
treatments_unique_map<-rep(NA,length(df))
temp2 <- data.frame(t(df))
treatments_unique_map[temp]<-match(data.frame(t(df[temp,])),temp2)
treatments_unique_map[!temp] <- match(data.frame(t(df[!temp,])),temp2)
db_treatments_map<-unique(treatments_unique_map)
treatments_unique_map <- sapply(treatments_unique_map, function(x) which(db_treatments_map == x))
rm(db_treatments_map)

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

#Create a table to hold the values going into the database
temp_numberRows <- ifelse(useExperimentals,nrow(db_experimentals)*nrow(db_treatments),nrow(db_treatments))
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
if(any(create_treatments=="LookupWeatherFolder")) {
	db_treatments_column_types[which(db_treatments_column_types[,1] == "LookupWeatherFolder"),1:2] <- c("LookupWeatherFolder_id","INTEGER")
	colnames(db_combined_exp_treatments)[-(1:2)] <- db_treatments_column_types[,1]
	fk_LookupWeatherFolder <- ", FOREIGN KEY(LookupWeatherFolder_id) REFERENCES weatherfolders(id)"
}

#Create the table
dbGetQuery(con, paste("CREATE TABLE treatments(id INTEGER PRIMARY KEY AUTOINCREMENT, ",if(useExperimentals) "experimental_id INTEGER,", " simulation_years_id INTEGER, ", paste(db_treatments_column_types[,1], " ", db_treatments_column_types[,2], sep="", collapse =", "), ", ", if(useExperimentals) "FOREIGN KEY(experimental_id) REFERENCES experimental_labels(id)",fk_LookupWeatherFolder,");", sep=""))

#Lets put in the treatments into combined. This will repeat the reduced rows of treatments into combined
db_combined_exp_treatments[,db_treatments_column_types[db_treatments_column_types[,3]==0,1]] <- db_treatments

if(useExperimentals) {
	exp_start_rows<-seq(from=1,to=nrow(db_treatments)*nrow(db_experimentals),by=nrow(db_treatments))
	#Insert data into our new data.frame
	for(start in exp_start_rows) {
		#Get experimental_label_id
		db_combined_exp_treatments[start:(start+nrow(db_treatments)-1),2] <- which(exp_start_rows==start)
		#insert all of the rows from experimentals
		db_combined_exp_treatments[start:(start+nrow(db_treatments)-1),db_treatments_column_types[db_treatments_column_types[,3]==1,1]] <- db_experimentals[which(exp_start_rows==start),]
	}
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

