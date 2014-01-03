# TODO: Add comment
# 
# Author: Ryan Murphy
# This file reads in the InputMaster data table and constructs the table for it in the database.
#
#	Tables Generated
#		* sites
# 
###############################################################################

###############################for testing#####################################
#library(RSQLite)
#drv <- dbDriver("SQLite")
#con <- dbConnect(drv, dbname = "/home/ryan/Documents/Work/1_PC_TempDry_Simulations_Prj04_r1_Rsoilwat/test.db")
#
#res<-dbSendQuery(con,"PRAGMA foreign_keys = ON;")
#fetch(res)
#dbClearResult(res)
#
#dir.in <- "/home/ryan/Documents/Work/1_PC_TempDry_Simulations_Prj04_r1_Rsoilwat/1_Data_SWInput"
#datafile.SWRunInformation <- "SWRuns_InputMaster_TemperateArid_SiteSubsample_v10.csv"
#
#SWRunInformation <- tryCatch(read.csv(file.path(dir.in, datafile.SWRunInformation), as.is=TRUE),error=function(e) { print("datafile.SWRunInformation: Bad Path"); print(e)})
#include_YN <- SWRunInformation$Include_YN
#labels <- SWRunInformation$Label
#
#runs <- sum(include_YN>0, na.rm=TRUE)
#trow  <- length(include_YN)
#seq.tr <- (1:trow)[include_YN > 0]
#seq.todo <- (1:(runs * ifelse(trowExperimentals > 0, trowExperimentals, 1))) # consecutive number of all (tr x exp) simulations to be executed
#runsN.todo <- length(seq.todo)
###############################END TESTING######################################


# Create weatherfolder table
# Two columns id, folder
dbGetQuery(con, "CREATE TABLE weatherfolders(id INTEGER PRIMARY KEY AUTOINCREMENT, folder TEXT UNIQUE NOT NULL);")
dbBeginTransaction(con)
dbGetPreparedQuery(con, "INSERT INTO weatherfolders VALUES(NULL, :folder)", bind.data = data.frame(folder=SWRunInformation$WeatherFolder[seq.tr],stringsAsFactors=FALSE))
dbCommit(con)

#Site Table
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
#This returns the SQLite Types of the columns
site_col_types <-sapply(X=1:ncol(SWRunInformation), function(x) mapType(typeof(SWRunInformation[[x]])))
site_columns <- colnames(SWRunInformation)
site_columns <- sub(pattern="ID", replacement="site_id",site_columns)
site_columns <- sub(pattern="WeatherFolder",replacement="WeatherFolder_id",site_columns)
dbGetQuery(con, paste("CREATE TABLE sites(id INTEGER PRIMARY KEY AUTOINCREMENT,", paste(site_columns, site_col_types, collapse=","), ", FOREIGN KEY(WeatherFolder_id) REFERENCES weatherfolders(id));", sep=""))


sites_data<-data.frame(SWRunInformation[seq.tr,],row.names=NULL, check.rows=FALSE, check.names=FALSE, stringsAsFactors=FALSE)

colnames(sites_data) <- site_columns
sites_data$WeatherFolder_id <- 1:length(sites_data$site_id)
#This works fast
dbBeginTransaction(con)
dbGetPreparedQuery(con, paste("INSERT INTO sites VALUES(NULL,",paste(":",site_columns,collapse=",",sep=""),")",sep=""), bind.data=sites_data)
dbCommit(con)

rm(site_col_types,site_columns,sites_data)
