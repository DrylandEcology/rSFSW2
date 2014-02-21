# TODO: Add comment
# 
# Author: ryan
# 
#	Walk you through connecting and then dumping one or all tables from the database.
#
###############################################################################
library(RSQLite)

dir.DB <- "/Users/drschlaep/Documents/drschlaepfer/2_Research/200907_UofWyoming_PostDoc/Projects_My/Product_PowellCenter/6_Projects_Year1/Prj02_Disturbances/1_PC_TempDry_Simulations_Prj02_r2mini/4_Data_SWOutputAggregated"
dir.out <- "/Users/drschlaep/Documents/drschlaepfer/2_Research/200907_UofWyoming_PostDoc/Projects_My/Product_PowellCenter/6_Projects_Year1/Prj02_Disturbances/1_PC_TempDry_Simulations_Prj02_r2mini/4_Data_SWOutputAggregated_csvTables"

filesInDBdir <- list.files(dir.DB,pattern="sqlite3")
headerTables <- c("runs","sqlite_sequence","header","run_labels","scenario_labels","sites","experimental_labels","treatments","simulation_years","weatherfolders")

for(dbIndex in seq_along(filesInDBdir)){

	dbName <- filesInDBdir[dbIndex]
	dbFile <- file.path(dir.DB, dbName)
	dir.create(dir.out.db <- file.path(dir.out, strsplit(dbName, split=".", fixed=TRUE)[[1]][1]))

	##establish connection##
	drv <- dbDriver("SQLite")
	con <- dbConnect(drv, dbFile)

	Tables <- dbListTables(con)
	if(any(Tables %in% headerTables)) Tables <- Tables[-which(Tables %in% headerTables)]

	for(i in 1:length(Tables)) {
		temp <- dbReadTable(con, Tables[i])
		write.csv(x=temp, file=file.path(dir.out.db, paste(Tables[i],".csv",sep="")), row.names=FALSE, )
	}
	con <- dbConnect(drv, file.path(dir.DB, "dbTables.sqlite3"))
	temp <- dbReadTable(con, "header")
	write.csv(x=temp, file=file.path(dir.out.db, paste("header",".csv",sep="")), row.names=FALSE, )
}
