# TODO: Add comment
# 
# Author: ryan
# 
#	Walk you through connecting and then dumping one or all tables from the database.
#
###############################################################################
library(RSQLite)

readNumberRePrompt <- function(variable, string) {
	ANSWER <- readline(string)
	if(!(ANSWER == "NA"))
		tryCatch(ANSWER <- as.numeric(ANSWER), error=function() { readNumberRePrompt(variable,string) }, warning=function(x) { readNumberRePrompt(variable,string) })
	if(is.numeric(ANSWER) || ANSWER=="NA")
		assign(variable, ifelse(ANSWER=="NA", NA, ANSWER), pos=1)
}

dir.DB <- ""
print("Path to database files. Windows use / instead of \\")
if(as.logical(readline(paste("Use Current Directory (TRUE or FALSE): ",getwd()," : ",sep="")))) {
	dir.DB <- getwd()
} else {
	dir.DB <- readline("Path to SQLite database (PATH): ")
	if(!file.exists(dir.DB)) {
		print("Path does not exist")
		dir.DB <- readline("Path to SQLite database (PATH): ")
	}
}

filesInDBdir <- list.files(dir.DB)
print(filesInDBdir)
dbIndex <- 0
readNumberRePrompt("dbIndex", "Please select DB: ")
dbName <- filesInDBdir[dbIndex]

dir.out <- ""
print("Path to database output. Windows use / instead of \\")
if(as.logical(readline(paste("Use Current Directory (TRUE or FALSE): ",getwd()," : ",sep="")))) {
	dir.DB <- getwd()
} else {
	dir.out <- readline("Path to output Table(s): ")
	if(!file.exists(dir.out)) {
		print("Path does not exist")
		dir.out <- readline("Path to output Table(s): ")
	}
}

dbFile <- file.path(dir.DB, dbName)

##establish connection##
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbFile)

Tables <- dbListTables(con)
print(Tables)
TableNumber <- 0
readNumberRePrompt("TableNumber", "Please select table to dump, 0=all : ")

if(TableNumber == 0) {
	for(i in 1:length(Tables)) {
		temp <- dbReadTable(con, Tables[i])
		write.csv(x=temp, file=file.path(dir.out, paste(Tables[i],".csv",sep="")), row.names=FALSE, )
	}
} else {
	temp <- dbReadTable(con, Tables[TableNumber])
	write.csv(x=temp, file=file.path(dir.out, paste(Tables[TableNumber], ".csv", sep="")), row.names=FALSE)
}


