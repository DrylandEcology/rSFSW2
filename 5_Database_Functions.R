#--------------------------------------------------------------------------------------------------#

#------CODE developed and written by
# - Daniel R Schlaepfer (dschlaep@uwyo.edu, drs): 2009-2013
#for contact and further information see also: sites.google.com/site/drschlaepfer

#------DISCLAIMER: This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#--------------------------------------------------------------------------------------------------#

#R packages
libraries <- c("RSQLite")
l <- lapply(libraries, FUN=function(lib) stopifnot(require(lib, character.only=TRUE, quietly=TRUE)))


#Directories
stopifnot(file.exists(dir.dat))

#File names
if(!exists("name.dbScen")) name.dbScen <- "dbTables.db"
temp <- try(!file.exists(names.dbEns[1]), silent=TRUE)
if(identical(class(temp), "try-error") || !temp) names.dbEns <- list.files(dir.dat, pattern="dbEnsemble_")

#Prepare database access
drv <- dbDriver("SQLite")


#Database functions
list.dbTables <- function(dbName){
	con <- dbConnect(drv, file.path(dir.dat, dbName))
	res <- dbListTables(con)
	dbDisconnect(con)
	return(res)
}

list.dbVariables <- function(dbName, dbTable){
	con <- dbConnect(drv, file.path(dir.dat, dbName))
	res <- dbListFields(con, dbTable)
	dbDisconnect(con)
	return(res)
}

list.dbVariablesOfAllTables <- function(dbName){
	tables <- list.dbTables(dbName)
	res <- sapply(tables, FUN=function(it) list.dbVariables(dbName, dbTable=it))
	return(res)
}


get.SeveralOverallVariables_Scenario <- function(responseName, MeanOrSD="Mean", scenario="Current"){
	dat <- iColumns <- NULL
	if(length(responseName) > 0){
		con <- dbConnect(drv, file.path(dir.dat, name.dbScen))
		iTable <- (temp <- dbListTables(con))[grepl(pattern=paste0("Overall_", MeanOrSD), x=temp, fixed=FALSE)]
		if(length(iTable) == 1){
			fields <- dbListFields(con, iTable)
			for(i in seq_along(responseName)){
				iColumns <- c(iColumns, fields[grepl(pattern=responseName[i], x=fields, fixed=FALSE)])
			}
			if(length(iColumns) > 0){
				sql <- paste0("SELECT ", paste0(paste0("\"", iColumns, "\""), collapse=", "), " FROM ", iTable, " WHERE Scenario=", shQuote(scenario), " ORDER BY P_id;")
				dat <- dbGetQuery(con, sql)
			}
		}
		dbDisconnect(con)
	}
	return(dat)
}


get.Table_Scenario <- function(responseName, MeanOrSD="Mean", scenario="Current"){
	dat <- NULL
	if(length(responseName) > 0){
		con <- dbConnect(drv, file.path(dir.dat, name.dbScen))
		iTable <- (temp <- dbListTables(con))[grepl(pattern=paste0(responseName, "_", MeanOrSD), x=temp, fixed=FALSE)]
		if(length(iTable) == 1){
			sql <- paste0("SELECT * FROM ", iTable, " WHERE Scenario=", shQuote(scenario), " ORDER BY P_id;")
			dat <- dbGetQuery(con, sql)
		}
		dbDisconnect(con)
	}	
	return(dat)
}

get.SeveralOverallVariables_Ensemble <- function(responseName, MeanOrSD="Mean", fam, level){
	dat <- iColumns <- NULL
	if(length(responseName) > 0){
		con <- dbConnect(drv, file.path(dir.dat, names.dbEns[grepl(pattern="Overall", x=names.dbEns)]))
		iTable <- (temp <- dbListTables(con))[grepl(pattern=fam, x=temp) & grepl(pattern=paste0("Rank", formatC(level, format="d", flag="0", width=2)), x=temp) & grepl(pattern=paste0("_", MeanOrSD), x=temp)]
		if(length(iTable) == 1){
			fields <- dbListFields(con, iTable)
			for(i in seq_along(responseName)){
				iColumns <- c(iColumns, fields[grepl(pattern=responseName[i], x=fields, fixed=FALSE)])
			}
			if(length(iColumns) > 0){
				sql <- paste0("SELECT ", paste0(paste0("\"", iColumns, "\""), collapse=", "), " FROM ", iTable, " ORDER BY P_id;")
				dat <- dbGetQuery(con, sql)
			}
		}
		dbDisconnect(con)
	}
	return(dat)
}

get.Table_Ensemble <- function(responseName, MeanOrSD="Mean", fam, level){
	dat <- NULL
	if(length(responseName) > 0){
		con <- dbConnect(drv, file.path(dir.dat, names.dbEns[grepl(pattern=paste0("_", responseName), x=names.dbEns)]))
		iTable <- (temp <- dbListTables(con))[grepl(pattern=fam, x=temp) & grepl(pattern=paste0("Rank", formatC(level, format="d", flag="0", width=2)), x=temp) & grepl(pattern=paste0("_", ifelse(responseName=="Overall", "", "Daily"), MeanOrSD), x=temp)]
		if(length(iTable) == 1){
			dat <- dbReadTable(con, iTable)
		}
		dbDisconnect(con)
	}
	return(dat)
}


split.Table_intoHeaderData <- function(dat){
	header <- NULL
	icol <- which(colnames(dat) == "Scenario")
	if(length(icol) == 0) icol <- which(colnames(dat) == "EnsembleName") + 1
	if(length(icol) > 0 && icol < ncol(dat)){
		header <- dat[, 1:icol]
		dat <- dat[, (icol+1):ncol(dat)]
	}
	return(list(header=header, dat=dat))
}
