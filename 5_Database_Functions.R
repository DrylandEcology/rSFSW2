#--------------------------------------------------------------------------------------------------#

#------CODE developed and written by
# - Daniel R Schlaepfer (dschlaep@uwyo.edu, drs): 2009-2013
#for contact and further information see also: sites.google.com/site/drschlaepfer

#------DISCLAIMER: This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#--------------------------------------------------------------------------------------------------#

#---Constants
currentSc <- "Current"

#---R packages
libraries <- c("RSQLite")
l <- lapply(libraries, FUN=function(lib) stopifnot(require(lib, character.only=TRUE, quietly=TRUE)))


#---Directories
stopifnot(file.exists(dir.dat))

#---File names
maker.climateScenarios <- function(currentScenario=currentSc, ensembleScenarios=c("SRESA2", "SRESB1"), ensembleLevels=c(2, 8, 15)){
	climCat <- data.frame(matrix(NA, nrow=1 + length(ensembleScenarios)*length(ensembleLevels), ncol=2, dimnames=list(NULL, c("Family", "Rank"))))
	climCat[1, 1] <- currentScenario
	climCat[-1, 1] <- rep(ensembleScenarios, each=length(ensembleLevels))
	climCat[-1, 2] <- rep(ensembleLevels, times=length(ensembleScenarios))
	rownames(climCat) <- apply(climCat, 1, FUN=function(r) paste0(trim(na.exclude(r)), collapse="_rank"))
	return(climCat)
}

if(!exists("climCat")) climCat <- maker.climateScenarios()
if(!exists("name.dbScen")) name.dbScen <- "dbTables.db"
temp <- try(!file.exists(names.dbEns[1]), silent=TRUE)
if(identical(class(temp), "try-error") || !temp) names.dbEns <- list.files(dir.dat, pattern="dbEnsemble_")

#---Prepare database access
drv <- dbDriver("SQLite")


#---Database functions
#List tables and variables of a database
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

#Access data from a database
#Get data of variables in the overall aggregation table for one of the scenarios
get.SeveralOverallVariables_Scenario <- function(responseName, MeanOrSD="Mean", scenario=currentSc, whereClause=NULL){
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
				if(length(whereClause) > 0){
					sql <- paste0("SELECT ", paste0(paste0("\"", iColumns, "\""), collapse=", "), " FROM ", iTable, " WHERE Scenario=", shQuote(scenario), " AND ", whereClause, " ORDER BY P_id;")
				} else {
					sql <- paste0("SELECT ", paste0(paste0("\"", iColumns, "\""), collapse=", "), " FROM ", iTable, " WHERE Scenario=", shQuote(scenario), " ORDER BY P_id;")
				}
				dat <- dbGetQuery(con, sql)
			}
		}
		dbDisconnect(con)
	}
	return(dat)
}

#Get data of variables in the overall aggregation table for one of the ensembles
get.SeveralOverallVariables_Ensemble <- function(responseName, MeanOrSD="Mean", fam, level, whereClause=NULL){
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
				if(length(whereClause) > 0){
					sql <- paste0("SELECT ", paste0(paste0("\"", iColumns, "\""), collapse=", "), " FROM ", iTable, " WHERE ", whereClause, " ORDER BY P_id;")
				} else {
					sql <- paste0("SELECT ", paste0(paste0("\"", iColumns, "\""), collapse=", "), " FROM ", iTable, " ORDER BY P_id;")
				}
				dat <- dbGetQuery(con, sql)
			}
		}
		dbDisconnect(con)
	}
	return(dat)
}

#Get data of variables in the overall aggregation table for one of the climCat rows (combining 'Current' and ensembles)
get.SeveralOverallVariables <- function(responseName, MeanOrSD="Mean", i_climCat=1, whereClause=NULL){
	if(length(responseName) > 0 && i_climCat <= nrow(climCat)){
		if(climCat[i_climCat, 1] == currentSc){
			dat <- get.SeveralOverallVariables_Scenario(responseName=responseName, MeanOrSD=MeanOrSD, scenario=climCat[i_climCat, 1], whereClause=whereClause)
		} else {
			dat <- get.SeveralOverallVariables_Ensemble(responseName=responseName, MeanOrSD=MeanOrSD, fam=climCat[i_climCat, 1], level=climCat[i_climCat, 2], whereClause=whereClause)
		}
		if(ncol(dat) == 1){
			dat <- as.vector(dat[,1])
		}
	} else {
		dat <- NULL
	}
	return(dat)
}

#Split a table into header and data
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

#Get data for an entire table for one of the scenarios
get.Table_Scenario <- function(responseName, MeanOrSD="Mean", scenario=currentSc, whereClause=NULL){
	dat <- NULL
	if(length(responseName) > 0){
		con <- dbConnect(drv, file.path(dir.dat, name.dbScen))
		iTable <- (temp <- dbListTables(con))[grepl(pattern=paste0(responseName, "_", MeanOrSD), x=temp, fixed=FALSE)]
		if(length(iTable) == 1){
			if(length(whereClause) > 0){
				sql <- paste0("SELECT * FROM ", iTable, " WHERE Scenario=", shQuote(scenario), " AND ", whereClause, " ORDER BY P_id;")
			} else {
				sql <- paste0("SELECT * FROM ", iTable, " WHERE Scenario=", shQuote(scenario), " ORDER BY P_id;")
			}
			dat <- dbGetQuery(con, sql)
		}
		dbDisconnect(con)
	}	
	return(dat)
}

#Get data for an entire table for one of the ensembles
get.Table_Ensemble <- function(responseName, MeanOrSD="Mean", fam, level, whereClause=NULL){
	dat <- NULL
	if(length(responseName) > 0){
		con <- dbConnect(drv, file.path(dir.dat, names.dbEns[grepl(pattern=paste0("_", responseName), x=names.dbEns)]))
		iTable <- (temp <- dbListTables(con))[grepl(pattern=fam, x=temp) & grepl(pattern=paste0("Rank", formatC(level, format="d", flag="0", width=2)), x=temp) & grepl(pattern=paste0("_", ifelse(responseName=="Overall", "", "Daily"), MeanOrSD), x=temp)]
		if(length(iTable) == 1){
			if(length(whereClause) > 0){
				sql <- paste0("SELECT * FROM ", iTable, " WHERE ", whereClause, " ORDER BY P_id;")
				dat <- dbGetQuery(con, sql)
			} else {
				dat <- dbReadTable(con, iTable)
			}
		}
		dbDisconnect(con)
	}
	return(dat)
}

#Get data-part for an entire table for one of the climCat rows (combining 'Current' and ensembles)
get.Table <- function(responseName, MeanOrSD="Mean", i_climCat=1, whereClause=NULL, addPid=FALSE){
	if(length(responseName) > 0 && i_climCat <= nrow(climCat)){
		if(climCat[i_climCat, 1] == currentSc){
			dat <- get.Table_Scenario(responseName=responseName, MeanOrSD=MeanOrSD, scenario=climCat[i_climCat, 1], whereClause=whereClause)
		} else {
			dat <- get.Table_Ensemble(responseName=responseName, MeanOrSD=MeanOrSD, fam=climCat[i_climCat, 1], level=climCat[i_climCat, 2], whereClause=whereClause)
		}
		temp <- split.Table_intoHeaderData(dat)
		if(addPid){
			dat <- data.frame(P_id=temp$header$P_id, temp$dat)
		} else {
			dat <- temp$dat
		}
	} else {
		dat <- NULL
	}
	return(dat)
}

#Get header/design and names of experimental factors of a table
get.TableDesign <- function(responseName, whereClause=NULL){
	datCur <- get.Table_Scenario(responseName=responseName, MeanOrSD="Mean", scenario=climCat[1, 1], whereClause=whereClause)
	trValues <- split.Table_intoHeaderData(datCur)$header
	trNames_All <- (temp <- colnames(trValues))[1:which("Scenario" == temp)[1]]
	trNames_Experiment <- (temp <- trNames_All[((temp <- which("Experimental_Label" == trNames_All)[1])):(which("Scenario" == trNames_All)[1] - 1)])[!(temp %in% c("LookupWeatherFolder", "YearStart", "YearEnd", "SimStartYear"))]
	return(list(trValues=trValues, trNames_Experiment=trNames_Experiment))
}

#Get header/design and names of experimental factors for the overall aggregation table
get.OverallDesign <- function(whereClause=NULL){
	trNames_All <- (temp <- list.dbVariables(dbName=name.dbScen, dbTable="Aggregation_Overall_Mean"))[1:which("Scenario" == temp)[1]]
	trNames_Experiment <- (temp <- trNames_All[((temp <- which("Experimental_Label" == trNames_All)[1])):(which("Scenario" == trNames_All)[1] - 1)])[!(temp %in% c("LookupWeatherFolder", "YearStart", "YearEnd", "SimStartYear"))]
	trValues <- get.SeveralOverallVariables_Scenario(responseName=trNames_All, MeanOrSD="Mean", scenario=climCat[1, 1], whereClause=whereClause)
	return(list(trValues=trValues, trNames_Experiment=trNames_Experiment))
}

