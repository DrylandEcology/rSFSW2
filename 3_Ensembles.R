# TODO: Add comment
# 
# Author: ryan
###############################################################################


library(Rmpi)
library(RSQLite)


calc.ensembles <- function(dat, elevels){ #dat must be three-dimensional object with dims=(runs, outputs, scenarios); runs and/or scenarios can be 1 or larger
	doRanks <- function(x, ranks){
		temp <- sort.int(x, na.last=NA, index.return=TRUE)
		return(c(temp$x[ranks], temp$ix[ranks]))
	}
	
	res <- NULL
	col.names <- colnames(dat)
	if(dim(dat)[3] == 1){ #only one scenario; i.e., all levels are identical to the scenario
		temp <- array(data=rep(unlist(dat), each=3), dim=c(length(elevels), dim(dat)[1], dim(dat)[2]))
		res <- array(data=1, dim=c(2*length(elevels), dim(dat)[1], dim(dat)[2]))
		res[1:length(elevels),,] <- temp
	} else {
		#if(parallel_runs){
		#	if(identical(parallel_backend, "mpi")) {
		#		mpi.bcast.Robj2slave(obj=dat)
		#		mpi.bcast.Robj2slave(obj=doRanks)
		#		mpi.bcast.Robj2slave(obj=elevels)
		#		if(dim(dat)[1] > 1){
		#			res <- mpi.parApply(dat[,,], MARGIN = c(1,2), doRanks, ranks=elevels)
		#		} else { #only one run=site
		#			res <- mpi.parApply(dat[,,], MARGIN = 1, doRanks, ranks=elevels)
		#			res <- array(data=res, dim=c(2*length(elevels), 1, dim(dat)[2]))
		#		}
		#	}
		#	if(identical(parallel_backend, "snow")){
		#		if(dim(dat)[1] > 1){
		#			res <- parApply(cl, dat[,,], MARGIN = c(1,2), doRanks, ranks=elevels)
		#		} else { #only one run=site
		#			res <- parApply(cl, dat[,,], MARGIN = 1, doRanks, ranks=elevels)
		#			res <- array(data=res, dim=c(2*length(elevels), 1, dim(dat)[2]))
		#		}
		#	}
		#	if(identical(parallel_backend, "multicore")){
		#		res <- foreach(i = 1:nrow(dat)) %dopar% apply(dat[i,,], MARGIN=1, doRanks, ranks=elevels)
		#		res <- aperm(array(data=unlist(res), dim=c(2*length(elevels), dim(dat)[2], length(res))), perm=c(1, 3, 2))
		#	}
		#} else {
			if(dim(dat)[1] > 1){
				res <- apply(dat[,,], MARGIN = c(1,2), doRanks, ranks=elevels)
			} else { #only one run=site
				res <- apply(dat[,,], MARGIN = 1, doRanks, ranks=elevels)
				res <- array(data=res, dim=c(2*length(elevels), 1, dim(dat)[2]))
			}
		#}
	}
	#returned object: array with 3 dims: 1. dim = 1:length(elevels) are the ensembles at the ensemble.levels; the second set of rows are the ranked GCMs; 2. dim = runs/sites; 3. dim = aggregated variables
	dimnames(res) <- list(NULL, NULL, col.names)
	return(res)
}

collect_EnsembleFromScenarios <- function(Table){
	drv <- dbDriver("SQLite")
	tfile <- file.path(dir.out, "dbTables.db")
	con <- dbConnect(drv, dbname = tfile)
	#########TIMING#########
	TableTimeStop <- Sys.time() - t.overall
	units(TableTimeStop) <- "secs"
	TableTimeStop <- as.double(TableTimeStop)
	print(paste("Table: ",Table,": started at ",TableTime<-Sys.time(),sep=""))
	
	#########FUNCTIONS######
	doWrite <- function(dat, headerInfo, elevel, outfile){
		#add info to 
		name <- ensemble.family
		cAdd <- data.frame(matrix(data=c(name, elevel), nrow=nrow(headerInfo), ncol=2, byrow=TRUE))
		colnames(cAdd) <- c("EnsembleName", "Level")
		if(is.vector(dat)) {
			dat <- cbind(headerInfo, cAdd, t(dat))
		} else {
			dat <- cbind(headerInfo, cAdd, dat)
		}
		written <- dbWriteTable(conEnsembleDB, name=outfile, dat, row.names=FALSE,append=TRUE)#
		
		if(written)
			return(1)
		else
			return(0)
	}
	read.scenarios <- function(Table, start, stop, ensemble.family, export.header=TRUE){
		#Read first file
		sqlString <- paste("SELECT * FROM \"", Table, "\" WHERE P_id BETWEEN ",start," AND ",stop," AND \"Scenario\" LIKE '", tolower(ensemble.family), "%'", " ORDER BY \"P_id\"", sep="")
		res <- dbSendQuery(con, sqlString)
		dataScen.Mean <- fetch(res, n=-1) #dataToQuantilize get the data from the query n=-1 to get all rows
		dbClearResult(res)
		
		columnCutoff <- match("Scenario", colnames(dataScen.Mean))
		if(export.header) {
			headerInfo <- lapply(unique(dataScen.Mean$Scenario), function (x) dataScen.Mean[dataScen.Mean$Scenario == x,(1:(columnCutoff-1))])
			headerInfo <- headerInfo[[1]]
		}
		col.names <- colnames(dataScen.Mean[,-(1:columnCutoff)])
		#We have all the scenarios in the family. We need to get unique scenario names and group them by that
		data.temp <- lapply(unique(dataScen.Mean$Scenario), function (x) dataScen.Mean[dataScen.Mean$Scenario == x,-(1:columnCutoff)])
		data.temp <- array(data=unlist(data.temp), dim=c(nrow(data.temp[[1]]), ncol(data.temp[[1]]), length(data.temp)) )
		colnames(data.temp) <- col.names
		class(data.temp) <- "numeric"
		
		if(export.header) {
			return(list(headerInfo=headerInfo, data.scen=data.temp))
		} else {
			return(list(data.scen=data.temp))
		}
	}
	
	if(!(TableTimeStop > (MaxRunDurationTime-1*60))) {#figure need at least 3 hours for big ones
		dir.out.ensemble.db <- dir.out
		tfile <- file.path(dir.out.ensemble.db, paste("dbEnsemble_",sub(pattern="_Mean", replacement="", Table),".db",sep=""))
		conEnsembleDB <- dbConnect(drv, dbname=tfile)
		
		nfiles <- 0
		#Grab x rows at a time
		ensembleCollectSize <- 500
		SQL <- paste("SELECT MAX(P_id) FROM ",Table,";",sep="")
		maxP_id <- as.integer(dbGetQuery(con,SQL))
		maxRun_id <- (maxP_id/scenario_No)
		
		for(j in 1:length(ensemble.families)) {
			EnsembleTimeStop <- Sys.time() - t.overall
			units(EnsembleTimeStop) <- "secs"
			EnsembleTimeStop <- as.double(EnsembleTimeStop)
			if((EnsembleTimeStop > (MaxRunDurationTime-1*60))) {#figure need at least 4 hours for a ensemble
				break
			}
			
			print(paste("     Ensemble ",ensemble.families[j]," started at ",EnsembleTime <- Sys.time(),sep=""))
			
			outputs <- gsub(pattern=".csv", replacement="",basename(ensembles.maker$outputs[which(Tables==Table),j,,]))
			dim(outputs) <- c(3,3)
			ensemble.family=ensemble.families[j]
			#########################
			for(i in seq(1,maxRun_id,ensembleCollectSize)) {
				ptimes<-numeric(5)
				start <- (i-1)*scenario_No+1
				stop <- (min(i+ensembleCollectSize-1,maxRun_id)-1)*scenario_No+scenario_No
				
				MeanReadTime<-Sys.time()
				dataScen.Mean <- read.scenarios(Table=Table,start=start,stop=stop, ensemble.family=ensemble.family, export.header=TRUE)
				temp2<-Sys.time() - MeanReadTime
				units(temp2) <- "secs"
				temp2 <- as.double(temp2)
				ptimes[1] <- temp2
				#print(paste("          Read Mean Data ended after ",temp2," seconds.",sep=""))
				
				Table <- sub(pattern="Mean", replacement="SD", Table)
				
				SDReadTime<-Sys.time()
				dataScen.SD <- read.scenarios(Table=Table,start=start,stop=stop, ensemble.family=ensemble.family, export.header=FALSE)			
				temp2<-Sys.time() - SDReadTime
				units(temp2) <- "secs"
				temp2 <- as.double(temp2)
				ptimes[2] <- temp2
				#print(paste("          Read SD Data ended after ",temp2," seconds.",sep=""))
				
				Table <- sub(pattern="SD", replacement="Mean", Table)
	
				#get ensembles for non-SD file
				calcTime <- Sys.time()
				dataEns.Mean <- calc.ensembles(dat=dataScen.Mean$data.scen, elevels=ensemble.levels)
				temp2<-Sys.time() - calcTime
				units(temp2) <- "secs"
				temp2 <- as.double(temp2)
				ptimes[3] <- temp2 
				#print(paste("          Calc Ensembles ended after ",temp2," seconds.",sep=""))

				#Lookup SD values from scenarios based on ranks determined from taking ensembles of the means
				rankTime <- Sys.time()
				if(length(dim(dataEns.Mean[(length(ensemble.levels) + 1):(2*length(ensemble.levels)),,])) == 2) {
					lookup <- aperm(dataEns.Mean[(length(ensemble.levels) + 1):(2*length(ensemble.levels)),,], perm=c(2,1))
					make <- array(c(lookup, dataScen.SD$data.scen), dim=c(nrow(lookup), length(ensemble.levels) + dim(dataScen.SD$data.scen)[3]))
					dataEns.SD <- apply(make, MARGIN=1, FUN=function(lookANDscen) lookANDscen[(length(ensemble.levels)+1):dim(make)[2]][lookANDscen[1:length(ensemble.levels)]])
					dimnames(dataEns.SD)[2] <- dimnames(dataScen.SD$data.scen)[2]
				} else {
					lookup <- aperm(dataEns.Mean[(length(ensemble.levels) + 1):(2*length(ensemble.levels)),,], perm=c(2,3,1))
					make <- array(c(lookup, dataScen.SD$data.scen), dim=c(nrow(lookup), ncol(lookup), length(ensemble.levels) + dim(dataScen.SD$data.scen)[3]))
					dataEns.SD <- apply(make, MARGIN=c(1,2), FUN=function(lookANDscen) lookANDscen[(length(ensemble.levels)+1):dim(make)[3]][lookANDscen[1:length(ensemble.levels)]])
					dimnames(dataEns.SD)[3] <- dimnames(dataScen.SD$data.scen)[2]
				}
				temp2<-Sys.time() - rankTime
				units(temp2) <- "secs"
				temp2 <- as.double(temp2)
				ptimes[4] <- temp2
				#print(paste("          Rank Lookup ended after ",temp2," seconds.",sep=""))
				
				
				#write ensemble files
				writeTime <- Sys.time()
				for(k in 1:length(ensemble.levels)){
					nfiles <- nfiles + doWrite(dat=dataEns.Mean[k,,], headerInfo=dataScen.Mean$headerInfo, elevel=ensemble.levels[k], outfile=outputs[k, 1])
					if(length(dim(dataEns.Mean[(length(ensemble.levels) + 1):(2*length(ensemble.levels)),,])) == 2) {
						nfiles <- nfiles + doWrite(dat=dataEns.SD[k,], headerInfo=dataScen.Mean$headerInfo, elevel=ensemble.levels[k], outfile=outputs[k, 2])
					} else {
						nfiles <- nfiles + doWrite(dat=dataEns.SD[k,,], headerInfo=dataScen.Mean$headerInfo, elevel=ensemble.levels[k], outfile=outputs[k, 2])
					}
					if(save.scenario.ranks) nfiles <- nfiles + doWrite(dat=dataEns.Mean[length(ensemble.levels) + k,,], headerInfo=dataScen.Mean$headerInfo, elevel=ensemble.levels[k], outfile=outputs[k, 3])
				}
				temp2<-Sys.time() - writeTime
				units(temp2) <- "secs"
				temp2 <- as.double(temp2)
				ptimes[5] <- temp2
				print(paste("          ",i,":",i+ensembleCollectSize-1," of ",maxRun_id," done. Mean Read: ",format(ptimes[1]/sum(ptimes)*100),"%. SD Read: ",format(ptimes[2]/sum(ptimes)*100),"%. Calc: ",format(ptimes[3]/sum(ptimes)*100),"%. Rank: ",format(ptimes[4]/sum(ptimes)*100),"%. Write: ",format(ptimes[5]/sum(ptimes)*100),"%.",sep=""))
				#print(paste("          Write ended after ",temp2," seconds.",sep=""))
			}
			#########################
			temp2<-Sys.time() - EnsembleTime
			units(temp2) <- "secs"
			temp2 <- as.double(temp2)
			print(paste("     ended at ",Sys.time(),", after ",temp2," seconds.",sep=""))
		}
	}
	
	temp2<-Sys.time() - TableTime
	units(temp2) <- "secs"
	temp2 <- as.double(temp2)
	print(paste("ended at ",Sys.time(),", after ",temp2," seconds.",sep=""))
	
	return(nfiles)
}
		
		library(RSQLite,quietly = TRUE)
		drv <- dbDriver("SQLite")
		tfile <- file.path(dir.out, "dbTables.db")
		con <- dbConnect(drv, dbname = tfile)
	
		Tables <- dbListTables(con) #get a list of tables
		Tables <- Tables[-grep(pattern="SD", Tables)]
		
		if(parallel_runs){
			#call the simulations depending on parallel backend
			if(identical(parallel_backend, "mpi")) {
				workersN <- (mpi.comm.size() - 1)
				list.export <- c("Tables","save.scenario.ranks","ensemble.levels","calc.ensembles","scenario_No","MaxRunDurationTime", "collect_EnsembleFromScenarios","dir.out","ensembles.maker","ensemble.families","t.overall")
				exportObjects(list.export)
		
				mpi.bcast.cmd(library(RSQLite,quietly = TRUE))
				mpi.bcast.cmd(drv<-dbDriver("SQLite"))
		
		
				ensembles.completed <- mpi.applyLB(x=Tables, fun=collect_EnsembleFromScenarios)
			}
		}

if(ensembles.completed != length(Tables)*ifelse(save.scenario.ranks, 3, 2)*length(ensemble.families)*length(ensemble.levels)) print("SWSF calculates ensembles: something went wrong with ensemble output.")



library(RSQLite,quietly = TRUE)
drv <- dbDriver("SQLite")
dbFiles <- list.files(path="./",pattern="dbEnsemble")

for(i in 1:length(dbFiles)) {#length(dbFiles)
	dbFile <- dbFiles[i]
	con <- dbConnect(drv, dbname = dbFile)
	
	dbEnsembleGroupName <- sub(pattern="dbEnsemble_Aggregation_", replacement="", x=dbFile)
	dbEnsembleGroupName <- sub(pattern="Seasons_DailyValues_", replacement="", x=dbEnsembleGroupName)
	dbEnsembleGroupName <- sub(pattern=".db",replacement = "", x=dbEnsembleGroupName)
	Tables <- dbListTables(con)
	if(!any(grepl(pattern=dbEnsembleGroupName, Tables)) & (dbEnsembleGroupName!="Overall")) {
		wrongName<-strsplit(Tables[1],split="_")[[1]][4]
		for(j in 1:length(Tables)) {
			dbGetQuery(con, paste("ALTER TABLE ", Tables[j], " RENAME TO ", sub(pattern=wrongName, replacement=dbEnsembleGroupName,Tables[j]),";",sep=""))
		}
		print(paste("Tables in ", dbFile, " had ",wrongName, ". Renamed to ", dbEnsembleGroupName, sep=" "))
	} else {
		print(paste("Names are correct for database", dbFile))
	}
}



