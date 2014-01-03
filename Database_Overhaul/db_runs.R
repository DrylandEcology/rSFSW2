# TODO: Add comment
# 
# Author: Ryan Murphy
#
#	This generates the runs table. The P_id from runs table will correspond to each row in the data tables.
#	It also generates the header view. This is a virtual table that shows the proper header generated from the tables, not data tables.
#
#	Tables Generated
#		* runs
#		* run_labels
#		* scenario_labels
#	Views Generated
#		* header

###############################################################################

###################FOR TESTING#################################################
#library(RSQLite)
#drv <- dbDriver("SQLite")
#con <- dbConnect(drv, dbname = "test.db")
#
#res<-dbSendQuery(con,"PRAGMA foreign_keys = ON;")
#fetch(res)
#dbClearResult(res)
#
#dir.in<-"/home/ryan/Documents/Work/1_PC_TempDry_Simulations_Prj04_r1_Rsoilwat/1_Data_SWInput"
###################END TESTING################################################

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
dbGetQuery(con, "CREATE TABLE runs(P_id INTEGER PRIMARY KEY, label_id INTEGER NOT NULL, site_id INTEGER NOT NULL, treatment_id INTEGER NOT NULL, scenario_id INTEGER NOT NULL, FOREIGN KEY(label_id) REFERENCES run_labels(id), FOREIGN KEY(site_id) REFERENCES sites(id), FOREIGN KEY(treatment_id) REFERENCES treatments(id), FOREIGN KEY(scenario_id) REFERENCES scenario_labels(id));")
db_runs <- data.frame(matrix(data=0, nrow=runsN.todo*scenario_No, ncol=5,dimnames=list(NULL,c("P_id","label_id","site_id","treatment_id","scenario_id"))))

db_runs$P_id <- 1:nrow(db_runs)
db_runs$scenario_id <- rep(1:scenario_No, times=runsN.todo)

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

dbGetPreparedQuery(con, "INSERT INTO runs VALUES(:P_id, :label_id, :site_id, :treatment_id, :scenario_id);", bind.data=db_runs)

##################################################

################CREATE VIEW########################
sites_columns <- colnames(SWRunInformation[Index_RunInformation])
sites_columns<-sub("ID","site_id",sites_columns)
sites_columns<-sub("WeatherFolder", "WeatherFolder_id",sites_columns)
header_columns<-c("runs.P_id","run_labels.label", paste("sites",sites_columns,sep=".",collapse = ", "), if(useExperimentals) "experimental_labels.label AS Experimental_Label", paste("treatments",colnames(db_combined_exp_treatments)[-(1:2)], sep=".", collapse=", "), "simulation_years.StartYear", "simulation_years.simulationStartYear AS SimStartYear", "simulation_years.EndYear", "scenario_labels.label AS Scenario")
header_columns<-paste(header_columns,collapse = ", ")

dbGetQuery(con, paste("CREATE VIEW header AS SELECT ",header_columns, " FROM runs, run_labels, sites, ", if(useExperimentals) "experimental_labels, ","treatments, scenario_labels, simulation_years WHERE runs.label_id=run_labels.id AND runs.site_id=sites.id AND runs.treatment_id=treatments.id AND runs.scenario_id=scenario_labels.id AND ", if(useExperimentals) "treatments.experimental_id=experimental_labels.id AND ","treatments.simulation_years_id=simulation_years.id;",sep=""))
##################################################


