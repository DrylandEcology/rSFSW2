# TODO: Add comment
# 
# Author: ryan
###############################################################################

sw_aet			<-1
sw_deepdrain	<-2
sw_estabs		<-3
sw_evsoil		<-4
sw_evapsurface	<-5
sw_hd			<-6
sw_inf_soil		<-7
sw_interception	<-8
sw_percolation	<-9
sw_pet			<-10
sw_precip		<-11
sw_runoff		<-12
sw_snow			<-13
sw_soiltemp		<-14
sw_surfaceWater	<-15
sw_swp			<-16
sw_swa			<-17
sw_swc			<-18
sw_temp			<-19
sw_transp		<-20
sw_vwc			<-21
sw_wetdays		<-22
sw_logfile		<-23
sw_yr			<-1
sw_mo			<-2
sw_wk			<-3
sw_dy			<-4


get_AET_yr <- function(sc){
	return(list(val=10 * runData[[sc]][[sw_aet]][[sw_yr]][simTime$index.useyr, 2]))
}

for(itests in 1:100) {

	for (sc in Exclude_ClimateAmbient:scenario_No){
		if(print.debug) print(paste("Start of SoilWat execution for scenario:", sc))
		if(!exists("use_janus")){
			runData[[sc]]<-tryCatch({ sw_exec(data=swRunScenariosData[[sc]],weatherList=i_sw_weatherList, echo=F, quiet=F,colNames=saveSoilWatInputOutput)
					}, warning = function(w) {
						print("------------Warning----------")
						print(w)
						print("-----------------------------")
						#assign("todo$aggregate", FALSE, pos=2)
						#mpi.send.Robj(i,0,4)
					}, error = function(e) {
						print("-------------Error-----------")
						print(e)
						print("-----------------------------")
						if(parallel_runs && identical(parallel_backend,"mpi"))
							mpi.send.Robj(i,0,4)
						return(NA)
					})
			if(isTRUE(is.na(runData[[sc]])))
				todo$aggregate <- FALSE
		} else {
			try(system(paste(exec_c_prefix, "./", shQuote(sw), " -f ", filesin, " -e -q", sep="")))
		}
	}

	if(!exists("AET.yr")) AET.yr <- get_AET_yr(1)
	print(paste(mean(AET.yr$val),sd(AET.yr$val)))
	rm(AET.yr)
}