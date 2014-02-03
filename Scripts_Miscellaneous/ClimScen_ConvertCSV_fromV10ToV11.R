##This will update the ClimateScenarios_Values v10 to v11##
##Takes TempC which represents the mean and changes to TempC_min TempC_max

#Location of the file to change
inputFileChange <-"/Users/drschlaep/Documents/drschlaepfer/2_Research/200907_UofWyoming_PostDoc/Projects_My/Product_PowellCenter/6_Projects_Year1/Prj06_VegetationBoundary/1_PC_TempDry_Simulations_Prj06_r2mini_NewExternal/1_Data_SWInput/datafiles/SWRuns_InputData_ClimateScenarios_Change_v10.csv"
inputFileValue<-"/Users/drschlaep/Documents/drschlaepfer/2_Research/200907_UofWyoming_PostDoc/Projects_My/Product_PowellCenter/6_Projects_Year1/Prj06_VegetationBoundary/1_PC_TempDry_Simulations_Prj06_r2mini_NewExternal/1_Data_SWInput/datafiles/SWRuns_InputData_ClimateScenarios_Values_SRESA2andSRESB1_v10.csv"

#output Location
outFileChange<-sub(pattern="v10.",replacement = "v11.",inputFileChange)
outFileValue<-sub(pattern="v10.",replacement = "v11.",inputFileValue)

sw_input_climscen_use <- tryCatch(read.csv(temp <- file.path(inputFileChange), nrows=1, stringsAsFactors=F),error=function(e) { print("datafile.climatescenarios: Bad Path"); print(e)})
sw_input_climscen <- read.csv(temp, skip=1)
colnames(sw_input_climscen) <- colnames(sw_input_climscen_use)

maxSc <- max(temp<-as.numeric(numberString<-sub(pattern="[PPTfactordeltaTempC]+_m[1234567890]+_sc",replacement = "", x=colnames(sw_input_climscen_use)[-1])))
width <- max(nchar(numberString))
minSc <- min(temp)
#go accross the scenarios
for(sc in minSc:maxSc) {
	start<-grep(pattern=paste("deltaTempC_m1_sc",formatC(sc, width=width, format="d", flag="0"),sep=""),x=colnames(sw_input_climscen_use))
	stop<-start+11
	useValues<-sw_input_climscen_use[start:stop]
	columnNames<-c(sub(pattern="TempC_",replacement = "TempC_min_",x=colnames(sw_input_climscen_use[start:stop])), sub(pattern="TempC_",replacement = "TempC_max_",x=colnames(sw_input_climscen_use[start:stop])))
	replacement<-cbind(useValues,useValues)
	colnames(replacement) <- columnNames
	if(stop!=ncol(sw_input_climscen_use)) {
		sw_input_climscen_use <- cbind(sw_input_climscen_use[,1:(start-1)],replacement,sw_input_climscen_use[,(stop+1):ncol(sw_input_climscen_use)])
	} else {
		sw_input_climscen_use <- cbind(sw_input_climscen_use[,1:(start-1)],replacement)
	}
	
	values<-sw_input_climscen[start:stop]
	replacement<-cbind(values,values)
	colnames(replacement) <- columnNames
	if(stop!=ncol(sw_input_climscen)) {
		sw_input_climscen <- cbind(sw_input_climscen[,1:(start-1)],replacement,sw_input_climscen[,(stop+1):ncol(sw_input_climscen)])
	} else {
		sw_input_climscen <- cbind(sw_input_climscen[,1:(start-1)],replacement)
	}
}
newFileData<-rbind(sw_input_climscen_use, sw_input_climscen)
write.csv(newFileData, file=file.path(outFileChange), row.names=FALSE)

############################
############################
############################

sw_input_climscen_values_use <- tryCatch(read.csv(temp <- file.path(inputFileValue), nrows=1,stringsAsFactors = FALSE),error=function(e) { print("datafile.climatescenarios_values: Bad Path"); print(e)})
sw_input_climscen_values <- read.csv(temp, skip=1)
colnames(sw_input_climscen_values) <- colnames(sw_input_climscen_values_use)

maxSc <- max(temp<-as.numeric(numberString<-sub(pattern="[PPTmTempC]+_m[1234567890]+_sc",replacement = "", x=colnames(sw_input_climscen_values_use)[-1])))
width <- max(nchar(numberString))
minSc <- min(temp)
#go accross the scenarios
for(sc in minSc:maxSc) {
	start<-grep(pattern=paste("TempC_m1_sc",formatC(sc, width=width, format="d", flag="0"),sep=""),x=colnames(sw_input_climscen_values_use))
	stop<-start+11
	useValues<-sw_input_climscen_values_use[start:stop]
	columnNames<-c(sub(pattern="TempC_",replacement = "TempC_min_",x=colnames(sw_input_climscen_values_use[start:stop])), sub(pattern="TempC_",replacement = "TempC_max_",x=colnames(sw_input_climscen_values_use[start:stop])))
	replacement<-cbind(useValues,useValues)
	colnames(replacement) <- columnNames
	if(stop!=ncol(sw_input_climscen_values_use)) {
		sw_input_climscen_values_use <- cbind(sw_input_climscen_values_use[,1:(start-1)],replacement,sw_input_climscen_values_use[,(stop+1):ncol(sw_input_climscen_values)])
	} else {
		sw_input_climscen_values_use <- cbind(sw_input_climscen_values_use[,1:(start-1)],replacement)
	}
	
	values<-sw_input_climscen_values[start:stop]
	replacement<-cbind(values,values)
	colnames(replacement) <- columnNames
	if(stop!=ncol(sw_input_climscen_values)) {
		sw_input_climscen_values <- cbind(sw_input_climscen_values[,1:(start-1)],replacement,sw_input_climscen_values[,(stop+1):ncol(sw_input_climscen_values)])
	} else {
		sw_input_climscen_values <- cbind(sw_input_climscen_values[,1:(start-1)],replacement)
	}
}
newFileData<-rbind(sw_input_climscen_values_use, sw_input_climscen_values)
write.csv(newFileData, file=file.path(outFileValue), row.names=FALSE)
