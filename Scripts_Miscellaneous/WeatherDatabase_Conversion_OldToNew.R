library(Rsoilwat)
drv <- dbDriver("SQLite")

# Get Some data from the old database
dbW_setConnection(dbFilePath="/media/Storage/work/dbWeatherData_GTD_Current.sqlite")
odata<-dbW_getWeatherData(Site_id=1,Scenario="Current")

# Create the new database and create the table
con <- dbConnect(drv,"/media/Storage/work/dbWeatherData_GTD_NEWTEST.sqlite3")
dbGetQuery(con, "CREATE TABLE \"WeatherData\" (\"Site_id\" INT NOT NULL, \"Scenario\" INT NOT NULL, \"StartYear\" INT NOT NULL, \"EndYear\" INT NOT NULL, \"data\" BLOB, PRIMARY KEY (\"Site_id\", \"Scenario\"));")

# Format and compress the data write to database
string <- character(length=length(odata))
for(i in 1:length(odata)) {
	zz <- textConnection("dataString","w")
	write.table(x=odata[[i]]@data[,2:4], file=zz,col.names=FALSE,sep=",",row.names=FALSE)
	close(zz)
	string[i] <-paste(dataString,collapse="\n")
}
string<-paste(string,collapse=";")
data_blob <- paste0("x'",paste0(memCompress(string,type="gzip"),collapse = ""),"'",sep="")
Site_id <- 1
scenarioID <- 1
StartYear <- 1979
EndYear <- 2010
dbGetQuery(con, paste("INSERT INTO WeatherData (Site_id, Scenario, StartYear, EndYear, data) VALUES (",Site_id,",",scenarioID,",",StartYear,",",EndYear,",",data_blob,");",sep=""))

# Example of New way to read the data
result <- dbGetQuery(con,"SELECT StartYear,EndYear,data FROM weatherdata WHERE Site_id=1 AND Scenario=1;")
data <- strsplit(rawToChar(memDecompress(result$data[[1]], type="gzip")), ";")[[1]]
years <- seq(from=result$StartYear, to=result$EndYear)

weatherData <- list()
for(i in 1:length(years)) {
	ydata <- read.csv(textConnection(data[i]),header=FALSE,sep=",",stringsAsFactors=FALSE)
	ydata <- as.matrix(cbind(seq(from=1,to=dim(ydata)[1]),ydata))
	colnames(ydata) <- c("DOY","Tmax_C","Tmin_C","PPT_cm")
	weatherData[[i]] <- new("swWeatherData",year=years[i],data=ydata)
}
names(weatherData) <- years