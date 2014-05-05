workersN <- num_cores

swDataFromFiles <- sw_inputDataFromFiles(dir=dir.sw.in,files.in=swFilesIn) #This acts for the basis for all runs.
filebasename <- basename(swFiles_WeatherPrefix(swDataFromFiles))

#i = i_sim: consecutive number of seq.todo, i.e., counting the simulation runs
#i_xxx = the i_tr-row of xxx for the i-th simulation run; if trowExperimentals > 0 then these will eventually be repeated, and below replaced with experimental values
#i_exp = the row of sw_input_experimentals for the i-th simulation run
#P_id is a unique id number for each scenario in each run
i_sim <- 1
i_tr <- seq.tr[(i_sim-1) %% runs + 1]

drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname=name.OutputDB)
if(getCurrentWeatherDataFromDatabase) conWeather <- dbConnect(drv, dbname=dbWeatherDataFile)

#weather folder name and structure
if(GriddedDailyWeatherFromMaurer2002_NorthAmerica & !any(create_treatments == "LookupWeatherFolder")){ #obtain external weather information that needs to be executed for each run
  dirname.sw.runs.weather <- paste("data", format(28.8125+round((SWRunInformation[i_tr,]$Y_WGS84-28.8125)/0.125,0)*0.125, nsmall=4), format(28.8125+round((SWRunInformation[i_tr,]$X_WGS84-28.8125)/0.125,0)*0.125, nsmall=4), sep="_")
  sw_weatherList <- ExtractGriddedDailyWeatherFromMaurer2002_NorthAmerica(cellname=dirname.sw.runs.weather,startYear=ifelse(any(create_treatments=="YearStart"), sw_input_treatments[i_tr,]$YearStart, simstartyr), endYear=ifelse(any(create_treatments=="YearEnd"), sw_input_treatments[i_tr,]$YearEnd, endyr))
  if(is.null(sw_weatherList)) stop("ExtractGriddedDailyWeatherFromMaurer2002_NorthAmerica failed")
} else {
  sw_weatherList <- NULL
#  temp <- dbGetQuery(con, paste("SELECT WeatherFolder FROM header WHERE P_id=",((i_sim-1)*scenario_No+1)))
#  if(WeatherDataFromDatabase) {
#    sw_weatherList <- onGetWeatherData_database(con=conWeather,weatherDirName=temp,startYear=ifelse(any(create_treatments=="YearStart"), sw_input_treatments[i_tr,]$YearStart, simstartyr), endYear=ifelse(any(create_treatments=="YearEnd"), sw_input_treatments[i_tr,]$YearEnd, endyr))
#  } else {
#    sw_weatherList <- onGetWeatherData_folders(LookupWeatherFolder=file.path(dir.sw.in.tr, "LookupWeatherFolder"),weatherDirName=temp,filebasename=filebasename,startYear=ifelse(any(create_treatments=="YearStart"), sw_input_treatments[i_tr,]$YearStart, simstartyr), endYear=ifelse(any(create_treatments=="YearEnd"), sw_input_treatments[i_tr,]$YearEnd, endyr))
#  }
}

nodeNumber <- 1

i <- i_sim
i_labels <- labels[i_tr]
i_SWRunInformation <- SWRunInformation[i_tr, ]
i_sw_input_soillayers <- sw_input_soillayers[i_tr, ]
i_sw_input_treatments <- sw_input_treatments[i_tr, ]
i_sw_input_cloud <- sw_input_cloud[i_tr, ]
i_sw_input_prod <- sw_input_prod[i_tr, ]
i_sw_input_site <- sw_input_site[i_tr, ]
i_sw_input_soils <- sw_input_soils[i_tr, ]
i_sw_input_weather <- sw_input_weather[i_tr, ]
i_sw_input_climscen <- sw_input_climscen[i_tr, ]
i_sw_input_climscen_values <- sw_input_climscen_values[i_tr, ]
i_sw_weatherList <- sw_weatherList


runs.completed <- length(seq.todo)
complete.aggregations <- TRUE

concats.completed <- length(seq.concats)
