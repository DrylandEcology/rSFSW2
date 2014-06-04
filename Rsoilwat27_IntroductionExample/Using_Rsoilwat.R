library(Rsoilwat)
packageVersion("Rsoilwat")

##### Some of the available functions
#	- sw_inputData
#	- sw_inputDataFromFiles
#	- sw_exec
#	- onGetWeatherData_folders
##### 


#obtain example input data
swIn1 <- sw_inputData()

#Or read input data from files
dir.sw <- "/Users/schlaepfer/Downloads/temp_work/Dave_Rsoilwat/swrun"
swIn2 <- sw_inputDataFromFiles(dir=dir.sw, file.in="files_v27.in")

str(swIn2, max.level=2)

#Read in forcing weather data
weatherList <- getWeatherData_folders(LookupWeatherFolder=dir.sw, weatherDirName="Weather_Test", 
    filebasename="weath", startYear=1979, endYear=2010)
str(weatherList, max.level=1)

#Execute a SoilWat run
swOut <- sw_exec(data=swIn2, weatherList=weatherList, dir="", file.in="files_v27.in", echo=FALSE, quiet=FALSE, colNames=TRUE)

str(swOut, max.level=1)
str(swOut$sw_pot, max.level=1)
matplot(x=swOut$vwc$yr[,1], swOut$vwc$yr[, -1], ylim=c(0, 0.4), type="l")

#Change input values, e.g.,
swYears_StartYear(swIn2) <- 1979
swWeather_FirstYearHistorical(swIn2) <- 1979
swYears_EndYear(swIn2) <- 2010

swProd_Composition(swIn2) <- c(0.4, 0.6, 0)	

#Complete list of accessing/assigning input values
swClear
swCloud_Humidity
swCloud_SkyCover
swCloud_SnowDensity
swCloud_Transmissivity
swCloud_WindSpeed
swEstab_useEstab
swFiles_Cloud
swFiles_Estab
swFiles_filesIn
swFiles_LogFile
swFiles_MarkovCov
swFiles_MarkovProbs
swFiles_Output
swFiles_OutputPrefix
swFiles_Prod
swFiles_ProjDir
swFiles_SiteParams
swFiles_Soils
swFiles_SWCsetup
swFiles_WeatherPrefix
swFiles_WeatherSetup
swFiles_Years
swMarkov_Conv
swMarkov_Prob
swOut
swOUT_OutputSeparator
swOUT_TimeStep
swProd_Albedo
swProd_CanopyHeight
swProd_Composition
swProd_Cover_stcr
swProd_CritSoilWaterPotential
swProd_Es_param_limit
swProd_EsTpartitioning_param
swProd_HydrRedstro
swProd_HydrRedstro_use
swProd_LitterInterParam
swProd_MonProd_grass
swProd_MonProd_shrub
swProd_MonProd_tree
swProd_Shade
swProd_VegInterParam
swSite_DrainageCoefficient
swSite_EvapCoefficients
swSite_IntrinsicSiteParams
swSite_ModelCoefficients
swSite_ModelFlags
swSite_SnowSimulationParams
swSite_SoilTemperatureConsts
swSite_SoilTemperatureFlag
swSite_SWClimits
swSite_TranspCoefficients
swSite_TranspirationRegions
swSoils_Layers
swSWC_FirstYear
swSWC_HistoricData
swSWC_HistoricList
swSWC_Method
swSWC_prefix
swSWC_use
swWeather_DaysRunningAverage
swWeather_FirstYearHistorical
swWeather_MonScalingParams
swWeather_pct_SnowDrift
swWeather_pct_SnowRunoff
swWeather_UseMarkov
swWeather_UseSnow
swYears_EDOEY
swYears_EndYear
swYears_FDOFY
swYears_isNorth
swYears_StartYear


