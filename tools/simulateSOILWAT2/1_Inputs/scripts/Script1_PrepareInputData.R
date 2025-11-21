# last update: 20250320_SoilClimateDynamics_SOILWAT2_simulations

#------ . ------
#------ SOILWAT2 simulations at YOUR-EXPERIMENT ------
#
#------ Prepare SOILWAT2 simulation runs ------
#
# * Sites: ... e.g., 164 sites
# * Years: ... e.g., 1980-2023
# * Weather: ... e.g., dayMet (1-km resolution)
# * Soils: ... e.g., USDA-NRCS Soil Properties of the USA (SOLUS100) v1 (100-m resolution)
# * Vegetation: ..., e.g., obs
# * Topography: ..., e.g., NED
#
#
#------ . ------
#------ Versions ------
# * version1: description
# * version2: description
versions <- c("run0-20240422", "run1-20240604")
versions_rSWSF <- c("20240422", "20240604")
#------ . ------

#----- Dependencies ------
stopifnot(
  getNamespaceVersion("rSW2data") >= "0.1.5",
  getNamespaceVersion("rSW2exter") >= "0.3.2",
  getNamespaceVersion("rSOILWAT2") >= "6.4.0",
  getNamespaceVersion("rSFSW2") >= "5.0.0"
)
#------ . ------


#------ Settings ------
vsel <- 1L # select version to use now, i.e., index of `versions`

# Completion dates for run1-20240422
tasks <- list(
  dbW = TRUE, # completed:
  dbFuture = FALSE, # completed: NA
  topo = TRUE, # completed:
  soils = TRUE, # completed:
  veg = TRUE # completed:
)

beVerbose <- TRUE

doFigures <- TRUE


#------ Simulation experiment ------
prjTag <- "yourExperiment"
simYears <- 1980:2023
prjCRS <- "WGS84"
mapCRS <- 5070

expVegTrt <- list(values = NULL, tag = NULL)

varsSimLabel <- "index"
varsCoords <- c("Longitude_WGS84", "Latitude_WGS84")
varsSoilCoords <- c("X_WGS84", "Y_WGS84")
varsVegID <- NULL
varsSoilID <- NULL # e.g., "pedlabsampnum" for KSSL

simSoilLayers <- c(5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 201)

southAspect <- 0


#------ Input data sources ------
#' @param weather_sources One or two data sources of daily weather.
#' The first value takes priority; the second (if any) completes.
#' Possible values: `"DayMet"`, `"gridMET"`, `"obs"`
weather_sources <- "gridMET"

#' @param future_sources NULL or one data source.
#' Possible values: NULL, `"MACAv2METDATA"`
future_sources <- NULL

#' @param topo_sources One or two data sources of topographic information.
#' The first value takes priority; the second (if any) completes.
#' Possible values: `"obs"`, `"NED1"`
topo_sources <- c("NED1", "obs")

#' @param soil_sources One or two data sources of soil information.
#' The first value takes priority; the second (if any) completes.
#' Possible values: `"obs"`, `"SOLUS100"`, `"KSSL"`, `"SDA"`
soil_sources <- c("KSSL", "SOLUS100")

#' @param veg_sources One or two data sources of vegetation inputs.
#' The first value takes priority; the second (if any) completes.
#' Possible values: `"obs"`, `"climVeg"`
veg_sources <- "climVeg"


weather_actions <- list(
  #' @param useStridedMACA A logical value.
  #' TRUE, download strided; FALSE, download aggregated files.
  useStridedMACA = FALSE,

  #' @param correctWeatherValues A logical value.
  #' TRUE, correct problematic weather values
  correctWeatherValues = FALSE,

  #' @param imputeMissingObsWeather A logical value.
  #' TRUE, impute missing weather from the `"obs"` weather source using
  #' values extracted from `"DayMet"`
  imputeMissingObsWeather = TRUE
)

topo_actions <- list(
  #' @param doTopographicPosition A logical value.
  #'    * TRUE, process elevation, slope and aspect.
  #'    * FALSE, process elevation only.
  doTopographicPosition = TRUE,

  #' @param use_FedData_for_topo A logical value.
  #'    * TRUE, use the `FedData` R package to download `"NED"` tiles and
  #'      mosaic them together (slow; not recommended).
  #'    * FALSE, use custom functions to download `"NED"` tiles and create
  #'      a `"vrt"` (fast; recommended).
  use_FedData_for_topo = FALSE,

  #' @param methodTopoDownload A character string.
  #'    * `"bbox"` downloads `"NED"` for the bounding box of simulated sites
  #'    * `"sparse"` attempts to download ony those `"NED"` tiles that are
  #'      actually occupied by a simulated site
  methodTopoDownload = "sparse"
)

soil_actions <- list(
  #' @param buffer_m An integer value. Buffer distance `[m]` across which
  #' to average `"SOLUS100` if soil information at a point location failed.
  buffer_m = 500,

  #' @param whatKSSL A character string (or NULL).
  #' Column name that identifies `"KSSL"` soil pedons.
  whatKSSL = "pedlabsampnum",

  #' @param combineSoilSource2 Approach to combine soil information from
  #' second source with information from first source.
  #' Possible values:
  #'     * `"missingSoil1"`, source 2 is used where source 1 provides no values.
  #'     * `"failedSoil1"`, source 2 is used where source 1 fails checks.
  #'     * `"complementSoil1"`, source 2 is used for incomplete sites,
  #'       soil layers, and variables
  #'       (e.g., complete partially observed field data).
  combineSoilSource2 = "failedSoil1",

  #' @param varsUseSoil2IfValueMissingInSoil1 A character string (or NULL).
  #' Column name(s) of soil properties that use values from soil source 2
  #' if the corresponding value is missing in soil source 1
  #' (particularly if `combineSoilSource2` is not `"complementSoil1"`).
  varsUseSoil2IfValueMissingInSoil1 = if (
    identical(soil_sources[[1L]], "KSSL")
  ) {
    c("Matricd", "GravelContent", "SOM")
  },

  #' @param hasObsSoilDepth A logical value. Soil source that determines
  #' soil depth in the final combined soil.
  #'    * TRUE, soil depth from the `"obs"` soil source is used.
  #'    * FALSE, soil depth from the other (not `"obs"`) soil source is used.
  hasObsSoilDepth = FALSE,

  #' @param imputeLOCF A logical value.
  #' Impute missing soil values per location by shallow-depth value carried
  #' deeper (in analogy to `LOCF`), but do not impute missing values
  #' in the shallowest horizon/layer. See [rSW2data::impute_soils()].
  imputeLOCF = TRUE,

  #' @param allowAllSandClayOrSilt A logical value.
  #' Check of soil texture allows a 100% content of sand, clay, or silt.
  allowAllSandClayOrSilt = TRUE
)

veg_actions <- list(
  #' @param obsBiomassValidAllMonths A logical value.
  #' Biomass related observed inputs are considered to apply to all months
  #' even if input source provides a single value.
  obsBiomassValidAllMonths = FALSE,

  #' @param doBasalAreaPIPO A logical value.
  #' TRUE, then estimate tree inputs from basal area relationships
  #' (but don't overwrite observed variables).
  doBasalAreaPIPO = FALSE,

  #' @param varBasalAreaPIPO A character string (or NULL).
  #' basal area variable `[m2 ha-1]`, e.g., ExpTrt_BA
  varBasalAreaPIPO = NULL,

  #' @param doFixedTreeBradford2014 A logical value.
  #' TRUE, then fix tree %live and tree LAIconv to values derived with
  #' equations from Bradford et al. 2014 Ecosystems
  #' (but don't overwrite observed variables)
  doFixedTreeBradford2014 = FALSE,

  #' @param hasCombinedLitter A logical value.
  #' TRUE, then litter `[g m-2]` provided by `"obs"` vegetation source
  #' represents the total litter (across all plant functional types).
  hasCombinedLitter = FALSE,

  #' @param varCombinedLitter A character string (or NULL).
  #' The column name of the combined litter variable, e.g., `"ExpTrt_Litter"`.
  varCombinedLitter = NULL,

  #' @param estimateLiveBiomassFraction A logical value.
  #' TRUE, then estimate `"FractionLive"` from `"BiomassLive"` and `"Biomass"`.
  estimateLiveBiomassFraction = FALSE,

  #' @param doScaleNonTreeCoverToUnderstory A logical value.
  #' TRUE, then scale non-tree cover to sum to `(1 - treeCover)`.
  doScaleNonTreeCoverToUnderstory = FALSE,

  #' @param doScaleCover A logical value. TRUE, then scale cover to sum to 1.
  doScaleCover = TRUE,

  #' @param isBiomassOnTheGround A logical value.
  #' TRUE, then biomass from the `"obs"` vegetation source reflects values as
  #' measured on the ground. The code will scale to reflect values at 100% cover
  isBiomassOnTheGround = FALSE,

  #' @param isLitterOnTheGround A logical value.
  #' TRUE, then litter from the `"obs"` vegetation source reflects values as
  #' measured on the ground. The code will scale to reflect values at 100% cover
  isLitterOnTheGround = FALSE,

  #' @param interpretPNVSucculents A character string.
  #' PNV estimates succulent cover, but simulations currently don't represent;
  #' assign succulent cover to one of the four implement types.
  interpretSucculentCoverPNV = "Shrub",

  #' @param monthScalePNVByObs An integer value.
  #' The month `[1-12]` when observed biomass was collected, i.e.,
  #' the code will scale PNV to match live biomass amount for grasses and forbs
  #' and total biomass for shrubs.
  #' A negative value will skip this step.
  monthScalePNVByObs = -1L
)


#------ Metadata of observations ------
metaObservedWeather <- if (any(weather_sources == "obs")) {
  list(
    date = list(yr = "Year", mon = "Month", day = "Date"),
    Tmax_C = list(var = "Ta.max.C", uc = function(x) x),
    Tmin_C = list(var = "Ta.min.C", uc = function(x) x),
    PPT_cm = list(var = "PPT.mm", uc = function(x) 0.1 * x),
    cloudCov_pct = list(var = "NA", uc = function(x) x),
    windSpeed_mPERs = list(var = "WS.m.s", uc = function(x) x),
    windSpeed_east_mPERs = list(var = "NA", uc = function(x) x),
    windSpeed_north_mPERs = list(var = "NA", uc = function(x) x),
    rHavg_pct = list(var = "RH.mean", uc = function(x) x),
    rHmax_pct = list(var = "NA", uc = function(x) x),
    rHmin_pct = list(var = "NA", uc = function(x) x),
    specHavg_pct = list(var = "NA", uc = function(x) x),
    Tdewpoint_C = list(var = "NA", uc = function(x) x),
    actVP_kPa = list(var = "NA", uc = function(x) x),
    shortWR = list(var = "Slr.MJ.m2.d", uc = function(x) x)
  )
}

metaObservedSoilProperties <- if (any(soil_sources == "obs")) {
  rbind(
    c(rSW2 = "Sand", exter = "SandFraction"),
    c(rSW2 = "Clay", exter = "ClayFraction")
  )
}

metaObservedVegetation <- if (any(veg_sources == "obs")) {
  rbind(
    c(rSW2 = "Composition_TreeFraction", exter = "Tree_FractionalCover"),
    c(rSW2 = "Composition_ShrubFraction", exter = "Shrub_FractionalCover"),
    c(rSW2 = "Composition_GrassFraction", exter = "Grass_FractionalCover"),
    c(rSW2 = "Composition_ForbFraction", exter = "Forb_FractionalCover")
  )
}


#------ Paths ------
dir_prj <- ".."
dir_script <- "."

dir_dataraw <- file.path(dir_prj, "data-raw")
stopifnot(dir.exists(dir_dataraw))

dir_data <- file.path(dir_prj, "data", versions[[vsel]])
dir.create(dir_data, recursive = TRUE, showWarnings = FALSE)

dir_R <- file.path(dir_prj, "R")
stopifnot(dir.exists(dir_R))

dir_results <- file.path(dir_prj, "results", versions[[vsel]])
dir.create(dir_results, recursive = TRUE, showWarnings = FALSE)

dir_figs <- file.path(dir_results, "figures")
dir.create(dir_figs, recursive = TRUE, showWarnings = FALSE)

dir_sim <- file.path(dir_prj, "..", "2_SOILWAT2_Simulations")
dir.create(dir_sim, recursive = TRUE, showWarnings = FALSE)


names(weather_sources) <- weather_sources


#------ . ------
#------ Load functions ------
matchMultipleVariables <- NULL
readInputFile <- NULL
create_xmain <- NULL
create_xsim <- NULL
plot_map <- NULL

listWeatherFolderNames <- NULL
setup_dbWeather <- NULL
download_gridMET <- NULL
pfun_get_DayMet <- NULL
observedWeatherData <- NULL
supplementDayMetWithgridMETWind <- NULL
metaMACAv2METDATA <- NULL
pfun_get_MACAv2METDATA <- NULL

getTopoNED1 <- NULL
combineTwoTopoDatasets <- NULL

createSoilsTemplate <- NULL
getSoilsFromSOLUS100 <- NULL
getSoilsFromSDA <- NULL
getSoilsFromKSSL <- NULL
copySoilColumns <- NULL
combineTwoSoils <- NULL

createVegetationTemplate <- NULL

source(file.path(dir_R, "Functions_InputDataPreparationSOILWAT2.R"))

if (isTRUE(veg_actions[["doBasalAreaPIPO"]])) {
  predictTreeInputsPIPO <- NULL
  tmp <- lapply(
    list.files(
      path = file.path(dir_dataraw, "Andrews+_2020_JAE", "R"),
      pattern = ".R$",
      full.names = TRUE
    ),
    source
  )
  stopifnot(!is.null(predictTreeInputsPIPO))
}


#------ . ------
#------ Create empty rSFSW2 project ------
fnames_dbW <- file.path(
  dir_sim,
  "0_WeatherDatabase",
  paste0(
    "dbWeatherData_",
    prjTag, "_", weather_sources, "_v", versions_rSWSF[[vsel]],
    ".sqlite3"
  )
)
names(fnames_dbW) <- weather_sources
dir.create(unique(dirname(fnames_dbW)), recursive = TRUE, showWarnings = FALSE)

if (!is.null(future_sources)) {
  fnames_dbFuture <- file.path(
    dir_sim,
    "0_WeatherDatabase",
    paste0(
      "dbWeatherData_",
      prjTag, "_", future_sources, "_v", versions_rSWSF[[vsel]],
      ".sqlite3"
    )
  )
  names(fnames_dbFuture) <- future_sources
  dir.create(
    unique(dirname(fnames_dbFuture)), recursive = TRUE, showWarnings = FALSE
  )
}

dir_rSFSW2 <- file.path(
  dir_sim,
  paste0(
    versions_rSWSF[[vsel]], "_",
    prjTag, "_SOILWAT2_simulations")
)
if (!dir.exists(dir_rSFSW2)) {
  rSFSW2::setup_rSFSW2_project_infrastructure(dir_prj = dir_rSFSW2)
}


#------ . ------
#------ Main inputs ------

fname_xsim1 <- file.path(dir_data, "xsim1.rds")

#--- ..* Sites/locations ------
if (file.exists(fname_xsim1)) {
  xsim <- readRDS(fname_xsim1)

} else {
  tmp <- switch(
    EXPR = versions[[vsel]],
    "run1-20240604" = file.path(
      "surveyCoordsTable-run1-20240604", "surveyCoordsTable-run1-20240604.csv"
    ),
    "run0-20240604" = file.path(
      "surveyCoordsTable-run0-20240422", "surveyCoordsTable.csv"
    ),
    stop("Version ", versions[[vsel]], " ist not implemented.")
  )

  xtmp <- readInputFile(file.path(dir_dataraw, tmp))

  xmain <- create_xmain(
    x = xtmp,
    varsCoords = varsCoords,
    varsKeep = unique(
      c(varsSimLabel, setdiff(colnames(xtmp), c(varsCoords, "fname_weather")))
    ),
    prjCRS = prjCRS
  )

  #--- ..* Simulation setup with "design treatments" ------
  xsim <- create_xsim(
    xmain = xmain,
    varsSimLabel = varsSimLabel,
    expVegTrt = expVegTrt
  )

  saveRDS(xsim, file = fname_xsim1)
}

Nsim <- nrow(xsim)



if (doFigures) {

  fname_fig_map_sites <- file.path(dir_figs, "Fig-map_sites.png")

  if (!file.exists(fname_fig_map_sites)) {
    plot_map(
      fname = fname_fig_map_sites,
      x = xsim,
      var_index = varsSimLabel,
      crs = mapCRS
    )
  }
}



#------ . ------
#------ Weather data ------
fname_xsim2 <- file.path(dir_data, "xsim2.rds")

if (any(!file.exists(fname_xsim2), isTRUE(tasks[["dbW"]]))) {

  #--- ..* Download gridMET (if requested) ------
  if (any(weather_sources == "gridMET")) {
    dir_gridmet <- file.path(dir_dataraw, "gridMET")

    fname_gridmet_nc <- download_gridMET(
      dir_gridmet = dir_gridmet,
      dir_gridmet_priority = "/Volumes/BookDuo_12TB/BigData/GIS/Data/Weather_Past/gridMET_4km_NA/YEARLY_GRIDS",
      dir_script = dir_script
    )

    dir_gridmet <- dirname(fname_gridmet_nc)

  } else {
    dir_gridmet <- NULL
    fname_gridmet_nc <- NULL
  }


  #--- ..* Identify/assign 'WeatherFolder' ------
  tmp_wfs <- listWeatherFolderNames(
    xsim = xsim,
    weather_sources = weather_sources,
    fname_gridmet_nc = fname_gridmet_nc
  )
}


if (file.exists(fname_xsim2)) {
  xsim <- readRDS(fname_xsim2)

} else {
  xsim[, "WeatherFolder"] <- tmp_wfs[[1L]][["cellID"]]

  saveRDS(xsim, file = fname_xsim2)
}


#--- ..* Create/populate weather database ------
if (isTRUE(tasks[["dbW"]])) {
  for (kw in seq_along(weather_sources)) {

    tasks_by_dbW <- setup_dbWeather(
      fdbWeather = fnames_dbW[[kw]],
      wfs = tmp_wfs[[kw]],
      weather_source = weather_sources[[kw]]
    )

    stopifnot(rSOILWAT2::dbW_setConnection(fnames_dbW[[kw]]))


    #--- ..* Populate dbWeather ------
    if (any(tasks_by_dbW[["add_to_dbW"]])) {

      #--- ....** Data source: DayMet ------
      if (identical(weather_sources[[kw]], "DayMet")) {

        #--- Set up parallel work
        library("doFuture") # load package to use `foreach` and `%dopar%`
        future::plan(strategy = "future::multisession", workers = 10)
        doFuture::registerDoFuture()
        doRNG::registerDoRNG(12345) # Initialize parallel RNG stream

        library("progressr")
        progressr::handlers(global = TRUE)
        progressr::handlers("txtprogressbar")
        prev_op <- options(progressr.enable = TRUE)

        conn_dbW <- DBI::dbConnect(RSQLite::SQLite(), fnames_dbW[[kw]])
        tmp <- DBI::dbExecute(conn_dbW, "PRAGMA journal_mode = WAL")


        #-- Get DayMet in parallel
        pfun_get_DayMet(
          tasks = tasks_by_dbW,
          simYears = simYears,
          fname_dbW = fnames_dbW[[kw]],
          correctWeatherValues = weather_actions[["correctWeatherValues"]]
        )


        #--- Clean up
        tmp <- DBI::dbExecute(conn_dbW, "PRAGMA optimize")
        tmp <- DBI::dbExecute(conn_dbW, "PRAGMA wal_checkpoint(TRUNCATE)")
        tmp <- DBI::dbExecute(conn_dbW, "PRAGMA journal_mode = DELETE")
        tmp <- DBI::dbDisconnect(conn_dbW)

        future::plan(strategy = "future::sequential")
        progressr::handlers(global = FALSE)
        options(prev_op)

        print(summary(warnings()))
      }


      #--- ....** Data source: gridMET ------
      if (identical(weather_sources[[kw]], "gridMET")) {
        ids_todo <- which(tasks_by_dbW[["add_to_dbW"]])

        res <- rSFSW2:::extract_daily_weather_from_gridMET(
          dir_data = dir_gridmet,
          site_ids = tasks_by_dbW[ids_todo, "ID_by_rSFSW2"],
          site_ids_by_dbW = tasks_by_dbW[ids_todo, "ID_by_dbW"],
          coords_WGS84 =
            tasks_by_dbW[ids_todo, c("Longitude", "Latitude"), drop = FALSE],
          start_year = simYears[[1L]],
          end_year = simYears[[length(simYears)]],
          id_ambient_scenario = 1L,
          correctWeatherValues = weather_actions[["correctWeatherValues"]],
          verbose = interactive()
        )
      }


      #--- ....** Data source: observations ------
      if (identical(weather_sources[[kw]], "obs")) {

        #--- ....** Process observed weather data ------
        dir_weatherraw <- file.path(dir_dataraw, "obsMeteoData")
        dir_weather <- file.path(dir_data, "WeatherData")
        dir.create(dir_weather, recursive = TRUE, showWarnings = FALSE)

        wobs <- utils::read.csv(file.path(dir_dataraw, "weatherObsNames.csv"))
        wobs <- data.frame(
          WeatherFolder = tasks_by_dbW[, "Label", drop = TRUE],
          DataFileName = xsim[, "fname_weather", drop = TRUE],
          Include_YN = xsim[, "Include_YN", drop = TRUE]
        )

        vars_wobs <- c("WeatherFolder", "DataFileName", "Include_YN")
        stopifnot(vars_wobs[1L:2L] %in% colnames(wobs))

        if (!vars_wobs[[3L]] %in% colnames(wobs)) {
          wobs[[vars_wobs[[3L]]]] <- rep(1L, nrow(wobs))
        }

        tasks_by_dbW_obs <- merge(
          tasks_by_dbW,
          wobs[, vars_wobs, drop = FALSE],
          by.x = "Label",
          by.y = vars_wobs[[1L]]
        )

        ids_todo <- which(
          tasks_by_dbW_obs[["add_to_dbW"]] &
            tasks_by_dbW_obs[[vars_wobs[[3L]]]] == 1L
        )

        for (ks in ids_todo) {
          wdata <- observedWeatherData(
            fileNameInput = file.path(
              dir_weatherraw, tasks_by_dbW_obs[ks, "DataFileName"]
            ),
            fileNameProcessed = file.path(
              dir_weather, paste0(tasks_by_dbW_obs[ks, "Label"], ".csv")
            ),
            longitude = tasks_by_dbW_obs[ks, "Longitude"],
            latitude = tasks_by_dbW_obs[ks, "Latitude"],
            simYears = simYears,
            metaObservedWeather = metaObservedWeather,
            imputeMissingObsWeather = weather_actions[["imputeMissingObsWeather"]],
            correctWeatherValues = weather_actions[["correctWeatherValues"]]
          )

          # Add to database
          stopifnot(rSOILWAT2::dbW_check_weatherData(wdata))

          rSOILWAT2::dbW_addWeatherData(
            weatherData = wdata,
            Label = tasks_by_dbW_obs[ks, "Label"],
            Site_id = tasks_by_dbW_obs[ks, "ID_by_dbW"],
            Scenario_id = 1L,
            Scenario = "Current"
          )
        }
      }
    }

    rSOILWAT2::dbW_disconnectConnection()
  }


  #--- ....** Supplement DayMet with daily wind data from gridMET ------
  if (
    all(
      identical(weather_sources[[1L]], "DayMet"),
      any(weather_sources == "gridMET")
    )
  ) {
    supplementDayMetWithgridMETWind(
      wfs = tmp_wfs[c("DayMet", "gridMET")],
      fnames_dbW = fnames_dbW[c("DayMet", "gridMET")]
    )
  }
}



#------ . ------
#------ Future projected weather data ------
if (isTRUE(tasks[["dbFuture"]])) {

  for (kw in seq_along(future_sources)) {

    ref_dbW <- switch(
      EXPR = future_sources[[kw]],
      MACAv2METDATA = "gridMET",
      stop(future_sources[[kw]], " is not yet implemented.")
    )

    metaFuture <- switch(
      EXPR = future_sources[[kw]],
      MACAv2METDATA = metaMACAv2METDATA(),
      stop(future_sources[[kw]], " is not yet implemented.")
    )


    #--- .. * Create future weather database -----
    if (!file.exists(fnames_dbFuture[[kw]])) {
      file.copy(from = fnames_dbW[[ref_dbW]], to = fnames_dbFuture[[kw]])
    }

    stopifnot(
      rSOILWAT2::dbW_setConnection(
        fnames_dbFuture[[kw]], create_if_missing = FALSE
      )
    )


    #--- .. * Identify tasks ------
    list_scenarios <- expand.grid(
      "idem.dall",
      experiment = metaFuture[["experiments"]][, "name"],
      model = metaFuture[["models"]][, "name"]
    ) |>
      as.data.frame()

    list_scenarios[, "Scenario"] <-
      list_scenarios[, c("Var1", "experiment", "model"), drop = FALSE] |>
      apply(MARGIN = 1L, paste, collapse = ".")

    stopifnot(
      rSOILWAT2::dbW_addScenarios(list_scenarios[, "Scenario"])
    )

    list_scenarios[, "Scenario_id"] <- rSOILWAT2::dbW_getScenarioId(
      list_scenarios[, "Scenario"]
    )

    list_sites <- merge(
      data.frame(Label = unique(xsim[, "WeatherFolder", drop = TRUE])),
      rSOILWAT2::dbW_getSiteTable(),
      all.x = TRUE
    )

    needsData <- !rSOILWAT2::dbW_has_weatherData(
      Site_ids = list_sites[["Site_id"]],
      Scenario_ids = list_scenarios[["Scenario_id"]]
    )
    dimnames(needsData) <- list(
      list_sites[["Label"]], list_scenarios[["Scenario"]]
    )

    tasks_by_dbF <- stats::reshape(
      as.data.frame(needsData),
      direction = "long",
      idvar = "Label",
      ids = row.names(needsData),
      timevar = "Scenario",
      times = colnames(needsData),
      v.names = "add_to_dbW",
      varying = list(colnames(needsData))
    ) |>
      merge(y = list_sites, all.x = TRUE) |>
      merge(y = list_scenarios, all.x = TRUE) |>
      merge(
        y = data.frame(
          metaFuture[["models"]][, c("name", "realization"), drop = FALSE]
        ),
        by.x = "model",
        by.y = "name",
        all.x = TRUE
      )

    row.names(tasks_by_dbF) <- NULL

    tasks_by_dbF[["Label2"]] <- gsub(
      paste0("^", ref_dbW), future_sources[[kw]], tasks_by_dbF[["Label"]]
    )



    #--- .. * Populate dbFuture ------

    #--- .... ** Data source: MACAv2METDATA ------
    if (identical(future_sources[[kw]], "MACAv2METDATA")) {
      Nworkers <- 10L
      pVerbose <- beVerbose || interactive() || future::nbrOfWorkers() == 1L

      #--- Set up parallel work
      library("doFuture") # load package to use `foreach` and `%dopar%`
      #future::plan(strategy = "future::multisession", workers = Nworkers)
      future::plan(strategy = "future::sequential")
      doFuture::registerDoFuture()
      doRNG::registerDoRNG(12345) # Initialize parallel RNG stream

      library("progressr")
      progressr::handlers(global = TRUE)
      progressr::handlers("txtprogressbar")
      prev_op <- options(progressr.enable = TRUE)

      #-- Get single-site MACAv2METDATA in parallel
      conn_dbW <- DBI::dbConnect(RSQLite::SQLite(), fnames_dbFuture[[kw]])
      tmp <- DBI::dbExecute(conn_dbW, "PRAGMA journal_mode = WAL")
      tmp <- DBI::dbDisconnect(conn_dbW)

      pfun_get_MACAv2METDATA(
        tasks = tasks_by_dbF,
        fname_dbW = fnames_dbFuture[[kw]],
        meta = metaFuture,
        strided = isTRUE(weather_actions[["useStridedMACA"]]),
        tmpdir = file.path(
          dir_dataraw,
          paste0(
            future_sources[[kw]],
            if (isTRUE(weather_actions[["useStridedMACA"]])) {
              "-strided"
            } else {
              "-agg"
            }
          )
        ),
        correctWeatherValues = weather_actions[["correctWeatherValues"]],
        verbose = isTRUE(pVerbose)
      )


      #--- Clean up
      conn_dbW <- DBI::dbConnect(RSQLite::SQLite(), fnames_dbFuture[[kw]])
      tmp <- DBI::dbExecute(conn_dbW, "PRAGMA optimize")
      tmp <- DBI::dbExecute(conn_dbW, "PRAGMA wal_checkpoint(TRUNCATE)")
      tmp <- DBI::dbExecute(conn_dbW, "PRAGMA journal_mode = DELETE")
      tmp <- DBI::dbDisconnect(conn_dbW)


      future::plan(strategy = "future::sequential")
      progressr::handlers(global = FALSE)
      options(prev_op)

      print(summary(warnings()))
    }


    #--- ..* Clean up ------
    rSOILWAT2::dbW_disconnectConnection()
  }
}


#------ . ------
#------ Topographic position ------
# Aspect
# * input: -1 = no slope; 0 degrees is north in this case. 90 is east, 180 south, 270 west, etc.
# * SW2: South facing slope: aspect = 0, East = -pi / 2, West = pi / 2, North = ±pi

stopifnot(
  topo_sources %in% c("obs", "NED1"),
  length(topo_sources) %in% 1L:2L
)

fname_xsim3 <- file.path(dir_data, "xsim3.rds")

if (all(file.exists(fname_xsim3), !isTRUE(tasks[["topo"]]))) {
  xsim <- readRDS(fname_xsim3)

} else {
  #--- ..* Create topo data container ------
  res_topo <- list()

  varsTopo <- c("ELEV_m", "Slope_deg", "Aspect_deg")
  varTopoSource <- "Source_Topo"
  varsTopoAll <- c(varsTopo, varTopoSource)
  templateTopo <- xsim[, c("Label", varsSimLabel)]
  templateTopo[, varsTopoAll] <- NA


  #------ ..* Data source: USGS NED-1 arcsec ------
  if (any(topo_sources == "NED1")) {

    fname_topoNED <- file.path(
      dir_data,
      paste0(
        prjTag, "_topo-NED1_v", versions[[vsel]], ".rds"
      )
    )

    if (file.exists(fname_topoNED)) {
      res_topo[["NED1"]] <- readRDS(fname_topoNED)

    } else {
      dir_ned1 <- file.path(dir_dataraw, "NED1")
      dir.create(dir_ned1, recursive = TRUE, showWarnings = FALSE)

      xned <- getTopoNED1(
        x = templateTopo,
        prjTag = prjTag,
        path = dir_ned1,
        doTopographicPosition = topo_actions[["doTopographicPosition"]],
        use_FedData_for_topo = topo_actions[["use_FedData_for_topo"]],
        methodTopoDownload = topo_actions[["methodTopoDownload"]]
      )

      #--- Transfer to vals_topo
      res_topo[["NED1"]] <- templateTopo

      ids <- matchMultipleVariables(
        xLeft = res_topo[["NED1"]], xRight = xned, variables = varsSimLabel
      )

      res_topo[["NED1"]][ids > 0L, varsTopoAll] <-
        as.data.frame(xned)[ids, varsTopoAll]

      #--- Write to disk
      saveRDS(res_topo[["NED1"]], file = fname_topoNED)
    }
  }


  #------ ..* Data source: obs ------
  if (any(topo_sources == "obs")) {

    fname_topoObs <- file.path(
      dir_data,
      paste0(
        prjTag, "_topo-obs_v", versions[[vsel]], ".rds"
      )
    )

    if (file.exists(fname_topoObs)) {
      res_topo[["obs"]] <- readRDS(fname_topoObs)

    } else {
      tmp <- switch(
        EXPR = versions[[vsel]],
        "run1-20240826" = "xsites.csv",
        "run1-20250306" = "xsites.csv",
        "run1-20250320" = "xsites.csv",
        stop("Version ", versions[[vsel]], " ist not implemented.")
      )

      tmpx <- readInputFile(file.path(dir_data, tmp))

      if (isTRUE(topo_actions[["doTopographicPosition"]])) {
        tmps <- grep("slope", colnames(tmpx), ignore.case = TRUE)
        tmp_slope <- tmpx[, tmps, drop = TRUE]
        tmp_slope[tmp_slope == -1] <- NA
        if (length(tmp_slope) == 0L) tmp_slope <- NA

        tmpa <- grep("aspect", colnames(tmpx), ignore.case = TRUE)
        tmp_aspect <- tmpx[, tmpa, drop = TRUE]
        tmp_aspect[tmp_aspect == -1] <- NA
        if (length(tmp_aspect) == 0L) tmp_aspect <- NA

      } else {
        tmp_slope <- NA
        tmp_aspect <- NA
      }

      xtobs <- data.frame(
        as.data.frame(tmpx)[, varsSimLabel, drop = FALSE],
        ELEV_m = tmpx[, "Elevation_m", drop = TRUE],
        Slope_deg = tmp_slope,
        Aspect_deg = if (isTRUE(southAspect == 0)) {
          tmp_aspect
        } else {
          rSW2utils::circ_add(tmp_aspect, y = 180, int = 360)
        },
        Source_Topo = NA_character_
      )

      ids_hasTopo <- apply(
        xtobs[, varsTopo, drop = FALSE],
        MARGIN = 1L,
        function(x) !all(is.na(x))
      )

      xtobs[ids_hasTopo, varTopoSource] <- "obs"

      #--- Transfer to vals_topo
      res_topo[["obs"]] <- templateTopo

      ids <- matchMultipleVariables(
        xLeft = res_topo[["obs"]], xRight = xtobs, variables = varsSimLabel
      )

      res_topo[["obs"]][ids > 0L, varsTopoAll] <-
        as.data.frame(xtobs)[ids, varsTopoAll]

      #--- Write to disk
      saveRDS(res_topo[["obs"]], file = fname_topoObs)
    }
  }


  #--- ..* Finalize and combine if two topo sources ------
  xsim <- combineTwoTopoDatasets(
    x1 = res_topo[[topo_sources[[1L]]]],
    x2 = res_topo[[topo_sources[[2L]]]],
    topo_sources = topo_sources,
    xTopoTemplate = xsim,
    varsSimLabel = varsSimLabel,
    varsTopo = varsTopo,
    varTopoSource = varTopoSource,
    doFigures = doFigures,
    pathFigure = dir_figs
  )

  saveRDS(xsim, file = fname_xsim3)
}



#------ . ------
#------ Soils ------

stopifnot(
  soil_sources %in% c("obs", "SOLUS100", "SDA", "KSSL"),
  length(soil_sources) %in% 1L:2L,
  !("obs" %in% soil_sources) || identical(soil_sources[[1L]], "obs")
)

fname_xsoils <- file.path(dir_data, "xsoils.rds")

if (all(file.exists(fname_xsoils), !isTRUE(tasks[["soils"]]))) {
  xsoils <- readRDS(fname_xsoils)

} else {
  #--- ..* Create soils data container ------
  res_soils <- list()

  soilHeader <- c(
    "Label", varsSimLabel, varsSoilID,
    grep("ExpTrt_", colnames(xsim), value = TRUE, fixed = TRUE),
    "exclusionReason"
  ) |>
    unique()

  res <- createSoilsTemplate(
    xsim, soilHeader, varsCoords = varsSoilCoords, prjCRS = prjCRS
  )

  xSoilsSpatial <- res[["xSoilsSpatial"]]
  xSoilsTemplate <- res[["xSoilsTemplate"]]

  reqElementsSoilsTemplate <- c(
    "ref", "table_keys", "table_depths", "table_texture"
  )

  stopifnot(
    reqElementsSoilsTemplate %in% names(xSoilsTemplate),
    identical(
      rep(Nsim, times = length(reqElementsSoilsTemplate) - 1L),
      vapply(
        xSoilsTemplate[reqElementsSoilsTemplate[-1L]],
        function(x) nrow(x),
        FUN.VALUE = NA_integer_,
        USE.NAMES = FALSE
      )
    )
  )


  #--- ..* Data source: NRCS SOLUS100 ------
  if (any(soil_sources == "SOLUS100")) {

    fname_soils_solus100 <- file.path(
      dir_data,
      paste0(prjTag, "_soils-SOLUS100-v20240108_v", versions[[vsel]], ".rds")
    )

    if (file.exists(fname_soils_solus100)) {
      res_soils[["SOLUS100"]] <- readRDS(fname_soils_solus100)

    } else {
      dir_solus100 <- file.path(dir_dataraw, "SOLUS100-v20240108")
      dir.create(dir_solus100, recursive = TRUE, showWarnings = FALSE)

      tmp <- "/Users/dschlaepfer/BigData/NRCS/SOLUS100/solus100notpub_v2024-01-08"
      if (dir.exists(tmp)) {
        dir_solus100 <- tmp
      }

      res_soils[["SOLUS100"]] <- getSoilsFromSOLUS100(
        x = xSoilsSpatial,
        simSoilLayers = simSoilLayers,
        xSoilsTemplate = xSoilsTemplate,
        pathData = dir_solus100,
        bufferDistance = soil_actions[["buffer_m"]],
        doFigures = doFigures,
        varsSimLabel = varsSimLabel,
        pathFigure = dir_figs,
        mapCRS = mapCRS
      )

      saveRDS(res_soils[["SOLUS100"]], file = fname_soils_solus100)
    }
  }


  #--- ..* Data source: NRCS SDA ------
  if (any(soil_sources == "SDA")) {

    fname_soils_sda <- file.path(
      dir_data,
      paste0(prjTag, "_soils-SDA_v", versions[[vsel]], ".rds")
    )

    if (file.exists(fname_soils_sda)) {
      res_soils[["SDA"]] <- readRDS(fname_soils_sda)

    } else {

      res_soils[["SDA"]] <- getSoilsFromSDA(
        x = xSoilsSpatial,
        simSoilLayers = simSoilLayers,
        xSoilsTemplate = xSoilsTemplate,
        pathData = dir_data,
        doFigures = doFigures,
        varsSimLabel = varsSimLabel,
        pathFigure = dir_figs,
        mapCRS = mapCRS,
        verbose = beVerbose
      )

      saveRDS(res_soils[["SDA"]], file = fname_soils_sda)
    }
  }


  #--- ..* Data source: NRCS KSSL ------
  if (any(soil_sources == "KSSL")) {

    fname_soils_KSSL <- file.path(
      dir_data,
      paste0(prjTag, "_soils-KSSL_v", versions[[vsel]], ".rds")
    )

    if (file.exists(fname_soils_KSSL)) {
      res_soils[["KSSL"]] <- readRDS(fname_soils_KSSL)

    } else {
      res_soils[["KSSL"]] <- getSoilsFromKSSL(
        x = xSoilsSpatial,
        simSoilLayers = simSoilLayers,
        xSoilsTemplate = xSoilsTemplate,
        whatKSSL = soil_actions[["whatKSSL"]],
        varsUseSoil2IfValueMissingInSoil1 =
          soil_actions[["varsUseSoil2IfValueMissingInSoil1"]],
        doFigures = doFigures,
        varsSimLabel = varsSimLabel,
        pathFigure = dir_figs,
        mapCRS = mapCRS
      )

      saveRDS(res_soils[["KSSL"]], file = fname_soils_KSSL)
    }
  }



  #--- ..* Data source: observed soil ------
  if (any(soil_sources == "obs")) {

    fname_soils_obs <- file.path(
      dir_data,
      paste0(prjTag, "_soils-obs_v", versions[[vsel]], ".rds")
    )

    if (file.exists(fname_soils_obs)) {
      res_soils[["obs"]] <- readRDS(fname_soils_obs)

    } else {

      xos <- xSoilsTemplate
      xos[["ref"]] <- "user provided"


      #--- ....** Read observed soil layers ------
      tmp <- switch(
        EXPR = versions[[vsel]],
        "run1-20250122" = file.path(
          "v20241226", "SOILWAT2_InputData_SoilLayers.xlsx"
        ),
        stop("Version ", versions[[vsel]], " ist not implemented.")
      )

      tmpx <- readInputFile(file.path(dir_dataraw, tmp))

      soilID <- intersect(soilHeader, colnames(tmpx))
      stopifnot(length(soilID) == 1L)
      ids <- match(
        xsim[, soilID, drop = TRUE],
        tmpx[, soilID, drop = TRUE],
        nomatch = 0L
      )

      tmpcns <- intersect(
        setdiff(colnames(tmpx), soilHeader),
        colnames(xos[["table_depths"]])
      )

      if (length(tmpcns) > 0L) {
        xos[["table_depths"]][ids > 0, tmpcns] <- data.matrix(
          tmpx[ids, tmpcns, drop = FALSE]
        )
      }

      if (!("N_horizons" %in% tmpcns)) {
        tmp <- grep(
          "^depth_L[[:digit:]]{1,2}$", colnames(xos[["table_depths"]])
        )
        xos[["table_depths"]][, "N_horizons"] <- apply(
          xos[["table_depths"]][, tmp, drop = FALSE],
          MARGIN = 1L,
          function(x) sum(!is.na(x))
        )
      }

      if (!("SoilDepth_cm" %in% tmpcns)) {
        tmp <- grep(
          "^depth_L[[:digit:]]{1,2}$", colnames(xos[["table_depths"]])
        )
        xos[["table_depths"]][, "SoilDepth_cm"] <- apply(
          xos[["table_depths"]][, tmp, drop = FALSE],
          MARGIN = 1L,
          function(x) max(x, na.rm = TRUE)
        )
      }


      #--- ....** Read observed soil properties ------
      tmp <- switch(
        EXPR = versions[[vsel]],
        "run1-20250122" = file.path(
          "v20241226", "SOILWAT2_InputData_Soils_DTv2.xlsx"
        ),
        stop("Version ", versions[[vsel]], " ist not implemented.")
      )

      tmpx <- readInputFile(file.path(dir_dataraw, tmp))

      soilID <- intersect(soilHeader, colnames(tmpx))
      stopifnot(length(soilID) == 1L)
      ids <- match(
        xsim[, soilID, drop = TRUE],
        tmpx[, soilID, drop = TRUE],
        nomatch = 0L
      )

      xos[["table_texture"]] <- copySoilColumns(
        xout = xos[["table_texture"]],
        xin = data.matrix(tmpx),
        meta = metaObservedSoilProperties,
        rowIDs = ids
      )


      #--- ....** Save to disk ------
      res_soils[["obs"]] <- xos
      saveRDS(res_soils[["obs"]], file = fname_soils_obs)
    }
  }


  #--- ..* Finalize and combine if two soil sources ------
  stopifnot(length(soil_sources) %in% 1L:2L)

  xsoils <- combineTwoSoils(
    x1 = res_soils[[soil_sources[[1L]]]],
    x2 = if (length(soil_sources) == 2L) res_soils[[soil_sources[[2L]]]],
    soil_sources = soil_sources,
    xSoilsTemplate = xSoilsTemplate,
    simSoilLayers = simSoilLayers,
    soilHeader = soilHeader,
    combineSoilSource2 = soil_actions[["combineSoilSource2"]],
    varsUseSoil2IfValueMissingInSoil1 =
      soil_actions[["varsUseSoil2IfValueMissingInSoil1"]],
    hasObsSoilDepth = soil_actions[["hasObsSoilDepth"]],
    imputeLOCF = soil_actions[["imputeLOCF"]],
    allowAllSandClayOrSilt = soil_actions[["allowAllSandClayOrSilt"]],
    doFigures = doFigures,
    pathFigure = dir_figs
  )

  #--- ....** Save final soils ------
  saveRDS(xsoils, file = fname_xsoils)
}



#--- ..* Map final soils ------
if (doFigures) {
  # Map of soil sources
  fname_fig_map_source <- file.path(dir_figs, "Fig-map_soil-source.png")

  if (!file.exists(fname_fig_map_source)) {
    plot_map(
      fname = fname_fig_map_source,
      x = cbind(
        xSoilsSpatial,
        xsim[, varsSimLabel],
        source = paste(
          xsoils[["table_keys"]][, varsSimLabel[[1L]]],
          xsoils[["table_keys"]][, "Source_Soils"],
          sep = "-"
        )
      ),
      var_index = varsSimLabel,
      crs = mapCRS,
      var_plot = "source",
      size = 0.01,
      ids_highlight = which(xsoils[["table_keys"]][["Include_YN"]] == 0L)
    )
  }


  # Map of soil depth
  fname_fig_map_depth <- file.path(dir_figs, "Fig-map_soil-maxDepth.png")

  if (!file.exists(fname_fig_map_depth)) {
    plot_map(
      fname = fname_fig_map_depth,
      x = cbind(
        xSoilsSpatial,
        xsim[, varsSimLabel],
        soilDepth = xsoils[["table_depths"]][, "SoilDepth_cm"]
      ),
      var_index = varsSimLabel,
      crs = mapCRS,
      var_plot = "soilDepth",
      size = 0.01,
      ids_highlight = which(xsoils[["table_depths"]][, "SoilDepth_cm"] == 0L)
    )
  }


  #--- Map of sites without soils data
  fname_fig_map_nosoil <- file.path(dir_figs, "Fig-map_soil-noData.png")

  if (!file.exists(fname_fig_map_nosoil)) {
    plot_map(
      fname = fname_fig_map_nosoil,
      x = xSoilsSpatial,
      var_index = varsSimLabel,
      crs = mapCRS,
      ids_highlight = which(xsoils[["table_keys"]][["Include_YN"]] == 0L)
    )
  }


  #--- Maps of soil properties
  tmpVars <- c("Matricd", "GravelContent", "Sand", "Clay", "SOM")
  tmpAcrossProfile <- c("mean", "max")

  for (ks in seq_along(tmpVars)) for (kp in seq_along(tmpAcrossProfile)) {
    tag <- paste0(tmpVars[[ks]], "-", tmpAcrossProfile[[kp]])

    fname_fig_map_soilp <- file.path(
      dir_figs, paste0("Fig-map_soil-", tag, ".png")
    )

    if (!file.exists(fname_fig_map_soilp)) {
      ids <- grep(
        paste0(tmpVars[[ks]], "_L[[:digit:]]{1,2}$"),
        colnames(xsoils[["table_texture"]])
      )

      xtmp <- cbind(
        xSoilsSpatial,
        xsim[, varsSimLabel],
        soilProperty = apply(
          X = xsoils[["table_texture"]][, ids, drop = FALSE],
          MARGIN = 1L,
          FUN = tmpAcrossProfile[[kp]],
          na.rm = TRUE
        )
      )

      colnames(xtmp)[grep("soilProperty", colnames(xtmp))] <- tag

      plot_map(
        fname = fname_fig_map_soilp,
        x = xtmp,
        var_index = varsSimLabel,
        crs = mapCRS,
        var_plot = tag,
        size = 0.2,
        colorScaleName = "viridis"
      )
    }
  }
}


#--- ..* Update sim ------
fname_xsim4 <- file.path(dir_data, "xsim4.rds")

if (file.exists(fname_xsim4)) {
  xsim <- readRDS(fname_xsim4)

} else {

  xsim[, "Source_Soils"] <- xsoils[["table_keys"]][, "Source_Soils"]

  #--- Exclude sites due to missing soils
  ids_exclude <- which(xsoils[["table_keys"]][["Include_YN"]] == 0L)

  if (length(ids_exclude) > 0L) {
    xsim[ids_exclude, "Include_YN"] <- 0L
    xsim[ids_exclude, "exclusionReason"] <-
      xsoils[["table_keys"]][ids_exclude, "exclusionReason"]
  }

  saveRDS(xsim, file = fname_xsim4)
}



#------ . ------
#------ Vegetation ------

fname_xveg <- file.path(dir_data, "xveg.rds")

if (all(file.exists(fname_xveg), !isTRUE(tasks[["veg"]]))) {
  xveg <- readRDS(fname_xveg)

} else {
  pfts <- c("Tree", "Shrub", "Grass", "Forb")
  pftsNonTree <- pfts[-1L]

  varsVegHas <- NULL
  varsVegAll <- NULL
  varsVegObs <- NULL

  #--- ..* Create vegetation data container ------
  vegHeader <- c(
    "Label", varsSimLabel, varsVegID,
    grep("ExpTrt_", colnames(xsim), value = TRUE, fixed = TRUE)
  ) |>
    unique()

  res <- createVegetationTemplate(xsim, vegHeader, pfts)
  xveg <- res[["xveg"]]
  varsVegAll <- res[["varsVeg"]]


  #--- ..* Data source: obs ------
  if (any(veg_sources == "obs")) {
    tmp <- switch(
      EXPR = versions[[vsel]],
      "run1-20250122" = file.path(
        "v20241226", "SOILWAT2_InputData_LandCover_Vegetation_DT.xlsx"
      ),
      stop("Version ", versions[[vsel]], " ist not implemented.")
    )

    tmpx <- readInputFile(file.path(dir_dataraw, tmp))
    ids <- do.call(order, tmpx[intersect(colnames(tmpx), vegHeader)])
    tmpx <- tmpx[ids, , drop = FALSE]

    ids <- match(
      colnames(tmpx), metaObservedVegetation[, "exter"], nomatch = 0L
    )
    varsVegObs <- metaObservedVegetation[ids, "rSW2"]
    stopifnot(varsVegObs %in% varsVegAll)
    colnames(tmpx)[ids > 0] <- varsVegObs

    ids <- matchMultipleVariables(
      xLeft = xveg,
      xRight = tmpx,
      variables = intersect(vegHeader, colnames(tmpx))
    )
    xveg[ids > 0L, varsVegObs] <- tmpx[ids, varsVegObs, drop = FALSE]

    varsVegHas <- unique(c(varsVegHas, varsVegObs))

    message("Added variables from observations: ", toString(varsVegObs))


    # Copy basal area if provided by veg inputs
    if (veg_actions[["varBasalAreaPIPO"]] %in% colnames(tmpx)) {
      xsim[ids > 0L, veg_actions[["varBasalAreaPIPO"]]] <-
        tmpx[ids, veg_actions[["varBasalAreaPIPO"]]]
    }
  }


  if (isTRUE(veg_actions[["obsBiomassValidAllMonths"]])) {
    varsBiomFix <- grep(
      "_Biomass_|_Litter_|_BiomassLive_|_LAIconv_", varsVegObs, value = TRUE
    )

    if (length(varsBiomFix) > 0L) {
      listVarBiomsExpand <- lapply(
        strsplit(varsBiomFix, split = "_m[[:digit:]]{1,2}$"),
        function(x) paste0(x, "_m", seq_len(12L))
      )
      stopifnot(unlist(listVarBiomsExpand) %in% varsVegAll)

      for (kv in seq_along(varsBiomFix)) {
        xveg[, listVarBiomsExpand[[kv]]] <-
          xveg[, varsBiomFix[[kv]], drop = TRUE]
      }

      varsVegHas <- unique(c(varsVegHas, unlist(listVarBiomsExpand)))
    }
  }


  #--- ..* Data source: PIPO values from basal area ------
  # GAMs trained on FVEV data from Andrews et al. 2020)
  #   * Tree cover
  #   * Tree total biomass, litter, fraction live, LAIconv
  if (isTRUE(veg_actions[["doBasalAreaPIPO"]])) {
    stopifnot(!is.null(veg_actions[["varBasalAreaPIPO"]]))

    vegPIPO <- predictTreeInputsPIPO(
      ba = xsim[, veg_actions[["varBasalAreaPIPO"]], drop = TRUE],
      baModels = readRDS(
        file.path(
          dir_dataraw, "Andrews+_2020_JAE", "data",
          "Andrews2020_biomassRegressionPIPO.rds"
        )
      )
    )

    # Don't overwrite variables from "obs" sources
    pipoVars <- setdiff(
      intersect(colnames(vegPIPO), setdiff(colnames(xveg), vegHeader)),
      varsVegObs
    )

    if (length(pipoVars) > 0L) {
      stopifnot(pipoVars %in% varsVegAll)
      xveg[, pipoVars] <- vegPIPO[, pipoVars, drop = FALSE]
      varsVegHas <- unique(c(varsVegHas, pipoVars))

    } else {
      warning("No variables from doBasalAreaPIPO.")
    }
  }


  # Fixed values for lodgepole pine from Bradford et al. 2014 Ecosystems
  # Don't overwrite variables from "obs" sources
  #   * Tree %live: 0.083
  #   * LAIconv: 500
  if (isTRUE(veg_actions[["doFixedTreeBradford2014"]])) {
    varsFix <- setdiff(paste0("Tree_FractionLive_m", seq_len(12L)), varsVegObs)
    if (length(varsFix) > 0L) {
      stopifnot(varsFix %in% varsVegAll)
      xveg[, varsFix] <- 0.083
      varsVegHas <- unique(c(varsVegHas, varsFix))
    }

    varsFix <- setdiff(paste0("Tree_LAIconv_m", seq_len(12L)), varsVegObs)
    if (length(varsFix) > 0L) {
      stopifnot(varsFix %in% varsVegAll)
      xveg[, varsFix] <- 500
      varsVegHas <- unique(c(varsVegHas, varsFix))
    }
  }


  #--- ..* Combined litter ------
  # User provides litter as total per site (and not for each pft)
  # --> Each pft has the same litter value at 100% cover
  if (isTRUE(veg_actions[["hasCombinedLitter"]])) {
    stopifnot(
      !is.null(veg_actions[["varCombinedLitter"]]),
      !grepl("_Litter_m", varsVegObs) # don't overwrite other user inputs
    )
    totalLitter <- xveg[, veg_actions[["varCombinedLitter"]], drop = TRUE]

    for (kv in seq_along(pfts)) {
      varsLitter <- paste0(pfts[[kv]], "_Litter_m", seq_len(12L))
      stopifnot(varsLitter %in% varsVegAll)
      xveg[, varsLitter] <- totalLitter
      varsVegHas <- unique(c(varsVegHas, varsLitter))
    }
  }


  #--- ..* Estimate fraction live biomass ------
  if (isTRUE(veg_actions[["estimateLiveBiomassFraction"]])) {
    listFractionLive <- lapply(
      grep("_FractionLive_", varsVegAll, value = TRUE),
      function(fl) {
        tmp <- c(
          fl,
          sub("_FractionLive_", "_BiomassLive_", fl),
          sub("_FractionLive_", "_Biomass_", fl)
        )
        if (
          all(
            tmp %in% varsVegHas,
            apply(xveg[, tmp, drop = FALSE], 2L, function(x) !all(is.na(x)))
          )
        ) {
          tmp
        }
      }
    )

    listFractionLive <- listFractionLive[lengths(listFractionLive) > 0L]

    if (length(listFractionLive) > 0L) {
      for (kv in seq_along(listFractionLive)) {
        tmpv <- listFractionLive[[kv]]
        tmp <- xveg[, tmpv[[2L]], drop = TRUE] / xveg[, tmpv[[3L]], drop = TRUE]
        stopifnot(!all(tmp > 1, na.rm = TRUE))
        ids0 <- which(
          abs(xveg[, tmpv[[3L]], drop = TRUE]) < sqrt(.Machine[["double.eps"]])
        )
        tmp[ids0] <- 0
        tmp[tmp > 1] <- 1
        xveg[, tmpv[[1L]]] <- tmp
      }

      varsVegHas <- unique(
        c(varsVegHas, unlist(lapply(listFractionLive, function(x) x[[1L]])))
      )
    }
  }


  #--- ..* Scale biomass to 100%: obs/PIPO biomass is on the ground ------
  # Use "raw" (unscaled) composition
  # SOILWAT2 expects biomass inputs each to reflect 100% cover
  if (isTRUE(veg_actions[["isBiomassOnTheGround"]])) {
    for (kv in seq_along(pfts)) {
      tmpVarCover <- paste0("Composition_", pfts[[kv]], "Fraction")

      if (isTRUE(tmpVarCover %in% varsVegHas)) {
        tmpVarBiomass <- intersect(
          paste0(pfts[[kv]], "_Biomass_m", seq_len(12L)),
          varsVegHas
        )

        if (length(tmpVarBiomass) > 0L) {
          tmpCover <- xveg[, tmpVarCover, drop = TRUE]
          ids_zero <- tmpCover < sqrt(.Machine[["double.eps"]])

          xveg[!ids_zero, tmpVarBiomass] <- sweep(
            xveg[!ids_zero, tmpVarBiomass, drop = FALSE],
            MARGIN = 1L,
            STATS = tmpCover[!ids_zero],
            FUN = "/"
          )

        } else {
          warning("No biomass for type ", shQuote(pfts[[kv]]), " to scale.")
        }
      } else {
        warning("No cover for type ", shQuote(pfts[[kv]]), " to scale biomass.")
      }
    }
  }


  #--- ..* Scale litter to 100%: obs/PIPO litter is on the ground ------
  # Use "raw" (unscaled) composition
  # SOILWAT2 expects litter inputs each to reflect 100% cover
  if (isTRUE(veg_actions[["isLitterOnTheGround"]])) {
    stopifnot(
      # hasCombinedLitter provides litter 100% cover
      !isTRUE(veg_actions[["hasCombinedLitter"]])
    )

    for (kv in seq_along(pfts)) {
      tmpVarCover <- paste0("Composition_", pfts[[kv]], "Fraction")

      if (isTRUE(tmpVarCover %in% varsVegHas)) {
        tmpVarLitter <- intersect(
          paste0(pfts[[kv]], "_Litter_m", seq_len(12L)),
          varsVegHas
        )

        if (length(tmpVarLitter) > 0L) {
          tmpCover <- xveg[, tmpVarCover, drop = TRUE]
          ids_zero <- tmpCover < sqrt(.Machine[["double.eps"]])

          xveg[!ids_zero, tmpVarLitter] <- sweep(
            xveg[!ids_zero, tmpVarLitter, drop = FALSE],
            MARGIN = 1L,
            STATS = tmpCover[!ids_zero],
            FUN = "/"
          )

        } else {
          warning("No litter for type ", shQuote(pfts[[kv]]), " to scale.")
        }
      } else {
        warning("No cover for type ", shQuote(pfts[[kv]]), " to scale litter.")
      }
    }
  }


  #--- ..* Data source: climate relationships ------
  if (any(veg_sources == "climVeg")) {
    fname_xvegClim <- file.path(dir_data, "xveg-clim.csv")

    if (file.exists(fname_xvegClim)) {
      xvegClim <- readInputFile(fname_xvegClim)

    } else {
      xvegClim <- data.frame(
        array(dim = dim(xveg), dimnames = list(NULL, colnames(xveg)))
      )
      xvegClim[, vegHeader] <- xveg[, vegHeader, drop = FALSE]

      stopifnot(
        rSOILWAT2::dbW_setConnection(fnames_dbW[[weather_sources[[1L]]]])
      )

      tmpTags <- lapply(
        c("_Biomass_m", "_FractionLive_m", "_AmountLive_m", "_Litter_m"),
        function(tag) paste0(tag, seq_len(12L))
      )

      stopifnot(veg_actions[["interpretSucculentCoverPNV"]] %in% pfts)
      varSucCov <- paste0(
        "Composition_", veg_actions[["interpretSucculentCoverPNV"]], "Fraction"
      )


      #--- ....** Estimate PNV ------
      pb <- utils::txtProgressBar(max = Nsim, style = 3L)

      for (ks in seq_len(Nsim)) {
        wd <- rSOILWAT2::dbW_getWeatherData(
          Label = xsim[ks, "WeatherFolder", drop = TRUE],
          startYear = simYears[[1L]],
          endYear = simYears[[length(simYears)]]
        )

        clim <- rSOILWAT2::calc_SiteClimate(
          wd,
          year.start = simYears[[1L]],
          year.end = simYears[[length(simYears)]],
          do_C4vars = TRUE,
          latitude = xsim[ks, "Y_WGS84", drop = TRUE]
        )

        # PNV cover
        covclim <- rSOILWAT2::estimate_PotNatVeg_composition(
          MAP_mm = 10 * clim[["MAP_cm"]],
          MAT_C = clim[["MAT_C"]],
          mean_monthly_ppt_mm = clim[["meanMonthlyPPTcm"]],
          mean_monthly_Temp_C = clim[["meanMonthlyTempC"]],
          dailyC4vars = clim[["dailyC4vars"]]
        )

        xvegClim[ks, "Composition_TreeFraction"] <-
          covclim[["Rel_Abundance_L1"]][["SW_TREES"]]

        xvegClim[ks, "Composition_ShrubFraction"] <-
          covclim[["Rel_Abundance_L1"]][["SW_SHRUB"]]

        xvegClim[ks, "Composition_GrassFraction"] <-
          covclim[["Rel_Abundance_L1"]][["SW_GRASS"]]

        xvegClim[ks, "Composition_ForbFraction"] <-
          covclim[["Rel_Abundance_L0"]][["Forbs"]]

        xvegClim[ks, varSucCov] <- xvegClim[ks, varSucCov] +
          covclim[["Rel_Abundance_L0"]][["Succulents"]]


        # PNV biomass
        vegclim <- rSOILWAT2::estimate_PotNatVeg_biomass(
          target_temp = clim[["meanMonthlyTempC"]],
          target_MAP_mm = 10 * clim[["MAP_cm"]],
          do_adjust_phenology = TRUE,
          do_adjust_biomass = TRUE,
          fgrass_c3c4ann = covclim[["Grasses"]]
        )

        for (pft in c("Shrub", "Grass", "Forb")) {
          xtmp <- switch(EXPR = pft, Shrub = "shrub", "grass")

          xvegClim[ks, paste0(pft, tmpTags[[1L]])] <-
            vegclim[[xtmp]][, "Biomass", drop = TRUE]
          xvegClim[ks, paste0(pft, tmpTags[[2L]])] <-
            vegclim[[xtmp]][, "Perc.Live", drop = TRUE]
          xvegClim[ks, paste0(pft, tmpTags[[3L]])] <-
            vegclim[[xtmp]][, "Amount.Live", drop = TRUE]
          xvegClim[ks, paste0(pft, tmpTags[[4L]])] <-
            vegclim[[xtmp]][, "Litter", drop = TRUE]
        }

        utils::setTxtProgressBar(pb, ks)
      }

      close(pb)

      rSOILWAT2::dbW_disconnectConnection()

      utils::write.csv(xvegClim, file = fname_xvegClim, row.names = FALSE)
    }


    varsVegClim <- intersect(setdiff(colnames(xvegClim), vegHeader), varsVegAll)

    isNotAllNA <- apply(
      xvegClim[, varsVegClim, drop = FALSE],
      MARGIN = 2L,
      function(x) !all(is.na(x))
    )

    varsVegClim <- varsVegClim[isNotAllNA]


    if (isTRUE(veg_actions[["monthScalePNVByObs"]] %in% seq_len(12L))) {
      #--- ....** Scale PNV to observed month ------
      varObsBiomassToScale <- list(
        Shrub = paste0(
          "Shrub_Biomass_m", veg_actions[["monthScalePNVByObs"]]
        )
      ) |> lapply(
        function(var) intersect(var, varsVegObs)
      )

      varObsAmountLiveToScale <- list(
        Grass = paste0(
          "Grass_AmountLive_m", veg_actions[["monthScalePNVByObs"]]
        ),
        Forb = paste0(
          "Forb_AmountLive_m", veg_actions[["monthScalePNVByObs"]]
        )
      ) |> lapply(
        function(var) intersect(var, varsVegObs)
      )

      stopifnot(
        c("obs", "climVeg") %in% veg_sources,
        names(varObsBiomassToScale) %in% pfts,
        names(varObsAmountLiveToScale) %in% pfts,
        length(
          intersect(names(varObsBiomassToScale), names(varObsAmountLiveToScale))
        ) == 0L,
        grepl("_Biomass_", unlist(varObsBiomassToScale), fixed = TRUE),
        grepl("_AmountLive_", unlist(varObsAmountLiveToScale), fixed = TRUE)
      )


      # Scale PNV total biomass and litter with observed biomass
      for (kv in seq_along(varObsBiomassToScale)) {
        fscale <-
          xveg[, varObsBiomassToScale[[kv]], drop = TRUE] /
          xvegClim[, varObsBiomassToScale[[kv]], drop = TRUE]

        varToScale <- paste0(
          sub("[[:digit:]]{1,2}", "", varObsBiomassToScale[[kv]]),
          seq_len(12L)
        )

        varToScaleLitter <- gsub("_Biomass_", "_Litter_", varToScale)

        xvegClim[, varToScale] <-
          fscale * xvegClim[, varToScale, drop = FALSE]

        xvegClim[, varToScaleLitter] <-
          fscale * xvegClim[, varToScaleLitter, drop = FALSE]
      }

      # Scale PNV live biomass amount and litter with observed biomass
      # Recalculate PNV total biomass
      for (kv in seq_along(varObsAmountLiveToScale)) {
        fscale <-
          xveg[, varObsAmountLiveToScale[[kv]], drop = TRUE] /
          xvegClim[, varObsAmountLiveToScale[[kv]], drop = TRUE]

        varToScale <- paste0(
          sub("[[:digit:]]{1,2}", "", varObsAmountLiveToScale[[kv]]),
          seq_len(12L)
        )

        varToScaleLitter <- gsub("_AmountLive_", "_Litter_", varToScale)

        xvegClim[, varToScale] <-
          fscale * xvegClim[, varToScale, drop = FALSE]

        xvegClim[, varToScaleLitter] <-
          fscale * xvegClim[, varToScaleLitter, drop = FALSE]

        # Recalculate PNV total biomass
        varBiomass <- gsub("_AmountLive_", "_Biomass_", varToScale)
        varPctLive <- gsub("_AmountLive_", "_FractionLive_", varToScale)

        xvegClim[, varBiomass] <-
          xvegClim[, varToScale] / xvegClim[, varPctLive]
      }

    } else {
      # Don't overwrite variables from other sources
      varsVegClim <- setdiff(varsVegClim, varsVegHas)
    }


    if (length(varsVegClim) > 0L) {
      stopifnot(varsVegClim %in% varsVegAll)
      xveg[, varsVegClim] <- xvegClim[, varsVegClim, drop = FALSE]
      varsVegHas <- unique(c(varsVegHas, varsVegClim))

    } else {
      warning("No variables from climVeg")
    }
  }


  #--- ..* Cover: scale understory to sum to `1 - tree cover` ------
  if (isTRUE(veg_actions[["doScaleNonTreeCoverToUnderstory"]])) {
    tmp <- paste0("Composition_", pftsNonTree, "Fraction")
    tmpUnderstoryCover <- rowSums(xveg[, tmp, drop = FALSE])
    fucs <-
      (1 - xveg[, "Composition_TreeFraction", drop = TRUE]) /
      tmpUnderstoryCover

    ids <- which(
      !is.na(tmpUnderstoryCover) &
        tmpUnderstoryCover >= sqrt(.Machine[["double.eps"]])
    )
    xveg[ids, tmp] <- fucs[ids] * xveg[ids, tmp, drop = FALSE]
  }


  #--- ..* Cover: scale to sum to 1 ------
  if (isTRUE(veg_actions[["doScaleCover"]])) {
    tmp <- paste0("Composition_", pfts, "Fraction")
    tmpCover <- rowSums(xveg[, tmp, drop = FALSE])
    fucs <- 1 / tmpCover

    ids <- which(!is.na(tmpCover) & tmpCover >= sqrt(.Machine[["double.eps"]]))
    xveg[ids, tmp] <- fucs[ids] * xveg[ids, tmp, drop = FALSE]
  }


  #--- ..* Save to disk ------
  saveRDS(xveg[, c(vegHeader, varsVegAll), drop = FALSE], file = fname_xveg)
}


#------ . ------
#------ Prepare rSFSW2 input tables ------

#--- ..* Table: soil layer depth profile ------
fname_outDepths <- file.path(
  dir_results,
  paste0(
    prjTag,
    "_InputData_SoilLayers_v9_",
    paste0(soil_sources, collapse = "_"),
    "_v", versions[[vsel]],
    ".csv"
  )
)

if (!file.exists(fname_outDepths)) {
  tmp <- file.path(
    dir_rSFSW2,
    "1_Input",
    "SWRuns_InputData_SoilLayers_v9.csv"
  ) |>
    readInputFile()

  xsoils_layers <- array(
    dim = c(Nsim, ncol(tmp)),
    dimnames = list(NULL, colnames(tmp))
  ) |>
    as.data.frame()

  ids <- intersect(
    colnames(xsoils_layers),
    colnames(xsoils[["table_depths"]])
  )
  xsoils_layers[, ids] <- xsoils[["table_depths"]][, ids]
  xsoils_layers[["Label"]] <- xsim[["Label"]]

  utils::write.csv(xsoils_layers, file = fname_outDepths, row.names = FALSE)
}


#--- ..* Table: soil properties ------
fname_outSoils <- file.path(
  dir_results,
  paste0(
    prjTag,
    "_InputData_Soils_v13_",
    paste0(soil_sources, collapse = "_"),
    "_v", versions[[vsel]],
    ".csv"
  )
)

if (!file.exists(fname_outSoils)) {

  tmp <- file.path(
    dir_rSFSW2,
    "1_Input",
    "datafiles",
    "SWRuns_InputData_soils_v13.csv"
  ) |>
    readInputFile()

  xsoils_properties <- array(
    dim = c(1L + Nsim, ncol(tmp)),
    dimnames = list(NULL, colnames(tmp))
  ) |>
    as.data.frame()

  xsoils_properties[1L, "Label"] <- "UseInformationToCreateSoilWatRuns"
  xsoils_properties[-1L, "Label"] <- xsim[["Label"]]
  xsoils_properties[1L, -1L] <- 0L

  ids <- intersect(
    colnames(xsoils_properties),
    colnames(xsoils[["table_texture"]])
  )
  xsoils_properties[-1L, ids] <- xsoils[["table_texture"]][, ids, drop = FALSE]

  hasVals <- apply(
    xsoils[["table_texture"]][, ids, drop = FALSE],
    MARGIN = 2L,
    function(x) !all(is.na(x))
  )
  xsoils_properties[1L, 1L + which(hasVals)] <- 1L

  utils::write.csv(xsoils_properties, file = fname_outSoils, row.names = FALSE)
}


#--- ..* Table: vegetation properties ------
fname_veg <- file.path(
  dir_results,
  paste0(
    prjTag,
    "_InputData_Vegetation_v11_v", versions[[vsel]],
    ".csv"
  )
)

if (!file.exists(fname_veg) && !is.null(xveg)) {
  tmp <- file.path(
    dir_rSFSW2,
    "1_Input",
    "datafiles",
    "SWRuns_InputData_prod_v11.csv"
  ) |>
    readInputFile()


  xveg_properties <- array(
    dim = c(1L + Nsim, ncol(tmp)),
    dimnames = list(NULL, colnames(tmp))
  ) |>
    as.data.frame()

  xveg_properties[1L, "Label"] <- "UseInformationToCreateSoilWatRuns"
  xveg_properties[-1L, "Label"] <- xsim[["Label"]]
  xveg_properties[1L, -1L] <- 0L

  varsKeep <- intersect(colnames(xveg), colnames(xveg_properties))

  isNotAllNA <- apply(
    xveg[, varsKeep, drop = FALSE],
    MARGIN = 2L,
    function(x) !all(is.na(x))
  )

  varsKeep <- varsKeep[isNotAllNA]

  xveg_properties[-1L, varsKeep] <- xveg[, varsKeep, drop = FALSE]
  xveg_properties[1L, varsKeep] <- 1L

  utils::write.csv(xveg_properties, file = fname_veg, row.names = FALSE)
}


#--- ..* Table: main simulation specification ------
fname_main <- file.path(
  dir_results,
  paste0(
    prjTag,
    "_InputMain_",
    paste0(topo_sources, collapse = "-"), "_",
    paste0(soil_sources, collapse = "-"), "_v", versions[[vsel]],
    ".csv"
  )
)

if (!file.exists(fname_main)) {
  utils::write.csv(
    sf::st_drop_geometry(xsim),
    file = fname_main,
    row.names = FALSE
  )
}


#------ . ------
#------ . ------
