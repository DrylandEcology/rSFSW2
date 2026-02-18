# last update: 20260216_PIPOFutureManagement_SOILWAT2_simulations

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
  getNamespaceVersion("rSW2data") >= numeric_version("0.1.5"),
  getNamespaceVersion("rSW2st") >= numeric_version("0.3.2"),
  getNamespaceVersion("rSW2exter") >= numeric_version("0.3.2"),
  getNamespaceVersion("rSOILWAT2") >= numeric_version("6.6.0"),
  getNamespaceVersion("rSFSW2") >= numeric_version("5.0.0")
)
#------ . ------


#------ Settings ------
vsel <- 1L # select version to use now, i.e., index of `versions`

methodSW <- "SWNC" # method for SOILWAT2 experiment: "SWNC", "rSFSW2"

# Completion dates for run1-20240422
tasks <- list(
  dbW = TRUE, # completed:
  dbFuture = FALSE, # completed:
  site = FALSE, # completed:
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
#' Possible values: `"obs"`, `"climVeg2014"` (Bradford et al. 2014)
veg_sources <- "climVeg2014"


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
  imputeMissingObsWeather = TRUE,

  #' @param convertWeatherDBToNC A logical value.
  #' TRUE, convert the weather database to a netCDF input file
  #' if `methodSW` is `"SWNC"`
  convertWeatherDBToNC = TRUE
)

site_actions <- list(
  #' @param doTasClimForLowerSoilTemperatureBoundary A logical value
  #' TRUE, calculate long-term mean air temperature (replicating `rSFSW2`)
  doTasClimForLowerSoilTemperatureBoundary = FALSE
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

  #' @param minSoilDepth A numeric value. Sites with a shallower soil depth
  #' will be adjusted to this new value (unless it is `NA` or `NULL`).
  #' Soil properties of added layers are set to `NA` (see `imputeLOCF`).
  minSoilDepth_cm = NA,

  #' @param imputeLOCF A logical value.
  #' Impute missing soil values per location by shallow-depth value carried
  #' deeper (in analogy to `LOCF`), but do not impute missing values
  #' in the shallowest horizon/layer. See [rSW2data::impute_soils()].
  imputeLOCF = TRUE,

  #' @param allowAllSandClayOrSilt A logical value.
  #' Check of soil texture allows a 100% content of sand, clay, or silt.
  allowAllSandClayOrSilt = TRUE,

  #' @param estimateEvCo A logical value.
  #' Estimate evaporation coefficients based on soil properties.
  estimateEvCo = TRUE,

  #' @param roundEvCoLikeRSFSW2 A logical value.
  #' Round estimated evaporation coefficients to 4 digits; this replicates
  #' the behavior of [rSFSW2:::get_BareSoilEvapCoefs()]
  roundEvCoLikeRSFSW2 = FALSE
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
  interpretSucculentCoverPNV = "shrub",

  #' @param monthScalePNVByObs An integer value.
  #' The month `[1-12]` when observed biomass was collected, i.e.,
  #' the code will scale PNV to match live biomass amount for grasses and forbs
  #' and total biomass for shrubs.
  #' A negative value will skip this step.
  monthScalePNVByObs = -1L,

  #' @param estimateTrCo A logical value.
  #' Estimate rooting distributions (potential transpiration coefficients)
  #' based on vegetation composition and soil layer depths.
  estimateTrCo = FALSE,

  #' @param rootingProfileTreeNL A character string.
  #' Rooting profile basis for `"treeNL"`, e.g.,
  #' `"Bradfordetal2014_LodgepolePine"` (default),
  #' `"SchenkJackson2003_PCdry_shrubs"`
  rootingProfileTreeNL = NULL
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

dir_R <- file.path(dir_script, "R")
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
attributesProject <- NULL
source(file.path(dir_R, paste0("Functions__", prjTag, ".R")))

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
tmp <- regmatches(versions[[vsel]], regexpr("[[:digit:]]{8}", versions[[vsel]]))

fnames_dbW <- file.path(
  dir_sim,
  "0_WeatherDatabase",
  paste0(
    "dbWeatherData_",
    prjTag, "_", weather_sources, "_v", tmp,
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
      prjTag, "_", future_sources, "_v", tmp,
      ".sqlite3"
    )
  )
  names(fnames_dbFuture) <- future_sources
  dir.create(
    unique(dirname(fnames_dbFuture)), recursive = TRUE, showWarnings = FALSE
  )
}


if (identical(methodSW, "rSFSW2")) {
  dir_rSFSW2 <- file.path(
    dir_sim,
    paste0(
      versions_rSWSF[[vsel]], "_",
      prjTag, "_SOILWAT2_simulations")
  )
  if (!dir.exists(dir_rSFSW2)) {
    rSFSW2::setup_rSFSW2_project_infrastructure(dir_prj = dir_rSFSW2)
  }
}


#------ . ------
#------ Main inputs ------

fname_xsim1 <- file.path(dir_data, "xsim1.rds")

#--- ..* Sites/locations ------
if (file.exists(fname_xsim1)) {
  xsim <- readRDS(fname_xsim1)

  if ("ExpTrt_tag" %in% colnames(xsim)) {
    varsSimLabel <- unique(c(varsSimLabel, "ExpTrt_tag"))
  }

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
      c(
        varsSimLabel,
        setdiff(
          colnames(xtmp),
          c(varsCoords, "fname_weather", attr(xtmp, "sf_column"))
        )
      )
    ),
    prjCRS = prjCRS
  )

  #--- ..* Simulation setup with "design treatments" ------
  tmp <- create_xsim(
    xmain = xmain,
    varsSimLabel = varsSimLabel,
    expVegTrt = expVegTrt
  )

  xsim <- tmp[["xsim"]]
  varsSimLabel <- tmp[["varsSimLabel"]]

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

if (
  any(
    !file.exists(fname_xsim2),
    isTRUE(tasks[["dbW"]]),
    isTRUE(tasks[["site"]]) &&
      isTRUE(site_actions[["doTasClimForLowerSoilTemperatureBoundary"]]),
    isTRUE(weather_actions[["convertWeatherDBToNC"]]) &&
      identical(methodSW, "SWNC")
  )
) {

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
      weather_source = weather_sources[[kw]],
      uniqueWeather = TRUE
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
#------ Site conditions ------

fname_xsiteconditions <- file.path(dir_data, "xsim-siteConditions.rds")

if (all(file.exists(fname_xsiteconditions), !isTRUE(tasks[["site"]]))) {
  xsiteconds <- readRDS(fname_xsiteconditions)

} else {
  #--- ..* Create site conditions data container ------
  varsSiteConds <- c(
    if (site_actions[["doTasClimForLowerSoilTemperatureBoundary"]]) {
      paste("tasclim", weather_sources, sep = "-")
    }
  )
  xsiteconds <- xsim[, "Label", drop = FALSE]
  xsiteconds[, varsSiteConds] <- NA


  #------ ..* Long-term mean air temperature ------
  if (site_actions[["doTasClimForLowerSoilTemperatureBoundary"]]) {

    for (kw in seq_along(weather_sources)) {

      vartasclim <- paste("tasclim", weather_sources[[kw]], sep = "-")

      stopifnot(rSOILWAT2::dbW_setConnection(fnames_dbW[[kw]]))

      tasks_by_dbW <- setup_dbWeather(
        fdbWeather = fnames_dbW[[kw]],
        wfs = tmp_wfs[[kw]],
        weather_source = weather_sources[[kw]],
        uniqueWeather = FALSE
      )


      #--- Loop over sites
      pb <- utils::txtProgressBar(max = Nsim, style = 3L)

      for (ks in seq_len(Nsim)) {
        wdata <- rSOILWAT2::dbW_getWeatherData(
          Label = tasks_by_dbW[ks, "Label"]
        )
        tmp <- rSOILWAT2::calc_SiteClimate(wdata)
        xsiteconds[ks, vartasclim] <- mean(tmp[["meanMonthlyTempC"]])

        utils::setTxtProgressBar(pb, value = ks)
      }


      #--- Clean up
      close(pb)

      rSOILWAT2::dbW_disconnectConnection()
    }
  }


  #--- Write to disk
  saveRDS(xsiteconds, file = fname_xsiteconditions)
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
      dir_ned1_priority <- "/Volumes/BookDuo_12TB/BigData/GIS/Data/Topography/NED_USA/NED1"
      if (dir.exists(dir_ned1_priority)) {
        dir_ned1 <- dir_ned1_priority
      } else {
        dir_ned1 <- file.path(dir_dataraw, "NED1")
        dir.create(dir_ned1, recursive = TRUE, showWarnings = FALSE)
      }

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
    minSoilDepth = soil_actions[["minSoilDepth_cm"]],
    imputeLOCF = soil_actions[["imputeLOCF"]],
    allowAllSandClayOrSilt = soil_actions[["allowAllSandClayOrSilt"]],
    doFigures = doFigures,
    pathFigure = dir_figs
  )


  #--- ..* Estimate evaporation coefficient ------
  if (isTRUE(soil_actions[["estimateEvCo"]])) {
    tmpc <- list(
      depth = grep("depth_L[[:digit:]]+", colnames(xsoils[["table_depths"]])),
      sand = grep("Sand_L[[:digit:]]+", colnames(xsoils[["table_texture"]])),
      clay = grep("Clay_L[[:digit:]]+", colnames(xsoils[["table_texture"]]))
    )
    tmp_evco <- rSW2data::calc_BareSoilEvapCoefs(
      layers_depth = xsoils[["table_depths"]][, tmpc[["depth"]], drop = FALSE],
      sand = xsoils[["table_texture"]][, tmpc[["sand"]], drop = FALSE],
      clay = xsoils[["table_texture"]][, tmpc[["clay"]], drop = FALSE],
      method_bad_soils = "pass",
      noSoilValue = NA
    )

    if (isTRUE(soil_actions[["roundEvCoLikeRSFSW2"]])) {
      # rSFSW2::get_BareSoilEvapCoefs() rounds evco to 4 digits
      tmp_evco <- round(tmp_evco, digits = 4L)
    }

    tmp <- grep("EvapCoeff_L[[:digit:]]+", colnames(xsoils[["table_texture"]]))
    xsoils[["table_texture"]][, tmp] <- tmp_evco
  }


  #--- ..* Save final soils ------
  saveRDS(xsoils, file = fname_xsoils)
}



#--- ..* Map final soils ------
if (doFigures) {

  if (!exists("xSoilsSpatial")) {
    tmp <- createSoilsTemplate(
      xsim,
      soilHeader = c("Label", varsSimLabel, varsSoilID),
      varsCoords = varsSoilCoords,
      prjCRS = prjCRS
    )
    xSoilsSpatial <- tmp[["xSoilsSpatial"]]
  }


  # Map of soil sources
  fname_fig_map_source <- file.path(dir_figs, "Fig-map_soil-source.png")

  if (!file.exists(fname_fig_map_source)) {
    plot_map(
      fname = fname_fig_map_source,
      x = cbind(
        xSoilsSpatial,
        xsim[, varsSimLabel],
        source = paste(
          #xsoils[["table_keys"]][, varsSimLabel[[1L]]],
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

  xsim[, "Source_Soils"] <-
    xsoils[["table_keys"]][, "Source_Soils", drop = TRUE]

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
pftsV1 <- c("Tree", "Shrub", "Forb", "Grass")

# names of pftsV2 used by
#   - rSOILWAT2::estimate_PotNatVeg_roots()
#   - rSFSW2 SWRuns_InputData_soils_v13
pftsV2 <- stats::setNames(nm = rSOILWAT2::namesVegTypes("v2"))


fname_xveg <- file.path(dir_data, "xveg.rds")

if (all(file.exists(fname_xveg), !isTRUE(tasks[["veg"]]))) {
  xveg <- readRDS(fname_xveg)

} else {
  pftsNonTree <- pftsV1[-1L]

  varsVegHas <- NULL
  varsVegAll <- NULL
  varsVegObs <- NULL

  #--- ..* Create vegetation data container ------
  vegHeader <- c(
    "Label", varsSimLabel, varsVegID,
    grep("ExpTrt_", colnames(xsim), value = TRUE, fixed = TRUE)
  ) |>
    unique()

  res <- createVegetationTemplate(xsim, vegHeader, pftsV1)
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

    for (kv in seq_along(pftsV1)) {
      varsLitter <- paste0(pftsV1[[kv]], "_Litter_m", seq_len(12L))
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
    for (kv in seq_along(pftsV1)) {
      tmpVarCover <- paste0("Composition_", pftsV1[[kv]], "Fraction")

      if (isTRUE(tmpVarCover %in% varsVegHas)) {
        tmpVarBiomass <- intersect(
          paste0(pftsV1[[kv]], "_Biomass_m", seq_len(12L)),
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
          warning("No biomass for type ", shQuote(pftsV1[[kv]]), " to scale.")
        }
      } else {
        warning("No cover for type ", shQuote(pftsV1[[kv]]), " to scale biomass.")
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

    for (kv in seq_along(pftsV1)) {
      tmpVarCover <- paste0("Composition_", pftsV1[[kv]], "Fraction")

      if (isTRUE(tmpVarCover %in% varsVegHas)) {
        tmpVarLitter <- intersect(
          paste0(pftsV1[[kv]], "_Litter_m", seq_len(12L)),
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
          warning("No litter for type ", shQuote(pftsV1[[kv]]), " to scale.")
        }
      } else {
        warning("No cover for type ", shQuote(pftsV1[[kv]]), " to scale litter.")
      }
    }
  }


  #--- ..* Data source: climate relationships (Bradford et al. 2014) ------
  if (any(veg_sources == "climVeg2014")) {
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
        stats::setNames(
          nm = c("Biomass", "FractionLive", "BiomassLive", "Litter", "LAIconv")
        ),
        function(tag) paste0("_", tag, "_m", seq_len(12L))
      )

      stopifnot(veg_actions[["interpretSucculentCoverPNV"]] %in% pftsV1)
      varSucCov <- paste0(
        "Composition_", veg_actions[["interpretSucculentCoverPNV"]], "Fraction"
      )


      #--- ....** Estimate PNV ------
      monVegDefault <- slot(
        rSOILWAT2::get_swProd(rSOILWAT2::sw_exampleData), "MonthlyVeg"
      )

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
          # Circa 2025-Oct to 2026-Feb-04, this was by mistake
          # `mean_monthly_ppt_mm = clim[["meanMonthlyPPTcm"]]`
          # -- see also SOILWAT2 issue #517
          mean_monthly_ppt_mm = 10 * clim[["meanMonthlyPPTcm"]],
          mean_monthly_Temp_C = clim[["meanMonthlyTempC"]],
          dailyC4vars = clim[["dailyC4vars"]],
          isNorth = xsim[ks, "Y_WGS84", drop = TRUE] > 0,
          shrub_limit = 0.2,
          fix_succulents = TRUE,
          Succulents_Fraction = 0,
          fix_annuals = TRUE,
          Annuals_Fraction = 0,
          fix_trees = TRUE,
          Trees_Fraction = 0,
          fix_BareGround = TRUE,
          BareGround_Fraction = 0,
          fill_empty_with_BareGround = TRUE,
          warn_extrapolation = TRUE
        )

        # rSFSW2 still uses V1
        xvegClim[ks, "Composition_TreeFraction"] <-
          covclim[["Rel_Abundance_L0"]][["Trees"]]

        xvegClim[ks, "Composition_ShrubFraction"] <-
          covclim[["Rel_Abundance_L0"]][["Shrubs"]]

        xvegClim[ks, "Composition_GrassFraction"] <-
          covclim[["Rel_Abundance_L0"]][["Grasses_C3"]] +
          covclim[["Rel_Abundance_L0"]][["Grasses_C4"]] +
          covclim[["Rel_Abundance_L0"]][["Grasses_Annuals"]]

        xvegClim[ks, "Composition_ForbFraction"] <-
          covclim[["Rel_Abundance_L0"]][["Forbs"]]

        xvegClim[ks, "Composition_BareGroundFraction"] <-
          covclim[["Rel_Abundance_L0"]][["BareGround"]]

        xvegClim[ks, varSucCov] <- xvegClim[ks, varSucCov] +
          covclim[["Rel_Abundance_L0"]][["Succulents"]]


        # PNV biomass
        vegclim <- rSOILWAT2::estimate_PotNatVeg_biomass(
          target_temp = clim[["meanMonthlyTempC"]],
          target_MAP_mm = 10 * clim[["MAP_cm"]],
          do_adjust_phenology = TRUE,
          do_adjust_biomass = TRUE,
          fgrass_c3c4ann = if (
            isTRUE(sum(covclim[["Grasses"]]) < sqrt(.Machine[["double.eps"]]))
          ) {
            c(1, 0, 0)
          } else {
            covclim[["Grasses"]]
          }
        )

        for (pft in pftsNonTree) {
            tmpp <- switch(EXPR = tolower(pft), shrub = "shrub", "grass")
            tmpp2 <- switch(EXPR = tolower(pft), shrub = "shrub", "grassC3")

          xvegClim[ks, paste0(pft, tmpTags[["Biomass"]])] <-
            vegclim[[tmpp]][, "Biomass", drop = TRUE]

          xvegClim[ks, paste0(pft, tmpTags[["FractionLive"]])] <-
            vegclim[[tmpp]][, "Perc.Live", drop = TRUE]

          xvegClim[ks, paste0(pft, tmpTags[["BiomassLive"]])] <-
            vegclim[[tmpp]][, "Amount.Live", drop = TRUE]

          xvegClim[ks, paste0(pft, tmpTags[["Litter"]])] <-
            vegclim[[tmpp]][, "Litter", drop = TRUE]

          xvegClim[ks, paste0(pft, tmpTags[["LAIconv"]])] <-
            monVegDefault[[tmpp2]][, "LAI_conv", drop = TRUE]
        }

        for (pft in setdiff(pftsV1, pftsNonTree)) {
          tmpp2 <- switch(EXPR = tolower(pft), tree = "treeNL", stop(pft))
          xvegClim[ks, paste0(pft, tmpTags[["Biomass"]])] <-
            monVegDefault[[tmpp2]][, "Biomass", drop = TRUE]

          xvegClim[ks, paste0(pft, tmpTags[["FractionLive"]])] <-
            monVegDefault[[tmpp2]][, "Live_pct", drop = TRUE]

          xvegClim[ks, paste0(pft, tmpTags[["BiomassLive"]])] <-
            monVegDefault[[tmpp2]][, "Biomass", drop = TRUE] *
            monVegDefault[[tmpp2]][, "Live_pct", drop = TRUE]

          xvegClim[ks, paste0(pft, tmpTags[["Litter"]])] <-
            monVegDefault[[tmpp2]][, "Litter", drop = TRUE]

          xvegClim[ks, paste0(pft, tmpTags[["LAIconv"]])] <-
            monVegDefault[[tmpp2]][, "LAI_conv", drop = TRUE]
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
        shrub = paste0(
          "shrub_Biomass_m", veg_actions[["monthScalePNVByObs"]]
        )
      ) |> lapply(
        function(var) intersect(var, varsVegObs)
      )

      varObsBiomassLiveToScale <- list(
        grass = paste0(
          "grass_BiomassLive_m", veg_actions[["monthScalePNVByObs"]]
        ),
        forbs = paste0(
          "forbs_BiomassLive_m", veg_actions[["monthScalePNVByObs"]]
        )
      ) |> lapply(
        function(var) intersect(var, varsVegObs)
      )

      stopifnot(
        c("obs", "climVeg2014") %in% veg_sources,
        names(varObsBiomassToScale) %in% pftsV1,
        names(varObsBiomassLiveToScale) %in% pftsV1,
        length(
          intersect(names(varObsBiomassToScale), names(varObsBiomassLiveToScale))
        ) == 0L,
        grepl("_Biomass_", unlist(varObsBiomassToScale), fixed = TRUE),
        grepl("_BiomassLive_", unlist(varObsBiomassLiveToScale), fixed = TRUE)
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
      for (kv in seq_along(varObsBiomassLiveToScale)) {
        fscale <-
          xveg[, varObsBiomassLiveToScale[[kv]], drop = TRUE] /
          xvegClim[, varObsBiomassLiveToScale[[kv]], drop = TRUE]

        varToScale <- paste0(
          sub("[[:digit:]]{1,2}", "", varObsBiomassLiveToScale[[kv]]),
          seq_len(12L)
        )

        varToScaleLitter <- gsub("_BiomassLive_", "_Litter_", varToScale)

        xvegClim[, varToScale] <-
          fscale * xvegClim[, varToScale, drop = FALSE]

        xvegClim[, varToScaleLitter] <-
          fscale * xvegClim[, varToScaleLitter, drop = FALSE]

        # Recalculate PNV total biomass
        varBiomass <- gsub("_BiomassLive_", "_Biomass_", varToScale)
        varPctLive <- gsub("_BiomassLive_", "_FractionLive_", varToScale)

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


  #--- ..* Check units of cover ------
  tmp <- intersect(paste0("Composition_", pftsV1, "Fraction"), colnames(xveg))
  if (max(colMeans(xveg[, tmp, drop = FALSE])) > 1.1) {
    stop(
      "Some vegetation cover values are substantially larger than 1 ",
      "suggesting incorrect units, e.g., % instead of fractions."
    )
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
    tmp <- paste0("Composition_", pftsV1, "Fraction")
    tmpCover <- rowSums(xveg[, tmp, drop = FALSE])
    fucs <- 1 / tmpCover

    ids <- which(!is.na(tmpCover) & tmpCover >= sqrt(.Machine[["double.eps"]]))
    xveg[ids, tmp] <- fucs[ids] * xveg[ids, tmp, drop = FALSE]
  }


  #--- ..* Save to disk ------
  saveRDS(xveg[, c(vegHeader, varsVegAll), drop = FALSE], file = fname_xveg)
}



#------ . ------
#------ Vegetation - rooting profiles ------
fname_xsoils2 <- file.path(dir_data, "xsoils2.rds")

hadXSoil2 <- file.exists(fname_xsoils2)

if (hadXSoil2) {
  xsoils <- readRDS(fname_xsoils2)
}


#------ .. * Update soil data: vegetation types v1 to v2 ------
idsVegV1 <- lapply(
  pftsV1,
  function(pftV1) {
    grep(paste0("^", pftV1, "_"), x = colnames(xsoils[["table_texture"]]))
  }
)

nHasVegV1 <- lengths(idsVegV1)

if (all(nHasVegV1 > 0L) && unique(diff(nHasVegV1)) == 0L) {

  pftV2mapping <- rSOILWAT2::mapVegTypes("2from1", order = "SOILWAT2")

  for (kv in seq_along(idsVegV1)) {
    tmp <- colnames(xsoils[["table_texture"]])[idsVegV1[[kv]]]
    if (length(tmp) > 0L) {
      kv2 <- which(pftV2mapping == kv)
      tmp <- gsub(
        pattern = paste0(pftsV1[[kv]], "_TranspCoeff_"),
        replacement = paste0("TrCo_", pftsV2[[kv2]], "_"),
        x = tmp,
        fixed = TRUE
      )
      colnames(xsoils[["table_texture"]])[idsVegV1[[kv]]] <- tmp
    }
  }

  if (hadXSoil2) {
    saveRDS(xsoils, file = fname_xsoils2)
  }
}



if (!hadXSoil2) {
  #--- ..* Estimate transpiration coefficient ------
  if (isTRUE(veg_actions[["estimateTrCo"]])) {

    tmpcd <- grep("depth_L[[:digit:]]+", colnames(xsoils[["table_depths"]]))

    tmp_trco <- apply(
      xsoils[["table_depths"]][, tmpcd, drop = FALSE],
      MARGIN = 1L,
      function(x) {
        if (!all(is.na(x))) {
          res <- rSOILWAT2::estimate_PotNatVeg_roots(
            layers_depth = x,
            trco_type_by_veg = list(
              treeNL = if (is.null(veg_actions[["rootingProfileTreeNL"]])) {
                "Bradfordetal2014_LodgepolePine"
              } else {
                veg_actions[["rootingProfileTreeNL"]]
              },
              treeBL = "Bradfordetal2014_LodgepolePine",
              shrub = "SchenkJackson2003_PCdry_shrubs",
              forbs = "SchenkJackson2003_PCdry_forbs",
              grassC3 = "SchenkJackson2003_PCdry_grasses",
              grassC4 = "SchenkJackson2003_PCdry_grasses",
              grass_annuals = "Jacksonetal1996_crops"
            ),
            fgrass_c3c4ann = c(
              grassC3 = 0.5, grassC4 = 0.5, grass_annuals = 0
            )
          )
          res[is.na(x)] <- NA_real_
          res[, names(pftsV2), drop = FALSE]
        } else {
          array(
            NA_real_,
            dim = c(length(x), length(pftsV2)),
            dimnames = list(NULL, names(pftsV2))
          )
        }
      },
      simplify = FALSE
    )

    # Convert list of trco to matrix with sites x (layer-pft)
    for (kv in seq_along(pftsV2)) {
      tmpx <- vapply(
        tmp_trco,
        FUN = function(x) x[, names(pftsV2)[[kv]], drop = TRUE],
        FUN.VALUE = rep(NA_real_, length(tmpcd))
      )

      tmpv <- grep(
        paste0("TrCo_", names(pftsV2)[[kv]], "_L[[:digit:]]+"),
        colnames(xsoils[["table_texture"]])
      )

      if (length(tmpv) == 0L) {
        xsoils[["table_texture"]] <- cbind(
          xsoils[["table_texture"]],
          array(
            data = NA_real_,
            dim = c(nrow(xsoils[["table_texture"]]), length(tmpcd)),
            dimnames = list(
              NULL,
              paste0("TrCo_", names(pftsV2)[[kv]], "_L", seq_along(tmpcd))
            )
          )
        )
      }

      xsoils[["table_texture"]][, tmpv] <- t(tmpx)
    }
  }


  #--- ..* Save updated soils ------
  saveRDS(xsoils, file = fname_xsoils2)
}


#------ . ------
#--- Output: documentation table ------
# rSFSW2 main simulation specification
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
#------ Output: rSFSW2 input tables ------
if (identical(methodSW, "rSFSW2")) {
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
    xtt <- xsoils[["table_texture"]]

    #------ .. * Downgrade soil data: vegetation types v2 to v1 for rSFSW2 ------
    idsVegV2 <- lapply(
      pftsV2,
      function(pftsV2) {
        grep(paste0("_", pftsV2, "_"), x = colnames(xtt))
      }
    )

    nHasVegV2 <- lengths(idsVegV2)

    if (all(nHasVegV2 > 0L) && unique(diff(nHasVegV2)) == 0L) {

      pftV2mapping <- rSOILWAT2::mapVegTypes("2from1", order = "SOILWAT2")
      idsVegV2to1 <- idsVegV2[pftV2mapping > 0L]

      for (kv in seq_along(idsVegV2to1)) {
        tmp <- colnames(xtt)[idsVegV2to1[[kv]]]
        if (length(tmp) > 0L) {
          tmp <- gsub(
            pattern = paste0("TrCo_", names(idsVegV2to1)[[kv]], "_"),
            replacement = paste0(pftsV1[[kv]], "_TranspCoeff_"),
            x = tmp,
            fixed = TRUE
          )
          colnames(xtt)[idsVegV2to1[[kv]]] <- tmp
        }
      }
    }


    #--- ....** Copy soil properties to rSFSW2 format ------
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
      colnames(xtt)
    )
    xsoils_properties[-1L, ids] <- xtt[, ids, drop = FALSE]

    hasVals <- apply(
      xsoils_properties[-1L, , drop = FALSE],
      MARGIN = 2L,
      function(x) !all(is.na(x))
    )
    xsoils_properties[1L, setdiff(which(hasVals), 1L)] <- 1L

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
}



#------ . ------
#------ Output: ncSOILWAT2 inputs ------
if (identical(methodSW, "SWNC")) {

  #--- ..* ncDomain ------
  fname_domain <- file.path(
    dir_results, "SW2ncDomain", paste0("domain__", prjTag, ".nc")
  )

  domain_bbox <- sf::st_bbox(xsim)

  cat("Values for domain.txt:\n")
  cat(sprintf("Domain    %s\n", "s"))
  cat(sprintf("nDimS     %d\n", Nsim))
  cat(sprintf("xmin_bbox %.6f\n", domain_bbox[["xmin"]]))
  cat(sprintf("ymin_bbox %.6f\n", domain_bbox[["ymin"]]))
  cat(sprintf("xmax_bbox %.6f\n", domain_bbox[["xmax"]]))
  cat(sprintf("ymax_bbox %.6f\n", domain_bbox[["ymax"]]))


  if (!file.exists(fname_domain)) {
    dir.create(dirname(fname_domain), recursive = TRUE, showWarnings = FALSE)

    #--- Prepare data
    domain_sites <- array(seq_len(Nsim), dim = c(Nsim, 1L))

    #--- Write to disk
    rSW2st::create_netCDF(
      filename = fname_domain,
      xyspace = xsim[, 0L],
      data = domain_sites,
      data_str = "s",
      data_type = "integer",
      var_attributes = list(
        name = "domain",
        long_name = "simulation domain",
        units = "1",
        grid_mapping = "crs",
        coordinates = "latitude longitude site"
      ),
      xy_attributes = list(
        name = c("longitude", "latitude"),
        standard_name = c("longitude", "latitude"),
        long_name = c("longitude", "latitude"),
        units = c("degrees_east", "degrees_north"),
        axis = c("X", "Y")
      ),
      crs_attributes = list(
        long_name = "WGS84",
        #crs_wkt = sf::st_crs("WGS84")$Wkt,
        crs_wkt = "GEOGCS[\"WGS 84\",DATUM[\"WGS_1984\",SPHEROID[\"WGS 84\",6378137,298.257223563,AUTHORITY[\"EPSG\",\"7030\"]],AUTHORITY[\"EPSG\",\"6326\"]],PRIMEM[\"Greenwich\",0,AUTHORITY[\"EPSG\",\"8901\"]],UNIT[\"degree\",0.0174532925199433,AUTHORITY[\"EPSG\",\"9122\"]],AUTHORITY[\"EPSG\",\"4326\"]]",
        grid_mapping_name = "latitude_longitude",
        longitude_of_prime_meridian = 0.0,
        semi_major_axis = 6378137.0,
        inverse_flattening = 298.257223563
      ),
      global_attributes = attributesProject(),
      nc_compression = TRUE
    )
  }


  #--- ..* ncTopo ------
  fname_topo <- file.path(
    dir_results,
    "SW2ncTopo",
    paste0("topo__", prjTag, "__", paste0(topo_sources, collapse = "-"), ".nc")
  )

  if (!file.exists(fname_topo)) {
    dir.create(dirname(fname_topo), recursive = TRUE, showWarnings = FALSE)

    #--- Prepare data
    ncVars <- c(
      elevation = "ELEV_m",
      slope = "Slope_deg",
      aspect = "Aspect_deg"
    )

    #--- Write to disk
    file.copy(from = fname_domain, to = fname_topo)
    xnc <- RNetCDF::open.nc(fname_topo, write = TRUE)

    for (kv in seq_along(ncVars)) {
      tmp_vals <- xsim[, ncVars[[kv]], drop = TRUE]
      if (all(is.na(tmp_vals))) next

      tmp_var <- names(ncVars)[[kv]]
      tmp_units <- switch(
        EXPR = tmp_var,
        elevation = "m",
        slope = "degree",
        aspect = "degree"
      )
      tmp_attr <- switch(
        EXPR = tmp_var,
        elevation = NULL,
        slope = list(comment = "no slope = 0, vertical surface = 90"),
        aspect = list(
          comment = paste(
            "surface azimuth angle (degrees): S=0, E=-90, N=180 or -180, W=90;",
            "ignored if slope = 0 or aspect takes a missing value"
          )
        )
      )

      rSW2st::setVariableNCSW(
        xnc,
        varName = tmp_var,
        dimensions = "site",
        units = tmp_units,
        coordinates = "latitude longitude site",
        grid_mapping = "crs",
        dataType = "double",
        values = tmp_vals,
        count = Nsim,
        attributes = tmp_attr
      )
    }

    RNetCDF::close.nc(xnc)
  }


  #--- ..* ncSoil ------
  fname_soils <- file.path(
    dir_results,
    "SW2ncSoils",
    paste0("soils__", prjTag, "__", paste0(soil_sources, collapse = "-"), ".nc")
  )

  if (!file.exists(fname_soils)) {
    dir.create(dirname(fname_soils), recursive = TRUE, showWarnings = FALSE)

    #--- Prepare data
    ncVars <- c(
      hzdpt = "depth_L",
      hzthk = "depth_L",
      dbovendry = "Matricd_L",
      fragvol = "GravelContent_L",
      sandtotal = "Sand_L",
      silttotal = NA,
      claytotal = "Clay_L",
      som = "SOM_L",
      evc = "EvapCoeff_L",
      trc_treeNL = "TrCo_treeNL_L",
      trc_treeBL = "TrCo_treeBL_L",
      trc_shrub = "TrCo_shrub_L",
      trc_forbs = "TrCo_forbs_L",
      trc_grassC3 = "TrCo_grassC3_L",
      trc_grassC4 = "TrCo_grassC4_L"
    )

    nMaxSoilLayers <- max(xsoils[["table_depths"]][, "N_horizons", drop = TRUE])


    #--- Write to disk
    file.copy(from = fname_domain, to = fname_soils)
    xnc <- RNetCDF::open.nc(fname_soils, write = TRUE)


    #--- Add vertical axis
    rSW2st::setAxisVerticalNCSW(
      xnc,
      verticalValues = seq_len(nMaxSoilLayers),
      verticalType = "layers"
    )


    #--- Add variables
    for (kv in seq_along(ncVars)) {
      tmp_var <- names(ncVars)[[kv]]

      tmpx <- switch(
        EXPR = tmp_var,
        hzdpt = ,
        hzthk = xsoils[["table_depths"]],
        xsoils[["table_texture"]]
      )

      if (
        identical(grepl("silt", tmp_var, ignore.case = TRUE) &&
          isTRUE(is.na(ncVars[[kv]]))
      ) {
        idSand <- grep("sand", colnames(tmpx), ignore.case = TRUE)
        idClay <- grep("clay", colnames(tmpx), ignore.case = TRUE)
        tmp_sand <-  tmpx[, idSand, drop = FALSE]
        tmp_clay <-  tmpx[, idClay, drop = FALSE]
        tmp_vals <- data.matrix(
          1 - (tmp_sand + tmp_clay)
        )[, seq_len(nMaxSoilLayers), drop = FALSE]

      } else if (identical(tmp_var, "hzthk")) {
        tmp <- cbind(
          0,
          tmpx[, paste0(ncVars[[kv]], seq_len(nMaxSoilLayers)), drop = FALSE]
        )
        tmp <- apply(tmp, MARGIN = 1L, diff)
        tmp_vals <- data.matrix(t(tmp))

      } else {
        vcn <- paste0(ncVars[[kv]], seq_len(nMaxSoilLayers))
        if (!all(vcn %in% colnames(tmpx))) next
        tmp_vals <- data.matrix(tmpx[, vcn, drop = FALSE])
      }

      tmp_longname <- switch(
        EXPR = tmp_var,
        hzdpt = "depth to soil layer bottom",
        hzthk = "thickness (width) of soil layer",
        dbovendry = "density of matric soil",
        fragvol = "coarse fragments in bulk soil",
        sandtotal = "sand content in the less than 2 mm soil fraction",
        silttotal = "silt content in the less than 2 mm soil fraction",
        claytotal = "clay content in the less than 2 mm soil fraction",
        som = "soil organic material in the less than 2 mm soil fraction",
        evc = "potential evaporation coefficient",
        trc_treeNL = "fractional needle-leaved tree rooting profile",
        trc_treeBL = "fractional broad-leaved tree rooting profile",
        trc_shrub = "fractional shrub rooting profile",
        trc_forbs = "fractional forb rooting profile",
        trc_grassC3 = "fractional C3-grass rooting profile",
        trc_grassC4 = "fractional C4-grass rooting profile"
      )
      tmp_units <- switch(
        EXPR = tmp_var,
        hzdpt = "cm",
        hzthk = "cm",
        dbovendry = "g cm-3",
        fragvol = "cm3 cm-3",
        sandtotal = "g g-1",
        silttotal = "g g-1",
        claytotal = "g g-1",
        som = "cm3 cm-3",
        evc = "1",
        trc_treeNL = "1",
        trc_treeBL = "1",
        trc_shrub = "1",
        trc_forbs = "1",
        trc_grassC3 = "1",
        trc_grassC4 = "1"
      )

      rSW2st::setVariableNCSW(
        xnc,
        varName = tmp_var,
        dimensions = c("vertical", "site"),
        long_name = tmp_longname,
        units = tmp_units,
        coordinates = "latitude longitude site",
        grid_mapping = "crs",
        dataType = "double",
        values = t(tmp_vals),
        count = c(nMaxSoilLayers, Nsim),
        attributes = NULL
      )
    }

    RNetCDF::close.nc(xnc)
  }


  #--- ..* ncVeg ------
  fname_veg <- file.path(
    dir_results,
    "SW2ncVeg",
    paste0("veg__", prjTag, "__", paste0(veg_sources, collapse = "-"), ".nc")
  )

  if (!file.exists(fname_veg)) {
    dir.create(dirname(fname_veg), recursive = TRUE, showWarnings = FALSE)


    #--- ....** Update vegetation types from v1 to v2 ------
    idsVegV1 <- lapply(
      pftsV1,
      function(pftV1) {
        c(
          grep(paste0("_", pftV1, "Fraction"), x = colnames(xveg)),
          grep(paste0("^", pftV1, "_"), x = colnames(xveg))
        )
      }
    )

    pftV2mapping <- rSOILWAT2::mapVegTypes("2from1", order = "SOILWAT2")

    for (kv in seq_along(idsVegV1)) {
      tmp <- colnames(xveg)[idsVegV1[[kv]]]
      if (length(tmp) > 0L) {
        kv2 <- which(pftV2mapping == kv)
        tmp <- gsub(pftsV1[[kv]], pftsV2[[kv2]], tmp, fixed = TRUE)
        colnames(xveg)[idsVegV1[[kv]]] <- tmp
      }
    }

    addVarsCover <- setdiff(
      paste0("Composition_", pftsV2, "Fraction"),
      colnames(xveg)
    )
    if (length(addVarsCover) > 0L) {
      tmp <- array(
        data = 0,
        dim = c(nrow(xveg), length(addVarsCover)),
        dimnames = list(NULL, addVarsCover)
      )
      xveg <- cbind(xveg, tmp)
    }


    #--- Convert all data columns to numeric (particularly logical NAs)
    for (kc in seq_len(ncol(xveg))) {
      if (mode(xveg[[kc]]) == "logical") {
        xveg[[kc]] <- as.numeric(xveg[[kc]])
      }
    }


    #--- ....** Set up nc for vegetation ------
    file.copy(from = fname_domain, to = fname_veg)
    xnc <- RNetCDF::open.nc(fname_veg, write = TRUE)

    NMonths <- 12L
    dataType <- "NC_DOUBLE"

    xDim <- list(sp = "site", clim = "time")

    varAttrSp <- list(
      coordinates = paste("latitude longitude", xDim[["sp"]]),
      coordinatesClim = paste(
        "latitude longitude", xDim[["sp"]], xDim[["clim"]]
      ),
      grid_mapping = "crs"
    )

    chunkVarClim <- c(NMonths, ceiling(Nsim / 3L))


    rSW2st::setGlobalAttributesNCSW(xnc, attributesProject())

    rSW2st::setGlobalAttributesNCSW(
      xnc,
      attributes = c(frequency = "month", featureType = "timeSeries")
    )

    #--- ......*** inVeg: month ------
    rSW2st::setAxisMonthClimatologyNCSW(
      xnc,
      startYear = simYears[[1L]],
      endYear = simYears[[length(simYears)]]
    )

    #--- ......*** inVeg: fcover_bg ------
    rSW2st::setVariableNCSW(
      xnc,
      varName = "fcover_bg",
      long_name = "fractional cover of bare ground",
      dimensions = xDim[["sp"]],
      units = "1",
      coordinates = varAttrSp[["coordinates"]],
      grid_mapping = varAttrSp[["grid_mapping"]],
      dataType = dataType,
      values = NULL
    )


    for (k in seq_along(pftsV2)) {
      #--- ......*** inVeg: fcover_[veg] ------
      rSW2st::setVariableNCSW(
        xnc,
        varName = paste0("fcover_", pftsV2[[k]]),
        long_name = paste("fractional cover of", pftsV2[[k]]),
        dimensions = xDim[["sp"]],
        var_chunksizes_xyzt = chunkVarClim,
        units = "1",
        coordinates = varAttrSp[["coordinates"]],
        grid_mapping = varAttrSp[["grid_mapping"]],
        dataType = dataType,
        values = NULL
      )

      #--- ......*** inVeg: litter_[veg] ------
      rSW2st::setVariableNCSW(
        xnc,
        varName = paste0("litter_", pftsV2[[k]]),
        long_name = paste("litter of", pftsV2[[k]]),
        dimensions = c(xDim[["clim"]], xDim[["sp"]]),
        var_chunksizes_xyzt = chunkVarClim,
        units = "g m-2",
        coordinates = varAttrSp[["coordinatesClim"]],
        grid_mapping = varAttrSp[["grid_mapping"]],
        dataType = dataType,
        values = NULL
      )

      #--- ......*** inVeg: biomass_[veg] ------
      rSW2st::setVariableNCSW(
        xnc,
        varName = paste0("biomass_", pftsV2[[k]]),
        long_name = paste("total biomass of", pftsV2[[k]]),
        dimensions = c(xDim[["clim"]], xDim[["sp"]]),
        var_chunksizes_xyzt = chunkVarClim,
        units = "g m-2",
        coordinates = varAttrSp[["coordinatesClim"]],
        grid_mapping = varAttrSp[["grid_mapping"]],
        dataType = dataType,
        values = NULL
      )

      #--- ......*** inVeg: live_[veg] ------
      rSW2st::setVariableNCSW(
        xnc,
        varName = paste0("live_", pftsV2[[k]]),
        long_name = paste(
          "fraction of biomass of", pftsV2[[k]], "that is living"
        ),
        dimensions = c(xDim[["clim"]], xDim[["sp"]]),
        var_chunksizes_xyzt = chunkVarClim,
        units = "g m-2",
        coordinates = varAttrSp[["coordinatesClim"]],
        grid_mapping = varAttrSp[["grid_mapping"]],
        dataType = dataType,
        values = NULL
      )

      #--- ......*** inVeg: convLAI_[veg] ------
      rSW2st::setVariableNCSW(
        xnc,
        varName = paste0("convLAI_", pftsV2[[k]]),
        long_name = paste(
          "biomass needed to produce LAI = 1 of", pftsV2[[k]]
        ),
        var_chunksizes_xyzt = chunkVarClim,
        dimensions = c(xDim[["clim"]], xDim[["sp"]]),
        units = "1",
        coordinates = varAttrSp[["coordinatesClim"]],
        grid_mapping = varAttrSp[["grid_mapping"]],
        dataType = dataType,
        values = NULL
      )
    }


    #--- ....** Write pre-calculated values to ncVeg ------
    startXY <- 1L
    startPXY <- c(1L, startXY)
    countXY <- Nsim
    countPXY <- c(NMonths, countXY)


    #--- Loop over vegetation types
    for (k in seq_along(pftsV2)) {
      #--- ......*** inVeg: fcover_[veg] ------
      tmpv <- grep(
        paste0("Composition_", pftsV2[[k]]),
        x = colnames(xveg),
        value = TRUE,
        ignore.case = TRUE
      )
      if (length(tmpv) != 1L) next

      rSW2st::setVariableNCSW(
        xnc,
        varName = paste0("fcover_", pftsV2[[k]]),
        values = xveg[, tmpv, drop = TRUE],
        start = startXY,
        count = countXY
      )


      #--- ......*** inVeg: litter_[veg] ------
      tmpv <- grep(
        paste0(pftsV2[[k]], "_Litter_m[[:digit:]]{1,2}"),
        x = colnames(xveg),
        value = TRUE,
        ignore.case = TRUE
      )
      if (length(tmpv) != 12L) next

      rSW2st::setVariableNCSW(
        xnc,
        varName = paste0("litter_", pftsV2[[k]]),
        values = t(xveg[, tmpv, drop = FALSE]),
        start = startPXY,
        count = countPXY
      )

      #--- ......*** inVeg: biomass_[veg] ------
      tmpv <- grep(
        paste0(pftsV2[[k]], "_Biomass_m[[:digit:]]{1,2}"),
        x = colnames(xveg),
        value = TRUE,
        ignore.case = TRUE
      )
      rSW2st::setVariableNCSW(
        xnc,
        varName = paste0("biomass_", pftsV2[[k]]),
        values = t(xveg[, tmpv, drop = FALSE]),
        start = startPXY,
        count = countPXY
      )

      #--- ......*** inVeg: live_[veg] ------
      tmpv <- grep(
        paste0(pftsV2[[k]], "_FractionLive_m[[:digit:]]{1,2}"),
        x = colnames(xveg),
        value = TRUE,
        ignore.case = TRUE
      )
      rSW2st::setVariableNCSW(
        xnc,
        varName = paste0("live_", pftsV2[[k]]),
        values = t(xveg[, tmpv, drop = FALSE]),
        start = startPXY,
        count = countPXY
      )

      #--- ......*** inVeg: convLAI_[veg] ------
      tmpv <- grep(
        paste0(pftsV2[[k]], "_LAIconv_m[[:digit:]]{1,2}"),
        x = colnames(xveg),
        value = TRUE,
        ignore.case = TRUE
      )
      rSW2st::setVariableNCSW(
        xnc,
        varName = paste0("convLAI_", pftsV2[[k]]),
        values = t(xveg[, tmpv, drop = FALSE]),
        start = startPXY,
        count = countPXY
      )
    }


    #--- ......*** inVeg: fcover_bg ------
    hasBG <- "Composition_BareGroundFraction" %in% colnames(xveg)

    rSW2st::setVariableNCSW(
      xnc,
      varName = "fcover_bg",
      values = if (hasBG) {
        xveg[, "Composition_BareGroundFraction", drop = TRUE]
      } else {
        array(data = 0, dim = countXY)
      },
      start = startXY,
      count = countXY
    )


    #--- ....** Clean up ------
    RNetCDF::close.nc(xnc)
  }


  #--- ..* ncWeather ------
  if (isTRUE(weather_actions[["convertWeatherDBToNC"]])) {

    #--- ....** Metadata ------
    dataType <- "NC_DOUBLE"

    xDim <- list(sp = "site", time = "time")

    varAttrSp <- list(
      coordinates = paste("latitude longitude", xDim[["sp"]]),
      grid_mapping = "crs"
    )
    ntime <- seq.Date(
      from = as.Date(paste0(simYears[[1L]], "-01-01")),
      to = as.Date(paste0(simYears[[length(simYears)]], "-12-31")),
      by = "day"
    ) |>
      length()


    #--- ....** Weather datasets ------
    if (is.null(future_sources)) {
      meteo_sources <- weather_sources
      fnames_dbMeteo <- fnames_dbW
      tmpMeteo_wfs <- tmp_wfs
    } else {
      meteo_sources <- c(weather_sources, future_sources)
      fnames_dbMeteo <- c(fnames_dbW, fnames_dbFuture)
      tmpMeteo_wfs <- stop("tmp_wfs not implemented for future sources")
    }

    for (kw in seq_along(meteo_sources)) {

      fname_weather <- file.path(
        dir_results,
        "SW2ncWeather",
        paste0(
          "weather__", prjTag, "__", meteo_sources[[kw]],
          ".nc"
        )
      )

      if (!file.exists(fname_weather)) {
        dir.create(
          dirname(fname_weather), recursive = TRUE, showWarnings = FALSE
        )

        tasks_by_dbW <- setup_dbWeather(
          fdbWeather = fnames_dbMeteo[[kw]],
          wfs = tmpMeteo_wfs[[kw]],
          weather_source = meteo_sources[[kw]],
          uniqueWeather = FALSE
        )

        idsUniqueMeteo <- which(!duplicated(tasks_by_dbW[["ID_by_dbW"]]))
        tasks_by_dbW <- tasks_by_dbW[idsUniqueMeteo, , drop = FALSE]
        Nweather <- nrow(tasks_by_dbW)

        xsimMeteo <- xsim[idsUniqueMeteo, 0, drop = FALSE]

        basedOnDomain <- Nweather == Nsim

        varsMeteo <- switch(
          EXPR = meteo_sources[[kw]],
          gridMET = c(
            "Tmax_C", "Tmin_C", "PPT_cm", "windSpeed_mPERs", "rHmax_pct",
            "rHmin_pct", "shortWR"
          ),
          stop("Not implemented: ", meteo_sources[[kw]])
        )


        # gridMET metadata
        ncMetaMeteo <- switch(
          EXPR = meteo_sources[[kw]],
          gridMET = list(
            Tmax_C = list(
              varName = "tasmax",
              long_name = "maximum air temperature",
              units = "degree_C",
              cell_method = "time: maximum",
              attributes = list(units_metadata = "temperature: on_scale")
            ),
            Tmin_C = list(
              varName = "tasmin",
              long_name = "minimum air temperature",
              units = "degree_C",
              cell_method = "time: minimum",
              attributes = list(units_metadata = "temperature: on_scale")
            ),
            PPT_cm = list(
              varName = "pr",
              long_name = "precipitation amount",
              units = "cm",
              cell_method = "time: sum"
            ),
            windSpeed_mPERs = list(
              varName = "vs",
              long_name = "wind speed",
              units = "m s-1",
              cell_method = "time: mean"
            ),
            rHmax_pct = list(
              varName = "hursmax",
              long_name = "maximum relative humidity",
              units = "%",
              cell_method = "time: maximum"
            ),
            rHmin_pct = list(
              varName = "hursmin",
              long_name = "minimum relative humidity",
              units = "%",
              cell_method = "time: minimum"
            ),
            shortWR = list(
              varName = "rsds",
              long_name = "incoming shortwave radiation",
              units = "W m-2",
              cell_method = "time: mean",
              typeRSDS = 1L # gridMET rsds is flux density over 24 hours
            )
          ),
          stop("Not implemented: ", meteo_sources[[kw]])
        )


        #--- ....** Set up nc for weather ------
        if (isTRUE(basedOnDomain)) {
          file.copy(from = fname_domain, to = fname_weather)

        } else {
          rSW2st::create_netCDF(
            filename = fname_weather,
            xyspace = xsimMeteo[, 0L],
            data = array(seq_len(Nweather), dim = c(Nweather, 1L)),
            data_str = "s",
            data_type = "integer",
            var_attributes = list(
              name = "domainWeather",
              long_name = "simulation domain for weather",
              units = "1",
              grid_mapping = "crs",
              coordinates = "latitude longitude site"
            ),
            xy_attributes = list(
              name = c("longitude", "latitude"),
              standard_name = c("longitude", "latitude"),
              long_name = c("longitude", "latitude"),
              units = c("degrees_east", "degrees_north"),
              axis = c("X", "Y")
            ),
            crs_attributes = list(
              long_name = "WGS84",
              #crs_wkt = sf::st_crs("WGS84")$Wkt,
              crs_wkt = "GEOGCS[\"WGS 84\",DATUM[\"WGS_1984\",SPHEROID[\"WGS 84\",6378137,298.257223563,AUTHORITY[\"EPSG\",\"7030\"]],AUTHORITY[\"EPSG\",\"6326\"]],PRIMEM[\"Greenwich\",0,AUTHORITY[\"EPSG\",\"8901\"]],UNIT[\"degree\",0.0174532925199433,AUTHORITY[\"EPSG\",\"9122\"]],AUTHORITY[\"EPSG\",\"4326\"]]",
              grid_mapping_name = "latitude_longitude",
              longitude_of_prime_meridian = 0.0,
              semi_major_axis = 6378137.0,
              inverse_flattening = 298.257223563
            ),
            global_attributes = attributesProject(),
            nc_compression = TRUE
          )
        }

        xnc <- RNetCDF::open.nc(fname_weather, write = TRUE)

        rSW2st::setGlobalAttributesNCSW(xnc, attributesProject())

        rSW2st::setGlobalAttributesNCSW(
          xnc,
          attributes = c(frequency = "day", featureType = "timeSeries")
        )

        #--- ......*** inWeather: time ------
        rSW2st::setAxisTimeNCSW(
          xnc,
          startYear = simYears[[1L]],
          timeValues = seq_len(ntime) - 0.5, # midday
          calendar = "standard"
        )


        #--- ......*** inWeather: variables ------
        for (kv in seq_along(varsMeteo)) {
          varm <- varsMeteo[[kv]]

          stopifnot(!is.null(ncMetaMeteo[[varm]]))

          rSW2st::setVariableNCSW(
            xnc,
            varName = ncMetaMeteo[[varm]][["varName"]],
            long_name = ncMetaMeteo[[varm]][["long_name"]],
            dimensions = c(xDim[["time"]], xDim[["sp"]]),
            units = ncMetaMeteo[[varm]][["units"]],
            coordinates = varAttrSp[["coordinates"]],
            grid_mapping = varAttrSp[["grid_mapping"]],
            cell_method = ncMetaMeteo[[varm]][["cell_method"]],
            attributes = ncMetaMeteo[[varm]][["attributes"]],
            dataType = dataType,
            values = NULL
          )
        }


        #--- ....** Write weather data for each site ------
        startTXY <- c(time = 1L, sp = 1L)
        countTXY <- c(time = ntime, sp = 1L)


        #--- Loop over sites
        stopifnot(rSOILWAT2::dbW_setConnection(fnames_dbMeteo[[kw]]))

        pb <- utils::txtProgressBar(max = Nweather, style = 3L)

        for (ks in seq_len(Nweather)) {
          wdata <- rSOILWAT2::dbW_getWeatherData(
            Label = tasks_by_dbW[ks, "Label"]
          ) |>
            rSOILWAT2::upgrade_weatherHistory() |>
            rSOILWAT2::dbW_weatherData_to_dataframe()
          dif <- rSOILWAT2::calc_dailyInputFlags(wdata)
          stopifnot(varsMeteo %in% names(dif)[dif])

          #--- ......*** inWeather: variables ------
          for (kv in seq_along(varsMeteo)) {
            varm <- varsMeteo[[kv]]
            rSW2st::setVariableNCSW(
              xnc,
              varName = ncMetaMeteo[[varm]][["varName"]],
              values = wdata[, varm, drop = FALSE],
              start = startTXY,
              count = countTXY
            )
          }

          startTXY[["sp"]] <- startTXY[["sp"]] + 1L
          utils::setTxtProgressBar(pb, value = ks)
        }

        close(pb)


        #--- ....** Clean up ------
        RNetCDF::close.nc(xnc)
        rSOILWAT2::dbW_disconnectConnection()
      }
    }
  }


  #--- ..* ncSite ------
  if (isTRUE(site_actions[["doTasClimForLowerSoilTemperatureBoundary"]])) {

    for (kw in seq_along(weather_sources)) {

      fname_tasclim <- file.path(
        dir_results,
        "SW2ncSite",
        paste0("tas-clim__", prjTag, "__", weather_sources[[kw]], ".nc")
      )

      if (!file.exists(fname_tasclim)) {
        dir.create(
          dirname(fname_tasclim), recursive = TRUE, showWarnings = FALSE
        )

        #--- Prepare data
        ncVars <- c(
          tas = paste("tasclim", weather_sources[[kw]], sep = "-")
        )

        #--- Write to disk
        file.copy(from = fname_domain, to = fname_tasclim)
        xnc <- RNetCDF::open.nc(fname_tasclim, write = TRUE)

        for (kv in seq_along(ncVars)) {
          tmp_vals <- xsiteconds[, ncVars[[kv]], drop = TRUE]
          if (all(is.na(tmp_vals))) next

          tmp_var <- names(ncVars)[[kv]]
          tmp_units <- switch(
            EXPR = tmp_var,
            tas = "degC"
          )
          tmp_attr <- switch(
            EXPR = tmp_var,
            tas = list(
              long_name = "mean air temperature",
              cell_method = "time: mean",
              attributes = list(units_metadata = "temperature: on_scale")
            )
          )

          rSW2st::setVariableNCSW(
            xnc,
            varName = tmp_var,
            dimensions = "site",
            units = tmp_units,
            coordinates = "latitude longitude site",
            grid_mapping = "crs",
            dataType = "double",
            values = tmp_vals,
            count = Nsim,
            attributes = tmp_attr
          )
        }

        RNetCDF::close.nc(xnc)
      }
    }
  }
}

#------ . ------
#------ . ------
