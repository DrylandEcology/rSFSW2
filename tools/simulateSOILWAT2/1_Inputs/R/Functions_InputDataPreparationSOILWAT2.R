

#------ . ------
#--- Misc functions ------
matchMultipleVariables <- function(xLeft, xRight, variables) {
  match(
    apply(
      as.data.frame(xLeft)[, variables, drop = FALSE],
      MARGIN = 1L,
      FUN = paste,
      collapse = "-"
    ),
    table = apply(
      as.data.frame(xRight)[, variables, drop = FALSE],
      MARGIN = 1L,
      FUN = paste,
      collapse = "-"
    ),
    nomatch = 0L
  )
}


#' Translate tildas and umlaute
#'
#' @examples
#' translateNonAsci("Doña Ana")
#'
translateNonAsci <- function(x) {
  tl <- rbind(
    c(from = "ä", to = "a"),
    c(from = "Ä", to = "A"),
    c(from = "á", to = "a"),
    c(from = "Á", to = "A"),
    c(from = "à", to = "a"),
    c(from = "À", to = "A"),
    c(from = "â", to = "a"),
    c(from = "Â", to = "A"),

    c(from = "ë", to = "e"),
    c(from = "Ë", to = "E"),
    c(from = "é", to = "e"),
    c(from = "É", to = "E"),
    c(from = "è", to = "e"),
    c(from = "È", to = "E"),
    c(from = "ê", to = "e"),
    c(from = "Ê", to = "E"),

    c(from = "ï", to = "i"),
    c(from = "Ï", to = "I"),
    c(from = "î", to = "i"),
    c(from = "Î", to = "I"),

    c(from = "ø", to = "o"),
    c(from = "Ø", to = "O"),
    c(from = "ö", to = "o"),
    c(from = "Ö", to = "O"),
    c(from = "ó", to = "o"),
    c(from = "Ó", to = "O"),
    c(from = "ò", to = "o"),
    c(from = "Ò", to = "O"),
    c(from = "ô", to = "o"),
    c(from = "Ô", to = "O"),

    c(from = "ü", to = "u"),
    c(from = "Ü", to = "U"),
    c(from = "ú", to = "u"),
    c(from = "Ú", to = "U"),
    c(from = "ù", to = "u"),
    c(from = "Ù", to = "U"),
    c(from = "û", to = "u"),
    c(from = "Û", to = "U"),

    c(from = "ç", to = "c"),
    c(from = "Ç", to = "C"),

    c(from = "ñ", to = "n"),
    c(from = "Ñ", to = "N")
  )

  for (k in seq_len(nrow(tl))) {
    x <- gsub(pattern = tl[k, "from"], replacement = tl[k, "to"], x)
  }

  iconv(x, to = "ASCII", sub = "") # Remove all other non-ASCII character
}


#------ . ------
#--- Input functions ------
readInputFile <- function(filename) {
  switch(
    EXPR = tools::file_ext(filename),
    xls = ,
    xlsx = readxl::read_excel(filename),
    csv = utils::read.csv(filename),
    stop("Not implemented.")
  )
}

create_xmain <- function(x, varsCoords, varsKeep, prjCRS = 4326) {
  hasCoords <- all(varsCoords %in% colnames(x))

  sf::st_as_sf(
    x = data.frame(
      Label = NA_character_,
      site_id = NA_integer_,
      Include_YN = 1L,
      WeatherFolder = NA_integer_,
      X_WGS84 = x[, varsCoords[[1L]], drop = TRUE],
      Y_WGS84 = x[, varsCoords[[2L]], drop = TRUE],
      ELEV_m = NA_real_,
      Slope = NA_real_,
      Aspect = NA_real_,
      if (hasCoords) {
        x[, setdiff(varsKeep, varsCoords), drop = FALSE]
      } else {
        x[, varsKeep, drop = FALSE]
      },
      exclusionReason = NA_character_
    ),
    coords = c("X_WGS84", "Y_WGS84"),
    remove = !hasCoords,
    crs = prjCRS
  )
}

#' Simulation setup with "design treatments"
#'
#' Note: "experimental treatments" are defined via
#' rSFSW2's "SWRuns_InputData_ExperimentalDesign_v12.csv"
create_xsim <- function(xmain, varsSimLabel, expVegTrt = NULL) {
  N_trt <- max(1L, nrow(expVegTrt[["values"]]))
  Nsim <- nrow(xmain) * N_trt

  # xsim: based on repeated copies of xmain, one copy for each "design treatment"
  xsim <- if (N_trt > 1L) {
    do.call(
      rbind,
      args = lapply(
        seq_len(N_trt),
        function(k) {
          cbind(
            xmain,
            ExpTrt_tag = expVegTrt[["tag"]][[k]],
            expVegTrt[["values"]][k, , drop = FALSE]
          )
        }
      )
    )
  } else {
    xmain
  }

  xsim[, "site_id"] <- seq_len(Nsim)


  #--- Labels
  tmp <- apply(
    as.matrix(xsim[, varsSimLabel, drop = TRUE]),
    MARGIN = 1L,
    paste0,
    collapse = "-"
  ) |>
    translateNonAsci() |>
    # Remove (non-nested) parentheses and their content
    gsub("\\([^()]*\\)", "", x = _) |>
    # Remove spaces
    gsub(" ", "", x = _)


  xsim[, "Label"] <- paste0(
    "SW2-",
    formatC(xsim[["site_id"]], width = ceiling(log10(Nsim + 1L)), flag = 0L),
    if (any(nchar(tmp) > 0L)) paste0("__", tmp)
  )

  xsim
}

#------ . ------
#--- Input validation functions ------
plot_map <- function(
  fname,
  x,
  var_index,
  crs = 5070,
  size = 0.1,
  ids_highlight = NULL,
  var_plot = NULL,
  colorScaleName = NULL
) {
  plotType <- 1L
  used_vars <- var_index

  if (!is.null(var_plot)) {
    plotType <- 2L
    used_vars <- c(used_vars, var_plot)
  }

  if (!is.null(ids_highlight)) {
    plotType <- 3L + as.integer(!is.null(var_plot))
    tmp <- rep(0L, nrow(x))
    tmp[ids_highlight] <- 1L
    x[, "group"] <- factor(tmp, levels = c(0, 1L))
    used_vars <- c(used_vars, "group")
  }

  xmain <- unique(x[, used_vars])

  if (!is.null(ids_highlight)) {
    xmain <- xmain[order(xmain[, "group", drop = TRUE]), , drop = FALSE]
  }


  xbbox <- sf::st_bbox(sf::st_transform(xmain, crs = crs))

  tmpg <- ggplot2::ggplot(data = xmain) +
    ggplot2::geom_sf(
      data = sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
    )

  tmpg <- if (plotType == 1L) {
    tmpg +
      ggplot2::geom_sf(size = size, color = "#56B4E9")

  } else if (plotType %in% c(2L, 4L)) {
    tmpg +
      ggplot2::geom_sf(
        ggplot2::aes(color = !!rlang::sym(var_plot)),
        size = size,
        show.legend = TRUE
      )

  } else if (plotType == 3L) {
    tmpg +
      ggplot2::geom_sf(ggplot2::aes(color = group), size = size) +
      ggplot2::scale_color_manual(values = c("#56B4E9", "#CC79A7"))
  }

  if (plotType == 4L) {
    tmpg <- tmpg +
      ggplot2::geom_sf(
        data = xmain[xmain[["group"]] == 1L, ],
        color = "#CC79A7",
        size = 2 * size
      )
  }

  st <- if (plotType %in% c(2L, 4L)) {
    ggplot2::scale_type(xmain[, var_plot, drop = TRUE])
  } else if (plotType == 3L) {
    "discrete"
  }

  if (identical(colorScaleName, "viridis")) {
    tmpg <- tmpg +
      switch(
        EXPR = st,
        discrete = ggplot2::scale_color_viridis_d(),
        continuous = ggplot2::scale_color_viridis_c()
      )
  }

  if (identical(st, "discrete")) {
    tmpg <- tmpg +
      ggplot2::guides(
        color = ggplot2::guide_legend(override.aes = list(size = max(3, size)))
      )
  }

  tmpg <- tmpg +
    ggplot2::coord_sf(
      xlim = xbbox[c("xmin", "xmax")],
      ylim = xbbox[c("ymin", "ymax")],
      crs = crs
    ) +
    ggplot2::theme_bw()


  # See `ggplot2::coord_sf()$aspect` to figure out the aspect ratio:
  # sf::st_is_longlat(sf::st_crs(crs_map)) is FALSE => ratio = 1
  asp <- (xbbox[["ymax"]] - xbbox[["ymin"]]) / (xbbox[["xmax"]] - xbbox[["xmin"]])
  asp <- min(c(max(c(0.5, asp)), 2))

  grDevices::png(
    filename = fname,
    width = 7,
    height = 7 * asp,
    units = "in",
    res = 300
  )
  plot(tmpg)
  grDevices::dev.off()
}

#------ . ------
#--- Weather functions ------
listWeatherFolderNames <- function(
    xsim, weather_sources, fname_gridmet_nc = NULL
) {
  lapply(
    weather_sources,
    function(ws) {
      switch(
        EXPR = ws,
        DayMet = rSFSW2:::get_DayMet_cellID(xsim),
        gridMET = rSFSW2:::get_gridMET_cellID(
          xsim,
          fname_gridMET = fname_gridmet_nc
        ),
        obs = list(
          cellID = paste0("weather_", xsim[["Label"]]),
          dm_WGS84 = xsim[, c("X_WGS84", "Y_WGS84"), drop = TRUE]
        ),
        stop("weather_source = ", ws, " is not supported.")
      )
    }
  )
}

setup_dbWeather <- function(fdbWeather, wfs, weather_source) {
  tmp <- data.frame(
    site_id = NA,
    X_WGS84 = wfs[["dm_WGS84"]][, 1L, drop = TRUE],
    Y_WGS84 = wfs[["dm_WGS84"]][, 2L, drop = TRUE],
    WeatherFolder = wfs[["cellID"]],
    dailyweather_source = switch(
      EXPR = weather_source,
      DayMet = "DayMet_NorthAmerica",
      gridMET = "gridMET_NorthAmerica",
      obs = "obs",
      stop("weather_source = ", ws, " is not supported.")
    ),
    stringsAsFactors = FALSE
  )

  SWRunInformation <- unique(tmp)
  SWRunInformation[["site_id"]] <- seq_len(nrow(SWRunInformation))

  # Create or connect to `dbWeather`
  res <- rSFSW2::make_dbW(
    fdbWeather = fdbWeather,
    SWRunInformation = SWRunInformation,
    runIDs_sites = SWRunInformation[["site_id"]],
    ambient_scenario = "Current"
  )

  stopifnot(rSOILWAT2::dbW_setConnection(fdbWeather))
  on.exit(rSOILWAT2::dbW_disconnectConnection())

  # Check if we already have some data in the weather database
  res[["add_to_dbW"]] <- !rSOILWAT2::dbW_has_weatherData(
    Site_ids = res[["ID_by_dbW"]],
    Scenario_ids = 1L
  )[, 1L]

  res
}


#--- ..* GridMET ------
download_gridMET <- function(
  dir_gridmet,
  dir_gridmet_priority = NULL,
  dir_script = "."
) {
  dir.create(dir_gridmet, recursive = TRUE, showWarnings = FALSE)

  if (isTRUE(try(dir.exists(dir_gridmet_priority), silent = TRUE))) {
    dir_gridmet <- dir_gridmet_priority
  }

  has_sh <- length(
    list.files(dir_script, pattern = "^wget_[[:print:]]+_metdata.sh$")
  ) > 0L

  has_nc <- length(
    list.files(dir_gridmet, pattern = "pr_[[:digit:]]{4}.nc", recursive = TRUE)
  ) > 0L

  if (!all(has_sh, has_nc)) {

    fname_wget_gridmet <- rSFSW2::gridMET_download_and_check(
      dir_data = dir_gridmet,
      dir_script = dir_script
    )

    if (isTRUE(!is.na(fname_wget_gridmet))) {
      message(
        "Execute script ", fname_wget_gridmet, " to download gridMET",
        " (script file may need execute permissions)"
      )
    }
  }

  list.files(
    dir_gridmet,
    pattern = "pr_[[:digit:]]{4}.nc",
    full.names = TRUE,
    recursive = TRUE
  )[[1L]]
}


#--- ..* DayMet ------
get_DayMet <- function(
  task, startYear, endYear, fname_dbW, correctWeatherValues = FALSE, pb = NULL
) {
  if (!is.null(pb)) pb() # progressr object

  stopifnot(rSOILWAT2::dbW_setConnection(fname_dbW))
  on.exit(rSOILWAT2::dbW_disconnectConnection())

  mm_dm <- rSOILWAT2::sw_meteo_obtain_DayMet(
    x = c(
      longitude = task[["Longitude"]],
      latitude = task[["Latitude"]]
    ),
    start_year = startYear,
    end_year = endYear
  )

  # Fill in missing values arising from DayMet's 365-day calendar
  wd <- rSOILWAT2::dbW_fixWeather(
    mm_dm[["weatherDF"]],
    correctWeatherValues = isTRUE(correctWeatherValues),
    return_weatherDF = FALSE
  )[["weatherData"]]

  # Check that weather data is well-formed
  stopifnot(rSOILWAT2::dbW_check_weatherData(wd, check_all = TRUE))

  rSOILWAT2::dbW_addWeatherData(
    weatherData = wd,
    Label = task[["Label"]],
    Site_id = task[["ID_by_dbW"]],
    Scenario_id = 1L,
    Scenario = "Current"
  )
}


pfun_get_DayMet <- function(
  tasks,
  simYears,
  fname_dbW,
  correctWeatherValues = FALSE
) {
  Nall <- nrow(tasks)
  Nsteps <- 100L
  p_nthstep <- max(1L, floor(Nall / Nsteps))
  p <- progressr::progressor(
    steps = max(1L, ceiling(Nall / p_nthstep))
  )

  ids_todo <- which(tasks[["add_to_dbW"]])
  Ntodo <- length(ids_todo)

  p(amount = floor((Nall - Ntodo) / p_nthstep))

  #--- Parallel loop
  kt <- NULL
  res <- foreach(
    kt = seq_len(Ntodo),
    .errorhandling = "pass",
    .inorder = FALSE
  ) %dopar% {
    get_DayMet(
      task = tasks[ids_todo[[kt]], , drop = TRUE],
      startYear = simYears[[1L]],
      endYear = simYears[[length(simYears)]],
      fname_dbW = fname_dbW,
      correctWeatherValues = correctWeatherValues,
      # only update progress every n-th step (or at the very end)
      pb = if (kt %% p_nthstep == 0 || kt == Ntodo) p
    )
  }
}


#--- ..* MACA ------
metaMACAv2METDATA <- function() {
  meta <- list(
    cmip = "CMIP5",
    organizedYears = list(
      fmt = "%4d_%4d",
      stride = 5L
    ),
    experiments = rbind(
      c(name = "historical", start = 1950, end = 2005),
      c(name = "RCP45", start = 2006, end = 2099),
      c(name = "RCP85", start = 2006, end = 2099)
    ),
    models = rbind(
      c(name = "bcc-csm1-1", realization = "r1i1p1"),
      c(name = "bcc-csm1-1-m", realization = "r1i1p1"),
      c(name = "BNU-ESM", realization = "r1i1p1"),
      c(name = "CanESM2", realization = "r1i1p1"),
      c(name = "CCSM4", realization = "r6i1p1"),
      c(name = "CNRM-CM5", realization = "r1i1p1"),
      c(name = "CSIRO-Mk3-6-0", realization = "r1i1p1"),
      c(name = "GFDL-ESM2M", realization = "r1i1p1"),
      c(name = "GFDL-ESM2G", realization = "r1i1p1"),
      c(name = "HadGEM2-ES365", realization = "r1i1p1"),
      c(name = "HadGEM2-CC365", realization = "r1i1p1"),
      c(name = "inmcm4", realization = "r1i1p1"),
      c(name = "IPSL-CM5A-LR", realization = "r1i1p1"),
      c(name = "IPSL-CM5A-MR", realization = "r1i1p1"),
      c(name = "IPSL-CM5B-LR", realization = "r1i1p1"),
      c(name = "MIROC5", realization = "r1i1p1"),
      c(name = "MIROC-ESM", realization = "r1i1p1"),
      c(name = "MIROC-ESM-CHEM", realization = "r1i1p1"),
      c(name = "MRI-CGCM3", realization = "r1i1p1"),
      c(name = "NorESM1-M", realization = "r1i1p1")
    ) |> data.frame(),
    vars = list(
      Tmax_C = list(var = "tasmax", uc = function(x) x - 273.15), # K -> C
      Tmin_C = list(var = "tasmin", uc = function(x) x - 273.15), # K -> C
      PPT_cm = list(var = "pr", uc = function(x) 0.1 * x), # mm -> cm
      cloudCov_pct = list(var = "NA", uc = function(x) x),
      windSpeed_mPERs = list(var = "NA", uc = function(x) x),
      windSpeed_east_mPERs = list(var = "uas", uc = function(x) x), # m s-1
      windSpeed_north_mPERs = list(var = "vas", uc = function(x) x), # m s-1
      rHavg_pct = list(var = "NA", uc = function(x) x),
      rHmax_pct = list(var = "rhsmax", uc = function(x) x), # %
      rHmin_pct = list(var = "rhsmin", uc = function(x) x), # %
      specHavg_gPERkg = list(var = "huss", uc = function(x) 1000 * x), # kg kg-1 -> g kg-1
      Tdewpoint_C = list(var = "NA", uc = function(x) x),
      actVP_kPa = list(var = "NA", uc = function(x) x),
      shortWR = list(var = "rsds", uc = function(x) x) # W m-2
    ),
    missingValue = -9999 # bcc-csm1-1, RCP8.5, 2099-Dec-31
  )

  # Models that exclude individual variables
  rownames(meta[["models"]]) <- meta[["models"]][["name"]]
  meta[["models"]][["varsNA"]] <- list("specHavg_gPERkg")
  meta[["models"]]["NorESM1-M", "varsNA"][[1L]] <- list(
    c("rHmax_pct", "rHmin_pct")
  )
  meta[["models"]]["CCSM4", "varsNA"][[1L]] <- list(
    c("rHmax_pct", "rHmin_pct")
  )

  meta
}

cleanup_download_MACAv2METDATA_csv <- function(res, ftmp, fdest) {
  if (
    !inherits(res, "try-error") && identical(res, 0L) && file.size(ftmp) > 1
  ) {
    if (file.exists(fdest)) {
      stop("File already exists: ", fdest, " -- tmp = ", ftmp)
    } else {
      file.rename(from = ftmp, to = fdest)
      unlink(ftmp)
    }
  } else {
    stop(
      "Download MACAv2METDATA data failed: ", res,
      " -- tmp = ", ftmp, " dest = ", fdest
    )
  }
}

sw_download_MACAv2METDATA_csv <- function(
  longitude, latitude,
  variable, yearStep, strided,
  experiment, model, realization,
  dirSingleSite,
  verbose
) {
  # http://thredds.northwestknowledge.net:8080/thredds/catalog/MACAV2
  # http://thredds.northwestknowledge.net:8080/thredds/ncss/MACAV2/BNU-ESM/
  #   macav2metdata_vpd_BNU-ESM_r1i1p1_rcp85_2096_2099_CONUS_daily.nc?
  #   var=all&latitude=45.0&longitude=-105.6&&temporal=all&accept=csv_file

  # http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_CMIP5_aggregated_macav2_catalog.html
  # http://thredds.northwestknowledge.net:8080/thredds/ncss/
  #   agg_macav2metdata_vas_CanESM2_r1i1p1_rcp85_2006_2099_CONUS_daily.nc?
  #   var=northward_wind&latitude=45.02&longitude=-105.6&temporal=all&accept=csv
  # http://thredds.northwestknowledge.net:8080/thredds/ncss/
  #   agg_macav2metdata_huss_NorESM1-M_r1i1p1_historical_1950_2005_CONUS_daily.nc?
  #   var=specific_humidity&latitude=45&longitude=-105&
  #   time_start=1950-01-01T00%3A00%3A00Z&time_end=1950-12-31T00%3A00%3A00Z&
  #   accept=csv
  fnc <- paste(
    if (isTRUE(strided)) "macav2metdata" else "agg_macav2metdata",
    tolower(variable), model, tolower(realization),
    tolower(experiment),
    yearStep,
    "CONUS_daily.nc",
    sep = "_"
  )

  fcsv <- file.path(dirSingleSite, sub(".nc$", ".csv", fnc))

  if (!file.exists(fcsv) || file.size(fcsv) < 1) {
    res <- 1L
    fcsv_tmp <- tempfile(fileext = ".csv")

    on.exit(cleanup_download_MACAv2METDATA_csv(res, fcsv_tmp, fcsv))

    url <- "http://thredds.northwestknowledge.net:8080/thredds/ncss"
    if (isTRUE(strided)) url <- paste0(url, "/MACAV2")

    subsetQuery <- paste0(
      "&var=all",
      "&longitude=", longitude,
      "&latitude=", latitude,
      "&temporal=all&accept=csv"
    )

    urlRequest <- paste0(
      url, "/",
      if (isTRUE(strided)) paste0(model, "/"),
      fnc, "?", subsetQuery
    )

    if (verbose) {
      t0 <- Sys.time()
      cat(format(t0), fill = TRUE)
    }

    Sys.sleep(stats::runif(n = 1, max = 5))
    res <- try(
      utils::download.file(urlRequest, destfile = fcsv_tmp, quiet = !verbose),
      silent = FALSE
    )

    cleanup_download_MACAv2METDATA_csv(res, ftmp = fcsv_tmp, fdest = fcsv)

    if (verbose) {
      cat(
        "downloaded in",
        format(round(difftime(Sys.time(), t0), 2L)),
        fill = TRUE
      )
    }

    on.exit()
  }

  if (file.exists(fcsv)) utils::read.csv(fcsv)
}


sw_meteo_obtain_MACAv2METDATA_SingleSite <- function(
  x,
  experiment, model, realization,
  dirSingleSite,
  strided = FALSE,
  metaVars = list(var1 = list(var = "NA", uc = function(x) x)),
  metaMissingValue = -9999,
  metaExperiments = data.frame(name = NA, start = NA, end = NA),
  metaStrideYearsN = NA,
  verbose = FALSE
) {
  if (!dir.exists(dirSingleSite)) {
    dir.create(dirSingleSite, recursive = TRUE, showWarnings = FALSE)
  }

  # Calendar years and days
  id <- which(metaExperiments[, "name"] == experiment)
  startYear <- as.integer(metaExperiments[id, "start"])
  endYear <- as.integer(metaExperiments[id, "end"])

  dates <- seq.Date(
    from = as.Date(paste0(startYear, "-01-01")),
    to = as.Date(paste0(endYear, "-12-31")),
    by = "1 day"
  )

  # Organize years by stride across multiple files
  if (isTRUE(strided)) {
    tmp <- seq(from = startYear, to = endYear, by = metaStrideYearsN)
    yearSteps <- data.frame(
      start = tmp,
      end = c(tmp[-1L] - 1L, max(tmp[length(tmp)], endYear))
    )
  } else {
    yearSteps <- data.frame(start = startYear, end = endYear)
  }

  yearSteps[["tag"]] <- apply(yearSteps, MARGIN = 1L, paste, collapse = "_")
  yearStepsN <- nrow(yearSteps)

  # Identify variables
  varsMACA <- vapply(
    metaVars,
    function(x) if (identical(x[["var"]], "NA")) NA_character_ else x[["var"]],
    FUN.VALUE = NA_character_
  )

  isNA_varsMACA <- is.na(varsMACA)
  varsSW <- names(varsMACA)

  tol <- sqrt(.Machine[["double.eps"]])

  # Prepare result container
  tmpm <- array(
    dim = c(length(dates), 2L + sum(!isNA_varsMACA)),
    dimnames = list(NULL, c("Year", "DOY", varsSW[!isNA_varsMACA]))
  )

  tmp <- as.POSIXlt(dates)
  tmpm[, "Year"] <- 1900L + tmp$year
  tmpm[, "DOY"] <- 1L + tmp$yday


  # Loop over variables and year strides
  for (ky in seq_len(yearStepsN)) {
    for (kv in seq_along(varsMACA)) {
      if (isNA_varsMACA[[kv]]) next

      # Download file and read values
      res <- sw_download_MACAv2METDATA_csv(
        longitude = x[["longitude"]],
        latitude = x[["latitude"]],
        variable = varsMACA[[kv]],
        yearStep = yearSteps[ky, "tag"],
        strided = isTRUE(strided),
        experiment = experiment,
        model = model,
        realization = realization,
        dirSingleSite = dirSingleSite,
        verbose = isTRUE(verbose)
      )

      stopifnot("time" %in% colnames(res))

      # Copy data into result container and convert units
      resDates <- as.Date(res[["time"]])
      stopifnot(
        resDates[[1L]] == paste0(yearSteps[ky, "start"], "-01-01"),
        resDates[[length(resDates)]] == paste0(yearSteps[ky, "end"], "-12-31")
      )

      ids_time <- match(dates, resDates, nomatch = 0L)


      # Prepare values
      x <- res[ids_time, ncol(res), drop = TRUE]

      # Check for missing values
      idsMissing <- which(abs(x - metaMissingValue) < tol)

      # Convert units
      x <- metaVars[[varsSW[[kv]]]][["uc"]](x)

      # Apply missing values
      if (length(idsMissing) > 0L) {
        x[idsMissing] <- NA
      }

      tmpm[ids_time > 0L, varsSW[[kv]]] <- x
    }
  }

  # Add missing variables and missing days to complete full calendar years
  sw_weather <- rSOILWAT2::dbW_convert_to_GregorianYears(
    weatherData = rSOILWAT2::upgrade_weatherDF(tmpm),
    new_startYear = startYear,
    new_endYear = endYear,
    type = "asis"
  )

  vars_meteo <- rSOILWAT2::weather_dataColumns()
  dif <- rSOILWAT2::calc_dailyInputFlags(sw_weather, name_data = vars_meteo)

  list(
    metadata = list(
      experiment = as.character(experiment),
      model = as.character(model),
      realization = as.character(realization)
    ),
    rawdata = NULL,
    weatherDF = sw_weather,
    vals_missing = rSOILWAT2::is_missing_weather(
      sw_weather[, vars_meteo, drop = FALSE]
    ),
    desc_rsds = NA_integer_,
    use_cloudCoverMonthly = FALSE, # use radiation instead
    use_windSpeedMonthly = FALSE, # has wind speed
    use_humidityMonthly = FALSE, # has relative humidity
    dailyInputFlags = dif
  )
}


get_MACAv2METDATA <- function(
  task, fname_dbW, meta, tmpdir,
  strided = FALSE,
  correctWeatherValues = FALSE,
  verbose = FALSE,
  timeout = if (isTRUE(strided)) 600L else 3600L,
  pb = NULL
) {
  if (!is.null(pb)) pb() # progressr object

  op_prev <- options(timeout = max(0, timeout, getOption("timeout")))
  on.exit(options(op_prev))

  vars_required <- c(
    "Longitude", "Latitude",
    "experiment", "model", "realization",
    "Label", "Label2", "Site_id", "Scenario_id", "Scenario"
  )

  stopifnot(vars_required %in% names(task))

  metaVarsModel <- meta[["vars"]]
  idm <- which(meta[["models"]][["name"]] %in% task[["model"]])
  modelVarsNA <- meta[["models"]][idm, "varsNA"][[1L]]
  idv <- which(names(metaVarsModel) %in% modelVarsNA)
  metaVarsModel[idv] <- list(list(var = "NA", uc = function(x) x))

  mm_dm <- sw_meteo_obtain_MACAv2METDATA_SingleSite(
    x = c(
      longitude = task[["Longitude"]],
      latitude = task[["Latitude"]]
    ),
    experiment = task[["experiment"]],
    model = task[["model"]],
    realization = task[["realization"]],
    metaVars = metaVarsModel,
    metaMissingValue = meta[["missingValue"]],
    metaExperiments = meta[["experiments"]],
    metaStrideYearsN = meta[["organizedYears"]][["stride"]],
    strided = isTRUE(strided),
    dirSingleSite = file.path(
      tmpdir,
      task[["Label2"]],
      paste(task[["model"]], task[["realization"]], sep = "_")
    ),
    verbose = isTRUE(verbose)
  )

  # Fill in missing values (bcc-csm1-1, RCP8.5, 2099-Dec-31)
  wd <- rSOILWAT2::dbW_fixWeather(
    mm_dm[["weatherDF"]],
    correctWeatherValues = isTRUE(correctWeatherValues),
    return_weatherDF = FALSE
  )[["weatherData"]]

  # Check that weather data is well-formed
  stopifnot(rSOILWAT2::dbW_check_weatherData(wd, check_all = TRUE))

  # Add data to weather database
  res <- FALSE

  if (rSOILWAT2::dbW_setConnection(fname_dbW)) {
    res <- try(
      rSOILWAT2::dbW_addWeatherData(
        weatherData = wd,
        Label = task[["Label"]],
        Site_id = task[["Site_id"]],
        Scenario_id = task[["Scenario_id"]],
        Scenario = task[["Scenario"]]
      )
    )
    rSOILWAT2::dbW_disconnectConnection()
  }

  !inherits(res, "try-error") && isTRUE(res)
}



pfun_get_MACAv2METDATA <- function(
  tasks,
  fname_dbW,
  meta = metaMACAv2METDATA(),
  strided = FALSE,
  tmpdir = tempdir(),
  correctWeatherValues = FALSE,
  verbose = FALSE,
  timeout = if (isTRUE(strided)) 600L else 3600L
) {
  Nall <- nrow(tasks)
  Nsteps <- 100L
  p_nthstep <- max(1L, floor(Nall / Nsteps))
  p <- progressr::progressor(
    steps = max(1L, floor(Nall / p_nthstep))
  )

  ids_todo <- which(tasks[["add_to_dbW"]])
  Ntodo <- length(ids_todo)

  p(amount = floor((Nall - Ntodo) / p_nthstep))


  #--- Parallel loop
  kt <- NULL

  # res <- foreach(
  #   kt = seq_len(Ntodo),
  #   .errorhandling = "stop", # "pass"
  #   .inorder = FALSE,
  #   verbose = isTRUE(verbose)
  # ) %dopar% {

  for (kt in seq_len(Ntodo)) {
    #print(kt)
    get_MACAv2METDATA(
      task = tasks[ids_todo[[kt]], , drop = TRUE],
      fname_dbW = fname_dbW,
      meta = meta,
      tmpdir = tmpdir,
      strided = isTRUE(strided),
      timeout = timeout,
      correctWeatherValues = correctWeatherValues,
      verbose = isTRUE(verbose),
      # only update progress every n-th step (or at the very end)
      pb = if (kt %% p_nthstep == 0 || kt == Ntodo) p
    )
  }
}


#--- ..* Observed weather ------
processObservedWeatherData <- function(
  filename,
  simYears,
  vals_missing = c(-999, -9999, NA),
  subWeatherData = NULL,
  mow = list(
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
  ),
  correctWeatherValues = FALSE
) {

  #--- * Read data
  wdh <- switch(
    EXPR = tools::file_ext(filename),
    csv = utils::read.csv(file = filename),
    xls = ,
    xlsx = readxl::read_excel(path = filename),
    stop(
      "File extention = ", tools::file_ext(filename),
      " is not supported."
    )
  ) |>
    data.matrix()

  #--- * Code missing values
  wdh[wdh %in% vals_missing] <- NA
  wdh <- as.data.frame(wdh)

  #--- * Format for SOILWAT2
  wdh[["DateCalc"]] <- paste(
    wdh[[mow[["date"]][["yr"]]]],
    wdh[[mow[["date"]][["mon"]]]],
    wdh[[mow[["date"]][["day"]]]],
    sep = "-"
  ) |>
    as.Date()
  ts_dates <- as.POSIXlt(wdh[["DateCalc"]])

  wdd <- data.frame(
    Date = wdh[["DateCalc"]],
    Year = 1900L + ts_dates$year,
    DOY = 1 + ts_dates$yday
  )

  vars_weather <- c(
    "Tmax_C", "Tmin_C", "PPT_cm", "cloudCov_pct", "windSpeed_mPERs",
    "windSpeed_east_mPERs", "windSpeed_north_mPERs", "rHavg_pct",
    "rHmax_pct", "rHmin_pct", "specHavg_pct", "Tdewpoint_C",
    "actVP_kPa", "shortWR"
  )

  for (vw in vars_weather) {
    res <- mow[[vw]][["uc"]](x = wdh[[mow[[vw]][["var"]]]])
    wdd[[vw]] <- if (is.null(res)) NA else res
  }


  #--- * Quality assessment
  #--- ....** TODO QA
  # e.g., Durre et al. 2011
  # for now
  is_qa_bad <- array(
    data = FALSE,
    dim = dim(wdd),
    dimnames = dimnames(wdd)
  )

  ids <- wdd[["Tmin_C"]] > wdd[["Tmax_C"]]
  is_qa_bad[ids, c("Tmax_C", "Tmin_C")] <- TRUE
  is_qa_bad[, "Tmin_C"][wdd[["Tmin_C"]] <= -100] <- TRUE
  is_qa_bad[, "PPT_cm"][wdd[["PPT_cm"]] < 0] <- TRUE
  is_qa_bad[, "windSpeed_mPERs"][wdd[["windSpeed_mPERs"]] < 0] <- TRUE
  ids <- wdd[["rHavg_pct"]] < 0 | wdd[["rHavg_pct"]] > 100
  is_qa_bad[ids, "rHavg_pct"] <- TRUE
  is_qa_bad[, "shortWR"][wdd[["shortWR"]] <= 0.1] <- TRUE

  wdd[is_qa_bad] <- NA

  #--- ....** Impute
  #   * gaps <= 2 days
  #       * linear interpolation
  #       * precipitation which is set to 0
  #   * substitute with other data
  #   * long-term daily means
  tmpw <- rSOILWAT2::dbW_fixWeather(
    weatherData = wdd,
    subData = subWeatherData,
    new_startYear = simYears[[1L]],
    new_endYear = simYears[[length(simYears)]],
    nmax_interp = 2L,
    precip_lt_nmax = 0,
    fillMissingValues = TRUE,
    correctWeatherValues = isTRUE(correctWeatherValues),
    return_weatherDF = TRUE
  )

  tmp <- rSOILWAT2::calc_dailyInputFlags(tmpw[["weatherData"]])
  vars_obs <- names(tmp)[tmp]

  stopifnot(
    !rSOILWAT2::is_missing_weather(tmpw[["weatherData"]][, vars_obs])
  )

  #--- * Return results
  list(
    weatherData = tmpw[["weatherData"]],
    hasData = length(vars_obs) > 0L,
    isImputed = data.frame(
      tmpw[["weatherData"]][, c("Year", "DOY")],
      tmpw[["meta"]],
      stringsAsFactors = FALSE
    )
  )
}


observedWeatherData <- function(
  fileNameInput,
  fileNameProcessed,
  longitude,
  latitude,
  simYears,
  metaObservedWeather,
  imputeMissingObsWeather = FALSE,
  correctWeatherValues = FALSE
) {

  #--- Obtain DayMet weather for substitution
  weather_dm <- if (isTRUE(imputeMissingObsWeather)) {
    rSOILWAT2::sw_meteo_obtain_DayMet(
      x = c(longitude = longitude, latitude = latitude),
      start_year = simYears[[1L]],
      end_year = simYears[[length(simYears)]]
    )
  }

  #--- Process observed weather
  #   * gaps <= 2 days
  #       * linear interpolation
  #       * precipitation which is set to 0
  #   * substitute with DayMet
  #   * long-term daily means
  owd <- processObservedWeatherData(
    filename = fileNameInput,
    simYears = simYears,
    subWeatherData = weather_dm[["weatherDF"]],
    mow = metaObservedWeather,
    correctWeatherValues = isTRUE(correctWeatherValues)
  )


  if (owd[["hasData"]]) {
    # Write to disk as csv file
    ftmp <- fileNameProcessed

    utils::write.csv(
      owd[["weatherData"]], file = ftmp, row.names = FALSE
    )

    if (isTRUE(imputeMissingObsWeather)) {
      ftmp2 <- file.path(
        dirname(ftmp),
        sub(
          pattern = ".csv$",
          replacement = "_isImputed.csv",
          x = basename(ftmp)
        )
      )

      utils::write.csv(
        owd[["isImputed"]], file = ftmp2, row.names = FALSE
      )
    }

    wdata <- rSOILWAT2::dbW_dataframe_to_weatherData(
      weatherDF = owd[["weatherData"]]
    )

  } else {
    wdata <- NULL
  }

  wdata
}



supplementDayMetWithgridMETWind <- function(
  wfs = list(DayMet = NULL, gridMET = NULL),
  fnames_dbW = list(DayMet = NULL, gridMET = NULL)
) {

  dbW_names <- c("DayMet", "gridMET")

  stopifnot(
    dbW_names %in% names(wfs),
    dbW_names %in% names(fnames_dbW)
  )

  #--- Create table to crosswalk weather database entries
  wd_cross <- lapply(
    dbW_names,
    function(wd) wfs[[wd]][["cellID"]]
  ) |>
    do.call(cbind, args = _) |>
    as.data.frame(stringsAsFactor = FALSE)

  colnames(wd_cross) <- paste0("cellID_", dbW_names)


  # Obtain dbW identifiers
  for (wd in dbW_names) {
    stopifnot(rSOILWAT2::dbW_setConnection(fnames_dbW[[wd]]))

    wd_cross[[paste0("ID_by_dbW__", wd)]] <- rSOILWAT2::dbW_getSiteId(
      Labels = wd_cross[[paste0("cellID_", wd)]]
    )

    rSOILWAT2::dbW_disconnectConnection()
  }


  #--- Update DayMet data with (averaged) wind data from gridMET grid cells
  wd_todos <- unique(wd_cross[, c("cellID_DayMet", "ID_by_dbW__DayMet")])
  N_todos <- nrow(wd_todos)

  conn_dm <- DBI::dbConnect(RSQLite::SQLite(), fnames_dbW[["DayMet"]])

  pb <- utils::txtProgressBar(max = N_todos, style = 3L)

  for (kw in seq_len(N_todos)) {
    # Obtain DayMet data
    stopifnot(rSOILWAT2::dbW_setConnection(fnames_dbW[["DayMet"]]))
    wd_dm <- rSOILWAT2::dbW_getWeatherData(
      Site_id = wd_todos[kw, "ID_by_dbW__DayMet"],
      Scenario_id = 1L
    ) |>
      rSOILWAT2::dbW_weatherData_to_dataframe()
    rSOILWAT2::dbW_disconnectConnection()

    if (all(is.na(wd_dm[, "windSpeed_mPERs"]))) {
      # Obtain associated gridMET wind data
      stopifnot(rSOILWAT2::dbW_setConnection(fnames_dbW[["gridMET"]]))
      kgs <- which(
        wd_cross[["ID_by_dbW__DayMet"]] == wd_todos[kw, "ID_by_dbW__DayMet"]
      )
      wd_gmk <- unique(wd_cross[kgs, , drop = FALSE])
      wds_gm <- vapply(
        seq_len(nrow(wd_gmk)),
        function(kg) {
          res <- rSOILWAT2::dbW_getWeatherData(
            Site_id = wd_gmk[kg, "ID_by_dbW__gridMET"],
            Scenario_id = 1L
          ) |>
            rSOILWAT2::dbW_weatherData_to_dataframe()
          res[, "windSpeed_mPERs", drop = TRUE]
        },
        FUN.VALUE = rep(NA_real_, times = nrow(wd_dm))
      )
      rSOILWAT2::dbW_disconnectConnection()


      # Update data in DayMet weather database
      # (average across associated gridMET grid cells, if multiple)
      wd_dm[, "windSpeed_mPERs"] <- rowMeans(wds_gm)

      res <- DBI::dbWithTransaction(
        conn = conn_dm,
        code = {
          res <- DBI::dbExecute(
            conn = conn_dm,
            statement = paste(
              "UPDATE WeatherData SET data = $1",
              "WHERE Site_id = $2 AND Scenario = $3"
            ),
            params = list(
              rSOILWAT2::dbW_dataframe_to_weatherData(wd_dm) |>
                rSOILWAT2::dbW_weatherData_to_blob(),
              wd_todos[kw, "ID_by_dbW__DayMet"],
              1L
            )
          )
          if (res != 1L) DBI::dbBreak()
        }
      )
      stopifnot(res == 1L)
    }

    utils::setTxtProgressBar(pb, value = kw)
  }

  DBI::dbDisconnect(conn_dm)

  close(pb)

  return(TRUE)
}


#------ . ------
#--- Topography functions ------
# Create a vrt instead of mosaic-ing NED tiles
# code based on FedData::get_ned() and FedData::get_ned_tile() v4.0.1
get_ned_tile2 <- function(
  template = NULL,
  res = "1",
  tileNorthing,
  tileWesting,
  path
) {
  message("(Down)Loading NED tile for ", tileNorthing, "N and ", tileWesting, "W.")

  tileWesting <- formatC(tileWesting, width = 3, format = "d", flag = "0")
  tileNorthing <- formatC(tileNorthing, width = 2, format = "d", flag = "0")

  fname <- paste0(
    "USGS_", res,
    "_n", tileNorthing,
    "w", tileWesting,
    ".tif"
  )

  filename <- file.path(path, fname)

  if (file.exists(filename)) return(filename)

  url <- paste0(
    "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/", res,
    "/TIFF/current/n", tileNorthing,
    "w", tileWesting,
    "/", fname
  )

  tmp <- terra::rast(url)

  terra::writeRaster(
    x = tmp,
    filename = filename,
    gdal = c("COMPRESS=DEFLATE", "ZLEVEL=9")
  )
}

get_vrt_ned <- function(
  template,
  label,
  res = "1",
  extraction.dir = file.path(
    tempdir(),
    "FedData",
    "extractions",
    "ned",
    label
  ),
  raster.options = c(
    "COMPRESS=DEFLATE",
    "ZLEVEL=9"
  ),
  force.redo = FALSE,
  method = c("bbox", "sparse")
) {
  method <- match.arg(method)

  extraction.dir <- normalizePath(extraction.dir, mustWork = FALSE)
  dir.create(extraction.dir, showWarnings = FALSE, recursive = TRUE)

  dir_tiles <- file.path(extraction.dir, "tiles")
  dir.create(dir_tiles, showWarnings = FALSE, recursive = TRUE)

  fname_vrt <- file.path(
    extraction.dir,
    paste0(label, "_NED_", res, ".vrt")
  )

  if (file.exists(fname_vrt) & !force.redo) {
    return(terra::rast(fname_vrt))
  }

  template <-
    FedData:::template_to_sf(template) |>
    sf::st_transform(4326)

  # Open USGS NED download service.
  # NED tiles are labeled by their northwest corner.
  # Thus, coordinate 36.42N, -105.71W is in grid n37w106

  if (identical(method, "bbox")) {
    extent.latlon <- sf::st_bbox(template)

    wests <- seq(ceiling(abs(extent.latlon["xmax"])), ceiling(abs(extent.latlon["xmin"])))
    norths <- seq(ceiling(abs(extent.latlon["ymin"])), ceiling(abs(extent.latlon["ymax"])))

    tilesLocations <- as.matrix(expand.grid(norths, wests, stringsAsFactors = FALSE))

  } else if (identical(method, "sparse")) {
    tmp <- sf::st_coordinates(template)
    tilesLocations <- unique(
      cbind(
        ceiling(abs(tmp[, 2L, drop = TRUE])),
        ceiling(abs(tmp[, 1L, drop = TRUE]))
      )
    )
  }

  message("Area of interest includes ", nrow(tilesLocations), " NED tiles.")

  # Download and crop tiles
  loc <- NULL
  tiles <-
    purrr::map(
      1:nrow(tilesLocations),
      function(loc) {
        return(
          tryCatch(
            get_ned_tile2(
              template = template,
              res = res,
              tileNorthing = tilesLocations[loc, 1],
              tileWesting = tilesLocations[loc, 2],
              path = dir_tiles
            ),
            error = function(e) {
              message("WARNING: ", e$message)
              return(NULL)
            },
            warning = function(w) NULL
          )
        )
      }
    )

  fname_tiles <- list.files(dir_tiles, pattern = ".tif$")

  if (length(fname_tiles) == 0L) {
    stop(
      "No NED tiles are available for your study area. ",
      "Please check your input data and internet connection."
    )
  }


  terra::vrt(file.path(dir_tiles, fname_tiles), filename = fname_vrt)
}


getTopoNED1 <- function(
  x,
  prjTag,
  path,
  doTopographicPosition = TRUE,
  use_FedData_for_topo = FALSE,
  methodTopoDownload = c("bbox", "sparse")
) {
  methodTopoDownload <- match.arg(methodTopoDownload)

  if (isTRUE(use_FedData_for_topo)) {
    # FedData::get_ned() as of v4.0.1 downloads NED tiles and then
    # uses terra::mosaic() to combine the tiles; this approach gets
    # extremely resource intensive for large spatial areas
    ned1 <- try(
      FedData::get_ned(
        template = x,
        label = prjTag,
        res = "1",
        extraction.dir = path
      )
    )

  } else {
    # Create a vrt instead of mosaic-ing NED tiles
    # code based on FedData::get_ned() and FedData::get_ned_tile() v4.0.1
    ned1 <- try(
      get_vrt_ned(
        template = x,
        label = prjTag,
        res = "1",
        extraction.dir = path,
        method = methodTopoDownload
      )
    )
  }

  fname_ned1_vrt <- file.path(path, paste0(prjTag, "_NED_1.vrt"))
  stopifnot(file.exists(fname_ned1_vrt))

  if (isTRUE(doTopographicPosition)) {
    tmp <- c(
      slope = paste0(prjTag, "_NED_1__slope.tif"),
      aspect = paste0(prjTag, "_NED_1__aspect.tif")
    )
    fname_tilt <- file.path(path, tmp)
    names(fname_tilt) <- names(tmp)

    # Derive slope and aspect
    for (pos in names(fname_tilt)) {
      if (!file.exists(fname_tilt[[pos]])) {
        tmp <- terra::terrain(
          x = ned1,
          v = pos,
          unit = "degrees",
          filename = fname_tilt[[pos]]
        )
      }
    }

  } else {
    fname_tilt <- NULL
  }


  #--- ..* Extract values from USGS NED-1 ------
  topo_ned <- rSW2exter::extract_topography_NEDUSA(
    x = x,
    path = dirname(fname_ned1_vrt),
    file_datasets = if (isTRUE(doTopographicPosition)) {
      list(
        elev = basename(fname_ned1_vrt),
        slope = basename(fname_tilt[["slope"]]),
        aspect = basename(fname_tilt[["aspect"]])
      )
    } else {
      list(elev = basename(fname_ned1_vrt))
    },
    units_slope = "degrees",
    units_aspect = "degrees",
    south_aspect = 180,
    flat_lt_slope = 0
  )

  if (!isTRUE(doTopographicPosition)) {
    topo_ned[, c("slope", "aspect")] <- NA
  }

  # Fix column names for output
  cns <- c(elev = "ELEV_m", slope = "Slope_deg", aspect = "Aspect_deg")
  colnames(topo_ned) <- cns[colnames(topo_ned)]

  ids_hasTopo <- apply(
    as.data.frame(topo_ned)[, cns, drop = FALSE],
    MARGIN = 1L,
    function(x) !all(is.na(x))
  )

  res <- x
  res[, cns] <- NA

  res[ids_hasTopo, cns] <-
    as.data.frame(topo_ned)[ids_hasTopo, cns, drop = FALSE]
  res[ids_hasTopo, "Source_Topo"] <- "NED"

  res
}

plot_compareTopoVariables <- function(
  fname,
  topo1,
  topo2,
  topo_sources,
  varsTopo
) {
  xtmp1 <- stack(topo1[, varsTopo])
  vtmp1 <- unique(as.character(xtmp1[complete.cases(xtmp1), "ind"]))

  xtmp2 <- stack(topo2[, varsTopo])
  vtmp2 <- unique(as.character(xtmp2[complete.cases(xtmp2), "ind"]))

  xtmp <- data.frame(xtmp1, xtmp2)
  colnames(xtmp) <- c("value1", "variable", "value2", "variable2")
  stopifnot(identical(xtmp[["variable"]], xtmp[["variable2"]]))

  ids <- xtmp[["variable"]] %in% intersect(vtmp1, vtmp2)
  xtmp <- xtmp[ids, , drop = FALSE]

  gtmp <- ggplot2::ggplot(
    data = xtmp,
    mapping = ggplot2::aes(x = value1, y = value2)
  ) +
    ggplot2::facet_wrap(ggplot2::vars(variable), scales = "free") +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(color = "blue") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
    ggplot2::xlab(topo_sources[[1L]]) +
    ggplot2::ylab(topo_sources[[2L]]) +
    ggplot2::coord_cartesian(xlim = c(0, NA), ylim = c(0, NA)) +
    ggplot2::theme_bw()

  # corresponding to `grDevices::n2mfrow()`
  nm <- ggplot2::wrap_dims(length(unique(xtmp[["variable"]])))

  grDevices::png(
    filename = fname,
    height = nm[[1L]] * 3,
    width = nm[[2L]] * 3,
    units = "in",
    res = 200
  )
  plot(gtmp)
  grDevices::dev.off()
}


combineTwoTopoDatasets <- function(
  x1,
  x2,
  topo_sources,
  xTopoTemplate,
  varsSimLabel,
  varsTopo,
  varTopoSource,
  doFigures = FALSE,
  pathFigure = "."
) {
  x1 <- as.data.frame(x1)
  stopifnot(
    identical(as.data.frame(xTopoTemplate)[, varsSimLabel], x1[, varsSimLabel])
  )

  varsTopoAll <- c(varsTopo, varTopoSource)
  xsim <- xTopoTemplate
  xsim[, varsTopoAll] <- x1[, varsTopoAll]

  if (length(topo_sources) == 2L) {
    x <- data.matrix(as.data.frame(xsim)[, varsTopo, drop = FALSE])

    x2 <- as.data.frame(x2)
    stopifnot(
      identical(as.data.frame(xsim)[, varsSimLabel], x2[, varsSimLabel])
    )

    ids <- which(
      is.na(x) & !is.na(x2[, varsTopo, drop = FALSE]),
      arr.ind = TRUE
    )

    x[ids] <- x2[, varsTopo][ids]

    xsim[, varsTopo] <- x

    idsSource <- unique(ids[, "row"])
    xsim[idsSource, varTopoSource] <- vapply(
      xsim[idsSource, varTopoSource, drop = TRUE],
      FUN = function(x) {
        if (anyNA(x)) topo_sources[[2L]] else toString(c(x, topo_sources[[2L]]))
      },
      FUN.VALUE = NA_character_
    )

    #--- Comparison
    if (doFigures) {
      fname_fig_scatter_topo <- file.path(
        pathFigure,
        paste0(
          "Fig-scatter_topo-variables_",
          topo_sources[[1L]], "-", topo_sources[[2L]],
          ".png"
        )
      )

      if (!file.exists(fname_fig_scatter_topo)) {
        plot_compareTopoVariables(
          fname = fname_fig_scatter_topo,
          topo1 = x1,
          topo2 = x2,
          topo_sources = topo_sources,
          varsTopo = varsTopo
        )
      }
    }
  }

  xsim
}


#------ . ------
#--- Soil functions ------

createSoilsTemplate <- function(
  x, soilHeader, varsCoords, prjCRS = 4326L, Nmax = 25L
) {
  Nsim <- nrow(x)

  varSoilsTag <- c(
    "Matricd_L", "GravelContent_L", "EvapCoeff_L", "Grass_TranspCoeff_L",
    "Shrub_TranspCoeff_L", "Tree_TranspCoeff_L", "Forb_TranspCoeff_L",
    "TranspRegion_L", "Sand_L", "Clay_L", "SOM_L", "Imperm_L",
    "SoilTemp_L"
  )
  ids <- seq_len(Nmax)

  xSoilsTemplate <- list(
    ref = NA,
    table_keys = data.frame(
      sf::st_drop_geometry(x)[, unique(c("Label", soilHeader)), drop = FALSE],
      Source_Soils = NA,
      stringsAsFactors = FALSE
    ),
    table_depths = array(
      dim = c(Nsim, 2L + Nmax),
      dimnames = list(
        NULL, c("N_horizons", "SoilDepth_cm", paste0("depth_L", ids))
      )
    ),
    table_texture = array(
      dim = c(Nsim, length(varSoilsTag) * Nmax),
      dimnames = list(
        NULL, paste0(varSoilsTag, rep(ids, each = length(varSoilsTag)))
      )
    )
  )

  xSoilsSpatial <- sf::st_as_sf(
    x = data.frame(
      x[, soilHeader, drop = FALSE],
      X_WGS84 = x[, varsCoords[[1L]], drop = TRUE],
      Y_WGS84 = x[, varsCoords[[2L]], drop = TRUE]
    ),
    coords = c("X_WGS84", "Y_WGS84"),
    remove = FALSE,
    crs = prjCRS
  )

  list(xSoilsSpatial = xSoilsSpatial, xSoilsTemplate = xSoilsTemplate)
}

copySoilColumns <- function(
  xout,
  xin,
  meta = data.frame(c(rSW2 = "", exter = "", transform = NA)),
  rowIDs = NULL
) {
  if (is.null(rowIDs)) {
    stopifnot(nrow(xout) == nrow(xin))
    rowIDs <- seq_len(nrow(xin))
  }

  hasTransformer <- "transform" %in% colnames(meta)

  for (k in seq_len(nrow(meta))) {
    ids1 <- grep(meta[k, "rSW2"], colnames(xout), value = TRUE)
    ids2 <- grep(meta[k, "exter"], colnames(xin), value = TRUE)
    tn <- seq_len(min(length(ids1), length(ids2)))
    res <- xin[rowIDs, ids2[tn], drop = FALSE]
    xout[rowIDs > 0L, ids1[tn]] <- if (
      hasTransformer && is.function(meta[k, "transform"][[1L]])
    ) {
      meta[k, "transform"][[1L]](res)
    } else {
      res
    }
  }

  xout
}


sitesWithProblematicSoils <- function(
  soils,
  vars = c(
    "Matricd_L", "GravelContent_L", "Sand_L", "Clay_L", "Silt_L", "SOM_L"
  ),
  vars_notzero = c("Sand_L", "Clay_L")
) {

  texture_checks <- suppressWarnings(
    rSW2data::check_texture_table(
      table_texture = soils[["table_texture"]],
      n_layers = soils[["table_depths"]][, "N_horizons"],
      vars = vars,
      vars_notzero = vars_notzero
    )
  )

  iddcn <- grep(
    "^depth_L[[:digit:]]{1,2}$", colnames(soils[["table_depths"]])
  )
  soildepth_checks <- suppressWarnings(
    rSW2data::check_depth_table(
      table_depths = soils[["table_depths"]][, iddcn, drop = FALSE],
      soil_depth = soils[["table_depths"]][, "SoilDepth_cm"],
      n_layers = soils[["table_depths"]][, "N_horizons"]
    )
  )

  nsdc <- c(
    "ids_sites_mismatchedDepth", "ids_sites_mismatchedLayerCount",
    "ids_sites_without_soils", "ids_sites_withDepthDiscontinuity",
    "ids_sites_withNonMonotonicDepths"
  )

  res <- c(
    texture_checks[["missing"]][["ids_sites_cond_anylayer"]],
    texture_checks[["zero"]][["ids_sites_cond_alllayers"]],
    if (!isTRUE(soildepth_checks)) unlist(soildepth_checks[nsdc])
  ) |>
    unique() |>
    sort()

  if (length(res) > 0L) {
    warning("Problematic soils: ", toString(res))
  }

  res
}


createMatrixIndexSoilProperties <- function(
  rowIDs,
  colIDs,
  variables,
  nHorizons
) {
  stopifnot(!anyNA(rowIDs), !anyNA(colIDs))

  mapply(
    function(row, n) {
      ids <- lapply(
        seq_len(n), function(k) grep(paste0("_L", k, "$"), variables)
      ) |>
        unlist()
      cbind(
        row = rep(row, times = length(ids)),
        col = colIDs[ids]
      )
    },
    row = rowIDs,
    n = nHorizons,
    SIMPLIFY = FALSE
  ) |>
    do.call(rbind, args = _)
}


plot_compareSoilProperties <- function(
  fname,
  soils1,
  soils2,
  idsSoilVars,
  soil_sources
) {
  # var_L1 -> var_L01 (for correct sorting by ggplot2)
  cns <- gsub("(_L)([[:digit:]]{1})$", "\\10\\2", colnames(soils1), perl = TRUE)

  xtmp <- data.frame(
    row = idsSoilVars[, 1L, drop = TRUE],
    variable = cns[idsSoilVars[, 2L, drop = TRUE]],
    value1 = soils1[idsSoilVars],
    value2 = soils2[idsSoilVars]
  )

  gtmp <- ggplot2::ggplot(
    data = xtmp,
    mapping = ggplot2::aes(x = value1, y = value2)
  ) +
    ggplot2::facet_wrap(ggplot2::vars(variable), scales = "free") +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(color = "blue") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
    ggplot2::xlab(soil_sources[[1L]]) +
    ggplot2::ylab(soil_sources[[2L]]) +
    ggplot2::coord_cartesian(xlim = c(0, NA), ylim = c(0, NA)) +
    ggplot2::theme_bw()

  nm <- ggplot2::wrap_dims( # corresponding to `grDevices::n2mfrow()`
    length(unique(idsSoilVars[, 2L, drop = TRUE]))
  )

  grDevices::png(
    filename = fname,
    height = nm[[1L]] * 3,
    width = nm[[2L]] * 3,
    units = "in",
    res = 200
  )
  plot(gtmp)
  grDevices::dev.off()
}


getSoilsFromSOLUS100 <- function(
  x,
  simSoilLayers,
  xSoilsTemplate,
  pathData,
  bufferDistance,
  doFigures,
  varsSimLabel,
  pathFigure,
  mapCRS
) {

  vars_solus100 <- c(
    "anylithicdpt_cm",
    "dbovendry", "fragvol", "sandtotal", "silttotal", "claytotal", "soc"
  )

  metaSOLUS100SoilProperties <- rbind(
    c(rSW2 = "Matricd", exter = "dbovendry", transform = NA),
    c(rSW2 = "GravelContent", exter = "fragvol", transform = NA),
    c(rSW2 = "Sand", exter = "sandtotal", transform = NA),
    c(rSW2 = "Clay", exter = "claytotal", transform = NA),
    # Convert soil organic carbon into soil organic matter
    c(rSW2 = "SOM", exter = "soc", transform = function(x) x / 0.58)
  )

  #--- ....** Download SOLUS100 ------
  fns_solus100 <- rSW2exter::download_SOLUS100(
    path = pathData,
    vars = vars_solus100,
    depths = rSW2exter::depth_profile_SOLUS100(),
    stat = "p",
    url_solus100 = "https://storage.googleapis.com/solus100notpub/"
  )


  #--- ....** Extract values ------
  xs100s <- rSW2exter::extract_soils_SOLUS100(
    x = x,
    path = pathData,
    vars = vars_solus100,
    var_depth = vars_solus100[[1L]],
    depths = rSW2exter::depth_profile_SOLUS100(),
    stat = "p",
    method_vertical = "interpolate_by_layer",
    requested_layer_depths = simSoilLayers,
    method_horizontal = "asis",
    digits = NA
  )


  #--- ....** Check values ------
  idsFailedSites <- sitesWithProblematicSoils(
    soils = xs100s,
    vars = c(
      "dbovendry_L", "fragvol_L",
      "sandtotal_L", "claytotal_L", "silttotal_L",
      "soc_L"
    ),
    vars_notzero = NULL
  )


  #--- ....** Problematic sites ------
  if (length(idsFailedSites) > 0L) {

    if (doFigures) {
      fname_fig_map_nosoil <- file.path(
        pathFigure, "Fig-map_soil-noData_SOLUS100.png"
      )

      if (!file.exists(fname_fig_map_nosoil)) {
        plot_map(
          fname = fname_fig_map_nosoil,
          x = x,
          var_index = varsSimLabel,
          crs = mapCRS,
          ids_highlight = idsFailedSites
        )
      }
    }


    #--- ......*** Extract with buffer ------
    warning("SOLUS100: buffer applied to sites with problematic soils.")

    xbuf <- rSW2exter::extract_soils_SOLUS100(
      x = sf::st_buffer(
        x[idsFailedSites, ],
        dist = units::set_units(bufferDistance, "m")
      ),
      path = pathData,
      vars = vars_solus100[-1L],
      var_depth = vars_solus100[[1L]],
      depths = rSW2exter::depth_profile_SOLUS100(),
      stat = "p",
      method_vertical = "interpolate_by_layer",
      requested_layer_depths = simSoilLayers,
      method_horizontal = "asis",
      fun = mean,
      digits = NA
    )

    stopifnot(xbuf[["table_depths"]][, "N_horizons"] > 0L)

    xs100s[["table_depths"]][idsFailedSites, ] <- xbuf[["table_depths"]]
    xs100s[["table_texture"]][idsFailedSites, ] <- xbuf[["table_texture"]]
    xs100s[["table_keys"]][idsFailedSites, "Comment"] <-
      "Extracted with buffer."
  }


  #--- ....** Format to template ------
  res <- xSoilsTemplate

  res[["ref"]] <- xs100s[["ref"]]

  tmp <- intersect(
    colnames(res[["table_depths"]]),
    colnames(xs100s[["table_depths"]])
  )
  res[["table_depths"]][, tmp] <-
    xs100s[["table_depths"]][, tmp, drop = FALSE]

  res[["table_texture"]] <- copySoilColumns(
    xout = res[["table_texture"]],
    xin = xs100s[["table_texture"]],
    meta = metaSOLUS100SoilProperties
  )

  res
}


getSoilsFromSDA <- function(
  x,
  simSoilLayers,
  xSoilsTemplate,
  pathData,
  doFigures,
  varsSimLabel,
  pathFigure,
  mapCRS,
  verbose
) {

  #--- ....** Obtain SSURGO mukeys ------
  fname_mukeys <- file.path(
    pathData, paste0(prjTag, "_soils-SDA-mukeysSSURGO.rds")
  )

  if (file.exists(fname_mukeys)) {
    mukeysSSURGO <- readRDS(fname_mukeys)

  } else {
    mukeysSSURGO <- rSW2exter::fetch_mukeys_spatially_NRCS_SDA(
      x = x,
      db = "SSURGO",
      progress_bar = verbose
    )

    saveRDS(mukeysSSURGO, file = fname_mukeys)
  }


  #--- ....** Extract values ------
  idsSDA <- setdiff(
    seq_len(nrow(x)), which(is.na(mukeysSSURGO[["mukeys"]]))
  )

  metaSDASoilProperties <- rbind(
    c(rSW2 = "Matricd", exter = "dbovendry"),
    c(rSW2 = "GravelContent", exter = "fragvol"),
    c(rSW2 = "Sand", exter = "sandtotal"),
    c(rSW2 = "Clay", exter = "claytotal"),
    c(rSW2 = "SOM", exter = "om")
  )

  xsdas <- rSW2exter::extract_soils_NRCS_SDA(
    x = x[idsSDA, ],
    mukeys = mukeysSSURGO[["mukeys"]][idsSDA],
    method = "SSURGO_then_STATSGO",
    remove_organic_horizons = "at_surface",
    replace_missing_fragvol_with_zero = "at_surface",
    estimate_missing_bulkdensity = TRUE,
    restrict_by_ec_or_ph = FALSE,
    impute = TRUE,
    progress_bar = TRUE,
    verbose = TRUE
  )


  #--- ....** Check values ------
  idsFailedSites <- sitesWithProblematicSoils(
    soils = xsdas,
    vars = c(
      "dbovendry_L", "fragvol_L",
      "sandtotal_L", "claytotal_L", "silttotal_L",
      "om_L"
    ),
    vars_notzero = NULL
  )


  #--- ....** Problematic sites ------
  if (length(idsFailedSites) > 0L && doFigures) {
    fname_fig_map_nosoil <- file.path(
      pathFigure, "Fig-map_soil-noData_SDA.png"
    )

    if (!file.exists(fname_fig_map_nosoil)) {
      plot_map(
        fname = fname_fig_map_nosoil,
        x = x[idsSDA, ],
        var_index = varsSimLabel,
        crs = mapCRS,
        ids_highlight = idsFailedSites
      )
    }
  }


  #--- ....** Save to disk ------
  res <- xSoilsTemplate

  tmp <- setdiff(
    colnames(xsdas[["table_keys"]]), colnames(res[["table_keys"]])
  )
  res[["table_keys"]][idsSDA, tmp] <-
    xsdas[["table_keys"]][, tmp, drop = FALSE]

  tmp <- intersect(
    colnames(res[["table_depths"]]), colnames(xsdas[["table_depths"]])
  )
  res[["table_depths"]][idsSDA, tmp] <-
    xsdas[["table_depths"]][, tmp, drop = FALSE]

  res[["table_texture"]][idsSDA, ] <- copySoilColumns(
    xout = res[["table_texture"]][idsSDA, , drop = FALSE],
    xin = xsdas[["table_texture"]],
    meta = metaSDASoilProperties
  )

  res
}


getSoilsFromKSSL <- function(
  x,
  simSoilLayers,
  xSoilsTemplate,
  pathData,
  whatKSSL,
  varsUseSoil2IfValueMissingInSoil1,
  doFigures,
  varsSimLabel,
  pathFigure,
  mapCRS
) {
  #--- Settings
  setMissingBDZero <- !(
    "Matricd" %in% varsUseSoil2IfValueMissingInSoil1
  )
  setMissingFragvolZero <- !(
    "GravelContent" %in% varsUseSoil2IfValueMissingInSoil1
  )
  setMissingSOMZero <- !(
    "SOM" %in% varsUseSoil2IfValueMissingInSoil1
  )

  metaKSSLSoilProperties <- rbind(
    c(rSW2 = "Matricd", exter = "dbovendry", transform = NA),
    c(rSW2 = "GravelContent", exter = "fragvol", transform = NA),
    c(rSW2 = "Sand", exter = "sandtotal", transform = NA),
    c(rSW2 = "Clay", exter = "claytotal", transform = NA),
    c(rSW2 = "SOM", exter = "om", transform = NA)
  )


  #--- ....** Extract values ------
  reqKSSL <- xSoilsTemplate[["table_keys"]][, whatKSSL, drop = TRUE]

  idsKSSL <- which(!is.na(reqKSSL))

  xskssl <- rSW2exter::extract_soils_KSSL(
    x = reqKSSL[idsKSSL],
    what = whatKSSL,
    fix_depths = TRUE,
    replace_missing_fragvol_with_zero = setMissingFragvolZero,
    replace_missing_om_with_zero = setMissingSOMZero,
    estimate_missing_bulkdensity = TRUE,
    digits = NA
  )


  #--- ....** Check values ------
  idsFailedSites <- sitesWithProblematicSoils(
    soils = xskssl,
    vars = c(
      "dbovendry_L", "fragvol_L",
      "sandtotal_L", "claytotal_L", "silttotal_L", "om_L"
    ),
    vars_notzero = NULL
  )


  #--- ....** Problematic sites ------
  if (length(idsFailedSites) > 0L && doFigures) {
    fname_fig_map_nosoil <- file.path(
      pathFigure, "Fig-map_soil-noData_KSSL.png"
    )

    if (!file.exists(fname_fig_map_nosoil)) {
      plot_map(
        fname = fname_fig_map_nosoil,
        x = x[idsKSSL, ],
        var_index = varsSimLabel,
        crs = mapCRS,
        ids_highlight = idsFailedSites
      )
    }
  }


  #--- ....** Save to disk ------
  res <- xSoilsTemplate

  tmp <- setdiff(
    colnames(xskssl[["table_keys"]]), colnames(res[["table_keys"]])
  )
  res[["table_keys"]][idsKSSL, tmp] <-
    xskssl[["table_keys"]][, tmp, drop = FALSE]

  tmp <- intersect(
    colnames(res[["table_depths"]]), colnames(xskssl[["table_depths"]])
  )
  res[["table_depths"]][idsKSSL, tmp] <-
    xskssl[["table_depths"]][, tmp, drop = FALSE]

  res[["table_texture"]][idsKSSL, ] <- copySoilColumns(
    xout = res[["table_texture"]][idsKSSL, , drop = FALSE],
    xin = xskssl[["table_texture"]][, , drop = FALSE],
    meta = metaKSSLSoilProperties
  )

  res
}


combineTwoSoils <- function(
  x1,
  x2,
  soil_sources,
  xSoilsTemplate,
  simSoilLayers,
  soilHeader,
  combineSoilSource2 = c("missingSoil1", "failedSoil1", "complementSoil1"),
  varsUseSoil2IfValueMissingInSoil1 = NULL,
  hasObsSoilDepth = FALSE,
  imputeLOCF = FALSE,
  allowAllSandClayOrSilt = FALSE,
  doFigures = FALSE,
  pathFigure = "."
) {

  combs <- match.arg(combineSoilSource2)

  hasOneSoil <- is.null(x2)

  stopifnot(
    !is.null(x1),
    any(
      hasOneSoil,
      identical(nrow(x1[["table_depths"]]), nrow(x2[["table_depths"]]))
    ),
    any(
      hasOneSoil,
      identical(nrow(x1[["table_texture"]]), nrow(x2[["table_texture"]]))
    )
  )

  xsoils <- if (hasOneSoil) x1 else x2
  Nsim <- nrow(xsoils[["table_depths"]])

  cnsDepth <- colnames(x1[["table_depths"]])
  idsDepths <- grep("^depth_L[[:digit:]]{1,2}$", cnsDepth)
  varDepths <- cnsDepth[idsDepths]


  #--- ......*** Sites with soils from first source ------
  idsSiteSoil1 <- if (hasOneSoil) {
    seq_len(Nsim)

  } else if (combs %in% c("missingSoil1", "complementSoil1")) {
    # Identify sites where source1 provides (some) data
    which(
      apply(x1[["table_depths"]], 1L, function(x) sum(!is.na(x)) > 0L) &
        apply(x1[["table_texture"]], 1L, function(x) sum(!is.na(x)) > 0L)
    )

  } else if (identical(combs, "failedSoil1")) {
    # Identify sites were source1 does not fail checks
    vars1 <- c("Matricd_L", "GravelContent_L", "Sand_L", "Clay_L", "SOM_L")
    tmp <- vapply(
      varsUseSoil2IfValueMissingInSoil1,
      function(var) grep(paste0("^", var), vars1),
      FUN.VALUE = NA_integer_
    )
    if (length(tmp) > 0L) vars1 <- vars1[-tmp]

    idsFailedSites <- suppressWarnings(
      sitesWithProblematicSoils(
        soils = x1, vars = vars1, vars_notzero = NULL
      )
    )

    seq_len(Nsim)[-idsFailedSites]
  }


  #--- ......*** Homogenize soil profiles ------
  if (
    any(
      identical(combs, "complementSoil1"),
      length(varsUseSoil2IfValueMissingInSoil1) > 0L
    )
  ) {
    n1 <- rSW2data::update_soil_profile(
      soil_layers = x1[["table_depths"]][idsSiteSoil1, varDepths, drop = FALSE],
      requested_soil_layers = simSoilLayers,
      soil_data = x1[["table_texture"]][idsSiteSoil1, , drop = FALSE],
      keep_prev_soildepth = TRUE,
      keep_prev_soillayers = FALSE
    )

    if (isTRUE(n1[["updated"]])) {
      x1[["table_depths"]][idsSiteSoil1, varDepths] <-
        n1[["soil_layers"]][, varDepths, drop = FALSE]

      x1[["table_depths"]][idsSiteSoil1, "N_horizons"] <- rowSums(
        !is.na(n1[["soil_layers"]][, varDepths, drop = FALSE])
      )

      tmp <- intersect(
        colnames(x1[["table_texture"]]), colnames(n1[["soil_data"]])
      )
      x1[["table_texture"]][idsSiteSoil1, tmp] <-
        n1[["soil_data"]][, tmp, drop = FALSE]
    }


    n2 <- rSW2data::update_soil_profile(
      soil_layers = xsoils[["table_depths"]][-idsSiteSoil1, varDepths, drop = FALSE],
      requested_soil_layers = simSoilLayers,
      soil_data = xsoils[["table_texture"]][-idsSiteSoil1, , drop = FALSE],
      keep_prev_soildepth = TRUE,
      keep_prev_soillayers = FALSE
    )

    if (isTRUE(n2[["updated"]])) {
      xsoils[["table_depths"]][-idsSiteSoil1, varDepths] <-
        n2[["soil_layers"]][, varDepths, drop = FALSE]

      xsoils[["table_depths"]][-idsSiteSoil1, "N_horizons"] <- rowSums(
        !is.na(n2[["soil_layers"]][, varDepths, drop = FALSE])
      )

      tmp <- intersect(
        colnames(xsoils[["table_texture"]]), colnames(n2[["soil_data"]])
      )
      xsoils[["table_texture"]][-idsSiteSoil1, tmp] <-
        n2[["soil_data"]][, tmp, drop = FALSE]
    }
  }


  #--- ......*** Soil properties from first source ------
  tmp <- setdiff(colnames(x1[["table_texture"]]), soilHeader)
  isNotAllNA <- apply(
    x1[["table_texture"]][, tmp, drop = FALSE],
    MARGIN = 2L,
    function(x) !all(is.na(x))
  )

  varsSoil1 <- tmp[isNotAllNA]


  #--- ......*** Copy soil layers from first source ------
  nSL1 <- regexpr("[[:digit:]]{1,2}$", varsSoil1) |>
    regmatches(x = varsSoil1, m = _) |>
    as.integer() |>
    max()

  stopifnot(
    nSL1 == max(x1[["table_depths"]][, "N_horizons", drop = TRUE], na.rm = TRUE)
  )


  if (combs %in% c("missingSoil1", "failedSoil1")) {
    #--- Copy depths for sites from source 1
    varUsed <- c(
      grep(
        "^depth_L[[:digit:]]{1,2}$", cnsDepth, invert = TRUE, value = TRUE
      ),
      varDepths[seq_len(nSL1)]
    )

    xsoils[["table_depths"]][idsSiteSoil1, varUsed] <-
      x1[["table_depths"]][idsSiteSoil1, varUsed, drop = FALSE]

  } else if (identical(combs, "complementSoil1")) {
    #--- Check depth consistency
    idsDepthsUsed <- mapply(
      function(row, n) {
        cbind(
          row = rep(row, times = n),
          col = idsDepths[seq_len(n)]
        )
      },
      row = idsSiteSoil1,
      n = x1[["table_depths"]][idsSiteSoil1, "N_horizons", drop = TRUE],
      SIMPLIFY = FALSE
    ) |>
      do.call(rbind, args = _)

    tmp <- intersect(cnsDepth, colnames(xsoils[["table_depths"]]))
    tmp <- xsoils[["table_depths"]][, tmp, drop = FALSE]

    if (
      !isTRUE(
        all.equal(tmp[idsDepthsUsed], x1[["table_depths"]][idsDepthsUsed])
      )
    ) {
      stop(
        "Method 'complementSoil1' requires that soils sources ",
        "provide the same soil layer depths for a site."
      )
    }
  }


  if (isTRUE(hasObsSoilDepth)) {
    stopifnot(identical(soil_sources[[1L]], "obs"))

    varsSoilProfile <- c("N_horizons", "SoilDepth_cm")
    xsoils[["table_depths"]][, varsSoilProfile] <-
      x1[["table_depths"]][, varsSoilProfile, drop = FALSE]
  }


  #--- ......*** Copy soil properties from first source ------
  stopifnot(
    identical(
      colnames(xsoils[["table_texture"]]),
      colnames(x1[["table_texture"]])
    )
  )

  # Indices of sites x soil properties to use from soil source 1
  idsSoilVars <- createMatrixIndexSoilProperties(
    rowIDs = idsSiteSoil1,
    colIDs = match(varsSoil1, colnames(x1[["table_texture"]])),
    variables = varsSoil1,
    nHorizons = x1[["table_depths"]][idsSiteSoil1, "N_horizons", drop = TRUE]
  )


  if (doFigures && !hasOneSoil) {
    fname_fig_soils <- file.path(
      pathFigure,
      paste0(
        "Fig-scatter_soil-properties_",
        soil_sources[[1L]], "-", soil_sources[[2L]],
        ".png"
      )
    )

    if (!file.exists(fname_fig_soils)) {
      plot_compareSoilProperties(
        fname = fname_fig_soils,
        soils1 = x1[["table_texture"]],
        soils2 = xsoils[["table_texture"]],
        idsSoilVars = idsSoilVars,
        soil_sources = soil_sources
      )
    }
  }


  # Remove such indices if NA and varsUseSoil2IfValueMissingInSoil1
  idsSoilKeep2 <- if (
    length(varsUseSoil2IfValueMissingInSoil1) > 0L
  ) {
    tmp <- setdiff(colnames(xsoils[["table_texture"]]), soilHeader)
    isNotAllNA <- apply(
      xsoils[["table_texture"]][, tmp, drop = FALSE],
      MARGIN = 2L,
      function(x) !all(is.na(x))
    )

    varsSoil2 <- tmp[isNotAllNA]

    tmp <- lapply(
      varsUseSoil2IfValueMissingInSoil1,
      function(var) grep(paste0("^", var), varsSoil2)
    ) |>
      unlist() |>
      sort()
    varsSoil2used <- varsSoil2[tmp]

    ids <- createMatrixIndexSoilProperties(
      rowIDs = idsSiteSoil1,
      colIDs = match(varsSoil2used, colnames(x1[["table_texture"]])),
      variables = varsSoil2used,
      nHorizons =
        x1[["table_depths"]][idsSiteSoil1, "N_horizons", drop = TRUE]
    )

    cbind(
      ids[which(is.na(x1[["table_texture"]][ids])), , drop = FALSE],
      remove = 1L
    )
  }


  if (is.null(idsSoilKeep2)) {
    idsSoilVars1 <- idsSoilVars
    idsSoilVars2 <- NULL

  } else {
    tmp <- merge(idsSoilVars, idsSoilKeep2, all.x = TRUE)
    idsSoilVars1 <- data.matrix(
      tmp[is.na(tmp[, "remove"]), c("row", "col"), drop = FALSE]
    )
    idsSoilVars2 <- idsSoilKeep2[, c("row", "col"), drop = FALSE]
  }


  if (combs %in% c("missingSoil1", "failedSoil1")) {
    if (is.null(idsSoilVars2)) {
      #--- Remove all values from source 2
      xsoils[["table_texture"]][idsSiteSoil1, ] <- NA
    } else {
      tmp <- xsoils[["table_texture"]][idsSoilVars2]
      xsoils[["table_texture"]][idsSiteSoil1, ] <- NA
      xsoils[["table_texture"]][idsSoilVars2] <- tmp
    }
  }

  xsoils[["table_texture"]][idsSoilVars1] <-
    x1[["table_texture"]][idsSoilVars1]


  #--- ......*** Impute missing soils ------
  if (isTRUE(imputeLOCF)) {
    varsSoilProperties <- colnames(xsoils[["table_texture"]]) |>
      strsplit(split = "_L", fixed = TRUE) |>
      lapply(FUN = function(x) x[[1L]]) |>
      unlist() |>
      unique()

    # reshaping from wide to semi-long
    tmp <- rSW2data::reshape_soilproperties_to_long(
      data.frame(
        Label = xsoils[["table_keys"]][["Label"]],
        xsoils[["table_texture"]]
      ),
      type_to = "long_by_properties",
      id_site = "Label",
      soilproperties = varsSoilProperties
    )

    ids <- which(
      rowSums(!is.na(tmp[, varsSoilProperties, drop = FALSE])) > 0L
    )


    # Impute
    tmpi <- rSW2data::impute_soils(
      x = tmp[ids, , drop = FALSE],
      var_values = varsSoilProperties,
      var_site_id = "Label",
      var_horizon = "soillayer",
      verbose = TRUE
    )

    # Reshape back to standard wide format
    tt <- rSW2data::reshape_soilproperties_to_wide(
      tmpi,
      type_from = "long_by_properties",
      id_site = "Label",
      soilproperties = varsSoilProperties
    )

    ids <- match(
      xsoils[["table_keys"]][["Label"]], tt[["Label"]], nomatch = 0L
    )
    ic <- intersect(colnames(xsoils[["table_texture"]]), colnames(tt))
    xsoils[["table_texture"]][ids > 0, ic] <- data.matrix(
      tt[ids, ic, drop = FALSE]
    )
  }


  #--- ......*** Set soil metadata ------
  md <- xSoilsTemplate[["table_keys"]]

  tmp1 <- setdiff(colnames(x1[["table_keys"]]), colnames(md))
  tmp2 <- setdiff(colnames(xsoils[["table_keys"]]), colnames(md))

  if (length(tmp1) > 0L) {
    md <- cbind(md, x1[["table_keys"]][, tmp1, drop = FALSE])
  }

  if (length(tmp2) > 0L) {
    tmp2a <- setdiff(tmp2, tmp1)
    md <- cbind(md, xsoils[["table_keys"]][, tmp2a, drop = FALSE])
    tmp2b <- setdiff(tmp2, tmp2a)
    if (length(tmp2b) > 0L) {
      md[-idsSiteSoil1, tmp2b] <-
        xsoils[["table_keys"]][-idsSiteSoil1, tmp2b, drop = FALSE]
    }
  }

  md[idsSiteSoil1, "Source_Soils"] <- soil_sources[[1L]]
  if (!hasOneSoil) {
    md[-idsSiteSoil1, "Source_Soils"] <- soil_sources[[2L]]
  }

  xsoils[["table_keys"]] <- md


  #--- ....** Check values ------
  idsFailedSites <- sitesWithProblematicSoils(
    soils = xsoils,
    vars = c("Matricd_L", "GravelContent_L", "Sand_L", "Clay_L", "SOM_L"),
    vars_notzero = if (!isTRUE(allowAllSandClayOrSilt)) c("Sand_L", "Clay_L")
  )

  if (length(idsFailedSites) > 0L) {
    warning(
      "Excluding n = ", length(idsFailedSites),
      " runs because of problematic soils data."
    )

    xsoils[["table_keys"]][idsFailedSites, "Include_YN"] <- 0L
    xsoils[["table_keys"]][idsFailedSites, "exclusionReason"] <-
      "Problematic soils data."
  }

  xsoils
}


#------ . ------
#--- Vegetation functions ------

createVegetationTemplate <- function(x, vegHeader, pfts) {
  xveg <- sf::st_drop_geometry(x[, vegHeader, drop = FALSE])

  # Add vegetation composition columns
  varsVeg <- paste0("Composition_", pfts, "Fraction")
  xveg[, varsVeg] <- NA

  # Add biomass columns
  for (kt in seq_along(pfts)) {
    tmp <- c(
      paste0(pfts[[kt]], "_Biomass_m", seq_len(12L)),
      paste0(pfts[[kt]], "_FractionLive_m", seq_len(12L)),
      paste0(pfts[[kt]], "_BiomassLive_m", seq_len(12L)),
      paste0(pfts[[kt]], "_LAIconv_m", seq_len(12L)),
      paste0(pfts[[kt]], "_Litter_m", seq_len(12L))
    )
    varsVeg <- c(varsVeg, tmp)
    xveg[, tmp] <- NA
  }

  list(xveg = xveg, varsVeg = varsVeg)
}

#------ . ------
