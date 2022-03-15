#---Downscaling/bias-correction functions


#' Set default paths to projected climate data sets unless already specified
#' @noRd
get_path_to_projectedclimate_datasources <- function(
  project_paths,
  clim_source
) {
  dir_ex_dat <- NULL

  pp <- project_paths
  dir_clim <- file.path(pp[["dir_ex_fut"]], "ClimateScenarios")
  ncs <- paste0("dir_", clim_source)

  if (identical(clim_source, "CMIP3_ClimateWizardEnsembles_Global")) {
    dir_ex_dat <- if (has_elem_name(ncs, pp)) {
      pp[[ncs]]
    } else {
      file.path(dir_clim, "ClimateWizardEnsembles_Global")
    }

  } else if (identical(clim_source, "CMIP3_ClimateWizardEnsembles_USA")) {
    dir_ex_dat <- if (has_elem_name(ncs, pp)) {
      pp[[ncs]]
    } else {
      file.path(dir_clim, "ClimateWizardEnsembles_USA")
    }

  } else {
    dir_ex_dat <- if (has_elem_name(ncs, pp)) {
      pp[[ncs]]
    } else {
      tmp <- strsplit(clim_source, split = "_", fixed = TRUE)[[1]]
      file.path(
        dir_clim,
        tmp[1],
        paste(tmp[-1], collapse = "_")
      )
    }
  }

  dir_ex_dat
}



#' Meta data for climate scenarios
#' @export
climscen_metadata <- function() {
  #--- Meta information of climate datasets
  template_bbox <- data.frame(
    matrix(nrow = 2, ncol = 2, dimnames = list(NULL, c("lat", "lon")))
  )
  template_tbox <- data.frame(
    matrix(
      nrow = 2,
      ncol = 2,
      dimnames = list(c("start", "end"), c("first", "second"))
    )
  )


  # SOILWAT2 required units are c("cm/day", "C", "C")
  var_names_fixed <- c("prcp", "tmin", "tmax", "tmean")

  climDB_metas <- list(
    CMIP3_ClimateWizardEnsembles_Global = list(
      convention = "ClimateWizardEnsembles",
      tres = "monthly",
      bbox = fill_bounding_box(
        template_bbox,
        list(y = c(-55, 84), x = c(-180, 180))
      ),
      tbox = fill_bounding_box(
        template_tbox,
        list(t1 = c(NA, NA), t2 = c(2070, 2099))
      ),
      units = c(prcp = "%", tmin = "C", tmax = "C", tmean = "C"),
      var_desc = data.frame(
        varname = NA,
        tag = NA,
        fileVarTags = NA,
        unit_given = NA,
        unit_real = NA
      )[0, ],
      sep_fname = NULL,
      str_fname = NULL
    ),

    CMIP3_ClimateWizardEnsembles_USA = list(
      convention = "ClimateWizardEnsembles",
      tres = "monthly",
      bbox = fill_bounding_box(
        template_bbox,
        list(y = c(25.125, 49.375), x = c(-124.75, -67))
      ),
      tbox = fill_bounding_box(
        template_tbox,
        list(t1 = c(NA, NA), t2 = c(2070, 2099))
      ),
      units = c(prcp = "%", tmin = "C", tmax = "C", tmean = "C"),
      var_desc = data.frame(
        varname = NA,
        tag = NA,
        fileVarTags = NA,
        unit_given = NA,
        unit_real = NA
      )[0, ],
      sep_fname = NULL,
      str_fname = NULL
    ),

    CMIP3_BCSD_GDODCPUCLLNL_Global = list(
      convention = "CF",
      tres = "monthly",
      bbox = fill_bounding_box(
        template_bbox,
        list(
          y = c(-55.25 - 0.25, 83.25 + 0.25),
          x = c(-179.75 - 0.25, 179.75 + 0.25)
        )
      ),
      tbox = fill_bounding_box(
        template_tbox,
        list(t1 = c(1950, 1999), t2 = c(2000, 2099))
      ),
      var_desc = data.frame(
        tag = tmp <- c("Prcp", "Tmin", "Tmax", "Tavg"),
        fileVarTags = paste("monthly", tmp, sep = "."),
        varname = tmp,
        unit_given = tmp <- c("mm/d", "C", "C", "C"),
        unit_real = tmp,
        row.names = var_names_fixed,
        stringsAsFactors = FALSE
      ),
      sep_fname = ".",
      str_fname = c(
        id_var = 5,
        id_gcm = 2,
        id_scen = 1,
        id_run = 3,
        id_timestart = 6,
        id_timeend = 6
      )
    ),

    CMIP5_BCSD_GDODCPUCLLNL_Global = list(
      convention = "CF",
      tres = "monthly",
      bbox = fill_bounding_box(
        template_bbox,
        list(
          y = c(-55.25 - 0.25, 83.25 + 0.25),
          x = c(-179.75 - 0.25, 179.75 + 0.25)
        )
      ),
      tbox = fill_bounding_box(
        template_tbox,
        list(t1 = c(1950, 2005), t2 = c(2006, 2099))
      ),
      var_desc = data.frame(
        tag = tmp <- c("pr", "tasmin", "tasmax", "tas"),
        fileVarTags = paste0("_", tmp, "_"),
        varname = tmp,
        unit_given = tmp <- c("mm/d", "C", "C", "C"),
        unit_real = tmp,
        row.names = var_names_fixed,
        stringsAsFactors = FALSE
      ),
      sep_fname = "_",
      str_fname = c(
        id_var = 3,
        id_gcm = 5,
        id_scen = 6,
        id_run = 7,
        id_timestart = 8,
        id_timeend = 8
      )
    ),

    CMIP3_BCSD_GDODCPUCLLNL_USA = list(
      convention = "CF",
      tres = "monthly",
      bbox = fill_bounding_box(
        template_bbox,
        list(y = c(25.125, 52.875), x = c(-124.625, -67))
      ),
      tbox = fill_bounding_box(
        template_tbox,
        list(t1 = c(1950, 1999), t2 = c(2000, 2099))
      ),
      var_desc = data.frame(
        tag = tmp <- c("Prcp", "Tmin", "Tmax", "Tavg"),
        fileVarTags = paste("monthly", tmp, sep = "."),
        varname = tmp,
        unit_given = tmp <- c("mm/d", "C", "C", "C"),
        unit_real = tmp,
        row.names = var_names_fixed,
        stringsAsFactors = FALSE
      ),
      sep_fname = ".",
      str_fname = c(
        id_var = 5,
        id_gcm = 2,
        id_scen = 1,
        id_run = 3,
        id_timestart = 6,
        id_timeend = 6
      )
    ),

    CMIP5_BCSD_GDODCPUCLLNL_USA = list(
      convention = "CF",
      tres = "monthly",
      bbox = fill_bounding_box(
        template_bbox,
        list(y = c(25.125, 52.875), x = c(-124.625, -67))
      ),
      tbox = fill_bounding_box(
        template_tbox,
        list(t1 = c(1950, 2005), t2 = c(2006, 2099))
      ),
      var_desc = data.frame(
        tag = tmp <- c("pr", "tasmin", "tasmax", "tas"),
        fileVarTags = paste0("_", tmp, "_"),
        varname = tmp,
        unit_given = tmp <- c("mm/d", "C", "C", "C"),
        unit_real = tmp,
        row.names = var_names_fixed,
        stringsAsFactors = FALSE
      ),
      sep_fname = "_",
      str_fname = c(
        id_var = 3,
        id_gcm = 5,
        id_scen = 6,
        id_run = 7,
        id_timestart = 8,
        id_timeend = 8
      )
    ),

    CMIP5_BCSD_NEX_USA = list(
      convention = "NEX",
      tres = "monthly",
      bbox = fill_bounding_box(
        template_bbox,
        list(y = c(24.0625, 49.9375), x = c(-125.02083333, -66.47916667))
      ),
      tbox = fill_bounding_box(
        template_tbox,
        list(t1 = c(1950, 2005), t2 = c(2006, 2099))
      ),
      var_desc = data.frame(
        tag = tmp <- c("pr", "tasmin", "tasmax", "tas"),
        fileVarTags = paste0("_", tmp, "_"),
        varname = tmp,
        unit_given = tmp <- c("kg/m2/s", "K", "K", "K"),
        unit_real = tmp,
        row.names = var_names_fixed,
        stringsAsFactors = FALSE
      ),
      sep_fname = NULL,
      str_fname = NULL
    ), # online access, i.e., no file names to parse

    CMIP5_BCSD_SageSeer_USA = list(
      convention = "CF",
      tres = "monthly",
      bbox = fill_bounding_box(
        template_bbox,
        list(y = c(31.75333, 49.00701), x = c(-124.2542, -102.2534))
      ),
      tbox = fill_bounding_box(
        template_tbox,
        list(t1 = c(1980, 1999), t2 = c(2070, 2099))
      ),
      var_desc = data.frame(
        tag = tmp <- c("pr", "tasmin", "tasmax", "tas"),
        varname = tmp,
        fileVarTags = paste0("_", tmp, "_"),
        unit_given = c("kg m-2 s-1", "K", "K", "K"),
        unit_real = c("mm/month", "C", "C", "C"),
        row.names = var_names_fixed,
        stringsAsFactors = FALSE
      ),
      sep_fname = "_",
      str_fname = c(
        id_var = 2,
        id_gcm = 4,
        id_scen = 5,
        id_run = 6,
        id_timestart = 7,
        id_timeend = 7
      )
    ),

    CMIP5_ESGF_Global = list(
      convention = "CF",
      tres = "monthly",
      bbox = fill_bounding_box(
        template_bbox,
        list(y = c(-90, 90), x = c(-180 - 0.25, 180 + 0.25))
      ),
      tbox = fill_bounding_box(
        template_tbox,
        list(t1 = c(1950, 2005), t2 = c(2006, 2100))
      ),
      var_desc = data.frame(
        tag = tmp <- c("pr", "tasmin", "tasmax", "tas"),
        fileVarTags = paste0(tmp, "_"),
        varname = tmp,
        unit_given = tmp <- c("kg m-2 s-1", "K", "K", "K"),
        unit_real = tmp,
        row.names = var_names_fixed,
        stringsAsFactors = FALSE
      ),
      sep_fname = "_",
      str_fname = c(
        id_var = 1,
        id_gcm = 3,
        id_scen = 4,
        id_run = 5,
        id_timestart = 6,
        id_timeend = 6
      )
    ),

    CMIP5_MACAv2metdataAgg_USA = list(
      convention = "CF",
      tres = "daily",
      bbox = fill_bounding_box(
        template_bbox,
        list(y = c(25.06308, 49.39602), x = c(-124.77216, -67.06476))
      ),
      tbox = fill_bounding_box(
        template_tbox,
        list(t1 = c(1950, 2005), t2 = c(2006, 2099))
      ),
      var_desc = data.frame(
        varname = c("precipitation", "air_temperature", "air_temperature", NA),
        tag = tmp <- c("pr", "tasmin", "tasmax", "tas"),
        fileVarTags = paste0("_", tmp, "_"),
        unit_given = c("mm", "K", "K", "K"),
        unit_real = c("mm/d", "K", "K", "K"),
        row.names = var_names_fixed,
        stringsAsFactors = FALSE
      ),
      sep_fname = "_",
      str_fname = c(
        id_var = 3,
        id_gcm = 4,
        id_scen = 6,
        id_run = 5,
        id_timestart = 7,
        id_timeend = 8
      )
    ),

    CMIP5_MACAv2metdata_USA = list(
      convention = "CF",
      tres = "daily",
      bbox = fill_bounding_box(
        template_bbox,
        list(y = c(25.06308, 49.39602), x = c(-124.77216, -67.06476))
      ),
      tbox = fill_bounding_box(
        template_tbox,
        list(t1 = c(1950, 2005), t2 = c(2006, 2099))
      ),
      var_desc = data.frame(
        varname = c("precipitation", "air_temperature", "air_temperature", NA),
        tag = tmp <- c("pr", "tasmin", "tasmax", "tas"),
        fileVarTags = paste0("_", tmp, "_"),
        unit_given = c("mm", "K", "K", "K"),
        unit_real = c("mm/d", "K", "K", "K"),
        row.names = var_names_fixed,
        stringsAsFactors = FALSE
      ),
      sep_fname = "_",
      str_fname = c(
        id_var = 2,
        id_gcm = 3,
        id_scen = 5,
        id_run = 4,
        id_timestart = 6,
        id_timeend = 7
      )
    )
  )

  climDB_metas
}


#------Helper functions

unique_times <- function(timeSlices, slice) {
  starts <- stats::na.exclude(
    timeSlices$Year[timeSlices$Slice == slice & timeSlices$Time == "start"]
  )
  ends <- stats::na.exclude(
    timeSlices$Year[timeSlices$Slice == slice & timeSlices$Time == "end"]
  )
  tmp <- lapply(seq_along(starts), function(x) starts[x]:ends[x])
  tmp1 <- vector("integer", length = 0)
  for (it in seq_along(tmp)) {
    tmp1 <- union(tmp1, tmp[[it]])
  }
  n <- 1 + length(tmp2 <- which(diff(tmp1) > 1))
  tmp2 <- c(1, 1 + tmp2, length(tmp1) + 1)
  res <- matrix(NA, nrow = n, ncol = 2)
  for (it in seq_len(n)) {
    res[it, ] <- c(tmp1[tmp2[it]], tmp1[tmp2[it + 1] - 1])
  }

  res
}

useSlices <- function(getYears, timeSlices, run, slice) {
  res <- rep(FALSE, length = nrow(getYears[[slice]]))

  tmp <- timeSlices$Year[timeSlices$Run == run & timeSlices$Slice == slice]

  if (!anyNA(tmp)) {
    istart <- findInterval(
      tmp[1],
      getYears[[slice]][, 1],
      rightmost.closed = FALSE,
      all.inside = FALSE
    )
    iend <- findInterval(
      tmp[2],
      getYears[[slice]][, 2],
      rightmost.closed = FALSE,
      all.inside = FALSE
    )

    res[istart:iend] <- TRUE
  }

  res
}

fill_bounding_box <- function(box, vals) {
  box[] <- vals
  box
}



#' Additive Precipitation adjustment by a delta value
#'
#' Additive Precipitation adjustment by a delta value
#'
#' Provide one of the arguments \code{addDelta} or \code{deltaPerEvent}, but not both.
#' The value of \code{addDelta} will be evenly split up among \code{ind_events}.
#'
#' @param data A numeric vector. Daily values of precipitation.
#' @param ind_events A logical or integer vector of the same length as \code{data} or
#'    \code{NULL}. If logical, then \code{TRUE}/\code{FALSE} for each element/day of
#'    \code{data}  whether it precipitates or not. If integer, then the indices of
#'    \code{data} on which it precipitates. If \code{NULL}, then it will be calculated as
#'    \code{data > 0}.
#' @param addDelta A numeric value or \code{NULL}. The total amount of precipitation
#'    that is to be added/removed from \code{data[ind_events]} together
#'    (divided among events), i.e., it requires the same unit as \code{data}.
#' @param deltaPerEvent A numeric vector of the length equal to \code{sum(ind_events)} or
#'    \code{NULL}. The daily amount of precipitation that is to be added/removed from
#'    each \code{data[ind_events]}, i.e., it requires the same unit as \code{data}.
#'
#' @return A list with two elements
#'  \describe{
#'    \item{data}{A copy of \code{data} with adjusted values.}
#'    \item{PPT_to_remove}{The total amount of precipitation that could not be removed
#'      from \code{data} due to lack of precipitation.}
#'  }
add_delta_to_PPT <- function(
  data,
  ind_events = NULL,
  addDelta = NULL,
  deltaPerEvent = NULL
) {
  stopifnot(xor(is.null(deltaPerEvent), is.null(addDelta)))

  if (is.null(ind_events)) ind_events <- data > 0
  if (!is.null(addDelta)) {
    elems_N <- if (is.logical(ind_events)) {
      sum(ind_events)
    } else {
      length(ind_events)
    }
    deltaPerEvent <- rep(addDelta[1] / elems_N, elems_N)
  }
  StillToSubtract <- 0

  if (all(deltaPerEvent > 0)) {
    # All deltas are additions -> no problem
    data[ind_events] <- data[ind_events] + deltaPerEvent
  } else {
    # There are subtractions -> check that all adjusted precipitation days > 0
    newRainyValues <- data[ind_events] + deltaPerEvent
    posRainyDays <- newRainyValues >= 0

    if (all(posRainyDays)) {
      # all ok
      data[ind_events] <- newRainyValues
    } else {
      # Some rainy days would now be negative; this is not ok
      negRainyDays <- !posRainyDays
      data[ind_events][posRainyDays] <- newRainyValues[posRainyDays]
      data[ind_events][negRainyDays] <- 0
      # 'StillToSubtract' is negative
      StillToSubtract <- sum(newRainyValues[negRainyDays])
      ppt_avail <- sum(data[ind_events])

      if (ppt_avail > 0) {
        # There is precipitation of the already adjusted rainy days from which
        # to subtract
        tmp <- Recall(
          data = data,
          ind_events = data > 0,
          addDelta = StillToSubtract
        )
        data <- tmp$data
        StillToSubtract <- tmp$PPT_to_remove
      }
    }
  }

  list(data = data, PPT_to_remove = StillToSubtract)
}

#' Adds/removes elements of data
#'
#' Adds/removes elements of data until \code{identical(length(data), targetLength)}
#'
#' Removal of elements is by random sampling. Addition of elements is by randomly
#' placing them within the sequence of days of \code{data} and them randomly
#' assigning values of the original \code{data} with replacement.
#'
#' @param data A numeric vector. Daily values of precipitation.
#' @param targetLength An integer value.
#' @param seed A seed set, \code{NULL}, or \code{NA}. \code{NA} will not affect
#'  the state of the \acronym{RNG}; \code{NULL} will re-initialize the \acronym{RNG};
#'  and all other values are passed to \code{\link{set.seed}}.
#'
#' @return A copy of \code{data} with adjusted length.
fix_PPTdata_length <- function(data, targetLength, seed = NA) {
  targetLength <- as.integer(targetLength)
  stopifnot(targetLength >= 0)

  Ndiff <- length(data) - targetLength
  absNdiff <- abs(Ndiff)

  if (!is.na(seed)) set.seed(seed)

  if (absNdiff > 0) {
    if (Ndiff > 0) {
      # remove random days
      ids <- sample(x = length(data), size = absNdiff, replace = FALSE)
      data <- data[-ids]
    } else {
      # Imputation
      # add random days with randomly sampled precipitation
      ids <- sample(x = targetLength, size = absNdiff, replace = FALSE)
      tmp <- rep(0, targetLength)
      tmp[-ids] <- data
      tmp[ids] <- sample(x = data, size = absNdiff, replace = TRUE)
      data <- tmp
    }
  }

  data
}

#' Distribute precipitation among days without too extreme values
#'
#' @param data_N An integer value. The number of days of the precipitation record to
#'  consider.
#' @param this_newPPTevent_N An integer value. The number of days which will received
#'  'spill-over' precipitation.
#' @param sigmaN An integer value. The multiplier of \code{this_newPPTevent_N} to
#'  determine the range of days to consider.
#' @param this_i_extreme An integer value. The index indicating for which day in the
#'  precipitation record, precipitation is removed and redistributed.
#' @param this_pptToDistribute An integer value. The amount of precipitation that was
#'  removed from day \code{this_i_extreme} and is now redistributed to other days.
#' @param seed A seed set, \code{NULL}, or \code{NA}. \code{NA} will not affect
#'  the state of the \acronym{RNG}; \code{NULL} will re-initialize the \acronym{RNG};
#'  and all other values are passed to \code{\link{set.seed}}.
calc_Days_withLoweredPPT <- function(
  data_N,
  this_newPPTevent_N,
  sigmaN,
  this_i_extreme,
  this_pptToDistribute,
  seed = NA
) {
  this_newPPTevent_N <- max(0L, as.integer(this_newPPTevent_N))
  if (!is.na(seed)) set.seed(seed)

  # Randomly select days within plus/minus sigmaN days from a normal distribution
  tmp <- (-sigmaN * this_newPPTevent_N):(sigmaN * this_newPPTevent_N)
  xt <- tmp[this_i_extreme + tmp > 0]
  this_xt <- this_i_extreme + xt
  xt <- xt[1 <= this_xt & this_xt <= data_N] # do not select days from previous or next year
  probs <- stats::dnorm(x = xt, mean = 0, sd = this_newPPTevent_N)
  probs[which.max(probs)] <- 0
  tmp <- sample(x = xt, size = this_newPPTevent_N, replace = FALSE, prob = probs)
  # dayDelta <- tmp[.Internal(order(na.last = TRUE, decreasing = FALSE, abs(tmp)))] # .Internal() is not allowed in packages
  dayDelta <- tmp[(order(abs(tmp), na.last = TRUE, decreasing = FALSE))]

  # Distribute PPT to add among the selected days with a linear decay function
  newDays <- this_i_extreme + dayDelta
  tmp <- 1 / abs(dayDelta)
  newPPT <- this_pptToDistribute * tmp / sum(tmp)

  list(newDays = newDays, newPPT = newPPT)
}


#' Check for and handle days with too extreme precipitation values
#'
#' @param data A numeric vector. Daily values of precipitation.
#' @param dailyPPTceiling A numeric value. The maximum value of daily precipitation.
#'  Values above this limit will be removed and redistributed to other days.
#' @param do_checks A logical value. See details.
#' @param sigmaN An integer value. A multiplier of \code{stats::sd} for data checks.
#' @param mfact A numeric value. See details.
#' @param seed A seed set, \code{NULL}, or \code{NA}. \code{NA} will not affect
#'  the state of the \acronym{RNG}; \code{NULL} will re-initialize the \acronym{RNG};
#'  and all other values are passed to \code{\link{set.seed}}.
#'
#' @details If \code{do_check == TRUE} and any daily precipitation is equal or larger than
#'  \code{mfact * dailyPPTceiling}, then the code will error out.
controlExtremePPTevents <- function(
  data,
  dailyPPTceiling,
  sigmaN,
  do_checks = FALSE,
  mfact = 10,
  seed = NA
) {
  if (do_checks) {
    # something went wrong, e.g., GCM data is off;
    # (10 / 1.5 * dailyPPTceiling) -> dailyPPTtoExtremeToBeReal
    # if more than 1000% of observed value then assume that
    # something went wrong and error out
    stopifnot(data < mfact * dailyPPTceiling)
  }

  data_N <- length(data)
  i_extreme <- which(data > dailyPPTceiling)
  irep <- 0

  if (!is.na(seed)) set.seed(seed)

  while (length(i_extreme) > 0 && irep < 30) {
    # limit calls: if too many wet days,
    # then not possible to distribute all the water!
    newValues <-
      dailyPPTceiling *
        stats::runif(n = length(i_extreme), min = 0.9, max = 1)
    pptToDistribute <- data[i_extreme] - newValues
    data[i_extreme] <- newValues
    newPPTevent_N <- ceiling(pptToDistribute / dailyPPTceiling)

    # more days with dailyPPTceiling precipitation would be necessary
    # than are available
    stopifnot(sum(newPPTevent_N) <= data_N)

    for (i in seq_along(i_extreme)) {
      newPPTevents <- calc_Days_withLoweredPPT(
        data_N = data_N,
        this_newPPTevent_N = newPPTevent_N[i],
        sigmaN = sigmaN,
        this_i_extreme = i_extreme[i],
        this_pptToDistribute = pptToDistribute[i]
      )

      data[newPPTevents$newDays] <-
        data[newPPTevents$newDays] + newPPTevents$newPPT
    }

    # prepare for next iteration in case a day with previous ppt got too much ppt
    i_extreme <- which(data > dailyPPTceiling)
    irep <- irep + 1
  }

  if (do_checks) rSW2utils::test_sigmaGamma(data = data, sigmaN)

  data
}

#' Add/multiply deltas to historic daily data to generate future daily
#' \pkg{rSOILWAT2}-formatted weather.
#'
#' Used by \code{downscale.raw}, \code{downscale.delta}, and \code{downscale.deltahybrid}
applyDeltas <- function(
  obs.hist.daily,
  obs.hist.monthly,
  delta_ts,
  ppt_fun,
  sigmaN = 6,
  do_checks = FALSE
) {
  # Hamlet et al. 2010: "an arbitrary ceiling of 150% of the observed maximum
  # precipitation value for each cell is also imposed by
  # "spreading out" very large daily precipitation values into one or more
  # adjacent days"
  dailyPPTceiling <-
    1.5 * max(sapply(obs.hist.daily, FUN = function(obs) max(obs@data[, 4])))

  res <- lapply(
    obs.hist.daily,
    function(obs) {
      month <- as.POSIXlt(paste(obs@year, obs@data[, "DOY"], sep = "-"), format = "%Y-%j", tz = "UTC")$mon + 1
      ydelta <- delta_ts[delta_ts[, "Year"] == obs@year, -(1:2)]
      tmax <- obs@data[, "Tmax_C"] + ydelta[month, "Tmax_C"]

      if (do_checks) rSW2utils::test_sigmaNormal(data = tmax, sigmaN)

      tmin <- obs@data[, "Tmin_C"] + ydelta[month, "Tmin_C"]
      if (do_checks) rSW2utils::test_sigmaNormal(data = tmin, sigmaN)

      ppt_data <- unlist(lapply(1:12, function(m) {
        im_month <- month == m
        m_ydelta <- ydelta[m, 3]
        m_data <- obs@data[im_month, "PPT_cm"]
        if (ppt_fun[m] == "*") { # multiply ppt
          res <- m_data * m_ydelta
        } else if (m_ydelta == 0) { # add ppt here and below: nothing to add
          res <- m_data
        } else if (any(i_rainyDays <- m_data > 0)) { # there are rainy days in the historic record: add to those
          res <- add_delta_to_PPT(data = m_data, ind_events = i_rainyDays, addDelta = m_ydelta)$data
        } else { # there are no rainy days in the historic record
          if (m_ydelta > 0) { # we need rainy days in the historic record to add precipitation
            if (any(i_rainyMYears <- obs.hist.monthly[obs.hist.monthly[, "Month"] == m, "PPT_cm"] > 0)) {
              # sample from the same historic month in an other with rainy days instead
              # Locate data of same month in other year
              i_newYear <- which(i_rainyMYears)[which.min(abs(obs.hist.monthly[obs.hist.monthly[, "Month"] == m, "PPT_cm"][i_rainyMYears] - m_ydelta))]
              newMonth <- as.POSIXlt(paste((newObs <- obs.hist.daily[i_newYear][[1]])@year, newObs@data[, "DOY"], sep = "-"), format = "%Y-%j", tz = "UTC")$mon + 1
              newMonthData <- newObs@data[, "PPT_cm"][newMonth == m]
              # Adjust data
              newMonthData <- fix_PPTdata_length(data = newMonthData, targetLength = sum(im_month)) # adjust number of days in case we got a leap year February issue
              res <- add_delta_to_PPT(newMonthData, newMonthData > 0, addDelta = m_ydelta)$data
            } else if (any(i_rainyMonth <- obs.hist.monthly[, "PPT_cm"] > 0)) { # no rainy day for this month in historic record: locate rainy days in any months from other years
              # Locate data of any month in any year
              i_newMYear <- which(i_rainyMonth)[which.min(abs(obs.hist.monthly[i_rainyMonth, "PPT_cm"] - m_ydelta))]
              i_newYear <- which(obs.hist.monthly[i_newMYear, "Year"] == sort(unique(obs.hist.monthly[, "Year"])))
              newMonth <- as.POSIXlt(paste((newObs <- obs.hist.daily[i_newYear][[1]])@year, newObs@data[, "DOY"], sep = "-"), format = "%Y-%j", tz = "UTC")$mon + 1
              newMonthData <- newObs@data[, "PPT_cm"][newMonth == obs.hist.monthly[i_newMYear, "Month"]]
              # Adjust data
              newMonthData <- fix_PPTdata_length(data = newMonthData, targetLength = sum(im_month)) # adjust number of days in case we got a month with a different number of days
              res <- add_delta_to_PPT(newMonthData, newMonthData > 0, addDelta = m_ydelta)$data
            } else {
              stop(paste("no rainy day in historic record, but requested for the future prediction"))
            }
          } else { # there is no rain in the historic record, so we cannot remove any
            res <- rep(0, length(m_data))
          }
        }
        return(res)
      }))

      ppt <- controlExtremePPTevents(
        data = ppt_data, dailyPPTceiling,
        do_checks = do_checks, sigmaN = sigmaN
      )

      new(
        "swWeatherData",
        data = round(
          data.matrix(
            cbind(obs@data[, "DOY"], tmax, tmin, ppt),
            rownames.force = FALSE
          ),
          2
        ),
        year = obs@year
      )
    }
  )

  res
}



#' Add/multiply deltas to historic daily precipitation to generate future daily
#'  precipitation without checks
#'
#' @param m An integer vector. Each element corresponds to a day (i.e., \code{length(m)}
#'  is 365 or 366 days) and the values are the number of the month.
#' @param data A numeric vector. Precipitation of each day.
#' @param ydelta A numeric vector. Delta values for each day. If computed deltas are
#'  monthly, then they must be repeated for each day before passed as argument to this
#'  function.
#' @param add_days A logical vector. \code{TRUE} for each day for which \code{ydelta} is
#'  applied additively.
#' @param mult_days A logical vector. \code{TRUE} for each day for which \code{ydelta} is
#'  applied multiplicatively.
#' @param set_negPPT_to0 A logical value. If \code{TRUE} (default) then force days with
#'  resulting negative values of precipitation to 0 -- thereby introducing a positive bias.
#'
#' @return A copy of \code{data} with adjusted values.
applyPPTdelta_simple <- function(
  m,
  data,
  ydelta,
  add_days,
  mult_days,
  set_negPPT_to0 = TRUE
) {
  ppt <- rep(0, length(data))
  ievents <- data > 0

  # additive delta
  if (any(add_days)) {
    # Spread monthly 'delta' amount of PPT to each daily precip event
    events_per_month <- tapply(as.integer(ievents), m, sum)[m]
    itmp <- add_days & ievents
    ppt[itmp] <- data[itmp] + ydelta[itmp] / events_per_month[itmp]

    # Simple correction for negative precipitation that can arise from
    # subtractive deltas
    if (set_negPPT_to0) {
      negppt <- ppt[itmp] < 0
      if (any(negppt)) {
        ppt[itmp][negppt] <- 0
      }
    }
  }

  # multiplicative delta
  if (any(mult_days)) {
    itmp <- mult_days & ievents
    ppt[itmp] <- data[itmp] * ydelta[itmp]
  }

  ppt
}

#' Add/multiply deltas to historic daily precipitation to generate future daily
#' precipitation with checks
#'
#' @inheritParams applyPPTdelta_simple
#' @inheritParams downscale
#' @return A list with two elements
#'  \describe{
#'    \item{data}{A copy of \code{data} with adjusted values.}
#'    \item{PPT_to_remove}{The total amount of precipitation that could not be removed
#'      from \code{data} due to lack of precipitation.}
#'  }
applyPPTdelta_detailed <- function(
  m,
  data,
  ydelta,
  add_days,
  mult_days,
  daily,
  monthly
) {
  ppt <- rep(0, length(data))
  PPT_to_remove <- 0

  # multiplicative delta
  if (any(mult_days)) {
    ppt[mult_days] <- data[mult_days] * ydelta[mult_days]
  }

  # additive delta
  if (any(add_days) && sum(abs(ydelta[add_days])) > 0) {
    ievents <- data > 0
    ievents_add <- ievents & add_days

    if (any(ievents_add)) {
      # there is precipitation among the days with additive delta: add to/subtract from those
      eventsN_add_per_month <- tapply(as.integer(ievents_add), m, sum)[m]
      tmp <- add_delta_to_PPT(
        data = data[add_days],
        ind_events = ievents_add[add_days],
        deltaPerEvent = (ydelta / eventsN_add_per_month)[ievents_add]
      )
      ppt[add_days] <- tmp[["data"]]

      if (tmp[["PPT_to_remove"]] < 0) {
        # there was not enough precipitation among the additive days; attempt to remove precipitation from all days
        tmp <- add_delta_to_PPT(
          data = ppt,
          ind_events = ievents,
          addDelta = tmp[["PPT_to_remove"]]
        )
        ppt <- tmp[["data"]]
        PPT_to_remove <- PPT_to_remove + tmp[["PPT_to_remove"]]
      }
    } else {
      # there are no precip days in the data with additive delta
      idelta_pos <- ydelta > 0 & ievents_add
      idelta_neg <- ydelta < 0 & ievents_add

      if (any(idelta_pos)) {
        # there are positive deltas: sample days with precipitation events from the historic record (to preserve precipitation event distribution) and adjust the amounts
        ipos_months <- m[idelta_pos]

        for (im in unique(ipos_months)) {
          this_month_im <- monthly[, "Month"] == im
          month_precip <- monthly[, "PPT_cm"] > 0
          this_month_precip <- month_precip & this_month_im
          precip_target <- (tmp <- ydelta[idelta_pos][ipos_months == im])[1] / length(tmp)

          if (any(this_month_precip)) {
            # locate data from the same historic month 'im' from a different year with the most similar monthly PPT
            itmp <- this_month_precip
          } else {
            # this month 'im' has no precipitation in all years
            if (any(month_precip)) {
              # locate data from any month from any year with the most similar monthly PPT
              itmp <- month_precip
            } else {
              stop(paste(
                "Historic record has no days with precipitation,",
                "but requested for the future."
              ))
            }
          }

          i_newYear <- monthly[itmp, "Year"][which.min(abs(monthly[itmp, "PPT_cm"] - precip_target))]
          newMonthData <- daily[[as.character(i_newYear)]]@data[m == im, "PPT_cm"]

          # Adjust data for this month
          these_days <- m == im
          # adjust number of days in case we got a leap year February issue
          newMonthData <- fix_PPTdata_length(
            data = newMonthData,
            targetLength = sum(these_days)
          )
          tmp <- add_delta_to_PPT(
            data = newMonthData,
            ind_events = newMonthData > 0,
            addDelta = precip_target - sum(newMonthData)
          )
          ppt[these_days] <- tmp[["data"]]
          PPT_to_remove <- PPT_to_remove + tmp[["PPT_to_remove"]]
        }
      }

      if (any(idelta_neg)) {
        # there are negative deltas and no precipitation among additive days: attempt to remove from all days otherwise return and attempt to remove from all years
        StillToSubtract <- sum(ydelta[idelta_neg]) # 'StillToSubtract' is negative

        # attempt to remove precipitation from all days
        tmp <- add_delta_to_PPT(
          data = ppt,
          ind_events = ievents,
          addDelta = StillToSubtract
        )
        ppt <- tmp[["data"]]
        PPT_to_remove <- PPT_to_remove + tmp[["PPT_to_remove"]]
      }
    }
  }

  list(data = ppt, PPT_to_remove = PPT_to_remove)
}


applyDelta_oneYear <- function(
  obs,
  delta_ts,
  ppt_fun,
  daily,
  monthly,
  ppt_type = NULL,
  dailyPPTceiling,
  sigmaN,
  do_checks
) {
  ppt_type <- match.arg(ppt_type, c(NA, "detailed", "simple"))

  month <- 1 + as.POSIXlt(rSW2utils::days_in_years(obs@year, obs@year))$mon
  ydeltas <- delta_ts[delta_ts[, "Year"] == obs@year, -(1:2)]
  add_days <- ppt_fun[month] == "+"
  mult_days <- !add_days
  PPT_to_remove <- 0

  tmax <- obs@data[, "Tmax_C"] + ydeltas[month, "Tmax_C"]
  if (do_checks) rSW2utils::test_sigmaNormal(data = tmax, sigmaN)

  tmin <- obs@data[, "Tmin_C"] + ydeltas[month, "Tmin_C"]
  if (do_checks) rSW2utils::test_sigmaNormal(data = tmin, sigmaN)

  if (isTRUE(ppt_type == "simple")) {
    ppt <- applyPPTdelta_simple(
      m = month,
      data = obs@data[, "PPT_cm"],
      ydelta = ydeltas[month, "PPT_cm"],
      add_days = add_days, mult_days = mult_days
    )
  } else if (isTRUE(ppt_type == "detailed")) {
    tmp <- applyPPTdelta_detailed(
      m = month,
      data = obs@data[, "PPT_cm"],
      ydelta = ydeltas[month, "PPT_cm"],
      add_days = add_days,
      mult_days = mult_days,
      daily = daily, monthly = monthly
    )
    ppt <- tmp[["data"]]
    PPT_to_remove <- tmp[["PPT_to_remove"]]

    if (dailyPPTceiling > 0) {
      ppt <- controlExtremePPTevents(
        data = ppt,
        do_checks = do_checks,
        dailyPPTceiling = dailyPPTceiling,
        sigmaN = sigmaN
      )
    }
  } else {
    stop(paste(
      "'applyDelta_oneYear': argument not recognized: ppt_type =",
      ppt_type
    ))
  }


  sw <- new(
    "swWeatherData",
    data = round(
      data.matrix(
        cbind(obs@data[, "DOY"], tmax, tmin, ppt),
        rownames.force = FALSE
      ),
      2
    ),
    year = obs@year
  )
  list(sw = sw, PPT_to_remove = PPT_to_remove)
}


applyDeltas2 <- function(
  daily,
  monthly,
  years,
  delta_ts,
  ppt_fun,
  ppt_type = NULL,
  dailyPPTceiling,
  sigmaN,
  dbW_digits = 4L,
  do_checks = FALSE
) {
  sw_list <- list()
  totalPPT_to_remove <- 0

  for (i in seq_along(daily)) {
    tmp <- applyDelta_oneYear(
      obs = daily[[i]],
      delta_ts = delta_ts,
      ppt_fun = ppt_fun,
      daily = daily,
      monthly = monthly,
      ppt_type = ppt_type,
      dailyPPTceiling = dailyPPTceiling,
      sigmaN = sigmaN,
      do_checks = do_checks
    )

    sw_list[[i]] <- tmp[["sw"]]
    totalPPT_to_remove <- totalPPT_to_remove + tmp[["PPT_to_remove"]]
  }

  if (totalPPT_to_remove < 0) {
    # Some years didn't have sufficient precipitation for the removal of the requested delta precipitation
    # Here, crude approach to remove this additional quantity by spreading it across all years

    daily2 <- rSOILWAT2::dbW_weatherData_to_dataframe(sw_list)
    totalPPT <- sum(daily2[, "PPT_cm"])

    if (totalPPT > abs(totalPPT_to_remove)) {
      daily2[, "PPT_cm"] <- daily2[, "PPT_cm"] * (1 - abs(totalPPT_to_remove) / totalPPT)
    } else {
      print(paste(
        "Total site precipitation should be reduced on average by a further",
        round((abs(totalPPT_to_remove) - totalPPT) / length(daily), 2), "cm / year"
      ))
      daily2[, "PPT_cm"] <- 0
    }

    sw_list <- rSOILWAT2::dbW_dataframe_to_weatherData(
      daily2,
      years,
      round = dbW_digits
    )
  }
  names(sw_list) <- years

  sw_list
}


#' \code{rbind} two objects, but make sure that there is no duplication in two
#' indicator columns.
#'
#' @param x1 A two dimensional numeric object; the first two columns (e.g.,
#'   years, months) are matched up against \code{x2}.
#' @param x2 Same as \code{x1}.
#'
#' @return The object that would result from \code{rbind(x1, x2)} but where
#'   any duplication, as determined by the first two columns, is resolved, i.e.,
#'   copy with \code{NAs} is ignored or arithmetic mean across \code{x1} and
#'   \code{x2}.
rbind_2cols_nonoverlapping <- function(x1, x2) {
  if (is.null(x1) || nrow(x1) == 0) {
    res <- x2
  } else if (is.null(x2) || nrow(x2) == 0) {
    res <- x1
  } else {
    # Check whether there is a year-month overlap
    ids1 <- apply(x1[, 1:2], 1, paste, collapse = "-")
    ids2 <- apply(x2[, 1:2], 1, paste, collapse = "-")

    idso1 <- ids1 %in% ids2
    if (any(idso1)) {
      # handle overlap
      idso2 <- ids2 %in% ids1

      # Check whether there is one uniquely non-NA set
      if (all(is.na(x1[idso1, -(1:2)]))) {
        res <- rbind(x1[!idso1, ], x2)
      } else if (all(is.na(x2[idso2, -(1:2)]))) {
        res <- rbind(x1, x2[!idso2, ])
      } else {
        # both sets have values for overlap: take mean
        tmp <- x1
        tmp[idso1, -(1:2)] <-
          (x1[idso1, -(1:2)] + x2[idso2, -(1:2)]) / 2
        res <- rbind(tmp, x2[!idso2, -(1:2)])
      }
    } else {
      # there is no overlap
      res <- rbind(x1, x2)
    }
  }

  res
}



#' Downscale and temporal disaggregation
#'
#' @section Details: Units are [degree Celsius] for temperature and [cm / day] or
#'  [cm / month], respectively, for precipitation
#'
#' @param obs.hist.daily A list. Each element corresponds to one year of
#'  \code{simstartyr:endyr} is an object of
#'  \code{\link[rSOILWAT2:swWeatherData-class]{rSOILWAT2::swWeatherData}}.
#' @param daily A list. Each element corresponds to one year of \code{simstartyr:endyr}
#'  is an object of class \code{\link[rSOILWAT2:swWeatherData-class]{rSOILWAT2::swWeatherData}}.
#' @param obs.hist.monthly A numeric matrix. Monthly time-series of observed weather
#'  calculated from \code{obs.hist.daily} for the years \code{simstartyr:endyr}.
#' @param monthly A numeric matrix. Monthly time-series of observed weather calculated
#'  from \code{daily} for the years \code{simstartyr:endyr}.
#' @param scen.hist.monthly A numeric matrix. Monthly time-series of scenario weather
#'  during the historic time period \code{DScur_startyr:DScur_endyr}
#' @param scen.fut.monthly A numeric matrix. Monthly time-series of scenario weather
#'  during the projected time period \code{DSfut_startyr:DSfut_endyr}
#' @param opt_DS A named list.
#' @param do_checks A logical value. If \code{TRUE} perform several sanity checks on the
#'  data.
#'
#' @name downscale
NULL


#' Time periods for downscaling functions
#' @inheritParams downscale
downscale.periods <- function(
  obs.hist.daily,
  obs.hist.monthly,
  scen.hist.monthly = NULL,
  scen.fut.monthly = NULL,
  years = NULL,
  DScur_startyear = NULL,
  DScur_endyear = NULL,
  DSfut_startyear = NULL,
  DSfut_endyear = NULL
) {

  # Time periods
  #  - historic observed period: simstartyr:endyr
  dyears <- sapply(obs.hist.daily, function(obs) obs@year)
  if (is.null(years)) years <- dyears
  startyear <- years[1]
  endyear <- years[length(years)]
  iuse_obs_hist_d <- dyears >= startyear & dyears <= endyear
  iuse_obs_hist_m <- obs.hist.monthly[, 1] >= startyear & obs.hist.monthly[, 1] <= endyear
  stopifnot(sum(iuse_obs_hist_m) == (endyear - startyear + 1) * 12)

  #  - historic training period: DScur_startyear:DScur_endyear
  if (!is.null(scen.hist.monthly)) {
    if (is.null(DScur_startyear)) {
      DScur_startyear <- scen.hist.monthly[1, 1]
    }
    if (is.null(DScur_endyear)) {
      DScur_endyear <- scen.hist.monthly[nrow(scen.hist.monthly), 1]
    }
    iuse_scen_hist_m <- scen.hist.monthly[, 1] >= DScur_startyear & scen.hist.monthly[, 1] <= DScur_endyear
    if (!(sum(iuse_scen_hist_m) == (DScur_endyear - DScur_startyear + 1) * 12)) {
      print(paste0(
        "downscale.periods: resulting record of 'scen.hist.monthly' covers only the years ",
        paste(range(scen.hist.monthly[iuse_scen_hist_m, 1]), collapse = "-"),
        " instead of the requested ", DScur_startyear, "-", DScur_endyear
      ))
    }
  } else {
    DScur_startyear <- DScur_endyear <- iuse_scen_hist_m <- NULL
  }

  #  - future training period: DSfut_startyear:DSfut_endyear
  if (!is.null(scen.fut.monthly)) {
    if (is.null(DSfut_startyear)) {
      DSfut_startyear <- scen.fut.monthly[1, 1]
    }
    if (is.null(DSfut_endyear)) {
      DSfut_endyear <- scen.fut.monthly[nrow(scen.fut.monthly), 1]
    }
    iuse_scen_fut_m <- scen.fut.monthly[, 1] >= DSfut_startyear & scen.fut.monthly[, 1] <= DSfut_endyear
    if (!(sum(iuse_scen_fut_m) == (DSfut_endyear - DSfut_startyear + 1) * 12)) {
      print(paste0(
        "downscale.periods: resulting record of 'scen.fut.monthly' covers only the years ",
        paste(range(scen.fut.monthly[iuse_scen_fut_m, 1]), collapse = "-"),
        " instead of the requested ", DSfut_startyear, "-", DSfut_endyear
      ))
    }
  } else {
    DSfut_startyear <- DSfut_endyear <- iuse_scen_fut_m <- NULL
  }


  # Return
  list(
    years = years,
    startyear = startyear,
    endyear = endyear,
    DScur_startyear = DScur_startyear,
    DScur_endyear = DScur_endyear,
    DSfut_startyear = DSfut_startyear,
    DSfut_endyear = DSfut_endyear,
    iuse_obs_hist_d = iuse_obs_hist_d,
    iuse_obs_hist_m = iuse_obs_hist_m,
    iuse_scen_hist_m = iuse_scen_hist_m,
    iuse_scen_fut_m = iuse_scen_fut_m
  )
}


#' Calculate Deltas, used for downscaling functionality
#' @inheritParams downscale
calcDeltas <- function(obs.hist.monthly, scen.fut.monthly, opt_DS) {

  # 1. Calculate mean monthly values in historic and future scenario values
  scen.fut.mean_tmax <- tapply(
    scen.fut.monthly[, "tmax"],
    scen.fut.monthly[, "month"],
    mean,
    na.rm = TRUE
  )
  scen.fut.mean_tmin <- tapply(
    scen.fut.monthly[, "tmin"],
    scen.fut.monthly[, "month"],
    mean,
    na.rm = TRUE
  )
  scen.fut.mean_ppt <- tapply(
    scen.fut.monthly[, "prcp"],
    scen.fut.monthly[, "month"],
    sum,
    na.rm = TRUE
  )

  obs.hist.mean_tmax <- tapply(
    obs.hist.monthly[, "Tmax_C"],
    obs.hist.monthly[, "Month"],
    mean,
    na.rm = TRUE
  )
  obs.hist.mean_tmin <- tapply(
    obs.hist.monthly[, "Tmin_C"],
    obs.hist.monthly[, "Month"],
    mean,
    na.rm = TRUE
  )
  obs.hist.mean_ppt <- tapply(
    obs.hist.monthly[, "PPT_cm"],
    obs.hist.monthly[, "Month"],
    sum,
    na.rm = TRUE
  )

  # 2. Calculate deltas between observed historic and future mean scenario values
  #  - Additive approach (Anandhi et al. 2011): tmp, close-to-zero PPT, small or very
  #     large PPT ratios
  #  - Multiplicative approach (Wang et al. 2014): PPT otherwise
  delta_ts <- matrix(
    nrow = nrow(obs.hist.monthly),
    ncol = 5,
    dimnames = list(
      NULL,
      c("Year", "Month", "Tmax_C", "Tmin_C", "PPT_cm")
    )
  )
  delta_ts[, 1:2] <- obs.hist.monthly[, 1:2]
  ppt_fun <- rep("*", 12)

  # Deltas of monthly means
  delta_ts[, "Tmax_C"] <- scen.fut.mean_tmax - obs.hist.mean_tmax
  delta_ts[, "Tmin_C"] <- scen.fut.mean_tmin - obs.hist.mean_tmin
  delta_ppts <- scen.fut.mean_ppt / obs.hist.mean_ppt
  tmp_add <-
    obs.hist.mean_ppt < SFSW2_glovars[["tol"]] |
      delta_ppts < 1 / (10 * opt_DS[["PPTratioCutoff"]]) |
      delta_ppts > opt_DS[["PPTratioCutoff"]]

  if (any(tmp_add)) {
    ppt_fun[tmp_add] <- "+"
    delta_ppts[tmp_add] <- scen.fut.mean_ppt[tmp_add] - obs.hist.mean_ppt[tmp_add]
  }

  delta_ts[, "PPT_cm"] <- delta_ppts

  list(delta_ts, ppt_fun)
}

#' Downscale with the 'direct approach'
#'
#' See 'direct' approach in Lenderink et al. (2007)
#'
#' @inheritParams downscale
#'
#' @references Lenderink, G., A. Buishand, and W. van Deursen. 2007. Estimates of future
#'  discharges of the river Rhine using two scenario methodologies: direct versus delta
#'  approach. Hydrology and Earth System Sciences 11:1145-1159.
#' @export
downscale.raw <- function(
  obs.hist.daily,
  obs.hist.monthly,
  scen.fut.monthly,
  itime,
  years = NULL,
  sim_time = NULL,
  opt_DS = list(ppt_type = "detailed", sigmaN = 6, PPTratioCutoff = 10),
  dailyPPTceiling,
  dbW_digits = 4L,
  do_checks = TRUE,
  ...
) {

  # Time periods
  tp <- downscale.periods(
    obs.hist.daily,
    obs.hist.monthly,
    scen.hist.monthly = NULL,
    scen.fut.monthly,
    years,
    sim_time[["DScur_startyr"]],
    sim_time[["DScur_endyr"]],
    sim_time[["future_yrs"]][itime, "DSfut_startyr"],
    sim_time[["future_yrs"]][itime, "DSfut_endyr"]
  )

  if (any(!tp$iuse_obs_hist_d)) {
    obs.hist.daily <- obs.hist.daily[tp$iuse_obs_hist_d]
  }
  if (any(!tp$iuse_obs_hist_m)) {
    obs.hist.monthly <- obs.hist.monthly[tp$iuse_obs_hist_m, ]
  }
  if (any(!tp$iuse_scen_fut_m)) {
    scen.fut.monthly <- scen.fut.monthly[tp$iuse_scen_fut_m, ]
  }
  # moved to calcDeltas
  # # 1. Calculate mean monthly values in historic and future scenario values
  # scen.fut.mean_tmax <- tapply(scen.fut.monthly[, "tmax"], INDEX = scen.fut.monthly[, "month"], mean, na.rm = TRUE)
  # scen.fut.mean_tmin <- tapply(scen.fut.monthly[, "tmin"], INDEX = scen.fut.monthly[, "month"], mean, na.rm = TRUE)
  # scen.fut.mean_ppt <- tapply(scen.fut.monthly[, "prcp"], INDEX = scen.fut.monthly[, "month"], sum, na.rm = TRUE)
  #
  # obs.hist.mean_tmax <- tapply(obs.hist.monthly[, "Tmax_C"], INDEX = obs.hist.monthly[, "Month"], mean, na.rm = TRUE)
  # obs.hist.mean_tmin <- tapply(obs.hist.monthly[, "Tmin_C"], INDEX = obs.hist.monthly[, "Month"], mean, na.rm = TRUE)
  # obs.hist.mean_ppt <- tapply(obs.hist.monthly[, "PPT_cm"], INDEX = obs.hist.monthly[, "Month"], sum, na.rm = TRUE)
  #
  # # 2. Calculate deltas between observed historic and future mean scenario values
  #     #  - Additive approach (Anandhi et al. 2011): tmp, close-to-zero PPT, small or very large PPT ratios
  #     #  - Multiplicative approach (Wang et al. 2014): PPT otherwise
  # delta_ts <- matrix(NA, ncol = 5, nrow = nrow(obs.hist.monthly), dimnames = list(NULL, c("Year", "Month", "Tmax_C", "Tmin_C", "PPT_cm")))
  # delta_ts[, 1:2] <- obs.hist.monthly[, 1:2]
  # ppt_fun <- rep("*", 12)
  #
  # # Deltas of monthly means
  # delta_ts[, "Tmax_C"] <- scen.fut.mean_tmax - obs.hist.mean_tmax
  # delta_ts[, "Tmin_C"] <- scen.fut.mean_tmin - obs.hist.mean_tmin
  # delta_ppts <- scen.fut.mean_ppt / obs.hist.mean_ppt
  # tmp_add <- obs.hist.mean_ppt < SFSW2_glovars[["tol"]] |
  #             delta_ppts < 1 / (10 * opt_DS[["PPTratioCutoff"]]) |
  #             delta_ppts > opt_DS[["PPTratioCutoff"]]
  # if (any(tmp_add)) {
  #   ppt_fun[tmp_add] <- "+"
  #   delta_ppts[tmp_add] <- scen.fut.mean_ppt[tmp_add] - obs.hist.mean_ppt[tmp_add]
  # }
  # delta_ts[, "PPT_cm"] <- delta_ppts
  delta_ts <- calcDeltas(obs.hist.monthly, scen.fut.monthly, opt_DS)
  ppt_fun <- delta_ts[[2]]
  delta_ts <- delta_ts[[1]]
  # 3. Apply deltas to historic daily weather
  applyDeltas2(
    daily = obs.hist.daily,
    monthly = obs.hist.monthly,
    years = tp$years,
    delta_ts = delta_ts,
    ppt_fun = ppt_fun,
    ppt_type = opt_DS[["ppt_type"]],
    dailyPPTceiling = dailyPPTceiling,
    sigmaN = opt_DS[["sigmaN"]],
    dbW_digits = dbW_digits,
    do_checks = do_checks
  )
}

#' Downscale with the 'delta approach'
#'
#' @inheritParams downscale
#'
#' @references Hay, L. E., R. L. Wilby, and G. H. Leavesley. 2000. A comparison of delta
#'  change and downscaled GCM scenarios for three mountainous basins in the United States.
#'  Journal of the American Water Resources Association 36:387-397.
#' @references Hamlet, A. F., E. P. Salathe, and P. Carrasco. 2010. Statistical
#'  downscaling techniques for global climate model simulations of temperature and
#'  precipitation with application to water resources planning studies. Chapter 4. Final
#'  Report for the Columbia Basin Climate Change Scenarios Project. Climate Impacts Group,
#'  Center for Science in the Earth System, Joint Institute for the Study of the
#'  Atmosphere and Ocean, University of Washington, Seattle, WA.
#' @export
downscale.delta <- function(
  obs.hist.daily,
  obs.hist.monthly,
  scen.hist.monthly,
  scen.fut.monthly,
  itime,
  years = NULL,
  sim_time = NULL,
  opt_DS = list(ppt_type = "detailed", sigmaN = 6, PPTratioCutoff = 10),
  dailyPPTceiling,
  dbW_digits = 4L,
  do_checks = TRUE,
  ...
) {
  # Time periods
  tp <- downscale.periods(
    obs.hist.daily,
    obs.hist.monthly,
    scen.hist.monthly,
    scen.fut.monthly,
    years,
    sim_time[["DScur_startyr"]],
    sim_time[["DScur_endyr"]],
    sim_time[["future_yrs"]][itime, "DSfut_startyr"],
    sim_time[["future_yrs"]][itime, "DSfut_endyr"]
  )

  if (any(!tp$iuse_obs_hist_d)) {
    obs.hist.daily <- obs.hist.daily[tp$iuse_obs_hist_d]
  }
  if (any(!tp$iuse_obs_hist_m)) {
    obs.hist.monthly <- obs.hist.monthly[tp$iuse_obs_hist_m, ]
  }
  if (any(!tp$iuse_scen_hist_m)) {
    scen.hist.monthly <- scen.hist.monthly[tp$iuse_scen_hist_m, ]
  }
  if (any(!tp$iuse_scen_fut_m)) {
    scen.fut.monthly <- scen.fut.monthly[tp$iuse_scen_fut_m, ]
  }

  # 1. Calculate mean monthly values in historic and future scenario values
  scen.fut.mean_tmax <- tapply(
    scen.fut.monthly[, "tmax"],
    INDEX = scen.fut.monthly[, "month"],
    mean,
    na.rm = TRUE
  )
  scen.fut.mean_tmin <- tapply(
    scen.fut.monthly[, "tmin"],
    INDEX = scen.fut.monthly[, "month"],
    mean,
    na.rm = TRUE
  )
  scen.fut.mean_ppt <- tapply(
    scen.fut.monthly[, "prcp"],
    INDEX = scen.fut.monthly[, "month"],
    sum,
    na.rm = TRUE
  )

  scen.hist.mean_tmax <- tapply(
    scen.hist.monthly[, "tmax"],
    INDEX = scen.hist.monthly[, "month"],
    mean,
    na.rm = TRUE
  )
  scen.hist.mean_tmin <- tapply(
    scen.hist.monthly[, "tmin"],
    INDEX = scen.hist.monthly[, "month"],
    mean,
    na.rm = TRUE
  )
  scen.hist.mean_ppt <- tapply(
    scen.hist.monthly[, "prcp"],
    INDEX = scen.hist.monthly[, "month"],
    sum,
    na.rm = TRUE
  )


  # 2. Calculate deltas between historic and future mean scenario values
  #  - Additive approach (Anandhi et al. 2011): tmp, close-to-zero PPT, small or very large PPT ratios
  #  - Multiplicative approach (Wang et al. 2014): PPT otherwise
  delta_ts <- matrix(
    ncol = 5,
    nrow = nrow(obs.hist.monthly),
    dimnames = list(NULL, c("Year", "Month", "Tmax_C", "Tmin_C", "PPT_cm"))
  )
  delta_ts[, 1:2] <- obs.hist.monthly[, 1:2]
  ppt_fun <- rep("*", 12)

  # Deltas of monthly means
  delta_ts[, "Tmax_C"] <- scen.fut.mean_tmax - scen.hist.mean_tmax
  delta_ts[, "Tmin_C"] <- scen.fut.mean_tmin - scen.hist.mean_tmin
  delta_ppts <- scen.fut.mean_ppt / scen.hist.mean_ppt
  tmp_add <-
    scen.hist.mean_ppt < SFSW2_glovars[["tol"]] |
      delta_ppts < 1 / (10 * opt_DS[["PPTratioCutoff"]]) |
      delta_ppts > opt_DS[["PPTratioCutoff"]]

  if (any(tmp_add)) {
    ppt_fun[tmp_add] <- "+"
    delta_ppts[tmp_add] <- scen.fut.mean_ppt[tmp_add] - scen.hist.mean_ppt[tmp_add]
  }
  delta_ts[, "PPT_cm"] <- delta_ppts


  # 3. Apply deltas to historic daily weather
  applyDeltas2(
    daily = obs.hist.daily,
    monthly = obs.hist.monthly,
    years = tp$years,
    delta_ts = delta_ts,
    ppt_fun = ppt_fun,
    ppt_type = opt_DS[["ppt_type"]],
    dailyPPTceiling = dailyPPTceiling,
    sigmaN = opt_DS[["sigmaN"]],
    dbW_digits = dbW_digits,
    do_checks = do_checks
  )
}

#' Downscale with the 'delta-hybrid approach' old version (prior to May 2016)
#'
#' Hybrid-delta downscaling developed by Hamlet et al. 2010 and Tohver et al. 2014.
#' Applied, e.g., by Dickerson-Lange et al. 2014
#'
#' @inheritParams downscale
#'
#' @references Hamlet, A. F., E. P. Salathe, and P. Carrasco. 2010. Statistical
#'  downscaling techniques for global climate model simulations of temperature and
#'  precipitation with application to water resources planning studies. Chapter 4. Final
#'  Report for the Columbia Basin Climate Change Scenarios Project. Climate Impacts Group,
#'  Center for Science in the Earth System, Joint Institute for the Study of the
#'  Atmosphere and Ocean, University of Washington, Seattle, WA.
#' @references Tohver, I.M., Hamlet, A.F. & Lee, S.-Y. (2014) Impacts of 21st-Century
#'  Climate Change on Hydrologic Extremes in the Pacific Northwest Region of North
#'  America. Journal of the American Water Resources Association, 50, 1461-1476.
#' @references Anandhi, A., A. Frei, D. C. Pierson, E. M. Schneiderman, M. S. Zion, D.
#'  Lounsbury, and A. H. Matonse. 2011. Examination of change factor methodologies for
#'  climate change impact assessment. Water Resources Research 47:W03501.
#' @references Dickerson-Lange, S. E., and R. Mitchell. 2014. Modeling the effects of
#'  climate change projections on streamflow in the Nooksack River basin, Northwest
#'  Washington. Hydrological Processes: \doi{10.1002/hyp.10012}.
#' @references Wang, L., and W. Chen. 2014. Equiratio cumulative distribution function
#'  matching as an improvement to the equidistant approach in bias correction of
#'  precipitation. Atmospheric Science Letters 15:1-6.
#'
#' @export
downscale.deltahybrid <- function(
  obs.hist.daily,
  obs.hist.monthly,
  scen.hist.monthly,
  scen.fut.monthly,
  itime,
  years = NULL,
  sim_time = NULL,
  opt_DS = list(sigmaN = 6, PPTratioCutoff = 10),
  do_checks = TRUE,
  ...
) {
  # Functions
  eCDF.Cunnane <- function(x) {
    na_N <- sum(is.na(x))
    x <- sort(x, na.last = NA)
    if (na_N > 0) {
      # if there are NAs in the data, add them in the middle assuming missing values represent median conditions
      i_center <- ceiling(length(x) / 2)
      x <- c(x[1:i_center], rep(NA, na_N), x[(i_center + 1):length(x)])
    }
    n <- length(x)
    q <- (1:n - 0.4) / (n + 0.2) # Cunnane (1978)
    f <- stats::splinefun(x = q, y = x, method = "monoH.FC", ties = mean) #' hyman' produces too extreme large values

    list(x = x, q = q, fun = f)
  }

  # Time periods
  tp <- downscale.periods(
    obs.hist.daily,
    obs.hist.monthly,
    scen.hist.monthly,
    scen.fut.monthly,
    years,
    sim_time[["DScur_startyr"]],
    sim_time[["DScur_endyr"]],
    sim_time[["future_yrs"]][itime, "DSfut_startyr"],
    sim_time[["future_yrs"]][itime, "DSfut_endyr"]
  )

  if (any(!tp$iuse_obs_hist_d)) {
    obs.hist.daily <- obs.hist.daily[tp$iuse_obs_hist_d]
  }
  if (any(!tp$iuse_obs_hist_m)) {
    obs.hist.monthly <- obs.hist.monthly[tp$iuse_obs_hist_m, ]
  }
  if (any(!tp$iuse_scen_hist_m)) {
    scen.hist.monthly <- scen.hist.monthly[tp$iuse_scen_hist_m, ]
  }
  if (any(!tp$iuse_scen_fut_m)) {
    scen.fut.monthly <- scen.fut.monthly[tp$iuse_scen_fut_m, ]
  }

  # Delta time series values
  delta_ts <- matrix(
    ncol = 5,
    nrow = nrow(obs.hist.monthly),
    dimnames = list(NULL, c("Year", "Month", "Tmax_C", "Tmin_C", "PPT_cm"))
  )
  delta_ts[, 1:2] <- obs.hist.monthly[, 1:2]
  ppt_fun <- rep("*", 12)
  for (iv in 1:3) {
    for (m in 1:12) {
      # 1. Calculate eCDF of historic weather and of scenario using Cunnane plotting position (Cunnane 1978) following Hamlet et al. 2010
      #    - interpolation and extrapolation by splines (Dickerson-Lange et al. 2014) instead of standard anomalies (Hamlet et al. 2010)
      obs.hist.x <- obs.hist.monthly[rep(1:12, times = nrow(obs.hist.monthly) / 12) == m, 2 + iv]
      scen.hist.x <- scen.hist.monthly[rep(1:12, times = nrow(scen.hist.monthly) / 12) == m, 2 + iv]
      scen.fut.x <- scen.fut.monthly[rep(1:12, times = nrow(scen.fut.monthly) / 12) == m, 2 + iv]

      # NA values are assumed to represent median conditions
      if (any(i_na <- is.na(obs.hist.x))) {
        obs.hist.x[i_na] <- stats::median(obs.hist.x, na.rm = TRUE)
      }
      if (any(i_na <- is.na(scen.hist.x))) {
        scen.hist.x[i_na] <- stats::median(scen.hist.x, na.rm = TRUE)
      }
      if (any(i_na <- is.na(scen.fut.x))) {
        scen.fut.x[i_na] <- stats::median(scen.fut.x, na.rm = TRUE)
      }

      # eCDFs
      obs.hist.ecdf <- eCDF.Cunnane(obs.hist.x)
      scen.hist.ecdf <- eCDF.Cunnane(scen.hist.x)
      scen.fut.ecdf <- eCDF.Cunnane(scen.fut.x)

      # 2. Adjust future scenario with quantile-based deltas from historic comparison for future scenario values with linear extrapolation
      #  - Additive approach (Anandhi et al. 2011): tmp, close-to-zero PPT, small or very large PPT ratios
      #  - Multiplicative approach (Wang et al. 2014): PPT otherwise
      scHistToFut <- scen.hist.ecdf$fun(scen.fut.ecdf$q, extrapol = "linear")
      scHistToFutRatio <- obs.hist.ecdf$fun(scen.fut.ecdf$q, extrapol = "linear") / scHistToFut

      if (any(
        iv <= 2,
        scHistToFut < 1 / (10 * opt_DS[["PPTratioCutoff"]]),
        scHistToFutRatio > opt_DS[["PPTratioCutoff"]],
        scHistToFutRatio < 1 / opt_DS[["PPTratioCutoff"]]
      )) {
        scen.fut.xadj <-
          scen.fut.x +
          obs.hist.ecdf$fun(
            scen.fut.ecdf$q,
            extrapol = "linear"
          ) - scHistToFut

        if (all(iv == 3, sum(tmp0 <- (scen.fut.xadj < 0)) > 0)) {
          scen.fut.xadj[tmp0] <- 0
        }
      } else {
        scen.fut.xadj <- scen.fut.x * scHistToFutRatio
      }

      stopifnot(is.finite(scen.fut.xadj))
      if (do_checks) {
        if (iv <= 2) {
          rSW2utils::test_sigmaNormal(data = scen.fut.xadj, opt_DS[["sigmaN"]])
        }
        if (iv == 3) {
          rSW2utils::test_sigmaGamma(data = scen.fut.xadj, opt_DS[["sigmaN"]])
        }
      }

      # 3. Calculate eCDF of future adjusted scenario
      scen.fut2.ecdf <- eCDF.Cunnane(scen.fut.xadj)

      # 5. Quantile map observed historic to adjusted future scenario
      #  - Additive approach (Anandhi et al. 2011): tmp, close-to-zero PPT, small or very large PPT ratios
      #  - Multiplicative approach (Wang et al. 2014): PPT otherwise
      scHistToHist <- obs.hist.ecdf$fun(obs.hist.ecdf$q, extrapol = "linear")
      scHistToFutRatio <- scen.fut2.ecdf$fun(
        obs.hist.ecdf$q,
        extrapol = "linear"
      ) / scHistToHist

      if (any(
        iv <= 2,
        scHistToHist < 1 / (10 * opt_DS[["PPTratioCutoff"]]),
        scHistToFutRatio > opt_DS[["PPTratioCutoff"]],
        scHistToFutRatio < 1 / opt_DS[["PPTratioCutoff"]]
      )) {
        mapFut <- scen.fut2.ecdf$fun(
          obs.hist.ecdf$q,
          extrapol = "linear"
        ) - scHistToHist

        if (iv == 3) {
          ppt_fun[m] <- "+"
        }
      } else {
        mapFut <- scHistToFutRatio
        stopifnot(all(!is.infinite(mapFut)), all(!is.nan(mapFut))) # if (sum(tmp <- is.nan(mapFut)) > 0) mapFut[tmp] <- 0
      }
      delta_ts[delta_ts[, "Month"] == m, 2 + iv] <- mapFut[rank(obs.hist.x, ties.method = "random")]
    }
  }

  # 6. Apply deltas to historic daily weather
  applyDeltas(
    obs.hist.daily,
    obs.hist.monthly,
    delta_ts,
    ppt_fun,
    opt_DS[["sigmaN"]],
    do_checks = do_checks
  )
}

#------------------------
#' Apply a quantile mapping
#'
#' Whereas the function \code{\link[qmap]{fitQmapQUANT}} estimates values of the empirical
#' cumulative distribution function of observed and modeled time series for regularly
#' spaced quantiles. \code{doQmapQUANT.default_drs} uses these estimates to perform
#' quantile mapping.
#'
#' @section Details: \itemize{
#'  \item \code{type} takes one of two possible values \itemize{
#'    \item \var{\dQuote{linear}}: linear interpolation using \code{\link[stats]{approx}}
#'    \item \var{\dQuote{tricub}}: monotonic tricubic spline interpolation using
#'      \code{\link[stats]{splinefun}}. Splines may result in abnormally high output
#'      values which appears to be due to at least two reasons: \enumerate{
#'        \item extrapolation errors
#'        \item huge oscillations in the spline-function which arise from non-monotone
#'          splines (\code{spline_method} is \var{\dQuote{fmm}} or \var{\dQuote{natural}})
#'          or which arise from numerical instabilities in the exact monotonicity if
#'          \code{spline_method} is \var{\dQuote{monoH.FC}}.
#'        }
#'  }
#'  \item \code{lin_extrapol} is the extrapolation for values of \code{x} that are outside
#'    \code{range(fobj[["par"]]$modq)}. This argument is only in effect if \code{type}
#'    is \var{\dQuote{linear}} and takes one of three possible values \itemize{
#'    \item \var{\dQuote{none}}: no linear extrapolation is performed, i.e., output of
#'      \code{\link[stats]{approx}} for type = 2 is return; that is 'values at the
#'      closest data extreme'.
#'    \item \var{\dQuote{Boe}}: constant extrapolation from Boe et al. 2007
#'    \item \var{\dQuote{Thermessl2012CC.QMv1b}}: same extrapolation as Boe et al. 2007,
#'      but not including three largest/smallest values, from Themessl et al. 2012.
#'  }
#'  \item \code{fix_spline} takes one of three values \itemize{
#'    \item \var{\dQuote{none}}: No correction to mapped values is applied.
#'    \item \var{\dQuote{fail}}: If mapped values fall outside the range suggested by
#'      \code{monthly_extremes}, then an error is generated.
#'    \item \var{\dQuote{attempt}}: The spline-based mapping is repeated up to ten times
#'      where the values of the quantile map \code{fobj} are jittered.
#'  }
#'  }
#'
#' @references Boe, J., L. Terray, F. Habets, and E. Martin. 2007.
#'  Statistical and dynamical downscaling of the Seine basin climate for
#'  hydro-meteorological studies. International Journal of Climatology 27:1643-1655.
#' @references Themessl, M. J., A. Gobiet, and G. Heinrich. 2011. Empirical-statistical
#'  downscaling and error correction of regional climate models and its impact on the
#'  climate change signal. Climatic Change 112:449-468.
#' @references Gudmundsson, L., J. B. Bremnes, J. E. Haugen, and T. Engen-Skaugen. 2012.
#'  Technical Note: Downscaling RCM precipitation to the station scale using statistical
#'  transformations - a comparison of methods. Hydrology and Earth System Sciences
#'  16:3383-3390.
#'
#' @seealso Based on code from \code{\link[qmap]{doQmapQUANT}} v1.0.4 (Gudmundsson et al.
#'  2012), but with additional methods and more granular control. See details.
#'
#' @param x A numeric vector. The values to map.
#' @param fobj An object of class \code{\link[qmap]{fitQmapQUANT}}.
#' @param type A character string. Type of interpolation between the fitted transformed
#'  values. See details.
#' @param lin_extrapol A character string. Type of extrapolation when interpolation is
#'  linear. See details.
#' @param spline_method A character string. Type of spline, passed to
#'  \code{\link[stats]{splinefun}} as \code{method} argument. The type
#'  \var{\dQuote{monoH.FC}} is the only appropriate method here because quantile mapping
#'  requires a monotone function if possible.
#' @param monthly_extremes A numeric vector of length two. The first element suggests a
#'  monthly minimum value and the second element a monthly maximum value for the mapped
#'  output.
#' @param fix_spline A character string. See details.
#' @param ... Additional arguments are ignored.
#'
#' @return A numeric vector of the length of \code{x}. Return values differ among repeated
#'  calls with identical input arguments if jitter correction (using random numbers) is
#'  applied, i.e., \code{type} is \var{\dQuote{spline}}, \code{fix_spline} is
#'  \var{\dQuote{attempt}} and there are values outside the range suggested by
#'  \code{monthly_extremes}.
#'
#' @name doQmapQUANT
doQmapQUANT.default_drs <- function(
  x,
  fobj,
  type = NULL,
  lin_extrapol = NULL,
  spline_method = NULL,
  monthly_extremes = NULL,
  fix_spline = NULL,
  ...
) {
  type <- match.arg(type, c(NA, "linear", "tricub"))
  lin_extrapol <- match.arg(
    lin_extrapol,
    c(NA, "Boe", "Thermessl2012CC.QMv1b", "none")
  )
  spline_method <- match.arg(spline_method, c(NA, "monoH.FC", "fmm", "natural"))
  fix_spline <- match.arg(fix_spline, c(NA, "fail", "none", "attempt"))

  wet <- if (!is.null(fobj$wet.day)) {
    x >= fobj$wet.day
  } else {
    rep(TRUE, length(x))
  }
  out <- rep(NA, length.out = length(x))

  if (stats::var(fobj[["par"]]$modq[, 1]) < SFSW2_glovars[["tol"]]) {
    # All values of 'fobj[["par"]]$modq[, 1]' are identical
    # ==> stats::approx() and stats::splinefun() [unless method = "fmm"] will fail
    # ==> use result from stats::splinefun(method = "fmm"), i.e., mean(y)
    message(
      "'doQmapQUANT.default_drs': interpolation is not possible because all ",
      "'modq' values are identical; will return mean of 'fitq' for each 'x'."
    )
    out[wet] <- mean(fobj[["par"]]$fitq[, 1])
  } else {
    if (identical(type, "linear")) {
      out[wet] <- stats::approx(
        x = fobj[["par"]]$modq[, 1],
        y = fobj[["par"]]$fitq[, 1],
        xout = x[wet],
        method = "linear",
        rule = 2,
        ties = mean
      )$y

      if (!identical(lin_extrapol, "none")) {
        qid <- switch(lin_extrapol, Boe = 0, Thermessl2012CC.QMv1b = 3)
        nq <- nrow(fobj[["par"]]$modq)
        largex <- x > fobj[["par"]]$modq[nq, 1] + SFSW2_glovars[["tol"]]
        if (any(largex)) {
          max.delta <- fobj[["par"]]$modq[nq - qid, 1] - fobj[["par"]]$fitq[nq - qid, 1]
          out[largex] <- x[largex] - max.delta
        }
        smallx <- x < fobj[["par"]]$modq[1, 1] - SFSW2_glovars[["tol"]]
        if (any(smallx)) {
          min.delta <- fobj[["par"]]$modq[1 + qid, 1] - fobj[["par"]]$fitq[1 + qid, 1]
          out[smallx] <- x[smallx] - min.delta
        }
      }
    } else if (identical(type, "tricub")) {
      sfun <- stats::splinefun(
        x = fobj[["par"]]$modq[, 1],
        y = fobj[["par"]]$fitq[, 1],
        method = spline_method,
        ties = mean
      )
      tmp <- sfun(x[wet])

      if (!is.null(monthly_extremes) && !identical(fix_spline, "none")) {
        # version previous to 20150705 didn't catch several bad cases
        icount <- 1
        while (
          (itmp <- sum((tmp < monthly_extremes[1]) | (tmp > monthly_extremes[2]))) > 0 &&
            icount < 10
        ) {
          if (fix_spline == "fail") {
            stop("Out-of-range splinefun values and 'fix_spline' set to fail")
          }
          sfun <- stats::splinefun(
            x = jitter(fobj[["par"]]$modq[, 1]),
            y = jitter(fobj[["par"]]$fitq[, 1]),
            method = spline_method,
            ties = mean
          )
          tmp <- sfun(x[wet])
          icount <- icount + 1
        }
        if (itmp > 0) {
          stop(
            "'doQmapQUANT.default_drs': ",
            "jitter failed to fix out-of-range splinefun values"
          )
        }
      }

      out[wet] <- tmp
    } else {
      stop(paste("'doQmapQUANT.default_drs': unkown type", shQuote(type)))
    }
  }

  out[!wet] <- 0
  if (!is.null(fobj$wet.day)) {
    out[out < 0] <- 0
  }

  out
}

#' @rdname doQmapQUANT
#'
#' @param type_map A character vector. The type of interpolation, extrapolation, and
#'  spline passed to \code{\link{doQmapQUANT.default_drs}}. Possible values include
#'  \var{\dQuote{linear_Boe}}, \var{\dQuote{linear_Thermessl2012CC.QMv1b}},
#'  \var{\dQuote{linear_none}}, \var{\dQuote{tricub_fmm}}, \var{\dQuote{tricub_monoH.FC}},
#'  \var{\dQuote{tricub_natural}}, and \var{\dQuote{normal_anomalies}}. See details.
#' @param monthly_obs_base A numeric vector. Base values used to calculate t-scores of
#'  \code{x} which are only used if \code{type_map} is \var{\dQuote{normal_anomalies}}.
#'
#' @section Details: \itemize{
#'  \item \code{type_map} with \var{\dQuote{normal_anomalies}} represents a 'linear
#'  interpolation with extrapolation following Boe et al. 2007 and a correction using
#'  standard anomalies (i.e. number of standard deviations from the mean) for values
#'  outside the observed quantile map that is based on Tohver et al. 2014 (Appendix A)}
#'
#' @references Tohver, I. M., A. F. Hamlet, and S.-Y. Lee. 2014. Impacts of 21st-Century
#'  Climate Change on Hydrologic Extremes in the Pacific Northwest Region of North
#'  America. Journal of the American Water Resources Association 50:1461-1476.
#'
#' @export
doQmapQUANT_drs <- function(
  x,
  fobj,
  type_map = NULL,
  monthly_obs_base = NULL,
  monthly_extremes = NULL,
  fix_spline = NULL,
  ...
) {
  fix_spline <- match.arg(fix_spline, c(NA, "fail", "none", "attempt"))
  type_map <- match.arg(
    type_map,
    c(
      "NA_NA",
      "linear_Boe",
      "linear_Thermessl2012CC.QMv1b",
      "linear_none",
      "tricub_fmm",
      "tricub_monoH.FC",
      "tricub_natural",
      "normal_anomalies"
    )
  )
  tmp <- strsplit(type_map, "_", fixed = TRUE)[[1]]
  type <- tmp[1]
  type_mod <- tmp[2]

  if (identical(type, "linear")) {
    out <- doQmapQUANT.default_drs(
      x,
      fobj,
      type = type,
      lin_extrapol = type_mod,
      ...
    )
  } else if (identical(type, "tricub")) {
    out <- doQmapQUANT.default_drs(
      x,
      fobj,
      type = type,
      spline_method = type_mod,
      monthly_extremes = monthly_extremes,
      fix_spline = fix_spline,
      ...
    )
  } else if (identical(type, "normal")) {
    out <- doQmapQUANT.default_drs(
      x,
      fobj,
      type = "linear",
      lin_extrapol = "Boe",
      ...
    )

    # -Inf, smallest observed value, largest observed value, Inf
    target_range <- c(
      -Inf,
      fobj[["par"]]$modq[1, 1] - SFSW2_glovars[["tol"]],
      max(fobj[["par"]]$modq[, 1]) + SFSW2_glovars[["tol"]],
      Inf
    )
    out_of_range <- !(findInterval(x, target_range) == 2)
    in_range <- !out_of_range

    if (length(monthly_obs_base) > 1 && sum(in_range) > 1) {
      # need at least two values to calculate variance
      if (any(out_of_range)) {
        tscore_x <- (x[out_of_range] - mean(monthly_obs_base)) / stats::sd(monthly_obs_base)
        out[out_of_range] <- mean(out[in_range]) + stats::sd(out[in_range]) * tscore_x
      }
    } else {
      message(
        "'doQmapQUANT_drs': type 'normal_anomalies' requires sufficient values ",
        "in 'monthly_obs_base' and at least two mapped values that are not out of ",
        "the target range"
      )
    }
  } else {
    stop(paste(
      "'doQmapQUANT_drs': unkown type", shQuote(type), shQuote(type_mod)
    ))
  }

  out
}


#------------------------

#' Downscale with the new version of the \var{sQuote{delta-hybrid approach}}
#' (post to May 2016)
#'
#' Hybrid-delta downscaling developed by Hamlet et al. 2010 and Tohver et al. 2014.
#' Applied, e.g., by Dickerson-Lange et al. 2014.
#' Quantile mapping performed by functions modified from Gudmundsson et al. 2012.
#'
#' @inheritParams downscale
#'
#' @references Hamlet, A. F., E. P. Salathe, and P. Carrasco. 2010. Statistical
#'  downscaling techniques for global climate model simulations of temperature and
#'  precipitation with application to water resources planning studies. Chapter 4. Final
#'  Report for the Columbia Basin Climate Change Scenarios Project. Climate Impacts Group,
#'  Center for Science in the Earth System, Joint Institute for the Study of the
#'  Atmosphere and Ocean, University of Washington, Seattle, WA.
#' @references Tohver, I.M., Hamlet, A.F. & Lee, S.-Y. (2014) Impacts of 21st-Century
#'  Climate Change on Hydrologic Extremes in the Pacific Northwest Region of North
#'  America. Journal of the American Water Resources Association, 50, 1461-1476.
#' @references Anandhi, A., A. Frei, D. C. Pierson, E. M. Schneiderman, M. S. Zion, D.
#'  Lounsbury, and A. H. Matonse. 2011. Examination of change factor methodologies for
#'  climate change impact assessment. Water Resources Research 47:W03501.
#' @references Dickerson-Lange, S. E., and R. Mitchell. 2014. Modeling the effects of
#'  climate change projections on streamflow in the Nooksack River basin, Northwest
#'  Washington. Hydrological Processes: \doi{10.1002/hyp.10012}.
#' @references Wang, L., and W. Chen. 2014. Equiratio cumulative distribution function
#'  matching as an improvement to the equidistant approach in bias correction of
#'  precipitation. Atmospheric Science Letters 15:1-6.
#' @references Gudmundsson, L., Bremnes, J.B., Haugen, J.E. & Engen-Skaugen, T. (2012).
#'  Technical Note: Downscaling RCM precipitation to the station scale using statistical
#'  transformations - a comparison of methods. Hydrology and Earth System Sciences, 16,
#'  3383-3390.
#'
#' @export
downscale.deltahybrid3mod <- function(
  obs.hist.daily,
  obs.hist.monthly,
  scen.hist.monthly,
  scen.fut.monthly,
  itime,
  years = NULL,
  sim_time = NULL,
  opt_DS = list(
    extrapol_type = "linear_Thermessl2012CC.QMv1b",
    ppt_type = "detailed",
    sigmaN = 6,
    PPTratioCutoff = 10,
    fix_spline = "attempt"
  ),
  dailyPPTceiling,
  monthly_extremes,
  dbW_digits = 4L,
  do_checks = TRUE,
  ...
) {
  stopifnot(requireNamespace("qmap"))
  qstep <- 0.01
  nboot <- 1

  # Time periods
  tp <- downscale.periods(
    obs.hist.daily,
    obs.hist.monthly,
    scen.hist.monthly,
    scen.fut.monthly,
    years,
    sim_time[["DScur_startyr"]],
    sim_time[["DScur_endyr"]],
    sim_time[["future_yrs"]][itime, "DSfut_startyr"],
    sim_time[["future_yrs"]][itime, "DSfut_endyr"]
  )

  if (any(!tp$iuse_obs_hist_d)) {
    obs.hist.daily <- obs.hist.daily[tp$iuse_obs_hist_d]
  }

  if (any(!tp$iuse_obs_hist_m)) {
    obs.hist.monthly <- obs.hist.monthly[tp$iuse_obs_hist_m, ]
  }

  if (any(!tp$iuse_scen_hist_m)) {
    scen.hist.monthly <- scen.hist.monthly[tp$iuse_scen_hist_m, ]
  }

  if (any(!tp$iuse_scen_fut_m)) {
    scen.fut.monthly <- scen.fut.monthly[tp$iuse_scen_fut_m, ]
  }

  # Data objects
  sbc.hist.monthly <- matrix(
    nrow = nrow(scen.hist.monthly),
    ncol = 5,
    dimnames = list(NULL, colnames(obs.hist.monthly))
  )
  sbc.hist.monthly[, 1:2] <- as.matrix(scen.hist.monthly[, 1:2])

  sbc.fut.monthly <- matrix(
    nrow = nrow(scen.fut.monthly),
    ncol = 5,
    dimnames = list(NULL, colnames(obs.hist.monthly))
  )
  sbc.fut.monthly[, 1:2] <- as.matrix(scen.fut.monthly[, 1:2])

  #  future simulation years = delta + simstartyr:endyr
  hd.fut.monthly <- delta_ts <- matrix(
    nrow = nrow(obs.hist.monthly),
    ncol = 5,
    dimnames = list(NULL, colnames(obs.hist.monthly))
  )
  hd.fut.monthly[, 1:2] <- delta_ts[, 1:2] <- as.matrix(obs.hist.monthly[, 1:2])
  hd.fut.monthly[, 1] <-
    hd.fut.monthly[, 1] +
    sim_time[["future_yrs"]][itime, "delta"]


  #------STEPS 1-4 based on the appendix of Tohver et al. 2014
  for (iv in 1:3) {
    # for each variable separately: Tmax, Tmin, PPT
    # NAs in scenario data: impute with median conditions
    # TODO(drs): implement a more sophisticated imputation scheme; this one biases variation downwards
    if (anyNA(scen.hist.monthly[, 2 + iv])) {
      id_nas <- is.na(scen.hist.monthly[, 2 + iv])
      scen.hist.monthly[id_nas, 2 + iv] <- stats::median(
        scen.hist.monthly[, 2 + iv],
        na.rm = TRUE
      )
    }

    if (anyNA(scen.fut.monthly[, 2 + iv])) {
      id_nas <- is.na(scen.fut.monthly[, 2 + iv])
      scen.fut.monthly[id_nas, 2 + iv] <- stats::median(
        scen.fut.monthly[, 2 + iv],
        na.rm = TRUE
      )
    }

    #---STEP 1: Statistical bias correction of GCM data
    # 1st part of this step is NOT carried out here because our GCM data is already BCSD downscaled: "first aggregating the gridded T and P observations to the GCM grid scale (at the time of this writing typically about 200km resolution)"

    # fit quantile map based on training data of same historic time period
    qm_fit <- qmap::fitQmapQUANT.default(
      obs = obs.hist.monthly[, 2 + iv],
      mod = scen.hist.monthly[, 2 + iv],
      qstep = qstep,
      nboot = nboot,
      wet.day = FALSE
    )

    # 2nd part: bias correcting historic data ("then using quantile mapping techniques to remove the systematic bias in the GCM simulations relative to the observed probability distributions")
    sbc.hist.monthly[, 2 + iv] <- doQmapQUANT_drs(
      x = scen.hist.monthly[, 2 + iv],
      fobj = qm_fit,
      type_map = opt_DS[["extrapol_type"]],
      montly_obs_base = obs.hist.monthly[, 2 + iv],
      monthly_extremes = monthly_extremes[[iv]],
      fix_spline = opt_DS[["fix_spline"]]
    )

    # 3rd part: bias correcting future data ("the same quantile map between simulations and observations is used to transform the future simulations from the GCM")
    sbc.fut.monthly[, 2 + iv] <- doQmapQUANT_drs(
      x = scen.fut.monthly[, 2 + iv],
      fobj = qm_fit,
      type_map = opt_DS[["extrapol_type"]],
      montly_obs_base = obs.hist.monthly[, 2 + iv],
      monthly_extremes = monthly_extremes[[iv]],
      fix_spline = opt_DS[["fix_spline"]]
    )


    #---STEP 2: Spatial downscaling
    #   - "the monthly T and P values at the GCM grid scale are interpolated to the fine scale grid"
    #   -> not done here because spatial aggregation (step 1, 1st part) not carried out

    for (im in 1:12) { # for each month separately
      #---STEP 3: Remapping the Historical Record to Interpolated GCM data
      id_sim_months <- obs.hist.monthly[, "Month"] == im # identical(obs.hist.monthly[, 2], hd.fut.monthly[, 2])

      qm_fitm <- qmap::fitQmapQUANT.default(
        obs = sbc.fut.monthly[sbc.fut.monthly[, 2] == im, 2 + iv],
        mod = obs.hist.monthly[id_sim_months, 2 + iv],
        qstep = qstep,
        nboot = nboot,
        wet.day = FALSE
      )

      hd.fut.monthly[id_sim_months, 2 + iv] <- doQmapQUANT_drs(
        x = obs.hist.monthly[id_sim_months, 2 + iv],
        fobj = qm_fitm, type_map = opt_DS[["extrapol_type"]],
        montly_obs_base = obs.hist.monthly[, 2 + iv],
        monthly_extremes = monthly_extremes[[iv]],
        fix_spline = opt_DS[["fix_spline"]]
      )
    }
  }

  #---STEP 4: Daily Time Step Disaggregation of Monthly Data
  delta_ts[, c("Tmax_C", "Tmin_C")] <-
    hd.fut.monthly[, c("Tmax_C", "Tmin_C")] -
    obs.hist.monthly[, c("Tmax_C", "Tmin_C")] # equation 8

  ppt_fun <- rep("*", 12)
  delta_ppts <- hd.fut.monthly[, "PPT_cm"] / obs.hist.monthly[, "PPT_cm"] # equation 7

  tmp_add <-
    is.infinite(delta_ppts) | is.nan(delta_ppts) |
      delta_ppts > opt_DS[["PPTratioCutoff"]] |
      delta_ppts < 1 / opt_DS[["PPTratioCutoff"]]

  if (any(tmp_add)) {
    ids_m <- unique(delta_ts[tmp_add, "Month"])
    ppt_fun[ids_m] <- "+"
    tmp_m <- delta_ts[, "Month"] %in% ids_m # all calendar month for which at least one instance qualifies for additive PPT
    delta_ppts[tmp_m] <-
      hd.fut.monthly[tmp_m, "PPT_cm"] - obs.hist.monthly[tmp_m, "PPT_cm"]
  }
  delta_ts[, "PPT_cm"] <- delta_ppts

  # Apply deltas to historic daily weather
  # Note: PPT differs from call to call to applyDeltas() because of controlExtremePPTevents (if dailyPPTceiling > 0)
  applyDeltas2(
    daily = obs.hist.daily,
    monthly = obs.hist.monthly,
    years = tp$years,
    delta_ts,
    ppt_fun,
    ppt_type = opt_DS[["ppt_type"]],
    dailyPPTceiling,
    sigmaN = opt_DS[["sigmaN"]],
    dbW_digits = dbW_digits,
    do_checks = do_checks
  )
}


downscale.wgen_package <- function(
  obs.hist.daily,
  obs.hist.monthly, scen.hist.monthly, scen.fut.monthly,
  itime, years = NULL, sim_time = NULL,
  opt_DS = list(
   extrapol_type = "linear_Thermessl2012CC.QMv1b",
   ppt_type = "detailed",
   sigmaN = 6,
   PPTratioCutoff = 10,
   fix_spline = "attempt"
  ),
  dailyPPTceiling, monthly_extremes,
  do_checks = TRUE,
  ...
) {
  dots <- list(...)

  if (isTRUE(dots[["verbose"]])) {
    print(paste(
      "downscale.wgen_package start(deltaFuture_yr =", sim_time[["future_yrs"]][itime, "delta"], "years",
      paste(years, collapse = "-"), "DScur_startyear", sim_time[["DScur_startyr"]], "DScur_endyear",
      sim_time[["DScur_endyr"]], "DSfut_startyear", sim_time[["future_yrs"]][itime, "DSfut_startyr"], "DSfut_endyear", sim_time[["future_yrs"]][itime, "DSfut_endyr"]
    ))
  }

  stopifnot(
    requireNamespace("zoo"),
    requireNamespace("weathergen"),
    requireNamespace("dplyr"),
    requireNamespace("lubridate")
  )

  # Time periods
  tp <- downscale.periods(
    obs.hist.daily,
    obs.hist.monthly,
    scen.hist.monthly = NULL,
    scen.fut.monthly,
    years,
    sim_time[["DScur_startyr"]],
    sim_time[["DScur_endyr"]],
    sim_time[["future_yrs"]][itime, "DSfut_startyr"],
    sim_time[["future_yrs"]][itime, "DSfut_endyr"]
  )

  if (any(!tp$iuse_obs_hist_d)) {
    obs.hist.daily <- obs.hist.daily[tp$iuse_obs_hist_d]
  }
  if (any(!tp$iuse_obs_hist_m)) {
    obs.hist.monthly <- obs.hist.monthly[tp$iuse_obs_hist_m, ]
  }
  if (any(!tp$iuse_scen_fut_m)) {
    scen.fut.monthly <- scen.fut.monthly[tp$iuse_scen_fut_m, ]
  }

  day_data <- rSOILWAT2::dbW_weatherData_to_dataframe(obs.hist.daily)

  dates <- as.Date(
    day_data[, "DOY"] - 1,
    origin = paste(day_data[, "Year"], "01", "01", sep = "-")
  )

  day_data <- data.frame(
    WYEAR = weathergen::wyear(dates),
    MONTH = format(dates, "%m"),
    DATE = dates,
    PRCP = day_data[, "PPT_cm"],
    tmp = (day_data[, "Tmin_C"] + day_data[, "Tmax_C"]) / 2,
    TMIN = day_data[, "Tmin_C"],
    TMAX = day_data[, "Tmax_C"],
    WIND = NA
  )

  # get water years, oct 1st to sep 30th... used if start_month should be 10
  # day_data <- day_data[min(which(as.numeric(format(day_data$DATE, "%d")) == 1 & as.numeric(format(day_data$DATE, "%m")) == 10)):max(which(as.numeric(format(day_data$DATE, "%d")) == 30 & as.numeric(format(day_data$DATE, "%m")) == 9)), ]
  start_month <- as.numeric(format(min(day_data$DATE), "%m"))

  # silence warning: `Undefined global functions or variables`
  DATE <- N <- PRCP <- TMAX <- TMIN <- tmp <- NULL

  climwyear <- dplyr::group_by(
    day_data,
    WYEAR = weathergen::wyear(DATE, start_month = start_month)
  ) %>%
    dplyr::summarise(
      N = dplyr::n(),
      PRCP = sum(PRCP),
      TMAX = mean(TMAX),
      TMIN = mean(TMIN),
      tmp = mean(tmp)
    )
  complete_years <- climwyear$WYEAR[which(climwyear$N >= 365)]

  wyear_list <- list(day_data$WYEAR)
  wyr_data <- data.frame(
    WYEAR = complete_years,
    PRCP = climwyear$PRCP[which(climwyear$N >= 365)],
    tmp = climwyear$tmp[which(climwyear$N >= 365)],
    TMIN = climwyear$TMIN[which(climwyear$N >= 365)],
    TMAX = climwyear$TMAX[which(climwyear$N >= 365)],
    WIND = NA
  )

  obs_dat <- list(day = day_data, wyr = wyr_data)
  zoo_day <- zoo::zoo(
    x = obs_dat[["day"]][, c("PRCP", "tmp", "TMIN", "TMAX", "WIND")],
    order.by = obs_dat[["day"]][["DATE"]]
  )
  start_yr <- as.integer(format(dates[1], "%Y")) ##
  end_yr <- as.integer(format(max(dates), "%Y"))

  dry_wet_threshold <- 0.3
  wet_extreme_threshold <- 0.8

  # can be one value or a vector of 12
  dry_spell_changes <- if (
    !is.null(dots[["add_params"]][["wgen_dry_spell_changes"]])
  ) {
    dots[["add_params"]][["wgen_dry_spell_changes"]]
  } else {
    1
  }

  wet_spell_changes <- if (
    !is.null(dots[["add_params"]][["wgen_wet_spell_changes"]])
  ) {
    dots[["add_params"]][["wgen_wet_spell_changes"]]
  } else {
    1
  }

  prcp_cv_changes <- if (
    !is.null(dots[["add_params"]][["wgen_prcp_cv_changes"]])
  ) {
    dots[["add_params"]][["wgen_prcp_cv_changes"]]
  } else {
    1
  }

  changes <- calcDeltas(obs.hist.monthly, scen.fut.monthly, opt_DS)[[1]]

  # replace with tapply()?
  prcp_mean_changes <- sapply(
    SFSW2_glovars[["st_mo"]],
    function(x) mean(changes[changes[, "Month"] == x, "PPT_cm"])
  )

  temp_mean_changes <- sapply(
    SFSW2_glovars[["st_mo"]],
    function(x) {
      mean(
        changes[changes[, "Month"] == x, "Tmax_C"] + changes[changes[, "Month"] == x, "Tmin_C"]
      )
    }
  ) / 2

  # set.seed(1) # for testing
  if (isTRUE(dots[["verbose"]])) {
    print(paste(
      "calling wgen_daily(zoo_day, n_year = ", end_yr - start_yr + 1,
      ", start_water_year = ", start_yr, ", start_month =", start_month, "dry_wet_threshold = ", dry_wet_threshold,
      "wet_extreme_threshold = ", wet_extreme_threshold, "dry_spell_changes = ", dry_spell_changes, "wet_spell_changes = ",
      wet_spell_changes, "prcp_mean_changes = ", prcp_mean_changes, "prcp_cv_changes = ", prcp_cv_changes, "temp_mean_changes = ", temp_mean_changes, ")"
    ))
  }

  # consider setting more parameters
  # weathergens knn_annual may be worth a check, when testing I got surprisingly many leapyears. But maybe just coincidence
  scen.fut.daily <- weathergen::wgen_daily(
    zoo_day,
    n_year = end_yr - start_yr + 1, # DScur_endyear - DScur_startyear,
    start_water_year = start_yr, # DScur_startyear,
    start_month = start_month,
    dry_wet_threshold = dry_wet_threshold,
    wet_extreme_quantile_threshold = wet_extreme_threshold,
    include_leap_days = TRUE,
    dry_spell_changes = dry_spell_changes,
    wet_spell_changes = wet_spell_changes,
    prcp_mean_changes = prcp_mean_changes,
    prcp_cv_changes = 1,
    temp_mean_changes = temp_mean_changes
  )

  scen.fut.daily <- data.frame(
    Year = format(scen.fut.daily$out$DATE, "%Y"),
    DOY = as.POSIXlt(scen.fut.daily$out$DATE, format = "%Y-%m-%d")$yday + 1,
    Tmax_C = scen.fut.daily$out$TMAX,
    Tmin_C = scen.fut.daily$out$TMIN,
    PPT_cm = scen.fut.daily$out$PRCP
  )

  # year start back to 1/1, probably only needed when setting start_month != 1
  # scen.fut.daily<- scen.fut.daily[min(which(scen.fut.daily$DOY == 1)):max(which(scen.fut.daily$DOY >= 365)), ]
  scen.fut.daily <- rSOILWAT2::dbW_dataframe_to_weatherData(
    scen.fut.daily,
    round = FALSE
  )
  scen.fut.daily
}



#------Monthly NEX extractions------
get_request_NEX <- function(
  service,
  request,
  i_tag,
  variable,
  scen,
  gcm,
  rip,
  lon,
  lat,
  startyear,
  endyear,
  dir_out_tmp
) {
  if (requireNamespace("RCurl")) {
    success <- try(RCurl::getURL(
      request,
      .opts = list(
        timeout = 5 * 60,
        connecttimeout = 60
      )
    ))

    if (!inherits(success, "try-error")) {
      if (isTRUE(grepl("Not Found", success, ignore.case = TRUE))) {
        class(success) <- "try-error"
      } else {
        if (service == "ncss") {
          ftmp <- textConnection(success)
        } else if (service == "opendap") {
          tmp <- strsplit(success, split = "\n\n", fixed = TRUE)
          ftmp <- textConnection(tmp[[1]][3])
          ttmp <-
            as.POSIXlt("1950-01-01", tz = "UTC") +
            86400 * as.numeric(scan(
              text = sub("\n", ", ", tmp[[1]][4], fixed = TRUE),
              what = "character",
              sep = ",",
              quiet = TRUE
            )[-1])
        }
        success <- 0
      }
    }
  } else {
    if (service == "opendap") {
      stop(
        "Curl must be present to access NEX-DCP30 data via thredds/dodsC (opendap)"
      )
    }
    ftmp <- file.path(
      dir_out_tmp,
      paste0(
        "NEX_", gcm, "_", scen, "_", rip, "_",
        variable, "_", round(lat, 5), "&", round(lon, 5), ".csv"
      )
    )
    success <- try(utils::download.file(
      url = request,
      destfile = ftmp,
      quiet = TRUE
    ))
  }

  yearsN <- endyear - startyear + 1
  dat <- rep(NA, times = 12 * yearsN)

  if (!inherits(success, "try-error") && success == 0) {
    if (service == "ncss") {
      tmp <- utils::read.csv(
        ftmp,
        colClasses = c("POSIXct", "NULL", "NULL", "numeric")
      ) # colnames = Time, Lat, Long, Variable
      vtmp <- tmp[, 2]
      ttmp <- as.POSIXlt(tmp[, 1], tz = "UTC")
    } else if (service == "opendap") {
      vtmp <- utils::read.csv(
        ftmp,
        colClasses = c("NULL", "numeric"),
        header = FALSE
      )[-1, ] # columns = Index, Variable
    }

    if (file.exists(ftmp)) {
      unlink(ftmp)
    }

    if (length(vtmp) < 12 * yearsN) {
      # some GCMs only have values up to Nov 2099
      tmpYearMonth <- paste(ttmp$year + 1900, ttmp$mo + 1, sep = "_")
      targetYearMonth <- paste(
        rep(startyear:endyear, each = 12),
        rep(1:12, times = yearsN),
        sep = "_"
      )
      iavail <- match(targetYearMonth, tmpYearMonth, nomatch = 0)
      dat[iavail > 0] <- vtmp[iavail]
    } else {
      dat <- vtmp
    }
  } else {
    stop(paste(
      i_tag, " extraction from NEX at", Sys.time(), "for", gcm, scen, rip, "at",
      lon, lat, ": not successful"
    ))
  }

  dat
}


extract_variable_NEX <- function(
  i_tag,
  variable,
  scen,
  gcm,
  rip,
  lon,
  lat,
  bbox,
  tbox,
  startyear,
  endyear,
  dir_out_tmp
) {
  gcmrun <- "r1i1p1"
  # 1st attempt: TRHEDDS ncss/netCDF subsetting service
  request <- paste0(
    paste(
      "http://dataserver.nccs.nasa.gov",
      "thredds/ncss/grid/bypass/NEX-DCP30/bcsd", scen, gcmrun,
      paste0(
        gcm, "_",
        variable, ".ncml"
      ),
      sep = "/"
    ),
    "?var=",
    paste0(gcm, "_", variable),
    "&latitude=", lat,
    "&longitude=", ifelse(lon > 180, lon - 360, lon),
    paste0(
      "&time_start=",
      startyear, "-01-01T00%3A00%3A00Z&time_end=", endyear,
      "-12-31T23%3A59%3A59Z&timeStride=1"
    ),
    "&accept=csv"
  )

  dat <- get_request_NEX(
    service = "ncss",
    request,
    i_tag,
    variable,
    scen,
    gcm,
    rip,
    lon,
    lat,
    startyear,
    endyear,
    dir_out_tmp
  )

  if (inherits(dat, "try-error") || any(dat > 1e5 | dat < -1e5, na.rm = TRUE)) {
    # thredds/ncss/ returns for some GCMs/RCPs/locations unrealistic large values,
    # e.g., 9.969210e+36 and sometimes 2.670153e+42 for pr, tasmin, and tasmax for the
    # month of May in every fifth year (2071, 2076, ...): bug report to NASA NCCS Support
    # Team on June 2, 2014 - confirmed on June 8, 2014 by Yingshuo Shen (issue = 48932)
    # 2nd attempt: TRHEDDS opendap/dodsC
    lat.index <- round((lat - bbox$lat[1]) / 0.0083333333, 0)
    lon.index <- round((lon - bbox$lon[1]) / 0.0083333333, 0)

    if (startyear < 2006 && scen == "historical") {
      index.time.start <- (startyear - tbox["start", "first"]) * 12
      index.time.end <- (endyear + 1 - tbox["start", "first"]) * 12 - 1
    } else {
      index.time.start <- (startyear - tbox["start", "second"]) * 12
      index.time.end <- (endyear + 1 - tbox["start", "second"]) * 12 - 1
    }

    request <- paste0(
      paste(
        "http://dataserver.nccs.nasa.gov",
        "thredds/dodsC/bypass/NEX-DCP30/bcsd", scen, gcmrun,
        paste0(
          gcm, "_", variable,
          ".ncml.ascii"
        ),
        sep = "/"
      ),
      "?lat[", lat.index, "], lon[", lon.index, "], ",
      gcm,
      "_",
      variable, "[", index.time.start, ":1:", index.time.end, "][", lat.index, "][",
      lon.index, "]"
    )

    dat <- get_request_NEX(
      service = "opendap",
      request,
      i_tag,
      variable,
      scen,
      gcm,
      rip,
      lon,
      lat,
      startyear,
      endyear,
      dir_out_tmp
    )

    stopifnot(
      !inherits(dat, "try-error"),
      all(dat < 1e5 & dat > -1e5, na.rm = TRUE)
    )
  }

  dat
}

#' Download downscaled \var{GCM} projections from \var{NEX}
#'
#' @return A list of one data.frame object with 5 columns and names of
#'   \var{\dQuote{year}}, \var{\dQuote{month}}, \var{\dQuote{tmax}}, \var{\dQuote{tmin}},
#'   and \var{\dQuote{prcp}}. Each row is one day.
#' Units are [degree Celsius] for temperature and [cm / day] and [cm / month],
#' respectively, for precipitation.
get_GCMdata_NEX <- function(
  i_tag,
  time,
  dpm,
  gcm,
  scen,
  rip,
  lon,
  lat,
  startyear,
  endyear,
  climDB_meta,
  ...
) {
  dots <- list(...) # dir_out_tmp

  n_var <- 3
  clim <- vector("list", length = n_var)
  names(clim) <- row.names(climDB_meta[["var_desc"]])[seq_len(n_var)]

  for (iv in seq_len(n_var)) {
    varname <- climDB_meta[["var_desc"]][iv, "varname"]

    # Extract data
    clim[[iv]] <- extract_variable_NEX(
      i_tag,
      variable = varname,
      scen = scen, gcm = gcm, rip = rip, lon = lon, lat = lat,
      bbox = climDB_meta[["bbox"]], tbox = climDB_meta[["tbox"]],
      startyear = startyear, endyear = endyear,
      dir_out_tmp = dots[["dir_out_temp"]]
    )

    # Adjust units
    if (varname == "pr") {
      clim[[iv]] <- rSW2utils::convert_precipitation(
        x = clim[[iv]],
        dpm = dpm,
        unit_from = climDB_meta[["var_desc"]][iv, "unit_real"],
        unit_to = "cm/month"
      )
    } else if (grepl("tas", varname, ignore.case = TRUE)) {
      clim[[iv]] <- rSW2utils::convert_temperature(
        x = clim[[iv]],
        unit_from = climDB_meta[["var_desc"]][iv, "unit_real"],
        unit_to = "C"
      )
    }
  }

  # Monthly weather time-series (var names as in 'var_names_fixed')
  list(data.frame(
    time,
    tmax = clim[["tmax"]],
    tmin = clim[["tmin"]],
    prcp = clim[["prcp"]]
  ))
}
#--- end NEX

#------netCDF helper functions------
get_SpatialIndices_netCDF <- function(filename, lon, lat) {
  stopifnot(requireNamespace("ncdf4"))

  if (inherits(filename, "ncdf4")) {
    nc <- filename
  } else {
    nc <- ncdf4::nc_open(
      filename = filename,
      write = FALSE,
      readunlim = TRUE,
      verbose = FALSE
    )
    on.exit(ncdf4::nc_close(nc))
  }

  # Get latitudes/longitudes from the netCDF files...;
  # they are the same for each CMIP x extent
  #  - these are used to get the correct indices in the whereNearest function
  tmp <- names(nc$dim)
  dim_lat <- grep(
    "(\\lat\\b)|(\\blatitude\\b)",
    tmp,
    value = TRUE,
    ignore.case = TRUE
  )
  dim_lon <- grep(
    "(\\lon\\b)|(\\blongitude\\b)",
    tmp,
    value = TRUE,
    ignore.case = TRUE
  )
  stopifnot(length(dim_lat) > 0, length(dim_lon) > 0)

  lats <- nc$dim[[dim_lat]]$vals
  lons <- nc$dim[[dim_lon]]$vals

  if (any(lons > 180)) {
    lons <- ifelse(lons > 180, lons - 360, lons)
  }

  # Calculate the spatial indices
  # TODO: make sure that CRS agree

  ncg <- list()
  ncg[["longitude"]] <- lons
  ncg[["latitude"]] <- lats
  ncg[["ix"]] <- sapply(
    lon,
    function(x) whereNearest(val = x, matrix = lons)
  )
  ncg[["iy"]] <- sapply(
    lat,
    function(x) whereNearest(val = x, matrix = lats)
  )

  ncg
}


get_time_unit <- function(tunit) {
  # http://cfconventions.org/cf-conventions/v1.6.0/cf-conventions.html#time-coordinate
  if (grepl("(day)|(\\bd\\b)", tunit, ignore.case = TRUE)) {
    1
  } else if (grepl("(hour)|(\\bh\\b)", tunit, ignore.case = TRUE)) {
    24
  } else if (grepl("(minute)|(\\bmin\\b)|(\\bmins\\b)", tunit, ignore.case = TRUE)) {
    1440
  } else if (grepl("(second)|(sec)|(\\bs\\b)", tunit, ignore.case = TRUE)) {
    86400
  } else {
    stop("time unit of netCDF not recognized")
  }
}


#' Read and interpret time dimension of a \var{netCDF} file with \acronym{CF} 1 or larger
#'
#' @param filename A character string, the name of a \var{netCDF} file; or,
#'   the result of \code{\link[ncdf4]{nc_open}}.
#' @param tres A character string. The temporal resolution (time step).
#'
#' @return A list with six elements:
#'  \describe{
#'    \item{calendar}{The calendar type, see
#'      \href{http://cfconventions.org/cf-conventions/v1.6.0/cf-conventions.html#calendar}{CF-conventions}.}
#'    \item{unit}{The units of the time dimension, see
#'      \href{http://cfconventions.org/cf-conventions/v1.6.0/cf-conventions.html#time-coordinate}{CF-conventions}.}
#'    \item{N}{The number of steps along the time dimension.}
#'    \item{base}{The start date of the time dimension.}
#'    \item{start}{A numeric vector representing the first date with named elements
#'      \var{\dQuote{year}} and \var{\dQuote{month}}.}
#'    \item{end}{A numeric vector representing the last date with named elements
#'      \var{\dQuote{year}} and \var{\dQuote{month}}.}
#'  }
#'
#' @export
read_time_netCDF <- function(filename, tres = c("monthly", "daily")) {
  tres <- match.arg(tres)
  stopifnot(requireNamespace("ncdf4"))

  if (inherits(filename, "ncdf4")) {
    nc <- filename
  } else {
    nc <- ncdf4::nc_open(
      filename = filename,
      write = FALSE,
      readunlim = TRUE,
      verbose = FALSE
    )
    ncdf4::nc_close(nc)
  }

  dim_time <- grep(
    "(\\btime\\b)|(\\bt\\b)",
    names(nc$dim),
    value = TRUE,
    ignore.case = TRUE
  )
  stopifnot(length(dim_time) > 0)

  utmp <- nc$dim[[dim_time]]$units
  tvals <- nc$dim[[dim_time]]$vals
  calendar <- tolower(nc$dim[[dim_time]]$calendar)

  N <- length(tvals)
  if (tres == "monthly") {
    tvals <- tvals[c(1, N)]
  }

  utmp <- strsplit(utmp, split = " ", fixed = TRUE)[[1]]
  tunit <- get_time_unit(utmp[1])

  if ("as" %in% utmp) {
    # for instance: "day as %Y%m%d.%f" used
    # by 'pr_Amon_EC-EARTH-DMI_1pctCO2_r1i1p1_185001-198912.nc'
    iformat <- grep("%Y", utmp, value = TRUE)[1]

    if (is.na(as.Date(as.character(tvals[1]), format = iformat))) {
      iformat <- sub(".%f", "", iformat)
    }

    time <- strptime(tvals, format = iformat, tz = "UTC")
    tbase <- time[[1]]
  } else if ("since" %in% utmp) {
    # for instance: "days since 1765-12-01 00:00:00" used
    # by 'pr_Amon_HadCM3_1pctCO2_r1i1p1_000101-010012.nc'
    tmp <- lapply(utmp, function(x) as.Date(x, format = "%Y-%m-%d"))
    tbase <- tmp[sapply(tmp, function(x) !is.na(x))][[1]]
    stopifnot(length(tbase) == 1)

    #--- http://cfconventions.org/cf-conventions/v1.6.0/cf-conventions.html#calendar
    # days per calendar year
    cdays <- switch(
      EXPR = calendar,
      noleap = 365,
      `365_day` = 365,
      `all_leap` = 366,
      `366_day` = 366,
      `360_day` = 360,
      julian = 365.25,
      gregorian = 365.2425,
      -1
    )

    if (
      identical(calendar, "proleptic_gregorian") ||
        identical(calendar, "gregorian") ||
        identical(calendar, "standard") ||
        identical(calendar, "julian") ||
        is.null(calendar)
    ) {

      # TODO: this doesn't seem to work perfectly well for Julian calendars,
      # but should be ok-ish for a few hundred years around 'origin = tbase'
      tmp <- if (identical(calendar, "julian")) {
        365.2425 / 365.25 # gregorian / julian days per year
      } else {
        1
      }
      day_scaler <- 86400 * tmp / tunit

      time <- as.POSIXlt(tvals * day_scaler, origin = tbase, tz = "UTC")
    } else if (cdays > 0) {
      if (identical(tres, "daily")) {
        # TODO
        stop(
          "Calendars with fixed duration of years are not yet implemented ",
          "for daily netCDF files."
        )
      }

      # all years are of a constant fixed duration
      tbase_utc <- as.POSIXlt(tbase, tz = "UTC")
      tmp <- tvals / tunit
      to_add_years <- tmp %/% cdays
      to_add_days <- tmp %% cdays # base0

      # Convert to base1
      iday_less_base <- to_add_days == 0
      if (any(iday_less_base)) {
        to_add_years[iday_less_base] <- to_add_years[iday_less_base] - 1L
        to_add_days[iday_less_base] <- cdays - 1L
      }

      if (cdays > 360) {
        # calendar is one of 'noleap', '365_day', 'all_leap', and '366_day'
        # format '%j' is base1: Day of year as decimal number (001-366)
        time <- strptime(
          paste(
            tbase_utc$year + 1900 + to_add_years,
            to_add_days,
            sep = "-"
          ),
          format = "%Y-%j",
          tz = "UTC"
        )
      } else if (cdays == 360) {
        # all years are 360 days divided into 30-day months
        to_add_months <- floor(to_add_days / 30)

        # POSIXlt element 'mon' is base0: 0-11 months after the first of year
        tmp_yr <- tbase_utc$year + 1900 + to_add_years
        tmp_mon <- tbase_utc$mon + 1 + to_add_months
        mons_next_yr <- tmp_mon - 12
        imon_next_yr <- mons_next_yr > 0
        if (any(imon_next_yr)) {
          tmp_yr[imon_next_yr] <- tmp_yr[imon_next_yr] + 1
          tmp_mon[imon_next_yr] <- mons_next_yr[imon_next_yr]
        }

        time <- lapply(
          seq_along(tvals),
          function(k) c(year = tmp_yr[k], month = tmp_mon[k])
        )
      }
    } else {
      stop("calendar of netCDF not recognized")
    }
  } else {
    stop("time unit of netCDF not recognized")
  }

  time12 <- lapply(
    time,
    function(x) {
      if (inherits(x, "POSIXt")) {
        c(year = x$year + 1900, month = x$mon + 1)
      } else {
        x
      }
    }
  )

  list(
    calendar = calendar,
    unit = tunit,
    N = N,
    time = time, # POSIXlt
    base = tbase,
    start = time12[[1]],
    end = time12[[2]]
  )
}


get_TimeIndices_netCDF <- function(
  filename,
  startyear,
  endyear,
  tres = c("monthly", "daily")
) {
  tres <- match.arg(tres)

  nct <- read_time_netCDF(filename, tres = tres)

  # we only extract full years and require data from the start["year"] on
  stopifnot(
    nct[["start"]]["year"] <= startyear ||
      (nct[["start"]]["month"] == 1 && nct[["start"]]["year"] == startyear)
  )

  # we extract beginning with January (1) of start["year"]
  timeStartIndex <- if (identical(tres, "monthly")) {
    tmp <- startyear - nct[["start"]]["year"]
    tmp * 12 + 2 - nct[["start"]]["month"]
  } else if (identical(tres, "daily")) {
    tmp <- as.POSIXlt(ISOdate(startyear, 1, 1, tz = "UTC"))
    which.min(abs(nct[["time"]] - tmp))
  }

  # account for missing months: assume all are at the end;
  # e.g., precipitation of 'HadGEM2-ES' has values only until
  # Nov 2099 instead Dec 2100

  # timeCount must include a count at timeStartIndex;
  # e.g., to extract two values at 1:2, use timeStartIndex = 1 and timeCount = 2
  timeCount_should <- if (identical(tres, "monthly")) {
    (endyear - startyear + 1) * 12
  } else if (identical(tres, "daily")) {
    tmp <- as.POSIXlt(ISOdate(endyear, 12, 31, tz = "UTC"))
    which.min(abs(nct[["time"]] - tmp)) - timeStartIndex + 1
  }

  N_should <- timeStartIndex + timeCount_should - 1

  if (nct[["N"]] >= N_should) {
    timeCount <- timeCount_should
    addMissingTimeAtEnd <- 0
  } else {
    timeCount <- nct[["N"]] - timeStartIndex + 1
    addMissingTimeAtEnd <- N_should - nct[["N"]]
  }

  list(
    timeStartIndex = timeStartIndex,
    timeCount = timeCount,
    time = nct[["time"]],
    addMissingTimeAtEnd = addMissingTimeAtEnd
  )
}


#------Monthly netCDF extractions------

do_ncvar_netCDF <- function(nc, nc_perm, variable, ncg, nct) {
  stopifnot(requireNamespace("ncdf4"))

  index <- grep("(\\btime\\b)|(\\bt\\b)", nc_perm, ignore.case = TRUE)

  if (index == 3L) {
    # if file is in order of (lat, lon, time)
    ncdf4::ncvar_get(
      nc,
      varid = variable,
      start = c(ncg$ix, ncg$iy, nct$timeStartIndex),
      count = c(1, 1, nct$timeCount)
    )
  } else if (index == 1L) {
    # if file is optimized for time series extraction and permutated to order
    # (time, lat, lon)
    ncdf4::ncvar_get(
      nc,
      varid = variable,
      start = c(nct$timeStartIndex, ncg$ix, ncg$iy),
      count = c(nct$timeCount, 1, 1)
    )
  } else {
    stop(
      "do_ncvar_netCDF: dimension 'time' must be either in ",
      "first or third place, but is instead at ", index
    )
  }
}


extract_monthly_variable_netCDF <- function(
  filename,
  variable,
  unit,
  ncg,
  nct,
  lon = NA,
  lat = NA,
  startyear = NA,
  endyear = NA
) {
  stopifnot(requireNamespace("ncdf4"))
  # the 'raster' package (version <= '2.5.2') cannot handle non-equally
  # spaced cells
  nc <- ncdf4::nc_open(
    filename = filename,
    write = FALSE,
    readunlim = TRUE,
    verbose = FALSE
  )
  on.exit(ncdf4::nc_close(nc))

  nc_var <- grep(
    paste0("\\b", variable, "\\b"),
    names(nc$var),
    value = TRUE,
    ignore.case = TRUE
  )
  stopifnot(length(nc_var) > 0)
  stopifnot(isTRUE(tolower(unit) == tolower(nc$var[[nc_var]]$units)))

  # getting the values from the netCDF files...
  nc_perm <- sapply(nc$var[[nc_var]]$dim, function(x) x$name)
  res <- try(do_ncvar_netCDF(nc, nc_perm, nc_var, ncg, nct))

  if (inherits(res, "try-error")) {
    stopifnot(
      is.finite(lon), is.finite(lat),
      is.finite(startyear), is.finite(endyear)
    )
    # in case of 'HadGEM2-ES x RCP45' where pr and tasmax/tasmin have
    # different timings
    ncg <- get_SpatialIndices_netCDF(filename = nc, lon, lat)
    nct <- get_TimeIndices_netCDF(
      filename = nc,
      startyear = startyear,
      endyear = endyear,
      tres = "monthly"
    )
    res <- do_ncvar_netCDF(nc, nc_perm, nc_var, ncg, nct)
  }

  # adjust for missing time
  if (nct$addMissingTimeAtEnd > 0) {
    res <- c(res, rep(NA, times = nct$addMissingTimeAtEnd))
  }

  if (all(is.na(res)) || inherits(res, "try-error")) {
    stop(
      "'extract_monthly_variable_netCDF' at (",
      round(lon, 5), ", ", round(lat, 5),
      "): extraction failed or no data available. Error message: ",
      paste(res[1:6], collapse = "/")
    )
  }

  res
}

#' Extract monthly \var{GCM} projection from a \var{netCDF} file
#'
#' @return A list of one data.frame object with 5 columns and names of
#'   \var{\dQuote{year}}, \var{\dQuote{month}},
#'   \var{\dQuote{tmax}}, \var{\dQuote{tmin}}, and \var{\dQuote{prcp}}.
#' Each row represents one month.
#' Units are [degree Celsius] for temperature and [cm / month]
#' for precipitation.
get_MonthlyGCMdata_netCDF <- function(
  i_tag,
  time,
  dpm,
  gcm,
  scen,
  rip,
  lon,
  lat,
  startyear,
  endyear,
  climDB_meta,
  ncg,
  nct,
  ncFiles
) {
  ctmp <- paste(c(i_tag, gcm, scen, rip), collapse = " * ")

  #--- Extract precipitation data
  ftmp1 <- grep(
    climDB_meta[["var_desc"]]["prcp", "fileVarTags"],
    ncFiles,
    ignore.case = TRUE,
    value = TRUE
  )

  if (length(ftmp1) == 1) {
    prcp <- extract_monthly_variable_netCDF(
      filename = ftmp1,
      variable = climDB_meta[["var_desc"]]["prcp", "varname"],
      unit = climDB_meta[["var_desc"]]["prcp", "unit_given"],
      ncg = ncg,
      nct = nct,
      lon = lon,
      lat = lat,
      startyear = startyear,
      endyear = endyear
    )
  } else {
    if (length(ftmp1) > 1) {
      stop(
        "More than one netCDF file with precipitation data ",
        "available for combination ", ctmp,
        " with files = ", paste(shQuote(basename(ftmp1)), collapse = "/")
      )
    } else {
      stop(
        "No suitable netCDF file with precipitation data ",
        "available for combination ", ctmp
      )
    }
  }

  #--- Extract temperature data
  ftmp3 <- grep(climDB_meta[["var_desc"]]["tmin", "fileVarTags"], ncFiles,
    ignore.case = TRUE,
    value = TRUE
  )
  ftmp4 <- grep(climDB_meta[["var_desc"]]["tmax", "fileVarTags"], ncFiles,
    ignore.case = TRUE,
    value = TRUE
  )

  if (length(ftmp3) > 0 && length(ftmp4) > 0) {
    if (length(ftmp3) == 1 && length(ftmp4) == 1) {
      tmin <- extract_monthly_variable_netCDF(
        filename = ftmp3,
        variable = climDB_meta[["var_desc"]]["tmin", "varname"],
        unit = climDB_meta[["var_desc"]]["tmin", "unit_given"],
        ncg = ncg,
        nct = nct,
        lon = lon,
        lat = lat,
        startyear = startyear,
        endyear = endyear
      )

      tmax <- extract_monthly_variable_netCDF(
        filename = ftmp4,
        variable = climDB_meta[["var_desc"]]["tmax", "varname"],
        unit = climDB_meta[["var_desc"]]["tmax", "unit_given"],
        ncg = ncg,
        nct = nct,
        lon = lon,
        lat = lat,
        startyear = startyear,
        endyear = endyear
      )
    } else {
      stop(
        "More than one netCDF file with tmin/tmax data ",
        "available for combination ", ctmp,
        " with files = ", paste(shQuote(basename(ftmp3)), collapse = "/"),
        " or ",
        paste(shQuote(basename(ftmp4)), collapse = "/")
      )
    }
  } else {
    ftmp2 <- grep(climDB_meta[["var_desc"]]["tmean", "fileVarTags"], ncFiles,
      ignore.case = TRUE,
      value = TRUE
    )

    if (length(ftmp2) == 1) {
      tmean <- extract_monthly_variable_netCDF(
        filename = ftmp2,
        variable = climDB_meta[["var_desc"]]["tmean", "varname"],
        unit = climDB_meta[["var_desc"]]["tmean", "unit_given"],
        ncg = ncg,
        nct = nct,
        lon = lon,
        lat = lat,
        startyear = startyear,
        endyear = endyear
      )
      tmin <- tmax <- tmean

      vars <- c("tmin", "tmax", "tmean")
      unit_from <- climDB_meta[["var_desc"]][vars, "unit_real"]
      stopifnot(unit_from[1] == unit_from[2], unit_from[1] == unit_from[3])
    } else {
      if (length(ftmp2) > 1) {
        stop(
          "More than one netCDF file with tmean data ",
          "available for combination ", ctmp,
          " with files = ", paste(shQuote(basename(ftmp2)), collapse = "/")
        )
      } else {
        stop(
          "No suitable netCDF file with tmean data ",
          "available for combination ", ctmp
        )
      }
    }
  }

  #--- Convert units
  prcp <- rSW2utils::convert_precipitation(
    x = prcp,
    dpm = dpm,
    unit_from = climDB_meta[["var_desc"]]["prcp", "unit_real"],
    unit_to = "cm/month"
  )

  tmin <- rSW2utils::convert_temperature(
    x = tmin,
    unit_from = climDB_meta[["var_desc"]]["tmin", "unit_real"],
    unit_to = "C"
  )

  tmax <- rSW2utils::convert_temperature(
    x = tmax,
    unit_from = climDB_meta[["var_desc"]]["tmax", "unit_real"],
    unit_to = "C"
  )

  list(data.frame(
    time,
    tmax = tmax,
    tmin = tmin,
    prcp = prcp
  ))
}


#' Extract monthly climate scenario data and downscale to daily weather data
#' @seealso \code{\link{calc_DailyScenarioWeather}}
#'
calc_MonthlyScenarioWeather <- function(
  i,
  ig,
  il,
  gcm,
  site_id,
  i_tag,
  clim_source,
  use_CF,
  use_NEX,
  climDB_meta,
  climDB_files,
  reqGCMs,
  reqRCPsPerGCM,
  reqDownscalingsPerGCM,
  climate.ambient,
  locations,
  compression_type,
  dbW_digits,
  getYears,
  assocYears,
  sim_time,
  task_seed,
  opt_DS,
  project_paths,
  dir_failed,
  verbose,
  print.debug
) {

  on.exit({
    save(
      list = ls(),
      file = file.path(
        dir_failed,
        paste0("ClimScen_failed_", i_tag, "_l2.RData")
      )
    )
  })

  # Set RNG seed for random number use by functions
  #   - fix_PPTdata_length
  #   - calc_Days_withLoweredPPT
  #   - controlExtremePPTevents
  set_RNG_stream(seed = task_seed)

  scen_historical <- "historical"
  lon <- locations[il, "X_WGS84"]
  lat <- locations[il, "Y_WGS84"]
  Site_id_by_dbW <- locations[il, "Site_id_by_dbW"]

  if (verbose) {
    print(paste0(
      i_tag, " extraction: ", shQuote(clim_source), " at ", Sys.time(),
      " for ", gcm, " (", paste(reqRCPsPerGCM[[ig]], collapse = ", "), ") at ",
      lon, " / ", lat
    ))
  }

  #--- Output container for downscaled scenario weather data
  tmp1 <- expand.grid(
    downscaling = reqDownscalingsPerGCM[[ig]],
    futures = rownames(sim_time[["future_yrs"]]),
    rcps = reqRCPsPerGCM[[ig]],
    stringsAsFactors = FALSE
  )[, 3:1]
  n <- dim(tmp1)[1]
  tmp1[, "tag"] <- paste0(tmp1[, "futures"], ".", tmp1[, "rcps"])
  tmp1[, "Scenario"] <- paste(
    tmp1[, "downscaling"],
    tmp1[, "tag"],
    gcm,
    sep = "."
  )
  tmp1[, "Scenario_id"] <- rSOILWAT2::dbW_getScenarioId(
    tmp1[, "Scenario"],
    ignore.case = TRUE
  )

  if (anyNA(tmp1[, "Scenario_id"])) {
    stop(
      "Not all requested scenarios available ",
      "in the weather database scenario table:\n",
      paste(shQuote(tmp1[tmp1[, "Scenario_id"], "Scenario"]), collapse = ", ")
    )
  }

  if (anyNA(Site_id_by_dbW)) {
    stop(
      "Not all requested sites matched up with entries in the weather ",
      "database scenario table:\n",
      paste("*", shQuote(locations[il, ]), collapse = "\n")
    )
  } else {
    tmp1[, "Site_id_by_dbW"] <- rep(Site_id_by_dbW, n)
  }

  tmp <- rep(NA, n)
  tmp <- list(
    todo = rep(TRUE, n),
    StartYear = tmp,
    EndYear = tmp,
    weatherData = tmp
  )
  df_wdataOut <- c(tmp, as.list(tmp1))

  #--- Determine if any are already downscaled and stored in weather database
  df_wdataOut[["todo"]] <- !rSOILWAT2::dbW_has_weatherData(
    Site_ids = Site_id_by_dbW,
    Scenario_ids = df_wdataOut[["Scenario_id"]]
  )[1, ]
  ids_down <- which(df_wdataOut[["todo"]])

  if (length(ids_down) > 0) {
    # restrict to actually still required scenarios
    rcps <- unique(df_wdataOut[["rcps"]][ids_down])
    all_scens <- c(scen_historical, rcps)
    n_scens <- length(all_scens)

    if (use_NEX) {
      rip <- "r1i1p1"
    } else if (use_CF) {
      #--- Select netCDF files for this 'gcm', scenarios, and variables
      tmp <- select_suitable_CFs(
        climDB_files = climDB_files,
        climDB_meta = climDB_meta,
        getYears = getYears,
        model_name = gcm,
        scenario_names = all_scens
      )

      stopifnot(!tmp[["is_sequential"]])
      fnc_gcmXscens <- tmp[["files"]]
      rip <- tmp[["rip"]]
    }

    #---Scenario monthly weather time-series:
    # Get GCM data for each scenario and time slice
    tmp <- vector("list", (getYears$n_first + getYears$n_second) * n_scens)
    scen.monthly <- matrix(
      tmp,
      ncol = getYears$n_first + getYears$n_second,
      dimnames = list(
        all_scens,
        c(
          paste0("first", seq_len(getYears$n_first)),
          paste0("second", seq_len(getYears$n_second))
        )
      )
    )

    if (print.debug) {
      print(paste0(
        i_tag, " extraction: first slice ('historical'): ",
        paste(getYears$first, collapse = "-")
      ))
    }

    args_extract1 <- list(
      i_tag = i_tag,
      gcm = gcm,
      scen = scen_historical,
      rip = rip,
      lon = lon,
      lat = lat,
      climDB_meta = climDB_meta
    )

    if (use_CF) {
      tag <- paste0(
        climDB_meta[["sep_fname"]],
        args_extract1[["scen"]],
        climDB_meta[["sep_fname"]]
      )
      fnc_gcmXscen <- grep(tag, fnc_gcmXscens, ignore.case = TRUE, value = TRUE)

      ncg <- get_SpatialIndices_netCDF(
        filename = fnc_gcmXscen[1],
        lon = lon,
        lat = lat
      )

      args_extract1 <- c(
        args_extract1,
        ncFiles = list(fnc_gcmXscen),
        ncg = list(ncg)
      )
    }

    if (use_NEX) {
      args_extract1 <- c(
        args_extract1,
        dir_out_tmp = project_paths[["dir_out_temp"]]
      )
    }

    for (it in seq_len(getYears$n_first)) {
      args_first <- c(
        args_extract1,
        time = list(data.frame(
          year = getYears$first_dates[[it]]$year + 1900,
          month = getYears$first_dates[[it]]$mon + 1
        )),
        dpm = list(getYears$first_dpm[[it]]),
        startyear = getYears$first[it, 1],
        endyear = getYears$first[it, 2]
      )

      if (use_CF) {
        # Time index: differs among variables from the same GCMxRCP:
        # in only once case: HadGEM2-ES x RCP45
        args_first <- c(
          args_first,
          nct = list(get_TimeIndices_netCDF(
            filename = fnc_gcmXscen[1],
            startyear = getYears$first[it, 1],
            endyear = getYears$first[it, 2],
            tres = climDB_meta[["tres"]]
          ))
        )
      }

      scen.monthly[1, it] <- if (use_CF) {
        do.call(get_MonthlyGCMdata_netCDF, args = args_first)
      } else if (use_NEX) {
        do.call(get_GCMdata_NEX, args = args_first)
      } else {
        NULL
      }
    }

    if (print.debug) {
      print(paste0(
        i_tag, " extraction: second slice ('future'): ",
        paste(t(getYears$second), collapse = "-")
      ))
    }

    for (it in seq_len(getYears$n_second)) {
      args_extract2 <- c(
        args_extract1,
        time = list(data.frame(
          year = getYears$second_dates[[it]]$year + 1900,
          month = getYears$second_dates[[it]]$mon + 1
        )),
        dpm = list(getYears$second_dpm[[it]]),
        startyear = getYears$second[it, 1],
        endyear = getYears$second[it, 2]
      )

      if (use_CF) {
        # Assume that netCDF file structure is identical
        # among RCPs within a variable
        #   - differs among variables from the same GCMxRCP: HadGEM2-ES x RCP45
        tag <- paste0(
          climDB_meta[["sep_fname"]],
          rcps[1],
          climDB_meta[["sep_fname"]]
        )
        tmp <- grep(tag, fnc_gcmXscens, ignore.case = TRUE, value = TRUE)[1]

        args_extract2[["nct"]] <- get_TimeIndices_netCDF(
          filename = tmp,
          startyear = getYears$second[it, 1],
          endyear = getYears$second[it, 2],
          tres = climDB_meta[["tres"]]
        )
      }

      for (isc in 2:nrow(scen.monthly)) {
        args_second <- args_extract2
        args_second[["scen"]] <- rcps[isc - 1]

        if (use_CF) {
          tag <- paste0(
            climDB_meta[["sep_fname"]], args_second[["scen"]],
            climDB_meta[["sep_fname"]]
          )
          args_second[["ncFiles"]] <- grep(
            tag,
            fnc_gcmXscens,
            ignore.case = TRUE,
            value = TRUE
          )
        }

        scen.monthly[isc, getYears$n_first + it] <- if (use_CF) {
          do.call(get_MonthlyGCMdata_netCDF, args = args_second)
        } else if (use_NEX) {
          do.call(get_GCMdata_NEX, args = args_second)
        } else {
          NULL
        }
      }
    }

    # Observed historic daily weather from weather database
    if (print.debug) {
      print(paste0(
        i_tag, " extraction: observed historic daily weather from weather DB: ",
        sim_time[["simstartyr"]], "-", sim_time[["endyr"]]
      ))
    }

    obs.hist.daily <- rSOILWAT2::dbW_getWeatherData(
      Site_id = Site_id_by_dbW,
      startYear = sim_time[["simstartyr"]],
      endYear = sim_time[["endyr"]],
      Scenario_id = 1L
    )

    if (obs.hist.daily[[1]]@year < 1950) {
      # TODO(drs): I don't know where the hard coded value of 1950 comes from; it doesn't
      # make sense to me
      print("Note: subsetting years 'obs.hist.daily' because 'simstartyr < 1950'")
      start_yr <- obs.hist.daily[[length(obs.hist.daily)]]@year - 1950
      obs.hist.daily <- obs.hist.daily[(length(obs.hist.daily) - start_yr):length(obs.hist.daily)]
    }

    sim_years <- as.integer(names(obs.hist.daily))
    obs.hist.monthly <- rSOILWAT2::dbW_weatherData_to_monthly(obs.hist.daily)

    if (print.debug) {
      obs.hist.monthly_mean <- stats::aggregate(
        obs.hist.monthly[, -(1:2)],
        list(obs.hist.monthly[, "Month"]), mean
      )
    }

    # Hamlet et al. 2010: "an arbitrary ceiling of 150% of the observed maximum
    # precipitation value for each cell is also imposed by ???spreading out??? very large
    # daily precipitation values into one or more adjacent days"
    dailyPPTceiling <- opt_DS[["daily_ppt_limit"]] *
      max(unlist(lapply(
        obs.hist.daily,
        function(obs) max(obs@data[, "PPT_cm"])
      )))

    # Monthly extremes are used to cut the most extreme spline oscillations; these limits
    # are ad hoc; monthly temperature extremes based on expanded daily extremes
    tmp <- rSW2utils::stretch_values(
      x = range(sapply(
        obs.hist.daily,
        function(obs) obs@data[, c("Tmax_C", "Tmin_C")]
      )),
      lambda = opt_DS[["monthly_limit"]]
    )

    monthly_extremes <- list(
      Tmax = tmp,
      Tmin = tmp,
      PPT = c(
        0,
        opt_DS[["monthly_limit"]] *
          max(tapply(
            obs.hist.monthly[, "PPT_cm"],
            obs.hist.monthly[, 1],
            sum
          ))
      )
    )


    # Loop through todos for downscaling
    for (k in ids_down) {
      ir <- which(rcps == df_wdataOut[["rcps"]][k])
      it <- which(
        rownames(sim_time[["future_yrs"]]) == df_wdataOut[["futures"]][k]
      )

      # Put historical data together
      # NOTE: both scen.hist.monthly and scen.fut.monthly may have NAs
      # because some GCMs do not provide data for the last month of a time slice
      # (e.g. December 2005 may be NA)
      scen.hist.monthly <- NULL
      if (!all(df_wdataOut[["downscaling"]][k] == "raw")) {
        for (itt in which(assocYears[["historical"]]$first)) {
          scen.hist.monthly <- rbind_2cols_nonoverlapping(
            scen.hist.monthly,
            scen.monthly[1, itt][[1]]
          )
        }

        for (itt in which(assocYears[["historical"]]$second)) {
          scen.hist.monthly <- rbind_2cols_nonoverlapping(
            scen.hist.monthly,
            scen.monthly[1 + ir, getYears$n_first + itt][[1]]
          )
        }
      }

      if (print.debug && !is.null(scen.hist.monthly)) {
        scen.hist.monthly_mean <- stats::aggregate(
          scen.hist.monthly[, -(1:2)],
          list(scen.hist.monthly[, "month"]),
          mean,
          na.rm = TRUE
        )

        tmp <- apply(
          scen.hist.monthly_mean[, -1] - obs.hist.monthly_mean[, -1],
          2,
          mean
        )
        print(paste0(
          i_tag, " extraction: 'scen hist' - 'obs hist': ",
          paste(
            colnames(obs.hist.monthly[, -(1:2)]),
            "=",
            round(tmp, 2),
            collapse = ", "
          )
        ))
      }

      # Put future data together
      scen.fut.monthly <- NULL
      for (itt in which(assocYears[[df_wdataOut[["tag"]][k]]]$first)) {
        scen.fut.monthly <- rbind_2cols_nonoverlapping(
          scen.fut.monthly,
          scen.monthly[1, itt][[1]]
        )
      }

      for (itt in which(assocYears[[df_wdataOut[["tag"]][k]]]$second)) {
        scen.fut.monthly <- rbind_2cols_nonoverlapping(
          scen.fut.monthly,
          scen.monthly[1 + ir, getYears$n_first + itt][[1]]
        )
      }

      if (print.debug) {
        scen.fut.monthly_mean <- stats::aggregate(
          scen.fut.monthly[, -(1:2)],
          list(scen.fut.monthly[, "month"]),
          mean,
          na.rm = TRUE
        )
      }

      # Comment: The variables are expected to cover the following time periods
      # 'obs.hist.daily' = simstartyr:endyr
      # 'obs.hist.monthly' = simstartyr:endyr
      # 'scen.hist.monthly' = DScur_startyr:DScur_endyr
      # 'scen.fut.monthly' = DSfut_startyr:DSfut_endyr
      # 'scen.fut.daily' will cover: delta + simstartyr:endyr
      # Units are [degree Celsius] for temperature and [cm / day] and [cm / month],
      #   respectively, for precipitation

      # Apply downscaling
      if (print.debug) {
        print(paste0(
          i_tag, " extraction: ", df_wdataOut[["tag"]][k],
          " downscaling with method ",
          shQuote(df_wdataOut[["downscaling"]][k])
        ))
      }

      dm_fun <- switch(
        df_wdataOut[["downscaling"]][k],
        raw = downscale.raw,
        delta = downscale.delta,
        `hybrid-delta` = downscale.deltahybrid,
        `hybrid-delta-3mod` = downscale.deltahybrid3mod,
        `wgen-package` = downscale.wgen_package,
        stop
      )

      # a list of additional parameters for downscaling
      dm_add_params <- switch(
        df_wdataOut[["downscaling"]][k],
        raw = NULL,
        delta = NULL,
        `hybrid-delta` = NULL,
        `hybrid-delta-3mod` = NULL,
        `wgen-package` = list(
          wgen_dry_spell_changes = if ("wgen_dry_spell_changes" %in% colnames(locations)) {
            locations[il, "wgen_dry_spell_changes"]
          } else {
            1
          },
          wgen_wet_spell_changes = if ("wgen_wet_spell_changes" %in% colnames(locations)) {
            locations[il, "wgen_wet_spell_changes"]
          } else {
            1
          },
          wgen_prcp_cv_changes = if ("wgen_prcp_cv_changes" %in% colnames(locations)) {
            locations[il, "wgen_prcp_cv_changes"]
          } else {
            1
          }
        ),
        stop
      )

      for (do_checks in c(TRUE, FALSE)) {
        scen.fut.daily <- try(dm_fun(
          obs.hist.daily = obs.hist.daily,
          obs.hist.monthly = obs.hist.monthly,
          scen.hist.monthly = scen.hist.monthly,
          scen.fut.monthly = scen.fut.monthly,
          itime = it,
          years = sim_years,
          sim_time = sim_time,
          opt_DS = opt_DS,
          dailyPPTceiling = dailyPPTceiling,
          monthly_extremes = monthly_extremes,
          dbW_digits = dbW_digits,
          do_checks = do_checks,
          add_params = dm_add_params
        ))

        if (!inherits(scen.fut.daily, "try-error")) {
          if (!do_checks) {
            print(paste0(
              i_tag, " extraction: ", df_wdataOut[["tag"]][k], ": ",
              shQuote(df_wdataOut[["downscaling"]][k]),
              " quality checks turned off"
            ))
          }
          break
        }
      }

      if (inherits(scen.fut.daily, "try-error")) {
        stop(scen.fut.daily)
      }

      if (print.debug) {
        tmp <- rSOILWAT2::dbW_weatherData_to_monthly(scen.fut.daily)
        scen.fut.down_mean <- stats::aggregate(
          tmp[, -(1:2)],
          list(tmp[, "Month"]),
          mean
        )

        tmp <- apply(
          scen.fut.down_mean[, -1] - obs.hist.monthly_mean[, -1],
          2,
          mean
        )

        print(paste0(
          i_tag, " extraction: ", df_wdataOut[["tag"]][k], ": ",
          shQuote(df_wdataOut[["downscaling"]][k]),
          "'downscaled fut' - 'obs hist': ",
          paste(
            colnames(obs.hist.monthly[, -(1:2)]),
            "=",
            round(tmp, 2),
            collapse = ", "
          )
        ))

        if (exists("scen.hist.monthly_mean")) {
          # this doesn't exist, e.g., for 'raw' DSing
          tmp <- apply(
            scen.fut.down_mean[, -1] - scen.hist.monthly_mean[, -1],
            2,
            mean
          )

          print(paste0(
            i_tag, " extraction: ", df_wdataOut[["tag"]][k], ": ",
            shQuote(df_wdataOut[["downscaling"]][k]),
            ": 'downscaled fut' - 'scen hist': ",
            paste(
              colnames(obs.hist.monthly[, -(1:2)]),
              "=",
              round(tmp, 2),
              collapse = ", "
            )
          ))
        }
      }

      years <- as.integer(names(scen.fut.daily))
      df_wdataOut[["StartYear"]][k] <- years[1]
      df_wdataOut[["EndYear"]][k] <- years[length(years)]
      df_wdataOut[["weatherData"]][k] <- list(
        rSOILWAT2::dbW_weatherData_to_blob(scen.fut.daily, compression_type)
      )
    }

    saveRDS(
      df_wdataOut,
      file = file.path(
        project_paths[["dir_out_temp"]],
        tolower(gcm),
        paste0(clim_source, "_", i_tag, ".rds")
      )
    )
  }

  on.exit()

  i
}

#' Make daily weather for a scenario
#'
#' A wrapper function for \code{calc_MonthlyScenarioWeather} with error control.
#'
#' @inheritParams calc_MonthlyScenarioWeather
try_MonthlyScenarioWeather <- function(
  i,
  clim_source,
  use_CF,
  use_NEX,
  climDB_meta,
  climDB_files,
  reqGCMs,
  reqRCPsPerGCM,
  reqDownscalingsPerGCM,
  climate.ambient,
  locations,
  compression_type,
  dbW_digits,
  getYears,
  assocYears,
  sim_time,
  seeds_DS,
  opt_DS,
  project_paths,
  dir_failed,
  fdbWeather,
  verbose,
  print.debug
) {

  # Identify index for site and scenario
  # Let ids be 1 to length(req_GCMs)
  #   then, i in ids result in ig = ids and il = 1
  #   then, i in (1 * length(req_GCMs) + ids) result in ig = ids and il = 2
  #   then, i in ((k - 1) * length(req_GCMs) + ids): ig = ids and il = k
  #   ...
  ig <- (i - 1) %% length(reqGCMs) + 1
  gcm <- reqGCMs[ig]
  il <- (i - 1) %/% length(reqGCMs) + 1
  site_id <- locations[il, "site_id"]

  i_tag <- paste0("SiteID", site_id, "-", gcm)

  # reconnect to weather database if connection is currently not valid
  if (!rSOILWAT2::dbW_IsValid()) {
    rSOILWAT2::dbW_setConnection(dbFilePath = fdbWeather)
  }

  res <- NULL
  if (!rSOILWAT2::dbW_IsValid()) {
    print(paste(
      "'try_MonthlyScenarioWeather':", shQuote(i_tag),
      "failed because weather database cannot be accessed."
    ))
  } else {
    tmp <- try(calc_MonthlyScenarioWeather(
      i = i,
      ig = ig,
      il = il,
      gcm = gcm,
      site_id = site_id,
      i_tag = i_tag,
      clim_source = clim_source,
      use_CF = use_CF,
      use_NEX = use_NEX,
      climDB_meta = climDB_meta,
      climDB_files = climDB_files,
      reqGCMs = reqGCMs,
      reqRCPsPerGCM = reqRCPsPerGCM,
      reqDownscalingsPerGCM = reqDownscalingsPerGCM,
      climate.ambient = climate.ambient,
      locations = locations,
      compression_type = compression_type,
      dbW_digits = dbW_digits,
      getYears = getYears, assocYears = assocYears,
      sim_time = sim_time,
      task_seed = seeds_DS[[i]],
      opt_DS = opt_DS,
      project_paths = project_paths,
      dir_failed = dir_failed,
      verbose = verbose,
      print.debug = print.debug
    ))

    if (inherits(tmp, "try-error")) {
      print(paste(Sys.time(), tmp))
      save(
        i, ig, il, gcm, site_id, i_tag, tmp, clim_source, use_CF, use_NEX, climDB_meta,
        climDB_files, reqGCMs, reqRCPsPerGCM, reqDownscalingsPerGCM, climate.ambient,
        locations, compression_type, getYears, assocYears, sim_time, opt_DS,
        project_paths, verbose,
        file = file.path(
          dir_failed,
          paste0("ClimScenMonthly_failed_", i_tag, "_l1.RData")
        )
      )
    } else {
      res <- i
    }
  }

  res
}

#------ End of Monthly netCDF extractions


#------Daily netCDF extractions------

extract_daily_variable_netCDF <- function(
  filenames,
  list_df_sites,
  var_std,
  variable,
  unit,
  unit_from,
  var_rSW2,
  unit_rSW2,
  tres = "daily",
  startyear,
  endyear,
  lon,
  lat
) {
  stopifnot(requireNamespace("ncdf4"))

  tres <- match.arg(tres)

  rsw2_time <- as.Date(
    apply(list_df_sites[[1]][, c("Year", "DOY")], 1, paste0, collapse = "-"),
    "%Y-%j"
  )

  for (knc in seq_along(filenames)) {
    nc <- ncdf4::nc_open(
      filename = filenames[knc],
      write = FALSE,
      readunlim = TRUE,
      verbose = FALSE
    )

    nc_var <- grep(
      paste0("\\b", variable, "\\b"),
      names(nc$var),
      value = TRUE,
      ignore.case = TRUE
    )

    stopifnot(length(nc_var) > 0)
    stopifnot(isTRUE(tolower(unit) == tolower(nc$var[[nc_var]]$units)))

    #--- Dimensions
    nc_perm <- sapply(nc$var[[nc_var]]$dim, function(x) x$name)
    it <- grep("(\\btime\\b)|(\\bt\\b)", nc_perm, ignore.case = TRUE)
    ilat <- grep("(\\lat\\b)|(\\blatitude\\b)", nc_perm, ignore.case = TRUE)
    ilon <- grep("(\\lon\\b)|(\\blongitude\\b)", nc_perm, ignore.case = TRUE)
    dimnames <- rep(NA, length = 3)
    dimnames[it] <- "time"
    dimnames[ilat] <- "latitude"
    dimnames[ilon] <- "longitude"

    nct <- read_time_netCDF(nc, tres = tres)
    ids_nct <- match(rsw2_time, as.Date(nct[["time"]]), nomatch = 0)
    ids_nct2 <- ids_nct > 0

    ncg <- get_SpatialIndices_netCDF(
      filename = nc,
      lon = lon,
      lat = lat
    )

    ks_count <- ks_start <- rep(1, length = 3)
    ks_start[it] <- min(ids_nct[ids_nct2])
    ks_count[it] <- sum(ids_nct2)

    for (ks in seq_along(list_df_sites)) {
      ks_start[ilon] <- ncg[["ix"]][ks]
      ks_start[ilat] <- ncg[["iy"]][ks]

      tmp <- ncdf4::ncvar_get(
        nc,
        varid = variable,
        start = ks_start,
        count = ks_count
      )

      if (var_std == "prcp") {
        tmp <- rSW2utils::convert_precipitation(
          x = tmp,
          unit_from = unit_from,
          unit_to = unit_rSW2
        )
      } else if (var_std %in% c("tmin", "tmax", "tmean")) {
        tmp <- rSW2utils::convert_temperature(
          x = tmp,
          unit_from = unit_from,
          unit_to = unit_rSW2
        )
      }

      list_df_sites[[ks]][ids_nct2, var_rSW2] <- tmp[ids_nct]
    }

    ncdf4::nc_close(nc)
  }

  list_df_sites
}

#' Extract all daily \var{GCM} projection values for precipitation, minimum and
#' maximum temperature from each one \var{netCDF} file.
#'
#' @return A list with three 3-dimensional arrays
#' \var{\dQuote{tmax}}, \var{\dQuote{tmin}}, and \var{\dQuote{prcp}}.
#' Units are [degree Celsius] for temperature and [cm / day] for precipitation.
get_DailyGCMdata_netCDF <- function(
  i_tag,
  climDB_meta,
  is_sequential,
  ncFiles,
  startyear,
  endyear,
  lon,
  lat,
  df_site_template,
  var_map
) {

  ctmp <- i_tag

  list_df_sites <- lapply(seq_along(lon), function(k) df_site_template)


  #--- Extract precipitation data
  ftmp1 <- grep(
    climDB_meta[["var_desc"]]["prcp", "fileVarTags"],
    ncFiles,
    ignore.case = TRUE,
    value = TRUE
  )

  if (length(ftmp1) == 0) {
    stop(
      "No suitable netCDF file with precipitation data ",
      "available for combination ", ctmp
    )
  } else {
    if (!is_sequential && length(ftmp1) > 1) {
      stop(
        "More than one netCDF file with precipitation data ",
        "available for combination ", ctmp,
        " with files = ",
        paste(shQuote(basename(ftmp1)), collapse = "/")
      )
    }
  }

  var <- "prcp"
  itmp <- var_map[, "vars_get_DailyGCMdata_netCDF"] == var

  list_df_sites <- extract_daily_variable_netCDF(
    filenames = ftmp1,
    list_df_sites = list_df_sites,
    var_std = var,
    variable = climDB_meta[["var_desc"]][var, "varname"],
    unit = climDB_meta[["var_desc"]][var, "unit_given"],
    unit_from = climDB_meta[["var_desc"]][var, "unit_real"],
    var_rSW2 = var_map[itmp, "vars_rSW2"],
    unit_rSW2 = var_map[itmp, "units_rSW2"],
    tres = climDB_meta[["tres"]],
    startyear = startyear,
    endyear = endyear,
    lon = lon,
    lat = lat
  )


  #--- Extract temperature data
  ftmp3 <- grep(
    climDB_meta[["var_desc"]]["tmin", "fileVarTags"],
    ncFiles,
    ignore.case = TRUE,
    value = TRUE
  )
  ftmp4 <- grep(
    climDB_meta[["var_desc"]]["tmax", "fileVarTags"],
    ncFiles,
    ignore.case = TRUE,
    value = TRUE
  )

  if (length(ftmp3) > 0 && length(ftmp4) > 0) {
    if (!is_sequential && (length(ftmp3) > 1 || length(ftmp4) > 1)) {
      stop(
        "More than one netCDF file with tmin/tmax data ",
        "available for combination ", ctmp,
        " with files = ",
        paste(shQuote(basename(ftmp3)), collapse = "/"),
        " or ",
        paste(shQuote(basename(ftmp4)), collapse = "/")
      )
    }

    var <- "tmin"
    itmp <- var_map[, "vars_get_DailyGCMdata_netCDF"] == var

    list_df_sites <- extract_daily_variable_netCDF(
      filenames = ftmp3,
      list_df_sites = list_df_sites,
      var_std = var,
      variable = climDB_meta[["var_desc"]][var, "varname"],
      unit = climDB_meta[["var_desc"]][var, "unit_given"],
      unit_from = climDB_meta[["var_desc"]][var, "unit_real"],
      var_rSW2 = var_map[itmp, "vars_rSW2"],
      unit_rSW2 = var_map[itmp, "units_rSW2"],
      tres = climDB_meta[["tres"]],
      startyear = startyear,
      endyear = endyear,
      lon = lon,
      lat = lat
    )

    var <- "tmax"
    itmp <- var_map[, "vars_get_DailyGCMdata_netCDF"] == var

    list_df_sites <- extract_daily_variable_netCDF(
      filenames = ftmp4,
      list_df_sites = list_df_sites,
      var_std = var,
      variable = climDB_meta[["var_desc"]][var, "varname"],
      unit = climDB_meta[["var_desc"]][var, "unit_given"],
      unit_from = climDB_meta[["var_desc"]][var, "unit_real"],
      var_rSW2 = var_map[itmp, "vars_rSW2"],
      unit_rSW2 = var_map[itmp, "units_rSW2"],
      tres = climDB_meta[["tres"]],
      startyear = startyear,
      endyear = endyear,
      lon = lon,
      lat = lat
    )


  } else {
    vars <- c("tmin", "tmax", "tmean")
    unit_from <- climDB_meta[["var_desc"]][vars, "unit_real"]
    stopifnot(unit_from[1] == unit_from[2], unit_from[1] == unit_from[3])

    ftmp2 <- grep(
      climDB_meta[["var_desc"]]["tmean", "fileVarTags"],
      ncFiles,
      ignore.case = TRUE,
      value = TRUE
    )

    if (length(ftmp2) == 0) {
      stop(
        "No suitable netCDF file with mean temperature data ",
        "available for combination ", ctmp
      )
    } else {
      if (!is_sequential && length(ftmp2) > 1) {
        stop(
          "More than one netCDF file with mean temperature data ",
          "available for combination ", ctmp,
          " with files = ",
          paste(shQuote(basename(ftmp2)), collapse = "/")
        )
      }
    }

    var <- "tmin"
    itmp <- var_map[, "vars_get_DailyGCMdata_netCDF"] == var

    list_df_sites <- extract_daily_variable_netCDF(
      filenames = ftmp1,
      list_df_sites = list_df_sites,
      var_std = var,
      variable = climDB_meta[["var_desc"]]["tmean", "varname"],
      unit = climDB_meta[["var_desc"]]["tmean", "unit_given"],
      unit_from = climDB_meta[["var_desc"]]["tmean", "unit_real"],
      var_rSW2 = var_map[itmp, "vars_rSW2"],
      unit_rSW2 = var_map[itmp, "units_rSW2"],
      tres = climDB_meta[["tres"]],
      startyear = startyear,
      endyear = endyear,
      lon = lon,
      lat = lat
    )

    # Copy tmean to both tmin/tmax
    var_rSW2 <- var_map[itmp, "vars_rSW2"]
    var <- "tmax"
    itmp2 <- var_map[, "vars_get_DailyGCMdata_netCDF"] == var
    var2_rSW2 <- var_map[itmp2, "vars_rSW2"]

    for (ks in seq_along(list_df_sites)) {
      list_df_sites[[ks]][, var2_rSW2] <- list_df_sites[[ks]][, var_rSW2]
    }
  }


  list_df_sites
}



get_DailyScenarioData_netCDF <- function(
  id_sim_scen,
  sim_scen_ids1,
  sim_scen_ids1_by_dbW,
  reqGCMs,
  reqRCPsPerGCM,
  clim_source,
  climDB_meta,
  climDB_files,
  locations,
  meta_locations = list(N = nrow(locations), offset = 0),
  getYears,
  fdbWeather,
  compression_type,
  dbW_digits,
  write_tmp_to_disk,
  dir_out_tmp,
  dir_failed,
  chunk_size = 500L,
  verbose = FALSE
) {

  if (!rSOILWAT2::dbW_IsValid()) {
    rSOILWAT2::dbW_setConnection(dbFilePath = fdbWeather)
  }

  ids_Done <- NULL


  #--- Determine which sites still need data
  # `ids_todo_sites` is an index for `locations`
  ids_todo_sites <- which(!as.vector(rSOILWAT2::dbW_has_weatherData(
    Site_ids = locations[, "Site_id_by_dbW"],
    Scenario_ids = sim_scen_ids1_by_dbW[id_sim_scen]
  )))
  #ids_todo_sites <- seq_len(nrow(locations))

  n_todo_sites <- length(ids_todo_sites)

  if (n_todo_sites > 0) {
    # Create chunked index over sites:
    # `ids_seq_todo_sites is indexing objects subset by `ids_todo_sites`,
    # e.g., `x`
    ids_seq_todo_sites <- rSW2utils::make_chunks(
      n_todo_sites,
      chunk_size = chunk_size
    )
    n_chunks <- length(ids_seq_todo_sites)

    #--- Determine RCP x GCM
    sim_scen <- sim_scen_ids1[id_sim_scen]
    id <- strsplit(sim_scen, split = ".", fixed = TRUE)[[1]]
    gcm <- id[4]
    igcm <- which(tolower(gcm) == tolower(reqGCMs))
    stopifnot(length(igcm) == 1)
    scen <- id[3]
    isc <- which(tolower(scen) == tolower(reqRCPsPerGCM[[igcm]]))
    stopifnot(length(isc) == 1)
    slice <- if (tolower(scen) == "historical") "first" else "second"


    if (verbose) {
      print(
        paste(
          "'get_DailyScenarioData_netCDF':", Sys.time(),
          "extracting data from", shQuote(sim_scen),
          "for sites n =", n_todo_sites,
          "distributed in chunks k =", n_chunks
        )
      )
    }

    var_map <- data.frame(
      vars_rSW2 = c("Tmax_C", "Tmin_C", "PPT_cm"),
      units_rSW2 = c("C", "C", "cm/day"),
      vars_get_DailyGCMdata_netCDF = c("tmax", "tmin", "prcp"),
      stringsAsFactors = FALSE
    )


    #--- Select netCDF files for this 'gcm', scenario, and variables
    tmp <- select_suitable_CFs(
      climDB_files = climDB_files,
      climDB_meta = climDB_meta,
      getYears = getYears,
      model_name = gcm,
      scenario_names = scen
    )

    fnc_gcmXscens <- tmp[["files"]]
    rip <- tmp[["rip"]]
    is_sequential = tmp[["is_sequential"]]


    #--- Prepare template for daily weather data
    tmp <- getYears[[paste0(slice, "_dates")]][[1]]

    df_site_template <- data.frame(
      Year = 1900 + tmp$year,
      DOY = 1 + tmp$yday,
      Tmax_C = NA,
      Tmin_C = NA,
      PPT_cm = NA
    )


    #--- Extract data (for chunk of sites)
    for (k in seq_len(n_chunks)) {
      ids_todo_chunk <- ids_todo_sites[ids_seq_todo_sites[[k]]]
      locations_chunk <- locations[ids_todo_chunk, , drop = FALSE]

      x <- try(
        get_DailyGCMdata_netCDF(
          i_tag = paste(c(gcm, scen, rip), collapse = " * "),
          climDB_meta = climDB_meta,
          is_sequential = is_sequential,
          ncFiles = fnc_gcmXscens,
          startyear = getYears[[slice]][1, 1],
          endyear = getYears[[slice]][1, 2],
          lon = locations_chunk[, "X_WGS84"],
          lat = locations_chunk[, "Y_WGS84"],
          df_site_template = df_site_template,
          var_map = var_map
        ),
        silent = TRUE
      )

      if (!inherits(x, "try-error")) {
        #--- Convert list into rSOILWAT2 weather objects for each site
        tmp_ids <- try_prepare_site_with_daily_scenario_weather(
          x,
          rcp = scen,
          scenario = sim_scen,
          scenario_id_by_dbW = sim_scen_ids1_by_dbW[id_sim_scen],
          site_ids_by_dbW = locations_chunk[, "Site_id_by_dbW"],
          compression_type = compression_type,
          dbW_digits = dbW_digits,
          write_tmp_to_disk = write_tmp_to_disk,
          path = file.path(dir_out_tmp, tolower(gcm)),
          filenames = paste0(
            clim_source,
            "_SiteID", locations_chunk[, "site_id"], "-",
            gcm, "-", scen,
            ".rds"
          ),
          dir_failed = dir_failed
        )

        tmp_ids <- as.vector(stats::na.omit(unlist(tmp_ids)))

        if (length(tmp_ids) > 0) {
          #--- Translate `tmp_ids` into values for `ids_ToDo`/`ids_AllToDo`,
          # i.e., length(reqGCMs) x nrow(locations_all)
          # `meta_locations` corrects for chunking of `locations` by
          # function `tryToGet_ClimDB()`
          ids_Done <- c(
            ids_Done,
            (igcm - 1) * meta_locations[["N"]] +
              meta_locations[["offset"]] + ids_todo_chunk[tmp_ids]
          )
        }

      } else {
        print(paste(
          "'get_DailyScenarioData_netCDF': ",
          "call to 'get_DailyGCMdata_netCDF' failed with error message:",
          shQuote(attr(x, "condition")[["message"]])
        ))
      }
    }
  }

  ids_Done
}


prepare_site_with_daily_scenario_weather <- function(
  x,
  rcp,
  scenario,
  scenario_id_by_dbW,
  site_id_by_dbW,
  compression_type,
  dbW_digits,
  write_tmp_to_disk,
  filename
) {

  years <- range(x[, "Year"], na.rm = TRUE)

  rownames(x) <- NULL


  blob_scen_fut_daily <- rSOILWAT2::dbW_weatherData_to_blob(
    weatherData = rSOILWAT2::dbW_dataframe_to_weatherData(
      weatherDF = x,
      weatherDF_dataColumns = colnames(x)[-1],
      round = dbW_digits
    ),
    type = compression_type
  )

  if (!write_tmp_to_disk) {
    # Insert into weather database directly:
    # Faster than writing to disk and then importing into dbWeather by
    # `copy_tempdata_to_dbW`
    # (but parallel concurrent writing to dbW requires to catch errors and
    # ideally wal-mode so that writers don't block readers)
    tmp <- try(
      rSOILWAT2:::dbW_addWeatherDataNoCheck(
        Site_id = site_id_by_dbW,
        Scenario_id = scenario_id_by_dbW,
        StartYear = years[1],
        EndYear = years[2],
        weather_blob = blob_scen_fut_daily
      ),
      silent = TRUE
    )

    # Write to disk instead if inserting to dbW failed
    write_tmp_to_disk <- inherits(tmp, "try-error")
  }


  if (write_tmp_to_disk) {
    # Prepare object for later insertion into weather database
    # by function `copy_tempdata_to_dbW`
    df_wdataOut <- data.frame(
      todo = TRUE,
      downscaling = "idem",
      futures = "dall",
      rcps = rcp,
      tag = paste0("dall.", rcp),
      Scenario = scenario,
      Scenario_id = scenario_id_by_dbW,
      Site_id_by_dbW = site_id_by_dbW,
      StartYear = years[1],
      EndYear = years[2],
      weatherData = NA
    )

    df_wdataOut[["weatherData"]] <- list(blob_scen_fut_daily)

    saveRDS(object = df_wdataOut, file = filename)
  }

  invisible(NULL)
}

try_prepare_site_with_daily_scenario_weather <- function(
  x,
  rcp,
  scenario,
  scenario_id_by_dbW,
  site_ids_by_dbW,
  compression_type,
  dbW_digits,
  write_tmp_to_disk,
  path,
  filenames,
  dir_failed
) {

  res <- rep(NA, length(x))

  for (i in seq_along(x)) {
    tmp <- try(
      prepare_site_with_daily_scenario_weather(
        x[[i]],
        rcp = rcp,
        scenario = scenario,
        scenario_id_by_dbW = scenario_id_by_dbW,
        site_id_by_dbW = site_ids_by_dbW[i],
        compression_type = compression_type,
        dbW_digits = dbW_digits,
        write_tmp_to_disk = write_tmp_to_disk,
        filename = file.path(path, filenames[i])
      )
    )

    if (inherits(tmp, "try-error")) {
      print(paste(Sys.time(), tmp))

      save(
        list = ls(),
        file = file.path(
          dir_failed,
          paste0(
            "ClimScenDaily_failed_",
            # remove file extension
            paste0(
              # nolint start
              {
                tmp <- strsplit(filenames[i], split = ".", fixed = TRUE)[[1]]
                tmp[seq_len(length(tmp) - 1)]
              },
              # nolint end
              collapse = "."
            ),
            ".RData"
          )
        )
      )

    } else {
      res[i] <- i
    }
  }

  res
}


#' Extract daily climate scenario data
#'
#' @section Details:
#' This function parallelize over \code{reqGCMs} x \code{reqRCPsPerGCM}
#' combinations, i.e., data for all \code{locations} are extracted for
#' one value of \code{reqGCMs} x \code{reqRCPsPerGCM} at a time.
#' This is good if file handling is slow and memory is not limiting.
#' However, if data cannot be loaded into memory, then this function
#' cannot work. In this case, this function would need to be re-written to
#' additionally loop over chunks of \code{locations}.
#'
#' @section Notes: This function works only if
#' \itemize{
#'   \item \code{sim_time[["future_yrs"]]} contains \var{"dall"} and
#'   \item \code{reqDownscalingsPerGCM} is \var{"idem"}.
#' }
#'
#' @seealso \code{\link{calc_MonthlyScenarioWeather}}
#'
calc_DailyScenarioWeather <- function(
  clim_source,
  climDB_meta,
  climDB_files,
  reqGCMs,
  reqRCPsPerGCM,
  reqDownscalingsPerGCM,
  locations,
  meta_locations,
  compression_type,
  dbW_digits,
  getYears,
  sim_scen_ids,
  dir_out_tmp,
  dir_failed,
  fdbWeather,
  dbW_mode,
  chunk_size = 500L,
  verbose = FALSE
) {

  ids_Done <- NULL # values of `ids_ToDo` that are completed

  stopifnot(all(unlist(reqDownscalingsPerGCM) == "idem"))

  #--- Check weather database connection
  if (!rSOILWAT2::dbW_IsValid()) {
    rSOILWAT2::dbW_setConnection(dbFilePath = fdbWeather)
  }

  if (rSOILWAT2::dbW_IsValid()) {
    # Scenario IDs in dbWeather (without current/ambient/observational)
    sim_scen_ids1_by_dbW <- rSOILWAT2::dbW_getScenarioId(
      Scenario = sim_scen_ids[-1],
      ignore.case = TRUE
    )

    ids_seq_scens <- seq_along(sim_scen_ids1_by_dbW)


    # Write data directly to dbW
    # (instead of indirectly via temporary disk files):
    # if not parallel or
    # if `concurrent_RW_dbW` exists and is set to TRUE
    # and "rSOILWAT2" is capable of handling it (starting with v5.0.2)
    # and if dbW is in WAL mode (so that writers don't block readers)
    write_tmp_to_dbW <-
      !SFSW2_glovars[["p_has"]] ||
      isTRUE(
        opt_parallel[["concurrent_RW_dbW"]] &&
        getNamespaceVersion("rSOILWAT2") >= "5.0.2" &&
        identical(dbW_mode, "wal")
      )

    cat(
      "Projected daily weather data objects are",
      if (write_tmp_to_dbW) {
        "directly inserted to the weather database."
      } else {
        "written to disk as temporary files and later added to the database."
      },
      "\n"
    )


    #--- Extract and loop over GCM x RCP combinations (in parallel)
    if (SFSW2_glovars[["p_has"]]) {
      if (identical(SFSW2_glovars[["p_type"]], "mpi")) {
        Rmpi::mpi.bcast.cmd(
          cmd = dbW_setConnection_local,
          dbFilePath = fdbWeather
        )
        on.exit(Rmpi::mpi.bcast.cmd(dbW_disconnectConnection_local), add = TRUE)

        ids_Done <- Rmpi::mpi.applyLB(
          ids_seq_scens,
          get_DailyScenarioData_netCDF,
          locations = locations,
          meta_locations = meta_locations,
          sim_scen_ids1 = sim_scen_ids[-1],
          sim_scen_ids1_by_dbW = sim_scen_ids1_by_dbW,
          reqGCMs = reqGCMs,
          reqRCPsPerGCM = reqRCPsPerGCM,
          clim_source = clim_source,
          climDB_meta = climDB_meta,
          climDB_files = climDB_files,
          getYears = getYears,
          fdbWeather = fdbWeather,
          compression_type = compression_type,
          dbW_digits = dbW_digits,
          write_tmp_to_disk = !write_tmp_to_dbW,
          dir_out_tmp = dir_out_tmp,
          dir_failed = dir_failed,
          chunk_size = chunk_size,
          verbose = verbose
        )

      } else if (identical(SFSW2_glovars[["p_type"]], "socket")) {
        parallel::clusterCall(
          SFSW2_glovars[["p_cl"]],
          fun = rSOILWAT2::dbW_setConnection,
          dbFilePath = fdbWeather
        )
        on.exit(
          parallel::clusterEvalQ(
            SFSW2_glovars[["p_cl"]],
            rSOILWAT2::dbW_disconnectConnection()
          ),
          add = TRUE
        )

        ids_Done <- parallel::clusterApplyLB(
          cl = SFSW2_glovars[["p_cl"]],
          x = ids_seq_scens,
          fun = get_DailyScenarioData_netCDF,
          locations = locations,
          meta_locations = meta_locations,
          sim_scen_ids1 = sim_scen_ids[-1],
          sim_scen_ids1_by_dbW = sim_scen_ids1_by_dbW,
          reqGCMs = reqGCMs,
          reqRCPsPerGCM = reqRCPsPerGCM,
          clim_source = clim_source,
          climDB_meta = climDB_meta,
          climDB_files = climDB_files,
          getYears = getYears,
          fdbWeather = fdbWeather,
          compression_type = compression_type,
          dbW_digits = dbW_digits,
          write_tmp_to_disk = !write_tmp_to_dbW,
          dir_out_tmp = dir_out_tmp,
          dir_failed = dir_failed,
          chunk_size = chunk_size,
          verbose = verbose
        )

      } else {
        ids_Done <- NULL
      }

      clean_SFSW2_cluster()
    } else {
      ids_Done <- lapply(
        ids_seq_scens,
        FUN = get_DailyScenarioData_netCDF,
        locations = locations,
        meta_locations = meta_locations,
        sim_scen_ids1 = sim_scen_ids[-1],
        sim_scen_ids1_by_dbW = sim_scen_ids1_by_dbW,
        reqGCMs = reqGCMs,
        reqRCPsPerGCM = reqRCPsPerGCM,
        clim_source = clim_source,
        climDB_meta = climDB_meta,
        climDB_files = climDB_files,
        getYears = getYears,
        fdbWeather = fdbWeather,
        compression_type = compression_type,
        dbW_digits = dbW_digits,
        write_tmp_to_disk = !write_tmp_to_dbW,
        dir_out_tmp = dir_out_tmp,
        dir_failed = dir_failed,
        chunk_size = chunk_size,
        verbose = verbose
      )
    }

  } else {
    print(paste(
      "'calc_DailyScenarioWeather': ",
      "failed because weather database cannot be accessed."
    ))
  }

  unlist(ids_Done)
}

#------ End of Daily netCDF extractions



#------Extraction functions------

#' Subset a list of netCDF CF file names to specific models, scenarios,
#' and variables
select_suitable_CFs <- function(
  climDB_files,
  climDB_meta,
  getYears,
  model_name,
  scenario_names
) {

  files <- climDB_files
  n_scens <- length(scenario_names)

  tag <- paste0(
    climDB_meta[["sep_fname"]], model_name, climDB_meta[["sep_fname"]]
  )
  files <- grep(tag, files, ignore.case = TRUE, value = TRUE)

  tag <- paste0(
    climDB_meta[["sep_fname"]], scenario_names, climDB_meta[["sep_fname"]]
  )
  tag <- paste0("(", tag, ")", collapse = "|")
  files <- grep(tag, files, ignore.case = TRUE, value = TRUE)

  tag <- paste0(
    "(", climDB_meta[["var_desc"]][["fileVarTags"]], ")",
    collapse = "|"
  )
  files <- grep(tag, files, ignore.case = TRUE, value = TRUE)

  fnc_parts <- strsplit(
    basename(files),
    split = climDB_meta[["sep_fname"]],
    fixed = TRUE
  )

  #--- Determine most suitable 'ensemble member' rip that is available
  tmp <- climDB_meta[["str_fname"]][c("id_scen", "id_var", "id_run")]
  ptmp <- sapply(fnc_parts, function(x) x[tmp])

  # Number of netCDF files per scenario, variable, and rip
  # 'pnc_count' should be
  #   * = 1 if netCDF file is available per scen x var x rip combination
  #   * > 1 if multiple time periods are available
  #   * = 0 if files are missing
  #         (yet 'tas' is allowed to replace missing 'tasmax'+'tasmin')
  pnc_count <- table(ptmp[1, ], ptmp[2, ], ptmp[3, ])
  pnc_avail <- apply(pnc_count, 2:3, function(x) sum(x >= 1) >= n_scens)
  tmp <- apply(
    pnc_avail,
    MARGIN = 2,
    FUN = function(x) {
      x[climDB_meta[["var_desc"]]["prcp", "tag"]] && (
        x[climDB_meta[["var_desc"]]["tmean", "tag"]] || (
          x[climDB_meta[["var_desc"]]["tmax", "tag"]] &&
            x[climDB_meta[["var_desc"]]["tmin", "tag"]]))
    }
  )
  rips <- names(tmp)
  rip <- if (length(rips) > 1) sort(rips)[1] else rips

  if (length(rip) == 0) {
    stop(
      "Input file(s) ",
      "for model ", shQuote(model_name),
      " and scenario(s) ", paste(shQuote(scenario_names), collapse = "/"),
      " not available: ",
      paste0(
        colnames(pnc_avail), ": ",
        apply(
          pnc_avail,
          2,
          function(x) paste(rownames(pnc_avail), "=", x, collapse = "/")
        ),
        collapse = " - "
      )
    )
  }

  #--- Presence of multiple netCDFs per `scen x var x rip` combination
  # Determine cause of multiple netCDFS:
  # * (partially) duplicate files one of which covers the requested time period
  # * multiple sequential files that combined cover the requested time period
  is_sequential <- FALSE

  pnc_count_rip <- array(
    pnc_count[, , rip],
    dim = dim(pnc_count)[1:2],
    dimnames = dimnames(pnc_count)[1:2]
  )
  i_count_rip <- which(pnc_count_rip > 1, arr.ind = TRUE)
  fnc_parts2 <- fnc_parts # information that is used to index/subset files
  req_years <- c(
    seq.int(getYears[["first"]][1, 1], getYears[["first"]][1, 2]),
    unlist(lapply(
      seq_len(nrow(getYears[["second"]])),
      function(k) {
        seq.int(getYears[["second"]][k, 1], getYears[["second"]][k, 2])
      }
    ))
  )

  for (k in seq_len(nrow(i_count_rip))) {
    itmp_remove <- NULL
    tmp_var <- colnames(pnc_count_rip)[i_count_rip[k, "col"]]
    tmp_scen <- rownames(pnc_count_rip)[i_count_rip[k, "row"]]

    ids_fnc <- which(sapply(
      fnc_parts2,
      function(x) any(x == rip) && any(x == tmp_var) && any(x == tmp_scen)
    ))

    tmp_times <- lapply(
      fnc_parts2[ids_fnc],
      function(x) {
        startyear <- substr(x[climDB_meta[["str_fname"]]["id_timestart"]], 1, 4)
        tmp_end <- if (
          climDB_meta[["str_fname"]]["id_timestart"] ==
          climDB_meta[["str_fname"]]["id_timeend"]
        ) {
          strsplit(
            x[climDB_meta[["str_fname"]]["id_timestart"]],
            split = "-",
            fixed = TRUE
          )[[1]][2]
        } else {
          substr(x[climDB_meta[["str_fname"]]["id_timeend"]], 1, 4)
        }
        endyear <- substr(tmp_end, 1, 4)

        seq.int(from = as.integer(startyear), to = as.integer(endyear))
      }
    )

    # Temporal overlap with requested years (remove files without overlap)
    has_overlap1 <- sapply(tmp_times, function(x) sum(x %in% req_years))
    itmp_remove <- c(itmp_remove, ids_fnc[has_overlap1 == 0])

    # Temporal overlap among netCDFs or sequential?
    tmp_overlap2 <- outer(
      tmp_times,
      tmp_times,
      FUN = function(x, y) {
        sapply(
          seq_along(x),
          function(k) any(intersect(x[[k]], y[[k]]))
        )
      }
    )
    tmp_overlap2[lower.tri(tmp_overlap2, diag = TRUE)] <- NA

    if (any(tmp_overlap2, na.rm = TRUE)) {
      # Duplicate files that (partially) cover the requested time period
      # --> select the one with the longest coverage
      itmp_remove <- c(itmp_remove, ids_fnc[-which.max(has_overlap1)])

    } else {
      # Multiple sequential files that combined cover the requested time period
      # --> keep the ones that (partially) cover the requested time period
      is_sequential <- TRUE
    }

    if (length(itmp_remove) > 0) {
      files <- files[-itmp_remove]
      fnc_parts2 <- fnc_parts2[-itmp_remove]
    }
  }

  # Subset files to selected rip
  if (length(rip) > 0) {
    tag <- paste0(climDB_meta[["sep_fname"]], rip, climDB_meta[["sep_fname"]])
    files <- grep(tag, files, ignore.case = TRUE, value = TRUE)
  }

  # Check that selected netCDF-files are available for requested variables:
  #   'prcp' and ('tmean' or ('tmax' and 'tmin'))
  fnc_parts <- strsplit(
    basename(files),
    split = climDB_meta[["sep_fname"]],
    fixed = TRUE
  )
  ptmp <- sapply(
    fnc_parts,
    function(x) x[climDB_meta[["str_fname"]][c("id_scen", "id_var")]]
  )
  pnc_count <- table(ptmp[1, ], ptmp[2, ])
  pnc_tmp <- apply(
    pnc_count,
    MARGIN = 2,
    FUN = function(x) {
      tmp <- if (is_sequential) sum(x >= 1) else sum(x == 1)
      tmp >= n_scens
    }
  )

  pnc_avail <- stats::setNames(
    rep(FALSE, length(climDB_meta[["var_desc"]][, "tag"])),
    climDB_meta[["var_desc"]][, "tag"]
  )

  pnc_avail[climDB_meta[["var_desc"]]["prcp", "tag"]] <-
    climDB_meta[["var_desc"]]["prcp", "tag"] %in% names(pnc_tmp) &&
      pnc_tmp[climDB_meta[["var_desc"]]["prcp", "tag"]]

  pnc_avail[climDB_meta[["var_desc"]][c("tmean", "tmin", "tmax"), "tag"]] <-
    (
      climDB_meta[["var_desc"]]["tmean", "tag"] %in% names(pnc_tmp) &&
        pnc_tmp[climDB_meta[["var_desc"]]["tmean", "tag"]]
    ) ||
    all(
      climDB_meta[["var_desc"]][c("tmin", "tmax"), "tag"] %in% names(pnc_tmp) &
        pnc_tmp[climDB_meta[["var_desc"]][c("tmin", "tmax"), "tag"]]
    )

  if (!all(pnc_avail)) {
    stop(
      "File(s) for model ", shQuote(model_name),
      " and scenario(s) ", paste(shQuote(scenario_names), collapse = "/"),
      " not available for required variables: ",
      paste(shQuote(names(pnc_avail)[!pnc_avail]), collapse = "/")
    )
  }

  list(rip = rip, files = files, is_sequential = is_sequential)
}

#' Organizes the calls (in parallel) which obtain specified scenario weather
#' for the weather database from one of the available \var{GCM} sources
#'
#' This function assumes that a whole bunch of global variables exist and
#' contain appropriate values.
#'
#' @section Details:
#' The daily extractions parallelize over \var{GCM} x \var{scenario}
#' combinations, i.e., data for chunks of \var{locations} are extracted for
#' one value of \var{GCM} x \var{scenario} at a time. This is good if
#' file handling is slow and chunk size is adjusted to fit available memory.
#'
#' The monthly extractions parallelize over \var{GCM} x \var{locations}
#' combinations, i.e., data for one \var{location} is extracted for
#' all \var{scenarios} of one \var{GCM} at a time. This is good if file
#' handling is fast and memory is limiting.
#'
#' @param seed A seed set, \code{NULL}, or \code{NA}. \code{NA} will not affect
#'  the state of the \acronym{RNG}; \code{NULL} will re-initialize the
#'  \acronym{RNG}; and all other values are passed to \code{\link{set.seed}}.
tryToGet_ClimDB <- function(
  ids_ToDo,
  clim_source,
  use_CF,
  use_NEX,
  climDB_meta,
  climDB_files,
  reqGCMs,
  reqRCPsPerGCM,
  reqDownscalingsPerGCM,
  locations0,
  is_idem,
  getYears,
  assocYears,
  project_paths,
  dir_failed,
  fdbWeather,
  climate.ambient,
  dbW_compression_type,
  dbW_digits = 4L,
  sim_time,
  seeds_DS,
  sim_scens,
  chunk_size = 500L,
  verbose = FALSE,
  print.debug = FALSE,
  seed = NA
) {

  #--- ids_ToDo based on length(reqGCMs) x nrow(locations0)

  # Query mode of dbWeather
  con <- DBI::dbConnect(RSQLite::SQLite(), fdbWeather)
  dbW_mode <- tolower(DBI::dbGetQuery(con, "PRAGMA journal_mode")[1, 1])
  DBI::dbDisconnect(con)

  if (!identical(dbW_mode, "wal")) {
    cat(
      "Weather database is not in WAL mode:",
      "WAL mode is recommended when adding data in parallel, i.e.,\n",
      "    ```DBI::dbExecute(con, 'PRAGMA journal_mode = wal')```\n\n",
      "Reset to default `delete` mode once weather database is complete,",
      "i.e.,\n",
      "    ```DBI::dbExecute(con, 'PRAGMA journal_mode = delete')```\n\n"
    )
  }

  # requests ids_ToDo: fastest if nc file is
  #  - DONE: permutated to (lat, lon, time) instead (time, lat, lon)
  #  - TODO: many sites are extracted from one nc-read instead of one site
  #          per nc-read (see benchmarking_GDODCPUCLLNL_extractions.R)
  #          (DONE for `is_idem`)


  if (is_idem) {
    #--- Create chunked index over locations0
    # `ids_todo_sites` is an index for `locations0`
    n_sites <- nrow(locations0)
    ids_seq_todo_sites <- rSW2utils::make_chunks(
      n_sites,
      chunk_size = chunk_size
    )
    n_chunks <- length(ids_seq_todo_sites)

    if (verbose) {
      print(
        paste(
          "'tryToGet_ClimDB':", Sys.time(),
          "extracting daily data for sites n =", n_sites,
          "distributed in chunks k =", n_chunks
        )
      )
    }

    #--- Loop over chunks
    for (k in seq_len(n_chunks)) {
      if (verbose) {
        print(
          paste(
            "'tryToGet_ClimDB':", Sys.time(),
            "working on chunk", k, "out of k =", n_chunks, "chunks"
          )
        )
      }

      #--- Extract data
      ids_Done <- calc_DailyScenarioWeather(
        clim_source = clim_source,
        climDB_meta = climDB_meta,
        climDB_files = climDB_files,
        reqGCMs = reqGCMs,
        reqRCPsPerGCM = reqRCPsPerGCM,
        reqDownscalingsPerGCM = reqDownscalingsPerGCM,
        locations = locations0[ids_seq_todo_sites[[k]], , drop = FALSE],
        meta_locations = list(
          N = n_sites,
          offset = min(ids_seq_todo_sites[[k]]) - 1
        ),
        compression_type = dbW_compression_type,
        dbW_digits = dbW_digits,
        getYears = getYears,
        sim_scen_ids = sim_scens[["id"]],
        dir_out_tmp = project_paths[["dir_out_temp"]],
        dir_failed = dir_failed,
        fdbWeather = fdbWeather,
        dbW_mode = dbW_mode,
        chunk_size = chunk_size,
        verbose = verbose
      )

      #--- Process any temporary datafile
      # (in sequential call so to not waste parallel core-time)
      if (!SFSW2_glovars[["p_has"]]) {
        copy_tempdata_to_dbW(
          fdbWeather = fdbWeather,
          clim_source = clim_source,
          dir_out_tmp = project_paths[["dir_out_temp"]],
          verbose = verbose
        )
      }
    }

  } else {
    stopifnot("daily" != climDB_meta[["tres"]])

    if (SFSW2_glovars[["p_has"]]) {
      if (!is.na(seed)) set.seed(seed)

      # attempt to prevent reading from same .nc at the same time
      ids_ToDo <- sample(x = ids_ToDo, size = length(ids_ToDo))

      # extract the GCM data depending on parallel backend
      if (identical(SFSW2_glovars[["p_type"]], "mpi")) {
        Rmpi::mpi.bcast.cmd(
          cmd = dbW_setConnection_local,
          dbFilePath = fdbWeather
        )
        on.exit(Rmpi::mpi.bcast.cmd(dbW_disconnectConnection_local), add = TRUE)

        ids_Done <- Rmpi::mpi.applyLB(
          ids_ToDo,
          try_MonthlyScenarioWeather,
          clim_source = clim_source,
          use_CF = use_CF,
          use_NEX = use_NEX,
          climDB_meta = climDB_meta,
          climDB_files = climDB_files,
          reqGCMs = reqGCMs,
          reqRCPsPerGCM = reqRCPsPerGCM,
          reqDownscalingsPerGCM = reqDownscalingsPerGCM,
          climate.ambient = climate.ambient,
          locations = locations0,
          compression_type = dbW_compression_type,
          dbW_digits = dbW_digits,
          getYears = getYears,
          assocYears = assocYears,
          sim_time = sim_time,
          seeds_DS = seeds_DS,
          opt_DS = sim_scens[["opt_DS"]],
          project_paths = project_paths,
          dir_failed = dir_failed,
          fdbWeather = fdbWeather,
          verbose = verbose, print.debug = print.debug
        )
      } else if (identical(SFSW2_glovars[["p_type"]], "socket")) {
        parallel::clusterCall(
          SFSW2_glovars[["p_cl"]],
          fun = rSOILWAT2::dbW_setConnection,
          dbFilePath = fdbWeather
        )
        on.exit(
          parallel::clusterEvalQ(
            SFSW2_glovars[["p_cl"]],
            rSOILWAT2::dbW_disconnectConnection()
          ),
          add = TRUE
        )

        ids_Done <- parallel::clusterApplyLB(
          SFSW2_glovars[["p_cl"]],
          x = ids_ToDo,
          fun = try_MonthlyScenarioWeather,
          clim_source = clim_source,
          use_CF = use_CF,
          use_NEX = use_NEX,
          climDB_meta = climDB_meta,
          climDB_files = climDB_files,
          reqGCMs = reqGCMs,
          reqRCPsPerGCM = reqRCPsPerGCM,
          reqDownscalingsPerGCM = reqDownscalingsPerGCM,
          climate.ambient = climate.ambient,
          locations = locations0,
          compression_type = dbW_compression_type,
          dbW_digits = dbW_digits,
          getYears = getYears,
          assocYears = assocYears,
          sim_time = sim_time,
          seeds_DS = seeds_DS,
          opt_DS = sim_scens[["opt_DS"]],
          project_paths = project_paths,
          dir_failed = dir_failed,
          fdbWeather = fdbWeather,
          verbose = verbose,
          print.debug = print.debug
        )
      } else {
        ids_Done <- NULL
      }

      clean_SFSW2_cluster()
    } else {
      rSOILWAT2::dbW_setConnection(dbFilePath = fdbWeather)
      on.exit(rSOILWAT2::dbW_disconnectConnection(), add = TRUE)

      ids_Done <- lapply(
        ids_ToDo,
        FUN = try_MonthlyScenarioWeather,
        clim_source = clim_source,
        use_CF = use_CF,
        use_NEX = use_NEX,
        climDB_meta = climDB_meta,
        climDB_files = climDB_files,
        reqGCMs = reqGCMs,
        reqRCPsPerGCM = reqRCPsPerGCM,
        reqDownscalingsPerGCM = reqDownscalingsPerGCM,
        climate.ambient = climate.ambient,
        locations = locations0,
        compression_type = dbW_compression_type,
        dbW_digits = dbW_digits,
        getYears = getYears,
        assocYears = assocYears,
        sim_time = sim_time,
        seeds_DS = seeds_DS,
        opt_DS = sim_scens[["opt_DS"]],
        project_paths = project_paths,
        dir_failed = dir_failed,
        fdbWeather = fdbWeather,
        verbose = verbose,
        print.debug = print.debug
      )

      ids_Done <- do.call(c, ids_Done)
    }
  }

  sort(unlist(ids_Done))
}


copy_tempdata_to_dbW <- function(
  fdbWeather,
  clim_source,
  dir_out_tmp,
  exclude_patterns = "failed",
  verbose = FALSE
) {

  if (verbose) {
    t1 <- Sys.time()
    tmp_call <- shQuote(match.call()[1])
  }

  rSOILWAT2::dbW_setConnection(dbFilePath = fdbWeather)
  on.exit(rSOILWAT2::dbW_disconnectConnection(), add = TRUE)

  dir_failed <- file.path(dir_out_tmp, "failed_copy_tempdata_to_dbW")
  dir.create2(dir_failed, showWarnings = FALSE)

  tmp_files <- list.files(
    path = dir_out_tmp,
    pattern = clim_source,
    recursive = TRUE,
    include.dirs = FALSE,
    no.. = TRUE
  )

  # Remove data files from "failed" attempts
  ids_remove <- if (!anyNA(exclude_patterns)) {
    tmp <- file.path(dirname(tmp_files), basename(tmp_files))
    unique(unlist(lapply(exclude_patterns, function(x) grep(x, tmp))))
  }
  if (length(ids_remove) > 0) {
    tmp_files <- tmp_files[-ids_remove]
  }

  # Make sure that we only process "rds" files (e.g., exclude rda/RData files)
  tmp_files <- grep(".rds\\>", tmp_files, value = TRUE)


  if (length(tmp_files) > 0) {
    if (verbose) {
      print(paste0(
        "rSFSW2's ", tmp_call, ": started at ", t1,
        " with adding temporary files (", length(tmp_files),
        ") into database for ", shQuote(clim_source)
      ))

      on.exit(
        {
          print(
            paste0(
              "rSFSW2's ", tmp_call, ": ended after ",
              round(difftime(Sys.time(), t1, units = "secs"), 2), " s"
            )
          )
        },
        add = TRUE
      )
    }

    req_wdata_fields <- c(
      "todo", "rcps", "futures", "downscaling", "tag",
      "Scenario", "Scenario_id", "Site_id_by_dbW", "StartYear", "EndYear",
      "weatherData"
    )

    for (f in tmp_files) {
      ok <- 0
      fail <- FALSE
      ftmp <- file.path(dir_out_tmp, f)

      if (file.exists(ftmp)) {
        df_wdataOut <- try(readRDS(file = ftmp), silent = !verbose)

        if (
          !inherits(df_wdataOut, "try-error") &&
            all(req_wdata_fields %in% names(df_wdataOut))
        ) {
          for (k in which(df_wdataOut[["todo"]])) {
            if (!is.na(df_wdataOut[["weatherData"]][k])) {
              res <- try(rSOILWAT2:::dbW_addWeatherDataNoCheck(
                Site_id = df_wdataOut[["Site_id_by_dbW"]][k],
                Scenario_id = df_wdataOut[["Scenario_id"]][k],
                StartYear = df_wdataOut[["StartYear"]][k],
                EndYear = df_wdataOut[["EndYear"]][k],
                weather_blob = df_wdataOut[["weatherData"]][k][[1]]
              ))

              if (!inherits(res, "try-error")) {
                ok <- ok + 1
              } else {
                fail <- TRUE
              }
            }
          }

          if (verbose) {
            print(paste0(
              Sys.time(), ": temporary scenario file ", shQuote(f),
              " successfully added n = ", ok,
              " out of t = ", sum(df_wdataOut[["todo"]]),
              " records to weather database",
              if (fail) " and some failed to add"
            ))
          }

        } else {
          cat(
            "Temporary scenario file", shQuote(f), "cannot be read,",
            "likely because it is corrupted, already processed,",
            "or contains malformed data.",
            fill = TRUE
          )
          fail <- TRUE
        }

        if (fail) {
          ftmp_to <- file.path(dir_failed, f)
          if (!dir.exists(dirname(ftmp_to))) {
            dir.create(dirname(ftmp_to), recursive = TRUE, showWarnings = FALSE)
          }
          file.rename(from = ftmp, to = ftmp_to)
        } else {
          unlink(ftmp)
        }

      } else {
        if (verbose) {
          cat(
            "Temporary scenario file", shQuote(f), "does not exist anymore.",
            fill = TRUE
          )
        }
      }
    }
  }

  invisible(tmp_files)
}

#' Determine climate scenario data sources
#'
#' Allow for multiple data sources among sites but not multiple sources per site
#' (for that you need a new row in the \var{\sQuote{InputMain}} spreadsheet)
climscen_determine_sources <- function(
  climDB_metas,
  xmain,
  runIDs_sites,
  runsN_sites,
  how_determine_sources = c("SWRunInformation", "order"),
  scen_sources,
  fmain,
  fpreprocin
) {
  how_determine_sources <- match.arg(how_determine_sources)

  xy <- xmain[runIDs_sites, c("X_WGS84", "Y_WGS84")]

  if (
    how_determine_sources == "SWRunInformation" &&
      "GCM_sources" %in% colnames(xmain)
  ) {
    sites_GCM_source <- prev_sites_GCM_source <-
      xmain[runIDs_sites, "GCM_sources"]
  } else if (
    how_determine_sources == "order" || !("GCM_sources" %in% colnames(xmain))
  ) {
    sites_GCM_source <- rep(NA, times = runsN_sites)
    prev_sites_GCM_source <- NULL
  }

  # determine which data product to use for each site
  # based on bounding boxes of datasets
  i_use <- rep(FALSE, times = runsN_sites)
  for (ds in scen_sources) {
    i_use <- in_box(
      xy,
      climDB_metas[[ds]][["bbox"]]$lon,
      climDB_metas[[ds]][["bbox"]]$lat,
      i_use
    )

    sites_GCM_source[i_use] <- ds
  }

  if (anyNA(sites_GCM_source)) {
    print(paste(
      "No climate change data available for",
      sum(is.na(sites_GCM_source)),
      "sites"
    ))
  }

  # write data to disk
  if (!isTRUE(all.equal(sites_GCM_source, prev_sites_GCM_source))) {
    xmain[runIDs_sites, "GCM_sources"] <- as.character(sites_GCM_source)
    utils::write.csv(xmain, file = fmain, row.names = FALSE)
    unlink(fpreprocin)
  }

  xmain
}


#' @references \url{http://cfconventions.org/}
is_ClimateForecastConvention <- function(climDB_meta) {
  grepl("CF", climDB_meta[["convention"]], ignore.case = TRUE)
}

#' @references \url{https://nex.nasa.gov/nex/projects/1356/}
is_NEX <- function(climDB_meta) {
  "NEX" %in% climDB_meta[["convention"]]
}

#' Calculate historical and future simulation time slices
#'
#' @param sim_time A list with elements \code{future_N}, \code{future_yrs},
#'  \code{DScur_startyr}, and \code{DScur_endyr}.
#' @param tbox A data.frame or matrix with two rows \code{start} and
#'  \code{end} and two columns \code{first} and \code{second} describing years
#'  including in a specific climate data source.
#'
#' @return A data.frame with rows for each extraction run-slice and
#'  four columns 'Run', 'Slice', 'Time', and 'Year'.
calc_timeSlices <- function(sim_time, tbox) {
  # timing: time slices: data is organized into
  #   - 'historical' runs 1950-2005 ( = "first"), and
  #   - future 'rcp' runs 2006-2099 ( = "second")

  deltas <- rownames(sim_time[["future_yrs"]])

  if ("dall" %in% deltas) {
    future_N <- 0
    runs <- "dall"
  } else {
    future_N <- sim_time[["future_N"]]
    runs <- c("historical", deltas)
  }

  timeSlices <- data.frame(matrix(
    nrow = 4 + 4 * future_N,
    ncol = 4,
    dimnames = list(NULL, c("Run", "Slice", "Time", "Year"))
  ))

  timeSlices[, 1:3] <- expand.grid(
    c("start", "end"),
    c("first", "second"),
    runs
  )[, 3:1]

  if ("dall" %in% deltas) {
    timeSlices[, "Year"] <- unlist(tbox)
  } else {

    # historic conditions for downscaling
    timeSlices[1, 4] <- max(tbox["start", "first"], sim_time[["DScur_startyr"]])
    timeSlices[2, 4] <- min(tbox["end", "first"], sim_time[["DScur_endyr"]])

    if (sim_time[["DScur_endyr"]] > tbox["end", "first"]) {
      timeSlices[3, 4] <- tbox["start", "second"]
      timeSlices[4, 4] <- min(tbox["end", "second"], sim_time[["DScur_endyr"]])
    }

    # future conditions for downscaling
    for (it in seq_len(sim_time[["future_N"]])) {
      it4 <- 4L * it
      timeSlices[3 + it4, 4] <- max(
        tbox["start", "second"],
        sim_time[["future_yrs"]][it, "DSfut_startyr"]
      )
      timeSlices[4 + it4, 4] <- min(
        tbox["end", "second"],
        sim_time[["future_yrs"]][it, "DSfut_endyr"]
      ) # limits timeSlices to 2099

      if (sim_time[["DScur_startyr"]] < 1950) {
        # TODO(drs): I don't know where the hard coded value of 1950 comes from;
        #   it doesn't make sense to me
        print("Note: adjustment to 'timeSlices' because 'DScur_startyr < 1950'")
        timeSlices[4 + it4, 4] <- min(
          timeSlices[4 + it4, 4],
          timeSlices[4 + 3 * it, 4] + (timeSlices[4, 4] - timeSlices[1, 4])
        )
      }

      if (
        sim_time[["future_yrs"]][it, "DSfut_startyr"] < tbox["start", "second"]
      ) {
        timeSlices[1 + it4, 4] <- max(
          tbox["start", "first"],
          sim_time[["future_yrs"]][it, "DSfut_startyr"]
        )
        timeSlices[2 + it4, 4] <- tbox["start", "second"]
      }
    }
  }

  timeSlices
}

calc_getYears <- function(timeSlices, is_idem = FALSE) {
  # get unique time slices
  tmp1 <- unique_times(timeSlices, slice = "first")
  tmp2 <- unique_times(timeSlices, slice = "second")

  x <- list(
    n_first = nrow(tmp1),
    first = tmp1,
    n_second = nrow(tmp2),
    second = tmp2
  )

  # Monthly/daily time-series
  tmp1 <- list(
    ISOdate(x[["first"]][, 1], 1, 1, tz = "UTC"),
    ISOdate(x[["first"]][, 2], 12, 31, tz = "UTC")
  )
  tmp2 <- list(
    ISOdate(x[["second"]][, 1], 1, 1, tz = "UTC"),
    ISOdate(x[["second"]][, 2], 12, 31, tz = "UTC")
  )

  daily_dates_first <- lapply(
    seq_len(x[["n_first"]]),
    function(it) {
      as.POSIXlt(seq(
        from = tmp1[[1]][it],
        to = tmp1[[2]][it],
        by = "1 day"
      ))
    }
  )

  daily_dates_second <- lapply(
    seq_len(x[["n_second"]]),
    function(it) {
      as.POSIXlt(seq(
        from = tmp2[[1]][it],
        to = tmp2[[2]][it],
        by = "1 day"
      ))
    }
  )

  x[["first_dates"]] <- if (is_idem) {
    daily_dates_first
  } else {
    lapply(
      seq_len(x[["n_first"]]),
      function(it) {
        as.POSIXlt(seq(
          from = tmp1[[1]][it],
          to = tmp1[[2]][it],
          by = "1 month"
        ))
      }
    )
  }

  x[["second_dates"]] <- if (is_idem) {
    daily_dates_second
  } else {
    lapply(
      seq_len(x[["n_second"]]),
      function(it) {
        as.POSIXlt(seq(
          from = tmp2[[1]][it],
          to = tmp2[[2]][it],
          by = "1 month"
        ))
      }
    )
  }

  # Days per month
  x[["first_dpm"]] <- lapply(
    daily_dates_first,
    function(tmp) {
      rle(tmp$mon)$lengths
    }
  )
  x[["second_dpm"]] <- lapply(
    daily_dates_second,
    function(tmp) {
      rle(tmp$mon)$lengths
    }
  )

  x
}


calc_assocYears <- function(
  sim_time,
  reqRCPs,
  getYears,
  timeSlices,
  is_idem = FALSE
) {
  deltas <- rownames(sim_time[["future_yrs"]])

  if (is_idem && "dall" %in% deltas) {
    future_N <- 0
    names_assocYears <- "dall"
  } else {
    future_N <- sim_time[["future_N"]]
    names_assocYears <- c(
      "historical",
      paste0(deltas, ".", rep(reqRCPs, each = future_N))
    )
  }

  x <- vector("list", length = 1 + length(reqRCPs) * future_N)

  for (it in seq_along(x)) {
    tmp <- strsplit(names_assocYears[it], ".", fixed = TRUE)[[1]][[1]]
    x[[it]] <- list(
      first = useSlices(getYears, timeSlices, run = tmp, slice = "first"),
      second = useSlices(getYears, timeSlices, run = tmp, slice = "second")
    )
  }

  names(x) <- names_assocYears

  x
}


calc_ids_ToDo <- function(ids_AllToDo, ids_Done) {
  if (length(ids_Done) > 0) {
    ids_AllToDo[-ids_Done]
  } else {
    ids_AllToDo
  }
}


# access climate change data
get_climatechange_data <- function(
  clim_source,
  SFSW2_prj_inputs,
  SFSW2_prj_meta,
  locations,
  climDB_meta,
  dbW_compression_type,
  dbW_digits = 4L,
  chunk_size = 500L,
  verbose = FALSE,
  print.debug = FALSE
) {

  if (verbose) {
    print(paste("Started", shQuote(clim_source), "at", Sys.time()))
  }

  # Global flags
  repeatN_max <- 3

  dir_ex_dat <- get_path_to_projectedclimate_datasources(
    project_paths = SFSW2_prj_meta[["project_paths"]],
    clim_source = clim_source
  )

  dir_failed <- file.path(
    SFSW2_prj_meta[["project_paths"]][["dir_out_temp"]],
    "failed_get_climatechange_data"
  )
  dir.create2(dir_failed, showWarnings = FALSE)

  use_CF <- is_ClimateForecastConvention(climDB_meta)
  use_NEX <- is_NEX(climDB_meta)

  # Specific flags
  if (use_CF) {

    # CMIP3 Global and USA
    #  - obs: 1950 Jan to 1999 Dec
    #  - SRES: 1950 Jan to 2099 Dec
    #  - all same time + spatial coordinates
    # CMIP5 Global and USA
    #  - historical: 1950 Jan to 2005 Dec (except: HadGEM2-CC and HadGEM2-ES, to 2005 Nov)
    #  - RCPs:
    #    - in general: 2006 Jan to 2099 Dec or 2100 Dec
    #    - HadGEM2-CC and HadGEM2-ES: 2005 Dec to 2099 Dec
    #    - RCP45 & Globus: HadGEM2-ES: 2005 Dec to 2099 Nov
    #    - RCP45: HadCM2 and MIROC4h: 2006 Jan to 2035 Dec
    #    - no RCP85: GISS-E2-H-CC, GISS-E2-R-CC
    #  => ignore missing Dec value; ignore 2005 Dec value if that is the start
    #  - all same spatial coordinates

    # get netCDF files
    tmp <- list.files(dir_ex_dat, full.names = TRUE, recursive = TRUE)
    ext <- sapply(
      strsplit(basename(tmp), split = ".", fixed = TRUE),
      function(x) x[length(x)]
    )
    climDB_files <- tmp[tolower(ext) %in% c("nc", "nc4", "ncdf", "netcdf")]
    if (length(climDB_files) == 0) {
      stop("Could find no files for ", shQuote(clim_source), " in ", dir_ex_dat)
    }

    # Fix `climDB_meta` in case files represent time-combined MACA data
    if (isTRUE(identical(clim_source, "CMIP5_MACAv2metdata_USA"))) {
      tmp <- unique(sapply(
        strsplit(basename(climDB_files), split = "_", fixed = TRUE),
        function(x) paste0(x[1:2], collapse = "_")
      ))

      if (isTRUE(identical(tmp, "agg_macav2metdata"))) {
        climDB_meta <- climscen_metadata()[["CMIP5_MACAv2metdataAgg_USA"]]
      }
    }


    # Extract metadata from file names
    climDB_fname_meta <- strsplit(
      basename(climDB_files),
      split = climDB_meta[["sep_fname"]],
      fixed = TRUE
    )
    stopifnot(diff(lengths(climDB_fname_meta)) == 0L)

    tmp <- matrix(unlist(climDB_fname_meta), ncol = length(climDB_fname_meta))
    climDB_struct <- lapply(
      climDB_meta[["str_fname"]],
      function(id) unique(tmp[id, ])
    )
  }

  if (use_NEX) {
    ## https://portal.nccs.nasa.gov/portal_home/published/NEX.html
    opt <- options("timeout")
    options(timeout = 5 * 60)

    if (requireNamespace("RCurl")) {
      if (!RCurl::url.exists("https://portal.nccs.nasa.gov/portal_home/published/NEX.html")) {
        # check whether we are online
        stop("We and/or the 'NEX' server are offline")
      }
    } else {
      print("We assume that we and the 'NEX' server are online.")
    }

    climDB_struct <- list(
      id_var = NULL,
      id_gcm = c(
        "inmcm4", "bcc-csm1-1", "bcc-csm1-1-m", "NorESM1-M", "MRI-CGCM3",
        "MPI-ESM-MR", "MPI-ESM-LR", "MIROC5", "MIROC-ESM", "MIROC-ESM-CHEM",
        "IPSL-CM5B-LR", "IPSL-CM5A-MR", "IPSL-CM5A-LR", "HadGEM2-ES",
        "HadGEM2-CC", "HadGEM2-AO", "GISS-E2-R", "GFDL-ESM2M", "GFDL-ESM2G",
        "GFDL-CM3", "FIO-ESM", "FGOALS-g2", "CanESM2", "CSIRO-Mk3-6-0",
        "CNRM-CM5", "CMCC-CM", "CESM1-CAM5", "CESM1-BGC", "CCSM4", "BNU-ESM",
        "ACCESS1-0"
      ),
      id_scen = c("historical", "rcp26", "rcp45", "rcp60", "rcp85"),
      id_run = NULL,
      id_timestart = NULL,
      id_timeend = NULL
    )
    climDB_files <- NULL
  }

  # Force dataset specific lower/uper case for GCMs and RCPs,
  # i.e., use values from 'climbDB_struct' and not reqGCMs and reqRCPs
  tmp <- match(
    tolower(SFSW2_prj_meta[["sim_scens"]][["reqMs"]]),
    tolower(climDB_struct[["id_gcm"]]),
    nomatch = 0
  )
  reqGCMs <- as.character(climDB_struct[["id_gcm"]][tmp])
  tmp <- match(
    tolower(SFSW2_prj_meta[["sim_scens"]][["reqCSs"]]),
    tolower(climDB_struct[["id_scen"]]),
    nomatch = 0
  )
  reqRCPs <- as.character(climDB_struct[["id_scen"]][tmp])
  reqRCPsPerGCM <- lapply(
    SFSW2_prj_meta[["sim_scens"]][["reqCSsPerM"]],
    function(r) {
      tmp <- match(
        tolower(r),
        tolower(climDB_struct[["id_scen"]]),
        nomatch = 0
      )
      as.character(climDB_struct[["id_scen"]][tmp])
    }
  )

  # Tests that all requested conditions will be extracted
  stopifnot(length(reqGCMs) > 0, all(!is.na(reqGCMs)))
  stopifnot(
    length(reqRCPs) > 0,
    all(!is.na(reqRCPs)),
    any(grepl("historic", climDB_struct[["id_scen"]], ignore.case = TRUE))
  )


  #--- put requests together
  # TODO: probably better to include scenarios as well, e.g.,
  # consider requestN <- length(reqRCPs) * length(reqGCMs) * nrow(locations)
  requestN <- length(reqGCMs) * nrow(locations)
  if (verbose) {
    print(paste(shQuote(clim_source), "will run", requestN, "times"))
  }

  if (
    any("wgen-package" %in% unlist(SFSW2_prj_meta[["sim_scens"]][["reqDSsPerM"]]))
  ) {
    icols <- c(
      "wgen_dry_spell_changes",
      "wgen_wet_spell_changes",
      "wgen_prcp_cv_changes"
    )
    locations <- cbind(
      locations,
      SFSW2_prj_inputs[["sw_input_treatments"]][, icols]
    )
  }

  is_idem <-
    "dall" %in% rownames(SFSW2_prj_meta[["sim_time"]][["future_yrs"]]) &&
    "idem" %in% unlist(SFSW2_prj_meta[["sim_scens"]][["reqDSsPerM"]]) &&
    "daily" %in% climDB_meta[["tres"]] &&
    use_CF

  # calculate time slices
  timeSlices <- calc_timeSlices(
    sim_time = SFSW2_prj_meta[["sim_time"]],
    tbox = climDB_meta[["tbox"]]
  )

  # calculate 'getYears' object
  getYears <- calc_getYears(
    timeSlices,
    is_idem = is_idem
  )

  # Logical on how to select from getYears
  assocYears <- calc_assocYears(
    sim_time = SFSW2_prj_meta[["sim_time"]],
    reqRCPs = reqRCPs,
    getYears = getYears,
    timeSlices = timeSlices,
    is_idem = is_idem
  )


  print(paste(
    "Scenario data will be extracted for a time period spanning",
    paste(range(timeSlices[, "Year"], na.rm = TRUE), collapse = " through ")
  ))

  #--- Repeat call to get climate data for all requests until complete
  repeatN <- 0

  # Indices 'ids_AllToDo' and 'ids_Done' are counters in 1:requestN and are
  # thus dependent on nrow(locations) and thus on which sites still
  # need (additional) climate scenario data that wasn't extracted in a
  # previous attempt to extract and downscale all data.
  # That is they shouldn't be used to identify work across runs (where
  # the object `locations` may change), i.e., they should be used locally,
  # but not for files, logs, etc. across repeated calls --
  # for those, use instead `locations[i, "site_id"]` and GCM.

  #--- ids_AllToDo is length(reqGCMs) x nrow(locations)
  ids_AllToDo <- seq_len(requestN)
  ids_Done <- NULL

  # Loop
  while (
    repeatN_max > repeatN &&
      length(ids_ToDo <- calc_ids_ToDo(ids_AllToDo, ids_Done)) > 0
  ) {
    repeatN <- repeatN + 1
    if (verbose) {
      print(paste(
        shQuote(clim_source), "will run", repeatN, "out of", repeatN_max,
        "repeats to extract n =", length(ids_ToDo), "requests"
      ))
    }

    ids_seeds <- as.vector(outer(
      seq_along(reqGCMs),
      (locations[, "site_id"] - 1) * length(reqGCMs),
      FUN = "+"
    ))

    out <- tryToGet_ClimDB(
      ids_ToDo = ids_ToDo,
      clim_source = clim_source,
      use_CF = use_CF,
      use_NEX = use_NEX,
      climDB_meta = climDB_meta,
      climDB_files = climDB_files,
      reqGCMs = reqGCMs,
      reqRCPsPerGCM = reqRCPsPerGCM,
      reqDownscalingsPerGCM = SFSW2_prj_meta[["sim_scens"]][["reqDSsPerM"]],
      locations0 = locations,
      is_idem = is_idem,
      getYears = getYears,
      assocYears = assocYears,
      project_paths = SFSW2_prj_meta[["project_paths"]],
      dir_failed = dir_failed,
      fdbWeather = SFSW2_prj_meta[["fnames_in"]][["fdbWeather"]],
      climate.ambient = SFSW2_prj_meta[["sim_scens"]][["ambient"]],
      dbW_compression_type = dbW_compression_type,
      dbW_digits = dbW_digits,
      sim_time = SFSW2_prj_meta[["sim_time"]],
      seeds_DS = SFSW2_prj_meta[["rng_specs"]][["seeds_DS"]][ids_seeds],
      sim_scens = SFSW2_prj_meta[["sim_scens"]],
      chunk_size = chunk_size,
      verbose = verbose,
      print.debug = print.debug
    )

    ids_Done <- sort(unique(c(ids_Done, out)))
  }

  #--- Process any temporary datafile from a current run
  copy_tempdata_to_dbW(
    fdbWeather = SFSW2_prj_meta[["fnames_in"]][["fdbWeather"]],
    clim_source = clim_source,
    dir_out_tmp = SFSW2_prj_meta[["project_paths"]][["dir_out_temp"]],
    verbose = verbose
  )

  #--- Determine progress
  if (length(ids_Done) > 0) {
    if (verbose) {
      print(paste(
        clim_source, "was extracted for n =", length(ids_Done), "out of",
        length(ids_AllToDo), "downscaling requests"
      ))
    }

    ils_done <- unique((ids_Done - 1) %/% length(reqGCMs) + 1)

    ids_ToDo <- ids_AllToDo[-ids_Done]
  } else {
    ids_ToDo <- ids_AllToDo
  }

  #--- Clean up: report unfinished locations, etc.
  if (length(ids_ToDo) > 0) {
    print(paste(
      length(ids_ToDo),
      "sites didn't extract climate scenario information by '",
      clim_source, "'"
    ))

    ils_notdone <- unique((ids_ToDo - 1) %/% length(reqGCMs) + 1)
    failedLocations_DB <- locations[ils_notdone, ]

    save(
      failedLocations_DB,
      ids_ToDo,
      ils_notdone,
      reqGCMs,
      locations,
      ids_AllToDo,
      file = file.path(
        SFSW2_prj_meta[["project_paths"]][["dir_out"]],
        paste0("ClimDB_failedLocations_", clim_source, ".RData")
      )
    )
  }

  if (verbose) {
    print(paste("Finished '", clim_source, "' at", Sys.time()))
  }

  invisible(TRUE)
}


#' Extract climate scenarios
#'
#' @param todos A logical vector of length \code{runsN_main}. Element locations with
#'  \code{TRUE} indicate to extract climate data for said 'run'. The \code{TRUE} elements
#'  should be a subset of the \code{TRUE}s of \code{SFSW2_prj_inputs[["include_YN"]]}.
#'
#' @export
ExtractClimateChangeScenarios <- function(
  climDB_metas,
  SFSW2_prj_meta,
  SFSW2_prj_inputs,
  todos,
  opt_parallel,
  opt_chunks,
  verbose = FALSE,
  print.debug = FALSE
) {

  if (verbose) {
    t1 <- Sys.time()
    tmp_call <- shQuote(match.call()[1])
    print(paste0("rSFSW2's ", tmp_call, ": started at ", t1))

    on.exit(
      {
        print(paste0(
          "rSFSW2's ", tmp_call, ": ended after ",
          round(difftime(Sys.time(), t1, units = "secs"), 2), " s"
        ))
        cat("\n")
      },
      add = TRUE
    )
  }

  #--- SET UP PARALLELIZATION
  # used in:
  #   - GriddedDailyWeatherFromNCEPCFSR_Global
  setup_SFSW2_cluster(
    opt_parallel,
    dir_out = SFSW2_prj_meta[["project_paths"]][["dir_log"]],
    verbose = opt_verbosity[["verbose"]],
    print.debug = opt_verbosity[["print.debug"]]
  )
  on.exit(
    exit_SFSW2_cluster(verbose = opt_verbosity[["verbose"]]),
    add = TRUE
  )
  on.exit(
    set_full_RNG(
      SFSW2_prj_meta[["rng_specs"]][["seed_prev"]],
      kind = SFSW2_prj_meta[["rng_specs"]][["RNGkind_prev"]][1],
      normal.kind = SFSW2_prj_meta[["rng_specs"]][["RNGkind_prev"]][2]
    ),
    add = TRUE
  )

  rSOILWAT2::dbW_setConnection(
    dbFilePath = SFSW2_prj_meta[["fnames_in"]][["fdbWeather"]]
  )
  on.exit(rSOILWAT2::dbW_disconnectConnection(), add = TRUE)

  dbW_compression_type <- rSOILWAT2::dbW_compression()

  for (m in SFSW2_prj_meta[["sim_scens"]][["reqMs"]]) {
    tmp <- file.path(
      SFSW2_prj_meta[["project_paths"]][["dir_out_temp"]],
      tolower(m)
    )
    dir.create2(
      path = tmp,
      showWarnings = opt_verbosity[["print.debug"]],
      recursive = TRUE
    )
  }

  # Generate seeds for climate change downscaling
  SFSW2_prj_meta[["rng_specs"]][["seeds_DS"]] <- generate_RNG_streams(
    N =
      length(SFSW2_prj_meta[["sim_scens"]][["reqMs"]]) *
        SFSW2_prj_meta[["sim_size"]][["runsN_main"]],
    seed = SFSW2_prj_meta[["rng_specs"]][["global_seed"]],
    reproducible = SFSW2_prj_meta[["opt_sim"]][["reproducible"]]
  )

  # keep track of successful/unsuccessful climate scenarios
  todos_siteIDs <- which(todos)

  # loop through data sources
  sites_GCM_source <- SFSW2_prj_inputs[["SWRunInformation"]][todos, "GCM_sources"]
  clim_sources <- stats::na.exclude(unique(sites_GCM_source))
  if ("NA" %in% clim_sources) {
    clim_sources <- clim_sources[!(clim_sources %in% "NA")]
  }
  icols <- c("X_WGS84", "Y_WGS84", "site_id", "WeatherFolder")

  for (clim_source in clim_sources) {
    iDS_runIDs_sites <- todos_siteIDs[sites_GCM_source %in% clim_source]

    if (length(iDS_runIDs_sites) > 0) {
      # locations of simulation runs
      locations <- SFSW2_prj_inputs[["SWRunInformation"]][iDS_runIDs_sites, icols]
      stopifnot(identical(iDS_runIDs_sites, locations[, "site_id"]))

      tmp <- SFSW2_prj_meta[["sim_size"]][["runIDs_sites"]] %in% iDS_runIDs_sites
      locations[, "Site_id_by_dbW"] <- SFSW2_prj_meta[["sim_size"]][["runIDs_sites_by_dbW"]][tmp]

      if (anyNA(locations[, "Site_id_by_dbW"])) {
        stop("Not all sites (labels) available in weather database.")
      }

      # obtain climate data for these locations requiring data from this climate source
      get_climatechange_data(
        clim_source = clim_source,
        SFSW2_prj_inputs = SFSW2_prj_inputs,
        SFSW2_prj_meta = SFSW2_prj_meta,
        locations = locations,
        climDB_meta = climDB_metas[[clim_source]],
        dbW_compression_type = dbW_compression_type,
        dbW_digits = SFSW2_prj_meta[["opt_sim"]][["dbW_digits"]],
        chunk_size = opt_chunks[["ensembleCollectSize"]],
        verbose = verbose,
        print.debug = print.debug
      )
    }
  }

  # Prepare 'include_YN_climscen'
  include_YN_climscen <- rep(0L, SFSW2_prj_meta[["sim_size"]][["runsN_main"]])

  tmp_ids <- SFSW2_prj_meta[["sim_size"]][["runIDs_sites"]]

  tmp <- !rSOILWAT2::dbW_have_sites_all_weatherData(
    site_labels =
      SFSW2_prj_inputs[["SWRunInformation"]][tmp_ids, "WeatherFolder"],
    site_ids =
      SFSW2_prj_meta[["sim_size"]][["runIDs_sites_by_dbW"]],
    scen_labels = SFSW2_prj_meta[["sim_scens"]][["id"]],
    verbose = verbose
  )

  include_YN_climscen[tmp_ids] <- ifelse(tmp, 0L, 1L)

  SFSW2_prj_inputs[["SWRunInformation"]][, "Include_YN_ClimateScenarioSources"] <- include_YN_climscen
  utils::write.csv(
    SFSW2_prj_inputs[["SWRunInformation"]],
    file = SFSW2_prj_meta[["fnames_in"]][["fmain"]],
    row.names = FALSE
  )
  unlink(SFSW2_prj_meta[["fnames_in"]][["fpreprocin"]])


  # Prepare return
  oe <- sys.on.exit()
  oe <- remove_from_onexit_expression(oe, "exit_SFSW2_cluster")
  on.exit(eval(oe), add = FALSE)

  list(SFSW2_prj_inputs = SFSW2_prj_inputs, SFSW2_prj_meta = SFSW2_prj_meta)
}


#' Extract climate scenarios from downloaded \url{ClimateWizard.org} data
#' @export
ExtractClimateWizard <- function(
  climDB_metas,
  SFSW2_prj_meta,
  SFSW2_prj_inputs,
  todos,
  verbose = FALSE
) {

  if (verbose) {
    t1 <- Sys.time()
    tmp_call <- shQuote(match.call()[1])
    print(paste0("rSFSW2's ", tmp_call, ": started at ", t1))

    on.exit(
      {
        print(paste0(
          "rSFSW2's ", tmp_call, ": ended after ",
          round(difftime(Sys.time(), t1, units = "secs"), 2), " s"
        ))
        cat("\n")
      },
      add = TRUE
    )
  }

  if (SFSW2_prj_meta[["sim_scens"]][["N"]] > 1) {
    if (any("CMIP3_ClimateWizardEnsembles_Global" %in% SFSW2_prj_meta[["sim_scens"]][["sources"]])) {
      # Maurer EP, Adam JC, Wood AW (2009) Climate model based consensus on the hydrologic impacts of climate change to the Rio Lempa basin of Central America. Hydrology and Earth System Sciences, 13, 183-194.
      # accessed via climatewizard.org on July 10, 2012
      dir_ex_dat <- get_path_to_projectedclimate_datasources(
        SFSW2_prj_meta[["project_paths"]],
        "CMIP3_ClimateWizardEnsembles_Global"
      )
    }

    if (any("CMIP3_ClimateWizardEnsembles_USA" %in% SFSW2_prj_meta[["sim_scens"]][["sources"]])) {
      # Maurer, E. P., L. Brekke, T. Pruitt, and P. B. Duffy. 2007. Fine-resolution climate projections enhance regional climate change impact studies. Eos Transactions AGU 88:504.
      # accessed via climatewizard.org
      dir_ex_dat <- get_path_to_projectedclimate_datasources(
        SFSW2_prj_meta[["project_paths"]],
        "CMIP3_ClimateWizardEnsembles_USA"
      )
    }

    list.scenarios.external <- basename(list.dirs2(
      path = dir_ex_dat,
      full.names = FALSE,
      recursive = FALSE
    ))

    if (all(SFSW2_prj_meta[["sim_scens"]][["id"]][-1] %in% list.scenarios.external)) {
      # locations of simulation runs
      locations <- sp::SpatialPoints(
        coords = SFSW2_prj_inputs[["SWRunInformation"]][todos, c("X_WGS84", "Y_WGS84")],
        proj4string = as(sf::st_crs(4326), "CRS")
      )

      # keep track of successful/unsuccessful climate scenarios
      include_YN_climscen <- rep(FALSE, SFSW2_prj_meta[["sim_size"]][["runsN_main"]])

      for (sc in seq_len(SFSW2_prj_meta[["sim_scens"]][["N"]] - 1)) {
        dir_ex_dat.sc <- file.path(dir_ex_dat, SFSW2_prj_meta[["sim_scens"]][["id"]][1 + sc])
        tmp <- basename(list.dirs2(
          path = dir_ex_dat.sc,
          full.names = FALSE,
          recursive = FALSE
        ))

        if ("CMIP3_ClimateWizardEnsembles_Global" %in% SFSW2_prj_meta[["sim_scens"]][["sources"]]) {
          dir_ex_dat.sc.ppt <- file.path(
            dir_ex_dat.sc,
            grep("Precipitation_Value", tmp, value = TRUE)
          )
          dir_ex_dat.sc.tmp <- file.path(
            dir_ex_dat.sc,
            grep("Tmean_Value", tmp, value = TRUE)
          )
        }
        if ("CMIP3_ClimateWizardEnsembles_USA" %in% SFSW2_prj_meta[["sim_scens"]][["sources"]]) {
          dir_ex_dat.sc.ppt <- file.path(
            dir_ex_dat.sc,
            grep("Precipitation_Change", tmp, value = TRUE)
          )
          dir_ex_dat.sc.tmp <- file.path(
            dir_ex_dat.sc,
            grep("Tmean_Change", tmp, value = TRUE)
          )
        }
        list.tmp.asc <- list.files(dir_ex_dat.sc.tmp, pattern = ".asc")
        list.ppt.asc <- list.files(dir_ex_dat.sc.ppt, pattern = ".asc")

        # extract data
        get.month <- function(path, grid, locations) {
          g <- raster::raster(file.path(path, grid))
          locations.CoordG <- sp::spTransform(
            locations,
            CRS = as(sf::st_crs(g), "CRS")
          )
          vals <- raster::extract(g, locations.CoordG)
        }
        sc.tmp <- sapply(
          SFSW2_glovars[["st_mo"]],
          function(m) {
            tmp <- grep(paste0("_", m, "_"), list.tmp.asc, value = TRUE)
            get.month(path = dir_ex_dat.sc.tmp, grid = tmp, locations)
          }
        )
        sc.ppt <- sapply(
          SFSW2_glovars[["st_mo"]],
          function(m) {
            tmp <- grep(paste0("_", m, "_"), list.ppt.asc, value = TRUE)
            get.month(path = dir_ex_dat.sc.ppt, grid = tmp, locations)
          }
        )

        if ("CMIP3_ClimateWizardEnsembles_Global" %in% SFSW2_prj_meta[["sim_scens"]][["sources"]]) {
          # tmp value in C
          # ppt value in mm
          # add data to sw_input_climscen and set the use flags
          itmp1 <- paste0("PPTmm_m", SFSW2_glovars[["st_mo"]], "_sc", formatC(sc, width = 2, format = "d", flag = "0"))
          SFSW2_prj_inputs[["sw_input_climscen_values_use"]][itmp1] <- TRUE
          SFSW2_prj_inputs[["sw_input_climscen_values"]][todos, itmp1] <- sc.ppt
          itmp2 <- paste0("TempC_m", SFSW2_glovars[["st_mo"]], "_sc", formatC(sc, width = 2, format = "d", flag = "0"))
          SFSW2_prj_inputs[["sw_input_climscen_values_use"]][itmp2] <- TRUE
          SFSW2_prj_inputs[["sw_input_climscen_values"]][todos, itmp2] <- sc.tmp

          include_YN_climscen[todos] <- include_YN_climscen[todos] &
            stats::complete.cases(SFSW2_prj_inputs[["sw_input_climscen_values"]][todos, c(itmp1, itmp2)])
        }

        if ("CMIP3_ClimateWizardEnsembles_USA" %in% SFSW2_prj_meta[["sim_scens"]][["sources"]]) {
          sc.tmp <- sc.tmp * 5 / 9 # tmp addand in C
          sc.ppt <- 1 + sc.ppt / 100 # ppt change as factor
          # add data to sw_input_climscen and set the use flags
          itmp1 <- paste0("PPTfactor_m", SFSW2_glovars[["st_mo"]], "_sc", formatC(sc, width = 2, format = "d", flag = "0"))
          SFSW2_prj_inputs[["sw_input_climscen_use"]][itmp1] <- TRUE
          SFSW2_prj_inputs[["sw_input_climscen"]][todos, itmp1] <- sc.ppt
          itmp2 <- paste0("deltaTempC_m", SFSW2_glovars[["st_mo"]], "_sc", formatC(sc, width = 2, format = "d", flag = "0"))
          SFSW2_prj_inputs[["sw_input_climscen_use"]][itmp2] <- TRUE
          SFSW2_prj_inputs[["sw_input_climscen"]][todos, itmp2] <- sc.tmp

          include_YN_climscen[todos] <- include_YN_climscen[todos] &
            stats::complete.cases(SFSW2_prj_inputs[["sw_input_climscen_values"]][todos, c(itmp1, itmp2)])
        }
      }


      # write data to disk
      utils::write.csv(
        reconstitute_inputfile(
          SFSW2_prj_inputs[["sw_input_climscen_values_use"]],
          SFSW2_prj_inputs[["sw_input_climscen_values"]]
        ),
        file = file.path(
          SFSW2_prj_meta[["fnames_in"]][["fclimscen_values"]]
        ),
        row.names = FALSE
      )
      unlink(SFSW2_prj_meta[["fnames_in"]][["fpreprocin"]])

      utils::write.csv(
        reconstitute_inputfile(
          SFSW2_prj_inputs[["sw_input_climscen_use"]],
          SFSW2_prj_inputs[["sw_input_climscen"]]
        ),
        file = file.path(
          SFSW2_prj_meta[["fnames_in"]][["fclimscen_delta"]]
        ),
        row.names = FALSE
      )
      unlink(SFSW2_prj_meta[["fnames_in"]][["fpreprocin"]])

      include_YN_climscen <- as.numeric(include_YN_climscen >= (SFSW2_prj_meta[["sim_scens"]][["N"]] - 1))
      SFSW2_prj_inputs[["SWRunInformation"]][, "Include_YN_ClimateScenarioSources"] <- include_YN_climscen
      utils::write.csv(
        SFSW2_prj_inputs[["SWRunInformation"]],
        file = SFSW2_prj_meta[["fnames_in"]][["fmain"]],
        row.names = FALSE
      )
      unlink(SFSW2_prj_meta[["fnames_in"]][["fpreprocin"]])

      no_ecw <- sum(include_YN_climscen == 0)
      if (no_ecw > 0) {
        print(paste(
          "'ExtractClimateWizard':", no_ecw, "sites didn't extract climate",
          "data"
        ))
      }
    } else {
      print(paste(
        "Not all scenarios requested in 'main file' are",
        "available with 'ExtractClimateWizard'"
      ))
    }
  }

  SFSW2_prj_inputs
}


#' Extracts monthly or daily climate change scenarios
#'
#' Downscales monthly to daily time series
#'
#' @section Details: Overview of algorithm:
#'
#' Function \code{PrepareClimateScenarios}
#'    1) calls `copy_tempdata_to_dbW()`
#'    2) calls `ExtractClimateChangeScenarios()`
#'
#' -> function \code{\link{ExtractClimateChangeScenarios}}
#'    1) prepares parallel setup
#'    1) loops over \var{clim_sources} calling `get_climatechange_data()`
#'
#' -> function \code{\link{get_climatechange_data}}
#'    1) loops over repeated attempts calling `tryToGet_ClimDB()`
#'    1) calls `copy_tempdata_to_dbW()`
#'
#' -> function \code{\link{tryToGet_ClimDB()}}
#'    * implements separate logic for monthly and daily data sets
#'
#'    * monthly data set
#'      * loops in parallel over \var{GCM} x location combinations
#'        calling `try_MonthlyScenarioWeather()`
#'
#'    * daily data set
#'      * loops over chunks of locations
#'        1) calls `calc_DailyScenarioWeather()`
#'        1) calls `copy_tempdata_to_dbW()` (if not parallel)
#'
#' -> function \code{\link{calc_DailyScenarioWeather()}}
#'    * loops in parallel over \var{GCM} x \var{scenarios}
#'      calling `get_DailyScenarioData_netCDF()`
#'
#' -> function \code{\link{get_DailyScenarioData_netCDF()}}
#'    * loops over chunks of locations
#'      1) calls `get_DailyGCMdata_netCDF()`
#'      1) calls `try_prepare_site_with_daily_scenario_weather()`
#'
#' @md
#' @export
PrepareClimateScenarios <- function(
  SFSW2_prj_meta,
  SFSW2_prj_inputs,
  opt_parallel,
  todo_method = c("dbW", "fmain"),
  opt_verbosity,
  opt_chunks
) {

  todo_method <- match.arg(todo_method)

  if (opt_verbosity[["verbose"]]) {
    t1 <- Sys.time()
    tmp_call <- shQuote(match.call()[1])
    print(paste0("rSFSW2's ", tmp_call, ": started at ", t1))

    on.exit(
      {
        print(paste0(
          "rSFSW2's ", tmp_call, ": ended after ",
          round(difftime(Sys.time(), t1, units = "secs"), 2), " s"
        ))
        cat("\n")
      },
      add = TRUE
    )
  }

  climDB_metas <- climscen_metadata()

  SFSW2_prj_inputs[["SWRunInformation"]] <- climscen_determine_sources(
    climDB_metas = climDB_metas,
    xmain = SFSW2_prj_inputs[["SWRunInformation"]],
    runIDs_sites = SFSW2_prj_meta[["sim_size"]][["runIDs_sites"]],
    runsN_sites = SFSW2_prj_meta[["sim_size"]][["runsN_sites"]],
    how_determine_sources = SFSW2_prj_meta[["opt_input"]][["how_determine_sources"]],
    scen_sources = SFSW2_prj_meta[["sim_scens"]][["sources"]],
    fmain = SFSW2_prj_meta[["fnames_in"]][["fmain"]],
    fpreprocin = SFSW2_prj_meta[["fnames_in"]][["fpreprocin"]]
  )


  conventions <- sapply(
    SFSW2_prj_meta[["sim_scens"]][["sources"]],
    function(x) {
      climDB_metas[[x]][["convention"]]
    }
  )


  #--- Process any temporary datafile from a potential previous run
  clim_sources <- unique(
    SFSW2_prj_inputs[["SWRunInformation"]][, "GCM_sources"]
  )
  clim_sources <- stats::na.exclude(clim_sources)
  if ("NA" %in% clim_sources) {
    clim_sources <- clim_sources[!(clim_sources %in% "NA")]
  }

  for (k in seq_along(clim_sources)) {
    copy_tempdata_to_dbW(
      fdbWeather = SFSW2_prj_meta[["fnames_in"]][["fdbWeather"]],
      clim_source = clim_sources[k],
      dir_out_tmp = SFSW2_prj_meta[["project_paths"]][["dir_out_temp"]],
      verbose = opt_verbosity[["verbose"]]
    )
  }


  #--- Determine sites for which climate scenario extractions remain to be done
  todos <-
    SFSW2_prj_inputs[["SWRunInformation"]][, "Include_YN"] %in% 1 &
    SFSW2_prj_inputs[["SWRunInformation"]][, "GCM_sources"] %in%
    SFSW2_prj_meta[["sim_scens"]][["sources"]]

  if (todo_method == "dbW") {
    #--- Compare against weather database
    tmp_var <- "ClimateScenarioSources_Completed"

    # Sites for which climate scenario resources are not already completed
    resave_fmain <- !(
      tmp_var %in% colnames(SFSW2_prj_inputs[["SWRunInformation"]])
    )

    if (resave_fmain) {
      SFSW2_prj_inputs[["SWRunInformation"]][, tmp_var] <- NA
    }

    tmp_ids <- which(todos)

    # Check sites
    if (length(tmp_ids) > 0) {
      rSOILWAT2::dbW_setConnection(
        dbFilePath = SFSW2_prj_meta[["fnames_in"]][["fdbWeather"]]
      )
      on.exit(rSOILWAT2::dbW_disconnectConnection(), add = TRUE)

      tmp_ids2 <- SFSW2_prj_meta[["sim_size"]][["runIDs_sites"]] %in% tmp_ids

      tmp <- !rSOILWAT2::dbW_have_sites_all_weatherData(
        site_labels =
          SFSW2_prj_inputs[["SWRunInformation"]][tmp_ids, "WeatherFolder"],
        site_ids =
          SFSW2_prj_meta[["sim_size"]][["runIDs_sites_by_dbW"]][tmp_ids2],
        scen_labels = SFSW2_prj_meta[["sim_scens"]][["id"]],
        verbose = opt_verbosity[["verbose"]]
      )

      # Determine sites for which we still need to extract data
      todos[tmp_ids] <- tmp

      # Update information in fmain
      resave_fmain <- TRUE
      SFSW2_prj_inputs[["SWRunInformation"]][tmp_ids, tmp_var] <-
        as.integer(!tmp)

    } else {
      todos[] <- FALSE
    }

    if (resave_fmain) {
      utils::write.csv(
        SFSW2_prj_inputs[["SWRunInformation"]],
        file = SFSW2_prj_meta[["fnames_in"]][["fmain"]],
        row.names = FALSE
      )
      unlink(SFSW2_prj_meta[["fnames_in"]][["fpreprocin"]])
    }

  } else if (todo_method == "fmain") {
    #--- Compare against fmain
    tmp_var <- "ClimateScenarioSources_Completed"
    if (tmp_var %in% colnames(SFSW2_prj_inputs[["SWRunInformation"]])) {
      todos <-
        todos & !(SFSW2_prj_inputs[["SWRunInformation"]][, tmp_var] %in% 1)
    }
  }
  names(todos) <- NULL


  #--- Extract
  if (any(todos)) {
    if (any("NEX" %in% conventions) || any("CF" %in% conventions)) {
      tmp <- ExtractClimateChangeScenarios(
        climDB_metas = climDB_metas,
        SFSW2_prj_meta = SFSW2_prj_meta,
        SFSW2_prj_inputs = SFSW2_prj_inputs,
        todos = todos,
        opt_parallel = opt_parallel,
        opt_chunks = opt_chunks,
        verbose = opt_verbosity[["verbose"]],
        print.debug = opt_verbosity[["print.debug"]]
      )

      SFSW2_prj_inputs <- tmp[["SFSW2_prj_inputs"]]
      SFSW2_prj_meta <- tmp[["SFSW2_prj_meta"]]
    }

    if (any("ClimateWizardEnsembles" %in% conventions)) {
      SFSW2_prj_inputs <- ExtractClimateWizard(
        climDB_metas,
        SFSW2_prj_meta,
        SFSW2_prj_inputs,
        todos,
        verbose = opt_verbosity[["verbose"]]
      )
    }
  }


  list(SFSW2_prj_inputs = SFSW2_prj_inputs, SFSW2_prj_meta = SFSW2_prj_meta)
}



#-----Obtain climate projection data------

#' Check and prepare local copy of aggregated \var{CMIP5_MACAv2metdata} dataset
#'
#' @param locations A data frame. Two columns \code{X_WGS84} and
#'   \code{Y_WGS84} of locations describe rectangle
#'   for which data will be downloaded.
#' @param dir_ex_fut A character string. The path name to future climate
#'   projections.
#'
#' @return If all files are available, then a message is printed to the
#'  R console with that information. Otherwise, the message points to a
#'  \var{.sh} script that was created at the
#'  \code{MACAv2metdata_USA} sub-folder. This script must be run
#'  separately to download the missing files.
#'
#' @section Notes: The download scripts use \var{wget}, i.e., it must be
#'   available on your system to work. The scripts are based on the dataset
#'   repository setup at
#'   \url{https://climate.northwestknowledge.net/MACA/index.php} as of
#'   Dec 2019. This dataset has been bias corrected against \var{gridMET}.
#'
#' @references Abatzoglou, J. T. (2013) Development of gridded surface
#'   meteorological data for ecological applications and modelling.
#'   \var{Int. J. Climatol.}, 33: 121–131.
#'
#' @examples
#' if (exists("SFSW2_prj_meta") && exists("SFSW2_prj_inputs")) {
#'   obtain_CMIP5_MACAv2metdata_USA(
#'     locations =
#'       SFSW2_prj_inputs[["SWRunInformation"]][, c("X_WGS84", "Y_WGS84")],
#'     dir_ex_fut = SFSW2_prj_meta[["project_paths"]][["dir_ex_fut"]],
#'   )
#' }
#'
#' @export
obtain_CMIP5_MACAv2metdata_USA <- function(locations, dir_ex_fut) {
  climDB_meta <- climscen_metadata()[["CMIP5_MACAv2metdataAgg_USA"]]

  dir_ex_dat <- file.path(
    dir_ex_fut,
    "ClimateScenarios",
    "CMIP5",
    "MACAv2metdata_USA"
  )

  bbox <- apply(locations, 2, range)

  stopifnot(
    min(bbox[, "X_WGS84"]) >= min(climDB_meta[["bbox"]][, "lon"]),
    max(bbox[, "X_WGS84"]) <= max(climDB_meta[["bbox"]][, "lon"]),
    min(bbox[, "Y_WGS84"]) >= min(climDB_meta[["bbox"]][, "lat"]),
    max(bbox[, "Y_WGS84"]) <= max(climDB_meta[["bbox"]][, "lat"])
  )

  gcms <- c(
    "bcc-csm1-1", "bcc-csm1-1-m", "BNU-ESM", "CanESM2", "CCSM4",
    "CNRM-CM5", "CSIRO-Mk3-6-0", "GFDL-ESM2G", "GFDL-ESM2M", "HadGEM2-CC365",
    "HadGEM2-ES365", "inmcm4", "IPSL-CM5A-LR", "IPSL-CM5A-MR", "IPSL-CM5B-LR",
    "MIROC-ESM", "MIROC-ESM-CHEM", "MIROC5", "MRI-CGCM3", "NorESM1-M"
  )

  rcps <- c("historical", "rcp45", "rcp85")

  ids <- !is.na(climDB_meta[["var_desc"]][, "varname"])
  vars <- climDB_meta[["var_desc"]][ids, c("varname", "tag")]


  #--- Should have these netCDF files
  ttmp <- apply(climDB_meta[["tbox"]], 2, paste0, collapse = "_")

  tmp <- expand.grid(
    Var = vars[, "tag"],
    Model = gcms,
    rip = NA,
    Scen = rcps
  )

  tmp[, "Period"] <- ifelse(
    tmp[, "Scen"] == "historical",
    ttmp[1],
    ttmp[2]
  )

  tmp[, "rip"] <- ifelse(
    tmp[, "Model"] == "CCSM4",
    "r6i1p1",
    "r1i1p1"
  )

  need_files <- paste0(
    "agg_macav2metdata_",
    apply(tmp, 1, paste0, collapse = "_"),
    "_CONUS_daily.nc"
  )


  #--- Check which one of these files are available
  doesnt_have_files <- !file.exists(file.path(dir_ex_dat, need_files))

  if (any(doesnt_have_files)) {
    ids_get_files <- which(doesnt_have_files)

    wget_bash <- c(
      "#!/bin/bash",
      paste0(
        'wget -nc -c -nd ',
        '"http://thredds.northwestknowledge.net:8080/thredds/ncss/grid/',
        need_files[ids_get_files],
        '?&var=',
          vars[match(tmp[ids_get_files, "Var"], vars[, "tag"]), "varname"],
        '&north=', max(bbox[, "Y_WGS84"]),
        '&south=', min(bbox[, "Y_WGS84"]),
        '&west=', min(bbox[, "X_WGS84"]),
        '&east=', max(bbox[, "X_WGS84"]),
        '&temporal=all&accept=netcdf&point=false" -O ',
        need_files[ids_get_files]
      )
    )

    fname_bash <- file.path(
      dir_ex_dat,
      paste0("macav2metdata_wget_", format(Sys.time(), "%Y%m%d%H%M%S"), ".sh")
    )

    writeLines(wget_bash, con = fname_bash)

    stop(
      "Please execute script ",
      shQuote(fname_bash),
      " to download missing MACAv2metdata_USA data."
    )
  } else {
    print(paste(
      "All MACAv2metdata_USA files are available;",
      "however, spatial coverage was not checked."
    ))
  }
}


#------END CLIMATE CHANGE DATA------
