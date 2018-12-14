#' \pkg{rSOILWAT2} data access functions
#'
#' @param x An object of class
#'   \code{\link[rSOILWAT2:swOutput-class]{rSOILWAT2::swOutput}}.
#' @param st An object as returned from the function
#'   \code{\link{setup_time_simulation_project}}.
#'
#' @name swOutput_access
NULL

#' @inheritParams swOutput_access
#' @rdname swOutput_access
get_Response_aggL <- function(response,
  tscale = c("dy", "dyAll", "mo", "moAll", "yr", "yrAll"), scaler = 10,
  FUN, weights = NULL, x, st, st2, topL, bottomL) {

  FUN <- match.fun(FUN)
  tscale <- match.arg(tscale)
  responseRepeats <- if (response %in% c("TRANSP", "HYDRED")) {
      # divide by 5, because each soil layer (cm) has five entries for:
      # total, trees, shrubs, forbs, grasses
      5L
    } else {
      # this case if for: sw_vwc, sw_evsoil, sw_soiltemp, sw_swc, sw_swa
      1L
    }

  temp1 <- scaler * slot(slot(x, response),
    switch(tscale,
      dy = "Day", dyAll = "Day",
      mo = "Month", moAll = "Month",
      yr = "Year", yrAll = "Year"))

  if (inherits(temp1, "try-error"))
    stop("Necessary SOILWAT2 output files are not present for aggregation of ",
      "results")

  if (tscale == "dy") {
    index.col <- 2
    index.usetimestep <- st$index.usedy
    timestep_ForEachEntry <- st2$doy_ForEachUsedDay
  } else if (tscale == "dyAll") {
    index.col <- 2
    index.usetimestep <- seq_len(nrow(temp1))
    timestep_ForEachEntry <- NULL
  } else if (tscale == "mo") {
    index.col <- 2
    index.usetimestep <- st$index.usemo
    timestep_ForEachEntry <- st2$month_ForEachUsedMonth
  } else if (tscale == "moAll") {
    index.col <- 2
    index.usetimestep <- seq_len(nrow(temp1))
    timestep_ForEachEntry <- NULL
  } else if (tscale == "yr") {
    index.col <- 1
    index.usetimestep <- st$index.useyr
    timestep_ForEachEntry <- NULL
  } else if (tscale == "yrAll") {
    index.col <- 1
    index.usetimestep <- seq_len(nrow(temp1))
    timestep_ForEachEntry <- NULL
  }

  layers <- seq_len((ncol(temp1) - index.col) / responseRepeats)

  # adjust topL and bottomL locally in case temp1 doesn't contain information
  # for every layer, e.g., soil evaporation
  if (max(layers) <= max(topL)) {
    topL <- layers
    bottomL <- 0
  } else if (max(layers) < max(bottomL)) {
    bottomL <- min(bottomL):max(layers)
  }

  res <- list()
  res[["top"]] <- if (length(topL) > 1) {
      if (is.null(weights)) {
        apply(temp1[index.usetimestep, index.col + topL, drop = FALSE], 1, FUN)
      } else {
        apply(temp1[index.usetimestep, index.col + topL, drop = FALSE], 1, FUN,
          weights[topL])
      }
    } else {
      temp1[index.usetimestep, index.col + topL]
    }
  res[["bottom"]] <- if (length(bottomL) > 1) {
      if (is.null(weights)) {
        apply(temp1[index.usetimestep, index.col + bottomL, drop = FALSE], 1,
          FUN)
      } else {
        apply(temp1[index.usetimestep, index.col + bottomL, drop = FALSE], 1,
          FUN, weights[bottomL])
      }
    } else if (is.null(bottomL) || identical(bottomL, 0)) {
      matrix(data = 0, nrow = length(index.usetimestep), ncol = 1)
    } else {
      temp1[index.usetimestep, index.col + bottomL]
    }

  if (!is.null(timestep_ForEachEntry)) {
    res[["aggMean.top"]] <- tapply(res[["top"]], timestep_ForEachEntry, mean)
    res[["aggMean.bottom"]] <- tapply(res[["bottom"]], timestep_ForEachEntry,
      mean)
  }

  if (tscale == "dyAll" || tscale == "moAll" || tscale == "yrAll") {
     res[["val"]] <- temp1
  }

  res
}

get_SWPmatric_aggL <- function(vwcmatric, texture, sand, clay) {
  res <- list()

  if (!is.null(vwcmatric[["top"]])) {
    res[["top"]] <- rSOILWAT2::VWCtoSWP(vwcmatric[["top"]],
      sand = texture[["sand.top"]], clay = texture[["clay.top"]])
  }

  if (!is.null(vwcmatric$bottom)) {
    res[["bottom"]] <- rSOILWAT2::VWCtoSWP(vwcmatric[["bottom"]],
      sand = texture[["sand.bottom"]], clay = texture[["clay.bottom"]])
  }

  if (!is.null(vwcmatric[["aggMean.top"]])) {
    res[["aggMean.top"]] <- rSOILWAT2::VWCtoSWP(vwcmatric[["aggMean.top"]],
      sand = texture[["sand.top"]], clay = texture[["clay.top"]])
    res[["aggMean.bottom"]] <- rSOILWAT2::VWCtoSWP(vwc =
      vwcmatric[["aggMean.bottom"]], sand = texture[["sand.bottom"]],
      clay = texture[["clay.bottom"]])
  }

  if (!is.null(vwcmatric$val)) {
    if (all(as.integer(vwcmatric$val[, 2]) == vwcmatric$val[, 2])) {
      index.header <- 1:2 # daily, weekly, monthly
    } else {
      index.header <- 1 # yearly
    }
    res[["val"]] <- cbind(vwcmatric[["val"]][, index.header],
      rSOILWAT2::VWCtoSWP(vwcmatric[["val"]][, -index.header], sand, clay))
  }

  res
}

#' @inheritParams swOutput_access
#' @rdname swOutput_access
get_Temp_yr <- function(x, st) {
  list(mean = slot(slot(x, "TEMP"), "Year")[st$index.useyr, 4, drop = FALSE])
}

#' @inheritParams swOutput_access
#' @rdname swOutput_access
get_Temp_mo <- function(x, st) {
  x <- slot(slot(x, "TEMP"), "Month")[st$index.usemo, ]
  list(min =  x[, 4],
       mean = x[, 5],
       max =  x[, 3])
}

#' @inheritParams swOutput_access
#' @rdname swOutput_access
get_Temp_dy <- function(x, st) {
  x <- slot(slot(x, "TEMP"), "Day")[st$index.usedy, ]
  list(min =  x[, 4],
       mean = x[, 5],
       max =  x[, 3],
       surface = x[, 6])
}

#' @inheritParams swOutput_access
#' @rdname swOutput_access
get_VPD_mo <- function(sc, temp.mo, xin, st2) {
  rH <- rSOILWAT2::swCloud_SkyCover(xin[[sc]])
  rH <- as.vector(rH[st2$month_ForEachUsedMonth])

  list(mean = vpd(temp.mo$min, temp.mo$max, rH))
}

get_VPD_dy <- function(sc, temp.dy, xin, st2) {
  rH <- rSOILWAT2::swCloud_SkyCover(xin[[sc]])
  rH <- as.vector(rH[st2$month_ForEachUsedDay])

  list(mean = vpd(temp.dy$min, temp.dy$max, rH))
}

get_PPT_yr <- function(x, st) {
  x <- 10 * slot(slot(x, "PRECIP"), "Year")[st$index.useyr, , drop = FALSE]
  list(ppt = x[, 2, drop = FALSE],
    rain = x[, 3, drop = FALSE],
    snowfall = x[, 4, drop = FALSE],
    snowmelt = x[, 5, drop = FALSE],
    snowloss = x[, 6, drop = FALSE])
}

#' @inheritParams swOutput_access
#' @rdname swOutput_access
get_PPT_mo <- function(x, st) {
  x <- 10 * slot(slot(x, "PRECIP"), "Month")[st$index.usemo, ]
  list(ppt = x[, 3], rain = x[, 4],
       snowmelt = x[, 6])
}

#' @inheritParams swOutput_access
#' @rdname swOutput_access
get_PPT_dy <- function(x, st) {
  x <- 10 * slot(slot(x, "PRECIP"), "Day")[st$index.usedy, ]
  list(ppt = x[, 3], rain = x[, 4],
       snowfall = x[, 5], snowmelt = x[, 6], snowloss = x[, 7])
}

#' @inheritParams swOutput_access
#' @rdname swOutput_access
get_PET_yr <- function(x, st) {
  list(val = 10 * slot(slot(x, "PET"), "Year")[st$index.useyr, 2, drop = FALSE])
}

#' @inheritParams swOutput_access
#' @rdname swOutput_access
get_PET_mo <- function(x, st) {
  list(val = 10 * slot(slot(x, "PET"), "Month")[st$index.usemo, 3])
}

#' @inheritParams swOutput_access
#' @rdname swOutput_access
get_AET_yr <- function(x, st) {
  list(val = 10 * slot(slot(x, "AET"), "Year")[st$index.useyr, 2, drop = FALSE])
}

#' @inheritParams swOutput_access
#' @rdname swOutput_access
get_AET_mo <- function(x, st) {
  list(val = 10 * slot(slot(x, "AET"), "Month")[st$index.usemo, 3])
}

#' @inheritParams swOutput_access
#' @rdname swOutput_access
get_AET_dy <- function(x, st) {
  list(val = 10 * slot(slot(x, "AET"), "Day")[st$index.usedy, 3])
}

#' @inheritParams swOutput_access
#' @rdname swOutput_access
get_SWE_mo <- function(x, st) {
  list(val = 10 * slot(slot(x, "SNOWPACK"), "Month")[st$index.usemo, 3])
}

#' @inheritParams swOutput_access
#' @rdname swOutput_access
get_SWE_dy <- function(x, st) {
  list(val = 10 * slot(slot(x, "SNOWPACK"), "Day")[st$index.usedy, 3])
}

#' @inheritParams swOutput_access
#' @rdname swOutput_access
get_Inf_yr <- function(x, st) {
  list(inf = 10 * slot(slot(x, "SOILINFILT"), "Year")[st$index.useyr, 2,
    drop = FALSE])
}

#' @inheritParams swOutput_access
#' @rdname swOutput_access
get_Inf_mo <- function(x, st) {
  list(inf = 10 * slot(slot(x, "SOILINFILT"), "Month")[st$index.usemo, 3])
}

#' @inheritParams swOutput_access
#' @rdname swOutput_access
get_Inf_dy <- function(x, st) {
  list(inf = 10 * slot(slot(x, "SOILINFILT"), "Day")[st$index.usedy, 3])
}

#' @inheritParams swOutput_access
#' @rdname swOutput_access
get_Esurface_yr <- function(x, st) {
  x <- 10 * slot(slot(x, "EVAPSURFACE"), "Year")[st$index.useyr, , drop = FALSE]
  list(sum = x[, 2, drop = FALSE],
       veg = rowSums(x[, 3:6, drop = FALSE]),
       litter = x[, 7, drop = FALSE],
       surfacewater = x[, 8, drop = FALSE])
}

#' @inheritParams swOutput_access
#' @rdname swOutput_access
get_Esurface_dy <- function(x, st) {
  x <- 10 * slot(slot(x, "EVAPSURFACE"), "Day")[st$index.usedy, ]
  list(sum = x[, 3],
       veg = rowSums(x[, 4:7, drop = FALSE]),
       litter = x[, 8],
       surfacewater = x[, 9])
}

#' @inheritParams swOutput_access
#' @rdname swOutput_access
get_Interception_yr <- function(x, st) {
  x <- 10 * slot(slot(x, "INTERCEPTION"), "Year")[st$index.useyr, ,
    drop = FALSE]
  list(sum = x[, 2, drop = FALSE],
       veg = rowSums(x[, 3:6, drop = FALSE]),
       litter = x[, 7, drop = FALSE])
}

#' @inheritParams swOutput_access
#' @rdname swOutput_access
get_DeepDrain_yr <- function(x, st) {
  list(val = 10 * slot(slot(x, "DEEPSWC"), "Year")[st$index.useyr, 2])
}

#' @inheritParams swOutput_access
#' @rdname swOutput_access
get_DeepDrain_mo <- function(x, st) {
  list(val = 10 * slot(slot(x, "DEEPSWC"), "Month")[st$index.usemo, 3])
}

#' @inheritParams swOutput_access
#' @rdname swOutput_access
get_DeepDrain_dy <- function(x, st) {
  list(val = 10 * slot(slot(x, "DEEPSWC"), "Day")[st$index.usedy, 3])
}

#' @inheritParams swOutput_access
#' @rdname swOutput_access
get_RunOnOff_mo <- function(x, st) {
  x <- 10 * slot(slot(x, "RUNOFF"), "Month")[st$index.usemo, ]
  list(net = x[, 3],
       total_runoff = x[, 4] + x[, 5],
       ponded_runoff = x[, 4],
       snowmelt_runoff = x[, 5],
       total_runon = x[, 6],
       ponded_runon = x[, 6])
}

#' @inheritParams swOutput_access
#' @rdname swOutput_access
get_RunOnOff_yr <- function(x, st) {
  x <- 10 * slot(slot(x, "RUNOFF"), "Year")[st$index.useyr, , drop = FALSE]
  list(net = x[, 2, drop = FALSE],
       total_runoff = x[, 3, drop = FALSE] + x[, 4, drop = FALSE],
       ponded_runoff = x[, 3, drop = FALSE],
       snowmelt_runoff = x[, 4, drop = FALSE],
       total_runon = x[, 5, drop = FALSE],
       ponded_runon = x[, 5, drop = FALSE])
}

#' @inheritParams swOutput_access
#' @rdname swOutput_access
get_Vegetation_yr <- function(x, st) {
  x <- slot(slot(x, "CO2EFFECTS"), "Year")[st$index.useyr, , drop = FALSE]
  list(val = x[, 1 + seq_len(2 * 5), drop = FALSE])
}


#' @inheritParams swOutput_access
#' @rdname swOutput_access
get_CO2effects_yr <- function(x, st) {
  x <- slot(slot(x, "CO2EFFECTS"), "Year")[st$index.useyr, , drop = FALSE]
  list(val = x[, 11 + seq_len(2 * 4), drop = FALSE])
}
