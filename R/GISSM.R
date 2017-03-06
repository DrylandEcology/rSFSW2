########################
#------ GISSM functions
# Schlaepfer, D.R., Lauenroth, W.K. & Bradford, J.B. (2014). Modeling regeneration responses of big sagebrush (Artemisia tridentata) to abiotic conditions. Ecol Model, 286, 66-77.

#' Function to convert soil depth to soil layer
SoilLayer_at_SoilDepth <- function(depth_cm, layers_depth) {
  pmax(1, pmin(length(layers_depth), 1 + findInterval(depth_cm - 0.01, layers_depth)))
}


#' Function to calculate for each day of the year, duration in days of upcoming favorable conditions accounting for consequences.unfavorable = 0 (if conditions become unfavorable, then restart the count), =1 (resume)
calculate_DurationFavorableConditions <- function(RYyear, consequences.unfavorable, Germination_DuringFavorableConditions, RYyear_ForEachUsedDay) {

  index.year <- RYyear_ForEachUsedDay == RYyear
  conditions <- Germination_DuringFavorableConditions[index.year]
  doys <- seq_len(sum(index.year))
  doys[!conditions] <- NA  #calculate only for favorable days
  out <- rep(NA, times = sum(index.year))

  if (consequences.unfavorable == 0) {
    # if conditions become unfavorable, then restart the count afterwards
    temp.rle <- rle(conditions)
    if (sum(!temp.rle$values) > 0) {
      temp.unfavorable_startdoy <- c((1 + c(0, cumsum(temp.rle$lengths)))[!temp.rle$values], 1 + sum(index.year)) #add starts for odd- and even-lengthed rle

      temp.rle$values <- if (temp.rle$values[1]) {
          #first rle period is favorable
          rep(temp.unfavorable_startdoy, each = 2)
        } else {
          #first rle period is unfavorable
          rep(temp.unfavorable_startdoy[-1], each = 2)
        }
      temp.rle$values <- temp.rle$values[seq_along(temp.rle$lengths)]

    } else {#every day is favorable
      temp.rle$values <- length(conditions) + 1
    }
    out <- inverse.rle(temp.rle) - doys  #difference to next following start of a period of unfavorable conditions

  } else if (consequences.unfavorable == 1) {
    # if conditions become unfavorable, then resume the count afterwards
    temp <- sum(conditions)
    count <- if (temp > 0) {
      temp:1
    } else {#every day is unfavorable
      vector("numeric", length = 0)
    }

    out <- stats::napredict(stats::na.action(stats::na.exclude(doys)), count)  #sum of following favorable conditions in this year
  }

  out
}

get_modifiedHardegree2006NLR <- function(RYdoy, Estimate_TimeToGerminate, TmeanJan,
  a, b, c, d, k1_meanJanTemp, k2_meanJanTempXIncubationTemp, k3_IncubationSWP, Tgerm.year,
  SWPgerm.year, durations, rec.delta = 1, nrec.max = 10L) {

  for (nrec in seq_len(nrec.max)) {
    Estimate_TimeToGerminate <- Estimate_TimeToGerminate.oldEstimate <- max(0, round(Estimate_TimeToGerminate))

    ids <- RYdoy:(RYdoy + Estimate_TimeToGerminate - 1)
    Tgerm <- mean(Tgerm.year[ids], na.rm = TRUE)
    SWPgerm <- mean(SWPgerm.year[ids], na.rm = TRUE)

    temp.c.lim <- -(Tgerm - b) * (d^2 - 1) / d
    c <- if (c > 0) {
      if (c > temp.c.lim) c else {temp.c.lim + SFSW2_glovars[["tol"]]}
    } else if (c < 0) {
      if (c < temp.c.lim) c else {temp.c.lim - SFSW2_glovars[["tol"]]}
    }

    #NLR model (eq.5) in Hardegree SP (2006) Predicting Germination Response to Temperature. I. Cardinal-temperature Models and Subpopulation-specific Regression. Annals of Botany, 97, 1115-1125.
    temp <- a * exp(-0.693147181 / log(d)^2 * log(1 + (Tgerm - b) * (d^2 - 1) / (c * d))^2) # all.equal(log(2), 0.693147181)

    #drs addition to time to germinate dependent on mean January temperature and soil water potential
    temp <- 1 / temp +
            k1_meanJanTemp * TmeanJan +
            k2_meanJanTempXIncubationTemp * TmeanJan * Tgerm +
            k3_IncubationSWP * SWPgerm
    Estimate_TimeToGerminate <- max(1, round(temp))

    #break if convergence or not enough time in this year
    if (abs(Estimate_TimeToGerminate - Estimate_TimeToGerminate.oldEstimate) <= rec.delta |
        RYdoy + Estimate_TimeToGerminate - 1 > 365)
      break
  }

  out <- if (nrec >= nrec.max) {
      round(mean(c(Estimate_TimeToGerminate, Estimate_TimeToGerminate.oldEstimate)), 0)
    } else {
      Estimate_TimeToGerminate
    }

  if (out <= durations[RYdoy] & RYdoy + out <= 365) out else NA #test whether enough time to germinate
}

#' Function to estimate time to germinate for each day of a given year and conditions (temperature, top soil SWP)
#' @param seed A seed set, \code{NULL}, or \code{NA}. \code{NA} will not affect
#'  the state of the RNG; \code{NULL} will re-initialize the RNG; and all other values
#'  are passed to \code{\link{set.seed}}.
calculate_TimeToGerminate_modifiedHardegree2006NLR <- function(RYyear,
  Germination_DuringFavorableConditions, LengthDays_FavorableConditions,
  RYyear_ForEachUsedDay, soilTmeanSnow, swp.TopMean, TmeanJan, param, seed = NA) {

  if (!is.na(seed)) set.seed(seed)
  runifs <- runif(2)

  #values for current year
  index.year <- RYyear_ForEachUsedDay == RYyear
  conditions <- Germination_DuringFavorableConditions[index.year]

  # determining time to germinate for every day
  a <- max(SFSW2_glovars[["tol"]], param$Hardegree_a)
  b <- param$Hardegree_b
  temp <- if (param$Hardegree_d == 1) {
      if (runifs[1] > 0.5) {
        1 + SFSW2_glovars[["tol"]]
      } else {
        1 - SFSW2_glovars[["toln"]]
      }
    } else {
      param$Hardegree_d
    }
  d <- max(SFSW2_glovars[["tol"]], temp)
  temp.c <- if (param$Hardegree_c != 0) param$Hardegree_c else sign(runifs[2] - 0.5) * SFSW2_glovars[["tol"]]

  TimeToGerminate.favorable <- unlist(lapply(which(conditions), get_modifiedHardegree2006NLR,
    Estimate_TimeToGerminate = 1, TmeanJan = TmeanJan, a = a, b = b, c = temp.c, d = d,
    k1_meanJanTemp = param$TimeToGerminate_k1_meanJanTemp,
    k2_meanJanTempXIncubationTemp = param$TimeToGerminate_k2_meanJanTempXIncubationTemp,
    k3_IncubationSWP = param$TimeToGerminate_k3_IncubationSWP,
    Tgerm.year = soilTmeanSnow[index.year],
    SWPgerm.year = swp.TopMean[index.year],
    durations = LengthDays_FavorableConditions[index.year]))  #consequences of unfavorable conditions coded in here

  res <- rep(NA, length(conditions))
  if (length(TimeToGerminate.favorable) > 0) {
      res[conditions] <- TimeToGerminate.favorable
  }

  res
}

do.vector <- function(kill.vector, max.duration.before.kill) {
  doys <- seq_along(kill.vector)
  doys[!kill.vector] <- NA  #calculate only for kill days
  temp.rle <- rle(kill.vector)

  if (sum(!temp.rle$values) > 0) {
    temp.startdoy <- (1 + c(0, cumsum(temp.rle$lengths)))[!temp.rle$values]
    temp.rle$values <- if (temp.rle$values[1]) {
        rep(temp.startdoy, each = 2)
      } else {
        rep(temp.startdoy[-1], each = 2)
      }
    temp.rle$values <- temp.rle$values[seq_along(temp.rle$lengths)]

  } else {#every day is kill free
    temp.rle$values <- length(kill.vector) + 1
  }
  kill.durations <- inverse.rle(temp.rle) - doys
  mortality <- rep(FALSE, times = length(kill.vector))
  mortality[kill.durations > max.duration.before.kill] <- TRUE

  mortality
}

#' Function to calculate mortality under conditions and checks survival limit
calculate_SeedlingMortality_ByCondition <- function(kill.conditions, max.duration.before.kill) {
  if (length(dim(kill.conditions)) > 0) { #i.e., is.matrix, columns = soil layers
    apply(kill.conditions, 2, do.vector, max.duration.before.kill)
  } else {
    do.vector(kill.conditions, max.duration.before.kill)
  }
}


#' Function to calculate favorable conditions for seedling growth for each day of a given year
calculate_SuitableGrowthThisYear_UnderCondition <- function(favorable.conditions, consequences.unfavorable) {
  out <- rep(NA, times = length(favorable.conditions))

  if (consequences.unfavorable == 0) {
    #if conditions become unfavorable, then stop growth for rest of season
    temp.rle <- rle(favorable.conditions)
    temp.firstFavorable.index <- which(temp.rle$values)[1]

    if (!is.na(temp.firstFavorable.index) && temp.firstFavorable.index < length(temp.rle$values)) {
      temp.rle$values[(temp.firstFavorable.index+1):length(temp.rle$values)] <- FALSE
      out <- inverse.rle(temp.rle)
    } else { #nothing changed, either because all days are either favorable or unfavorable or because first favorable period is also the last in the season
      out <- favorable.conditions
    }

  } else if (consequences.unfavorable == 1) {
    #if conditions become unfavorable, then resume growth afterwards
    out <- favorable.conditions
  }

  out
}


#' Function to calculate rooting depth at given age
SeedlingRootingDepth <- function(age, P0, K, r) {
  depth <- K * P0 * exp(r * age) / (K + P0 * (exp(r * age) - 1))  #[age] = days, [P0, K, r] = mm

  pmax(0, depth) / 10 # units = cm
}


get.DoyAtLevel <- function(x, level) {
  which(x == level & x > 0)
}

get.DoyMostFrequentSuccesses <- function(doys, data) {
  res1.max <- sapply(1:2, function(x) stats::quantile(doys[doys[, x] > 0, x], probs = c(0.1, 1), type = 3)) # must return one of the values because the quantiles are compared against the values in function 'get.DoyAtLevel'
  germ.doy <- if (all(!data[, 1])) {
      #no successful germination
      list(NA, NA)
    } else {
      lapply(1:2, function(x) get.DoyAtLevel(doys[, 1], res1.max[x, 1]))
    }
  sling.doy <- if (all(!data[, 2])) {
      #no successful seedlings
      list(NA, NA)
    } else {
      lapply(1:2, function(x) get.DoyAtLevel(doys[, 2], res1.max[x, 2]))
    }
  res1.max <- list(germ.doy, sling.doy)

  unlist(lapply(res1.max, function(x) c(min(x[[1]]), stats::median(x[[2]]), max(x[[1]]))))
}

#------ End of GISSM functions
########################
