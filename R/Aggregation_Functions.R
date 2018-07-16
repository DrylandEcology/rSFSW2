
#' Generate field names (and their numbers) for overall aggregation options
#'
#' @param aon A named logical list. Names are the output options.
#' @param opt_agg A named list. Additional information for aggregation
#'   functions.
#'
#' @section Notes: All '.' will be translated to "_" because of \var{sqlite}
#'   field name constraints.
#'
#' @return A data.frame with two columns \code{N} and \code{fields}. Each
#'   (named) row corresponds to one element of \code{aon}. \code{N} represent
#'   the number of output fields and \code{fields} contains a list with the
#'   names of those output fields.
#' @export
generate_OverallAggregation_fields <- function(aon, opt_agg) { # nolint
  field_args <- list(
    aon = aon,
    opt_agg = opt_agg,

    fieldtag_SWPcrit_MPa = paste0(
      abs(round(-1000 * opt_agg[["SWPcrit_MPa"]], 0)), "kPa"),
    fieldtag_Tmin_crit_C = paste0(ifelse(opt_agg[["Tmin_crit_C"]] < 0, "Neg",
      ifelse(opt_agg[["Tmin_crit_C"]] > 0, "Pos", "")),
      abs(opt_agg[["Tmin_crit_C"]]), "C"),
    fieldtag_Tmax_crit_C = paste0(ifelse(opt_agg[["Tmax_crit_C"]] < 0, "Neg",
      ifelse(opt_agg[["Tmax_crit_C"]] > 0, "Pos", "")),
      abs(opt_agg[["Tmax_crit_C"]]), "C"),
    fieldtag_Tmean_crit_C = paste0(ifelse(opt_agg[["Tmean_crit_C"]] < 0, "Neg",
      ifelse(opt_agg[["Tmean_crit_C"]] > 0, "Pos", "")),
      abs(opt_agg[["Tmean_crit_C"]]), "C")
  )

  aon_names <- names(aon)

  x <- lapply(seq_along(aon), function(k)
    do.call(paste0("fields_", aon_names[k]), args = field_args))

  res <- data.frame(N = sapply(x, function(x) x[["N"]]), fields = NA)
  rownames(res) <- sapply(x, function(x) x[["aon"]])

  for (k in seq_along(aon)) {
    res[k, "fields"][[1]] <- x[[k]][["fields"]]
  }

  res
}


coerce_sqlNames <- function(x) {
  gsub("\\.", "_", x)
}


#0.
fields_input_SoilProfile <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "input_SoilProfile"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("SWinput.Soil.", c("maxDepth_cm", "soilLayers_N",
      "topLayers.Sand_fraction", "bottomLayers.Sand_fraction",
      "topLayers.Clay_fraction", "bottomLayers.Clay_fraction",
      "topLayers.Gravel_fraction", "bottomLayers.Gravel_fraction", "deltaX"))
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#1.
fields_input_FractionVegetationComposition <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "input_FractionVegetationComposition"

  if (isTRUE(aon[[id]])) {
    vtemps <- c("Grasses", "Shrubs", "Trees", "Forbs")
    temp <- paste0("SWinput.Composition.", c(vtemps, "BareGround",
      "C3ofGrasses", "C4ofGrasses", "AnnualsofGrasses"), "_fraction_const")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#2b
fields_input_VegetationBiomassMonthly <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "input_VegetationBiomassMonthly"

  if (isTRUE(aon[[id]])) {
    vtemp <- c("Grass", "Shrub", "Tree", "Forb")
    temp <- paste0(rep(vtemp, each = 36L), "_",
      c(rep("Litter", 12), rep("TotalBiomass", 12), rep("LiveBiomass", 12)),
      "_m", SFSW2_glovars[["st_mo"]], "_gPERm2")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#2b
fields_input_VegetationBiomassTrends <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "input_VegetationBiomassTrends"

  if (isTRUE(aon[[id]])) {
    vtemp <- c("Grass", "Shrub", "Tree", "Forb")
    temp <- paste0(rep(c(vtemp, "Total"), 2), "_",
      rep(c("Total", "Live"), each = 5), "Biomass_gPERm2_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#3.
fields_input_VegetationPeak <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "input_VegetationPeak"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("SWinput.PeakLiveBiomass_", c("month_mean",
      "months_duration"))
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#4.
fields_input_Phenology <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "input_Phenology"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("SWinput.GrowingSeason.", c("Start", "End"), "_month_const")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#5.
fields_input_TranspirationCoeff <- function(aon, opt_agg, ...) { # nolint
  temp <- NULL
  id <- "input_TranspirationCoeff"

  if (isTRUE(aon[[id]])) {
    if (opt_agg[["doy_slyrs"]][["do"]]) {
      ltemp <- paste0("L0to", opt_agg[["doy_slyrs"]][["first_cm"]], "cm")

      if (is.null(opt_agg[["doy_slyrs"]][["second_cm"]])) {
        ltemp <- c(ltemp, paste0("L", opt_agg[["doy_slyrs"]][["first_cm"]],
          "toSoilDepth"))
      } else if (is.numeric(opt_agg[["doy_slyrs"]][["second_cm"]])) {
        ltemp <- c(ltemp, paste0("L", opt_agg[["doy_slyrs"]][["first_cm"]],
          "to", opt_agg[["doy_slyrs"]][["second_cm"]], "cm"))
      }

      if (is.null(opt_agg[["doy_slyrs"]][["third_cm"]])) {
        ltemp <- c(ltemp, paste0("L", opt_agg[["doy_slyrs"]][["second_cm"]],
          "toSoilDepth"))
      } else if (is.na(opt_agg[["doy_slyrs"]][["third_cm"]])) {
      } else if (is.numeric(opt_agg[["doy_slyrs"]][["third_cm"]])) {
        ltemp <- c(ltemp, paste0("L", opt_agg[["doy_slyrs"]][["second_cm"]],
          "to", opt_agg[["doy_slyrs"]][["third_cm"]], "cm"))
      }

      if (is.null(opt_agg[["doy_slyrs"]][["fourth_cm"]])) {
        ltemp <- c(ltemp, paste0("L", opt_agg[["doy_slyrs"]][["third_cm"]],
          "toSoilDepth"))
      } else if (is.na(opt_agg[["doy_slyrs"]][["fourth_cm"]])) {
      } else if (is.numeric(opt_agg[["doy_slyrs"]][["fourth_cm"]])) {
        ltemp <- c(ltemp, paste0("L", opt_agg[["doy_slyrs"]][["third_cm"]],
          "to", opt_agg[["doy_slyrs"]][["fourth_cm"]], "cm"))
      }

      ltemp <- c(ltemp, paste0("NA",
        (length(ltemp) + 1):SFSW2_glovars[["slyrs_maxN"]]))

    } else {
      ltemp <- paste0("L", formatC(SFSW2_glovars[["slyrs_ids"]], width = 2,
        format = "d", flag = "0"))
    }

    vtemp <- c("Grass", "Shrub", "Tree", "Forb")
    temp <- c(
      paste0("SWinput.", rep(vtemp, each = SFSW2_glovars[["slyrs_maxN"]]),
        ".TranspirationCoefficients.", rep(ltemp, times = 4), "_fraction"),
      paste0("SWinput.", rep(vtemp, each = 2),
        ".TranspirationCoefficients.",
        rep(c("topLayer", "bottomLayer"), times = 4), "_fraction"))
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#6.
fields_input_ClimatePerturbations <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "input_ClimatePerturbations"

  if (isTRUE(aon[[id]])) {
    temp <- paste0(rep(paste0("SWinput.ClimatePerturbations.",
      c("PrcpMultiplier.m", "TmaxAddand.m", "TminAddand.m")), each = 12),
      SFSW2_glovars[["st_mo"]], rep(c("_none", "_C", "_C"), each = 12),
      "_const")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#6b
fields_input_CO2Effects <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "input_CO2Effects"

  if (isTRUE(aon[[id]])) {
    vtemp <- c("Grass", "Shrub", "Tree", "Forb")
    temp <- paste0(rep(vtemp, 2), "_",
      rep(c("Biomass", "WUE"), each = 4), "_CO2multiplier_fraction_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#------Aggregation: Climate and weather---###################################

#7.
fields_yearlyTemp <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "yearlyTemp"

  if (isTRUE(aon[[id]])) {
    temp <- "MAT_C_mean"
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#8.
fields_yearlyPPT <- function(aon, opt_agg, ...) {
  temp <- NULL
  id <- "yearlyPPT"

  if (isTRUE(aon[[id]])) {
    temp <- c("MAP_mm_mean", "SnowOfPPT_fraction_mean")

    if (isTRUE(opt_agg[["use_doy_range"]])) {
      ranges <- if (is.null(opt_agg$doy_ranges$yearlyPPT)) {
        c(opt_agg$doy_ranges$default)
      } else {
        c(opt_agg$doy_ranges$yearlyPPT)
      }

      temp <- c(temp,
        paste0("MAP_mm_doyRange", ranges[1], "to", ranges[2], "_mean"),
        paste0("SnowOfPPT_fraction_doyRange", ranges[1], "to", ranges[2],
          "_mean"))
    }
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#9. #10.
fields_dailySnowpack <- function(aon, opt_agg, ...) {
  temp <- NULL
  id <- "dailySnowpack"

  if (isTRUE(aon[[id]])) {
    temp <- c("RainOnSnowOfMAP_fraction_mean",
      paste0("Snowcover.NSadj.", c("Peak_doy", "LongestContinuous.FirstDay_doy",
        "LongestContinuous.LastDay_doy", "LongestContinuous.Duration_days",
        "Total_days", "Peak_mmSWE", "SnowCover.FirstDay_doy",
        "SnowCover.LastDay_doy"), "_mean"))

    if (isTRUE(opt_agg[["use_doy_range"]])) {
      # because of current implementation, can't determine DOYs for WaterYears
      # at this point
      temp <- c(temp, paste0("Snowcover.NSadj.",
        c("Peak_doy", "Total_days", "Peak_mmSWE"), "_doyRange_mean"))
    }
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#11
fields_dailyFrostInSnowfreePeriod <- function(aon, opt_agg,  # nolint
  fieldtag_Tmin_crit_C, ...) {

  temp <- NULL
  id <- "dailyFrostInSnowfreePeriod"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("TminBelow", rep(fieldtag_Tmin_crit_C, each = 3),
      c("withoutSnow", "withoutSpringSnow", "withoutFallSnow"), "_days_mean")

    if (isTRUE(opt_agg[["use_doy_range"]])) {
      # because of current implementation, can't determine DOYs for WaterYears
      # at this point
      temp <- c(temp, paste0("TminBelow", fieldtag_Tmin_crit_C,
        "withoutSnow_doyRange_mean"))
    }
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#12
fields_dailyHotDays <- function(aon, fieldtag_Tmax_crit_C, ...) {
  temp <- NULL
  id <- "dailyHotDays"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("TmaxAbove", fieldtag_Tmax_crit_C, "_days_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#12b
fields_dailyWarmDays <- function(aon, fieldtag_Tmean_crit_C, ...) {
  temp <- NULL
  id <- "dailyWarmDays"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("TmeanAbove", fieldtag_Tmean_crit_C, "_days_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#12c
fields_dailyColdDays <- function(aon, fieldtag_Tmin_crit_C, ...) {
  temp <- NULL
  id <- "dailyColdDays"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("TminSurfaceBelow", fieldtag_Tmin_crit_C, "_days_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#12d
fields_dailyCoolDays <- function(aon, fieldtag_Tmean_crit_C, ...) {
  temp <- NULL
  id <- "dailyCoolDays"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("TminSurfaceBelow", fieldtag_Tmean_crit_C, "_days_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#13
fields_dailyPrecipitationEventSizeDistribution <- function(aon, opt_agg, ...) {  # nolint
  temp <- NULL
  id <- "dailyPrecipitationEventSizeDistribution"

  if (isTRUE(aon[[id]])) {
    bins.summary <- (0:6) * opt_agg[["bin_prcp_mm"]]
    temp <- paste0("PrcpEvents.Annual", c("_count", paste0(".SizeClass",
      bins.summary, "to", c(bins.summary[-1], "Inf"), "mm_fraction")), "_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#15
fields_yearlyPET <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "yearlyPET"

  if (isTRUE(aon[[id]])) {
    temp <- "PET_mm_mean"
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#16
fields_monthlySeasonalityIndices <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "monthlySeasonalityIndices"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("Seasonality.monthly", c("PETandSWPtopLayers",
    "PETandSWPbottomLayers", "TandPPT"), "_PearsonCor_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}


#---Aggregation: Climatic dryness
#17
fields_yearlymonthlyTemperateDrylandIndices <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "yearlymonthlyTemperateDrylandIndices"

  if (isTRUE(aon[[id]])) {
    temp1 <- c("UNAridityIndex", "TrewarthaD", "TemperateDryland12")
    temp <- paste0(c(paste0(temp1, ".Normals"), paste0(temp1, ".Annual")),
      rep(c("_none", "_TF", "_TF"), times = 2), "_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#18
fields_yearlyDryWetPeriods <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "yearlyDryWetPeriods"

  if (isTRUE(aon[[id]])) {
    temp <- paste0(c("Dry", "Wet"),
      "SpellDuration.90PercentEvents.ShorterThan_years_quantile0.9")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#19
fields_dailyWeatherGeneratorCharacteristics <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "dailyWeatherGeneratorCharacteristics"

  if (isTRUE(aon[[id]])) {
    temp <- paste0(rep(c("WetSpellDuration", "DrySpellDuration",
      "TempAir.StDevOfDailyValues"), each = 12), ".m", SFSW2_glovars[["st_mo"]],
      rep(c("_days", "_days", "_C"), each = 12), "_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#20
fields_dailyPrecipitationFreeEventDistribution <- function(aon, opt_agg, ...) {  # nolint
  temp <- NULL
  id <- "dailyPrecipitationFreeEventDistribution"

  if (isTRUE(aon[[id]])) {
    bins.summary <- (0:3) * opt_agg[["bin_prcpfree_days"]]
    temp <- paste0("DrySpells.Annual", c("_count", paste0(".SizeClass",
      bins.summary + 1, "to", c(bins.summary[-1], "365"), "days_fraction")),
      "_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#21
fields_monthlySPEIEvents <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "monthlySPEIEvents"

  if (isTRUE(aon[[id]])) {
    binSPEI_m <- c(1, 12, 24, 48) #months
    probs <- c(0.025, 0.5, 0.975)
    temp <- NULL

    for (iscale in seq_along(binSPEI_m)) {
      rvec <- rep(NA, times = 4 * length(probs))
      temp <- c(temp, paste0(rep(paste0("SPEI.", binSPEI_m[iscale],
        "monthsScale."), length(rvec)), "Spell",
        rep(c("Pos.", "Neg."), each = 2 * length(probs)),
        rep(rep(c("Duration_months", "Value_none"), each = length(probs)),
          times = 2), "_quantile", rep(probs, times = 4)))
    }
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#---Aggregation: Climatic control
#22
fields_monthlyPlantGrowthControls <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "monthlyPlantGrowthControls"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("NemaniEtAl2003.NPPControl.",
      c("Temperature", "Water", "Radiation"), "_none_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#23
fields_dailyC4_TempVar <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "dailyC4_TempVar"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("TeeriEtAl1976.NSadj.", c("TempAirMin.7thMonth_C",
      "FreezeFreeGrowingPeriod_days", "AccumDegreeDaysAbove65F_daysC"), "_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#24
fields_dailyDegreeDays <- function(aon, opt_agg, ...) {
  temp <- NULL
  id <- "dailyDegreeDays"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("DegreeDays.Base", opt_agg[["Tbase_DD_C"]],
      "C.dailyTmean_Cdays_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#25
fields_dailyColdDegreeDays <- function(aon, opt_agg, ...) {
  temp <- NULL
  id <- "dailyColdDegreeDays"

  if (isTRUE(aon[[id]])) {
    temp <- paste0(c("ColdDegreeDays", "ColdDegreeDays.SnowFree"), ".Base.",
      ifelse(opt_agg[["Tbase_coldDD_C"]] < 0, "Neg",
        ifelse(opt_agg[["Tbase_coldDD_C"]] > 0, "Pos", "")),
      abs(opt_agg[["Tbase_coldDD_C"]]), "C.dailyTMean_Cdays_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#------Aggregation: Yearly water balance---##################################

#27.0
fields_yearlyAET <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "yearlyAET"

  if (isTRUE(aon[[id]])) {
    temp <- "AET_mm_mean"
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#27
fields_yearlyWaterBalanceFluxes <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "yearlyWaterBalanceFluxes"

  if (isTRUE(aon[[id]])) {
    temp <- paste0(c(paste0(c("Rain", "Rain.ReachingSoil", "Snowfall",
      "Snowmelt", "Snowloss", "Interception.Total", "Interception.Vegetation",
      "Interception.Litter", "Infiltration", "Runoff", "Runon",
      "Evaporation.Total", "Evaporation.SurfaceWater",
      "Evaporation.InterceptedByVegetation",
      "Evaporation.InterceptedByLitter", "Evaporation.Soil.Total",
      "Evaporation.Soil.topLayers", "Evaporation.Soil.bottomLayers",
      "Transpiration.Total", "Transpiration.topLayers",
      "Transpiration.bottomLayers",
      "HydraulicRedistribution.TopToBottom", "Percolation.TopToBottom",
      "DeepDrainage", "SWC.StorageChange"), "_mm"),
      "TranspirationBottomToTranspirationTotal_fraction", "TtoAET", "EStoAET",
      "AETtoPET", "TtoPET", "EStoPET"), "_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#27.1
fields_yearlyTranspirationBySoilLayer <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "yearlyTranspirationBySoilLayer"

  if (isTRUE(aon[[id]])) {
    vegtypes <- c("total", "tree", "shrub", "forb", "grass")

    temp <- paste0("Transpiration_",
      rep(vegtypes, each = SFSW2_glovars[["slyrs_maxN"]]),
      "_L", SFSW2_glovars[["slyrs_ids"]], "_mm_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#27.2
fields_dailySoilWaterPulseVsStorage <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "dailySoilWaterPulseVsStorage"

  if (isTRUE(aon[[id]])) {
    temp <- c(paste0("WaterExtractionSpell_MeanContinuousDuration_L",
      SFSW2_glovars[["slyrs_ids"]], "_days_mean"),
      paste0("WaterExtractionSpell_AnnualSummedExtraction_L",
        SFSW2_glovars[["slyrs_ids"]], "_mm_mean"))
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#------Aggregation: Daily extreme values---##################################
#28
fields_dailyTranspirationExtremes <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "dailyTranspirationExtremes"

  if (isTRUE(aon[[id]])) {
    temp <- c(paste0("Transpiration.", c("DailyMax", "DailyMin"), "_mm_mean"),
      paste0("Transpiration.", c("DailyMax", "DailyMin"), "_doy_mean"))
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#29
fields_dailyTotalEvaporationExtremes <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "dailyTotalEvaporationExtremes"

  if (isTRUE(aon[[id]])) {
    temp <- c(
      paste0("Evaporation.Total.", c("DailyMax", "DailyMin"), "_mm_mean"),
      paste0("Evaporation.Total.", c("DailyMax", "DailyMin"), "_doy_mean"))
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#30
fields_dailyDrainageExtremes <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "dailyDrainageExtremes"

  if (isTRUE(aon[[id]])) {
    temp <- c(paste0("DeepDrainage.", c("DailyMax", "DailyMin"), "_mm_mean"),
      paste0("DeepDrainage.", c("DailyMax", "DailyMin"), "_doy_mean"))
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#31
fields_dailyInfiltrationExtremes <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "dailyInfiltrationExtremes"

  if (isTRUE(aon[[id]])) {
    temp <- c(paste0("Infiltration.", c("DailyMax", "DailyMin"), "_mm_mean"),
      paste0("Infiltration.", c("DailyMax", "DailyMin"), "_doy_mean"))
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#32
fields_dailyAETExtremes <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "dailyAETExtremes"

  if (isTRUE(aon[[id]])) {
    temp <- c(paste0("AET.", c("DailyMax", "DailyMin"), "_mm_mean"),
      paste0("AET.", c("DailyMax", "DailyMin"), "_doy_mean"))
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#33
fields_dailySWPextremes <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "dailySWPextremes"

  if (isTRUE(aon[[id]])) {
    temp <- paste0(paste0("SWP.",
      rep(c("topLayers.", "bottomLayers."), each = 2),
      rep(c("DailyMax", "DailyMin"), times = 2)),
      rep(c("_MPa_mean", "_doy_mean"), each = 4))
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#34
fields_dailyRechargeExtremes <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "dailyRechargeExtremes"

  if (isTRUE(aon[[id]])) {
    temp <- paste0(paste0("RelRecharge.",
      rep(c("topLayers.", "bottomLayers."), each = 2),
      rep(c("DailyMax", "DailyMin"), times = 2)),
      rep(c("_Fraction_mean", "_doy_mean"), each = 4))
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}


#------Aggregation: Ecological dryness---####################################

#35a
#' @section Abbreviations: \itemize{
#'   \item GT = greater than
#'   \item LT = less than
#'   \item EQ = equal
#'   \item MCS = MoistureControlSection
#'   \item ACS = AnhydrousControlSection
#'   \item consec = consecutive
#' }
fields_dailyNRCS_SoilMoistureTemperatureRegimes_Intermediates <- function(aon,  # nolint
  ...) {

  temp <- NULL
  id <- "dailyNRCS_SoilMoistureTemperatureRegimes_Intermediates"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("NRCS_",
      c(c("SoilTemp_simulated_TF", "SoilTemp_realistic_TF",
        "Depth50cmOrImpermeable_cm",
        "MCS_Upper_cm", "MCS_Lower_cm",
        "ACS_Upper_cm", "ACS_Lower_cm",
        "Permafrost_years", "SMR_normalyears_N", "Soil_with_Ohorizon_TF"),
        # MATLanh, MAT50:
        paste0(c("SoilTemp_ACS_Annual_C", "SoilTemp_at50cm_Annual_C",
          "SoilTemp_at50cm_JJA_C", "SoilTemp_at50cm_DJF_C", # T50jja, T50djf
          "Saturation_ConsecutiveMaxDuration_JJA_days", # CSPartSummer
          "SoilTemp_Offset_from_MeanAirTemp_C", # meanTair_Tsoil50_offset_C
          # Anhydrous_annual_means:
          "COND1_ACS_at50cm_LE0C_prob", # COND1
          "COND2_ACS_atAnhDepth_LE5C_prob", # COND2
          # COND3:
          "COND3_ACS_MoreThanHalfDry_and_at50cm_GT0C_isGThalf_at50cm_GT0C_prob",
          # HalfDryDaysCumAbove0C:
          "COND3_ACS_MoreThanHalfDry_and_at50cm_GT0C_days",
          "COND3_ACS_at50cm_GT0C_days", # SoilAbove0C
          "COND3_ACS_at50cm_GT0C_prob", # T50_at0C
          "COND3_ACS_MoreThanHalfDry_prob", # Lanh_Dry_Half
          "COND3_ACS_MoreThanHalfDry_and_at50cm_GT0C_prob", # COND3_Test
          # MCS_annual_means:
          "COND0_mPPT_GT_mPET_prob", # COND0
          "COND1_MCS_AllDry_and_at50cm_GT5C_days", # DryDaysCumAbove5C
          "COND1_MCS_at50cm_GT5C_days", # SoilAbove5C
          "COND1_MCS_AllDry_and_at50cm_GT5C_isGThalf_at50cm_GT5C_prob", # COND1
          # MaxContDaysAnyMoistCumAbove8:
          "COND2_MCS_AnyWetConsec_Max_at50cm_GT8C_days",
          "COND2_MCS_AnyWetConsec_LT90Days_at50cm_GT8C_prob", # COND2
          "COND2-1_MCS_AnyWetConsec_LT180Days_at50cm_GT8C_prob", # COND2_1
          "COND2-2_MCS_AnyWetConsec_LT270Days_at50cm_GT8C_prob", # COND2_2
          "COND2-3_MCS_AnyWetConsec_LE45Days_at50cm_GT8C_prob", # COND2_3
          "COND3_MCS_AnyDry_days", # DryDaysCumAny
          "COND3_MCS_AnyDryTotal_LT90Days_prob", # COND3
          "COND3-1_MCS_AnyDryTotal_LT30Days_prob", # COND3_1
          "COND4_MCS_at50cm_GT22C_prob", # COND4
          "COND5_MCS_at50cm_DiffJJAtoDJF_C", # AbsDiffSoilTemp_DJFvsJJA
          "COND5_MCS_at50cm_DiffJJAtoDJF_GT6C_prob", # COND5
          "COND6_MCS_AllDry_Summer_days",  # DryDaysConsecSummer
          "COND6_MCS_AllDry_Summer_LT45Days_prob", # COND6
          "COND6-1_MCS_AllDry_Summer_GT90Days_prob", # COND6_1
          "COND7_MCS_AnyMoist_GT180Days_days", # MoistDaysCumAny
          "COND7_MCS_AnyMoist_GT180Days_prob", # COND7
          "COND8_MCS_AnyWetConsec_days", # MoistDaysConsecAny
          "COND8_MCS_AnyWetConsec_GT90Days_prob", # COND8
          "COND9_MCS_AllWet_Winter_days", # MoistDaysConsecWinter
          "COND9_MCS_AllWet_Winter_GT45days_prob", # COND9
          "COND10_MCS_AllDry_days", # AllDryDaysCumAny
          "COND10_MCS_AllDry_prob", # COND10

          "Days_at50cm_GT5C_prob", "Days_at50cm_GT8C_prob",
          "Days_MCS_AllWet_prob",
          "COND1_MCS_AllDry_and_at50cm_GT5C_prob", # COND1_Test
          "COND2_MCS_AnyWet_and_at50cm_GT8C_prob"), # COND2_Test
          "_mean")))
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#' @seealso
#' \code{\link{fields_dailyNRCS_SoilMoistureTemperatureRegimes_Intermediates}}
fields_dailyNRCS_SoilMoistureTemperatureRegimes <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "dailyNRCS_SoilMoistureTemperatureRegimes"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("NRCS_", c(
      paste0("SoilTemperatureRegime_", STR_names()),
      paste0("SoilMoistureRegime_", SMR_names()),
      paste0("SoilMoistureRegimeQualifier_", SMRq_names())))
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#35b
fields_dailyNRCS_Chambers2014_ResilienceResistance <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "dailyNRCS_Chambers2014_ResilienceResistance"

  if (isTRUE(aon[[id]])) {
    cats <- c("Low", "ModeratelyLow", "Moderate", "ModeratelyHigh", "High")
    temp <- paste0("NRCS_Chambers2014_Sagebrush",
      rep(c("Resilience", "Resistance"), each = length(cats)), "_", cats)
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#35c
fields_dailyNRCS_Maestas2016_ResilienceResistance <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "dailyNRCS_Maestas2016_ResilienceResistance"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("NRCS_Maestas2016_SagebrushRR_",
      c("Low", "Moderate", "High"))
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#35.2
fields_dailyWetDegreeDays <- function(aon, opt_agg, fieldtag_SWPcrit_MPa, ...) {
  temp <- NULL
  id <- "dailyWetDegreeDays"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("WetDegreeDays.SWPcrit", rep(fieldtag_SWPcrit_MPa, each = 3),
      rep(c(".topLayers", ".bottomLayers", ".anyLayer"),
      times = opt_agg[["SWPcrit_N"]]), "_Cdays_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#35.3
fields_dailyThermalDrynessStartEnd <- function(aon, fieldtag_SWPcrit_MPa, ...) {  # nolint
  temp <- NULL
  id <- "dailyThermalDrynessStartEnd"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("ThermalDrySoilPeriods_SWPcrit",
      rep(fieldtag_SWPcrit_MPa, each = 4), "_NSadj_",
      rep(c("topLayers", "bottomLayers"), each = 2), "_",
      rep(c("Start", "End"), times = 2), "_LongestContinuous_days_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#35.4
fields_dailyThermalSWPConditionCount <- function(aon, opt_agg,  # nolint
  fieldtag_SWPcrit_MPa, fieldtag_Tmean_crit_C, ...) {

  temp <- NULL
  id <- "dailyThermalSWPConditionCount"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("SoilPeriods_Warm",
      rep(paste0(rep(c("Dry", "Wet"), times = 3), "_",
        rep(c("allLayers", "topLayer", "bottomLayer"), each = 2)),
        each = length(opt_agg[["Tmean_crit_C"]]) * opt_agg[["SWPcrit_N"]]),
      "_Tcrit",
      rep(fieldtag_Tmean_crit_C, times = opt_agg[["SWPcrit_N"]]), "_SWPcrit",
      rep(fieldtag_SWPcrit_MPa, each = length(opt_agg[["Tmean_crit_C"]])),
      "_Count_days_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#36
fields_monthlySWPdryness <- function(aon, opt_agg, fieldtag_SWPcrit_MPa, ...) {
  temp <- NULL
  id <- "monthlySWPdryness"

  if (isTRUE(aon[[id]])) {
    temp <- c(paste0("DrySoilPeriods.SWPcrit",
      rep(fieldtag_SWPcrit_MPa, times = 2), ".NSadj.",
      rep(c("topLayers", "bottomLayers"), each = opt_agg[["SWPcrit_N"]]),
      ".Duration.Total_months_mean"),
      paste0("DrySoilPeriods.SWPcrit", rep(fieldtag_SWPcrit_MPa, times = 2),
        ".NSadj.",
        rep(c("topLayers", "bottomLayers"), each = opt_agg[["SWPcrit_N"]]),
        ".Start_month_mean"))
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#37
fields_dailySWPdrynessANDwetness <- function(aon, fieldtag_SWPcrit_MPa, ...) {  # nolint
  temp <- NULL
  id <- "dailySWPdrynessANDwetness"

  if (isTRUE(aon[[id]])) {
    temp <- paste0(rep(c("WetSoilPeriods", "DrySoilPeriods"), each = 8),
      ".SWPcrit", rep(fieldtag_SWPcrit_MPa, each = 16), ".NSadj.",
      c(rep(c("topLayers", "bottomLayers"), times = 4),
        rep(rep(c("topLayers", "bottomLayers"), each = 2), times = 2)),
      rep(c(".AnyLayerWet.", ".AllLayersWet.", ".AllLayersDry.", ""), each = 4),
      c(rep(rep(c("Duration.Total_days", "Duration.LongestContinuous_days"),
        each = 2), times = 2),
        rep(c("Duration.Total_days", "Duration.LongestContinuous_days"),
          times = 2),
        rep(c(".PeriodsForAtLeast10Days.Start_doy",
          ".PeriodsForAtLeast10Days.End_doy"), times = 2)),
      "_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#38
fields_dailySuitablePeriodsDuration <- function(aon, opt_agg,  # nolint
  fieldtag_SWPcrit_MPa, ...) {

  temp <- NULL
  id <- "dailySuitablePeriodsDuration"

  if (isTRUE(aon[[id]])) {
    quantiles <- c(0.05, 0.5, 0.95)
    temp <- paste0("ThermalSnowfreeWetPeriods.SWPcrit",
      rep(paste0(rep(fieldtag_SWPcrit_MPa, each = 2),
        rep(c(".topLayers", ".bottomLayers"), times = opt_agg[["SWPcrit_N"]])),
        each = length(quantiles)), "_Duration_days_quantile",
      rep(quantiles, times = 2))
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#39
fields_dailySuitablePeriodsAvailableWater <- function(aon, opt_agg,  # nolint
  fieldtag_SWPcrit_MPa, ...) {

  temp <- NULL
  id <- "dailySuitablePeriodsAvailableWater"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("ThermalSnowfreeWetPeriods.SWPcrit",
      rep(fieldtag_SWPcrit_MPa, each = 2),
      rep(c(".topLayers", ".bottomLayers"), times = opt_agg[["SWPcrit_N"]]),
      "_AvailableWater_mm_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#40
fields_dailySuitablePeriodsDrySpells <- function(aon, opt_agg,  # nolint
  fieldtag_SWPcrit_MPa, ...) {

  temp <- NULL
  id <- "dailySuitablePeriodsDrySpells"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("ThermalSnowfreeDryPeriods.SWPcrit",
      rep(paste0(rep(fieldtag_SWPcrit_MPa, each = 2),
        rep(c(".topLayers", ".bottomLayers"), times = opt_agg[["SWPcrit_N"]])),
        each = 4),
      c("_DrySpellsAllLayers_meanDuration_days_mean",
        "_DrySpellsAllLayers_maxDuration_days_mean",
        "_DrySpellsAllLayers_Total_days_mean",
        "_DrySpellsAtLeast10DaysAllLayers_Start_doy_mean"))
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#41
fields_dailySWPdrynessDurationDistribution <- function(aon, opt_agg,  # nolint
  fieldtag_SWPcrit_MPa, ...) {

  temp <- NULL
  id <- "dailySWPdrynessDurationDistribution"

  if (isTRUE(aon[[id]])) {
    quantiles <- (0:4) / 4
    season.flag <- c("DJF", "MAM", "JJA", "SON")

    temp <- paste0("DrySoilPeriods.SWPcrit",
      rep(rep(fieldtag_SWPcrit_MPa, each = 2 * length(quantiles)),
        times = length(season.flag)), ".Month",
      rep(season.flag, each = 2 * length(quantiles) * opt_agg[["SWPcrit_N"]]),
      ".",
      rep(rep(paste0(rep(c("topLayers", "bottomLayers"),
        each = length(quantiles)), ".Duration_days_quantile",
        rep(quantiles, times = 2)), times = opt_agg[["SWPcrit_N"]]),
        times = length(season.flag)))
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#42
fields_dailySWPdrynessEventSizeDistribution <- function(aon,  # nolint
  fieldtag_SWPcrit_MPa, ...) {

  temp <- NULL
  id <- "dailySWPdrynessEventSizeDistribution"

  if (isTRUE(aon[[id]])) {
    # binSize: closed interval lengths in [days] within a year
    binSize <- c(1, 8, 15, 29, 57, 183, 367)
    binsN <- length(binSize) - 1
    binTitle <- paste0("SizeClass",
      paste(binSize[-length(binSize)], binSize[-1] - 1, sep = "to"), "days")

    temp <- paste0("DrySoilPeriods.SWPcrit",
      rep(fieldtag_SWPcrit_MPa, each = 2 * (binsN + 1)), ".Annual.",
      rep(c("topLayers", "bottomLayers"), each = binsN + 1),
      rep(c("_count", paste0(".", binTitle, "_fraction")), times = 2),
      "_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#43
fields_dailySWPdrynessIntensity <- function(aon, fieldtag_SWPcrit_MPa, ...) {  # nolint
  temp <- NULL
  id <- "dailySWPdrynessIntensity"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("DrySoilPeriods.SWPcrit",
      rep(fieldtag_SWPcrit_MPa, each = 4 * 2), ".MissingWater.",
      rep(c("topLayers", "bottomLayers"), each = 4), ".",
      rep(c("AnnualSum_mmH2O", "PerEventPerDay_mmH2O", "Duration.Event_days",
        "Events_count"), times = 2),
      "_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#43.2
fields_dailyThermalDrynessStress <- function(aon, opt_agg,  # nolint
  fieldtag_SWPcrit_MPa, ...) {

  temp <- NULL
  id <- "dailyThermalDrynessStress"

  if (isTRUE(aon[[id]])) {
    extremes <- c("Hottest", "Coldest")
    resp <- c("Days_VPD_kPa", "Days_Temp_C", "SnowfreeDays_Temp_C")
    aggs <- c(rep("mean", length(resp)), "max", "min", "min")
    Naggs <- 2
    soils <- c("allLayers", "topLayer", "bottomLayer")
    Nout <- length(resp) * Naggs * opt_agg[["SWPcrit_N"]]

    temp <- c(
      paste0("Mean10", rep(extremes, each = length(resp) * Naggs), resp, "_",
        aggs),
      paste0("Mean10", rep(extremes, each = Nout),
        rep(resp, each = opt_agg[["SWPcrit_N"]]),
        "_MoistureStress_SWPcrit", fieldtag_SWPcrit_MPa, "_",
        rep(soils, each = Nout * length(extremes)), "_",
        rep(aggs, each = opt_agg[["SWPcrit_N"]])))
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#43.3
fields_periodicVWCmatricFirstLayer <- function(aon, opt_agg, ...) {  # nolint
  temp <- NULL
  id <- "periodicVWCmatricFirstLayer"

  if (isTRUE(aon[[id]])) {
    if (isTRUE(opt_agg$use_doy_range)) {
      ranges <- if (is.null(opt_agg$doy_ranges$periodicVWCmatric)) {
          c(opt_agg$doy_ranges$default)
        } else {
          c(opt_agg$doy_ranges$periodicVWCmatric)
        }

      temp <- c(temp,
        paste0("periodicVWCmatricMean_FirstLayer_doyRange", ranges[1], "to",
          ranges[2], "_mean"),
        paste0("periodicVWCmatricSum_FirstLayer_doyRange", ranges[1], "to",
          ranges[2], "_mean"))
    }
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#------Aggregation: Mean monthly values---###################################

#44
fields_monthlyTemp <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "monthlyTemp"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("TempAir.m", SFSW2_glovars[["st_mo"]], "_C_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#45
fields_monthlyPPT <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "monthlyPPT"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("Precip.m", SFSW2_glovars[["st_mo"]], "_mm_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#46
fields_monthlySnowpack <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "monthlySnowpack"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("Snowpack.m", SFSW2_glovars[["st_mo"]], "_mmSWE_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#47
fields_monthlySoilTemp <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "monthlySoilTemp"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("TempSoil.", c(
      paste0("topLayers.m", SFSW2_glovars[["st_mo"]]),
      paste0("bottomLayers.m", SFSW2_glovars[["st_mo"]])), "_C_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#48
fields_monthlyRunoff <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "monthlyRunoff"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("Runoff.Total.m", SFSW2_glovars[["st_mo"]], "_mm_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

fields_monthlyRunon <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "monthlyRunon"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("Runon.Total.m", SFSW2_glovars[["st_mo"]], "_mm_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#49
fields_monthlyHydraulicRedistribution <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "monthlyHydraulicRedistribution"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("HydraulicRedistribution.",
      c(paste0("topLayers.m", SFSW2_glovars[["st_mo"]]),
        paste0("bottomLayers.m", SFSW2_glovars[["st_mo"]])), "_mm_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#50
fields_monthlyInfiltration <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "monthlyInfiltration"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("Infiltration.m", SFSW2_glovars[["st_mo"]], "_mm_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#51
fields_monthlyDeepDrainage <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "monthlyDeepDrainage"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("DeepDrainage.m", SFSW2_glovars[["st_mo"]], "_mm_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#52
fields_monthlySWPmatric <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "monthlySWPmatric"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("SWPmatric.", c(
      paste0("topLayers.m", SFSW2_glovars[["st_mo"]]),
      paste0("bottomLayers.m", SFSW2_glovars[["st_mo"]])), "_MPa_FromVWCmean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#53 a.)
fields_monthlyVWCbulk <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "monthlyVWCbulk"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("VWCbulk.", c(
      paste0("topLayers.m", SFSW2_glovars[["st_mo"]]),
      paste0("bottomLayers.m", SFSW2_glovars[["st_mo"]])), "_mPERm_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#53 b.)
fields_monthlyVWCmatric <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "monthlyVWCmatric"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("VWCmatric.", c(
      paste0("topLayers.m", SFSW2_glovars[["st_mo"]]),
      paste0("bottomLayers.m", SFSW2_glovars[["st_mo"]])), "_mPERm_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#54
fields_monthlySWCbulk <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "monthlySWCbulk"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("SWCbulk.", c(
      paste0("topLayers.m", SFSW2_glovars[["st_mo"]]),
      paste0("bottomLayers.m", SFSW2_glovars[["st_mo"]])), "_mm_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#55
fields_monthlySWAbulk <- function(aon, fieldtag_SWPcrit_MPa, ...) {
  temp <- NULL
  id <- "monthlySWAbulk"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("SWAbulk_", "SWPcrit",
      rep(fieldtag_SWPcrit_MPa, each = 24), "_",
      c(paste0("topLayers_m", SFSW2_glovars[["st_mo"]]),
        paste0("bottomLayers_m", SFSW2_glovars[["st_mo"]])),
      "_mm_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#56
fields_monthlyTranspiration <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "monthlyTranspiration"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("Transpiration.", c(
      paste0("topLayers.m", SFSW2_glovars[["st_mo"]]),
      paste0("bottomLayers.m", SFSW2_glovars[["st_mo"]])), "_mm_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#57
fields_monthlySoilEvaporation <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "monthlySoilEvaporation"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("Evaporation.Soil.m", SFSW2_glovars[["st_mo"]], "_mm_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#58
fields_monthlyAET <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "monthlyAET"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("AET.m", SFSW2_glovars[["st_mo"]], "_mm_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#59
fields_monthlyPET <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "monthlyPET"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("PET.m", SFSW2_glovars[["st_mo"]], "_mm_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#59.2
fields_monthlyVPD <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "monthlyVPD"

  if (isTRUE(aon[[id]])) {
    temp <- paste0("VPD_m", SFSW2_glovars[["st_mo"]], "_kPa_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#60
fields_monthlyAETratios <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "monthlyAETratios"

  if (isTRUE(aon[[id]])) {
    temp <- paste0(rep(c("TranspToAET.m", "EvapSoilToAET.m"), each = 12),
      SFSW2_glovars[["st_mo"]], "_fraction_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#61
fields_monthlyPETratios <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "monthlyPETratios"

  if (isTRUE(aon[[id]])) {
    temp <- paste0(rep(c("TranspToPET.m", "EvapSoilToPET.m"), each = 12),
      SFSW2_glovars[["st_mo"]], "_fraction_mean")
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#------Aggregation: Potential regeneration---################################

#62
fields_dailyRegeneration_bySWPSnow <- function(aon, ...) { # nolint
  temp <- NULL
  id <- "dailyRegeneration_bySWPSnow"

  if (isTRUE(aon[[id]])) {
    temp <- "Regeneration.Potential.SuitableYears.NSadj_fraction_mean"
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}

#63
fields_dailyRegeneration_GISSM <- function(aon, opt_agg, ...) {
  temp <- NULL
  id <- "dailyRegeneration_GISSM"

  if (isTRUE(aon[[id]])) {

    SlingMortality_ByYear_cnames <- paste0(
      "Seedlings1stSeason.Mortality.",
      c("UnderneathSnowCover", "ByTmin", "ByTmax", "ByChronicSWPMax",
        "ByChronicSWPMin", "ByAcuteSWPMin", "DuringStoppedGrowth.DueSnowCover",
        "DuringStoppedGrowth.DueTmin", "DuringStoppedGrowth.DueTmax"))
    temp1 <- c("Germination", "Seedlings1stSeason")

    for (sp in seq_len(opt_agg[["GISSM_species_No"]])) {
      fields.header1 <- c(paste0(temp1, ".SuitableYears_fraction_mean"),
        paste0(rep(temp1, each = 3),
          ".UnsuitableYears.Successive_years_quantile", rep(c(0.05, 0.5, 0.95),
          times = 2)),
        paste0(temp1, ".SuitableDaysPerYear_days_mean"),
        paste0(paste0(rep(temp1, each = 3), ".", c("Start", "Middle", "End")),
          "_doy_quantile", rep(c(0.9, 0.5, 0.9), times = 2)),
        paste0("Germination.RestrictedDays.By", c("Tmax", "Tmin", "SWPmin",
          "AnyCondition", "TimeToGerminate"), "_days_mean"),
        "Germination.TimeToGerminate_days_mean",
        paste0(SlingMortality_ByYear_cnames, "_days_mean"))

      temp <- c(temp, paste(colnames(opt_agg[["GISSM_params"]])[sp],
        fields.header1, sep = "."))

      #Output for time series: not yet implemented for db
    }
  }

  list(aon = id, N = length(temp), fields = list(coerce_sqlNames(temp)))
}
