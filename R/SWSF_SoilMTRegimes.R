########################
#------ SMTR functions

# Based on references provided by Chambers, J. C., D. A. Pyke, J. D. Maestas, M. Pellant, C. S. Boyd, S. B. Campbell, S. Espinosa, D. W. Havlina, K. E. Mayer, and A. Wuenschel. 2014. Using Resistance and Resilience Concepts to Reduce Impacts of Invasive Annual Grasses and Altered Fire Regimes on the Sagebrush Ecosystem and Greater Sage-Grouse: A Strategic Multi-Scale Approach. Gen. Tech. Rep. RMRS-GTR-326. U.S. Department of Agriculture, Forest Service, Rocky Mountain Research Station, Fort Collins, CO.
#

#' Categories of soil temperature regimes and soil moisture regimes
#'
#' @section Definitions: Soil temperature and moisture regimes are defined in
#'  SSS 2014. Our operationalization is explained in the vignette
#'  'SoilMoistureRegimes_SoilTemperatureRegimes'.
#'
#' @references Soil Survey Staff. 2014. Keys to soil taxonomy, 12th ed., USDA Natural
#'  Resources Conservation Service, Washington, DC.
#' @examples
#'  vignette("SoilMoistureRegimes_SoilTemperatureRegimes", package = "rSWSF")
#' @name STMR
NULL

#' Soil temperature regime categories
#' @rdname STMR
#' @export
STR_names <- function() {
  c("Hyperthermic", "Thermic", "Mesic", "Frigid", "Cryic", "Gelic")
}

#' Soil moisture regime categories
#' @rdname STMR
#' @export
SMR_names <- function() {
  c("Anhydrous", "Aridic", "Xeric", "Ustic", "Udic", "Perudic", "Aquic")
}

#' Soil moisture regime categories including qualifiers
#' @rdname STMR
#' @export
SMRq_names <- function() {
  c("Extreme-Aridic", "Typic-Aridic", "Weak-Aridic", #Aridic
    "Dry-Xeric", "Typic-Xeric", # Xeric
    "Typic-Tempustic", "Xeric-Tempustic", "Wet-Tempustic", "Aridic-Tropustic",
    "Typic-Tropustic", "Udic-Ustic", # Ustic
    "Typic-Udic", "Dry-Tropudic", "Dry-Tempudic") # Udic
}

#' Soil temperature regime: based on Soil Survey Staff 2014 (Key to Soil Taxonomy): p.31
#'  we ignore distinction between iso- and not iso-
#' @export
STR_logic <- function(MAST, MSST, SatSoilSummer_days, has_permafrost, has_Ohorizon) {
  temp <- STR_names()
  Tregime <- rep(0, length(temp))
  names(Tregime) <- temp

  if (MAST >= 22) {
      Tregime["Hyperthermic"] <- 1L
  } else if (MAST >= 15) {
      Tregime["Thermic"] <- 1L
  } else if (MAST >= 8) {
      Tregime["Mesic"] <- 1L

  } else if (MAST > 0 && !has_permafrost) {
    # mineral soils
    if (SatSoilSummer_days > 0) {
      # "soil is saturated with water during some part of summer"
      if (has_Ohorizon) { # TODO: should be: 'O-horizon' OR 'histic epipedon'
        if (MSST < 6) {
          Tregime["Cryic"] <- 1L
        } else {
          Tregime["Frigid"] <- 1L
        }
      } else {
        if (MSST < 13) {
          Tregime["Cryic"] <- 1L
        } else {
          Tregime["Frigid"] <- 1L
        }
      }

    } else {
      # "not saturated with water during some part of the summer"
      if (has_Ohorizon) {
        if (MSST < 8) {
          Tregime["Cryic"] <- 1L
        } else {
          Tregime["Frigid"] <- 1L
        }
      } else {
        if (MSST < 15) {
          Tregime["Cryic"] <- 1L
        } else {
          Tregime["Frigid"] <- 1L
        }
      }
    }
    # TODO: else organic soils: cryic if mean(T50jja) > 0 C and < 6 C

  } else if (MAST <= 0 || has_permafrost) {
    # limit should be 1 C for Gelisols
    Tregime["Gelic"] <- 1L
  }

  Tregime
}


#' Soil moisture regime: Soil Survey Staff 2014 (Key to Soil Taxonomy): p.28-31
#' @export
SMR_logic <- function(ACS_COND1, ACS_COND2, ACS_COND3, MCS_COND0,
  MCS_COND1, MCS_COND2, MCS_COND2_1, MCS_COND2_2, MCS_COND2_3, MCS_COND3, MCS_COND3_1,
  MCS_COND4, MCS_COND5, MCS_COND6, MCS_COND6_1, MCS_COND7, MCS_COND8, MCS_COND9,
  MCS_COND10, has_permafrost) {

  temp <- c(SMR_names(), SMRq_names())
  Sregime <- rep(0, length(temp))
  names(Sregime) <- temp

  #Anhydrous condition: Soil Survey Staff 2010: p.16/Soil Survey Staff 2014: p.18
  #we ignore test for 'ice-cemented permafrost' and 'rupture-resistance class'
  if (ACS_COND1 && ACS_COND2 && ACS_COND3)
    Sregime["Anhydrous"] <- 1L

  # We ignore 'Aquic' because we have no information on soil oxygen content

  if (MCS_COND0) {
    # Perudic soil moisture regime
    Sregime["Perudic"] <- 1L

  } else if (MCS_COND1 && MCS_COND2) {
    # Aridic soil moisture regime; The limits set for soil temperature exclude from these soil moisture regimes soils in the very cold and dry polar regions and in areas at high elevations. Such soils are considered to have anhydrous condition
    Sregime["Aridic"] <- 1L

    # Qualifier for aridic SMR
    if (MCS_COND10) {
      Sregime["Extreme-Aridic"] <- 1L
    } else if (MCS_COND2_3) {
      # NOTE: COND2_3: assumes that 'MaxContDaysAnyMoistCumAbove8' is equivalent to jNSM variable 'ncpm[2]'
      Sregime["Typic-Aridic"] <- 1L
    } else {
      Sregime["Weak-Aridic"] <- 1L
    }

  } else if (!MCS_COND6 && MCS_COND9 && !MCS_COND4 && MCS_COND5) {
    # Xeric soil moisture regime
    Sregime["Xeric"] <- 1L

    #Qualifier for xeric SMR
    if (MCS_COND6_1) {
      # NOTE: this conditional assumes that 'DryDaysConsecSummer' is equivalent to jNSM variable 'nccd'
      Sregime["Dry-Xeric"] <- 1L
    } else {
      Sregime["Typic-Xeric"] <- 1L
    }

  } else if (MCS_COND3 && (!MCS_COND4 && MCS_COND5 && MCS_COND6 || (MCS_COND4 || !MCS_COND5))) {
    # Udic soil moisture regime - #we ignore test for 'three- phase system' during T50 > 5
    Sregime["Udic"] <- 1L

    # Qualifier for udic SMR
    if (MCS_COND3_1) {
      Sregime["Typic-Udic"] <- 1L
    } else if (!MCS_COND5) {
      Sregime["Dry-Tropudic"] <- 1L
    } else {
      Sregime["Dry-Tempudic"] <- 1L
    }

  } else if (!has_permafrost && !MCS_COND3 &&
      ((MCS_COND4 || !MCS_COND5) && (MCS_COND7 || MCS_COND8) || !MCS_COND4 && MCS_COND5 &&
      !MCS_COND1 && (MCS_COND9 && MCS_COND6 || !MCS_COND9))) {
    # Ustic soil moisture regime
    Sregime["Ustic"] <- 1L

    # Qualifier for ustic SMR
    if (MCS_COND5) {
      if (!MCS_COND9) {
        # NOTE: this conditional assumes that 'MoistDaysConsecWinter' is equivalent to jNSM variable 'nccm'
        Sregime["Typic-Tempustic"] <- 1L
      } else if (!MCS_COND6) {
        # NOTE: this conditional assumes that 'DryDaysConsecSummer' is equivalent to jNSM variable 'nccd'
        Sregime["Xeric-Tempustic"] <- 1L
      } else {
        Sregime["Wet-Tempustic"] <- 1L
      }
    } else {
      # NOTE: COND2_1 and COND2_2: assume that 'MaxContDaysAnyMoistCumAbove8' is equivalent to jNSM variable 'ncpm[2]'
      if (MCS_COND2_1) {
        Sregime["Aridic-Tropustic"] <- 1L
      } else if (MCS_COND2_2) {
        Sregime["Typic-Tropustic"] <- 1L
      } else {
        Sregime["Udic-Ustic"] <- 1L
      }
    }
  }

  Sregime
}


#------ End of SMTR functions
########################
