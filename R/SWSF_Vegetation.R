
#' Calculate the composition of a potential natural vegetation based of shrub, C3 grass,
#'  and C4 grass functional group components using climate relationships
#'
#' Equations by Paruelo & Lauenroth 1996 are used to estimate shrub, C3 grass, and C3
#'  grass components. Equations by Teeri & Stowe 1976 are used to limit occurrence of
#'  C4 grasses.
#'
#' @param MAP_mm A numeric value. Mean annual precipitation in millimeter of the location.
#' @param MAT_C A numeric value. Mean annual temperature in degree Celcius.
#' @param mean_monthly_ppt_mm A numeric vector of length 12. Mean monthly precipitation
#'  in millimeter.
#' @param dailyC4vars A named numeric vector of length 3. \describe{
#'    \item{\code{Month7th_NSadj_MinTemp_C}}{Mean minimum temperature of July on the
#'      northern hemisphere and January on the southern hemisphere}
#'    \item{\code{DegreeDaysAbove65F_NSadj_DaysC}}{Degree days above 65 F = 18.33 C in units
#'      of days x degree Celcius}
#'    \item{\code{LengthFreezeFreeGrowingPeriod_NSadj_Days}}{Mean annual number of days
#'      of the longest continuous period where minimum daily temperature remain above
#'      freezing}
#'  }
#' @param isNorth A logical value. \code{TRUE} for locations on northern hemisphere.
#' @param shrub_limit A numeric value. Default value is 0.2 based on page 1213 of
#'  Paruelo & Lauenroth 1996.
#' @param fix_annuals A logical value. If \code{TRUE}, then value for the annual component
#'  is fixed at \code{Annuals_Fraction}.
#' @param Annuals_Fraction A numeric value. Default value is 0. A value between 0 and 1.
#' @param fix_C4grasses A logical value. If \code{TRUE}, then value for the C4-grass
#'  component is fixed at \code{C4_Fraction} instead of calculated from climatic
#'  relationships.
#' @param C4_Fraction A numeric value between 0 and 1. \code{NA} is treated as if
#'  \code{fix_C4grasses} is \code{FALSE}.
#' @param fix_C3grasses A logical value. If \code{TRUE}, then value for the C3-grass
#'  component is fixed at \code{C3_Fraction} instead of calculated from climatic
#'  relationships.
#' @param C3_Fraction A numeric value between 0 and 1. \code{NA} is treated as if
#'  \code{fix_C3grasses} is \code{FALSE}.
#' @param fix_shrubs A logical value. If \code{TRUE}, then value for the shrub
#'  component is fixed at \code{Shrubs_Fraction} instead of calculated from climatic
#'  relationships.
#' @param Shrubs_Fraction A numeric value between 0 and 1. \code{NA} is treated as if
#'  \code{fix_shrubs} is \code{FALSE}.
#' @param fix_forbs A logical value. If \code{TRUE}, then value for the forb component
#'  is fixed at \code{Forbs_Fraction}.
#' @param Forbs_Fraction A numeric value. Default value is 0. A value between 0 and 1.
#' @param fix_BareGround A logical value. If \code{TRUE}, then value for the bare ground
#'  component is fixed at \code{BareGround_Fraction}.
#' @param BareGround_Fraction A numeric value. Default value is 0. A value between 0 and 1.
#'
#' @return A list with two named numeric vectors. \describe{
#'    \item{Composition}{Relative composition [0-1] of the vegetation for \code{Grasses},
#'      \code{Shrubs}, \code{Trees}, \code{Forbs}, and \code{BareGround}. \code{Grasses}
#'      are the sum of C3-grasses, C4-grasses, and annuals functional groups. \code{Trees}
#'      is set to zero. The sum of \code{Composition} is 1.}
#'  \item{grasses.c3c4ann.fractions}{Relative contribution [0-1] of the C3-grasses,
#'      C4-grasses, and annuals functional groups. The sum of
#'      \code{grasses.c3c4ann.fractions} is 1.}
#' }
#'
#' @references Paruelo JM, Lauenroth WK (1996) Relative abundance of plant functional
#'  types in grasslands and shrublands of North America. Ecological Applications, 6,
#'  1212-1224.
#' @references Teeri JA, Stowe LG (1976) Climatic patterns and the distribution of C4
#'  grasses in North America. Oecologia, 23, 1-12.
#'
#' @export
PotNatVeg_Composition_Estimate_ShrubsC3C4_Fraction <- function(MAP_mm, MAT_C,
  mean_monthly_ppt_mm, dailyC4vars, isNorth = TRUE, shrub_limit = 0.2,
  fix_annuals = TRUE, Annuals_Fraction = 0, fix_C4grasses = FALSE, C4_Fraction = NA,
  fix_C3grasses = FALSE, C3_Fraction = NA, fix_shrubs = FALSE, Shrubs_Fraction = NA,
  fix_forbs = FALSE, Forbs_Fraction = NA, fix_BareGround = TRUE, BareGround_Fraction = 0) {

  f.digits <- 3
  tolerance <- 1.1*10^-f.digits

  #Get the user specified fractions, if column is false set to NA
  tree.fraction <- 0 #option 'PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996' doesn't estimate tree cover, i.e., assumed to be == 0
  forb.fraction <- 0
  bareGround.fraction <- 0
  AnnC4C3ShrubForbBareGroundFraction <- rep(NA, 6)
  if(fix_annuals){
    AnnC4C3ShrubForbBareGroundFraction[1] <- finite01(Annuals_Fraction)
  } else {
    AnnC4C3ShrubForbBareGroundFraction[1] <- 0 #Annuals can not be NA
  }
  if(fix_C4grasses)
    AnnC4C3ShrubForbBareGroundFraction[2] <- C4_Fraction
  if(fix_C3grasses)
    AnnC4C3ShrubForbBareGroundFraction[3] <- C3_Fraction
  if(fix_shrubs)
    AnnC4C3ShrubForbBareGroundFraction[4] <- Shrubs_Fraction

  if(fix_forbs) {
    AnnC4C3ShrubForbBareGroundFraction[5] <- finite01(Forbs_Fraction)
  } else {
    AnnC4C3ShrubForbBareGroundFraction[5] <- forb.fraction
  }
  if(fix_BareGround) {
    AnnC4C3ShrubForbBareGroundFraction[6] <- finite01(BareGround_Fraction)
  } else {
    AnnC4C3ShrubForbBareGroundFraction[6] <- bareGround.fraction
  }
  AnnC4C3ShrubForbBareGroundFraction <- cut0Inf(AnnC4C3ShrubForbBareGroundFraction) #treat negatives as if NA
  TotalFraction <- sum(AnnC4C3ShrubForbBareGroundFraction, na.rm=TRUE)

  #Decide if all fractions are sufficiently defined or if they need to be calculated based on climate variables
  if(!isTRUE(all.equal(TotalFraction, 1, tolerance=tolerance)) && TotalFraction < 1 && sum(is.na(AnnC4C3ShrubForbBareGroundFraction)) == 0) {
    stop(print(paste(" run: User defined fractions of Shrub, C3, C4, Annuals are all set, but less than 1", sep=""))) #throw an error
  }

  if(isTRUE(all.equal(TotalFraction, 1, tolerance=tolerance)) || TotalFraction > 1 || sum(is.na(AnnC4C3ShrubForbBareGroundFraction)) == 1){

    if(sum(is.na(AnnC4C3ShrubForbBareGroundFraction)) == 1){ #if only one is NA, then this can be calculated
      AnnC4C3ShrubForbBareGroundFraction[which(is.na(AnnC4C3ShrubForbBareGroundFraction))] <- cut0Inf(1 - TotalFraction)
    } else {
      AnnC4C3ShrubForbBareGroundFraction <- finite01(AnnC4C3ShrubForbBareGroundFraction) #the composition is >= 1, so set eventually remaining NA to 0
    }

    TotalFraction <- sum(AnnC4C3ShrubForbBareGroundFraction, na.rm=TRUE)
    AnnC4C3ShrubForbBareGroundFraction <- AnnC4C3ShrubForbBareGroundFraction / TotalFraction #Rescale, in case it is needed

  } else { #i.e., (TotalFraction < 1 && sum(is.na(AnnC4C3ShrubForbBareGroundFraction)) > 1) is TRUE; thus, calculate some fractions based on climate variables
    if(isNorth){ #Northern hemisphere
      Months_WinterTF <- c(12, 1:2)
      Months_SummerTF <- c(6:8)
    } else {
      Months_WinterTF <- c(6:8)
      Months_SummerTF <- c(12, 1:2)
    }
    ppt.SummerToMAP <- sum(mean_monthly_ppt_mm[Months_SummerTF]) / MAP_mm
    ppt.WinterToMAP <- sum(mean_monthly_ppt_mm[Months_WinterTF]) / MAP_mm

    #---Potential natural vegetation
    #1. step: Paruelo JM, Lauenroth WK (1996) Relative abundance of plant functional types in grasslands and shrublands of North America. Ecological Applications, 6, 1212-1224.
    if(MAP_mm < 1){
      shrubs.fractionNA <- NA
    } else {
      shrubs.fractionNA <- cut0Inf(1.7105 - 0.2918 * log(MAP_mm) + 1.5451 * ppt.WinterToMAP) 								#if NA, then not enough winter precipitation above a given MAP
    }
    if(MAT_C <= 0){
      grass.c4.fractionNA <- 0
    } else {
      grass.c4.fractionNA <- cut0Inf(-0.9837 + 0.000594 * MAP_mm + 1.3528 * ppt.SummerToMAP + 0.2710 * log(MAT_C))			#if NA, then either MAT < 0 or not enough summer precipitation or too cold below a given MAP
    }
    if(ppt.WinterToMAP <= 0){
      grass.c3ingrasslands.fractionNA <- grass.c3inshrublands.fractionNA <- NA
    } else {
      grass.c3ingrasslands.fractionNA <- cut0Inf(1.1905 - 0.02909 * MAT_C + 0.1781 * log(ppt.WinterToMAP) - 0.2383 * 1)		#if NA, then not enough winter precipitation or too warm below a given MAP
      grass.c3inshrublands.fractionNA <- cut0Inf(1.1905 - 0.02909 * MAT_C + 0.1781 * log(ppt.WinterToMAP) - 0.2383 * 2)
    }
    grass.c3.fractionNA <- ifelse(shrubs.fractionNA >= shrub_limit && !is.na(shrubs.fractionNA), grass.c3inshrublands.fractionNA, grass.c3ingrasslands.fractionNA)

    grass.Annual.fraction <- AnnC4C3ShrubForbBareGroundFraction[1] #Ann will be 0 or something <= 1

    #2. step: Teeri JA, Stowe LG (1976) Climatic patterns and the distribution of C4 grasses in North America. Oecologia, 23, 1-12.
    #This equations give percent species/vegetation -> use to limit Paruelo's C4 equation, i.e., where no C4 species => there are no C4 abundance > 0
    if(dailyC4vars["LengthFreezeFreeGrowingPeriod_NSadj_Days"] <= 0){
      grass.c4.species <- 0
    } else {
      x10 <- dailyC4vars["Month7th_NSadj_MinTemp_C"] * 9/5 + 32
      x13 <- dailyC4vars["DegreeDaysAbove65F_NSadj_DaysC"] * 9/5
      x18 <- log(dailyC4vars["LengthFreezeFreeGrowingPeriod_NSadj_Days"])
      grass.c4.species <- as.numeric((1.60 * x10 + 0.0086 * x13 - 8.98 * x18 - 22.44) / 100)
    }
    grass.c4.fractionNA <- ifelse(grass.c4.species >= 0, grass.c4.fractionNA, NA)

    #3. step: Replacing missing values: If no or only one successful equation, then add 100% C3 if MAT < 10 C, 100% shrubs if MAP < 600 mm, and 100% C4 if MAT >= 10C & MAP >= 600 mm	[these rules are made up arbitrarily by drs, Nov 2012]
    if(sum(!is.na(shrubs.fractionNA), !is.na(grass.c4.fractionNA), !is.na(grass.c3.fractionNA)) <= 1){
      if(MAP_mm < 600) shrubs.fractionNA <- 1 + ifelse(is.na(shrubs.fractionNA), 0, shrubs.fractionNA)
      if(MAT_C < 10)  grass.c3.fractionNA <- 1 + ifelse(is.na(grass.c3.fractionNA), 0, grass.c3.fractionNA)
      if(MAT_C >= 10  & MAP_mm >= 600)  grass.c4.fractionNA <- 1 + ifelse(is.na(grass.c4.fractionNA), 0, grass.c4.fractionNA)
    }

    #4. step: Scale fractions to 0-1 with a sum of 1 including grass.Annual.fraction, but don't scale grass.Annual.fraction
    #if na then use calc fraction else use the user defined fraction
    shrubs.fraction <- NAto0(shrubs.fractionNA)
    grass.c4.fraction <- NAto0(grass.c4.fractionNA)
    grass.c3.fraction <- NAto0(grass.c3.fractionNA)

    sumVegWithoutAnnuals <- shrubs.fraction + grass.c4.fraction + grass.c3.fraction
    shrubs.fraction <- (shrubs.fraction / sumVegWithoutAnnuals) * (1 - grass.Annual.fraction) #scale these down to 1-annual fraction
    grass.c4.fraction <- (grass.c4.fraction / sumVegWithoutAnnuals) * (1 - grass.Annual.fraction)
    grass.c3.fraction <- (grass.c3.fraction / sumVegWithoutAnnuals) * (1 - grass.Annual.fraction)

    calcAnnC4C3ShrubForbBareGroundFraction <- c(grass.Annual.fraction, grass.c4.fraction, grass.c3.fraction, shrubs.fraction)
    naIndex <- which(is.na(AnnC4C3ShrubForbBareGroundFraction))
    #replace missing values
    if(isTRUE(all.equal(sum(calcAnnC4C3ShrubForbBareGroundFraction[naIndex]), 0)) && isTRUE(all.equal(temp <- sum(AnnC4C3ShrubForbBareGroundFraction[!naIndex]), 0))){ #there would be no vegetation, so force vegetation > 0
      AnnC4C3ShrubForbBareGroundFraction[naIndex] <- (1 - temp) / length(naIndex)
    } else {
      AnnC4C3ShrubForbBareGroundFraction[naIndex] <- calcAnnC4C3ShrubForbBareGroundFraction[naIndex]
    }
    #now we need to get the sum and scale the naIndex values accordingly
    AnnC4C3ShrubForbBareGroundFraction[naIndex] <- sapply(AnnC4C3ShrubForbBareGroundFraction[naIndex], function(x) (x/sum(AnnC4C3ShrubForbBareGroundFraction[naIndex])) * (1-sum(AnnC4C3ShrubForbBareGroundFraction[-naIndex])))
  }

  #Scale Grass components to one (or set to 0)
  if(!isTRUE(all.equal(sum(AnnC4C3ShrubForbBareGroundFraction[4:6]), 1))){
    grass.c4.fractionG <- AnnC4C3ShrubForbBareGroundFraction[2] / (1-sum(AnnC4C3ShrubForbBareGroundFraction[4:6]))
    grass.c3.fractionG <- AnnC4C3ShrubForbBareGroundFraction[3] / (1-sum(AnnC4C3ShrubForbBareGroundFraction[4:6]))
    grass.Annual.fractionG <- AnnC4C3ShrubForbBareGroundFraction[1] / (1-sum(AnnC4C3ShrubForbBareGroundFraction[4:6]))
  } else {
    grass.c4.fractionG <- grass.c3.fractionG <- grass.Annual.fractionG <- 0
  }
  grass.fraction <- sum(AnnC4C3ShrubForbBareGroundFraction[c(1:3)])

  list(
    Composition = c(
      Grasses = grass.fraction,
      Shrubs = AnnC4C3ShrubForbBareGroundFraction[4],
      Trees = tree.fraction,
      Forbs = AnnC4C3ShrubForbBareGroundFraction[5],
      BareGround = AnnC4C3ShrubForbBareGroundFraction[6]),

    grasses.c3c4ann.fractions = c(
      C3 = grass.c3.fractionG,
      C4 = grass.c4.fractionG,
      Annuals = grass.Annual.fractionG)
  )
}

predict_season <- function(biomass_Standard, std.season.padded, std.season.seq, site.season.seq) {
  #length(std.season.seq) >= 3 because of padding and test that season duration > 0
  lcoef <- calc.loess_coeff(N = length(std.season.seq), span = 0.4)

  op <- options(c("warn", "error"))
  on.exit(options(op))
  options(warn = -1, error = traceback) #stats::loess throws many warnings: 'pseudoinverse used', see calc.loess_coeff(), etc.

  sapply(apply(biomass_Standard, 2, function(x) {
      lf <- stats::loess(x[std.season.padded] ~ std.season.seq, span = lcoef$span, degree = lcoef$degree)
      stats::predict(lf, newdata = data.frame(std.season.seq = site.season.seq))
    }),
    FUN = function(x) max(0, x)) # guarantee that > 0
}

#' Biomass equations
#' @references Milchunas & Lauenroth 1993 (Fig. 2): Y [g/m2/yr] = c1 * MAP [mm/yr] + c2
#' @name biomass
NULL

#' Estimate shrub biomass density from mean annual precipitation
#' @export
#' @rdname biomass
Shrub_ANPP <- function(MAP_mm) 0.393 * MAP_mm - 10.2

#' Estimate grass biomass density from mean annual precipitation
#' @export
#' @rdname biomass
Grass_ANPP <- function(MAP_mm) 0.646 * MAP_mm - 102.5

adjBiom_by_ppt <- function(biom_shrubs, biom_C3, biom_C4, biom_annuals, biom_maxs,
         map_mm_shrubs, map_mm_std_shrubs,
         map_mm_grasses, map_mm_std_grasses,
         vegcomp_std_shrubs, vegcomp_std_grass) {

  #Intercepts to match outcomes of M & L 1993 equations under 'default' MAP with our previous default inputs for shrubs and sgs-grasslands
  #Whereas these intercepts were introduced artificially, they could also be interpreted as perennial storage, e.g., Lauenroth & Whitman (1977) found "Accumulation in the standing dead was 63% of inputs, in the litter 8%, and belowground 37%.". Lauenroth, W.K. & Whitman, W.C. (1977) Dynamics of dry matter production in a mixed-grass prairie in western North Dakota. Oecologia, 27, 339-351.
  Shrub_ANPPintercept <- (vegcomp_std_shrubs[1] * biom_maxs["Sh.Amount.Live"] +
                          vegcomp_std_shrubs[2] * biom_maxs["C3.Amount.Live"] +
                          vegcomp_std_shrubs[3] * biom_maxs["C4.Amount.Live"]) -
                        Shrub_ANPP(map_mm_std_shrubs)
  Grasses_ANPPintercept <- (vegcomp_std_grass[1] * biom_maxs["Sh.Amount.Live"] +
                            vegcomp_std_grass[2] * biom_maxs["C3.Amount.Live"] +
                            vegcomp_std_grass[3] * biom_maxs["C4.Amount.Live"]) -
                          Grass_ANPP(map_mm_std_grasses)

  #Get scaling values for scaled biomass; guarantee that > minimum.totalBiomass
  minimum.totalBiomass <- 0 #This is a SOILWAT2 parameter
  Shrub_BiomassScaler <- max(minimum.totalBiomass, Shrub_ANPP(map_mm_shrubs) + Shrub_ANPPintercept)
  Grass_BiomassScaler <- max(minimum.totalBiomass, Grass_ANPP(map_mm_grasses) + Grasses_ANPPintercept)

  #Scale live biomass amount by productivity; assumption: ANPP = peak standing live biomass
  biom_shrubs$Sh.Amount.Live <- biom_shrubs$Sh.Amount.Live * Shrub_BiomassScaler
  biom_C3$C3.Amount.Live <- biom_C3$C3.Amount.Live * Grass_BiomassScaler
  biom_C4$C4.Amount.Live <- biom_C4$C4.Amount.Live * Grass_BiomassScaler
  biom_annuals$Annual.Amount.Live <- biom_annuals$Annual.Amount.Live * Grass_BiomassScaler

  #Scale litter amount by productivity and adjust for ratio of litter/live
  biom_shrubs$Sh.Litter <- biom_shrubs$Sh.Litter * Shrub_BiomassScaler * biom_maxs["Sh.Litter"] / biom_maxs["Sh.Amount.Live"]
  biom_C3$C3.Litter <- biom_C3$C3.Litter * Grass_BiomassScaler * biom_maxs["C3.Litter"] / biom_maxs["C3.Amount.Live"]
  biom_C4$C4.Litter <- biom_C4$C4.Litter * Grass_BiomassScaler * biom_maxs["C4.Litter"] / biom_maxs["C4.Amount.Live"]
  biom_annuals$Annual.Litter <- biom_annuals$Annual.Litter * Grass_BiomassScaler * biom_maxs["Annual.Litter"] / biom_maxs["Annual.Amount.Live"]

  #Guarantee that live fraction = ]0, 1]
  biom_shrubs$Sh.Perc.Live <- pmin(1, pmax(swsf_glovars[["tol"]], biom_shrubs$Sh.Perc.Live))
  biom_C3$C3.Perc.Live <- pmin(1, pmax(swsf_glovars[["tol"]], biom_C3$C3.Perc.Live))
  biom_C4$C4.Perc.Live <- pmin(1, pmax(swsf_glovars[["tol"]], biom_C4$C4.Perc.Live))
  biom_annuals$Annual.Perc.Live <- pmin(1, pmax(swsf_glovars[["tol"]], biom_annuals$Annual.Perc.Live))

  #Calculate total biomass based on scaled live biomass amount
  biom_shrubs$Sh.Biomass <- biom_shrubs$Sh.Amount.Live / biom_shrubs$Sh.Perc.Live
  biom_C3$C3.Biomass <- biom_C3$C3.Amount.Live / biom_C3$C3.Perc.Live
  biom_C4$C4.Biomass <- biom_C4$C4.Amount.Live / biom_C4$C4.Perc.Live
  biom_annuals$Annual.Biomass <- biom_annuals$Annual.Amount.Live / biom_annuals$Annual.Perc.Live

  list(biom_shrubs = biom_shrubs,
       biom_C3 = biom_C3,
       biom_C4 = biom_C4,
       biom_annuals = biom_annuals)
}


#' Adjust mean monthly biomass values of grass and shrub functional groups by climate
#'  relationships
#'
#' @param tr_VegBiom A data.frame with 12 rows (one for each month) and columns
#'  \code{X.Biomass}, \code{X.Amount.Live}, \code{X.Perc.Live}, and \code{X.Litter} where
#'  \code{X} are for the functional groups shrubs, \code{X = Sh}; C3-grasses,
#'  \code{X = C3}; C4-grasses, \code{X = C4}; and annuals, \code{X = Annual} containing
#'  default input values.
#' @param do_adjBiom_by_temp A logical value. If \code{TRUE} then monthly phenology is
#'  adjusted by temperature.
#' @param do_adjBiom_by_ppt A logical value. If \code{TRUE} then monthly biomass is
#'  adjusted by precipitation.
#' @param fgrass_c3c4ann A numeric vector of length 3. Relative contribution [0-1] of
#'  the C3-grasses, C4-grasses, and annuals functional groups. The sum of
#'  \code{fgrass_c3c4ann} is 1.
#' @param growing_limit_C A numeric value. Mean monthly temperatures equal or above this
#'  limit are here considered suitable for growth (growing season). Default value is 4 C.
#' @param isNorth A logical value. \code{TRUE} for locations on northern hemisphere.
#' @param MAP_mm A numeric value. Mean annual precipitation in millimeter of the location.
#' @param mean_monthly_temp_C A numeric vector of length 12. Mean monthly temperature
#'  in Celcius. The default inputs considered March-October as growing season.
#'
#' @section Default inputs: \itemize{
#'    \item Shrubs are based on location 'IM_USC00107648_Reynolds' which resulted in a
#'      vegetation composition of 70 \% shrubs and 30 \% C3-grasses. Default monthly
#'      biomass values were estimated for MAP = 450 mm yr-1.
#'    \item Grasses are based on location 'GP_SGSLTER' (shortgrass steppe) which resulted
#'      in 12 \% shrubs, 22 \% C3-grasses, and 66 \% C4-grasses. Default biomass values
#'      were estimated for MAP = 340 mm yr-1.
#'  }
#'
#' @return A list with two elements \code{grass}, \code{shrub}. Each element is a matrix
#'  with 12 rows (one for each month) and columns \code{Biomass}, \code{Amount.Live},
#'  \code{Perc.Live}, and \code{Litter}.
#'
#' @references Bradford, J.B., Schlaepfer, D.R., Lauenroth, W.K. & Burke, I.C. (2014).
#'  Shifts in plant functional types have time-dependent and regionally variable impacts
#'  on dryland ecosystem water balance. J Ecol, 102, 1408-1418.
#'
#' @export
PotNatVeg_MonthlyBiomassPhenology_from_Climate <- function(tr_VegBiom,
  do_adjBiom_by_temp = FALSE, do_adjBiom_by_ppt = FALSE, fgrass_c3c4ann = c(1, 0, 0),
  growing_limit_C = 4, isNorth = TRUE, MAP_mm = 450,
  mean_monthly_temp_C = c(rep(growing_limit_C - 1, 2), rep(growing_limit_C + 1, 8),
    rep(growing_limit_C - 1, 2))) {

  #Default shrub biomass input is at MAP = 450 mm/yr, and default grass biomass input is at MAP = 340 mm/yr
  #Describe conditions for which the default vegetation biomass values are valid
  std.winter <- c(11:12, 1:2) #Assumes that the "growing season" (valid for growing_limit_C == 4) in 'tr_VegetationComposition' starts in March and ends after October, for all functional groups.
  std.growing <- swsf_glovars[["st_mo"]][-std.winter] #Assumes that the "growing season" in 'tr_VegetationComposition' starts in March and ends after October, for all functional groups.
  #Default site for the grass description is SGS LTER
  StandardGrasses_MAP_mm <- 340
  StandardGrasses_VegComposition <- c(0.12, 0.22, 0.66) #Fraction of shrubs, C3, and C4
  #Default site for the shrub description is Reynolds Creek, ID
  StandardShrub_MAP_mm <- 250
  StandardShrub_VegComposition <- c(0.7, 0.3, 0) #Fraction of shrubs, C3, and C4

  #Calculate 'live biomass amount'
  tr_VegBiom$Sh.Amount.Live <- tr_VegBiom$Sh.Biomass * tr_VegBiom$Sh.Perc.Live
  tr_VegBiom$C3.Amount.Live <- tr_VegBiom$C3.Biomass * tr_VegBiom$C3.Perc.Live
  tr_VegBiom$C4.Amount.Live <- tr_VegBiom$C4.Biomass * tr_VegBiom$C4.Perc.Live
  tr_VegBiom$Annual.Amount.Live <- tr_VegBiom$Annual.Biomass * tr_VegBiom$Annual.Perc.Live

  #Scale monthly values of litter and live biomass amount by column-max; total biomass will be back calculated from 'live biomass amount' / 'percent live'
  itemp <- grepl("Litter", names(tr_VegBiom)) | grepl("Amount.Live", names(tr_VegBiom))
  colmax <- apply(tr_VegBiom[, itemp], MARGIN=2, FUN=max)
  #  colmin <- apply(tr_VegBiom[, itemp], MARGIN=2, FUN=min)
  tr_VegBiom[, itemp] <- sweep(tr_VegBiom[, itemp], MARGIN=2, STATS=colmax, FUN="/")

  #Pull different vegetation types
  biom_shrubs <- biom_std_shrubs <- tr_VegBiom[, grepl("Sh", names(tr_VegBiom))]
  biom_C3 <- biom_std_C3 <- tr_VegBiom[, grepl("C3", names(tr_VegBiom))]
  biom_C4 <- biom_std_C4 <- tr_VegBiom[, grepl("C4", names(tr_VegBiom))]
  biom_annuals <- biom_std_annuals <- tr_VegBiom[, grepl("Annual", names(tr_VegBiom))]

  #adjust phenology for mean monthly temperatures
  if (do_adjBiom_by_temp) {
    growing.season <- as.vector(mean_monthly_temp_C >= growing_limit_C)
    n_nonseason <- sum(!growing.season)
    n_season <- sum(growing.season)

    if (!isNorth)
      # Standard growing season needs to be adjusted for southern Hemisphere
      growing.season <- c(growing.season[7:12], growing.season[1:6])

    #Adjust for timing and duration of non-growing season
    if (n_nonseason > 0) {
      if (n_nonseason < 12) {
        temp <- c(std.winter[1] - 1, std.winter, std.winter[length(std.winter)] + 1) - 1
        std.winter.padded <- temp %% 12 + 1
        std.winter.seq <- 0:(length(std.winter.padded) - 1)
        site.winter.seq <- seq(from = 1, to = length(std.winter), length = n_nonseason)
        starts <- calc_starts(!growing.season)
        site.winter.start <- starts[length(starts)] #Calculate first month of winter == last start of non-growing season
        site.winter.months <- (site.winter.start + seq_len(n_nonseason) - 2) %% 12 + 1

        biom_shrubs[site.winter.months,] <- predict_season(biom_std_shrubs,
          std.winter.padded, std.winter.seq, site.winter.seq)
        biom_C3[site.winter.months,] <- predict_season(biom_std_C3, std.winter.padded,
          std.winter.seq, site.winter.seq)
        biom_C4[site.winter.months,] <- predict_season(biom_std_C4, std.winter.padded,
          std.winter.seq, site.winter.seq)
        biom_annuals[site.winter.months,] <- predict_season(biom_std_annuals,
          std.winter.padded, std.winter.seq, site.winter.seq)

      } else { #if winter lasts 12 months
        #Take the mean of the winter months
        temp <- apply(biom_std_shrubs[std.winter,], 2, mean)
        biom_shrubs[] <- matrix(temp, nrow = 12, ncol = ncol(biom_shrubs), byrow = TRUE)
        temp <- apply(biom_std_C3[std.winter,], 2, mean)
        biom_C3[] <- matrix(temp, nrow = 12, ncol = ncol(biom_C3), byrow = TRUE)
        temp <- apply(biom_std_C4[std.winter,], 2, mean)
        biom_C4[] <- matrix(temp, nrow = 12, ncol = ncol(biom_C4), byrow = TRUE)
        temp <- apply(biom_std_annuals[std.winter,], 2, mean)
        biom_annuals[] <- matrix(, nrow = 12, ncol = ncol(biom_annuals), byrow = TRUE)
      }
    }

    #Adjust for timing and duration of growing season
    if (n_season > 0) {
      if (n_season < 12) {
        temp <- c(std.growing[1] - 1, std.growing, std.growing[length(std.growing)] + 1) - 1
        std.growing.padded <- temp %% 12 + 1
        std.growing.seq <- 0:(length(std.growing.padded) - 1)
        site.growing.seq <- seq(from = 1, to = length(std.growing), length = n_season)
        starts <- calc_starts(growing.season)
        site.growing.start <- starts[1] #Calculate first month of growing season == first start of growing season
        site.growing.months <- (site.growing.start + seq_len(n_season) - 2) %% 12 + 1

        biom_shrubs[site.growing.months,] <- predict_season(biom_std_shrubs,
          std.growing.padded, std.growing.seq, site.growing.seq)
        biom_C3[site.growing.months,] <- predict_season(biom_std_C3, std.growing.padded,
          std.growing.seq, site.growing.seq)
        biom_C4[site.growing.months,] <- predict_season(biom_std_C4, std.growing.padded,
          std.growing.seq, site.growing.seq)
        biom_annuals[site.growing.months,] <- predict_season(biom_std_annuals,
          std.growing.padded, std.growing.seq, site.growing.seq)

      } else { #if growing season lasts 12 months
        temp <- apply(biom_std_shrubs[std.growing,], 2, max)
        biom_shrubs[] <- matrix(temp, nrow = 12, ncol = ncol(biom_shrubs), byrow = TRUE)
        temp <- apply(biom_std_C3[std.growing,], 2, max)
        biom_C3[] <- matrix(temp, nrow = 12, ncol = ncol(biom_C3), byrow = TRUE)
        temp <- apply(biom_std_C4[std.growing,], 2, max)
        biom_C4[] <- matrix(temp, nrow = 12, ncol = ncol(biom_C4), byrow = TRUE)
        temp <- apply(biom_std_annuals[std.growing,], 2, max)
        biom_annuals[] <- matrix(temp, nrow = 12, ncol = ncol(biom_annuals), byrow = TRUE)
      }
    }

    if (!isNorth) { #Adjustements were done as if on northern hemisphere
      biom_shrubs <- rbind(biom_shrubs[7:12,], biom_shrubs[1:6,])
      biom_C3 <- rbind(biom_C3[7:12,], biom_C3[1:6,])
      biom_C4 <- rbind(biom_C4[7:12,], biom_C4[1:6,])
      biom_annuals <- rbind(biom_annuals[7:12,], biom_annuals[1:6,])
    }
  }

  # if (do_adjBiom_by_ppt) then adjust biomass amounts by productivity relationship with MAP
  temp <- adjBiom_by_ppt(biom_shrubs, biom_C3, biom_C4, biom_annuals, biom_maxs = colmax,
    map_mm_shrubs = if (do_adjBiom_by_ppt) MAP_mm else StandardShrub_MAP_mm,
    map_mm_std_shrubs = StandardShrub_MAP_mm,
    map_mm_grasses = if (do_adjBiom_by_ppt) MAP_mm else StandardGrasses_MAP_mm,
    map_mm_std_grasses = StandardGrasses_MAP_mm,
    vegcomp_std_shrubs = StandardShrub_VegComposition,
    vegcomp_std_grass = StandardGrasses_VegComposition)

  biom_grasses <- temp$biom_C3 * fgrass_c3c4ann[1] + temp$biom_C4 * fgrass_c3c4ann[2] +
    temp$biom_annuals * fgrass_c3c4ann[3]
  cn <- dimnames(biom_grasses)[[2]]
  cn <- sapply(strsplit(cn, split = ".", fixed = TRUE), function(x) paste0(x[-1],
    collapse = "."))
  dimnames(biom_grasses)[[2]] <- cn

  biom_shrubs <- temp$biom_shrubs
  dimnames(biom_shrubs)[[2]] <- cn

  list(grass = as.matrix(biom_grasses),
       shrub = as.matrix(biom_shrubs))
}

#' Lookup transpiration coefficients for grasses, shrubs, and trees per soil layer or per soil depth increment of 1 cm per distribution type for each simulation run and copy values to 'datafile.soils'
#'
#' first row of datafile is label for per soil layer 'Layer' or per soil depth increment of 1 cm 'DepthCM'
#' second row of datafile is source of data
#' the other rows contain the data for each distribution type = columns
#' @section Note:
#'  cannot write data from sw_input_soils to datafile.soils
#' @export
TranspCoeffByVegType <- function(tr_input_code, tr_input_coeff,
  soillayer_no, trco_type, layers_depth,
  adjustType = c("positive", "inverse", "allToLast")) {

  #extract data from table by category
  trco.code <- as.character(tr_input_code[, which(colnames(tr_input_code) == trco_type)])
  trco <- rep(0, times = soillayer_no)
  trco.raw <- stats::na.omit(tr_input_coeff[, which(colnames(tr_input_coeff) == trco_type)])

  if (trco.code == "DepthCM") {
    temp <- sum(trco.raw, na.rm = TRUE)
    trco_sum <- if (temp == 0 || is.na(temp)) 1L else temp
    lup <- 1
    for(l in 1:soillayer_no){
      llow <- as.numeric(layers_depth[l])
      if(is.na(llow) | lup > length(trco.raw))
      {
        l <- l - 1
        break
      }
      trco[l] <- sum(trco.raw[lup:llow], na.rm=TRUE) / trco_sum
      lup <- llow + 1
    }
    usel <- l
  } else if(trco.code == "Layer"){
    usel <- ifelse(length(trco.raw) < soillayer_no, length(trco.raw), soillayer_no)
    trco[1:usel] <- trco.raw[1:usel] / ifelse((temp <- sum(trco.raw[1:usel], na.rm=TRUE)) == 0 & is.na(temp), 1, temp)
  }

  if(identical(adjustType, "positive")){
    trco <- trco / sum(trco)	#equivalent to: trco + (1 - sum(trco)) * trco / sum(trco)
  } else if(identical(adjustType, "inverse")){
    irows <- 1:max(which(trco > 0))
    trco[irows] <- trco[irows] + rev(trco[irows]) * (1 / sum(trco[irows]) - 1)	#equivalent to: trco + (1 - sum(trco)) * rev(trco) / sum(trco)
  } else if(identical(adjustType, "allToLast")){
    irow <- max(which(trco > 0))
    if(irow > 1){
      trco[irow] <- 1 - sum(trco[1:(irow - 1)]) 	#adding all the missing roots because soil is too shallow to the deepest available layer
    } else {
      trco[1] <- 1
    }
  }

  trco
}

#' Replace selected biomass values of a \linkS4class{swProd}-object
#'
#' @param fg A character string. One of the functional groups represented by
#'  \code{Rsoilwat}
#' @param use A logical vector.
update_biomass <- function(fg = c("Grass", "Shrub", "Tree", "Forb"), use,
  prod_input, prod_default) {

  fg <- match.arg(fg)

  comps <- c("_Litter", "_Biomass", "_FractionLive", "_LAIconv")
  veg_ids = lapply(comps, function(x)
    grep(paste0(fg, x), names(use)))
  veg_incl = lapply(veg_ids, function(x) use[x])

  temp <- slot(prod_default, paste0("MonthlyProductionValues_", tolower(fg)))
  if (any(unlist(veg_incl))) {
    for (k in seq_along(comps)) if (any(veg_incl[[k]]))
      temp[veg_incl[[k]], k] <- as.numeric(prod_input[, veg_ids[[k]][veg_incl[[k]]]])
  }

  temp
}
