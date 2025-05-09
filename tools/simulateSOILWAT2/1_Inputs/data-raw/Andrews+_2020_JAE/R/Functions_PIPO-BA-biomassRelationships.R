#------ . ------
#--- Ponderosa Pine basal area and SOILWAT2 biomass ------
#
# Developed 2022-Nov-18 for
# Rodman, Kyle C. et al. (2025) Ecological Applications
#
# Based on data from Andrews et al. (2021) Journal of Applied Ecology
#
#------ . ------

#------ References ------

# Kyle C. Rodman, John B. Bradford, Alicia M. Formanack, Peter Z. Fulé,
# David W. Huffman, Thomas E. Kolb, Ana T. Miller-ter Kuile, Donald P.
# Normandin, Kiona Ogle, Rory J. Pederson, Daniel R. Schlaepfer, Michael
# T. Stoddard, Amy E.M. Waltz (2025. Restoration Treatments Enhance
# Tree Growth and Alter Climatic Constraints During Extreme Drought.
# Ecological Applications, 35(1), e3072. https://doi.org/10.1002/eap.3072

# Andrews, C. M., D’Amato, A. W., Fraver, S., Palik, B., Battaglia, M. A., &
# Bradford, J. B. (2020). Low stand density moderates growth declines during hot
# droughts in semi‐arid forests. Journal of Applied Ecology, 57(6), 1089–1102.
# https://doi.org/10.1111/1365-2664.13615


#------ . ------


#------ Estimate tree inputs for SOILWAT2 as PIPO from ba ------
#' @examples
#' xs <- seq(0, 51)
#' bm <- readRDS(
#'   file.path(dir_data, "Andrews2020_biomassRegressionPIPO.rds")
#' )
#' predictTreeInputsPIPO(xs, baModels = bm)
predictTreeInputsPIPO <- function(ba, baModels) {
  stopifnot(requireNamespace("mgcv", quietly = TRUE))

  ids_zero_ba <- abs(ba) < sqrt(.Machine[["double.eps"]])

  tmp <- lapply(
    baModels[["m"]],
    function(m) {
      tmp <- unname(
        predict(m, newdata = data.frame(ba_m2PERha = ba), type = "response")
      )
      tmp[ids_zero_ba] <- 0
      tmp
    }
  )

  res <- do.call(cbind, tmp)
  tmp_cn <- paste0("Tree_", baModels[["Variable"]], "_m", baModels[["time"]])
  ids <- grep("Cover", baModels[["Variable"]], fixed = TRUE)
  tmp_cn[ids] <- "Composition_TreeFraction"
  colnames(res) <- tmp_cn

  res
}


#--- Predict PIPO litter biomass from litter depth ------

# FIA litter depth to litter biomass per unit area conversion
# @reference Chojnacky, D., M. Amacher, and M. Gavazzi. 2009. Separating Duff and Litter for Improved Mass and Carbon Estimates. Southern Journal of Applied Forestry 33:29–34. https://doi.org/10.1093/sjaf/33.1.29.
#
# assume our low-elevation forest plots are best represented by
# pine litter only without duff
#
# thus, from Table 2
#   Pine litter mean = median = 0.03 g/cm3 (based on n = 12)
#   ==> 300 g / m2 litter for each cm litter depth
#   95% confidence interval is ±0.0057 g/cm3 or, respectively, ±57 g / m2

# John (2022-Nov-10): I agree that the tree litter values from Andrews are way
# too low….not sure what happened there but apparently I overlooked that
# mistake, because those values are not realistic. So, I suggest that we use
# the values from depth, but let’s check the simulation results to make sure
# that we aren’t simulating too much litter interception and failing to get
# moisture into the soil profile.
predictLitterBiomassPIPO <- function(litterDepth) {
  300 * litterDepth
}

#------ . ------
