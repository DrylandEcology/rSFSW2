
getLayersWidth <- rSOILWAT2:::getLayersWidth
calc_weights_from_depths <- rSOILWAT2:::calc_weights_from_depths
add_layer_to_soil <- rSOILWAT2:::add_layer_to_soil

#' The wrapper only handles 1-cm resolution of soil depths
#' (mainly because of the \var{trco})
adjustLayersDepth <- function(layers_depth, d) {
  round(layers_depth[seq_len(d)])
}

setLayerSequence <- function(d) seq_len(d)



check_soil_data <- function(data) {
    check_soil <- is.finite(data)
    check_soil[, "depth_cm"] <- check_soil[, "depth_cm"] &
      data[, "depth_cm"] > 0 &
      diff(c(0, data[, "depth_cm"])) > 0
    check_soil[, "matricd"] <- check_soil[, "matricd"] &
      data[, "matricd"] > 0.3 &
      data[, "matricd"] - 2.65 <= SFSW2_glovars[["tol"]]
    check_soil[, "gravel_content"] <- check_soil[, "gravel_content"] &
      data[, "gravel_content"] >= 0 & data[, "gravel_content"] < 1
    itemp <- c("sand", "clay")
    check_soil[, itemp] <- check_soil[, itemp] & data[, itemp] > 0 &
      data[, itemp] - 1 <= SFSW2_glovars[["tol"]]
    itemp <- c("EvapBareSoil_frac", "transpGrass_frac", "transpShrub_frac",
              "transpTree_frac", "transpForb_frac", "imperm")
    check_soil[, itemp] <- check_soil[, itemp] & data[, itemp] >= 0 &
      data[, itemp] - 1 <= SFSW2_glovars[["tol"]]

    check_soil
}

#' Check that soil moisture extraction coefficients meet expectations
#'
#' Expectations are: \itemize{
#'  \item Every coefficient must be equal or larger than 0,
#'  \item Their sum is strictly larger than 0,
#'  \item Their sum is equal or smaller than 1.
#' }
#'
#' @param data A numeric vector. The coefficient of each soil layer.
#'
#' @return A logical value. \code{TRUE} if \code{data} meets expectations.
check_soilco <- function(data) {
    temp <- sum(data)
    all(data >= 0, temp > 0, temp - 1 <= SFSW2_glovars[["tol"]])
}


#' Determine which soil layers will be aggregated into two, three, or four layer
#' aggregations for mean daily aggregation output
assign_aggregation_soillayers <- function(layers_depth, daily_lyr_agg) {
  d <- length(layers_depth)
  vals <- list()

  # first layer
  DeepestFirstDailyAggLayer <- findInterval(daily_lyr_agg[["first_cm"]],
    c(0, layers_depth) + SFSW2_glovars[["tol"]], all.inside = TRUE)
  vals[[1]] <- seq_len(DeepestFirstDailyAggLayer)

  # second layer
  if (!is.null(daily_lyr_agg[["second_cm"]])) {
    DeepestSecondDailyAggLayer <- findInterval(daily_lyr_agg[["second_cm"]],
      c(0, layers_depth) + SFSW2_glovars[["tol"]], all.inside = TRUE)
  } else {
    DeepestSecondDailyAggLayer <- d
  }

  if (is.numeric(DeepestSecondDailyAggLayer) &&
      is.numeric(DeepestFirstDailyAggLayer) && d > DeepestFirstDailyAggLayer) {

    vals[[2]] <- (DeepestFirstDailyAggLayer + 1):DeepestSecondDailyAggLayer
  }

  # third layer
  if (!is.null(daily_lyr_agg[["third_cm"]])) {
    if (!is.na(daily_lyr_agg[["third_cm"]])) {
      DeepestThirdDailyAggLayer <- findInterval(daily_lyr_agg[["third_cm"]],
        c(0, layers_depth) + SFSW2_glovars[["tol"]], all.inside = TRUE)
    } else {
      DeepestThirdDailyAggLayer <- NULL
    }
  } else {
    DeepestThirdDailyAggLayer <- d
  }

  if (is.numeric(DeepestThirdDailyAggLayer) &&
    is.numeric(DeepestSecondDailyAggLayer) && d > DeepestSecondDailyAggLayer) {

    vals[[3]] <- (DeepestSecondDailyAggLayer + 1):DeepestThirdDailyAggLayer
  }

  # fourth layer
  if (!is.null(daily_lyr_agg[["fourth_cm"]])) {
    if (!is.na(daily_lyr_agg[["fourth_cm"]])) {
      DeepestFourthDailyAggLayer <- findInterval(daily_lyr_agg[["fourth_cm"]],
        c(0, layers_depth) + SFSW2_glovars[["tol"]], all.inside = TRUE)
    } else {
      DeepestFourthDailyAggLayer <- NULL
    }
  } else {
    DeepestFourthDailyAggLayer <- d
  }

  if (is.numeric(DeepestFourthDailyAggLayer) &&
    is.numeric(DeepestThirdDailyAggLayer) && d > DeepestThirdDailyAggLayer) {

    vals[[4]] <- ((DeepestThirdDailyAggLayer + 1):DeepestFourthDailyAggLayer)
  }

  vals
}

init_soiltemperature <- function(layers_depth, lower.Tdepth, soilTupper,
  soilTlower) {

  sl <- c(0, lower.Tdepth) # nolint
  st <- c(soilTupper, soilTlower) # nolint

  stats::predict(stats::lm(st ~ sl), data.frame(sl = layers_depth))
}


setDeepestTopLayer <- function(layers_depth, Depth_TopLayers_cm) {
  max(1, findInterval(Depth_TopLayers_cm, layers_depth))
}

setTopLayer <- function(d, DeepestTopLayer) {
  seq_len(if (d < DeepestTopLayer) d else DeepestTopLayer)
}

setBottomLayer <- function(d, DeepestTopLayer) {
  if (d <= DeepestTopLayer) {
    NULL
  } else {
    (DeepestTopLayer + 1L):d
  }
}


#' NAs present but only in deepest soil layers
#'
#' Checks that NAs are present and that NAs occur only grouped together in the
#' right-most columns per row (e.g., deepest soil layers if columns represent
#' soil layers and rows represent sites).
#'
#' @param x A data.frame, matrix, or array with at least two dimensions.
#'
#' @return A logical vector of length equal to the first dimension of \code{x}
#'   with \code{TRUE} if there are n[k] \code{NA}s in the k-th row and they
#'   occupy the k rightmost columns.
#'
has_NAs_pooled_at_depth <- function(x) {
  stopifnot(!is.null(dim(x)))
  temp <- apply(x, 1, function(dat) rle(is.na(dat)))
  sapply(temp, function(dat)
    length(dat$values) <= 2 && dat$values[length(dat$values)])
}




#' Merge two soil input datafiles
#'
#' Merge datafiles from two soil data sources (source 1 overrides source 2) and
#' choose some or none of the variables to come from one source only.
#'
#' @param fmaster A character string. Path to the target master file.
#' @param fmaster1 A character string. Path to master file derived from
#'   extracting from soil data source 1
#' @param fmaster2 A character string. Path to master file derived from
#'   extracting from soil data source 2
#' @param fslayer A character string. Path to the target soil layer structure
#'   file.
#' @param fslayer1 A character string. Path to soil layer file derived from data
#'   source 1
#' @param fslayer2 A character string. Path to soil layer file derived from data
#'   source 2
#' @param fstexture A character string. Path to the target soil texture input
#'   file.
#' @param fstexture1 A character string. Path to soil texture file derived from
#'   data source 1
#' @param fstexture2 A character string. Path to soil texture file derived from
#'   data source 2
#' @param var_from2 A vector of character strings. Variables of the soil texture
#'   file, which will be take values from source2 if available even if source1
#'   is available
#'
#' @return A logical value. This function is called for its side effects, i.e.,
#'   storing updated/new files to \code{fmaster}, \code{fslayer}, and
#'   \code{fstexture}.
merge_2soils <- function(fmaster, fmaster1, fmaster2, fslayer, fslayer1,
  fslayer2, fstexture, fstexture1, fstexture2, var_from2 = NULL) {

  #------ MASTER FILES
  master1 <- utils::read.csv(fmaster1)
  master2 <- utils::read.csv(fmaster2)
  master <- if (file.exists(fmaster)) utils::read.csv(fmaster) else master1

  source1 <- as.character(unique(stats::na.exclude(master1$SoilTexture_source)))
  source2 <- as.character(unique(stats::na.exclude(master2$SoilTexture_source)))

  stopifnot(length(source1) == 1, length(source2) == 1)

  print(paste("'merge_2soils': data from", shQuote(source1), "and",
    shQuote(source2), "will be merged, and values from", shQuote(source1),
    "will be used for sites which contain data from both sources.",
    if (length(var_from2) > 0) paste("However, data from", shQuote(source2),
    "for variables", paste(shQuote(var_from2), collapse = ", "), "will be",
    "used for all sites if available")))

  temp1 <- !is.na(master1$SoilTexture_source) &
    !is.na(master1$Include_YN_SoilSources) & master1$Include_YN_SoilSources > 0
  temp2 <- !is.na(master2$SoilTexture_source) &
    !is.na(master2$Include_YN_SoilSources) & master2$Include_YN_SoilSources > 0
  iuse_source <- ifelse(temp1, 1, ifelse(temp2, 2, NA))

  soiltally <- table(iuse_source, useNA = "ifany")
  print(soiltally)

  # Indices of soil datasets
  id1 <- id1c <- !is.na(master1$SoilTexture_source) &
    master1$SoilTexture_source == source1
  id2 <- !is.na(master2$SoilTexture_source) &
    master2$SoilTexture_source == source2
  id2c <- id2 & !id1
  id12 <- id1 & id2
  idnot <- !id1c & !id2c

  # Copy data
  master[idnot, "SoilTexture_source"] <- NA
  master[id1c, "SoilTexture_source"] <- source1
  master[id2c, "SoilTexture_source"] <- source2
  master[idnot, "Include_YN_SoilSources"] <- 0
  master[!idnot, "Include_YN_SoilSources"] <- 1

  # Save to disk
  utils::write.csv(master, file = fmaster, row.names = FALSE)


  #------SOIL LAYERS
  sl1 <- utils::read.csv(fslayer1)
  sl2 <- utils::read.csv(fslayer2)
  sl <- if (file.exists(fslayer)) utils::read.csv(fslayer) else sl1

  # Copy data
  sl[idnot, -1] <- NA
  sl[id1c, ] <- sl1[id1c, ]
  sl[id2c, ] <- sl2[id2c, ]

  # Save to disk
  utils::write.csv(sl, file = fslayer, row.names = FALSE)


  #------SOIL TEXTURE DATA
  st1_use <- utils::read.csv(fstexture1, nrows = 1)
  st1 <- utils::read.csv(fstexture1, skip = 1)
  st2_use <- utils::read.csv(fstexture2, nrows = 1)
  st2 <- utils::read.csv(fstexture2, skip = 1)
  st_use <- ifelse(st1_use == 1 | st2_use == 1, 1, 0)

  st <- if (file.exists(fstexture)) {
      utils::read.csv(fstexture, skip = 1)
    } else st1

  names(st1) <- names(st2) <- names(st) <- names(st_use) <- names(st1_use)

  # Copy data
  st[idnot, -1] <- NA
  st[id1c, ] <- st1[id1c, ]
  st[id2c, ] <- st2[id2c, ]

  # Replace content of variables 'var_from2' with values from source2
  if (sum(id12) > 0 && length(var_from2) > 0) {
    for (k in seq_along(var_from2)) {
      icol2 <- grep(var_from2[k], names(st), ignore.case = TRUE)

      if (length(icol2) > 0) {
        st[id12, icol2] <- st2[id12, icol2]
      } else {
        print(paste("'merge_2soils': no columns found for",
          shQuote(var_from2[k])))
      }
    }
  }

  # Reset transpiration regions
  st[, grep("TranspRegion", names(st))] <- NA
  print(paste("'merge_2soils': NOTE: transpiration regions have been reset.",
    "They require updated values before a simulation can be run successfully."))

  # Save to disk
  utils::write.csv(rbind(st_use, st), file = fstexture, row.names = FALSE)

  TRUE
}
