

adjustLayersDepth <- function(layers_depth, d) {
  # The wrapper only handles 1-cm resolution of soil depths (maily because of the trco)
  round(layers_depth[seq_len(d)])
}
getLayersWidth <- function(layers_depth) diff(c(0, layers_depth))
setLayerSequence <- function(d) seq_len(d)



check_soil_data <- function(data) {
    check_soil <- is.finite(data)
    check_soil[, "depth_cm"] <- check_soil[, "depth_cm"] & data[, "depth_cm"] > 0 &
      diff(c(0, data[, "depth_cm"])) > 0
    check_soil[, "matricd"] <- check_soil[, "matricd"] & data[, "matricd"] > 0.3 &
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



#two, three, or four layer aggregation for average daily aggregation output
setAggSoilLayerForAggDailyResponses <- function(layers_depth, daily_lyr_agg) {
  d <- length(layers_depth)
  vals <- list()
  #first layer
  DeepestFirstDailyAggLayer <- findInterval(daily_lyr_agg[["first_cm"]], c(0, layers_depth) + SFSW2_glovars[["tol"]], all.inside = TRUE)
  vals[[1]] <- seq_len(DeepestFirstDailyAggLayer)
  #second layer
  if (!is.null(daily_lyr_agg[["second_cm"]])) {
    DeepestSecondDailyAggLayer <- findInterval(daily_lyr_agg[["second_cm"]], c(0, layers_depth) + SFSW2_glovars[["tol"]], all.inside = TRUE)
  } else {
    DeepestSecondDailyAggLayer <- d
  }
  if (is.numeric(DeepestSecondDailyAggLayer) && is.numeric(DeepestFirstDailyAggLayer) && d > DeepestFirstDailyAggLayer) {
    vals[[2]] <- (DeepestFirstDailyAggLayer+1):DeepestSecondDailyAggLayer
  }
  #third layer
  if (!is.null(daily_lyr_agg[["third_cm"]])) {
    if (!is.na(daily_lyr_agg[["third_cm"]])) {
      DeepestThirdDailyAggLayer <- findInterval(daily_lyr_agg[["third_cm"]], c(0, layers_depth) + SFSW2_glovars[["tol"]], all.inside = TRUE)
    } else {
      DeepestThirdDailyAggLayer <- NULL
    }
  } else {
    DeepestThirdDailyAggLayer <- d
  }
  if (is.numeric(DeepestThirdDailyAggLayer) && is.numeric(DeepestSecondDailyAggLayer) && d > DeepestSecondDailyAggLayer) {
    vals[[3]] <- (DeepestSecondDailyAggLayer+1):DeepestThirdDailyAggLayer
  }
  #fourth layer
  if (!is.null(daily_lyr_agg[["fourth_cm"]])) {
    if (!is.na(daily_lyr_agg[["fourth_cm"]])) {
      DeepestFourthDailyAggLayer <- findInterval(daily_lyr_agg[["fourth_cm"]], c(0, layers_depth) + SFSW2_glovars[["tol"]], all.inside = TRUE)
    } else {
      DeepestFourthDailyAggLayer <- NULL
    }
  } else {
    DeepestFourthDailyAggLayer <- d
  }
  if (is.numeric(DeepestFourthDailyAggLayer) && is.numeric(DeepestThirdDailyAggLayer) && d > DeepestThirdDailyAggLayer) {
    vals[[4]] <- ((DeepestThirdDailyAggLayer+1):DeepestFourthDailyAggLayer)
  }

  vals
}



calc_weights_from_depths <- function(il_new, target_cm, depths_cm) {
  if (il_new == 0) {
    c(target_cm, depths_cm[1] - target_cm)

  } else if (il_new >= length(depths_cm)) {
    c(0, target_cm)

  } else {
    abs(target_cm - depths_cm[il_new + c(1, 0)])

  }
}



#' Split soil layer in two layers
#'
#' @param x A numeric data.frame or matrix. Columns are soil layers.
#' @param il An integer value. The column/soil layer number after which a new layer is added.
#' @param w A numeric vector of length one or two. The weights used to calculate the
#'  values of the new layer.
#' @param method A character string. See \code{Details}.
#'
#' @section Details: If the weight vector is of length one and \code{x} contains a row
#'  with name 'depth_cm', then it is assumed that the value of \code{w} corresponds to the
#'  weight of the first layer and the weight of the second layer is calculated
#'  as \code{(depth of first layer of x) - (first value of w)}. If this is case and if
#'  the added layer is either more shallow or deeper than any input layers, then the depth
#'  of the added layer is calculated proportionally if \code{sum(w) <= 1} otherwise
#'  additively.
#' @section Details: The method \code{interpolate} calculates the weighted mean of the
#'  columns/layers \code{il} and \code{il + 1}. If \code{il == 0}, i.e., add layer at a
#'  more shallow depth than any existing layer, then values from the previously first
#'  layer are copied to the newly created layer.
#'  The method \code{exhaust} distributes the value of \code{il + 1} according to the
#'  weights.
#'
#' @return An object like x with one column more at position \code{il + 1}.
add_layer_to_soil <- function(x, il, w, method = c("interpolate", "exhaust")) {
  method <- match.arg(method)
  if (!is.matrix(x))
    x <- as.matrix(x)
  ncols <- dim(x)[2]
  if (length(w) == 1L && "depth_cm" %in% dimnames(x)[[1]] && x["depth_cm", 1] >= w)
    w <- c(w, x["depth_cm", 1] - w)

  stopifnot(length(w) == 2L, ncols > 0, is.finite(il), il >= 0, il <= ncols)
  w_sum <- sum(w)

  if (ncols > il) {
    # Add layer at an intermediate depth of existing layers
    x <- x[, c(seq_len(il), NA, (il + 1):ncols)]

    if (method == "interpolate") {
      if (il > 0) {
        x[, il + 1] <- (x[, il] * w[1] + x[, il + 2] * w[2]) / w_sum

      } else {
        # Add layer at a more shallow depth than any existing layer
        x[, 1] <- x[, 2]
        if ("depth_cm" %in% dimnames(x)[[1]])
          x["depth_cm", 1] <- if (w_sum <= 1 || w[1] > x["depth_cm", 2]) {
              x["depth_cm", 2] * w[1] / w_sum
            } else {
              w[1]
            }
      }

    } else if (method == "exhaust") {
      x[, il + 1] <- x[, il + 2] * w[1] / w_sum
      x[, il + 2] <- x[, il + 2] * w[2] / w_sum
    }

  } else if (ncols == il) {
    # Add a deeper layer than any existing layer
    x <- x[, c(seq_len(ncols), NA)]

    if (method == "interpolate") {
      x[, il + 1] <- x[, il]
      if ("depth_cm" %in% dimnames(x)[[1]])
        x["depth_cm", il + 1] <- if (w_sum <= 1) {
            x["depth_cm", il] * (1 + w[2] / w_sum)
          } else {
            x["depth_cm", il] + w[2]
          }

    } else if (method == "exhaust") {
      x[, il + 1] <- x[, il] * w[2] / w_sum
      x[, il] <- x[, il] * w[1] / w_sum
    }
  }

  x
}

identify_soillayers <- function(depths, sdepth) {
  it <- findInterval(depths, sdepth)
  if (any(is.na(it))) {
    as.integer(stats::na.exclude(it))
  } else if (length(it) > 1 && diff(it) > 0) {
    (1 + it[1]):(it[2])
  } else {
    it[1]
  }
}

adjustLayer_byImp <- function(depths, imp_depth, sdepths) {
  if (any(imp_depth < depths[1])) {
    depths <- imp_depth
    if (length(sdepths) >= 2) {
      temp <- findInterval(imp_depth, sdepths)
      if (temp > 1) {
        depths <- c(sdepths[temp - 1], imp_depth)
      } else {
        depths <- c(imp_depth, sdepths[temp + 1])
      }
    }
  } else if (any(imp_depth < depths[2])) {
    depths <- c(depths[1], imp_depth)
  }

  depths
}

EstimateInitialSoilTemperatureForEachSoilLayer <- function(layers_depth, lower.Tdepth, soilTupper, soilTlower) {
  sl <- c(0, lower.Tdepth)
  st <- c(soilTupper, soilTlower)

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
#' Checks that NAs are present and that NAs occur only grouped together
#' in the right-most columns per row (e.g., deepest soil layers if columns represent
#' soil layers and rows represent sites).
#'
#' @param x A data.frame, matrix, or array with at least two dimensions.
#'
#' @return A logical vector of length equal to the first dimension of \code{x} with
#'  \code{TRUE} if there are n[k] \code{NA}s in the k-th row and they occupy the k
#'  rightmost columns.
#'
has_NAs_pooled_at_depth <- function(x) {
  stopifnot(!is.null(dim(x)))
  temp <- apply(x, 1, function(dat) rle(is.na(dat)))
  sapply(temp, function(dat) length(dat$values) <= 2 && dat$values[length(dat$values)])
}




#' Merge two soil input datafiles
#'
#' Merge datafiles from two soil data sources (source 1 overrides source 2)
#'  and choose some or none of the variables to come from one source only.
#'
#' @param fmaster A character string. Path to the target master file.
#' @param fmaster1 A character string. Path to master file derived from extracting from
#'  soil data source 1
#' @param fmaster2 A character string. Path to master file derived from extracting from
#'  soil data source 2
#' @param fslayer A character string. Path to the target soil layer structure file.
#' @param fslayer1 A character string. Path to soil layer file derived from data source 1
#' @param fslayer2 A character string. Path to soil layer file derived from data source 2
#' @param fstexture A character string. Path to the target soil texture input file.
#' @param fstexture1 A character string. Path to soil texture file derived from data source 1
#' @param fstexture2 A character string. Path to soil texture file derived from data source 2
#' @param var_from2 A vector of character strings. Variables of the soil texture file,
#'  which will be take values from source2 if available even if source1 is available
#'
#' @return A logical value. This function is called for its side effects, i.e., storing
#'  updated/new files to \code{fmaster}, \code{fslayer}, and \code{fstexture}.
merge_2soils <- function(fmaster, fmaster1, fmaster2, fslayer, fslayer1, fslayer2,
  fstexture, fstexture1, fstexture2, var_from2 = NULL) {

  #------ MASTER FILES
  master1 <- utils::read.csv(fmaster1)
  master2 <- utils::read.csv(fmaster2)
  master <- if (file.exists(fmaster)) utils::read.csv(fmaster) else master1

  source1 <- as.character(unique(stats::na.exclude(master1$SoilTexture_source)))
  source2 <- as.character(unique(stats::na.exclude(master2$SoilTexture_source)))

  stopifnot(length(source1) == 1, length(source2) == 1)

  print(paste("'merge_2soils': data from", shQuote(source1), "and", shQuote(source2),
    "will be merged, and values from", shQuote(source1), "will be used for sites which",
    "contain data from both sources.",
    if (length(var_from2) > 0) paste("However, data from", shQuote(source2), "for",
    "variables", paste(shQuote(var_from2), collapse = ", "), "will be",
    "used for all sites if available")))

  iuse_source <- ifelse(!is.na(master1$SoilTexture_source) &
    !is.na(master1$Include_YN_SoilSources) & master1$Include_YN_SoilSources > 0, 1,
    ifelse(!is.na(master2$SoilTexture_source) &
    !is.na(master2$Include_YN_SoilSources) & master2$Include_YN_SoilSources > 0, 2, NA))

  soiltally <- table(iuse_source, useNA = "ifany")
  print(soiltally)

  # Indices of soil datasets
  id1 <- id1c <- !is.na(master1$SoilTexture_source) & master1$SoilTexture_source == source1
  id2 <- !is.na(master2$SoilTexture_source) & master2$SoilTexture_source == source2
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

  st <- if (file.exists(fstexture)) utils::read.csv(fstexture, skip = 1) else st1
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
        print(paste("'merge_2soils': no columns found for", shQuote(var_from2[k])))
      }
    }
  }

  # Reset transpiration regions
  st[, grep("TranspRegion", names(st))] <- NA
  print(paste("'merge_2soils': NOTE: transpiration regions have been reset. They require",
    "updated values before a simulation can be run successfully."))

  # Save to disk
  utils::write.csv(rbind(st_use, st), file = fstexture, row.names = FALSE)

  TRUE
}

