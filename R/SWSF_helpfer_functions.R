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
  master1 <- read.csv(fmaster1)
  master2 <- read.csv(fmaster2)
  master <- if (file.exists(fmaster)) read.csv(fmaster) else master1

  source1 <- as.character(unique(na.exclude(master1$SoilTexture_source)))
  source2 <- as.character(unique(na.exclude(master2$SoilTexture_source)))

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
  write.csv(master, file = fmaster, row.names = FALSE)


  #------SOIL LAYERS
  sl1 <- read.csv(fslayer1)
  sl2 <- read.csv(fslayer2)
  sl <- if (file.exists(fslayer)) read.csv(fslayer) else sl1

  # Copy data
  sl[idnot, -1] <- NA
  sl[id1c, ] <- sl1[id1c, ]
  sl[id2c, ] <- sl2[id2c, ]

  # Save to disk
  write.csv(sl, file = fslayer, row.names = FALSE)


  #------SOIL TEXTURE DATA
  st1_use <- read.csv(fstexture1, nrows = 1)
  st1 <- read.csv(fstexture1, skip = 1)
  st2_use <- read.csv(fstexture2, nrows = 1)
  st2 <- read.csv(fstexture2, skip = 1)
  st_use <- ifelse(st1_use == 1 | st2_use == 1, 1, 0)

  st <- if (file.exists(fstexture)) read.csv(fstexture, skip = 1) else st1
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
  write.csv(rbind(st_use, st), file = fstexture, row.names = FALSE)

  TRUE
}
