########################
#------ datafile-IO functions

required_colnames_SWRunInformation <- function()
  c("Label", "site_id", "WeatherFolder", "X_WGS84", "Y_WGS84", "ELEV_m", "Include_YN")

#' Read a comma-separated value (csv) file
#'
#' Call function \code{\link[iotools]{read.csv.raw}}, if available, or else,
#' \code{\link[utils]{read.csv}}. \code{\link[iotools]{read.csv.raw}} can be much faster,
#'  particularly for large files. It reads, however, only \code{nrowsClasses} rows to
#'  determine the class of a column unlike \code{\link[utils]{read.csv}} which uses all
#'  rows to determine the column class.
#'
#' @param file A character string. The path to the file which is to be read.
#' @param stringsAsFactors A logical value. Should character vectors be converted to
#'  factors?
#' @param use_iotools A logical value. If \code{TRUE} and if \pkg{iotools} available,
#'  then \code{\link[iotools]{read.csv.raw}} instead of \code{\link[utils]{read.csv}} is
#'  used to read the \code{file}.
#' @param \dots Further arguments to be passed to \code{\link[iotools]{read.csv.raw}} or
#'  \code{\link[utils]{read.csv}}.
#'
#' @return A data frame (\code{\link[base]{data.frame}}) containing a representation of
#'  the data in the file.
swsf_read_csv <- function(file, stringsAsFactors = FALSE,
  use_iotools = TRUE, ...) {

  dots <- list(...)
  dots[["file"]] <- file
  dots[["stringsAsFactors"]] <- stringsAsFactors
  use_iotools <- requireNamespace("iotools", quietly = TRUE) && use_iotools
  res <- NULL

  if (use_iotools) {
    # faster than utils::read.csv
    dots2 <- dots[names(dots) %in% names(formals(iotools::read.csv.raw))]
    if (!any(names(dots2) == "nrowsClasses"))
      dots2[["nrowsClasses"]] <- 1000L

    temp <- try(do.call(iotools::read.csv.raw, args = dots2), silent = TRUE)
    if (inherits(temp, "try-error")) {
      use_iotools <- FALSE
    } else {
      names(temp) <- gsub("\"", "", names(temp))
      res <- temp
    }
  }

  if (!use_iotools) {
    dots2 <- dots[names(dots) %in% names(formals(utils::read.table))]
    res <- try(do.call(utils::read.csv, args = dots2), silent = TRUE)
  }

  if (dim(res)[2] < 2)
    print(paste("'swsf_read_csv': found only two columns in", shQuote(dots[["file"]]),
      " suggesting that it may be tab- instead of comma-separated."))

  res
}


#' Read the data from a 'SWSF-inputfile'
#'
#' 'SWSF-inputfiles' are comma-separated value files with \itemize{
#'  \item First row: field names of which the first one is 'Label'
#'  \item Second row: flags indicating which column information is applied (1) or not (0);
#'    the first entry is the character string 'UseInformationToCreateSoilWatRuns'.
#'  \item Third - last row: values of the input file; first column: site labels.
#' }
#'
#' @inheritParams swsf_read_csv
#'
#' @return A list of length two with the elements \describe{
#'  \item{use}{A named logical vector. The names are from the first row of the \code{file}
#'    and the values are \code{FALSE} if the second row of the \code{file} contains a 0
#'    and \code{TRUE} otherwise. The first entry, corresponding to column 'Label' is
#'    always \code{FALSE}.}
#'  \item{data}{A data frame (\code{\link[base]{data.frame}}) containing a representation
#'    of the values in the \code{file} with column names from the first row of the
#'    \code{file}.}
#' }
swsf_read_inputfile <- function(file, header_rows = 1, use_iotools = TRUE,
  ...) {

  sw_use <- tryCatch(swsf_read_csv(file, nrows = header_rows, use_iotools = use_iotools),
    error = function(e) print(paste("Failed to read file:", shQuote(basename(file)), "with", e)))
  sw <- swsf_read_csv(file, skip = header_rows, use_iotools = use_iotools, ...)
  names(sw) <- names(sw_use)
  sw_use <- c(FALSE, as.logical(as.numeric(sw_use[, -1])))
  sw_use[is.na(sw_use)] <- FALSE
  names(sw_use) <- names(sw)

  list(use = sw_use, data = sw)
}

#' Re-combine elements to create a 'SWSF-inputfile'
#'
#' Combines the output of \code{\link{swsf_read_inputfile}} to a data frame
#' (\code{\link[base]{data.frame}}) with proper 'SWSF-inputfile' format. This can be
#'  written back to disk.
#'
#' @param sw_use A named logical vector. See element \code{use} described under the
#'  section \code{Value} of \code{\link{swsf_read_inputfile}}.
#' @param data A named logical vector. See element \code{data} described under the
#'  section \code{Value} of \code{\link{swsf_read_inputfile}}.
#'
#' @return A data frame (\code{\link[base]{data.frame}}) with proper 'SWSF-inputfile'
#'  format.
reconstitute_inputfile <- function(sw_use, data) {
  temp <- as.data.frame(matrix(as.integer(sw_use), nrow = 1L))
  colnames(temp) <- names(sw_use)
  temp[1, 1] <- "UseInformationToCreateSoilWatRuns"
  rbind(temp, data)
}

check_requested_sites <- function(include_YN, SWRunInformation, fnames_in,
  verbose = FALSE) {

  includes_all_sources <- grep("Include_YN", colnames(SWRunInformation),
    ignore.case = TRUE, value = TRUE)
  do_ignore <- includes_all_sources %in% c("Include_YN", "include_YN_available")
  includes_sources <- includes_all_sources[!do_ignore]

  check <- FALSE

  if (length(includes_sources) > 0L) {
    include_YN_sources <- apply(SWRunInformation[, includes_sources, drop = FALSE], 1,
      function(x) all(x > 0L))

    if (all(include_YN_sources[include_YN > 0L])) {
      check <- TRUE

      if (verbose)
        print(paste("Data sources available for all requested SWSF simulation runs"))

    } else {
      include_YN_available <- rep(0L, dim(SWRunInformation)[1])
      include_YN_available[include_YN_sources] <- 1L
      SWRunInformation[, "include_YN_available"] <- include_YN_available

      utils::write.csv(SWRunInformation, file = fnames_in[["fmaster"]], row.names = FALSE)
      unlink(fnames_in[["fpreprocin"]])

      stop("Data sources not available for every requested SWSF simulation run. ",
        "New column 'include_YN_available' with updated information stored to ",
        "MasterInput file 'SWRunInformation' on disk. SWSF should be stopped so that you ",
        "can bring 'include_YN' and 'include_YN_available' in agreement before running ",
        "the simulations.")
    }

  }

  list(SWRunInformation = SWRunInformation, check = check)
}


map_input_variables <- function(map_vars, SWSF_prj_meta, SWSF_prj_inputs,
  verbose = FALSE) {

  if (verbose) {
    t1 <- Sys.time()
    print(paste0("SWSF's ", shQuote(match.call()[1]), ": started at ", t1))

    on.exit(print(paste0("SWSF's ", shQuote(match.call()[1]), ": ended after ",
      round(difftime(Sys.time(), t1, units = "secs"), 2), " s")), add = TRUE)
  }

  dir.inmap <- file.path(SWSF_prj_meta[["project_paths"]][["dir_out"]], "Input_maps")
  dir.create(dir.inmap, showWarnings = FALSE)

  input_avail <- list(
    SWRunInformation = list(
      cols = names(SWSF_prj_inputs[["SWRunInformation"]]),
      use = rep(TRUE, ncol(SWSF_prj_inputs[["SWRunInformation"]]))),
    sw_input_soillayers = list(
      cols = names(SWSF_prj_inputs[["sw_input_soillayers"]]),
      use = rep(TRUE, ncol(SWSF_prj_inputs[["sw_input_soillayers"]]))),
    sw_input_cloud = list(
      cols = names(SWSF_prj_inputs[["sw_input_cloud"]]),
      use = SWSF_prj_inputs[["sw_input_cloud_use"]]),
    sw_input_prod = list(
      cols = names(SWSF_prj_inputs[["sw_input_prod"]]),
      use = SWSF_prj_inputs[["sw_input_prod_use"]]),
    sw_input_site = list(
      cols = names(SWSF_prj_inputs[["sw_input_site"]]),
      use = SWSF_prj_inputs[["sw_input_site_use"]]),
    sw_input_soils = list(
      cols = names(SWSF_prj_inputs[["sw_input_soils"]]),
      use = SWSF_prj_inputs[["sw_input_soils_use"]]),
    sw_input_weather = list(
      cols = names(SWSF_prj_inputs[["sw_input_weather"]]),
      use = SWSF_prj_inputs[["sw_input_weather_use"]]),
    sw_input_climscen = list(
      cols = names(SWSF_prj_inputs[["sw_input_climscen"]]),
      use = SWSF_prj_inputs[["sw_input_climscen_use"]]),
    sw_input_climscen_values = list(
      cols = names(SWSF_prj_inputs[["sw_input_climscen_values"]]),
      use = SWSF_prj_inputs[["sw_input_climscen_use"]])
  )

  for (iv in seq_along(map_vars)) {
    iv_locs <- lapply(input_avail, function(ina)
      grep(map_vars[iv], ina$cols[ina$use], ignore.case = TRUE, value = TRUE))
    iv_locs <- iv_locs[lengths(iv_locs) > 0]

    if (length(iv_locs) > 0) {
      dir.create(dir.inmapvar <- file.path(dir.inmap, map_vars[iv]), showWarnings = FALSE)

      for (it1 in seq_along(iv_locs)) for (it2 in seq_along(iv_locs[[it1]])) {
        dat <- SWSF_prj_inputs[[names(iv_locs)[it1]]][SWSF_prj_meta[["sim_size"]][["runIDs_sites"]], iv_locs[[it1]][it2]]
        dat <- try(as.numeric(dat), silent = TRUE) # e.g., sw_input_cloud[, "SnowD_Hemisphere"] contains only strings for which as.numeric() issues a warning

        # this code plots only numeric maps
        if (any(is.finite(dat)) && !inherits(dat, "try-error")) {
          names(dat) <- iv_locs[[it1]][it2]

          map_flag <- paste(names(iv_locs)[it1], iv_locs[[it1]][it2],
            SWSF_prj_meta[["sim_space"]][["scorp"]], sep = "_")

          # Convert data to spatial object
          if (SWSF_prj_meta[["sim_space"]][["scorp"]] == "point") {
            sp_dat <- as(SWSF_prj_meta[["sim_space"]][["run_sites"]], "SpatialPointsDataFrame")
            temp <- as.data.frame(dat)
            colnames(temp) <-  iv_locs[[it1]][it2]
            slot(sp_dat, "data") <- temp

            if (!raster::compareCRS(SWSF_prj_meta[["sim_space"]][["crs_sites"]], SWSF_prj_meta[["sim_space"]][["sim_crs"]])) {
              sp_dat <- sp::spTransform(sp_dat, CRS = SWSF_prj_meta[["sim_space"]][["sim_crs"]])
            }

          } else if (SWSF_prj_meta[["sim_space"]][["scorp"]] == "cell") {
            # if failing, then need a more sophisticated assignment of values than
            # implemented below
            stopifnot(raster::canProcessInMemory(SWSF_prj_meta[["sim_space"]][["sim_raster"]]))

            temp <- SWSF_prj_meta[["sim_space"]][["run_sites"]]
            if (!raster::compareCRS(SWSF_prj_meta[["sim_space"]][["crs_sites"]], SWSF_prj_meta[["sim_space"]][["sim_crs"]])) {
              temp <- sp::spTransform(temp, CRS = SWSF_prj_meta[["sim_space"]][["sim_crs"]])
            }

            # init with NAs
            sp_dat <- raster::init(SWSF_prj_meta[["sim_space"]][["sim_raster"]], fun = function(x) rep(NA, x))
            sp_dat[raster::cellFromXY(sp_dat, sp::coordinates(temp))] <- dat
          }

          # Save to disk
          saveRDS(sp_dat, file = file.path(dir.inmapvar, paste0(map_flag, ".rds")))

          # Figure
          grDevices::png(height = 10, width = 6, units = "in", res = 200,
            file = file.path(dir.inmapvar, paste0(map_flag, ".png")))
          par_old <- graphics::par(mfrow = c(2, 1), mar = c(2.5, 2.5, 0.5, 0.5),
            mgp = c(1.25, 0.25, 0), tcl = 0.5, cex = 1)

          # panel a: map
          n_cols <- 255
          cols <- rev(grDevices::terrain.colors(7))
          cols[1] <- "gray"
          cols <- grDevices::colorRampPalette(c(cols, "dodgerblue3"))(n_cols)
          if (SWSF_prj_meta[["sim_space"]][["scorp"]] == "point") {
            par1 <- graphics::par(mar = c(2.5, 2.5, 0.5, 8.5))
            cdat <- cut(dat, n_cols)
            p_size <- function(x) max(0.25, min(2, 100 / x))
            sp::plot(sp_dat, col = cols[as.integer(cdat)], pch = 15,
              cex = p_size(length(dat)), axes = TRUE, asp = 1)
            # legend
            ids <- round(seq(1, n_cols, length.out = 12))
            lusr <- graphics::par("usr")
            lxy <- cbind(rep(lusr[2] + (lusr[2] - lusr[1]) / 15, 12),
              lusr[3] + (lusr[4] - lusr[3]) / 4 + seq(0, 1, length.out = 12) *
              (lusr[4] - lusr[3]) / 2)
            graphics::points(lxy, col = cols[ids], pch = 15, cex = 2, xpd = NA)
            graphics::text(lxy, pos = 4, labels = levels(cdat)[ids], xpd = NA)
            graphics::par(par1)

          } else if (SWSF_prj_meta[["sim_space"]][["scorp"]] == "cell") {
            raster::plot(sp_dat, col = cols, asp = 1)
          }
          graphics::mtext(side = 3, line = -1, adj = 0.03, text = paste0("(", letters[1], ")"),
            font = 2)

          # panel b: histogram
          graphics::hist(dat, xlab = paste(names(iv_locs)[it1], iv_locs[[it1]][it2]), main = "")
          graphics::mtext(side = 3, line = -1, adj = 0.03, text = paste0("(", letters[2], ")"),
            font = 2)

          graphics::par(par_old)
          grDevices::dev.off()
        }
      }
    }
  }

  invisible(TRUE)
}

#' Read from disk the default input files of SOILWAT2
#' @export
read_SOILWAT2_FileDefaults <- function(dir_in_sw, swFiles_tag = "file") {
  temp <- list.files(dir_in_sw)
  swFilesIn <- grep(swFiles_tag, temp, value = TRUE)[1]

  if (length(swFilesIn) == 0 || is.na(swFilesIn))
    stop("'read_SOILWAT2_FileDefaults': cannot find SOILWAT2's overview file ",
      shQuote(swFiles_tag), " in folder ", shQuote(dir_in_sw))

  # 'swDataFromFiles' acts as the basis for all runs
  swDataFromFiles <- rSOILWAT2::sw_inputDataFromFiles(dir = dir_in_sw,
    files.in = swFilesIn)

  # we don't need the example weather data; the code will get weather data separately
  if (length(swDataFromFiles@weatherHistory) > 0)
    swDataFromFiles@weatherHistory <- list(rSOILWAT2::swClear(swDataFromFiles@weatherHistory[[1]]))

  swDataFromFiles
}

complete_with_defaultpaths <- function(project_paths, fnames_in) {
  # full names of files located in 'dir_in'
  ftemp <- c("fmaster", "fslayers", "ftreatDesign", "fexpDesign", "fpreprocin",
    "fdbWeather", "fsimraster")

  for (f in ftemp) {
    if (f %in% names(fnames_in) && identical(basename(fnames_in[[f]]), fnames_in[[f]]))
      fnames_in[[f]] <- file.path(project_paths[["dir_in"]], fnames_in[[f]])
  }

  # full names of files located in 'dir_in_dat'
  ftemp <- c("fclimnorm", "fvegetation", "fsite", "fsoils", "fweathersetup",
    "fclimscen_delta", "fclimscen_values")

  for (f in ftemp) {
    if (f %in% names(fnames_in) && identical(basename(fnames_in[[f]]), fnames_in[[f]]))
      fnames_in[[f]] <- file.path(project_paths[["dir_in_dat"]], fnames_in[[f]])
  }

  # full names of files located in 'dir_in_treat'
  ftemp <- c("LookupClimatePPTScenarios", "LookupClimateTempScenarios",
    "LookupShiftedPPTScenarios", "LookupEvapCoeffFromTable", "LookupTranspCoeffFromTable",
    "LookupTranspRegionsFromTable", "LookupSnowDensityFromTable",
    "LookupVegetationComposition")

  for (f in ftemp) {
    if (f %in% names(fnames_in) && identical(basename(fnames_in[[f]]), fnames_in[[f]]))
      fnames_in[[f]] <- file.path(project_paths[["dir_in_treat"]], f, fnames_in[[f]])
  }

  fnames_in
}

load_Rsw_treatment_templates <- function(project_paths, create_treatments, ftag, class) {
  tr_list <- list()

  if (any(create_treatments == ftag)) {
    temp <- file.path(project_paths[["dir_in_treat"]], paste0("tr_", ftag))
    stopifnot(dir.exists(temp))

    temp <- list.files(temp, pattern = ".in", include.dirs = FALSE, recursive = TRUE,
      full.names = TRUE)

    tr_list[basename(temp)] <- unlist(lapply(temp, function(x)
      rSOILWAT2::swReadLines(rSOILWAT2::swClear(new(class)), x)))

  }
  tr_list
}


process_inputs <- function(project_paths, fnames_in, use_preprocin = TRUE, verbose = FALSE) {

  if (verbose) {
    t1 <- Sys.time()
    print(paste0("SWSF's ", shQuote(match.call()[1]), ": started at ", t1))
  }

  do_check_include <- FALSE

  if (!use_preprocin || !file.exists(fnames_in[["fpreprocin"]])) {

    SWRunInformation <- tryCatch(swsf_read_csv(fnames_in[["fmaster"]]), error = print)
    stopifnot(sapply(required_colnames_SWRunInformation(),
        function(x) x %in% names(SWRunInformation)),		# required columns
      all(SWRunInformation$site_id == seq_len(nrow(SWRunInformation))),	# consecutive site_id
      !grepl("[[:space:]]", SWRunInformation$Label),	# no space-characters in label
      !grepl("[[:space:]]", SWRunInformation$WeatherFolder)	# no space-characters in weather-data names
    )
    include_YN <- as.logical(SWRunInformation$Include_YN)
    nrowsClasses <- max(dim(SWRunInformation)[1], 25L, na.rm = TRUE)

    sw_input_soillayers <- tryCatch(swsf_read_csv(fnames_in[["fslayers"]],
      nrowsClasses = nrowsClasses), error = print)

    temp <- tryCatch(swsf_read_inputfile(fnames_in[["ftreatDesign"]],
      nrowsClasses = nrowsClasses), error = print)
    sw_input_treatments_use <- temp[["use"]]
    sw_input_treatments <- temp[["data"]]
    stopifnot(
      !grepl("[[:space:]]", sw_input_treatments$LookupWeatherFolder)	# no space-characters in weather-data names
    )

    temp <- tryCatch(swsf_read_inputfile(fnames_in[["fexpDesign"]],
      nrowsClasses = nrowsClasses), error = print)
    sw_input_experimentals_use <- temp[["use"]]
    sw_input_experimentals <- temp[["data"]]
    create_experimentals <- names(sw_input_experimentals_use[sw_input_experimentals_use])
    stopifnot(
      !grepl("[[:space:]]", sw_input_experimentals$LookupWeatherFolder)	# no space-characters in weather-data names
    )

    temp <- tryCatch(swsf_read_inputfile(fnames_in[["fclimnorm"]],
      nrowsClasses = nrowsClasses), error = print)
    sw_input_cloud_use <- temp[["use"]]
    sw_input_cloud <- temp[["data"]]

    temp <- tryCatch(swsf_read_inputfile(fnames_in[["fvegetation"]],
      nrowsClasses = nrowsClasses), error = print)
    sw_input_prod <- temp[["data"]]
    sw_input_prod_use <- temp[["use"]]

    temp <- tryCatch(swsf_read_inputfile(fnames_in[["fsite"]],
      nrowsClasses = nrowsClasses), error = print)
    sw_input_site <- temp[["data"]]
    sw_input_site_use <- temp[["use"]]

    temp <- tryCatch(swsf_read_inputfile(fnames_in[["fsoils"]],
      nrowsClasses = nrowsClasses), error = print)
    sw_input_soils_use <- temp[["use"]]
    sw_input_soils <- temp[["data"]]

    temp <- tryCatch(swsf_read_inputfile(fnames_in[["fweathersetup"]],
      nrowsClasses = nrowsClasses), error = print)
    sw_input_weather_use <- temp[["use"]]
    sw_input_weather <- temp[["data"]]

    temp <- tryCatch(swsf_read_inputfile(fnames_in[["fclimscen_delta"]],
      nrowsClasses = nrowsClasses), error = print)
    sw_input_climscen_use <- temp[["use"]]
    sw_input_climscen <- temp[["data"]]

    temp <- tryCatch(swsf_read_inputfile(fnames_in[["fclimscen_values"]],
      nrowsClasses = nrowsClasses), error = print)
    sw_input_climscen_values_use <- temp[["use"]]
    sw_input_climscen_values <- temp[["data"]]

    # update treatment specifications based on experimental design
    create_treatments <- union(names(sw_input_treatments_use)[sw_input_treatments_use],
      create_experimentals)

    # update specifications based on experimental design
    sw_input_cloud_use <- sw_input_cloud_use |
      names(sw_input_cloud_use) %in% create_experimentals
    sw_input_prod_use <- sw_input_prod_use |
      names(sw_input_prod_use) %in% create_experimentals
    sw_input_site_use <- sw_input_site_use |
      names(sw_input_site_use) %in% create_experimentals
    sw_input_soils_use <- sw_input_soils_use |
      names(sw_input_soils_use) %in% create_experimentals
    sw_input_weather_use <- sw_input_weather_use |
      names(sw_input_weather_use) %in% create_experimentals

    # Create a list of possible treatment files with data
    if (any(create_treatments == "sw"))
      print(paste("SW treatment is not used because library rSOILWAT2 only uses one",
        "version of SOILWAT2. Sorry"))

    tr_files <- load_Rsw_treatment_templates(project_paths, create_treatments, "filesin", "swFiles")
    tr_prod <- load_Rsw_treatment_templates(project_paths, create_treatments, "prodin", "swProd")
    tr_site <- load_Rsw_treatment_templates(project_paths, create_treatments, "siteparamin", "swSite")
    tr_soil <- load_Rsw_treatment_templates(project_paths, create_treatments, "soilsin", "swSoils")
    tr_weather <- load_Rsw_treatment_templates(project_paths, create_treatments, "weathersetupin", "swWeather")
    tr_cloud <- load_Rsw_treatment_templates(project_paths, create_treatments, "cloudin", "swCloud")

    tr_input_climPPT <- tr_input_climTemp <- tr_input_shiftedPPT <- list()
    tr_input_EvapCoeff <- tr_input_TranspCoeff_Code <- tr_input_TranspCoeff <- list()
    tr_input_TranspRegions <- tr_input_SnowD <- tr_VegetationComposition <- list()

    if (any(create_treatments == "LookupClimatePPTScenarios"))
      tr_input_climPPT <- swsf_read_csv(fnames_in[["LookupClimatePPTScenarios"]])

    if (any(create_treatments == "LookupClimateTempScenarios"))
      tr_input_climTemp <- swsf_read_csv(fnames_in[["LookupClimateTempScenarios"]])

    if (any(create_treatments == "LookupShiftedPPTScenarios"))
      tr_input_shiftedPPT <- swsf_read_csv(fnames_in[["LookupShiftedPPTScenarios"]],
        row.names = 1)

    if (any(create_treatments == "LookupEvapCoeffFromTable"))
      tr_input_EvapCoeff <- swsf_read_csv(fnames_in[["LookupEvapCoeffFromTable"]],
        row.names = 1)

    if (any(grepl("LookupTranspCoeffFromTable_", create_treatments),
        create_treatments == "AdjRootProfile")) {
      tr_input_TranspCoeff_Code <- tryCatch(utils::read.csv(fnames_in[["LookupTranspCoeffFromTable"]],
        nrows = 2, stringsAsFactors = FALSE), error = print)
      tr_input_TranspCoeff_Code <- tr_input_TranspCoeff_Code[-2,]
      tr_input_TranspCoeff <- utils::read.csv(fnames_in[["LookupTranspCoeffFromTable"]],
        skip = 2, stringsAsFactors = FALSE)
      colnames(tr_input_TranspCoeff) <- colnames(tr_input_TranspCoeff_Code)
    }

    if (any(create_treatments == "LookupTranspRegionsFromTable"))
      tr_input_TranspRegions <- utils::read.csv(fnames_in[["LookupTranspRegionsFromTable"]],
        row.names = 1, stringsAsFactors = FALSE)

    if (any(create_treatments == "LookupSnowDensityFromTable"))
      tr_input_SnowD <- utils::read.csv(fnames_in[["LookupSnowDensityFromTable"]],
        row.names = 1, stringsAsFactors = FALSE)

    if (any(create_treatments == "AdjMonthlyBioMass_Temperature"))
      tr_VegetationComposition <- utils::read.csv(fnames_in[["LookupVegetationComposition"]],
        skip = 1, row.names = 1, stringsAsFactors = FALSE)


    #-import regeneration data
    ftemp_GISSM <- list.files(project_paths[["dir_in_gissm"]],
      pattern = ".csv")
    GISSM_species_No <- length(ftemp_GISSM)

    if (GISSM_species_No > 0) {
      f.temp <- utils::read.csv(file.path(project_paths[["dir_in_gissm"]], ftemp_GISSM[1]),
        stringsAsFactors = FALSE)
      GISSM_params <- matrix(NA, nrow = nrow(f.temp), ncol = GISSM_species_No)
      colnames(GISSM_params) <- sub(".csv", "", ftemp_GISSM)
      rownames(GISSM_params) <- f.temp[, 1]
      GISSM_params[, 1] <- f.temp[, 2]

      if (GISSM_species_No > 1) for (f in 2:GISSM_species_No) {
        f.temp <- utils::read.csv(file.path(project_paths[["dir_in_gissm"]], ftemp_GISSM[f]),
          stringsAsFactors = FALSE)
        GISSM_params[, f] <- f.temp[, 2]
      }

    } else {
      GISSM_params <- list()
    }

    #--- set flag to check include_YN columns
    do_check_include <- TRUE

    temp <- list(do_check_include = do_check_include,
      SWRunInformation = SWRunInformation, include_YN = include_YN,
      create_experimentals = create_experimentals, create_treatments = create_treatments,
      sw_input_soillayers = sw_input_soillayers,
      sw_input_treatments_use = sw_input_treatments_use, sw_input_treatments = sw_input_treatments,
      sw_input_experimentals_use = sw_input_experimentals_use, sw_input_experimentals = sw_input_experimentals,
      sw_input_cloud_use = sw_input_cloud_use, sw_input_cloud = sw_input_cloud,
      sw_input_prod_use = sw_input_prod_use, sw_input_prod = sw_input_prod,
      sw_input_site_use = sw_input_site_use, sw_input_site = sw_input_site,
      sw_input_soils_use = sw_input_soils_use, sw_input_soils = sw_input_soils,
      sw_input_weather_use = sw_input_weather_use, sw_input_weather = sw_input_weather,
      sw_input_climscen_use = sw_input_climscen_use, sw_input_climscen = sw_input_climscen,
      sw_input_climscen_values_use = sw_input_climscen_values_use, sw_input_climscen_values = sw_input_climscen_values,
      tr_files = tr_files, tr_prod = tr_prod, tr_site = tr_site, tr_soil = tr_soil,
      tr_weather = tr_weather, tr_cloud = tr_cloud, tr_input_climPPT = tr_input_climPPT,
      tr_input_climTemp = tr_input_climTemp, tr_input_shiftedPPT = tr_input_shiftedPPT,
      tr_input_EvapCoeff = tr_input_EvapCoeff, tr_input_TranspCoeff_Code = tr_input_TranspCoeff_Code,
      tr_input_TranspCoeff = tr_input_TranspCoeff, tr_input_TranspRegions = tr_input_TranspRegions,
      tr_input_SnowD = tr_input_SnowD, tr_VegetationComposition = tr_VegetationComposition,
      GISSM_params = GISSM_params, GISSM_species_No = GISSM_species_No
    )

    inputs <- list2env(x = temp, envir = new.env(parent = emptyenv()))

    saveRDS(inputs, file = fnames_in[["fpreprocin"]])

  } else {
    inputs <- readRDS(fnames_in[["fpreprocin"]])
  }

  inputs[["do_check_include"]] <- do_check_include

  if (!is.environment(inputs) || length(inputs) == 0) {
    print(paste0("SWSF's ", shQuote(match.call()[1]), ": failed; 'SWSF_prj_inputs' is ",
      "empty or not of type 'environment'."))
  }

  if (verbose)
    print(paste0("SWSF's ", shQuote(match.call()[1]), ": ended after ",
      round(difftime(Sys.time(), t1, units = "secs"), 2), " s"))

  inputs
}



#------ End of datafile-IO functions
########################
