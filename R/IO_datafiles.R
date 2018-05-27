########################
#------ datafile-IO functions

req_fields_SWRunInformation <- function() {
  c("Label", "site_id", "WeatherFolder", "X_WGS84", "Y_WGS84", "ELEV_m",
    "Include_YN")
}

#' Read a comma-separated value (\var{csv}) file
#'
#' Call function \code{\link[iotools]{read.csv.raw}}, if available, or else,
#' \code{\link[utils]{read.csv}}. \code{\link[iotools]{read.csv.raw}} can be
#' much faster, particularly for large files. It reads, however, only
#' \code{nrowsClasses} rows to determine the class of a column unlike
#' \code{\link[utils]{read.csv}} which uses all rows to determine the column
#' class.
#'
#' @param file A character string. The path to the file which is to be read.
#' @param stringsAsFactors A logical value. Should character vectors be
#'   converted to factors?
#' @param use_iotools A logical value. If \code{TRUE} and if \pkg{iotools}
#'   available, then \code{\link[iotools]{read.csv.raw}} instead of
#'   \code{\link[utils]{read.csv}} is used to read the \code{file}.
#' @param \dots Further arguments to be passed to
#'   \code{\link[iotools]{read.csv.raw}} or \code{\link[utils]{read.csv}}.
#'
#' @return A data frame (\code{\link[base]{data.frame}}) containing a
#'   representation of the data in the file.
SFSW2_read_csv <- function(file, stringsAsFactors = FALSE,
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
    print(paste("'SFSW2_read_csv': found only two columns in",
      shQuote(dots[["file"]]), "suggesting that it may be tab- instead of",
      "comma-separated."))

  res
}


#' Read the data from a \var{\sQuote{rSFSW2-inputfile}}
#'
#' \var{\sQuote{rSFSW2-inputfiles}} are comma-separated value files with
#' \itemize{ \item First row: field names of which the first one is
#' \var{\dQuote{Label}} \item Second row: flags indicating which column
#' information is applied (1) or not (0); the first entry is the character
#' string \var{\dQuote{UseInformationToCreateSoilWatRuns}}. \item Third - last
#' row: values of the input file; first column: site labels. }
#'
#' @inheritParams SFSW2_read_csv
#' @param header_rows An integer value. The row number which contains the
#'   header.
#'
#' @return A list of length two with the elements \describe{ \item{use}{A named
#'   logical vector. The names are from the first row of the \code{file} and the
#'   values are \code{FALSE} if the second row of the \code{file} contains a 0
#'   and \code{TRUE} otherwise. The first entry, corresponding to column
#'   \var{\dQuote{Label}} is always \code{FALSE}.} \item{data}{A data frame
#'   (\code{\link[base]{data.frame}}) containing a representation of the values
#'   in the \code{file} with column names from the first row of the
#'   \code{file}.} }
SFSW2_read_inputfile <- function(file, header_rows = 1, use_iotools = TRUE,
  ...) {

  sw_use <- tryCatch(SFSW2_read_csv(file, nrows = header_rows,
    use_iotools = use_iotools), error = function(e)
    print(paste("Failed to read file:", shQuote(basename(file)), "with", e)))

  sw <- SFSW2_read_csv(file, skip = header_rows, use_iotools = use_iotools, ...)
  names(sw) <- names(sw_use)
  sw_use <- c(FALSE, as.logical(as.numeric(sw_use[, -1])))
  sw_use[is.na(sw_use)] <- FALSE
  names(sw_use) <- names(sw)

  list(use = sw_use, data = sw)
}

#' Re-combine elements to create a \var{\sQuote{rSFSW2-inputfile}}
#'
#' Combines the output of \code{\link{SFSW2_read_inputfile}} to a data frame
#' (\code{\link[base]{data.frame}}) with proper \var{\sQuote{rSFSW2-inputfile}}
#' format. This can be written back to disk.
#'
#' @param sw_use A named logical vector. See element \code{use} described under
#'   the section \code{Value} of \code{\link{SFSW2_read_inputfile}}.
#' @param data A named logical vector. See element \code{data} described under
#'   the section \code{Value} of \code{\link{SFSW2_read_inputfile}}.
#'
#' @return A data frame (\code{\link[base]{data.frame}}) with proper
#'   \var{\sQuote{rSFSW2-inputfile}} format.
reconstitute_inputfile <- function(sw_use, data) {
  temp <- as.data.frame(matrix(as.integer(sw_use), nrow = 1L))
  colnames(temp) <- names(sw_use)
  temp[1, 1] <- "UseInformationToCreateSoilWatRuns"
  rbind(temp, data)
}

check_requested_sites <- function(include_YN, SWRunInformation, fnames_in,
  verbose = FALSE) {

  incl_all_sources <- grep("Include_YN", colnames(SWRunInformation),
    ignore.case = TRUE, value = TRUE)
  do_ignore <- incl_all_sources %in% c("Include_YN", "include_YN_available")
  incl_sources <- incl_all_sources[!do_ignore]

  check <- FALSE

  if (length(incl_sources) > 0L) {
    include_YN_sources <- apply(SWRunInformation[, incl_sources, drop = FALSE],
      1, function(x) all(x > 0L))

    if (all(include_YN_sources[include_YN > 0L])) {
      check <- TRUE

      if (verbose) {
        print(paste("Data sources available for all requested rSFSW2",
          "simulation runs"))
      }

    } else {
      include_YN_available <- rep(0L, dim(SWRunInformation)[1])
      include_YN_available[include_YN_sources] <- 1L
      SWRunInformation[, "include_YN_available"] <- include_YN_available

      utils::write.csv(SWRunInformation, file = fnames_in[["fmaster"]],
        row.names = FALSE)
      unlink(fnames_in[["fpreprocin"]])

      stop("Data sources not available for every requested rSFSW2 simulation ",
        "run. New column 'include_YN_available' with updated information ",
        "stored to MasterInput file 'SWRunInformation' on disk. rSFSW2 ",
        "should be stopped so that you can bring 'include_YN' and ",
        "'include_YN_available' in agreement before running the simulations.")
    }

  }

  list(SWRunInformation = SWRunInformation, check = check)
}


map_input_variables <- function(map_vars, SFSW2_prj_meta, SFSW2_prj_inputs,
  verbose = FALSE) {

  if (verbose) {
    t1 <- Sys.time()
    temp_call <- shQuote(match.call()[1])
    print(paste0("rSFSW2's ", temp_call, ": started at ", t1))

    on.exit({
      print(paste0("rSFSW2's ", temp_call, ": ended after ",
      round(difftime(Sys.time(), t1, units = "secs"), 2), " s")); cat("\n")
      },
      add = TRUE)
  }


  dir.inmap <- file.path(SFSW2_prj_meta[["project_paths"]][["dir_out"]],
    "Input_maps")
  dir.create(dir.inmap, showWarnings = FALSE)

  input_avail <- list(
    SWRunInformation = list(
      cols = names(SFSW2_prj_inputs[["SWRunInformation"]]),
      use = rep(TRUE, ncol(SFSW2_prj_inputs[["SWRunInformation"]]))),
    sw_input_soillayers = list(
      cols = names(SFSW2_prj_inputs[["sw_input_soillayers"]]),
      use = rep(TRUE, ncol(SFSW2_prj_inputs[["sw_input_soillayers"]]))),
    sw_input_cloud = list(
      cols = names(SFSW2_prj_inputs[["sw_input_cloud"]]),
      use = SFSW2_prj_inputs[["sw_input_cloud_use"]]),
    sw_input_prod = list(
      cols = names(SFSW2_prj_inputs[["sw_input_prod"]]),
      use = SFSW2_prj_inputs[["sw_input_prod_use"]]),
    sw_input_site = list(
      cols = names(SFSW2_prj_inputs[["sw_input_site"]]),
      use = SFSW2_prj_inputs[["sw_input_site_use"]]),
    sw_input_soils = list(
      cols = names(SFSW2_prj_inputs[["sw_input_soils"]]),
      use = SFSW2_prj_inputs[["sw_input_soils_use"]]),
    sw_input_weather = list(
      cols = names(SFSW2_prj_inputs[["sw_input_weather"]]),
      use = SFSW2_prj_inputs[["sw_input_weather_use"]]),
    sw_input_climscen = list(
      cols = names(SFSW2_prj_inputs[["sw_input_climscen"]]),
      use = SFSW2_prj_inputs[["sw_input_climscen_use"]]),
    sw_input_climscen_values = list(
      cols = names(SFSW2_prj_inputs[["sw_input_climscen_values"]]),
      use = SFSW2_prj_inputs[["sw_input_climscen_use"]])
  )

  sim_space <- SFSW2_prj_meta[["sim_space"]]

  if (sim_space[["scorp"]] == "point") {
    p_size <- function(x) max(0.25, min(2, 100 / x))
  }

  for (iv in seq_along(map_vars)) {
    iv_locs <- lapply(input_avail, function(ina)
      grep(map_vars[iv], ina$cols[ina$use], ignore.case = TRUE, value = TRUE))
    iv_locs <- iv_locs[lengths(iv_locs) > 0]

    if (length(iv_locs) > 0) {
      dir.create(dir.inmapvar <- file.path(dir.inmap, map_vars[iv]),
        showWarnings = FALSE)

      for (it1 in seq_along(iv_locs)) for (it2 in seq_along(iv_locs[[it1]])) {
        temp <- SFSW2_prj_inputs[[names(iv_locs)[it1]]]
        dat <- temp[SFSW2_prj_meta[["sim_size"]][["runIDs_sites"]],
          iv_locs[[it1]][it2]]
        # e.g., sw_input_cloud[, "SnowD_Hemisphere"] contains only strings for
        # which as.numeric() issues a warning
        dat <- try(as.numeric(dat), silent = TRUE)

        # this code plots only numeric maps
        if (any(is.finite(dat)) && !inherits(dat, "try-error")) {
          names(dat) <- iv_locs[[it1]][it2]

          map_flag <- paste(names(iv_locs)[it1], iv_locs[[it1]][it2],
            sim_space[["scorp"]], sep = "_")

          # Convert data to spatial object
          if (sim_space[["scorp"]] == "point") {
            sp_dat <- as(sim_space[["run_sites"]], "SpatialPointsDataFrame")
            temp <- as.data.frame(dat)
            colnames(temp) <-  iv_locs[[it1]][it2]
            slot(sp_dat, "data") <- temp

            if (!raster::compareCRS(sim_space[["crs_sites"]],
              sim_space[["sim_crs"]])) {

              sp_dat <- sp::spTransform(sp_dat, CRS = sim_space[["sim_crs"]])
            }

          } else if (sim_space[["scorp"]] == "cell") {
            # if failing, then need a more sophisticated assignment of values
            # than implemented below
            stopifnot(raster::canProcessInMemory(sim_space[["sim_raster"]]))

            if (!raster::compareCRS(sim_space[["crs_sites"]],
              sim_space[["sim_crs"]])) {

              temp <- sp::spTransform(sim_space[["run_sites"]],
                CRS = sim_space[["sim_crs"]])
            }

            # init with NAs
            sp_dat <- raster::init(sim_space[["sim_raster"]],
              fun = function(x) rep(NA, x))
            temp <- sp::coordinates(sim_space[["run_sites"]])
            sp_dat[raster::cellFromXY(sp_dat, temp)] <- dat
          }

          # Save to disk
          saveRDS(sp_dat, file = file.path(dir.inmapvar,
            paste0(map_flag, ".rds")))

          # Figure
          grDevices::png(height = 10, width = 6, units = "in", res = 200,
            file = file.path(dir.inmapvar, paste0(map_flag, ".png")))
          par_old <- graphics::par(mfrow = c(2, 1), mar = c(2.5, 2.5, 0.5, 0.5),
            mgp = c(1.25, 0.25, 0), tcl = 0.5, cex = 1)

          # panel a: map
          dx <- diff(range(dat, na.rm = TRUE))

          if (abs(dx) < SFSW2_glovars[["tol"]] || !is.finite(dx)) {
            n_cols <- 1L
            cols <- "dodgerblue3"
            n_legend <- 1L

          } else {
            n_cols <- 255L
            cols <- rev(grDevices::terrain.colors(7))
            cols[1] <- "gray"
            cols <- grDevices::colorRampPalette(c(cols, "dodgerblue3"))(n_cols)
            n_legend <- 12L
          }

          if (sim_space[["scorp"]] == "point") {
            par1 <- graphics::par(mar = c(2.5, 2.5, 0.5, 8.5))

            if (n_cols == 1L) {
              legend_labs <- as.character(dat[1])
              sp::plot(sp_dat, col = cols, pch = 15, cex = 1, axes = TRUE,
                asp = 1)

            } else {
              cdat <- cut(dat, n_cols)
              legend_labs <- levels(cdat)
              sp::plot(sp_dat, col = cols[as.integer(cdat)], pch = 15,
                cex = p_size(length(dat)), axes = TRUE, asp = 1)
            }

            lusr <- graphics::par("usr")
            lx <- lusr[2] + (lusr[2] - lusr[1]) / 15
            lys <- c(lusr[3] + (lusr[4] - lusr[3]) / 4, (lusr[4] - lusr[3]) / 2)
            ids <- round(seq(1, n_cols, length.out = n_legend))
            lxy <- cbind(rep(lx, n_legend),
              lys[1] + seq(0, 1, length.out = n_legend) * lys[2])

            # legend
            graphics::points(lxy, col = cols[ids], pch = 15, cex = 2, xpd = NA)
            graphics::text(lxy, pos = 4, labels = legend_labs[ids], xpd = NA)
            graphics::par(par1)

          } else if (sim_space[["scorp"]] == "cell") {
            raster::plot(sp_dat, col = cols, asp = 1)
          }

          graphics::mtext(side = 3, line = -1, adj = 0.03,
            text = paste0("(", letters[1], ")"), font = 2)

          # panel b: histogram
          graphics::hist(dat, xlab = paste(names(iv_locs)[it1],
            iv_locs[[it1]][it2]), main = "")
          graphics::mtext(side = 3, line = -1, adj = 0.03,
            text = paste0("(", letters[2], ")"), font = 2)

          graphics::par(par_old)
          grDevices::dev.off()
        }
      }
    }
  }

  invisible(TRUE)
}



#' Prepare default inputs from \pkg{rSOILWAT2}
#'
#' This function loads the data \code{\link[rSOILWAT2]{sw_exampleData}} from
#' \pkg{rSOILWAT2}; removes all but one soil layer (to prevent carry-over
#' effects of deeper layers in sites that have a shallower soil profile
#' simulated by \pkg{rSFSW2}); turns on soil temperature simulations; and
#' removes any weather data.
#'
#' @return A \code{\link[rSOILWAT2:swInputData-class]{rSOILWAT2::swInputData}}
#'   object.
#' @export
read_SOILWAT2_DefaultInputs <- function() {
  # 'example1' of rSOILWAT2 package is defined as 'default' from SOILWAT2
  swData <- rSOILWAT2::sw_exampleData

  # Delete all but one soil layer
  temp <- rSOILWAT2::swSoils_Layers(swData)[1, , drop = FALSE]
  rSOILWAT2::swSoils_Layers(swData) <- temp

  # Turn soil temperature on
  rSOILWAT2::swSite_SoilTemperatureFlag(swData) <- TRUE

  # Delete weather data folder (all rSFSW2 projects get their own weather data)
  rSOILWAT2::set_swWeatherData(swData) <- new("swWeatherData")

  swData
}



complete_with_defaultpaths <- function(project_paths, fnames_in) {
  # full names of files located in 'dir_in'
  ftemp <- c("fmaster", "fslayers", "ftreatDesign", "fexpDesign", "fpreprocin",
    "fdbWeather", "fsimraster")

  for (f in ftemp) {
    if (f %in% names(fnames_in) &&
        identical(basename(fnames_in[[f]]), fnames_in[[f]]))

      fnames_in[[f]] <- file.path(project_paths[["dir_in"]], fnames_in[[f]])
  }

  # full names of files located in 'dir_in_dat'
  ftemp <- c("fclimnorm", "fvegetation", "fsite", "fsoils", "fweathersetup",
    "fclimscen_delta", "fclimscen_values")

  for (f in ftemp) {
    if (f %in% names(fnames_in) &&
        identical(basename(fnames_in[[f]]), fnames_in[[f]]))

      fnames_in[[f]] <- file.path(project_paths[["dir_in_dat"]], fnames_in[[f]])
  }

  # full names of files located in 'dir_in_treat'
  ftemp <- c("LookupCarbonScenarios", "LookupClimatePPTScenarios",
    "LookupClimateTempScenarios", "LookupShiftedPPTScenarios",
    "LookupEvapCoeffFromTable", "LookupTranspCoeffFromTable",
    "LookupTranspRegionsFromTable", "LookupSnowDensityFromTable",
    "LookupVegetationComposition")

  for (f in ftemp) {
    if (f %in% names(fnames_in) &&
        identical(basename(fnames_in[[f]]), fnames_in[[f]]))

      fnames_in[[f]] <- file.path(project_paths[["dir_in_treat"]], f,
        fnames_in[[f]])
  }

  fnames_in
}

load_Rsw_treatment_templates <- function(project_paths, create_treatments,
  ftag, class) {

  tr_list <- list()

  if (any(create_treatments == ftag)) {
    temp <- file.path(project_paths[["dir_in_treat"]], paste0("tr_", ftag))
    stopifnot(dir.exists(temp))

    temp <- list.files(temp, pattern = ".in", include.dirs = FALSE,
      recursive = TRUE, full.names = TRUE)

    tr_list[basename(temp)] <- unlist(lapply(temp, function(x)
      rSOILWAT2::swReadLines(new(class), x)))

  }
  tr_list
}

fix_rowlabels <- function(x, master, verbose = TRUE) {

  ml <- as.character(master[, "Label"])

  if ("Label" %in% names(x)) {
    xl <- as.character(x[, "Label"])

  } else {
    x <- data.frame(Label = rep(NA, length(ml)), x, stringsAsFactors = FALSE)
    xl <- NULL
  }

  if (!identical(xl, ml)) {
    argnames <- as.character(match.call()[2:3])

    if (dim(x)[1] == 0L) {
      if (verbose) {
        print(paste("Datafile", shQuote(argnames[1]), "contains zero rows.",
          "'Label's of the master input file", shQuote(argnames[2]),
          "are used to populate rows and 'Label's of the datafile."))
      }

      x[seq_along(ml), "Label"] <- ml

    } else if (dim(master)[1] == dim(x)[1]) {
      print(paste("Datafile", shQuote(argnames[1]), "and master input file",
        shQuote(argnames[2]), "contain the same number of rows and yet they",
        "disagree in the simulation 'Label's. Master 'Label's replace those",
        "from the datafile."))

      x[, "Label"] <- ml

    } else {
      stop(paste("Datafile", shQuote(argnames[1]), "and the master input file",
        shQuote(argnames[2]), "disagree in the number of rows,",
        paste0("n[datafile] = ", dim(x)[1], " vs. n[master] = ",
          dim(master)[1]),
        "and they disagree in the simulation 'Label's.",
        "'rSFSW2' cannot continue."))
    }
  }

  x
}


#' Load pre-processed simulation project inputs
#'
#' @param SFSW2_prj_meta A list.
#' @param verbose A logical value.
#' @return A list \code{SFSW2_prj_inputs}.
#' @export
load_preprocessed_inputs <- function(SFSW2_prj_meta, verbose = FALSE) {
  if (file.exists(SFSW2_prj_meta[["fnames_in"]][["fpreprocin"]]) &&
    todo_intracker(SFSW2_prj_meta, "load_inputs", "prepared")) {

    SFSW2_prj_inputs <- process_inputs(SFSW2_prj_meta[["project_paths"]],
      SFSW2_prj_meta[["fnames_in"]], use_preprocin = TRUE, verbose = verbose)

  } else {
    stop("'load_preprocessed_inputs': cannot load pre-processed inputs  ",
      "because they are missing or out of date according to 'SFSW2_prj_meta'.")
  }

  SFSW2_prj_inputs
}


#' Load and prepare inputs for a \pkg{rSFSW2} simulation project
process_inputs <- function(project_paths, fnames_in, use_preprocin = TRUE,
  verbose = FALSE) {

  temp_call <- shQuote(match.call()[1])
  if (verbose) {
    t1 <- Sys.time()
    print(paste0("rSFSW2's ", temp_call, ": started at ", t1))

    on.exit({
      print(paste0("rSFSW2's ", temp_call, ": ended after ",
      round(difftime(Sys.time(), t1, units = "secs"), 2), " s")); cat("\n")
      },
      add = TRUE)
  }

  do_check_include <- FALSE

  if (!use_preprocin || !file.exists(fnames_in[["fpreprocin"]])) {

    SWRunInformation <- tryCatch(SFSW2_read_csv(fnames_in[["fmaster"]]),
      error = print)
    stopifnot(sapply(req_fields_SWRunInformation(),
        function(x) x %in% names(SWRunInformation)),    # required columns
      nrow(SWRunInformation) > 0,
      # consecutive site_id:
      all(SWRunInformation$site_id == seq_len(nrow(SWRunInformation))),
      # no space-characters in label:
      !grepl("[[:space:]]", SWRunInformation$Label),
      # no space-characters in weather-data names:
      !grepl("[[:space:]]", SWRunInformation$WeatherFolder)
    )
    include_YN <- as.logical(SWRunInformation$Include_YN)
    nrowsClasses <- max(dim(SWRunInformation)[1], 25L, na.rm = TRUE)

    sw_input_soillayers <- tryCatch(SFSW2_read_csv(fnames_in[["fslayers"]],
      nrowsClasses = nrowsClasses), error = print)
    sw_input_soillayers <- fix_rowlabels(sw_input_soillayers, SWRunInformation,
      verbose = verbose)
    sw_input_soillayers[, - (1:2)] <- check_monotonic_increase(
      data.matrix(sw_input_soillayers[, - (1:2)]), strictly = TRUE, fail = TRUE,
      na.rm = TRUE)

    temp <- tryCatch(SFSW2_read_inputfile(fnames_in[["ftreatDesign"]],
      nrowsClasses = nrowsClasses), error = print)
    sw_input_treatments_use <- temp[["use"]]
    sw_input_treatments <- temp[["data"]]
    sw_input_treatments <- fix_rowlabels(sw_input_treatments, SWRunInformation,
      verbose = verbose)
    # no space-characters in weather-data names:
    stopifnot(!grepl("[[:space:]]", sw_input_treatments$LookupWeatherFolder))

    temp <- tryCatch(SFSW2_read_inputfile(fnames_in[["fexpDesign"]],
      nrowsClasses = nrowsClasses), error = print)
    sw_input_experimentals_use <- temp[["use"]]
    sw_input_experimentals <- temp[["data"]]
    create_experimentals <-
      names(sw_input_experimentals_use[sw_input_experimentals_use])
    # no space-characters in weather-data names:
    stopifnot(!grepl("[[:space:]]", sw_input_experimentals$LookupWeatherFolder))

    temp <- tryCatch(SFSW2_read_inputfile(fnames_in[["fclimnorm"]],
      nrowsClasses = nrowsClasses), error = print)
    sw_input_cloud_use <- temp[["use"]]
    sw_input_cloud <- temp[["data"]]
    sw_input_cloud <- fix_rowlabels(sw_input_cloud, SWRunInformation,
      verbose = verbose)

    temp <- tryCatch(SFSW2_read_inputfile(fnames_in[["fvegetation"]],
      nrowsClasses = nrowsClasses), error = print)
    sw_input_prod <- temp[["data"]]
    sw_input_prod <- fix_rowlabels(sw_input_prod, SWRunInformation,
      verbose = verbose)
    sw_input_prod_use <- temp[["use"]]

    temp <- tryCatch(SFSW2_read_inputfile(fnames_in[["fsite"]],
      nrowsClasses = nrowsClasses), error = print)
    sw_input_site <- temp[["data"]]
    sw_input_site <- fix_rowlabels(sw_input_site, SWRunInformation,
      verbose = verbose)
    sw_input_site_use <- temp[["use"]]

    temp <- tryCatch(SFSW2_read_inputfile(fnames_in[["fsoils"]],
      nrowsClasses = nrowsClasses), error = print)
    sw_input_soils_use <- temp[["use"]]
    sw_input_soils <- temp[["data"]]
    sw_input_soils <- fix_rowlabels(sw_input_soils, SWRunInformation,
      verbose = verbose)

    temp <- tryCatch(SFSW2_read_inputfile(fnames_in[["fweathersetup"]],
      nrowsClasses = nrowsClasses), error = print)
    sw_input_weather_use <- temp[["use"]]
    sw_input_weather <- temp[["data"]]
    sw_input_weather <- fix_rowlabels(sw_input_weather, SWRunInformation,
      verbose = verbose)

    temp <- tryCatch(SFSW2_read_inputfile(fnames_in[["fclimscen_delta"]],
      nrowsClasses = nrowsClasses), error = print)
    sw_input_climscen_use <- temp[["use"]]
    sw_input_climscen <- temp[["data"]]
    sw_input_climscen <- fix_rowlabels(sw_input_climscen, SWRunInformation,
      verbose = verbose)

    temp <- tryCatch(SFSW2_read_inputfile(fnames_in[["fclimscen_values"]],
      nrowsClasses = nrowsClasses), error = print)
    sw_input_climscen_values_use <- temp[["use"]]
    sw_input_climscen_values <- temp[["data"]]
    sw_input_climscen_values <- fix_rowlabels(sw_input_climscen_values,
      SWRunInformation, verbose = verbose)

    # update treatment specifications based on experimental design
    create_treatments <- union(
      names(sw_input_treatments_use)[sw_input_treatments_use],
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
      print(paste("SW treatment is not used because 'rSOILWAT2' package",
        "only uses one version of SOILWAT2. Sorry"))

    tr_files <- load_Rsw_treatment_templates(project_paths, create_treatments,
      "filesin", "swFiles")
    tr_prod <- load_Rsw_treatment_templates(project_paths, create_treatments,
      "prodin", "swProd")
    tr_site <- load_Rsw_treatment_templates(project_paths, create_treatments,
      "siteparamin", "swSite")
    tr_soil <- load_Rsw_treatment_templates(project_paths, create_treatments,
      "soilsin", "swSoils")
    tr_weather <- load_Rsw_treatment_templates(project_paths, create_treatments,
      "weathersetupin", "swWeather")
    tr_cloud <- load_Rsw_treatment_templates(project_paths, create_treatments,
      "cloudin", "swCloud")

    tr_input_CarbonScenario <- tr_input_climPPT <- list()
    tr_input_climTemp <- tr_input_shiftedPPT <- list()
    tr_input_EvapCoeff <- tr_input_TranspCoeff_Code <- list()
    tr_input_TranspRegions <- tr_input_TranspCoeff <- list()
    tr_input_SnowD <- tr_VegetationComposition <- list()

    if (any(create_treatments == "LookupClimatePPTScenarios")) {
      tr_input_climPPT <- SFSW2_read_csv(
        fnames_in[["LookupClimatePPTScenarios"]])
    }

    if (any(create_treatments == "LookupCarbonScenarios")) {
      tr_input_CarbonScenario <- SFSW2_read_csv(
        fnames_in[["LookupCarbonScenarios"]])
    }

    if (any(create_treatments == "LookupClimateTempScenarios")) {
      tr_input_climTemp <- SFSW2_read_csv(
        fnames_in[["LookupClimateTempScenarios"]])
    }

    if (any(create_treatments == "LookupShiftedPPTScenarios")) {
      tr_input_shiftedPPT <- SFSW2_read_csv(
        fnames_in[["LookupShiftedPPTScenarios"]], row.names = 1)
    }

    if (any(create_treatments == "LookupEvapCoeffFromTable")) {
      tr_input_EvapCoeff <- SFSW2_read_csv(
        fnames_in[["LookupEvapCoeffFromTable"]], row.names = 1)
    }

    if (any(grepl("LookupTranspCoeffFromTable_", create_treatments),
        create_treatments == "AdjRootProfile")) {
      tr_input_TranspCoeff_Code <- tryCatch(utils::read.csv(
        fnames_in[["LookupTranspCoeffFromTable"]], nrows = 2,
        stringsAsFactors = FALSE), error = print)
      tr_input_TranspCoeff_Code <- tr_input_TranspCoeff_Code[-2, ]
      tr_input_TranspCoeff <- utils::read.csv(
        fnames_in[["LookupTranspCoeffFromTable"]], skip = 2,
        stringsAsFactors = FALSE)
      colnames(tr_input_TranspCoeff) <- colnames(tr_input_TranspCoeff_Code)
    }

    if (any(create_treatments == "LookupTranspRegionsFromTable"))
      tr_input_TranspRegions <- utils::read.csv(
        fnames_in[["LookupTranspRegionsFromTable"]], row.names = 1,
        stringsAsFactors = FALSE)

    if (any(create_treatments == "LookupSnowDensityFromTable"))
      tr_input_SnowD <- utils::read.csv(
        fnames_in[["LookupSnowDensityFromTable"]], row.names = 1,
        stringsAsFactors = FALSE)

    if (any(create_treatments == "AdjMonthlyBioMass_Temperature"))
      tr_VegetationComposition <- utils::read.csv(
        fnames_in[["LookupVegetationComposition"]], skip = 1, row.names = 1,
        stringsAsFactors = FALSE)


    #-import regeneration data
    ftemp_GISSM <- list.files(project_paths[["dir_in_gissm"]], pattern = ".csv")
    GISSM_species_No <- length(ftemp_GISSM)

    if (GISSM_species_No > 0) {
      f.temp <- utils::read.csv(
        file.path(project_paths[["dir_in_gissm"]], ftemp_GISSM[1]),
        stringsAsFactors = FALSE)
      GISSM_params <- matrix(NA, nrow = nrow(f.temp), ncol = GISSM_species_No)
      colnames(GISSM_params) <- sub(".csv", "", ftemp_GISSM)
      rownames(GISSM_params) <- f.temp[, 1]
      GISSM_params[, 1] <- f.temp[, 2]

      if (GISSM_species_No > 1) for (f in 2:GISSM_species_No) {
        f.temp <- utils::read.csv(
          file.path(project_paths[["dir_in_gissm"]], ftemp_GISSM[f]),
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
      create_experimentals = create_experimentals,
      create_treatments = create_treatments,
      sw_input_soillayers = sw_input_soillayers,
      sw_input_treatments_use = sw_input_treatments_use,
      sw_input_treatments = sw_input_treatments,
      sw_input_experimentals_use = sw_input_experimentals_use,
      sw_input_experimentals = sw_input_experimentals,
      sw_input_cloud_use = sw_input_cloud_use,
      sw_input_cloud = sw_input_cloud,
      sw_input_prod_use = sw_input_prod_use,
      sw_input_prod = sw_input_prod,
      sw_input_site_use = sw_input_site_use,
      sw_input_site = sw_input_site,
      sw_input_soils_use = sw_input_soils_use,
      sw_input_soils = sw_input_soils,
      sw_input_weather_use = sw_input_weather_use,
      sw_input_weather = sw_input_weather,
      sw_input_climscen_use = sw_input_climscen_use,
      sw_input_climscen = sw_input_climscen,
      sw_input_climscen_values_use = sw_input_climscen_values_use,
      sw_input_climscen_values = sw_input_climscen_values,
      tr_files = tr_files, tr_prod = tr_prod, tr_site = tr_site,
      tr_soil = tr_soil, tr_weather = tr_weather, tr_cloud = tr_cloud,
      tr_input_CarbonScenario = tr_input_CarbonScenario,
      tr_input_climPPT = tr_input_climPPT,
      tr_input_climTemp = tr_input_climTemp,
      tr_input_shiftedPPT = tr_input_shiftedPPT,
      tr_input_EvapCoeff = tr_input_EvapCoeff,
      tr_input_TranspCoeff_Code = tr_input_TranspCoeff_Code,
      tr_input_TranspCoeff = tr_input_TranspCoeff,
      tr_input_TranspRegions = tr_input_TranspRegions,
      tr_input_SnowD = tr_input_SnowD,
      tr_VegetationComposition = tr_VegetationComposition,
      GISSM_params = GISSM_params, GISSM_species_No = GISSM_species_No
    )

    inputs <- list2env(x = temp, envir = new.env(parent = emptyenv()))

    saveRDS(inputs, file = fnames_in[["fpreprocin"]])

  } else {
    inputs <- readRDS(fnames_in[["fpreprocin"]])
  }

  inputs[["do_check_include"]] <- do_check_include

  if (!is.environment(inputs) || length(inputs) == 0) {
    print(paste0("rSFSW2's ", temp_call, ": failed; 'SFSW2_prj_inputs' is ",
      "empty or not of type 'environment'."))
  }

  inputs
}


#' Serialization Interface for Single Objects with backup
#'
#' Function to write a single object to file, but create a backup file first if
#' an older version of the file exists. This backup is restored in case the
#' writing to the file fails. Situations where \code{\link{saveRDS}} may fail
#' include forced termination of the running R process (e.g., \var{HPC}
#' schedulers); those situations likely will not allow that the original file be
#' restored from the backup -- this will have to be done manually.
#'
#' @inheritParams base::saveRDS
#' @param tag_backup A character string. A tag that is appended at the end of
#'   the \code{file} name to identify the backup.
#'
#' @seealso \code{\link{saveRDS}}
#' @export
save_to_rds_with_backup <- function(object, file, tag_backup = "backup", ...) {
  if (file.exists(file)) {
    temp <- strsplit(basename(file), split = ".", fixed = TRUE)[[1]]
    fbackup <- paste0(paste(temp[-length(temp)], collapse = ""), "_",
      tag_backup, ".", temp[length(temp)])

    file.rename(from = file, to = file.path(dirname(file), fbackup))
  }

  temp <- try(saveRDS(object, file = file, ...))
  res <- !inherits(temp, "try-error")

  if (!res) {
    print(paste("'save_to_rds_with_backup': saving object to",
      shQuote(basename(file)), "has failed; restoring from backup if",
      "available..."))
    file.rename(from = file.path(dirname(file), fbackup), to = file)
    print(paste("'save_to_rds_with_backup': restoring from backup completed."))
  }

  invisible(res)
}



#------ End of datafile-IO functions
########################
