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
#' @param ... Further arguments to be passed to \code{\link[iotools]{read.csv.raw}} or
#'  \code{\link[utils]{read.csv}}.
#'
#' @return A data frame (\code{\link[base]{data.frame}}) containing a representation of
#'  the data in the file.
swsf_read_csv <- compiler::cmpfun(function(file, stringsAsFactors = FALSE,
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

  res
})


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
#'    always \code{FALSE}.
#'  \item{data}{A data frame (\code{\link[base]{data.frame}}) containing a representation
#'    of the values in the \code{file} with column names from the first row of the
#'    \code{file}.}
#' }
swsf_read_inputfile <- compiler::cmpfun(function(file, header_rows = 1, use_iotools = TRUE,
  ...) {

  sw_use <- tryCatch(swsf_read_csv(file, nrows = header_rows, use_iotools = use_iotools),
    error = function(e) print(paste("Failed to read file:", shQuote(basename(file)), "with", e)))
  sw <- swsf_read_csv(file, skip = header_rows, use_iotools = use_iotools, ...)
  names(sw) <- names(sw_use)
  sw_use <- c(FALSE, as.logical(as.numeric(sw_use[, -1])))
  sw_use[is.na(sw_use)] <- FALSE
  names(sw_use) <- names(sw)

  list(use = sw_use, data = sw)
})

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
reconstitute_inputfile <- compiler::cmpfun(function(sw_use, data) {
  temp <- as.data.frame(matrix(as.integer(sw_use), nrow = 1L))
  colnames(temp) <- names(sw_use)
  temp[1, 1] <- "UseInformationToCreateSoilWatRuns"
  rbind(temp, data)
})

check_requested_sites <- function(include_YN, SWRunInformation, fmaster, fpreprocin,
  verbose = FALSE) {

  includes_all_sources <- grep("Include_YN", colnames(SWRunInformation),
    ignore.case = TRUE, value = TRUE)
  do_ignore <- includes_all_sources %in% c("Include_YN", "include_YN_available")
  includes_sources <- includes_all_sources[!do_ignore]

  if (length(includes_sources) > 0L) {
    include_YN_sources <- apply(SWRunInformation[, includes_sources, drop = FALSE], 1,
      function(x) all(x > 0L))

    if (all(include_YN_sources[include_YN > 0L])) {
      if (verbose)
        print(paste("Data sources available for all requested SWSF simulation runs"))

    } else {
      include_YN_available <- rep(0L, dim(SWRunInformation)[1])
      include_YN_available[include_YN_sources] <- 1L
      SWRunInformation[, "include_YN_available"] <- include_YN_available

      write.csv(SWRunInformation, file = fmaster, row.names = FALSE)
      unlink(fpreprocin)

      stop("Data sources not available for every requested SWSF simulation run. ",
        "New column 'include_YN_available' with updated information stored to ",
        "MasterInput file 'SWRunInformation' on disk. SWSF is stopped so that you can ",
        "bring 'include_YN' and 'include_YN_available' in agreement before running ",
        "the simulations.")
    }

  }

  SWRunInformation
}


map_input_variables <- function(map_vars, SWRunInformation, sw_input_soillayers,
  sw_input_cloud_use, sw_input_cloud, sw_input_prod_use, sw_input_prod, sw_input_site_use,
  sw_input_site, sw_input_soils_use, sw_input_soils, sw_input_weather_use,
  sw_input_weather, sw_input_climscen_use, sw_input_climscen, sw_input_climscen_values_use,
  sw_input_climscen_values, runIDs_sites, sim_cells_or_points, run_sites, crs_sites,
  sim_crs, sim_raster, dir.out, verbose = FALSE) {

  if (verbose)
    print(paste("SWSF generates maps of input variables for quality control: started at",
    t1 <- Sys.time()))

  dir.inmap <- file.path(dir.out, "Input_maps")
  dir.create(dir.inmap, showWarnings = FALSE)

  input_avail <- list(
    SWRunInformation = list(
      cols = names(SWRunInformation), use = rep(TRUE, ncol(SWRunInformation))),
    sw_input_soillayers = list(
      cols = names(sw_input_soillayers), use = rep(TRUE, ncol(sw_input_soillayers))),
    sw_input_cloud = list(
      cols = names(sw_input_cloud), use = sw_input_cloud_use),
    sw_input_prod = list(
      cols = names(sw_input_prod), use = sw_input_prod_use),
    sw_input_site = list(
      cols = names(sw_input_site), use = sw_input_site_use),
    sw_input_soils = list(
      cols = names(sw_input_soils), use = sw_input_soils_use),
    sw_input_weather = list(
      cols = names(sw_input_weather), use = sw_input_weather_use),
    sw_input_climscen = list(
      cols = names(sw_input_climscen), use = sw_input_climscen_use),
    sw_input_climscen_values = list(
      cols = names(sw_input_climscen_values), use = sw_input_climscen_use)
  )

  for (iv in seq_along(map_vars)) {
    iv_locs <- lapply(input_avail, function(ina)
      grep(map_vars[iv], ina$cols[ina$use], ignore.case = TRUE, value = TRUE))
    iv_locs <- iv_locs[lengths(iv_locs) > 0]

    if (length(iv_locs) > 0) {
      dir.create(dir.inmapvar <- file.path(dir.inmap, map_vars[iv]), showWarnings = FALSE)

      for (it1 in seq_along(iv_locs)) for (it2 in seq_along(iv_locs[[it1]])) {
        dat <- get(names(iv_locs)[it1])[runIDs_sites, iv_locs[[it1]][it2]]
        dat <- try(as.numeric(dat), silent = TRUE) # e.g., sw_input_cloud[, "SnowD_Hemisphere"] contains only strings for which as.numeric() issues a warning

        # this code plots only numeric maps
        if (any(is.finite(dat)) && !inherits(dat, "try-error")) {
          names(dat) <- iv_locs[[it1]][it2]

          map_flag <- paste(names(iv_locs)[it1], iv_locs[[it1]][it2],
            sim_cells_or_points, sep = "_")

          # Convert data to spatial object
          if (sim_cells_or_points == "point") {
            sp_dat <- as(run_sites, "SpatialPointsDataFrame")
            temp <- as.data.frame(dat)
            colnames(temp) <-  iv_locs[[it1]][it2]
            methods::slot(sp_dat, "data") <- temp

            if (!raster::compareCRS(crs_sites, sim_crs)) {
              sp_dat <- sp::spTransform(sp_dat, CRS = sim_crs)
            }

          } else if (sim_cells_or_points == "cell") {
            # if failing, then need a more sophisticated assignment of values than
            # implemented below
            stopifnot(raster::canProcessInMemory(sim_raster))

            temp <- run_sites
            if (!raster::compareCRS(crs_sites, sim_crs)) {
              temp <- sp::spTransform(temp, CRS = sim_crs)
            }

            # init with NAs
            sp_dat <- raster::init(sim_raster, fun = function(x) rep(NA, x))
            sp_dat[raster::cellFromXY(sp_dat, sp::coordinates(temp))] <- dat
          }

          # Save to disk
          saveRDS(sp_dat, file = file.path(dir.inmapvar, paste0(map_flag, ".rds")))

          # Figure
          png(height = 10, width = 6, units = "in", res = 200,
            file = file.path(dir.inmapvar, paste0(map_flag, ".png")))
          par_old <- par(mfrow = c(2, 1), mar = c(2.5, 2.5, 0.5, 0.5),
            mgp = c(1.25, 0.25, 0), tcl = 0.5, cex = 1)

          # panel a: map
          n_cols <- 255
          cols <- rev(terrain.colors(7))
          cols[1] <- "gray"
          cols <- colorRampPalette(c(cols, "dodgerblue3"))(n_cols)
          if (sim_cells_or_points == "point") {
            par1 <- par(mar = c(2.5, 2.5, 0.5, 8.5))
            cdat <- cut(dat, n_cols)
            p_size <- function(x) max(0.25, min(2, 100 / x))
            sp::plot(sp_dat, col = cols[as.integer(cdat)], pch = 15,
              cex = p_size(length(dat)), axes = TRUE, asp = 1)
            # legend
            ids <- round(seq(1, n_cols, length.out = 12))
            lusr <- par("usr")
            lxy <- cbind(rep(lusr[2] + (lusr[2] - lusr[1]) / 15, 12),
              lusr[3] + (lusr[4] - lusr[3]) / 4 + seq(0, 1, length.out = 12) *
              (lusr[4] - lusr[3]) / 2)
            points(lxy, col = cols[ids], pch = 15, cex = 2, xpd = NA)
            text(lxy, pos = 4, labels = levels(cdat)[ids], xpd = NA)
            par(par1)

          } else if (sim_cells_or_points == "cell") {
            raster::plot(sp_dat, col = cols, asp = 1)
          }
          mtext(side = 3, line = -1, adj = 0.03, text = paste0("(", letters[1], ")"),
            font = 2)

          # panel b: histogram
          hist(dat, xlab = paste(names(iv_locs)[it1], iv_locs[[it1]][it2]), main = "")
          mtext(side = 3, line = -1, adj = 0.03, text = paste0("(", letters[2], ")"),
            font = 2)

          par(par_old)
          dev.off()
        }
      }
    }
  }

  if (verbose)
    print(paste("SWSF input maps: ended after",
      round(difftime(Sys.time(), t1, units = "secs"), 2), "s"))

  invisible(TRUE)
}

read_SoilWat_FileDefaults <- function(dir.sw.in, swFiles_tag = "file") {
  temp <- list.files(dir.sw.in)
  swFilesIn <- grep(swFiles_tag, temp, value = TRUE)[1]

  if (length(swFilesIn) == 0 || is.na(swFilesIn))
    stop("'read_SoilWat_FileDefaults': cannot find SOILWAT's overview file ",
      shQuote(swFiles_tag), " in folder ", shQuote(dir.sw.in))

  # 'swDataFromFiles' acts as the basis for all runs
  swDataFromFiles <- Rsoilwat31::sw_inputDataFromFiles(dir = dir.sw.in, files.in = swFilesIn)
  # we don't need the example weather data; the code will get weather data separately
  if (length(swDataFromFiles@weatherHistory) > 0)
    swDataFromFiles@weatherHistory <- list(Rsoilwat31::swClear(swDataFromFiles@weatherHistory[[1]]))

  swDataFromFiles
}


#------ End of datafile-IO functions
########################
