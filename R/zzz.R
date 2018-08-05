
.onAttach <- function(libname, pkgname) {
  if (interactive()) {
      meta <- utils::packageDescription(pkgname)
      packageStartupMessage("Package ", shQuote(pkgname), " v", meta$Version,
        " (", meta$Date, ") attached/loaded.")
  }

  invisible()
}


.onLoad <- function(libname, pkgname) {
  #--- Define options and set default values
  # based on chapter "When you do need side-effects" by Wickham, H. 2015.
  # R packages. O'Reilly and Associates.
  op_old <- options()
  op_rSFSW2 <- list()
  toset <- !(names(op_rSFSW2) %in% names(op_old))
  if (any(toset)) options(op_rSFSW2[toset])

  #--- Define package level variables that should be hidden from package user
  # and should not be changed
  assign("minVersion_dbWeather", numeric_version("3.1.0"),
    envir = SFSW2_glovars)

  # number of implemented soil layers
  assign("slyrs_maxN", 20L, envir = SFSW2_glovars)
  assign("slyrs_ids", seq_len(SFSW2_glovars[["slyrs_maxN"]]),
    envir = SFSW2_glovars)

  # SOILWAT2 assumes 2 m height
  assign("windspeed_height_m", 2L, envir = SFSW2_glovars)

  assign("tol", sqrt(.Machine$double.eps), envir = SFSW2_glovars)
  assign("toln", sqrt(.Machine$double.neg.eps), envir = SFSW2_glovars)

  assign("st_mo", seq_len(12L), envir = SFSW2_glovars)

  #-- Parallel framework: see \code{\link{init_SFSW2_cluster}}
  assign("p_type", NA_character_, envir = SFSW2_glovars)
  assign("p_workersN", 1L, envir = SFSW2_glovars)
  assign("p_cl", NULL, envir = SFSW2_glovars)
  assign("p_has", FALSE, envir = SFSW2_glovars)
  assign("p_pids", NULL, envir = SFSW2_glovars)
  assign("lockfile", NULL, envir = SFSW2_glovars)
  # Worker tag: this needs to be an object with a name starting with a dot as
  #  in '.x' so that it does not get deleted by `rm(list = ls())`
  assign("p_wtag", ".node_id", envir = SFSW2_glovars)

  invisible()
}



.onUnload <- function(libpath) {
  #--- Remove package options
  op_old <- options()
  op_rSFSW2 <- list()
  toset <- names(op_rSFSW2) %in% names(op_old)
  if (any(toset)) options(op_rSFSW2[toset])

  #--- Clean up C code
  library.dynam.unload("rSFSW2", libpath)

  invisible()
}
