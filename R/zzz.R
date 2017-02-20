
.onAttach <- function(libname, pkgname) {
  if (interactive()) {
      meta <- utils::packageDescription(pkgname)
      packageStartupMessage("Package ", shQuote(pkgname), " v", meta$Version, " (",
        meta$Date,") attached/loaded.")
  }

  invisible()
}


.onLoad <- function(libname, pkgname) {
  #--- Define options and set default values
  # based on chapter "When you do need side-effects" by Wickham, H. 2015. R packages. O'Reilly and Associates.
  op_old <- options()
  op_SWSF <- list()
  toset <- !(names(op_SWSF) %in% names(op_old))
  if (any(toset)) options(op_SWSF[toset])

  #--- Define package level variables that should be hidden from package user and should not be changed
  assign("minVersion_dbWeather", numeric_version("3.1.0"), envir = swsf_glovars)

  assign("slyrs_maxN", 20L, envir = swsf_glovars) # number of implemented soil layers
  assign("slyrs_ids", seq_len(swsf_glovars[["slyrs_maxN"]]), envir = swsf_glovars)

  assign("windspeed_height_m", 2L, envir = swsf_glovars) # SOILWAT2 assumes 2 m height

  assign("tol", sqrt(.Machine$double.eps), envir = swsf_glovars)
  assign("toln", sqrt(.Machine$double.neg.eps), envir = swsf_glovars)

  assign("st_mo", seq_len(12L), envir = swsf_glovars)


  invisible()
}



.onUnload <- function(libpath) {
  #--- Remove package options
  op_old <- options()
  op_SWSF <- list()
  toset <- names(op_SWSF) %in% names(op_old)
  if (any(toset)) options(op_SWSF[toset])

  #--- Clean up C code
  library.dynam.unload("rSWSF", libpath)

  invisible()
}
