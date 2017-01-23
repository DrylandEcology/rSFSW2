.onAttach <- function(libname, pkgname) {
  if (interactive()) {
      meta <- packageDescription("rSWSF")
      packageStartupMessage("Package 'rSWSF', ", meta$Version, " (", meta$Date, ") attached/loaded.")
  }

  invisible()
}


.onLoad <- function(libname, pgkname) {
  #--- Define options and set default values
  # based on chapter "When you do need side-effects" by Wickham, H. 2015. R packages. O'Reilly and Associates.
  op_old <- options()
  op_SWSF <- list()
  toset <- !(names(op_SWSF) %in% names(op_old))
  if (any(toset)) options(op_SWSF[toset])

  #--- Define package level variables that should be hidden from package user and should not be changed
  assign("var1", 0L, envir = swsf_vars) #test

  invisible()
}



.onUnload <- function(libpath) {
  #--- Remove package options
  op_old <- options()
  op_SWSF <- list()
  toset <- names(op_SWSF) %in% names(op_old)
  if (any(toset)) options(op_SWSF[toset])

  invisible()
}