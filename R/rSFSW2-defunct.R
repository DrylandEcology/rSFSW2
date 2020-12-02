#' Defunct functions in package \pkg{rSFSW2}
#'
#' Executing a defunct function will fail and tell you which function
#' replaces them.
#'
#' @name rSFSW2-defunct
NULL


#' @rdname rSFSW2-defunct
#' @export
calc_BareSoilEvapCoefs <- function(...) {
  .Defunct(
    new = "calc_BareSoilEvapCoefs",
    package = "rSW2data",
    msg = paste(
      "`rSFSW2::calc_BareSoilEvapCoefs()`",
      "is defunct after v4.3.1;",
      "please use",
      "`rSW2data::calc_BareSoilEvapCoefs()`",
      "instead."
    )
  )
}

#' @rdname rSFSW2-defunct
#' @export
crs_units <- function(...) {
  .Defunct(
    new = "crs_units",
    package = "rSW2st",
    msg = paste(
      "`rSFSW2::crs_units()`",
      "is defunct after v4.3.1;",
      "please use",
      "`rSW2st::crs_units()`",
      "instead."
    )
  )
}

#' @rdname rSFSW2-defunct
#' @export
init_soiltemperature <- function(...) {
  .Defunct(
    new = "init_soiltemperature",
    package = "rSW2data",
    msg = paste(
      "`rSFSW2:::init_soiltemperature()`",
      "is defunct after v4.3.1;",
      "please use",
      "`rSW2data::init_soiltemperature()`",
      "instead."
    )
  )
}

