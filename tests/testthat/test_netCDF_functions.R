context("netCDF file interaction")

# skip_on_travis() and skip_on_appveyor() are meant to be used within
# test_that() calls. Here, we need to skip preparation code outside of a
# test_that call. However, the skip_* functions cause CIs to error out, even
# when wrapped in try() statements, with
#   - "Error: On Appveyor", respectively
#   - "Error: On Travis"
# Check values of the ENV variables directly as a work-around:

do_skip <- c(
  !identical(tolower(Sys.getenv("RSFSW2_ALLTESTS")), "true"),
  # whereas skip_on_cran() skips if not "true", I believe it should skip only
  # if "false" (i.e., not "" and not "true")
  identical(tolower(Sys.getenv("NOT_CRAN")), "false"),
  # mimmics skip_on_travis():
  identical(tolower(Sys.getenv("TRAVIS")), "true"),
  # mimmics skip_on_appveyor():
  identical(tolower(Sys.getenv("APPVEYOR")), "true"))

suppressWarnings(is_online <-
  !inherits(try(close(url(getOption("repos"), open = "r")), silent = TRUE),
  "try-error"))


if (!any(do_skip) && is_online) {

  #--- Inputs
  dir_temp <- tempdir()

  test_ncs <- list(
    list(filename = file.path(dir_temp,
        "pr_Amon_CESM-CAM5.1-FV_1pctCO2_r1i1p1_000101-015012.nc"),
      url = NA,
      expect = structure(list(calendar = "noleap", unit = 1, N = 1800L,
        base = structure(-719162, class = "Date"), start = structure(c(1, 1),
        .Names = c("year", "month")), end = structure(c(150, 12),
        .Names = c("year", "month"))), .Names = c("calendar", "unit", "N",
          "base", "start", "end"))),

    list(filename = file.path(dir_temp,
        "pr_Amon_EC-EARTH-DMI_1pctCO2_r1i1p1_185001-198912.nc"),
      url = NA,
      expect = structure(list(calendar = "proleptic_gregorian", unit = 1,
        N = 1680L,
        base = structure(list(sec = 0, min = 0L, hour = 0L,
          mday = 16L, mon = 0L, year = -50L, wday = 3L, yday = 15L, isdst = 0L),
          .Names = c("sec", "min", "hour", "mday", "mon", "year", "wday",
            "yday", "isdst"),
          class = c("POSIXlt", "POSIXt"), tzone = "UTC"),
        start = structure(c(1850, 1), .Names = c("year", "month")),
        end = structure(c(1989, 12), .Names = c("year", "month"))),
        .Names = c("calendar", "unit", "N", "base", "start", "end"))),

    list(filename = file.path(dir_temp,
        "pr_Amon_CSIRO-Mk3L-1-2_G1_r1i1p1_000101-007012.nc"),
      url = paste("http://esgf.nci.org.au/thredds/fileServer/geomip/output",
        "UNSW/CSIRO-Mk3L-1-2/G1/mon/atmos/Amon/r1i1p1/v20170728/pr",
        "pr_Amon_CSIRO-Mk3L-1-2_G1_r1i1p1_000101-007012.nc", sep = "/"),
      expect = structure(list(calendar = "365_day", unit = 1, N = 840L,
        base = structure(-719162, class = "Date"), start = structure(c(1, 1),
        .Names = c("year", "month")), end = structure(c(70, 12),
        .Names = c("year", "month"))),
        .Names = c("calendar", "unit", "N", "base", "start", "end"))),

    list(filename = file.path(dir_temp,
        "tasmin_Amon_NorESM1-M_rcp45_r1i1p1_200601-210012.nc"),
      url = paste("http://noresg.norstore.no/thredds/fileServer/esg_dataroot",
        "cmor/CMIP5/output1/NCC/NorESM1-M/rcp45/mon/atmos/Amon/r1i1p1",
        "v20120412/tasmin/tasmin_Amon_NorESM1-M_rcp45_r1i1p1_200601-210012.nc",
        sep = "/"),
      expect = structure(list(calendar = "noleap", unit = 1, N = 1140L,
        base = structure(13149, class = "Date"),
        start = structure(c(2006, 1), .Names = c("year", "month")),
        end = structure(c(2100, 12), .Names = c("year", "month"))),
        .Names = c("calendar", "unit", "N", "base", "start", "end")))
  )

  # Download test files
  is_nc <- function(filename) {
    .local <- function() {
      stopifnot(requireNamespace("ncdf4"))

      nc <- ncdf4::nc_open(filename = filename, write = FALSE, readunlim = TRUE,
        verbose = FALSE)
      ncdf4::nc_close(nc)
    }

    try(.local(), silent = TRUE)
  }

  # suppress warnings if a download fails (e.g., "cannot open URL",
  #   "NetCDF: Unknown file format")
  has_test_ncs <- suppressWarnings(lapply(test_ncs, function(x) {
    if (is.na(x[["url"]]) || is.na(x[["expect"]])) {
      FALSE
    } else {
      has <- try(utils::download.file(url = x[["url"]],
        destfile = x[["filename"]], quiet = TRUE), silent = TRUE)

      if (isTRUE(has == 0) && isTRUE(is_nc(x[["filename"]]))) {
        TRUE
      } else {
        unlink(x[["filename"]])
        FALSE
      }
    }
  }))

  #--- Tests
  test_that("read_time_netCDF:", {
    for (k in seq_along(has_test_ncs)) {
      if (has_test_ncs[[k]]) {
        expect_equal(read_time_netCDF(test_ncs[[k]][["filename"]]),
          test_ncs[[k]][["expect"]],
          info = basename(test_ncs[[k]][["filename"]]))
      }
    }
  })


  #--- Clean up
  unlink(sapply(test_ncs, function(x) x[["filename"]]))
}
