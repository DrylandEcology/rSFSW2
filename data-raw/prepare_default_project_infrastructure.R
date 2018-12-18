#!/usr/bin/env Rscript

stopifnot(requireNamespace("usethis"))

# default (input) infrastructure of a rSFSW2 simulation project

dir_definf <- file.path("data-raw", "1_Input")


# update example treatment input files from (installed) rSOILWAT2 defaults
sw_in <- rSOILWAT2::sw_exampleData

tr_update <- list(
  tr_cloudin = basename(rSOILWAT2::swFiles_Cloud(sw_in)),
  tr_prodin = basename(rSOILWAT2::swFiles_Prod(sw_in)),
  tr_siteparamin = basename(rSOILWAT2::swFiles_SiteParams(sw_in)),
  tr_soilsin = basename(rSOILWAT2::swFiles_Soils(sw_in)),
  tr_weathersetupin = basename(rSOILWAT2::swFiles_WeatherSetup(sw_in)))

path_demo <- system.file("extdata", "example1", "Input", package = "rSOILWAT2")

for (k in seq_along(tr_update)) {
  tr_dir <- file.path(dir_definf, "treatments", names(tr_update)[k])
  files_has <- list.files(tr_dir, full.names = TRUE)

  file_should <- file.path(path_demo, tr_update[[k]])
  file_new <- file.path(tr_dir, tr_update[[k]])

  # Check whether existing file is up-to-date and the only one
  do_update <- length(files_has) != 1 ||
    basename(files_has) != tr_update[[k]] ||
    !isTRUE(all.equal(readLines(file_should), readLines(files_has)))

  if (do_update) {
    unlink(files_has)
    file.copy(from = file_should, to = file_new)
  }
}



# create an internal package data object

ftemp <- list.files(dir_definf, recursive = TRUE, full.names = TRUE)

definf <- list()

for (k in seq_along(ftemp)) {
  # Path (but remove 'data-raw'
  ptemp <- dirname(ftemp[k])
  ptemp <- strsplit(ptemp, split = .Platform$file.sep, fixed = TRUE)[[1]]
  ptemp <- paste0(ptemp[-1], collapse = .Platform$file.sep)
  btemp <- basename(ftemp[k])

  # File extension: # should be one of c("csv", "in", "YYYY") where YYYY is a
  # calendar year (weather input data)
  ttemp <- strsplit(btemp, split = ".", fixed = TRUE)[[1]]
  ttemp <- ttemp[length(ttemp)]

  definf[[k]] <- list(
    path = ptemp,
    fname = btemp,
    ftype = ttemp,
    data = memCompress(readLines(ftemp[k]), type = "gzip")
  )
}

usethis::use_data(definf, internal = TRUE, overwrite = TRUE)
