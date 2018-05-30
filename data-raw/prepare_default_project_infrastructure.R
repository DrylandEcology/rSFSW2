#!/usr/bin/env Rscript

# default (input) infrastructure of a rSFSW2 simulation project

dir_definf <- file.path("data-raw", "1_Input")


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

devtools::use_data(definf, internal = TRUE)
