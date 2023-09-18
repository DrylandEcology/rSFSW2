#--- Zip each folder inside 3_Runs of a rSFSW2 project ------

library("foreach") # required for `%dopar%`

delete <- TRUE # TRUE, delete original files on success


#------ Grab command line arguments (if any) ------
# e.g., `Rscript zip_3runs.R -path=. -nparallel=10 -delete`

args <- commandArgs(trailingOnly = TRUE)

dir_prj <- if (any(ids <- grepl("-path", args))) {
  sub("-path=", "", args[ids])
} else {
  "."
}

nparallel <- if (any(ids <- grepl("-nparallel", args))) {
  as.integer(sub("-nparallel=", "", args[ids]))
} else {
  0L
}

delete <- if (any(ids <- grepl("-delete", args))) {
  TRUE
} else {
  delete
}


#--- Paths ------
dir_runs <- file.path(dir_prj, "3_Runs")

fname_log <- file.path(
  dir_prj,
  "logs",
  paste0(
    "log_zipping_3Runs_",
    format(Sys.time(), "%Y%m%d-%H%M%S"),
    ".txt"
  )
)

dir.create(dirname(fname_log), recursive = TRUE, showWarnings = FALSE)


#--- List all simulation run folders ------
ftmps_runs <- list.files(
  dir_runs,
  recursive = FALSE,
  include.dirs = TRUE,
  full.names = TRUE
)

#--- Remove folders already zipped up ------
ftmps_runs <- grep(".zip$", ftmps_runs, value = TRUE, invert = TRUE)


#--- Set up parallel ------
if (nparallel > 0) {
  cl <- parallel::makeCluster(spec = nparallel, outfile = fname_log)
  doParallel::registerDoParallel(cl)
} else {
  foreach::registerDoSEQ()
}


#--- Do the zipping & deleting ------
cat(
  format(Sys.time(), "%Y%m%d-%H%M%S"),
  "starting to process '3_Runs' n =", length(ftmps_runs),
  "on cores n = ", nparallel,
  fill = TRUE
)

fname_run <- NULL # define to avoid "Undefined global functions or variables"

res <- foreach::foreach(
  fname_run = ftmps_runs,
  .combine = "+",
  .init = 0,
  .inorder = FALSE,
  .errorhandling = "remove"
) %dopar% {
  fname_zip <- file.path(
    dirname(fname_run),
    paste0(basename(fname_run), ".zip")
  )

  # Zip
  #   * r: recursively
  #   * 0, no compression
  #   * j: junk paths (relative to script): just store files and folder
  #   * T: test integrity of the new zip file
  #   * q: quiet
  #   * don't exclude extras such as file times
  ret <- utils::zip(
    zipfile = fname_zip,
    files = fname_run,
    flags = "-jrTq0"
  )

  # Delete original files only if zip file exists (and was successful)
  res <- 0L

  if (ret == 0 && file.exists(fname_zip)) {
    if (delete) {
      unlink(fname_run, recursive = TRUE)
      if (dir.exists(fname_run)) res <- 1L # return +1 if successful
    } else {
      res <- 1L
    }
  }

  res
}

cat(
  format(Sys.time(), "%Y%m%d-%H%M%S"),
  "successfully processed n =", res,
  fill = TRUE
)


#--- Clean up parallel ------
if (nparallel > 0) {
  parallel::stopCluster(cl)
}
