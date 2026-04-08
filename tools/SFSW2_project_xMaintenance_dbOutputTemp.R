#!/usr/bin/env Rscript

#------------------------------------------------------------------------------#
# rSFSW2: FRAMEWORK FOR SOILWAT2 SIMULATIONS: CREATING SIMULATION RUNS,
#         EXECUTING SIMULATIONS, AND AGGREGATING OUTPUTS

#----- LICENSE
#    Copyright (C) 2017-2019 by `r packageDescription("rSFSW2")[["Author"]]`
#    Contact information `r packageDescription("rSFSW2")[["Maintainer"]]`

#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, version 3 of the License.

#------ DISCLAIMER:
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.

#------ NOTES:
#  - You get an overview by: `r package?rSFSW2`
#  - An index of functionality is displayed by: `r help(package = "rSFSW2")`
#------------------------------------------------------------------------------#


################################################################################


#--- USER INPUTS
## Initialize simulation project folder?
update <- FALSE

## Adjust `dir_out` element of `project_paths` in local copy of metadata:
do_adjust_dir_out <- FALSE

## Relative location of temporary output files
dir_temp <- "temp"

## If TRUE, use `fname_dbOutput_concat` instead of metadata information
do_use_dbOutput_concat <- FALSE
## Name of copy of dbOutput used for moving output data into
fname_dbOutput_concat <- "dbOutput_concating.sqlite3"

## Check data for possible duplicates or deviation in values?
check_if_Pid_present <- FALSE



#--- LOAD SIMULATION PROJECT META-DATA / DESCRIPTION FILE
t_job_start <- Sys.time()
library("rSFSW2")

dir_prj <- getwd()
fmeta <- file.path(dir_prj, "SFSW2_project_descriptions.rds")


SFSW2_prj_meta <- if (update || !file.exists(fmeta)) {
rSFSW2::init_rSFSW2_project(
  fmetar = file.path(dir_prj, "SFSW2_project_descriptions.R"), update = TRUE)
} else {
  readRDS(fmeta)
}

source(
  file.path(dir_prj, "SFSW2_project_settings.R"),
  verbose = FALSE,
  keep.source = FALSE
)

## Set `project_paths` on local copy of `SFSW2_prj_meta`
if (do_adjust_dir_out) {
  # Location of dbWork
  SFSW2_prj_meta[["project_paths"]][["dir_out"]] <- file.path(
    SFSW2_prj_meta[["project_paths"]][["dir_prj"]], "4_Simulation")

  # Location of concatFile and failedFile
  SFSW2_prj_meta[["project_paths"]][["dir_out_temp"]] <- file.path(
    SFSW2_prj_meta[["project_paths"]][["dir_out"]], "temp")

  # Location of dbOutput
  SFSW2_prj_meta[["fnames_out"]][["dbOutput"]] <- file.path(
    SFSW2_prj_meta[["project_paths"]][["dir_out"]],
    basename(SFSW2_prj_meta[["fnames_out"]][["dbOutput"]]))

  # Location of dbOutput_current
  SFSW2_prj_meta[["fnames_out"]][["dbOutput_current"]] <- file.path(
    SFSW2_prj_meta[["project_paths"]][["dir_out"]],
    basename(SFSW2_prj_meta[["fnames_out"]][["dbOutput_current"]]))
}

if (do_use_dbOutput_concat) {
  # Name of dbOutput used for moving data into
  SFSW2_prj_meta[["fnames_out"]][["dbOutput"]] <- file.path(
    SFSW2_prj_meta[["project_paths"]][["dir_out"]], fname_dbOutput_concat)
}

# Location of temporary output files
dir_out_temp <- file.path(
  SFSW2_prj_meta[["project_paths"]][["dir_out"]],
  dir_temp
)
stopifnot(dir.exists(dir_out_temp))


## Turn off debug print statements
opt_verbosity[["print.debug"]] <- FALSE

## Unlimited wall-time
opt_parallel[["opt_job_time"]][["wall_time_s"]] <- Inf


#--- MOVE THE DATA TO `dbOutput`
rSFSW2::move_output_to_dbOutput(
  SFSW2_prj_meta,
  t_job_start,
  opt_parallel,
  opt_behave,
  opt_out_run,
  opt_verbosity,
  dir_out_temp = dir_out_temp,
  check_if_Pid_present = check_if_Pid_present
)
