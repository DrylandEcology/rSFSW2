#!/usr/bin/env Rscript

#----------------------------------------------------------------------------------------#
# rSFSW2: FRAMEWORK FOR SOILWAT2 SIMULATIONS: CREATING SIMULATION RUNS, EXECUTING
#        SIMULATIONS, AND AGGREGATING OUTPUTS

#----- LICENSE
#    Copyright (C) 2017 by `r packageDescription("rSFSW2")[["Author"]]`
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
#----------------------------------------------------------------------------------------#


##############################################################################
t_job_start <- Sys.time()

library("rSFSW2")

#------ Turn on/off actions to be carried out by simulation framework
actions <- list(
  # Input preparation
  #   - Create a new(!) weather database and populate with current weather data
  #     (formerly 'createAndPopulateWeatherDatabase')
  #   - "external": pulls data from 'external' data sources from 'dir_external' as
  #     specified by 'req_data'
  prep_inputs = TRUE,

  # Input checking
  check_inputs = TRUE,

  # Simulation runs
  # "sim_create", "sim_execute", and "sim_aggregate" can be used individually if
  # "saveRsoilwatInput" and/or "saveRsoilwatOutput" are true
  #   - Prepare/collect inputs for a rSOILWAT2 run (formerly, 'create')
  sim_create = TRUE,
  #   - Execute SOILWAT2 simulations (formerly 'execute')
  sim_execute = TRUE,
  #   - Calculate aggregated response variables from  SOILWAT2 output and store results
  #     in temporary text files on disk (formerly, "aggregate')
  sim_aggregate = TRUE,

  # Output handling
  #   - Copy simulation results from temporary text files to a output SQL-database
  #     (formerly, 'concatenate')
  concat_dbOut = TRUE,
  #   - Calculate 'ensembles' across climate scenarios and stores the results
  #     in additional SQL-databases as specified by 'ensemble.families' and 'ensemble.levels'
  ensemble = FALSE,
  #   - Check completeness of output database
  check_dbOut = TRUE
)



##############################################################################
#------ 1) CREATE A NEW SIMULATION PROJECT (DO ONCE) -------------------------

dir_prj <- "SFSW2_default_project"

if (FALSE) {
  # If this is a test project:
  #   * if interactive: current working directory must be rSFSW2_tools/
  #   * if !interactive: current working directory must be folder of test projects,
  #       * e.g., rSFSW2_tools/Test_projects/Test4_AllOverallAggregations
  if (interactive()) {
    dir_prj <- normalizePath(file.path(".", "Test_projects", "SFSW2_default_project"))
    setwd(dir_prj)
  }

  dir_prj <- getwd()
}

writeLines(c("", "",
  "##############################################################################",
  paste("#------ rSFSW2-PROJECT:", shQuote(basename(dir_prj)), "run started at",
    t_job_start),
  "##############################################################################", ""))

fmeta <- file.path(dir_prj, "SFSW2_project_descriptions.rds")
fmetar <- file.path(dir_prj, "SFSW2_project_descriptions.R")

if (file.exists(fmeta)) {

  # Load pre-prepared project description if it was setup previously
  SFSW2_prj_meta <- readRDS(fmeta)

  # Ensure that all necessary paths do exists
  dir_safe_create(SFSW2_prj_meta[["project_paths"]])

} else {

  # 1a) Setup default project infrastructure
  setup_rSFSW2_project_infrastructure(dir_prj)

  # 1b) In text editor: specify project description/metadata ("SFSW2_project_description.R")
  warning("'SFSW2_project_code.R': Check/adjust project description/metadata in file ",
    shQuote(basename(fmetar)), " before further steps are executed.", call. = FALSE,
    immediate. = TRUE)

  # 1c) Load and prepare project description
  SFSW2_prj_meta <- new.env(parent = baseenv())
  sys.source(fmetar, envir = SFSW2_prj_meta, keep.source = FALSE)

  SFSW2_prj_meta <- init_rSFSW2_project(SFSW2_prj_meta, fmeta)

  saveRDS(SFSW2_prj_meta, file = fmeta)
}



##############################################################################
#------ 2) LOAD SETTINGS FOR THIS RUN ----------------------------------------
# Setting objects:
#   opt_behave, opt_parallel, opt_verbosity, opt_out_run, opt_chunks
source(file.path(dir_prj, "SFSW2_project_settings.R"), verbose = FALSE,
  keep.source = FALSE)



##############################################################################
#------ 3) POPULATE PROJECT WITH INPUT DATA (REPEAT UNTIL COMPLETE) ----------

if (actions[["prep_inputs"]]) {

  temp <- populate_rSFSW2_project_with_data(SFSW2_prj_meta, opt_behave, opt_parallel,
    opt_chunks, opt_out_run, opt_verbosity)

  SFSW2_prj_meta <- temp[["SFSW2_prj_meta"]]
  SFSW2_prj_inputs <- temp[["SFSW2_prj_inputs"]]

  warning("'SFSW2_project_code.R': Modify/reset input tracker status ",
    "'SFSW2_prj_meta[['input_status']]', if needed, manually or by calling function ",
    "'update_intracker' and re-run project.", call. = FALSE, immediate. = TRUE)
}



##############################################################################
#------ 4) ATTEMPT TO CHECK INPUT DATA ---------------------------------------

if (actions[["check_inputs"]]) {

  temp <- check_rSFSW2_project_input_data(SFSW2_prj_meta, SFSW2_prj_inputs, opt_verbosity)

  SFSW2_prj_meta <- temp[["SFSW2_prj_meta"]]
  SFSW2_prj_inputs <- temp[["SFSW2_prj_inputs"]]

  warning("'SFSW2_project_code.R': Modify/reset input tracker status ",
    "'SFSW2_prj_meta[['input_status']]', if needed, manually or by calling function ",
    "'update_intracker' and re-run project.", call. = FALSE, immediate. = TRUE)
}



##############################################################################
#------ 5) RUN SIMULATION EXPERIMENT (REPEAT UNTIL COMPLETE) -----------------

if (any(unlist(actions[c("sim_create", "sim_execute", "sim_aggregate", "concat_dbOut")]))) {

  SFSW2_prj_meta <- simulate_SOILWAT2_experiment(actions, SFSW2_prj_meta, SFSW2_prj_inputs,
    t_job_start, opt_behave, opt_parallel, opt_chunks, opt_out_run, opt_verbosity)
}



##############################################################################
#------ 6) ENSEMBLE GENERATION -----------------------------------------------

if (actions[["ensemble"]]) {

  rSFSW2:::generate_ensembles(SFSW2_prj_meta, t_job_start, opt_parallel, opt_chunks,
    verbose = opt_verbosity[["verbose"]])
}



##############################################################################
#------ 7) CHECK COMPLETENESS OF OUTPUT DATABASE AND SIMULATION --------------

if (actions[["check_dbOut"]]) {

  check_outputDB_completeness(SFSW2_prj_meta, opt_parallel, opt_behave,
    opt_out_run, verbose = opt_verbosity[["verbose"]])
}



##############################################################################
#------ 8) FINISH RUN CLEANLY

#--- Terminate infrastructure for parallel framework runs
exit_SFSW2_cluster(verbose = opt_verbosity[["verbose"]])

#--- Goodbye message
writeLines(c("",
  "##############################################################################",
  paste("#------ rSFSW2-PROJECT:", shQuote(basename(dir_prj)), "run ended at",
    Sys.time()),
  "##############################################################################", ""))
