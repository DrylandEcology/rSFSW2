#!/usr/bin/env Rscript

#----------------------------------------------------------------------------------------#
# rSWSF: FRAMEWORK FOR SOILWAT2 SIMULATIONS: CREATING SIMULATION RUNS, EXECUTING
#        SIMULATIONS, AND AGGREGATING OUTPUTS

#----- LICENSE
#    Copyright (C) 2017 by `r packageDescription("Rsoilwat31")[["Author"]]`
#    Contact information `r packageDescription("Rsoilwat31")[["Maintainer"]]`

#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, version 3 of the License.

#------ DISCLAIMER:
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.

#------ NOTES:
#  - You get an overview by: `r package?rSWSF`
#  - An index of functionality is displayed by: `r help(package = "rSWSF")`
#----------------------------------------------------------------------------------------#


##############################################################################
library("rSWSF")

#------ Turn on/off actions to be carried out by simulation framework
actions <- list(
  # Input preparation
  #   - Create a new(!) weather database and populate with current weather data
  #     (formerly 'createAndPopulateWeatherDatabase')
  #   - "external": pulls data from 'external' data sources from 'dir_external' as
  #     specified by 'req_data'
  prep_inputs = FALSE,

  # Input checking
  check_inputs = FALSE,

  # Simulation runs
  # "sim_create", "sim_execute", and "sim_aggregate" can be used individually if
  # "saveRsoilwatInput" and/or "saveRsoilwatOutput" are true
  #   - Prepare/collect inputs for a Rsoilwat run (formerly, 'create')
  sim_create = FALSE,
  #   - Execute SOILWAT2 simulations (formerly 'execute')
  sim_execute = FALSE,
  #   - Calculate aggregated response variables from  SOILWAT2 output and store results
  #     in temporary text files on disk (formerly, "aggregate')
  sim_aggregate = FALSE,

  # Output handling
  #   - Copy simulation results from temporary text files to a output SQL-database
  #     (formerly, 'concatenate')
  concat_dbOut = FALSE,
  #   - Calculate 'ensembles' across climate scenarios and stores the results
  #     in additional SQL-databases as specified by 'ensemble.families' and 'ensemble.levels'
  ensemble = FALSE,
  #   - Check completeness of output database
  check_dbOut = FALSE
)



##############################################################################
#------ 1) CREATE A NEW SIMULATION PROJECT (DO ONCE) -------------------------

dir_prj <- "SWSF_default_project"

if (FALSE) {
  # If this is a test project:
  #   * if interactive: current working directory must be rSWSFtools/
  #   * if !interactive: current working directory must be folder of test projects,
  #       * e.g., rSWSFtools/Test_projects/Test4_AllOverallAggregations
  if (interactive()) {
    dir_prj <- normalizePath(file.path(".", "Test_projects", "Test4_AllOverallAggregations_snow"))
    setwd(dir_prj)
  }

  dir_prj <- getwd()
}

fmeta <- file.path(dir_prj, "SWSF_project_descriptions.rds")
fmetar <- file.path(dir_prj, "SWSF_project_descriptions.R")

if (file.exists(fmeta)) {

  # Load pre-prepared project description if it was setup previously
  SWSF_prj_meta <- readRDS(fmeta)

} else {

  # 1a) Setup default project infrastructure
  setup_rSWSF_project_infrastructure(dir_prj)

  # 1b) In text editor: specify project description/metadata ("SWSF_project_description.R")
  stop("Specify project description/metadata via file ", shQuote(basename(fmetar)))

  # 1c) Load and prepare project description
  SWSF_prj_meta <- new.env(parent = baseenv())
  sys.source(fmetar, envir = SWSF_prj_meta, keep.source = FALSE)

  SWSF_prj_meta <- init_rSWSF_project(SWSF_prj_meta, fmeta)

  saveRDS(SWSF_prj_meta, file = fmeta)
}



##############################################################################
#------ 2) LOAD SETTINGS FOR THIS RUN ----------------------------------------
# Setting objects:
#   opt_behave, opt_parallel, opt_verbosity, opt_out_run, opt_chunks
source(file.path(dir_prj, "SWSF_project_settings.R"), verbose = FALSE,
  keep.source = FALSE)



##############################################################################
#------ 3) POPULATE PROJECT WITH INPUT DATA (REPEAT UNTIL COMPLETE) ----------

if (actions[["prep_inputs"]]) {

  temp <- populate_rSWSF_project_with_data(SWSF_prj_meta, opt_behave,
    opt_parallel, opt_chunks, opt_out_run, opt_verbosity)

  SWSF_prj_meta <- temp[["SWSF_prj_meta"]]
  SWSF_prj_inputs <- temp[["SWSF_prj_inputs"]]
}



##############################################################################
#------ 4) ATTEMPT TO CHECK INPUT DATA ---------------------------------------

if (actions[["check_inputs"]]) {

  temp <- check_rSWSF_project_input_data(SWSF_prj_meta, SWSF_prj_inputs, opt_verbosity)

  SWSF_prj_meta <- temp[["SWSF_prj_meta"]]
  SWSF_prj_inputs <- temp[["SWSF_prj_inputs"]]
}



##############################################################################
#------ 5) RUN SIMULATION EXPERIMENT (REPEAT UNTIL COMPLETE) -----------------

if (any(unlist(actions[c("sim_create", "sim_execute", "sim_aggregate", "concat_dbOut",
  "ensemble", "check_dbOut")])) && all(SWSF_prj_meta[["input_status"]])) {

  simulate_SOILWAT2_experiment(actions, opt_behave, opt_sim, req_scens,
    req_out, opt_agg, project_paths, fnames_in, fnames_out, sim_space, opt_parallel,
    opt_chunks, opt_job_time, opt_verbosity)
}

##############################################################################
