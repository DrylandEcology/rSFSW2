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
  #   - "check_inputs": creates maps of input data as specified by 'map_vars'
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
fmeta <- file.path(dir_prj, "SWSF_project_descriptions.rds")

if (file.exists(fmeta)) {

  # Load pre-prepared project description if previously setup
  SWSF_prj_meta <- readRDS(fmeta)

} else {

  # 1a) Setup default project infrastructure
  setup_rSWSF_project_infrastructure(dir_prj)

  # 1b) In text editor: specify project description/metadata ("SWSF_project_description.R")
  stop("Specify project description/metadata via file 'SWSF_project_descriptions.R'")

  # 1c) Load and prepare project description
  SWSF_prj_meta <- new.env()
  sys.source(file.path(dir_prj, "SWSF_project_descriptions.R"), envir = SWSF_prj_meta,
    keep.source = FALSE)

  SWSF_prj_meta <- init_rSWSF_project(SWSF_prj_meta)

  saveRDS(SWSF_prj_meta, file = fmeta)
}



##############################################################################
#------ 2) POPULATE PROJECT WITH INPUT DATA (REPEAT UNTIL COMPLETE) ----------

if (actions[["prep_inputs"]]) {
  # Load variable 'opt_prepare'
  source(file.path(dir_prj, "SWSF_project_extdata.R"), keep.source = FALSE)

  populate_rSWSF_project_with_data(SWSF_prj_meta, opt_prepare)
}


##############################################################################
#------ 3) ATTEMPT TO CHECK INPUT DATA ---------------------------------------

if (actions[["check_inputs"]]) {
  check_rSWSF_project_input_data()
}


##############################################################################
#------ 4) RUN SIMULATION EXPERIMENT (REPEAT UNTIL COMPLETE) -----------------

if (any(unlist(actions[c("sim_create", "sim_execute", "sim_aggregate", "concat_dbOut",
  "ensemble", "check_dbOut")]))) {
  simulate_SOILWAT2_experiment(actions, opt_behave, opt_prepare, opt_sim, req_scens,
    req_out, opt_agg, project_paths, fnames_in, fnames_out, sim_space, opt_parallel,
    opt_chunks, opt_job_time, opt_verbosity)
}

##############################################################################
