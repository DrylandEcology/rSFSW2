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
  stop("Specify project description/metadata in file 'SWSF_project_descriptions.R'")

  # 1c) Load and prepare project description
  SWSF_prj_meta <- new.env()
  sys.source(file.path(dir_prj, "SWSF_project_descriptions.R"), envir = SWSF_prj_meta,
    keep.source = FALSE)

  SWSF_prj_meta <- init_rSWSF_project(SWSF_prj_meta)

  saveRDS(SWSF_prj_meta, file = fmeta)
}



##############################################################################
#------ 2) POPULATE PROJECT WITH INPUT DATA (REPEAT UNTIL COMPLETE) ----------

# Load variable 'opt_prepare'
source(file.path(dir_prj, "SWSF_project_extdata.R"), keep.source = FALSE)

populate_rSWSF_project_with_data(SWSF_prj_meta, opt_prepare)



##############################################################################
#------ 3) ATTEMPT TO CHECK INPUT DATA ---------------------------------------

check_rSWSF_project_input_data()



##############################################################################
#------ 4) RUN SIMULATION EXPERIMENT (REPEAT UNTIL COMPLETE) -----------------

simulate_SOILWAT2_experiment(actions, opt_behave, opt_prepare, opt_sim, req_scens,
  req_out, opt_agg, project_paths, fnames_in, fnames_out, sim_space, opt_parallel,
  opt_chunks, opt_job_time, opt_verbosity)


##############################################################################
