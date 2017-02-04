#----------------------------------------------------------------------------------------#
# rSWSF: FRAMEWORK FOR SOILWAT2 SIMULATIONS: CREATING SIMULATION RUNS, EXECUTING
#        SIMULATIONS, AND AGGREGATING OUTPUTS
#
# See demo/SWSF_project_code.R for details
#----------------------------------------------------------------------------------------#


##############################################################################
#----------------------- SETTINGS FOR A RUN OF A SIMULATION PROJECT ----------

# NOTE: The values may be changed/adjusted from run to run a SWSF simulation project. The
#  values of the description of a project (file demo/SWSF_project_description.R) cannot
#  be changed once a SWSF simulation project is set up.



opt_behave <- list(
  # Resumes/continues with unfinished part of simulation after abort if TRUE, i.e.,
  #  - It doesn't delete an existing weather database, if a new one is requested
  #  - It doesn't re-extract external information (soils, elevation, climate normals,
  #     NCEPCFSR) if already extracted
  #  - It doesn't lookup values from tables if already available in input datafiles, i.e.,
  #     'LookupEvapCoeffFromTable', 'LookupTranspRegionsFromTable', and
  #     'LookupSnowDensityFromTable'
  #  - It doesn't repeat calls to 'do_OneSite' that are listed in 'runIDs_done'
  resume = TRUE,
  # Use preprocessed input data if available
  use_preprocin = TRUE,
  # If action == "check_dbOut" detects missing Pids, then workDB is updated (so that a new run
  #   of the script can be started to add missing runs)
  check_updates_dbWork = TRUE,
  # Check linked BLAS library before simulation runs
  check_blas = FALSE
)



#------ Options for parallel framework
opt_parallel <- list(
  # Should job be run in parallel
  parallel_runs = !interactive(),
  # Number of cores/workers/slaves if job is run in parallel
  num_cores = 2,
  # Parallel_backend: "cluster" (via package 'parallel') or "mpi" (via 'Rmpi')
  parallel_backend = "cluster"
)


#------ Computation time requests
# Time limits are only enforced if parallel_backend == "mpi"
opt_job_time <- list(
  wall_time_s = 12 * 3600, # requested wall time
  one_sim_s = 60, # time needed to complete one call to do_OneSite()
  one_concat_s = 60 # time needed to process one temporary SQL file
)


#------ Options for printing progress and debugging information
opt_verbosity <- list(
  # Prints status of progress to standard output
  verbose = TRUE,
  # Prints details of progress to standard output
  print.debug = interactive(),
  # Calculates and prints estimated time of job completion at end of each call of
  #   'do_OneSite' (a somewhat expensive operation)
  print.eta = interactive(),

  # Sets global option 'warn' for the duration of a simulation project
  #   Possible values: -1, 0, 1, 2; for details: ?options -> Value: warn
  debug.warn.level = 2 * interactive(),
  # Should R objects be dumped to disk on error (including for each call to 'do_OneSite')
  debug.dump.objects = interactive()
)


#------ Output options
opt_out_run <- list(
  # Write Rsoilwat input and output objects to disk for each SOILWAT2 simulation
  saveRsoilwatInput = FALSE,
  saveRsoilwatOutput = FALSE,

  # Write data to big input files for experimental design x treatment design
  makeInputForExperimentalDesign = FALSE,

  # Delete any previous dbOutput; be careful to not wipe your data!
  wipe_dbOutput = FALSE,
  # Delete temporary text files produced by workers (temporary storage of simulation
  #   output which is copied to dbOutput byu action 'concatenate')
  deleteTmpSQLFiles = TRUE
)


#----- Define chunk size for operations on massive data
opt_chunks <- list(
  # chunk_size == 1e4 && n_extract 6e4 will use about 30 GB of memory
  ExtractSkyDataFromNOAAClimateAtlas_USA = 10000,
  # Extracting data from NCEP/CFSR is also OS-limited by the number of concurrently open
  #   files (on 'unix' platforms, check with 'ulimit -a')
  ExtractSkyDataFromNCEPCFSR_Global = 100,
  DailyWeatherFromNCEPCFSR_Global = 100,
  # This value is the chunk size for reads of 'runID' from the database, i.e.,
  #   chunk size = ensembleCollectSize * scenario_No.
  #   Find a balance between available memory, cores, read/write times, etc. For instance,
  #   on Yellowstone, 500 seems to work well.
  ensembleCollectSize = 500
)



##############################################################################
