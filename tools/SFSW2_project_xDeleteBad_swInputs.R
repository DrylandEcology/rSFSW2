#!/usr/bin/env Rscript



failed_rSOILWAT2_objects <- function(path, delete = FALSE, verbose = FALSE) {
  SW2_runs <- list.files(path)
  N_runs <- length(SW2_runs)

  has_failed <- list()

  if (verbose) {
    message("Processing ", N_runs, " rSOILWAT2 runs:")
    has_progress_bar <- requireNamespace("utils")
  } else {
    has_progress_bar <- FALSE
  }

  if (has_progress_bar) {
    pb <- utils::txtProgressBar(max = N_runs, style = 3)
  }


  for (k1 in seq_along(SW2_runs)) {
    success <- NA

    # Check rSOILWAT2 output files
    ftmp_out <- list.files(
      path = file.path(path, SW2_runs[k1]),
      pattern = "sw_output",
      full.names = TRUE
    )

    for (k2 in seq_along(ftmp_out)) {
      attach(ftmp_out[k2], name = "sim_data_container")

      sim_data <- if (exists("runDataSC")) {
        get("runDataSC", pos = "sim_data_container")
      } else {
        NULL
      }

      detach("sim_data_container")

      success <- (k2 == 1 || isTRUE(success)) && inherits(sim_data, "swOutput")

      if (!success) break
    }


    #--- Check rSOILWAT2 input file if no output
    if (is.na(success)) {

      fname_in <- list.files(
        path = file.path(path, SW2_runs[k1]),
        pattern = "sw_input",
        full.names = TRUE
      )

      if (length(fname_in) == 1) {
        attach(fname_in, name = "input_data_container")

        sim_input <- if (exists("swRunScenariosData")) {
          get("swRunScenariosData", pos = "input_data_container")[[1]]
        } else {
          NULL
        }

        detach("input_data_container")

        if (!is.null(sim_input) && inherits(sim_input, "swInputData")) {
          # Check whether `sim_input` may successfully run rSOILWAT2
          # TODO: make this work for all possible input objects
          rSOILWAT2::swYears_StartYear(sim_input) <- 0
          rSOILWAT2::swYears_EndYear(sim_input) <- 2010
          rSOILWAT2::swYears_StartYear(sim_input) <- 2009

          sim_out <- try(
            suppressMessages(suppressWarnings(
              rSOILWAT2::sw_exec(
                inputData = sim_input,
                weatherList = rSOILWAT2::weatherData
              )
            )),
            silent = TRUE
          )

          success <- inherits(sim_out, "swOutput")
        }
      }
    }


    if (!isTRUE(success)) {
      has_failed[[k1]] <- SW2_runs[k1]

      #--- Delete if requested
      if (delete) {
        unlink(
          x = list.files(file.path(path, SW2_runs[k1]), full.names = TRUE),
          recursive = TRUE
        )
      }
    }


    if (has_progress_bar) {
      utils::setTxtProgressBar(pb, k1)
    }
  }

  if (has_progress_bar) {
    close(pb)
  }

  unlist(has_failed)
}

has_failed <- failed_rSOILWAT2_objects(path = "3_Runs", verbose = TRUE)

saveRDS(
  has_failed,
  file = paste0(format(Sys.time(), "%Y%m%d"), "_failed_rSOILWAT2_objects.rds")
)

print(length(has_failed))
print(has_failed)



delete_rSOILWAT2_objects <- function(path = ".", SW2_runs) {
  for (k1 in seq_along(SW2_runs)) {
    unlink(
      x = list.files(file.path(path, SW2_runs[k1]), full.names = TRUE),
      recursive = TRUE
    )
  }
}

if (FALSE) {
  delete_rSOILWAT2_objects(path = "3_Runs", SW2_runs = has_failed)
}
