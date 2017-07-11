#' Export R objects to MPI/socket workers
#'
#' @param varlist A vector of R object names to export
#' @param list_envs A list of environments in which to search for the R objects
#' @export
gather_objects_for_export <- function(varlist, list_envs) {
  #---Determine environments
  obj_env <- new.env(parent = emptyenv())
  vtemp <- NULL

  for (k in seq_along(list_envs)) {
    temp <- varlist[varlist %in% ls(pos = list_envs[[k]])]
    temp <- temp[!(temp %in% vtemp)]

    for (i in seq_along(temp))
      assign(temp[i], value = get(temp[i], list_envs[[k]]), obj_env)

    vtemp <- c(vtemp, temp)
  }

  cannot_export <- !(varlist %in% vtemp)
  if (any(cannot_export))
    print(paste("Objects in 'varlist' that cannot be located:",
          paste(varlist[cannot_export], collapse = ", ")))

  obj_env
}


do_import_objects <- function(obj_env) {
  temp <- list2env(as.list(obj_env), envir = globalenv())

  NULL
}

#' Export objects to workers
#'
#' @param parallel_backend A character vector, either 'mpi' or 'socket'
#' @param cl A parallel (socket) cluster object
#'
#' @return A logical value. \code{TRUE} if every object was exported successfully.
#' @export
export_objects_to_workers <- function(obj_env,
  parallel_backend = c("mpi", "socket"), cl = NULL) {

  t.bcast <- Sys.time()
  parallel_backend <- match.arg(parallel_backend)
  N <- length(ls(obj_env))
  print(paste("Exporting", N, "objects from master process to workers"))

  success <- FALSE
  done_N <- 0

  if (inherits(cl, "cluster") && identical(parallel_backend, "socket")) {
    # Remove suppressWarnings() when rSFSW2 becomes a R package
    temp <- suppressWarnings(try(parallel::clusterExport(cl, as.list(ls(obj_env)),
      envir = obj_env)))

    success <- !inherits(temp, "try-error")

    if (success) {
      temp <- parallel::clusterCall(cl, function() ls(globalenv()))
      done_N <- min(lengths(temp))
    } else {
      print(paste("'export_objects_to_workers': error:", temp))
    }

  } else if (identical(parallel_backend, "mpi")) {
    temp <- try(Rmpi::mpi.bcast.cmd(assign,
      x = "do_import_objects", value = do_import_objects))
    if (!inherits(temp, "try-error"))
      temp <- try(Rmpi::mpi.bcast.cmd(do_import_objects, obj_env = obj_env))

    success <- !inherits(temp, "try-error")
    if (success) {
      temp <- Rmpi::mpi.remote.exec(cmd = ls, envir = globalenv(), simplify = FALSE)
      done_N <- min(lengths(temp))
    } else {
      print(paste("'export_objects_to_workers': error:", temp))
    }

  } else {
    temp <- "requested 'parallel_backend' not implemented"
  }

  if (success && done_N >= N) {
    print(paste("Export of", done_N, "objects took",
              round(difftime(Sys.time(), t.bcast, units = "secs"), 2),
              "secs"))
  } else {
    success <- FALSE
    print(paste("Export not successful:", done_N, "instead of", N, "objects exported:",
      temp))
  }

  success
}





#' Rmpi work function for calling \code{do_OneSite}
#'
#' @param verbose A logical value.
#'
#' @references
#'   based on the example file \href{http://acmmac.acadiau.ca/tl_files/sites/acmmac/resources/examples/task_pull.R.txt}{'task_pull.R' by ACMMaC}
#' @section Note:
#'  If an error occurs, then the worker will likely not report back to master because
#'  it hangs in miscommunication and remains idle (check activity, e.g., with \code{top}).
#' @export
mpi_work <- function(verbose = FALSE) {
  # Note the use of the tag for sent messages:
  #     1 = ready_for_task, 2 = done_task, 3 = exiting
  # Note the use of the tag for received messages:
  #     1 = task, 2 = done_tasks

  if (verbose) {
    print(paste(Sys.time(), "MPI worker", Rmpi::mpi.comm.rank(), "starts working."))
  }

  junk <- 0L
  done <- 0L
  while (done != 1L) {
    # Signal being ready to receive a new task
    Rmpi::mpi.send.Robj(junk, 0, 1)

    # Receive a task
    dat <- Rmpi::mpi.recv.Robj(Rmpi::mpi.any.source(), Rmpi::mpi.any.tag())
    task_info <- Rmpi::mpi.get.sourcetag()
    tag <- task_info[2]

    if (tag == 1L) {
      if (dat$do_OneSite) {
        if (verbose)
          print(paste(Sys.time(), "MPI worker", Rmpi::mpi.comm.rank(), "works on:",
            dat$i_sim, dat$i_labels))

        result <- do.call("do_OneSite", args = dat[-1])

        # Send a result message back to the master
        Rmpi::mpi.send.Robj(list(i = dat$i_sim, r = result), 0, 2)
      }

    } else if (tag == 2L) {
      done <- 1L
      if (verbose)
        print(paste(Sys.time(), "MPI worker", Rmpi::mpi.comm.rank(),
          "shuts down 'mpi_work()'"))
    }
    # We'll just ignore any unknown messages
  }
  Rmpi::mpi.send.Robj(junk, 0, 3)
}


#' Properly end mpi workers before quitting R (e.g., at a crash)
#' @section Notes: code is based on http://acmmac.acadiau.ca/tl_files/sites/acmmac/resources/examples/task_pull.R.txt
mpi_last <- function() {
  if (requireNamespace("Rmpi")) { # && is.loaded("mpi_initialize") && is.loaded("mpi_finalize")
    if (Rmpi::mpi.comm.size(1) > 0)
      Rmpi::mpi.close.Rslaves()
    # .Call("mpi_finalize", PACKAGE = "Rmpi")
    Rmpi::mpi.exit()
  }
}


#' Clean up and terminate a parallel cluster used for a rSFSW2 simulation project
#' @export
exit_SFSW2_cluster <- function(verbose = FALSE) {
  if (SFSW2_glovars[["p_has"]]) {
    if (verbose) {
      t1 <- Sys.time()
      temp_call <- shQuote(match.call()[1])
      print(paste0("rSFSW2's ", temp_call, ": started at ", t1))

      on.exit({print(paste0("rSFSW2's ", temp_call, ": ended after ",
        round(difftime(Sys.time(), t1, units = "secs"), 2), " s")); cat("\n")}, add = TRUE)
    }

    if (verbose) {
      print(paste("Cleaning up", SFSW2_glovars[["p_workersN"]], "workers",
      "of the", shQuote(SFSW2_glovars[["p_type"]]), "cluster."))
    }

    if (identical(SFSW2_glovars[["p_type"]], "mpi")) {
      mpi_last()

    } else if (identical(SFSW2_glovars[["p_type"]], "socket")) {
      parallel::stopCluster(SFSW2_glovars[["p_cl"]])
    }

    init_SFSW2_cluster()
  }

  invisible(TRUE)
}


#' Clean memory and workspace of parallel cluster workers
clean_SFSW2_cluster <- function() {
  if (SFSW2_glovars[["p_has"]]) {
    if (identical(SFSW2_glovars[["p_type"]], "mpi")) {
      Rmpi::mpi.bcast.cmd(cmd = clean_worker)
    }

    if (identical(SFSW2_glovars[["p_type"]], "socket")) {
      parallel::clusterCall(SFSW2_glovars[["p_cl"]], fun = clean_worker)
    }
  }

  invisible(TRUE)
}

#' Remove almost all objects from a worker's global environment
#'
#' Remove all objects except those with a name starting with a dot '.'
#' @section Notes: Do not call 'ls(all = TRUE)' because there are important (hidden)
#'    \code{.X} objects that are important for proper worker functioning!
clean_worker <- function() {
  rm(list = ls(envir = globalenv(), all.names = FALSE), envir = globalenv())
  gc()

  invisible(TRUE)
}

#' Initialize a parallel cluster
#' @export
init_SFSW2_cluster <- function() {
  assign("p_workersN", 1L, envir = SFSW2_glovars) # Number of currently set-up workers
  assign("p_cl", NULL, envir = SFSW2_glovars) # Parallel cluster

  unlink(SFSW2_glovars[["lockfile"]], recursive = TRUE)
  assign("lockfile", NULL, envir = SFSW2_glovars)

  invisible(TRUE)
}


#' Set-up a parallel cluster to be used for a rSFSW2 simulation project
#' @export
setup_SFSW2_cluster <- function(opt_parallel, dir_out, verbose = FALSE) {
  if (!SFSW2_glovars[["p_has"]]) {
    init_SFSW2_cluster()
  }

  if (!SFSW2_glovars[["p_has"]] && opt_parallel[["parallel_runs"]]) {
    if (verbose) {
      t1 <- Sys.time()
      temp_call <- shQuote(match.call()[1])
      print(paste0("rSFSW2's ", temp_call, ": started at ", t1))

      on.exit({print(paste0("rSFSW2's ", temp_call, ": ended after ",
        round(difftime(Sys.time(), t1, units = "secs"), 2), " s and prepared ",
        SFSW2_glovars[["p_workersN"]], " worker(s)")); cat("\n")}, add = TRUE)
    }

    SFSW2_glovars[["p_type"]] <- switch(opt_parallel[["parallel_backend"]],
      mpi = "mpi", socket = "socket", cluster = "socket", NA_character_)

    SFSW2_glovars[["lockfile"]] <- tempfile(pattern = "rSFSW2lock",
      tmpdir = normalizePath(tempdir()))

    if (identical(SFSW2_glovars[["p_type"]], "mpi")) {
      if (!requireNamespace("Rmpi", quietly = TRUE)) {
        print(paste("'Rmpi' requires a MPI backend, e.g., OpenMPI is available from",
          shQuote("https://www.open-mpi.org/software/ompi/"),
          "with installation instructions at",
          shQuote("https://www.open-mpi.org/faq/?category=building#easy-build")))
        stop("If no MPI is available, installation of 'Rmpi' will fail and may print",
          "the error message: 'Cannot find mpi.h header file'")
      }

      if (!isTRUE(SFSW2_glovars[["p_cl"]])) {
        if (verbose)
          print(paste("Setting up", opt_parallel[["num_cores"]], "mpi cluster workers."))

        Rmpi::mpi.spawn.Rslaves(nslaves = opt_parallel[["num_cores"]])
        Rmpi::mpi.bcast.cmd(library("rSFSW2"))
        Rmpi::mpi.bcast.cmd(library("rSOILWAT2"))
        SFSW2_glovars[["p_cl"]] <- TRUE

        reg.finalizer(SFSW2_glovars, mpi_last, onexit = TRUE)

      } else {
        print("MPI master/workers are already set up.")
      }

    } else if (identical(SFSW2_glovars[["p_type"]], "socket")) {
      if (is.null(SFSW2_glovars[["p_cl"]])) {
        if (verbose)
          print(paste("Setting up", opt_parallel[["num_cores"]], "socket cluster workers."))

        SFSW2_glovars[["p_cl"]] <- parallel::makePSOCKcluster(opt_parallel[["num_cores"]],
          outfile = if (verbose) shQuote(file.path(dir_out, paste0(format(Sys.time(),
          "%Y%m%d-%H%M"), "_olog_cluster.txt"))) else "")

#TODO (drs): it is ok to load into globalenv() because this happens on workers and not on master;
#  -> R CMD CHECK reports this nevertheless as issue
      # pos = 1 assigns into globalenv() of the worker
        parallel::clusterApplyLB(SFSW2_glovars[["p_cl"]], seq_len(opt_parallel[["num_cores"]]),
          function(x, id) assign(id, x, pos = 1L), id = SFSW2_glovars[["p_wtag"]])

      } else {
        print("Socket cluster is already set up.")
      }
    }

    SFSW2_glovars[["p_workersN"]] <- if (identical(SFSW2_glovars[["p_type"]], "mpi")) {
        Rmpi::mpi.comm.size() - 1
      } else {
        opt_parallel[["num_cores"]] #parallel::detectCores(all.tests = TRUE)
      }

    SFSW2_glovars[["p_has"]] <- !is.null(SFSW2_glovars[["p_cl"]]) &&
      SFSW2_glovars[["p_workersN"]] > 1
  }

  invisible(TRUE)
}
