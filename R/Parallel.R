# nolint start
#' Export \var{R} objects to \var{MPI} or socket workers
# nolint end
#'
#' @param varlist A vector of R object names to export.
#' @param list_envs A list of environments in which to search for the R objects.
#'
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
  temp <- list2env(as.list(obj_env), envir = globalenv()) # nolint

  NULL
}

#' Export objects to workers
#'
#' @param obj_env An environment containing R objects to export.
#' @param parallel_backend A character vector, either \var{\dQuote{mpi}} or
#'   \var{\dQuote{socket}}
#' @param cl A parallel (socket) cluster object
#'
#' @return A logical value. \code{TRUE} if every object was exported
#'   successfully.
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
    temp <- suppressWarnings(try(parallel::clusterExport(cl,
      as.list(ls(obj_env)), envir = obj_env)))

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
      temp <- Rmpi::mpi.remote.exec(cmd = ls, envir = globalenv(),
        simplify = FALSE)
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
    print(paste("Export not successful:", done_N, "instead of", N,
      "objects exported:", temp))
  }

  success
}

# We have to rename rSOILWAT2 functions locally (and export to workers):
# 'do.call' (as called by 'mpi.remote.exec'/'mpi.bcast.cmd' of Rmpi v0.6.6)
# does not handle 'what' arguments of a character string format "pkg::fun"
# because "pkg::fun" is not the name of a function
dbW_setConnection_local <- function(...) rSOILWAT2::dbW_setConnection(...)
dbW_disconnectConnection_local <- function(...)
  rSOILWAT2::dbW_disconnectConnection(...)


#' Setting values of package-level global variables on workers
#' @param x A character string. The name of a global variable.
#' @param value A R object. The value to be assigned to the global variable
#'   identified by \code{x}.
#' @seealso \code{\link{assign}}
set_glovar <- function(x, value) {
  # The environment 'SFSW2_glovars' is the one of the package copy on the
  # workers!
  assign(x = x, value = value, envir = SFSW2_glovars)
}

export_parallel_glovars <- function(verbose = FALSE) {
  if (SFSW2_glovars[["p_has"]]) {
    if (verbose) {
      t1 <- Sys.time()
      temp_call <- shQuote(match.call()[1])
      print(paste0("rSFSW2's ", temp_call, ": started at ", t1))

      on.exit({
        print(paste0("rSFSW2's ", temp_call, ": ended after ",
        round(difftime(Sys.time(), t1, units = "secs"), 2), " s"))
        cat("\n")}, add = TRUE)
    }

    p_varnames <- grep("p_", ls(envir = SFSW2_glovars), value = TRUE)

    if (identical(SFSW2_glovars[["p_type"]], "socket")) {
      for (x in p_varnames) {
        parallel::clusterCall(SFSW2_glovars[["p_cl"]], fun = set_glovar, x = x,
          value = SFSW2_glovars[[x]])
      }

    } else if (identical(SFSW2_glovars[["p_type"]], "mpi")) {
      # We have to rename functions locally (and export to workers):
      # 'do.call' (as called by 'mpi.remote.exec'/'mpi.bcast.cmd' of Rmpi
      # v0.6.6)  does not handle 'what' arguments of a character string format
      # "pkg::fun" because "pkg::fun" is not the name of a function
      .set_glovar <- function(x, value) set_glovar(x, value)
      Rmpi::mpi.bcast.Robj2slave(.set_glovar)
      for (x in p_varnames) {
        Rmpi::mpi.bcast.cmd(cmd = .set_glovar, x = x,
          value = SFSW2_glovars[[x]])
      }
    }
  }

  invisible(TRUE)
}




#' \pkg{Rmpi} work function for calling \code{do_OneSite}
#'
#' @param verbose A logical value.
#'
#' @references based on the example file
# nolint start
#'   \url{http://acmmac.acadiau.ca/tl_files/sites/acmmac/resources/examples/task_pull.R.txt}
# nolint end
#' @section Notes: If an error occurs, then the worker will likely not report
#'   back to master because it hangs in miscommunication and remains idle
#'   (check activity, e.g., with \code{top}).
#' @section Details:
#' Message tags sent by this function from workers to master: \itemize{
#'  \item 1 = worker is ready for master to send a task
#'  \item 2 = worker is done with a task
#'  \item 3 = worker is exiting
#'  \item 4 = worker failed with task
#' }
#' Message tags from master which this function can receive and understand:
#' \itemize{
#'  \item 1 = this communication is a new task
#'  \item 2 = tells worker to shut down because all tasks are completed
#' }
#'
#' @export
mpi_work <- function(verbose = FALSE) {

  # Define tags
  junk <- 0L
  worker_is_done <- FALSE
  master <- 0L
  worker_id <- Rmpi::mpi.comm.rank()

  if (verbose) {
    print(paste(Sys.time(), "MPI-worker", worker_id, "starts working."))
  }

  #--- Loop until all work is completed
  while (!worker_is_done) {
    # Signal master that worker is ready to receive a new task
    Rmpi::mpi.send.Robj(junk, dest = master, tag = 1L)

    # Worker is receiving a message from master
    dat <- Rmpi::mpi.recv.Robj(Rmpi::mpi.any.source(), Rmpi::mpi.any.tag())
    task_info <- Rmpi::mpi.get.sourcetag()
    tag_from_master <- task_info[2]

    if (tag_from_master == 1L) {
      # Worker received a new task
      if (dat$do_OneSite) {
        if (verbose) {
          print(paste(Sys.time(), "MPI-worker", worker_id, "works on task =",
            dat$i_sim, shQuote(dat$i_SWRunInformation$Label)))
        }

        t.do_OneSite <- Sys.time()

        result <- try(do.call("do_OneSite", args = dat[-1]))

        delta.do_OneSite <- round(difftime(Sys.time(), t.do_OneSite,
          units = "secs"), 2)
        status <- !inherits(result, "try-error")
        dat <- list(i = dat$i_sim, r = result,
          status = if (status) as.logical(result) else FALSE,
          time_s = delta.do_OneSite)

        if (status) {
          # Send result back to the master and message that task has been
          # completed
          if (verbose) {
            print(paste(Sys.time(), "MPI-worker", worker_id,
              "successfully completed task =", dat$i_sim))
          }

          Rmpi::mpi.send.Robj(dat, dest = master, tag = 2L)

        } else {
          # Tell master that task failed
          print(paste(Sys.time(), "MPI-worker", worker_id, "failed with task =",
            dat$i_sim, "with error", shQuote(paste(result, collapse = " / "))))

          Rmpi::mpi.send.Robj(dat, dest = master, tag = 4L)
        }
      }

    } else if (tag_from_master == 2L) {
      # Worker is told to shut down
      worker_is_done <- TRUE

      if (verbose) {
        print(paste(Sys.time(), "MPI-worker", worker_id,
          "shuts down 'mpi_work'"))
      }

    } else {
      # We'll just ignore any unknown message from master
      print(paste(Sys.time(), "MPI-worker", worker_id, "received tag =",
        tag_from_master, "from master but doesn't know what this means."))
    }
  }

  # Worker is signaling to master that it is exiting
  Rmpi::mpi.send.Robj(junk, dest = master, tag = 3L)
}


#' Properly end \var{\dQuote{mpi}} workers before quitting R (e.g., at a crash)
#' @section Notes: Code is based on
# nolint start
#'   \url{http://acmmac.acadiau.ca/tl_files/sites/acmmac/resources/examples/task_pull.R.txt}.
# nolint end
#' @section Details: \code{gv} will usually be the package-level global
#'   variable environment \code{SFSW2_glovars}. This is because this function
#'   is registered as finalizer to the object \code{SFSW2_glovars}.
#' @section Notes: Ideally, we use \code{Rmpi::mpi.exit()} on exit, but we
#'   cannot as of \pkg{Rmpi} v0.6.6. Because \pkg{rSFSW2} does not attach
#'   \pkg{Rmpi}, the function \code{\link[Rmpi]{mpi.exit}} throws
#'   an error when it executes \code{detach(package:Rmpi)} which results in
#'   \code{"Error in detach(package:Rmpi) : invalid 'name' argument"}.
#' @section Notes: \code{Rmpi::mpi.comm.size(1)} crashes with a segfault if
#'   there are no workers running because of the call
#'   \code{.Call("mpi_comm_is_null", as.integer(comm), PACKAGE = "Rmpi")}.
#' @section Notes: Ideally, we would use \code{Rmpi::mpi.close.Rslaves()}.
#'   However, this function hangs our R session because of its call to
#'   \code{Rmpi::mpi.comm.disconnect()}; even wrapping this
#'   in \code{\link[base]{try}} doesn't prevent it from hanging.
#'
#' @param gv A list with at least one named element \code{p_has}. \code{p_has}
#'   is a logical value and indicates whether call is from a parallel run.
mpi_last <- function(gv) {
  if (requireNamespace("Rmpi")) {
    if (gv[["p_has"]] && Rmpi::mpi.comm.size(1) > 0) {
      Rmpi::mpi.bcast.cmd(cmd = break)
      Rmpi::mpi.bcast.cmd(cmd = .Call("mpi_finalize", PACKAGE = "Rmpi"))

    } else {
      # Maybe master still needs to finalize?
      Rmpi::mpi.finalize()
    }
  }
}


#' Clean up and terminate a parallel cluster used for a \pkg{rSFSW2}
#' simulation project
#'
#' @param verbose A logical value.
#' @export
exit_SFSW2_cluster <- function(verbose = FALSE) {
  if (SFSW2_glovars[["p_has"]]) {
    if (verbose) {
      t1 <- Sys.time()
      temp_call <- shQuote(match.call()[1])
      print(paste0("rSFSW2's ", temp_call, ": started at ", t1))

      on.exit({
        print(paste0("rSFSW2's ", temp_call, ": ended after ",
        round(difftime(Sys.time(), t1, units = "secs"), 2), " s"))
        cat("\n")}, add = TRUE)
    }

    if (verbose) {
      print(paste("Cleaning up", SFSW2_glovars[["p_workersN"]], "workers",
      "of the", shQuote(SFSW2_glovars[["p_type"]]), "cluster."))
    }

    if (identical(SFSW2_glovars[["p_type"]], "socket")) {
      parallel::stopCluster(SFSW2_glovars[["p_cl"]])

    } else if (identical(SFSW2_glovars[["p_type"]], "mpi")) {
      mpi_last(SFSW2_glovars)

      # Locate remaining R workers
      #   - remove master PID
      temp <- Sys.getpid() == SFSW2_glovars[["p_pids"]]
      pids <- SFSW2_glovars[["p_pids"]][!temp]
      #   - remove PIDs that are properly closed down
      isalive <- if (.Platform$OS.type == "unix") {
          sapply(pids, function(x)
            NROW(system2("ps", args = paste("-p", x), stdout = TRUE)) > 0)

        } else if (.Platform$OS.type == "windows") {
          sapply(pids, function(x) {
            temp <- system2("tasklist", args = paste0('/FI "PID eq ', x, '"'),
              stdout = TRUE)
            NROW(temp) > 1
          })
        } else NULL
      pids <- pids[isalive]

      if (length(pids) > 0) {
        print(paste("Something went wrong when taking down the cluster:",
          "kill remaining R workers with PIDs =", paste(pids, collapse = ", ")))
        # This is likely because Rmpi::mpi.close.Rslaves()
        # [due to Rmpi::mpi.comm.disconnect()] doesn't work as of Rmpi v0.6.6

        tools::pskill(pids, signal = tools::SIGKILL)
      }
    }

    # Reset the package parallel settings
    init_SFSW2_cluster()
  }

  invisible(TRUE)
}


#' Clean memory and workspace of parallel cluster workers
clean_SFSW2_cluster <- function() {
  if (SFSW2_glovars[["p_has"]]) {
    if (identical(SFSW2_glovars[["p_type"]], "mpi")) {
      Rmpi::mpi.bcast.cmd(cmd = clean_worker())
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
#' @section Notes: Do not call 'ls(all = TRUE)' because there are important
#'   (hidden) \code{.X} objects that are important for proper worker
#'   functioning!
clean_worker <- function() {
  rm(list = ls(envir = globalenv(), all.names = FALSE), envir = globalenv())
  gc()

  invisible(TRUE)
}

#' Initialize a parallel cluster
#'
#' Call for the side effect of setting values in the global variable
#' environment of the package: \itemize{
#'   \item \code{p_workersN}: Number of currently set-up workers
#'   \item \code{p_cl}: Parallel cluster
#'   \item \code{p_has}: Do we have a parallel cluster set up?
#'   \item \code{p_pids}: Process IDs of workers
#'   \item \code{lockfile}: Set \code{lockfile} to \code{NULL}
#' }
#'
#' @export
init_SFSW2_cluster <- function() {
  assign("p_workersN", 1L, envir = SFSW2_glovars)
  assign("p_cl", NULL, envir = SFSW2_glovars)
  assign("p_has", FALSE, envir = SFSW2_glovars)
  assign("p_pids", NULL, envir = SFSW2_glovars)

  unlink(SFSW2_glovars[["lockfile"]], recursive = TRUE)
  assign("lockfile", NULL, envir = SFSW2_glovars)

  invisible(TRUE)
}


#' Set-up a parallel cluster to be used for a \pkg{rSFSW2} simulation project
#' @export
setup_SFSW2_cluster <- function(opt_parallel, dir_out, verbose = FALSE,
  print.debug = FALSE) {

  if (!SFSW2_glovars[["p_has"]]) {
    init_SFSW2_cluster()
  }

  if (!SFSW2_glovars[["p_has"]] && opt_parallel[["parallel_runs"]]) {
    if (verbose) {
      t1 <- Sys.time()
      temp_call <- shQuote(match.call()[1])
      print(paste0("rSFSW2's ", temp_call, ": started at ", t1))

      on.exit({
        print(paste0("rSFSW2's ", temp_call, ": ended after ",
        round(difftime(Sys.time(), t1, units = "secs"), 2), " s and prepared ",
        SFSW2_glovars[["p_workersN"]], " worker(s)"))
        cat("\n")}, add = TRUE)
    }

    SFSW2_glovars[["p_type"]] <- switch(opt_parallel[["parallel_backend"]],
      mpi = "mpi", socket = "socket", cluster = "socket", NA_character_)

#    SFSW2_glovars[["lockfile"]] <- tempfile(pattern = "rSFSW2lock",
#      tmpdir = normalizePath(tempdir()))
    SFSW2_glovars[["lockfile"]] <- NULL

    if (identical(SFSW2_glovars[["p_type"]], "mpi")) {
      if (!requireNamespace("Rmpi", quietly = TRUE)) {
        print(paste("'Rmpi' requires a MPI backend, e.g., OpenMPI is available",
          "from", shQuote("https://www.open-mpi.org/software/ompi/"),
          "with installation instructions at",
          shQuote("https://www.open-mpi.org/faq/?category=building#easy-build"))) # nolint
        stop("If no MPI is available, installation of 'Rmpi' will fail and may",
          "print the error message: 'Cannot find mpi.h header file'")
      }

      if (!isTRUE(SFSW2_glovars[["p_cl"]])) {
        if (verbose) {
          print(paste("Setting up", opt_parallel[["num_cores"]],
            "mpi cluster workers."))
        }

        Rmpi::mpi.spawn.Rslaves(nslaves = opt_parallel[["num_cores"]])
        Rmpi::mpi.bcast.cmd(library("rSFSW2"))
        Rmpi::mpi.bcast.cmd(library("rSOILWAT2"))

        SFSW2_glovars[["p_cl"]] <- TRUE
        SFSW2_glovars[["p_pids"]] <- as.integer(
          Rmpi::mpi.remote.exec(cmd = Sys.getpid(), simplify = FALSE))

        reg.finalizer(SFSW2_glovars, mpi_last, onexit = TRUE)

        # We have to rename rSOILWAT2 functions locally (and export to workers):
        # 'do.call' (as called by 'mpi.remote.exec'/'mpi.bcast.cmd' of Rmpi
        # v0.6.6) does not handle 'what' arguments of a character string
        # format "pkg::fun" because "pkg::fun" is not the name of a function
        Rmpi::mpi.bcast.Robj2slave(dbW_setConnection_local)
        Rmpi::mpi.bcast.Robj2slave(dbW_disconnectConnection_local)

      } else {
        print("MPI master/workers are already set up.")
      }

    } else if (identical(SFSW2_glovars[["p_type"]], "socket")) {
      if (is.null(SFSW2_glovars[["p_cl"]])) {
        if (verbose) {
          print(paste("Setting up", opt_parallel[["num_cores"]],
            "socket cluster workers."))
        }

        SFSW2_glovars[["p_cl"]] <- parallel::makePSOCKcluster(
          opt_parallel[["num_cores"]],
          outfile = if (verbose) {
              temp <- file.path(dir_out,
                paste0(format(Sys.time(), "%Y%m%d-%H%M"), "_olog_cluster.txt"))
              if (utils::packageVersion("parallel") >= "3.5.0") {
                temp
              } else {
                # previous versions of `parallel:::newPSOCKnode` didn't call
                # `shQuote` on `outfile` before processing in a shell
                shQuote(temp)
              }
            } else "")

        SFSW2_glovars[["p_pids"]] <- as.integer(
          parallel::clusterCall(SFSW2_glovars[["p_cl"]], fun = Sys.getpid))

        # Note (drs): it is ok to load into globalenv() because this happens
        #  on workers and not on master;
        #  -> R CMD CHECK reports this nevertheless as issue
        # pos = 1 assigns into globalenv() of the worker
        parallel::clusterApplyLB(SFSW2_glovars[["p_cl"]],
          seq_len(opt_parallel[["num_cores"]]), function(x, id)
            assign(id, x, pos = 1L), id = SFSW2_glovars[["p_wtag"]])

      } else {
        print("Socket cluster is already set up.")
      }
    }

    temp <- identical(SFSW2_glovars[["p_type"]], "mpi")
    SFSW2_glovars[["p_workersN"]] <- if (temp) {
        Rmpi::mpi.comm.size() - 1
      } else {
        opt_parallel[["num_cores"]]
      }

    SFSW2_glovars[["p_has"]] <- !is.null(SFSW2_glovars[["p_cl"]]) &&
      SFSW2_glovars[["p_workersN"]] > 1

    if (print.debug) {
      temp <- sapply(grep("p_", ls(envir = SFSW2_glovars), value = TRUE),
        function(x) paste(shQuote(x), "=", paste(SFSW2_glovars[[x]],
          collapse = " / ")))
      temp <- paste(temp, collapse = "; ")

      print(paste("Workers set up with:", temp))
    }
  }

  export_parallel_glovars(verbose = print.debug)

  invisible(TRUE)
}
