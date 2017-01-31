#' Export R objects to MPI slaves or SNOW workers
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
#' @param parallel_backend A character vector, either 'mpi' or 'cluster'
#' @param cl A parallel (socket) cluster object
#'
#' @return A logical value. \code{TRUE} if every object was exported successfully.
#' @export
export_objects_to_workers <- function(obj_env,
  parallel_backend = c("mpi", "cluster"), cl = NULL) {

  t.bcast <- Sys.time()
  parallel_backend <- match.arg(parallel_backend)
  N <- length(ls(obj_env))
  print(paste("Exporting", N, "objects from master process to workers"))

  success <- FALSE
  done_N <- 0

  if (inherits(cl, "cluster") && identical(parallel_backend, "cluster")) {
    # Remove suppressWarnings() when SWSF becomes a R package
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
#'  If an error occurs, then the slave will likely not report back to master because
#'  it hangs in miscommunication and remains idle (check activity, e.g., with \code{top}).
#' @export
mpi_work <- function(verbose = FALSE) {
  # Note the use of the tag for sent messages:
  #     1=ready_for_task, 2=done_task, 3=exiting
  # Note the use of the tag for received messages:
  #     1=task, 2=done_tasks

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
          print(paste(Sys.time(), "MPI slave", Rmpi::mpi.comm.rank(), "works on:",
            dat$i_sim, dat$i_labels))

        result <- do.call("do_OneSite", args = dat[-1])

        # Send a result message back to the master
        Rmpi::mpi.send.Robj(list(i = dat$i_sim, r = result), 0, 2)
      }

    } else if (tag == 2L) {
      done <- 1L
      if (verbose)
        print(paste(Sys.time(), "MPI slave", Rmpi::mpi.comm.rank(),
          "shuts down 'mpi_work()'"))
    }
    # We'll just ignore any unknown messages
  }
  Rmpi::mpi.send.Robj(junk, 0, 3)
}


#' @export
clean_SWSF_cluster <- function(parallel_backend = c("mpi", "cluster"), cl = NULL,
  verbose = FALSE) {

  parallel_backend <- match.arg(parallel_backend)
  if (verbose) {
    print(paste0("SWSF: started to clean parallel ", parallel_backend,
      "-workers at ", Sys.time()))
  }

  if (identical(parallel_backend, "mpi")) {
    #clean up mpi slaves

    #TODO: The following line is commented because Rmpi::mpi.comm.disconnect(comm) hangs
    # Rmpi::mpi.close.Rslaves(dellog = FALSE)

    Rmpi::mpi.exit()
  }

  if (identical(parallel_backend, "cluster") && !is.null(cl)) {
    #clean up parallel cluster
    parallel::stopCluster(cl)
  }

  if (verbose) {
    print(paste0("SWSF: ", parallel_backend,
      "-workers successfully closed down at ", Sys.time()))
  }

  invisible(TRUE)
}


#' @export
setup_SWSF_cluster <- function(opt_parallel, opt_verbosity, dir_out) {

  opt_parallel <- c(opt_parallel, list(workersN = 1, worker_tag = ".worker_id",
    do_parallel = FALSE, cl = NULL, lockfile = NULL))

  if (opt_parallel[["parallel_runs"]]) {
    if (opt_verbosity[["verbose"]])
      print(paste("SWSF prepares parallelization: started at", t1 <- Sys.time()))

    opt_parallel[["lockfile"]] <- tempfile(pattern = "swsflock",
      tmpdir = normalizePath(tempdir()))

    if (identical(opt_parallel[["parallel_backend"]], "mpi")) {
      if (!requireNamespace("Rmpi", quietly = TRUE)) {
        print(paste("'Rmpi' requires a MPI backend, e.g., OpenMPI is available from",
          shQuote("https://www.open-mpi.org/software/ompi/"), "with install instructions at",
          shQuote("https://www.open-mpi.org/faq/?category=building#easy-build")))
        print(paste("If no MPI is available, installation of 'Rmpi' will fail and may print",
          "the error message: 'Cannot find mpi.h header file'"))
      }

      Rmpi::mpi.spawn.Rslaves(nslaves = opt_parallel[["num_cores"]])

      Rmpi::mpi.bcast.cmd(require("rSWSF", quietly = TRUE))

      mpi_last <- function(x) { #Properly end mpi slaves before quitting R (e.g., at a crash)
        # based on http://acmmac.acadiau.ca/tl_files/sites/acmmac/resources/examples/task_pull.R.txt
        if (is.loaded("mpi_initialize")) {
          if (requireNamespace("Rmpi") && Rmpi::mpi.comm.size(1) > 0)
            Rmpi::mpi.close.Rslaves()
          .Call("mpi_finalize", PACKAGE = "Rmpi")
        }
      }
      reg.finalizer(swsf_glovars, mpi_last, onexit = TRUE)

    } else if (identical(opt_parallel[["parallel_backend"]], "cluster")) {

      opt_parallel[["cl"]] <- parallel::makePSOCKcluster(opt_parallel[["num_cores"]],
        outfile = if (opt_verbosity[["verbose"]]) "" else {
        file.path(dir_out, paste0(format(Sys.time(), "%Y%m%d-%H%M"), "_olog_cluster.txt"))})

      # Worker ID: this needs to be a .x object that does not get deleted with rm(list = ls())
#TODO (drs): it is ok to load into globalenv() because this happens on workers and not on master;
#  -> R CMD CHECK reports this nevertheless as issue
      parallel::clusterApplyLB(opt_parallel[["cl"]], seq_len(opt_parallel[["num_cores"]]),
        function(x) assign(opt_parallel[["worker_tag"]], x, envir = globalenv()))
      #parallel::clusterSetRNGStream(opt_parallel[["cl"]], seed) #random numbers setup

      parallel::clusterEvalQ(opt_parallel[["cl"]], require("rSWSF", quietly = TRUE))
    }

    opt_parallel[["workersN"]] <- if (identical(opt_parallel[["parallel_backend"]], "mpi")) {
        Rmpi::mpi.comm.size() - 1
      } else {
        opt_parallel[["num_cores"]] #parallel::detectCores(all.tests = TRUE)
      }

    opt_parallel[["do_parallel"]] <- TRUE

    if (opt_verbosity[["verbose"]])
      print(paste("SWSF prepares parallelization: initialization of",
        opt_parallel[["workersN"]], "workers ended after",
        round(difftime(Sys.time(), t1, units = "secs"), 2), "s"))
  }

  opt_parallel
}
