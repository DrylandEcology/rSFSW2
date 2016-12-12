#' Export R objects to MPI slaves or SNOW workers
#'
#' @param varlist A vector of R object names to export
#' @param list_envs A list of environments in which to search for the R objects
#' @param parallel_backend A character vector, either 'mpi' or 'snow'
#' @param cl A snow cluster object
#'
#' @return A logical value. \code{TRUE} if every object was exported successfully.
gather_objects_for_export <- compiler::cmpfun(function(varlist, list_envs) {
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
})


do_import_objects <- compiler::cmpfun(function(obj_env) {
  temp <- list2env(as.list(obj_env), envir = .GlobalEnv)

  NULL
})


export_objects_to_workers <- compiler::cmpfun(function(obj_env,
  parallel_backend = c("mpi", "snow"), cl = NULL) {

  t.bcast <- Sys.time()
  parallel_backend <- match.arg(parallel_backend)
  N <- length(ls(obj_env))
  print(paste("Exporting", N, "objects from master process to workers"))

  success <- FALSE
  done_N <- 0

  if (inherits(cl, "cluster") && identical(parallel_backend, "snow")) {
    temp <- try(snow::clusterExport(cl, as.list(ls(obj_env)), envir = obj_env))

    success <- !inherits(temp, "try-error")

    if (success) {
      done_N <- min(unlist(snow::clusterCall(cl,
        function() length(ls(.GlobalEnv)))), na.rm = TRUE)
    }

  } else if (identical(parallel_backend, "mpi")) {
    temp <- try(Rmpi::mpi.bcast.cmd(assign,
      x = "do_import_objects", value = do_import_objects))
    if (!inherits(temp, "try-error"))
      temp <- try(Rmpi::mpi.bcast.cmd(do_import_objects, obj_env = obj_env))

    success <- !inherits(temp, "try-error")
    if (success) {
      done_N <- min(lengths(Rmpi::mpi.remote.exec(cmd = ls,
        envir = .GlobalEnv, simplify = FALSE)))
    }
  }

  if (success && done_N >= N) {
    print(paste("Export of", done_N, "objects took",
              round(difftime(Sys.time(), t.bcast, units = "secs"), 2),
              "secs"))
  } else {
    success <- FALSE
    print(paste("Export not successful:", done_N, "instead of", N, "objects exported"))
  }

  success
})





#' Rmpi work function for calling \code{do_OneSite}
#'
#' @param verbose A logical value.
#'
#' @references
#'   based on the example file \href{http://acmmac.acadiau.ca/tl_files/sites/acmmac/resources/examples/task_pull.R.txt}{'task_pull.R' by ACMMaC}
#' @section Note:
#'  If an error occurs, then the slave will likely not report back to master because
#'  it hangs in miscommunication and remains idle (check activity, e.g., with \code{top}).
mpi_work <- compiler::cmpfun(function(verbose = FALSE) {
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

        result <- match.fun("do_OneSite")(i_sim = dat$i_sim,
          i_labels = dat$i_labels,
          i_SWRunInformation = dat$i_SWRunInformation,
          i_sw_input_soillayers = dat$i_sw_input_soillayers,
          i_sw_input_treatments = dat$i_sw_input_treatments,
          i_sw_input_cloud = dat$i_sw_input_cloud,
          i_sw_input_prod = dat$i_sw_input_prod,
          i_sw_input_site = dat$i_sw_input_site,
          i_sw_input_soils = dat$i_sw_input_soils,
          i_sw_input_weather = dat$i_sw_input_weather,
          i_sw_input_climscen = dat$i_sw_input_climscen,
          i_sw_input_climscen_values = dat$i_sw_input_climscen_values)

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
})
