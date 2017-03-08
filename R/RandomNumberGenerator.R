
#' Implementation of Pierre L'Ecuyer's RngStreams for N tasks
#'
#' The function \code{\link[parallel]{clusterSetRNGStream}} creates a stream for each
#'  worker/slave, and thus replicability can only be realized if each task is assigned to
#'  the same worker on repeated runs. This is usually not guaranteed with
#'  load-balancing parallel computations or when a long computation is being re-started
#'  from previous partially completed tasks/results.
#' This implementation generates a stream for each unique tasks (instead for each worker)
#'  and thus avoids such problems.
#'
#' The current RNG kind, if required, must be captured before this function is called
#'  because the function sets the kind to "L'Ecuyer-CMRG" (see examples).
#'
#' @param N An integer. The number of streams to generate.
#' @param seed An integer or \code{NULL}. The seed used by \code{\link{set.seed}} to
#'  set the (global/master) random generator, i.e., before generating the seeds of the
#'  streams.
#' @param reproducible A logical value. If \code{TRUE}, then \code{N} are
#'  prepared. If \code{FALSE}, then instead \code{NA}s are returned.
#' @return A list of length \code{N} containing the seed for each stream.
#'
#' @seealso \code{\link[parallel]{clusterSetRNGStream}}
#'
#' @examples
#' RNGkind_prev <- RNGkind()
#' seeds <- generate_RNG_streams(10, seed = 123)
#' # do work with random numbers
#' RNGkind(kind = RNGkind_prev[1], normal.kind = RNGkind_prev[2])
#'
#' @export
generate_RNG_streams <- function(N, seed = NULL, reproducible = TRUE) {

  if (reproducible) {
    oldseed <- get0(".Random.seed", envir = as.environment(1L), inherits = FALSE)

    on.exit({
      if (!is.null(oldseed)) {
          assign(".Random.seed", oldseed, pos = 1L)
      } else {
        rm(.Random.seed, pos = 1L)
      }
    }, add = TRUE)

    RNGkind("L'Ecuyer-CMRG")

    if (!is.null(seed)) set.seed(seed)

    seeds <- vector("list", N)
    seeds[[1L]] <- .Random.seed

    for (i in seq_len(N - 1L)) {
      seeds[[i + 1L]] <- parallel::nextRNGStream(seeds[[i]])
    }

    seeds

  } else {
    as.list(rep(NA, N))
  }
}


#' Specify all aspects of the R random number generator
#'
#' @inheritParams base::Random
#'
#' @seealso \code{\link{set.seed}}, \code{\link{RNGkind}}
#' @export
set_full_RNG <- function(seed = NULL, kind = "default", normal.kind = "default") {
  RNGkind(kind = kind, normal.kind = normal.kind)
  set.seed(seed = seed)
}


#' Set the seed of a random number generator (stream)
#'
#' This function is to be called by a (parallel) worker using as argument a pre-prepared
#'  seed from the function \code{\link{generate_RNG_streams}}. Note: it will also set
#'  RNGkind accordingly to the first element of seed.
#'
#' @param seed A vector appropriate for \code{\link{.Random.seed}} of the current RNG; a
#'    single integer or NULL that will be passed to set.seed(); or NA which will not
#'    affect the random number generator.
#'
#' @seealso \code{\link{set.seed}}, \code{\link{RNGkind}}
#' @export
set_RNG_stream <- function(seed = NA) {
  if (!anyNA(seed)) {
    if (length(seed) > 1) {
      assign(".Random.seed", seed, pos = 1L)

    } else {
      print(paste("'seed' was not appropriate to init '.Random.seed':",
        paste(seed[seq_len(3)], collapse = "/"), "/...; instead the function 'set.seed'",
        "will be used"))
      set_full_RNG(seed, kind = NULL, normal.kind = NULL)
    }
  }

  invisible(NULL)
}

#' Organizing previous state and streams of random number generator
#'
#' @section Usage: RNG - parallelized function calls by \code{rSFSW2}
#'  \itemize{
#'    \item \code{try.ScenarioWeather} wraps \code{calc.ScenarioWeather} which calls
#'          \code{set_RNG_stream} to prepare RNG for functions \itemize{
#'          \item \code{fix_PPTdata_length}
#'          \item \code{calc_Days_withLoweredPPT}
#'          \item \code{controlExtremePPTevents}
#'          }
#'    \item \code{do_OneSite} calls \code{set_RNG_stream} to prepare RNG for functions
#'          \itemize{
#'          \item \code{calculate_TimeToGerminate_modifiedHardegree2006NLR}
#'          }
#'    }
#'
#' @section Note: Parallelized function calls without using RNG
#'  \itemize{
#'    \item \code{ISRICWISE12_extract_SUIDs}
#'    \item \code{ISRICWISE12_try_weightedMeanForSimulationCell}
#'    \item \code{collect_EnsembleFromScenarios}
#'    \item \code{missing_Pids_outputDB}
#'    \item \code{gribDailyWeatherData}
#'    \item \code{writeDailyWeatherData}
#'    \item \code{gribMonthlyClimate}
#'    \item \code{writeMonthlyClimate}
#'    }
#'
#' @param streams_N An integer value representing the number of tasks for which Pierre
#'  L'Ecuyer's RngStreams should be generated.
#' @param global_seed A vector appropriate for \code{\link{.Random.seed}} of the current
#'  RNG; a single integer or NULL that will be passed to set.seed().
#' @param reproducible A logical value. If \code{TRUE}, then \code{streams_N} are
#'  prepared. If \code{FALSE}, then instead \code{NA}s are returned.
#'
#' @return A list with four elements: \itemize{
#'    \item \code{seed_prev} captures the previous state of the RNG seed.
#'    \item \code{RNGkind_prev} captures the previous kind of the RNG.
#'    \item \code{global_seed} is the seed used to set the RNG for stream generation.
#'    \item \code{seeds_runN} is a list with a stream of \code{streams_N} seeds.
#'    \item \code{seeds_DS} is an empty list -- a placeholder for a list with a stream
#'          of seeds for climate scenario downscaling.
#'    }
#'
#' @seealso \code{\link{set.seed}}
#' @export
setup_RNG <- function(streams_N, global_seed = NULL, reproducible = TRUE) {
  rng <- list()

  rng[["seed_prev"]] <- get0(".Random.seed", envir = as.environment(1L), inherits = FALSE)

  set_full_RNG(global_seed)
  rng[["RNGkind_prev"]] <- RNGkind()
  rng[["global_seed"]] <- global_seed

  # Seeds for climate change downscaling: only generate if required
  rng[["seeds_DS"]] <- list()

  # Seeds for simulation runs 'do_OneSite'
  rng[["seeds_runN"]] <- generate_RNG_streams(N = streams_N, seed = global_seed,
    reproducible = reproducible)

  rng
}
