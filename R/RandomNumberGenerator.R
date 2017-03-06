#parallel-called functions:
#  try.ScenarioWeather
#  ISRICWISE12_extract_SUIDs
#  ISRICWISE12_try_weightedMeanForSimulationCell
#  collect_EnsembleFromScenarios
#  missing_Pids_outputDB
#  do_OneSite
#  gribDailyWeatherData
#  writeDailyWeatherData
#  gribMonthlyClimate
#  writeMonthlyClimate


#' Implementation of Pierre L'Ecuyer's RngStreams for N tasks
#'
#' The function \code{\link{parallel::clusterSetRNGStream}} creates a stream for each
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
#' @param iseed An integer or \code{NULL}. The seed used by \code{\link{set.seed}} to
#'  set the (global/master) random generator, i.e., before generating the seeds of the
#'  streams.
#' @return A list of length N containing the seed for each stream.
#'
#' @seealso \code{\link{parallel::clusterSetRNGStream}}
#'
#' @examples
#' RNGkind_prev <- RNGkind()
#' seeds <- prepare_RNG_streams(10, iseed = 123)
#' # do work with random numbers
#' RNGkind(kind = RNGkind_prev[1], normal.kind = RNGkind_prev[2])
#'
#' @export
prepare_RNG_streams <- function(N, iseed = NULL) {

  oldseed <- get0(".Random.seed", envir = as.environment(1L), inherits = FALSE)

  RNGkind("L'Ecuyer-CMRG")

  if (!is.null(iseed)) set.seed(iseed)

  seeds <- vector("list", N)
  seeds[[1L]] <- .Random.seed

  for (i in seq_len(N - 1L)) seeds[[i + 1L]] <- parallel::nextRNGStream(seeds[[i]])

  if (!is.null(oldseed)) {
      assign(".Random.seed", oldseed, pos = 1L)
  } else {
    rm(.Random.seed, pos = 1L)
  }

  seeds
}


#' Specify all aspects of the random number generator
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
#'  seed from the function \code{\link{prepare_RNG_streams}}. Note: it will also set
#'  RNGkind accordingly to the first element of seed.
#'
#' @param seed A vector appropriate for \code{\link{.Random.seed}} of the current RNG; a
#'    single integer or NULL that will be passed to set.seed(); or NA which will not
#'    affect the random number generator.
#'
#' @seealso \code{\link{set.seed}}, \code{\link{RNGkind}}
#' @export
set_RNG_stream <- function(seed) {
  if (!anyNA(seed)) {
    if (length(seed) > 1) {
      assign(".Random.seed", seed, pos = 1L)

    } else {
      print(paste("'seed' was not appropriate to init '.Random.seed':",
        paste(head(seed, n = 3), collapse = "/"), "/...; instead the function 'set.seed'",
        "will be used"))
      set_full_RNG(seed, kind = NULL, normal.kind = NULL)
    }
  }

  invisible(NULL)
}


setup_RNG <- function(streams_N, global_seed = NULL, reproducible = TRUE) {
  rng <- list()

  rng[["seed_prev"]] <- get0(".Random.seed", envir = as.environment(1L), inherits = FALSE)

  set_full_RNG(global_seed)
  rng[["RNGkind_prev"]] <- RNGkind()

  if (reproducible) {
    rng[["seeds_runN"]] <- prepare_RNG_streams(N = streams_N, iseed = global_seed)
    rng[["global_seed"]] <- global_seed

  } else {
    rng[["seeds_runN"]] <- as.list(rep(global_seed, streams_N))
    rng[["global_seed"]] <- global_seed
  }

  rng
}
