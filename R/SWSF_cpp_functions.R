#------ Remove when this becomes a R package


########################
#------ GISSM functions
# Schlaepfer, D.R., Lauenroth, W.K. & Bradford, J.B. (2014). Modeling regeneration responses of big sagebrush (Artemisia tridentata) to abiotic conditions. Ecol Model, 286, 66-77.


#' Determine if all conditions across rooted soil layers are deadly
#'
#' Function that checks whether all relevant (those with roots) soil layers are under conditions of mortality (kill.conditions) for each day of a given year
#'
#'  \code{relevantLayers} takes either \code{NA} if no soil layers should be considered
#'  (e.g., because not yet germinated), or an integer number between 1 and the number of
#'  simulated soil layers. The number indicates the depth to which a seedling has grown
#'  roots and over which layers \code{kill.conditions} will be evaluated.
#'
#' @setion: Note: The Rcpp version of the function is about 165x faster than the version
#'  previous to commit 6344857a9cdb08acf68fa031c43cf4a596613aad 'Small speed improvements'
#'  and about 70x faster than the R version. The Rcpp version also reduced the memory
#'  footprint by a factor of 200.
#'
#' @param relevantLayers An integer vector, usually of length 365 or 366 (days).
#' @param kill.conditions A m x p logical matrix with \code{m >= length(relevantLayers)}
#'  and p represents the number of simulated soil layers, i.e.,
#'  \code{p >= max(relevantLayers, na.rm = TRUE)}.
#'
#' @return A logical vector of the length of \code{relevantLayers} with values containing
#'  \code{NA} for days when conditions were not evaluated, \code{TRUE} if all
#'  relevant soil layers (columns) of \code{kill.conditions} were \code{TRUE}, and with
#'  \code{FALSE} otherwise
if (use_rcpp && requireNamespace("Rcpp")) {
  Rcpp::sourceCpp(file.path(dir.code, "src", "GISSM_get_KilledBySoilLayers.cpp"))

} else {
  get_KilledBySoilLayers <- compiler::cmpfun(function(relevantLayers, kill.conditions) {
    vapply(seq_along(relevantLayers), function(k)
        all(as.logical(kill.conditions[k, if (is.finite(relevantLayers[k])) seq_len(relevantLayers[k]) else NA, drop = FALSE])),
      FUN.VALUE = NA)
  })
}


#' @setion: Note: The Rcpp version of the function is about 270x faster for vectors of
#'  length 365 and 12,000x faster for vectors of length 11,000 than the R version.
#'  The Rcpp version also reduced the memory footprint by a factor of >> 3080.
if (use_rcpp && requireNamespace("Rcpp")) {
  Rcpp::sourceCpp(file.path(dir.code, "src", "GISSM_germination_wait_times.cpp"))

} else {
  germination_wait_times <- compiler::cmpfun(function(time_to_germinate, duration_fave_cond) {
    N <- length(time_to_germinate)
    na.exclude(unlist(lapply(seq_len(N), function(t) {
      if (is.finite(time_to_germinate[t])) {
        t1 <- duration_fave_cond[t:N]
        t2 <- na.exclude(t1)
        t3 <- which(t2[time_to_germinate[t]] == t1)[1]
        sum(is.na(t1[1:t3]))
      } else {
        NA
      }
    })))
  })
}


#' @setion: Note: The Rcpp version of the function is about 4x faster than the R version.
#'  The Rcpp version also reduced the memory footprint by a factor of 4.
if (use_rcpp && requireNamespace("Rcpp")) {
  Rcpp::sourceCpp(file.path(dir.code, "src", "GISSM_setFALSE_SeedlingSurvival_1stSeason.cpp"))

} else {
  setFALSE_SeedlingSurvival_1stSeason <- compiler::cmpfun(function(ss1s, ry_year_day, ry_useyrs, y, doy) {
    ss1s[ry_year_day == ry_useyrs[y]][doy] <- FALSE

    ss1s
  })
}




#------ End of GISSM functions
########################
