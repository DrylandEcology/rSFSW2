#' Error function
#' @seealso Code is from examples of \code{\link[stats]{pnorm}}.
#' @param x A numeric vector.
#' @return A numeric vector of the size of \code{x}.
erf <- function(x) 2 * stats::pnorm(x * sqrt(2)) - 1

#' Stretch values
#'
#' Values above the mean of \code{x} are made larger and
#' values below the mean are made smaller - each by
#' \code{lambda * dist(x, mean(x))}.
#'
#' @param x A numeric vector.
#' @param lambda A numeric value. The stretching factor applied to \code{x}.
#'
#' @return A numeric vector of the size of \code{x}.
stretch_values <- function(x, lambda = 0) {
  (1 + lambda) * x - lambda * mean(x)
}

in_box <- function(xy, xbounds, ybounds, i_use) {
  !i_use &
  xy[, 1] >= xbounds[1] & xy[, 1] <= xbounds[2] &
  xy[, 2] >= ybounds[1] & xy[, 2] <= ybounds[2]
}


cut0Inf <- rSOILWAT2:::cut0Inf
finite01 <- rSOILWAT2:::finite01



#' Functions for circular descriptive statistics
#'
#' @param x A numeric vector or a matrix. If a data.frame is supplied, then
#'   \code{x} is coerced to a matrix.
#' @param int A numeric value. The number of units of \code{x} in a full circle,
#'   e.g., for unit days: \code{int = 365}; for unit months: \code{int = 12}.
#' @param na.rm A logical value indicating whether \code{NA} values should be
#'   stripped before the computation proceeds.
#'
#' @return A numeric value or \code{NA}.
#'
#' @seealso \code{\link[circular]{mean.circular}},
#'   \code{\link[circular]{range.circular}}, \code{\link[circular]{sd.circular}}
#'
#' @aliases circ_mean circ_range circ_sd
#' @name circular
NULL

#' @rdname circular
circ_mean <- function(x, int, na.rm = FALSE) {
  if (!all(is.na(x)) && requireNamespace("circular", quietly = TRUE)) {
    circ <- 2 * pi / int
    x_circ <- circular::circular(x * circ, type = "angles", units = "radians",
      rotation = "clock", modulo = "2pi")
    x_int <- circular::mean.circular(x_circ, na.rm = na.rm) / circ

    # map 0 -> int; rounding to 13 digits: 13 was empirically derived for
    # int = {12, 365} and
    # x = c((-1):2, seq(x-5, x+5, by = 1), seq(2*x-5, 2*x+5, by = 1)) assuming
    # that this function will never need to calculate for x > t*int with t>2
    round(as.numeric(x_int) - 1, 13) %% int + 1
  } else {
    NA
  }
}

#' @rdname circular
circ_range <- function(x, int, na.rm = FALSE) {
  if (!all(is.na(x)) && requireNamespace("circular", quietly = TRUE)) {
    circ <- 2 * pi / int
    x_circ <- circular::circular(x * circ, type = "angles", units = "radians",
      rotation = "clock", modulo = "2pi")
    x_int <- range(x_circ, na.rm = na.rm) / circ
    as.numeric(x_int)

  } else {
    NA
  }
}

#' @rdname circular
circ_sd <- function(x, int, na.rm = FALSE) {
  if (length(x) - sum(is.na(x)) > 1 && requireNamespace("circular",
    quietly = TRUE)) {

    if (stats::sd(x, na.rm = TRUE) > 0) {
      circ <- 2 * pi / int
      x_circ <- circular::circular(x * circ, type = "angles", units = "radians",
        rotation = "clock", modulo = "2pi")
      x_int <- circular::sd.circular(x_circ, na.rm = na.rm) / circ
      as.numeric(x_int)
    } else {
      0
    }
  } else {
    NA
  }
}

#' Find the \code{k}-largest/smallest values (and apply a function to these
#' values)
#'
#' @param x A numeric vector
#' @param largest A logical value. See return value.
#' @param fun A function which requires one argument or \code{"index"}.
#'   \code{fun} will be applied to the \code{k}-largest/smallest values of
#'   \code{x}.
#' @param k An integer value. The \code{k}-largest/smallest value(s) of \code{x}
#'   will be used. The largest/smallest value will be used if 0 or negative.
#' @param na.rm A logical value indicating whether \code{NA} values should be
#'   stripped before the computation proceeds.
#' @param \dots Optional arguments to be passed to \code{fun}
#'
#' @return A vector of length \code{k}, \itemize{
#'   \item if \code{is.null(fun)}, then a vector with the \code{k}-largest
#'     (if \code{largest = TRUE}) or \code{k}-smallest
#'     (if \code{largest = FALSE}) values of \code{x};
#'   \item if \code{fun = "index"}, then a vector with indices of the
#'     \code{k}-largest/smallest values (NOTE: this is truncated to the
#'     \code{k}-first indices!). } Otherwise, the result of applying \code{fun}
#'     to the \code{k}-largest/smallest values.
fun_kLargest <- function(x, largest = TRUE, fun = NULL, k = 10L,
  na.rm = FALSE, ...) {

  res <- if (na.rm) {
      stats::na.exclude(x)
    } else {
      x
    }

  # Determine k-largest/smallest values
  res <- sort.int(res, decreasing = largest, na.last = !na.rm,
    method = if (getRversion() >= "3.3.0") "radix" else "quick")
  res <- res[seq_len(max(1L, min(length(res), as.integer(k))))]

  # Calculate return values
  if (is.null(fun)) {
    res
  } else if (identical(fun, "index")) {
    which(x %in% res)[seq_len(k)]
  } else {
    fun(res, ...)
  }
}


handle_NAs <- function(x, na.index, na.act) {
  if (length(na.index) > 0) {
    stats::napredict(na.act, x)
  } else {
    x
  }
}

scale_by_sum <- function(x) {
  temp <- sum(x, na.rm = TRUE)
  if (temp > 0 && is.finite(temp)) {
    x / temp
  } else {
    x
  }
}


cor2 <- function(y) {
  res <- try(stats::cor(y[, 1], y[, 2]), silent = TRUE)
  if (inherits(res, "try-error")) NA else res
}


#' Check that data are within range of normal distribution
#'
#' @param data A numeric vector. Daily values of temperature.
#' @param sigmaN An integer value. A multiplier of \code{stats::sd}.
test_sigmaNormal <- function(data, sigmaN = 6) {
  md <- mean(data)
  sdd <- stats::sd(data) * sigmaN
  stopifnot(data < md + sdd, data > md - sdd)
}


#' Check that data are within range of an approximated gamma distribution
#'
#' @section Note: Approximate shape and scale instead of very slow call:
#'   \code{g <- MASS::fitdistr(data, "gamma")}
#' @param data A numeric vector. Daily values of precipitation.
#' @param sigmaN An integer value. A multiplier of \code{stats::sd}.
#' @references Choi, S. C., and R. Wette. 1969. Maximum Likelihood Estimation of
#'   the Parameters of the Gamma Distribution and Their Bias. Technometrics
#'   11:683-690.
# nolint start
#' @references
#'   \url{http://en.wikipedia.org/wiki/Gamma_distribution#Maximum_likelihood_estimation}
# nolint end
test_sigmaGamma <- function(data, sigmaN = 6) {
  tempD <- data[data > 0]

  if (length(tempD) >= 2 && stats::sd(tempD) > 0) {
    tempM <- mean(tempD)
    temp <- log(tempM) - mean(log(tempD))
    gshape <- (3 - temp + sqrt((temp - 3) ^ 2 + 24 * temp)) / (12 * temp)
    gscale <- tempM / gshape
    stopifnot(data < stats::qgamma(erf(sigmaN / sqrt(2)), shape = gshape,
      scale = gscale))
  }
}

#' Index of the closest value in the matrix to the passed in value.
whereNearest <- function(val, matrix) {
  which.min(abs(matrix - val))
}

#' Test whether input represents a natural number
#' @param x An integer, numeric, or complex vector, matrix, or array.
#' @return A logical value.
is.natural <- function(x) {
  typeof(x) %in% c("integer", "double", "complex") &&
  !is.null(x) && length(x) > 0 && !is.na(x) &&
  isTRUE(all.equal(x, round(x))) && x > 0
}

#' The intersection on any number of vectors
#'
#' @param \dots Any number of vectors or a list of vectors.
#' @return A vector of the same mode as inputs.
#' @seealso \code{\link{intersect}}
intersect2 <- function(...) {
  x <- list(...)
  n <- length(x)

  if (is.list(x[[1]]) && n == 1) {
    x <- x[[1]]
    n <- length(x)
  }

  res <- NULL
  if (n > 1) {
    if (all(lengths(x)) > 0) {
      res <- x[[1]]
      for (k in 2:n) {
        res <- intersect(res, x[[k]])
      }
    }

  } else {
    res <- x[[1]]
  }

  res
}


#' Recursive comparisons which also works for nested lists
#'
#' @param x1 A R object
#' @param x2 A R object
#'
#' @seealso \code{\link{all.equal}}
#'
#' @return \itemize{
#'  \item If both \code{x1} and \code{x2} are lists, then \code{do_compare}
#'    is called recursively on mutually shared names if names exists and on
#'    each element otherwise, and the output is a list from the return value
#'    of each recursive call.
#'  \item Otherwise, the function \code{\link{all.equal}} is called. If the
#'    result is \code{TRUE}, then \code{NA} is returned. If the result is
#'    \code{FALSE}, then a list with three elements is returned with \describe{
#'    \item{eq}{the result of the call to \code{\link{all.equal}}}
#'    \item{x1}{The object \code{x1}}
#'    \item{x2}{The object \code{x2}}
#'  }}
#'
#' @examples
#'  ## expected result: NA
#'  do_compare(1L, 1L)
#'
#'  ## expected result: list(eq = "Mean relative difference: 1", x1 = 1, x2 = 2)
#'  do_compare(1, 2)
#'
#   ## expected result: first comparison returns NA; second shows a difference
#'  do_compare(list(1, 2), list(1, 3))
#'  ## expected result: comparison for elements a and b return NA; comparison
#'  ## for element c shows a difference
#'  do_compare(list(a = 1, b = 2), list(b = 2, c = 0, a = 1))
#' @export
do_compare <- function(x1, x2) {
  if (is.list(x1) && is.list(x2)) {
    dims <- if (!is.null(names(x1)) && !is.null(names(x2))) {
        unique(c(names(x1), names(x2)))
      } else {
        seq_len(min(length(x1), length(x2)))
      }

    # as of R v3.4.1 'Recall' doesn't work as argument to apply-type calls
    res <- lapply(dims, function(k) do_compare(x1 = x1[[k]], x2 = x2[[k]]))
    names(res) <- dims
    res

  } else {
    eq <- all.equal(x1, x2)

    if (isTRUE(eq)) {
      NA
    } else {
      list(eq = eq, x1 = x1, x2 = x2)
    }
  }
}


#' Check that values in matrix-like object are (strictly) monotonically
#' increasing/decreasing
#'
#' @param x A numeric matrix like object.
#' @param MARGIN An integer value giving the subscripts over which the
#'   monotonicity will be checked; 1 indicates rows, 2 indicates columns.
#' @param increase A logical value. If \code{TRUE}, check monotonic increase; if
#'   \code{FALSE}, check monotonic decrease.
#' @param strictly A logical value. If \code{TRUE}, check for a strict monotonic
#'   pattern.
#' @param fail A logical value. If \code{TRUE}, throw error if monotonic check
#'   fails.
#' @param replacement A value that replaces non-(strictly) monotonically
#'   increasing/decreasing values if \code{fail} is \code{FALSE}.
#' @param na.rm A logical value. If \code{TRUE}, then ignore \code{NA}s; if
#'   \code{FALSE}, then fail if \code{strictly} or replace with
#'   \code{replacement}.
#' @return The updated \code{x}.
#' @export
check_monotonic_increase <- function(x, MARGIN = 1, increase = TRUE,
  strictly = FALSE, fail = FALSE, replacement = NA, na.rm = FALSE) {

  stopifnot(MARGIN %in% c(1, 2), length(dim(x)) == 2)

  x <- as.matrix(x)
  if (MARGIN == 2) {
    x <- t(x)
  }

  mfun <- if (increase) {
      if (strictly) ">" else ">="
    } else {
      if (strictly) "<" else "=<"
    }

  ord <- !match.fun(mfun)(x[, -1, drop = FALSE], x[, -ncol(x), drop = FALSE])
  has_na <- is.na(x)

  if (any(ord, na.rm = TRUE) || (has_na && !na.rm && strictly)) {
    if (fail) {
      stop(paste0("'check_monotonic_increase': data are not ",
        if (strictly) "strictly ", "monotonically ",
        if (increase) "increasing " else "decreasing ",
        if (MARGIN == 1) "in rows." else "in columns."))

    } else {
      x[, -1][is.na(ord) | ord] <- replacement
      x[is.na(x[, 1]), 1] <- replacement
    }
  }

  if (MARGIN == 1) x else t(x)
}
