########################
#------ Synchronicity functions

#' Synchronicity with backing store locks
#'
#' @section Note: My attempts at using lock/unlock functions from \pkg{flock} failed
#'  because unlock appeared to not free resources. My attempts at using such functions
#'  from \pkg{synchronicity} failed because of segfaults with
#'  \code{GetResourceName(m@mutexInfoAddr)}; additionally, \pkg{synchronicity} is
#'  restricted to unix systems. Instead, I use here ideas based on functions
#'  from \pkg{Rdsm} and Samuel.
#'
#' @param lock An object of class \code{SWSF_lock}.
#'
#' @aliases lock unlock lock_init lock_attempt unlock_access lock_access
#'  check_lock_content remove_lock
#' @name synchronicity
NULL

#' Create and initialize an object of class \code{SWSF_lock}
#'
#' @param fname_lock A character string. Path to locking directory.
#' @param id A R object. Identifier used to test unique write access in locking directory.
#' @return An object of class \code{SWSF_lock}.
#' @rdname synchronicity
lock_init <- function(fname_lock, id) {
  lock <- list(
    dir = fname_lock,
    file = file.path(fname_lock, "lockfile.txt"),
    locker_id = id,
    code = paste("access locked for", id),
    obtained = FALSE,
    confirmed_access = NA)
  class(lock) <- "SWSF_lock"
  lock
}


#' Check content of a backing store lock
#'
#' @return A logical value. \code{TRUE} if lock file contains proper code of the lock.
#' @rdname synchronicity
check_lock_content <- compiler::cmpfun(function(lock) {
  if (file.exists(lock$file)) {
    out <- readBin(lock$file, what = "character")
    identical(lock$code, out)
  } else {
    FALSE
  }
})

#' Remove the files associated with a backing store lock
#'
#' @return The return value of unlinking the directory associated with \code{lock}.
#' @rdname synchronicity
remove_lock <- function(lock) {
  unlink(lock$dir, recursive = TRUE)
}

#' Unlock a backing store lock
#'
#' @return The updated \code{lock} object of class \code{SWSF_lock}.
#' @rdname synchronicity
unlock_access <- function(lock) {
  if (inherits(lock, "SWSF_lock")) {
    lock$confirmed_access <- check_lock_content(lock)
    remove_lock(lock)
  }

  lock
}


#' Attempt to obtain access of a backing store lock
#'
#' @return The updated \code{lock} object of class \code{SWSF_lock}.
#' @rdname synchronicity
lock_attempt <- compiler::cmpfun(function(lock) {
  if (dir.create(lock$dir, showWarnings = FALSE)) {
    writeBin(lock$code, con = lock$file)
    lock$obtained <- check_lock_content(lock)
    if (!lock$obtained)
      remove_lock(lock)
  }

  lock
})


#' Set a backing store lock
#'
#' Access to the backing store lock is attempted until successfully obtained.
#'
#' @param verbose A logical value. If \code{TRUE}, then each attempt at obtaining the lock
#'  is printed.
#' @return The updated \code{lock} object of class \code{SWSF_lock}.
#' @rdname synchronicity
lock_access <- compiler::cmpfun(function(lock, verbose = FALSE) {
  if (inherits(lock, "SWSF_lock")) while (!lock$obtained) {
    if (verbose)
      print(paste(Sys.time(), "attempt to obtain lock for", shQuote(lock$locker_id)))
    lock <- lock_attempt(lock)
    Sys.sleep(runif(1, 0.02, 0.1))
  }

  lock
})

#------ End of synchronicity functions
########################
