getStartYear <- function(simstartyr) simstartyr + 1

set_PRAGMAs <- function(con, settings) {
  temp <- lapply(settings, function(x) RSQLite::dbGetQuery(con, x))
  invisible(0)
}

getSiteIds <- function(con, folderNames) {
  wf_ids <- RSQLite::dbGetQuery(con, "SELECT id, folder FROM weatherfolders")
  wf_ids[match(folderNames, wf_ids[, "folder"], nomatch = NA), "id"]
}

has_nodata <- compiler::cmpfun(function(data, tag = NULL, MARGIN = 1) {
  if (is.null(tag)) {
    apply(data, MARGIN, function(x) all(is.na(x)))
  } else {
    apply(data[, grepl(tag, colnames(data)), drop = FALSE], MARGIN, function(x) all(is.na(x)))
  }
})

has_incompletedata <- compiler::cmpfun(function(data, tag = NULL, MARGIN = 1) {
  if (is.null(tag)) {
    apply(data, MARGIN, anyNA)
  } else {
    apply(data[, grepl(tag, colnames(data)), drop = FALSE], MARGIN, anyNA)
  }
})

