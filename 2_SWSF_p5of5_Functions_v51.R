getStartYear <- function(simstartyr) simstartyr + 1

set_PRAGMAs <- function(con, settings) {
  temp <- lapply(settings, function(x) RSQLite::dbGetQuery(con, x))
  invisible(0)
}

getSiteIds <- function(con, folderNames) {
  wf_ids <- RSQLite::dbGetQuery(con, "SELECT id, folder FROM weatherfolders")
  wf_ids[match(folderNames, wf_ids[, "folder"], nomatch = NA), "id"]
}

