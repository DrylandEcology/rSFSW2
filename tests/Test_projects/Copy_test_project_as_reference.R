#' Copy output database of a test project to reference folder
#'
#' This function is called for its side effect of copying a file to the reference folder.
#'
#' @param dir_test A character vector. Path to test project folder.
#' @param dir_ref A character vector. Path to folder with reference database.
#' @param SWSF_version A character vector. The version ID of the simulation framework as
#'  reported by the file \code{DESCRIPTION}.
#'
#' @return A logical value. \code{TRUE} if successful.
make_test_output_reference <- function(dir_test, dir_ref = NULL, SWSF_version = NULL) {
  if (is.null(SWSF_version)) {
    temp <- readLines(file.path(dir_test, "..", "..", "..", "DESCRIPTION"))
    v <- grep("Version: ", temp, value = TRUE)
    if (length(v) > 0) {
      v <- strsplit(v[1], "Version: ", fixed = TRUE)[[1]][2]
    } else {
      print("'SWSF_version' cannot be detected.")
      return(FALSE)
    }
  }

	if (is.null(dir_ref))
		dir_ref <- file.path(dir_test, "..", "0_ReferenceOutput")
	if (!file.exists(dir_ref))
	  dir.create(dir_ref, recursive = TRUE, showWarnings = FALSE)
  
  fdb <- file.path(dir_test, "4_Data_SWOutputAggregated", "dbTables.sqlite3")
  if (file.exists(fdb)) {
    fdb_ref <- paste0("dbTables_", basename(dir_test), "_v", v, ".sqlite3")
    res <- file.rename(fdb, file.path(dir_ref, fdb_ref))
    
  } else {
    print(paste("Output DB of test project", shQuote(basename(dir_test)), "cannot be located"))
    res <- FALSE
  }
  
  res
}
