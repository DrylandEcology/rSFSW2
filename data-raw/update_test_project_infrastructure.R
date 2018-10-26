#!/usr/bin/env Rscript

# test project infrastructure

dir_demo <- file.path("demo")
dir_definf <- file.path("data-raw")
ddefin <- file.path(dir_definf, "1_Input")
dir_testprj <- file.path("tests", "test_data", "TestPrj4")
dir_backup <- sub("TestPrj4", "TestPrj4_backup", dir_testprj)
dtestin <- file.path(dir_testprj, "1_Input")

# List of files that need manual checking/updating
fupdate_manual <- NULL

#--- Backup
print(paste("Create backup of", shQuote(dir_testprj), "as",
  shQuote(dir_backup)))

dir.create(dir_backup, showWarnings = FALSE)
stopifnot(dir.exists(dir_backup))
file.copy(from = dir_testprj, to = dir_backup, recursive = TRUE,
  copy.mode = TRUE, copy.date = TRUE)



#--- Update code files
fupdate_manual <- c(fupdate_manual,
  "SFSW2_project_descriptions.R", "SFSW2_project_settings.R")


#--- Input files
fnew <- list.files(ddefin, recursive = TRUE)

for (k in seq_along(fnew)) {
  if (file.exists(file.path(dtestin, fnew[k]))) {
    # Read first line and check whether identical header
    col_new <- readLines(file.path(ddefin, fnew[k]), n = 1)
    col_prev <- readLines(file.path(dtestin, fnew[k]), n = 1)

    if (!identical(col_new, col_prev)) {
      # file header not identical --> file needs updating:
      #  - TODO: should write function based on
      #    function `R/upgraders.R/upgrade_datafile`
      #  - but for now simply add file name to list of those which need manual
      #    work
      fupdate_manual <- c(fupdate_manual, fnew[k])
    }

  } else {
    if (!grepl("_YOURPROJECT_", fnew[k])) {
      # File does not yet exist in test project
      file.copy(from = file.path(ddefin, fnew[k]),
        to = file.path(dtestin, fnew[k]), copy.mode = TRUE, copy.date = TRUE)

    } else {
      fupdate_manual <- c(fupdate_manual, "SWRuns_InputMaster_Test_v11.csv")
    }
  }
}

if (length(fupdate_manual) > 0) {
  cat(paste0("Following files should be checked and, if needed, ",
    "updated manually:\n",
    paste0("  * ", shQuote(fupdate_manual), collapse = "\n"), "\n"))
}

#-----------------------
print(paste("NOTE: Remove", shQuote(dir_backup), "before pushing to repository",
  "if script worked well."))
