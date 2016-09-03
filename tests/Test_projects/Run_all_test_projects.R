print(paste0(Sys.time(), ": running SWSF test projects"))

#---User input
# Current working directory must be a subfolder of 'SoilWat_R_Wrapper' or itself
dir.old <- getwd()
dir.test <- if (grepl("SoilWat_R_Wrapper", dir.old)) {
    temp <- strsplit(dir.old, .Platform$file.sep, fixed = TRUE)[[1]]
    temp <- do.call("file.path", as.list(temp[seq_len(which(temp == "SoilWat_R_Wrapper"))]))
    file.path(temp, "tests", "Test_projects")
    
  } else {
    temp <- if (interactive()) {
      readline("Enter path to folder of test projects 'Test_projects': ")
    } else dir.old
    if ("Test_projects" != basename(temp)) {
      stop("Folder of test projects 'Test_projects' could not be located.")
    }
    temp
  }
dir.test <- normalizePath(dir.test)
setwd(dir.test)

# Ask if output should be saved as future reference DB
make_new_ref <- if (interactive()) {
    temp <- readline("Should the test output be used as future reference (y/n): ")
    temp == "y" || temp == "Y" || grepl("yes", temp, ignore.case = TRUE)
  } else {
    FALSE
  }

# Ask if output should be deleted if all was ok
delete_output <- if (interactive()) {
    temp <- readline("Should the test output be deleted (y/n): ")
    temp == "y" || temp == "Y" || grepl("yes", temp, ignore.case = TRUE)
  } else {
    FALSE
  }

# Ask if output should be deleted even if there were problems
force_delete_output <- if (interactive()) {
    temp <- readline("Should the test output be force deleted (y/n): ")
    temp == "y" || temp == "Y" || grepl("yes", temp, ignore.case = TRUE)
  } else {
    FALSE
  }


# Ask which of the test projects should be run
temp <- list.dirs(dir.test, full.names = FALSE, recursive = FALSE)
tests <- file.path(dir.test, grep("[Test][[:digit:]+][_]", basename(temp), value = TRUE))
which_tests_torun <- if (interactive()) {
    temp <- readline(paste("Which of the",
                          length(tests),
                          "tests should be run",
                          "('all'; a single number; several numbers separated by commas): "))
    if ("all" == temp) {
      seq_along(tests)
    } else {
      temp <- unique(as.integer(strsplit(gsub("[[:space:]]", "", temp), ",")[[1]]))
      intersect(temp, seq_along(tests))
    }
  } else {
    seq_along(tests)
  }


#---Load functions
source(file.path(dir.test, "Functions_for_test_projects.R"), keep.source = FALSE)


#---Run projects
out <- run_test_projects(dir.test, tests,
  which_tests_torun,
  delete_output, force_delete_output, make_new_ref)

print(out)

setwd(dir.old)
print(paste0(Sys.time(), ": end of SWSF test projects"))
