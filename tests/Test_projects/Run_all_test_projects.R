#!/usr/bin/env Rscript
print(paste0(Sys.time(), ": running SWSF test projects"))
dir.old <- getwd()


#---Command line input for non-interactive use
if (!interactive()) {
  script_args <- commandArgs(trailingOnly = TRUE)

  if (any("--help" == script_args)) {
    print("Options:")
    print("    '--path' or '-p': -p=path to folder of test projects 'Test_projects'")
    print("    '--new_ref' or '-r': add output DB as new reference if successful")
    print("    '--force-delete' or '-D': force delete test output; implies '-d'")
    print("    '--delete' or '-d': delete test output if successful")
    print("    '--tests' or '-t': -t=all runs all available test projects;")
    print("                       -t=1,2 runs test projects 1 and 2 (separated by comma)")

    stop("End of help")
  }

  temp <- grepl("--path", script_args) | grepl("-p", script_args)
  path <- if (any(temp)) strsplit(script_args[temp][1], "=")[[1]][2] else dir.old

  make_new_ref <- any("--new_ref" == script_args) || any("-r" == script_args)

  force_delete_output <- any("--force-delete" == script_args) || any("-D" == script_args)

  delete_output <- any("--delete" == script_args) || any("-d" == script_args)

  temp <- grepl("--tests", script_args) | grepl("-t", script_args)
  which_tests_torun <- if (any(temp)) {
      strsplit(script_args[temp][1], "=")[[1]][2]
    } else {
      NA
    }

} else {
  path <- make_new_ref <- force_delete_output <- delete_output <- which_tests_torun <- NA
}


#---Interactive user input
# Current working directory must be a subfolder of 'SoilWat_R_Wrapper' or itself
dir.test <- if (grepl("SoilWat_R_Wrapper", dir.old)) {
    temp <- strsplit(dir.old, .Platform$file.sep, fixed = TRUE)[[1]]
    temp <- do.call("file.path", as.list(temp[seq_len(which(temp == "SoilWat_R_Wrapper"))]))
    file.path(temp, "tests", "Test_projects")

  } else {
    temp <- if (interactive()) {
      readline("Enter path to folder of test projects 'Test_projects': ")
    } else path

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
    make_new_ref
  }

# Ask if output should be deleted even if there were problems
force_delete_output <- if (interactive()) {
    temp <- readline("Should the test output be force deleted (y/n): ")
    temp == "y" || temp == "Y" || grepl("yes", temp, ignore.case = TRUE)
  } else {
    force_delete_output
  }

# Ask if output should be deleted if all was ok
delete_output <- if (!force_delete_output) {
    if (interactive()) {
      temp <- readline("Should the test output be deleted (y/n): ")
      temp == "y" || temp == "Y" || grepl("yes", temp, ignore.case = TRUE)
    } else {
      delete_output
    }
  } else TRUE

# Ask which of the test projects should be run
temp <- list.dirs(dir.test, full.names = FALSE, recursive = FALSE)
tests <- file.path(dir.test, grep("[Test][[:digit:]+][_]", basename(temp), value = TRUE))
temp <- if (interactive()) {
    readline(paste("Which of the",
                    length(tests),
                    "tests should be run",
                    "('all'; a single number; several numbers separated by commas;",
                    "zero or a negative number to delete any temporary objects): "))
  } else which_tests_torun

which_tests_torun <- if (!is.na(temp)) {
    if ("all" == temp) {
      seq_along(tests)
    } else {
      temp <- unique(as.integer(strsplit(gsub("[[:space:]]", "", temp), ",")[[1]]))
      if (all(temp < 1)) {
        -1
      } else {
        intersect(temp, seq_along(tests))
      }
    }
  } else {
    seq_along(tests)
  }


#---Load functions
source(file.path(dir.test, "Functions_for_test_projects.R"), keep.source = FALSE)


#---Run projects
if (any(which_tests_torun > 0)) {
  out <- run_test_projects(dir.test, tests, dir.old, which_tests_torun,
    delete_output, force_delete_output, make_new_ref)
  print(out)

} else if (which_tests_torun < 1) {
  print(paste0(Sys.time(), ": delete temporary disk files of SWSF test projects"))
  lapply(tests, delete_test_output)
}


setwd(dir.old)
print(paste0(Sys.time(), ": end of SWSF test projects"))
