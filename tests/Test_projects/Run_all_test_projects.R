print(paste0(Sys.time(), ": running SWSF test projects"))

# User input
#------
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

make_new_ref <- if (interactive()) {
    temp <- readline("Should the test output be used as future reference (y/n): ")
    temp == "y" || temp == "Y" || grepl("yes", temp, ignore.case = TRUE)
  } else {
    FALSE
  }

delete_output <- if (interactive()) {
    temp <- readline("Should the test output be deleted (y/n): ")
    temp == "y" || temp == "Y" || grepl("yes", temp, ignore.case = TRUE)
  } else {
    FALSE
  }

problems <- list()
fname_report <- "Test_project_report.txt"

source(file.path(dir.test, "Delete_output_of_test_projects.R"), keep.source = FALSE)
source(file.path(dir.test, "Compare_output_of_test_project_to_reference.R"), keep.source = FALSE)
source(file.path(dir.test, "Copy_test_project_as_reference.R"), keep.source = FALSE)

temp <- list.dirs(dir.test, full.names = FALSE, recursive = FALSE)
tests <- file.path(dir.test, grep("[Test][[:digit:]+][_]", basename(temp), value = TRUE))

if (length(tests) > 0) {
  for (it in seq_along(tests)) {
    print(paste0(Sys.time(), ": running test project '", basename(tests[it]), "'"))

    test_code <- list.files(tests[it], pattern = "2_SWSF_p1of")

    if (length(test_code) == 1L) {
      setwd(if (interactive()) file.path(dir.test, "..", "..") else tests[it])
      temp <- try(source(file.path(tests[it], test_code), verbose = FALSE, chdir = FALSE))
    
      if (!inherits(temp, "try-error")) {
        comp <- compare_test_output(tests[it])
        if (length(comp) > 0) {
          problems <- c(problems, 
            paste("Problem list for test project", shQuote(basename(tests[it])), ":"),
            comp)
        }
      } else {
        problems <- c(problems, 
          paste("Source code for test project", shQuote(basename(tests[it])), "unsuccessful."))
    }
    
    } else {
        problems <- c(problems, 
          paste("Source code for test project", shQuote(basename(tests[it])), "not found."))
    }
  }

  has_problems <- length(problems) > 0
  if (has_problems) {
      if (delete_output || make_new_ref)
        print("Test output will not be deleted because problems were detected.")

      fname_report <- paste0(format(Sys.time(), "%Y%m%d-%H%M"), "_", fname_report)
      print(paste("See problem report in file", shQuote(fname_report)))
      writeLines(unlist(problems), con = file.path(dir.test, fname_report))
  }
  
  made_new_refs <- if (make_new_ref && !has_problems) {
      all(sapply(tests, function(test) make_test_output_reference(test)))
    } else TRUE
  
  if (delete_output && !has_problems && made_new_refs)
    for (test in tests) delete_test_output(test)
}

print(paste0(Sys.time(), ": end of SWSF test projects"))
