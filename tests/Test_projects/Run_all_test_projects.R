print(paste0(Sys.time(), ": running SWSF test projects"))

delete_output <- FALSE
delete_output_noninteractive <- FALSE
problems <- list()
fname_report <- "Test_project_report.txt"

dir.old <- getwd()

if (basename(dir.old) != "Test_projects") {
  dir.test <- normalizePath(file.path(".", "tests", "Test_projects"))
  setwd(dir.test)
} else {
  dir.test <- dir.old
}

source(file.path(dir.test, "Delete_output_of_test_projects.R"), keep.source = FALSE)
source(file.path(dir.test, "Compare_output_of_test_project_to_reference.R"), keep.source = FALSE)

temp <- list.dirs(dir.test, full.names = FALSE, recursive = FALSE)
tests <- file.path(dir.test, grep("[Test][[:digit:]+][_]", basename(temp), value = TRUE))

if (length(tests) > 0) {
  for (it in seq_along(tests)) {
    print(paste0(Sys.time(), ": running test project '", basename(tests[it]), "'"))

    test_code <- list.files(tests[it], pattern = "2_SWSF_p1of")

    if (length(test_code) == 1L) {
      setwd(tests[it])
      temp <- try(source(test_code, verbose = FALSE, chdir = FALSE))
    
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
      print("Test output will not be deleted because problems were detected.")
      fname_report <- paste0(format(Sys.time(), "%Y%m%d-%H%M"), "_", fname_report)
      print(paste("See problem report in file", shQuote(fname_report)))
      writeLines(unlist(problems), con = file.path(dir.test, fname_report))
  }
  
  delete_output <- if (!has_problems) {
    if (interactive()) {
      temp <- readline("Should the test output be deleted (y/n): ")
      temp == "y" || temp == "Y" || grepl("yes", temp, ignore.case = TRUE)
    } else {
      delete_output_noninteractive
    }
  } else delete_output

  if (delete_output) for (test in tests)
    delete_test_output(test)
}

print(paste0(Sys.time(), ": end of SWSF test projects"))
