print(paste0(Sys.time(), ": running SWSF test projects"))

delete_output_noninteractive <- FALSE

dir.old <- getwd()

if (basename(dir.old) != "Test_projects") {
	dir.test <- normalizePath(file.path(".", "tests", "Test_projects"))
	setwd(dir.test)
} else {
	dir.test <- dir.old
}

source(file.path(dir.test, "Delete_output_of_test_projects.R"), keep.source = FALSE)

temp <- list.dirs(dir.test, full.names = FALSE, recursive = FALSE)
tests <- file.path(dir.test, grep("[Test][[:digit:]+][_]", basename(temp), value = TRUE))

if (length(tests) > 0) {
	for (it in seq_along(tests)) {
		print(paste0(Sys.time(), ": running test project '", basename(tests[it]), "'"))

		test_code <- list.files(tests[it], pattern = "2_SWSF_p1of")
	
		if (length(test_code) == 1L) {
			setwd(tests[it])
			try(source(test_code, verbose = FALSE, chdir = FALSE))
		
		} else {
			print("Source code for test project not found.")
		}
	}

	delete_output <- if (interactive()) {
			temp <- readline("Should the test output be deleted (y/n): ")
			temp == "y" || temp == "Y" || grepl("yes", temp, ignore.case = TRUE)
		} else {
			delete_output_noninteractive
		}

	if (delete_output) for (test in tests)
		delete_test_output(test)
}

print(paste0(Sys.time(), ": end of SWSF test projects"))
