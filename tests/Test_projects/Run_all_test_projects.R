print(paste0(Sys.time(), ": running SWSF test projects"))

dir.old <- getwd()

if (basename(dir.old) != "Test_projects") {
	dir.test <- normalizePath(file.path(".", "tests", "Test_projects"))
	setwd(dir.test)
} else {
	dir.test <- dir.old
}

tests <- list.dirs(dir.test, recursive = FALSE)

if (length(tests) > 0) {
	for (it in seq_along(tests)) {
		print(paste0(Sys.time(), ": running test project '", basename(tests[it]), "'"))

		test_code <- list.files(tests[it], pattern = "2_SWSF_p1of4_")
	
		if (length(test_code) == 1L) {
			setwd(tests[it])
			source(test_code, verbose = FALSE, chdir = FALSE)
		
		} else {
			print("Source code for test project not found.")
		}
	}

	delete_output <- if (interactive()) {
			temp <- readline("Should the test output be deleted (y/n): ")
			temp == "y" || temp == "Y" || grepl("yes", temp, ignore.case = TRUE)
		} else {
			FALSE
		}

	if (delete_output) for (it in seq_along(tests)) {
		try(unlink(file.path(tests[it], "1_Data_SWInput", "dbWeatherData_test.sqlite3")))
		try(unlink(file.path(tests[it], "1_Data_SWInput", "SWRuns_InputAll_PreProcessed.RData")))
		try(unlink(file.path(tests[it], "last.dump.rda")))
		try(unlink(file.path(tests[it], "1_Data_SWInput", "swrun", ".Rapp.history")))
		try(unlink(file.path(tests[it], "3_Runs"), recursive = TRUE))
		try(unlink(file.path(tests[it], "4_Data_SWOutputAggregated"), recursive = TRUE))
	}
}

print(paste0(Sys.time(), ": end of SWSF test projects"))
