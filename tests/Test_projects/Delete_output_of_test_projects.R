delete_test_output <- function(dir_test) {
		try(unlink(file.path(dir_test, "1_Data_SWInput", "dbWeatherData_test.sqlite3")), silent = TRUE)
		try(unlink(file.path(dir_test, "1_Data_SWInput", "SWRuns_InputAll_PreProcessed.RData")), silent = TRUE)
		try(unlink(file.path(dir_test, "1_Data_SWInput", "swrun", ".Rapp.history")), silent = TRUE)
		try(unlink(file.path(dir_test, list.files(dir_test, pattern = "last.dump"))), silent = TRUE)
		try(unlink(file.path(dir_test, ".Rapp.history")), silent = TRUE)
		try(unlink(file.path(dir_test, "3_Runs"), recursive = TRUE), silent = TRUE)
		try(unlink(file.path(dir_test, "4_Data_SWOutputAggregated"), recursive = TRUE), silent = TRUE)
}
