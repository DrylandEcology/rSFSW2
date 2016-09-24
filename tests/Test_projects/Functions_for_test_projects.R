#' Run test projects
#'
#' @param dir_test A character string. Path to overall test project folder.
#' @param dir_tests A vector of character strings. Paths to individual test projects.
#' @param dir_prev A character string. Paths to directory that should be set when function returns.
#' @param which_tests_torun An integer vector. Indices of \code{dir_tests} which will be
#'  carried out.
#' @param delete_output A logical value. If \code{TRUE} then output will be deleted unless
#'  problems showed up.
#' @param force_delete_output A logical value. If \code{TRUE} then output will be deleted
#'  even if problems showed up.
#' @param make_new_ref A logical value. If \code{TRUE} then output database will be saved
#'  as new reference.
#'
#' @return A logical, named vector with four items: \code{has_run}, \code{has_problems},
#'  \code{made_new_refs}, \code{deleted_output}
run_test_projects <- function(dir_test, dir_tests, dir_prev = NULL,
                              which_tests_torun = seq_along(dir_tests),
                              delete_output = FALSE,
                              force_delete_output = FALSE,
                              make_new_ref = FALSE) {

  if (is.null(dir_prev))
    dir_prev <- getwd()
  on.exit(setwd(dir_prev))
  op_prev <- options(c("warn", "error"))
  on.exit(options(op_prev), add = TRUE)

  problems <- list()
  fname_report <- "Test_project_report.txt"
  has_run <- FALSE

  if (length(which_tests_torun) > 0) {
    for (it in which_tests_torun) {
      print(paste0(Sys.time(), ": running test project '", basename(dir_tests[it]), "'"))

      test_code <- list.files(dir_tests[it], pattern = "2_SWSF_p1of")

      if (length(test_code) == 1L) {
        setwd(if (interactive()) file.path(dir_test, "..", "..") else dir_tests[it])
        temp <- try(source(file.path(dir_tests[it], test_code), verbose = FALSE, chdir = FALSE))

        if (!inherits(temp, "try-error")) {
          has_run <- TRUE
          comp <- compare_test_output(dir_tests[it])
          if (length(comp) > 0) {
            problems <- c(problems,
              paste("Problem list for test project", shQuote(basename(dir_tests[it])), ":"),
              comp)
          }
        } else {
          problems <- c(problems,
            paste("Source code for test project", shQuote(basename(dir_tests[it])), "unsuccessful."))
      }

      } else {
          problems <- c(problems,
            paste("Source code for test project", shQuote(basename(dir_tests[it])), "not found."))
      }
    }

    has_problems <- length(problems) > 0
    if (has_problems) {
        if (delete_output && !force_delete_output)
          print("Test output will not be deleted because problems were detected.")
        if (make_new_ref)
          print("Test output will not be used as future reference because problems were detected.")

        fname_report <- paste0(format(Sys.time(), "%Y%m%d-%H%M"), "_", fname_report)
        print(paste("See problem report in file", shQuote(fname_report)))
        writeLines(unlist(problems), con = file.path(dir_test, fname_report))
    }

    made_new_refs <- if (make_new_ref && !has_problems) {
        all(sapply(dir_tests, function(test) make_test_output_reference(test)))
      } else FALSE

    deleted_output <- if (force_delete_output ||
                         (delete_output && !has_problems &&
                            (!make_new_ref || (make_new_ref && made_new_refs)))) {
        all(sapply(dir_tests, function(test) delete_test_output(test)))
      } else FALSE

  } else {
    has_problems <- made_new_refs <- deleted_output <- FALSE
  }

  c(has_run = has_run,
    has_problems = has_problems,
    made_new_refs = made_new_refs,
    deleted_output = deleted_output)
}


#' Copy output database of a test project to reference folder
#'
#' This function is called for its side effect of copying a file to the reference folder.
#'
#' @param dir_test A character string. Path to test project folder.
#' @param dir_ref A character string. Path to folder with reference database.
#' @param SWSF_version A character string. The version ID of the simulation framework as
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



#' Delete output of a test project
delete_test_output <- function(dir_test) {
		try(unlink(file.path(dir_test, list.files(dir_test, pattern = "last.dump"))), silent = TRUE)
		try(unlink(file.path(dir_test, list.files(dir_test, pattern = ".log"))), silent = TRUE)
		try(unlink(file.path(dir_test, ".Rapp.history")), silent = TRUE)
		try(unlink(file.path(dir_test, "1_Data_SWInput", "dbWeatherData_test.sqlite3")), silent = TRUE)
		try(unlink(file.path(dir_test, "1_Data_SWInput", "SWRuns_InputAll_PreProcessed.RData")), silent = TRUE)
		try(unlink(file.path(dir_test, "1_Data_SWInput", "swrun", ".Rapp.history")), silent = TRUE)
		try(unlink(file.path(dir_test, "3_Runs"), recursive = TRUE), silent = TRUE)
		try(unlink(file.path(dir_test, "4_Data_SWOutputAggregated"), recursive = TRUE), silent = TRUE)

  invisible(TRUE)
}



#' Compare test project output database with reference
#'
#' Reference database is identified by containing \code{basename(dir_test)} in the file name.
#'
#' @param dir_test A character string. Path to test project folder.
#' @param dir_ref A character string. Path to folder with reference database.
#' @param tol A numeric value. Differences smaller than tolerance are not reported.
#'  Passed to \link{\code{all.equal}}.
#' @param comp_absolute A logical value. If \code{TRUE} then absolute comparisons will be
#'  reported, otherwise relative differences.
#'  See argument \code{scale} of function \link{\code{all.equal}}.
#' @param verbose A logical value. If \code{TRUE} then messages are printed.
#'
#' @return A (possibly empty) list of character vectors describing differences between
#'  test and reference databases.
#'
#' @seealso \link{\code{all.equal}}
compare_test_output <- function(dir_test, dir_ref = NULL,
                                tol = 1e-3, comp_absolute = TRUE,
                                verbose = FALSE) {
  diff_msgs <- list()
  if (verbose)
    on.exit(print(diff_msgs))

	if (is.null(dir_ref))
		dir_ref <- file.path(dir_test, "..", "0_ReferenceOutput")

	#---Identify and connect to reference data base
	refs <- list.files(dir_ref, pattern = basename(dir_test))
	if (length(refs) == 0L) {
		diff_msgs <- c(diff_msgs,
		  paste(Sys.time(), "no reference database found for", shQuote(basename(dir_test))))
		return(diff_msgs)
	}
	refDB <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(dir_ref, refs[length(refs)]))

	#---Identify and connect to test data base
	ftemp_test <- file.path(dir_test, "4_Data_SWOutputAggregated", "dbTables.sqlite3")
	if (!file.exists(ftemp_test)) {
		diff_msgs <- c(diff_msgs,
		  paste(Sys.time(), "no test database found for", shQuote(basename(dir_test))))
		return(diff_msgs)
	}
	testDB <- RSQLite::dbConnect(RSQLite::SQLite(), ftemp_test)

	#---Identify set of shared tables
	refDB_tables <- RSQLite::dbListTables(refDB)
	testDB_tables <- RSQLite::dbListTables(testDB)
	tocomp_tables <- intersect(refDB_tables, testDB_tables)
	if (length(tocomp_tables) == 0L) {
		diff_msgs <- c(diff_msgs,
		  paste(Sys.time(), "test and reference database contain no shared tables"))
		return(diff_msgs)
	}

	testDB_tables_comp <- testDB_tables %in% tocomp_tables
	if (any(!testDB_tables_comp)) {
		diff_msgs <- c(diff_msgs,
	    paste("Test database contains tables without an analog in the reference database:",
	          paste(shQuote(testDB_tables[!testDB_tables_comp]), collapse = ", ")))
	}
	refDB_tables_comp <- refDB_tables %in% tocomp_tables
	if (any(!refDB_tables_comp)) {
		diff_msgs <- c(diff_msgs,
	    paste("Reference database contains tables without an analog in the test database:",
	          paste(shQuote(refDB_tables[!refDB_tables_comp]), collapse = ", ")))
	}

	#---Confirm that 'design' of test agrees with reference
	design_tables <- c("experimental_labels", "header", "run_labels", "runs",
		"scenario_labels", "simulation_years", "sites", "sqlite_sequence", "treatments",
		"weatherfolders")
	has_samedesign <- all(design_tables %in% tocomp_tables)
	if (has_samedesign) {
		has_samedesign <- all(sapply(design_tables, function(desT) {
			temp <- RSQLite::dbReadTable(refDB, desT)
			x_ref <- temp[do.call("order", unname(temp)), ]

			temp <- RSQLite::dbReadTable(testDB, desT)
			x_test <- temp[do.call("order", unname(temp)), ]

			identical(x_ref, x_test)
		}))
	}
	if (!has_samedesign) {
		diff_msgs <- c(diff_msgs,
		  paste(Sys.time(), "reference and test database have a different design and cannot be compared"))
		return(diff_msgs)
	}

	tocomp_tables <- tocomp_tables[!(tocomp_tables %in% design_tables)]


	#---Loop over shared result tables and compare shared fields
	for (k in seq_along(tocomp_tables)) {
    #---Identify set of shared fields
    refDB_fields <- RSQLite::dbListFields(refDB, tocomp_tables[k])
    testDB_fields <- RSQLite::dbListFields(testDB, tocomp_tables[k])
    tocomp_fields <- intersect(refDB_fields, testDB_fields)
    if (length(tocomp_fields) == 0L) {
		  diff_msgs <- c(diff_msgs,
        paste("Table", shQuote(tocomp_tables[k]),
              "contains no shared fields between the test and reference databases"))
      next
    }

    # Must have 'P_id' as first field
    if (!("P_id" %in% tocomp_fields)) {
		  diff_msgs <- c(diff_msgs,
        paste("The unique identifier 'P_id' is not a shared field in table",
              shQuote(tocomp_tables[k]), "preventing any comparison"))
      next
    }
    tocomp_fields <- c("P_id", tocomp_fields[!("P_id" == tocomp_fields)])

    # If field 'Soil_Layer' is present, then it must be shared and be the second field
    ref_has_sl <- any("Soil_Layer" %in% refDB_fields)
    test_has_sl <- any("Soil_Layer" %in% testDB_fields)
    if (xor(ref_has_sl, test_has_sl)) {
		  diff_msgs <- c(diff_msgs,
        paste("The soil layer identifier 'Soil_Layer' is not a shared field in table",
              shQuote(tocomp_tables[k]), "preventing any comparison"))
      next
    }
    if (ref_has_sl && test_has_sl) {
      tocomp_fields <- tocomp_fields[!("Soil_Layer" == tocomp_fields)]
      tocomp_fields <- c(tocomp_fields[1], "Soil_Layer",
        if (length(tocomp_fields) > 1) tocomp_fields[2:length(tocomp_fields)])
    }

    # Fields that are not shared
    testDB_fields_comp <- testDB_fields %in% tocomp_fields
    if (any(!testDB_fields_comp)) {
		  diff_msgs <- c(diff_msgs,
        paste("Test database table", shQuote(tocomp_tables[k]),
              "contains fields without an analog in the reference database:",
              paste(shQuote(testDB_fields[!testDB_fields_comp]), collapse = ", ")))
    }
    refDB_fields_comp <- refDB_fields %in% tocomp_fields
    if (any(!refDB_fields_comp)) {
		  diff_msgs <- c(diff_msgs,
        paste("Reference database table", shQuote(tocomp_tables[k]),
              "contains fields without an analog in the test database:",
              paste(shQuote(refDB_fields[!refDB_fields_comp]), collapse = ", ")))
    }

	  #---Extract field data, sorted by 'P_id' (and 'Soil_Layer')
	  sql <- paste0("SELECT ",
	    paste0("\"", tocomp_fields, "\"", collapse = ", "),
			" FROM ", tocomp_tables[k],
			" ORDER BY P_id",
			if (ref_has_sl && test_has_sl) ", Soil_Layer",
			";")

    x_ref <- RSQLite::dbGetQuery(refDB, sql)
    x_test <- RSQLite::dbGetQuery(testDB, sql)

	  #---Compare field data and report if differences were found
		ident <- all.equal(x_ref, x_test, tol = tol, scale = if (comp_absolute) 1 else NULL)
		if (!isTRUE(ident))
		  temp <- list(ident)
		  names(temp) <- tocomp_tables[k]
		  diff_msgs <- c(diff_msgs, temp)
	}

	diff_msgs
}
