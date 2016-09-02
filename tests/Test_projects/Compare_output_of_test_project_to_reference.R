#' Compare test project output database with reference
#'
#' Reference database is identified by containing \code{basename(dir_test)} in the file name.
#'
#' @param dir_test A character vector. Path to test project folder.
#' @param dir_ref A character vector. Path to folder with reference database.
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
