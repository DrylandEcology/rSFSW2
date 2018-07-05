#' Run test projects
#'
#' @param dir_tests A vector of character strings. Paths to individual test projects.
#' @param dir_prj_tests A character string. Path to overall test projects folder.
#' @param dir_ref A character string. Path to folder with reference database.
#' @param dir_prev A character string. Paths to directory that should be set when function returns.
#' @param which_tests_torun An integer vector. Indices of \code{dir_tests} which will be
#'  carried out.
#' @param delete_output A logical value. If \code{TRUE} then output will be deleted unless
#'  problems showed up.
#' @param force_delete_output A logical value. If \code{TRUE} then output will be deleted
#'  even if problems showed up.
#' @param make_new_ref A logical value. If \code{TRUE} then output database will be saved
#'  as new reference.
#' @param write_report_to_disk A logical value. If \code{TRUE} then report of differences
#'  against reference databases is written to disk.
#'
#' @return A list with two elements: \describe{
#'  \item{res}{A data.frame where each row represents the outcomes of a test project.
#'    The columns return elapsed time in seconds \code{elapsed_s}, four logical values
#'    \code{has_run}, \code{has_problems}, \code{made_new_refs}, \code{deleted_output},
#'    and one character string \code{referenceDB} of the reference database name against
#'    which this run of the test project was compared.}
#'  \item{report}{A character vector describing differences between test and reference
#'    databases}
#' }
#' @export
run_test_projects <- function(dir_tests, dir_prj_tests = NULL, dir_ref = NULL,
  dir_prev = NULL, which_tests_torun = seq_along(dir_tests), delete_output = FALSE,
  force_delete_output = FALSE, make_new_ref = FALSE, write_report_to_disk = TRUE) {

  # Locate paths
  if (is.null(dir_prev)) {
    dir_prev <- getwd()
  }
  dir_prev <- normalizePath(dir_prev)

  if (is.null(dir_prj_tests)) {
    # Is parent of first test project the overall test projects folder?
    temp <- file.path(dir_tests[1], "..")
    if (dir.exists(temp)) {
      dir_prj_tests <- temp

    } else {
      # Is current working directory in a sub-folder of the overall test projects folder?
      temp <- strsplit(getwd(), .Platform$file.sep, fixed = TRUE)[[1]]
      temp <- do.call("file.path",
        args = as.list(temp[seq_len(which(temp == basename(dir_tests[1])) - 1L)]))
      if (dir.exists(temp)) {
        dir_prj_tests <- temp

      } else {
        stop("Overall test projects folder not found.")
      }
    }
  }
  dir_prj_tests <- normalizePath(dir_prj_tests)

  if (is.null(dir_ref)) {
    temp <- file.path(dir_prj_tests, "0_ReferenceOutput")
    if (dir.exists(temp)) {
      dir_ref <- temp

    } else {
      stop("Reference database folder not found.")
    }
  }
  dir_ref <- normalizePath(dir_ref)

  # Initialize
  op_prev <- options(c("warn", "error"))
  on.exit(setwd(dir_prev))
  on.exit(options(op_prev), add = TRUE)

  problems <- list()
  fname_report <- "Test_project_report.txt"
  vars <- c("elapsed_s", "has_run", "has_problems", "made_new_refs", "deleted_output")
  nruns <- length(which_tests_torun)

  res <- data.frame(matrix(FALSE, nrow = nruns, ncol = length(vars),
    dimnames = list(if (nruns > 0) paste0("Test", which_tests_torun), vars)),
    referenceDB = vector("character", nruns), stringsAsFactors = FALSE)

  # Loop over test projects
  if (nruns > 0 && all(which_tests_torun > 0)) {
    setwd(dir_prj_tests)
    dir_tests[which_tests_torun] <- normalizePath(dir_tests[which_tests_torun])

    for (k in seq_len(nruns)) {
      it <- which_tests_torun[k]
      setwd(dir_tests[it])

      print(paste0(Sys.time(), ": running test project '", basename(dir_tests[it]), "'"))
      files_to_delete <- NULL

      test_code <- list.files(dir_tests[it], pattern = "project_code")
      problems2 <- list()

      if (length(test_code) == 1L) {
        if (exists("SFSW2_prj_meta")) {
          rm("SFSW2_prj_meta", pos = utils::find("SFSW2_prj_meta"))
        }
        if (exists("fmeta")) {
          rm("fmeta", pos = utils::find("fmeta"))
        }

        ctime <- system.time(temp <- try(source(file.path(dir_tests[it], test_code),
          verbose = FALSE, chdir = FALSE)))

        if (!inherits(temp, "try-error")) {
          res[k, "has_run"] <- TRUE
          res[k, "elapsed_s"] <- ctime["elapsed"]
          comp <- compare_test_output(dir_test = dir_tests[it], dir_ref = dir_ref)

          if (length(comp) > 0) {
            res[k, "referenceDB"] <- comp[[1]]
          }

          if (length(comp) > 1) {
            problems2 <- c(problems2,
              paste("Problem list for test project", shQuote(basename(dir_tests[it])), ":"),
              comp[-1])
          }

        } else {
          problems2 <- c(problems2,
            paste("Source code for test project", shQuote(basename(dir_tests[it])), "unsuccessful."))
        }

        # Determine data input files to be deleted
        if (!exists("SFSW2_prj_meta")) {
          if (exists("fmeta") && file.exists(fmeta)) {
            SFSW2_prj_meta <- readRDS(fmeta)

          } else {
            fmeta <- file.path(dir_tests[it], "SFSW2_project_descriptions.rds")
            if (file.exists(fmeta)) {
              SFSW2_prj_meta <- readRDS(fmeta)
            }
          }
        }

        if (exists("SFSW2_prj_meta")) {
          temp <- NULL
          if (SFSW2_prj_meta[["exinfo"]][["ExtractSkyDataFromNOAAClimateAtlas_USA"]] ||
            SFSW2_prj_meta[["exinfo"]][["ExtractSkyDataFromNCEPCFSR_Global"]]) {

            temp <- c(temp, SFSW2_prj_meta[["fnames_in"]][["fclimnorm"]])
          }

          if (SFSW2_prj_meta[["exinfo"]][["ExtractSoilDataFromCONUSSOILFromSTATSGO_USA"]] ||
              SFSW2_prj_meta[["exinfo"]][["ExtractSoilDataFromGriddedGlobalFrom100m"]] ||
              SFSW2_prj_meta[["exinfo"]][["ExtractSoilDataFromISRICWISEv12_Global"]]) {

              temp <- c(temp, SFSW2_prj_meta[["fnames_in"]][["fslayers"]],
                SFSW2_prj_meta[["fnames_in"]][["fsoils"]])
          }

          if (!is.null(temp))
            print(paste("Will delete input files:", paste0(basename(temp),
              collapse = ", ")))

          files_to_delete <- temp
        }

      } else {
          problems2 <- c(problems2,
            paste("Source code for test project", shQuote(basename(dir_tests[it])), "not found."))
      }

      if (length(problems2) > 0) {
        res[k, "has_problems"] <- TRUE
        problems <- c(problems, problems2)
      }

      # Make reference output
      res[k, "made_new_refs"] <- if (make_new_ref && !(res[k, "has_problems"])) {
          make_test_output_reference(dir_tests[it])
        } else {
          FALSE
        }

      # Delete test project output
      do_delete <- force_delete_output ||
          (delete_output && !res[k, "has_problems"] &&
          (!make_new_ref || (make_new_ref && res[k, "made_new_refs"])))

      res[k, "deleted_output"] <- if (do_delete) {
          delete_test_output(dir_tests[it], delete_filepaths = files_to_delete)
        } else {
          FALSE
        }

    } # end of for-loop along 'which_tests_torun'
  } # end of if has tests to run

  # Write report of problems to disk file
  if (any(res[, "has_problems"])) {
    if (delete_output && !force_delete_output)
      print("Test output not be deleted because problems were detected.")
    if (make_new_ref)
      print("Test output not be used as future reference because problems were detected.")

    report <- rep(names(problems), times = lengths(problems))
    report <- paste0(report, ifelse(nchar(report) > 0, ": ", ""))
    report <- paste0(report, unlist(problems))

    if (write_report_to_disk) {
      fname_report <- paste0(format(Sys.time(), "%Y%m%d-%H%M"), "_", fname_report)
      print(paste("See problem report in file", shQuote(fname_report)))
      writeLines(report, con = file.path(dir_prj_tests, fname_report))
    }

  } else {
    report <- NULL
  }

  # Force delete if not already deleted (e.g., when delete all)
  its_delete <- if (nruns > 0) which(!res[, "deleted_output"]) else seq_along(dir_tests)

  if (force_delete_output && length(its_delete) > 0) for (k in its_delete) {

    # Delete designated files and folders: include files which the test project will re-create
    ftemp <- file.path(dir_tests[k], "1_Data_SWInput", "Test_referenceinputfiles_which_will_be_deleted")
    delete_filepaths <- if (dir.exists(ftemp)) {
        temp <- basename(list.files(ftemp))
        temp <- unlist(lapply(temp, function(x) list.files(dir_tests[k], pattern = x,
          full.names = TRUE, recursive = TRUE)))

        temp[!grepl(basename(ftemp), temp)]
      } else NULL

    res[k, "deleted_output"] <- delete_test_output(dir_tests[k], delete_filepaths)

    # Replace files with their initial state
    ftemp <- file.path(dir_tests[k], "1_Data_SWInput", "Test_referenceinputfiles_which_will_be_replaced")
    if (dir.exists(ftemp)) {
      init_files <- list.files(ftemp)

      for (f in init_files) {
        temp <- basename(f)
        temp <- unlist(lapply(temp, function(x) list.files(dir_tests[k], pattern = x,
          full.names = TRUE, recursive = TRUE)))
        files_to_replace <- temp[!grepl(basename(ftemp), temp)]

        sapply(files_to_replace, function(x)
          try(file.copy(from = file.path(ftemp, f), to = x, overwrite = TRUE,
            copy.mode = TRUE, copy.date = TRUE), silent = TRUE))
      }

    }

  }

  list(res = res, report = report)
}



#' Copy output database of a test project to reference folder
#'
#' This function is called for its side effect of copying a file to the reference folder.
#'
#' @param dir_test A character string. Path to test project folder.
#' @param dir_ref A character string. Path to folder with reference database.
#' @param SFSW2_version A character string. The version ID of the simulation framework as
#'  reported by the file \code{DESCRIPTION}.
#'
#' @return A logical value. \code{TRUE} if successful.
#' @export
make_test_output_reference <- function(dir_test, dir_ref = NULL, SFSW2_version = NULL) {

  if (is.null(SFSW2_version)) {
    SFSW2_version <- utils::packageVersion("rSFSW2")
  }

  if (is.null(dir_ref))
    dir_ref <- file.path(dir_test, "..", "0_ReferenceOutput")
  if (!file.exists(dir_ref))
    dir.create(dir_ref, recursive = TRUE, showWarnings = FALSE)

  fdb <- file.path(dir_test, "4_Data_SWOutputAggregated", "dbTables.sqlite3")
  if (file.exists(fdb)) {
    fdb_ref <- paste0("dbTables_", basename(dir_test), "_v", SFSW2_version, ".sqlite3")
    res <- file.rename(fdb, file.path(dir_ref, fdb_ref))

  } else {
    print(paste("Output DB of test project", shQuote(basename(dir_test)), "cannot be located"))
    res <- FALSE
  }

  res
}



#' Delete output of a test project
#'
#' @param dir_test A character string. Path to overall test project folder.
#' @param delete_filepaths A vector of character strings or \code{NULL}. Files to delete.
#'
#' @export
delete_test_output <- function(dir_test, delete_filepaths = NULL) {
  files_to_delete <- c(
    delete_filepaths,

    list.files(dir_test, pattern = "last.dump", recursive = TRUE, full.names = TRUE),
    list.files(dir_test, pattern = ".log", recursive = TRUE, full.names = TRUE),
    list.files(dir_test, pattern = ".Rapp.history", recursive = TRUE, full.names = TRUE),
    list.files(dir_test, pattern = ".Rhistory", recursive = TRUE, full.names = TRUE),
    list.files(dir_test, pattern = "_olog_cluster.txt", recursive = TRUE,
      full.names = TRUE),
    list.files(dir_test, pattern = "ClimDB_failedLocations_", recursive = TRUE,
      full.names = TRUE),
    list.files(dir_test, pattern = "backup", recursive = TRUE, full.names = TRUE),

    file.path(dir_test, "SFSW2_project_descriptions.rds"),
    file.path(dir_test, "1_Data_SWInput", "dbWeatherData_test.sqlite3"),
    file.path(dir_test, "1_Data_SWInput", "SWRuns_InputAll_PreProcessed.rds")
  )

  dirs_to_delete <- c(
    file.path(dir_test, "3_Runs"),
    file.path(dir_test, "4_Data_SWOutputAggregated"))

  try(unlink(unlist(files_to_delete)), silent = TRUE)
  try(unlink(unlist(dirs_to_delete), recursive = TRUE), silent = TRUE)

  invisible(TRUE)
}


#' Run checks on the values of the output database table \code{aggregation_overall_mean}
#'
#' The implemented water-balance checks correspond to unit tests of \itemize{
#'  \item SOILWAT2/test/test_WaterBalance.cc
#'  \item rSOILWAT2/tests/testthat/test_WaterBalance.R
#' }
#'
#' @param x A data.frame. The content of the table \code{aggregation_overall_mean}.
#'
#' @return If all checks pass, then \code{TRUE}. If at least one check fails, then a
#'  list of the failing checks where each element is the value of a call to
#'  \code{\link[base]{all.equal}} and its name describes the check.
#' @export
check_aggregated_output <- function(x) {
  checks_passed <- TRUE

  #--- Water balance checks
  # (1) AET <= PET
  temp <- with(x, all(AET_mm_mean <= PET_mm_mean))

  if (!isTRUE(temp)) {
    temp <- list("(1) AET <= PET" = temp)
    checks_passed <- if (is.list(checks_passed)) c(checks_passed, temp) else temp
  }


  # (2) AET == E(total) + T(total)
  temp <- with(x, all.equal(AET_mm_mean, Transpiration_Total_mm_mean +
    Evaporation_Total_mm_mean))

  if (!isTRUE(temp)) {
    temp <- list("AET == Ttotal + Etotal" = temp)
    checks_passed <- if (is.list(checks_passed)) c(checks_passed, temp) else temp
  }


  # (3) T(total) = sum of T(veg-type i from soil layer j)
  temp <- with(x, all.equal(Transpiration_Total_mm_mean, Transpiration_topLayers_mm_mean +
    Transpiration_bottomLayers_mm_mean))

  if (!isTRUE(temp)) {
    temp <- list("Total == sum of T(veg-type i from soil layer j)" = temp)
    checks_passed <- if (is.list(checks_passed)) c(checks_passed, temp) else temp
  }


  # (4) E(total) = E(total bare-soil) + E(ponded water) + E(total litter-intercepted) +
  #            + E(total veg-intercepted) + E(snow sublimation)
  temp <- with(x, all.equal(Evaporation_Total_mm_mean, Evaporation_Soil_Total_mm_mean +
    Evaporation_SurfaceWater_mm_mean + Evaporation_InterceptedByVegetation_mm_mean +
    Evaporation_InterceptedByLitter_mm_mean + Snowloss_mm_mean))

  if (!isTRUE(temp)) {
    temp <- list("Etotal == Esoil + Eponded + Eveg + Elitter + Esnow" = temp)
    checks_passed <- if (is.list(checks_passed)) c(checks_passed, temp) else temp
  }


  # (6) infiltration = [rain + snowmelt + runon] - (runoff + intercepted + delta_surfaceWater + Eponded)

  # ==> we currently cannot implement check (6) because we don't have output for
  # delta_surfaceWater


  # (7) E(soil) + Ttotal = infiltration - (deepDrainage + delta(swc))
  temp <- with(x, all.equal(Evaporation_Soil_Total_mm_mean + Transpiration_Total_mm_mean,
    Infiltration_mm_mean - (DeepDrainage_mm_mean + SWC_StorageChange_mm_mean)))

  if (!isTRUE(temp)) {
    temp <- list("Esoil + Ttotal == infiltration - (deepDrainage + delta(swc))" = temp)
    checks_passed <- if (is.list(checks_passed)) c(checks_passed, temp) else temp
  }


  # (8) for every soil layer j: delta(swc) =
  #   = infiltration/percolationIn + hydraulicRedistribution -
  #     (percolationOut/deepDrainage + transpiration + evaporation)

  # ==> we currently cannot implement check (8) because we don't have output delta(swc)
  # separately for top and bottom layers

  checks_passed
}



#' Compare test project output database with reference
#'
#' Reference database is identified by containing \code{basename(dir_test)} in the file name.
#'
#' @param dir_test A character string. Path to test project folder.
#' @param dir_ref A character string. Path to folder with reference database.
#' @param tol A numeric value. Differences smaller than tolerance are not reported.
#'  Passed to \code{\link{all.equal}}.
#' @param comp_absolute A logical value. If \code{TRUE} then absolute comparisons will be
#'  reported, otherwise relative differences.
#'  See argument \code{scale} of function \code{\link{all.equal}}.
#' @param verbose A logical value. If \code{TRUE} then messages are printed.
#'
#' @return A (possibly empty) list of character vectors describing differences between
#'  test and reference databases. A first entry is the file name of the reference database.
#'
#' @seealso \code{\link{all.equal}}
#' @export
compare_test_output <- function(dir_test, dir_ref = NULL, tol = 1e-3,
  comp_absolute = TRUE, verbose = FALSE) {

  diff_msgs <- list()
  if (verbose)
    on.exit(print(diff_msgs[-1]))

  if (is.null(dir_ref))
    dir_ref <- file.path(dir_test, "..", "0_ReferenceOutput")

  #---Identify and connect to reference data base
  fname_refDB <- list.files(dir_ref, pattern = basename(dir_test))
  if (length(fname_refDB) == 0L) {
    diff_msgs <- c(diff_msgs, "",
      paste(Sys.time(), "no reference database found for", shQuote(basename(dir_test))))
    return(diff_msgs)

  } else {
    if (length(fname_refDB) > 1) {
      # Identify latest version
      temp <- strsplit(fname_refDB, split = "_")
      temp <- sapply(temp, function(x) strsplit(x[length(x)], split = ".", fixed = TRUE))
      v_refDB <- lapply(temp, function(x) numeric_version(paste(sub("v", "", x[-length(x)]),
        collapse = ".")))
      v_latest <- 1
      for (k in seq_along(v_refDB)[-1]) {
        if (v_refDB[[v_latest]] < v_refDB[[k]])
          v_latest <- k
      }

      fname_refDB <- fname_refDB[v_latest]
    }

    diff_msgs <- c(diff_msgs, fname_refDB)
  }
  refDB <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(dir_ref, fname_refDB))

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
  has_samedesign <- dbOutput_ListDesignTables() %in% tocomp_tables
  diff_design <- NULL

  if (all(has_samedesign)) {
    diff_design <- sapply(dbOutput_ListDesignTables(), function(desT) {
      temp <- RSQLite::dbReadTable(refDB, desT)
      x_ref <- temp[do.call("order", unname(temp)), ]

      temp <- RSQLite::dbReadTable(testDB, desT)
      x_test <- temp[do.call("order", unname(temp)), ]

      all.equal(x_ref, x_test)
    })

    has_samedesign <- sapply(diff_design, function(x) is.logical(x) && isTRUE(x))
  }

  if (any(!has_samedesign)) {
    diff_msgs <- c(diff_msgs,
      paste(Sys.time(), "reference and test database have a different design"),
      if (!is.null(diff_design)) diff_design[!has_samedesign] else NULL)
  }

  tocomp_tables <- tocomp_tables[!(tocomp_tables %in% dbOutput_ListDesignTables())]


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
    if (!isTRUE(ident)) {
      temp <- list(ident)
      names(temp) <- tocomp_tables[k]
      diff_msgs <- c(diff_msgs, temp)
    }

    #---Run additional checks on aggregated output
    if (tocomp_tables[k] == "aggregation_overall_mean") {
      temp <- check_aggregated_output(x_test)
      if (!isTRUE(temp)) {
        diff_msgs <- c(diff_msgs, temp)
      }
    }
  }

  diff_msgs
}
