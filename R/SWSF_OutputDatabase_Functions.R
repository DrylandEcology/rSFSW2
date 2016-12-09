#--------------------------------------------------------------------------------------------------#

#------CODE developed and written by
# - Daniel R Schlaepfer (dschlaep@uwyo.edu, drs): 2009-2016
#for contact and further information see also: sites.google.com/site/drschlaepfer

#------DISCLAIMER: This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#--------------------------------------------------------------------------------------------------#

missing_Pids_outputDB <- compiler::cmpfun(function(Table, dbname) {
  mP_ids <- -1L

  if (file.exists(dbname)) {
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbname, flags = RSQLite::SQLITE_RO)

    temp <- DBI::dbListTables(con)

    if (any("header" == temp) && any(Table == temp)) {
      sql <- paste0("SELECT header.P_id FROM header LEFT JOIN ", Table, " ON (header.P_id=",
        Table, ".P_id) WHERE ", Table, ".P_id is NULL AND header.Include_YN = 1 ",
        "ORDER BY header.P_id")
      mP_ids <- RSQLite::dbGetQuery(con, sql)[, "header.P_id"]
    }

    DBI::dbDisconnect(con)
  }

  mP_ids
})

headerTables <- function() c("runs", "sqlite_sequence", "header", "run_labels",
  "scenario_labels", "sites", "experimental_labels", "treatments", "simulation_years",
  "weatherfolders")



# PRAGMA, see http://www.sqlite.org/pragma.html
PRAGMA_settings1 <- function() c("PRAGMA cache_size = 400000;",
            "PRAGMA synchronous = 1;",
            "PRAGMA locking_mode = EXCLUSIVE;",
            "PRAGMA temp_store = MEMORY;",
            "PRAGMA auto_vacuum = NONE;")
PRAGMA_settings2 <- function() c(PRAGMA_settings1(),
            "PRAGMA page_size=65536;", # no return value
            "PRAGMA max_page_count=2147483646;", # returns the maximum page count
            "PRAGMA foreign_keys = ON;") #no return value

set_PRAGMAs <- compiler::cmpfun(function(con, settings) {
  temp <- lapply(force(settings), function(x) RSQLite::dbGetQuery(con, x))
  invisible(0)
})

getSiteIds <- compiler::cmpfun(function(con, folderNames) {
  wf_ids <- RSQLite::dbGetQuery(con, "SELECT id, folder FROM weatherfolders")
  wf_ids[match(folderNames, wf_ids[, "folder"], nomatch = NA), "id"]
})

local_weatherDirName <- compiler::cmpfun(function(i_sim, scN, runN, runIDs, name.OutputDB) {	# Get name of weather file from output database
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = name.OutputDB, flags = RSQLite::SQLITE_RO)
  temp <- DBI::dbGetQuery(con, paste("SELECT WeatherFolder FROM header WHERE P_id=", it_Pid(i_sim, 1, scN, runN, runIDs)))[1,1]
  DBI::dbDisconnect(con)
  temp
})




#---File names
maker.climateScenarios <- function(currentScenario = "Current",
  ensembleScenarios = c("RCP45", "RCP85"), ensembleLevels = c(2, 8, 15)) {

  climCat <- data.frame(matrix(NA,
              nrow = 1 + length(ensembleScenarios) * length(ensembleLevels),
              ncol = 2,
            dimnames = list(NULL, c("Family", "Rank"))))
  climCat[1, 1] <- currentScenario
  climCat[-1, 1] <- rep(ensembleScenarios, each = length(ensembleLevels))
  climCat[-1, 2] <- rep(ensembleLevels, times = length(ensembleScenarios))
  rownames(climCat) <- apply(climCat, 1, function(r)
              paste0(gsub(" ", "", na.exclude(r)), collapse = "_rank"))

  climCat
}


#---Database functions
#List tables and variables of a database
list.dbTables <- function(dbName) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbName, flags = RSQLite::SQLITE_RO)
  res <- DBI::dbListTables(con)
  RSQLite::dbDisconnect(con)

  res
}

list.dbVariables <- function(dbName, dbTable) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbName, flags = RSQLite::SQLITE_RO)
  res <- DBI::dbListFields(con, dbTable)
  RSQLite::dbDisconnect(con)

  res
}

list.dbVariablesOfAllTables <- function(dbName) {
  tables <- list.dbTables(dbName)
  sapply(tables, function(it) list.dbVariables(dbName, dbTable = it))
}

addHeaderToWhereClause <- function(whereClause, headers = NULL, fdbSWSF = NULL) {
  if (is.null(headers) && file.exists(fdbSWSF)) {
    con <- RSQLite::dbConnect(RSQLite::SQLite(), fdbSWSF, flags = RSQLite::SQLITE_RO)
    headers <- DBI::dbListFields(con, name = "header")
    RSQLite::dbDisconnect(con)
  }

  temp1 <- res <- strsplit(whereClause, split = " ", fixed = TRUE)[[1]]	#Locate all "Label = 'x'"
  temp1F <- strsplit(temp1, split = "=", fixed = TRUE)
  ielem <- grepl("=", temp1) &
       !grepl("header.", temp1, fixed = TRUE) &
       sapply(temp1F, function(ch) ch[1] %in% headers)
  temp2 <- temp1[ielem]
  if (length(temp2) > 0)
    res[ielem] <- paste0("header.", temp2) #add 'header.'

  paste(res, collapse = " ")
}

#Access data from a database
get_fieldnames <- function(responseName, fields.header, fields.iTable) {
  outOrder <- iColumns.iTable <- iColumns.header <- NULL

  if ("P_id" %in% responseName) {
    addPid <- TRUE
    responseName <- responseName[!(responseName == "P_id")]
  } else {
    addPid <- FALSE
  }

  if (length(responseName) > 0) {
    fields.header_ <- gsub(".", "_", fields.header, fixed = TRUE)
    fields.iTable_ <- gsub(".", "_", fields.iTable, fixed = TRUE)
    responseName <- gsub(".", "_", responseName, fixed = TRUE)

    for (i in seq_along(responseName)) {
      iColumns.iTable <- c(iColumns.iTable,
        fields.iTable[grepl(responseName[i], fields.iTable_, fixed = FALSE)])
      iColumns.header <- c(iColumns.header,
        fields.header[grepl(responseName[i], fields.header_, fixed = FALSE)])
      outOrder <- c(outOrder,
        fields.iTable[grepl(responseName[i], fields.iTable_, fixed = FALSE)],
        fields.header[grepl(responseName[i], fields.header_, fixed = FALSE)])
    }
    iColumns.iTable <- unique(iColumns.iTable)
    iColumns.header <- unique(iColumns.header)
    outOrder <- unique(outOrder)
  }

  list(addPid = addPid,
     iTable = iColumns.iTable,
     header = iColumns.header,
     outOrder = outOrder,
     has_columns = length(iColumns.header) > 0 || length(iColumns.iTable) > 0)
}



#Get data of variables in the overall aggregation table for one of the scenarios
get.SeveralOverallVariables_Scenario <- function(fdbSWSF, responseName, MeanOrSD = "Mean",
  scenario = "Current", whereClause = NULL) {

  dat <- NULL
  iColumns <- list()

  if (length(responseName) > 0) {
    con <- RSQLite::dbConnect(RSQLite::SQLite(), fdbSWSF, flags = RSQLite::SQLITE_RO)
    iTable <- DBI::dbListTables(con)
    iTable <- grep(paste0("Overall_", MeanOrSD), iTable, ignore.case = TRUE,
      fixed = FALSE, value = TRUE)

    if (length(iTable) == 1) {
      iColumns <- get_fieldnames(responseName,
        fields.header = DBI::dbListFields(con, "header"),
        fields.iTable = DBI::dbListFields(con, iTable))

      if (iColumns[["has_columns"]] || iColumns[["addPid"]]) {
        sql <- paste0("SELECT ",
          if (iColumns[["addPid"]])
            "header.P_id AS P_id",
          if (iColumns[["addPid"]] && iColumns[["has_columns"]])
            ", ",
          if (length(iColumns[["header"]]) > 0)
            paste0("\"", iColumns[["header"]], "\"", collapse = ", "),
          if (length(iColumns[["header"]]) > 0 && length(iColumns[["iTable"]]) > 0)
            ", ",
          if (length(iColumns[["iTable"]]) > 0)
            paste0("\"", iColumns[["iTable"]], "\"", collapse = ", "),
          " FROM ", iTable,
          " INNER JOIN header ON ", iTable, ".P_id = header.P_id",
          " WHERE header.Scenario = ", shQuote(scenario),
          if (length(whereClause) > 0)
            paste0(" AND ", addHeaderToWhereClause(whereClause, fdbSWSF = fdbSWSF)),
          " ORDER BY header.P_id;")

        dat <- RSQLite::dbGetQuery(con, sql)
      }
    }

    RSQLite::dbDisconnect(con)
  }

  dat[, iColumns[["outOrder"]]]
}

#Get data of variables in the overall aggregation table for one of the ensembles
get.SeveralOverallVariables_Ensemble <- function(fdbSWSF, fdbSWSFens, responseName,
  MeanOrSD = "Mean", fam, level, whereClause = NULL) {

  dat <- NULL
  iColumns <- list()

  if (length(responseName) > 0) {
    con <- RSQLite::dbConnect(RSQLite::SQLite())
    temp_fdbSWSFens <- grep("Overall", fdbSWSFens, ignore.case = TRUE, value = TRUE)
    RSQLite::dbGetQuery(con, paste0("ATTACH ", shQuote(temp_fdbSWSFens), " AS X;"))
    RSQLite::dbGetQuery(con, paste0("ATTACH ", shQuote(fdbSWSF), " AS Y;"))
    temp <- unlist(RSQLite::dbGetQuery(con, "SELECT name FROM X.sqlite_master WHERE type = 'table';"))
    iTable <- temp[grepl(fam, temp, ignore.case = TRUE) &
           grepl(paste0("rank_", formatC(level, format = "d", flag = "0", width = 2)), temp) &
           grepl(paste0("_", MeanOrSD), temp, ignore.case = TRUE)]

    if (length(iTable) == 1) {
      iColumns <- get_fieldnames(responseName,
        fields.header = RSQLite::dbGetQuery(con, paste0("PRAGMA Y.table_info(header);"))$name,
        fields.iTable = RSQLite::dbGetQuery(con, paste0("PRAGMA X.table_info(", iTable, ");"))$name)

      if (iColumns[["has_columns"]] || iColumns[["addPid"]]) {
        sql <- paste0("SELECT ",
          if (iColumns[["addPid"]])
            "Y.header.P_id AS P_id",
          if (iColumns[["addPid"]] && iColumns[["has_columns"]])
            ", ",
          if (length(iColumns[["header"]]) > 0)
            paste0("\"", iColumns[["header"]], "\"", collapse = ", "),
          if (length(iColumns[["header"]]) > 0 && length(iColumns[["iTable"]]) > 0)
            ", ",
          if (length(iColumns[["iTable"]]) > 0)
            paste0("\"", iColumns[["iTable"]], "\"", collapse = ", "),
          " FROM X.", iTable,
          " INNER JOIN Y.header ON X.", iTable, ".P_id = Y.header.P_id",
          if (length(whereClause) > 0)
            paste0(" WHERE ", addHeaderToWhereClause(whereClause, fdbSWSF = fdbSWSF)),
          " ORDER BY Y.header.P_id;")

        dat <- RSQLite::dbGetQuery(con, sql)
      }
    }
    RSQLite::dbDisconnect(con)
  }

  dat[, iColumns[["outOrder"]]]
}

#Get data of variables in the overall aggregation table for one of the climCat rows (combining 'Current' and ensembles)
get.SeveralOverallVariables <- function(fdbSWSF, fdbSWSFens, climCat, responseName,
  MeanOrSD = "Mean", i_climCat = 1, whereClause = NULL) {

  if (length(responseName) > 0 && i_climCat <= nrow(climCat)) {
    dat <- if (climCat[i_climCat, 1] == "Current") {
          get.SeveralOverallVariables_Scenario(
            fdbSWSF = fdbSWSF,
            responseName = responseName,
            MeanOrSD = MeanOrSD,
            scenario = climCat[i_climCat, 1],
            whereClause = whereClause)
        } else {
          get.SeveralOverallVariables_Ensemble(
            fdbSWSF = fdbSWSF, fdbSWSFens = fdbSWSFens,
            responseName = responseName,
            MeanOrSD = MeanOrSD,
            fam = climCat[i_climCat, 1],
            level = climCat[i_climCat, 2],
            whereClause = whereClause)
        }

    if (!is.null(dat) && ncol(dat) == 1) {
      as.vector(dat[, 1])
    } else {
      dat
    }
  } else {
    NULL
  }
}

#Get header and data for an entire table for one of the scenarios
get.Table_Scenario <- function(fdbSWSF, responseName, MeanOrSD = "Mean",
  scenario = "Current", whereClause = NULL, header = FALSE) {

  dat <- NULL
  if (length(responseName) > 0) {
    con <- RSQLite::dbConnect(RSQLite::SQLite(), fdbSWSF, flags = RSQLite::SQLITE_RO)
    temp1 <- DBI::dbListTables(con)
    temp2 <- grepl(pattern = paste0(responseName, "_", MeanOrSD), x = temp1,
      ignore.case = TRUE, fixed = FALSE)
    iTable <- temp1[temp2]

    if (length(iTable) == 1) {
      fields <- DBI::dbListFields(con, iTable)[-1]

      sql <- paste0("SELECT ",
        if (header)
          "header. * , ",
        paste0("\"", fields, "\"", collapse = ", "),
        " FROM ", iTable, " INNER JOIN header ON ", iTable, ".P_id = header.P_id",
        " WHERE header.Scenario = ", shQuote(scenario),
        if (length(whereClause) > 0)
          paste0(" AND ", whereClause),
        " ORDER BY header.P_id;")

      dat <- RSQLite::dbGetQuery(con, sql)
    }
    RSQLite::dbDisconnect(con)
  }

  dat
}

#Get header and data for an entire table for one of the ensembles
get.Table_Ensemble <- function(fdbSWSF, fdbSWSFens, responseName, MeanOrSD = "Mean",
  fam, level, whereClause = NULL, header = FALSE) {

  dat <- NULL
  if (length(responseName) > 0) {
    con <- RSQLite::dbConnect(RSQLite::SQLite())
    temp_fdbSWSFens <- fdbSWSFens[grepl(pattern = paste0("_", responseName),
      x = fdbSWSFens, ignore.case = TRUE)]
    RSQLite::dbGetQuery(con, paste0("ATTACH ", shQuote(temp_fdbSWSFens), " AS X;"))
    RSQLite::dbGetQuery(con, paste0("ATTACH ", shQuote(fdbSWSF), " AS Y;"))
    temp <- unlist(RSQLite::dbGetQuery(con, "SELECT name FROM X.sqlite_master WHERE type = 'table';"))
    iTable <- temp[grepl(pattern = fam, x = temp, ignore.case = T) & grepl(pattern = paste0("rank_", formatC(level, format = "d", flag = "0", width = 2)), x = temp) & grepl(pattern = MeanOrSD, x = temp, ignore.case = T)]
    if (length(iTable) == 1) {
      column_names_iTable<-RSQLite::dbGetQuery(con, paste("PRAGMA X.table_info(",iTable,");",sep = ""))$name
      column_names_iTable<-column_names_iTable[-1]#Remove P_id
      column_names_header<-RSQLite::dbGetQuery(con, "PRAGMA Y.table_info(header);")$name
      column_names_header<-column_names_header[-1]#Remove P_id
      column_names_header<-column_names_header[-length(column_names_header)]#Remove Scenario
      if ("Soil_Layer" %in% column_names_iTable) {
        column_names_iTable<-column_names_iTable[-1] #Remove Soil_Layer
        temp<-paste0(paste0("\"", column_names_header, "\"",sep = ""), collapse = ", ")
        sql<-paste("SELECT ", if (header) "Y.header.P_id AS P_id, ", "Soil_Layer, ", if (header) temp,", ",paste0(paste0("\"", column_names_iTable, "\"",sep = ""), collapse = ", "),sep = "")
      } else {
        sql<-paste("SELECT ", if (header) "Y.header. * , ",paste0(paste0("\"", column_names_iTable, "\"",sep = ""), collapse = ", "),sep = "")
      }
      if (length(whereClause) > 0) {
        sql <- paste0(sql," FROM X.", iTable, " INNER JOIN Y.header ON X.",iTable,".P_id = Y.header.P_id WHERE ", whereClause, " ORDER BY Y.header.P_id;",sep = "")
        dat <- RSQLite::dbGetQuery(con, sql)
      } else {
        sql <- paste0(sql," FROM X.", iTable, " INNER JOIN Y.header ON X.",iTable,".P_id = Y.header.P_id ORDER BY Y.header.P_id;",sep = "")
        dat <- RSQLite::dbGetQuery(con, sql)
      }
    }
    RSQLite::dbDisconnect(con)
  }

  dat
}

#Get data-part for an entire table for one of the climCat rows (combining 'Current' and ensembles)
get.Table <- function(fdbSWSF, fdbSWSFens, climCat, responseName, MeanOrSD = "Mean",
  i_climCat = 1, whereClause = NULL, addPid = FALSE) {

  if (length(responseName) > 0 && i_climCat <= nrow(climCat)) {
    #print(paste(paste(responseName,collapse = ", "), MeanOrSD, i_climCat, whereClause, addPid, sep = " "))
    if (climCat[i_climCat, 1] == "Current") {
      scenario<-climCat[i_climCat, 1]
      con <- RSQLite::dbConnect(RSQLite::SQLite(), fdbSWSF, flags = RSQLite::SQLITE_RO)
      iTable <- (temp <- DBI::dbListTables(con))[grepl(pattern = paste0(responseName, "_", MeanOrSD), x = temp, ignore.case = TRUE, fixed = FALSE)]
      if (length(iTable) == 1) {
        fields <- DBI::dbListFields(con, iTable)
        fields<-fields[-1]
        if (length(whereClause) > 0) {
          sql <- paste0("SELECT ", if (addPid) paste("header.P_id AS P_id, ",sep = ""), paste0(paste0("\"", fields, "\"",sep = ""), collapse = ", ") ," FROM ", iTable, " INNER JOIN header ON ",iTable,".P_id = header.P_id WHERE header.Scenario = ", shQuote(scenario), " AND ", addHeaderToWhereClause(whereClause, fdbSWSF = fdbSWSF), " ORDER BY header.P_id;")
        } else {
          sql <- paste0("SELECT ", if (addPid) paste("header.P_id AS P_id, ",sep = ""), paste0(paste0("\"", fields, "\"",sep = ""), collapse = ", ") ," FROM ", iTable, " INNER JOIN header ON ",iTable,".P_id = header.P_id WHERE header.Scenario = ", shQuote(scenario), " ORDER BY header.P_id;")
        }
        dat <- RSQLite::dbGetQuery(con, sql)
      }
      RSQLite::dbDisconnect(con)

    } else {
      fam <- climCat[i_climCat, 1]
      level <- climCat[i_climCat, 2]
      con <- RSQLite::dbConnect(RSQLite::SQLite())
      temp_fdbSWSFens <- fdbSWSFens[grepl(pattern = paste0("_", responseName),
        x = fdbSWSFens, ignore.case = TRUE)]
      RSQLite::dbGetQuery(con, paste0("ATTACH ", shQuote(temp_fdbSWSFens), " AS X;"))
      RSQLite::dbGetQuery(con, paste0("ATTACH ", shQuote(fdbSWSF), " AS Y;"))
      temp <- unlist(RSQLite::dbGetQuery(con, "SELECT name FROM X.sqlite_master WHERE type = 'table';"))
      iTable <- temp[grepl(pattern = fam, x = temp, ignore.case = T) & grepl(pattern = paste0("rank_", formatC(level, format = "d", flag = "0", width = 2)), x = temp) & grepl(pattern = MeanOrSD, x = temp, ignore.case = T)]
      if (length(iTable) == 1) {
        fields <- RSQLite::dbGetQuery(con, paste("PRAGMA X.table_info(",iTable,");",sep = ""))$name
        fields <- fields[-1]
        if (length(whereClause) > 0) {
          sql <- paste0("SELECT ", if (addPid) paste("Y.header.P_id AS P_id, ",sep = ""), paste0(paste0("\"", fields, "\"",sep = ""), collapse = ", ")," FROM X.", iTable, " INNER JOIN Y.header ON X.",iTable,".P_id = Y.header.P_id WHERE ", addHeaderToWhereClause(whereClause, fdbSWSF = fdbSWSF), " ORDER BY Y.header.P_id;",sep = "")
          dat <- RSQLite::dbGetQuery(con, sql)
        } else {
          sql <- paste0("SELECT ", if (addPid) paste("X.",iTable,".P_id AS P_id, ",sep = ""), paste0(paste0("\"", fields, "\"",sep = ""), collapse = ", ")," FROM X.", iTable, " ORDER BY P_id;",sep = "")
          dat <- RSQLite::dbGetQuery(con, sql)
        }
      }
      RSQLite::dbDisconnect(con)
    }
  } else {
    dat <- NULL
  }

  dat
}


move_temporary_to_outputDB <- function(dir.out.temp, name.OutputDB,
  name.OutputDBCurrent = NULL, t.overall = Sys.time(), opt_comp_time = NULL,
  do_DBCurrent = FALSE, cleanDB = FALSE, deleteTmpSQLFiles = FALSE, continueAfterAbort = TRUE,
  print.debug = FALSE, verbose = FALSE) {

  t1 <- Sys.time()
  if (verbose)
    print(paste("Inserting data from temp SQL files into output DB: started at", t1))

  #concatenate file keeps track of sql files inserted into data
  concatFile <- "sqlFilesInserted.csv"

  # Locate temporary SQL files
  theFileList <- list.files(path = dir.out.temp, pattern = "SQL",
    full.names = FALSE, recursive = TRUE, include.dirs = FALSE, ignore.case = FALSE)

  # remove any already inserted files from list
  if (!deleteTmpSQLFiles && continueAfterAbort) {
    temp <- file.path(dir.out.temp, concatFile)
    completedFiles <- if (file.exists(temp)) {
        basename(readLines(temp))
      } else {
        character(0)
      }
    temp <- which(theFileList == completedFiles)
    if (length(temp) > 0) {
      theFileList <- theFileList[-temp]
    }
  }

  if (length(theFileList) > 0) {
    #Connect to the Database
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = name.OutputDB)

    reset_DBCurrent <- do_DBCurrent && (cleanDB || !file.exists(name.OutputDBCurrent))
    if (reset_DBCurrent)
      file.copy(from = name.OutputDB, to = name.OutputDBCurrent)
    if (do_DBCurrent)
      con2 <- DBI::dbConnect(RSQLite::SQLite(), dbname = name.OutputDBCurrent)
    if (reset_DBCurrent)
      DBI::dbGetQuery(con2, "DELETE FROM runs WHERE scenario_id != 1;") # DROP ALL ROWS THAT ARE NOT CURRENT FROM HEADER
    out_tables <- DBI::dbListTables(con)
    out_tables_aggr <- grep("aggregation_", out_tables, value = TRUE)

    # Prepare output databases
    set_PRAGMAs(con, PRAGMA_settings1())
    if (do_DBCurrent) set_PRAGMAs(con2, PRAGMA_settings1())

    # Check what has already been inserted in each tables
    pids_inserted <- lapply(out_tables_aggr, function(agg_table)
      DBI::dbGetQuery(con, paste0("SELECT P_id FROM ", agg_table, ";"))[, 1])
    names(pids_inserted) <- out_tables_aggr

    if (do_DBCurrent) {
      pids2_inserted <- lapply(out_tables_aggr, function(agg_table)
        DBI::dbGetQuery(con2, paste0("SELECT P_id FROM ", agg_table, ";"))[, 1])
      names(pids2_inserted) <- out_tables_aggr
    }

    # Add data to SQL databases
    for (j in seq_along(theFileList)) {
      tDB1 <- Sys.time()
      has_time_to_concat <- (difftime(tDB1, t.overall, units = "secs") +
        opt_comp_time[["one_concat_s"]]) < opt_comp_time[["wall_time_s"]]
      if (!has_time_to_concat)
        break

      OK_tempfile <- TRUE
      # Read SQL statements from temporary file
      sql_cmds <- readLines(file.path(dir.out.temp, theFileList[j]))
      add_to_DBCurrent <- do_DBCurrent && grepl("SQL_Current", theFileList[j])

      if (verbose)
        print(paste("Adding", shQuote(theFileList[j]), "with", length(sql_cmds), "lines",
          "to output DB: started at ", tDB1))

      # Send SQL statements to database
      OK_tempfile <- OK_tempfile && !inherits(try(DBI::dbBegin(con)), "try-error")
      if (add_to_DBCurrent)
        OK_tempfile <- OK_tempfile && !inherits(try(DBI::dbBegin(con2)), "try-error")

      notOK_lines <- NULL

      if (OK_tempfile) for (k in seq_along(sql_cmds)) {
        OK_line <- TRUE

        # Determine P_id
        id_start <- as.integer(regexpr(" VALUES (", sql_cmds[k], fixed = TRUE))
        id_end <- as.integer(regexpr(",", sql_cmds[k], fixed = TRUE))
        if (id_end < 0)
          id_end <- as.integer(regexpr(")", sql_cmds[k], fixed = TRUE))

        if (any(id_start < 1, id_end <= id_start)) {
          print(paste0("P_id not located in file ", shQuote(theFileList[j]), " on line ",
            k, ": ", substr(sql_cmds[k], 1, 100)))
          next
        }

        id <- as.integer(substr(sql_cmds[k], 9 + id_start, -1 + id_end))
        OK_line <- OK_line && is.finite(id)

        # Determine table
        id_table <- as.integer(gregexpr('\"', sql_cmds[k], fixed = TRUE)[[1]])

        if (any(id_table[1] < 1, id_table[2] <= id_table[1])) {
          print(paste0("Name of table not located in file ", shQuote(theFileList[j]),
            " on line ", k, ": ", substr(sql_cmds[k], 1, 100)))
          next
        }

        table_name <- substr(sql_cmds[k], 1 + id_table[1], -1 + id_table[2])
        OK_line <- OK_line && any(table_name == out_tables_aggr)

        # Check if P_id already in output DB
        OK_line1 <- id %in% pids_inserted[[table_name]]
        OK_line2 <- if (add_to_DBCurrent) id %in% pids2_inserted[[table_name]] else FALSE

        # If P_id already in output DB, then check whether data agree
        OK_agree1 <- FALSE
        if (OK_line1) {
          id_data_DB <- DBI::dbGetQuery(con, paste0("SELECT * FROM \"", table_name,
            "\" WHERE P_id = ", id))

          tmp_data <- substr(sql_cmds[k], 9 + id_start, nchar(sql_cmds[k]))
          repeat {
            nt <- nchar(tmp_data)
            if (nt <= 1 || substr(tmp_data, nt, nt) == ")") break
            tmp_data <- substr(tmp_data, 1, nt - 1)
          }
          if (nt > 1 ) {
            tmp_data <- paste0("c(", tmp_data)
            tmp_data <- gsub("NULL", "NA", tmp_data)
            tmp_data <- eval(parse(text = tmp_data, keep.source = FALSE))

            OK_agree1 <- isTRUE(all.equal(as.numeric(id_data_DB), tmp_data,
              tolerance = 1e2 * tol))

            if (!OK_agree1)
              print(paste("Data already in output DB with P_id =", id, "of table",
              shQuote(table_name), "differ from data of file", shQuote(theFileList[j])))
          }
        }

        OK_agree2 <- FALSE
        if (OK_line2) {
          id_data_DB <- DBI::dbGetQuery(con2, paste0("SELECT * FROM \"", table_name,
            "\" WHERE P_id = ", id))

          tmp_data <- substr(sql_cmds[k], 9 + id_start, nchar(sql_cmds[k]))
          repeat {
            nt <- nchar(tmp_data)
            if (nt <= 1 || substr(tmp_data, nt, nt) == ")") break
            tmp_data <- substr(tmp_data, 1, nt - 1)
          }
          if (nt > 1 ) {
            tmp_data <- paste0("c(", tmp_data)
            tmp_data <- gsub("NULL", "NA", tmp_data)
            tmp_data <- eval(parse(text = tmp_data, keep.source = FALSE))

            OK_agree2 <- isTRUE(all.equal(as.numeric(id_data_DB), tmp_data))

            if (!OK_agree2)
              print(paste("Data already in output DB with P_id =", id, "of table",
              shQuote(table_name), "differ from data of file", shQuote(theFileList[j])))
          }
        }

        # Insert data via temporary SQL statement
        OK_line1 <- OK_line && !OK_line1
        OK_line2 <- if (add_to_DBCurrent) OK_line && !OK_line2 else TRUE

        if (OK_line1) {
          res <- try(DBI::dbSendQuery(con, sql_cmds[k]))
          OK_line1 <- OK_line1 && !inherits(res, "try-error")
          if (OK_line1)
            pids_inserted[[table_name]] <- c(pids_inserted[[table_name]], id)
        }
        if (OK_line2 && add_to_DBCurrent) {
          res <- try(DBI::dbSendQuery(con2, sql_cmds[k]))
          OK_line2 <- OK_line2 && !inherits(res, "try-error")
          if (OK_line2)
            pids2_inserted[[table_name]] <- c(pids2_inserted[[table_name]], id)
        }

        # Add processed Pid to vector
        if (OK_line1 && OK_line2) {
          if (print.debug)
            print(paste("Added to table", shQuote(table_name), "of output DB: P_id =", id,
              "from row", k, "of", shQuote(theFileList[j])))

        } else {
          if (!OK_agree1 || (OK_agree2 && add_to_DBCurrent)) {
            notOK_lines <- c(notOK_lines, k)
            print(paste("The output DB has problems with inserting P_id =", id, "to table",
              shQuote(table_name), "when processing file", shQuote(theFileList[j])))
          }
        }
      }

      if (is.null(notOK_lines)) {
        OK_tempfile <- OK_tempfile && DBI::dbCommit(con)
        if (add_to_DBCurrent)
          OK_tempfile <- OK_tempfile && DBI::dbCommit(con2)

      } else {
        OK_tempfile <- FALSE
        DBI::dbRollback(con)
        if (add_to_DBCurrent) DBI::dbRollback(con2)
        # Write failed lines to new file
        writeLines(sql_cmds[notOK_lines],
          con = file.path(dir.out.temp, sub(".", "_failed.", theFileList[j], fixed = TRUE)))
      }

      # Clean up and report
      if (OK_tempfile || !is.null(notOK_lines)) {
        cat(file.path(dir.out.temp, theFileList[j]),
            file = file.path(dir.out.temp, concatFile), append = TRUE, sep = "\n")

        if (deleteTmpSQLFiles)
          try(file.remove(file.path(dir.out.temp, theFileList[j])), silent = TRUE)
      }

      if (print.debug) {
        tDB <- round(difftime(Sys.time(), tDB1, units = "secs"), 2)
        print(paste("    ended at", Sys.time(), "after", tDB, "s"))
      }
    }

    if (verbose)
      print(paste("Output DB complete in :",
        round(difftime(Sys.time(), t1, units = "secs"), 2), "s"))

    DBI::dbDisconnect(con)
    if (do_DBCurrent) DBI::dbDisconnect(con2)
  }


  invisible(TRUE)
}


do_copyCurrentConditionsFromDatabase <- function(name.OutputDB, name.OutputDBCurrent,
  verbose = FALSE) {

  if (verbose)
    print(paste("Database is copied and subset to ambient condition: start at ",  Sys.time()))
  #Get sql for tables and index
  resSQL<-dbSendQuery(con, "SELECT sql FROM sqlite_master WHERE type='table' ORDER BY name;")
  sqlTables <- fetch(resSQL,n=-1)
  sqlTables <- unlist(sqlTables)
  sqlTables <- sqlTables[-grep(pattern="sqlite_sequence",sqlTables)]
  dbClearResult(resSQL)
  resIndex<-dbSendQuery(con, "SELECT sql FROM sqlite_master WHERE type='view' ORDER BY name;")
  sqlView <- fetch(resIndex,n=-1)
  dbClearResult(resIndex)
  sqlView<-unlist(sqlView)
  sqlView <- sqlView[!is.na(sqlView)]
  Tables <- dbListTables(con)
  Tables <- Tables[-grep(pattern="sqlite_sequence",Tables)]

  con <- DBI::dbConnect(RSQLite::SQLite(), name.OutputDBCurrent)
  for(i in 1:length(sqlTables)) {#Create the tables
    res<-dbSendQuery(con, sqlTables[i])
    dbClearResult(res)
  }
  DBI::dbGetQuery(con, sqlView)

  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = name.OutputDB)
  #Get Tables minus ones we do not want
  Tables <- dbListTables(con)
  Tables <- Tables[-grep(pattern="sqlite_sequence",Tables)]
  Tables <- Tables[-(which(Tables %in% headerTables()))]

  writeLines(text=paste(".mode insert ", Tables, "\n.out ", Tables,".sql\nSELECT * FROM ",Tables," WHERE P_id IN (SELECT P_id FROM runs WHERE scenario_id = 1 ORDER BY P_id);",sep=""),con="dump.txt")
  lines <- c("PRAGMA cache_size = 400000;","PRAGMA synchronous = 1;","PRAGMA locking_mode = EXCLUSIVE;","PRAGMA temp_store = MEMORY;","PRAGMA auto_vacuum = NONE;")
  writeLines(text=c(lines,paste(".read ",Tables,".sql",sep="")),con="insert.txt")

  system(paste("cat dump.txt | sqlite3 ", shQuote(name.OutputDB)))
  system(paste("cat insert.txt | sqlite3 ", shQuote(name.OutputDBCurrent)))

  unlink(paste(Tables,".sql",sep=""))

  Tables <- dbListTables(con)
  Tables <- Tables[-grep(pattern="sqlite_sequence",Tables)]
  Tables <- Tables[(which(Tables %in% headerTables()[-1]))]

  writeLines(text=paste(".mode insert ", Tables, "\n.out ", Tables,".sql\nSELECT * FROM ",Tables,";",sep=""),con="dump.txt")
  lines <- c("PRAGMA cache_size = 400000;","PRAGMA synchronous = 1;","PRAGMA locking_mode = EXCLUSIVE;","PRAGMA temp_store = MEMORY;","PRAGMA auto_vacuum = NONE;")
  writeLines(text=c(lines,paste(".read ",Tables,".sql",sep="")),con="insert.txt")

  system(paste("cat dump.txt | sqlite3 ", shQuote(name.OutputDB)))
  system(paste("cat insert.txt | sqlite3 ", shQuote(name.OutputDBCurrent)))

  unlink(paste(Tables,".sql",sep=""))
  unlink(c("dump.txt","insert.txt"))

  DBI::dbDisconnect(con)

  invisible(TRUE)
}


check_outputDB_completeness <- function(name.OutputDB, name.OutputDBCurrent = NULL,
  do_DBcurrent = FALSE, parallel_runs = FALSE, parallel_init = FALSE,
  parallel_backend = NULL, cl = NULL, dir.out = getwd(), swsf_env = NULL) {

  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = name.OutputDB,
    flags = RSQLite::SQLITE_RO)
  Tables <- DBI::dbListTables(con) #get a list of tables
  Tables <- Tables[-which(Tables %in% headerTables())]
  DBI::dbDisconnect(con)

  missing_Pids <- missing_Pids_current <- NULL

  if (parallel_runs && parallel_init) {

    obj2exp <- gather_objects_for_export(varlist = ls(envir = swsf_env),
      list_envs = list(rSWSF = swsf_env))

    #call the simulations depending on parallel backend
    if (identical(parallel_backend, "mpi")) {
      Rmpi::mpi.bcast.cmd(require(RSQLite, quietly = TRUE))
      export_objects_to_workers(obj2exp, "mpi")

      missing_Pids <- Rmpi::mpi.applyLB(x = Tables, fun = missing_Pids_outputDB,
        dbname = name.OutputDB)

      if (do_DBcurrent) {
        missing_Pids_current <- Rmpi::mpi.applyLB(x = Tables, fun = missing_Pids_outputDB,
          dbname = name.OutputDBCurrent)
      }

      Rmpi::mpi.bcast.cmd(rm(list = ls()))
      Rmpi::mpi.bcast.cmd(gc())

    } else if(identical(parallel_backend, "snow")) {
      snow::clusterEvalQ(cl, require(RSQLite, quietly = TRUE))
      export_objects_to_workers(obj2exp, "snow", cl)

      missing_Pids <- snow::clusterApplyLB(cl, x = Tables, fun = missing_Pids_outputDB,
        dbname = name.OutputDB)

      if (do_DBcurrent) {
        missing_Pids_current <- snow::clusterApplyLB(cl, x = Tables,
          fun = missing_Pids_outputDB, dbname = name.OutputDBCurrent)
      }

      snow::clusterEvalQ(cl, rm(list = ls()))
      snow::clusterEvalQ(cl, gc())
    }

  } else {
    missing_Pids <- lapply(Tables, missing_Pids_outputDB, dbname = name.OutputDB)

    if (do_DBcurrent) {
      missing_Pids_current <- lapply(Tables, missing_Pids_outputDB,
        dbname = name.OutputDBCurrent)
    }
  }

  missing_Pids <- unique(unlist(missing_Pids))
  missing_Pids_current <- unique(unlist(missing_Pids_current))

  if (length(missing_Pids) > 0) {
    missing_Pids <- as.integer(sort(missing_Pids))
    ftemp <- file.path(dir.out, "dbTables_Pids_missing.rds")

    if (identical(missing_Pids, -1L)) {
      print(paste("Output DB", shQuote(name.OutputDB), "is empty and not complete"))

    } else {
      print(paste("Output DB", shQuote(name.OutputDB), "is missing n =",
        length(missing_Pids), "records; P_id of these records are saved to file",
        shQuote(ftemp)))
    }
    saveRDS(missing_Pids, file = ftemp)
  }

  if (length(missing_Pids_current) > 0) {
    missing_Pids_current <- as.integer(sort(missing_Pids_current))
    ftemp <- file.path(dir.out, "dbTablesCurrent_Pids_missing.rds")

    if (identical(missing_Pids_current, -1L)) {
      print(paste("Current output DB", shQuote(name.OutputDBCurrent), "is empty",
        "and not complete"))

    } else {
      print(paste("Current output DB", shQuote(name.OutputDBCurrent), "is missing n =",
        length(missing_Pids_current), "records; P_id of these records are saved to file",
        shQuote(ftemp)))
    }
    saveRDS(missing_Pids_current, file = ftemp)
  }

  invisible(list(missing_Pids = missing_Pids, missing_Pids_current = missing_Pids_current))
}
