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

    if (DBI::dbExistsTable(con, "header") && DBI::dbExistsTable(con, Table)) {
      sql <- paste0("SELECT header.P_id FROM header LEFT JOIN ", Table, " ON (header.P_id=",
        Table, ".P_id) WHERE header.Include_YN = 1 AND ", Table, ".P_id is NULL ",
        "ORDER BY header.P_id")
      mP_ids <- RSQLite::dbGetQuery(con, sql)[, 1]
    }

    DBI::dbDisconnect(con)
  }

  as.integer(mP_ids)
})

getIDs_from_db_Pids <- function(dbname, Pids) {
  res <- data.frame(site_id = -1L, treatment_id = -1L)[-1, ]

  if (file.exists(dbname)) {
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbname, flags = RSQLite::SQLITE_RO)

    if (DBI::dbExistsTable(con, "runs")) {
      sql <- paste("SELECT site_id, treatment_id FROM runs WHERE P_id IN (?) ORDER BY site_id")
      rs <- RSQLite::dbSendQuery(con, sql)
      RSQLite::dbBind(rs, list(Pids))
      res <- RSQLite::dbFetch(rs)
      RSQLite::dbClearResult(rs)
    }

    DBI::dbDisconnect(con)
  }

  res
}


dbOutput_ListDesignTables <- function() c("runs", "sqlite_sequence", "header", "run_labels",
  "scenario_labels", "sites", "experimental_labels", "treatments", "simulation_years",
  "weatherfolders")


dbOutput_ListOutputTables <- function(con = NULL, dbname = NULL) {
  use_con <- !is.null(con) && inherits(con, "SQLiteConnection") && DBI::dbIsValid(con)

  if (!use_con) {
    if (is.null(dbname)) {
      print("'dbOutput_ListOutputTables': arguments con and dbname cannot both be NULL")
      return(NULL)
    }
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbname, flags = RSQLite::SQLITE_RO)
  }

  temp <- DBI::dbListTables(con)
  tables <- temp[!(temp %in% dbOutput_ListDesignTables())]

  if (!use_con)
    DBI::dbDisconnect(con)

  tables
}


dbOutput_Tables_have_SoilLayers <- function(tables = NULL, con = NULL, dbname = NULL) {
  use_con <- !is.null(con) && inherits(con, "SQLiteConnection") && DBI::dbIsValid(con)

  if (!use_con) {
    if (is.null(dbname)) {
      print("'dbOutput_ListTables_wSoilLayers': arguments con and dbname cannot both be NULL")
      return(NULL)
    }
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbname, flags = RSQLite::SQLITE_RO)
  }

  if (!is.null(tables))
    tables <- dbOutput_ListOutputTables(con)

  has_soillayers <- sapply(tables, function(table) {
    temp <- DBI::dbListFields(con, table)
    any(temp == "Soil_Layer")
  })
  names(has_soillayers) <- tables

  if (!use_con)
    DBI::dbDisconnect(con)

  has_soillayers
}



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

#' Get name of weather file from output database
local_weatherDirName <- compiler::cmpfun(function(i_sim, runN, scN, dbOutput) {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbOutput, flags = RSQLite::SQLITE_RO)
  temp <- DBI::dbGetQuery(con, paste("SELECT WeatherFolder FROM header WHERE P_id=",
    it_Pid(i_sim, runN, 1, scN)))[1, 1]
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

get_inserted_ids <- function(con, tables, tables_w_soillayers) {
  ids <- list()

  for (k in seq_along(tables)) {
    sql <- if (tables_w_soillayers[k]) {
        paste("SELECT P_id, Soil_Layer FROM", tables[k], "ORDER BY P_id;")
      } else {
        paste("SELECT P_id FROM", tables[k], "ORDER BY P_id;")
      }
    temp <- DBI::dbGetQuery(con, sql)

    res <- list(pids = temp[, "P_id"])
    if (tables_w_soillayers[k]) {
      res <- c(res, list(sids = apply(temp[, c("P_id", "Soil_Layer")], 1, paste,
        collapse = "-")))
    }

    ids[[tables[k]]] <- res
  }

  ids
}

check_data_agreement <- function(con, table_name, id, sl = NULL,
  tmp_data, has_soillayer, filename = "") {

  OK_agree <- FALSE
  # check whether data agree
  id_data_DB <- if (is.null(sl)) {
      DBI::dbGetQuery(con, paste0("SELECT * FROM \"", table_name,
        "\" WHERE P_id = ", id))
    } else {
      DBI::dbGetQuery(con, paste0("SELECT * FROM \"", table_name,
        "\" WHERE P_id = ", id, " AND Soil_Layer = ", sl))
    }

  repeat {
    nt <- nchar(tmp_data)
    if (nt <= 1 || substr(tmp_data, nt, nt) == ")") break
    tmp_data <- substr(tmp_data, 1, nt - 1)
  }
  if (nt > 1 ) {
    tmp_data <- paste0("c(", tmp_data)
    tmp_data <- gsub("NULL", "NA", tmp_data)
    tmp_data <- eval(parse(text = tmp_data, keep.source = FALSE))

    res <- all.equal(as.numeric(id_data_DB), tmp_data, tolerance = 1e2 * tol)
    OK_agree <- isTRUE(res)

    if (!OK_agree)
      print(paste("Data which are in output DB with P_id =", id,
        if (!is.null(sl)) paste("and soil layer =", sl) else NULL, "of table",
        shQuote(table_name), "differ from data of file", shQuote(filename), ":",
        paste(res, collapse = "--")))
  }

  OK_agree
}


move_temporary_to_outputDB <- function(path, dbOutput,
  name.OutputDBCurrent = NULL, t.overall = Sys.time(), opt_job_time = NULL,
  do_DBCurrent = FALSE, cleanDB = FALSE, deleteTmpSQLFiles = FALSE, continueAfterAbort = TRUE,
  print.debug = FALSE, verbose = FALSE) {

  t1 <- Sys.time()

  #concatenate file keeps track of sql files inserted into data
  concatFile <- "sqlFilesInserted.txt"

  # Locate temporary SQL files
  theFileList <- list.files(path = path, pattern = "SQL",
    full.names = FALSE, recursive = TRUE, include.dirs = FALSE, ignore.case = FALSE)

  # remove any already inserted files from list
  if (!deleteTmpSQLFiles && continueAfterAbort) {
    temp <- file.path(path, concatFile)
    completedFiles <- if (file.exists(temp)) {
        basename(readLines(temp))
      } else {
        character(0)
      }
    temp <- theFileList %in% completedFiles
    if (any(temp)) {
      theFileList <- theFileList[!temp]
    }
  }

  if (length(theFileList) > 0) {
    # Connect to the Database
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbOutput)
    out_tables_aggr <- dbOutput_ListOutputTables(con)

    reset_DBCurrent <- do_DBCurrent && (cleanDB || !file.exists(name.OutputDBCurrent))
    if (reset_DBCurrent)
      file.copy(from = dbOutput, to = name.OutputDBCurrent)
    if (do_DBCurrent)
      con2 <- DBI::dbConnect(RSQLite::SQLite(), dbname = name.OutputDBCurrent)
    if (reset_DBCurrent)
      DBI::dbGetQuery(con2, "DELETE FROM runs WHERE scenario_id != 1;") # DROP ALL ROWS THAT ARE NOT CURRENT FROM HEADER

    # Prepare output databases
    set_PRAGMAs(con, PRAGMA_settings1())
    if (do_DBCurrent)
      set_PRAGMAs(con2, PRAGMA_settings1())

    # Check what has already been inserted in each tables
    tables_w_soillayers <- dbOutput_Tables_have_SoilLayers(out_tables_aggr, con)
    ids_inserted <- get_inserted_ids(con, out_tables_aggr, tables_w_soillayers)
    if (do_DBCurrent)
      ids2_inserted <- get_inserted_ids(con2, out_tables_aggr, tables_w_soillayers)

    # Add data to SQL databases
    for (j in seq_along(theFileList)) {
      tDB1 <- Sys.time()
      has_time_to_concat <- (difftime(tDB1, t.overall, units = "secs") +
        opt_job_time[["one_concat_s"]]) < opt_job_time[["wall_time_s"]]
      if (!has_time_to_concat)
        break

      # Read SQL statements from temporary file
      sql_cmds <- readLines(file.path(path, theFileList[j]))
      add_to_DBCurrent <- do_DBCurrent && grepl("SQL_Current", theFileList[j])

      if (verbose)
        print(paste("Adding", shQuote(theFileList[j]), "with", length(sql_cmds), "lines",
          "to output DB: started at ", tDB1))

      #--- Send SQL statements to database
      OK_tempfile <- TRUE
      notOK_lines <- NULL

      for (k in seq_along(sql_cmds)) {
        OK_line <- TRUE

        # Determine table
        id_table <- as.integer(gregexpr('\"', sql_cmds[k], fixed = TRUE)[[1]])

        if (any(id_table[1] < 1, id_table[2] <= id_table[1])) {
          print(paste0("Name of table not located in file ", shQuote(theFileList[j]),
            " on line ", k, ": ", substr(sql_cmds[k], 1, 100)))
          next
        }

        table_name <- substr(sql_cmds[k], 1 + id_table[1], -1 + id_table[2])
        OK_line <- OK_line && any(table_name == out_tables_aggr)

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

        # Check if P_id already in output DB
        OK_check1 <- OK_line && (id %in% ids_inserted[[table_name]][["pids"]])
        OK_check2 <- if (OK_line && add_to_DBCurrent) {
            id %in% ids2_inserted[[table_name]][["pids"]]
          } else FALSE

        # If P_id already in output DB, then check whether table has soil layers
        # and, if so, whether soil layer is in DB
        if ((OK_check1 || OK_check2) && tables_w_soillayers[table_name]) {
          # Determine soil layer
          id_sl <- as.integer(gregexpr(",", sql_cmds[k], fixed = TRUE)[[1]])
          if (any(id_sl[1] < 1, id_sl[2] <= id_sl[1])) {
            print(paste0("ID of soil layer not located in file ", shQuote(theFileList[j]),
              " on line ", k, ": ", substr(sql_cmds[k], 1, 100)))
            next
          }

          sl <- as.integer(substr(sql_cmds[k], 1 + id_sl[1], -1 + id_sl[2]))
          OK_line <- OK_line && is.finite(sl)
          id_sl <- paste0(id, "-", sl)

          # Check if P_id already in output DB
          OK_check1 <- OK_line && OK_check1 &&
            (id_sl %in% ids_inserted[[table_name]][["sids"]])
          OK_check2 <- if (OK_line && OK_check2 && add_to_DBCurrent) {
              id_sl %in% ids2_inserted[[table_name]][["sids"]]
            } else FALSE

        } else {
          sl <- NULL
        }

        OK_agree1 <- if (OK_check1) {
            check_data_agreement(con, table_name, id, sl,
              tmp_data = substr(sql_cmds[k], 9 + id_start, nchar(sql_cmds[k])),
              has_soillayer = tables_w_soillayers[table_name], filename = theFileList[j])
          } else FALSE

        OK_agree2 <- if (OK_check2) {
            check_data_agreement(con2, table_name, id, sl,
              tmp_data = substr(sql_cmds[k], 9 + id_start, nchar(sql_cmds[k])),
              has_soillayer = tables_w_soillayers[table_name], filename = theFileList[j])
          } else FALSE

        # Insert data via temporary SQL statement
        OK_add1 <- OK_line && !OK_agree1
        OK_add2 <- if (add_to_DBCurrent) OK_line && !OK_agree2 else FALSE

        if (OK_add1) {
          res <- DBI::dbWithTransaction(con, {
            res <- try(DBI::dbSendStatement(con, sql_cmds[k]))
            temp <- !inherits(res, "try-error")
            if (temp) {
              ids_inserted[[table_name]][["pids"]] <- unique(
                c(ids_inserted[[table_name]][["pids"]], id))
              if (!is.null(sl))
               ids_inserted[[table_name]][["sids"]] <- unique(
                c(ids_inserted[[table_name]][["sids"]], id_sl))
            }
            temp && DBI::dbClearResult(res)
          })
          OK_add1 <- OK_add1 && res
        }
        if (OK_add2) {
          res <- DBI::dbWithTransaction(con2, {
            res <- try(DBI::dbSendStatement(con2, sql_cmds[k]))
            temp <- !inherits(res, "try-error")
            if (temp) {
              ids2_inserted[[table_name]][["pids"]] <- unique(
                c(ids2_inserted[[table_name]][["pids"]], id))
              if (!is.null(sl))
               ids2_inserted[[table_name]][["sids"]] <- unique(
                c(ids2_inserted[[table_name]][["sids"]], id_sl))
            }
            temp && DBI::dbClearResult(res)
          })
          OK_add2 <- OK_add2 && res
        }

        # Add processed Pid to vector
        if (OK_add1 &&
          ((!OK_add2 && !add_to_DBCurrent) || (OK_add2 && add_to_DBCurrent))) {

          if (print.debug)
            print(paste("Added to table", shQuote(table_name), "of output DB: P_id =", id,
              if (!is.null(sl)) paste("and soil layer =", sl) else NULL,
              "from row", k, "of", shQuote(theFileList[j])))

        } else {
          if (!OK_agree1 || (!OK_agree2 && add_to_DBCurrent)) {
            notOK_lines <- c(notOK_lines, k)
            print(paste("The output DB has problems with inserting P_id =", id,
              if (!is.null(sl)) paste("and soil layer =", sl) else NULL, "to table",
              shQuote(table_name), "when processing row =", k, "of file",
              shQuote(theFileList[j])))
          }
        }
      }

      #- end transaction
      if (!is.null(notOK_lines)) {
        OK_tempfile <- FALSE
        # Write failed lines to new file
        writeLines(sql_cmds[notOK_lines],
          con = file.path(path, sub(".", "_failed.", theFileList[j], fixed = TRUE)))
      }

      # Clean up and report
      if (OK_tempfile || !is.null(notOK_lines)) {
        cat(file.path(path, theFileList[j]),
            file = file.path(path, concatFile), append = TRUE, sep = "\n")

        if (deleteTmpSQLFiles)
          try(file.remove(file.path(path, theFileList[j])), silent = TRUE)
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


do_copyCurrentConditionsFromDatabase <- function(dbOutput, name.OutputDBCurrent,
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

  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbOutput)
  #Get Tables minus ones we do not want
  Tables <- dbOutput_ListOutputTables(con)

  writeLines(text=paste(".mode insert ", Tables, "\n.out ", Tables,".sql\nSELECT * FROM ",Tables," WHERE P_id IN (SELECT P_id FROM runs WHERE scenario_id = 1 ORDER BY P_id);",sep=""),con="dump.txt")
  lines <- c("PRAGMA cache_size = 400000;","PRAGMA synchronous = 1;","PRAGMA locking_mode = EXCLUSIVE;","PRAGMA temp_store = MEMORY;","PRAGMA auto_vacuum = NONE;")
  writeLines(text=c(lines,paste(".read ",Tables,".sql",sep="")),con="insert.txt")

  system(paste("cat dump.txt | sqlite3 ", shQuote(dbOutput)))
  system(paste("cat insert.txt | sqlite3 ", shQuote(name.OutputDBCurrent)))

  unlink(paste(Tables,".sql",sep=""))

  Tables <- dbOutput_ListOutputTables(con)

  writeLines(text=paste(".mode insert ", Tables, "\n.out ", Tables,".sql\nSELECT * FROM ",Tables,";",sep=""),con="dump.txt")
  lines <- c("PRAGMA cache_size = 400000;","PRAGMA synchronous = 1;","PRAGMA locking_mode = EXCLUSIVE;","PRAGMA temp_store = MEMORY;","PRAGMA auto_vacuum = NONE;")
  writeLines(text=c(lines,paste(".read ",Tables,".sql",sep="")),con="insert.txt")

  system(paste("cat dump.txt | sqlite3 ", shQuote(dbOutput)))
  system(paste("cat insert.txt | sqlite3 ", shQuote(name.OutputDBCurrent)))

  unlink(paste(Tables,".sql",sep=""))
  unlink(c("dump.txt","insert.txt"))

  DBI::dbDisconnect(con)

  invisible(TRUE)
}


check_outputDB_completeness <- function(dbOutput, name.OutputDBCurrent = NULL,
  update_workDB = FALSE, do_DBcurrent = FALSE, opt_parallel, dir_out = getwd(),
  swsf_env = NULL) {

  Tables <- dbOutput_ListOutputTables(dbname = dbOutput)

  missing_Pids <- missing_Pids_current <- NULL

  if (opt_parallel[["do_parallel"]]) {

    obj2exp <- gather_objects_for_export(varlist = ls(envir = swsf_env),
      list_envs = list(rSWSF = swsf_env))

    #call the simulations depending on parallel backend
    if (identical(opt_parallel[["parallel_backend"]], "mpi")) {
      Rmpi::mpi.bcast.cmd(require(RSQLite, quietly = TRUE))
      export_objects_to_workers(obj2exp, "mpi")

      missing_Pids <- Rmpi::mpi.applyLB(X = Tables, FUN = missing_Pids_outputDB,
        dbname = dbOutput)

      if (do_DBcurrent) {
        missing_Pids_current <- Rmpi::mpi.applyLB(X = Tables, FUN = missing_Pids_outputDB,
          dbname = name.OutputDBCurrent)
      }

      Rmpi::mpi.bcast.cmd(rm(list = ls()))
      Rmpi::mpi.bcast.cmd(gc())

    } else if(identical(opt_parallel[["parallel_backend"]], "cluster")) {
      parallel::clusterEvalQ(opt_parallel[["cl"]], require(RSQLite, quietly = TRUE))
      export_objects_to_workers(obj2exp, "cluster", opt_parallel[["cl"]])

      missing_Pids <- parallel::clusterApplyLB(opt_parallel[["cl"]], x = Tables, fun = missing_Pids_outputDB,
        dbname = dbOutput)

      if (do_DBcurrent) {
        missing_Pids_current <- parallel::clusterApplyLB(opt_parallel[["cl"]], x = Tables,
          fun = missing_Pids_outputDB, dbname = name.OutputDBCurrent)
      }

      parallel::clusterEvalQ(opt_parallel[["cl"]], rm(list = ls()))
      parallel::clusterEvalQ(opt_parallel[["cl"]], gc())
    }

  } else {
    missing_Pids <- lapply(Tables, missing_Pids_outputDB, dbname = dbOutput)

    if (do_DBcurrent) {
      missing_Pids_current <- lapply(Tables, missing_Pids_outputDB,
        dbname = name.OutputDBCurrent)
    }
  }

  missing_Pids <- unique(unlist(missing_Pids))
  missing_Pids <- as.integer(sort(missing_Pids))
  missing_runIDs <- NULL
  missing_Pids_current <- unique(unlist(missing_Pids_current))
  missing_Pids_current <- as.integer(sort(missing_Pids_current))

  if (length(missing_Pids) > 0) {
    ftemp <- file.path(dir_out, "dbTables_Pids_missing.rds")
    if (identical(missing_Pids, -1L)) {
      print(paste("Output DB", shQuote(dbOutput), "is empty and not complete"))

    } else {
      print(paste("Output DB", shQuote(dbOutput), "is missing n =",
        length(missing_Pids), "records"))

     # Output missing Pids to rds file
      print(paste("P_id of these records are saved to file", shQuote(ftemp)))
      saveRDS(missing_Pids, file = ftemp)

      # Update workDB
      if (update_workDB) {
        print("'workDB' is updated with these missing P_id to be prepared for a re-run")

        con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbOutput,
          flags = RSQLite::SQLITE_RO)
        scN <- RSQLite::dbGetQuery(con, "SELECT Max(id) FROM scenario_labels")[1, 1]
        RSQLite::dbDisconnect(con)

        missing_runIDs <- it_sim2(missing_Pids, scN)
        temp <- dbWork_redo(dir_out, runIDs = missing_runIDs)
      }
    }
  }

  if (length(missing_Pids_current) > 0) {
    ftemp <- file.path(dir_out, "dbTablesCurrent_Pids_missing.rds")

    if (identical(missing_Pids_current, -1L)) {
      print(paste("Current output DB", shQuote(name.OutputDBCurrent), "is empty",
        "and not complete"))

    } else {
      print(paste("Current output DB", shQuote(name.OutputDBCurrent), "is missing n =",
        length(missing_Pids_current), "records; P_id of these records are saved to file",
        shQuote(ftemp)))
     saveRDS(missing_Pids_current, file = ftemp)
   }
  }

  invisible(list(missing_Pids = missing_Pids, missing_Pids_current = missing_Pids_current,
    missing_runIDs = missing_runIDs))
}


  dbOutput_create_Design <- function(con_dbOut, SWRunInformation, Index_RunInformation,
      runsN_master,runIDs_sites, runsN_Pid, runsN_total, scenario_No, expN,
      create_treatments, create_experimentals, sw_input_treatments, sw_input_treatments_use,
      sw_input_experimentals, climate.conditions, simstartyr, startyr, endyr,
      digitsN_total) {

		RSQLite::dbGetQuery(con_dbOut, paste("CREATE TABLE",
      "weatherfolders(id INTEGER PRIMARY KEY AUTOINCREMENT, folder TEXT UNIQUE NOT NULL)"))

		if (!(all(any((SWRunInformation$dailyweather_source[runIDs_sites] == "LookupWeatherFolder")),
				  any(create_treatments == "LookupWeatherFolder")))) {
			if (any(!is.na(SWRunInformation$WeatherFolder))) {
				RSQLite::dbBegin(con_dbOut)
				RSQLite::dbGetPreparedQuery(con_dbOut, "INSERT INTO weatherfolders VALUES(NULL, :folder)",
					bind.data = data.frame(folder = unique(na.exclude(SWRunInformation$WeatherFolder)), stringsAsFactors = FALSE))
				RSQLite::dbCommit(con_dbOut)

				# Slightly slower alternative to RSQLite::dbGetPreparedQuery()
#				temp <- unique(na.exclude(SWRunInformation$WeatherFolder))
#				RSQLite::dbWriteTable(con, "weatherfolders", append = TRUE,
#					value = data.frame(id = rep(NA, length(temp)), folder = temp), row.names = FALSE)

			} else {
				stop("All WeatherFolder names in master input file are NAs.")
			}
		}


		#############Site Table############################
		# Note: invariant to 'include_YN', i.e., do not subset rows of 'SWRunInformation'
		index_sites <- sort(unique(c(sapply(required_colnames_SWRunInformation(),
				function(x) which(x == colnames(SWRunInformation))),
			Index_RunInformation)))
		sites_data <- data.frame(SWRunInformation[, index_sites], row.names = NULL,
		  check.rows = FALSE, check.names = FALSE, stringsAsFactors = FALSE)
		# Get WeatherFolder_id from table weatherfolders
		sites_data$WeatherFolder <- getSiteIds(con_dbOut, sites_data$WeatherFolder)
		colnames(sites_data) <- sub(pattern = "WeatherFolder",
		  replacement = "WeatherFolder_id", colnames(sites_data))
		site_col_types <- sapply(sites_data, function(x) RSQLite::dbDataType(con_dbOut, x))
		RSQLite::dbGetQuery(con_dbOut,
			paste0("CREATE TABLE sites(\"id\" INTEGER PRIMARY KEY AUTOINCREMENT, ",
				paste0('\"', colnames(sites_data), '\" ', site_col_types, collapse = ", "),
				", FOREIGN KEY(WeatherFolder_id) REFERENCES weatherfolders(id));"))

		RSQLite::dbWriteTable(con_dbOut, "sites", append = TRUE,
			value = cbind(id = NA, sites_data), row.names = FALSE)

		useExperimentals <- expN > 0 && length(create_experimentals) > 0
		useTreatments <- any(!(create_treatments %in% create_experimentals))

		#############simulation_years table#########################
		RSQLite::dbGetQuery(con_dbOut, paste("CREATE TABLE",
		  "simulation_years(id INTEGER PRIMARY KEY AUTOINCREMENT,",
		  "simulationStartYear INTEGER NOT NULL, StartYear INTEGER NOT NULL,",
		  "EndYear INTEGER NOT NULL);"))
		##################################################


		##########Create table experimental_labels only if using experimentals
		if (useExperimentals) {
			RSQLite::dbGetQuery(con_dbOut, paste("CREATE TABLE",
			  "experimental_labels(id INTEGER PRIMARY KEY AUTOINCREMENT,",
			  "label TEXT UNIQUE NOT NULL);"))
			RSQLite::dbBegin(con_dbOut)
			RSQLite::dbGetPreparedQuery(con_dbOut, paste("INSERT INTO",
			  "experimental_labels VALUES(NULL, :label);"),
				bind.data = data.frame(label = sw_input_experimentals[,1],
				  stringsAsFactors = FALSE))
			RSQLite::dbCommit(con_dbOut)
		}
		################################

		# If LookupWeatherFolder is ON we need to make sure all of the weather folders are in
		# weatherfolders table
#TODO: WeatherFolder update
		if (any(create_treatments == "LookupWeatherFolder")) {
			#which ones are not in SWRunInformation$WeatherFolder

			#make a combined list of experimentals and treatments LookupWeatherFolder List
			#first add any from the experimentals table if its turned on
			#next add any from the treatments table if its turned on
			treatments_lookupweatherfolders <- character(0)
			if (any(names(sw_input_treatments_use[sw_input_treatments_use])=="LookupWeatherFolder")) {
				treatments_lookupweatherfolders <- c(treatments_lookupweatherfolders,
				  sw_input_treatments$LookupWeatherFolder[runIDs_sites])
			}
			if (any(create_experimentals == "LookupWeatherFolder")) {
				treatments_lookupweatherfolders <- c(treatments_lookupweatherfolders,
				  sw_input_experimentals$LookupWeatherFolder[runIDs_sites])
			}
			#Remove NA because that defaults to sites default weatherFolder also make sure each folder is unique
			temp <- !is.na(treatments_lookupweatherfolders)
			treatments_lookupweatherfolders <- treatments_lookupweatherfolders[temp]
			treatments_lookupweatherfolders <- unique(treatments_lookupweatherfolders)
			if (length(treatments_lookupweatherfolders) == 0) {
				print("LookupWeatherFolder is turned on in treatments or experimentals or both but is not used")

			} else {
				#make a temp data.frame of a column NA's and a column of folder names
				LWF_index <- data.frame(id = rep(NA, length(treatments_lookupweatherfolders)),
				  folder = treatments_lookupweatherfolders, stringsAsFactors = FALSE)
				#Get the id from sites table if the folder is in it
				LWF_index$id <- getSiteIds(con_dbOut, LWF_index$folder)
				#if there are any NA's we need to add those to the weatherfolder db table and
				# update its id in our lookuptable for weatherfolder
				if (any(is.na(LWF_index$id))) {
					#get max id from weatherfolders table
					temp <- is.na(LWF_index$id)
					weatherfolders_index <- as.numeric(RSQLite::dbGetQuery(con_dbOut,
					  "SELECT MAX(id) FROM weatherfolders;"))
					LWF_index$id[temp] <- as.integer(seq.int(from = weatherfolders_index + 1L,
					  to = weatherfolders_index + sum(temp), by = 1L))
					#Write those in
					RSQLite::dbBegin(con_dbOut)
					RSQLite::dbGetPreparedQuery(con_dbOut,
					  "INSERT INTO weatherfolders VALUES(:id,:folder)",
					  bind.data = LWF_index[temp, ])
					RSQLite::dbCommit(con_dbOut)
				}
			}
		}

		# get unique rows from both treatments and experimentals
		if (useExperimentals) {#Only use experimentals if there is something in it
			#Are all the columns NA
			temp <- is.na(sw_input_experimentals[, create_experimentals])
			if (all(temp))
			  stop("All Columns in experimentals table are NA")
			if (any(apply(temp, MARGIN = 2, function(x) all(x))))
			  warning("One ore more columns in experimentals table are turned on with no values or only with NA.")
			db_experimentals <- unique(sw_input_experimentals[, create_experimentals])
			#note experimentals should be unique if we have less rows then the original then lets throw an Error
			stopifnot(nrow(db_experimentals) == nrow(sw_input_experimentals))

		} else {
			#experimentals does not have any rows. Are any of the create_experimentals turned on
			if (length(create_experimentals) > 0 && expN == 0)
			  stop("No rows in experimentals table but columns are turned on")
			if (expN > 0 && length(create_experimentals) == 0)
			  warning("Rows in experimentals are not being used.")
		}

		if (useTreatments) {
			# Note: invariant to 'include_YN', i.e., do not subset 'SWRunInformation'
			# we only need the columns that are turned on and not in experimentals. Experimentals over write.
			temp <- create_treatments[!(create_treatments %in% create_experimentals)]
			temp_df <- sw_input_treatments[, temp, drop = FALSE]
			db_treatments <- unique(temp_df)
			db_treatments_rows <- nrow(db_treatments)
			#this maps locations from reduced
			temp <- duplicated(temp_df)
			treatments_unique_map <- rep(NA, nrow(temp_df))
			temp2 <- data.frame(t(temp_df))
			treatments_unique_map[temp] <- match(data.frame(t(temp_df[temp,])), temp2)
			treatments_unique_map[!temp] <- match(data.frame(t(temp_df[!temp,])), temp2)
			db_treatments_map <- unique(treatments_unique_map)
			treatments_unique_map <- sapply(treatments_unique_map, function(x)
			  which(db_treatments_map == x))

		} else {
			db_treatments_rows <- 1
		}

		#Replace the LookupWeatherFolder with the LookupWeatherFolder_id in either db_experimentals or db_treatments
		if (any(create_treatments == "LookupWeatherFolder")) {
			if (any(create_experimentals == "LookupWeatherFolder")) {
				#rename the column
				temp <- which(create_experimentals == "LookupWeatherFolder")
				colnames(db_experimentals)[temp] <- "LookupWeatherFolder_id"
				#get the id numbers for those columns and replace text
				db_experimentals$LookupWeatherFolder_id <- sapply(db_experimentals$LookupWeatherFolder_id,
				  function(x) LWF_index$id[LWF_index$folder == x])

			} else {
				#rename the column
				temp <- which(colnames(db_treatments) == "LookupWeatherFolder")
				colnames(db_treatments)[temp] <- "LookupWeatherFolder_id"
				#get the id numbers for those columns and replace text
				db_treatments$LookupWeatherFolder_id <- sapply(db_treatments$LookupWeatherFolder_id,
				  function(x) LWF_index$id[LWF_index$folder == x])
			}
		}

		useTreatmentWeatherFolder <- FALSE
		if (useExperimentals | useTreatments) {
			#Create a table to hold the values going into the database
			temp_numberRows <- if(useExperimentals) {
			    nrow(db_experimentals) * db_treatments_rows
			  } else nrow(db_treatments)
			temp_numberColumns <- {if (useExperimentals) 3 else 2} + length(create_treatments)
			temp_columnNames <- c("id", if (useExperimentals) c("experimental_id"),
			  "simulation_years_id", create_treatments)
			db_combined_exp_treatments <- data.frame(matrix(data = NA, nrow = temp_numberRows,
			  ncol = temp_numberColumns, dimnames = list(NULL, temp_columnNames)),
			  stringsAsFactors = FALSE)

			#fill in the id column.
			db_combined_exp_treatments$id <- seq_len(nrow(db_combined_exp_treatments))

			#column types are listed in this data.frame along with what table it is from
			db_treatments_column_types <- data.frame(column = create_treatments,
			  type = character(length(create_treatments)),
			  table = numeric(length(create_treatments)), stringsAsFactors = FALSE)
			#0 for teatments 1 for experimentals
			temp <- db_treatments_column_types[, "column"] %in% create_experimentals
			db_treatments_column_types[temp, "table"] <- 1

			######################
			#Get the column types from the proper tables
			temp <- create_treatments[!(create_treatments %in% create_experimentals)]
			db_treatments_column_types[, "type"] <- sapply(db_treatments_column_types[, "column"],
			  function(columnName) {
					if (columnName %in% create_experimentals) {
						RSQLite::dbDataType(con_dbOut, sw_input_experimentals[, columnName])
					} else if (columnName %in% temp) {
						RSQLite::dbDataType(con_dbOut, sw_input_treatments[, columnName])
					}
				})

			#Finalize db_treatments_column_types
			#remove YearStart or YearEnd
			db_treatments_years <- NULL
			if (any(db_treatments_column_types$column == "YearStart")) {
			  temp <- which(db_treatments_column_types[, "column"] == "YearStart")
				db_treatments_years <- rbind(db_treatments_years,
				  db_treatments_column_types[temp, ])
				db_treatments_column_types <- db_treatments_column_types[-temp, ]
			}
			if (any(db_treatments_column_types$column == "YearEnd")) {
			  temp <- which(db_treatments_column_types[, "column"] == "YearEnd")
				db_treatments_years <- rbind(db_treatments_years,
				  db_treatments_column_types[temp, ])
				db_treatments_column_types <- db_treatments_column_types[-temp, ]
			}

			#rename weather folder column name and create the fk
			fk_LookupWeatherFolder <- ""
			if (any(create_treatments == "LookupWeatherFolder")) {
				useTreatmentWeatherFolder <- TRUE
				temp <- which(db_treatments_column_types[, "column"] == "LookupWeatherFolder")
				db_treatments_column_types[temp, c("column", "type")] <- c("LookupWeatherFolder_id", "INTEGER")
				colnames(db_combined_exp_treatments)["table"] <- db_treatments_column_types[, "column"]
				fk_LookupWeatherFolder <- ", FOREIGN KEY(LookupWeatherFolder_id) REFERENCES weatherfolders(id)"
			}
			#Create the table
			RSQLite::dbGetQuery(con_dbOut,
			  paste("CREATE TABLE treatments(id INTEGER PRIMARY KEY AUTOINCREMENT,",
			  if (useExperimentals) "experimental_id INTEGER,",
			  "simulation_years_id INTEGER,",
			  paste(db_treatments_column_types[, "column"],
			    db_treatments_column_types[, "type"], collapse = ", "),
			  if (useExperimentals || fk_LookupWeatherFolder != "") ", ",
			  if (useExperimentals)
			    "FOREIGN KEY(experimental_id) REFERENCES experimental_labels(id)",
			  if (fk_LookupWeatherFolder != "")
			    ", ", fk_LookupWeatherFolder, ");"))

			#Lets put in the treatments into combined. This will repeat the reduced rows of treatments into combined
			if (useTreatments) {
				use_start <- colnames(db_treatments) == "YearStart"
				use_end <- colnames(db_treatments) == "YearEnd"
				i_use <- seq_len(ncol(db_treatments))
				if (any(use_start))
				  i_use <- i_use[!use_start]
				if (any(use_end))
				  i_use <- i_use[!use_end]
				temp <- db_treatments_column_types[, "table"] == 0
				temp <- db_treatments_column_types[temp, "column"]
				db_combined_exp_treatments[, temp] <- db_treatments[, i_use]
				#Handle StartYear and EndYear separately
				if (any(use_start) && !is.null(db_treatments_years) &&
				  db_treatments_years[db_treatments_years$column == "YearStart", "table"] == 0) {

				  db_combined_exp_treatments[, "YearStart"] <- db_treatments[, "YearStart"]
				}
				if (any(use_end) && !is.null(db_treatments_years) &&
				  db_treatments_years[db_treatments_years$column == "YearEnd", "table"] == 0) {

				  db_combined_exp_treatments[, "YearEnd"] <- db_treatments[, "YearEnd"]
				}
			}

			if (useExperimentals) {
				exp_start_rows <- seq(from = 1, to = db_treatments_rows * nrow(db_experimentals),
				  by = db_treatments_rows)
				#Insert data into our new data.frame
				for (istart in exp_start_rows) {
				  irows <- istart:(istart + db_treatments_rows - 1)
				  irows2 <- which(exp_start_rows == istart)
					#Get experimental_label_id
					db_combined_exp_treatments[irows, "experimental_id"] <- irows2
					#insert all of the rows from experimentals
					temp <- db_treatments_column_types[db_treatments_column_types[, "table"] == 1, "column"]
					db_combined_exp_treatments[irows, temp] <- db_experimentals[irows2, ]
				}
			}
		} else {
			db_combined_exp_treatments <- data.frame(matrix(data = 1, nrow = 1, ncol = 2,
			  dimnames = list(NULL, c("id", "simulation_years_id"))), stringsAsFactors = FALSE)
			RSQLite::dbGetQuery(con_dbOut, paste("CREATE TABLE",
			  "treatments(id INTEGER PRIMARY KEY AUTOINCREMENT, simulation_years_id INTEGER);"))
		}

		#if the column startYear or endYear are present move over to simulation_years
		if (any(colnames(db_combined_exp_treatments) == "YearStart") ||
		  any(colnames(db_combined_exp_treatments) == "YearEnd")) {

			simulation_years <- matrix(data = NA, nrow = nrow(db_combined_exp_treatments),
			  ncol = 4, dimnames = list(NULL,
			  c("id", "simulationStartYear", "StartYear", "EndYear")))
			#Get from treatments or get from settings
			if (any(colnames(db_combined_exp_treatments) == "YearStart")) {
				simulation_years[, "simulationStartYear"] <- db_combined_exp_treatments[, "YearStart"]
				temp <- colnames(db_combined_exp_treatments) == "YearStart"
				db_combined_exp_treatments <- db_combined_exp_treatments[, !temp]

			} else {
				simulation_years[, "simulationStartYear"] <- simstartyr
			}
			if (any(colnames(db_combined_exp_treatments) == "YearEnd")) {
				simulation_years[, "EndYear"] <- db_combined_exp_treatments[, "YearEnd"]
				temp <- colnames(db_combined_exp_treatments) == "YearEnd"
				db_combined_exp_treatments <- db_combined_exp_treatments[, !temp]

			} else {
				simulation_years[, "EndYear"] <- endyr
			}
			simulation_years[, "StartYear"] <- getStartYear(simulation_years[, "simulationStartYear"])

			unique_simulation_years <- unique(simulation_years)
			#each row is unique so add id to db_combined
			if (nrow(unique_simulation_years) == nrow(simulation_years)) {
				id <- seq_len(nrow(unique_simulation_years))
			  unique_simulation_years <- cbind(id,
			    unique_simulation_years[, c("simulationStartYear", "StartYear", "EndYear")])
				db_combined_exp_treatments[, "simulation_years_id"] <- unique_simulation_years[, "id"]

			} else {#treatment table has a map to reduced rows in simulation_years
				temp <- duplicated(simulation_years)
				sim_years_unique_map <- rep(NA, nrow(simulation_years))
				temp2 <- data.frame(t(simulation_years))
				sim_years_unique_map[temp] <- match(data.frame(t(simulation_years[temp, ])), temp2)
				sim_years_unique_map[!temp] <- match(data.frame(t(simulation_years[!temp, ])), temp2)
				treatments_toYears_map <- unique(sim_years_unique_map)
				sim_years_unique_map <- sapply(sim_years_unique_map, function(x)
				  which(treatments_toYears_map == x))
				db_combined_exp_treatments[, "simulation_years_id"] <- sim_years_unique_map
			}
			#write to the database
			RSQLite::dbBegin(con_dbOut)
			RSQLite::dbGetPreparedQuery(con_dbOut, paste("INSERT INTO simulation_years",
			  "VALUES(NULL, :simulationStartYear, :StartYear, :EndYear);",
			  bind.data = data.frame(unique_simulation_years)))
			RSQLite::dbCommit(con_dbOut)

		} else {#Treatment option for simulation Years is turned off. Get the default one from settings.
			db_combined_exp_treatments$simulation_years_id <- 1
			temp <- data.frame(simulationStartYear = simstartyr, StartYear = startyr,
			  EndYear = endyr)
			RSQLite::dbBegin(con_dbOut)
			RSQLite::dbGetPreparedQuery(con_dbOut, paste("INSERT INTO simulation_years",
			  "VALUES(NULL, :simulationStartYear, :StartYear, :EndYear);"),
			  bind.data = temp)
			RSQLite::dbCommit(con_dbOut)
		}

		#Insert the data into the treatments table
		RSQLite::dbBegin(con_dbOut)
		RSQLite::dbGetPreparedQuery(con_dbOut, paste0("INSERT INTO treatments VALUES(",
		  paste0(":", colnames(db_combined_exp_treatments), collapse = ", "), ")"),
		  bind.data = db_combined_exp_treatments)
		RSQLite::dbCommit(con_dbOut)

		##############scenario_labels table###############
		RSQLite::dbGetQuery(con_dbOut, paste("CREATE TABLE",
		  "scenario_labels(id INTEGER PRIMARY KEY AUTOINCREMENT, label TEXT UNIQUE NOT NULL)"))
		RSQLite::dbBegin(con_dbOut)
		RSQLite::dbGetPreparedQuery(con_dbOut, paste("INSERT INTO scenario_labels",
		  "VALUES(NULL, :label);"),
		  bind.data = data.frame(label = climate.conditions, stringsAsFactors = FALSE))
		RSQLite::dbCommit(con_dbOut)
		##################################################

		#############run_labels table#########################
		# Note: invariant to 'include_YN', i.e., do not subset 'SWRunInformation'
		RSQLite::dbGetQuery(con_dbOut, paste("CREATE TABLE",
		  "run_labels(id INTEGER PRIMARY KEY AUTOINCREMENT, label TEXT UNIQUE NOT NULL);"))
		temp <- if (useExperimentals) {
        temp1 <- formatC(SWRunInformation[, "site_id"], width = digitsN_total,
          format = "d", flag = "0")
        temp2 <- rep(sw_input_experimentals[, "Label"], each = runsN_master)
        paste(temp1, temp2, SWRunInformation$Label, sep = "_")
      } else {
        SWRunInformation$Label
      }
    rs <- DBI::dbSendStatement(con_dbOut, "INSERT INTO run_labels VALUES(NULL, :label)",
      params = list(label = temp))
    stopifnot(DBI::dbHasCompleted(rs), DBI::dbClearResult(rs))
		##################################################


		#####################runs table###################
		# Note: invariant to 'include_YN', i.e., do not subset 'SWRunInformation'
		RSQLite::dbGetQuery(con_dbOut, paste("CREATE TABLE",
		  "runs(P_id INTEGER PRIMARY KEY, label_id INTEGER NOT NULL,",
		  "site_id INTEGER NOT NULL, treatment_id INTEGER NOT NULL,",
		  "scenario_id INTEGER NOT NULL, FOREIGN KEY(label_id) REFERENCES run_labels(id),",
		  "FOREIGN KEY(site_id) REFERENCES sites(id),",
		  "FOREIGN KEY(treatment_id) REFERENCES treatments(id),",
		  "FOREIGN KEY(scenario_id) REFERENCES scenario_labels(id));"))

		db_runs <- data.frame(matrix(data = 0, nrow = runsN_Pid, ncol = 5,
		  dimnames = list(NULL, c("P_id", "label_id", "site_id", "treatment_id",
		  "scenario_id"))))
		db_runs$P_id <- seq_len(runsN_Pid)
		db_runs$label_id <- rep(seq_len(runsN_total), each = scenario_No)
		db_runs$site_id <- rep(rep(SWRunInformation$site_id, times = max(expN, 1L)), each = scenario_No)
		db_runs$scenario_id <- rep(seq_len(scenario_No), times = runsN_total)

    temp <- if (useExperimentals) {
        as.vector(matrix(data = exp_start_rows, nrow = runsN_master,
          ncol = expN, byrow = TRUE))
      } else NULL

    db_runs$treatment_id <- if (useTreatments) {
        if (useExperimentals) {
          rep(temp + treatments_unique_map - 1, each = scenario_No)
        } else {
          rep(treatments_unique_map, each = scenario_No)
        }
      } else {
        if (useExperimentals) rep(temp, each = scenario_No) else 1
      }

		RSQLite::dbBegin(con_dbOut)
		RSQLite::dbGetPreparedQuery(con_dbOut, paste("INSERT INTO runs",
		  "VALUES(:P_id, :label_id, :site_id, :treatment_id, :scenario_id);"),
		  bind.data = db_runs)
		RSQLite::dbCommit(con_dbOut)
		##################################################

		################CREATE VIEW########################
		if (length(Index_RunInformation) > 0) {
			sites_columns <- colnames(SWRunInformation)[Index_RunInformation]

      for (k_excl in c("label", "WeatherFolder", "Include_YN")) {
        icol <- grep(k_excl, sites_columns, ignore.case = TRUE)
        if (length(icol) > 0)
          sites_columns <- sites_columns[-icol]
      }

		} else {
			sites_columns <- NULL
		}
		treatment_columns <- colnames(db_combined_exp_treatments)[-(1:3)]
		if (useTreatmentWeatherFolder)
			treatment_columns <- treatment_columns[-grep("WeatherFolder", treatment_columns)]
		header_columns <- paste(c(
				"runs.P_id",
				"run_labels.label AS Labels",
        "sites.Include_YN AS Include_YN",
				if (!is.null(sites_columns))
					paste0("sites.\"", sites_columns, "\"", collapse = ", "),
				if (useExperimentals)
					"experimental_labels.label AS Experimental_Label",
				"weatherfolders.folder AS WeatherFolder",
				if (useExperimentals || useTreatments)
					paste("treatments", treatment_columns, sep = ".", collapse = ", "),
				"simulation_years.StartYear",
				"simulation_years.simulationStartYear AS SimStartYear",
				"simulation_years.EndYear",
				"scenario_labels.label AS Scenario"),
			collapse = ", ")

		RSQLite::dbGetQuery(con_dbOut, paste0(
			"CREATE VIEW header AS SELECT ", header_columns, " FROM runs, run_labels, sites, ",
			if (useExperimentals)
				"experimental_labels, ",
			"treatments, scenario_labels, simulation_years, weatherfolders",
			" WHERE runs.label_id=run_labels.id AND runs.site_id=sites.id AND",
			" runs.treatment_id=treatments.id AND runs.scenario_id=scenario_labels.id AND ",
			if (useTreatmentWeatherFolder) {
				"treatments.LookupWeatherFolder_id=weatherfolders.id AND "
			} else {
				"sites.WeatherFolder_id=weatherfolders.id AND "
			},
			if (useExperimentals)
				"treatments.experimental_id=experimental_labels.id AND ",
			"treatments.simulation_years_id=simulation_years.id;"
		))
		##################################################

    invisible(NULL)
  }

  dbOutput_create_OverallAggregationTable <- function(con_dbOut, aon, daily_lyr_agg,
    SoilLayer_MaxNo, SWPcrit_MPa, Tmin_crit_C, Tmax_crit_C, Tmean_crit_C, bin.prcpSizes,
    bin.prcpfreeDurations, DegreeDayBase, st_mo, no.species_regeneration,
    param.species_regeneration) {
		## Note: All '.' will be translated to "_" because of sqlite field name constraints
		temp <- character(0)

		fieldtag_SWPcrit_MPa <- paste0(abs(round(-1000 * SWPcrit_MPa, 0)), "kPa")
		fieldtag_Tmin_crit_C <- paste0(ifelse(Tmin_crit_C < 0, "Neg",
		  ifelse(Tmin_crit_C > 0, "Pos", "")), abs(Tmin_crit_C), "C")
		fieldtag_Tmax_crit_C <- paste0(ifelse(Tmax_crit_C < 0, "Neg",
		  ifelse(Tmax_crit_C > 0, "Pos", "")), abs(Tmax_crit_C), "C")
		fieldtag_Tmean_crit_C <- paste0(ifelse(Tmean_crit_C < 0, "Neg",
		  ifelse(Tmean_crit_C > 0, "Pos", "")), abs(Tmean_crit_C), "C")

	#0.
		if (aon$input_SoilProfile) {
			temp <- paste("SWinput.Soil.", c("maxDepth_cm", "soilLayers_N", "topLayers.Sand_fraction", "bottomLayers.Sand_fraction", "topLayers.Clay_fraction", "bottomLayers.Clay_fraction", "topLayers.Gravel_fraction", "bottomLayers.Gravel_fraction","deltaX"), sep="")
		}

	#1.
		if (aon$input_FractionVegetationComposition) {
			temp <- c(temp, paste("SWinput.Composition.", c("Grasses", "Shrubs", "Trees", "Forbs", "BareGround", "C3ofGrasses", "C4ofGrasses", "AnnualsofGrasses"), "_fraction_const", sep=""))
		}
	#2.
		if (aon$input_VegetationBiomassMonthly) {
			temp <- c(temp, paste(c(rep("Grass",36),rep("Shrub",36),rep("Tree",36),rep("Forb",36)),"_",c(rep("Litter",12),rep("TotalBiomass",12),rep("LiveBiomass",12)),"_m", st_mo,"_gPERm2",sep=""))
		}
	#3.
		if (aon$input_VegetationPeak) {
			temp <- c(temp, paste("SWinput.PeakLiveBiomass_", c("month_mean","months_duration"), sep=""))
		}

	#4.
		if (aon$input_Phenology) {
			temp <- c(temp, paste("SWinput.GrowingSeason.", c("Start", "End"), "_month_const", sep=""))
		}
	#5.
		if (aon$input_TranspirationCoeff) {
			if (daily_lyr_agg[["do"]]) {
				ltemp <- paste("L0to", daily_lyr_agg[["first_cm"]], "cm", sep="")
				if (is.null(daily_lyr_agg[["second_cm"]])) {
					ltemp <- c(ltemp, paste("L", daily_lyr_agg[["first_cm"]], "toSoilDepth", sep=""))
				} else if (is.numeric(daily_lyr_agg[["second_cm"]])) {
					ltemp <- c(ltemp, paste("L", daily_lyr_agg[["first_cm"]], "to", daily_lyr_agg[["second_cm"]], "cm", sep=""))
				}
				if (is.null(daily_lyr_agg[["third_cm"]])) {
					ltemp <- c(ltemp, paste("L", daily_lyr_agg[["second_cm"]], "toSoilDepth", sep=""))
				} else if (is.na(daily_lyr_agg[["third_cm"]])) {
				} else if (is.numeric(daily_lyr_agg[["third_cm"]])) {
					ltemp <- c(ltemp, paste("L", daily_lyr_agg[["second_cm"]], "to", daily_lyr_agg[["third_cm"]], "cm", sep=""))
				}
				if (is.null(daily_lyr_agg[["fourth_cm"]])) {
					ltemp <- c(ltemp, paste("L", daily_lyr_agg[["third_cm"]], "toSoilDepth", sep=""))
				} else if (is.na(daily_lyr_agg[["fourth_cm"]])) {
				} else if (is.numeric(daily_lyr_agg[["fourth_cm"]])) {
					ltemp <- c(ltemp, paste("L", daily_lyr_agg[["third_cm"]], "to", daily_lyr_agg[["fourth_cm"]], "cm", sep=""))
				}
				ltemp <- c(ltemp, paste("NA", (length(ltemp)+1):SoilLayer_MaxNo, sep=""))
			} else {
				ltemp <- paste("L", formatC(lmax, width=2, format="d", flag="0"), sep="")
			}

			temp <- c(temp, c(paste("SWinput.", rep(vtemp <- c("Grass", "Shrub", "Tree","Forb"), each=SoilLayer_MaxNo), ".TranspirationCoefficients.", rep(ltemp, times=4), "_fraction", sep=""), paste("SWinput.", rep(vtemp, each=2), ".TranspirationCoefficients.", rep(c("topLayer", "bottomLayer"), times=4), "_fraction", sep="")))

		}

	#6.
		if (aon$input_ClimatePerturbations) {
			temp <- c(temp, paste(rep(paste("SWinput.ClimatePerturbations.", c("PrcpMultiplier.m", "TmaxAddand.m", "TminAddand.m"), sep=""), each=12), st_mo, rep(c("_none", "_C", "_C"), each=12), "_const", sep=""))
		}

		##############################################################---Aggregation: Climate and weather---##############################################################

	#7.
		if (aon$yearlyTemp) {
			temp <- c(temp, "MAT_C_mean")
		}

	#8.
		if (aon$yearlyPPT) {
			temp <- c(temp, c("MAP_mm_mean", "SnowOfPPT_fraction_mean"))
		}

	#9.
		if (aon$dailySnowpack) {
			temp <- c(temp, "RainOnSnowOfMAP_fraction_mean")
		}

	#10.
		if (aon$dailySnowpack) {
			temp <- c(temp, paste("Snowcover.NSadj.", c("Peak_doy", "LongestContinuous.LastDay_doy", "LongestContinuous.Duration_days", "Total_days", "Peak_mmSWE"), "_mean", sep=""))
		}
	#11
		if (aon$dailyFrostInSnowfreePeriod) {
			temp <- c(temp, paste0("TminBelow", fieldtag_Tmin_crit_C, "withoutSnowpack_days_mean"))
		}
	#12
		if (aon$dailyHotDays) {
			temp <- c(temp, paste0("TmaxAbove", fieldtag_Tmax_crit_C, "_days_mean"))
		}
	#12b
		if (aon$dailyWarmDays) {
		  temp <- c(temp, paste0("TmeanAbove", fieldtag_Tmean_crit_C, "_days_mean"))
		}
	#13
		if (aon$dailyPrecipitationEventSizeDistribution) {
			bins.summary <- (0:6) * bin.prcpSizes
			temp <- c(temp, paste("PrcpEvents.Annual", c("_count", paste(".SizeClass", bins.summary, "to", c(bins.summary[-1], "Inf"), "mm_fraction", sep="")), "_mean", sep=""))
		}

	#15
		if (aon$yearlyPET) {
			temp <- c(temp, "PET_mm_mean")
		}

	#16
		if (aon$monthlySeasonalityIndices) {
			temp <- c(temp, paste("Seasonality.monthly", c("PETandSWPtopLayers", "PETandSWPbottomLayers", "TandPPT"), "_PearsonCor_mean", sep=""))
		}


				#---Aggregation: Climatic dryness
	#17
		if (aon$yearlymonthlyTemperateDrylandIndices) {
			temp <- c(temp, paste(c(paste(temp <- c("UNAridityIndex", "TrewarthaD", "TemperateDryland12"), ".Normals", sep=""), paste(temp, ".Annual", sep="")), rep(c("_none", "_TF", "_TF"), times=2), "_mean", sep=""))
		}

	#18
		if (aon$yearlyDryWetPeriods) {
			temp <- c(temp, paste(c("Dry", "Wet"), "SpellDuration.90PercentEvents.ShorterThan_years_quantile0.9", sep=""))
		}

	#19
		if (aon$dailyWeatherGeneratorCharacteristics) {
			temp <- c(temp, paste(rep(c("WetSpellDuration", "DrySpellDuration", "TempAir.StDevOfDailyValues"), each=12), ".m", st_mo, rep(c("_days", "_days", "_C"), each=12), "_mean", sep=""))
		}

	#20
		if (aon$dailyPrecipitationFreeEventDistribution) {
			bins.summary <- (0:3) * bin.prcpfreeDurations
			temp <- c(temp, paste("DrySpells.Annual", c("_count", paste(".SizeClass", bins.summary+1, "to", c(bins.summary[-1], "365"), "days_fraction", sep="")), "_mean", sep=""))
		}

	#21
		if (aon$monthlySPEIEvents) {
			binSPEI_m <- c(1, 12, 24, 48) #months
			probs <- c(0.025, 0.5, 0.975)
			for (iscale in seq_along(binSPEI_m)) {
				rvec <- rep(NA, times=4 * length(probs))
				temp <- c(temp, paste(rep(paste("SPEI.", binSPEI_m[iscale], "monthsScale.", sep=""), length(rvec)), "Spell", rep(c("Pos.", "Neg."), each=2*length(probs)), rep(rep(c("Duration_months", "Value_none"), each=length(probs)), times=2), "_quantile", rep(probs, times=4), sep=""))

			}
		}

	#---Aggregation: Climatic control
	#22
		if (aon$monthlyPlantGrowthControls) {
			temp <- c(temp, paste("NemaniEtAl2003.NPPControl.", c("Temperature", "Water", "Radiation"), "_none_mean", sep=""))
		}

	#23
		if (aon$dailyC4_TempVar) {
			temp <- c(temp, paste("TeeriEtAl1976.NSadj.", c("TempAirMin.7thMonth_C", "FreezeFreeGrowingPeriod_days", "AccumDegreeDaysAbove65F_daysC"), "_mean", sep=""))
		}

	#24
		if (aon$dailyDegreeDays) {
			temp <- c(temp, paste("DegreeDays.Base", DegreeDayBase, "C.dailyTmean_Cdays_mean", sep=""))
		}

		##############################################################---Aggregation: Yearly water balance---##############################################################

	#27.0
		if (aon$yearlyAET) {
			temp <- c(temp, "AET_mm_mean")
		}

	#27
		if (aon$yearlyWaterBalanceFluxes) {
			temp <- c(temp, paste(c("Rain_mm", "Rain.ReachingSoil_mm", "Snowfall_mm", "Snowmelt_mm", "Snowloss_mm", "Interception.Total_mm", "Interception.Vegetation_mm", "Interception.Litter_mm", "Evaporation.InterceptedByVegetation_mm", "Evaporation.InterceptedByLitter_mm", "Infiltration_mm", "Runoff_mm", "Evaporation.Total_mm", "Evaporation.Soil.Total_mm", "Evaporation.Soil.topLayers_mm",
									"Evaporation.Soil.bottomLayers_mm", "Transpiration.Total_mm", "Transpiration.topLayers_mm", "Transpiration.bottomLayers_mm", "HydraulicRedistribution.TopToBottom_mm", "Percolation.TopToBottom_mm", "DeepDrainage_mm", "SWC.StorageChange_mm", "TranspirationBottomToTranspirationTotal_fraction", "TtoAET", "EStoAET", "AETtoPET", "TtoPET", "EStoPET"), "_mean", sep=""))
		}


	#27.2
		if (aon$dailySoilWaterPulseVsStorage) {
			temp <- c(temp, paste0("WaterExtractionSpell_MeanContinuousDuration_L", lmax, "_days_mean"),
							paste0("WaterExtractionSpell_AnnualSummedExtraction_L", lmax, "_mm_mean"))
		}

		##############################################################---Aggregation: Daily extreme values---##############################################################
	#28
		if (aon$dailyTranspirationExtremes) {
			temp <- c(temp, paste("Transpiration.", c("DailyMax", "DailyMin"), "_mm_mean", sep=""), paste("Transpiration.", c("DailyMax", "DailyMin"), "_doy_mean", sep=""))
		}

	#29
		if (aon$dailyTotalEvaporationExtremes) {
			temp <- c(temp, paste("Evaporation.Total.", c("DailyMax", "DailyMin"), "_mm_mean", sep=""), paste("Evaporation.Total.", c("DailyMax", "DailyMin"), "_doy_mean", sep=""))
		}

	#30
		if (aon$dailyDrainageExtremes) {
			temp <- c(temp, paste("DeepDrainage.", c("DailyMax", "DailyMin"), "_mm_mean", sep=""), paste("DeepDrainage.", c("DailyMax", "DailyMin"), "_doy_mean", sep=""))
		}

	#31
		if (aon$dailyInfiltrationExtremes) {
			temp <- c(temp, paste("Infiltration.", c("DailyMax", "DailyMin"), "_mm_mean", sep=""), paste("Infiltration.", c("DailyMax", "DailyMin"), "_doy_mean", sep=""))
		}

	#32
		if (aon$dailyAETExtremes) {
			temp <- c(temp, paste("AET.", c("DailyMax", "DailyMin"), "_mm_mean", sep=""), paste("AET.", c("DailyMax", "DailyMin"), "_doy_mean", sep=""))
		}

	#33
		if (aon$dailySWPextremes) {
			temp <- c(temp, paste(paste("SWP.", rep(c("topLayers.", "bottomLayers."), each=2), rep(c("DailyMax", "DailyMin"), times=2), sep=""), rep(c("_MPa_mean", "_doy_mean"), each=4), sep=""))
		}
	#34
		if (aon$dailyRechargeExtremes) {
			temp <- c(temp, paste(paste("RelRecharge.", rep(c("topLayers.", "bottomLayers."), each=2), rep(c("DailyMax", "DailyMin"), times=2), sep=""), rep(c("_Fraction_mean", "_doy_mean"), each=4), sep=""))
		}


		##############################################################---Aggregation: Ecological dryness---##############################################################

	#35a
  if (aon$dailyNRCS_SoilMoistureTemperatureRegimes_Intermediates) {
      # abbreviations:
      #     - GT = greater than; LT = less than; EQ = equal
      #     - MCS = MoistureControlSection; ACS = AnhydrousControlSection
      #     - consec = consecutive
      temp <- c(temp,
        paste0("NRCS_",
          c(c("Depth50cmOrImpermeable_cm",
              "MCS_Upper_cm", "MCS_Lower_cm",
              "ACS_Upper_cm", "ACS_Lower_cm",
              "Permafrost_years", "SMR_normalyears_N", "Soil_with_Ohorizon_TF"),
            paste0(c("SoilTemp_ACS_Annual_C", "SoilTemp_at50cm_Annual_C", # MATLanh, MAT50
                      "SoilTemp_at50cm_JJA_C", "SoilTemp_at50cm_DJF_C", # T50jja, T50djf
                      "Saturation_ConsecutiveMaxDuration_JJA_days", # CSPartSummer
                      "SoilTemp_Offset_from_MeanAirTemp_C", # meanTair_Tsoil50_offset_C
                    # Anhydrous_annual_means:
                      "COND1_ACS_at50cm_LE0C_prob", # COND1
                      "COND2_ACS_atAnhDepth_LE5C_prob", # COND2
                      "COND3_ACS_MoreThanHalfDry_and_at50cm_GT0C_isGThalf_at50cm_GT0C_prob", # COND3
                      "COND3_ACS_MoreThanHalfDry_and_at50cm_GT0C_days", # HalfDryDaysCumAbove0C
                      "COND3_ACS_at50cm_GT0C_days", # SoilAbove0C
                      "COND3_ACS_at50cm_GT0C_prob", # T50_at0C
                      "COND3_ACS_MoreThanHalfDry_prob", # Lanh_Dry_Half
                      "COND3_ACS_MoreThanHalfDry_and_at50cm_GT0C_prob", # COND3_Test
                     # MCS_annual_means:
                      "COND0_mPPT_GT_mPET_prob", # COND0
                      "COND1_MCS_AllDry_and_at50cm_GT5C_days", # DryDaysCumAbove5C
                      "COND1_MCS_at50cm_GT5C_days", # SoilAbove5C
                      "COND1_MCS_AllDry_and_at50cm_GT5C_isGThalf_at50cm_GT5C_prob", # COND1
                      "COND2_MCS_AnyWetConsec_Max_at50cm_GT8C_days", # MaxContDaysAnyMoistCumAbove8
                      "COND2_MCS_AnyWetConsec_LT90Days_at50cm_GT8C_prob", # COND2
                      "COND2-1_MCS_AnyWetConsec_LT180Days_at50cm_GT8C_prob", # COND2_1
                      "COND2-2_MCS_AnyWetConsec_LT270Days_at50cm_GT8C_prob", # COND2_2
                      "COND2-3_MCS_AnyWetConsec_LE45Days_at50cm_GT8C_prob", # COND2_3
                      "COND3_MCS_AnyDry_days", # DryDaysCumAny
                      "COND3_MCS_AnyDryTotal_LT90Days_prob", # COND3
                      "COND3-1_MCS_AnyDryTotal_LT30Days_prob", # COND3_1
                      "COND4_MCS_at50cm_GT22C_prob", # COND4
                      "COND5_MCS_at50cm_DiffJJAtoDJF_C", # AbsDiffSoilTemp_DJFvsJJA
                      "COND5_MCS_at50cm_DiffJJAtoDJF_GT6C_prob", # COND5
                      "COND6_MCS_AllDry_Summer_days",  # DryDaysConsecSummer
                      "COND6_MCS_AllDry_Summer_LT45Days_prob", # COND6
                      "COND6-1_MCS_AllDry_Summer_GT90Days_prob", # COND6_1
                      "COND7_MCS_AnyMoist_GT180Days_days", # MoistDaysCumAny
                      "COND7_MCS_AnyMoist_GT180Days_prob", # COND7
                      "COND8_MCS_AnyWetConsec_days", # MoistDaysConsecAny
                      "COND8_MCS_AnyWetConsec_GT90Days_prob", # COND8
                      "COND9_MCS_AllWet_Winter_days", # MoistDaysConsecWinter
                      "COND9_MCS_AllWet_Winter_GT45days_prob", # COND9
                      "COND10_MCS_AllDry_days", # AllDryDaysCumAny
                      "COND10_MCS_AllDry_prob", # COND10

                      "Days_at50cm_GT5C_prob", "Days_at50cm_GT8C_prob",
                      "Days_MCS_AllWet_prob",
                      "COND1_MCS_AllDry_and_at50cm_GT5C_prob", # COND1_Test
                      "COND2_MCS_AnyWet_and_at50cm_GT8C_prob"), # COND2_Test
                    "_mean"))))
    }
  if (aon$dailyNRCS_SoilMoistureTemperatureRegimes) {
      # abbreviations:
      #     - GT = greater than; LT = less than; EQ = equal
      #     - MCS = MoistureControlSection; ACS = AnhydrousControlSection
      #     - consec = consecutive
      temp <- c(temp, paste0("NRCS_",
                c(paste0("SoilTemperatureRegime_", STR_names()),
                  paste0("SoilMoistureRegime_", SMR_names()),
                  paste0("SoilMoistureRegimeQualifier_", SMRq_names()))))
    }
	#35b
    if (aon$dailyNRCS_Chambers2014_ResilienceResistance) {
      cats <- c("Low", "ModeratelyLow", "Moderate", "ModeratelyHigh", "High")
      temp <- c(temp, paste0("NRCS_Chambers2014_Sagebrush",
                            rep(c("Resilience", "Resistance"), each = length(cats)),
                            "_", cats))
    }

    #35c
    if (aon$dailyNRCS_Maestas2016_ResilienceResistance) {
      temp <- c(temp, paste0("NRCS_Maestas2016_SagebrushRR_", c("Low", "Moderate", "High")))
    }

	#35.2
		if (aon$dailyWetDegreeDays) {
			temp <- c(temp, paste("WetDegreeDays.SWPcrit", rep(fieldtag_SWPcrit_MPa, each=3), rep(c(".topLayers", ".bottomLayers", ".anyLayer"), times=length(SWPcrit_MPa)), "_Cdays_mean", sep=""))
		}

	#35.3
		if (aon$dailyThermalDrynessStartEnd) {
		  temp <- c(temp, paste0("ThermalDrySoilPeriods_SWPcrit",
							rep(fieldtag_SWPcrit_MPa, each = 4),
							"_NSadj_",
							rep(c("topLayers", "bottomLayers"), each = 2), "_",
							rep(c("Start", "End"), times = 2),
							"_LongestContinuous_days_mean"))
		}

	#35.4
		if (aon$dailyThermalSWPConditionCount) {
		  temp <- c(temp, paste0("SoilPeriods_Warm",
							rep(paste0(rep(c("Dry", "Wet"), times = 3), "_",
								rep(c("allLayers", "topLayer", "bottomLayer"), each = 2)),
								each = length(Tmean_crit_C) * length(SWPcrit_MPa)),
							"_Tcrit", rep(fieldtag_Tmean_crit_C, times = length(SWPcrit_MPa)),
							"_SWPcrit", rep(fieldtag_SWPcrit_MPa, each = length(Tmean_crit_C)),
							"_Count_days_mean"))
		}

	#36
		if (aon$monthlySWPdryness) {
			temp <- c(temp, paste("DrySoilPeriods.SWPcrit", rep(fieldtag_SWPcrit_MPa, times=2), ".NSadj.", rep(c("topLayers", "bottomLayers"), each=length(SWPcrit_MPa)), ".Duration.Total_months_mean", sep=""),
					paste("DrySoilPeriods.SWPcrit", rep(fieldtag_SWPcrit_MPa, times=2), ".NSadj.", rep(c("topLayers", "bottomLayers"), each=length(SWPcrit_MPa)), ".Start_month_mean", sep=""))
		}

	#37
		if (aon$dailySWPdrynessANDwetness) {
			temp <- c(temp, paste(rep(c("WetSoilPeriods", "DrySoilPeriods"), each=8), ".SWPcrit", rep(fieldtag_SWPcrit_MPa, each=16), ".NSadj.", c(rep(c("topLayers", "bottomLayers"), times=4), rep(rep(c("topLayers", "bottomLayers"), each=2), times=2)),
							rep(c(".AnyLayerWet.", ".AllLayersWet.", ".AllLayersDry.", ""), each=4), c(rep(rep(c("Duration.Total_days", "Duration.LongestContinuous_days"), each=2), times=2), rep(c("Duration.Total_days", "Duration.LongestContinuous_days"), times=2), rep(c(".PeriodsForAtLeast10Days.Start_doy", ".PeriodsForAtLeast10Days.End_doy"), times=2)), "_mean", sep=""))
		}

	#38
		if (aon$dailySuitablePeriodsDuration) {
			quantiles <- c(0.05, 0.5, 0.95)
			temp <- c(temp, paste("ThermalSnowfreeWetPeriods.SWPcrit", rep(paste(rep(fieldtag_SWPcrit_MPa, each=2), rep(c(".topLayers", ".bottomLayers"), times=length(SWPcrit_MPa)), sep=""), each=length(quantiles)), "_Duration_days_quantile", rep(quantiles, times=2), sep=""))
		}
	#39
		if (aon$dailySuitablePeriodsAvailableWater) {
			temp <- c(temp, paste("ThermalSnowfreeWetPeriods.SWPcrit", rep(fieldtag_SWPcrit_MPa, each=2), rep(c(".topLayers", ".bottomLayers"), times=length(SWPcrit_MPa)), "_AvailableWater_mm_mean", sep=""))
		}
	#40
		if (aon$dailySuitablePeriodsDrySpells) {
			temp <- c(temp, paste("ThermalSnowfreeDryPeriods.SWPcrit", rep(paste(rep(fieldtag_SWPcrit_MPa, each=2), rep(c(".topLayers", ".bottomLayers"), times=length(SWPcrit_MPa)), sep=""), each=4), c("_DrySpellsAllLayers_meanDuration_days_mean", "_DrySpellsAllLayers_maxDuration_days_mean", "_DrySpellsAllLayers_Total_days_mean", "_DrySpellsAtLeast10DaysAllLayers_Start_doy_mean"), sep=""))
		}
	#41
		if (aon$dailySWPdrynessDurationDistribution) {
			deciles <- (0:10)*10/100
			quantiles <- (0:4)/4
			mo_seasons <- matrix(data=c(12,1:11), ncol=3, nrow=4, byrow=TRUE)
			season.flag <- c("DJF", "MAM", "JJA", "SON")

			temp <- c(temp, paste0("DrySoilPeriods.SWPcrit",
								rep(rep(fieldtag_SWPcrit_MPa, each = 2 * length(quantiles)), times = length(season.flag)),
								".Month",
								rep(season.flag, each = 2 * length(quantiles) * length(SWPcrit_MPa)), ".",
								rep(rep(paste0(rep(c("topLayers", "bottomLayers"), each = length(quantiles)),
									".Duration_days_quantile",
									rep(quantiles, times = 2)), times = length(SWPcrit_MPa)),
								times = length(season.flag))))
		}

	#42
		if (aon$dailySWPdrynessEventSizeDistribution) {
			binSize <- c(1, 8, 15, 29, 57, 183, 367) #closed interval lengths in [days] within a year; NOTE: n_variables is set for binsN == 4
			binsN <- length(binSize) - 1
			binTitle <- paste("SizeClass", paste(binSize[-length(binSize)], binSize[-1]-1, sep="to") ,"days", sep="")

			temp <- c(temp, paste0("DrySoilPeriods.SWPcrit",
								rep(fieldtag_SWPcrit_MPa, each = 2 * (binsN + 1)),
								".Annual.",
								rep(c("topLayers", "bottomLayers"), each = binsN + 1),
								rep(c("_count", paste0(".", binTitle, "_fraction")), times = 2),
								"_mean"))
		}

	#43
		if (aon$dailySWPdrynessIntensity) {
			temp <- c(temp, paste0("DrySoilPeriods.SWPcrit",
								rep(fieldtag_SWPcrit_MPa, each = 4 * 2),
								".MissingWater.",
								rep(c("topLayers", "bottomLayers"), each = 4), ".",
								rep(c("AnnualSum_mmH2O", "PerEventPerDay_mmH2O", "Duration.Event_days", "Events_count"), times = 2),
								"_mean"))
		}

	#43.2
		if (aon$dailyThermalDrynessStress) {
			temp <- c(temp,
						paste0("Mean10HottestDays_VPD_kPa",
							c("_mean", "_max",
							paste0(paste0("_MoistureStress_",
									"SWPcrit", rep(fieldtag_SWPcrit_MPa, times = 3), "_",
									rep(rep(c("allLayers", "topLayer", "bottomLayer"), each = length(SWPcrit_MPa)), each = 2)
								),
								rep(c("_mean", "_max"), each = length(SWPcrit_MPa))))))

		}

		##############################################################---Aggregation: Mean monthly values---##############################################################

	#44
		if (aon$monthlyTemp) {
			temp <- c(temp, paste("TempAir.m", st_mo, "_C_mean", sep=""))
		}

	#45
		if (aon$monthlyPPT) {
			temp <- c(temp, paste("Precip.m", st_mo, "_mm_mean", sep=""))
		}

	#46
		if (aon$monthlySnowpack) {
			temp <- c(temp, paste("Snowpack.m", st_mo, "_mmSWE_mean", sep=""))
		}

	#47
		if (aon$monthlySoilTemp) {
			temp <- c(temp, paste("TempSoil.", c(paste("topLayers.m", st_mo, sep=""), paste("bottomLayers.m", st_mo, sep="")), "_C_mean", sep=""))
		}

	#48
		if (aon$monthlyRunoff) {
			temp <- c(temp, paste("Runoff.Total.m", st_mo, "_mm_mean", sep=""))
		}

	#49
		if (aon$monthlyHydraulicRedistribution) {
			temp <- c(temp, paste("HydraulicRedistribution.", c(paste("topLayers.m", st_mo, sep=""), paste("bottomLayers.m", st_mo, sep="")), "_mm_mean", sep=""))
		}

	#50
		if (aon$monthlyInfiltration) {
			temp <- c(temp, paste("Infiltration.m", st_mo, "_mm_mean", sep=""))
		}

	#51
		if (aon$monthlyDeepDrainage) {
			temp <- c(temp, paste("DeepDrainage.m", st_mo, "_mm_mean", sep=""))
		}

	#52
		if (aon$monthlySWPmatric) {
			temp <- c(temp, paste("SWPmatric.", c(paste("topLayers.m", st_mo, sep=""), paste("bottomLayers.m", st_mo, sep="")), "_MPa_FromVWCmean", sep=""))
		}

	#53 a.)
		if (aon$monthlyVWCbulk) {
			temp <- c(temp, paste("VWCbulk.", c(paste("topLayers.m", st_mo, sep=""), paste("bottomLayers.m", st_mo, sep="")), "_mPERm_mean", sep=""))
		}
	#53 b.)
		if (aon$monthlyVWCmatric) {
			temp <- c(temp, paste("VWCmatric.", c(paste("topLayers.m", st_mo, sep=""), paste("bottomLayers.m", st_mo, sep="")), "_mPERm_mean", sep=""))
		}

	#54
		if (aon$monthlySWCbulk) {
			temp <- c(temp, paste("SWCbulk.", c(paste("topLayers.m", st_mo, sep=""), paste("bottomLayers.m", st_mo, sep="")), "_mm_mean", sep=""))
		}

	#55
		if (aon$monthlySWAbulk) {
			temp <- c(temp, paste0("SWAbulk_",
								"SWPcrit", rep(fieldtag_SWPcrit_MPa, each = 24), "_",
								c(paste0("topLayers_m", st_mo), paste0("bottomLayers_m", st_mo)),
								"_mm_mean"))
		}

	#56
		if (aon$monthlyTranspiration) {
			temp <- c(temp, paste("Transpiration.", c(paste("topLayers.m", st_mo, sep=""), paste("bottomLayers.m", st_mo, sep="")), "_mm_mean", sep=""))
		}

	#57
		if (aon$monthlySoilEvaporation) {
			temp <- c(temp, paste("Evaporation.Soil.m", st_mo, "_mm_mean", sep=""))
		}

	#58
		if (aon$monthlyAET) {
			temp <- c(temp, paste("AET.m", st_mo, "_mm_mean", sep=""))
		}

	#59
		if (aon$monthlyPET) {
			temp <- c(temp, paste("PET.m", st_mo, "_mm_mean", sep=""))
		}

	#59.2
		if (aon$monthlyVPD) {
			temp <- c(temp, paste0("VPD_m", st_mo, "_kPa_mean"))
		}

	#60
		if (aon$monthlyAETratios) {
			temp <- c(temp, paste(rep(c("TranspToAET.m", "EvapSoilToAET.m"), each=12), st_mo, "_fraction_mean", sep=""))
		}

	#61
		if (aon$monthlyPETratios) {
			temp <- c(temp, paste(rep(c("TranspToPET.m", "EvapSoilToPET.m"), each=12), st_mo, "_fraction_mean", sep=""))
		}

		##############################################################---Aggregation: Potential regeneration---##############################################################

	#62
		if (aon$dailyRegeneration_bySWPSnow) {
			temp <- c(temp, "Regeneration.Potential.SuitableYears.NSadj_fraction_mean")
		}

	#63
		if (aon$dailyRegeneration_GISSM && no.species_regeneration > 0) {
			for (sp in 1:no.species_regeneration) {
				SeedlingMortality_CausesByYear_colnames <- paste("Seedlings1stSeason.Mortality.", c("UnderneathSnowCover", "ByTmin", "ByTmax", "ByChronicSWPMax", "ByChronicSWPMin", "ByAcuteSWPMin",
						"DuringStoppedGrowth.DueSnowCover", "DuringStoppedGrowth.DueTmin", "DuringStoppedGrowth.DueTmax"), sep="")

				temp.header1 <- c(paste(temp1 <- c("Germination", "Seedlings1stSeason"), ".SuitableYears_fraction_mean", sep=""),
						paste(rep(temp1, each=3), ".UnsuitableYears.Successive_years_quantile", rep(c(0.05, 0.5, 0.95), times=2), sep=""),
						paste(temp1, ".SuitableDaysPerYear_days_mean", sep=""),
						paste(paste(rep(temp1, each=3), ".", c("Start", "Middle", "End"), sep=""), "_doy_quantile", rep(c(0.9, 0.5, 0.9), times=2), sep=""),
						paste("Germination.RestrictedDays.By", c("Tmax", "Tmin", "SWPmin", "AnyCondition", "TimeToGerminate"), "_days_mean", sep=""),
						"Germination.TimeToGerminate_days_mean",
						paste(SeedlingMortality_CausesByYear_colnames, "_days_mean", sep=""))

				temp <- c(temp, paste(colnames(param.species_regeneration)[sp], temp.header1, sep="."))

				#Output for time series: not yet implemented for db
			}
		}

		#---Aggregation: done with options

		#Convert '.' to "_"
		temp <- gsub(".", "_", temp, fixed = TRUE)

		dbOverallColumns <- length(temp)

		if (dbOverallColumns > 0)
			temp <- paste(paste0("\"", temp, "\""), " REAL", collapse = ", ")

		meanString <- paste(c("\"P_id\" INTEGER PRIMARY KEY", temp), collapse = ", ")
		sdString <- paste(c("\"P_id\" INTEGER PRIMARY KEY", gsub("_mean", "_sd", temp)),
		  collapse = ", ")

		SQL_Table_Definitions1 <- paste0("CREATE TABLE \"aggregation_overall_mean\" (",
		  meanString, ");")
		SQL_Table_Definitions2 <- paste0("CREATE TABLE \"aggregation_overall_sd\" (",
		  sdString, ");")

		rs <- RSQLite::dbGetQuery(con_dbOut, paste(SQL_Table_Definitions1, collapse = "\n"))
		rs <- RSQLite::dbGetQuery(con_dbOut, paste(SQL_Table_Definitions2, collapse = "\n"))

    dbOverallColumns
  }

  dbOutput_create_DailyAggregationTable <- function(con_dbOut, output_aggregate_daily,
    daily_no) {

		if (!is.null(output_aggregate_daily)) {
			doy_colnames <- paste0("doy", formatC(seq_len(366), width = 3, format = "d",
			  flag = "0"))
			doy_colnames <- paste(paste0("\"", doy_colnames, "\""), " REAL", collapse = ", ")

			dailySQL <- paste(c("\"P_id\" INTEGER PRIMARY KEY", doy_colnames), collapse = ", ")
			dailyLayersSQL <- paste(c("\"P_id\" INTEGER", "\"Soil_Layer\" INTEGER",
			  doy_colnames, "PRIMARY KEY (\"P_id\",\"Soil_Layer\")"), collapse = ", ")

			if (daily_no > 0) {
				for (doi in seq_len(daily_no)) {
					if (regexpr("SWAbulk", output_aggregate_daily[doi]) > 0) {
						agg.resp <- "SWAbulk"
						#index.SWPcrit <- -as.numeric(sub("kPa", "", sub("SWAatSWPcrit", "", output_aggregate_daily[doi])))/1000
					} else {
						agg.resp <- output_aggregate_daily[doi]
					}
					#"VWCbulk","VWCmatric", "SWCbulk", "SWPmatric","SWAbulk"
					agg.analysis <- switch(EXPR = agg.resp,
					  AET = 1, Transpiration = 2, EvaporationSoil = 1, EvaporationSurface = 1,
					  EvaporationTotal = 1, VWCbulk = 2, VWCmatric = 2, SWCbulk = 2, SWPmatric = 2,
					  SWAbulk = 2, Snowpack = 1, Rain = 1, Snowfall = 1, Snowmelt = 1, SnowLoss = 1,
					  Infiltration = 1, DeepDrainage = 1, PET = 1, TotalPrecipitation = 1,
					  TemperatureMin = 1, TemperatureMax = 1, SoilTemperature = 2, Runoff = 1)
					tableName <- paste0("aggregation_doy_", output_aggregate_daily[doi])

					if (agg.analysis == 1) {
						SQL_Table_Definitions1 <- paste0("CREATE TABLE \"", tableName, "_Mean\" (",
						  dailySQL, ");")
						SQL_Table_Definitions2 <- paste0("CREATE TABLE \"", tableName, "_SD\" (",
						  dailySQL, ");")

					} else {
						SQL_Table_Definitions1 <- paste0("CREATE TABLE \"", tableName, "_Mean\" (",
						  dailyLayersSQL, ");")
						SQL_Table_Definitions2 <- paste0("CREATE TABLE \"", tableName, "_SD\" (",
						  dailyLayersSQL, ");")
					}

          rs <- RSQLite::dbSendQuery(con_dbOut, paste(SQL_Table_Definitions1,
            collapse = "\n"))
          RSQLite::dbClearResult(rs)
          rs <- RSQLite::dbSendQuery(con_dbOut, paste(SQL_Table_Definitions2,
            collapse = "\n"))
          RSQLite::dbClearResult(rs)

				}
			}
		}

		invisible(NULL)
	}


#' @section: NOTE: Do not change the design of the output database without adjusting the index
#'   functions 'it_Pid', 'it_exp', and 'it_site' (see part 4)
make_dbOutput <- function(dbOutput, SWRunInformation, Index_RunInformation,
    runsN_master, runIDs_sites, runsN_Pid, runsN_total, scenario_No, expN,
    create_treatments, create_experimentals, sw_input_treatments, sw_input_treatments_use,
    sw_input_experimentals, climate.conditions, simstartyr, startyr, endyr,
    digitsN_total, aon, daily_no, daily_lyr_agg, output_aggregate_daily, SoilLayer_MaxNo,
    SWPcrit_MPa, Tmin_crit_C, Tmax_crit_C, Tmean_crit_C, bin.prcpSizes,
    bin.prcpfreeDurations, DegreeDayBase, st_mo, no.species_regeneration,
    param.species_regeneration, do_clean) {

  if (do_clean && file.exists(dbOutput)) {
    unlink(dbOutput)
  }

  con_dbOut <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbOutput)
  set_PRAGMAs(con_dbOut, PRAGMA_settings2())

  n_tables <- RSQLite::dbListTables(con_dbOut)
  if (length(n_tables) > 0 && n_tables > 0)
    return(n_tables)

  dbOutput_create_Design(con_dbOut, SWRunInformation, Index_RunInformation,
    runsN_master, runIDs_sites, runsN_Pid, runsN_total, scenario_No, expN,
    create_treatments, create_experimentals, sw_input_treatments, sw_input_treatments_use,
    sw_input_experimentals, climate.conditions, simstartyr, startyr, endyr,
    digitsN_total)

  dbOverallColumns <- dbOutput_create_OverallAggregationTable(con_dbOut, aon,
    daily_lyr_agg, SoilLayer_MaxNo, SWPcrit_MPa, Tmin_crit_C, Tmax_crit_C, Tmean_crit_C,
    bin.prcpSizes, bin.prcpfreeDurations, DegreeDayBase, st_mo, no.species_regeneration,
    param.species_regeneration)

  dbOutput_create_DailyAggregationTable(con_dbOut, output_aggregate_daily, daily_no)


  ##########################################ENSEMBLE GENERATION#################################################
  #&& ((do_clean && (temp <- length(list.files(dir_out, pattern="dbEnsemble_"))) > 0) || !do_clean && temp == 0)
  Tables <- dbOutput_ListOutputTables(con_dbOut)
  RSQLite::dbDisconnect(con_dbOut)

  if (FALSE && do.ensembles) {
    Tables <- Tables[-grep(pattern="_sd", Tables, ignore.case = T)]
    Tables <- sub(pattern="_Mean",replacement="",x=Tables,ignore.case = T)
    respName<-sub(pattern="aggregation_",replacement="",x=Tables,ignore.case = T)
    respName<-sub(pattern="doy_",replacement="",x=respName,ignore.case = T)
    respName<-sub(pattern="atSWPcrit[0-9]+kPa",replacement="",x=respName)

    dbEnsemblesFilePaths <- file.path(dir_out, paste("dbEnsemble_",Tables,".sqlite3",sep=""))
    for (i in seq_along(dbEnsemblesFilePaths)) {
      if (do_clean && file.exists(dbEnsemblesFilePaths[i])) {
        unlink(dbEnsemblesFilePaths[i])
      }

      con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=dbEnsemblesFilePaths[i])
      set_PRAGMAs(con, PRAGMA_settings2())

      for (j in seq_along(ensemble.families)) {
        for (k in seq_along(ensemble.levels)) {
          EnsembleFamilyLevelTables<-paste(ensemble.families[j],"_rank_",formatC(ensemble.levels[k], width=2, flag="0"),"_",c("means","sds",if (save.scenario.ranks) "scenarioranks"),sep="")
          if (grepl(patter="overall",respName[i],ignore.case=TRUE)) {
            RSQLite::dbGetQuery(con,paste("CREATE TABLE \"",EnsembleFamilyLevelTables[1],"\" (", meanString, ");", sep=""))
            RSQLite::dbGetQuery(con,paste("CREATE TABLE \"",EnsembleFamilyLevelTables[2],"\" (", sdString, ");", sep=""))
            if (save.scenario.ranks) RSQLite::dbGetQuery(con,paste("CREATE TABLE \"",EnsembleFamilyLevelTables[3],"\" (", gsub(pattern="REAL",replacement="INTEGER",x=meanString), ");", sep=""))
          } else {
            agg.analysis <- switch(EXPR=respName[i], AET=1, Transpiration=2, EvaporationSoil=1, EvaporationSurface=1, EvaporationTotal=1, VWCbulk=2, VWCmatric=2, SWCbulk=2, SWPmatric=2, SWAbulk=2, Snowpack=1, Rain=1, Snowfall=1, Snowmelt=1, SnowLoss=1, Infiltration=1, DeepDrainage=1, PET=1, TotalPrecipitation=1, TemperatureMin=1, TemperatureMax=1, SoilTemperature=2, Runoff=1)
            if (agg.analysis == 1) {
              RSQLite::dbGetQuery(con,paste("CREATE TABLE \"",EnsembleFamilyLevelTables[1],"\" (", dailySQL, ");", sep=""))
              RSQLite::dbGetQuery(con,paste("CREATE TABLE \"",EnsembleFamilyLevelTables[2],"\" (", dailySQL, ");", sep=""))
              if (save.scenario.ranks) RSQLite::dbGetQuery(con,paste("CREATE TABLE \"",EnsembleFamilyLevelTables[3],"\" (", gsub(pattern="REAL",replacement="INTEGER",x=dailySQL), ");", sep=""))
            } else {
              RSQLite::dbGetQuery(con,paste("CREATE TABLE \"",EnsembleFamilyLevelTables[1],"\" (", dailyLayersSQL, ");", sep=""))
              RSQLite::dbGetQuery(con,paste("CREATE TABLE \"",EnsembleFamilyLevelTables[2],"\" (", dailyLayersSQL, ");", sep=""))
              if (save.scenario.ranks) RSQLite::dbGetQuery(con,paste("CREATE TABLE \"",EnsembleFamilyLevelTables[3],"\" (", gsub(pattern="REAL",replacement="INTEGER",x=dailyLayersSQL), ");", sep=""))
            }
          }
        }
      }
      RSQLite::dbDisconnect(con)
    }
  }

  dbOverallColumns
}
