#------------------------------------------------------------------------------#

#------CODE developed and written by
# - Daniel R Schlaepfer (dschlaep@uwyo.edu, drs): 2009-2016
# for contact and further information see also:
# \url{sites.google.com/site/drschlaepfer}

#------DISCLAIMER: This program is distributed in the hope that it will be
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#------------------------------------------------------------------------------#

#' Identify \var{P_id} for which output is not completely available in the
#' database \var{\sQuote{dbOutput}}
#' @export
missing_Pids_outputDB <- function(Table, dbname) {
  mP_ids <- -1L

  if (file.exists(dbname)) {
    con <- dbConnect(SQLite(), dbname = dbname,
      flags = SQLITE_RO)
    on.exit(dbDisconnect(con), add = TRUE)

    if (dbExistsTable(con, "header") && dbExistsTable(con, Table)) {
      sql <- paste0("SELECT header.P_id FROM header LEFT JOIN ", Table,
        " ON (header.P_id=", Table, ".P_id) WHERE header.Include_YN = 1 AND ",
        Table, ".P_id is NULL ", "ORDER BY header.P_id")
      mP_ids <- dbGetQuery(con, sql)[, 1]
    }
  }

  as.integer(mP_ids)
}

getIDs_from_db_Pids <- function(dbname, Pids) {
  res <- data.frame(site_id = -1L, treatment_id = -1L)[-1, ]

  if (file.exists(dbname)) {
    con <- dbConnect(SQLite(), dbname = dbname,
      flags = SQLITE_RO)
    on.exit(dbDisconnect(con), add = TRUE)

    if (dbExistsTable(con, "runs")) {
      sql <- paste("SELECT site_id, treatment_id FROM runs WHERE P_id IN (?)",
        "ORDER BY site_id")
      rs <- dbSendStatement(con, sql)
      dbBind(rs, list(Pids))
      res <- dbFetch(rs)
      dbClearResult(rs)
    }
  }

  res
}

add_dbOutput_index <- function(con) {
  sql <- "SELECT * FROM sqlite_master WHERE type = 'index'"
  prev_indices <- dbGetQuery(con, sql)

  if (NROW(prev_indices) == 0L ||
      !("index_aomean_Pid" %in% prev_indices[, "name"])) {
    dbExecute(con, paste("CREATE INDEX index_aomean_Pid ON",
      "aggregation_overall_mean (P_id)"))
  }

  if (NROW(prev_indices) == 0L ||
      !("index_aosd_Pid" %in% prev_indices[, "name"])) {
    dbExecute(con, paste("CREATE INDEX index_aosd_Pid ON",
      "aggregation_overall_sd (P_id)"))
  }
}


#' List the design tables of \var{\sQuote{dbOutput}}
#' @export
dbOutput_ListDesignTables <- function() c("runs", "header", "run_labels",
  "scenario_labels", "sites", "experimental_labels", "treatments",
  "simulation_years", "weatherfolders")


#' List the \var{SQLite} internal tables of \var{\sQuote{dbOutput}}
#' @export
dbOutput_ListInternalTables <- function() c("sqlite_sequence", "sqlite_stat1",
  "sqlite_stat4")


#' List the available output tables of \var{\sQuote{dbOutput}}
#' @export
dbOutput_ListOutputTables <- function(con = NULL, dbname = NULL) {
  use_con <- !is.null(con) && inherits(con, "SQLiteConnection") &&
    dbIsValid(con)

  if (!use_con) {
    if (is.null(dbname)) {
      print(paste("'dbOutput_ListOutputTables': arguments con and dbname",
        "cannot both be NULL"))
      return(NULL)
    }
    con <- dbConnect(SQLite(), dbname = dbname,
      flags = SQLITE_RO)
    on.exit(dbDisconnect(con), add = TRUE)
  }

  temp <- dbListTables(con)
  tables <- temp[!(temp %in% c(dbOutput_ListDesignTables(),
    dbOutput_ListInternalTables()))]

  tables
}


#' Checks whether output tables of \var{\sQuote{dbOutput}} store output of
#' variables for each soil layer
#'
#' @param tables A vector of character strings. The names of those tables that
#'  should be checked for the presence of a field named \var{\code{Soil_Layer}}.
#'  If \code{NULL}, then all output tables will be checked.
#' @param con A valid \code{SQLiteConnection} database connection to
#'   \var{\sQuote{dbOutput}} or \code{NULL}.
#' @param dbname A character string. The path including name to
#'  \var{\sQuote{dbOutput}} or \code{NULL}.
#'
#' @section Note: At least one of \code{con} and \code{dbname} must be provided.
#'  Argument \code{con} has priority if both arguments are provided and
#'  \code{con} is valid.
#'
#' @return A named logical vector where names are tables. \code{TRUE} indicates
#'   that a table has records by soil layers.
#'
#' @export
dbOutput_Tables_have_SoilLayers <- function(tables = NULL, con = NULL, # nolint
  dbname = NULL) {

  use_con <- !is.null(con) && inherits(con, "SQLiteConnection") &&
    dbIsValid(con)

  if (!use_con) {
    if (is.null(dbname)) {
      print(paste("'dbOutput_ListTables_wSoilLayers': arguments con and dbname",
        "cannot both be NULL"))
      return(NULL)
    }
    con <- dbConnect(SQLite(), dbname = dbname,
      flags = SQLITE_RO)
    on.exit(dbDisconnect(con), add = TRUE)
  }

  if (is.null(tables))
    tables <- dbOutput_ListOutputTables(con)

  has_soillayers <- sapply(tables, function(table) {
    temp <- dbListFields(con, table)
    any(temp == "Soil_Layer")
  })
  names(has_soillayers) <- tables

  has_soillayers
}




getSiteIds <- function(con, folderNames) {
  wf_ids <- dbGetQuery(con, "SELECT id, folder FROM weatherfolders")
  wf_ids[match(folderNames, wf_ids[, "folder"], nomatch = NA), "id"]
}

#' Get name of weather file from database \var{\sQuote{dbOutput}}
#' @export
local_weatherDirName <- function(i_sim, runN, scN, dbOutput) {
  con <- dbConnect(SQLite(), dbname = dbOutput,
    flags = SQLITE_RO)
  on.exit(dbDisconnect(con), add = TRUE)

  dbGetQuery(con, paste("SELECT WeatherFolder FROM header WHERE P_id=",
    it_Pid(i_sim, runN, 1, scN)))[1, 1]
}




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
              paste0(gsub(" ", "", stats::na.exclude(r)), collapse = "_rank"))

  climCat
}


#---Database functions

addHeaderToWhereClause <- function(whereClause, headers = NULL,
  fdbrSFSW2 = NULL) {

  if (is.null(headers) && file.exists(fdbrSFSW2)) {
    con <- dbConnect(SQLite(), fdbrSFSW2,
      flags = SQLITE_RO)
    on.exit(dbDisconnect(con), add = TRUE)
    headers <- dbListFields(con, name = "header")
  }

  #Locate all "Label = 'x'"
  temp1 <- res <- strsplit(whereClause, split = " ", fixed = TRUE)[[1]]
  temp1F <- strsplit(temp1, split = "=", fixed = TRUE)
  ielem <- grepl("=", temp1) &
       !grepl("header.", temp1, fixed = TRUE) &
       sapply(temp1F, function(ch) ch[1] %in% headers)
  temp2 <- temp1[ielem]
  if (length(temp2) > 0)
    res[ielem] <- paste0("header.", temp2) #add 'header.'

  paste(res, collapse = " ")
}

#' Access data from a database
#' @export
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



#' Get data of variables in the overall aggregation table for one of the
#' scenarios
#' @export
get.SeveralOverallVariables_Scenario <- function(fdbrSFSW2, responseName, # nolint
  MeanOrSD = "Mean", scenario = "Current", whereClause = NULL) {

  dat <- NULL
  iColumns <- list()

  if (length(responseName) > 0) {
    con <- dbConnect(SQLite(), fdbrSFSW2,
      flags = SQLITE_RO)
    on.exit(dbDisconnect(con), add = TRUE)

    iTable <- dbListTables(con)
    iTable <- grep(paste0("Overall_", MeanOrSD), iTable, ignore.case = TRUE,
      fixed = FALSE, value = TRUE)

    if (length(iTable) == 1) {
      iColumns <- get_fieldnames(responseName,
        fields.header = dbListFields(con, "header"),
        fields.iTable = dbListFields(con, iTable))

      if (iColumns[["has_columns"]] || iColumns[["addPid"]]) {
        sql <- paste0("SELECT ",
          if (iColumns[["addPid"]])
            "header.P_id AS P_id",
          if (iColumns[["addPid"]] && iColumns[["has_columns"]])
            ", ",
          if (length(iColumns[["header"]]) > 0)
            paste0("\"", iColumns[["header"]], "\"", collapse = ", "),
          if (length(iColumns[["header"]]) > 0 &&
              length(iColumns[["iTable"]]) > 0)
            ", ",
          if (length(iColumns[["iTable"]]) > 0)
            paste0("\"", iColumns[["iTable"]], "\"", collapse = ", "),
          " FROM ", iTable,
          " INNER JOIN header ON ", iTable, ".P_id = header.P_id",
          " WHERE header.Scenario = ", shQuote(scenario),
          if (length(whereClause) > 0)
            paste0(" AND ", addHeaderToWhereClause(whereClause,
              fdbrSFSW2 = fdbrSFSW2)),
          " ORDER BY header.P_id;")

        dat <- dbGetQuery(con, sql)
      }
    }
  }

  dat[, iColumns[["outOrder"]]]
}

#' Get data of variables in the overall aggregation table for one of the
#' ensembles
#' @export
get.SeveralOverallVariables_Ensemble <- function(fdbrSFSW2, fdbrSFSW2ens, # nolint
  responseName, MeanOrSD = "Mean", fam, level, whereClause = NULL) {

  dat <- NULL
  iColumns <- list()

  if (length(responseName) > 0) {
    con <- dbConnect(SQLite())
    on.exit(dbDisconnect(con), add = TRUE)

    temp_fdbrSFSW2ens <- grep("Overall", fdbrSFSW2ens, ignore.case = TRUE,
      value = TRUE)
    dbExecute(con, paste("ATTACH", shQuote(temp_fdbrSFSW2ens), "AS X;"))
    dbExecute(con, paste("ATTACH", shQuote(fdbrSFSW2), "AS Y;"))
    sql <- "SELECT name FROM X.sqlite_master WHERE type = 'table';"
    temp <- unlist(dbGetQuery(con, sql))
    iTable <- temp[grepl(fam, temp, ignore.case = TRUE) &
           grepl(paste0("rank_", formatC(level, format = "d", flag = "0",
             width = 2)), temp) &
           grepl(paste0("_", MeanOrSD), temp, ignore.case = TRUE)]

    if (length(iTable) == 1) {
      sql1 <- "PRAGMA Y.table_info(header);"
      sql2 <- paste0("PRAGMA X.table_info(", iTable, ");")
      iColumns <- get_fieldnames(responseName,
        fields.header = dbExecute(con, sql1)$name,
        fields.iTable = dbExecute(con, sql2)$name)

      if (iColumns[["has_columns"]] || iColumns[["addPid"]]) {
        sql <- paste0("SELECT ",
          if (iColumns[["addPid"]])
            "Y.header.P_id AS P_id",
          if (iColumns[["addPid"]] && iColumns[["has_columns"]])
            ", ",
          if (length(iColumns[["header"]]) > 0)
            paste0("\"", iColumns[["header"]], "\"", collapse = ", "),
          if (length(iColumns[["header"]]) > 0 &&
              length(iColumns[["iTable"]]) > 0)
            ", ",
          if (length(iColumns[["iTable"]]) > 0)
            paste0("\"", iColumns[["iTable"]], "\"", collapse = ", "),
          " FROM X.", iTable,
          " INNER JOIN Y.header ON X.", iTable, ".P_id = Y.header.P_id",
          if (length(whereClause) > 0)
            paste0(" WHERE ", addHeaderToWhereClause(whereClause,
              fdbrSFSW2 = fdbrSFSW2)),
          " ORDER BY Y.header.P_id;")

        dat <- dbGetQuery(con, sql)
      }
    }
  }

  dat[, iColumns[["outOrder"]]]
}

#' Get data of variables in the overall aggregation table for one of the
#' \code{climCat} rows (combining 'Current' and ensembles)
#'
#' @export
get.SeveralOverallVariables <- function(fdbrSFSW2, fdbrSFSW2ens, climCat,
  responseName, MeanOrSD = "Mean", i_climCat = 1, whereClause = NULL,
  climate.ambient = "Current") {

  if (length(responseName) > 0 && i_climCat <= nrow(climCat)) {
    dat <- if (climCat[i_climCat, 1] == climate.ambient) {
          get.SeveralOverallVariables_Scenario(
            fdbrSFSW2 = fdbrSFSW2,
            responseName = responseName,
            MeanOrSD = MeanOrSD,
            scenario = climCat[i_climCat, 1],
            whereClause = whereClause)
        } else {
          get.SeveralOverallVariables_Ensemble(
            fdbrSFSW2 = fdbrSFSW2, fdbrSFSW2ens = fdbrSFSW2ens,
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

#' Get header and data for an entire table for one of the scenarios
#' @export
get.Table_Scenario <- function(fdbrSFSW2, responseName, MeanOrSD = "Mean",
  scenario = "Current", whereClause = NULL, header = FALSE) {

  dat <- NULL
  if (length(responseName) > 0) {
    con <- dbConnect(SQLite(), fdbrSFSW2,
      flags = SQLITE_RO)
    on.exit(dbDisconnect(con), add = TRUE)

    temp1 <- dbListTables(con)
    temp2 <- grepl(pattern = paste0(responseName, "_", MeanOrSD), x = temp1,
      ignore.case = TRUE, fixed = FALSE)
    iTable <- temp1[temp2]

    if (length(iTable) == 1) {
      fields <- dbListFields(con, iTable)[-1]

      sql <- paste0("SELECT ",
        if (header)
          "header. * , ",
        paste0("\"", fields, "\"", collapse = ", "),
        " FROM ", iTable, " INNER JOIN header ON ", iTable,
        ".P_id = header.P_id",
        " WHERE header.Scenario = ", shQuote(scenario),
        if (length(whereClause) > 0)
          paste0(" AND ", whereClause),
        " ORDER BY header.P_id;")

      dat <- dbGetQuery(con, sql)
    }
  }

  dat
}

#' Get header and data for an entire table for one of the ensembles
#' @export
get.Table_Ensemble <- function(fdbrSFSW2, fdbrSFSW2ens, responseName,
  MeanOrSD = "Mean", fam, level, whereClause = NULL, header = FALSE) {

  dat <- NULL
  if (length(responseName) > 0) {
    con <- dbConnect(SQLite())
    on.exit(dbDisconnect(con), add = TRUE)

    temp_fdbrSFSW2ens <- fdbrSFSW2ens[grepl(pattern = paste0("_", responseName),
      x = fdbrSFSW2ens, ignore.case = TRUE)]
    dbExecute(con, paste("ATTACH", shQuote(temp_fdbrSFSW2ens), "AS X;"))
    dbExecute(con, paste("ATTACH", shQuote(fdbrSFSW2), "AS Y;"))
    sql <- "SELECT name FROM X.sqlite_master WHERE type = 'table';"
    temp <- unlist(dbGetQuery(con, sql))
    iTable <- temp[grepl(pattern = fam, x = temp, ignore.case = TRUE) &
        grepl(pattern = paste0("rank_", formatC(level, format = "d", flag = "0",
          width = 2)), x = temp) &
        grepl(pattern = MeanOrSD, x = temp, ignore.case = TRUE)]
    if (length(iTable) == 1) {
      sql <- paste("PRAGMA X.table_info(", iTable, ");")
      column_names_iTable <- dbExecute(con, sql)$name
      column_names_iTable <- column_names_iTable[-1]#Remove P_id
      sql <- "PRAGMA Y.table_info(header);"
      column_names_header <- dbExecute(con, sql)$name
      column_names_header <- column_names_header[-1]#Remove P_id
      #Remove Scenario:
      column_names_header <- column_names_header[-length(column_names_header)]
      if ("Soil_Layer" %in% column_names_iTable) {
        column_names_iTable <- column_names_iTable[-1] #Remove Soil_Layer
        temp <- paste0(paste0("\"", column_names_header, "\""), collapse = ", ")
        sql <- paste0("SELECT ", if (header) "Y.header.P_id AS P_id, ",
          "Soil_Layer, ", if (header) temp, ", ",
          paste0(paste0("\"", column_names_iTable, "\""), collapse = ", "))
      } else {
        sql <- paste0("SELECT ", if (header) "Y.header. * , ",
          paste0(paste0("\"", column_names_iTable, "\""), collapse = ", "))
      }
      if (length(whereClause) > 0) {
        sql <- paste0(sql, " FROM X.", iTable, " INNER JOIN Y.header ON X.",
          iTable, ".P_id = Y.header.P_id WHERE ", whereClause,
          " ORDER BY Y.header.P_id;")
        dat <- dbGetQuery(con, sql)
      } else {
        sql <- paste0(sql, " FROM X.", iTable, " INNER JOIN Y.header ON X.",
          iTable, ".P_id = Y.header.P_id ORDER BY Y.header.P_id;")
        dat <- dbGetQuery(con, sql)
      }
    }
  }

  dat
}

#' Get data-part for an entire table for one of the \code{climCat} rows
#' (combining 'Current' and ensembles)
#' @export
get.Table <- function(fdbrSFSW2, fdbrSFSW2ens, climCat, responseName,
  MeanOrSD = "Mean", i_climCat = 1, whereClause = NULL, addPid = FALSE,
  climate.ambient = "Current") {

  if (length(responseName) > 0 && i_climCat <= nrow(climCat)) {
    if (climCat[i_climCat, 1] == climate.ambient) {
      scenario <- climCat[i_climCat, 1]
      con <- dbConnect(SQLite(), fdbrSFSW2,
        flags = SQLITE_RO)
      on.exit(dbDisconnect(con), add = TRUE)

      iTable <- dbListTables(con)
      iTable <- iTable[grepl(pattern = paste0(responseName, "_", MeanOrSD),
        x = iTable, ignore.case = TRUE, fixed = FALSE)]
      if (length(iTable) == 1) {
        fields <- dbListFields(con, iTable)
        fields <- fields[-1]
        if (length(whereClause) > 0) {
          sql <- paste0("SELECT ", if (addPid) "header.P_id AS P_id, ",
            paste0(paste0("\"", fields, "\""), collapse = ", "), " FROM ",
            iTable, " INNER JOIN header ON ", iTable,
            ".P_id = header.P_id WHERE header.Scenario = ", shQuote(scenario),
            " AND ", addHeaderToWhereClause(whereClause, fdbrSFSW2 = fdbrSFSW2),
            " ORDER BY header.P_id;")
        } else {
          sql <- paste0("SELECT ", if (addPid) "header.P_id AS P_id, ",
            paste0(paste0("\"", fields, "\""), collapse = ", "), " FROM ",
            iTable, " INNER JOIN header ON ", iTable,
            ".P_id = header.P_id WHERE header.Scenario = ", shQuote(scenario),
            " ORDER BY header.P_id;")
        }
        dat <- dbGetQuery(con, sql)
      }

    } else {
      fam <- climCat[i_climCat, 1]
      level <- climCat[i_climCat, 2]
      con <- dbConnect(SQLite())
      on.exit(dbDisconnect(con), add = TRUE)

      temp_fdbrSFSW2ens <- fdbrSFSW2ens[grepl(pattern =
          paste0("_", responseName), x = fdbrSFSW2ens, ignore.case = TRUE)]
      dbExecute(con, paste("ATTACH", shQuote(temp_fdbrSFSW2ens), "AS X;"))
      dbExecute(con, paste("ATTACH", shQuote(fdbrSFSW2), "AS Y;"))
      sql <- "SELECT name FROM X.sqlite_master WHERE type = 'table';"
      temp <- unlist(dbGetQuery(con, sql))
      iTable <- temp[grepl(pattern = fam, x = temp, ignore.case = TRUE) &
          grepl(pattern = paste0("rank_", formatC(level, format = "d",
            flag = "0", width = 2)), x = temp) &
          grepl(pattern = MeanOrSD, x = temp, ignore.case = TRUE)]
      if (length(iTable) == 1) {
        sql <- paste0("PRAGMA X.table_info(", iTable, ");")
        fields <- dbExecute(con, sql)$name
        fields <- fields[-1]
        if (length(whereClause) > 0) {
          sql <- paste0("SELECT ", if (addPid) "Y.header.P_id AS P_id, ",
            paste0(paste0("\"", fields, "\""), collapse = ", "), " FROM X.",
            iTable, " INNER JOIN Y.header ON X.", iTable,
            ".P_id = Y.header.P_id WHERE ",
            addHeaderToWhereClause(whereClause, fdbrSFSW2 = fdbrSFSW2),
            " ORDER BY Y.header.P_id;")
          dat <- dbGetQuery(con, sql)
        } else {
          sql <- paste0("SELECT ", if (addPid) paste0("X.", iTable,
            ".P_id AS P_id, "),
            paste0(paste0("\"", fields, "\""), collapse = ", "),
            " FROM X.", iTable, " ORDER BY P_id;")
          dat <- dbGetQuery(con, sql)
        }
      }
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
    temp <- dbGetQuery(con, sql)

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

  if (is.character(tmp_data)) {
    tmp_data <- get_DF_from_temptxt(tmp_data)
  }

  if (length(dim(tmp_data)) != 2) {
    tmp_data <- matrix(tmp_data, nrow = 1, length(tmp_data))
  }

  N <- length(id)
  stopifnot(length(table_name) == N, is.null(sl) || length(sl) == N,
    nrow(tmp_data) == N)

  tol <- 1e2 * SFSW2_glovars[["tol"]]

  OK_agree <- rep(FALSE, N)

  for (k in seq_len(N)) {
    # check whether data agree
    db_data <- if (is.null(sl)) {
      dbGetQuery(con, paste0("SELECT * FROM \"", table_name[k],
        "\" WHERE P_id = ", id[k]))
    } else {
      dbGetQuery(con, paste0("SELECT * FROM \"", table_name[k],
        "\" WHERE P_id = ", id[k], " AND Soil_Layer = ", sl[k]))
    }
    db_data <- as.numeric(db_data)

    res <- all.equal(db_data, as.numeric(tmp_data[k, ]), tolerance = tol)
    OK_agree[k] <- isTRUE(res)

    if (!OK_agree[k]) {
      ndiffs <- sum(abs(db_data - tmp_data) > tol, na.rm = TRUE)

      print(paste("dbOutput data with P_id =", id[k],
        if (!is.null(sl)) paste("and soil layer =", sl[k]) else NULL,
        "of table", shQuote(table_name[k]), "differ in n =", ndiffs,
        "fields from data of file", shQuote(filename), ":",
        paste(res, collapse = "--")))
    }
  }

  OK_agree
}


#' Locate file names of temporary output database files
get_fnames_dbTempOut <- function(dir_out_temp, ...) {
  list.files(path = dir_out_temp, pattern = "SQL_Node_[[:digit:]]+\\.sqlite3",
    full.names = TRUE, recursive = TRUE, include.dirs = FALSE,
    ignore.case = FALSE)
}


#' Locate file names of temporary output text files
get_fnames_temporaryOutput <- function(dir_out_temp, concatFile,
  deleteTmpSQLFiles = TRUE, resume = TRUE) {

  theFileList <- c(
    list.files(path = dir_out_temp, pattern = "SQL_Node_[[:digit:]]+\\.sql",
      full.names = FALSE, recursive = TRUE, include.dirs = FALSE,
      ignore.case = FALSE),
    list.files(path = dir_out_temp,
      pattern = "SQL_Current_Node_[[:digit:]]+\\.sql",
      full.names = FALSE, recursive = TRUE, include.dirs = FALSE,
      ignore.case = FALSE))

  # make sure that we don't include any database files
  theFileList <- grep(".sqlite3", theFileList, value = TRUE, invert = TRUE)

  # remove any already inserted files from list
  if (!deleteTmpSQLFiles && resume) {
    completedFiles <- if (file.exists(concatFile)) {
        basename(readLines(concatFile))
      } else {
        character(0)
      }
    temp <- theFileList %in% completedFiles
    if (any(temp)) {
      theFileList <- theFileList[!temp]
    }
  }

  theFileList
}

#' Extract names of \var{\sQuote{dbOutput}} tables from content of temporary
#' output files
#'
#' Table names are expected to be wrapped by '\"', e.g., \code{"INSERT INTO
#' \"aggregation_overall_sd\" VALUES (1139776,NULL,..."} where \code{table_name
#' = 'aggregation_overall_sd'}
get_tablename_from_temptxt <- function(str, k = -1, verbose = FALSE) {
  # If there is a tablename in an element of str, then we expect it to be
  # wrapped by '\"'
  # id_table will be a matrix with two rows and ncol = length(str)
  temp <- gregexpr('\"', str, fixed = TRUE)
  id_table <- matrix(-1, ncol = length(str), nrow = 2)
  ids <- lengths(temp) == 2L
  id_table[, ids] <- unlist(temp[ids], recursive = FALSE, use.names = FALSE)

  if (verbose) {
    ids_bad <- id_table[1, ] < 1

    if (any(ids_bad)) {
      cat(paste("Name of table(s) not located in file on\n",
        paste("\t* line", k + which(ids_bad), "str =",
          substr(str[ids_bad], 1, 100), "...", collapse = " /\n")))
    }
  }

  substr(str, 1 + id_table[1, ], -1 + id_table[2, ])
}


get_DF_from_temptxt <- function(str, k = -1) {
  id_start <- regexpr(" VALUES (", str, fixed = TRUE)
  id_start <- as.integer(attr(id_start, "match.length") + id_start)

  id_end <- regexpr(")", str, fixed = TRUE)
  id_end <- as.integer(id_end)

  tmp_data <- substr(str, id_start, id_end)
  tmp_data <- paste0("c(", tmp_data)
  tmp_data <- gsub("NULL", "NA", tmp_data)
  tmp_data <- paste0("list(", paste(tmp_data, collapse = ", "), ")")
  tmp_data <- eval(parse(text = tmp_data, keep.source = FALSE))

  do.call("rbind", tmp_data) # much faster than:
    # matrix(unlist(tmp_data), nrow = length(str), ncol = length(tmp_data[[1]]), byrow = TRUE) # nolint
}


#' Extract \var{P_id} from content of temporary output files
#'
#' \var{P_id} values are expected to be at the first position of values, e.g.,
#' \code{"INSERT INTO \"aggregation_overall_sd\" VALUES (1139776,NULL,..."}
#' where \code{P_id = 1139776}
get_Pid_from_temptxt <- function(str, k = -1, verbose = FALSE) {
  id_start <- regexpr(" VALUES (", str, fixed = TRUE)
  id_start <- attr(id_start, "match.length") + id_start

  id_end <- regexpr(",", str, fixed = TRUE)

  ids <- id_end < 0
  if (any(ids)) {
    # In case the only value is the Pid
    id_end[ids] <- regexpr(")", str[ids], fixed = TRUE)
  }

  ids_bad <- id_start < 1 | id_end <= id_start
  id_end[ids_bad] <- -1L

  if (verbose && any(ids_bad)) {
    cat(paste("P_id(s) not located in file on\n",
      paste("\t* line", k + which(ids_bad), "str =",
        substr(str[ids_bad], 1, 100), "...", collapse = " /\n")))
  }

  as.integer(substr(str, id_start, -1 + id_end))
}


#' Extract soil layer ID from content of temporary output files
#'
#' Soil layer ID values are expected to be at the second position of values,
#' e.g., \code{"INSERT INTO \"aggregation_overall_sd\" VALUES
#' (1139776,NULL,..."} where \code{sl = NULL}
get_SoilLayerID_from_temptxt <- function(str, k = -1) {
  id_sl <- as.integer(gregexpr(",", str, fixed = TRUE)[[1]])
  if (any(id_sl[1] < 1, id_sl[2] <= id_sl[1])) {
    stop(paste0("ID of soil layer not located on line ", k, ": ",
      substr(str, 1, 100)))
  }

  as.integer(substr(str, 1 + id_sl[1], -1 + id_sl[2]))
}


has_Pid <- function(con, table, Pid) {
  nPid <- length(Pid)
  ntable <- length(table)
  stopifnot(nPid == ntable || nPid == 1 || ntable == 1)

  utable <- unique(table)
  res <- rep(NA, max(ntable, nPid))

  for (k in seq_along(utable)) {
    ids <- table == utable[k]

    if ((sum(ids) > 1 || ntable == 1) && nPid > 1) {
      sql <- paste("SELECT P_id FROM", utable[k], "WHERE P_id IN (",
        paste(if (ntable > 1) Pid[ids] else Pid, collapse = ","), ")")
      temp <- dbGetQuery(con, sql)[, "P_id"]
      res[ids] <- Pid[ids] %in% temp
    } else {
      sql <- paste("SELECT Count(*) FROM", utable[k], "WHERE P_id =",
        if (nPid > 1) Pid[ids] else Pid)
      res[ids] <- as.logical(dbGetQuery(con, sql))
    }
  }

  res
}

has_Pid_SoilLayerID <- function(con, table, Pid, sl) {
  nPid <- length(Pid)
  nsl <- length(sl)

  idsl <- paste(Pid, sl, sep = "-")
  nidsl <- length(idsl)
  ntable <- length(table)
  stopifnot(nidsl == ntable || nidsl == 1  || ntable == 1)

  utable <- unique(table)
  res <- rep(NA, max(ntable, nidsl))

  for (k in seq_along(utable)) {
    ids <- table == utable[k]

    if ((sum(ids) > 1 || ntable == 1) && nidsl > 1) {
      sql <- paste("SELECT P_id, Soil_Layer FROM", utable[k],
        "WHERE P_id IN (",
        paste(if (ntable > 1) Pid[ids] else Pid, collapse = ","),
        ") AND Soil_Layer IN (",
        paste(if (ntable > 1) sl[ids] else sl, collapse = ","), ")")
      temp <- apply(dbGetQuery(con, sql)[, c("P_id", "Soil_Layer")], 1,
        paste, collapse = "-")
      res[ids] <- idsl[ids] %in% temp
    } else {
      sql <- paste("SELECT Count(*) FROM", utable[k], "WHERE P_id =",
        if (nPid > 1) Pid[ids] else Pid, "AND Soil_Layer =",
        if (nsl > 1) sl[ids] else sl)
      res[ids] <- as.logical(dbGetQuery(con, sql))
    }
  }

  res
}


#' Moves simulation output that was written to temporary \var{SQL}-databases to
#' a final output \var{SQL}-database
#'
#' Speed tests suggest that the chunking option slows the process down
#' considerably; thus, the default for \code{chunk_size} turns the chunking off.
#'
#' @return Invisibly the number of temporary \var{SQL}-databases.
move_dbTempOut_to_dbOut <- function(SFSW2_prj_meta, t_job_start, opt_parallel,
  opt_behave, opt_out_run, opt_verbosity, chunk_size = -1L, dir_out_temp = NULL,
  check_if_Pid_present = FALSE) {

  if (opt_verbosity[["verbose"]]) {
    t1 <- Sys.time()
    temp_call <- shQuote(match.call()[1])
    print(paste0("rSFSW2's ", temp_call, ": started at ", t1))

    on.exit({
      print(paste0("rSFSW2's ", temp_call, ": ended after ",
      round(difftime(Sys.time(), t1, units = "secs"), 2), " s")); cat("\n")
      },
      add = TRUE)
  }

  if (is.null(dir_out_temp)) {
    # Use default project location for temporary text files
    dir_out_temp <- SFSW2_prj_meta[["project_paths"]][["dir_out_temp"]]
  }

  # get list of dbTempOut not yet moved to dbOutput
  theFileList <- get_fnames_dbTempOut(dir_out_temp)

  if (length(theFileList) > 0) {
    # Connect to the final output database
    con_dbOut <- dbConnect(SQLite(),
      dbname = SFSW2_prj_meta[["fnames_out"]][["dbOutput"]])
    on.exit(dbDisconnect(con_dbOut), add = TRUE)

    if (check_if_Pid_present) {
      # Connect to the failed output text file
      jfname_failed <- file.path(
        SFSW2_prj_meta[["project_paths"]][["dir_out_temp"]],
        "SQL_tmptxt_failed.txt")
    }

    # Prepare output databases
    set_PRAGMAs(con_dbOut, PRAGMA_settings2())

    # Add data to SQL databases
    for (k1 in seq_along(theFileList)) {
      ok <- TRUE

      tDB1 <- Sys.time()
      temp <- difftime(tDB1, t_job_start, units = "secs") +
        opt_parallel[["opt_job_time"]][["one_concat_s"]]
      has_time_to_concat <-
        temp < opt_parallel[["opt_job_time"]][["wall_time_s"]]
      if (!has_time_to_concat) {
        break
      }

      if (opt_verbosity[["verbose"]]) {
        print(paste("Adding", shQuote(theFileList[k1]),
          "to output DB: started at", tDB1))
      }

      # Attach temporary DB
      sql <- paste("ATTACH", shQuote(theFileList[k1]), "AS dbTempOut")
      dbExecute(con_dbOut, sql)

      # Transfer records for each table from temporary to final output DB
      tables <- unlist(dbGetQuery(con_dbOut,
        "SELECT name FROM dbTempOut.sqlite_master WHERE type = 'table'"))

      if (check_if_Pid_present) {
        # obtain Pids/sl that are in dbTempOut but not yet in dbOut
        ok <- FALSE
        warning("option 'check_if_Pid_present' is not yet implemented; ",
          "code continues without checks")
        check_if_Pid_present <- FALSE

      #} else {
      }
      if (!check_if_Pid_present) {

        # no Pid checks; discard/ignore non-unique records
        for (k2 in seq_along(tables)) {
          # make sure that there is at least one record to transfer
          sql <- paste0("SELECT COUNT(*) FROM dbTempOut.", tables[k2],
            " LIMIT 1")
          has_TempOut <- as.integer(dbGetQuery(con_dbOut, sql)) > 0

          if (has_TempOut) {
            sql0 <- paste0("INSERT OR IGNORE INTO ", tables[k2],
              " SELECT * FROM dbTempOut.", tables[k2])

            if (chunk_size > 0) {
              off <- 0L
              repeat {
                sql <- paste(sql0, "LIMIT", chunk_size, "OFFSET", off)
                temp <- try(dbExecute(con_dbOut, sql),
                  silent = !opt_verbosity[["verbose"]])

                if (inherits(temp, "try-error")) {
                  n <- 0L
                  ok <- FALSE
                } else {
                  n <- temp
                  off <- off + temp
                }

                if (n == 0) break
              }

            } else {
              # no chunking
              temp <- try(dbExecute(con_dbOut, sql0),
                silent = !opt_verbosity[["verbose"]])

              ok <- ok && !inherits(temp, "try-error")
            }
          }
        }

        if (!ok) {
          # rename temporary DB to failed if anything didn't work
          temp0 <- basename(theFileList[k1])
          temp1 <- gregexpr(".", temp0, fixed = TRUE)
          etemp <- temp1[[1]][length(temp1[[1]])] # position of file extension
          ftemp <- paste0(substr(temp0, 1L, etemp - 1L), "_failed",
            substr(temp0, etemp, nchar(temp0)))

          try(file.rename(from = theFileList[k1],
            to = file.path(dirname(theFileList[k1]), ftemp)),
            silent = !opt_verbosity[["verbose"]])
        }
      }

      # Detach temporary DB
      dbExecute(con_dbOut, "DETACH dbTempOut")

      # Delete temporary DB
      if (opt_out_run[["deleteTmpSQLFiles"]]) {
        try(unlink(theFileList[k1], force = TRUE),
          silent = !opt_verbosity[["verbose"]])

        if (file.exists(theFileList[k1])) {
        }
      }
    }

    #--- run optimize on database
    dbExecute(con_dbOut, "PRAGMA optimize")

    #--- clean up
    dbDisconnect(con_dbOut)

    oe <- sys.on.exit()
    oe <- remove_from_onexit_expression(oe, "dbDisconnect")
    do.call(on.exit, args = c(list(oe), add = FALSE))

    if (opt_out_run[["deleteTmpSQLFiles"]]) {
      to_delete <- !file.exists(theFileList)

      if (any(to_delete)) {
        # Windows OS has problems with deleting files even if it claims
        # that `unlink` was successful

        # Try again and also try deleting with `file.remove`
        try(unlink(theFileList[to_delete], force = TRUE),
          silent = !opt_verbosity[["verbose"]])
        try(file.remove(theFileList[to_delete]),
          silent = !opt_verbosity[["verbose"]])

        to_delete <- !file.exists(theFileList)
        if (any(to_delete)) {
          print(paste("The temporary file(s)",
            paste(shQuote(theFileList[to_delete]), collapse = ", ",
            "was/were attempted to be deleted thrice but failed.")))
        }
      }
    }
  }

  invisible(length(theFileList))
}

#' Moves simulation output that was written to temporary text files to a
#' \var{SQL}-database
#'
#' @section Details: \code{move_temporary_to_outputDB}: no checking of temporary
#'   text files is done. Any line that fails to be added to the database (for
#'   whatever reason including a record with identical combination of \var{P_id}
#'   and \var{SoilLayerID} is already present) is written to a new file
#'   \file{SQL_tmptxt_failed.txt}.
#' @section Details: Initial tests suggest that performance degrades if
#'   \code{chunk_size} was small (e.g., 10); values around 1000 have been
#'   successful; values of 10,000 work about as fast as those of 1000, but
#'   memory usage is a bit larger -- and the risk that an entire transaction
#'   fails increases with \code{chunk_size}.
#'
#' @param chunk_size An integer value. The number of lines that are read at once
#'   from the temporary text files and processed in one SQL-transaction.
move_temporary_to_outputDB <- function(SFSW2_prj_meta, t_job_start,
  opt_parallel, opt_behave, opt_out_run, opt_verbosity, chunk_size = 1000L,
  dir_out_temp = NULL) {

  .Deprecated(new = "move_dbTempOut_to_dbOut")

  if (opt_verbosity[["verbose"]]) {
    t1 <- Sys.time()
    temp_call <- shQuote(match.call()[1])
    print(paste0("rSFSW2's ", temp_call, ": started at ", t1))

    on.exit({
      print(paste0("rSFSW2's ", temp_call, ": ended after ",
      round(difftime(Sys.time(), t1, units = "secs"), 2), " s")); cat("\n")
      },
      add = TRUE)
  }

  if (is.null(dir_out_temp)) {
    # Use default project location for temporary text files
    dir_out_temp <- SFSW2_prj_meta[["project_paths"]][["dir_out_temp"]]
  }

  #concatenate file keeps track of sql files inserted into data
  concatFile <- file.path(SFSW2_prj_meta[["project_paths"]][["dir_out_temp"]],
    "sqlFilesInserted.txt")

  # get list of all temporary output files not yet moved to dbOutput
  theFileList <- get_fnames_temporaryOutput(dir_out_temp, concatFile,
    deleteTmpSQLFiles = opt_out_run[["deleteTmpSQLFiles"]],
    resume = opt_behave[["resume"]])

  if (length(theFileList) > 0) {
    # Track status
    temp <- list(con = NULL, do = TRUE)
    OKs <- list(all = temp, cur = temp)
    targets <- names(OKs)

    OKs[["all"]][["jfname_failed"]] <- file.path(
      SFSW2_prj_meta[["project_paths"]][["dir_out_temp"]],
      "SQL_tmptxt_failed.txt")
    OKs[["cur"]][["jfname_failed"]] <- file.path(
      SFSW2_prj_meta[["project_paths"]][["dir_out_temp"]],
      "SQL_tmptxt_failedCurrent.txt")

    # Connect to the Database
    OKs[["all"]][["con"]] <- dbConnect(SQLite(),
      dbname = SFSW2_prj_meta[["fnames_out"]][["dbOutput"]])
    on.exit(dbDisconnect(OKs[["all"]][["con"]]), add = TRUE)

    do_DBCurrent <-
      SFSW2_prj_meta[["opt_out_fix"]][["dbOutCurrent_from_tempTXT"]] &&
      !SFSW2_prj_meta[["opt_out_fix"]][["dbOutCurrent_from_dbOut"]]

    reset_DBCurrent <- do_DBCurrent &&
      (SFSW2_prj_meta[["prj_todos"]][["wipe_dbOut"]] ||
      !file.exists(SFSW2_prj_meta[["fnames_out"]][["dbOutput_current"]]))

    if (reset_DBCurrent) {
      file.copy(from = SFSW2_prj_meta[["fnames_out"]][["dbOutput"]],
        to = SFSW2_prj_meta[["fnames_out"]][["dbOutput_current"]])
    }

    if (do_DBCurrent) {
      OKs[["cur"]][["con"]] <- dbConnect(SQLite(),
        dbname = SFSW2_prj_meta[["fnames_out"]][["dbOutput_current"]])
      on.exit(dbDisconnect(OKs[["cur"]][["con"]]), add = TRUE)

      if (reset_DBCurrent) {
        # DROP ALL ROWS THAT ARE NOT CURRENT FROM HEADER
        dbExecute(OKs[["cur"]][["con"]],
          "DELETE FROM runs WHERE scenario_id != 1;")
      }
    }

    # Prepare output databases
    set_PRAGMAs(OKs[["all"]][["con"]], PRAGMA_settings1())

    if (do_DBCurrent) {
      set_PRAGMAs(OKs[["cur"]][["con"]], PRAGMA_settings1())
    }

    # Add data to SQL databases
    for (j in seq_along(theFileList)) {

      tDB1 <- Sys.time()
      temp <- difftime(tDB1, t_job_start, units = "secs") +
        opt_parallel[["opt_job_time"]][["one_concat_s"]]
      has_time_to_concat <-
        temp < opt_parallel[["opt_job_time"]][["wall_time_s"]]
      if (!has_time_to_concat) {
        break
      }

      if (opt_verbosity[["verbose"]]) {
        print(paste("Adding", shQuote(theFileList[j]),
          "to output DB: started at", tDB1))
      }
      OKs[["cur"]][["do"]] <- do_DBCurrent &&
        grepl("SQL_Current", theFileList[j])

      for (tg in targets) if (OKs[[tg]][["do"]]) {
        # Read sequentially SQL statements from temporary file
        jfcon <- file(file.path(dir_out_temp, theFileList[j]), open = "rt")

        # Use transaction to send SQL statements from file to database
        k <- 1L

        repeat {
          # Read next line
          ksql_cmd <- readLines(jfcon, n = chunk_size)

          nlineread <- length(ksql_cmd)
          if (nlineread == 0) {
            # end of file
            break
          }

          res <- try(dbWithTransaction(OKs[[tg]][["con"]], {
            added <- vapply(ksql_cmd, function(str) {
                !inherits(try(dbExecute(OKs[[tg]][["con"]], str),
                  silent = !opt_verbosity[["print.debug"]]), "try-error")
              }, FUN.VALUE = NA, USE.NAMES = FALSE)
          }), silent = !opt_verbosity[["print.debug"]])

          # Report on success
          if (opt_verbosity[["print.debug"]] &&
              !inherits(res, "try-error") && any(added)) {
            print(paste("Added rows/chunk", sum(added), "/", chunk_size,
              "of file", shQuote(theFileList[j]),
              "successfully to dbOutput for", shQuote(tg)))
          }

          # Write failed to new file
          failed <- !added
          if (any(failed)) {
            cat(ksql_cmd[failed], file = OKs[[tg]][["jfname_failed"]],
              sep = "\n", append = TRUE)

            if (opt_verbosity[["print.debug"]]) {
              print(paste("The output DB has problems with inserting",
                "rows/chunk", sum(failed), "/", chunk_size, "of file",
                shQuote(theFileList[j]), "for", shQuote(tg)))
            }
          }

          k <- k + nlineread
        }

        # Clean up and report
        close(jfcon)
      }

      cat(file.path(dir_out_temp, theFileList[j]), file = concatFile,
        append = TRUE, sep = "\n")

      if (opt_out_run[["deleteTmpSQLFiles"]]) {
        try(file.remove(file.path(dir_out_temp, theFileList[j])), silent = TRUE)
      }

      if (opt_verbosity[["verbose"]]) {
        tDB <- round(difftime(Sys.time(), tDB1, units = "secs"), 2)
        print(paste("Processed file", shQuote(theFileList[j]), "with n =",
          k - 1, "lines ended at", Sys.time(), "after", tDB, "s"))
      }
    }
  }

  invisible(TRUE)
}


#' @section Details: \code{move_temporary_to_outputDB_withChecks}: temporary
#'   text files are checked for presence of table names and identification
#'   values (\var{P_id} and \var{soil layer ID}). If argument
#'   \code{check_if_Pid_present} is true and the record ID already exists in the
#'   database, then values are checked for agreement. The speed penalty for
#'   running the checks vs. \code{\link{move_temporary_to_outputDB}} was about
#'   20% in a set of tests.
#'   \itemize{
#'     \item Lines that have insufficient information or that fail to be
#'       added to the database are written to a new
#'       file \file{SQL_tmptxt_failed.txt}.
#'     \item Lines with record identified by \var{Pid} (and \var{sl})
#'       that are already in database and data does agree (agreement
#'       information only available if \code{check_if_Pid_present}) are
#'       written to a new file \file{SQL_tmptxt_duplicates.txt}.
#'    \item Lines with record identified by \var{Pid} (and \var{sl}) is
#'      already in database, but data do not agree (agreement information
#'      only available if \code{check_if_Pid_present}) are written to a new file
#'      \file{SQL_tmptxt_repeats.txt}.}
#'
#' @rdname move_temporary_to_outputDB
move_temporary_to_outputDB_withChecks <- function(SFSW2_prj_meta, t_job_start, # nolint
  opt_parallel, opt_behave, opt_out_run, opt_verbosity, chunk_size = 1000L,
  check_if_Pid_present = TRUE, dir_out_temp = NULL) {

  .Deprecated(new = "move_dbTempOut_to_dbOut")

  if (opt_verbosity[["verbose"]]) {
    t1 <- Sys.time()
    temp_call <- shQuote(match.call()[1])
    print(paste0("rSFSW2's ", temp_call, ": started at ", t1))

    on.exit({
      print(paste0("rSFSW2's ", temp_call, ": ended after ",
      round(difftime(Sys.time(), t1, units = "secs"), 2), " s")); cat("\n")
      },
      add = TRUE)
  }

  if (is.null(dir_out_temp)) {
    # Use default project location for temporary text files
    dir_out_temp <- SFSW2_prj_meta[["project_paths"]][["dir_out_temp"]]
  }

  #concatenate file keeps track of sql files inserted into data
  concatFile <- file.path(SFSW2_prj_meta[["project_paths"]][["dir_out_temp"]],
    "sqlFilesInserted.txt")

  # get list of all temporary output files not yet moved to dbOutput
  theFileList <- get_fnames_temporaryOutput(dir_out_temp, concatFile,
    deleteTmpSQLFiles = opt_out_run[["deleteTmpSQLFiles"]],
    resume = opt_behave[["resume"]])

  if (length(theFileList) > 0) {
    # Track status
    OK_ndefault <- rep(FALSE, chunk_size)
    temp <- list(con = NULL, do = TRUE)
    OKs <- list(all = temp, cur = temp)

    targets <- names(OKs)
    # elements of 'OKs' that don't get properly initialized/reset otherwise
    resets <- c("hasPid", "hasSL", "agree", "added")

    jfname_failed <- file.path(
      SFSW2_prj_meta[["project_paths"]][["dir_out_temp"]],
      "SQL_tmptxt_failed.txt")
    jfname_duplicates <- file.path(
      SFSW2_prj_meta[["project_paths"]][["dir_out_temp"]],
      "SQL_tmptxt_duplicates.txt")
    jfname_repeats <- file.path(
      SFSW2_prj_meta[["project_paths"]][["dir_out_temp"]],
      "SQL_tmptxt_repeats.txt")

    # Connect to the Database
    OKs[["all"]][["con"]] <- dbConnect(SQLite(),
      dbname = SFSW2_prj_meta[["fnames_out"]][["dbOutput"]])
    on.exit(dbDisconnect(OKs[["all"]][["con"]]), add = TRUE)

    out_tables_aggr <- dbOutput_ListOutputTables(OKs[["all"]][["con"]])

    do_DBCurrent <-
      SFSW2_prj_meta[["opt_out_fix"]][["dbOutCurrent_from_tempTXT"]] &&
      !SFSW2_prj_meta[["opt_out_fix"]][["dbOutCurrent_from_dbOut"]]

    reset_DBCurrent <-
      do_DBCurrent && (SFSW2_prj_meta[["prj_todos"]][["wipe_dbOut"]] ||
        !file.exists(SFSW2_prj_meta[["fnames_out"]][["dbOutput_current"]]))

    if (reset_DBCurrent) {
      file.copy(from = SFSW2_prj_meta[["fnames_out"]][["dbOutput"]],
        to = SFSW2_prj_meta[["fnames_out"]][["dbOutput_current"]])
    }

    if (do_DBCurrent) {
      OKs[["cur"]][["con"]] <- dbConnect(SQLite(),
        dbname = SFSW2_prj_meta[["fnames_out"]][["dbOutput_current"]])
      on.exit(dbDisconnect(OKs[["cur"]][["con"]]), add = TRUE)

      if (reset_DBCurrent) {
        # DROP ALL ROWS THAT ARE NOT CURRENT FROM HEADER
        dbExecute(OKs[["cur"]][["con"]],
          "DELETE FROM runs WHERE scenario_id != 1;")
      }
    }

    # Prepare output databases
    set_PRAGMAs(OKs[["all"]][["con"]], PRAGMA_settings1())
    if (do_DBCurrent) {
      set_PRAGMAs(OKs[["cur"]][["con"]], PRAGMA_settings1())
    }

    # Check whether we have tables where rows correspond to Pid - Soil
    # layer units
    tables_w_soillayers <- dbOutput_Tables_have_SoilLayers(out_tables_aggr,
      con = OKs[["all"]][["con"]])

    # Add data to SQL databases
    for (j in seq_along(theFileList)) {

      tDB1 <- Sys.time()
      temp <- difftime(tDB1, t_job_start, units = "secs") +
        opt_parallel[["opt_job_time"]][["one_concat_s"]]
      has_time_to_concat <-
        temp < opt_parallel[["opt_job_time"]][["wall_time_s"]]
      if (!has_time_to_concat) {
        break
      }

      # Read sequentially SQL statements from temporary file
      jfcon <- file(file.path(dir_out_temp, theFileList[j]), open = "rt")

      OKs[["cur"]][["do"]] <- do_DBCurrent &&
        grepl("SQL_Current", theFileList[j])

      if (opt_verbosity[["verbose"]]) {
        print(paste("Adding", shQuote(theFileList[j]),
          "to output DB: started at", tDB1))
      }

      #--- Send SQL statements to database
      k <- 1

      repeat {
        # Read next chunk of lines
        ksql_cmd <- readLines(jfcon, n = chunk_size)

        nlineread <- length(ksql_cmd)
        if (nlineread == 0) {
          # end of file
          break
        }

        # Track status
        for (tg in targets) for (rs in resets) {
          OKs[[tg]][[rs]] <- OK_ndefault[seq_len(nlineread)]
        }

        # Obtain data table
        # Determine table
        tablenames <- get_tablename_from_temptxt(ksql_cmd, k,
          verbose = opt_verbosity[["print.debug"]])
        OK_line <- tablenames %in% out_tables_aggr

        # Determine P_id
        Pids <- get_Pid_from_temptxt(ksql_cmd, k,
          verbose = opt_verbosity[["print.debug"]])
        OK_line <- OK_line & is.finite(Pids)

        for (tg in targets) if (OKs[[tg]][["do"]]) {
          # Check if P_id already in output DB
          OKs[[tg]][["hasPid"]][OK_line] <- has_Pid(OKs[[tg]][["con"]],
            tablenames[OK_line], Pids[OK_line])
          ids <- OKs[[tg]][["hasPid"]]

          OKs[[tg]][["hasSL"]][ids] <- tables_w_soillayers[tablenames[ids]]

          # If P_id already in output DB, then check whether table has
          # soil layers and, if so, whether soil layer is in DB
          if (any(OKs[[tg]][["hasSL"]])) {
            ids <- OKs[[tg]][["hasSL"]]
            sl <- OK_ndefault
            stop("I don't know variable 'dat' in: ",
              'sl[ids] <- as.integer(dat[["val"]][, 2L])') # nolint

            OKs[[tg]][["hasSL"]][ids] <- OK_line[ids] & is.finite(sl[ids])
            ids <- OKs[[tg]][["hasSL"]]

            # Check if P_id already in output DB
            OKs[[tg]][["hasPid"]][ids] <- has_Pid_SoilLayerID(
              OKs[[tg]][["con"]], tablenames[ids], Pids[ids], sl[ids])

          } else {
            sl <- NULL
          }

          # Check if data in temporary file and DB agree
          if (check_if_Pid_present && any(OKs[[tg]][["hasPid"]])) {
            ids <- OKs[[tg]][["hasPid"]]
            table_name <- tablenames[ids]

            OKs[[tg]][["agree"]][ids] <- check_data_agreement(
              OKs[[tg]][["con"]],
              table_name = table_name, id = Pids[ids], sl = sl[ids],
              tmp_data = ksql_cmd[ids],
              has_soillayer = tables_w_soillayers[table_name],
              filename = theFileList[j])
          }

          # Insert data via temporary SQL statement:
          # if good data line and if not already in DB
          OKs[[tg]][["add"]] <- OK_line & !OKs[[tg]][["hasPid"]]

          if (any(OKs[[tg]][["add"]])) {
            ids <- OKs[[tg]][["add"]]
            utables <- unique(tablenames[ids])

            for (tab in utables) {
              ids2 <- which(ids & tablenames == tab)

              dbWithTransaction(OKs[[tg]][["con"]], for (i in ids2) {
                res <- try(dbExecute(OKs[[tg]][["con"]], ksql_cmd[i]),
                  silent = !opt_verbosity[["verbose"]])

                OKs[[tg]][["added"]][i] <- !inherits(res, "try-error")
              })
            }
          }

          # Report on success
          if (opt_verbosity[["print.debug"]] && any(OKs[[tg]][["added"]])) {
            ids <- OKs[[tg]][["added"]]

            print(paste("Added to table(s)",
              paste(shQuote(unique(tablenames)), collapse = " / "),
              "of output DB: P_id =", paste(Pids[ids], collapse = " / "),
              if (!is.null(sl)) {
                paste("and soil layer =", paste(sl[ids], collapse = " / "))
              } else NULL,
              "from rows", k, "to", k + nlineread - 1, "of file",
              shQuote(theFileList[j])))
          }

          # Write failed, repeated or duplicated lines to new files
          if (any(!OKs[[tg]][["added"]])) {
            # repeats: record identified by Pid (+sl) is already in database,
            # but data do not agree (agreement information only available
            # if check_if_Pid_present)
            ids1 <- OKs[[tg]][["hasPid"]] & !OKs[[tg]][["agree"]]
            if (any(ids1)) {
              cat(ksql_cmd[ids1], file = jfname_repeats, sep = "\n",
                append = TRUE)
            }

            # duplicates: record identified by Pid (+sl) is already in
            # database and data does agree (agreement information only
            # available if check_if_Pid_present)
            ids2 <- OKs[[tg]][["hasPid"]] & OKs[[tg]][["agree"]]
            if (any(ids2)) {
              cat(ksql_cmd[ids2], file = jfname_duplicates, sep = "\n",
                append = TRUE)
            }

            # failed: temporary text line doesn't have sufficient information or
            # adding to database failed for other/unknown reasons
            ids3 <- !OKs[[tg]][["hasPid"]] | (OKs[[tg]][["add"]] &
                !OKs[[tg]][["added"]])
            if (any(ids3)) {
              cat(ksql_cmd[ids3], file = jfname_failed, sep = "\n",
                append = TRUE)
            }

            ids <- ids1 | ids2 | ids3
            if (opt_verbosity[["print.debug"]] && any(ids)) {
              print(paste("The output DB has problems with inserting P_id =",
                paste(Pids[ids], collapse = " / "),
                if (!is.null(sl)) {
                  paste("and soil layer =", paste(sl[ids], collapse = " / "))
                } else NULL,
                "from rows", k, "to", k + nlineread - 1, "of file",
                shQuote(theFileList[j])))
            }
          }
        }

        k <- k + nlineread
      }

      # Clean up and report
      close(jfcon)

      cat(file.path(dir_out_temp, theFileList[j]), file = concatFile,
        append = TRUE, sep = "\n")

      if (opt_out_run[["deleteTmpSQLFiles"]]) {
        try(file.remove(file.path(dir_out_temp, theFileList[j])), silent = TRUE)
      }

      if (opt_verbosity[["verbose"]]) {
        tDB <- round(difftime(Sys.time(), tDB1, units = "secs"), 2)
        print(paste("Processed file", shQuote(theFileList[j]), "with n =",
          k - 1, "lines ended at", Sys.time(), "after", tDB, "s"))
      }
    }
  }

  invisible(TRUE)
}




do_copyCurrentConditionsFromDatabase <- function(dbOutput, dbOutput_current, # nolint
  verbose = FALSE) {

  if (verbose)
    print(paste("Database is copied and subset to ambient condition: start at ",
      Sys.time()))
  #Get sql for tables and index
  resSQL <- dbSendStatement(con,
    "SELECT sql FROM sqlite_master WHERE type='table' ORDER BY name;")
  sqlTables <- dbFetch(resSQL, n = -1)
  sqlTables <- unlist(sqlTables)
  sqlTables <- sqlTables[-grep(pattern = "sqlite_sequence", sqlTables)]
  dbClearResult(resSQL)

  resIndex <- dbSendStatement(con,
    "SELECT sql FROM sqlite_master WHERE type = 'view' ORDER BY name;")
  sqlView <- dbFetch(resIndex, n = -1)
  dbClearResult(resIndex)

  sqlView <- unlist(sqlView)
  sqlView <- sqlView[!is.na(sqlView)]
  Tables <- dbListTables(con)
  Tables <- Tables[-grep(pattern = "sqlite_sequence", Tables)]

  con_cur <- dbConnect(SQLite(), dbOutput_current)
  on.exit(dbDisconnect(con_cur), add = TRUE)

  for (i in seq_along(sqlTables)) {
    #Create the tables
    res <- dbSendStatement(con_cur, sqlTables[i])
    dbClearResult(res)
  }
  dbExecute(con_cur, sqlView)

  con <- dbConnect(SQLite(), dbname = dbOutput)
  on.exit(dbDisconnect(con), add = TRUE)

  #Get Tables minus ones we do not want
  Tables <- dbOutput_ListOutputTables(con)

  writeLines(text = paste0(".mode insert ", Tables, "\n.out ", Tables,
    ".sql\nSELECT * FROM ", Tables, " WHERE P_id IN (SELECT P_id FROM runs ",
    "WHERE scenario_id = 1 ORDER BY P_id);"), con = "dump.txt")
  lines <- c("PRAGMA cache_size = 400000;", "PRAGMA synchronous = 1;",
    "PRAGMA locking_mode = EXCLUSIVE;", "PRAGMA temp_store = MEMORY;",
    "PRAGMA auto_vacuum = NONE;")
  writeLines(text = c(lines, paste0(".read ", Tables, ".sql")),
    con = "insert.txt")

  system(paste0("cat dump.txt | sqlite3 ", shQuote(dbOutput)))
  system(paste0("cat insert.txt | sqlite3 ", shQuote(dbOutput_current)))

  unlink(paste0(Tables, ".sql"))

  Tables <- dbOutput_ListOutputTables(con)

  writeLines(text = paste0(".mode insert ", Tables, "\n.out ", Tables,
    ".sql\nSELECT * FROM ", Tables, ";"), con = "dump.txt")
  lines <- c("PRAGMA cache_size = 400000;", "PRAGMA synchronous = 1;",
    "PRAGMA locking_mode = EXCLUSIVE;", "PRAGMA temp_store = MEMORY;",
    "PRAGMA auto_vacuum = NONE;")
  writeLines(text = c(lines, paste0(".read ", Tables, ".sql")),
    con = "insert.txt")

  system(paste0("cat dump.txt | sqlite3 ", shQuote(dbOutput)))
  system(paste0("cat insert.txt | sqlite3 ", shQuote(dbOutput_current)))

  unlink(paste0(Tables, ".sql"))
  unlink(c("dump.txt", "insert.txt"))

  invisible(TRUE)
}


#' Check whether \var{\sQuote{dbOutput}} contains a complete set of
#' output/simulation results
#' @export
check_outputDB_completeness <- function(SFSW2_prj_meta, opt_parallel,
  opt_behave, opt_out_run, opt_verbosity) {

  temp_call <- shQuote(match.call()[1])
  if (opt_verbosity[["verbose"]]) {
    t1 <- Sys.time()
    print(paste0("rSFSW2's ", temp_call, ": started at ", t1))

    on.exit({
      print(paste0("rSFSW2's ", temp_call, ": ended after ",
      round(difftime(Sys.time(), t1, units = "secs"), 2), " s")); cat("\n")
      },
      add = TRUE)
  }

  #--- CHECK THAT ALL TEMPORARY DATA HAVE BEEN MOVED TO dbOutput
  temp1_files <- get_fnames_temporaryOutput(
    dir_out_temp = SFSW2_prj_meta[["project_paths"]][["dir_out_temp"]],
    concatFile = file.path(SFSW2_prj_meta[["project_paths"]][["dir_out_temp"]],
      "sqlFilesInserted.txt"),
    deleteTmpSQLFiles = opt_out_run[["deleteTmpSQLFiles"]],
    resume = opt_behave[["resume"]])
  temp2_files <- get_fnames_dbTempOut(
    SFSW2_prj_meta[["project_paths"]][["dir_out_temp"]])

  tempN_todo <- length(temp1_files) + length(temp2_files)

  runsN_todo <- if (opt_behave[["keep_dbWork_updated"]]) {
      dbWork_Ntodo(SFSW2_prj_meta[["project_paths"]][["dir_out"]])
    } else 0L

  if (runsN_todo > 0 || tempN_todo > 0) {
    if (opt_verbosity[["verbose"]] && tempN_todo) {
      print(paste("Unfinished temporary files:",
        paste(shQuote(temp1_files), collapse = ", "),
        paste(shQuote(temp2_files), collapse = ", ")))
    }

    msg <- paste(temp_call, "can only process `dbOutput` after all simulation",
      "runs have  completed and once all temporary output files have been",
      "moved to the database:\n",
      "Currently, n(unfinished runs) =", runsN_todo,
      "and n(unfinished temporary files) =", tempN_todo)

    # Check whether we are on Windows OS which has trouble deleting files
    # properly (see `move_dbTempOut_to_dbOut`)
    if (tempN_todo > 0 && .Platform$OS.type == "windows") {
      print(msg)
      print(paste("However, we are on Windows OS and thus it may have not",
        "actually deleted files confirmed to be deleted. We currently ignore",
        "this and attempt to continue."))
    } else {
      stop(msg)
    }
  }


  #--- SET UP PARALLELIZATION
  setup_SFSW2_cluster(opt_parallel,
    dir_out = SFSW2_prj_meta[["project_paths"]][["dir_prj"]],
    verbose = opt_verbosity[["verbose"]],
    print.debug = opt_verbosity[["print.debug"]])
  on.exit(exit_SFSW2_cluster(verbose = opt_verbosity[["verbose"]]),
    add = TRUE)
  on.exit(set_full_RNG(SFSW2_prj_meta[["rng_specs"]][["seed_prev"]],
    kind = SFSW2_prj_meta[["rng_specs"]][["RNGkind_prev"]][1],
    normal.kind = SFSW2_prj_meta[["rng_specs"]][["RNGkind_prev"]][2]),
    add = TRUE)


  Tables <- dbOutput_ListOutputTables(dbname =
    SFSW2_prj_meta[["fnames_out"]][["dbOutput"]])

  missing_Pids <- missing_Pids_current <- NULL

  do_DBcurrent <-
    SFSW2_prj_meta[["opt_out_fix"]][["dbOutCurrent_from_dbOut"]] ||
    SFSW2_prj_meta[["opt_out_fix"]][["dbOutCurrent_from_tempTXT"]]

  if (SFSW2_glovars[["p_has"]]) {
    if (identical(SFSW2_glovars[["p_type"]], "mpi")) {
      missing_Pids <- Rmpi::mpi.applyLB(Tables, missing_Pids_outputDB,
        dbname = SFSW2_prj_meta[["fnames_out"]][["dbOutput"]])

      if (do_DBcurrent) {
        missing_Pids_current <- Rmpi::mpi.applyLB(Tables, missing_Pids_outputDB,
          dbname = SFSW2_prj_meta[["fnames_out"]][["dbOutput_current"]])
      }

    } else if (identical(SFSW2_glovars[["p_type"]], "socket")) {
      missing_Pids <- parallel::clusterApplyLB(SFSW2_glovars[["p_cl"]],
        x = Tables, fun = missing_Pids_outputDB,
        dbname = SFSW2_prj_meta[["fnames_out"]][["dbOutput"]])

      if (do_DBcurrent) {
        missing_Pids_current <- parallel::clusterApplyLB(
          SFSW2_glovars[["p_cl"]], x = Tables, fun = missing_Pids_outputDB,
          dbname = SFSW2_prj_meta[["fnames_out"]][["dbOutput_current"]])
      }
    }

    clean_SFSW2_cluster()

  } else {
    missing_Pids <- lapply(Tables, missing_Pids_outputDB,
      dbname = SFSW2_prj_meta[["fnames_out"]][["dbOutput"]])

    if (do_DBcurrent) {
      missing_Pids_current <- lapply(Tables, missing_Pids_outputDB,
        dbname = SFSW2_prj_meta[["fnames_out"]][["dbOutput_current"]])
    }
  }

  missing_Pids <- as.integer(sort(unique(unlist(missing_Pids))))
  missing_runIDs <- NULL
  missing_Pids_current <- unique(unlist(missing_Pids_current))
  if (!is.null(missing_Pids_current)) {
    missing_Pids_current <- as.integer(sort(missing_Pids_current))
  }

  do_update_status <- FALSE

  if (length(missing_Pids) > 0) {
    ftemp <- file.path(SFSW2_prj_meta[["project_paths"]][["dir_out"]],
      "dbOutput_Pids_missing.rds")

    if (identical(missing_Pids, -1L)) {
      print(paste("Output DB",
        shQuote(SFSW2_prj_meta[["fnames_out"]][["dbOutput"]]),
        "is empty and not complete"))

    } else {
      print(paste("Output DB",
        shQuote(SFSW2_prj_meta[["fnames_out"]][["dbOutput"]]),
        "is missing n =", length(missing_Pids), "records"))

     # Output missing Pids to rds file
      print(paste("P_id of these records are saved to file", shQuote(ftemp)))
      saveRDS(missing_Pids, file = ftemp)

      # Update workDB
      if (opt_behave[["check_updates_dbWork"]] ||
          opt_out_run[["deleteTmpSQLFiles"]]) {
        print(paste("'workDB' is updated with these missing P_id to",
          "be prepared for a re-run"))

        if (opt_behave[["keep_dbWork_updated"]]) {
          stopifnot(dbWork_clean(
            SFSW2_prj_meta[["project_paths"]][["dir_out"]]))

          con <- dbConnect(SQLite(),
            dbname = SFSW2_prj_meta[["fnames_out"]][["dbOutput"]],
            flags = SQLITE_RO)
          on.exit(dbDisconnect(con), add = TRUE)
          sql <- "SELECT Max(id) FROM scenario_labels"
          scN <- dbGetQuery(con, sql)[1, 1]

          missing_runIDs <- it_sim2(missing_Pids, scN)
          temp <- dbWork_redo(SFSW2_prj_meta[["project_paths"]][["dir_out"]],
            runIDs = missing_runIDs)

          do_update_status <- TRUE

        } else {
          # if 'keep_dbWork_updated' is FALSE, then the fastest method to update
          # missing Pids is to recreate dbWork
          recreate_dbWork(SFSW2_prj_meta = SFSW2_prj_meta,
            verbose = opt_verbosity[["verbose"]],
            print.debug = opt_verbosity[["print.debug"]])
        }
      }
    }

  } else {
     do_update_status <- TRUE
  }

  if (do_update_status) {
    # Set modification status: up-to-date
    dbWork_update_status(SFSW2_prj_meta[["project_paths"]][["dir_out"]],
      status = FALSE, verbose = opt_verbosity[["print.debug"]])
  }


  if (length(missing_Pids_current) > 0) {
    ftemp <- file.path(SFSW2_prj_meta[["project_paths"]][["dir_out"]],
      "dbOutputCurrent_Pids_missing.rds")

    if (identical(missing_Pids_current, -1L)) {
      print(paste("Current output DB",
        shQuote(SFSW2_prj_meta[["fnames_out"]][["dbOutput_current"]]),
        "is empty and not complete"))

    } else {
      print(paste("Current output DB",
        shQuote(SFSW2_prj_meta[["fnames_out"]][["dbOutput_current"]]),
        "is missing n =", length(missing_Pids_current), "records;",
        "P_id of these records are saved to file", shQuote(ftemp)))

     saveRDS(missing_Pids_current, file = ftemp)
   }
  }

  oe <- sys.on.exit()
  oe <- remove_from_onexit_expression(oe, "exit_SFSW2_cluster")
  on.exit(eval(oe), add = FALSE)

  invisible(list(missing_Pids = missing_Pids,
    missing_Pids_current = missing_Pids_current,
    missing_runIDs = missing_runIDs))
}


dbOutput_create_Design <- function(con_dbOut, SFSW2_prj_meta,
  SFSW2_prj_inputs) {

  fieldname_weatherf <- "LookupWeatherFolder"
  fieldname_weatherid <- "LookupWeatherFolder_id"

  dbExecute(con_dbOut, paste("CREATE TABLE",
    "weatherfolders(id INTEGER PRIMARY KEY AUTOINCREMENT,",
    "folder TEXT UNIQUE NOT NULL)"))

  temp <- SFSW2_prj_inputs[["SWRunInformation"]]$dailyweather_source[
    SFSW2_prj_meta[["sim_size"]][["runIDs_sites"]]] == fieldname_weatherf

  if (!all(any(temp),
        any(SFSW2_prj_inputs[["create_treatments"]] == fieldname_weatherf))) {

    if (any(!is.na(SFSW2_prj_inputs[["SWRunInformation"]]$WeatherFolder))) {

      temp <- unique(stats::na.exclude(
        SFSW2_prj_inputs[["SWRunInformation"]]$WeatherFolder))

      sql <- "INSERT INTO weatherfolders VALUES(NULL, :folder)"
      rs <- dbSendStatement(con_dbOut, sql)
      dbBind(rs, param = list(folder = temp))
      dbClearResult(rs)

    } else {
      stop("All WeatherFolder names in master input file are NAs.")
    }
  }


  #############Site Table############################
  # Note: invariant to 'include_YN', i.e., do not subset
  # rows of 'SFSW2_prj_inputs[["SWRunInformation"]]'
  index_sites <- sort(unique(c(sapply(req_fields_SWRunInformation(),
      function(x) which(x == colnames(SFSW2_prj_inputs[["SWRunInformation"]]))),
    SFSW2_prj_meta[["opt_out_fix"]][["Index_RunInformation"]])))
  sites_data <- data.frame(SFSW2_prj_inputs[["SWRunInformation"]][,
    index_sites], row.names = NULL, check.rows = FALSE, check.names = FALSE,
    stringsAsFactors = FALSE)

  # Get WeatherFolder_id from table weatherfolders
  sites_data$WeatherFolder <- getSiteIds(con_dbOut, sites_data$WeatherFolder)
  colnames(sites_data) <- sub(pattern = "WeatherFolder",
    replacement = "WeatherFolder_id", colnames(sites_data))
  site_col_types <- sapply(sites_data, function(x)
    dbDataType(con_dbOut, x))
  dbExecute(con_dbOut,
    paste0("CREATE TABLE sites(\"id\" INTEGER PRIMARY KEY AUTOINCREMENT, ",
      paste0('\"', colnames(sites_data), '\" ', site_col_types,
        collapse = ", "),
      ", FOREIGN KEY(WeatherFolder_id) REFERENCES weatherfolders(id));"))

  dbWriteTable(con_dbOut, "sites", append = TRUE,
    value = cbind(id = NA, sites_data), row.names = FALSE)

  useExperimentals <- SFSW2_prj_meta[["sim_size"]][["expN"]] > 0 &&
    length(SFSW2_prj_inputs[["create_experimentals"]]) > 0
  useTreatments <- any(!(SFSW2_prj_inputs[["create_treatments"]] %in%
      SFSW2_prj_inputs[["create_experimentals"]]))

  #############simulation_years table#########################
  dbExecute(con_dbOut, paste("CREATE TABLE",
    "simulation_years(id INTEGER PRIMARY KEY AUTOINCREMENT,",
    "simulationStartYear INTEGER NOT NULL, StartYear INTEGER NOT NULL,",
    "EndYear INTEGER NOT NULL);"))
  ##################################################


  ##########Create table experimental_labels only if using experimentals
  if (useExperimentals) {
    dbExecute(con_dbOut, paste("CREATE TABLE",
      "experimental_labels(id INTEGER PRIMARY KEY AUTOINCREMENT,",
      "label TEXT UNIQUE NOT NULL);"))

    sql <- "INSERT INTO experimental_labels VALUES(NULL, :label)"
    rs <- dbSendStatement(con_dbOut, sql)
    dbBind(rs, param = list(
      label = SFSW2_prj_inputs[["sw_input_experimentals"]][, 1]))
    dbClearResult(rs)

  }
  ################################

  # If LookupWeatherFolder is ON we need to make sure all of the weather
  # folders are in weatherfolders table
#TODO: WeatherFolder update
  if (any(SFSW2_prj_inputs[["create_treatments"]] == fieldname_weatherf)) {
    #which ones are not in SFSW2_prj_inputs[["SWRunInformation"]]$WeatherFolder

    # make a combined list of experimentals and treatments LookupWeatherFolder
    #List first add any from the experimentals table if its turned on
    # next add any from the treatments table if its turned on
    tr_weather_name <- character(0)
    if (any(names(SFSW2_prj_inputs[["sw_input_treatments"]][
      SFSW2_prj_inputs[["sw_input_treatments_use"]]]) == fieldname_weatherf)) {
      tr_weather_name <- c(tr_weather_name,
        SFSW2_prj_inputs[["sw_input_treatments"]][
          SFSW2_prj_meta[["sim_size"]][["runIDs_sites"]], fieldname_weatherf])
    }
    if (any(SFSW2_prj_inputs[["create_experimentals"]] == fieldname_weatherf)) {
      tr_weather_name <- c(tr_weather_name,
        SFSW2_prj_inputs[["sw_input_experimentals"]][
          SFSW2_prj_meta[["sim_size"]][["runIDs_sites"]], fieldname_weatherf])
    }
    # Remove NA because that defaults to sites default weatherFolder also make
    # sure each folder is unique
    temp <- !is.na(tr_weather_name)
    tr_weather_name <- tr_weather_name[temp]
    tr_weather_name <- unique(tr_weather_name)
    if (length(tr_weather_name) == 0) {
      print(paste("LookupWeatherFolder is turned on in treatments or",
        "experimentals or both but is not used"))

    } else {
      # make a temp data.frame of a column NA's and a column of folder names
      LWF_index <- data.frame(id = rep(NA,
        length(tr_weather_name)),
        folder = tr_weather_name, stringsAsFactors = FALSE)
      # Get the id from sites table if the folder is in it
      LWF_index$id <- getSiteIds(con_dbOut, LWF_index$folder)
      # if there are any NA's we need to add those to the weatherfolder db
      # table andupdate its id in our lookuptable for weatherfolder
      if (any(is.na(LWF_index$id))) {
        #get max id from weatherfolders table
        isna <- is.na(LWF_index$id)
        maxid <- as.numeric(dbGetQuery(con_dbOut,
          "SELECT MAX(id) FROM weatherfolders;"))
        weatherfolders_index <- if (is.na(maxid)) 0L else maxid
        LWF_index$id[isna] <- as.integer(seq.int(
          from = weatherfolders_index + 1L,
          to = weatherfolders_index + sum(isna), by = 1L))

        #Write those in
        sql <- "INSERT INTO weatherfolders VALUES(:id, :folder)"
        rs <- dbSendStatement(con_dbOut, sql)
        dbBind(rs, param = as.list(LWF_index[isna, ]))
        dbClearResult(rs)
      }
    }
  }

  # get unique rows from both treatments and experimentals
  if (useExperimentals) {
    #Only use experimentals if there is something in it
    #Are all the columns NA
    temp <- is.na(SFSW2_prj_inputs[["sw_input_experimentals"]][,
      SFSW2_prj_inputs[["create_experimentals"]]])
    if (all(temp))
      stop("All Columns in experimentals table are NA")
    if (any(apply(temp, MARGIN = 2, function(x) all(x)))) {
      stop("One ore more columns in experimentals table are turned on ",
        "with no values or only with NA.")
    }
    db_experimentals <- unique(SFSW2_prj_inputs[["sw_input_experimentals"]][,
      SFSW2_prj_inputs[["create_experimentals"]]])

    # note experimentals should be unique; if we have less rows then the
    # original then lets throw an Error
    ttemp <- nrow(db_experimentals) ==
      nrow(SFSW2_prj_inputs[["sw_input_experimentals"]])
    if (!ttemp) {
      print(SFSW2_prj_inputs[["create_experimentals"]])
      print("'db_experimentals':")
      print(db_experimentals)
      print("'SFSW2_prj_inputs[[\"sw_input_experimentals\"]]':")
      print(SFSW2_prj_inputs[["sw_input_experimentals"]])
      stop("Each row of 'experimental-design' must be unique.")
    }

  } else {
    # experimentals does not have any rows. Are any of
    # the SFSW2_prj_inputs[["create_experimentals"]] turned on
    if (length(SFSW2_prj_inputs[["create_experimentals"]]) > 0 &&
        SFSW2_prj_meta[["sim_size"]][["expN"]] == 0)
      stop("No rows in experimentals table but columns are turned on")
    if (SFSW2_prj_meta[["sim_size"]][["expN"]] > 0 &&
        length(SFSW2_prj_inputs[["create_experimentals"]]) == 0)
      stop("Rows in experimentals are not being used.")
  }

  if (useTreatments) {
    # Note: invariant to 'include_YN', i.e., do not
    # subset 'SFSW2_prj_inputs[["SWRunInformation"]]'
    # we only need the columns that are turned on and not in experimentals.
    # Experimentals over write.
    temp <- SFSW2_prj_inputs[["create_treatments"]] %in%
      SFSW2_prj_inputs[["create_experimentals"]]
    temp <- SFSW2_prj_inputs[["create_treatments"]][!temp]
    temp_df <- SFSW2_prj_inputs[["sw_input_treatments"]][, temp, drop = FALSE]
    db_treatments <- unique(temp_df)
    db_treatments_rows <- nrow(db_treatments)
    #this maps locations from reduced
    temp <- duplicated(temp_df)
    treatments_unique_map <- rep(NA, nrow(temp_df))
    temp2 <- data.frame(t(temp_df))
    treatments_unique_map[temp] <- match(data.frame(t(temp_df[temp, ])), temp2)
    treatments_unique_map[!temp] <- match(data.frame(t(temp_df[!temp, ])),
      temp2)
    db_treatments_map <- unique(treatments_unique_map)
    treatments_unique_map <- sapply(treatments_unique_map, function(x)
      which(db_treatments_map == x))

  } else {
    db_treatments_rows <- 1
  }

  # Replace the LookupWeatherFolder with the LookupWeatherFolder_id in either
  # db_experimentals or db_treatments
  if (any(SFSW2_prj_inputs[["create_treatments"]] == fieldname_weatherf)) {
    if (any(SFSW2_prj_inputs[["create_experimentals"]] == fieldname_weatherf)) {
      #rename the column
      temp <- which(SFSW2_prj_inputs[["create_experimentals"]] ==
          fieldname_weatherf)
      colnames(db_experimentals)[temp] <- fieldname_weatherid
      #get the id numbers for those columns and replace text
      db_experimentals[, fieldname_weatherid] <-
        sapply(db_experimentals[, fieldname_weatherid],
        function(x) LWF_index$id[LWF_index$folder == x])

    } else {
      #rename the column
      temp <- which(colnames(db_treatments) == fieldname_weatherf)
      colnames(db_treatments)[temp] <- fieldname_weatherid
      #get the id numbers for those columns and replace text
      db_treatments[, fieldname_weatherid] <-
        sapply(db_treatments[, fieldname_weatherid],
        function(x) LWF_index$id[LWF_index$folder == x])
    }
  }

  useTreatmentWeatherFolder <- FALSE
  if (useExperimentals | useTreatments) {
    #Create a table to hold the values going into the database
    temp_numberRows <- if (useExperimentals) {
        nrow(db_experimentals) * db_treatments_rows
      } else nrow(db_treatments)
    temp_numberColumns <- (if (useExperimentals) 3 else 2) +
      length(SFSW2_prj_inputs[["create_treatments"]])
    temp_columnNames <- c("id", if (useExperimentals) "experimental_id",
      "simulation_years_id", SFSW2_prj_inputs[["create_treatments"]])
    db_combined_exp_treatments <- data.frame(matrix(data = NA,
      nrow = temp_numberRows, ncol = temp_numberColumns,
      dimnames = list(NULL, temp_columnNames)), stringsAsFactors = FALSE)

    #fill in the id column.
    db_combined_exp_treatments[, "id"] <-
      seq_len(nrow(db_combined_exp_treatments))

    #column types are listed in this data.frame along with what table it is from
    db_treatments_column_types <- data.frame(
      column = SFSW2_prj_inputs[["create_treatments"]],
      type = character(length(SFSW2_prj_inputs[["create_treatments"]])),
      table = numeric(length(SFSW2_prj_inputs[["create_treatments"]])),
      stringsAsFactors = FALSE)
    #0 for teatments 1 for experimentals
    temp <- db_treatments_column_types[, "column"] %in%
      SFSW2_prj_inputs[["create_experimentals"]]
    db_treatments_column_types[temp, "table"] <- 1

    ######################
    #Get the column types from the proper tables
    temp <- SFSW2_prj_inputs[["create_treatments"]] %in%
      SFSW2_prj_inputs[["create_experimentals"]]
    temp <- SFSW2_prj_inputs[["create_treatments"]][!temp]
    db_treatments_column_types[, "type"] <-
      sapply(db_treatments_column_types[, "column"],
      function(columnName) {
        if (columnName %in% SFSW2_prj_inputs[["create_experimentals"]]) {
          dbDataType(con_dbOut,
            SFSW2_prj_inputs[["sw_input_experimentals"]][, columnName])
        } else if (columnName %in% temp) {
          dbDataType(con_dbOut,
            SFSW2_prj_inputs[["sw_input_treatments"]][, columnName])
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
    fk_LookupWeatherFolder <- NA
    if (any(SFSW2_prj_inputs[["create_treatments"]] == fieldname_weatherf)) {
      useTreatmentWeatherFolder <- TRUE
      # Change name from 'LookupWeatherFolder' to 'LookupWeatherFolder_id'
      temp <- which(db_treatments_column_types[, "column"] ==
          fieldname_weatherf)
      db_treatments_column_types[temp, c("column", "type")] <-
        c(fieldname_weatherid, "INTEGER")
      temp <- colnames(db_combined_exp_treatments)
      temp[which(temp == fieldname_weatherf)] <- fieldname_weatherid
      colnames(db_combined_exp_treatments) <- temp
      fk_LookupWeatherFolder <- paste0(", FOREIGN KEY(", fieldname_weatherid,
        ") REFERENCES weatherfolders(id)")
    }

    #Create the table
    dbExecute(con_dbOut, paste0("CREATE TABLE treatments(",
      "id INTEGER PRIMARY KEY AUTOINCREMENT, ",
      if (useExperimentals) "experimental_id INTEGER, ",
      "simulation_years_id INTEGER, ",
      paste(db_treatments_column_types[, "column"],
        db_treatments_column_types[, "type"], collapse = ", "),
      if (useExperimentals) {
        ", FOREIGN KEY(experimental_id) REFERENCES experimental_labels(id)"
      },
      if (!is.na(fk_LookupWeatherFolder)) fk_LookupWeatherFolder,
      ");"))

    # Lets put in the treatments into combined. This will repeat the reduced
    # rows of treatments into combined
    if (useTreatments) {
      use_start <- colnames(db_treatments) == "YearStart"
      use_end <- colnames(db_treatments) == "YearEnd"
      i_use <- rep(TRUE, ncol(db_treatments))
      i_use[use_start] <- FALSE
      i_use[use_end] <- FALSE

      temp <- db_treatments_column_types[, "table"] == 0
      temp <- db_treatments_column_types[temp, "column"]
      db_combined_exp_treatments[, temp] <- db_treatments[, i_use]

      #Handle StartYear and EndYear separately
      if (any(use_start) && !is.null(db_treatments_years) &&
        db_treatments_years[
          db_treatments_years$column == "YearStart", "table"] == 0) {

        db_combined_exp_treatments[, "YearStart"] <-
          db_treatments[, "YearStart"]
      }
      if (any(use_end) && !is.null(db_treatments_years) &&
        db_treatments_years[
          db_treatments_years$column == "YearEnd", "table"] == 0) {

        db_combined_exp_treatments[, "YearEnd"] <- db_treatments[, "YearEnd"]
      }
    }

    if (useExperimentals) {
      exp_start_rows <- seq(from = 1,
        to = db_treatments_rows * nrow(db_experimentals),
        by = db_treatments_rows)
      #Insert data into our new data.frame
      for (istart in exp_start_rows) {
        irows <- istart:(istart + db_treatments_rows - 1)
        irows2 <- which(exp_start_rows == istart)
        #Get experimental_label_id
        db_combined_exp_treatments[irows, "experimental_id"] <- irows2
        #insert all of the rows from experimentals
        temp <- db_treatments_column_types[
          db_treatments_column_types[, "table"] == 1, "column"]
        db_combined_exp_treatments[irows, temp] <- db_experimentals[irows2, ]
      }
    }
  } else {
    db_combined_exp_treatments <- data.frame(
      matrix(data = 1, nrow = 1, ncol = 2,
        dimnames = list(NULL, c("id", "simulation_years_id"))),
      stringsAsFactors = FALSE)
    dbExecute(con_dbOut, paste("CREATE TABLE",
      "treatments(id INTEGER PRIMARY KEY AUTOINCREMENT,",
      "simulation_years_id INTEGER);"))
  }

  #if the column startYear or endYear are present move over to simulation_years
  if (any(colnames(db_combined_exp_treatments) == "YearStart") ||
    any(colnames(db_combined_exp_treatments) == "YearEnd")) {

    simulation_years <- matrix(data = NA,
      nrow = nrow(db_combined_exp_treatments), ncol = 4, dimnames = list(NULL,
      c("id", "simulationStartYear", "StartYear", "EndYear")))
    #Get from treatments or get from settings
    if (any(colnames(db_combined_exp_treatments) == "YearStart")) {
      simulation_years[, "simulationStartYear"] <-
        db_combined_exp_treatments[, "YearStart"]
      temp <- colnames(db_combined_exp_treatments) == "YearStart"
      db_combined_exp_treatments <- db_combined_exp_treatments[, !temp]

    } else {
      simulation_years[, "simulationStartYear"] <-
        SFSW2_prj_meta[["sim_time"]][["simstartyr"]]
    }
    if (any(colnames(db_combined_exp_treatments) == "YearEnd")) {
      simulation_years[, "EndYear"] <- db_combined_exp_treatments[, "YearEnd"]
      temp <- colnames(db_combined_exp_treatments) == "YearEnd"
      db_combined_exp_treatments <- db_combined_exp_treatments[, !temp]

    } else {
      simulation_years[, "EndYear"] <- SFSW2_prj_meta[["sim_time"]][["endyr"]]
    }
    simulation_years[, "StartYear"] <- getStartYear(
      simulation_years[, "simulationStartYear"],
      SFSW2_prj_meta[["sim_time"]][["spinup_N"]])

    # Replace NAs with values from SFSW2_prj_meta[["sim_time"]]
    if (anyNA(simulation_years[, "simulationStartYear"])) {
      ids <- is.na(simulation_years[, "simulationStartYear"])
      simulation_years[ids, "simulationStartYear"] <-
        SFSW2_prj_meta[["sim_time"]][["simstartyr"]]
    }
    if (anyNA(simulation_years[, "StartYear"])) {
      ids <- is.na(simulation_years[, "StartYear"])
      simulation_years[ids, "StartYear"] <-
        SFSW2_prj_meta[["sim_time"]][["startyr"]]
    }
    if (anyNA(simulation_years[, "EndYear"])) {
      ids <- is.na(simulation_years[, "EndYear"])
      simulation_years[ids, "EndYear"] <-
        SFSW2_prj_meta[["sim_time"]][["endyr"]]
    }

    # Create unique table of simulation years
    unique_simulation_years <- unique(simulation_years)
    if (nrow(unique_simulation_years) == nrow(simulation_years)) {
      # each row is unique so add id to db_combined
      id <- seq_len(nrow(unique_simulation_years))
      unique_simulation_years <- cbind(id,
        unique_simulation_years[,
          c("simulationStartYear", "StartYear", "EndYear")])
      db_combined_exp_treatments[, "simulation_years_id"] <-
        unique_simulation_years[, "id"]

    } else {
      # create map to unique rows in simulation_years
      temp <- duplicated(simulation_years, fromLast = FALSE) |
        duplicated(simulation_years, fromLast = TRUE)
      ids_sy <- apply(simulation_years, 1, paste, collapse = "_")
      db_combined_exp_treatments[, "simulation_years_id"] <-
        match(ids_sy, ids_sy)
    }

    unique_simulation_years <- data.frame(unique_simulation_years[,
      c("simulationStartYear", "StartYear", "EndYear")])

  } else {
    # Treatment option for simulation Years is turned off.
    # Get the default one from settings.
    db_combined_exp_treatments$simulation_years_id <- 1

    unique_simulation_years <- data.frame(
      simulationStartYear = SFSW2_prj_meta[["sim_time"]][["simstartyr"]],
      StartYear = SFSW2_prj_meta[["sim_time"]][["startyr"]],
      EndYear = SFSW2_prj_meta[["sim_time"]][["endyr"]])
  }

  # write to the database
  sql <- paste("INSERT INTO simulation_years VALUES(NULL,",
    ":simulationStartYear, :StartYear, :EndYear)")
  rs <- dbSendStatement(con_dbOut, sql)
  dbBind(rs, param = as.list(unique_simulation_years))
  dbClearResult(rs)

  #Insert the data into the treatments table
  sql <- paste0("INSERT INTO treatments VALUES(", paste0(":",
    colnames(db_combined_exp_treatments), collapse = ", "), ")")
  rs <- dbSendStatement(con_dbOut, sql)
  dbBind(rs, param = as.list(db_combined_exp_treatments))
  dbClearResult(rs)


  ##############scenario_labels table###############
  dbExecute(con_dbOut, paste("CREATE TABLE",
    "scenario_labels(id INTEGER PRIMARY KEY AUTOINCREMENT,",
    "label TEXT UNIQUE NOT NULL)"))

  sql <- "INSERT INTO scenario_labels VALUES(NULL, :label)"
  rs <- dbSendStatement(con_dbOut, sql)
  dbBind(rs, param = list(label = SFSW2_prj_meta[["sim_scens"]][["id"]]))
  dbClearResult(rs)

  ##################################################

  #############run_labels table#########################
  # Note: invariant to 'include_YN', i.e., do not
  # subset 'SFSW2_prj_inputs[["SWRunInformation"]]'
  dbExecute(con_dbOut, paste("CREATE TABLE",
    "run_labels(id INTEGER PRIMARY KEY AUTOINCREMENT,",
    "label TEXT UNIQUE NOT NULL);"))
  temp <- if (useExperimentals) {
      temp1 <- formatC(SFSW2_prj_inputs[["SWRunInformation"]][, "site_id"],
        width = SFSW2_prj_meta[["sim_size"]][["digitsN_total"]],
        format = "d", flag = "0")
      temp2 <- rep(SFSW2_prj_inputs[["sw_input_experimentals"]][, "Label"],
        each = SFSW2_prj_meta[["sim_size"]][["runsN_master"]])
      paste(temp1, temp2, SFSW2_prj_inputs[["SWRunInformation"]]$Label,
        sep = "_")

    } else {
      SFSW2_prj_inputs[["SWRunInformation"]]$Label
    }

  sql <- "INSERT INTO run_labels VALUES(NULL, :label)"
  rs <- dbSendStatement(con_dbOut, sql)
  dbBind(rs, param = list(label = temp))
  dbClearResult(rs)
  ##################################################


  #####################runs table###################
  # Note: invariant to 'include_YN', i.e., do not
  # subset 'SFSW2_prj_inputs[["SWRunInformation"]]'
  dbExecute(con_dbOut, paste("CREATE TABLE",
    "runs(P_id INTEGER PRIMARY KEY, label_id INTEGER NOT NULL,",
    "site_id INTEGER NOT NULL, treatment_id INTEGER NOT NULL,",
    "scenario_id INTEGER NOT NULL,",
    "FOREIGN KEY(label_id) REFERENCES run_labels(id),",
    "FOREIGN KEY(site_id) REFERENCES sites(id),",
    "FOREIGN KEY(treatment_id) REFERENCES treatments(id),",
    "FOREIGN KEY(scenario_id) REFERENCES scenario_labels(id));"))

  db_runs <- data.frame(matrix(data = 0,
    nrow = SFSW2_prj_meta[["sim_size"]][["runsN_Pid"]], ncol = 5,
    dimnames = list(NULL,
      c("P_id", "label_id", "site_id", "treatment_id", "scenario_id"))))
  db_runs$P_id <- seq_len(SFSW2_prj_meta[["sim_size"]][["runsN_Pid"]])
  db_runs$label_id <- rep(seq_len(
    SFSW2_prj_meta[["sim_size"]][["runsN_total"]]),
    each = SFSW2_prj_meta[["sim_scens"]][["N"]])
  db_runs$site_id <- rep(rep(
    SFSW2_prj_inputs[["SWRunInformation"]]$site_id,
    times = max(SFSW2_prj_meta[["sim_size"]][["expN"]], 1L)),
    each = SFSW2_prj_meta[["sim_scens"]][["N"]])
  db_runs$scenario_id <- rep(seq_len(
    SFSW2_prj_meta[["sim_scens"]][["N"]]),
    times = SFSW2_prj_meta[["sim_size"]][["runsN_total"]])

  temp <- if (useExperimentals) {
      as.vector(matrix(data = exp_start_rows,
        nrow = SFSW2_prj_meta[["sim_size"]][["runsN_master"]],
        ncol = SFSW2_prj_meta[["sim_size"]][["expN"]], byrow = TRUE))
    } else NULL

  db_runs$treatment_id <- if (useTreatments) {
      if (useExperimentals) {
        rep(temp + treatments_unique_map - 1,
          each = SFSW2_prj_meta[["sim_scens"]][["N"]])
      } else {
        rep(treatments_unique_map, each = SFSW2_prj_meta[["sim_scens"]][["N"]])
      }
    } else {
      if (useExperimentals) {
        rep(temp, each = SFSW2_prj_meta[["sim_scens"]][["N"]])
      } else 1
    }

  sql <- paste("INSERT INTO runs VALUES(:P_id, :label_id, :site_id,",
    ":treatment_id, :scenario_id)")
  rs <- dbSendStatement(con_dbOut, sql)
  dbBind(rs, param = as.list(db_runs))
  dbClearResult(rs)
  ##################################################

  ################CREATE VIEW########################
  if (length(SFSW2_prj_meta[["opt_out_fix"]][["Index_RunInformation"]]) > 0) {
    sites_columns <- colnames(SFSW2_prj_inputs[["SWRunInformation"]])[
      SFSW2_prj_meta[["opt_out_fix"]][["Index_RunInformation"]]]

    for (k_excl in c("label", "WeatherFolder", "Include_YN")) {
      icol <- grep(k_excl, sites_columns, ignore.case = TRUE)
      if (length(icol) > 0)
        sites_columns <- sites_columns[-icol]
    }

  } else {
    sites_columns <- NULL
  }
  treatment_columns <- colnames(db_combined_exp_treatments)[- (1:3)]
  if (useTreatmentWeatherFolder)
    treatment_columns <- treatment_columns[-grep("WeatherFolder",
      treatment_columns)]
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

  dbExecute(con_dbOut, paste0(
    "CREATE VIEW header AS SELECT ", header_columns,
    " FROM runs, run_labels, sites, ",
    if (useExperimentals)
      "experimental_labels, ",
    "treatments, scenario_labels, simulation_years, weatherfolders",
    " WHERE runs.label_id=run_labels.id AND runs.site_id=sites.id AND",
    " runs.treatment_id=treatments.id AND",
    " runs.scenario_id=scenario_labels.id AND ",
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

dbOutput_create_OverallAggregationTable <- function(con_dbOut, fields) { # nolint

  ncol_dbOut_overall <- sum(fields[, "N"])

  fieldnames <- if (ncol_dbOut_overall > 0) {
      paste0(paste0("\"", unlist(fields[, "fields"]), "\""), " REAL",
        collapse = ", ")
    } else {
      NULL
    }

  meanString <- paste(c("\"P_id\" INTEGER PRIMARY KEY", fieldnames),
    collapse = ", ")
  sdString <- paste(c("\"P_id\" INTEGER PRIMARY KEY",
    gsub("_mean", "_sd", fieldnames)), collapse = ", ")

  dbExecute(con_dbOut, paste0("CREATE TABLE \"aggregation_overall_mean\"",
    "(", meanString, ")"))
  dbExecute(con_dbOut, paste0("CREATE TABLE \"aggregation_overall_sd\" (",
    sdString, ")"))

  list(ncol_dbOut_overall = ncol_dbOut_overall, meanString = meanString,
    sdString = sdString)
}

dbOutput_create_DailyAggregationTable <- function(con_dbOut, req_aggs) { # nolint
  dailySQL <- dailyLayersSQL <- NULL

  if (req_aggs[["N"]] > 0) {
    doy_colnames <- paste0("doy", formatC(seq_len(366), width = 3, format = "d",
      flag = "0"))
    doy_colnames <- paste0(paste0("\"", doy_colnames, "\""), " REAL",
      collapse = ", ")

    dailySQL <- paste(c("\"P_id\" INTEGER PRIMARY KEY", doy_colnames),
      collapse = ", ")
    dailyLayersSQL <- paste(c("\"P_id\" INTEGER", "\"Soil_Layer\" INTEGER",
      doy_colnames, "PRIMARY KEY (\"P_id\", \"Soil_Layer\")"), collapse = ", ")

    for (doi in seq_len(req_aggs[["N"]])) {
      if (regexpr("SWAbulk", req_aggs[["tag"]][doi]) > 0) {
        agg.resp <- "SWAbulk"
      } else {
        agg.resp <- req_aggs[["tag"]][doi]
      }
      #"VWCbulk", "VWCmatric", "SWCbulk", "SWPmatric", "SWAbulk"
      agg.analysis <- switch(EXPR = agg.resp,
        AET = 1,
        Transpiration = 2,
        EvaporationSoil = 1,
        EvaporationSurface = 1,
        EvaporationTotal = 1,
        VWCbulk = 2,
        VWCmatric = 2,
        SWCbulk = 2,
        SWPmatric = 2,
        SWAbulk = 2,
        Snowpack = 1,
        Rain = 1,
        Snowfall = 1,
        Snowmelt = 1,
        SnowLoss = 1,
        Infiltration = 1,
        DeepDrainage = 1,
        PET = 1,
        TotalPrecipitation = 1,
        TemperatureMin = 1,
        TemperatureMax = 1,
        SoilTemperature = 2,
        Runoff = 1,
        Runon = 1)
      tableName <- paste0("aggregation_doy_", req_aggs[["tag"]][doi])

      if (agg.analysis == 1) {
        SQL_Table_Definitions1 <- paste0("CREATE TABLE \"", tableName,
          "_Mean\" (", dailySQL, ");")
        SQL_Table_Definitions2 <- paste0("CREATE TABLE \"", tableName,
          "_SD\" (", dailySQL, ");")

      } else {
        SQL_Table_Definitions1 <- paste0("CREATE TABLE \"", tableName,
          "_Mean\" (", dailyLayersSQL, ");")
        SQL_Table_Definitions2 <- paste0("CREATE TABLE \"", tableName,
          "_SD\" (", dailyLayersSQL, ");")
      }

      dbExecute(con_dbOut, paste(SQL_Table_Definitions1, collapse = "\n"))
      dbExecute(con_dbOut, paste(SQL_Table_Definitions2, collapse = "\n"))
    }
  }

  list(dailySQL = dailySQL, dailyLayersSQL = dailyLayersSQL)
}


dbOutput_create_EnsembleTables <- function(con_dbOut, dbOutput, sim_scens,
  meanString, sdString, dailySQL, dailyLayersSQL, do_ensembles = TRUE,
  wipe_dbOut = FALSE) {

  if (!do_ensembles)
    return(invisible(NULL))

  Tables <- dbOutput_ListOutputTables(con = con_dbOut)
  Tables <- grep("_sd", Tables, ignore.case = TRUE, invert = TRUE, value = TRUE)
  Tables <- sub("_Mean", "", Tables, ignore.case = TRUE)

  respName <- sub("aggregation_", "", Tables, ignore.case = TRUE)
  respName <- sub("doy_", "", respName, ignore.case = TRUE)
  respName <- sub("atSWPcrit[0-9]+kPa", "", respName)

  dbEnsemblesFilePaths <- file.path(dirname(dbOutput), paste0("dbEnsemble_",
    Tables, ".sqlite3"))

  for (i in seq_along(dbEnsemblesFilePaths)) {
    if (wipe_dbOut && file.exists(dbEnsemblesFilePaths[i])) {
      unlink(dbEnsemblesFilePaths[i])
    }

    con <- dbConnect(SQLite(),
      dbname = dbEnsemblesFilePaths[i])
    on.exit(dbDisconnect(con), add = TRUE)
    set_PRAGMAs(con, PRAGMA_settings2())

    for (j in seq_along(sim_scens[["ensemble.families"]])) {
      for (k in seq_along(sim_scens[["ensemble.levels"]])) {
        EnsembleFamilyLevelTables <- paste0(
          sim_scens[["ensemble.families"]][j], "_rank_",
          formatC(sim_scens[["ensemble.levels"]][k], width = 2, flag = "0"),
          "_", c("means", "sds",
            if (sim_scens[["save.scenario.ranks"]]) "scenarioranks"))

        if (grepl("overall", respName[i], ignore.case = TRUE)) {
          sql1 <- paste0("CREATE TABLE \"", EnsembleFamilyLevelTables[1],
            "\" (", meanString, ");")
          sql2 <- paste0("CREATE TABLE \"", EnsembleFamilyLevelTables[2],
            "\" (", sdString, ");")
          sql3 <- if (sim_scens[["save.scenario.ranks"]]) {
              paste0("CREATE TABLE \"", EnsembleFamilyLevelTables[3], "\" (",
                gsub("REAL", "INTEGER", meanString), ");")
            } else NULL

        } else {
          agg.analysis <- switch(
            EXPR = respName[i],
            AET = 1,
            Transpiration = 2,
            EvaporationSoil = 1,
            EvaporationSurface = 1,
            EvaporationTotal = 1,
            VWCbulk = 2,
            VWCmatric = 2,
            SWCbulk = 2,
            SWPmatric = 2,
            SWAbulk = 2,
            Snowpack = 1,
            Rain = 1,
            Snowfall = 1,
            Snowmelt = 1,
            SnowLoss = 1,
            Infiltration = 1,
            DeepDrainage = 1,
            PET = 1,
            TotalPrecipitation = 1,
            TemperatureMin = 1,
            TemperatureMax = 1,
            SoilTemperature = 2,
            Runoff = 1,
            Runon = 1
          )

          if (agg.analysis == 1) {
            sql1 <- paste0("CREATE TABLE \"", EnsembleFamilyLevelTables[1],
              "\" (", dailySQL, ");")
            sql2 <- paste0("CREATE TABLE \"", EnsembleFamilyLevelTables[2],
              "\" (", dailySQL, ");")
            sql3 <- if (sim_scens[["save.scenario.ranks"]]) {
              paste0("CREATE TABLE \"", EnsembleFamilyLevelTables[3],
                "\" (", gsub("REAL", "INTEGER", dailySQL), ");")
            } else NULL

          } else {
            sql1 <- paste0("CREATE TABLE \"", EnsembleFamilyLevelTables[1],
              "\" (", dailyLayersSQL, ");")
            sql2 <- paste0("CREATE TABLE \"", EnsembleFamilyLevelTables[2],
              "\" (", dailyLayersSQL, ");")
            sql3 <- if (sim_scens[["save.scenario.ranks"]]) {
              paste0("CREATE TABLE \"", EnsembleFamilyLevelTables[3], "\" (",
                gsub("REAL", "INTEGER", dailyLayersSQL), ");")
            } else NULL
          }
        }

        dbExecute(con, sql1)
        dbExecute(con, sql2)
        if (sim_scens[["save.scenario.ranks"]])
          dbExecute(con, sql3)
      }
    }
  }

  invisible(NULL)
}


#' Create \var{\sQuote{dbOutput}} if requested and/or not already present
#'
#' @section NOTE: Do not change the design of the output database without
#'   adjusting the index functions \code{it_Pid}, \code{it_exp}, and
#'   \code{it_site} (see part 4)
#' @return An integer value. The number of fields in the
#'   \var{\sQuote{overall_aggregation}} tables minus 1 (i.e., the field
#'   \var{\dQuote{P_id}} is not counted here)
#' @export
make_dbOutput <- function(SFSW2_prj_meta, SFSW2_prj_inputs, verbose = FALSE) {

  if (verbose) {
    t1 <- Sys.time()
    temp_call <- shQuote(match.call()[1])
    print(paste0("rSFSW2's ", temp_call, ": started at ", t1))

    on.exit({
      print(paste0("rSFSW2's ", temp_call, ": ended after ",
      round(difftime(Sys.time(), t1, units = "secs"), 2), " s")); cat("\n")
      },
      add = TRUE)
  }

  if (SFSW2_prj_meta[["prj_todos"]][["wipe_dbOut"]] &&
    file.exists(SFSW2_prj_meta[["fnames_out"]][["dbOutput"]])) {

    unlink(SFSW2_prj_meta[["fnames_out"]][["dbOutput"]])
  }

  con_dbOut <- try(dbConnect(SQLite(),
    dbname = SFSW2_prj_meta[["fnames_out"]][["dbOutput"]]))
  on.exit(dbDisconnect(con_dbOut), add = TRUE)

  if (inherits(con_dbOut, "try-error")) {
    unlink(SFSW2_prj_meta[["fnames_out"]][["dbOutput"]])
    stop(paste("Creation of output database failed:", con_dbOut,
      collapse = ", "))
  }

  set_PRAGMAs(con_dbOut, PRAGMA_settings2())

  tables <- dbListTables(con_dbOut)

  #--- Check whether dbOutput exists and has a suitable design
  # Has suitable tables?
  isgood <- length(tables) > 0 &&
    all(dbOutput_ListDesignTables() %in% tables) &&
    "aggregation_overall_mean" %in% tables

  if (isgood) {
    temp <- dbListFields(con_dbOut, "aggregation_overall_mean")
    ncol_dbOut_overall <- length(temp) - 1L

    fields <- generate_OverallAggregation_fields(
      aon = SFSW2_prj_meta[["prj_todos"]][["aon"]],
      opt_agg = SFSW2_prj_meta[["opt_agg"]])

    # Has correct (number of) fields in table `aggregation_overall_mean`
    isgood <- isgood && ncol_dbOut_overall == sum(fields[, "N"]) &&
      identical(temp[-1], unlist(fields[, "fields"]))

    if (isgood) {
      return(list(fields = fields, ncol_dbOut_overall = ncol_dbOut_overall))
    }
  }

  #--- dbOutput needs to be created
  # Add design and output tables
  dbOutput_create_Design(con_dbOut, SFSW2_prj_meta, SFSW2_prj_inputs)

  fields <- generate_OverallAggregation_fields(
    aon = SFSW2_prj_meta[["prj_todos"]][["aon"]],
    opt_agg = SFSW2_prj_meta[["opt_agg"]])

  res_oa <- dbOutput_create_OverallAggregationTable(con_dbOut, fields)
  add_dbOutput_index(con_dbOut)

  res_da <- dbOutput_create_DailyAggregationTable(con_dbOut,
    req_aggs = SFSW2_prj_meta[["prj_todos"]][["adaily"]])

  if (SFSW2_prj_meta[["prj_todos"]][["do_ensembles"]]) {
    dbOutput_create_EnsembleTables(con_dbOut,
      dbOutput = SFSW2_prj_meta[["fnames_out"]][["dbOutput"]],
      sim_scens = SFSW2_prj_meta[["sim_scens"]],
      meanString = res_oa[["meanString"]], sdString = res_oa[["sdString"]],
      dailySQL = res_da[["dailySQL"]],
      dailyLayersSQL = res_da[["dailyLayersSQL"]],
      do_ensembles = SFSW2_prj_meta[["prj_todos"]][["do_ensembles"]],
      wipe_dbOut = SFSW2_prj_meta[["prj_todos"]][["wipe_dbOut"]])
  }

  #--- run optimize on database
  dbExecute(con_dbOut, "PRAGMA optimize")

  list(fields = fields, ncol_dbOut_overall = res_oa[["ncol_dbOut_overall"]])
}


make_dbTempOut <- function(dbOutput, dir_out_temp, fields, adaily,
  verbose = FALSE) {

  if (verbose) {
    t1 <- Sys.time()
    temp_call <- shQuote(match.call()[1])
    print(paste0("rSFSW2's ", temp_call, ": started at ", t1))

    on.exit({
      print(paste0("rSFSW2's ", temp_call, ": ended after ",
      round(difftime(Sys.time(), t1, units = "secs"), 2), " s")); cat("\n")
      },
      add = TRUE)
  }


  # IDs of temporary dbOutputs
  IDs <- if (SFSW2_glovars[["p_has"]]) {
      seq_len(SFSW2_glovars[["p_workersN"]])
    } else 0L
  fnames_dbTempOut <- file.path(dir_out_temp,
    paste0("SQL_Node_", IDs, ".sqlite3"))

  # Tables for temporary dbOutputs
  dbOut_tables <- dbOutput_ListOutputTables(dbname = dbOutput)

  # Create temporary dbOutput
  on.exit(dbDisconnect(con), add = TRUE)

  for (k in seq_along(IDs)) {
    if (verbose) {
      print(paste("Processing dbTempOut",
        shQuote(basename(fnames_dbTempOut[k])), "for node", k))
    }

    con <- try(dbConnect(SQLite(),
      dbname = fnames_dbTempOut[k]), silent = !verbose)

    if (inherits(con, "try-error")) {
      stop(paste("Creation of temporary output database failed:", con,
        collapse = ", "))
    }

    set_PRAGMAs(con, PRAGMA_settings2())
    tables <- dbListTables(con)

    #--- Check whether temporary dbOutput exists and has a suitable design
    # Has suitable tables?
    isgood <- length(tables) > 0 && all(dbOut_tables %in% tables) &&
      "aggregation_overall_mean" %in% tables

    if (isgood) {
      # Has correct (number of) fields in table `aggregation_overall_mean`
      temp <- dbListFields(con, "aggregation_overall_mean")
      ncols <- length(temp) - 1L

      isgood <- isgood && ncols == sum(fields[, "N"]) &&
        identical(temp[-1], unlist(fields[, "fields"]))
    }

    if (!isgood) {
      temp <- dbOutput_create_OverallAggregationTable(con, fields)
      add_dbOutput_index(con)

      temp <- dbOutput_create_DailyAggregationTable(con, adaily)
    }

    # Close connection
    dbDisconnect(con)
  }

  # Remove dbDisconnect-call from on.exit
  oe <- sys.on.exit()
  oe <- remove_from_onexit_expression(oe, "dbDisconnect")
  do.call(on.exit, args = c(list(oe), add = FALSE))

  invisible(fnames_dbTempOut)
}


#' Add fields to an existing \var{\sQuote{dbOutput}}
#'
#' You realize that you want additional output fields after starting a
#' simulation project; or, the package is updated while you are working on a
#' simulation, and produces now additional output fields for output options that
#' are active in your simulation project. In either case, you don't want to
#' discard data that is already in \var{\sQuote{dbOutput}}.
#'
#' @param SFSW2_prj_meta See elsewhere
#' @param col_ids An integer vector. If \code{NULL} then the code will match old
#'   and new fields automatically. If not \code{NULL} and its length is equal to
#'   the number of fields in the old table, then this information is used to
#'   transfer data. Possible use case: field names have changed, but they
#'   represent the same output.
#' @param chunksize An integer value. Chunks used to transfer data to the new
#'   table.
#' @param verbose A logical value.
#'
#' @export
dbOutput_update_OverallAggregationTable <- function(SFSW2_prj_meta, # nolint
  col_ids = NULL, chunksize = 1000, verbose = FALSE) {

  con_dbOut <- dbConnect(SQLite(),
    dbname = SFSW2_prj_meta[["fnames_out"]][["dbOutput"]])
  on.exit(dbDisconnect(con_dbOut), add = TRUE)

  tdata <- c("aggregation_overall_mean", "aggregation_overall_sd")
  told <- c("ao_mean_old", "ao_sd_old")

  # rename old tables (potentially) with data
  for (k in seq_along(tdata)) {
    dbExecute(con_dbOut, paste("ALTER TABLE", tdata[k], "RENAME TO",
      told[k]))
  }

  # create new tables
  fields <- generate_OverallAggregation_fields(
    aon = SFSW2_prj_meta[["prj_todos"]][["aon"]],
    opt_agg = SFSW2_prj_meta[["opt_agg"]])

  dbOutput_create_OverallAggregationTable(con_dbOut, fields)

  for (k in seq_along(tdata)) {
    hasfields <- dbListFields(con_dbOut, told[k])
    newfields <- dbListFields(con_dbOut, tdata[k])
    col_ids_use <- if (length(col_ids) == length(hasfields)) {
        col_ids
      } else {
        match(hasfields, newfields, nomatch = 0)
      }

    sql_hasfields <- paste(paste0("\"", hasfields[col_ids_use > 0], "\""),
      collapse = ", ")
    sql_newfields <- paste(paste0("\"", newfields[col_ids_use], "\""),
      collapse = ", ")

    P_ids <- dbGetQuery(con_dbOut, paste("SELECT P_id FROM", told[k]))[, 1]
    recordsN <- length(P_ids)
    if (recordsN > 0) {
      seq_ids <- parallel::splitIndices(recordsN, ceiling(recordsN / chunksize))
      seqN <- length(seq_ids)

      # transfer data from old to new tables
      for (chunk_ids in seq_len(seqN)) {
        if (verbose) {
          print(paste0("'dbOutput_update_OverallAggregationTable': ",
            Sys.time(), " transfering data batch ", chunk_ids, "/", seqN,
            " to table ", shQuote(tdata[k])))
        }

        dbWithTransaction(con_dbOut, {
          dbExecute(con_dbOut, paste("INSERT INTO", tdata[k], "(",
            sql_newfields, ")",
            "SELECT", sql_hasfields, "FROM", told[k], "WHERE P_id = :x"),
            params = list(x = seq_ids[[chunk_ids]]))
        })
      }

      # delete old table if new table contains same P_ids
      P_ids_new <- dbGetQuery(con_dbOut,
        paste("SELECT P_id FROM", tdata[k]))[, 1]
      if (identical(sort(P_ids), sort(P_ids_new))) {
        dbExecute(con_dbOut, paste("DROP TABLE", told[k]))
      } else {
        stop("Updated table", shQuote(tdata[k]), " is missing n= ",
          length(setdiff(P_ids, P_ids_new)), " Pids")
      }

    } else {
      # delete empty old table
      dbExecute(con_dbOut, paste("DROP TABLE", told[k]))
    }
  }

  # Clean up database
  dbExecute(con_dbOut, "VACUUM")
  add_dbOutput_index(con_dbOut)

  invisible(TRUE)
}




#' Compare one output database with another output database
#'
#' @param dbOut1 A character string. Path to first output database used as
#'   reference.
#' @param dbOut2 A character string. Path to second output database.
#' @param tol A numeric value. Differences smaller than tolerance are not
#'   reported. Passed to \code{\link{all.equal}}.
#' @param comp_absolute A logical value. If \code{TRUE} then absolute
#'   comparisons will be reported, otherwise relative differences. See argument
#'   \code{scale} of function \code{\link{all.equal}}.
#' @param verbose A logical value. If \code{TRUE} then messages are printed.
#'
#' @return A (possibly empty) list of character vectors describing differences
#'   between \code{dbOut2} and \code{dbOut1} databases.
#'
#' @seealso \code{\link{all.equal}}
#' @export
compare_two_dbOutput <- function(dbOut1, dbOut2, tol = 1e-3,
  comp_absolute = TRUE, verbose = FALSE) {

  diff_msgs <- list()
  if (verbose)
    on.exit(if (length(diff_msgs) > 0) print(diff_msgs))

  if (!file.exists(dbOut1)) {
    diff_msgs <- c(diff_msgs, paste(Sys.time(), shQuote(basename(dbOut1)),
      "cannot be located."))
    return(diff_msgs)
  }
  refDB <- dbConnect(SQLite(), dbOut1)

  if (!file.exists(dbOut2)) {
    diff_msgs <- c(diff_msgs, paste(Sys.time(), shQuote(basename(dbOut2)),
      "cannot be located."))
    return(diff_msgs)
  }
  testDB <- dbConnect(SQLite(), dbOut2)

  #---Identify set of shared tables
  refDB_tables <- setdiff(dbListTables(refDB),
    dbOutput_ListInternalTables())
  testDB_tables <- setdiff(dbListTables(testDB),
    dbOutput_ListInternalTables())
  tocomp_tables <- intersect(refDB_tables, testDB_tables)
  if (length(tocomp_tables) == 0L) {
    diff_msgs <- c(diff_msgs,
      paste(Sys.time(), shQuote(basename(dbOut1)), "and",
        shQuote(basename(dbOut2)), "do not contain shared tables"))
    return(diff_msgs)
  }

  testDB_tables_comp <- testDB_tables %in% tocomp_tables
  if (any(!testDB_tables_comp)) {
    diff_msgs <- c(diff_msgs, paste0(shQuote(basename(dbOut2)),
      " contains tables without analog in ", shQuote(basename(dbOut1)), ": ",
      paste(shQuote(testDB_tables[!testDB_tables_comp]), collapse = ", ")))
  }
  refDB_tables_comp <- refDB_tables %in% tocomp_tables
  if (any(!refDB_tables_comp)) {
    diff_msgs <- c(diff_msgs, paste0(shQuote(basename(dbOut1)),
      " contains tables without analog in ", shQuote(basename(dbOut2)), ": ",
      paste(shQuote(refDB_tables[!refDB_tables_comp]), collapse = ", ")))
  }

  #---Confirm that 'design' of test agrees with reference
  has_samedesign <- dbOutput_ListDesignTables() %in% tocomp_tables
  diff_design <- NULL

  if (all(has_samedesign)) {
    diff_design <- sapply(dbOutput_ListDesignTables(), function(desT) {
      temp <- dbReadTable(refDB, desT)
      x_ref <- temp[do.call("order", unname(temp)), ]

      temp <- dbReadTable(testDB, desT)
      x_test <- temp[do.call("order", unname(temp)), ]

      all.equal(x_ref, x_test)
    })

    has_samedesign <- sapply(diff_design, function(x)
      is.logical(x) && isTRUE(x))
  }

  if (any(!has_samedesign)) {
    diff_msgs <- c(diff_msgs,
      paste(Sys.time(), shQuote(basename(dbOut1)), "and",
        shQuote(basename(dbOut2)), "have a different design"),
      if (!is.null(diff_design)) diff_design[!has_samedesign] else NULL)
  }

  temp <- !(tocomp_tables %in% dbOutput_ListDesignTables())
  tocomp_tables <- tocomp_tables[temp]


  #---Loop over shared result tables and compare shared fields
  for (k in seq_along(tocomp_tables)) {
    #---Identify set of shared fields
    refDB_fields <- dbListFields(refDB, tocomp_tables[k])
    testDB_fields <- dbListFields(testDB, tocomp_tables[k])
    tocomp_fields <- intersect(refDB_fields, testDB_fields)
    if (length(tocomp_fields) == 0L) {
      diff_msgs <- c(diff_msgs, paste("Table", shQuote(tocomp_tables[k]),
        "contains no shared fields between the databases"))
      next
    }

    # Must have 'P_id' as first field
    if (!("P_id" %in% tocomp_fields)) {
      diff_msgs <- c(diff_msgs, paste("The unique identifier 'P_id' is not a ",
        "shared field in table", shQuote(tocomp_tables[k]),
        "preventing any comparison"))
      next
    }
    tocomp_fields <- c("P_id", tocomp_fields[!("P_id" == tocomp_fields)])

    # If field 'Soil_Layer' is present, then it must be shared and be the
    # second field
    ref_has_sl <- any("Soil_Layer" %in% refDB_fields)
    test_has_sl <- any("Soil_Layer" %in% testDB_fields)
    if (xor(ref_has_sl, test_has_sl)) {
      diff_msgs <- c(diff_msgs, paste("The soil layer identifier 'Soil_Layer'",
        "is not a shared field in table", shQuote(tocomp_tables[k]),
        "preventing any comparison"))
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
      diff_msgs <- c(diff_msgs, paste0("Table ", shQuote(tocomp_tables[k]),
        " of ", shQuote(basename(dbOut2)), " contains fields without an ",
        " analog in ", shQuote(basename(dbOut1)), ": ",
        paste(shQuote(testDB_fields[!testDB_fields_comp]), collapse = ", ")))
    }
    refDB_fields_comp <- refDB_fields %in% tocomp_fields
    if (any(!refDB_fields_comp)) {
      diff_msgs <- c(diff_msgs, paste0("Table ", shQuote(tocomp_tables[k]),
        " of ", shQuote(basename(dbOut1)), " contains fields without an ",
        " analog in ", shQuote(basename(dbOut2)), ": ",
        paste(shQuote(refDB_fields[!refDB_fields_comp]), collapse = ", ")))
    }

    #---Extract field data, sorted by 'P_id' (and 'Soil_Layer')
    sql <- paste0("SELECT ",
      paste0("\"", tocomp_fields, "\"", collapse = ", "),
      " FROM ", tocomp_tables[k],
      " ORDER BY P_id",
      if (ref_has_sl && test_has_sl) ", Soil_Layer",
      ";")

    x_ref <- dbGetQuery(refDB, sql)
    x_test <- dbGetQuery(testDB, sql)

    #---Compare field data and report if differences were found
    ident <- all.equal(x_ref, x_test, tol = tol,
      scale = if (comp_absolute) 1 else NULL)
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


dbOut_prepare1 <- function(dbOut_fname, dbNew_fname, fields_include = NULL,
  fields_exclude = NULL) {

  #--- Connect to dbOut
  con_dbOut <- dbConnect(SQLite(), dbname = dbOut_fname, flags = SQLITE_RW)

  # Attach dbNew
  dbNew_fname <- normalizePath(dbNew_fname)
  dbExecute(con_dbOut, paste("ATTACH",
    dbQuoteIdentifier(con_dbOut, dbNew_fname), "AS dbNew"))

  #--- Identify tables and fields to update
  has_tables0 <- dbOutput_ListOutputTables(con = con_dbOut)
  has_tables_new <- dbOutput_ListOutputTables(dbname = dbNew_fname)

  # Find output tables
  req_tables <- if (is.null(fields_include)) {
      has_tables_new
    } else {
      names(fields_include)
    }

  # Confirm that requested tables are available in dbOut and dbNew
  has_temp <- req_tables %in% has_tables0 & req_tables %in% has_tables_new
  not_temp <- !has_temp
  if (any(not_temp)) {
    print(paste("Requested tables not available:",
      paste0(shQuote(req_tables[not_temp]), collapse = ", ")))
  }

  tables <- req_tables[has_temp]
  tables_w_soillayers <- dbOutput_Tables_have_SoilLayers(tables,
    con = con_dbOut)


  # Find fields to match and fields to update the output values
  fields_design <- c("P_id", "Soil_Layer")
  result_fields <- design <- list()

  for (k in seq_along(tables)) {
    fields0 <- dbListFields(con_dbOut, tables[k])
    fields_new <- names(dbGetQuery(con_dbOut, paste0("SELECT * FROM dbNew.",
      dbQuoteIdentifier(con_dbOut, tables[k]), " LIMIT 0")))

    temp <- seq_len(if (tables_w_soillayers[tables[k]]) 2 else 1)
    design[[k]] <- fields_design[temp]

    req_fields <- if (is.null(fields_include) ||
        is.null(fields_include[[tables[k]]])) {
        fields_new
      } else {
        fields_include[[tables[k]]]
      }

    # Exclude design fields
    temp <- req_fields %in% design[[k]]
    req_fields <- req_fields[!temp]

    # Exclude fields requested to be excluded
    if (!(is.null(fields_exclude) || is.null(fields_exclude[[tables[k]]]))) {
      temp <- req_fields %in% fields_exclude[[tables[k]]]
      req_fields <- req_fields[!temp]
    }

    # Confirm that requested fields are available in dbOut and dbNew
    has_temp <- req_fields %in% fields0 & req_fields %in% fields_new
    not_temp <- !has_temp
    if (any(not_temp)) {
      print(paste("Requested fields not available:",
        paste0(shQuote(req_fields[not_temp]), collapse = ", ")))
    }

    result_fields[[k]] <- req_fields[has_temp]
  }

  dbExecute(con_dbOut, "DETACH dbNew")

  list(con_dbOut = con_dbOut, tables = tables, fields = result_fields,
    design = design)
}

#' Check that cells of \var{dbOutput} agree with corresponding cells of another
#' database
#'
#' @param dbOut_fname A character string. The file path of the main
#'   \var{\code{dbOutput}} that is to be updated.
#' @param dbNew_fname A character string. The file path of a database with
#'   values that are to be compared against \var{\code{dbOutput}}.
#' @param fields_check A named list of vectors with character strings. The
#'   field names per table that are used must have equal values in the original
#'   and the new database for a record to be checked. If \code{NULL},
#'   then all output tables, according to
#'   \code{\link{dbOutput_ListOutputTables}}, and all fields
#'   (except for ID-fields, i.e., \var{\code{P_id}} and \var{\code{Soil_Layer}})
#'   are checked.
#' @param tol A numeric value. Differences smaller than tolerance are not
#'   considered.
#' @param verbose A logical value.
#' @return The connection to an in-memory database with one table that tracks
#'   which records (identified by \var{\code{P_id}}) agree (value 1) and which
#'   records do not agree (value 0) for each table (as field names). Value of
#'   records that were not compared is \code{NA}/\code{NULL}.
#'
#' @examples
#' \dontrun{
#'   con_dbCheck <- dbOut_check_values(
#'     dbOut_fname = SFSW2_prj_meta[["fnames_out"]][["dbOutput"]],
#'     dbNew_fname = "path/to/new.sqlite3",
#'     fields_check = list(
#'       aggregation_overall_mean = c("MAT_C_mean", "MAP_mm_mean"),
#'       aggregation_overall_sd = c("MAT_C_sd", "MAP_mm_sd"),
#'     )
#'
#'   table <- dbListTables(con_dbCheck)
#'   fields <- dbQuoteIdentifier(con_dbCheck, dbListFields(con_dbCheck, table))
#'
#'   # Extract Pids from records that matched up
#'   sql <- paste("SELECT P_id FROM", table, "WHERE",
#'     paste(fields[-1], "= 1", collapse = " AND "))
#'   is_good <- dbGetQuery(con_dbCheck, sql)
#'
#'   # Extract Pids from records that did not match up; this should be empty
#'   sql <- paste("SELECT P_id FROM", table, "WHERE",
#'     paste(fields[-1], "= 0", collapse = " OR "))
#'   is_bad <- dbGetQuery(con_dbCheck, sql)
#' }
#'
#' @export
dbOut_check_values <- function(dbOut_fname, dbNew_fname, fields_check = NULL,
  tol = 1e-3, verbose = FALSE) {

  if (verbose) {
    t1 <- Sys.time()
    temp_call <- shQuote(match.call()[1])
    print(paste0("rSFSW2's ", temp_call, ": started at ", t1))

    on.exit({
      print(paste0("rSFSW2's ", temp_call, ": ended after ",
        round(difftime(Sys.time(), t1, units = "secs"), 2), " s")); cat("\n")
    },
      add = TRUE)
  }

  prep <- dbOut_prepare1(dbOut_fname, dbNew_fname,
    fields_include = fields_check)
  dbDisconnect(prep[["con_dbOut"]])

  #--- Create in-memory database with one table that tracks comparison
  con_res <- dbConnect(SQLite(), ":memory:")
  table_comparison <- paste0("Comparison_", format(Sys.Date(), "%Y%m%d"))
  sql <- paste0("CREATE TABLE ", table_comparison, " ",
    "(P_id INTEGER PRIMARY KEY, ",
    paste(prep[["tables"]], "INTEGER DEFAULT NULL", collapse = ", "), ")")
  dbExecute(con_res, sql)

  # Attach dbOut and fill table with P_id
  sql <- paste("ATTACH", dbQuoteIdentifier(con_res, dbOut_fname), "AS dbOut")
  dbExecute(con_res, sql)

  sql <- paste("INSERT INTO", table_comparison, "(P_id)",
    "SELECT P_id FROM dbOut.runs")
  dbExecute(con_res, sql)


  #--- Check values
  # Attach dbNew
  sql <- paste("ATTACH", dbQuoteIdentifier(con_res, dbNew_fname), "AS dbNew")
  dbExecute(con_res, sql)

  for (k in seq_along(prep[["tables"]])) if (length(prep[["fields"]][[k]])) {
    tfield <- ttable <- dbQuoteIdentifier(con_res, prep[["tables"]][k])
    tchecks <-  dbQuoteIdentifier(con_res, prep[["fields"]][[k]])
    tdesign <- dbQuoteIdentifier(con_res, prep[["design"]][[k]])

    if (verbose) {
      print(paste0(Sys.time(), ": checking table ",
        shQuote(prep[["tables"]][k]), " with ", length(prep[["fields"]][[k]]),
        " fields."))
    }

    sql_design <- paste0(tdesign, collapse = ",")
    sql_join_NtoM <- paste0("dbOut.", ttable, " JOIN dbNew.", ttable,
      " USING (", sql_design, ")")
    sql_checked <- paste0("dbNew.", ttable, ".", tchecks,
      " BETWEEN dbOut.", ttable, ".", tchecks, " - ", tol, " AND ",
        "dbOut.", ttable, ".", tchecks, " + ", tol,
      collapse = " AND ")
    sql_not_checked <- paste0("dbNew.", ttable, ".", tchecks,
      " NOT BETWEEN dbOut.", ttable, ".", tchecks, " - ", tol, " AND ",
        "dbOut.", ttable, ".", tchecks, " + ", tol,
      collapse = " AND ")

    if (FALSE) {
      # Determine count of matching records
      sql <- paste0("SELECT COUNT(*) FROM ",
        "(SELECT P_id FROM ", sql_join_NtoM, " WHERE ", sql_checked, ")")
      print(as.integer(dbGetQuery(con_res, sql)))
    }

    # Update records that match between dbNew and dbOut
    sql <- paste0(
      "UPDATE ", table_comparison, " ",
      "SET ", tfield, " = 1 ",
      "WHERE P_id IN ",
        "(SELECT P_id FROM ", sql_join_NtoM, " WHERE ", sql_checked, ")")
    dbExecute(con_res, sql)

    # Update records that do not match between dbNew and dbOut, but which are
    # in dbNew
    sql <- paste0(
      "UPDATE ", table_comparison, " ",
      "SET ", tfield, " = 0 ",
      "WHERE ",
        "(SELECT P_id FROM ", sql_join_NtoM, " WHERE ", sql_not_checked, ")")
    dbExecute(con_res, sql)
  }

  #--- Clean up
  dbExecute(con_res, "DETACH dbOut")
  dbExecute(con_res, "DETACH dbNew")

  con_res
}


#' Update values of \var{dbOutput} based on a new database
#'
#' @param dbOut_fname A character string. The file path of the main
#'   \var{\code{dbOutput}} that is to be updated.
#' @param dbNew_fname A character string. The file path of a database with
#'   new values that are used to update corresponding values in
#'   \var{\code{dbOutput}}.
#' @param fields_update A named list of vectors with character strings. The
#'   field names per table to be updated. Each table is represented by a
#'   correspondingly named element. If \code{NULL}, then all output tables,
#'   according to \code{\link{dbOutput_ListOutputTables}}, and all fields
#'   (except for ID-fields, i.e., \var{\code{P_id}} and \var{\code{Soil_Layer}})
#'   are updated.
#' @param fields_exclude A named list of vectors with character strings. The
#'   field names per table to be updated. Each table is represented by a
#'   correspondingly named element. If \code{NULL}, then no fields are excluded
#'   from the update operation.
#' @param verbose A logical value.
#' @return Invisibly, the name of a new table that tracks which records
#'   (identified by \var{\code{P_id}}) have been updated (value 1) for each
#'   table.
#'
#' @examples
#' \dontrun{
#'   table <- dbOut_update_values(
#'     dbOut_fname = SFSW2_prj_meta[["fnames_out"]][["dbOutput"]],
#'     dbNew_fname = "path/to/new.sqlite3",
#'     fields_exclude = list(
#'       aggregation_overall_mean = c("MAT_C_mean", "MAP_mm_mean"),
#'       aggregation_overall_sd = c("MAT_C_sd", "MAP_mm_sd")))
#'
#'   con <- dbConnect(SQLite(), SFSW2_prj_meta[["fnames_out"]][["dbOutput"]])
#'   fields <- dbQuoteIdentifier(con, dbListFields(con, table))
#'
#'   # Extract Pids from records that were updated
#'   sql <- paste("SELECT P_id FROM", table, "WHERE",
#'     paste(fields[-1], "= 1", collapse = " AND "))
#'   is_good <- dbGetQuery(con, sql)
#'
#'   dbDisconnect(con)
#' }
#'
#' @export
dbOut_update_values <- function(dbOut_fname, dbNew_fname, fields_update = NULL,
  fields_exclude = NULL, verbose = FALSE) {

  if (verbose) {
    t1 <- Sys.time()
    temp_call <- shQuote(match.call()[1])
    print(paste0("rSFSW2's ", temp_call, ": started at ", t1))

    on.exit({
      print(paste0("rSFSW2's ", temp_call, ": ended after ",
        round(difftime(Sys.time(), t1, units = "secs"), 2), " s")); cat("\n")
    },
      add = TRUE)
  }

  prep <- dbOut_prepare1(dbOut_fname, dbNew_fname,
    fields_include = fields_update, fields_exclude = fields_exclude)
  on.exit(dbDisconnect(prep[["con_dbOut"]]), add = TRUE)

  #--- Add table that tracks updated cells (insert 0 = not updated as default)
  table_updated <- paste0("Updated_", format(Sys.Date(), "%Y%m%d"))
  sql <- paste0("CREATE TABLE IF NOT EXISTS ", table_updated, " ",
    "(P_id INTEGER PRIMARY KEY, ",
      paste(prep[["tables"]], "INTEGER DEFAULT 0", collapse = ", "), ")")
  dbExecute(prep[["con_dbOut"]], sql)

  sql <- paste("INSERT INTO", table_updated, "(P_id)",
    "SELECT P_id FROM runs")
  dbExecute(prep[["con_dbOut"]], sql)

  #--- Update values
  # Attach dbNew
  sql <- paste("ATTACH", dbQuoteIdentifier(prep[["con_dbOut"]], dbNew_fname),
    "AS dbNew")
  dbExecute(prep[["con_dbOut"]], sql)

  for (k in seq_along(prep[["tables"]])) {
    ttable <- dbQuoteIdentifier(prep[["con_dbOut"]], prep[["tables"]][k])
    tfields <- dbQuoteIdentifier(prep[["con_dbOut"]], prep[["fields"]][[k]])
    tdesign <- dbQuoteIdentifier(prep[["con_dbOut"]], prep[["design"]][[k]])

    sql_fields <- paste0(tfields, collapse = ",")
    sql_where <- paste0("dbNew.", ttable, ".", tdesign,
      " = main.", ttable, ".", tdesign, collapse = " AND ")

    # Determine count of records to update
    sql <- paste0("SELECT COUNT(*) FROM dbNew.", ttable, " WHERE ",
      gsub("main.", "", sql_where))
    n <- as.integer(dbGetQuery(prep[["con_dbOut"]], sql))

    if (verbose) {
      print(paste0(Sys.time(), ": updating table ",
        shQuote(prep[["tables"]][k]), ": ", length(prep[["fields"]][[k]]),
        " fields of ", n, " records."))
    }

    # Update records in main
    sql <- paste0(
      "UPDATE ", ttable, " ",
      "SET (", sql_fields, ") = ",
        "(SELECT ", sql_fields, " FROM dbNew.", ttable,
        " WHERE ", sql_where, ") ",
      "WHERE ", tdesign,
        " IN (SELECT ", tdesign, " FROM dbNew.", ttable, ")")

    res <- dbExecute(prep[["con_dbOut"]], sql)

    # Check that correct number of records was updated
    if (isTRUE(all.equal(res, n))) {
      # Enter information of updated records into tracking table
      sql <- paste(
        "UPDATE", table_updated,
        "SET", ttable, " = 1 ",
        "WHERE P_id IN (SELECT DISTINCT P_id FROM dbNew.", ttable, ")")
      dbExecute(prep[["con_dbOut"]], sql)

    } else {
      print(paste("Update of table", shQuote(prep[["tables"]][k]), "failed:",
        res, "instead of", n, "records updated."))
    }
  }

  #--- Clean up
  dbExecute(prep[["con_dbOut"]], "DETACH dbNew")
  dbExecute(prep[["con_dbOut"]], "PRAGMA optimize")

  invisible(table_updated)
}
