#--------------------------------------------------------------------------------------------------#

#------CODE developed and written by
# - Daniel R Schlaepfer (dschlaep@uwyo.edu, drs): 2009-2016
#for contact and further information see also: sites.google.com/site/drschlaepfer

#------DISCLAIMER: This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#--------------------------------------------------------------------------------------------------#


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
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbName)
  res <- DBI::dbListTables(con)
  RSQLite::dbDisconnect(con)

  res
}

list.dbVariables <- function(dbName, dbTable) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbName)
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
    con <- RSQLite::dbConnect(RSQLite::SQLite(), fdbSWSF)
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
    con <- RSQLite::dbConnect(RSQLite::SQLite(), fdbSWSF)
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
    con <- RSQLite::dbConnect(RSQLite::SQLite(), fdbSWSF)
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
      con <- RSQLite::dbConnect(RSQLite::SQLite(), fdbSWSF)
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
