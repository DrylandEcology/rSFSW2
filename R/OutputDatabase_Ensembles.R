update_scenarios_with_ensembles <- function(SFSW2_prj_meta) {
  sim_scens <- SFSW2_prj_meta[["sim_scens"]]

  if (length(sim_scens[["ensemble.levels"]]) > 0)
    sim_scens[["ensemble.levels"]] <- sort(sim_scens[["ensemble.levels"]])

  sim_scens[["has_ensembles"]] <- nrow(sim_scens[["df"]]) > 1 &&
    !is.null(sim_scens[["ensemble.families"]]) &&
    length(sim_scens[["ensemble.levels"]]) > 0 &&
    all(is.numeric(sim_scens[["ensemble.levels"]]))

  if (sim_scens[["has_ensembles"]]) {
    sim_scens[["ensemble.families"]] <- paste0(rownames(SFSW2_prj_meta[["sim_time"]][["future_yrs"]]), ".",
      rep(sim_scens[["ensemble.families"]], each = nrow(SFSW2_prj_meta[["sim_time"]][["future_yrs"]])))  #add (multiple) future_yrs
    scenarios.ineach.ensemble <- sapply(
      sim_scens[["ensemble.families"]],
      function(x) {
        grepl(x, sim_scens[["df"]][, "id_sim"], ignore.case = TRUE)
      }
    )
    temp <- apply(scenarios.ineach.ensemble, 2, any)

    sim_scens[["ensemble.families"]] <- sim_scens[["ensemble.families"]][temp]
    scenarios.ineach.ensemble <- scenarios.ineach.ensemble[, temp]
    families_N <- length(sim_scens[["ensemble.families"]])

    if (families_N > 1) {
      scenariosPERensemble_N <- max(temp <- apply(scenarios.ineach.ensemble, 2, sum))
      stopifnot(any(sim_scens[["ensemble.levels"]] <= min(temp)))

    } else{
      scenariosPERensemble_N <- sum(scenarios.ineach.ensemble)
      stopifnot(any(sim_scens[["ensemble.levels"]] <= scenariosPERensemble_N))
    }
  }

  SFSW2_prj_meta[["sim_scens"]] <- sim_scens

  SFSW2_prj_meta
}


  calc.ensembles <- function(dat, elevels) {
    # dat must be three-dimensional object with dims = (runs, outputs, scenarios); runs and/or scenarios can be 1 or larger
    doRanks <- function(x) {
      temp <- sort.int(x, na.last = NA, index.return = TRUE)

      c(temp$x[elevels], temp$ix[elevels])
    }

    res <- NULL
    col.names <- colnames(dat)
    if (dim(dat)[3] == 1) { #only one scenario; i.e., all levels are identical to the scenario
      temp <- array(data = rep(unlist(dat), each = 3), dim = c(length(elevels), dim(dat)[1], dim(dat)[2]))
      res <- array(data = 1, dim = c(2*length(elevels), dim(dat)[1], dim(dat)[2]))
      res[1:length(elevels), ,] <- temp
    } else {
      if (dim(dat)[1] > 1) {
        res <- apply(dat[, ,], MARGIN = c(1, 2), doRanks)
      } else { #only one run = site
        res <- apply(dat[, ,], MARGIN = 1, doRanks)
        res <- array(data = res, dim = c(2*length(elevels), 1, dim(dat)[2]))
      }
    }
    #returned object: array with 3 dims: 1. dim = 1:length(elevels) are the ensembles at the ensemble.levels; the second set of rows are the ranked GCMs; 2. dim = runs/sites; 3. dim = aggregated variables
    dimnames(res) <- list(NULL, NULL, col.names)

    res
  }

  collect_EnsembleFromScenarios <- function(Table, name.OutputDB, t.overall, opt_job_time,
    dir_out, sim_scens, ensemble.families, ensemble.levels, save.scenario.ranks,
    opt_chunks) {

    con <- dbConnect(SQLite(), dbname = name.OutputDB)
    on.exit(dbDisconnect(con), add = TRUE)

    #########TIMING#########
    TableTimeStop <- Sys.time() - t.overall
    units(TableTimeStop) <- "secs"
    TableTimeStop <- as.double(TableTimeStop)
    print(paste0("Table: ", Table, ": started at ", TableTime <- Sys.time()))

    #########FUNCTIONS######
    doWrite <- function(dat, headerInfo, elevel, outfile) {
      #add info to
      name <- ensemble.family
      if (is.vector(dat)) {
        dat <- cbind(headerInfo, t(dat))
      } else {
        dat <- cbind(headerInfo, dat)
      }

      sql <- paste0("INSERT INTO ", outfile, " VALUES (", paste0(paste(":", colnames(dat)),
          collapse = ", "), ")")
      rs <- dbSendStatement(conEnsembleDB, sql)
      dbBind(rs, params = as.list(dat))
      dbClearResult(rs)

      written <- 1
      #written <- RSQLite::dbWriteTable(conEnsembleDB, name = outfile, dat, row.names = FALSE, append = TRUE)#
      if (written)
        return(1)
      else
        return(0)
    }
    read.scenarios <- function(Table, start, stop, ensemble.family, export.header = TRUE) {
      #Read first file
      columns <- RSQLite::dbListFields(con, Table)[-1]
      if (Layers <- any(temp <- grepl(pattern = "Soil_Layer", x = columns))) columns <- columns[-temp]
      columns <- paste0("\"", columns, "\"", collapse = ", ")
      sqlString <- paste0("SELECT '", Table, "'.P_id AS P_id, header.Scenario AS Scenario, ", columns, " FROM '", Table, "' INNER JOIN header ON '", Table, "'.P_id = header.P_id WHERE header.P_id BETWEEN ", start, " AND ", stop, " AND header.Scenario LIKE '%", tolower(ensemble.family), "%'", " ORDER BY P_id;")
      res <- dbSendStatement(con, sqlString)
      dataScen.Mean <- dbFetch(res, n = -1) #dataToQuantilize get the data from the query n = -1 to get all rows
      dbClearResult(res)

      columnCutoff <- match("Scenario", colnames(dataScen.Mean))
      if (export.header) {
        sqlString <- paste0("SELECT '", Table, "'.P_id AS P_id ", if (Layers) ", Soil_Layer ", "FROM '", Table, "', header WHERE '", Table, "'.P_id = header.P_id AND header.P_id BETWEEN ", start, " AND ", stop, " AND header.Scenario = 'Current' ORDER BY P_id;")
        res <- dbSendStatement(con, sqlString)
        headerInfo <- dbFetch(res, n = -1) #dataToQuantilize get the data from the query n = -1 to get all rows
        dbClearResult(res)
      }
      col.names <- colnames(dataScen.Mean[, -(1:columnCutoff)])
      #We have all the scenarios in the family. We need to get unique scenario names and group them by that
      data.temp <- lapply(unique(dataScen.Mean$Scenario), function (x) dataScen.Mean[dataScen.Mean$Scenario == x, -(1:columnCutoff)])
      data.temp <- array(data = unlist(data.temp), dim = c(nrow(data.temp[[1]]), ncol(data.temp[[1]]), length(data.temp)))
      colnames(data.temp) <- col.names
      class(data.temp) <- "numeric"

      if (export.header) {
        return(list(headerInfo = headerInfo, data.scen = data.temp))
      } else {
        return(list(data.scen = data.temp))
      }
    }

    if (!(TableTimeStop > (opt_job_time[["wall_time_s"]]-1*60)) | !SFSW2_glovars[["p_has"]] | !identical(SFSW2_glovars[["p_type"]], "mpi")) {#figure need at least 3 hours for big ones
      tfile <- file.path(dir_out, paste0("dbEnsemble_", sub(pattern = "_Mean", replacement = "", Table, ignore.case = TRUE), ".sqlite3"))
      conEnsembleDB <- dbConnect(SQLite(), dbname = tfile)
      on.exit(dbDisconnect(conEnsembleDB), add = TRUE)

      nfiles <- 0
      #Grab x rows at a time
      SQL <- paste0("SELECT MAX(P_id) FROM '", Table, "';")
      maxP_id <- as.integer(dbGetQuery(con, SQL))
      maxRun_id <- (maxP_id/nrow(sim_scens[["df"]]))

      for (j in 1:length(ensemble.families)) {
        EnsembleTimeStop <- Sys.time() - t.overall
        units(EnsembleTimeStop) <- "secs"
        EnsembleTimeStop <- as.double(EnsembleTimeStop)
        if ((EnsembleTimeStop > (opt_job_time[["wall_time_s"]]-1*60)) & SFSW2_glovars[["p_has"]] & identical(SFSW2_glovars[["p_type"]], "mpi")) {#figure need at least 4 hours for a ensemble
          break
        }
        print(paste0("Table: ", Table, ", Ensemble: ", ensemble.families[j], " started at ", EnsembleTime <- Sys.time()))

        ensemble.family = ensemble.families[j]
        EnsembleFamilyLevelTables <- paste0(ensemble.family, "_rank_", formatC(ensemble.levels, width = 2, flag = "0"), "_", c("means", "sds", if (save.scenario.ranks) "scenarioranks"))
        LastPid <- integer(length = length(EnsembleFamilyLevelTables))
        for (i in 1:length(LastPid)) {
          SQL <- paste0("SELECT MAX(P_id) FROM '", EnsembleFamilyLevelTables[i], "';")
          LastPid[i] <- as.integer(dbGetQuery(conEnsembleDB, SQL))+(nrow(sim_scens[["df"]])-1)#Need to add all the scenarios because last P_id will always be Current
        }
        if (any(is.na(LastPid))) { #If any of the tables are empty we need to start at the beginning
          minRun_id <- 1
        } else {
          minRun_id <- (min(LastPid)/nrow(sim_scens[["df"]]))+1 #This is already done so we add one
        }
        #########################
        for (i in seq(minRun_id, maxRun_id, opt_chunks[["ensembleCollectSize"]])) {
          start <- (i-1)*nrow(sim_scens[["df"]])+1
          stop <- (min(i+opt_chunks[["ensembleCollectSize"]]-1, maxRun_id)-1)*nrow(sim_scens[["df"]])+nrow(sim_scens[["df"]])
          dataScen.Mean <- read.scenarios(Table = Table, start = start, stop = stop, ensemble.family = ensemble.family, export.header = TRUE)
          Table <- sub(pattern = "Mean", replacement = "stats::sd", Table)
          dataScen_sd <- read.scenarios(Table = Table, start = start, stop = stop, ensemble.family = ensemble.family, export.header = FALSE)
          Table <- sub(pattern = "stats::sd", replacement = "Mean", Table)
          #get ensembles for non-stats::sd file
          dataEns.Mean <- calc.ensembles(dat = dataScen.Mean$data.scen, elevels = ensemble.levels)
          #Lookup stats::sd values from scenarios based on ranks determined from taking ensembles of the means
          if (length(dim(dataEns.Mean[(length(ensemble.levels) + 1):(2*length(ensemble.levels)), ,])) == 2) {
            lookup <- aperm(dataEns.Mean[(length(ensemble.levels) + 1):(2*length(ensemble.levels)), ,], perm = c(2, 1))
            make <- array(c(lookup, dataScen_sd$data.scen), dim = c(nrow(lookup), length(ensemble.levels) + dim(dataScen_sd$data.scen)[3]))
            dataEns_sd <- apply(make, MARGIN = 1, FUN = function(lookANDscen) lookANDscen[(length(ensemble.levels)+1):dim(make)[2]][lookANDscen[1:length(ensemble.levels)]])
            dimnames(dataEns_sd)[2] <- dimnames(dataScen_sd$data.scen)[2]
          } else {
            lookup <- aperm(dataEns.Mean[(length(ensemble.levels) + 1):(2*length(ensemble.levels)), ,], perm = c(2, 3, 1))
            make <- array(c(lookup, dataScen_sd$data.scen), dim = c(nrow(lookup), ncol(lookup), length(ensemble.levels) + dim(dataScen_sd$data.scen)[3]))
            dataEns_sd <- apply(make, MARGIN = c(1, 2), FUN = function(lookANDscen) lookANDscen[(length(ensemble.levels)+1):dim(make)[3]][lookANDscen[1:length(ensemble.levels)]])
            dimnames(dataEns_sd)[3] <- dimnames(dataScen_sd$data.scen)[2]
          }
          #write ensemble files
          ntemp <- 0
          for (k in 1:length(ensemble.levels)) {
            outputs <- paste0(ensemble.family, "_rank_", formatC(ensemble.levels[k], width = 2, flag = "0"), "_", c("means", "sds", "scenarioranks"))
            ntemp <- ntemp + doWrite(dat = dataEns.Mean[k, ,], headerInfo = dataScen.Mean$headerInfo, elevel = ensemble.levels[k], outfile = paste0("'", outputs[1], "'"))
            if (length(dim(dataEns.Mean[(length(ensemble.levels) + 1):(2*length(ensemble.levels)), ,])) == 2) {
              ntemp <- ntemp + doWrite(dat = dataEns_sd[k, ], headerInfo = dataScen.Mean$headerInfo, elevel = ensemble.levels[k], outfile = paste0("'", outputs[2], "'"))
            } else {
              ntemp <- ntemp + doWrite(dat = dataEns_sd[k, ,], headerInfo = dataScen.Mean$headerInfo, elevel = ensemble.levels[k], outfile = paste0("'", outputs[2], "'"))
            }
            if (save.scenario.ranks) ntemp <- ntemp + doWrite(dat = dataEns.Mean[length(ensemble.levels) + k, ,], headerInfo = dataScen.Mean$headerInfo, elevel = ensemble.levels[k], outfile = paste0("'", outputs[3], "'"))
          }
          if (i == 1) nfiles <- nfiles + ntemp
          print(paste0("          ", i, ":", min(i+opt_chunks[["ensembleCollectSize"]]-1, maxRun_id), " of ", maxRun_id, " done."))
        }
        #########################
        temp2 <- Sys.time() - EnsembleTime
        units(temp2) <- "secs"
        temp2 <- as.double(temp2)
        print(paste0("Table: ", Table, ", Ensemble: ", ensemble.families[j], " ended at ", Sys.time(), ", after ", round(temp2), " s."))
      }
    }
    temp2 <- Sys.time() - TableTime
    units(temp2) <- "secs"
    temp2 <- as.double(temp2)
    print(paste0("Table: ", Table, " ended at ", Sys.time(), ", after ", round(temp2), " s."))

    nfiles
  }


generate_ensembles <- function(SFSW2_prj_meta, t_job_start, opt_parallel, opt_chunks,
  verbose = FALSE) {

  con <- dbConnect(SQLite(),
    dbname = SFSW2_prj_meta[["fnames_out"]][["dbOutput"]])
  on.exit(dbDisconnect(con), add = TRUE)

  Tables <- dbOutput_ListOutputTables(con)
  Tables <- Tables[-grep(pattern = "_sd", Tables, ignore.case = TRUE)]

  if (SFSW2_glovars[["p_has"]]) {
    #call the simulations depending on parallel backend

    if (identical(SFSW2_glovars[["p_type"]], "mpi")) {

      ensembles.completed <- Rmpi::mpi.applyLB(Tables, collect_EnsembleFromScenarios,
        name.OutputDB = SFSW2_prj_meta[["fnames_out"]][["dbOutput"]],
        t.overall = t_job_start, opt_job_time = opt_parallel[["opt_job_time"]],
        dir_out = SFSW2_prj_meta[["project_paths"]][["dir_out"]],
        sim_scens = SFSW2_prj_meta[["sim_scens"]],
        opt_chunks = opt_chunks)

    } else if (identical(SFSW2_glovars[["p_type"]], "socket")) {

      ensembles.completed <- parallel::clusterApplyLB(SFSW2_glovars[["p_cl"]],
        x = Tables, fun = collect_EnsembleFromScenarios,
        name.OutputDB = SFSW2_prj_meta[["fnames_out"]][["dbOutput"]],
        t.overall = t_job_start, opt_job_time = opt_parallel[["opt_job_time"]],
        dir_out = SFSW2_prj_meta[["project_paths"]][["dir_out"]],
        sim_scens = SFSW2_prj_meta[["sim_scens"]],
        opt_chunks = opt_chunks)
    }

  } else {
    ensembles.completed <- lapply(Tables, FUN = collect_EnsembleFromScenarios,
        name.OutputDB = SFSW2_prj_meta[["fnames_out"]][["dbOutput"]],
        t.overall = t_job_start, opt_job_time = opt_parallel[["opt_job_time"]],
        dir_out = SFSW2_prj_meta[["project_paths"]][["dir_out"]],
        sim_scens = SFSW2_prj_meta[["sim_scens"]],
        opt_chunks = opt_chunks)
  }

  ensembles.completed <- sum(unlist(ensembles.completed))

  temp <- {if (SFSW2_prj_meta[["sim_scens"]][["save.scenario.ranks"]]) 3 else 2} *
    length(Tables) * length(SFSW2_prj_meta[["sim_scens"]][["ensemble.families"]]) *
    length(SFSW2_prj_meta[["sim_scens"]][["ensemble.levels"]])

  if (ensembles.completed != temp)
    print(paste("rSFSW2 calculates ensembles: something went wrong with ensemble output:",
      "ensembles.completed = ", ensembles.completed, " instead of ", temp, "."))

  ensembles.completed
}
