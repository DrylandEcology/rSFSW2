#' Checks data in a weather database
#'
#' @param dir_prj A character string. The directory path the rSFSW2 simulation project.
#' @param fdbWeather A character string. The file path of weather database.
#' @param repeats An integer value. The number of times each weather object is extracted
#   (repeats > 1 enable comparison of the duplicates).
#' @param do_preprocess_tempfiles A logial value. Set to TRUE, for instance, if a
#'   previous run was prematurely aborted.
#' @param seed A seed set, \code{NULL}, or \code{NA}. \code{NA} will not affect
#'  the state of the RNG; \code{NULL} will re-initialize the RNG; and all other values
#'  are passed to \code{\link{set.seed}}.
#'
#' @export
check_weatherDB <- function(dir_prj, fdbWeather, repeats = 2L,
  do_preprocess_tempfiles = TRUE, n_cores = 20L, startyear = 1979, endyear = 2010,
  seed = NA) {

  #---Settings
  if (!is.na(seed)) set.seed(seed)
  name_wid <- ".wid"

  vars <- c("MAP_mm", "aPPT_mm_sd", "MAT_C", "MATmax_C", "MATmin_C")
  vars_mult <- c("MAP_mm")

  #---Paths
  dir.create(dir_out <- file.path(dir_prj, "6_Results", "Weather_summary"),
    recursive = TRUE, showWarnings = FALSE)
  dir.create(dir_temp <- file.path(dir_out, "temp"), recursive = TRUE,
    showWarnings = FALSE)

  pattern_temp <- "Summary_climate_dbWeatherData_temp_worker"  # Basename of temporary files, one for each worker, for storing extracted results
  ftemp <- file.path(dir_out, "Summary_climate_dbWeatherData_temp.csv") # Temporary file, aggregated from worker output temp files
  fdups <- file.path(dir_out, "Summary_climate_dbWeatherData_duplicated.csv")  # Duplicated extractions with non-identical output
  fout <- file.path(dir_out, "Summary_climate_dbWeatherData.csv")  # Successful extractions
  fclimate <- file.path(dir_out, paste0("Summary_climate_dbWeatherData_",
    format(Sys.Date(), "%Y%m%d"), ".rds"))  # 'climate'


  #---Calculate in parallel
  cl <- parallel::makeCluster(n_cores, type = "PSOCK", outfile = "workers_log.txt")
  temp <- parallel::clusterExport(cl, c("name_wid"))
  #temp <- parallel::clusterEvalQ(cl, paste(Sys.info()[['nodename']], Sys.getpid(), sep = '-'))
#TODO (drs): it is ok to load into globalenv() because this happens on workers and not on master;
#  -> R CMD CHECK reports this nevertheless as issue
  temp <- parallel::clusterApply(cl, seq_len(n_cores), function(x)
    assign(name_wid, x, envir = globalenv())) # worker identification number


  merge_workers_tempfiles <- function(dir_temp, pattern, file) {
    ftemps <- list.files(dir_temp, pattern = pattern, full.names = TRUE)
    if (length(ftemps) > 0) {
      temps <- lapply(ftemps, function(f) utils::read.csv(f, header = TRUE))
      res <- do.call("rbind", temps)

      stopifnot(nrow(res) == sum(vapply(temps, nrow, NA_integer_)))

      utils::write.table(res, file = file, append = TRUE, sep = ",", dec = ".",
        qmethod = "double", row.names = FALSE, col.names = FALSE)
      unlink(ftemps)
    }
  }


  #---Connect to weather database
  rSOILWAT2::dbW_setConnection(dbFilePath = fdbWeather, FALSE)
  fsite <- file.path(file.path(dir_out, "Sites.cvs"))
  if (!file.exists(fsite)) {
    dbW_iSiteTable <- rSOILWAT2::dbW_getSiteTable()
    utils::write.csv(dbW_iSiteTable, file = fsite, row.names = FALSE)
  } else {
    dbW_iSiteTable <- utils::read.csv(fsite, header = TRUE)
  }
  fscen <- file.path(file.path(dir_out, "Scenarios.cvs"))
  if (!file.exists(fscen)) {
    dbW_iScenarioTable <- rSOILWAT2::dbW_getScenariosTable()
    utils::write.csv(dbW_iScenarioTable, file = fscen, row.names = FALSE)
  } else {
    dbW_iScenarioTable <- utils::read.csv(fscen, header = TRUE)
  }
  rSOILWAT2::dbW_disconnectConnection()



  #---Define output
  #  Non-empty rows in 'climate' will be extracted
  used_sites <- dbW_iSiteTable$Latitude > -90 & dbW_iSiteTable$Latitude > -180
  sitesN <- sum(used_sites)

  climate <- as.data.frame(matrix(NA, nrow = nrow(dbW_iScenarioTable) * sitesN,
                    ncol = 3 + length(vars),
                    dimnames = list(NULL, c("Site_id", "Scenario_id", "Status", vars))))
  climate[, "Site_id"] <- dbW_iSiteTable[used_sites, "Site_id"]
  climate[, "Scenario_id"] <- rep(dbW_iScenarioTable[, "id"], each = sitesN)

  ids_todo <- seq_len(nrow(climate))


  #---Check progress
  make_ids <- compiler::cmpfun(function(data, id_vars = c("Site_id", "Scenario_id")) {
    as.vector(apply(data[, id_vars, drop = FALSE], 1, paste0, collapse = "_"))
  })

  revert_ids <- compiler::cmpfun(function(ids, id_vars = c("Site_id", "Scenario_id"), id_class = "integer") {
    temp <- as.data.frame(t(simplify2array(strsplit(ids, split = "_", fixed = TRUE))), stringsAsFactors = FALSE)
    stopifnot(length(id_vars) == ncol(temp))
    colnames(temp) <- id_vars

    id_class <- rep_len(id_class, length.out = ncol(temp))
    for (i in seq_len(ncol(temp))) {
      temp[, i] <- as(temp[, i], id_class[i])
    }

    temp
  })

  copy_matches <- compiler::cmpfun(function(out, data, match_vars, copy_vars,
    ids_out = NULL) {

    out_ids <- make_ids(out, id_vars = match_vars)
    data_ids <- make_ids(data, id_vars = match_vars)

    im_data <- match(out_ids, data_ids, nomatch = 0)
    im_out <- out_ids %in% data_ids

    out[im_out, copy_vars] <- data[im_data, copy_vars]

    list(out = out, ids_out = if (!is.null(ids_out)) ids_out[!im_out] else NA)
  })

  process_good_extractions <- function(climate, ids_todo, var_out = vars, file = fout) {
    if (file.exists(fout)) {
      climate_good <- utils::read.csv(fout, header = TRUE)

      # Transfer to output
      if (nrow(climate_good) > 0) {
        temp <- copy_matches(out = climate, data = climate_good,
                  match_vars = c("Site_id", "Scenario_id"),
                  copy_vars = c("Status", vars),
                  ids_out = ids_todo)
        climate <- temp[["out"]]
        ids_todo <- temp[["ids_out"]]
      }
    } else {
      climate_good <- climate[0, ]
      utils::write.table(climate_good, file = file, append = FALSE, sep = ",", dec = ".",
        qmethod = "double", row.names = FALSE, col.names = TRUE)
    }

    list(climate = climate, ids_todo = ids_todo)
  }

  paste0(Sys.time(), ": process previous progress")

  progress <- process_good_extractions(climate, ids_todo)
  climate <- progress[["climate"]]
  ids_todo <- progress[["ids_todo"]]


  check_entry <- compiler::cmpfun(function(i, idss, data, vars, repeats) {
    # 3 types of entries in 'data'
    #  - # of duplicates < repeats ==> (-1) repeat: do not copy to 'climate'; leave lines in 'ftemp'
    #  - # of duplicates >= repeats and
    #    - identical output ==> (1) good extraction: copy to 'fout' and 'climate'; remove lines from 'ftemp'
    #    - varied output ==> (0) repeat: do not copy to 'climate'; move duplicated entries to 'fdups'

    if (i %% 1000 == 1) print(paste0(Sys.time(), ": checking ", i, "-th entry"))

    idss <- unlist(idss)
    tr <- data[data[, "Site_id"] == idss["Site_id"] & data[, "Scenario_id"] == idss["Scenario_id"], ]

    res <- unlist(c(seq_id = i, Dups_N = nrow(tr),
          tr[1, c("Site_id", "Scenario_id")],
          Status = -1L))

    if (NROW(tr) >= repeats) {
      variation <- apply(tr[, c("Status", vars)], 2, function(x)
              # duplicates with and without NAs OR with different values among non-NAs
              anyNA(x) && !all(is.na(x)) || any(abs(diff(stats::na.omit(x))) > sqrt(.Machine$double.eps)))
      res["Status"] <- if (any(variation)) 0L else 1L
    }

    res
  })


  process_tempfiles <- function(climate, ids_todo, var_out = vars, ftemp, fdups, fout,
    repeats, dir_temp, pattern_temp) {

    merge_workers_tempfiles(dir_temp, pattern = pattern_temp, file = ftemp)

    if (file.exists(ftemp)) {
      climate_progress <- utils::read.csv(ftemp, header = TRUE)

      if (nrow(climate_progress) > 0) {
        ids_progress <- as.vector(apply(climate_progress[, c("Site_id", "Scenario_id")],
          1, paste0, collapse = "_"))
        ids_unique <- sort(unique(ids_progress))
        idus_ss <- revert_ids(ids_unique)

        fstat1 <- sub(".csv", "_uniqueIDs.rds", ftemp)
        fstat2 <- sub(".csv", "_status.rds", ftemp)
        do_calc_status <- TRUE

        if (file.exists(fstat1) && file.exists(fstat2)) {
          ids_unique_prev <- readRDS(fstat1)
          do_calc_status <- !identical(ids_unique_prev, ids_unique)
        }

        if (do_calc_status) {
          saveRDS(ids_unique, file = fstat1)
          parallel::clusterExport(cl, c("climate_progress", "idus_ss", "repeats", "vars",
            "check_entry"), envir = environment())

          print(paste0(Sys.time(), ": 'process_tempfiles' check status of n = ",
            length(ids_unique), " extractions; this may take a while."))
          status <- data.frame(t(parallel::parSapply(cl, X = seq_along(ids_unique), FUN = function(i)
            check_entry(i, idss = idus_ss[i, ], data = climate_progress, vars = vars,
              repeats = repeats))))
          saveRDS(status, file = fstat2)
          temp <- parallel::clusterEvalQ(cl, rm(list = ls()))
        } else {
          status <- readRDS(fstat2)
        }

        print(paste0(Sys.time(), ": 'process_tempfiles' process successful database extractions"))
        #  - # of duplicates < repeats ==> (-1) repeat: do not copy to 'climate'; leave lines in 'ftemp'
        ids_keep <- rep(TRUE, nrow(climate_progress))
        #  - # of duplicates >= repeats and
        #    - identical output ==> (1) good extraction: write one copy to 'fout' and add to 'climate'; remove all lines from 'ftemp'
        igood <- status[, "Status"] == 1L
        ids_good <- ids_progress %in% ids_unique[status[igood, "seq_id"]]
        climate_good <- unique(climate_progress[ids_good, ])
        stopifnot(nrow(climate_good) == sum(igood), sum(ids_good) == sum(status[igood, "Dups_N"]))

        if (nrow(climate_good) > 0) {
          utils::write.table(climate_good, file = fout, append = TRUE, sep = ",", dec = ".",
            qmethod = "double", row.names = FALSE, col.names = FALSE)
          temp <- copy_matches(out = climate, data = climate_good,
                    match_vars = c("Site_id", "Scenario_id"),
                    copy_vars = c("Status", vars),
                    ids_out = ids_todo)
          climate <- temp[["out"]]
          ids_todo <- temp[["ids_out"]]
        }

        ids_keep <- ids_keep & !ids_good

        print(paste0(Sys.time(), ": 'process_tempfiles' process database extractions with variation among repeats"))
        #    - varied output ==> (0) repeat: do not copy to 'climate'; move duplicated entries to 'fdups'
        ids_vdups <- ids_unique[status[status[, "Status"] == 0L, "seq_id"]]
        idups <- ids_progress %in% ids_vdups

        climate_dups <- climate_progress[idups, ]
        appD <- file.exists(fdups)
        utils::write.table(climate_dups, file = fdups, append = appD, sep = ",", dec = ".",
          qmethod = "double", row.names = FALSE, col.names = !appD)

        ids_keep <- ids_keep & !idups
        utils::write.table(climate_progress[ids_keep, ], file = ftemp, append = FALSE, sep = ",",
          dec = ".", qmethod = "double", row.names = FALSE, col.names = TRUE)
      }

    } else {
      climate_progress <- climate[0, ]
      utils::write.table(climate_progress, file = ftemp, append = FALSE, sep = ",", dec = ".",
        qmethod = "double", row.names = FALSE, col.names = TRUE)
    }

    list(climate = climate, ids_todo = ids_todo)
  }

  paste0(Sys.time(), ": process previous output")

  if (do_preprocess_tempfiles) {
    progress2 <- process_tempfiles(climate, ids_todo, vars, ftemp, fdups, fout, repeats,
      dir_temp, pattern_temp)
    climate <- progress2[["climate"]]
    ids_todo <- progress2[["ids_todo"]]
  }


  #---Go through the weather database
  summarize_weather <- compiler::cmpfun(function(i, iclimate, scen, startyear, endyear,
    db_name) {

    stopifnot(exists("outfile"), file.exists(outfile))

    if (i %% 1000 == 1)
      print(paste0(Sys.time(), ": run = ", i, ": site_id/scenario = ",
        iclimate["Site_id"], "/", scen))

    # Access data from database
    wtemp <- try(rSOILWAT2::dbW_getWeatherData(Site_id = iclimate["Site_id"],
            startYear = startyear, endYear = endyear,
            Scenario = scen),
          silent = TRUE)

    if (inherits(wtemp, "try-error")) {
      # Maybe the connection to the database failed? Re-set connection and attempt extraction once more
      rSOILWAT2::dbW_disconnectConnection()
      rSOILWAT2::dbW_setConnection(dbFilePath = db_name, FALSE)

      wtemp <- try(rSOILWAT2::dbW_getWeatherData(Site_id = iclimate["Site_id"],
              startYear = startyear, endYear = endyear,
              Scenario = scen),
            silent = TRUE)
    }

    if (inherits(wtemp, "try-error")) {
      iclimate["Status"] <- 0
      print(paste0(Sys.time(), ": run = ", i, ": site_id/scenario = ",
        iclimate["Site_id"], "/", scen, " failed:", wtemp))

    } else {
      iclimate["Status"] <- 1
      wd <- rSOILWAT2::dbW_weatherData_to_dataframe(wtemp)

      # Calculate climate variables (this is the slow part of the function)
      wy_ppt <- tapply(wd[, "PPT_cm"], wd[, "Year"], sum)
      iclimate["MAP_mm"] <- round(mean(wy_ppt))
      iclimate["aPPT_mm_sd"] <- round(stats::sd(wy_ppt), 2)

      wy_tmax <- tapply(wd[, "Tmax_C"], wd[, "Year"], mean)
      iclimate["MATmax_C"] <- round(mean(wy_tmax), 2)

      wy_tmin <- tapply(wd[, "Tmin_C"], wd[, "Year"], mean)
      iclimate["MATmin_C"] <- round(mean(wy_tmin), 2)

      #wd_tmean <- apply(wd[, c("Tmax_C", "Tmin_C")], 1, mean)
      #wy_tmean <- tapply(wd_tmean, wd[, "Year"], mean)
      wy_tmean <- apply(cbind(wy_tmax, wy_tmin), 1, mean)
      iclimate["MAT_C"] <- round(mean(wy_tmean), 2)
    }

    # Temporary output
    utils::write.table(iclimate, file = outfile, append = TRUE, sep = ",", dec = ".",
      qmethod = "double", row.names = FALSE, col.names = FALSE)

    i
  })


  # Calculate in parallel
  if (length(ids_todo) > 0) {
    print(paste0(Sys.time(), ": # run =", length(ids_todo), " out of", nrow(climate)))

    parallel::clusterExport(cl, c("climate", "dir_temp", "pattern_temp", "name_wid",
      "summarize_weather", "fdbWeather", "dbW_iScenarioTable", "startyear", "endyear"))
    temp <- parallel::clusterEvalQ(cl, {
      require("rSOILWAT2")
      rSOILWAT2::dbW_setConnection(dbFilePath = fdbWeather, FALSE)
    })
    temp <- parallel::clusterEvalQ(cl, {
      outfile <- file.path(dir_temp, paste0(pattern_temp, "-", get(name_wid), ".csv"))
      utils::write.table(climate[0, ], file = outfile, append = FALSE, sep = ",", dec = ".",
        qmethod = "double", row.names = FALSE, col.names = TRUE)
    })

    itests <- unlist(lapply(seq_len(repeats), function(i) sample(x = ids_todo,
      size = length(ids_todo))))

    idone <- parallel::parSapply(cl, X = itests, FUN = function(i) summarize_weather(i,
      iclimate = climate[i, ],
      scen = dbW_iScenarioTable[as.integer(climate[i, "Scenario_id"]), "Scenario"],
      startyear = startyear, endyear = endyear, db_name = fdbWeather))

    temp <- parallel::clusterEvalQ(cl, rSOILWAT2::dbW_disconnectConnection())
  }

  #---Final save
  if (length(ids_todo) > 0) {
    print(paste0(Sys.time(), ": process new output"))

    progress3 <- process_tempfiles(climate, ids_todo, vars, ftemp, fdups, fout, repeats,
      dir_temp, pattern_temp)
    climate <- progress3[["climate"]]
    ids_todo <- progress3[["ids_todo"]]
  }

  print(paste0(Sys.time(), ": script completed"))
  if (length(ids_todo) > 0)
    print(paste("# run =", length(ids_todo), "out of", nrow(climate), "still to do"))

  saveRDS(climate, file = fclimate)

  #---Clean-up
  parallel::stopCluster(cl)


  #---Report on extracted 'climate'
  # Failures
  failed <- climate$Status == 0
  iNAs <- apply(climate, 1, anyNA)
  identical(failed, iNAs)
  print(paste0("Unsuccessful extractions: n = ", sum(failed), "; f = ",
    signif(sum(failed) / length(failed), 2)))
  with(climate[failed, ], plot(Site_id, Scenario_id))

  failed_siteID <- climate[failed, "Site_id"]
  print(paste0("Sites with at least one unsuccessful extractions: n = ",
    length(unique(failed_siteID))))
  failed_siteID_freq <- tapply(rep(1, sum(failed)), failed_siteID, sum)
  probs <- c(0, 0.01, 0.5, 0.99, 1)
  print(paste0("Unsuccessful extractions per scenario: quantiles: ", paste(probs,
    stats::quantile(failed_siteID_freq, probs = probs), sep = "% = ", collapse = ", ")))

  failed_scenID <- climate[failed, "Scenario_id"]
  print(paste0("Scenarios with at least one unsuccessful extractions: n = ",
    length(unique(failed_scenID))))
  print(paste0("Unsuccessful extractions per scenario: n = "))
    print(table(failed_scenID))


  # Variation among downscaled scenarios as difference to current

  dat <- climate[!failed & climate$Scenario_id > 1, ]
  dat_cur <- climate[climate$Site_id %in% dat$Site_id & climate$Scenario_id == 1, ]
  dat <- dat[do.call(order, dat), ]

  dat_cur <- copy_matches(out = dat, data = dat_cur[do.call(order, dat_cur), ],
          match_vars = c("Site_id"),
          copy_vars = vars)[["out"]]

  dat_diff <- dat
  for (iv in vars) {
    dat_diff[, iv] <- if (iv %in% vars_mult) {
                dat[, iv] / dat_cur[, iv]
              } else {
                dat[, iv] - dat_cur[, iv]
              }
  }

  for (iv in vars) {
    print(paste0("Mean variation within sites among downscaled scenarios for variable ", iv))
    temp <- stats::aggregate(dat_diff[, iv], by = list(dat_diff$Site_id), FUN = function(x) {
      rx <- range(x)
      c(mean = mean(x), min = min(rx), max = max(rx), range = diff(rx))
    })
    print(round(apply(temp[[2]], 2, mean), 2))
  }

  dat
}
