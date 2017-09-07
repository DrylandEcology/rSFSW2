
#' Compare two netCDF files
do_compare_nc <- function(fnc1, fnc2, var) {
  stopifnot(requireNamespace("ncdf4"))

  nc1 <- ncdf4::nc_open(fnc1)
  nc2 <- ncdf4::nc_open(fnc2)

  comp <- list()
  temp <- unlist(do_compare(nc1, nc2))
  temp <- temp[!is.na(temp)]
  comp[["layout"]] <- temp[!grepl("id|group_id|filename", names(temp))]

  comp[["var"]] <- do_compare(ncdf4::ncvar_get(nc1, var), ncdf4::ncvar_get(nc2, var))
  comp[["varname"]] <- var

  ncdf4::nc_close(nc1)
  ncdf4::nc_close(nc2)

  comp
}

calc_ncfile_times <- function(x_times, ts_expected = NULL) {
  N <- length(x_times)

  temp_yr <- data.frame(
    start = as.integer(substr(x_times, 1, 4)),
    end = as.integer(substr(x_times, 8, 11)))
  temp_mo <- data.frame(
    start = as.integer(substr(x_times, 5, 6)),
    end = as.integer(substr(x_times, 12, 13)))

  tsl_yrmo <- lapply(seq_len(N), function(k) {
    temp <- seq.Date(
      from = as.Date(ISOdate(temp_yr[k, "start"], temp_mo[k, "start"], 15)),
      to = as.Date(ISOdate(temp_yr[k, "end"], temp_mo[k, "end"], 15)), by = "month")
  })
  for (k in seq_len(N)) {
    ts_yrmo <- if (k > 1) c(ts_yrmo, tsl_yrmo[[k]]) else tsl_yrmo[[k]]
  }

  if (is.null(ts_expected)) {
    ts_expected <- seq.Date(
      from = as.Date(ISOdate(temp_yr[1, "start"], temp_mo[1, "start"], 15)),
      to = as.Date(ISOdate(temp_yr[N, "end"], temp_mo[N, "end"], 15)),
      by = "month")
  }

  yrmo_dups_rel1 <- lapply(seq_len(N), function(k) {
    if (k > 1) {
      x <- c(tsl_yrmo[[1]], tsl_yrmo[[k]])
      sort(unique(x[duplicated(x)]))
    } else {
      NULL
    }
  })

  list(ts_yrmo = ts_yrmo, tsl_yrmo = tsl_yrmo, ts_expected = ts_expected,
    has_everymonth = identical(ts_expected, ts_yrmo[!duplicated(ts_yrmo)]),
    has_duplicated = anyDuplicated(ts_yrmo) > 0,
    yrmo_dups = yrmo_dups_rel1)
}


get_nctime_attributes <- function(filename) {
  nc <- ncdf4::nc_open(filename = filename, write = FALSE, verbose = FALSE)
  ncdf4::nc_close(nc)

  dim_time <- grep("(\\btime\\b)|(\\bt\\b)", names(nc$dim), value = TRUE, ignore.case = TRUE)
  stopifnot(length(dim_time) > 0)

  list(dim_time = dim_time, units = nc$dim[[dim_time]]$units,
    calendar = nc$dim[[dim_time]]$calendar)
}

get_ncvar_attributes <- function(filename) {
  nc <- ncdf4::nc_open(filename = filename, write = FALSE, verbose = FALSE)
  ncdf4::nc_close(nc)

  temp <- unlist(nc, recursive = TRUE)
  res <- temp[grep("(hasAddOffset)|(hasScaleFact)|(compression)|(units)", names(temp))]

  res[order(names(res))]
}

get_ncrecordvariable_name <- function(filename) {
  nc <- ncdf4::nc_open(filename = filename, write = FALSE, verbose = FALSE)
  ncdf4::nc_close(nc)

  names(nc$dim)[nc$unlimdimid]
}


#' read time values
get_nc_time_axis <- function(filename, dim_time = "time") {
  stopifnot(check_cltool("ncks", "4.6.6"))

  fvals <- tempfile()
  system2("ncks", args = paste("-H -v", dim_time, "--cal --cdl", filename,
    "| tr \",\" \"\\n\""), stdout = fvals)
  tvals <- readLines(fvals)
  unlink(fvals)

  str_time_equal <- paste(dim_time, "=")
  temp <- grep(str_time_equal, tvals)
  id_time_start <- temp[length(temp)]

  temp <- grep("[=;]", tvals)
  itemp <- temp > id_time_start
  id_time_end <- if (any(itemp)) {
      x <- temp[itemp][1]
      if (grepl("[[:digit:]]", tvals[x])) x else {x - 1}
    } else {
      temp <- grep("[;]", tvals)
      itemp <- temp >= id_time_start # for cases with one time value
      if (any(itemp)) temp[itemp][1] else length(tvals)
    }

  temp <- tvals[id_time_start:id_time_end]
  temp <- sub(str_time_equal, "", temp)
  temp <- trimws(gsub("[\";]", "", temp))
  temp <- temp[nchar(temp) > 1]
  strptime(temp, format = "%Y-%m-%d", tz = "UTC")
}

get_nc_time_axis <- memoise::memoise(get_nc_time_axis)

get_ncvalues_at_times <- function(filename, varname, at, dim_time = NULL, ztime = NULL) {
  nc <- ncdf4::nc_open(filename = filename, write = FALSE, verbose = FALSE)
  vals <- ncdf4::ncvar_get(nc, varname)
  ncdf4::nc_close(nc)

  if (is.null(ztime)) {
    ztime <- get_nc_time_axis(filename, dim_time)
  }

  N <- length(ztime)
  stopifnot(!anyNA(ztime), length(at) <= N)

  # find requested times 'at'
  at <- as.POSIXlt(at)
  at_yrmo <- format(at, format = "%Y%m")
  ztime_yrmo <- format(ztime, format = "%Y%m")
  ats <- which(ztime_yrmo %in% at_yrmo)

  if (anyDuplicated(ztime_yrmo[ats])) {
    res <- NULL

  } else {
    stopifnot(identical(length(ats), length(at)))

    # read variable values at times 'at'
    if (N > 1 || length(dim(vals)) > 2) {
      dimid_time <- which(dim(vals) == N)

      res <- if (dimid_time == 1) {
          vals[ats, , ]
        } else if (dimid_time == 2) {
          vals[, ats, ]
        } else if (dimid_time == 3) {
          vals[, , ats]
        } else if (dimid_time == 4) {
          vals[, , , ats]
        } else {
          stop("Time as dim = ", dimid_time, " is not implemented.")
        }

    } else {
      res <- vals
    }
  }

  res
}

check_nc_time_axis <- function(filename, dim_time = NULL, ztime = NULL) {
  if (is.null(ztime)) {
    ztime <- get_nc_time_axis(filename, dim_time)
  }

  dtime_days <- difftime(ztime[-1], ztime[-length(ztime)], units = "day")
  month_days <- 28:31

  c(
    step_is_1month = all(dtime_days %in% month_days),
    step_lt_1month = any(dtime_days < min(month_days)),
    step_gt_1month = any(dtime_days > max(month_days))
  )
}

time_agrees_with_ncfilename <- function(filename, ftime) {
  temp <- calc_ncfile_times(ftime)
  ts_yrmo <- as.POSIXlt(temp[["ts_yrmo"]])
  ctime <- rSFSW2::read_time_netCDF(filename)

  c(
    step_N = ctime[["N"]] == length(ts_yrmo),

    span_yrmo = ts_yrmo[1][["year"]] + 1900 == ctime[["start"]][["year"]] &&
      ts_yrmo[1][["mon"]] + 1 == ctime[["start"]][["month"]] &&
      ts_yrmo[length(ts_yrmo)][["year"]] + 1900 == ctime[["end"]][["year"]] &&
      ts_yrmo[length(ts_yrmo)][["mon"]] + 1 == ctime[["end"]][["month"]]
  )
}

#' Process downloaded netCDF files to concatenate if needed otherwise move to dedicated directory
#' @examples
#' \dontrun{
#' dir_prj <- file.path("/Volumes", "BookDuo_12TB", "BigData", "GIS", "Data",
#'   "Weather_Future", "ESGF")
#' dir_code <- file.path(dir_prj, "Scripts")
#' dir_data <- file.path(dir_prj, "Downloads")
#' dir_duplicates <- file.path(dir_prj, "Duplicates")
#' dir_concatables <- file.path(dir_prj, "Raw_to_concat")
#' dir_delete <- file.path(dir_prj, "to_delete")
#' dir_scrutinize <- file.path(dir_prj, "to_scrutinize")
#' dir_out <- file.path(dir_prj, "..", "ClimateScenarios", "CMIP5", "ESGF_Global")
#' prepare_climatedata_netCDF_files(dir_code, dir_data, dir_duplicates, dir_concatables,
#'   dir_delete, dir_scrutinize, dir_out, climDB_tag = "CMIP5_ESGF_Global")
#' }
#' @export
prepare_climatedata_netCDF_files <- function(dir_code, dir_data, dir_duplicates,
dir_concatables, dir_delete, dir_scrutinize, dir_out, climDB_tag = NULL,
  climDB_meta = NULL) {

  stopifnot(!(is.null(climDB_tag) && is.null(climDB_meta)))
  if (is.null(climDB_meta)) {
    climDB_meta <- rSFSW2::climscen_metadata()[[climDB_tag]]
  }

  # Shell-command output file
  f_toconcat <- file.path(dir_code, paste0(format(Sys.time(), format = "%Y%m%d%H%M"),
    "_concat_along_time_esgf.sh"))

  if (file.exists(f_toconcat)) {
    f_toconcat <- file(f_toconcat, open = "a+")

  } else {
    writeLines(c("#!/bin/bash", ""), con = f_toconcat)
    f_toconcat <- file(f_toconcat, open = "a+")
    cat(paste("cd", dir_code), sep = "\n", file = f_toconcat, append = TRUE)
  }

  # Locate netCDF files to process
  ftemps <- list.files(dir_data, pattern = ".nc")

  f_parts <- strsplit(ftemps, split = "_", fixed = TRUE)
  f_vars <- sapply(f_parts, function(x) x[climDB_meta[["str_fname"]][["id_var"]]])
  f_scens <- sapply(f_parts, function(x) x[climDB_meta[["str_fname"]][["id_scen"]]])
  f_times <- sapply(f_parts, function(x) x[climDB_meta[["str_fname"]][["id_time"]]])
  f_body <- sapply(f_parts, function(x) {
      paste(x[-climDB_meta[["str_fname"]][["id_time"]]], collapse = "_")
    })

  funi_body <- unique(f_body)

  for (fid in seq_along(funi_body)) {
    f <- funi_body[fid]
    i_alltimes <- grep(f, ftemps)
    print(paste(Sys.time(), "work on", shQuote(f), "including", length(i_alltimes),
      "netCDF file(s)."))

    # Sort historically and by length
    temp_ts <- calc_ncfile_times(f_times[i_alltimes])
    i_alltimes <- i_alltimes[order(sapply(temp_ts[["tsl_yrmo"]],
      function(x) x[[1]]), -lengths(temp_ts[["tsl_yrmo"]]))]

    # Files
    f_alltimes <- ftemps[i_alltimes]
    ffrom <- file.path(dir_data, f_alltimes)

    # Obtain time attributes
    f_attr_time <- lapply(ffrom, get_nctime_attributes)

    # Check that filenames agree with content of time dimension
    goodtimes <- sapply(seq_along(i_alltimes), function(k) {
        filename <- file.path(dir_data, ftemps[i_alltimes[k]])
        c(check_nc_time_axis(filename, dim_time = f_attr_time[[k]][["dim_time"]]),
          time_agrees_with_ncfilename(filename, ftime = f_times[i_alltimes[k]]))
      })

    check_vars <- c("step_is_1month", "step_N", "span_yrmo")
    badtimes <- !apply(goodtimes[check_vars, , drop = FALSE], 2, all)

    if (any(badtimes)) {
      i_badtimes <- i_alltimes[badtimes]

      for (k in seq_along(i_badtimes)) {
        x <- goodtimes[, badtimes, drop = FALSE][, k]

        temp <- x
        temp[check_vars] <- !temp[check_vars]
        dir_scrutinize_to <- file.path(dir_scrutinize, paste0("check_",
          paste(gsub("_", "-", names(temp))[temp], sep = "_", collapse = "_")))
        dir_safe_create(dir_scrutinize_to)

        temp <- try(file.rename(from = file.path(dir_data, ftemps[i_badtimes[k]]),
          to = file.path(dir_scrutinize_to, ftemps[i_badtimes[k]])))

        print(paste("Moving", shQuote(basename(ftemps[i_badtimes[k]])), "from",
          shQuote(dir_data), "to", shQuote(dir_scrutinize_to),
          if (inherits(temp, "try-error") || isTRUE(identical(temp, FALSE))) {
            "failed."
          } else {
            "succeeded."
          }))
      }

      has_goodtimes <- !badtimes
      i_alltimes <- i_alltimes[has_goodtimes]
      f_alltimes <- f_alltimes[has_goodtimes]
      ffrom <- ffrom[has_goodtimes]
      f_attr_time <- f_attr_time[has_goodtimes]
    }

    if (length(i_alltimes) > 0) {
      fto0 <- file.path(dir_out, f_scens[i_alltimes], f_alltimes)
      fto <- rep(NA, length(i_alltimes))

      if (length(f_alltimes) > 1) {
        #--- concatenate
        stopifnot(length(unique(f_scens[i_alltimes])) == 1L)

        # Check that year-month time series is complete
        f_all_ts <- calc_ncfile_times(f_times[i_alltimes])

        if (f_all_ts[["has_everymonth"]]) {
          # Complete data for time series ==> we can concatenate
          # http://nco.sourceforge.net/nco.html#ncrcat
          #   - "ncrcat concatenates record variables across an arbitrary number of
          #     input-files"
          #   - "The record coordinate, if any, should be monotonic (or else non-fatal
          #     warnings may be generated)."
          #   - "Hyperslabs along the record dimension that span more than one file are
          #     handled correctly."
          #   - "ncrcat does not unpack data, it simply copies the data from the input-files,
          #     and the metadata from the first input-file, to the output-file. This means
          #     that data compressed with a packing convention must use the identical packing
          #     parameters (e.g., scale_factor and add_offset) for a given variable across
          #     all input files."
          ts_posix <- as.POSIXlt(f_all_ts[["ts_yrmo"]])
          ts_years <- unique(ts_posix$year + 1900)
          ts_months <- ts_posix$mon + 1

          fout_concat <- file.path("..", "Downloads", paste0(f, "_",
            formatC(ts_years[1], width = 4, flag = 0),
            formatC(ts_months[1], width = 2, flag = 0), "-",
            formatC(ts_years[length(ts_years)], width = 4, flag = 0),
            formatC(ts_months[length(ts_months)], width = 2, flag = 0),
            ".nc"))

          # Obtain variable attributes (packing, compression, variable units)
          f_attr_var <- lapply(ffrom, get_ncvar_attributes)

          # Check that all data are packed/compress identically
          comp_vars <- is.na(sapply(f_attr_var, function(p) do_compare(f_attr_var[[1]], p)))

          # Check that calendar and time units are identically
          temp <- is.na(sapply(f_attr_time, function(p) do_compare(f_attr_time[[1]], p)))
          comp_time1 <- apply(temp, 2, all)

          # Check that record variable is 'time'
          f_recvar <- lapply(ffrom, get_ncrecordvariable_name)
          has_recvar_time <- sapply(seq_along(ffrom), function(k)
            identical(f_recvar[[k]], f_attr_time[[k]][["dim_time"]]))

          # Check that time units are unique
          comp_time2 <- unname(sapply(seq_along(ffrom), function(k) {
              temp <- get_nc_time_axis(ffrom[k], dim_time = f_attr_times[[k]][["dim_time"]])
              !anyNA(temp) && identical(anyDuplicated(format(temp, format = "%Y%m")), 0L)
            }))

          # Index of files suitable for concatenation
          is_alltimes_suitable <- comp_vars & comp_time1 & comp_time2 & has_recvar_time
          i_suitable <- i_alltimes[is_alltimes_suitable]
          i_unsuitable <- i_alltimes[!is_alltimes_suitable]
          f_unsuittimes <- ftemps[i_unsuitable]

          # Check that year-month time series is complete
          f_suitable_ts <- calc_ncfile_times(f_times[i_suitable], f_all_ts[["ts_expected"]])

          if (f_suitable_ts[["has_everymonth"]]) {
            f_suittimes <- ftemps[i_suitable]
            f_attr_suittimes <- f_attr_time[is_alltimes_suitable]
            ffrom_suit <- file.path(dir_data, f_suittimes)
            fin_concat <- file.path("..", basename(dir_concatables), f_suittimes)

            if (f_suitable_ts[["has_duplicated"]] && length(i_suitable) > 1) {
              #--- Deal with duplicated data between files
              f_suit_dupvals <- list()
              ztime1 <- get_nc_time_axis(ffrom_suit[1],
                dim_time = f_attr_suittimes[[1]][["dim_time"]])

              for (k in seq_along(i_suitable)[-1]) {
                f_suit_dupvals[[k - 1]] <- list(
                  ref1 = get_ncvalues_at_times(filename = ffrom_suit[1],
                    varname = f_vars[i_suitable[1]], ztime = ztime1,
                    at = f_suitable_ts[["yrmo_dups"]][[k]]),
                  x = get_ncvalues_at_times(filename = ffrom_suit[k],
                    varname = f_vars[i_suitable[k]],
                    dim_time = f_attr_suittimes[[1]][["dim_time"]],
                    at = f_suitable_ts[["yrmo_dups"]][[k]]))
              }

              has_identical_dups <- c(TRUE, # first (reference) file is by definition identical to itself
                sapply(f_suit_dupvals, function(x) {
                  temp <- do_compare(x[["ref1"]], x[["x"]])
                  !is.null(x[["ref1"]]) && !is.list(temp) && is.na(temp)
                }))

              if (all(has_identical_dups)) {
                # Duplicated data are equal among files:
                #   1) cut into non-overlapping time periods
                #   2) concatenate unique time periods (relative to dir_code)

                # sort historically and by length
                k_suitordered <- order(sapply(f_suitable_ts[["tsl_yrmo"]],
                  function(x) x[[1]]), -lengths(f_suitable_ts[["tsl_yrmo"]]))

                fuse_yrmo <- rep(NA, length(f_all_ts[["ts_expected"]]))
                for (k in k_suitordered) {
                  ids_need_source <- which(is.na(fuse_yrmo))
                  if (any(ids_need_source)) {
                    times_need_source <- f_all_ts[["ts_expected"]][ids_need_source]
                    from_filek <- times_need_source %in% f_suitable_ts[["tsl_yrmo"]][[k]]
                    fuse_yrmo[ids_need_source[which(from_filek)]] <- ftemps[i_suitable[k]]
                  } else {
                    break
                  }
                }

                fuse_yrmo_ids <- lapply(seq_along(i_suitable), function(k) {
                  temp <- f_all_ts[["ts_expected"]][ftemps[i_suitable[k]] == fuse_yrmo]
                  ids <- which(f_suitable_ts[["tsl_yrmo"]][[k]] %in% temp)
                  str_yrmo <- f_suitable_ts[["tsl_yrmo"]][[k]][ids]

                  list(ids = ids,
                    start_yrmo = format(str_yrmo[1], format = "%Y%m"),
                    end_yrmo = format(str_yrmo[length(str_yrmo)], format = "%Y%m"))
                })

                fuse_yrmo_used <- sapply(seq_along(i_suitable), function(k)
                  length(fuse_yrmo_ids[[k]][["ids"]]) > 0)

                cat("\n", file = f_toconcat, append = TRUE)
                cat(paste("echo", f), sep = "\n", file = f_toconcat, append = TRUE)

                temp_fouts <- rep(NA, length(i_suitable))

                for (k in seq_along(i_suitable)) {
                  temp3 <- file.path("..", basename(dir_concatables), f_suittimes[k])

                  if (fuse_yrmo_used[k]) {
                    # cut suitable file to usable time period
                    temp1 <- fuse_yrmo_ids[[k]][["ids"]]
                    temp_fouts[k] <- file.path(dir_concatables, paste0(f, "_",
                      fuse_yrmo_ids[[k]][["start_yrmo"]], "-", fuse_yrmo_ids[[k]][["end_yrmo"]],
                      ".nc"))

                    if (!identical(basename(temp_fouts[k]), basename(temp3))) {
                      cmd_ncrcat <- paste0("ncrcat",
                        " -F -d time,", temp1[1], ",", temp1[length(temp1)], # specify time series (in base-1 indexing)
                        " -o", temp_fouts[k], # output file name
                        " ", temp3) # input file
                      cat(cmd_ncrcat, sep = "\n", file = f_toconcat, append = TRUE)
                      del_fins <- TRUE
                    } else {
                      # use all of this file
                      del_fins <- FALSE # don't delete because uncut file is input to concatenation
                    }
                  } else {
                     del_fins <- TRUE
                  }

                  # move suitable, whether cut or unused file to delete folder since no longer needed
                  if (del_fins)  {
                    cmd_move <- paste("mv", temp3, file.path(dir_delete, f_suittimes[k]))
                    cat(cmd_move, sep = "\n", file = f_toconcat, append = TRUE)
                  }
                }

                # file destination
                fto[is_alltimes_suitable] <- file.path(dir_concatables, f_suittimes)

                # concatenate cut output files
                temp_fouts <- file.path("..", basename(dir_concatables),
                  basename(na.exclude(temp_fouts)))

                if (!identical(basename(temp_fouts), basename(fout_concat))) {
                  cmd_ncrcat <- paste0("ncrcat",
                    " -F -d time,1,", length(f_all_ts[["ts_expected"]]), # specify time series (in base-1 indexing)
                    " -o ", fout_concat, # output file name
                    " ", paste(temp_fouts, collapse = " ")) # input files
                  cat(cmd_ncrcat, sep = "\n", file = f_toconcat, append = TRUE)

                  for (k in seq_along(temp_fouts)) {
                    cmd_move <- paste("mv", temp_fouts[k],
                      file.path(dir_delete, basename(temp_fouts[k])))
                    cat(cmd_move, sep = "\n", file = f_toconcat, append = TRUE)
                  }

                } else {
                  # one among several suitable files contains all the data: move to destination
                  itemp <- which(f_suittimes == basename(fout_concat))
                  fto[is_alltimes_suitable][itemp] <- fto0[is_alltimes_suitable][itemp]
                }

                fto[!is_alltimes_suitable] <- file.path(dir_delete, f_unsuittimes)

              } else {
                print(paste("Data for time-series suggested by filenames is complete, but",
                  "duplicated data between files is not equal:", paste(shQuote(f_suittimes),
                  collapse = ", ")))

                dir_scrutinize_to <- file.path(dir_scrutinize, "duplicated-data-with-diffs")
                dir_safe_create(dir_scrutinize_to)
                fto <- file.path(dir_scrutinize_to, f_alltimes)
              }

            } else {
              # No duplicated data: prepare simple concatenation command (relative to dir_code)
              cat("\n", file = f_toconcat, append = TRUE)
              cat(paste("echo", f), sep = "\n", file = f_toconcat, append = TRUE)

              cmd_ncrcat <- paste0("ncrcat",
                " -F -d time,1,", length(f_all_ts[["ts_expected"]]), # specify time series (in base-1 indexing)
                " -o ", fout_concat, # output file name
                " ", paste(fin_concat, collapse = " ")) # input files
              cat(cmd_ncrcat, sep = "\n", file = f_toconcat, append = TRUE)

              for (k in seq_along(f_suittimes)) {
                cmd_move <- paste("mv", file.path(dir_concatables, f_suittimes[k]),
                  file.path(dir_delete, f_suittimes[k]))
                cat(cmd_move, sep = "\n", file = f_toconcat, append = TRUE)
              }

              # file destination
              fto[is_alltimes_suitable] <- file.path(dir_concatables, f_suittimes)
              fto[!is_alltimes_suitable] <- file.path(dir_delete, f_unsuittimes)
            }

          } else {
            print(paste("Data for time-series suggested by filenames is complete, but some",
              "not-required files are incompatible:", paste(shQuote(f_unsuittimes),
              collapse = ", ")))

            # file destination
            fto[!is_alltimes_suitable] <- file.path(dir_delete, f_unsuittimes)
          }

        } else {
          print("Data for time-series suggested by filenames is not complete.")

          dir_scrutinize_to <- file.path(dir_scrutinize, "incomplete-timeseries")
          dir_safe_create(dir_scrutinize_to)
          fto <- file.path(dir_scrutinize, f_alltimes)
        }

      } else {
        #--- check for already available duplicate file
        if (file.exists(fto0)) {
          comp <- do_compare_nc(fnc1 = ffrom, fnc2 = fto0, var = f_vars[i_alltimes])

          if (length(comp[["layout"]]) > 0 || !is.na(comp[["var"]])) {
            print(paste(shQuote(f_alltimes), "has non-identical duplicate file:"))
            print(comp)
            # two copies are not identical
            if (is.na(comp[["var"]]) &&
              identical(names(comp[["layout"]]), c("natts.eq", "natts.x1", "natts.x2"))) {
              # two copies only differ in number of attributes; keep the copy with more
              if (comp[["layout"]][["natts.x1"]] > comp[["layout"]][["natts.x2"]]) {
                 fto <- fto0
              } else {
                fto <- file.path(dir_delete, f_alltimes)
              }
            } else {
              fto <- file.path(dir_duplicates, f_alltimes)
            }
          } else {
            # two copies are identical; delete this one
            fto <- file.path(dir_delete, f_alltimes)
          }

        } else {
          fto <- fto0
        }
      }

      #--- move to dedicated location
      for (k in seq_along(i_alltimes)) {
        if (!is.na(fto[k]) && identical(basename(ffrom[k]), basename(fto[k]))) {
          temp <- try(file.rename(from = ffrom[k], to = fto[k]))
          print(paste("Moving", shQuote(basename(ffrom[k])), "from", shQuote(dir_data), "to",
            shQuote(dirname(fto[k])),
            if (inherits(temp, "try-error")) "failed." else "succeeded."))
        }
      }
    }
  }

  close(f_toconcat)

  invisible(length(ftemps))
}

