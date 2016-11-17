#------ Remove when this becomes a R package
# - compiler::cmpfun
# - move constants to package environment

slot <- methods::slot

#------ Constants
output_timescales_maxNo <- 4L
SoilLayer_MaxNo <- 20L
lmax <- seq_len(SoilLayer_MaxNo)
SoilWat.windspeedAtHeightAboveGround <- 2	#m
st_mo <- seq_len(12L)
tol <- sqrt(.Machine$double.eps)
toln <- sqrt(.Machine$double.neg.eps)

#------ Funtions
swsf_read_csv <- compiler::cmpfun(function(file, stringsAsFactors = FALSE, ...) {
  dots <- list(...)
  dots[["file"]] <- file
  dots[["stringsAsFactors"]] <- stringsAsFactors
  use_iotools <- requireNamespace("iotools", quietly = TRUE)
  res <- NULL

  if (use_iotools) {
    # faster than utils::read.csv
    dots2 <- dots[names(dots) %in% names(formals(iotools::read.csv.raw))]
    if (!any(names(dots2) == "nrowsClasses"))
      dots2[["nrowsClasses"]] <- 1000L

    temp <- try(do.call(iotools::read.csv.raw, args = dots2), silent = TRUE)
    if (inherits(temp, "try-error")) {
      use_iotools <- FALSE
    } else {
      names(temp) <- gsub("\"", "", names(temp))
      res <- temp
    }
  }

  if (!use_iotools) {
    dots2 <- dots[names(dots) %in% names(formals(utils::read.csv))]
    res <- try(do.call(utils::read.csv, args = dots2), silent = TRUE)
  }

  res
})

swsf_read_inputfile <- compiler::cmpfun(function(file, header_rows = 1, ...) {
  sw_use <- tryCatch(swsf_read_csv(file, nrows = header_rows),
    error = function(e) print(paste("Failed to read file:", shQuote(basename(file)), "with", e)))
  sw <- swsf_read_csv(file, skip = header_rows, ...)
  names(sw) <- names(sw_use)
  sw_use <- c(FALSE, as.logical(as.numeric(sw_use[, -1])))
  sw_use[is.na(sw_use)] <- FALSE
  names(sw_use) <- names(sw)

  list(use = sw_use, data = sw)
})

reconstitute_inputfile <- compiler::cmpfun(function(sw_use, data) {
  temp <- as.data.frame(matrix(as.integer(sw_use), nrow = 1L))
  colnames(temp) <- names(sw_use)
  temp[1, 1] <- "UseInformationToCreateSoilWatRuns"
  rbind(temp, data)
})


getStartYear <- compiler::cmpfun(function(simstartyr) simstartyr + 1)

set_PRAGMAs <- compiler::cmpfun(function(con, settings) {
  temp <- lapply(settings, function(x) RSQLite::dbGetQuery(con, x))
  invisible(0)
})

getSiteIds <- compiler::cmpfun(function(con, folderNames) {
  wf_ids <- RSQLite::dbGetQuery(con, "SELECT id, folder FROM weatherfolders")
  wf_ids[match(folderNames, wf_ids[, "folder"], nomatch = NA), "id"]
})

has_nodata <- compiler::cmpfun(function(data, tag = NULL, MARGIN = 1) {
  if (is.null(tag)) {
    apply(data, MARGIN, function(x) all(is.na(x)))
  } else {
    apply(data[, grepl(tag, colnames(data)), drop = FALSE], MARGIN, function(x) all(is.na(x)))
  }
})

has_incompletedata <- compiler::cmpfun(function(data, tag = NULL, MARGIN = 1) {
  if (is.null(tag)) {
    apply(data, MARGIN, anyNA)
  } else {
    apply(data[, grepl(tag, colnames(data)), drop = FALSE], MARGIN, anyNA)
  }
})

#custom list.dirs function because the ones in 2.13 and 2.15 are different... this function will behave like the one in 2.15 no matter which version you are using...
#note: should work on any system where the directory seperator is .Platform$file.sep (ie Unix)
list.dirs2 <- compiler::cmpfun(function(path, full.names=TRUE, recursive=TRUE) {
  dir.list <- list.dirs(path, full.names)

  if(is.null(dir.list))
    return (dir.list)
  if(length(dir.list) == 0)
    return (dir.list)
  if(recursive == TRUE)
    return (dir.list)

  nSlash = length(strsplit(dir.list[1], .Platform$file.sep)[[1]]) + 1
  if(nSlash == 1)
    return(dir.list[-1])

  n = length(dir.list)
  for(i in n:1)
    if(length(strsplit(dir.list[i], .Platform$file.sep)[[1]]) != nSlash)
      dir.list <- dir.list[-i]

  return (dir.list)
})
#custom file.copy2 function, b/c it was giving errors on JANUS when run with MPI
file.copy2 <- compiler::cmpfun(function(from="", to="", overwrite=TRUE, copy.mode=TRUE, times=0) {
  file.copy(from, to, overwrite, FALSE, copy.mode)
  if(times < 24)
    if(file.exists(from))
      if(!file.exists(to)) {
        print("trying to copy the file again")
        Recall(from, to, overwrite, copy.mode, (times+1))	#recursively call the function again because when run with MPI the file copying doesn't seem to work everytime...
      }
  #else { #this commented out part copies the file via the system command cp
  #	if(any(grepl("/", to, fixed=TRUE))) { #this part makes the to directory if it doesn't exist... so pretty much this can copy files to places that don't exist, which generally isn't what you want to do but in this case it might help solve an error I keep getting.
  #		y <- to
  #		while(substr(y, nchar(y), nchar(y)) != '/')
  #			y <- substr(y, 1, nchar(y)-1)
  #		y <- substr(y, 1, nchar(y)-1)
  #		if(y != "")
  #			system(paste("mkdir -p", y), ignore.stdout=FALSE, ignore.stderr=FALSE)
  #	}
  #	command <- "cp" #this just calls the system command cp...
  #	if(overwrite == TRUE) command <- paste(command, "-f")
  #	if(copy.mode == TRUE) command <- paste(command, "-p")
  #	system(paste(command, from, to), ignore.stdout=FALSE, ignore.stderr=FALSE)
  #}
})
#made this function b/c dir.create wasn't always working correctly on JANUS for some reason... so if the simulations are being run on JANUS then it uses the system mkdir call to make the directories.
dir.create2 <- compiler::cmpfun(function(path, showWarnings = TRUE, recursive = FALSE, mode = "0777", times = 0) {
  dir.create(path, showWarnings, recursive, mode)
  if(times < 24)
    if(!dir.exists(path)) {
      print("trying to make directory again")
      Recall(path, showWarnings, TRUE, mode, (times+1)) #recursively call the function b/c when run on JANUS with MPI it doesn't seem to make the directories everytime... quite aggravating.
    }
  #else if(recursive == TRUE) #this commented out part makes the directory via the system call mkdir
  #	system(paste("mkdir -p", path), ignore.stdout=TRUE, ignore.stderr=FALSE)
  #else
  #	system(paste("mkdir", path), ignore.stdout=TRUE, ignore.stderr=FALSE)
})
#copy directory and content as in system(paste("cp -R", shQuote(from), shQuote(to)))
dir.copy <- compiler::cmpfun(function(dir.from, dir.to, overwrite=FALSE){
  dir.create2(dir.to, recursive=TRUE)
  dir.list <- basename(list.dirs2(dir.from, full.names=FALSE, recursive=FALSE))
  file.list <- list.files(dir.from)
  if(length(dir.list) > 0) {
    sapply(dir.list, function(x) {Recall(dir.from=file.path(dir.from, x), dir.to=file.path(dir.to, x), overwrite=overwrite)})
    #file.list <- file.list[-match(dir.list, table=file.list)] #this line gives an error when run in R v. 2.13
    file.list <- file.list[file.list != dir.list] #this line does the same as the other line, but does not throw the error
  }
  if(length(file.list) > 0) {
    sapply(file.list, function(x) {file.copy2(from=file.path(dir.from, x), to=file.path(dir.to, x), overwrite=overwrite, copy.mode=TRUE)})
  }
  invisible(1)
})
#remove directory and content
dir.remove <- compiler::cmpfun(function(dir){
  file.list <- try(list.files(dir, all.files=TRUE))
  file.list <- file.list[-which(file.list %in% c(".", ".."))]
  dir.list <- basename(list.dirs2(dir, full.names=FALSE, recursive=FALSE))
  if(length(dir.list) > 0) {
    sapply(dir.list, function(x) Recall(dir=file.path(dir, x)))
    file.list <- file.list[-match(dir.list, table=file.list)]
  }
  if(length(file.list) > 0) {
    sapply(file.list, function(x) {file.remove(file.path(dir, x))})
  }
  file.remove(dir)
})

isLeapYear <- compiler::cmpfun(function(y) {
  #from package: tis
  y %% 4 == 0 & (y %% 100 != 0 | y %% 400 == 0)
})

#' Iterator functions
#'
#' @param isim An integer value. A value of \code{runIDs_todo} as subset of \code{runIDs_total}, i.e., a consecutive index across loops 1+2b
#' @param sc An integer value. The iterator position along \code{scenario_No}.
#' @param scN An integer value. The number of (climate) scenarios used in the project, i.e., \eqn{scN == scenario_No}.
#' @param runN An integer value. The number of runs/sites set up in the master input file, i.e., \eqn{runN == runsN_master}.
#' @param runIDs An integer vector. The identification IDs of rows in the master file that are included, i.e., \eqn{runIDs == runIDs_sites}.
#'
#' @section NOTE:
#'  Do not change the iterators without adjusting the design of the output databases!
#'
#' @section Simulation runs:
#'  * Simulations are run over three nested loops
#'    - loop1 (1...expN) nested in loop2b (1...runsN_sites) nested in loop3 (1...scenario_No)
#'        - Note: loop3 (along scenarios) occurs within the function 'do_OneSite'
#'        - Note: loop2b is a subset of loop2a (1...runsN_master)
#'    - column 'include_YN' reduces 'site_id' to 'runIDs_sites'
#'    - 'site_id' and 'P_id' are invariant to 'include_YN'
#'
#'  * Master input file: column 'include_YN' selects rows which are included in the simulation
#'    - Note: rows of the master input file correspond to rows of the treatment input file
#'    - column 'site_id' == consecutive identification numbers of all rows in the master file; this is treated as a unique (and stable) identifier of a site
#'    - runsN_master == number of rows in the master file
#'    - runsN_sites == number of rows in the master file that are included (runsN_sites <= max(site_id))
#'    - runIDs_sites == identification of rows in the master file that are included
#'
#'  * Experimental input file: each row defines a condition which is applied to every runIDs_sites
#'    - expN == number of experimental treatments
#'
#'  * The function 'do_OneSite' will be called n-times with n = runsN_total
#'    - runsN_total == (number of included sites) x (number of experimental treatments)
#'    - runIDs_total == consecutive identification numbers along runsN_total
#'    - runIDs_done == values of runIDs_total that have already been processed by 'do_OneSite'
#'    - runIDs_todo == values of runIDs_total that await simulation by 'do_OneSite'
#'    - runsN_todo == number of runIDs_total that await simulation by 'do_OneSite'
#'
#'  * The function 'do_OneSite' could be called n-times with n = runsN_incl if all 'include_YN' were on
#'    - runsN_incl == (number of sites) x (number of experimental treatments)
#'
#'  * The variable 'climate.conditions' defines climate conditions that are applied to each 'runIDs_total'
#'    - scenario_No == number of climate conditions
#'
#'  * A grand total of n = runsN_Pid SoilWat runs could be carried out (n == number of rows in the output database)
#'    - runsN_Pid == max(P_id) == runsN_incl x scenario_No
#'    - P_id == a consecutive identification number for each possible SoilWat simulation; used as the ID for the output database
#'
#' @name iterators
NULL
#' @rdname iterators
#' @return An integer value of the iterator position in loop 1 based on the position across loops 1+2b
it_exp <- compiler::cmpfun(function(isim, runN) (isim - 1L) %/% runN + 1L)
#' @rdname iterators
#' @return An integer value of the iterator position across 'runIDs_sites', i.e., the position in loop 2 based on position across loops 1+2b
it_site <- compiler::cmpfun(function(isim, runN, runIDs) runIDs[(isim - 1L) %% runN + 1L])
#' @rdname iterators
#' @return An integer value of the iterator position across all loops 1+2a+3, dependent on \code{include_YN}.
it_Pid_old <- compiler::cmpfun(function(isim, sc, scN) (isim - 1L) * scN + sc)
#' @rdname iterators
#' @return An integer value of the iterator position across all loops 1+2a+3, invariant
#'  to \code{include_YN}. A consecutive identification number for each possible SoilWat
#'  simulation--used as the ID for the output database.
it_Pid <- compiler::cmpfun(function(isim, sc, scN, runN, runIDs) {
  ((it_exp(isim, runN) - 1L) * runN + it_site(isim, runN, runIDs) - 1L) * scN + sc
})


## Tests
#include_YN <- c(0, 0, 1, 0, 0, 1, 1, 0)
#include_YN <- rep(1, 8)
#t(sapply(runIDs_todo, function(isim) c(isim, it_site(isim, 8), it_exp(isim, 8), it_Pid(isim, 1, 3, 8), it_Pid_old(isim, 1))))
#t(sapply(runIDs_total, function(isim) c(isim, it_site(isim, 8), it_exp(isim, 8), it_Pid(isim, 1, 3, 8), it_Pid_old(isim, 1))))


#' Export R objects to MPI slaves or SNOW workers
#'
#' @param varlist A vector of R object names to export
#' @param list_envs A list of environments in which to search for the R objects
#' @param parallel_backend A character vector, either 'mpi' or 'snow'
#' @param cl A snow cluster object
#'
#' @return A logical value. \code{TRUE} if every object was exported successfully.
gather_objects_for_export <- compiler::cmpfun(function(varlist, list_envs) {
  #---Determine environments
  obj_env <- new.env(parent = emptyenv())
  vtemp <- NULL

  for (k in seq_along(list_envs)) {
    temp <- varlist[varlist %in% ls(pos = list_envs[[k]])]
    temp <- temp[!(temp %in% vtemp)]

    for (i in seq_along(temp))
      assign(temp[i], value = get(temp[i], list_envs[[k]]), obj_env)

    vtemp <- c(vtemp, temp)
  }

  cannot_export <- !(varlist %in% vtemp)
  if (any(cannot_export))
    print(paste("Objects in 'varlist' that cannot be located:",
          paste(varlist[cannot_export], collapse = ", ")))

  obj_env
})


do_import_objects <- compiler::cmpfun(function(obj_env) {
  temp <- list2env(as.list(obj_env), envir = .GlobalEnv)

  NULL
})


export_objects_to_workers <- compiler::cmpfun(function(obj_env, parallel_backend = c("mpi", "snow"), cl = NULL) {
  t.bcast <- Sys.time()
  parallel_backend <- match.arg(parallel_backend)
  N <- length(ls(obj_env))
  print(paste("Exporting", N, "objects from master process to workers"))

  success <- FALSE
  done_N <- 0

  if (inherits(cl, "cluster") && identical(parallel_backend, "snow")) {
    temp <- try(snow::clusterExport(cl, as.list(ls(obj_env)), envir = obj_env))

    success <- !inherits(temp, "try-error")

    if (success) {
      done_N <- min(unlist(snow::clusterCall(cl,
        function() length(ls(.GlobalEnv)))), na.rm = TRUE)
    }

  } else if (identical(parallel_backend, "mpi")) {
    temp <- try(Rmpi::mpi.bcast.cmd(assign,
      x = "do_import_objects", value = do_import_objects))
    if (!inherits(temp, "try-error"))
      temp <- try(Rmpi::mpi.bcast.cmd(do_import_objects, obj_env = obj_env))

    success <- !inherits(temp, "try-error")
    if (success) {
      done_N <- min(lengths(Rmpi::mpi.remote.exec(cmd = ls,
        envir = .GlobalEnv, simplify = FALSE)))
    }
  }

  if (success && done_N >= N) {
    print(paste("Export of", done_N, "objects took",
              round(difftime(Sys.time(), t.bcast, units = "secs"), 2),
              "secs"))
  } else {
    success <- FALSE
    print(paste("Export not successful:", done_N, "instead of", N, "objects exported"))
  }

  success
})





#' Rmpi work function for calling \code{do_OneSite}
#'
#' @references
#'   based on the example file \href{http://acmmac.acadiau.ca/tl_files/sites/acmmac/resources/examples/task_pull.R.txt}{'task_pull.R' by ACMMaC}
#' @section Note:
#'  In case an error occurs, the slave will like not report back to master because
#'  it hangs in miscommunication, and reminds idle (check activity, e.g., with \code{top}).
mpi_work <- compiler::cmpfun(function() {
  # Note the use of the tag for sent messages:
  #     1=ready_for_task, 2=done_task, 3=exiting
  # Note the use of the tag for received messages:
  #     1=task, 2=done_tasks

  junk <- 0L
  done <- 0L
  while (done != 1L) {
    # Signal being ready to receive a new task
    Rmpi::mpi.send.Robj(junk, 0, 1)

    # Receive a task
    dat <- Rmpi::mpi.recv.Robj(Rmpi::mpi.any.source(), Rmpi::mpi.any.tag())
    task_info <- Rmpi::mpi.get.sourcetag()
    tag <- task_info[2]

    if (tag == 1L) {
      if (dat$do_OneSite) {
        #print(paste("MPI slave", Rmpi::mpi.comm.rank(), "works on:", dat$i_sim, dat$labels))
        result <- match.fun("do_OneSite")(i_sim = dat$i_sim,
          i_labels = dat$labels,
          i_SWRunInformation = dat$SWRunInformation,
          i_sw_input_soillayers = dat$sw_input_soillayers,
          i_sw_input_treatments = dat$sw_input_treatments,
          i_sw_input_cloud = dat$sw_input_cloud,
          i_sw_input_prod = dat$sw_input_prod,
          i_sw_input_site = dat$sw_input_site,
          i_sw_input_soils = dat$sw_input_soils,
          i_sw_input_weather = dat$sw_input_weather,
          i_sw_input_climscen = dat$sw_input_climscen,
          i_sw_input_climscen_values = dat$sw_input_climscen_values)

        # Send a results message back to the master
        Rmpi::mpi.send.Robj(list(i = dat$i_sim, r = result), 0, 2)
      }

    } else if (tag == 2L) {
      done <- 1L
    }
    # We'll just ignore any unknown messages
  }
  Rmpi::mpi.send.Robj(junk, 0, 3)
})


load_NCEPCFSR_shlib <- compiler::cmpfun(function(cfsr_so){
  if(!is.loaded("writeMonthlyClimate_R")) dyn.load(cfsr_so) # load because .so is available
  invisible(0)
})

prepare_NCEPCFSR_extraction <- compiler::cmpfun(function(dir.big, dir.cfsr.data, dir.cfsr.code = dir.cfsr.data) {
  dir.create(dir.in.cfsr <- file.path(dir.big, "ncepcfsr"), showWarnings=FALSE)
  fname_cfsr <- file.path(dir.in.cfsr, "cfsr_convert.so")

  .local <- function(){
    #Check for the shared object 'cfsr_convert.so' that contains the C functions accessible to R
    if(!file.exists(fname_cfsr)){ # compile
      dtemp <- getwd()
      setwd(dir.cfsr.code)
      stopifnot(file.exists("cfsr_convert.c", "generic2.c", "generic2.h", "filefuncs2.c", "filefuncs2.h", "mymemory2.c", "mymemory2.h"))
      unlink(c("cfsr_convert.o", "generic2.o", "filefuncs2.o", "mymemory2.o"))
      stopifnot(system2(command=file.path(Sys.getenv()[["R_HOME"]], "R"), args=paste("CMD SHLIB -o", fname_cfsr, "cfsr_convert.c generic2.c filefuncs2.c mymemory2.c"), wait=TRUE) == 0)
      setwd(dtemp)
    }
    load_NCEPCFSR_shlib(fname_cfsr)

    #Check for wgrib2 (http://www.cpc.ncep.noaa.gov/products/wesley/wgrib2/)
    if(!file.exists(wgrib2 <- file.path(dir.in.cfsr, "wgrib2"))){
      temp2 <- if(nchar(temp <- Sys.which("wgrib2")) > 0) temp else if(file.exists(temp <- "/opt/local/bin/wgrib2")) temp else ""
      stopifnot(nchar(temp2) > 0)
      file.copy(from=temp2, to=wgrib2)
    }

    #Soft link to gribbed data
    fname_gribDir <- "griblargeC2"
    dir.grib <- file.path(dir.in.cfsr, fname_gribDir)
    if(!file.exists(dir.grib)){ # value of gribDir defined in cfsr_convert.c
      stopifnot(system2(command="ln", args=paste("-s", file.path(dir.cfsr.data, fname_gribDir), dir.grib)) == 0)
    }

    #Set up temporary directory for C code to store objects
    if(file.exists(ftemp <- file.path(dir.in.cfsr, "temporary_dy"))) unlink(ftemp, recursive=TRUE)
    temp <- lapply(lapply(c("tmax", "tmin", "ppt"), FUN=function(x) file.path(ftemp, x)), FUN=function(x) dir.create(x, recursive=TRUE, showWarnings=FALSE))

    0L
  }

  temp <- .local()
  res <- if(!inherits(temp, "try-error")) list(dir.in.cfsr=dir.in.cfsr, cfsr_so=fname_cfsr)  else temp

  res
})

# Wrapper functions for C code to access NCEP/CFSR data and write out to temporary files
gribDailyWeatherData <- compiler::cmpfun(function(id, do_daily, nSites, latitudes, longitudes) {
  if(id %% 36 == 1) print(paste(Sys.time(), ": NCEP/CFSR extraction: year=", do_daily[id, "years"]))

  gribData <- .C("dailyWeather2_R",
            nSites = as.integer(nSites),
            latitudes = as.double(latitudes),
            longitudes = as.double(longitudes),
            year = as.integer(do_daily[id, "years"]),
            month = as.integer(do_daily[id, "months"]),
            type = as.integer(do_daily[id, "types"]))
  1L
})

writeDailyWeatherData <- compiler::cmpfun(function(year, nSites, siteNames, siteDirsC) {
  dataWrite <- .C("dailyWeather2Write_R",
            nSites = as.integer(nSites),
            siteNames = as.character(siteNames),
            siteDirs = as.character(siteDirsC),
            year = as.integer(year))
  1L
})

gribMonthlyClimate <- compiler::cmpfun(function(type, nSites, latitudes, longitudes, siteDirsC, yearLow, yearHigh) {
  gribData <- .C("monthlyClimate2_R",
            nSites = as.integer(nSites),
            latitudes = as.double(latitudes),
            longitudes = as.double(longitudes),
            siteDirs = as.character(siteDirsC),
            yearLow = as.integer(yearLow),
            yearHigh = as.integer(yearHigh),
            type = as.integer(type))
  1L
})

writeMonthlyClimate <- compiler::cmpfun(function(id, siteDirsC) {
  dataWrite <- .C("writeMonthlyClimate2_R", siteDir = as.character(siteDirsC[id]))
  1L
})

create_filename_for_Maurer2002_NorthAmerica <- compiler::cmpfun(function(X_WGS84, Y_WGS84){
  gsub("[[:space:]]", "", paste("data", formatC(28.8125+round((Y_WGS84-28.8125)/0.125,0)*0.125, digits=4, format="f"), formatC(28.8125+round((X_WGS84-28.8125)/0.125,0)*0.125, digits=4, format="f"), sep="_"))
})

#' @examples
#'  month1 <- function() as.POSIXlt(seq(from = ISOdate(1980, 1, 1, tz = "UTC"),
#'     to = ISOdate(2010, 12, 31, tz = "UTC"), by = "1 day"))$mon + 1
#'  month2 <- function() seq_month_ofeach_day(list(1980, 1, 1),
#'    list(2010, 12, 31), tz = "UTC")
#'
#'  if (requireNamespace("microbenchmark", quietly = TRUE))
#'    microbenchmark::microbenchmark(month1(), month2())    # barely any difference
#'
seq_month_ofeach_day <- compiler::cmpfun(function(from = list(year = 1900, month = 1, day = 1),
  to = list(year = 1900, month = 12, day = 31), tz = "UTC") {

  x <- paste(from[[1]], from[[2]], from[[3]], 12, 0, 0, sep = "-")
  from0 <- unclass(as.POSIXct.POSIXlt(strptime(x, "%Y-%m-%d-%H-%M-%OS", tz = tz)))
  x <- paste(to[[1]], to[[2]], to[[3]], 12, 0, 0, sep = "-")
  to0 <- unclass(as.POSIXct.POSIXlt(strptime(x, "%Y-%m-%d-%H-%M-%OS", tz = tz)))

  res <- seq.int(0, to0 - from0, by = 86400) + from0
  as.POSIXlt.POSIXct(.POSIXct(res, tz = tz))$mon + 1
})


simTiming <- compiler::cmpfun(function(startyr, simstartyr, endyr) {
  res <- list()
  #simyrs <- simstartyr:endyr
  #no.simyr <- endyr - simstartyr + 1
  temp <- ISOdate(startyr, 1, 1, tz = "UTC")

  res[["useyrs"]] <- startyr:endyr

  res[["no.useyr"]] <- endyr - startyr + 1
  res[["no.usemo"]] <- res[["no.useyr"]] * 12
  res[["no.usedy"]] <- as.numeric(ISOdate(endyr, 12, 31, tz = "UTC") - temp) + 1

  res[["discardyr"]] <- startyr - simstartyr
  res[["discardmo"]] <- res[["discardyr"]] * 12
  res[["discarddy"]] <- as.numeric(temp - ISOdate(simstartyr, 1, 1, tz = "UTC"))

  res[["index.useyr"]] <- res[["discardyr"]] + seq_len(res[["no.useyr"]])
  res[["index.usemo"]] <- res[["discardmo"]] + seq_len(res[["no.usemo"]])
  res[["index.usedy"]] <- res[["discarddy"]] + seq_len(res[["no.usedy"]])

  res
})

simTiming_ForEachUsedTimeUnit <- compiler::cmpfun(function(st, sim_tscales, latitude = 90, account_NorthSouth = TRUE) {	#positive latitudes -> northern hemisphere; negative latitudes -> southern hemisphere
  res <- list()

  if (any(sim_tscales == "daily")) {
    temp <- as.POSIXlt(seq(from = ISOdate(min(st$useyrs), 1, 1, tz = "UTC"),
                           to = ISOdate(max(st$useyrs), 12, 31, tz = "UTC"),
                           by = "1 day"))

    res$doy_ForEachUsedDay <- res$doy_ForEachUsedDay_NSadj <- temp$yday + 1
    res$month_ForEachUsedDay <- res$month_ForEachUsedDay_NSadj <- temp$mon + 1
    res$year_ForEachUsedDay <- res$year_ForEachUsedDay_NSadj <- temp$year + 1900

    if (latitude < 0 && account_NorthSouth) {
      dshift <- as.POSIXlt(ISOdate(st$useyrs, 6, 30, tz = "UTC"))$yday + 1	#new month either at end of year or in the middle because the two halfs (6+6 months) of a year are of unequal length (182 (183 if leap year) and 183 days): I chose to have a new month at end of year (i.e., 1 July -> 1 Jan & 30 June -> 31 Dec; but, 1 Jan -> July 3/4): and instead of a day with doy=366, there are two with doy=182
      res$doy_ForEachUsedDay_NSadj <- unlist(lapply(seq_along(st$useyrs), function(x) {
        temp <- res$doy_ForEachUsedDay[st$useyrs[x] == res$year_ForEachUsedDay]
        c(temp[-(1:dshift[x])], temp[1:dshift[x]])
      }))
      res$month_ForEachUsedDay_NSadj <- strptime(paste(res$year_ForEachUsedDay, res$doy_ForEachUsedDay_NSadj, sep="-"), format="%Y-%j")$mon + 1
      temp <- length(res$year_ForEachUsedDay)
      delta <- if(dshift[1] == 182) 2 else 3
      res$year_ForEachUsedDay_NSadj <- c(
        rep(st$useyrs[1] - 1, times = dshift[1] + delta),
        res$year_ForEachUsedDay[-((temp - dshift[1] - delta):temp)]
      )
    }
  }

  if (any(sim_tscales == "weekly")) {

  }

  if (any(sim_tscales == "monthly")) {
    res$yearno_ForEachUsedMonth <- res$yearno_ForEachUsedMonth_NSadj <- rep(seq_len(st$no.useyr), each = 12)
    res$month_ForEachUsedMonth <- res$month_ForEachUsedMonth_NSadj <- rep(st_mo, times = st$no.useyr)

    if (latitude < 0 && account_NorthSouth) {
      res$month_ForEachUsedMonth_NSadj <- (res$month_ForEachUsedMonth + 5) %% 12 + 1
    }
  }

  if (any(sim_tscales == "yearly")) {

  }

  res
})


#------auxiliary functions

in_box <- compiler::cmpfun(function(xy, xbounds, ybounds, i_use) {
  !i_use &
  xy[, 1] >= xbounds[1] & xy[, 1] <= xbounds[2] &
  xy[, 2] >= ybounds[1] & xy[, 2] <= ybounds[2]
})

adjustLayersDepth <- compiler::cmpfun(function(layers_depth, d) round(layers_depth[seq_len(d)])) #The wrapper only handles 1-cm resolution of soil depths (maily because of the trco)
getLayersWidth <- compiler::cmpfun(function(layers_depth) diff(c(0, layers_depth)))
setLayerSequence <- compiler::cmpfun(function(d) seq_len(d))

sw_dailyC4_TempVar <- compiler::cmpfun(function(dailyTempMin, dailyTempMean, simTime2) {
  #Variables to estimate percent C4 species in North America: Teeri JA, Stowe LG (1976) Climatic patterns and the distribution of C4 grasses in North America. Oecologia, 23, 1-12.

  temp7 <- simTime2$month_ForEachUsedDay_NSadj == 7
  Month7th_MinTemp_C <- tapply(dailyTempMin[temp7], simTime2$year_ForEachUsedDay_NSadj[temp7], min)
  LengthFreezeFreeGrowingPeriod_Days <- tapply(dailyTempMin, simTime2$year_ForEachUsedDay_NSadj,
    function(x) {
      temp <- rle(x > 0)
      if (any(temp$values)) max(temp$lengths[temp$values], na.rm = TRUE) else 0
    })
  temp_base65F <- dailyTempMean - 18.333  # 18.333 C = 65 F with (65 - 32) * 5 / 9
  temp_base65F[temp_base65F < 0] <- 0
  DegreeDaysAbove65F_DaysC <- tapply(temp_base65F, simTime2$year_ForEachUsedDay_NSadj, sum)

  nyrs <- seq_along(Month7th_MinTemp_C) #if southern Hemisphere, then 7th month of last year is not included
  temp <- cbind(Month7th_MinTemp_C[nyrs],
                LengthFreezeFreeGrowingPeriod_Days[nyrs],
                DegreeDaysAbove65F_DaysC[nyrs])
  res <- c(apply(temp, 2, mean), apply(temp, 2, sd))
  temp <- c("Month7th_NSadj_MinTemp_C",
            "LengthFreezeFreeGrowingPeriod_NSadj_Days",
            "DegreeDaysAbove65F_NSadj_DaysC")
  names(res) <- c(temp, paste0(temp, ".sd"))

  res
})

sw_SiteClimate_Ambient <- compiler::cmpfun(function(weatherList, year.start, year.end, do.C4vars = FALSE, simTime2 = NULL) {
  x <- Rsoilwat31::dbW_weatherData_to_dataframe(weatherList)

  # Trim to years
  years <- as.numeric(unlist(lapply(weatherList, function(x) x@year)))
  years <- years[year.start <= years & year.end >= years]

  x <- x[year.start <= x[, "Year"] & year.end >= x[, "Year"], ]
  xl <- list(
          months = as.POSIXlt(seq(from = ISOdate(years[1], 1, 1, tz = "UTC"),
                                 to = ISOdate(years[length(years)], 12, 31, tz = "UTC"),
                                 by = "1 day"))$mon + 1,
          Tmean_C = rowMeans(x[, c("Tmax_C", "Tmin_C")])
        )

  index <- xl[["months"]] + 100 * x[, "Year"]
  temp <- vapply(list(xl[["Tmean_C"]], x[, "Tmin_C"], x[, "Tmax_C"]), function(data)
    matrix(tapply(data, index, mean), nrow = 12),
    FUN.VALUE = matrix(NA_real_, nrow = 12, ncol = length(years)))
  tempPPT <- matrix(tapply(x[, "PPT_cm"], index, sum), nrow = 12)

  list(
    meanMonthlyTempC = apply(temp[, , 1], 1, mean),
    minMonthlyTempC = apply(temp[, , 2], 1, mean),
    maxMonthlyTempC = apply(temp[, , 3], 1, mean),
    meanMonthlyPPTcm = apply(tempPPT, 1, mean),

    MAP_cm = sum(tempPPT) / length(years),
    MAT_C = mean(xl[["Tmean_C"]]),

    dailyTempMin = if (do.C4vars) x[, "Tmin_C"] else NA,
    dailyTempMean = if (do.C4vars) xl[["Tmean_C"]] else NA,
    dailyC4vars = if (do.C4vars) {
        sw_dailyC4_TempVar(dailyTempMin = x[, "Tmin_C"], dailyTempMean = xl[["Tmean_C"]], simTime2)
      } else NA
  )
})

cut0Inf <- compiler::cmpfun(function(x, val = NA) {
  x[x < 0] <- val
  x
})
NAto0 <- compiler::cmpfun(function(x) {
  x[is.na(x)] <- 0
  x
})
finite01 <- compiler::cmpfun(function(x, val_low = 0, val_high = 1) {
  x[x < 0 | is.na(x)] <- val_low
  x[x > 1] <- val_high
  x
})

PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996 <- compiler::cmpfun(function(MAP_mm,MAT_C,monthly.ppt,monthly.temp,dailyC4vars,isNorth,shrub.fraction.limit,
    use_Annuals_Fraction,Annuals_Fraction,
    use_C4_Fraction,C4_Fraction,
    use_C3_Fraction,C3_Fraction,
    use_Shrubs_Fraction,Shrubs_Fraction,
    use_Forbs_Fraction, Forbs_Fraction,
    use_BareGround_Fraction, BareGround_Fraction) {

  f.digits <- 3
  tolerance <- 1.1*10^-f.digits

  #Get the user specified fractions, if column is false set to NA
  tree.fraction <- 0 #option 'PotentialNaturalVegetation_CompositionShrubsC3C4_Paruelo1996' doesn't estimate tree cover, i.e., assumed to be == 0
  forb.fraction <- 0
  bareGround.fraction <- 0
  AnnC4C3ShrubForbBareGroundFraction <- rep(NA, 6)
  if(use_Annuals_Fraction){
    AnnC4C3ShrubForbBareGroundFraction[1] <- finite01(Annuals_Fraction)
  } else {
    AnnC4C3ShrubForbBareGroundFraction[1] <- 0 #Annuals can not be NA
  }
  if(use_C4_Fraction)
    AnnC4C3ShrubForbBareGroundFraction[2] <- C4_Fraction
  if(use_C3_Fraction)
    AnnC4C3ShrubForbBareGroundFraction[3] <- C3_Fraction
  if(use_Shrubs_Fraction)
    AnnC4C3ShrubForbBareGroundFraction[4] <- Shrubs_Fraction

  if(use_Forbs_Fraction) {
    AnnC4C3ShrubForbBareGroundFraction[5] <- finite01(Forbs_Fraction)
  } else {
    AnnC4C3ShrubForbBareGroundFraction[5] <- forb.fraction
  }
  if(use_BareGround_Fraction) {
    AnnC4C3ShrubForbBareGroundFraction[6] <- finite01(BareGround_Fraction)
  } else {
    AnnC4C3ShrubForbBareGroundFraction[6] <- bareGround.fraction
  }
  AnnC4C3ShrubForbBareGroundFraction <- cut0Inf(AnnC4C3ShrubForbBareGroundFraction) #treat negatives as if NA
  TotalFraction <- sum(AnnC4C3ShrubForbBareGroundFraction, na.rm=TRUE)

  #Decide if all fractions are sufficiently defined or if they need to be calculated based on climate variables
  if(!isTRUE(all.equal(TotalFraction, 1, tolerance=tolerance)) && TotalFraction < 1 && sum(is.na(AnnC4C3ShrubForbBareGroundFraction)) == 0) {
    stop(print(paste(" run: User defined fractions of Shrub, C3, C4, Annuals are all set, but less than 1", sep=""))) #throw an error
  }

  if(isTRUE(all.equal(TotalFraction, 1, tolerance=tolerance)) || TotalFraction > 1 || sum(is.na(AnnC4C3ShrubForbBareGroundFraction)) == 1){

    if(sum(is.na(AnnC4C3ShrubForbBareGroundFraction)) == 1){ #if only one is NA, then this can be calculated
      AnnC4C3ShrubForbBareGroundFraction[which(is.na(AnnC4C3ShrubForbBareGroundFraction))] <- cut0Inf(1 - TotalFraction)
    } else {
      AnnC4C3ShrubForbBareGroundFraction <- finite01(AnnC4C3ShrubForbBareGroundFraction) #the composition is >= 1, so set eventually remaining NA to 0
    }

    TotalFraction <- sum(AnnC4C3ShrubForbBareGroundFraction, na.rm=TRUE)
    AnnC4C3ShrubForbBareGroundFraction <- AnnC4C3ShrubForbBareGroundFraction / TotalFraction #Rescale, in case it is needed

  } else { #i.e., (TotalFraction < 1 && sum(is.na(AnnC4C3ShrubForbBareGroundFraction)) > 1) is TRUE; thus, calculate some fractions based on climate variables
    if(isNorth){ #Northern hemisphere
      Months_WinterTF <- c(12, 1:2)
      Months_SummerTF <- c(6:8)
    } else {
      Months_WinterTF <- c(6:8)
      Months_SummerTF <- c(12, 1:2)
    }
    ppt.SummerToMAP <- sum(monthly.ppt[Months_SummerTF]) / MAP_mm
    ppt.WinterToMAP <- sum(monthly.ppt[Months_WinterTF]) / MAP_mm

    #---Potential natural vegetation
    #1. step: Paruelo JM, Lauenroth WK (1996) Relative abundance of plant functional types in grasslands and shrublands of North America. Ecological Applications, 6, 1212-1224.
    if(MAP_mm < 1){
      shrubs.fractionNA <- NA
    } else {
      shrubs.fractionNA <- cut0Inf(1.7105 - 0.2918 * log(MAP_mm) + 1.5451 * ppt.WinterToMAP) 								#if NA, then not enough winter precipitation above a given MAP
    }
    if(MAT_C <= 0){
      grass.c4.fractionNA <- 0
    } else {
      grass.c4.fractionNA <- cut0Inf(-0.9837 + 0.000594 * MAP_mm + 1.3528 * ppt.SummerToMAP + 0.2710 * log(MAT_C))			#if NA, then either MAT < 0 or not enough summer precipitation or too cold below a given MAP
    }
    if(ppt.WinterToMAP <= 0){
      grass.c3ingrasslands.fractionNA <- grass.c3inshrublands.fractionNA <- NA
    } else {
      grass.c3ingrasslands.fractionNA <- cut0Inf(1.1905 - 0.02909 * MAT_C + 0.1781 * log(ppt.WinterToMAP) - 0.2383 * 1)		#if NA, then not enough winter precipitation or too warm below a given MAP
      grass.c3inshrublands.fractionNA <- cut0Inf(1.1905 - 0.02909 * MAT_C + 0.1781 * log(ppt.WinterToMAP) - 0.2383 * 2)
    }
    grass.c3.fractionNA <- ifelse(shrubs.fractionNA >= shrub.fraction.limit && !is.na(shrubs.fractionNA), grass.c3inshrublands.fractionNA, grass.c3ingrasslands.fractionNA)

    grass.Annual.fraction <- AnnC4C3ShrubForbBareGroundFraction[1] #Ann will be 0 or something <= 1

    #2. step: Teeri JA, Stowe LG (1976) Climatic patterns and the distribution of C4 grasses in North America. Oecologia, 23, 1-12.
    #This equations give percent species/vegetation -> use to limit Paruelo's C4 equation, i.e., where no C4 species => there are no C4 abundance > 0
    if(dailyC4vars["LengthFreezeFreeGrowingPeriod_NSadj_Days"] <= 0){
      grass.c4.species <- 0
    } else {
      x10 <- dailyC4vars["Month7th_NSadj_MinTemp_C"] * 9/5 + 32
      x13 <- dailyC4vars["DegreeDaysAbove65F_NSadj_DaysC"] * 9/5
      x18 <- log(dailyC4vars["LengthFreezeFreeGrowingPeriod_NSadj_Days"])
      grass.c4.species <- as.numeric((1.60 * x10 + 0.0086 * x13 - 8.98 * x18 - 22.44) / 100)
    }
    grass.c4.fractionNA <- ifelse(grass.c4.species >= 0, grass.c4.fractionNA, NA)

    #3. step: Replacing missing values: If no or only one successful equation, then add 100% C3 if MAT < 10 C, 100% shrubs if MAP < 600 mm, and 100% C4 if MAT >= 10C & MAP >= 600 mm	[these rules are made up arbitrarily by drs, Nov 2012]
    if(sum(!is.na(shrubs.fractionNA), !is.na(grass.c4.fractionNA), !is.na(grass.c3.fractionNA)) <= 1){
      if(MAP_mm < 600) shrubs.fractionNA <- 1 + ifelse(is.na(shrubs.fractionNA), 0, shrubs.fractionNA)
      if(MAT_C < 10)  grass.c3.fractionNA <- 1 + ifelse(is.na(grass.c3.fractionNA), 0, grass.c3.fractionNA)
      if(MAT_C >= 10  & MAP_mm >= 600)  grass.c4.fractionNA <- 1 + ifelse(is.na(grass.c4.fractionNA), 0, grass.c4.fractionNA)
    }

    #4. step: Scale fractions to 0-1 with a sum of 1 including grass.Annual.fraction, but don't scale grass.Annual.fraction
    #if na then use calc fraction else use the user defined fraction
    shrubs.fraction <- NAto0(shrubs.fractionNA)
    grass.c4.fraction <- NAto0(grass.c4.fractionNA)
    grass.c3.fraction <- NAto0(grass.c3.fractionNA)

    sumVegWithoutAnnuals <- shrubs.fraction + grass.c4.fraction + grass.c3.fraction
    shrubs.fraction <- (shrubs.fraction / sumVegWithoutAnnuals) * (1 - grass.Annual.fraction) #scale these down to 1-annual fraction
    grass.c4.fraction <- (grass.c4.fraction / sumVegWithoutAnnuals) * (1 - grass.Annual.fraction)
    grass.c3.fraction <- (grass.c3.fraction / sumVegWithoutAnnuals) * (1 - grass.Annual.fraction)

    calcAnnC4C3ShrubForbBareGroundFraction <- c(grass.Annual.fraction, grass.c4.fraction, grass.c3.fraction, shrubs.fraction)
    naIndex <- which(is.na(AnnC4C3ShrubForbBareGroundFraction))
    #replace missing values
    if(isTRUE(all.equal(sum(calcAnnC4C3ShrubForbBareGroundFraction[naIndex]), 0)) && isTRUE(all.equal(temp <- sum(AnnC4C3ShrubForbBareGroundFraction[!naIndex]), 0))){ #there would be no vegetation, so force vegetation > 0
      AnnC4C3ShrubForbBareGroundFraction[naIndex] <- (1 - temp) / length(naIndex)
    } else {
      AnnC4C3ShrubForbBareGroundFraction[naIndex] <- calcAnnC4C3ShrubForbBareGroundFraction[naIndex]
    }
    #now we need to get the sum and scale the naIndex values accordingly
    AnnC4C3ShrubForbBareGroundFraction[naIndex] <- sapply(AnnC4C3ShrubForbBareGroundFraction[naIndex], function(x) (x/sum(AnnC4C3ShrubForbBareGroundFraction[naIndex])) * (1-sum(AnnC4C3ShrubForbBareGroundFraction[-naIndex])))
  }

  #Scale Grass components to one (or set to 0)
  if(!isTRUE(all.equal(sum(AnnC4C3ShrubForbBareGroundFraction[4:6]), 1))){
    grass.c4.fractionG <- AnnC4C3ShrubForbBareGroundFraction[2] / (1-sum(AnnC4C3ShrubForbBareGroundFraction[4:6]))
    grass.c3.fractionG <- AnnC4C3ShrubForbBareGroundFraction[3] / (1-sum(AnnC4C3ShrubForbBareGroundFraction[4:6]))
    grass.Annual.fractionG <- AnnC4C3ShrubForbBareGroundFraction[1] / (1-sum(AnnC4C3ShrubForbBareGroundFraction[4:6]))
  } else {
    grass.c4.fractionG <- grass.c3.fractionG <- grass.Annual.fractionG <- 0
  }
  grass.fraction <- sum(AnnC4C3ShrubForbBareGroundFraction[c(1:3)])

  list(Composition = c(Grasses = grass.fraction,
                         Shrubs = AnnC4C3ShrubForbBareGroundFraction[4],
                         Trees = tree.fraction,
                         Forbs = AnnC4C3ShrubForbBareGroundFraction[5],
                         BareGround = AnnC4C3ShrubForbBareGroundFraction[6]),
       grasses.c3c4ann.fractions = c(grass.c3.fractionG,
                                       grass.c4.fractionG,
                                       grass.Annual.fractionG))
})

calc.loess_coeff <- compiler::cmpfun(function(N, span) {
  #prevent call to loessc.c:ehg182(104): "span too small.   fewer data values than degrees of freedom"
  lcoef <- list(span = min(1, span), degree = 2)
  if (span <= 1) {
    nf <- floor(lcoef$span * N) - 1 #see R/trunk/src/library/stats/src/loessf.f:ehg136()
    if (nf > 2) {
      lcoef$degree <- 2
    } else if(nf > 1){
      lcoef$degree <- 1
    } else {
      lcoef <- Recall(N, lcoef$span + 0.1)
    }
  }
  lcoef
})

predict.season <- compiler::cmpfun(function(biomass_Standard, std.season.padded, std.season.seq, site.season.seq) {
  #length(std.season.seq) >= 3 because of padding and test that season duration > 0
  lcoef <- calc.loess_coeff(N = length(std.season.seq), span = 0.4)

  op <- options(c("warn", "error"))
  on.exit(options(op))
  options(warn = -1, error = traceback) #loess throws many warnings: 'pseudoinverse used', see calc.loess_coeff(), etc.

  sapply(apply(biomass_Standard, 2, function(x) {
      lf <- loess(x[std.season.padded] ~ std.season.seq, span = lcoef$span, degree = lcoef$degree)
      predict(lf, newdata = data.frame(std.season.seq = site.season.seq))
    }),
    FUN = function(x) max(0, x)) # guarantee that > 0
})

#Equations: Milchunas & Lauenroth 1993 (Fig. 2): Y [g/m2/yr] = c1 * MAP [mm/yr] + c2
Shrub_ANPP <- compiler::cmpfun(function(MAP_mm) 0.393 * MAP_mm - 10.2)
Grass_ANPP <- compiler::cmpfun(function(MAP_mm) 0.646 * MAP_mm - 102.5)

#' @section Default inputs:
#'    - shrubs (IM_USC00107648_Reynolds; 70% shrubs, 30% C3): biomass was estimated at MAP = 450 mm/yr
#'    - sgs-grassland (GP_SGSLTER; 12% shrubs, 22% C3, and 66% C4): biomass was estimated at MAP = 340 mm/yr
adjBiom_by_ppt <- compiler::cmpfun(function(biom_shrubs, biom_C3, biom_C4, biom_annuals, biom_maxs,
         map_mm_shrubs, map_mm_std_shrubs,
         map_mm_grasses, map_mm_std_grasses,
         vegcomp_std_shrubs, vegcomp_std_grass, tol. = tol) {

  #Intercepts to match outcomes of M & L 1993 equations under 'default' MAP with our previous default inputs for shrubs and sgs-grasslands
  #Whereas these intercepts were introduced artificially, they could also be interpreted as perennial storage, e.g., Lauenroth & Whitman (1977) found "Accumulation in the standing dead was 63% of inputs, in the litter 8%, and belowground 37%.". Lauenroth, W.K. & Whitman, W.C. (1977) Dynamics of dry matter production in a mixed-grass prairie in western North Dakota. Oecologia, 27, 339-351.
  Shrub_ANPPintercept <- (vegcomp_std_shrubs[1] * biom_maxs["Sh.Amount.Live"] +
                          vegcomp_std_shrubs[2] * biom_maxs["C3.Amount.Live"] +
                          vegcomp_std_shrubs[3] * biom_maxs["C4.Amount.Live"]) -
                        Shrub_ANPP(map_mm_std_shrubs)
  Grasses_ANPPintercept <- (vegcomp_std_grass[1] * biom_maxs["Sh.Amount.Live"] +
                            vegcomp_std_grass[2] * biom_maxs["C3.Amount.Live"] +
                            vegcomp_std_grass[3] * biom_maxs["C4.Amount.Live"]) -
                          Grass_ANPP(map_mm_std_grasses)

  #Get scaling values for scaled biomass; guarantee that > minimum.totalBiomass
  minimum.totalBiomass <- 0 #This is a SoilWat parameter
  Shrub_BiomassScaler <- max(minimum.totalBiomass, Shrub_ANPP(map_mm_shrubs) + Shrub_ANPPintercept)
  Grass_BiomassScaler <- max(minimum.totalBiomass, Grass_ANPP(map_mm_grasses) + Grasses_ANPPintercept)

  #Scale live biomass amount by productivity; assumption: ANPP = peak standing live biomass
  biom_shrubs$Sh.Amount.Live <- biom_shrubs$Sh.Amount.Live * Shrub_BiomassScaler
  biom_C3$C3.Amount.Live <- biom_C3$C3.Amount.Live * Grass_BiomassScaler
  biom_C4$C4.Amount.Live <- biom_C4$C4.Amount.Live * Grass_BiomassScaler
  biom_annuals$Annual.Amount.Live <- biom_annuals$Annual.Amount.Live * Grass_BiomassScaler

  #Scale litter amount by productivity and adjust for ratio of litter/live
  biom_shrubs$Sh.Litter <- biom_shrubs$Sh.Litter * Shrub_BiomassScaler * biom_maxs["Sh.Litter"] / biom_maxs["Sh.Amount.Live"]
  biom_C3$C3.Litter <- biom_C3$C3.Litter * Grass_BiomassScaler * biom_maxs["C3.Litter"] / biom_maxs["C3.Amount.Live"]
  biom_C4$C4.Litter <- biom_C4$C4.Litter * Grass_BiomassScaler * biom_maxs["C4.Litter"] / biom_maxs["C4.Amount.Live"]
  biom_annuals$Annual.Litter <- biom_annuals$Annual.Litter * Grass_BiomassScaler * biom_maxs["Annual.Litter"] / biom_maxs["Annual.Amount.Live"]

  #Guarantee that live fraction = ]0, 1]
  biom_shrubs$Sh.Perc.Live <- pmin(1, pmax(tol., biom_shrubs$Sh.Perc.Live))
  biom_C3$C3.Perc.Live <- pmin(1, pmax(tol., biom_C3$C3.Perc.Live))
  biom_C4$C4.Perc.Live <- pmin(1, pmax(tol., biom_C4$C4.Perc.Live))
  biom_annuals$Annual.Perc.Live <- pmin(1, pmax(tol., biom_annuals$Annual.Perc.Live))

  #Calculate total biomass based on scaled live biomass amount
  biom_shrubs$Sh.Biomass <- biom_shrubs$Sh.Amount.Live / biom_shrubs$Sh.Perc.Live
  biom_C3$C3.Biomass <- biom_C3$C3.Amount.Live / biom_C3$C3.Perc.Live
  biom_C4$C4.Biomass <- biom_C4$C4.Amount.Live / biom_C4$C4.Perc.Live
  biom_annuals$Annual.Biomass <- biom_annuals$Annual.Amount.Live / biom_annuals$Annual.Perc.Live

  list(biom_shrubs = biom_shrubs,
       biom_C3 = biom_C3,
       biom_C4 = biom_C4,
       biom_annuals = biom_annuals)
})

calc_starts <- compiler::cmpfun(function(x) {
  temp1 <- rle(as.logical(x))
  temp2 <- cumsum(c(0, temp1$lengths)) + 1
  temp2[-length(temp2)][temp1$values]
})

AdjMonthlyBioMass <- compiler::cmpfun(function(tr_VegBiom,
                do_adjBiom_by_temp = FALSE, do_adjBiom_by_ppt = FALSE,
                fgrass_c3c4ann, growing_limit_C = 4,
                isNorth = TRUE, MAP_mm = 450, monthly.temp) {

  #Default shrub biomass input is at MAP = 450 mm/yr, and default grass biomass input is at MAP = 340 mm/yr
  #Describe conditions for which the default vegetation biomass values are valid
  std.winter <- c(11:12, 1:2) #Assumes that the "growing season" (valid for growing_limit_C == 4) in 'tr_VegetationComposition' starts in March and ends after October, for all functional groups.
  std.growing <- seq_len(12L)[-std.winter] #Assumes that the "growing season" in 'tr_VegetationComposition' starts in March and ends after October, for all functional groups.
  #Default site for the grass description is SGS LTER
  StandardGrasses_MAP_mm <- 340
  StandardGrasses_VegComposition <- c(0.12, 0.22, 0.66) #Fraction of shrubs, C3, and C4
  #Default site for the shrub description is Reynolds Creek, ID
  StandardShrub_MAP_mm <- 250
  StandardShrub_VegComposition <- c(0.7, 0.3, 0) #Fraction of shrubs, C3, and C4

  #Calculate 'live biomass amount'
  tr_VegBiom$Sh.Amount.Live <- tr_VegBiom$Sh.Biomass * tr_VegBiom$Sh.Perc.Live
  tr_VegBiom$C3.Amount.Live <- tr_VegBiom$C3.Biomass * tr_VegBiom$C3.Perc.Live
  tr_VegBiom$C4.Amount.Live <- tr_VegBiom$C4.Biomass * tr_VegBiom$C4.Perc.Live
  tr_VegBiom$Annual.Amount.Live <- tr_VegBiom$Annual.Biomass * tr_VegBiom$Annual.Perc.Live

  #Scale monthly values of litter and live biomass amount by column-max; total biomass will be back calculated from 'live biomass amount' / 'percent live'
  itemp <- grepl("Litter", names(tr_VegBiom)) | grepl("Amount.Live", names(tr_VegBiom))
  colmax <- apply(tr_VegBiom[, itemp], MARGIN=2, FUN=max)
#  colmin <- apply(tr_VegBiom[, itemp], MARGIN=2, FUN=min)
  tr_VegBiom[, itemp] <- sweep(tr_VegBiom[, itemp], MARGIN=2, STATS=colmax, FUN="/")

  #Pull different vegetation types
  biom_shrubs <- biom_std_shrubs <- tr_VegBiom[, grepl("Sh", names(tr_VegBiom))]
  biom_C3 <- biom_std_C3 <- tr_VegBiom[, grepl("C3", names(tr_VegBiom))]
  biom_C4 <- biom_std_C4 <- tr_VegBiom[, grepl("C4", names(tr_VegBiom))]
  biom_annuals <- biom_std_annuals <- tr_VegBiom[, grepl("Annual", names(tr_VegBiom))]

  #adjust phenology for mean monthly temperatures
  if (do_adjBiom_by_temp) {
    growing.season <- as.vector(monthly.temp >= growing_limit_C)
    n_nonseason <- sum(!growing.season)
    n_season <- sum(growing.season)

    if (!isNorth)
      growing.season <- c(growing.season[7:12], growing.season[1:6]) #Standard growing season needs to be adjusted for southern Hemi

    #Adjust for timing and duration of non-growing season
    if (n_nonseason > 0) {
      if (n_nonseason < 12) {
        std.winter.padded <- (c(std.winter[1] - 1, std.winter, std.winter[length(std.winter)] + 1) - 1) %% 12 + 1
        std.winter.seq <- 0:(length(std.winter.padded) - 1)
        site.winter.seq <- seq(from = 1, to = length(std.winter), length = n_nonseason)
        starts <- calc_starts(!growing.season)
        site.winter.start <- starts[length(starts)] #Calculate first month of winter == last start of non-growing season
        site.winter.months <- (site.winter.start + seq_len(n_nonseason) - 2) %% 12 + 1

        biom_shrubs[site.winter.months,] <- predict.season(biom_std_shrubs, std.winter.padded, std.winter.seq, site.winter.seq)
        biom_C3[site.winter.months,] <- predict.season(biom_std_C3, std.winter.padded, std.winter.seq, site.winter.seq)
        biom_C4[site.winter.months,] <- predict.season(biom_std_C4, std.winter.padded, std.winter.seq, site.winter.seq)
        biom_annuals[site.winter.months,] <- predict.season(biom_std_annuals, std.winter.padded, std.winter.seq, site.winter.seq)

      } else { #if winter lasts 12 months
        #Take the mean of the winter months
        biom_shrubs[] <- matrix(apply(biom_std_shrubs[std.winter,], 2, mean), nrow=12, ncol=ncol(biom_shrubs), byrow=TRUE)
        biom_C3[] <- matrix(apply(biom_std_C3[std.winter,], 2, mean), nrow=12, ncol=ncol(biom_C3), byrow=TRUE)
        biom_C4[] <- matrix(apply(biom_std_C4[std.winter,], 2, mean), nrow=12, ncol=ncol(biom_C4), byrow=TRUE)
        biom_annuals[] <- matrix(apply(biom_std_annuals[std.winter,], 2, mean), nrow=12, ncol=ncol(biom_annuals), byrow=TRUE)
      }
    }

    #Adjust for timing and duration of growing season
    if (n_season > 0) {
      if (n_season < 12) {
        std.growing.padded <- (c(std.growing[1] - 1, std.growing, std.growing[length(std.growing)] + 1) - 1) %% 12 + 1
        std.growing.seq <- 0:(length(std.growing.padded) - 1)
        site.growing.seq <- seq(from = 1, to = length(std.growing), length = n_season)
        starts <- calc_starts(growing.season)
        site.growing.start <- starts[1] #Calculate first month of growing season == first start of growing season
        site.growing.months <- (site.growing.start + seq_len(n_season) - 2) %% 12 + 1

        biom_shrubs[site.growing.months,] <- predict.season(biom_std_shrubs, std.growing.padded, std.growing.seq, site.growing.seq)
        biom_C3[site.growing.months,] <- predict.season(biom_std_C3, std.growing.padded, std.growing.seq, site.growing.seq)
        biom_C4[site.growing.months,] <- predict.season(biom_std_C4, std.growing.padded, std.growing.seq, site.growing.seq)
        biom_annuals[site.growing.months,] <- predict.season(biom_std_annuals, std.growing.padded, std.growing.seq, site.growing.seq)

      } else { #if growing season lasts 12 months
        biom_shrubs[] <- matrix(apply(biom_std_shrubs[std.growing,], MARGIN=2, FUN=max), nrow=12, ncol=ncol(biom_shrubs), byrow=TRUE)
        biom_C3[] <- matrix(apply(biom_std_C3[std.growing,], MARGIN=2, FUN=max), nrow=12, ncol=ncol(biom_C3), byrow=TRUE)
        biom_C4[] <- matrix(apply(biom_std_C4[std.growing,], MARGIN=2, FUN=max), nrow=12, ncol=ncol(biom_C4), byrow=TRUE)
        biom_annuals[] <- matrix(apply(biom_std_annuals[std.growing,], MARGIN=2, FUN=max), nrow=12, ncol=ncol(biom_annuals), byrow=TRUE)
      }
    }

    if (!isNorth) { #Adjustements were done as if on northern hemisphere
      biom_shrubs <- rbind(biom_shrubs[7:12,], biom_shrubs[1:6,])
      biom_C3 <- rbind(biom_C3[7:12,], biom_C3[1:6,])
      biom_C4 <- rbind(biom_C4[7:12,], biom_C4[1:6,])
      biom_annuals <- rbind(biom_annuals[7:12,], biom_annuals[1:6,])
    }
  }

  # if (do_adjBiom_by_ppt) then adjust biomass amounts by productivity relationship with MAP
  temp <- adjBiom_by_ppt(
    biom_shrubs, biom_C3, biom_C4, biom_annuals,
    biom_maxs = colmax,
    map_mm_shrubs = if (do_adjBiom_by_ppt) MAP_mm else StandardShrub_MAP_mm,
    map_mm_std_shrubs = StandardShrub_MAP_mm,
    map_mm_grasses = if (do_adjBiom_by_ppt) MAP_mm else StandardGrasses_MAP_mm,
    map_mm_std_grasses = StandardGrasses_MAP_mm,
    vegcomp_std_shrubs = StandardShrub_VegComposition,
    vegcomp_std_grass = StandardGrasses_VegComposition)

  biom_shrubs <- temp$biom_shrubs
  biom_C3 <- temp$biom_C3
  biom_C4 <- temp$biom_C4
  biom_annuals <- temp$biom_annuals

  biom_grasses <- biom_C3 * fgrass_c3c4ann[1] +
                  biom_C4 * fgrass_c3c4ann[2] +
                  biom_annuals * fgrass_c3c4ann[3]

  list(grass = as.matrix(biom_grasses),
       shrub = as.matrix(biom_shrubs))
})

#' Lookup transpiration coefficients for grasses, shrubs, and trees per soil layer or per soil depth increment of 1 cm per distribution type for each simulation run and copy values to 'datafile.soils'
#'
#' first row of datafile is label for per soil layer 'Layer' or per soil depth increment of 1 cm 'DepthCM'
#' second row of datafile is source of data
#' the other rows contain the data for each distribution type = columns
#' @section Note:
#'  cannot write data from sw_input_soils to datafile.soils
TranspCoeffByVegType <- compiler::cmpfun(function(tr_input_code, tr_input_coeff,
  soillayer_no, trco_type, layers_depth,
  adjustType = c("positive", "inverse", "allToLast")) {

  #extract data from table by category
  trco.code <- as.character(tr_input_code[, which(colnames(tr_input_code) == trco_type)])
  trco <- rep(0, times = soillayer_no)
  trco.raw <- na.omit(tr_input_coeff[, which(colnames(tr_input_coeff) == trco_type)])

  if (trco.code == "DepthCM") {
    temp <- sum(trco.raw, na.rm = TRUE)
    trco_sum <- if (temp == 0 || is.na(temp)) 1L else temp
    lup <- 1
    for(l in 1:soillayer_no){
      llow <- as.numeric(layers_depth[l])
      if(is.na(llow) | lup > length(trco.raw))
      {
        l <- l - 1
        break
      }
      trco[l] <- sum(trco.raw[lup:llow], na.rm=TRUE) / trco_sum
      lup <- llow + 1
    }
    usel <- l
  } else if(trco.code == "Layer"){
    usel <- ifelse(length(trco.raw) < soillayer_no, length(trco.raw), soillayer_no)
    trco[1:usel] <- trco.raw[1:usel] / ifelse((temp <- sum(trco.raw[1:usel], na.rm=TRUE)) == 0 & is.na(temp), 1, temp)
  }

  if(identical(adjustType, "positive")){
    trco <- trco / sum(trco)	#equivalent to: trco + (1 - sum(trco)) * trco / sum(trco)
  } else if(identical(adjustType, "inverse")){
    irows <- 1:max(which(trco > 0))
    trco[irows] <- trco[irows] + rev(trco[irows]) * (1 / sum(trco[irows]) - 1)	#equivalent to: trco + (1 - sum(trco)) * rev(trco) / sum(trco)
  } else if(identical(adjustType, "allToLast")){
    irow <- max(which(trco > 0))
    if(irow > 1){
      trco[irow] <- 1 - sum(trco[1:(irow - 1)]) 	#adding all the missing roots because soil is too shallow to the deepest available layer
    } else {
      trco[1] <- 1
    }
  }

  trco
})


check_soil_data <- compiler::cmpfun(function(data) {
    check_soil <- is.finite(data)
    check_soil[, "depth_cm"] <- check_soil[, "depth_cm"] & data[, "depth_cm"] > 0 &
      diff(c(0, data[, "depth_cm"])) > 0
    check_soil[, "matricd"] <- check_soil[, "matricd"] & data[, "matricd"] > 0.3 &
      data[, "matricd"] <= 2.65
    check_soil[, "gravel_content"] <- check_soil[, "gravel_content"] &
      data[, "gravel_content"] >= 0 & data[, "gravel_content"] < 1
    itemp <- c("sand", "clay")
    check_soil[, itemp] <- check_soil[, itemp] & data[, itemp] > 0 & data[, itemp] <= 1
    itemp <- c("EvapBareSoil_frac", "transpGrass_frac", "transpShrub_frac",
              "transpTree_frac", "transpForb_frac", "imperm")
    check_soil[, itemp] <- check_soil[, itemp] & data[, itemp] >= 0 & data[, itemp] <= 1

    check_soil
})


#Circular functions: int=number of units in circle, e.g., for days: int=365; for months: int=12
circ.mean <- compiler::cmpfun(function(x, int, na.rm = FALSE) {
  if (!all(is.na(x))) {
    circ <- 2 * pi / int
    x_circ <- circular::circular(x * circ, type = "angles", units = "radians", rotation = "clock", modulo = "2pi")
    x_int <- circular::mean.circular(x_circ, na.rm = na.rm) / circ

    round(as.numeric(x_int) - 1, 13) %% int + 1	# map 0 -> int; rounding to 13 digits: 13 was empirically derived for int={12, 365} and x=c((-1):2, seq(x-5, x+5, by=1), seq(2*x-5, 2*x+5, by=1)) assuming that this function will never need to calculate for x > t*int with t>2
  } else {
    NA
  }
})

circ.range <- compiler::cmpfun(function(x, int, na.rm = FALSE) {
  if (!all(is.na(x))) {
    circ <- 2 * pi / int
    x_circ <- circular::circular(x * circ, type = "angles", units = "radians", rotation = "clock", modulo = "2pi")
    x_int <- range(x_circ, na.rm = na.rm) / circ
    as.numeric(x_int)

  } else {
    NA
  }
})

circ.sd <- compiler::cmpfun(function(x, int, na.rm=FALSE){
  if (length(x) - sum(is.na(x)) > 1) {
    if (sd(x, na.rm = TRUE) > 0) {
      circ <- 2 * pi / int
      x_circ <- circular::circular(x * circ, type = "angles", units = "radians", rotation = "clock", modulo = "2pi")
      x_int <- circular::sd.circular(x_circ, na.rm = na.rm) / circ
      as.numeric(x_int)
    } else {
      0
    }
  } else {
    NA
  }
})


#functions wet and dry periods

#' Saturation vapor pressure
#'
#' @param T A numeric vector of temperature(s) (deg C)
#' @return A numeric vector of length \code{T} of saturation vapor pressure (kPa) at
#'    temperature T
#' @references Yoder, R. E., L. O. Odhiambo, and W. C. Wright. 2005. Effects of Vapor-Pressure Deficit and Net-Irradiance Calculation Methods on Accuracy of Standardized Penman-Monteith Equation in a Humid Climate Journal of Irrigation and Drainage Engineering 131:228-237.
vp0 <- compiler::cmpfun(function(T) {
  0.6108 * exp(17.27 * T / (T + 273.3))	# eq. 5 of Yoder et al. 2005
})


#' Vapor pressure deficit
#'
#' @param Tmin A numeric vector of daily minimum temperature(s) (deg C)
#' @param Tmax A numeric vector of daily maximum temperature(s) (deg C)
#' @param RHmean A numeric vector of daily mean relative humidity (percentage)
#' @return A numeric vector of length \code{T} of vapor pressure deficit (kPa)
#' @references Yoder, R. E., L. O. Odhiambo, and W. C. Wright. 2005. Effects of Vapor-Pressure Deficit and Net-Irradiance Calculation Methods on Accuracy of Standardized Penman-Monteith Equation in a Humid Climate Journal of Irrigation and Drainage Engineering 131:228-237.
vpd <- compiler::cmpfun(function(Tmin, Tmax, RHmean = NULL) {
  if (is.null(RHmean)) {
    (vp0(Tmax) - vp0(Tmin)) / 2	# eq. 6 - eq. 13 of Yoder et al. 2005 (VPD6 in Table 4)
  } else {
    (vp0(Tmax) + vp0(Tmin)) / 2 * (1 - RHmean / 100)	# eq. 6 - eq. 11 of Yoder et al. 2005 (VPD4 in Table 4)
  }
})


#' @param x A numeric vector
#' @param fun A function which requires one argument. \code{fun} will be applied to
#'    the k-largest values of \code{x}.
#' @param k An integer value. The k-largest value(s) of \code{x} will be used. The largest
#'    value will be used if 0 or negative.
#' @param na.rm A logical value
#' @param ... Optional arguments to be passed to \code{fun}
#'
#' @return A vector with the k-largest values of \code{x} if \code{is.null(fun)},
#'    otherwise the result of applying \code{fun} to the k-largest values.
fun_kLargest <- compiler::cmpfun(function(x, fun = NULL, k = 10L, na.rm = FALSE, ...) {
  if (na.rm)
    x <- na.exclude(x)
  x <- sort.int(x, decreasing = TRUE, na.last = !na.rm, method = if (getRversion() >= "3.3.0") "radix" else "quick")
  x <- x[seq_len(max(1L, min(length(x), as.integer(k))))]

  if (is.null(fun)) x else fun(x, ...)
})

max.duration <- compiler::cmpfun(function(x, target_val = 1L, return_doys = FALSE) {
  r <- rle(x)
  rgood <- r$values == target_val
  igood <- which(rgood)

  if (length(igood) > 0) {
    len <- max(r$lengths[igood])

    if (return_doys) {
      imax <- which(rgood & r$lengths == len)[1]

      rdoys <- cumsum(r$lengths)
      doys <- if (imax == 1L) {
          c(start = 1L, end = rdoys[1])
        } else {
          c(start = rdoys[imax - 1] + 1,
            end = rdoys[imax])
        }
    }

  } else {
    len <- 0L
    doys <- c(start = NA, end = NA)
  }

  if (return_doys)
    return(c(len, doys))

  len
})

startDoyOfDuration <- compiler::cmpfun(function(x, duration=10) {
  r <- rle(x)
  if(length(r$lengths)==1 | sum(r$values==1 & r$lengths>=duration)==0 ){
    return (ifelse((length(r$lengths)==1 & (r$values==0 | r$lengths<duration)) | sum(r$values==1 & r$lengths>=10)==0, NA, 1)[1])
  } else {
    first10dry <- r$lengths[which(r$values==1 & r$lengths>=duration)][1] #pick first period
    if( !is.na(first10dry) ){
      ind <- which(r$lengths==first10dry & r$values==1)[1] #always pick start of first suitable period
    } else {
      ind <- -1
    }
    if(ind==1) {#start of period at beginning of year
      return(1)
    } else if(ind==-1) {#no period this year
      return(NA)
    } else {
      return(cumsum(r$lengths)[ind-1]+1)
    }
  }
})

endDoyAfterDuration <- compiler::cmpfun(function(x, duration=10) {
  r <- rle(x)
  if(length(r$lengths)==1 | sum(r$values==1 & r$lengths>=duration)==0 ){
    return (ifelse((length(r$lengths)==1 & (r$values==0 | r$lengths<duration)) | sum(r$values==1 & r$lengths>=duration)==0, 365, NA)[1])
  } else {
    last10dry <- (rl <- r$lengths[which(r$values==1 & r$lengths>=duration)])[length(rl)] #pick last period
    if( length(last10dry) > 0 ){
      ind <- (temp <- which(r$lengths==last10dry & r$values==1))[length(temp)]	#always pick end of last suitable period
    } else {
      ind <- -1
    }
    if(ind==-1) {#no period this year
      return(NA)
    } else {
      return(cumsum(r$lengths)[ind])
    }
  }
})


#' Calculates temperate dryland criteria
#'
#' @param annualPPT A numeric vector. Annual precipitation values.
#' @param annualPET A numeric vector. Annual potential evapotranspiration values.
#'  The values must be in the same units as those of \code{annualPPT}, e.g., \code{mm}.
#' @monthlyTemp A numeric vector. Monthly mean air temperature in degree Celsius for each
#'  year for which precipitation and PET values are provided.
#' @param ai_limit A numeric value. Used for return item \code{criteria_12}.
#'
#' @references
#' Deichmann, U. & L. Eklundh. 1991. Global digital datasets for land degradation studies: a GIS approach. Global Environment Monitoring System (GEMS), United Nations Environment Programme (UNEP), Nairobi, Kenya.
#' Trewartha GT, Horn LH (1980) An introduction to climate. McGraw-Hill, New York, page 284: Temperate Areas
#'
#' @return
#'  A list with three items: UN-aridity index (numeric value), temperateness (logical value),
#'  and temperate drylands (logical value).
calc_drylandindices <- compiler::cmpfun(function(annualPPT, annualPET, monthlyTemp, ai_limit = 0.5) {
  ai <- annualPPT / annualPET	#Deichmann, U. & L. Eklundh. 1991. Global digital datasets for land degradation studies: a GIS approach. Global Environment Monitoring System (GEMS), United Nations Environment Programme (UNEP), Nairobi, Kenya.
  temp <- matrix(monthlyTemp >= 10, nrow = 12)
  temp <- .colSums(temp, nrow(temp), ncol(temp))
  TD <- temp >= 4 & temp < 8 #Trewartha & Horn 1980, page 284: temperate areas
  criteria12 <- as.integer(TD & ai < ai_limit)

  list(ai = ai, temperateness = TD, criteria12 = criteria12)
})


extreme_values_and_doys <- compiler::cmpfun(function(x, tol = sqrt(.Machine$double.eps), na.rm = FALSE) {
  tmax <- max(x, na.rm = na.rm)
  tmin <- min(x, na.rm = na.rm)

  c(tmax, tmin,
    circ.mean(which(abs(x - tmax) < tol), int = 365, na.rm = na.rm),
    circ.mean(which(abs(x - tmin) < tol), int = 365, na.rm = na.rm))
})


handle_NAs <- compiler::cmpfun(function(x, na.index, na.act) {
  if (length(na.index) > 0) {
    napredict(na.act, x)
  } else {
    x
  }
})

scale_by_sum <- compiler::cmpfun(function(x) {
  temp <- sum(x, na.rm = TRUE)
  if (temp > 0 && is.finite(temp)) {
    x / temp
  } else {
    x
  }
})


#' Pedotransfer functions to convert between soil moisture (volumetric water content, VWC)
#'  and soil water potential (SWP)
#'
#' @param sand A numeric value or vector. Sand content of the soil layer(s) (fraction 0-1).
#' @param clay A numeric value or vector. Clay content of the soil layer(s) (fraction 0-1).
#'
#' @references
#'  Cosby, B. J., G. M. Hornberger, R. B. Clapp, and T. R. Ginn. 1984. A statistical exploration of the relationships of soil moisture characteristics to the physical properties of soils. Water Resources Research 20:682-690.
#'
#' @name pedotransfer
NULL

#' @rdname pedotransfer
#' @section Note:
#'  either swp or sand/clay needs be a single value
pdf_to_vwc <- compiler::cmpfun(function(swp, sand, clay, thetas, psis, b, MPa_toBar = -10, bar_conversion = 1024) {
  thetas * (psis / (swp * MPa_toBar * bar_conversion)) ^ (1 / b) / 100
})

#' @rdname pedotransfer
#' @section Note:
#'  either vwc or sand/clay needs be a single value
pdf_to_swp <- compiler::cmpfun(function(vwc, sand, clay, thetas, psis, b, bar_toMPa = -0.1, bar_conversion = 1024) {
  psis / ((vwc * 100 / thetas) ^ b * bar_conversion) * bar_toMPa
})

pedotransfer <- compiler::cmpfun(function(x, sand, clay, pdf) {
  stopifnot(length(sand) && length(sand) == length(clay))
  sand <- finite01(sand, NA, NA)
  clay <- finite01(clay, NA, NA)

  if (any(complete.cases(sand, clay))) {
    thetas <- -14.2 * sand - 3.7 * clay + 50.5
    psis <- 10 ^ (-1.58 * sand - 0.63 * clay + 2.17)
    b <- -0.3 * sand + 15.7 * clay + 3.10
    if (any(b <= 0, na.rm = TRUE))
      stop("Pedotransfer for soil texture with b <= 0 is not possible.")

    np_x <- NROW(x) * NCOL(x) # NROW(x) * NCOL(x) != prod(dim(x)) != length(x), e.g., for a data.frame with one column

    if (NROW(x) == 1 || NCOL(x) == 1) {
      # cases 1-4
      if (np_x == 1 || length(sand) == 1) {
        # cases 1-3
        res <- pdf(x, sand, clay, thetas, psis, b)

      } else {
        # case 4; Note: case 3 could also be calculated with the code for case 4, but is much slower, unless x is a data.frame with one column
        temp <- lapply(x, function(v) pdf(v, sand, clay, thetas, psis, b))
        res <- matrix(unlist(temp), nrow = np_x, byrow = TRUE)
      }

    } else {
      # cases 5-6
      dx <- dim(x)

      if (length(sand) == 1) {
        # case 5
        res <- vapply(seq_len(dx[2]), function(d) {
          temp <- pdf(x[, d], sand, clay, thetas, psis, b)
        }, rep(1, dx[1]), USE.NAMES = FALSE)

      } else {
        # case 6
        stopifnot(dx[2] == length(sand))
        res <- vapply(seq_len(dx[2]), function(d) {
            pdf(x[, d], sand[d], clay[d], thetas[d], psis[d], b[d])
         }, rep(1, dx[1]), USE.NAMES = FALSE)
      }
    }

  } else {
    res <- x
    res[] <- NA
  }

  res #if SWP then in units of MPa [-Inf, 0]; if VWC then in units of m3/m3 [0, 1]
})

#' @rdname pedotransfer
#' @param swp A numeric value, vector, or 2-dimensional object (matrix or data.frame).
#'  The soil water potential (of the soil matrix) in units of MPa, i.e.,
#'  the soil without the volume of rock and gravel.
#'
#' @return Volumetric water content in units of m^3 (of water) / m^3 (of soil) [0, 1].
#'  There are six use cases:\enumarate{
#'    \item 1) \itemize{
#'                \item Input: SWP [single value]; sand and clay [single values]
#'                \item Output: VWC [single value]}
#'    \item 2) \itemize{
#'                \item Input: SWP [single value]; sand and clay [vectors of length d]
#'                \item Output: VWC [vector of length d]}
#'    \item 3) \itemize{
#'                \item Input: SWP [vector of length l]; sand and clay in fraction [single values]
#'                \item Output: VWC [vector of length l]}
#'    \item 4) \itemize{
#'                \item Input: SWP [vector of length l]; sand and clay [vectors of length d]
#'                \item Output: VWC [l x d matrix] where SWP is repeated for each column}
#'    \item 5) \itemize{
#'                \item Input: SWP [l x d matrix]; sand and clay [single values]
#'                \item Output: VWC [l x d matrix]}
#'    \item 6) \itemize{
#'                \item Input: SWP [l x d matrix]; sand and clay [vectors of length d]
#'                \item Output: VWC [l x d matrix], sand and clay vectors are repeated for each row}
#'  }
SWPtoVWC <- compiler::cmpfun(function(swp, sand, clay) {
  pedotransfer(swp, sand, clay, pdf = pdf_to_vwc)
})


#' @rdname pedotransfer
#' @param vwc A numeric value, vector, or 2-dimensional object (matrix or data.frame).
#'  The matric soil moisture, i.e., reduced by the volume of rock and gravel.
#'
#' @return Soil water potential in units of MPa [-Inf, 0]. There are six use cases:
#'  \enumarate{
#'    \item 1) \itemize{
#'                \item Input: VWC [single value]; sand and clay [single values]
#'                \item Output: SWP [single value]}
#'    \item 2) \itemize{
#'                \item Input: VWC [single value]; sand and clay [vectors of length d]
#'                \item Output: SWP [vector of length d]}
#'    \item 3) \itemize{
#'                \item Input: VWC [vector of length l]; sand and clay in fraction [single values]
#'                \item Output: SWP [vector of length l]}
#'    \item 4) \itemize{
#'                \item Input: VWC [vector of length l]; sand and clay [vectors of length d]
#'                \item Output: SWP [l x d matrix] where VWC is repeated for each column}
#'    \item 5) \itemize{
#'                \item Input: VWC [l x d matrix]; sand and clay [single values]
#'                \item Output: SWP [l x d matrix]}
#'    \item 6) \itemize{
#'                \item Input: VWC [l x d matrix]; sand and clay [vectors of length d]
#'                \item Output: SWP [l x d matrix], sand and clay vectors are repeated for each row}
#'  }
VWCtoSWP <- compiler::cmpfun(function(vwc, sand, clay) {
  pedotransfer(vwc, sand, clay, pdf = pdf_to_swp)
})


#two, three, or four layer aggregation for average daily aggregation output
setAggSoilLayerForAggDailyResponses <- compiler::cmpfun(function(layers_depth, daily_lyr_agg){
  d <- length(layers_depth)
  vals <- list()
  #first layer
  DeepestFirstDailyAggLayer <- findInterval(daily_lyr_agg[["first_cm"]], c(0, layers_depth) + tol, all.inside=TRUE)
  vals[[1]] <- seq_len(DeepestFirstDailyAggLayer)
  #second layer
  if(!is.null(daily_lyr_agg[["second_cm"]])){
    DeepestSecondDailyAggLayer <- findInterval(daily_lyr_agg[["second_cm"]], c(0, layers_depth) + tol, all.inside=TRUE)
  } else {
    DeepestSecondDailyAggLayer <- d
  }
  if(is.numeric(DeepestSecondDailyAggLayer) && is.numeric(DeepestFirstDailyAggLayer) && d > DeepestFirstDailyAggLayer){
    vals[[2]] <- (DeepestFirstDailyAggLayer+1):DeepestSecondDailyAggLayer
  }
  #third layer
  if(!is.null(daily_lyr_agg[["third_cm"]])){
    if(!is.na(daily_lyr_agg[["third_cm"]])){
      DeepestThirdDailyAggLayer <- findInterval(daily_lyr_agg[["third_cm"]], c(0, layers_depth) + tol, all.inside=TRUE)
    } else {
      DeepestThirdDailyAggLayer <- NULL
    }
  } else {
    DeepestThirdDailyAggLayer <- d
  }
  if(is.numeric(DeepestThirdDailyAggLayer) && is.numeric(DeepestSecondDailyAggLayer) && d > DeepestSecondDailyAggLayer){
    vals[[3]] <- (DeepestSecondDailyAggLayer+1):DeepestThirdDailyAggLayer
  }
  #fourth layer
  if(!is.null(daily_lyr_agg[["fourth_cm"]])){
    if(!is.na(daily_lyr_agg[["fourth_cm"]])){
      DeepestFourthDailyAggLayer <- findInterval(daily_lyr_agg[["fourth_cm"]], c(0, layers_depth) + tol, all.inside=TRUE)
    } else {
      DeepestFourthDailyAggLayer <- NULL
    }
  } else {
    DeepestFourthDailyAggLayer <- d
  }
  if(is.numeric(DeepestFourthDailyAggLayer) && is.numeric(DeepestThirdDailyAggLayer) && d > DeepestThirdDailyAggLayer){
    vals[[4]] <- ((DeepestThirdDailyAggLayer+1):DeepestFourthDailyAggLayer)
  }

  return(vals)
})


#function to extrapolate windspeeds measured at heights different than SoilWat required 2-m above ground
adjust.WindspeedHeight <- compiler::cmpfun(function(uz, height){
  # Allen RG, Walter IA, Elliott R, Howell T, Itenfisu D, Jensen M (2005) In The ASCE standardized reference evapotranspiration equation, pp. 59. ASCE-EWRI Task Committee Report.
  # input: windspeed [m/s] at height x
  # output: windspeed [m/s] at height 2 m

  stopifnot(all(uz >= 0) && height >= 2 )
  return( uz * 4.87 / log(67.8 * height - 5.42) )	# eqn. 33 in Allen et al. (2005)
})


get.LookupFromTable <- compiler::cmpfun(function(pattern, trtype, tr_input, sw_input_use, sw_input, nvars) {
  nruns <- NROW(sw_input)
  if (length(trtype) == 1L && nruns > 1L)
    trtype <- rep(trtype, nruns)
  stopifnot(length(trtype) == nruns)

  # extract data from table by type
  ids <- match(trtype, rownames(tr_input), nomatch = NA)
  res <- tr_input[ids, seq_len(nvars), drop = FALSE]

  # identify columns with relevant data
  icols_in <- grep(pattern, names(sw_input_use))
  icols_res <- which(apply(!is.na(res), 2L, any))
  stopifnot(length(icols_in) >= max(icols_res))
  seq1_icols_in <- icols_in[icols_res]
  seq2_icols_in <- icols_in[-icols_res]

  # add data to datafiles and set the use flags
  sw_input[, seq1_icols_in] <- res[, icols_res]
  sw_input_use[seq1_icols_in] <- TRUE

  if (length(seq2_icols_in) > 0) {
    sw_input[, seq2_icols_in] <- NA
    sw_input_use[seq2_icols_in] <- FALSE
  }

  list(sw_input_use = sw_input_use,
       sw_input = sw_input)
})

fill_empty <- compiler::cmpfun(function(data, pattern, fill, tol = tol) {
  stopifnot(names(data) %in% c("sw_input", "sw_input_use"))

  icols <- sapply(data, function(x) grep(pattern, names(x)))
  stopifnot(identical(icols[, "sw_input_use"], icols[, "sw_input"]))
  icols <- icols[, "sw_input_use"]

  for (k in icols) {
    iempty <- is.na(data$sw_input[, k])
    if (any(iempty)) {
      data$sw_input[iempty, k] <- fill
      data$sw_input_use[k] <- TRUE
    }
  }

  data
})

#' Split soil layer in two layers
#'
#' @details The method \code{interpolate} calculates the weighted mean of the columns/layers
#'  \code{il} and \code{il + 1}.
#'  The method \code{exhaust} distributes the value of \code{il + 1} according to the weights.
#'
#' @param x A numeric data.frame or matrix. Columns are soil layers.
#' @param il An integer value. The column/soil layer number after which a new layer is added.
#' @param w A numeric vector of length two. The weights used to calculate the values of the new layer.
#' @param method A character string. \code{See details.}
#'
#' @return An object like x with one column more at position \code{il + 1}.
add_layer_to_soil <- compiler::cmpfun(function(x, il, w, method = c("interpolate", "exhaust")) {
  method <- match.arg(method)
  if (!is.matrix(x))
    x <- as.matrix(x)
  ncols <- dim(x)[2]

  if (ncols > il) {
    x <- x[, c(seq_len(il), NA, (il + 1):ncols)]

    if (method == "interpolate") {
      x[, il + 1] <- if (il > 0) {
        (x[, il] * w[1] + x[, il + 2] * w[2]) / sum(w)
      } else {
        x[, il + 2]
      }

    } else if (method == "exhaust") {
      x[, il + 1] <- x[, il + 2] * w[1] / sum(w)
      x[, il + 2] <- x[, il + 2] * w[2] / sum(w)
    }

  } else if (ncols == il) {
    x <- x[, c(seq_len(ncols), NA)]

    if (method == "interpolate") {
      x[, il + 1] <- x[, il]

    } else if (method == "exhaust") {
      x[, il + 1] <- x[, il] * w[2] / sum(w)
      x[, il] <- x[, il] * w[1] / sum(w)
    }

  } else {
    stop("Object x has ", ncols, " columns; thus, a new ", il, "-th column cannot be created")
  }

  x
})

identify_soillayers <- compiler::cmpfun(function(depths, sdepth) {
  it <- findInterval(depths, sdepth)
  if (any(is.na(it))) {
    as.integer(na.exclude(it))
  } else if (length(it) > 1 && diff(it) > 0) {
    (1 + it[1]):(it[2])
  } else {
    it[1]
  }
})

adjustLayer_byImp <- compiler::cmpfun(function(depths, imp_depth, sdepths) {
  if (any(imp_depth < depths[1])) {
    depths <- imp_depth
    if (length(sdepths) >= 2) {
      temp <- findInterval(imp_depth, sdepths)
      if (temp > 1) {
        depths <- c(sdepths[temp - 1], imp_depth)
      } else {
        depths <- c(imp_depth, sdepths[temp + 1])
      }
    }
  } else if(any(imp_depth < depths[2])){
    depths <- c(depths[1], imp_depth)
  }

  depths
})

EstimateInitialSoilTemperatureForEachSoilLayer <- compiler::cmpfun(function(layers_depth, lower.Tdepth, soilTupper, soilTlower){
  sl <- c(0, lower.Tdepth)
  st <- c(soilTupper, soilTlower)
  return( predict(lm(st ~ sl), data.frame(sl=layers_depth)) )
})

#--put information from experimental design into appropriate input variables; create_treatments and the _use files were already adjusted for the experimental design when files were read in/created
transferExpDesignToInput <- compiler::cmpfun(function(x, i_exp, df_exp, df_exp_use) {
  temp <- match(names(df_exp)[df_exp_use], names(x), nomatch = 0)
  ctemp <- temp[!(temp == 0)]
  if (length(ctemp) > 0) {
    cexp <- match(names(x)[ctemp], names(df_exp), nomatch = 0)
    x[ctemp] <- df_exp[i_exp, cexp]
  }
  x
})

setDeepestTopLayer <- compiler::cmpfun(function(layers_depth, Depth_TopLayers) {
  max(1, findInterval(Depth_TopLayers, layers_depth))
})

setTopLayer <- compiler::cmpfun(function(d, DeepestTopLayer) {
  seq_len(if(d < DeepestTopLayer) d else DeepestTopLayer)
})

setBottomLayer <- compiler::cmpfun(function(d, DeepestTopLayer) {
  if (d <= DeepestTopLayer) {
    NULL
  } else {
    (DeepestTopLayer + 1L):d
  }
})

local_weatherDirName <- compiler::cmpfun(function(i_sim, scN, runN, runIDs, name.OutputDB) {	# Get name of weather file from output database
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = name.OutputDB)
  temp <- DBI::dbGetQuery(con, paste("SELECT WeatherFolder FROM header WHERE P_id=", it_Pid(i_sim, 1, scN, runN, runIDs)))[1,1]
  DBI::dbDisconnect(con)
  temp
})

tempError <- compiler::cmpfun(function() .Call("tempError"))

#data access functions
get_Response_aggL <- compiler::cmpfun(function(sc_i, response,
                              tscale = c("dy", "dyAll", "mo", "moAll", "yr", "yrAll"),
                              scaler = 10, FUN, weights = NULL,
                              x, st, st2, topL, bottomL) {
  FUN <- match.fun(FUN)
  tscale <- match.arg(tscale)
  responseRepeats <- if (response %in% c("TRANSP", "HYDRED")) {
      # divide by 5, because: each soil layer (cm): total, trees, shrubs, forbs, grasses
      5L
    } else {
      # c(sw_vwc, sw_evsoil, sw_soiltemp, sw_swc, sw_swa)
      1L
    }

  temp1 <- scaler * slot(slot(x[[sc_i]], response),
    switch(tscale,
      dy = "Day", dyAll = "Day",
      mo = "Month", moAll = "Month",
      yr = "Year", yrAll = "Year"))

  if (inherits(temp1, "try-error"))
    stop("Necessary SoilWat output files are not present for aggregation of results")

  if (tscale == "dy") {
    index.col <- 2
    index.usetimestep <- st$index.usedy
    timestep_ForEachEntry <- st2$doy_ForEachUsedDay
  } else if (tscale == "dyAll") {
    index.col <- 2
    index.usetimestep <- seq_len(nrow(temp1))
    timestep_ForEachEntry <- NULL
  } else if (tscale == "mo") {
    index.col <- 2
    index.usetimestep <- st$index.usemo
    timestep_ForEachEntry <- st2$month_ForEachUsedMonth
  } else if (tscale == "moAll") {
    index.col <- 2
    index.usetimestep <- seq_len(nrow(temp1))
    timestep_ForEachEntry <- NULL
  } else if (tscale == "yr") {
    index.col <- 1
    index.usetimestep <- st$index.useyr
    timestep_ForEachEntry <- NULL
  } else if (tscale == "yrAll") {
    index.col <- 1
    index.usetimestep <- seq_len(nrow(temp1))
    timestep_ForEachEntry <- NULL
  }

  layers <- seq_len((ncol(temp1) - index.col) / responseRepeats)

  #adjust topL and bottomL locally in case temp1 doesn't contain information for every layer, e.g., soil evaporation
  if (max(layers) <= max(topL)) {
    topL <- layers
    bottomL <- 0
  } else if (max(layers) < max(bottomL)) {
    bottomL <- min(bottomL):max(layers)
  }

  res <- list()
  res[["top"]] <- if (length(topL) > 1) {
      if (is.null(weights)) {
        apply(temp1[index.usetimestep, index.col + topL, drop = FALSE], 1, FUN)
      } else {
        apply(temp1[index.usetimestep, index.col + topL, drop = FALSE], 1, FUN, weights[topL])
      }
    } else {
      temp1[index.usetimestep, index.col + topL]
    }
  res[["bottom"]] <- if (length(bottomL) > 1) {
      if(is.null(weights)){
        apply(temp1[index.usetimestep, index.col + bottomL, drop = FALSE], 1, FUN)
      } else {
        apply(temp1[index.usetimestep, index.col + bottomL, drop = FALSE], 1, FUN, weights[bottomL])
      }
    } else if (is.null(bottomL) || identical(bottomL, 0)) {
      matrix(data = 0, nrow = length(index.usetimestep), ncol = 1)
    } else {
      temp1[index.usetimestep, index.col + bottomL]
    }

  if (!is.null(timestep_ForEachEntry)) {
    res[["aggMean.top"]] <- tapply(res[["top"]], timestep_ForEachEntry, mean)
    res[["aggMean.bottom"]] <- tapply(res[["bottom"]], timestep_ForEachEntry, mean)
  }

  if (tscale == "dyAll" || tscale == "moAll" || tscale == "yrAll") {
     res[["val"]] <- temp1
  }

  res
})

get_SWPmatric_aggL <- compiler::cmpfun(function(vwcmatric, texture, sand, clay) {
  res <- list()

  res[["top"]] <- VWCtoSWP(vwcmatric$top, texture$sand.top, texture$clay.top)
  res[["bottom"]] <- VWCtoSWP(vwcmatric$bottom, texture$sand.bottom, texture$clay.bottom)

  if (!is.null(vwcmatric$aggMean.top)) {
    res[["aggMean.top"]] <- VWCtoSWP(vwcmatric$aggMean.top, texture$sand.top, texture$clay.top)
    res[["aggMean.bottom"]] <- VWCtoSWP(vwcmatric$aggMean.bottom, texture$sand.bottom, texture$clay.bottom)
  }

  if (!is.null(vwcmatric$val)) {
    if (all(as.integer(vwcmatric$val[, 2]) == vwcmatric$val[, 2])) {
      index.header <- 1:2
    } else {
      index.header <- 1
    }
    res[["val"]] <- cbind(vwcmatric$val[, index.header], VWCtoSWP(vwcmatric$val[, -index.header], sand, clay))
  }

  res
})

get_Temp_yr <- compiler::cmpfun(function(sc, x, st) {
  list(mean = slot(slot(x[[sc]], "TEMP"), "Year")[st$index.useyr, 4])
})

get_Temp_mo <- compiler::cmpfun(function(sc, x, st) {
  x <- slot(slot(x[[sc]], "TEMP"), "Month")[st$index.usemo, ]
  list(min =  x[, 4],
       mean = x[, 5],
       max =  x[, 3])
})

get_Temp_dy <- compiler::cmpfun(function(sc, x, st) {
  x <- slot(slot(x[[sc]], "TEMP"), "Day")[st$index.usedy, ]
  list(min =  x[, 4],
       mean = x[, 5],
       max =  x[, 3])
})

get_VPD_mo <- compiler::cmpfun(function(sc, temp.mo, xin, st2) {
  rH <- Rsoilwat31::swCloud_SkyCover(xin[[sc]])
  rH <- as.vector(rH[st2$month_ForEachUsedMonth])

  list(mean = vpd(temp.mo$min, temp.mo$max, rH))
})

get_VPD_dy <- compiler::cmpfun(function(sc, temp.dy, xin, st2) {
  rH <- Rsoilwat31::swCloud_SkyCover(xin[[sc]])
  rH <- as.vector(rH[st2$month_ForEachUsedDay])

  list(mean = vpd(temp.dy$min, temp.dy$max, rH))
})

get_PPT_yr <- compiler::cmpfun(function(sc, x, st) {
  x <- 10 * slot(slot(x[[sc]], "PRECIP"), "Year")[st$index.useyr, ]
  list(ppt = x[, 2], rain = x[, 3],
       snowfall = x[, 4], snowmelt = x[, 5], snowloss = x[, 6])
})

get_PPT_mo <- compiler::cmpfun(function(sc, x, st) {
  x <- 10 * slot(slot(x[[sc]], "PRECIP"), "Month")[st$index.usemo, ]
  list(ppt = x[, 3], rain = x[, 4],
       snowmelt = x[, 6])
})

get_PPT_dy <- compiler::cmpfun(function(sc, x, st) {
  x <- 10 * slot(slot(x[[sc]], "PRECIP"), "Day")[st$index.usedy, ]
  list(ppt = x[, 3], rain = x[, 4],
       snowfall = x[, 5], snowmelt = x[, 6], snowloss = x[, 7])
})

get_PET_yr <- compiler::cmpfun(function(sc, x, st) {
  list(val = 10 * slot(slot(x[[sc]], "PET"), "Year")[st$index.useyr, 2])
})

get_PET_mo <- compiler::cmpfun(function(sc, x, st) {
  list(val = 10 * slot(slot(x[[sc]], "PET"), "Month")[st$index.usemo, 3])
})

get_AET_yr <- compiler::cmpfun(function(sc, x, st) {
  list(val = 10 * slot(slot(x[[sc]], "AET"), "Year")[st$index.useyr, 2])
})

get_AET_mo <- compiler::cmpfun(function(sc, x, st) {
  list(val = 10 * slot(slot(x[[sc]], "AET"), "Month")[st$index.usemo, 3])
})

get_AET_dy <- compiler::cmpfun(function(sc, x, st) {
  list(val = 10 * slot(slot(x[[sc]], "AET"), "Day")[st$index.usedy, 3])
})

get_SWE_mo <- compiler::cmpfun(function(sc, x, st) {
  list(val = 10 * slot(slot(x[[sc]], "SNOWPACK"), "Month")[st$index.usemo, 3])
})

get_SWE_dy <- compiler::cmpfun(function(sc, x, st) {
  list(val = 10 * slot(slot(x[[sc]], "SNOWPACK"), "Day")[st$index.usedy, 3])
})

get_Inf_yr <- compiler::cmpfun(function(sc, x, st) {
  list(inf = 10 * slot(slot(x[[sc]], "SOILINFILT"), "Year")[st$index.useyr, 2])
})

get_Inf_mo <- compiler::cmpfun(function(sc, x, st) {
  list(inf = 10 * slot(slot(x[[sc]], "SOILINFILT"), "Month")[st$index.usemo, 3])
})

get_Inf_dy <- compiler::cmpfun(function(sc, x, st) {
  list(inf = 10 * slot(slot(x[[sc]], "SOILINFILT"), "Day")[st$index.usedy, 3])
})

get_Esurface_yr <- compiler::cmpfun(function(sc, x, st) {
  x <- 10 * slot(slot(x[[sc]], "EVAPSURFACE"), "Year")[st$index.useyr, ]
  list(sum = x[, 2],
       veg = rowSums(x[, 3:6]),
       litter = x[, 7],
       surfacewater = x[, 8])
})

get_Esurface_dy <- compiler::cmpfun(function(sc, x, st) {
  x <- 10 * slot(slot(x[[sc]], "EVAPSURFACE"), "Day")[st$index.usedy, ]
  list(sum = x[, 3],
       veg = rowSums(x[, 4:7]),
       litter = x[, 8],
       surfacewater = x[, 9])
})

get_Interception_yr <- compiler::cmpfun(function(sc, x, st) {
  x <- 10 * slot(slot(x[[sc]], "INTERCEPTION"), "Year")[st$index.useyr, ]
  list(sum = x[, 2],
       veg = rowSums(x[, 3:6]),
       litter = x[, 7])
})

get_DeepDrain_yr <- compiler::cmpfun(function(sc, x, st) {
  list(val = 10 * slot(slot(x[[sc]], "DEEPSWC"), "Year")[st$index.useyr, 2])
})

get_DeepDrain_mo <- compiler::cmpfun(function(sc, x, st) {
  list(val = 10 * slot(slot(x[[sc]], "DEEPSWC"), "Month")[st$index.usemo, 3])
})

get_DeepDrain_dy <- compiler::cmpfun(function(sc, x, st) {
  list(val = 10 * slot(slot(x[[sc]], "DEEPSWC"), "Day")[st$index.usedy, 3])
})

get_Runoff_mo <- compiler::cmpfun(function(sc, x, st) {
  x <- 10 * slot(slot(x[[sc]], "RUNOFF"), "Month")[st$index.usemo, ]
  list(val = x[, 3],
       ponded = x[, 4],
       snowmelt = x[, 5])
})

get_Runoff_yr <- compiler::cmpfun(function(sc, x, st) {
  x <- 10 * slot(slot(x[[sc]], "RUNOFF"), "Year")[st$index.useyr, ]
  list(val = x[, 2],
       ponded = x[, 3],
       snowmelt = x[, 4])
})

cor2  <- compiler::cmpfun(function(y) {
	res <- try(cor(y[, 1], y[, 2]), silent = TRUE)
	if (inherits(res, "try-error")) NA else res
})

#data is the values for one year adj for SWPcrit_MPa; TRUE==dry
EventDistribution <- compiler::cmpfun(function(data, N, size) {
  bins <- rep(0, times = N)
  temp <- rle(data)
  temp <- temp$lengths[temp$values]
  if (length(temp) > 0) for (z in seq_along(temp)) {
    ix <- findInterval(temp[z], size)
    bins[ix] <- bins[ix] + 1
  }
  bins
})

daily_spells_permonth <- compiler::cmpfun(function(x, simTime2) {
  temp <- tapply(x,
    simTime2$month_ForEachUsedDay_NSadj + 100 * simTime2$year_ForEachUsedDay_NSadj,
    function(xm) {
      temp <- rle(xm)
      if (any(temp$values)) {
        mean(temp$lengths[temp$values], na.rm = TRUE)
      } else {
        NA
      }
    })

  matrix(temp, nrow = 12)
})

tabulate_values_in_bins <- compiler::cmpfun(function(x, method = c("duration", "values"),
  vcrit = NULL, bins, nbins, simTime, simTime2) {
  method <- match.arg(method)

  bins.summary <- (seq_len(nbins) - 1) * bins

  dat <- if (method == "duration" && is.logical(x)) {
      # bin duration of TRUE runs
      lapply(simTime$useyrs, function(y) {
        temp <- rle(x[simTime2$year_ForEachUsedDay == y])
        temp <- floor((temp$lengths[temp$values] - 1) / bins) * bins
        findInterval(temp, vec = bins.summary)
      })
    } else if (method == "values") {
      # bin values
      lapply(simTime$useyrs, function(y) {
        temp <- x[simTime2$year_ForEachUsedDay == y]
        if (!is.null(vcrit)) temp <- temp[temp > vcrit]
        floor(temp / bins) * bins
        findInterval(temp, vec = bins.summary)
      })
    } else {
      print("'tabulate_values_in_bins' cannot be calculated")
      NULL
    }

  if (length(unlist(dat)) > 0) {
    counts.summary <- sapply(dat, function(x)
      tabulate(x, nbins = length(bins.summary)))
    eventsPerYear <- apply(counts.summary, 2, sum)
    freq.summary <- sweep(counts.summary, 2, STATS = eventsPerYear, FUN = "/")
    rm(counts.summary)

  } else {
    freq.summary <- matrix(0, nrow = length(bins.summary), ncol = simTime$no.useyr)
    eventsPerYear <- rep(0, simTime$no.useyr)
  }

  list(eventsPerYear = eventsPerYear, freq.summary = freq.summary)
})


#------------------------DAILY WEATHER
#TODO replace with Rsoilwat31::getWeatherData_folders
ExtractLookupWeatherFolder <- compiler::cmpfun(function(dir.weather, weatherfoldername) {
  WeatherFolder <- file.path(dir.weather, weatherfoldername)
  weath <- list.files(WeatherFolder, pattern = "weath.")
  years <- as.numeric(sub(pattern = "weath.", replacement = "", weath))
  stopifnot(!anyNA(years))

  weatherData <- list()
  for (j in seq_along(weath)) {
    data_sw <- as.matrix(read.table(file.path(WeatherFolder, weath[j]), header = FALSE, comment.char = "#", blank.lines.skip = TRUE, sep = "\t"))
    data_sw[, -1] <- round(data_sw[, -1], 2) #weather.digits
    colnames(data_sw) <- c("DOY", "Tmax_C", "Tmin_C", "PPT_cm")
    weatherData[[j]] <- new("swWeatherData",
                            year = years[j],
                            data = data.matrix(data_sw, rownames.force = FALSE))
  }

  names(weatherData) <- years
  weatherData
})

#' @return A list of which each element represents one year of daily weather data of class \linkS4class{swWeatherData}.
#' Units are [degree Celsius] for temperature and [cm / day] and for precipitation.
ExtractGriddedDailyWeatherFromMaurer2002_NorthAmerica <- compiler::cmpfun(function(dir_data, cellname, startYear = simstartyr, endYear = endyr) {
  #read data from Maurer et al. 2002
  weath.data <- try(read.table(file=file.path(dir_data, cellname), comment.char=""), silent=TRUE)
  weathDataList <- list()

  if(!inherits(weath.data, "try-error")){
    colnames(weath.data) <- c("year", "month", "day", "prcp_mm", "Tmax_C", "Tmin_C", "Wind_mPERs")

    #times
    doy <- 1 + as.POSIXlt(seq(from = with(weath.data[1, ], ISOdate(year, month, day, tz = "UTC")),
        to = with(weath.data[nrow(weath.data), ], ISOdate(year, month, day, tz = "UTC")),
        by = "1 day"))$yday

    # conversion precipitation: mm/day -> cm/day
    data_all <- with(weath.data, data.frame(
      DOY = doy, Tmax_C = Tmax_C, Tmin_C = Tmin_C, PPT_cm = prcp_mm / 10))

    years <- startYear:endYear
    n_years <- length(years)
    if(!all(years %in% unique(weath.data$year)))
      stop("simstartyr or endyr out of weather data range")
    for(y in seq_along(years)) {
      data_sw <- data_all[weath.data$year == years[y], ]
      data_sw[, -1] <- round(data_sw[, -1], 2) #weather.digits
      weathDataList[[y]] <- new("swWeatherData",
                              year = years[y],
                              data = data.matrix(data_sw, rownames.force = FALSE)) #strip row.names, otherwise they consume about 60% of file size
    }
    names(weathDataList) <- as.character(years)
    weath.data <- weathDataList
  }

  weathDataList
})


get_DayMet_cellID <- compiler::cmpfun(function(coords_WGS84) {
  # Determine 1-km cell that contains requested location
  res_DayMet <- 1000L

  proj_LCC <- sp::CRS("+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  proj_WGS84 <- sp::CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

  xy_LCC <- sp::coordinates(sp::spTransform(sp::SpatialPoints(coords = coords_WGS84, proj4string = proj_WGS84), proj_LCC))
  dm_LCC <- floor(xy_LCC / res_DayMet) # Origin at lower-lef corner (-2015000, -3037000)
    ## ==> (0, 0)- cell includes xlim = [0, 1000[ and ylim = [0, 1000[
    ## ==> at 100-m and 1-m scale: ok; but some deviations at 0.5-m scale

  cellID <- apply(dm_LCC, 1, FUN = function(chr) paste0("daymet_pixel_",
                        if(chr[1] < 0) "-" else "+", formatC(abs(chr[1]), width=6, flag="0", format="d"), "_",
                        if(chr[2] < 0) "-" else "+", formatC(abs(chr[2]), width=6, flag="0", format="d")))

  dm_LCC <- res_DayMet * dm_LCC + 500 # center of 1-km cells to avoid projection errors at cell margins
  dm_WGS84 <- sp::coordinates(sp::spTransform(sp::SpatialPoints(coords = dm_LCC, proj4string = proj_LCC), proj_WGS84))

  list(cellID = cellID, dm_LCC = dm_LCC, dm_WGS84 = dm_WGS84)
})

#' @return A list of which each element represents one year of daily weather data of class \linkS4class{swWeatherData}.
#' Units are [degree Celsius] for temperature and [cm / day] and for precipitation.
#' @references
#'  \href{https://daymet.ornl.gov/}{daymet website}
#'  publication: Thornton, P.E., Running, S.W., White, M.A. 1997. Generating surfaces of daily meteorological variables over large regions of complex terrain. Journal of Hydrology 190: 214 - 251. http://dx.doi.org/10.1016/S0022-1694(96)03128-9
#'  dataset v3: Thornton, P.E., M.M. Thornton, B.W. Mayer, Y. Wei, R. Devarakonda, R.S. Vose, and R.B. Cook. 2016. Daymet: Daily Surface Weather Data on a 1-km Grid for North America, Version 3. ORNL DAAC, Oak Ridge, Tennessee, USA. Accessed Month DD, YYYY. Time period: YYYY-MM-DD to YYYY-MM-DD. Spatial Range: N=DD.DD, S=DD.DD, E=DDD.DD, W=DDD.DD. http://dx.doi.org/10.3334/ORNLDAAC/1328
#'  \hred{https://github.com/khufkens/daymetr}{DaymetR package}
get_DayMet_NorthAmerica <- compiler::cmpfun(function(dir_data, cellID, Xdm_WGS84, Ydm_WGS84, start_year = simstartyr, end_year = endyr) {
  # Filename for data of this 1-km cell
  ftemp <- file.path(dir_data, paste0(cellID, "_", start_year, "_", end_year, ".csv"))

  # Get data
  pwd <- getwd()
  get_from_ornl <- TRUE
  if(file.exists(ftemp)){
    dm_temp <- try(read.table(ftemp, sep = ",", skip = 6, header = TRUE), silent=TRUE)
    if(!inherits(dm_temp, "try-error")) get_from_ornl <- FALSE
  }
  if(get_from_ornl){
    setwd(dir_data)
    # DaymetR package: https://bitbucket.org/khufkens/daymetr
    dm_temp <- try(DaymetR::download.daymet(site=cellID, lat=Ydm_WGS84, lon=Xdm_WGS84, start_yr=start_year, end_yr=end_year, internal=TRUE, quiet=TRUE), silent=TRUE)
  }

  # Convert to Rsoilwat format
  if(!inherits(dm_temp, "try-error")){
    if(exists(cellID, envir=.GlobalEnv)){
      temp <- get(cellID, envir=.GlobalEnv)$data
    } else if(!get_from_ornl && inherits(dm_temp, "data.frame")){
      temp <- dm_temp
    } else stop(paste("Daymet data not successful", cellID))

    data_all <- with(temp, data.frame(year, yday, tmax..deg.c., tmin..deg.c., prcp..mm.day./10))
    stopifnot(!anyNA(data_all), sum(data_all == -9999L) == 0)
    template_sw <- data.frame(matrix(NA, nrow=366, ncol=4, dimnames=list(NULL, c("DOY", "Tmax_C", "Tmin_C", "PPT_cm"))))

    years <- start_year:end_year
    weathDataList <- list()
    for(y in seq_along(years)){
      data_sw <- template_sw
      # All Daymet years, including leap years, have 1 - 365 days. For leap years, the Daymet database includes leap day. Values for December 31 are discarded from leap years to maintain a 365-day year.
      data_sw[1:365, ] <- data_all[data_all$year == years[y], -1]
      if(isLeapYear(years[y])){
        data_sw[366, ] <- c(366, data_sw[365, -1])
      }
      data_sw[, -1] <- round(data_sw[, -1], 2) #weather.digits
      weathDataList[[y]] <- new("swWeatherData",
                                year=years[y],
                                data = data.matrix(data_sw[if(isLeapYear(years[y])) 1:366 else 1:365, ], rownames.force=FALSE)) #strip row.names, otherwise they consume about 60% of file size
    }
    names(weathDataList) <- as.character(years)
  } else {
    weathDataList <- dm_temp
  }

  # Clean up
  if (exists(cellID, envir = .GlobalEnv))
    rm(list = cellID, envir = .GlobalEnv)
  setwd(pwd)

  weathDataList
})


ExtractGriddedDailyWeatherFromDayMet_NorthAmerica_swWeather <- compiler::cmpfun(function(dir_data, site_ids, coords_WGS84, start_year, end_year) {
  xy_WGS84 <- matrix(unlist(coords_WGS84), ncol = 2)[1, , drop = FALSE]
  dm <- get_DayMet_cellID(xy_WGS84)

  get_DayMet_NorthAmerica(
    dir_data = dir_data,
    cellID = dm$cellID[1],
    Xdm_WGS84 = dm$dm_WGS84[1, 1], Ydm_WGS84 = dm$dm_WGS84[1, 2],
    start_year, end_year)
})

# Function to be executed for all SoilWat-sites together
#' @return An invisible zero. A list of which each element represents one year of daily weather data of class \linkS4class{swWeatherData}. The list is copied to the weather database.
#' Units are [degree Celsius] for temperature and [cm / day] and for precipitation.
#' @references
#'  \href{https://daymet.ornl.gov/}{daymet website}
#'  publication: Thornton, P.E., Running, S.W., White, M.A. 1997. Generating surfaces of daily meteorological variables over large regions of complex terrain. Journal of Hydrology 190: 214 - 251. http://dx.doi.org/10.1016/S0022-1694(96)03128-9
#'  dataset v3: Thornton, P.E., M.M. Thornton, B.W. Mayer, Y. Wei, R. Devarakonda, R.S. Vose, and R.B. Cook. 2016. Daymet: Daily Surface Weather Data on a 1-km Grid for North America, Version 3. ORNL DAAC, Oak Ridge, Tennessee, USA. Accessed Month DD, YYYY. Time period: YYYY-MM-DD to YYYY-MM-DD. Spatial Range: N=DD.DD, S=DD.DD, E=DDD.DD, W=DDD.DD. http://dx.doi.org/10.3334/ORNLDAAC/1328
ExtractGriddedDailyWeatherFromDayMet_NorthAmerica_dbW <- compiler::cmpfun(function(dir_data, site_ids, coords_WGS84, start_year, end_year, dir_temp = tempdir(), dbW_compression_type = "gzip") {
  print(paste("Started 'ExtractGriddedDailyWeatherFromDayMet_NorthAmerica' at", Sys.time()))

  # Check if weather data was previously partially extracted
  wtemp_file <- file.path(dir_temp, "DayMet_weather_temp.rds")
  site_ids_done <- if (file.exists(wtemp_file)) readRDS(wtemp_file) else NULL
  iuse <- !(site_ids %in% site_ids_done)

  if (sum(iuse) > 0) {
    site_ids_todo <- site_ids[iuse]
    xy_WGS84 <- coords_WGS84[iuse, , drop=FALSE]
    dm <- get_DayMet_cellID(xy_WGS84)

    #TODO: re-write for parallel processing (does it make sense to download in parallel?)
    # Extract weather data sequentially for requested locations
    for (idm in seq_along(site_ids_todo)) {
      print(paste(Sys.time(), "DayMet data extraction of site", site_ids_todo[idm], "at", paste(round(coords_WGS84[idm, ], 4), collapse="/")))

      weatherData <- get_DayMet_NorthAmerica(
        dir_data = dir_data,
        cellID = dm$cellID[idm],
        Xdm_WGS84 = dm$dm_WGS84[idm, 1], Ydm_WGS84 = dm$dm_WGS84[idm, 2],
        start_year, end_year)

      if (!inherits(weatherData, "try-error")) {
        # Store site weather data in weather database
        data_blob <- Rsoilwat31::dbW_weatherData_to_blob(weatherData, type = dbW_compression_type)
        Rsoilwat31:::dbW_addWeatherDataNoCheck(Site_id = site_ids_todo[idm],
          Scenario_id = 1,
          StartYear = start_year,
          EndYear = end_year,
          weather_blob = data_blob)

        site_ids_done <- c(site_ids_done, site_ids_todo[idm])
        saveRDS(site_ids_done, file = wtemp_file)
      } else {
        print(paste(Sys.time(), "DayMet data extraction NOT successful for site", site_ids_todo[idm], weatherData))
      }
    }
  }

  print(paste("Finished 'ExtractGriddedDailyWeatherFromDayMet_NorthAmerica' at", Sys.time()))

  invisible(0)
})


# Function to be executed for all SoilWat-sites together
#' @return An invisible zero. A list of which each element represents one year of daily weather data of class \linkS4class{swWeatherData}. The list is copied to the weather database.
#' Units are [degree Celsius] for temperature and [cm / day] and for precipitation.
ExtractGriddedDailyWeatherFromNRCan_10km_Canada <- compiler::cmpfun(function(dir_data,
  site_ids, coords_WGS84, start_year, end_year,
  dir_temp = tempdir(), dbW_compression_type = "gzip", do_parallel = FALSE, ncores = 1L) {

  print(paste("Started 'ExtractGriddedDailyWeatherFromNRCan_10km_Canada' at", Sys.time()))

  NRC_years <- as.integer(list.dirs(path=dir_temp, recursive=FALSE, full.names=FALSE))
  NRC_target_years <- NRC_years[NRC_years %in% start_year:end_year]
  stopifnot(start_year:end_year %in% NRC_target_years)

  vars <- c("max", "min", "pcp") # units = C, C, mm/day
  prj_geographicWGS84 <- sp::CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
  prj_geographicNAD83 <- sp::CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")

  sp_locs <- sp::SpatialPoints(coords=coords_WGS84, proj4string=prj_geographicWGS84)
  sp_locs <- sp::spTransform(sp_locs, CRSobj=prj_geographicNAD83)

  if (do_parallel)
    raster::beginCluster(n = ncores, type = "SOCK")

  #TODO: re-write for a more memory friendly approach

  # Check if weather data was partially extracted already
  wtemp_file <- file.path(dir_temp, "NRCan_weather_temp.RData")
  if(file.exists(wtemp_file)){
    load(wtemp_file) # NRC_weather, iy
    yr_offset <- iy
    NRC_use_years <- NRC_target_years[-(1:iy)]
  } else {
    NRC_weather <- array(NA, dim=c(length(sp_locs), 366, length(NRC_target_years), 3), dimnames=list(NULL, NULL, NRC_target_years, c("Tmax(C)", "Tmin(C)", "PPT(mm)")))
    NRC_use_years <- NRC_target_years
    yr_offset <- 0
  }

  # Extract weather data for all locations together for each day of each year
  pwd <- getwd()
  for(iy in seq_along(NRC_use_years)){ # Loop through years
    print(paste(Sys.time(), "NRC data extraction of year", NRC_use_years[iy]))
    setwd(file.path(dir_temp, NRC_use_years[iy]))
    NRC_days <- list.files() #find all days for this year
    ndays <- length(NRC_days) / length(vars)
    stopifnot(ndays == if(isLeapYear(NRC_use_years[iy])) 366 else 365)

    # Stack rasters for each day and extract data
    NRC_stack <- raster::stack(NRC_days, RAT=FALSE, quick=TRUE)
    raster::projection(NRC_stack) <- prj_geographicNAD83
    temp <- round(raster::extract(NRC_stack, sp_locs), 2) #weather.digits; [sp_locs, NRC_days x vars]

    # Convert extraction information to array
    ivars <- substr(NRC_days, 1, 3) # sapply(vars, nchar) == 3
    for(iv in seq_along(vars)){
      idays <- as.integer(sapply(strsplit(NRC_days[vars[iv] == ivars], split="[_.]"), FUN=function(x) x[2]))
      NRC_weather[, 1:ndays, yr_offset + iy, iv] <- temp[, which(vars[iv] == ivars)[order(idays)][1:ndays]]
    }
    save(NRC_weather, iy, file=wtemp_file)
  }
  setwd(pwd)
  if (do_parallel)
    raster::endCluster()


  # Convert weather array to SoilWat weather objects for each sites
  NRC_weather[, , , "PPT(mm)"] <- NRC_weather[, , , "PPT(mm)"] / 10	# convert from mm/day to cm/day

  for (i in seq_along(site_ids)) {
    if (i %% 100 == 1)
      print(paste(Sys.time(), "storing NRC weather data of site_id", site_ids[i], i, "of", length(site_ids), "sites in database"))

    weatherData <- list()
    for (iy in seq_along(NRC_target_years)) {
      doys <- if (isLeapYear(NRC_use_years[iy])) 1:366 else 1:365
      data_sw <- cbind(doys, NRC_weather[i, doys, iy, ]) #DOY Tmax(C) Tmin(C) PPT(cm) [ppt was converted from mm to cm]
      colnames(data_sw) <- c("DOY", "Tmax_C", "Tmin_C", "PPT_cm")
      weatherData[[iy]] <- new("swWeatherData",
                              year = NRC_target_years[iy],
                              data = data.matrix(data_sw, rownames.force = FALSE))
    }
    names(weatherData) <- as.character(NRC_target_years)

    # Store site weather data in weather database
    data_blob <- Rsoilwat31::dbW_weatherData_to_blob(weatherData, type = dbW_compression_type)
    Rsoilwat31:::dbW_addWeatherDataNoCheck(Site_id = site_ids[i],
      Scenario_id = 1,
      StartYear = start_year,
      EndYear = end_year,
      weather_blob = data_blob)
  }
  #unlink(file=wtemp_file)

  print(paste("Finished 'ExtractGriddedDailyWeatherFromNRCan_10km_Canada' at", Sys.time()))

  rm(NRC_weather, weatherData, data_blob)
  gc()

  invisible(0)
})

get_NCEPCFSR_data <- compiler::cmpfun(function(dat_sites, daily = FALSE, monthly = FALSE,
                cfsr_so,
                yearLow, yearHigh, dir.in.cfsr, dir_temp,
                n_site_per_core = 100,
                do_parallel = FALSE, parallel_backend = "snow", cl = NULL,
                rm_mc_files = FALSE, continueAfterAbort = FALSE) {

#str(dat_sites): 'data.frame':	n_sites obs. of  3 variables:
# $ WeatherFolder: chr  ...
# $ X_WGS84      : num  -117 -117 -117 -117 -120 ...
# $ Y_WGS84      : num  32.8 32.8 32.8 32.8 38.9 ...

  years <- yearLow:yearHigh

  # directory paths
  dir_temp_cfsr <- file.path(dir_temp, "temp_NCEFCFSR")
  dir_temp_sites <- file.path(dir_temp_cfsr, dat_sites[, "WeatherFolder"])

  # determine previous efforts
  if (continueAfterAbort) {
    i_done <- file.exists(dir_temp_sites)
    if (sum(i_done) > 0) {
      for (i in which(i_done)) {
        i_done[i] <-
          if (monthly) {
            file.exists(file.path(dir_temp_sites[i], "mc.csv")) ||
            {file.exists(file.path(dir_temp_sites[i], "cc.txt")) &&
            file.exists(file.path(dir_temp_sites[i], "rh.txt")) &&
            file.exists(file.path(dir_temp_sites[i], "ws.txt"))}
          } else {
            TRUE
          } && if (daily) {
            d_files <- list.files(dir_temp_sites[i], pattern = "weath.")
            d_years <- as.integer(sapply(strsplit(d_files, ".", fixed = TRUE), function(x) x[2]))
            all(d_years %in% years)
          } else {
            TRUE
          }
        if (!i_done[i])
          unlink(dir_temp_sites[i], recursive = TRUE)
      }
    }
    i_todo <- !i_done

  } else {
    i_todo <- rep(TRUE, nrow(dat_sites))
  }

  # prepare tasks
  # do the extractions, loop over chunks of sites
  n_sites <- sum(i_todo)
  n_sites_all <- nrow(dat_sites)

  if (n_sites > 0) {
    dat_sites_todo <- dat_sites[i_todo, ]

    dir.create(dir_temp_cfsr, showWarnings = FALSE)
    temp <- lapply(dir_temp_sites, dir.create, showWarnings = FALSE)
    dir_temp.sitesC <- gsub("/", "//", normalizePath(dir_temp_sites)) # C-style paths; they cannot be relative to ~

    n_years <- length(years)
    n_climvars <- n_dailyvars <- 3
    do_sites <- parallel::splitIndices(n_sites, ceiling(n_sites / n_site_per_core))
    do_daily <- expand.grid(types = seq_len(n_dailyvars) - 1, months = st_mo, years = years)

    dtemp <- getwd()
    setwd(dir.in.cfsr)

    # set up parallel
    if (do_parallel) {
      obj2exp <- gather_objects_for_export(
        varlist = c("load_NCEPCFSR_shlib", "cfsr_so", "dir.in.cfsr"),
        list_envs = list(local = environment(), parent = parent.frame(), global = .GlobalEnv))

      if (identical(parallel_backend, "mpi")) {
        export_objects_to_workers(obj2exp, "mpi")
        Rmpi::mpi.bcast.cmd(load_NCEPCFSR_shlib(cfsr_so))
        Rmpi::mpi.bcast.cmd(setwd(dir.in.cfsr))

      } else if (identical(parallel_backend, "snow")) {
        export_objects_to_workers(obj2exp, "snow", cl)
        snow::clusterEvalQ(cl, load_NCEPCFSR_shlib(cfsr_so))
        snow::clusterEvalQ(cl, setwd(dir.in.cfsr))
      }
    }

    for (k in seq_along(do_sites)) {
      print(paste(Sys.time(), ": NCEP/CFSR extraction of",
        if(daily) "daily",
        if(daily && monthly) "and",
        if(monthly) "monthly",
        "data: chunk", k, "of", length(do_sites)))

      nDailyReads <- nDailyWrites <- nMonthlyReads <- nMonthlyWrites <- 0
      ntemp <- length(do_sites[[k]])
      irows <- do_sites[[k]]
      longs <- dat_sites_todo[irows, "X_WGS84"]
      lats <- dat_sites_todo[irows, "Y_WGS84"]
      dtemp <- dir_temp.sitesC[irows]

#      if (print.debug)
#        print(paste(Sys.time(), "cfsr chunk", k, ": # open R files", system2(command="lsof", args="-c R | wc -l", stdout=TRUE)))

      if (do_parallel) {
        if (identical(parallel_backend, "mpi")) {
          if (daily) {
            nDailyReads <- Rmpi::mpi.applyLB(x=1:nrow(do_daily), fun=gribDailyWeatherData, do_daily=do_daily, nSites=ntemp, latitudes=lats, longitudes=longs)
            nDailyReads <- do.call(sum, nDailyReads)

            nDailyWrites <- Rmpi::mpi.applyLB(x=years, fun=writeDailyWeatherData, nSites=ntemp, siteNames=dat_sites_todo[irows, "WeatherFolder"], siteDirsC=dtemp)
            nDailyWrites <- do.call(sum, nDailyWrites)
          }
          if (monthly) {
            nMonthlyReads <- Rmpi::mpi.applyLB(x=0:(n_climvars-1), fun=gribMonthlyClimate, nSites=ntemp, latitudes=lats, longitudes=longs, siteDirsC=dtemp, yearLow=yearLow, yearHigh=yearHigh)
            nMonthlyReads <- do.call(sum, nMonthlyReads)
          }
          if (monthly && k == length(do_sites)) { # only do at the end
            nMonthlyWrites <- Rmpi::mpi.applyLB(x=seq_len(n_sites_all), fun=writeMonthlyClimate, siteDirsC=dir_temp.sitesC)
            nMonthlyWrites <- do.call(sum, nMonthlyWrites)
          }
        } else if (identical(parallel_backend, "snow")) {
          if (daily) {
            nDailyReads <- snow::clusterApplyLB(cl, x=1:nrow(do_daily), fun=gribDailyWeatherData, do_daily=do_daily, nSites=ntemp, latitudes=lats, longitudes=longs)
            nDailyReads <- do.call(sum, nDailyReads)

            nDailyWrites <- snow::clusterApplyLB(cl, x=years, fun=writeDailyWeatherData, nSites=ntemp, siteNames=dat_sites[irows, "WeatherFolder"], siteDirsC=dtemp)
            nDailyWrites <- do.call(sum, nDailyWrites)
          }
          if (monthly) {
            nMonthlyReads <- snow::clusterApplyLB(cl, x=0:(n_climvars-1), fun=gribMonthlyClimate, nSites=ntemp, latitudes=lats, longitudes=longs, siteDirsC=dtemp, yearLow=yearLow, yearHigh=yearHigh)
            nMonthlyReads <- do.call(sum, nMonthlyReads)
          }
          if (monthly && k == length(do_sites)) { # only do at the end
            nMonthlyWrites <- snow::clusterApplyLB(cl, x=seq_len(n_sites_all), fun=writeMonthlyClimate, siteDirsC=dir_temp.sitesC)
            nMonthlyWrites <- do.call(sum, nMonthlyWrites)
          }
        } else if (identical(parallel_backend, "multicore")) {
          list.export <- ls(obj2exp)
          if (daily) {
            nDailyReads <- foreach::foreach(id = 1:nrow(do_daily), .combine="sum", .errorhandling="remove", .inorder=FALSE, .export=list.export) %dopar%
              gribDailyWeatherData(id, do_daily=do_daily, nSites=ntemp, latitudes=lats, longitudes=longs)
            nDailyWrites <- foreach::foreach(y = years, .combine="sum", .errorhandling="remove", .inorder=FALSE, .export=list.export) %dopar%
              writeDailyWeatherData(y, nSites=ntemp, siteNames=dat_sites[irows, "WeatherFolder"], siteDirsC=dtemp)
          }
          if (monthly) {
            nMonthlyReads <- foreach::foreach(iv = 0:(n_climvars-1), .combine="sum", .errorhandling="remove", .inorder=FALSE, .export=list.export) %dopar%
              gribMonthlyClimate(iv, nSites=ntemp, latitudes=lats, longitudes=longs, siteDirsC=dtemp, yearLow=yearLow, yearHigh=yearHigh)
          }
          if (monthly && k == length(do_sites)) { # only do at the end
            nMonthlyWrites <- foreach::foreach(ic = seq_len(n_sites_all), .combine="sum", .errorhandling="remove", .inorder=FALSE, .export=list.export) %dopar%
              writeMonthlyClimate(ic, siteDirsC=dir_temp.sitesC)
          }
        }
      } else {
        if (daily) {
          nDailyReads <- foreach::foreach(id = 1:nrow(do_daily), .combine="sum", .errorhandling="remove", .inorder=FALSE) %do%
            gribDailyWeatherData(id, do_daily=do_daily, nSites=ntemp, latitudes=lats, longitudes=longs)
          nDailyWrites <- foreach::foreach(y = years, .combine="sum", .errorhandling="remove", .inorder=FALSE) %do%
            writeDailyWeatherData(y, nSites=ntemp, siteNames=dat_sites[irows, "WeatherFolder"], siteDirsC=dtemp)
        }
        if (monthly) {
          nMonthlyReads <- foreach::foreach(iv = 0:(n_climvars-1), .combine="sum", .errorhandling="remove", .inorder=FALSE) %do%
            gribMonthlyClimate(iv, nSites=ntemp, latitudes=lats, longitudes=longs, siteDirsC=dtemp, yearLow=yearLow, yearHigh=yearHigh)
        }
        if (monthly && k == length(do_sites)) { # only do at the end
          nMonthlyWrites <- foreach::foreach(ic = seq_len(n_sites_all), .combine="sum", .errorhandling="remove", .inorder=FALSE) %do%
            writeMonthlyClimate(ic, siteDirsC=dir_temp.sitesC)
        }
      }

      # check that all was done
      if (daily)
        stopifnot(nDailyReads == nrow(do_daily), nDailyWrites == n_years)
      if (monthly)
        stopifnot(nMonthlyReads == n_climvars)
    }

    # check that all was done
    if (monthly)
      stopifnot(nMonthlyWrites == n_sites)

    # clean up parallel
    if (do_parallel) {
      if (identical(parallel_backend, "mpi")) {
        Rmpi::mpi.bcast.cmd(rm(list=ls()))
        Rmpi::mpi.bcast.cmd(gc())
      }
      if (identical(parallel_backend, "snow")) {
        snow::clusterEvalQ(cl, rm(list=ls()))
        snow::clusterEvalQ(cl, gc())
      }
    }

    setwd(dtemp)
  }


  # concatenating the monthlyClimate csv files
  if (monthly) {
    res_clim <- data.frame(matrix(NA, nrow = n_sites_all, ncol = 1 + n_climvars * 12))
    colnames(res_clim) <- c("WeatherFolder", paste0("Cloud_m", st_mo), paste0("Wind_m", st_mo), paste0("RH_m", st_mo))
    res_clim[, "WeatherFolder"] <- dat_sites[, "WeatherFolder"]

    for (i in seq_len(n_sites_all)) {
      ftemp <- file.path(dir_temp_sites[i], "mc.csv")
      if (file.exists(ftemp)) {
        table.mc <- read.csv(file=ftemp, comment="", stringsAsFactors=FALSE)
        res_clim[i, 1 + st_mo] <- table.mc[, "Cloud_Cover"]
        res_clim[i, 1 + 12 + st_mo] <- table.mc[, "Surface_Wind"]
        res_clim[i, 1 + 24 + st_mo] <- table.mc[, "Rel_Humidity"]

        if (rm_mc_files == TRUE) unlink(ftemp)
      }
    }
  } else {
    res_clim <- NULL
  }

  list(dir_temp_cfsr = dir_temp_cfsr, res_clim = res_clim)
})


GriddedDailyWeatherFromNCEPCFSR_Global <- compiler::cmpfun(function(site_ids, dat_sites, start_year, end_year,
  meta_cfsr, n_site_per_core = 100, do_parallel = FALSE, parallel_backend = "snow", cl = NULL,
  rm_temp = TRUE, continueAfterAbort = FALSE, dir_temp = tempdir(),
  dbW_compression_type = "gzip") {

  #Citations: Saha, S., et al. 2010. NCEP Climate Forecast System Reanalysis (CFSR) Selected Hourly Time-Series Products, January 1979 to December 2010. Research Data Archive at the National Center for Atmospheric Research, Computational and Information Systems Laboratory. http://dx.doi.org/10.5065/D6513W89.
  # http://rda.ucar.edu/datasets/ds093.1/. Accessed 8 March 2012.

  # do the extractions
  etemp <- get_NCEPCFSR_data(dat_sites = dat_sites,
    daily = TRUE, monthly =  FALSE,
    cfsr_so = meta_cfsr$cfsr_so,
    yearLow = start_year, yearHigh = end_year,
    dir.in.cfsr = meta_cfsr$dir.in.cfsr,
    dir_temp = dir_temp,
    n_site_per_core = n_site_per_core,
    do_parallel = do_parallel,
    parallel_backend = parallel_backend, cl = cl,
    rm_mc_files = TRUE,
    continueAfterAbort = continueAfterAbort)

  # move the weather data into the database
  for (i in seq_along(site_ids)) {
    weatherData <- Rsoilwat31::getWeatherData_folders(
      LookupWeatherFolder = etemp$dir_temp_cfsr,
      weatherDirName = dat_sites[i, "WeatherFolder"],
      filebasename = "weath",
      startYear = start_year,
      endYear = end_year)

    # Store site weather data in weather database
    data_blob <- Rsoilwat31::dbW_weatherData_to_blob(weatherData, type = dbW_compression_type)
    Rsoilwat31:::dbW_addWeatherDataNoCheck(Site_id = site_ids[i],
      Scenario_id = 1,
      StartYear = start_year,
      EndYear = end_year,
      weather_blob = data_blob)
  }

  if (rm_temp) {
    dir.remove(etemp$dir_temp_cfsr)
    temp <- lapply(c("ppt", "tmax", "tmin"), FUN=function(x) dir.remove(file.path(meta_cfsr$dir.in.cfsr, "temporary_dy", x)))
  }

  print(paste("Finished 'ExtractGriddedDailyWeatherFromNCEPCFSR_Global' at", Sys.time()))

  invisible(0)
})


update_biomass <- compiler::cmpfun(function(funct_veg = c("Grass", "Shrub", "Tree", "Forb"), use, prod_input, prod_default) {
  funct_veg <- match.arg(funct_veg)

  comps <- c("_Litter", "_Biomass", "_FractionLive", "_LAIconv")
  veg_ids = lapply(comps, function(x)
    grep(paste0(funct_veg, x), names(use)))
  veg_incl = lapply(veg_ids, function(x) use[x])

  temp <- slot(prod_default, paste0("MonthlyProductionValues_", tolower(funct_veg)))
  if (any(unlist(veg_incl))) {
    for (k in seq_along(comps)) if (any(veg_incl[[k]]))
      temp[veg_incl[[k]], k] <- as.numeric(prod_input[, veg_ids[[k]][veg_incl[[k]]]])
  }

  temp
})


########################
#------ GISSM functions
# Schlaepfer, D.R., Lauenroth, W.K. & Bradford, J.B. (2014). Modeling regeneration responses of big sagebrush (Artemisia tridentata) to abiotic conditions. Ecol Model, 286, 66-77.

#' Function to convert soil depth to soil layer
SoilLayer_at_SoilDepth <- compiler::cmpfun(function(depth_cm, layers_depth) {
  pmax(1, pmin(length(layers_depth), 1 + findInterval(depth_cm - 0.01, layers_depth)))
})


#' Function to calculate for each day of the year, duration in days of upcoming favorable conditions accounting for consequences.unfavorable=0 (if conditions become unfavorable, then restart the count), =1 (resume)
calculate_DurationFavorableConditions <- compiler::cmpfun(function(RYyear, consequences.unfavorable, Germination_DuringFavorableConditions, RYyear_ForEachUsedDay) {

  index.year <- RYyear_ForEachUsedDay == RYyear
  conditions <- Germination_DuringFavorableConditions[index.year]
  doys <- seq_len(sum(index.year))
  doys[!conditions] <- NA	#calculate only for favorable days
  out <- rep(NA, times = sum(index.year))

  if (consequences.unfavorable == 0) {
    # if conditions become unfavorable, then restart the count afterwards
    temp.rle <- rle(conditions)
    if (sum(!temp.rle$values) > 0) {
      temp.unfavorable_startdoy <- c((1 + c(0, cumsum(temp.rle$lengths)))[!temp.rle$values], 1 + sum(index.year)) #add starts for odd- and even-lengthed rle

      temp.rle$values <- if (temp.rle$values[1]) {
          #first rle period is favorable
          rep(temp.unfavorable_startdoy, each = 2)
        } else {
          #first rle period is unfavorable
          rep(temp.unfavorable_startdoy[-1], each = 2)
        }
      temp.rle$values <- temp.rle$values[seq_along(temp.rle$lengths)]

    } else {#every day is favorable
      temp.rle$values <- length(conditions) + 1
    }
    out <- inverse.rle(temp.rle) - doys	#difference to next following start of a period of unfavorable conditions

  } else if(consequences.unfavorable == 1) {
    # if conditions become unfavorable, then resume the count afterwards
    temp <- sum(conditions)
    count <- if(temp > 0) {
      temp:1
    } else {#every day is unfavorable
      vector("numeric", length = 0)
    }

    out <- napredict(na.action(na.exclude(doys)), count)	#sum of following favorable conditions in this year
  }

  out
})

get_modifiedHardegree2006NLR <- compiler::cmpfun(function(RYdoy, Estimate_TimeToGerminate, TmeanJan, a, b, c, d, k1_meanJanTemp, k2_meanJanTempXIncubationTemp, k3_IncubationSWP, Tgerm.year, SWPgerm.year, durations, rec.delta = 1, nrec.max = 10L) {
  for (nrec in seq_len(nrec.max)) {
    Estimate_TimeToGerminate <- Estimate_TimeToGerminate.oldEstimate <- max(0, round(Estimate_TimeToGerminate))

    ids <- RYdoy:(RYdoy + Estimate_TimeToGerminate - 1)
    Tgerm <- mean(Tgerm.year[ids], na.rm = TRUE)
    SWPgerm <- mean(SWPgerm.year[ids], na.rm = TRUE)

    temp.c.lim <- -(Tgerm - b) * (d^2 - 1) / d
    c <- if (c > 0) {
      if (c > temp.c.lim) c else {temp.c.lim + tol}
    } else if (c < 0) {
      if (c < temp.c.lim) c else {temp.c.lim - tol}
    }

    #NLR model (eq.5) in Hardegree SP (2006) Predicting Germination Response to Temperature. I. Cardinal-temperature Models and Subpopulation-specific Regression. Annals of Botany, 97, 1115-1125.
    temp <- a * exp(-0.693147181 / log(d)^2 * log(1 + (Tgerm - b) * (d^2 - 1) / (c * d))^2) # all.equal(log(2), 0.693147181)

    #drs addition to time to germinate dependent on mean January temperature and soil water potential
    temp <- 1 / temp +
            k1_meanJanTemp * TmeanJan +
            k2_meanJanTempXIncubationTemp * TmeanJan * Tgerm +
            k3_IncubationSWP * SWPgerm
    Estimate_TimeToGerminate <- max(1, round(temp) )

    #break if convergence or not enough time in this year
    if (abs(Estimate_TimeToGerminate - Estimate_TimeToGerminate.oldEstimate) <= rec.delta |
        RYdoy + Estimate_TimeToGerminate - 1 > 365)
      break
  }

  out <- if (nrec >= nrec.max) {
      round(mean(c(Estimate_TimeToGerminate, Estimate_TimeToGerminate.oldEstimate)), 0)
    } else {
      Estimate_TimeToGerminate
    }

  if (out <= durations[RYdoy] & RYdoy + out <= 365) out else NA #test whether enough time to germinate
})

#' Function to estimate time to germinate for each day of a given year and conditions (temperature, top soil SWP)
calculate_TimeToGerminate_modifiedHardegree2006NLR <- compiler::cmpfun(function(RYyear, Germination_DuringFavorableConditions, LengthDays_FavorableConditions, RYyear_ForEachUsedDay, soilTmeanSnow, swp.TopMean, TmeanJan, param) {
  #values for current year
  index.year <- RYyear_ForEachUsedDay == RYyear
  conditions <- Germination_DuringFavorableConditions[index.year]

  # determining time to germinate for every day
  a <- max(tol, param$Hardegree_a)
  b <- param$Hardegree_b
  d <- max(tol, if (param$Hardegree_d == 1) {
                  if (runif(1) > 0.5) {1 + tol} else {1 - toln}
                } else {
                  param$Hardegree_d
                })
  temp.c <- if (param$Hardegree_c != 0) param$Hardegree_c else sign(runif(1) - 0.5) * tol

  TimeToGerminate.favorable <- unlist(lapply(which(conditions), get_modifiedHardegree2006NLR,
    Estimate_TimeToGerminate = 1, TmeanJan = TmeanJan, a = a, b = b, c = temp.c, d = d,
    k1_meanJanTemp = param$TimeToGerminate_k1_meanJanTemp,
    k2_meanJanTempXIncubationTemp = param$TimeToGerminate_k2_meanJanTempXIncubationTemp,
    k3_IncubationSWP = param$TimeToGerminate_k3_IncubationSWP,
    Tgerm.year = soilTmeanSnow[index.year],
    SWPgerm.year = swp.TopMean[index.year],
    durations = LengthDays_FavorableConditions[index.year]))	#consequences of unfavorable conditions coded in here

  res <- rep(NA, length(conditions))
  if (length(TimeToGerminate.favorable) > 0) {
      res[conditions] <- TimeToGerminate.favorable
  }

  res
})

do.vector <- compiler::cmpfun(function(kill.vector, max.duration.before.kill) {
  doys <- seq_along(kill.vector)
  doys[!kill.vector] <- NA	#calculate only for kill days
  temp.rle <- rle(kill.vector)

  if (sum(!temp.rle$values) > 0) {
    temp.startdoy <- (1 + c(0, cumsum(temp.rle$lengths)))[!temp.rle$values]
    temp.rle$values <- if(temp.rle$values[1]) {
        rep(temp.startdoy, each = 2)
      } else {
        rep(temp.startdoy[-1], each = 2)
      }
    temp.rle$values <- temp.rle$values[seq_along(temp.rle$lengths)]

  } else {#every day is kill free
    temp.rle$values <- length(kill.vector) + 1
  }
  kill.durations <- inverse.rle(temp.rle) - doys
  mortality <- rep(FALSE, times = length(kill.vector))
  mortality[kill.durations > max.duration.before.kill] <- TRUE

  mortality
})

#' Function to calculate mortality under conditions and checks survival limit
calculate_SeedlingMortality_ByCondition <- compiler::cmpfun(function(kill.conditions, max.duration.before.kill) {
  if (length(dim(kill.conditions)) > 0) { #i.e., is.matrix, columns=soil layers
    apply(kill.conditions, 2, do.vector, max.duration.before.kill)
  } else {
    do.vector(kill.conditions, max.duration.before.kill)
  }
})


#' Function to calculate favorable conditions for seedling growth for each day of a given year
calculate_SuitableGrowthThisYear_UnderCondition <- compiler::cmpfun(function(favorable.conditions, consequences.unfavorable) {
  out <- rep(NA, times = length(favorable.conditions))

  if (consequences.unfavorable == 0) {
    #if conditions become unfavorable, then stop growth for rest of season
    temp.rle <- rle(favorable.conditions)
    temp.firstFavorable.index <- which(temp.rle$values)[1]

    if(!is.na(temp.firstFavorable.index) && temp.firstFavorable.index < length(temp.rle$values)){
      temp.rle$values[(temp.firstFavorable.index+1):length(temp.rle$values)] <- FALSE
      out <- inverse.rle(temp.rle)
    } else { #nothing changed, either because all days are either favorable or unfavorable or because first favorable period is also the last in the season
      out <- favorable.conditions
    }

  } else if(consequences.unfavorable == 1) {
    #if conditions become unfavorable, then resume growth afterwards
    out <- favorable.conditions
  }

  out
})


#' Function to calculate rooting depth at given age
SeedlingRootingDepth <- compiler::cmpfun(function(age, P0, K, r) {
  depth <- K * P0 * exp(r * age) / (K + P0 * (exp(r * age) - 1))	#[age] = days, [P0, K, r] = mm

  pmax(0, depth) / 10 # units = cm
})


get.DoyAtLevel <- compiler::cmpfun(function(x, level) {
  which(x == level & x > 0)
})

get.DoyMostFrequentSuccesses <- compiler::cmpfun(function(doys, data) {
  res1.max <- sapply(1:2, function(x) quantile(doys[doys[, x] > 0, x], probs = c(0.1, 1), type = 3)) # must return one of the values because the quantiles are compared against the values in function 'get.DoyAtLevel'
  germ.doy <- if (all(!data[, 1])) {
      #no successful germination
      list(NA, NA)
    } else {
      lapply(1:2, function(x) get.DoyAtLevel(doys[, 1], res1.max[x, 1]))
    }
  sling.doy <- if (all(!data[, 2])) {
      #no successful seedlings
      list(NA, NA)
    } else {
      lapply(1:2, function(x) get.DoyAtLevel(doys[, 2], res1.max[x, 2]))
    }
  res1.max <- list(germ.doy, sling.doy)

  unlist(lapply(res1.max, function(x) c(min(x[[1]]), median(x[[2]]), max(x[[1]]))))
})

#------ End of GISSM functions
########################
