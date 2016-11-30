#' @title Extract Weather Data from a Livneh Database
#'
#' @description Extracts daily weather data, including precipitation,
#' maximum temperature and minimum temperature
#'
#' @param    dir_data        directory containing Livneh data
#' @param    dir_db          the database directory
#' @param    site_ids        the sites to gather weather data for
#' @param    coords          the coordinates for each site in WGS84 format
#' @param    start_year      the start year in the sequence of data to gather
#' @param    end_year        the end year in the sequence of data to gather
#' @param    f_check         flag to check for errors in file structure - TRUE
#'                            for check else no integrity check
#' @param    backup          flag to create a backup of the weather data prior
#'                            to insertion in the database
#'                            (can create large files) - TRUE for backup
#'                            else no backup
#' @param    comp_type       the compression type of the data to
#'                            be inserted into the database
#' @param    run_parallel    whether the extraction should be ran in parallel
#' @param    num_cores       the num of cores to use if parallel
#'
#' @author   Charles Duso    <cd622@@nau.edu>
#' @export
extract_daily_weather_from_livneh_2013_north_america <-
                                          function(dir_data, dir_db,
                                                   site_ids, coords,
                                                   start_year, end_year,
                                                   f_check = TRUE,
                                                   backup = TRUE,
                                                   comp_type = "gzip",
                                                   run_parallel = FALSE,
                                                   num_cores = 0) {

    ########################################
    # Ensure necessary libraries are loaded
    ########################################
    require(ncdf4)      # For extracting netcdf data
    require(raster)     # For stacking netcdf data as raster objects
    require(Rsoilwat31) # For formatting the weather data
    require(RSQLite)    # For inserting data into the database
    require(sp)         # For the raster package

    #########################
    # Configuration settings
    #########################

    # Start timer for timing the extraction process
    t_elapsed        <- proc.time()
    print("Preparing to extract weather data from Livneh database.")

    # Go to the directory for the weather database for extraction
    setwd(dir_data)
    db_files         <- sort(list.files(dir_data))

    # Verify the data's integrity before executing
    if (f_check) {
      print("Verifying data integrity.")
      db_months        <- unique(substr(db_files, 44, 45))
      db_years         <- unique(substr(db_files, 40, 43))
      f_count         <- 1
      for (i in 1:length(db_years)) {
        for (j in 1:length(db_months)) {
          fName      <- paste(db_years[i], db_months[j], sep="")
          if (!grepl(pattern = fName, db_files[f_count])) {
            print(paste("ERROR: Monthly data file is missing for year-month: ",
                        db_years[i], "-", db_months[j], sep=""))
            stop()

          }
          f_count = f_count + 1
        }
      }
      print("Data integrity has been verified; no errors have been detected.")
    }

    # Refine coordinates to resolution suitable for Livneh
    print("Refining coordinates to match database resolution.")
    colnames(coords)      <- NULL
    coords                <- na.omit(coords)
    xy_wgs84              <- matrix(unlist(coords), ncol = 2)
    xy_wgs84              <- apply(xy_wgs84, 2, conv_res)

    # Create coordinates as spatial points for extraction with raster layers
    prj_geographicWGS84   <- CRS("+proj=longlat +ellps=WGS84
                                 +datum=WGS84 +no_defs +towgs84=0,0,0")
    sp_locs               <- SpatialPoints(coords=xy_wgs84,
                                           proj4string=prj_geographicWGS84)

    # Create necessary variables and containers for extraction
    seq_years             <-  seq(start_year, end_year)
    len_years             <-  length(seq_years)
    site_length           <-  length(site_ids)
    data_sw               <-  array(dim = c(site_length, 366, 3, len_years))

    print("Extracting data for supplied sequence of years.")

    # Prepare parallel extraction if set to TRUE
    if (run_parallel) {
      raster::beginCluster(n = num_cores, type = "SOCK")
    }

    #######################
    # Extract weather data
    #######################

    # Extract the data for each site for each year for each month
    j <- 1
    for (i in 1:len_years) {

      # Get data files for respective year
      files <- db_files[j:(j + 11)]

      print(paste("Extracting data for year ", seq_years[i], sep=""))
      print("Files: ")
      print(files)
      print("================================")

      # Extract Weather Data as Raster Stacks
      l_brick    <- lapply(files, convert_to_brick, type="Prec")
      l_stack    <- raster::stack(l_brick, bands = NULL, varname=type,
                                  layers = NULL, quick = TRUE)
      prec       <- round(raster::extract(x = l_stack, y = sp_locs,
                                          method = "simple", fun = NULL), 2)

      l_brick    <- lapply(files, convert_to_brick, type="Tmax")
      l_stack    <- raster::stack(l_brick, bands = NULL, varname=type,
                                  layers = NULL, quick = TRUE)
      tmax       <- round(raster::extract(x = l_stack, y = sp_locs,
                                          method = "simple", fun = NULL), 2)

      l_brick    <- lapply(files, convert_to_brick, type="Tmin")
      l_stack    <- raster::stack(l_brick, bands = NULL, varname=type,
                                  layers = NULL, quick = TRUE)
      tmin       <- round(raster::extract(x = l_stack, y = sp_locs,
                                          method = "simple", fun = NULL), 2)

      # Add data to global data array
      for (k in 1:site_length) {
        if (is_leapyear(seq_years[i])) {
          data_sw[k, , 1, i] <- tmax[k, ]
          data_sw[k, , 2, i] <- tmin[k, ]
          data_sw[k, , 3, i] <- (prec[k, ] / 10)
        } else {
          data_sw[k, , 1, i] <- c(tmax[k, ], 1)
          data_sw[k, , 2, i] <- c(tmin[k, ], 1)
          data_sw[k, , 3, i] <- c((prec[k, ] / 10), 1)
        }
      }


      # Increment j to the next year
      j = j + 12
    }

    # Stop parallel execution
    if (run_parallel) {
      raster::endCluster()
    }

    # Connect to the weather database
    setwd(dir_db)
    Rsoilwat31:::dbW_setConnection(file.path(dir_db, "dbWeatherData.sqlite"))

    # Backup RData in the event of an error with insertion
    if (backup) {
      print("Backing up data object.")
      save(data_sw, file="weathData.RData")
      print("Data object has been backed-up.")
    }

    # Format data and add it to the weather database
    print("Inserting data into weather database.")
    for (i in 1:site_length) {
      weather_data <- list()
      for (k in 1:len_years) {
        doys <- if (is_leapyear(seq_years[k]))  1:366 else 1:365
        out  <- cbind(doys, data_sw[i, doys, , k])
        colnames(out) <- c("DOY", "Tmax_C", "Tmin_C", "PPT_cm")
        weather_data[[k]] <-
                    new("swWeatherData",
                         year = seq_years[k],
                         data = data.matrix(out, rownames.force = FALSE))
      }
      names(weather_data) <- as.character(seq_years)

      # Write out to data blob so that data is appropriate for database
      data_blob <- Rsoilwat31::dbW_weatherData_to_blob(weather_data,
                                                       type = comp_type)
      # Store site weather data in weather database
      Rsoilwat31:::dbW_addWeatherDataNoCheck(Site_id      = site_ids[i],
                                             Scenario_id  = 1,
                                             StartYear    = start_year,
                                             EndYear      = end_year,
                                             weather_blob = data_blob)
    }


    #######################
    # Clean-up environment
    #######################

    print("Weather data has been successfully inserted.")

    # Remove files & clean garbage to free-up RAM for executions that
    # don't just involve database creation
    print("Cleaning up garbage.")
    gc()
    rm(weather_data, out, data_sw, data_blob, l_stack, l_brick, tmax, tmin,
       prec)

    # End timer and notify user that extraction has finished
    print("Data has been inserted.")
    print(proc.time() - t_elapsed)
    invisible(0)
}
