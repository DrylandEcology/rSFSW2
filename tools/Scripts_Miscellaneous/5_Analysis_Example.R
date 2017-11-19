#!/usr/bin/env Rscript


library("rSFSW2")

dir_prj <- "YOURPROJECT"

dir_dat <- file.path(dir_prj, "4_Data_SWOutputAggregated")

outDB_fname <- file.path(dir_dat, "dbTables.sqlite3")

# See index of all help pages of 'rSFSW2'
help(package = "rSFSW2")

# You can open the help/documentation for any function (with more or less helpful text),
# for example:
?get.SeveralOverallVariables_Scenario

# Connect to output database
outDB_con <- RSQLite::dbConnect(RSQLite::SQLite(), outDB_fname)

# Obtain list of all table names
outDB_tablenames <- dbOutput_ListOutputTables(outDB_con)
print(outDB_tablenames)

# Obtain list of all fields/columns of one table
outDB_overall_fields <- RSQLite::dbListFields(outDB_con, "aggregation_overall_mean")
print(outDB_overall_fields)

# Explanation of each possible output filed in the 'overall aggregated table':
#   https://github.com/Burke-Lauenroth-Lab/rSFSW2/blob/master/tools/Database_Stuff/Explanation_output_variables_overall_aggregation.xlsx

# Get data for an entire table
# for example: mean daily bulk VWC
dat_daily_vwcbulk_mean <- get.Table_Scenario(outDB_fname,
  responseName = "aggregation_doy_VWCmatric", MeanOrSD = "Mean",
  scenario = "Current", header = TRUE)

# for example: SD of daily bulk VWC
dat_daily_vwcbulk_sd <- get.Table_Scenario(outDB_fname,
  responseName = "aggregation_doy_VWCmatric", MeanOrSD = "SD",
  scenario = "Current", header = TRUE)

# Get data for several fields from one output table
# for example: means from 'outDB_overall_fields'
dat_overall_out1_mean <- get.SeveralOverallVariables_Scenario(outDB_fname,
  responseName = c("Labels", "SWinput_Soil_maxDepth_cm", "MAT_C", "MAP_mm"),
  MeanOrSD = "Mean", scenario = "Current")

# for example: SDs from 'outDB_overall_fields'
dat_overall_out1_sd <- get.SeveralOverallVariables_Scenario(outDB_fname,
  responseName = c("Labels", "SWinput_Soil_maxDepth_cm", "MAT_C", "MAP_mm"),
  MeanOrSD = "SD", scenario = "Current")


# Clean up
RSQLite::dbDisconnect(outDB_con)
