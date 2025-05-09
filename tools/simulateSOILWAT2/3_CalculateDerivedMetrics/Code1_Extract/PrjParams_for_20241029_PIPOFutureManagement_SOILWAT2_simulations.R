#------ Describe project: 20241029_PIPOFutureManagement_SOILWAT2_simulations
# rSW2metrics commit

do_full <- exists("args") && isTRUE(args[["do_full"]])

dir_prj <- ".."

if (do_full) {
  dir_rSFSW2 <- file.path(
    dir_prj, "..",
    "2_SOILWAT2_Simulations",
    "20241029_PIPOFutureManagement_SOILWAT2_simulations"
  )

} else {
  dir_rSFSW2 <- file.path(
    dir_prj, "..",
    "2_SOILWAT2_Simulations",
    "20241029_PIPOFutureManagement_SOILWAT2_simulations"
  )
}

dir_sw2_output <- file.path(dir_rSFSW2, "3_Runs")
has_rSOILWAT2_inputs <- TRUE

make_short_run_names <- rSW2metrics::shorten_run_names

N_exp <- 12L
N_scen <- 91L
id_scen_used <- 1L:N_scen


#--- Output path
if (do_full) {
  dir_out <- file.path(dir_prj, "20241029_PIPOFutureManagement_SOILWAT2_simulations", "Outputs")
} else {
  dir_out <- file.path(dir_prj, "Outputs_tests")
}


#--- Timeframe over which metrics are calculated
years_historical <- 1980:2020
years_future_projection <- NULL

years_timeseries_by_scen <- c(
  list(years_historical),
  lapply(1:18, function(k) 1950:2005),
  rcp45 = unlist(
    lapply(1:18, function(k) c(list(2025:2055), list(2069:2099))),
    recursive = FALSE
  ),
  rcp85 = unlist(
    lapply(1:18, function(k) c(list(2025:2055), list(2069:2099))),
    recursive = FALSE
  )
)

#--- Across-year aggregations of metrics
years_aggs_by_scen <- c(
  list(list(hist = years_historical)),
  lapply(
    id_scen_used[-1],
    function(k) list(histprj = 1950:2005, nearterm = 2025:2055, longterm = 2069:2099)
  )
)

# `fun_aggs_across_yrs` required only if `-add_aggs_across_yrs` and `-ts`
fun_aggs_across_yrs <- rSW2metrics::mean_sd_cv_mmk


#--- Seasonal index by month
#   1 = Winter = DJF, 2 = Spring = MAM, 3 = Summer = JJA, 4 = Fall = SON
season_by_month <- c(rep(1, 2), rep(2:4, each = 3), 1)
first_month_of_year <- 12L # First season (winter) starts in December


#--- Soil parameters
Nmax_soillayers <- 14L
# see `list_soil_variables()` for possible values
used_soil_variables <- c("depth", "sand", "clay", "gravel")


#--- Parameters for output type "SW2toTable"
# used by `metric_SW2toTable_daily()`
dir_out_SW2toTable <- file.path(
  dir_prj,
  basename(dir_rSFSW2),
  paste0(
    format(Sys.Date(), "%Y%m%d"),
    "_SOILWAT2_OutputShared__",
    basename(dir_rSFSW2)
  )
)

# Format of files written to disk: "rds" (default) or "csv"
format_share_SW2toTable <- "rds"

# Groups of variables to include in output
#   - "all" (default, includes all the following)
#   - "meteo", "snow", "radiation", "waterbalance", "evapotranspiration",
#     "soiltemperature", "VWC", "SWP", "SWAat30bar", "MDD"
outputs_SW2toTable <- "all"
