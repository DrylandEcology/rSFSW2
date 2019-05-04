#------ FUNCTIONS THAT DEAL WITH SIMULATION TIME



isLeapYear <- rSOILWAT2:::isLeapYear


#' Determine maximal span of simulation years across all experimental and design
#' treatments
#'
#' @param st An object as returned from the function
#'   \code{\link{setup_time_simulation_project}}.
#' @param SFSW2_prj_inputs An object as returned from function
#'   \code{\link{process_inputs}}.
#' @return The object \code{st} augmented with two named elements \itemize{
#'   \item \code{overall_simstartyr} which is the earliest year requested by any
#'   input \item \code{overall_endyr} which is the last year requested by any
#'   input }
get_simulation_time <- function(st, SFSW2_prj_inputs) {

  stopifnot(!is.null(st[["simstartyr"]]), !is.null(st[["endyr"]]))
  use_treat <- SFSW2_prj_inputs[["sw_input_treatments_use"]]
  use_exp <- SFSW2_prj_inputs[["sw_input_experimentals_use"]]

  if (any(SFSW2_prj_inputs[["create_treatments"]] == "YearStart")) {
    temp_tr <- if (use_treat["YearStart"]) {
        SFSW2_prj_inputs[["sw_input_treatments"]][, "YearStart"]
      } else NA

    temp_exp <- if (use_exp["YearStart"]) {
        SFSW2_prj_inputs[["sw_input_experimentals"]][, "YearStart"]
      } else NA

    st[["overall_simstartyr"]] <- min(st[["simstartyr"]], temp_tr, temp_exp,
      na.rm = TRUE)

  } else {
    st[["overall_simstartyr"]] <- st[["simstartyr"]]
  }

  if (any(SFSW2_prj_inputs[["create_treatments"]] == "YearEnd")) {
    temp_tr <- if (use_treat["YearEnd"]) {
        SFSW2_prj_inputs[["sw_input_treatments"]][, "YearEnd"]
      } else NA

    temp_exp <- if (use_exp["YearEnd"]) {
        SFSW2_prj_inputs[["sw_input_experimentals"]][, "YearEnd"]
      } else NA

    st[["overall_endyr"]] <- max(st[["endyr"]], temp_tr, temp_exp, na.rm = TRUE)

  } else {
    st[["overall_endyr"]] <- st[["endyr"]]
  }

  st
}

#' Describe the time of a simulation project
#'
#' @param sim_time A list with at least values for four named elements:
#'   \var{\dQuote{simstartyr}} and \var{\dQuote{endyr}}, one of the following
#'   two: \var{\dQuote{startyr}} or \var{\dQuote{spinup_N}}, and
#'   \var{dQuote{future_yrs}}.
#' @param add_st2 A logical value. If \code{TRUE}, the output of calling the
#'   function \code{\link[rSOILWAT2]{simTiming_ForEachUsedTimeUnit}}
#'   is appended to the returned list.
#' @param use_doy_range A logical value. If \code{TRUE}, then the result is
#'   additional daily indices indicating whether the \var{DOY} is within the
#'   days indicated in the \code{doy_ranges}.
#' @param doy_ranges A named list. Aggregation output variables and the daily
#'   \code{c(min, max)} of days you wish to calculate the aggregation over.
#' @param adjust_NS A logical value. If \code{TRUE}, then the result is
#'   corrected for locations on the southern vs. northern hemisphere. Only used
#'   if \code{add_st2} is \code{TRUE}.
#' @param A named list, i.e., the updated version of \code{sim_time}.
#'
#' @seealso \code{\link[rSOILWAT2]{setup_time_simulation_run}}
setup_time_simulation_project <- function(sim_time, add_st2 = FALSE,
  adjust_NS = FALSE, use_doy_range = FALSE, doy_ranges = list()) {

  sim_time <- rSOILWAT2::setup_time_simulation_run(sim_time = sim_time)

  if (is.matrix(sim_time[["future_yrs"]])) {
    stopifnot(dim(sim_time[["future_yrs"]])[2] == 3)

  } else if (is.list(sim_time[["future_yrs"]]) &&
    all(lengths(sim_time[["future_yrs"]]) == 3)) {

    ctemp <- c("delta", "DSfut_startyr", "DSfut_endyr")
    temp <- matrix(unlist(sim_time[["future_yrs"]]), ncol = length(ctemp),
      byrow = TRUE, dimnames = list(NULL, ctemp))
    rownames(temp) <- make.names(paste0("d", temp[, "delta"], "yrs"),
      unique = TRUE)
    sim_time[["future_yrs"]] <- temp

  } else {
    stop("'setup_time_simulation_project': incorrect format of 'future_yrs'")
  }

  sim_time[["future_N"]] <- dim(sim_time[["future_yrs"]])[1]

  if (add_st2) {
    sim_time[["sim_time2_North"]] <-
      rSOILWAT2::simTiming_ForEachUsedTimeUnit(sim_time[["useyrs"]],
        sim_tscales = c("daily", "monthly", "yearly"),
        use_doy_range = use_doy_range,
        doy_ranges =  doy_ranges,
        latitude = 90, account_NorthSouth = adjust_NS)

    if (adjust_NS) {
      sim_time[["sim_time2_South"]] <-
        rSOILWAT2::simTiming_ForEachUsedTimeUnit(sim_time[["useyrs"]],
          sim_tscales = c("daily", "monthly", "yearly"),
          use_doy_range = use_doy_range,
          doy_ranges = doy_ranges,
          latitude = -90,
          account_NorthSouth = TRUE)

    } else {
      sim_time[["sim_time2_South"]] <- sim_time[["sim_time2_North"]]
    }
  }

  sim_time
}
