#' Initialize a timing file for a simulation job
#' @export
init_timer <- function(timerfile2) {
  cat(", Time_s, Number", file = timerfile2, sep = "\n")
}

write_timer <- function(timerfile2, label, time_sec = "", number = "") {
  cat(paste(label, time_sec, number, sep = ","), file = timerfile2,
    append = TRUE, sep = "\n")
}

#' Write timing information to the timing file
#' @export
compile_overall_timer <- function(timerfile2, dir_out, workersN = 0,
  runs.completed = 0, scenario_No = 0, ensembles.completed = 0,
  delta.overall = NA, delta.outputDB = NA, delta.check = NA,
  delta.ensembles = NA) {

  if (!is.na(delta.overall)) {
    write_timer(timerfile2, "Time_Total", time_sec = delta.overall)
  }
  if (!is.na(delta.outputDB)) {
    write_timer(timerfile2, "Time_OutputDB", time_sec = delta.outputDB)
  }
  if (!is.na(delta.check)) {
    write_timer(timerfile2, "Time_Check", time_sec = delta.check)
  }
  if (!is.na(delta.ensembles)) {
    write_timer(timerfile2, "Time_Ensembles", time_sec = delta.ensembles)
  }

  times <- dbWork_timing(dir_out)
  if (length(times) > 0) {
    write_timer(timerfile2, "Time_OneRun_Mean", time_sec = mean(times))
    write_timer(timerfile2, "Time_OneRun_SD", time_sec = stats::sd(times))
    write_timer(timerfile2, "Time_OneRun_Median",
      time_sec = stats::median(times))
    write_timer(timerfile2, "Time_OneRun_Min", time_sec = min(times))
    write_timer(timerfile2, "Time_OneRun_Max", time_sec = max(times))
  }

  write_timer(timerfile2, "N_cores", number = workersN)
  write_timer(timerfile2, "N_Runs", number = runs.completed)
  write_timer(timerfile2, "N_SWruns", number = runs.completed * scenario_No)
  write_timer(timerfile2, "N_EnsembleFiles",
    number = if (exists("ensembles.completed")) ensembles.completed else 0)

  invisible(TRUE)
}
