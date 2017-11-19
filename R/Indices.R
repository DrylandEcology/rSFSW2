#' Index functions
#'
#' @param isim An integer vector. A subset of \code{runIDs_todo} which is a subset of
#'  \code{runIDs_total}, i.e., a consecutive index across loop1 and loop2b
#' @param sc An integer value. The index along loop3 \code{1...scN}.
#' @param scN An integer value. The number of (climate) scenarios used in the project,
#'  i.e., \eqn{scN == sim_scens[["N"]]}.
#' @param runN An integer value. The number of runs/sites set up in the master input file,
#'  i.e., \eqn{runN == runsN_master}.
#' @param iexp An integer value. The index value along loop1 \code{1...expN}.
#' @param isite An integer value. The index value along loop2b \code{1...runsN_sites}.
#' @param pid An integer value. The index value across loop1, loop2b, and loop3 combined,
#'  i.e., a unique consecutive identification number across all simulation runs.
#'
#' @section NOTE:
#'  Do not change the indices without adjusting the design of the output databases!
#'
#' @section Simulation runs: \itemize{
#'  \item Simulations are run over three nested loops \itemize{
#'    \item loop1 (1...expN) nested in loop2b (1...runsN_sites) nested in loop3 (1...scN)
#'        - Note: loop3 (along scenarios) occurs within the function 'do_OneSite'
#'        - Note: loop2b is a subset of loop2a (1...runsN_master)
#'    \item column 'include_YN' reduces 'site_id' to 'runIDs_sites'
#'    \item 'site_id' and 'P_id' are invariant to 'include_YN'}
#'
#'  \item Master input file: column 'include_YN' selects rows which are included in the
#'    simulation \itemize{
#'    \item Note: rows of the master input file correspond to rows of the treatment input file
#'    \item column 'site_id' == consecutive identification numbers of all rows in the master
#'      file; this is treated as a unique (and stable) identifier of a site
#'    \item runsN_master == number of rows in the master file
#'    \item runIDs_master == consecutive identification numbers along runsN_master
#'    \item runsN_sites == number of rows in the master file that are included;
#'      runsN_sites <= max(site_id) and runsN_sites == length(runIDs_sites)
#'    \item runIDs_sites == values of runIDs_master which are included}
#'
#'  \item Experimental input file: each row defines a condition which is applied to every
#'    runIDs_sites \itemize{
#'    \item expN == number of experimental treatments}
#'
#'  \item The function 'do_OneSite' will be called n-times with n = runsN_call \itemize{
#'    \item runsN_job == (number of included sites) x (number of experimental treatments)
#'      == runsN_sites x expN
#'    \item runIDs_job == consecutive identification numbers along runsN_job
#'    \item runsN_total == (number of sites) x (number of experimental treatments)
#'      == runsN_master x expN
#'    \item runIDs_total == consecutive identification numbers along runsN_total
#'    \item runIDs_done == values of runIDs_total that have already been processed by
#'      'do_OneSite'
#'    \item runIDs_todo == values of runIDs_total that await simulation by 'do_OneSite'
#'    \item runsN_todo == number of runIDs_total that await simulation by 'do_OneSite'}
#'
#'  \item The function 'do_OneSite' could be called n-times with n = runsN_incl if all
#'    'include_YN' were on \itemize{
#'    \item runsN_incl == runsN_total}
#'
#'  \item The variable 'climate.conditions' defines climate conditions that are applied to
#'    each 'runIDs_total' \itemize{
#'    \item scN == number of climate conditions}
#'
#'  \item A grand total of n = runsN_Pid rSFSW2 runs could be carried out (n == number of
#'    rows in the output database) \itemize{
#'    \item runsN_Pid == max(P_id) == runsN_total x scN == runsN_master x expN x scN
#'    \item P_id == a unique consecutive identification number for each possible rSFSW2
#'      simulation; used as the ID for the output database}
#' }
#'
#' @aliases it_exp it_site it_Pid
#' @name indices
NULL

#' @rdname indices
#' @return The function \code{it_exp} returns an integer value of the index in loop 1
#'  based on the position across loops 1+2b; invariant to \code{include_YN}.
#' @export
it_exp <- function(isim, runN) {
  stopifnot(sapply(list(isim, runN), is.natural))
  (isim - 1L) %/% runN + 1L
}
#' @rdname indices
#' @export
it_exp2 <- function(pid, runN, scN) {
  stopifnot(sapply(list(pid, runN, scN), is.natural))
  it_exp(isim = it_sim2(pid, scN), runN)
}

#' @rdname indices
#' @return The function \code{it_site} returns an integer value out of 'runIDs_sites',
#'   i.e., the position in loop 2b based on position across loops 1+2b; invariant to
#'   \code{include_YN}.
#' @export
it_site <- function(isim, runN) {
  stopifnot(sapply(list(isim, runN), is.natural))
  (isim - 1L) %% runN + 1L
}
#' @rdname indices
#' @export
it_site2 <- function(pid, runN, scN) {
  stopifnot(sapply(list(pid, runN, scN), is.natural))
  it_site(isim = it_sim2(pid, scN), runN)
}

#' @rdname indices
#' @return The function \code{it_Pid} returns an integer value of the index across all
#'  loops 1+2a+3, invariant to \code{include_YN}. A consecutive identification number
#'  for each possible rSFSW2 simulation--used as the ID for the output database.
#' @export
it_Pid <- function(isim, runN, sc, scN) {
  stopifnot(sapply(list(isim, runN, sc, scN), is.natural))
  (isim - 1L) * scN + sc
}
#' @rdname indices
#' @export
it_Pid0 <- function(iexp, isite, runN, sc, scN) {
  stopifnot(sapply(list(iexp, isite, runN, sc, scN), is.natural))
  it_Pid(isim = it_sim0(iexp, isite, runN), runN, sc, scN)
}

#' @rdname indices
#' @export
it_sim0 <- function(iexp, isite, runN) {
  stopifnot(sapply(list(iexp, isite, runN), is.natural))
  (iexp - 1L) * runN + isite
}

#' @rdname indices
#' @export
it_sim2 <- function(pid, scN) {
  stopifnot(sapply(list(pid, scN), is.natural))
  1L + (pid - 1L) %/% scN
}

#' @rdname indices
#' @export
it_scen2 <- function(pid, scN) {
  stopifnot(sapply(list(pid, scN), is.natural))
  1L + (pid - 1L) %% scN
}



#' Calculate the size of the simulation experiment: number of runs, etc.
#'
#' @section Details of indices: \code{\link{indices}}.
#' @seealso \code{\link{indices}}, \code{\link{it_exp}}, \code{\link{it_site}},
#'  \code{\link{it_Pid}}
#' @export
determine_simulation_size <- function(SWRunInformation, include_YN,
  sw_input_experimentals, sim_scens) {

  runsN_master <- dim(SWRunInformation)[1]
  runIDs_sites <- which(include_YN)
  runsN_sites <- length(runIDs_sites)
  if (!(runsN_sites > 0))
    stop(paste("'determine_simulation_size': at least 1 rSFSW2-run needed for ",
      "simulation, but", runsN_sites, "found"))

  # identify how many rSFSW2-runs = rows are to be carried out
  expN <- NROW(sw_input_experimentals)
  runsN_total <- runsN_master * max(expN, 1L)
  runIDs_total <- seq_len(runsN_total) # consecutive number of all possible (tr x exp) simulations
  digitsN_total <- 1 + ceiling(log10(runsN_total))  #max index digits
  runsN_job <- runsN_sites * max(expN, 1L)
  runsN_Pid <- runsN_total * sim_scens[["N"]]

  list(expN = expN, runsN_master = runsN_master, runIDs_sites = runIDs_sites,
    runsN_sites = runsN_sites, runsN_total = runsN_total, runIDs_total = runIDs_total,
    runsN_job = runsN_job, runsN_Pid = runsN_Pid, runIDs_todo = NULL, runsN_todo = 0,
    digitsN_total = digitsN_total
  )
}
