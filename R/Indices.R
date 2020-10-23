#' Index functions
#'
#' @param isim An integer vector. A subset of \code{runIDs_todo} which is a
#'   subset of \code{runIDs_total}, i.e., a consecutive index across loop1 and
#'   loop2b
#' @param sc An integer value. The index along loop3 \code{1...scN}.
#' @param scN An integer value. The number of (climate) scenarios used in the
#'   project, i.e., \eqn{scN == sim_scens[["N"]]}.
#' @param runN An integer value. The number of runs/sites set up in the main
#'   input file, i.e., \eqn{runN == runsN_main}.
#' @param iexp An integer value. The index value along loop1 \code{1...expN}.
#' @param isite An integer value. The index value along loop2b
#'   \code{1...runsN_sites}.
#' @param pid An integer value. The index value across loop1, loop2b, and loop3
#'   combined, i.e., a unique consecutive identification number across all
#'   simulation runs.
#'
#' @section NOTE: Do not change the indices without adjusting the design of the
#'   output databases!
#'
#' @section Simulation runs: \itemize{
#'  \item Simulations are run over three nested loops \itemize{
#'    \item loop1 \code{(1...expN)} nested in loop2b \code{(1...runsN_sites)}
#'      nested in loop3 \code{(1...scN)} \itemize{
#'      \item Note: loop3 (along scenarios) occurs within the function
#'        \code{\link{do_OneSite}}
#'      \item Note: loop2b is a subset of loop2a \code{(1...runsN_main)}}
#'    \item column \var{\sQuote{include_YN}} reduces \code{site_id} to
#'      \code{runIDs_sites}
#'    \item \code{site_id} and \code{P_id} are invariant to
#'      \var{\sQuote{include_YN}}}
#'
#'  \item Main input file: column \var{\sQuote{include_YN}} selects rows
#'    which are included in the simulation \itemize{
#'    \item Note: rows of the main input file correspond to rows of the
#'      treatment input file
#'    \item column \code{site_id} == consecutive identification numbers of all
#'      rows in the main file; this is treated as a unique (and stable)
#'      identifier of a site
#'    \item \code{runsN_main} == number of rows in the main file
#'    \item \code{runIDs_main} == consecutive identification numbers along
#'      \code{runsN_main}
#'    \item \code{runsN_sites} == number of rows in the main file that are
#'      included; \code{runsN_sites <= max(site_id)} and
#'      \code{runsN_sites == length(runIDs_sites)}
#'    \item \code{runIDs_sites} == values of \code{runIDs_main} which are
#'      included}
#'
#'  \item Experimental input file: each row defines a condition which is
#'    applied to every \code{runIDs_sites} \itemize{
#'    \item \code{expN} == number of experimental treatments}
#'
#'  \item The function \code{\link{do_OneSite}} will be called n-times with
#'    \code{n = runsN_call} \itemize{
#'    \item \code{runsN_job == (number of included sites) x
#'      (number of experimental treatments) == runsN_sites x expN}
#'    \item \code{runIDs_job} == consecutive identification numbers along
#'      \code{runsN_job}
#'    \item \code{runsN_total == (number of sites) x (number of experimental
#'      treatments) == runsN_main x expN}
#'    \item \code{runIDs_total} == consecutive identification numbers along
#'     \code{runsN_total}
#'    \item \code{runIDs_done} == values of \code{runIDs_total} that have
#'      already been processed by \code{\link{do_OneSite}}
#'    \item \code{runIDs_todo} == values of \code{runIDs_total} that await
#'      simulation by \code{\link{do_OneSite}}
#'    \item \code{runsN_todo} == number of \code{runIDs_total} that await
#'      simulation by \code{\link{do_OneSite}}}
#'
#'  \item The function \code{\link{do_OneSite}} could be called n-times with
#'    \code{n = runsN_incl} if all \var{\sQuote{include_YN}} were on \itemize{
#'    \item \code{runsN_incl} == \code{runsN_total}}
#'
#'  \item The variable \code{climate.conditions} defines climate conditions
#'    that are applied to each \code{runIDs_total} \itemize{
#'    \item \code{scN} == number of climate conditions}
#'
#'  \item A grand total of \code{n = runsN_Pid} \pkg{rSFSW2} runs could be
#'    carried out (\var{n} == number of rows in the output database) \itemize{
#'    \item \code{runsN_Pid == max(P_id) == runsN_total x scN ==
#'       runsN_main x expN x scN}
#'    \item \code{P_id} == a unique consecutive identification number for each
#'      possible \pkg{rSFSW2} simulation; used as the ID for the output
#'      database}
#' }
#'
#' @aliases it_exp it_site it_Pid
#' @name indices
NULL

#' @rdname indices
#' @return The function \code{it_exp} returns an integer value of the index in
#'   loop 1 based on the position across loops 1+2b; invariant to
#'   \code{include_YN}.
#' @export
it_exp <- function(isim, runN) {
  stopifnot(sapply(list(isim, runN), rSW2utils::is.natural))
  (isim - 1L) %/% runN + 1L
}
#' @rdname indices
#' @export
it_exp2 <- function(pid, runN, scN) {
  stopifnot(sapply(list(pid, runN, scN), rSW2utils::is.natural))
  it_exp(isim = it_sim2(pid, scN), runN)
}

#' @rdname indices
#' @return The function \code{it_site} returns an integer value out of
#'   \code{runIDs_sites}, i.e., the position in loop 2b based on position
#'   across loops 1+2b; invariant to \code{include_YN}.
#' @export
it_site <- function(isim, runN) {
  stopifnot(sapply(list(isim, runN), rSW2utils::is.natural))
  (isim - 1L) %% runN + 1L
}
#' @rdname indices
#' @export
it_site2 <- function(pid, runN, scN) {
  stopifnot(sapply(list(pid, runN, scN), rSW2utils::is.natural))
  it_site(isim = it_sim2(pid, scN), runN)
}

#' @rdname indices
#' @return The function \code{it_Pid} returns an integer value of the index
#'   across all loops 1+2a+3, invariant to \code{include_YN}. A consecutive
#'   identification number for each possible \pkg{rSFSW2} simulation--used as
#'   the ID for the output database.
#' @export
it_Pid <- function(isim, runN, sc, scN) {
  stopifnot(sapply(list(isim, runN, sc, scN), rSW2utils::is.natural))
  (isim - 1L) * scN + sc
}
#' @rdname indices
#' @export
it_Pid0 <- function(iexp, isite, runN, sc, scN) {
  stopifnot(sapply(list(iexp, isite, runN, sc, scN), rSW2utils::is.natural))
  it_Pid(isim = it_sim0(iexp, isite, runN), runN, sc, scN)
}

#' @rdname indices
#' @export
it_sim0 <- function(iexp, isite, runN) {
  stopifnot(sapply(list(iexp, isite, runN), rSW2utils::is.natural))
  (iexp - 1L) * runN + isite
}

#' @rdname indices
#' @export
it_sim2 <- function(pid, scN) {
  stopifnot(sapply(list(pid, scN), rSW2utils::is.natural))
  1L + (pid - 1L) %/% scN
}

#' @rdname indices
#' @export
it_scen2 <- function(pid, scN) {
  stopifnot(sapply(list(pid, scN), rSW2utils::is.natural))
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

  runsN_main <- dim(SWRunInformation)[1]
  runIDs_sites <- which(include_YN)
  runsN_sites <- length(runIDs_sites)
  if (!(runsN_sites > 0))
    stop(paste("'determine_simulation_size': at least 1 rSFSW2-run needed for ",
      "simulation, but", runsN_sites, "found"))

  # identify how many rSFSW2-runs = rows are to be carried out
  expN <- NROW(sw_input_experimentals)
  runsN_total <- runsN_main * max(expN, 1L)
  # consecutive number of all possible (tr x exp) simulations
  runIDs_total <- seq_len(runsN_total)
  digitsN_total <- 1 + ceiling(log10(runsN_total))  # max index digits
  runsN_job <- runsN_sites * max(expN, 1L)
  runsN_Pid <- runsN_total * sim_scens[["N"]]

  list(expN = expN, runsN_main = runsN_main, runIDs_sites = runIDs_sites,
    runsN_sites = runsN_sites, runsN_total = runsN_total,
    runIDs_total = runIDs_total, runsN_job = runsN_job, runsN_Pid = runsN_Pid,
    runIDs_todo = NULL, runsN_todo = 0, digitsN_total = digitsN_total
  )
}
