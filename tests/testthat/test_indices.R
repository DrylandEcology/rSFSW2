context("Indices")

#--- Inputs
library("RSQLite")

# Initialization
expN <- 11L
runsN_main <- 17L
runsN_total <- runsN_main * max(expN, 1L)
runIDs_total <- seq_len(runsN_total)
scenario_No <- 5L

# isim are elements of runIDs_todo
runIDs_todo <- list(test0 = integer(0), test1 = 0,
  test2 = runIDs_total, test3 = runIDs_total[1],
  test4 = as.integer(c(7, 18, 20, 24, 35, 48, 68, 76, 97, 101, 113, 130, 137,
    144, 150, 169, 185)))

exp_experiment <- list(test0 = integer(0), test1 = 0,
  test2 = rep(seq_len(expN), each = runsN_main),
  test3 = 1L, test4 = as.integer(c(1, 2, 2, 2, 3, 3, 4, 5, 6, 6, 7, 8, 9, 9, 9,
    10, 11)))

exp_site <- list(test0 = integer(0), test1 = 0,
  test2 = rep(seq_len(runsN_main), expN),
  test3 = 1L, test4 = as.integer(c(7, 1, 3, 7, 1, 14, 17, 8, 12, 16, 11, 11, 1,
    8, 14, 16, 15)))

add_to_pids <- function(atlast, scN) {
  lapply(rev(seq_len(scN)), function(x) atlast - x + 1)
}

exp_pids <- list(test0 = rep(list(integer(0)), scenario_No),
  test1 = rep(list(0), scenario_No),
  test2 = add_to_pids(atlast = scenario_No * runIDs_total, scenario_No),
  test3 = seq_len(scenario_No),
  test4 = add_to_pids(atlast = c(31, 86, 96, 116, 171, 236, 336, 376, 481,
    501, 561, 646, 681, 716, 746, 841, 921) + scenario_No - 1, scenario_No))


#--- Unit tests
test_that("rSFSW2 indices", {
  for (k in seq_along(runIDs_todo)) {

    if (rSW2utils::is.natural(runIDs_todo[[k]])) {
      # Index of experimental treatments (row in experimental design file)
      expect_equal(it_exp(isim = runIDs_todo[[k]], runN = runsN_main),
        exp_experiment[[k]], label = names(runIDs_todo)[k])
      expect_equal(it_exp2(pid = exp_pids[[k]][[1]], runN = runsN_main,
        scN = scenario_No), exp_experiment[[k]], label = names(runIDs_todo)[k])

      # Index of site_id (row in main input file)
      expect_equal(it_site(isim = runIDs_todo[[k]], runN = runsN_main),
        exp_site[[k]], label = names(runIDs_todo)[k])
      expect_equal(it_site2(pid = exp_pids[[k]][[1]], runN = runsN_main,
        scN = scenario_No), exp_site[[k]], label = names(runIDs_todo)[k])

      # Index of simulation runs
      expect_equal(it_sim0(iexp = exp_experiment[[k]], isite = exp_site[[k]],
        runN = runsN_main), runIDs_todo[[k]], label = names(runIDs_todo)[k])
      expect_equal(it_sim2(pid = exp_pids[[k]][[1]], scN = scenario_No),
        runIDs_todo[[k]], label = names(runIDs_todo)[k])

      for (sc in seq_len(scenario_No)) {
        # Index of P_id (unique id in dbOutput)
        expect_equal(it_Pid(isim = runIDs_todo[[k]], runN = runsN_main,
          sc = sc, scN = scenario_No), exp_pids[[k]][[sc]],
          label = paste(names(runIDs_todo)[k], " - scenario =", sc))
        expect_equal(it_Pid0(iexp = exp_experiment[[k]], isite = exp_site[[k]],
          runN = runsN_main, sc = sc, scN = scenario_No), exp_pids[[k]][[sc]],
          label = paste(names(runIDs_todo)[k], " - scenario =", sc))

        # Index of scenario
        expect_equal(it_scen2(pid = exp_pids[[k]][[sc]], scN = scenario_No),
          rep(sc, length(exp_pids[[k]][[sc]])),
          label = paste(names(runIDs_todo)[k], " - scenario =", sc))
      }

    } else {
      # Index of experimental treatments (row in experimental design file)
      expect_error(it_exp(isim = runIDs_todo[[k]], runN = runsN_main),
        label = names(runIDs_todo)[k])
      expect_error(it_exp2(pid = exp_pids[[k]][[1]], runN = runsN_main,
        scN = scenario_No), label = names(runIDs_todo)[k])

      # Index of site_id (row in main input file)
      expect_error(it_site(isim = runIDs_todo[[k]], runN = runsN_main),
        label = names(runIDs_todo)[k])
      expect_error(it_site2(pid = exp_pids[[k]][[1]], runN = runsN_main,
        scN = scenario_No), label = names(runIDs_todo)[k])

      # Index of simulation runs
      expect_error(it_sim0(iexp = exp_experiment[[k]], isite = exp_site[[k]],
        runN = runsN_main), label = names(runIDs_todo)[k])
      expect_error(it_sim2(pid = exp_pids[[k]][[1]], scN = scenario_No),
        label = names(runIDs_todo)[k])

      for (sc in seq_len(scenario_No)) {
        # Index of P_id (unique id in dbOutput)
        expect_error(it_Pid(isim = runIDs_todo[[k]], runN = runsN_main,
          sc = sc, scN = scenario_No),
          label = paste(names(runIDs_todo)[k], " - scenario =", sc))
        expect_error(it_Pid0(iexp = exp_experiment[[k]], isite = exp_site[[k]],
          runN = runsN_main, sc = sc, scN = scenario_No),
          label = paste(names(runIDs_todo)[k], " - scenario =", sc))

        # Index of scenario
        expect_error(it_scen2(pid = exp_pids[[k]][[sc]], scN = scenario_No),
          label = paste(names(runIDs_todo)[k], " - scenario =", sc))
      }
    }
  }
})
