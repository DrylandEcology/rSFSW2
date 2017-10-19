context("Test projects")

# See repository 'rSFSW2_tools' for a more comprehensive set of test projects
# current directory is assumed to be "rSFSW2/tests/testthat"


test_that("Test projects", {
  skip_on_cran()

  suppressWarnings(
    tp <- try(run_test_projects(
        dir_tests = file.path("..", "test_data", "Test4_AllOverallAggregations_snow"),
        dir_prj_tests = ".", dir_ref = file.path("..", "test_data", "0_ReferenceOutput"),
        dir_prev = ".", which_tests_torun = 1, delete_output = TRUE,
        force_delete_output = TRUE, make_new_ref = FALSE, write_report_to_disk = FALSE)
      , silent = FALSE)
  )
  info_res <- paste(names(tp[["res"]]), "=", format(tp[["res"]]), collapse = " / ")
  info_report <- if (length(tp[["report"]]) > 0) {
      paste0(c("", rep("* ", length(tp[["report"]]) - 1)), tp[["report"]],
        collapse = "\n")
    } else ""

  expect_false(inherits(tp, "try-error"))
  expect_true(all(tp[["res"]][, "has_run"]), info = info_res)
  expect_false(any(tp[["res"]][, "has_problems"]), info = info_res)
  expect_null(tp[["report"]], info = info_report)
})

