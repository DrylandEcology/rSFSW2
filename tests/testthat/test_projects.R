context("Test projects")

# See repository 'rSFSW2_tools' for a more comprehensive set of test projects
# current directory is assumed to be "rSFSW2/tests/testthat"


test_that("Test projects", {
  skip_on_cran()

  suppressMessages(
    tp <- try(run_test_projects(
        dir_tests = file.path("..", "test_data", "Test4_AllOverallAggregations_snow"),
        dir_prj_tests = ".", dir_ref = file.path("..", "test_data", "0_ReferenceOutput"),
        dir_prev = ".", which_tests_torun = 1, delete_output = TRUE,
        force_delete_output = TRUE, make_new_ref = FALSE, write_report_to_disk = FALSE)
      , silent = TRUE)
  )

  expect_false(inherits(tp, "try-error"))
  expect_true(all(tp[["res"]][, "has_run"]))
  expect_false(any(tp[["res"]][, "has_problems"]))
  expect_null(tp[["report"]])
})

