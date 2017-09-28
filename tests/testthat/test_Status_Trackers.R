context("Project status trackers")

#---INPUTS
req_cols <- c("prepared", "prep_time", "checked", "check_time")
req_rows <- c("load_inputs", "calc_size", "spatial_setup", "prj_todos", "rng_setup", "dbWork",
    "dbW_paths", "dbW_sources", "dbW_current", "dbW_scenarios", "soil_data", "elev_data",
    "climnorm_data", "req_soillayers", "calc_bsevap", "table_lookup")

#---TESTS
test_that("Status tracker", {
  m <- list()

  # Initialize a status tracker
  expect_silent(m[["input_status"]] <- init_intracker())
  expect_true(all(!m[["input_status"]]))
  expect_equal(colnames(m[["input_status"]]), req_cols)
  expect_equal(rownames(m[["input_status"]]), req_rows)

  # Check whether a specific status needs still to be completed
  expect_true(todo_intracker(m, tracker = "load_inputs", status = "prepared"))
  expect_true(todo_intracker(m, tracker = "load_inputs", status = "checked"))
  expect_false(todo_intracker(m, tracker = "tracker_doesnot_exist", status = "prepared"))
  expect_warning(todo_intracker(m, tracker = "load_inputs", status = "status_doesnot_exist"))

  # Check whether a specific status is already completed
  expect_false(isdone_intracker(m, tracker = "load_inputs", status = "prepared"))
  expect_false(isdone_intracker(m, tracker = "load_inputs", status = "checked"))
  expect_false(isdone_intracker(m, tracker = "tracker_doesnot_exist", status = "prepared"))
  expect_warning(isdone_intracker(m, tracker = "load_inputs", status = "status_doesnot_exist"))

  # Update trackers: prepared, checked, and with 'clean_subsequent
  st <- m[["input_status"]]
  st <- update_intracker(st, tracker = "load_inputs", prepared = TRUE)
  st <- update_intracker(st, tracker = "dbWork", prepared = TRUE, checked = TRUE)
  st <- update_intracker(st, tracker = "calc_size", checked = TRUE)
  st <- update_intracker(st, tracker = "dbW_scenarios", prepared = TRUE)
  st <- update_intracker(st, tracker = "dbW_current", prepared = TRUE, clean_subsequent = TRUE)
  m[["input_status"]] <- st

  # Check whether a specific status needs still to be completed
  expect_true(isdone_intracker(m, tracker = "load_inputs", status = "prepared"))
  expect_false(isdone_intracker(m, tracker = "load_inputs", status = "checked"))
  expect_true(isdone_intracker(m, tracker = "dbWork", status = "prepared"))
  expect_true(isdone_intracker(m, tracker = "dbWork", status = "checked"))
  expect_false(isdone_intracker(m, tracker = "calc_size", status = "prepared"))
  expect_true(isdone_intracker(m, tracker = "calc_size", status = "checked"))
  expect_false(isdone_intracker(m, tracker = "dbW_scenarios", status = "prepared"))
  expect_false(isdone_intracker(m, tracker = "dbW_scenarios", status = "checked"))
  expect_true(isdone_intracker(m, tracker = "dbW_current", status = "prepared"))
  expect_false(isdone_intracker(m, tracker = "dbW_current", status = "checked"))
})
