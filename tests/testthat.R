library("testthat")
library("rSFSW2")


# Run package tests
test_check("rSFSW2", reporter = ListReporter, encoding = "UTF-8")
