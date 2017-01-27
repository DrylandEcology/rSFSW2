context("Delta-hybrid (mod3) downscaling: add_delta_to_PPT")

# Inputs
ppt0 <- c(0, 0, 1, 0.1, 0, 0.1, 1, 0, 0)
rainyElems <- ppt0 > 0
rainyElems_int <- which(ppt0 > 0)
delta1a <- 1
delta2a <- -0.4
delta3a <- -1
delta4a <- -4
delta1b <- rep(delta1a / sum(rainyElems), sum(rainyElems))
delta2b <- rep(delta2a / sum(rainyElems), sum(rainyElems))
delta3b <- rep(delta3a / sum(rainyElems), sum(rainyElems))
delta4b <- rep(delta4a / sum(rainyElems), sum(rainyElems))
delta5b <- c(1, -0.1, -0.1 -1 -0.5, -0.5)
delta6b <- c(-1, -0.1, -0.1 -1 -0.5, -0.5)

# Expected results
ppt1 <- c(0, 0, 1.25, 0.35, 0, 0.35, 1.25, 0, 0)
ppt2 <- c(0, 0, 0.9, 0, 0, 0, 0.9, 0, 0)
ppt3 <- c(0, 0, 0.6, 0, 0, 0, 0.6, 0, 0)
ppt4 <- rep(0, length(ppt0))
ppt5 <- c(0, 0, 1, 0, 0, 0, 0, 0, 0)


test_that("add_delta_to_PPT: errors", {
	# Calling with both 'delta' arguments NULL should fail
	expect_error(
		add_delta_to_PPT(data = ppt, ind_events = rainyElems, addDelta = NULL, deltaPerEvent = NULL)
		, NULL)
	# Calling with both 'delta' arguments should fail too.
	expect_error(
		add_delta_to_PPT(data = ppt0, ind_events = rainyElems, addDelta = delta1a, deltaPerEvent = delta1a)
		, NULL)
})

test_that("add_delta_to_PPT: calls with argument 'addDelta'", {
	# All deltas are additions -> no problem; logical precip-day index
	expect_equal(
		add_delta_to_PPT(data = ppt0, ind_events = rainyElems, addDelta = delta1a)
		, list(data = ppt1, PPT_to_remove = 0))
	# Integer precip-day index
	expect_equal(
		add_delta_to_PPT(data = ppt0, ind_events = rainyElems_int, addDelta = delta1a)
		, list(data = ppt1, PPT_to_remove = 0))
	# No precip-day index
	expect_equal(
		add_delta_to_PPT(data = ppt0, ind_events = NULL, addDelta = delta1a)
		, list(data = ppt1, PPT_to_remove = 0))

	# There are subtractions, but all positive
	expect_equal(
		add_delta_to_PPT(data = ppt0, ind_events = rainyElems, addDelta = delta2a)
		, list(data = ppt2, PPT_to_remove = 0))
	# There is enough precipitation from which to subtract when shared among all (already adjusted) rainy days
	expect_equal(
		add_delta_to_PPT(data = ppt0, ind_events = rainyElems, addDelta = delta3a)
		, list(data = ppt3, PPT_to_remove = 0))
	# There is not enough precipitation overall from which to subtract --> all days will be 0
	expect_equal(
		add_delta_to_PPT(data = ppt0, ind_events = rainyElems, addDelta = delta4a)
		, list(data = ppt4, PPT_to_remove = -1.8))
})


test_that("add_delta_to_PPT: calls with argument 'deltaPerEvent'", {
	# All deltas are additions -> no problem
	expect_equal(
		add_delta_to_PPT(data = ppt0, ind_events = rainyElems, deltaPerEvent = delta1b)
		, list(data = ppt1, PPT_to_remove = 0))
	# There are subtractions, but all positive
	expect_equal(
		add_delta_to_PPT(data = ppt0, ind_events = rainyElems, deltaPerEvent = delta2b)
		, list(data = ppt2, PPT_to_remove = 0))
	# There is enough precipitation from which to subtract when shared among all (already adjusted) rainy days
	expect_equal(
		add_delta_to_PPT(data = ppt0, ind_events = rainyElems, deltaPerEvent = delta3b)
		, list(data = ppt3, PPT_to_remove = 0))
	# There is not enough precipitation overall from which to subtract --> all days will be 0
	expect_equal(
		add_delta_to_PPT(data = ppt0, ind_events = rainyElems, deltaPerEvent = delta4b)
		, list(data = ppt4, PPT_to_remove = -1.8))
	# There is enough precipitation from which to subtract when shared among all (already adjusted) rainy days
	# This uses 3 recursive calls
	expect_equal(
		add_delta_to_PPT(data = ppt0, ind_events = rainyElems, deltaPerEvent = delta5b)
		, list(data = ppt5, PPT_to_remove = 0))
	# There is not enough precipitation overall from which to subtract --> all days will be 0
	# This uses 1 recursive calls
	expect_equal(
		add_delta_to_PPT(data = ppt0, ind_events = rainyElems, deltaPerEvent = delta6b)
		, list(data = ppt4, PPT_to_remove = -1))
})
