context("Delta-hybrid (mod3) downscaling: applyPPTdelta_simple and applyPPTdelta_detailed")

# Inputs
seed <- 42
years <- 1979:2010
this_year <- years[1]

daily_months <- 1 + as.POSIXlt(seq(ISOdate(years[1], 1, 1), ISOdate(years[length(years)], 12, 31), by = "day"))$mon
month <- as.POSIXlt(paste(this_year, 1:365, sep = "-"), format = "%Y-%j")$mon + 1
iadd <- rep(TRUE, length(month))
inotadd <- rep(FALSE, length(month))
iadd6 <- iadd
	iadd6[month <= 6] <- FALSE

daily <- readRDS("dailySW.rds")
monthly <- dbW_weatherData_to_monthly(dailySW = daily)
data0 <- daily[[as.character(this_year)]]@data[, "PPT_cm"]

ydelta0 <- c(1.221, 1.017, 0.844, 1.514, 1.283, 1.475, 0.788, 0.679, 1.093, 2.111, 1.765, 1.113)[month]
ydelta1 <- rep(-1, 12)[month]
ydelta2 <- rep(-10, 12)[month]
ydelta3 <- c(rep(1, 6), rep(-10, 6))[month]

# Expected results
ievents0 <- data0 > 0
events_per_month0 <- tapply(as.integer(ievents0), month, sum)[month]

res0s1 <- data0 * ydelta0
res0s2 <- data0
	res0s2[ievents0] <- res0s2[ievents0] + ydelta0[ievents0] / events_per_month0[ievents0]
res0s3 <- data0
	res0s3[ievents0] <- res0s3[ievents0] + ydelta1[ievents0] / events_per_month0[ievents0]
res0s4 <- data0
	res0s4[ievents0] <- res0s4[ievents0] + ydelta2[ievents0] / events_per_month0[ievents0]
res0s5 <- data0
	res0s5[ievents0] <- res0s5[ievents0] + ydelta3[ievents0] / events_per_month0[ievents0]
res0s6 <- data0
	res0s6[!iadd6] <- (data0 * ydelta3)[!iadd6]
	res0s6[iadd6 & ievents0] <- res0s6[iadd6 & ievents0] + ydelta3[iadd6 & ievents0] / events_per_month0[iadd6 & ievents0]



test_that("applyPPTdelta_simple", {
	# All deltas are multiplications -> no problem
	expect_equal(
		applyPPTdelta_simple(m = month, data = data, ydelta = ydelta0, add_days = inotadd, mult_days = !inotadd)
		, res0s1)

	# All deltas are additions -> no problem
	expect_equal(
		applyPPTdelta_simple(m = month, data = data, ydelta = ydelta0, add_days = iadd, mult_days = !iadd)
		, res0s2)
	# Deltas are subtractions, but are smaller than precipitation events
	expect_equal(
		applyPPTdelta_simple(m = month, data = data, ydelta = ydelta1, add_days = iadd, mult_days = !iadd)
		, res0s3)
	# Deltas are subtractions, but some are larger than precipitation events causing negative precipitation
	expect_equal(
		applyPPTdelta_simple(m = month, data = data, ydelta = ydelta2, add_days = iadd, mult_days = !iadd)
		, res0s4)
	# Some deltas are additions, some are subtractions
	expect_equal(
		applyPPTdelta_simple(m = month, data = data, ydelta = ydelta3, add_days = iadd, mult_days = !iadd)
		, res0s5)

	# Some deltas are additions/subtractions, some are multiplications
	expect_equal(
		applyPPTdelta_simple(m = month, data = data, ydelta = ydelta3, add_days = iadd6, mult_days = !iadd6)
		, res0s6)
})


test_that("applyPPTdelta_detailed", {
	# All deltas are multiplications -> no problem
	set.seed(seed)
	temp <- applyPPTdelta_detailed(m = month, data = data, ydelta = ydelta0, add_days = inotadd, mult_days = !inotadd, daily, monthly, daily_months)
	expect_equal_to_reference(temp, "test_reference_applyPPTdelta_detailed_01.rds")
	expect_gte(temp, 0)

	# All deltas are additions -> no problem
	set.seed(seed)
	temp <- applyPPTdelta_detailed(m = month, data = data, ydelta = ydelta0, add_days = iadd, mult_days = !iadd, daily, monthly, daily_months)
	expect_equal_to_reference(temp, "test_reference_applyPPTdelta_detailed_02.rds")
	expect_gte(temp, 0)
	# Deltas are subtractions, but are smaller than precipitation events
	set.seed(seed)
	temp <- applyPPTdelta_detailed(m = month, data = data, ydelta = ydelta1, add_days = iadd, mult_days = !iadd, daily, monthly, daily_months) 
	expect_equal_to_reference(temp, "test_reference_applyPPTdelta_detailed_03.rds")
	expect_gte(temp, 0)
	# Deltas are subtractions, but some are larger than precipitation events causing negative precipitation
	set.seed(seed)
	temp <- applyPPTdelta_detailed(m = month, data = data, ydelta = ydelta2, add_days = iadd, mult_days = !iadd, daily, monthly, daily_months)
	expect_equal_to_reference(temp, "test_reference_applyPPTdelta_detailed_04.rds")
	expect_gte(temp, 0)
	# Some deltas are additions, some are subtractions
	set.seed(seed)
	temp <- applyPPTdelta_detailed(m = month, data = data, ydelta = ydelta3, add_days = iadd, mult_days = !iadd, daily, monthly, daily_months) 
	expect_equal_to_reference(temp, "test_reference_applyPPTdelta_detailed_05.rds")
	expect_gte(temp, 0)

	# Some deltas are additions/subtractions, some are multiplications
	set.seed(seed)
	temp <- applyPPTdelta_detailed(m = month, data = data, ydelta = ydelta3, add_days = iadd6, mult_days = !iadd6, daily, monthly, daily_months)
	expect_equal_to_reference(temp, "test_reference_applyPPTdelta_detailed_06.rds")
	expect_gte(temp, 0)
})
