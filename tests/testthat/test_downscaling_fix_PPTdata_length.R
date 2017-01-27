context("Delta-hybrid (mod3) downscaling: fix_PPTdata_length")

# Inputs
ppt0 <- c(0, 0, 1, 0.1, 0, 0.1, 1, 0, 0)

targetLength0a <- length(ppt0)

targetLength1a <- length(ppt0) - 2
targetLength2a <- 1
targetLength3a <- 0

targetLength4a <- length(ppt0) + 2
targetLength5a <- 2 * length(ppt0)
targetLength6a <- 10 * length(ppt0)


test_that("fix_PPTdata_length: result == input", {
	# Target length == length of data --> result is input
	expect_equal(fix_PPTdata_length(ppt0, targetLength0a), ppt0)
})

test_that("fix_PPTdata_length: adjust length", {
	# Randomly remove days
	expect_length(fix_PPTdata_length(ppt0, targetLength1a), targetLength1a)
	expect_length(fix_PPTdata_length(ppt0, targetLength2a), targetLength2a)
	expect_length(fix_PPTdata_length(ppt0, targetLength3a), targetLength3a)

	# Randomly add days
	expect_length(fix_PPTdata_length(ppt0, targetLength4a), targetLength4a)
	expect_length(fix_PPTdata_length(ppt0, targetLength5a), targetLength5a)
	expect_length(fix_PPTdata_length(ppt0, targetLength6a), targetLength6a)
})

N <- 1e5
tol = 0.01 * mean(ppt0)

test_that("fix_PPTdata_length: maintain mean and variance of data", {
	# Randomly remove all but one day
	#	- The mean is maintained
	expect_equal(
		mean(replicate(N, fix_PPTdata_length(ppt0, 1))),
		mean(ppt0), tolerance = tol)
	#	- The variance is on average maintained
	expect_equal(
		mean(replicate(N, var(fix_PPTdata_length(ppt0, 3)))),
		var(ppt0), tolerance = tol)

	# Randomly add lots of days
	#	- The mean is maintained
	expect_equal(
		mean(fix_PPTdata_length(ppt0, N)),
		mean(ppt0), tolerance = tol)
	#	- TODO: The variance is on average NOT maintained for the same length of data
	expect_equal(
		mean(replicate(N, var(fix_PPTdata_length(ppt0, 10 * length(ppt0))))),
		var(ppt0) * (1 - 1 / length(ppt0)), tolerance = tol)
})


if (FALSE) {
	reps <- 2^(0:10)
	n <- sample(50, 1)
	dats <- stats::runif(n)
	vars <- sapply(reps, function(i) var(rep(dats, i)))

	plot(reps, vars, ylim = c(0, max(vars)), xlab = "# data repeats", ylab = "Variance")
	var0l <- c(var(dats), vars[length(vars)])
	abline(h = var0l, col = "gray", lty = 2)

	graphics::mtext(side = 3, text = paste("var(data) =", signif(var0l[1], 3), "vs.",
								"var(limit) =", signif(var0l[2], 3), "\n",
								"#(data) =", n, ": var(data) / n =", signif(var0l[1] / n, 3), "vs.",
								"diff(var0l) =", abs(signif(diff(var0l), 3))))
}
