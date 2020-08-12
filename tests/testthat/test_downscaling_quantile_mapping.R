context("Quantile mapping for downscaling climate data")

#---TESTS
# cannot reliably test option "attempt" because not predicatable when it fails
fix_spline <- c(NA, "fail", "none")
type <- c("linear_Boe", "linear_Thermessl2012CC.QMv1b", "linear_none",
  "tricub_fmm", "tricub_monoH.FC", "tricub_natural", "normal_anomalies")

qmaps <- list(
  qmap::fitQmapQUANT(1:10, 5:15, wet.day = FALSE),
  qmap::fitQmapQUANT(c(rep(1, 10), 11:20), 5:15, wet.day = FALSE),
  qmap::fitQmapQUANT(1:10, c(rep(1, 10), 5:15), wet.day = FALSE),
  qmap::fitQmapQUANT(1:10, rep(0, 10), wet.day = FALSE)
)
# to plot:
if (FALSE) {
  k2 <- 3
  graphics::plot(qmaps[[k2]][["par"]][["modq"]], qmaps[[k2]][["par"]][["fitq"]])
}

xs <- list(inter = 5:15, extra = 0:30)

tests <- rbind(
  expand.grid(fix_spline = fix_spline[1],
    type = grep("tricub", type, invert = TRUE, value = TRUE),
    stringsAsFactors = FALSE),
  expand.grid(fix_spline = fix_spline[-1],
    type = grep("tricub", type, invert = FALSE, value = TRUE),
    stringsAsFactors = FALSE))

set.seed(124)
test_that("Test applying a quantile map", {
  for (k0 in seq_along(qmaps)) {
    qm <- qmaps[[k0]]
    fitq <- qm[["par"]][["fitq"]]
    range_fitq <- c(min(fitq) - 1e-3, max(fitq) + 1e-3)
    has_novar <- as.vector(var(qm[["par"]][["modq"]]) < .Machine$double.eps)

    for (k1 in seq_along(xs)) {
      x <- xs[[k1]]
      for (k2 in seq_len(dim(tests)[1])) {
        info <- paste("qm =", k0, "/ x =", k1, "/ t =", k2)
        do_args <- list(x = x, fobj = qm, type_map = tests[k2, "type"],
          monthly_obs_base = x, monthly_extremes = range(x),
          fix_spline = tests[k2, "fix_spline"])

        if (has_novar) {
          expect_message(do.call("doQmapQUANT_drs", do_args),
            "values are identical", info = info)
          expect_equal(do.call("doQmapQUANT_drs", do_args), rep(mean(fitq),
            length(x)), info = info)

        } else if (identical(tests[k2, "fix_spline"], "fail") &&
            !(k0 == 3 && k1 == 1)) {
          expect_error(do.call("doQmapQUANT_drs", do_args), info = info)

        } else {
          expect_silent(res <- do.call("doQmapQUANT_drs", do_args))
          expect_type(res, "double")
          expect_false(anyNA(res))

          if (!(tests[k2, "type"] %in% c("tricub_fmm", "tricub_natural"))) {
            if (identical(names(xs)[k1], "inter") ||
                identical(tests[k2, "type"], "linear_none")) {
              # No extrapolation requested by 'x' or
              # no extrapolation provided by 'type' as 'linear_none' and
              # not non-monotonic splines (which extrapolate)
              expect_gte(min(res), range_fitq[1])
              expect_lte(max(res), range_fitq[2])

            } else if (identical(names(xs)[k1], "extra")) {
              # Extrapolation occurred at smallest and/or largest values
              expect_true(min(res) < range_fitq[1] || max(res) > range_fitq[2])
            }
          }
        }
      }
    }
  }
})
