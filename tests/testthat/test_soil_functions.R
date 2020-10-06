context("Soil functions")

soil_swdat <- structure(c(5, 10, 20, 30, 40, 60, 80, 85, 1.43, 1.41, 1.39,
1.39, 1.38, 1.15, 1.31, 1.31, 0, 0, 0, 0, 0, 0, 0, 0, 0.812,
0.153, 0.034, 0, 0, 0, 0, 0, 0.033, 0.033, 0.067, 0.067, 0.067,
0.133, 0.133, 0.133, 0.134, 0.094, 0.176, 0.175, 0.11, 0.179,
0.101, 0.030, 0.033, 0.033, 0.067, 0.067, 0.067, 0.133, 0.133,
0.133, 0.134, 0.094, 0.176, 0.175, 0.11, 0.179, 0.101, 0.030,
0.51, 0.44, 0.35, 0.32, 0.31, 0.32, 0.57, 0.57, 0.15, 0.26, 0.41,
0.45, 0.47, 0.47, 0.28, 0.28, 0, 0, 0, 0, 0, 0, 0, 0, 0.186,
0.372, 0.744, 1.116, 1.488, 2.232, 2.975, 2.975), .Dim = c(8L,
12L), .Dimnames = list(NULL, c("depth_cm", "matricd", "gravel_content",
"EvapBareSoil_frac", "transpGrass_frac", "transpShrub_frac",
"transpTree_frac", "transpForb_frac", "sand", "clay", "imperm",
"soilTemp_c")))

test_that("Check soils", {
  soildat <- soil_swdat
  expect_true(all(check_soil_data(soildat)))
  expect_true(check_soilco(soildat[, "EvapBareSoil_frac"]))
  expect_true(all(apply(soildat[, c("transpGrass_frac", "transpShrub_frac",
      "transpTree_frac", "transpForb_frac"), drop = FALSE], 2, check_soilco)))

  soildat[3, c("EvapBareSoil_frac", "transpGrass_frac", "transpShrub_frac",
      "transpTree_frac", "transpForb_frac")] <- 1
  expect_true(all(check_soil_data(soildat)))
  expect_false(check_soilco(soildat[, "EvapBareSoil_frac"]))
  expect_false(all(apply(soildat[, c("transpGrass_frac", "transpShrub_frac",
      "transpTree_frac", "transpForb_frac"), drop = FALSE], 2, check_soilco)))

  soildat <- soil_swdat
  soildat[1, 1] <- NA
  expect_false(all(check_soil_data(soildat)))

  soildat <- soil_swdat
  soildat[1, 1] <- -1
  expect_false(all(check_soil_data(soildat)))

  soildat <- soil_swdat
  soildat[1, 1] <- 0
  expect_false(all(check_soil_data(soildat)))
})
