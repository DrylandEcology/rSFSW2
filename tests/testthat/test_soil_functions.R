context("Soil functions")

soil_swdat <- data.frame(
  depth_cm = c(5, 10, 20, 30, 40, 60, 80, 85),
  `bulkDensity_g/cm^3` = c(1.43, 1.41, 1.39, 1.39, 1.38, 1.15, 1.31, 1.31),
  gravel_content = c(0, 0, 0, 0, 0, 0, 0, 0),
  EvapBareSoil_frac = c(0.812, 0.153, 0.034, 0, 0, 0, 0, 0),
  transpGrass_frac = c(0.033, 0.033, 0.067, 0.067, 0.067, 0.133, 0.133, 0.133),
  transpShrub_frac = c(0.134, 0.094, 0.176, 0.175, 0.11, 0.179, 0.101, 0.030),
  transpTree_frac = c(0.033, 0.033, 0.067, 0.067, 0.067, 0.133, 0.133, 0.133),
  transpForb_frac = c(0.134, 0.094, 0.176, 0.175, 0.11, 0.179, 0.101, 0.030),
  sand_frac = c(0.51, 0.44, 0.35, 0.32, 0.31, 0.32, 0.57, 0.57),
  clay_frac = c(0.15, 0.26, 0.41, 0.45, 0.47, 0.47, 0.28, 0.28),
  impermeability_frac = c(0, 0, 0, 0, 0, 0, 0, 0),
  soilTemp_c = c(0.186, 0.372, 0.744, 1.116, 1.488, 2.232, 2.975, 2.975),
  row.names = NULL
)

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
