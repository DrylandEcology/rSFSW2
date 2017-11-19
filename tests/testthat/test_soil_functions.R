context("Soil functions")

#--- INPUTS
print_debug <- FALSE
vars <- c("ld", "sp", "cp", "md")
get_siteN <- function(x) if (is.null(dim(x))) 1L else dim(x)[1]
get_layerN <- function(x) if (is.null(dim(x))) length(x) else dim(x)[2]

layers_depth <- list(
  c(5),
  c(5, 10, 15, 30),
  c(5, 10, 30, 50),
  c(15, 50),
  c(50),
  c(200),
  c(5, NA, 30, 50),
  c(0, 5, 30, 50),
  c(-5, 5, 30),
  c(1.5, 10, 30))

sites_Nmax <- 5
lyrs_N <- sapply(layers_depth, get_layerN)
lyrs_Nmax <- max(lyrs_N)

depth_max_bs_evap_cm <- c(-5, 0, 1.5, 5, 15, 200, NA)

sand <- list(
  rep(NA, lyrs_Nmax),
  rep(0, lyrs_Nmax),
  rep(0, lyrs_Nmax - 1),
  rep(1, lyrs_Nmax),
  rep(0.75, lyrs_Nmax),
  rep(0.1, lyrs_Nmax),
  matrix(0.5, nrow = sites_Nmax, ncol = lyrs_Nmax))

clay <- list(
  rep(NA, lyrs_Nmax),
  rep(0, lyrs_Nmax),
  rep(0, lyrs_Nmax - 1),
  rep(1, lyrs_Nmax),
  rep(0.75, lyrs_Nmax),
  rep(0.1, lyrs_Nmax),
  matrix(0.2, nrow = sites_Nmax, ncol = lyrs_Nmax))

#--- TESTS
k <- 1
test_that("Bare-soil evaporation coefficients", {
  for (k1 in seq_along(layers_depth)) {
    for (k2 in seq_along(sand)) {
      for (k3 in seq_along(depth_max_bs_evap_cm)) {
        ld <- layers_depth[[k1]]
        sp <- sand[[k2]][min(get_siteN(sand[[k2]]), seq_len(lyrs_N[k1]))]
        cp <- clay[[k2]][min(get_siteN(clay[[k2]]), seq_len(lyrs_N[k1]))]
        md <- depth_max_bs_evap_cm[k3]
        Ns <- get_siteN(sp)
        Nl <- get_layerN(sp)

        info <- paste0("Test #", k, ": ", k1, k2, k3, ": input = ",
          paste(lapply(vars, function(x) {
            temp <- get(x)
            paste(x, "=", paste(temp, collapse = "-"))
          }), collapse = " / "))

        if (anyNA(ld) || anyNA(sp) || anyNA(cp) || anyNA(md) ||
            Nl < length(ld) || Ns != get_siteN(cp) || Nl != get_layerN(cp) ||
            any(md < 0) || any(ld <= 0) || any(sp < 0) || any(cp < 0) || any(sp > 1) ||
            any(cp > 1) || any(sp + cp > 1)) {

          if (print_debug) {
            print(paste0(k1, k2, k3, ": ", info, ": expect error"))
          }
          expect_error(calc_BareSoilEvaporationCoefficientsFromSoilTexture(ld, sp, cp, md),
            info = info)
        } else {
          bsevap_coeff <- calc_BareSoilEvaporationCoefficientsFromSoilTexture(ld, sp, cp, md)

          # Coeffs of each site sum to one
          expect_equal(apply(bsevap_coeff, 1, sum), rep(1, Ns), info = info)
          # Coeffs are between 0 and 1
          expect_equal(as.vector(bsevap_coeff <= 1), rep(TRUE, Ns * Nl), info = info)
          expect_equal(as.vector(bsevap_coeff >= 0), rep(TRUE, Ns * Nl), info = info)
          # If max is shallower than first layer, then first layer is 1
          if (ld[1] >= md) {
            expect_equal(bsevap_coeff[, 1], rep(1, Ns))
          }
          # Monotonic decrease with soil depth
          if (Ns * Nl > 1) {
            deltas <- as.vector(apply(sweep(bsevap_coeff, 2, ld, FUN = "/"), 1, diff))
            expect_equal(deltas <= 0, rep(TRUE, Ns * Nl - 1L))
          }
          # No bare-soil evaporation from depths greater than 'depth_max_bs_evap_cm'
          lmax <- max(1, min(Nl, findInterval(md, c(0, ld))))
          expect_equal(apply(bsevap_coeff, 1, function(x) sum(x > 0)) <= rep(lmax, Ns),
            rep(TRUE, Ns), info = info)

          if (print_debug) {
            print(paste0(k1, k2, k3, ": ", info, ": bsevap = ", paste(bsevap_coeff,
              collapse = ":")))
          }
        }

        k <- k + 1
      }
    }
  }

})


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
