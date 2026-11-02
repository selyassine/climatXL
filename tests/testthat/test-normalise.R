fx <- function(f) testthat::test_path("fixtures", f)

test_that("read_fred moyenne par année et ignore les manquants", {
  cpi <- read_fred(fx("fred_sample.csv"), "cpi")
  expect_equal(cpi$year, c(1995L, 1996L))
  expect_equal(cpi$cpi, c(151.5, mean(c(154.7, 157.3))))
})

test_that("read_noaa_temp lit le CSV Climate at a Glance", {
  t <- read_noaa_temp(fx("noaa_temp_sample.csv"))
  expect_equal(t$year, c(1995L, 1996L, 2024L))
  expect_equal(t$temp_anom[3], 1.29)
})

test_that("read_oni calcule les moyennes et gère les manquants", {
  o <- read_oni(fx("oni_sample.data"))
  expect_equal(o$year, c(1995L, 1996L, 2024L))
  expect_equal(o$enso_jjason[1], mean(c(-0.05, -0.23, -0.42, -0.62, -0.80, -0.92)))
  expect_true(is.na(o$enso[3]))
  expect_true(is.na(o$enso_jjason[3]))   # novembre 2024 manquant
})

test_that("normalisation : facteur 1 l'année de base, inflation seule", {
  idx <- data.frame(year = 2000:2002, cpi = c(100, 110, 120), wealth = c(50, 60, 100))
  f <- normalisation_factors(idx, base_year = 2002, method = "cpi")
  expect_equal(f$factor, c(1.2, 120 / 110, 1))
  n <- normalise_losses(data.frame(year = 2000, loss = 1e6), f)
  expect_equal(n$loss_norm, 1.2e6)
})

test_that("normalisation par la richesse corrige plus que l'inflation seule", {
  idx <- data.frame(year = 2000:2002, cpi = c(100, 110, 120), wealth = c(50, 60, 100))
  fw <- normalisation_factors(idx, 2002, "wealth")
  fc <- normalisation_factors(idx, 2002, "cpi")
  expect_equal(fw$factor[1], 2)
  expect_true(all(fw$factor >= fc$factor))
})

test_that("une année sans facteur déclenche une erreur", {
  f <- data.frame(year = 2000, factor = 1)
  expect_error(normalise_losses(data.frame(year = 1999, loss = 1), f), "1999")
  expect_error(normalisation_factors(data.frame(year = 2000, cpi = 1), 2001, "cpi"))
})
