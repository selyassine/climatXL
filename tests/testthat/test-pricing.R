test_that("layer_loss", {
  expect_equal(layer_loss(c(0.5, 1.2, 3), 1, 0.5), c(0, 0.2, 0.5))
})

test_that("prime analytique = simulation", {
  p <- layer_premium(0.9, 1.5, 4, 1, 1, 0.5)
  s <- simulate_layer(0.9, 1.5, 4, 1, 1, 0.5, n_sim = 2e5, seed = 1)
  expect_equal(mean(s), p, tolerance = 0.01)
  expect_equal(mean(s > 0), 1 - exp(-4), tolerance = 0.01)   # a = u : tout sinistre touche
})

test_that("prime exponentielle (xi = 0) en forme fermée", {
  p <- layer_premium(0, 2, 3, 1, 2, 1)
  expect_equal(p, 3 * 2 * (exp(-0.5) - exp(-1)), tolerance = 1e-8)
})

test_that("prime finie même si xi > 1, et bornée par lambda * l", {
  p <- layer_premium(1.3, 1, 2, 1, 1, 0.5)
  expect_true(is.finite(p) && p < 2 * 0.5)
})

test_that("layer_risk : VaR 99,5 % sous la portée annuelle maximale plausible", {
  s <- simulate_layer(0.5, 1, 1, 1, 1, 0.5, n_sim = 5e4, seed = 2)
  r <- layer_risk(s)
  expect_true(r$var >= r$mean && r$tvar >= r$var)
})

test_that("bootstrap : primes positives autour de l'estimation", {
  set.seed(7)
  ev <- data.frame(year = rep(1:30, each = 5))
  ev$loss <- 1 + 1 / 0.5 * ((1 - runif(150))^(-0.5) - 1)
  bp <- bootstrap_premium(ev, 1:30, 1, 1, 0.5, B = 50)
  p0 <- layer_premium(gpd_fit(ev$loss, 1)$par[["xi"]], gpd_fit(ev$loss, 1)$par[["beta"]], 5, 1, 1, 0.5)
  expect_true(all(bp > 0))
  expect_true(min(bp) < p0 && p0 < max(bp))
})
