sim_ns <- function(n = 4000, seed = 4) {
  set.seed(seed)
  z <- runif(n, -1, 1)
  beta <- exp(1 + 0.5 * z)
  xi <- 0.3
  y <- beta / xi * ((1 - runif(n))^(-xi) - 1)
  data.frame(loss = y + 10, z = z)
}

test_that("gpd_fit_ns retrouve l'effet d'une covariable", {
  d <- sim_ns()
  f <- gpd_fit_ns(d, 10, ~ z)
  expect_equal(unname(f$coef), c(0.3, 1, 0.5), tolerance = 0.1)
  expect_lt(lr_test(gpd_fit_ns(d, 10, ~ 1), f)$p_value, 1e-6)
})

test_that("gpd_fit_ns stationnaire = gpd_fit", {
  f0 <- gpd_fit_ns(data.frame(loss = danish$loss), 10, ~ 1)
  f <- gpd_fit(danish$loss, 10)
  expect_equal(unname(f0$coef[1]), f$par[["xi"]], tolerance = 1e-4)
  expect_equal(exp(unname(f0$coef[2])), f$par[["beta"]], tolerance = 1e-4)
  expect_equal(f0$loglik, f$loglik, tolerance = 1e-6)
})

test_that("sans effet, le test ne rejette pas trop souvent", {
  set.seed(5)
  p <- replicate(40, {
    d <- data.frame(loss = 10 + 2 / 0.3 * ((1 - runif(300))^(-0.3) - 1), z = rnorm(300))
    lr_test(gpd_fit_ns(d, 10, ~ 1), gpd_fit_ns(d, 10, ~ z))$p_value
  })
  expect_lt(mean(p < 0.05), 0.2)
})

test_that("poisson_trend détecte une tendance simulée", {
  set.seed(6)
  d <- data.frame(year = 1:60)
  d$n <- rpois(60, exp(1 + 0.03 * d$year))
  r <- poisson_trend(d, n ~ year)
  expect_lt(r$lr$p_value, 0.01)
  expect_equal(unname(coef(r$fit)[2]), 0.03, tolerance = 0.3)
})

test_that("exceedance_counts compte zéro les années vides", {
  ev <- data.frame(year = c(2000, 2000, 2002), loss = c(5, 1, 7))
  expect_equal(exceedance_counts(ev, 2, 2000:2002)$n, c(1L, 0L, 1L))
})

test_that("return_period et return_level_at sont inverses", {
  z <- return_level_at(100, 0.5, 2, 4, 1)
  expect_equal(return_period(z, 0.5, 2, 4, 1), 100)
})
