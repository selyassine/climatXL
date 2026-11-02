test_that("l'ajustement retrouve les paramètres d'une GPD simulée", {
  set.seed(1)
  xi <- 0.3; beta <- 2; n <- 20000
  y <- beta / xi * ((1 - runif(n))^(-xi) - 1)
  fit <- gpd_fit(y + 5, threshold = 5)
  expect_equal(fit$par[["xi"]], xi, tolerance = 0.05)
  expect_equal(fit$par[["beta"]], beta, tolerance = 0.05)
})

test_that("gpd_fit concorde avec evd::fpot", {
  skip_if_not_installed("evd")
  ref <- evd::fpot(danish$loss, threshold = 10, std.err = FALSE)
  fit <- gpd_fit(danish$loss, threshold = 10)
  expect_equal(fit$par[["xi"]], unname(ref$estimate["shape"]), tolerance = 1e-3)
  expect_equal(fit$par[["beta"]], unname(ref$estimate["scale"]), tolerance = 1e-3)
})

test_that("gpd_fit concorde avec fExtremes::gpdFit", {
  skip_if_not_installed("fExtremes")
  ref <- fExtremes::gpdFit(danish$loss, u = 10)@fit$par.ests
  fit <- gpd_fit(danish$loss, threshold = 10)
  expect_equal(fit$par[["xi"]], unname(ref["xi"]), tolerance = 1e-3)
  expect_equal(fit$par[["beta"]], unname(ref["beta"]), tolerance = 1e-3)
})

test_that("niveaux de retour croissants avec la période", {
  fit <- gpd_fit(danish$loss, threshold = 10, n_years = 11)
  rl <- return_level(fit, c(10, 50, 100, 200))
  expect_true(all(diff(rl$return_level) > 0))
  expect_error(return_level(gpd_fit(danish$loss, 10)), "n_years")
})

test_that("le niveau de retour à 1/lambda an vaut le seuil", {
  fit <- gpd_fit(danish$loss, threshold = 10, n_years = 11)
  lambda <- fit$n_exceed / 11
  expect_equal(return_level(fit, 1 / lambda * 1.0000001)$return_level, 10, tolerance = 1e-4)
})

test_that("gpd_risk refuse les probabilités sous le seuil", {
  fit <- gpd_fit(danish$loss, threshold = 10)
  expect_error(gpd_risk(fit, 0.5))
})

test_that("trop peu de dépassements déclenche une erreur", {
  expect_error(gpd_fit(danish$loss, threshold = 200), "depassements")
})
