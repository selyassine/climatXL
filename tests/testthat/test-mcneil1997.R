# Reproduction de McNeil (1997) sur les sinistres incendie danois.
#
# Valeurs de référence : estimations GPD publiées par McNeil (1997) pour
# plusieurs seuils (xi et quantile à 99,9 % par sinistre), et VaR / ES à 99 %
# au seuil u = 10 reprises dans McNeil, Frey et Embrechts (2005/2015),
# *Quantitative Risk Management*. McNeil travaillait sur 2156 sinistres et la
# série diffusée en contient 2167 : on tolère donc quelques pour cent d'écart.

test_that("le seuil u = 10 donne les 109 dépassements de McNeil", {
  expect_equal(sum(danish$loss > 10), 109)
})

test_that("u = 10 : xi ≈ 0,50 et beta ≈ 6,97 (McNeil 1997)", {
  fit <- gpd_fit(danish$loss, threshold = 10)
  expect_equal(fit$convergence, 0)
  expect_equal(fit$par[["xi"]], 0.497, tolerance = 0.01)
  expect_equal(fit$par[["beta"]], 6.97, tolerance = 0.01)
  expect_equal(fit$se[["xi"]], 0.14, tolerance = 0.05)
})

test_that("quantiles à 99,9 % selon le seuil reproduisent McNeil (1997)", {
  publie <- data.frame(
    u  = c(3, 4, 5, 10, 20),
    xi = c(0.67, 0.72, 0.63, 0.50, 0.68),
    q  = c(129, 147, 122, 95, 103)
  )
  for (i in seq_len(nrow(publie))) {
    fit <- gpd_fit(danish$loss, threshold = publie$u[i])
    q <- gpd_risk(fit, 0.999)$var
    expect_equal(fit$par[["xi"]], publie$xi[i], tolerance = 0.03,
                 label = paste("xi, u =", publie$u[i]))
    expect_equal(q, publie$q[i], tolerance = 0.03,
                 label = paste("x_0.999, u =", publie$u[i]))
  }
})

test_that("u = 10 : VaR 99 % ≈ 27,3 et ES 99 % ≈ 58,2", {
  fit <- gpd_fit(danish$loss, threshold = 10)
  r <- gpd_risk(fit, 0.99)
  expect_equal(r$var, 27.3, tolerance = 0.005)
  expect_equal(r$es, 58.2, tolerance = 0.005)
})

test_that("l'intervalle profilé du quantile 99,9 % est asymétrique et contient l'estimation", {
  fit <- gpd_fit(danish$loss, threshold = 10)
  ci <- gpd_quantile_ci(fit, 0.999)
  expect_true(ci[["lower"]] < ci[["estimate"]] && ci[["estimate"]] < ci[["upper"]])
  # Queue lourde : la borne haute est plus loin que la borne basse.
  expect_gt(ci[["upper"]] - ci[["estimate"]], ci[["estimate"]] - ci[["lower"]])
})
