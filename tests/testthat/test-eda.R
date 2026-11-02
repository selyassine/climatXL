test_that("Hill sur une Pareto exacte retrouve xi", {
  set.seed(2)
  x <- (1 - runif(50000))^(-0.5)   # Pareto d'indice alpha = 2, xi = 0,5
  h <- hill(x, k = 2000)
  expect_equal(h$xi, 0.5, tolerance = 0.05)
})

test_that("Hill refuse les pertes négatives", {
  expect_error(hill(c(-1, 2, 3)), "positif")
})

test_that("excès moyens : calcul direct", {
  me <- mean_excess(c(1, 2, 3, 4, 10), thresholds = 2, min_exceed = 1)
  expect_equal(me$mean_excess, mean(c(1, 2, 8)))
  expect_equal(me$n_exceed, 3)
})

test_that("excès moyens croissants au-dessus de 10 pour les données danoises", {
  me <- mean_excess(danish$loss, thresholds = c(10, 15, 20), min_exceed = 10)
  # Pente positive attendue pour une queue de Pareto avec xi > 0.
  expect_gt(stats::coef(stats::lm(mean_excess ~ threshold, data = me))[2], 0)
})
