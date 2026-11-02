test_that("model_parameters et scenario_parameters", {
  set.seed(8)
  yrs <- 2000:2019
  cv <- data.frame(year = yrs, temp_anom = seq(0.3, 1, length = 20))
  ev <- data.frame(year = rep(yrs, each = 6))
  ev$loss <- 0.2 + 1 / 0.5 * ((1 - runif(nrow(ev)))^(-0.5) - 1)
  mp <- model_parameters(ev, cv, yrs, threshold = 1, B = 5)
  expect_equal(nrow(mp$boot), 5)
  s <- scenario_parameters(mp$point)
  expect_equal(unname(s["lambda"]), sum(ev$loss > 1) / 20)
  c1 <- scenario_parameters(mp$point, 0.5)
  expect_true(all(is.finite(c1)) && c1["beta"] > 0)
})
