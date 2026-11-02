test_that("la clause 72 h regroupe les épisodes proches", {
  d <- data.frame(
    date = as.Date(c("2005-08-28", "2005-08-29", "2005-08-30", "2005-09-10", "2005-09-11")),
    year = 2005L,
    main_type = c("Hurricane", "Storm Surge", "Flood", "Hail", "Tornado"),
    loss_norm = c(10, 30, 5, 2, 1)
  )
  ev <- hours_clause(d, hours = 72)
  expect_equal(nrow(ev), 2)
  expect_equal(ev$loss, c(45, 3))
  expect_equal(ev$n_episodes, c(3L, 2L))
  expect_equal(ev$main_type, c("Storm Surge", "Hail"))
  expect_equal(ev$date[1], as.Date("2005-08-28"))
})

test_that("le 4e jour ouvre un nouvel événement et min_loss filtre", {
  d <- data.frame(date = as.Date("2020-01-01") + c(0, 3), year = 2020L,
                  main_type = "x", loss_norm = c(5, 5))
  expect_equal(nrow(hours_clause(d, 72)), 2)
  expect_equal(nrow(hours_clause(d, 96)), 1)
  expect_equal(nrow(hours_clause(d, 72, min_loss = 6)), 0)
})

test_that("threshold_stability : xi stable pour une GPD exacte", {
  set.seed(3)
  y <- 2 / 0.4 * ((1 - runif(20000))^(-0.4) - 1)
  st <- threshold_stability(y, c(0, 1, 2))
  expect_true(all(abs(st$xi - 0.4) < 0.05))
  expect_true(all(abs(st$beta_star - 2) < 0.2))
})
