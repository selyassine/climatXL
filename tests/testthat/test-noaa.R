fx <- function(f) testthat::test_path("fixtures", f)

test_that("parse_damage convertit les codes NOAA", {
  expect_equal(parse_damage(c("25K", "1.5M", "2B", "0.00K", "350", "10h")),
               c(25e3, 1.5e6, 2e9, 0, 350, 1000))
  expect_true(all(is.na(parse_damage(c("", NA, "K", "abc")))))
})

test_that("read_storm_events lit un fichier details", {
  ev <- read_storm_events(fx("storm_details_sample.csv"))
  expect_equal(nrow(ev), 5)
  expect_equal(ev$damage_property[1], 1.5e9)
  expect_true(is.na(ev$damage_crops[3]))
  expect_equal(ev$begin_date[1], as.Date("1995-08-01"))
  expect_true(is.na(ev$episode_id[5]))
})

test_that("aggregate_episodes somme les dommages par tempête", {
  ev <- read_storm_events(fx("storm_details_sample.csv"))
  ep <- aggregate_episodes(ev)
  # L'épisode 1002 (0 $) disparaît ; l'événement sans épisode reste seul.
  expect_equal(nrow(ep), 2)
  big <- ep[which.max(ep$loss), ]
  expect_equal(big$loss, 1.5e9 + 250e6 + 25e3)
  expect_equal(big$n_events, 3)
  expect_equal(big$n_states, 2)
  expect_equal(big$main_type, "Hurricane (Typhoon)")
  expect_equal(big$date, as.Date("1995-08-01"))
  with_crops <- aggregate_episodes(ev, crops = TRUE)
  expect_equal(max(with_crops$loss), big$loss + 10e6)
})

test_that("storm_file_table garde la version la plus récente par année", {
  files <- c("StormEvents_details-ftp_v1.0_d1995_c20210803.csv.gz",
             "StormEvents_details-ftp_v1.0_d1995_c20240101.csv.gz",
             "StormEvents_details-ftp_v1.0_d1996_c20220101.csv.gz")
  tab <- storm_file_table(files, 1995:1996, "https://x/")
  expect_equal(tab$file[tab$year == 1995], files[2])
  expect_warning(storm_file_table(files, 1995:1997, "https://x/"), "1997")
})
