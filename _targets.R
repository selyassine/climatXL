# Pipeline climatXL : `targets::tar_make()` refait toute l'analyse.
# Semaines 1 à 3 branchées ; les semaines 4 et 5 sont notées en commentaire.

library(targets)
tar_option_set(packages = "climatXL")

list(
  # --- Validation : McNeil (1997) ------------------------------------------
  tar_target(danish_losses, climatXL::danish$loss),
  tar_target(danish_me, mean_excess(danish_losses, min_exceed = 10)),
  tar_target(danish_hill, hill(danish_losses, k = 15:500)),
  tar_target(danish_fit, gpd_fit(danish_losses, threshold = 10, n_years = 11)),
  tar_target(danish_risk, gpd_risk(danish_fit, c(0.99, 0.995, 0.999))),
  tar_target(danish_q999_ci, gpd_quantile_ci(danish_fit, 0.999)),
  tar_target(danish_rl, return_level(danish_fit, c(10, 50, 100, 200))),
  # --- Semaine 2 : donnees NOAA et normalisation ---------------------------
  # Telecharge environ 1 Go la premiere fois (un fichier par annee).
  tar_target(years, 1990:2024),
  tar_target(base_year, 2024),
  tar_target(noaa_files, download_storm_events(years, dest = "data-raw/noaa"),
             format = "file"),
  tar_target(events, read_storm_events(noaa_files)),
  tar_target(episodes, aggregate_episodes(events)),
  tar_target(cpi, read_fred("https://fred.stlouisfed.org/graph/fredgraph.csv?id=CPIAUCSL", "cpi")),
  tar_target(gdp, read_fred("https://fred.stlouisfed.org/graph/fredgraph.csv?id=GDPA", "wealth")),
  tar_target(temp, read_noaa_temp(paste0(
    "https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/",
    "global/time-series/globe/tavg/land_ocean/12/12/1950-2024/data.csv"))),
  tar_target(enso, read_oni("https://psl.noaa.gov/data/correlation/oni.data")),
  tar_target(index, merge(cpi, gdp, by = "year")),
  tar_target(losses_cpi, normalise_losses(episodes,
             normalisation_factors(index, base_year, "cpi"))),
  tar_target(losses_norm, normalise_losses(episodes,
             normalisation_factors(index, base_year, "wealth"))),
  tar_target(covariates, merge(temp, enso, by = "year")),
  # --- Semaine 3 : modele stationnaire --------------------------------------
  # Avant 1996, la NOAA ne relevait que tornades, vent et grele (pas
  # d'ouragans, pas d'episodes) : l'analyse commence donc en 1996.
  tar_target(first_year, 1996),
  tar_target(cat_events, hours_clause(losses_norm[losses_norm$year >= first_year, ],
                                      hours = 72, min_loss = 5e7)),
  tar_target(cat_x, cat_events$loss / 1e9),          # milliards USD 2024
  tar_target(n_obs_years, max(years) - first_year + 1),
  tar_target(stability, threshold_stability(cat_x, exp(seq(log(0.2), log(6), length = 25)))),
  tar_target(fit_stat, gpd_fit(cat_x, threshold = 1, n_years = n_obs_years)),
  tar_target(rl_stat, return_level(fit_stat, c(10, 50, 100, 200), ci = TRUE))

  # --- Semaine 4 : modèle non stationnaire ---------------------------------
  # tar_target(fit_ns, fit_nonstationary(losses_norm, covariates = c("year", "temp_anom"))),
  # tar_target(lr_test, compare_models(fit_stat, fit_ns)),

  # --- Semaine 5 : tarification --------------------------------------------
  # tar_target(premium, price_layer(fit_ns, attachment = 1e9, limit = 5e8, scenario = "2035")),
)
