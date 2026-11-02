# Pipeline climatXL : `targets::tar_make()` refait toute l'analyse.
# Semaine 1 : seule la validation sur les données danoises est branchée.
# Les étapes NOAA (semaines 2 à 5) sont notées en commentaire.

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
  tar_target(danish_rl, return_level(danish_fit, c(10, 50, 100, 200)))

  # --- Semaine 2 : données NOAA ----------------------------------------------
  # tar_target(noaa_raw, read_storm_events(years = 1990:2024)),
  # tar_target(cpi, read_cpi()),
  # tar_target(losses_norm, normalise_losses(noaa_raw, cpi, exposure)),

  # --- Semaine 3 : modèle stationnaire -------------------------------------
  # tar_target(fit_stat, gpd_fit(losses_norm$loss, threshold = u_choisi)),

  # --- Semaine 4 : modèle non stationnaire ---------------------------------
  # tar_target(fit_ns, fit_nonstationary(losses_norm, covariates = c("year", "temp_anom"))),
  # tar_target(lr_test, compare_models(fit_stat, fit_ns)),

  # --- Semaine 5 : tarification --------------------------------------------
  # tar_target(premium, price_layer(fit_ns, attachment = 1e9, limit = 5e8, scenario = "2035")),
)
