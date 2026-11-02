# Pipeline climatXL : `targets::tar_make()` refait toute l'analyse.
# Toutes les étapes, des données brutes aux paramètres de l'application.
# Le rapport se construit ensuite avec `quarto render rapport`.

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
  tar_target(rl_stat, return_level(fit_stat, c(10, 50, 100, 200), ci = TRUE)),
  # --- Semaine 4 : modele non stationnaire -----------------------------------
  tar_target(cat_data, transform(merge(transform(cat_events, loss = loss / 1e9),
                                       covariates, by = "year"),
                                 t = (year - 2010) / 10)),
  tar_target(counts, transform(merge(exceedance_counts(cat_data, 1, first_year:max(years)),
                                     covariates, by = "year"), t = (year - 2010) / 10)),
  tar_target(ns_m0, gpd_fit_ns(cat_data, 1, ~ 1)),
  tar_target(ns_year, gpd_fit_ns(cat_data, 1, ~ t)),
  tar_target(ns_temp, gpd_fit_ns(cat_data, 1, ~ temp_anom)),
  tar_target(ns_enso, gpd_fit_ns(cat_data, 1, ~ enso_jjason)),
  tar_target(severity_tests, rbind(year = lr_test(ns_m0, ns_year),
                                   temp = lr_test(ns_m0, ns_temp),
                                   enso = lr_test(ns_m0, ns_enso))),
  tar_target(freq_year, poisson_trend(counts, n ~ t)),
  tar_target(freq_temp, poisson_trend(counts, n ~ temp_anom)),
  # --- Semaine 5 : tarification ---------------------------------------------
  # Tranches sur la perte de marche americaine, en Md$ 2024.
  tar_target(temp_trend, lm(temp_anom ~ year, covariates[covariates$year >= first_year, ])),
  tar_target(temp_2035, unname(predict(temp_trend, data.frame(year = 2035)))),
  tar_target(premium_stat, layer_premium(fit_stat$par[["xi"]], fit_stat$par[["beta"]],
                                         fit_stat$n_exceed / n_obs_years, 1, 10, 5)),
  tar_target(premium_2035, layer_premium(
    ns_temp$coef[["xi"]], gpd_ns_scale(ns_temp, data.frame(temp_anom = temp_2035)),
    unname(predict(freq_temp$fit, data.frame(temp_anom = temp_2035), type = "response")),
    1, 10, 5)),
  tar_target(climate_loading, premium_2035 / premium_stat - 1),
  tar_target(premium_boot, bootstrap_premium(transform(cat_data, loss = loss),
                                             first_year:max(years), 1, 10, 5, B = 1000)),

  # --- Semaine 6 : parametres de l'application Shiny ---------------------------
  tar_target(app_params, {
    saveRDS(model_parameters(cat_data, covariates, first_year:max(years), 1, B = 300),
            "app/parametres.rds")
    "app/parametres.rds"
  }, format = "file")
)
