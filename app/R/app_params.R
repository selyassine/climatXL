#' Paramètres des modèles pour l'application Shiny
#'
#' Ajuste les modèles stationnaire et climatique (échelle et fréquence en
#' fonction de l'anomalie de température) et tire `B` jeux de paramètres
#' par bootstrap des années. L'application n'a besoin que de ces paramètres,
#' pas des données brutes.
#'
#' @param cat_data Événements (`year`, `loss` en Md$, `temp_anom`).
#' @param covariates Covariables annuelles (`year`, `temp_anom`).
#' @param years Années d'observation.
#' @param threshold Seuil.
#' @param B Nombre de tirages bootstrap.
#' @param seed Graine.
#' @return Liste : `point` (un tirage, estimations), `boot` (`B` tirages),
#'   `temp` (anomalies observées et tendance), `threshold`, `years`,
#'   `largest` (plus gros sinistres).
#' @export
model_parameters <- function(cat_data, covariates, years, threshold = 1,
                             B = 300, seed = 1) {
  tcov <- stats::setNames(covariates$temp_anom, covariates$year)
  one <- function(idx_years) {
    rows <- unlist(lapply(idx_years, function(y) which(cat_data$year == y)))
    d <- cat_data[rows, ]
    d$temp_anom <- tcov[as.character(d$year)]
    fs <- gpd_fit(d$loss, threshold)
    fn <- gpd_fit_ns(d, threshold, ~ temp_anom)
    cnt <- data.frame(n = vapply(idx_years, function(y) sum(cat_data$year == y &
                                                          cat_data$loss > threshold), 1L),
                      temp_anom = tcov[as.character(idx_years)])
    pf <- stats::glm(n ~ temp_anom, stats::poisson(), cnt)
    data.frame(xi_s = fs$par[["xi"]], beta_s = fs$par[["beta"]],
               lambda_s = fs$n_exceed / length(idx_years),
               xi_c = fn$coef[[1]], g0 = fn$coef[[2]], g1 = fn$coef[[3]],
               p0 = stats::coef(pf)[[1]], p1 = stats::coef(pf)[[2]])
  }
  point <- one(years)
  set.seed(seed)
  boot <- do.call(rbind, lapply(seq_len(B), function(b) {
    tryCatch(one(sample(years, replace = TRUE)), error = function(e) NULL)
  }))
  tt <- covariates[covariates$year %in% years, ]
  tr <- stats::lm(temp_anom ~ year, tt)
  list(point = point, boot = boot,
       temp = list(observed = tt, slope = stats::coef(tr)[[2]],
                   intercept = stats::coef(tr)[[1]]),
       threshold = threshold, years = years,
       largest = utils::head(cat_data[order(-cat_data$loss),
                                      intersect(c("date", "main_type", "loss"), names(cat_data))], 10))
}

#' Paramètres GPD et fréquence pour un scénario de température
#'
#' @param p Une ligne de [model_parameters()] (`point` ou `boot`).
#' @param temp Anomalie de température du scénario, ou `NULL` pour le
#'   modèle stationnaire.
#' @return Vecteur `xi`, `beta`, `lambda`.
#' @export
scenario_parameters <- function(p, temp = NULL) {
  if (is.null(temp)) return(c(xi = p$xi_s, beta = p$beta_s, lambda = p$lambda_s))
  c(xi = p$xi_c, beta = exp(p$g0 + p$g1 * temp), lambda = exp(p$p0 + p$p1 * temp))
}
