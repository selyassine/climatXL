#' Ajuster une GPD non stationnaire
#'
#' L'échelle dépend de covariables par un lien logarithmique, la forme
#' \eqn{\xi} reste constante :
#' \deqn{\log \beta_i = \gamma_0 + \gamma_1 z_{i1} + \dots}
#' C'est le modèle que propose `extRemes::fevd(type = "GP", scale.fun = ...,
#' use.phi = TRUE)`, réécrit ici pour rester sans dépendance et testable.
#'
#' @param data `data.frame` d'événements : colonne de perte `loss_col` et
#'   covariables.
#' @param threshold Seuil \eqn{u} (constant).
#' @param scale Formule à droite pour \eqn{\log\beta}, par exemple
#'   `~ temp_anom`. `~ 1` donne le modèle stationnaire.
#' @param loss_col Nom de la colonne de perte.
#' @return Objet `climatxl_gpd_ns` : `coef`, `se`, `vcov`, `loglik`, `aic`,
#'   `npar`, `threshold`, `scale` (formule), `data` (les dépassements).
#' @export
gpd_fit_ns <- function(data, threshold, scale = ~ 1, loss_col = "loss") {
  exc <- data[data[[loss_col]] > threshold, , drop = FALSE]
  y <- exc[[loss_col]] - threshold
  X <- stats::model.matrix(scale, exc)
  colnames(X) <- paste0("log_beta:", colnames(X))
  st <- gpd_fit(y + threshold, threshold)
  p0 <- c(st$par[["xi"]], log(st$par[["beta"]]), rep(0, ncol(X) - 1))
  nll <- function(p) gpd_nll_vec(p[1], exp(drop(X %*% p[-1])), y)
  opt <- stats::optim(p0, nll, method = "BFGS",
                      control = list(reltol = 1e-12, maxit = 2000))
  h <- stats::optimHess(opt$par, nll)
  vc <- tryCatch(solve(h), error = function(e) matrix(NA_real_, length(p0), length(p0)))
  nm <- c("xi", colnames(X))
  names(opt$par) <- nm; dimnames(vc) <- list(nm, nm)
  structure(list(
    coef = opt$par, se = sqrt(diag(vc)), vcov = vc, loglik = -opt$value,
    npar = length(p0), aic = 2 * opt$value + 2 * length(p0),
    threshold = threshold, scale = scale, data = exc, loss_col = loss_col,
    convergence = opt$convergence
  ), class = "climatxl_gpd_ns")
}

gpd_nll_vec <- function(xi, beta, y) {
  if (any(beta <= 0)) return(1e10)
  if (abs(xi) < 1e-8) return(sum(log(beta) + y / beta))
  z <- 1 + xi * y / beta
  if (any(z <= 0)) return(1e10)
  sum(log(beta)) + (1 + 1 / xi) * sum(log(z))
}

#' @export
print.climatxl_gpd_ns <- function(x, ...) {
  cat("GPD non stationnaire, u =", format(x$threshold),
      " echelle :", deparse(x$scale), "\n")
  print(round(cbind(estimate = x$coef, se = x$se), 4))
  cat("  log-vraisemblance :", round(x$loglik, 3), " AIC :", round(x$aic, 2), "\n")
  invisible(x)
}

#' Échelle GPD prédite pour de nouvelles covariables
#'
#' @param fit Objet [gpd_fit_ns()].
#' @param newdata `data.frame` de covariables.
#' @return Vecteur des \eqn{\beta}.
#' @export
gpd_ns_scale <- function(fit, newdata) {
  X <- stats::model.matrix(fit$scale, newdata)
  exp(drop(X %*% fit$coef[-1]))
}

#' Test du rapport de vraisemblance entre deux modèles emboîtés
#'
#' @param fit0 Modèle restreint (par exemple stationnaire).
#' @param fit1 Modèle complet.
#' @return Un `data.frame` avec `lr` (statistique), `df`, `p_value`,
#'   `aic0`, `aic1`.
#' @export
lr_test <- function(fit0, fit1) {
  lr <- 2 * (fit1$loglik - fit0$loglik)
  df <- fit1$npar - fit0$npar
  data.frame(lr = lr, df = df,
             p_value = stats::pchisq(lr, df, lower.tail = FALSE),
             aic0 = fit0$aic, aic1 = fit1$aic)
}

#' Nombre annuel de dépassements
#'
#' @param events `data.frame` avec `year` et la colonne de perte.
#' @param threshold Seuil.
#' @param years Années couvertes (les années sans dépassement comptent 0).
#' @param loss_col Nom de la colonne de perte.
#' @return Un `data.frame` avec `year` et `n`.
#' @export
exceedance_counts <- function(events, threshold, years, loss_col = "loss") {
  yy <- events$year[events[[loss_col]] > threshold]
  data.frame(year = years, n = as.integer(table(factor(yy, levels = years))))
}

#' Tester une tendance dans la fréquence des dépassements
#'
#' Ajuste un modèle de Poisson log-linéaire du nombre annuel de dépassements
#' et le compare au modèle à taux constant. Le rapport de dispersion
#' (déviance de Pearson / degrés de liberté) signale une surdispersion : au-
#' dessus de 1,5 environ, un modèle quasi-Poisson ou binomial négatif est
#' plus prudent.
#'
#' @param counts Sortie de [exceedance_counts()] fusionnée avec les
#'   covariables.
#' @param formula Formule du modèle complet, par exemple `n ~ temp_anom`.
#' @return Liste avec `fit` (glm), `null` (glm à taux constant), `lr`
#'   (`data.frame` comme [lr_test()]) et `dispersion`.
#' @export
poisson_trend <- function(counts, formula = n ~ year) {
  f1 <- stats::glm(formula, family = stats::poisson(), data = counts)
  f0 <- stats::glm(n ~ 1, family = stats::poisson(), data = counts)
  lr <- f0$deviance - f1$deviance
  df <- f0$df.residual - f1$df.residual
  disp <- sum(stats::residuals(f1, type = "pearson")^2) / f1$df.residual
  list(fit = f1, null = f0, dispersion = disp,
       lr = data.frame(lr = lr, df = df,
                       p_value = stats::pchisq(lr, df, lower.tail = FALSE),
                       aic0 = stats::AIC(f0), aic1 = stats::AIC(f1)))
}

#' Période de retour d'un montant sous un climat donné
#'
#' Combine la fréquence annuelle \eqn{\lambda} et l'échelle \eqn{\beta}
#' correspondant à un état du climat :
#' \deqn{T(z) = 1 / \left[\lambda (1 + \xi (z-u)/\beta)^{-1/\xi}\right].}
#'
#' @param z Montants.
#' @param xi,beta,lambda,threshold Paramètres pour l'état du climat voulu.
#' @return Périodes de retour en années.
#' @export
return_period <- function(z, xi, beta, lambda, threshold) {
  s <- if (abs(xi) < 1e-8) exp(-(z - threshold) / beta)
       else pmax(1 + xi * (z - threshold) / beta, 0)^(-1 / xi)
  1 / (lambda * s)
}

#' Niveau de retour sous un climat donné
#'
#' @inheritParams return_period
#' @param period Période de retour en années.
#' @return Montants.
#' @export
return_level_at <- function(period, xi, beta, lambda, threshold) {
  if (abs(xi) < 1e-8) threshold + beta * log(lambda * period)
  else threshold + beta / xi * ((lambda * period)^xi - 1)
}
