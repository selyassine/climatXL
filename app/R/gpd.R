#' Ajuster une loi de Pareto généralisée au-dessus d'un seuil
#'
#' Ajuste par maximum de vraisemblance la loi GPD aux excès \eqn{Y = X - u}
#' des pertes au-dessus du seuil `threshold` :
#' \deqn{G_{\xi,\beta}(y) = 1 - (1 + \xi y / \beta)^{-1/\xi}.}
#' Les erreurs types viennent de la matrice d'information observée.
#'
#' @param x Vecteur de pertes.
#' @param threshold Seuil \eqn{u}.
#' @param n_years Durée d'observation en années, utilisée par
#'   [return_level()]. Facultatif.
#' @return Un objet de classe `climatxl_gpd` : liste avec `par` (`xi`,
#'   `beta`), `se`, `vcov`, `loglik`, `threshold`, `n`, `n_exceed`,
#'   `exceedances`, `n_years`, `convergence`.
#' @references McNeil, A. J. (1997). Estimating the tails of loss severity
#'   distributions using extreme value theory. *ASTIN Bulletin*, 27(1), 117-137.
#' @export
#' @examples
#' fit <- gpd_fit(danish$loss, threshold = 10, n_years = 11)
#' fit
gpd_fit <- function(x, threshold, n_years = NULL) {
  x <- check_losses(x)
  stopifnot(is.numeric(threshold), length(threshold) == 1)
  y <- x[x > threshold] - threshold
  if (length(y) < 10) stop("Moins de 10 depassements : baissez le seuil.")

  # Point de depart : methode des moments.
  m <- mean(y); v <- stats::var(y)
  xi0 <- 0.5 * (1 - m^2 / v)
  beta0 <- 0.5 * m * (m^2 / v + 1)
  if (!is.finite(xi0) || beta0 <= 0) { xi0 <- 0.1; beta0 <- m }

  nll <- function(p) gpd_nll(xi = p[1], beta = exp(p[2]), y = y)
  opt <- stats::optim(c(xi0, log(beta0)), nll, method = "BFGS",
                      control = list(reltol = 1e-12, maxit = 1000))
  par <- c(xi = opt$par[1], beta = exp(opt$par[2]))

  # Information observee sur l'echelle (xi, beta).
  h <- stats::optimHess(par, function(p) gpd_nll(p[1], p[2], y))
  vc <- tryCatch(solve(h), error = function(e) matrix(NA_real_, 2, 2))
  dimnames(vc) <- list(names(par), names(par))

  structure(list(
    par = par, se = sqrt(diag(vc)), vcov = vc, loglik = -opt$value,
    threshold = threshold, n = length(x), n_exceed = length(y),
    exceedances = y + threshold, n_years = n_years,
    convergence = opt$convergence
  ), class = "climatxl_gpd")
}

gpd_nll <- function(xi, beta, y) {
  if (beta <= 0) return(1e10)
  if (abs(xi) < 1e-8) return(length(y) * log(beta) + sum(y) / beta)
  z <- 1 + xi * y / beta
  if (any(z <= 0)) return(1e10)
  length(y) * log(beta) + (1 + 1 / xi) * sum(log(z))
}

#' @export
print.climatxl_gpd <- function(x, ...) {
  cat("Loi de Pareto generalisee au-dessus de u =", format(x$threshold), "\n")
  cat("  depassements :", x$n_exceed, "sur", x$n, "\n")
  est <- cbind(estimate = x$par, se = x$se)
  print(round(est, 4))
  cat("  log-vraisemblance :", round(x$loglik, 3), "\n")
  invisible(x)
}

#' Mesures de risque de queue : VaR et expected shortfall
#'
#' Estimateur de queue de McNeil (1997) :
#' \deqn{\widehat{x}_p = u + \frac{\beta}{\xi}\left[\left(\frac{n}{N_u}(1-p)\right)^{-\xi} - 1\right],}
#' \deqn{\widehat{ES}_p = \frac{\widehat{x}_p}{1-\xi} + \frac{\beta - \xi u}{1-\xi}.}
#' Les probabilités `p` doivent dépasser \eqn{1 - N_u/n}.
#'
#' @param fit Objet renvoyé par [gpd_fit()].
#' @param p Probabilités (niveaux de quantile) par sinistre.
#' @return Un `data.frame` avec `p`, `var` et `es` (`NA` si \eqn{\xi \ge 1}).
#' @export
#' @examples
#' fit <- gpd_fit(danish$loss, threshold = 10)
#' gpd_risk(fit, c(0.99, 0.999))
gpd_risk <- function(fit, p) {
  stopifnot(inherits(fit, "climatxl_gpd"))
  pu <- fit$n_exceed / fit$n
  if (any(p <= 1 - pu | p >= 1)) {
    stop("`p` doit etre dans (1 - Nu/n, 1) = (", round(1 - pu, 4), ", 1).")
  }
  xi <- fit$par[["xi"]]; beta <- fit$par[["beta"]]; u <- fit$threshold
  q <- gpd_quantile_tail(p, u, xi, beta, pu)
  es <- if (xi < 1) q / (1 - xi) + (beta - xi * u) / (1 - xi) else NA_real_
  data.frame(p = p, var = q, es = es)
}

gpd_quantile_tail <- function(p, u, xi, beta, pu) {
  r <- (1 - p) / pu
  if (abs(xi) < 1e-8) u - beta * log(r) else u + beta / xi * (r^(-xi) - 1)
}

#' Intervalle de confiance du quantile par vraisemblance profilée
#'
#' Reparamètre la GPD par \eqn{(\xi, x_p)} et profile la vraisemblance en
#' \eqn{x_p}. C'est la méthode utilisée par McNeil (1997) pour ses
#' intervalles, plus fiable que la méthode delta pour les quantiles extrêmes,
#' dont la loi est très asymétrique.
#'
#' @param fit Objet renvoyé par [gpd_fit()].
#' @param p Probabilité du quantile.
#' @param level Niveau de confiance.
#' @param upper_max Borne de recherche supérieure, en multiple de l'estimation.
#' @return Vecteur nommé `estimate`, `lower`, `upper`.
#' @export
#' @examples
#' fit <- gpd_fit(danish$loss, threshold = 10)
#' gpd_quantile_ci(fit, 0.999)
gpd_quantile_ci <- function(fit, p, level = 0.95, upper_max = 20) {
  stopifnot(inherits(fit, "climatxl_gpd"), length(p) == 1)
  u <- fit$threshold
  y <- fit$exceedances - u
  pu <- fit$n_exceed / fit$n
  r <- (1 - p) / pu
  est <- gpd_risk(fit, p)$var

  prof <- function(xp) {
    nll_xi <- function(xi) {
      beta <- xi * (xp - u) / (r^(-xi) - 1)
      gpd_nll(xi, beta, y)
    }
    o <- stats::optimize(nll_xi, c(-0.99, 3), tol = 1e-10)
    -o$objective
  }
  crit <- fit$loglik - stats::qchisq(level, 1) / 2
  g <- function(xp) prof(xp) - crit
  lower <- stats::uniroot(g, c(u + 1e-6 * (est - u), est), tol = 1e-8)$root
  hi <- est * upper_max
  upper <- if (g(hi) < 0) stats::uniroot(g, c(est, hi), tol = 1e-8)$root else Inf
  c(estimate = est, lower = lower, upper = upper)
}

#' Niveaux de retour
#'
#' Montant dépassé en moyenne une fois tous les `period` ans :
#' \deqn{z_m = u + \frac{\beta}{\xi}\left[(\lambda m)^{\xi} - 1\right],}
#' où \eqn{\lambda = N_u / T} est le nombre annuel moyen de dépassements.
#'
#' @param fit Objet renvoyé par [gpd_fit()] avec `n_years` renseigné.
#' @param period Périodes de retour en années.
#' @param ci Ajouter un intervalle par vraisemblance profilée.
#' @param level Niveau de l'intervalle.
#' @return Un `data.frame` avec `period` et `return_level` (et `lower`,
#'   `upper` si `ci = TRUE`).
#' @export
#' @examples
#' fit <- gpd_fit(danish$loss, threshold = 10, n_years = 11)
#' return_level(fit, c(10, 50, 100, 200))
return_level <- function(fit, period = c(10, 50, 100, 200), ci = FALSE, level = 0.95) {
  stopifnot(inherits(fit, "climatxl_gpd"))
  if (is.null(fit$n_years)) stop("Renseignez `n_years` dans gpd_fit().")
  lambda <- fit$n_exceed / fit$n_years
  if (any(lambda * period <= 1)) {
    stop("Periode trop courte : moins d'un depassement attendu.")
  }
  xi <- fit$par[["xi"]]; beta <- fit$par[["beta"]]; u <- fit$threshold
  z <- if (abs(xi) < 1e-8) u + beta * log(lambda * period)
       else u + beta / xi * ((lambda * period)^xi - 1)
  out <- data.frame(period = period, return_level = z)
  if (ci) {
    # Le niveau de retour a m ans est le quantile p = 1 - T / (m n) par sinistre.
    p <- 1 - fit$n_years / (period * fit$n)
    b <- t(vapply(p, function(pp) gpd_quantile_ci(fit, pp, level)[c("lower", "upper")],
                  numeric(2)))
    out$lower <- b[, 1]; out$upper <- b[, 2]
  }
  out
}
