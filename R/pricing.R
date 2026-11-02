#' Charge d'une tranche en excédent de sinistre
#'
#' Pour une tranche « `limit` au-delà de `attachment` », la part d'un
#' sinistre \eqn{X} payée par le réassureur vaut
#' \eqn{\min(\max(X - a, 0), l)}.
#'
#' @param x Sinistres.
#' @param attachment Priorité \eqn{a}.
#' @param limit Portée \eqn{l}.
#' @return Charges de la tranche.
#' @export
#' @examples
#' layer_loss(c(0.5, 1.2, 3), attachment = 1, limit = 0.5)
layer_loss <- function(x, attachment, limit) {
  pmin(pmax(x - attachment, 0), limit)
}

#' Prime pure d'une tranche XL sous un modèle POT
#'
#' Charge annuelle moyenne de la tranche quand les sinistres au-delà de
#' \eqn{u} arrivent selon un processus de Poisson de taux \eqn{\lambda} et
#' suivent une GPD :
#' \deqn{E[L] = \lambda \int_a^{a+l} \left(1 + \xi \frac{z-u}{\beta}\right)^{-1/\xi} dz,}
#' qui reste finie même si \eqn{\xi \ge 1}, car la tranche est bornée.
#' Il faut \eqn{a \ge u}.
#'
#' @param xi,beta Paramètres GPD.
#' @param lambda Nombre annuel moyen de sinistres au-delà de `threshold`.
#' @param threshold Seuil \eqn{u}.
#' @param attachment,limit Priorité et portée.
#' @return Prime pure annuelle.
#' @export
layer_premium <- function(xi, beta, lambda, threshold, attachment, limit) {
  if (attachment < threshold) stop("La priorite doit etre au-dessus du seuil du modele.")
  surv <- function(z) {
    if (abs(xi) < 1e-8) exp(-(z - threshold) / beta)
    else pmax(1 + xi * (z - threshold) / beta, 0)^(-1 / xi)
  }
  lambda * stats::integrate(surv, attachment, attachment + limit,
                            rel.tol = 1e-10)$value
}

#' Simuler la charge annuelle d'une tranche XL
#'
#' Tire pour chaque année un nombre de sinistres \eqn{N \sim}
#' Poisson(\eqn{\lambda}) au-delà de \eqn{u}, leurs montants selon la GPD,
#' puis somme les charges de la tranche (sans limite annuelle ni
#' reconstitution).
#'
#' @inheritParams layer_premium
#' @param n_sim Nombre d'années simulées.
#' @param seed Graine aléatoire, ou `NULL`.
#' @return Vecteur des charges annuelles simulées.
#' @export
simulate_layer <- function(xi, beta, lambda, threshold, attachment, limit,
                           n_sim = 1e5, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  n <- stats::rpois(n_sim, lambda)
  tot <- sum(n)
  w <- stats::runif(tot)
  x <- if (abs(xi) < 1e-8) threshold - beta * log(w)
       else threshold + beta / xi * (w^(-xi) - 1)
  y <- layer_loss(x, attachment, limit)
  out <- numeric(n_sim)
  if (tot > 0) {
    s <- rowsum(y, rep.int(seq_len(n_sim), n))
    out[as.integer(rownames(s))] <- s[, 1]
  }
  out
}

#' Résumé du risque d'une tranche
#'
#' @param sims Charges annuelles simulées ([simulate_layer()]).
#' @param p Niveau de la VaR (99,5 % pour Solvabilité II).
#' @return `data.frame` avec `mean`, `sd`, `prob_hit` (probabilité d'au
#'   moins une charge dans l'année), `var` et `tvar`.
#' @export
layer_risk <- function(sims, p = 0.995) {
  v <- stats::quantile(sims, p, names = FALSE)
  data.frame(mean = mean(sims), sd = stats::sd(sims), prob_hit = mean(sims > 0),
             var = v, tvar = mean(sims[sims >= v]))
}

#' Bootstrap de la prime pure par rééchantillonnage des années
#'
#' Rééchantillonne les années avec remise (ce qui garde ensemble les
#' sinistres d'une même année), réajuste la GPD et le taux, et recalcule la
#' prime. Donne un intervalle pour la prime qui tient compte de
#' l'incertitude d'estimation.
#'
#' @param events `data.frame` avec `year` et `loss`.
#' @param years Années d'observation.
#' @param threshold,attachment,limit Seuil, priorité, portée.
#' @param B Nombre de réplications.
#' @param seed Graine.
#' @return Vecteur des primes bootstrap (réplications en échec retirées).
#' @export
bootstrap_premium <- function(events, years, threshold, attachment, limit,
                              B = 1000, seed = 1) {
  set.seed(seed)
  by_year <- split(events$loss, factor(events$year, levels = years))
  out <- vapply(seq_len(B), function(b) {
    yy <- sample(seq_along(years), replace = TRUE)
    x <- unlist(by_year[yy], use.names = FALSE)
    fit <- tryCatch(gpd_fit(x, threshold), error = function(e) NULL)
    if (is.null(fit)) return(NA_real_)
    lambda <- fit$n_exceed / length(years)
    layer_premium(fit$par[["xi"]], fit$par[["beta"]], lambda, threshold,
                  attachment, limit)
  }, numeric(1))
  out[!is.na(out)]
}
