#' Fonction des excès moyens empirique
#'
#' Calcule \eqn{e(u) = E[X - u \mid X > u]} sur une grille de seuils. Une
#' courbe à peu près linéaire et croissante au-delà d'un seuil indique une
#' queue de type Pareto généralisée avec \eqn{\xi > 0}.
#'
#' @param x Vecteur numérique de pertes.
#' @param thresholds Seuils à évaluer. Par défaut, les valeurs observées
#'   triées, sauf les `min_exceed` plus grandes.
#' @param min_exceed Nombre minimal de dépassements pour qu'un seuil soit gardé.
#' @param level Niveau des intervalles de confiance (approximation normale).
#' @return Un `data.frame` avec `threshold`, `mean_excess`, `n_exceed`,
#'   `lower`, `upper`.
#' @export
#' @examples
#' me <- mean_excess(danish$loss)
#' plot(me$threshold, me$mean_excess, type = "l", log = "x")
mean_excess <- function(x, thresholds = NULL, min_exceed = 5, level = 0.95) {
  x <- check_losses(x)
  if (is.null(thresholds)) {
    xs <- sort(unique(x))
    thresholds <- xs[seq_len(max(0, length(xs) - min_exceed))]
  }
  z <- stats::qnorm(1 - (1 - level) / 2)
  rows <- lapply(thresholds, function(u) {
    exc <- x[x > u] - u
    n <- length(exc)
    if (n < min_exceed) return(NULL)
    m <- mean(exc)
    se <- if (n > 1) stats::sd(exc) / sqrt(n) else NA_real_
    data.frame(threshold = u, mean_excess = m, n_exceed = n,
               lower = m - z * se, upper = m + z * se)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Estimateur de Hill de l'indice de queue
#'
#' Pour chaque nombre `k` de plus grandes observations, calcule
#' \eqn{\hat\xi_k = \frac{1}{k}\sum_{i=1}^{k} \log X_{(i)} - \log X_{(k+1)}},
#' où \eqn{X_{(1)} \ge X_{(2)} \ge \dots} sont les statistiques d'ordre.
#' Valable seulement pour une queue lourde (\eqn{\xi > 0}) et des pertes
#' strictement positives.
#'
#' @param x Vecteur de pertes strictement positives.
#' @param k Nombres d'observations de queue utilisés. Par défaut `2:(n-1)`.
#' @param level Niveau des intervalles de confiance asymptotiques.
#' @return Un `data.frame` avec `k`, `threshold` (= \eqn{X_{(k+1)}}), `xi`,
#'   `alpha` (= 1/xi), `lower`, `upper` (intervalle pour `xi`).
#' @export
#' @examples
#' h <- hill(danish$loss, k = 10:500)
#' plot(h$k, h$xi, type = "l")
hill <- function(x, k = NULL, level = 0.95) {
  x <- check_losses(x)
  if (any(x <= 0)) stop("`x` doit etre strictement positif pour l'estimateur de Hill.")
  xs <- sort(x, decreasing = TRUE)
  n <- length(xs)
  if (is.null(k)) k <- seq.int(2, n - 1)
  if (any(k < 1 | k >= n)) stop("`k` doit etre compris entre 1 et n - 1.")
  lx <- log(xs)
  cum <- cumsum(lx)
  xi <- cum[k] / k - lx[k + 1]
  z <- stats::qnorm(1 - (1 - level) / 2)
  data.frame(k = k, threshold = xs[k + 1], xi = xi, alpha = 1 / xi,
             lower = xi - z * xi / sqrt(k), upper = xi + z * xi / sqrt(k))
}

check_losses <- function(x) {
  if (!is.numeric(x)) stop("`x` doit etre un vecteur numerique.")
  x <- x[!is.na(x)]
  if (length(x) < 2) stop("`x` doit contenir au moins deux valeurs non manquantes.")
  x
}
