#' Regrouper les épisodes en sinistres catastrophe (clause horaire)
#'
#' Les épisodes NOAA sont découpés par bureau météo et par État : l'ouragan
#' Katrina y apparaît en plusieurs épisodes. Un traité catastrophe, lui,
#' définit un sinistre par une **clause horaire** : toutes les pertes
#' survenues dans une fenêtre de `hours` heures (souvent 72 h pour une
#' tempête) forment un seul événement.
#'
#' La fonction applique cette règle aux épisodes d'au moins `min_loss` :
#' chaque groupe commence au premier épisode non encore affecté et contient
#' les épisodes qui débutent moins de `hours` heures après lui. Les petits
#' épisodes sont ignorés : ils pèsent peu dans la queue et regrouperaient
#' sinon des orages sans lien à travers tout le pays.
#'
#' @param losses `data.frame` avec `date` (Date), `year`, `main_type` et la
#'   colonne de perte `loss_col`.
#' @param hours Durée de la clause, en heures (résolution : le jour).
#' @param min_loss Taille minimale d'un épisode pris en compte.
#' @param loss_col Nom de la colonne de perte.
#' @return Un `data.frame` d'événements : `event`, `date`, `year`,
#'   `n_episodes`, `main_type` (type de l'épisode le plus coûteux), `loss`.
#' @export
hours_clause <- function(losses, hours = 72, min_loss = 0, loss_col = "loss_norm") {
  d <- losses[losses[[loss_col]] >= min_loss, ]
  d$date <- as.Date(d$date)
  d <- d[order(d$date), ]
  n <- nrow(d)
  ev <- integer(n)
  days <- hours / 24
  k <- 0L; start <- NULL
  for (i in seq_len(n)) {
    if (is.null(start) || as.numeric(d$date[i] - start) >= days) {
      k <- k + 1L; start <- d$date[i]
    }
    ev[i] <- k
  }
  x <- d[[loss_col]]
  top <- vapply(split(seq_len(n), ev), function(i) i[which.max(x[i])], 1L)
  out <- data.frame(
    event = seq_len(k),
    date = d$date[top],
    year = d$year[top],
    n_episodes = tabulate(ev, k),
    main_type = d$main_type[top],
    loss = as.numeric(rowsum(x, ev)),
    stringsAsFactors = FALSE
  )
  out$date <- as.Date(tapply(d$date, ev, min), origin = "1970-01-01")
  out
}

#' Stabilité des paramètres GPD selon le seuil
#'
#' Ajuste la GPD pour chaque seuil. Au-dessus du bon seuil, \eqn{\xi} et
#' l'échelle modifiée \eqn{\beta^* = \beta - \xi u} doivent rester à peu
#' près constants : c'est le complément du graphique des excès moyens.
#'
#' @param x Vecteur de pertes.
#' @param thresholds Seuils à tester.
#' @param level Niveau des intervalles (méthode delta).
#' @return Un `data.frame` avec `threshold`, `n_exceed`, `xi`, `xi_lower`,
#'   `xi_upper`, `beta_star`.
#' @export
threshold_stability <- function(x, thresholds, level = 0.95) {
  z <- stats::qnorm(1 - (1 - level) / 2)
  rows <- lapply(thresholds, function(u) {
    fit <- tryCatch(gpd_fit(x, u), error = function(e) NULL)
    if (is.null(fit)) return(NULL)
    xi <- fit$par[["xi"]]; se <- fit$se[["xi"]]
    data.frame(threshold = u, n_exceed = fit$n_exceed, xi = xi,
               xi_lower = xi - z * se, xi_upper = xi + z * se,
               beta_star = fit$par[["beta"]] - xi * u)
  })
  do.call(rbind, rows)
}
