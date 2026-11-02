#' Sinistres incendie danois, 1980-1990
#'
#' Grands sinistres incendie recueillis par Copenhagen Re, utilisés par
#' McNeil (1997) pour illustrer la théorie des valeurs extrêmes en assurance.
#' Le package s'en sert pour valider son code avant les données NOAA.
#'
#' @format Un `data.frame` de 2167 lignes :
#' \describe{
#'   \item{date}{Date du sinistre.}
#'   \item{loss}{Montant en millions de DKK (corrigé de l'inflation, base 1985).}
#' }
#' @source `fExtremes::danishClaims`, identique à `evir::danish`. McNeil (1997)
#'   annonce 2156 sinistres ; la version diffusée en compte 2167.
#' @references McNeil, A. J. (1997). Estimating the tails of loss severity
#'   distributions using extreme value theory. *ASTIN Bulletin*, 27(1), 117-137.
"danish"
