#' Facteurs de normalisation des pertes
#'
#' Calcule, pour chaque année, le facteur qui transforme une perte de cette
#' année en perte équivalente de l'année de base :
#' \itemize{
#'   \item `"cpi"` : inflation seule, \eqn{CPI_{base} / CPI_t} ;
#'   \item `"wealth"` : inflation et richesse exposée, \eqn{W_{base} / W_t},
#'     où \eqn{W} est un indicateur nominal de richesse (par exemple le PIB
#'     nominal). Un ouragan de 1990 est ainsi ramené à ce qu'il coûterait
#'     sur le parc de biens de l'année de base.
#' }
#' La méthode `"wealth"` suit l'esprit de Pielke et al. (2008), qui utilisent
#' inflation × richesse réelle par habitant × population. Le PIB nominal en est
#' une approximation nationale : il ne voit pas que la côte s'urbanise plus
#' vite que le reste du pays. Comparer les deux méthodes montre ce que la
#' normalisation change à la conclusion.
#'
#' @param index `data.frame` avec `year` et les colonnes `cpi` et/ou `wealth`.
#' @param base_year Année de référence.
#' @param method `"cpi"` ou `"wealth"`.
#' @return Un `data.frame` avec `year` et `factor`.
#' @references Pielke, R. A. Jr. et al. (2008). Normalized hurricane damage in
#'   the United States: 1900-2005. *Natural Hazards Review*, 9(1), 29-42.
#' @export
normalisation_factors <- function(index, base_year, method = c("wealth", "cpi")) {
  method <- match.arg(method)
  col <- method
  if (!col %in% names(index)) stop("Colonne `", col, "` absente de `index`.")
  if (!base_year %in% index$year) stop("`base_year` absente de `index`.")
  ref <- index[[col]][index$year == base_year]
  data.frame(year = index$year, factor = ref / index[[col]])
}

#' Normaliser des pertes
#'
#' Multiplie chaque perte par le facteur de son année.
#'
#' @param losses `data.frame` avec au moins `year` et `loss`.
#' @param factors Sortie de [normalisation_factors()].
#' @return `losses` avec deux colonnes en plus : `factor` et `loss_norm`.
#' @export
#' @examples
#' idx <- data.frame(year = 2000:2002, cpi = c(100, 102, 104),
#'                   wealth = c(10, 11, 12))
#' f <- normalisation_factors(idx, base_year = 2002)
#' normalise_losses(data.frame(year = c(2000, 2001), loss = c(1e6, 2e6)), f)
normalise_losses <- function(losses, factors) {
  i <- match(losses$year, factors$year)
  if (anyNA(i)) {
    stop("Pas de facteur pour les annees : ",
         paste(unique(losses$year[is.na(i)]), collapse = ", "))
  }
  losses$factor <- factors$factor[i]
  losses$loss_norm <- losses$loss * losses$factor
  losses
}
