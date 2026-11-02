#' Lire une série FRED (CPI, PIB, ...) et la moyenner par année
#'
#' Lit le CSV renvoyé par `https://fred.stlouisfed.org/graph/fredgraph.csv?id=SERIE`
#' (deux colonnes : date, valeur) et calcule la moyenne annuelle.
#' Séries conseillées : `CPIAUCSL` (indice des prix) et `GDPA` (PIB nominal
#' annuel), qui sert d'indicateur de la richesse exposée.
#'
#' @param file Chemin ou URL du CSV.
#' @param name Nom de la colonne de sortie.
#' @return Un `data.frame` avec `year` et la colonne `name`.
#' @export
read_fred <- function(file, name = "value") {
  d <- utils::read.csv(file, colClasses = "character")
  date <- as.Date(d[[1]])
  val <- suppressWarnings(as.numeric(d[[2]]))   # FRED code les manquants par "."
  ok <- !is.na(date) & !is.na(val)
  yr <- as.integer(format(date[ok], "%Y"))
  m <- tapply(val[ok], yr, mean)
  out <- data.frame(year = as.integer(names(m)), value = as.numeric(m))
  names(out)[2] <- name
  out
}

#' Lire l'anomalie de température globale de la NOAA
#'
#' Lit le CSV « Climate at a Glance » de la NOAA (série globale terre + océan,
#' annuelle) : quelques lignes d'en-tête puis des lignes `année,anomalie`.
#'
#' @param file Chemin ou URL du CSV.
#' @return Un `data.frame` avec `year` et `temp_anom` (°C, écart à la
#'   moyenne du XXe siècle).
#' @export
read_noaa_temp <- function(file) {
  lines <- readLines(file, warn = FALSE)
  lines <- grep("^[0-9]{4}(0[0-9]|1[0-2])?,", lines, value = TRUE)
  parts <- strsplit(lines, ",")
  yr <- as.integer(substr(vapply(parts, `[`, "", 1), 1, 4))
  val <- as.numeric(vapply(parts, `[`, "", 2))
  data.frame(year = yr, temp_anom = val)
}

#' Lire l'indice ENSO (ONI) de la NOAA PSL
#'
#' Lit le format texte de `https://psl.noaa.gov/data/correlation/oni.data` :
#' une ligne par année avec 12 valeurs mensuelles, `-99.90` pour les
#' manquants. Renvoie la moyenne annuelle et la moyenne de la saison des
#' ouragans (juin à novembre), plus pertinente pour les tempêtes de
#' l'Atlantique.
#'
#' @param file Chemin ou URL du fichier.
#' @return Un `data.frame` avec `year`, `enso` (moyenne annuelle) et
#'   `enso_jjason` (juin à novembre).
#' @export
read_oni <- function(file) {
  lines <- readLines(file, warn = FALSE)
  lines <- grep("^\\s*[0-9]{4}(\\s+-?[0-9.]+){12}\\s*$", lines, value = TRUE)
  m <- do.call(rbind, lapply(strsplit(trimws(lines), "\\s+"), as.numeric))
  vals <- m[, -1, drop = FALSE]
  vals[vals <= -99] <- NA
  data.frame(
    year = as.integer(m[, 1]),
    enso = rowMeans(vals),
    enso_jjason = rowMeans(vals[, 6:11, drop = FALSE])
  )
}
