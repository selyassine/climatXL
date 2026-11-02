#' Lister les fichiers NOAA Storm Events disponibles
#'
#' Lit l'index du serveur NCEI et renvoie, pour chaque année, l'URL du
#' fichier « details » le plus récent (un fichier CSV compressé par année).
#'
#' @param years Années voulues.
#' @param base_url Répertoire NCEI des fichiers CSV.
#' @return Un `data.frame` avec `year`, `file` et `url`.
#' @export
#' @examples
#' \dontrun{
#' noaa_storm_files(1990:2024)
#' }
noaa_storm_files <- function(years,
                             base_url = "https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/") {
  html <- paste(readLines(base_url, warn = FALSE), collapse = "\n")
  files <- unique(regmatches(html, gregexpr(
    "StormEvents_details-ftp_v1\\.0_d[0-9]{4}_c[0-9]{8}\\.csv\\.gz", html))[[1]])
  storm_file_table(files, years, base_url)
}

storm_file_table <- function(files, years, base_url) {
  yr <- as.integer(sub(".*_d([0-9]{4})_c.*", "\\1", files))
  created <- sub(".*_c([0-9]{8})\\.csv\\.gz", "\\1", files)
  keep <- yr %in% years
  df <- data.frame(year = yr[keep], file = files[keep], created = created[keep])
  # Plusieurs versions par annee : on garde la plus recente.
  df <- df[order(df$year, df$created, decreasing = TRUE), ]
  df <- df[!duplicated(df$year), ]
  df <- df[order(df$year), ]
  missing <- setdiff(years, df$year)
  if (length(missing)) warning("Annees absentes de l'index : ", paste(missing, collapse = ", "))
  data.frame(year = df$year, file = df$file, url = paste0(base_url, df$file))
}

#' Télécharger les fichiers NOAA Storm Events
#'
#' @param years Années voulues.
#' @param dest Dossier de destination. Les fichiers déjà présents ne sont pas
#'   retéléchargés.
#' @return Les chemins des fichiers locaux, de façon invisible.
#' @export
#' @examples
#' \dontrun{
#' download_storm_events(1990:2024, dest = "data-raw/noaa")
#' }
download_storm_events <- function(years, dest = "data-raw/noaa") {
  dir.create(dest, showWarnings = FALSE, recursive = TRUE)
  tab <- noaa_storm_files(years)
  paths <- file.path(dest, tab$file)
  for (i in seq_len(nrow(tab))) {
    if (!file.exists(paths[i])) {
      message("Telechargement ", tab$year[i])
      utils::download.file(tab$url[i], paths[i], mode = "wb", quiet = TRUE)
    }
  }
  invisible(paths)
}

#' Convertir les montants de dommages NOAA en dollars
#'
#' La NOAA code les dommages sous forme de texte : `"25K"`, `"1.5M"`,
#' `"2B"`, `"0.00K"`. Les valeurs vides ou illisibles deviennent `NA`.
#'
#' @param x Vecteur de caractères.
#' @return Vecteur numérique en dollars courants.
#' @export
#' @examples
#' parse_damage(c("25K", "1.5M", "2B", "0", "", "350"))
parse_damage <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x[x %in% c("", "NA")] <- NA_character_
  unit <- sub("^[0-9.]*", "", x)
  num_txt <- sub("[A-Z]+$", "", x)
  num <- suppressWarnings(as.numeric(num_txt))
  # "K" seul signifie un montant non renseigne.
  num[!is.na(x) & num_txt == ""] <- NA_real_
  mult <- c(H = 1e2, K = 1e3, M = 1e6, B = 1e9)[unit]
  mult[unit == ""] <- 1
  out <- num * unname(mult)
  out[is.na(x)] <- NA_real_
  out
}

#' Lire les fichiers NOAA Storm Events
#'
#' Lit un ou plusieurs fichiers « details » (CSV, éventuellement `.gz`) et
#' garde les colonnes utiles à l'analyse des sinistres.
#'
#' @param files Chemins des fichiers.
#' @return Un `data.frame` avec `event_id`, `episode_id`, `state`, `year`,
#'   `begin_date`, `event_type`, `damage_property`, `damage_crops` (dollars
#'   courants).
#' @export
read_storm_events <- function(files) {
  cols <- c("EVENT_ID", "EPISODE_ID", "STATE", "YEAR", "BEGIN_YEARMONTH", "BEGIN_DAY",
            "EVENT_TYPE", "DAMAGE_PROPERTY", "DAMAGE_CROPS")
  one <- function(f) {
    d <- utils::read.csv(f, colClasses = "character", na.strings = "",
                         check.names = FALSE)
    miss <- setdiff(cols, names(d))
    if (length(miss)) stop("Colonnes absentes dans ", f, " : ", paste(miss, collapse = ", "))
    d[cols]
  }
  d <- do.call(rbind, lapply(files, one))
  data.frame(
    event_id = as.integer(d$EVENT_ID),
    episode_id = as.integer(d$EPISODE_ID),
    state = d$STATE,
    year = as.integer(d$YEAR),
    # BEGIN_DATE_TIME a une annee sur deux chiffres : on reconstruit la date.
    begin_date = as.Date(sprintf("%s-%s-%02d", substr(d$BEGIN_YEARMONTH, 1, 4),
                                 substr(d$BEGIN_YEARMONTH, 5, 6),
                                 as.integer(d$BEGIN_DAY))),
    event_type = d$EVENT_TYPE,
    damage_property = parse_damage(d$DAMAGE_PROPERTY),
    damage_crops = parse_damage(d$DAMAGE_CROPS),
    stringsAsFactors = FALSE
  )
}

#' Regrouper les événements NOAA en sinistres
#'
#' Un traité catastrophe couvre un événement (une tempête), pas chaque
#' rapport local. La NOAA regroupe les rapports d'un même système météo dans
#' un épisode (`episode_id`). On somme donc les dommages par épisode. Les
#' événements sans épisode restent seuls.
#'
#' @param events Sortie de [read_storm_events()].
#' @param crops Inclure les dommages aux cultures (par défaut non : la
#'   réassurance de biens couvre surtout les dommages matériels).
#' @return Un `data.frame` avec `episode_id`, `year`, `date`, `n_events`,
#'   `n_states`, `main_type` (type le plus coûteux) et `loss` (dollars
#'   courants). Les épisodes sans dommage sont exclus.
#' @export
aggregate_episodes <- function(events, crops = FALSE) {
  dmg <- ifelse(is.na(events$damage_property), 0, events$damage_property)
  if (crops) dmg <- dmg + ifelse(is.na(events$damage_crops), 0, events$damage_crops)
  key <- ifelse(is.na(events$episode_id), -events$event_id, events$episode_id)
  # Calcul vectorise (une boucle par episode prenait 30 min sur 1,8 M de lignes).
  keys <- sort(unique(key))
  g <- match(key, keys)
  loss <- as.numeric(rowsum(dmg, g, reorder = TRUE))
  n_events <- tabulate(g, length(keys))
  ord <- order(g, events$begin_date)
  d1 <- ord[!duplicated(g[ord])]
  date <- events$begin_date[d1]
  year <- as.integer(format(date, "%Y"))
  year[is.na(year)] <- events$year[d1][is.na(year)]
  st <- unique(data.frame(g = g, s = events$state))
  n_states <- tabulate(st$g, length(keys))
  # Type le plus couteux par episode.
  gt <- paste(g, events$event_type, sep = "\r")
  by_type <- rowsum(dmg, gt)
  bt_g <- as.integer(sub("\r.*", "", rownames(by_type)))
  bt_type <- sub("^[^\r]*\r", "", rownames(by_type))
  o <- order(bt_g, -by_type[, 1])
  top <- o[!duplicated(bt_g[o])]
  main_type <- character(length(keys))
  main_type[bt_g[top]] <- bt_type[top]
  ep <- events$episode_id[d1]
  out <- data.frame(episode_id = ep, year = year, date = date,
                    n_events = n_events, n_states = n_states,
                    main_type = main_type, loss = loss,
                    stringsAsFactors = FALSE)
  out <- out[out$loss > 0, ]
  out <- out[order(out$date), ]
  rownames(out) <- NULL
  out
}
