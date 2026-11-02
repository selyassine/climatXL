# Préparation du jeu de données `danish` (sinistres incendie danois 1980-1990).
# Source : objet `danishClaims` du package fExtremes (même série que `evir::danish`),
# montants en millions de couronnes danoises (DKK), base 1985.
library(fExtremes)
data(danishClaims)
danish <- data.frame(
  date = as.Date(as.character(danishClaims[, 1])),
  loss = danishClaims[, 2]
)
save(danish, file = "data/danish.rda", compress = "xz")
