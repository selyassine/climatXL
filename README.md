# climatXL

> Le climat rend-il la réassurance catastrophe plus chère ?

Ce package teste si la loi des sinistres extrêmes change avec le temps et le
climat, puis traduit la réponse en prime de traité en excédent de sinistre.
La conclusion peut aussi être « pas de tendance significative » : elle doit
seulement être démontrée.

## État d'avancement

| Semaine | Travail | État |
| --- | --- | --- |
| 1 | Package, CI, reproduction de McNeil (1997) | fait |
| 2 | Données NOAA, normalisation des pertes | à faire |
| 3 | Modèle stationnaire | à faire |
| 4 | Modèle non stationnaire et tests | à faire |
| 5 | Tarification, scénarios, bootstrap, backtest | à faire |
| 6 | Rapport Quarto, application Shiny | à faire |

## Installation

```r
# install.packages("remotes")
remotes::install_github("selyassine/climatXL")
```

## Validation sur les sinistres incendie danois

```r
library(climatXL)
fit <- gpd_fit(danish$loss, threshold = 10, n_years = 11)
fit                                   # xi = 0,497 (0,136), beta = 6,97 (1,11)
gpd_risk(fit, c(0.99, 0.999))         # VaR 99 % = 27,3 ; ES 99 % = 58,2
gpd_quantile_ci(fit, 0.999)           # 94,3 [63,2 ; 189,1] par vraisemblance profilée
```

| Seuil u | Dépassements | xi (climatXL) | xi (McNeil) | x₀.₉₉₉ (climatXL) | x₀.₉₉₉ (McNeil) |
| --- | --- | --- | --- | --- | --- |
| 3 | 532 | 0,668 | 0,67 | 129 | 129 |
| 4 | 362 | 0,720 | 0,72 | 146 | 147 |
| 5 | 254 | 0,632 | 0,63 | 121 | 122 |
| 10 | 109 | 0,497 | 0,50 | 94 | 95 |
| 20 | 36 | 0,684 | 0,68 | 102 | 103 |

Ces chiffres sont vérifiés par `tests/testthat/test-mcneil1997.R`, et
l'ajustement est recoupé avec `evd::fpot` et `fExtremes::gpdFit`.
La série diffusée compte 2167 sinistres contre 2156 dans l'article, d'où
les petits écarts.

## Pipeline

```r
targets::tar_make()
```

## Référence

McNeil, A. J. (1997). Estimating the tails of loss severity distributions
using extreme value theory. *ASTIN Bulletin*, 27(1), 117-137.
