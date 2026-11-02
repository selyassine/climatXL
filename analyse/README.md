# Scripts d'analyse (semaines 3 à 5)

Scripts exploratoires qui ont produit les chiffres et figures du README.
À lancer depuis la racine du dépôt, après `targets::tar_make()` :

```r
dir.create("analyse/sortie/fig", recursive = TRUE)
write.csv(targets::tar_read(losses_norm), "analyse/sortie/losses_norm.csv", row.names = FALSE)
write.csv(targets::tar_read(covariates), "analyse/sortie/covariates.csv", row.names = FALSE)
source("analyse/s3.R")   # puis s3b, s4, s4b, s4c, s5, s5b, s5c
```

Le pipeline `_targets.R` reprend les résultats principaux de façon reproductible.
