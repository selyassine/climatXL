devtools::load_all(".", quiet = TRUE)
s <- readRDS("analyse/sortie/s4.rds"); d <- s$d; cnt <- s$cnt; u <- 1
cv <- read.csv("analyse/sortie/covariates.csv")
T96 <- cv$temp_anom[cv$year==1996]; T24 <- cv$temp_anom[cv$year==2024]
f <- gpd_fit_ns(d, u, ~ temp_anom); pf <- poisson_trend(cnt, n ~ temp_anom)$fit
lam <- function(tt) exp(sum(coef(pf) * c(1, tt)))
b <- function(co, tt) exp(co[2] + co[3]*tt)
z96 <- return_level_at(100, f$coef[1], b(f$coef,T96), lam(T96), u)
cat("Centennal climat 1996 :", round(z96,1), "Md$\n")
cat("Periode de retour en climat 2024 (severite + frequence) :", round(return_period(z96, f$coef[1], b(f$coef,T24), lam(T24), u),1), "\n")
cat("Severite seule (lambda constant) :", round(return_period(z96, f$coef[1], b(f$coef,T24), lam(T96), u),1), "\n")
# incertitude : simulation des parametres
set.seed(1); B <- 4000
cs <- MASS::mvrnorm(B, f$coef, f$vcov); ps <- MASS::mvrnorm(B, coef(pf), vcov(pf))
rp <- sapply(1:B, function(i) { co <- cs[i,]; l <- function(tt) exp(sum(ps[i,]*c(1,tt)))
  z <- return_level_at(100, co[1], b(co,T96), l(T96), u); return_period(z, co[1], b(co,T24), l(T24), u) })
print(quantile(rp, c(.025,.5,.975))); cat("P(rp<100) =", mean(rp<100), "\n")
# Modele annee
fy <- gpd_fit_ns(d, u, ~ t); py <- poisson_trend(cnt, n ~ t)$fit
by <- function(tt) exp(fy$coef[2]+fy$coef[3]*tt); ly <- function(tt) exp(sum(coef(py)*c(1,tt)))
z <- return_level_at(100, fy$coef[1], by(-1.4), ly(-1.4), u)
cat("Modele annee: centennal 1996", round(z,1), " RP 2024 :", round(return_period(z, fy$coef[1], by(1.4), ly(1.4), u),1),
 " severite seule:", round(return_period(z, fy$coef[1], by(1.4), ly(-1.4), u),1), "\n")
