devtools::load_all(".", quiet = TRUE)
s <- readRDS("analyse/sortie/s4.rds"); d <- s$d; cnt <- s$cnt; u <- 1
cv <- read.csv("analyse/sortie/covariates.csv")
tr <- d[d$year <= 2012, ]; te <- d[d$year >= 2013, ]; Ttr <- 17; Tte <- 12
fs <- gpd_fit(tr$loss, u); lam <- fs$n_exceed / Ttr
fn <- gpd_fit_ns(tr, u, ~ temp_anom); pf <- poisson_trend(cnt[cnt$year<=2012,], n ~ temp_anom)$fit
tt <- cv$temp_anom[cv$year %in% 2013:2024]
cat("Train: xi", round(fs$par,3), " lambda", round(lam,2), "\n")
for (z in c(1, 5, 20, 50)) {
  S0 <- (1 + fs$par[["xi"]]*(z-u)/fs$par[["beta"]])^(-1/fs$par[["xi"]])
  e0 <- Tte * lam * S0
  e1 <- sum(sapply(tt, function(t) exp(sum(coef(pf)*c(1,t))) * (1 + fn$coef[1]*(z-u)/exp(fn$coef[2]+fn$coef[3]*t))^(-1/fn$coef[1])))
  obs <- sum(te$loss > z)
  cat(sprintf("> %2d Md$ : observe %3d | stationnaire %6.1f [%d-%d] p=%.3f | climat %6.1f [%d-%d]\n", z, obs, e0, qpois(.025,e0), qpois(.975,e0),
     min(1, 2*min(ppois(obs, e0), 1-ppois(obs-1, e0))), e1, qpois(.025,e1), qpois(.975,e1)))
}
for (L in list(c(1,0.5), c(10,5))) {
  p0 <- layer_premium(fs$par[["xi"]], fs$par[["beta"]], lam, u, L[1], L[2])
  sim <- replicate(20000, sum(simulate_layer(fs$par[["xi"]], fs$par[["beta"]], lam, u, L[1], L[2], n_sim = Tte)))
  obs <- sum(layer_loss(te$loss, L[1], L[2]))
  cat(sprintf("Tranche %.1f xs %g : charge 2013-2024 observee %.2f Md$ ; prevue %.2f Md$ ; rang dans la simulation %.2f\n", L[2], L[1], obs, p0*Tte, mean(sim <= obs)))
}
