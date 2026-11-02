devtools::load_all(".", quiet = TRUE)
s <- readRDS("analyse/sortie/s4.rds"); d <- s$d; cnt <- s$cnt; u <- 1
cv <- read.csv("analyse/sortie/covariates.csv")
yrs <- 1996:2024; Ty <- length(yrs)
tr <- lm(temp_anom ~ year, cv[cv$year %in% yrs, ]); T35 <- predict(tr, data.frame(year = 2035)); T24 <- cv$temp_anom[cv$year==2024]
Tfit24 <- predict(tr, data.frame(year=2024))
cat("tendance temp:", round(coef(tr)[2]*10,3), "C/decennie ; T2024 obs", T24, " tendance 2024", round(Tfit24,2), " T2035", round(T35,2), "\n")
fs <- gpd_fit(d$loss, u, n_years = Ty); lam0 <- fs$n_exceed / Ty
fn <- gpd_fit_ns(d, u, ~ temp_anom); pf <- poisson_trend(cnt, n ~ temp_anom)$fit
lamT <- function(tt) exp(sum(coef(pf)*c(1,tt))); betT <- function(tt) exp(fn$coef[2]+fn$coef[3]*tt)
price <- function(a, l) {
  sc <- list(
    "Stationnaire" = c(fs$par[["xi"]], fs$par[["beta"]], lam0),
    "Climat 2024 (severite seule)" = c(fn$coef[1], betT(Tfit24), lam0),
    "Climat 2024" = c(fn$coef[1], betT(Tfit24), lamT(Tfit24)),
    "2035 (severite seule)" = c(fn$coef[1], betT(T35), lam0),
    "2035" = c(fn$coef[1], betT(T35), lamT(T35)))
  do.call(rbind, lapply(names(sc), function(k) { p <- sc[[k]]
    sim <- simulate_layer(p[1], p[2], p[3], u, a, l, n_sim = 2e5, seed = 1)
    r <- layer_risk(sim)
    data.frame(scenario = k, xi = p[1], beta = p[2], lambda = p[3],
      prime = layer_premium(p[1], p[2], p[3], u, a, l) * 1000, var995 = r$var*1000, prob_hit = r$prob_hit) }))
}
for (L in list(c(1, 0.5), c(10, 5))) { cat("\n== Tranche", L[2], "Md$ xs", L[1], "Md$ (montants en M$) ==\n")
  r <- price(L[1], L[2]); r$charg <- round(100*(r$prime/r$prime[1]-1),1); print(r, digits = 3, row.names = FALSE) }
# Bootstrap stationnaire
ev <- d[, c("year","loss")]
bp <- bootstrap_premium(ev, yrs, u, 1, 0.5, B = 1000); cat("\nBootstrap prime stat 0.5xs1 (M$):", round(quantile(bp*1000, c(.025,.5,.975)),1), "\n")
bp2 <- bootstrap_premium(ev, yrs, u, 10, 5, B = 1000); cat("Bootstrap prime stat 5xs10 (M$):", round(quantile(bp2*1000, c(.025,.5,.975)),1), "\n")
# Bootstrap chargement climatique 2035
set.seed(2); by <- split(seq_len(nrow(d)), factor(d$year, levels = yrs)); tcov <- setNames(cv$temp_anom, cv$year)
load <- t(replicate(500, { yy <- sample(yrs, replace = TRUE)
  idx <- unlist(by[as.character(yy)]); db <- d[idx, ]
  f1 <- tryCatch(gpd_fit_ns(db, u, ~ temp_anom), error = function(e) NULL); if (is.null(f1)) return(c(NA,NA))
  cb <- data.frame(year = yy, n = sapply(yy, function(y) sum(d$year==y & d$loss>u)), temp_anom = tcov[as.character(yy)])
  pb <- glm(n ~ temp_anom, poisson, cb); f0 <- gpd_fit(db$loss, u)
  p0 <- layer_premium(f0$par[["xi"]], f0$par[["beta"]], f0$n_exceed/Ty, u, 1, 0.5)
  sapply(list(c(1,0.5), c(10,5)), function(L) {
    p0 <- layer_premium(f0$par[["xi"]], f0$par[["beta"]], f0$n_exceed/Ty, u, L[1], L[2])
    p1 <- layer_premium(f1$coef[1], exp(f1$coef[2]+f1$coef[3]*T35), exp(sum(coef(pb)*c(1,T35))), u, L[1], L[2]); 100*(p1/p0-1) }) }))
cat("Chargement 2035 bootstrap 0.5xs1 (%):", round(quantile(load[,1], c(.025,.5,.975), na.rm=TRUE),1), " P(>0)=", round(mean(load[,1]>0,na.rm=T),2), "\n")
cat("Chargement 2035 bootstrap 5xs10 (%):", round(quantile(load[,2], c(.025,.5,.975), na.rm=TRUE),1), " P(>0)=", round(mean(load[,2]>0,na.rm=T),2), "\n")
saveRDS(list(bp=bp,bp2=bp2,load=load,T35=T35,Tfit24=Tfit24), "analyse/sortie/s5.rds")
