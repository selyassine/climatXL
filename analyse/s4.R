devtools::load_all(".", quiet = TRUE)
ev <- readRDS("analyse/sortie/ev.rds"); ev$loss <- ev$loss / 1e9
cv <- read.csv("analyse/sortie/covariates.csv")
d <- merge(ev, cv, by = "year"); d$t <- (d$year - 2010) / 10
d$post2013 <- as.numeric(d$year >= 2013)
u <- 1; yrs <- 1996:2024
cat("cor(year,temp) =", round(cor(cv$year[cv$year %in% yrs], cv$temp_anom[cv$year %in% yrs]),3), "\n")
m0 <- gpd_fit_ns(d, u, ~ 1)
ms <- list(year = ~ t, temp = ~ temp_anom, enso = ~ enso_jjason, temp_enso = ~ temp_anom + enso_jjason, post2013 = ~ post2013)
res <- do.call(rbind, lapply(names(ms), function(k) { f <- gpd_fit_ns(d, u, ms[[k]]); r <- lr_test(m0, f)
  data.frame(model = k, coef = paste(round(f$coef[-(1:2)],3), collapse="/"), se = paste(round(f$se[-(1:2)],3), collapse="/"), xi = round(f$coef[1],3), r) }))
cat("Severite (echelle) :\n"); print(res, digits = 3); cat("AIC m0", m0$aic, "\n")
cnt <- exceedance_counts(ev, u, yrs); cnt <- merge(cnt, cv, by = "year"); cnt$t <- (cnt$year-2010)/10; cnt$post2013 <- as.numeric(cnt$year>=2013)
print(cnt$n)
for (fm in list(n ~ t, n ~ temp_anom, n ~ enso_jjason, n ~ post2013)) { r <- poisson_trend(cnt, fm)
  cat(deparse(fm), " coef", round(coef(r$fit)[2],3), " se", round(sqrt(diag(vcov(r$fit)))[2],3), " p", signif(r$lr$p_value,3), " disp", round(r$dispersion,2), " AIC", round(r$lr$aic0,1), round(r$lr$aic1,1), "\n") }
# Counts at other thresholds
for (uu in c(0.5, 2, 5)) { c2 <- merge(exceedance_counts(ev, uu, yrs), cv); r <- poisson_trend(c2, n ~ temp_anom); cat("u",uu," temp coef",round(coef(r$fit)[2],3)," p",signif(r$lr$p_value,3),"\n")}
saveRDS(list(d=d,cnt=cnt), "analyse/sortie/s4.rds")
