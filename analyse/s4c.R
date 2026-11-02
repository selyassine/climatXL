devtools::load_all(".", quiet = TRUE)
s <- readRDS("analyse/sortie/s4.rds"); d <- s$d; cnt <- s$cnt; u <- 1
cv <- read.csv("analyse/sortie/covariates.csv"); cv <- cv[cv$year>=1996,]
f <- gpd_fit_ns(d, u, ~ temp_anom); pf <- poisson_trend(cnt, n ~ temp_anom)$fit; py <- poisson_trend(cnt, n ~ t)$fit
png("analyse/sortie/fig/semaine4.png", 1400, 560, res = 130)
par(mfrow = c(1,2), mar = c(4.2,4.2,2.5,1))
plot(cnt$year, cnt$n, type = "h", lwd = 6, col = "#9bb8dc", xlab = "Année", ylab = "Sinistres > 1 Md$",
     main = "Fréquence : légère baisse, non significative")
lines(cnt$year, fitted(py), col = "#1f5fa8", lwd = 2)
lines(cnt$year, fitted(pf), col = "#c0392b", lwd = 2, lty = 2)
legend("topright", c("Poisson ~ année", "Poisson ~ température"), col = c("#1f5fa8","#c0392b"), lty = 1:2, lwd = 2, bty = "n", cex = .85)
exc <- d[d$loss > u, ]
plot(exc$date, exc$loss, log = "y", pch = 19, cex = .6, col = "#1f5fa8", xlab = "Année", ylab = "Sinistre (Md$ 2024)",
     main = "Sévérité : échelle ~ température (p = 0,16)")
bb <- gpd_ns_scale(f, cv); xi <- f$coef[1]
for (p in c(0.5, 0.9)) lines(as.Date(paste0(cv$year,"-07-01")), u + bb/xi*((1-p)^(-xi)-1), col = "#c0392b", lwd = 2, lty = ifelse(p==.5,1,2))
legend("topleft", c("médiane des excès", "quantile 90 %"), col = "#c0392b", lty = 1:2, lwd = 2, bty = "n", cex = .85)
dev.off()
