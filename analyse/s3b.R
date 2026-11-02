devtools::load_all(".", quiet = TRUE)
ev <- readRDS("analyse/sortie/ev.rds")
x <- ev$loss / 1e9; T <- 29
fit <- gpd_fit(x, 1, n_years = T); print(fit)
rl <- return_level(fit, c(10, 50, 100, 200), ci = TRUE); print(rl)
# empirical check: largest losses vs return periods
cat("max obs:", max(x), " 2nd:", sort(x,TRUE)[2], "\n")
# Katrina return period under fit
lam <- fit$n_exceed / T; xi <- fit$par[["xi"]]; b <- fit$par[["beta"]]
rp <- function(z) 1/(lam * (1 + xi*(z-1)/b)^(-1/xi))
cat("RP Katrina:", rp(max(x)), " RP Harvey:", rp(sort(x,TRUE)[2]), " RP 10Md:", rp(10), "\n")
# sensitivity without Katrina
f2 <- gpd_fit(x[x < max(x)], 1, n_years = T); print(round(f2$par,3)); print(return_level(f2, c(100)))
# sensitivity threshold
for (u in c(0.5, 2)) { f <- gpd_fit(x, u, n_years=T); cat(u, round(f$par,3), round(return_level(f,c(10,100))$return_level,1), "\n") }
# Diagnostics: QQ plot of exceedances
y <- sort(fit$exceedances - 1); n <- length(y); pp <- (1:n)/(n+1)
q <- b/xi*((1-pp)^(-xi)-1)
png("analyse/sortie/fig/semaine3.png", 1400, 1000, res = 130)
par(mfrow = c(2,2), mar = c(4.2,4.2,2.5,1))
me <- mean_excess(x, thresholds = exp(seq(log(0.1), log(15), length = 60)), min_exceed = 8)
plot(me$threshold, me$mean_excess, log = "x", type = "l", lwd = 2, col = "#1f5fa8",
     xlab = "Seuil u (Md$ 2024)", ylab = "Excès moyen (Md$)", main = "Excès moyens",
     ylim = range(c(me$lower, me$upper)))
lines(me$threshold, me$lower, lty = 2, col = "grey50"); lines(me$threshold, me$upper, lty = 2, col = "grey50")
abline(v = 1, col = "#c0392b", lty = 3)
st <- threshold_stability(x, exp(seq(log(0.2), log(6), length = 25)))
plot(st$threshold, st$xi, log = "x", type = "b", pch = 19, cex = .6, ylim = c(-0.2, 1.6),
     xlab = "Seuil u (Md$ 2024)", ylab = expression(hat(xi)), main = "Stabilité de xi selon le seuil", col = "#1f5fa8")
arrows(st$threshold, st$xi_lower, st$threshold, st$xi_upper, length = 0, col = "grey60")
abline(v = 1, col = "#c0392b", lty = 3); abline(h = 1, col = "grey40", lty = 2)
plot(q, y, pch = 19, cex = .6, col = "#1f5fa8", log = "xy",
     xlab = "Quantiles GPD ajustée", ylab = "Excès observés", main = "QQ-plot des excès au-dessus de 1 Md$")
abline(0, 1, col = "#c0392b")
per <- exp(seq(log(1/lam*1.05), log(500), length = 80))
r <- return_level(fit, per, ci = TRUE)
emp_rp <- T / seq_along(sort(x, TRUE))
plot(per, r$return_level, log = "xy", type = "l", lwd = 2, col = "#1f5fa8",
     ylim = c(1, max(r$upper[is.finite(r$upper)], max(x))), xlab = "Période de retour (années)",
     ylab = "Sinistre (Md$ 2024)", main = "Niveaux de retour (IC 95 % profilé)")
lines(per, r$lower, lty = 2, col = "grey50"); lines(per, pmin(r$upper, 1e6), lty = 2, col = "grey50")
points(emp_rp[sort(x,TRUE) > 1], sort(x, TRUE)[sort(x,TRUE) > 1], pch = 19, cex = .5)
dev.off()
