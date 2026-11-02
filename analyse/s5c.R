r <- readRDS("analyse/sortie/s5.rds")
png("analyse/sortie/fig/semaine5.png", 1400, 560, res = 130)
par(mfrow = c(1,2), mar = c(4.2,4.5,2.5,1))
hist(r$load[,2], breaks = 40, col = "#9bb8dc", border = "white", xlim = c(-80, 300),
     main = "Chargement 2035 (5 Md$ xs 10 Md$)", xlab = "Chargement (%) — 500 bootstraps", ylab = "Fréquence")
abline(v = 27, col = "#c0392b", lwd = 2); abline(v = 0, lty = 2)
abline(v = quantile(r$load[,2], c(.025,.975), na.rm = TRUE), col = "grey40", lty = 3)
legend("topright", c("estimation : +27 %", "IC 95 % : [−52 % ; +177 %]"), col = c("#c0392b","grey40"), lty = c(1,3), lwd = c(2,1), bty = "n", cex = .85)
obs <- c(41, 12, 3, 1); st <- c(52.9, 13.2, 3.1, 1.2); cl <- c(45.7, 19.9, 6.1, 2.4)
m <- rbind(Observé = obs, Stationnaire = st, Climatique = cl)
barplot(m, beside = TRUE, names.arg = c("> 1 Md$", "> 5 Md$", "> 20 Md$", "> 50 Md$"), log = "y",
        col = c("grey25", "#1f5fa8", "#c0392b"), ylab = "Nombre de sinistres 2013-2024",
        main = "Backtest 2013-2024 (ajusté 1996-2012)", legend.text = TRUE,
        args.legend = list(x = "topright", bty = "n", cex = .85))
dev.off()
