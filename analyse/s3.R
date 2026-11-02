devtools::load_all(".", quiet = TRUE)
d <- read.csv("analyse/sortie/losses_norm.csv")
d <- d[d$year >= 1996, ]
for (m in c(2.5e7, 5e7, 1e8)) {
  ev <- hours_clause(d, 72, min_loss = m)
  cat("min_loss", m/1e6, "M: events", nrow(ev), " >1Md:", sum(ev$loss > 1e9), " merged>1:", sum(ev$n_episodes>1), "\n")
}
ev <- hours_clause(d, 72, min_loss = 5e7)
x <- ev$loss / 1e9   # milliards USD 2024
print(head(ev[order(-ev$loss), c("date","main_type","n_episodes","loss")], 12))
st <- threshold_stability(x, c(0.25, 0.5, 0.75, 1, 1.5, 2, 3, 5))
print(st)
h <- hill(x, k = c(30, 50, 75, 100, 150, 200))
print(h)
me <- mean_excess(x, thresholds = c(0.25,0.5,1,2,3,5,8), min_exceed = 5); print(me)
saveRDS(ev, "analyse/sortie/ev.rds")
