# ============================================================
# 04_model.R - Which factors drive congestion? (logistic regression + decision tree)
# ============================================================
suppressPackageStartupMessages({library(dplyr); library(readr); library(rpart); library(ggplot2)})
set.seed(42)
df <- read_csv("data/clean/junction_traffic_clean.csv", show_col_types = FALSE) %>%
  mutate(period = factor(period, levels = c("Off-Peak/Night","Midday","Morning Peak","Evening Peak")))
mar <- "#7A1F3D"

# split 70/30 -------------------------------------------------------------------
idx <- sample(nrow(df), 0.7 * nrow(df)); train <- df[idx, ]; test <- df[-idx, ]
cat("Train rows:", nrow(train), " Test rows:", nrow(test), "\n")

# standardise continuous predictors so effects are comparable -------------------
cont <- c("vehicle_count","heavy_vehicle_pct","pedestrian_count","green_ratio")
mu <- sapply(train[cont], mean); sdv <- sapply(train[cont], sd)
scale_df <- function(d) { for (v in cont) d[[v]] <- (d[[v]] - mu[v]) / sdv[v]; d }
tr_s <- scale_df(train); te_s <- scale_df(test)

f_cls <- is_congested ~ vehicle_count + heavy_vehicle_pct + pedestrian_count + green_ratio + rain_flag +
  road_work + accident + signal_fault + event_nearby + illegal_parking + is_weekend + is_holiday + period
glm1 <- suppressWarnings(glm(f_cls, data = tr_s, family = binomial))

# --- A. classifier accuracy (does the data explain congestion?) -------------------
auc <- function(y, p) { r <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0); (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0) }
pr <- predict(glm1, te_s, type = "response"); cls <- as.integer(pr > 0.5)
cm <- table(Actual = te_s$is_congested, Predicted = cls)
cat("\n=== A. LOGISTIC CLASSIFIER - TEST SET ===\n"); print(cm)
acc <- sum(diag(cm)) / sum(cm); prec <- cm[2, 2] / sum(cm[, 2]); rec <- cm[2, 2] / sum(cm[2, ])
cat(sprintf("Accuracy %.3f | Precision %.3f | Recall %.3f | F1 %.3f | AUC %.3f\n", acc, prec, rec, 2 * prec * rec / (prec + rec), auc(te_s$is_congested, pr)))

# --- B. driver analysis: OLS on log(delay) -> % effect on delay -------------------
f_lm <- log(delay_min) ~ vehicle_count + heavy_vehicle_pct + pedestrian_count + green_ratio + rain_flag +
  road_work + accident + signal_fault + event_nearby + illegal_parking + is_weekend + is_holiday + period
lm1 <- lm(f_lm, data = tr_s)
cat("\n=== B. LINEAR MODEL ON log(delay): fit ===\n")
cat(sprintf("R-squared (train) %.3f | Adjusted R-squared %.3f\n", summary(lm1)$r.squared, summary(lm1)$adj.r.squared))
pl <- predict(lm1, te_s); ss_res <- sum((log(te_s$delay_min) - pl)^2); ss_tot <- sum((log(te_s$delay_min) - mean(log(te_s$delay_min)))^2)
cat(sprintf("R-squared (test) %.3f | RMSE (log scale) %.3f\n", 1 - ss_res / ss_tot, sqrt(ss_res / nrow(te_s))))
co <- summary(lm1)$coefficients
eff <- data.frame(term = rownames(co), pct_change_in_delay = round(100 * (exp(co[, 1]) - 1), 1), p_value = signif(co[, 4], 3), row.names = NULL)
labs <- c(vehicle_count = "Traffic volume (+1 SD)", heavy_vehicle_pct = "Heavy-vehicle share (+1 SD)", pedestrian_count = "Pedestrian volume (+1 SD)",
  green_ratio = "Green-time ratio (+1 SD)", rain_flag = "Rain", road_work = "Road work", accident = "Accident", signal_fault = "Signal fault",
  event_nearby = "Nearby event", illegal_parking = "Illegal parking", is_weekend = "Weekend", is_holiday = "Public holiday",
  periodMidday = "Midday (vs night)", `periodMorning Peak` = "Morning peak (vs night)", `periodEvening Peak` = "Evening peak (vs night)")
eff$label <- ifelse(eff$term %in% names(labs), labs[eff$term], eff$term); eff <- eff[eff$term != "(Intercept)", ]
cat("\nEffect on delay (holding other factors constant):\n"); print(eff %>% select(label, pct_change_in_delay, p_value))
write_csv(eff, "outputs/delay_effects.csv")

# --- importance = partial sum of squares lost when the factor is removed -----------
d1 <- drop1(lm1); ss <- d1$`Sum of Sq`[-1]
imp <- data.frame(factor = rownames(d1)[-1], ss = ss)
short <- c(vehicle_count = "Traffic volume", heavy_vehicle_pct = "Heavy-vehicle share", pedestrian_count = "Pedestrian volume",
  green_ratio = "Green-time ratio", rain_flag = "Rain", road_work = "Road work", accident = "Accident", signal_fault = "Signal fault",
  event_nearby = "Nearby event", illegal_parking = "Illegal parking", is_weekend = "Weekend", is_holiday = "Public holiday", period = "Time period")
imp$label <- short[imp$factor]; imp <- imp %>% arrange(desc(ss)); imp$share_pct <- round(100 * imp$ss / sum(imp$ss), 1)
cat("\n=== FACTOR IMPORTANCE (share of explained variation lost if factor removed) ===\n"); print(imp %>% select(label, share_pct))
write_csv(imp, "outputs/factor_importance.csv")
p <- ggplot(imp, aes(reorder(label, ss), share_pct)) + geom_col(fill = mar, width = 0.7) + coord_flip() +
  geom_text(aes(label = paste0(share_pct, "%")), hjust = -0.1, size = 3.4) + scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "What drives congestion? Factor importance", subtitle = "Share of explained variation in delay attributable to each factor",
       x = NULL, y = "Importance (%)") + theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", colour = mar), panel.grid.minor = element_blank())
ggsave("figures/fig9_importance.png", p, width = 8, height = 5, dpi = 200, bg = "white")

# decision tree -----------------------------------------------------------------------
tree <- rpart(is_congested ~ vehicle_count + heavy_vehicle_pct + pedestrian_count + green_ratio + rain_flag + road_work + accident +
              signal_fault + event_nearby + illegal_parking + is_weekend + hour, data = train, method = "class",
              control = rpart.control(cp = 0.01, maxdepth = 4))
tp <- predict(tree, test, type = "class"); tcm <- table(Actual = test$is_congested, Predicted = tp)
cat("\n=== DECISION TREE ===\n"); print(tcm); cat(sprintf("Tree accuracy: %.3f\n", sum(diag(tcm)) / sum(tcm)))
cat("\nVariable importance (tree):\n"); print(round(100 * tree$variable.importance / sum(tree$variable.importance), 1))
png("figures/fig10_tree.png", width = 1900, height = 1200, res = 200, bg = "white")
par(mar = c(1, 1, 3, 1)); plot(tree, uniform = TRUE, margin = 0.08, branch = 0.6); text(tree, use.n = TRUE, cex = 0.75, fancy = FALSE)
title("Decision tree: when does the junction become congested?", col.main = mar); dev.off()

# threshold: at what volume does congestion start? -----------------------------------------------
cat("\n=== % CONGESTED BY VOLUME BAND ===\n")
vb <- df %>% mutate(band = cut(vehicle_count, c(0, 400, 800, 1000, 1200, 1400, Inf), dig.lab = 4)) %>% group_by(band) %>%
  summarise(hours = n(), pct_congested = round(100 * mean(is_congested), 1)); print(as.data.frame(vb))
write_csv(vb, "outputs/volume_band.csv")

print(p1)  # Hourly Delay[cite: 3]
print(p2)  # Delay Heatmap[cite: 3]
print(p3)  # Congestion Mix[cite: 3]
print(p4)  # Factor Impact[cite: 3]
print(p5)  # Weather Boxplot[cite: 3]
print(p6)  # Speed vs Volume[cite: 3]
print(p8)  # Monthly Trend[cite: 3]
