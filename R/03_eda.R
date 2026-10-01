# ============================================================
# 03_eda.R - Exploratory data analysis & figures
# ============================================================
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(readr); library(ggplot2); library(scales); library(corrplot)})
df <- read_csv("data/clean/junction_traffic_clean.csv", show_col_types = FALSE) %>%
  mutate(day_name = factor(day_name, levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")),
         period = factor(period, levels = c("Morning Peak","Midday","Evening Peak","Off-Peak/Night")),
         congestion_level = factor(congestion_level, levels = c("Low","Moderate","High","Severe")),
         month_name = factor(month_name, levels = c("Apr","May","Jun","Jul","Aug","Sep")))
mar <- "#7A1F3D"; gold <- "#C9A227"; grey <- "#5B6770"
pal <- c(Low = "#4C9F70", Moderate = "#E3B23C", High = "#E07B39", Severe = "#B0243B")
th <- theme_minimal(base_size = 12, base_family = "sans") +
  theme(plot.title = element_text(face = "bold", colour = mar), panel.grid.minor = element_blank(),
        legend.position = "bottom")
save <- function(p, f, w = 8, h = 4.6) ggsave(file.path("figures", f), p, width = w, height = h, dpi = 200, bg = "white")

cat("=== STRUCTURE ===\n"); glimpse(df)
cat("\n=== SUMMARY (key variables) ===\n")
print(summary(df %>% select(vehicle_count, avg_speed_kmph, queue_length_m, delay_min, heavy_vehicle_pct, green_ratio)))
cat("\n=== CONGESTION LEVEL SHARE ===\n"); print(round(prop.table(table(df$congestion_level)) * 100, 1))
cat("\n=== BY PERIOD ===\n")
by_period <- df %>% group_by(period) %>% summarise(hours = n(), avg_vehicles = round(mean(vehicle_count)),
  avg_speed = round(mean(avg_speed_kmph), 1), avg_queue_m = round(mean(queue_length_m)), avg_delay_min = round(mean(delay_min), 1),
  pct_congested = round(mean(is_congested) * 100, 1)); print(by_period)

# 1 hourly profile ---------------------------------------------------------
h <- df %>% mutate(dtype = ifelse(is_weekend == 1, "Weekend", "Weekday")) %>% group_by(hour, dtype) %>%
  summarise(delay = mean(delay_min), vehicles = mean(vehicle_count), .groups = "drop")
p1 <- ggplot(h, aes(hour, delay, colour = dtype)) + geom_line(linewidth = 1.2) + geom_point(size = 1.8) +
  scale_colour_manual(values = c(Weekday = mar, Weekend = gold), name = NULL) + scale_x_continuous(breaks = seq(0, 23, 3)) +
  labs(title = "Average delay by hour of day", x = "Hour of day", y = "Average delay (min)") + th
save(p1, "fig1_hourly_delay.png")

# 2 heatmap -----------------------------------------------------------------
hm <- df %>% group_by(day_name, hour) %>% summarise(delay = mean(delay_min), .groups = "drop")
p2 <- ggplot(hm, aes(hour, day_name, fill = delay)) + geom_tile(colour = "white") +
  scale_fill_gradient(low = "#FBF3E4", high = "#7A1F3D", name = "Delay (min)") + scale_y_discrete(limits = rev) +
  scale_x_continuous(breaks = seq(0, 23, 3)) + labs(title = "Delay heatmap: day of week x hour", x = "Hour", y = NULL) + th
save(p2, "fig2_heatmap.png")

# 3 congestion mix by period ---------------------------------------------------
mix <- df %>% count(period, congestion_level) %>% group_by(period) %>% mutate(p = n / sum(n))
p3 <- ggplot(mix, aes(period, p, fill = congestion_level)) + geom_col() + scale_fill_manual(values = pal, name = "Level") +
  scale_y_continuous(labels = percent) + labs(title = "Congestion level mix by time period", x = NULL, y = "Share of hours") + th
save(p3, "fig3_period_mix.png")

# 4 factor impact ---------------------------------------------------------------
fac <- c(road_work = "Road work", accident = "Accident", signal_fault = "Signal fault", event_nearby = "Nearby event",
         illegal_parking = "Illegal parking", rain_flag = "Rain", is_holiday = "Public holiday", is_weekend = "Weekend")
imp <- bind_rows(lapply(names(fac), function(nm) { lab <- fac[[nm]]
  df %>% group_by(v = .data[[nm]]) %>%
    summarise(pct = mean(is_congested) * 100, delay = mean(delay_min), n = n(), .groups = "drop") %>% mutate(factor = lab) }))
cat("\n=== % HOURS CONGESTED (High/Severe) WITH vs WITHOUT EACH FACTOR ===\n")
fx <- imp %>% mutate(state = ifelse(v == 1, "With", "Without")) %>% select(factor, state, pct, delay, n) %>%
  pivot_wider(names_from = state, values_from = c(pct, delay, n)) %>%
  mutate(uplift_pp = round(pct_With - pct_Without, 1)) %>% arrange(desc(uplift_pp)) %>% mutate(across(where(is.numeric), ~round(.x, 1)))
print(as.data.frame(fx))
write_csv(fx, "outputs/factor_impact_table.csv")
p4 <- imp %>% mutate(state = ifelse(v == 1, "Factor present", "Factor absent")) %>%
  ggplot(aes(reorder(factor, pct), pct, fill = state)) + geom_col(position = "dodge", width = 0.7) + coord_flip() +
  scale_fill_manual(values = c("Factor absent" = "#B9C0C6", "Factor present" = mar), name = NULL) +
  labs(title = "% of hours High/Severe congested: factor present vs absent", x = NULL, y = "% of hours congested") + th
save(p4, "fig4_factor_impact.png")

# 5 weather -----------------------------------------------------------------------
p5 <- ggplot(df, aes(reorder(weather, delay_min, median), delay_min, fill = weather)) + geom_boxplot(outlier.alpha = 0.25, show.legend = FALSE) +
  scale_fill_manual(values = c(Clear = "#D9E4EA", Cloudy = "#B9C0C6", `Light Rain` = "#E3B23C", `Heavy Rain` = mar)) +
  labs(title = "Delay distribution by weather", x = NULL, y = "Delay (min)") + th
save(p5, "fig5_weather_box.png")

# 6 speed vs volume ---------------------------------------------------------------
p6 <- ggplot(df, aes(vehicle_count, avg_speed_kmph, colour = congestion_level)) + geom_point(alpha = 0.55, size = 1.5) +
  scale_colour_manual(values = pal, name = "Level") + labs(title = "Speed drops sharply as volume approaches capacity",
  x = "Vehicles per hour", y = "Average speed (km/h)") + th
save(p6, "fig6_speed_volume.png")

# 7 correlation -----------------------------------------------------------------
cm <- cor(df %>% select(vehicle_count, avg_speed_kmph, queue_length_m, delay_min, heavy_vehicle_pct, pedestrian_count,
                        green_ratio, rainfall_mm, road_work, accident, signal_fault, illegal_parking))
cat("\n=== CORRELATION WITH delay_min ===\n"); print(round(sort(cm[, "delay_min"], decreasing = TRUE), 2))
png("figures/fig7_correlation.png", width = 1700, height = 1500, res = 200, bg = "white")
corrplot(cm, method = "color", type = "lower", tl.col = "black", tl.cex = 0.8, addCoef.col = "black", number.cex = 0.55,
         col = colorRampPalette(c("#2E6F95", "white", mar))(200), title = "Correlation matrix", mar = c(0, 0, 2, 0))
dev.off()

# 8 monthly -----------------------------------------------------------------------
mo <- df %>% group_by(month_name) %>% summarise(pct = mean(is_congested) * 100, delay = mean(delay_min))
cat("\n=== MONTHLY TREND ===\n"); print(as.data.frame(mo %>% mutate(across(where(is.numeric), ~round(.x, 1)))))
p8 <- ggplot(mo, aes(month_name, pct, group = 1)) + geom_col(fill = mar, width = 0.6) + geom_text(aes(label = round(pct, 1)), vjust = -0.4) +
  labs(title = "Share of hours High/Severe congested by month", x = NULL, y = "% of hours") + th
save(p8, "fig8_monthly.png")

# 9 top hours ---------------------------------------------------------------------
cat("\n=== TOP 5 WORST HOURS OF DAY (avg delay, min) ===\n")
print(as.data.frame(df %>% group_by(hour) %>% summarise(avg_delay = round(mean(delay_min), 1), avg_speed = round(mean(avg_speed_kmph), 1)) %>%
  arrange(desc(avg_delay)) %>% head(5)))
cat("\nFigures saved to figures/\n")
