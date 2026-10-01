
set.seed(2026)
library(dplyr)

ts <- seq(as.POSIXct("2026-04-01 00:00:00", tz = "Asia/Kolkata"),
          as.POSIXct("2026-09-30 23:00:00", tz = "Asia/Kolkata"), by = "hour")
n <- length(ts)
hour <- as.integer(format(ts, "%H"))
wday <- as.integer(format(ts, "%u"))          # 1 = Mon
month <- as.integer(format(ts, "%m"))
day  <- as.Date(ts)
base <- 350 +
  1150 * exp(-((hour - 9.5)^2) / 5) +
  1350 * exp(-((hour - 18.5)^2) / 6) +
  450  * exp(-((hour - 13)^2) / 8)
base[hour < 5] <- base[hour < 5] * 0.35
weekend <- wday >= 6
base <- base * ifelse(weekend, 0.72, 1)

holiday_days <- as.Date(c("2026-04-14","2026-05-01","2026-08-15","2026-09-14","2026-09-25"))
is_holiday <- day %in% holiday_days
base <- base * ifelse(is_holiday, 0.65, 1)
monsoon <- month %in% 6:9
ud <- unique(day)
rain_p  <- ifelse(as.integer(format(ud, "%m")) %in% 6:9, 0.30, 0.03)
rain_day <- setNames(runif(length(ud)) < rain_p, ud)
rain_hr  <- rain_day[as.character(day)] & runif(n) < 0.55
rain_mm  <- ifelse(rain_hr, round(rexp(n, 1/6), 1), 0)
weather  <- ifelse(rain_mm > 12, "Heavy Rain", ifelse(rain_mm > 0, "Light Rain",
             ifelse(runif(n) < 0.18, "Cloudy", "Clear")))

road_work_days <- sample(unique(day), 14)
road_work <- as.integer(day %in% road_work_days & hour >= 8 & hour <= 19)
event_nearby <- as.integer(runif(n) < ifelse(weekend & hour >= 17 & hour <= 21, 0.08, 0.008))
accident <- as.integer(runif(n) < (0.006 + 0.010 * (rain_mm > 0) + 0.004 * (hour %in% 17:20)))
illegal_parking <- as.integer(runif(n) < ifelse(hour %in% 10:20, 0.35, 0.08))
signal_fault <- as.integer(runif(n) < 0.007)

vehicle_count <- round(pmax(40, base * rnorm(n, 1, 0.09) * (1 + 0.10 * event_nearby)))
heavy_pct  <- round(pmin(38, pmax(2, rnorm(n, ifelse(hour %in% 22:23 | hour < 6, 24, 11), 3.5))), 1)
ped_count  <- round(pmax(0, rnorm(n, 180 + 320 * exp(-((hour - 18)^2) / 9) +
                                     220 * exp(-((hour - 9)^2) / 6), 45)))
signal_cycle <- ifelse(hour %in% 7:21, 150, 110)
green_time   <- round(signal_cycle * runif(n, 0.36, 0.46))

capacity <- 1750 * (1 - 0.28 * road_work) * (1 - 0.30 * accident) *
  (1 - 0.10 * illegal_parking) * (1 - 0.16 * (rain_mm > 0) - 0.10 * (rain_mm > 12)) *
  (1 - 0.35 * signal_fault) * (1 - 0.004 * (heavy_pct - 10)) * (green_time / (0.41 * signal_cycle))
vc <- vehicle_count / capacity
avg_speed <- pmax(3, 38 * (1 - pmin(vc, 1.25)^1.6 * 0.82) + rnorm(n, 0, 1.8) - 0.03 * ped_count / 10)
queue_len <- pmax(0, round(35 + 520 * pmax(vc - 0.55, 0)^1.4 * 3 + rnorm(n, 0, 18)))
delay_min <- round(pmax(0.2, 0.6 + 14 * pmax(vc - 0.6, 0)^1.3 * 3 + rnorm(n, 0, 0.7)), 1)
congestion_level <- cut(vc, c(-Inf, 0.55, 0.85, 1.05, Inf),
                        labels = c("Low", "Moderate", "High", "Severe"))

raw <- data.frame(
  record_id = seq_len(n),
  timestamp = format(ts, "%Y-%m-%d %H:%M:%S"),
  vehicle_count, avg_speed_kmph = round(avg_speed, 1), queue_length_m = queue_len,
  delay_min, signal_cycle_s = signal_cycle, green_time_s = green_time,
  heavy_vehicle_pct = heavy_pct, pedestrian_count = ped_count, rainfall_mm = rain_mm,
  weather, road_work, accident, event_nearby, illegal_parking, signal_fault,
  is_holiday = as.integer(is_holiday), congestion_level = as.character(congestion_level),
  stringsAsFactors = FALSE
)
pick <- function(k) sample(n, k)
raw$avg_speed_kmph[pick(70)]   <- NA
raw$vehicle_count[pick(55)]    <- NA
raw$queue_length_m[pick(60)]   <- NA
raw$heavy_vehicle_pct[pick(45)] <- NA
raw$weather[pick(40)]          <- NA
raw$avg_speed_kmph[pick(12)]   <- sample(c(-5, 0, 185, 240, 999), 12, TRUE)     # sensor errors
raw$vehicle_count[pick(8)]     <- sample(c(-20, 9800, 15000), 8, TRUE)          # impossible counts
raw$queue_length_m[pick(6)]    <- sample(c(-40, 5200, 8000), 6, TRUE)
# inconsistent weather labels (case / spaces)
idx <- which(!is.na(raw$weather) & runif(n) < 0.12)
raw$weather[idx] <- sapply(raw$weather[idx], function(w)
  sample(c(toupper(w), tolower(w), paste0(" ", w, " ")), 1))
raw$congestion_level[pick(30)] <- NA
raw$vehicle_count <- as.character(raw$vehicle_count)
ti <- pick(15)
raw$vehicle_count[ti] <- ifelse(is.na(raw$vehicle_count[ti]), NA, paste0(raw$vehicle_count[ti], " veh"))
dups <- raw[sample(n, 60), ]                  # duplicated rows
raw <- rbind(raw, dups)
raw <- raw[sample(nrow(raw)), ]               # shuffle
raw <- raw[order(raw$record_id), ]

write.csv(raw, "data/raw/junction_traffic_raw.csv", row.names = FALSE, na = "")
cat("Raw dataset written:", nrow(raw), "rows x", ncol(raw), "columns\n")

