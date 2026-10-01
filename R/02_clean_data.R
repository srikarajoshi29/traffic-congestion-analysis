
suppressPackageStartupMessages({library(dplyr); library(readr); library(lubridate); library(rpart); library(openxlsx)})

raw <- read_csv("data/raw/junction_traffic_raw.csv", col_types = cols(.default = "c"), na = c("", "NA"))
log <- data.frame(step = character(), issue = character(), action = character(), rows_affected = integer())
add_log <- function(step, issue, action, k) log[nrow(log) + 1, ] <<- list(step, issue, action, as.integer(k))

cat("== RAW DATA ==\nRows:", nrow(raw), " Columns:", ncol(raw), "\n")
cat("Missing values per column (raw):\n"); print(colSums(is.na(raw)))

n0 <- nrow(raw); raw <- distinct(raw)
add_log("1", "Exact duplicate records", "Removed duplicates", n0 - nrow(raw))

raw$vehicle_count <- gsub("[^0-9.-]", "", raw$vehicle_count)      # "812 veh" -> "812"
n_txt <- sum(grepl("veh", read_csv("data/raw/junction_traffic_raw.csv", col_types = cols(.default = "c"))$vehicle_count, fixed = TRUE))
add_log("2", "Text units inside numeric column (vehicle_count)", "Stripped non-numeric characters, converted to numeric", n_txt)
num_cols <- c("vehicle_count","avg_speed_kmph","queue_length_m","delay_min","signal_cycle_s","green_time_s",
              "heavy_vehicle_pct","pedestrian_count","rainfall_mm","road_work","accident","event_nearby",
              "illegal_parking","signal_fault","is_holiday")
df <- raw %>% mutate(across(all_of(num_cols), as.numeric),
                     timestamp = ymd_hms(timestamp, tz = "Asia/Kolkata"),
                     record_id = as.integer(record_id))

before <- df$weather
df$weather <- tools::toTitleCase(tolower(trimws(df$weather)))
add_log("3", "Inconsistent weather labels (case / stray spaces)", "Trimmed and standardised to Title Case",
        sum(before != df$weather, na.rm = TRUE))

rule <- function(x, bad) { k <- sum(bad, na.rm = TRUE); x[which(bad)] <- NA; list(x = x, k = k) }
r <- rule(df$avg_speed_kmph, df$avg_speed_kmph <= 0 | df$avg_speed_kmph > 120); df$avg_speed_kmph <- r$x
add_log("4a", "Impossible speed (<=0 or >120 km/h, sensor error)", "Set to NA, imputed in step 5", r$k)
r <- rule(df$vehicle_count, df$vehicle_count < 0 | df$vehicle_count > 6000); df$vehicle_count <- r$x
add_log("4b", "Impossible hourly vehicle count (<0 or >6000)", "Set to NA, imputed in step 5", r$k)
r <- rule(df$queue_length_m, df$queue_length_m < 0 | df$queue_length_m > 2000); df$queue_length_m <- r$x
add_log("4c", "Impossible queue length (<0 or >2000 m)", "Set to NA, imputed in step 5", r$k)

df <- df %>% mutate(hour = hour(timestamp), is_weekend = as.integer(wday(timestamp, week_start = 1) >= 6))
med_impute <- function(d, col) {
  m <- d %>% group_by(hour, is_weekend) %>% mutate(.m = median(.data[[col]], na.rm = TRUE)) %>% pull(.m)
  k <- sum(is.na(d[[col]])); d[[col]][is.na(d[[col]])] <- m[is.na(d[[col]])]
  add_log("5", paste("Missing", col), "Median of same hour-of-day and weekday/weekend group", k); d
}
for (cl in c("avg_speed_kmph","vehicle_count","queue_length_m","heavy_vehicle_pct")) df <- med_impute(df, cl)

k_w <- sum(is.na(df$weather))
df$weather <- ifelse(is.na(df$weather) & df$rainfall_mm > 12, "Heavy Rain",
              ifelse(is.na(df$weather) & df$rainfall_mm > 0, "Light Rain", df$weather))
mode_w <- names(which.max(table(df$weather))); df$weather[is.na(df$weather)] <- mode_w
add_log("5", "Missing weather", "Inferred from rainfall_mm; otherwise most frequent category", k_w)

lv <- c("Low","Moderate","High","Severe")
df$congestion_level <- factor(df$congestion_level, levels = lv)
k_c <- sum(is.na(df$congestion_level))
tr <- rpart(congestion_level ~ delay_min + avg_speed_kmph + queue_length_m + vehicle_count,
            data = df[!is.na(df$congestion_level), ], method = "class")
df$congestion_level[is.na(df$congestion_level)] <- predict(tr, df[is.na(df$congestion_level), ], type = "class")
add_log("5", "Missing congestion_level (target)", "Predicted with decision tree on delay, speed, queue, volume", k_c)

# 6. Feature engineering ------------------------------------------------------
df <- df %>% mutate(
  date = as.Date(timestamp),
  day_name = factor(wday(timestamp, label = TRUE, abbr = FALSE, week_start = 1, locale = "C"),
                    levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")),
  month_name = factor(month(timestamp, label = TRUE, abbr = TRUE, locale = "C"), ordered = FALSE),
  period = factor(case_when(hour %in% 8:11 ~ "Morning Peak", hour %in% 17:20 ~ "Evening Peak",
                            hour %in% 12:16 ~ "Midday", TRUE ~ "Off-Peak/Night"),
                  levels = c("Morning Peak","Midday","Evening Peak","Off-Peak/Night")),
  green_ratio = round(green_time_s / signal_cycle_s, 3),
  is_congested = as.integer(congestion_level %in% c("High","Severe")),
  rain_flag = as.integer(rainfall_mm > 0),
  incident_any = as.integer(accident == 1 | signal_fault == 1 | road_work == 1)
)
add_log("6", "Feature engineering", "Added hour, day_name, month_name, period, green_ratio, is_congested, rain_flag, incident_any", nrow(df))

df <- df %>% select(record_id, timestamp, date, hour, day_name, month_name, period, is_weekend, is_holiday,
                    vehicle_count, avg_speed_kmph, queue_length_m, delay_min, heavy_vehicle_pct, pedestrian_count,
                    signal_cycle_s, green_time_s, green_ratio, rainfall_mm, weather, rain_flag,
                    road_work, accident, signal_fault, event_nearby, illegal_parking, incident_any,
                    congestion_level, is_congested)

stopifnot(sum(is.na(df)) == 0)
cat("\n== CLEAN DATA ==\nRows:", nrow(df), " Columns:", ncol(df), " Missing cells:", sum(is.na(df)), "\n")
print(log)

out <- df %>% mutate(timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S"), date = as.character(date))
write_csv(out, "data/clean/junction_traffic_clean.csv")
write_csv(log, "data/clean/cleaning_log.csv")

dict <- data.frame(
  column = names(df),
  description = c("Unique observation id","Date-time of hourly observation (IST)","Calendar date","Hour of day (0-23)",
    "Day of week","Month","Time-of-day period","1 = Saturday/Sunday","1 = public holiday",
    "Vehicles crossing the junction in the hour","Average approach speed (km/h)","Maximum queue length (m)",
    "Average delay per vehicle (minutes)","Share of heavy vehicles (%)","Pedestrians crossing in the hour",
    "Signal cycle length (s)","Green time for main approach (s)","green_time_s / signal_cycle_s","Rainfall in the hour (mm)",
    "Weather category","1 = rainfall > 0","1 = road work / lane closure","1 = accident in the hour","1 = signal malfunction",
    "1 = event near junction","1 = illegal / roadside parking observed","1 = accident, signal fault or road work",
    "Congestion class (Low/Moderate/High/Severe)","1 = High or Severe (target variable)"),
  type = sapply(df, function(x) class(x)[1]), stringsAsFactors = FALSE)

summ <- df %>% select(where(is.numeric), -record_id) %>% summarise(across(everything(),
  list(Mean = ~round(mean(.x),2), Median = ~round(median(.x),2), SD = ~round(sd(.x),2), Min = ~min(.x), Max = ~max(.x)))) %>%
  tidyr::pivot_longer(everything(), names_to = c("Variable","Stat"), names_pattern = "(.*)_(Mean|Median|SD|Min|Max)$") %>%
  tidyr::pivot_wider(names_from = Stat, values_from = value)

wb <- createWorkbook(); hs <- createStyle(fontName="Arial", fontSize=10, textDecoration="bold", fgFill="#7A1F3D",
  fontColour="#FFFFFF", halign="center", border="Bottom"); bs <- createStyle(fontName="Arial", fontSize=10)
add_sheet <- function(name, d, widths = "auto") { addWorksheet(wb, name); writeData(wb, name, d, headerStyle = hs)
  addStyle(wb, name, bs, rows = 2:(nrow(d)+1), cols = 1:ncol(d), gridExpand = TRUE, stack = TRUE)
  setColWidths(wb, name, cols = 1:ncol(d), widths = widths); freezePane(wb, name, firstRow = TRUE)
  addFilter(wb, name, row = 1, cols = 1:ncol(d)) }
add_sheet("Clean_Data", out, widths = 16)
add_sheet("Data_Dictionary", dict, widths = c(22, 55, 12))
add_sheet("Cleaning_Log", log, widths = c(8, 55, 65, 14))
add_sheet("Summary_Stats", summ, widths = c(22, 12, 12, 12, 12, 12))
saveWorkbook(wb, "data/clean/junction_traffic_clean.xlsx", overwrite = TRUE)
cat("\nSaved: CSV + XLSX in data/clean/\n")
