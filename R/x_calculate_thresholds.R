# August 29, 2024

# This script calculates and exports gross range, climatology, spike, and
# rolling sd thresholds for:


# Thresholds are calculated from historical data
## Datasets sent to the Open Data Portal in December 2022.


# See thresholds_tracker.xlsx for decisions on grouping and stats used for
# each variable and threshold

library(data.table)
library(dplyr)
library(here)
library(lubridate)
library(qaqcmar)
library(readr)
library(sensorstrings)
library(tidyr)


dat <- readRDS(here("data/2024-08-28_wave_data_prelim_qc.rds")) %>%
  filter(
    !(variable == "average_height_largest_33_percent_m" & value > 15),
    !(variable == "period_largest_10_percent_s" & value > 60),
    !(variable == "period_largest_33_percent_s" & value > 60),
    !(variable == "period_maximum_s" & value > 60)
  )

# Grossrange sensor thresholds --------------------------------------------
# compiled from sensor manuals

# grossrange <- read_csv(
#   here("data/grossrange_thresholds.csv"),
#   show_col_types = FALSE) %>%
#   pivot_longer(
#     cols = c(sensor_min, sensor_max),
#     names_to = "threshold", values_to = "threshold_value"
#   ) %>%
#   mutate(county = NA, month = NA, qc_test = "grossrange")

grossrange_county_quartile <- dat %>%
  filter(
    variable %in% c(
      "significant_height_m",
      "average_height_largest_33_percent_m",
      "average_height_largest_10_percent_m",
      "maximum_height_m",
      "sea_water_speed_m_s")
  ) %>%
  group_by(county, variable) %>%
  summarise(user_max = round(quantile(value, probs = 0.997), digits = 2)) %>%
  ungroup() %>%
  mutate(
    qc_test = "grossrange",
    user_min = 0,
    gr_min = 0,
    gr_max = 3 * user_max
    ) %>%
  select(qc_test, variable, user_min, everything()) %>%
  pivot_longer(
    cols = contains("user"),
    values_to = "threshold_value", names_to = "threshold"
  ) %>%
  mutate(threshold_value = round(threshold_value, digits = 3))




grossrange_pooled_quartile <- dat %>%
  filter(
    variable %in% c(
      "peak_period_s",
      "period_largest_33_percent_s",
      "period_largest_10_percent_s",
      "period_maximum_s",
      "sensor_depth_below_surface_m")
  ) %>%
  group_by(variable) %>%
  summarise(user_max = round(quantile(value, probs = 0.997), digits = 2)) %>%
  ungroup() %>%
  mutate(
    qc_test = "grossrange",
    user_min = 0,
    gr_min = 0,
    gr_max = 3 * user_max
  ) %>%
  select(qc_test, variable, user_min, everything()) %>%
  pivot_longer(
    cols = contains("user"),
    values_to = "threshold_value", names_to = "threshold"
  ) %>%
  mutate(threshold_value = round(threshold_value, digits = 3))

grossrange_other <- data.frame(
  qc_test = "grossrange",
  variable = c("to_direction_degree", "sea_water_to_direction_degree"),
  user_min = c(0, 0),
  user_max = c(360, 360),
  gr_min = c(0, 0),
  gr_max = c(360, 360)
) %>%
  pivot_longer(
    cols = c(contains("user"), contains("gr")),
    values_to = "threshold_value", names_to = "threshold"
  ) %>%
  mutate(threshold_value = round(threshold_value, digits = 3))


grossrange <- bind_rows(
  grossrange_county_quartile, grossrange_pooled_quartile, grossrange_other
)



# Export ------------------------------------------------------------------

wv_thresholds <- bind_rows(
  grossrange
) %>%
  select(qc_test, variable, county, threshold, threshold_value)


#fwrite(thresholds, file = here("output/thresholds.csv"), na = "NA")

# export directly to waves
save(
  wv_thresholds,
  file = "C:/Users/Danielle Dempsey/Desktop/RProjects/Packages/waves/data/wv_thresholds.rda")

