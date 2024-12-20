# November 7, 2024

# This script calculates and exports grossrange, rolling sd thresholds for:

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
library(zoo)

zero_crossing <- c(
  "average_height_largest_33_percent_m",
  "average_height_largest_10_percent_m",
  "period_largest_33_percent_s",
  "period_largest_10_percent_s",
  "maximum_height_m",
  "period_maximum_s"
)

dat <- readRDS(here("data/2024-08-28_wave_data_prelim_qc.rds")) %>%
  filter(
    !(variable == "average_height_largest_33_percent_m" & value > 15),
    !(variable == "period_largest_10_percent_s" & value > 60),
    !(variable == "period_largest_33_percent_s" & value > 60),
    !(variable == "period_maximum_s" & value > 60)
  ) %>%
  filter(
    !(variable == "average_height_largest_33_percent_m" & value > 15),
    !(variable == "period_largest_10_percent_s" & value > 60),
    !(variable == "period_largest_33_percent_s" & value > 60),
    !(variable == "period_maximum_s" & value > 60),
    !(variable %in% zero_crossing & deployment_id == "IV001"),
    !(variable %in% zero_crossing & deployment_id == "QN010"),
    deployment_id != "PC001"
  )


max_interval_hours <- 4
period_hours <- 24
align_window <- "center"

dat_roll_sd <- dat %>%
  group_by(county, deployment_id, station, variable) %>%
  dplyr::arrange(timestamp_utc, .by_group = TRUE) %>%
  mutate(
    # sample interval
    int_sample = difftime(timestamp_utc, lag(timestamp_utc), units = "mins"),
    int_sample = round(as.numeric(int_sample)),

    # number of samples in  period_hours
    # 60 mins / hour * 1 sample / int_sample mins * 24 hours / period
    n_sample = round((60 / int_sample) * period_hours),
    # n_sample = if_else(is.na(n_sample), 1, n_sample), # first obs
    n_sample_effective = case_when(
      # first observation of each group is NA, which will give error in rollapply
      is.na(n_sample) ~ 0,
      # if the sample interval is greater than acceptable limit, set n_sample to 0
      # so that roll_sd will be NA
      int_sample > max_interval_hours * 60 ~ 0,
      TRUE ~ n_sample
    ),
    # rolling sd
    sd_roll = rollapply(
      value,
      width = n_sample_effective,
      align = align_window, FUN = sd, fill = NA
    ),
    sd_roll = round(sd_roll, digits = 2)
  ) %>%
  ungroup() %>%
  filter(!is.na(sd_roll))

dat_spike <- dat %>%
  group_by(county, deployment_id, station, variable) %>%
  dplyr::arrange(timestamp_utc, .by_group = TRUE) %>%
  mutate(
    lag_value = lag(value),
    lead_value = lead(value),
    spike_ref = (lag_value + lead_value) / 2,
    spike_value = abs(value - spike_ref)
  ) %>%
  ungroup() %>%
  filter(!is.na(spike_value))

# Grossrange sensor thresholds --------------------------------------------

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
  summarise(
    user_max = round(
      quantile(value, probs = 0.997, na.rm = TRUE), digits = 2)
  ) %>%
  ungroup() %>%
  mutate(
    qc_test = "grossrange",
    user_min = 0,
    gr_min = 0,
    gr_max = 3 * user_max
    ) %>%
  select(qc_test, variable, user_min, everything()) %>%
  pivot_longer(
    cols = c(contains("gr"), contains("user")),
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
    cols = c(contains("gr"), contains("user")),
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
  grossrange_county_quartile,
  grossrange_pooled_quartile, grossrange_other
)

attr(grossrange$threshold_value, "names") <- NULL

# Rolling SD --------------------------------------------------------------

roll_sd_county_quartile <- dat_roll_sd %>%
  filter(
    variable %in% c(
      "significant_height_m",
      "average_height_largest_33_percent_m",
      "average_height_largest_10_percent_m",
      "maximum_height_m",
      "period_largest_33_percent_s",
      "period_largest_10_percent_s",
      "period_maximum_s")
  ) %>%
  group_by(county, variable) %>%
  summarise(
    q = quantile(sd_roll, probs = 0.997, na.rm = TRUE)
  ) %>%
  rename(rolling_sd_max = q) %>%
  ungroup() %>%
  mutate(qc_test = "rolling_sd") %>%
  pivot_longer(
    cols = "rolling_sd_max",
    values_to = "threshold_value", names_to = "threshold"
  ) %>%
  mutate(threshold_value = round(threshold_value, digits = 3))

attr(roll_sd_county_quartile$threshold_value, "names") <- NULL

# mean and sd
roll_sd_county_mean <- dat_roll_sd %>%
  filter(
    variable %in% c(
      "peak_period_s",
      "to_direction_degree",
      "sensor_depth_below_surface_m",
      "sea_water_speed_m_s",
      "sea_water_to_direction_degree"
    )
  ) %>%
  group_by(county, variable) %>%
  summarise(
    mean_sd_roll = mean(sd_roll, na.rm = TRUE),
    sd_sd_roll = sd(sd_roll, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(rolling_sd_max = mean_sd_roll + 3 * sd_sd_roll) %>%
  select(-c(mean_sd_roll, sd_sd_roll)) %>%
  mutate(qc_test = "rolling_sd") %>%
  pivot_longer(
    cols = "rolling_sd_max",
    values_to = "threshold_value", names_to = "threshold"
  ) %>%
  mutate(threshold_value = round(threshold_value, digits = 3))

roll_sd <- bind_rows(roll_sd_county_quartile, roll_sd_county_mean)


# Spike -------------------------------------------------------------------
spike <- dat_spike %>%
  group_by(county, variable) %>%
  summarise(
    spike_low = quantile(spike_value, probs = 0.997, na.rm = TRUE)
  ) %>%
  mutate(spike_high = spike_low * 3) %>%
  ungroup() %>%
  mutate(qc_test = "spike") %>%
  pivot_longer(
    cols = contains("spike"),
    values_to = "threshold_value", names_to = "threshold"
  ) %>%
  mutate(threshold_value = round(threshold_value, digits = 3))

attr(spike$threshold_value, "names") <- NULL


# Export ------------------------------------------------------------------

wv_thresholds <- bind_rows(
  grossrange, roll_sd, spike
) %>%
  select(qc_test, variable, county, threshold, threshold_value)

# dummy thresholds for Pictou; only deployment will be marked
# as Fail for the whole depl
pic <- wv_thresholds %>%
  filter(county == "Halifax") %>%
  mutate(county = "Pictou")

wv_thresholds <- wv_thresholds %>%
  bind_rows(pic) %>%
  arrange(qc_test, county, variable)

fwrite(wv_thresholds, file = here("output/wv_thresholds.csv"), na = "NA")

# export directly to waves
save(
  wv_thresholds,
  file = "C:/Users/Danielle Dempsey/Desktop/RProjects/Packages/waves/data/wv_thresholds.rda"
)

