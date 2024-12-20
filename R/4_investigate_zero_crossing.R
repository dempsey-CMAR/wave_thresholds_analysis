library(dplyr)
library(DT)
library(ggplot2)
library(googlesheets4)
library(here)
library(htmltools)
library(knitr)
library(leaflet)
library(lubridate)
library(plotly)
library(purrr)
library(stringr)
library(summaryplots)
library(tidyr)
library(waves)

theme_set(theme_light())

zero_crossing <- c(
  "average_height_largest_33_percent_m",
  "average_height_largest_10_percent_m",
  "period_largest_33_percent_s",
  "period_largest_10_percent_s",
  "maximum_height_m",
  "period_maximum_s"
)


# all data - no qc --------------------------------------------------------

dat_raw <- readRDS(here("data/2024-08-28_wave_data_no_qc.rds"))

dat1 <- dat_raw %>%
  filter(variable %in% zero_crossing) %>%
  pivot_wider(names_from = "variable", values_from = "value") %>%
  wv_test_grossrange(county = "Halifax") %>%
  mutate(
    timestamp_ast = timestamp_utc - hours(3),
    hour_ast = hour(timestamp_ast)
    ) %>%
  wv_pivot_flags_longer(qc_tests = "grossrange")

dat_fail1 <- dat1 %>%
  filter(grossrange_flag_value == 4) %>%
  select(-grossrange_flag_value) %>%
  wv_convert_vars_to_ordered_factor()

plot_histogram(dat_fail1, hist_col = hour_ast, binwidth = 1) +
  facet_wrap(~variable, ncol = 2) +
  scale_x_continuous("Hour") +
  labs(
    title = "Data from 58 ADCP deployments",
    caption = "Zero crossing parameter observations with values of 0 or -0.1 recorded each hour"
  )


dat_pass1 <- dat1 %>%
  filter(grossrange_flag_value != 4)

plot_histogram(dat_pass1, hist_col = hour_ast, binwidth = 1) +
  facet_wrap(~variable, ncol = 2) +
  scale_x_continuous("Hour")


# all data - remove worst deployments -------------------------------------

dat2 <- dat_raw %>%
  filter(
    variable %in% zero_crossing,
    !(variable %in% zero_crossing & deployment_id == "IV001"),
    !(variable %in% zero_crossing & deployment_id == "QN010"),
    deployment_id != "PC001"
  ) %>%
  pivot_wider(names_from = "variable", values_from = "value") %>%
  wv_test_grossrange(county = "Halifax") %>%
  mutate(
    timestamp_ast = timestamp_utc - hours(3),
    hour_ast = hour(timestamp_ast)
    ) %>%
  wv_pivot_flags_longer(qc_tests = "grossrange")

dat_fail2 <- dat2 %>%
  filter(grossrange_flag_value == 4) %>%
  select(-grossrange_flag_value)

plot_histogram(dat_fail2, hist_col = hour_ast, binwidth = 1) +
  facet_wrap(~variable, ncol = 2) +
  scale_x_continuous("Hour")

dat_pass2 <- dat2 %>%
  filter(grossrange_flag_value != 4)

plot_histogram(dat_pass2, hist_col = hour_ast, binwidth = 1) +
  facet_wrap(~variable, ncol = 2) +
  scale_x_continuous("Hour")



# Saddle Island Example ---------------------------------------------------

dat3 <- dat_raw %>%
  filter(variable %in% zero_crossing, deployment_id == "LN007") %>%
  pivot_wider(names_from = "variable", values_from = "value") %>%
  wv_test_grossrange(county = "Lunenburg") %>%
  mutate(
    timestamp_ast = timestamp_utc - hours(3),
    hour_ast = hour(timestamp_ast)
    ) %>%
  wv_pivot_flags_longer(qc_tests = "grossrange")

dat_fail3 <- dat3 %>%
  filter(grossrange_flag_value == 4, value %in% c(-0.1, 0)) %>%
  select(-grossrange_flag_value)  %>%
  wv_convert_vars_to_ordered_factor()

plot_histogram(dat_fail3, hist_col = hour_ast, binwidth = 1) +
  facet_wrap(~variable, ncol = 2) +
  scale_x_continuous("Hour") +
  labs(
    title = "Data from deployment at Saddle Island 2023-05-10",
    caption = "Zero crossing parameter observations with values of 0 or -0.1 recorded each hour"
  )

dat_pass3 <- dat3 %>%
  filter(grossrange_flag_value != 4)

plot_histogram(dat_pass2, hist_col = hour_ast, binwidth = 1) +
  facet_wrap(~variable, ncol = 2) +
  scale_x_continuous("Hour")





