# August 28, 2024

# Raw data has preliminary QC applied:

# Wave period variables with values <= 0 are flagged as "Fail". Based on the
# data files, it is likely that values of 0 and -0.1 are used as \code{NULL}
# values. All other values are flagged as "Pass".
#
# For the zero-crossing parameters, when wave period is <= 0, the corresponding
# wave height is almost always 0 m. The converse is also true (when wave height
# is 0, the period is <= 0). This suggests that 0 is used as a NULL value for
# wave height variables in the WavesMon software.
#
# For the variables calculated from frequency (significant_height_m,
# peak_period_s), wave height of 0 m more often corresponds to a non-zero
# frequency.
#
# Wave height parameters are flagged as "Fail" if the wave height is 0 m *and*
# the period is <= 0 s. All other variables are flagged as "Pass".

# Direction observations are flagged as "Pass" if they are >= 0 and <= 360, and
# are flagged as "Fail" otherwise.

library(dplyr)
library(here)
library(purrr)
library(tidyr)
library(waves)


# read in data ------------------------------------------------------------

files <- list.files(here("data-raw"), pattern = "rds", full.names = TRUE)

dat_raw <- files %>%
  purrr::map_dfr(readRDS)

# no QC to check tests ----------------------------------------------------

dat1 <- dat_raw %>%
  wv_assign_short_variable_names() %>%
  wv_pivot_vars_longer(first_pivot_col = 6, last_pivot_col = 17) %>%
  select(-contains("flag"))

saveRDS(dat1, here("data/2024-08-28_wave_data_no_qc.rds"))


# re-format & filter ------------------------------------------------------

dat_qc <- dat_raw %>%
  wv_assign_short_variable_names() %>%
  wv_pivot_vars_longer(first_pivot_col = 6, last_pivot_col = 17) %>%
  # add placeholder flag values so vars aren't dropped when pivoted
  mutate(
    grossrange_flag_sensor_depth_below_surface_m = ordered(1, levels = c(1:4)),
    grossrange_flag_sea_water_speed_m_s = ordered(1, levels = c(1:4)),
    grossrange_flag_sea_water_to_direction_degree = ordered(1, levels = c(1:4))
  ) %>%
  wv_pivot_flags_longer() %>%
  filter(
    grossrange_flag_value == 1,
    depth_trim_flag == 1
  ) %>%
  select(-c(grossrange_flag_value, depth_trim_flag))

saveRDS(dat_qc, here("data/2024-08-28_wave_data_prelim_qc.rds"))


# rolling sd --------------------------------------------------------------




