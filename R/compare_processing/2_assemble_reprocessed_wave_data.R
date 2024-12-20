# December 19, 2024

library(dplyr)
library(here)
library(purrr)
library(tidyr)
library(waves)
library(zoo)


# read in data ------------------------------------------------------------

files <- list.files(here("data-compare"), pattern = "rds", full.names = TRUE)

dat_raw <- files %>%
  purrr::map_dfr(readRDS)

# no QC to check tests ----------------------------------------------------

dat1 <- dat_raw %>%
  wv_pivot_vars_longer()


saveRDS(dat1, here("data/2024-12-19_reprocessed_examples.rds"))

