library(tidyverse)
library(purrr)
library(readxl)

setwd(here::here("code"))

muwo_rain <- read_excel(here::here("data", "muwo_rain.xlsx"))

marin_rain_folder <- here::here("data", "MMWD_RainfallRecords2")
rain_files <- list.files(path = marin_rain_folder, pattern = "\\.xlsx$", full.names = TRUE)

combined_data <- rain_files %>%
  map_dfr(~ read_excel(.x)) # trying to combine into one sheet, does not work yet
