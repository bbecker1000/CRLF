library(tidyverse)
library(purrr)
library(readxl)

setwd(here::here("code"))

muwo_rain <- read_excel(here::here("data", "muwo_rain.xlsx"))

marin_rain_folder <- here::here("data", "MMWD_RainfallRecords2")
rain_files <- list.files(path = marin_rain_folder, pattern = "\\.xlsx$", full.names = TRUE)
rain_df_list <- lapply(rain_files, read_excel) # creates list of data frames (one data frame = one spreadsheet)
rain_df <- bind_rows(rain_df_list, .id = "id")
