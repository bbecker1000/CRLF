library(tidyverse)
library(purrr)
library(readxl)
library(lubridate)

setwd(here::here("code"))

# reading in muir woods data
muwo_rain <- read_excel(here::here("data", "muwo_rain.xlsx"))

#reading in corte madera data
marin_rain_folder <- here::here("data", "MMWD_RainfallRecords2")
rain_files <- list.files(path = marin_rain_folder, pattern = "\\.xlsx$", full.names = TRUE)
sheet_name = "CM"

## rain_data_list = list of tables, each one = 1 year

rain_data_list <- rain_files %>% map( ~ {
  dates_col <- read_xlsx(.x, sheet = sheet_name, range = cell_rows(3:4), col_types = "date") %>% 
    set_names(c("Jul", "Aug", "Sept", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun")) %>% 
    t()
  totals_col <- read_xlsx(.x, sheet = sheet_name, range = cell_rows(36:37)) %>% 
    set_names(c("dayOfMonth", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "dayOfMonthAgain")) %>%
    select(-dayOfMonth, -dayOfMonthAgain) %>% 
    t()
  cbind(dates_col, totals_col)
  
})


## combine list of data frames into one single dataframe
# rain_data <- rain_data_list %>% bind_rows(.id = "File")

#extract monthly rain data from rain_data