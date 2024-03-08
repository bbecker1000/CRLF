library(tidyverse)
library(purrr)
library(readxl)

setwd(here::here("code"))

# reading in muir woods data
muwo_rain <- read_excel(here::here("data", "muwo_rain.xlsx"))

#reading in corte madera data
marin_rain_folder <- here::here("data", "MMWD_RainfallRecords2")
rain_files <- list.files(path = marin_rain_folder, pattern = "\\.xlsx$", full.names = TRUE)
sheet_name = "CM"

## rain_data_list = list of tables, each one = 1 year
rain_data_list <- map(rain_files, ~read_excel(.x, sheet = sheet_name) %>%
                        set_names(c("dayOfMonth", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "dayOfMonthAgain"))) #creates list of data frames, one for each year

# rain_data_list <- map(rain_data_list, ~mutate(.x, across(where(is.numeric), 
#                                                          ~ as.Date(.x, origin = "1899-12-30")))) # DOES NOT WORK YET!! date formatting is weird :(

pivot <- rain_data_list %>% 
  lapply(t) %>% 
  purrr::pluck(1,2, 38,39)
  select(c(-1,-2,-38,-39))
view(pivot[[1]])

## combine list of data frames into one single dataframe
rain_data <- rain_data_list %>% bind_rows(.id = "File")

#extract monthly rain data from rain_data