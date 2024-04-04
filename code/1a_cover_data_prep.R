library(tidyverse)
library(dplyr)
library(here)
library(lubridate)
library(readxl)
library(here)

setwd(here::here("code"))

# read in cover data file, data filtering (not done yet, need to set data types)
cover_data <- read_xlsx(here::here("data", "canopy_cover.xlsx"), sheet = "Data") %>% 
  mutate(GoogleEarthPhotoDate = as.Date(GoogleEarthPhotoDate))

# making columns for whether it's the first or last date (for later filtering)
cover_data <- cover_data %>%
  group_by(LocationID) %>% 
  mutate(first_date = if_else(GoogleEarthPhotoDate == min(GoogleEarthPhotoDate), TRUE, FALSE),
         last_date = if_else(GoogleEarthPhotoDate == max(GoogleEarthPhotoDate), TRUE, FALSE)) %>% 
  ungroup()

# create a filtered cover data table with only first and last dates (to try to see a linear model for all sites)
cover_data_first_and_last <- cover_data %>% 
  filter(first_date | last_date)

# visualization purpose: open water and veg. proportion of site LS01 over time.
cover_data_LS01 <- cover_data |>
  filter(LocationID == "LS01")
cover_data_LS01$GoogleEarthPhotoDate <- as.Date(cover_data_LS01$GoogleEarthPhotoDate)
cover_data_LS01$year_numeric <- as.numeric(format(cover_data_LS01$GoogleEarthPhotoDate, "%Y"))
cover_data_LS01 <- cover_data_LS01 |>
  select(year_numeric,OpenWater_percent,SubmergentVegetation_percent,EmergentVegetation_percent)
cover_data_LS01_long <- gather(cover_data_LS01, key = "component", value = "proportion", -year_numeric)
cover_data_LS01_long
ggplot(cover_data_LS01_long,aes(x=year_numeric,y=proportion,fill=component)) + geom_area() + 
  scale_fill_manual(values = c("lightgreen", "steelblue", "darkgreen"))

# visualization purpose: open water and veg. proportion of site RS07 over time.
cover_data_RC07 <- cover_data |>
  filter(LocationID == "RC07")
cover_data_RC07$GoogleEarthPhotoDate <- as.Date(cover_data_RC07$GoogleEarthPhotoDate)
cover_data_RC07$year_numeric <- as.numeric(format(cover_data_RC07$GoogleEarthPhotoDate, "%Y"))
cover_data_RC07 <- cover_data_RC07 |>
  select(year_numeric,OpenWater_percent,SubmergentVegetation_percent,EmergentVegetation_percent)
cover_data_RC07_long <- gather(cover_data_RC07, key = "component", value = "proportion", -year_numeric)
cover_data_RC07_long
ggplot(cover_data_RC07_long,aes(x=year_numeric,y=proportion,fill=component)) + geom_area() + 
  scale_fill_manual(values = c("lightgreen", "steelblue", "darkgreen"))



# TODO: create a filtered cover data table with intermediates for only the sites that Ruby did for every year
# to see if it's actually a linear relationship
# sites are LS01, RC07, RC10, RL02, TV02