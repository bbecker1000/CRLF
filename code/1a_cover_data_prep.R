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

# visualization purpose: open water and veg. proportion of the 5 site with continuous data over time.
## LS01 
cover_data_LS01 <- cover_data |>
  filter(LocationID == "LS01")
cover_data_LS01$GoogleEarthPhotoDate <- as.Date(cover_data_LS01$GoogleEarthPhotoDate)
cover_data_LS01$year_numeric <- as.numeric(format(cover_data_LS01$GoogleEarthPhotoDate, "%Y"))
cover_data_LS01 <- cover_data_LS01 |>
  select(year_numeric,OpenWater_percent,SubmergentVegetation_percent,EmergentVegetation_percent)
cover_data_LS01_long <- gather(cover_data_LS01, key = "component", value = "proportion", -year_numeric)
ggplot(cover_data_LS01_long,aes(x=year_numeric,y=proportion,fill=component)) + geom_area() + 
  scale_fill_manual(values = c("lightgreen", "steelblue", "darkgreen"))

## RS07
cover_data_RC07 <- cover_data |>
  filter(LocationID == "RC07")
cover_data_RC07$GoogleEarthPhotoDate <- as.Date(cover_data_RC07$GoogleEarthPhotoDate)
cover_data_RC07$year_numeric <- as.numeric(format(cover_data_RC07$GoogleEarthPhotoDate, "%Y"))
cover_data_RC07 <- cover_data_RC07 |>
  select(year_numeric,OpenWater_percent,SubmergentVegetation_percent,EmergentVegetation_percent,TreeCover_percent)
cover_data_RC07_long <- gather(cover_data_RC07, key = "component", value = "proportion", -year_numeric)
ggplot(cover_data_RC07_long,aes(x=year_numeric,y=proportion,fill=component)) + geom_area() + 
  scale_fill_manual(values = c("lightgreen", "steelblue", "grey","darkgreen"))

## RS10
cover_data_RC10 <- cover_data |>
  filter(LocationID == "RC10")
cover_data_RC10$GoogleEarthPhotoDate <- as.Date(cover_data_RC10$GoogleEarthPhotoDate)
cover_data_RC10$year_numeric <- as.numeric(format(cover_data_RC10$GoogleEarthPhotoDate, "%Y"))
cover_data_RC10 <- cover_data_RC10 |>
  select(year_numeric,OpenWater_percent,SubmergentVegetation_percent,EmergentVegetation_percent,TreeCover_percent)
cover_data_RC10_long <- gather(cover_data_RC10, key = "component", value = "proportion", -year_numeric)
ggplot(cover_data_RC10_long,aes(x=year_numeric,y=proportion,fill=component)) + geom_area() + 
  scale_fill_manual(values = c("lightgreen", "steelblue", "grey","darkgreen"))


## RL02
cover_data_RL02 <- cover_data |>
  filter(LocationID == "RL02")
cover_data_RL02$GoogleEarthPhotoDate <- as.Date(cover_data_RL02$GoogleEarthPhotoDate)
cover_data_RL02$year_numeric <- as.numeric(format(cover_data_RL02$GoogleEarthPhotoDate, "%Y"))
cover_data_RL02 <- cover_data_RL02 |>
  select(year_numeric,OpenWater_percent,SubmergentVegetation_percent,EmergentVegetation_percent,TreeCover_percent)
cover_data_RL02_long <- gather(cover_data_RL02, key = "component", value = "proportion", -year_numeric)
ggplot(cover_data_RL02_long,aes(x=year_numeric,y=proportion,fill=component)) + geom_area() + 
  scale_fill_manual(values = c("lightgreen", "steelblue", "grey","darkgreen"))

## TV02
cover_data_TV02 <- cover_data |>
  filter(LocationID == "TV02")
cover_data_TV02$GoogleEarthPhotoDate <- as.Date(cover_data_TV02$GoogleEarthPhotoDate)
cover_data_TV02$year_numeric <- as.numeric(format(cover_data_TV02$GoogleEarthPhotoDate, "%Y"))
cover_data_TV02 <- cover_data_TV02 |>
  select(year_numeric,OpenWater_percent,SubmergentVegetation_percent,EmergentVegetation_percent,TreeCover_percent)
cover_data_TV02_long <- gather(cover_data_TV02, key = "component", value = "proportion", -year_numeric)
ggplot(cover_data_TV02_long,aes(x=year_numeric,y=proportion,fill=component)) + geom_area() + 
  scale_fill_manual(values = c("lightgreen", "steelblue", "grey","darkgreen"))




cover_data$GoogleEarthPhotoDate <- as.Date(cover_data$GoogleEarthPhotoDate)
cover_data$year_numeric <- as.numeric(format(cover_data$GoogleEarthPhotoDate, "%Y"))
t1 <- cover_data %>%
  filter(LocationID == "LS01") 

## create model for all site


summary(lm(OpenWater_percent~year_numeric, data = t1))
plot(lm(OpenWater_percent~year_numeric, data = t1))

  
  





# TODO: create a filtered cover data table with intermediates for only the sites that Ruby did for every year
# to see if it's actually a linear relationship
# sites are LS01, RC07, RC10, RL02, TV02