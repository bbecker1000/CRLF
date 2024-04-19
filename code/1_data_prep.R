library(tidyverse)
library(dplyr)
library(here)
library(lubridate)
library(reshape2)

# reading in data from spreadsheets
setwd(here::here("code"))
raw_data <- read_csv(here::here("data", "CRLF_EGG_RAWDATA.csv"))
rainfall_daily <- read_csv(here::here("data", "cm_daily_rain.csv"))
rainfall_yearly <- read_csv(here::here("data", "cm_yearly_rain.csv"))
land_cover <- read_csv(here::here("data", "cover_estimates.csv"))

# removing unnecessary columns, making new column for total vegetation (to make sure it adds to 100), making data types more accurate/easier to use
# the DATA variable that this pipe generates has all validated rows and has not been filtered

# filtered data is denoted below this, and uses DATA as a starting point
data <- raw_data %>% select(-ParkCode, -ProjectCode, -BTime, -TTime, -USGS_ID, -SEASON, -SvyLength, -SvyWidth, -tblEvents.Comments, 
                            -DateEntered, -EventID, -SpeciesID, -WaterDepth, -EggDepth, -Distance, -EggMassStageID, -AS_UTMSOURCE, -AS_UTMZONE, 
                            -GPS_ID, -tblEggCount_CRLF.Comments, -RangeofEggMasses, -AttachType) %>% 
  filter(Validation == 'TRUE') %>%
  filter(OldMass == "FALSE") %>%
  mutate(Date = strptime(Date, format = "%m/%d/%Y")) %>%
  mutate(Survey_MONTH = as.integer(format(Date, "%m"))) %>%
  mutate(LocationID = as.factor(LocationID), Watershed = as.factor(Watershed), Date = as.Date(Date), Obsv1 = as.factor(Obsv1), Obsv2 = as.factor(Obsv2), Obsv3 = as.factor(Obsv3), 
         Weather = as.factor(Weather), Wind = as.integer(Wind), HabType= as.factor(HabType), SurveyMethodID = as.integer(SurveyMethodID), SalinityMethodID = as.integer(SalinityMethodID), 
         WaterFlowID = as.integer(WaterFlowID), MassID = as.integer(MassID), NumberofEggMasses = as.integer(NumberofEggMasses)) %>%
  mutate(
    beginningWY = case_when(
      month(Date) > 9 ~ floor_date(Date, unit = "year") + months(9),
      TRUE ~ floor_date(Date, unit = "year") - months(3) # gets the beginning of the water year for each date 
    ) # might be relevant to add that day of water year is zero-indexed, so October 1st is the 0th day of the water year. 
      # I think this makes the most sense, as computers like zero-indexed things, but humans often don't so we can reconsider if y'all want
  ) %>%
  mutate(dayOfWY = as.numeric(Date - beginningWY)) %>% # adds column for number of days after the beginning of the water year
  mutate(CoastalSite = if_else((LocationID == "LS01" | LocationID == "LS02" | LocationID == "LS03" | LocationID == "LS04" | LocationID == "LS08" | LocationID == "LS11" | 
                                  LocationID == "RC14" | LocationID == "RC20" | LocationID == "RC21" | LocationID == "RL04" | LocationID == "RL05" | LocationID == "TV06" | LocationID == "WG01"), TRUE, FALSE)) %>% 
  mutate(WaterSalinity = if_else(!CoastalSite & is.na(WaterSalinity), 0, WaterSalinity)) %>% 
  group_by(EggCountGUID) %>% 
  mutate(obsv_total = sum(!is.na(Obsv1), !is.na(Obsv2), !is.na(Obsv3))) %>% 
  ungroup() %>% 
  select(-Obsv1, -Obsv2, -Obsv3, -OldMass) %>% 
  left_join(., rainfall_yearly, join_by(BRDYEAR == Water_Year)) %>% 
  left_join(., land_cover, join_by(LocationID, BRDYEAR == year_numeric)) %>% 
  rename(
    ground_sub = PercentSubVeg,
    ground_emerg = PercentEmergVeg,
    ground_open_water = PercentOpenWater,
    interpolated_sub = SubmergentVegetation_percent,
    interpolated_emerg = EmergentVegetation_percent,
    interpolated_open_water = OpenWater_percent
  ) %>% 
  mutate(ground_percent_cover_validation = if_else(ground_sub + ground_emerg + ground_open_water == 100 & !is.na(ground_sub) & !is.na(ground_emerg) & !is.na(ground_open_water), TRUE, FALSE),
         interpolated_percent_cover_validation = !is.na(interpolated_sub)) %>% 
  rowwise() %>% 
  mutate(
    mean_percent_sub = if_else(ground_percent_cover_validation == TRUE, if_else(interpolated_percent_cover_validation, mean(c_across(all_of(c("ground_sub", "interpolated_sub")))), ground_sub), interpolated_sub),
    mean_percent_emerg = if_else(ground_percent_cover_validation == TRUE, if_else(interpolated_percent_cover_validation, mean(c_across(all_of(c("ground_emerg", "interpolated_emerg")))), ground_emerg), interpolated_emerg),
    mean_percent_water = if_else(ground_percent_cover_validation == TRUE, if_else(interpolated_percent_cover_validation, mean(c_across(all_of(c("ground_open_water", "interpolated_open_water")))), ground_open_water), interpolated_open_water)
  )

### ~~~ *** DATA FILTERING *** ~~~ ###

# filter to only include the 7 watersheds that Darren said had the most data and only sites that had at least 2 surveys in a given year
data <- data %>% 
  group_by(LocationID, BRDYEAR) %>% 
  summarize(survey_count_site_yr = n_distinct(EventGUID), .groups = "drop") %>% 
  ungroup() %>% 
  full_join(data, by = c("LocationID" = "LocationID", "BRDYEAR" = "BRDYEAR")) %>% 
  filter(survey_count_site_yr > 1) %>% 
  filter(Watershed == "Kanoff Creek" | Watershed == "Laguna Salada" | Watershed =="Milagra Creek"|
           Watershed == "Redwood Creek" | Watershed == "Rodeo Lagoon" | Watershed=="Tennessee Valley" |
           Watershed == "Wilkins Gulch")

### ~~~ *** BETWEEN YEAR DATA *** ~~~ ###
### this is complete besides BULLFROG and COVER DATA ###

between_year_data <- data %>% 
  select(LocationID, BRDYEAR, Watershed, NumberofEggMasses, AirTemp, WaterTemp, MaxD, WaterSalinity, CoastalSite, yearly_rain, mean_percent_sub, 
         mean_percent_emerg, mean_percent_water, ground_sub, ground_emerg, ground_open_water, interpolated_sub, interpolated_emerg, interpolated_open_water) %>% 
  group_by(LocationID, BRDYEAR) %>% 
  summarize(avg_max_depth_per_year = mean(MaxD),
         max_max_depth_per_year = max(MaxD),
         avg_salinity_per_year = mean(WaterSalinity, na.rm = TRUE),
         max_salinity_per_year = ifelse(all(is.na(WaterSalinity)), NA, max(WaterSalinity, na.rm = TRUE)),
         egg_masses_per_year = sum(NumberofEggMasses), 
         mean_percent_sub = mean(mean_percent_sub),
         mean_percent_emerg = mean(mean_percent_emerg),
         mean_percent_water = mean(mean_percent_water),
         mean_ground_sub = mean(ground_sub),
         mean_ground_emerg = mean(ground_emerg),
         mean_ground_open_water = mean(ground_open_water),
         mean_interpolated_sub = mean(interpolated_sub),
         mean_interpolated_emerg = mean(interpolated_emerg),
         mean_interpolated_open_water = mean(interpolated_open_water),
         across(everything(), ~first(.))) %>% 
  select(-MaxD, -WaterSalinity, -NumberofEggMasses, -ground_sub, -ground_emerg, -ground_open_water, -interpolated_sub, -interpolated_emerg, -interpolated_open_water) %>% 
  ungroup()

### ~~~ *** WITHIN YEAR DATA *** ~~~ ###

temp_daily_rain_table <- left_join(data, rainfall_daily, by = c("BRDYEAR" = "Water_Year")) %>%
  mutate(across(starts_with("day_"), as.numeric))

rain_to_date_col <- data.frame(matrix(nrow = nrow(temp_daily_rain_table), ncol = 1))
for (i in 1:nrow(temp_daily_rain_table)) {
  dayOfWY <- temp_daily_rain_table$dayOfWY[i]
  daysToSum <- select(temp_daily_rain_table[i , ], starts_with("day_"))[ , 1:(dayOfWY + 1)]
  rain_to_date_col[i, 1] <- sum(daysToSum)
}

colnames(rain_to_date_col) <- c("rain_to_date")
onset_of_breeding <- cbind(temp_daily_rain_table, rain_to_date_col) %>% select(-starts_with("day_")) %>% 
  select(LocationID, BRDYEAR, Watershed, dayOfWY, rain_to_date, MaxD, NumberofEggMasses, yearly_rain, AirTemp, WaterTemp) %>% 
  group_by(BRDYEAR, LocationID) %>% 
  filter(NumberofEggMasses > 0) %>% 
  mutate(MaxD_yearly = mean(MaxD, na.rm = TRUE),
         MaxD_proportion = if_else(!is.na(MaxD), MaxD/MaxD_yearly, NA)) %>% 
  arrange(BRDYEAR, LocationID, dayOfWY) %>% 
  slice(1) %>% 
  rename(first_breeding = dayOfWY)

