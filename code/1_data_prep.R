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

# removing unnecessary columns, making new column for total vegetation (to make sure it adds to 100), making data types more accurate/easier to use
# the DATA variable that this pipe generates has all validated rows and has not been filtered
# filtered data is denoted below this, and uses DATA as a starting point
data <- raw_data %>% select(-ParkCode, -ProjectCode, -BTime, -TTime, -USGS_ID, -SEASON, -SvyLength, -SvyWidth, -tblEvents.Comments, 
                            -DateEntered, -EventID, -SpeciesID, -WaterDepth, -EggDepth, -Distance, -EggMassStageID, -AS_UTMSOURCE, -AS_UTMZONE, 
                            -GPS_ID, -tblEggCount_CRLF.Comments, -RangeofEggMasses, -AttachType, -PercentSubVeg, -PercentEmergVeg, -PercentOpenWater) %>% 
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
  mutate(CoastalSite = if_else((LocationID == "LS01" | LocationID == "LS02" | LocationID == "LS03" | LocationID == "LS04" | LocationID == "LS11" | LocationID == "RC20"
                                   | LocationID == "RC21" | LocationID == "RL04" | LocationID == "RL05" | LocationID == "TV06" | LocationID == "WG01"), TRUE, FALSE)) %>% 
  mutate(WaterSalinity = if_else(!CoastalSite & is.na(WaterSalinity), 0, WaterSalinity)) %>% 
  group_by(EggCountGUID) %>% 
  mutate(obsv_total = sum(!is.na(Obsv1), !is.na(Obsv2), !is.na(Obsv3))) %>% 
  ungroup() %>% 
  select(-Obsv1, -Obsv2, -Obsv3, -OldMass) %>% 
  left_join(., rainfall_yearly, join_by(BRDYEAR == Water_Year))

### ~~~ *** DATA FILTERING *** ~~~ ###

# filter to only include the 7 watersheds that Darren said had the most data
data <- data %>% 
  filter(Watershed == "Kanoff Creek" | Watershed == "Laguna Salada" | Watershed =="Milagra Creek"|
           Watershed == "Redwood Creek" | Watershed == "Rodeo Lagoon" | Watershed=="Tennessee Valley" |
           Watershed == "Wilkins Gulch")

# filters data to only include sites that had at least 2 surveys in a given year
survey_count_filtered <- data %>% 
  group_by(LocationID, BRDYEAR) %>% 
  summarize(survey_count_site_yr = n_distinct(EventGUID), .groups = "drop") %>% 
  ungroup()
  
data <- survey_count_filtered %>%
  full_join(data, by = c("LocationID" = "LocationID", "BRDYEAR" = "BRDYEAR")) 

data <- data %>%
  filter(survey_count_site_yr > 1)

### ~~~ *** BETWEEN YEAR DATA *** ~~~ ###

between_year_data <- data %>% 
  select(-dayOfWY, -Date, -Survey_MONTH, -Weather, -Wind, -AirTemp, -WaterTemp, -WaterVis, -Validation, -SurveyMethodID, -SalinityMethodID, -WaterFlowID, -MassID, -Stranded,
         -UTMNY_EggObs, -UTMEX_EggObs, -HorizontalError_m, -Lat_EggObs, -Long_EggObs, -beginningWY, -GageHeight, -DominantSub, -DominantEmerg, -OtherVeg) %>% 
  group_by(LocationID, BRDYEAR) %>% 
  mutate(avg_max_depth_per_year = mean(MaxD),
         max_max_depth_per_year = max(MaxD),
         avg_salinity_per_year = mean(WaterSalinity, na.rm = TRUE),
         max_salinity_per_year = ifelse(all(is.na(WaterSalinity)), NA, max(WaterSalinity, na.rm = TRUE)))

### ~~~ *** WITHIN YEAR DATA *** ~~~ ###

temp_daily_rain_table <- left_join(data, rainfall_daily, by = c("BRDYEAR" = "Water_Year")) %>%
  mutate(across(starts_with("day_"), as.numeric))

rain_to_date_col <- data.frame(matrix(nrow = nrow(temp_daily_rain_table), ncol = 1))
for (i in 1:nrow(temp_daily_rain_table)) {
  dayOfWY <- temp_daily_rain_table$dayOfWY[i]
  daysToSum <- select(temp_daily_rain_table[i , ], starts_with("day_"))[ , 1:(dayOfWY + 1)]
  rain_to_date_col[i, 1] <- sum(daysToSum)
}

colnames(rain_to_date_col) <- c("Rain_to_date")
within_year_data <- cbind(temp_daily_rain_table, rain_to_date_col) %>% select(-starts_with("day_")) %>% 
  select(-Weather, -Wind, -AirTemp, -WaterTemp, -WaterVis, -Validation, -SurveyMethodID, -SalinityMethodID, -WaterFlowID, -MassID, -Stranded,
         -UTMNY_EggObs, -UTMEX_EggObs, -HorizontalError_m, -Lat_EggObs, -Long_EggObs, -GageHeight, -DominantSub, -DominantEmerg, -OtherVeg)

