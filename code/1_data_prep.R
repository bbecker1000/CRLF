library(tidyverse)
library(dplyr)
library(here)
library(lubridate)

setwd(here::here("code"))
raw_data <- read_csv(here::here("data", "CRLF_EGG_RAWDATA.csv"))
rainfall_daily <- read_csv(here::here("data", "cm_daily_rain.csv"))
rainfall_yearly <- read_csv(here::here("data", "cm_yearly_rain.csv"))

# removing unnecessary columns, making new column for total vegetation (to make sure it adds to 100), making data types more accurate/easier to use
# the DATA variable that this pipe generates has all validated rows and has not been filtered
# filtered data is denoted below this, and uses DATA as a starting point
data <- raw_data %>% select(-ParkCode, -ProjectCode, -BTime, -TTime, -USGS_ID, -SEASON, -SvyLength, -SvyWidth, -tblEvents.Comments, 
                            -DateEntered, -EventID, -SpeciesID, -WaterDepth, -EggDepth, -Distance, -EggMassStageID, -AS_UTMSOURCE, -AS_UTMZONE, 
                            -GPS_ID, -tblEggCount_CRLF.Comments, -RangeofEggMasses, -AttachType) %>% 
  filter(Validation == 'TRUE') %>%
  mutate(TotalVeg = PercentSubVeg + PercentEmergVeg + PercentOpenWater) %>%
  mutate(Date = strptime(Date, format = "%m/%d/%Y")) %>%
  mutate(Survey_MONTH = as.integer(format(Date, "%m"))) %>%
  mutate(LocationID = as.factor(LocationID), Watershed = as.factor(Watershed), Date = as.Date(Date), Obsv1 = as.factor(Obsv1), Obsv2 = as.factor(Obsv2), Obsv3 = as.factor(Obsv3), 
         Weather = as.factor(Weather), Wind = as.integer(Wind), HabType= as.factor(HabType), SurveyMethodID = as.integer(SurveyMethodID), SalinityMethodID = as.integer(SalinityMethodID), 
         WaterFlowID = as.integer(WaterFlowID), MassID = as.integer(MassID), NumberofEggMasses = as.integer(NumberofEggMasses)) %>%
  mutate(
    beginningWY = case_when(
      month(Date) > 9 ~ floor_date(Date, unit = "year") + months(9),
      TRUE ~ floor_date(Date, unit = "year") - months(3) # gets the beginning of the water year for each date 
    )
  ) %>%
  mutate(dayOfWY = as.numeric(Date - beginningWY)) # adds column for number of days after the beginning of the water year

### ~~~ *** NUMBER OF OBSERVERS *** ~~~ ###

# creates a column that takes the sum of non-NA values in Obsv1, Obsv2, Obsv3 columns
total_observations <- data %>% group_by(EggCountGUID) %>% summarise(Obsv_Total = sum(!is.na(Obsv1),!is.na(Obsv2),!is.na(Obsv3)))

data$obsv_total <- total_observations$Obsv_Total

### ~~~ *** INCORPORATING RAINFALL DATA *** ~~~ ###
data <- left_join(data, rainfall_yearly, join_by(BRDYEAR == Water_Year))

# temp_daily_rain_table <- left_join(data, rainfall_daily, join_by())

# make daily rain table have 365 columns, one for each day of WY
# index of col = day of WY

### ~~~ *** DATA FILTERING *** ~~~ ###

# filter to only include the 7 watersheds that Darren said had the most data
data <- data %>% 
  filter(Watershed == "Kanoff Creek" | Watershed == "Laguna Salada" | Watershed =="Milagra Creek"|
           Watershed == "Redwood Creek" | Watershed == "Rodeo Lagoon" | Watershed=="Tennessee Valley" |
           Watershed == "Wilkins Gulch")

# filters data to only include sites that had at least 2 surveys in a given year
survey_count_filtered <- data %>% 
  group_by(LocationID, BRDYEAR) %>% 
  summarize(survey_count_site_yr = n_distinct(EventGUID))
  
data <- survey_count_filtered %>% full_join(data, by = c("LocationID" = "LocationID", "BRDYEAR" = "BRDYEAR")) %>%
  filter(survey_count_site_yr > 1)


