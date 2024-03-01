library(tidyverse)
library(dplyr)
library(here)

setwd(here::here("code"))
raw_data <- read_csv(here::here("data", "CRLF_EGG_RAWDATA.csv"))

#removing unnecessary columns, making new column for total vegetation (to make sure it adds to 100), making date formats easier to use
  data <- raw_data %>% select(-ParkCode, -ProjectCode, -BTime, -TTime, -USGS_ID, -SEASON, -tblEvents.Comments, -DateEntered, -EventID, -SpeciesID, -WaterDepth, -EggDepth, -Distance, -EggMassStageID, -AS_UTMSOURCE, -AS_UTMZONE, -GPS_ID, -tblEggCount_CRLF.Comments, -RangeofEggMasses) %>% 
  filter(Validation == 'TRUE') %>%
  mutate(TotalVeg = PercentSubVeg + PercentEmergVeg + PercentOpenWater) %>%
  mutate(Date = strptime(Date, format = "%m/%d/%Y")) %>%
  mutate(Survey_MONTH = as.integer(format(Date, "%m")))

data

#checking type of each column
str(data,na.rm = TRUE)
