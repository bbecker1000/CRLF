# plots of covariate raw data between years
library(ggplot2)
library(tidyverse)

setwd(here::here("code"))
between_year_data <- read_csv(here::here("data", "between_year_data.csv"))


#### mean_percent_emerg ####
# emergent vegetation vs. egg masses
emergent_plot <- between_year_data %>% 
  ggplot(aes(x=mean_percent_emerg, y=num_egg_masses)) +
  geom_point() +
  geom_smooth()

# emergent vs. time
emergent_BRD_plot <- between_year_data %>% 
  ggplot(aes(x=BRDYEAR, y=mean_percent_emerg)) +
  geom_point() +
  geom_smooth()

#### mean_percent_sub ####
# submerg vs. egg masses
submergent_plot <- between_year_data %>% 
  ggplot(aes(x=mean_percent_sub, y=num_egg_masses)) +
  geom_point() +
  geom_smooth()

## submerg vs. time
subemergent_BRD_plot <- between_year_data %>% 
  ggplot(aes(x=BRDYEAR, y=mean_percent_sub)) +
  geom_point() +
  geom_smooth()

#### mean_percent_water ####
# open water vs. egg masses
open_water_plot <- between_year_data %>% 
  ggplot(aes(x=mean_percent_water, y=num_egg_masses)) +
  geom_point() +
  geom_smooth()

## open water vs. time
open_water_BRD_plot <- between_year_data %>% 
  ggplot(aes(x=BRDYEAR, y=mean_percent_water)) +
  geom_point() +
  geom_smooth()

#### interpolated_canopy ####
# canopy vs. egg masses
canopy_plot <- between_year_data %>% 
  ggplot(aes(x=interpolated_canopy, y=num_egg_masses)) +
  geom_point() +
  geom_smooth()

## canopy vs. time
canopy_BRD_plot <- between_year_data %>% 
  ggplot(aes(x=BRDYEAR, y=interpolated_canopy)) +
  geom_point() +
  geom_smooth()

#### yearly_rain ####
# yearly rain vs. egg masses
rain_plot <- between_year_data %>% 
  ggplot(aes(x=yearly_rain, y=num_egg_masses)) +
  geom_point() +
  geom_smooth()

## yearly rain vs. time
rain_BRD_plot <- between_year_data %>% 
  ggplot(aes(x=BRDYEAR, y=yearly_rain)) +
  geom_point() +
  geom_smooth()

#### mean_max_depth ####
# mean_max_depth vs. egg masses
mean_max_depth_plot <- between_year_data %>% 
  ggplot(aes(x=mean_max_depth, y=num_egg_masses)) +
  geom_point() +
  geom_smooth() # SE = false ? why is this below 0?

## mean_max_depth vs. time
mean_max_depth_BRD_plot <- between_year_data %>% 
  ggplot(aes(x=BRDYEAR, y=mean_max_depth)) +
  geom_point() +
  geom_smooth()

#### max_depth ####
# max_depth vs. egg masses
max_depth_plot <- between_year_data %>% 
  ggplot(aes(x=max_depth, y=num_egg_masses)) +
  geom_point() +
  geom_smooth()

## max_depth vs. time
max_depth_BRD_plot <- between_year_data %>% 
  ggplot(aes(x=BRDYEAR, y=max_depth)) +
  geom_point() +
  geom_smooth()

#### AirTemp ####
# AirTemp vs. egg masses
AirTemp_plot <- between_year_data %>% 
  ggplot(aes(x=AirTemp, y=num_egg_masses)) +
  geom_point() +
  geom_smooth()

## AirTemp vs. time
AirTemp_BRD_plot <- between_year_data %>% 
  ggplot(aes(x=BRDYEAR, y=AirTemp)) +
  geom_point() +
  geom_smooth()

#### WaterTemp ####
# WaterTemp vs. egg masses
WaterTemp_plot <- between_year_data %>% 
  ggplot(aes(x=WaterTemp, y=num_egg_masses)) +
  geom_point() +
  geom_smooth()

## WaterTemp vs. time
WaterTemp_BRD_plot <- between_year_data %>% 
  ggplot(aes(x=BRDYEAR, y=WaterTemp)) +
  geom_point() +
  geom_smooth()

#### mean_salinity:CoastalSite ####
#TODO: do these look right?
coastal_between_year_data <- between_year_data %>% 
  filter(CoastalSite == TRUE)

# mean_salinity vs. egg masses
mean_salinity_plot <- coastal_between_year_data %>% 
  ggplot(aes(x=mean_salinity, y=num_egg_masses)) +
  geom_point() +
  geom_smooth()

## mean_salinity vs. time
mean_salinity_BRD_plot <- coastal_between_year_data %>% 
  ggplot(aes(x=BRDYEAR, y=mean_salinity)) +
  geom_point() +
  geom_smooth()

#### max_salinity:CoastalSite ####
# max_salinity vs. egg masses
max_salinity_plot <- coastal_between_year_data %>% 
  ggplot(aes(x=max_salinity, y=num_egg_masses)) +
  geom_point() +
  geom_smooth()

## max_salinity vs. time
max_salinity_BRD_plot <- coastal_between_year_data %>% 
  ggplot(aes(x=BRDYEAR, y=max_salinity)) +
  geom_point() +
  geom_smooth()

#### combined output with egg masses####