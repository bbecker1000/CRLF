# plots of covariate raw data between years
library(ggplot2)
library(tidyverse)
library(cowplot)

setwd(here::here("code"))
between_year_data <- read_csv(here::here("data", "between_year_data.csv"))


#### mean_percent_emerg ####
# emergent vegetation vs. egg masses
emergent_plot <- between_year_data %>% 
  ggplot(aes(x=mean_percent_emerg, y=num_egg_masses)) +
  geom_point() +
  geom_smooth() +
  xlab("Emergent Vegetation (%)") +
  ylab(NULL)

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
  geom_smooth() +
  xlab("Submergent Vegetation (%)")  +
  ylab(NULL)

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
  geom_smooth() +
  xlab("Open Water (%)") +
  ylab(NULL)

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
  geom_smooth() +
  xlab("Canopy (%)") +
  ylab(NULL)

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
  geom_smooth() +
  xlab("Rainfall") +
  ylab(NULL)

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
  geom_smooth() + # SE = false ? why is this below 0?
  xlab("Mean Max Depth") +
  ylab(NULL)

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
  geom_smooth() +
  xlab("Max Depth") +
  ylab(NULL)

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
  geom_smooth() +
  xlab("Air Temperature") +
  ylab(NULL)

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
  geom_smooth() +
  xlab("Water Temperature") +
  ylab(NULL)

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
  geom_smooth() +
  xlab("Mean Salinity (Coastal)") +
  ylab(NULL)

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
  geom_smooth() +
  xlab("Max Salinity (Coastal)") +
  ylab(NULL)

## max_salinity vs. time
max_salinity_BRD_plot <- coastal_between_year_data %>% 
  ggplot(aes(x=BRDYEAR, y=max_salinity)) +
  geom_point() +
  geom_smooth()

#### combined output with egg masses####
# saved in Ouput folder
plot_grid(emergent_plot, submergent_plot, open_water_plot, canopy_plot, rain_plot, mean_max_depth_plot, max_depth_plot, AirTemp_plot, WaterTemp_plot, mean_salinity_plot, max_salinity_plot, nrow=4)

