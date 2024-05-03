library("glmmTMB")
library(tidyverse)
library(here)
setwd(here::here("code"))

source("1_data_prep.R")

between_year_data <- read_csv(here::here("data", "between_year_data.csv"))

# lmer or glmmTMB model to plot this

## defining covarites from between year data
egg_mass <- between_year_data$num_egg_masses
year <- between_year_data$BRDYEAR

emerg_veg <- between_year_data$mean_percent_emerg
sub_veg <- between_year_data$mean_percent_sub
water <- between_year_data$mean_percent_water
canopy <- between_year_data$interpolated_canopy

rain <- between_year_data$yearly_rain
mean_max_depth <- between_year_data$mean_max_depth
max_depth <- between_year_data$max_depth

temp_air <- between_year_data$AirTemp
temp_water <- between_year_data$WaterTemp

watershed <- between_year_data$Watershed
site <- between_year_data$LocationID

# alternatively: dplyr pipe so that all these variables can be in the same data frame


# initial model -- MISSING SALINITY
model1 <- glmmTMB(egg_mass ~ year + 
                    emerg_veg + 
                    sub_veg +
                    water +
                    canopy +
                    rain + #total annual rainfall
                    mean_max_depth +
                    max_depth +
                    temp_air +
                    temp_water +
                    (1|watershed/site) +
                    ar(1),
                  data = between_year_data,
                  family = poisson) #autoregressive (temporal autocorrelation)
summary(model1)
