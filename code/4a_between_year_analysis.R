library("glmmTMB")
library(tidyverse)
library(here)
library("bbmle") 
setwd(here::here("code"))

source("1_data_prep.R")

between_year_data <- read_csv(here::here("data", "between_year_data.csv"))

# lmer or glmmTMB model to plot this

# ## defining covariates from between year data
# egg_mass <- between_year_data$num_egg_masses
# year <- between_year_data$BRDYEAR
# 
# emerg_veg <- between_year_data$mean_percent_emerg
# sub_veg <- between_year_data$mean_percent_sub
# water <- between_year_data$mean_percent_water
# canopy <- between_year_data$interpolated_canopy
# 
# rain <- between_year_data$yearly_rain
# mean_max_depth <- between_year_data$mean_max_depth
# max_depth <- between_year_data$max_depth
# 
# temp_air <- between_year_data$AirTemp
# temp_water <- between_year_data$WaterTemp
# 
# watershed <- between_year_data$Watershed
# site <- between_year_data$LocationID

# alternatively: dplyr pipe so that all these variables can be in the same data frame


# initial model -- MISSING SALINITY
model1 <- glmmTMB(num_egg_masses ~ BRDYEAR + 
                    mean_percent_emerg + 
                    mean_percent_sub +
                    mean_percent_water +
                    interpolated_canopy +
                    yearly_rain + #total annual rainfall
                    mean_max_depth +
                    max_depth +
                    AirTemp +
                    WaterTemp +
                    mean_salinity:CoastalSite +
                    max_salinity:CoastalSite +
                    (1 | Watershed) +
                    (1 | LocationID),
                  data = between_year_data,
                  ziformula = ~1,
                  family = poisson) 
summary(model1)

#### COMPLETE CASES ####
# creating a "complete case" column
between_year_data$complete_case <- complete.cases(between_year_data)

# 161 complete cases 
complete_btw_data <- between_year_data %>% filter(complete_case == TRUE)

model2 <- glmmTMB(num_egg_masses ~ BRDYEAR + 
                    mean_percent_emerg + 
                    mean_percent_sub +
                    mean_percent_water +
                    interpolated_canopy +
                    yearly_rain + #total annual rainfall
                    mean_max_depth +
                    max_depth +
                    AirTemp +
                    WaterTemp +
                    mean_salinity:CoastalSite +
                    max_salinity:CoastalSite +
                    (1 | Watershed) +
                    (1 | LocationID),
                  data = complete_btw_data,
                  ziformula = ~1,
                  family = poisson) 
summary(model2)
