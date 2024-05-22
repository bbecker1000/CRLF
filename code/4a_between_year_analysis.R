library(glmmTMB)
library(tidyverse)
library(here)
library(bbmle) 
library(lme4)
library(sjPlot)
library(ggplot2)
setwd(here::here("code"))

between_year_data <- read_csv(here::here("data", "between_year_data.csv"))

#### initial model -- MISSING SALINITY ####
model1 <- glmmTMB(num_egg_masses ~ BRDYEAR + 
                    # mean_percent_emerg + 
                    # mean_percent_sub +
                    # mean_percent_water +
                    # interpolated_canopy +
                    # yearly_rain + #total annual rainfall
                    # mean_max_depth +
                    # max_depth +
                    # AirTemp +
                     WaterTemp +
                    # mean_salinity:CoastalSite +
                    # max_salinity:CoastalSite +
                    (1 | Watershed) +
                    (1 | LocationID),
                  data = between_year_data,
                  ziformula = ~1,
                  family = nbinom2) 
summary(model1)

#### lme4 model ####
model1 <- glmer(num_egg_masses ~ I(BRDYEAR - 2009) + 
                    # mean_percent_emerg + 
                    # mean_percent_sub +
                    mean_percent_water +
                    # interpolated_canopy +
                    yearly_rain + #total annual rainfall
                    mean_max_depth +
                    # max_depth +
                    AirTemp +
                    WaterTemp +
                    # mean_salinity:CoastalSite +
                    # max_salinity:CoastalSite +
                    (1 | Watershed) +
                    (1 | LocationID),
                  data = between_year_data,
                  # ziformula = ~1,
                  family = negative.binomial(1)) 
summary(model1)

#### complete case model with scaled covariates ####
# creating a "complete case" column
between_year_data$complete_case <- complete.cases(between_year_data)
complete_btw_data <- between_year_data %>% filter(complete_case == TRUE)

# scaling covariates
scaled_between_year <- complete_btw_data %>% 
  mutate(
    BRDYEAR = scale(BRDYEAR),
    mean_percent_sub = scale(mean_percent_sub),
    mean_percent_emerg = scale(mean_percent_emerg),
    mean_percent_water = scale(mean_percent_water),
    interpolated_canopy = scale(interpolated_canopy),
    yearly_rain = scale(yearly_rain),
    mean_max_depth = scale(mean_max_depth),
    max_depth = scale(max_depth),
    AirTemp = scale(AirTemp),
    WaterTemp = scale(WaterTemp),
    mean_salinity = scale(mean_salinity),
    max_salinity = scale(max_salinity)
  )

complete_case_model <- glmmTMB(num_egg_masses ~ BRDYEAR + 
                                 yearly_rain + 
                                 AirTemp +
                                 WaterTemp +
                                 # mean_percent_water +
                                 mean_salinity:CoastalSite +
                                 max_salinity:CoastalSite +
                                 interpolated_canopy,
                                 # (1 | Watershed) +
                                 # (1 | LocationID),
                               data = scaled_between_year,
                               ziformula = ~ yearly_rain +
                                 max_depth +
                                 AirTemp +
                                 WaterTemp +
                                 mean_percent_water +
                                 # mean_salinity:CoastalSite +
                                 # max_salinity:CoastalSite +
                                 # mean_percent_emerg +
                                 # mean_percent_sub +
                                 interpolated_canopy +
                                 (1 | Watershed) +
                                 (1 | LocationID),
                               family = nbinom2) 
summary(complete_case_model)



#### plotting complete case model ####

# forest plot (excluding salinity)
plot_model(complete_case_model, terms = c("BRDYEAR", "mean_percent_emerg", "mean_percent_sub", "mean_percent_water", 
                                          "interpolated_canopy", "yearly_rain", "mean_max_depth", "max_depth", "AirTemp", 
                                          "WaterTemp"), vline.color = "slategrey", show.p = TRUE)

# forest plot including salinity (not sure how to just take out the CoastalSiteFALSE variable)
plot_model(complete_case_model, vline.color = "slategrey", show.p = TRUE)

# plotting random effects
plot_model(complete_case_model, type = "re", vline.color = "slategrey")

# to test model assumptions
plot_model(complete_case_model,  type = "diag", vline.color = "slategrey")

#### active breeding (actBRD) site model ####

# how many sites active per year?
yearly_active_breeding <- between_year_data %>%
  mutate(breeding = if_else(num_egg_masses > 0, TRUE, FALSE)) %>% 
  filter(breeding == TRUE) %>% 
  select(BRDYEAR, Watershed, LocationID, yearly_rain) %>% 
  group_by(BRDYEAR)%>% 
  summarise(
    actBRD_sites = n(), # active breeding sites each year
    yearly_rain = sum(yearly_rain)
  )
yearly_active_breeding <- as.data.frame(active_breeding)

## plot
plot_active_sites <- yearly_active_breeding %>% ggplot(aes(x=BRDYEAR, y=actBRD_sites))+
  geom_line()+
  geom_point()+
  ylab("number of active breeding sites")
plot_active_sites

## TODO: plotting active breeding with rainfall
  
# create 0/1 binomial for breeding (where num_egg_masses == 0)
scaled_between_year <- scaled_between_year %>% 
  mutate(breeding = if_else(num_egg_masses > 0, TRUE, FALSE))

# logistic regression using glm
BRD_model1 <- glmer(breeding ~ BRDYEAR + 
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
                  data = scaled_between_year,
                  family = binomial)

summary(BRD_model1)
