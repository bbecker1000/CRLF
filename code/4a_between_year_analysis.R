library(glmmTMB)
library(tidyverse)
library(here)
#library(bbmle) 
library(lme4)
library(sjPlot)
library(mgcv)
library(gamlss)
library(gratia)


between_year_data <- read_csv(here::here("data", "between_year_data.csv"))  %>% 
  mutate(LocationID = as.factor(LocationID),
         Watershed = as.factor(Watershed), 
         LocationInWatershed = interaction(Watershed, LocationID))

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
    max_salinity = scale(max_salinity),
  )

complete_case_model <- glmmTMB(num_egg_masses ~ BRDYEAR + 
                                 mean_percent_emerg +
                                 mean_percent_sub +
                                 mean_percent_water +
                                 interpolated_canopy +
                                 yearly_rain + 
                                 mean_max_depth +
                                 max_depth +
                                 WaterTemp +
                                 (1 | LocationID),
                               data = scaled_between_year,
                               ziformula = ~ yearly_rain +
                                 max_depth +
                                 WaterTemp +
                                 mean_salinity:CoastalSite +
                                 max_salinity:CoastalSite +
                                 interpolated_canopy +
                                 (1 | LocationID),
                               family = nbinom2)
summary(complete_case_model)

complete_case_model_glmer <- glmer(num_egg_masses ~ BRDYEAR + 
                                     I(BRDYEAR^2) +
                                     # mean_percent_emerg + 
                                     # mean_percent_sub +
                                     # mean_percent_water +
                                     # interpolated_canopy +
                                     yearly_rain + 
                                     mean_max_depth +
                                     # max_depth +
                                     # AirTemp +
                                     WaterTemp +
                                     # mean_percent_water +
                                     # mean_salinity:CoastalSite +
                                     # max_salinity:CoastalSite +
                                     interpolated_canopy +
                                     (1 | LocationID),# +
                                   #(1 | LocationID),
                                   data = scaled_between_year,
                                   family = negative.binomial(0.88)) 
summary(complete_case_model_glmer)

plot_model(complete_case_model_glmer)



plot_model(complete_case_model)

#### GAM model ####
model1.gam <- gam(num_egg_masses ~ s(BRDYEAR) + 
                    s(mean_percent_emerg, k = 3) + # small k so that it doesn't get too wigggly for cover data
                    s(mean_percent_sub, k = 3) +   # alternatively we could just not smooth these terms?
                    s(mean_percent_water, k = 3) +
                    s(interpolated_canopy, k = 3) +
                    s(yearly_rain) + 
                    mean_max_depth +
                    max_depth + # is it ok to have 2 measures of max depth?
                    # s(AirTemp) + # thinking of excluding because I'm not sure how biologically relevant it is...
                    s(WaterTemp) + # not sure if this should be a smooth variable or not
                    mean_salinity:CoastalSite +
                    max_salinity:CoastalSite +
                    # s(Watershed, bs = 're') +
                    # s(LocationID, bs = 're') +
                    s(LocationInWatershed, bs = 're'),  # one random effect for sites in watersheds
                  data = complete_btw_data,
                  family = negbin(0.88))
summary(model1.gam)

#### plotting GAM model ####
# check assumptions
appraise(model1.gam)

# smooth terms
draw(model1.gam)

# all terms
plot(model1.gam, pages = 1, all.terms = TRUE, rug = TRUE)

# just over year
draw(model1.gam, select = 1, rug = FALSE)

# just for rainfall
draw(model1.gam, select = 6)

### zero-inflated GAM model -- not working yet ####
# gamlss uses pb() instead of s()

model1.gamlss <- gamlss(num_egg_masses ~ pb(BRDYEAR) + 
                          mean_percent_emerg + 
                          mean_percent_sub +
                          pb(mean_percent_water) +
                          interpolated_canopy +
                          pb(yearly_rain) + 
                          mean_max_depth +
                          max_depth +
                          AirTemp +
                          pb(WaterTemp) +
                          mean_salinity:CoastalSite +
                          max_salinity:CoastalSite +
                          # re(random = ~1 | Watershed) +
                          # re(random = ~1 | LocationID),
                          re(random = ~1 | LocationInWatershed),
                        data = complete_btw_data,
                        family = ZINBI,
                        control = gamlss.control(n.cyc = 20))

summary(model1.gamlss)

#### initial model -- ignore for now ####
model1 <- glmmTMB(num_egg_masses ~ BRDYEAR +
                     yearly_rain +
                    (1 | LocationID),
                  data = between_year_data,
                  ziformula = ~0,
                  family = nbinom2) 
summary(model1)

#### lme4 model ####
model1 <- glmer(num_egg_masses ~ I(BRDYEAR - 2009) + 
                    # mean_percent_emerg + 
                    # mean_percent_sub +
                    # mean_percent_water +
                    # interpolated_canopy +
                    yearly_rain + #total annual rainfall
                    #mean_max_depth +
                    # max_depth +
                    #AirTemp +
                    # WaterTemp +
                    # mean_salinity:CoastalSite +
                    # max_salinity:CoastalSite +
                    (1 | Watershed/LocationID),
                  data = between_year_data,
                  family = negative.binomial(0.88) )
summary(model1)


#some gams to deal with non-linearity
#need groups to be factors

between_year_data$Watershed <- factor(between_year_data$Watershed)
between_year_data$LocationID <- factor(between_year_data$LocationID)
between_year_data$CoastalSite <- factor(between_year_data$CoastalSite)



#### plotting complete case model ####

# forest plot (excluding salinity)
plot_model(complete_case_model, terms = c("BRDYEAR", "mean_percent_emerg", "mean_percent_sub", "mean_percent_water", 
                                          "interpolated_canopy", "yearly_rain", "mean_max_depth", "max_depth", "AirTemp", 
                                          "WaterTemp"), vline.color = "slategrey", show.p = TRUE)

# forest plot including salinity (not sure how to just take out the CoastalSiteFALSE variable)
plot_model(complete_case_model, vline.color = "slategrey", show.p = TRUE)

## BB look into whether we can fix the coefficient for coastal site = FALSE
 ## PREFERRED data so no variation and slope = 0?  replace zeros with NAs
 ## Robin will do this.
## BB Need to figure out how to remove the coeffient for coastal site from plot.

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
