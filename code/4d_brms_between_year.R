library(brms)

# testing to make sure i can push from new computer --Robin
t0 <- Sys.time()
mod.brm <- brm(bf(num_egg_masses ~  #bf creates a model statement for compilation
                      s(BRDYEAR_scaled) + 
                      # s(yearly_rain_scaled) + ## commented out because water_regime added
                      s(mean_percent_water_scaled) + 
                      s(interpolated_canopy_scaled) +
                      s(WaterTemp_scaled) +  
                      max_depth_scaled +
                      (max_salinity_scaled * CoastalSite) + # not smoothed
                      s(yearly_rain_scaled * water_regime) + # smooth or not?
                      (1|Watershed/LocationID),
               zi ~ s(yearly_rain_scaled) +      # inflated model for zeros
                 (1|Watershed/LocationID)),   # added random effects 
               data = scaled_between_year,
               family = zero_inflated_negbinomial(),
               chains = 2, cores = 2,
               iter = 8000, # needs more iterations with added covariates
               control = list(adapt_delta = 0.98)) #reduce divergences

save(mod.brm, file = "Output/mod.brm.RData")
#load("Output/mod.brm.RData")

  
summary(mod.brm)
t1 <- Sys.time()
t1-t0 # zi-model run time

## hurdle model ##
t2 <- Sys.time()

#set priors for problematic covariates
#could add more prioris if helpful
bprior <- c(prior(student_t(1, 0.5, 0.5), #slightly positive based on prior knowledge
                  coef = syearly_rain_scaled_1),
            prior(student_t(1, 0.5, 0.5),  #slightly positive based on prior knowledge
                  coef = syearly_rain_scaled:water_regimeperennial_1),
            prior(student_t(1, 0, 0.5), 
                  coef =  syearly_rain_scaled:water_regimeseasonal_1),
            prior(student_t(1, 0, 0.5), 
                  coef =  smax_salinity_scaled:CoastalSiteFALSE_1),
            prior(student_t(1, -0.25, 0.5),  #slightly negative based on prior knowledge
                  coef =  smax_salinity_scaled:CoastalSiteTRUE_1)
)


mod.hurdle <- brm(
  bf(num_egg_masses ~ 
       s(BRDYEAR_scaled) + 
       s(yearly_rain_scaled) +
       s(mean_percent_water_scaled) + 
       CoastalSite +  #new
       water_regime + #new
       s(interpolated_canopy_scaled) +
       s(WaterTemp_scaled) +  
       s(max_salinity_scaled, by = CoastalSite) + # not smoothed
       s(yearly_rain_scaled, by = water_regime) + # smooth or not?
       (1 | Watershed/LocationID),
     hu ~ s(yearly_rain_scaled, by = water_regime) + #new water regime for hurdle 
       (1|Watershed/LocationID)),
  data = scaled_between_year,
  prior = bprior, #set above before model
  family = hurdle_negbinomial(),
  chains = 3, cores = 3,
  iter = 5000, # needs more iterations with interactions
  control = list(adapt_delta = 0.98,   #reduce divergences, 
                 max_treedepth = 20))  #had treedepth exceedances at default 10

save(mod.hurdle, file = "Output/mod.hurdle.RData")
#load("Output/mod.hurdle.RData")
summary(mod.hurdle)

t3 <- Sys.time()
t3-t2 # hurdle model run time

#get coefficient names for priors
prior_summary(mod.hurdle) 

# pairs(mod.brm)
conditional_effects(mod.brm, surface = FALSE, prob = 0.8)
conditional_effects(mod.hurdle, surface = FALSE, prob = 0.8)
#from Mark
conditional_effects(mod.brm)|>
  plot(points = TRUE, theme = theme_classic())

#plot the hurdle effect by adding dpar
conditional_effects(mod.hurdle, dpar = "hu")







#try some plots with 
library(tidybayes)
#search tidybayes + brms for vignette

library(brms)
library(tidybayes)
library(ggplot2)
library(tidyverse)
# Tried something suggested by Chatgpt...

# Posterior predictive distribution plot
mod.brm|>
  tidybayes::add_predicted_draws(newdata = scaled_between_year, allow_new_levels = TRUE)|>
  ggplot(aes(x = .prediction, y = num_egg_masses)) +
  geom_point(alpha = 0.5) +
  geom_density_2d() +
  labs(x = "Predicted Number of Egg Masses", y = "Observed Number of Egg Masses") +
  theme_classic()

# Posterior intervals for fixed effects
mod.brm|>
  tidybayes::spread_draws(b_Intercept, b_CoastalSiteTRUE, b_zi_yearly_rain_scaled)|>
  tidyr::pivot_longer(cols = starts_with("b_"), names_to = "Variable", values_to = "Estimate") |>
  ggplot(aes(x = Estimate, y = Variable)) +
  tidybayes::geom_halfeyeh() +
  labs(x = "Estimate", y = "Variable") +
  theme_classic()

# Smooth terms visualization
mod.brm|>
  tidybayes::add_epred_draws(newdata = scaled_between_year, allow_new_levels = TRUE)|>
  ggplot(aes(x = BRDYEAR_scaled, y = .epred)) +
  tidybayes::stat_lineribbon(aes(y = num_egg_masses), .width = c(0.95, 0.8, 0.5)) +
  geom_point(data = scaled_between_year, aes(x = BRDYEAR_scaled, y = num_egg_masses), alpha = 0.5) +
  scale_fill_brewer() +
  labs(x = "BRDYEAR_scaled", y = "Number of Egg Masses") +
  theme_classic()

# Conditional effects plot
conditional_effects(mod.brm)|>
  plot(points = TRUE, theme = theme_classic())

