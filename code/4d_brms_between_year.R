library(brms)

# testing to make sure i can push from new computer --Robin

mod.brm <- brm(bf(num_egg_masses ~  #bf creates a model statement for compilation
                      s(BRDYEAR_scaled) + 
                      s(yearly_rain_scaled) +
                      s(mean_percent_water_scaled) + 
                      s(interpolated_canopy_scaled) +
                      s(WaterTemp_scaled) +  
                      max_depth_scaled +
                      (max_salinity_scaled * CoastalSite) + # not smoothed
                      (1|Watershed/LocationID),
               zi ~ s(yearly_rain_scaled) +      # inflated model for zeros
                 (1|Watershed/LocationID)),   # added random effects 
               data = scaled_between_year,
               family = zero_inflated_negbinomial(),
               chains = 2, cores = 2,
               iter = 6000, # needs more iterations with added covariates
               control = list(adapt_delta = 0.98)) #reduce divergences

save(mod.brm, file = "Output/mod.brm.RData")
#load("Output/mod.brm.RData")

  
summary(mod.brm)

mod.hurdle <- brm(
  bf(num_egg_masses ~ 
       s(BRDYEAR_scaled) + 
       s(yearly_rain_scaled) +
       s(mean_percent_water_scaled) + 
       s(interpolated_canopy_scaled) +
       s(WaterTemp_scaled) +  
       (max_salinity_scaled * CoastalSite) + 
       (1 | Watershed/LocationID),
     hu ~ yearly_rain_scaled +      # inflated model for zeros
       (1|Watershed/LocationID)),
  data = scaled_between_year,
  family = hurdle_negbinomial(),
  chains = 2, cores = 2,
  control = list(adapt_delta = 0.98)
)

save(mod.hurdle, file = "Output/mod.hurdle.RData")
#load("Output/mod.hurdle.RData")
summary(mod.hurdle)


#pairs(mod.brm)
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


