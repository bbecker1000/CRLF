library(brms)



mod.brm <- brm(bf(num_egg_masses ~  #bf creates a model statement for compilation
                      s(BRDYEAR_scaled) + 
                      s(yearly_rain_scaled) +
                      s(mean_percent_water_scaled) + 
                      s(interpolated_canopy_scaled) +
                      s(WaterTemp_scaled) +  
                      CoastalSite + #not smoothed
                      (1|Watershed/LocationID),
               zi ~ yearly_rain_scaled),
               data = scaled_between_year,
               family = zero_inflated_negbinomial(),
               chains = 2, cores = 2,
               control = list(adapt_delta = 0.98)) #reduce divergences

summary(mod.brm)
#pairs(mod.brm)
conditional_effects(mod.brm, surface = FALSE, prob = 0.8)




#for forest plot
devtools::install_github("mvuorre/brmstools")
library(brmstools) #from github
forest(mod.brm)

