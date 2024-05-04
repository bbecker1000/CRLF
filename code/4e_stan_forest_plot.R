#causal plots

post <- extract.samples(fit)
PROBS = c(0.05, 0.5, 0.95) ##80 % CIs
#Effect of Rain --> Breach --> DO --> Goby
Rain_MaxDepth <- as_tibble(quantile( with(post, beta_Rain*beta_mean_max_depth), probs = PROBS)) 
Canopy_WaterTemp <- as_tibble(quantile( with(post, beta_Canopy*beta_WaterTemp), probs = PROBS)) 

