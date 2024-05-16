#mvgam

library(mvgam)
data("portal_data")

portal_data %>%
  
  # mvgam requires a 'time' variable be present in the data to index
  # the temporal observations. This is especially important when tracking 
  # multiple time series. In the Portal data, the 'moon' variable indexes the
  # lunar monthly timestep of the trapping sessions
  dplyr::mutate(time = moon - (min(moon)) + 1) %>%
  
  # We can also provide a more informative name for the outcome variable, which 
  # is counts of the 'PP' species (Chaetodipus penicillatus) across all control
  # plots
  dplyr::mutate(count = PP) %>%
  
  # The other requirement for mvgam is a 'series' variable, which needs to be a
  # factor variable to index which time series each row in the data belongs to.
  # Again, this is more useful when you have multiple time series in the data
  dplyr::mutate(series = as.factor('PP')) %>%
  
  # Select the variables of interest to keep in the model_data
  dplyr::select(series, year, time, count, mintemp, ndvi) -> model_data


plot_mvgam_series(data = model_data, series = 1, y = 'count')


model_data %>%
  
  # Create a 'year_fac' factor version of 'year'
  dplyr::mutate(year_fac = factor(year)) -> model_data

attach(rstan)
model1 <- mvgam(count ~ s(year_fac, bs = 're') - 1,
                family = poisson(),
                data = model_data)

searchsummary(model1)
