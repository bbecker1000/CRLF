# rainfall correlations, etc
# rainfall calculations that are helpful for EDA but not part of data prep or analysis

### *** ~~~ THIS CODE WILL NOT WORK OR RUN AT ALL ~~~ *** ###
# that's ok, and by design (for now). the goal is to get 3_rainfall_data_prep.R to output
# a CSV file that can be used for EDA here

# plot rainfall by location
ggplot(data = rain_to_compare, aes(x = Water_Year, y = rainfall, color = location)) + geom_line()

# plot muwo on x axis, cm on Y axis, geom_smooth
ggplot(data = rain_to_compare_wide, aes(x = corte_madera, y= half_moon_bay)) + geom_smooth(method = "lm") + geom_point()

# calculating correlation coefficient
model_rain = lm (rainfall ~ location, data = rain_to_compare)
summary(model_rain)

ggscatter(rain_to_compare_wide, x = "corte_madera", y = "muir_woods",
          add = "reg.line",cor.coef=TRUE,coor.method=" ",color = "orange")

summary(lm(corte_madera ~ muir_woods, data = rain_to_compare_wide))

summary(lm(half_moon_bay ~ corte_madera, data = rain_to_compare_after_missing_data))



### monthly rain comparisons ###

# making muir woods data tidy
muwo_monthly_rain <- muwo_rain %>%
  select(-TOTALS) %>% 
  pivot_longer(cols = 2:13, names_to = "month", values_to = "monthly_rain")

# TODO: combine with corte madera data

#TODO: plot timing over individual years (then in EDA, can plot this with egg timing!)