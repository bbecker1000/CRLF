# rainfall calculations that are helpful for EDA but not part of data prep or analysis
# main goal is to chow high correlations between corte madera data and muir woods/half moon bay data

library(tidyverse)
library(dplyr)
library(here)
library(lubridate)
library(readxl)

setwd(here::here("code"))

# reading in corte madera data
cm_rain<- read_csv(here::here("data", "cm_yearly_rain.csv"))

# reading in half moon bay data
hmb_rain <- read_csv(here::here("data", "hmb_rain.csv")) %>% 
  select("DATE", "PRCP") %>% 
  mutate(DATE = trimws(DATE), DATE = ym(DATE)) %>% 
  rename(monthly_rain = PRCP, date = DATE) %>% 
  mutate(
    beginningWY = case_when(
      month(date) > 9 ~ floor_date(date, unit = "year") + months(9),
      TRUE ~ floor_date(date, unit = "year") - months(3) # gets the beginning of the water year for each date 
    ),
    Water_Year = as.numeric(year(beginningWY)) + 1) %>% 
  select(-beginningWY) %>% 
  mutate(month = format(date, "%b")) %>% 
  group_by(Water_Year) %>% 
  summarise(yearly_rain = sum(monthly_rain))

# reading in muir woods data
muwo_rain <- read_excel(here::here("data", "muwo_rain.xlsx")) %>% 
  mutate(Oct = as.double(Oct), Nov = as.double(Nov), Dec = as.double(Dec), Jan = as.double(Jan), Feb = as.double(Feb), 
         Mar = as.double(Mar), Apr = as.double(Apr), May = as.double(May), Jun = as.double(Jun), Jul = as.double(Jul), 
         Aug = as.double(Aug), Sep = as.double(Sep), yearly_rain = TOTALS) %>% 
  select(Water_Year, yearly_rain)

# for yearly rain -- all locations

rain_to_compare_wide <- cm_rain %>% 
  inner_join(muwo_rain, by = c("date", "Water_Year"), suffix = c("_cm", "_muwo")) %>% 
  left_join(hmb_rain, by = c("date", "Water_Year"), suffix = c("", "_hmb"))

# rain_to_compare_wide <- merge(merge(cm_rain, muwo_rain, all = TRUE), hmb_rain, all = TRUE) %>% 
#   select(Water_Year, yearly_rain, TOTALS, yearly_rain) %>% 
#   rename(corte_madera = yearly_rain_cortemadera, muir_woods = TOTALS, half_moon_bay = yearly_rain_halfmoonbay) %>% 
#   filter(Water_Year > 1998, Water_Year < 2022)

rain_to_compare <- rain_to_compare_wide %>% pivot_longer(cols = 2:4, names_to = "location", values_to = "rainfall")

rain_to_compare_after_missing_data <- rain_to_compare_wide %>% 
  filter(Water_Year > 2003)

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