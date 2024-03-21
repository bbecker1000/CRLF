library(tidyverse)
library(purrr)
library(readxl)
library(lubridate)
library(ggpubr)
library(here)

# TODO: make this export rain table as CSV, can load it into data prep

setwd(here::here("code"))

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
  mutate(month = format(date, "%b"))

# reading in muir woods data
muwo_rain <- read_excel(here::here("data", "muwo_rain.xlsx")) %>% 
  mutate(Oct = as.double(Oct), Nov = as.double(Nov), Dec = as.double(Dec), Jan = as.double(Jan), Feb = as.double(Feb), 
         Mar = as.double(Mar), Apr = as.double(Apr), May = as.double(May), Jun = as.double(Jun), Jul = as.double(Jul), 
         Aug = as.double(Aug), Sep = as.double(Sep))

#reading in corte madera data
marin_rain_folder <- here::here("data", "MMWD_RainfallRecords2")
rain_files <- list.files(path = marin_rain_folder, pattern = "\\.xlsx$", full.names = TRUE)
sheet_name = "CM"

## rain_data_list = list of matrices, each one = 1 year
rain_data_list <- rain_files %>% map( ~ {
  dates_col <- read_xlsx(.x, sheet = sheet_name, range = cell_rows(3:4), col_types = "date") %>% 
    set_names(c("Jul", "Aug", "Sept", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun")) %>% 
    t()
  days_table <- read_xlsx(.x, sheet = sheet_name, range = cell_rows(5:37)) %>% 
    set_names(c("dayOfMonth", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "dayOfMonthAgain")) %>%
    select(-dayOfMonth, -dayOfMonthAgain) %>% 
    t()
  as.data.frame(cbind(dates_col, days_table))
  
})

## CM: combine list of matrices into one single dataframe
cm_rain <- rain_data_list %>% bind_rows()

colnames(cm_rain) <- c("date", c(1:31), "monthly_rain")
cm_rain <- cm_rain %>% mutate(date = as.Date(date), monthly_rain = as.double(monthly_rain)) %>% 
  mutate(
    beginningOfWY = case_when(
      month(date) > 9 ~ floor_date(date, unit = "year") + months(9),
      TRUE ~ floor_date(date, unit = "year") - months(3) # gets the beginning of the water year for each date 
    )
  ) %>% 
  mutate(beginningOfWY = as.Date(beginningOfWY)) %>% 
  mutate(Water_Year = as.numeric(format(beginningOfWY, "%Y")) + 1) %>% 
  select(-beginningOfWY) %>% 
  mutate(month = format(date, "%b"))

# combine into one monthly rain table for all locations
temp_monthly_rain_table <- inner_join(hmb_rain, cm_rain, by = "date", suffix = c(".hmb", ".cm")) %>% 
  select(Water_Year.hmb, month.hmb, monthly_rain.hmb, monthly_rain.cm) %>% 
  rename(Water_Year = Water_Year.hmb, month = month.hmb, hmb_monthly_rain = monthly_rain.hmb, cm_monthly_rain = monthly_rain.cm)

full_monthly_rain_table <- inner_join(temp_monthly_rain_table, muwo_monthly_rain, by = c("Water_Year", "month")) %>% 
  rename(muwo_monthly_rain = monthly_rain)

# for yearly rain -- all locations
yearly_rain_cm_table <- cm_rain %>% 
  group_by(Water_Year) %>% 
  summarise(yearly_rain_cortemadera = sum(monthly_rain))

yearly_rain_hmb_table <- hmb_rain %>% 
  group_by(Water_Year) %>% 
  summarise(yearly_rain_halfmoonbay = sum(monthly_rain, na.rm = TRUE))

rain_to_compare_wide <- merge(merge(yearly_rain_cm_table, muwo_rain, all = TRUE), yearly_rain_hmb_table, all = TRUE) %>% 
  select(Water_Year, yearly_rain_cortemadera, TOTALS, yearly_rain_halfmoonbay) %>% 
  rename(corte_madera = yearly_rain_cortemadera, muir_woods = TOTALS, half_moon_bay = yearly_rain_halfmoonbay) %>% 
  filter(Water_Year > 1998, Water_Year < 2022)
  
rain_to_compare <- rain_to_compare_wide %>% pivot_longer(cols = 2:4, names_to = "location", values_to = "rainfall")
  
rain_to_compare_after_missing_data <- rain_to_compare_wide %>% 
  filter(Water_Year > 2003)

### ~~~ *** EDA PLOTS: COULD BE SEPARATED INTO ITS OWN FILE *** ~~~ ###

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
