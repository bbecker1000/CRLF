library(tidyverse)
library(purrr)
library(readxl)
library(lubridate)
library(ggpubr)
library(here)

setwd(here::here("code"))

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
  totals_col <- read_xlsx(.x, sheet = sheet_name, range = cell_rows(36:37)) %>% 
    set_names(c("dayOfMonth", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "dayOfMonthAgain")) %>%
    select(-dayOfMonth, -dayOfMonthAgain) %>% 
    t()
  as.data.frame(cbind(dates_col, totals_col))
  
})

## combine list of matrices into one single dataframe
cm_rain_data <- rain_data_list %>% bind_rows()

colnames(cm_rain_data) <- c("date", "monthly_rain")
cm_rain_data <- cm_rain_data %>% mutate(date = as.Date(date), monthly_rain = as.double(monthly_rain)) %>% 
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


### ~~~ *** EDA PLOTS: COULD BE SEPARATED INTO ITS OWN FILE *** ~~~ ###

# for yearly rain
yearly_rain_cm_table <- cm_rain_data %>% 
  group_by(Water_Year) %>% 
  summarise(yearly_rain_cortemadera = sum(monthly_rain))

rain_to_compare_wide <- merge(yearly_rain_cm_table, muwo_rain, all = TRUE) %>% 
  select(Water_Year, yearly_rain_cortemadera, TOTALS) %>% 
  rename(corte_madera = yearly_rain_cortemadera, muir_woods = TOTALS) %>% 
  filter(Water_Year > 1998, Water_Year < 2022)
  
rain_to_compare <- rain_to_compare_wide %>% pivot_longer(cols = 2:3, names_to = "location", values_to = "rainfall")
  
# plot rainfall by location
ggplot(data = rain_to_compare, aes(x = Water_Year, y = rainfall, color = location)) + geom_line()

# plot muwo on x axis, cm on Y axis, geom_smooth
ggplot(data = rain_to_compare_wide, aes(x = corte_madera, y=  muir_woods)) + geom_smooth(method = "lm") + geom_point()
  
# calculate correlation coefficient between muir woods + corte madera rain -- it is 0.039 according to this (yearly rain)
correlation_coefficient <- cor(rain_to_compare$rainfall, as.numeric(factor(rain_to_compare$location)))


# monthly rain
muwo_monthly_rain <- muwo_rain %>%
  select(-TOTALS) %>% 
  pivot_longer(cols = 2:13, names_to = "month", values_to = "monthly_rain")


model_rain = lm (rainfall ~ location, data = rain_to_compare)
summary(model_rain)

ggscatter(rain_to_compare_wide, x = "corte_madera", y = "muir_woods",
          add = "reg.line",cor.coef=TRUE,coor.method=" ",color = "orange")

summary(lm(muir_woods ~ corte_madera, data = rain_to_compare_wide))
