# Cover Data Regressions

library(sjPlot)
library(lme4) #glmer won't run today...having issue with matrix as of 2024-04-09
library(glmmTMB) #instead of lme4
library(cowplot)

setwd(here::here("code"))
source("1a_cover_data_prep.R")
# from 1a_cover_data_prep.R

# sites with >2 time points
cover_multi_date <- c("LS01", "RC07", "RC10", "RL02", "TV02")

# make df
cover_multi_df <- cover_data %>%
  filter(LocationID %in% cover_multi_date) 
    
# check filter
unique(cover_multi_df$LocationID)

# binomial TMB for cover
m.cover <- glmmTMB(cbind(OpenWater_percent, 100) ~ year_numeric + (1|LocationID), 
                 family = binomial, 
                 data = cover_multi_df)
summary(m.cover)

plot_model(m.cover, type = "re")
plot_model(m.cover, type = "diag")

p.model <- plot_model(m.cover, type = "pred",
           axis.lim = c(0,1),
           show.data=TRUE) + #wrong scale from raw input data...won't plot
  theme_gray(base_size = 18) +
  theme(legend.position = c(0.8, 0.8)) +
  ylab(" ") +
  xlab("Year") 
p.model

#plot raw data
p.raw <- ggplot(cover_multi_df, aes(year_numeric, OpenWater_percent, color = LocationID)) +
  geom_point(size = 4) +
 # geom_smooth(method = "lm", level = 0.8, alpha = 0.2) +
  geom_smooth(method = "loess", span = 6, level = 0.8, alpha = 0.2, size = 2) +
  ylim(0,100) +
  theme_gray(base_size = 18) +
  theme(legend.position = c(0.8, 0.8)) +
  ylab("Percent open water") +
  xlab("Year")
p.raw 

#plot Model and raw data side by side
plot_grid(p.raw, p.model)

#report glmmTMB results and linear interpolation reasonable.

