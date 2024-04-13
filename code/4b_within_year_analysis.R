library(here)
library(sjPlot)
library(lme4)
library(glmmTMB) #instead of lme4
library(lme4)
library(cowplot)
library(lmerTest)

source("1_data_prep.R")

# onset_of_breeding is the relevant table here

# the internet said Gamma is the correct family to use for timing variables, but I'm not attached to it if anyone has other ideas
within_year_model <- glmmTMB(first_breeding ~ rain_to_date + (1|LocationID), family = Gamma, data = onset_of_breeding)
within_year_model_lmer <- lmer(first_breeding ~ rain_to_date  + (1|LocationID), data = onset_of_breeding)
within_year_model_lmer <- lmer(rain_to_date ~ (1|LocationID), data = onset_of_breeding)

summary(within_year_model)
summary(within_year_model_lmer)

plot_model(within_year_model, type = "diag")
plot_model(within_year_model_lmer, type = "diag")

cor.test(onset_of_breeding$rain_to_date,onset_of_breeding$MaxD)


p.raw <- ggplot(onset_of_breeding, aes(x = first_breeding, y = rain_to_date, color = LocationID)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE)
p.raw



#means of rainfall at first breed

ggplot(onset_of_breeding, aes(rain_to_date, fill = LocationID)) +
  geom_histogram() #+
  #geom_point() + 
  #geom_vline(xintercept = mean(rain_to_date)) +
  #facet_wrap(.~Watershed)

ggplot(onset_of_breeding, aes(first_breeding, rain_to_date, color = Watershed)) +
  geom_point() +
  geom_smooth(method = "loess", span = 2, se = FALSE)
#geom_vline(xintercept = mean(rain_to_date)) +
#facet_wrap(.~Watershed)




m.rain_date <- lmer(rain_to_date ~ (1|Watershed/LocationID), data = onset_of_breeding)

plot_model(m.rain_date, type = "re")
plot_model(m.rain_date, type = "diag")



