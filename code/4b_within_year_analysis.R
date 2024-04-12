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
within_year_model <- glmmTMB(first_breeding ~ rain_to_date + (1|LocationID),family = Gamma, data = onset_of_breeding)
within_year_model_lmer <- lmer(first_breeding ~ rain_to_date  + (1|LocationID), data = onset_of_breeding)
within_year_model_lmer <- lmer(rain_to_date ~ (1|LocationID), data = onset_of_breeding)

summary(within_year_model)
summary(within_year_model_lmer)

plot_model(within_year_model, type = "diag")
plot_model(within_year_model_lmer, type = "diag")

cor.test(onset_of_breeding$rain_to_date,onset_of_breeding$MaxD)


p.raw <- ggplot(onset_of_breeding, aes(x = first_breeding, y = rain_to_date, color = Watershed)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE)
p.raw
