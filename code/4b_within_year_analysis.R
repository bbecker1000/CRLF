library(here)
library(sjPlot)
library(lme4)
library(glmmTMB) #instead of lme4
library(cowplot)

source("1_data_prep.R")

# onset_of_breeding is the relevant table here

within_year_model <- glmmTMB(first_breeding ~ rain_to_date + (1|LocationID), family = Gamma, data = onset_of_breeding)

summary(within_year_model)

plot_model(within_year_model, type = "diag")
