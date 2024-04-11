library(here)
library(sjPlot)
library(lme4)
library(glmmTMB) #instead of lme4
library(lme4)
library(cowplot)

source("1_data_prep.R")

# onset_of_breeding is the relevant table here

# I am getting weird errors when I try to use lme4 or glmmTMB... not sure if I'm doing something wrong here?
# stack overflow says the glmmTMB error could also be because of the Matrix error...

within_year_model <- glmmTMB(first_breeding ~ rain_to_date + MaxD + (1|LocationID), family = gaussian, data = onset_of_breeding)
within_year_model <- lmer(first_breeding ~ rain_to_date + MaxD + (1|LocationID), data = onset_of_breeding)

summary(within_year_model)

plot_model(within_year_model, type = "diag")


