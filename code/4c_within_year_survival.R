#survival analysis of first eggs detected

#install.packages(c("survival", "survminer"))

library("survival")
library("survminer")
library("mgcv")
#rename file
onset_of_breeding_surv <- onset_of_breeding


onset_of_breeding_surv$MaxD_f <- as.factor(ifelse(onset_of_breeding_surv$MaxD <0.5, "L", 
                                                  ifelse(onset_of_breeding_surv$MaxD >=0.5 & onset_of_breeding_surv$MaxD < 1, "ML",      
                                                         ifelse(onset_of_breeding_surv$MaxD >=1 & onset_of_breeding_surv$MaxD < 1.5, "MH",
                                                                "H"))))
#assign "dead" to all known breeders.  no censoring.
onset_of_breeding_surv$status <- 2 

#Generative additive model: first look at onset of breeding with fixed variables
#respectively, and plot to see is the line looks linear or curve.
fit1_k6 <- gam(first_breeding~s(rain_to_date, k = 6), data = onset_of_breeding_surv)
summary(fit1_k6)
plot(fit1_k6)
fit1_k7 <- gam(first_breeding~s(WaterTemp, k = 6), data = onset_of_breeding_surv)
summary(fit1_k7)
plot(fit1_k7)

#install this package called "gam.hp", it tells the R2 for each fixed variable.
#somehow this results shows water temperature is not significant? very low R2
library(gam.hp)
fit3_test <- gam(first_breeding ~ s(rain_to_date, k = 10) + s(WaterTemp, k = 10), data = onset_of_breeding_surv)
gam.hp(fit3_test)
gam.hp(mod=fit3_test,type="dev")
permu.gamhp(fit3_test,permutations=100)
plot(gam.hp(mod=fit3_test,type="dev"))


#the 2D and 3D plots for GAM (cumulative rain $ water temp)
fit_interaction <- gam(first_breeding ~ te(rain_to_date, WaterTemp, k = c(6, 6)), data = onset_of_breeding_surv)
vis.gam(fit_interaction, view = c("rain_to_date", "WaterTemp"), theta = 30, phi = 30, color = "topo")
vis.gam(fit_interaction, color = 'cm', plot.type = 'contour')
points(onset_of_breeding_surv$rain_to_date, onset_of_breeding_surv$WaterTemp, pch = 16)


#I tried this method using gamm() to include Watershed as random variable, and it gives
#two summary, one for fixed one for random, still trying to interpret.
library(nlme) 
fit_interaction_gamm <- gamm(
  first_breeding ~ te(rain_to_date, WaterTemp, k = c(10, 10)),
  random = list(Watershed = ~1),  # Adding random effect for watershed
  data = onset_of_breeding_surv,
  method = "REML"  # Using REML for estimating random effects
)
summary(fit_interaction_gamm$lme)
summary(fit_interaction_gamm$gam)


#gam model with 3 variables, and MaxD turns out to be insignifant, so we probably
#don't need to include that in later analysis.
fit2_test <- gam(first_breeding ~ s(rain_to_date, k = 10) + s(WaterTemp, k = 10) + s(MaxD, k = 10), data = onset_of_breeding_surv)
summary(fit2_test)
plot(fit2_test, select = 1, pch = 20, se = TRUE, rug = TRUE, residuals = TRUE)
plot(fit2_test, select = 2, pch = 20, se = TRUE, rug = TRUE, residuals = TRUE)
plot(fit2_test, select = 3, pch = 20, se = TRUE, rug = TRUE, residuals = TRUE)
vis.gam(fit2_test, view = c("rain_to_date", "WaterTemp"), theta = 30, phi = 30, color = "heat")


d.rw <- onset_of_breeding_surv %>% filter(Watershed=="Redwood Creek")

#intercept model of the mean
fit.null <- survfit(Surv(rain_to_date, status) ~ 1, data = onset_of_breeding_surv)
#survival (breeding probability) curves by watershed
fit.watershed <- survfit(Surv(rain_to_date, status) ~ Watershed + MaxD_f, data = onset_of_breeding_surv)

fit.watershed.rw <- survfit(Surv(rain_to_date, status) ~ Watershed + MaxD_f, data = d.rw)

fit.depth <- survfit(Surv(MaxD, status) ~ 1, data = onset_of_breeding_surv)
fit.watertemp <- survfit(Surv(WaterTemp, status) ~ 1, data = onset_of_breeding_surv)

# model_depth_temp <- coxph(Surv(rain_to_date, status) ~ MaxD_proportion, data = onset_of_breeding_surv)
# ggsurvplot(survfit(model_depth_temp, newdata = list(MaxD_proportion = median(onset_of_breeding_surv$MaxD_proportion))), data = onset_of_breeding_surv, risk.table = TRUE)


#pick one to inspect/plot
fit <- fit.null
fit <- fit.watershed
fit <- fit.depth
fit <- fit.watershed.rw
fit <- fit.watertemp

print(fit)
# Summary of survival curves
summary(fit)
# Access to the sort summary table
summary(fit)$table

#dataframe for whatever
d <- data.frame(time = fit$time,
                n.risk = fit$n.risk,
                n.event = fit$n.event,
                n.censor = fit$n.censor,
                surv = fit$surv,
                upper = fit$upper,
                lower = fit$lower)
d

# plot
ggsurvplot(
  fit,                     # survfit object with calculated statistics.
  pval = TRUE,             # show p-value of log-rank test.
  conf.int = FALSE,         # show confidence intervals for 
  risk.table = TRUE,       # Add risk table
  risk.table.col = "strata",
  conf.int.style = "step", #or ribbon
  xlab = "Temp",   # customize X axis label.
  ylab = "p(breeding)",
  break.time.by = 1,      # break X axis in time intervals by 200.
  ggtheme = theme_light(), # customize plot and risk table with a theme.
  surv.median.line = "hv",  # add the median survival pointer.
  fun = "event"             #flips plot to culumative probability
  
)

# cox model: univariate
MaxD.cox <- coxph(Surv(rain_to_date, status) ~ Watershed, data = onset_of_breeding_surv)
summary(MaxD.cox)

# univariate cox models for continuous variables
covariates <- c("MaxD_proportion", "AirTemp", "WaterTemp", "BRDYEAR")
univ_formulas <- sapply(covariates, function(x) as.formula(paste('Surv(rain_to_date, status) ~', x)))
univ_models <- lapply(univ_formulas, function(x){coxph(x, data = onset_of_breeding_surv)})

univ_results <- lapply(univ_models, function(x) {
  if (!is.null(x)) {
    x <- summary(x)
    p.value <- signif(x$wald["pvalue"], digits = 2)
    wald.test <- signif(x$wald["test"], digits = 2)
    beta <- signif(x$coef[1], digits = 2)
    HR <- signif(x$coef[2], digits = 2)
    HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
    HR.confint.upper <- signif(x$conf.int[,"upper .95"], 2)
    HR <- paste0(HR, " (", HR.confint.lower, "-", HR.confint.upper, ")")
    res <- c(beta, HR, wald.test, p.value)
    names(res) <- c("beta", "HR (95% CI for HR)", "wald.test", "p.value")
    return(res)
  } else {
    return(rep(NA, 4))  # Return NA for missing models
  }
})
res <- t(as.data.frame(univ_results, check.names = FALSE))
as.data.frame(res)


# to use the cox model, results of test_assumptions must not be significant, but they are for MaxD_proportion
# test_assumptions <- cox.zph(cox_model)
# test_assumptions
# 
# ggsurvplot(survfit(cox_model), color = "#2E9FDF",
#            ggtheme = theme_minimal())
