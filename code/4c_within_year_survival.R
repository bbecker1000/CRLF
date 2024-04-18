#survival analysis of first eggs detected

#install.packages(c("survival", "survminer"))

library("survival")
library("survminer")

#rename file
onset_of_breeding_surv <- onset_of_breeding
#assign "dead" to all known breeders.  no censoring.
onset_of_breeding_surv$status <- 2 
#intercept model of the mean
fit.null <- survfit(Surv(rain_to_date, status) ~ 1, data = onset_of_breeding_surv)
#survival (breeding probability) curves by watershed
fit.watershed <- survfit(Surv(rain_to_date, status) ~ Watershed, data = onset_of_breeding_surv)

#pick one to inspect/plot
fit <- fit.null
fit <- fit.watershed

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
  conf.int = TRUE,         # show confidence intervals for 
  risk.table = TRUE,       # Add risk table
  risk.table.col = "strata",
  conf.int.style = "step", #or ribbon
  xlab = "Culumative rainfall",   # customize X axis label.
  ylab = "p(breeding)",
  break.time.by = 5,      # break X axis in time intervals by 200.
  ggtheme = theme_light(), # customize plot and risk table with a theme.
  surv.median.line = "hv",  # add the median survival pointer.
  fun = "event"             #flips plot to culumative probability
  
)





