#tidybayes
library(magrittr)
library(dplyr)
library(purrr)
library(forcats)
library(tidyr)
library(modelr)
library(tidybayes)
library(tidybayes.rethinking)
library(ggplot2)
library(cowplot)
library(rstan)
library(rethinking)
library(ggrepel)
library(RColorBrewer)
#library(gganimate)

theme_set(theme_tidybayes() + panel_border())

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

set.seed(5)
n = 10
n_condition = 5
ABC =
  tibble(
    condition = factor(rep(c("A","B","C","D","E"), n)),
    response = rnorm(n * 5, c(0,1,2,1,-1), 0.5)
  )

head(ABC, 10)


ABC %>%
  ggplot(aes(y = fct_rev(condition), x = response)) +
  geom_point()


m = ulam(alist(
  response ~ normal(mu, sigma),
  
  # submodel for conditional mean
  mu <- intercept[condition],
  intercept[condition] ~ normal(mu_condition, tau_condition),
  mu_condition ~ normal(0, 5),
  tau_condition ~ exponential(1),
  
  # submodel for conditional standard deviation
  log(sigma) <- sigma_intercept[condition],
  sigma_intercept[condition] ~ normal(0, 1)
), 
data = ABC,
chains = 4,
cores = parallel::detectCores(),
iter = 2000,
cmdstan = FALSE
)


summary(m)


str(rethinking::extract.samples(m))

m<-m1.ulam
m %>%
  spread_draws(a_Location[Location]) %>%
  head(10)


m %>%
  recover_types(dat) %>%
  spread_draws(a_Location[Location]) %>%
  head(10)


m %<>% recover_types(dat)


m %>%
  spread_draws(beta_Year, beta_Year_2, beta_CoastalSite, beta_WaterTemp, beta_Rain)



dat %>%
  #group_by(cyl) %>%
  #data_grid(hp = seq_range(hp, n = 101)) %>%
  add_linpred_draws(m, ndraws = 100) %>%
  ggplot(aes(x = hp, y = mpg, color = cyl)) +
  geom_line(aes(y = .linpred, group = paste(cyl, .draw)), alpha = .1) +
  geom_point(data = mtcars_clean) +
  scale_color_brewer(palette = "Dark2")

plot(m, depth=2)




dat %>%
  #group_by(cyl) %>%
  #data_grid(hp = seq_range(hp, n = 101)) %>%
  add_predicted_draws(m) %>%
  ggplot(aes(x = hp, y = mpg)) +
  stat_lineribbon(aes(y = .prediction), .width = c(.95, .8, .5), color = brewer.pal(5, "Blues")[[5]]) +
  geom_point(data = mtcars_clean) +
  scale_fill_brewer() +
  facet_grid(. ~ cyl, space = "free_x", scales = "free_x")
