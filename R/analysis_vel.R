#========================
# Vellend et al. analysis
#========================

library(rstan)
library(ggmcmc)
library(rstanarm)
source("R/dataprep.R")
source("R/helperfunctions.R")

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan_dta <- stan_in(dta_SR)   # here we can switch between Vellend 1.0 and 2.0

# Construct the Stan models
#--------------------------
stan_mod <- stan_model("R/model_duration.stan")
stan_mod_randslope <- stan_model("R/model_duration_randslope.stan")

# Sampling
#-------------------------
# Random intercepts
stan_fit <- stan_sampling(stan_mod, stan_dta, iter = 5000)
print(stan_fit, pars = c("mu_b","beta", "sigma_b", "sigma"), digits = 4)

# Random slopes and intercepts
stan_fit_randslope <- stan_sampling(stan_mod_randslope, stan_dta, iter = 5000)
print(stan_fit_randslope, pars = c("mu_a","mu_b", "sigma_a","sigma_b", "sigma"), digits = 4)

ggs_density(ggs(stan_fit_randslope, family = "mu_b")) + 
  geom_vline(aes(xintercept = 0)) + 
  ggtitle("Effect size")

# compare with precompiled Stan model
stan_pre <- stan_lmer(log_SR_ratio ~ 1 + Duration + (1 + Duration | Study), data = dta_duration,
                      chains = 2, iter = 1000)
print(stan_pre, pars = "Duration", digits = 3)

# Predictions and graph
#--------------------------
stan_pred <- stan_extract(stan_fit_randslope)

ggplot(dta_SR, aes(Duration, log_SR_ratio)) + 
  geom_point() +
  geom_hline(aes(yintercept = 0)) +
  geom_line(data = stan_pred, aes(x = new_x, y = new_y), size = 1.2) +
  geom_line(data = stan_pred, aes(x = new_x, y = plo), linetype = 2, size = 1.2, col = "blue", alpha = .5) +
  geom_line(data = stan_pred, aes(x = new_x, y = phi), linetype = 2, size = 1.2, col = "blue", alpha = .5) +
  xlab("\nStudy Duration (years)") +
  ylab("Effect Size: ln(S2/S1)\n") +
  theme_bw(base_size=17) +
  annotate(geom="text", x=0.25, y=1.35, label="a)") +
  xlim(c(0,275))
