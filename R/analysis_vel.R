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

stan_mod <- stan_model("R/model_duration.stan")

# Original data
#==============

stan_dta <- stan_in(dta_duration)
stan_fit <- stan_sampling(stan_mod, stan_dta, iter = 5000)

print(stan_fit, pars = c("mu_b","beta", "sigma_b", "sigma"), digits = 3)

ggs_density(ggs(stan_fit, family = "mu_b")) + geom_vline(aes(xintercept = 0)) + ggtitle("Effect size")
ggs_density(ggs(stan_fit, family = "beta")) + geom_vline(aes(xintercept = 0)) + ggtitle("Slope for duration effect")

# compare with precompiled Stan model
pre_stan <- stan_lmer(log_SR_ratio ~ mn_duration + (1 | Study), data = dta_duration,
                      chains = 2, iter = 1000)
print(pre_stan, pars = "duration", digits = 3)
