#================================================
# Vellend et al. analysis: all versus impact only
#================================================

library(rstan)
library(ggmcmc)
library(ggplot2)

source("R/dataprep.R")
source("R/helperfunctions.R")

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

standat_velorig_Iraw <- stan_in_vel(vel_orig_I, lrr = "raw")
standat_velorig_Idecade <- stan_in_vel(vel_orig_I, lrr = "decade")
standat_velupd_Iraw <- stan_in_vel(vel_upd_I, lrr = "raw")
standat_velupd_Idecade <- stan_in_vel(vel_upd_I, lrr = "decade")

# Construct the Stan models
#--------------------------
stanmod_rint <- stan_model("R/model_rint.stan")

# Sampling
#-------------------------
# Original data
# raw log ratio
stanfit_velorig_Iraw <- stan_sampling(stanmod_rint, standat_velorig_Iraw)
print(stanfit_velorig_Iraw, pars = c("mu_a","sigma_a","sigma"), digits = 4)

# log ratio per decade
stanfit_velorig_Idecade <- stan_sampling(stanmod_rint, standat_velorig_Idecade)
print(stanfit_velorig_Idecade, pars = c("mu_a","sigma_a","sigma"), digits = 4)

# Updated data
# raw log ratio
stanfit_velupd_Iraw <- stan_sampling(stanmod_rint, standat_velupd_Iraw)
print(stanfit_velupd_Iraw, pars = c("mu_a","sigma_a","sigma"), digits = 4)

# log ratio per decade
stanfit_velupd_Idecade <- stan_sampling(stanmod_rint, standat_velupd_Idecade)
print(stanfit_velupd_Idecade, pars = c("mu_a","sigma_a","sigma"), digits = 4)

# checking conversion and model fit
#---------------------------------
ggs_traceplot(ggs(stanfit_velorig_Iraw, family = "mu"))
ggs_traceplot(ggs(stanfit_velorig_Idecade, family = "mu"))
ggs_traceplot(ggs(stanfit_velupd_Iraw, family = "mu"))
ggs_traceplot(ggs(stanfit_velupd_Idecade, family = "mu"))
