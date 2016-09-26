#================================================
# Vellend et al. analysis: all versus impact only
#================================================

library(nlme)
library(rstan)
library(ggmcmc)
library(ggplot2)

source("R/dataprep.R")
source("R/helperfunctions.R")

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

standat_velorig <- stan_in_vel(vel_orig)
standat_velorig_I <- stan_in_vel(vel_orig_I)
standat_velupd <- stan_in_vel(vel_upd)
standat_velupd_I <- stan_in_vel(vel_upd_I)

# Construct the Stan models
#--------------------------
stanmod_rint <- stan_model("R/model_rint.stan")

# Sampling
#-------------------------
  # Original data
    # impact + recovery
stanfit_velorig <- stan_sampling(stanmod_rint, standat_velorig)

print(stanfit_velorig, pars = c("mu_a","sigma_a","sigma"), digits = 4)
(lme(log_SR_ratio ~ 1, random =~ 1 | Study, vel_orig))

    # impact only
stanfit_velorig_I <- stan_sampling(stanmod_rint, standat_velorig_I)

print(stanfit_velorig_I, pars = c("mu_a","sigma_a","sigma"), digits = 4)
(lme(log_SR_ratio ~ 1, random =~ 1 | Study, vel_orig_I))

  # Updated data
    # impact + recovery
stanfit_velupd <- stan_sampling(stanmod_rint, standat_velupd)

print(stanfit_velupd, pars = c("mu_a","sigma_a","sigma"), digits = 4)
(lme(log_SR_ratio ~ 1, random =~ 1 | Study, vel_upd))

    # impact only
stanfit_velupd_I <- stan_sampling(stanmod_rint, standat_velupd_I)

print(stanfit_velupd_I, pars = c("mu_a","sigma_a","sigma"), digits = 4)
(lme(log_SR_ratio ~ 1, random =~ 1 | Study, vel_upd_I))

# checking conversion and model fit
#---------------------------------
ggs_traceplot(ggs(stanfit_velorig, family = "mu"))
ggs_traceplot(ggs(stanfit_velorig_I, family = "mu"))
ggs_traceplot(ggs(stanfit_velupd, family = "mu"))
ggs_traceplot(ggs(stanfit_velupd_I, family = "mu"))

# graph
#----------------------------
extractquant <- function(infile)quantile(rstan::extract(infile)$"mu_a", probs = c(.025, .5, .975))

impact_result <- data.frame(expand.grid(subset = c("Impact", "Impact + recovery"), version = c("original","update")),
                     rbind(extractquant(stanfit_velorig_I),
                           extractquant(stanfit_velorig),
                           extractquant(stanfit_velupd_I),
                           extractquant(stanfit_velupd)))

ggplot(impact_result, aes(subset, X50., col = version)) +
  geom_point(position = position_dodge(width = .2)) +
  geom_linerange(aes(ymin = X2.5., ymax = X97.5.), position = position_dodge(width = .2)) +
  scale_x_discrete("") +
  scale_y_continuous(name = expression(paste("Effect size (ln(",S[2]/S[1],").",decade^-1,")")), limits = c(-.1, .1)) +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_blank()) +
  coord_flip()
