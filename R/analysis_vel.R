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


####################################################################
# here you can switch between the Vellend 1.0 data (vel_orig) 
# or Vellend 2.0 data (vel_upd)
dataset <- vel_orig
#dataset <- vel_upd
#
stan_dta <- stan_in_vel(dataset)
#
####################################################################


# Construct the Stan models
#--------------------------
stan_mod_rintslope <- stan_model("R/model_duration_rintslope.stan")
stan_mod_rslope <- stan_model("R/model_duration_rslope.stan")

# Sampling
#-------------------------
# Random slopes and intercepts
stan_fit_rintslope <- stan_sampling(stan_mod_rintslope, stan_dta, iter = 5000)
print(stan_fit_rintslope, pars = c("mu_a","mu_b", "sigma_a","sigma_b", "sigma"), digits = 4)

ggs_density(ggs(stan_fit_rintslope, family = "mu_b")) + 
  geom_vline(aes(xintercept = 0)) + 
  ggtitle("Effect size")

# Random slopes and no intercept
stan_fit_rslope <- stan_sampling(stan_mod_rslope, stan_dta, iter = 5000)
print(stan_fit_rslope, pars = c("mu_b","sigma_b", "sigma"), digits = 4)

# optional: compare with precompiled Stan model
stan_pre <- stan_lmer(log_SR_ratio ~ 1 + Duration + (1 + Duration | Study), data = dataset,
                      chains = 2, iter = 1000)
print(stan_pre, pars = "Duration", digits = 3)


####################################################################
# choose the model for which to draw the graphs
select_model <- stan_fit_rintslope
#select_model <- stan_fit_rslope
####################################################################


# extract credible/prediction intervals
stan_cre <- stan_extract_cre(select_model, stan_dta)
stan_pre <- stan_extract_pre(select_model, stan_dta)

# graphs
ggplot(dataset, aes(Duration, log_SR_ratio)) + 
  geom_point() +
  geom_hline(aes(yintercept = 0)) +
  geom_line(data = stan_cre, aes(x = new_x, y = new_y), size = 1.2) +
  geom_line(data = stan_cre, aes(x = new_x, y = plo), linetype = 2, size = 1.2, col = "blue", alpha = .5) +
  geom_line(data = stan_cre, aes(x = new_x, y = phi), linetype = 2, size = 1.2, col = "blue", alpha = .5) +
  xlab("\nStudy Duration (years)") +
  ylab("Effect Size: ln(S2/S1)\n") +
  theme_bw(base_size=17) +
  annotate(geom="text", x=0.25, y=1.35, label="a)") +
  xlim(c(0,275))

ggplot(dataset, aes(Duration, log_SR_ratio)) + 
  geom_point() +
  geom_hline(aes(yintercept = 0)) +
  geom_line(data = stan_pre, aes(x = new_x, y = new_y), size = 1.2) +
  geom_line(data = stan_pre, aes(x = new_x, y = plo), linetype = 2, size = 1.2, col = "blue", alpha = .5) +
  geom_line(data = stan_pre, aes(x = new_x, y = phi), linetype = 2, size = 1.2, col = "blue", alpha = .5) +
  xlab("\nStudy Duration (years)") +
  ylab("Effect Size: ln(S2/S1)\n") +
  theme_bw(base_size=17) +
  annotate(geom="text", x=0.25, y=1.35, label="a)") +
  xlim(c(0,275))

