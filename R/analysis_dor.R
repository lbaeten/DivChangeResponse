#========================
# Dornelas et al. analysis
#========================

library(rstan)
library(ggmcmc)
library(rstanarm)
source("R/dataprep.R")
source("R/helperfunctions.R")

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


####################################################################
# here you can switch between the original Dornelas data (dorn_raw) 
# or the data where study 147 was modified (dorn_modif)
dataset <- dorn_raw
#dataset <- dorn_modif
#
# choose the response variable and corresponding duration variable
stan_dta <- stan_in_dorn(dataset, "log_ratio", "duration_lr")
#
####################################################################


# Construct the Stan models
#--------------------------
stan_mod_intslope <- stan_model("R/model_duration_intslope.stan")
stan_mod_slope <- stan_model("R/model_duration_slope.stan")

# Sampling
#-------------------------
# linear regression (intercept and slope)
stan_fit_intslope <- stan_sampling(stan_mod_intslope, stan_dta, iter = 5000)
print(stan_fit_intslope, pars = c("a","b","sigma"), digits = 4)

ggs_density(ggs(stan_fit_intslope, family = "b")) + 
  geom_vline(aes(xintercept = 0)) + 
  ggtitle("Effect size")

# linear regression (only slope)
stan_fit_slope <- stan_sampling(stan_mod_slope, stan_dta, iter = 5000)
print(stan_fit_slope, pars = c("b", "sigma"), digits = 4)


####################################################################
# choose the model for which to draw the graphs
select_model <- stan_fit_intslope
#select_model <- stan_fit_slope
####################################################################


# extract credible/prediction intervals
stan_cre <- stan_extract_cre(select_model, stan_dta)
stan_pre <- stan_extract_pre(select_model, stan_dta)

# graphs
  # //x and y variable need to be changed for log_ratio or slope analysis
ggplot(dataset, aes(duration_slope, slope)) + 
  geom_point() +
  geom_hline(aes(yintercept = 0)) +
  geom_line(data = stan_cre, aes(x = new_x, y = new_y), size = 1.2) +
  geom_line(data = stan_cre, aes(x = new_x, y = plo), linetype = 2, size = 1.2, col = "blue", alpha = .5) +
  geom_line(data = stan_cre, aes(x = new_x, y = phi), linetype = 2, size = 1.2, col = "blue", alpha = .5) +
  xlab("\nStudy Duration (years)") +
  ylab("Effect Size: ln(S2/S1)\n") +
  theme_bw(base_size=17)

ggplot(dataset, aes(duration_lr, log_ratio)) + 
  geom_point() +
  geom_hline(aes(yintercept = 0)) +
  geom_line(data = stan_pre, aes(x = new_x, y = new_y), size = 1.2) +
  geom_line(data = stan_pre, aes(x = new_x, y = plo), linetype = 2, size = 1.2, col = "blue", alpha = .5) +
  geom_line(data = stan_pre, aes(x = new_x, y = phi), linetype = 2, size = 1.2, col = "blue", alpha = .5) +
  xlab("\nStudy Duration (years)") +
  ylab("Effect Size: ln(S2/S1)\n") +
  theme_bw(base_size=17)

