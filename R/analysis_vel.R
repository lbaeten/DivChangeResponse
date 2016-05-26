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
#
vel_origdataset <- vel_orig
vel_origstan_dta <- stan_in_vel(vel_origdataset)
vel_upddataset <- vel_upd
vel_updstan_dta <- stan_in_vel(vel_upddataset)
#
####################################################################

# Construct the Stan models
#--------------------------
stan_mod_rintslope <- stan_model("R/model_duration_rintslope.stan")
stan_mod_rslope <- stan_model("R/model_duration_rslope.stan")

# Sampling
#-------------------------
# Random slopes and intercepts
vel_origstan_fit_rintslope <- stan_sampling(stan_mod_rintslope, vel_origstan_dta, iter = 5000)
print(vel_origstan_fit_rintslope, pars = c("mu_a","mu_b", "sigma_a","sigma_b", "sigma"), digits = 4)

ggs_density(ggs(vel_origstan_fit_rintslope, family = "mu_b")) + geom_vline(aes(xintercept = 0)) + ggtitle("Effect size")

# Random slopes and no intercept
vel_origstan_fit_rslope <- stan_sampling(stan_mod_rslope, vel_origstan_dta, iter = 5000)
print(vel_origstan_fit_rslope, pars = c("mu_b","sigma_b", "sigma"), digits = 4)

# optional: compare with precompiled Stan model
vel_origstan_pre <- stan_lmer(log_SR_ratio ~ 1 + Duration + (1 + Duration | Study), data = vel_origdataset,
                      chains = 2, iter = 1000)
print(vel_origstan_pre, pars = "Duration", digits = 3)

# Sampling
#-------------------------
# Random slopes and intercepts
vel_updstan_fit_rintslope <- stan_sampling(stan_mod_rintslope, vel_updstan_dta, iter = 5000)
print(vel_updstan_fit_rintslope, pars = c("mu_a","mu_b", "sigma_a","sigma_b", "sigma"), digits = 4)
ggs_density(ggs(vel_updstan_fit_rintslope, family = "mu_b")) + geom_vline(aes(xintercept = 0)) + ggtitle("Effect size")

# Random slopes and no intercept
vel_updstan_fit_rslope <- stan_sampling(stan_mod_rslope, vel_updstan_dta, iter = 5000)
print(vel_updstan_fit_rslope, pars = c("mu_b","sigma_b", "sigma"), digits = 4)

# optional: compare with precompiled Stan model
vel_updstan_pre <- stan_lmer(log_SR_ratio ~ 1 + Duration + (1 + Duration | Study), data = vel_upddataset,
                      chains = 2, iter = 1000)
print(vel_updstan_pre, pars = "Duration", digits = 3)

####################################################################
# choose the model for which to draw the graphs
vel_origselect_model_rintslope <- vel_origstan_fit_rintslope
vel_origselect_model_rslope <- vel_origstan_fit_rslope
vel_updselect_model_rintslope <- vel_updstan_fit_rintslope
vel_updselect_model_rslope <- vel_updstan_fit_rslope
####################################################################

# extract credible/prediction intervals
vel_origstan_cre_rintslope <- stan_extract_cre(vel_origselect_model_rintslope, stan_dta)
names(vel_origstan_cre_rintslope) <- c("Duration", "log_SR_ratio", "plo", "phi")
vel_origstan_pre_rintslope <- stan_extract_pre(vel_origselect_model_rintslope, stan_dta)
names(vel_origstan_pre_rintslope) <- c("Duration", "log_SR_ratio", "plo", "phi")

vel_origstan_cre_rslope <- stan_extract_cre(vel_origselect_model_rslope, stan_dta)
names(vel_origstan_cre_rslope) <- c("Duration", "log_SR_ratio", "plo", "phi")
vel_origstan_pre_rslope <- stan_extract_pre(vel_origselect_model_rslope, stan_dta)
names(vel_origstan_pre_rslope) <- c("Duration", "log_SR_ratio", "plo", "phi")

vel_updstan_cre_rintslope <- stan_extract_cre(vel_updselect_model_rintslope, stan_dta)
names(vel_updstan_cre_rintslope) <- c("Duration", "log_SR_ratio", "plo", "phi")
vel_updstan_pre_rintslope <- stan_extract_pre(vel_updselect_model_rintslope, stan_dta)
names(vel_updstan_pre_rintslope) <- c("Duration", "log_SR_ratio", "plo", "phi")

vel_updstan_cre_rslope <- stan_extract_cre(vel_updselect_model_rslope, stan_dta)
names(vel_updstan_cre_rslope) <- c("Duration", "log_SR_ratio", "plo", "phi")
vel_updstan_pre_rslope <- stan_extract_pre(vel_updselect_model_rslope, stan_dta)
names(vel_updstan_pre_rslope) <- c("Duration", "log_SR_ratio", "plo", "phi")

# graphs
vel_origstan_cre_rintslopePlot <- ggplot(vel_origdataset, aes(Duration, log_SR_ratio)) + 
  geom_point() +
  geom_hline(aes(yintercept = 0)) +
  geom_line(data = vel_origstan_cre_rintslope, aes(x = Duration, y = log_SR_ratio), size = 1.2, colour = "blue") +
  geom_abline(slope = -0.00365, intercept = 0.1118, size = 1.2, linetype = 3, colour = "blue") +
  geom_ribbon(data = vel_origstan_cre_rintslope, aes(x = Duration, ymin = plo, ymax = phi), alpha = 0.3, fill = "blue") +
  xlim(0, 300) +
  ylim(-2.5, 2.5) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw(base_size=15) +
  annotate(geom="text", x=125, y=2.4, label="a) Original dataset - intercept NOT zero", size = 5)

vel_origstan_cre_rslopePlot <- ggplot(vel_origdataset, aes(Duration, log_SR_ratio)) + 
  geom_point() +
  geom_hline(aes(yintercept = 0)) +
  geom_line(data = vel_origstan_cre_rslope, aes(x = Duration, y = log_SR_ratio), size = 1.2, colour = "blue") +
  geom_abline(slope = 0.00112, intercept = 0, size = 1.2, linetype = 3, colour = "blue") +
  geom_ribbon(data = vel_origstan_cre_rslope, aes(x = Duration, ymin = plo, ymax = phi), alpha = 0.3, fill = "blue") +
  xlim(0, 300) +
  ylim(-2.5, 2.5) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw(base_size=15) +
  annotate(geom="text", x=105, y=2.4, label="b) Original dataset - intercept = 0", size = 5)

vel_updstan_cre_rintslopePlot <- ggplot(vel_upddataset, aes(Duration, log_SR_ratio)) + 
  geom_point(colour = "black", fill = "grey", shape = 21) +
  geom_point(data = vel_origdataset, aes(Duration, log_SR_ratio), colour = "black") +
  geom_hline(aes(yintercept = 0)) +
  geom_line(data = vel_updstan_cre_rintslope, aes(x = Duration, y = log_SR_ratio), size = 1.2, colour = "blue") +
  geom_abline(slope = -0.00255, intercept = 0.0935, size = 1.2, linetype = 3, colour = "blue") +
  geom_ribbon(data = vel_updstan_cre_rintslope, aes(x = Duration, ymin = plo, ymax = phi), alpha = 0.3, fill = "blue") +
  xlim(0, 300) +
  ylim(-2.5, 2.5) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw(base_size=15) +
  annotate(geom="text", x=125, y=2.4, label="c) Updated dataset - intercept NOT zero", size = 5)

vel_updstan_cre_rslopePlot <- ggplot(vel_upddataset, aes(Duration, log_SR_ratio)) + 
  geom_point(colour = "black", fill = "grey", shape = 21) +
  geom_point(data = vel_origdataset, aes(Duration, log_SR_ratio), colour = "black") +
  geom_hline(aes(yintercept = 0)) +
  geom_line(data = vel_updstan_cre_rslope, aes(x = Duration, y = log_SR_ratio), size = 1.2, colour = "blue") +
  geom_abline(slope = 0.00135, intercept = 0, size = 1.2, linetype = 3, colour = "blue") +
  geom_ribbon(data = vel_updstan_cre_rslope, aes(x = Duration, ymin = plo, ymax = phi), alpha = 0.3, fill = "blue") +
  xlim(0, 300) +
  ylim(-2.5, 2.5) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw(base_size=15) +
  annotate(geom="text", x=105, y=2.4, label="d) Updated dataset - intercept = 0", size = 5)

library(gridExtra)
library(grid)
png(file="velPlots.png", width=800, height=800)
grid.arrange(vel_origstan_cre_rintslopePlot, vel_origstan_cre_rslopePlot, vel_updstan_cre_rintslopePlot, vel_updstan_cre_rslopePlot, ncol=2, nrow=2, bottom = textGrob("Study Duration (years)", gp=gpar(fontsize=20)), left = textGrob(expression(paste("Effect size (ln(",S[2]/S[1],"))")), gp=gpar(fontsize=20), rot = 90))
dev.off()
