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
#
dorn_rawdataset <- dorn_raw
dorn_rawstan_dta <- stan_in_dorn(dorn_rawdataset, "log_ratio", "duration_lr")
dorn_modifdataset <- dorn_modif
dorn_modifstan_dta <- stan_in_dorn(dorn_modifdataset, "log_ratio", "duration_lr")
#
####################################################################

# Construct the Stan models
#--------------------------
stan_mod_intslope <- stan_model("R/model_duration_intslope.stan")
stan_mod_slope <- stan_model("R/model_duration_slope.stan")

# Sampling
#-------------------------
# linear regression (intercept and slope)
dorn_rawstan_fit_intslope <- stan_sampling(stan_mod_intslope, dorn_rawstan_dta, iter = 5000)
print(dorn_rawstan_fit_intslope, pars = c("a","b","sigma"), digits = 4)

ggs_density(ggs(dorn_rawstan_fit_intslope, family = "b")) + 
  geom_vline(aes(xintercept = 0)) + 
  ggtitle("Effect size")

# linear regression (only slope)
dorn_rawstan_fit_slope <- stan_sampling(stan_mod_slope, dorn_rawstan_dta, iter = 5000)
print(dorn_rawstan_fit_slope, pars = c("b", "sigma"), digits = 4)

# Sampling
#-------------------------
# linear regression (intercept and slope)
dorn_modifstan_fit_intslope <- stan_sampling(stan_mod_intslope, dorn_modifstan_dta, iter = 5000)
print(dorn_modifstan_fit_intslope, pars = c("a","b","sigma"), digits = 4)

ggs_density(ggs(dorn_modifstan_fit_intslope, family = "b")) + 
  geom_vline(aes(xintercept = 0)) + 
  ggtitle("Effect size")

# linear regression (only slope)
dorn_modifstan_fit_slope <- stan_sampling(stan_mod_slope, dorn_modifstan_dta, iter = 5000)
print(dorn_modifstan_fit_slope, pars = c("b", "sigma"), digits = 4)

####################################################################
dorn_rawselect_model_intslope <- dorn_rawstan_fit_intslope
dorn_rawselect_model_slope <- dorn_rawstan_fit_slope
dorn_modifselect_model_intslope <- dorn_modifstan_fit_intslope
dorn_modifselect_model_slope <- dorn_modifstan_fit_slope
####################################################################

# extract credible/prediction intervals
dorn_rawstan_cre_intslope <- stan_extract_cre(dorn_rawselect_model_intslope, dorn_rawstan_dta)
names(dorn_rawstan_cre_intslope) <- c("duration_lr", "log_ratio", "plo", "phi")
dorn_rawstan_cre_slope <- stan_extract_cre(dorn_rawselect_model_slope, dorn_rawstan_dta)
names(dorn_rawstan_cre_slope) <- c("duration_lr", "log_ratio", "plo", "phi")

dorn_rawstan_pre_intslope <- stan_extract_pre(dorn_rawselect_model_intslope, dorn_rawstan_dta)
names(dorn_rawstan_pre_intslope) <- c("duration_lr", "log_ratio", "plo", "phi")
dorn_rawstan_pre_slope <- stan_extract_pre(dorn_rawselect_model_slope, dorn_rawstan_dta)
names(dorn_rawstan_pre_slope) <- c("duration_lr", "log_ratio", "plo", "phi")

dorn_modifstan_cre_intslope <- stan_extract_cre(dorn_modifselect_model_intslope, dorn_modifstan_dta)
names(dorn_modifstan_cre_intslope) <- c("duration_lr", "log_ratio", "plo", "phi")
dorn_modifstan_cre_slope <- stan_extract_cre(dorn_modifselect_model_slope, dorn_modifstan_dta)
names(dorn_modifstan_cre_slope) <- c("duration_lr", "log_ratio", "plo", "phi")

dorn_modifstan_pre_intslope <- stan_extract_pre(dorn_modifselect_model_intslope, dorn_modifstan_dta)
names(dorn_modifstan_pre_intslope) <- c("duration_lr", "log_ratio", "plo", "phi")
dorn_modifstan_pre_slope <- stan_extract_pre(dorn_modifselect_model_slope, dorn_modifstan_dta)
names(dorn_modifstan_pre_slope) <- c("duration_lr", "log_ratio", "plo", "phi")

# graphs
  # //x and y variable need to be changed for log_ratio or slope analysis
dorn_rawstan_cre_intslopePlot <- ggplot(dorn_rawdataset, aes(duration_lr, log_ratio)) + 
  geom_point() +
  geom_point(aes(x = 90,y = -3.76120012), colour = "red", fill = "white", size = 6, shape = 21) +
  geom_point(aes(x = 90,y = -3.76120012), colour = "red") +
  geom_hline(aes(yintercept = 0)) +
  geom_line(data = dorn_rawstan_cre_intslope, aes(x = duration_lr, y = log_ratio), size = 1.2, colour = "blue") +
  geom_abline(slope = -0.0127, intercept = 0.2936, size = 1.2, linetype = 3, colour = "blue") +
  geom_ribbon(data = dorn_rawstan_cre_intslope, aes(x = duration_lr, ymin = plo, ymax = phi), alpha = 0.3, fill = "blue") +
  xlim(0, 100) +
  ylim(-5, 5) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw(base_size=15) +
  annotate(geom="text", x=40, y=4.8, label="a) Original dataset - with intercept", size = 5)

dorn_rawstan_cre_slopePlot <- ggplot(dorn_rawdataset, aes(duration_lr, log_ratio)) + 
  geom_point() +
  geom_point(aes(x = 90,y = -3.76120012), colour = "red", fill = "white", size = 6, shape = 21) +
  geom_point(aes(x = 90,y = -3.76120012), colour = "red") +
  geom_hline(aes(yintercept = 0)) +
  geom_line(data = stan_cre_slope, aes(x = duration_lr, y = log_ratio), size = 1.2, colour = "blue") +
  geom_abline(slope = -0.0036, intercept = 0, size = 1.2, linetype = 3, colour = "blue") +
  geom_ribbon(data = stan_cre_slope, aes(x = duration_lr, ymin = plo, ymax = phi), alpha = 0.3, fill = "blue") +
  xlim(0, 100) +
  ylim(-5, 5) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw(base_size=15) +
  annotate(geom="text", x=35, y=4.8, label="b) Original dataset - intercept = 0", size = 5)

dorn_modifstan_cre_intslopePlot <- ggplot(dorn_modifdataset, aes(duration_lr, log_ratio)) + 
  geom_point() +
  geom_point(aes(x = 86,y = -2.197224577), colour = "red", fill = "white", size = 6, shape = 21) +
  geom_point(aes(x = 86,y = -2.197224577), colour = "red") +
  geom_hline(aes(yintercept = 0)) +
  geom_line(data = dorn_modifstan_cre_intslope, aes(x = duration_lr, y = log_ratio), size = 1.2, colour = "blue") +
  geom_abline(slope = -0.0080, intercept = 0.2197, size = 1.2, linetype = 3, colour = "blue") +
  geom_ribbon(data = dorn_modifstan_cre_intslope, aes(x = duration_lr, ymin = plo, ymax = phi), alpha = 0.3, fill = "blue") +
  xlim(0, 100) +
  ylim(-5, 5) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw(base_size=15) +
  annotate(geom="text", x=40, y=4.8, label="c) Modified dataset - with intercept", size = 5)

dorn_modifstan_cre_slopePlot <- ggplot(dorn_modifdataset, aes(duration_lr, log_ratio)) + 
  geom_point() +
  geom_point(aes(x = 86,y = -2.197224577), colour = "red", fill = "white", size = 6, shape = 21) +
  geom_point(aes(x = 86,y = -2.197224577), colour = "red") +
  geom_hline(aes(yintercept = 0)) +
  geom_line(data = dorn_modifstan_cre_slope, aes(x = duration_lr, y = log_ratio), size = 1.2, colour = "blue") +
  geom_abline(slope = -0.0012, intercept = 0, size = 1.2, linetype = 3, colour = "blue") +
  geom_ribbon(data = dorn_modifstan_cre_slope, aes(x = duration_lr, ymin = plo, ymax = phi), alpha = 0.3, fill = "blue") +
  xlim(0, 100) +
  ylim(-5, 5) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw(base_size=15) +
  annotate(geom="text", x=35, y=4.8, label="d) Modified dataset - intercept = 0", size = 5)

library(gridExtra)
library(grid)
png(file="dornPlots.png", width=800, height=800)
grid.arrange(vel_origstan_cre_rintslopePlot, vel_origstan_cre_rslopePlot, vel_updstan_cre_rintslopePlot, vel_updstan_cre_rslopePlot, ncol=2, nrow=2, bottom = textGrob("Study Duration (years)", gp=gpar(fontsize=20)), left = textGrob(expression(paste("Effect size (ln(",S[2]/S[1],"))")), gp=gpar(fontsize=20), rot = 90))
dev.off()

