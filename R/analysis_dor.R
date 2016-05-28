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
dorn_modifdataset <- dorn_modif
# Log ratios
dorn_rawstan_dta <- stan_in_dorn(dorn_rawdataset, "log_ratio", "duration_lr")
dorn_modifstan_dta <- stan_in_dorn(dorn_modifdataset, "log_ratio", "duration_lr")
# Slopes
slopedorn_rawstan_dta <- stan_in_dorn(dorn_rawdataset, "slope", "duration_slope")
slopedorn_modifstan_dta <- stan_in_dorn(dorn_modifdataset, "slope", "duration_slope")
#
####################################################################

# Construct the Stan models
#--------------------------
stan_mod_intslope <- stan_model("R/model_duration_intslope.stan")
stan_mod_slope <- stan_model("R/model_duration_slope.stan")

# Sampling dorn_raw
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

# Sampling dorn_modif
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

# Sampling slope dorn_raw
#-------------------------
# linear regression (intercept and slope)
slopedorn_rawstan_fit_intslope <- stan_sampling(stan_mod_intslope, slopedorn_rawstan_dta, iter = 5000)
print(slopedorn_rawstan_fit_intslope, pars = c("a","b","sigma"), digits = 4)

ggs_density(ggs(dorn_rawstan_fit_intslope, family = "b")) + 
  geom_vline(aes(xintercept = 0)) + 
  ggtitle("Effect size")

# linear regression (only slope)
slopedorn_rawstan_fit_slope <- stan_sampling(stan_mod_slope, slopedorn_rawstan_dta, iter = 5000)
print(slopedorn_rawstan_fit_slope, pars = c("b", "sigma"), digits = 4)

# Sampling slope dorn_modif
#-------------------------
# linear regression (intercept and slope)
slopedorn_modifstan_fit_intslope <- stan_sampling(stan_mod_intslope, slopedorn_modifstan_dta, iter = 5000)
print(slopedorn_modifstan_fit_intslope, pars = c("a","b","sigma"), digits = 4)

ggs_density(ggs(slopedorn_modifstan_fit_intslope, family = "b")) + 
  geom_vline(aes(xintercept = 0)) + 
  ggtitle("Effect size")

# linear regression (only slope)
slopedorn_modifstan_fit_slope <- stan_sampling(stan_mod_slope, slopedorn_modifstan_dta, iter = 5000)
print(slopedorn_modifstan_fit_slope, pars = c("b", "sigma"), digits = 4)

####################################################################
dorn_rawselect_model_intslope <- dorn_rawstan_fit_intslope
dorn_rawselect_model_slope <- dorn_rawstan_fit_slope
dorn_modifselect_model_intslope <- dorn_modifstan_fit_intslope
dorn_modifselect_model_slope <- dorn_modifstan_fit_slope
slopedorn_rawselect_model_intslope <- slopedorn_rawstan_fit_intslope
slopedorn_rawselect_model_slope <- slopedorn_rawstan_fit_slope
slopedorn_modifselect_model_intslope <- slopedorn_modifstan_fit_intslope
slopedorn_modifselect_model_slope <- slopedorn_modifstan_fit_slope
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

slopedorn_rawstan_cre_intslope <- stan_extract_cre(slopedorn_rawselect_model_intslope, slopedorn_rawstan_dta)
names(slopedorn_rawstan_cre_intslope) <- c("duration_slope", "slope", "plo", "phi")
slopedorn_rawstan_cre_slope <- stan_extract_cre(slopedorn_rawselect_model_slope, slopedorn_rawstan_dta)
names(slopedorn_rawstan_cre_slope) <- c("duration_slope", "slope", "plo", "phi")

slopedorn_rawstan_pre_intslope <- stan_extract_pre(slopedorn_rawselect_model_intslope, slopedorn_rawstan_dta)
names(slopedorn_rawstan_pre_intslope) <- c("duration_slope", "slope", "plo", "phi")
slopedorn_rawstan_pre_slope <- stan_extract_pre(slopedorn_rawselect_model_slope, slopedorn_rawstan_dta)
names(slopedorn_rawstan_pre_slope) <- c("duration_slope", "slope", "plo", "phi")

slopedorn_modifstan_cre_intslope <- stan_extract_cre(slopedorn_modifselect_model_intslope, slopedorn_modifstan_dta)
names(slopedorn_modifstan_cre_intslope) <- c("duration_slope", "slope", "plo", "phi")
slopedorn_modifstan_cre_slope <- stan_extract_cre(slopedorn_modifselect_model_slope, slopedorn_modifstan_dta)
names(slopedorn_modifstan_cre_slope) <- c("duration_slope", "slope", "plo", "phi")

slopedorn_modifstan_pre_intslope <- stan_extract_pre(slopedorn_modifselect_model_intslope, slopedorn_modifstan_dta)
names(slopedorn_modifstan_pre_intslope) <- c("duration_slope", "slope", "plo", "phi")
slopedorn_modifstan_pre_slope <- stan_extract_pre(slopedorn_modifselect_model_slope, slopedorn_modifstan_dta)
names(slopedorn_modifstan_pre_slope) <- c("duration_slope", "slope", "plo", "phi")

# graphs
# Slopes
slopedorn_rawstan_cre_intslopePlot <- ggplot(dorn_rawdataset, aes(x = duration_slope, y = slope)) + 
  geom_point() +
  geom_point(aes(x = 129,y = -8.027379e-02), colour = "red", fill = "white", size = 6, shape = 21) +
  geom_point(aes(x = 129,y = -8.027379e-02), colour = "red") +
  geom_hline(aes(yintercept = 0)) +
  geom_line(data = slopedorn_rawstan_cre_intslope, aes(x = duration_slope, y = slope), size = 1, colour = "blue") +
  geom_abline(slope = -0.096, intercept = 4.1570, size = 1, linetype = 3, colour = "blue") +
  geom_ribbon(data = slopedorn_rawstan_cre_intslope, aes(x = duration_slope, ymin = plo, ymax = phi), alpha = 0.3, fill = "blue") +
  xlim(0, 150) +
  ylim(-50, 250) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw(base_size=12) +
  annotate(geom="text", x=70, y=240, label="a) Original dataset (slopes) - with intercept", size = 3)

slopedorn_modifstan_cre_slopePlot <- ggplot(dorn_rawdataset, aes(duration_slope, slope)) + 
  geom_point() +
  geom_point(aes(x = 129,y = -8.027379e-02), colour = "red", fill = "white", size = 6, shape = 21) +
  geom_point(aes(x = 129,y = -8.027379e-02), colour = "red") +
  geom_hline(aes(yintercept = 0)) +
  geom_line(data = slopedorn_modifstan_cre_slope, aes(x = duration_slope, y = slope), size = 1, colour = "blue") +
  geom_abline(slope = 0.021, intercept = 0, size = 1, linetype = 3, colour = "blue") +
  geom_ribbon(data = slopedorn_modifstan_cre_slope, aes(x = duration_slope, ymin = plo, ymax = phi), alpha = 0.3, fill = "blue") +
  xlim(0, 150) +
  ylim(-50, 250) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw(base_size=12) +
  annotate(geom="text", x=70, y=240, label="b) Original dataset (slopes) - intercept = 0", size = 3)

# Log ratios
dorn_rawstan_cre_intslopePlot <- ggplot(dorn_rawdataset, aes(duration_lr, log_ratio)) + 
  geom_point() +
  geom_point(aes(x = 90,y = -3.76120012), colour = "red", fill = "white", size = 6, shape = 21) +
  geom_point(aes(x = 90,y = -3.76120012), colour = "red") +
  geom_hline(aes(yintercept = 0)) +
  geom_line(data = dorn_rawstan_cre_intslope, aes(x = duration_lr, y = log_ratio), size = 1, colour = "blue") +
  geom_abline(slope = -0.0127, intercept = 0.2936, size = 1, linetype = 3, colour = "blue") +
  geom_ribbon(data = dorn_rawstan_cre_intslope, aes(x = duration_lr, ymin = plo, ymax = phi), alpha = 0.3, fill = "blue") +
  xlim(0, 100) +
  ylim(-5, 5) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw(base_size=12) +
  annotate(geom="text", x=50, y=4.8, label="c) Original dataset (log ratios) - with intercept", size = 3)

dorn_rawstan_cre_slopePlot <- ggplot(dorn_rawdataset, aes(duration_lr, log_ratio)) + 
  geom_point() +
  geom_point(aes(x = 90,y = -3.76120012), colour = "red", fill = "white", size = 6, shape = 21) +
  geom_point(aes(x = 90,y = -3.76120012), colour = "red") +
  geom_hline(aes(yintercept = 0)) +
  geom_line(data = stan_cre_slope, aes(x = duration_lr, y = log_ratio), size = 1, colour = "blue") +
  geom_abline(slope = -0.0036, intercept = 0, size = 1, linetype = 3, colour = "blue") +
  geom_ribbon(data = stan_cre_slope, aes(x = duration_lr, ymin = plo, ymax = phi), alpha = 0.3, fill = "blue") +
  xlim(0, 100) +
  ylim(-5, 5) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw(base_size=12) +
  annotate(geom="text", x=50, y=4.8, label="d) Original dataset (log ratios) - intercept = 0", size = 3)

dorn_modifstan_cre_intslopePlot <- ggplot(dorn_modifdataset, aes(duration_lr, log_ratio)) + 
  geom_point() +
  geom_point(aes(x = 86,y = -2.197224577), colour = "red", fill = "white", size = 6, shape = 21) +
  geom_point(aes(x = 86,y = -2.197224577), colour = "red") +
  geom_hline(aes(yintercept = 0)) +
  geom_line(data = dorn_modifstan_cre_intslope, aes(x = duration_lr, y = log_ratio), size = 1, colour = "blue") +
  geom_abline(slope = -0.0080, intercept = 0.2197, size = 1, linetype = 3, colour = "blue") +
  geom_ribbon(data = dorn_modifstan_cre_intslope, aes(x = duration_lr, ymin = plo, ymax = phi), alpha = 0.3, fill = "blue") +
  xlim(0, 100) +
  ylim(-5, 5) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw(base_size=12) +
  annotate(geom="text", x=50, y=4.8, label="e) Modified dataset (log ratios) - with intercept", size = 3)

dorn_modifstan_cre_slopePlot <- ggplot(dorn_modifdataset, aes(duration_lr, log_ratio)) + 
  geom_point() +
  geom_point(aes(x = 86,y = -2.197224577), colour = "red", fill = "white", size = 6, shape = 21) +
  geom_point(aes(x = 86,y = -2.197224577), colour = "red") +
  geom_hline(aes(yintercept = 0)) +
  geom_line(data = dorn_modifstan_cre_slope, aes(x = duration_lr, y = log_ratio), size = 1, colour = "blue") +
  geom_abline(slope = -0.0012, intercept = 0, size = 1, linetype = 3, colour = "blue") +
  geom_ribbon(data = dorn_modifstan_cre_slope, aes(x = duration_lr, ymin = plo, ymax = phi), alpha = 0.3, fill = "blue") +
  xlim(0, 100) +
  ylim(-5, 5) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw(base_size=12) +
  annotate(geom="text", x=50, y=4.8, label="f) Modified dataset (log ratios) - intercept = 0", size = 3)

library(gridExtra)
library(grid)
pdf(file="figures/dornPlots.pdf", width = 7, height = 10.5)
grid.arrange(slopedorn_rawstan_cre_intslopePlot, slopedorn_modifstan_cre_slopePlot, dorn_rawstan_cre_intslopePlot, dorn_rawstan_cre_slopePlot, dorn_modifstan_cre_intslopePlot, dorn_modifstan_cre_slopePlot, ncol=2, nrow=3, bottom = textGrob("Study Duration (years)\n", gp=gpar(fontsize=12)), left = textGrob(expression(paste("\nEffect size (ln(",S[2]/S[1],"))                                                                           Slope")), gp=gpar(fontsize=12), rot = 90, hjust = 0.35))
dev.off()

