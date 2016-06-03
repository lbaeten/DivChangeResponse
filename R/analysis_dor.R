#========================
# Dornelas et al. analysis
#========================

library(rstan)
library(ggplot2)
library(gridExtra)
library(grid)
source("R/dataprep.R")
source("R/helperfunctions.R")

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Log ratios
standat_dornraw_logr <- stan_in_dorn(dorn_raw, "log_ratio", "duration_lr")
standat_dornmodif_logr <- stan_in_dorn(dorn_modif, "log_ratio", "duration_lr")

# Slopes
standat_dornraw_slope <- stan_in_dorn(dorn_raw, "slope", "duration_slope")
standat_dornmodif_slope <- stan_in_dorn(dorn_modif, "slope", "duration_slope")

# Construct the Stan models
#--------------------------
stanmod_intslope <- stan_model("R/model_duration_intslope.stan")
stanmod_slope <- stan_model("R/model_duration_slope.stan")

# Sampling
#-------------------------
  # effect size: log ratio 
    # raw data
      # intercept + slope model
stanfit_logr_dornraw_intslope <- stan_sampling(stanmod_intslope, standat_dornraw_logr)
print(stanfit_logr_dornraw_intslope, pars = c("a","b", "sigma"), digits = 4)
      # slope model
stanfit_logr_dornraw_slope <- stan_sampling(stanmod_slope, standat_dornraw_logr)
print(stanfit_logr_dornraw_slope, pars = c("b", "sigma"), digits = 4)

    # modified data
      # intercept + slope model
stanfit_logr_dornmodif_intslope <- stan_sampling(stanmod_intslope, standat_dornmodif_logr)
print(stanfit_logr_dornmodif_intslope, pars = c("a","b", "sigma"), digits = 4)
      # slope model
stanfit_logr_dornmodif_slope <- stan_sampling(stanmod_slope, standat_dornmodif_logr)
print(stanfit_logr_dornmodif_slope, pars = c("b", "sigma"), digits = 4)

  # effect size: slope
      # intercept + slope model
stanfit_slope_dornraw_intslope <- stan_sampling(stanmod_intslope, standat_dornraw_slope)
print(stanfit_slope_dornraw_intslope, pars = c("a","b","sigma"), digits = 4)
      # slope model
stanfit_slope_dornraw_slope <- stan_sampling(stanmod_slope, standat_dornraw_slope)
print(stanfit_slope_dornraw_slope, pars = c("b", "sigma"), digits = 4)

# extract credible intervals
#---------------------------
stancre_logr_dornraw_intslope <- stan_extract_cre(stanfit_logr_dornraw_intslope, standat_dornraw_logr)
names(stancre_logr_dornraw_intslope) <- c("duration_lr", "log_ratio", "plo", "phi")
stancre_logr_dornraw_slope <- stan_extract_cre(stanfit_logr_dornraw_slope, standat_dornraw_logr)
names(stancre_logr_dornraw_slope) <- c("duration_lr", "log_ratio", "plo", "phi")

stancre_logr_dornmodif_intslope <- stan_extract_cre(stanfit_logr_dornmodif_intslope, standat_dornmodif_logr)
names(stancre_logr_dornmodif_intslope) <- c("duration_lr", "log_ratio", "plo", "phi")
stancre_logr_dornmodif_slope <- stan_extract_cre(stanfit_logr_dornmodif_slope, standat_dornmodif_logr)
names(stancre_logr_dornmodif_slope) <- c("duration_lr", "log_ratio", "plo", "phi")

stancre_slope_dornraw_intslope <- stan_extract_cre(stanfit_slope_dornraw_intslope, standat_dornraw_slope)
names(stancre_slope_dornraw_intslope) <- c("duration_slope", "slope", "plo", "phi")
stancre_slope_dornraw_slope <- stan_extract_cre(stanfit_slope_dornraw_slope, standat_dornraw_slope)
names(stancre_slope_dornraw_slope) <- c("duration_slope", "slope", "plo", "phi")


# graphs
#------------
  # Slopes
plot_slope_dornraw_intslope <- ggplot(dorn_raw, aes(x = duration_slope, y = slope)) + 
  geom_point() +
  geom_point(aes(x = 129,y = -8.027379e-02), colour = "red", fill = "white", size = 6, shape = 21) +
  geom_point(aes(x = 129,y = -8.027379e-02), colour = "red") +
  geom_hline(aes(yintercept = 0)) +
  geom_line(data = stancre_slope_dornraw_intslope, aes(x = duration_slope, y = slope), size = 1, colour = "blue") +
  geom_abline(slope = -0.096, intercept = 4.1570, size = 1, linetype = 3, colour = "blue") +
  geom_ribbon(data = stancre_slope_dornraw_intslope, aes(x = duration_slope, ymin = plo, ymax = phi), alpha = 0.3, fill = "blue") +
  xlim(0, 150) +
  ylim(-50, 250) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw(base_size=12) +
  annotate(geom="text", x=70, y=240, label="a) Original dataset (slopes) - with intercept", size = 3)

plot_slope_dornraw_slope <- ggplot(dorn_raw, aes(duration_slope, slope)) + 
  geom_point() +
  geom_point(aes(x = 129,y = -8.027379e-02), colour = "red", fill = "white", size = 6, shape = 21) +
  geom_point(aes(x = 129,y = -8.027379e-02), colour = "red") +
  geom_hline(aes(yintercept = 0)) +
  geom_line(data = stancre_slope_dornraw_slope, aes(x = duration_slope, y = slope), size = 1, colour = "blue") +
  geom_abline(slope = 0.021, intercept = 0, size = 1, linetype = 3, colour = "blue") +
  geom_ribbon(data = stancre_slope_dornraw_slope, aes(x = duration_slope, ymin = plo, ymax = phi), alpha = 0.3, fill = "blue") +
  xlim(0, 150) +
  ylim(-50, 250) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw(base_size=12) +
  annotate(geom="text", x=70, y=240, label="b) Original dataset (slopes) - intercept = 0", size = 3)

  # Log ratios
plot_logr_dornraw_intslope <- ggplot(dorn_raw, aes(duration_lr, log_ratio)) + 
  geom_point() +
  geom_point(aes(x = 90,y = -3.76120012), colour = "red", fill = "white", size = 6, shape = 21) +
  geom_point(aes(x = 90,y = -3.76120012), colour = "red") +
  geom_hline(aes(yintercept = 0)) +
  geom_line(data = stancre_logr_dornraw_intslope, aes(x = duration_lr, y = log_ratio), size = 1, colour = "blue") +
  geom_abline(slope = -0.0127, intercept = 0.2936, size = 1, linetype = 3, colour = "blue") +
  geom_ribbon(data = stancre_logr_dornraw_intslope, aes(x = duration_lr, ymin = plo, ymax = phi), alpha = 0.3, fill = "blue") +
  xlim(0, 100) +
  ylim(-5, 5) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw(base_size=12) +
  annotate(geom="text", x=50, y=4.8, label="c) Original dataset (log ratios) - with intercept", size = 3)

plot_logr_dornraw_slope <- ggplot(dorn_raw, aes(duration_lr, log_ratio)) + 
  geom_point() +
  geom_point(aes(x = 90,y = -3.76120012), colour = "red", fill = "white", size = 6, shape = 21) +
  geom_point(aes(x = 90,y = -3.76120012), colour = "red") +
  geom_hline(aes(yintercept = 0)) +
  geom_line(data = stancre_logr_dornraw_slope, aes(x = duration_lr, y = log_ratio), size = 1, colour = "blue") +
  geom_abline(slope = -0.0036, intercept = 0, size = 1, linetype = 3, colour = "blue") +
  geom_ribbon(data = stancre_logr_dornraw_slope, aes(x = duration_lr, ymin = plo, ymax = phi), alpha = 0.3, fill = "blue") +
  xlim(0, 100) +
  ylim(-5, 5) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw(base_size=12) +
  annotate(geom="text", x=50, y=4.8, label="d) Original dataset (log ratios) - intercept = 0", size = 3)

plot_logr_dornmodif_intslope <- ggplot(dorn_modif, aes(duration_lr, log_ratio)) + 
  geom_point() +
  geom_point(aes(x = 86,y = -2.197224577), colour = "red", fill = "white", size = 6, shape = 21) +
  geom_point(aes(x = 86,y = -2.197224577), colour = "red") +
  geom_hline(aes(yintercept = 0)) +
  geom_line(data = stancre_logr_dornmodif_intslope, aes(x = duration_lr, y = log_ratio), size = 1, colour = "blue") +
  geom_abline(slope = -0.0080, intercept = 0.2197, size = 1, linetype = 3, colour = "blue") +
  geom_ribbon(data = stancre_logr_dornmodif_intslope, aes(x = duration_lr, ymin = plo, ymax = phi), alpha = 0.3, fill = "blue") +
  xlim(0, 100) +
  ylim(-5, 5) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw(base_size=12) +
  annotate(geom="text", x=50, y=4.8, label="e) Modified dataset (log ratios) - with intercept", size = 3)

plot_logr_dornmodif_slope <- ggplot(dorn_modif, aes(duration_lr, log_ratio)) + 
  geom_point() +
  geom_point(aes(x = 86,y = -2.197224577), colour = "red", fill = "white", size = 6, shape = 21) +
  geom_point(aes(x = 86,y = -2.197224577), colour = "red") +
  geom_hline(aes(yintercept = 0)) +
  geom_line(data = stancre_logr_dornmodif_slope, aes(x = duration_lr, y = log_ratio), size = 1, colour = "blue") +
  geom_abline(slope = -0.0012, intercept = 0, size = 1, linetype = 3, colour = "blue") +
  geom_ribbon(data = stancre_logr_dornmodif_slope, aes(x = duration_lr, ymin = plo, ymax = phi), alpha = 0.3, fill = "blue") +
  xlim(0, 100) +
  ylim(-5, 5) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw(base_size=12) +
  annotate(geom="text", x=50, y=4.8, label="f) Modified dataset (log ratios) - intercept = 0", size = 3)

grid.arrange(plot_slope_dornraw_intslope, plot_slope_dornraw_slope, 
             plot_logr_dornraw_intslope, plot_logr_dornraw_slope, 
             plot_logr_dornmodif_intslope, plot_logr_dornmodif_slope, 
             ncol=2, nrow=3, bottom = textGrob("Study Duration (years)\n", gp=gpar(fontsize=12)), 
             left = textGrob(expression(paste("\nEffect size (ln(",S[2]/S[1],"))                                                                           Slope")), gp=gpar(fontsize=12), rot = 90, hjust = 0.35))

