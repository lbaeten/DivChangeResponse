#========================
# Dornelas et al. analysis
#========================

library(rstan)
library(ggplot2)
library(ggmcmc)
library(gridExtra)
library(grid)
source("R/dataprep.R")
source("R/helperfunctions.R")

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Log ratios
standat_dornraw_logr <- stan_in_dorn(dorn_raw, "log_ratio")
standat_dornmodif_logr <- stan_in_dorn(dorn_modif, "log_ratio")

# Slopes
standat_dornraw_slope <- stan_in_dorn(dorn_raw, "slope")

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

# checking conversion and model fit
#----------------------------------
ggs_traceplot(ggs(stanfit_logr_dornraw_intslope) %>% filter(Parameter %in% c("a","b")))
y_fit <- apply(rstan::extract(stanfit_logr_dornraw_intslope)$"y_hat", 2, quantile, probs = .5)
plot(y_fit, standat_dornraw_logr$y); abline(a=0,b=1)

ggs_traceplot(ggs(stanfit_logr_dornraw_slope) %>% filter(Parameter == "b"))
y_fit <- apply(rstan::extract(stanfit_logr_dornraw_slope)$"y_hat", 2, quantile, probs = .5)
plot(y_fit, standat_dornraw_logr$y); abline(a=0,b=1)

ggs_traceplot(ggs(stanfit_logr_dornmodif_intslope) %>% filter(Parameter %in% c("a","b")))
y_fit <- apply(rstan::extract(stanfit_logr_dornmodif_intslope)$"y_hat", 2, quantile, probs = .5)
plot(y_fit, standat_dornmodif_logr$y); abline(a=0,b=1)

ggs_traceplot(ggs(stanfit_logr_dornmodif_slope) %>% filter(Parameter == "b"))
y_fit <- apply(rstan::extract(stanfit_logr_dornmodif_slope)$"y_hat", 2, quantile, probs = .5)
plot(y_fit, standat_dornmodif_logr$y); abline(a=0,b=1)

ggs_traceplot(ggs(stanfit_slope_dornraw_intslope) %>% filter(Parameter %in% c("a","b")))
y_fit <- apply(rstan::extract(stanfit_slope_dornraw_intslope)$"y_hat", 2, quantile, probs = .5)
plot(y_fit, standat_dornraw_slope$y); abline(a=0,b=1)

ggs_traceplot(ggs(stanfit_slope_dornraw_slope) %>% filter(Parameter == "b"))
y_fit <- apply(rstan::extract(stanfit_slope_dornraw_slope)$"y_hat", 2, quantile, probs = .5)
plot(y_fit, standat_dornraw_slope$y); abline(a=0,b=1)

# extract credible intervals
#---------------------------
stancre_logr_dornraw_intslope <- stan_extract_cre(stanfit_logr_dornraw_intslope, standat_dornraw_logr)
names(stancre_logr_dornraw_intslope) <- c("duration", "log_ratio", "plo", "phi")
stancre_logr_dornraw_slope <- stan_extract_cre(stanfit_logr_dornraw_slope, standat_dornraw_logr)
names(stancre_logr_dornraw_slope) <- c("duration", "log_ratio", "plo", "phi")

stancre_logr_dornmodif_intslope <- stan_extract_cre(stanfit_logr_dornmodif_intslope, standat_dornmodif_logr)
names(stancre_logr_dornmodif_intslope) <- c("duration", "log_ratio", "plo", "phi")
stancre_logr_dornmodif_slope <- stan_extract_cre(stanfit_logr_dornmodif_slope, standat_dornmodif_logr)
names(stancre_logr_dornmodif_slope) <- c("duration", "log_ratio", "plo", "phi")

stancre_slope_dornraw_intslope <- stan_extract_cre(stanfit_slope_dornraw_intslope, standat_dornraw_slope)
names(stancre_slope_dornraw_intslope) <- c("duration", "slope", "plo", "phi")
stancre_slope_dornraw_slope <- stan_extract_cre(stanfit_slope_dornraw_slope, standat_dornraw_slope)
names(stancre_slope_dornraw_slope) <- c("duration", "slope", "plo", "phi")


# graphs
#------------
  # Slopes
plot_slope_dornraw_intslope <- ggplot(dorn_raw, aes(x = duration, y = slope)) + 
  geom_point() +
  geom_point(aes(x = 90,y = -8.027379e-02), colour = "red", fill = "white", size = 6, shape = 21) +
  geom_point(aes(x = 90,y = -8.027379e-02), colour = "red") +
  geom_hline(aes(yintercept = 0)) +
  geom_line(data = stancre_slope_dornraw_intslope, aes(x = duration, y = slope), size = 1, colour = "blue") +
  geom_abline(slope = -0.1791, intercept = 6.7347, size = 1, linetype = 3, colour = "blue") +
  geom_ribbon(data = stancre_slope_dornraw_intslope, aes(x = duration, ymin = plo, ymax = phi), alpha = 0.3, fill = "blue") +
  xlim(0, 100) +
  ylim(-50, 250) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw(base_size=12) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  annotate(geom="text", x=50, y=240, label="a) Original dataset (slopes) - with intercept", size = 3)

plot_slope_dornraw_slope <- ggplot(dorn_raw, aes(duration, slope)) + 
  geom_point() +
  geom_point(aes(x = 90,y = -8.027379e-02), colour = "red", fill = "white", size = 6, shape = 21) +
  geom_point(aes(x = 90,y = -8.027379e-02), colour = "red") +
  geom_hline(aes(yintercept = 0)) +
  geom_line(data = stancre_slope_dornraw_slope, aes(x = duration, y = slope), size = 1, colour = "blue") +
  geom_abline(slope = 0.0277, intercept = 0, size = 1, linetype = 3, colour = "blue") +
  geom_ribbon(data = stancre_slope_dornraw_slope, aes(x = duration, ymin = plo, ymax = phi), alpha = 0.3, fill = "blue") +
  xlim(0, 100) +
  ylim(-50, 250) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw(base_size=12) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  annotate(geom="text", x=50, y=240, label="b) Original dataset (slopes) - intercept = 0", size = 3)

  # Log ratios
plot_logr_dornraw_intslope <- ggplot(dorn_raw, aes(duration, log_ratio)) + 
  geom_point() +
  geom_point(aes(x = 90,y = -3.76120012), colour = "red", fill = "white", size = 6, shape = 21) +
  geom_point(aes(x = 90,y = -3.76120012), colour = "red") +
  geom_hline(aes(yintercept = 0)) +
  geom_line(data = stancre_logr_dornraw_intslope, aes(x = duration, y = log_ratio), size = 1, colour = "blue") +
  geom_abline(slope = -0.0127, intercept = 0.2936, size = 1, linetype = 3, colour = "blue") +
  geom_ribbon(data = stancre_logr_dornraw_intslope, aes(x = duration, ymin = plo, ymax = phi), alpha = 0.3, fill = "blue") +
  xlim(0, 100) +
  ylim(-5, 5) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw(base_size=12) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  annotate(geom="text", x=50, y=4.8, label="c) Original dataset (log ratios) - with intercept", size = 3)

plot_logr_dornraw_slope <- ggplot(dorn_raw, aes(duration, log_ratio)) + 
  geom_point() +
  geom_point(aes(x = 90,y = -3.76120012), colour = "red", fill = "white", size = 6, shape = 21) +
  geom_point(aes(x = 90,y = -3.76120012), colour = "red") +
  geom_hline(aes(yintercept = 0)) +
  geom_line(data = stancre_logr_dornraw_slope, aes(x = duration, y = log_ratio), size = 1, colour = "blue") +
  geom_abline(slope = -0.0036, intercept = 0, size = 1, linetype = 3, colour = "blue") +
  geom_ribbon(data = stancre_logr_dornraw_slope, aes(x = duration, ymin = plo, ymax = phi), alpha = 0.3, fill = "blue") +
  xlim(0, 100) +
  ylim(-5, 5) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw(base_size=12) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  annotate(geom="text", x=50, y=4.8, label="d) Original dataset (log ratios) - intercept = 0", size = 3)

plot_logr_dornmodif_intslope <- ggplot(dorn_modif, aes(duration, log_ratio)) + 
  geom_point() +
  geom_point(aes(x = 86,y = -2.197224577), colour = "red", fill = "white", size = 6, shape = 21) +
  geom_point(aes(x = 86,y = -2.197224577), colour = "red") +
  geom_hline(aes(yintercept = 0)) +
  geom_line(data = stancre_logr_dornmodif_intslope, aes(x = duration, y = log_ratio), size = 1, colour = "blue") +
  geom_abline(slope = -0.0080, intercept = 0.2197, size = 1, linetype = 3, colour = "blue") +
  geom_ribbon(data = stancre_logr_dornmodif_intslope, aes(x = duration, ymin = plo, ymax = phi), alpha = 0.3, fill = "blue") +
  xlim(0, 100) +
  ylim(-5, 5) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw(base_size=12) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  annotate(geom="text", x=50, y=4.8, label="e) Modified dataset (log ratios) - with intercept", size = 3)

plot_logr_dornmodif_slope <- ggplot(dorn_modif, aes(duration, log_ratio)) + 
  geom_point() +
  geom_point(aes(x = 86,y = -2.197224577), colour = "red", fill = "white", size = 6, shape = 21) +
  geom_point(aes(x = 86,y = -2.197224577), colour = "red") +
  geom_hline(aes(yintercept = 0)) +
  geom_line(data = stancre_logr_dornmodif_slope, aes(x = duration, y = log_ratio), size = 1, colour = "blue") +
  geom_abline(slope = -0.0012, intercept = 0, size = 1, linetype = 3, colour = "blue") +
  geom_ribbon(data = stancre_logr_dornmodif_slope, aes(x = duration, ymin = plo, ymax = phi), alpha = 0.3, fill = "blue") +
  xlim(0, 100) +
  ylim(-5, 5) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw(base_size=12) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  annotate(geom="text", x=50, y=4.8, label="f) Modified dataset (log ratios) - intercept = 0", size = 3)

pdf(file="dornPlots.pdf", width = 7, height = 10.5)
grid.arrange(plot_slope_dornraw_intslope, plot_slope_dornraw_slope, 
             plot_logr_dornraw_intslope, plot_logr_dornraw_slope, 
             plot_logr_dornmodif_intslope, plot_logr_dornmodif_slope, 
             ncol=2, nrow=3, bottom = textGrob("Study duration (years)\n", gp=gpar(fontsize=12)), 
             left = textGrob(expression(paste("\nEffect size (ln(",S[2]/S[1],")                                                                              Slope")), 
                             gp=gpar(fontsize=12), rot = 90, hjust = 0.35))
dev.off()

