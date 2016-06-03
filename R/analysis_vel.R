#========================
# Vellend et al. analysis
#========================

library(rstan)
library(ggplot2)
library(gridExtra)
library(grid)
source("R/dataprep.R")
source("R/helperfunctions.R")

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

standat_velorig <- stan_in_vel(vel_orig)
standat_velupd <- stan_in_vel(vel_upd)

# Construct the Stan models
#--------------------------
stanmod_rintslope <- stan_model("R/model_duration_rintslope.stan")
stanmod_rslope <- stan_model("R/model_duration_rslope.stan")

# Sampling
#-------------------------
  # Original data
    # random intercepts + slopes model
stanfit_velorig_rintslope <- stan_sampling(stanmod_rintslope, standat_velorig)
print(stanfit_velorig_rintslope, pars = c("mu_a","mu_b", "sigma_a","sigma_b", "sigma"), digits = 4)
    # random slopes model
stanfit_velorig_rslope <- stan_sampling(stanmod_rslope, standat_velorig)
print(stanfit_velorig_rslope, pars = c("mu_b","sigma_b", "sigma"), digits = 4)

  # Updated data
    # random intercepts + slopes model
stanfit_velupd_rintslope <- stan_sampling(stanmod_rintslope, standat_velupd)
print(stanfit_velupd_rintslope, pars = c("mu_a","mu_b", "sigma_a","sigma_b", "sigma"), digits = 4)
    # random slopes model
stanfit_velupd_rslope <- stan_sampling(stanmod_rslope, standat_velupd)
print(stanfit_velupd_rslope, pars = c("mu_b","sigma_b", "sigma"), digits = 4)

# extract credible intervals
#---------------------------
stancre_velorig_rintslope <- stan_extract_cre(stanfit_velorig_rintslope, standat_velorig)
names(stancre_velorig_rintslope) <- c("Duration", "log_SR_ratio", "plo", "phi")

stancre_velorig_rslope <- stan_extract_cre(stanfit_velorig_rslope, standat_velorig)
names(stancre_velorig_rslope) <- c("Duration", "log_SR_ratio", "plo", "phi")

stancre_velupd_rintslope <- stan_extract_cre(stanfit_velupd_rintslope, standat_velupd)
names(stancre_velupd_rintslope) <- c("Duration", "log_SR_ratio", "plo", "phi")

stancre_velupd_rslope <- stan_extract_cre(stanfit_velupd_rslope, standat_velupd)
names(stancre_velupd_rslope) <- c("Duration", "log_SR_ratio", "plo", "phi")

# graphs
#-----------
plot_velorig_rintslope <- ggplot(vel_orig, aes(Duration, log_SR_ratio)) + 
  geom_point() +
  geom_hline(aes(yintercept = 0)) +
  geom_line(data = stancre_velorig_rintslope, aes(x = Duration, y = log_SR_ratio), size = 1, colour = "blue") +
  geom_abline(slope = -0.00365, intercept = 0.1118, size = 1, linetype = 3, colour = "blue") +
  geom_ribbon(data = stancre_velorig_rintslope, aes(x = Duration, ymin = plo, ymax = phi), alpha = 0.3, fill = "blue") +
  xlim(0, 300) +
  ylim(-2.5, 2.5) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw(base_size=12) +  annotate(geom="text", x=140, y=2.4, label="a) Original dataset - with intercept", size = 4)

plot_velorig_rslope <- ggplot(vel_orig, aes(Duration, log_SR_ratio)) + 
  geom_point() +
  geom_hline(aes(yintercept = 0)) +
  geom_line(data = stancre_velorig_rslope, aes(x = Duration, y = log_SR_ratio), size = 1, colour = "blue") +
  geom_abline(slope = 0.00112, intercept = 0, size = 1, linetype = 3, colour = "blue") +
  geom_ribbon(data = stancre_velorig_rslope, aes(x = Duration, ymin = plo, ymax = phi), alpha = 0.3, fill = "blue") +
  xlim(0, 300) +
  ylim(-2.5, 2.5) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw(base_size=12) +
  annotate(geom="text", x=140, y=2.4, label="b) Original dataset - intercept = 0", size = 4)

plot_velupd_rintslope <- ggplot(vel_upd, aes(Duration, log_SR_ratio)) + 
  geom_point(colour = "red") +
  geom_point(data = vel_orig, aes(Duration, log_SR_ratio), colour = "black") +
  geom_hline(aes(yintercept = 0)) +
  geom_line(data = stancre_velupd_rintslope, aes(x = Duration, y = log_SR_ratio), size = 1, colour = "blue") +
  geom_abline(slope = -0.00255, intercept = 0.0935, size = 1, linetype = 3, colour = "blue") +
  geom_ribbon(data = stancre_velupd_rintslope, aes(x = Duration, ymin = plo, ymax = phi), alpha = 0.3, fill = "blue") +
  xlim(0, 300) +
  ylim(-2.5, 2.5) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw(base_size=12) +
  annotate(geom="text", x=140, y=2.4, label="c) Updated dataset - with intercept", size = 4)

plot_velupd_rslope <- ggplot(vel_upd, aes(Duration, log_SR_ratio)) + 
  scale_colour_manual(name="Dataset", values=c("Vellend et al. 2013"="black", "Vellend et al. updated"="red")) +
  geom_point(aes(colour = "Vellend et al. updated")) +
  geom_point(data = vel_orig, aes(Duration, log_SR_ratio, colour = "Vellend et al. 2013")) +
  geom_hline(aes(yintercept = 0)) +
  geom_line(data = stancre_velupd_rslope, aes(x = Duration, y = log_SR_ratio), size = 1, colour = "blue") +
  geom_abline(slope = 0.00135, intercept = 0, size = 1, linetype = 3, colour = "blue") +
  geom_ribbon(data = stancre_velupd_rslope, aes(x = Duration, ymin = plo, ymax = phi), alpha = 0.3, fill = "blue") +
  xlim(0, 300) +
  ylim(-2.5, 2.5) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw(base_size=12) +
  theme(legend.justification=c(1,0), legend.position=c(1,0), legend.key = element_blank(),legend.text=element_text(size=12),legend.title=element_text(size=12)) +
  annotate(geom="text", x=140, y=2.4, label="d) Updated dataset - intercept = 0", size = 4)


grid.arrange(plot_velorig_rintslope, plot_velorig_rslope, 
             plot_velupd_rintslope, plot_velupd_rslope, 
             ncol=2, nrow=2, bottom = textGrob("Study Duration (years)\n", gp=gpar(fontsize=12)), 
             left = textGrob(expression(paste("\nEffect size (ln(",S[2]/S[1],"))")), gp=gpar(fontsize=12), rot = 90))
