#========================
# Gonzalez et al. analysis
#========================

library(lme4)
library(lmerTest)
library(ggplot2)
source("R/dataprep.R")

# Dornelas data
#---------------

# Linear regression model
gonz_fit_dor <- lm(log_ratio ~ duration_lr, data = dorn_raw)
summary(gonz_fit_dor)

# Predictions
pred <- expand.grid(duration_lr=c(seq(from=min(dorn_raw$duration_lr),to=max(dorn_raw$duration_lr),by=1)),log_ratio=0)
matrix <- model.matrix(terms(gonz_fit_dor),data=pred)           #sets up the model matrix
pred$log_ratio <- matrix %*% coef(gonz_fit_dor)                #this calculates the predictions
pvar <- diag(matrix %*% tcrossprod(vcov(gonz_fit_dor),matrix))  #this adds the variance
pred <- data.frame(pred, plo=pred$log_ratio - 2*sqrt(pvar), phi=pred$log_ratio+2*sqrt(pvar))

# Plot the results
gonzPlot_dorn <- qplot(duration_lr, log_ratio, data=dorn_raw, size=I(2)) +
  geom_abline(intercept=0, slope=0, 
              lwd=1, color="black") +
  geom_abline(intercept=coef(gonz_fit_dor)[1], slope=coef(gonz_fit_dor)[2], 
              lwd=1.5, color="blue") +
  geom_ribbon(data = pred, aes(x = duration_lr, ymin = plo, ymax = phi), alpha = 0.3, fill = "blue") +
  xlab("\nStudy Duration (years)") +
  ylab("Effect Size: ln(S2/S1)\n") +
  theme_bw(base_size=17) +
  annotate(geom="text", x=0.25, y=1.35, label="a)") +
  xlim(c(0,90))+
  ggtitle("Dornelas et al.")
gonzPlot_dorn


# Vellend data
#----------------

#Linear mixed model with random slopes
gonz_fit_vel <- lmer(log_SR_ratio ~ Duration + (1+Duration|Study), data=vel_orig, REML=T)
summary(gonz_fit_vel)

# Predictions
pred <- expand.grid(Duration=c(seq(from=min(vel_orig$Duration),to=max(vel_orig$Duration),by=1)),log_SR_ratio=0)
matrix <- model.matrix(terms(gonz_fit_vel),data=pred)           #sets up the model matrix
pred$log_SR_ratio <- matrix %*% fixef(gonz_fit_vel)             #this calculates the predictions
pvar <- diag(matrix %*% tcrossprod(vcov(gonz_fit_vel),matrix))  #this adds the variance
pred <- data.frame(pred, plo=pred$log_SR_ratio - 2*sqrt(pvar), phi=pred$log_SR_ratio+2*sqrt(pvar))

# Plot the results
gonzPlot_vel <- qplot(Duration, log_SR_ratio, data=vel_orig, size=I(2)) +
  geom_abline(intercept=0, slope=0, 
              lwd=1, color="black") +
  geom_abline(intercept=fixef(gonz_fit_vel)[1], slope=fixef(gonz_fit_vel)[2], 
              lwd=1.5, color="blue") +
  geom_ribbon(data = pred, aes(x = Duration, ymin = plo, ymax = phi), alpha = 0.3, fill = "blue") +
  xlab("\nStudy Duration (years)") +
  ylab("Effect Size: ln(S2/S1)\n") +
  theme_bw(base_size=17) +
  annotate(geom="text", x=0.25, y=1.35, label="a)") +
  xlim(c(0,275))+
  ggtitle("Vellend et al.")
gonzPlot_vel

