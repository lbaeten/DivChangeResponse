#========================
# Gonzalez et al. analysis
#========================

library(lme4)
library(lmerTest)
library(ggplot2)
source("R/dataprep.R")

# Linear mixed model with random slopes
gonz_fit <- lmer(log_SR_ratio ~ Duration + (1+Duration|Study), data=vel_orig, REML=T)
summary(gonz_fit)

# Predictions
pred <- expand.grid(Duration=c(seq(from=min(vel_orig$Duration),to=max(vel_orig$Duration),by=1)),log_SR_ratio=0)
matrix <- model.matrix(terms(gonz_fit),data=pred)           #sets up the model matrix
pred$log_SR_ratio <- matrix %*% fixef(gonz_fit)             #this calculates the predictions
pvar <- diag(matrix %*% tcrossprod(vcov(gonz_fit),matrix))  #this adds the variance
pred <- data.frame(pred, plo=pred$log_SR_ratio - 2*sqrt(pvar), phi=pred$log_SR_ratio+2*sqrt(pvar))

# Plot the results
gonzPlot <- qplot(Duration, log_SR_ratio, data=vel_orig, size=I(2)) +
  geom_abline(intercept=0, slope=0, 
              lwd=1, color="black") +
  geom_abline(intercept=fixef(gonz_fit)[1], slope=fixef(gonz_fit)[2], 
              lwd=1.5, color="blue") +
  geom_ribbon(data = pred, aes(x = Duration, ymin = plo, ymax = phi), alpha = 0.3, fill = "blue") +
  xlab("\nStudy Duration (years)") +
  ylab("Effect Size: ln(S2/S1)\n") +
  theme_bw(base_size=17) +
  annotate(geom="text", x=0.25, y=1.35, label="a)") +
  xlim(c(0,275))+
  ggtitle("Vellend et al.")

gonzPlot

