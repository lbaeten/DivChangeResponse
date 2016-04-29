#========================
# Some helperfunctions
#========================

# Data input function
new_duration <- seq(from=min(dta_SR$Duration),to=max(dta_SR$Duration),by=1)

stan_in <- function(infile){
  y <- infile$log_SR_ratio
  n <- length(y)
  study <- as.numeric(infile$Study_nr)
  n_study <- max(study)
  
  duration <- infile$Duration
  
  n_pred <- length(new_duration)
  
  list(n=n, n_study=n_study, n_pred, study = study, y=y, duration=duration, new_duration = new_duration)
}

# Draw samples function
stan_sampling <- function(object, data, iter = 2000){
  sampling(object, data, iter = iter, warmup = iter/2, thin = 1, chains = 3,
           init="random", verbose = F, control = list(adapt_delta = .9, max_treedepth = 10))
}

# Extract predictions
stan_extract <- function(object) {
  stan_pred <- data.frame(expand.grid(new_x = new_duration, new_y = 0))
  y_quant <- apply(rstan::extract(object)$y_pred, 2, quantile, probs = c(.025, .5, .975)) 

  stan_pred$new_y <- y_quant[2,]
  stan_pred$plo <- y_quant[1,]
  stan_pred$phi <- y_quant[3,]

  stan_pred
}