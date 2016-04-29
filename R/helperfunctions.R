#================
# Helperfunctions
#================

# Stan data input function
stan_in <- function(infile){
  y <- infile$log_SR_ratio
  n <- length(y)
  study <- as.numeric(infile$Study_nr)
  duration <- infile$Duration
  
  n_study <- max(study)

  stan_dat <- list(n=n, n_study=n_study, study = study, y=y, duration=duration)
}

# Sampling Stan model
stan_sampling <- function(object, data, iter = 2000){
  sampling(object, data, iter = iter, warmup = iter/2, thin = 1, chains = 3,
           init="random", verbose = F, control = list(adapt_delta = .9, max_treedepth = 10))
}
