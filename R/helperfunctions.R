#========================
# Some helperfunctions
#========================

# Data input functions
#---------------------
  # data input for Dornelas analyses
stan_in_dorn <- function(infile, y_variable){
  y <- infile[,y_variable]
  n <- length(y)

  duration <- infile$duration

  new_duration <- seq(from=min(duration), to=max(duration), by=1)
  n_pred <- length(new_duration)

  list(n=n, n_pred=n_pred, y=y, duration=duration, new_duration=new_duration)
}

  # data input for Vellend analyses
stan_in_vel <- function(infile, lrr){
  if(lrr == "raw"){
    y <- infile$log_SR_ratio
  }
  if(lrr == "decade"){
    y <- infile$log_SR_ratio/(1/10 * infile$Duration)
  }

  n <- length(y)
  study <- as.numeric(infile$Study_nr)
  n_study <- max(study)
  
  duration <- infile$Duration

  new_duration <- seq(from=min(infile$Duration),to=max(infile$Duration),by=1)
  n_pred <- length(new_duration)
  
  list(n=n, n_study=n_study, n_pred = n_pred, study = study, y=y, duration=duration, new_duration = new_duration)
}

# Draw samples
#----------------
stan_sampling <- function(object, data, iter = 10000){
  sampling(object, data, iter = iter, warmup = iter/2, thin = 1, chains = 3,
           init="random", verbose = F, control = list(adapt_delta = .9, max_treedepth = 10))
}

# Extract credible intervals
#--------------------------
stan_extract_cre <- function(object, data) {
  stan_int <- data.frame(expand.grid(new_x = data$new_duration, new_y = 0))
  y_cre <- apply(rstan::extract(object)$"y_cre", 2, quantile, probs = c(.025, .5, .975)) 

  stan_int$new_y <- y_cre[2,]
  stan_int$plo <- y_cre[1,]
  stan_int$phi <- y_cre[3,]
  
  stan_int
}
