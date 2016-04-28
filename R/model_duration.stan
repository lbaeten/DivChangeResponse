data {
  int<lower=0> n;
  int<lower=0> n_study;
  int<lower=1,upper=n_study> study[n];
  vector[n] duration;
  vector[n] y;
} 
parameters {
  vector[n_study] b;
  real beta;
  real mu_b;
  real<lower=0> sigma;
  real<lower=0> sigma_b;
} 
transformed parameters {
  vector[n] y_hat;
  
  // linear predictor
  for (i in 1:n)
    y_hat[i] <- b[study[i]] + duration[i] * beta;
}
model {
  //priors
  mu_b ~ normal(0, 1);
  b ~ normal(mu_b, sigma_b);
  beta ~ normal(0, 1);

  // likelihood
  y ~ normal(y_hat, sigma);
}