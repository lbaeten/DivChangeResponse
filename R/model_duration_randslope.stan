data {
  int<lower=0> n;
  int<lower=0> n_study;
  int<lower=1,upper=n_study> study[n];
  vector[n] duration;
  vector[n] y;
} 
parameters {
  vector[n_study] a;
  vector[n_study] b;
  real mu_a;
  real mu_b;
  real<lower=0> sigma;
  real<lower=0> sigma_a;
  real<lower=0> sigma_b;
} 
transformed parameters {
  vector[n] y_hat;
  
  // linear predictor
  for (i in 1:n)
    y_hat[i] <- a[study[i]] + duration[i] * b[study[i]];
}
model {
  //priors
  mu_a ~ normal(0, 1);
  a ~ normal(mu_a, sigma_a);
  
  mu_b ~ normal(0, 1);
  b ~ normal(0.1 * mu_b, sigma_b);

  // likelihood
  y ~ normal(y_hat, sigma);
}