data {
  int<lower=0> n;
  int<lower=0> n_study;
  int<lower=0> n_pred;
  int<lower=1,upper=n_study> study[n];
  vector[n] duration;
  vector[n_pred] new_duration;
  vector[n] y;
}
parameters {
  vector[n_study] b;
  real mu_b;
  real<lower=0> sigma;
  real<lower=0> sigma_b;
}
transformed parameters {
  vector[n] y_hat;
  
  // linear predictor
  for (i in 1:n)
    y_hat[i] <- duration[i] * b[study[i]];
}
model {
  //priors
  mu_b ~ normal(0, 1);
  b ~ normal(mu_b, sigma_b);

  // likelihood
  y ~ normal(y_hat, sigma);
}
generated quantities {
  real b_tilde;
  vector[n_pred] y_cre;
  vector[n_pred] y_pre;

  // credible intervals
  y_cre <- mu_b * new_duration;

  // prediction intervals
  b_tilde <- normal_rng(mu_b, sigma_b);

  for(i in 1:n_pred)
    y_pre[i] <- normal_rng(b_tilde * new_duration[i], sigma);
}