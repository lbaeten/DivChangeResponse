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
  mu_b ~ normal(0, 1);
  b ~ normal(mu_b, sigma_b);

  y ~ normal(y_hat, sigma);
}
generated quantities {
  vector[n_pred] y_cre;

  y_cre <- mu_b * new_duration;
}