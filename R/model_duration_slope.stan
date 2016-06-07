data {
  int<lower=0> n;
  int<lower=0> n_pred;
  vector[n] duration;
  vector[n_pred] new_duration;
  vector[n] y;
} 
parameters {
  real b;
  real<lower=0> sigma;
}
transformed parameters {
  vector[n] y_hat;
  
  for (i in 1:n)
    y_hat[i] <- duration[i] * b;
}
model {
  y ~ normal(y_hat, sigma);
}
generated quantities {
  vector[n_pred] y_cre;

  y_cre <- new_duration * b;
}