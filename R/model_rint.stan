data {
  int<lower=0> n;
  int<lower=0> n_study;
  int<lower=1,upper=n_study> study[n];
  vector[n] y;
} 
parameters {
  vector[n_study] a;
  real mu_a;
  real<lower=0> sigma;
  real<lower=0> sigma_a;
}
transformed parameters {
  vector[n] y_hat;

  // linear predictor
  for (i in 1:n)
    y_hat[i] = a[study[i]];
}
model {
  mu_a ~ normal(0, 1);
  sigma_a ~ cauchy(0,5);
  a ~ normal(mu_a, sigma_a);

  sigma ~ cauchy(0,5);

  y ~ normal(y_hat, sigma);
}
