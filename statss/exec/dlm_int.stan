data {
  int<lower=0> N;
  vector[N] y;
  int<lower=0> K; # number of covariates
  matrix[N, K] x; # matrix of covariates 
}
parameters {
  real x0;
  vector[K] beta;  
  vector[N-1] pro_dev;
  real<lower=0> sigma_process;
  real<lower=0> sigma_obs;
}
transformed parameters {
  vector[N] pred;
  vector[N] intercept;
  intercept[1] = x0;
  for(i in 2:N) {
    intercept[i] = intercept[i-1] + pro_dev[i-1];
  }
  pred = x * beta + intercept;  
}
model {
  x0 ~ normal(0,10);
  sigma_process ~ cauchy(0,5);
  sigma_obs ~ cauchy(0,5);
  pro_dev ~ normal(0, sigma_process);
  y ~ normal(pred, sigma_obs);
}
generated quantities {
  vector[N] log_lik;
  # regresssion example in loo() package 
  for (n in 1:N) log_lik[n] = normal_lpdf(y[n] | pred[n], sigma_obs);
}