data {
  int<lower=0> N;
  int<lower=0> K;
  matrix[N, K] x;
  vector[N] y;
}
parameters {
  vector[K] beta;
  real<lower=0> sigma;
}
transformed parameters {
  vector[N] pred;
  pred = x * beta;
}
model {
  beta ~ normal(0,2);
  sigma ~ cauchy(0, 5);
  y ~ normal(pred, sigma);
}
generated quantities {
  vector[N] log_lik;
  # regresssion example in loo() package 
  for (n in 1:N) log_lik[n] = normal_lpdf(y[n] | pred[n], sigma);
}
