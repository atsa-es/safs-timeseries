data {
  int<lower=0> N;
  int<lower=0> K;
  matrix[N, K] x;
  vector[N] y;
}
parameters {
  real alpha;
  vector[K] beta;
  real<lower=0> sigma;
}
transformed parameters {
  vector[N] pred;
  pred = x * beta + alpha;
}
model {
  y ~ normal(pred, sigma);
}
