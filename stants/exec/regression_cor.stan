data {
  int<lower=0> N;
  int<lower=0> K;
  matrix[N, K] x;
  vector[N] y;
}
parameters {
  vector[K] beta;
  real<lower=0> sigma;
  real<lower=-1,upper=1> phi;
}
transformed parameters {
  vector[N] pred;
  vector[N] epsilon;
  real sigma_cor;
  pred[1] = x[1] * beta;
  epsilon[1] = y[1]-pred[1];
  for(i in 2:N) {
  pred[i] = x[i] * beta;
  epsilon[i] = (y[i] - pred[i]) - phi*epsilon[i-1];
  }
  sigma_cor = sqrt(sigma*sigma * (1-phi*phi)); # Var = sigma2 * (1-rho^2)
}
model {
  phi ~ normal(0,1);
  beta ~ normal(0,2);
  y ~ normal(pred, sigma_cor);
}
