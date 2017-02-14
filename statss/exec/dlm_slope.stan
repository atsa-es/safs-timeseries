data {
  int<lower=0> N;
  vector[N] y;
  int<lower=0> K; # number of covariates
  matrix[N, K] x; # matrix of covariates 
}
parameters {
  real x0;
  vector[K] beta0;  
  vector[K] pro_dev[N-1];
  real<lower=0> sigma_process[K];
  real<lower=0> sigma_obs;
}
transformed parameters {
  vector[N] pred;
  vector[K] beta[N]; # elements accessed [N,K]
  
  for(k in 1:K) {
   beta[1,k] = beta0[k];
   for(i in 2:N) {
    beta[i,k] = beta[i-1,k] + pro_dev[i-1,k];
   }
  }
  for(i in 1:N) {
    pred[i] = x[i] * beta[i] + x0;   
  }
 
}
model {
  x0 ~ normal(0,10);
  sigma_obs ~ cauchy(0,5);
  for(k in 1:K) {
    beta0[k] ~ normal(0,1);
    sigma_process[k] ~ cauchy(0,5);
    pro_dev[k] ~ normal(0, sigma_process[k]);
  }
  y ~ normal(pred, sigma_obs);
}
generated quantities {
  vector[N] log_lik;
  # regresssion example in loo() package 
  for (n in 1:N) log_lik[n] = normal_lpdf(y[n] | pred[n], sigma_obs);
}
