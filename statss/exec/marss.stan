data {
  int<lower=0> N;
  int<lower=0> M;  
  int<lower=0> states[M]; # vector assigning time series to states
  int<lower=0> S; # number of states
  int<lower=0> obsVariances[M];
  int<lower=0> n_obsvar;
  int<lower=0> proVariances[S];
  int<lower=0> n_provar;  
  int<lower=0> trends[S];
  int<lower=0> n_trends;
  int<lower=0> n_pos; # number of non-NA values
  int<lower=0> col_indx_pos[n_pos];
  int<lower=0> row_indx_pos[n_pos];
  vector[n_pos] y; # data
}
parameters {
  vector[S] x0; # initial states
  vector[S] pro_dev[N-1];
  vector[n_trends] U;
  real<lower=0> sigma_process[S];
  real<lower=0> sigma_obs[n_obsvar];
}
transformed parameters {
  vector[M] pred[N];
  vector[S] x[N]; # elements accessed [N,K]
  # random walk in states
  for(s in 1:S) {
   x[1,s] = x0[s]; # initial state, vague prior below
   for(t in 2:N) {
    x[t,s] = x[t-1,s] + U[trends[s]] + pro_dev[t-1,s];
   }
  }
  # map predicted states to time series
  for(m in 1:M) {
    for(t in 1:N) {
      pred[t,m] = x[t,states[M]];   
    }
  }
}
model {
  x0 ~ normal(0,10);
  for(i in 1:n_obsvar) {
    sigma_obs[i] ~ cauchy(0,5);
  }
  for(s in 1:n_provar) {
    sigma_process[s] ~ cauchy(0,5); # process var 
  }
  for(i in 1:n_trends) {
    U[i] ~ normal(0,1);
  }
  for(s in 1:S) {
    pro_dev[s] ~ normal(0, sigma_process[proVariances[s]]); # process deviations
  }
  
  # likelihood
  for(i in 1:n_pos) {
    y[i] ~ normal(pred[col_indx_pos[i], row_indx_pos[i]], sigma_obs[obsVariances[row_indx_pos[i]]]);
  }
}
