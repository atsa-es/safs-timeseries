#' fit_stan is the primary function which calls pre-written stan scripts for time series data.
#'
#' @param y The response variable (numeric)
#' @param x The predictors, either a vector or matrix
#' @param model_name The specific name of the model to be fitted. Currently supported are 'regression', 'ar', 'rw', 'ma', 'ss_ar' (state space univariate AR), or 'ss_rw' (state space univariate random walk).
#' @param est_drift Whether or not to estimate a drift parameter (default = FALSE). Only applicable to the rw and ar models.
#' @param P The order of the ar model, with minimum value = 1 (default).
#' @param Q The order of the ma model, with minimum value = 1 (default).
#' @param mcmc_list A list of MCMC control parameters. These include the number of 'iterations' (default = 1000), burn in or warmup (default = 500), chains (default = 3), and thinning (default = 1)
#'
#' @return an object of class 'rstan'
#' @export
#'
#' @examples
fit_stan <- function(y, x=NA, model_name = NA, est_drift = FALSE, P = 1, Q = 1, mcmc_list = list(n_mcmc = 1000, n_burn = 500, n_chain = 3, n_thin = 1)) {
  if(model_name == "regression") {
    if(class(x)!="matrix") x = matrix(x,ncol=1)
    mod = stan("exec/regression.stan", data = list("N"=length(y),"K"=dim(x)[2],"x"=x,"y"=y),
      pars = c("beta","sigma","pred"), chains = mcmc_list$n_chain, iter = mcmc_list$n_mcmc, thin = mcmc_list$n_thin)
  }
  if(model_name == "regression_cor") {
    if(class(x)!="matrix") x = matrix(x,ncol=1)
    mod = stan("exec/regression_cor.stan", data = list("N"=length(y),"K"=dim(x)[2],"x"=x,"y"=y),
      pars = c("beta","sigma","pred","phi","sigma_cor"), chains = mcmc_list$n_chain, iter = mcmc_list$n_mcmc, thin = mcmc_list$n_thin)
  }
  if(model_name == "rw" & est_drift == FALSE) {
    mod = stan("exec/rw.stan", data = list("y"=y,"N"=length(y)), pars = c("sigma","pred"),
      chains = mcmc_list$n_chain, iter = mcmc_list$n_mcmc, thin = mcmc_list$n_thin)
  }
  if(model_name == "rw" & est_drift == TRUE) {
    mod = stan("exec/rw_drift.stan", data = list("y"=y,"N"=length(y)), pars = c("sigma","pred","mu"),
      chains = mcmc_list$n_chain, iter = mcmc_list$n_mcmc, thin = mcmc_list$n_thin)
  }
  if(model_name == "ar" & est_drift == FALSE) {
    mod = stan("exec/ar.stan", data = list("y"=y,"N"=length(y)), pars = c("sigma","pred","phi"),
      chains = mcmc_list$n_chain, iter = mcmc_list$n_mcmc, thin = mcmc_list$n_thin)
  }
  if(model_name == "ar" & est_drift == TRUE) {
    mod = stan("exec/ar_drift.stan", data = list("y"=y,"N"=length(y)), pars = c("sigma","pred","mu","phi"),
      chains = mcmc_list$n_chain, iter = mcmc_list$n_mcmc, thin = mcmc_list$n_thin)
  }
  if(model_name == "ma" & Q == 1) {
    mod = stan("exec/ma1.stan", data = list("y"=y,"N"=length(y)), pars = c("sigma","pred","mu","theta"),
      chains = mcmc_list$n_chain, iter = mcmc_list$n_mcmc, thin = mcmc_list$n_thin)
  }
  if(model_name == "ma" & Q > 1) {
    mod = stan("exec/ma.stan", data = list("Q"=Q,"y"=y,"N"=length(y)), pars = c("sigma","pred","mu","theta"),
      chains = mcmc_list$n_chain, iter = mcmc_list$n_mcmc, thin = mcmc_list$n_thin)
  }
  if(model_name == "ss_rw" & est_drift == FALSE) {
    mod = stan("exec/ss_rw.stan", data = list("y"=y,"N"=length(y)), pars = c("sigma_process","pred", "sigma_obs"),
      chains = mcmc_list$n_chain, iter = mcmc_list$n_mcmc, thin = mcmc_list$n_thin)
  }
  if(model_name == "ss_rw" & est_drift == TRUE) {
    mod = stan("exec/ss_rw_drift.stan", data = list("y"=y,"N"=length(y)), pars = c("sigma_process","pred", "sigma_obs", "mu"),
      chains = mcmc_list$n_chain, iter = mcmc_list$n_mcmc, thin = mcmc_list$n_thin)
  }
  if(model_name == "ss_ar" & est_drift == FALSE) {
    mod = stan("exec/ss_ar.stan", data = list("y"=y,"N"=length(y)), pars = c("sigma_process","pred", "sigma_obs", "phi"),
      chains = mcmc_list$n_chain, iter = mcmc_list$n_mcmc, thin = mcmc_list$n_thin)
  }
  if(model_name == "ss_ar" & est_drift == TRUE) {
    mod = stan("exec/ss_ar_drift.stan", data = list("y"=y,"N"=length(y)), pars = c("sigma_process","pred", "sigma_obs", "mu", "phi"),
      chains = mcmc_list$n_chain, iter = mcmc_list$n_mcmc, thin = mcmc_list$n_thin)
  }
  if(model_name == "arma11") {
    mod = stan("exec/arma11.stan", data = list("y"=y,"N"=length(y)), pars = c("sigma", "theta", "mu", "phi"),
      chains = mcmc_list$n_chain, iter = mcmc_list$n_mcmc, thin = mcmc_list$n_thin)
  }
  return(mod)
}
