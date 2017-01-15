<!-- README.md is generated from README.Rmd. Please edit that file -->
stants
======

The stant R package implements Bayesian time series models, primarily for illustrative purposes and teaching (University of Washington's Fish 507, Winter quarter 2017) You can install the development version of the package with:

``` r
# install.packages("devtools")
devtools::install_github("eric-ward/safs-timeseries/stants")
```

An example model
----------------

Simulate data:

``` r
library(rstan)
#> Loading required package: ggplot2
#> Loading required package: StanHeaders
#> rstan (Version 2.13.2, packaged: 2016-12-18 07:04:42 UTC, GitRev: 5fa1e80eb817)
#> For execution on a local, multicore CPU with excess RAM we recommend calling
#> rstan_options(auto_write = TRUE)
#> options(mc.cores = parallel::detectCores())
library(stants)
set.seed(123)
s = cumsum(rnorm(50))
```

``` r
plot(s)
```

![](README-figs/plot-1.png)

Fit several models to this data:

``` r
# Regression, no slope
regression_model = fit_stan(y = s, x = model.matrix(lm(s~1)), model_name="regression")

# Regression, with slope
regression_model = fit_stan(y = s, x = model.matrix(lm(s~seq(1,length(s)))), model_name="regression")

# AR(1) time series model
ar1_model = fit_stan(y = s, est_drift=FALSE, P = 1, model_name = "ar")

# ARMA(1,1) time series model
arma1_model = fit_stan(y = s, model_name = "arma11")

# univariate ss model -- without drift but mean reversion estimated
ss_model = fit_stan(y = s, model_name = "ss_ar", est_drift=FALSE)
```

References
==========

[Fish 507 class website](https://catalyst.uw.edu/workspace/fish203/35553/243766) ...
