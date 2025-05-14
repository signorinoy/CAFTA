
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CAFTA

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/CAFTA)](https://CRAN.R-project.org/package=CAFTA)
[![R-CMD-check](https://github.com/signorinoy/CAFTA/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/signorinoy/CAFTA/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/signorinoy/CAFTA/graph/badge.svg)](https://app.codecov.io/gh/signorinoy/CAFTA)
<!-- badges: end -->

The goal of CAFTA is to …

## Installation

You can install the development version of CAFTA like so:

``` r
# install.packages("pak")
pak::pak("signorinoy/CAFTA")
```

## Example

This is a basic example which shows you how to fit and update a model
using CAFTA:

``` r
library(CAFTA)

formula <- Surv(time, status) ~ x1 + x2 + x31 + x42 + x43 + x44
fit <- cafta(formula, sim[sim$group == 1, ], dist = "weibull", robust = TRUE)
for (k in 2:6) {
  fit <- update(fit, sim[sim$group == k, ])
}
summary(fit)
#> Call:
#> update.cafta(object = fit, newdata = sim[sim$group == k, ])
#> 
#> Coefficients:
#>             Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)  0.34509    0.10215   3.378 0.000729 ***
#> x1           0.15768    0.03262   4.834 1.34e-06 ***
#> x2          -0.22310    0.03497  -6.379 1.78e-10 ***
#> x31          0.31520    0.07825   4.028 5.63e-05 ***
#> x42         -0.07377    0.12558  -0.587 0.556913    
#> x43         -0.13745    0.10769  -1.276 0.201849    
#> x44          0.17426    0.11444   1.523 0.127835    
#> log(scale)  -0.23404    0.03625  -6.456 1.07e-10 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```
