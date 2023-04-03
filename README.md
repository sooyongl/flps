
<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/flps)](https://CRAN.R-project.org/package=flps)
<!-- badges: end -->

## Fully Latent Principal Stratification (FLPS)

Fully Latent Principal Stratification (**FLPS**) is an extension of
principal stratification.

## Install

Install the latest release from CRAN:

``` r
devtools::install_github("sooyongl/flps")
```

The documentation is available at (…)

## Basic working example

### Running with the package

-   Generate a simulated rectangular data.
-   This data will be converted to a list of data for
    [`rstan`](https://github.com/stan-dev/rstan) package.
-   For latent variable models, Rasch, 2PL, GRM, and SEM (one-factor
    CFA) are available.

``` r
inp_data <- flps::makeInpData(
  N       = 200,  # sample size
  R2Y     = 0.2,  # r^2 of outcome
  R2eta   = 0.5,  # r^2 of eta by one covariates
  omega   = 0.2,  # the effect of eta
  tau0    = 0.13, # direct effect
  tau1    = -0.06,# interaction effect between Z and eta
  lambda  = 0.8,  # the proportion of administered items
  nitem    = 10,   # the total number of items
  nfac    = 1,    # the number of latent factors
  lvmodel = 'rasch' # tag for latent variable model; case-sensitive (use lower-case letters)
)
```

`makeInpData()` creates input data for running FLPS.

-   `inp_data`: a data frame containing all the data for FLPS. It is
    used in `runFLPS` function.

``` r
# Input data matrix
data.table::data.table(inp_data)
```

    ##               Y Z            X       eta1 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
    ##   1: -0.4330534 1 -0.008305128  1.0284558  1  1  1  1  1  1  0  1  1   1
    ##   2: -0.6644784 1  0.432836254  1.2121487  1  0  1  0  1  1  1  0  1   1
    ##   3:  1.0098029 1 -0.308463756  0.6607198  1  1  1  1  1  0  0  0  1   0
    ##   4: -0.1393475 1 -0.893797649 -1.8706669  1  0  1  1  0  0  0  0  1   1
    ##   5:  1.3176301 1 -1.581849086 -0.3999409  0  0  1  0  0  0  0  1  1   0
    ##  ---                                                                    
    ## 196: -0.1245758 0 -1.552931331 -2.2896061 NA NA NA NA NA NA NA NA NA  NA
    ## 197:  0.8742926 0 -2.043462164 -0.6438803 NA NA NA NA NA NA NA NA NA  NA
    ## 198:  0.4904238 0 -1.002213540 -0.3043799 NA NA NA NA NA NA NA NA NA  NA
    ## 199: -0.5427682 0  0.187801699  1.5806214 NA NA NA NA NA NA NA NA NA  NA
    ## 200:  0.1677528 0 -0.533701416  0.3118214 NA NA NA NA NA NA NA NA NA  NA

-   Fit your FLPS model

Now, provide information about your model. `runFLPS` internally coverts
`inp_data` into the data format for `rstan` given the information, and
runs FLPS.

``` r
res <- runFLPS(
  inp_data = inp_data,
  outcome = "Y",
  group = "Z",
  covariate = c("X"),
  lv_type = "rasch",
  lv_model = "F =~ v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9 + v10",
  stan_options = list(iter = 4000, warmup = 2000, cores = 1, chains = 4)
)
```
