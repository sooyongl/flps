
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

The documentation is available at
[here](https://sooyongl.github.io/flps/).

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

    ##                 Y Z           X       eta1 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
    ##   1: -0.002611468 1  0.90744790  1.5779526  1  0  1  0  0  1  1  1  1   1
    ##   2:  2.086529291 1 -1.80016638  0.1821458  1  1  0  0  0  1  0  1  0   0
    ##   3: -0.601860875 1  0.94258573 -0.3488853  1  1  0  1  0  1  0  1  0   0
    ##   4:  0.755511330 1  0.08701257  0.2454388  1  1  0  1  1  0  1  1  1   0
    ##   5:  1.019806352 1 -0.55140690 -0.4461404  0  1  0  0  1  1  0  0  0   1
    ##  ---                                                                     
    ## 196:  1.195159718 0 -1.59344311 -2.7175960 NA NA NA NA NA NA NA NA NA  NA
    ## 197:  0.183609571 0  0.11719486 -0.2373423 NA NA NA NA NA NA NA NA NA  NA
    ## 198:  0.574084691 0  0.63512354  0.3048086 NA NA NA NA NA NA NA NA NA  NA
    ## 199:  0.797686049 0  0.30126864  1.1631879 NA NA NA NA NA NA NA NA NA  NA
    ## 200: -0.770394718 0  0.32392566 -0.5996783 NA NA NA NA NA NA NA NA NA  NA

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
  stan_options = list(iter = 1000, warmup = 500, cores = 1, chains = 2)
)
```

The summary of results can be shown by `summary()`.

``` r
summary.flps(res, type = "casual")
```
