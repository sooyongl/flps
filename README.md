
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

If compiling errors occur, see
[here](https://github.com/stan-dev/rstan/wiki/Configuring-C---Toolchain-for-Windows#r-42).

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

    ##                Y Z          X       eta1 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
    ##   1:  0.08869462 1  0.2216264  0.8981923  1  1  1  0  0  1  1  0  1   0
    ##   2: -0.01824707 1  0.1287782  0.4112477  1  0  1  1  0  1  1  1  0   0
    ##   3:  0.61522760 1  0.2463158  0.2468933  1  1  1  0  1  1  0  0  0   0
    ##   4:  0.23335010 1  0.1046426  0.1454832  1  1  1  1  0  1  1  0  1   0
    ##   5: -0.67888432 1 -0.0803088 -0.3228670  1  0  1  0  0  1  1  0  1   0
    ##  ---                                                                   
    ## 196:  0.07394363 0 -0.2793789 -1.4326300 NA NA NA NA NA NA NA NA NA  NA
    ## 197: -0.58291780 0  1.6094844  0.3070913 NA NA NA NA NA NA NA NA NA  NA
    ## 198:  0.98271575 0  0.3940290  0.4581900 NA NA NA NA NA NA NA NA NA  NA
    ## 199:  0.04670380 0 -0.1170385  0.7839863 NA NA NA NA NA NA NA NA NA  NA
    ## 200: -1.09842684 0  0.5072524  2.5368779 NA NA NA NA NA NA NA NA NA  NA

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
summary(res, type = "casual")
```
