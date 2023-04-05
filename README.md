
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

    ##               Y Z           X       eta1 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
    ##   1:  0.1334739 1  0.02084144 -0.6655418  1  0  0  0  1  0  0  0  1   0
    ##   2:  0.5637497 1  1.12518541  0.4075764  1  1  1  0  0  0  1  1  1   0
    ##   3:  1.4244225 1 -0.58799823 -0.4149190  0  0  1  1  1  0  0  1  1   0
    ##   4:  0.3644402 1 -0.20996355  1.1467449  1  1  1  0  1  1  1  1  1   1
    ##   5: -0.1702918 1  0.54647799  0.4955865  1  1  1  0  1  0  1  1  1   0
    ##  ---                                                                   
    ## 196: -0.2333591 0 -1.10325551 -0.7155695 NA NA NA NA NA NA NA NA NA  NA
    ## 197: -0.7420041 0 -0.72720950 -0.5975197 NA NA NA NA NA NA NA NA NA  NA
    ## 198: -0.5379497 0 -1.52642396 -1.1299039 NA NA NA NA NA NA NA NA NA  NA
    ## 199:  0.6956099 0  0.26472627  0.6585079 NA NA NA NA NA NA NA NA NA  NA
    ## 200: -2.4699201 0  2.34152782  0.5237120 NA NA NA NA NA NA NA NA NA  NA

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
