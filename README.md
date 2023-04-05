
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

If compling errors occur, see
[here](https://github.com/stan-dev/rstan/wiki/Configuring-C---Toolchain-for-Windows#r-42).

## Basic working example

### Running with the package

- Generate a simulated rectangular data.
- This data will be converted to a list of data for
  [`rstan`](https://github.com/stan-dev/rstan) package.
- For latent variable models, Rasch, 2PL, GRM, and SEM (one-factor CFA)
  are available.

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

- `inp_data`: a data frame containing all the data for FLPS. It is used
  in `runFLPS` function.

``` r
# Input data matrix
data.table::data.table(inp_data)
```

    ##                Y Z           X       eta1 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
    ##   1:  0.62562534 1 -0.42116814  0.5728188  1  1  0  1  1  1  0  0  1   1
    ##   2: -0.31757694 1  0.93418192  0.2817544  0  0  1  1  1  1  0  0  1   1
    ##   3:  0.74942580 1 -1.98573532 -2.3957913  0  1  1  0  0  1  0  0  0   0
    ##   4:  0.16054994 1  0.31994754 -0.1088874  1  1  0  1  1  1  0  0  0   0
    ##   5:  0.51777415 1  0.01307849 -1.2011336  1  1  0  1  0  1  1  1  0   0
    ##  ---                                                                    
    ## 196:  0.77154946 0 -2.23472798 -2.4594862 NA NA NA NA NA NA NA NA NA  NA
    ## 197: -0.30967398 0  1.92133226  0.7003654 NA NA NA NA NA NA NA NA NA  NA
    ## 198: -0.06059372 0 -0.36360236 -0.1223452 NA NA NA NA NA NA NA NA NA  NA
    ## 199: -0.40910139 0  0.39644264  0.3700518 NA NA NA NA NA NA NA NA NA  NA
    ## 200:  0.13253869 0  0.69286702  0.1302301 NA NA NA NA NA NA NA NA NA  NA

- Fit your FLPS model

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
