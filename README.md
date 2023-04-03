
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

    ##               Y Z          X       eta1 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
    ##   1:  1.7726499 1  0.1793591 -0.1339627  0  1  0  1  1  1  1  0  1   1
    ##   2: -0.1592046 1 -0.7585487 -0.4683049  1  0  0  1  1  1  0  0  0   0
    ##   3: -0.5428223 1 -0.5108447 -0.6776410  1  0  0  0  0  0  0  0  1   1
    ##   4: -1.2374792 1 -0.5074413  0.4336264  1  1  1  1  1  1  1  0  0   1
    ##   5: -0.1376694 1 -1.1565499 -0.3100630  0  0  1  1  1  0  1  0  0   1
    ##  ---                                                                  
    ## 196: -0.7119943 0  0.5735908  0.4257563 NA NA NA NA NA NA NA NA NA  NA
    ## 197:  0.4722451 0  0.5944321  0.4997790 NA NA NA NA NA NA NA NA NA  NA
    ## 198: -1.6659408 0  1.0867065  0.3386805 NA NA NA NA NA NA NA NA NA  NA
    ## 199:  2.7139583 0 -0.5198940  0.3586834 NA NA NA NA NA NA NA NA NA  NA
    ## 200:  0.4116134 0 -0.6648635 -0.2775411 NA NA NA NA NA NA NA NA NA  NA

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
