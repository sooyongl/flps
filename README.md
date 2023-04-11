
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

    ##               Y Z           X        eta1 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
    ##   1: -0.8229286 1  0.81970123  0.80513323  1  1  1  0  0  0  0  0  1   1
    ##   2: -2.2757364 1 -0.04787554 -0.17824663  0  1  1  0  0  1  1  1  1   0
    ##   3:  1.7244453 1 -0.28450512 -0.04733339  0  0  1  0  0  0  0  0  1   0
    ##   4: -0.9570852 1  2.47234904  3.03804671  1  1  1  1  1  0  1  1  1   1
    ##   5: -0.4641767 1  0.52827230  0.40092126  0  0  1  0  1  0  0  1  1   0
    ##  ---                                                                    
    ## 196: -0.5238044 0  0.70031265  0.87764963 NA NA NA NA NA NA NA NA NA  NA
    ## 197:  1.3158738 0 -2.03092566 -1.53048045 NA NA NA NA NA NA NA NA NA  NA
    ## 198: -1.8832812 0  2.08024036  1.96220171 NA NA NA NA NA NA NA NA NA  NA
    ## 199: -1.2764512 0  1.91509187  0.96052374 NA NA NA NA NA NA NA NA NA  NA
    ## 200:  1.4095648 0 -0.47788144  0.07156641 NA NA NA NA NA NA NA NA NA  NA

-   Fit your FLPS model

Now, provide information about your model. `runFLPS` internally coverts
`inp_data` into the data format for `rstan` given the information, and
runs FLPS.

To avoid having to compile the Stan code every time you run it, you can
pre-compile the code using the `modelBuilder()` function along with the
relevant measurement model. This function will compile the Stan code and
store the resulting stanmodel object in the `flps` package directory.
The next time you run `runFLPS()`, the code will skip the compilation
step, making your analysis faster and more efficient. Otherwise, it will
take a while for `runFLPS()` to compile the Stan code.

``` r
modelBuilder(type = "rasch")
```

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
