
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
set.seed(10000)
inp_data <- flps::makeInpData(
  N       = 200,  # sample size
  R2Y     = 0.2,  # r^2 of outcome
  R2eta   = 0.5,  # r^2 of eta by one covariates
  omega   = 0.2,  # the effect of eta
  tau0    = 0.23, # direct effect
  tau1    = -0.16,# interaction effect between Z and eta
  betaL   = 0.2,
  betaY   = 0.4,
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

    ##                Y Z          X1        eta1 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
    ##   1: -0.72862564 1 -0.20087849  0.49705730  0  1  0  0  1  0  1  0  1   1
    ##   2:  0.43624761 1 -0.81367558  0.09644683  1  1  1  1  1  1  0  0  0   1
    ##   3:  0.71005101 1 -0.09306958 -0.30660832  1  1  1  0  0  1  1  1  0   1
    ##   4:  0.01947398 1 -0.08743884 -0.37419814  0  1  1  0  0  0  1  0  0   0
    ##   5: -1.00950577 1 -2.16774891 -1.81547040  0  1  0  0  0  0  0  0  0   0
    ##  ---                                                                     
    ## 196: -0.47559245 0  0.23763106 -0.27108910 NA NA NA NA NA NA NA NA NA  NA
    ## 197:  0.46877629 0 -0.03646065  1.12609970 NA NA NA NA NA NA NA NA NA  NA
    ## 198:  0.78717334 0  0.06867924  0.07008599 NA NA NA NA NA NA NA NA NA  NA
    ## 199:  0.56380180 0  0.56467755  0.34826071 NA NA NA NA NA NA NA NA NA  NA
    ## 200:  0.36809486 0  0.82158503 -0.35012492 NA NA NA NA NA NA NA NA NA  NA

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
  covariate = c("X1"),
  lv_type = "rasch",
  lv_model = "F =~ v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9 + v10",
  stan_options = list(iter = 1000, warmup = 500, cores = 1, chains = 2)
)
```

    ## Compiling Stan code...

    ## 
    ## SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.00086 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 8.6 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:   1 / 1000 [  0%]  (Warmup)
    ## Chain 1: Iteration: 100 / 1000 [ 10%]  (Warmup)
    ## Chain 1: Iteration: 200 / 1000 [ 20%]  (Warmup)
    ## Chain 1: Iteration: 300 / 1000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 400 / 1000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 500 / 1000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 501 / 1000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 600 / 1000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 700 / 1000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 800 / 1000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 900 / 1000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 1000 / 1000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 12.862 seconds (Warm-up)
    ## Chain 1:                9.492 seconds (Sampling)
    ## Chain 1:                22.354 seconds (Total)
    ## Chain 1:

    ## Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
    ## Running the chains for more iterations may help. See
    ## https://mc-stan.org/misc/warnings.html#bulk-ess

    ## Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
    ## Running the chains for more iterations may help. See
    ## https://mc-stan.org/misc/warnings.html#tail-ess

The summary of results can be shown by `summary()`.

``` r
summary(res, type = "causal")
```

    ##               mean     se_mean        sd        2.5%        25%        50%
    ## tau0     0.2292788 0.005370111 0.1353325 -0.04784608  0.1410179  0.2345303
    ## tau1[1] -0.2288520 0.101820994 0.4674258 -0.92391714 -0.5972068 -0.3055769
    ##                75%     97.5%     n_eff      Rhat
    ## tau0    0.32088555 0.4838128 635.09305 0.9983786
    ## tau1[1] 0.07374961 0.7885876  21.07418 1.0158583

The `flps_plot()` shows the plot related to FLPS models

``` r
flps_plot(res, type = "causal")
```

<img src="man/figures/causal_1.png" width="60%" style="display: block; margin: auto;" />

``` r
flps_plot(res, type = "latent")
```

<img src="man/figures/latent_1.png" width="60%" style="display: block; margin: auto;" />
