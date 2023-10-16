
<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/flps)](https://CRAN.R-project.org/package=flps)
<!-- badges: end -->

## Fully Latent Principal Stratification (FLPS)[^1]

Fully Latent Principal Stratification (**FLPS**) is an extension of
principal stratification.

## Installation

Install the latest release from CRAN or git repository:

``` r
devtools::install_github("sooyongl/flps")
install.packages("flps")
```

``` r
library(flps)
```

    ## Version: 1.0.0
    ## 
    ## It is a demo.
    ## Acknowledgements. It is supported by the Institute of Education Sciences, U.S. Department of Education, through Grant R305D210036.

- Documentation is available [here](https://sooyongl.github.io/flps/).

- For compiling errors on Windows, see the relevant
  [guide](https://github.com/stan-dev/rstan/wiki/Configuring-C---Toolchain-for-Windows#r-42).

## Basic working example

### Load Example Data

- `binary`: a data frame containing all the data for FLPS. It is used in
  `runFLPS` function.
- This data will be converted to a list of data for
  [`rstan`](https://github.com/stan-dev/rstan) package.
- For latent variable models, Rasch, 2PL, GRM, SEM (one-factor CFA), and
  mixture models (LCA and LPA) are available.
- Multilevel structure will be supported soon.

``` r
data(binary)
```

``` r
# Input data frame
data.table::data.table(binary)
```

    ##       schid   id sex race pretest stdscore trt      Y q1 q2 q3 q4 q5 q6 q7 q8
    ##    1:     1 2383   0    1      20  -0.3296   1 -0.487  0 NA  1  1  1  1  1 NA
    ##    2:     1 2384   1    0       8   1.1597   1  0.487  1 NA  1  1  1  1  1  1
    ##    3:     1 2385   0    1      14  -0.7385   1 -1.073  0 NA  1  1  0 NA NA NA
    ##    4:     1 2387   0    1      12  -1.3518   1 -1.087  0 NA  1  1  1 NA NA NA
    ##    5:     1 2388   0    1       6  -1.2057   1 -1.171  0 NA  1  1 NA NA NA NA
    ##   ---                                                                        
    ## 4762:    63 4761   0    1      16   0.6457   0 -0.114  0 NA NA NA NA NA NA NA
    ## 4763:    63 4763   0    0      11   0.3168   0 -0.438  0 NA NA NA NA NA NA NA
    ## 4764:    63 4764   0    0      18  -0.4114   0 -1.047  0 NA NA NA NA NA NA NA
    ## 4765:    63 4765   1    0      31   2.2116   0  0.765  0 NA NA NA NA NA NA NA
    ## 4766:    63 4766   1    1      13  -0.0668   0  0.539  0 NA NA NA NA NA NA NA
    ##       q9 q10 q11 q12 q13 q14 q15 q16 q17 q18 q19 q20
    ##    1: NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
    ##    2:  1   1   1   1  NA  NA  NA  NA  NA  NA  NA  NA
    ##    3: NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
    ##    4: NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
    ##    5: NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
    ##   ---                                               
    ## 4762: NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
    ## 4763: NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
    ## 4764: NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
    ## 4765: NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
    ## 4766: NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA

### Model Fitting with FLPS

- `runFLPS()` internally transforms binary data into the format suitable
  for `rstan`, subsequently executing FLPS.

- To avoid re-compiling the Stan code each time, pre-compile it using
  `modelBuilder()`, which stores the stanmodel object in the `flps`
  directory, accelerating subsequent analyses.

- Once the Stan model is compiled, use `importModel()` to bring in the
  compiled Stan code. This code can then be provided to the
  `compiled_stan` argument in `runFLPS.` If this step is omitted,
  `runFLPS()` will compile the Stan code during each execution of FLPS.

``` r
modelBuilder(type = "rasch")
complied_stan <- importModel(type = "rasch")
```

- In case of errors, try the latest `rstan` and `StanHeaders` packages.

``` r
remove.packages(c("rstan", "StanHeaders"))
install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
```

Now, execute your FLPS model. Given the time-intensive nature of the
process, chains and iterations have been initially limited to 1 and
5000, respectively. It is advisable to increase these values for your
specific research needs.

``` r
# Subset of data: 1000 students
binary <- binary[c(sample(which(binary$trt == 1), 250), 
                   sample(which(binary$trt == 0), 250)),]

res <- runFLPS(
  inp_data = binary,
  # complied_stan = complied # if necessary
  outcome = "Y",
  trt = "trt",
  covariate = c("sex","race","pretest","stdscore"),
  lv_type = "rasch",
  lv_model = "F =~ q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 + q9 + q10",
  stan_options = list(iter = 5000, cores = 1, chains = 2)
)
```

    ## Compiling Stan code...

    ## 
    ## SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.001795 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 17.95 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 5000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  500 / 5000 [ 10%]  (Warmup)
    ## Chain 1: Iteration: 1000 / 5000 [ 20%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 5000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 2000 / 5000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 2500 / 5000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 2501 / 5000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 5000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 3500 / 5000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 4000 / 5000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 4500 / 5000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 5000 / 5000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 221.323 seconds (Warm-up)
    ## Chain 1:                140.054 seconds (Sampling)
    ## Chain 1:                361.377 seconds (Total)
    ## Chain 1:

    ## Warning: There were 1 divergent transitions after warmup. See
    ## https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
    ## to find out why this is a problem and how to eliminate them.

    ## Warning: There were 1 chains where the estimated Bayesian Fraction of Missing Information was low. See
    ## https://mc-stan.org/misc/warnings.html#bfmi-low

    ## Warning: Examine the pairs() plot to diagnose sampling problems

    ## Warning: The largest R-hat is NA, indicating chains have not mixed.
    ## Running the chains for more iterations may help. See
    ## https://mc-stan.org/misc/warnings.html#r-hat

    ## Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
    ## Running the chains for more iterations may help. See
    ## https://mc-stan.org/misc/warnings.html#bulk-ess

    ## Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
    ## Running the chains for more iterations may help. See
    ## https://mc-stan.org/misc/warnings.html#tail-ess

### Results

Retrieve summaries and visualize results with the following:

``` r
summary(res, type = "causal")
```

    ##               mean    se_mean        sd        2.5%         25%        50%
    ## tau0     0.1332205 0.01317565 0.1098414 -0.05848257  0.05958877  0.1266737
    ## tau1[1] -0.1875916 0.02120112 0.1323682 -0.42341767 -0.27890833 -0.1939362
    ##                75%      97.5%    n_eff     Rhat
    ## tau0     0.1991413 0.36759419 69.50043 1.070010
    ## tau1[1] -0.1043238 0.08867978 38.98070 1.101409

The `flps_plot()` shows the plot related to FLPS models

``` r
flps_plot(res, type = "causal")
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
flps_plot(res, type = "latent")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

[^1]: **Acknowledgements.** This package is supported by the Institute
    of Education Sciences, U.S. Department of Education, through Grant
    R305D210036.
