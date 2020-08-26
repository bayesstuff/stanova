
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stanova

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/bayesstuff/stanova.svg?branch=master)](https://travis-ci.org/bayesstuff/stanova)
[![R build
status](https://github.com/bayesstuff/stanova/workflows/R-CMD-check/badge.svg)](https://github.com/bayesstuff/stanova/actions)
<!-- badges: end -->

The goal of `stanova` is to provide a more relevant and interpretable
`summary` for Bayesian models with categorical covariates and possibly
interactions and continuous covariates estimated in `Stan`. The core
functions are `stanova()` which requires specifying which `rstanarm`
function should be called through argument `model_fun` (e.g., `model_fun
= glmer` calls `stan_glmer` and allows fitting Bayesian mixed models)
and `stanova_brm()` which estimates models using `brms::brm()`.

The issue `stanova` tries to address is that categorical variables with
\(k\) levels need to be transformed into \(k-1\) numerical model
coefficients. This poses a problem if a model includes a factor with
more than two levels that interacts with another variable. In this case,
the most reasonable parameterization of the model is such that the
intercept correspond to the (unweighted) grand mean and therefore
estimates of the coefficients for the main effects represent average
effects (compared to simple effects). In this parameterization, factors
with \(k\) levels, where \(k > 2\) (i.e., more than two levels), cannot
be mapped in a 1-to-1 fashion to the \(k-1\) coefficients as no such
mapping exists. Thus, the estimates of the model coefficients do not
represent effects of one factor level, but always pertain to more than
one factor level and thus cannot be directly interpreted. In other
words, these coefficients should not be looked at. Instead, `stanova`
transforms these parameters back such that one gets the information on
the factor levels (or combination of factor levels for interactions).
The default output shows for each factor level the difference from the
intercept which as discussed before corresponds to the (unweighted)
grand mean.

Another problem adressed by `stanova` is that for Bayesian models the
mapping of factor-levels to model coefficients needs to be done such
that the marginal prior for each factor-level is the same. If one uses
one of the contrast coding schemes commonly used in frequentist models
in which the intercept corresponds to the (unweighted) grand mean the
marginal prior differs across factor levels. For example, when using
`contr.sum` all but the last factor level are mapped to exactly one
model coefficient with positive sign, and the last factor level is
mapped with negative sign on all model coefficients. Thus, the marginal
prior for the last factor level is more diffuse than for the other
factor levels (if \(k >2\)). `stanova` per default uses the contrast
coding scheme suggested by Rouder, Morey, Speckman, and Province (2012)
which is such that the marginal prior is the same for all factor levels.
When using this contrast, the sum-to-zero constraint that is necessary
for the intercept to represent the (unweighted) grand mean is also
imposed. This contrast is implemented in the `contr.bayes()` function.

## Installation

For the moment, you can only install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("bayesstuff/stanova")
```

At least version `1.5.0` of `emmeans` is needed which can be installed
from CRAN:

``` r
install.packages("emmeans")
```

For the `brms` support, in addition to a C++ compiler, the latest
development version of `brms` is needed. This needs to be installed from
GitHub for now:

``` r
remotes::install_github("paul-buerkner/brms")
```

For the `rstanarm` fucntionality, `stanova` attaches the new contrasts
to the model instead of to the data if `rstanarm` version `2.21.2`,
which is not yet on CRAN, is installed from source:

``` r
Sys.setenv("MAKEFLAGS" = "-j4")  ## uses 4 cores during installation
remotes::install_github("stan-dev/rstanarm", build_vignettes = FALSE)
```

## `rstanarm` Example

The most basic example only uses a single factor with three levels to
demonstrate the output.

``` r
library(stanova)
data("Machines", package = "MEMSS")

m_machines <- stanova(score ~ Machine + (Machine|Worker),
                      model_fun = "glmer",
                      data=Machines, chains = 2,
                      warmup = 250, iter = 750)
```

The `summary` method of a `stanova` objects first provides some general
information about the fitted model (similar to `rstanarm`). It then
provides statistics about the intercept. The next block provides
estimates for each factor-level of the `Machine` factor. These estimates
represent the difference of the factor level against the intercept. For
example, for `Machine A`, the estimate is around `-7` suggesting that
the mean of this factor level is 7 points below the intercept (i.e., the
grand mean).

``` r
summary(m_machines)
#> 
#> Model Info:
#>  function:     stanova_glmer
#>  family:       gaussian [identity]
#>  formula:      score ~ Machine + (Machine | Worker)
#>  algorithm:    sampling
#>  chains:       2
#>  sample:       1000 (posterior sample size)
#>  priors:       see help('prior_summary', package = 'rstanarm')
#>  observations: 54
#>  groups:       Worker (6)
#> 
#> Estimate Intercept:
#>      Variable   Mean MAD_SD     5%    50%    95%  rhat ess_bulk ess_tail
#> 1 (Intercept) 59.822  1.741 56.774 59.815 62.846 1.007  301.177  419.510
#> 
#> 
#> Estimates 'Machine' - difference from intercept:
#>    Variable   Mean MAD_SD     5%    50%    95%  rhat ess_bulk ess_tail
#> 1 Machine A -7.232  1.252 -9.484 -7.184 -5.024 1.006  360.512  490.292
#> 2 Machine B  0.628  1.330 -1.598  0.587  3.000 1.011  392.514  523.328
#> 3 Machine C  6.603  1.091  4.467  6.608  8.563 1.005  347.099  342.109
```

If one is not interested in the differences from the factor levels, it
is also possible to obtain the estimates marginal means. For this one
just needs to set `diff_intercept = FALSE`.

``` r
summary(m_machines, diff_intercept = FALSE)
#> 
#> Model Info:
#>  function:     stanova_glmer
#>  family:       gaussian [identity]
#>  formula:      score ~ Machine + (Machine | Worker)
#>  algorithm:    sampling
#>  chains:       2
#>  sample:       1000 (posterior sample size)
#>  priors:       see help('prior_summary', package = 'rstanarm')
#>  observations: 54
#>  groups:       Worker (6)
#> 
#> Estimate Intercept:
#>      Variable   Mean MAD_SD     5%    50%    95%  rhat ess_bulk ess_tail
#> 1 (Intercept) 59.822  1.741 56.774 59.815 62.846 1.007  301.177  419.510
#> 
#> 
#> Estimates 'Machine' - marginal means:
#>    Variable   Mean MAD_SD     5%    50%    95%  rhat ess_bulk ess_tail
#> 1 Machine A 52.591  1.679 49.584 52.620 55.915 1.002  369.999  373.980
#> 2 Machine B 60.451  2.613 55.720 60.500 65.445 1.011  302.915  452.468
#> 3 Machine C 66.426  1.877 63.311 66.352 69.710 1.004  321.080  409.438
```

The key to the output is the `stanova_samples()` function which takes a
fitted model objects and returns the posterior samples transformed to
represent the difference from the intercept for each factor level (or
the marginal means if `diff_intercept = FALSE`). The default output is
an `array`, but this can be changed to a `matrix` or `data.frame` with
the `return` argument.

``` r
out_array <- stanova_samples(m_machines)
str(out_array)
#> List of 2
#>  $ (Intercept): num [1:500, 1, 1:2] 60.5 60 60.8 59.8 60 ...
#>   ..- attr(*, "dimnames")=List of 3
#>   .. ..$ Iteration: chr [1:500] "1" "2" "3" "4" ...
#>   .. ..$ Parameter: chr "(Intercept)"
#>   .. ..$ Chain    : chr [1:2] "1" "2"
#>  $ Machine    : num [1:500, 1:3, 1:2] -5.36 -7.86 -7.27 -7.66 -7.74 ...
#>   ..- attr(*, "dimnames")=List of 3
#>   .. ..$ Iteration: chr [1:500] "1" "2" "3" "4" ...
#>   .. ..$ Parameter: chr [1:3] "Machine A" "Machine B" "Machine C"
#>   .. ..$ Chain    : chr [1:2] "1" "2"
#>   ..- attr(*, "estimate")= chr "difference from intercept"
```

One can also change which dimension of the `array` represents the chain
via the `dimension_chain` argument.

``` r
out_array2 <- stanova_samples(m_machines, dimension_chain = 2)
str(out_array2)
#> List of 2
#>  $ (Intercept): num [1:500, 1:2, 1] 60.5 60 60.8 59.8 60 ...
#>   ..- attr(*, "dimnames")=List of 3
#>   .. ..$ Iteration: chr [1:500] "1" "2" "3" "4" ...
#>   .. ..$ Chain    : chr [1:2] "1" "2"
#>   .. ..$ Parameter: chr "(Intercept)"
#>  $ Machine    : num [1:500, 1:2, 1:3] -5.36 -7.86 -7.27 -7.66 -7.74 ...
#>   ..- attr(*, "dimnames")=List of 3
#>   .. ..$ Iteration: chr [1:500] "1" "2" "3" "4" ...
#>   .. ..$ Chain    : chr [1:2] "1" "2"
#>   .. ..$ Parameter: chr [1:3] "Machine A" "Machine B" "Machine C"
#>   ..- attr(*, "estimate")= chr "difference from intercept"
```

This makes it easy to produce plots via the `bayesplot` package on the
level of the differences from the intercept:

``` r
bayesplot::mcmc_trace(out_array2$Machine)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

## `brms` Example

We can use the same example for `brms`.

``` r
library(stanova)
data("Machines", package = "MEMSS")

m_machines_brm <- stanova_brm(score ~ Machine + (Machine|Worker),
                              data=Machines, chains = 2,
                              warmup = 250, iter = 750)
```

The `summary` methods works the same. The default shows the difference
from the intercept.

``` r
summary(m_machines_brm)
#> Warning: There were 2 divergent transitions after warmup. Increasing adapt_delta
#> above 0.8 may help. See http://mc-stan.org/misc/warnings.html#divergent-
#> transitions-after-warmup
#> 
#> Model Info:
#>  function:     brms::brm
#>  family:       gaussian(identity)
#>  formula:      score ~ Machine + (Machine | Worker)
#>  algorithm:    sampling
#>  chains:       2
#>  sample:       1000 (posterior sample size)
#>  priors:       Use brms::prior_summary(object) for prior information
#>  observations: 54
#>  groups:       Worker (6)
#> 
#> Estimate Intercept:
#>      Variable   Mean MAD_SD     5%    50%    95%  rhat ess_bulk ess_tail
#> 1 (Intercept) 59.915  2.169 56.284 59.900 64.193 1.004  430.852  379.244
#> 
#> 
#> Estimates 'Machine' - difference from intercept:
#>    Variable   Mean MAD_SD     5%    50%    95%  rhat ess_bulk ess_tail
#> 1 Machine A -7.394  1.223 -9.866 -7.338 -4.881 1.002  585.078  520.365
#> 2 Machine B  0.771  1.575 -2.195  0.797  3.642 1.006  478.505  557.933
#> 3 Machine C  6.623  1.413  3.951  6.579  9.436 1.004  621.441  634.558
```

And we can get the estimated marginal means using `diff_intercept =
FALSE`.

``` r
summary(m_machines_brm, diff_intercept = FALSE)
#> Warning: There were 2 divergent transitions after warmup. Increasing adapt_delta
#> above 0.8 may help. See http://mc-stan.org/misc/warnings.html#divergent-
#> transitions-after-warmup
#> 
#> Model Info:
#>  function:     brms::brm
#>  family:       gaussian(identity)
#>  formula:      score ~ Machine + (Machine | Worker)
#>  algorithm:    sampling
#>  chains:       2
#>  sample:       1000 (posterior sample size)
#>  priors:       Use brms::prior_summary(object) for prior information
#>  observations: 54
#>  groups:       Worker (6)
#> 
#> Estimate Intercept:
#>      Variable   Mean MAD_SD     5%    50%    95%  rhat ess_bulk ess_tail
#> 1 (Intercept) 59.915  2.169 56.284 59.900 64.193 1.004  430.852  379.244
#> 
#> 
#> Estimates 'Machine' - marginal means:
#>    Variable   Mean MAD_SD     5%    50%    95%  rhat ess_bulk ess_tail
#> 1 Machine A 52.521  2.207 48.114 52.549 56.595 1.003  496.900  395.046
#> 2 Machine B 60.686  3.208 54.787 60.851 66.645 1.007  431.779  514.153
#> 3 Machine C 66.538  2.417 62.450 66.501 71.325 1.007  459.667  440.038
```

# References

Rouder, J. N., Morey, R. D., Speckman, P. L., & Province, J. M. (2012).
Default Bayes factors for ANOVA designs. Journal of Mathematical
Psychology, 56(5), 356-374. <https://doi.org/10.1016/j.jmp.2012.08.001>
