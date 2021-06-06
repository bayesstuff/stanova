
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stanova

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/bayesstuff/stanova.svg?branch=master)](https://travis-ci.org/bayesstuff/stanova)
[![R build
status](https://github.com/bayesstuff/stanova/workflows/R-CMD-check/badge.svg)](https://github.com/bayesstuff/stanova/actions)
[![DOI](https://zenodo.org/badge/241858315.svg)](https://zenodo.org/badge/latestdoi/241858315)
<!-- badges: end -->

The goal of `stanova` is to provide a more relevant and interpretable
`summary` for Bayesian models with categorical covariates and possibly
interactions and continuous covariates estimated in `Stan`. The core
functions are `stanova()` which requires specifying which `rstanarm`
function should be called through argument `model_fun` (e.g.,
`model_fun = glmer` calls `stan_glmer` and allows fitting Bayesian mixed
models) and `stanova_brm()` which estimates models using `brms::brm()`.

The issue `stanova` tries to address is that categorical variables with
*k* levels need to be transformed into *k* − 1 numerical model
coefficients. This poses a problem if a model includes a factor with
more than two levels that interacts with another variable. In this case,
the most reasonable parameterization of the model is such that the
intercept correspond to the (unweighted) grand mean and therefore
estimates of the coefficients for the main effects represent average
effects (compared to simple effects). In this parameterization, factors
with *k* levels, where *k* &gt; 2 (i.e., more than two levels), cannot
be mapped in a 1-to-1 fashion to the *k* − 1 coefficients as no such
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
factor levels (if *k* &gt; 2). `stanova` per default uses the contrast
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

For the `brms` support, in addition to a C++ compiler, at least `brms`
version `2.15.0` is needed. This can be installed from CRAN for now:

``` r
install.packages("brms")
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
#> 1 (Intercept) 59.722  1.807 56.366 59.670 63.034 1.021  226.845  414.380
#> 
#> 
#> Estimates 'Machine' - difference from intercept:
#>    Variable   Mean MAD_SD     5%    50%    95%  rhat ess_bulk ess_tail
#> 1 Machine A -7.398  1.190 -9.570 -7.390 -5.265 1.002  293.763  440.176
#> 2 Machine B  0.789  1.215 -1.391  0.734  3.157 0.999  306.112  445.466
#> 3 Machine C  6.609  1.105  4.784  6.585  8.637 1.002  488.434  628.321
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
#> 1 (Intercept) 59.722  1.807 56.366 59.670 63.034 1.021  226.845  414.380
#> 
#> 
#> Estimates 'Machine' - marginal means:
#>    Variable   Mean MAD_SD     5%    50%    95%  rhat ess_bulk ess_tail
#> 1 Machine A 52.324  1.739 49.218 52.312 55.831 1.025  186.573  466.659
#> 2 Machine B 60.511  2.626 55.713 60.522 65.490 1.010  250.168  397.672
#> 3 Machine C 66.331  1.946 63.081 66.319 69.727 1.013  282.753  376.227
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
#>  $ (Intercept): num [1:500, 1, 1:2] 63.6 63.2 61.1 57.8 59.6 ...
#>   ..- attr(*, "dimnames")=List of 3
#>   .. ..$ Iteration: chr [1:500] "1" "2" "3" "4" ...
#>   .. ..$ Parameter: chr "(Intercept)"
#>   .. ..$ Chain    : chr [1:2] "1" "2"
#>  $ Machine    : num [1:500, 1:3, 1:2] -7.16 -6.93 -8.36 -5.85 -7.14 ...
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
#>  $ (Intercept): num [1:500, 1:2, 1] 63.6 63.2 61.1 57.8 59.6 ...
#>   ..- attr(*, "dimnames")=List of 3
#>   .. ..$ Iteration: chr [1:500] "1" "2" "3" "4" ...
#>   .. ..$ Chain    : chr [1:2] "1" "2"
#>   .. ..$ Parameter: chr "(Intercept)"
#>  $ Machine    : num [1:500, 1:2, 1:3] -7.16 -6.93 -8.36 -5.85 -7.14 ...
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
#> Warning: There were 1 divergent transitions after warmup. Increasing adapt_delta
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
#> 1 (Intercept) 59.987  2.356 55.798 59.980 64.233 1.005  391.109  378.052
#> 
#> 
#> Estimates 'Machine' - difference from intercept:
#>    Variable   Mean MAD_SD     5%    50%    95%  rhat ess_bulk ess_tail
#> 1 Machine A -7.275  1.376 -9.807 -7.339 -4.471 1.006  424.105  437.737
#> 2 Machine B  0.704  1.673 -2.304  0.725  3.698 1.003  494.681  544.226
#> 3 Machine C  6.570  1.481  3.991  6.559  9.155 1.000  541.001  568.342
```

And we can get the estimated marginal means using
`diff_intercept = FALSE`.

``` r
summary(m_machines_brm, diff_intercept = FALSE)
#> Warning: There were 1 divergent transitions after warmup. Increasing adapt_delta
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
#> 1 (Intercept) 59.987  2.356 55.798 59.980 64.233 1.005  391.109  378.052
#> 
#> 
#> Estimates 'Machine' - marginal means:
#>    Variable   Mean MAD_SD     5%    50%    95%  rhat ess_bulk ess_tail
#> 1 Machine A 52.713  2.384 48.547 52.630 57.334 1.004  481.561  434.842
#> 2 Machine B 60.692  3.382 54.488 60.605 66.948 1.001  387.032  347.542
#> 3 Machine C 66.557  2.640 62.258 66.454 71.246 1.001  354.172  419.227
```

# References

Rouder, J. N., Morey, R. D., Speckman, P. L., & Province, J. M. (2012).
Default Bayes factors for ANOVA designs. Journal of Mathematical
Psychology, 56(5), 356-374. <https://doi.org/10.1016/j.jmp.2012.08.001>
