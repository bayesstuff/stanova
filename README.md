
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
#> 1 (Intercept) 59.544  1.858 56.203 59.551 63.131 1.007  309.309  356.113
#> 
#> 
#> Estimates 'Machine' - difference from intercept:
#>    Variable   Mean MAD_SD     5%    50%    95%  rhat ess_bulk ess_tail
#> 1 Machine A -7.202  1.279 -9.609 -7.132 -5.012 1.006  394.708  467.065
#> 2 Machine B  0.663  1.311 -1.548  0.656  2.953 1.001  268.390  378.224
#> 3 Machine C  6.540  1.148  4.578  6.497  8.753 1.004  296.233  306.291
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
#> 1 (Intercept) 59.544  1.858 56.203 59.551 63.131 1.007  309.309  356.113
#> 
#> 
#> Estimates 'Machine' - marginal means:
#>    Variable   Mean MAD_SD     5%    50%    95%  rhat ess_bulk ess_tail
#> 1 Machine A 52.341  1.840 48.928 52.329 55.596 1.007  391.147  493.278
#> 2 Machine B 60.207  2.795 55.185 60.134 65.504 1.005  248.027  381.530
#> 3 Machine C 66.084  1.930 62.594 66.124 69.304 1.006  303.862  411.347
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
#>  $ (Intercept): num [1:500, 1, 1:2] 59.4 60.3 61.2 55.6 55.8 ...
#>   ..- attr(*, "dimnames")=List of 3
#>   .. ..$ Iteration: chr [1:500] "1" "2" "3" "4" ...
#>   .. ..$ Parameter: chr "(Intercept)"
#>   .. ..$ Chain    : chr [1:2] "1" "2"
#>  $ Machine    : num [1:500, 1:3, 1:2] -7.84 -8.01 -8.92 -5.95 -6.91 ...
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
#>  $ (Intercept): num [1:500, 1:2, 1] 59.4 60.3 61.2 55.6 55.8 ...
#>   ..- attr(*, "dimnames")=List of 3
#>   .. ..$ Iteration: chr [1:500] "1" "2" "3" "4" ...
#>   .. ..$ Chain    : chr [1:2] "1" "2"
#>   .. ..$ Parameter: chr "(Intercept)"
#>  $ Machine    : num [1:500, 1:2, 1:3] -7.84 -8.01 -8.92 -5.95 -6.91 ...
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
#> 1 (Intercept) 59.624  2.088 55.659 59.651 63.545 1.002  382.992  394.201
#> 
#> 
#> Estimates 'Machine' - difference from intercept:
#>    Variable   Mean MAD_SD     5%    50%    95%  rhat ess_bulk ess_tail
#> 1 Machine A -7.299  1.287 -9.633 -7.279 -4.741 1.002  579.371  734.007
#> 2 Machine B  0.667  1.493 -2.235  0.622  3.686 0.999  497.840  465.746
#> 3 Machine C  6.631  1.499  3.823  6.654  9.423 1.000  528.319  672.698
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
#> 1 (Intercept) 59.624  2.088 55.659 59.651 63.545 1.002  382.992  394.201
#> 
#> 
#> Estimates 'Machine' - marginal means:
#>    Variable   Mean MAD_SD     5%    50%    95%  rhat ess_bulk ess_tail
#> 1 Machine A 52.325  2.198 48.244 52.380 56.185 0.999  347.682  501.531
#> 2 Machine B 60.291  3.225 54.467 60.299 66.182 1.001  401.992  430.714
#> 3 Machine C 66.255  2.461 62.197 66.093 70.772 1.003  530.649  685.704
```

# References

Rouder, J. N., Morey, R. D., Speckman, P. L., & Province, J. M. (2012).
Default Bayes factors for ANOVA designs. Journal of Mathematical
Psychology, 56(5), 356-374. <https://doi.org/10.1016/j.jmp.2012.08.001>
