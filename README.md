
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stanova

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/bayesstuff/stanova.svg?branch=master)](https://travis-ci.org/bayesstuff/stanova)
<!-- badges: end -->

The goal of `stanova` is to provide a more relevant and interpretable
`summary` for Bayesian models with categorical covariates and possibly
interactions. The core function is `stanova()` which requires specifying
which `rstanarm` function should be called through argument `model_fun`
(e.g., `model_fun = glmer` calls `stan_glmer` and allows fitting
Bayesian mixed models).

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
# install.packages("devtools")
devtools::install_github("bayesstuff/stanova")
```

## Example

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
#>      Variable Mean MAD_SD   5%  50%  95% rhat ess_bulk ess_tail
#> 1 (Intercept) 59.5   1.79 55.8 59.5 62.9 1.01      361      486
#> 
#> 
#> Estimates 'Machine' - difference from intercept:
#>    Variable   Mean MAD_SD    5%    50%   95% rhat ess_bulk ess_tail
#> 1 Machine A -7.257   1.18 -9.35 -7.244 -5.13 1.01      378      525
#> 2 Machine B  0.603   1.31 -1.73  0.545  2.91 1.00      378      523
#> 3 Machine C  6.654   1.20  4.40  6.655  8.88 1.01      345      304
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
#>      Variable Mean MAD_SD   5%  50%  95% rhat ess_bulk ess_tail
#> 1 (Intercept) 59.5   1.79 55.8 59.5 62.9 1.01      361      486
#> 
#> 
#> Estimates 'Machine' - marginal means:
#>    Variable Mean MAD_SD   5%  50%  95% rhat ess_bulk ess_tail
#> 1 Machine A 52.2   1.84 48.8 52.3 55.5 1.00      415      536
#> 2 Machine B 60.1   2.71 55.0 60.1 65.0 1.01      363      455
#> 3 Machine C 66.1   1.78 62.5 66.2 69.5 1.02      307      417
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
#>  $ (Intercept): num [1:500, 1, 1:2] 63.5 59.7 58.6 59.1 58.4 ...
#>   ..- attr(*, "dimnames")=List of 3
#>   .. ..$ Iteration: chr [1:500] "1" "2" "3" "4" ...
#>   .. ..$ Parameter: chr "(Intercept)"
#>   .. ..$ Chain    : chr [1:2] "chain:1" "chain:2"
#>  $ Machine    : num [1:500, 1:3, 1:2] -7.34 -7.39 -6.76 -7.75 -6.66 ...
#>   ..- attr(*, "dimnames")=List of 3
#>   .. ..$ Iteration: chr [1:500] "1" "2" "3" "4" ...
#>   .. ..$ Parameter: chr [1:3] "Machine A" "Machine B" "Machine C"
#>   .. ..$ Chain    : chr [1:2] "chain:1" "chain:2"
#>   ..- attr(*, "estimate")= chr "difference from intercept"
```

One can also change which dimension of the `array` represents the chain
via the `dimension_chain` argument.

``` r
out_array2 <- stanova_samples(m_machines, dimension_chain = 2)
str(out_array2)
#> List of 2
#>  $ (Intercept): num [1:500, 1:2, 1] 63.5 59.7 58.6 59.1 58.4 ...
#>   ..- attr(*, "dimnames")=List of 3
#>   .. ..$ Iteration: chr [1:500] "1" "2" "3" "4" ...
#>   .. ..$ Chain    : chr [1:2] "chain:1" "chain:2"
#>   .. ..$ Parameter: chr "(Intercept)"
#>  $ Machine    : num [1:500, 1:2, 1:3] -7.34 -7.39 -6.76 -7.75 -6.66 ...
#>   ..- attr(*, "dimnames")=List of 3
#>   .. ..$ Iteration: chr [1:500] "1" "2" "3" "4" ...
#>   .. ..$ Chain    : chr [1:2] "chain:1" "chain:2"
#>   .. ..$ Parameter: chr [1:3] "Machine A" "Machine B" "Machine C"
#>   ..- attr(*, "estimate")= chr "difference from intercept"
```

This makes it easy to produce plots via the `bayesplot` package on the
level of the differences from the intercept:

``` r
bayesplot::mcmc_trace(out_array2$Machine)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

# References

Rouder, J. N., Morey, R. D., Speckman, P. L., & Province, J. M. (2012).
Default Bayes factors for ANOVA designs. Journal of Mathematical
Psychology, 56(5), 356-374. <https://doi.org/10.1016/j.jmp.2012.08.001>
