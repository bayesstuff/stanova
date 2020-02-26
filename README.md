
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stanova

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/bayesstuff/stanova.svg?branch=master)](https://travis-ci.org/bayesstuff/stanova)
<!-- badges: end -->

The goal of `stanova` is to provide a more relevant and interpretable
`summary` for Bayesian models with categorical covariates and possibly
interactions. At the moment, it only provides wrappers for `rstanarm`
via functions `stanova_lmer` and `stanova_glmer`.

The issue `stanova` tries to address is that categorical variables with
\(k\) levels need to be transformed into \(k-1\) numerical model
coefficients. This poses a problem if a model includes a factor with
more than two levels that interacts with another variable. In this case,
the most reasonable parameterization of the model is such that the
intercept correspond to the (unweighted) grand mean and therefore tests
of main effects represent average effects (compared to simple effects).
In this parameterization, factors with \(k\) levels, where \(k > 2\)
(i.e., more than two levels), cannot be mapped in a 1-to-1 fashion to
the \(k-1\) coefficients as no such mapping exists. Thus, the values of
the model coefficients do not represent effects of one factor level, but
always pertain to more than one factor level and thus cannot be directly
interpreted. In other words, these coefficients should not be looked at.
Instead, `stanova` transforms these parameters back such that one gets
the information on the factor levels (or combination of factor levels
for interactions). The default output shows for each factor level the
difference from the intercept which as discussed before corresponds to
the (unweighted) grand mean.

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
factor levels (if \(k >2\)). `stanova` per default uses the prior
suggested by Rouder, Morey, Speckman, and Province (2012) in which the
marginal prior is the same for all factor levels. In this prior, the
sum-to-zero constraint that is necessary for the intercept to represent
the (unweighted) grand mean is also imposed. This contrast is
implemented in the `contr.bayes()` function.

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

m_machines <- stanova_lmer(score ~ Machine + (Machine|Worker),
                           data=Machines, chains = 2, iter = 500)
```

The `summary` method of a `staonva` objects first provides some general
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
#>  function:     stanova_lmer
#>  family:       gaussian [identity]
#>  formula:      score ~ Machine + (Machine | Worker)
#>  algorithm:    sampling
#>  chains:       2
#>  sample:       500 (posterior sample size)
#>  priors:       see help('prior_summary', package = 'rstanarm')
#>  observations: 54
#>  groups:       Worker (6)
#> 
#> Estimate Intercept:
#>      Variable Mean MAD_SD   5%  50%  95% rhat ess_bulk ess_tail
#> 1 (Intercept) 59.3    1.9 56.3 59.5 62.2    1      159      271
#> 
#> 
#> Estimates 'Machine' - difference from intercept:
#>    Variable   Mean MAD_SD    5%    50%   95% rhat ess_bulk ess_tail
#> 1 Machine A -7.314   1.20 -9.48 -7.285 -5.46 1.00      255      228
#> 2 Machine B  0.646   1.25 -1.47  0.667  2.86 1.01      162      207
#> 3 Machine C  6.668   1.21  4.57  6.693  8.71 1.01      224      267
```

If one is not interested in the differences from the factor levels, it
is also possible to obtain the estimates marginal means. For this one
just needs to set `diff_intercept = FALSE`.

``` r
summary(m_machines, diff_intercept = FALSE)
#> 
#> Model Info:
#>  function:     stanova_lmer
#>  family:       gaussian [identity]
#>  formula:      score ~ Machine + (Machine | Worker)
#>  algorithm:    sampling
#>  chains:       2
#>  sample:       500 (posterior sample size)
#>  priors:       see help('prior_summary', package = 'rstanarm')
#>  observations: 54
#>  groups:       Worker (6)
#> 
#> Estimate Intercept:
#>      Variable Mean MAD_SD   5%  50%  95% rhat ess_bulk ess_tail
#> 1 (Intercept) 59.3    1.9 56.3 59.5 62.2    1      159      271
#> 
#> 
#> Estimates 'Machine' - marginal means:
#>    Variable Mean MAD_SD   5%  50%  95%  rhat ess_bulk ess_tail
#> 1 Machine A   52   1.71 49.0 52.1 55.1 1.003      212      314
#> 2 Machine B   60   2.97 55.6 60.2 64.1 1.003      144      191
#> 3 Machine C   66   1.73 62.6 66.0 69.3 0.997      201      272
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
#>  $ (Intercept): num [1:250, 1, 1:2] 57.7 59.9 60.6 61.2 59.6 ...
#>   ..- attr(*, "dimnames")=List of 3
#>   .. ..$ Iteration: chr [1:250] "1" "2" "3" "4" ...
#>   .. ..$ Parameter: chr "(Intercept)"
#>   .. ..$ Chain    : chr [1:2] "chain:1" "chain:2"
#>  $ Machine    : num [1:250, 1:3, 1:2] -6.38 -8.17 -7.86 -8.12 -7.63 ...
#>   ..- attr(*, "dimnames")=List of 3
#>   .. ..$ Iteration: chr [1:250] "1" "2" "3" "4" ...
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
#>  $ (Intercept): num [1:250, 1:2, 1] 57.7 59.9 60.6 61.2 59.6 ...
#>   ..- attr(*, "dimnames")=List of 3
#>   .. ..$ Iteration: chr [1:250] "1" "2" "3" "4" ...
#>   .. ..$ Chain    : chr [1:2] "chain:1" "chain:2"
#>   .. ..$ Parameter: chr "(Intercept)"
#>  $ Machine    : num [1:250, 1:2, 1:3] -6.38 -8.17 -7.86 -8.12 -7.63 ...
#>   ..- attr(*, "dimnames")=List of 3
#>   .. ..$ Iteration: chr [1:250] "1" "2" "3" "4" ...
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
Psychology, 56(5), 356â€“374.
<https://doi.org/10.1016/j.jmp.2012.08.001>
