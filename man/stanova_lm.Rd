% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stanova_lm.R
\name{stanova_lm}
\alias{stanova_lm}
\alias{stanova_aov}
\alias{stanova_glm}
\title{Estimate ANOVA-type models with rstanarm}
\usage{
stanova_lm(formula, data, check_contrasts = "contr.bayes", ...)

stanova_aov(formula, data, check_contrasts = "contr.bayes", ...)

stanova_glm(formula, data, family, check_contrasts = "contr.bayes", ...)
}
\arguments{
\item{formula}{a formula describing the model to be fitted. Passed to
\code{rstanarm::stan_glm}.}

\item{data}{\code{data.frame} containing the data.}

\item{check_contrasts}{\code{character} string (of length 1) denoting a contrast
function or a contrast function which should be assigned to all \code{character}
and \code{factor} variables in the model (as long as the specified contrast is
not the global default). Default is \link{contr.bayes}. Set to \code{NULL} to disable
the check.}

\item{...}{further arguments passed to the \code{rstanarm} function used for
fitting. Typical arguments are \code{prior}, \code{prior_intercept}, \code{chain}, \code{iter},
or \code{core}.}

\item{family}{\code{family} argument passed to \code{stan_glm} (set to \code{"gaussian"} for
\code{stanova_lm} and \code{stanova_aov}).}
}
\description{
Estimate ANOVA-type models with rstanarm
}
\note{
\code{stanova_aov} is a copy of \code{stanova_lm}. All functions discussed here
are only wrappers around \code{\link{stanova}} setting \code{model_fun} to \code{"glm"} (and
\code{family = "gaussian"} for \code{stanova_lm}).
}
\examples{

fit_warp <- stanova_lm(breaks ~ wool * tension, data = warpbreaks,
                    prior = rstanarm::student_t(3, 0, 20, autoscale = FALSE),
                    chains = 2, iter = 500)
summary(fit_warp)

### from: ?predict.glm
## example from Venables and Ripley (2002, pp. 190-2.)
dfbin <- data.frame(
  ldose = rep(0:5, 2),
  numdead = c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16),
  sex = factor(rep(c("M", "F"), c(6, 6)))
)
budworm.lg <- stanova_glm(cbind(numdead, numalive = 20-numdead) ~ sex*ldose,
                          data = dfbin,
                          family = binomial,
                          chains = 2, iter = 500)
## note: only sex is categorical, ldose is continuous
summary(budworm.lg)
}
