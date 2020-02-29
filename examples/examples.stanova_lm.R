
fit_warp <- stanova_lm(breaks ~ wool * tension, data = warpbreaks,
                    prior = rstanarm::student_t(3, 0, 3, autoscale = FALSE),
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
