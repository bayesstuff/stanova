

#################################################################
##                  Regular Regression Models                  ##
#################################################################

# ANOVA
fit_warp <- stanova(breaks ~ wool * tension, data = warpbreaks,
                    prior = rstanarm::student_t(3, 0, 20, autoscale = FALSE),
                    model_fun = "glm",
                    chains = 2, iter = 500)

summary(fit_warp) ## difference from intercept
summary(fit_warp, diff_intercept = FALSE)  ## marginal means

\dontrun{ ## for speed reasons
## binomial model
### from: ?predict.glm
## example from Venables and Ripley (2002, pp. 190-2.)
dfbin <- data.frame(
  ldose = rep(0:5, 2),
  numdead = c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16),
  sex = factor(rep(c("M", "F"), c(6, 6)))
)
budworm.lg <- stanova(cbind(numdead, numalive = 20-numdead) ~ sex*ldose,
                      data = dfbin,
                      model_fun = "glm",
                      family = binomial,
                      chains = 2, iter = 500)
## note: only sex is categorical, ldose is continuous
summary(budworm.lg)


## negative binomial
## requires attaching rstanarm
library("rstanarm")
fit6a <- stanova(Days ~ Sex/(Age + Eth*Lrn), data = MASS::quine,
                 model_fun = "glm.nb",
                 link = "log", prior_aux = exponential(1.5),
                 chains = 2, iter = 200) # for speed of example only
summary(fit6a)




##################################################################
##                     Mixed Effects Models                     ##
##################################################################

data("Machines", package = "MEMSS")

## note: model_fun = "lmer" only works after library("rstanarm")
## but you can simply use model_fun = "glmer" with family = "gaussian"
m_machines <- stanova(score ~ Machine + (Machine|Worker),
                      model_fun = "glmer", family = "gaussian",
                      data=Machines, chains = 2, iter = 500)
summary(m_machines)


## binomial model
cbpp <- lme4::cbpp
cbpp$prob <- with(cbpp, incidence / size)
example_model <- stanova(prob ~ period + (1|herd),
                         data = cbpp, family = binomial,
                         model_fun = "glmer",
                         weight = size,
                         chains = 2, cores = 1, seed = 12345, iter = 500)
summary(example_model)
}

## poisson model
data(Salamanders, package = "glmmTMB")
gm1 <- stanova(count~spp * mined + (1 | site), data = Salamanders,
               model_fun = "glmer", family = "poisson",
               chains = 2, cores = 1, seed = 12345, iter = 500)
summary(gm1)



