
fit_warp <- stanova(breaks ~ wool * tension, data = warpbreaks,
                    prior = rstanarm::student_t(3, 0, 3, autoscale = FALSE),
                    model_fun = "glm",
                    chains = 2, iter = 500)
summary(fit_warp)

####

data("Machines", package = "MEMSS")

## note: model_fun = "lmer" only works after library("rstanarm")
## but you can simply use model_fun = "glmer" with family = "gaussian"
m_machines <- stanova(score ~ Machine + (Machine|Worker),
                      model_fun = "glmer", family = "gaussian",
                      data=Machines, chains = 2, iter = 500)
summary(m_machines)

\dontrun{
## negative binomial
## requires attaching rstanarm
library("rstanarm")
fit6a <- stanova(Days ~ Sex/(Age + Eth*Lrn), data = MASS::quine,
                 model_fun = "glm.nb",
                 link = "log", prior_aux = exponential(1.5),
                 chains = 2, iter = 200) # for speed of example only
summary(fit6a)
}

