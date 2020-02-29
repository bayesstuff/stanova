
library("rstanarm")
m1 <- stanova(yield ~ block + N*P*K, npk, prior = R2(0.5))
m1
summary(m1)

fit_warp <- stanova(breaks ~ wool * tension, data = warpbreaks, prior = R2(0.5))
summary(fit_warp)



## negative binomial
fit6a <- stanova(Days ~ Sex/(Age + Eth*Lrn), data = MASS::quine,
                 model_fun = "glm.nb",
                 link = "log", prior_aux = exponential(1.5),
                 chains = 2, iter = 200) # for speed of example only
summary(fit6a)

## beta regression (fails somehow)
data("GasolineYield", package = "betareg")
str(GasolineYield)

fit_beta <- stanova(yield ~ batch + temp, data = GasolineYield,
                    model_fun = "betareg",
                    link = "logit", link.phi = "log",
                    chains = 2, iter = 500)
summary(fit_beta)
