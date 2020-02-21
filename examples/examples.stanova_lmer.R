
data("Machines", package = "MEMSS")

m_machines <- stanova_lmer(score ~ Machine + (Machine|Worker),
                           data=Machines, chains = 2, iter = 500)
summary(m_machines)

out_array <- stanova_samples(m_machines)
str(out_array)

out_array2 <- stanova_samples(m_machines, dimension_chain = 2)
str(out_array2)

bayesplot::mcmc_trace(out_array2$`(Intercept)`)
bayesplot::mcmc_trace(out_array2$Machine)

out_df <- stanova_samples(m_machines, return = "data.frame")
str(out_df)

data("obk.long", package = "afex")

m2 <- stanova_lmer(value ~ treatment * phase + (1|id), obk.long,
                   chains = 2, iter = 500)

m2
summary(m2)


### glmer models

cbpp <- lme4::cbpp
cbpp$prob <- with(cbpp, incidence / size)
example_model <- stanova_glmer(prob ~ period + (1|herd),
                            data = cbpp, family = binomial,
                            weight = size,
                            chains = 2, cores = 1, seed = 12345, iter = 500)
summary(example_model)
