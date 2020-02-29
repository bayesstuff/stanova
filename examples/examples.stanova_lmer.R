
data("Machines", package = "MEMSS")

m_machines <- stanova_lmer(score ~ Machine + (Machine|Worker),
                           data=Machines, chains = 2, iter = 500)
summary(m_machines) ## default: difference from intercept

summary(m_machines, diff_intercept = FALSE) ## alt: marginal means

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


## with continuous variable
data(md_16.4, package = "afex")
md_16.4$cog <- scale(md_16.4$cog, scale=FALSE)


m_cont0 <- stanova_lmer(induct ~ cog + (cog|room:cond), md_16.4,
                        chains = 2, iter = 500)
summary(m_cont0)

# with interaction:
m_cont1 <- stanova_lmer(induct ~ cond*cog + (cog|room:cond), md_16.4,
                        chains = 2, iter = 500)
summary(m_cont1)

summary(m_cont1, diff_intercept = TRUE)

### glmer models

## binomial model
cbpp <- lme4::cbpp
cbpp$prob <- with(cbpp, incidence / size)
example_model <- stanova_glmer(prob ~ period + (1|herd),
                            data = cbpp, family = binomial,
                            weight = size,
                            chains = 2, cores = 1, seed = 12345, iter = 500)
summary(example_model)

\dontrun{
## to run this call in a function, make sure to quote() the weight argument:
tmp <- function() {
  cbpp <- lme4::cbpp
  cbpp$prob <- cbpp$incidence / cbpp$size
  example_model <- stanova_glmer(prob ~ period + (1|herd),
                                 data = cbpp, family = binomial,
                                 weight = quote(size),
                                 chains = 2, cores = 1,
                                 seed = 12345, iter = 500)
}
tmp()
}

## poisson model
data(Salamanders, package = "glmmTMB")
gm1 <- stanova_glmer(count~spp * mined + (1 | site), data = Salamanders,
                   family = "poisson",
                   chains = 2, cores = 1, seed = 12345, iter = 500)
summary(gm1)
