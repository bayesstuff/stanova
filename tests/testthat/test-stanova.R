test_that("Basics Works without Warnings", {
  capture.output(
    fit_warp <- stanova(breaks ~ wool * tension, data = warpbreaks,
                        #prior = rstanarm::student_t(3, 0, 3, autoscale = FALSE),
                        model_fun = "glm",
                        chains = 2, iter = 1000)
  )

  expect_is(fit_warp, "stanova")
  ar <- stanova_samples(fit_warp, return = "array")
  expect_is(ar, "list")
  expect_length(ar, 4)
  expect_is(ar$`(Intercept)`, "array")
  expect_length(dim(ar$`(Intercept)`), 3)
  expect_equal(dim(ar$`wool:tension`), c(500, 6, 2))

  sum1 <- summary(fit_warp, diff_intercept = TRUE)
  expect_equivalent(sum1$`(Intercept)`$Mean, 28.1, tolerance = 0.4)

  sum2 <- summary(fit_warp, diff_intercept = FALSE)

  modlm <- lm(breaks ~ wool * tension, data = warpbreaks,
              contrasts = list(wool = "contr.sum", tension = "contr.sum"))

  expect_equivalent(sum2$`(Intercept)`$Mean,
                    coef(modlm,)[1],
                    tolerance = 1, scale = 1)
  expect_equivalent(sum2$`wool:tension`$Mean,
                    summary(emmeans::emmeans(modlm, c("wool", "tension")))$emmean,
                    tolerance = 1, scale = 1)
  expect_equivalent(sum2$`wool:tension`$Mean[1], 45.5,
                    tolerance = 1, scale = 1)

})

test_that("Binomial GLM works", {

  ## binomial model
  ### from: ?predict.glm
  ## example from Venables and Ripley (2002, pp. 190-2.)
  dfbin <- data.frame(
    ldose = rep(0:5, 2),
    numdead = c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16),
    sex = factor(rep(c("M", "F"), c(6, 6)))
  )
  capture.output(
    budworm.lg <- stanova(cbind(numdead, numalive = 20-numdead) ~ sex*ldose,
                          data = dfbin,
                          model_fun = "glm",
                          family = binomial,
                          chains = 2, iter = 1000)
  )
  sum1 <- summary(budworm.lg, diff_intercept = FALSE)
  modglm <- glm(cbind(numdead, numalive = 20-numdead) ~ sex*ldose,
                data = dfbin, family = binomial,
                contrasts = list(sex = "contr.sum"))
  summary(modglm)

  expect_equivalent(sum1$`(Intercept)`$Mean,
                    coef(modglm)[1],
                    tolerance = 0.5, scale = 1)
  expect_equivalent(sum1$sex$Mean,
                    summary(emmeans::emmeans(modglm, c("sex")))$emmean,
                    tolerance = 0.3, scale = 1)

  expect_equivalent(sum1$ldose$Mean,
                    summary(emmeans::emtrends(modglm, var = "ldose",
                                              specs = "1"))$ldose.trend,
                    tolerance = 0.3, scale = 1)
})


test_that("Negative Binomial GLM works", {

  library("rstanarm")

  capture.output(
    fit6a <- stanova(Days ~ Sex/(Age + Eth*Lrn), data = MASS::quine,
                 model_fun = "glm.nb",
                 link = "log", prior_aux = exponential(1.5),
                 chains = 2, iter = 1000)

  )
  sum1 <- summary(fit6a)
  expect_equivalent(sum1$`(Intercept)`$Mean, 2.77,
                    tolerance = 0.5, scale = 1)
  expect_equivalent(sum1$Sex$Mean, c(-0.075, 0.035),
                    tolerance = 0.1, scale = 1)
})

test_that("only continuous works", {
  skip_if_not_installed("afex")
  ## with continuous variable
  data(md_16.4, package = "afex")
  md_16.4$cog <- scale(md_16.4$cog, scale=FALSE)

  capture.output(
    m_cont0 <- stanova(induct ~ cog, md_16.4,
                       model_fun = "glm",
                       chains = 2, iter = 1000)
  )
  sum1 <- summary(m_cont0)
  expect_equivalent(sum1$`(Intercept)`$Mean, 31.7,
                    tolerance = 0.2, scale = 1)
  expect_equivalent(sum1$cog$Mean, 0.7,
                    tolerance = 0.2, scale = 1)
})
