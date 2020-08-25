test_that("brms models work", {

  tmp <- capture.output(
    fit_warp_brms <- stanova_brm(breaks ~ wool * tension, data = warpbreaks,
                                 chains = 2, iter = 1000)
  )

  expect_is(fit_warp_brms, "stanova")
  ar <- stanova_samples(fit_warp_brms, return = "array")
  expect_is(ar, "list")
  expect_length(ar, 4)
  expect_is(ar$`(Intercept)`, "array")
  expect_length(dim(ar$`(Intercept)`), 3)
  expect_equal(dim(ar$`wool:tension`), c(500, 6, 2))

  df <- stanova_samples(fit_warp_brms, return = "data.frame")
  expect_is(df, "list")
  expect_length(df, 4)
  expect_is(df$`(Intercept)`, "data.frame")
  expect_length(dim(df$`(Intercept)`), 2)
  expect_equal(dim(df$`wool:tension`), c(6000, 6))
  expect_equal(df$`wool:tension`$Chain, rep(1:2, each = 3000))
  expect_equal(df$`wool:tension`$Iteration,
               rep(seq_len(nrow(df$`(Intercept)`)/2), 6*2))


  sum1 <- summary(fit_warp_brms, diff_intercept = TRUE)
  expect_equivalent(sum1$`(Intercept)`$Mean, 28.1, tolerance = 0.4)

  sum2 <- summary(fit_warp_brms, diff_intercept = FALSE)

  modlm <- lm(breaks ~ wool * tension, data = warpbreaks,
              contrasts = list(wool = "contr.sum", tension = "contr.sum"))

  expect_equivalent(sum2$`(Intercept)`$Mean,
                    coef(modlm,)[1],
                    tolerance = 1, scale = 1)
  expect_equivalent(sum2$`wool:tension`$Mean,
                    summary(emmeans::emmeans(modlm, c("wool", "tension")))$emmean,
                    tolerance = 1, scale = 1)
  expect_equivalent(sum2$`wool:tension`$Mean[1], 45.5,
                    tolerance = 2, scale = 1)
})



test_that("Binomial GLM brms works", {


  ## binomial model
  ### from: ?predict.glm
  ## example from Venables and Ripley (2002, pp. 190-2.)
  dfbin <- data.frame(
    ldose = rep(0:5, 2),
    numdead = c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16),
    sex = factor(rep(c("M", "F"), c(6, 6)))
  )
  dfbin$n <- 20
  capture.output(
    budworm.lg_brm <- stanova_brm(numdead | trials(n) ~ sex*ldose,
                                  data = dfbin,
                                  family = binomial,
                                  chains = 2, iter = 1000)
  )


  sum1 <- summary(budworm.lg_brm, diff_intercept = FALSE)
  modglm <- glm(cbind(numdead, numalive = 20-numdead) ~ sex*ldose,
                data = dfbin, family = binomial,
                contrasts = list(sex = "contr.sum"))
  #summary(modglm)

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

  sum2 <- summary(budworm.lg_brm, diff_intercept = TRUE)
  expect_equivalent(sum2$`(Intercept)`$Mean,
                    coef(modglm)[1],
                    tolerance = 0.5, scale = 1)

  expect_equivalent(sum2$sex$Mean,
                    summary(emmeans::emmeans(modglm, c("sex")))$emmean - coef(modglm)[1],
                    tolerance = 0.3, scale = 1)
  expect_equivalent(sum2$ldose$Mean,
                    summary(emmeans::emtrends(modglm, var = "ldose",
                                              specs = "1"))$ldose.trend,
                    tolerance = 0.3, scale = 1)
})

