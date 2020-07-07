test_that("stanova_lmer basics", {
  skip_if_not_installed("afex")
  data("obk.long", package = "afex")

  suppressWarnings(capture.output(
    m2 <- stanova_lmer(value ~ treatment * phase + (1|id),
                       obk.long,
                       chains = 2, iter = 500)
  ))
  expect_is(m2, "stanova")
  ar <- stanova_samples(m2, return = "array")
  expect_is(ar, "list")
  expect_length(ar, 4)
  expect_is(ar$`(Intercept)`, "array")
  expect_length(dim(ar$`(Intercept)`), 3)
  expect_equal(dim(ar$`treatment:phase`), c(250, 9, 2))

})


test_that("binomial stanova_glmer", {
  skip_if_not_installed("lme4")
  cbpp <- lme4::cbpp
  cbpp$prob <- cbpp$incidence / cbpp$size
  suppressWarnings(capture.output(
    example_model <- stanova_glmer(prob ~ period + (1|herd),
                                   data = cbpp, family = binomial,
                                   weight = size,
                                   chains = 2, cores = 1,
                                   seed = 12345, iter = 500)
  ))
  expect_is(example_model, "stanova")
  ar <- stanova_samples(example_model, return = "array")
  expect_is(ar$`(Intercept)`, "array")
  expect_length(dim(ar$`(Intercept)`), 3)
  expect_equal(dim(ar$period), c(250, 4, 2))
})

test_that("poisson stanova_glmer", {
  skip_if_not_installed("glmmTMB")
  data(Salamanders, package = "glmmTMB")
  suppressWarnings(capture.output(
    gm1 <- stanova_glmer(count~spp * mined + (1 | site), data = Salamanders,
                         family = "poisson",
                         chains = 2, cores = 1, seed = 12345, iter = 500)
  ))
  expect_is(gm1, "stanova")
  ar <- stanova_samples(gm1, return = "array")
  expect_is(ar$`(Intercept)`, "array")
  expect_length(dim(ar$`(Intercept)`), 3)
  expect_equal(dim(ar$`spp:mined`), c(250, 14, 2))
})

