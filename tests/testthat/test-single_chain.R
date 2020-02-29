test_that("Single chains work as expected", {
  testthat::skip_if_not_installed("MEMSS")
  data("Machines", package = "MEMSS")

  capture.output(m_machines_1 <- stanova_lmer(score ~ Machine + (Machine|Worker),
                           data=Machines, chains = 1, iter = 200))
  ar <- stanova_samples(m_machines_1, return = "array")
  expect_is(ar, "list")
  expect_length(ar, 2)
  expect_is(ar$`(Intercept)`, "array")
  expect_length(dim(ar$`(Intercept)`), 3)
  expect_equal(dim(ar$Machine), c(100, 3, 1))

  ar2 <- stanova_samples(m_machines_1, return = "array", dimension_chain = 2)
  expect_equal(dim(ar2$Machine), c(100, 1, 3))

  ma <- stanova_samples(m_machines_1, return = "matrix")
  expect_is(ma, "list")
  expect_length(ma, 2)
  expect_is(ma$`(Intercept)`, "matrix")
  expect_length(dim(ma$`(Intercept)`), 2)

  df <- stanova_samples(m_machines_1, return = "data.frame")
  expect_is(df$`(Intercept)`, "data.frame")
  expect_is(df$Machine, "data.frame")
})
