test_that("stanova_samples return behaviour", {

  chains <- 2
  post_samp <- 250
  capture.output(
    mod <- stanova(breaks ~ wool * tension, data = warpbreaks,
                    prior = rstanarm::R2(0.5),
                   model_fun = "lm",
                   chains = chains, cores = 1,
                   seed = 12345, iter = post_samp*2)
  )
  ar <- stanova_samples(mod, return = "array")
  expect_is(ar, "list")
  expect_is(ar[[2]], "array")
  expect_equal(dim(ar$`wool:tension`), c(post_samp, 6, chains))

  mat <- stanova_samples(mod, return = "matrix")
  expect_is(mat, "list")
  expect_is(mat[[2]], "matrix")
  expect_equal(dim(mat$`wool:tension`), c(post_samp * chains, 6))

  df <- stanova_samples(mod, return = "data.frame")
  expect_is(df, "list")
  expect_is(df[[2]], "data.frame")
  expect_equal(dim(df$`wool:tension`), c(post_samp * chains * 6, 4))

  testthat::skip_if_not_installed("tibble")

  tbl <- stanova_samples(mod, return = "tibble")
  expect_is(tbl, "list")
  expect_is(tbl[[2]], "tbl_df")
  expect_equal(dim(tbl$`wool:tension`), c(post_samp * chains * 6, 4))

})
