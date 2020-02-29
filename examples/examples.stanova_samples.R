

fit_warp <- stanova(breaks ~ wool * tension, data = warpbreaks,
                    prior = rstanarm::R2(0.5),
                    model_fun = "lm",
                    chains = 2, iter = 500)

arr_warp <- stanova_samples(fit_warp)
str(arr_warp)

mat_warp <- stanova_samples(fit_warp, return = "matrix")
str(mat_warp)

df_warp <- stanova_samples(fit_warp, return = "data.frame")
str(df_warp)

tail(df_warp$`wool:tension`)

if (requireNamespace("tibble")) {
  tbl_warp <- stanova_samples(fit_warp, return = "tibble")
  tbl_warp
}

if (require("tidybayes") && require("tidyverse")) {
  tidy_warp <- stanova_samples(fit_warp, return = "tidybayes")
  tidy_warp

  tidy_warp$tension %>%
    group_by(variable) %>%
    median_qi(value) %>%
    ggplot(aes(y = variable, x = value, xmin = .lower, xmax = .upper)) +
    geom_pointintervalh()

  tidy_warp$tension %>%
    ggplot(aes(y = variable, x = value)) +
    stat_halfeyeh()

  bind_rows(tidy_warp) %>%
    ggplot(aes(y = variable, x = value)) +
    stat_halfeyeh() +
    facet_wrap(vars(term), scales = "free")

   bind_rows(tidy_warp) %>%
    ggplot(aes(y = variable, x = value)) +
    stat_intervalh() +
    facet_wrap(vars(term), scales = "free")

   ## marginal means instead of differences from mean:
   stanova_samples(fit_warp, return = "tidybayes", diff_intercept = FALSE) %>%
     bind_rows() %>%
     ggplot(aes(y = variable, x = value)) +
     stat_halfeyeh() +
     facet_wrap(vars(term), scales = "free")
}


