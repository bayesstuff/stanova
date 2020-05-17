
library("tidyverse")

fit_warp <- stanova_lm(breaks ~ wool * tension, data = warpbreaks,
                    prior = rstanarm::student_t(3, 0, 3, autoscale = FALSE),
                    chains = 2, iter = 500)
summary(fit_warp)

fit_warp_prior <- stanova_lm(breaks ~ wool * tension, data = warpbreaks,
                    prior = rstanarm::student_t(3, 0, 3, autoscale = FALSE),
                    chains = 2, iter = 1000, prior_PD = TRUE)
summary(fit_warp_prior)

samp_post <- stanova_samples(fit_warp, return = "tidybayes")
samp_prior <- stanova_samples(fit_warp_prior, return = "tidybayes")


df_pred <- tibble(
  value = seq(-15, 15, length.out = 101)
) %>%
  mutate(density = extraDistr::dlst(value, df = 3, mu = 0,
                                    sigma = 3 * max(contr.bayes(2))))

samp_post$wool %>%
  ggplot(aes(value, after_stat(density))) +
  geom_histogram(bins = 50) +
  geom_density(data = samp_prior$wool) +
  geom_line(data = df_pred, aes(x = value, y = density, group = 1),
            color = "red") +
  facet_wrap("variable")

contr.bayes(3)
contr.bayes(5)
rowSums(contr.bayes(10))


df_pred <- tibble(
  value = seq(-15, 15, length.out = 101)
) %>%
  mutate(density = extraDistr::dlst(value, df = 3, mu = 0,
                                    sigma = 3 * max(rowSums(contr.bayes(3)))))
samp_post$tension %>%
  ggplot(aes(value, after_stat(density))) +
  geom_histogram(bins = 50) +
  geom_density(data = samp_prior$tension) +
  geom_line(data = df_pred, aes(x = value, y = density, group = 1),
            color = "red") +
  facet_wrap("variable")

df_pred <- tibble(
  value = seq(-15, 15, length.out = 101)
) %>%
  mutate(density = extraDistr::dlst(
    x = value,
    df = 3,
    mu = 0,
    sigma = 3 * max(rowSums(contr.bayes(6))))
  )

samp_post$`wool:tension` %>%
  ggplot(aes(value, after_stat(density))) +
  geom_histogram(bins = 50) +
  geom_density(data = samp_prior$`wool:tension`) +
  geom_line(data = df_pred, aes(x = value, y = density, group = 1),
            color = "red") +
  facet_wrap("variable")


