
\dontrun{
fit_warp_brms <- stanova_brm(breaks ~ wool * tension, data = warpbreaks,
                             chains = 2, iter = 1000)
summary(fit_warp_brms)
}

\dontrun{
### binomial GLMM

data("stroop", package = "afex")
## for frequentist analysis see:
### https://cran.r-project.org/package=afex/vignettes/afex_analysing_accuracy_data.html
library("tidyverse")
stroop_e1 <- stroop %>%
  filter(!is.na(acc)) %>%
  filter(study == "1") %>%
  droplevels() %>%
  group_by(condition, congruency, pno) %>%
  summarise(
    succ = sum(acc),
    acc = mean(acc),
    n = n())

bglmm_stroop <- stanova_brm(
  succ | trials(n) ~ congruency*condition + (congruency*condition|pno),
  data = stroop_e1, family = binomial,
  chains = 2, iter = 1000
)
summary(bglmm_stroop)
}
