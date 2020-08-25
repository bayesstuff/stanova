
\dontrun{
fit_warp_brms <- stanova_brm(breaks ~ wool * tension, data = warpbreaks,
                             chains = 2, iter = 1000)
summary(fit_warp_brms)
}
