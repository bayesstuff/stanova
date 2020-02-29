#' Estimate ANOVA-type mixed models with rstanarm
#'
#' @param formula a formula describing the full mixed-model to be fitted. Passed
#'   to `rstanarm::stan_g/lmer`.
#' @param family `family` argument passed to `stan_glmer`.
#' @inheritParams stanova
#'
#' @example examples/examples.stanova_lmer.R
#'
#' @note These functions are only wrappers around [`stanova`] setting
#'   `model_fun` to `"glmer"` (and `family = "gaussian"` for `stanova_lmer`).
#'
#' @export
stanova_lmer <- function(
  formula,
  data,
  check_contrasts = "contr.bayes",
  ...) {
  call <- match.call(expand.dots = TRUE)
  call[[1]] <- stanova
  call["family"] <- "gaussian"
  call["model_fun"] <- "glmer"
  out <- eval(call)
  out$stan_function <- "stanova_lmer"
  return(out)
}

#' @rdname stanova_lmer
#' @export
stanova_glmer <- function(
  formula,
  data,
  family,
  check_contrasts = "contr.bayes",
  ...) {
  call <- match.call(expand.dots = TRUE)
  call[[1]] <- stanova
  call["model_fun"] <- "glmer"
  eval(call)
}

