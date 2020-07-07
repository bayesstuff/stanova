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
  mc <- match.call()
  mc[[1]] <- quote(stanova)
  mc[["model_fun"]] <- "glmer"
  mc[["family"]] <- "gaussian"
  eval.parent(mc)
}

#' @rdname stanova_lmer
#' @export
stanova_glmer <- function(
  formula,
  data,
  family,
  check_contrasts = "contr.bayes",
  ...) {
  mc <- match.call()
  mc[[1]] <- quote(stanova)
  mc[["model_fun"]] <- "glmer"
  eval.parent(mc)
}

