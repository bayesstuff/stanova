#' Estimate ANOVA-type models with rstanarm
#'
#' @param formula a formula describing the model to be fitted. Passed to
#'   `rstanarm::stan_glm`.
#' @param family `family` argument passed to `stan_glm` (set to `"gaussian"` for
#'   `stanova_lm` and `stanova_aov`).
#' @inheritParams stanova
#'
#' @example examples/examples.stanova_lm.R
#'
#' @note `stanova_aov` is a copy of `stanova_lm`. All functions discussed here
#'   are only wrappers around [`stanova`] setting `model_fun` to `"glm"` (and
#'   `family = "gaussian"` for `stanova_lm`).
#'
#' @export
stanova_lm <- function(
  formula,
  data,
  check_contrasts = "contr.bayes",
  ...) {
  mc <- match.call()
  mc[[1]] <- quote(stanova)
  mc[["model_fun"]] <- "glm"
  mc[["family"]] <- "gaussian"
  eval.parent(mc)
}

#' @rdname stanova_lm
#' @export
stanova_aov <- stanova_lm

#' @rdname stanova_lm
#' @export
stanova_glm <- function(
  formula,
  data,
  family,
  check_contrasts = "contr.bayes",
  ...) {
  mc <- match.call()
  mc[[1]] <- quote(stanova)
  mc[["model_fun"]] <- "glm"
  eval.parent(mc)
}

