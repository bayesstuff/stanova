#' Estimate ANOVA-type models with rstanarm
#'
#' @param formula a formula describing the full mixed-model to be fitted. Passed
#'   to `rstanarm::stan_g/lmer`.
#' @param data `data.frame` containing the data.
#' @param check_contrasts `character` string (of length 1) denoting a contrast
#'   function which should be assigned to all `character` and `factor` variables
#'   in the model (as long as the specified contrast is not the global default).
#'   Default is [contr.bayes].
#' @param family `family` argument passed to `stan_glmer`.
#' @param ... further arguments passed to `rstanarm::stan_g/lmer`.
#' @example examples/examples.stanova_lmer.R
#'
#' @export
stanova_lmer <- function(
  formula,
  data,
  check_contrasts = "contr.bayes",
  ...) {
  call <- match.call(expand.dots = TRUE)

  if (!is.null(check_contrasts)) {
    data <- check_contrasts(formula = formula, data = data,
                            new_contrast = check_contrasts)
  }
  mout <- rstanarm::stan_glmer(
    formula = formula,
    data = data,
    family = "gaussian",
    ...)
  mout$call <- call
  mout$stan_function <- "stanova_lmer"
  class(mout) <- c("stanova", class(mout))
  return(mout)
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

  if (!is.null(check_contrasts)) {
    data <- check_contrasts(formula = formula, data = data,
                            new_contrast = check_contrasts)
  }
  call["check_contrasts"] <- NULL

  call[[1]] <- rstanarm::stan_glmer
  call[["data"]] <- data
  mout <- eval(call)
  mout$call <- call
  mout$stan_function <- "stanova_glmer"
  class(mout) <- c("stanova", class(mout))
  return(mout)
}

