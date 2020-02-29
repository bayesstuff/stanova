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
#' @param model_fun character string identifying the `rstanarm` function that
#'   should be used for fitting (omitting the `stan_` prefix).
#'
#' @example examples/examples.stanova.R
#'
#' @export
stanova <- function(
  formula,
  data,
  model_fun = "aov",
  ...,
  check_contrasts = "contr.bayes") {
  call <- match.call(expand.dots = TRUE)

  if (!is.null(check_contrasts)) {
    data <- check_contrasts(formula = formula, data = data,
                            new_contrast = check_contrasts)
  }
  call["check_contrasts"] <- NULL
  call["model_fun"] <- NULL

  call[[1]] <- getFromNamespace(paste0("stan_", model_fun), ns = "rstanarm")
  call[["data"]] <- data
  mout <- eval(call)
  mout$call <- call
  mout$stan_function <- paste0("stanova_", model_fun)
  class(mout) <- c("stanova", class(mout))
  return(mout)
}

