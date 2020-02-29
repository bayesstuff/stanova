#' Estimate ANOVA-type models with rstanarm
#'
#' @param formula a formula describing the statistical model to be estimated.
#'   Passed to the `rstanarm::stan_` function specified in `model_fun`.
#' @param data `data.frame` containing the data.
#' @param check_contrasts `character` string (of length 1) denoting a contrast
#'   function which should be assigned to all `character` and `factor` variables
#'   in the model (as long as the specified contrast is not the global default).
#'   Default is [contr.bayes]. Set to `NULL` to disable the check.
#' @param ... further arguments passed to the specified `rstanarm::stan_`
#'   function.
#' @param model_fun character string identifying the `rstanarm` function that
#'   should be used for fitting (omitting the `stan_` prefix) such as `"glm"` or
#'   `"lmer"`.
#'
#' @example examples/examples.stanova.R
#'
#' @export
stanova <- function(
  formula,
  data,
  model_fun,
  ...,
  check_contrasts = "contr.bayes") {
  call <- match.call(expand.dots = TRUE)

  if (!is.null(check_contrasts)) {
    data <- check_contrasts(formula = formula, data = data,
                            new_contrast = check_contrasts)
  }
  call["check_contrasts"] <- NULL
  call["model_fun"] <- NULL

  call[[1]] <- str2lang(paste0("rstanarm::stan_", model_fun))
  #call[[1]] <- getFromNamespace(paste0("stan_", model_fun), ns = "rstanarm")
  call[["data"]] <- data
  mout <- eval(call)
  mout$call <- call
  mout$stan_function <- paste0("stanova_", model_fun)
  class(mout) <- c("stanova", class(mout))
  return(mout)
}

