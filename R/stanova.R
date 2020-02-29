#' Estimate ANOVA-type models with rstanarm
#'
#' @param formula a formula describing the statistical model to be estimated.
#'   Passed to the `rstanarm::stan_` function specified in `model_fun`.
#' @param data `data.frame` containing the data.
#' @param check_contrasts `character` string (of length 1) denoting a contrast
#'   function which should be assigned to all `character` and `factor` variables
#'   in the model (as long as the specified contrast is not the global default).
#'   Default is [contr.bayes]. Set to `NULL` to disable the check.
#' @param ... further arguments passed to the `rstanarm` function used for
#'   fitting. Typical arguments are `prior`, `prior_intercept`, `chain`, `iter`,
#'   or `core`. Ensure that arguments that cannot be evaluated in the given
#'   context (e.g., `weight` arguments to binomial models in which only an
#'   unquoted column name is passed) are enclosed in `quote()`.
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

  ## make call nicer for returned object
  call[[1]] <- str2lang("stanova")

  mout <-  do.call(
    what = getExportedValue("rstanarm",paste0("stan_", model_fun)),
    args = c(
      formula = formula,
      data = list(data),
      list(...)
    ))
  mout$call <- call
  mout$stan_function <- paste0("stanova_", model_fun)
  class(mout) <- c("stanova", class(mout))
  return(mout)
}

