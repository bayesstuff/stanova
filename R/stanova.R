#' Estimate ANOVA-type models with rstanarm
#'
#' @param formula a formula describing the statistical model to be estimated.
#'   Passed to the `rstanarm::stan_` function specified in `model_fun`.
#' @param data `data.frame` containing the data.
#' @param model_fun character string identifying the `rstanarm` function that
#'   should be used for fitting (omitting the `stan_` prefix) such as `"glm"` or
#'   `"glmer"`.
#' @param check_contrasts `character` string (of length 1) denoting a contrast
#'   function which should be assigned to all `character` and `factor` variables
#'   in the model (as long as the specified contrast is not the global default).
#'   Default is [contr.bayes]. Set to `NULL` to disable the check.
#' @param pass_contrasts If `TRUE`, assigned (or checked) contrasts are passed
#'   to the modelling function as a `list`. If `FALSE`, assigned contrasts are
#'   added to the passed `data` object and this object is passed to the
#'   modelling function (i.e., `FALSE` changes the original data and `TRUE` does
#'   not).
#' @param ... further arguments passed to the `rstanarm` function used for
#'   fitting. Typical arguments are `prior`, `prior_intercept`, `chain`, `iter`,
#'   or `core`.
#'
#' @example examples/examples.stanova.R
#'
#' @export
stanova <- function(
  formula,
  data,
  model_fun,
  ...,
  check_contrasts = "contr.bayes",
  pass_contrasts = TRUE) {
  call <- match.call()
  orig_call <- call

  # if (model_fun == "lm") {
  #   message('model_fun = "lm" replaced with model_fun = "glm"')
  #   model_fun = "glm"
  #   call[["family"]] <- "gaussian"
  # }

  if (!is.null(check_contrasts)) {
    if (pass_contrasts) {
      contrasts_list <- create_contrasts_list(formula = formula, data = data,
                            new_contrast = check_contrasts)
    } else {
      data <- check_contrasts(formula = formula, data = data,
                              new_contrast = check_contrasts)
    }
  }


  ## prepare call for passing to rstanarm
  call[[1]] <- str2lang(paste0("rstanarm::stan_", model_fun))
  call[["model_fun"]] <- NULL
  if ("check_contrasts" %in% names(call)) {
    call[["check_contrasts"]] <- NULL
  }
    if ("pass_contrasts" %in% names(call)) {
    call[["pass_contrasts"]] <- NULL
  }
  if (pass_contrasts && !is.null(contrasts_list)) {
    call[["contrasts"]] <- contrasts_list
  } else {
    call[["data"]] <- data
  }
  mout <- eval.parent(call)

  ## prepare output object
  mout$call <- orig_call
  mout$stan_function <- paste0("stanova_", model_fun)
  class(mout) <- c("stanova", class(mout))
  return(mout)
}

