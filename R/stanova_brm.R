#' Estimate ANOVA-type models with brms
#'
#' @inheritParams stanova
#'
#' @example examples/examples.stanova_brm.R
#'
#' @export
stanova_brm <- function(
  formula,
  data,
  model_fun,
  ...,
  check_contrasts = "contr.bayes"
) {
  if (!requireNamespace("brms")) {
    stop("package brms is required.", call. = FALSE)
  }
  call <- match.call()
  orig_call <- call

  # if (model_fun == "lm") {
  #   message('model_fun = "lm" replaced with model_fun = "glm"')
  #   model_fun = "glm"
  #   call[["family"]] <- "gaussian"
  # }

  if (!is.null(check_contrasts)) {
    data <- check_contrasts(formula = as.formula(formula),
                            data = data,
                            new_contrast = check_contrasts)
  }


  ## prepare call for passing to rstanarm
  call[[1]] <- quote(brms::brm)
  if ("check_contrasts" %in% names(call)) {
    call[["check_contrasts"]] <- NULL
  }
  call[["data"]] <- data
  mout <- eval.parent(call)

  #browser()

  ## prepare output object
  mout$call <- orig_call
  class(mout) <- c("stanova", class(mout))
  return(mout)
}

