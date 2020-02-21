#' Estimate ANOVA-type models with rstanarm
#'
#' @param formula a formula describing the full mixed-model to be fitted. Passed
#'   to [`rstanarm::stan_lmer()`].
#' @param data `data.frame` containing the data.
#' @param check_contrasts `character` string (of length 1) denoting a contrast
#'   function which should be assigned to all `character` and `factor` variables
#'   in the model (as long as the specified contrast is not the global default).
#'   Default is [contr.bayes].
#' @param ... further arguments passed to [`rstanarm::stan_lmer`].
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


check_contrasts <- function(formula, data, new_contrast) {
  vars.to.check <- all.vars(as.formula(formula))
  resetted <- NULL
  for (i in vars.to.check) {
    if (is.character(data[,i])) {
      data[,i] <- factor(data[,i])
    }
    if (is.factor(data[,i])) {
      if (is.null(attr(data[,i], "contrasts")) &
          (options("contrasts")[[1]][1] != new_contrast)) {
        contrasts(data[,i]) <- new_contrast
        resetted  <- c(resetted, i)
      }
      else if (!is.null(attr(data[,i], "contrasts")) &&
               attr(data[,i], "contrasts") != new_contrast) {
        contrasts(data[,i]) <- new_contrast
        resetted  <- c(resetted, i)
      }
    }
  }
  if (!is.null(resetted))
    message(paste0("Contrasts set to ", new_contrast," for the following variables: ",
                   paste0(resetted, collapse=", ")))
  return(data)
}
