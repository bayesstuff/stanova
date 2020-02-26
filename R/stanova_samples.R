#' Posterior samples representing difference from intercept
#'
#' @param object Fitted model. Currently only support for `stanova` objects but
#'   likely supports `rstanarm` and potentially other objects as well.
#' @param diff_intercept logical. If TRUE (the default) samples for factor
#'   levels represent the difference from the intercept. If FALSE, marginal
#'   means of factor levels are returned.
#' @param terms optional character vector denoting a subset of model terms for
#'   which difference from intercept be returned.
#' @param return `character` string denoting format in which samples should be
#'   returned. Allowed values are `c("array", "matrix", "data.frame")`, possibly
#'   abbreviated.
#' @param dimension_chain Scalar integer. If `return = "array"`, determines the
#'   dimension of the chain. The default `3` means chain is the third dimension
#'   of the returned array. Value should be between 1 and 3.
#' @param family `family` argument passed to `stan_glmer`.
#' @param ... currently ignored.
#'
#' @importFrom stats mad quantile
#'
#' @export
stanova_samples <- function(object, ...) UseMethod("stanova_samples", object)


#' @rdname stanova_samples
#' @export
stanova_samples.stanova <- function(
  object,
  diff_intercept = TRUE,
  terms,
  return = c("array", "matrix", "data.frame"),
  dimension_chain = 3L,
  ...
) {
  return <- match.arg(return)
  all_terms <- stats::terms(lme4::nobars(object$formula))
  terms_chr <- attr(all_terms, "term.labels")
  if (!missing(terms)) {
    # if (length(terms[ !(terms %in% terms2)]) > 0)
    #   warning("Terms ", terms[ !(terms %in% terms2)], " not in model.")
    terms_chr <- terms[ terms %in% terms_chr]
  }
  term2 <- strsplit(terms_chr, split = ":")
  names(term2) <- terms_chr

  ### extract samples as arrays
  post_intercept <- as.array(object, pars = "(Intercept)")
  post_intercept <- aperm(post_intercept, perm = c(1, 3, 2))
  names(dimnames(post_intercept)) <- c("Iteration", "Parameter", "Chain")

  post_diff <- lapply(term2, get_stanova_samples,
                      object = object,
                      diff_intercept = diff_intercept,
                      intercept_array = post_intercept,
                      dimension_chain = dimension_chain)
  dimnames(post_intercept)[[1]] <- dimnames(post_diff[[1]])[[1]]
  dims <- c("iter" = 1, "par" = 2, "chain" = 3)
  if (dimension_chain == 2L) {
    post_intercept <- aperm(post_intercept, c(1, 3, 2))
    dims <- c("iter" = 1, "par" = 3, "chain" = 2)
  }
  if (dimension_chain == 1L) {
    post_intercept <- aperm(post_intercept, c(3, 1, 2))
    dims <- c("iter" = 2, "par" = 3, "chain" = 1)
  }
  out <- c(
    `(Intercept)` = list(post_intercept),
    post_diff
  )

  if (return == "matrix") {
    for (i in seq_along(out)) {
      tmp_arr <- aperm(out[[i]],
                       c(dims["iter"],dims["chain"],dims["par"]))
      dim(tmp_arr) <- c(
        dim(out[[i]])[dims["iter"]] * dim(out[[i]])[dims["chain"]],
        dim(out[[i]])[dims["par"]]
      )
      colnames(tmp_arr) <- dimnames(out[[i]])[["Parameter"]]
      out[[i]] <- tmp_arr
    }

    # ### extract samples as matrices
    #
    # ## extract intercept and concatenate chains
    # post_intercept <- as.matrix(object, pars = "(Intercept)")
    #
    # post_diff <- lapply(term2, function(x) {
    #   ## extract samples per factor-level or design cell and concatenate chains
    #   tmp <- as.matrix(coda::as.mcmc(emmeans::emmeans(object, x)))
    #   ## calculate difference from intercept
    #   tmp - post_intercept[,1]
    # })
  }

  if (return == "data.frame") {
    for (i in seq_along(out)) {
      out[[i]] <- as.data.frame.table(out[[i]], responseName = "Value")
    }
  }

  return(out)
}

get_stanova_samples <- function(term, object, diff_intercept,
                                intercept_array,
                                dimension_chain) {
  if (any(vapply(object[["data"]][,term], is.numeric, TRUE))) {
    stop("not yet implemented ):")
    tmp <- coda::as.mcmc(emmeans::emmeans(object, term))
  } else {
      ## extract samples per factor-level or design cell and concatenate chains
      tmp <- coda::as.mcmc(emmeans::emmeans(object, term))
      ## in case we only have one chain, we need to transform to mcmc.list first
      ## before transforming into array
      if (!coda::is.mcmc.list(tmp)) {
        tmp <- coda::as.mcmc.list(tmp)
      }
      tmp <- as.array(tmp, drop = FALSE)
      if (diff_intercept) {
        for (j in seq_len(dim(tmp)[2])) {
          tmp[ ,j, ] <- tmp[ ,j, ] -
            intercept_array[, 1, ]
        }
      }
      dimnames(tmp)[[3]] <- dimnames(intercept_array)[[3]]
      names(dimnames(tmp)) <- names(dimnames(intercept_array))
      if (dimension_chain == 2L) {
        tmp <- aperm(tmp, c(1, 3, 2))
      }
      if (dimension_chain == 1L) {
        tmp <- aperm(tmp, c(3, 1, 2))
      }
      tmp
  }
}
