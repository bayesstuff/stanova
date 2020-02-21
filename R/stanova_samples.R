#' Extract stanova difference from intercept samples
#'
#' @param object Fitted model. Currently only tested for `stanova` objects but
#'   likely supports `rstanarm` and potentially other objects as well.
#' @param terms optional character vector denoting a subset of model terms for
#'   which difference from intercept be returned.
#' @param return `character` string denoting format in which samples should be
#'   returned. Allowed values are `c("array", "matrix", "data.frame")`, possibly
#'   abbreviated.
#' @param dimension_chain Scalar integer. If `return = "array"`, determines the
#'   dimension of the chain. The default `3` means chain is the third dimension
#'   of the returned array. Value should be between 1 and 3.
#'
#' @export
stanova_samples <- function(object,
                            terms,
                            return = c("array", "matrix", "data.frame"),
                            dimension_chain = 3L,
                            ...) {
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


  if (return %in% c("array", "data.frame")) {
    ### extract samples as arrays
    post_intercept <- as.array(object, pars = "(Intercept)")
    post_intercept <- aperm(post_intercept, perm = c(1, 3, 2))
    names(dimnames(post_intercept)) <- c("Iteration", "Parameter", "Chain")

    post_diff <- lapply(term2, function(x) {
      ## extract samples per factor-level or design cell and concatenate chains
      tmp <- coda::as.mcmc(emmeans::emmeans(object, x))
      ## in case we only have one chain, we need to transform to mcmc.list first
      ## before transforming into array
      if (!coda::is.mcmc.list(tmp)) {
        tmp <- coda::as.mcmc.list(tmp)
      }
      tmp <- as.array(tmp, drop = FALSE)
      for (j in seq_len(dim(tmp)[2])) {
        tmp[ ,j, ] <- tmp[ ,j, ] -
          post_intercept[, 1, ]
      }
      dimnames(tmp)[[3]] <- dimnames(post_intercept)[[3]]
      names(dimnames(tmp)) <- names(dimnames(post_intercept))
      if (dimension_chain == 2L) {
        tmp <- aperm(tmp, c(1, 3, 2))
      }
      if (dimension_chain == 1L) {
        tmp <- aperm(tmp, c(3, 1, 2))
      }
      tmp
    })
    dimnames(post_intercept)[[1]] <- dimnames(post_diff[[1]])[[1]]
    if (dimension_chain == 2L) {
      post_intercept <- aperm(post_intercept, c(1, 3, 2))
    }
    if (dimension_chain == 1L) {
      post_intercept <- aperm(post_intercept, c(3, 1, 2))
    }

  } else if (return == "matrix") {
    ### extract samples as matrices

    ## extract intercept and concatenate chains
    post_intercept <- as.matrix(object, pars = "(Intercept)")

    post_diff <- lapply(term2, function(x) {
      ## extract samples per factor-level or design cell and concatenate chains
      tmp <- as.matrix(coda::as.mcmc(emmeans::emmeans(object, x)))
      ## calculate difference from intercept
      tmp - post_intercept[,1]
    })
  }
  out <- c(
    `(Intercept)` = list(post_intercept),
    post_diff
  )
  if (return == "data.frame") {
    for (i in seq_along(out)) {
      out[[i]] <- as.data.frame.table(out[[i]], responseName = "Value")
    }
  }

  return(out)
}

