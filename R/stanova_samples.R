#' Posterior samples for ANOVA-type models
#'
#' @param object Fitted model. Currently only support for `stanova` objects.
#' @param diff_intercept logical. If `TRUE` (the default) samples for factor
#'   levels represent the difference from the intercept. If `FALSE`, marginal
#'   means of factor levels are returned.
#' @param terms optional character vector denoting a subset of model terms for
#'   which difference from intercept be returned.
#' @param return `character` string denoting format in which samples should be
#'   returned. Allowed values are
#'   `c("array", "matrix", "data.frame", "tibble", "tidybayes")`,
#'   possibly abbreviated. `"tibble"` requires package `tibble` and returns the
#'   same object as `data.frame` wrapped into `as_tibble()`. `"tidybayes"` also
#'   returns essentially the same object, but renames the columns to follow
#'   `tidybayes` conventions.
#' @param dimension_chain Scalar integer. If `return = "array"`, determines the
#'   dimension of the chain. The default `3` means chain is the third dimension
#'   of the returned array. Value should be between 1 and 3.
#' @param ... currently ignored.
#'
#' @example examples/examples.stanova_samples.R
#'
#' @export
stanova_samples <- function(object, ...) UseMethod("stanova_samples", object)


#' @rdname stanova_samples
#' @export
stanova_samples.stanova <- function(
  object,
  diff_intercept = TRUE,
  terms,
  return = c("array", "matrix", "data.frame", "tibble", "tidybayes"),
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
  post_intercept <- extract_array_rstanarm(object, pars = "(Intercept)")

  post_diff <- lapply(term2, safe_get_stanova_samples,
                      object = object,
                      diff_intercept = diff_intercept,
                      intercept_array = post_intercept,
                      dimension_chain = dimension_chain)
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
      type_est <- attr(out[[i]], "estimate")
      tmp_arr <- aperm(out[[i]],
                       c(dims["iter"],dims["chain"],dims["par"]))
      dim(tmp_arr) <- c(
        dim(out[[i]])[dims["iter"]] * dim(out[[i]])[dims["chain"]],
        dim(out[[i]])[dims["par"]]
      )
      colnames(tmp_arr) <- dimnames(out[[i]])[["Parameter"]]
      out[[i]] <- tmp_arr
      attr(out[[i]], "estimate") <- type_est
    }
  }

  if (return %in% c("data.frame", "tibble", "tidybayes")) {
    if (return %in% c("tibble", "tidybayes")) {
      if (!requireNamespace("tibble", quietly = TRUE)) {
        warning("package tibble is required for returning a tibble",
                call. = FALSE)
      }
    }
    for (i in seq_along(out)) {
      type_est <- attr(out[[i]], "estimate")
      out[[i]] <- cbind(
        Term = names(out)[i],
        as.data.frame.table(out[[i]], responseName = "Value")
      )[, c("Term", "Parameter", "Value", "Chain", "Iteration")]
      colnames(out[[i]])[2] <- "Variable"
      out[[i]]$Iteration <- as.numeric(as.character(out[[i]]$Iteration))
      out[[i]]$Chain <- as.numeric(substr(out[[i]]$Chain,
                                          start = 7, stop = 1e6))
      out[[i]]$Draw <- max(out[[i]]$Iteration) * (out[[i]]$Chain - 1) +
        out[[i]]$Iteration
      attr(out[[i]], "estimate") <- type_est
      if ((return %in% c("tibble", "tidybayes")) &&
          requireNamespace("tibble", quietly = TRUE)) {
        out[[i]] <- tibble::as_tibble(out[[i]])
      }
      if (return == "tidybayes") {
        colnames(out[[i]]) <- c("term", "variable", "value",
                                ".chain",	".iteration",	".draw")
      }
    }
  }

  return(out)
}

get_stanova_samples <- function(term, object, diff_intercept,
                                intercept_array,
                                dimension_chain) {
  num_vars <- vapply(object[["data"]][,term,drop = FALSE], is.numeric, TRUE)
  if (any(num_vars)) {
    if (length(term) == 1) {
      tmp <- extract_array_rstanarm(object, pars = term)
      attr(tmp, "estimate") <- paste0("trend ('", term, "')")
    } else {
      i <- 1 ## which numerical variable is chosen, default is 1
      tmp <- coda::as.mcmc(emmeans::emtrends(
        object = object,
        specs = term[-which(num_vars)[i]],
        var = term[which(num_vars)[i]]
      ))
      if (!coda::is.mcmc.list(tmp)) {
        tmp <- coda::as.mcmc.list(tmp)
      }
      tmp <- as.array(tmp, drop = FALSE)
      dimnames(tmp)[[3]] <- dimnames(intercept_array)[[3]]
      names(dimnames(tmp)) <- names(dimnames(intercept_array))
      attr(tmp, "estimate") <- paste0("trend ('", term[which(num_vars)[i]], "')")
    }
  } else {
      ## extract samples per factor-level or design cell and concatenate chains
      tmp <- suppressMessages(coda::as.mcmc(emmeans::emmeans(object, term)))
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
        attr(tmp, "estimate") <- "difference from intercept"
      } else {
        attr(tmp, "estimate") <- "marginal means"
      }
      dimnames(tmp)[[3]] <- dimnames(intercept_array)[[3]]
      names(dimnames(tmp)) <- names(dimnames(intercept_array))
  }
  if (dimension_chain == 2L) {
    type_est <- attr(tmp, "estimate")
    tmp <- aperm(tmp, c(1, 3, 2))
    attr(tmp, "estimate") <- type_est
  }
  if (dimension_chain == 1L) {
    type_est <- attr(tmp, "estimate")
    tmp <- aperm(tmp, c(3, 1, 2))
    attr(tmp, "estimate") <- type_est
  }
  tmp
}

safe_get_stanova_samples <- function(term, object, diff_intercept,
                                intercept_array,
                                dimension_chain) {
  tryCatch(get_stanova_samples(
    term = term,
    object = object,
    diff_intercept = diff_intercept,
    intercept_array = intercept_array,
    dimension_chain = dimension_chain
  ), error = function(e) {
    warning(term, ": ", e, call. = FALSE)
    nn <- as.list(rep(list(NULL), 3))
    names(nn) <- names(dimnames(intercept_array))
    tmp <- array(NA_real_, dim = c(0, 0, 0), dimnames = nn)
    attr(tmp, "estimate") <- "failed"
    return(tmp)
  }
  )
}

extract_array_rstanarm <- function(object, pars) {
  post <- as.array(object, pars = pars)
  post <- aperm(post, perm = c(1, 3, 2))
  names(dimnames(post)) <- c("Iteration", "Parameter", "Chain")
  dimnames(post)[[1]] <- as.character(seq_len(dim(post)[1]))
  post
}
