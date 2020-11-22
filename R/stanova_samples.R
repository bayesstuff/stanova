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
  if (inherits(object, "brmsfit")) {
    all_terms <- tryCatch(
      expr = stats::terms(lme4::nobars(object$formula$formula)),
      error = function(e) stats::terms(object$formula$formula)
    )
  } else if (inherits(object, "stanreg")) {
    all_terms <- stats::terms(lme4::nobars(object$formula))
  } else {
    stop("not supported model.", call. = FALSE)
  }
  terms_chr <- attr(all_terms, "term.labels")
  if (!missing(terms)) {
    # if (length(terms[ !(terms %in% terms2)]) > 0)
    #   warning("Terms ", terms[ !(terms %in% terms2)], " not in model.")
    terms_chr <- terms[ terms %in% terms_chr]
  } else {
    terms_chr <- terms_chr[!grepl("\\|", terms_chr)]
  }
  term2 <- strsplit(terms_chr, split = ":")
  names(term2) <- terms_chr

  ### extract samples as arrays
  # post_intercept <- safe_get_stanova_samples(
  #   term = "1",
  #   object = object,
  #   diff_intercept = FALSE,
  #   intercept_array = NULL,
  #   dimension_chain = dimension_chain
  # )
  if (inherits(object, "brmsfit")) {
    post_intercept <- extract_posterior_array(object,
                                              pars = "b_Intercept", fixed = TRUE)
    dimnames(post_intercept)$Parameter <- "(Intercept)"
  } else {
    post_intercept <- extract_posterior_array(object, pars = "(Intercept)")
  }

  ## warning: using emmeans(object, term = "1") does not extract the intercept!

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
      colnames(tmp_arr) <- dimnames(out[[i]])[[2]]
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
      out[[i]]$Chain <- as.numeric(out[[i]]$Chain)
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
  if (term[1] == "1") {
    num_vars <- FALSE
  } else {
    num_vars <- vapply(object[["data"]][,term,drop = FALSE], is.numeric, TRUE)
    names_num_vars <- names(num_vars)[num_vars]
  }
  if (any(num_vars)) {
    sd_num <- apply(object[["data"]][,names_num_vars,drop = FALSE], 2,
                    stats::sd)
    mean_num <- apply(object[["data"]][,names_num_vars,drop = FALSE], 2, mean)
    if (length(term) == 1) {
      if (inherits(object, "brmsfit")) {
        tmp <- extract_posterior_array(object,
                                       pars = paste0("b_", term),
                                       fixed = TRUE)
        dimnames(tmp)[[2]] <- substr(dimnames(tmp)[[2]], 3, 1e6)
      } else if (inherits(object, "stanreg")) {
        tmp <- extract_posterior_array(object, pars = term)
      }
      attr(tmp, "estimate") <- paste0("trend ('", term, "')")
    } else {
      i <- 1 ## which numerical variable is chosen, default is 1
      spec_vars <- term[-which(num_vars)[i]]
      ## calculate 'at' values in case there are additional numerical variables
      addl_num <- spec_vars[spec_vars %in% names_num_vars]
      if (length(addl_num) > 0) {
        atlist <- lapply(addl_num, function(x) {c(
          m1 = mean_num[x] - 1*sd_num[x],
          mean_num[x],
          mean_num[x] + 1*sd_num[x])})
        names(atlist) <- addl_num
      } else {
        atlist <- list()
      }
      emms <- suppressMessages(emmeans::emtrends(
        object = object,
        specs = spec_vars,
        var = term[which(num_vars)[i]],
        at = atlist
      ))
      tmp <- coda::as.mcmc(emms)
      if (!coda::is.mcmc.list(tmp)) {
        tmp <- coda::as.mcmc.list(tmp)
      }
      tmp <- as.array(tmp, drop = FALSE)
      dimnames(tmp)[[3]] <- dimnames(intercept_array)[[3]]
      names(dimnames(tmp)) <- names(dimnames(intercept_array))
      attr(tmp, "estimate") <- paste0("trend ('", term[which(num_vars)[i]], "')")

      ## in case there are numerical variables to condition on, rename
      if (length(addl_num) > 0) {
        df_for_names <- as.data.frame(summary(emms))
        for (j in seq_along(addl_num)) {
          tmp_vals <- factor(df_for_names[,addl_num[j]])
          levels(tmp_vals) <- c("M-SD", "M   ", "M+SD")
          df_for_names[,addl_num[j]] <- as.character(tmp_vals)
        }
        namelist <- lapply(spec_vars, function(x) paste(x, df_for_names[, x]))
        if (length(namelist) > 1) {
          namelist <- apply(as.data.frame(namelist), 1, paste, collapse = ", ")
        } else {
          namelist <- namelist[[1]]
        }
        dimnames(tmp)[["Parameter"]] <- namelist
      }
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


extract_posterior_array <- function(object, pars, ...) {
  post <- as.array(object, pars = pars, ...)
  post <- aperm(post, perm = c(1, 3, 2))
  names(dimnames(post)) <- c("Iteration", "Parameter", "Chain")
  dimnames(post)[[1]] <- as.character(seq_len(dim(post)[1]))
  dimnames(post)[[3]] <- as.character(seq_len(dim(post)[3]))
  post
}
