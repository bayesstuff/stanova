#' @importFrom stats mad quantile
#' @export
summary.stanova <- function(object,
                            diff_intercept = TRUE,
                            probs = c(0.05, 0.5, 0.95),
                            ...,
                            digits = 3L) {
  ## get summaries:
  matrix_diff <- stanova_samples(object,
                                 diff_intercept = diff_intercept,
                                 return = "matrix")
  summaries <- lapply(matrix_diff, function(x) {
    cbind(
      Mean = apply(x, 2L, mean)
      , MAD_SD = apply(x, 2L, mad)
      , t(apply(x, 2L, quantile, probs))
    )
  })

  ## get diagnostics:
  array_diff <- suppressWarnings(stanova_samples(object,
                                diff_intercept = diff_intercept,
                                return = "array"))
  rhat <- lapply(
    array_diff,
    function(x)
      vapply(seq_len(dim(x)[2]), function(y) rstan::Rhat(x[,y,]), 1)
  )
  ess_bulk <- lapply(
    array_diff,
    function(x)
      vapply(seq_len(dim(x)[2]), function(y) rstan::ess_bulk(x[,y,]), 1)
  )
  ess_tail <- lapply(
    array_diff,
    function(x)
      vapply(seq_len(dim(x)[2]), function(y) rstan::ess_tail(x[,y,]), 1)
  )
  out <- vector("list", length = length(summaries))
  names(out) <- names(summaries)
  for (i in seq_along(out)) {
    if (ncol(summaries[[i]]) > 0) {
      out[[i]] <- data.frame(
        Variable = rownames(summaries[[i]])
      )
      out[[i]] <- cbind(out[[i]], summaries[[i]])
      out[[i]]$rhat <- rhat[[i]]
      out[[i]]$ess_bulk <- ess_bulk[[i]]
      out[[i]]$ess_tail <- ess_tail[[i]]
      rownames(out[[i]]) <- NULL
    }
  }

  ## get attributes from rstanarm summary:
  tmp <- object
  class(tmp) <- class(tmp)[class(tmp) != "stanova"]
  tmp2 <- summary(tmp)

  if (inherits(object, "brmsfit")) {
    ## prepare return object as summary.stanreg
    out <- structure(
      out,
      call = object$call,
      algorithm = object$algorithm,
      stan_function = "brms::brm",
      family = summary(stats::family(object)),
      formula = tmp2$formula$formula,
      posterior_sample_size = (tmp2$iter - tmp2$warmup) * tmp2$chains,
      nchain = tmp2$chains,
      nobs = tmp2$nobs,
      npreds = NULL,
      ngrps = tmp2$ngrps,
      estimate = unlist(lapply(array_diff, function(x) attr(x, "estimate"))),
      print.digits = digits,
      priors = "Use brms::prior_summary(object) for prior information", #"No priors specified", #tmp2$prior,
      no_ppd_diagnostic = NULL,
      class = "summary.stanova"
    )
  } else if (inherits(object, "stanreg")) {
    ## prepare return object as summary.stanreg
    out <- structure(
      out,
      call = object$call,
      algorithm = object$algorithm,
      stan_function = object$stan_function,
      family = attr(tmp2, "family"),
      formula = formula(object),
      posterior_sample_size = attr(tmp2, "posterior_sample_size"),
      nchain = length(object$stanfit@stan_args),
      nobs = attr(tmp2, "nobs"),
      npreds = attr(tmp2, "npreds"),
      ngrps = attr(tmp2, "ngrps"),
      estimate = unlist(lapply(array_diff, function(x) attr(x, "estimate"))),
      print.digits = digits,
      priors = "see help('prior_summary', package = 'rstanarm')", #object$prior.info,
      no_ppd_diagnostic = attr(tmp2, "no_ppd_diagnostic"),
      class = "summary.stanova"
    )
  } else {
    stop("not supported model.", call. = FALSE)
  }
  out
}

formula_string <- function (formula, break_and_indent = TRUE)
{
  coll <- if (break_and_indent)
    "--MARK--"
  else " "
  char <- gsub("\\s+", " ", paste(deparse(formula), collapse = coll))
  if (!break_and_indent)
    return(char)
  gsub("--MARK--", "\n\t  ", char, fixed = TRUE)
}

#' @export
print.summary.stanova <-
  function(x, digits = max(1L, attr(x, "print.digits")),
           ...) {
    atts <- attributes(x)
    cat("\nModel Info:")
    cat("\n function:    ", atts$stan_function)
    cat("\n family:      ", atts$family)
    cat("\n formula:     ", formula_string(atts$formula))
    cat("\n algorithm:   ", atts$algorithm)
    cat("\n chains:      ", atts$nchain)
    if (!is.null(atts$posterior_sample_size) && atts$algorithm == "sampling") {
      cat("\n sample:      ", atts$posterior_sample_size,
          "(posterior sample size)")
    }
    cat("\n priors:      ", atts$priors)

    cat("\n observations:", atts$nobs)
    if (!is.null(atts$npreds)) {
      cat("\n predictors:  ", atts$npreds)
    }
    if (!is.null(atts$ngrps)) {
      cat("\n groups:      ", paste0(names(atts$ngrps), " (",
                                     unname(atts$ngrps), ")",
                                     collapse = ", "))
    }

    cat("\n\nEstimate Intercept:\n")
    print_round(x$`(Intercept)`, digits = digits)
    cat("\n")

    for (i in seq_len(length(x)-1)) {
      cat("\nEstimates '", names(x)[i+1] ,"' - ",
          atts$estimate[i],
          ":\n", sep = "")
      print_round(x[[i+1]], digits = digits)
    }
    invisible(x)
  }

print_round <- function(x, digits, ...) {
  if (is.list(x)) {
    for (i in seq_along(x)) {
      if (is.numeric(x[[i]])) {
        x[[i]] <- round(x[[i]], digits = digits)
      }
    }
  }
  print(format(x, nsmall = digits), quote = FALSE, ...)
}
