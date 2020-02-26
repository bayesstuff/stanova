#' @export
summary.stanova <- function(object,
                            probs = c(0.05, 0.5, 0.95),
                            ...,
                            digits = 3L) {

  ## get summaries:
  matrix_diff <- stanova_samples(object, return = "matrix")
  summaries <- lapply(matrix_diff, function(x) {
    cbind(
      Mean = apply(x, 2L, mean)
      , MAD_SD = apply(x, 2L, mad)
      , t(apply(x, 2L, quantile, probs))
    )
  })

  ## get diagnostics:
  array_diff <- stanova_samples(object, return = "array")
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
    out[[i]] <- data.frame(
      Variable = rownames(summaries[[i]])
    )
    out[[i]] <- cbind(out[[i]], summaries[[i]])
    out[[i]]$rhat <- rhat[[i]]
    out[[i]]$ess_bulk <- ess_bulk[[i]]
    out[[i]]$ess_tail <- ess_tail[[i]]
    rownames(out[[i]]) <- NULL
  }

  ## get attributes from rstanarm summary:
  tmp <- object
  class(tmp) <- class(tmp)[class(tmp) != "stanova"]
  tmp2 <- summary(tmp)

  ## prepare return object as summary.stanreg
  structure(
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
    print.digits = digits,
    priors = object$prior.info,
    no_ppd_diagnostic = attr(tmp2, "no_ppd_diagnostic"),
    class = "summary.stanova"
  )
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
  function(x, digits = max(3L, attr(x, "print.digits")),
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
    cat("\n priors:      ", "see help('prior_summary', package = 'rstanarm')")

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
    print(x$`(Intercept)`, digits = digits)

    for (i in seq_len(length(x)-1)) {
      cat("\n\nEstimates '", names(x)[i+1] ,"':\n", sep = "")
      print(x[[i+1]], digits = digits)
    }
    invisible(x)
  }
