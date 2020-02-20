#' @export
summary.stanova <- function(object, probs = c(0.05, 0.5, 0.95), ...) {

  ## get summaries:
  matrix_diff <- get_difference_list(object, as.array = FALSE)
  summaries <- lapply(matrix_diff, function(x) {
    cbind(
      Mean = apply(x, 2L, mean)
      , MAD_SD = apply(x, 2L, mad)
      , t(apply(x, 2L, quantile, probs))
    )
  })

  ## get diagnostics:
  array_diff <- get_difference_list(object, as.array = TRUE)
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
      tested = rownames(summaries[[i]])
    )
    out[[i]] <- cbind(out[[i]], summaries[[i]])
    out[[i]]$rhat <- rhat[[i]]
    out[[i]]$ess_bulk <- ess_bulk[[i]]
    out[[i]]$ess_tail <- ess_tail[[i]]
    rownames(out[[i]]) <- NULL
  }

  class(out) <- "summary.stanova"
  return(out)
}

get_difference_list <- function(object, as.array = TRUE, ...) {
  terms <- terms(lme4::nobars(object$formula))
  terms_chr <- attr(terms, "term.labels")
  term2 <- strsplit(terms_chr, split = ":")
  names(term2) <- terms_chr

  if (as.array) {
    ### extract samples as arrays
    post_intercept <- as.array(object, pars = "(Intercept)")
    post_intercept <- aperm(post_intercept, perm = c(1, 3, 2))

    post_diff <- lapply(term2, function(x) {
      ## extract samples per factor-level or design cell and concatenate chains
      tmp <- as.array(coda::as.mcmc(emmeans::emmeans(object, x)))
      for (j in seq_len(dim(tmp)[2])) {
        tmp[ ,j, ] <- post_intercept[, 1,] - tmp[ ,j, ]
      }
      tmp
    })
  } else {
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
    Intercept = list(post_intercept),
    post_diff
  )
  return(out)
}

