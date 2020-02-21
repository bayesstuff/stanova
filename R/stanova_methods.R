#' @export
summary.stanova <- function(object, probs = c(0.05, 0.5, 0.95), ...) {

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

