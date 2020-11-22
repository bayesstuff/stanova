#' @importFrom stats as.formula contrasts<- formula
check_contrasts_fun <- function(formula, data, new_contrast) {
  vars.to.check <- all.vars(lme4::nobars(as.formula(formula)))
  resetted <- NULL
  for (i in vars.to.check) {
    if (is.character(data[[i]])) {
      data[,i] <- factor(data[[i]])
    }
    if (is.factor(data[[i]])) {
      if (!is.character(new_contrast) || (
        (
          is.null(attr(data[[i]], "contrasts")) &
          (options("contrasts")[[1]][1] != new_contrast)
        ) ||
        (
          !is.null(attr(data[[i]], "contrasts")) &&
          attr(data[[i]], "contrasts") != new_contrast
        )
      )) {
        contrasts(data[[i]]) <- new_contrast
        resetted  <- c(resetted, i)
      }
    }
  }
  if (!is.null(resetted)) {
    if (is.character(new_contrast)) {
      contrast_name <- new_contrast
    } else {
      contrast_name <- "passed contrast function"
    }
    message(paste0("Contrasts set to ", contrast_name," for the following variables: ",
                   paste0(resetted, collapse=", ")))
  }
  return(data)
}

create_contrasts_list <- function(formula, data, new_contrast) {
  vars.to.check <- all.vars(lme4::nobars(as.formula(formula)))
  resetted <- NULL
  outlist <- list()
  for (i in vars.to.check) {
    if (is.character(data[[i]]) | is.factor(data[,i])) {
      if (is.null(attr(data[[i]], "contrasts")) &
          (options("contrasts")[[1]][1] != new_contrast)) {
        outlist[[i]] <- new_contrast
        resetted  <- c(resetted, i)
      }
      else if (!is.null(attr(data[,i], "contrasts")) &&
               attr(data[[i]], "contrasts") != new_contrast) {
        outlist[[i]] <- new_contrast
        resetted  <- c(resetted, i)
      }
    }
  }
  if (!is.null(resetted))
    message(paste0("Contrasts set to ", new_contrast,
                   " for the following variables: ",
                   paste0(resetted, collapse=", ")))
  if (length(outlist) == 0) outlist <- NULL
  return(outlist)
}


