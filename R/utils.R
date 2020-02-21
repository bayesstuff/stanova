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

