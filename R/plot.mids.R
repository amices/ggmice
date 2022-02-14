# plot convergence
#' Title
#'
#' @param imp An object of class \code{mids}
#'
#' @return An object of class \code{ggplot}
#'
#' @examples
#' imp <- mice::mice(mice::nhanes, print = FALSE)
#' plot_chains(imp)
#' @export
plot_chains <- function(imp){
  # call <- match.call()
  if (!mice::is.mids(imp)) {
    stop("argument 'imp' must be a 'mids' object", call. = FALSE)
  }
  if (is.null(imp$chainMean)) {
    stop("no convergence diagnostics found", call. = FALSE)
  }

  # extract chain means and chain variances
  mn <- imp$chainMean
  sm <- sqrt(imp$chainVar)

  # select subset of nonmissing entries
  obs <- apply(!(is.nan(mn) | is.na(mn)), 1, all)
  varlist <- names(obs)[obs]
  p <- length(varlist)
  m <- imp$m
  it <- imp$iteration
  dat <-  cbind(
    expand.grid(.it = seq_len(it), .m = seq_len(m)),
    data.frame(
    .ms = rep(c("mean", "sd"), each = m * it * p),
    vrb = rep(varlist, each = m * it, times = 2),
    val = c(matrix(aperm(mn[varlist, , , drop = FALSE], c(2, 3, 1)), nrow = m * it * p),
            matrix(aperm(sm[varlist, , , drop = FALSE], c(2, 3, 1)), nrow = m * it * p))
  ))

  # ## Dummy to trick R CMD check
  # .m <- NULL
  # rm(.m)

  ggplot2::ggplot(dat, ggplot2::aes(x = .it, y = val, color = as.factor(.m))) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(vrb~.ms, scales = "free", ncol = 2, strip.position = "left") +
    ggplot2::labs(x = "Iteration",
         y = "",
         color = "Imputation") +
    theme_mice()
}
