#' Plot the trace lines of the imputation algorithm
#'
#' @param data An object of class `mice::mids`.
#' @param vrb String or vector with variable name(s), default is "all".
#'
#' @return An object of class `ggplot2::ggplot`.
#'
#' @examples
#' imp <- mice::mice(mice::nhanes, print = FALSE)
#' plot_trace(imp)
#' @export
plot_trace <- function(data, vrb = "all") {
  if (!mice::is.mids(data)) {
    stop("argument 'data' must be a 'mids' object", call. = FALSE)
  }
  if (is.null(data$chainMean)) {
    stop("no convergence diagnostics found", call. = FALSE)
  }

  # extract chain means and chain standard deviations
  mn <- data$chainMean
  sm <- sqrt(data$chainVar)

  # select variable to plot from list of imputed variables
  varlist <- names(data$imp)[apply(!(is.nan(mn) | is.na(mn)), 1, all)]
  if (vrb %nin% varlist & vrb != "all") {
    stop(paste0("No convergence diagnostics found for variable '", vrb, "'. No plot can be produced. Are you sure this variable is imputed?"))
  }
  if (vrb == "all") {
    vrb <- varlist
  }
  p <- length(vrb)
  m <- data$m
  it <- data$iteration
  long <- cbind(
    expand.grid(.it = seq_len(it), .m = seq_len(m)),
    data.frame(
      .ms = rep(c("mean", "sd"), each = m * it * p),
      vrb = rep(vrb, each = m * it, times = 2),
      val = c(
        matrix(aperm(mn[vrb, , , drop = FALSE], c(2, 3, 1)), nrow = m * it * p),
        matrix(aperm(sm[vrb, , , drop = FALSE], c(2, 3, 1)), nrow = m * it * p)
      )
    )
  )

  # plot the convergence diagnostics
  ggplot2::ggplot(long, ggplot2::aes(x = .data$.it, y = .data$val, color = as.factor(.data$.m))) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(vrb ~ .ms, scales = "free", ncol = 2, strip.position = "left") +
    ggplot2::labs(
      x = "Iteration",
      y = "",
      color = "Imputation number"
    ) +
    theme_mice()
}

# TODO: make iterations and statistic arguments as well
