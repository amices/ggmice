#' Plot the trace lines of the imputation algorithm
#'
#' @param data An object of class [mice::mids].
#' @param vrb String, vector, or unquoted expression with variable name(s), default is "all".
#'
#' @return An object of class [ggplot2::ggplot].
#'
#' @examples
#' imp <- mice::mice(mice::nhanes, print = FALSE)
#' plot_trace(imp)
#' @export
plot_trace <- function(data, vrb = "all") {
  verify_data(data, imp = TRUE)
  if (is.null(data$chainMean)) {
    stop("no convergence diagnostics found", call. = FALSE)
  }

  # extract chain means and chain standard deviations
  mn <- data$chainMean
  sm <- sqrt(data$chainVar)

  # select variable to plot from list of imputed variables
  vrb <- substitute(vrb)
  varlist <-
    names(data$imp)[apply(!(is.nan(mn) | is.na(mn)), 1, all)]
  if (as.character(vrb)[1] == "all") {
    vrb <- varlist
  } else {
    vrb <- names(dplyr::select(data$data, {{ vrb }}))
  }
  if (any(vrb %nin% varlist)) {
    message(
      paste0(
        "No convergence diagnostics found for variable(s) '",
        vrb[which(vrb %nin% varlist)],
        "'. No plots can be produced for these. Are you sure these variables are imputed?"
      )
    )
    if (any(vrb %in% varlist)) {
      vrb <- vrb[which(vrb %in% varlist)]
    } else {
      stop("None of the variables are imputed. No plots can be produced.")
    }
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
        matrix(aperm(mn[vrb, , , drop = FALSE], c(
          2, 3, 1
        )), nrow = m * it * p),
        matrix(aperm(sm[vrb, , , drop = FALSE], c(
          2, 3, 1
        )), nrow = m * it * p)
      )
    )
  )

  # plot the convergence diagnostics
  ggplot2::ggplot(
    long,
    ggplot2::aes(
      x = .data$.it,
      y = .data$val,
      color = as.factor(.data$.m)
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::facet_grid(
      vrb ~ .ms,
      scales = "free",
      switch = "y",
      labeller = ggplot2::labeller(.ms = function(x){paste("Imputation", x)})
    ) +
    ggplot2::labs(
      x = "Iteration",
      y = "",
      color = "Imputation number"
    ) +
    theme_mice()
}
