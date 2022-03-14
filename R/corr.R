#' Plot correlations between (incomplete) variables
#'
#' @param data A dataset of class `data.frame`, `tibble`, or `matrix`.
#' @param vrb String or vector with variable name(s), default is "all".
#' @param label Logical indicating whether correlation values should be displayed.
#' @param square Logical indicating whether the plot tiles should be squares.
#' @param diagonal Logical indicating whether the correlation of each variable with itself should be displayed.
#' @param rotate Logical indicating whether the variable name labels should be rotated 90 degrees.
#'
#' @return An object of class `ggplot`.
#'
#' @examples
#' plot_corr(mice::nhanes, label = TRUE)
#' @export
plot_corr <- function(data, vrb = "all", label = FALSE, square = TRUE, diagonal = FALSE, rotate = FALSE) {
  if (!is.data.frame(data) & !is.matrix(data)) {
    stop("Dataset should be a 'data.frame' or 'matrix'.")
  }
  if (vrb[1] == "all") {
    vrb <- names(data)
  }
  p <- length(vrb)
  corrs <- data.frame(
    vrb = rep(vrb, each = p),
    prd = vrb,
    corr = matrix(round(stats::cov2cor(stats::cov(data.matrix(data[, vrb]), use = "pairwise.complete.obs")), 2), nrow = p * p, byrow = TRUE)
  )
  if (!diagonal) {
    corrs[corrs$vrb == corrs$prd, "corr"] <- NA
  }
  gg <- ggplot2::ggplot(corrs, ggplot2::aes(x = .data$prd, y = .data$vrb, label = .data$corr, fill = .data$corr)) +
    ggplot2::geom_tile(color = "black", alpha = 0.6) +
    ggplot2::scale_x_discrete(limits = vrb, position = "top") +
    ggplot2::scale_y_discrete(limits = rev(vrb)) +
    ggplot2::scale_fill_gradient2(low = ggplot2::alpha("deepskyblue", 0.6), mid = "lightyellow", high = ggplot2::alpha("orangered", 0.6), na.value = "white", limits = c(-1, 1)) +
    ggplot2::labs(
      x = "Imputation model predictor",
      y = "Variable to impute",
      fill = "Correlation*
      ",
      caption = "*pairwise complete observations"
    ) +
    theme_minimice()
  if (label) {
    gg <- gg + ggplot2::geom_text(color = "black", show.legend = FALSE, na.rm = TRUE)
  }
  if (square) {
    gg <- gg + ggplot2::coord_fixed()
  }
  if (rotate) {
    gg <- gg + ggplot2::theme(axis.text.x.top = ggplot2::element_text(angle = 90))
  }
  return(gg)
}

# TODO: add plot for missingness indicators predictors
# TODO: maybe add model.matrix argument to correlation plot?
