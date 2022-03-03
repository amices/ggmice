# plot predictor matrix
#' Title Plot predictorMatrix for `mice::mice()` argument
#'
#' @param pred A predictor matrix for `mice`.
#' @param label Logical indicating whether predictor matrix values should be displayed.
#' @param square Logical indicating whether the plot tiles should be squares (setting the plot height equal to the plot width).
#'
#' @return An object of class `ggplot`.
#'
#' @examples
#' pred <- mice::make.predictorMatrix(mice::nhanes)
#' plot_pred(pred, label = TRUE)
#' @export
plot_pred <- function(pred, label = FALSE, square = TRUE) {
  if (!is.matrix(pred) | dim(pred)[1] != dim(pred)[2]) {
    stop("Predictor matrix should be a square matrix, try using mice::make.predictorMatrix() or mice::quickpred.")
  }
  vrbs <- row.names(pred)
  p <- dim(pred)[2]
  long <- data.frame(
    prd = rep(vrbs, each = p),
    vrb = vrbs,
    ind = matrix(pred, nrow = p * p, byrow = TRUE)
  )
  gg <- ggplot2::ggplot(long, ggplot2::aes(x = .data$prd, y = .data$vrb, label = .data$ind, fill = ifelse(.data$ind == 0, "no", "yes"))) +
    ggplot2::geom_tile(color = "black", alpha = 1) +
    ggplot2::scale_x_discrete(limits = vrbs, position = "top") +
    ggplot2::scale_y_discrete(limits = rev(vrbs)) +
    ggplot2::scale_fill_manual(values = c("yes" = "#006CC2B3", "no" = "white")) +
    ggplot2::labs(
      x = "Imputation model predictor",
      y = "Variable to impute",
      fill = "Predictor used",
      color = ""
    ) +
    theme_minimice()
  if (label) {
    gg <- gg + ggplot2::geom_text(color = "black", show.legend = FALSE)
  }
  if (square) {
    gg <- gg + ggplot2::coord_equal()
  }
  return(gg)
}

#' Title Plot correlations between (incomplete) variables
#'
#' @param dat An incomplete data.frame.
#' @param vrb String or vector with variable name(s), default is "all".
#' @param label Logical indicating whether correlation values should be displayed.
#' @param square Logical indicating whether the plot tiles should be squares (setting the plot height equal to the plot width).
#' @param diagonal Logical indicating whether the correlation of each variable with itself should be displayed.
#'
#' @return An object of class `ggplot`
#'
#' @examples
#' plot_corr(mice::nhanes, label = TRUE)
#' @export
plot_corr <- function(dat, vrb = "all", label = FALSE, square = TRUE, diagonal = FALSE) {
  if (!is.data.frame(dat) & !is.matrix(dat)) {
    stop("Dataset should be a 'data.frame' or 'matrix'.")
  }
  if (vrb[1] == "all") {
    vrb <- names(dat)
  }
  p <- length(vrb)
  corrs <- data.frame(
    vrb = rep(vrb, each = p),
    prd = vrb,
    corr = matrix(round(stats::cov2cor(stats::cov(data.matrix(dat[, vrb]), use = "pairwise.complete.obs")), 2), nrow = p * p, byrow = TRUE)
  )
  if (!diagonal) {
    corrs[corrs$vrb == corrs$prd, "corr"] <- NA
  }
  gg <- ggplot2::ggplot(corrs, ggplot2::aes(x = .data$prd, y = .data$vrb, label = .data$corr, fill = .data$corr)) +
    ggplot2::geom_tile(color = "black") +
    ggplot2::scale_x_discrete(limits = vrb, position = "top") +
    ggplot2::scale_y_discrete(limits = rev(vrb)) +
    ggplot2::scale_fill_gradient2(low = "deepskyblue", mid = "lightyellow", high = "orangered", na.value = "white", limits = c(-1, 1)) +
    ggplot2::labs(
      x = "Imputation model predictor",
      y = "Variable to impute",
      fill = "Correlation*\n",
      caption = "*paiwise complete observations"
    ) +
    theme_minimice()
  if (label) {
    gg <- gg + ggplot2::geom_text(color = "black", show.legend = FALSE)
  }
  if (square) {
    gg <- gg + ggplot2::coord_equal()
  }
  return(gg)
}

# TODO: add plot for missingness indicators predictors
# TODO: maybe add model.matrix argument to correlation plot?
# TODO: add argument to rotate/shorten variable names
