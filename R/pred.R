#' Plot the predictor matrix of an imputation model
#'
#' @param data A predictor matrix for `mice`, typically generated with [mice::make.predictorMatrix] or [mice::quickpred].
#' @param method Character string or vector with imputation methods.
#' @param label Logical indicating whether predictor matrix values should be displayed.
#' @param square Logical indicating whether the plot tiles should be squares.
#' @param rotate Logical indicating whether the variable name labels should be rotated 90 degrees.
#'
#' @return An object of class `ggplot2::ggplot`.
#'
#' @examples
#' pred <- mice::quickpred(mice::nhanes)
#' plot_pred(pred)
#' @export
plot_pred <- function(data, method = NULL, label = TRUE, square = TRUE, rotate = FALSE) {
  if (!is.matrix(data) | dim(data)[1] != dim(data)[2]) {
    stop("Predictor matrix should be a square matrix, try using mice::make.predictorMatrix() or mice::quickpred().")
  }
  p <- nrow(data)
  if (!is.null(method) & is.character(method)) {
    if (length(method) == 1) {
      method <- rep(method, p)
    }
    if (length(method) == p) {
      ylabel <- "Imputation method"
    }
  }
  if (is.null(method)) {
    method <- rep("", p)
    ylabel <- ""
  }
  if (!is.character(method) | length(method) != p) {
    stop("Method should be NULL or a character string or vector (of length 1 or `ncol(data)`).")
  }

  vrbs <- row.names(data)
  long <- data.frame(
    vrb = 1:p,
    prd = rep(vrbs, each = p),
    ind = matrix(data, nrow = p * p, byrow = TRUE)
  ) %>% dplyr::mutate(
    clr = factor(
      .data$ind,
      levels = c(-2, 0, 1, 2, 3),
      labels = c("cluster variable", "not used", "predictor", "random effect", "inclusion-restriction variable"),
      ordered = TRUE
    )
  )

  gg <- ggplot2::ggplot(long, ggplot2::aes(x = .data$prd, y = .data$vrb, label = .data$ind, fill = .data$clr)) +
    ggplot2::geom_tile(color = "black", alpha = 0.6) +
    ggplot2::scale_x_discrete(limits = vrbs, position = "top") +
    ggplot2::scale_y_reverse(
      breaks = 1:p,
      labels = vrbs,
      sec.axis = ggplot2::dup_axis(labels = method, name = ylabel)) +
    ggplot2::scale_fill_manual(values = c(
      "cluster variable" = "lightyellow",
      "not used" = "grey90",
      "predictor" = "palegreen3",
      "random effect" = "deepskyblue",
      "inclusion-restriction variable" = "orangered"
    )) +
    ggplot2::labs(
      x = "Imputation model predictor",
      y = "Variable to impute",
      fill = "",
      color = ""
    ) +
    theme_minimice()

  if (label) {
    gg <- gg + ggplot2::geom_text(color = "black", show.legend = FALSE)
  }
  if (square) {
    gg <- gg + ggplot2::coord_fixed(expand = FALSE)
  } else {
    gg <- gg + ggplot2::coord_cartesian(expand = FALSE)
  }
  if (rotate) {
    gg <- gg + ggplot2::theme(axis.text.x.top = ggplot2::element_text(angle = 90))
  }
  return(gg)
}
