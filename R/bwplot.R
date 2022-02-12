# bwplot
#' Title
#'
#' @param imp An object of class \code{mids}
#' @param x A variable name
#' @param y An optional second variable name
#'
#' @return An object of class \code{ggplot}
#'
#' @examples
#' imp <- mice::mice(mice::nhanes, print = FALSE)
#' plot_imps(imp, x = "bmi")
#' @export
plot_imps <- function(imp, x, y = NULL) {
  type = "bwplot" #c("bwplot", "stripplot", "densityplot")
  # pre-process mids object for plotting
  if(is.null(y)){y <- x}
  comp <- rbind(cbind(.imp = 0, .id = rownames(imp$data), imp$data),
                mice::complete(imp, "long")[is.na(imp$data[, x]) | is.na(imp$data[, y]), ])

  # basic plot object
  p <- ggplot2::ggplot(comp) +
    ggplot2::theme_classic() +
    ggplot2::scale_color_manual(
      values = c("#B61A51B3", "#006CC2B3"),
      labels = c("Imputed", "Observed"))

  # bwplot or stripplot
  if (type == "stripplot") {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(x = .imp, y = get(x), group = factor(.imp, ordered = TRUE), color = ifelse(.imp < 1, "Observed", "Imputed")),
      position = ggplot2::position_jitter(width = 0.25, height = 0),
      data = p$data)
  }
  if (type == "stripplot" | type == "bwplot"){
    p <- p + ggplot2::geom_boxplot(
      ggplot2::aes(x = .imp, y = get(x), group = factor(.imp, ordered = TRUE), color = ifelse(.imp < 1, "Observed", "Imputed")),
      size = 1,
      width = 0.5,
      alpha = 0.5,
      outlier.shape = NA,
      data = p$data) +
      ggplot2::labs(x = "Imputation",
           y = x,
           color = "",
           fill = "")
  }

  # densityplot
  if (type == "densityplot"){
    p <-  p + ggplot2::geom_density(
      ggplot2::aes(x = get(x), group = factor(.imp, ordered = TRUE), color = ifelse(.imp < 1, "Observed", "Imputed")),
      data = p$data) +
      ggplot2::labs(x = x,
           color = "",
           fill = "")
  }

  # # xyplot
  if (type == "xyplot") {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(x = get(x), y = get(y), color = ifelse(.imp < 1, "Observed", "Imputed")),
      data = p$data) +
      ggplot2::labs(x = x,
           y = y,
           color = "")
  }

  # output
  return(p)
}
