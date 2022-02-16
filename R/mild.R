# bwplot
#' Title
#'
#' @param imp An object of class `mids`
#' @param x A variable name
#' @param y An optional second variable name
#'
#' @return An object of class `ggplot`
#'
#' @examples
#' imp <- mice::mice(mice::nhanes, print = FALSE)
#' plot_imps(imp, x = "bmi")
#' @export
plot_imps <- function(imp, x, y = NULL) {
  type = "bwplot" #c("bwplot", "stripplot", "densityplot")
  # pre-process mids object for plotting
  if(is.null(y)){y <- x}
  imputed <- rbind(
    data.frame(.imp = 0, .id = rownames(imp$data), imp$data),
    mice::complete(imp, "long")[is.na(imp$data[, x]) | is.na(imp$data[, y]), ]
    )

  # basic plot object
  p <- ggplot2::ggplot(dplyr::mutate(imputed,
                       .R = factor(ifelse(.imp < 1, "Observed", "Imputed"), levels = c("Observed", "Imputed"), ordered = TRUE),
                       .imp = factor(.imp, levels = 0:imp$m, ordered = TRUE))) +
    theme_mice() +
    ggplot2::scale_color_manual(
      values = c("#006CC2B3", "#B61A51B3"),
      labels = c("Observed", "Imputed"),
      drop = FALSE)

  # bwplot or stripplot
  if (type == "stripplot") {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(x = .imp, y = get(x), group = .imp, color = .R),
      position = ggplot2::position_jitter(width = 0.25, height = 0),
      data = p$data)
  }
  if (type == "stripplot" | type == "bwplot"){
    p <- p + ggplot2::geom_boxplot(
      ggplot2::aes(x = .imp, y = get(x), group = .imp, color = .R),
      size = 1,
      width = 0.5,
      alpha = 0.5,
      # outlier.shape = NA,
      data = p$data) +
      ggplot2::scale_x_discrete(drop = FALSE) +
      ggplot2::labs(x = "Imputation",
           y = x,
           color = "",
           fill = "")
  }

  # densityplot
  if (type == "densityplot"){
    p <-  p + ggplot2::geom_density(
      ggplot2::aes(x = get(x), group = .imp, color = .R),
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
