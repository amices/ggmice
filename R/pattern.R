# md pattern plot
#' Plot the missing data pattern of an incomplete dataset
#'
#' @param dat An incomplete dataset of class `data.frame`, `tibble`, or `matrix`.
#' @param square Logical indicating whether the plot tiles should be squares.
#' @param rotate Logical indicating whether the variable name labels should be rotated 90 degrees.
#'
#' @return An object of class `ggplot`.
#' @examples
#' plot_pattern(mice::nhanes)
#' @export
plot_pattern <- function(dat, square = FALSE, rotate = FALSE) {
  if (!is.data.frame(dat) & !is.matrix(dat)) {
    stop("Dataset should be a 'data.frame' or 'matrix'.")
  }
  # get missing data pattern and extract info
  pat <- mice::md.pattern(dat, plot = FALSE)
  rws <- nrow(pat)
  cls <- ncol(pat)
  vrb <- colnames(pat)[-cls]
  frq <- row.names(pat)[-rws]
  na_row <- pat[-rws, cls]
  na_col <- pat[rws, -cls]
  na_tot <- pat[rws, cls]

  # tidy the pattern
  long <- data.frame(y = 1:(rws - 1), pat[-rws, -cls], row.names = NULL) %>%
    tidyr::pivot_longer(cols = vrb, names_to = "x", values_to = ".where") %>%
    dplyr::mutate(
      x = as.numeric(factor(.data$x, levels = vrb, ordered = TRUE)),
      .where = factor(.data$.where, levels = c(0, 1), labels = c("missing", "observed"))
    )

  # create the plot
  gg <- ggplot2::ggplot(long, ggplot2::aes(.data$x, .data$y, fill = .data$.where)) +
    ggplot2::geom_tile(color = "black") +
    ggplot2::scale_fill_manual(values = c("observed" = "#006CC2B3", "missing" = "#B61A51B3")) +
    ggplot2::scale_x_continuous(
      breaks = 1:(cls - 1),
      labels = na_col,
      sec.axis = ggplot2::dup_axis(
        labels = vrb,
        name = ""
      )
    ) +
    ggplot2::scale_y_reverse(
      breaks = 1:(rws - 1),
      labels = frq,
      sec.axis = ggplot2::dup_axis(
        labels = na_row,
        name = "Number of missing entries per pattern*\n"
      )
    ) +
    ggplot2::labs(
      x = "Number of missing entries per variable*",
      y = "Pattern frequency",
      fill = "",
      caption = paste("*total number of missing entries =", na_tot)
    ) +
    theme_minimice()
  if (square) {
    gg <- gg + ggplot2::coord_fixed()
  }
  if (rotate) {
    gg <- gg + ggplot2::theme(axis.text.x.top = ggplot2::element_text(angle = 90))
  }

  return(gg)
}
