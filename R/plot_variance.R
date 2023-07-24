#' Plot the scaled between imputation variance for every cell as a heatmap
#'
#' This function plots the cell-level between imputation variance. The function
#' scales the variances column-wise, without centering cf. `base::scale(center = FALSE)`
#' and plots the data image as a heatmap. Darker red cells indicate more variance,
#' lighter cells indicate less variance. White cells represent observed cells or unobserved cells with zero between
#' imputation variance.
#'
#' @param data A multiply imputed object of class [`mice::mids`].
#' @param grid Logical indicating whether grid lines should be displayed.
#' @param caption Logical indicating whether the figure caption should be displayed.
#'
#' @return An object of class `ggplot`.
#' @examples
#' imp <- mice::mice(mice::nhanes, printFlag = FALSE)
#' plot_variance(imp)
#' @export
plot_variance <- function(data, grid = TRUE, caption = TRUE) {
  verify_data(data, imp = TRUE)
  if (data$m < 2) {
    cli::cli_abort(
      c(
        "The between imputation variance cannot be computed if there are fewer than two imputations (m < 2).",
        "i" = "Please provide an object with 2 or more imputations."
      )
    )
  }
  if (grid) {
    gridcol <- "black"
  } else {
    gridcol <- NA
  }
  if (caption) {
    lab_fill <- "Imputation variability*
      "
    lab_cap <- "*scaled cell-level between imputation variance"
  } else {
    lab_fill <- "Imputation variability"
    lab_cap <- NULL
  }

  gg <- mice::complete(data, "long") %>%
    dplyr::mutate(dplyr::across(where(is.factor), as.numeric)) %>%
    dplyr::select(-.imp) %>%
    dplyr::group_by(.id) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), stats::var)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dplyr::across(.cols = -.id, ~ scale_above_zero(.))) %>%
    tidyr::pivot_longer(cols = -.id) %>%
    ggplot2::ggplot(ggplot2::aes(name, .id, fill = value)) +
    ggplot2::geom_tile(color = gridcol) +
    ggplot2::scale_fill_gradient(low = "white", high = mice::mdc(2)) +
    ggplot2::labs(
      x = "Column name",
      y = "Row number",
      fill = lab_fill,
      caption = lab_cap
    ) +
    ggplot2::scale_x_discrete(position = "top", expand = c(0, 0)) +
    ggplot2::scale_y_continuous(trans = "reverse", expand = c(0, 0)) +
    theme_minimice()

  if (!grid) {
    gg <-
      gg + ggplot2::theme(panel.border = ggplot2::element_rect(fill = NA))
  }

  # return the ggplot object
  return(gg)
}

#' Utils function to scale only non-zero values without centering
#'
#' @param x An object of class 'matrix' or 'data.frame'
#'
#' @return A matrix with the scaled data
#'
#' @keywords internal
#' @noRd
scale_above_zero <- function(x) {
  x <- as.matrix(x)
  x[x != 0] <- scale(x[x != 0], center = FALSE)
  return(x)
}
