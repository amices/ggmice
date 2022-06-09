#' Plot the scaled between imputation variance for every cell as a heatmap
#'
#' This function plots the cell-level between imputation variance. The function
#' scales the variances column-wise, without centering cf. `base::scale(center = FALSE)`
#' and plots the data image as a heatmap. Darker red cells indicate more variance,
#' lighter cells indicate less variance. White cells represent observed cells or unobserved cells with zero between
#' imputation variance.
#'
#' @param data A package `mice` generated multiply imputed data set of class
#' `mids`. Non-`mids` objects that have not been generated with `mice::mice()`
#' can be converted through a pipeline with `mice::as.mids()`.
#' @param grid Logical indicating whether grid lines should be displayed.
#'
#' @return An object of class `ggplot`.
#' @examples
#' imp <- mice::mice(mice::nhanes, printFlag = FALSE)
#' plot_variance(imp)
#' @export
plot_variance <- function(data, grid = TRUE){
  if (!mice::is.mids(data)) {
    stop("Input is not a Multiply Imputed Data Set of class mids. \n
         Perhaps function mice::as.mids() can be of use?")
  }
  if (data$m < 2) {
    stop("The between inmputation variance cannot be computed if there are fewer than two imputations (m < 2).")
  }
  if (grid) {
    gridcol <- "black"
  } else {
    gridcol <- NA
  }

  gg <- mice::complete(data, "long")  %>%
    dplyr::mutate(dplyr::across(where(is.factor), as.numeric)) %>%
    dplyr::select(-.imp) %>%
    dplyr::group_by(.id) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(),  stats::var)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dplyr::across(.cols = -.id, ~scale_above_zero(.))) %>%
    tidyr::pivot_longer(cols = -.id) %>%
    ggplot2::ggplot(ggplot2::aes(name, .id, fill= value)) +
    ggplot2::geom_tile(color = gridcol) +
    ggplot2::scale_fill_gradient(low = "white", high = mice::mdc(2)) +
    ggplot2::labs(x = "Column name",
                  y = "Row number",
                  fill = "Imputation variability*
      ",
      caption = "*scaled cell-level between imputation variance") + # "Cell-level between imputation\nvariance (scaled)\n\n"
    ggplot2::scale_x_discrete(position = "top", expand = c(0,0)) +
    ggplot2::scale_y_continuous(trans = "reverse", expand = c(0,0)) +
    theme_minimice()

  if (!grid) {
    gg <- gg + ggplot2::theme(panel.border = ggplot2::element_rect(fill = NA))
  }

  # return the ggplot object
  return(gg)
}

# function to scale only non-zero values without centering
scale_above_zero <- function(x){
  x <- as.matrix(x)
  x[x!=0] <- scale(x[x!=0], center = FALSE)
  return(x)
}
