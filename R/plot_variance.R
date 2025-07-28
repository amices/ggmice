#' Plot the scaled between imputation variance for every cell as a heatmap
#'
#' This function plots the cell-level between imputation variance. The function
#' scales the variances column-wise, without centering cf. `base::scale(center = FALSE)`
#' and plots the data image as a heatmap. Darker red cells indicate more variance,
#' lighter cells indicate less variance. White cells represent observed cells or unobserved cells with zero between
#' imputation variance.
#'
#' @param data A multiply imputed object of class [mice::mids].
#' @param vrb String, vector, or unquoted expression with variable name(s), default is "all".
#' @param grid Logical indicating whether borders should be present between tiles.
#' @param rotate Logical indicating whether the variable name labels should be rotated 90 degrees.
#' @param square  Logical indicating whether the plot tiles should be squares, defaults to squares.
#' @return An object of class [ggplot2::ggplot].
#'
#' @examples
#' # impute a dataset
#' imp <- mice::mice(mice::nhanes, print = FALSE)
#'
#' # plot correlations for all columns
#' plot_variance(imp)
#'
#' # plot correlations for specific columns by supplying a character vector
#' plot_variance(imp, c("chl", "hyp"))
#'
#' # plot correlations for specific columns by supplying unquoted variable names
#' plot_variance(imp, c(chl, hyp))
#'
#' # plot correlations for specific columns by passing an object with variable names
#' # from the environment, unquoted with `!!`
#' my_variables <- c("chl", "hyp")
#' plot_variance(imp, !!my_variables)
#' # object with variable names must be unquoted with `!!`
#' try(plot_variance(imp, my_variables))
#'
#' @export
plot_variance <-
  function(data,
           vrb = "all",
           rotate = FALSE,
           grid = FALSE,
           square = FALSE) {
    # input processing
    verify_data(data = data, imp = TRUE)
    vrb <- rlang::enexpr(vrb)
    vrb_matched <- match_vrb(vrb, names(data$data))
    # extract variances
    long <- mice::complete(data, "long")  %>%
      dplyr::mutate(dplyr::across(where(is.factor), as.numeric)) %>%
      dplyr::select(c(.id, vrb_matched)) %>%
      dplyr::group_by(.id) %>%
      dplyr::summarise(dplyr::across(dplyr::everything(), stats::var)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(dplyr::across(.cols = -.id, ~ scale_above_zero(.))) %>%
      tidyr::pivot_longer(cols = -.id)
    # plot
    gg <- ggplot2::ggplot(long, ggplot2::aes(name, as.numeric(.id), fill = value)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradient(low = "white", high = mice::mdc(2)) +
      ggplot2::labs(
        x = "Column name",
        y = "Row number",
        fill = "Imputation variability*
      ",
        caption = "*scaled cell-level between imputation variance"
      ) +
      ggplot2::scale_x_discrete(position = "top", expand = c(0, 0)) +
      ggplot2::scale_y_reverse(expand = c(0, 0)) +
      theme_minimice()

    # additional arguments
    if (grid) {
      gg <- gg + ggplot2::geom_tile(color = "black")
    } else{
      gg <- gg + ggplot2::geom_tile()
    }
    if (square) {
      gg <- gg + ggplot2::coord_fixed(expand = FALSE)
    } else {
      gg <- gg + ggplot2::coord_cartesian(expand = FALSE)
    }
    if (rotate) {
      gg <-
        gg + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
    }
    return(gg)
  }

# function to scale only non-zero values without centering
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
