#' Plot as a heatmap the scaled between imputation variance for every cell
#'
#' This function plots the cell-level between imputation variance. The function
#' scales the variances column-wise and plots the data image as a heatmap.
#' Darker red cells indicate more variance, lighter cells indicate less variance.
#' white cells represent observed cells or unobserved cells with zero between
#' imputation variance.
#'
#' @param object A `mice` generated multiply imputed data set of class `mids`.
#'
#' @return An object of class `ggplot`.
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom dplyr everything
#' @importFrom ggplot2 aes
#' @importFrom mice mdc
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom tidyr pivot_longer
#' @importFrom stats var
#' @examples
#' library(mice)
#' library(dplyr)
#' library(tidyr)
#' library(ggplot2)
#' imp <- mice(nhanes)
#' plot_variance(imp)
#' @export
plot_variance <- function(object){
  if (!mice::is.mids(object)) {
    stop("Input is not a Multiply Imputed Data Set of class mids. \n
         Perhaps function mice::as.mids() can be of use?")
  }
  gg <- mice::complete(object, "long")  %>%
    dplyr::mutate(across(where(is.factor), as.numeric)) %>%
    dplyr::select(-.imp) %>%
    dplyr::group_by(.id) %>%
    dplyr::summarise(across(everything(),  stats::var)) %>%
    dplyr::ungroup() %>%
    mutate(across(.cols = -.id, ~ scale_above_zero(.))) %>%
    pivot_longer(cols = -.id) %>%
    ggplot2::ggplot(aes(name, .id, fill= value)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient(low = "white", high = mdc(2)) +
    ggplot2::labs(x = "column",
                  y = "rows",
                  fill = "scaled variance",
                  title =  "Heatmap-like plot of scaled between imputation variances for all cells") +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::scale_y_continuous(trans = "reverse") +
    ggplot2::theme_light() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank())

  # return the ggplot object
  return(gg)
}

# function to scale only non-zero values without centering
scale_above_zero <- function(x){
  x <- as.matrix(x)
  x[x!=0] <- scale(x[x!=0], center = FALSE)
  return(x)
}
