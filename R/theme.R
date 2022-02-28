# mice ggplot2 theme
#' Title
#'
#' @return A ggplot2 theme
#'
#' @export
theme_mice <- function() {
  ggplot2::theme_classic() +
    ggplot2::theme(
      # text = ggplot2::element_text(family="sans"),
      legend.position = "bottom",
      legend.justification = "right",
      strip.placement = "outside"
    )
}
