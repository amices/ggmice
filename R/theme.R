# mice ggplot2 theme
#' Theme for 'mice' style 'ggplot2' objects
#'
#' @return A ggplot2 theme.
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

#' Minimal theme for mice
#'
#' @return A ggplot2 theme.
theme_minimice <- function() {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      # text = ggplot2::element_text(family="sans"),
      legend.position = "bottom",
      legend.justification = "right",
      strip.placement = "outside",
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "grey95"),
      axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(l = 6))
    )
}

# TODO: make facets in plot_trace() look more pretty
