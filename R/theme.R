#' Theme for [mice] style [ggplot2::ggplot] objects
#'
#' @return A [ggplot2] theme.
theme_mice <- function() {
  ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.justification = "right",
      strip.placement = "outside"
    )
}

#' Minimal theme for [mice] style [ggplot2::ggplot] objects
#'
#' @return A [ggplot2] theme.
theme_minimice <- function() {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.justification = "right",
      strip.placement = "outside",
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_line(colour = "black"),
      axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(l = 6))
    )
}
