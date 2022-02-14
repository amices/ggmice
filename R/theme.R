# mice ggplot2 theme
#' Title
#'
#' @return A ggplot2 theme
#'
#' @export
theme_mice <- function(){
  # param
  font <- "Arial"
  # theme
  ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "top",
      strip.placement = "outside"
    )
  }
