#' Influx and outflux plot of multivariate missing data patterns
#'
#' @param dat An incomplete dataset of class `data.frame`, `tibble`, or `matrix`.
#' @param label Logical indicating whether variable names should be displayed within the plot (the default) or with colors in the legend.
#' @param caption Logical indicating whether the figure caption should be displayed.
#'
#' @return An object of class `ggplot`.
#'
#' @examples
#' plot_flux(mice::nhanes)
#' @export
plot_flux <- function(dat, label = TRUE, caption = TRUE) {
  # escape function if dataset is complete
  # if(!any(is.na(dat))){return(plot_a_mouse())}
  # plot in and outflux
  flx <- mice::flux(dat)[, c("influx", "outflux")]
  gg <- data.frame(vrb = rownames(flx), flx, outflux_nudge = flx$outflux - 0.025) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$influx,
                                 y = .data$outflux_nudge,
                                 color = .data$vrb,
                                 label = .data$vrb)) +
    ggplot2::geom_abline(intercept = 1,
                         slope = -1,
                         linetype = "dashed") +
    ggplot2::lims(x = c(-0.05, 1.05), y = c(-0.05, 1.05)) +
    ggplot2::coord_cartesian(clip = "off") +
    theme_mice()
  if (label) {
    gg <- gg +
      ggplot2::geom_text(color = "black", position = ggplot2::position_nudge(y = 0.025)) #position = ggplot2::position_jitter(width = 0.05, height = 0),  hjust = "outward"
  } else {
    gg <- gg +
      ggplot2::geom_point(shape = 1, position = ggplot2::position_nudge(y = 0.025)) +
      ggplot2::labs(color = "")
  }
  if (caption) {
    gg <- gg +
      ggplot2::labs(x = "Influx*",
                    y = "Outflux**",
                    caption = "*connection of a variable's missingness indicator with observed data on other variables\n **connection of a variable's observed data with missing data on other variables")
  } else {
    gg <- gg +
      ggplot2::labs(x = "Influx",
                    y = "Outflux")
  }
  # output
  return(gg)
}
