#' Plot the influx and outflux of a multivariate missing data pattern
#'
#' @param data An incomplete dataset of class `data.frame` or `matrix`.
#' @param vrb String or vector with variable name(s), default is "all".
#' @param label Logical indicating whether variable names should be displayed within the plot (the default) or with colors in the legend.
#' @param caption Logical indicating whether the figure caption should be displayed.
#'
#' @return An object of class `ggplot2::ggplot`.
#'
#' @examples
#' plot_flux(mice::nhanes)
#' @export
plot_flux <- function(data, vrb = "all", label = TRUE, caption = TRUE) {
  if (vrb == "all") {
    vrb <- names(data)
  }
  # plot in and outflux
  flx <- mice::flux(data[, vrb])[, c("influx", "outflux")]
  gg <- data.frame(vrb = rownames(flx), flx, outflux_nudge = flx$outflux - 0.025) %>%
    ggplot2::ggplot(ggplot2::aes(
      x = .data$influx,
      y = .data$outflux_nudge,
      color = .data$vrb,
      label = .data$vrb
    )) +
    ggplot2::geom_abline(
      intercept = 1,
      slope = -1,
      linetype = "dashed"
    ) +
    ggplot2::lims(x = c(-0.05, 1.05), y = c(-0.05, 1.05)) +
    ggplot2::coord_cartesian(clip = "off") +
    theme_mice()
  if (label) {
    gg <- gg +
      ggplot2::geom_text(color = "black", position = ggplot2::position_nudge(y = 0.025)) # position = ggplot2::position_jitter(width = 0.05, height = 0),  hjust = "outward"
  } else {
    gg <- gg +
      ggplot2::geom_point(shape = 1, position = ggplot2::position_nudge(y = 0.025)) +
      ggplot2::labs(color = "")
  }
  if (caption) {
    gg <- gg +
      ggplot2::labs(
        x = "Influx*",
        y = "Outflux**",
        caption = "*connection of a variable's missingness indicator with observed data on other variables\n **connection of a variable's observed data with missing data on other variables"
      )
  } else {
    gg <- gg +
      ggplot2::labs(
        x = "Influx",
        y = "Outflux"
      )
  }
  # output
  return(gg)
}
