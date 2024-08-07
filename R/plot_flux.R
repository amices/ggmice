#' Plot the influx and outflux of a multivariate missing data pattern
#'
#' @param data An incomplete dataset of class `data.frame` or `matrix`.
#' @param vrb String, vector, or unquoted expression with variable name(s), default is "all".
#' @param label Logical indicating whether variable names should be displayed within the plot (the default) or with colors in the legend.
#' @param caption Logical indicating whether the figure caption should be displayed.
#'
#' @return An object of class [ggplot2::ggplot].
#'
#' @examples
#' # plot flux for all columns
#' plot_flux(mice::nhanes)
#'
#' # plot flux for specific columns by supplying a character vector
#' plot_flux(mice::nhanes, c("chl", "hyp"))
#'
#' # plot flux for specific columns by supplying unquoted variable names
#' plot_flux(mice::nhanes, c(chl, hyp))
#'
#' # plot flux for specific columns by passing an object with variable names
#' # from the environment, unquoted with `!!`
#' my_variables <- c("chl", "hyp")
#' plot_flux(mice::nhanes, !!my_variables)
#' # object with variable names must be unquoted with `!!`
#' try(plot_flux(mice::nhanes, my_variables))
#'
#' @export
plot_flux <-
  function(data,
           vrb = "all",
           label = TRUE,
           caption = TRUE) {
    verify_data(data, df = TRUE)
    vrb <- rlang::enexpr(vrb)
    vrb_matched <- match_vrb(vrb, names(data))
    if (length(vrb_matched) < 2) {
      cli::cli_abort("The number of variables should be two or more to compute flux.")
    }
    # compute flux
    flx <- mice::flux(data[, vrb_matched])[, c("influx", "outflux")]
    # create plot
    gg <-
      data.frame(
        vrb = rownames(flx),
        flx,
        outflux = flx$outflux - 0.025
      ) %>%
      ggplot2::ggplot(
        ggplot2::aes(
          x = .data$influx,
          y = .data$outflux,
          color = .data$vrb,
          label = .data$vrb
        )
      ) +
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
        ggplot2::geom_text(
          color = "black",
          position = ggplot2::position_nudge(y = 0.025)
        )
    } else {
      gg <- gg +
        ggplot2::geom_point(
          shape = 1,
          position = ggplot2::position_nudge(y = 0.025)
        ) +
        ggplot2::labs(color = "")
    }
    if (caption) {
      gg <- gg +
        ggplot2::labs(
          x = "Influx*",
          y = "Outflux**",
          caption = "*connection of a variable's missingness indicator with observed data on other variables\n
          **connection of a variable's observed data with missing data on other variables"
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
