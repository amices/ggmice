# plot_flux <- function(dat) {
#   # escape function if dataset is complete
#   # if(!any(is.na(dat))){return(plot_a_mouse())}
#   # plot in and outflux
#   flx <- mice::flux(dat) %>% cbind(variable = rownames(.))
#   gg <- flx %>%
#     ggplot2::ggplot(ggplot2::aes(x = influx,
#                                  y = outflux,
#                                  label = variable)) +
#     ggplot2::geom_abline(intercept = 1,
#                          slope = -1,
#                          linetype = "dashed") +
#     ggplot2::geom_text(position = ggplot2::position_jitter(width = 0.01, height = 0.01)) +
#     ggplot2::lims(x = c(-0.01, 1.01), y = c(-0.01, 1.01)) +
#     ggplot2::theme_classic()
#   # output
#   return(gg)
# }
