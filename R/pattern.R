# md pattern plot
#' Plot the missing data pattern of an incomplete dataset
#'
#' @param dat An incomplete data.frame.
#'
#' @return An object of class `ggplot`
#' @examples
#' plot_pattern(mice::nhanes)
#' @export
plot_pattern <- function(dat) {
  return(ggplot2::ggplot(dat))
}
