#' Scatterplot of observed and imputed data
#'
#' @param imp A `mids` object, typically created by `mice::mice()` or `mice::mice.mids()` (see `?mice::mids`).
#' @param x,y String with a variable name.
#'
#' @return A `ggplot` object
#' @export
#'
#' @examples
#' imp <- mice::mice(mice::nhanes, printFlag = FALSE)
#' xyplot(imp, "age", "bmi")
xyplot <- function(imp, x, y) {
  if (!mice::is.mids(imp)) {
    stop("argument 'imp' must be a 'mids' object", call. = FALSE)
  }
  gg <- ggmice(imp, ggplot2::aes_string(x, y)) +
    ggplot2::geom_point()
  return(gg)
}

# TODO: think about how to plot just one imp
