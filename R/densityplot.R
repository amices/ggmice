#' Densityplot of observed and imputed data
#'
#' @param imp A `mids` object, typically created by `mice()` or `mice.mids()`.
#' @param vrb String or vector with variable name(s), default is "all".
#'
#' @return A (list with) `ggplot` object(s).
#' @export
#'
#' @examples
#' imp <- mice::mice(mice::nhanes, printFlag = FALSE)
#' densityplot(imp, "bmi")
densityplot <- function(imp, vrb = "all") {
  if (!mice::is.mids(imp)) {
    stop("argument 'imp' must be a 'mids' object", call. = FALSE)
  }
  if (vrb == "all") {
    vrb <- names(imp$data)
  }
  gg <- purrr::map(vrb, ~ {
    ggmice(imp, ggplot2::aes_string(x = .x, group = ".imp", size = ".where")) +
      ggplot2::geom_density(fill = NA) +
      ggplot2::scale_size_manual(values = c("observed" = 1, "imputed" = 0.5), guide = "none")
  }) %>% stats::setNames(vrb)
  if (length(vrb) == 1) {
    return(gg[[1]])
  } else {
    return(gg)
  }
}
