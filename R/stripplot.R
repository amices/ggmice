#' Stripplot of observed and imputed data
#'
#' @param imp A `mids` object, typically created by `mice()` or `mice.mids()`.
#' @param vrb String or vector with variable name(s), default is "all".
#'
#' @return A list with one or more `ggplot` objects.
#' @export
#'
#' @examples
#' imp <- mice::mice(mice::nhanes, printFlag = FALSE)
#' stripplot(imp, "bmi")
stripplot <- function(imp, vrb = "all") {
  if (!mice::is.mids(imp)) {
    stop("argument 'imp' must be a 'mids' object", call. = FALSE)
  }
  if (vrb == "all") {
    vrb <- names(imp$data)
  }
  gg <- purrr::map(vrb, ~ {
    ggmice(imp, ggplot2::aes_string(x = ".imp", y = .x)) +
      ggplot2::geom_jitter(width = 0.25, height = 0) +
      ggplot2::scale_x_discrete(drop = FALSE) +
      ggplot2::labs(x = "Imputation number\n(0 = original data)")
  }) %>% stats::setNames(vrb)
  return(gg)
}

# TODO: add vignette with stripplot() + geom_boxplot(alpha = 0.5, outlier.shape = NA)
# TODO: add vertical jitter or warning for categorical variables
