# mice plotting functions

#' Box-and-whisker plot of observed and imputed data
#'
#' @param imp A `mids` object, typically created by `mice::mice()` or `mice::mice.mids()` (see `?mice::mids`).
#' @param vrb String or vector with variable name(s), default is "all".
#'
#' @return A (list with) `ggplot` object(s).
#' @export
#'
#' @examples
#' imp <- mice::mice(mice::nhanes, printFlag = FALSE)
#' bwplot(imp, "bmi")
bwplot <- function(imp, vrb = "all") {
  if (!mice::is.mids(imp)) {
    stop("argument 'imp' must be a 'mids' object", call. = FALSE)
  }
  if (vrb == "all") {
    vrb <- names(imp$data)
  }
  gg <- purrr::map(vrb, ~ {
    ggmice(imp, ggplot2::aes_string(x = ".imp", y = .x)) +
      ggplot2::geom_boxplot() +
      ggplot2::scale_x_discrete(drop = FALSE) +
      ggplot2::labs(x = "Imputation number\n(0 = original data)")
  }) %>% stats::setNames(vrb)
  if (length(vrb) == 1) {
    return(gg[[1]])
  } else {
    return(gg)
  }
}

#' Densityplot of observed and imputed data
#'
#' @param imp A `mids` object, typically created by `mice::mice()` or `mice::mice.mids()` (see `?mice::mids`).
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

#' Stripplot of observed and imputed data
#'
#' @param imp A `mids` object, typically created by `mice::mice()` or `mice::mice.mids()` (see `?mice::mids`).
#' @param vrb String or vector with variable name(s), default is "all".
#'
#' @return A (list with) `ggplot` object(s).
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
  if (length(vrb) == 1) {
    return(gg[[1]])
  } else {
    return(gg)
  }
}
# TODO: add vignette with stripplot() + geom_boxplot(alpha = 0.5, outlier.shape = NA)
# TODO: add vertical jitter or warning for categorical variables

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