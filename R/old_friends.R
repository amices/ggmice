# Plotting functions from the `mice` package

#' Box-and-whisker plot of observed and imputed data
#'
#' @param ... Any arguments passed to the function.
#'
#' @return The output of `mice::bwplot(...)` and a message about the `ggmice` equivalent.
#' @export
#'
#' @examples
#' imp <- mice::mice(mice::nhanes, maxit = 1, printFlag = FALSE)
#' bwplot(imp)
bwplot <- function(...) {
  message("Hint: Did you know, an equivalent figure can be created with `ggmice()`?\nFor example, to plot a variable named 'my_vrb' from a mids object called 'my_mids', run: \n
    ggmice(my_mids, ggplot2::aes(x = .imp, y = my_vrb)) +
    ggplot2::geom_boxplot() \n\nSee amices.org/ggmice for more info.")
  mice::bwplot(...)
}

#' Densityplot of observed and imputed data
#'
#' @param ... Any arguments passed to the function.
#'
#' @return The output of `mice::densityplot(...)` and a message about the `ggmice` equivalent.
#' @export
#'
#' @examples
#' imp <- mice::mice(mice::nhanes, maxit = 1, printFlag = FALSE)
#' densityplot(imp)
densityplot <- function(...) {
  message("Hint: Did you know, an equivalent figure can be created with `ggmice()`?\nFor example, to plot a variable named 'my_vrb' from a mids object called 'my_mids', run: \n
    ggmice(my_mids, ggplot2::aes(x = my_vrb, group = .imp)) +
    ggplot2::geom_density() \n\nSee amices.org/ggmice for more info.")
  mice::densityplot(...)
}

#' Stripplot of observed and imputed data
#'
#' @param ... Any arguments passed to the function.
#'
#' @return The output of `mice::stripplot(...)` and a message about the `ggmice` equivalent.
#' @export
#'
#' @examples
#' imp <- mice::mice(mice::nhanes, maxit = 1, printFlag = FALSE)
#' stripplot(imp)
stripplot <- function(...) {
  message("Hint: Did you know, an equivalent figure can be created with `ggmice()`?\nFor example, to plot a variable named 'my_vrb' from a mids object called 'my_mids', run: \n
    ggmice(my_mids, ggplot2::aes(x = .imp, y = my_vrb)) +
    ggplot2::geom_jitter() \n\nSee amices.org/ggmice for more info.")
  mice::stripplot(...)
}

#' Scatterplot of observed and imputed data
#'
#' @param ... Any arguments passed to the function.
#'
#' @return The output of `mice::xyplot(...)` and a message about the `ggmice` equivalent.
#' @export
#'
#' @examples
#' imp <- mice::mice(mice::nhanes, maxit = 1, printFlag = FALSE)
#' xyplot(imp, bmi ~ age)
xyplot <- function(...) {
  message("Hint: Did you know, an equivalent figure can be created with `ggmice()`?\nFor example, to plot 2 variables named 'my_x' and 'my_y' from a mids object called 'my_mids', run: \n
    ggmice(my_mids, ggplot2::aes(x = my_x, y = my_y)) +
    ggplot2::geom_point() \n\nSee amices.org/ggmice for more info.")
  mice::xyplot(...)
}
