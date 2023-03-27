#' Plot pairwise variable-missingness indicator tests
#'
#' Performs t-tests and Fisher's exact tests to compare variables and their missing data indicators in a matrix format.
#'
#' @param data A dataset of class `data.frame`, `tibble`, or `matrix`.
#' @param vrb String or vector with variable name(s), default is "all".
#' @param square Logical indicating whether the plot tiles should be squares.
#' @param rotate Logical indicating whether the variable name labels should be rotated 90 degrees.
#' @param label Logical indicating whether correlation values should be displayed.
#'
#' @return An object of class [ggplot2::ggplot].
#'
#' @examples
#' plot_tests(mice::nhanes, label = TRUE)
#' @export
plot_tests <- function(data, vrb = "all", square = TRUE, rotate = FALSE, label = FALSE) {
  if (!is.data.frame(data) & !is.matrix(data)) {
    stop("Dataset should be a 'data.frame' or 'matrix'.")
  }
  if (is.matrix(data)) data <- as.data.frame(data)
  if (vrb == "all") vrb <- colnames(data)
  if (any(!(vrb %in% colnames(data)))) {
    stop("Supplied variable name(s) not found in the dataset.")
  }

  R <- is.na(data)
  p <- length(vrb)
  vrbs_num <- vrb[purrr::map_lgl(data, is.numeric)]
  vrbs_complete <- vrb[!purrr::map_lgl(data, anyNA)]

  pval <- purrr::map_dfr(vrb, function(.y) {
    if (.y %in% vrbs_num) {
      ps <- purrr::map_dbl(vrb, function(.x) {
        tryCatch({
          stats::t.test(data[, .y] ~ R[, .x])$p.value
        },
        error = function(e) {NA})
      })
    }
    if (.y %nin% vrbs_num) {
      ps <- purrr::map_dbl(vrb, function(.x) {
        tryCatch({
          stats::fisher.test(table(data[, .y], R[, .x]), simulate.p.value = TRUE)$p.value
        },
        error = function(e) {NA})
      })
    }
    return(stats::setNames(ps, vrb))
  })

  tests <- data.frame(
    vrb = rep(vrb, each = p),
    prd = vrb,
    test = matrix(as.matrix(pval), nrow = p * p, byrow = TRUE))
  tests$text = round(tests$test, 3)
  tests$text[tests$vrb %in% vrbs_complete] <- "(a)"
  tests$text[is.na(tests$text)] <- "(b)"
  tests$text[tests$vrb == tests$prd] <- ""

 gg <- ggplot2::ggplot(tests, ggplot2::aes(x = .data$prd, y = .data$vrb, label = .data$text, fill = .data$test)) +
    ggplot2::geom_tile(color = "black", alpha = 0.6) +
    ggplot2::scale_x_discrete(limits = vrb, position = "top") +
    ggplot2::scale_y_discrete(limits = rev(vrb)) +
    ggplot2::scale_fill_gradient(high = "white", low = "#B61A51B3", na.value = "grey90", limits = c(0, 1)) +
    ggplot2::labs(
      x = "Imputation model predictor",
      y = "Nonresponse indicator",
      fill = "Test p-value
      ",
      caption = "(a) Completely observed column; \n(b) Missing data pattern overlapping."
      ) +
    theme_minimice()


  if (label) {
    gg <- gg + ggplot2::geom_text(color = "black", show.legend = FALSE, na.rm = TRUE)
  }
  if (square) {
    gg <- gg + ggplot2::coord_fixed(expand = FALSE)
  } else {
    gg <- gg + ggplot2::coord_cartesian(expand = FALSE)
  }
  if (rotate) {
    gg <- gg + ggplot2::theme(axis.text.x.top = ggplot2::element_text(angle = 90))
  }

  return(gg)
}

