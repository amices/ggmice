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
#' plot_tests(mice::nhanes, rotate = TRUE)
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
  tests$text[tests$vrb == tests$prd] <- ""
  tests$text[tests$vrb %in% vrbs_complete] <- "(a)"
  tests$text[is.na(tests$text)] <- "(b)"

 ggplot2::ggplot(tests, ggplot2::aes(x = .data$prd, y = .data$vrb, label = .data$text, fill = .data$test)) +
    ggplot2::geom_tile(color = "black", alpha = 0.6) +
    ggplot2::scale_x_discrete(limits = vrb, position = "top") +
    ggplot2::scale_y_discrete(limits = rev(vrb)) +
    ggplot2::scale_fill_gradient(high = "white", low = "#B61A51B3", na.value = "grey90", limits = c(0, 1)) +
    ggplot2::labs(
      x = "Imputation model predictor",
      y = "Nonresponse indicator",
      fill = "Test p-value
      ",
      caption = ""
    ) +
    theme_minimice()

  # result <- matrix(
  #   nrow = length(vrb),
  #   ncol = length(vrb),
  #   dimnames = list(vrb, vrb)
  # )
  #
  #
  #
  # for (r in vrb) {
  #   for (c in vrb) {
  #
  #     if (r == c) result[r, c] = -1
  #     else {
  #       if (anyNA(data[,r])) {
  #         result[r, c] <- -5
  #         miss <- is.na(data[, r])
  #
  #         if (is.numeric(data[, c])) {
  #           test <- tryCatch({
  #             stats::t.test(data[miss, c], data[!miss, c])},
  #             error = function(e) {NULL}
  #           )
  #
  #           if (!is.null(test))        result[r, c] <- test$p.value
  #           else if (mean(miss) > 0.5) result[r, c] <- -3
  #           else                       result[r, c] <- -4
  #         }
  #
  #         if (is.factor(data[, c])) {
  #           test <- tryCatch({
  #             stats::fisher.test(table(data[, c], miss), simulate.p.value = TRUE)},
  #             error = function(e) {NULL}
  #           )
  #
  #           if (!is.null(test))        result[r, c] <- test$p.value
  #           else if (mean(miss) > 0.5) result[r, c] <- -3
  #           else                       result[r, c] <- -4
  #         }
  #
  #       } else {
  #         result[r, c] <- -2
  #       }
  #     }
  #   }
  # }
  #
  # labels <- 1:9
  # breaks <- c(-Inf, -5, -4, -3, -2, -1, 0.01, 0.05, 0.10, 1.00)
  # fills <- c(
  #   "#ffffffff", "#b61a511d", "#b61a51b3", "#006CC2B3", "#D3D3D3D3",
  #   "#b61a51b3", "#b61a5176", "#b61a513b", "#ffffffff")
  # colors <- c(
  #   "#BCBCBC", "#5b5657", "#472730", "#26374C", "#BCBCBC", "#472730",
  #   "#4C343B", "#514146", "#BCBCBC")
  #
  # long <- as.data.frame(result)
  # long$x <- vrb
  # long <- long %>%
  #   tidyr::gather(key = "y", value = "p", -"x") %>%
  #   dplyr::mutate(
  #     c = base::cut(.data$p, breaks = breaks, labels = labels),
  #     i = as.numeric(.data$c),
  #     p = base::sprintf("%.2f", .data$p),
  #     p = base::replace(.data$p, .data$p == "0.00", "<0.01"),
  #     p = base::replace(.data$p, .data$p == "-3.00", "(1)"),
  #     p = base::replace(.data$p, .data$p == "-2.00", "(4)"),
  #     p = base::replace(.data$p, .data$p == "-4.00", "(2)"),
  #     p = base::replace(.data$p, .data$p == "-5.00", "(3)"),
  #     p = base::replace(.data$p, .data$p == "-1.00" | .data$p == "-2.00", " ")
  #   )
  #
  # fills  <-  fills[base::sort(base::unique(long[,"i"]))]
  # colors <- colors[base::sort(base::unique(long[,"i"]))]
  #
  # gg <-
  #   ggplot2::ggplot(long, ggplot2::aes(x = .data$x, y = .data$y)) +
  #   ggplot2::geom_tile(ggplot2::aes(fill = .data$c), color = "#D3D3D3D3") +
  #   ggplot2::geom_text(ggplot2::aes(label = .data$p, color = .data$c), fontface = "bold", size = 2.5) +
  #   ggplot2::scale_x_discrete(limits = vrb, position = "top") +
  #   ggplot2::scale_y_discrete(limits = rev(vrb)) +
  #   ggplot2::theme_minimal() +
  #   ggplot2::theme(
  #     panel.grid.major = ggplot2::element_blank(),
  #     panel.grid.minor = ggplot2::element_blank(),
  #     panel.border     = ggplot2::element_blank(),
  #     plot.caption     = ggplot2::element_text(hjust = 0),
  #     legend.position  = "none"
  #   ) +
  #   ggplot2::labs(
  #     title = "Pairwise tests variable-missingness",
  #     x     = "Missingness indicator",
  #     y     = "Variable",
  #     caption = "(1) No Statistical test because extreme missingness \n(2) No Statistical test because sparse missingness\n(3) No statistical test for variable type\n(4) No missingness"
  #   ) +
  #   ggplot2::scale_fill_manual(values = fills) +
  #   ggplot2::scale_color_manual(values = colors)


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

