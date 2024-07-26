#' Plot correlations between (incomplete) variables
#'
#' @param data A dataset of class `data.frame`, `tibble`, or `matrix`.
#' @param vrb String, vector, or unquoted expression with variable name(s), default is "all".
#' @param label Logical indicating whether correlation values should be displayed.
#' @param square Logical indicating whether the plot tiles should be squares.
#' @param diagonal Logical indicating whether the correlation of each variable with itself should be displayed.
#' @param rotate Logical indicating whether the variable name labels should be rotated 90 degrees.
#' @param caption Logical indicating whether the figure caption should be displayed.
#'
#' @return An object of class [ggplot2::ggplot].
#'
#' @examples
#' # plot correlations for all columns
#' plot_corr(mice::nhanes)
#'
#' # plot correlations for specific columns by supplying a character vector
#' plot_corr(mice::nhanes, c("chl", "hyp"))
#'
#' # plot correlations for specific columns by supplying unquoted variable names
#' plot_corr(mice::nhanes, c(chl, hyp))
#'
#' # plot correlations for specific columns by passing an object with variable names
#' # from the environment, unquoted with `!!`
#' my_variables <- c("chl", "hyp")
#' plot_corr(mice::nhanes, !!my_variables)
#' # object with variable names must be unquoted with `!!`
#' try(plot_corr(mice::nhanes, my_variables))
#'
#' @export
plot_corr <-
  function(data,
           vrb = "all",
           label = FALSE,
           square = TRUE,
           diagonal = FALSE,
           rotate = FALSE,
           caption = TRUE) {
    if (is.matrix(data) && ncol(data) > 1) {
      data <- as.data.frame(data)
    }
    verify_data(data = data, df = TRUE)
    vrb <- rlang::enexpr(vrb)
    vrb_matched <- match_vrb(vrb, names(data))
    if (length(vrb_matched) < 2) {
      cli::cli_abort("The number of variables should be two or more to compute correlations.")
    }
    # check if any column is constant
    constants <- apply(data[, vrb_matched], MARGIN = 2, function(x) {
      all(is.na(x)) || max(x, na.rm = TRUE) == min(x, na.rm = TRUE)
    })
    if (any(constants)) {
      vrb_matched <- vrb_matched[!constants]
      cli::cli_inform(
        c(
          "No correlations computed for variable(s):",
          " " = paste(names(constants[which(constants)]), collapse = ", "),
          "i" = "Correlations are undefined for constants."
        )
      )
    }
    # compute correlations
    p <- length(vrb_matched)
    corrs <- data.frame(
      vrb = rep(vrb_matched, each = p),
      prd = vrb_matched,
      corr = matrix(
        round(stats::cov2cor(
          stats::cov(data.matrix(data[, vrb_matched]), use = "pairwise.complete.obs")
        ), 2),
        nrow = p * p,
        byrow = TRUE
      )
    )
    if (!diagonal) {
      corrs[corrs$vrb == corrs$prd, "corr"] <- NA
    }
    # create plot
    gg <-
      ggplot2::ggplot(corrs,
                      ggplot2::aes(
                        x = .data$prd,
                        y = .data$vrb,
                        label = .data$corr,
                        fill = .data$corr
                      )) +
      ggplot2::geom_tile(color = "black", alpha = 0.6) +
      ggplot2::scale_x_discrete(limits = vrb_matched, position = "top") +
      ggplot2::scale_y_discrete(limits = rev(vrb_matched)) +
      ggplot2::scale_fill_gradient2(
        low = ggplot2::alpha("deepskyblue", 0.6),
        mid = "lightyellow",
        high = ggplot2::alpha("orangered", 0.6),
        na.value = "grey90",
        limits = c(-1, 1)
      )  +
      theme_minimice()
    if (caption) {
      gg <- gg +
        ggplot2::labs(
          x = "Imputation model predictor",
          y = "Variable to impute",
          fill = "Correlation*
      ",
      caption = "*pairwise complete observations"
        )
    } else {
      gg <- gg +
        ggplot2::labs(x = "Imputation model predictor",
                      y = "Variable to impute",
                      fill = "Correlation")
    }
    if (label) {
      gg <-
        gg + ggplot2::geom_text(color = "black",
                                show.legend = FALSE,
                                na.rm = TRUE)
    }
    if (square) {
      gg <- gg + ggplot2::coord_fixed(expand = FALSE)
    } else {
      gg <- gg + ggplot2::coord_cartesian(expand = FALSE)
    }
    if (rotate) {
      gg <-
        gg + ggplot2::theme(axis.text.x.top = ggplot2::element_text(angle = 90))
    }
    return(gg)
  }
