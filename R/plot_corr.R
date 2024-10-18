#' Plot correlations between (incomplete) variables
#'
#' @param data A dataset of class `data.frame`, `tibble`, or `matrix`.
#' @param vrb String, vector, or unquoted expression with variable name(s), default is "all".
#' @param diff Logical indicating whether the difference between the observed and imputed data is plotted.
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
           diff = FALSE,
           label = FALSE,
           square = TRUE,
           diagonal = FALSE,
           rotate = FALSE,
           caption = TRUE) {
    # process inputs
    if (is.matrix(data) && ncol(data) > 1) {
      data <- as.data.frame(data)
    }
    verify_data(data = data, df = TRUE, imp = TRUE)
    if (diff && !mice::is.mids(data)) {
      cli::cli_abort("Difference in correlations can only be computed with imputed data.")
    }
    vrb <- rlang::enexpr(vrb)
    if (mice::is.mids(data)) {
      imp <- TRUE
      mids <- data
      data <- data$data
    } else {
      imp <- FALSE
    }
    vrbs_in_data <- names(data)
    vrb_matched <- match_vrb(vrb, vrbs_in_data)
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
    if (length(vrb_matched) < 2) {
      cli::cli_abort("The number of variables should be two or more to compute correlations.")
    }
    # create plot labels
    lab_x <- "Imputation model predictor"
    lab_y <- "Imputation target"
    lab_fill <- "Correlation*
    "
    if (!imp) {
      lab_note <- "*pairwise complete observations"
    }
    if (imp) {
      lab_note <- "*pooled across imputations"
    }
    if (diff) {
      lab_fill <- "Difference in correlations*
"
      lab_note <- "*pooled imputed minus observed"
    }
    # compute correlations
    if (!imp | diff) {
      corr <- stats::cov2cor(stats::cov(data.matrix(data[, vrb_matched]), use = "pairwise.complete.obs"))
    }
    if (imp) {
      imps <- mice::complete(mids, "all")
      corrs <- purrr::map(imps, ~ {
        stats::cov2cor(stats::cov(data.matrix(.x[, vrb_matched]), use = "pairwise.complete.obs"))
      })
      if (diff) {
        corr <- (Reduce("+", corrs) / length(corrs)) - corr
      } else {
        corr <- Reduce("+", corrs) / length(corrs)
      }
    }
    # convert correlations into plotting object
    p <- length(vrb_matched)
    long <- data.frame(
      vrb = rep(vrb_matched, each = p),
      prd = vrb_matched,
      corr = matrix(round(corr, 2), nrow = p * p, byrow = TRUE)
    )
    if (!diff) {
      long$text <- long$corr
    }
    if (diff) {
      long$text <- sprintf("%+.2f", long$corr)
      long$text[long$corr < 0] <- long$corr[long$corr < 0]
    }
    if (!diagonal) {
      long[long$vrb == long$prd, "corr"] <- NA
      long[long$vrb == long$prd, "text"] <- ""
    }
    # create plot
    gg <-
      ggplot2::ggplot(long,
                      ggplot2::aes(
                        x = .data$prd,
                        y = .data$vrb,
                        label = .data$text,
                        fill = .data$corr
                      )) +
      ggplot2::geom_tile(color = "black", alpha = 0.6) +
      ggplot2::scale_x_discrete(limits = vrb_matched, position = "top") +
      ggplot2::scale_y_discrete(limits = rev(vrb_matched)) +
      ggplot2::labs(
        x = lab_x,
        y = lab_y,
        fill = lab_fill,
        caption = lab_note
      ) +
      theme_minimice()
    # edit plot to match function arguments
    if (!diff) {
      gg <- gg +
        ggplot2::scale_fill_gradient2(
          low = ggplot2::alpha("deepskyblue", 0.6),
          mid = "lightyellow",
          high = ggplot2::alpha("orangered", 0.6),
          na.value = "grey90",
          limits = c(-1, 1)
        )
    }
    if (diff) {
      gg <- gg + ggplot2::scale_fill_gradient2(
        low = ggplot2::alpha("deepskyblue", 1),
        mid = "lightyellow",
        high = ggplot2::alpha("orangered", 1),
        na.value = "grey90",
        limits = c(-2, 2)
      )
    }
    if (!caption) {
      lab_fill <- substring(lab_fill, 1, nchar(lab_fill) - 2)
      gg <- gg +
        ggplot2::labs(fill = lab_fill, caption = NULL)
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
