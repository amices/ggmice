#' Plot the predictor matrix of an imputation model
#'
#' @param data A predictor matrix for `mice`, typically generated with [mice::make.predictorMatrix] or [mice::quickpred], or an object of class [`mice::mids`].
#' @param vrb String, vector, or unquoted expression with variable name(s), default is "all".
#' @param method Character string or vector with imputation methods.
#' @param label Logical indicating whether predictor matrix values should be displayed.
#' @param square Logical indicating whether the plot tiles should be squares.
#' @param rotate Logical indicating whether the variable name labels should be rotated 90 degrees.
#'
#' @return An object of class `ggplot2::ggplot`.
#'
#' @examples
#' # generate a predictor matrix
#' pred <- mice::quickpred(mice::nhanes)
#'
#' # plot predictor matrix for all columns
#' plot_pred(pred)
#'
#' # plot predictor matrix for specific columns by supplying a character vector
#' plot_pred(pred, c("chl", "hyp"))
#'
#' # plot predictor matrix for specific columns by supplying unquoted variable names
#' plot_pred(pred, c(chl, hyp))
#'
#' # plot predictor matrix for specific columns by passing an object with variable names
#' # from the environment, unquoted with `!!`
#' my_variables <- c("chl", "hyp")
#' plot_pred(pred, !!my_variables)
#' # object with variable names must be unquoted with `!!`
#' try(plot_pred(pred, my_variables))
#'
#' # plot predictor matrix of mids object
#' imp <- mice::mice(mice::nhanes, print = FALSE)
#' plot_pred(imp)
#'
#' @export
plot_pred <-
  function(data,
           vrb = "all",
           method = NULL,
           label = TRUE,
           square = TRUE,
           rotate = FALSE) {
    verify_data(data, pred = TRUE, imp = TRUE)
    vrb <- rlang::enexpr(vrb)
    if (mice::is.mids(data)) {
      if (!is.null(method)) {
        cli::cli_warn(
          c("!" = "Input `method` is ignored when `data` is of class `mids`.",
            "i" = "The `method` vector from the `mids` object will be used.")
        )
      }
      method <- data$method
      data <- data$predictorMatrix
    }
    vrb_matched <- match_vrb(vrb, row.names(data))
    p <- length(vrb_matched)
    if (!is.null(method) && is.character(method)) {
      if (length(method) == 1) {
        method <- rep(method, p)
      }
      if (length(method > p)) {
        method <- method[row.names(data) %in% vrb_matched]
      }
      if (length(method) == p) {
        ylabel <- "Imputation method"
      }
    }
    if (is.null(method)) {
      method <- rep("", p)
      ylabel <- ""
    }
    if (!is.character(method) || length(method) != p) {
      cli::cli_abort("Method should be `NULL` or a character string or vector
                     (of length 1 or `ncol(data)`).")
    }
    long <- data.frame(
      vrb = 1:p,
      prd = rep(vrb_matched, each = p),
      ind = matrix(data[vrb_matched, vrb_matched], nrow = p * p, byrow = TRUE)
    ) %>% dplyr::mutate(clr = factor(
      .data$ind,
      levels = c(-3, -2, 0, 1, 2),
      labels = c(
        "inclusion-restriction variable",
        "cluster variable",
        "not used",
        "predictor",
        "random effect"
      ),
      ordered = TRUE
    ))

    gg <-
      ggplot2::ggplot(long,
                      ggplot2::aes(
                        x = .data$prd,
                        y = .data$vrb,
                        label = .data$ind,
                        fill = .data$clr
                      )) +
      ggplot2::geom_tile(color = "black", alpha = 0.6) +
      ggplot2::scale_x_discrete(limits = vrb_matched, position = "top") +
      ggplot2::scale_y_reverse(
        breaks = 1:p,
        labels = vrb_matched,
        sec.axis = ggplot2::dup_axis(labels = method, name = ylabel)
      ) +
      ggplot2::scale_fill_manual(
        values = c(
          "inclusion-restriction variable" = "orangered",
          "cluster variable" = "lightyellow",
          "not used" = "grey90",
          "predictor" = "palegreen3",
          "random effect" = "deepskyblue"
        )
      ) +
      ggplot2::labs(
        x = "Imputation model predictor",
        y = "Variable to impute",
        fill = "",
        color = ""
      ) +
      theme_minimice()

    if (all(method == "")) {
      gg <-
        gg + ggplot2::theme(axis.ticks.y.right = ggplot2::element_blank())
    }
    if (label) {
      gg <- gg + ggplot2::geom_text(color = "black", show.legend = FALSE)
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
