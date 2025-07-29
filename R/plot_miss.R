#' Plot missingness in a dataset
#'
#' @param data An incomplete dataset of class `data.frame` or `matrix`.
#' @param vrb String, vector, or unquoted expression with variable name(s), default is "all".
#' @param ordered Logical indicating whether rows should be ordered according to their pattern.
#' @param grid Logical indicating whether borders should be present between tiles.
#' @param rotate Logical indicating whether the variable name labels should be rotated 90 degrees.
#' @param square  Logical indicating whether the plot tiles should be squares, defaults to squares.
#'
#' @return An object of class [ggplot2::ggplot].
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @examples
#' # plot correlations for all columns
#' plot_miss(mice::nhanes)
#'
#' # plot correlations for specific columns by supplying a character vector
#' plot_miss(mice::nhanes, c("chl", "hyp"))
#'
#' # plot correlations for specific columns by supplying unquoted variable names
#' plot_miss(mice::nhanes, c(chl, hyp))
#'
#' # plot correlations for specific columns by passing an object with variable names
#' # from the environment, unquoted with `!!`
#' my_variables <- c("chl", "hyp")
#' plot_miss(mice::nhanes, !!my_variables)
#' # object with variable names must be unquoted with `!!`
#' try(plot_miss(mice::nhanes, my_variables))
#'
#' # plot larger dataset
#' plot_miss(mice::boys)
#' plot_miss(mice::boys, ordered = TRUE)
#'
#' @export
plot_miss <-
  function(data,
           vrb = "all",
           ordered = FALSE,
           rotate = FALSE,
           grid = FALSE,
           square = FALSE) {
    # flag lifecycle
    lifecycle::signal_stage("experimental", "plot_miss()")
    # input processing
    if (is.matrix(data) && ncol(data) > 1) {
      data <- as.data.frame(data)
    }
    verify_data(data = data, df = TRUE)
    vrb <- rlang::enexpr(vrb)
    vrb_matched <- match_vrb(vrb, names(data))
    if (".y" %in% vrb_matched) {
      cli::cli_abort(
        c(
          "The variable name '.y' is used internally in this function.",
          "i" = "Please exclude or rename your variable(s)."
        )
      )
    }
    # extract response indicator
    if (!ordered) {
      R <- !is.na(data[, vrb_matched])
    }
    if (ordered) {
      if (length(vrb_matched) < 2) {
        cli::cli_abort("The number of variables should be two or more to compute missing data patterns.")
      }
      md_pat <- mice::md.pattern(
        data[, vrb_matched],
        plot = FALSE)[, -(length(vrb_matched) + 1)]
      n_pat <- nrow(md_pat) - 1
      md_pat <- md_pat[-(n_pat + 1), ]
      pat_frq <- as.numeric(rownames(md_pat))
      row.names(md_pat) <- 1:n_pat
      R <- md_pat[rep(row.names(md_pat), times = pat_frq), ] == 1
    }

    # transform to long format
    .vrb <- colnames(R)
    .rws <- nrow(R)
    .cls <- ncol(R)
    long <-
      as.data.frame(cbind(.y = 1:.rws, R)) %>%
      tidyr::pivot_longer(
        cols = tidyselect::all_of(.vrb),
        names_to = "x",
        values_to = ".where"
      ) %>%
      dplyr::mutate(.x = as.numeric(factor(
        .data$x,
        levels = .vrb, ordered = TRUE
      )),
      .where = factor(
        .data$.where,
        levels = c(0, 1),
        labels = c("missing", "observed")
      ))
    gg <-
      ggplot2::ggplot(long,
                      ggplot2::aes(.data$.x,
                                   as.numeric(.data$.y),
                                   fill = .data$.where)) +
      ggplot2::scale_fill_manual(values = c(
        "observed" = "#006CC2B3",
        "missing" = "#B61A51B3"
      )) +
      ggplot2::scale_alpha_continuous(limits = c(0, 1), guide = "none") +
      ggplot2::scale_x_continuous(breaks = 1:.cls,
                                  labels = .vrb,
                                  position = "top") +
      ggplot2::scale_y_reverse(breaks = \(y) {
        eb = scales::extended_breaks()(y)
        eb[1] = min(long$.y)
        eb[length(eb)] = max(long$.y)
        eb
      }) +
      ggplot2::labs(
        x = "Column name",
        y = "Row number",
        fill = "",
        alpha = ""
      ) +
      theme_minimice()
    # additional arguments
    if (grid) {
      gg <- gg + ggplot2::geom_tile(color = "black")
    } else{
      gg <- gg + ggplot2::geom_tile()
    }
    if (square) {
      gg <- gg + ggplot2::coord_fixed(expand = FALSE)
    } else {
      gg <- gg + ggplot2::coord_cartesian(expand = FALSE)
    }
    if (ordered) {
      gg <- gg +
        ggplot2::theme(
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank()
        )
    }
    if (rotate) {
      gg <-
        gg + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
    }
    return(gg)
  }
