#' Plot missingness in a dataset
#'
#' @param data An incomplete dataset of class `data.frame` or `matrix`.
#' @param vrb String, vector, or unquoted expression with variable name(s), default is "all".
#' @param border Logical indicating whether borders should be present between tiles.
#' @param ordered Logical indicating whether rows should be ordered according to their pattern.
#' @param square  Logical indicating whether the plot tiles should be squares, defaults to squares.
#'
#' @return An object of class [ggplot2::ggplot].
#'
#' @examples
#' plot_miss(mice::nhanes)
#' @export

plot_miss <-
  function(data,
           vrb = "all",
           border = FALSE,
           square = FALSE,
           ordered = FALSE) {
    # input processing
    if (is.matrix(data) && ncol(data) > 1) {
      data <- as.data.frame(data)
    }
    verify_data(data, df = TRUE)
    vrb <- substitute(vrb)
    if (vrb[1] == "all") {
      vrb <- names(data)
    } else {
      vrb <- names(dplyr::select(as.data.frame(data), {{vrb}}))
    }
    if (".x" %in% vrb || ".y" %in% vrb) {
      cli::cli_abort(
        c(
          "The variable names '.x' and '.y' are used internally to produce the missing data pattern plot.",
          "i" = "Please exclude or rename your variable(s)."
        )
      )
    }
    if (ordered) {
      # extract md.pattern matrix
      mdpat <- utils::head(mice::md.pattern(data, plot = FALSE), -1)
      # save frequency of patterns
      freq.pat <- rownames(mdpat) %>%
        as.numeric()

      na.mat <- mdpat %>%
        as.data.frame() %>%
        dplyr::select(-ncol(.data)) %>%
        dplyr::mutate(nmis = freq.pat) %>%
        tidyr::uncount(nmis)
    } else {
      # Create missingness indicator matrix
      na.mat <-
        purrr::map_df(data[, vrb], function(y)
          as.numeric(!is.na(y)))
    }
    # extract pattern info
    vrb <- colnames(na.mat)
    rws <- nrow(na.mat)
    cls <- ncol(na.mat)

    # transform to long format
    long <-
      as.data.frame(cbind(.y = 1:rws, na.mat)) %>%
      tidyr::pivot_longer(
        cols = tidyselect::all_of(vrb),
        names_to = "x",
        values_to = ".where"
      ) %>%
      dplyr::mutate(.x = as.numeric(factor(
        .data$x,
        levels = vrb, ordered = TRUE
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
      ggplot2::scale_x_continuous(breaks = 1:cls,
                                  labels = vrb) +
      ggplot2::scale_y_reverse() +
      ggplot2::labs(
        x = "Column name",
        y = "Row number",
        fill = "",
        alpha = ""
      ) +
      theme_minimice()
    # additional arguments
    if (border) {
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
    return(gg)
  }
