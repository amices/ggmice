#' Plot the missing data pattern of an incomplete dataset
#'
#' @param data An incomplete dataset of class `data.frame` or `matrix`.
#' @param vrb String or vector with variable name(s), default is "all".
#' @param square Logical indicating whether the plot tiles should be squares.
#' @param rotate Logical indicating whether the variable name labels should be rotated 90 degrees.
#' @param cluster Optional character string specifying which variable should be used for clustering (e.g., for multilevel data).
#'
#' @return An object of class `ggplot2::ggplot`.
#'
#' @examples
#' plot_pattern(mice::nhanes)
#' @export
plot_pattern <- function(data, vrb = "all", square = FALSE, rotate = FALSE, cluster = NULL) {
  if (!is.data.frame(data) & !is.matrix(data)) {
    stop("Dataset should be a 'data.frame' or 'matrix'.")
  }
  if (vrb == "all") {
    vrb <- names(data)
  }
  if (!is.null(cluster)) {
    if (cluster %nin% names(data[, vrb])) {
      stop("Cluster variable not recognized, please provide the variable name as a character string.")
    }
  }
  # get missing data pattern and extract info
  pat <- mice::md.pattern(data[, vrb], plot = FALSE)
  rws <- nrow(pat)
  cls <- ncol(pat)
  vrb <- colnames(pat)[-cls]
  frq <- row.names(pat)[-rws]
  na_row <- pat[-rws, cls]
  na_col <- pat[rws, -cls]
  # na_tot <- pat[rws, cls]

  if (is.null(cluster)) {
    pat_clean <- cbind(.opacity = 1, pat[-rws, vrb])
  } else {
    pats <- purrr::map(split(data[, vrb], ~ get(cluster)), ~ {
      mice::md.pattern(., plot = FALSE) %>%
        pat_to_chr(., ord = vrb)
    })
    pat_used <- purrr::map_dfr(pats, ~ {
      pat_to_chr(pat) %in% .x
    }) %>%
      rowMeans()
    pat_clean <- data.frame(.opacity = pat_used, pat[-rws, vrb]) # .5 + ((pat_used - 0.5)/2)
  }

  # tidy the pattern
  long <- data.frame(y = 1:(rws - 1), pat_clean, row.names = NULL) %>%
    tidyr::pivot_longer(cols = vrb, names_to = "x", values_to = ".where") %>%
    dplyr::mutate(
      x = as.numeric(factor(.data$x, levels = vrb, ordered = TRUE)),
      .where = factor(.data$.where, levels = c(0, 1), labels = c("missing", "observed")),
      # TODO: always obs/always missing, add title, maybe make y axis prop to freq, add asterisk to clust var with caption that can tell that there is missingness in it
      .opacity = as.numeric(.data$.opacity)
    )

  # create the plot
  gg <- ggplot2::ggplot(long, ggplot2::aes(.data$x, .data$y, fill = .data$.where, alpha = 0.1 + .data$.opacity / 2)) +
    ggplot2::geom_tile(color = "black") +
    ggplot2::scale_fill_manual(values = c("observed" = "#006CC2B3", "missing" = "#B61A51B3")) +
    ggplot2::scale_alpha_continuous(limits = c(0, 1), guide = "none") +
    ggplot2::scale_x_continuous(
      breaks = 1:(cls - 1),
      labels = na_col,
      sec.axis = ggplot2::dup_axis(
        labels = vrb,
        name = "Variable\n(name)"
      )
    ) +
    ggplot2::scale_y_reverse(
      breaks = 1:(rws - 1),
      labels = frq,
      sec.axis = ggplot2::dup_axis(
        labels = na_row,
        name = "Pattern\n(number of missing entries)"
      )
    ) +
    ggplot2::labs(
      x = "Variable\n(number of missing entries)",
      y = "Pattern\n(frequency)",
      fill = "",
      alpha = "" # ,
      # caption = paste("*total number of missing entries =", na_tot)
    ) +
    theme_minimice()
  if (square) {
    gg <- gg + ggplot2::coord_fixed()
  }
  if (rotate) {
    gg <- gg + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
  }

  return(gg)
}

#' Utils function to process missing data pattern
#'
#' @param pat Numeric matrix with a missing data pattern.
#' @param ord Vector with variable names.
pat_to_chr <- function(pat, ord = NULL) {
  if (is.null(ord)) {
    ord <- colnames(pat)[-ncol(pat)]
  }
  apply(pat[-nrow(pat), ord], 1, function(x) paste(as.numeric(x), collapse = ""))
}
