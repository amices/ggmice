#' Plot the missing data pattern of an incomplete dataset
#'
#' @param data An incomplete dataset of class `data.frame` or `matrix`.
#' @param vrb String, vector, or unquoted expression with variable name(s), default is "all".
#' @param square Logical indicating whether the plot tiles should be squares, defaults to squares to mimick `mice::md.pattern()`.
#' @param rotate Logical indicating whether the variable name labels should be rotated 90 degrees.
#' @param cluster Optional character string specifying which variable should be used for clustering (e.g., for multilevel data).
#' @param npat Optional numeric input specifying the number of missing data patterns to be visualized, defaults to all patterns.
#' @param caption Logical indicating whether the figure caption should be displayed.
#'
#' @return An object of class [ggplot2::ggplot].
#'
#' @examples
#' plot_pattern(mice::nhanes)
#' @export
plot_pattern <-
  function(data,
           vrb = "all",
           square = TRUE,
           rotate = FALSE,
           cluster = NULL,
           npat = NULL,
           caption = T) {
    if (is.matrix(data) && ncol(data) > 1) {
      data <- as.data.frame(data)
    }
    verify_data(data, df = TRUE)
    vrb <- substitute(vrb)
    if (vrb != "all" && length(vrb) < 2) {
      cli::cli_abort("The number of variables should be two or more to compute missing data patterns.")
    }
    if (vrb[1] == "all") {
      vrb <- names(data)
    } else {
      vrb <- names(dplyr::select(data, {
        {
          vrb
        }
      }))
    }
    if (".x" %in% vrb || ".y" %in% vrb) {
      cli::cli_abort(
        c(
          "The variable names '.x' and '.y' are used internally to produce the missing data pattern plot.",
          "i" = "Please exclude or rename your variable(s)."
        )
      )
    }
    if (!is.null(cluster)) {
      if (cluster %nin% names(data[, vrb])) {
        cli::cli_abort(
          c("Cluster variable not recognized.",
            "i" = "Please provide the variable name as a character string.")
        )
      }
    }
    if (!is.null(npat)) {
      if (!is.numeric(npat) || npat < 1) {
        cli::cli_abort(
          c("The minimum number of patterns to display is one.",
            "i" = "Please provide a positive integer.")
        )
      }
    }

    # get missing data pattern
    pat <- mice::md.pattern(data[, vrb], plot = FALSE)
    rows_pat_full <-
      (nrow(pat) - 1) # full number of missing data patterns

    # filter npat most frequent patterns
    if (!is.null(npat)) {
      if (npat < rows_pat_full) {
        top_n_pat <-
          sort(as.numeric(row.names(pat)), decreasing = TRUE)[1:npat]
        pat <-
          pat[rownames(pat) %in% c(top_n_pat, ""), , drop = FALSE]

        if (npat != (nrow(pat) - 1)) {
          # if npat != number of missing patterns
          # show number of requested, shown, and hidden missing data patterns
          cli::cli_inform(
            c(
              "i" = "{npat} missing data patterns were requested.",
              "i" = "{nrow(pat) - 1} missing data patterns are shown.",
              "i" = "{rows_pat_full - (nrow(pat)-1)} missing data patterns are hidden."
            )
          )
        }
      } else {
        cli::cli_warn(
          c(
            "Number of patterns specified is equal to or greater than the total number of patterns.",
            "i" = "All missing data patterns are shown."
          )
        )
      }
    }

    # extract pattern info
    rws <- nrow(pat)
    cls <- ncol(pat)
    vrb <- colnames(pat)[-cls]
    frq <- row.names(pat)[-rws]
    na_row <- pat[-rws, cls]
    na_col <- pat[rws, -cls]

    # add opacity for clustering
    if (is.null(cluster)) {
      pat_clean <- cbind(.opacity = 1, pat[-rws, vrb, drop = FALSE])
    } else {
      pats <- purrr::map(split(data[, vrb], ~ get(cluster)), ~ {
        mice::md.pattern(., plot = FALSE) %>%
          pat_to_chr(., ord = vrb)
      })
      pat_used <- purrr::map_dfr(pats, ~ {
        pat_to_chr(pat) %in% .x
      }) %>%
        rowMeans()
      pat_clean <- data.frame(.opacity = pat_used, pat[-rws, vrb])
    }

    # tidy the pattern
    long <-
      data.frame(.y = 1:(rws - 1), pat_clean, row.names = NULL) %>%
      tidyr::pivot_longer(
        cols = tidyselect::all_of(vrb),
        names_to = "x",
        values_to = ".where"
      ) %>%
      dplyr::mutate(
        .x = as.numeric(factor(
          .data$x,
          levels = vrb, ordered = TRUE
        )),
        .where = factor(
          .data$.where,
          levels = c(0, 1),
          labels = c("missing", "observed")
        ),
        .opacity = as.numeric(.data$.opacity)
      )

    # create the plot
    gg <-
      ggplot2::ggplot(
        long,
        ggplot2::aes(
          .data$.x,
          .data$.y,
          fill = .data$.where,
          alpha = 0.1 + .data$.opacity / 2
        )
      ) +
      ggplot2::geom_tile(color = "black") +
      ggplot2::scale_fill_manual(values = c(
        "observed" = "#006CC2B3",
        "missing" = "#B61A51B3"
      )) +
      ggplot2::scale_alpha_continuous(limits = c(0, 1), guide = "none") +
      ggplot2::scale_x_continuous(
        breaks = 1:(cls - 1),
        labels = na_col,
        sec.axis = ggplot2::dup_axis(labels = vrb,
                                     name = "Column name")
      ) +
      ggplot2::scale_y_reverse(
        breaks = 1:(rws - 1),
        labels = frq,
        sec.axis = ggplot2::dup_axis(labels = na_row,
                                     name = "Number of missing entries\nper pattern")
      ) +
      ggplot2::labs(
        x = "Number of missing entries\nper column",
        y = "Pattern frequency",
        fill = "",
        alpha = ""
      ) +
      theme_minimice()
    if (square) {
      gg <- gg + ggplot2::coord_fixed(expand = FALSE)
    } else {
      gg <- gg + ggplot2::coord_cartesian(expand = FALSE)
    }
    if (rotate) {
      gg <-
        gg + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
    }
    if (caption) {
      if (!is.null(npat) & npat < rows_pat_full) {
        gg <- gg +
          ggplot2::labs(
            caption = paste0(
              "There are a total of ",
              sum(is.na(data[, vrb])),
              " empty cells. \n ",
              rows_pat_full - (nrow(pat) - 1),
              " missing data patterns are hidden."
            )
          )
      } else{
        gg <- gg +
          ggplot2::labs(caption = paste0("There are a total of ",
                                         sum(is.na(data[, vrb])),
                                         " empty cells."))
      }
    }

    return(gg)
  }

#' Utils function to process missing data pattern
#'
#' @param pat Numeric matrix with a missing data pattern.
#' @param ord Vector with variable names.
#' @keywords internal
pat_to_chr <- function(pat, ord = NULL) {
  if (is.null(ord)) {
    ord <- colnames(pat)[-ncol(pat)]
  }
  apply(pat[-nrow(pat), ord], 1, function(x) {
    paste(as.numeric(x), collapse = "")
  })
}
