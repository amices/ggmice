#' Plot the scaled between imputation variance for every cell as a heatmap
#'
#' This function plots the cell-level between imputation variance. The function
#' scales the variances column-wise, without centering cf. `base::scale(center = FALSE)`
#' and plots the data image as a heatmap. Darker red cells indicate more variance,
#' lighter cells indicate less variance. White cells represent observed cells or unobserved cells with zero between
#' imputation variance.
#'
#' @param data A multiply imputed object of class [`mice::mids`] or [`mice::mira`].
#' @param grid Logical indicating whether grid lines should be displayed.
#'
#' @return An object of class `ggplot`.
#' @examples
#' imp <- mice::mice(mice::nhanes, printFlag = FALSE)
#' plot_variance(imp)
#' @export
plot_variance <- function(data, grid = TRUE) {
  if (!(mice::is.mids(data) | mice::is.mira(data))) {
    stop(
      "Input is not a Multiply Imputed Data Set of class `mids`/ `mira`. \n
         Perhaps function mice::as.mids() can be of use?"
    )
  }
  if (mice::is.mids(data)) {
      stopifnot("The between inmputation variance cannot be computed if there are fewer than two imputations (m < 2)." = data$m > 1)
  } else {
      stopifnot("The between inmputation variance cannot be computed if there are fewer than two imputations (m < 2)." = length(data$analyses) > 1)
    }

  if (grid) {
    gridcol <- "black"
  } else {
    gridcol <- NA
  }

  if (mice::is.mids(data)) {
    long <- mice::complete(data, "long")  %>%
      dplyr::mutate(dplyr::across(where(is.factor), as.numeric)) %>%
      dplyr::select(-.imp) %>%
      dplyr::group_by(.id) %>%
      dplyr::summarise(dplyr::across(dplyr::everything(),  stats::var)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(dplyr::across(.cols = -.id, ~ scale_above_zero(.))) %>%
      tidyr::pivot_longer(cols = -.id)

    legend <- "Imputation variability*\n "
    caption <- "*scaled cell-level between imputation variance"

    gg <-
      ggplot2::ggplot(long, ggplot2::aes(name, .id, fill = value)) +
      ggplot2::geom_tile(color = gridcol) +
      ggplot2::scale_fill_gradient(low = "white", high = mice::mdc(2)) +
      ggplot2::labs(
        x = "Column name",
        y = "Row number",
        fill = legend,
        caption = caption
      ) +
      ggplot2::scale_x_discrete(position = "top", expand = c(0, 0)) +
      ggplot2::scale_y_continuous(trans = "reverse", expand = c(0, 0)) +
      theme_minimice()
  }

  if (mice::is.mira(data)) {
    long <- purrr::map_dfr(1:length(data$analyses), ~ {
      pred = predict(data$analyses[[.x]])
      data.frame(
        .imp = .x,
        .id = names(pred),
        pred = pred,
        row.names = NULL
      )
    }) %>%
      dplyr::group_by(.id) %>%
      dplyr::summarise(avg = mean(pred), vrn = var(pred)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(.id = as.numeric(.id))

    legend <- "Imputation variability*\n "
    caption <-
      "*absolute prediction-level between imputation variance"

    gg <- ggplot2::ggplot(long, ggplot2::aes(avg, .id, fill = vrn, size = vrn)) +
      ggplot2::geom_point(color = gridcol, shape = 21) +
      ggplot2::scale_fill_gradient(low = "white", high = mice::mdc(2), guide = "legend") +
      ggplot2::labs(
        x = "Predicted value",
        y = "Row number",
        size = legend,
        fill = legend,
        caption = caption
      ) +
      # ggplot2::scale_x_discrete(position = "top", expand = c(0, 0)) +
      # ggplot2::scale_y_continuous(trans = "reverse", expand = c(0, 0)) +
      theme_minimice()

    # summ <- long %>%
    #   dplyr::group_by(.id) %>%
    #   dplyr::summarise(avg = mean(pred), vrn = var(pred)) %>%
    #   # dplyr::arrange(avg) %>%
    #   dplyr::mutate(.id = reorder(.id, -avg), .rws = as.numeric(row.names(.)))

    # long <- long %>%
    #   # dplyr::summarise(dplyr::across(dplyr::everything(),  stats::var)) %>%
    #   dplyr::ungroup() %>%
    #   dplyr::mutate(.id = as.numeric(.id),
    #                 name = "")

    # ggplot2::ggplot(long, ggplot2::aes(x = avg, y = .rws, color = vrn)) +
    #   ggplot2::geom_point() +
    # ggplot2::scale_y_continuous(trans = "reverse", expand = c(0, 0))


    # ggplot2::ggplot(long, ggplot2::aes(x = pred, y = .id, color = .imp)) +
    #   ggplot2::geom_point()

    # dplyr::bind_cols(., DV = dat[,dv]) %>%
    # dplyr::rowwise() %>%
    # dplyr::mutate(means = mean(dplyr::c_across(-DV)),
    #               vars = var(dplyr::c_across(-DV)))

  }




  if (!grid) {
    gg <-
      gg + ggplot2::theme(panel.border = ggplot2::element_rect(fill = NA))
  }

  # return the ggplot object
  return(gg)
}

# function to scale only non-zero values without centering
scale_above_zero <- function(x) {
  x <- as.matrix(x)
  x[x != 0] <- scale(x[x != 0], center = FALSE)
  return(x)
}
