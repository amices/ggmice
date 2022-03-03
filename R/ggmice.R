#' Plot incomplete or imputed data
#'
#' @param data An incomplete dataset or an object of class `mids`.
#' @param mapping A list of aesthetic mappings created with `ggplot2::aes()`.
#'
#' @return A ggplot object of class `gg`.
#'
#' @examples
#' dat <- mice::nhanes
#' ggmice(dat, ggplot2::aes(x = age, y = bmi)) + ggplot2::geom_point()
#' @export
ggmice <- function(data = NULL, mapping = ggplot2::aes()) {
  # process inputs
  if (!(is.data.frame(data) | mice::is.mids(data))) {
    stop("Dataset (e.g., 'data.frame' or 'tibble') or 'mids' object (e.g. created with mice::mice()) is required.")
  }
  mapping_args <- names(mapping)
  if (!any(c("x", "y") %in% mapping_args)) {
    stop("At least one of the mapping arguments 'x' or 'y' are required. Supply variable name(s) with ggplot2::aes().")
  }
  if (is.character(mapping$x) | is.character(mapping$y)) {
    stop("The mapping argument requires variable name(s) of type 'quosure', typically created with ggplot2::aes(). To supply a string instead, try using ggplot2::aes_string()")
  }
  if (any(c("colour", "fill") %in% mapping_args)) {
    warning("The aes() arguments 'colour', 'fill' and 'group' have a special use in ggmmice() and will be overwritten. Try using 'shape' or 'linetype' for additional mapping, or use faceting.")
  }
  # extract variable names from mapping object
  if (mice::is.mids(data)) {
    vrbs <- names(data$data)
    vrbs_num <- vrbs[purrr::map_lgl(data$data, is.numeric)]
  } else {
    vrbs <- names(data)
    vrbs_num <- vrbs[purrr::map_lgl(data, is.numeric)]
  }
  vrb_x <- vrbs[stringr::str_detect(ggplot2::as_label(mapping$x), vrbs)]
  vrb_y <- vrbs[stringr::str_detect(ggplot2::as_label(mapping$y), vrbs)]
  if (identical(vrb_x, character(0)) & identical(vrb_y, character(0))) {
    stop("Mapping variable(s) not found in the data.")
  }

  # edit data and mapping objects
  if (mice::is.mids(data)) {
    where_xy <- rowSums(as.matrix(data$where[, c(vrb_x, vrb_y)])) > 0L
    mice_data <- dplyr::mutate(
      rbind(
        data.frame(.where = "observed", .imp = 0, .id = rownames(data$data), data$data)[!where_xy, ],
        data.frame(.where = "imputed", mice::complete(data, action = "long"))[where_xy, ]
      ),
      .imp = factor(.imp, ordered = TRUE)
    )
    mice_mapping <- utils::modifyList(mapping, ggplot2::aes(colour = .where, fill = .where))
    mice_colors <- c("observed" = "#006CC2B3", "imputed" = "#B61A51B3")
  } else {
    where_xy <- rowSums(is.na(as.matrix(data[, c(vrb_x, vrb_y)]))) > 0L
    mice_data <- dplyr::mutate(
      data,
      dplyr::across(vrbs_num, ~ tidyr::replace_na(as.numeric(.x), -Inf)),
      dplyr::across(vrbs[vrbs %nin% vrbs_num], ~ {
        as.factor(tidyr::replace_na(as.character(.x), " "))
      }),
      .where = factor(where_xy, levels = c(FALSE, TRUE), labels = c("observed", "missing"), ordered = TRUE)
    )
    mice_mapping <- utils::modifyList(mapping, ggplot2::aes(colour = .where, fill = .where))
    mice_colors <- c("observed" = "#006CC2B3", "missing" = "#B61A51B3")
  }
  # create plot
  gg <- ggplot2::ggplot(data = mice_data, mapping = mice_mapping) +
    ggplot2::scale_color_manual(values = mice_colors, drop = TRUE, name = "") +
    ggplot2::scale_fill_manual(values = mice_colors, drop = TRUE, name = "") +
    theme_mice()
  if (!mice::is.mids(data)) {
    gg <- gg +
      ggplot2::coord_cartesian(clip = "off")
    if ("x" %in% mapping_args) {
      if (vrb_x %nin% vrbs_num) {
        gg <- gg +
          ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = c(0, 0.6)))
      }
    }
    if ("y" %in% mapping_args) {
      if (vrb_y %nin% vrbs_num) {
        gg <- gg +
          ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = c(0, 0.6)))
      }
    }
  }
  # if(mice::is.mids(data)){
  #   gg <- gg +
  #     ggplot2::facet_wrap(~ .imp)
  # }
  # else {
  #   gg <- gg +
  #     annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = .2)
  # }
  # output
  return(gg)
}

# TODO: add jitter to categorical variables?
