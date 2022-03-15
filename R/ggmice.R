#' Plot incomplete or imputed data
#'
#' @param data An incomplete dataset (of class `data.frame` or `matrix`), or an object of class [`mice::mids`].
#' @param mapping A list of aesthetic mappings created with [ggplot2::aes()].
#'
#' @return An object of class [`ggplot2::ggplot`].
#'
#' @examples
#' dat <- mice::nhanes
#' ggmice(dat, ggplot2::aes(x = age, y = bmi)) + ggplot2::geom_point()
#' @seealso See the `ggmice` vignette to use the `ggmice()` function on [incomplete data](https://amices.org/ggmice/articles/ggmice.html#the-ggmice-function) or [imputed data](https://amices.org/ggmice/articles/ggmice.html#the-ggmice-function-1).
#' @export
ggmice <- function(data = NULL, mapping = ggplot2::aes()) {
  # process inputs
  if (!(is.data.frame(data) | mice::is.mids(data))) {
    stop("Dataset (e.g., 'data.frame' or 'tibble') or 'mids' object (e.g. created with mice::mice()) is required.")
  }
  if (is.null(mapping$x) & is.null(mapping$y)) {
    stop("At least one of the mapping arguments 'x' or 'y' is required. Supply variable name(s) with ggplot2::aes().")
  }
  if (is.character(mapping$x) | is.character(mapping$y)) {
    stop("The mapping argument requires variable name(s) of type 'quosure', typically created with ggplot2::aes(). To supply a string instead, try using ggplot2::aes_string()")
  }
  if (!is.null(mapping$colour)) {
    warning("The aes() argument 'colour' has a special use in ggmmice() and will be overwritten. Try using 'shape' or 'linetype' for additional mapping, or use faceting.")
  }

  # extract variable names from mapping object
  if (is.data.frame(data)) {
    vrbs <- names(data)
    vrbs_num <- vrbs[purrr::map_lgl(data, is.numeric)]
  }
  if (mice::is.mids(data)) {
    vrbs <- names(data$data)
    vrbs_num <- vrbs[purrr::map_lgl(data$data, is.numeric)]
  }
  mapping_x <- ggplot2::as_label(mapping$x)
  mapping_y <- ggplot2::as_label(mapping$y)
  vrb_x <- vrbs[stringr::str_detect(mapping_x, vrbs)]
  vrb_y <- vrbs[stringr::str_detect(mapping_y, vrbs)]
  if (!is.null(mapping$x) & mapping_x %nin% c(".id", ".imp", ".where") & identical(vrb_x, character(0))) {
    stop(paste0("Mapping variable '", mapping_x, "' not found in the data or imputations."))
  }
  if (!is.null(mapping$y) & mapping_y %nin% c(".id", ".imp", ".where") & identical(vrb_y, character(0))) {
    stop(paste0("Mapping variable '", mapping_y, "' not found in the data or imputations."))
  }

  # edit data and mapping objects
  if (is.data.frame(data)) {
    where_xy <- rowSums(is.na(as.matrix(data[, c(vrb_x, vrb_y)]))) > 0L
    mice_data <- cbind(
      .where = factor(where_xy == 1, levels = c(FALSE, TRUE), labels = c("observed", "missing"), ordered = TRUE),
      data
    )
    if (!is.null(mapping$x) & !is.null(mapping$y)) {
      mice_data <- dplyr::mutate(
        mice_data,
        dplyr::across(vrbs_num, ~ tidyr::replace_na(as.numeric(.x), -Inf)),
        dplyr::across(vrbs[vrbs %nin% vrbs_num], ~ {
          as.factor(tidyr::replace_na(as.character(.x), " "))
        })
      )
    }
    mice_mapping <- utils::modifyList(mapping, ggplot2::aes(colour = .where))
    mice_colors <- c("observed" = "#006CC2B3", "missing" = "#B61A51B3")
  }
  if (mice::is.mids(data)) {
    where_xy <- rowSums(as.matrix(data$where[, c(vrb_x, vrb_y)])) > 0L
    mice_data <- dplyr::mutate(
      rbind(
        data.frame(.where = "observed", .imp = 0, .id = rownames(data$data), data$data)[!where_xy, ],
        data.frame(.where = "imputed", mice::complete(data, action = "long"))[where_xy, ]
      ),
      .imp = factor(.imp, ordered = TRUE)
    )
    mice_mapping <- utils::modifyList(mapping, ggplot2::aes(colour = .where)) # , fill = .where
    mice_colors <- c("observed" = "#006CC2B3", "imputed" = "#B61A51B3")
  }

  # create plot
  gg <- ggplot2::ggplot(data = mice_data, mapping = mice_mapping) +
    ggplot2::scale_color_manual(values = mice_colors, name = "") +
    theme_mice()

  # edit plot to display missing values on the axes
  if (is.data.frame(data) & !is.null(mapping$x) & !is.null(mapping$y)) {
    gg <- gg +
      ggplot2::coord_cartesian(clip = "off")
    if (!is.null(mapping$x)) {
      if (vrb_x %nin% vrbs_num) {
        gg <- gg +
          ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = c(0, 0.6)))
      }
    }
    if (!is.null(mapping$y)) {
      if (vrb_y %nin% vrbs_num) {
        gg <- gg +
          ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = c(0, 0.6)))
      }
    }
  }

  # output
  return(gg)
}
