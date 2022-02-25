#' Title
#'
#' @param data An incomplete dataset or an object of class `mids`
#' @param mapping A list of aesthetic mappings created with `ggplot2::aes()`
#'
#' @return A ggplot object of class `gg`
#'
#' @examples
#' ggmice(mice::nhanes, ggplot2::aes(x = age, y = bmi))
#' @export
ggmice <- function(data = NULL, mapping = ggplot2::aes()) {
  # process inputs
  mapping_args <- names(mapping)
  try(if(!any(c("x", "y") %in% mapping_args)){
    stop("At least one of the aes() arguments 'x' or 'y' is required. Cannot create ggmice object without mapping.")
  })
  if(any(c("colour", "fill", "group") %in% mapping_args)){
    warning("The aes() arguments 'colour', 'fill' and 'group' have a special use in ggmmice() and will be overwritten. Try using 'shape' or 'linetype' for additional mapping.")
  }
  # extract variable names from mapping object
  if(mice::is.mids(data)){
    vrbs <- names(data$data)
    vrbs_num <- vrbs[purrr::map_lgl(data$data, is.numeric)]
  } else {
    vrbs <- names(data)
    vrbs_num <- vrbs[purrr::map_lgl(data, is.numeric)]
  }
  vrbs_xy <- vrbs[stringr::str_detect(ggplot2::as_label(mapping$x), vrbs) | stringr::str_detect(ggplot2::as_label(mapping$y), vrbs)]

  # edit data and mapping objects
  if(mice::is.mids(data)){
    where_xy <- rowSums(as.matrix(data$where[, vrbs_xy])) > 0L
    mice_data <- dplyr::mutate(
      rbind(
        data.frame(.where = "observed", .imp = 0, .id = rownames(data$data), data$data)[!where_xy, ],
        data.frame(.where = "imputed", mice::complete(data, action = "long"))[where_xy, ]),
      .imp = factor(.imp, ordered = TRUE))
    mice_mapping <- utils::modifyList(mapping, ggplot2::aes(colour = .where, fill = .where))
    if("x" %nin% mapping_args){
      mice_mapping <- utils::modifyList(mice_mapping, ggplot2::aes(x = .imp))
    }
    if("y" %nin% mapping_args){
      mice_mapping <- utils::modifyList(mice_mapping, ggplot2::aes(y = .imp))
      mice_data$.imp <- ordered(mice_data$.imp, levels = rev(levels(mice_data$.imp)))
    }
  } else {
    where_xy <- rowSums(is.na(as.matrix(data[, vrbs_xy]))) > 0L
    mice_data <- dplyr::mutate(
      data,
      dplyr::across(vrbs_num, ~tidyr::replace_na(.x, -Inf)),
      dplyr::across(vrbs[vrbs %nin% vrbs_num], ~{as.factor(tidyr::replace_na(as.character(.x), "-Inf"))}),
      .where = factor(where_xy, levels = c(FALSE, TRUE), labels = c("observed", "missing"), ordered = TRUE))
    mice_mapping <- utils::modifyList(mapping, ggplot2::aes(colour = .where, fill = .where))
  }
  # create plot
  mice_colors <- c("observed" = "#006CC2B3", "missing" = "#B61A51B3", "imputed" = "#B61A51B3")
  gg <- ggplot2::ggplot(data = mice_data, mapping = mice_mapping) +
    ggplot2::scale_color_manual(values = mice_colors, drop = TRUE, name = "") +
    ggplot2::scale_fill_manual(values = mice_colors, drop = TRUE, name = "") +
    theme_mice()
  # if(mice::is.mids(data)){
  #   gg <- gg +
  #     ggplot2::facet_wrap(~ .imp)
  # }
  # else {
  #   gg <- gg +
  #     ggplot2::coord_cartesian(clip = "off")
  #   #   annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = .2)
  # }
  # output
  return(gg)
}

#TODO resolve notes about global variables/functions by adding .data, see https://dplyr.tidyverse.org/articles/programming.html

# ggmice(imp, ggplot2::aes(bmi)) + ggplot2::geom_boxplot()
# x_mis <- if("x" %in% mapping_args){data$where[, rlang::as_name(mapping$x)]} else FALSE
# y_mis <- if("y" %in% mapping_args){data$where[, rlang::as_name(mapping$y)]} else FALSE
# xy_mis <- x_mis | y_mis
# mapping_vrb <- c(if("x" %in% mapping_args){rlang::as_name(mapping$x)}, if("y" %in% mapping_args){rlang::as_name(mapping$y)})
# mice_data <- cbind(mice::complete(data, action = "long", include = TRUE),
#                    .mis = factor(rowSums(data$where[, mapping_vrb] > 0), levels = c(0, 1), labels = c("observed", "imputed"), ordered = TRUE))
# yvar <- if("y" %in% mapping_args) {ggplot2::as_label(mapping$y)} else {ggplot2::as_label(mapping$x)}
