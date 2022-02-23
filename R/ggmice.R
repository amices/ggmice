#' Title
#'
#' @param data An incomplete dataset or an object of class `mids`
#' @param mapping A list of aesthetic mappings created with `ggplot2::aes()`
#'
#' @return A ggplot object of class `gg`
#'
#' @examples
#' imp <- mice::mice(mice::nhanes, print = FALSE)
#' ggmice(imp, ggplot2::aes(x = age, y = bmi))
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
  } else {
    vrbs <- names(data)
  }
  vrbs_xy <- vrbs[stringr::str_detect(ggplot2::as_label(mapping$x), vrbs) | stringr::str_detect(ggplot2::as_label(mapping$y), vrbs)]
  # edit data and mapping objects
  if(mice::is.mids(data)){
    mice_data <- dplyr::mutate(mice::complete(data, action = "long", include = TRUE),
                        .imp = factor(.imp, levels = 0:data$m, ordered = TRUE),
                        .mis = rep(rowSums(as.matrix(data$where[, vrbs_xy])) > 0L, data$m + 1L),
                        .mis = factor(.mis, levels = c(FALSE, TRUE), labels = c("observed", "imputed"), ordered = TRUE))
    mice_mapping <- utils::modifyList(mapping, ggplot2::aes(colour = .mis, fill = .mis))
  } else {
    mice_data <- dplyr::mutate(data,
                       .mis = rowSums(is.na(as.matrix(data[, vrbs_xy]))) > 0L,
                       .mis = factor(.mis, levels = c(FALSE, TRUE), labels = c("observed", "missing"), ordered = TRUE))
    mice_mapping <- utils::modifyList(mapping, ggplot2::aes(colour = .mis, fill = .mis))
  }
  # create plot
  mice_colors <- c("observed" = "#006CC2B3", "missing" = "#B61A51B3", "imputed" = "#B61A51B3")
  gg <- ggplot2::ggplot(data = mice_data, mapping = mice_mapping) +
    ggplot2::scale_color_manual(values = mice_colors, drop = TRUE, name = "") +
    ggplot2::scale_fill_manual(values = mice_colors, drop = TRUE, name = "") +
    theme_mice()

  # output
  return(gg)
}


# x_mis <- if("x" %in% mapping_args){data$where[, rlang::as_name(mapping$x)]} else FALSE
# y_mis <- if("y" %in% mapping_args){data$where[, rlang::as_name(mapping$y)]} else FALSE
# xy_mis <- x_mis | y_mis
# mapping_vrb <- c(if("x" %in% mapping_args){rlang::as_name(mapping$x)}, if("y" %in% mapping_args){rlang::as_name(mapping$y)})
# mice_data <- cbind(mice::complete(data, action = "long", include = TRUE),
#                    .mis = factor(rowSums(data$where[, mapping_vrb] > 0), levels = c(0, 1), labels = c("observed", "imputed"), ordered = TRUE))
# yvar <- if("y" %in% mapping_args) {ggplot2::as_label(mapping$y)} else {ggplot2::as_label(mapping$x)}
