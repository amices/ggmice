ggmice <- function(data = NULL, mapping = aes()) {
  mapping_args <- names(mapping)
  try(if(!any(c("x", "y") %in% mapping_args)){
    stop("Provide at least one of the mapping arguments 'x' or 'y'.")
  })
  if(any(c("colour", "fill", "group") %in% mapping_args)){
    warning("The mapping arguments 'colour', 'fill' and 'group' have a special use in ggmmice() and will be overwritten, maybe use 'shape' or 'linetype' instead?")
  }
  # x_mis <- if("x" %in% mapping_args){data$where[, rlang::as_name(mapping$x)]} else FALSE
  # y_mis <- if("y" %in% mapping_args){data$where[, rlang::as_name(mapping$y)]} else FALSE
  # xy_mis <- x_mis | y_mis
  xy_vrb <- c(if("y" %in% mapping_args){rlang::as_name(mapping$y)}, if("y" %in% mapping_args){rlang::as_name(mapping$y)})
  xy_mis <- rowSums(data$where[, xy_vrb] > 0)

  mice_data <- cbind(mice::complete(data, action = "long", include = TRUE),
                     .mis = factor(xy_mis, levels = c(0,1), labels = c("observed", "imputed"), ordered = TRUE))
  mice_mapping <- modifyList(mapping, aes(colour = .mis, fill = .mis, group = factor(.imp, ordered = TRUE)))
  gg <- ggplot2::ggplot(data = mice_data, mapping = mice_mapping)
  return(gg)
}
