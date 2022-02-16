# plot predictor matrix
#' Title
#'
#' @param pred A predictor matrix for `mice`
#' @param label Logical indicating whether predictor matrix values should be displayed
#'
#' @return An object of class `ggplot`
#'
#' @examples
#' pred <- mice::make.predictorMatrix(mice::nhanes)
#' plot_pred(pred)
#' @export
plot_pred <- function(pred, label = FALSE){
  vrbs <- row.names(pred)
  p <- dim(pred)[2]
  long <- data.frame(
    prd = rep(vrbs, each = p),
    vrb = vrbs,
    ind = matrix(pred, nrow = p*p, byrow = TRUE))
  gg <- ggplot2::ggplot(long, ggplot2::aes(x = prd, y = vrb, label = ind, color = ifelse(ind == 0, "no", "yes"))) +
    ggplot2::geom_tile(fill = "white", size = 3, width = 0.9, height = 0.9) +
    ggplot2::scale_x_discrete(limits = vrbs, position = "top") +
    ggplot2::scale_y_discrete(limits = rev(vrbs)) +
    ggplot2::scale_color_manual(values = c("yes" = "#006CC2B3", "no" = "#B61A51B3")) +
    ggplot2::labs(x = "Predictor in imputation model",
         y = "Variable to impute",
         color = "Predictor used",
         fill = "") +
    ggplot2::theme_minimal()
  if(label == TRUE) {gg <- gg + ggplot2::geom_text(color = "black", show.legend = FALSE)}
  return(gg)
}

# plot quickpred
#' Title
#'
#' @param dat An incomplete data.frame
#' @param ... Optional additional arguments to `mice::quickpred`
#'
#' @return An object of class `ggplot`
#'
#' @examples
#' plot_quickpred(mice::nhanes)
#' @export
plot_quickpred <- function(dat, ...){
  #TODO: make this work with other pred matrices
  vrbs <- names(dat)
  p <- length(vrbs)
  corrs <- data.frame(
    vrb = rep(vrbs, each = p),
    prd = vrbs,
    corr = matrix(round(stats::cov2cor(stats::cov(dat, use =  "pairwise.complete.obs")), 3), nrow = p*p, byrow = TRUE))
  plot_pred(mice::quickpred(dat, ...)) +
    ggplot2::geom_text(ggplot2::aes(x = vrb, y = prd, label = paste("r =", corr)), color = "black", data = corrs)
}

#' Title
#'
#' @param dat An incomplete data.frame
#' @param label Logical indicating whether correlation values should be displayed
#'
#' @return An object of class `ggplot`
#'
#' @examples
#' plot_corr(mice::nhanes, label = TRUE)
#' @export
plot_corr <- function(dat, label = FALSE){
  vrbs <- names(dat)
  p <- dim(dat)[2]
  corrs <- data.frame(
    vrb = rep(vrbs, each = p),
    prd = vrbs,
    corr = matrix(round(stats::cov2cor(stats::cov(data.matrix(dat), use =  "pairwise.complete.obs")), 2), nrow = p*p, byrow = TRUE))
  gg <- ggplot2::ggplot(corrs, ggplot2::aes(x = prd, y = vrb, label = corr, color = corr)) +
    ggplot2::geom_tile(fill = "white", size = 2*p, width = 0.8, height = 0.8) +
    ggplot2::scale_x_discrete(limits = vrbs, position = "top") +
    ggplot2::scale_y_discrete(limits = rev(vrbs)) +
    ggplot2::scale_color_gradient2(low = "navyblue", mid = "white", high = "darkred", limits = c(-1, 1)) +
    ggplot2::labs(x = "",
                  y = "",
                  color = "Correlation*",
                  fill = "",
                  caption = "*paiwise complete observations") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom",
                   legend.justification = "right")
  if(label == TRUE) {gg <- gg + ggplot2::geom_text(color = "black", show.legend = FALSE)}
  return(gg)
}
