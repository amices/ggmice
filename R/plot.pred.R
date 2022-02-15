# plot predictor matrix
#' Title
#'
#' @param pred A predictor matrix for `mice`
#'
#' @return An object of class `ggplot`
#'
#' @examples
#' pred <- mice::make.predictorMatrix(mice::nhanes)
#' plot_pred(pred)
#' @export
plot_pred <- function(pred){
  vrbs <- row.names(pred)
  p <- length(vrbs)
  long <- data.frame(
    vrb = rep(vrbs, each = p),
    prd = vrbs,
    ind = matrix(pred, nrow = p*p, byrow = TRUE))
  # long <- tidyr::pivot_longer(cbind(vrb = vrbs, data.frame(pred)), vrbs, names_to = "prd")
  # text <- tidyr::pivot_longer(cbind(vrb = vrbs, data.frame(round(cov2cor(cov(dat, use =  "pairwise.complete.obs")), 3))), vrbs, names_to = "txt")
  # data.frame(pred, correlation = round(cov2cor(cov(dat, use =  "pairwise.complete.obs")), 3))
  ggplot2::ggplot(long, ggplot2::aes(x = prd, y = vrb, color = ifelse(ind == 1, "yes", "no"))) +
    ggplot2::geom_tile(fill = "white", size = 3, width = 0.9, height = 0.9) +
    ggplot2::scale_x_discrete(limits = vrbs, position = "top") +
    ggplot2::scale_y_discrete(limits = rev(vrbs)) +
    ggplot2::scale_color_manual(values = c("yes" = "#006CC2B3", "no" = "#B61A51B3")) +
    ggplot2::labs(x = "Predictor in imputation model",
         y = "Variable to impute",
         color = "Predictor used",
         fill = "") +
    ggplot2::theme_minimal()
}

# plot quickpred
#' Title
#'
#' @param dat An incomplete data.frame
#' @param ... Optional additional arguments to \code{mice::quickpred}
#'
#' @return An object of class \code{ggplot}
#'
#' @examples
#' plot_quickpred(mice::nhanes)
#' @export
plot_quickpred <- function(dat, ...){
  vrbs <- names(dat)
  p <- length(vrbs)
  corrs <- data.frame(
    vrb = rep(vrbs, each = p),
    prd = vrbs,
    corr = matrix(round(stats::cov2cor(stats::cov(dat, use =  "pairwise.complete.obs")), 3), nrow = p*p, byrow = TRUE))
  plot_pred(mice::quickpred(dat, ...)) +
    ggplot2::geom_text(ggplot2::aes(x = vrb, y = prd, label = paste("r =", corr)), color = "black", data = corrs)
}
