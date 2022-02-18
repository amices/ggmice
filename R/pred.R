# plot predictor matrix
#' Title Plot predictorMatrix for `mice::mice()` argument
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
    ggplot2::geom_tile(fill = "white", size = 1.5*p, width = 0.8, height = 0.8) +
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

#' Title Plot correlations between (incomplete) variables
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

#' Title
#'
#' @param dat An incomplete data.frame
#'
#' @return An object of class `ggplot`
#'
#' @examples
#' plot_corr(mice::nhanes)
#' @export
plot_test <- function(dat){
  M <- is.na(dat)
  v <- names(dat)

  est <- purrr::map_dfr(v[colSums(M)>0], ~{dat[, v != .x] %>% scale() %>% as.data.frame() %>%
    glm(M[, .x] ~ . -1, data = ., family = "binomial", na.action = "na.exclude") %>%
    coef() %>% data.frame(M = .x, prd = names(.), b = ., row.names = NULL)})

  ggplot2::ggplot(est, ggplot2::aes(x = prd, y = M, fill = b)) +
    ggplot2::geom_tile() +
    ggplot2::scale_x_discrete(limits = v, position = "top") +
    ggplot2::scale_y_discrete(limits = rev(v), labels = rev(paste0("M(", v, ")")), drop = FALSE) +
    ggplot2::scale_fill_gradient2(low = "navyblue", mid = "white", high = "darkred", limits = c(-1, 1), na.value = "white") +
    ggplot2::labs(x = "Predictor variable",
         y = "Missingness indicator",
         fill = "St. beta") +
    ggplot2::theme_minimal()
}
