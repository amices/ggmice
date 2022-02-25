# bwplot
bwplot <- function(imp, vrb =  "all") {
  if(vrb == "all") {
      vrb <- names(imp$data)
  }
  gg <- purrr::map(vrb, ~{
    ggmice(imp, ggplot2::aes_string(y = .x)) +
      ggplot2::geom_boxplot() +
      ggplot2::labs(x = "Imputation number\n(0 = original data)")
  })
}
