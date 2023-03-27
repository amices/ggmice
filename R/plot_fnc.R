library(ggplot2)


#' Create a plot based on synthetic datasets
#'
#' @param syn.obj a list of synthetic datasets
#' @param lm.formula a linear model formula that user specify e.g., y ~ x
#'
#'
#' @return a ggplot object

synplot1 <- function(syn.obj, lm.formula){
  vars <- all.vars(lm.formula)
  # row-bind all dfs
  dplyr::bind_rows(!!!syn.obj, .id="id") %>%
  # suppose lm.formula contains one predictor x
    ggplot(aes_string(x = vars[2], y = vars[1], color= "id")) +
    geom_point(size=1, alpha = 0.2) +
    geom_line(stat = "smooth", method = lm, alpha = 0.5, position = position_dodge(width = 1)) +
    theme_classic() +
    labs(title = paste("Linear Regression", deparse(lm.formula)), color="")
}




#' Create a plot of fitted values vs. observed values
#'
#' @param model.list a list of fitted model objects
#' @param smoother a smoothing method to use (default = "lm")
#'
#'
#' @return a ggplot object

synplot2 <- function(model.list, smoother = "lm"){
  # get the model formula
  form <- formula(model.list[[1]])
  model.list %>%
    purrr::map(
      ~.x[c("fitted.values", "model", "residuals")] %>%
        dplyr::bind_cols() %>%
        # select fitted.values & DV which are the 1st and 2nd vars
        dplyr::select(1:2, last_col())
    ) %>%
    dplyr::bind_rows(.id = "ids") %>%
    dplyr::group_by(ids) %>%
    # compute MSE per dataset
    dplyr::mutate(MSEs = mean(residuals^2),
                  ids = glue::glue('{ids}, MSE= {round(MSEs, 2)}')) %>%
    dplyr::ungroup() %>%
    {   # grab the dependent variable and unquote it
      ggplot(., aes(x = fitted.values,
                    y = !!rlang::sym(all.vars(form)[1]),

                    color= ids)) +
        geom_point(size = 1, alpha = 0.2) +
        geom_line(stat = "smooth",
                  method = smoother,
                  alpha = 0.5,
                  #position = position_dodge(width = 1)
        ) +
        theme_classic() +
        labs(title = paste(smoother, "line for", deparse(form)), color="",
             subtitle = paste("Average MSE =", round(mean(.$residuals^2),2)))
    }
}



library(magrittr)
library(mice)

#' Create a plot of average fitted values vs. observed values
#' to show the variance between fitted values for the imputed datasets
#'
#' @param dat original data
#' @param imp.method imputation method (default = NULL)
#' @param lm.formula linear model formula
#' @param seed seed for mice (default = NA)
#' @param print print the output table (default = FALSE)
#'
#' @return a ggplot object

modelplot <- function(dat, imp.method=NULL, lm.formula, seed=NA, print=FALSE){
  # get the DV
  dv <- all.vars(lm.formula)[1]
  # imputing...
  tmp <- dat %>% mice::mice(print=FALSE, meth = imp.method, seed=seed) %>% complete("all") %>%
    # fit the model
    purrr::map(~.x %$% do.call("lm", list(lm.formula, .)) %>%
                 # extract fitted vals
                 .$fitted.values) %>%
    # gerko: dv --> observed value ?
   b %>%  dplyr::bind_cols(., DV = dat[,dv]) %>%
    dplyr::rowwise() %>%
    # get the avg.fitted vals and variance (last col = observed val)
    dplyr::mutate(means = mean(dplyr::c_across(-DV)),
                  vars = var(dplyr::c_across(-DV))
    )
  plot <- tmp %>%
    ggplot(aes(x = means, y = DV, fill = vars, size = vars)) +
    geom_point(alpha = 0.7, col = "black", shape=21)  +
    # reverse the col order
    #scale_color_distiller(palette = "YlOrRd", trans="reverse") +
    scale_fill_gradient(low =  "white", high = "#B61A51B3") +
    labs(title = paste("Model: ", deparse(lm.formula)),
         x = "average fitted value", y = paste("observed", dv), color = "variance") +
    theme_classic() +
    # flip the color bar
    # guides(size = FALSE, col = guide_colourbar(reverse=T))
    guides(size = FALSE)

  # output table
  tab <- tmp %>% data.table::data.table()

  if(print) return(list(plot, tab)) else return(plot)
}



