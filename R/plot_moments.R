#' Plot the moments of the data and imputed values
#'
#' @param data An object of class [mice::mids].
#' @param vrb String, vector, or unquoted expression with variable name(s),
#'   default is "all".
#'
#' @details
#' The `vrb` argument is "quoted" via [rlang::enexpr()] and evaluated according
#' to [tidy evaluation principles](https://adv-r.hadley.nz/metaprogramming.html).
#' In practice, this technical nuance only affects users when passing an object
#' from the environment (e.g., a vector of variable names) to the `vrb` argument.
#' In such cases, the object must be "unquoted" via the `!!` prefix operator.
#'
#' @returns An object of class [ggplot2::ggplot].
#'
#' @examples
#' # create [mice::mids] object with [mice::mice()]
#' imp <- mice::mice(mice::nhanes, print = FALSE)
#'
#' # plot moments for all imputed columns
#' plot_moments(imp)
#'
#' # plot trace lines for specific columns by supplying a string or character vector
#' plot_moments(imp, "chl")
#' plot_moments(imp, c("chl", "hyp"))

#' # plot trace lines for specific columns by supplying unquoted variable names
#' plot_moments(imp, chl)
#' plot_moments(imp, c(chl, hyp))
#'
#' # plot trace lines for specific columns by passing an object with variable names
#' # from the environment, unquoted with `!!`
#' my_variables <- c("chl", "hyp")
#' plot_moments(imp, !!my_variables)
#' # object with variable names must be unquoted with `!!`
#' try(plot_moments(imp, my_variables))
#'
#' @export
plot_moments <- function(data, vrb = "all") {
  verify_data(data, imp = TRUE)
  # # check if any imp
  # if (is.null(data$chainMean) && is.null(data$chainVar)) {
  #   cli::cli_abort("No convergence diagnostics found", call. = FALSE)
  # }
  vrb <- rlang::enexpr(vrb)
  vrbs_in_data <- names(data$imp)
  vrb_matched <- match_vrb(vrb, vrbs_in_data)
  # extract means and variances
  means_obs <- colMeans(data$data, na.rm = TRUE)
  means_imp <- rowMeans(data$chainMean[, data$iteration, ])
  sds_obs <- sqrt(apply(data$data, na.rm = TRUE, 2, var))
  sds_imp <- sqrt(rowMeans(data$chainVar[, data$iteration, ]))
  # extract relevant variables
  available_vrbs <- vrbs_in_data[(!(is.nan(means_imp) | is.na(sds_imp)))]
  if (any(vrb_matched %nin% available_vrbs)) {
    cli::cli_inform(
      c(
        "Moments plot could not be produced for variable(s):",
        " " = paste(vrb_matched[which(vrb_matched %nin% available_vrbs)], collapse = ", ")
        )
    )
  }
  vrb_matched <- vrb_matched[which(vrb_matched %in% available_vrbs)]
  # create plotting data
  p <- length(vrb_matched)
  long <- cbind(
    data.frame(vrb = vrb_matched),
    .mm = c(rep("mean", p), rep("SD", p)),
    obs = c(means_obs[vrb_matched], sds_obs[vrb_matched]),
    imp = c(means_imp[vrb_matched], sds_imp[vrb_matched])
  )
  # create plot
  ggplot2::ggplot(long,
                  ggplot2::aes(
                    x = .data$obs,
                    y = .data$imp,
                    color = .data$vrb
                  )) +
    ggplot2::geom_point() +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    ggplot2::facet_wrap(
      ~ .mm,
      dir = "v",
      ncol = 2,
      scales = "free",
      labeller = ggplot2::as_labeller(
        c(mean = "mean", SD = "SD")),
      strip.position = "top"
    ) +
    ggplot2::labs(
      x = "Observed data",
      y = "Imputed data",
      color = ""
    ) +
    theme_mice() +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      strip.placement = "outside",
      strip.switch.pad.wrap = ggplot2::unit(0, "cm")
    )
}
