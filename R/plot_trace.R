#' Plot the trace lines of the imputation algorithm
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
#' # plot trace lines for all imputed columns
#' plot_trace(imp)
#'
#' # plot trace lines for specific columns by supplying a string or character vector
#' plot_trace(imp, "chl")
#' plot_trace(imp, c("chl", "hyp"))

#' # plot trace lines for specific columns by supplying unquoted variable names
#' plot_trace(imp, chl)
#' plot_trace(imp, c(chl, hyp))
#'
#' # plot trace lines for specific columns by passing an object with variable names
#' # from the environment, unquoted with `!!`
#' my_variables <- c("chl", "hyp")
#' plot_trace(imp, !!my_variables)
#' # object with variable names must be unquoted with `!!`
#' try(plot_trace(imp, my_variables))
#'
#' @export
plot_trace <- function(data, vrb = "all") {
  verify_data(data, imp = TRUE)
  if (is.null(data$chainMean) && is.null(data$chainVar)) {
    cli::cli_abort("No convergence diagnostics found", call. = FALSE)
  }
  vrb <- rlang::enexpr(vrb)
  vrbs_in_data <- names(data$imp)
  vrb_matched <- match_vrb(vrb, vrbs_in_data)
  # extract chain means and chain standard deviations
  mn <- data$chainMean
  sm <- sqrt(data$chainVar)
  available_vrbs <- vrbs_in_data[apply(!(is.nan(mn) | is.na(sm)), 1, all)]
  if (any(vrb_matched %nin% available_vrbs)) {
    cli::cli_inform(
      c(
        "Trace plot could not be produced for variable(s):",
        " " = paste(vrb_matched[which(vrb_matched %nin% available_vrbs)], collapse = ", "),
        "i" = "No convergence diagnostics found."
      )
    )
  }
  vrb_matched <- vrb_matched[which(vrb_matched %in% available_vrbs)]
  # compute diagnostics
  p <- length(vrb_matched)
  m <- data$m
  it <- data$iteration
  long <- cbind(
    expand.grid(.it = seq_len(it), .m = seq_len(m)),
    data.frame(
      .ms = rep(c("mean", "sd"), each = m * it * p),
      vrb_matched = rep(vrb_matched, each = m * it, times = 2),
      val = c(
        matrix(aperm(mn[vrb_matched, , , drop = FALSE], c(
          2, 3, 1)), nrow = m * it * p),
        matrix(aperm(sm[vrb_matched, , , drop = FALSE], c(
          2, 3, 1)), nrow = m * it * p)
        )
    )
  )
  # create plot
  ggplot2::ggplot(long,
                  ggplot2::aes(
                    x = .data$.it,
                    y = .data$val,
                    color = as.factor(.data$.m)
                  )) +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::geom_hline(yintercept = -Inf) +
    ggplot2::facet_wrap(
      .ms ~ vrb_matched,
      dir = "v",
      ncol = 2,
      scales = "free_y",
      strip.position = "left",
      labeller = function(labels) {
        labels <- lapply(labels, as.character)
        list(do.call(paste, c(labels, list(sep = "\n"))))
      }
    ) +
    ggplot2::labs(x = "Iteration", y = "Imputation parameter", color = "Imputation number") +
    theme_mice() +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      strip.placement = "outside",
      strip.switch.pad.wrap = ggplot2::unit(0, "cm")
    )
}
