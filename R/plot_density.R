#' Plot the density of observed and imputed values
#'
#' @param data An object of class [mice::mids].
#' @param vrb String, vector, or unquoted expression with variable name(s), default is "all".
#' @param panels Logical, vector of length 1 specifying whether the density plots should be broken into panels (TRUE) or not (FALSE)
#'
#' @return An object of class [ggplot2::ggplot].
#'
#' @examples
#' imp <- mice::mice(mice::nhanes, print = FALSE)
#' plot_density(data = imp)
#' @export
plot_density <- function(data, vrb = "all", panels = FALSE) {
    verify_data(data, imp = TRUE)
    if (is.null(data$chainMean) && is.null(data$chainVar)) {
        cli::cli_abort("No convergence diagnostics found", call. = FALSE)
    }
    
    # List all variables with missing values
    vrb <- substitute(vrb)
    varlist <- colnames(data$where[, colSums(data$where) != 0])

    # Select variables
    if (as.character(vrb)[1] == "all") {
        vrb <- varlist
    } else {
        vrb <- names(dplyr::select(data$data, {{ vrb }}))
    }
    if (any(vrb %nin% varlist)) {
        cli::cli_inform(
            c(
                "Density plot could not be produced for variable(s):",
                " " = paste(vrb[which(vrb %nin% varlist)], collapse = ", "),
                "x" = "No density plot available."
            )
        )
        if (any(vrb %in% varlist)) {
            vrb <- vrb[which(vrb %in% varlist)]
        } else {
            cli::cli_abort(c(
                "x" = "None of the variables are imputed.",
                "No plots can be produced."
            ))
        }
    }

    # Reshape the mids object to be the best ggplot data possible
    imps_ggplot <- reshape_mids(data)

    # Keep only observed caases
    imps_ggplot_obs <- imps_ggplot %>%
      dplyr::filter(
        .imp == 0,
        .data$miss == FALSE
      )

    # Keep only imputed values
    imps_ggplot_imps <- imps_ggplot %>%
        dplyr::filter(
            .imp != 0,
            .data$miss == TRUE
        )

    # Stack observed and imputed
    imps_ggplot <- rbind(imps_ggplot_obs, imps_ggplot_imps)

    # Create a grouping variable for the densities
    imps_ggplot$group <- paste0(imps_ggplot$.imp, imps_ggplot$miss)

    # Make the grouping variable a factor with meaningful labels
    imps_ggplot$group <- factor(
        imps_ggplot$group,
        levels = unique(imps_ggplot$group),
        labels = c(
            "Observed data",
            paste0("Imputation chain ", seq(1:(length(unique(imps_ggplot$group)) - 1)))
        )
    )

    # Create empty list
    plot_list <- list()

    # Make plot for 1 variable at the time
    for (i in 1:length(vrb)) {
        # Active data for plot
        imps_ggplot_active <- imps_ggplot %>%
            dplyr::filter(.data$variable %in% vrb[i])

        # Base plot
        plot_list[[i]] <- imps_ggplot_active %>%
            ggplot2::ggplot(
                ggplot2::aes(
                    x = value,
                    color = .data$group
                )
            ) +
            ggplot2::geom_density(
                adjust = 1
            )

        # Panel structure
        if (panels == TRUE) {
            plot_list[[i]] <- plot_list[[i]] + ggplot2::facet_grid(
                cols = ggplot2::vars(.data$group),
                scales = "free",
                switch = "y"
            )
        } else {
            plot_list[[i]] <- plot_list[[i]] + ggplot2::facet_grid(
                cols = ggplot2::vars(.data$variable),
                scales = "free",
                switch = "y"
            )
        }

        # Cosmetics
        plot_list[[i]] <- plot_list[[i]] +
            ggplot2::scale_color_manual(
                values = c(
                    "#006CC2B3",
                    rep("#B61A51B3", length(unique(imps_ggplot$.imp)) - 1)
                )
            ) +
            ggplot2::scale_x_continuous(
                breaks = seq(
                    from = min(imps_ggplot_active$value),
                    to = max(imps_ggplot_active$value),
                    length = 5
                ),
                limits = c(
                    min(imps_ggplot_active$value) - stats::sd(imps_ggplot_active$value),
                    max(imps_ggplot_active$value) + stats::sd(imps_ggplot_active$value)
                )
            ) +
            ggplot2::labs(x = vrb[i]) +
            theme_mice() +
            ggplot2::theme(
                strip.text.x = ggplot2::element_blank(),
                strip.background = ggplot2::element_blank(),
                strip.placement = "outside",
                strip.switch.pad.wrap = ggplot2::unit(0, "cm"),
                legend.position = "none"
            )
    }

    # Collect plot together with patchwork
    if(length(plot_list) > 1){
        if (panels == TRUE) {
            patchwork::wrap_plots(plot_list, nrow = min(c(length(vrb), 5)))
        } else {
            patchwork::wrap_plots(plot_list, ncol = min(c(length(vrb), 5)))
        }
    } else {
        plot_list[[1]]
    }
}