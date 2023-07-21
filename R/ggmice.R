#' Plot incomplete or imputed data
#'
#' @param data An incomplete dataset (of class `data.frame`), or an object of class [`mice::mids`].
#' @param mapping A list of aesthetic mappings created with [ggplot2::aes()].
#'
#' @return An object of class [`ggplot2::ggplot`].
#'
#' @examples
#' dat <- mice::nhanes
#' ggmice(dat, ggplot2::aes(x = age, y = bmi)) + ggplot2::geom_point()
#' @seealso See the `ggmice` vignette to use the `ggmice()` function on
#' [incomplete data](https://amices.org/ggmice/articles/ggmice.html#the-ggmice-function)
#' or [imputed data](https://amices.org/ggmice/articles/ggmice.html#the-ggmice-function-1).
#' @export
ggmice <- function(data = NULL,
                   mapping = ggplot2::aes()) {
  # validate inputs
  verify_data(data, df = TRUE, imp = TRUE)
  if (is.null(mapping$x) && is.null(mapping$y)) {
    cli::cli_abort(
      c(
        "At least one of the mapping arguments 'x' or 'y' is required.",
        "i" = "Supply variable name(s) with ggplot2::aes()."
      )
    )
  }
  if (is.character(mapping$x) || is.character(mapping$y)) {
    cli::cli_abort(
      c(
        "The mapping argument requires variable name(s) of type 'quosure', supplied with ggplot2::aes().",
        "x" = "You have supplied a string."
      )
    )
  }
  if (!is.null(mapping$colour)) {
    cli::cli_warn(
      c(
        "The aes() argument 'colour' has a special use in ggmice() and will be overwritten.",
        "i" = "Try using 'shape' or 'linetype' for additional mapping, or use faceting."
      )
    )
  }

  # extract variable names from mapping object
  if (is.data.frame(data)) {
    vrbs <- names(data)
    vrbs_num <- vrbs[purrr::map_lgl(data, is.numeric)]
  }
  if (mice::is.mids(data)) {
    vrbs <- names(data$data)
    vrbs_num <- vrbs[purrr::map_lgl(data$data, is.numeric)]
  }
  if (length(vrbs) > length(unique(vrbs))) {
    cli::cli_abort(
      c("The data must have unique column names.",
        "x" = "Duplication found in {vrbs[duplicated(vrbs)]}")
    )
  }
  # extract mapping variables
  vrb_x <- match_mapping(data, vrbs, mapping$x)
  vrb_y <- match_mapping(data, vrbs, mapping$y)

  # edit data and mapping objects
  if (is.data.frame(data)) {
    where_xy <- rowSums(is.na(as.matrix(data[, c(vrb_x, vrb_y)]))) > 0L
    mice_data <- cbind(.where = factor(
      where_xy == 1,
      levels = c(FALSE, TRUE),
      labels = c("observed", "missing"),
      ordered = TRUE
    ),
    data)
    if (!is.null(mapping$x) && !is.null(mapping$y)) {
      mice_data <- dplyr::mutate(
        mice_data,
        dplyr::across(
          tidyselect::all_of(vrbs_num),
          ~ tidyr::replace_na(as.numeric(.x), -Inf)
        ),
        dplyr::across(tidyselect::all_of(vrbs[vrbs %nin% vrbs_num]), ~ {
          as.factor(tidyr::replace_na(as.character(.x), " "))
        })
      )
    }
    mice_mapping <-
      utils::modifyList(mapping, ggplot2::aes(colour = .where))
    mice_colors <-
      c("observed" = "#006CC2B3",
        "missing" = "#B61A51B3")
  }
  if (mice::is.mids(data)) {
    where_xy <- rowSums(as.matrix(data$where[, c(vrb_x, vrb_y)])) > 0L
    miss_xy <-
      rowSums(as.matrix(is.na(data$data[, c(vrb_x, vrb_y)]))) > 0L
    mice_data <- dplyr::mutate(
      rbind(
        data.frame(
          .where = "observed",
          .imp = 0,
          .id = rownames(data$data),
          data$data
        )[!miss_xy, ],
        data.frame(.where = "imputed", mice::complete(data, action = "long"))[where_xy, ]
      ),
      .where = factor(
        .where,
        levels = c("observed", "imputed"),
        ordered = TRUE
      ),
      .imp = factor(.imp, levels = 0:data$m, ordered = TRUE)
    )
    mice_mapping <-
      utils::modifyList(mapping, ggplot2::aes(colour = .where))
    mice_colors <-
      c("observed" = "#006CC2B3",
        "imputed" = "#B61A51B3")
  }

  # create plot
  gg <- ggplot2::ggplot(data = mice_data, mapping = mice_mapping) +
    ggplot2::scale_color_manual(values = mice_colors,
                                name = "",
                                drop = FALSE) +
    theme_mice()

  # edit plot to display missing values on the axes
  if (is.data.frame(data) &&
      !is.null(mapping$x) && !is.null(mapping$y)) {
    gg <- gg +
      ggplot2::coord_cartesian(clip = "off")
    if (!is.null(mapping$x)) {
      if (vrb_x %nin% vrbs_num) {
        gg <- gg +
          ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = c(0, 0.6)))
      }
    }
    if (!is.null(mapping$y)) {
      if (vrb_y %nin% vrbs_num) {
        gg <- gg +
          ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = c(0, 0.6)))
      }
    }
  }

  # output
  return(gg)
}


#' Utils function to extract mapping variables
#'
#' @param data Incomplete dataset or mids object.
#' @param vrbs Column names.
#' @param mapping_in Mapping provided to ggmice().
#' @return Variable name from mapping_in argument matched on vrbs argument.
#' @keywords internal
#' @noRd
match_mapping <- function(data, vrbs, mapping_in) {
  if (is.null(mapping_in)) {
    return(NULL)
  }
  # parse data
  if (mice::is.mids(data)) {
    mapping_data <- data$data
  } else {
    mapping_data <- data
  }
  # parse mapping
  mapping_text <- ggplot2::as_label(mapping_in)
  if (stringr::str_detect(mapping_text, "log\\(")) {
    cli::cli_abort(
      c(
        "Log transformations are currently not supported by ggmice() in the mapping input.",
        "i" = "Please transform the data input, or use the ggplot2::scale_*_continuous(trans='log10') function."
      )
    )
  }
  if (mapping_text %in% vrbs) {
    mapping_out <- mapping_text
  }
  if ((mice::is.mids(data) &&
       mapping_text %in% c(".id", ".imp", ".where"))) {
    mapping_out <- NULL
  }
  if (!is.null(mapping_in) &&
      mapping_text %nin% c(vrbs, ".id", ".imp", ".where")) {
    mapping_out <- vrbs[stringr::str_detect(mapping_text, vrbs)]
    if (identical(mapping_out, character(0)) ||
        inherits(try(dplyr::mutate(mapping_data,
                                   !!rlang::parse_quo(mapping_text, env = rlang::current_env())),
                     silent = TRUE)
                 ,
                 "try-error")) {
      cli::cli_abort(
        c("Must provide a valid mapping variable.",
          "x" = "Mapping variable '{mapping_text}' not found in the data or imputations.")
      )
    } else {
      cli::cli_warn(
        c(
          "Mapping variable '{mapping_text}' recognized internally as {mapping_out}.",
          "Please verify whether this matches the requested mapping variable."
        )
      )
    }
  }
  # output
  return(mapping_out)
}
