# Functions for internal use

# Shorthand 'not in' for code readability
`%nin%` <- Negate(`%in%`)

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' Utils function to validate data argument inputs
#'
#' @param data The input supplied to the 'data' argument.
#' @param classes String or character vector specifying which of the data types `data.frame`, `mids`, and/or `matrix` are allowed.
#'
#' @return Either nothing or an error.
#'
#' @keywords internal
#' @noRd
verify_data <- function(data,
                        classes) {
  if(!rlang::inherits_any(data, classes)){
    classes_format <- purrr::map(classes, function(x) paste0("`", x, "`")) %>% unlist() # format type names to be used in function
    cli::cli_abort(c(
      "!" = "The {.arg data} argument requires an object of class {stringr::str_flatten_comma({classes_format}, \", or \")}.",
      "i" = "Input object is of class `{class(data)}`"
    ),
    call. = FALSE)
  }
}

#' Utils function to process variable vector as character vector
#'
#' @param vrb The input supplied to the `vrb` argument.
#' @param data The input supplied to the `data` argument.
#'
#' @return String with variable names
#'
#' @keywords internal
#' @noRd
vrb_to_cols <- function(vrb, data){
  if (vrb[1] == "all") {
    vrb <- colnames(data)
  } else {
    vrb <- as.data.frame(data) %>%
      dplyr::select({{vrb}}) %>%
      colnames()
  }
  return(vrb)
}

# suppress undefined global functions or variables note
utils::globalVariables(c(".id", ".imp", ".where", ".id", "where", "name", "value"))
