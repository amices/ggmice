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

# suppress undefined global functions or variables note
utils::globalVariables(c(".id", ".imp", ".where", ".id", "where", "name", "value", "nmis"))

# Alias a function with `foo <- function(...) pkgB::blah(...)`

#' Utils function to validate data argument inputs
#'
#' @param data The input supplied to the 'data' argument.
#' @param df Logical indicating whether 'data.frame' inputs are permitted.
#' @param imp Logical indicating whether 'mids' inputs are permitted.
#' @param pred Logical indicating whether predictor matrix inputs are permitted.
#'
#' @return Either nothing or an error.
#'
#' @keywords internal
#' @noRd
verify_data <- function(data,
                        df = FALSE,
                        imp = FALSE,
                        pred = FALSE) {
  if (df && !imp) {
    if (!(is.data.frame(data) || is.matrix(data))) {
      cli::cli_abort(
        c(
          "The 'data' argument requires an object of class 'data.frame' or 'matrix'.",
          "i" = "Input object is of class {class(data)}."
        ),
        call. = FALSE
      )
    }
  }
  if (df && imp) {
    if (!(is.data.frame(data) ||
          is.matrix(data) || mice::is.mids(data))) {
      cli::cli_abort(
        c(
          "The 'data' argument requires an object of class 'data.frame', 'matrix', or 'mids'.",
          "i" = "Input object is of class {class(data)}."
        ),
        call. = FALSE
      )
    }
  }
  if (imp && !df) {
    if (!mice::is.mids(data)) {
      cli::cli_abort(
        c(
          "The 'data' argument requires an object of class 'mids'.",
          "i" = "Input object is of class {class(data)}."
        ),
        call. = FALSE
      )
    }
  }
  if (pred) {
    if (!is.matrix(data)) {
      cli::cli_abort(
        c(
          "The 'data' argument requires an object of class 'matrix'.",
          "i" = "Input object is of class {class(data)}."
        ),
        call. = FALSE
      )
    }
    if (dim(data)[1] != dim(data)[2]) {
      cli::cli_abort(
        c(
          "The 'data' argument requires a square predictor matrix.",
          "i" = "Input object has {dim(data)[1]} rows and {dim(data)[2]} columns."
        ),
        call. = FALSE
      )
    }
    if (is.null(rownames(data)) || is.null(colnames(data)) ||
        !all.equal(rownames(data), colnames(data))) {
      cli::cli_warn(
        c(
          "The 'data' argument expects a square predictor matrix with equal row and column names.",
          "i" = "Try using `mice::make.predictorMatrix()` or `mice::quickpred()`."
        ),
        call. = FALSE
      )
    }
  }
}

#' Utils function to match `vrb` argument to variable names in `data`
#'
#' @param vrb The input supplied to the 'vrb' argument.
#' @param vrbs_in_data A character vector of available variable names in `data`.
#'
#' @return String or character vector with matched variable name(s).
#'
#' @keywords internal
#' @noRd
match_vrb <- function(vrb, vrbs_in_data) {
  if (is.call(vrb))
    vrb <- as.character(vrb)[-1]
  if (is.symbol(vrb))
    vrb <- as.character(vrb)
  if (length(vrb) == 1 && as.character(vrb) == "all") {
    vrb <- vrbs_in_data
  }
  if (all(vrb %nin% vrbs_in_data)) {
    cli::cli_abort(
      c(
        "x" = "The variable name(s) supplied to {.var vrb} could not be found in {.var data}.",
        "i" = "If you supply an object with variable names from the environment, use `!!` to unqote:",
        " " = paste0("{.code vrb = !!", vrb, "}")
      )
    )
  }
  if (any(vrb %nin% vrbs_in_data)) {
    cli::cli_warn(c("x" = "The following variables are not present in {.var data}:", " " = paste(
      setdiff(vrb, vrbs_in_data), collapse = ", "
    )))
  }
  return(vrb)
}

# suppress undefined global functions or variables note
utils::globalVariables(c(".id", ".imp", ".where", ".id", "where", "name", "value"))
