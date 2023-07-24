# Functions for internal use

# util functions
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
utils::globalVariables(c(".id", ".imp", ".where", ".id", "where", "name", "value"))

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
