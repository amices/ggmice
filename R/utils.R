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
utils::globalVariables(c(".imp", ".where", ".id", "where", "name", "value"))

# Alias a function with `foo <- function(...) pkgB::blah(...)`

# argument preprocessing
verify_data <- function(data,
                        df = FALSE,
                        imp = FALSE,
                        pred = FALSE) {
  if (df && !imp) {
    if (!(is.data.frame(data) || is.matrix(data))) {
      stop("The 'data' argument requires an object of class 'data.frame' or 'matrix'.",
        call. = FALSE
      )
    }
  }
  if (df && imp) {
    if (!(is.data.frame(data) ||
      is.matrix(data) || mice::is.mids(data))) {
      stop(
        "The 'data' argument requires an object of class 'data.frame', 'matrix', or 'mids'.",
        call. = FALSE
      )
    }
  }
  if (imp && !df) {
    if (!mice::is.mids(data)) {
      stop("The 'data' argument requires an object of class 'mids'.",
        call. = FALSE
      )
    }
  }
  if (pred) {
    if (!is.matrix(data)) {
      stop("The 'data' argument requires an object of class 'matrix'.",
        call. = FALSE
      )
    }
    if (dim(data)[1] != dim(data)[2] || is.null(rownames(data)) || is.null(colnames(data))) {
      warning(
        "The 'data' argument expects a square predictor matrix with equal row and column names.\n
        Try using `mice::make.predictorMatrix()` or `mice::quickpred()`.",
        call. = FALSE
      )
    }
  }
}

verify_vrb <- function(data, vrb) {
  vrb <- substitute(vrb)
  if (vrb[1] == "all") {
    vrb <- names(data)
  } else {
    vrb <- names(dplyr::select(data, {{ vrb }}))
  }
  return(vrb)
}
