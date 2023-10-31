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

#' Utils function to reshape mids object into a super-long format for density plots
#'
#' @param data A mids object.
#' @details
#' 
#' ggplot works best with long format data. This function takes a mids object and returns a super-long format version of the mids object that is easy to process with ggplot
#' This super-long format has 5 columns:
#' - .imp: reference imputation data set (same as in mids object)
#' - .id: observation id (same as in mids object)
#' - miss: response vector (logical vector indicating whether the case was missing (1) or not (0))
#' - variable: vector of names of the variables in the original data set
#' - value: the data point (observed, na, or imputation value)
#' 
#' This structure makes it easy to group, color, filter, and facet by the two most important factors for density plots: missingness and variables.
#'
#' @return data.frame in super-long format
#' 
#' @examples
#' imp <- mice::mice(mice::nhanes, print = FALSE)
#' reshape_mids(data = imp)
#' @keywords internal
#' @noRd
reshape_mids <- function(data) {

  # Create an empty list to store intermediate objects
  shelf <- list()

  # Extract imputations in long format
  imps <- data.frame(mice::complete(data, "long", include = TRUE))

  # Define column names for melting (depends on mice long format column names)
  id_vars <- colnames(imps)[1:2]

  # Define the response matrix
  Rmat <- data$where

  # Loop over the variables in the original data
  for (j in 1:ncol(Rmat)) {

    # Check if there are missing values
    if (any(Rmat[, j])) {

      # What variable are we considering
      J <- colnames(Rmat)[j]

      # Keep only the .imp identifier and the variable value
      active_data <- imps[, c(id_vars, J)]

      # Force active variable to numeric
      active_data[, J] <- as.numeric(active_data[, J])

      # attach the response indicator
      active_data <- cbind(
        active_data,
        miss = Rmat[, J]
      )

      # Melt values
      ad_melt <- reshape2::melt(active_data, id.vars = c(id_vars, "miss"))

      # Store the result
      shelf[[j]] <- ad_melt
    }
  }

  # Combine the results from the many variables
  do.call(rbind, shelf)

}