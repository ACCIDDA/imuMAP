#' @title Configure imuMAP Execution Options
#'
#' @description Creates an `imumap_options` configuration object for tuning
#'   execution parameters such as matrix sparsity, distance thresholds, and
#'   zero-population handling.
#'
#' @param max_distance Numeric. Global cutoff distance. Default is `Inf`.
#' @param zero_pop_handling Character. Strategy for handling target units with 0
#'   population: `"distribute_evenly"`, `"ignore"`, or `"error"`.
#' @param tie_breaking Character. Strategy for equidistant source-target ties.
#' @param sparse Logical. If `TRUE`, intermediate matrices utilize sparse
#'   `dgCMatrix` formats. Default `TRUE`.
#'
#' @return An S3 object of class `imumap_options`.
#'
#' @examples
#' opts <- imumap_options(sparse = TRUE, max_distance = 10000)
#'
#' @export
imumap_options <- function(
  max_distance = Inf,
  zero_pop_handling = c("distribute_evenly", "ignore", "error"),
  tie_breaking = c("split", "first"),
  sparse = TRUE
) {
  zero_pop_handling <- match.arg(zero_pop_handling)
  tie_breaking <- match.arg(tie_breaking)

  structure(
    list(
      max_distance = max_distance,
      zero_pop_handling = zero_pop_handling,
      tie_breaking = tie_breaking,
      sparse = isTRUE(sparse)
    ),
    class = "imumap_options"
  )
}

#' @title Print imuMAP Options Summary
#'
#' @description Prints a concise summary of an `imumap_options` configuration object.
#'
#' @param x An `imumap_options` object.
#' @param ... Additional arguments passed to print methods.
#'
#' @return The object `x`, invisibly.
#' @export
print.imumap_options <- function(x, ...) {
  cat(
    "<imumap_options>\n",
    "Max distance:      ",
    x$max_distance,
    "\n",
    "Zero pop handling: ",
    x$zero_pop_handling,
    "\n",
    "Sparse matrices:   ",
    x$sparse,
    "\n",
    sep = ""
  )
  invisible(x)
}
