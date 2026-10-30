# Centralized Error & Warning Format Constants

ERR_INVALID_SF <- "argument `{arg}` must be an 'sf' object"
ERR_MISSING_COLUMN <- "expected column '{col}' in '{name}'"
ERR_EMPTY_GEOMETRY <- "spatial layer '{name}' contains empty or NA geometries"
ERR_DUPLICATE_ID <- "duplicate IDs found in '{name}'"
ERR_INVALID_CRS <- "spatial layer '{name}' must have a valid CRS"
ERR_INVALID_DIST_MATRIX <- "argument `{arg}` must be an 'imumap_dist_matrix' object"
ERR_INVALID_KERNEL <- "unknown decay kernel '{kernel}'; must be one of: {valid}"
ERR_INVALID_KERNEL_OPTIONS <- "argument `{arg}` must be an 'imumap_kernel_options' object"
ERR_INVALID_WEIGHTS <- "argument `{arg}` must be an 'imumap_weights' object"
ERR_NON_NUMERIC_POP <- "target demographic population must be numeric and non-negative"
ERR_MISMATCH_TARGET_IDS <- "target demographic IDs do not match distance matrix target IDs"
ERR_INVALID_ALIGNMENT <- "unknown age alignment method '{method}'"
ERR_MISSING_COVERAGE_COL <- "expected column '{col}' in coverage data"
ERR_TIER_MISSING_NAME <- "allocation tier must have a non-empty 'name'"
ERR_INVALID_TIER <- "argument `{arg}` must be an 'imumap_allocation_tier' object"
ERR_DUPLICATE_TIER_NAME <- "duplicate tier names found: {names}"
ERR_UNASSIGNED_SOURCES <- "{count} source locations were not assigned to any tier"
ERR_OVERLAPPING_TIERS <- "source '{id}' was assigned to multiple tiers"
ERR_SOLVER_UNAVAILABLE <- "package '{pkg}' is required for method '{method}'"

#' @title Format String with Named Variables
#'
#' @description Substitutes named variable placeholders in message templates.
#'
#' @param template Character string with `{var}` placeholders.
#' @param ... Named arguments to substitute into `template`.
#' @return Formatted character scalar.
#' @keywords internal
#' @noRd
fmt_msg <- function(template, ...) {
  args <- list(...)
  res <- template
  for (nm in names(args)) {
    val <- as.character(args[[nm]])
    res <- gsub(paste0("\\{", nm, "\\}"), val, res)
  }
  res
}

#' @title Signal Formatted Error If Condition is Met
#'
#' @description Throws a structured error with formatted message if `cond` is `TRUE`.
#'
#' @param cond Logical scalar condition to check.
#' @param template Error message template with `{var}` placeholders.
#' @param ... Named values for placeholders.
#' @param class Custom error class.
#' @keywords internal
#' @noRd
stop_fmt_if <- function(cond, template, ..., class = "imumap_error") {
  if (isTRUE(cond)) {
    msg <- fmt_msg(template, ...)
    err <- structure(
      list(message = msg, call = NULL),
      class = unique(c(class, "imumap_error", "error", "condition"))
    )
    stop(err)
  }
}

#' @title Signal Formatted Warning If Condition is Met
#'
#' @description Signals a structured warning with formatted message if `cond` is `TRUE`.
#'
#' @param cond Logical scalar condition to check.
#' @param template Warning message template with `{var}` placeholders.
#' @param ... Named values for placeholders.
#' @param class Custom warning class.
#' @keywords internal
#' @noRd
warn_fmt_if <- function(cond, template, ..., class = "imumap_warning") {
  if (isTRUE(cond)) {
    msg <- fmt_msg(template, ...)
    w <- structure(
      list(message = msg, call = NULL),
      class = unique(c(class, "imumap_warning", "warning", "condition"))
    )
    warning(w)
  }
}
