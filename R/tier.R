#' @title Define an Allocation Tier
#'
#' @description Specifies source selection, institutional eligibility rules,
#'   distance decay kernel, and allocation method for a single stage within an
#'   iterative allocation pipeline.
#'
#' @param name Character. Unique identifier for the tier (e.g. `"public"`, `"private"`).
#' @param source_filter Named list, expression, logical vector, or function `f(source)`
#'   selecting the source facilities for this tier. Defaults to `NULL` (all sources).
#' @param eligibility An `imumap_eligibility_rule` or `imumap_eligibility_rules` object
#'   restricting candidate target units for this tier. Defaults to `NULL`.
#' @param kernel_options An `imumap_kernel_options` object configuring distance decay.
#'   Defaults to `kernel_options("gravity")`.
#' @param method Character specifying the allocation solver: `"gravity"` (default,
#'   distance-decay weighted by residual target capacity) or `"transport"`
#'   (optimal transport linear programming).
#' @param options Named list of solver-specific tuning options.
#'
#' @return An S3 object of class `imumap_allocation_tier`.
#'
#' @examples
#' # Public school tier restricted to same county
#' rule_pub <- eligibility_rule(match_cols = "county")
#' tier_pub <- allocation_tier(
#'   name = "public",
#'   source_filter = list(school_type = "Public"),
#'   eligibility = rule_pub,
#'   kernel_options = kernel_options("power", list(power = 2.0))
#' )
#'
#' # Private school tier using optimal transport
#' tier_priv <- allocation_tier(
#'   name = "private",
#'   source_filter = list(school_type = "Private"),
#'   method = "transport"
#' )
#'
#' @export
allocation_tier <- function(
  name,
  source_filter = NULL,
  eligibility = NULL,
  kernel_options = imuMAP::kernel_options("gravity"),
  method = c("gravity", "transport"),
  options = list()
) {
  stop_fmt_if(
    missing(name) || !is.character(name) || length(name) != 1 || nchar(name) == 0,
    ERR_TIER_MISSING_NAME
  )

  method <- match.arg(method)

  if (!is.null(eligibility)) {
    stop_fmt_if(
      !inherits(eligibility, c("imumap_eligibility_rule", "imumap_eligibility_rules")) &&
        !is.list(eligibility),
      ERR_INVALID_ELIGIBILITY,
      arg = "eligibility"
    )
  }

  stop_fmt_if(
    !inherits(kernel_options, "imumap_kernel_options") && !is.function(kernel_options),
    ERR_INVALID_KERNEL_OPTIONS,
    arg = "kernel_options"
  )

  structure(
    list(
      name = name,
      source_filter = source_filter,
      eligibility = eligibility,
      kernel_options = kernel_options,
      method = method,
      options = options
    ),
    class = "imumap_allocation_tier"
  )
}

#' @title Print Allocation Tier Summary
#'
#' @description Prints a concise summary of an `imumap_allocation_tier` object.
#'
#' @param x An `imumap_allocation_tier` object.
#' @param ... Additional arguments passed to print methods.
#'
#' @return The object `x`, invisibly.
#' @export
print.imumap_allocation_tier <- function(x, ...) {
  cat(
    "<imumap_allocation_tier: ", x$name, ">\n",
    "Source filter: ",
    if (is.null(x$source_filter)) {
      "<all>"
    } else if (is.list(x$source_filter)) {
      paste(names(x$source_filter), x$source_filter, sep = "=", collapse = ", ")
    } else {
      "<custom filter>"
    },
    "\n",
    "Method:        ", x$method, "\n",
    "Eligibility:   ",
    if (is.null(x$eligibility)) "<none>" else class(x$eligibility)[1], "\n",
    sep = ""
  )
  invisible(x)
}

#' @title Validate and Partition Sources Across Tiers
#'
#' @description Verifies that a list of tiers partitions source IDs without
#'   unassigned units or overlapping memberships.
#'
#' @param tiers List of `imumap_allocation_tier` objects.
#' @param source_data An `sf` or `data.frame` of source units containing column `id`.
#'
#' @return A named list where each element contains the source indices for that tier.
#' @keywords internal
#' @noRd
validate_tiers <- function(tiers, source_data) {
  stop_fmt_if(!is.list(tiers) || length(tiers) == 0, ERR_INVALID_TIER, arg = "tiers")

  tier_names <- character(length(tiers))
  for (i in seq_along(tiers)) {
    stop_fmt_if(
      !inherits(tiers[[i]], "imumap_allocation_tier"),
      ERR_INVALID_TIER,
      arg = paste0("tiers[[", i, "]]")
    )
    tier_names[i] <- tiers[[i]]$name
  }

  dups <- tier_names[duplicated(tier_names)]
  stop_fmt_if(length(dups) > 0, ERR_DUPLICATE_TIER_NAME, names = paste(unique(dups), collapse = ", "))

  all_ids <- as.character(source_data$id)
  n_src <- length(all_ids)
  assigned_tier <- rep(NA_character_, n_src)
  names(assigned_tier) <- all_ids
  tier_indices <- vector("list", length(tiers))
  names(tier_indices) <- tier_names

  for (i in seq_along(tiers)) {
    tr <- tiers[[i]]
    mask <- eval_filter(source_data, tr$source_filter)
    matched_idx <- which(mask)

    # Check for overlapping sources
    overlapping <- which(mask & !is.na(assigned_tier))
    if (length(overlapping) > 0) {
      stop_fmt_if(
        TRUE,
        ERR_OVERLAPPING_TIERS,
        id = all_ids[overlapping[1]]
      )
    }

    assigned_tier[matched_idx] <- tr$name
    tier_indices[[i]] <- matched_idx
  }

  unassigned <- which(is.na(assigned_tier))
  if (length(unassigned) > 0) {
    stop_fmt_if(
      TRUE,
      ERR_UNASSIGNED_SOURCES,
      count = length(unassigned)
    )
  }

  tier_indices
}
