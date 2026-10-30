ERR_INVALID_ELIGIBILITY <- paste(
  "argument `{arg}` must be an eligibility rule, list of rules,",
  "function, or logical matrix"
)
ERR_MISSING_MATCH_COL <- "expected match column '{col}' in '{name}'"

#' @title Define Spatial Eligibility Rule
#'
#' @description Constructs an eligibility rule partitioning how sources map
#'   to targets based on spatial or metadata attributes (e.g. school type,
#'   district containment, or subset-specific distance cutoffs).
#'
#' @param source_filter Named list, character expression, logical vector, or
#'   function `f(source)` selecting the source subset this rule applies to.
#'   For example: `list(school_type = "public")`. Defaults to `NULL` (all sources).
#' @param match_cols Character vector of metadata column names that must match
#'   identically between source and target units (e.g. `"district_id"`).
#' @param max_distance Numeric. Maximum distance cutoff specific to this rule.
#'   Defaults to `Inf`.
#' @param target_filter Optional named list, logical vector, or function `f(target)`
#'   restricting eligible target units.
#' @param predicate Optional function `f(source_sub, target_sub)` returning a
#'   logical matrix for matching source and target subsets.
#'
#' @return An S3 object of class `imumap_eligibility_rule`.
#'
#' @examples
#' # Rule: public schools only map to block groups in the same school district
#' rule_pub <- eligibility_rule(
#'   source_filter = list(school_type = "public"),
#'   match_cols = "district_id"
#' )
#'
#' # Rule: private schools map within a 30km radius without district constraints
#' rule_priv <- eligibility_rule(
#'   source_filter = list(school_type = "private"),
#'   max_distance = 30000
#' )
#'
#' @export
eligibility_rule <- function(
  source_filter = NULL,
  match_cols = NULL,
  max_distance = Inf,
  target_filter = NULL,
  predicate = NULL
) {
  structure(
    list(
      source_filter = source_filter,
      match_cols = match_cols,
      max_distance = max_distance,
      target_filter = target_filter,
      predicate = predicate
    ),
    class = "imumap_eligibility_rule"
  )
}

#' @title Bundle Multiple Eligibility Rules
#'
#' @description Combines multiple `imumap_eligibility_rule` objects into a single
#'   rule set.
#'
#' @param ... One or more `imumap_eligibility_rule` objects.
#'
#' @return An S3 object of class `imumap_eligibility_rules`.
#'
#' @examples
#' r1 <- eligibility_rule(list(type = "pub"), match_cols = "district_id")
#' r2 <- eligibility_rule(list(type = "priv"), max_distance = 25000)
#' rules <- eligibility_rules(r1, r2)
#'
#' @export
eligibility_rules <- function(...) {
  rules <- list(...)
  for (i in seq_along(rules)) {
    stop_fmt_if(
      !inherits(rules[[i]], "imumap_eligibility_rule"),
      ERR_INVALID_ELIGIBILITY,
      arg = paste0("rule_", i)
    )
  }
  structure(rules, class = "imumap_eligibility_rules")
}

#' @title Print Eligibility Rule Summary
#'
#' @description Prints a concise summary of an `imumap_eligibility_rule` object.
#'
#' @param x An `imumap_eligibility_rule` object.
#' @param ... Additional arguments passed to print methods.
#'
#' @return The object `x`, invisibly.
#' @export
print.imumap_eligibility_rule <- function(x, ...) {
  cat(
    "<imumap_eligibility_rule>\n",
    "Source filter: ",
    if (is.null(x$source_filter)) {
      "<all>"
    } else {
      paste(names(x$source_filter), x$source_filter, sep = "=", collapse = ", ")
    },
    "\n",
    "Match cols:    ",
    if (is.null(x$match_cols)) {
      "<none>"
    } else {
      paste(x$match_cols, collapse = ", ")
    },
    "\n",
    "Max distance:  ",
    x$max_distance,
    "\n",
    sep = ""
  )
  invisible(x)
}

#' @title Helper to Evaluate Source or Sink Filters
#'
#' @description Evaluates a metadata filter list or function on an `sf` object.
#'
#' @param data An `sf` data.frame.
#' @param filter Filter criteria.
#' @return Logical vector of matching rows.
#' @keywords internal
#' @noRd
eval_filter <- function(data, filter) {
  if (is.null(filter)) {
    return(rep(TRUE, nrow(data)))
  }
  if (is.function(filter)) {
    return(as.logical(filter(data)))
  }
  if (is.logical(filter) && length(filter) == nrow(data)) {
    return(filter)
  }
  if (is.list(filter)) {
    match_vec <- rep(TRUE, nrow(data))
    for (col in names(filter)) {
      if (col %in% names(data)) {
        val <- filter[[col]]
        match_vec <- match_vec & (data[[col]] %in% val)
      }
    }
    return(match_vec)
  }
  rep(TRUE, nrow(data))
}

#' @title Apply Eligibility Constraints to Distance Matrix
#'
#' @description Applies single or multiple eligibility rules to filter distance matrices.
#'
#' @param dist_mat Numeric matrix of distances.
#' @param source Canonicalized `sf` object of sources.
#' @param target Canonicalized `sf` object of targets.
#' @param eligibility Eligibility rule, rules list, matrix, or function.
#' @return Modified numeric matrix of distances with ineligible entries set to `Inf`.
#' @keywords internal
#' @noRd
apply_eligibility <- function(dist_mat, source, target, eligibility) {
  if (is.null(eligibility)) {
    return(dist_mat)
  }

  if (is.matrix(eligibility)) {
    dist_mat[!eligibility] <- Inf
    return(dist_mat)
  }

  if (is.function(eligibility)) {
    elig_mat <- eligibility(source, target)
    dist_mat[!elig_mat] <- Inf
    return(dist_mat)
  }

  rules <- list()
  if (inherits(eligibility, "imumap_eligibility_rule")) {
    rules <- list(eligibility)
  } else if (
    inherits(eligibility, "imumap_eligibility_rules") || is.list(eligibility)
  ) {
    rules <- eligibility
  } else {
    stop_fmt_if(TRUE, ERR_INVALID_ELIGIBILITY, arg = "eligibility")
  }

  for (rule in rules) {
    if (!inherits(rule, "imumap_eligibility_rule")) {
      next
    }

    src_mask <- eval_filter(source, rule$source_filter)
    tgt_mask <- eval_filter(target, rule$target_filter)

    src_idx <- which(src_mask)
    if (length(src_idx) == 0) {
      next
    }

    # Restrict targets by target_filter
    if (!all(tgt_mask)) {
      dist_mat[src_idx, !tgt_mask] <- Inf
    }

    # Match columns constraint (e.g. district_id)
    if (!is.null(rule$match_cols)) {
      for (col in rule$match_cols) {
        stop_fmt_if(
          !col %in% names(source),
          ERR_MISSING_MATCH_COL,
          col = col,
          name = "source"
        )
        stop_fmt_if(
          !col %in% names(target),
          ERR_MISSING_MATCH_COL,
          col = col,
          name = "target"
        )

        src_vals <- source[[col]][src_idx]
        tgt_vals <- target[[col]]

        # Outer comparison matrix for matching attribute values
        match_eq <- outer(src_vals, tgt_vals, `==`)
        match_eq[is.na(match_eq)] <- FALSE

        dist_mat[src_idx, ][!match_eq] <- Inf
      }
    }

    # Rule-specific max_distance
    if (is.finite(rule$max_distance)) {
      sub_d <- dist_mat[src_idx, , drop = FALSE]
      sub_d[sub_d > rule$max_distance] <- Inf
      dist_mat[src_idx, ] <- sub_d
    }

    # Custom predicate
    if (is.function(rule$predicate)) {
      pred_mat <- rule$predicate(source[src_idx, ], target)
      dist_mat[src_idx, ][!pred_mat] <- Inf
    }
  }

  dist_mat
}
