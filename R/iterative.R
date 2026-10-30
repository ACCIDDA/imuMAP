#' @title Compute Multi-Tier Iterative Allocation Weights
#'
#' @description Iteratively allocates source populations to target geographic
#'   units across sequential tiers (e.g. district-bounded public schools
#'   followed by kernel-based private schools allocating against residual capacity).
#'
#' @param dist_matrix An `imumap_dist_matrix` object created by `calc_distance_matrix()`.
#' @param source_pop A `data.frame`, `data.table`, or named numeric vector containing
#'   source population counts (must contain `id` and `population`, or `total`).
#' @param target_pop A `data.frame` or `imumap_demographics` object containing target
#'   capacity counts (must contain `id` and `population`).
#' @param tiers A list of `imumap_allocation_tier` objects in sequential order of execution.
#' @param source_data Optional `data.frame` or `sf` object containing source metadata
#'   used for filtering tiers. If `NULL`, metadata is extracted from `source_pop`.
#' @param oversubscription Character strategy for targets where allocated demand exceeds
#'   remaining capacity: `"clamp"` (truncate residual capacity at 0, default),
#'   `"proportional"` (scale down flows into saturated targets), or `"warn"`.
#' @param sparse Logical. If `TRUE`, returns composite weights as a sparse `dgCMatrix`.
#'   Default is `TRUE`.
#'
#' @return An S3 object of class `c("imumap_iterative_weights", "imumap_weights")`
#'   containing:
#'   \describe{
#'     \item{weights}{Composite $M \times N$ allocation weights matrix (rows sum to 1.0).}
#'     \item{source_ids}{Character vector of source identifiers.}
#'     \item{target_ids}{Character vector of target identifiers.}
#'     \item{residual_capacity}{Named numeric vector of final unallocated target population.}
#'     \item{tier_results}{Named list of allocated flows and weights per tier.}
#'     \item{diagnostics}{Summary data.table of capacity utilization and oversubscription.}
#'   }
#'
#' @examples
#' pts <- sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), crs = 3857)
#' src <- sf::st_sf(id = c("s1", "s2"), school_type = c("Public", "Private"), geometry = pts)
#' tgt_pts <- sf::st_sfc(sf::st_point(c(0, 0.5)), sf::st_point(c(1, 1.5)), crs = 3857)
#' tgt <- sf::st_sf(id = c("c1", "c2"), geometry = tgt_pts)
#' dmat <- calc_distance_matrix(src, tgt)
#'
#' src_pop <- data.frame(id = c("s1", "s2"), population = c(100, 50))
#' tgt_pop <- data.frame(id = c("c1", "c2"), population = c(120, 80))
#'
#' t1 <- allocation_tier("pub", source_filter = list(school_type = "Public"))
#' t2 <- allocation_tier("priv", source_filter = list(school_type = "Private"))
#'
#' res <- calc_iterative_weights(dmat, src_pop, tgt_pop, list(t1, t2), source_data = src)
#'
#' @importFrom stats setNames
#' @export
calc_iterative_weights <- function(
  dist_matrix,
  source_pop,
  target_pop,
  tiers,
  source_data = NULL,
  oversubscription = c("clamp", "proportional", "warn"),
  sparse = TRUE
) {
  stop_fmt_if(
    !inherits(dist_matrix, "imumap_dist_matrix"),
    ERR_INVALID_DIST_MATRIX,
    arg = "dist_matrix"
  )

  oversubscription <- match.arg(oversubscription)
  d_mat <- dist_matrix$matrix
  src_ids <- dist_matrix$source_ids
  tgt_ids <- dist_matrix$target_ids
  n_src <- length(src_ids)
  n_tgt <- length(tgt_ids)

  # Canonicalize source population
  if (is.numeric(source_pop) && !is.null(names(source_pop))) {
    src_pop_vec <- source_pop[src_ids]
  } else {
    stop_fmt_if(!is.data.frame(source_pop), ERR_MISSING_COLUMN, col = "data.frame", name = "source_pop")
    sp_dt <- data.table::as.data.table(source_pop)
    stop_fmt_if(!"id" %in% names(sp_dt), ERR_MISSING_COLUMN, col = "id", name = "source_pop")
    pop_col <- if ("population" %in% names(sp_dt)) "population" else if ("total" %in% names(sp_dt)) "total" else NULL
    stop_fmt_if(is.null(pop_col), ERR_MISSING_COLUMN, col = "population or total", name = "source_pop")
    src_pop_vec <- setNames(as.numeric(sp_dt[[pop_col]]), as.character(sp_dt$id))[src_ids]
  }
  src_pop_vec[is.na(src_pop_vec)] <- 0

  # Canonicalize target population
  tgt_dt <- canonicalize_demographics(target_pop)
  tgt_pop_vec <- setNames(tgt_dt$population, tgt_dt$id)[tgt_ids]
  tgt_pop_vec[is.na(tgt_pop_vec)] <- 0

  # Source metadata for filtering tiers
  src_meta <- if (!is.null(source_data)) {
    data.table::as.data.table(source_data)
  } else if (is.data.frame(source_pop)) {
    data.table::as.data.table(source_pop)
  } else {
    data.table::data.table(id = src_ids)
  }
  src_meta$id <- as.character(src_meta$id)

  # Validate and partition sources across tiers
  tier_indices <- validate_tiers(tiers, src_meta)

  # Initialize composite weights matrix and residual capacity
  W_composite <- matrix(0, nrow = n_src, ncol = n_tgt, dimnames = list(src_ids, tgt_ids))
  current_target_capacity <- tgt_pop_vec
  tier_results <- vector("list", length(tiers))
  names(tier_results) <- names(tier_indices)
  oversub_records <- list()

  for (i in seq_along(tiers)) {
    tr <- tiers[[i]]
    tr_name <- tr$name
    idx <- tier_indices[[tr_name]]

    if (length(idx) == 0) {
      next
    }

    sub_dist <- d_mat[idx, , drop = FALSE]
    sub_src_pop <- src_pop_vec[idx]

    if (identical(tr$method, "transport")) {
      tier_alloc <- solve_transport_tier(
        sub_dist = sub_dist,
        source_pop = sub_src_pop,
        target_capacity = current_target_capacity
      )
    } else {
      # Native gravity / distribution decay residual solver
      tier_alloc <- solve_gravity_tier(
        sub_dist = sub_dist,
        source_pop = sub_src_pop,
        target_capacity = current_target_capacity,
        kernel_options = tr$kernel_options
      )
    }

    flow_tier <- tier_alloc$flow
    W_tier <- tier_alloc$weights

    # Total allocated to each target in this tier
    tgt_allocated <- colSums(flow_tier)

    # Check for oversubscription where demand exceeds available capacity
    oversub_idx <- which(tgt_allocated > current_target_capacity & current_target_capacity > 0)
    if (length(oversub_idx) > 0) {
      oversub_records[[tr_name]] <- data.table::data.table(
        tier = tr_name,
        target_id = tgt_ids[oversub_idx],
        demanded = tgt_allocated[oversub_idx],
        available = current_target_capacity[oversub_idx]
      )

      if (identical(oversubscription, "warn")) {
        warn_fmt_if(
          TRUE,
          "tier '{name}' oversubscribed target capacity in {count} units",
          name = tr_name,
          count = length(oversub_idx)
        )
      } else if (identical(oversubscription, "proportional")) {
        scale_fac <- current_target_capacity[oversub_idx] / tgt_allocated[oversub_idx]
        flow_tier[, oversub_idx] <- sweep(
          flow_tier[, oversub_idx, drop = FALSE],
          2,
          scale_fac,
          "*"
        )
        row_s <- rowSums(flow_tier)
        row_s[row_s == 0] <- 1
        W_tier <- flow_tier / row_s
        tgt_allocated <- colSums(flow_tier)
      }
    }

    # Decrement residual capacity (clamped at 0)
    current_target_capacity <- stats::setNames(
      pmax(current_target_capacity - tgt_allocated, 0),
      tgt_ids
    )

    # Place tier weights into composite matrix
    W_composite[idx, ] <- W_tier

    tier_results[[tr_name]] <- list(
      weights = W_tier,
      flow = flow_tier,
      allocated_to_targets = tgt_allocated,
      source_pop = sub_src_pop
    )
  }

  if (isTRUE(sparse)) {
    W_out <- Matrix::Matrix(W_composite, sparse = TRUE)
  } else {
    W_out <- W_composite
  }

  diag_dt <- data.table::data.table(
    target_id = tgt_ids,
    initial_capacity = tgt_pop_vec,
    residual_capacity = current_target_capacity,
    allocated_total = tgt_pop_vec - current_target_capacity
  )

  structure(
    list(
      weights = W_out,
      source_ids = src_ids,
      target_ids = tgt_ids,
      normalized = TRUE,
      tiers = tiers,
      tier_results = tier_results,
      residual_capacity = current_target_capacity,
      diagnostics = diag_dt,
      oversubscription = if (length(oversub_records) > 0) data.table::rbindlist(oversub_records) else NULL
    ),
    class = c("imumap_iterative_weights", "imumap_weights")
  )
}

#' @title Solve Single Tier Allocation via Gravity Distance Decay
#'
#' @description Computes allocation weights and flows using distance decay weighted
#'   by current residual target capacity.
#'
#' @param sub_dist Numeric matrix of distances for tier sources.
#' @param source_pop Numeric vector of source populations for tier.
#' @param target_capacity Numeric vector of current available target capacities.
#' @param kernel_options An `imumap_kernel_options` object.
#'
#' @return List containing `$weights` and `$flow`.
#' @keywords internal
#' @noRd
solve_gravity_tier <- function(sub_dist, source_pop, target_capacity, kernel_options) {
  raw_w <- apply_kernel(sub_dist, kernel_options)
  # Weight by available residual capacity
  eff_cap <- pmax(target_capacity, 0)
  weighted_w <- sweep(raw_w, 2, eff_cap, "*")

  row_s <- rowSums(weighted_w)
  row_s[row_s == 0] <- 1
  W <- weighted_w / row_s
  flow <- sweep(W, 1, source_pop, "*")

  list(weights = W, flow = flow)
}

#' @title Solve Single Tier Allocation via Optimal Transport
#'
#' @description Computes allocation weights and flows using linear programming
#'   mass transport via the `transport` package.
#'
#' @param sub_dist Numeric matrix of distances for tier sources.
#' @param source_pop Numeric vector of source populations for tier.
#' @param target_capacity Numeric vector of current available target capacities.
#'
#' @return List containing `$weights` and `$flow`.
#' @keywords internal
#' @noRd
solve_transport_tier <- function(sub_dist, source_pop, target_capacity) {
  stop_fmt_if(
    !requireNamespace("transport", quietly = TRUE),
    ERR_SOLVER_UNAVAILABLE,
    pkg = "transport",
    method = "transport"
  )

  n_s <- nrow(sub_dist)
  n_t <- ncol(sub_dist)
  tot_supply <- sum(source_pop)
  eff_cap <- pmax(target_capacity, 0)
  tot_cap <- sum(eff_cap)

  # Check trivial cases
  if (tot_supply == 0 || tot_cap == 0) {
    W <- matrix(1 / n_t, nrow = n_s, ncol = n_t)
    flow <- matrix(0, nrow = n_s, ncol = n_t)
    return(list(weights = W, flow = flow))
  }

  cost_mat <- sub_dist
  finite_dists <- cost_mat[is.finite(cost_mat)]
  max_finite <- if (length(finite_dists) > 0) max(finite_dists) else 100000
  cost_mat[!is.finite(cost_mat)] <- max_finite * 10

  if (tot_supply <= tot_cap) {
    # Sources underfill target capacity: add virtual slack source
    slack_supply <- tot_cap - tot_supply
    supplies <- c(source_pop, slack_supply)
    costs <- rbind(cost_mat, rep(0, n_t))

    res <- transport::transport(
      a = supplies,
      b = eff_cap,
      cost = costs,
      method = "revsimplex"
    )

    flow <- matrix(0, nrow = n_s, ncol = n_t)
    for (row_i in seq_len(nrow(res))) {
      from_idx <- res$from[row_i]
      to_idx <- res$to[row_i]
      mass <- res$mass[row_i]
      if (from_idx <= n_s) {
        flow[from_idx, to_idx] <- mass
      }
    }
  } else {
    # Supply exceeds capacity: add virtual slack target
    slack_demand <- tot_supply - tot_cap
    demands <- c(eff_cap, slack_demand)
    costs <- cbind(cost_mat, rep(max_finite * 20, n_s))

    res <- transport::transport(
      a = source_pop,
      b = demands,
      cost = costs,
      method = "revsimplex"
    )

    flow <- matrix(0, nrow = n_s, ncol = n_t)
    for (row_i in seq_len(nrow(res))) {
      from_idx <- res$from[row_i]
      to_idx <- res$to[row_i]
      mass <- res$mass[row_i]
      if (to_idx <= n_t) {
        flow[from_idx, to_idx] <- mass
      }
    }
  }

  row_s <- rowSums(flow)
  row_s[row_s == 0] <- 1
  W <- flow / row_s

  list(weights = W, flow = flow)
}

#' @title Print Iterative Allocation Weights Summary
#'
#' @description Prints a concise summary of an `imumap_iterative_weights` object.
#'
#' @param x An `imumap_iterative_weights` object.
#' @param ... Additional arguments passed to print methods.
#'
#' @return The object `x`, invisibly.
#' @export
print.imumap_iterative_weights <- function(x, ...) {
  cat(
    "<imumap_iterative_weights>\n",
    "Sources:             ", length(x$source_ids), "\n",
    "Targets:             ", length(x$target_ids), "\n",
    "Tiers:               ", paste(names(x$tier_results), collapse = " -> "), "\n",
    "Residual capacity:   ", sum(x$residual_capacity), "\n",
    "Format:              ", class(x$weights)[1], "\n",
    sep = ""
  )
  invisible(x)
}

#' @title Summary Method for Iterative Allocation Weights
#'
#' @description Summarizes per-tier allocation statistics and target capacity utilization.
#'
#' @param object An `imumap_iterative_weights` object.
#' @param ... Additional arguments passed to summary methods.
#'
#' @return An object of class `summary_imumap_iterative_weights`.
#' @export
summary.imumap_iterative_weights <- function(object, ...) {
  tier_rows <- lapply(names(object$tier_results), function(nm) {
    tr <- object$tier_results[[nm]]
    data.table::data.table(
      tier = nm,
      sources = nrow(tr$weights),
      source_pop = sum(tr$source_pop),
      allocated = sum(tr$allocated_to_targets)
    )
  })
  tier_summary <- data.table::rbindlist(tier_rows)

  structure(
    list(
      tier_summary = tier_summary,
      diagnostics = object$diagnostics,
      oversubscription = object$oversubscription,
      total_sources = length(object$source_ids),
      total_targets = length(object$target_ids),
      initial_capacity = sum(object$diagnostics$initial_capacity),
      residual_capacity = sum(object$residual_capacity)
    ),
    class = "summary_imumap_iterative_weights"
  )
}

#' @title Print Summary of Iterative Weights
#'
#' @description Prints the tabulated summary of an `imumap_iterative_weights` object.
#'
#' @param x A `summary_imumap_iterative_weights` object.
#' @param ... Additional arguments passed to print methods.
#'
#' @return The object `x`, invisibly.
#' @export
print.summary_imumap_iterative_weights <- function(x, ...) {
  cat("=== imuMAP Multi-Tier Allocation Summary ===\n\n")
  cat("Tier Breakdown:\n")
  print(x$tier_summary)
  cat("\nTarget Capacity Utilization:\n")
  cat("  Initial Capacity:  ", x$initial_capacity, "\n")
  cat("  Allocated Total:   ", x$initial_capacity - x$residual_capacity, "\n")
  cat("  Residual Capacity: ", x$residual_capacity, "\n")
  if (!is.null(x$oversubscription) && nrow(x$oversubscription) > 0) {
    cat("\nOversubscription Alerts (", nrow(x$oversubscription), " incidents):\n")
    print(head(x$oversubscription, 5))
  }
  invisible(x)
}
