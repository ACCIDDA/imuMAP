#' @title Calculate Spatial Allocation Weights Matrix
#'
#' @description Combines pairwise spatial distances, target population counts,
#'   and distance decay kernels into an allocation weights matrix mapping sources
#'   to targets.
#'
#' @param dist_matrix An `imumap_dist_matrix` object created by `calc_distance_matrix()`.
#' @param target_pop Optional `data.frame` or `imumap_demographics` object containing
#'   target demographic population counts (must include `id` and `population`).
#' @param kernel_options An `imumap_kernel_options` object, or a list of
#'   `imumap_kernel_options` objects. Defaults to `kernel_options("gravity")`.
#' @param normalize Logical. If `TRUE`, normalizes weights so each source row
#'   sums to 1. Defaults to `TRUE`.
#' @param sparse Logical. If `TRUE`, stores weights as a sparse `dgCMatrix`.
#'   Defaults to `TRUE`.
#'
#' @return An S3 object of class `imumap_weights`.
#'
#' @examples
#' pts <- sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), crs = 3857)
#' src <- sf::st_sf(id = c("s1", "s2"), geometry = pts)
#' tgt_pts <- sf::st_sfc(sf::st_point(c(0, 0.5)), sf::st_point(c(1, 1.5)), crs = 3857)
#' tgt <- sf::st_sf(id = c("c1", "c2"), geometry = tgt_pts)
#' dmat <- calc_distance_matrix(src, tgt)
#' w <- calc_allocation_weights(dmat)
#'
#' @importFrom stats setNames
#' @export
calc_allocation_weights <- function(
  dist_matrix,
  target_pop = NULL,
  kernel_options = imuMAP::kernel_options("gravity"),
  normalize = TRUE,
  sparse = TRUE
) {
  stop_fmt_if(
    !inherits(dist_matrix, "imumap_dist_matrix"),
    ERR_INVALID_DIST_MATRIX,
    arg = "dist_matrix"
  )

  d <- dist_matrix$matrix
  src_ids <- dist_matrix$source_ids
  tgt_ids <- dist_matrix$target_ids

  # Compute decay weights
  if (inherits(kernel_options, "imumap_kernel_options")) {
    w <- apply_kernel(d, kernel_options)
  } else if (is.list(kernel_options)) {
    # Heterogeneous kernel list per source type / id
    w <- matrix(0, nrow = nrow(d), ncol = ncol(d), dimnames = dimnames(d))
    for (nm in names(kernel_options)) {
      matching_rows <- which(rownames(d) == nm | grepl(nm, rownames(d)))
      if (length(matching_rows) > 0) {
        k_opt <- kernel_options[[nm]]
        w[matching_rows, ] <- apply_kernel(
          d[matching_rows, , drop = FALSE],
          k_opt
        )
      }
    }
  } else {
    stop_fmt_if(TRUE, ERR_INVALID_KERNEL_OPTIONS, arg = "kernel_options")
  }

  # Apply population weighting
  if (!is.null(target_pop)) {
    pop_dt <- canonicalize_demographics(target_pop)
    pop_vec <- setNames(pop_dt$population, pop_dt$id)
    matched_pops <- pop_vec[tgt_ids]
    matched_pops[is.na(matched_pops)] <- 0

    # Multiply each column j by pop_j
    w <- sweep(w, 2, matched_pops, "*")
  }

  # Normalize each source row to sum to 1
  if (isTRUE(normalize)) {
    row_sums <- rowSums(w)
    row_sums[row_sums == 0] <- 1 # Prevent division by zero
    w <- w / row_sums
  }

  if (isTRUE(sparse)) {
    w_mat <- Matrix::Matrix(w, sparse = TRUE)
  } else {
    w_mat <- w
  }

  structure(
    list(
      weights = w_mat,
      source_ids = src_ids,
      target_ids = tgt_ids,
      normalized = normalize,
      kernel_options = kernel_options
    ),
    class = "imumap_weights"
  )
}

#' @title Print Allocation Weights Summary
#'
#' @description Prints a concise summary of an `imumap_weights` object.
#'
#' @param x An `imumap_weights` object.
#' @param ... Additional arguments passed to print methods.
#'
#' @return The object `x`, invisibly.
#' @export
print.imumap_weights <- function(x, ...) {
  cat(
    "<imumap_weights>\n",
    "Sources:    ",
    length(x$source_ids),
    "\n",
    "Targets:    ",
    length(x$target_ids),
    "\n",
    "Normalized: ",
    x$normalized,
    "\n",
    "Format:     ",
    class(x$weights)[1],
    "\n",
    sep = ""
  )
  invisible(x)
}

#' @title Dimensions of Allocation Weights
#'
#' @description Returns the dimensions of the underlying spatial allocation weights matrix.
#'
#' @param x An `imumap_weights` object.
#' @return Integer vector of length 2.
#' @export
dim.imumap_weights <- function(x) {
  dim(x$weights)
}
