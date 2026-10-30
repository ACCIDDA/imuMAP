#' @title Compute Pairwise Distance Matrix with Optional Eligibility Filtering
#'
#' @description Calculates pairwise spatial Euclidean or ellipsoidal distances
#'   between canonicalized source and target geometries. If geometries are
#'   polygons, centroids are computed automatically.
#'
#' @param source An `sf` object of source geometries with required column `id`.
#' @param target An `sf` object of target geometries with required column `id`.
#' @param max_distance Numeric. Global maximum threshold cutoff distance (in CRS units).
#'   Distances greater than this threshold are set to `Inf`. Defaults to `Inf`.
#' @param eligibility Optional eligibility specification to partition and restrict
#'   how source locations map to targets. Accepts an `imumap_eligibility_rule`,
#'   an `imumap_eligibility_rules` rule set, a list of rules, a custom predicate
#'   function `f(source, target)`, or a pre-computed logical matrix.
#'
#' @return An S3 object of class `imumap_dist_matrix` containing the pairwise
#'   distance matrix, source IDs, and target IDs.
#'
#' @examples
#' pts <- sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), crs = 3857)
#' src <- sf::st_sf(id = c("s1", "s2"), school_type = c("pub", "priv"), geometry = pts)
#' tgt_pts <- sf::st_sfc(sf::st_point(c(0, 0.5)), sf::st_point(c(1, 1.5)), crs = 3857)
#' tgt <- sf::st_sf(id = c("c1", "c2"), geometry = tgt_pts)
#' # Rule-based eligibility: limit public schools to 500m
#' r_pub <- eligibility_rule(source_filter = list(school_type = "pub"), max_distance = 500)
#' dmat <- calc_distance_matrix(src, tgt, eligibility = r_pub)
#'
#' @export
calc_distance_matrix <- function(
  source,
  target,
  max_distance = Inf,
  eligibility = NULL
) {
  canon <- canonicalize_spatial(source, target)
  src <- canon$source
  tgt <- canon$target

  # Convert polygons to centroids if needed
  src_geom <- sf::st_geometry(src)
  if (any(sf::st_geometry_type(src_geom) %in% c("POLYGON", "MULTIPOLYGON"))) {
    src_geom <- sf::st_centroid(src_geom)
  }

  tgt_geom <- sf::st_geometry(tgt)
  if (any(sf::st_geometry_type(tgt_geom) %in% c("POLYGON", "MULTIPOLYGON"))) {
    tgt_geom <- sf::st_centroid(tgt_geom)
  }

  dist_raw <- sf::st_distance(src_geom, tgt_geom)
  dist_mat <- matrix(
    as.numeric(dist_raw),
    nrow = nrow(src),
    ncol = nrow(tgt),
    dimnames = list(src$id, tgt$id)
  )

  if (is.finite(max_distance)) {
    dist_mat[dist_mat > max_distance] <- Inf
  }

  if (!is.null(eligibility)) {
    dist_mat <- apply_eligibility(dist_mat, src, tgt, eligibility)
  }

  structure(
    list(
      matrix = dist_mat,
      source_ids = src$id,
      target_ids = tgt$id,
      max_distance = max_distance
    ),
    class = "imumap_dist_matrix"
  )
}

#' @title Print Distance Matrix Summary
#'
#' @description Prints a concise summary of an `imumap_dist_matrix` object.
#'
#' @param x An `imumap_dist_matrix` object.
#' @param ... Additional arguments passed to print methods.
#'
#' @return The object `x`, invisibly.
#' @export
print.imumap_dist_matrix <- function(x, ...) {
  cat(
    "<imumap_dist_matrix>\n",
    "Sources: ",
    length(x$source_ids),
    "\n",
    "Targets: ",
    length(x$target_ids),
    "\n",
    "Max dist cutoff: ",
    x$max_distance,
    "\n",
    sep = ""
  )
  invisible(x)
}

#' @title Dimension of Distance Matrix
#'
#' @description Returns the dimensions of the underlying pairwise distance matrix.
#'
#' @param x An `imumap_dist_matrix` object.
#' @return Integer vector of length 2 giving dimensions.
#' @export
dim.imumap_dist_matrix <- function(x) {
  dim(x$matrix)
}
