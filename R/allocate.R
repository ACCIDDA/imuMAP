#' @title Re-project / Allocate Coverage Estimates Across Spatial Units
#'
#' @description Projects source-level coverage data (or Bayesian posterior
#'   draws) across target spatial units (sinks) using pre-computed allocation
#'   weights.
#'
#' @param coverage_data A `data.frame` or `data.table` containing source-level
#'   observations with column `id` (matching `weights` source IDs) and numeric
#'   columns such as `vaccinated`, `total` (or `coverage`).
#' @param weights An `imumap_weights` object created by `calc_allocation_weights()`.
#' @param age_alignment Character specifying age harmonization method (`"uniform"`,
#'   `"proportional"`, or `"exact"`). Default is `"uniform"`.
#' @param draws_col Optional character naming the column representing posterior draws.
#' @param options An `imumap_options` object.
#'
#' @return A tidy `data.table` with allocated estimates by sink `id`.
#'
#' @examples
#' pts <- sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), crs = 3857)
#' src <- sf::st_sf(id = c("s1", "s2"), geometry = pts)
#' snk_pts <- sf::st_sfc(sf::st_point(c(0, 0.5)), sf::st_point(c(1, 1.5)), crs = 3857)
#' snk <- sf::st_sf(id = c("c1", "c2"), geometry = snk_pts)
#' dmat <- calc_distance_matrix(src, snk)
#' w <- calc_allocation_weights(dmat)
#' cov_data <- data.frame(id = c("s1", "s2"), vaccinated = c(80, 95), total = c(100, 100))
#' res <- allocate(cov_data, w)
#'
#' @importFrom stats setNames
#' @importFrom data.table as.data.table data.table rbindlist
#' @export
allocate <- function(
  coverage_data,
  weights,
  age_alignment = c("uniform", "proportional", "exact"),
  draws_col = NULL,
  options = imuMAP::imumap_options()
) {
  stop_fmt_if(
    !inherits(weights, "imumap_weights"),
    ERR_INVALID_WEIGHTS,
    arg = "weights"
  )

  age_alignment <- match.arg(age_alignment)
  cov_dt <- data.table::as.data.table(coverage_data)

  stop_fmt_if(
    !"id" %in% names(cov_dt),
    ERR_MISSING_COLUMN,
    col = "id",
    name = "coverage_data"
  )

  cov_dt$id <- as.character(cov_dt$id)
  src_ids <- weights$source_ids
  snk_ids <- weights$sink_ids
  w_mat <- weights$weights

  # Align source IDs
  if (!is.null(draws_col) && draws_col %in% names(cov_dt)) {
    # Bayesian posterior draws handling
    val_col <- setdiff(names(cov_dt), c("id", draws_col))[1]
    draw_ids <- unique(cov_dt[[draws_col]])

    res_list <- vector("list", length(draw_ids))
    for (idx in seq_along(draw_ids)) {
      dr <- draw_ids[idx]
      sub_dt <- cov_dt[get(draws_col) == dr]
      src_vec <- setNames(sub_dt[[val_col]], sub_dt$id)[src_ids]
      src_vec[is.na(src_vec)] <- 0

      snk_vals <- as.numeric(Matrix::crossprod(w_mat, src_vec))
      res_list[[idx]] <- data.table::data.table(
        id = snk_ids,
        draw = dr,
        value = snk_vals
      )
    }
    res_dt <- data.table::rbindlist(res_list)
    return(res_dt)
  }

  # Standard count and rate allocation
  has_counts <- all(c("vaccinated", "total") %in% names(cov_dt))
  has_coverage <- "coverage" %in% names(cov_dt)

  if (has_counts) {
    vax_vec <- setNames(cov_dt$vaccinated, cov_dt$id)[src_ids]
    tot_vec <- setNames(cov_dt$total, cov_dt$id)[src_ids]
    vax_vec[is.na(vax_vec)] <- 0
    tot_vec[is.na(tot_vec)] <- 0

    snk_vax <- as.numeric(Matrix::crossprod(w_mat, vax_vec))
    snk_tot <- as.numeric(Matrix::crossprod(w_mat, tot_vec))
    snk_cov <- ifelse(snk_tot > 0, snk_vax / snk_tot, NA_real_)

    res_dt <- data.table::data.table(
      id = snk_ids,
      vaccinated = snk_vax,
      total = snk_tot,
      coverage = snk_cov
    )
  } else if (has_coverage) {
    cov_vec <- setNames(cov_dt$coverage, cov_dt$id)[src_ids]
    cov_vec[is.na(cov_vec)] <- 0
    snk_cov <- as.numeric(Matrix::crossprod(w_mat, cov_vec))

    res_dt <- data.table::data.table(
      id = snk_ids,
      coverage = snk_cov
    )
  } else {
    stop_fmt_if(
      TRUE,
      ERR_MISSING_COVERAGE_COL,
      col = "vaccinated/total or coverage"
    )
  }

  res_dt
}
