#' @title Canonicalize Spatial Source and Target Geometries
#'
#' @description Validates schema requirements (required `id` column, non-empty
#'   geometries, unique identifiers) and harmonizes coordinate reference
#'   systems (CRS) by transforming `source` to match `target`'s CRS if they differ.
#'
#' @param source An `sf` object representing source geometries (e.g. school points).
#' @param target An `sf` object representing target geometries (e.g. CBG polygons).
#'
#' @return A list of class `imumap_spatial` containing validated `source` and
#'   `target` `sf` objects with matching CRS.
#'
#' @examples
#' pts <- sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), crs = 4326)
#' src <- sf::st_sf(id = c("s1", "s2"), geometry = pts)
#' poly_mat <- matrix(c(0, 0, 2, 0, 2, 2, 0, 2, 0, 0), ncol = 2, byrow = TRUE)
#' tgt <- sf::st_sf(id = "c1", geometry = sf::st_sfc(sf::st_polygon(list(poly_mat)), crs = 4326))
#' res <- canonicalize_spatial(src, tgt)
#'
#' @export
canonicalize_spatial <- function(source, target) {
  stop_fmt_if(!inherits(source, "sf"), ERR_INVALID_SF, arg = "source")
  stop_fmt_if(!inherits(target, "sf"), ERR_INVALID_SF, arg = "target")

  stop_fmt_if(
    !"id" %in% names(source),
    ERR_MISSING_COLUMN,
    col = "id",
    name = "source"
  )
  stop_fmt_if(
    !"id" %in% names(target),
    ERR_MISSING_COLUMN,
    col = "id",
    name = "target"
  )

  source$id <- as.character(source$id)
  target$id <- as.character(target$id)

  stop_fmt_if(
    anyNA(source$id) || anyDuplicated(source$id) > 0,
    ERR_DUPLICATE_ID,
    name = "source"
  )
  stop_fmt_if(
    anyNA(target$id) || anyDuplicated(target$id) > 0,
    ERR_DUPLICATE_ID,
    name = "target"
  )

  stop_fmt_if(any(sf::st_is_empty(source)), ERR_EMPTY_GEOMETRY, name = "source")
  stop_fmt_if(any(sf::st_is_empty(target)), ERR_EMPTY_GEOMETRY, name = "target")

  src_crs <- sf::st_crs(source)
  tgt_crs <- sf::st_crs(target)

  stop_fmt_if(is.na(src_crs), ERR_INVALID_CRS, name = "source")
  stop_fmt_if(is.na(tgt_crs), ERR_INVALID_CRS, name = "target")

  if (src_crs != tgt_crs) {
    source <- sf::st_transform(source, tgt_crs)
  }

  structure(
    list(source = source, target = target),
    class = "imumap_spatial"
  )
}

#' @title Canonicalize Target Demographic Data
#'
#' @description Validates required schema columns (`id`, `population`, and
#'   optional `age_group`) for target demographic distributions.
#'
#' @param data A `data.frame` or `data.table` containing target population counts.
#'
#' @return A validated `data.table` of class `imumap_demographics`.
#'
#' @examples
#' df <- data.frame(id = c("c1", "c2"), population = c(100, 250))
#' dt <- canonicalize_demographics(df)
#'
#' @importFrom data.table as.data.table setattr
#' @export
canonicalize_demographics <- function(data) {
  stop_fmt_if(
    !is.data.frame(data),
    ERR_MISSING_COLUMN,
    col = "data.frame",
    name = "data"
  )

  dt <- data.table::as.data.table(data)

  stop_fmt_if(
    !"id" %in% names(dt),
    ERR_MISSING_COLUMN,
    col = "id",
    name = "data"
  )
  stop_fmt_if(
    !"population" %in% names(dt),
    ERR_MISSING_COLUMN,
    col = "population",
    name = "data"
  )

  dt$id <- as.character(dt$id)

  stop_fmt_if(
    !is.numeric(dt$population) || any(dt$population < 0, na.rm = TRUE),
    ERR_NON_NUMERIC_POP
  )

  data.table::setattr(dt, "class", c("imumap_demographics", class(dt)))
  dt
}
