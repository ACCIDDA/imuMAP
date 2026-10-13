#' @title North Carolina Example School Locations
#'
#' @description Spatial point geometries representing 24 school locations across
#'   three adjacent counties (Scruggs, Simone, Watson) in western North Carolina.
#'   These match the simulated school locations and hierarchical grouping in the
#'   `imuGAP` simulation dataset (`imuGAP::locations_sim`).
#'
#' @format An `sf` data frame with 24 features and 3 fields:
#' \describe{
#'   \item{id}{Character string identifying the school name,
#'     matching `imuGAP::locations_sim$loc_id`.}
#'   \item{county}{Character string indicating county name ("Scruggs", "Simone", or "Watson").}
#'   \item{school_type}{School institutional subtype ("Public", "Private", or "Charter").}
#'   \item{geometry}{`sfc_POINT` geometry in WGS 84 (EPSG:4326).}
#' }
#'
#' @source Geocoded school address registries from North Carolina, anonymized
#'   and aligned with `imuGAP`.
#' @examples
#' data(nc_schools)
#' head(nc_schools)
"nc_schools"

#' @title North Carolina Example Census Block Groups
#'
#' @description Spatial polygon geometries representing 113 Census Block Groups (CBGs)
#'   across three adjacent counties (Scruggs, Simone, Watson) in western North Carolina.
#'   Used as target/sink spatial units for allocating coverage from schools.
#'
#' @format An `sf` data frame with 113 features and 4 fields:
#' \describe{
#'   \item{id}{Character string representing the 12-digit Census Block Group GEOID.}
#'   \item{county}{Character string indicating county name ("Scruggs", "Simone", or "Watson").}
#'   \item{ALAND}{Numeric land area in square meters.}
#'   \item{AWATER}{Numeric water area in square meters.}
#'   \item{geometry}{`sfc_MULTIPOLYGON` geometry in WGS 84 (EPSG:4326).}
#' }
#'
#' @source US Census Bureau 2020 TIGER/Line Shapefiles via `tigris`.
#' @examples
#' data(nc_cbgs)
#' head(nc_cbgs)
"nc_cbgs"

#' @title North Carolina Example Demographics by Census Block Group
#'
#' @description Target population estimates per Census Block Group calibrated to the
#'   county demographic totals in `imuGAP::locations_sim`.
#'
#' @format A `data.table` and `data.frame` with 113 rows and 3 fields:
#' \describe{
#'   \item{id}{Character string matching Census Block Group GEOID in `nc_cbgs`.}
#'   \item{county}{Character string indicating county name ("Scruggs", "Simone", or "Watson").}
#'   \item{population}{Numeric target population count (e.g. child cohort size).}
#' }
#'
#' @source Calibrated from US Census Bureau 2020 ACS and `imuGAP::locations_sim`.
#' @examples
#' data(nc_demographics)
#' head(nc_demographics)
"nc_demographics"
