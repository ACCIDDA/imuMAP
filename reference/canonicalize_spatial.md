# Canonicalize Spatial Source and Sink Geometries

Validates schema requirements (required `id` column, non-empty
geometries, unique identifiers) and harmonizes coordinate reference
systems (CRS) by transforming `source` to match `sink`'s CRS if they
differ.

## Usage

``` r
canonicalize_spatial(source, sink)
```

## Arguments

- source:

  An `sf` object representing source geometries (e.g. school points).

- sink:

  An `sf` object representing target/sink geometries (e.g. CBG
  polygons).

## Value

A list of class `imumap_spatial` containing validated `source` and
`sink` `sf` objects with matching CRS.

## Examples

``` r
pts <- sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), crs = 4326)
src <- sf::st_sf(id = c("s1", "s2"), geometry = pts)
poly_mat <- matrix(c(0, 0, 2, 0, 2, 2, 0, 2, 0, 0), ncol = 2, byrow = TRUE)
snk <- sf::st_sf(id = "c1", geometry = sf::st_sfc(sf::st_polygon(list(poly_mat)), crs = 4326))
res <- canonicalize_spatial(src, snk)
```
