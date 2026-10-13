# Compute Pairwise Distance Matrix with Optional Eligibility Filtering

Calculates pairwise spatial Euclidean or ellipsoidal distances between
canonicalized source and sink geometries. If geometries are polygons,
centroids are computed automatically.

## Usage

``` r
calc_distance_matrix(source, sink, max_distance = Inf, eligibility = NULL)
```

## Arguments

- source:

  An `sf` object of source geometries with required column `id`.

- sink:

  An `sf` object of sink geometries with required column `id`.

- max_distance:

  Numeric. Global maximum threshold cutoff distance (in CRS units).
  Distances greater than this threshold are set to `Inf`. Defaults to
  `Inf`.

- eligibility:

  Optional eligibility specification to partition and restrict how
  source locations map to sink targets. Accepts an
  `imumap_eligibility_rule`, an `imumap_eligibility_rules` rule set, a
  list of rules, a custom predicate function `f(source, sink)`, or a
  pre-computed logical matrix.

## Value

An S3 object of class `imumap_dist_matrix` containing the pairwise
distance matrix, source IDs, and sink IDs.

## Examples

``` r
pts <- sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), crs = 3857)
src <- sf::st_sf(id = c("s1", "s2"), school_type = c("pub", "priv"), geometry = pts)
snk_pts <- sf::st_sfc(sf::st_point(c(0, 0.5)), sf::st_point(c(1, 1.5)), crs = 3857)
snk <- sf::st_sf(id = c("c1", "c2"), geometry = snk_pts)
# Rule-based eligibility: limit public schools to 500m
r_pub <- eligibility_rule(source_filter = list(school_type = "pub"), max_distance = 500)
dmat <- calc_distance_matrix(src, snk, eligibility = r_pub)
```
