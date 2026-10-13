# Calculate Spatial Allocation Weights Matrix

Combines pairwise spatial distances, sink population counts, and
distance decay kernels into an allocation weights matrix mapping sources
to sinks.

## Usage

``` r
calc_allocation_weights(
  dist_matrix,
  sink_pop = NULL,
  kernel_options = imuMAP::kernel_options("gravity"),
  normalize = TRUE,
  sparse = TRUE
)
```

## Arguments

- dist_matrix:

  An `imumap_dist_matrix` object created by
  [`calc_distance_matrix()`](https://accidda.github.io/imuMAP/reference/calc_distance_matrix.md).

- sink_pop:

  Optional `data.frame` or `imumap_demographics` object containing sink
  demographic population counts (must include `id` and `population`).

- kernel_options:

  An `imumap_kernel_options` object, or a list of
  `imumap_kernel_options` objects. Defaults to
  `kernel_options("gravity")`.

- normalize:

  Logical. If `TRUE`, normalizes weights so each source row sums to 1.
  Defaults to `TRUE`.

- sparse:

  Logical. If `TRUE`, stores weights as a sparse `dgCMatrix`. Defaults
  to `TRUE`.

## Value

An S3 object of class `imumap_weights`.

## Examples

``` r
pts <- sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), crs = 3857)
src <- sf::st_sf(id = c("s1", "s2"), geometry = pts)
snk_pts <- sf::st_sfc(sf::st_point(c(0, 0.5)), sf::st_point(c(1, 1.5)), crs = 3857)
snk <- sf::st_sf(id = c("c1", "c2"), geometry = snk_pts)
dmat <- calc_distance_matrix(src, snk)
w <- calc_allocation_weights(dmat)
```
