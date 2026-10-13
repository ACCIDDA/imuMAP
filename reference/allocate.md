# Re-project / Allocate Coverage Estimates Across Spatial Units

Projects source-level coverage data (or Bayesian posterior draws) across
target spatial units (sinks) using pre-computed allocation weights.

## Usage

``` r
allocate(
  coverage_data,
  weights,
  age_alignment = c("uniform", "proportional", "exact"),
  draws_col = NULL,
  options = imuMAP::imumap_options()
)
```

## Arguments

- coverage_data:

  A `data.frame` or `data.table` containing source-level observations
  with column `id` (matching `weights` source IDs) and numeric columns
  such as `vaccinated`, `total` (or `coverage`).

- weights:

  An `imumap_weights` object created by
  [`calc_allocation_weights()`](https://accidda.github.io/imuMAP/reference/calc_allocation_weights.md).

- age_alignment:

  Character specifying age harmonization method (`"uniform"`,
  `"proportional"`, or `"exact"`). Default is `"uniform"`.

- draws_col:

  Optional character naming the column representing posterior draws.

- options:

  An `imumap_options` object.

## Value

A tidy `data.table` with allocated estimates by sink `id`.

## Examples

``` r
pts <- sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), crs = 3857)
src <- sf::st_sf(id = c("s1", "s2"), geometry = pts)
snk_pts <- sf::st_sfc(sf::st_point(c(0, 0.5)), sf::st_point(c(1, 1.5)), crs = 3857)
snk <- sf::st_sf(id = c("c1", "c2"), geometry = snk_pts)
dmat <- calc_distance_matrix(src, snk)
w <- calc_allocation_weights(dmat)
cov_data <- data.frame(id = c("s1", "s2"), vaccinated = c(80, 95), total = c(100, 100))
res <- allocate(cov_data, w)
```
