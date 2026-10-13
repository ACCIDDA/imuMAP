# Configure imuMAP Execution Options

Creates an `imumap_options` configuration object for tuning execution
parameters such as matrix sparsity, distance thresholds, and
zero-population handling.

## Usage

``` r
imumap_options(
  max_distance = Inf,
  zero_pop_handling = c("distribute_evenly", "ignore", "error"),
  tie_breaking = c("split", "first"),
  sparse = TRUE
)
```

## Arguments

- max_distance:

  Numeric. Global cutoff distance. Default is `Inf`.

- zero_pop_handling:

  Character. Strategy for handling sink units with 0 population:
  `"distribute_evenly"`, `"ignore"`, or `"error"`.

- tie_breaking:

  Character. Strategy for equidistant source-sink ties.

- sparse:

  Logical. If `TRUE`, intermediate matrices utilize sparse `dgCMatrix`
  formats. Default `TRUE`.

## Value

An S3 object of class `imumap_options`.

## Examples

``` r
opts <- imumap_options(sparse = TRUE, max_distance = 10000)
```
