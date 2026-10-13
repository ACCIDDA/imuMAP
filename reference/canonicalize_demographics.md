# Canonicalize Sink Demographic Data

Validates required schema columns (`id`, `population`, and optional
`age_group`) for sink demographic distributions.

## Usage

``` r
canonicalize_demographics(data)
```

## Arguments

- data:

  A `data.frame` or `data.table` containing sink population counts.

## Value

A validated `data.table` of class `imumap_demographics`.

## Examples

``` r
df <- data.frame(id = c("c1", "c2"), population = c(100, 250))
dt <- canonicalize_demographics(df)
```
