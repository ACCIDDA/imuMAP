# Bundle Multiple Eligibility Rules

Combines multiple `imumap_eligibility_rule` objects into a single rule
set.

## Usage

``` r
eligibility_rules(...)
```

## Arguments

- ...:

  One or more `imumap_eligibility_rule` objects.

## Value

An S3 object of class `imumap_eligibility_rules`.

## Examples

``` r
r1 <- eligibility_rule(list(type = "pub"), match_cols = "district_id")
r2 <- eligibility_rule(list(type = "priv"), max_distance = 25000)
rules <- eligibility_rules(r1, r2)
```
