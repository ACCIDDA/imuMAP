# Define Spatial Eligibility Rule

Constructs an eligibility rule partitioning how sources map to sinks
based on spatial or metadata attributes (e.g. school type, district
containment, or subset-specific distance cutoffs).

## Usage

``` r
eligibility_rule(
  source_filter = NULL,
  match_cols = NULL,
  max_distance = Inf,
  sink_filter = NULL,
  predicate = NULL
)
```

## Arguments

- source_filter:

  Named list, character expression, logical vector, or function
  `f(source)` selecting the source subset this rule applies to. For
  example: `list(school_type = "public")`. Defaults to `NULL` (all
  sources).

- match_cols:

  Character vector of metadata column names that must match identically
  between source and sink units (e.g. `"district_id"`).

- max_distance:

  Numeric. Maximum distance cutoff specific to this rule. Defaults to
  `Inf`.

- sink_filter:

  Optional named list, logical vector, or function `f(sink)` restricting
  eligible sink units.

- predicate:

  Optional function `f(source_sub, sink_sub)` returning a logical matrix
  for matching source and sink subsets.

## Value

An S3 object of class `imumap_eligibility_rule`.

## Examples

``` r
# Rule: public schools only map to block groups in the same school district
rule_pub <- eligibility_rule(
  source_filter = list(school_type = "public"),
  match_cols = "district_id"
)

# Rule: private schools map within a 30km radius without district constraints
rule_priv <- eligibility_rule(
  source_filter = list(school_type = "private"),
  max_distance = 30000
)
```
