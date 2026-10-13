# Spatial Allocation and Re-projection Workflow

## Overview

`imuMAP` (*Immunity: Mapping, Aggregation, and Projection*) provides
modular, reproducible tools for re-projecting vaccination coverage
estimates and demographic counts across differing spatial scales,
geometry types (e.g., point observations to polygon areas), and
demographic strata.

This vignette outlines the end-to-end workflow using decoupled
intermediate objects for reusable distance and allocation matrices.

------------------------------------------------------------------------

## 1. Input Canonicalization

`imuMAP` enforces schema requirements on input spatial layers and
demographic tables. The canonicalization step harmonizes coordinate
reference systems (CRS) to match the target sink and validates required
schema columns (`id`, `geometry`, etc.):

``` r

library(imuMAP)
library(sf)

# Load raw spatial geometries
schools_raw <- sf::read_sf("schools.gpkg") # Point observations (Source)
cbgs_raw    <- sf::read_sf("nc_cbgs.gpkg") # Target areal units (Sink)

# Canonicalize spatial objects: verifies required ID schemas and transforms source CRS to match sink
spatial_data <- canonicalize_spatial(
  source = schools_raw,
  sink   = cbgs_raw
)

# Canonicalize sink demographic data (e.g., population by sink id and age cohort)
demographics <- canonicalize_demographics(
  data = read.csv("cbg_pop_age0_5.csv")
)
```

------------------------------------------------------------------------

## 2. Reusable Intermediate Objects

### Step A: Distance Matrix with Eligibility Filtering

Pairwise spatial distances are stable across estimation years and
scenarios. You can pre-calculate the distance matrix once and apply
generic eligibility filters (e.g., district containment, catchment
zones, or distance cutoffs):

``` r

# Define split eligibility rules based on source metadata (school_type)
# 1. Public schools are restricted to block groups within the same school district
rule_pub <- eligibility_rule(
  source_filter = list(school_type = "public"),
  match_cols = "district_id"
)

# 2. Private schools have statewide access up to a 50km radius
rule_priv <- eligibility_rule(
  source_filter = list(school_type = "private"),
  max_distance = 50000
)

# Compute distance matrix applying both conditional eligibility rules
dist_mat <- calc_distance_matrix(
  source = spatial_data$source,
  sink   = spatial_data$sink,
  eligibility = eligibility_rules(rule_pub, rule_priv)
)
```

### Step B: Allocation Weights & Kernel Options

Construct kernel options specifying distance decay parameters. Different
subsets of sources (e.g., public vs. private schools) can utilize
distinct decay kernels:

``` r

# Define kernel options for different school types / strata
public_kernel <- kernel_options(
  kernel = "gravity",
  params = list(beta = 2.0)
)

private_kernel <- kernel_options(
  kernel = "exponential",
  params = list(decay_rate = 0.05)
)

# Compute normalized allocation weights matrix
alloc_weights <- calc_allocation_weights(
  dist_matrix    = dist_mat,
  sink_pop       = demographics,
  kernel_options = list(
    public  = public_kernel,
    private = private_kernel
  ),
  normalize      = TRUE
)
```

------------------------------------------------------------------------

## 3. Allocation & Re-projection

Once the allocation weight matrix is constructed, coverage data across
multiple years or posterior draws from Bayesian models (e.g., `imuGAP`)
can be rapidly projected onto target spatial units:

``` r

# Load annual coverage data
school_vax_2023 <- read.csv("school_vax_2023.csv")
school_vax_2024 <- read.csv("school_vax_2024.csv")

# Project 2023 estimates onto CBGs
cbg_coverage_2023 <- allocate(
  coverage_data = school_vax_2023,
  weights       = alloc_weights,
  age_alignment = "uniform"
)

# Project 2024 estimates onto CBGs using the cached weights matrix
cbg_coverage_2024 <- allocate(
  coverage_data = school_vax_2024,
  weights       = alloc_weights,
  age_alignment = "uniform"
)
```

------------------------------------------------------------------------

## 4. Working with Posterior Uncertainty (e.g., `imuGAP`)

When coverage data contains posterior samples/draws,
[`allocate()`](https://accidda.github.io/imuMAP/reference/allocate.md)
propagates the entire distribution through the allocation matrix:

``` r

# Project posterior draws
cbg_draws <- allocate(
  coverage_data = imugap_predict_draws,
  weights       = alloc_weights,
  draws_col     = "draw"
)
```
