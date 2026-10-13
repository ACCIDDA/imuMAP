# Create Distance Decay Kernel Options

Configures the mathematical decay function applied to spatial distances
during allocation weight calculations.

## Usage

``` r
kernel_options(
  kernel = c("gravity", "exponential", "power", "uniform", "gaussian"),
  params = list()
)
```

## Arguments

- kernel:

  Character or function. Choices include `"gravity"`, `"exponential"`,
  `"power"`, `"uniform"`, `"gaussian"`, or a custom function
  `f(d, params)` taking distance and parameter list.

- params:

  Named list of parameters for the decay kernel.

## Value

An S3 object of class `imumap_kernel_options`.

## Examples

``` r
# Gravity decay with power beta = 2
opt <- kernel_options("gravity", list(beta = 2, min_dist = 10))

# Exponential decay with rate 0.05
exp_opt <- kernel_options("exponential", list(decay_rate = 0.05))
```
