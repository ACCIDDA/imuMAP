#' @title Create Distance Decay Kernel Options
#'
#' @description Configures the mathematical decay function applied to spatial
#'   distances during allocation weight calculations.
#'
#' @param kernel Character or function. Supported character choices include
#'   `"gravity"`, `"exponential"`, `"power"`, `"uniform"`, `"gaussian"`, and
#'   `"cauchy"`. Alternatively, accepts any distribution density or decay
#'   function `f(d, ...)` (e.g. `stats::dnorm`, `stats::dexp`, or function
#'   factories from packages such as `accessibility`).
#' @param params Named list of parameters for the decay kernel.
#'
#' @return An S3 object of class `imumap_kernel_options`.
#'
#' @examples
#' # Gravity decay with power beta = 2
#' opt <- kernel_options("gravity", list(beta = 2, min_dist = 10))
#'
#' # Exponential decay using stats::dexp rate
#' exp_opt <- kernel_options("exponential", list(decay_rate = 0.05))
#'
#' # Gaussian decay using stats::dnorm
#' gaus_opt <- kernel_options("gaussian", list(sigma = 500))
#'
#' @export
kernel_options <- function(
  kernel = c("gravity", "exponential", "power", "uniform", "gaussian", "cauchy"),
  params = list()
) {
  if (is.character(kernel)) {
    valid_kernels <- c(
      "gravity",
      "exponential",
      "power",
      "uniform",
      "gaussian",
      "cauchy"
    )
    kernel <- match.arg(kernel, valid_kernels)
  } else {
    stop_fmt_if(
      !is.function(kernel),
      ERR_INVALID_KERNEL,
      kernel = "unknown",
      valid = "character or function"
    )
  }

  default_params <- list()
  if (identical(kernel, "gravity")) {
    default_params <- list(beta = 2.0, min_dist = 1.0)
  } else if (identical(kernel, "exponential")) {
    default_params <- list(decay_rate = 0.05)
  } else if (identical(kernel, "power")) {
    default_params <- list(power = 1.0, min_dist = 1.0)
  } else if (identical(kernel, "uniform")) {
    default_params <- list()
  } else if (identical(kernel, "gaussian")) {
    default_params <- list(sigma = 1000.0)
  } else if (identical(kernel, "cauchy")) {
    default_params <- list(scale = 1000.0)
  }

  for (nm in names(default_params)) {
    if (!nm %in% names(params)) {
      params[[nm]] <- default_params[[nm]]
    }
  }

  structure(
    list(
      kernel = kernel,
      params = params
    ),
    class = "imumap_kernel_options"
  )
}

#' @title Apply Distance Decay Kernel to Distance Values
#'
#' @description Computes raw distance decay weights according to kernel options.
#'
#' @param d Numeric vector or matrix of distances.
#' @param options An `imumap_kernel_options` object.
#' @return Numeric vector or matrix of decay weights.
#' @keywords internal
#' @noRd
apply_kernel <- function(d, options) {
  stop_fmt_if(
    !inherits(options, "imumap_kernel_options"),
    ERR_INVALID_KERNEL_OPTIONS,
    arg = "options"
  )

  kernel <- options$kernel
  params <- options$params
  w <- matrix(0, nrow = nrow(d), ncol = ncol(d), dimnames = dimnames(d))
  valid_mask <- is.finite(d)

  if (!any(valid_mask)) {
    return(w)
  }

  d_valid <- d[valid_mask]

  if (is.function(kernel)) {
    fn_args <- names(formals(kernel))
    if ("params" %in% fn_args) {
      w[valid_mask] <- kernel(d_valid, params)
    } else if (length(params) > 0 && any(names(params) %in% fn_args)) {
      call_args <- c(list(d_valid), params[intersect(names(params), fn_args)])
      w[valid_mask] <- do.call(kernel, call_args)
    } else {
      w[valid_mask] <- tryCatch(
        kernel(d_valid),
        error = function(e) kernel(d_valid, params)
      )
    }
  } else if (kernel == "gravity") {
    min_d <- params$min_dist %||% 1.0
    beta <- params$beta %||% 2.0
    d_eff <- pmax(d_valid, min_d)
    w[valid_mask] <- 1.0 / (d_eff^beta)
  } else if (kernel == "exponential") {
    lambda <- params$decay_rate %||% 0.05
    w[valid_mask] <- stats::dexp(d_valid, rate = lambda)
  } else if (kernel == "power") {
    min_d <- params$min_dist %||% 1.0
    pow <- params$power %||% 1.0
    d_eff <- pmax(d_valid, min_d)
    w[valid_mask] <- d_eff^(-pow)
  } else if (kernel == "uniform") {
    w[valid_mask] <- 1.0
  } else if (kernel == "gaussian") {
    sigma <- params$sigma %||% 1000.0
    w[valid_mask] <- stats::dnorm(d_valid, mean = 0, sd = sigma)
  } else if (kernel == "cauchy") {
    scale <- params$scale %||% 1000.0
    w[valid_mask] <- stats::dcauchy(d_valid, location = 0, scale = scale)
  }

  w
}

#' @title Infix NULL Coalescing Operator
#'
#' @description Returns the left-hand side if not NULL, otherwise the right-hand side.
#'
#' @param x Any object.
#' @param y Default object if `x` is NULL.
#' @return Either `x` or `y`.
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' @title Print Kernel Options Summary
#'
#' @description Prints a concise summary of an `imumap_kernel_options` object.
#'
#' @param x An `imumap_kernel_options` object.
#' @param ... Additional arguments passed to print methods.
#'
#' @return The object `x`, invisibly.
#' @export
print.imumap_kernel_options <- function(x, ...) {
  cat(
    "<imumap_kernel_options>\n",
    "Kernel: ",
    if (is.function(x$kernel)) "<custom function>" else x$kernel,
    "\n",
    "Params: ",
    paste(names(x$params), x$params, sep = "=", collapse = ", "),
    "\n",
    sep = ""
  )
  invisible(x)
}
