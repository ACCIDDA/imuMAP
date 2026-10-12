# AGENTS.md — Developer and AI Agent Guidelines for imuMAP

This document outlines architectural principles, coding conventions, testing standards, and development workflows for developers and AI coding agents working on `imuMAP`.

---

## 1. Role, Scope & Package Architecture

`imuMAP` (*Immunity: Mapping, Aggregation, and Projection*) is an R package within the ACCIDDA project providing geospatial mapping, spatial aggregation, and re-projection tools for vaccination coverage estimates across spatial organizations and scales.

* **Package Scope**:
  * **Geospatial & Projection Tools**: Handling spatial layers, boundary shapefiles/geometries, spatial aggregation, and re-projecting/re-computing coverage estimates onto alternative spatial scales and administrative units.
  * **Visualization**: Publication-ready maps and diagnostic visual summaries.
* **Relation to Sibling Package (`imuGAP`)**:
  * `imuGAP` holds the statistical model definitions (Stan), Bayesian sampling routines, and parameter estimation.
  * `imuMAP` should support consuming `imuGAP` predict outputs (after mild transformation), but should not be tightly coupled (i.e. support the *inputs* rather than the specific process that generated them). `imuMAP` does not contain Stan models or statistical fitting code.

---

## 2. Core Tenets & Development Workflows

### The `justfile` as Single Source of Truth
* All builds, linting, formatting, tests, documentation generation, and site previews must be performed via `justfile` recipes.
* CI workflows (`.github/workflows/*.yaml`) invoke `just <recipe>`.

Common recipes:
```sh
just format        # Format R code using air
just lint          # Check formatting and linting (air + lintr)
just docs          # Regenerate man/*.Rd, NAMESPACE, R/globals.R
just diagrams      # Compile Mermaid diagrams (.mmd -> .svg/.pdf)
just install       # Install development version of package
just test          # Run testthat test suite
just test-fast     # Run test suite stopping on first failure
just coverage      # Check test coverage via covr
just spell         # Check documentation spelling
just site          # Fast build of pkgdown documentation site
just site-full     # Full build of pkgdown documentation site with install
just check         # Build and check package tarball
just check-cran    # Check package with --as-cran
```

---

## 3. R Coding & Quality Standards

### Code Formatting (`air`) & Linting (`lintr`)
* All R files must adhere to `air` formatting (`indent-width = 2`, `line-width = 80`).
* `lintr` enforces line length $\le 100$, snake_case naming (allowing `imuMAP`, `imuGAP`, `ERR_`, `MSG_`), and implicit returns.
* Always run `just format && just lint` before finishing any task.

### Error Messages & Signaling Standards
* Define centralized error and warning format strings at the top of R files:
  ```r
  ERR_INPUT_FILE_MISSING <- "input file '{filename}' was not found in directory '{dir}'"
  ```
* Use named `{var}` placeholders rather than unnamed format tokens.
* Format code entities (function names, formal parameters, R expressions) with **backticks (`` `code` ``)**.
* Format user-supplied values, paths, and column names with **single quotes (`'value'`)**.

### Documentation (`roxygen2`)
* Use Markdown-formatted roxygen comments.
* Always use explicit `@title` and `@description` tags in roxygen blocks.
* Unexported internal functions should be marked with `@keywords internal` and `@noRd` to keep code documented in source without generating unused `.Rd` files.
* Document `@param` concisely with type and default where applicable.
* Document `@return` explicitly describing return type and structure.
* For exported functions, include runnable and fast `@examples`.

### Unit Testing (`testthat`)
* All user-facing functions must have comprehensive test coverage in `tests/testthat/`.
* Test both expected success paths and error conditions.
* Assert on error messages using exact regex or template pattern matching.
* Ensure tests run cleanly across Linux, macOS, and Windows.

### Diagrams & Vignettes
* Store source diagrams as Mermaid files in `vignettes/figures/*.mmd`.
* Generated SVG/PDF files are gitignored and built dynamically via `just diagrams`.
* Ensure pkgdown dark-mode compatibility for visual assets and plots.

---

## 4. Git & Commit Guidelines

* Maintain a clean git history.
* Keep commit messages descriptive and focused.
* Do not commit generated build artifacts (`*.tar.gz`, `*.Rcheck/`, `docs/`, `man/*.Rd`, `R/globals.R`).
