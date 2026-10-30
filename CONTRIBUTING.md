# Contributing to imuMAP

Thank you for your interest in contributing to `imuMAP`! This guide explains how to contribute to the package and how to keep the project documentation clear, consistent, and approachable.

---

## Architecture & Scope

`imuMAP` (*Immunity: Mapping, Aggregation, and Projection*) provides geospatial mapping, spatial aggregation, and re-projection tools to translate and visualize vaccination coverage estimates across spatial organizations and scales.

* **Relation to `imuGAP`**: `imuGAP` holds the statistical model definitions (Stan) and Bayesian parameter estimation. `imuMAP` should support consuming `imuGAP` predict outputs (after mild transformation), but should not be tightly coupled (i.e. support the *inputs* rather than the specific process that generated them). `imuMAP` concerns downstream mapping, spatial aggregation, re-projecting estimates onto alternative administrative/geographic scales, and cartographic visualization.

---

## Coding Standards & Tooling

We adhere to modern R package conventions and use automated tooling for code formatting and quality checks:

### 1. Code Formatting with `air`
* All R code must be formatted using [`air`](https://github.com/posit-dev/air).
* Format your code locally before committing:
  ```sh
  just format
  ```

### 2. Static Analysis with `lintr`
* We use `lintr` to ensure style consistency (line length $\le 100$, snake_case naming, implicit return style).
* Check your code locally:
  ```sh
  just lint
  ```

### 3. Documentation with `roxygen2`
* All public and internal functions must be thoroughly documented using `roxygen2` with Markdown support enabled (`Roxygen: list(markdown = TRUE, ...)`).
* Always use explicit `@title` and `@description` tags in roxygen documentation blocks.
* Unexported internal functions should be marked with `@keywords internal` and `@noRd` to keep code documented in source without generating unused `.Rd` files.
* `man/*.Rd` and `NAMESPACE` are generated from roxygen comments:
  ```sh
  just docs
  ```

### 4. Voice and Clarity in Documentation and Vignettes
* Write user-facing prose in active voice with an explicit actor.
* Start with the person, package, or function doing the work: "We designed `imuMAP` to re-project coverage estimates" or "This workflow allocates school-level coverage to Census Block Groups."
* Where possible, avoid pronouns in explanatory prose. Use a noun instead: for example, "the tool provides" instead of "it provides". However, "we" for authorial voice and "you" for the reader are preferred.
* Prefer concrete, direct verbs over passive phrasing or abstract placeholders. Avoid heavy nominalizations, redundant `to be` constructions, and jargon that readers need to decode before they reach the point.
* Keep the narrative conversational but formal. Explain the problem, then show how the code solves it in plain English.
* Use consistent terminology across documentation and vignettes so readers can follow the same concept from the intro to the examples.

---

## Unit Testing & Test-Driven Development

We use `testthat` (edition 3) for unit and integration testing.

### 1. Running Tests
* Run full test suite:
  ```sh
  just test
  ```
* Run test suite stopping on first failure:
  ```sh
  just test-fast
  ```
* Check test coverage:
  ```sh
  just coverage
  ```

### 2. Error Messages, Signaling Standards, and Unit Testing

All user-facing validation errors and warnings should follow these standards:

#### Centralized Named Template Constants
* Define error and warning message format strings as constants at the top of each R file prefixed with `ERR_` or `MSG_`:
* Use named `{var}` placeholders rather than cryptic format specifiers:
  ```r
  ERR_INPUT_DIR_NOT_FOUND <- "input directory '{path}' does not exist"
  ERR_INVALID_GEO_COLUMN  <- "expected column '{col}' in spatial layer '{layer}'"
  ```

#### Signaling Functions: `stop_fmt_if` and `warn_fmt_if`
* When using error signaling helpers, pass named arguments matching `{var}` placeholders.
* Custom conditions inherit from `imumap_error` or `imumap_warning` (and base `error` / `warning`) for programmatic inspection.

#### Typography: Backticks vs. Single Quotes
* **Backticks (`` `code` ``)**: Use for formal R code symbols, argument names, function names, expressions, and classes (e.g. `` `path` must be a character scalar ``).
* **Single Quotes (`'value'`)**: Use for user-supplied string values, file paths, column names, or discrete inputs (e.g. `` file '{path}' not found ``).

#### Markdown List Formatting Standards
* Precede list blocks with an empty blank line after introductory text.
* Use consistent bullet markers (`-`) for unordered lists and (`1.`, `2.`) for top-level ordered sequences.
* Indent sub-lists by 2 or 4 spaces and wrap continuations with matching margins.

---

## Diagrams & Documentation Assets

* **Mermaid Diagrams**: Diagram sources live in `vignettes/figures/*.mmd`.
* Compiled SVG/PDF assets are generated via `just diagrams` (integrated into `just docs`, `just render`, and `just site`).
* Embed diagrams using `knitr::include_graphics("figures/<name>.svg")`.
* Inverted dark-mode filter styles in `pkgdown/extra.css` ensure diagrams blend seamlessly across light and dark themes.

---

## Pull Request and CI Workflows

Every pull request triggers automated GitHub Actions workflows that delegate directly to `justfile` recipes:

1. **`R-CMD-check`**: Runs `R CMD check --as-cran` across Ubuntu, macOS, and Windows on R release, oldrel, and devel (9 jobs), using shared build artifacts.
2. **`lint`**: Runs `just lint` (verifying `air format . --check` and `lintr::lint_package()`).
3. **`test-coverage`**: Computes code coverage with `covr` and uploads results to Codecov.
4. **`pkgdown`**: Builds the documentation site and validates vignettes. Deployed to GitHub Pages upon push to `main` and published releases.

All checks must pass before merging.
