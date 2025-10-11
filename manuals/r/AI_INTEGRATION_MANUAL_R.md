# AI Integration Manual - R

**Version:** 1.0.0  
**Last Updated:** 2025-10-11  
**Target Audience:** AI Agents (LLM-based development assistants)  
**Language:** R  
**R Version:** >= 4.0.0

---

## Table of Contents

1. [Introduction](#introduction)
2. [R-Specific Setup](#r-specific-setup)
3. [Project Structure](#project-structure)
4. [Phase 1: Planning](#phase-1-planning)
5. [Phase 2: Workspace Configuration](#phase-2-workspace-configuration)
6. [Phase 3: Implementation](#phase-3-implementation)
7. [Phase 4: Testing](#phase-4-testing)
8. [Phase 5: Documentation](#phase-5-documentation)
9. [Phase 6: Package Building & Distribution](#phase-6-package-building--distribution)
10. [Phase 7: Review & Quality Assurance](#phase-7-review--quality-assurance)
11. [R Best Practices](#r-best-practices)
12. [CRAN Submission Guidelines](#cran-submission-guidelines)
13. [Bioconductor Guidelines](#bioconductor-guidelines)
14. [Quick Reference](#quick-reference)

---

## Introduction

This manual adapts the standard AI Integration Manual for R development, incorporating R-specific tools, conventions, and best practices. R projects in HiveLLM ecosystem should follow this guide for consistency and quality.

### Core R Principles

1. **Package Structure**: Follow standard R package layout
2. **Documentation**: Use roxygen2 for inline documentation
3. **Testing**: Use testthat for comprehensive testing
4. **Style**: Follow tidyverse style guide
5. **Dependencies**: Minimize dependencies, prefer base R when possible
6. **CRAN Ready**: Structure projects for CRAN submission
7. **Reproducibility**: Version control all dependencies

### R Ecosystem Tools

| Tool | Purpose |
|------|---------|
| **devtools** | Package development workflow |
| **usethis** | Project setup automation |
| **testthat** | Unit testing framework |
| **roxygen2** | Documentation generation |
| **pkgdown** | Website generation |
| **lintr** | Static code analysis |
| **styler** | Code formatting |
| **covr** | Code coverage |
| **rhub** | CRAN submission checks |
| **goodpractice** | Best practice checks |

---

## R-Specific Setup

### Step 1: Install R and Development Tools

```bash
# Ubuntu/Debian
sudo apt-get update
sudo apt-get install -y r-base r-base-dev

# Install system dependencies for common packages
sudo apt-get install -y \
  libcurl4-openssl-dev \
  libssl-dev \
  libxml2-dev \
  libfontconfig1-dev \
  libharfbuzz-dev \
  libfribidi-dev \
  libfreetype6-dev \
  libpng-dev \
  libtiff5-dev \
  libjpeg-dev

# Verify installation
R --version
```

### Step 2: Install Essential Development Packages

Create `scripts/setup-r-env.R`:

```r
#!/usr/bin/env Rscript

# Install essential development packages
install.packages(
  c(
    "devtools",      # Package development
    "usethis",       # Project automation
    "testthat",      # Testing framework
    "roxygen2",      # Documentation
    "pkgdown",       # Website generation
    "lintr",         # Linting
    "styler",        # Code formatting
    "covr",          # Coverage
    "rhub",          # CRAN checks
    "goodpractice",  # Best practices
    "rcmdcheck",     # CMD check
    "spelling",      # Spell checking
    "knitr",         # Vignettes
    "rmarkdown",     # Markdown support
    "remotes"        # Remote package installation
  ),
  repos = "https://cloud.r-project.org/"
)

cat("✓ R development environment setup complete\n")
```

Run setup:

```bash
chmod +x scripts/setup-r-env.R
Rscript scripts/setup-r-env.R
```

### Step 3: Configure RStudio (Optional)

If using RStudio, create `.Rproj` file:

```bash
# Create project file
usethis::create_project(".", rstudio = TRUE)
```

### Step 4: Configure renv for Dependency Management

```r
# Initialize renv for reproducible environments
install.packages("renv")
renv::init()

# Snapshot dependencies
renv::snapshot()
```

---

## Project Structure

### Standard R Package Structure

```
package-name/
├── .Rbuildignore           # Files to ignore in R CMD build
├── .Rprofile               # Project-specific R profile
├── .gitignore              # Git ignore rules
├── .lintr                  # Lintr configuration
├── .github/
│   └── workflows/
│       ├── R-CMD-check.yaml
│       ├── test-coverage.yaml
│       └── pkgdown.yaml
├── DESCRIPTION             # Package metadata
├── NAMESPACE               # Exported functions (auto-generated)
├── LICENSE                 # License file
├── README.md               # Project overview
├── NEWS.md                 # Changelog
├── R/                      # R source code
│   ├── package.R           # Package-level documentation
│   ├── data.R              # Data documentation
│   ├── utils.R             # Utility functions
│   └── [feature].R         # Feature implementations
├── man/                    # Documentation (auto-generated)
├── tests/
│   ├── testthat.R          # Test runner
│   └── testthat/           # Test files
│       ├── setup.R         # Test setup
│       ├── test-[feature].R
│       └── ...
├── vignettes/              # Long-form documentation
│   ├── [package-name].Rmd
│   └── ...
├── data/                   # Package data (.rda files)
├── data-raw/               # Raw data and processing scripts
│   ├── [dataset].R
│   └── ...
├── inst/                   # Installed files
│   ├── CITATION            # Citation information
│   ├── extdata/            # External data
│   └── ...
├── docs/                   # HiveLLM documentation
│   ├── ROADMAP.md
│   ├── SPECS.md
│   ├── specs/
│   └── reviews/
├── scripts/                # Development scripts
│   ├── setup-r-env.R
│   ├── build.R
│   └── test.R
├── renv/                   # renv dependency management
├── renv.lock               # Dependency lockfile
└── _pkgdown.yml            # pkgdown configuration
```

---

## Phase 1: Planning

### Step 1.1: Create Package Structure

Use `usethis` to bootstrap the package:

```r
# Create new package
usethis::create_package("path/to/package")

# Setup Git
usethis::use_git()

# Setup GitHub (if needed)
usethis::use_github()

# Add standard files
usethis::use_readme_md()
usethis::use_news_md()
usethis::use_mit_license("Your Name")  # or use_gpl3_license(), etc.

# Setup testing
usethis::use_testthat()

# Setup CI/CD
usethis::use_github_action("check-standard")
usethis::use_github_action("test-coverage")
usethis::use_github_action("pkgdown")

# Setup pkgdown website
usethis::use_pkgdown()
```

### Step 1.2: Create DESCRIPTION File

Edit `DESCRIPTION`:

```dcf
Package: packagename
Title: Brief Description of Package (Title Case)
Version: 0.0.0.9000
Authors@R: 
    person("First", "Last", , "email@example.com", role = c("aut", "cre"),
           comment = c(ORCID = "YOUR-ORCID-ID"))
Description: What the package does (one paragraph). This should be detailed
    enough to understand the package's purpose but concise.
License: MIT + file LICENSE
Encoding: UTF-8
Roxygen: list(markdown = TRUE)
RoxygenNote: 7.3.0
Depends: 
    R (>= 4.0.0)
Imports:
    rlang (>= 1.0.0)
Suggests: 
    testthat (>= 3.0.0),
    knitr,
    rmarkdown,
    covr
Config/testthat/edition: 3
VignetteBuilder: knitr
URL: https://github.com/username/packagename, https://username.github.io/packagename/
BugReports: https://github.com/username/packagename/issues
```

### Step 1.3: Generate ROADMAP.md

Create `docs/ROADMAP.md`:

```markdown
# Package Development Roadmap

**Package:** packagename  
**Version:** 0.0.0.9000  
**Last Updated:** 2025-10-11

## Phase 1: Foundation (v0.1.0)

### 1.1 Package Structure
- [x] Initialize package structure
- [x] Setup DESCRIPTION file
- [x] Configure testing framework
- [x] Setup CI/CD workflows
- [ ] Create package-level documentation
- [ ] Add package logo (optional)

### 1.2 Core Functionality
- [ ] 1.2.1 Define main S3/S4/R6 classes
- [ ] 1.2.2 Implement constructor functions
- [ ] 1.2.3 Implement validation functions
- [ ] 1.2.4 Write unit tests for core classes

### 1.3 Basic Operations
- [ ] 1.3.1 Implement basic methods
- [ ] 1.3.2 Add print/summary methods
- [ ] 1.3.3 Add plot methods (if applicable)
- [ ] 1.3.4 Write tests for all methods

## Phase 2: Extended Features (v0.2.0)

### 2.1 Advanced Functionality
- [ ] 2.1.1 Feature 1 implementation
- [ ] 2.1.2 Feature 2 implementation
- [ ] 2.1.3 Integration tests

### 2.2 Data Processing
- [ ] 2.2.1 Input validation
- [ ] 2.2.2 Data transformation pipelines
- [ ] 2.2.3 Output formatting
- [ ] 2.2.4 Performance optimization

## Phase 3: Documentation & Polish (v0.3.0)

### 3.1 Documentation
- [ ] 3.1.1 Complete all roxygen2 documentation
- [ ] 3.1.2 Write introductory vignette
- [ ] 3.1.3 Write use case vignettes
- [ ] 3.1.4 Add code examples to all functions

### 3.2 Package Website
- [ ] 3.2.1 Configure pkgdown
- [ ] 3.2.2 Customize website theme
- [ ] 3.2.3 Add articles/tutorials
- [ ] 3.2.4 Deploy to GitHub Pages

## Phase 4: CRAN Preparation (v1.0.0)

### 4.1 CRAN Checks
- [ ] 4.1.1 Pass R CMD check with no errors/warnings/notes
- [ ] 4.1.2 Pass rhub checks
- [ ] 4.1.3 Pass goodpractice checks
- [ ] 4.1.4 Spell check all documentation

### 4.2 Final Review
- [ ] 4.2.1 Peer review by 2+ R developers
- [ ] 4.2.2 Address all review comments
- [ ] 4.2.3 Final testing on multiple platforms
- [ ] 4.2.4 Prepare CRAN submission

### 4.3 CRAN Submission
- [ ] 4.3.1 Submit to CRAN
- [ ] 4.3.2 Address CRAN reviewer comments
- [ ] 4.3.3 Package accepted

## Status Legend
- [ ] Todo
- [~] In Progress  
- [x] Completed
- [!] Blocked

## Dependencies Tracking

| Package | Version | Purpose | Status |
|---------|---------|---------|--------|
| rlang | >= 1.0.0 | Tidy evaluation | Required |
| ... | ... | ... | ... |

## Last Updated: 2025-10-11
```

### Step 1.4: Generate SPECS.md

Create `docs/SPECS.md`:

```markdown
# Package Specifications

**Package:** packagename  
**Version:** 0.0.0.9000

## Overview

Brief description of what the package does, the problem it solves, and its main features.

## Target Audience

- Data scientists working with [domain]
- Researchers in [field]
- Developers building [type of applications]

## Features

### Feature 1: [Name]
**Priority**: High  
**Status**: Planning  
**Description**: [2-3 sentence summary]  
**Dependencies**: Base R, rlang

### Feature 2: [Name]
**Priority**: Medium  
**Status**: Planning  
**Description**: [2-3 sentence summary]  
**Dependencies**: Feature 1

## Technical Stack

- **Language**: R (>= 4.0.0)
- **Class System**: S3 / S4 / R6
- **Testing**: testthat (>= 3.0.0)
- **Documentation**: roxygen2, pkgdown
- **Build**: devtools, usethis

## External Dependencies

### Hard Dependencies (Imports)
- rlang (>= 1.0.0) - Tidy evaluation and error handling
- [package] (>= version) - [purpose]

### Soft Dependencies (Suggests)
- testthat (>= 3.0.0) - Testing
- knitr - Vignettes
- rmarkdown - Documentation

## API Design Philosophy

- **Consistency**: Follow tidyverse design principles
- **Simplicity**: Minimize required arguments
- **Flexibility**: Support pipes (`|>` and `%>%`)
- **Type Safety**: Validate inputs early
- **Informative Errors**: Use rlang for clear error messages

## Performance Targets

- Handle datasets up to [size] efficiently
- Operations complete in < [time] for typical use cases
- Memory usage < [limit] for standard workflows

## Compatibility

- R version: >= 4.0.0
- Operating Systems: Windows, macOS, Linux
- RStudio: Compatible but not required
```

### Step 1.5: Generate Feature Specifications

For each feature, create `docs/specs/[feature-name].md`:

```markdown
# Feature: [Feature Name]

## Problem Analysis

### Context
R users need to [describe problem] but existing solutions are [limitations].

### Requirements
1. Function must accept [input types]
2. Return value should be [output type]
3. Performance: O(n) complexity
4. Memory: Linear in input size

### Constraints
- Must work with base R data types
- No compiled code dependencies (or minimal)
- Must pass CRAN checks

### Assumptions
- Input data is already validated at entry points
- Users have R >= 4.0.0 installed

## Technical Architecture

### Function Signature

```r
#' Brief description
#'
#' Detailed description of what the function does.
#'
#' @param x Input data (data.frame, matrix, or vector)
#' @param method Character string specifying method ("a", "b", or "c")
#' @param verbose Logical; if TRUE, print progress messages
#' @param ... Additional arguments passed to internal functions
#'
#' @return An object of class "feature_result" containing:
#'   \item{result}{Processed data}
#'   \item{metadata}{Processing metadata}
#'   \item{call}{Matched call}
#'
#' @export
#' @examples
#' # Basic usage
#' result <- feature_function(mtcars, method = "a")
#' 
#' # With options
#' result <- feature_function(iris, method = "b", verbose = TRUE)
feature_function <- function(x, method = "a", verbose = FALSE, ...) {
  # Implementation
}
```

### Class Structure (if using S3)

```r
# Constructor
new_feature_result <- function(result, metadata, call) {
  structure(
    list(
      result = result,
      metadata = metadata,
      call = call
    ),
    class = "feature_result"
  )
}

# Validator
validate_feature_result <- function(x) {
  if (!is.list(x)) {
    stop("feature_result must be a list", call. = FALSE)
  }
  # Additional validation
  x
}

# User-facing constructor
feature_result <- function(result, metadata, call) {
  validate_feature_result(
    new_feature_result(result, metadata, call)
  )
}
```

### Methods

```r
#' @export
print.feature_result <- function(x, ...) {
  cat("<feature_result>\n")
  cat("Result dimensions:", dim(x$result), "\n")
  # Additional printing
}

#' @export
summary.feature_result <- function(object, ...) {
  # Summary implementation
}

#' @export
plot.feature_result <- function(x, ...) {
  # Plot implementation
}
```

## Testing Strategy

### Unit Tests

Create `tests/testthat/test-feature.R`:

```r
test_that("feature_function handles basic input", {
  result <- feature_function(mtcars, method = "a")
  
  expect_s3_class(result, "feature_result")
  expect_true(is.data.frame(result$result))
  expect_equal(nrow(result$result), nrow(mtcars))
})

test_that("feature_function validates input", {
  expect_error(
    feature_function(NULL),
    "must not be NULL"
  )
  
  expect_error(
    feature_function(mtcars, method = "invalid"),
    "method must be one of"
  )
})

test_that("feature_function handles edge cases", {
  # Empty input
  expect_warning(
    feature_function(data.frame()),
    "empty data"
  )
  
  # Single row
  result <- feature_function(mtcars[1, ])
  expect_equal(nrow(result$result), 1)
  
  # Large input (if applicable)
  big_data <- data.frame(x = 1:10000, y = rnorm(10000))
  expect_silent(feature_function(big_data))
})

test_that("methods work correctly", {
  result <- feature_function(mtcars)
  
  # Print method
  expect_output(print(result), "feature_result")
  
  # Summary method
  summ <- summary(result)
  expect_type(summ, "list")
  
  # Plot method (if applicable)
  expect_silent(plot(result))
})
```

### Integration Tests

```r
test_that("feature integrates with other functions", {
  result1 <- feature_function(mtcars, method = "a")
  result2 <- another_function(result1)
  
  expect_s3_class(result2, "expected_class")
})
```

### Performance Tests

```r
test_that("feature performs efficiently", {
  skip_on_cran()
  
  big_data <- data.frame(
    x = 1:100000,
    y = rnorm(100000)
  )
  
  timing <- system.time(
    result <- feature_function(big_data)
  )
  
  expect_lt(timing["elapsed"], 5)  # Should complete in < 5 seconds
})
```

## Implementation Checklist

- [ ] Function signatures defined
- [ ] Input validation implemented
- [ ] Core logic implemented
- [ ] S3 methods implemented (print, summary, plot)
- [ ] Error handling with rlang
- [ ] Roxygen2 documentation complete
- [ ] Examples added
- [ ] Unit tests written (>95% coverage)
- [ ] Integration tests written
- [ ] Performance tests written
- [ ] Tests passing
- [ ] R CMD check passing (0 errors, 0 warnings, 0 notes)
- [ ] Code formatted with styler
- [ ] Lintr passing
- [ ] NEWS.md updated
- [ ] Vignette created (if major feature)

## Success Criteria

- [ ] All tests pass
- [ ] Test coverage > 95%
- [ ] R CMD check: 0 errors, 0 warnings, 0 notes
- [ ] lintr: 0 issues
- [ ] goodpractice: score > 90%
- [ ] Documentation complete with examples
- [ ] Performance meets targets
- [ ] Peer review approved by 2+ R developers

## References
- [Related R packages]
- [Academic papers]
- [Documentation]
```

---

## Phase 2: Workspace Configuration

### Step 2.1: Configure .Rbuildignore

Create/edit `.Rbuildignore`:

```
^.*\.Rproj$
^\.Rproj\.user$
^LICENSE\.md$
^README\.Rmd$
^\.github$
^docs$
^_pkgdown\.yml$
^pkgdown$
^\.lintr$
^codecov\.yml$
^renv$
^renv\.lock$
^scripts$
```

### Step 2.2: Configure .lintr

Create `.lintr`:

```r
linters: linters_with_defaults(
  line_length_linter(120),
  object_name_linter = NULL,  # Allow snake_case and camelCase
  cyclocomp_linter(25),
  commented_code_linter = NULL
)
exclusions: list(
  "R/RcppExports.R",
  "tests/testthat.R"
)
```

### Step 2.3: Configure styler

Create `scripts/format-code.R`:

```r
#!/usr/bin/env Rscript

# Format all R code using styler
styler::style_pkg(
  transformers = styler::tidyverse_style(indent_by = 2)
)

cat("✓ Code formatting complete\n")
```

### Step 2.4: Setup GitHub Actions

Use `usethis` to add workflows:

```r
# Standard R CMD check across multiple platforms
usethis::use_github_action("check-standard")

# Test coverage with covr
usethis::use_github_action("test-coverage")

# Build and deploy pkgdown site
usethis::use_github_action("pkgdown")

# Optional: PR commands
usethis::use_github_action("pr-commands")
```

Or manually create `.github/workflows/R-CMD-check.yaml`:

```yaml
on:
  push:
    branches: [main, develop]
  pull_request:
    branches: [main, develop]

name: R-CMD-check

jobs:
  R-CMD-check:
    runs-on: ${{ matrix.config.os }}

    name: ${{ matrix.config.os }} (${{ matrix.config.r }})

    strategy:
      fail-fast: false
      matrix:
        config:
          - {os: macos-latest,   r: 'release'}
          - {os: windows-latest, r: 'release'}
          - {os: ubuntu-latest,   r: 'devel', http-user-agent: 'release'}
          - {os: ubuntu-latest,   r: 'release'}
          - {os: ubuntu-latest,   r: 'oldrel-1'}

    env:
      GITHUB_PAT: ${{ secrets.GITHUB_TOKEN }}
      R_KEEP_PKG_SOURCE: yes

    steps:
      - uses: actions/checkout@v3

      - uses: r-lib/actions/setup-pandoc@v2

      - uses: r-lib/actions/setup-r@v2
        with:
          r-version: ${{ matrix.config.r }}
          http-user-agent: ${{ matrix.config.http-user-agent }}
          use-public-rspm: true

      - uses: r-lib/actions/setup-r-dependencies@v2
        with:
          extra-packages: any::rcmdcheck
          needs: check

      - uses: r-lib/actions/check-r-package@v2
        with:
          upload-snapshots: true
```

### Step 2.5: Setup Package Documentation

```r
# Create package-level documentation
usethis::use_package_doc()

# Create pkgdown configuration
usethis::use_pkgdown()

# Customize _pkgdown.yml
```

Edit `_pkgdown.yml`:

```yaml
url: https://username.github.io/packagename/

template:
  bootstrap: 5
  bootswatch: cosmo

reference:
  - title: Main Functions
    desc: Core functionality
    contents:
    - feature_function
    - another_function
  
  - title: Helper Functions
    desc: Utility functions
    contents:
    - has_concept("helpers")
  
  - title: Data
    desc: Package datasets
    contents:
    - has_concept("datasets")

articles:
  - title: Get Started
    contents:
    - packagename
  
  - title: Use Cases
    contents:
    - use-case-1
    - use-case-2

navbar:
  structure:
    left:  [intro, reference, articles, tutorials, news]
    right: [search, github]
  components:
    articles:
      text: Articles
      menu:
      - text: Getting Started
        href: articles/packagename.html
```

---

## Phase 3: Implementation

### Step 3.1: Package-Level Documentation

Create `R/package.R`:

```r
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
```

### Step 3.2: Implementing Functions

General structure for `R/[feature].R`:

```r
#' Feature Function
#'
#' Detailed description of what this function does. Can span multiple lines
#' and include details about the algorithm, special cases, etc.
#'
#' @param x A data frame, matrix, or vector to process
#' @param method Character string specifying the method. Options:
#'   \itemize{
#'     \item "a": First method (default)
#'     \item "b": Second method
#'     \item "c": Third method
#'   }
#' @param threshold Numeric threshold value between 0 and 1. Default is 0.5.
#' @param verbose Logical; if \code{TRUE}, print progress messages
#' @param ... Additional arguments passed to internal functions
#'
#' @return An object of class \code{feature_result} which is a list containing:
#'   \item{result}{Processed data}
#'   \item{metadata}{List of processing metadata}
#'   \item{params}{List of parameters used}
#'   \item{call}{The matched call}
#'
#' @details
#' This function processes input data using the specified method. The algorithm
#' consists of three main steps:
#' \enumerate{
#'   \item Input validation and preprocessing
#'   \item Core processing using selected method
#'   \item Result formatting and metadata collection
#' }
#'
#' @section Methods:
#' The available methods are:
#' \describe{
#'   \item{a}{Fast but less accurate}
#'   \item{b}{Balanced speed and accuracy}
#'   \item{c}{Most accurate but slower}
#' }
#'
#' @section Performance:
#' Time complexity is O(n) for methods "a" and "b", and O(n log n) for method "c".
#'
#' @seealso
#' \code{\link{related_function}} for related functionality
#' \code{\link{another_function}} for alternative approaches
#'
#' @export
#' @examples
#' # Basic usage
#' result <- feature_function(mtcars)
#' print(result)
#'
#' # With custom method
#' result <- feature_function(iris, method = "b", threshold = 0.7)
#' summary(result)
#'
#' # Verbose mode
#' result <- feature_function(airquality, verbose = TRUE)
#'
#' # Using with pipes
#' library(dplyr)
#' mtcars %>%
#'   feature_function(method = "c") %>%
#'   summary()
feature_function <- function(x,
                              method = c("a", "b", "c"),
                              threshold = 0.5,
                              verbose = FALSE,
                              ...) {
  
  # Match arguments
  method <- match.arg(method)
  
  # Validate inputs
  if (is.null(x)) {
    rlang::abort(
      "Argument `x` must not be NULL",
      class = "feature_error_null_input"
    )
  }
  
  if (!is.numeric(threshold) || threshold < 0 || threshold > 1) {
    rlang::abort(
      "Argument `threshold` must be numeric between 0 and 1",
      class = "feature_error_invalid_threshold"
    )
  }
  
  # Store call
  call <- match.call()
  
  # Process based on method
  if (verbose) message("Processing with method: ", method)
  
  result <- switch(
    method,
    a = process_method_a(x, threshold, ...),
    b = process_method_b(x, threshold, ...),
    c = process_method_c(x, threshold, ...)
  )
  
  # Collect metadata
  metadata <- list(
    method = method,
    threshold = threshold,
    n_obs = NROW(x),
    timestamp = Sys.time()
  )
  
  # Create result object
  structure(
    list(
      result = result,
      metadata = metadata,
      params = list(method = method, threshold = threshold),
      call = call
    ),
    class = "feature_result"
  )
}

# Internal processing functions
process_method_a <- function(x, threshold, ...) {
  # Implementation
}

process_method_b <- function(x, threshold, ...) {
  # Implementation
}

process_method_c <- function(x, threshold, ...) {
  # Implementation
}

#' @export
print.feature_result <- function(x, ...) {
  cat("<feature_result>\n")
  cat("Method:", x$metadata$method, "\n")
  cat("Observations:", x$metadata$n_obs, "\n")
  cat("Threshold:", x$params$threshold, "\n")
  invisible(x)
}

#' @export
summary.feature_result <- function(object, ...) {
  cat("Feature Result Summary\n")
  cat("======================\n\n")
  
  cat("Processing Details:\n")
  cat("  Method:", object$metadata$method, "\n")
  cat("  Threshold:", object$params$threshold, "\n")
  cat("  Observations:", object$metadata$n_obs, "\n")
  cat("  Timestamp:", format(object$metadata$timestamp), "\n\n")
  
  cat("Result Statistics:\n")
  print(summary(object$result))
  
  invisible(object)
}

#' @export
plot.feature_result <- function(x, ...) {
  # Basic plot implementation
  plot(x$result, 
       main = paste("Feature Result -", x$metadata$method),
       ...)
}
```

### Step 3.3: Data Documentation

If package includes data, create `R/data.R`:

```r
#' Example Dataset
#'
#' A dataset containing example data for demonstrating package functionality.
#'
#' @format A data frame with 100 rows and 5 variables:
#' \describe{
#'   \item{id}{Unique identifier, integer}
#'   \item{group}{Grouping variable, factor with levels A, B, C}
#'   \item{value}{Numeric measurement}
#'   \item{category}{Categorical variable, factor}
#'   \item{timestamp}{Date-time of observation, POSIXct}
#' }
#' @source Generated synthetically for package examples
#' @examples
#' data(example_data)
#' head(example_data)
#' summary(example_data)
"example_data"
```

Create data file in `data-raw/example_data.R`:

```r
# Create example dataset
set.seed(123)

example_data <- data.frame(
  id = 1:100,
  group = factor(sample(c("A", "B", "C"), 100, replace = TRUE)),
  value = rnorm(100, mean = 50, sd = 10),
  category = factor(sample(c("Low", "Medium", "High"), 100, replace = TRUE)),
  timestamp = as.POSIXct("2025-01-01") + seq(0, by = 3600, length.out = 100)
)

# Save to data/
usethis::use_data(example_data, overwrite = TRUE)
```

### Step 3.4: Using Dependencies

```r
# Add to Imports
usethis::use_package("rlang")
usethis::use_package("dplyr", min_version = "1.0.0")

# Add to Suggests
usethis::use_package("testthat", type = "Suggests")
usethis::use_package("knitr", type = "Suggests")

# Use pipe (if needed)
usethis::use_pipe()  # Adds magrittr %>%

# Use specific functions without importing entire package
usethis::use_import_from("stats", c("cor", "sd", "mean"))
```

In code, use `package::function()` notation or import via roxygen2:

```r
#' @importFrom rlang abort inform warn
#' @importFrom stats cor sd mean
NULL
```

---

## Phase 4: Testing

### Step 4.1: Test Structure

```r
# tests/testthat.R
# DO NOT MODIFY - Generated by usethis
library(testthat)
library(packagename)

test_check("packagename")
```

### Step 4.2: Writing Tests

Create `tests/testthat/test-feature.R`:

```r
# Test structure: arrange, act, assert

test_that("feature_function works with basic input", {
  # Arrange
  data <- mtcars
  
  # Act
  result <- feature_function(data, method = "a")
  
  # Assert
  expect_s3_class(result, "feature_result")
  expect_type(result$result, "list")
  expect_equal(result$metadata$method, "a")
})

test_that("feature_function validates arguments", {
  # NULL input
  expect_error(
    feature_function(NULL),
    class = "feature_error_null_input"
  )
  
  # Invalid method
  expect_error(
    feature_function(mtcars, method = "invalid"),
    "should be one of"
  )
  
  # Invalid threshold
  expect_error(
    feature_function(mtcars, threshold = -1),
    class = "feature_error_invalid_threshold"
  )
  
  expect_error(
    feature_function(mtcars, threshold = 2),
    class = "feature_error_invalid_threshold"
  )
})

test_that("feature_function handles edge cases", {
  # Empty data
  empty_df <- data.frame()
  expect_warning(
    feature_function(empty_df),
    "empty"
  )
  
  # Single row
  single_row <- mtcars[1, , drop = FALSE]
  result <- feature_function(single_row)
  expect_equal(result$metadata$n_obs, 1)
  
  # NA values
  data_with_na <- mtcars
  data_with_na[1, 1] <- NA
  expect_warning(
    feature_function(data_with_na),
    "missing values"
  )
})

test_that("feature_function methods work correctly", {
  result <- feature_function(mtcars)
  
  # Test each method
  for (method in c("a", "b", "c")) {
    result <- feature_function(mtcars, method = method)
    expect_equal(result$metadata$method, method)
  }
})

test_that("S3 methods work", {
  result <- feature_function(mtcars)
  
  # print
  expect_output(print(result), "feature_result")
  expect_output(print(result), "Method:")
  
  # summary
  expect_output(summary(result), "Feature Result Summary")
  
  # plot (check no errors)
  expect_silent(plot(result))
})

test_that("verbose mode works", {
  expect_message(
    feature_function(mtcars, verbose = TRUE),
    "Processing with method"
  )
})

test_that("additional arguments are passed correctly", {
  # If function accepts ...
  result <- feature_function(mtcars, method = "a", extra_arg = TRUE)
  expect_s3_class(result, "feature_result")
})
```

### Step 4.3: Test Fixtures

Create `tests/testthat/fixtures/`:

```r
# tests/testthat/fixtures/make-test-data.R

make_test_data <- function(n = 100, seed = 123) {
  set.seed(seed)
  data.frame(
    x = rnorm(n),
    y = rnorm(n),
    group = sample(letters[1:3], n, replace = TRUE)
  )
}

# Save fixture
test_data <- make_test_data()
saveRDS(test_data, "tests/testthat/fixtures/test_data.rds")
```

Use in tests:

```r
test_that("function works with fixture data", {
  test_data <- readRDS("fixtures/test_data.rds")
  result <- feature_function(test_data)
  expect_s3_class(result, "feature_result")
})
```

### Step 4.4: Snapshot Tests

For complex outputs:

```r
test_that("output format is stable", {
  result <- feature_function(mtcars)
  expect_snapshot(print(result))
  expect_snapshot(summary(result))
})
```

### Step 4.5: Running Tests

```r
# Run all tests
devtools::test()

# Run specific file
devtools::test_file("tests/testthat/test-feature.R")

# Run with coverage
covr::package_coverage()

# Interactive coverage report
covr::report()
```

Or via command line:

```bash
Rscript -e "devtools::test()"
Rscript -e "covr::package_coverage()"
```

---

## Phase 5: Documentation

### Step 5.1: Generate Documentation

```r
# Generate man/ files from roxygen2 comments
devtools::document()

# Check documentation
devtools::check_man()
```

### Step 5.2: Create Vignettes

```r
# Create vignette
usethis::use_vignette("packagename-intro", "Introduction to packagename")
```

Edit `vignettes/packagename-intro.Rmd`:

```rmd
---
title: "Introduction to packagename"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Introduction to packagename}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)
```

## Introduction

This vignette introduces the basic functionality of `packagename`.

## Installation

```{r eval=FALSE}
# From CRAN
install.packages("packagename")

# Development version from GitHub
# install.packages("remotes")
remotes::install_github("username/packagename")
```

## Basic Usage

```{r setup}
library(packagename)
```

### Example 1: Basic Functionality

```{r}
# Load example data
data(example_data)
head(example_data)

# Apply feature function
result <- feature_function(example_data, method = "a")
print(result)
```

### Example 2: Different Methods

```{r}
# Compare methods
result_a <- feature_function(example_data, method = "a")
result_b <- feature_function(example_data, method = "b")
result_c <- feature_function(example_data, method = "c")

# Summarize
summary(result_a)
```

### Example 3: Visualization

```{r}
# Plot results
plot(result_a)
```

## Advanced Usage

### Custom Thresholds

```{r}
result <- feature_function(
  example_data, 
  method = "b",
  threshold = 0.7,
  verbose = TRUE
)
```

### Integration with tidyverse

```{r eval=FALSE}
library(dplyr)
library(ggplot2)

result <- example_data %>%
  feature_function(method = "c") %>%
  summary()
```

## Conclusion

This vignette covered the basic usage of `packagename`. For more details, see:

- `?feature_function` for function documentation
- Additional vignettes for advanced use cases
- GitHub issues for questions and bug reports
```

### Step 5.3: Build Package Website

```r
# Build pkgdown site locally
pkgdown::build_site()

# Preview
pkgdown::preview_site()

# Build specific components
pkgdown::build_reference()
pkgdown::build_articles()
pkgdown::build_news()
```

---

## Phase 6: Package Building & Distribution

### Step 6.1: Run R CMD check

```r
# Full check
devtools::check()

# Quick check (skip examples, vignettes)
devtools::check(args = "--no-examples", vignettes = FALSE)

# Check as CRAN
devtools::check(
  remote = TRUE,
  manual = TRUE,
  cran = TRUE
)
```

Command line:

```bash
R CMD build .
R CMD check packagename_0.1.0.tar.gz --as-cran
```

### Step 6.2: Check on Multiple Platforms

```r
# Check on R-hub (multiple platforms)
rhub::check_for_cran()

# Check on Windows
rhub::check_on_windows()

# Check on macOS
rhub::check_on_macos()

# Check on Linux
rhub::check_on_linux()

# Check with additional options
rhub::check(
  platform = c("ubuntu-gcc-release", "windows-x86_64-devel"),
  env_vars = c(R_COMPILE_AND_INSTALL_PACKAGES = "always")
)
```

### Step 6.3: Check Best Practices

```r
# Run goodpractice checks
goodpractice::gp()

# Check spelling
spelling::spell_check_package()

# Update word list
spelling::update_wordlist()
```

### Step 6.4: Build Package

```r
# Build source package
devtools::build()

# Build binary package
devtools::build(binary = TRUE)

# Build without vignettes (faster)
devtools::build(vignettes = FALSE)
```

### Step 6.5: Local Installation

```r
# Install from source
devtools::install()

# Install with all dependencies
devtools::install(dependencies = TRUE)

# Install and reload
devtools::load_all()  # During development
```

---

## Phase 7: Review & Quality Assurance

### Step 7.1: Pre-Submission Checklist

Create `docs/reviews/pre-submission-checklist.md`:

```markdown
# Pre-Submission Checklist

**Package:** packagename  
**Version:** 0.1.0  
**Date:** 2025-10-11  
**Reviewer:** [Agent ID]

## R CMD check

- [ ] R CMD check passes with 0 errors
- [ ] R CMD check passes with 0 warnings
- [ ] R CMD check passes with 0 notes (or justified notes documented)
- [ ] Checked on Windows (rhub or win-builder)
- [ ] Checked on macOS (rhub)
- [ ] Checked on Linux (rhub)
- [ ] Checked with r-devel
- [ ] Checked with r-oldrel

## Documentation

- [ ] All exported functions have roxygen2 documentation
- [ ] All parameters documented with @param
- [ ] Return values documented with @return
- [ ] Examples provided for all exported functions
- [ ] Examples run successfully
- [ ] At least one vignette provided
- [ ] README.md is informative and up-to-date
- [ ] NEWS.md documents all changes
- [ ] pkgdown site builds successfully

## Testing

- [ ] Test coverage > 90%
- [ ] All tests pass
- [ ] Tests cover edge cases
- [ ] Tests cover error conditions
- [ ] No skip_on_cran() without justification

## Code Quality

- [ ] No lintr warnings (or justified exceptions)
- [ ] Code formatted with styler
- [ ] No commented-out code
- [ ] No browser() or debug statements
- [ ] goodpractice score > 90%

## DESCRIPTION File

- [ ] Title is in title case
- [ ] Description is clear and detailed
- [ ] Authors@R is correct
- [ ] License is specified correctly
- [ ] URL points to package website
- [ ] BugReports points to issues page
- [ ] All Imports are actually used
- [ ] All Suggests are used in tests/vignettes
- [ ] Version number follows semantic versioning

## Dependencies

- [ ] Minimal dependencies (justified)
- [ ] All dependencies on CRAN (or Bioconductor)
- [ ] No deprecated dependencies
- [ ] Dependency versions specified when needed

## NAMESPACE

- [ ] Only exports necessary functions
- [ ] Imports specified correctly
- [ ] No conflicts with common packages

## Files and Structure

- [ ] .Rbuildignore is correct
- [ ] .gitignore is correct
- [ ] No unnecessary files in package
- [ ] inst/ directory used appropriately
- [ ] data/ contains only .rda files
- [ ] data-raw/ documented

## Spelling

- [ ] Spell check passed
- [ ] WORDLIST updated if needed
- [ ] No typos in documentation

## License

- [ ] LICENSE file present and correct
- [ ] Copyright notices present
- [ ] Third-party code properly attributed

## Vignettes

- [ ] Vignettes build successfully
- [ ] Vignettes are informative
- [ ] Code in vignettes runs
- [ ] Figures display correctly

## Performance

- [ ] No performance regressions
- [ ] Memory usage reasonable
- [ ] Examples complete in reasonable time

## Platform Compatibility

- [ ] Works on Windows
- [ ] Works on macOS  
- [ ] Works on Linux
- [ ] No platform-specific code (or properly guarded)

## Special Cases

- [ ] No internet usage (or properly handled)
- [ ] No file system writes outside tempdir()
- [ ] No use of .Rprofile or .Renviron
- [ ] No modification of user options (or restored)
- [ ] No parallel code (or properly handled)

## Submission Materials

- [ ] cran-comments.md prepared
- [ ] Reverse dependency check complete (if applicable)
- [ ] Maintainer email verified
```

### Step 7.2: Peer Review Request

Create `docs/reviews/peer-review-request.md`:

```markdown
# Peer Review Request

**Package:** packagename  
**Version:** 0.1.0  
**Requested by:** [Agent ID]  
**Date:** 2025-10-11

## Review Context

This package is ready for peer review before CRAN submission.

## Package Summary

[2-3 paragraph description of package purpose and functionality]

## Review Focus Areas

1. **Code Quality**: R best practices, efficiency
2. **API Design**: Function interfaces, consistency
3. **Documentation**: Completeness and clarity
4. **Testing**: Coverage and thoroughness
5. **CRAN Readiness**: Compliance with CRAN policies

## Files for Review

- Source code: `R/*.R`
- Tests: `tests/testthat/*.R`
- Documentation: `man/*.Rd`, `vignettes/*.Rmd`
- Configuration: `DESCRIPTION`, `NAMESPACE`

## Timeline

- Review deadline: 2025-10-15
- Response to feedback: 2025-10-18
- Target submission: 2025-10-20

## Review Checklist

Reviewers should assess:

- [ ] Code follows tidyverse style guide
- [ ] Functions are well-designed and composable
- [ ] Error handling is appropriate
- [ ] Documentation is clear and complete
- [ ] Tests are comprehensive
- [ ] Package is CRAN-ready

## How to Review

1. Install package: `remotes::install_github("username/packagename@review-branch")`
2. Run checks: `devtools::check()`
3. Review source code
4. Try examples and vignettes
5. Provide feedback in `docs/reviews/peer-review-[reviewer-name].md`
```

### Step 7.3: Peer Review Report Template

Create `docs/reviews/peer-review-template.md`:

```markdown
# Peer Review Report

**Reviewer:** [Agent ID]  
**Package:** packagename  
**Version:** 0.1.0  
**Review Date:** 2025-10-11

## Executive Summary

[2-3 paragraph overview of findings and recommendation]

## Detailed Review

### Code Quality (Score: X/10)

**Strengths:**
- [Strength 1]
- [Strength 2]

**Issues:**
1. **[Severity]** [Issue description]
   - Location: `R/file.R:line`
   - Recommendation: [How to fix]

### API Design (Score: X/10)

**Strengths:**
- [Strength 1]

**Issues:**
- [Issue 1]

### Documentation (Score: X/10)

**Strengths:**
- [Strength 1]

**Issues:**
- [Issue 1]

### Testing (Score: X/10)

- Test coverage: X%
- Number of tests: X
- Tests passing: X/X

**Issues:**
- [Issue 1]

### CRAN Readiness (Score: X/10)

- R CMD check: [Result]
- rhub checks: [Result]
- goodpractice: [Score]

**Issues:**
- [Issue 1]

## Recommendation

- [ ] **APPROVE** - Ready for CRAN submission
- [ ] **APPROVE WITH MINOR CHANGES** - Address minor issues first
- [ ] **REVISION REQUIRED** - Significant issues need fixing
- [ ] **REJECT** - Fundamental problems

## Priority Issues

### Must Fix
1. [Issue 1]
2. [Issue 2]

### Should Fix
1. [Issue 1]
2. [Issue 2]

### Nice to Have
1. [Suggestion 1]
2. [Suggestion 2]

## Additional Comments

[Any other observations]

---

**Reviewer Signature:** [Agent ID]  
**Date:** 2025-10-11
```

---

## R Best Practices

### Naming Conventions

```r
# Functions: snake_case
calculate_mean <- function(x) { }
process_data <- function(data) { }

# Variables: snake_case
user_input <- 10
result_data <- process_data(my_data)

# Constants: SCREAMING_SNAKE_CASE
DEFAULT_THRESHOLD <- 0.5
MAX_ITERATIONS <- 1000

# Classes: snake_case or PascalCase (consistent within package)
new_my_class <- function() { }
# or
new_MyClass <- function() { }

# Private functions: start with dot
.internal_helper <- function(x) { }
```

### Function Design

```r
# Good: Clear parameters, documented, returns appropriate type
calculate_statistics <- function(x, 
                                  na.rm = FALSE,
                                  method = c("mean", "median"),
                                  ...) {
  method <- match.arg(method)
  
  if (!is.numeric(x)) {
    rlang::abort("`x` must be numeric", class = "invalid_input")
  }
  
  result <- switch(
    method,
    mean = mean(x, na.rm = na.rm),
    median = median(x, na.rm = na.rm)
  )
  
  structure(
    list(value = result, method = method, n = length(x)),
    class = "statistic_result"
  )
}

# Bad: Unclear purpose, no validation, side effects
do_stuff <- function(x, y) {
  print(x)  # Side effect
  x + y     # No validation
}
```

### Error Handling

```r
# Use rlang for better error messages
library(rlang)

validate_input <- function(x, threshold) {
  # Check for NULL
  if (is.null(x)) {
    abort(
      "`x` must not be NULL",
      class = "error_null_input"
    )
  }
  
  # Check type
  if (!is.numeric(x)) {
    abort(
      sprintf("`x` must be numeric, not %s", class(x)[1]),
      class = "error_wrong_type"
    )
  }
  
  # Check threshold
  if (!is.numeric(threshold) || length(threshold) != 1) {
    abort(
      "`threshold` must be a single numeric value",
      class = "error_invalid_threshold"
    )
  }
  
  if (threshold < 0 || threshold > 1) {
    abort(
      sprintf("`threshold` must be between 0 and 1, not %.2f", threshold),
      class = "error_threshold_out_of_range"
    )
  }
}

# Warnings for recoverable issues
process_with_na <- function(x) {
  if (anyNA(x)) {
    warn(
      "Input contains missing values; these will be removed",
      class = "warning_na_values"
    )
    x <- x[!is.na(x)]
  }
  x
}

# Messages for information
process_verbose <- function(x, verbose = FALSE) {
  if (verbose) {
    inform(sprintf("Processing %d observations", length(x)))
  }
  # Process x
}
```

### Performance

```r
# Good: Vectorized operations
calculate_distances <- function(x, y) {
  sqrt((x[, 1] - y[1])^2 + (x[, 2] - y[2])^2)
}

# Bad: Loops when vectorization possible
calculate_distances_bad <- function(x, y) {
  distances <- numeric(nrow(x))
  for (i in seq_len(nrow(x))) {
    distances[i] <- sqrt((x[i, 1] - y[1])^2 + (x[i, 2] - y[2])^2)
  }
  distances
}

# Preallocate vectors
good_accumulate <- function(n) {
  result <- numeric(n)
  for (i in seq_len(n)) {
    result[i] <- i^2
  }
  result
}

# Bad: Growing vectors
bad_accumulate <- function(n) {
  result <- numeric(0)
  for (i in seq_len(n)) {
    result <- c(result, i^2)  # Avoid!
  }
  result
}
```

### Memory Management

```r
# Process large datasets in chunks
process_large_data <- function(file_path, chunk_size = 10000) {
  con <- file(file_path, "r")
  on.exit(close(con))
  
  results <- list()
  chunk_num <- 1
  
  repeat {
    chunk <- readLines(con, n = chunk_size)
    if (length(chunk) == 0) break
    
    results[[chunk_num]] <- process_chunk(chunk)
    chunk_num <- chunk_num + 1
  }
  
  do.call(rbind, results)
}
```

---

## CRAN Submission Guidelines

### Step 1: Prepare cran-comments.md

Create `cran-comments.md`:

```markdown
## Test environments

* local: Windows 10, R 4.3.2
* GitHub Actions:
  - windows-latest (release)
  - macOS-latest (release)
  - ubuntu-latest (devel, release, oldrel-1)
* rhub:
  - Windows Server 2022 (devel)
  - Ubuntu Linux 20.04 (release)
  - Fedora Linux (devel)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Additional comments

This is the initial release of packagename.
```

### Step 2: Check CRAN Policies

Ensure compliance:

- [ ] Package size < 5MB (or justified)
- [ ] No internet access without checks
- [ ] No file writes outside tempdir()
- [ ] Examples run in < 5 seconds
- [ ] No modification of user environment
- [ ] License is CRAN-compatible
- [ ] No non-ASCII characters (or justified)

### Step 3: Submit to CRAN

```r
# Final check
devtools::check(cran = TRUE)

# Submit (first time)
devtools::submit_cran()

# Or manually:
# 1. Build source: devtools::build()
# 2. Upload to https://cran.r-project.org/submit.html
```

### Step 4: Respond to CRAN Feedback

If CRAN requests changes:

1. Address all issues
2. Increment version number (patch level)
3. Update cran-comments.md with response
4. Resubmit

---

## Bioconductor Guidelines

If submitting to Bioconductor instead of CRAN:

### Requirements

- [ ] Package provides bioinformatics functionality
- [ ] Follows Bioconductor code standards
- [ ] Uses BiocCheck::BiocCheck()
- [ ] Vignette uses BiocStyle
- [ ] Includes NEWS file
- [ ] Version number: 0.99.x for new packages

### Submission Process

```r
# Install BiocCheck
BiocManager::install("BiocCheck")

# Run BiocCheck
BiocCheck::BiocCheck(".")

# Address all ERROR, WARNING, and NOTE items

# Submit via GitHub pull request to Bioconductor/Contributions
```

---

## Quick Reference

### Essential Commands

```r
# Setup
usethis::create_package("packagename")
usethis::use_git()
usethis::use_testthat()
usethis::use_package("rlang")

# Development
devtools::load_all()       # Load package for testing
devtools::document()       # Generate documentation
devtools::test()           # Run tests
devtools::check()          # R CMD check

# Documentation
usethis::use_vignette("intro")
pkgdown::build_site()

# Quality
lintr::lint_package()
styler::style_pkg()
covr::package_coverage()
goodpractice::gp()

# Submission
devtools::check(cran = TRUE)
rhub::check_for_cran()
devtools::submit_cran()
```

### Common Workflows

```bash
# Daily development
Rscript -e "devtools::load_all(); devtools::test()"

# Pre-commit
Rscript -e "styler::style_pkg(); devtools::document(); devtools::test()"

# Pre-submission
Rscript -e "devtools::check(); rhub::check_for_cran(); goodpractice::gp()"
```

### Directory Structure Checklist

```
- [ ] R/ (source code)
- [ ] man/ (documentation, auto-generated)
- [ ] tests/testthat/ (tests)
- [ ] vignettes/ (long-form docs)
- [ ] data/ (.rda files)
- [ ] data-raw/ (data generation scripts)
- [ ] DESCRIPTION
- [ ] NAMESPACE (auto-generated)
- [ ] README.md
- [ ] NEWS.md
- [ ] LICENSE
- [ ] .Rbuildignore
- [ ] .gitignore
```

---

## Troubleshooting

### Common Issues

**Issue**: R CMD check shows "Undefined global functions or variables"

**Solution**: Add to package documentation:

```r
# R/utils.R
utils::globalVariables(c("variable1", "variable2"))
```

Or use `.data$variable` from rlang in dplyr chains.

---

**Issue**: Vignettes fail to build

**Solution**: Check that:
- VignetteBuilder: knitr in DESCRIPTION
- Vignette header is correct
- All required packages in Suggests

---

**Issue**: Tests fail on CRAN but pass locally

**Solution**:
- Check for absolute paths
- Check for internet dependencies
- Check for platform-specific code
- Use skip_on_cran() if necessary

---

**Issue**: Package size too large

**Solution**:
- Compress data files
- Move large data to separate package
- Use inst/extdata for external data
- Check for unnecessary files in .Rbuildignore

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-10-11 | Initial R manual creation |

---

## Resources

### Official Documentation
- [Writing R Extensions](https://cran.r-project.org/doc/manuals/r-release/R-exts.html)
- [CRAN Repository Policy](https://cran.r-project.org/web/packages/policies.html)
- [Tidyverse Style Guide](https://style.tidyverse.org/)

### Books
- [R Packages (2e)](https://r-pkgs.org/) by Hadley Wickham and Jenny Bryan
- [Advanced R](https://adv-r.hadley.nz/) by Hadley Wickham

### Tools Documentation
- [devtools](https://devtools.r-lib.org/)
- [usethis](https://usethis.r-lib.org/)
- [testthat](https://testthat.r-lib.org/)
- [roxygen2](https://roxygen2.r-lib.org/)
- [pkgdown](https://pkgdown.r-lib.org/)

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

