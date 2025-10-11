# R Best Practices Guide

**Version:** 1.0.0  
**Last Updated:** 2025-10-11  
**Target Audience:** AI Agents developing R packages  
**Compliance:** Tidyverse Style Guide, CRAN Policies

---

## Table of Contents

1. [Code Style](#code-style)
2. [Function Design](#function-design)
3. [Object-Oriented Programming](#object-oriented-programming)
4. [Error Handling](#error-handling)
5. [Performance](#performance)
6. [Memory Management](#memory-management)
7. [Documentation](#documentation)
8. [Testing](#testing)
9. [Package Structure](#package-structure)
10. [Dependencies](#dependencies)
11. [Data Handling](#data-handling)
12. [Security](#security)
13. [Anti-Patterns](#anti-patterns)

---

## Code Style

### File Organization

```r
# Good: Organized structure
# 1. Package documentation
# 2. Imports
# 3. Constants
# 4. Main functions
# 5. Helper functions
# 6. S3 methods

#' @importFrom rlang abort warn inform
#' @importFrom stats na.omit sd
NULL

# Constants
DEFAULT_THRESHOLD <- 0.5
MAX_ITERATIONS <- 1000

# Main function
main_function <- function(x, y) {
  # Implementation
}

# Helper function
.helper_function <- function(x) {
  # Implementation
}
```

### Naming Conventions

```r
# Functions and variables: snake_case
calculate_mean <- function(data) { }
user_input <- 42
result_data <- process(input)

# Constants: SCREAMING_SNAKE_CASE
MAX_VALUE <- 100
DEFAULT_COLOR <- "blue"

# Private functions: prefix with dot
.internal_helper <- function(x) { }
.validate_input <- function(x) { }

# Classes: snake_case (S3) or PascalCase (R6)
new_my_result <- function() { }  # S3
MyClass <- R6::R6Class("MyClass")  # R6

# Avoid:
myFunction <- function() { }  # camelCase
my.function <- function() { }  # dots (reserved for S3)
```

### Spacing and Indentation

```r
# Good: Proper spacing
x <- 5
y <- x + 2
result <- calculate_mean(x, y, na.rm = TRUE)

if (x > 0) {
  print("positive")
} else {
  print("non-positive")
}

# Bad: No spacing
x<-5
y<-x+2
result<-calculate_mean(x,y,na.rm=TRUE)

if(x>0){
  print("positive")
}else{
  print("non-positive")
}

# Indentation: 2 spaces (not tabs)
my_function <- function(x, y) {
  if (x > 0) {
    result <- x + y
  } else {
    result <- x - y
  }
  result
}
```

### Line Length

```r
# Maximum 80 characters per line

# Good: Break long lines
result <- very_long_function_name(
  parameter1 = value1,
  parameter2 = value2,
  parameter3 = value3
)

# Good: Break long pipes
data %>%
  filter(category == "A") %>%
  group_by(group) %>%
  summarise(mean_value = mean(value))

# Bad: Too long
result <- very_long_function_name(parameter1 = value1, parameter2 = value2, parameter3 = value3, parameter4 = value4)
```

### Assignment Operator

```r
# Use <- for assignment (not =)

# Good
x <- 5
result <- calculate(x)

# Bad
x = 5
result = calculate(x)

# Exception: = for function arguments
my_function(x = 5, y = 10)
```

---

## Function Design

### Function Length

```r
# Keep functions focused and short (< 50 lines ideal)

# Good: Single responsibility
calculate_mean <- function(x, na.rm = FALSE) {
  if (!is.numeric(x)) {
    abort("`x` must be numeric")
  }
  mean(x, na.rm = na.rm)
}

# Bad: Multiple responsibilities
process_everything <- function(x) {
  # 100 lines of unrelated operations
}
```

### Default Arguments

```r
# Good: Sensible defaults
process_data <- function(data,
                         method = "standard",
                         threshold = 0.5,
                         na.rm = FALSE,
                         verbose = FALSE) {
  # Implementation
}

# Use match.arg for multiple choice defaults
calculate <- function(x, method = c("mean", "median", "mode")) {
  method <- match.arg(method)
  
  switch(
    method,
    mean = mean(x),
    median = median(x),
    mode = calculate_mode(x)
  )
}
```

### Return Values

```r
# Explicit returns for clarity

# Good: Explicit return
calculate_stats <- function(x) {
  result <- list(
    mean = mean(x),
    sd = sd(x),
    n = length(x)
  )
  return(result)
}

# Also good: Implicit return (last expression)
calculate_stats <- function(x) {
  list(
    mean = mean(x),
    sd = sd(x),
    n = length(x)
  )
}

# Bad: Unclear what is returned
calculate_stats <- function(x) {
  mean_val <- mean(x)
  sd_val <- sd(x)
  n <- length(x)
}
```

### Side Effects

```r
# Avoid side effects; use messages/warnings explicitly

# Good: Explicit communication
process_data <- function(data, verbose = FALSE) {
  if (verbose) {
    message("Processing ", nrow(data), " rows")
  }
  
  # Process data
  result <- transform_data(data)
  
  if (anyNA(result)) {
    warning("Result contains missing values")
  }
  
  result
}

# Bad: Unexpected side effects
process_data <- function(data) {
  print("Processing...")  # Side effect
  Sys.sleep(1)            # Side effect
  # Process data
}
```

---

## Object-Oriented Programming

### S3 Classes (Recommended for Most Cases)

```r
# Constructor (internal)
new_my_result <- function(data, metadata) {
  structure(
    list(
      data = data,
      metadata = metadata
    ),
    class = "my_result"
  )
}

# Validator
validate_my_result <- function(x) {
  if (!is.list(x)) {
    abort("`my_result` must be a list")
  }
  
  if (!all(c("data", "metadata") %in% names(x))) {
    abort("`my_result` must have 'data' and 'metadata' components")
  }
  
  if (!is.data.frame(x$data)) {
    abort("`data` component must be a data.frame")
  }
  
  x
}

# User-facing constructor
my_result <- function(data, metadata = list()) {
  # Validate inputs
  if (!is.data.frame(data)) {
    abort("`data` must be a data.frame")
  }
  
  # Create and validate
  result <- new_my_result(data, metadata)
  validate_my_result(result)
}

# Methods
#' @export
print.my_result <- function(x, ...) {
  cat("<my_result>\n")
  cat("Data: ", nrow(x$data), " x ", ncol(x$data), "\n", sep = "")
  cat("Metadata: ", length(x$metadata), " items\n", sep = "")
  invisible(x)
}

#' @export
summary.my_result <- function(object, ...) {
  cat("My Result Summary\n")
  cat("=================\n\n")
  cat("Data dimensions:", dim(object$data), "\n")
  cat("Data summary:\n")
  print(summary(object$data))
  invisible(object)
}

#' @export
`[.my_result` <- function(x, i, j, ...) {
  # Subset method
  new_my_result(
    data = x$data[i, j, ...],
    metadata = x$metadata
  )
}
```

### S4 Classes (For Complex Systems)

```r
# Define class
setClass(
  "MyS4Class",
  slots = c(
    data = "data.frame",
    metadata = "list",
    timestamp = "POSIXct"
  ),
  prototype = list(
    data = data.frame(),
    metadata = list(),
    timestamp = Sys.time()
  )
)

# Validity check
setValidity("MyS4Class", function(object) {
  if (nrow(object@data) == 0) {
    return("data must not be empty")
  }
  TRUE
})

# Constructor
MyS4Class <- function(data, metadata = list()) {
  new(
    "MyS4Class",
    data = data,
    metadata = metadata,
    timestamp = Sys.time()
  )
}

# Methods
setMethod("show", "MyS4Class", function(object) {
  cat("An object of class 'MyS4Class'\n")
  cat("Data dimensions:", dim(object@data), "\n")
  cat("Created:", format(object@timestamp), "\n")
})

setGeneric("getData", function(x) standardGeneric("getData"))
setMethod("getData", "MyS4Class", function(x) x@data)
```

### R6 Classes (For Reference Semantics)

```r
library(R6)

MyR6Class <- R6Class(
  "MyR6Class",
  public = list(
    # Public fields
    data = NULL,
    
    # Constructor
    initialize = function(data, threshold = 0.5) {
      if (!is.data.frame(data)) {
        stop("`data` must be a data.frame", call. = FALSE)
      }
      
      self$data <- data
      private$threshold <- threshold
      private$created <- Sys.time()
      
      invisible(self)
    },
    
    # Public methods
    process = function() {
      private$validate()
      result <- private$apply_transformation()
      return(result)
    },
    
    print = function() {
      cat("<MyR6Class>\n")
      cat("Data: ", nrow(self$data), " x ", ncol(self$data), "\n", sep = "")
      cat("Threshold:", private$threshold, "\n")
      invisible(self)
    }
  ),
  
  private = list(
    # Private fields
    threshold = NULL,
    created = NULL,
    
    # Private methods
    validate = function() {
      if (nrow(self$data) == 0) {
        stop("Data is empty", call. = FALSE)
      }
    },
    
    apply_transformation = function() {
      # Transform self$data using private$threshold
      self$data[self$data > private$threshold]
    }
  ),
  
  active = list(
    # Active bindings (computed properties)
    n_rows = function() {
      nrow(self$data)
    }
  )
)
```

---

## Error Handling

### Using rlang

```r
library(rlang)

# Errors (abort)
validate_input <- function(x, threshold) {
  # NULL check
  if (is.null(x)) {
    abort(
      "`x` must not be NULL",
      class = "error_null_input"
    )
  }
  
  # Type check
  if (!is.numeric(x)) {
    abort(
      sprintf("`x` must be numeric, not %s", typeof(x)),
      class = "error_wrong_type"
    )
  }
  
  # Range check
  if (threshold < 0 || threshold > 1) {
    abort(
      sprintf("`threshold` must be between 0 and 1, got %.2f", threshold),
      class = "error_out_of_range",
      threshold = threshold
    )
  }
}

# Warnings (warn)
process_data <- function(data) {
  if (anyNA(data)) {
    warn(
      "Data contains missing values; these will be removed",
      class = "warning_na_values"
    )
    data <- na.omit(data)
  }
  data
}

# Messages (inform)
process_verbose <- function(data, verbose = FALSE) {
  if (verbose) {
    inform(sprintf("Processing %d observations", nrow(data)))
  }
  # Process
}
```

### Catching Errors

```r
# Use tryCatch for error recovery
safe_calculate <- function(x, y) {
  tryCatch(
    {
      result <- x / y
      return(result)
    },
    error = function(e) {
      warn(sprintf("Calculation failed: %s", e$message))
      return(NA_real_)
    }
  )
}

# Use try for silent failures
result <- try(risky_operation(), silent = TRUE)
if (inherits(result, "try-error")) {
  # Handle error
}

# Use purrr::safely for functional error handling
library(purrr)

safe_log <- safely(log)
result <- safe_log(-1)

if (is.null(result$error)) {
  value <- result$result
} else {
  # Handle error
  value <- NA_real_
}
```

---

## Performance

### Vectorization

```r
# Good: Vectorized
calculate_distances <- function(x, y) {
  sqrt((x - y)^2)
}

# Bad: Loop when vectorization possible
calculate_distances_bad <- function(x, y) {
  result <- numeric(length(x))
  for (i in seq_along(x)) {
    result[i] <- sqrt((x[i] - y[i])^2)
  }
  result
}

# When loops are necessary, use seq_along/seq_len
for (i in seq_along(x)) {
  # Process x[i]
}

for (i in seq_len(n)) {
  # Do n iterations
}
```

### Preallocation

```r
# Good: Preallocate
compute_values <- function(n) {
  result <- numeric(n)
  for (i in seq_len(n)) {
    result[i] <- expensive_calculation(i)
  }
  result
}

# Bad: Growing vectors
compute_values_bad <- function(n) {
  result <- numeric(0)
  for (i in seq_len(n)) {
    result <- c(result, expensive_calculation(i))  # Slow!
  }
  result
}
```

### Apply Family

```r
# Use *apply functions for operations on lists/arrays

# lapply: Returns list
results <- lapply(1:10, function(x) x^2)

# sapply: Simplifies to vector/matrix (use carefully)
results <- sapply(1:10, function(x) x^2)

# vapply: Type-safe sapply (preferred)
results <- vapply(1:10, function(x) x^2, FUN.VALUE = numeric(1))

# mapply: Multiple arguments
results <- mapply(
  function(x, y) x + y,
  x = 1:10,
  y = 11:20,
  SIMPLIFY = FALSE
)
```

### Benchmarking

```r
library(bench)

# Benchmark alternatives
result <- bench::mark(
  vectorized = sqrt(x^2 + y^2),
  loop = {
    result <- numeric(length(x))
    for (i in seq_along(x)) {
      result[i] <- sqrt(x[i]^2 + y[i]^2)
    }
    result
  },
  check = TRUE,  # Verify results are identical
  iterations = 100
)

print(result)
plot(result)
```

### Profiling

```r
# Profile code to find bottlenecks
profvis::profvis({
  # Code to profile
  result <- expensive_function(large_data)
})

# Profile with Rprof
Rprof("profile.out")
result <- my_function()
Rprof(NULL)
summaryRprof("profile.out")
```

---

## Memory Management

### Avoid Copies

```r
# Good: Modify in place when possible
modify_list <- function(x) {
  x$new_element <- calculate_value()
  x
}

# Be aware of copy-on-modify
x <- 1:1000000
tracemem(x)
x[1] <- 0  # Triggers copy
```

### Large Data

```r
# Process in chunks
process_large_file <- function(file, chunk_size = 10000) {
  con <- file(file, "r")
  on.exit(close(con))
  
  results <- list()
  chunk_num <- 1
  
  while (length(chunk <- readLines(con, n = chunk_size)) > 0) {
    results[[chunk_num]] <- process_chunk(chunk)
    chunk_num <- chunk_num + 1
  }
  
  do.call(rbind, results)
}

# Use data.table for large data
library(data.table)
dt <- fread("large_file.csv")  # Fast reading
dt[, new_col := transform(old_col)]  # Modify by reference
```

### Clean Up

```r
# Remove large objects when done
large_data <- load_big_data()
result <- process(large_data)
rm(large_data)
gc()  # Force garbage collection

# Use on.exit for cleanup
process_with_temp <- function(data) {
  temp_file <- tempfile()
  on.exit(unlink(temp_file))
  
  write.csv(data, temp_file)
  # Process temp_file
}
```

---

## Documentation

### Function Documentation

```r
#' Calculate Summary Statistics
#'
#' This function calculates summary statistics for numeric vectors,
#' with options for handling missing values and selecting the type
#' of summary.
#'
#' @param x A numeric vector
#' @param na.rm Logical; if \code{TRUE}, remove missing values before
#'   calculation. Default is \code{FALSE}.
#' @param type Character string specifying the type of summary. Options
#'   are \code{"basic"} (mean and sd), \code{"full"} (mean, sd, min, max,
#'   median), or \code{"quartiles"} (quartiles only). Default is \code{"basic"}.
#'
#' @return A named numeric vector containing the requested statistics.
#'   For \code{type = "basic"}:
#'   \item{mean}{Arithmetic mean}
#'   \item{sd}{Standard deviation}
#'   
#'   For \code{type = "full"}:
#'   \item{mean}{Arithmetic mean}
#'   \item{sd}{Standard deviation}
#'   \item{min}{Minimum value}
#'   \item{max}{Maximum value}
#'   \item{median}{Median value}
#'
#' @details
#' The function uses standard R functions (\code{\link[base]{mean}},
#' \code{\link[stats]{sd}}, etc.) for calculations. For large vectors,
#' consider using specialized packages for better performance.
#'
#' @section Warning:
#' If \code{x} contains infinite values and \code{na.rm = TRUE}, these
#' will also be removed.
#'
#' @seealso
#' \code{\link{mean}}, \code{\link{sd}}, \code{\link{summary}}
#'
#' @export
#' @examples
#' # Basic usage
#' x <- rnorm(100)
#' calculate_summary(x)
#'
#' # With missing values
#' x[1:5] <- NA
#' calculate_summary(x, na.rm = TRUE)
#'
#' # Full statistics
#' calculate_summary(x, na.rm = TRUE, type = "full")
#'
#' # Quartiles only
#' calculate_summary(x, na.rm = TRUE, type = "quartiles")
calculate_summary <- function(x,
                               na.rm = FALSE,
                               type = c("basic", "full", "quartiles")) {
  type <- match.arg(type)
  
  # Implementation
}
```

### Package Documentation

```r
# R/package.R

#' packagename: Brief Package Description
#'
#' A more detailed description of what the package does. This can
#' span multiple paragraphs and should give users a good understanding
#' of the package's purpose and main functionality.
#'
#' @section Main Functions:
#' The main functions provided by this package are:
#' \itemize{
#'   \item \code{\link{function1}}: Does X
#'   \item \code{\link{function2}}: Does Y
#'   \item \code{\link{function3}}: Does Z
#' }
#'
#' @section Getting Started:
#' To get started with packagename, see \code{vignette("introduction")}.
#'
#' @docType package
#' @name packagename-package
#' @aliases packagename
#' @keywords internal
"_PACKAGE"
```

### Data Documentation

```r
#' Example Dataset Name
#'
#' A dataset containing information about [subject]. This data was
#' collected from [source] and includes [time period/scope].
#'
#' @format A data frame with X rows and Y variables:
#' \describe{
#'   \item{variable1}{Description of variable1, including units if applicable}
#'   \item{variable2}{Description of variable2}
#'   \item{variable3}{Factor with levels \code{A}, \code{B}, \code{C}}
#'   \item{variable4}{Date-time in POSIXct format}
#' }
#'
#' @details
#' Additional details about the data collection process, any transformations
#' applied, or important caveats users should know about.
#'
#' @source
#' Reference or URL for data source
#'
#' @examples
#' data(dataset_name)
#' head(dataset_name)
#' summary(dataset_name)
#' 
#' # Example analysis
#' library(ggplot2)
#' ggplot(dataset_name, aes(x = variable1, y = variable2)) +
#'   geom_point()
"dataset_name"
```

---

## Testing

### Test Organization

```r
# tests/testthat/test-feature.R

# Group related tests
test_that("function_name handles basic input correctly", {
  # Arrange
  input <- data.frame(x = 1:10, y = 11:20)
  
  # Act
  result <- function_name(input)
  
  # Assert
  expect_s3_class(result, "expected_class")
  expect_equal(nrow(result), 10)
  expect_true(all(result$z > 0))
})

test_that("function_name validates input", {
  expect_error(
    function_name(NULL),
    class = "error_null_input"
  )
  
  expect_error(
    function_name("not a data frame"),
    class = "error_wrong_type"
  )
})

test_that("function_name handles edge cases", {
  # Empty input
  empty <- data.frame()
  expect_warning(
    function_name(empty),
    "empty"
  )
  
  # Single row
  single <- data.frame(x = 1, y = 2)
  result <- function_name(single)
  expect_equal(nrow(result), 1)
  
  # Missing values
  with_na <- data.frame(x = c(1, NA, 3), y = c(1, 2, 3))
  expect_warning(
    function_name(with_na),
    "missing values"
  )
})
```

### Expectations

```r
# Value comparisons
expect_equal(result, expected)
expect_identical(result, expected)  # Stricter
expect_equivalent(result, expected)  # Ignores attributes

# Type checks
expect_type(x, "double")
expect_s3_class(obj, "my_class")
expect_s4_class(obj, "MyS4Class")

# Logical checks
expect_true(condition)
expect_false(condition)

# Errors and warnings
expect_error(code, "error message")
expect_error(code, class = "specific_error_class")
expect_warning(code, "warning message")
expect_message(code, "message text")
expect_silent(code)

# Length and dimensions
expect_length(x, 10)
expect_length(result, nrow(input))

# Named objects
expect_named(result, c("mean", "sd", "n"))

# Snapshots
expect_snapshot(complex_output)
```

### Test Helpers

```r
# tests/testthat/helper.R

# Create test data generators
make_test_data <- function(n = 100, seed = 123) {
  set.seed(seed)
  data.frame(
    id = 1:n,
    value = rnorm(n),
    category = sample(letters[1:3], n, replace = TRUE)
  )
}

# Create custom expectations
expect_valid_result <- function(object) {
  act <- quasi_label(rlang::enquo(object), arg = "object")
  
  expect(
    inherits(act$val, "my_result"),
    sprintf("%s is not a valid my_result object", act$lab)
  )
  
  expect(
    !is.null(act$val$data),
    sprintf("%s does not have data component", act$lab)
  )
  
  invisible(act$val)
}
```

---

## Package Structure

### Minimal Package

```
packagename/
├── DESCRIPTION
├── NAMESPACE
├── R/
│   └── functions.R
├── man/
│   └── (generated)
└── tests/
    └── testthat/
```

### Complete Package

```
packagename/
├── .Rbuildignore
├── .gitignore
├── .lintr
├── DESCRIPTION
├── NAMESPACE
├── LICENSE
├── README.md
├── NEWS.md
├── R/
│   ├── package.R
│   ├── data.R
│   ├── feature1.R
│   └── utils.R
├── man/
│   └── (generated by roxygen2)
├── tests/
│   ├── testthat.R
│   └── testthat/
│       ├── helper.R
│       ├── fixtures/
│       └── test-*.R
├── vignettes/
│   └── introduction.Rmd
├── data/
│   └── dataset.rda
├── data-raw/
│   └── dataset.R
├── inst/
│   ├── CITATION
│   └── extdata/
├── src/
│   └── (C/C++ code if needed)
└── _pkgdown.yml
```

---

## Dependencies

### Choosing Dependencies

```r
# Prefer base R when possible
mean(x)  # Base R
sum(x) / length(x)  # Also base R, but less clear

# Use well-maintained packages
library(rlang)     # Error handling
library(cli)       # User messages
library(testthat)  # Testing

# Avoid heavy dependencies for simple tasks
# Instead of: library(tidyverse)  # Heavy
# Use: library(dplyr)             # Lighter
```

### Declaring Dependencies

```r
# In DESCRIPTION:
# Imports: rlang (>= 1.0.0), dplyr (>= 1.0.0)
# Suggests: testthat (>= 3.0.0), knitr, rmarkdown

# Add dependency
usethis::use_package("rlang", min_version = "1.0.0")
usethis::use_package("testthat", type = "Suggests")

# Import functions
#' @importFrom rlang abort warn inform
#' @importFrom stats sd mean median
NULL

# Or use :: notation (preferred for few calls)
calculate <- function(x) {
  rlang::abort("Not implemented")
}
```

---

## Data Handling

### Reading Data

```r
# Base R
data <- read.csv("file.csv", stringsAsFactors = FALSE)
data <- read.delim("file.txt")

# readr (better defaults, faster)
library(readr)
data <- read_csv("file.csv")
data <- read_tsv("file.txt")

# data.table (fastest for large files)
library(data.table)
data <- fread("file.csv")
```

### Tidyverse Style

```r
library(dplyr)
library(tidyr)

# Pipes
result <- data %>%
  filter(category == "A") %>%
  group_by(group) %>%
  summarise(
    mean_value = mean(value),
    sd_value = sd(value),
    n = n()
  ) %>%
  arrange(desc(mean_value))

# Tidy selection
data %>%
  select(starts_with("x"))

data %>%
  select(where(is.numeric))

# Tidying
data %>%
  pivot_longer(
    cols = c(col1, col2, col3),
    names_to = "variable",
    values_to = "value"
  )
```

---

## Security

### Never Hardcode Credentials

```r
# Bad
api_key <- "12345-SECRET-KEY"
connect(api_key)

# Good: Use environment variables
api_key <- Sys.getenv("API_KEY")
if (api_key == "") {
  stop("API_KEY environment variable not set", call. = FALSE)
}
connect(api_key)
```

### Validate User Input

```r
execute_query <- function(query) {
  # Validate before executing
  if (!is.character(query) || length(query) != 1) {
    abort("`query` must be a single string")
  }
  
  # Sanitize
  query <- trimws(query)
  
  # Check for dangerous patterns
  if (grepl("(DROP|DELETE|TRUNCATE)", query, ignore.case = TRUE)) {
    abort("Query contains potentially dangerous commands")
  }
  
  # Execute
}
```

### Temporary Files

```r
# Always use tempdir() and tempfile()
process_with_temp <- function(data) {
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file), add = TRUE)
  
  write.csv(data, temp_file)
  result <- process_file(temp_file)
  
  result
}

# Never write to user directories without permission
# Bad: write.csv(data, "~/results.csv")
# Good: write.csv(data, file.choose())  # Let user choose
```

---

## Anti-Patterns

### Avoid

```r
# 1. Using = for assignment
x = 5  # Bad
x <- 5  # Good

# 2. Growing vectors in loops
for (i in 1:n) {
  result <- c(result, i^2)  # Bad
}

# 3. Using attach()
attach(data)  # Bad - namespace pollution
with(data, ...)  # Better

# 4. Not checking for NULL/NA
result <- mean(x)  # Bad if x might have NA
result <- mean(x, na.rm = TRUE)  # Better

# 5. Using T/F instead of TRUE/FALSE
if (x == T) { }  # Bad - T can be overwritten
if (x == TRUE) { }  # Good

# 6. Not using vectorization
for (i in seq_along(x)) {
  result[i] <- x[i] + y[i]  # Bad
}
result <- x + y  # Good

# 7. Using 1:length(x)
for (i in 1:length(x)) { }  # Bad - fails if length(x) == 0
for (i in seq_along(x)) { }  # Good

# 8. Modifying global state
my_function <- function(x) {
  global_var <<- x  # Bad
}

# 9. Using print() instead of return()
my_function <- function(x) {
  result <- x + 1
  print(result)  # Bad
}
my_function <- function(x) {
  x + 1  # Good
}

# 10. Not documenting functions
calculate <- function(x, y) {
  x + y  # Bad - no documentation
}
```

---

## Checklist

### Before Committing Code

- [ ] Code formatted with styler
- [ ] No linter warnings
- [ ] All tests pass
- [ ] Documentation updated
- [ ] Examples work
- [ ] No hardcoded paths or credentials
- [ ] No browser() or debug statements

### Before Releasing

- [ ] R CMD check passes (0 errors, 0 warnings, 0 notes)
- [ ] Test coverage > 90%
- [ ] All examples run
- [ ] Vignettes build
- [ ] NEWS.md updated
- [ ] Version number incremented
- [ ] pkgdown site builds
- [ ] README accurate

---

## Resources

- [Tidyverse Style Guide](https://style.tidyverse.org/)
- [R Packages Book](https://r-pkgs.org/)
- [Advanced R](https://adv-r.hadley.nz/)
- [CRAN Repository Policy](https://cran.r-project.org/web/packages/policies.html)

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

