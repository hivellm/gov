# AI Integration Manual - C

**Version**: 1.0.0  
**Last Updated**: 2025-10-11  
**Language**: C (C11/C17/C23)  
**Target Audience**: AI Agents (LLM-based development assistants)

---

## Table of Contents

1. [Introduction](#introduction)
2. [Quick Start](#quick-start)
3. [C-Specific Setup](#c-specific-setup)
4. [Configuration Standards](#configuration-standards)
5. [Source Code Standards](#source-code-standards)
6. [Testing Standards](#testing-standards)
7. [Build & Deployment](#build--deployment)
8. [Documentation](#documentation)
9. [C Best Practices](#c-best-practices)
10. [Common Patterns](#common-patterns)
11. [Troubleshooting](#troubleshooting)
12. [Complete Workflow](#complete-workflow)

---

## Introduction

This manual extends the [AI Integration Manual Template](../AI_INTEGRATION_MANUAL_TEMPLATE.md) with C-specific implementations.

**When to use this manual**:
- Systems programming (OS, drivers, embedded)
- High-performance applications
- Low-level libraries and APIs
- Embedded systems and IoT
- Network programming
- Cross-platform libraries
- CLI tools and utilities

**Prerequisites**:
- Read [AI Integration Manual Template](../AI_INTEGRATION_MANUAL_TEMPLATE.md)
- Read [Language Standards](../LANGUAGE_STANDARDS.md)
- Basic C knowledge

---

## Quick Start

### Minimum Viable Project Setup

```bash
# 1. Create project directory
mkdir my-project && cd my-project

# 2. Create directory structure
mkdir -p src include tests docs build scripts

# 3. Create basic files
touch src/main.c
touch include/my_project.h
touch CMakeLists.txt
touch Makefile
touch README.md

# 4. Initialize Git
git init
git add .
git commit -m "chore: initial project setup"

# 5. Create basic CMakeLists.txt
cat > CMakeLists.txt << 'EOF'
cmake_minimum_required(VERSION 3.20)
project(my_project VERSION 0.1.0 LANGUAGES C)

set(CMAKE_C_STANDARD 17)
set(CMAKE_C_STANDARD_REQUIRED ON)

add_executable(my_project src/main.c)
target_include_directories(my_project PRIVATE include)
EOF

# 6. Build project
mkdir -p build && cd build
cmake ..
make

# 7. Run
./my_project
```

### Library Project

```bash
# Create library
mkdir my-lib && cd my-lib
mkdir -p src include tests

# Create CMakeLists.txt for library
cat > CMakeLists.txt << 'EOF'
cmake_minimum_required(VERSION 3.20)
project(my_lib VERSION 0.1.0 LANGUAGES C)

set(CMAKE_C_STANDARD 17)
set(CMAKE_C_STANDARD_REQUIRED ON)

# Library source files
add_library(my_lib SHARED
    src/my_lib.c
)

target_include_directories(my_lib PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>
)

# Static library variant
add_library(my_lib_static STATIC
    src/my_lib.c
)

set_target_properties(my_lib_static PROPERTIES OUTPUT_NAME my_lib)

# Install rules
install(TARGETS my_lib my_lib_static
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
)

install(DIRECTORY include/ DESTINATION include)
EOF
```

---

## C-Specific Setup

### 1. Environment Setup

#### Install C Compiler

**Linux (Ubuntu/Debian)**:
```bash
# GCC
sudo apt update
sudo apt install build-essential gcc g++

# Clang
sudo apt install clang

# Verify installation
gcc --version
clang --version
```

**macOS**:
```bash
# Install Xcode Command Line Tools
xcode-select --install

# Or install via Homebrew
brew install gcc
brew install llvm
```

**Windows**:
```powershell
# Install MinGW-w64
# Download from: https://www.mingw-w64.org/

# Or use MSYS2
# Download from: https://www.msys2.org/
# Then install:
pacman -S mingw-w64-x86_64-gcc
pacman -S mingw-w64-x86_64-clang

# Or use Visual Studio Build Tools
# Download from: https://visualstudio.microsoft.com/downloads/
```

#### Install Build Tools

```bash
# CMake
# Linux
sudo apt install cmake

# macOS
brew install cmake

# Windows (via chocolatey)
choco install cmake

# Make
# Usually included with GCC/build-essential

# Ninja (alternative build system)
# Linux
sudo apt install ninja-build

# macOS
brew install ninja

# Windows
choco install ninja
```

#### Install Development Tools

```bash
# Valgrind (memory debugging - Linux/macOS only)
sudo apt install valgrind  # Linux
brew install valgrind      # macOS

# GDB (debugger)
sudo apt install gdb       # Linux
brew install gdb           # macOS

# Clang-format (code formatter)
sudo apt install clang-format  # Linux
brew install clang-format      # macOS

# Clang-tidy (static analyzer)
sudo apt install clang-tidy    # Linux
brew install llvm              # macOS

# Cppcheck (static analyzer)
sudo apt install cppcheck      # Linux
brew install cppcheck          # macOS

# Doxygen (documentation generator)
sudo apt install doxygen graphviz  # Linux
brew install doxygen graphviz      # macOS
```

### 2. Project Structure

```
my-project/
├── .github/
│   └── workflows/
│       ├── ci.yml
│       └── release.yml
├── .clang-format          # Code formatting rules
├── .clang-tidy            # Static analysis rules
├── .gitignore
├── CMakeLists.txt         # CMake build configuration
├── Makefile               # Alternative make build
├── README.md
├── CHANGELOG.md
├── LICENSE
├── docs/
│   ├── ROADMAP.md
│   ├── SPECS.md
│   ├── specs/
│   ├── api/
│   └── user-guide/
├── include/               # Public headers
│   └── my_project/
│       ├── my_project.h
│       └── types.h
├── src/                   # Source files
│   ├── main.c
│   ├── module1.c
│   └── internal.h        # Private headers
├── tests/                 # Test files
│   ├── unit/
│   │   ├── test_module1.c
│   │   └── test_runner.c
│   ├── integration/
│   └── fixtures/
├── examples/              # Example programs
│   └── example1.c
├── scripts/               # Build/deploy scripts
│   ├── build.sh
│   ├── test.sh
│   └── format.sh
├── build/                 # Build output (ignored by git)
└── dist/                  # Distribution packages
```

### 3. Version Management

C doesn't have a standard version manager, but we can manage versions via:

```bash
# Using compiler version
gcc-11 --version
gcc-12 --version

# Using environment modules (HPC environments)
module load gcc/11.3.0

# Using Docker for consistent environments
docker run -v $(pwd):/workspace gcc:11 gcc --version
```

---

## Configuration Standards

### 1. CMakeLists.txt

**Main Project CMakeLists.txt**:

```cmake
cmake_minimum_required(VERSION 3.20)
project(my_project 
    VERSION 0.1.0 
    DESCRIPTION "My C Project"
    LANGUAGES C
)

# C Standard
set(CMAKE_C_STANDARD 17)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS OFF)

# Build type
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

# Compiler flags
set(CMAKE_C_FLAGS_DEBUG "-g -O0 -Wall -Wextra -Wpedantic -fsanitize=address,undefined")
set(CMAKE_C_FLAGS_RELEASE "-O3 -DNDEBUG")

# Options
option(BUILD_SHARED_LIBS "Build shared libraries" ON)
option(BUILD_TESTS "Build tests" ON)
option(ENABLE_COVERAGE "Enable code coverage" OFF)
option(ENABLE_SANITIZERS "Enable sanitizers" OFF)

# Dependencies
find_package(Threads REQUIRED)

# Include directories
include_directories(${PROJECT_SOURCE_DIR}/include)

# Source files
file(GLOB_RECURSE SOURCES "src/*.c")
file(GLOB_RECURSE HEADERS "include/*.h")

# Executable
add_executable(${PROJECT_NAME} ${SOURCES} ${HEADERS})

# Link libraries
target_link_libraries(${PROJECT_NAME} PRIVATE
    Threads::Threads
    m  # Math library
)

# Include directory for target
target_include_directories(${PROJECT_NAME} PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>
)

# Compiler warnings
target_compile_options(${PROJECT_NAME} PRIVATE
    -Wall
    -Wextra
    -Wpedantic
    -Werror
    -Wconversion
    -Wsign-conversion
    -Wshadow
    -Wstrict-prototypes
    -Wmissing-prototypes
    -Wold-style-definition
)

# Sanitizers
if(ENABLE_SANITIZERS)
    target_compile_options(${PROJECT_NAME} PRIVATE
        -fsanitize=address
        -fsanitize=undefined
        -fno-omit-frame-pointer
    )
    target_link_options(${PROJECT_NAME} PRIVATE
        -fsanitize=address
        -fsanitize=undefined
    )
endif()

# Tests
if(BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests)
endif()

# Install
install(TARGETS ${PROJECT_NAME}
    RUNTIME DESTINATION bin
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
)

install(DIRECTORY include/ DESTINATION include)
```

### 2. Makefile (Alternative)

```makefile
# Project Configuration
PROJECT_NAME = my_project
VERSION = 0.1.0

# Directories
SRC_DIR = src
INC_DIR = include
BUILD_DIR = build
TEST_DIR = tests
OBJ_DIR = $(BUILD_DIR)/obj
BIN_DIR = $(BUILD_DIR)/bin

# Compiler Configuration
CC = gcc
CFLAGS = -std=c17 -Wall -Wextra -Wpedantic -Werror -I$(INC_DIR)
CFLAGS_DEBUG = $(CFLAGS) -g -O0 -DDEBUG -fsanitize=address,undefined
CFLAGS_RELEASE = $(CFLAGS) -O3 -DNDEBUG
LDFLAGS = -lm -lpthread

# Source Files
SOURCES = $(wildcard $(SRC_DIR)/*.c)
OBJECTS = $(patsubst $(SRC_DIR)/%.c,$(OBJ_DIR)/%.o,$(SOURCES))
TARGET = $(BIN_DIR)/$(PROJECT_NAME)

# Test Files
TEST_SOURCES = $(wildcard $(TEST_DIR)/*.c)
TEST_OBJECTS = $(patsubst $(TEST_DIR)/%.c,$(OBJ_DIR)/test_%.o,$(TEST_SOURCES))
TEST_TARGET = $(BIN_DIR)/test_runner

# Default target
.PHONY: all
all: release

# Create directories
$(OBJ_DIR) $(BIN_DIR):
	mkdir -p $@

# Release build
.PHONY: release
release: CFLAGS := $(CFLAGS_RELEASE)
release: $(TARGET)

# Debug build
.PHONY: debug
debug: CFLAGS := $(CFLAGS_DEBUG)
debug: $(TARGET)

# Link executable
$(TARGET): $(OBJECTS) | $(BIN_DIR)
	$(CC) $(OBJECTS) -o $@ $(LDFLAGS)

# Compile source files
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c | $(OBJ_DIR)
	$(CC) $(CFLAGS) -c $< -o $@

# Build tests
.PHONY: test
test: $(TEST_TARGET)
	$(TEST_TARGET)

# Link test executable
$(TEST_TARGET): $(filter-out $(OBJ_DIR)/main.o, $(OBJECTS)) $(TEST_OBJECTS) | $(BIN_DIR)
	$(CC) $^ -o $@ $(LDFLAGS)

# Compile test files
$(OBJ_DIR)/test_%.o: $(TEST_DIR)/%.c | $(OBJ_DIR)
	$(CC) $(CFLAGS_DEBUG) -c $< -o $@

# Run static analysis
.PHONY: lint
lint:
	clang-tidy $(SOURCES) -- $(CFLAGS)
	cppcheck --enable=all --suppress=missingIncludeSystem $(SRC_DIR)

# Format code
.PHONY: format
format:
	clang-format -i $(SOURCES) $(wildcard $(INC_DIR)/*.h)

# Check formatting
.PHONY: format-check
format-check:
	clang-format --dry-run -Werror $(SOURCES) $(wildcard $(INC_DIR)/*.h)

# Memory check
.PHONY: memcheck
memcheck: debug
	valgrind --leak-check=full --show-leak-kinds=all --track-origins=yes $(TARGET)

# Coverage
.PHONY: coverage
coverage: CFLAGS := $(CFLAGS_DEBUG) --coverage
coverage: LDFLAGS := $(LDFLAGS) --coverage
coverage: clean test
	lcov --capture --directory . --output-file coverage.info
	lcov --remove coverage.info '/usr/*' --output-file coverage.info
	genhtml coverage.info --output-directory coverage_report

# Clean
.PHONY: clean
clean:
	rm -rf $(BUILD_DIR) *.gcda *.gcno coverage.info coverage_report

# Install
.PHONY: install
install: release
	install -d $(DESTDIR)/usr/local/bin
	install -m 755 $(TARGET) $(DESTDIR)/usr/local/bin
	install -d $(DESTDIR)/usr/local/include/$(PROJECT_NAME)
	install -m 644 $(INC_DIR)/*.h $(DESTDIR)/usr/local/include/$(PROJECT_NAME)

# Uninstall
.PHONY: uninstall
uninstall:
	rm -f $(DESTDIR)/usr/local/bin/$(PROJECT_NAME)
	rm -rf $(DESTDIR)/usr/local/include/$(PROJECT_NAME)

# Help
.PHONY: help
help:
	@echo "Available targets:"
	@echo "  all         - Build release version (default)"
	@echo "  release     - Build optimized release version"
	@echo "  debug       - Build debug version with sanitizers"
	@echo "  test        - Build and run tests"
	@echo "  lint        - Run static analysis"
	@echo "  format      - Format code with clang-format"
	@echo "  format-check- Check code formatting"
	@echo "  memcheck    - Run valgrind memory check"
	@echo "  coverage    - Generate code coverage report"
	@echo "  clean       - Remove build artifacts"
	@echo "  install     - Install to system"
	@echo "  uninstall   - Remove from system"
```

### 3. .clang-format

```yaml
---
Language: C
BasedOnStyle: LLVM
IndentWidth: 4
TabWidth: 4
UseTab: Never
ColumnLimit: 100
BreakBeforeBraces: Linux
AllowShortIfStatementsOnASingleLine: false
AllowShortLoopsOnASingleLine: false
AllowShortFunctionsOnASingleLine: Empty
IndentCaseLabels: false
PointerAlignment: Right
AlignConsecutiveAssignments: true
AlignConsecutiveDeclarations: true
AlignTrailingComments: true
SpaceAfterCStyleCast: true
SpacesInParentheses: false
```

### 4. .clang-tidy

```yaml
---
Checks: >
  -*,
  clang-analyzer-*,
  bugprone-*,
  cert-*,
  misc-*,
  modernize-*,
  performance-*,
  readability-*,
  -readability-identifier-length,
  -modernize-use-trailing-return-type

WarningsAsErrors: '*'

CheckOptions:
  - key: readability-identifier-naming.FunctionCase
    value: lower_case
  - key: readability-identifier-naming.VariableCase
    value: lower_case
  - key: readability-identifier-naming.ConstantCase
    value: UPPER_CASE
  - key: readability-identifier-naming.MacroCase
    value: UPPER_CASE
  - key: readability-identifier-naming.TypedefCase
    value: lower_case
  - key: readability-identifier-naming.StructCase
    value: lower_case
```

### 5. .gitignore

```gitignore
# Build artifacts
build/
dist/
*.o
*.obj
*.so
*.dll
*.dylib
*.a
*.lib
*.exe
*.out

# Debug files
*.dSYM/
*.su
*.idb
*.pdb

# Coverage
*.gcda
*.gcno
*.gcov
coverage.info
coverage_report/
*.lcov

# Valgrind
vgcore.*
*.log

# IDE
.vscode/
.idea/
*.swp
*.swo
*~

# CMake
CMakeCache.txt
CMakeFiles/
cmake_install.cmake
install_manifest.txt
Makefile  # Only if using CMake

# OS
.DS_Store
Thumbs.db

# Documentation
docs/html/
docs/latex/
docs/api/generated/

# Testing
test_results/
*.test
```

---

## Source Code Standards

### 1. File Naming Conventions

```
src/module_name.c          # Implementation
include/module_name.h      # Public header
src/module_name_internal.h # Private header
tests/test_module_name.c   # Test file
```

### 2. Header File Template

```c
/**
 * @file module_name.h
 * @brief Brief description of the module
 * @details Detailed description of the module functionality
 * @author Author Name
 * @date 2025-10-11
 * @version 0.1.0
 */

#ifndef MODULE_NAME_H
#define MODULE_NAME_H

#ifdef __cplusplus
extern "C" {
#endif

/* Include necessary system headers */
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

/* Version information */
#define MODULE_NAME_VERSION_MAJOR 0
#define MODULE_NAME_VERSION_MINOR 1
#define MODULE_NAME_VERSION_PATCH 0

/* Constants */
#define MODULE_MAX_SIZE 1024

/* Error codes */
typedef enum {
    MODULE_SUCCESS = 0,
    MODULE_ERROR_INVALID_PARAM = -1,
    MODULE_ERROR_OUT_OF_MEMORY = -2,
    MODULE_ERROR_NOT_FOUND = -3,
} module_error_t;

/* Type definitions */
typedef struct module_context module_context_t;

/**
 * @brief Initialize the module
 * @param[out] ctx Pointer to context pointer
 * @return MODULE_SUCCESS on success, error code otherwise
 */
module_error_t module_init(module_context_t **ctx);

/**
 * @brief Cleanup and free module resources
 * @param[in] ctx Context pointer
 */
void module_cleanup(module_context_t *ctx);

/**
 * @brief Perform module operation
 * @param[in] ctx Context pointer
 * @param[in] input Input data
 * @param[out] output Output buffer
 * @param[in] output_size Size of output buffer
 * @return MODULE_SUCCESS on success, error code otherwise
 */
module_error_t module_process(
    module_context_t *ctx,
    const void *input,
    void *output,
    size_t output_size
);

#ifdef __cplusplus
}
#endif

#endif /* MODULE_NAME_H */
```

### 3. Implementation File Template

```c
/**
 * @file module_name.c
 * @brief Implementation of module_name
 */

#include "module_name.h"
#include <stdlib.h>
#include <string.h>
#include <assert.h>

/* Internal structure definition */
struct module_context {
    void *data;
    size_t size;
    bool initialized;
};

/* Internal helper functions */
static module_error_t validate_params(const module_context_t *ctx, const void *input)
{
    if (ctx == NULL || input == NULL) {
        return MODULE_ERROR_INVALID_PARAM;
    }
    
    if (!ctx->initialized) {
        return MODULE_ERROR_INVALID_PARAM;
    }
    
    return MODULE_SUCCESS;
}

/* Public API implementations */
module_error_t module_init(module_context_t **ctx)
{
    if (ctx == NULL) {
        return MODULE_ERROR_INVALID_PARAM;
    }
    
    module_context_t *new_ctx = (module_context_t *)calloc(1, sizeof(module_context_t));
    if (new_ctx == NULL) {
        return MODULE_ERROR_OUT_OF_MEMORY;
    }
    
    new_ctx->data = NULL;
    new_ctx->size = 0;
    new_ctx->initialized = true;
    
    *ctx = new_ctx;
    return MODULE_SUCCESS;
}

void module_cleanup(module_context_t *ctx)
{
    if (ctx == NULL) {
        return;
    }
    
    if (ctx->data != NULL) {
        free(ctx->data);
        ctx->data = NULL;
    }
    
    ctx->initialized = false;
    free(ctx);
}

module_error_t module_process(
    module_context_t *ctx,
    const void *input,
    void *output,
    size_t output_size)
{
    module_error_t err = validate_params(ctx, input);
    if (err != MODULE_SUCCESS) {
        return err;
    }
    
    if (output == NULL || output_size == 0) {
        return MODULE_ERROR_INVALID_PARAM;
    }
    
    /* Implementation here */
    
    return MODULE_SUCCESS;
}
```

### 4. Code Style Guidelines

**Naming Conventions**:
- Functions: `snake_case` (e.g., `module_init`, `get_value`)
- Variables: `snake_case` (e.g., `buffer_size`, `is_valid`)
- Constants: `UPPER_SNAKE_CASE` (e.g., `MAX_BUFFER_SIZE`)
- Macros: `UPPER_SNAKE_CASE` (e.g., `MIN(a, b)`)
- Types: `snake_case_t` suffix (e.g., `module_context_t`)
- Structs: `snake_case` (e.g., `struct module_config`)
- Enums: `snake_case_t` with `UPPER_SNAKE_CASE` values

**Formatting**:
- Indentation: 4 spaces (no tabs)
- Line length: 100 characters maximum
- Braces: K&R style (opening brace on same line for functions)
- Pointer: `type *name` (asterisk with variable name)

**Documentation**:
- Use Doxygen-style comments
- Document all public functions, structures, and constants
- Include `@param`, `@return`, `@brief` tags

---

## Testing Standards

### 1. Testing Framework Options

**Unity** (recommended for embedded):
```bash
# Clone Unity
git clone https://github.com/ThrowTheSwitch/Unity.git vendor/unity

# Create test
cat > tests/test_example.c << 'EOF'
#include "../vendor/unity/src/unity.h"
#include "../include/my_module.h"

void setUp(void) {
    /* Setup before each test */
}

void tearDown(void) {
    /* Cleanup after each test */
}

void test_module_init_success(void) {
    module_context_t *ctx = NULL;
    module_error_t err = module_init(&ctx);
    
    TEST_ASSERT_EQUAL(MODULE_SUCCESS, err);
    TEST_ASSERT_NOT_NULL(ctx);
    
    module_cleanup(ctx);
}

void test_module_init_null_pointer(void) {
    module_error_t err = module_init(NULL);
    TEST_ASSERT_EQUAL(MODULE_ERROR_INVALID_PARAM, err);
}

int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_module_init_success);
    RUN_TEST(test_module_init_null_pointer);
    return UNITY_END();
}
EOF
```

**Check** (Unix-like systems):
```bash
# Install Check
sudo apt install check  # Linux
brew install check      # macOS

# Create test
cat > tests/test_check_example.c << 'EOF'
#include <check.h>
#include "../include/my_module.h"

START_TEST(test_module_init)
{
    module_context_t *ctx = NULL;
    module_error_t err = module_init(&ctx);
    
    ck_assert_int_eq(err, MODULE_SUCCESS);
    ck_assert_ptr_nonnull(ctx);
    
    module_cleanup(ctx);
}
END_TEST

Suite *module_suite(void)
{
    Suite *s;
    TCase *tc_core;
    
    s = suite_create("Module");
    tc_core = tcase_create("Core");
    
    tcase_add_test(tc_core, test_module_init);
    suite_add_tcase(s, tc_core);
    
    return s;
}

int main(void)
{
    int number_failed;
    Suite *s;
    SRunner *sr;
    
    s = module_suite();
    sr = srunner_create(s);
    
    srunner_run_all(sr, CK_NORMAL);
    number_failed = srunner_ntests_failed(sr);
    srunner_free(sr);
    
    return (number_failed == 0) ? 0 : 1;
}
EOF
```

**Criterion** (modern testing framework):
```bash
# Install Criterion
# Linux: Build from source or use package manager
# macOS
brew install snaipe/soft/criterion

# Create test
cat > tests/test_criterion_example.c << 'EOF'
#include <criterion/criterion.h>
#include "../include/my_module.h"

Test(module, init_success) {
    module_context_t *ctx = NULL;
    module_error_t err = module_init(&ctx);
    
    cr_assert_eq(err, MODULE_SUCCESS);
    cr_assert_not_null(ctx);
    
    module_cleanup(ctx);
}

Test(module, init_null_pointer) {
    module_error_t err = module_init(NULL);
    cr_assert_eq(err, MODULE_ERROR_INVALID_PARAM);
}
EOF
```

### 2. CMakeLists.txt for Tests

```cmake
# tests/CMakeLists.txt
cmake_minimum_required(VERSION 3.20)

# Find test framework
find_package(Check REQUIRED)
# Or use Unity (vendored)
# add_subdirectory(../vendor/unity unity)

# Test executable
add_executable(test_runner
    test_module1.c
    test_module2.c
    test_runner.c
)

# Link with library under test
target_link_libraries(test_runner PRIVATE
    my_lib
    ${CHECK_LIBRARIES}  # For Check
    # unity               # For Unity
)

target_include_directories(test_runner PRIVATE
    ${CHECK_INCLUDE_DIRS}
    ${CMAKE_SOURCE_DIR}/include
)

# Add tests
add_test(NAME test_runner COMMAND test_runner)

# Enable coverage if requested
if(ENABLE_COVERAGE)
    target_compile_options(test_runner PRIVATE --coverage)
    target_link_options(test_runner PRIVATE --coverage)
endif()
```

### 3. Coverage Report

```bash
# Using gcov/lcov
# Build with coverage flags
mkdir build && cd build
cmake -DENABLE_COVERAGE=ON ..
make

# Run tests
./test_runner

# Generate coverage report
lcov --capture --directory . --output-file coverage.info
lcov --remove coverage.info '/usr/*' '*/tests/*' --output-file coverage.info
genhtml coverage.info --output-directory coverage_report

# Open report
xdg-open coverage_report/index.html  # Linux
open coverage_report/index.html      # macOS
```

### 4. Memory Testing

```bash
# Valgrind memory check
valgrind --leak-check=full \
         --show-leak-kinds=all \
         --track-origins=yes \
         --verbose \
         --log-file=valgrind.log \
         ./test_runner

# AddressSanitizer (compile-time)
gcc -fsanitize=address -g -O0 -o test_runner test_runner.c
./test_runner

# MemorySanitizer
clang -fsanitize=memory -g -O0 -o test_runner test_runner.c
./test_runner

# UndefinedBehaviorSanitizer
gcc -fsanitize=undefined -g -O0 -o test_runner test_runner.c
./test_runner
```

---

## Build & Deployment

### 1. Build Scripts

**scripts/build.sh**:
```bash
#!/bin/bash
set -e

BUILD_TYPE="${1:-Release}"
BUILD_DIR="build"

echo "Building project in $BUILD_TYPE mode..."

# Create build directory
mkdir -p "$BUILD_DIR"
cd "$BUILD_DIR"

# Configure
cmake .. \
    -DCMAKE_BUILD_TYPE="$BUILD_TYPE" \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=ON

# Build
cmake --build . --parallel $(nproc)

echo "Build complete!"
```

**scripts/test.sh**:
```bash
#!/bin/bash
set -e

BUILD_DIR="build"

echo "Running tests..."

# Build tests
cd "$BUILD_DIR"
cmake --build . --target test_runner --parallel $(nproc)

# Run tests
ctest --output-on-failure --verbose

# Memory check (optional)
if command -v valgrind &> /dev/null; then
    echo "Running memory checks..."
    valgrind --leak-check=full --error-exitcode=1 ./test_runner
fi

echo "All tests passed!"
```

**scripts/format.sh**:
```bash
#!/bin/bash
set -e

echo "Formatting code..."

# Find all C source and header files
find src include tests -name "*.c" -o -name "*.h" | while read -r file; do
    echo "Formatting $file"
    clang-format -i "$file"
done

echo "Formatting complete!"
```

**scripts/lint.sh**:
```bash
#!/bin/bash
set -e

echo "Running static analysis..."

# Clang-tidy
find src -name "*.c" | while read -r file; do
    echo "Analyzing $file"
    clang-tidy "$file" -- -Iinclude
done

# Cppcheck
cppcheck --enable=all \
         --suppress=missingIncludeSystem \
         --error-exitcode=1 \
         --inline-suppr \
         src/

echo "Static analysis complete!"
```

### 2. CI/CD Configuration

**.github/workflows/ci.yml**:
```yaml
name: CI

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main, develop ]

jobs:
  build-and-test:
    runs-on: ${{ matrix.os }}
    strategy:
      matrix:
        os: [ubuntu-latest, macos-latest]
        compiler: [gcc, clang]
        build_type: [Debug, Release]
        
    steps:
    - uses: actions/checkout@v3
      with:
        submodules: recursive
        
    - name: Install Dependencies (Ubuntu)
      if: runner.os == 'Linux'
      run: |
        sudo apt-get update
        sudo apt-get install -y cmake ninja-build valgrind lcov clang-tidy cppcheck check
        
    - name: Install Dependencies (macOS)
      if: runner.os == 'macOS'
      run: |
        brew install cmake ninja lcov llvm check
        
    - name: Configure CMake
      run: |
        cmake -B build \
          -G Ninja \
          -DCMAKE_BUILD_TYPE=${{ matrix.build_type }} \
          -DCMAKE_C_COMPILER=${{ matrix.compiler }} \
          -DBUILD_TESTS=ON \
          -DENABLE_COVERAGE=${{ matrix.build_type == 'Debug' && 'ON' || 'OFF' }}
          
    - name: Build
      run: cmake --build build --parallel
      
    - name: Run Tests
      run: cd build && ctest --output-on-failure --verbose
      
    - name: Memory Check (Linux Debug)
      if: runner.os == 'Linux' && matrix.build_type == 'Debug'
      run: |
        cd build
        valgrind --leak-check=full --error-exitcode=1 ./test_runner
        
    - name: Generate Coverage (Debug)
      if: matrix.build_type == 'Debug' && matrix.compiler == 'gcc'
      run: |
        cd build
        lcov --capture --directory . --output-file coverage.info
        lcov --remove coverage.info '/usr/*' '*/tests/*' --output-file coverage.info
        lcov --list coverage.info
        
    - name: Upload Coverage
      if: matrix.build_type == 'Debug' && matrix.compiler == 'gcc'
      uses: codecov/codecov-action@v3
      with:
        files: ./build/coverage.info
        
    - name: Run Static Analysis
      if: matrix.compiler == 'clang'
      run: |
        ./scripts/lint.sh

  format-check:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v3
    
    - name: Install clang-format
      run: sudo apt-get install -y clang-format
      
    - name: Check formatting
      run: |
        find src include tests -name "*.c" -o -name "*.h" | while read file; do
          clang-format --dry-run -Werror "$file"
        done
```

### 3. Package Distribution

**Creating a distributable library**:

```bash
# Install to system
mkdir build && cd build
cmake .. -DCMAKE_INSTALL_PREFIX=/usr/local
make
sudo make install

# Create package
cpack -G "TGZ;DEB;RPM"

# Create static/shared libraries
mkdir build && cd build
cmake .. -DBUILD_SHARED_LIBS=ON
make
make install

# pkg-config file
cat > my_lib.pc << 'EOF'
prefix=/usr/local
exec_prefix=${prefix}
libdir=${exec_prefix}/lib
includedir=${prefix}/include

Name: My Library
Description: Description of my library
Version: 0.1.0
Requires:
Libs: -L${libdir} -lmy_lib
Cflags: -I${includedir}
EOF
```

---

## Documentation

### 1. Doxygen Configuration

**Doxyfile**:
```bash
# Generate default Doxyfile
doxygen -g

# Key settings to modify:
PROJECT_NAME           = "My Project"
PROJECT_NUMBER         = 0.1.0
PROJECT_BRIEF          = "Brief description"
OUTPUT_DIRECTORY       = docs/api
INPUT                  = include src
RECURSIVE              = YES
EXTRACT_ALL            = YES
EXTRACT_PRIVATE        = YES
EXTRACT_STATIC         = YES
GENERATE_HTML          = YES
GENERATE_LATEX         = NO
HAVE_DOT               = YES
CALL_GRAPH             = YES
CALLER_GRAPH           = YES
```

**Generate documentation**:
```bash
# Generate HTML docs
doxygen Doxyfile

# Open docs
xdg-open docs/api/html/index.html  # Linux
open docs/api/html/index.html      # macOS
```

### 2. README.md Template

```markdown
# My Project

Brief description of your project.

## Features

- Feature 1
- Feature 2
- Feature 3

## Requirements

- C11/C17 compiler (GCC 9+, Clang 10+, MSVC 2019+)
- CMake 3.20+
- Make or Ninja

## Building

\`\`\`bash
mkdir build && cd build
cmake ..
make
\`\`\`

## Installation

\`\`\`bash
sudo make install
\`\`\`

## Usage

\`\`\`c
#include <my_project/my_project.h>

int main(void) {
    module_context_t *ctx = NULL;
    module_init(&ctx);
    
    /* Use the module */
    
    module_cleanup(ctx);
    return 0;
}
\`\`\`

## Testing

\`\`\`bash
make test
\`\`\`

## Documentation

See [API Documentation](docs/api/html/index.html)

## License

MIT License - See LICENSE file
```

---

## C Best Practices

### 1. Memory Management

```c
/* Always initialize pointers */
void *ptr = NULL;

/* Check allocation */
ptr = malloc(size);
if (ptr == NULL) {
    return ERROR_OUT_OF_MEMORY;
}

/* Use calloc for zero-initialization */
ptr = calloc(count, size);

/* Always free allocated memory */
free(ptr);
ptr = NULL;  /* Prevent double-free */

/* Use RAII-like patterns with cleanup functions */
void cleanup_resource(resource_t **res) {
    if (res != NULL && *res != NULL) {
        free_resource(*res);
        *res = NULL;
    }
}

#define AUTO_CLEANUP __attribute__((cleanup(cleanup_resource)))

void function(void) {
    AUTO_CLEANUP resource_t *res = NULL;
    /* res is automatically cleaned up on scope exit */
}
```

### 2. Error Handling

```c
/* Use error codes */
typedef enum {
    SUCCESS = 0,
    ERROR_INVALID_PARAM = -1,
    ERROR_OUT_OF_MEMORY = -2,
    ERROR_IO = -3,
} error_t;

/* Return error codes, use output parameters for results */
error_t get_value(const input_t *input, output_t *output) {
    if (input == NULL || output == NULL) {
        return ERROR_INVALID_PARAM;
    }
    
    /* Process */
    
    return SUCCESS;
}

/* Use errno for system errors */
#include <errno.h>
#include <string.h>

FILE *fp = fopen("file.txt", "r");
if (fp == NULL) {
    fprintf(stderr, "Failed to open file: %s\n", strerror(errno));
    return ERROR_IO;
}
```

### 3. Thread Safety

```c
#include <pthread.h>

typedef struct {
    pthread_mutex_t lock;
    void *data;
} thread_safe_t;

error_t thread_safe_init(thread_safe_t **ctx) {
    thread_safe_t *ts = calloc(1, sizeof(thread_safe_t));
    if (ts == NULL) {
        return ERROR_OUT_OF_MEMORY;
    }
    
    pthread_mutex_init(&ts->lock, NULL);
    *ctx = ts;
    return SUCCESS;
}

error_t thread_safe_operation(thread_safe_t *ctx) {
    if (ctx == NULL) {
        return ERROR_INVALID_PARAM;
    }
    
    pthread_mutex_lock(&ctx->lock);
    
    /* Critical section */
    
    pthread_mutex_unlock(&ctx->lock);
    return SUCCESS;
}

void thread_safe_cleanup(thread_safe_t *ctx) {
    if (ctx != NULL) {
        pthread_mutex_destroy(&ctx->lock);
        free(ctx);
    }
}
```

### 4. Input Validation

```c
/* Always validate inputs */
error_t process_buffer(const uint8_t *buffer, size_t size, uint8_t *output) {
    /* Check for NULL pointers */
    if (buffer == NULL || output == NULL) {
        return ERROR_INVALID_PARAM;
    }
    
    /* Check for reasonable sizes */
    if (size == 0 || size > MAX_BUFFER_SIZE) {
        return ERROR_INVALID_PARAM;
    }
    
    /* Check for integer overflow */
    size_t required_size = size * 2;
    if (required_size < size) {  /* Overflow occurred */
        return ERROR_INVALID_PARAM;
    }
    
    /* Process data */
    
    return SUCCESS;
}
```

### 5. Const Correctness

```c
/* Use const for read-only parameters */
error_t read_data(const uint8_t *input, size_t input_size, uint8_t *output);

/* Use const for return values that shouldn't be modified */
const char *get_version(void);

/* Use const for struct members that shouldn't change */
typedef struct {
    const uint32_t id;
    char *name;
} entity_t;
```

### 6. Opaque Types

```c
/* In header file (my_module.h) */
typedef struct module_context module_context_t;  /* Forward declaration */

/* In implementation file (my_module.c) */
struct module_context {
    /* Internal implementation hidden from users */
    void *private_data;
    size_t size;
};
```

---

## Common Patterns

### 1. Constructor/Destructor Pattern

```c
/* Constructor */
error_t object_create(object_t **obj, const config_t *config) {
    if (obj == NULL || config == NULL) {
        return ERROR_INVALID_PARAM;
    }
    
    object_t *new_obj = calloc(1, sizeof(object_t));
    if (new_obj == NULL) {
        return ERROR_OUT_OF_MEMORY;
    }
    
    /* Initialize */
    new_obj->state = STATE_INITIALIZED;
    
    *obj = new_obj;
    return SUCCESS;
}

/* Destructor */
void object_destroy(object_t **obj) {
    if (obj == NULL || *obj == NULL) {
        return;
    }
    
    object_t *o = *obj;
    
    /* Cleanup resources */
    if (o->resource != NULL) {
        free(o->resource);
    }
    
    free(o);
    *obj = NULL;
}
```

### 2. Iterator Pattern

```c
typedef struct {
    void *current;
    void *end;
} iterator_t;

error_t iterator_create(container_t *container, iterator_t **it) {
    if (container == NULL || it == NULL) {
        return ERROR_INVALID_PARAM;
    }
    
    iterator_t *iter = malloc(sizeof(iterator_t));
    if (iter == NULL) {
        return ERROR_OUT_OF_MEMORY;
    }
    
    iter->current = container->data;
    iter->end = container->data + container->size;
    
    *it = iter;
    return SUCCESS;
}

bool iterator_has_next(const iterator_t *it) {
    return it != NULL && it->current < it->end;
}

void *iterator_next(iterator_t *it) {
    if (!iterator_has_next(it)) {
        return NULL;
    }
    
    void *value = it->current;
    it->current = (char *)it->current + sizeof(element_t);
    return value;
}
```

### 3. Callback Pattern

```c
typedef error_t (*callback_fn)(void *ctx, const event_t *event);

typedef struct {
    callback_fn callback;
    void *user_data;
} event_handler_t;

error_t register_callback(event_handler_t *handler, callback_fn fn, void *user_data) {
    if (handler == NULL || fn == NULL) {
        return ERROR_INVALID_PARAM;
    }
    
    handler->callback = fn;
    handler->user_data = user_data;
    
    return SUCCESS;
}

error_t trigger_event(event_handler_t *handler, const event_t *event) {
    if (handler == NULL || handler->callback == NULL) {
        return ERROR_INVALID_PARAM;
    }
    
    return handler->callback(handler->user_data, event);
}
```

### 4. String Manipulation

```c
/* Safe string copy */
error_t safe_strcpy(char *dest, size_t dest_size, const char *src) {
    if (dest == NULL || src == NULL || dest_size == 0) {
        return ERROR_INVALID_PARAM;
    }
    
    size_t src_len = strlen(src);
    if (src_len >= dest_size) {
        return ERROR_BUFFER_TOO_SMALL;
    }
    
    memcpy(dest, src, src_len + 1);
    return SUCCESS;
}

/* Dynamic string building */
typedef struct {
    char *data;
    size_t length;
    size_t capacity;
} string_builder_t;

error_t string_builder_append(string_builder_t *sb, const char *str) {
    if (sb == NULL || str == NULL) {
        return ERROR_INVALID_PARAM;
    }
    
    size_t str_len = strlen(str);
    size_t required = sb->length + str_len + 1;
    
    if (required > sb->capacity) {
        size_t new_capacity = sb->capacity * 2;
        if (new_capacity < required) {
            new_capacity = required;
        }
        
        char *new_data = realloc(sb->data, new_capacity);
        if (new_data == NULL) {
            return ERROR_OUT_OF_MEMORY;
        }
        
        sb->data = new_data;
        sb->capacity = new_capacity;
    }
    
    memcpy(sb->data + sb->length, str, str_len + 1);
    sb->length += str_len;
    
    return SUCCESS;
}
```

---

## Troubleshooting

### 1. Common Build Errors

**Problem**: `undefined reference to 'pthread_create'`
```bash
# Solution: Link pthread library
gcc -o program main.c -lpthread
# Or in CMakeLists.txt:
find_package(Threads REQUIRED)
target_link_libraries(program PRIVATE Threads::Threads)
```

**Problem**: `undefined reference to 'sqrt'`
```bash
# Solution: Link math library
gcc -o program main.c -lm
```

**Problem**: Header file not found
```bash
# Solution: Add include directory
gcc -I./include -o program main.c
# Or in CMakeLists.txt:
target_include_directories(program PRIVATE include)
```

### 2. Memory Issues

**Problem**: Segmentation fault
```bash
# Debug with GDB
gcc -g -o program main.c
gdb ./program
(gdb) run
(gdb) backtrace

# Or use Valgrind
valgrind --leak-check=full ./program
```

**Problem**: Memory leaks
```bash
# Use Valgrind
valgrind --leak-check=full --show-leak-kinds=all ./program

# Or compile with AddressSanitizer
gcc -fsanitize=address -g -o program main.c
./program
```

### 3. Performance Issues

**Problem**: Slow execution
```bash
# Profile with gprof
gcc -pg -o program main.c
./program
gprof program gmon.out > analysis.txt

# Or use perf (Linux)
perf record ./program
perf report

# Or use Valgrind's callgrind
valgrind --tool=callgrind ./program
kcachegrind callgrind.out.*
```

---

## Complete Workflow

### Example: Creating a New C Library

```bash
# 1. Create project structure
mkdir my-lib && cd my-lib
mkdir -p src include tests docs scripts

# 2. Initialize Git
git init
git checkout -b develop

# 3. Create CMakeLists.txt
cat > CMakeLists.txt << 'EOF'
cmake_minimum_required(VERSION 3.20)
project(my_lib VERSION 0.1.0 LANGUAGES C)

set(CMAKE_C_STANDARD 17)
set(CMAKE_C_STANDARD_REQUIRED ON)

add_library(my_lib SHARED src/my_lib.c)
target_include_directories(my_lib PUBLIC include)

if(BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests)
endif()
EOF

# 4. Create header
cat > include/my_lib.h << 'EOF'
#ifndef MY_LIB_H
#define MY_LIB_H

#include <stddef.h>

typedef struct my_context my_context_t;

int my_lib_init(my_context_t **ctx);
void my_lib_cleanup(my_context_t *ctx);
int my_lib_process(my_context_t *ctx, const void *input, size_t size);

#endif /* MY_LIB_H */
EOF

# 5. Create implementation
cat > src/my_lib.c << 'EOF'
#include "../include/my_lib.h"
#include <stdlib.h>

struct my_context {
    void *data;
};

int my_lib_init(my_context_t **ctx) {
    if (ctx == NULL) return -1;
    
    my_context_t *c = calloc(1, sizeof(my_context_t));
    if (c == NULL) return -1;
    
    *ctx = c;
    return 0;
}

void my_lib_cleanup(my_context_t *ctx) {
    free(ctx);
}

int my_lib_process(my_context_t *ctx, const void *input, size_t size) {
    if (ctx == NULL || input == NULL) return -1;
    /* Implementation */
    return 0;
}
EOF

# 6. Create test
mkdir tests
cat > tests/CMakeLists.txt << 'EOF'
add_executable(test_runner test_my_lib.c)
target_link_libraries(test_runner PRIVATE my_lib)
add_test(NAME test_runner COMMAND test_runner)
EOF

cat > tests/test_my_lib.c << 'EOF'
#include <assert.h>
#include "../include/my_lib.h"

int main(void) {
    my_context_t *ctx = NULL;
    assert(my_lib_init(&ctx) == 0);
    assert(ctx != NULL);
    my_lib_cleanup(ctx);
    return 0;
}
EOF

# 7. Create documentation
cat > docs/ROADMAP.md << 'EOF'
# Roadmap

## Phase 1: Core Implementation
- [x] Project setup
- [x] Basic API
- [ ] Advanced features

## Phase 2: Testing
- [x] Unit tests
- [ ] Integration tests
EOF

# 8. Build and test
mkdir build && cd build
cmake .. -DBUILD_TESTS=ON
make
ctest

# 9. Commit
git add .
git commit -m "feat: initial implementation"

# 10. Create release
git tag -a v0.1.0 -m "Release v0.1.0"
```

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-10-11 | Initial C manual creation |

---

## References

- [C11 Standard](https://www.iso.org/standard/57853.html)
- [CMake Documentation](https://cmake.org/documentation/)
- [GCC Manual](https://gcc.gnu.org/onlinedocs/)
- [Clang Documentation](https://clang.llvm.org/docs/)
- [Valgrind Manual](https://valgrind.org/docs/manual/)
- [Doxygen Manual](https://www.doxygen.nl/manual/)

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

