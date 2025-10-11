# AI Integration Manual - C++

**Version:** 1.0.0  
**Last Updated:** 2025-10-11  
**Target Audience:** AI Agents (LLM-based development assistants)  
**Language:** C++ (C++17/20/23)

---

## Table of Contents

1. [Introduction](#introduction)
2. [C++ Environment Setup](#cpp-environment-setup)
3. [Project Structure](#project-structure)
4. [Build System (CMake)](#build-system-cmake)
5. [Testing Framework](#testing-framework)
6. [Code Style & Formatting](#code-style--formatting)
7. [Documentation (Doxygen)](#documentation-doxygen)
8. [Package Management](#package-management)
9. [Implementation Guidelines](#implementation-guidelines)
10. [Memory Management](#memory-management)
11. [Error Handling](#error-handling)
12. [Concurrency & Threading](#concurrency--threading)
13. [Performance Optimization](#performance-optimization)
14. [Security Best Practices](#security-best-practices)
15. [Continuous Integration](#continuous-integration)
16. [Distribution & Deployment](#distribution--deployment)

---

## Introduction

This manual extends the base AI Integration Manual Template with C++-specific practices, tools, and workflows. It assumes familiarity with modern C++ (C++17 minimum, C++20/23 preferred) and focuses on producing high-quality, maintainable, and safe C++ code.

### Core Principles for C++

1. **Modern C++**: Use C++17/20/23 features; avoid C++03/11 legacy patterns
2. **RAII**: Resource Acquisition Is Initialization for all resource management
3. **Zero-Cost Abstractions**: Leverage templates and compile-time features
4. **Move Semantics**: Optimize resource transfer with move constructors/assignment
5. **Type Safety**: Prefer strong typing over void* and C-style casts
6. **Const Correctness**: Mark everything const that can be const
7. **Standard Library First**: Use STL before third-party libraries

---

## C++ Environment Setup

### Required Tools

```bash
# Compiler (choose one or more)
# GCC 11+ (recommended)
sudo apt install gcc-11 g++-11

# Clang 14+ (alternative)
sudo apt install clang-14 clang++-14

# MSVC 2019+ (Windows)
# Install via Visual Studio Installer

# Build System
sudo apt install cmake ninja-build

# Package Managers
# vcpkg
git clone https://github.com/microsoft/vcpkg.git
./vcpkg/bootstrap-vcpkg.sh

# Conan
pip install conan

# Code Quality Tools
sudo apt install clang-format clang-tidy cppcheck valgrind

# Documentation
sudo apt install doxygen graphviz

# Debugging
sudo apt install gdb lldb

# Profiling
sudo apt install valgrind perf linux-tools-generic
```

### Version Requirements

- **Minimum C++ Standard**: C++17
- **Recommended**: C++20 or C++23
- **CMake**: 3.20+
- **GCC**: 11+ or Clang 14+
- **vcpkg/Conan**: Latest stable

### Environment Configuration

```bash
# Set compiler
export CXX=/usr/bin/g++-11
export CC=/usr/bin/gcc-11

# Enable modern C++
export CXXFLAGS="-std=c++20 -Wall -Wextra -Wpedantic"

# Build type
export CMAKE_BUILD_TYPE=Release  # or Debug, RelWithDebInfo

# vcpkg integration
export VCPKG_ROOT=/path/to/vcpkg
```

---

## Project Structure

### Standard C++ Project Layout

```
project-root/
├── .github/
│   └── workflows/
│       ├── ci.yml
│       ├── sanitizers.yml
│       └── coverage.yml
├── .clang-format
├── .clang-tidy
├── .gitignore
├── CMakeLists.txt
├── conanfile.txt (or conanfile.py)
├── vcpkg.json
├── README.md
├── CHANGELOG.md
├── LICENSE
├── docs/
│   ├── ROADMAP.md
│   ├── SPECS.md
│   ├── specs/
│   ├── Doxyfile
│   └── user-guide/
├── include/
│   └── project_name/           # Public headers
│       ├── core.hpp
│       ├── types.hpp
│       └── utils.hpp
├── src/                        # Implementation files
│   ├── core.cpp
│   ├── types.cpp
│   └── utils.cpp
├── tests/
│   ├── CMakeLists.txt
│   ├── unit/
│   │   ├── test_core.cpp
│   │   └── test_utils.cpp
│   ├── integration/
│   ├── benchmarks/
│   └── fixtures/
├── examples/                   # Usage examples
│   ├── CMakeLists.txt
│   ├── basic_usage.cpp
│   └── advanced_usage.cpp
├── scripts/
│   ├── build.sh
│   ├── test.sh
│   ├── format.sh
│   └── analyze.sh
├── cmake/                      # CMake modules
│   ├── CompilerWarnings.cmake
│   ├── Sanitizers.cmake
│   └── Coverage.cmake
└── build/                      # Build artifacts (gitignored)
```

---

## Build System (CMake)

### Root CMakeLists.txt Template

```cmake
cmake_minimum_required(VERSION 3.20)

project(ProjectName 
    VERSION 1.0.0
    DESCRIPTION "Project description"
    LANGUAGES CXX
)

# C++ Standard
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Build options
option(BUILD_SHARED_LIBS "Build shared libraries" OFF)
option(BUILD_TESTING "Build tests" ON)
option(BUILD_EXAMPLES "Build examples" ON)
option(ENABLE_COVERAGE "Enable coverage reporting" OFF)
option(ENABLE_SANITIZERS "Enable sanitizers" OFF)

# Output directories
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# Include custom CMake modules
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")

# Compiler warnings
include(cmake/CompilerWarnings.cmake)
set_project_warnings()

# Sanitizers
if(ENABLE_SANITIZERS)
    include(cmake/Sanitizers.cmake)
    enable_sanitizers()
endif()

# Coverage
if(ENABLE_COVERAGE)
    include(cmake/Coverage.cmake)
    enable_coverage()
endif()

# Dependencies
find_package(Threads REQUIRED)

# vcpkg or Conan integration
if(EXISTS ${CMAKE_SOURCE_DIR}/vcpkg.json)
    # vcpkg integration
elseif(EXISTS ${CMAKE_SOURCE_DIR}/conanfile.txt)
    include(${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
    conan_basic_setup()
endif()

# Library target
add_library(${PROJECT_NAME}
    src/core.cpp
    src/types.cpp
    src/utils.cpp
)

target_include_directories(${PROJECT_NAME}
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
        $<INSTALL_INTERFACE:include>
    PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}/src
)

target_link_libraries(${PROJECT_NAME}
    PUBLIC
        Threads::Threads
)

# Set target properties
set_target_properties(${PROJECT_NAME} PROPERTIES
    VERSION ${PROJECT_VERSION}
    SOVERSION ${PROJECT_VERSION_MAJOR}
    CXX_VISIBILITY_PRESET hidden
    VISIBILITY_INLINES_HIDDEN YES
)

# Tests
if(BUILD_TESTING)
    enable_testing()
    add_subdirectory(tests)
endif()

# Examples
if(BUILD_EXAMPLES)
    add_subdirectory(examples)
endif()

# Installation
include(GNUInstallDirs)
install(TARGETS ${PROJECT_NAME}
    EXPORT ${PROJECT_NAME}Targets
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)

install(DIRECTORY include/ DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})

# Export targets
install(EXPORT ${PROJECT_NAME}Targets
    FILE ${PROJECT_NAME}Targets.cmake
    NAMESPACE ${PROJECT_NAME}::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}
)

# Package configuration
include(CMakePackageConfigHelpers)
write_basic_package_version_file(
    ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake
    VERSION ${PROJECT_VERSION}
    COMPATIBILITY SameMajorVersion
)

install(FILES
    ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}
)
```

### Compiler Warnings (cmake/CompilerWarnings.cmake)

```cmake
function(set_project_warnings)
    set(MSVC_WARNINGS
        /W4     # Warning level 4
        /WX     # Warnings as errors
        /w14640 # Enable warning on thread un-safe static member initialization
        /w14242 # 'identifier': conversion from 'type1' to 'type2', possible loss of data
        /w14254 # 'operator': conversion from 'type1:field_bits' to 'type2:field_bits'
        /w14263 # 'function': member function does not override any base class virtual member
        /w14265 # 'classname': class has virtual functions, but destructor is not virtual
        /w14287 # 'operator': unsigned/negative constant mismatch
        /w14296 # 'operator': expression is always 'boolean_value'
        /w14311 # 'variable': pointer truncation from 'type1' to 'type2'
        /w14545 # Expression before comma evaluates to a function which is missing an argument list
        /w14546 # Function call before comma missing argument list
        /w14547 # 'operator': operator before comma has no effect
        /w14549 # 'operator': operator before comma has no effect
        /w14555 # Expression has no effect
        /w14619 # Pragma warning: there is no warning number 'number'
        /w14826 # Conversion from 'type1' to 'type2' is sign-extended
        /w14905 # Wide string literal cast to 'LPSTR'
        /w14906 # String literal cast to 'LPWSTR'
        /w14928 # Illegal copy-initialization
        /permissive- # Standards conformance mode
    )

    set(GCC_CLANG_WARNINGS
        -Wall
        -Wextra
        -Wpedantic
        -Wshadow
        -Wnon-virtual-dtor
        -Wold-style-cast
        -Wcast-align
        -Wunused
        -Woverloaded-virtual
        -Wconversion
        -Wsign-conversion
        -Wmisleading-indentation
        -Wduplicated-cond
        -Wduplicated-branches
        -Wlogical-op
        -Wnull-dereference
        -Wuseless-cast
        -Wdouble-promotion
        -Wformat=2
        -Wimplicit-fallthrough
    )

    if(MSVC)
        set(PROJECT_WARNINGS ${MSVC_WARNINGS})
    else()
        set(PROJECT_WARNINGS ${GCC_CLANG_WARNINGS})
    endif()

    add_compile_options(${PROJECT_WARNINGS})
endfunction()
```

### Build Scripts

**scripts/build.sh**:
```bash
#!/bin/bash
set -e

BUILD_TYPE=${1:-Release}
BUILD_DIR="build"

echo "Building with CMake (${BUILD_TYPE})..."

# Create build directory
mkdir -p ${BUILD_DIR}
cd ${BUILD_DIR}

# Configure
cmake .. \
    -DCMAKE_BUILD_TYPE=${BUILD_TYPE} \
    -DBUILD_TESTING=ON \
    -DBUILD_EXAMPLES=ON \
    -GNinja

# Build
ninja -j$(nproc)

echo "Build complete!"
```

---

## Testing Framework

### Google Test Setup

**tests/CMakeLists.txt**:
```cmake
cmake_minimum_required(VERSION 3.20)

# Find Google Test
find_package(GTest REQUIRED)
include(GoogleTest)

# Helper function to create test
function(add_project_test TEST_NAME)
    add_executable(${TEST_NAME} ${ARGN})
    target_link_libraries(${TEST_NAME}
        PRIVATE
            ${PROJECT_NAME}
            GTest::gtest
            GTest::gtest_main
    )
    gtest_discover_tests(${TEST_NAME})
endfunction()

# Unit tests
add_project_test(test_core unit/test_core.cpp)
add_project_test(test_utils unit/test_utils.cpp)

# Integration tests
add_project_test(test_integration integration/test_integration.cpp)
```

### Test File Template

**tests/unit/test_core.cpp**:
```cpp
#include <gtest/gtest.h>
#include <project_name/core.hpp>

namespace project_name::test {

/**
 * @brief Test fixture for Core functionality
 */
class CoreTest : public ::testing::Test {
protected:
    void SetUp() override {
        // Setup code
    }

    void TearDown() override {
        // Cleanup code
    }

    // Test fixtures
};

TEST_F(CoreTest, ConstructorInitializesCorrectly) {
    // Arrange
    const int expected_value = 42;

    // Act
    Core core(expected_value);

    // Assert
    EXPECT_EQ(core.get_value(), expected_value);
}

TEST_F(CoreTest, ProcessThrowsOnInvalidInput) {
    // Arrange
    Core core;

    // Act & Assert
    EXPECT_THROW(core.process(-1), std::invalid_argument);
}

TEST_F(CoreTest, ProcessHandlesEdgeCases) {
    // Arrange
    Core core;

    // Act & Assert - Zero
    EXPECT_NO_THROW(core.process(0));
    
    // Act & Assert - Maximum value
    EXPECT_NO_THROW(core.process(std::numeric_limits<int>::max()));
}

/**
 * @brief Parameterized test for multiple inputs
 */
class CoreParamTest : public ::testing::TestWithParam<int> {};

TEST_P(CoreParamTest, ProcessHandlesVariousInputs) {
    // Arrange
    Core core;
    int input = GetParam();

    // Act
    auto result = core.process(input);

    // Assert
    EXPECT_GT(result, 0);
}

INSTANTIATE_TEST_SUITE_P(
    ValidInputs,
    CoreParamTest,
    ::testing::Values(1, 10, 100, 1000)
);

} // namespace project_name::test
```

### Test Execution Script

**scripts/test.sh**:
```bash
#!/bin/bash
set -e

BUILD_DIR="build"
cd ${BUILD_DIR}

echo "Running tests..."
ctest --output-on-failure --parallel $(nproc)

echo "Generating coverage report..."
if [ -f "coverage.info" ]; then
    lcov --directory . --capture --output-file coverage.info
    lcov --remove coverage.info '/usr/*' --output-file coverage.info
    lcov --list coverage.info
    genhtml coverage.info --output-directory coverage_html
    echo "Coverage report generated in coverage_html/"
fi
```

---

## Code Style & Formatting

### .clang-format

```yaml
---
Language: Cpp
BasedOnStyle: LLVM
AccessModifierOffset: -4
AlignAfterOpenBracket: Align
AlignConsecutiveAssignments: false
AlignConsecutiveDeclarations: false
AlignEscapedNewlines: Right
AlignOperands: true
AlignTrailingComments: true
AllowAllParametersOfDeclarationOnNextLine: true
AllowShortBlocksOnASingleLine: false
AllowShortCaseLabelsOnASingleLine: false
AllowShortFunctionsOnASingleLine: Inline
AllowShortIfStatementsOnASingleLine: false
AllowShortLoopsOnASingleLine: false
AlwaysBreakAfterReturnType: None
AlwaysBreakBeforeMultilineStrings: false
AlwaysBreakTemplateDeclarations: Yes
BinPackArguments: true
BinPackParameters: true
BraceWrapping:
  AfterClass: false
  AfterControlStatement: false
  AfterEnum: false
  AfterFunction: false
  AfterNamespace: false
  AfterStruct: false
  AfterUnion: false
  AfterExternBlock: false
  BeforeCatch: false
  BeforeElse: false
  IndentBraces: false
  SplitEmptyFunction: false
  SplitEmptyRecord: false
  SplitEmptyNamespace: false
BreakBeforeBinaryOperators: None
BreakBeforeBraces: Attach
BreakBeforeTernaryOperators: true
BreakConstructorInitializers: BeforeColon
BreakInheritanceList: BeforeColon
BreakStringLiterals: true
ColumnLimit: 100
CommentPragmas: '^ IWYU pragma:'
CompactNamespaces: false
ConstructorInitializerIndentWidth: 4
ContinuationIndentWidth: 4
Cpp11BracedListStyle: true
DerivePointerAlignment: false
FixNamespaceComments: true
IncludeBlocks: Regroup
IncludeCategories:
  - Regex: '^<ext/.*\.h>'
    Priority: 2
  - Regex: '^<.*\.h>'
    Priority: 1
  - Regex: '^<.*'
    Priority: 2
  - Regex: '.*'
    Priority: 3
IndentCaseLabels: true
IndentPPDirectives: None
IndentWidth: 4
IndentWrappedFunctionNames: false
KeepEmptyLinesAtTheStartOfBlocks: false
MaxEmptyLinesToKeep: 1
NamespaceIndentation: None
PointerAlignment: Left
ReflowComments: true
SortIncludes: true
SortUsingDeclarations: true
SpaceAfterCStyleCast: false
SpaceAfterTemplateKeyword: true
SpaceBeforeAssignmentOperators: true
SpaceBeforeCpp11BracedList: false
SpaceBeforeCtorInitializerColon: true
SpaceBeforeInheritanceColon: true
SpaceBeforeParens: ControlStatements
SpaceBeforeRangeBasedForLoopColon: true
SpaceInEmptyParentheses: false
SpacesBeforeTrailingComments: 1
SpacesInAngles: false
SpacesInCStyleCastParentheses: false
SpacesInContainerLiterals: false
SpacesInParentheses: false
SpacesInSquareBrackets: false
Standard: c++20
TabWidth: 4
UseTab: Never
```

### .clang-tidy

```yaml
---
Checks: >
  *,
  -fuchsia-*,
  -google-*,
  -zircon-*,
  -abseil-*,
  -modernize-use-trailing-return-type,
  -llvmlibc-*,
  -altera-*,
  -misc-non-private-member-variables-in-classes,
  -readability-identifier-length,
  -readability-magic-numbers,
  -cppcoreguidelines-avoid-magic-numbers

WarningsAsErrors: '*'

HeaderFilterRegex: '.*'

CheckOptions:
  - key: readability-identifier-naming.NamespaceCase
    value: lower_case
  - key: readability-identifier-naming.ClassCase
    value: CamelCase
  - key: readability-identifier-naming.StructCase
    value: CamelCase
  - key: readability-identifier-naming.FunctionCase
    value: lower_case
  - key: readability-identifier-naming.VariableCase
    value: lower_case
  - key: readability-identifier-naming.ConstantCase
    value: UPPER_CASE
  - key: readability-identifier-naming.ParameterCase
    value: lower_case
  - key: readability-identifier-naming.EnumCase
    value: CamelCase
  - key: readability-identifier-naming.EnumConstantCase
    value: UPPER_CASE
  - key: readability-identifier-naming.MemberCase
    value: lower_case
  - key: readability-identifier-naming.MemberSuffix
    value: '_'
  - key: readability-identifier-naming.PrivateMemberSuffix
    value: '_'
  - key: readability-identifier-naming.ProtectedMemberSuffix
    value: '_'
  - key: modernize-use-nullptr.NullMacros
    value: 'NULL'
```

### Formatting Script

**scripts/format.sh**:
```bash
#!/bin/bash
set -e

echo "Formatting C++ code..."

# Find all C++ files
find include src tests examples -type f \( -name "*.cpp" -o -name "*.hpp" -o -name "*.h" \) \
    -exec clang-format -i {} \;

echo "Formatting complete!"

# Run clang-tidy
echo "Running static analysis..."
clang-tidy src/*.cpp tests/unit/*.cpp -- -std=c++20 -Iinclude

echo "Analysis complete!"
```

---

## Documentation (Doxygen)

### Doxyfile Configuration

**docs/Doxyfile**:
```
# Project information
PROJECT_NAME           = "Project Name"
PROJECT_NUMBER         = 1.0.0
PROJECT_BRIEF          = "Brief project description"

# Input/Output
INPUT                  = ../include ../src README.md
OUTPUT_DIRECTORY       = ./doxygen
RECURSIVE              = YES
FILE_PATTERNS          = *.hpp *.cpp *.h *.md

# Build options
EXTRACT_ALL            = YES
EXTRACT_PRIVATE        = NO
EXTRACT_STATIC         = YES
EXTRACT_LOCAL_CLASSES  = YES

# Warnings
WARNINGS               = YES
WARN_IF_UNDOCUMENTED   = YES
WARN_IF_DOC_ERROR      = YES

# Output formats
GENERATE_HTML          = YES
GENERATE_LATEX         = NO
HTML_OUTPUT            = html
HTML_FILE_EXTENSION    = .html

# Graphs
HAVE_DOT               = YES
UML_LOOK               = YES
CALL_GRAPH             = YES
CALLER_GRAPH           = YES
DOT_IMAGE_FORMAT       = svg
```

### Documentation Standards

**Header file example (include/project_name/core.hpp)**:
```cpp
/**
 * @file core.hpp
 * @brief Core functionality for Project Name
 * @author AI Agent
 * @version 1.0.0
 * @date 2025-10-11
 * 
 * @copyright Copyright (c) 2025
 * 
 * Detailed description of what this header provides.
 */

#pragma once

#include <cstdint>
#include <string>
#include <memory>
#include <optional>

namespace project_name {

/**
 * @brief Core class providing main functionality
 * 
 * Detailed description of the Core class, its purpose,
 * and how it fits into the overall architecture.
 * 
 * Example usage:
 * @code
 * Core core(42);
 * auto result = core.process(10);
 * @endcode
 * 
 * @see Helper
 * @note This class is thread-safe
 * @warning Do not use with negative values
 */
class Core {
public:
    /**
     * @brief Construct a new Core object
     * 
     * @param initial_value The initial value (must be > 0)
     * @throws std::invalid_argument if initial_value <= 0
     */
    explicit Core(int initial_value);

    /**
     * @brief Destroy the Core object
     */
    ~Core();

    // Prevent copying
    Core(const Core&) = delete;
    Core& operator=(const Core&) = delete;

    // Allow moving
    Core(Core&&) noexcept;
    Core& operator=(Core&&) noexcept;

    /**
     * @brief Process an input value
     * 
     * Performs complex processing on the input value based
     * on the internal state.
     * 
     * @param input The input value to process
     * @return int The processed result
     * 
     * @throws std::invalid_argument if input is invalid
     * @throws std::runtime_error if processing fails
     * 
     * @par Complexity
     * O(log n) where n is the input value
     * 
     * @par Thread Safety
     * This method is thread-safe
     */
    [[nodiscard]] int process(int input);

    /**
     * @brief Get the current value
     * 
     * @return int The current internal value
     * @note This is a const operation
     */
    [[nodiscard]] int get_value() const noexcept;

private:
    class Impl; ///< Forward declaration for PIMPL idiom
    std::unique_ptr<Impl> impl_; ///< Implementation pointer
};

/**
 * @brief Helper enumeration for operation modes
 */
enum class OperationMode : uint8_t {
    FAST,    ///< Fast mode with reduced accuracy
    NORMAL,  ///< Normal balanced mode
    ACCURATE ///< Accurate mode with increased precision
};

/**
 * @brief Convert OperationMode to string
 * 
 * @param mode The mode to convert
 * @return std::string String representation
 */
[[nodiscard]] std::string to_string(OperationMode mode);

} // namespace project_name
```

---

## Package Management

### vcpkg.json

```json
{
  "name": "project-name",
  "version": "1.0.0",
  "description": "Project description",
  "homepage": "https://github.com/org/project",
  "license": "MIT",
  "dependencies": [
    {
      "name": "fmt",
      "version>=": "10.0.0"
    },
    {
      "name": "spdlog",
      "version>=": "1.12.0"
    },
    {
      "name": "nlohmann-json",
      "version>=": "3.11.0"
    },
    {
      "name": "catch2",
      "version>=": "3.4.0"
    }
  ],
  "dev-dependencies": [
    {
      "name": "gtest",
      "version>=": "1.14.0"
    },
    {
      "name": "benchmark",
      "version>=": "1.8.0"
    }
  ]
}
```

### Conanfile.txt (Alternative)

```ini
[requires]
fmt/10.1.1
spdlog/1.12.0
nlohmann_json/3.11.2

[generators]
CMakeDeps
CMakeToolchain

[options]
fmt:shared=False
spdlog:shared=False
```

### Installation Script

**scripts/install-deps.sh**:
```bash
#!/bin/bash
set -e

# vcpkg
if [ -d "${VCPKG_ROOT}" ]; then
    echo "Installing dependencies with vcpkg..."
    ${VCPKG_ROOT}/vcpkg install
fi

# Conan
if [ -f "conanfile.txt" ]; then
    echo "Installing dependencies with Conan..."
    mkdir -p build
    cd build
    conan install .. --build=missing -s build_type=Release
    cd ..
fi

echo "Dependencies installed!"
```

---

## Implementation Guidelines

### Modern C++ Best Practices

#### Use Smart Pointers

```cpp
// ❌ Bad - Manual memory management
Widget* widget = new Widget();
// ... use widget ...
delete widget;

// ✅ Good - RAII with unique_ptr
auto widget = std::make_unique<Widget>();
// ... use widget ...
// Automatically deleted

// ✅ Good - Shared ownership
auto shared_widget = std::make_shared<Widget>();
```

#### Use Structured Bindings (C++17)

```cpp
// ❌ Bad
std::pair<int, std::string> get_data();
auto result = get_data();
int id = result.first;
std::string name = result.second;

// ✅ Good
auto [id, name] = get_data();
```

#### Use std::optional for Optional Values

```cpp
// ❌ Bad - Using nullptr or sentinel values
Widget* find_widget(int id) {
    // ...
    return nullptr; // Indicates not found
}

// ✅ Good - Clear intent
std::optional<Widget> find_widget(int id) {
    // ...
    return std::nullopt; // Clearly indicates absence
}

// Usage
if (auto widget = find_widget(42)) {
    // Use *widget
}
```

#### Use std::variant for Type-Safe Unions

```cpp
#include <variant>

// ✅ Good - Type-safe variant
using Value = std::variant<int, double, std::string>;

Value process(const Value& input) {
    return std::visit([](auto&& arg) -> Value {
        using T = std::decay_t<decltype(arg)>;
        if constexpr (std::is_same_v<T, int>) {
            return arg * 2;
        } else if constexpr (std::is_same_v<T, double>) {
            return arg * 1.5;
        } else {
            return arg + "_processed";
        }
    }, input);
}
```

#### Use Concepts (C++20)

```cpp
#include <concepts>

// Define concept
template<typename T>
concept Numeric = std::integral<T> || std::floating_point<T>;

// Use concept
template<Numeric T>
T square(T value) {
    return value * value;
}

// Or as constraint
template<typename T>
    requires std::copyable<T> && std::equality_comparable<T>
class Container {
    // ...
};
```

#### Use Ranges (C++20)

```cpp
#include <ranges>
#include <vector>
#include <algorithm>

std::vector<int> numbers = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

// ✅ Good - Ranges
auto even_squares = numbers 
    | std::views::filter([](int n) { return n % 2 == 0; })
    | std::views::transform([](int n) { return n * n; });

for (int value : even_squares) {
    // Use value
}
```

---

## Memory Management

### RAII Principles

```cpp
/**
 * @brief RAII wrapper for file handle
 */
class FileHandle {
public:
    explicit FileHandle(const std::string& filename)
        : file_(std::fopen(filename.c_str(), "r")) {
        if (!file_) {
            throw std::runtime_error("Failed to open file");
        }
    }

    ~FileHandle() {
        if (file_) {
            std::fclose(file_);
        }
    }

    // Delete copy operations
    FileHandle(const FileHandle&) = delete;
    FileHandle& operator=(const FileHandle&) = delete;

    // Move operations
    FileHandle(FileHandle&& other) noexcept 
        : file_(std::exchange(other.file_, nullptr)) {}

    FileHandle& operator=(FileHandle&& other) noexcept {
        if (this != &other) {
            if (file_) {
                std::fclose(file_);
            }
            file_ = std::exchange(other.file_, nullptr);
        }
        return *this;
    }

    FILE* get() const noexcept { return file_; }

private:
    FILE* file_;
};
```

### Custom Deleters

```cpp
// Custom deleter for C API resources
struct CResourceDeleter {
    void operator()(CResource* resource) const {
        if (resource) {
            c_resource_destroy(resource);
        }
    }
};

using CResourcePtr = std::unique_ptr<CResource, CResourceDeleter>;

CResourcePtr create_resource() {
    return CResourcePtr(c_resource_create());
}
```

### Memory Safety Tools

**scripts/memcheck.sh**:
```bash
#!/bin/bash
set -e

BUILD_DIR="build"

# Valgrind memory check
echo "Running Valgrind..."
valgrind --leak-check=full \
         --show-leak-kinds=all \
         --track-origins=yes \
         --verbose \
         --log-file=valgrind-out.txt \
         ./${BUILD_DIR}/bin/test_all

# AddressSanitizer (rebuild required)
echo "Building with AddressSanitizer..."
cd ${BUILD_DIR}
cmake .. -DCMAKE_CXX_FLAGS="-fsanitize=address -fno-omit-frame-pointer"
ninja
./bin/test_all

# ThreadSanitizer
echo "Building with ThreadSanitizer..."
cmake .. -DCMAKE_CXX_FLAGS="-fsanitize=thread"
ninja
./bin/test_all

echo "Memory checks complete!"
```

---

## Error Handling

### Exception Hierarchy

```cpp
namespace project_name {

/**
 * @brief Base exception for all project errors
 */
class Exception : public std::exception {
public:
    explicit Exception(std::string message) 
        : message_(std::move(message)) {}

    [[nodiscard]] const char* what() const noexcept override {
        return message_.c_str();
    }

protected:
    std::string message_;
};

/**
 * @brief Invalid argument exception
 */
class InvalidArgument : public Exception {
public:
    explicit InvalidArgument(const std::string& arg_name, const std::string& reason)
        : Exception("Invalid argument '" + arg_name + "': " + reason) {}
};

/**
 * @brief Runtime error exception
 */
class RuntimeError : public Exception {
public:
    explicit RuntimeError(const std::string& message)
        : Exception(message) {}
};

/**
 * @brief IO error exception
 */
class IoError : public Exception {
public:
    explicit IoError(const std::string& filename, const std::string& reason)
        : Exception("IO error with file '" + filename + "': " + reason) {}
};

} // namespace project_name
```

### Error Handling with std::expected (C++23)

```cpp
#include <expected>
#include <system_error>

enum class ParseError {
    INVALID_FORMAT,
    OUT_OF_RANGE,
    UNEXPECTED_CHARACTER
};

std::expected<int, ParseError> parse_int(const std::string& str) {
    try {
        size_t pos;
        int value = std::stoi(str, &pos);
        
        if (pos != str.length()) {
            return std::unexpected(ParseError::UNEXPECTED_CHARACTER);
        }
        
        return value;
    } catch (const std::invalid_argument&) {
        return std::unexpected(ParseError::INVALID_FORMAT);
    } catch (const std::out_of_range&) {
        return std::unexpected(ParseError::OUT_OF_RANGE);
    }
}

// Usage
auto result = parse_int("42");
if (result) {
    std::cout << "Parsed: " << *result << '\n';
} else {
    std::cerr << "Parse error: " << static_cast<int>(result.error()) << '\n';
}
```

### noexcept Specification

```cpp
// Functions that should never throw
class Widget {
public:
    // Destructors should always be noexcept
    ~Widget() noexcept;

    // Move operations should be noexcept when possible
    Widget(Widget&&) noexcept;
    Widget& operator=(Widget&&) noexcept;

    // Swap should be noexcept
    void swap(Widget& other) noexcept;

    // Getters of simple types can be noexcept
    [[nodiscard]] int get_id() const noexcept;
};
```

---

## Concurrency & Threading

### Thread-Safe Singleton

```cpp
#include <mutex>

class ThreadSafeManager {
public:
    // Meyer's Singleton (C++11 thread-safe)
    static ThreadSafeManager& instance() {
        static ThreadSafeManager instance;
        return instance;
    }

    void do_work() {
        std::lock_guard<std::mutex> lock(mutex_);
        // Thread-safe work
    }

private:
    ThreadSafeManager() = default;
    ~ThreadSafeManager() = default;

    // Delete copy and move
    ThreadSafeManager(const ThreadSafeManager&) = delete;
    ThreadSafeManager& operator=(const ThreadSafeManager&) = delete;
    ThreadSafeManager(ThreadSafeManager&&) = delete;
    ThreadSafeManager& operator=(ThreadSafeManager&&) = delete;

    mutable std::mutex mutex_;
};
```

### Thread Pool Pattern

```cpp
#include <thread>
#include <queue>
#include <functional>
#include <condition_variable>

class ThreadPool {
public:
    explicit ThreadPool(size_t num_threads) : stop_(false) {
        for (size_t i = 0; i < num_threads; ++i) {
            workers_.emplace_back([this] {
                while (true) {
                    std::function<void()> task;
                    
                    {
                        std::unique_lock<std::mutex> lock(queue_mutex_);
                        condition_.wait(lock, [this] {
                            return stop_ || !tasks_.empty();
                        });

                        if (stop_ && tasks_.empty()) {
                            return;
                        }

                        task = std::move(tasks_.front());
                        tasks_.pop();
                    }

                    task();
                }
            });
        }
    }

    ~ThreadPool() {
        {
            std::unique_lock<std::mutex> lock(queue_mutex_);
            stop_ = true;
        }
        condition_.notify_all();
        
        for (auto& worker : workers_) {
            if (worker.joinable()) {
                worker.join();
            }
        }
    }

    template<typename F>
    void enqueue(F&& task) {
        {
            std::unique_lock<std::mutex> lock(queue_mutex_);
            tasks_.emplace(std::forward<F>(task));
        }
        condition_.notify_one();
    }

private:
    std::vector<std::thread> workers_;
    std::queue<std::function<void()>> tasks_;
    std::mutex queue_mutex_;
    std::condition_variable condition_;
    bool stop_;
};
```

### Atomic Operations

```cpp
#include <atomic>

class Counter {
public:
    void increment() {
        count_.fetch_add(1, std::memory_order_relaxed);
    }

    [[nodiscard]] int get() const {
        return count_.load(std::memory_order_acquire);
    }

private:
    std::atomic<int> count_{0};
};
```

---

## Performance Optimization

### Profiling Script

**scripts/profile.sh**:
```bash
#!/bin/bash
set -e

BUILD_DIR="build"
EXECUTABLE="${BUILD_DIR}/bin/benchmark"

# Build with profiling symbols
cd ${BUILD_DIR}
cmake .. -DCMAKE_BUILD_TYPE=RelWithDebInfo
ninja

# perf profiling (Linux)
echo "Running perf profiling..."
perf record -g ${EXECUTABLE}
perf report

# gprof profiling
echo "Running gprof profiling..."
g++ -pg -o ${EXECUTABLE}_prof src/*.cpp
./${EXECUTABLE}_prof
gprof ${EXECUTABLE}_prof gmon.out > profile.txt

# Valgrind callgrind
echo "Running callgrind..."
valgrind --tool=callgrind --callgrind-out-file=callgrind.out ${EXECUTABLE}
kcachegrind callgrind.out

echo "Profiling complete!"
```

### Optimization Hints

```cpp
// Branch prediction hints
#define LIKELY(x)   __builtin_expect(!!(x), 1)
#define UNLIKELY(x) __builtin_expect(!!(x), 0)

if (UNLIKELY(error_condition)) {
    handle_error();
}

// Force inline
#define FORCE_INLINE __attribute__((always_inline)) inline

FORCE_INLINE int fast_function(int x) {
    return x * x;
}

// Restrict pointer aliasing
void process(int* __restrict__ a, const int* __restrict__ b, size_t n) {
    for (size_t i = 0; i < n; ++i) {
        a[i] = b[i] * 2;
    }
}

// Cache alignment
struct alignas(64) CacheAligned {
    int data[16];
};
```

### Google Benchmark Integration

**tests/benchmarks/benchmark_core.cpp**:
```cpp
#include <benchmark/benchmark.h>
#include <project_name/core.hpp>

static void BM_CoreProcess(benchmark::State& state) {
    Core core(42);
    
    for (auto _ : state) {
        benchmark::DoNotOptimize(core.process(state.range(0)));
    }
    
    state.SetComplexityN(state.range(0));
}

BENCHMARK(BM_CoreProcess)
    ->Range(8, 8<<10)
    ->Complexity();

BENCHMARK_MAIN();
```

---

## Security Best Practices

### Input Validation

```cpp
/**
 * @brief Validate and sanitize user input
 */
class InputValidator {
public:
    static bool validate_integer(const std::string& input, int& output) {
        try {
            size_t pos;
            output = std::stoi(input, &pos);
            return pos == input.length();
        } catch (...) {
            return false;
        }
    }

    static std::string sanitize_filename(const std::string& filename) {
        static const std::regex invalid_chars(R"([<>:"/\\|?*])");
        return std::regex_replace(filename, invalid_chars, "_");
    }
};
```

### Secure Coding Guidelines

```cpp
// ✅ Use secure functions
#include <cstring>

// ❌ Bad - Buffer overflow risk
char buffer[100];
strcpy(buffer, user_input);

// ✅ Good - Bounds checking
char buffer[100];
strncpy(buffer, user_input, sizeof(buffer) - 1);
buffer[sizeof(buffer) - 1] = '\0';

// ✅ Better - Use std::string
std::string buffer = user_input;

// ✅ Prevent integer overflow
bool safe_multiply(int a, int b, int& result) {
    if (a > 0 && b > 0 && a > std::numeric_limits<int>::max() / b) {
        return false; // Overflow
    }
    result = a * b;
    return true;
}

// ✅ Zero sensitive memory
void clear_sensitive_data(void* ptr, size_t size) {
    volatile unsigned char* p = static_cast<volatile unsigned char*>(ptr);
    while (size--) {
        *p++ = 0;
    }
}
```

---

## Continuous Integration

### GitHub Actions Workflow

**.github/workflows/ci.yml**:
```yaml
name: C++ CI

on:
  push:
    branches: [ main, develop, feature/* ]
  pull_request:
    branches: [ main, develop ]

jobs:
  build-linux:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        compiler: [gcc-11, clang-14]
        build_type: [Debug, Release]
    
    steps:
      - uses: actions/checkout@v3
        with:
          submodules: recursive

      - name: Install dependencies
        run: |
          sudo apt-get update
          sudo apt-get install -y cmake ninja-build ${{ matrix.compiler }}

      - name: Configure CMake
        run: |
          cmake -B build -G Ninja \
            -DCMAKE_BUILD_TYPE=${{ matrix.build_type }} \
            -DCMAKE_CXX_COMPILER=${{ matrix.compiler }} \
            -DBUILD_TESTING=ON \
            -DENABLE_COVERAGE=ON

      - name: Build
        run: cmake --build build --parallel

      - name: Run tests
        run: cd build && ctest --output-on-failure

      - name: Generate coverage
        if: matrix.build_type == 'Debug'
        run: |
          cd build
          lcov --capture --directory . --output-file coverage.info
          lcov --remove coverage.info '/usr/*' --output-file coverage.info
          lcov --list coverage.info

      - name: Upload coverage
        if: matrix.build_type == 'Debug'
        uses: codecov/codecov-action@v3
        with:
          files: ./build/coverage.info

  sanitizers:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        sanitizer: [address, thread, undefined]
    
    steps:
      - uses: actions/checkout@v3

      - name: Build with ${{ matrix.sanitizer }} sanitizer
        run: |
          cmake -B build -G Ninja \
            -DCMAKE_BUILD_TYPE=Debug \
            -DCMAKE_CXX_FLAGS="-fsanitize=${{ matrix.sanitizer }} -fno-omit-frame-pointer"
          cmake --build build

      - name: Run tests
        run: cd build && ctest --output-on-failure

  static-analysis:
    runs-on: ubuntu-latest
    
    steps:
      - uses: actions/checkout@v3

      - name: Install tools
        run: |
          sudo apt-get update
          sudo apt-get install -y clang-tidy cppcheck

      - name: Run clang-tidy
        run: |
          clang-tidy src/*.cpp tests/unit/*.cpp -- -std=c++20 -Iinclude

      - name: Run cppcheck
        run: |
          cppcheck --enable=all --inconclusive --std=c++20 \
            -I include src/ tests/

  format-check:
    runs-on: ubuntu-latest
    
    steps:
      - uses: actions/checkout@v3

      - name: Check formatting
        run: |
          find include src tests -type f \( -name "*.cpp" -o -name "*.hpp" \) \
            -exec clang-format --dry-run --Werror {} \;
```

---

## Distribution & Deployment

### Package Script

**scripts/package.sh**:
```bash
#!/bin/bash
set -e

VERSION=$(cat VERSION)
BUILD_DIR="build"

echo "Packaging version ${VERSION}..."

# Build release
cd ${BUILD_DIR}
cmake .. -DCMAKE_BUILD_TYPE=Release
ninja

# Create package
cpack -G "TGZ;DEB;RPM"

# Create archives
tar -czf ../project-name-${VERSION}-linux-x86_64.tar.gz \
    bin/ lib/ include/

echo "Packaging complete!"
```

### CPack Configuration

Add to root CMakeLists.txt:
```cmake
# CPack configuration
include(CPack)
set(CPACK_PACKAGE_NAME ${PROJECT_NAME})
set(CPACK_PACKAGE_VERSION ${PROJECT_VERSION})
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY ${PROJECT_DESCRIPTION})
set(CPACK_PACKAGE_VENDOR "HiveLLM")
set(CPACK_PACKAGE_CONTACT "contact@hivellm.org")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_SOURCE_DIR}/LICENSE")
set(CPACK_RESOURCE_FILE_README "${CMAKE_SOURCE_DIR}/README.md")

# Debian package
set(CPACK_DEBIAN_PACKAGE_DEPENDS "libc6 (>= 2.31)")
set(CPACK_DEBIAN_PACKAGE_MAINTAINER "HiveLLM Team")

# RPM package
set(CPACK_RPM_PACKAGE_LICENSE "MIT")
set(CPACK_RPM_PACKAGE_GROUP "Development/Libraries")
```

---

## Quick Reference

### Essential Commands

```bash
# Setup
./scripts/install-deps.sh

# Build
./scripts/build.sh Release

# Test
./scripts/test.sh

# Format
./scripts/format.sh

# Analyze
clang-tidy src/*.cpp -- -std=c++20 -Iinclude

# Profile
./scripts/profile.sh

# Package
./scripts/package.sh

# Documentation
cd docs && doxygen Doxyfile
```

### Checklist for Implementation

- [ ] Project structure created
- [ ] CMakeLists.txt configured
- [ ] Dependencies installed (vcpkg/Conan)
- [ ] .clang-format configured
- [ ] .clang-tidy configured
- [ ] Doxygen configured
- [ ] Tests written (Google Test)
- [ ] All tests passing
- [ ] Code formatted
- [ ] Static analysis clean
- [ ] Memory leaks checked (Valgrind)
- [ ] Sanitizers passed
- [ ] Documentation generated
- [ ] CHANGELOG updated
- [ ] Version tagged

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-10-11 | Initial C++ manual creation |

---

## References

- [C++ Core Guidelines](https://isocpp.github.io/CppCoreGuidelines/)
- [Google C++ Style Guide](https://google.github.io/styleguide/cppguide.html)
- [CMake Documentation](https://cmake.org/documentation/)
- [Doxygen Manual](https://www.doxygen.nl/manual/)
- [Google Test Primer](https://google.github.io/googletest/primer.html)

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

