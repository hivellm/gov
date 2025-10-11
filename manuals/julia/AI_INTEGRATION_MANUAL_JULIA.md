# AI Integration Manual - Julia

**Version**: 1.0.0  
**Last Updated**: 2025-10-11  
**Language**: Julia 1.10+  
**Target Audience**: AI Agents (LLM-based development assistants)

---

## Table of Contents

1. [Introduction](#introduction)
2. [Quick Start](#quick-start)
3. [Julia-Specific Setup](#julia-specific-setup)
4. [Configuration Standards](#configuration-standards)
5. [Source Code Standards](#source-code-standards)
6. [Testing Standards](#testing-standards)
7. [Build & Deployment](#build--deployment)
8. [Documentation](#documentation)
9. [Julia Best Practices](#julia-best-practices)
10. [Common Patterns](#common-patterns)
11. [Troubleshooting](#troubleshooting)
12. [Complete Workflow](#complete-workflow)

---

## Introduction

This manual extends the [AI Integration Manual Template](../AI_INTEGRATION_MANUAL_TEMPLATE.md) with Julia-specific implementations.

**When to use this manual**:
- Scientific computing and numerical analysis
- Data science and machine learning
- High-performance computing (HPC)
- Mathematical modeling and optimization
- Statistical analysis and visualization
- Differential equations and simulations
- Bioinformatics and computational biology
- Package development for Julia ecosystem

**Prerequisites**:
- Read [AI Integration Manual Template](../AI_INTEGRATION_MANUAL_TEMPLATE.md)
- Read [Language Standards](../LANGUAGE_STANDARDS.md)
- Basic Julia knowledge

---

## Quick Start

### Minimum Viable Project Setup

```bash
# 1. Create project directory
mkdir MyProject && cd MyProject

# 2. Start Julia REPL
julia

# 3. Activate package mode (press ])
]

# 4. Generate new package
generate MyProject

# 5. Exit package mode (press backspace)

# 6. Exit Julia (Ctrl+D)

# 7. Navigate to package
cd MyProject

# 8. Project structure created:
# MyProject/
# ├── Project.toml    # Package manifest
# ├── src/
# │   └── MyProject.jl
# └── test/
#     └── runtests.jl

# 9. Open Julia in project
julia --project=.

# 10. Add dependencies
]
add DataFrames, Plots, HTTP

# 11. Run tests
test
```

### Library Project

```bash
# Create package
julia

# In Julia REPL
using Pkg
Pkg.generate("MyLibrary")
cd("MyLibrary")

# Activate project
Pkg.activate(".")

# Add dependencies
Pkg.add("LinearAlgebra")
Pkg.add("Statistics")

# Add test dependencies
Pkg.add("Test"; target=:test)

# Run tests
Pkg.test()
```

---

## Julia-Specific Setup

### 1. Environment Setup

#### Install Julia

**Linux (Ubuntu/Debian)**:
```bash
# Download Julia
wget https://julialang-s3.julialang.org/bin/linux/x64/1.10/julia-1.10.0-linux-x86_64.tar.gz

# Extract
tar -xvzf julia-1.10.0-linux-x86_64.tar.gz

# Move to /opt
sudo mv julia-1.10.0 /opt/julia

# Create symlink
sudo ln -s /opt/julia/bin/julia /usr/local/bin/julia

# Verify installation
julia --version
```

**macOS**:
```bash
# Using Homebrew
brew install julia

# Or download from julialang.org
# Drag Julia.app to Applications

# Verify installation
julia --version
```

**Windows**:
```powershell
# Using Chocolatey
choco install julia

# Or using winget
winget install Julia.Julia

# Or download installer from julialang.org

# Verify installation
julia --version
```

#### Install Development Tools

```julia
# In Julia REPL
using Pkg

# Revise.jl (auto-reload code changes)
Pkg.add("Revise")

# JuliaFormatter (code formatter)
Pkg.add("JuliaFormatter")

# Documenter.jl (documentation)
Pkg.add("Documenter")

# BenchmarkTools (performance testing)
Pkg.add("BenchmarkTools")

# ProfileView (profiling)
Pkg.add("ProfileView")

# Debugger
Pkg.add("Debugger")

# VS Code Julia extension
# Install from VS Code marketplace
```

#### Configure Julia Environment

```bash
# Create startup.jl for auto-loading packages
mkdir -p ~/.julia/config
cat > ~/.julia/config/startup.jl << 'EOF'
# Auto-load Revise for development
try
    using Revise
catch e
    @warn "Error loading Revise" exception=(e, catch_backtrace())
end

# Enable stack traces
ENV["JULIA_DEBUG"] = "loading"

# Set number of threads
ENV["JULIA_NUM_THREADS"] = "auto"
EOF
```

### 2. Project Structure

```
MyProject/
├── .github/
│   └── workflows/
│       ├── CI.yml
│       ├── Documentation.yml
│       └── TagBot.yml
├── .gitignore
├── LICENSE
├── README.md
├── CHANGELOG.md
├── Project.toml          # Package dependencies
├── Manifest.toml         # Exact dependency versions (gitignored)
├── docs/
│   ├── make.jl
│   ├── Project.toml
│   └── src/
│       ├── index.md
│       └── api.md
├── src/
│   ├── MyProject.jl      # Main module file
│   ├── types.jl          # Type definitions
│   ├── functions.jl      # Function implementations
│   └── utils.jl          # Utility functions
├── test/
│   ├── runtests.jl       # Test runner
│   ├── test_module1.jl
│   └── test_utils.jl
├── examples/
│   └── example1.jl
├── benchmark/
│   └── benchmarks.jl
└── scripts/
    └── setup.jl
```

### 3. Version Management

**Using juliaup (recommended)**:
```bash
# Install juliaup
curl -fsSL https://install.julialang.org | sh

# Install specific Julia version
juliaup add 1.10

# Set default version
juliaup default 1.10

# List installed versions
juliaup list

# Update Julia
juliaup update
```

**Using asdf**:
```bash
# Install asdf Julia plugin
asdf plugin-add julia

# Install Julia version
asdf install julia 1.10.0

# Set local version
asdf local julia 1.10.0
```

---

## Configuration Standards

### 1. Project.toml

**Basic Package Configuration**:

```toml
name = "MyProject"
uuid = "12345678-1234-1234-1234-123456789abc"  # Generate with: using UUIDs; uuid4()
authors = ["Your Name <email@example.com>"]
version = "0.1.0"

[deps]
DataFrames = "a93c6f00-e57d-5684-b7b6-d8193f3e46c0"
LinearAlgebra = "37e2e46d-f89d-539d-b4ee-838fcccc9c8e"
Statistics = "10745b16-79ce-11e8-11f9-7d13ad32a3b2"

[compat]
julia = "1.10"
DataFrames = "1"

[extras]
Test = "8dfed614-e22c-5e08-85e1-65c5234f0b40"

[targets]
test = ["Test"]
```

**Application Configuration**:

```toml
[deps]
HTTP = "cd3eb016-35fb-5094-929b-558a96fad6f3"
JSON3 = "0f8b85d8-7281-11e9-16c2-39a750bddbf1"
Dates = "ade2ca70-3891-5945-98fb-dc099432e06a"

[compat]
julia = "1.10"
HTTP = "1"
JSON3 = "1"
```

### 2. .gitignore

```gitignore
# Julia
*.jl.cov
*.jl.*.cov
*.jl.mem
/Manifest.toml
/docs/build/
/docs/site/

# VS Code
.vscode/

# JetBrains
.idea/

# OS
.DS_Store
Thumbs.db

# Temporary files
*.tmp
*.bak
*~

# Local environment
LocalPreferences.toml
```

### 3. .JuliaFormatter.toml

```toml
style = "blue"
indent = 4
margin = 92
always_for_in = true
whitespace_typedefs = true
whitespace_ops_in_indices = true
remove_extra_newlines = true
import_to_using = false
pipe_to_function_call = false
short_to_long_function_def = false
always_use_return = false
trailing_comma = true
join_lines_based_on_source = true
```

### 4. startup.jl (Development)

```julia
# ~/.julia/config/startup.jl

# Load Revise for automatic code reloading
try
    using Revise
catch e
    @warn "Error initializing Revise" exception=(e, catch_backtrace())
end

# Load OhMyREPL for better REPL experience
try
    using OhMyREPL
catch e
    @warn "OhMyREPL not available"
end

# Set environment variables
ENV["JULIA_NUM_THREADS"] = "auto"
ENV["JULIA_EDITOR"] = "code"  # or "vim", "emacs", etc.

# Custom prompt
using REPL
REPL.LineEdit.activate(REPL.active_repl)
```

---

## Source Code Standards

### 1. File Naming Conventions

```
src/MyProject.jl          # Main module file
src/types.jl              # Type definitions
src/functions.jl          # Function implementations
test/runtests.jl          # Test runner
test/test_module.jl       # Test file
```

### 2. Module Template

**src/MyProject.jl**:

```julia
"""
    MyProject

Brief description of the module's purpose.

# Examples
```julia
using MyProject
result = MyProject.process(data)
```
"""
module MyProject

# Exports
export process, MyType, myfunction

# Imports
using LinearAlgebra
using Statistics
import Base: show, +, *

# Include submodules
include("types.jl")
include("functions.jl")
include("utils.jl")

# Module constants
const VERSION = v"0.1.0"

"""
    process(data)

Process the input data and return the result.

# Arguments
- `data`: Input data to process

# Returns
- Processed result

# Examples
```julia
result = process([1, 2, 3])
```
"""
function process(data)
    # Implementation
    return result
end

end # module
```

### 3. Type Definitions

**src/types.jl**:

```julia
"""
    MyType{T}

A parametric type representing...

# Fields
- `data::Vector{T}`: The data vector
- `name::String`: The name identifier
- `metadata::Dict{Symbol,Any}`: Additional metadata

# Examples
```julia
obj = MyType([1, 2, 3], "example")
```
"""
struct MyType{T<:Real}
    data::Vector{T}
    name::String
    metadata::Dict{Symbol,Any}
    
    # Inner constructor with validation
    function MyType{T}(data::Vector{T}, name::String) where T<:Real
        if isempty(data)
            throw(ArgumentError("data cannot be empty"))
        end
        new{T}(data, name, Dict{Symbol,Any}())
    end
end

# Outer constructor
MyType(data::Vector{T}, name::String) where T<:Real = MyType{T}(data, name)

# Default name
MyType(data::Vector{T}) where T<:Real = MyType{T}(data, "unnamed")

# Show method
function Base.show(io::IO, obj::MyType)
    print(io, "MyType(name=\"$(obj.name)\", n=$(length(obj.data)))")
end

# Custom operators
function Base.:+(a::MyType, b::MyType)
    return MyType(a.data .+ b.data, "$(a.name)+$(b.name)")
end
```

### 4. Function Implementations

**src/functions.jl**:

```julia
"""
    calculate(x::Real, y::Real) -> Real

Calculate the result of x and y.

# Arguments
- `x::Real`: First value
- `y::Real`: Second value

# Returns
- `Real`: The calculated result

# Examples
```julia
result = calculate(2.0, 3.0)
```
"""
function calculate(x::Real, y::Real)
    return x * y + x / y
end

# Multiple dispatch examples
"""
    process(data::Vector{<:Real}) -> Vector{Float64}

Process numeric vector data.
"""
function process(data::Vector{<:Real})
    return Float64.(data) .^ 2
end

"""
    process(data::Matrix{<:Real}) -> Matrix{Float64}

Process numeric matrix data.
"""
function process(data::Matrix{<:Real})
    return Float64.(data) .^ 2
end

"""
    process(data::String) -> String

Process string data.
"""
function process(data::String)
    return uppercase(data)
end
```

### 5. Code Style Guidelines

**Naming Conventions**:
- Functions: `snake_case` or `mixedcase` (e.g., `process_data` or `processData`)
- Types: `PascalCase` (e.g., `MyType`, `DataProcessor`)
- Constants: `UPPER_CASE` or `PascalCase` (e.g., `MAX_SIZE` or `DefaultValue`)
- Modules: `PascalCase` (e.g., `MyModule`)
- Private functions: prefix with `_` (e.g., `_internal_helper`)
- Type parameters: Single uppercase letter (e.g., `T`, `N`, `S`)

**Formatting**:
- Indentation: 4 spaces
- Line length: 92 characters maximum
- Use trailing commas in multi-line arrays/tuples
- Space after commas, around operators

**Documentation**:
- Use docstrings for all exported functions and types
- Include `# Arguments`, `# Returns`, `# Examples` sections
- Use `@doc` for additional documentation

---

## Testing Standards

### 1. Test Structure

**test/runtests.jl**:

```julia
using MyProject
using Test

@testset "MyProject.jl" begin
    include("test_types.jl")
    include("test_functions.jl")
    include("test_utils.jl")
end
```

**test/test_functions.jl**:

```julia
@testset "Functions" begin
    @testset "calculate" begin
        @test calculate(2.0, 3.0) ≈ 6.666666666666666
        @test calculate(0.0, 1.0) ≈ 0.0
        @test_throws DivideError calculate(1.0, 0.0)
    end
    
    @testset "process" begin
        # Test with vector
        @test process([1, 2, 3]) == [1.0, 4.0, 9.0]
        
        # Test with matrix
        @test process([1 2; 3 4]) == [1.0 4.0; 9.0 16.0]
        
        # Test with string
        @test process("hello") == "HELLO"
    end
end
```

**test/test_types.jl**:

```julia
@testset "Types" begin
    @testset "MyType construction" begin
        obj = MyType([1.0, 2.0, 3.0], "test")
        @test obj.name == "test"
        @test length(obj.data) == 3
        
        # Test empty data error
        @test_throws ArgumentError MyType(Float64[], "empty")
    end
    
    @testset "MyType operations" begin
        a = MyType([1.0, 2.0], "a")
        b = MyType([3.0, 4.0], "b")
        c = a + b
        
        @test c.data == [4.0, 6.0]
        @test c.name == "a+b"
    end
    
    @testset "MyType show" begin
        obj = MyType([1.0, 2.0], "test")
        @test repr(obj) == "MyType(name=\"test\", n=2)"
    end
end
```

### 2. Advanced Testing

**Property-Based Testing**:

```julia
using Test
using Random

@testset "Property tests" begin
    @testset "Commutative addition" begin
        for _ in 1:100
            a = rand(1:100)
            b = rand(1:100)
            @test myfunction(a, b) == myfunction(b, a)
        end
    end
end
```

**Approximate Comparisons**:

```julia
@testset "Floating point tests" begin
    @test 0.1 + 0.2 ≈ 0.3  # Using ≈ or isapprox
    @test isapprox(0.1 + 0.2, 0.3, atol=1e-10)
    @test 0.1 + 0.2 ≈ 0.3 atol=1e-10 rtol=1e-5
end
```

**Testing Exceptions**:

```julia
@testset "Exception tests" begin
    @test_throws ArgumentError myfunction(-1)
    @test_throws DomainError sqrt(-1)
    @test_throws "invalid input" myfunction("bad")
end
```

### 3. Coverage and Benchmarking

**Generate Coverage Report**:

```bash
# Run tests with coverage
julia --project=. --code-coverage=user test/runtests.jl

# Generate coverage report
julia -e 'using Pkg; Pkg.add("Coverage"); using Coverage; c = process_folder(); LCOV.writefile("coverage.info", c)'

# View coverage (requires lcov)
genhtml coverage.info --output-directory coverage
```

**Benchmarking**:

```julia
# benchmark/benchmarks.jl
using BenchmarkTools
using MyProject

suite = BenchmarkGroup()

suite["process"] = BenchmarkGroup()
suite["process"]["vector"] = @benchmarkable process($([1.0, 2.0, 3.0]))
suite["process"]["matrix"] = @benchmarkable process($([1.0 2.0; 3.0 4.0]))

# Run benchmarks
results = run(suite, verbose=true)

# Display results
display(results)

# Save results
using JSON
JSON.write("benchmark_results.json", results)
```

---

## Build & Deployment

### 1. CI/CD Configuration

**.github/workflows/CI.yml**:

```yaml
name: CI

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main, develop ]

jobs:
  test:
    runs-on: ${{ matrix.os }}
    strategy:
      matrix:
        os: [ubuntu-latest, macos-latest, windows-latest]
        julia-version: ['1.9', '1.10', 'nightly']
        arch: [x64]
        
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup Julia
        uses: julia-actions/setup-julia@v1
        with:
          version: ${{ matrix.julia-version }}
          arch: ${{ matrix.arch }}
          
      - name: Cache artifacts
        uses: actions/cache@v3
        env:
          cache-name: cache-artifacts
        with:
          path: ~/.julia/artifacts
          key: ${{ runner.os }}-test-${{ env.cache-name }}-${{ hashFiles('**/Project.toml') }}
          restore-keys: |
            ${{ runner.os }}-test-${{ env.cache-name }}-
            ${{ runner.os }}-test-
            ${{ runner.os }}-
            
      - name: Build package
        uses: julia-actions/julia-buildpkg@v1
        
      - name: Run tests
        uses: julia-actions/julia-runtest@v1
        
      - name: Process coverage
        uses: julia-actions/julia-processcoverage@v1
        
      - name: Upload coverage
        uses: codecov/codecov-action@v3
        with:
          files: lcov.info
```

**.github/workflows/Documentation.yml**:

```yaml
name: Documentation

on:
  push:
    branches: [ main ]
    tags: '*'
  pull_request:

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup Julia
        uses: julia-actions/setup-julia@v1
        with:
          version: '1.10'
          
      - name: Install dependencies
        run: julia --project=docs -e 'using Pkg; Pkg.develop(PackageSpec(path=pwd())); Pkg.instantiate()'
        
      - name: Build documentation
        run: julia --project=docs docs/make.jl
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
          DOCUMENTER_KEY: ${{ secrets.DOCUMENTER_KEY }}
```

**.github/workflows/TagBot.yml**:

```yaml
name: TagBot

on:
  issue_comment:
    types: [created, edited]
  workflow_dispatch:

jobs:
  TagBot:
    if: github.event_name == 'workflow_dispatch' || github.actor == 'JuliaTagBot'
    runs-on: ubuntu-latest
    steps:
      - uses: JuliaRegistries/TagBot@v1
        with:
          token: ${{ secrets.GITHUB_TOKEN }}
          ssh: ${{ secrets.DOCUMENTER_KEY }}
```

### 2. Package Registration

**Register Package to General Registry**:

```julia
# First time setup
using Pkg
Pkg.add("LocalRegistry")

# Create a new version
# 1. Update version in Project.toml
# 2. Update CHANGELOG.md
# 3. Commit changes
# 4. Tag the release

# Register to General registry
using PkgDev
PkgDev.register("MyProject")

# Or via Registrator.jl
# Comment on GitHub: @JuliaRegistrator register

# Or manually via PR to General registry
```

### 3. Building Artifacts

**Binary Dependencies**:

```julia
# deps/build.jl
using BinaryBuilder

# Define build script for C library
name = "MyLibrary"
version = v"1.0.0"

sources = [
    ArchiveSource("https://github.com/user/library/archive/v1.0.0.tar.gz", 
                  "sha256hash")
]

script = raw"""
cd $WORKSPACE/srcdir/library-1.0.0
./configure --prefix=${prefix} --build=${MACHTYPE} --host=${target}
make -j${nproc}
make install
"""

platforms = supported_platforms()

products = [
    LibraryProduct("libmylibrary", :libmylibrary)
]

dependencies = Dependency[]

build_tarballs(ARGS, name, version, sources, script, platforms, products, dependencies)
```

---

## Documentation

### 1. Documenter.jl Setup

**docs/make.jl**:

```julia
using Documenter
using MyProject

makedocs(
    sitename = "MyProject.jl",
    authors = "Your Name",
    format = Documenter.HTML(
        prettyurls = get(ENV, "CI", nothing) == "true",
        canonical = "https://yourname.github.io/MyProject.jl",
        assets = String[],
    ),
    pages = [
        "Home" => "index.md",
        "Manual" => [
            "Getting Started" => "manual/getting_started.md",
            "Examples" => "manual/examples.md",
        ],
        "API Reference" => "api.md",
    ],
    modules = [MyProject],
    checkdocs = :exports,
)

deploydocs(
    repo = "github.com/yourname/MyProject.jl.git",
    devbranch = "main",
)
```

**docs/src/index.md**:

````markdown
# MyProject.jl

Documentation for MyProject.jl

## Installation

```julia
using Pkg
Pkg.add("MyProject")
```

## Quick Start

```julia
using MyProject

# Example usage
result = process([1, 2, 3])
```

## Features

- Feature 1
- Feature 2
- Feature 3
````

**docs/src/api.md**:

````markdown
# API Reference

```@index
```

## Types

```@docs
MyType
```

## Functions

```@docs
process
calculate
```
````

### 2. Generate Documentation

```bash
# Install Documenter
julia --project=docs -e 'using Pkg; Pkg.add("Documenter")'

# Build documentation
julia --project=docs docs/make.jl

# View locally
cd docs/build
python -m http.server 8000
# Open http://localhost:8000
```

---

## Julia Best Practices

### 1. Type Stability

```julia
# ❌ BAD: Type-unstable function
function bad_function(x)
    if x > 0
        return x          # Returns Int
    else
        return "negative" # Returns String
    end
end

# ✅ GOOD: Type-stable function
function good_function(x)
    if x > 0
        return x
    else
        return zero(x)    # Same type
    end
end

# Check type stability
using Test
@inferred good_function(5)
```

### 2. Use Multiple Dispatch

```julia
# ✅ GOOD: Multiple dispatch for different types
distance(a::Real, b::Real) = abs(a - b)
distance(a::Complex, b::Complex) = abs(a - b)
distance(a::Vector, b::Vector) = norm(a - b)
distance(a::String, b::String) = levenshtein(a, b)
```

### 3. Avoid Global Variables

```julia
# ❌ BAD: Global variable
GLOBAL_COUNTER = 0

function increment()
    global GLOBAL_COUNTER
    GLOBAL_COUNTER += 1
end

# ✅ GOOD: Pass state explicitly
mutable struct Counter
    value::Int
end

function increment!(counter::Counter)
    counter.value += 1
end

# ✅ BETTER: Use const for constants
const DEFAULT_TOLERANCE = 1e-6
```

### 4. Preallocate Arrays

```julia
# ❌ SLOW: Growing array
function slow_sum(n)
    result = []
    for i in 1:n
        push!(result, i^2)
    end
    return sum(result)
end

# ✅ FAST: Preallocated array
function fast_sum(n)
    result = Vector{Int}(undef, n)
    for i in 1:n
        result[i] = i^2
    end
    return sum(result)
end

# ✅ BETTER: Use comprehension or generator
fast_sum_v2(n) = sum(i^2 for i in 1:n)
```

### 5. Use Broadcasting

```julia
# ❌ SLOW: Element-wise loop
function element_wise(a, b)
    result = similar(a)
    for i in eachindex(a)
        result[i] = a[i] + b[i]
    end
    return result
end

# ✅ FAST: Broadcasting
element_wise_fast(a, b) = a .+ b

# More complex broadcasting
f(x) = x^2 + 2x + 1
result = f.(array)  # Apply to each element
```

---

## Common Patterns

### 1. Iterator Protocol

```julia
struct MyRange
    start::Int
    stop::Int
    step::Int
end

Base.iterate(r::MyRange) = (r.start, r.start)

function Base.iterate(r::MyRange, state)
    next = state + r.step
    next > r.stop ? nothing : (next, next)
end

Base.length(r::MyRange) = (r.stop - r.start) ÷ r.step + 1

# Usage
for x in MyRange(1, 10, 2)
    println(x)  # 1, 3, 5, 7, 9
end
```

### 2. Functor Pattern

```julia
struct Polynomial
    coeffs::Vector{Float64}
end

# Make Polynomial callable
function (p::Polynomial)(x)
    result = 0.0
    for (i, c) in enumerate(p.coeffs)
        result += c * x^(i-1)
    end
    return result
end

# Usage
p = Polynomial([1.0, 2.0, 3.0])  # 1 + 2x + 3x²
p(2.0)  # Evaluates at x=2
```

### 3. Do-Block Syntax

```julia
# File operations with automatic cleanup
function process_file(filename, f)
    open(filename, "r") do file
        f(file)
    end  # Automatically closes file
end

# Usage
process_file("data.txt") do file
    for line in eachline(file)
        println(line)
    end
end
```

---

## Troubleshooting

### Common Issues

**Problem**: Package precompilation fails
```julia
# Solution: Clear compiled cache
using Pkg
Pkg.build()
Pkg.resolve()

# Or manually delete
rm(joinpath(first(DEPOT_PATH), "compiled"), recursive=true, force=true)
```

**Problem**: Method ambiguity
```julia
# Caused by overlapping method signatures
# Add more specific method or use abstract types carefully
```

**Problem**: StackOverflowError
```julia
# Usually from infinite recursion
# Check base cases in recursive functions
```

---

## Complete Workflow

### Example: Creating Julia Package

```bash
# 1. Generate package
julia -e 'using Pkg; Pkg.generate("MyPackage")'
cd MyPackage

# 2. Initialize Git
git init
git add .
git commit -m "Initial commit"

# 3. Activate and add dependencies
julia --project=. -e 'using Pkg; Pkg.add(["DataFrames", "CSV"])'

# 4. Run tests
julia --project=. -e 'using Pkg; Pkg.test()'

# 5. Setup documentation
mkdir -p docs/src
# Create docs/make.jl and docs/src/index.md

# 6. Add CI workflows
mkdir -p .github/workflows
# Create CI.yml, Documentation.yml, TagBot.yml

# 7. Create release
git tag v0.1.0
git push origin v0.1.0

# 8. Register package (comment on GitHub)
# @JuliaRegistrator register
```

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-10-11 | Initial Julia manual creation |

---

## References

- [Julia Documentation](https://docs.julialang.org/)
- [Julia Style Guide](https://docs.julialang.org/en/v1/manual/style-guide/)
- [Performance Tips](https://docs.julialang.org/en/v1/manual/performance-tips/)
- [Pkg.jl Documentation](https://pkgdocs.julialang.org/)
- [Documenter.jl](https://documenter.juliadocs.org/)
- [JuliaHub](https://juliahub.com/)

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

