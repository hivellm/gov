# Julia Best Practices

**Version**: 1.0.0  
**Last Updated**: 2025-10-11  
**Target Audience**: AI Agents developing Julia projects

---

## Table of Contents

1. [Julia Idioms](#julia-idioms)
2. [Anti-Patterns](#anti-patterns)
3. [Performance Optimization](#performance-optimization)
4. [Type System Best Practices](#type-system-best-practices)
5. [Multiple Dispatch](#multiple-dispatch)
6. [Memory Management](#memory-management)
7. [Concurrency and Parallelism](#concurrency-and-parallelism)
8. [Code Organization](#code-organization)
9. [Testing Best Practices](#testing-best-practices)
10. [Common Gotchas](#common-gotchas)

---

## Julia Idioms

### 1. Use Type-Stable Functions

```julia
# ❌ BAD: Type-unstable (returns different types)
function bad_abs(x)
    if x >= 0
        return x           # Int/Float
    else
        return -x          # Could be different
    end
end

# ✅ GOOD: Type-stable
function good_abs(x::T) where T<:Real
    return x >= zero(T) ? x : -x  # Always returns T
end

# Verify type stability
using Test
@inferred good_abs(5)
@inferred good_abs(5.0)
```

### 2. Embrace Multiple Dispatch

```julia
# ✅ GOOD: Multiple dispatch for different types
norm(x::Number) = abs(x)
norm(x::AbstractVector) = sqrt(sum(abs2, x))
norm(x::AbstractMatrix) = maximum(svdvals(x))
norm(x::AbstractString) = length(x)

# Usage is clean
norm(5)              # Uses Number method
norm([1, 2, 3])      # Uses Vector method
norm([1 2; 3 4])     # Uses Matrix method
norm("hello")        # Uses String method
```

### 3. Use Broadcasting Effectively

```julia
# ✅ GOOD: Broadcasting for element-wise operations
a = [1, 2, 3]
b = [4, 5, 6]

# Element-wise operations
c = a .+ b           # [5, 7, 9]
c = a .* b           # [4, 10, 18]
c = sqrt.(a)         # [1.0, 1.414..., 1.732...]

# Apply function to each element
f(x) = x^2 + 2x + 1
result = f.(a)       # Apply f to each element

# Broadcasting with scalars
c = a .+ 10          # [11, 12, 13]

# Fused broadcasting (single loop)
c = @. a^2 + 2a + 1  # All operations fused
```

### 4. Preallocate and Reuse Arrays

```julia
# ❌ SLOW: Creating new arrays
function slow_accumulate(n)
    result = Int[]
    for i in 1:n
        push!(result, i^2)
    end
    return result
end

# ✅ FAST: Preallocated array
function fast_accumulate(n)
    result = Vector{Int}(undef, n)
    for i in 1:n
        result[i] = i^2
    end
    return result
end

# ✅ BETTER: Use comprehension
fast_comprehension(n) = [i^2 for i in 1:n]

# ✅ BEST: Use generator for memory efficiency
fast_generator(n) = (i^2 for i in 1:n)
```

### 5. Use Views Instead of Copies

```julia
# ❌ SLOW: Creates copies
function slow_process(arr)
    for i in 1:10
        subarr = arr[1:100]  # Copy!
        sum(subarr)
    end
end

# ✅ FAST: Uses views (no copy)
function fast_process(arr)
    for i in 1:10
        subarr = @view arr[1:100]  # View!
        sum(subarr)
    end
end

# Or use @views macro for entire function
function faster_process(arr)
    @views for i in 1:10
        subarr = arr[1:100]  # Automatic view
        sum(subarr)
    end
end
```

---

## Anti-Patterns

### 1. Global Variables

```julia
# ❌ BAD: Mutable global variable (very slow!)
counter = 0

function increment()
    global counter
    counter += 1
end

# ✅ GOOD: Use const for constants
const MAX_ITERATIONS = 1000

# ✅ GOOD: Pass state explicitly
mutable struct Counter
    value::Int
end

function increment!(c::Counter)
    c.value += 1
end

# ✅ BETTER: Use let blocks for local state
let counter = 0
    global increment() = (counter += 1; counter)
    global reset() = (counter = 0)
end
```

### 2. Abstract Field Types

```julia
# ❌ BAD: Abstract field types (performance killer!)
struct BadContainer
    data::AbstractVector  # Don't do this!
    value::Real          # Or this!
end

# ✅ GOOD: Concrete types or parametric types
struct GoodContainer{T}
    data::Vector{T}
    value::T
end

# Or use parametric with constraints
struct BetterContainer{T<:Real, V<:AbstractVector{T}}
    data::V
    value::T
end
```

### 3. Type Piracy

```julia
# ❌ BAD: Extending functions you don't own with types you don't own
import Base: +
+(x::String, y::String) = string(x, y)  # Don't do this!

# ✅ GOOD: Create your own wrapper type
struct MyString
    value::String
end

Base.:+(x::MyString, y::MyString) = MyString(string(x.value, y.value))

# ✅ ALTERNATIVE: Define your own function
myconcat(x::String, y::String) = string(x, y)
```

### 4. Unnecessary Allocations

```julia
# ❌ SLOW: Many allocations
function slow_sum(A, B)
    return sum(A + B)  # Allocates A + B
end

# ✅ FAST: No intermediate allocation
function fast_sum(A, B)
    return sum(A[i] + B[i] for i in eachindex(A, B))
end

# ✅ BETTER: Use in-place operations
function faster_sum!(C, A, B)
    C .= A .+ B
    return sum(C)
end
```

### 5. Changing Container Sizes in Loops

```julia
# ❌ SLOW: Growing array in loop
function slow_filter(arr, threshold)
    result = []
    for x in arr
        if x > threshold
            push!(result, x)
        end
    end
    return result
end

# ✅ FAST: Preallocate or use filter
function fast_filter(arr, threshold)
    return filter(x -> x > threshold, arr)
end

# ✅ ALTERNATIVE: Preallocate with resize
function preallocated_filter(arr, threshold)
    result = Vector{eltype(arr)}(undef, length(arr))
    idx = 0
    for x in arr
        if x > threshold
            idx += 1
            result[idx] = x
        end
    end
    resize!(result, idx)
    return result
end
```

---

## Performance Optimization

### 1. Use @inbounds for Checked Bounds

```julia
# ❌ SLOW: Bounds checking every iteration
function slow_sum(arr)
    total = zero(eltype(arr))
    for i in 1:length(arr)
        total += arr[i]  # Bounds checked
    end
    return total
end

# ✅ FAST: Skip bounds checking (use carefully!)
function fast_sum(arr)
    total = zero(eltype(arr))
    @inbounds for i in 1:length(arr)
        total += arr[i]  # No bounds check
    end
    return total
end

# ⚠️ WARNING: Only use @inbounds when you're certain indices are valid
```

### 2. Use @simd for Vectorization

```julia
# ✅ GOOD: SIMD optimization
function simd_sum(arr)
    total = zero(eltype(arr))
    @simd for i in eachindex(arr)
        total += arr[i]
    end
    return total
end

# Combine with @inbounds
function fast_simd_sum(arr)
    total = zero(eltype(arr))
    @inbounds @simd for i in eachindex(arr)
        total += arr[i]
    end
    return total
end
```

### 3. Avoid Capturing Variables

```julia
# ❌ SLOW: Captures variable in closure
function slow_make_adder(x)
    return y -> x + y  # Captures x
end

# ✅ FAST: No capture needed (function barrier)
struct Adder{T}
    x::T
end
(a::Adder)(y) = a.x + y

fast_make_adder(x) = Adder(x)
```

### 4. Use Function Barriers

```julia
# ❌ SLOW: Type-unstable causing slow code throughout
function slow_process(x)
    if x > 0
        data = [1, 2, 3]      # Vector{Int}
    else
        data = [1.0, 2.0, 3.0]  # Vector{Float64}
    end
    
    # Lots of processing here...
    result = sum(data)
    for i in 1:100
        result += data[1] * i
    end
    return result
end

# ✅ FAST: Use function barrier
function _process_data(data)  # Type-stable
    result = sum(data)
    for i in 1:100
        result += data[1] * i
    end
    return result
end

function fast_process(x)
    if x > 0
        data = [1, 2, 3]
    else
        data = [1.0, 2.0, 3.0]
    end
    return _process_data(data)  # Barrier
end
```

### 5. Profile Before Optimizing

```julia
using Profile
using ProfileView

function my_function(n)
    # Your code here
    result = 0
    for i in 1:n
        result += expensive_operation(i)
    end
    return result
end

# Profile the function
@profile my_function(1000)

# View profile
ProfileView.view()

# Or text-based profiling
Profile.print()

# Benchmark specific parts
using BenchmarkTools
@benchmark expensive_operation(100)
```

---

## Type System Best Practices

### 1. Prefer Abstract Types for Function Arguments

```julia
# ❌ TOO RESTRICTIVE: Only accepts Vector{Int}
function bad_sum(arr::Vector{Int})
    return sum(arr)
end

# ✅ GOOD: Accepts any vector of reals
function good_sum(arr::AbstractVector{<:Real})
    return sum(arr)
end

# ✅ BETTER: Generic with type constraints
function better_sum(arr::AbstractVector{T}) where T<:Real
    return sum(arr)
end
```

### 2. Use Parametric Types for Structs

```julia
# ❌ BAD: Non-parametric (inflexible and slow)
struct BadPoint
    x::Float64
    y::Float64
end

# ✅ GOOD: Parametric type
struct GoodPoint{T<:Real}
    x::T
    y::T
end

# Now works with any Real type efficiently
p1 = GoodPoint(1, 2)           # GoodPoint{Int}
p2 = GoodPoint(1.0, 2.0)       # GoodPoint{Float64}
p3 = GoodPoint(1//2, 3//4)     # GoodPoint{Rational}
```

### 3. Union Types for Simple Alternatives

```julia
# ✅ GOOD: Union for simple alternatives
function process(x::Union{Int, Float64})
    return x * 2
end

# Julia can optimize small unions well
Maybe{T} = Union{T, Nothing}

function find_first(arr, pred)::Maybe{eltype(arr)}
    for x in arr
        pred(x) && return x
    end
    return nothing
end
```

### 4. Type Stability Checkers

```julia
using Test

# Check if function is type-stable
@inferred sin(1.0)          # OK
@inferred [1, 2.0]          # Error: type-unstable

# Use @code_warntype to inspect
function maybe_type_unstable(x)
    if x > 0
        return x
    else
        return 0.0
    end
end

@code_warntype maybe_type_unstable(1)  # Check for red warnings
```

### 5. Traits Pattern

```julia
# Define trait hierarchy
abstract type NumberTrait end
struct IsInteger <: NumberTrait end
struct IsFloatingPoint <: NumberTrait end

# Trait function
number_trait(::Type{<:Integer}) = IsInteger()
number_trait(::Type{<:AbstractFloat}) = IsFloatingPoint()

# Trait-based dispatch
process(x::T) where T<:Real = process(number_trait(T), x)
process(::IsInteger, x) = x * 2
process(::IsFloatingPoint, x) = x * 2.0

# Usage
process(5)      # Calls IsInteger method
process(5.0)    # Calls IsFloatingPoint method
```

---

## Multiple Dispatch

### 1. Leverage Type Hierarchy

```julia
# Define type hierarchy
abstract type Animal end
abstract type Pet <: Animal end
abstract type Wild <: Animal end

struct Dog <: Pet
    name::String
end

struct Cat <: Pet
    name::String
end

struct Wolf <: Wild
    pack_size::Int
end

# Methods dispatch on hierarchy
speak(a::Pet) = "I'm a pet"
speak(d::Dog) = "Woof!"
speak(c::Cat) = "Meow!"
speak(w::Wild) = "..."

# More specific methods override general ones
pet_or_wild(::Pet) = "pet"
pet_or_wild(::Wild) = "wild"
```

### 2. Multi-Argument Dispatch

```julia
# Different behavior based on both argument types
combine(x::Int, y::Int) = x + y
combine(x::String, y::String) = string(x, y)
combine(x::Int, y::String) = string(x, y)
combine(x::String, y::Int) = string(x, y)

# Using abstract types
distance(a::Number, b::Number) = abs(a - b)
distance(a::AbstractVector, b::AbstractVector) = norm(a - b)
distance(a::AbstractString, b::AbstractString) = levenshtein(a, b)
```

### 3. Avoid Method Ambiguities

```julia
# ❌ CREATES AMBIGUITY
f(x::Int, y::Any) = 1
f(x::Any, y::Int) = 2
# f(1, 1) is ambiguous!

# ✅ RESOLVE: Add specific method
f(x::Int, y::Int) = 3  # Most specific wins

# Or redesign to avoid ambiguity
better_f(x::T, y::S) where {T<:Integer, S} = 1
better_f(x::S, y::T) where {T<:Integer, S} = 2
better_f(x::T, y::T) where T<:Integer = 3
```

### 4. Use Generated Functions Sparingly

```julia
# For when you need to generate specialized code
@generated function unroll_sum(x::NTuple{N, T}) where {N, T}
    ex = :(x[1])
    for i in 2:N
        ex = :($ex + x[$i])
    end
    return ex
end

# Generates optimized code for each tuple size
unroll_sum((1, 2, 3))      # sum(x[1], x[2], x[3])
unroll_sum((1, 2, 3, 4))   # sum(x[1], x[2], x[3], x[4])
```

---

## Memory Management

### 1. Understanding Memory Layout

```julia
# Arrays are column-major
A = rand(1000, 1000)

# ❌ SLOW: Row-major access
function slow_sum(A)
    total = 0.0
    for i in 1:size(A, 1)
        for j in 1:size(A, 2)
            total += A[i, j]  # Cache-unfriendly
        end
    end
    return total
end

# ✅ FAST: Column-major access
function fast_sum(A)
    total = 0.0
    for j in 1:size(A, 2)
        for i in 1:size(A, 1)
            total += A[i, j]  # Cache-friendly
        end
    end
    return total
end

# ✅ BEST: Use built-ins
fastest_sum(A) = sum(A)
```

### 2. Manage Allocations

```julia
# Check allocations
using BenchmarkTools

# ❌ MANY ALLOCATIONS
function allocating_version(n)
    result = 0.0
    for i in 1:n
        temp = rand(10)  # Allocates!
        result += sum(temp)
    end
    return result
end

# ✅ ZERO ALLOCATIONS
function non_allocating_version(n)
    result = 0.0
    temp = Vector{Float64}(undef, 10)
    for i in 1:n
        rand!(temp)  # Reuses temp
        result += sum(temp)
    end
    return result
end

@btime allocating_version(1000)      # Shows allocations
@btime non_allocating_version(1000)  # Minimal allocations
```

### 3. Garbage Collection

```julia
# Manual GC control for benchmarking
GC.gc()  # Force garbage collection

# Disable GC temporarily
GC.enable(false)
# ... critical section ...
GC.enable(true)

# Check GC stats
GC.gc()
@time GC.gc()  # See GC time
```

---

## Concurrency and Parallelism

### 1. Multi-Threading

```julia
# Start Julia with multiple threads
# julia -t 4  or  julia -t auto

# Check thread count
Threads.nthreads()

# Parallel loop
using Base.Threads

function parallel_sum(arr)
    sums = zeros(Threads.nthreads())
    @threads for i in eachindex(arr)
        tid = Threads.threadid()
        sums[tid] += arr[i]
    end
    return sum(sums)
end

# Thread-safe operations
using Base.Threads
const counter = Atomic{Int}(0)

@threads for i in 1:1000
    atomic_add!(counter, 1)
end
```

### 2. Distributed Computing

```julia
using Distributed

# Add worker processes
addprocs(4)

# Load code on all processes
@everywhere function expensive_computation(x)
    return sum(sin(i) for i in 1:x)
end

# Parallel map
results = pmap(expensive_computation, 1:100)

# Distributed for loop
@distributed (+) for i in 1:1000
    expensive_computation(i)
end
```

### 3. Async/Tasks

```julia
# Asynchronous I/O
function async_download(urls)
    tasks = [Threads.@spawn download(url) for url in urls]
    return [fetch(t) for t in tasks]
end

# Channels for producer-consumer
function producer(ch::Channel)
    for i in 1:10
        put!(ch, i)
        sleep(0.1)
    end
end

function consumer(ch::Channel)
    for val in ch
        println("Received: ", val)
    end
end

ch = Channel(producer, ctype=Int, csize=5)
consumer(ch)
```

---

## Code Organization

### 1. Module Structure

```julia
module MyModule

# Exports
export public_function, PublicType

# Imports
using LinearAlgebra
import Statistics: mean, median

# Constants
const DEFAULT_TOLERANCE = 1e-6

# Type definitions
struct PublicType{T}
    data::Vector{T}
end

# Public functions
function public_function(x)
    return _internal_helper(x) * 2
end

# Private functions (not exported)
function _internal_helper(x)
    return x + 1
end

end # module
```

### 2. Include Files

```julia
# src/MyPackage.jl
module MyPackage

include("types.jl")
include("functions.jl")
include("algorithms.jl")

# Re-export from submodules
using .Types: MyType
using .Functions: myfunction

export MyType, myfunction

end
```

### 3. Precompilation

```julia
# Add to module for faster loading
module MyModule

# ... module code ...

# Precompile frequently used functions
precompile(myfunction, (Int,))
precompile(myfunction, (Float64,))
precompile(MyType, (Vector{Int},))

end
```

---

## Testing Best Practices

### 1. Comprehensive Test Coverage

```julia
using Test

@testset "MyModule Tests" begin
    @testset "Basic functionality" begin
        @test myfunction(1) == 2
        @test myfunction(2) == 4
    end
    
    @testset "Edge cases" begin
        @test myfunction(0) == 0
        @test myfunction(-1) == -2
    end
    
    @testset "Type stability" begin
        @inferred myfunction(1)
        @inferred myfunction(1.0)
    end
    
    @testset "Error handling" begin
        @test_throws ArgumentError myfunction(nothing)
        @test_throws DomainError myfunction(-Inf)
    end
end
```

### 2. Property-Based Testing

```julia
using Test
using Random

@testset "Property tests" begin
    # Commutativity
    for _ in 1:100
        a, b = rand(1:100, 2)
        @test myfunction(a, b) == myfunction(b, a)
    end
    
    # Associativity
    for _ in 1:100
        a, b, c = rand(1:100, 3)
        @test myfunction(myfunction(a, b), c) == 
              myfunction(a, myfunction(b, c))
    end
end
```

### 3. Approximate Testing

```julia
@testset "Floating point tests" begin
    # Using ≈ (approximately equal)
    @test 0.1 + 0.2 ≈ 0.3
    
    # With tolerance
    @test isapprox(0.1 + 0.2, 0.3, atol=1e-10)
    
    # Array comparison
    @test [0.1, 0.2] .+ [0.2, 0.3] ≈ [0.3, 0.5]
end
```

---

## Common Gotchas

### 1. Array Indexing Starts at 1

```julia
arr = [10, 20, 30]

# ❌ ERROR
arr[0]  # BoundsError!

# ✅ CORRECT
arr[1]  # First element = 10
arr[end]  # Last element = 30
arr[end-1]  # Second to last = 20
```

### 2. Division Behavior

```julia
# Integer division
5 / 2    # 2.5 (Float64)
5 ÷ 2    # 2 (Int) - integer division
5 // 2   # 5//2 (Rational)

# Be careful with types
x = 1 / 2  # x is Float64
y = 1 // 2  # y is Rational{Int}
```

### 3. Scope in Loops

```julia
# ❌ ERROR: x not defined
for i in 1:10
    x = i^2
end
println(x)  # ERROR: UndefVarError

# ✅ CORRECT: Define outside loop
x = 0
for i in 1:10
    x = i^2
end
println(x)  # Works

# Or use let
let x = 0
    for i in 1:10
        x = i^2
    end
    println(x)
end
```

### 4. Copy vs. Reference

```julia
# Arrays are passed by reference
a = [1, 2, 3]
b = a  # Reference, not copy!
b[1] = 100
println(a)  # [100, 2, 3] - a changed too!

# Make a copy
c = copy(a)  # Shallow copy
d = deepcopy(a)  # Deep copy (for nested structures)
```

### 5. Broadcasting Gotcha

```julia
# ❌ ERROR: Dimension mismatch
a = [1, 2, 3]
b = [4, 5]
c = a + b  # ERROR!

# ✅ CORRECT: Must be same size
a = [1, 2, 3]
b = [4, 5, 6]
c = a + b  # [5, 7, 9]

# Or broadcast with scalar
c = a .+ 10  # [11, 12, 13]
```

---

## Additional Resources

- [Julia Performance Tips](https://docs.julialang.org/en/v1/manual/performance-tips/)
- [Julia Style Guide](https://docs.julialang.org/en/v1/manual/style-guide/)
- [JuliaLang Discourse](https://discourse.julialang.org/)
- [Julia Package Documentation](https://pkgdocs.julialang.org/)
- [Fast Track to Julia](https://juliadocs.github.io/Julia-Cheat-Sheet/)

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

