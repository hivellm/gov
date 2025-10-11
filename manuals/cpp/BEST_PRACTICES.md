# C++ Best Practices

**Version**: 1.0.0  
**Last Updated**: 2025-10-11  
**Target Audience**: AI Agents developing C++ projects  
**C++ Standard**: C++17/20/23

---

## Table of Contents

1. [Modern C++ Idioms](#modern-cpp-idioms)
2. [Anti-Patterns](#anti-patterns)
3. [Performance Optimization](#performance-optimization)
4. [Security Best Practices](#security-best-practices)
5. [Error Handling](#error-handling)
6. [Memory Management](#memory-management)
7. [Type Safety](#type-safety)
8. [Code Organization](#code-organization)
9. [Testing Best Practices](#testing-best-practices)
10. [Common Gotchas](#common-gotchas)

---

## Modern C++ Idioms

### 1. Use Smart Pointers, Not Raw Pointers

```cpp
// ❌ BAD: Manual memory management
Widget* widget = new Widget();
// ... use widget ...
delete widget;  // Easy to forget or miss in error paths

// ✅ GOOD: Automatic memory management
auto widget = std::make_unique<Widget>();
// ... use widget ...
// Automatically deleted when out of scope

// ✅ GOOD: Shared ownership
auto shared_widget = std::make_shared<Widget>();
```

### 2. Use RAII for All Resources

```cpp
// ❌ BAD: Manual resource management
FILE* file = fopen("data.txt", "r");
if (!file) return error;
// ... use file ...
fclose(file);  // Easy to miss on error path

// ✅ GOOD: RAII wrapper
class FileHandle {
    FILE* file_;
public:
    explicit FileHandle(const char* filename) 
        : file_(fopen(filename, "r")) {
        if (!file_) throw std::runtime_error("Failed to open file");
    }
    ~FileHandle() { if (file_) fclose(file_); }
    
    FILE* get() const { return file_; }
};

FileHandle file("data.txt");
// Automatically closed when out of scope
```

### 3. Prefer `auto` for Type Deduction

```cpp
// ❌ BAD: Verbose and error-prone
std::vector<std::string>::iterator it = vec.begin();
std::map<std::string, std::vector<int>>::const_iterator map_it = map.find(key);

// ✅ GOOD: Clear and concise
auto it = vec.begin();
auto map_it = map.find(key);

// ✅ GOOD: For complex types
auto widget = std::make_unique<Widget>();
auto result = calculate_complex_value();
```

### 4. Use Range-Based For Loops

```cpp
// ❌ BAD: Index-based loop
for (size_t i = 0; i < vec.size(); ++i) {
    process(vec[i]);
}

// ✅ GOOD: Range-based loop
for (const auto& item : vec) {
    process(item);
}

// ✅ GOOD: Modify elements
for (auto& item : vec) {
    item.update();
}

// ✅ GOOD: C++20 ranges
for (auto value : vec | std::views::filter(is_valid) 
                      | std::views::transform(process)) {
    use(value);
}
```

### 5. Use Structured Bindings (C++17)

```cpp
// ❌ BAD: Manual extraction
std::pair<int, std::string> get_data();
auto result = get_data();
int id = result.first;
std::string name = result.second;

// ✅ GOOD: Structured binding
auto [id, name] = get_data();

// ✅ GOOD: With maps
std::map<std::string, int> scores;
for (const auto& [key, value] : scores) {
    std::cout << key << ": " << value << '\n';
}
```

### 6. Use `std::optional` for Optional Values

```cpp
// ❌ BAD: Using nullptr or special values
Widget* find_widget(int id) {
    // ...
    return nullptr;  // Ambiguous: not found or error?
}

// ✅ GOOD: Clear intent
std::optional<Widget> find_widget(int id) {
    // ...
    return std::nullopt;  // Clearly indicates absence
}

// Usage
if (auto widget = find_widget(42)) {
    use(*widget);
} else {
    handle_not_found();
}
```

### 7. Use `std::variant` for Type-Safe Unions

```cpp
// ❌ BAD: C-style union
union Value {
    int i;
    double d;
    char* str;
};

// ✅ GOOD: Type-safe variant
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

### 8. Use `constexpr` for Compile-Time Computation

```cpp
// ❌ BAD: Runtime computation
const int factorial(int n) {
    return (n <= 1) ? 1 : n * factorial(n - 1);
}

// ✅ GOOD: Compile-time when possible
constexpr int factorial(int n) {
    return (n <= 1) ? 1 : n * factorial(n - 1);
}

// Computed at compile time
constexpr int result = factorial(5);  // No runtime cost
```

### 9. Use `[[nodiscard]]` for Important Return Values

```cpp
// ✅ GOOD: Warn if return value is ignored
[[nodiscard]] bool save_data(const Data& data);
[[nodiscard]] std::optional<Config> load_config();
[[nodiscard]] int calculate_critical_value();

// Compiler warns if you do this:
save_data(data);  // Warning: ignoring return value
```

### 10. Use Concepts (C++20) for Generic Programming

```cpp
// ❌ BAD: SFINAE is hard to read
template<typename T, 
         typename = std::enable_if_t<std::is_integral_v<T>>>
T square(T value) {
    return value * value;
}

// ✅ GOOD: Concepts are clear
template<std::integral T>
T square(T value) {
    return value * value;
}

// ✅ GOOD: Custom concepts
template<typename T>
concept Numeric = std::integral<T> || std::floating_point<T>;

template<Numeric T>
T add(T a, T b) {
    return a + b;
}
```

---

## Anti-Patterns

### 1. Using Raw `new` and `delete`

```cpp
// ❌ BAD: Manual memory management
Widget* widget = new Widget();
delete widget;

Widget* array = new Widget[10];
delete[] array;

// ✅ GOOD: Smart pointers
auto widget = std::make_unique<Widget>();
auto array = std::make_unique<Widget[]>(10);

// ✅ GOOD: Containers
std::vector<Widget> widgets(10);
```

### 2. Not Following Rule of Five/Zero

```cpp
// ❌ BAD: Inconsistent special members
class BadClass {
    int* data_;
public:
    BadClass() : data_(new int(0)) {}
    ~BadClass() { delete data_; }
    // Missing copy/move operations - double delete!
};

// ✅ GOOD: Rule of Zero (prefer this)
class GoodClass {
    std::unique_ptr<int> data_;
public:
    GoodClass() : data_(std::make_unique<int>(0)) {}
    // Compiler generates correct copy/move operations
};

// ✅ GOOD: Rule of Five (when needed)
class ExplicitClass {
    int* data_;
public:
    ExplicitClass() : data_(new int(0)) {}
    ~ExplicitClass() { delete data_; }
    
    // Copy constructor
    ExplicitClass(const ExplicitClass& other) 
        : data_(new int(*other.data_)) {}
    
    // Copy assignment
    ExplicitClass& operator=(const ExplicitClass& other) {
        if (this != &other) {
            *data_ = *other.data_;
        }
        return *this;
    }
    
    // Move constructor
    ExplicitClass(ExplicitClass&& other) noexcept 
        : data_(std::exchange(other.data_, nullptr)) {}
    
    // Move assignment
    ExplicitClass& operator=(ExplicitClass&& other) noexcept {
        if (this != &other) {
            delete data_;
            data_ = std::exchange(other.data_, nullptr);
        }
        return *this;
    }
};
```

### 3. Using C-Style Casts

```cpp
// ❌ BAD: C-style casts
double value = 3.14;
int truncated = (int)value;
Base* base = (Base*)derived;

// ✅ GOOD: C++ style casts
double value = 3.14;
int truncated = static_cast<int>(value);
Base* base = dynamic_cast<Base*>(derived);  // Runtime check
const char* str = reinterpret_cast<const char*>(buffer);
```

### 4. Ignoring `const` Correctness

```cpp
// ❌ BAD: Non-const methods that don't modify
class Widget {
    int value_;
public:
    int get_value() { return value_; }  // Should be const
    void print() { std::cout << value_; }  // Should be const
};

// ✅ GOOD: Proper const correctness
class Widget {
    int value_;
public:
    [[nodiscard]] int get_value() const { return value_; }
    void print() const { std::cout << value_; }
    
    void set_value(int value) { value_ = value; }  // Non-const is correct
};
```

### 5. Using `std::endl` Instead of `'\n'`

```cpp
// ❌ BAD: Unnecessary flush
std::cout << "Hello" << std::endl;  // Flushes buffer
std::cout << "World" << std::endl;  // Flushes buffer again

// ✅ GOOD: Just newline
std::cout << "Hello\n";
std::cout << "World\n";

// ✅ GOOD: Explicit flush when needed
std::cout << "Critical message" << std::flush;
```

### 6. Not Using `emplace` for Containers

```cpp
// ❌ BAD: Unnecessary temporary
std::vector<Widget> widgets;
widgets.push_back(Widget(10, "name"));  // Creates temporary

// ✅ GOOD: Construct in-place
std::vector<Widget> widgets;
widgets.emplace_back(10, "name");  // No temporary
```

### 7. Passing Large Objects by Value

```cpp
// ❌ BAD: Expensive copy
void process(std::vector<int> data) {
    // data is copied
}

// ✅ GOOD: Pass by const reference
void process(const std::vector<int>& data) {
    // No copy
}

// ✅ GOOD: Pass by value if you need to modify
void process(std::vector<int> data) {
    data.push_back(42);  // Modify the copy
    return data;
}

// ✅ BEST: Move when transferring ownership
std::vector<int> create_data() {
    std::vector<int> data = {1, 2, 3};
    return data;  // RVO or move, no copy
}
```

### 8. Using `using namespace std`

```cpp
// ❌ BAD: Pollutes global namespace
using namespace std;
vector<string> names;

// ✅ GOOD: Explicit namespace
std::vector<std::string> names;

// ✅ ACCEPTABLE: Localized using
void function() {
    using std::cout;
    using std::endl;
    cout << "Message" << endl;
}
```

---

## Performance Optimization

### 1. Use Reserve for Vectors

```cpp
// ❌ BAD: Multiple reallocations
std::vector<int> numbers;
for (int i = 0; i < 1000; ++i) {
    numbers.push_back(i);  // May reallocate multiple times
}

// ✅ GOOD: Single allocation
std::vector<int> numbers;
numbers.reserve(1000);
for (int i = 0; i < 1000; ++i) {
    numbers.push_back(i);  // No reallocation
}
```

### 2. Use `std::string_view` for Read-Only Strings

```cpp
// ❌ BAD: Unnecessary copies
void print(const std::string& str) {
    std::cout << str;
}

std::string name = "John";
print(name);  // OK
print("Literal");  // Creates temporary string

// ✅ GOOD: No copies with string_view
void print(std::string_view str) {
    std::cout << str;
}

std::string name = "John";
print(name);  // No copy
print("Literal");  // No temporary
```

### 3. Use Move Semantics

```cpp
// ❌ BAD: Unnecessary copy
std::vector<int> create_large_vector() {
    std::vector<int> vec(1000000);
    return vec;
}

std::vector<int> data = create_large_vector();  // Copy? No, RVO

// ✅ GOOD: Explicit move when needed
std::vector<int> source = {1, 2, 3, 4, 5};
std::vector<int> dest = std::move(source);  // source is now empty

// ✅ GOOD: Moving in containers
std::vector<std::unique_ptr<Widget>> widgets;
widgets.push_back(std::move(widget_ptr));
```

### 4. Prefer Algorithms Over Hand-Written Loops

```cpp
// ❌ BAD: Manual loop
std::vector<int> numbers = {1, 2, 3, 4, 5};
int sum = 0;
for (const auto& n : numbers) {
    sum += n;
}

// ✅ GOOD: Algorithm (can be optimized by compiler)
int sum = std::accumulate(numbers.begin(), numbers.end(), 0);

// ✅ GOOD: C++20 ranges
int sum = std::ranges::fold_left(numbers, 0, std::plus{});
```

### 5. Use `std::array` for Fixed-Size Arrays

```cpp
// ❌ BAD: C-style array
int numbers[10];
int size = 10;  // Need to track size separately

// ✅ GOOD: std::array
std::array<int, 10> numbers;
auto size = numbers.size();  // Size is part of type

// Benefits: bounds checking, iterators, STL algorithms
```

### 6. Avoid Unnecessary Temporaries

```cpp
// ❌ BAD: Creates temporary
std::string get_name() {
    return std::string("John Doe");
}

// ✅ GOOD: Direct return
std::string get_name() {
    return "John Doe";  // Compiler optimizes
}

// ❌ BAD: Unnecessary temporary
std::vector<int> numbers = {1, 2, 3};
std::vector<int> copy = numbers;
copy[0] = 42;

// ✅ GOOD: Modify in place
std::vector<int> numbers = {1, 2, 3};
numbers[0] = 42;
```

### 7. Use `constexpr` for Compile-Time Computation

```cpp
// ❌ BAD: Runtime computation
const int buffer_size = 1024 * 1024;

// ✅ GOOD: Compile-time computation
constexpr int buffer_size = 1024 * 1024;

// ✅ GOOD: Complex compile-time computation
constexpr int fibonacci(int n) {
    return (n <= 1) ? n : fibonacci(n - 1) + fibonacci(n - 2);
}

constexpr int fib_10 = fibonacci(10);  // Computed at compile time
```

### 8. Profile Before Optimizing

```cpp
// ✅ GOOD: Use profiling tools
// - perf (Linux)
// - Instruments (macOS)
// - VTune (Intel)
// - Valgrind (callgrind)

// ✅ GOOD: Add timing instrumentation
#include <chrono>

class Timer {
    std::chrono::steady_clock::time_point start_;
public:
    Timer() : start_(std::chrono::steady_clock::now()) {}
    
    ~Timer() {
        auto end = std::chrono::steady_clock::now();
        auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(
            end - start_
        );
        std::cout << "Elapsed: " << duration.count() << "ms\n";
    }
};

void expensive_function() {
    Timer timer;  // Automatic timing
    // ... work ...
}
```

---

## Security Best Practices

### 1. Prevent Buffer Overflows

```cpp
// ❌ BAD: Unsafe C functions
char buffer[100];
strcpy(buffer, user_input);  // Buffer overflow risk
gets(buffer);  // Never use gets()

// ✅ GOOD: Safe alternatives
char buffer[100];
strncpy(buffer, user_input, sizeof(buffer) - 1);
buffer[sizeof(buffer) - 1] = '\0';

// ✅ BETTER: Use std::string
std::string buffer = user_input;
```

### 2. Check Integer Overflow

```cpp
// ❌ BAD: Unchecked arithmetic
int multiply(int a, int b) {
    return a * b;  // May overflow
}

// ✅ GOOD: Check for overflow
bool safe_multiply(int a, int b, int& result) {
    if (a > 0 && b > 0 && a > std::numeric_limits<int>::max() / b) {
        return false;  // Would overflow
    }
    result = a * b;
    return true;
}

// ✅ GOOD: Use larger type
long long safe_multiply(int a, int b) {
    return static_cast<long long>(a) * b;
}
```

### 3. Validate All Input

```cpp
// ❌ BAD: No validation
void set_age(int age) {
    age_ = age;
}

// ✅ GOOD: Validate input
void set_age(int age) {
    if (age < 0 || age > 150) {
        throw std::invalid_argument("Invalid age");
    }
    age_ = age;
}

// ✅ GOOD: Strong types
class Age {
    int value_;
public:
    explicit Age(int value) {
        if (value < 0 || value > 150) {
            throw std::invalid_argument("Invalid age");
        }
        value_ = value;
    }
    
    [[nodiscard]] int get() const { return value_; }
};
```

### 4. Zero Sensitive Memory

```cpp
// ❌ BAD: Password left in memory
void authenticate(const std::string& password) {
    // ... use password ...
}  // password may remain in memory

// ✅ GOOD: Explicitly zero memory
void secure_zero(void* ptr, size_t size) {
    volatile unsigned char* p = static_cast<volatile unsigned char*>(ptr);
    while (size--) {
        *p++ = 0;
    }
}

void authenticate(std::string password) {
    // ... use password ...
    secure_zero(password.data(), password.size());
}

// ✅ BETTER: Use dedicated secure string class
class SecureString {
    std::vector<char> data_;
public:
    explicit SecureString(std::string_view str) 
        : data_(str.begin(), str.end()) {}
    
    ~SecureString() {
        secure_zero(data_.data(), data_.size());
    }
    
    // Delete copy, allow move
    SecureString(const SecureString&) = delete;
    SecureString& operator=(const SecureString&) = delete;
    SecureString(SecureString&&) noexcept = default;
    SecureString& operator=(SecureString&&) noexcept = default;
};
```

### 5. Use Sanitizers During Development

```bash
# AddressSanitizer (memory errors)
g++ -fsanitize=address -fno-omit-frame-pointer -g program.cpp

# UndefinedBehaviorSanitizer
g++ -fsanitize=undefined -g program.cpp

# ThreadSanitizer (data races)
g++ -fsanitize=thread -g program.cpp

# MemorySanitizer (uninitialized memory)
clang++ -fsanitize=memory -g program.cpp
```

### 6. Prevent SQL Injection (if using databases)

```cpp
// ❌ BAD: String concatenation
std::string query = "SELECT * FROM users WHERE id = '" + user_id + "'";
execute_query(query);

// ✅ GOOD: Prepared statements
auto stmt = db.prepare("SELECT * FROM users WHERE id = ?");
stmt.bind(1, user_id);
stmt.execute();

// ✅ GOOD: Use ORM
auto user = db.users.find_by_id(user_id);
```

---

## Error Handling

### 1. Use Exceptions for Exceptional Cases

```cpp
// ❌ BAD: Error codes for normal flow
int divide(int a, int b, int& result) {
    if (b == 0) return -1;  // Error code
    result = a / b;
    return 0;  // Success
}

// ✅ GOOD: Exceptions for errors
int divide(int a, int b) {
    if (b == 0) {
        throw std::invalid_argument("Division by zero");
    }
    return a / b;
}

// ✅ GOOD: std::optional for "not found"
std::optional<User> find_user(int id) {
    // ...
    if (not_found) return std::nullopt;
    return user;
}
```

### 2. Create Custom Exception Hierarchy

```cpp
// ✅ GOOD: Structured exception hierarchy
class AppException : public std::exception {
protected:
    std::string message_;
public:
    explicit AppException(std::string msg) 
        : message_(std::move(msg)) {}
    
    [[nodiscard]] const char* what() const noexcept override {
        return message_.c_str();
    }
};

class ValidationError : public AppException {
public:
    explicit ValidationError(const std::string& field, const std::string& reason)
        : AppException("Validation failed for '" + field + "': " + reason) {}
};

class DatabaseError : public AppException {
public:
    explicit DatabaseError(const std::string& operation)
        : AppException("Database error during " + operation) {}
};
```

### 3. Use RAII for Exception Safety

```cpp
// ❌ BAD: Not exception-safe
void process_file(const char* filename) {
    FILE* file = fopen(filename, "r");
    // If exception thrown here, file leaks
    complex_operation();
    fclose(file);
}

// ✅ GOOD: RAII ensures cleanup
void process_file(const char* filename) {
    FileHandle file(filename);
    // Exception-safe: file closed automatically
    complex_operation();
}
```

### 4. Use `noexcept` Appropriately

```cpp
// ✅ GOOD: Mark functions that never throw
class Widget {
public:
    // Destructors should always be noexcept
    ~Widget() noexcept;
    
    // Move operations should be noexcept when possible
    Widget(Widget&&) noexcept;
    Widget& operator=(Widget&&) noexcept;
    
    // Simple getters can be noexcept
    [[nodiscard]] int get_id() const noexcept { return id_; }
    
    // Swap should be noexcept
    void swap(Widget& other) noexcept;
    
private:
    int id_;
};
```

### 5. Catch Exceptions by Reference

```cpp
// ❌ BAD: Catch by value (slicing)
try {
    risky_operation();
} catch (std::exception e) {  // Slicing!
    std::cout << e.what();
}

// ✅ GOOD: Catch by const reference
try {
    risky_operation();
} catch (const std::exception& e) {
    std::cout << e.what();
}

// ✅ GOOD: Catch specific exceptions first
try {
    risky_operation();
} catch (const ValidationError& e) {
    handle_validation_error(e);
} catch (const DatabaseError& e) {
    handle_database_error(e);
} catch (const std::exception& e) {
    handle_generic_error(e);
}
```

### 6. Use `std::expected` (C++23) for Error Handling

```cpp
#include <expected>

// ✅ GOOD: Expected for recoverable errors
enum class ParseError {
    INVALID_FORMAT,
    OUT_OF_RANGE,
    EMPTY_STRING
};

std::expected<int, ParseError> parse_int(std::string_view str) {
    if (str.empty()) {
        return std::unexpected(ParseError::EMPTY_STRING);
    }
    
    try {
        return std::stoi(std::string(str));
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
    std::cout << "Error: " << static_cast<int>(result.error()) << '\n';
}
```

---

## Memory Management

### 1. Prefer Stack Allocation

```cpp
// ✅ GOOD: Stack allocation (automatic cleanup)
void process() {
    Widget widget;  // Destroyed automatically
    std::vector<int> numbers(100);  // Memory freed automatically
}

// ❌ BAD: Unnecessary heap allocation
void process() {
    Widget* widget = new Widget();
    // ... 
    delete widget;
}
```

### 2. Use Smart Pointers for Ownership

```cpp
// ✅ GOOD: Unique ownership
std::unique_ptr<Widget> create_widget() {
    return std::make_unique<Widget>();
}

// ✅ GOOD: Shared ownership
class Node {
    std::shared_ptr<Node> left_;
    std::shared_ptr<Node> right_;
};

// ✅ GOOD: Weak pointers to break cycles
class Node {
    std::shared_ptr<Node> left_;
    std::shared_ptr<Node> right_;
    std::weak_ptr<Node> parent_;  // Breaks circular reference
};
```

### 3. Follow Rule of Zero

```cpp
// ✅ BEST: Let compiler generate everything
class Widget {
    std::string name_;
    std::vector<int> data_;
    std::unique_ptr<Resource> resource_;
public:
    // No need to define special members!
    // Compiler generates correct:
    // - Destructor
    // - Copy constructor
    // - Copy assignment
    // - Move constructor
    // - Move assignment
};
```

### 4. Use `std::make_unique` and `std::make_shared`

```cpp
// ❌ BAD: Direct new
std::unique_ptr<Widget> widget(new Widget(args));
std::shared_ptr<Widget> shared(new Widget(args));

// ✅ GOOD: make_unique/make_shared
auto widget = std::make_unique<Widget>(args);
auto shared = std::make_shared<Widget>(args);

// Benefits:
// - Exception safe
// - More efficient (especially make_shared)
// - Less typing
```

### 5. Be Careful with `this` Pointer

```cpp
// ❌ BAD: Storing raw this pointer
class Widget {
    void register_callback() {
        callback_system.register(this);  // Dangerous!
    }
};

// ✅ GOOD: Use shared_from_this
class Widget : public std::enable_shared_from_this<Widget> {
    void register_callback() {
        callback_system.register(shared_from_this());
    }
};

// Usage
auto widget = std::make_shared<Widget>();
widget->register_callback();  // Safe
```

### 6. Check for Memory Leaks

```bash
# Valgrind
valgrind --leak-check=full ./program

# AddressSanitizer
g++ -fsanitize=address program.cpp
./a.out

# HeapTrack
heaptrack ./program
heaptrack --analyze heaptrack.program.123.gz
```

---

## Type Safety

### 1. Use `enum class` Instead of `enum`

```cpp
// ❌ BAD: Unscoped enum
enum Color { RED, GREEN, BLUE };
enum Status { RED, ACTIVE };  // Error: RED already defined

// ✅ GOOD: Scoped enum
enum class Color { Red, Green, Blue };
enum class Status { Red, Active };

Color c = Color::Red;
Status s = Status::Active;
// if (c == s) {}  // Error: different types
```

### 2. Use Strong Types

```cpp
// ❌ BAD: Primitive obsession
void transfer(int from_account, int to_account, double amount) {
    // Easy to swap arguments
}

// ✅ GOOD: Strong types
class AccountId {
    int value_;
public:
    explicit AccountId(int value) : value_(value) {}
    [[nodiscard]] int get() const { return value_; }
};

class Amount {
    double value_;
public:
    explicit Amount(double value) {
        if (value < 0) throw std::invalid_argument("Negative amount");
        value_ = value;
    }
    [[nodiscard]] double get() const { return value_; }
};

void transfer(AccountId from, AccountId to, Amount amount) {
    // Type-safe: can't swap arguments
}
```

### 3. Use Templates for Generic Code

```cpp
// ✅ GOOD: Type-safe generic function
template<typename T>
T max(T a, T b) {
    return (a > b) ? a : b;
}

// ✅ GOOD: Constrained template (C++20)
template<std::integral T>
T safe_add(T a, T b) {
    // Only accepts integral types
    return a + b;
}
```

### 4. Avoid `void*` and C-Style Casts

```cpp
// ❌ BAD: Type-unsafe void pointer
void process(void* data) {
    int* value = (int*)data;  // Unsafe cast
}

// ✅ GOOD: Templates for type safety
template<typename T>
void process(T* data) {
    // Type-safe
}

// ✅ GOOD: std::any for runtime polymorphism
void process(std::any data) {
    if (auto* value = std::any_cast<int>(&data)) {
        // Type-safe access
    }
}
```

### 5. Use `static_assert` for Compile-Time Checks

```cpp
// ✅ GOOD: Compile-time assertions
template<typename T>
class Container {
    static_assert(std::is_copy_constructible_v<T>, 
                  "T must be copy constructible");
    static_assert(sizeof(T) <= 1024, 
                  "T is too large");
};

// ✅ GOOD: Check assumptions
static_assert(sizeof(int) == 4, "32-bit int required");
static_assert(std::endian::native == std::endian::little, 
              "Little endian required");
```

---

## Code Organization

### 1. Header Guards and `#pragma once`

```cpp
// ✅ GOOD: #pragma once (preferred)
#pragma once

// Header contents

// ✅ ACCEPTABLE: Include guards
#ifndef PROJECT_MODULE_HEADER_HPP
#define PROJECT_MODULE_HEADER_HPP

// Header contents

#endif  // PROJECT_MODULE_HEADER_HPP
```

### 2. Forward Declarations

```cpp
// header.hpp
// ❌ BAD: Unnecessary includes
#include "heavy_class.hpp"

class MyClass {
    HeavyClass* ptr_;  // Only pointer, don't need full definition
};

// ✅ GOOD: Forward declaration
class HeavyClass;  // Forward declaration

class MyClass {
    HeavyClass* ptr_;  // OK with forward declaration
};

// implementation.cpp
#include "heavy_class.hpp"  // Include in implementation
```

### 3. PIMPL Idiom for Implementation Hiding

```cpp
// widget.hpp
class Widget {
public:
    Widget();
    ~Widget();
    
    void do_something();
    
private:
    class Impl;  // Forward declaration
    std::unique_ptr<Impl> impl_;  // Pointer to implementation
};

// widget.cpp
class Widget::Impl {
public:
    // Implementation details hidden from header
    int data_;
    std::vector<std::string> internal_state_;
    
    void internal_method() { /* ... */ }
};

Widget::Widget() : impl_(std::make_unique<Impl>()) {}
Widget::~Widget() = default;  // Must be in .cpp for Impl

void Widget::do_something() {
    impl_->internal_method();
}
```

### 4. Namespace Organization

```cpp
// ✅ GOOD: Hierarchical namespaces
namespace company {
namespace project {
namespace module {

class Widget {
    // ...
};

}  // namespace module
}  // namespace project
}  // namespace company

// ✅ GOOD: C++17 nested namespace
namespace company::project::module {

class Widget {
    // ...
};

}  // namespace company::project::module

// ✅ GOOD: Inline namespace for versioning
namespace company::project::inline v1 {

class Widget {
    // ...
};

}  // namespace company::project::inline v1
```

### 5. Include Order

```cpp
// ✅ GOOD: Consistent include order
// 1. Related header (for .cpp files)
#include "widget.hpp"

// 2. C system headers
#include <sys/types.h>

// 3. C++ standard library
#include <iostream>
#include <vector>
#include <memory>

// 4. Third-party libraries
#include <boost/algorithm/string.hpp>
#include <fmt/format.h>

// 5. Project headers
#include "project/core.hpp"
#include "project/utils.hpp"
```

---

## Testing Best Practices

### 1. Test Structure (Arrange-Act-Assert)

```cpp
// ✅ GOOD: Clear test structure
TEST(WidgetTest, ProcessReturnsCorrectValue) {
    // Arrange
    Widget widget(42);
    const int input = 10;
    const int expected = 52;
    
    // Act
    const int result = widget.process(input);
    
    // Assert
    EXPECT_EQ(result, expected);
}
```

### 2. Test Fixtures for Shared Setup

```cpp
// ✅ GOOD: Reusable test fixture
class WidgetTest : public ::testing::Test {
protected:
    void SetUp() override {
        widget_ = std::make_unique<Widget>(42);
        test_data_ = load_test_data();
    }
    
    void TearDown() override {
        cleanup_resources();
    }
    
    std::unique_ptr<Widget> widget_;
    TestData test_data_;
};

TEST_F(WidgetTest, ProcessHandlesNormalInput) {
    EXPECT_EQ(widget_->process(10), 52);
}

TEST_F(WidgetTest, ProcessHandlesZeroInput) {
    EXPECT_EQ(widget_->process(0), 42);
}
```

### 3. Parameterized Tests

```cpp
// ✅ GOOD: Test multiple inputs
class WidgetParamTest : public ::testing::TestWithParam<std::pair<int, int>> {};

TEST_P(WidgetParamTest, ProcessVariousInputs) {
    const auto [input, expected] = GetParam();
    Widget widget(42);
    
    EXPECT_EQ(widget.process(input), expected);
}

INSTANTIATE_TEST_SUITE_P(
    ValidInputs,
    WidgetParamTest,
    ::testing::Values(
        std::make_pair(0, 42),
        std::make_pair(10, 52),
        std::make_pair(-10, 32)
    )
);
```

### 4. Mock Objects for Dependencies

```cpp
// ✅ GOOD: Mock interface
class DatabaseInterface {
public:
    virtual ~DatabaseInterface() = default;
    virtual std::optional<User> get_user(int id) = 0;
    virtual bool save_user(const User& user) = 0;
};

class MockDatabase : public DatabaseInterface {
public:
    MOCK_METHOD(std::optional<User>, get_user, (int id), (override));
    MOCK_METHOD(bool, save_user, (const User& user), (override));
};

TEST(UserServiceTest, GetUserReturnsCorrectData) {
    MockDatabase mock_db;
    UserService service(&mock_db);
    
    User expected_user{1, "John"};
    EXPECT_CALL(mock_db, get_user(1))
        .WillOnce(::testing::Return(expected_user));
    
    auto result = service.get_user(1);
    
    ASSERT_TRUE(result.has_value());
    EXPECT_EQ(result->name, "John");
}
```

### 5. Test Coverage

```bash
# Generate coverage report with gcov
g++ -fprofile-arcs -ftest-coverage test.cpp
./a.out
gcov test.cpp

# Generate HTML report with lcov
lcov --capture --directory . --output-file coverage.info
genhtml coverage.info --output-directory coverage_html

# Aim for:
# - Overall coverage: >90%
# - Critical paths: 100%
```

---

## Common Gotchas

### 1. Undefined Behavior with Uninitialized Variables

```cpp
// ❌ BAD: Uninitialized variable
int value;  // Contains garbage
std::cout << value;  // Undefined behavior

// ✅ GOOD: Always initialize
int value = 0;
int another{};  // Uniform initialization, zero-initialized
```

### 2. Dangling References

```cpp
// ❌ BAD: Reference to temporary
const std::string& get_name() {
    return std::string("John");  // Returns reference to temporary
}  // Temporary destroyed here

const std::string& name = get_name();  // Dangling reference!

// ✅ GOOD: Return by value
std::string get_name() {
    return "John";  // RVO, no copy
}

std::string name = get_name();  // OK
```

### 3. Iterator Invalidation

```cpp
// ❌ BAD: Iterator invalidation
std::vector<int> numbers = {1, 2, 3, 4, 5};
for (auto it = numbers.begin(); it != numbers.end(); ++it) {
    if (*it % 2 == 0) {
        numbers.erase(it);  // Invalidates iterator!
    }
}

// ✅ GOOD: Use erase-remove idiom
std::vector<int> numbers = {1, 2, 3, 4, 5};
numbers.erase(
    std::remove_if(numbers.begin(), numbers.end(),
                   [](int n) { return n % 2 == 0; }),
    numbers.end()
);

// ✅ GOOD: C++20 ranges
auto result = numbers | std::views::filter([](int n) { return n % 2 != 0; });
```

### 4. Slicing with Polymorphism

```cpp
// ❌ BAD: Object slicing
class Base {
public:
    virtual void print() { std::cout << "Base\n"; }
};

class Derived : public Base {
public:
    void print() override { std::cout << "Derived\n"; }
};

void process(Base obj) {  // Pass by value - slicing!
    obj.print();  // Always prints "Base"
}

Derived d;
process(d);  // Sliced to Base

// ✅ GOOD: Pass by reference
void process(const Base& obj) {
    obj.print();  // Correctly dispatches to Derived
}

// ✅ GOOD: Pass by pointer
void process(const Base* obj) {
    obj->print();
}
```

### 5. Const and Mutable

```cpp
// ⚠️ GOTCHA: const doesn't mean immutable
class Widget {
    mutable int cache_;  // Can be modified in const methods
    int value_;
    
public:
    int get_value() const {
        ++cache_;  // OK: cache_ is mutable
        // ++value_;  // Error: value_ is not mutable
        return value_;
    }
};
```

### 6. Copy vs. Move with RVO

```cpp
// ✅ GOOD: RVO (Return Value Optimization)
std::vector<int> create_vector() {
    std::vector<int> vec(1000);
    return vec;  // No copy, no move - optimized away
}

// ❌ BAD: Prevent RVO
std::vector<int> create_vector() {
    std::vector<int> vec(1000);
    return std::move(vec);  // DON'T DO THIS - prevents RVO!
}
```

### 7. Static Initialization Order Fiasco

```cpp
// file1.cpp
int global1 = compute_value();  // Initialization order undefined

// file2.cpp
int global2 = global1 + 10;  // May use uninitialized global1!

// ✅ GOOD: Function-local static (lazy initialization)
int& get_global() {
    static int value = compute_value();
    return value;
}

// Usage
int global2 = get_global() + 10;  // Always initialized
```

### 8. Unsigned Integer Underflow

```cpp
// ❌ BAD: Unsigned underflow
std::vector<int> vec = {1, 2, 3};
for (size_t i = vec.size() - 1; i >= 0; --i) {  // size_t is unsigned
    // Infinite loop! i wraps around when going below 0
}

// ✅ GOOD: Use signed type or reverse iterator
for (auto it = vec.rbegin(); it != vec.rend(); ++it) {
    // Correct iteration
}

// ✅ GOOD: C++20 ranges
for (int value : vec | std::views::reverse) {
    // Clean and correct
}
```

---

## Quick Reference

### Memory Management Checklist
- [ ] Use RAII for all resources
- [ ] Prefer stack over heap
- [ ] Use smart pointers, not raw pointers
- [ ] Follow Rule of Zero (or Five if needed)
- [ ] No naked `new` or `delete`

### Performance Checklist
- [ ] Reserve space in vectors
- [ ] Use `std::string_view` for read-only strings
- [ ] Enable move semantics
- [ ] Use algorithms over hand-written loops
- [ ] Profile before optimizing

### Safety Checklist
- [ ] Initialize all variables
- [ ] Validate all inputs
- [ ] Check integer overflow
- [ ] Use sanitizers during development
- [ ] Mark functions `const` and `noexcept` appropriately

### Code Quality Checklist
- [ ] Follow const correctness
- [ ] Use `[[nodiscard]]` for important returns
- [ ] Proper exception handling
- [ ] No `using namespace std` in headers
- [ ] Forward declare when possible

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-10-11 | Initial C++ best practices guide |

---

## References

- [C++ Core Guidelines](https://isocpp.github.io/CppCoreGuidelines/)
- [Effective Modern C++](https://www.oreilly.com/library/view/effective-modern-c/9781491908419/)
- [C++ Best Practices (Jason Turner)](https://github.com/cpp-best-practices)

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

