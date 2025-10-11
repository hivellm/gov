# C Best Practices

**Version**: 1.0.0  
**Last Updated**: 2025-10-11  
**Target Audience**: AI Agents developing C projects

---

## Table of Contents

1. [C Idioms](#c-idioms)
2. [Anti-Patterns](#anti-patterns)
3. [Performance Optimization](#performance-optimization)
4. [Security Best Practices](#security-best-practices)
5. [Error Handling](#error-handling)
6. [Memory Management](#memory-management)
7. [Portability](#portability)
8. [Code Organization](#code-organization)
9. [Testing Best Practices](#testing-best-practices)
10. [Common Gotchas](#common-gotchas)

---

## C Idioms

### 1. Always Initialize Variables

```c
// ❌ BAD: Uninitialized variables
int value;
char *str;

// ✅ GOOD: Initialize variables
int value = 0;
char *str = NULL;

// ✅ BETTER: Use calloc for zero-initialization
int *array = calloc(10, sizeof(int));
```

### 2. Use Opaque Pointers

```c
// ❌ BAD: Exposing implementation in header
// header.h
typedef struct {
    int private_data;
    void *internal_state;
} context_t;

// ✅ GOOD: Opaque pointer (forward declaration)
// header.h
typedef struct context context_t;

// implementation.c
struct context {
    int private_data;
    void *internal_state;
};
```

### 3. Return Error Codes, Use Output Parameters

```c
// ❌ BAD: Returning pointer without error indication
char *get_data(const char *key);  // Returns NULL on error

// ✅ GOOD: Error code + output parameter
typedef enum {
    SUCCESS = 0,
    ERROR_NOT_FOUND = -1,
    ERROR_INVALID_PARAM = -2,
} error_t;

error_t get_data(const char *key, char **output) {
    if (key == NULL || output == NULL) {
        return ERROR_INVALID_PARAM;
    }
    
    // Implementation
    *output = result;
    return SUCCESS;
}
```

### 4. Use const Correctly

```c
// ✅ GOOD: const correctness
// Pointer to const data (can't modify data)
const char *str = "hello";

// Const pointer (can't change pointer)
char *const ptr = buffer;

// Const pointer to const data
const char *const str = "hello";

// Function parameters
error_t process(const uint8_t *input, size_t input_size, uint8_t *output);

// Return const pointers for read-only data
const char *get_version(void);
```

### 5. Use sizeof with Variable Names

```c
// ❌ BAD: Using type in sizeof
int *array = malloc(100 * sizeof(int));

// ✅ GOOD: Using variable name
int *array = malloc(100 * sizeof(*array));

// Benefits: Type-safe if variable type changes
```

### 6. Check All Allocations

```c
// ❌ BAD: Not checking allocation
void *ptr = malloc(size);
memcpy(ptr, data, size);  // Crash if ptr is NULL

// ✅ GOOD: Always check
void *ptr = malloc(size);
if (ptr == NULL) {
    return ERROR_OUT_OF_MEMORY;
}
memcpy(ptr, data, size);
```

### 7. Nullify Pointers After Free

```c
// ❌ BAD: Dangling pointer
free(ptr);
// ptr still points to freed memory

// ✅ GOOD: Set to NULL after free
free(ptr);
ptr = NULL;
```

### 8. Use stdint.h for Fixed-Width Types

```c
// ❌ BAD: Platform-dependent sizes
int value;          // Size varies (16/32/64 bits)
unsigned long big;  // Size varies

// ✅ GOOD: Fixed-width types
#include <stdint.h>

int32_t value;      // Always 32 bits
uint64_t big;       // Always 64 bits
size_t count;       // Size of objects
```

---

## Anti-Patterns

### 1. Using gets()

```c
// ❌ NEVER USE gets() - Buffer overflow vulnerability
char buffer[100];
gets(buffer);  // DANGEROUS!

// ✅ GOOD: Use fgets
char buffer[100];
if (fgets(buffer, sizeof(buffer), stdin) != NULL) {
    // Remove newline if present
    buffer[strcspn(buffer, "\n")] = '\0';
}
```

### 2. Ignoring Return Values

```c
// ❌ BAD: Ignoring return values
malloc(size);
fopen("file.txt", "r");
scanf("%d", &value);

// ✅ GOOD: Check return values
void *ptr = malloc(size);
if (ptr == NULL) {
    // Handle error
}

FILE *fp = fopen("file.txt", "r");
if (fp == NULL) {
    // Handle error
}

if (scanf("%d", &value) != 1) {
    // Handle error
}
```

### 3. Using strcpy/strcat Without Bounds Checking

```c
// ❌ BAD: Buffer overflow risk
char dest[10];
strcpy(dest, source);  // What if source is longer?
strcat(dest, more);    // What if result exceeds buffer?

// ✅ GOOD: Use bounded versions
char dest[10];
strncpy(dest, source, sizeof(dest) - 1);
dest[sizeof(dest) - 1] = '\0';  // Ensure null termination

// ✅ BETTER: Use snprintf
snprintf(dest, sizeof(dest), "%s%s", source, more);

// ✅ BEST: Use safe wrapper
error_t safe_copy(char *dest, size_t dest_size, const char *src) {
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
```

### 4. Casting malloc Return Value

```c
// ❌ BAD: Casting malloc (hides errors in C)
int *array = (int *)malloc(size);

// ✅ GOOD: No cast needed in C
int *array = malloc(size);
if (array == NULL) {
    return ERROR_OUT_OF_MEMORY;
}
```

### 5. Using Magic Numbers

```c
// ❌ BAD: Magic numbers
char buffer[256];
for (int i = 0; i < 10; i++) {
    if (value > 100) {
        // ...
    }
}

// ✅ GOOD: Named constants
#define BUFFER_SIZE 256
#define MAX_ITERATIONS 10
#define THRESHOLD 100

char buffer[BUFFER_SIZE];
for (int i = 0; i < MAX_ITERATIONS; i++) {
    if (value > THRESHOLD) {
        // ...
    }
}
```

### 6. Not Using restrict for Optimization

```c
// ❌ SUBOPTIMAL: Compiler can't optimize (pointers may alias)
void copy(void *dest, void *src, size_t n);

// ✅ GOOD: Using restrict hints no aliasing
void copy(void *restrict dest, const void *restrict src, size_t n) {
    memcpy(dest, src, n);  // Compiler can optimize better
}
```

### 7. Comparing Floats with ==

```c
// ❌ BAD: Direct comparison
float a = 0.1f + 0.2f;
if (a == 0.3f) {  // Likely false due to precision
    // ...
}

// ✅ GOOD: Use epsilon comparison
#include <math.h>
#define EPSILON 1e-6f

bool float_equals(float a, float b) {
    return fabsf(a - b) < EPSILON;
}

if (float_equals(a, 0.3f)) {
    // ...
}
```

---

## Performance Optimization

### 1. Use inline for Small Functions

```c
// ✅ GOOD: Inline small frequently-called functions
static inline int min(int a, int b) {
    return a < b ? a : b;
}

static inline int max(int a, int b) {
    return a > b ? a : b;
}
```

### 2. Minimize Dynamic Allocation

```c
// ❌ SLOW: Allocating in hot loop
for (int i = 0; i < 1000000; i++) {
    char *buffer = malloc(1024);
    // Process
    free(buffer);
}

// ✅ FAST: Allocate once outside loop
char *buffer = malloc(1024);
if (buffer != NULL) {
    for (int i = 0; i < 1000000; i++) {
        // Process using same buffer
    }
    free(buffer);
}
```

### 3. Use Stack Allocation for Small Buffers

```c
// ❌ SLOWER: Heap allocation
void process(void) {
    char *buffer = malloc(256);
    if (buffer == NULL) return;
    // Use buffer
    free(buffer);
}

// ✅ FASTER: Stack allocation (if size is reasonable)
void process(void) {
    char buffer[256];
    // Use buffer
}
```

### 4. Cache-Friendly Data Layout

```c
// ❌ BAD: Structure of Arrays (cache unfriendly for iteration)
struct {
    int *ids;
    float *values;
    char **names;
} data;

// ✅ GOOD: Array of Structures (better cache locality)
typedef struct {
    int id;
    float value;
    char *name;
} item_t;

item_t *items = malloc(count * sizeof(item_t));
```

### 5. Use Restrict for Pointer Parameters

```c
// ❌ SLOWER: Compiler must assume aliasing
void add_arrays(int *a, int *b, int *result, size_t n) {
    for (size_t i = 0; i < n; i++) {
        result[i] = a[i] + b[i];
    }
}

// ✅ FASTER: No aliasing, better optimization
void add_arrays(
    const int *restrict a,
    const int *restrict b,
    int *restrict result,
    size_t n)
{
    for (size_t i = 0; i < n; i++) {
        result[i] = a[i] + b[i];
    }
}
```

### 6. Compiler Optimization Flags

```bash
# Development (with debug info)
gcc -O0 -g -Wall -Wextra

# Performance testing
gcc -O2 -march=native

# Maximum optimization
gcc -O3 -march=native -flto -DNDEBUG

# Size optimization
gcc -Os -DNDEBUG
```

### 7. Profile Before Optimizing

```bash
# Compile with profiling
gcc -pg -o program program.c

# Run program
./program

# Analyze
gprof program gmon.out > analysis.txt

# Or use perf (Linux)
perf record ./program
perf report
```

---

## Security Best Practices

### 1. Validate All Inputs

```c
// ✅ GOOD: Comprehensive input validation
error_t process_data(const uint8_t *data, size_t size) {
    // Check for NULL
    if (data == NULL) {
        return ERROR_INVALID_PARAM;
    }
    
    // Check for reasonable size
    if (size == 0 || size > MAX_DATA_SIZE) {
        return ERROR_INVALID_SIZE;
    }
    
    // Check for integer overflow
    size_t required = size * 2;
    if (required < size) {
        return ERROR_OVERFLOW;
    }
    
    // Process data
    return SUCCESS;
}
```

### 2. Use Secure String Functions

```c
// ❌ UNSAFE: strcpy, strcat, sprintf
strcpy(dest, src);
strcat(dest, more);
sprintf(buffer, "%s", input);

// ✅ SAFE: Bounded alternatives
strncpy(dest, src, sizeof(dest) - 1);
dest[sizeof(dest) - 1] = '\0';

strncat(dest, more, sizeof(dest) - strlen(dest) - 1);

snprintf(buffer, sizeof(buffer), "%s", input);
```

### 3. Clear Sensitive Data

```c
// ❌ BAD: Compiler may optimize out the memset
char password[64];
// Use password
memset(password, 0, sizeof(password));  // May be optimized away!

// ✅ GOOD: Use explicit_bzero or volatile
#ifdef __linux__
explicit_bzero(password, sizeof(password));
#else
// Fallback for other platforms
volatile char *p = password;
while (sizeof(password)--) {
    *p++ = 0;
}
#endif

// ✅ ALTERNATIVE: Use memset_s (C11)
#ifdef __STDC_LIB_EXT1__
memset_s(password, sizeof(password), 0, sizeof(password));
#endif
```

### 4. Avoid Time-of-Check-Time-of-Use (TOCTOU)

```c
// ❌ VULNERABLE: TOCTOU race condition
if (access("file.txt", W_OK) == 0) {
    FILE *fp = fopen("file.txt", "w");  // File may have changed!
    // ...
}

// ✅ BETTER: Open directly and check result
FILE *fp = fopen("file.txt", "w");
if (fp == NULL) {
    if (errno == EACCES) {
        // Permission denied
    }
    return ERROR_FILE_ACCESS;
}
```

### 5. Use Safe Integer Operations

```c
// ❌ VULNERABLE: Integer overflow
size_t total = count * size;
void *buffer = malloc(total);  // Overflow may cause small allocation!

// ✅ SAFE: Check for overflow
size_t total;
if (__builtin_mul_overflow(count, size, &total)) {
    return ERROR_OVERFLOW;
}
void *buffer = malloc(total);

// ✅ ALTERNATIVE: Manual check
if (size > 0 && count > SIZE_MAX / size) {
    return ERROR_OVERFLOW;
}
size_t total = count * size;
```

### 6. Avoid Format String Vulnerabilities

```c
// ❌ VULNERABLE: User-controlled format string
char user_input[256];
printf(user_input);  // DANGEROUS!

// ✅ SAFE: Use literal format string
printf("%s", user_input);

// ❌ VULNERABLE: sprintf with user input
sprintf(buffer, user_input);

// ✅ SAFE: Use format specifier
snprintf(buffer, sizeof(buffer), "%s", user_input);
```

### 7. Randomness for Security

```c
// ❌ INSECURE: rand() is not cryptographically secure
srand(time(NULL));
int token = rand();

// ✅ SECURE: Use cryptographically secure PRNG
#ifdef __linux__
#include <sys/random.h>

uint32_t token;
if (getrandom(&token, sizeof(token), 0) != sizeof(token)) {
    return ERROR_RANDOM;
}
#elif defined(_WIN32)
#include <windows.h>
#include <bcrypt.h>

uint32_t token;
BCryptGenRandom(NULL, (PUCHAR)&token, sizeof(token), BCRYPT_USE_SYSTEM_PREFERRED_RNG);
#endif
```

---

## Error Handling

### 1. Consistent Error Reporting

```c
// ✅ GOOD: Define error codes
typedef enum {
    SUCCESS = 0,
    ERROR_INVALID_PARAM = -1,
    ERROR_OUT_OF_MEMORY = -2,
    ERROR_IO = -3,
    ERROR_NOT_FOUND = -4,
    ERROR_PERMISSION = -5,
} error_t;

// Use consistently across all functions
error_t init(context_t **ctx);
error_t process(context_t *ctx, const data_t *input);
void cleanup(context_t *ctx);
```

### 2. Error Context

```c
// ✅ GOOD: Provide error context
typedef struct {
    error_t code;
    char message[256];
    const char *file;
    int line;
} error_info_t;

#define SET_ERROR(info, err_code, msg) do { \
    (info)->code = (err_code); \
    snprintf((info)->message, sizeof((info)->message), "%s", (msg)); \
    (info)->file = __FILE__; \
    (info)->line = __LINE__; \
} while(0)

error_t function(error_info_t *error) {
    if (something_failed) {
        SET_ERROR(error, ERROR_IO, "Failed to read file");
        return ERROR_IO;
    }
    return SUCCESS;
}
```

### 3. Cleanup on Error

```c
// ✅ GOOD: Proper cleanup using goto
error_t complex_operation(void) {
    error_t result = SUCCESS;
    void *buffer1 = NULL;
    void *buffer2 = NULL;
    FILE *fp = NULL;
    
    buffer1 = malloc(SIZE1);
    if (buffer1 == NULL) {
        result = ERROR_OUT_OF_MEMORY;
        goto cleanup;
    }
    
    buffer2 = malloc(SIZE2);
    if (buffer2 == NULL) {
        result = ERROR_OUT_OF_MEMORY;
        goto cleanup;
    }
    
    fp = fopen("file.txt", "r");
    if (fp == NULL) {
        result = ERROR_IO;
        goto cleanup;
    }
    
    // Process...
    
cleanup:
    if (fp != NULL) fclose(fp);
    free(buffer2);
    free(buffer1);
    
    return result;
}
```

### 4. Error Propagation

```c
// ✅ GOOD: Propagate errors up the call stack
error_t low_level_operation(void) {
    if (failed) {
        return ERROR_IO;
    }
    return SUCCESS;
}

error_t mid_level_operation(void) {
    error_t err = low_level_operation();
    if (err != SUCCESS) {
        // Log or add context
        fprintf(stderr, "Low level operation failed\n");
        return err;  // Propagate
    }
    return SUCCESS;
}

error_t high_level_operation(void) {
    error_t err = mid_level_operation();
    if (err != SUCCESS) {
        // Handle or propagate
        return err;
    }
    return SUCCESS;
}
```

---

## Memory Management

### 1. RAII-like Pattern with Cleanup Attributes

```c
// ✅ GOOD: Automatic cleanup using GCC/Clang attributes
#define AUTO_FREE __attribute__((cleanup(cleanup_free)))

static void cleanup_free(void *p) {
    void **ptr = (void **)p;
    free(*ptr);
}

void function(void) {
    AUTO_FREE char *buffer = malloc(1024);
    if (buffer == NULL) {
        return;  // No need to explicitly free
    }
    
    // Use buffer
    
    // buffer is automatically freed on return
}
```

### 2. Memory Pools

```c
// ✅ GOOD: Memory pool for frequent allocations
typedef struct {
    void *memory;
    size_t size;
    size_t used;
} memory_pool_t;

error_t pool_init(memory_pool_t *pool, size_t size) {
    pool->memory = malloc(size);
    if (pool->memory == NULL) {
        return ERROR_OUT_OF_MEMORY;
    }
    pool->size = size;
    pool->used = 0;
    return SUCCESS;
}

void *pool_alloc(memory_pool_t *pool, size_t size) {
    if (pool->used + size > pool->size) {
        return NULL;
    }
    
    void *ptr = (char *)pool->memory + pool->used;
    pool->used += size;
    return ptr;
}

void pool_reset(memory_pool_t *pool) {
    pool->used = 0;
}

void pool_cleanup(memory_pool_t *pool) {
    free(pool->memory);
    pool->memory = NULL;
}
```

### 3. Reference Counting

```c
// ✅ GOOD: Reference counting for shared resources
typedef struct {
    void *data;
    size_t ref_count;
} shared_resource_t;

shared_resource_t *resource_create(void) {
    shared_resource_t *res = malloc(sizeof(shared_resource_t));
    if (res == NULL) return NULL;
    
    res->data = malloc(DATA_SIZE);
    if (res->data == NULL) {
        free(res);
        return NULL;
    }
    
    res->ref_count = 1;
    return res;
}

void resource_retain(shared_resource_t *res) {
    if (res != NULL) {
        res->ref_count++;
    }
}

void resource_release(shared_resource_t *res) {
    if (res == NULL) return;
    
    res->ref_count--;
    if (res->ref_count == 0) {
        free(res->data);
        free(res);
    }
}
```

### 4. Aligned Memory Allocation

```c
// ✅ GOOD: Allocate aligned memory for SIMD
#include <stdlib.h>

void *aligned_alloc_safe(size_t alignment, size_t size) {
    // Ensure alignment is power of 2
    if ((alignment & (alignment - 1)) != 0) {
        return NULL;
    }
    
    #ifdef _WIN32
    return _aligned_malloc(size, alignment);
    #else
    return aligned_alloc(alignment, size);
    #endif
}

void aligned_free_safe(void *ptr) {
    #ifdef _WIN32
    _aligned_free(ptr);
    #else
    free(ptr);
    #endif
}

// Usage for SIMD (16-byte aligned for SSE, 32 for AVX)
float *data = aligned_alloc_safe(32, sizeof(float) * COUNT);
```

---

## Portability

### 1. Use Standard Types

```c
// ✅ GOOD: Portable types
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

int32_t value;      // Always 32-bit signed
uint64_t large;     // Always 64-bit unsigned
size_t count;       // Size of objects
ptrdiff_t diff;     // Pointer difference
bool flag;          // Boolean type
```

### 2. Platform-Specific Code

```c
// ✅ GOOD: Conditional compilation
#ifdef _WIN32
    #include <windows.h>
    #define PATH_SEPARATOR '\\'
#elif defined(__linux__) || defined(__APPLE__)
    #include <unistd.h>
    #define PATH_SEPARATOR '/'
#endif

// ✅ GOOD: Platform abstraction
#ifdef _WIN32
    #define sleep_ms(ms) Sleep(ms)
#else
    #include <unistd.h>
    #define sleep_ms(ms) usleep((ms) * 1000)
#endif
```

### 3. Endianness Handling

```c
// ✅ GOOD: Portable endianness conversion
#include <stdint.h>

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    #define htobe32(x) __builtin_bswap32(x)
    #define be32toh(x) __builtin_bswap32(x)
#else
    #define htobe32(x) (x)
    #define be32toh(x) (x)
#endif

// Usage
uint32_t network_value = htobe32(host_value);
uint32_t host_value = be32toh(network_value);
```

### 4. File Path Handling

```c
// ✅ GOOD: Portable path concatenation
#include <stdio.h>

#ifdef _WIN32
    #define PATH_SEP "\\"
#else
    #define PATH_SEP "/"
#endif

error_t build_path(char *dest, size_t dest_size, 
                   const char *dir, const char *file) {
    int ret = snprintf(dest, dest_size, "%s%s%s", dir, PATH_SEP, file);
    if (ret < 0 || (size_t)ret >= dest_size) {
        return ERROR_BUFFER_TOO_SMALL;
    }
    return SUCCESS;
}
```

---

## Code Organization

### 1. Header Organization

```c
// ✅ GOOD: Well-organized header file
#ifndef MODULE_H
#define MODULE_H

/* System includes */
#include <stddef.h>
#include <stdint.h>

/* Third-party includes */
#include <external_lib.h>

/* Project includes */
#include "common_types.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Version information */
#define MODULE_VERSION_MAJOR 1
#define MODULE_VERSION_MINOR 0
#define MODULE_VERSION_PATCH 0

/* Public constants */
#define MODULE_MAX_SIZE 1024

/* Public types */
typedef struct module_context module_context_t;

typedef enum {
    MODULE_SUCCESS = 0,
    MODULE_ERROR = -1,
} module_error_t;

/* Public API */
module_error_t module_init(module_context_t **ctx);
void module_cleanup(module_context_t *ctx);
module_error_t module_process(module_context_t *ctx, const void *data);

#ifdef __cplusplus
}
#endif

#endif /* MODULE_H */
```

### 2. Implementation Organization

```c
// ✅ GOOD: Well-organized implementation file
#include "module.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>

/* Internal constants */
#define INTERNAL_BUFFER_SIZE 512

/* Internal types */
struct module_context {
    void *data;
    size_t size;
    bool initialized;
};

/* Internal function declarations */
static error_t validate_params(const module_context_t *ctx);
static error_t allocate_resources(module_context_t *ctx);
static void free_resources(module_context_t *ctx);

/* Internal function implementations */
static error_t validate_params(const module_context_t *ctx) {
    if (ctx == NULL) {
        return MODULE_ERROR;
    }
    return MODULE_SUCCESS;
}

/* Public API implementations */
module_error_t module_init(module_context_t **ctx) {
    /* Implementation */
}
```

### 3. Modular Design

```c
// ✅ GOOD: Separate concerns into modules
// network.h - Network operations
// storage.h - Data storage
// crypto.h - Cryptographic operations
// utils.h - Utility functions

// main.c ties them together
#include "network.h"
#include "storage.h"
#include "crypto.h"
#include "utils.h"
```

---

## Testing Best Practices

### 1. Test Boundary Conditions

```c
// ✅ GOOD: Test edge cases
void test_buffer_operations(void) {
    // Test empty buffer
    assert(process(NULL, 0) == ERROR_INVALID_PARAM);
    
    // Test single element
    uint8_t single[1] = {0};
    assert(process(single, 1) == SUCCESS);
    
    // Test maximum size
    uint8_t *large = malloc(MAX_SIZE);
    assert(process(large, MAX_SIZE) == SUCCESS);
    free(large);
    
    // Test overflow (MAX_SIZE + 1)
    uint8_t *overflow = malloc(MAX_SIZE + 1);
    assert(process(overflow, MAX_SIZE + 1) == ERROR_INVALID_SIZE);
    free(overflow);
}
```

### 2. Memory Leak Testing

```c
// ✅ GOOD: Test for memory leaks
void test_memory_management(void) {
    // Valgrind should show no leaks
    module_context_t *ctx = NULL;
    
    // Test normal flow
    assert(module_init(&ctx) == SUCCESS);
    assert(ctx != NULL);
    module_cleanup(ctx);
    
    // Test error flow
    assert(module_init(NULL) == ERROR_INVALID_PARAM);
}

// Run with: valgrind --leak-check=full ./test
```

### 3. Mock External Dependencies

```c
// ✅ GOOD: Use function pointers for testability
typedef int (*read_fn)(void *ctx, void *buffer, size_t size);
typedef int (*write_fn)(void *ctx, const void *buffer, size_t size);

typedef struct {
    read_fn read;
    write_fn write;
    void *ctx;
} io_operations_t;

// Production code uses real functions
int real_read(void *ctx, void *buffer, size_t size) {
    return fread(buffer, 1, size, (FILE *)ctx);
}

// Test code uses mocks
int mock_read(void *ctx, void *buffer, size_t size) {
    // Return test data
    return TEST_SIZE;
}
```

---

## Common Gotchas

### 1. Undefined Behavior

```c
// ❌ UNDEFINED BEHAVIOR: Modifying string literal
char *str = "hello";
str[0] = 'H';  // CRASH!

// ✅ CORRECT: Use array or malloc
char str[] = "hello";
str[0] = 'H';  // OK

// Or
char *str = malloc(6);
strcpy(str, "hello");
str[0] = 'H';  // OK
free(str);
```

### 2. Sequence Point Violations

```c
// ❌ UNDEFINED: Multiple modifications between sequence points
int i = 0;
int j = i++ + i++;  // Undefined behavior!

// ✅ CORRECT: Separate statements
int i = 0;
int j = i;
i++;
j += i;
i++;
```

### 3. Array Decay

```c
// ❌ GOTCHA: sizeof on array parameter
void func(int array[100]) {
    // sizeof(array) is sizeof(int*), not sizeof(int[100])!
    size_t size = sizeof(array);  // Wrong! Returns pointer size
}

// ✅ CORRECT: Pass size explicitly
void func(int *array, size_t array_size) {
    // Use array_size
}

// Or use a macro for local arrays
#define ARRAY_SIZE(arr) (sizeof(arr) / sizeof((arr)[0]))
```

### 4. Macro Pitfalls

```c
// ❌ BAD: Macro without parentheses
#define SQUARE(x) x * x
int result = SQUARE(1 + 2);  // Expands to 1 + 2 * 1 + 2 = 5, not 9!

// ✅ GOOD: Use parentheses
#define SQUARE(x) ((x) * (x))
int result = SQUARE(1 + 2);  // Correctly expands to ((1 + 2) * (1 + 2)) = 9

// ❌ BAD: Macro with side effects
int i = 5;
int result = SQUARE(i++);  // i is incremented twice!

// ✅ GOOD: Use inline function instead
static inline int square(int x) {
    return x * x;
}
```

### 5. Signed Integer Overflow

```c
// ❌ UNDEFINED: Signed integer overflow
int a = INT_MAX;
int b = a + 1;  // Undefined behavior!

// ✅ CORRECT: Check before operation
#include <limits.h>

if (a > INT_MAX - 1) {
    // Handle overflow
}

// Or use unsigned
unsigned int a = UINT_MAX;
unsigned int b = a + 1;  // Defined: wraps to 0
```

### 6. Switch Fall-Through

```c
// ❌ GOTCHA: Unintended fall-through
switch (value) {
case 1:
    do_something();
    // Falls through to case 2!
case 2:
    do_something_else();
    break;
}

// ✅ CORRECT: Explicit break or comment
switch (value) {
case 1:
    do_something();
    break;  // Explicit break
case 2:
    do_something_else();
    break;
case 3:
    do_something();
    /* FALLTHROUGH */  // Explicit comment for intentional fall-through
case 4:
    do_common_thing();
    break;
}
```

---

## Additional Resources

- [SEI CERT C Coding Standard](https://wiki.sei.cmu.edu/confluence/display/c/SEI+CERT+C+Coding+Standard)
- [MISRA C Guidelines](https://www.misra.org.uk/)
- [ISO/IEC 9899:2018 (C17 Standard)](https://www.iso.org/standard/74528.html)
- [GCC Warning Options](https://gcc.gnu.org/onlinedocs/gcc/Warning-Options.html)
- [Valgrind Documentation](https://valgrind.org/docs/manual/)

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

