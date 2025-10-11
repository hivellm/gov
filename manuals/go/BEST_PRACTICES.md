# Go Best Practices

**Version**: 1.0.0  
**Last Updated**: 2025-10-11  
**Target Audience**: AI Agents developing Go projects

---

## Table of Contents

1. [Go Idioms](#go-idioms)
2. [Anti-Patterns](#anti-patterns)
3. [Performance Optimization](#performance-optimization)
4. [Security Best Practices](#security-best-practices)
5. [Error Handling](#error-handling)
6. [Concurrency Patterns](#concurrency-patterns)
7. [Interface Design](#interface-design)
8. [Code Organization](#code-organization)
9. [Testing Best Practices](#testing-best-practices)
10. [Common Gotchas](#common-gotchas)

---

## Go Idioms

### 1. Accept Interfaces, Return Structs

```go
// ❌ BAD: Accept and return concrete types
func ProcessUser(repo *UserRepository) *User {
    return repo.GetUser()
}

// ✅ GOOD: Accept interface, return struct
func ProcessUser(repo UserRepository) *User {
    return repo.GetUser()
}
```

### 2. Make Zero Value Useful

```go
// ✅ GOOD: Zero value is ready to use
type Buffer struct {
    data []byte
}

func (b *Buffer) Write(p []byte) {
    if b.data == nil {
        b.data = make([]byte, 0)
    }
    b.data = append(b.data, p...)
}

// Usage - no initialization needed
var buf Buffer
buf.Write([]byte("hello"))
```

### 3. Use defer for Cleanup

```go
// ❌ BAD: Manual cleanup
func readFile(path string) ([]byte, error) {
    f, err := os.Open(path)
    if err != nil {
        return nil, err
    }
    data, err := io.ReadAll(f)
    f.Close()  // Easy to forget
    return data, err
}

// ✅ GOOD: defer ensures cleanup
func readFile(path string) ([]byte, error) {
    f, err := os.Open(path)
    if err != nil {
        return nil, err
    }
    defer f.Close()
    
    return io.ReadAll(f)
}
```

### 4. Handle Errors Explicitly

```go
// ❌ BAD: Ignoring errors
result, _ := DoSomething()

// ✅ GOOD: Handle errors
result, err := DoSomething()
if err != nil {
    return fmt.Errorf("failed to do something: %w", err)
}
```

### 5. Use Channels for Communication

```go
// ✅ GOOD: Use channels between goroutines
func worker(jobs <-chan int, results chan<- int) {
    for job := range jobs {
        results <- process(job)
    }
}

func main() {
    jobs := make(chan int, 100)
    results := make(chan int, 100)
    
    // Start workers
    for w := 0; w < 3; w++ {
        go worker(jobs, results)
    }
    
    // Send jobs
    for j := 0; j < 5; j++ {
        jobs <- j
    }
    close(jobs)
    
    // Get results
    for r := 0; r < 5; r++ {
        <-results
    }
}
```

### 6. Use Functional Options Pattern

```go
// ✅ GOOD: Flexible configuration
type Server struct {
    host string
    port int
    timeout time.Duration
}

type Option func(*Server)

func WithHost(host string) Option {
    return func(s *Server) {
        s.host = host
    }
}

func WithPort(port int) Option {
    return func(s *Server) {
        s.port = port
    }
}

func NewServer(opts ...Option) *Server {
    s := &Server{
        host:    "localhost",
        port:    8080,
        timeout: 30 * time.Second,
    }
    
    for _, opt := range opts {
        opt(s)
    }
    
    return s
}

// Usage
server := NewServer(
    WithHost("0.0.0.0"),
    WithPort(3000),
)
```

### 7. Use Table-Driven Tests

```go
func TestValidateEmail(t *testing.T) {
    tests := []struct {
        name    string
        email   string
        wantErr bool
    }{
        {"valid email", "test@example.com", false},
        {"invalid email", "invalid", true},
        {"empty email", "", true},
    }
    
    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            err := ValidateEmail(tt.email)
            if (err != nil) != tt.wantErr {
                t.Errorf("ValidateEmail() error = %v, wantErr %v", err, tt.wantErr)
            }
        })
    }
}
```

### 8. Use Context for Cancellation

```go
// ✅ GOOD: Respect context cancellation
func fetchData(ctx context.Context) ([]byte, error) {
    req, err := http.NewRequestWithContext(ctx, "GET", url, nil)
    if err != nil {
        return nil, err
    }
    
    resp, err := http.DefaultClient.Do(req)
    if err != nil {
        return nil, err
    }
    defer resp.Body.Close()
    
    return io.ReadAll(resp.Body)
}
```

### 9. Use sync.Once for One-Time Initialization

```go
var (
    instance *Singleton
    once     sync.Once
)

func GetInstance() *Singleton {
    once.Do(func() {
        instance = &Singleton{}
    })
    return instance
}
```

### 10. Use Named Return Values for Documentation

```go
// ✅ GOOD: Named returns document purpose
func divide(a, b int) (quotient int, remainder int, err error) {
    if b == 0 {
        return 0, 0, errors.New("division by zero")
    }
    return a / b, a % b, nil
}
```

---

## Anti-Patterns

### 1. Goroutine Leaks

```go
// ❌ BAD: Goroutine never terminates
func StartWorker() {
    go func() {
        for {
            // No way to stop
            doWork()
        }
    }()
}

// ✅ GOOD: Goroutine can be stopped
func StartWorker(ctx context.Context) {
    go func() {
        for {
            select {
            case <-ctx.Done():
                return
            default:
                doWork()
            }
        }
    }()
}
```

### 2. Not Closing Channels

```go
// ❌ BAD: Range will block forever
func producer(ch chan int) {
    for i := 0; i < 10; i++ {
        ch <- i
    }
    // Forgot to close
}

// ✅ GOOD: Close when done
func producer(ch chan int) {
    defer close(ch)
    for i := 0; i < 10; i++ {
        ch <- i
    }
}
```

### 3. Not Using Pointers Appropriately

```go
// ❌ BAD: Copying large struct
func ProcessUser(user User) {
    // User is copied
}

// ✅ GOOD: Use pointer
func ProcessUser(user *User) {
    // No copy
}
```

### 4. Not Handling Errors

```go
// ❌ BAD: Ignoring error
result, _ := DoSomething()

// ✅ GOOD: Handle error
result, err := DoSomething()
if err != nil {
    log.Printf("failed: %v", err)
    return err
}
```

### 5. Reinventing the Wheel

```go
// ❌ BAD: Custom implementation
func contains(slice []string, item string) bool {
    for _, s := range slice {
        if s == item {
            return true
        }
    }
    return false
}

// ✅ GOOD: Use slices package (Go 1.21+)
import "slices"

if slices.Contains(slice, item) {
    // ...
}
```

---

## Performance Optimization

### 1. Use sync.Pool for Temporary Objects

```go
var bufferPool = sync.Pool{
    New: func() interface{} {
        return new(bytes.Buffer)
    },
}

func processData(data []byte) []byte {
    buf := bufferPool.Get().(*bytes.Buffer)
    defer bufferPool.Put(buf)
    
    buf.Reset()
    buf.Write(data)
    // Process...
    
    return buf.Bytes()
}
```

### 2. Preallocate Slices When Size is Known

```go
// ❌ SLOW: Multiple allocations
var result []int
for i := 0; i < 1000; i++ {
    result = append(result, i)
}

// ✅ FAST: Single allocation
result := make([]int, 0, 1000)
for i := 0; i < 1000; i++ {
    result = append(result, i)
}
```

### 3. Use strings.Builder for Concatenation

```go
// ❌ SLOW: String concatenation
var result string
for _, s := range strings {
    result += s  // Creates new string each time
}

// ✅ FAST: strings.Builder
var builder strings.Builder
for _, s := range strings {
    builder.WriteString(s)
}
result := builder.String()
```

### 4. Use Map for Fast Lookups

```go
// ❌ SLOW: O(n) lookup
func contains(slice []string, item string) bool {
    for _, s := range slice {
        if s == item {
            return true
        }
    }
    return false
}

// ✅ FAST: O(1) lookup
items := map[string]bool{
    "item1": true,
    "item2": true,
}

if items["item1"] {
    // Found
}
```

---

## Concurrency Patterns

### 1. Worker Pool

```go
func workerPool(jobs <-chan Job, results chan<- Result, workers int) {
    var wg sync.WaitGroup
    
    for i := 0; i < workers; i++ {
        wg.Add(1)
        go func() {
            defer wg.Done()
            for job := range jobs {
                results <- process(job)
            }
        }()
    }
    
    wg.Wait()
    close(results)
}
```

### 2. Fan-Out, Fan-In

```go
func fanOut(input <-chan int, workers int) []<-chan int {
    channels := make([]<-chan int, workers)
    
    for i := 0; i < workers; i++ {
        ch := make(chan int)
        channels[i] = ch
        
        go func() {
            defer close(ch)
            for val := range input {
                ch <- process(val)
            }
        }()
    }
    
    return channels
}

func fanIn(channels ...<-chan int) <-chan int {
    out := make(chan int)
    var wg sync.WaitGroup
    
    for _, ch := range channels {
        wg.Add(1)
        go func(c <-chan int) {
            defer wg.Done()
            for val := range c {
                out <- val
            }
        }(ch)
    }
    
    go func() {
        wg.Wait()
        close(out)
    }()
    
    return out
}
```

### 3. Semaphore Pattern

```go
type Semaphore chan struct{}

func NewSemaphore(max int) Semaphore {
    return make(chan struct{}, max)
}

func (s Semaphore) Acquire() {
    s <- struct{}{}
}

func (s Semaphore) Release() {
    <-s
}

// Usage
sem := NewSemaphore(10)  // Max 10 concurrent operations

for _, item := range items {
    sem.Acquire()
    go func(item Item) {
        defer sem.Release()
        process(item)
    }(item)
}
```

---

## Security Best Practices

### 1. Input Validation

```go
import "github.com/go-playground/validator/v10"

type CreateUserRequest struct {
    Email string `json:"email" validate:"required,email"`
    Name  string `json:"name" validate:"required,min=2,max=100"`
    Age   int    `json:"age" validate:"omitempty,min=0,max=150"`
}

func validate(req CreateUserRequest) error {
    validate := validator.New()
    return validate.Struct(req)
}
```

### 2. SQL Injection Prevention

```go
// ❌ BAD: String concatenation
query := fmt.Sprintf("SELECT * FROM users WHERE id = '%s'", userID)
rows, err := db.Query(query)

// ✅ GOOD: Parameterized query
query := "SELECT * FROM users WHERE id = $1"
rows, err := db.Query(query, userID)
```

### 3. Use crypto Packages

```go
import (
    "crypto/rand"
    "crypto/sha256"
    "golang.org/x/crypto/bcrypt"
)

// ✅ GOOD: Secure random
token := make([]byte, 32)
_, err := rand.Read(token)

// ✅ GOOD: Password hashing
hash, err := bcrypt.GenerateFromPassword([]byte(password), bcrypt.DefaultCost)

// Verify
err = bcrypt.CompareHashAndPassword(hash, []byte(password))
```

### 4. Rate Limiting

```go
import "golang.org/x/time/rate"

// ✅ GOOD: Rate limiter
limiter := rate.NewLimiter(10, 100)  // 10 req/s, burst of 100

if !limiter.Allow() {
    http.Error(w, "Too many requests", http.StatusTooManyRequests)
    return
}
```

---

## Error Handling

### 1. Wrap Errors with Context

```go
import "fmt"

// ✅ GOOD: Add context to errors
func processFile(path string) error {
    data, err := os.ReadFile(path)
    if err != nil {
        return fmt.Errorf("failed to read file %s: %w", path, err)
    }
    
    if err := process(data); err != nil {
        return fmt.Errorf("failed to process data: %w", err)
    }
    
    return nil
}
```

### 2. Use errors.Is and errors.As

```go
import "errors"

// ✅ GOOD: Check error type
if errors.Is(err, ErrNotFound) {
    // Handle not found
}

// ✅ GOOD: Get underlying error
var appErr *AppError
if errors.As(err, &appErr) {
    log.Printf("App error code: %s", appErr.Code)
}
```

### 3. Define Sentinel Errors

```go
// ✅ GOOD: Package-level error variables
var (
    ErrNotFound      = errors.New("not found")
    ErrAlreadyExists = errors.New("already exists")
    ErrInvalidInput  = errors.New("invalid input")
)
```

---

## Interface Design

### 1. Keep Interfaces Small

```go
// ✅ GOOD: Small, focused interfaces
type Reader interface {
    Read(p []byte) (n int, err error)
}

type Writer interface {
    Write(p []byte) (n int, err error)
}

// Compose when needed
type ReadWriter interface {
    Reader
    Writer
}
```

### 2. Define Interfaces at Usage Site

```go
// ❌ BAD: Define interface with implementation
// repository/user_repository.go
type UserRepository interface {
    FindByID(id string) (*User, error)
}

type PostgresUserRepository struct {}

// ✅ GOOD: Define interface where it's used
// service/user_service.go
type userRepository interface {
    FindByID(id string) (*User, error)
}

type UserService struct {
    repo userRepository
}
```

---

## Testing Best Practices

### 1. Use Subtests

```go
func TestUser(t *testing.T) {
    t.Run("CreateUser", func(t *testing.T) {
        t.Run("with valid data", func(t *testing.T) {
            // Test
        })
        
        t.Run("with invalid email", func(t *testing.T) {
            // Test
        })
    })
}
```

### 2. Use testify for Assertions

```go
import "github.com/stretchr/testify/assert"

func TestDivide(t *testing.T) {
    result, err := divide(10, 2)
    assert.NoError(t, err)
    assert.Equal(t, 5, result)
}
```

### 3. Use Mocks with Interface

```go
type mockRepository struct {
    mock.Mock
}

func (m *mockRepository) FindByID(id string) (*User, error) {
    args := m.Called(id)
    if args.Get(0) == nil {
        return nil, args.Error(1)
    }
    return args.Get(0).(*User), args.Error(1)
}
```

---

## Common Gotchas

### 1. Slice Append Gotcha

```go
// ⚠️ WARNING: append can reallocate
s1 := []int{1, 2, 3}
s2 := append(s1, 4)
s2[0] = 99  // May or may not affect s1!

// ✅ SAFE: Copy explicitly if needed
s2 := make([]int, len(s1))
copy(s2, s1)
```

### 2. Range Loop Variable

```go
// ❌ BAD: All goroutines see same value
for _, item := range items {
    go func() {
        process(item)  // Bug: all see last item
    }()
}

// ✅ GOOD: Pass as parameter
for _, item := range items {
    go func(i Item) {
        process(i)
    }(item)
}
```

### 3. Map Concurrent Access

```go
// ❌ BAD: Data race
m := make(map[string]int)
go func() { m["key"] = 1 }()
go func() { m["key"] = 2 }()  // Race!

// ✅ GOOD: Use sync.Map or mutex
var mu sync.Mutex
m := make(map[string]int)

go func() {
    mu.Lock()
    m["key"] = 1
    mu.Unlock()
}()
```

### 4. nil Slice vs Empty Slice

```go
var s1 []int        // nil slice
s2 := []int{}       // empty slice
s3 := make([]int, 0) // empty slice

len(s1) == 0  // true
len(s2) == 0  // true
s1 == nil     // true
s2 == nil     // false
```

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-10-11 | Initial Go best practices guide |

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

