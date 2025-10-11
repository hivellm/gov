# Rust Best Practices

**Version**: 1.0.0  
**Last Updated**: 2025-10-11  
**Target Audience**: AI Agents developing Rust projects

---

## Table of Contents

1. [Rust Idioms](#rust-idioms)
2. [Anti-Patterns](#anti-patterns)
3. [Performance Optimization](#performance-optimization)
4. [Security Best Practices](#security-best-practices)
5. [Error Handling](#error-handling)
6. [Async/Await Patterns](#asyncawait-patterns)
7. [Ownership & Borrowing](#ownership--borrowing)
8. [Code Organization](#code-organization)
9. [Testing Best Practices](#testing-best-practices)
10. [Common Gotchas](#common-gotchas)

---

## Rust Idioms

### 1. Use `Result` Instead of Panicking

```rust
// ❌ BAD: Panic on error
fn divide(a: i32, b: i32) -> i32 {
    if b == 0 {
        panic!("Division by zero");
    }
    a / b
}

// ✅ GOOD: Return Result
fn divide(a: i32, b: i32) -> Result<i32, String> {
    if b == 0 {
        return Err("Division by zero".to_string());
    }
    Ok(a / b)
}

// ✅ BETTER: Custom error type
fn divide(a: i32, b: i32) -> Result<i32, DivisionError> {
    match b {
        0 => Err(DivisionError::DivisionByZero),
        _ => Ok(a / b),
    }
}
```

### 2. Use `Option` for Nullable Values

```rust
// ❌ BAD: Using sentinel values
fn find_user(id: &str) -> User {
    // Returns empty User if not found
}

// ✅ GOOD: Use Option
fn find_user(id: &str) -> Option<User> {
    database.get(id)
}

// Usage
match find_user("123") {
    Some(user) => println!("Found: {}", user.name),
    None => println!("Not found"),
}
```

### 3. Use Iterators

```rust
// ❌ BAD: Imperative loops
let mut result = Vec::new();
for i in 0..10 {
    if i % 2 == 0 {
        result.push(i * 2);
    }
}

// ✅ GOOD: Iterator chains
let result: Vec<_> = (0..10)
    .filter(|i| i % 2 == 0)
    .map(|i| i * 2)
    .collect();
```

### 4. Use `#[derive]` Macros

```rust
// ❌ BAD: Manual implementation
struct Point {
    x: i32,
    y: i32,
}

impl Clone for Point {
    fn clone(&self) -> Self {
        Point { x: self.x, y: self.y }
    }
}

// ✅ GOOD: Derive
#[derive(Debug, Clone, PartialEq, Eq)]
struct Point {
    x: i32,
    y: i32,
}
```

### 5. Prefer `&str` Over `String` for Parameters

```rust
// ❌ BAD: Takes ownership unnecessarily
fn print_name(name: String) {
    println!("{}", name);
}

// ✅ GOOD: Borrows
fn print_name(name: &str) {
    println!("{}", name);
}
```

### 6. Use `impl Trait` for Return Types

```rust
// ❌ BAD: Concrete type
fn get_users() -> Vec<User> {
    vec![]
}

// ✅ GOOD: Iterator trait
fn get_users() -> impl Iterator<Item = User> {
    vec![].into_iter()
}
```

### 7. Use `?` Operator for Error Propagation

```rust
// ❌ BAD: Manual error handling
fn process() -> Result<String, Error> {
    let data = match read_file() {
        Ok(d) => d,
        Err(e) => return Err(e),
    };
    
    let processed = match transform(data) {
        Ok(p) => p,
        Err(e) => return Err(e),
    };
    
    Ok(processed)
}

// ✅ GOOD: ? operator
fn process() -> Result<String, Error> {
    let data = read_file()?;
    let processed = transform(data)?;
    Ok(processed)
}
```

### 8. Use `match` for Pattern Matching

```rust
// ✅ GOOD: Exhaustive pattern matching
let message = match status {
    Status::Pending => "Waiting",
    Status::Approved => "Done",
    Status::Rejected => "Denied",
};

// ✅ GOOD: Match with guards
match value {
    x if x < 0 => "Negative",
    0 => "Zero",
    x if x > 0 => "Positive",
    _ => unreachable!(),
}
```

### 9. Use `Into` and `From` Traits

```rust
#[derive(Debug)]
struct UserId(String);

impl From<String> for UserId {
    fn from(s: String) -> Self {
        UserId(s)
    }
}

// Automatically implements Into<UserId> for String
fn get_user(id: impl Into<UserId>) -> User {
    let user_id = id.into();
    // ...
}

// Usage
get_user("user-123".to_string());
get_user(UserId("user-123".to_string()));
```

### 10. Use Newtype Pattern

```rust
// ✅ GOOD: Type-safe IDs
struct UserId(String);
struct PostId(String);

fn get_user(id: UserId) -> User { /* ... */ }
fn get_post(id: PostId) -> Post { /* ... */ }

// Can't accidentally mix IDs
let user_id = UserId("123".to_string());
let post_id = PostId("456".to_string());

get_user(user_id);  // ✅ OK
get_user(post_id);  // ❌ Compile error
```

---

## Anti-Patterns

### 1. Cloning Everything

```rust
// ❌ BAD: Unnecessary clones
fn process(data: Vec<String>) -> Vec<String> {
    let cloned = data.clone();
    cloned.iter().map(|s| s.to_uppercase()).collect()
}

// ✅ GOOD: Borrow when possible
fn process(data: &[String]) -> Vec<String> {
    data.iter().map(|s| s.to_uppercase()).collect()
}
```

### 2. Using `unwrap()` Everywhere

```rust
// ❌ BAD: Can panic
fn get_config() -> Config {
    let file = std::fs::read_to_string("config.toml").unwrap();
    toml::from_str(&file).unwrap()
}

// ✅ GOOD: Proper error handling
fn get_config() -> Result<Config, ConfigError> {
    let file = std::fs::read_to_string("config.toml")?;
    let config = toml::from_str(&file)?;
    Ok(config)
}
```

### 3. Not Using `?` Operator

```rust
// ❌ BAD: Verbose error handling
fn read_and_parse() -> Result<Data, Error> {
    let content = match std::fs::read_to_string("file.txt") {
        Ok(c) => c,
        Err(e) => return Err(e.into()),
    };
    
    match parse(&content) {
        Ok(d) => Ok(d),
        Err(e) => Err(e.into()),
    }
}

// ✅ GOOD: Use ? operator
fn read_and_parse() -> Result<Data, Error> {
    let content = std::fs::read_to_string("file.txt")?;
    let data = parse(&content)?;
    Ok(data)
}
```

### 4. String vs &str Confusion

```rust
// ❌ BAD: Unnecessary allocations
fn greet(name: String) -> String {
    format!("Hello, {}!", name)
}

// ✅ GOOD: Borrow when possible
fn greet(name: &str) -> String {
    format!("Hello, {}!", name)
}
```

### 5. Not Implementing Error Trait

```rust
// ❌ BAD: Error without Error trait
#[derive(Debug)]
struct MyError(String);

// ✅ GOOD: Use thiserror
use thiserror::Error;

#[derive(Error, Debug)]
#[error("My error: {0}")]
struct MyError(String);
```

---

## Performance Optimization

### 1. Use `&[T]` Instead of `&Vec<T>`

```rust
// ❌ BAD: Tied to Vec
fn process(items: &Vec<String>) -> usize {
    items.len()
}

// ✅ GOOD: Works with any slice
fn process(items: &[String]) -> usize {
    items.len()
}
```

### 2. Use `SmallVec` for Small Collections

```rust
use smallvec::SmallVec;

// Stack-allocated for up to 4 items, heap for more
let mut vec: SmallVec<[u32; 4]> = SmallVec::new();
vec.push(1);
vec.push(2);
```

### 3. Use `Cow` for Copy-on-Write

```rust
use std::borrow::Cow;

fn process(input: &str) -> Cow<str> {
    if input.contains("bad") {
        Cow::Owned(input.replace("bad", "good"))
    } else {
        Cow::Borrowed(input)
    }
}
```

### 4. Avoid Unnecessary Allocations

```rust
// ❌ BAD: Allocates on every call
fn get_default_name() -> String {
    "default".to_string()
}

// ✅ GOOD: Static string
fn get_default_name() -> &'static str {
    "default"
}

// ✅ GOOD: Lazy static for complex values
use once_cell::sync::Lazy;

static DEFAULT_CONFIG: Lazy<Config> = Lazy::new(|| Config::default());
```

### 5. Use Zero-Copy Parsing

```rust
// Use serde with borrowing
#[derive(Deserialize)]
struct Data<'a> {
    #[serde(borrow)]
    name: &'a str,
    #[serde(borrow)]
    tags: Vec<&'a str>,
}
```

---

## Security Best Practices

### 1. Input Validation

```rust
use validator::Validate;

#[derive(Debug, Validate, Deserialize)]
struct CreateUserRequest {
    #[validate(email)]
    email: String,
    
    #[validate(length(min = 2, max = 100))]
    name: String,
    
    #[validate(range(min = 0, max = 150))]
    age: u8,
}

fn create_user(req: CreateUserRequest) -> Result<User> {
    req.validate()?;
    // Process validated data
}
```

### 2. SQL Injection Prevention

```rust
// ❌ BAD: String concatenation
async fn get_user(id: &str) -> Result<User> {
    let query = format!("SELECT * FROM users WHERE id = '{}'", id);
    sqlx::query_as(&query).fetch_one(&pool).await
}

// ✅ GOOD: Parameterized queries
async fn get_user(id: &str) -> Result<User> {
    sqlx::query_as("SELECT * FROM users WHERE id = $1")
        .bind(id)
        .fetch_one(&pool)
        .await
}
```

### 3. Secrets Management

```rust
// ❌ BAD: Hardcoded
const API_KEY: &str = "sk-1234567890";

// ✅ GOOD: From environment
use dotenvy::dotenv;
use std::env;

fn get_api_key() -> Result<String> {
    dotenv().ok();
    env::var("API_KEY").map_err(|_| "API_KEY not set".into())
}
```

### 4. Safe Concurrent Access

```rust
use std::sync::{Arc, Mutex, RwLock};

// ✅ GOOD: Thread-safe shared state
let counter = Arc::new(Mutex::new(0));

// ✅ GOOD: Read-heavy workloads
let cache = Arc::new(RwLock::new(HashMap::new()));
```

---

## Error Handling

### 1. Use thiserror for Library Errors

```rust
use thiserror::Error;

#[derive(Error, Debug)]
pub enum UserError {
    #[error("User not found: {0}")]
    NotFound(String),
    
    #[error("Invalid email: {0}")]
    InvalidEmail(String),
    
    #[error("Database error")]
    Database(#[from] sqlx::Error),
    
    #[error("IO error")]
    Io(#[from] std::io::Error),
}
```

### 2. Use anyhow for Application Errors

```rust
use anyhow::{Context, Result};

fn process_file(path: &str) -> Result<String> {
    let content = std::fs::read_to_string(path)
        .context(format!("Failed to read file: {}", path))?;
    
    let parsed = serde_json::from_str(&content)
        .context("Failed to parse JSON")?;
    
    Ok(parsed)
}
```

### 3. Convert Errors with `From` Trait

```rust
impl From<sqlx::Error> for AppError {
    fn from(err: sqlx::Error) -> Self {
        AppError::Database(err.to_string())
    }
}

// Now can use ? operator
fn get_user(id: &str) -> Result<User, AppError> {
    let user = sqlx::query_as("SELECT * FROM users WHERE id = $1")
        .bind(id)
        .fetch_one(&pool)
        .await?;  // Automatically converts sqlx::Error to AppError
    
    Ok(user)
}
```

---

## Async/Await Patterns

### 1. Use Tokio for Async Runtime

```rust
// Main function with tokio
#[tokio::main]
async fn main() -> Result<()> {
    let result = async_operation().await?;
    Ok(())
}

// Or manually
fn main() {
    let runtime = tokio::runtime::Runtime::new().unwrap();
    runtime.block_on(async {
        async_operation().await.unwrap();
    });
}
```

### 2. Parallel Execution with join!

```rust
use tokio::join;

// ✅ GOOD: Parallel execution
async fn fetch_data() -> (User, Vec<Post>) {
    let (user, posts) = join!(
        fetch_user(),
        fetch_posts()
    );
    
    (user.unwrap(), posts.unwrap())
}
```

### 3. Use try_join! for Fallible Operations

```rust
use tokio::try_join;

async fn fetch_data() -> Result<(User, Vec<Post>)> {
    let (user, posts) = try_join!(
        fetch_user(),
        fetch_posts()
    )?;
    
    Ok((user, posts))
}
```

### 4. Use Select for Racing Futures

```rust
use tokio::select;
use tokio::time::{sleep, Duration};

async fn with_timeout() -> Result<Data> {
    select! {
        result = fetch_data() => result,
        _ = sleep(Duration::from_secs(5)) => {
            Err(anyhow::anyhow!("Timeout"))
        }
    }
}
```

---

## Ownership & Borrowing

### 1. Understand Ownership Rules

```rust
// Rule 1: Each value has one owner
let s1 = String::from("hello");
let s2 = s1;  // s1 is moved, no longer valid

// Rule 2: Multiple immutable borrows OR one mutable borrow
let s = String::from("hello");
let r1 = &s;  // OK
let r2 = &s;  // OK
let r3 = &mut s;  // ❌ Error: can't borrow as mutable

// Rule 3: References must be valid
let reference_to_nothing = {
    let s = String::from("hello");
    &s  // ❌ Error: s dropped here
};
```

### 2. Use References Appropriately

```rust
// ✅ GOOD: Immutable borrow
fn calculate_length(s: &String) -> usize {
    s.len()
}

// ✅ BETTER: Generic over Deref
fn calculate_length(s: &str) -> usize {
    s.len()
}

// ✅ GOOD: Mutable borrow
fn append_text(s: &mut String, text: &str) {
    s.push_str(text);
}
```

### 3. Use Lifetimes When Needed

```rust
// ✅ GOOD: Explicit lifetimes
struct User<'a> {
    name: &'a str,
    email: &'a str,
}

fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
    if x.len() > y.len() { x } else { y }
}
```

---

## Code Organization

### 1. Use Modules Effectively

```rust
// src/lib.rs
pub mod api;
pub mod core;
pub mod service;

// Re-export common types
pub use error::{Error, Result};

// src/api/mod.rs
mod routes;
mod handlers;

pub use routes::configure_routes;
```

### 2. Visibility Modifiers

```rust
pub struct User {
    pub id: String,        // Public field
    pub(crate) email: String,  // Visible within crate
    name: String,          // Private field
}

impl User {
    pub fn new(id: String) -> Self {  // Public method
        Self::internal_new(id)
    }
    
    fn internal_new(id: String) -> Self {  // Private method
        User { id, email: String::new(), name: String::new() }
    }
}
```

### 3. Use Traits for Abstraction

```rust
// Define behavior
pub trait Repository<T> {
    async fn find_by_id(&self, id: &str) -> Result<Option<T>>;
    async fn save(&self, item: T) -> Result<T>;
}

// Implement for specific type
impl Repository<User> for PostgresRepository {
    async fn find_by_id(&self, id: &str) -> Result<Option<User>> {
        // Implementation
    }
}
```

---

## Testing Best Practices

### 1. Unit Tests

```rust
#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_divide_success() {
        assert_eq!(divide(10, 2), Ok(5));
    }
    
    #[test]
    fn test_divide_by_zero() {
        assert!(divide(10, 0).is_err());
    }
    
    #[test]
    #[should_panic(expected = "overflow")]
    fn test_overflow() {
        add_numbers(i32::MAX, 1);
    }
}
```

### 2. Async Tests

```rust
#[cfg(test)]
mod tests {
    use super::*;
    
    #[tokio::test]
    async fn test_async_function() {
        let result = async_operation().await;
        assert!(result.is_ok());
    }
}
```

### 3. Use mockall for Mocking

```rust
use mockall::mock;
use mockall::predicate::*;

mock! {
    pub UserRepository {}
    
    #[async_trait]
    impl UserRepository for UserRepository {
        async fn find_by_id(&self, id: &str) -> Result<Option<User>>;
    }
}

#[tokio::test]
async fn test_with_mock() {
    let mut mock = MockUserRepository::new();
    mock.expect_find_by_id()
        .with(eq("123"))
        .times(1)
        .returning(|_| Ok(Some(User::default())));
    
    let user = mock.find_by_id("123").await.unwrap();
    assert!(user.is_some());
}
```

---

## Common Gotchas

### 1. String vs &str

```rust
// String: Owned, heap-allocated, growable
let owned = String::from("hello");

// &str: Borrowed, reference to string data
let borrowed: &str = "hello";
let slice: &str = &owned[0..2];
```

### 2. Vec vs Slice

```rust
// Vec<T>: Owned, heap-allocated
let owned: Vec<i32> = vec![1, 2, 3];

// &[T]: Borrowed slice
let borrowed: &[i32] = &[1, 2, 3];
let slice: &[i32] = &owned[0..2];
```

### 3. Copy vs Clone

```rust
// Copy: Bitwise copy (stack only)
let x: i32 = 5;
let y = x;  // x still valid (Copy)

// Clone: Deep copy
let s1 = String::from("hello");
let s2 = s1.clone();  // Both valid
```

### 4. Integer Overflow

```rust
// Debug mode: Panics on overflow
// Release mode: Wraps around

// Explicit handling
let result = a.checked_add(b);  // Returns Option
let result = a.saturating_add(b);  // Saturates at max
let result = a.wrapping_add(b);  // Wraps around
```

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-10-11 | Initial Rust best practices guide |

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

