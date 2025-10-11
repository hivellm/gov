# AI Integration Manual - Rust

**Version**: 1.0.0  
**Last Updated**: 2025-10-11  
**Language**: Rust 1.75+ (stable)  
**Target Audience**: AI Agents (LLM-based development assistants)

---

## Table of Contents

1. [Introduction](#introduction)
2. [Quick Start](#quick-start)
3. [Rust-Specific Setup](#rust-specific-setup)
4. [Configuration Standards](#configuration-standards)
5. [Source Code Standards](#source-code-standards)
6. [Testing Standards](#testing-standards)
7. [Build & Deployment](#build--deployment)
8. [Documentation](#documentation)
9. [Rust Best Practices](#rust-best-practices)
10. [Common Patterns](#common-patterns)
11. [Troubleshooting](#troubleshooting)
12. [Complete Workflow](#complete-workflow)

---

## Introduction

This manual extends the [AI Integration Manual Template](../templates/AI_INTEGRATION_MANUAL_TEMPLATE.md) with Rust-specific implementations.

**When to use this manual**:
- Rust applications (systems programming, web services, CLI)
- High-performance APIs (Actix-web, Axum, Rocket)
- WebAssembly projects
- Embedded systems
- CLI tools
- Libraries/SDKs

**Prerequisites**:
- Read [AI Integration Manual Template](../templates/AI_INTEGRATION_MANUAL_TEMPLATE.md)
- Read [Language Standards](../LANGUAGE_STANDARDS.md)
- Basic Rust knowledge

---

## Quick Start

### Minimum Viable Project Setup

```bash
# 1. Create new project with Cargo
cargo new my-project --bin
cd my-project

# 2. Create additional directories
mkdir -p tests benches examples docs

# 3. Build project
cargo build

# 4. Run project
cargo run

# 5. Run tests
cargo test

# 6. Check code quality
cargo clippy
cargo fmt --check
```

### Library Project

```bash
# Create library
cargo new my-lib --lib
cd my-lib

# Build library
cargo build --release

# Run tests with coverage
cargo test

# Generate documentation
cargo doc --open
```

---

## Rust-Specific Setup

### 1. Environment Setup

#### Install Rust (rustup)

```bash
# Install rustup (Unix/Mac/Windows)
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# Windows alternative: Download from https://rustup.rs

# Update PATH (usually done automatically)
source $HOME/.cargo/env

# Verify installation
rustc --version
cargo --version
```

#### Install Rust Toolchain

```bash
# Install stable toolchain (default)
rustup install stable
rustup default stable

# Install nightly (for experimental features)
rustup install nightly

# Update toolchains
rustup update

# Install components
rustup component add rustfmt clippy
```

#### Create rust-toolchain.toml

```toml
[toolchain]
channel = "stable"
components = ["rustfmt", "clippy"]
targets = ["x86_64-unknown-linux-gnu", "x86_64-pc-windows-msvc"]
```

### 2. Essential Tools

```bash
# cargo-watch - Auto-rebuild on file changes
cargo install cargo-watch

# cargo-edit - Add/remove dependencies easily
cargo install cargo-edit

# cargo-audit - Security vulnerability scanning
cargo install cargo-audit

# cargo-tarpaulin - Code coverage (Linux only)
cargo install cargo-tarpaulin

# cargo-nextest - Faster test runner
cargo install cargo-nextest

# cargo-expand - Expand macros
cargo install cargo-expand
```

### 3. Development Workflow

```bash
# Development mode (fast compile, debug info)
cargo run

# Watch mode (auto-rebuild on changes)
cargo watch -x run

# Release mode (optimized)
cargo run --release

# Check without building
cargo check

# Format code
cargo fmt

# Lint code
cargo clippy -- -D warnings

# Run tests
cargo test

# Run specific test
cargo test test_name

# Run tests with output
cargo test -- --nocapture

# Run benchmarks
cargo bench
```

---

## Configuration Standards

### 1. Cargo.toml

**Complete Configuration**:

```toml
[package]
name = "my-project"
version = "1.0.0"
edition = "2021"
rust-version = "1.75"
authors = ["Author Name <author@example.com>"]
description = "Project description"
documentation = "https://docs.rs/my-project"
homepage = "https://github.com/user/my-project"
repository = "https://github.com/user/my-project"
license = "MIT"
keywords = ["rust", "api", "web"]
categories = ["web-programming", "api-bindings"]
readme = "README.md"

[dependencies]
# Async runtime
tokio = { version = "1.35", features = ["full"] }

# Web framework (choose one)
actix-web = "4.4"
# axum = "0.7"
# rocket = "0.5"

# Serialization
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"

# Database
sqlx = { version = "0.7", features = ["runtime-tokio", "postgres", "uuid", "chrono"] }

# Error handling
thiserror = "1.0"
anyhow = "1.0"

# Logging
tracing = "0.1"
tracing-subscriber = "0.3"

# Environment variables
dotenvy = "0.15"

# Validation
validator = { version = "0.16", features = ["derive"] }

# UUID
uuid = { version = "1.6", features = ["v4", "serde"] }

# Date/Time
chrono = { version = "0.4", features = ["serde"] }

[dev-dependencies]
mockall = "0.12"
tokio-test = "0.4"
criterion = "0.5"

[profile.dev]
opt-level = 0
debug = true

[profile.release]
opt-level = 3
lto = true
codegen-units = 1
strip = true

[profile.test]
opt-level = 0
debug = true

[[bin]]
name = "my-project"
path = "src/main.rs"

[[bench]]
name = "benchmarks"
harness = false
```

### 2. rustfmt.toml

**Formatting Configuration**:

```toml
edition = "2021"
max_width = 100
hard_tabs = false
tab_spaces = 4
newline_style = "Unix"
use_small_heuristics = "Default"
reorder_imports = true
reorder_modules = true
remove_nested_parens = true
fn_single_line = false
where_single_line = false
imports_granularity = "Crate"
group_imports = "StdExternalCrate"
```

### 3. clippy.toml

**Linting Configuration**:

```toml
# Clippy configuration
msrv = "1.75"
cognitive-complexity-threshold = 10
```

**In Cargo.toml, add**:

```toml
[lints.clippy]
all = "deny"
pedantic = "warn"
nursery = "warn"
cargo = "warn"

# Allow some pedantic lints
module_name_repetitions = "allow"
missing_errors_doc = "allow"
```

### 4. .cargo/config.toml

**Cargo Configuration**:

```toml
[build]
incremental = true

[term]
color = "always"

[net]
git-fetch-with-cli = true

[alias]
# Custom aliases
b = "build"
c = "check"
t = "test"
r = "run"
f = "fmt"
l = "clippy"
```

### 5. Environment Configuration

**.env.example**:

```env
# Application
RUST_LOG=debug
HOST=0.0.0.0
PORT=8000

# Database
DATABASE_URL=postgresql://user:pass@localhost:5432/dbname
DATABASE_POOL_SIZE=10

# Redis
REDIS_URL=redis://localhost:6379

# External APIs
API_KEY=your-api-key-here

# Task Queue
TASK_QUEUE_URL=http://localhost:8080
TASK_QUEUE_TOKEN=your-token-here

# Vectorizer
VECTORIZER_URL=http://localhost:9000
VECTORIZER_TOKEN=your-token-here
```

**Load config.rs**:

```rust
use dotenvy::dotenv;
use std::env;

#[derive(Debug, Clone)]
pub struct Config {
    pub host: String,
    pub port: u16,
    pub database_url: String,
    pub redis_url: Option<String>,
}

impl Config {
    pub fn from_env() -> Result<Self, anyhow::Error> {
        dotenv().ok();
        
        Ok(Self {
            host: env::var("HOST").unwrap_or_else(|_| "0.0.0.0".to_string()),
            port: env::var("PORT")
                .unwrap_or_else(|_| "8000".to_string())
                .parse()?,
            database_url: env::var("DATABASE_URL")?,
            redis_url: env::var("REDIS_URL").ok(),
        })
    }
}
```

---

## Source Code Standards

### 1. Directory Structure

```
src/
├── main.rs              # Binary entry point
├── lib.rs               # Library root (if library)
├── core/                # Core business logic
│   ├── mod.rs
│   ├── domain/         # Domain entities
│   ├── usecase/        # Use case implementations
│   └── port/           # Traits/interfaces
├── infrastructure/     # External concerns
│   ├── mod.rs
│   ├── database/      # Database implementation
│   └── http/          # HTTP server
├── api/               # API layer
│   ├── mod.rs
│   ├── routes.rs      # Route definitions
│   ├── handlers.rs    # Request handlers
│   └── dto.rs         # Data transfer objects
├── service/           # Application services
│   └── mod.rs
├── repository/        # Data access layer
│   └── mod.rs
├── model/             # Data models
│   └── mod.rs
├── error.rs           # Error types
├── config.rs          # Configuration
└── util/              # Utilities
    └── mod.rs

tests/
├── common/            # Shared test utilities
│   └── mod.rs
├── integration.rs     # Integration tests
└── e2e.rs            # End-to-end tests

benches/
└── benchmarks.rs     # Performance benchmarks

examples/
└── basic_usage.rs    # Usage examples
```

### 2. Naming Conventions

| Element | Convention | Example |
|---------|-----------|---------|
| **Crates** | snake_case | `my_project` |
| **Modules** | snake_case | `user_service` |
| **Files** | snake_case | `user_service.rs` |
| **Structs** | PascalCase | `UserService` |
| **Enums** | PascalCase | `UserRole` |
| **Traits** | PascalCase | `UserRepository` |
| **Functions** | snake_case | `create_user()` |
| **Variables** | snake_case | `user_id` |
| **Constants** | UPPER_SNAKE_CASE | `MAX_RETRY_COUNT` |
| **Type Parameters** | PascalCase (single letter) | `T`, `E`, `Item` |

### 3. Module Organization

**lib.rs**:

```rust
//! My Project
//!
//! This crate provides functionality for...

pub mod api;
pub mod config;
pub mod core;
pub mod error;
pub mod model;
pub mod repository;
pub mod service;

pub use error::{Error, Result};
pub use config::Config;
```

**mod.rs pattern**:

```rust
// src/service/mod.rs
mod user;
mod auth;

pub use user::UserService;
pub use auth::AuthService;
```

### 4. Error Handling with thiserror

```rust
use thiserror::Error;

#[derive(Error, Debug)]
pub enum AppError {
    #[error("User not found: {0}")]
    UserNotFound(String),
    
    #[error("Validation error: {field}: {message}")]
    Validation { field: String, message: String },
    
    #[error("Database error: {0}")]
    Database(#[from] sqlx::Error),
    
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
    
    #[error("Internal server error")]
    Internal,
}

pub type Result<T> = std::result::Result<T, AppError>;
```

### 5. Service Pattern

```rust
use crate::model::User;
use crate::repository::UserRepository;
use crate::error::{AppError, Result};

/// Service for managing users
pub struct UserService {
    repository: Box<dyn UserRepository>,
}

impl UserService {
    pub fn new(repository: Box<dyn UserRepository>) -> Self {
        Self { repository }
    }
    
    /// Creates a new user
    ///
    /// # Arguments
    ///
    /// * `email` - User's email address
    /// * `name` - User's full name
    ///
    /// # Returns
    ///
    /// The created user
    ///
    /// # Errors
    ///
    /// Returns `AppError::Validation` if email is invalid
    /// Returns `AppError::Database` if database operation fails
    pub async fn create_user(&self, email: String, name: String) -> Result<User> {
        // Validate
        if !email.contains('@') {
            return Err(AppError::Validation {
                field: "email".to_string(),
                message: "Invalid email format".to_string(),
            });
        }
        
        // Check if exists
        if self.repository.exists_by_email(&email).await? {
            return Err(AppError::Validation {
                field: "email".to_string(),
                message: "Email already in use".to_string(),
            });
        }
        
        // Create user
        self.repository.create(email, name).await
    }
}
```

### 6. Repository Trait

```rust
use async_trait::async_trait;
use crate::model::User;
use crate::error::Result;

#[async_trait]
pub trait UserRepository: Send + Sync {
    async fn find_by_id(&self, id: &str) -> Result<Option<User>>;
    async fn find_by_email(&self, email: &str) -> Result<Option<User>>;
    async fn exists_by_email(&self, email: &str) -> Result<bool>;
    async fn create(&self, email: String, name: String) -> Result<User>;
    async fn update(&self, id: &str, user: User) -> Result<User>;
    async fn delete(&self, id: &str) -> Result<()>;
}
```

---

## Testing Standards

### 1. Test Structure

```
tests/
├── common/
│   └── mod.rs          # Shared test utilities
├── integration.rs      # Integration tests
└── api_tests.rs        # API tests

src/
├── lib.rs
└── [modules with inline unit tests]
```

### 2. Unit Tests (inline)

```rust
// src/service/user_service.rs

#[cfg(test)]
mod tests {
    use super::*;
    use mockall::predicate::*;
    use mockall::mock;
    
    mock! {
        UserRepo {}
        
        #[async_trait]
        impl UserRepository for UserRepo {
            async fn find_by_id(&self, id: &str) -> Result<Option<User>>;
            async fn exists_by_email(&self, email: &str) -> Result<bool>;
            async fn create(&self, email: String, name: String) -> Result<User>;
        }
    }
    
    #[tokio::test]
    async fn test_create_user_success() {
        // Arrange
        let mut mock_repo = MockUserRepo::new();
        mock_repo
            .expect_exists_by_email()
            .with(eq("test@example.com"))
            .times(1)
            .returning(|_| Ok(false));
        
        mock_repo
            .expect_create()
            .times(1)
            .returning(|email, name| {
                Ok(User {
                    id: "123".to_string(),
                    email,
                    name,
                    created_at: chrono::Utc::now(),
                })
            });
        
        let service = UserService::new(Box::new(mock_repo));
        
        // Act
        let result = service
            .create_user("test@example.com".to_string(), "Test".to_string())
            .await;
        
        // Assert
        assert!(result.is_ok());
        let user = result.unwrap();
        assert_eq!(user.email, "test@example.com");
    }
    
    #[tokio::test]
    async fn test_create_user_invalid_email() {
        // Arrange
        let mock_repo = MockUserRepo::new();
        let service = UserService::new(Box::new(mock_repo));
        
        // Act
        let result = service
            .create_user("invalid".to_string(), "Test".to_string())
            .await;
        
        // Assert
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), AppError::Validation { .. }));
    }
}
```

### 3. Integration Tests

```rust
// tests/integration.rs
use my_project::config::Config;
use my_project::repository::PostgresUserRepository;
use sqlx::PgPool;

#[tokio::test]
async fn test_user_repository() {
    // Setup database
    let pool = PgPool::connect("postgresql://localhost/test_db")
        .await
        .expect("Failed to connect to database");
    
    // Run migrations
    sqlx::migrate!("./migrations")
        .run(&pool)
        .await
        .expect("Failed to run migrations");
    
    let repository = PostgresUserRepository::new(pool.clone());
    
    // Test create
    let user = repository
        .create("test@example.com".to_string(), "Test".to_string())
        .await
        .expect("Failed to create user");
    
    assert_eq!(user.email, "test@example.com");
    
    // Test find
    let found = repository
        .find_by_email("test@example.com")
        .await
        .expect("Failed to find user");
    
    assert!(found.is_some());
    
    // Cleanup
    pool.close().await;
}
```

### 4. Coverage Requirements

- **Overall**: > 90%
- **Unit Tests**: > 95%
- **Integration Tests**: > 85%

**Check coverage**:

```bash
# Linux only
cargo tarpaulin --out Html --output-dir coverage

# Alternative: cargo-llvm-cov (works on all platforms)
cargo install cargo-llvm-cov
cargo llvm-cov --html
```

---

## Build & Deployment

### 1. Build Profiles

```bash
# Debug build (fast compile, slow runtime)
cargo build

# Release build (slow compile, fast runtime)
cargo build --release

# Size-optimized build
cargo build --release --config profile.release.opt-level='"z"'
```

### 2. Cross-Compilation

```bash
# Add target
rustup target add x86_64-unknown-linux-musl

# Build for target
cargo build --release --target x86_64-unknown-linux-musl
```

### 3. Docker Support

**Dockerfile (Multi-stage)**:

```dockerfile
# Build stage
FROM rust:1.75-slim as builder

WORKDIR /app

# Copy manifests
COPY Cargo.toml Cargo.lock ./

# Create dummy main to cache dependencies
RUN mkdir src && \
    echo "fn main() {}" > src/main.rs && \
    cargo build --release && \
    rm -rf src

# Copy actual source
COPY src ./src
COPY migrations ./migrations

# Build application
RUN cargo build --release

# Production stage
FROM debian:bookworm-slim

RUN apt-get update && \
    apt-get install -y ca-certificates && \
    rm -rf /var/lib/apt/lists/*

# Create non-root user
RUN useradd -m -u 1000 appuser

WORKDIR /app

# Copy binary from build stage
COPY --from=builder /app/target/release/my-project /app/my-project

# Change ownership
RUN chown -R appuser:appuser /app
USER appuser

# Expose port
EXPOSE 8000

# Run application
CMD ["./my-project"]
```

### 4. Publishing to crates.io

```bash
# Login to crates.io
cargo login [your-api-token]

# Dry run (verify package)
cargo publish --dry-run

# Publish
cargo publish
```

**Before publishing checklist**:
- [ ] All tests passing
- [ ] Documentation complete
- [ ] Examples provided
- [ ] CHANGELOG updated
- [ ] Version bumped in Cargo.toml
- [ ] Git tag created
- [ ] No private dependencies

---

## Documentation

### 1. Doc Comments

```rust
/// Processes a payment transaction.
///
/// This function handles the complete payment workflow including
/// validation, authorization, and confirmation.
///
/// # Arguments
///
/// * `user_id` - The user's unique identifier
/// * `amount` - Payment amount in cents (must be positive)
/// * `currency` - ISO 4217 currency code (e.g., "USD", "EUR")
///
/// # Returns
///
/// Payment confirmation with transaction details
///
/// # Errors
///
/// * `ValidationError` - If amount is negative or currency is invalid
/// * `InsufficientFundsError` - If user balance is insufficient
/// * `PaymentGatewayError` - If payment gateway is unavailable
///
/// # Examples
///
/// ```
/// # use my_project::process_payment;
/// # async fn run() -> Result<(), Box<dyn std::error::Error>> {
/// let result = process_payment("user-123", 1000, "USD").await?;
/// println!("Transaction ID: {}", result.transaction_id);
/// # Ok(())
/// # }
/// ```
///
/// # Panics
///
/// This function panics if `amount` is negative (in debug mode only)
pub async fn process_payment(
    user_id: &str,
    amount: i64,
    currency: &str,
) -> Result<PaymentResult> {
    // Implementation
}
```

### 2. Module-Level Documentation

```rust
//! User service module.
//!
//! This module provides functionality for managing users including
//! creation, retrieval, update, and deletion operations.
//!
//! # Examples
//!
//! ```
//! use my_project::service::UserService;
//!
//! let service = UserService::new(repository);
//! let user = service.create_user("test@example.com", "Test").await?;
//! ```

pub mod user_service;
pub mod auth_service;
```

### 3. Generate Documentation

```bash
# Generate documentation
cargo doc

# Generate and open in browser
cargo doc --open

# Include private items
cargo doc --document-private-items

# No dependencies docs
cargo doc --no-deps
```

---

## Rust Best Practices

See [BEST_PRACTICES.md](BEST_PRACTICES.md) for complete guide.

### Quick Tips

1. **Use `Result` instead of panics** for recoverable errors
2. **Prefer `&str` over `String`** for function parameters
3. **Use `#[derive]` macros** (Debug, Clone, etc.)
4. **Implement `Display` and `Error`** for custom errors
5. **Use `Option` and `Result`** - avoid null/exceptions
6. **Prefer iterators** over loops
7. **Use `Vec<T>` for owned data**, `&[T]` for borrowed
8. **Use `async/await`** for I/O operations
9. **Follow Rust API Guidelines**
10. **Use `clippy`** and fix all warnings

---

## Common Patterns

Refer to [Source Code Standards](#source-code-standards) and [BEST_PRACTICES.md](BEST_PRACTICES.md) for:
- Repository Pattern (with traits)
- Service Layer Pattern
- Builder Pattern
- Factory Pattern
- Newtype Pattern
- Result/Option handling
- Error conversion with `From` trait

---

## Troubleshooting

### Common Rust Issues

#### Issue: Borrow checker errors

**Solution**: Understand ownership rules, use references, or clone when needed

#### Issue: Lifetime errors

**Solution**: Add explicit lifetime annotations or restructure code

#### Issue: "trait bounds were not satisfied"

**Solution**: Implement required traits or add trait bounds

#### Issue: Slow compile times

**Solutions**:
```bash
# Use sccache for caching
cargo install sccache
export RUSTC_WRAPPER=sccache

# Use incremental compilation (enabled by default)
# Use cargo-check instead of cargo-build during development
cargo check

# Split into smaller crates
```

---

## Complete Workflow

Follow the [AI Integration Manual Template](../templates/AI_INTEGRATION_MANUAL_TEMPLATE.md) for the complete 6-phase workflow, using Rust-specific configurations and commands from this manual.

### Quick Reference

1. **Planning**: Use templates from `gov/manuals/templates/`
2. **Setup**: Follow [Rust-Specific Setup](#rust-specific-setup)
3. **Implementation**: Follow [Source Code Standards](#source-code-standards)
4. **Testing**: Follow [Testing Standards](#testing-standards)
5. **Build**: Follow [Build & Deployment](#build--deployment)
6. **Documentation**: Follow [Documentation](#documentation)

---

## Additional Resources

- [The Rust Programming Language (Book)](https://doc.rust-lang.org/book/)
- [Rust by Example](https://doc.rust-lang.org/rust-by-example/)
- [Rust API Guidelines](https://rust-lang.github.io/api-guidelines/)
- [Actix-web Documentation](https://actix.rs/)
- [Tokio Documentation](https://tokio.rs/)
- [BEST_PRACTICES.md](BEST_PRACTICES.md) - This project's best practices

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-10-11 | Initial Rust manual |

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

