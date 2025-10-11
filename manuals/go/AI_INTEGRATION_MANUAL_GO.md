# AI Integration Manual - Go

**Version**: 1.0.0  
**Last Updated**: 2025-10-11  
**Language**: Go 1.21+ / 1.22  
**Target Audience**: AI Agents (LLM-based development assistants)

---

## Table of Contents

1. [Introduction](#introduction)
2. [Quick Start](#quick-start)
3. [Go-Specific Setup](#go-specific-setup)
4. [Configuration Standards](#configuration-standards)
5. [Source Code Standards](#source-code-standards)
6. [Testing Standards](#testing-standards)
7. [Build & Deployment](#build--deployment)
8. [Documentation](#documentation)
9. [Go Best Practices](#go-best-practices)
10. [Common Patterns](#common-patterns)
11. [Troubleshooting](#troubleshooting)
12. [Complete Workflow](#complete-workflow)

---

## Introduction

This manual extends the [AI Integration Manual Template](../templates/AI_INTEGRATION_MANUAL_TEMPLATE.md) with Go-specific implementations.

**When to use this manual**:
- Go applications (microservices, APIs, CLI)
- REST/gRPC APIs
- Cloud-native applications
- System utilities
- Docker containers
- Libraries/Packages

**Prerequisites**:
- Read [AI Integration Manual Template](../templates/AI_INTEGRATION_MANUAL_TEMPLATE.md)
- Read [Language Standards](../LANGUAGE_STANDARDS.md)
- Basic Go knowledge

---

## Quick Start

### Minimum Viable Project Setup

```bash
# 1. Create project directory
mkdir my-project && cd my-project

# 2. Initialize Go module
go mod init github.com/user/my-project

# 3. Create main.go
cat > main.go << 'EOF'
package main

import "fmt"

func main() {
    fmt.Println("Hello, Go!")
}
EOF

# 4. Run
go run main.go

# 5. Build
go build -o my-project

# 6. Test
go test ./...
```

### Web API Project

```bash
# Create project
mkdir my-api && cd my-api
go mod init github.com/user/my-api

# Install popular web framework
go get -u github.com/gin-gonic/gin
# Or: go get -u github.com/gorilla/mux
# Or: go get -u github.com/gofiber/fiber/v2

# Create main.go with Gin
cat > main.go << 'EOF'
package main

import "github.com/gin-gonic/gin"

func main() {
    r := gin.Default()
    r.GET("/ping", func(c *gin.Context) {
        c.JSON(200, gin.H{
            "message": "pong",
        })
    })
    r.Run(":8080")
}
EOF

# Run
go run main.go
```

---

## Go-Specific Setup

### 1. Environment Setup

#### Install Go

```bash
# Download from: https://go.dev/dl/

# Linux
wget https://go.dev/dl/go1.22.0.linux-amd64.tar.gz
sudo rm -rf /usr/local/go
sudo tar -C /usr/local -xzf go1.22.0.linux-amd64.tar.gz

# Add to PATH
export PATH=$PATH:/usr/local/go/bin

# macOS
brew install go

# Windows
# Download installer from https://go.dev/dl/

# Verify
go version
```

#### Configure Go Environment

```bash
# Set GOPATH (optional, defaults to ~/go)
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin

# Enable Go modules (default since Go 1.16)
export GO111MODULE=on

# Configure private modules
go env -w GOPRIVATE=github.com/yourorg/*
```

### 2. Essential Tools

```bash
# Install common tools
go install golang.org/x/tools/cmd/goimports@latest
go install github.com/golangci/golangci-lint/cmd/golangci-lint@latest
go install github.com/securego/gosec/v2/cmd/gosec@latest
go install github.com/swaggo/swag/cmd/swag@latest
go install gotest.tools/gotestsum@latest

# Air for live reload (development)
go install github.com/cosmtrek/air@latest
```

### 3. Development Workflow

```bash
# Format code
go fmt ./...
goimports -w .

# Vet code
go vet ./...

# Lint code
golangci-lint run

# Run tests
go test ./...

# Run tests with coverage
go test -cover ./...

# Build
go build -o bin/app

# Install dependencies
go mod download
go mod tidy
go mod verify
```

---

## Configuration Standards

### 1. go.mod

**Module Configuration**:

```go
module github.com/user/my-project

go 1.22

require (
    github.com/gin-gonic/gin v1.9.1
    github.com/lib/pq v1.10.9
    github.com/joho/godotenv v1.5.1
    github.com/stretchr/testify v1.8.4
    go.uber.org/zap v1.26.0
)

require (
    // Indirect dependencies
    github.com/bytedance/sonic v1.9.1 // indirect
    github.com/gin-contrib/sse v0.1.0 // indirect
)
```

**go.sum** is automatically generated - commit it to git.

### 2. .golangci.yml

**Linting Configuration**:

```yaml
run:
  timeout: 5m
  tests: true
  modules-download-mode: readonly

linters:
  enable:
    - gofmt
    - goimports
    - govet
    - errcheck
    - staticcheck
    - unused
    - gosimple
    - ineffassign
    - typecheck
    - goconst
    - gocyclo
    - misspell
    - unparam
    - nakedret
    - prealloc
    - exportloopref
    - nolintlint
    - revive
    - gosec
  
  disable:
    - deadcode  # Deprecated
    - varcheck  # Deprecated

linters-settings:
  gocyclo:
    min-complexity: 10
  
  goconst:
    min-len: 3
    min-occurrences: 3
  
  misspell:
    locale: US
  
  nakedret:
    max-func-lines: 30

issues:
  exclude-rules:
    - path: _test\.go
      linters:
        - gocyclo
        - errcheck
        - dupl
        - gosec
```

### 3. .air.toml (Live Reload)

```toml
root = "."
testdata_dir = "testdata"
tmp_dir = "tmp"

[build]
  args_bin = []
  bin = "./tmp/main"
  cmd = "go build -o ./tmp/main ."
  delay = 1000
  exclude_dir = ["assets", "tmp", "vendor", "testdata"]
  exclude_file = []
  exclude_regex = ["_test.go"]
  exclude_unchanged = false
  follow_symlink = false
  full_bin = ""
  include_dir = []
  include_ext = ["go", "tpl", "tmpl", "html"]
  include_file = []
  kill_delay = "0s"
  log = "build-errors.log"
  poll = false
  poll_interval = 0
  rerun = false
  rerun_delay = 500
  send_interrupt = false
  stop_on_error = false

[color]
  app = ""
  build = "yellow"
  main = "magenta"
  runner = "green"
  watcher = "cyan"

[log]
  main_only = false
  time = false

[misc]
  clean_on_exit = false

[screen]
  clear_on_rebuild = false
  keep_scroll = true
```

### 4. Makefile

```makefile
.PHONY: help build test lint fmt clean run

help:
	@echo "Available targets:"
	@echo "  build   - Build the application"
	@echo "  test    - Run tests with coverage"
	@echo "  lint    - Run linters"
	@echo "  fmt     - Format code"
	@echo "  clean   - Clean build artifacts"
	@echo "  run     - Run application"

build:
	@echo "Building..."
	go build -o bin/app .

test:
	@echo "Running tests..."
	go test -v -race -coverprofile=coverage.out ./...
	go tool cover -html=coverage.out -o coverage.html

lint:
	@echo "Linting..."
	golangci-lint run ./...
	go vet ./...

fmt:
	@echo "Formatting..."
	go fmt ./...
	goimports -w .

clean:
	@echo "Cleaning..."
	rm -rf bin/ tmp/ coverage.out coverage.html

run:
	@echo "Running..."
	go run main.go
```

### 5. Environment Configuration

**.env.example**:

```env
# Application
APP_NAME=MyProject
APP_ENV=development
LOG_LEVEL=debug

# Server
HOST=0.0.0.0
PORT=8080

# Database
DATABASE_URL=postgresql://user:pass@localhost:5432/dbname?sslmode=disable
DATABASE_MAX_OPEN_CONNS=25
DATABASE_MAX_IDLE_CONNS=5

# Redis
REDIS_URL=redis://localhost:6379/0

# External APIs
API_KEY=your-api-key-here

# Task Queue
TASK_QUEUE_URL=http://localhost:8080
TASK_QUEUE_TOKEN=

# Vectorizer
VECTORIZER_URL=http://localhost:9000
VECTORIZER_TOKEN=
```

**Load config**:

```go
package config

import (
    "github.com/joho/godotenv"
    "os"
    "strconv"
)

type Config struct {
    AppName    string
    AppEnv     string
    Host       string
    Port       string
    DatabaseURL string
    RedisURL   string
}

func Load() (*Config, error) {
    // Load .env file (optional in production)
    _ = godotenv.Load()
    
    return &Config{
        AppName:    getEnv("APP_NAME", "MyProject"),
        AppEnv:     getEnv("APP_ENV", "development"),
        Host:       getEnv("HOST", "0.0.0.0"),
        Port:       getEnv("PORT", "8080"),
        DatabaseURL: os.Getenv("DATABASE_URL"),
        RedisURL:   getEnv("REDIS_URL", "redis://localhost:6379/0"),
    }, nil
}

func getEnv(key, defaultValue string) string {
    if value := os.Getenv(key); value != "" {
        return value
    }
    return defaultValue
}

func getEnvAsInt(key string, defaultValue int) int {
    if value := os.Getenv(key); value != "" {
        if intVal, err := strconv.Atoi(value); err == nil {
            return intVal
        }
    }
    return defaultValue
}
```

---

## Source Code Standards

### 1. Directory Structure

**Standard Go Project Layout**:

```
my-project/
├── cmd/                    # Main applications
│   └── server/
│       └── main.go        # Entry point
├── internal/              # Private application code
│   ├── config/           # Configuration
│   ├── handler/          # HTTP handlers
│   ├── middleware/       # HTTP middleware
│   ├── model/            # Data models
│   ├── repository/       # Data access
│   ├── service/          # Business logic
│   └── util/             # Utilities
├── pkg/                   # Public libraries
│   └── api/              # Public API
├── api/                   # API definitions (OpenAPI, protobuf)
├── test/                  # Additional test files
├── scripts/               # Build scripts
├── deployments/           # Docker, k8s configs
├── go.mod
├── go.sum
├── Makefile
├── README.md
└── .golangci.yml
```

### 2. Naming Conventions

| Element | Convention | Example |
|---------|-----------|---------|
| **Packages** | lowercase (single word) | `user`, `userservice` |
| **Files** | snake_case | `user_service.go` |
| **Structs** | PascalCase | `UserService` |
| **Interfaces** | PascalCase (-er suffix) | `UserRepository`, `Reader` |
| **Functions** | PascalCase (exported) | `CreateUser()` |
| **Functions** | camelCase (unexported) | `validateEmail()` |
| **Variables** | camelCase | `userId` |
| **Constants** | PascalCase or camelCase | `MaxRetryCount` or `maxRetryCount` |

### 3. Package Organization

```go
// internal/model/user.go
package model

import "time"

// User represents a user in the system
type User struct {
    ID        string    `json:"id" db:"id"`
    Email     string    `json:"email" db:"email"`
    Name      string    `json:"name" db:"name"`
    Age       *int      `json:"age,omitempty" db:"age"`
    CreatedAt time.Time `json:"created_at" db:"created_at"`
    UpdatedAt time.Time `json:"updated_at" db:"updated_at"`
}

// CreateUserRequest represents user creation data
type CreateUserRequest struct {
    Email string `json:"email" binding:"required,email"`
    Name  string `json:"name" binding:"required,min=2,max=100"`
    Age   *int   `json:"age" binding:"omitempty,min=0,max=150"`
}
```

### 4. Repository Pattern

```go
// internal/repository/user_repository.go
package repository

import (
    "context"
    "database/sql"
    "github.com/user/my-project/internal/model"
)

// UserRepository defines user data access operations
type UserRepository interface {
    FindByID(ctx context.Context, id string) (*model.User, error)
    FindByEmail(ctx context.Context, email string) (*model.User, error)
    Create(ctx context.Context, user *model.User) error
    Update(ctx context.Context, user *model.User) error
    Delete(ctx context.Context, id string) error
}

// PostgresUserRepository implements UserRepository for PostgreSQL
type PostgresUserRepository struct {
    db *sql.DB
}

// NewPostgresUserRepository creates a new repository instance
func NewPostgresUserRepository(db *sql.DB) *PostgresUserRepository {
    return &PostgresUserRepository{db: db}
}

// FindByID retrieves a user by ID
func (r *PostgresUserRepository) FindByID(ctx context.Context, id string) (*model.User, error) {
    var user model.User
    
    query := `SELECT id, email, name, age, created_at, updated_at 
              FROM users WHERE id = $1`
    
    err := r.db.QueryRowContext(ctx, query, id).Scan(
        &user.ID,
        &user.Email,
        &user.Name,
        &user.Age,
        &user.CreatedAt,
        &user.UpdatedAt,
    )
    
    if err == sql.ErrNoRows {
        return nil, ErrNotFound
    }
    
    if err != nil {
        return nil, err
    }
    
    return &user, nil
}

// Create inserts a new user
func (r *PostgresUserRepository) Create(ctx context.Context, user *model.User) error {
    query := `INSERT INTO users (id, email, name, age, created_at, updated_at)
              VALUES ($1, $2, $3, $4, $5, $6)`
    
    _, err := r.db.ExecContext(
        ctx,
        query,
        user.ID,
        user.Email,
        user.Name,
        user.Age,
        user.CreatedAt,
        user.UpdatedAt,
    )
    
    return err
}
```

### 5. Service Layer

```go
// internal/service/user_service.go
package service

import (
    "context"
    "errors"
    "github.com/google/uuid"
    "github.com/user/my-project/internal/model"
    "github.com/user/my-project/internal/repository"
    "log/slog"
    "time"
)

// UserService handles user business logic
type UserService struct {
    repo   repository.UserRepository
    logger *slog.Logger
}

// NewUserService creates a new UserService
func NewUserService(repo repository.UserRepository, logger *slog.Logger) *UserService {
    return &UserService{
        repo:   repo,
        logger: logger,
    }
}

// CreateUser creates a new user
func (s *UserService) CreateUser(ctx context.Context, req model.CreateUserRequest) (*model.User, error) {
    s.logger.Debug("creating user", "email", req.Email)
    
    // Check if email exists
    existing, err := s.repo.FindByEmail(ctx, req.Email)
    if err != nil && !errors.Is(err, repository.ErrNotFound) {
        return nil, err
    }
    
    if existing != nil {
        return nil, ErrEmailAlreadyExists
    }
    
    // Create user
    user := &model.User{
        ID:        uuid.New().String(),
        Email:     req.Email,
        Name:      req.Name,
        Age:       req.Age,
        CreatedAt: time.Now().UTC(),
        UpdatedAt: time.Now().UTC(),
    }
    
    if err := s.repo.Create(ctx, user); err != nil {
        return nil, err
    }
    
    s.logger.Info("user created", "user_id", user.ID)
    
    return user, nil
}

// Custom errors
var (
    ErrEmailAlreadyExists = errors.New("email already exists")
    ErrUserNotFound       = errors.New("user not found")
)
```

### 6. HTTP Handler (Gin)

```go
// internal/handler/user_handler.go
package handler

import (
    "net/http"
    "github.com/gin-gonic/gin"
    "github.com/user/my-project/internal/model"
    "github.com/user/my-project/internal/service"
)

// UserHandler handles user HTTP requests
type UserHandler struct {
    userService *service.UserService
}

// NewUserHandler creates a new UserHandler
func NewUserHandler(userService *service.UserService) *UserHandler {
    return &UserHandler{userService: userService}
}

// CreateUser godoc
// @Summary Create a new user
// @Description Create a new user with email and name
// @Tags users
// @Accept json
// @Produce json
// @Param user body model.CreateUserRequest true "User data"
// @Success 201 {object} model.User
// @Failure 400 {object} ErrorResponse
// @Failure 409 {object} ErrorResponse
// @Router /users [post]
func (h *UserHandler) CreateUser(c *gin.Context) {
    var req model.CreateUserRequest
    
    if err := c.ShouldBindJSON(&req); err != nil {
        c.JSON(http.StatusBadRequest, ErrorResponse{
            Error:   "VALIDATION_ERROR",
            Message: err.Error(),
        })
        return
    }
    
    user, err := h.userService.CreateUser(c.Request.Context(), req)
    if err != nil {
        if errors.Is(err, service.ErrEmailAlreadyExists) {
            c.JSON(http.StatusConflict, ErrorResponse{
                Error:   "CONFLICT",
                Message: err.Error(),
            })
            return
        }
        
        c.JSON(http.StatusInternalServerError, ErrorResponse{
            Error:   "INTERNAL_ERROR",
            Message: "An unexpected error occurred",
        })
        return
    }
    
    c.JSON(http.StatusCreated, user)
}

// GetUser godoc
// @Summary Get user by ID
// @Description Get a user by their ID
// @Tags users
// @Produce json
// @Param id path string true "User ID"
// @Success 200 {object} model.User
// @Failure 404 {object} ErrorResponse
// @Router /users/{id} [get]
func (h *UserHandler) GetUser(c *gin.Context) {
    id := c.Param("id")
    
    user, err := h.userService.GetUserByID(c.Request.Context(), id)
    if err != nil {
        if errors.Is(err, service.ErrUserNotFound) {
            c.JSON(http.StatusNotFound, ErrorResponse{
                Error:   "NOT_FOUND",
                Message: err.Error(),
            })
            return
        }
        
        c.JSON(http.StatusInternalServerError, ErrorResponse{
            Error:   "INTERNAL_ERROR",
            Message: "An unexpected error occurred",
        })
        return
    }
    
    c.JSON(http.StatusOK, user)
}

// ErrorResponse represents an error response
type ErrorResponse struct {
    Error   string `json:"error"`
    Message string `json:"message"`
}
```

### 7. Error Handling

```go
// internal/errors/errors.go
package errors

import "fmt"

// AppError represents an application error
type AppError struct {
    Code       string
    Message    string
    StatusCode int
    Err        error
}

// Error implements error interface
func (e *AppError) Error() string {
    if e.Err != nil {
        return fmt.Sprintf("%s: %v", e.Message, e.Err)
    }
    return e.Message
}

// Unwrap returns the underlying error
func (e *AppError) Unwrap() error {
    return e.Err
}

// NewNotFoundError creates a not found error
func NewNotFoundError(resource, id string) *AppError {
    return &AppError{
        Code:       "NOT_FOUND",
        Message:    fmt.Sprintf("%s with id %s not found", resource, id),
        StatusCode: 404,
    }
}

// NewConflictError creates a conflict error
func NewConflictError(message string) *AppError {
    return &AppError{
        Code:       "CONFLICT",
        Message:    message,
        StatusCode: 409,
    }
}
```

---

## Testing Standards

### 1. Test Structure

```
internal/
├── service/
│   ├── user_service.go
│   └── user_service_test.go       # Unit tests alongside code
├── handler/
│   ├── user_handler.go
│   └── user_handler_test.go
└── repository/
    ├── user_repository.go
    └── user_repository_test.go

test/                                # Integration/e2e tests
├── integration/
│   └── api_test.go
└── testutil/
    └── helpers.go
```

### 2. Unit Test Example

```go
// internal/service/user_service_test.go
package service_test

import (
    "context"
    "testing"
    "github.com/stretchr/testify/assert"
    "github.com/stretchr/testify/mock"
    "github.com/user/my-project/internal/model"
    "github.com/user/my-project/internal/service"
)

// MockUserRepository is a mock repository
type MockUserRepository struct {
    mock.Mock
}

func (m *MockUserRepository) FindByEmail(ctx context.Context, email string) (*model.User, error) {
    args := m.Called(ctx, email)
    if args.Get(0) == nil {
        return nil, args.Error(1)
    }
    return args.Get(0).(*model.User), args.Error(1)
}

func (m *MockUserRepository) Create(ctx context.Context, user *model.User) error {
    args := m.Called(ctx, user)
    return args.Error(0)
}

func TestUserService_CreateUser_Success(t *testing.T) {
    // Arrange
    mockRepo := new(MockUserRepository)
    logger := slog.Default()
    svc := service.NewUserService(mockRepo, logger)
    
    req := model.CreateUserRequest{
        Email: "test@example.com",
        Name:  "Test User",
    }
    
    mockRepo.On("FindByEmail", mock.Anything, req.Email).Return(nil, repository.ErrNotFound)
    mockRepo.On("Create", mock.Anything, mock.AnythingOfType("*model.User")).Return(nil)
    
    // Act
    user, err := svc.CreateUser(context.Background(), req)
    
    // Assert
    assert.NoError(t, err)
    assert.NotNil(t, user)
    assert.Equal(t, req.Email, user.Email)
    assert.Equal(t, req.Name, user.Name)
    
    mockRepo.AssertExpectations(t)
}

func TestUserService_CreateUser_EmailExists(t *testing.T) {
    // Arrange
    mockRepo := new(MockUserRepository)
    logger := slog.Default()
    svc := service.NewUserService(mockRepo, logger)
    
    req := model.CreateUserRequest{
        Email: "existing@example.com",
        Name:  "Test",
    }
    
    existingUser := &model.User{Email: req.Email}
    mockRepo.On("FindByEmail", mock.Anything, req.Email).Return(existingUser, nil)
    
    // Act
    user, err := svc.CreateUser(context.Background(), req)
    
    // Assert
    assert.Error(t, err)
    assert.Nil(t, user)
    assert.ErrorIs(t, err, service.ErrEmailAlreadyExists)
    
    mockRepo.AssertExpectations(t)
    mockRepo.AssertNotCalled(t, "Create")
}
```

### 3. Table-Driven Tests

```go
func TestValidateEmail(t *testing.T) {
    tests := []struct {
        name    string
        email   string
        wantErr bool
    }{
        {
            name:    "valid email",
            email:   "test@example.com",
            wantErr: false,
        },
        {
            name:    "invalid email",
            email:   "invalid",
            wantErr: true,
        },
        {
            name:    "empty email",
            email:   "",
            wantErr: true,
        },
    }
    
    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            err := ValidateEmail(tt.email)
            if tt.wantErr {
                assert.Error(t, err)
            } else {
                assert.NoError(t, err)
            }
        })
    }
}
```

### 4. Coverage Requirements

- **Overall**: > 90%
- **Unit Tests**: > 95%
- **Integration Tests**: > 85%

**Check coverage**:

```bash
# Run tests with coverage
go test -cover ./...

# Generate detailed coverage
go test -coverprofile=coverage.out ./...
go tool cover -html=coverage.out

# With gotestsum (better output)
gotestsum --format testname -- -cover ./...
```

---

## Build & Deployment

### 1. Build Process

```bash
# Simple build
go build -o bin/app

# Build with flags
go build -ldflags="-s -w" -o bin/app

# Build for specific OS/arch
GOOS=linux GOARCH=amd64 go build -o bin/app-linux

# Cross-compilation examples
GOOS=windows GOARCH=amd64 go build -o bin/app.exe
GOOS=darwin GOARCH=arm64 go build -o bin/app-mac-arm64
```

### 2. Docker Support

**Dockerfile (Multi-stage)**:

```dockerfile
# Build stage
FROM golang:1.22-alpine AS builder

WORKDIR /app

# Install dependencies
RUN apk add --no-cache git

# Copy go mod files
COPY go.mod go.sum ./
RUN go mod download

# Copy source
COPY . .

# Build
RUN CGO_ENABLED=0 GOOS=linux go build -a -installsuffix cgo -ldflags="-s -w" -o app ./cmd/server

# Production stage
FROM alpine:latest

RUN apk --no-cache add ca-certificates

WORKDIR /root/

# Copy binary from builder
COPY --from=builder /app/app .

# Create non-root user
RUN adduser -D -u 1000 appuser
USER appuser

# Expose port
EXPOSE 8080

# Run
CMD ["./app"]
```

### 3. Module Publishing

**Before publishing**:

```bash
# Ensure go.mod is clean
go mod tidy

# Run all checks
go test ./...
golangci-lint run
go vet ./...

# Tag version
git tag v1.0.0
git push origin v1.0.0

# Go automatically discovers tagged versions
# Users can: go get github.com/user/my-project@v1.0.0
```

---

## Documentation

### 1. Package Documentation

```go
// Package user provides user management functionality.
//
// This package includes services, repositories, and handlers for
// managing user data in the application.
//
// Example usage:
//
//	repo := repository.NewPostgresUserRepository(db)
//	service := service.NewUserService(repo, logger)
//	user, err := service.CreateUser(ctx, req)
package user
```

### 2. Function Documentation

```go
// CreateUser creates a new user in the system.
//
// The function validates the input, checks for duplicate emails,
// and persists the user to the database.
//
// Parameters:
//   - ctx: Context for cancellation and timeout
//   - req: User creation request data
//
// Returns:
//   - *model.User: The created user
//   - error: Error if creation fails
//
// Errors:
//   - ErrEmailAlreadyExists: If email is already in use
//   - ErrInvalidInput: If input validation fails
//
// Example:
//
//	req := model.CreateUserRequest{
//	    Email: "user@example.com",
//	    Name:  "John Doe",
//	}
//	user, err := service.CreateUser(ctx, req)
//	if err != nil {
//	    log.Fatal(err)
//	}
func (s *UserService) CreateUser(ctx context.Context, req model.CreateUserRequest) (*model.User, error) {
    // Implementation
}
```

### 3. Generate Documentation

```bash
# View documentation locally
godoc -http=:6060

# Then visit: http://localhost:6060/pkg/github.com/user/my-project/

# Or use pkg.go.dev (automatic after publishing)
# https://pkg.go.dev/github.com/user/my-project
```

---

## Go Best Practices

See [BEST_PRACTICES.md](BEST_PRACTICES.md) for complete guide.

### Quick Tips

1. **Use `gofmt` and `goimports`** - always format code
2. **Accept interfaces, return structs**
3. **Use context.Context** for cancellation
4. **Handle errors explicitly** - don't ignore them
5. **Prefer composition over inheritance**
6. **Use defer for cleanup**
7. **Make zero value useful**
8. **Use goroutines and channels** for concurrency
9. **Follow Effective Go** guidelines
10. **Keep it simple** - don't over-engineer

---

## Common Patterns

Refer to [Source Code Standards](#source-code-standards) and [BEST_PRACTICES.md](BEST_PRACTICES.md) for:
- Repository Pattern
- Service Layer Pattern
- Dependency Injection (manual/wire)
- Factory Functions
- Functional Options Pattern
- Worker Pool Pattern
- Pipeline Pattern

---

## Troubleshooting

### Common Go Issues

#### Issue: Package not found

**Solution**:
```bash
go mod tidy
go mod download
```

#### Issue: Import cycle

**Solution**: Refactor to break circular dependency, extract interface

#### Issue: Race condition detected

**Solution**:
```bash
go test -race ./...
# Fix by using sync.Mutex or channels
```

#### Issue: Too many open files

**Solution**: Close resources with defer, increase limits

---

## Complete Workflow

Follow the [AI Integration Manual Template](../templates/AI_INTEGRATION_MANUAL_TEMPLATE.md) for the complete 6-phase workflow, using Go-specific configurations and commands from this manual.

### Quick Reference

1. **Planning**: Use templates from `gov/manuals/templates/`
2. **Setup**: Follow [Go-Specific Setup](#go-specific-setup)
3. **Implementation**: Follow [Source Code Standards](#source-code-standards)
4. **Testing**: Follow [Testing Standards](#testing-standards)
5. **Build**: Follow [Build & Deployment](#build--deployment)
6. **Documentation**: Follow [Documentation](#documentation)

---

## Additional Resources

- [Effective Go](https://go.dev/doc/effective_go)
- [Go Code Review Comments](https://github.com/golang/go/wiki/CodeReviewComments)
- [Standard Go Project Layout](https://github.com/golang-standards/project-layout)
- [Gin Framework](https://gin-gonic.com/docs/)
- [Go by Example](https://gobyexample.com/)
- [BEST_PRACTICES.md](BEST_PRACTICES.md) - This project's best practices

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-10-11 | Initial Go manual |

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

