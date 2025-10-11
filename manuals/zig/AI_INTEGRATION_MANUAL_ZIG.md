# AI Integration Manual - Zig

**Version**: 1.0.0  
**Last Updated**: 2025-10-11  
**Language**: Zig 0.11+ / 0.12 / 0.13  
**Target Audience**: AI Agents (LLM-based development assistants)

---

## Table of Contents

1. [Introduction](#introduction)
2. [Quick Start](#quick-start)
3. [Zig-Specific Setup](#zig-specific-setup)
4. [Configuration Standards](#configuration-standards)
5. [Source Code Standards](#source-code-standards)
6. [Testing Standards](#testing-standards)
7. [Build & Deployment](#build--deployment)
8. [Documentation](#documentation)
9. [Zig Best Practices](#zig-best-practices)
10. [Common Patterns](#common-patterns)
11. [Troubleshooting](#troubleshooting)
12. [Complete Workflow](#complete-workflow)

---

## Introduction

This manual extends the [AI Integration Manual Template](../templates/AI_INTEGRATION_MANUAL_TEMPLATE.md) with Zig-specific implementations.

**When to use this manual**:
- Systems programming projects
- High-performance backend services
- CLI tools and utilities
- Network services
- Embedded systems
- C library replacements
- Cross-platform applications
- Performance-critical libraries

**Prerequisites**:
- Read [AI Integration Manual Template](../templates/AI_INTEGRATION_MANUAL_TEMPLATE.md)
- Read [Language Standards](../LANGUAGE_STANDARDS.md)
- Basic Zig knowledge

**Key Zig Characteristics**:
- No hidden control flow
- No hidden memory allocations
- Manual memory management with allocators
- Explicit error handling
- Compile-time code execution (comptime)
- Built-in build system
- Integrated testing
- Cross-compilation by default
- C interoperability without FFI
- No preprocessor

---

## Quick Start

### Minimum Viable Project

```bash
# 1. Create project directory
mkdir my-project && cd my-project

# 2. Initialize Zig project
zig init-exe  # For executable
# or
zig init-lib  # For library

# 3. Build
zig build

# 4. Run
zig build run

# 5. Test
zig build test
```

### CLI Application Quick Start

```bash
# Create CLI project
mkdir my-cli && cd my-cli
zig init-exe

# Edit src/main.zig with CLI logic

# Build optimized binary
zig build -Doptimize=ReleaseFast

# Binary location: zig-out/bin/my-cli
./zig-out/bin/my-cli --help
```

### Library Quick Start

```bash
# Create library
mkdir my-lib && cd my-lib
zig init-lib

# Edit src/main.zig with library code

# Build library
zig build

# Run tests
zig build test

# Generate documentation
zig build docs
```

---

## Zig-Specific Setup

### 1. Install Zig

**Using Official Downloads**:

```bash
# Download from https://ziglang.org/download/

# Linux/macOS
wget https://ziglang.org/download/0.12.0/zig-linux-x86_64-0.12.0.tar.xz
tar xf zig-linux-x86_64-0.12.0.tar.xz
sudo mv zig-linux-x86_64-0.12.0 /usr/local/zig
export PATH=$PATH:/usr/local/zig

# Verify
zig version
```

**Using Package Managers**:

```bash
# Homebrew (macOS)
brew install zig

# Snap (Linux)
snap install zig --classic --beta

# Scoop (Windows)
scoop install zig

# Verify
zig version
```

**Using Version Managers**:

```bash
# zigup - Zig version manager
# https://github.com/marler8997/zigup

# Install zigup
curl -L https://github.com/marler8997/zigup/releases/latest/download/zigup.linux-x86_64 -o zigup
chmod +x zigup
sudo mv zigup /usr/local/bin/

# Install Zig version
zigup 0.12.0

# Switch versions
zigup 0.11.0
```

### 2. Editor Setup

**VS Code** (Recommended):

```bash
# Install Zig Language extension
code --install-extension ziglang.vscode-zig
```

**Settings** (`.vscode/settings.json`):

```json
{
  "zig.path": "zig",
  "zig.zls.path": "zls",
  "zig.buildOnSave": true,
  "zig.formattingProvider": "zls",
  "[zig]": {
    "editor.formatOnSave": true,
    "editor.defaultFormatter": "ziglang.vscode-zig"
  }
}
```

### 3. Zig Language Server (ZLS)

```bash
# Download from https://github.com/zigtools/zls/releases

# Or build from source
git clone https://github.com/zigtools/zls
cd zls
zig build -Doptimize=ReleaseSafe
sudo cp zig-out/bin/zls /usr/local/bin/

# Verify
zls --version
```

### 4. Essential Tools

```bash
# zig-fmt (built-in formatter)
zig fmt src/

# zig-doc (built-in documentation generator)
zig build docs

# zig-test (built-in test runner)
zig test src/main.zig
```

---

## Configuration Standards

### 1. build.zig (Build Configuration)

**Complete Build Script**:

```zig
const std = @import("std");

pub fn build(b: *std.Build) void {
    // Target and optimization options
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Create executable
    const exe = b.addExecutable(.{
        .name = "my-project",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    // Link system libraries if needed
    exe.linkLibC();
    // exe.linkSystemLibrary("sqlite3");

    // Add module dependencies
    const clap = b.dependency("clap", .{
        .target = target,
        .optimize = optimize,
    });
    exe.root_module.addImport("clap", clap.module("clap"));

    // Install executable
    b.installArtifact(exe);

    // Create run step
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the application");
    run_step.dependOn(&run_cmd.step);

    // Create test step
    const unit_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    const run_unit_tests = b.addRunArtifact(unit_tests);
    
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);

    // Create documentation step
    const docs = b.addTest(.{
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    const install_docs = b.addInstallDirectory(.{
        .source_dir = docs.getEmittedDocs(),
        .install_dir = .prefix,
        .install_subdir = "docs",
    });

    const docs_step = b.step("docs", "Generate documentation");
    docs_step.dependOn(&install_docs.step);

    // Benchmarks
    const bench = b.addExecutable(.{
        .name = "bench",
        .root_source_file = .{ .path = "bench/main.zig" },
        .target = target,
        .optimize = .ReleaseFast,
    });

    const run_bench = b.addRunArtifact(bench);
    const bench_step = b.step("bench", "Run benchmarks");
    bench_step.dependOn(&run_bench.step);
}
```

### 2. build.zig.zon (Dependencies)

**Dependency Configuration**:

```zig
.{
    .name = "my-project",
    .version = "1.0.0",
    .minimum_zig_version = "0.12.0",

    .dependencies = .{
        .clap = .{
            .url = "https://github.com/Hejsil/zig-clap/archive/refs/tags/0.8.0.tar.gz",
            .hash = "122062d301a203d003547b414237229b09a7980095061697349f8bef41be9c30266b",
        },
        .httpz = .{
            .url = "https://github.com/karlseguin/http.zig/archive/refs/tags/v0.1.0.tar.gz",
            .hash = "1220abcd...",
        },
    },

    .paths = .{
        "build.zig",
        "build.zig.zon",
        "src",
        "LICENSE",
        "README.md",
    },
}
```

### 3. .gitignore

```gitignore
# Zig build artifacts
zig-cache/
zig-out/
.zig-cache/

# Dependencies
.gyp/

# IDE
.vscode/
.idea/
*.swp
*.swo
*~

# OS
.DS_Store
Thumbs.db

# Test coverage
*.profdata
*.profraw
```

### 4. .editorconfig

```ini
root = true

[*]
charset = utf-8
end_of_line = lf
insert_final_newline = true
trim_trailing_whitespace = true

[*.zig]
indent_style = space
indent_size = 4
max_line_length = 100

[build.zig]
indent_style = space
indent_size = 4
```

---

## Source Code Standards

### 1. Directory Structure

```
my-project/
├── build.zig              # Build configuration
├── build.zig.zon          # Dependencies
├── src/
│   ├── main.zig           # Entry point
│   ├── lib.zig            # Library root (if lib)
│   ├── server/
│   │   ├── handler.zig
│   │   └── router.zig
│   ├── db/
│   │   ├── connection.zig
│   │   └── query.zig
│   ├── models/
│   │   └── user.zig
│   └── util/
│       ├── allocator.zig
│       └── logger.zig
├── tests/
│   ├── integration.zig
│   └── helpers.zig
├── bench/
│   └── main.zig
├── examples/
│   └── basic.zig
├── docs/
│   ├── ROADMAP.md
│   └── SPECS.md
├── LICENSE
└── README.md
```

### 2. Naming Conventions

| Element | Convention | Example |
|---------|-----------|---------|
| **Files** | snake_case | `http_server.zig` |
| **Directories** | snake_case | `src/http_client/` |
| **Types** | PascalCase | `User`, `HttpServer` |
| **Functions** | camelCase | `createUser()`, `parseJson()` |
| **Constants** | snake_case | `max_connections` |
| **Variables** | snake_case | `user_count` |
| **Global constants** | UPPER_SNAKE_CASE | `MAX_BUFFER_SIZE` |
| **Private** | No special marker | (use `pub` for public) |

### 3. Module Structure (main.zig)

```zig
//! Main application module.
//!
//! This module provides the entry point for the HTTP server application.
//!
//! # Example
//! ```zig
//! const std = @import("std");
//! const app = @import("main.zig");
//!
//! pub fn main() !void {
//!     try app.run();
//! }
//! ```

const std = @import("std");
const builtin = @import("builtin");
const assert = std.debug.assert;
const log = std.log.scoped(.app);

// Public API
pub const User = @import("models/user.zig").User;
pub const Server = @import("server/http.zig").Server;

// Module constants
pub const VERSION = "1.0.0";
pub const MAX_CONNECTIONS = 1000;
pub const DEFAULT_PORT = 8080;

// Type definitions
pub const Error = error{
    InvalidInput,
    ConnectionFailed,
    Timeout,
};

/// Application configuration
pub const Config = struct {
    port: u16 = DEFAULT_PORT,
    host: []const u8 = "127.0.0.1",
    max_connections: usize = MAX_CONNECTIONS,

    pub fn init() Config {
        return .{};
    }

    pub fn validate(self: Config) !void {
        if (self.port == 0) return Error.InvalidInput;
        if (self.max_connections == 0) return Error.InvalidInput;
    }
};

/// Main entry point
pub fn main() !void {
    // Setup allocator
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Parse arguments
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    // Initialize config
    var config = Config.init();
    try config.validate();

    // Run application
    log.info("Starting server on {s}:{d}", .{ config.host, config.port });
    try run(allocator, config);
}

/// Run the application with given config
pub fn run(allocator: std.mem.Allocator, config: Config) !void {
    var server = try Server.init(allocator, config);
    defer server.deinit();

    try server.listen();
}

// Tests
test "config validation" {
    var config = Config.init();
    try config.validate();
}

test "config invalid port" {
    var config = Config{ .port = 0 };
    try std.testing.expectError(Error.InvalidInput, config.validate());
}
```

### 4. Struct with Methods

```zig
/// User model representing a system user
pub const User = struct {
    id: u64,
    name: []const u8,
    email: []const u8,
    age: ?u32,

    const Self = @This();

    /// Creates a new user
    pub fn init(id: u64, name: []const u8, email: []const u8) Self {
        return .{
            .id = id,
            .name = name,
            .email = email,
            .age = null,
        };
    }

    /// Checks if user is an adult
    pub fn isAdult(self: Self) bool {
        if (self.age) |age| {
            return age >= 18;
        }
        return false;
    }

    /// Formats user for display
    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("User#{d}({s}, {s})", .{ self.id, self.name, self.email });
    }

    /// Cleans up user resources
    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        _ = self;
        _ = allocator;
        // Cleanup if needed
    }
};

test "User.isAdult" {
    const user = User{
        .id = 1,
        .name = "John",
        .email = "john@example.com",
        .age = 25,
    };

    try std.testing.expect(user.isAdult());
}
```

### 5. Error Handling

```zig
const std = @import("std");

/// Custom error set
pub const UserError = error{
    InvalidEmail,
    UserNotFound,
    DuplicateEmail,
};

/// Combined error set
pub const Error = UserError || std.mem.Allocator.Error || std.fs.File.OpenError;

/// Function with explicit error handling
pub fn createUser(
    allocator: std.mem.Allocator,
    email: []const u8,
    name: []const u8,
) Error!User {
    // Validate email
    if (!isValidEmail(email)) {
        return UserError.InvalidEmail;
    }

    // Check for duplicates
    if (try checkEmailExists(email)) {
        return UserError.DuplicateEmail;
    }

    // Allocate user
    const user = try allocator.create(User);
    errdefer allocator.destroy(user);

    user.* = User{
        .id = try generateId(),
        .email = try allocator.dupe(u8, email),
        .name = try allocator.dupe(u8, name),
        .age = null,
    };

    return user.*;
}

/// Error handling with catch
pub fn getUserById(id: u64) !User {
    const user = findUser(id) catch |err| switch (err) {
        UserError.UserNotFound => {
            std.log.warn("User {d} not found", .{id});
            return err;
        },
        else => return err,
    };

    return user;
}

/// Using try with explicit error propagation
pub fn processUser(id: u64) !void {
    const user = try getUserById(id);
    defer user.deinit();

    try validateUser(user);
    try saveUser(user);
}
```

### 6. Allocator Patterns

```zig
const std = @import("std");

/// Function accepting allocator
pub fn processData(allocator: std.mem.Allocator, input: []const u8) ![]u8 {
    // Allocate buffer
    var buffer = try allocator.alloc(u8, input.len * 2);
    errdefer allocator.free(buffer);

    // Process data
    var i: usize = 0;
    for (input) |byte| {
        buffer[i] = byte;
        buffer[i + 1] = byte;
        i += 2;
    }

    return buffer;
}

/// Arena allocator for temporary allocations
pub fn buildReport(allocator: std.mem.Allocator) ![]const u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const arena_allocator = arena.allocator();

    // All allocations freed when arena.deinit() is called
    var list = std.ArrayList(u8).init(arena_allocator);
    try list.appendSlice("Report:\n");
    try list.appendSlice("Data: ...\n");

    // Dupe to parent allocator before arena is freed
    return allocator.dupe(u8, list.items);
}

/// Fixed buffer allocator for stack allocations
pub fn formatMessage(message: []const u8) ![]const u8 {
    var buffer: [1024]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    const allocator = fba.allocator();

    return std.fmt.allocPrint(allocator, "Message: {s}", .{message});
}
```

### 7. Comptime Patterns

```zig
const std = @import("std");

/// Comptime generic function
pub fn max(comptime T: type, a: T, b: T) T {
    return if (a > b) a else b;
}

/// Comptime type generation
pub fn ArrayList(comptime T: type) type {
    return struct {
        items: []T,
        capacity: usize,
        allocator: std.mem.Allocator,

        const Self = @This();

        pub fn init(allocator: std.mem.Allocator) Self {
            return .{
                .items = &[_]T{},
                .capacity = 0,
                .allocator = allocator,
            };
        }

        pub fn append(self: *Self, item: T) !void {
            // Implementation
            _ = self;
            _ = item;
        }
    };
}

/// Comptime reflection
pub fn printFields(comptime T: type) void {
    const info = @typeInfo(T);
    inline for (info.Struct.fields) |field| {
        std.debug.print("Field: {s} type: {}\n", .{ field.name, field.type });
    }
}

/// Comptime validation
pub fn validateStruct(comptime T: type) void {
    const info = @typeInfo(T);
    if (info != .Struct) {
        @compileError("Expected struct type");
    }
}
```

---

## Testing Standards

### 1. Test Structure

**Inline Tests** (Recommended):

```zig
const std = @import("std");
const testing = std.testing;

pub fn add(a: i32, b: i32) i32 {
    return a + b;
}

test "add positive numbers" {
    try testing.expectEqual(@as(i32, 5), add(2, 3));
}

test "add negative numbers" {
    try testing.expectEqual(@as(i32, -5), add(-2, -3));
}

test "add mixed numbers" {
    try testing.expectEqual(@as(i32, 1), add(-2, 3));
}
```

**Separate Test Files**:

```zig
// tests/user_test.zig
const std = @import("std");
const testing = std.testing;
const User = @import("../src/models/user.zig").User;

test "User creation" {
    const user = User.init(1, "John", "john@example.com");
    try testing.expectEqual(@as(u64, 1), user.id);
    try testing.expectEqualStrings("John", user.name);
}

test "User.isAdult with age" {
    var user = User.init(1, "John", "john@example.com");
    user.age = 25;
    try testing.expect(user.isAdult());
}

test "User.isAdult without age" {
    const user = User.init(1, "John", "john@example.com");
    try testing.expect(!user.isAdult());
}
```

### 2. Test Assertions

```zig
const testing = @import("std").testing;

test "assertions" {
    // Equality
    try testing.expectEqual(@as(i32, 42), 42);
    
    // String equality
    try testing.expectEqualStrings("hello", "hello");
    
    // Boolean
    try testing.expect(true);
    
    // Error
    try testing.expectError(error.InvalidInput, failingFunction());
    
    // Slice equality
    const a = [_]i32{ 1, 2, 3 };
    const b = [_]i32{ 1, 2, 3 };
    try testing.expectEqualSlices(i32, &a, &b);
    
    // Approximate equality (floats)
    try testing.expectApproxEqRel(@as(f64, 1.0), 1.0001, 0.001);
}
```

### 3. Test with Allocator

```zig
test "function with allocation" {
    const allocator = testing.allocator;
    
    const result = try processData(allocator, "test");
    defer allocator.free(result);
    
    try testing.expectEqualStrings("tteesstt", result);
}
```

### 4. Running Tests

```bash
# Run all tests
zig build test

# Run specific test file
zig test src/models/user.zig

# Run tests with coverage
zig test src/main.zig --test-coverage

# Run tests in debug mode
zig test src/main.zig --test-filter "User"
```

### 5. Coverage Requirements

- **Overall**: > 90%
- **Critical paths**: 100%
- **Error handling**: 100%

---

## Build & Deployment

### 1. Build Modes

```bash
# Debug (default) - Fast compilation, runtime safety checks
zig build

# ReleaseSafe - Optimized, with safety checks
zig build -Doptimize=ReleaseSafe

# ReleaseFast - Maximum performance, no safety checks
zig build -Doptimize=ReleaseFast

# ReleaseSmall - Optimized for size
zig build -Doptimize=ReleaseSmall
```

### 2. Cross-Compilation

```bash
# Linux x86_64
zig build -Dtarget=x86_64-linux

# macOS ARM64
zig build -Dtarget=aarch64-macos

# Windows x86_64
zig build -Dtarget=x86_64-windows

# List all targets
zig targets
```

### 3. Docker Support

**Dockerfile (Multi-stage)**:

```dockerfile
# Build stage
FROM alpine:latest AS builder

# Install Zig
RUN wget https://ziglang.org/download/0.12.0/zig-linux-x86_64-0.12.0.tar.xz && \
    tar -xf zig-linux-x86_64-0.12.0.tar.xz && \
    mv zig-linux-x86_64-0.12.0 /usr/local/zig && \
    ln -s /usr/local/zig/zig /usr/local/bin/zig

WORKDIR /build

# Copy source
COPY . .

# Build release binary
RUN zig build -Doptimize=ReleaseFast

# Production stage
FROM alpine:latest

WORKDIR /app

# Copy binary from build stage
COPY --from=builder /build/zig-out/bin/my-project /app/

# Create non-root user
RUN addgroup -g 1000 appuser && \
    adduser -D -u 1000 -G appuser appuser && \
    chown -R appuser:appuser /app

USER appuser

EXPOSE 8080

ENTRYPOINT ["/app/my-project"]
```

### 4. Static Binary

```bash
# Build fully static binary (no libc)
zig build -Dtarget=x86_64-linux-musl -Doptimize=ReleaseSafe

# Verify
ldd zig-out/bin/my-project  # "not a dynamic executable"
```

---

## Documentation

### 1. Doc Comments

```zig
/// User represents a system user.
///
/// Users have an ID, name, email, and optional age.
///
/// # Example
/// ```zig
/// const user = User.init(1, "John", "john@example.com");
/// std.debug.print("{}\n", .{user});
/// ```
pub const User = struct {
    id: u64,
    name: []const u8,
    email: []const u8,
    age: ?u32,

    /// Creates a new user with the given details.
    ///
    /// # Parameters
    /// - `id`: Unique user identifier
    /// - `name`: User's display name
    /// - `email`: User's email address
    ///
    /// # Returns
    /// A new User instance
    pub fn init(id: u64, name: []const u8, email: []const u8) User {
        return .{
            .id = id,
            .name = name,
            .email = email,
            .age = null,
        };
    }
};
```

### 2. Module Documentation

```zig
//! HTTP server module.
//!
//! This module provides a simple HTTP server implementation
//! with support for routing, middleware, and static files.
//!
//! # Quick Start
//! ```zig
//! const Server = @import("server.zig").Server;
//!
//! pub fn main() !void {
//!     var server = try Server.init(allocator, .{});
//!     defer server.deinit();
//!     try server.listen();
//! }
//! ```
//!
//! # Features
//! - HTTP/1.1 protocol
//! - Route matching
//! - Middleware support
//! - Static file serving
//! - WebSocket support

const std = @import("std");
```

### 3. Generate Documentation

```bash
# Generate docs
zig build docs

# Output: zig-out/docs/index.html

# Serve docs locally
python -m http.server -d zig-out/docs
```

---

## Zig Best Practices

See [BEST_PRACTICES.md](BEST_PRACTICES.md) for complete guide.

### Quick Tips

1. **Use allocators explicitly** - Pass allocators as parameters
2. **Handle all errors** - Use `try` or explicit `catch`
3. **Use `defer` for cleanup** - Ensures resources are freed
4. **Prefer comptime** - Move work to compile-time when possible
5. **No hidden allocations** - All memory allocations are explicit
6. **Use `errdefer`** - Clean up on error paths
7. **Write tests inline** - Tests live next to code
8. **Use `std.testing.allocator`** - Detects memory leaks in tests
9. **Format code** - Run `zig fmt` before commit
10. **Cross-compile** - Test on multiple targets

---

## Common Patterns

### Result Type Pattern

```zig
pub fn Result(comptime T: type) type {
    return union(enum) {
        ok: T,
        err: anyerror,

        pub fn isOk(self: @This()) bool {
            return self == .ok;
        }

        pub fn unwrap(self: @This()) !T {
            return switch (self) {
                .ok => |value| value,
                .err => |err| err,
            };
        }
    };
}
```

---

## Troubleshooting

### Common Issues

**Issue**: Memory leaks detected in tests

**Solution**:
```zig
// Use testing.allocator, it tracks leaks
test "no leaks" {
    const allocator = std.testing.allocator;
    const data = try allocator.alloc(u8, 100);
    defer allocator.free(data);  // Must free!
}
```

**Issue**: Compilation takes too long

**Solution**: Use incremental compilation or reduce comptime usage

---

## Complete Workflow

Follow the [AI Integration Manual Template](../templates/AI_INTEGRATION_MANUAL_TEMPLATE.md) for the complete 6-phase workflow.

---

## Additional Resources

- [Zig Official Documentation](https://ziglang.org/documentation/)
- [Zig Language Reference](https://ziglang.org/documentation/master/)
- [Zig Standard Library](https://ziglang.org/documentation/master/std/)
- [Ziglearn](https://ziglearn.org/)
- [BEST_PRACTICES.md](BEST_PRACTICES.md)

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-10-11 | Initial Zig manual |

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

