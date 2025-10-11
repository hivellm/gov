# Zig Best Practices Guide

**Version**: 1.0.0  
**Last Updated**: 2025-10-11  
**Target Audience**: AI Agents & Developers

---

## Table of Contents

1. [Zig Idioms](#zig-idioms)
2. [Anti-Patterns](#anti-patterns)
3. [Memory Management](#memory-management)
4. [Performance Optimization](#performance-optimization)
5. [Security Best Practices](#security-best-practices)
6. [Error Handling](#error-handling)
7. [Comptime Patterns](#comptime-patterns)
8. [Common Gotchas](#common-gotchas)

---

## Zig Idioms

### 1. Explicit Allocators

**✅ Good**:
```zig
pub fn createUser(allocator: std.mem.Allocator, name: []const u8) !*User {
    const user = try allocator.create(User);
    errdefer allocator.destroy(user);
    
    user.* = .{
        .name = try allocator.dupe(u8, name),
    };
    
    return user;
}
```

**❌ Bad**:
```zig
var global_allocator: std.mem.Allocator = undefined;

pub fn createUser(name: []const u8) !*User {
    // Hidden dependency on global state
    const user = try global_allocator.create(User);
    // ...
}
```

### 2. Use `defer` for Cleanup

**✅ Good**:
```zig
pub fn processFile(path: []const u8) !void {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    
    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);
    
    try process(content);
}
```

**❌ Bad**:
```zig
pub fn processFile(path: []const u8) !void {
    const file = try std.fs.cwd().openFile(path, .{});
    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    
    try process(content);
    
    allocator.free(content);
    file.close();  // Manual cleanup, error-prone
}
```

### 3. Use `errdefer` for Error Cleanup

**✅ Good**:
```zig
pub fn initUser(allocator: std.mem.Allocator) !User {
    const name = try allocator.alloc(u8, 100);
    errdefer allocator.free(name);  // Only runs on error
    
    const email = try allocator.alloc(u8, 100);
    errdefer allocator.free(email);
    
    return User{
        .name = name,
        .email = email,
    };
}
```

**❌ Bad**:
```zig
pub fn initUser(allocator: std.mem.Allocator) !User {
    const name = try allocator.alloc(u8, 100);
    
    const email = allocator.alloc(u8, 100) catch |err| {
        allocator.free(name);  // Manual error cleanup
        return err;
    };
    
    return User{ .name = name, .email = email };
}
```

### 4. Struct Init Syntax

**✅ Good**:
```zig
const user = User{
    .id = 1,
    .name = "John",
    .email = "john@example.com",
    .age = null,
};

// Or with inference
const user: User = .{
    .id = 1,
    .name = "John",
    .email = "john@example.com",
    .age = null,
};
```

### 5. Self Type Pattern

**✅ Good**:
```zig
pub const User = struct {
    id: u64,
    name: []const u8,
    
    const Self = @This();
    
    pub fn init(id: u64, name: []const u8) Self {
        return .{
            .id = id,
            .name = name,
        };
    }
    
    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
    }
};
```

### 6. Optional Types

**✅ Good**:
```zig
pub fn findUser(id: u64) ?User {
    // Return null if not found
    if (id == 0) return null;
    return User{ .id = id, .name = "Test" };
}

// Usage
if (findUser(1)) |user| {
    std.debug.print("Found: {s}\n", .{user.name});
} else {
    std.debug.print("Not found\n", .{});
}

// Or with orelse
const user = findUser(1) orelse return error.NotFound;
```

### 7. Tagged Unions

**✅ Good**:
```zig
pub const Result = union(enum) {
    success: []const u8,
    error_msg: []const u8,
    not_found: void,
    
    pub fn isSuccess(self: Result) bool {
        return self == .success;
    }
};

// Usage
const result = processRequest();
switch (result) {
    .success => |data| std.debug.print("Success: {s}\n", .{data}),
    .error_msg => |msg| std.debug.print("Error: {s}\n", .{msg}),
    .not_found => std.debug.print("Not found\n", .{}),
}
```

### 8. Comptime Generic Functions

**✅ Good**:
```zig
pub fn max(comptime T: type, a: T, b: T) T {
    return if (a > b) a else b;
}

pub fn swap(comptime T: type, a: *T, b: *T) void {
    const temp = a.*;
    a.* = b.*;
    b.* = temp;
}

// Usage
const result = max(i32, 10, 20);
```

### 9. ArrayList Pattern

**✅ Good**:
```zig
pub fn collectItems(allocator: std.mem.Allocator) ![]Item {
    var list = std.ArrayList(Item).init(allocator);
    defer list.deinit();
    
    try list.append(item1);
    try list.append(item2);
    
    return list.toOwnedSlice();  // Transfer ownership
}
```

### 10. Error Unions

**✅ Good**:
```zig
pub const FileError = error{
    NotFound,
    PermissionDenied,
    InvalidFormat,
};

pub fn readConfig(path: []const u8) !Config {
    const file = std.fs.cwd().openFile(path, .{}) catch |err| {
        return switch (err) {
            error.FileNotFound => FileError.NotFound,
            error.AccessDenied => FileError.PermissionDenied,
            else => err,
        };
    };
    defer file.close();
    
    // Parse config...
}
```

---

## Anti-Patterns

### 1. Global State

**❌ Bad**:
```zig
var global_users: std.ArrayList(User) = undefined;

pub fn init() void {
    global_users = std.ArrayList(User).init(std.heap.page_allocator);
}

pub fn addUser(user: User) !void {
    try global_users.append(user);
}
```

**✅ Good**:
```zig
pub const UserManager = struct {
    users: std.ArrayList(User),
    allocator: std.mem.Allocator,
    
    pub fn init(allocator: std.mem.Allocator) UserManager {
        return .{
            .users = std.ArrayList(User).init(allocator),
            .allocator = allocator,
        };
    }
    
    pub fn deinit(self: *UserManager) void {
        self.users.deinit();
    }
    
    pub fn addUser(self: *UserManager, user: User) !void {
        try self.users.append(user);
    }
};
```

### 2. Ignoring Errors

**❌ Bad**:
```zig
pub fn main() void {
    const file = std.fs.cwd().openFile("data.txt", .{}) catch unreachable;
    // If file doesn't exist, program crashes
}
```

**✅ Good**:
```zig
pub fn main() !void {
    const file = try std.fs.cwd().openFile("data.txt", .{});
    defer file.close();
    // Proper error propagation
}
```

### 3. Memory Leaks

**❌ Bad**:
```zig
pub fn createName() ![]u8 {
    const allocator = std.heap.page_allocator;
    const name = try allocator.alloc(u8, 100);
    // Never freed! Memory leak
    return name;
}
```

**✅ Good**:
```zig
pub fn createName(allocator: std.mem.Allocator) ![]u8 {
    const name = try allocator.alloc(u8, 100);
    // Caller is responsible for freeing
    return name;
}

// Or use arena allocator
pub fn buildReport(parent_allocator: std.mem.Allocator) ![]u8 {
    var arena = std.heap.ArenaAllocator.init(parent_allocator);
    defer arena.deinit();  // Frees all allocations
    
    const allocator = arena.allocator();
    // ... multiple allocations ...
    
    // Dupe to parent allocator before arena cleanup
    return parent_allocator.dupe(u8, result);
}
```

### 4. Unnecessary Heap Allocations

**❌ Bad**:
```zig
pub fn formatNumber(allocator: std.mem.Allocator, n: i32) ![]u8 {
    return std.fmt.allocPrint(allocator, "{d}", .{n});
}
```

**✅ Good**:
```zig
pub fn formatNumber(n: i32) ![20]u8 {
    var buffer: [20]u8 = undefined;
    return std.fmt.bufPrint(&buffer, "{d}", .{n});
}

// Or with FixedBufferAllocator
pub fn formatNumber(n: i32) ![]u8 {
    var buffer: [20]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    return std.fmt.allocPrint(fba.allocator(), "{d}", .{n});
}
```

### 5. Implicit Casts

**❌ Bad**:
```zig
const a: i32 = 100;
const b: i64 = a;  // Compilation error in Zig
```

**✅ Good**:
```zig
const a: i32 = 100;
const b: i64 = @intCast(a);  // Explicit cast
```

---

## Memory Management

### 1. Allocator Patterns

**General Purpose Allocator**:
```zig
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const leaked = gpa.deinit();
        if (leaked == .leak) {
            std.log.err("Memory leaked!", .{});
        }
    }
    
    const allocator = gpa.allocator();
    // Use allocator...
}
```

**Arena Allocator**:
```zig
pub fn processRequest(parent_allocator: std.mem.Allocator) !Response {
    var arena = std.heap.ArenaAllocator.init(parent_allocator);
    defer arena.deinit();
    
    const allocator = arena.allocator();
    
    // All temporary allocations freed at once
    const temp1 = try allocator.alloc(u8, 100);
    const temp2 = try allocator.alloc(u8, 200);
    
    // Build response using parent allocator
    return Response{
        .data = try parent_allocator.dupe(u8, result),
    };
}
```

**Fixed Buffer Allocator**:
```zig
pub fn stackOperation() !void {
    var buffer: [4096]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    const allocator = fba.allocator();
    
    // Fast stack-based allocations
    const data = try allocator.alloc(u8, 100);
    // No need to free, buffer is on stack
}
```

### 2. Ownership Rules

**Clear ownership transfer**:
```zig
// Function takes ownership
pub fn takeOwnership(allocator: std.mem.Allocator, data: []u8) void {
    defer allocator.free(data);  // Function frees
    // Use data...
}

// Function borrows
pub fn borrow(data: []const u8) void {
    // Just reads, doesn't free
}

// Function returns ownership
pub fn giveOwnership(allocator: std.mem.Allocator) ![]u8 {
    const data = try allocator.alloc(u8, 100);
    // Caller must free
    return data;
}
```

### 3. Double-Free Prevention

**✅ Good**:
```zig
pub const Buffer = struct {
    data: ?[]u8,
    allocator: std.mem.Allocator,
    
    pub fn deinit(self: *Buffer) void {
        if (self.data) |data| {
            self.allocator.free(data);
            self.data = null;  // Prevent double-free
        }
    }
};
```

---

## Performance Optimization

### 1. Use Comptime

**✅ Good**:
```zig
pub fn pow(comptime base: i32, comptime exp: u32) i32 {
    // Computed at compile time
    return comptime blk: {
        var result: i32 = 1;
        var i: u32 = 0;
        while (i < exp) : (i += 1) {
            result *= base;
        }
        break :blk result;
    };
}

const result = pow(2, 10);  // 1024 computed at compile time
```

### 2. Inline Functions

**✅ Good**:
```zig
inline fn fastAdd(a: i32, b: i32) i32 {
    return a + b;
}

// Or force inline
pub fn hot_path() void {
    @setEvalBranchQuota(10000);
    // Critical performance code
}
```

### 3. Packed Structs

**✅ Good**:
```zig
// Minimize memory usage
const Flags = packed struct {
    read: bool,
    write: bool,
    execute: bool,
    _padding: u5 = 0,
};

// Total size: 1 byte instead of 3+
```

### 4. SIMD Operations

**✅ Good**:
```zig
pub fn addVectors(a: @Vector(4, f32), b: @Vector(4, f32)) @Vector(4, f32) {
    return a + b;  // Uses SIMD instructions
}
```

### 5. Buffer Reuse

**✅ Good**:
```zig
pub const Parser = struct {
    buffer: [4096]u8,
    
    pub fn parse(self: *Parser, input: []const u8) !Result {
        // Reuse buffer instead of allocating
        @memcpy(self.buffer[0..input.len], input);
        // Parse...
    }
};
```

---

## Security Best Practices

### 1. Integer Overflow Protection

**✅ Good**:
```zig
pub fn safeAdd(a: i32, b: i32) !i32 {
    var result: i32 = undefined;
    if (@addWithOverflow(&result, a, b)) {
        return error.Overflow;
    }
    return result;
}

// Or use saturating arithmetic
const result = std.math.saturating_add(a, b);
```

### 2. Bounds Checking

**✅ Good (Debug mode)**:
```zig
pub fn getElement(array: []i32, index: usize) i32 {
    return array[index];  // Panics in Debug if out of bounds
}

// Explicit bounds check
pub fn safeGetElement(array: []i32, index: usize) ?i32 {
    if (index >= array.len) return null;
    return array[index];
}
```

### 3. No Undefined Behavior

**✅ Good**:
```zig
// Zig catches undefined behavior in Debug mode
var x: i32 = undefined;  // OK if initialized before use

if (condition) {
    x = 10;
}
// Using x here causes panic in Debug mode if condition was false
```

### 4. Safe Type Conversions

**✅ Good**:
```zig
const value: i64 = 1000;

// Safe checked cast
const safe: i32 = std.math.cast(i32, value) orelse return error.Overflow;

// Or explicit error handling
const safe: i32 = @intCast(value);  // Runtime panic if overflow in Debug
```

---

## Error Handling

### 1. Error Sets

**✅ Good**:
```zig
pub const FileError = error{
    NotFound,
    PermissionDenied,
    TooLarge,
};

pub const NetworkError = error{
    ConnectionFailed,
    Timeout,
};

// Combined error set
pub const Error = FileError || NetworkError || std.mem.Allocator.Error;

pub fn readRemoteFile(url: []const u8) Error![]u8 {
    // Can return any error from Error set
}
```

### 2. Error Handling Patterns

**Try (propagate)**:
```zig
pub fn outerFunction() !void {
    try innerFunction();  // Propagates error
}
```

**Catch (handle)**:
```zig
pub fn handleError() void {
    const result = mayFail() catch |err| {
        std.log.err("Failed: {}", .{err});
        return;
    };
    // Use result
}
```

**Switch on error**:
```zig
pub fn specificHandling() !void {
    mayFail() catch |err| switch (err) {
        error.NotFound => {
            // Handle not found
            return;
        },
        error.PermissionDenied => {
            // Handle permission
            return error.Unauthorized;
        },
        else => return err,
    };
}
```

### 3. errdefer Pattern

**✅ Good**:
```zig
pub fn multiStep(allocator: std.mem.Allocator) !Resource {
    const step1 = try allocate1(allocator);
    errdefer cleanup1(allocator, step1);
    
    const step2 = try allocate2(allocator);
    errdefer cleanup2(allocator, step2);
    
    const step3 = try allocate3(allocator);
    errdefer cleanup3(allocator, step3);
    
    return Resource{ .s1 = step1, .s2 = step2, .s3 = step3 };
}
```

---

## Comptime Patterns

### 1. Generic Types

**✅ Good**:
```zig
pub fn Result(comptime T: type, comptime E: type) type {
    return union(enum) {
        ok: T,
        err: E,
        
        pub fn isOk(self: @This()) bool {
            return self == .ok;
        }
        
        pub fn unwrap(self: @This()) !T {
            return switch (self) {
                .ok => |val| val,
                .err => |e| e,
            };
        }
    };
}

const IntResult = Result(i32, error{Overflow});
```

### 2. Comptime Validation

**✅ Good**:
```zig
pub fn validateStruct(comptime T: type) void {
    const info = @typeInfo(T);
    if (info != .Struct) {
        @compileError("Expected struct type, got " ++ @typeName(T));
    }
    
    // Validate fields
    inline for (info.Struct.fields) |field| {
        if (field.type == void) {
            @compileError("Field " ++ field.name ++ " cannot be void");
        }
    }
}
```

### 3. Comptime String Manipulation

**✅ Good**:
```zig
pub fn upperCase(comptime str: []const u8) *const [str.len]u8 {
    comptime {
        var result: [str.len]u8 = undefined;
        for (str, 0..) |c, i| {
            result[i] = std.ascii.toUpper(c);
        }
        return &result;
    }
}

const NAME = upperCase("hello");  // "HELLO" at compile time
```

---

## Common Gotchas

### 1. Array vs Slice

```zig
const array: [5]i32 = .{ 1, 2, 3, 4, 5 };  // Fixed size
const slice: []const i32 = &array;         // Reference to array

// Array decay to slice
fn processSlice(data: []const i32) void {
    // ...
}

processSlice(&array);  // Array coerces to slice
```

### 2. Sentinel-Terminated Arrays

```zig
const str: [:0]const u8 = "hello";  // Null-terminated
const regular: []const u8 = str;    // Can convert to regular slice

// C compatibility
extern fn strlen(s: [*:0]const u8) usize;
const len = strlen(str.ptr);
```

### 3. Pointer Types

```zig
var x: i32 = 10;

const single: *i32 = &x;           // Single item pointer
const many: [*]i32 = &x;           // Many-item pointer (no len)
const slice: []i32 = &[_]i32{x};   // Slice (ptr + len)
```

### 4. Undefined vs Null

```zig
var x: i32 = undefined;  // Uninitialized, must set before use
var y: ?i32 = null;      // Optional, explicitly no value
```

### 5. Comptime vs Runtime

```zig
comptime var x = 10;  // Compile-time variable
var y = 10;           // Runtime variable

const z = comptime blk: {
    var sum = 0;
    var i = 0;
    while (i < 10) : (i += 1) {
        sum += i;
    }
    break :blk sum;
};  // z = 45, computed at compile time
```

---

## Additional Resources

- [Zig Language Reference](https://ziglang.org/documentation/master/)
- [Zig Standard Library](https://ziglang.org/documentation/master/std/)
- [Ziglearn](https://ziglearn.org/)
- [Zig by Example](https://zig.guide/)
- [Zig Patterns](https://github.com/ziglings/exercises)

---

**Version**: 1.0.0  
**Maintained by**: HiveLLM Governance Team  
**Last Updated**: 2025-10-11

