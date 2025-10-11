# Lua Best Practices

**Version**: 1.0.0  
**Last Updated**: 2025-10-11  
**Target Audience**: AI Agents developing Lua projects

---

## Table of Contents

1. [Lua Idioms](#lua-idioms)
2. [Anti-Patterns](#anti-patterns)
3. [Performance Optimization](#performance-optimization)
4. [Security Best Practices](#security-best-practices)
5. [Error Handling](#error-handling)
6. [Memory Management](#memory-management)
7. [Metaprogramming](#metaprogramming)
8. [Code Organization](#code-organization)
9. [Testing Best Practices](#testing-best-practices)
10. [Common Gotchas](#common-gotchas)

---

## Lua Idioms

### 1. Use Local Variables

```lua
-- ❌ BAD: Global variables (slow and dangerous)
function calculate()
  total = 0  -- Implicitly global!
  for i = 1, 100 do
    total = total + i
  end
  return total
end

-- ✅ GOOD: Local variables (fast and safe)
local function calculate()
  local total = 0
  for i = 1, 100 do
    total = total + i
  end
  return total
end

-- ✅ BEST: Declare all locals at function start (helps LuaJIT)
local function calculate()
  local total, i
  total = 0
  for i = 1, 100 do
    total = total + i
  end
  return total
end
```

### 2. Proper Module Pattern

```lua
-- ❌ BAD: Polluting global namespace
function myfunction()
  return "hello"
end

CONSTANT = 42

-- ✅ GOOD: Module pattern with local state
local M = {}
local VERSION = "1.0.0"  -- Private constant

function M.public_function()
  return "hello"
end

local function private_helper()
  -- Only accessible within module
end

M.PUBLIC_CONSTANT = 42  -- Public constant

return M
```

### 3. Table as Return Value

```lua
-- ❌ BAD: Multiple return values for complex data
function get_user()
  return "John", 30, "john@example.com"
end
local name, age, email = get_user()

-- ✅ GOOD: Return table for structured data
function get_user()
  return {
    name = "John",
    age = 30,
    email = "john@example.com"
  }
end
local user = get_user()
print(user.name)
```

### 4. Nil-Safe Access

```lua
-- ❌ BAD: Multiple nil checks
local value
if config then
  if config.database then
    if config.database.host then
      value = config.database.host
    end
  end
end

-- ✅ GOOD: Use and operator for nil-safe access
local value = config and config.database and config.database.host

-- ✅ BETTER: With default value
local value = (config and config.database and config.database.host) or "localhost"
```

### 5. Conditional Assignment

```lua
-- ❌ VERBOSE: Traditional if-else
local result
if condition then
  result = value1
else
  result = value2
end

-- ✅ GOOD: Use and/or idiom
local result = condition and value1 or value2

-- ⚠️ WARNING: Doesn't work if value1 is false or nil
local result = condition and false or true  -- Always true!

-- ✅ SAFE: Use ternary function
local function ternary(cond, t, f)
  if cond then return t else return f end
end
local result = ternary(condition, false, true)
```

### 6. Table Construction

```lua
-- ❌ SLOW: Using table.insert for known indices
local t = {}
for i = 1, 1000 do
  table.insert(t, i)  -- Slower
end

-- ✅ FAST: Direct assignment
local t = {}
for i = 1, 1000 do
  t[i] = i
end

-- ✅ GOOD: Literal construction for small tables
local t = {1, 2, 3, 4, 5}

-- ✅ GOOD: Named fields in tables
local person = {
  name = "John",
  age = 30,
  email = "john@example.com"
}
```

---

## Anti-Patterns

### 1. Global Variables

```lua
-- ❌ BAD: Implicit global
function calculate()
  result = 42  -- Global!
end

-- ✅ GOOD: Always use local
local function calculate()
  local result = 42
  return result
end

-- ✅ BETTER: Strict.lua to catch globals
local _G_mt = getmetatable(_G) or {}
_G_mt.__newindex = function(_, key)
  error("attempt to write to undeclared global " .. tostring(key), 2)
end
setmetatable(_G, _G_mt)
```

### 2. Unnecessary Table Traversal

```lua
-- ❌ BAD: Repeated table lookups
for i = 1, 1000000 do
  local result = math.sqrt(math.random())  -- Two table lookups per iteration
end

-- ✅ GOOD: Cache table references
local sqrt, random = math.sqrt, math.random
for i = 1, 1000000 do
  local result = sqrt(random())
end
```

### 3. String Concatenation in Loops

```lua
-- ❌ BAD: Creates many intermediate strings
local result = ""
for i = 1, 10000 do
  result = result .. tostring(i)  -- O(n²) complexity!
end

-- ✅ GOOD: Use table.concat
local parts = {}
for i = 1, 10000 do
  parts[i] = tostring(i)
end
local result = table.concat(parts)

-- ✅ ALTERNATIVE: String buffer pattern
local buffer = {}
local function append(str)
  buffer[#buffer + 1] = str
end
for i = 1, 10000 do
  append(tostring(i))
end
local result = table.concat(buffer)
```

### 4. Misusing Pairs vs IPairs

```lua
local array = {10, 20, 30, 40, 50}

-- ❌ SLOW: pairs() for arrays
for k, v in pairs(array) do
  print(k, v)
end

-- ✅ FAST: ipairs() for arrays
for i, v in ipairs(array) do
  print(i, v)
end

-- ✅ FASTEST: Numeric for loop
for i = 1, #array do
  print(i, array[i])
end

-- ✅ GOOD: pairs() for dictionaries only
local dict = {name = "John", age = 30}
for k, v in pairs(dict) do
  print(k, v)
end
```

### 5. Creating Closures in Loops

```lua
-- ❌ BAD: Creating many closure instances
local callbacks = {}
for i = 1, 1000 do
  callbacks[i] = function()
    return i  -- New closure each time
  end
end

-- ✅ GOOD: Factory function
local function make_callback(value)
  return function()
    return value
  end
end

local callbacks = {}
for i = 1, 1000 do
  callbacks[i] = make_callback(i)
end

-- ✅ BETTER: Avoid closures when possible
local callbacks = {}
for i = 1, 1000 do
  callbacks[i] = i  -- Just store the value
end
```

### 6. Not Reusing Tables

```lua
-- ❌ BAD: Creating new tables repeatedly
function process_data(data)
  local result = {}  -- New table each call
  for i, v in ipairs(data) do
    result[i] = v * 2
  end
  return result
end

-- ✅ GOOD: Reuse table when possible
local result_buffer = {}
function process_data(data)
  -- Clear existing data
  for i = 1, #result_buffer do
    result_buffer[i] = nil
  end
  
  -- Fill with new data
  for i, v in ipairs(data) do
    result_buffer[i] = v * 2
  end
  return result_buffer
end
```

---

## Performance Optimization

### 1. LuaJIT Optimizations

```lua
-- ✅ GOOD: LuaJIT loves consistent types
local function sum_numbers(arr)
  local total = 0  -- Keep as number
  for i = 1, #arr do
    total = total + arr[i]  -- All numbers
  end
  return total
end

-- ❌ BAD: Type instability breaks JIT
local function sum_mixed(arr)
  local total = 0
  for i = 1, #arr do
    if type(arr[i]) == "number" then
      total = total + arr[i]
    else
      total = total .. arr[i]  -- Now total is string!
    end
  end
  return total
end
```

### 2. FFI for Performance-Critical Code

```lua
-- ✅ GOOD: Use FFI for C interop
local ffi = require("ffi")

ffi.cdef[[
  typedef struct { double x, y; } point_t;
  double sqrt(double x);
]]

local function distance(p1, p2)
  local dx = p1.x - p2.x
  local dy = p1.y - p2.y
  return ffi.C.sqrt(dx * dx + dy * dy)
end

-- Create points using FFI (faster)
local point_t = ffi.typeof("point_t")
local p1 = point_t(0, 0)
local p2 = point_t(3, 4)
print(distance(p1, p2))  -- 5.0
```

### 3. Avoid Table Size Changes

```lua
-- ❌ SLOW: Table grows dynamically
local t = {}
for i = 1, 100000 do
  t[i] = i
end

-- ✅ FAST: Pre-allocate table size
local t = {}
for i = 1, 100000 do
  t[i] = 0  -- Pre-fill with dummy value
end
for i = 1, 100000 do
  t[i] = i  -- Now replace values
end

-- ✅ ALTERNATIVE: Use table.new (LuaJIT)
local table_new = require("table.new")
local t = table_new(100000, 0)  -- narr=100000, nrec=0
for i = 1, 100000 do
  t[i] = i
end
```

### 4. Tail Call Optimization

```lua
-- ❌ NOT tail call (uses stack)
local function factorial(n)
  if n <= 1 then
    return 1
  end
  return n * factorial(n - 1)  -- Not a tail call!
end

-- ✅ Tail call optimization
local function factorial(n, acc)
  acc = acc or 1
  if n <= 1 then
    return acc
  end
  return factorial(n - 1, n * acc)  -- Tail call!
end
```

### 5. Memoization

```lua
-- ✅ GOOD: Memoize expensive functions
local function memoize(fn)
  local cache = {}
  return function(...)
    local args = table.concat({...}, "\0")
    if not cache[args] then
      cache[args] = fn(...)
    end
    return cache[args]
  end
end

local function fibonacci(n)
  if n <= 2 then return 1 end
  return fibonacci(n - 1) + fibonacci(n - 2)
end

local fib_memo = memoize(fibonacci)
print(fib_memo(35))  -- Much faster on repeated calls
```

### 6. Loop Unrolling

```lua
-- ❌ SLOW: Simple loop
local function sum_array(arr)
  local total = 0
  for i = 1, #arr do
    total = total + arr[i]
  end
  return total
end

-- ✅ FAST: Unrolled loop (for critical paths)
local function sum_array_fast(arr)
  local total = 0
  local i = 1
  local len = #arr
  
  -- Process 4 elements at a time
  while i <= len - 3 do
    total = total + arr[i] + arr[i+1] + arr[i+2] + arr[i+3]
    i = i + 4
  end
  
  -- Handle remaining elements
  while i <= len do
    total = total + arr[i]
    i = i + 1
  end
  
  return total
end
```

---

## Security Best Practices

### 1. Avoid load() and loadstring()

```lua
-- ❌ DANGEROUS: User input to load()
local user_code = get_user_input()
local fn = load(user_code)  -- Code injection risk!
fn()

-- ✅ SAFE: Don't execute user code
-- If absolutely necessary, use sandbox

local function create_sandbox()
  return {
    print = print,
    math = math,
    string = string,
    -- Only include safe functions
  }
end

local function safe_execute(code)
  local fn, err = load(code)
  if not fn then
    return nil, err
  end
  
  -- Set limited environment
  local sandbox = create_sandbox()
  setfenv(fn, sandbox)  -- Lua 5.1/LuaJIT
  -- or debug.setfenv(fn, sandbox)  -- Lua 5.2+
  
  return pcall(fn)
end
```

### 2. Input Validation

```lua
-- ❌ BAD: No validation
function process_user(data)
  return data.name .. " (" .. data.age .. ")"
end

-- ✅ GOOD: Validate all inputs
function process_user(data)
  if type(data) ~= "table" then
    return nil, "data must be a table"
  end
  
  if type(data.name) ~= "string" or data.name == "" then
    return nil, "name must be a non-empty string"
  end
  
  if type(data.age) ~= "number" or data.age < 0 or data.age > 150 then
    return nil, "age must be a number between 0 and 150"
  end
  
  return data.name .. " (" .. data.age .. ")"
end
```

### 3. SQL Injection Prevention

```lua
-- ❌ VULNERABLE: String concatenation
function get_user(db, username)
  local query = "SELECT * FROM users WHERE name = '" .. username .. "'"
  return db:execute(query)
end

-- ✅ SAFE: Parameterized queries
function get_user(db, username)
  local stmt = db:prepare("SELECT * FROM users WHERE name = ?")
  stmt:bind(1, username)
  return stmt:step()
end

-- ✅ SAFE: Escape user input
function escape_sql(str)
  return (str:gsub("'", "''"))
end

function get_user(db, username)
  username = escape_sql(username)
  local query = "SELECT * FROM users WHERE name = '" .. username .. "'"
  return db:execute(query)
end
```

### 4. Path Traversal Prevention

```lua
-- ❌ VULNERABLE: Direct file access
function read_file(filename)
  local file = io.open(filename, "r")
  if not file then return nil, "file not found" end
  local content = file:read("*a")
  file:close()
  return content
end
-- User could pass: "../../../../etc/passwd"

-- ✅ SAFE: Validate and sanitize paths
function read_file(filename)
  -- Remove path separators
  filename = filename:gsub("[/\\]", "")
  
  -- Allow only specific directory
  local base_dir = "/safe/data/dir/"
  local full_path = base_dir .. filename
  
  -- Verify path is within base_dir
  local real_path = get_real_path(full_path)  -- Resolve symlinks
  if not real_path:find("^" .. base_dir) then
    return nil, "invalid path"
  end
  
  local file = io.open(real_path, "r")
  if not file then return nil, "file not found" end
  local content = file:read("*a")
  file:close()
  return content
end
```

### 5. Secure Random Numbers

```lua
-- ❌ INSECURE: math.random for security
math.randomseed(os.time())
local token = math.random(1000000, 9999999)  -- Predictable!

-- ✅ SECURE: Use proper CSPRNG
local function secure_random_bytes(n)
  -- Linux/Unix
  local f = io.open("/dev/urandom", "rb")
  if f then
    local bytes = f:read(n)
    f:close()
    return bytes
  end
  
  -- Fallback to OpenSSL
  local ssl = require("openssl")
  return ssl.random(n)
end

local function generate_token(length)
  length = length or 32
  local bytes = secure_random_bytes(length)
  -- Convert to hex
  return (bytes:gsub(".", function(c)
    return string.format("%02x", c:byte())
  end))
end
```

---

## Error Handling

### 1. Multiple Return Values Pattern

```lua
-- ✅ GOOD: Lua convention (result, error)
function divide(a, b)
  if b == 0 then
    return nil, "division by zero"
  end
  return a / b
end

-- Usage
local result, err = divide(10, 2)
if not result then
  print("Error:", err)
  return
end
print("Result:", result)
```

### 2. Protected Calls

```lua
-- ✅ GOOD: Use pcall for error handling
local function risky_operation()
  -- Code that might error
  error("something went wrong")
end

local success, result = pcall(risky_operation)
if not success then
  print("Error caught:", result)
end

-- ✅ GOOD: Use xpcall for traceback
local function error_handler(err)
  print("Error:", err)
  print(debug.traceback())
end

local success, result = xpcall(risky_operation, error_handler)
```

### 3. Error Context

```lua
-- ✅ GOOD: Provide meaningful error messages
function read_config(filename)
  local file, err = io.open(filename, "r")
  if not file then
    return nil, string.format("failed to open config file '%s': %s", filename, err)
  end
  
  local content = file:read("*a")
  file:close()
  
  local config, parse_err = parse_json(content)
  if not config then
    return nil, string.format("failed to parse config file '%s': %s", filename, parse_err)
  end
  
  return config
end
```

### 4. Error Levels

```lua
-- ✅ GOOD: Use error levels appropriately
local function validate_param(value, name)
  if not value then
    -- Level 2: Report error at caller
    error(string.format("parameter '%s' is required", name), 2)
  end
end

function public_api(param1, param2)
  validate_param(param1, "param1")  -- Error points to public_api call site
  validate_param(param2, "param2")
  -- ...
end
```

---

## Memory Management

### 1. Avoiding Memory Leaks

```lua
-- ❌ BAD: Strong references prevent collection
local cache = {}
function register(obj)
  cache[obj.id] = obj  -- Keeps obj alive forever
end

-- ✅ GOOD: Use weak tables
local cache = {}
setmetatable(cache, {__mode = "v"})  -- Weak values

function register(obj)
  cache[obj.id] = obj  -- Can be garbage collected
end

-- Weak keys
local weak_keys = {}
setmetatable(weak_keys, {__mode = "k"})

-- Both weak
local weak_both = {}
setmetatable(weak_both, {__mode = "kv"})
```

### 2. Cleaning Up Resources

```lua
-- ✅ GOOD: Explicit cleanup
function process_file(filename)
  local file = io.open(filename, "r")
  if not file then
    return nil, "failed to open file"
  end
  
  -- Ensure file is closed even on error
  local success, result = pcall(function()
    local content = file:read("*a")
    return parse_data(content)
  end)
  
  file:close()
  
  if not success then
    return nil, result  -- result is error message
  end
  return result
end

-- ✅ BETTER: Use do-after pattern
local function with_file(filename, callback)
  local file, err = io.open(filename, "r")
  if not file then
    return nil, err
  end
  
  local success, result = pcall(callback, file)
  file:close()
  
  if not success then
    return nil, result
  end
  return result
end

-- Usage
local data = with_file("config.json", function(file)
  local content = file:read("*a")
  return parse_json(content)
end)
```

### 3. Controlling GC

```lua
-- ✅ GOOD: Manual GC control for performance-critical sections
function bulk_process()
  -- Stop GC during intensive work
  collectgarbage("stop")
  
  for i = 1, 1000000 do
    process_item(i)
  end
  
  -- Resume GC and force collection
  collectgarbage("restart")
  collectgarbage("collect")
end

-- ✅ GOOD: Tune GC for your workload
-- More aggressive GC (default: 200)
collectgarbage("setpause", 100)

-- More frequent GC steps (default: 200)
collectgarbage("setstepmul", 400)
```

---

## Metaprogramming

### 1. Metatables for OOP

```lua
-- ✅ GOOD: Class-like behavior with metatables
local MyClass = {}
MyClass.__index = MyClass

function MyClass.new(name)
  local self = setmetatable({}, MyClass)
  self.name = name
  self.data = {}
  return self
end

function MyClass:add(key, value)
  self.data[key] = value
end

function MyClass:get(key)
  return self.data[key]
end

-- Usage
local obj = MyClass.new("example")
obj:add("key", "value")
print(obj:get("key"))
```

### 2. Operator Overloading

```lua
-- ✅ GOOD: Arithmetic operators
local Vec2 = {}
Vec2.__index = Vec2

function Vec2.new(x, y)
  return setmetatable({x = x, y = y}, Vec2)
end

function Vec2.__add(a, b)
  return Vec2.new(a.x + b.x, a.y + b.y)
end

function Vec2.__sub(a, b)
  return Vec2.new(a.x - b.x, a.y - b.y)
end

function Vec2.__mul(a, scalar)
  return Vec2.new(a.x * scalar, a.y * scalar)
end

function Vec2:__tostring()
  return string.format("Vec2(%f, %f)", self.x, self.y)
end

-- Usage
local v1 = Vec2.new(1, 2)
local v2 = Vec2.new(3, 4)
local v3 = v1 + v2
print(v3)  -- Vec2(4.000000, 6.000000)
```

### 3. Proxy Tables

```lua
-- ✅ GOOD: Lazy loading with __index
local function create_lazy_loader(loader)
  return setmetatable({}, {
    __index = function(t, key)
      local value = loader(key)
      rawset(t, key, value)  -- Cache result
      return value
    end
  })
end

-- Usage
local modules = create_lazy_loader(function(name)
  print("Loading module:", name)
  return require(name)
end)

print(modules.math)  -- Prints "Loading module: math", then math module
print(modules.math)  -- Uses cached value
```

### 4. Read-Only Tables

```lua
-- ✅ GOOD: Make tables read-only
local function readonly(t)
  return setmetatable({}, {
    __index = t,
    __newindex = function(_, key, value)
      error("attempt to modify read-only table", 2)
    end,
    __metatable = false  -- Hide metatable
  })
end

-- Usage
local config = readonly({
  host = "localhost",
  port = 8080
})

print(config.host)  -- OK
config.host = "other"  -- Error!
```

---

## Code Organization

### 1. Module Structure

```lua
-- ✅ GOOD: Standard module structure
local M = {}

-- Module metadata
M.VERSION = "1.0.0"
M.AUTHOR = "Your Name"

-- Dependencies
local json = require("json")
local utils = require("utils")

-- Private state
local _config = {}
local _initialized = false

-- Private functions
local function validate_config(config)
  -- ...
end

-- Public API
function M.init(config)
  if _initialized then
    return false, "already initialized"
  end
  
  local ok, err = validate_config(config)
  if not ok then
    return false, err
  end
  
  _config = config
  _initialized = true
  return true
end

function M.get_config()
  return _config
end

return M
```

### 2. File Organization

```
myproject/
├── init.lua              # Main entry point
├── config.lua            # Configuration
├── core/                 # Core functionality
│   ├── init.lua
│   ├── module1.lua
│   └── module2.lua
├── utils/                # Utilities
│   ├── init.lua
│   ├── string.lua
│   └── table.lua
└── vendor/               # Third-party code
    └── lib.lua
```

---

## Testing Best Practices

### 1. Test Organization

```lua
-- ✅ GOOD: Descriptive test structure
describe("MyModule", function()
  describe("init()", function()
    it("should initialize with valid config", function()
      local ok = MyModule.init({key = "value"})
      assert.is_true(ok)
    end)
    
    it("should fail with invalid config", function()
      local ok, err = MyModule.init(nil)
      assert.is_nil(ok)
      assert.is_not_nil(err)
    end)
  end)
  
  describe("process()", function()
    before_each(function()
      MyModule.init({key = "value"})
    end)
    
    after_each(function()
      MyModule.cleanup()
    end)
    
    it("should process valid data", function()
      local result = MyModule.process("data")
      assert.is_not_nil(result)
    end)
  end)
end)
```

### 2. Using Spies and Stubs

```lua
-- ✅ GOOD: Test interactions with spies
describe("callback functionality", function()
  it("should call callback on success", function()
    local callback = spy.new(function() end)
    
    MyModule.process("data", callback)
    
    assert.spy(callback).was_called()
    assert.spy(callback).was_called_with("success")
  end)
end)

-- ✅ GOOD: Stub dependencies
describe("with stubbed dependency", function()
  local original_http_get
  
  before_each(function()
    original_http_get = http.get
    http.get = stub()
    http.get.returns({status = 200, body = "OK"})
  end)
  
  after_each(function()
    http.get = original_http_get
  end)
  
  it("should use stubbed HTTP client", function()
    local result = MyModule.fetch_data()
    assert.equals("OK", result)
  end)
end)
```

---

## Common Gotchas

### 1. Table Length Operator

```lua
-- ❌ GOTCHA: # operator with holes
local t = {1, 2, nil, 4, 5}
print(#t)  -- Might be 2, 4, or 5 (undefined!)

-- ✅ SOLUTION: Use explicit counter
local function table_length(t)
  local count = 0
  for _ in pairs(t) do
    count = count + 1
  end
  return count
end
```

### 2. Floating Point Comparison

```lua
-- ❌ WRONG: Direct comparison
local a = 0.1 + 0.2
if a == 0.3 then  -- Often false!
  print("equal")
end

-- ✅ CORRECT: Epsilon comparison
local function almost_equal(a, b, epsilon)
  epsilon = epsilon or 1e-10
  return math.abs(a - b) < epsilon
end

if almost_equal(a, 0.3) then
  print("equal")
end
```

### 3. For Loop Variable Scope

```lua
-- ❌ GOTCHA: Loop variable captured by closure
local funcs = {}
for i = 1, 5 do
  funcs[i] = function() return i end  -- All return 5!
end
print(funcs[1]())  -- 5, not 1!

-- ✅ FIX: Create new scope
local funcs = {}
for i = 1, 5 do
  local j = i  -- New variable
  funcs[i] = function() return j end
end
print(funcs[1]())  -- 1
```

### 4. Boolean and Nil

```lua
-- ⚠️ GOTCHA: Only nil and false are falsy
if 0 then print("0 is truthy") end           -- Prints
if "" then print("empty string is truthy") end  -- Prints
if false then else print("false is falsy") end  -- Prints
if nil then else print("nil is falsy") end      -- Prints
```

### 5. Varargs

```lua
-- ❌ WRONG: Losing varargs
local function wrapper(...)
  local args = {...}  -- args is a table now
  return other_function(args)  -- Passes table, not varargs!
end

-- ✅ CORRECT: Preserve varargs
local function wrapper(...)
  return other_function(...)  -- Passes varargs correctly
end

-- ✅ CORRECT: Store varargs safely
local function wrapper(...)
  local n = select("#", ...)  -- Count args (includes nils)
  local args = {...}
  -- Now process args, knowing exact count
end
```

---

## Additional Resources

- [Lua Performance Tips](http://www.lua.org/gems/sample.pdf)
- [LuaJIT Performance Guide](http://wiki.luajit.org/Numerical-Computing-Performance-Guide)
- [Programming in Lua](https://www.lua.org/pil/)
- [Lua Users Wiki](http://lua-users.org/wiki/)

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

