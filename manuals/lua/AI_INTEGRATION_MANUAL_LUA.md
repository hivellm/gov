# AI Integration Manual - Lua

**Version**: 1.0.0  
**Last Updated**: 2025-10-11  
**Language**: Lua 5.4 / LuaJIT 2.1  
**Target Audience**: AI Agents (LLM-based development assistants)

---

## Table of Contents

1. [Introduction](#introduction)
2. [Quick Start](#quick-start)
3. [Lua-Specific Setup](#lua-specific-setup)
4. [Configuration Standards](#configuration-standards)
5. [Source Code Standards](#source-code-standards)
6. [Testing Standards](#testing-standards)
7. [Build & Deployment](#build--deployment)
8. [Documentation](#documentation)
9. [Lua Best Practices](#lua-best-practices)
10. [Common Patterns](#common-patterns)
11. [Troubleshooting](#troubleshooting)
12. [Complete Workflow](#complete-workflow)

---

## Introduction

This manual extends the [AI Integration Manual Template](../AI_INTEGRATION_MANUAL_TEMPLATE.md) with Lua-specific implementations.

**When to use this manual**:
- Game development scripts (Love2D, Corona, Defold)
- Embedded scripting (Neovim plugins, Redis scripts)
- Web applications (OpenResty, Lapis)
- Configuration and automation scripts
- API development (Nginx/OpenResty backends)
- Data processing pipelines
- IoT and embedded systems

**Prerequisites**:
- Read [AI Integration Manual Template](../AI_INTEGRATION_MANUAL_TEMPLATE.md)
- Read [Language Standards](../LANGUAGE_STANDARDS.md)
- Basic Lua knowledge

---

## Quick Start

### Minimum Viable Project Setup

```bash
# 1. Create project directory
mkdir my-project && cd my-project

# 2. Create directory structure
mkdir -p src lib tests docs spec

# 3. Create basic files
touch src/main.lua
touch README.md
touch rockspec

# 4. Initialize rockspec (Lua package manager)
luarocks write_rockspec

# 5. Create .luacheckrc (linter config)
cat > .luacheckrc << 'EOF'
std = "lua54+luajit"
max_line_length = 100
ignore = {"212/self"}  -- Unused argument self

globals = {
    "love",     -- Love2D
    "ngx",      -- OpenResty
    "vim",      -- Neovim
}
EOF

# 6. Run project
lua src/main.lua
```

### Library Project

```bash
# Create library
mkdir my-lib && cd my-lib
mkdir -p lib tests docs

# Create rockspec
cat > my-lib-0.1.0-1.rockspec << 'EOF'
package = "my-lib"
version = "0.1.0-1"
source = {
   url = "git://github.com/username/my-lib",
   tag = "v0.1.0"
}
description = {
   summary = "My Lua Library",
   detailed = "A detailed description",
   homepage = "https://github.com/username/my-lib",
   license = "MIT"
}
dependencies = {
   "lua >= 5.4"
}
build = {
   type = "builtin",
   modules = {
      ["my-lib"] = "lib/my-lib.lua"
   }
}
EOF

# Install dependencies
luarocks install --only-deps my-lib-0.1.0-1.rockspec

# Build and install locally
luarocks make
```

---

## Lua-Specific Setup

### 1. Environment Setup

#### Install Lua

**Linux (Ubuntu/Debian)**:
```bash
# Lua 5.4
sudo apt update
sudo apt install lua5.4 liblua5.4-dev

# LuaJIT (faster, but Lua 5.1 compatible)
sudo apt install luajit libluajit-5.1-dev

# Verify installation
lua -v
luajit -v
```

**macOS**:
```bash
# Using Homebrew
brew install lua
brew install luajit

# Verify installation
lua -v
luajit -v
```

**Windows**:
```powershell
# Using Chocolatey
choco install lua

# Or download from: https://github.com/rjpcomputing/luaforwindows/releases

# Verify installation
lua -v
```

#### Install LuaRocks (Package Manager)

```bash
# Linux
sudo apt install luarocks

# macOS
brew install luarocks

# Windows (after installing Lua)
# Download from: https://luarocks.org/

# Verify installation
luarocks --version
```

#### Install Development Tools

```bash
# Luacheck (linter)
luarocks install luacheck

# LuaFormatter
luarocks install --server=https://luarocks.org/dev luaformatter

# Busted (testing framework)
luarocks install busted

# LuaCov (code coverage)
luarocks install luacov

# LDoc (documentation generator)
luarocks install ldoc

# Penlight (utility library)
luarocks install penlight

# LuaSocket (networking)
luarocks install luasocket
```

### 2. Project Structure

```
my-project/
├── .github/
│   └── workflows/
│       └── ci.yml
├── .luacheckrc           # Linter configuration
├── .luacov               # Coverage configuration
├── .editorconfig
├── .gitignore
├── README.md
├── CHANGELOG.md
├── LICENSE
├── rockspec              # Or *.rockspec
├── config.lua            # Configuration
├── docs/
│   ├── ROADMAP.md
│   ├── SPECS.md
│   ├── specs/
│   └── api/
├── src/                  # Main source (or lib/)
│   ├── init.lua         # Module entry point
│   ├── main.lua         # Executable entry point
│   ├── module1.lua
│   └── utils.lua
├── lib/                  # Alternative to src/
│   └── myproject/
│       ├── init.lua
│       └── module.lua
├── bin/                  # Executable scripts
│   └── myproject
├── tests/                # Test files (or spec/)
│   ├── test_module1.lua
│   └── test_utils.lua
├── spec/                 # Alternative test directory
│   ├── module1_spec.lua
│   └── utils_spec.lua
├── examples/             # Example scripts
│   └── example1.lua
└── scripts/              # Build/utility scripts
    ├── test.sh
    └── lint.sh
```

### 3. Version Management

**Using asdf (recommended)**:
```bash
# Install asdf
git clone https://github.com/asdf-vm/asdf.git ~/.asdf

# Install Lua plugin
asdf plugin-add lua

# Install specific version
asdf install lua 5.4.6
asdf install lua luajit-2.1.0-beta3

# Set version for project
asdf local lua 5.4.6

# Create .tool-versions
cat > .tool-versions << 'EOF'
lua 5.4.6
EOF
```

**Using LuaVer**:
```bash
# Install luaver
curl https://raw.githubusercontent.com/dhavalkapil/luaver/master/install.sh -o install.sh && bash install.sh

# Install Lua version
luaver install 5.4.6

# Use specific version
luaver use 5.4.6
```

---

## Configuration Standards

### 1. Rockspec File

**Basic Rockspec**:

```lua
-- my-project-0.1.0-1.rockspec
package = "my-project"
version = "0.1.0-1"

source = {
   url = "git://github.com/username/my-project",
   tag = "v0.1.0"
}

description = {
   summary = "Brief description of my project",
   detailed = [[
      Detailed description of my project.
      Can span multiple lines.
   ]],
   homepage = "https://github.com/username/my-project",
   license = "MIT",
   maintainer = "Your Name <email@example.com>"
}

dependencies = {
   "lua >= 5.4, < 5.5",
   "luasocket >= 3.0",
   "penlight >= 1.13.0",
}

build = {
   type = "builtin",
   modules = {
      ["my-project"] = "src/init.lua",
      ["my-project.module1"] = "src/module1.lua",
      ["my-project.utils"] = "src/utils.lua",
   },
   install = {
      bin = {
         "bin/my-project"
      }
   }
}

test_dependencies = {
   "busted >= 2.0",
   "luacov >= 0.15.0"
}

test = {
   type = "busted"
}
```

**Advanced Rockspec with C Extensions**:

```lua
package = "my-native-lib"
version = "0.1.0-1"

-- ... source and description ...

dependencies = {
   "lua >= 5.4"
}

external_dependencies = {
   OPENSSL = {
      header = "openssl/ssl.h",
      library = "ssl"
   }
}

build = {
   type = "builtin",
   modules = {
      ["my-native-lib"] = {
         sources = {"src/native.c"},
         libraries = {"ssl", "crypto"},
         incdirs = {"$(OPENSSL_INCDIR)"},
         libdirs = {"$(OPENSSL_LIBDIR)"}
      }
   }
}
```

### 2. .luacheckrc Configuration

```lua
-- .luacheckrc
std = "lua54+luajit"
max_line_length = 100
max_code_line_length = 100
max_comment_line_length = 120

-- Global settings
codes = true
jobs = 4

-- Files to ignore
exclude_files = {
   "tests/**/*.lua",
   "spec/**/*.lua",
   ".luarocks/**",
   ".install/**"
}

-- Warnings to ignore
ignore = {
   "212/self",  -- Unused argument 'self'
   "212/event", -- Unused argument 'event'
   "213",       -- Unused loop variable
}

-- Global variables allowed
globals = {
   -- Testing frameworks
   "describe",
   "it",
   "before_each",
   "after_each",
   "assert",
   "spy",
   "stub",
   "mock",
   
   -- Game engines
   "love",    -- Love2D
   
   -- Web frameworks
   "ngx",     -- OpenResty
   
   -- Editors
   "vim",     -- Neovim
}

-- Read-only globals
read_globals = {
   "awesome",  -- AwesomeWM
   "client",
   "root",
}

-- Per-file overrides
files["src/init.lua"] = {
   max_line_length = 120
}

files["tests/**/*.lua"] = {
   std = "+busted"
}
```

### 3. .luacov Configuration

```lua
-- .luacov
return {
   -- Files to include
   include = {
      "^src/",
      "^lib/"
   },
   
   -- Files to exclude
   exclude = {
      "tests/",
      "spec/",
      ".luarocks/"
   },
   
   -- Statistics file
   statsfile = "luacov.stats.out",
   
   -- Report file
   reportfile = "luacov.report.out",
   
   -- Delete stats file
   deletestats = false,
   
   -- Run reporter
   runreport = true,
   
   -- Coverage threshold
   threshold = 90,
   
   -- Reporter configuration
   reporter = "default",
   
   -- Custom reporter settings
   reportfile = "coverage.txt",
}
```

### 4. .editorconfig

```ini
# .editorconfig
root = true

[*.lua]
indent_style = space
indent_size = 2
end_of_line = lf
charset = utf-8
trim_trailing_whitespace = true
insert_final_newline = true
max_line_length = 100

[*.rockspec]
indent_style = space
indent_size = 3
```

### 5. .gitignore

```gitignore
# Lua
luac.out
*.lua~
*.luac

# LuaRocks
.luarocks/
*.rock
*.src.rock

# Testing
luacov.*.out
*.test

# IDE
.vscode/
.idea/
*.swp
*.swo
*~

# OS
.DS_Store
Thumbs.db

# Build artifacts
build/
dist/

# Logs
*.log
logs/

# Coverage
coverage/
luacov.report.out
luacov.stats.out
```

---

## Source Code Standards

### 1. File Naming Conventions

```
src/module_name.lua       # Module implementation
src/init.lua              # Package entry point
tests/module_name_test.lua # Test file
spec/module_name_spec.lua  # Alternative test file
```

### 2. Module Template

```lua
--- Module description
-- Brief description of the module's purpose and functionality.
--
-- @module my-module
-- @author Your Name
-- @license MIT
-- @copyright 2025

local M = {}

-- Module version
M.VERSION = "0.1.0"

--- Private functions ---

-- Private helper function
-- @param value The value to process
-- @return Processed value
local function private_helper(value)
  if not value then
    return nil
  end
  return value * 2
end

--- Public API ---

--- Initialize the module
-- @param config Configuration table
-- @return true on success, nil and error message on failure
function M.init(config)
  if not config then
    return nil, "configuration required"
  end
  
  -- Validate config
  if not config.name then
    return nil, "config.name is required"
  end
  
  -- Initialize
  M.config = config
  return true
end

--- Process data
-- @param data Input data
-- @return Processed result or nil on error
-- @return Error message if failed
function M.process(data)
  if not data then
    return nil, "data cannot be nil"
  end
  
  -- Process
  local result = private_helper(data)
  return result
end

--- Clean up resources
function M.cleanup()
  M.config = nil
end

return M
```

### 3. Class-like Pattern (OOP Style)

```lua
--- MyClass implementation
-- @classmod MyClass

local MyClass = {}
MyClass.__index = MyClass

--- Create a new instance
-- @param name Instance name
-- @return New instance
function MyClass.new(name)
  local self = setmetatable({}, MyClass)
  self.name = name or "default"
  self.data = {}
  return self
end

--- Add data to instance
-- @param key The key
-- @param value The value
function MyClass:add(key, value)
  if not key then
    error("key cannot be nil", 2)
  end
  self.data[key] = value
end

--- Get data from instance
-- @param key The key
-- @return The value or nil
function MyClass:get(key)
  return self.data[key]
end

--- Get string representation
-- @return String representation
function MyClass:__tostring()
  return string.format("MyClass(%s)", self.name)
end

return MyClass
```

### 4. Error Handling Pattern

```lua
--- Error handling example
-- @module error-handling

local M = {}

--- Result type for operations
-- @field ok Boolean indicating success
-- @field value Result value (if ok)
-- @field error Error message (if not ok)

--- Wrap a result in a Result type
-- @param value The value
-- @return Result table
function M.ok(value)
  return {ok = true, value = value}
end

--- Wrap an error in a Result type
-- @param message Error message
-- @return Result table
function M.err(message)
  return {ok = false, error = message}
end

--- Protected call wrapper
-- @param fn Function to call
-- @param ... Arguments to function
-- @return Result table
function M.try(fn, ...)
  local success, result = pcall(fn, ...)
  if success then
    return M.ok(result)
  else
    return M.err(result)
  end
end

--- Example usage
local function divide(a, b)
  if b == 0 then
    error("division by zero")
  end
  return a / b
end

function M.safe_divide(a, b)
  local result = M.try(divide, a, b)
  if result.ok then
    return result.value
  else
    return nil, result.error
  end
end

return M
```

### 5. Code Style Guidelines

**Naming Conventions**:
- Functions: `snake_case` (e.g., `get_value`, `process_data`)
- Variables: `snake_case` (e.g., `user_name`, `max_count`)
- Constants: `UPPER_SNAKE_CASE` (e.g., `MAX_SIZE`, `DEFAULT_PORT`)
- Modules: `lowercase` or `snake_case` (e.g., `mymodule`, `my_module`)
- Classes: `PascalCase` (e.g., `MyClass`, `HttpClient`)
- Private functions: `_prefixed` (e.g., `_internal_helper`)

**Formatting**:
- Indentation: 2 spaces
- Line length: 100 characters maximum
- String quotes: Double quotes for strings, single for chars
- Tables: Trailing comma in multiline tables

**Documentation**:
- Use LDoc-style comments
- Document all public functions
- Include `@param`, `@return`, `@usage` tags

---

## Testing Standards

### 1. Busted Testing Framework

**Install Busted**:
```bash
luarocks install busted
luarocks install luacov
```

**Basic Test Structure**:

```lua
-- tests/module_test.lua
local mymodule = require("src.mymodule")

describe("mymodule", function()
  describe("init", function()
    it("should initialize with valid config", function()
      local config = {name = "test"}
      local ok, err = mymodule.init(config)
      
      assert.is_true(ok)
      assert.is_nil(err)
    end)
    
    it("should fail without config", function()
      local ok, err = mymodule.init(nil)
      
      assert.is_nil(ok)
      assert.is_not_nil(err)
      assert.equals("configuration required", err)
    end)
  end)
  
  describe("process", function()
    before_each(function()
      mymodule.init({name = "test"})
    end)
    
    after_each(function()
      mymodule.cleanup()
    end)
    
    it("should process data correctly", function()
      local result = mymodule.process(10)
      assert.equals(20, result)
    end)
    
    it("should handle nil data", function()
      local result, err = mymodule.process(nil)
      assert.is_nil(result)
      assert.equals("data cannot be nil", err)
    end)
  end)
end)
```

**Testing with Spies and Stubs**:

```lua
-- tests/with_mocks_test.lua
describe("testing with mocks", function()
  local mymodule
  
  before_each(function()
    mymodule = require("src.mymodule")
  end)
  
  it("should call callback function", function()
    local callback = spy.new(function() end)
    
    mymodule.process_with_callback({}, callback)
    
    assert.spy(callback).was_called()
    assert.spy(callback).was_called_with(match._, match._)
  end)
  
  it("should use stubbed dependency", function()
    local dependency = stub(mymodule, "get_data")
    dependency.returns({value = 42})
    
    local result = mymodule.process()
    
    assert.equals(42, result.value)
    assert.stub(dependency).was_called()
  end)
end)
```

### 2. Test Organization

```
tests/
├── test_helper.lua       # Common test utilities
├── unit/                 # Unit tests
│   ├── module1_test.lua
│   └── utils_test.lua
├── integration/          # Integration tests
│   └── api_test.lua
└── fixtures/             # Test data
    ├── sample_data.lua
    └── mock_responses.lua
```

### 3. Running Tests

**Run all tests**:
```bash
# Basic run
busted

# With coverage
busted --coverage

# Verbose output
busted --verbose

# Run specific test
busted tests/module_test.lua

# Run with pattern
busted --pattern=_test.lua

# Watch mode (requires inotify-tools)
busted --watch
```

**Generate Coverage Report**:
```bash
# Run tests with coverage
busted --coverage

# Generate report
luacov

# View report
cat luacov.report.out

# Or generate HTML report
luacov-html
```

### 4. .busted Configuration

```lua
-- .busted
return {
  default = {
    verbose = true,
    coverage = false,
    lpath = "src/?.lua;src/?/init.lua",
    cpath = "src/?.so",
    pattern = "_test.lua$",
    ROOT = {"tests/"},
  },
  
  coverage = {
    coverage = true,
    verbose = true,
    lpath = "src/?.lua;src/?/init.lua",
    cpath = "src/?.so",
  }
}
```

---

## Build & Deployment

### 1. Build Scripts

**scripts/build.sh**:
```bash
#!/bin/bash
set -e

echo "Building Lua project..."

# Create build directory
mkdir -p build

# Copy source files
cp -r src build/
cp -r lib build/ 2>/dev/null || true

# Compile rockspec
luarocks make *.rockspec

echo "Build complete!"
```

**scripts/test.sh**:
```bash
#!/bin/bash
set -e

echo "Running tests..."

# Install test dependencies
luarocks install --only-deps *.rockspec

# Run linter
luacheck src/ lib/

# Run tests with coverage
busted --coverage

# Generate coverage report
luacov

# Check coverage threshold
echo "Checking coverage threshold..."
LUA_COVERAGE=$(luacov | grep -oP '\d+\.\d+(?=% coverage)' || echo "0")
THRESHOLD=90

if (( $(echo "$LUA_COVERAGE < $THRESHOLD" | bc -l) )); then
  echo "❌ Coverage ($LUA_COVERAGE%) is below threshold ($THRESHOLD%)"
  exit 1
fi

echo "✅ All tests passed with $LUA_COVERAGE% coverage"
```

**scripts/lint.sh**:
```bash
#!/bin/bash
set -e

echo "Running linter..."

# Install luacheck if not present
command -v luacheck >/dev/null 2>&1 || luarocks install luacheck

# Run luacheck
luacheck src/ lib/ --config .luacheckrc

echo "✅ Linting complete"
```

### 2. CI/CD Configuration

**.github/workflows/ci.yml**:
```yaml
name: CI

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main, develop ]

jobs:
  test:
    runs-on: ${{ matrix.os }}
    strategy:
      matrix:
        os: [ubuntu-latest, macos-latest]
        lua-version: ["5.4", "5.3", "luajit"]
        
    steps:
    - uses: actions/checkout@v3
    
    - name: Setup Lua
      uses: leafo/gh-actions-lua@v10
      with:
        luaVersion: ${{ matrix.lua-version }}
        
    - name: Setup LuaRocks
      uses: leafo/gh-actions-luarocks@v4
      
    - name: Install Dependencies
      run: |
        luarocks install luacheck
        luarocks install busted
        luarocks install luacov
        luarocks install --only-deps *.rockspec
        
    - name: Run Linter
      run: luacheck src/ lib/
      
    - name: Run Tests
      run: busted --verbose --coverage
      
    - name: Generate Coverage
      run: luacov
      
    - name: Upload Coverage
      uses: codecov/codecov-action@v3
      with:
        files: ./luacov.report.out

  format-check:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v3
    
    - name: Setup Lua
      uses: leafo/gh-actions-lua@v10
      with:
        luaVersion: "5.4"
        
    - name: Setup LuaRocks
      uses: leafo/gh-actions-luarocks@v4
      
    - name: Install Formatter
      run: luarocks install --server=https://luarocks.org/dev luaformatter
      
    - name: Check Formatting
      run: find src lib -name "*.lua" -exec lua-format --check {} \;
```

### 3. Publishing to LuaRocks

**Prepare for release**:
```bash
# 1. Update version in rockspec
# 2. Update CHANGELOG.md
# 3. Commit changes
git add .
git commit -m "chore: release v0.1.0"

# 4. Create tag
git tag -a v0.1.0 -m "Release v0.1.0"
git push origin v0.1.0

# 5. Upload to LuaRocks
luarocks upload my-project-0.1.0-1.rockspec --api-key=YOUR_API_KEY

# 6. Verify installation
luarocks install my-project
```

**Create source rock**:
```bash
# Pack source
luarocks pack my-project-0.1.0-1.rockspec

# Test installation from rock
luarocks install my-project-0.1.0-1.src.rock
```

---

## Documentation

### 1. LDoc Configuration

**config.ld**:
```lua
-- config.ld
project = "My Project"
title = "My Project Documentation"
description = "Detailed project description"

-- Source files
file = {"src", "lib"}

-- Output directory
dir = "docs/api"

-- Format
format = "markdown"

-- Template
template = true
style = true

-- Options
all = false
readme = "README.md"
topics = {"CHANGELOG.md"}

-- Custom tags
custom_tags = {
  {"usage", title="Usage", format=nil},
  {"note", title="Note"},
  {"todo", title="TODO"}
}

-- Exclude patterns
exclude = {
  "tests",
  "spec"
}
```

**Generate documentation**:
```bash
# Install LDoc
luarocks install ldoc

# Generate docs
ldoc .

# Open documentation
xdg-open docs/api/index.html  # Linux
open docs/api/index.html      # macOS
```

### 2. README.md Template

```markdown
# My Lua Project

Brief description of your project.

## Features

- Feature 1
- Feature 2
- Feature 3

## Installation

\`\`\`bash
# Using LuaRocks
luarocks install my-project

# From source
git clone https://github.com/username/my-project
cd my-project
luarocks make
\`\`\`

## Quick Start

\`\`\`lua
local myproject = require("my-project")

-- Initialize
myproject.init({name = "example"})

-- Use the module
local result = myproject.process({data = "test"})
print(result)

-- Cleanup
myproject.cleanup()
\`\`\`

## Documentation

See [API Documentation](docs/api/index.html)

## Testing

\`\`\`bash
# Run tests
busted

# Run with coverage
busted --coverage
luacov
\`\`\`

## License

MIT License - See LICENSE file
```

---

## Lua Best Practices

### 1. Local Variables

```lua
-- ❌ BAD: Global variables
function calculate(x)
  result = x * 2  -- Creates global!
  return result
end

-- ✅ GOOD: Use local
local function calculate(x)
  local result = x * 2
  return result
end

-- ✅ BETTER: Direct return
local function calculate(x)
  return x * 2
end
```

### 2. Table Best Practices

```lua
-- ❌ BAD: Inefficient table construction
local t = {}
for i = 1, 1000 do
  table.insert(t, i)
end

-- ✅ GOOD: Direct assignment (faster)
local t = {}
for i = 1, 1000 do
  t[i] = i
end

-- ✅ BETTER: Pre-allocate if size known
local t = table.move({}, 1, 0, 1, {})  -- Empty but pre-sized

-- Table as array (sequential indices)
local array = {1, 2, 3, 4, 5}

-- Table as dictionary
local dict = {
  name = "John",
  age = 30,
  ["key-with-dash"] = "value"
}

-- Mixed (avoid this)
local mixed = {
  1, 2, 3,           -- Array part
  name = "test",     -- Hash part
}
```

### 3. String Operations

```lua
-- ❌ BAD: String concatenation in loop
local result = ""
for i = 1, 1000 do
  result = result .. tostring(i)  -- Creates new string each time!
end

-- ✅ GOOD: Use table.concat
local parts = {}
for i = 1, 1000 do
  parts[i] = tostring(i)
end
local result = table.concat(parts)

-- ✅ GOOD: String formatting
local formatted = string.format("Name: %s, Age: %d", name, age)
```

### 4. Module Pattern

```lua
-- ❌ BAD: Polluting global namespace
function myfunction()
  -- ...
end

-- ✅ GOOD: Module pattern
local M = {}

function M.public_function()
  -- ...
end

local function private_function()
  -- Only accessible within module
end

return M
```

### 5. Error Handling

```lua
-- ❌ BAD: Using assert for user errors
local function divide(a, b)
  assert(b ~= 0, "division by zero")
  return a / b
end

-- ✅ GOOD: Return nil + error message
local function divide(a, b)
  if b == 0 then
    return nil, "division by zero"
  end
  return a / b
end

-- Usage
local result, err = divide(10, 0)
if not result then
  print("Error:", err)
  return
end

-- ✅ GOOD: Use pcall for protected calls
local success, result = pcall(risky_function, arg1, arg2)
if not success then
  print("Function failed:", result)
end
```

### 6. Performance Optimization

```lua
-- ❌ SLOW: Table lookups in loop
for i = 1, 1000000 do
  local value = math.sqrt(i)  -- Table lookup each time
end

-- ✅ FAST: Cache table lookups
local sqrt = math.sqrt
for i = 1, 1000000 do
  local value = sqrt(i)
end

-- ❌ SLOW: Creating closures in loop
local functions = {}
for i = 1, 100 do
  functions[i] = function() return i end  -- New closure each iteration
end

-- ✅ FAST: Avoid closures when possible
local functions = {}
local function make_function(value)
  return function() return value end
end
for i = 1, 100 do
  functions[i] = make_function(i)
end
```

---

## Common Patterns

### 1. Iterator Pattern

```lua
--- Custom iterator example
local function range(from, to, step)
  step = step or 1
  return function(_, current)
    local next = current + step
    if next > to then
      return nil
    end
    return next
  end, nil, from - step
end

-- Usage
for i in range(1, 10, 2) do
  print(i)  -- 1, 3, 5, 7, 9
end
```

### 2. Builder Pattern

```lua
--- Builder pattern for complex objects
local Builder = {}
Builder.__index = Builder

function Builder.new()
  return setmetatable({
    _data = {}
  }, Builder)
end

function Builder:with_name(name)
  self._data.name = name
  return self  -- Chain
end

function Builder:with_age(age)
  self._data.age = age
  return self
end

function Builder:build()
  return self._data
end

-- Usage
local person = Builder.new()
  :with_name("John")
  :with_age(30)
  :build()
```

### 3. Callback Pattern

```lua
--- Callback pattern for async operations
local function process_async(data, callback)
  -- Simulate async operation
  local result = data * 2
  
  -- Call callback with result
  if callback then
    callback(nil, result)  -- (error, result) convention
  end
end

-- Usage
process_async(10, function(err, result)
  if err then
    print("Error:", err)
    return
  end
  print("Result:", result)
end)
```

### 4. Memoization Pattern

```lua
--- Memoization for expensive functions
local function memoize(fn)
  local cache = {}
  return function(...)
    local key = table.concat({...}, ",")
    if not cache[key] then
      cache[key] = fn(...)
    end
    return cache[key]
  end
end

-- Usage
local function expensive_calculation(n)
  -- Simulate expensive operation
  local result = 0
  for i = 1, n do
    result = result + i
  end
  return result
end

local memoized = memoize(expensive_calculation)
print(memoized(1000))  -- Calculates
print(memoized(1000))  -- Returns cached
```

---

## Troubleshooting

### 1. Common Errors

**Problem**: `attempt to index a nil value`
```lua
-- ❌ ERROR: Trying to access field on nil
local config = nil
print(config.name)  -- Error!

-- ✅ FIX: Check for nil first
local config = nil
if config then
  print(config.name)
end

-- ✅ BETTER: Use default values
local config = config or {}
print(config.name or "default")
```

**Problem**: `attempt to call a nil value`
```lua
-- ❌ ERROR: Function doesn't exist
local mymodule = require("mymodule")
mymodule.nonexistent()  -- Error!

-- ✅ FIX: Check if function exists
if mymodule.nonexistent then
  mymodule.nonexistent()
end
```

**Problem**: Module not found
```bash
# Check LUA_PATH
lua -e "print(package.path)"

# Add custom path
export LUA_PATH="./src/?.lua;./lib/?.lua;;"

# Or in code
package.path = package.path .. ";./src/?.lua"
```

### 2. Memory Leaks

```lua
-- ❌ BAD: Circular references
local a = {}
local b = {ref = a}
a.ref = b  -- Circular reference

-- ✅ GOOD: Break circular references
local a = {}
local b = {ref = a}
-- Later when done:
b.ref = nil
a = nil
b = nil

-- ❌ BAD: Keeping references in closures
local huge_data = {...}  -- Large dataset
return function()
  return huge_data[1]  -- Keeps entire table in memory
end

-- ✅ GOOD: Extract only what's needed
local first_item = huge_data[1]
return function()
  return first_item
end
```

### 3. Performance Issues

```bash
# Profile with LuaJIT profiler
luajit -jp script.lua

# Profile with lua-profiler
luarocks install luaprofiler
lua -lluaprofiler script.lua
```

---

## Complete Workflow

### Example: Creating a Lua Library

```bash
# 1. Create project structure
mkdir my-lua-lib && cd my-lua-lib
mkdir -p lib tests docs

# 2. Initialize Git
git init
git checkout -b develop

# 3. Create rockspec
cat > my-lua-lib-0.1.0-1.rockspec << 'EOF'
package = "my-lua-lib"
version = "0.1.0-1"
source = {
   url = "git://github.com/username/my-lua-lib",
   tag = "v0.1.0"
}
description = {
   summary = "My Lua Library",
   license = "MIT"
}
dependencies = {
   "lua >= 5.4"
}
build = {
   type = "builtin",
   modules = {
      ["my-lua-lib"] = "lib/my-lua-lib.lua"
   }
}
EOF

# 4. Create module
cat > lib/my-lua-lib.lua << 'EOF'
local M = {}
M.VERSION = "0.1.0"

function M.hello(name)
  return "Hello, " .. (name or "World") .. "!"
end

return M
EOF

# 5. Create test
cat > tests/my-lua-lib_test.lua << 'EOF'
local mylib = require("my-lua-lib")

describe("my-lua-lib", function()
  it("should greet with name", function()
    assert.equals("Hello, Lua!", mylib.hello("Lua"))
  end)
  
  it("should greet with default", function()
    assert.equals("Hello, World!", mylib.hello())
  end)
end)
EOF

# 6. Install dependencies and test
luarocks install busted
busted

# 7. Create documentation
cat > docs/ROADMAP.md << 'EOF'
# Roadmap
## Phase 1: Core Implementation
- [x] Project setup
- [x] Basic functionality
- [ ] Advanced features
EOF

# 8. Commit
git add .
git commit -m "feat: initial implementation"

# 9. Tag and release
git tag -a v0.1.0 -m "Release v0.1.0"
```

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-10-11 | Initial Lua manual creation |

---

## References

- [Lua 5.4 Reference Manual](https://www.lua.org/manual/5.4/)
- [LuaRocks Documentation](https://github.com/luarocks/luarocks/wiki)
- [LuaJIT Documentation](https://luajit.org/luajit.html)
- [Busted Testing Framework](https://olivinelabs.com/busted/)
- [LDoc Documentation Generator](https://github.com/lunarmodules/LDoc)
- [Lua Style Guide](http://lua-users.org/wiki/LuaStyleGuide)

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

