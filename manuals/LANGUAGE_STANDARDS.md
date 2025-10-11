# Language-Specific Standards

**Version**: 1.0.0  
**Last Updated**: 2025-10-11  
**Target Audience**: AI Agents implementing language-specific manuals

---

## Table of Contents

1. [Overview](#overview)
2. [Mandatory Components](#mandatory-components)
3. [Configuration Standards](#configuration-standards)
4. [Source Code Standards](#source-code-standards)
5. [Testing Standards](#testing-standards)
6. [Documentation Standards](#documentation-standards)
7. [Best Practices Manual](#best-practices-manual)
8. [Language-Specific Requirements](#language-specific-requirements)

---

## Overview

Every language-specific manual MUST include standardized sections for:
- **Configuration**: Project setup, dependencies, tooling
- **Source Code**: Organization, patterns, conventions
- **Testing**: Test structure, coverage, frameworks
- **Documentation**: API docs, guides, examples
- **Best Practices**: Language idioms, anti-patterns, optimization

This document defines the minimum requirements for each section across all supported languages.

---

## Mandatory Components

### File Structure

All projects MUST follow this structure:

```
project-root/
├── .github/                       # GitHub configuration
│   ├── workflows/                 # CI/CD workflows
│   │   ├── ci.yml
│   │   ├── cd.yml
│   │   └── security.yml
│   ├── ISSUE_TEMPLATE/            # Issue templates
│   │   ├── bug_report.md
│   │   ├── feature_request.md
│   │   └── documentation.md
│   ├── PULL_REQUEST_TEMPLATE.md   # PR template
│   └── CODEOWNERS                 # Code ownership
├── config/                        # Configuration files
│   ├── [env].config.[ext]         # Environment-specific configs
│   └── defaults.[ext]             # Default configuration
├── docs/                          # Documentation
│   ├── ROADMAP.md
│   ├── SPECS.md
│   ├── specs/
│   ├── api/
│   └── user-guide/
├── scripts/                       # Automation scripts
│   ├── build.[ext]
│   ├── test.[ext]
│   ├── lint.[ext]
│   ├── coverage.[ext]
│   └── deploy.[ext]
├── src/                           # Source code
│   ├── [core]/                    # Core business logic
│   ├── [utils]/                   # Utilities
│   ├── [types]/                   # Type definitions
│   └── index.[ext]                # Entry point
├── tests/                         # Tests
│   ├── unit/
│   ├── integration/
│   ├── e2e/
│   ├── fixtures/
│   └── helpers/
├── .cursorrules                   # AI development rules
├── .gitignore
├── .editorconfig                  # Editor configuration
├── CHANGELOG.md
├── CONTRIBUTING.md
├── LICENSE
├── README.md
└── [language-specific files]      # package.json, Cargo.toml, etc.
```

---

## Configuration Standards

### 1. Package/Dependency Management

Every project MUST have:

#### Dependency File

| Language | File | Purpose |
|----------|------|---------|
| TypeScript/JavaScript | `package.json` | npm/pnpm/yarn dependencies |
| Python | `requirements.txt`, `pyproject.toml` | pip dependencies |
| Rust | `Cargo.toml` | Cargo dependencies |
| Java | `pom.xml`, `build.gradle` | Maven/Gradle dependencies |
| C# | `*.csproj`, `packages.config` | NuGet dependencies |
| Go | `go.mod` | Go modules |
| PHP | `composer.json` | Composer dependencies |
| Ruby | `Gemfile` | Bundler dependencies |

**Mandatory Fields**:
- Project name
- Version (SemVer)
- Description
- Author/Maintainer
- License
- Dependencies with version constraints
- Scripts for common tasks (build, test, lint)

**Example (package.json)**:
```json
{
  "name": "project-name",
  "version": "1.0.0",
  "description": "Project description",
  "main": "dist/index.js",
  "author": "Author Name",
  "license": "MIT",
  "scripts": {
    "build": "tsc",
    "test": "vitest run",
    "test:watch": "vitest",
    "test:coverage": "vitest run --coverage",
    "lint": "eslint src/",
    "lint:fix": "eslint src/ --fix",
    "format": "prettier --write src/",
    "format:check": "prettier --check src/",
    "type-check": "tsc --noEmit"
  },
  "dependencies": {
    "package-name": "^1.0.0"
  },
  "devDependencies": {
    "test-framework": "^1.0.0",
    "linter": "^1.0.0"
  }
}
```

#### Lock File

MUST include lock file for reproducible builds:
- `package-lock.json` / `pnpm-lock.yaml` / `yarn.lock` (Node.js)
- `Pipfile.lock` / `poetry.lock` (Python)
- `Cargo.lock` (Rust)
- Maven/Gradle lock files (Java)
- `packages.lock.json` (C#)
- `go.sum` (Go)
- `composer.lock` (PHP)
- `Gemfile.lock` (Ruby)

### 2. Linter Configuration

Every project MUST have a linter configured:

| Language | Linter | Config File |
|----------|--------|-------------|
| TypeScript/JavaScript | ESLint | `.eslintrc.json`, `eslint.config.js` |
| Python | Ruff, Pylint, Flake8 | `.ruff.toml`, `.pylintrc`, `.flake8` |
| Rust | Clippy | `clippy.toml` or `Cargo.toml` |
| Java | Checkstyle, SpotBugs | `checkstyle.xml`, `spotbugs.xml` |
| C# | StyleCop, Roslyn | `.editorconfig`, `stylecop.json` |
| Go | golangci-lint | `.golangci.yml` |
| PHP | PHP_CodeSniffer | `phpcs.xml` |
| Ruby | RuboCop | `.rubocop.yml` |

**Mandatory Rules**:
- No unused variables/imports
- No console.log/print statements in production code
- Consistent naming conventions
- Maximum line length (80-120 characters)
- Maximum function/method complexity
- Enforce async/await usage
- No any/dynamic types (in strongly-typed languages)

**Example (.eslintrc.json)**:
```json
{
  "extends": [
    "eslint:recommended",
    "plugin:@typescript-eslint/recommended"
  ],
  "parser": "@typescript-eslint/parser",
  "plugins": ["@typescript-eslint"],
  "rules": {
    "no-console": "error",
    "no-unused-vars": "error",
    "@typescript-eslint/explicit-function-return-type": "error",
    "@typescript-eslint/no-explicit-any": "error",
    "complexity": ["error", 10],
    "max-lines-per-function": ["error", 50]
  }
}
```

### 3. Formatter Configuration

Every project MUST have a code formatter:

| Language | Formatter | Config File |
|----------|-----------|-------------|
| TypeScript/JavaScript | Prettier | `.prettierrc.json` |
| Python | Black, autopep8 | `pyproject.toml` |
| Rust | rustfmt | `rustfmt.toml` |
| Java | google-java-format | `.clang-format` |
| C# | dotnet format | `.editorconfig` |
| Go | gofmt, goimports | (built-in) |
| PHP | PHP-CS-Fixer | `.php-cs-fixer.php` |
| Ruby | RuboCop | `.rubocop.yml` |

**Mandatory Settings**:
- Consistent indentation (2 or 4 spaces, NO tabs)
- Trailing commas (where applicable)
- Single or double quotes (consistent)
- Line endings (LF for Unix, CRLF for Windows)
- Max line length

**Example (.prettierrc.json)**:
```json
{
  "semi": true,
  "trailingComma": "es5",
  "singleQuote": true,
  "printWidth": 100,
  "tabWidth": 2,
  "useTabs": false,
  "endOfLine": "lf"
}
```

### 4. Type Checking Configuration (if applicable)

For statically-typed or type-checked languages:

| Language | Type Checker | Config File |
|----------|--------------|-------------|
| TypeScript | tsc | `tsconfig.json` |
| Python | mypy, pyright | `mypy.ini`, `pyrightconfig.json` |
| Rust | (built-in) | `Cargo.toml` |
| Java | (built-in) | Compiler settings |
| C# | (built-in) | `.csproj` |
| Go | (built-in) | (none needed) |

**Mandatory Settings (TypeScript)**:
```json
{
  "compilerOptions": {
    "target": "ES2022",
    "module": "commonjs",
    "lib": ["ES2022"],
    "outDir": "./dist",
    "rootDir": "./src",
    "strict": true,
    "esModuleInterop": true,
    "skipLibCheck": true,
    "forceConsistentCasingInFileNames": true,
    "resolveJsonModule": true,
    "declaration": true,
    "declarationMap": true,
    "sourceMap": true
  },
  "include": ["src/**/*"],
  "exclude": ["node_modules", "dist", "tests"]
}
```

### 5. Editor Configuration

Every project MUST have `.editorconfig`:

```ini
root = true

[*]
charset = utf-8
end_of_line = lf
insert_final_newline = true
trim_trailing_whitespace = true

[*.{js,ts,jsx,tsx}]
indent_style = space
indent_size = 2

[*.{py}]
indent_style = space
indent_size = 4

[*.{rs}]
indent_style = space
indent_size = 4

[*.{java}]
indent_style = space
indent_size = 4

[*.{yml,yaml}]
indent_style = space
indent_size = 2

[*.md]
trim_trailing_whitespace = false
```

### 6. Environment Configuration

Every project MUST support environment-specific configuration:

**Files**:
- `.env.example` - Template with all required variables
- `.env` - Local environment (gitignored)
- `.env.test` - Test environment
- `.env.production` - Production environment (if needed)

**Example (.env.example)**:
```env
# Application
NODE_ENV=development
PORT=3000
LOG_LEVEL=info

# Database
DATABASE_URL=postgresql://user:pass@localhost:5432/dbname
DATABASE_POOL_SIZE=10

# Redis
REDIS_URL=redis://localhost:6379

# External Services
API_KEY=your-api-key-here
API_SECRET=your-api-secret-here

# Task Queue
TASK_QUEUE_URL=http://localhost:8080
TASK_QUEUE_TOKEN=your-token-here

# Vectorizer
VECTORIZER_URL=http://localhost:9000
VECTORIZER_TOKEN=your-token-here
```

---

## Source Code Standards

### 1. Directory Organization

#### TypeScript/JavaScript
```
src/
├── core/              # Core business logic
├── api/               # API routes/controllers
├── services/          # Business services
├── models/            # Data models
├── repositories/      # Data access layer
├── utils/             # Utility functions
├── types/             # Type definitions
├── middleware/        # Middleware (if applicable)
├── config/            # Configuration
└── index.ts           # Entry point
```

#### Python
```
src/
├── core/              # Core modules
├── api/               # API endpoints
├── services/          # Business services
├── models/            # Data models
├── repositories/      # Data access
├── utils/             # Utilities
├── types/             # Type stubs
├── config/            # Configuration
└── __init__.py
```

#### Rust
```
src/
├── core/              # Core modules
├── api/               # API handlers
├── services/          # Business logic
├── models/            # Data structures
├── repositories/      # Data access
├── utils/             # Utilities
├── config/            # Configuration
├── lib.rs             # Library root
└── main.rs            # Binary entry point
```

### 2. Naming Conventions

| Language | Files | Classes | Functions | Variables | Constants |
|----------|-------|---------|-----------|-----------|-----------|
| TypeScript | kebab-case | PascalCase | camelCase | camelCase | UPPER_SNAKE_CASE |
| Python | snake_case | PascalCase | snake_case | snake_case | UPPER_SNAKE_CASE |
| Rust | snake_case | PascalCase | snake_case | snake_case | UPPER_SNAKE_CASE |
| Java | PascalCase | PascalCase | camelCase | camelCase | UPPER_SNAKE_CASE |
| C# | PascalCase | PascalCase | PascalCase | camelCase | PascalCase |
| Go | snake_case | PascalCase | CamelCase/camelCase | camelCase | CamelCase |

### 3. Code Organization Patterns

#### Module Structure (TypeScript Example)

```typescript
// user.service.ts
import { UserRepository } from './user.repository';
import { User, CreateUserDto } from './types';

/**
 * Service for managing users
 */
export class UserService {
  constructor(private readonly userRepository: UserRepository) {}

  /**
   * Creates a new user
   * @param data User creation data
   * @returns Created user
   * @throws ValidationError if data is invalid
   */
  async createUser(data: CreateUserDto): Promise<User> {
    this.validateUserData(data);
    return this.userRepository.create(data);
  }

  private validateUserData(data: CreateUserDto): void {
    // Validation logic
  }
}
```

#### Error Handling

**TypeScript**:
```typescript
export class CustomError extends Error {
  constructor(
    message: string,
    public code: string,
    public statusCode: number = 500
  ) {
    super(message);
    this.name = this.constructor.name;
    Error.captureStackTrace(this, this.constructor);
  }
}

// Usage
throw new CustomError('User not found', 'USER_NOT_FOUND', 404);
```

**Python**:
```python
class CustomError(Exception):
    def __init__(self, message: str, code: str, status_code: int = 500):
        self.message = message
        self.code = code
        self.status_code = status_code
        super().__init__(self.message)

# Usage
raise CustomError('User not found', 'USER_NOT_FOUND', 404)
```

**Rust**:
```rust
use thiserror::Error;

#[derive(Error, Debug)]
pub enum CustomError {
    #[error("User not found: {0}")]
    UserNotFound(String),
    
    #[error("Validation error: {0}")]
    ValidationError(String),
}

// Usage
Err(CustomError::UserNotFound("user123".to_string()))
```

### 4. Code Documentation

Every public API MUST be documented:

**TypeScript (JSDoc)**:
```typescript
/**
 * Processes a user payment
 * 
 * @param userId - The user's unique identifier
 * @param amount - Payment amount in cents
 * @param currency - Three-letter currency code (e.g., 'USD')
 * @returns Payment confirmation with transaction ID
 * @throws {ValidationError} If amount is negative
 * @throws {InsufficientFundsError} If user has insufficient balance
 * 
 * @example
 * ```typescript
 * const result = await processPayment('user123', 1000, 'USD');
 * console.log(result.transactionId);
 * ```
 */
export async function processPayment(
  userId: string,
  amount: number,
  currency: string
): Promise<PaymentResult> {
  // Implementation
}
```

**Python (Docstring)**:
```python
def process_payment(user_id: str, amount: int, currency: str) -> PaymentResult:
    """
    Processes a user payment.
    
    Args:
        user_id: The user's unique identifier
        amount: Payment amount in cents
        currency: Three-letter currency code (e.g., 'USD')
    
    Returns:
        Payment confirmation with transaction ID
    
    Raises:
        ValidationError: If amount is negative
        InsufficientFundsError: If user has insufficient balance
    
    Example:
        >>> result = process_payment('user123', 1000, 'USD')
        >>> print(result.transaction_id)
    """
    # Implementation
```

**Rust (Doc Comments)**:
```rust
/// Processes a user payment
///
/// # Arguments
///
/// * `user_id` - The user's unique identifier
/// * `amount` - Payment amount in cents
/// * `currency` - Three-letter currency code (e.g., "USD")
///
/// # Returns
///
/// Payment confirmation with transaction ID
///
/// # Errors
///
/// * `ValidationError` - If amount is negative
/// * `InsufficientFundsError` - If user has insufficient balance
///
/// # Example
///
/// ```
/// let result = process_payment("user123", 1000, "USD").await?;
/// println!("{}", result.transaction_id);
/// ```
pub async fn process_payment(
    user_id: &str,
    amount: i64,
    currency: &str,
) -> Result<PaymentResult, PaymentError> {
    // Implementation
}
```

---

## Testing Standards

### 1. Test Directory Structure

```
tests/
├── unit/                          # Unit tests (fast, isolated)
│   ├── services/
│   │   └── user.service.test.ts
│   ├── utils/
│   │   └── validators.test.ts
│   └── models/
│       └── user.model.test.ts
├── integration/                   # Integration tests (with deps)
│   ├── api/
│   │   └── user.api.test.ts
│   └── database/
│       └── user.repository.test.ts
├── e2e/                          # End-to-end tests
│   └── user-flows.test.ts
├── fixtures/                      # Test data
│   ├── users.json
│   └── products.json
├── helpers/                       # Test utilities
│   ├── setup.ts
│   ├── teardown.ts
│   └── factories.ts
└── config.ts                     # Test configuration
```

### 2. Test Naming Conventions

**Pattern**: `[unit].test.[ext]` or `[unit].spec.[ext]`

**Test Structure**:
```typescript
describe('[Component/Class] [scenario]', () => {
  // Setup
  beforeAll(() => {
    // One-time setup
  });

  beforeEach(() => {
    // Per-test setup
  });

  afterEach(() => {
    // Per-test cleanup
  });

  afterAll(() => {
    // One-time cleanup
  });

  describe('[method/function]', () => {
    it('should [expected behavior] when [condition]', () => {
      // Arrange
      const input = createTestData();
      
      // Act
      const result = functionUnderTest(input);
      
      // Assert
      expect(result).toBe(expected);
    });

    it('should throw [error] when [invalid condition]', () => {
      // Arrange
      const invalidInput = createInvalidData();
      
      // Act & Assert
      expect(() => functionUnderTest(invalidInput)).toThrow(ExpectedError);
    });
  });
});
```

### 3. Coverage Requirements

**Minimum Thresholds**:
- **Overall**: 90%
- **Unit Tests**: 95%
- **Integration Tests**: 85%
- **Critical Paths**: 100%

**Coverage Configuration (package.json)**:
```json
{
  "jest": {
    "coverageThresholds": {
      "global": {
        "branches": 90,
        "functions": 90,
        "lines": 90,
        "statements": 90
      }
    }
  }
}
```

### 4. Test Frameworks by Language

| Language | Framework | Assertion | Mocking | Coverage |
|----------|-----------|-----------|---------|----------|
| TypeScript | Vitest, Jest | expect | vitest/fn | vitest |
| Python | pytest | assert | pytest-mock | pytest-cov |
| Rust | cargo test | assert! | mockall | tarpaulin |
| Java | JUnit 5 | Assertions | Mockito | JaCoCo |
| C# | xUnit, NUnit | Assert | Moq | coverlet |
| Go | testing | testing | gomock | go test |

---

## Documentation Standards

### 1. API Documentation Generation

| Language | Tool | Output |
|----------|------|--------|
| TypeScript | TypeDoc | HTML/JSON |
| Python | Sphinx, pdoc | HTML/PDF |
| Rust | rustdoc | HTML |
| Java | Javadoc | HTML |
| C# | DocFX | HTML |
| Go | godoc | HTML |

**Configuration Required**:
- Input source directories
- Output directory (`docs/api/`)
- Theme/template
- Exclusions (test files, internal APIs)

**Example (typedoc.json)**:
```json
{
  "entryPoints": ["src/index.ts"],
  "out": "docs/api",
  "exclude": ["**/*+(test|spec).ts", "**/__tests__/**"],
  "excludePrivate": true,
  "includeVersion": true,
  "readme": "README.md"
}
```

### 2. User Documentation

**Required Files**:
- `README.md` - Project overview, quick start
- `docs/user-guide/installation.md` - Installation instructions
- `docs/user-guide/getting-started.md` - Basic usage tutorial
- `docs/user-guide/configuration.md` - Configuration options
- `docs/user-guide/examples.md` - Usage examples
- `docs/user-guide/troubleshooting.md` - Common issues and solutions
- `docs/user-guide/faq.md` - Frequently asked questions

### 3. README.md Template

```markdown
# [Project Name]

[Brief description of the project]

[![CI](https://github.com/user/repo/workflows/CI/badge.svg)](https://github.com/user/repo/actions)
[![Coverage](https://codecov.io/gh/user/repo/branch/main/graph/badge.svg)](https://codecov.io/gh/user/repo)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

## Features

- Feature 1
- Feature 2
- Feature 3

## Installation

\`\`\`bash
npm install package-name
# or
pip install package-name
# or
cargo add package-name
\`\`\`

## Quick Start

\`\`\`typescript
import { Something } from 'package-name';

const instance = new Something();
const result = await instance.doSomething();
\`\`\`

## Documentation

- [API Documentation](docs/api/)
- [User Guide](docs/user-guide/)
- [Examples](examples/)

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md)

## License

[MIT](LICENSE)
```

---

## Best Practices Manual

Every language-specific manual MUST include a "Best Practices" section covering:

### 1. Language Idioms

**What to Include**:
- Preferred patterns for the language
- Idiomatic code examples
- Language-specific features to leverage

**Example Topics**:
- TypeScript: Use type inference, avoid `any`, prefer `const`
- Python: Use list comprehensions, context managers, generators
- Rust: Use ownership system, prefer `Result` over panics
- Java: Use streams, optional, try-with-resources

### 2. Anti-Patterns

**What to Include**:
- Common mistakes
- Code smells specific to the language
- Performance pitfalls

**Example**:
```typescript
// ❌ BAD: Mutating parameters
function addItem(array: string[], item: string): void {
  array.push(item);
}

// ✅ GOOD: Return new array
function addItem(array: string[], item: string): string[] {
  return [...array, item];
}
```

### 3. Performance Optimization

**Topics to Cover**:
- Algorithmic complexity considerations
- Memory management
- Async/concurrency patterns
- Caching strategies
- Database query optimization

### 4. Security Best Practices

**Topics to Cover**:
- Input validation
- SQL injection prevention
- XSS prevention
- Authentication/authorization
- Secrets management
- Dependency security

### 5. Error Handling

**Topics to Cover**:
- When to throw vs return errors
- Error types/hierarchy
- Error logging
- User-facing error messages
- Recovery strategies

### 6. Code Organization

**Topics to Cover**:
- When to split files/modules
- Dependency injection
- Separation of concerns
- SOLID principles application
- Design patterns for the language

---

## Language-Specific Requirements

### TypeScript/JavaScript

**Additional Requirements**:
- `package.json` with all scripts
- `tsconfig.json` (TypeScript only)
- `.eslintrc.json` + `.prettierrc.json`
- `.nvmrc` or `.node-version`
- TypeDoc configuration
- Vitest/Jest configuration

**Specific Standards**:
- Use `strict` mode in TypeScript
- Prefer `const` over `let`
- Use async/await over promises
- No `any` types
- Explicit return types for functions
- Use ES modules

### Python

**Additional Requirements**:
- `requirements.txt` or `pyproject.toml`
- `.python-version` or `.tool-versions`
- Type stubs (`py.typed`)
- `mypy.ini` or `pyrightconfig.json`
- `.ruff.toml` or equivalent
- Sphinx or pdoc configuration

**Specific Standards**:
- Follow PEP 8
- Use type hints (PEP 484)
- Docstrings for all public APIs (PEP 257)
- Use dataclasses or Pydantic for models
- Async with asyncio

### Rust

**Additional Requirements**:
- `Cargo.toml` with metadata
- `rust-toolchain.toml`
- `clippy.toml`
- `rustfmt.toml`
- rustdoc configuration
- cargo-deny configuration

**Specific Standards**:
- Follow Rust API guidelines
- Use `Result` instead of panics
- Implement `Display` and `Error` for errors
- Use `#[derive]` where possible
- Document all public items
- Use Rust 2021 edition

### Java

**Additional Requirements**:
- `pom.xml` or `build.gradle`
- `checkstyle.xml`
- `spotbugs.xml`
- Javadoc configuration
- JUnit 5 configuration

**Specific Standards**:
- Follow Google Java Style Guide
- Use Optional for nullable returns
- Use streams for collections
- Implement equals/hashCode properly
- Use try-with-resources
- Java 17+ features

---

## Checklist for Language-Specific Manual Creation

- [ ] All mandatory components documented
- [ ] Configuration section complete with examples
- [ ] Source code standards defined
- [ ] Testing standards specified
- [ ] Documentation generation explained
- [ ] Best practices section comprehensive
- [ ] Language-specific patterns documented
- [ ] Anti-patterns identified
- [ ] Performance guidelines included
- [ ] Security practices covered
- [ ] Examples for all patterns
- [ ] CI/CD workflow adapted
- [ ] All templates adapted for language
- [ ] Naming conventions specified
- [ ] Directory structure defined
- [ ] Tool configurations provided

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-10-11 | Initial standards document |

---

**Maintained by**: HiveLLM Governance Team  
**Last Review**: 2025-10-11  
**License**: MIT

