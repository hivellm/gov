# AI Integration Manual - TypeScript

**Version**: 1.0.0  
**Last Updated**: 2025-10-11  
**Language**: TypeScript 5.x / Node.js 20.x  
**Target Audience**: AI Agents (LLM-based development assistants)

---

## Table of Contents

1. [Introduction](#introduction)
2. [Quick Start](#quick-start)
3. [TypeScript-Specific Setup](#typescript-specific-setup)
4. [Configuration Standards](#configuration-standards)
5. [Source Code Standards](#source-code-standards)
6. [Testing Standards](#testing-standards)
7. [Build & Deployment](#build--deployment)
8. [Documentation](#documentation)
9. [TypeScript Best Practices](#typescript-best-practices)
10. [Common Patterns](#common-patterns)
11. [Troubleshooting](#troubleshooting)
12. [Complete Workflow](#complete-workflow)

---

## Introduction

This manual extends the [AI Integration Manual Template](../templates/AI_INTEGRATION_MANUAL_TEMPLATE.md) with TypeScript-specific implementations.

**When to use this manual**:
- TypeScript/Node.js projects
- Frontend (React, Vue, Angular, Svelte)
- Backend (Express, Fastify, NestJS)
- Full-stack applications
- CLI tools
- Libraries/SDKs

**Prerequisites**:
- Read [AI Integration Manual Template](../templates/AI_INTEGRATION_MANUAL_TEMPLATE.md)
- Read [Language Standards](../LANGUAGE_STANDARDS.md)
- Basic TypeScript knowledge

---

## Quick Start

### Recommended: CMMV Framework

**For TypeScript applications, we strongly recommend using the CMMV framework.**

CMMV (Contract-Model-Model-View) is a HiveLLM-native TypeScript framework that provides:

- **Contract-Driven Development**: Define TypeScript contracts that automatically generate APIs, controllers, ORM entities, and RPC communication
- **Modular Architecture**: Structure your application with built-in modules
- **RPC + REST Support**: Binary RPC via WebSocket and traditional REST APIs
- **Server-Side Rendering (SSR)**: Built-in SEO optimization
- **Full-Stack**: Backend (Node.js) and Frontend (Vue 3, React, Angular)
- **Built-in Modules**:
  - `@cmmv/auth` - Authentication with 2FA, JWT, sessions
  - `@cmmv/cache` - Redis/Memcached/MongoDB caching
  - `@cmmv/repository` - Auto-generated CRUD with TypeORM (SQLite, PostgreSQL, MySQL, MongoDB)
  - `@cmmv/queue` - Task queues (Kafka, RabbitMQ, Redis)
  - `@cmmv/elastic` - Elasticsearch integration
  - `@cmmv/email` - Email service (SMTP, AWS SES)
  - `@cmmv/encryptor` - ECC-based encryption, AES-256-GCM
  - `@cmmv/events` - Event-driven architecture
  - `@cmmv/inspector` - Debugging and monitoring
  - `@cmmv/ws` - WebSocket/RPC communication

**Quick Start with CMMV**:

```bash
# Install CMMV CLI
pnpm dlx @cmmv/cli@latest create my-project

# Follow the interactive setup:
# - Choose Vite (recommended)
# - Enable RPC (for high-performance APIs)
# - Enable Cache (Redis/Memcached)
# - Choose Repository type (PostgreSQL/MySQL/SQLite/MongoDB)
# - Choose View (Vue 3 / React / None)

# Navigate to project
cd my-project

# Install dependencies
pnpm install

# Start development server
pnpm dev
```

**Documentation**: https://cmmv.io

**Why CMMV for HiveLLM Projects**:
- Automatically generates boilerplate code from contracts
- Enforces consistent architecture across projects
- Integrates seamlessly with Task Queue and Vectorizer
- Built-in support for multi-language (i18n)
- Optimized for performance and SEO
- Full TypeScript type safety
- Reduces development time by 50-70%
- Built-in testing support with mocks

### Alternative: Manual Setup

If CMMV doesn't fit your needs, you can set up a basic TypeScript project manually:

```bash
# 1. Initialize project
mkdir my-project && cd my-project
npm init -y

# 2. Install TypeScript and essential dependencies
npm install -D typescript @types/node tsx

# 3. Create tsconfig.json
npx tsc --init --strict

# 4. Create basic structure
mkdir -p src tests docs

# 5. Create entry point
echo "console.log('Hello, TypeScript!');" > src/index.ts

# 6. Add scripts to package.json
npm pkg set scripts.build="tsc"
npm pkg set scripts.dev="tsx watch src/index.ts"
npm pkg set scripts.start="node dist/index.js"

# 7. Test it works
npm run dev
```

---

## CMMV Framework Integration

### What is CMMV?

CMMV (Contract-Model-Model-View) is a revolutionary TypeScript framework for building scalable, modular applications using a contract-driven approach. It's the **recommended framework for all HiveLLM TypeScript projects**.

### Key Concepts

#### 1. Contract-Driven Development

Define your application logic in TypeScript contracts, and CMMV automatically generates:
- REST API endpoints
- RPC services (Protobuf)
- Database entities (TypeORM)
- Frontend models and types
- Validation logic
- API documentation (Swagger)

#### 2. Architecture Layers

```
┌─────────────────────────────────────────┐
│         Frontend (View Layer)            │
│  Vue 3 / React / Angular / Vanilla      │
└────────────┬────────────────────────────┘
             │ HTTP / WebSocket RPC
┌────────────┴────────────────────────────┐
│      Application Layer (CMMV Core)      │
│  Contracts → Auto-Generated Code        │
├─────────────────────────────────────────┤
│  Controllers │ Services │ Repositories  │
├─────────────────────────────────────────┤
│  Auth │ Cache │ Queue │ Events │ etc.  │
└────────────┬────────────────────────────┘
             │
┌────────────┴────────────────────────────┐
│    Infrastructure Layer                 │
│  Database │ Redis │ RabbitMQ │ etc.    │
└─────────────────────────────────────────┘
```

### When to Use CMMV

**Use CMMV when**:
- Building web applications (frontend + backend)
- Need automatic CRUD generation
- Want RPC + REST support
- Require SSR for SEO
- Building admin panels or dashboards
- Need multi-language support (i18n)
- Want to reduce boilerplate code
- Working on HiveLLM ecosystem projects

**Consider alternatives when**:
- Building simple CLI tools
- Pure libraries without HTTP/database
- Microservices with very specific needs
- Projects requiring non-standard architecture

### CMMV Project Structure

**IMPORTANT**: The CMMV CLI automatically generates the correct project structure. 

After running `pnpm dlx @cmmv/cli@latest create my-project`, the CLI will create:
- Application structure based on your choices (RPC, Cache, Repository type, View)
- Necessary configuration files
- Module organization
- Contract examples
- Entry points

**Follow the official CMMV documentation** for the exact structure:
- **Documentation**: https://cmmv.io
- **Vectorizer Collections**: Search `cmmv-core-docs` and `cmmv-typescript-*` collections
- The structure varies based on which modules you enable during CLI setup

**Typical CMMV project includes**:
- `src/` - Application source code
  - Modules (app.module.ts, feature.module.ts)
  - Controllers (auto-generated from contracts)
  - Services (business logic)
  - Contracts (schema definitions)
- `.cmmvrc.json` - CMMV configuration
- `package.json` - Dependencies and scripts
- `tsconfig.json` - TypeScript configuration

### CMMV Configuration

Configuration varies based on enabled modules. Refer to CMMV documentation at https://cmmv.io for:
- Repository configuration (database connection)
- Cache configuration (Redis/Memcached)
- RPC configuration (Protobuf settings)
- View configuration (SSR settings)
- i18n configuration (localization)

### CMMV Contract-Driven Development

CMMV uses TypeScript contracts to automatically generate code. Search the Vectorizer for "CMMV contracts" to find examples and detailed documentation.

### CMMV vs Manual Setup

| Feature | CMMV | Manual Setup |
|---------|------|--------------|
| **CRUD Generation** | Automatic | Manual |
| **API Documentation** | Auto (Swagger) | Manual (TypeDoc) |
| **RPC Support** | Built-in (Protobuf) | Manual implementation |
| **SSR** | Built-in | Configure separately |
| **i18n** | Built-in | Add libraries |
| **Auth** | Module (@cmmv/auth) | Build from scratch |
| **Caching** | Module (@cmmv/cache) | Configure Redis manually |
| **Development Time** | Fast (50-70% reduction) | Standard |
| **Learning Curve** | Moderate | Low |
| **Flexibility** | High (modular) | Very High |

### CMMV Learning Resources

**⚠️ IMPORTANT**: For accurate CMMV implementation details, always refer to:

1. **Official Documentation**: https://cmmv.io (primary source)
2. **Vectorizer Search**: Search collections starting with `cmmv-` for:
   - `cmmv-core-docs` - Core framework documentation
   - `cmmv-core-source` - Framework source code
   - `cmmv-typescript-starter-source` - Starter project structure
   - `cmmv-*-docs` - Module-specific documentation
3. **GitHub Repository**: https://github.com/cmmvio/cmmv
4. **CLI Help**: Run `pnpm dlx @cmmv/cli@latest --help`

**Before implementing with CMMV**:
- [ ] Search Vectorizer for "CMMV [feature you need]"
- [ ] Read official documentation for that feature
- [ ] Check starter projects in Vectorizer
- [ ] Use CLI to generate project structure (don't create manually)
- [ ] Follow generated structure, don't modify unless necessary

---

## TypeScript-Specific Setup

### 1. Environment Setup

#### Install Node.js Version Manager

**Using nvm (Recommended)**:
```bash
# Install nvm (Unix/Mac)
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.0/install.sh | bash

# Install nvm (Windows)
# Download and install from: https://github.com/coreybutler/nvm-windows

# Install Node.js 20 (LTS)
nvm install 20
nvm use 20
nvm alias default 20

# Verify installation
node --version  # Should show v20.x.x
npm --version   # Should show 10.x.x
```

#### Create .nvmrc file

```bash
echo "20" > .nvmrc
```

Now anyone can just run `nvm use` to get the correct version.

### 2. Package Manager Selection

**Recommended**: Choose **ONE** package manager for consistency.

| Manager | Speed | Workspace Support | Lock File | Notes |
|---------|-------|-------------------|-----------|-------|
| **npm** | Good | Yes | `package-lock.json` | Default, widely supported |
| **pnpm** | Excellent | Yes | `pnpm-lock.yaml` | Fastest, saves disk space |
| **yarn** | Good | Yes | `yarn.lock` | Good for monorepos |

**This manual uses npm**, but commands are easily adaptable:

```bash
npm install    →  pnpm install    →  yarn install
npm add pkg    →  pnpm add pkg    →  yarn add pkg
npm run build  →  pnpm build      →  yarn build
```

### 3. Essential Dependencies

#### Production Dependencies

```bash
# None required for basic project
# Add as needed for your project
```

#### Development Dependencies

```bash
# TypeScript and type definitions
npm install -D typescript @types/node

# Build and development
npm install -D tsx              # Fast TypeScript execution
npm install -D ts-node          # Alternative TypeScript runtime

# Testing
npm install -D vitest          # Test framework
npm install -D @vitest/coverage-v8  # Coverage

# Linting and formatting
npm install -D eslint @eslint/js @types/eslint__js typescript-eslint
npm install -D prettier

# Git hooks
npm install -D husky lint-staged

# Additional types (as needed)
npm install -D @types/express  # If using Express
npm install -D @types/jest     # If using Jest
```

---

## Configuration Standards

### 1. package.json

**Mandatory Fields**:

```json
{
  "name": "project-name",
  "version": "1.0.0",
  "description": "Project description",
  "main": "dist/index.js",
  "types": "dist/index.d.ts",
  "type": "module",
  "engines": {
    "node": ">=20.0.0",
    "npm": ">=10.0.0"
  },
  "scripts": {
    "dev": "tsx watch src/index.ts",
    "build": "tsc",
    "build:clean": "rm -rf dist && npm run build",
    "start": "node dist/index.js",
    "test": "vitest run",
    "test:watch": "vitest",
    "test:coverage": "vitest run --coverage",
    "test:ui": "vitest --ui",
    "lint": "eslint src/",
    "lint:fix": "eslint src/ --fix",
    "format": "prettier --write \"src/**/*.ts\"",
    "format:check": "prettier --check \"src/**/*.ts\"",
    "type-check": "tsc --noEmit",
    "clean": "rm -rf dist coverage .turbo node_modules",
    "prepare": "husky install"
  },
  "keywords": ["typescript", "nodejs"],
  "author": "Your Name",
  "license": "MIT",
  "repository": {
    "type": "git",
    "url": "https://github.com/user/repo.git"
  },
  "bugs": {
    "url": "https://github.com/user/repo/issues"
  },
  "homepage": "https://github.com/user/repo#readme",
  "files": [
    "dist",
    "README.md",
    "LICENSE"
  ]
}
```

### 2. tsconfig.json

**Strict Configuration (Recommended)**:

```json
{
  "compilerOptions": {
    /* Language and Environment */
    "target": "ES2022",
    "lib": ["ES2022"],
    "module": "NodeNext",
    "moduleResolution": "NodeNext",
    
    /* Emit */
    "outDir": "./dist",
    "rootDir": "./src",
    "declaration": true,
    "declarationMap": true,
    "sourceMap": true,
    "removeComments": false,
    "importHelpers": true,
    "downlevelIteration": true,
    
    /* Interop Constraints */
    "esModuleInterop": true,
    "allowSyntheticDefaultImports": true,
    "forceConsistentCasingInFileNames": true,
    "isolatedModules": true,
    
    /* Type Checking */
    "strict": true,
    "noUnusedLocals": true,
    "noUnusedParameters": true,
    "noImplicitReturns": true,
    "noFallthroughCasesInSwitch": true,
    "noUncheckedIndexedAccess": true,
    "noImplicitOverride": true,
    "noPropertyAccessFromIndexSignature": true,
    "exactOptionalPropertyTypes": true,
    
    /* Completeness */
    "skipLibCheck": true,
    "resolveJsonModule": true
  },
  "include": ["src/**/*"],
  "exclude": [
    "node_modules",
    "dist",
    "tests",
    "**/*.spec.ts",
    "**/*.test.ts"
  ]
}
```

**For Tests** (tsconfig.test.json):

```json
{
  "extends": "./tsconfig.json",
  "compilerOptions": {
    "types": ["vitest/globals", "node"]
  },
  "include": ["tests/**/*", "src/**/*"]
}
```

### 3. ESLint Configuration

**eslint.config.js** (Flat Config - ESLint 9+):

```javascript
import js from '@eslint/js';
import tseslint from 'typescript-eslint';

export default tseslint.config(
  js.configs.recommended,
  ...tseslint.configs.strictTypeChecked,
  {
    languageOptions: {
      parserOptions: {
        project: true,
        tsconfigRootDir: import.meta.dirname,
      },
    },
  },
  {
    rules: {
      // Error Prevention
      'no-console': 'error',
      'no-debugger': 'error',
      '@typescript-eslint/no-unused-vars': ['error', {
        argsIgnorePattern: '^_',
        varsIgnorePattern: '^_',
      }],
      
      // Type Safety
      '@typescript-eslint/no-explicit-any': 'error',
      '@typescript-eslint/explicit-function-return-type': 'error',
      '@typescript-eslint/explicit-module-boundary-types': 'error',
      '@typescript-eslint/no-non-null-assertion': 'error',
      
      // Best Practices
      '@typescript-eslint/prefer-nullish-coalescing': 'error',
      '@typescript-eslint/prefer-optional-chain': 'error',
      '@typescript-eslint/no-floating-promises': 'error',
      '@typescript-eslint/await-thenable': 'error',
      
      // Code Quality
      'complexity': ['error', 10],
      'max-lines-per-function': ['error', 50],
      'max-depth': ['error', 3],
      'max-params': ['error', 4],
    },
  },
  {
    ignores: ['dist/', 'coverage/', 'node_modules/', '*.config.js'],
  }
);
```

### 4. Prettier Configuration

**.prettierrc.json**:

```json
{
  "semi": true,
  "trailingComma": "es5",
  "singleQuote": true,
  "printWidth": 100,
  "tabWidth": 2,
  "useTabs": false,
  "endOfLine": "lf",
  "arrowParens": "always",
  "bracketSpacing": true,
  "bracketSameLine": false
}
```

**.prettierignore**:

```
dist
coverage
node_modules
*.min.js
*.map
pnpm-lock.yaml
package-lock.json
```

### 5. Vitest Configuration

**vitest.config.ts**:

```typescript
import { defineConfig } from 'vitest/config';
import { resolve } from 'path';

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html', 'lcov'],
      exclude: [
        'node_modules/',
        'dist/',
        'tests/',
        '**/*.d.ts',
        '**/*.config.*',
        '**/mockData/**',
      ],
      thresholds: {
        lines: 90,
        functions: 90,
        branches: 90,
        statements: 90,
      },
    },
    include: ['tests/**/*.test.ts', 'tests/**/*.spec.ts'],
    exclude: ['node_modules', 'dist'],
  },
  resolve: {
    alias: {
      '@': resolve(__dirname, './src'),
      '@tests': resolve(__dirname, './tests'),
    },
  },
});
```

### 6. Git Hooks with Husky

```bash
# Initialize husky
npx husky init

# Create pre-commit hook
echo "npx lint-staged" > .husky/pre-commit

# Create commit-msg hook (optional)
echo "npx --no -- commitlint --edit \$1" > .husky/commit-msg
```

**package.json** (add lint-staged config):

```json
{
  "lint-staged": {
    "*.ts": [
      "eslint --fix",
      "prettier --write"
    ],
    "*.{json,md}": [
      "prettier --write"
    ]
  }
}
```

### 7. Editor Configuration

**.editorconfig**:

```ini
root = true

[*]
charset = utf-8
end_of_line = lf
insert_final_newline = true
trim_trailing_whitespace = true

[*.{js,ts,jsx,tsx,json}]
indent_style = space
indent_size = 2

[*.md]
trim_trailing_whitespace = false
```

### 8. Environment Configuration

**.env.example**:

```env
# Application
NODE_ENV=development
PORT=3000
LOG_LEVEL=debug

# Database
DATABASE_URL=postgresql://user:pass@localhost:5432/dbname
DATABASE_POOL_MIN=2
DATABASE_POOL_MAX=10

# Redis
REDIS_URL=redis://localhost:6379

# External APIs
API_KEY=your-api-key-here
API_SECRET=your-api-secret-here

# Task Queue
TASK_QUEUE_URL=http://localhost:8080
TASK_QUEUE_TOKEN=your-token-here

# Vectorizer
VECTORIZER_URL=http://localhost:9000
VECTORIZER_TOKEN=your-token-here
```

**Load environment variables** (src/config/env.ts):

```typescript
import { config } from 'dotenv';
import { z } from 'zod';

// Load .env file
config();

// Define schema
const envSchema = z.object({
  NODE_ENV: z.enum(['development', 'test', 'production']).default('development'),
  PORT: z.coerce.number().default(3000),
  LOG_LEVEL: z.enum(['debug', 'info', 'warn', 'error']).default('info'),
  DATABASE_URL: z.string().url(),
  REDIS_URL: z.string().url().optional(),
});

// Parse and validate
export const env = envSchema.parse(process.env);

// Type-safe environment
export type Env = z.infer<typeof envSchema>;
```

---

## Source Code Standards

### 1. Directory Structure

```
src/
├── core/                   # Core business logic
│   ├── entities/          # Domain entities
│   ├── use-cases/         # Use case implementations
│   └── interfaces/        # Core interfaces
├── infrastructure/        # External concerns
│   ├── database/         # Database implementation
│   ├── http/             # HTTP server
│   └── queue/            # Message queue
├── api/                   # API layer
│   ├── routes/           # Route definitions
│   ├── controllers/      # Request handlers
│   ├── middleware/       # Express middleware
│   └── validators/       # Request validation
├── services/             # Application services
├── repositories/         # Data access layer
├── models/              # Data models/DTOs
├── utils/               # Utility functions
├── types/               # TypeScript type definitions
├── config/              # Configuration
│   ├── env.ts          # Environment variables
│   └── constants.ts    # Constants
└── index.ts            # Application entry point
```

### 2. Naming Conventions

| Element | Convention | Example |
|---------|-----------|---------|
| **Files** | kebab-case | `user-service.ts` |
| **Classes** | PascalCase | `UserService` |
| **Interfaces** | PascalCase (no I prefix) | `User`, `UserRepository` |
| **Types** | PascalCase | `UserId`, `CreateUserDto` |
| **Functions** | camelCase | `createUser()` |
| **Variables** | camelCase | `userId` |
| **Constants** | UPPER_SNAKE_CASE | `MAX_RETRY_COUNT` |
| **Enums** | PascalCase | `UserRole` |
| **Enum Members** | PascalCase | `UserRole.Admin` |

### 3. Code Organization Patterns

#### Clean Architecture Structure

```typescript
// core/entities/user.ts
export interface User {
  readonly id: string;
  email: string;
  name: string;
  createdAt: Date;
  updatedAt: Date;
}

export interface CreateUserData {
  email: string;
  name: string;
}
```

```typescript
// core/interfaces/user-repository.ts
import { User, CreateUserData } from '../entities/user';

export interface UserRepository {
  findById(id: string): Promise<User | null>;
  findByEmail(email: string): Promise<User | null>;
  create(data: CreateUserData): Promise<User>;
  update(id: string, data: Partial<User>): Promise<User>;
  delete(id: string): Promise<void>;
}
```

```typescript
// core/use-cases/create-user.ts
import { UserRepository } from '../interfaces/user-repository';
import { CreateUserData, User } from '../entities/user';
import { ValidationError } from '../../utils/errors';

export class CreateUserUseCase {
  constructor(private readonly userRepository: UserRepository) {}

  async execute(data: CreateUserData): Promise<User> {
    // Validate
    await this.validate(data);
    
    // Check if email already exists
    const existingUser = await this.userRepository.findByEmail(data.email);
    if (existingUser) {
      throw new ValidationError('Email already in use');
    }
    
    // Create user
    return this.userRepository.create(data);
  }

  private async validate(data: CreateUserData): Promise<void> {
    if (!data.email || !data.email.includes('@')) {
      throw new ValidationError('Invalid email');
    }
    if (!data.name || data.name.length < 2) {
      throw new ValidationError('Name must be at least 2 characters');
    }
  }
}
```

#### Service Layer

```typescript
// services/user.service.ts
import { injectable, inject } from 'tsyringe';
import { UserRepository } from '../repositories/user.repository';
import { CreateUserDto, UserDto } from '../models/user.dto';
import { User } from '../core/entities/user';

@injectable()
export class UserService {
  constructor(
    @inject('UserRepository') private readonly userRepository: UserRepository
  ) {}

  /**
   * Creates a new user
   * @param data User creation data
   * @returns Created user
   * @throws ValidationError if data is invalid
   * @throws ConflictError if email already exists
   */
  async createUser(data: CreateUserDto): Promise<UserDto> {
    const user = await this.userRepository.create(data);
    return this.toDto(user);
  }

  async getUserById(id: string): Promise<UserDto | null> {
    const user = await this.userRepository.findById(id);
    return user ? this.toDto(user) : null;
  }

  private toDto(user: User): UserDto {
    return {
      id: user.id,
      email: user.email,
      name: user.name,
      createdAt: user.createdAt.toISOString(),
    };
  }
}
```

#### Repository Pattern

```typescript
// repositories/user.repository.ts
import { PrismaClient } from '@prisma/client';
import { UserRepository } from '../core/interfaces/user-repository';
import { User, CreateUserData } from '../core/entities/user';

export class PrismaUserRepository implements UserRepository {
  constructor(private readonly prisma: PrismaClient) {}

  async findById(id: string): Promise<User | null> {
    return this.prisma.user.findUnique({ where: { id } });
  }

  async findByEmail(email: string): Promise<User | null> {
    return this.prisma.user.findUnique({ where: { email } });
  }

  async create(data: CreateUserData): Promise<User> {
    return this.prisma.user.create({ data });
  }

  async update(id: string, data: Partial<User>): Promise<User> {
    return this.prisma.user.update({ where: { id }, data });
  }

  async delete(id: string): Promise<void> {
    await this.prisma.user.delete({ where: { id } });
  }
}
```

### 4. Error Handling

**Custom Error Classes**:

```typescript
// utils/errors.ts
export class AppError extends Error {
  constructor(
    message: string,
    public readonly code: string,
    public readonly statusCode: number = 500,
    public readonly isOperational: boolean = true
  ) {
    super(message);
    this.name = this.constructor.name;
    Error.captureStackTrace(this, this.constructor);
  }
}

export class ValidationError extends AppError {
  constructor(message: string) {
    super(message, 'VALIDATION_ERROR', 400);
  }
}

export class NotFoundError extends AppError {
  constructor(resource: string, id: string) {
    super(`${resource} with id ${id} not found`, 'NOT_FOUND', 404);
  }
}

export class UnauthorizedError extends AppError {
  constructor(message: string = 'Unauthorized') {
    super(message, 'UNAUTHORIZED', 401);
  }
}

export class ConflictError extends AppError {
  constructor(message: string) {
    super(message, 'CONFLICT', 409);
  }
}
```

**Error Handler Middleware** (Express):

```typescript
// api/middleware/error-handler.ts
import { Request, Response, NextFunction } from 'express';
import { AppError } from '../../utils/errors';
import { logger } from '../../utils/logger';

export function errorHandler(
  error: Error,
  req: Request,
  res: Response,
  next: NextFunction
): void {
  if (error instanceof AppError) {
    logger.warn('Operational error', {
      code: error.code,
      message: error.message,
      statusCode: error.statusCode,
      path: req.path,
    });

    res.status(error.statusCode).json({
      error: error.code,
      message: error.message,
    });
    return;
  }

  // Unhandled errors
  logger.error('Unhandled error', {
    message: error.message,
    stack: error.stack,
    path: req.path,
  });

  res.status(500).json({
    error: 'INTERNAL_SERVER_ERROR',
    message: 'An unexpected error occurred',
  });
}
```

### 5. Type Definitions

**Use Type Aliases for Primitives**:

```typescript
// types/brand.ts
type Brand<K, T> = K & { __brand: T };

export type UserId = Brand<string, 'UserId'>;
export type Email = Brand<string, 'Email'>;
export type Timestamp = Brand<number, 'Timestamp'>;

// Usage
function getUserById(id: UserId): Promise<User> {
  // ...
}

// Type-safe creation
const userId = 'user-123' as UserId;  // Explicit cast
```

**Discriminated Unions**:

```typescript
// types/result.ts
export type Result<T, E = Error> = 
  | { success: true; data: T }
  | { success: false; error: E };

// Usage
function divide(a: number, b: number): Result<number, string> {
  if (b === 0) {
    return { success: false, error: 'Division by zero' };
  }
  return { success: true, data: a / b };
}

const result = divide(10, 2);
if (result.success) {
  console.log(result.data);  // TypeScript knows this exists
} else {
  console.error(result.error);  // TypeScript knows this exists
}
```

**Utility Types**:

```typescript
// types/utils.ts
export type Nullable<T> = T | null;
export type Optional<T> = T | undefined;
export type Maybe<T> = T | null | undefined;

export type DeepPartial<T> = {
  [P in keyof T]?: T[P] extends object ? DeepPartial<T[P]> : T[P];
};

export type DeepReadonly<T> = {
  readonly [P in keyof T]: T[P] extends object ? DeepReadonly<T[P]> : T[P];
};

export type Awaited<T> = T extends Promise<infer U> ? U : T;

export type ValueOf<T> = T[keyof T];
```

---

## Testing Standards

### 1. Test Structure

```
tests/
├── unit/                           # Unit tests
│   ├── services/
│   │   └── user.service.test.ts
│   ├── utils/
│   │   └── validators.test.ts
│   └── use-cases/
│       └── create-user.test.ts
├── integration/                    # Integration tests
│   ├── api/
│   │   └── users.api.test.ts
│   └── repositories/
│       └── user.repository.test.ts
├── e2e/                           # End-to-end tests
│   └── user-flows.test.ts
├── fixtures/                       # Test data
│   ├── users.ts
│   └── products.ts
└── helpers/                        # Test utilities
    ├── setup.ts
    ├── factories.ts
    └── mocks.ts
```

### 2. Unit Test Example

```typescript
// tests/unit/services/user.service.test.ts
import { describe, it, expect, vi, beforeEach } from 'vitest';
import { UserService } from '../../../src/services/user.service';
import { UserRepository } from '../../../src/repositories/user.repository';
import { ValidationError } from '../../../src/utils/errors';
import { createMockUser } from '../../helpers/factories';

describe('UserService', () => {
  let userService: UserService;
  let mockUserRepository: UserRepository;

  beforeEach(() => {
    mockUserRepository = {
      findById: vi.fn(),
      findByEmail: vi.fn(),
      create: vi.fn(),
      update: vi.fn(),
      delete: vi.fn(),
    };
    userService = new UserService(mockUserRepository);
  });

  describe('createUser', () => {
    it('should create a user successfully', async () => {
      // Arrange
      const input = {
        email: 'test@example.com',
        name: 'Test User',
      };
      const expected = createMockUser(input);
      
      vi.mocked(mockUserRepository.findByEmail).mockResolvedValue(null);
      vi.mocked(mockUserRepository.create).mockResolvedValue(expected);

      // Act
      const result = await userService.createUser(input);

      // Assert
      expect(result).toEqual(expected);
      expect(mockUserRepository.findByEmail).toHaveBeenCalledWith(input.email);
      expect(mockUserRepository.create).toHaveBeenCalledWith(input);
    });

    it('should throw ValidationError when email is invalid', async () => {
      // Arrange
      const input = {
        email: 'invalid-email',
        name: 'Test User',
      };

      // Act & Assert
      await expect(userService.createUser(input)).rejects.toThrow(ValidationError);
      expect(mockUserRepository.create).not.toHaveBeenCalled();
    });

    it('should throw ConflictError when email already exists', async () => {
      // Arrange
      const input = {
        email: 'existing@example.com',
        name: 'Test User',
      };
      
      vi.mocked(mockUserRepository.findByEmail).mockResolvedValue(
        createMockUser({ email: input.email })
      );

      // Act & Assert
      await expect(userService.createUser(input)).rejects.toThrow('Email already in use');
      expect(mockUserRepository.create).not.toHaveBeenCalled();
    });
  });
});
```

### 3. Test Helpers

```typescript
// tests/helpers/factories.ts
import { User } from '../../src/core/entities/user';
import { faker } from '@faker-js/faker';

export function createMockUser(overrides?: Partial<User>): User {
  return {
    id: faker.string.uuid(),
    email: faker.internet.email(),
    name: faker.person.fullName(),
    createdAt: new Date(),
    updatedAt: new Date(),
    ...overrides,
  };
}

export function createMockUserList(count: number): User[] {
  return Array.from({ length: count }, () => createMockUser());
}
```

```typescript
// tests/helpers/mocks.ts
import { vi } from 'vitest';
import { UserRepository } from '../../src/core/interfaces/user-repository';

export function createMockUserRepository(): UserRepository {
  return {
    findById: vi.fn(),
    findByEmail: vi.fn(),
    create: vi.fn(),
    update: vi.fn(),
    delete: vi.fn(),
  };
}
```

### 4. Integration Test Example

```typescript
// tests/integration/api/users.api.test.ts
import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import request from 'supertest';
import { app } from '../../../src/app';
import { prisma } from '../../../src/infrastructure/database/prisma';

describe('Users API', () => {
  beforeAll(async () => {
    // Setup: Clear database and seed test data
    await prisma.user.deleteMany();
  });

  afterAll(async () => {
    // Cleanup
    await prisma.user.deleteMany();
    await prisma.$disconnect();
  });

  describe('POST /api/users', () => {
    it('should create a new user', async () => {
      // Arrange
      const userData = {
        email: 'newuser@example.com',
        name: 'New User',
      };

      // Act
      const response = await request(app)
        .post('/api/users')
        .send(userData)
        .expect(201);

      // Assert
      expect(response.body).toMatchObject({
        id: expect.any(String),
        email: userData.email,
        name: userData.name,
        createdAt: expect.any(String),
      });

      // Verify in database
      const user = await prisma.user.findUnique({
        where: { email: userData.email },
      });
      expect(user).not.toBeNull();
    });

    it('should return 400 for invalid email', async () => {
      // Arrange
      const userData = {
        email: 'invalid-email',
        name: 'Test User',
      };

      // Act
      const response = await request(app)
        .post('/api/users')
        .send(userData)
        .expect(400);

      // Assert
      expect(response.body).toMatchObject({
        error: 'VALIDATION_ERROR',
        message: expect.stringContaining('email'),
      });
    });
  });
});
```

---

## Build & Deployment

### 1. Build Process

```bash
# Clean build
npm run build:clean

# Build output structure
dist/
├── index.js
├── index.d.ts
├── index.js.map
├── core/
├── services/
└── ...
```

### 2. Production Optimization

**tsconfig.prod.json**:

```json
{
  "extends": "./tsconfig.json",
  "compilerOptions": {
    "sourceMap": false,
    "declarationMap": false,
    "removeComments": true
  }
}
```

**Build script**:

```json
{
  "scripts": {
    "build:prod": "tsc -p tsconfig.prod.json"
  }
}
```

### 3. Docker Support

**Dockerfile**:

```dockerfile
# Build stage
FROM node:20-alpine AS builder
WORKDIR /app
COPY package*.json ./
RUN npm ci
COPY . .
RUN npm run build

# Production stage
FROM node:20-alpine
WORKDIR /app
ENV NODE_ENV=production
COPY package*.json ./
RUN npm ci --only=production
COPY --from=builder /app/dist ./dist
EXPOSE 3000
CMD ["node", "dist/index.js"]
```

**.dockerignore**:

```
node_modules
dist
coverage
.git
.env
*.log
```

### 4. npm Package Publishing

```bash
# Login to npm
npm login

# Update version
npm version patch|minor|major

# Publish
npm publish --access public
```

**Before publishing checklist**:
- [ ] All tests passing
- [ ] Build successful
- [ ] CHANGELOG updated
- [ ] README updated
- [ ] Version bumped
- [ ] Git tag created

---

## Documentation

### 1. TSDoc/JSDoc

```typescript
/**
 * Processes a payment transaction
 * 
 * @param userId - The unique identifier of the user
 * @param amount - Payment amount in cents (must be positive)
 * @param currency - ISO 4217 currency code (e.g., 'USD', 'EUR')
 * @returns Payment confirmation with transaction details
 * @throws {ValidationError} If amount is negative or currency is invalid
 * @throws {InsufficientFundsError} If user balance is insufficient
 * @throws {PaymentGatewayError} If payment gateway is unavailable
 * 
 * @example
 * ```typescript
 * const result = await processPayment('user-123', 1000, 'USD');
 * console.log(`Transaction ID: ${result.transactionId}`);
 * ```
 * 
 * @since 1.0.0
 * @see {@link PaymentResult} for return type details
 */
export async function processPayment(
  userId: string,
  amount: number,
  currency: string
): Promise<PaymentResult> {
  // Implementation
}
```

### 2. TypeDoc Configuration

**typedoc.json**:

```json
{
  "entryPoints": ["src/index.ts"],
  "out": "docs/api",
  "exclude": [
    "**/*+(test|spec).ts",
    "**/__tests__/**",
    "**/node_modules/**"
  ],
  "excludePrivate": true,
  "excludeProtected": false,
  "excludeInternal": true,
  "includeVersion": true,
  "readme": "README.md",
  "plugin": ["typedoc-plugin-markdown"],
  "theme": "default"
}
```

**Generate documentation**:

```bash
npm install -D typedoc typedoc-plugin-markdown
npm pkg set scripts.docs="typedoc"
npm pkg set scripts.docs:serve="npx http-server docs/api"

npm run docs
```

---

## TypeScript Best Practices

See [BEST_PRACTICES.md](BEST_PRACTICES.md) for complete guide.

### Quick Tips

1. **Use `const` by default**, `let` only when needed, never `var`
2. **Enable `strict` mode** in tsconfig.json
3. **Avoid `any`** - use `unknown` instead
4. **Use explicit return types** for functions
5. **Prefer interfaces over type aliases** for object shapes
6. **Use enums sparingly** - prefer union types
7. **Use `readonly` for immutable data**
8. **Use async/await** over promises
9. **Use optional chaining** (`?.`) and nullish coalescing (`??`)
10. **Use template literal types** for type safety

---

## Common Patterns

Refer to [Source Code Standards](#source-code-standards) section for:
- Repository Pattern
- Service Layer Pattern
- Dependency Injection
- Error Handling

For additional TypeScript patterns and best practices, see [BEST_PRACTICES.md](BEST_PRACTICES.md).

---

## Troubleshooting

### Common TypeScript Issues

#### Issue: "Cannot find module"

**Solution**:
```bash
npm install @types/[module-name]
```

#### Issue: "Type 'X' is not assignable to type 'Y'"

**Solution**: Check type compatibility, use type assertions if needed:
```typescript
const value = unknownValue as KnownType;
// or
const value = <KnownType>unknownValue;
```

#### Issue: Build is slow

**Solutions**:
1. Enable incremental builds in tsconfig.json
2. Use `tsx` for development instead of `ts-node`
3. Exclude unnecessary files from compilation

---

## Complete Workflow

Follow the [AI Integration Manual Template](../templates/AI_INTEGRATION_MANUAL_TEMPLATE.md) for the complete 6-phase workflow, using TypeScript-specific configurations and commands from this manual.

### Quick Reference

1. **Planning**: Use templates from `gov/manuals/templates/`
2. **Setup**: Follow [TypeScript-Specific Setup](#typescript-specific-setup)
3. **Implementation**: Follow [Source Code Standards](#source-code-standards)
4. **Testing**: Follow [Testing Standards](#testing-standards)
5. **Build**: Follow [Build & Deployment](#build--deployment)
6. **Documentation**: Follow [Documentation](#documentation)

---

## Additional Resources

- [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/intro.html)
- [TypeScript Deep Dive](https://basarat.gitbook.io/typescript/)
- [Node.js Best Practices](https://github.com/goldbergyoni/nodebestpractices)
- [Clean Code TypeScript](https://github.com/labs42io/clean-code-typescript)
- [BEST_PRACTICES.md](BEST_PRACTICES.md) - This project's best practices

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-10-11 | Initial TypeScript manual |

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

