# AI Integration Manual - JavaScript

**Version**: 1.0.0  
**Last Updated**: 2025-10-11  
**Language**: JavaScript (ES2015+/ES6+)  
**Target Audience**: AI Agents (LLM-based development assistants)

---

## Table of Contents

1. [Introduction](#introduction)
2. [Quick Start](#quick-start)
3. [JavaScript-Specific Setup](#javascript-specific-setup)
4. [Configuration Standards](#configuration-standards)
5. [Source Code Standards](#source-code-standards)
6. [Testing Standards](#testing-standards)
7. [Build & Deployment](#build--deployment)
8. [Documentation](#documentation)
9. [JavaScript Best Practices](#javascript-best-practices)
10. [Common Patterns](#common-patterns)
11. [Troubleshooting](#troubleshooting)
12. [Complete Workflow](#complete-workflow)

---

## Introduction

This manual extends the [AI Integration Manual Template](../templates/AI_INTEGRATION_MANUAL_TEMPLATE.md) with JavaScript-specific implementations.

**When to use this manual**:
- Node.js backend services and APIs
- Frontend applications (React, Vue, Angular)
- Full-stack JavaScript applications
- CLI tools and utilities
- Serverless functions (AWS Lambda, Vercel)
- Real-time applications (WebSockets, Socket.io)
- Libraries and npm packages
- Build tools and automation scripts

**Prerequisites**:
- Read [AI Integration Manual Template](../templates/AI_INTEGRATION_MANUAL_TEMPLATE.md)
- Read [Language Standards](../LANGUAGE_STANDARDS.md)
- Basic JavaScript knowledge

**Note**: For TypeScript projects, prefer the [TypeScript Manual](../typescript/AI_INTEGRATION_MANUAL_TYPESCRIPT.md) which includes type safety and better tooling. This manual focuses on pure JavaScript (ES6+) projects.

---

## Quick Start

### Minimum Viable Node.js Project

```bash
# 1. Create project directory
mkdir my-project && cd my-project

# 2. Initialize npm
npm init -y

# 3. Create entry point
cat > index.js << 'EOF'
console.log('Hello, World!');
EOF

# 4. Run
node index.js
```

### Express API Quick Start

```bash
# Create project
mkdir my-api && cd my-api
npm init -y

# Install dependencies
npm install express

# Create server
cat > server.js << 'EOF'
const express = require('express');
const app = express();

app.use(express.json());

app.get('/', (req, res) => {
  res.json({ message: 'Hello, World!' });
});

const PORT = process.env.PORT || 3000;
app.listen(PORT, () => {
  console.log(`Server running on port ${PORT}`);
});
EOF

# Run
node server.js
```

### Modern ES Modules Project

```bash
# Create project
mkdir my-esm-project && cd my-esm-project
npm init -y

# Enable ESM
npm pkg set type="module"

# Create entry point
cat > index.js << 'EOF'
import { createServer } from 'http';

const server = createServer((req, res) => {
  res.writeHead(200, { 'Content-Type': 'application/json' });
  res.end(JSON.stringify({ message: 'Hello from ESM!' }));
});

server.listen(3000, () => {
  console.log('Server running on http://localhost:3000');
});
EOF

# Run
node index.js
```

---

## JavaScript-Specific Setup

### 1. Install Node.js

**Using nvm (Recommended)**:

```bash
# Install nvm
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.0/install.sh | bash

# Restart terminal or source
source ~/.bashrc

# Install Node.js LTS
nvm install --lts
nvm use --lts

# Verify
node --version
npm --version
```

**Using Official Installer**:

Download from [nodejs.org](https://nodejs.org/)

**Recommended Node.js versions**:
- **LTS (Long Term Support)**: 18.x, 20.x
- **Current**: 21.x

### 2. Package Managers

**npm (Default)**:
```bash
npm --version
```

**pnpm (Recommended for monorepos)**:
```bash
npm install -g pnpm
pnpm --version
```

**yarn (Alternative)**:
```bash
npm install -g yarn
yarn --version
```

### 3. Essential Tools

```bash
# ESLint - Linter
npm install -g eslint

# Prettier - Formatter
npm install -g prettier

# nodemon - Auto-restart dev server
npm install -g nodemon

# npm-check-updates - Update dependencies
npm install -g npm-check-updates
```

---

## Configuration Standards

### 1. package.json

**Complete Configuration**:

```json
{
  "name": "my-project",
  "version": "1.0.0",
  "description": "Project description",
  "type": "module",
  "main": "dist/index.js",
  "exports": {
    ".": {
      "import": "./dist/index.js",
      "require": "./dist/index.cjs"
    }
  },
  "scripts": {
    "start": "node dist/index.js",
    "dev": "nodemon src/index.js",
    "test": "node --experimental-vm-modules node_modules/jest/bin/jest.js",
    "test:watch": "npm test -- --watch",
    "test:coverage": "npm test -- --coverage",
    "lint": "eslint src/**/*.js",
    "lint:fix": "eslint src/**/*.js --fix",
    "format": "prettier --write \"src/**/*.js\"",
    "format:check": "prettier --check \"src/**/*.js\"",
    "build": "node build.js",
    "prepublishOnly": "npm run lint && npm test && npm run build"
  },
  "keywords": ["api", "backend"],
  "author": "Your Name <email@example.com>",
  "license": "MIT",
  "engines": {
    "node": ">=18.0.0",
    "npm": ">=9.0.0"
  },
  "dependencies": {
    "express": "^4.18.2",
    "dotenv": "^16.3.1"
  },
  "devDependencies": {
    "eslint": "^8.50.0",
    "eslint-config-prettier": "^9.0.0",
    "eslint-plugin-jest": "^27.4.0",
    "jest": "^29.7.0",
    "nodemon": "^3.0.1",
    "prettier": "^3.0.3",
    "supertest": "^6.3.3"
  }
}
```

### 2. .eslintrc.json

**ESLint Configuration**:

```json
{
  "env": {
    "node": true,
    "es2022": true,
    "jest": true
  },
  "extends": [
    "eslint:recommended",
    "prettier"
  ],
  "parserOptions": {
    "ecmaVersion": "latest",
    "sourceType": "module"
  },
  "rules": {
    "no-console": "warn",
    "no-unused-vars": ["error", { 
      "argsIgnorePattern": "^_",
      "varsIgnorePattern": "^_"
    }],
    "no-var": "error",
    "prefer-const": "error",
    "prefer-arrow-callback": "error",
    "arrow-body-style": ["error", "as-needed"],
    "no-throw-literal": "error",
    "prefer-template": "error",
    "prefer-destructuring": ["error", {
      "array": true,
      "object": true
    }],
    "object-shorthand": ["error", "always"],
    "no-param-reassign": "error",
    "eqeqeq": ["error", "always"],
    "curly": ["error", "all"],
    "brace-style": ["error", "1tbs"]
  }
}
```

### 3. .prettierrc.json

**Prettier Configuration**:

```json
{
  "semi": true,
  "trailingComma": "es5",
  "singleQuote": true,
  "printWidth": 100,
  "tabWidth": 2,
  "useTabs": false,
  "arrowParens": "avoid",
  "endOfLine": "lf"
}
```

### 4. jest.config.js

**Jest Configuration**:

```javascript
export default {
  testEnvironment: 'node',
  coverageDirectory: 'coverage',
  collectCoverageFrom: [
    'src/**/*.js',
    '!src/**/*.test.js',
    '!src/**/*.spec.js',
  ],
  coverageThreshold: {
    global: {
      branches: 90,
      functions: 90,
      lines: 90,
      statements: 90,
    },
  },
  testMatch: [
    '**/__tests__/**/*.js',
    '**/*.test.js',
    '**/*.spec.js',
  ],
  transform: {},
  moduleFileExtensions: ['js', 'json'],
  verbose: true,
};
```

### 5. .nvmrc

**Node Version**:

```
18.18.0
```

### 6. .gitignore

```gitignore
# Dependencies
node_modules/
npm-debug.log*
yarn-debug.log*
yarn-error.log*
pnpm-debug.log*

# Build output
dist/
build/
*.bundle.js

# Environment
.env
.env.local
.env.*.local

# IDE
.vscode/
.idea/
*.swp
*.swo
*~

# OS
.DS_Store
Thumbs.db

# Testing
coverage/
.nyc_output/

# Logs
logs/
*.log
```

### 7. .editorconfig

```ini
root = true

[*]
charset = utf-8
end_of_line = lf
insert_final_newline = true
trim_trailing_whitespace = true

[*.{js,json}]
indent_style = space
indent_size = 2

[*.md]
trim_trailing_whitespace = false
```

---

## Source Code Standards

### 1. Directory Structure

**Backend API (Express)**:

```
my-api/
├── src/
│   ├── index.js              # Entry point
│   ├── app.js                # Express app setup
│   ├── config/
│   │   ├── database.js
│   │   └── environment.js
│   ├── controllers/
│   │   └── userController.js
│   ├── services/
│   │   └── userService.js
│   ├── models/
│   │   └── user.js
│   ├── routes/
│   │   └── userRoutes.js
│   ├── middleware/
│   │   ├── auth.js
│   │   ├── errorHandler.js
│   │   └── validation.js
│   └── utils/
│       ├── logger.js
│       └── validators.js
├── tests/
│   ├── unit/
│   │   └── services/
│   │       └── userService.test.js
│   ├── integration/
│   │   └── routes/
│   │       └── userRoutes.test.js
│   └── helpers/
│       └── testSetup.js
├── docs/
│   ├── ROADMAP.md
│   └── SPECS.md
├── .env.example
├── .eslintrc.json
├── .prettierrc.json
├── .nvmrc
├── jest.config.js
├── package.json
└── README.md
```

### 2. Naming Conventions

| Element | Convention | Example |
|---------|-----------|---------|
| **Files** | camelCase | `userController.js` |
| **Directories** | kebab-case | `user-management/` |
| **Classes** | PascalCase | `UserService` |
| **Functions** | camelCase | `createUser()` |
| **Constants** | UPPER_SNAKE_CASE | `MAX_CONNECTIONS` |
| **Variables** | camelCase | `userData` |
| **Private** | _camelCase | `_privateMethod()` |

### 3. Entry Point (index.js)

```javascript
/**
 * Application entry point
 * @module index
 */

import app from './app.js';
import config from './config/environment.js';
import logger from './utils/logger.js';

const PORT = config.port || 3000;

/**
 * Start the server
 */
const server = app.listen(PORT, () => {
  logger.info(`Server running on port ${PORT}`);
  logger.info(`Environment: ${config.env}`);
});

/**
 * Graceful shutdown
 */
const gracefulShutdown = signal => {
  logger.info(`${signal} received, closing server...`);
  
  server.close(() => {
    logger.info('Server closed');
    process.exit(0);
  });

  // Force shutdown after 10s
  setTimeout(() => {
    logger.error('Forced shutdown');
    process.exit(1);
  }, 10000);
};

process.on('SIGTERM', () => gracefulShutdown('SIGTERM'));
process.on('SIGINT', () => gracefulShutdown('SIGINT'));

// Handle uncaught exceptions
process.on('uncaughtException', err => {
  logger.error('Uncaught exception:', err);
  process.exit(1);
});

process.on('unhandledRejection', (reason, promise) => {
  logger.error('Unhandled rejection at:', promise, 'reason:', reason);
  process.exit(1);
});

export default server;
```

### 4. Express App Setup (app.js)

```javascript
/**
 * Express application configuration
 * @module app
 */

import express from 'express';
import helmet from 'helmet';
import cors from 'cors';
import compression from 'compression';
import rateLimit from 'express-rate-limit';

import config from './config/environment.js';
import logger from './utils/logger.js';
import errorHandler from './middleware/errorHandler.js';
import userRoutes from './routes/userRoutes.js';

const app = express();

// Security middleware
app.use(helmet());
app.use(cors(config.cors));

// Rate limiting
const limiter = rateLimit({
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: 100, // limit each IP to 100 requests per windowMs
});
app.use('/api/', limiter);

// Body parsing
app.use(express.json({ limit: '10mb' }));
app.use(express.urlencoded({ extended: true, limit: '10mb' }));

// Compression
app.use(compression());

// Request logging
app.use((req, res, next) => {
  logger.info(`${req.method} ${req.path}`);
  next();
});

// Health check
app.get('/health', (req, res) => {
  res.status(200).json({ status: 'ok', timestamp: new Date().toISOString() });
});

// API routes
app.use('/api/users', userRoutes);

// 404 handler
app.use((req, res) => {
  res.status(404).json({ error: 'Not found' });
});

// Error handler (must be last)
app.use(errorHandler);

export default app;
```

### 5. Controller

```javascript
/**
 * User controller
 * @module controllers/userController
 */

import userService from '../services/userService.js';
import { ValidationError } from '../utils/errors.js';

/**
 * Get all users
 */
export const getAllUsers = async (req, res, next) => {
  try {
    const users = await userService.getAllUsers();
    res.json({ data: users });
  } catch (error) {
    next(error);
  }
};

/**
 * Get user by ID
 */
export const getUserById = async (req, res, next) => {
  try {
    const { id } = req.params;
    
    if (!id) {
      throw new ValidationError('User ID is required');
    }

    const user = await userService.getUserById(id);
    
    if (!user) {
      return res.status(404).json({ error: 'User not found' });
    }

    res.json({ data: user });
  } catch (error) {
    next(error);
  }
};

/**
 * Create user
 */
export const createUser = async (req, res, next) => {
  try {
    const userData = req.body;
    
    // Validate
    if (!userData.email || !userData.name) {
      throw new ValidationError('Email and name are required');
    }

    const user = await userService.createUser(userData);
    
    res.status(201).json({ data: user });
  } catch (error) {
    next(error);
  }
};

/**
 * Update user
 */
export const updateUser = async (req, res, next) => {
  try {
    const { id } = req.params;
    const userData = req.body;

    const user = await userService.updateUser(id, userData);
    
    if (!user) {
      return res.status(404).json({ error: 'User not found' });
    }

    res.json({ data: user });
  } catch (error) {
    next(error);
  }
};

/**
 * Delete user
 */
export const deleteUser = async (req, res, next) => {
  try {
    const { id } = req.params;

    await userService.deleteUser(id);
    
    res.status(204).send();
  } catch (error) {
    next(error);
  }
};
```

### 6. Service Layer

```javascript
/**
 * User service
 * @module services/userService
 */

import User from '../models/user.js';
import logger from '../utils/logger.js';
import { NotFoundError, ConflictError } from '../utils/errors.js';

/**
 * Get all users
 * @returns {Promise<Array>} List of users
 */
export const getAllUsers = async () => {
  logger.debug('Fetching all users');
  
  const users = await User.findAll();
  
  return users.map(user => user.toJSON());
};

/**
 * Get user by ID
 * @param {string} id - User ID
 * @returns {Promise<Object>} User data
 * @throws {NotFoundError} If user not found
 */
export const getUserById = async id => {
  logger.debug(`Fetching user with id: ${id}`);
  
  const user = await User.findById(id);
  
  if (!user) {
    throw new NotFoundError(`User with id ${id} not found`);
  }
  
  return user.toJSON();
};

/**
 * Create new user
 * @param {Object} userData - User data
 * @returns {Promise<Object>} Created user
 * @throws {ConflictError} If email already exists
 */
export const createUser = async userData => {
  logger.debug(`Creating user: ${userData.email}`);
  
  // Check if email exists
  const existingUser = await User.findByEmail(userData.email);
  
  if (existingUser) {
    throw new ConflictError('Email already in use');
  }
  
  const user = await User.create(userData);
  
  logger.info(`User created: ${user.id}`);
  
  return user.toJSON();
};

/**
 * Update user
 * @param {string} id - User ID
 * @param {Object} userData - Updated data
 * @returns {Promise<Object>} Updated user
 */
export const updateUser = async (id, userData) => {
  logger.debug(`Updating user: ${id}`);
  
  const user = await User.findById(id);
  
  if (!user) {
    throw new NotFoundError(`User with id ${id} not found`);
  }
  
  await user.update(userData);
  
  logger.info(`User updated: ${id}`);
  
  return user.toJSON();
};

/**
 * Delete user
 * @param {string} id - User ID
 */
export const deleteUser = async id => {
  logger.debug(`Deleting user: ${id}`);
  
  const user = await User.findById(id);
  
  if (!user) {
    throw new NotFoundError(`User with id ${id} not found`);
  }
  
  await user.delete();
  
  logger.info(`User deleted: ${id}`);
};

export default {
  getAllUsers,
  getUserById,
  createUser,
  updateUser,
  deleteUser,
};
```

### 7. Custom Errors

```javascript
/**
 * Custom error classes
 * @module utils/errors
 */

/**
 * Base application error
 */
export class AppError extends Error {
  constructor(message, statusCode = 500) {
    super(message);
    this.name = this.constructor.name;
    this.statusCode = statusCode;
    this.isOperational = true;
    Error.captureStackTrace(this, this.constructor);
  }
}

/**
 * Validation error
 */
export class ValidationError extends AppError {
  constructor(message) {
    super(message, 400);
  }
}

/**
 * Not found error
 */
export class NotFoundError extends AppError {
  constructor(message) {
    super(message, 404);
  }
}

/**
 * Conflict error
 */
export class ConflictError extends AppError {
  constructor(message) {
    super(message, 409);
  }
}

/**
 * Unauthorized error
 */
export class UnauthorizedError extends AppError {
  constructor(message = 'Unauthorized') {
    super(message, 401);
  }
}

/**
 * Forbidden error
 */
export class ForbiddenError extends AppError {
  constructor(message = 'Forbidden') {
    super(message, 403);
  }
}
```

### 8. Error Handler Middleware

```javascript
/**
 * Error handler middleware
 * @module middleware/errorHandler
 */

import logger from '../utils/logger.js';
import { AppError } from '../utils/errors.js';

/**
 * Global error handler
 */
const errorHandler = (err, req, res, next) => {
  // Log error
  logger.error({
    message: err.message,
    stack: err.stack,
    url: req.originalUrl,
    method: req.method,
  });

  // Operational error
  if (err.isOperational) {
    return res.status(err.statusCode).json({
      error: {
        message: err.message,
        code: err.name,
      },
    });
  }

  // Programming or unknown error
  return res.status(500).json({
    error: {
      message: 'Internal server error',
      code: 'INTERNAL_ERROR',
    },
  });
};

export default errorHandler;
```

---

## Testing Standards

### 1. Test Structure

```
tests/
├── unit/
│   ├── services/
│   │   └── userService.test.js
│   └── utils/
│       └── validators.test.js
├── integration/
│   └── routes/
│       └── userRoutes.test.js
└── helpers/
    └── testSetup.js
```

### 2. Unit Test Example (Jest)

```javascript
/**
 * User service tests
 */

import { jest } from '@jest/globals';
import userService from '../../src/services/userService.js';
import User from '../../src/models/user.js';

// Mock User model
jest.mock('../../src/models/user.js');

describe('UserService', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('getAllUsers', () => {
    it('should return all users', async () => {
      // Arrange
      const mockUsers = [
        { id: '1', name: 'John', email: 'john@example.com', toJSON: () => ({ id: '1', name: 'John' }) },
        { id: '2', name: 'Jane', email: 'jane@example.com', toJSON: () => ({ id: '2', name: 'Jane' }) },
      ];
      
      User.findAll.mockResolvedValue(mockUsers);

      // Act
      const users = await userService.getAllUsers();

      // Assert
      expect(users).toHaveLength(2);
      expect(User.findAll).toHaveBeenCalledTimes(1);
    });
  });

  describe('createUser', () => {
    it('should create user with valid data', async () => {
      // Arrange
      const userData = {
        email: 'test@example.com',
        name: 'Test User',
      };

      const mockUser = {
        id: '1',
        ...userData,
        toJSON: () => ({ id: '1', ...userData }),
      };

      User.findByEmail.mockResolvedValue(null);
      User.create.mockResolvedValue(mockUser);

      // Act
      const user = await userService.createUser(userData);

      // Assert
      expect(user).toEqual({ id: '1', ...userData });
      expect(User.findByEmail).toHaveBeenCalledWith(userData.email);
      expect(User.create).toHaveBeenCalledWith(userData);
    });

    it('should throw ConflictError if email exists', async () => {
      // Arrange
      const userData = { email: 'existing@example.com', name: 'Test' };
      User.findByEmail.mockResolvedValue({ id: '1', email: userData.email });

      // Act & Assert
      await expect(userService.createUser(userData)).rejects.toThrow('Email already in use');
      expect(User.create).not.toHaveBeenCalled();
    });
  });
});
```

### 3. Integration Test Example

```javascript
/**
 * User routes integration tests
 */

import request from 'supertest';
import app from '../../src/app.js';
import User from '../../src/models/user.js';

describe('User Routes', () => {
  beforeEach(async () => {
    await User.deleteAll();
  });

  describe('POST /api/users', () => {
    it('should create user with valid data', async () => {
      // Arrange
      const userData = {
        email: 'test@example.com',
        name: 'Test User',
      };

      // Act
      const response = await request(app)
        .post('/api/users')
        .send(userData)
        .expect(201);

      // Assert
      expect(response.body.data).toMatchObject(userData);
      expect(response.body.data.id).toBeDefined();
    });

    it('should return 400 for invalid data', async () => {
      // Arrange
      const userData = { email: 'invalid-email' };

      // Act
      const response = await request(app)
        .post('/api/users')
        .send(userData)
        .expect(400);

      // Assert
      expect(response.body.error).toBeDefined();
    });
  });

  describe('GET /api/users/:id', () => {
    it('should return user by id', async () => {
      // Arrange
      const user = await User.create({
        email: 'test@example.com',
        name: 'Test User',
      });

      // Act
      const response = await request(app)
        .get(`/api/users/${user.id}`)
        .expect(200);

      // Assert
      expect(response.body.data.id).toBe(user.id);
    });

    it('should return 404 for non-existent user', async () => {
      // Act
      await request(app)
        .get('/api/users/non-existent-id')
        .expect(404);
    });
  });
});
```

### 4. Coverage Requirements

- **Overall**: > 90%
- **Unit Tests**: > 95%
- **Integration Tests**: > 85%

**Check coverage**:

```bash
npm test -- --coverage

# Open HTML report
open coverage/lcov-report/index.html
```

---

## Build & Deployment

### 1. Environment Configuration

**.env.example**:

```env
# Server
NODE_ENV=development
PORT=3000

# Database
DATABASE_URL=postgresql://user:pass@localhost:5432/mydb

# Security
JWT_SECRET=your-secret-key
API_KEY=your-api-key

# Logging
LOG_LEVEL=info
```

### 2. Docker Support

**Dockerfile**:

```dockerfile
# Build stage
FROM node:18-alpine AS builder

WORKDIR /app

# Copy package files
COPY package*.json ./

# Install dependencies
RUN npm ci --only=production

# Production stage
FROM node:18-alpine

WORKDIR /app

# Copy dependencies from builder
COPY --from=builder /app/node_modules ./node_modules

# Copy source
COPY . .

# Create non-root user
RUN addgroup -g 1000 appuser && \
    adduser -D -u 1000 -G appuser appuser && \
    chown -R appuser:appuser /app

USER appuser

EXPOSE 3000

HEALTHCHECK --interval=30s --timeout=3s --retries=3 \
  CMD node healthcheck.js || exit 1

CMD ["node", "src/index.js"]
```

### 3. Publishing to npm

```bash
# Login
npm login

# Publish
npm publish

# Publish scoped package
npm publish --access public
```

---

## Documentation

### 1. JSDoc Comments

```javascript
/**
 * Process a payment transaction
 * 
 * @param {string} userId - The user's unique identifier
 * @param {number} amount - Payment amount in cents (must be positive)
 * @param {string} currency - ISO 4217 currency code (e.g., "USD", "EUR")
 * @returns {Promise<PaymentResult>} Payment confirmation with transaction details
 * @throws {ValidationError} If amount is negative or currency is invalid
 * @throws {InsufficientFundsError} If user balance is insufficient
 * 
 * @example
 * const result = await processPayment('user-123', 1000, 'USD');
 * console.log(`Transaction ID: ${result.transactionId}`);
 */
async function processPayment(userId, amount, currency) {
  // Implementation
}
```

### 2. Generate Documentation

```bash
# Install JSDoc
npm install -g jsdoc

# Generate docs
jsdoc -c jsdoc.json src/

# Output: docs/
```

---

## JavaScript Best Practices

See [BEST_PRACTICES.md](BEST_PRACTICES.md) for complete guide.

### Quick Tips

1. **Use modern ES6+ features** (arrow functions, destructuring, spread)
2. **Prefer `const` over `let`**, never use `var`
3. **Use async/await** instead of callbacks or raw promises
4. **Handle errors properly** with try/catch
5. **Use strict equality** (`===` instead of `==`)
6. **Avoid callback hell** with async/await
7. **Use template literals** for string interpolation
8. **Destructure objects and arrays**
9. **Use default parameters**
10. **Format code with Prettier**

---

## Common Patterns

Refer to [Source Code Standards](#source-code-standards) and [BEST_PRACTICES.md](BEST_PRACTICES.md).

---

## Troubleshooting

### Common Issues

**Issue**: `Cannot find module`

**Solution**: Check import paths and ensure module is installed

**Issue**: Memory leaks

**Solution**: Remove event listeners, clear intervals/timeouts

---

## Complete Workflow

Follow the [AI Integration Manual Template](../templates/AI_INTEGRATION_MANUAL_TEMPLATE.md) for the complete 6-phase workflow.

---

## Additional Resources

- [MDN JavaScript Guide](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide)
- [Node.js Documentation](https://nodejs.org/docs/latest/api/)
- [Express.js Guide](https://expressjs.com/)
- [JavaScript.info](https://javascript.info/)
- [BEST_PRACTICES.md](BEST_PRACTICES.md)

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-10-11 | Initial JavaScript manual |

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

