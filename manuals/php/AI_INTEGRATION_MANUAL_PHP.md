# AI Integration Manual - PHP

**Version**: 1.0.0  
**Last Updated**: 2025-10-11  
**Language**: PHP 8.2+ / 8.3  
**Target Audience**: AI Agents (LLM-based development assistants)

---

## Table of Contents

1. [Introduction](#introduction)
2. [Quick Start](#quick-start)
3. [PHP-Specific Setup](#php-specific-setup)
4. [Configuration Standards](#configuration-standards)
5. [Source Code Standards](#source-code-standards)
6. [Testing Standards](#testing-standards)
7. [Build & Deployment](#build--deployment)
8. [Documentation](#documentation)
9. [PHP Best Practices](#php-best-practices)
10. [Common Patterns](#common-patterns)
11. [Troubleshooting](#troubleshooting)
12. [Complete Workflow](#complete-workflow)

---

## Introduction

This manual extends the [AI Integration Manual Template](../templates/AI_INTEGRATION_MANUAL_TEMPLATE.md) with PHP-specific implementations.

**When to use this manual**:
- PHP web applications
- REST APIs (Laravel, Symfony, Slim)
- Content Management Systems
- E-commerce platforms
- Libraries/Packages
- CLI tools

**Prerequisites**:
- Read [AI Integration Manual Template](../templates/AI_INTEGRATION_MANUAL_TEMPLATE.md)
- Read [Language Standards](../LANGUAGE_STANDARDS.md)
- Basic PHP knowledge

---

## Quick Start

### Plain PHP Project

```bash
# 1. Create project directory
mkdir my-project && cd my-project

# 2. Initialize composer
composer init

# 3. Install dev dependencies
composer require --dev phpunit/phpunit
composer require --dev phpstan/phpstan
composer require --dev friendsofphp/php-cs-fixer

# 4. Create directories
mkdir -p src tests public config

# 5. Create autoload
# Edit composer.json to add:
# "autoload": { "psr-4": { "App\\": "src/" } }

# 6. Update autoload
composer dump-autoload
```

### Laravel Project

```bash
# Create new Laravel project
composer create-project laravel/laravel my-project
cd my-project

# Install additional packages
composer require --dev laravel/pint
composer require --dev nunomaduro/larastan

# Run development server
php artisan serve
```

### Symfony Project

```bash
# Create new Symfony project (web)
composer create-project symfony/skeleton my-project
cd my-project

# Or full web application
composer create-project symfony/website-skeleton my-project

# Install packages
composer require symfony/orm-pack
composer require --dev symfony/maker-bundle
composer require --dev phpunit/phpunit

# Run development server
symfony serve
```

---

## PHP-Specific Setup

### 1. Environment Setup

#### Install PHP

**Using Package Managers**:

```bash
# Ubuntu/Debian
sudo apt update
sudo apt install php8.3 php8.3-cli php8.3-fpm php8.3-mysql php8.3-xml php8.3-mbstring php8.3-curl

# macOS (Homebrew)
brew install php@8.3

# Windows
# Download from: https://windows.php.net/download/

# Verify installation
php -v
php -m  # List installed modules
```

#### Install Composer

```bash
# Download and install
curl -sS https://getcomposer.org/installer | php
sudo mv composer.phar /usr/local/bin/composer

# Verify
composer --version
```

### 2. Essential Extensions

**Required PHP Extensions**:

```bash
# Install common extensions (Ubuntu/Debian)
sudo apt install \
    php8.3-cli \
    php8.3-fpm \
    php8.3-mysql \
    php8.3-pgsql \
    php8.3-sqlite3 \
    php8.3-redis \
    php8.3-mbstring \
    php8.3-xml \
    php8.3-curl \
    php8.3-zip \
    php8.3-gd \
    php8.3-bcmath \
    php8.3-intl
```

### 3. Development Tools

```bash
# Install globally useful tools
composer global require laravel/installer
composer global require symfony/cli
composer global require phpstan/phpstan
composer global require friendsofphp/php-cs-fixer
```

---

## Configuration Standards

### 1. composer.json

**Complete Configuration**:

```json
{
    "name": "vendor/my-project",
    "description": "Project description",
    "type": "project",
    "license": "MIT",
    "authors": [
        {
            "name": "Author Name",
            "email": "author@example.com"
        }
    ],
    "minimum-stability": "stable",
    "prefer-stable": true,
    "require": {
        "php": "^8.2",
        "ext-json": "*",
        "ext-mbstring": "*",
        "ext-pdo": "*"
    },
    "require-dev": {
        "phpunit/phpunit": "^10.5",
        "phpstan/phpstan": "^1.10",
        "friendsofphp/php-cs-fixer": "^3.45",
        "psalm/plugin-phpunit": "^0.19",
        "mockery/mockery": "^1.6"
    },
    "autoload": {
        "psr-4": {
            "App\\": "src/"
        }
    },
    "autoload-dev": {
        "psr-4": {
            "Tests\\": "tests/"
        }
    },
    "scripts": {
        "test": "phpunit",
        "test:coverage": "phpunit --coverage-html coverage",
        "analyse": "phpstan analyse src tests --level=max",
        "format": "php-cs-fixer fix",
        "format:check": "php-cs-fixer fix --dry-run --diff",
        "check": [
            "@analyse",
            "@format:check",
            "@test"
        ]
    },
    "config": {
        "optimize-autoloader": true,
        "preferred-install": "dist",
        "sort-packages": true,
        "allow-plugins": {
            "php-http/discovery": true
        }
    }
}
```

### 2. .php-cs-fixer.php

**Code Style Configuration (PSR-12)**:

```php
<?php

use PhpCsFixer\Config;
use PhpCsFixer\Finder;

$finder = Finder::create()
    ->in(__DIR__ . '/src')
    ->in(__DIR__ . '/tests')
    ->name('*.php')
    ->ignoreDotFiles(true)
    ->ignoreVCS(true);

return (new Config())
    ->setRules([
        '@PSR12' => true,
        '@PHP82Migration' => true,
        'array_syntax' => ['syntax' => 'short'],
        'ordered_imports' => ['sort_algorithm' => 'alpha'],
        'no_unused_imports' => true,
        'not_operator_with_successor_space' => true,
        'trailing_comma_in_multiline' => true,
        'phpdoc_scalar' => true,
        'unary_operator_spaces' => true,
        'binary_operator_spaces' => true,
        'blank_line_before_statement' => [
            'statements' => ['break', 'continue', 'declare', 'return', 'throw', 'try'],
        ],
        'phpdoc_single_line_var_spacing' => true,
        'phpdoc_var_without_name' => true,
        'class_attributes_separation' => [
            'elements' => [
                'const' => 'one',
                'method' => 'one',
                'property' => 'one',
            ],
        ],
        'method_argument_space' => [
            'on_multiline' => 'ensure_fully_multiline',
            'keep_multiple_spaces_after_comma' => true,
        ],
        'single_trait_insert_per_statement' => true,
    ])
    ->setFinder($finder);
```

### 3. phpstan.neon

**Static Analysis Configuration**:

```neon
parameters:
    level: max
    paths:
        - src
        - tests
    excludePaths:
        - tests/Fixtures/*
    
    checkMissingIterableValueType: false
    checkGenericClassInNonGenericObjectType: false
    
    ignoreErrors:
        # Add specific ignores if needed
```

### 4. phpunit.xml

**Testing Configuration**:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<phpunit xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:noNamespaceSchemaLocation="vendor/phpunit/phpunit/phpunit.xsd"
         bootstrap="vendor/autoload.php"
         colors="true"
         failOnWarning="true"
         failOnRisky="true"
         beStrictAboutOutputDuringTests="true"
         cacheDirectory=".phpunit.cache">
    <testsuites>
        <testsuite name="Unit">
            <directory>tests/Unit</directory>
        </testsuite>
        <testsuite name="Integration">
            <directory>tests/Integration</directory>
        </testsuite>
    </testsuites>
    
    <source>
        <include>
            <directory>src</directory>
        </include>
    </source>
    
    <coverage>
        <report>
            <html outputDirectory="coverage"/>
            <text outputFile="php://stdout" showUncoveredFiles="true"/>
            <clover outputFile="coverage/clover.xml"/>
        </report>
    </coverage>
    
    <php>
        <env name="APP_ENV" value="testing"/>
        <env name="DB_CONNECTION" value="sqlite"/>
        <env name="DB_DATABASE" value=":memory:"/>
        <env name="CACHE_DRIVER" value="array"/>
        <env name="QUEUE_CONNECTION" value="sync"/>
    </php>
</phpunit>
```

### 5. Environment Configuration

**.env.example**:

```env
# Application
APP_NAME=MyProject
APP_ENV=local
APP_DEBUG=true
APP_URL=http://localhost

# Database
DB_CONNECTION=mysql
DB_HOST=127.0.0.1
DB_PORT=3306
DB_DATABASE=mydb
DB_USERNAME=root
DB_PASSWORD=

# Redis
REDIS_HOST=127.0.0.1
REDIS_PASSWORD=null
REDIS_PORT=6379

# Mail
MAIL_MAILER=smtp
MAIL_HOST=smtp.mailtrap.io
MAIL_PORT=2525
MAIL_USERNAME=null
MAIL_PASSWORD=null

# Task Queue
TASK_QUEUE_URL=http://localhost:8080
TASK_QUEUE_TOKEN=

# Vectorizer
VECTORIZER_URL=http://localhost:9000
VECTORIZER_TOKEN=
```

---

## Source Code Standards

### 1. Directory Structure

**Plain PHP Project**:

```
src/
├── Controller/         # Controllers
├── Service/           # Business logic
├── Repository/        # Data access
├── Model/             # Data models/entities
├── Middleware/        # Middleware
├── Exception/         # Custom exceptions
├── Util/              # Utilities
└── Config/            # Configuration

tests/
├── Unit/              # Unit tests
├── Integration/       # Integration tests
└── Fixtures/          # Test data
```

**Laravel Project**:

```
app/
├── Console/           # Artisan commands
├── Http/
│   ├── Controllers/   # HTTP controllers
│   ├── Middleware/    # HTTP middleware
│   └── Requests/      # Form requests (validation)
├── Models/            # Eloquent models
├── Services/          # Business logic
├── Repositories/      # Data access (optional)
└── Exceptions/        # Custom exceptions

tests/
├── Unit/
├── Feature/           # Laravel feature tests
└── TestCase.php

config/                # Configuration files
database/
├── migrations/
├── seeders/
└── factories/
```

### 2. Naming Conventions

| Element | Convention | Example |
|---------|-----------|---------|
| **Namespaces** | PascalCase | `App\Services` |
| **Classes** | PascalCase | `UserService` |
| **Interfaces** | PascalCase (Interface suffix) | `UserRepositoryInterface` |
| **Traits** | PascalCase | `Timestampable` |
| **Methods** | camelCase | `createUser()` |
| **Properties** | camelCase | `$userId` |
| **Constants** | UPPER_SNAKE_CASE | `MAX_RETRY_COUNT` |
| **Files** | PascalCase | `UserService.php` |

### 3. Entity/Model (Eloquent - Laravel)

```php
<?php

namespace App\Models;

use Illuminate\Database\Eloquent\Factories\HasFactory;
use Illuminate\Database\Eloquent\Model;
use Illuminate\Database\Eloquent\SoftDeletes;

/**
 * User model
 *
 * @property string $id
 * @property string $email
 * @property string $name
 * @property int|null $age
 * @property \Carbon\Carbon $created_at
 * @property \Carbon\Carbon $updated_at
 */
class User extends Model
{
    use HasFactory, SoftDeletes;
    
    /**
     * The attributes that are mass assignable.
     */
    protected $fillable = [
        'email',
        'name',
        'age',
    ];
    
    /**
     * The attributes that should be hidden.
     */
    protected $hidden = [
        'password',
    ];
    
    /**
     * Get the attributes that should be cast.
     */
    protected function casts(): array
    {
        return [
            'age' => 'integer',
            'email_verified_at' => 'datetime',
            'created_at' => 'datetime',
            'updated_at' => 'datetime',
        ];
    }
}
```

### 4. Repository Pattern

```php
<?php

namespace App\Repositories;

use App\Models\User;
use App\Repositories\Interfaces\UserRepositoryInterface;
use Illuminate\Database\Eloquent\Collection;

/**
 * User repository implementation
 */
class UserRepository implements UserRepositoryInterface
{
    /**
     * Find user by ID
     */
    public function findById(string $id): ?User
    {
        return User::find($id);
    }
    
    /**
     * Find user by email
     */
    public function findByEmail(string $email): ?User
    {
        return User::where('email', $email)->first();
    }
    
    /**
     * Get all users
     */
    public function getAll(): Collection
    {
        return User::all();
    }
    
    /**
     * Create a new user
     *
     * @throws \App\Exceptions\ValidationException
     */
    public function create(array $data): User
    {
        return User::create($data);
    }
    
    /**
     * Update user
     */
    public function update(string $id, array $data): User
    {
        $user = $this->findById($id);
        
        if (!$user) {
            throw new \App\Exceptions\NotFoundException('User', $id);
        }
        
        $user->update($data);
        
        return $user;
    }
    
    /**
     * Delete user
     */
    public function delete(string $id): bool
    {
        $user = $this->findById($id);
        
        if (!$user) {
            throw new \App\Exceptions\NotFoundException('User', $id);
        }
        
        return $user->delete();
    }
}
```

### 5. Service Layer

```php
<?php

namespace App\Services;

use App\Models\User;
use App\Repositories\Interfaces\UserRepositoryInterface;
use App\Exceptions\ConflictException;
use Psr\Log\LoggerInterface;

/**
 * User service
 */
class UserService
{
    public function __construct(
        private readonly UserRepositoryInterface $userRepository,
        private readonly LoggerInterface $logger
    ) {
    }
    
    /**
     * Create a new user
     *
     * @param array{email: string, name: string, age?: int} $data
     * @return User
     * @throws ConflictException If email already exists
     */
    public function createUser(array $data): User
    {
        $this->logger->debug('Creating user', ['email' => $data['email']]);
        
        // Check if email exists
        if ($this->userRepository->findByEmail($data['email'])) {
            throw new ConflictException('Email already in use');
        }
        
        $user = $this->userRepository->create($data);
        
        $this->logger->info('User created', ['user_id' => $user->id]);
        
        return $user;
    }
    
    /**
     * Get user by ID
     *
     * @throws \App\Exceptions\NotFoundException
     */
    public function getUserById(string $id): User
    {
        $user = $this->userRepository->findById($id);
        
        if (!$user) {
            throw new \App\Exceptions\NotFoundException('User', $id);
        }
        
        return $user;
    }
}
```

### 6. Controller (Laravel)

```php
<?php

namespace App\Http\Controllers;

use App\Http\Requests\CreateUserRequest;
use App\Http\Resources\UserResource;
use App\Services\UserService;
use Illuminate\Http\JsonResponse;
use Illuminate\Http\Response;

/**
 * User management controller
 */
class UserController extends Controller
{
    public function __construct(
        private readonly UserService $userService
    ) {
    }
    
    /**
     * Create a new user
     *
     * @param CreateUserRequest $request
     * @return JsonResponse
     */
    public function store(CreateUserRequest $request): JsonResponse
    {
        $user = $this->userService->createUser($request->validated());
        
        return (new UserResource($user))
            ->response()
            ->setStatusCode(Response::HTTP_CREATED);
    }
    
    /**
     * Get user by ID
     *
     * @param string $id
     * @return UserResource
     */
    public function show(string $id): UserResource
    {
        $user = $this->userService->getUserById($id);
        
        return new UserResource($user);
    }
    
    /**
     * Get all users
     *
     * @return \Illuminate\Http\Resources\Json\AnonymousResourceCollection
     */
    public function index()
    {
        $users = $this->userService->getAllUsers();
        
        return UserResource::collection($users);
    }
}
```

### 7. Form Request (Laravel Validation)

```php
<?php

namespace App\Http\Requests;

use Illuminate\Foundation\Http\FormRequest;

/**
 * Create user request validation
 */
class CreateUserRequest extends FormRequest
{
    /**
     * Determine if the user is authorized to make this request.
     */
    public function authorize(): bool
    {
        return true;
    }
    
    /**
     * Get the validation rules that apply to the request.
     *
     * @return array<string, \Illuminate\Contracts\Validation\ValidationRule|array|string>
     */
    public function rules(): array
    {
        return [
            'email' => ['required', 'email', 'unique:users,email', 'max:255'],
            'name' => ['required', 'string', 'min:2', 'max:100'],
            'age' => ['nullable', 'integer', 'min:0', 'max:150'],
        ];
    }
    
    /**
     * Get custom messages for validator errors.
     */
    public function messages(): array
    {
        return [
            'email.required' => 'Email is required',
            'email.email' => 'Email must be valid',
            'email.unique' => 'Email already in use',
        ];
    }
}
```

### 8. Custom Exceptions

```php
<?php

namespace App\Exceptions;

use Exception;

/**
 * Base application exception
 */
class AppException extends Exception
{
    protected string $code;
    protected int $statusCode;
    
    public function __construct(
        string $message,
        string $code,
        int $statusCode = 500,
        ?\Throwable $previous = null
    ) {
        parent::__construct($message, 0, $previous);
        $this->code = $code;
        $this->statusCode = $statusCode;
    }
    
    public function getStatusCode(): int
    {
        return $this->statusCode;
    }
    
    public function getErrorCode(): string
    {
        return $this->code;
    }
}

/**
 * Resource not found exception
 */
class NotFoundException extends AppException
{
    public function __construct(string $resource, string $id)
    {
        parent::__construct(
            sprintf('%s with id %s not found', $resource, $id),
            'NOT_FOUND',
            404
        );
    }
}

/**
 * Conflict exception
 */
class ConflictException extends AppException
{
    public function __construct(string $message)
    {
        parent::__construct($message, 'CONFLICT', 409);
    }
}
```

### 9. Exception Handler (Laravel)

```php
<?php

namespace App\Exceptions;

use Illuminate\Foundation\Exceptions\Handler as ExceptionHandler;
use Illuminate\Http\JsonResponse;
use Illuminate\Validation\ValidationException;
use Symfony\Component\HttpKernel\Exception\NotFoundHttpException;
use Throwable;

class Handler extends ExceptionHandler
{
    /**
     * Render an exception into an HTTP response.
     */
    public function render($request, Throwable $e): JsonResponse
    {
        if ($e instanceof NotFoundException) {
            return response()->json([
                'error' => $e->getErrorCode(),
                'message' => $e->getMessage(),
            ], $e->getStatusCode());
        }
        
        if ($e instanceof ConflictException) {
            return response()->json([
                'error' => $e->getErrorCode(),
                'message' => $e->getMessage(),
            ], $e->getStatusCode());
        }
        
        if ($e instanceof ValidationException) {
            return response()->json([
                'error' => 'VALIDATION_ERROR',
                'message' => 'Validation failed',
                'errors' => $e->errors(),
            ], 422);
        }
        
        // Log unexpected errors
        $this->report($e);
        
        return response()->json([
            'error' => 'INTERNAL_ERROR',
            'message' => 'An unexpected error occurred',
        ], 500);
    }
}
```

---

## Testing Standards

### 1. Test Structure

```
tests/
├── Unit/                   # Unit tests
│   ├── Services/
│   │   └── UserServiceTest.php
│   └── Util/
│       └── ValidatorTest.php
├── Integration/            # Integration tests
│   ├── Api/
│   │   └── UserControllerTest.php
│   └── Repository/
│       └── UserRepositoryTest.php
├── Feature/                # Laravel feature tests
│   └── UserManagementTest.php
├── Fixtures/               # Test data
│   └── UserFixtures.php
├── TestCase.php           # Base test case
└── CreatesApplication.php  # Laravel trait
```

### 2. Unit Test Example (PHPUnit)

```php
<?php

namespace Tests\Unit\Services;

use App\Models\User;
use App\Repositories\Interfaces\UserRepositoryInterface;
use App\Services\UserService;
use App\Exceptions\ConflictException;
use Mockery;
use PHPUnit\Framework\TestCase;
use Psr\Log\LoggerInterface;

class UserServiceTest extends TestCase
{
    private UserRepositoryInterface $mockRepository;
    private LoggerInterface $mockLogger;
    private UserService $userService;
    
    protected function setUp(): void
    {
        parent::setUp();
        
        $this->mockRepository = Mockery::mock(UserRepositoryInterface::class);
        $this->mockLogger = Mockery::mock(LoggerInterface::class);
        $this->userService = new UserService(
            $this->mockRepository,
            $this->mockLogger
        );
    }
    
    protected function tearDown(): void
    {
        Mockery::close();
        parent::tearDown();
    }
    
    public function test_create_user_success(): void
    {
        // Arrange
        $data = [
            'email' => 'test@example.com',
            'name' => 'Test User',
        ];
        
        $user = new User($data);
        $user->id = '123';
        
        $this->mockRepository
            ->shouldReceive('findByEmail')
            ->once()
            ->with($data['email'])
            ->andReturn(null);
        
        $this->mockRepository
            ->shouldReceive('create')
            ->once()
            ->with($data)
            ->andReturn($user);
        
        $this->mockLogger
            ->shouldReceive('debug')
            ->once();
        
        $this->mockLogger
            ->shouldReceive('info')
            ->once();
        
        // Act
        $result = $this->userService->createUser($data);
        
        // Assert
        $this->assertInstanceOf(User::class, $result);
        $this->assertEquals($data['email'], $result->email);
    }
    
    public function test_create_user_email_exists(): void
    {
        // Arrange
        $data = ['email' => 'existing@example.com', 'name' => 'Test'];
        $existingUser = new User($data);
        
        $this->mockRepository
            ->shouldReceive('findByEmail')
            ->once()
            ->with($data['email'])
            ->andReturn($existingUser);
        
        $this->mockLogger->shouldReceive('debug')->once();
        
        // Act & Assert
        $this->expectException(ConflictException::class);
        $this->expectExceptionMessage('Email already in use');
        
        $this->userService->createUser($data);
    }
}
```

### 3. Feature Test (Laravel)

```php
<?php

namespace Tests\Feature;

use App\Models\User;
use Illuminate\Foundation\Testing\RefreshDatabase;
use Tests\TestCase;

class UserManagementTest extends TestCase
{
    use RefreshDatabase;
    
    public function test_can_create_user(): void
    {
        // Arrange
        $data = [
            'email' => 'test@example.com',
            'name' => 'Test User',
            'age' => 25,
        ];
        
        // Act
        $response = $this->postJson('/api/users', $data);
        
        // Assert
        $response->assertStatus(201);
        $response->assertJson([
            'data' => [
                'email' => $data['email'],
                'name' => $data['name'],
            ],
        ]);
        
        $this->assertDatabaseHas('users', [
            'email' => $data['email'],
        ]);
    }
    
    public function test_cannot_create_user_with_invalid_email(): void
    {
        // Arrange
        $data = [
            'email' => 'invalid-email',
            'name' => 'Test User',
        ];
        
        // Act
        $response = $this->postJson('/api/users', $data);
        
        // Assert
        $response->assertStatus(422);
        $response->assertJsonValidationErrors(['email']);
    }
}
```

### 4. Coverage Requirements

- **Overall**: > 90%
- **Unit Tests**: > 95%
- **Integration Tests**: > 85%

**Check coverage**:

```bash
# With PCOV (faster)
composer require --dev pcov/clobber
vendor/bin/pcov clobber
phpunit --coverage-html coverage

# Or with Xdebug
XDEBUG_MODE=coverage phpunit --coverage-html coverage
```

---

## Build & Deployment

### 1. Build Process

```bash
# Install dependencies (production)
composer install --no-dev --optimize-autoloader

# Optimize autoloader
composer dump-autoload --optimize --classmap-authoritative

# Laravel specific
php artisan config:cache
php artisan route:cache
php artisan view:cache
```

### 2. Docker Support

**Dockerfile (Laravel)**:

```dockerfile
# Build stage
FROM composer:latest AS build

WORKDIR /app

COPY composer.json composer.lock ./
RUN composer install --no-dev --optimize-autoloader --no-scripts

COPY . .
RUN composer dump-autoload --optimize --classmap-authoritative

# Production stage
FROM php:8.3-fpm-alpine

WORKDIR /var/www/html

# Install PHP extensions
RUN apk add --no-cache \
    postgresql-dev \
    libpng-dev \
    libzip-dev \
    && docker-php-ext-install pdo_pgsql gd zip bcmath

# Copy application
COPY --from=build /app /var/www/html

# Set permissions
RUN chown -R www-data:www-data /var/www/html/storage /var/www/html/bootstrap/cache

# Expose port
EXPOSE 9000

CMD ["php-fpm"]
```

### 3. Publishing to Packagist

```bash
# Create package
composer init

# Tag version
git tag -a v1.0.0 -m "Release v1.0.0"
git push origin v1.0.0

# Submit to Packagist.org
# Visit https://packagist.org/packages/submit
```

**Before publishing**:
- [ ] composer.json properly configured
- [ ] README.md with installation instructions
- [ ] LICENSE file
- [ ] All tests passing
- [ ] Documentation complete
- [ ] Git tag created

---

## Documentation

### 1. PHPDoc Comments

```php
<?php

/**
 * Process a payment transaction
 *
 * This method handles the complete payment workflow including
 * validation, authorization, and confirmation.
 *
 * @param string $userId The user's unique identifier
 * @param int $amount Payment amount in cents (must be positive)
 * @param string $currency ISO 4217 currency code (e.g., "USD", "EUR")
 * @return PaymentResult Payment confirmation with transaction details
 * @throws ValidationException If amount is negative or currency is invalid
 * @throws InsufficientFundsException If user balance is insufficient
 * @throws PaymentGatewayException If payment gateway is unavailable
 *
 * @example
 * ```php
 * $result = $service->processPayment('user-123', 1000, 'USD');
 * echo "Transaction ID: " . $result->getTransactionId();
 * ```
 *
 * @since 1.0.0
 * @see PaymentResult
 */
public function processPayment(string $userId, int $amount, string $currency): PaymentResult
{
    // Implementation
}
```

### 2. Generate Documentation

**Using phpDocumentor**:

```bash
# Install
composer require --dev phpdocumentor/phpdocumentor

# Generate docs
vendor/bin/phpdoc -d src -t docs/api
```

**Using ApiGen**:

```bash
# Install
composer require --dev apigen/apigen

# Generate docs
vendor/bin/apigen src --destination docs/api
```

---

## PHP Best Practices

See [BEST_PRACTICES.md](BEST_PRACTICES.md) for complete guide.

### Quick Tips

1. **Use strict types** (`declare(strict_types=1)`)
2. **Use type declarations** for parameters and return types
3. **Follow PSR-12** coding standard
4. **Use composer** for dependency management
5. **Use readonly properties** (PHP 8.1+)
6. **Use enums** (PHP 8.1+)
7. **Use named arguments** (PHP 8.0+)
8. **Avoid global state**
9. **Use dependency injection**
10. **Follow SOLID principles**

---

## Common Patterns

Refer to [Source Code Standards](#source-code-standards) and [BEST_PRACTICES.md](BEST_PRACTICES.md) for:
- Repository Pattern
- Service Layer Pattern
- Dependency Injection (Laravel Container)
- Factory Pattern
- Builder Pattern
- Observer Pattern (Laravel Events)
- Middleware Pattern

---

## Troubleshooting

### Common PHP Issues

#### Issue: Class not found

**Solution**:
```bash
composer dump-autoload
```

#### Issue: Permission denied (storage/cache)

**Solution**:
```bash
chmod -R 775 storage bootstrap/cache
chown -R www-data:www-data storage bootstrap/cache
```

#### Issue: Memory limit exceeded

**Solution**:
```bash
# Increase memory limit
php -d memory_limit=512M artisan command
```

#### Issue: Composer install fails

**Solution**:
```bash
composer clear-cache
composer install --no-cache
```

---

## Complete Workflow

Follow the [AI Integration Manual Template](../templates/AI_INTEGRATION_MANUAL_TEMPLATE.md) for the complete 6-phase workflow, using PHP-specific configurations and commands from this manual.

### Quick Reference

1. **Planning**: Use templates from `gov/manuals/templates/`
2. **Setup**: Follow [PHP-Specific Setup](#php-specific-setup)
3. **Implementation**: Follow [Source Code Standards](#source-code-standards)
4. **Testing**: Follow [Testing Standards](#testing-standards)
5. **Build**: Follow [Build & Deployment](#build--deployment)
6. **Documentation**: Follow [Documentation](#documentation)

---

## Additional Resources

- [PHP Official Documentation](https://www.php.net/docs.php)
- [Laravel Documentation](https://laravel.com/docs)
- [Symfony Documentation](https://symfony.com/doc/current/index.html)
- [PHP The Right Way](https://phptherightway.com/)
- [PSR Standards](https://www.php-fig.org/psr/)
- [PHPUnit Documentation](https://phpunit.de/documentation.html)
- [BEST_PRACTICES.md](BEST_PRACTICES.md) - This project's best practices

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-10-11 | Initial PHP manual |

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

