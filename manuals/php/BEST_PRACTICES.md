# PHP Best Practices

**Version**: 1.0.0  
**Last Updated**: 2025-10-11  
**Target Audience**: AI Agents developing PHP projects

---

## Table of Contents

1. [PHP Idioms](#php-idioms)
2. [Anti-Patterns](#anti-patterns)
3. [Performance Optimization](#performance-optimization)
4. [Security Best Practices](#security-best-practices)
5. [Error Handling](#error-handling)
6. [Modern PHP Features](#modern-php-features)
7. [PSR Standards](#psr-standards)
8. [Code Organization](#code-organization)
9. [Testing Best Practices](#testing-best-practices)
10. [Common Gotchas](#common-gotchas)

---

## PHP Idioms

### 1. Use Strict Types

```php
<?php

declare(strict_types=1);

// ❌ BAD: No strict types
function add($a, $b) {
    return $a + $b;
}
add("5", "10");  // Returns 15 (type juggling)

// ✅ GOOD: With strict types
function add(int $a, int $b): int {
    return $a + $b;
}
add("5", "10");  // TypeError
```

### 2. Use Type Declarations

```php
<?php

// ❌ BAD: No types
function getUser($id) {
    return $this->repository->find($id);
}

// ✅ GOOD: Type declarations
function getUser(string $id): ?User {
    return $this->repository->find($id);
}
```

### 3. Use readonly Properties (PHP 8.1+)

```php
<?php

// ❌ BAD: Mutable properties
class User {
    public string $id;
    public string $email;
}

// ✅ GOOD: Readonly properties
class User {
    public function __construct(
        public readonly string $id,
        public readonly string $email
    ) {
    }
}
```

### 4. Use Enums (PHP 8.1+)

```php
<?php

// ❌ BAD: Class constants
class Status {
    public const PENDING = 'pending';
    public const APPROVED = 'approved';
    public const REJECTED = 'rejected';
}

// ✅ GOOD: Enum
enum Status: string {
    case PENDING = 'pending';
    case APPROVED = 'approved';
    case REJECTED = 'rejected';
    
    public function label(): string {
        return match($this) {
            self::PENDING => 'Pending Review',
            self::APPROVED => 'Approved',
            self::REJECTED => 'Rejected',
        };
    }
}
```

### 5. Use Named Arguments (PHP 8.0+)

```php
<?php

// ❌ BAD: Positional arguments (unclear)
createUser('test@example.com', 'Test', 25, true, false);

// ✅ GOOD: Named arguments
createUser(
    email: 'test@example.com',
    name: 'Test',
    age: 25,
    sendEmail: true,
    isAdmin: false
);
```

### 6. Use Null-Safe Operator (PHP 8.0+)

```php
<?php

// ❌ BAD: Manual null checks
$country = null;
if ($user !== null) {
    if ($user->address !== null) {
        $country = $user->address->country;
    }
}

// ✅ GOOD: Null-safe operator
$country = $user?->address?->country;
```

### 7. Use Match Expression (PHP 8.0+)

```php
<?php

// ❌ BAD: Switch statement
switch ($status) {
    case 'pending':
        $message = 'Waiting';
        break;
    case 'approved':
        $message = 'Done';
        break;
    default:
        $message = 'Unknown';
}

// ✅ GOOD: Match expression
$message = match($status) {
    'pending' => 'Waiting',
    'approved' => 'Done',
    default => 'Unknown',
};
```

### 8. Use Constructor Property Promotion (PHP 8.0+)

```php
<?php

// ❌ BAD: Verbose constructor
class UserService {
    private UserRepository $repository;
    private LoggerInterface $logger;
    
    public function __construct(
        UserRepository $repository,
        LoggerInterface $logger
    ) {
        $this->repository = $repository;
        $this->logger = $logger;
    }
}

// ✅ GOOD: Property promotion
class UserService {
    public function __construct(
        private readonly UserRepository $repository,
        private readonly LoggerInterface $logger
    ) {
    }
}
```

### 9. Use Array Destructuring

```php
<?php

// ❌ BAD: Individual assignment
$user = getUserData();
$name = $user['name'];
$email = $user['email'];

// ✅ GOOD: Array destructuring
['name' => $name, 'email' => $email] = getUserData();
```

### 10. Use Spaceship Operator

```php
<?php

// ❌ BAD: Verbose comparison
usort($array, function($a, $b) {
    if ($a == $b) return 0;
    return ($a < $b) ? -1 : 1;
});

// ✅ GOOD: Spaceship operator
usort($array, fn($a, $b) => $a <=> $b);
```

---

## Anti-Patterns

### 1. Not Using Prepared Statements

```php
<?php

// ❌ BAD: SQL injection vulnerability
$userId = $_GET['id'];
$query = "SELECT * FROM users WHERE id = '$userId'";
$result = mysqli_query($conn, $query);

// ✅ GOOD: Prepared statement
$stmt = $pdo->prepare("SELECT * FROM users WHERE id = :id");
$stmt->execute(['id' => $userId]);

// ✅ BETTER: Use ORM
$user = User::find($userId);
```

### 2. Suppressing Errors with @

```php
<?php

// ❌ BAD: Suppresses errors
$file = @file_get_contents('nonexistent.txt');

// ✅ GOOD: Handle errors properly
try {
    $file = file_get_contents('file.txt');
} catch (\Exception $e) {
    $logger->error('Failed to read file', ['error' => $e->getMessage()]);
}
```

### 3. Using extract()

```php
<?php

// ❌ BAD: Unpredictable variable creation
extract($_POST);  // Creates variables from user input!

// ✅ GOOD: Explicit variable assignment
$name = $_POST['name'] ?? null;
$email = $_POST['email'] ?? null;
```

### 4. Not Validating User Input

```php
<?php

// ❌ BAD: Direct use of user input
$email = $_POST['email'];
User::create(['email' => $email]);

// ✅ GOOD: Validate first
$validated = $request->validate([
    'email' => 'required|email|unique:users',
]);
User::create($validated);
```

### 5. Using == Instead of ===

```php
<?php

// ❌ BAD: Type-coercing comparison
if ($value == 0) {  // true for "", "0", false, null, []
    // ...
}

// ✅ GOOD: Strict comparison
if ($value === 0) {  // Only true for integer 0
    // ...
}
```

### 6. Not Using Dependency Injection

```php
<?php

// ❌ BAD: Hard dependency
class UserService {
    private $repository;
    
    public function __construct() {
        $this->repository = new UserRepository();
    }
}

// ✅ GOOD: Dependency injection
class UserService {
    public function __construct(
        private readonly UserRepositoryInterface $repository
    ) {
    }
}
```

---

## Performance Optimization

### 1. Use OpCache

```ini
; php.ini
opcache.enable=1
opcache.memory_consumption=128
opcache.interned_strings_buffer=8
opcache.max_accelerated_files=10000
opcache.validate_timestamps=0  ; Production only
opcache.save_comments=1
opcache.fast_shutdown=1
```

### 2. Eager Loading (Laravel)

```php
<?php

// ❌ BAD: N+1 query problem
$users = User::all();
foreach ($users as $user) {
    echo $user->posts->count();  // Queries posts for each user
}

// ✅ GOOD: Eager loading
$users = User::with('posts')->get();
foreach ($users as $user) {
    echo $user->posts->count();  // No additional queries
}
```

### 3. Use Generators for Large Datasets

```php
<?php

// ❌ BAD: Loads all in memory
function getAllUsers(): array {
    return User::all()->toArray();  // Loads 1M users
}

// ✅ GOOD: Generator
function getAllUsers(): \Generator {
    foreach (User::cursor() as $user) {
        yield $user;
    }
}
```

### 4. Cache Expensive Operations

```php
<?php

// ✅ GOOD: Cache results (Laravel)
$value = Cache::remember('users.all', 3600, function () {
    return User::all();
});

// ✅ GOOD: Redis caching
$redis->setex('user:123', 3600, json_encode($user));
$cached = json_decode($redis->get('user:123'), true);
```

---

## Security Best Practices

### 1. Prevent XSS

```php
<?php

// ❌ BAD: Unescaped output
echo $_POST['comment'];

// ✅ GOOD: Escape output
echo htmlspecialchars($_POST['comment'], ENT_QUOTES, 'UTF-8');

// ✅ GOOD: In Blade (Laravel)
{{ $comment }}  // Auto-escaped
{!! $html !!}   // Unescaped (use carefully)
```

### 2. Prevent CSRF

```php
<?php

// ✅ GOOD: Laravel CSRF protection (automatic)
<form method="POST" action="/users">
    @csrf
    <!-- form fields -->
</form>
```

### 3. Password Hashing

```php
<?php

// ❌ BAD: Plain text or MD5
$password = md5($_POST['password']);

// ✅ GOOD: Use password_hash
$hash = password_hash($_POST['password'], PASSWORD_ARGON2ID);

// Verify
if (password_verify($input, $hash)) {
    // Correct password
}

// ✅ GOOD: Laravel Hash facade
use Illuminate\Support\Facades\Hash;

$hash = Hash::make($password);
Hash::check($input, $hash);
```

### 4. Sanitize File Uploads

```php
<?php

// ✅ GOOD: Validate file uploads
$request->validate([
    'avatar' => 'required|image|mimes:jpeg,png,jpg|max:2048',
]);

// Store safely
$path = $request->file('avatar')->store('avatars', 'public');
```

### 5. Rate Limiting (Laravel)

```php
<?php

use Illuminate\Support\Facades\RateLimiter;

// In RouteServiceProvider
RateLimiter::for('api', function (Request $request) {
    return Limit::perMinute(60)->by($request->user()?->id ?: $request->ip());
});

// Apply to routes
Route::middleware(['throttle:api'])->group(function () {
    // Routes
});
```

---

## PSR Standards

### Follow PSR-12 (Extended Coding Style)

```php
<?php

declare(strict_types=1);

namespace App\Services;

use App\Repositories\UserRepositoryInterface;
use Psr\Log\LoggerInterface;

/**
 * User service class
 */
class UserService
{
    public function __construct(
        private readonly UserRepositoryInterface $repository,
        private readonly LoggerInterface $logger
    ) {
    }
    
    public function createUser(array $data): User
    {
        // Method body
    }
}
```

### Follow PSR-4 (Autoloading)

```json
{
    "autoload": {
        "psr-4": {
            "App\\": "src/",
            "App\\Domain\\": "src/Domain/"
        }
    }
}
```

### Follow PSR-7 (HTTP Messages)

```php
<?php

use Psr\Http\Message\ResponseInterface;
use Psr\Http\Message\ServerRequestInterface;

function handle(ServerRequestInterface $request): ResponseInterface
{
    // Handle request
}
```

---

## Testing Best Practices

### 1. Use Data Providers

```php
<?php

class ValidatorTest extends TestCase
{
    /**
     * @dataProvider emailProvider
     */
    public function test_email_validation(string $email, bool $expected): void
    {
        $result = EmailValidator::isValid($email);
        $this->assertEquals($expected, $result);
    }
    
    public static function emailProvider(): array
    {
        return [
            ['test@example.com', true],
            ['invalid', false],
            ['', false],
        ];
    }
}
```

### 2. Use setUp and tearDown

```php
<?php

class UserServiceTest extends TestCase
{
    private UserService $service;
    private UserRepository $repository;
    
    protected function setUp(): void
    {
        parent::setUp();
        $this->repository = $this->createMock(UserRepository::class);
        $this->service = new UserService($this->repository);
    }
    
    protected function tearDown(): void
    {
        parent::tearDown();
        Mockery::close();
    }
}
```

### 3. Use Assertions Appropriately

```php
<?php

// Specific assertions
$this->assertSame($expected, $actual);  // Strict (===)
$this->assertEquals($expected, $actual);  // Loose (==)
$this->assertTrue($value);
$this->assertNull($value);
$this->assertInstanceOf(User::class, $user);
$this->assertCount(5, $array);
$this->assertArrayHasKey('email', $data);
```

---

## Common Gotchas

### 1. Comparison Operators

```php
<?php

// == vs ===
0 == "0"    // true
0 === "0"   // false

0 == ""     // true
0 === ""    // false

null == false    // true
null === false   // false
```

### 2. Array vs Object

```php
<?php

$array = ['key' => 'value'];
$object = (object)['key' => 'value'];

$array['key'];   // 'value'
$object->key;    // 'value'
```

### 3. Include vs Require

```php
<?php

include 'file.php';    // Warning if file not found, continues
require 'file.php';    // Fatal error if file not found

include_once 'file.php';  // Includes once
require_once 'file.php';  // Requires once (common for configs)
```

### 4. References

```php
<?php

// Pass by value (default)
function increment($n) {
    $n++;
}
$x = 5;
increment($x);  // $x is still 5

// Pass by reference
function increment(&$n) {
    $n++;
}
$x = 5;
increment($x);  // $x is now 6
```

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-10-11 | Initial PHP best practices guide |

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

