# C# Best Practices

**Version**: 1.0.0  
**Last Updated**: 2025-10-11  
**Target Audience**: AI Agents developing C# projects

---

## Table of Contents

1. [C# Idioms](#c-idioms)
2. [Anti-Patterns](#anti-patterns)
3. [Performance Optimization](#performance-optimization)
4. [Security Best Practices](#security-best-practices)
5. [Error Handling](#error-handling)
6. [Async/Await Patterns](#asyncawait-patterns)
7. [LINQ Best Practices](#linq-best-practices)
8. [Code Organization](#code-organization)
9. [Testing Best Practices](#testing-best-practices)
10. [Common Gotchas](#common-gotchas)

---

## C# Idioms

### 1. Use Nullable Reference Types

```csharp
// Enable in .csproj
<Nullable>enable</Nullable>

// ❌ BAD: Potential null reference
public class UserService
{
    private IUserRepository _repository;  // Warning: Non-nullable field
}

// ✅ GOOD: Explicit nullability
public class UserService
{
    private readonly IUserRepository _repository;
    
    public UserService(IUserRepository repository)
    {
        _repository = repository ?? throw new ArgumentNullException(nameof(repository));
    }
}
```

### 2. Use Records for DTOs

```csharp
// ❌ BAD: Verbose class
public class UserDto
{
    public Guid Id { get; set; }
    public string Name { get; set; }
    public string Email { get; set; }
    
    public override bool Equals(object obj) { /* ... */ }
    public override int GetHashCode() { /* ... */ }
}

// ✅ GOOD: Concise record
public record UserDto(Guid Id, string Name, string Email);

// ✅ GOOD: Record with validation
public record CreateUserDto
{
    public required string Name { get; init; }
    public required string Email { get; init; }
    public int? Age { get; init; }
}
```

### 3. Use Pattern Matching

```csharp
// ❌ BAD: Traditional if-else
if (obj is string)
{
    string str = (string)obj;
    Console.WriteLine(str.Length);
}

// ✅ GOOD: Pattern matching
if (obj is string str)
{
    Console.WriteLine(str.Length);
}

// ✅ GOOD: Switch expressions
var result = status switch
{
    Status.Pending => "Waiting",
    Status.Approved => "Done",
    Status.Rejected => "Denied",
    _ => throw new ArgumentException("Unknown status")
};
```

### 4. Use Expression-Bodied Members

```csharp
// ❌ BAD: Verbose property
public string FullName
{
    get { return $"{FirstName} {LastName}"; }
}

// ✅ GOOD: Expression-bodied property
public string FullName => $"{FirstName} {LastName}";

// ✅ GOOD: Expression-bodied method
public override string ToString() => $"User: {Name}";
```

### 5. Use String Interpolation

```csharp
// ❌ BAD: String concatenation
string message = "Hello, " + name + "!";

// ✅ GOOD: String interpolation
string message = $"Hello, {name}!";

// ✅ GOOD: Formatted interpolation
string message = $"Total: {amount:C2}";  // Currency format
```

### 6. Use Collection Expressions (C# 12+)

```csharp
// ❌ BAD: Verbose initialization
List<int> numbers = new List<int> { 1, 2, 3 };
int[] array = new int[] { 1, 2, 3 };

// ✅ GOOD: Collection expressions
List<int> numbers = [1, 2, 3];
int[] array = [1, 2, 3];
Span<int> span = [1, 2, 3];
```

### 7. Use Primary Constructors (C# 12+)

```csharp
// ❌ BAD: Traditional constructor
public class UserService
{
    private readonly IUserRepository _repository;
    
    public UserService(IUserRepository repository)
    {
        _repository = repository;
    }
}

// ✅ GOOD: Primary constructor
public class UserService(IUserRepository repository)
{
    public async Task<User> GetUserAsync(Guid id) =>
        await repository.GetByIdAsync(id);
}
```

### 8. Use `using` for IDisposable

```csharp
// ❌ BAD: Manual disposal
var connection = new SqlConnection(connectionString);
try
{
    connection.Open();
    // Use connection
}
finally
{
    connection.Dispose();
}

// ✅ GOOD: using statement
using (var connection = new SqlConnection(connectionString))
{
    connection.Open();
    // Use connection
}  // Automatically disposed

// ✅ BETTER: using declaration (C# 8+)
using var connection = new SqlConnection(connectionString);
connection.Open();
// Use connection
// Disposed at end of scope
```

### 9. Use Null-Coalescing Operators

```csharp
// ❌ BAD: Verbose null check
string name = user.Name != null ? user.Name : "Unknown";

// ✅ GOOD: Null-coalescing
string name = user.Name ?? "Unknown";

// ✅ GOOD: Null-coalescing assignment
_cache ??= new Dictionary<string, object>();

// ✅ GOOD: Null-conditional operator
int? length = user.Name?.Length;
```

### 10. Use Target-Typed New (C# 9+)

```csharp
// ❌ BAD: Redundant type
Dictionary<string, List<int>> dict = new Dictionary<string, List<int>>();

// ✅ GOOD: Target-typed new
Dictionary<string, List<int>> dict = new();
```

---

## Anti-Patterns

### 1. Returning Null from Collections

```csharp
// ❌ BAD: Caller must check for null
public List<User>? GetUsers()
{
    if (noUsers)
        return null;
    return users;
}

// ✅ GOOD: Return empty collection
public List<User> GetUsers()
{
    return users ?? new List<User>();
}

// ✅ BETTER: Use Enumerable.Empty
public IEnumerable<User> GetUsers()
{
    return users ?? Enumerable.Empty<User>();
}
```

### 2. Catching Generic Exception

```csharp
// ❌ BAD: Catches everything
try
{
    RiskyOperation();
}
catch (Exception ex)
{
    // Too broad
}

// ✅ GOOD: Specific exceptions
try
{
    RiskyOperation();
}
catch (IOException ex)
{
    _logger.LogError(ex, "IO error occurred");
}
catch (SqlException ex)
{
    _logger.LogError(ex, "Database error");
    throw;
}
```

### 3. Async Void Methods

```csharp
// ❌ BAD: Async void (except event handlers)
public async void ProcessData()
{
    await Task.Delay(1000);
}

// ✅ GOOD: Async Task
public async Task ProcessDataAsync()
{
    await Task.Delay(1000);
}

// ✅ ACCEPTABLE: Event handler
private async void Button_Click(object sender, EventArgs e)
{
    await ProcessDataAsync();
}
```

### 4. Not Disposing IDisposable

```csharp
// ❌ BAD: Resource leak
var stream = new FileStream("file.txt", FileMode.Open);
// Forgot to dispose

// ✅ GOOD: Using statement
using var stream = new FileStream("file.txt", FileMode.Open);
```

### 5. String Concatenation in Loops

```csharp
// ❌ BAD: Creates many string objects
string result = "";
foreach (var item in items)
{
    result += item;
}

// ✅ GOOD: Use StringBuilder
var result = new StringBuilder();
foreach (var item in items)
{
    result.Append(item);
}
return result.ToString();

// ✅ BETTER: Use String.Join
string result = string.Join("", items);
```

### 6. Ignoring CancellationToken

```csharp
// ❌ BAD: Ignores cancellation
public async Task<User> GetUserAsync(Guid id)
{
    return await _repository.GetByIdAsync(id);
}

// ✅ GOOD: Pass cancellation token
public async Task<User> GetUserAsync(Guid id, CancellationToken cancellationToken = default)
{
    return await _repository.GetByIdAsync(id, cancellationToken);
}
```

---

## Performance Optimization

### 1. Use Span<T> for Performance

```csharp
// ❌ SLOW: Creates substring
string input = "Hello World";
string result = input.Substring(0, 5);

// ✅ FAST: No allocation
ReadOnlySpan<char> span = input.AsSpan(0, 5);
```

### 2. Use ValueTask for Hot Paths

```csharp
// ✅ GOOD: ValueTask for frequently completed synchronously
public async ValueTask<User?> GetCachedUserAsync(Guid id)
{
    if (_cache.TryGetValue(id, out var user))
    {
        return user;  // Synchronous completion
    }
    
    return await _repository.GetByIdAsync(id);
}
```

### 3. Use ArrayPool for Temporary Arrays

```csharp
using System.Buffers;

// ✅ GOOD: Rent from pool
byte[] buffer = ArrayPool<byte>.Shared.Rent(1024);
try
{
    // Use buffer
}
finally
{
    ArrayPool<byte>.Shared.Return(buffer);
}
```

### 4. Avoid Boxing

```csharp
// ❌ BAD: Boxing
int value = 42;
object obj = value;  // Boxing

// ✅ GOOD: Use generics
public void Process<T>(T value) where T : struct
{
    // No boxing
}
```

### 5. Use struct for Small, Immutable Types

```csharp
// ✅ GOOD: Value type for small data
public readonly record struct Point(int X, int Y);

// Allocated on stack, not heap
var point = new Point(10, 20);
```

---

## Security Best Practices

### 1. Input Validation with FluentValidation

```csharp
using FluentValidation;

public class CreateUserDtoValidator : AbstractValidator<CreateUserDto>
{
    public CreateUserDtoValidator()
    {
        RuleFor(x => x.Email)
            .NotEmpty().WithMessage("Email is required")
            .EmailAddress().WithMessage("Invalid email format");
        
        RuleFor(x => x.Name)
            .NotEmpty().WithMessage("Name is required")
            .MinimumLength(2).WithMessage("Name must be at least 2 characters")
            .MaximumLength(100).WithMessage("Name must not exceed 100 characters");
        
        RuleFor(x => x.Age)
            .GreaterThanOrEqualTo(0).WithMessage("Age must be positive")
            .LessThanOrEqualTo(150).WithMessage("Age must be realistic");
    }
}
```

### 2. SQL Injection Prevention

```csharp
// ❌ BAD: String concatenation
public User GetUser(string userId)
{
    var query = $"SELECT * FROM Users WHERE Id = '{userId}'";
    return _context.Users.FromSqlRaw(query).FirstOrDefault();
}

// ✅ GOOD: Parameterized query
public User GetUser(string userId)
{
    return _context.Users
        .FromSqlInterpolated($"SELECT * FROM Users WHERE Id = {userId}")
        .FirstOrDefault();
}

// ✅ BETTER: Use LINQ
public async Task<User?> GetUserAsync(Guid id)
{
    return await _context.Users.FindAsync(id);
}
```

### 3. Secrets Management

```csharp
// ❌ BAD: Hardcoded secrets
private const string ApiKey = "sk-1234567890";

// ✅ GOOD: Configuration
public class MyService
{
    private readonly string _apiKey;
    
    public MyService(IConfiguration configuration)
    {
        _apiKey = configuration["ApiKey"] 
            ?? throw new InvalidOperationException("ApiKey not configured");
    }
}

// ✅ BETTER: Use Secret Manager (development)
// dotnet user-secrets set "ApiKey" "your-key-here"

// ✅ PRODUCTION: Use Azure Key Vault
builder.Configuration.AddAzureKeyVault(/* ... */);
```

### 4. Use HTTPS and Secure Cookies

```csharp
// Program.cs
app.UseHttpsRedirection();

app.UseCookiePolicy(new CookiePolicyOptions
{
    HttpOnly = Microsoft.AspNetCore.CookiePolicy.HttpOnlyPolicy.Always,
    Secure = CookieSecurePolicy.Always,
    MinimumSameSitePolicy = SameSiteMode.Strict
});
```

---

## Async/Await Patterns

### 1. Always Use Async/Await for I/O

```csharp
// ❌ BAD: Blocking I/O
public User GetUser(Guid id)
{
    return _repository.GetByIdAsync(id).Result;  // Deadlock risk!
}

// ✅ GOOD: Async all the way
public async Task<User> GetUserAsync(Guid id, CancellationToken cancellationToken = default)
{
    return await _repository.GetByIdAsync(id, cancellationToken);
}
```

### 2. Use Task.WhenAll for Parallel Operations

```csharp
// ❌ BAD: Sequential
var user = await GetUserAsync();
var posts = await GetPostsAsync();

// ✅ GOOD: Parallel
var (user, posts) = await Task.WhenAll(GetUserAsync(), GetPostsAsync());

// Or
var userTask = GetUserAsync();
var postsTask = GetPostsAsync();
await Task.WhenAll(userTask, postsTask);
var user = await userTask;
var posts = await postsTask;
```

### 3. Use ConfigureAwait(false) in Libraries

```csharp
// In library code (not UI)
public async Task<User> GetUserAsync(Guid id)
{
    var user = await _repository.GetByIdAsync(id).ConfigureAwait(false);
    return user;
}
```

### 4. Avoid async void

```csharp
// ❌ BAD: Can't catch exceptions
public async void ProcessData()
{
    await Task.Delay(1000);
    throw new Exception();  // Unhandled!
}

// ✅ GOOD: Return Task
public async Task ProcessDataAsync()
{
    await Task.Delay(1000);
}
```

---

## LINQ Best Practices

### 1. Use LINQ Efficiently

```csharp
// ❌ BAD: Multiple enumerations
var users = GetUsers().ToList();
var count = users.Count();  // Enumerates again
var first = users.FirstOrDefault();  // Enumerates again

// ✅ GOOD: Single enumeration
var users = GetUsers().ToList();
var count = users.Count;  // List.Count property
var first = users.FirstOrDefault();  // Indexed access
```

### 2. Use Any() Instead of Count() > 0

```csharp
// ❌ BAD: Enumerates entire collection
if (users.Count() > 0)

// ✅ GOOD: Stops at first item
if (users.Any())
```

### 3. Use FirstOrDefault Carefully

```csharp
// ⚠️ CAUTION: Returns null for reference types, default for value types
var user = users.FirstOrDefault();  // null if not found

// ✅ GOOD: Handle null
var user = users.FirstOrDefault();
if (user is null)
{
    throw new NotFoundException();
}

// ✅ BETTER: Use First with try-catch
try
{
    var user = users.First();
}
catch (InvalidOperationException)
{
    throw new NotFoundException();
}
```

### 4. Prefer Query Syntax for Complex Queries

```csharp
// ✅ GOOD: Method syntax for simple queries
var adults = users.Where(u => u.Age >= 18);

// ✅ GOOD: Query syntax for complex queries
var result = from user in users
             join post in posts on user.Id equals post.UserId
             where user.Age >= 18
             orderby user.Name
             select new { user.Name, PostCount = post.Count() };
```

---

## Code Organization

### 1. Use Dependency Injection

```csharp
// ❌ BAD: Hard dependency
public class UserService
{
    private readonly UserRepository _repository = new UserRepository();
}

// ✅ GOOD: Constructor injection
public class UserService
{
    private readonly IUserRepository _repository;
    
    public UserService(IUserRepository repository)
    {
        _repository = repository;
    }
}

// Register in Program.cs
builder.Services.AddScoped<IUserRepository, UserRepository>();
builder.Services.AddScoped<UserService>();
```

### 2. Use Interfaces for Abstraction

```csharp
// ✅ GOOD: Define interface
public interface IUserRepository
{
    Task<User?> GetByIdAsync(Guid id, CancellationToken cancellationToken = default);
    Task<User> CreateAsync(User user, CancellationToken cancellationToken = default);
}

// ✅ GOOD: Multiple implementations
public class SqlUserRepository : IUserRepository { }
public class MongoUserRepository : IUserRepository { }
public class InMemoryUserRepository : IUserRepository { }
```

### 3. Follow SOLID Principles

```csharp
// Single Responsibility
public class UserService  // Only handles user operations
public class EmailService  // Only handles emails

// Open/Closed
public abstract class Shape
{
    public abstract double Area();
}

// Liskov Substitution
IUserRepository repository = new SqlUserRepository();  // Can substitute

// Interface Segregation
public interface IReadRepository<T> { }
public interface IWriteRepository<T> { }

// Dependency Inversion
public class UserService
{
    private readonly IUserRepository _repository;  // Depends on abstraction
}
```

---

## Testing Best Practices

### 1. Use Fluent Assertions

```csharp
// ❌ OK: Traditional assertions
Assert.NotNull(user);
Assert.Equal("test@example.com", user.Email);

// ✅ BETTER: Fluent assertions
user.Should().NotBeNull();
user.Email.Should().Be("test@example.com");
user.Age.Should().BeGreaterThan(0);
```

### 2. Use Theory for Parameterized Tests

```csharp
[Theory]
[InlineData("", false)]
[InlineData("invalid", false)]
[InlineData("test@example.com", true)]
public void ValidateEmail_ShouldReturnExpectedResult(string email, bool expected)
{
    var result = EmailValidator.IsValid(email);
    result.Should().Be(expected);
}
```

### 3. Use AutoFixture for Test Data

```csharp
using AutoFixture;

[Fact]
public void Test_WithAutoFixture()
{
    // Arrange
    var fixture = new Fixture();
    var user = fixture.Create<User>();  // Auto-generated data
    
    // Act & Assert
    user.Should().NotBeNull();
    user.Email.Should().NotBeNullOrEmpty();
}
```

---

## Common Gotchas

### 1. Reference vs Value Types

```csharp
// class = reference type (heap)
public class Person { public string Name { get; set; } }

// struct = value type (stack)
public struct Point { public int X; public int Y; }

// record class = reference type
public record PersonRecord(string Name);

// record struct = value type
public record struct PointRecord(int X, int Y);
```

### 2. String Comparison

```csharp
string str1 = "hello";
string str2 = "HELLO";

str1 == str2;  // false (case-sensitive)
str1.Equals(str2, StringComparison.OrdinalIgnoreCase);  // true
```

### 3. DateTime vs DateTimeOffset

```csharp
// ❌ BAD: DateTime (ambiguous timezone)
DateTime now = DateTime.Now;

// ✅ GOOD: DateTimeOffset (includes timezone)
DateTimeOffset now = DateTimeOffset.UtcNow;

// ✅ GOOD: DateTime with UTC
DateTime now = DateTime.UtcNow;
```

### 4. Decimal for Money

```csharp
// ❌ BAD: Floating point for money
double price = 19.99;
double total = price * 1.1;  // Precision errors

// ✅ GOOD: Decimal for money
decimal price = 19.99m;
decimal total = price * 1.1m;
```

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-10-11 | Initial C# best practices guide |

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

