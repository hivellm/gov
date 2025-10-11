# AI Integration Manual - C#

**Version**: 1.0.0  
**Last Updated**: 2025-10-11  
**Language**: C# 12 / .NET 8+  
**Target Audience**: AI Agents (LLM-based development assistants)

---

## Table of Contents

1. [Introduction](#introduction)
2. [Quick Start](#quick-start)
3. [C#-Specific Setup](#c-specific-setup)
4. [Configuration Standards](#configuration-standards)
5. [Source Code Standards](#source-code-standards)
6. [Testing Standards](#testing-standards)
7. [Build & Deployment](#build--deployment)
8. [Documentation](#documentation)
9. [C# Best Practices](#c-best-practices)
10. [Common Patterns](#common-patterns)
11. [Troubleshooting](#troubleshooting)
12. [Complete Workflow](#complete-workflow)

---

## Introduction

This manual extends the [AI Integration Manual Template](../templates/AI_INTEGRATION_MANUAL_TEMPLATE.md) with C#-specific implementations.

**When to use this manual**:
- C# / .NET applications
- ASP.NET Core web applications
- Web APIs (REST, gRPC)
- Desktop applications (WPF, WinForms)
- Xamarin/MAUI mobile apps
- Libraries/SDKs
- CLI tools

**Prerequisites**:
- Read [AI Integration Manual Template](../templates/AI_INTEGRATION_MANUAL_TEMPLATE.md)
- Read [Language Standards](../LANGUAGE_STANDARDS.md)
- Basic C# knowledge

---

## Quick Start

### Minimum Viable Project Setup

```bash
# 1. Create new console application
dotnet new console -n MyProject -f net8.0
cd MyProject

# 2. Create solution
dotnet new sln -n MyProject
dotnet sln add MyProject.csproj

# 3. Create test project
dotnet new xunit -n MyProject.Tests -f net8.0
dotnet sln add MyProject.Tests/MyProject.Tests.csproj
dotnet add MyProject.Tests reference MyProject.csproj

# 4. Build
dotnet build

# 5. Run
dotnet run

# 6. Test
dotnet test
```

### Web API Project

```bash
# Create Web API
dotnet new webapi -n MyProject.Api -f net8.0
cd MyProject.Api

# Add packages
dotnet add package Microsoft.EntityFrameworkCore
dotnet add package Microsoft.EntityFrameworkCore.Design
dotnet add package Npgsql.EntityFrameworkCore.PostgreSQL
dotnet add package Swashbuckle.AspNetCore

# Run
dotnet run
```

### Full Solution Structure

```bash
# Create solution
dotnet new sln -n MyProject

# Create projects
dotnet new classlib -n MyProject.Core -f net8.0
dotnet new classlib -n MyProject.Infrastructure -f net8.0
dotnet new webapi -n MyProject.Api -f net8.0
dotnet new xunit -n MyProject.Tests -f net8.0

# Add to solution
dotnet sln add **/*.csproj

# Add project references
dotnet add MyProject.Api reference MyProject.Core
dotnet add MyProject.Api reference MyProject.Infrastructure
dotnet add MyProject.Infrastructure reference MyProject.Core
dotnet add MyProject.Tests reference MyProject.Core
dotnet add MyProject.Tests reference MyProject.Api
```

---

## C#-Specific Setup

### 1. Environment Setup

#### Install .NET SDK

```bash
# Windows: Download from https://dotnet.microsoft.com/download
# Or use winget
winget install Microsoft.DotNet.SDK.8

# Linux (Ubuntu)
wget https://dot.net/v1/dotnet-install.sh
chmod +x dotnet-install.sh
./dotnet-install.sh --channel 8.0

# Mac
brew install dotnet-sdk

# Verify installation
dotnet --version
dotnet --list-sdks
```

#### Create global.json

```json
{
  "sdk": {
    "version": "8.0.0",
    "rollForward": "latestMinor"
  }
}
```

### 2. Essential NuGet Packages

#### Production Dependencies

```bash
# ASP.NET Core Web API
dotnet add package Microsoft.AspNetCore.OpenApi

# Entity Framework Core
dotnet add package Microsoft.EntityFrameworkCore
dotnet add package Microsoft.EntityFrameworkCore.Design
dotnet add package Npgsql.EntityFrameworkCore.PostgreSQL

# Validation
dotnet add package FluentValidation
dotnet add package FluentValidation.AspNetCore

# Logging
dotnet add package Serilog.AspNetCore
dotnet add package Serilog.Sinks.Console
dotnet add package Serilog.Sinks.File

# Caching
dotnet add package Microsoft.Extensions.Caching.StackExchangeRedis

# Authentication
dotnet add package Microsoft.AspNetCore.Authentication.JwtBearer

# AutoMapper
dotnet add package AutoMapper
dotnet add package AutoMapper.Extensions.Microsoft.DependencyInjection
```

#### Development Dependencies

```bash
# Testing
dotnet add package xunit
dotnet add package xunit.runner.visualstudio
dotnet add package Moq
dotnet add package FluentAssertions
dotnet add package Microsoft.AspNetCore.Mvc.Testing

# Code Coverage
dotnet add package coverlet.collector
dotnet add package coverlet.msbuild

# Code Analysis
dotnet add package StyleCop.Analyzers
dotnet add package SonarAnalyzer.CSharp
```

### 3. Development Tools

```bash
# Install EF Core CLI tools
dotnet tool install --global dotnet-ef

# Install code formatters
dotnet tool install --global dotnet-format

# Install ReportGenerator (coverage reports)
dotnet tool install --global dotnet-reportgenerator-globaltool
```

---

## Configuration Standards

### 1. Project File (.csproj)

**Complete Configuration**:

```xml
<Project Sdk="Microsoft.NET.Sdk.Web">

  <PropertyGroup>
    <TargetFramework>net8.0</TargetFramework>
    <Nullable>enable</Nullable>
    <ImplicitUsings>enable</ImplicitUsings>
    <TreatWarningsAsErrors>true</TreatWarningsAsErrors>
    <GenerateDocumentationFile>true</GenerateDocumentationFile>
    <NoWarn>$(NoWarn);1591</NoWarn>
    <Version>1.0.0</Version>
    <Authors>Author Name</Authors>
    <Company>HiveLLM</Company>
    <Product>MyProject</Product>
    <Description>Project description</Description>
    <Copyright>Copyright © 2025</Copyright>
    <PackageLicenseExpression>MIT</PackageLicenseExpression>
    <PackageProjectUrl>https://github.com/user/repo</PackageProjectUrl>
    <RepositoryUrl>https://github.com/user/repo</RepositoryUrl>
    <RepositoryType>git</RepositoryType>
    <PackageTags>csharp;dotnet;api</PackageTags>
  </PropertyGroup>

  <ItemGroup>
    <PackageReference Include="Microsoft.AspNetCore.OpenApi" Version="8.0.0" />
    <PackageReference Include="Swashbuckle.AspNetCore" Version="6.5.0" />
    <PackageReference Include="StyleCop.Analyzers" Version="1.2.0-beta.556">
      <PrivateAssets>all</PrivateAssets>
      <IncludeAssets>runtime; build; native; contentfiles; analyzers</IncludeAssets>
    </PackageReference>
  </ItemGroup>

</Project>
```

### 2. .editorconfig

**Code Style Configuration**:

```ini
root = true

[*]
charset = utf-8
end_of_line = lf
insert_final_newline = true
trim_trailing_whitespace = true

[*.{cs,csx,vb,vbx}]
indent_style = space
indent_size = 4

[*.{csproj,vbproj,vcxproj,proj,projitems,shproj}]
indent_size = 2

# C# files
[*.cs]

# Organize usings
dotnet_sort_system_directives_first = true
dotnet_separate_import_directive_groups = false

# this. preferences
dotnet_style_qualification_for_field = false:suggestion
dotnet_style_qualification_for_property = false:suggestion
dotnet_style_qualification_for_method = false:suggestion
dotnet_style_qualification_for_event = false:suggestion

# Language keywords vs BCL types preferences
dotnet_style_predefined_type_for_locals_parameters_members = true:suggestion
dotnet_style_predefined_type_for_member_access = true:suggestion

# Parentheses preferences
dotnet_style_parentheses_in_arithmetic_binary_operators = always_for_clarity:suggestion
dotnet_style_parentheses_in_relational_binary_operators = always_for_clarity:suggestion

# Modifier preferences
dotnet_style_require_accessibility_modifiers = for_non_interface_members:suggestion
dotnet_style_readonly_field = true:suggestion

# Expression-level preferences
dotnet_style_object_initializer = true:suggestion
dotnet_style_collection_initializer = true:suggestion
dotnet_style_explicit_tuple_names = true:suggestion
dotnet_style_prefer_inferred_tuple_names = true:suggestion
dotnet_style_prefer_inferred_anonymous_type_member_names = true:suggestion
dotnet_style_prefer_auto_properties = true:suggestion
dotnet_style_prefer_conditional_expression_over_assignment = true:suggestion
dotnet_style_prefer_conditional_expression_over_return = true:suggestion

# C# Coding Conventions
csharp_prefer_braces = true:suggestion
csharp_style_var_for_built_in_types = false:suggestion
csharp_style_var_when_type_is_apparent = true:suggestion
csharp_style_var_elsewhere = true:suggestion

# Expression-bodied members
csharp_style_expression_bodied_methods = false:suggestion
csharp_style_expression_bodied_constructors = false:suggestion
csharp_style_expression_bodied_operators = false:suggestion
csharp_style_expression_bodied_properties = true:suggestion
csharp_style_expression_bodied_indexers = true:suggestion
csharp_style_expression_bodied_accessors = true:suggestion

# Pattern matching preferences
csharp_style_pattern_matching_over_is_with_cast_check = true:suggestion
csharp_style_pattern_matching_over_as_with_null_check = true:suggestion

# Null-checking preferences
csharp_style_throw_expression = true:suggestion
csharp_style_conditional_delegate_call = true:suggestion

# Modifier preferences
csharp_preferred_modifier_order = public,private,protected,internal,static,extern,new,virtual,abstract,sealed,override,readonly,unsafe,volatile,async:suggestion

# Code-block preferences
csharp_prefer_simple_using_statement = true:suggestion

# Naming conventions
dotnet_naming_rule.interface_should_be_begins_with_i.severity = suggestion
dotnet_naming_rule.interface_should_be_begins_with_i.symbols = interface
dotnet_naming_rule.interface_should_be_begins_with_i.style = begins_with_i

dotnet_naming_rule.types_should_be_pascal_case.severity = suggestion
dotnet_naming_rule.types_should_be_pascal_case.symbols = types
dotnet_naming_rule.types_should_be_pascal_case.style = pascal_case

dotnet_naming_rule.non_field_members_should_be_pascal_case.severity = suggestion
dotnet_naming_rule.non_field_members_should_be_pascal_case.symbols = non_field_members
dotnet_naming_rule.non_field_members_should_be_pascal_case.style = pascal_case

# Symbol specifications
dotnet_naming_symbols.interface.applicable_kinds = interface
dotnet_naming_symbols.interface.applicable_accessibilities = public, internal, private, protected, protected_internal, private_protected

dotnet_naming_symbols.types.applicable_kinds = class, struct, interface, enum
dotnet_naming_symbols.types.applicable_accessibilities = public, internal, private, protected, protected_internal, private_protected

dotnet_naming_symbols.non_field_members.applicable_kinds = property, event, method
dotnet_naming_symbols.non_field_members.applicable_accessibilities = public, internal, private, protected, protected_internal, private_protected

# Naming styles
dotnet_naming_style.pascal_case.capitalization = pascal_case

dotnet_naming_style.begins_with_i.required_prefix = I
dotnet_naming_style.begins_with_i.capitalization = pascal_case
```

### 3. appsettings.json

```json
{
  "Logging": {
    "LogLevel": {
      "Default": "Information",
      "Microsoft.AspNetCore": "Warning"
    }
  },
  "AllowedHosts": "*",
  "ConnectionStrings": {
    "DefaultConnection": "Host=localhost;Database=mydb;Username=user;Password=pass"
  },
  "Redis": {
    "Configuration": "localhost:6379"
  },
  "Jwt": {
    "Key": "your-secret-key-here-minimum-32-characters",
    "Issuer": "MyProject",
    "Audience": "MyProject",
    "ExpireMinutes": 60
  }
}
```

**appsettings.Development.json**:

```json
{
  "Logging": {
    "LogLevel": {
      "Default": "Debug",
      "Microsoft.AspNetCore": "Information"
    }
  },
  "ConnectionStrings": {
    "DefaultConnection": "Host=localhost;Database=mydb_dev;Username=user;Password=pass"
  }
}
```

### 4. Environment Configuration

**Configuration class**:

```csharp
public class AppSettings
{
    public string ConnectionString { get; set; } = string.Empty;
    public RedisSettings Redis { get; set; } = new();
    public JwtSettings Jwt { get; set; } = new();
}

public class RedisSettings
{
    public string Configuration { get; set; } = "localhost:6379";
}

public class JwtSettings
{
    public string Key { get; set; } = string.Empty;
    public string Issuer { get; set; } = string.Empty;
    public string Audience { get; set; } = string.Empty;
    public int ExpireMinutes { get; set; } = 60;
}
```

**Load in Program.cs**:

```csharp
var builder = WebApplication.CreateBuilder(args);

// Bind configuration
builder.Services.Configure<AppSettings>(
    builder.Configuration.GetSection("AppSettings")
);

// Or inject IOptions<AppSettings>
builder.Services.AddSingleton(
    builder.Configuration.GetSection("AppSettings").Get<AppSettings>()!
);
```

---

## Source Code Standards

### 1. Directory Structure

```
src/
├── MyProject.Core/                 # Domain layer
│   ├── Entities/
│   ├── Interfaces/
│   ├── Exceptions/
│   └── ValueObjects/
├── MyProject.Application/          # Application layer
│   ├── Services/
│   ├── DTOs/
│   ├── Mappings/
│   └── Validators/
├── MyProject.Infrastructure/       # Infrastructure layer
│   ├── Data/
│   │   ├── Repositories/
│   │   └── Configurations/
│   ├── Services/
│   └── Migrations/
└── MyProject.Api/                  # Presentation layer
    ├── Controllers/
    ├── Middleware/
    ├── Filters/
    └── Program.cs

tests/
├── MyProject.UnitTests/
├── MyProject.IntegrationTests/
└── MyProject.Tests.Common/
    ├── Builders/
    ├── Fixtures/
    └── Helpers/
```

### 2. Naming Conventions

| Element | Convention | Example |
|---------|-----------|---------|
| **Namespaces** | PascalCase | `MyProject.Services` |
| **Classes** | PascalCase | `UserService` |
| **Interfaces** | IPascalCase | `IUserRepository` |
| **Methods** | PascalCase | `CreateUser()` |
| **Properties** | PascalCase | `UserId` |
| **Fields (private)** | _camelCase | `_userRepository` |
| **Constants** | PascalCase | `MaxRetryCount` |
| **Parameters** | camelCase | `userId` |
| **Local Variables** | camelCase | `userName` |

### 3. Entity (EF Core)

```csharp
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace MyProject.Core.Entities;

[Table("users")]
public class User
{
    [Key]
    [Column("id")]
    public Guid Id { get; set; }
    
    [Required]
    [MaxLength(255)]
    [Column("email")]
    public string Email { get; set; } = string.Empty;
    
    [Required]
    [MaxLength(100)]
    [Column("name")]
    public string Name { get; set; } = string.Empty;
    
    [Column("age")]
    public int? Age { get; set; }
    
    [Column("created_at")]
    public DateTime CreatedAt { get; set; }
    
    [Column("updated_at")]
    public DateTime UpdatedAt { get; set; }
}
```

### 4. Repository Interface

```csharp
namespace MyProject.Core.Interfaces;

public interface IUserRepository
{
    Task<User?> GetByIdAsync(Guid id, CancellationToken cancellationToken = default);
    Task<User?> GetByEmailAsync(string email, CancellationToken cancellationToken = default);
    Task<IEnumerable<User>> GetAllAsync(CancellationToken cancellationToken = default);
    Task<User> CreateAsync(User user, CancellationToken cancellationToken = default);
    Task<User> UpdateAsync(User user, CancellationToken cancellationToken = default);
    Task DeleteAsync(Guid id, CancellationToken cancellationToken = default);
    Task<bool> ExistsAsync(Guid id, CancellationToken cancellationToken = default);
}
```

### 5. Repository Implementation

```csharp
using Microsoft.EntityFrameworkCore;
using MyProject.Core.Entities;
using MyProject.Core.Interfaces;

namespace MyProject.Infrastructure.Data.Repositories;

public class UserRepository : IUserRepository
{
    private readonly ApplicationDbContext _context;
    
    public UserRepository(ApplicationDbContext context)
    {
        _context = context ?? throw new ArgumentNullException(nameof(context));
    }
    
    public async Task<User?> GetByIdAsync(Guid id, CancellationToken cancellationToken = default)
    {
        return await _context.Users
            .AsNoTracking()
            .FirstOrDefaultAsync(u => u.Id == id, cancellationToken);
    }
    
    public async Task<User?> GetByEmailAsync(string email, CancellationToken cancellationToken = default)
    {
        return await _context.Users
            .AsNoTracking()
            .FirstOrDefaultAsync(u => u.Email == email, cancellationToken);
    }
    
    public async Task<User> CreateAsync(User user, CancellationToken cancellationToken = default)
    {
        user.Id = Guid.NewGuid();
        user.CreatedAt = DateTime.UtcNow;
        user.UpdatedAt = DateTime.UtcNow;
        
        await _context.Users.AddAsync(user, cancellationToken);
        await _context.SaveChangesAsync(cancellationToken);
        
        return user;
    }
}
```

### 6. Service Layer

```csharp
using MyProject.Application.DTOs;
using MyProject.Core.Entities;
using MyProject.Core.Exceptions;
using MyProject.Core.Interfaces;
using AutoMapper;
using Microsoft.Extensions.Logging;

namespace MyProject.Application.Services;

/// <summary>
/// Service for managing users
/// </summary>
public class UserService
{
    private readonly IUserRepository _userRepository;
    private readonly IMapper _mapper;
    private readonly ILogger<UserService> _logger;
    
    public UserService(
        IUserRepository userRepository,
        IMapper mapper,
        ILogger<UserService> logger)
    {
        _userRepository = userRepository ?? throw new ArgumentNullException(nameof(userRepository));
        _mapper = mapper ?? throw new ArgumentNullException(nameof(mapper));
        _logger = logger ?? throw new ArgumentNullException(nameof(logger));
    }
    
    /// <summary>
    /// Creates a new user
    /// </summary>
    /// <param name="createDto">User creation data</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Created user</returns>
    /// <exception cref="ConflictException">Thrown when email already exists</exception>
    public async Task<UserDto> CreateUserAsync(
        CreateUserDto createDto,
        CancellationToken cancellationToken = default)
    {
        _logger.LogDebug("Creating user with email: {Email}", createDto.Email);
        
        // Check if email exists
        var existingUser = await _userRepository.GetByEmailAsync(createDto.Email, cancellationToken);
        if (existingUser is not null)
        {
            throw new ConflictException("Email already in use");
        }
        
        // Map and create
        var user = _mapper.Map<User>(createDto);
        user = await _userRepository.CreateAsync(user, cancellationToken);
        
        _logger.LogInformation("User created with id: {UserId}", user.Id);
        
        return _mapper.Map<UserDto>(user);
    }
}
```

### 7. Controller

```csharp
using Microsoft.AspNetCore.Mvc;
using MyProject.Application.DTOs;
using MyProject.Application.Services;

namespace MyProject.Api.Controllers;

/// <summary>
/// User management API
/// </summary>
[ApiController]
[Route("api/[controller]")]
[Produces("application/json")]
public class UsersController : ControllerBase
{
    private readonly UserService _userService;
    private readonly ILogger<UsersController> _logger;
    
    public UsersController(UserService userService, ILogger<UsersController> logger)
    {
        _userService = userService;
        _logger = logger;
    }
    
    /// <summary>
    /// Creates a new user
    /// </summary>
    /// <param name="createDto">User creation data</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Created user</returns>
    /// <response code="201">User created successfully</response>
    /// <response code="400">Invalid request data</response>
    /// <response code="409">Email already exists</response>
    [HttpPost]
    [ProducesResponseType(typeof(UserDto), StatusCodes.Status201Created)]
    [ProducesResponseType(StatusCodes.Status400BadRequest)]
    [ProducesResponseType(StatusCodes.Status409Conflict)]
    public async Task<ActionResult<UserDto>> CreateUser(
        [FromBody] CreateUserDto createDto,
        CancellationToken cancellationToken = default)
    {
        var user = await _userService.CreateUserAsync(createDto, cancellationToken);
        return CreatedAtAction(nameof(GetUser), new { id = user.Id }, user);
    }
    
    /// <summary>
    /// Gets a user by ID
    /// </summary>
    /// <param name="id">User ID</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>User if found</returns>
    /// <response code="200">User found</response>
    /// <response code="404">User not found</response>
    [HttpGet("{id:guid}")]
    [ProducesResponseType(typeof(UserDto), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<ActionResult<UserDto>> GetUser(
        Guid id,
        CancellationToken cancellationToken = default)
    {
        var user = await _userService.GetUserByIdAsync(id, cancellationToken);
        return Ok(user);
    }
}
```

### 8. Custom Exceptions

```csharp
namespace MyProject.Core.Exceptions;

public class AppException : Exception
{
    public string Code { get; }
    public int StatusCode { get; }
    
    public AppException(string message, string code, int statusCode = 500)
        : base(message)
    {
        Code = code;
        StatusCode = statusCode;
    }
}

public class NotFoundException : AppException
{
    public NotFoundException(string resource, Guid id)
        : base($"{resource} with id {id} not found", "NOT_FOUND", 404)
    {
    }
}

public class ConflictException : AppException
{
    public ConflictException(string message)
        : base(message, "CONFLICT", 409)
    {
    }
}

public class ValidationException : AppException
{
    public ValidationException(string message)
        : base(message, "VALIDATION_ERROR", 400)
    {
    }
}
```

### 9. Global Exception Handler

```csharp
using Microsoft.AspNetCore.Diagnostics;
using Microsoft.AspNetCore.Mvc;
using MyProject.Core.Exceptions;

namespace MyProject.Api.Middleware;

public class GlobalExceptionHandler : IExceptionHandler
{
    private readonly ILogger<GlobalExceptionHandler> _logger;
    
    public GlobalExceptionHandler(ILogger<GlobalExceptionHandler> logger)
    {
        _logger = logger;
    }
    
    public async ValueTask<bool> TryHandleAsync(
        HttpContext httpContext,
        Exception exception,
        CancellationToken cancellationToken)
    {
        _logger.LogError(exception, "Exception occurred: {Message}", exception.Message);
        
        var problemDetails = exception switch
        {
            NotFoundException notFound => new ProblemDetails
            {
                Status = notFound.StatusCode,
                Title = "Resource Not Found",
                Detail = notFound.Message,
                Type = "https://tools.ietf.org/html/rfc7231#section-6.5.4"
            },
            ConflictException conflict => new ProblemDetails
            {
                Status = conflict.StatusCode,
                Title = "Conflict",
                Detail = conflict.Message,
                Type = "https://tools.ietf.org/html/rfc7231#section-6.5.8"
            },
            ValidationException validation => new ProblemDetails
            {
                Status = validation.StatusCode,
                Title = "Validation Error",
                Detail = validation.Message,
                Type = "https://tools.ietf.org/html/rfc7231#section-6.5.1"
            },
            _ => new ProblemDetails
            {
                Status = StatusCodes.Status500InternalServerError,
                Title = "Internal Server Error",
                Detail = "An unexpected error occurred",
                Type = "https://tools.ietf.org/html/rfc7231#section-6.6.1"
            }
        };
        
        httpContext.Response.StatusCode = problemDetails.Status ?? 500;
        await httpContext.Response.WriteAsJsonAsync(problemDetails, cancellationToken);
        
        return true;
    }
}
```

---

## Testing Standards

### 1. Test Structure

```
tests/
├── MyProject.UnitTests/
│   ├── Services/
│   │   └── UserServiceTests.cs
│   ├── Validators/
│   │   └── UserValidatorTests.cs
│   └── Helpers/
│       └── TestDataBuilder.cs
├── MyProject.IntegrationTests/
│   ├── Api/
│   │   └── UsersControllerTests.cs
│   ├── Repositories/
│   │   └── UserRepositoryTests.cs
│   └── WebApplicationFactory.cs
└── MyProject.Tests.Common/
    ├── Fixtures/
    └── Utilities/
```

### 2. Unit Test Example (xUnit)

```csharp
using FluentAssertions;
using Moq;
using MyProject.Application.Services;
using MyProject.Core.Entities;
using MyProject.Core.Exceptions;
using MyProject.Core.Interfaces;
using Xunit;

namespace MyProject.UnitTests.Services;

public class UserServiceTests
{
    private readonly Mock<IUserRepository> _mockRepository;
    private readonly Mock<IMapper> _mockMapper;
    private readonly Mock<ILogger<UserService>> _mockLogger;
    private readonly UserService _userService;
    
    public UserServiceTests()
    {
        _mockRepository = new Mock<IUserRepository>();
        _mockMapper = new Mock<IMapper>();
        _mockLogger = new Mock<ILogger<UserService>>();
        _userService = new UserService(
            _mockRepository.Object,
            _mockMapper.Object,
            _mockLogger.Object
        );
    }
    
    [Fact]
    public async Task CreateUserAsync_WithValidData_ShouldCreateUser()
    {
        // Arrange
        var createDto = new CreateUserDto
        {
            Email = "test@example.com",
            Name = "Test User",
            Age = 25
        };
        
        var user = new User
        {
            Id = Guid.NewGuid(),
            Email = createDto.Email,
            Name = createDto.Name,
            Age = createDto.Age
        };
        
        _mockRepository
            .Setup(r => r.GetByEmailAsync(createDto.Email, It.IsAny<CancellationToken>()))
            .ReturnsAsync((User?)null);
        
        _mockMapper
            .Setup(m => m.Map<User>(createDto))
            .Returns(user);
        
        _mockRepository
            .Setup(r => r.CreateAsync(It.IsAny<User>(), It.IsAny<CancellationToken>()))
            .ReturnsAsync(user);
        
        _mockMapper
            .Setup(m => m.Map<UserDto>(user))
            .Returns(new UserDto { Id = user.Id, Email = user.Email, Name = user.Name });
        
        // Act
        var result = await _userService.CreateUserAsync(createDto);
        
        // Assert
        result.Should().NotBeNull();
        result.Email.Should().Be(createDto.Email);
        
        _mockRepository.Verify(
            r => r.CreateAsync(It.IsAny<User>(), It.IsAny<CancellationToken>()),
            Times.Once
        );
    }
    
    [Fact]
    public async Task CreateUserAsync_WithExistingEmail_ShouldThrowConflictException()
    {
        // Arrange
        var createDto = new CreateUserDto { Email = "existing@example.com" };
        var existingUser = new User { Email = createDto.Email };
        
        _mockRepository
            .Setup(r => r.GetByEmailAsync(createDto.Email, It.IsAny<CancellationToken>()))
            .ReturnsAsync(existingUser);
        
        // Act
        Func<Task> act = async () => await _userService.CreateUserAsync(createDto);
        
        // Assert
        await act.Should().ThrowAsync<ConflictException>()
            .WithMessage("Email already in use");
        
        _mockRepository.Verify(
            r => r.CreateAsync(It.IsAny<User>(), It.IsAny<CancellationToken>()),
            Times.Never
        );
    }
    
    [Theory]
    [InlineData("")]
    [InlineData("  ")]
    [InlineData("invalid-email")]
    public async Task CreateUserAsync_WithInvalidEmail_ShouldThrowValidationException(string email)
    {
        // Arrange
        var createDto = new CreateUserDto { Email = email };
        
        // Act & Assert
        await Assert.ThrowsAsync<ValidationException>(() =>
            _userService.CreateUserAsync(createDto)
        );
    }
}
```

### 3. Integration Test Example

```csharp
using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.DependencyInjection;
using System.Net;
using System.Net.Http.Json;
using Xunit;

namespace MyProject.IntegrationTests.Api;

public class UsersControllerTests : IClassFixture<WebApplicationFactory<Program>>
{
    private readonly HttpClient _client;
    private readonly WebApplicationFactory<Program> _factory;
    
    public UsersControllerTests(WebApplicationFactory<Program> factory)
    {
        _factory = factory.WithWebHostBuilder(builder =>
        {
            builder.ConfigureServices(services =>
            {
                // Replace database with in-memory
                var descriptor = services.SingleOrDefault(
                    d => d.ServiceType == typeof(DbContextOptions<ApplicationDbContext>));
                
                if (descriptor != null)
                {
                    services.Remove(descriptor);
                }
                
                services.AddDbContext<ApplicationDbContext>(options =>
                {
                    options.UseInMemoryDatabase("TestDb");
                });
            });
        });
        
        _client = _factory.CreateClient();
    }
    
    [Fact]
    public async Task CreateUser_WithValidData_ReturnsCreated()
    {
        // Arrange
        var createDto = new CreateUserDto
        {
            Email = "test@example.com",
            Name = "Test User",
            Age = 25
        };
        
        // Act
        var response = await _client.PostAsJsonAsync("/api/users", createDto);
        
        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.Created);
        
        var user = await response.Content.ReadFromJsonAsync<UserDto>();
        user.Should().NotBeNull();
        user!.Email.Should().Be(createDto.Email);
    }
}
```

### 4. Coverage Requirements

- **Overall**: > 90%
- **Unit Tests**: > 95%
- **Integration Tests**: > 85%

**Check coverage**:

```bash
# Run tests with coverage
dotnet test /p:CollectCoverage=true /p:CoverletOutputFormat=opencover

# Generate HTML report
reportgenerator -reports:coverage.opencover.xml -targetdir:coverage-report

# Open report
start coverage-report/index.html
```

---

## Build & Deployment

### 1. Build Process

```bash
# Restore packages
dotnet restore

# Build
dotnet build

# Build in Release mode
dotnet build -c Release

# Publish
dotnet publish -c Release -o ./publish
```

### 2. Docker Support

**Dockerfile**:

```dockerfile
# Build stage
FROM mcr.microsoft.com/dotnet/sdk:8.0 AS build
WORKDIR /src

# Copy csproj and restore
COPY ["MyProject.Api/MyProject.Api.csproj", "MyProject.Api/"]
COPY ["MyProject.Core/MyProject.Core.csproj", "MyProject.Core/"]
COPY ["MyProject.Infrastructure/MyProject.Infrastructure.csproj", "MyProject.Infrastructure/"]
RUN dotnet restore "MyProject.Api/MyProject.Api.csproj"

# Copy everything and build
COPY . .
WORKDIR "/src/MyProject.Api"
RUN dotnet build "MyProject.Api.csproj" -c Release -o /app/build

# Publish
FROM build AS publish
RUN dotnet publish "MyProject.Api.csproj" -c Release -o /app/publish

# Runtime stage
FROM mcr.microsoft.com/dotnet/aspnet:8.0 AS final
WORKDIR /app

# Create non-root user
RUN adduser --disabled-password --gecos '' appuser && chown -R appuser /app
USER appuser

COPY --from=publish /app/publish .

EXPOSE 8080

HEALTHCHECK --interval=30s --timeout=3s --retries=3 \
    CMD curl -f http://localhost:8080/health || exit 1

ENTRYPOINT ["dotnet", "MyProject.Api.dll"]
```

### 3. Publishing to NuGet

```bash
# Pack project
dotnet pack -c Release

# Push to NuGet
dotnet nuget push bin/Release/MyProject.1.0.0.nupkg \
    --api-key YOUR_API_KEY \
    --source https://api.nuget.org/v3/index.json
```

---

## Documentation

### 1. XML Documentation Comments

```csharp
/// <summary>
/// Processes a payment transaction
/// </summary>
/// <param name="userId">The user's unique identifier</param>
/// <param name="amount">Payment amount in cents (must be positive)</param>
/// <param name="currency">ISO 4217 currency code (e.g., "USD", "EUR")</param>
/// <param name="cancellationToken">Cancellation token</param>
/// <returns>Payment confirmation with transaction details</returns>
/// <exception cref="ValidationException">Thrown when amount is negative or currency is invalid</exception>
/// <exception cref="InsufficientFundsException">Thrown when user balance is insufficient</exception>
/// <exception cref="PaymentGatewayException">Thrown when payment gateway is unavailable</exception>
/// <example>
/// <code>
/// var result = await ProcessPaymentAsync("user-123", 1000, "USD");
/// Console.WriteLine($"Transaction ID: {result.TransactionId}");
/// </code>
/// </example>
/// <remarks>
/// This method is thread-safe and can be called concurrently.
/// </remarks>
public async Task<PaymentResult> ProcessPaymentAsync(
    string userId,
    int amount,
    string currency,
    CancellationToken cancellationToken = default)
{
    // Implementation
}
```

### 2. Generate Documentation

**Enable in .csproj**:

```xml
<PropertyGroup>
    <GenerateDocumentationFile>true</GenerateDocumentationFile>
</PropertyGroup>
```

**Use DocFX**:

```bash
# Install DocFX
dotnet tool install -g docfx

# Initialize
docfx init

# Build documentation
docfx build

# Serve locally
docfx serve _site
```

---

## C# Best Practices

See [BEST_PRACTICES.md](BEST_PRACTICES.md) for complete guide.

### Quick Tips

1. **Use nullable reference types** (enabled by default in .NET 6+)
2. **Use `async`/`await`** for I/O operations
3. **Use dependency injection** (built-in DI container)
4. **Use records** for DTOs and value objects
5. **Use pattern matching** (switch expressions, is patterns)
6. **Use LINQ** for collections
7. **Implement `IDisposable`** for unmanaged resources
8. **Use `ConfigureAwait(false)`** in libraries
9. **Follow naming conventions** (PascalCase, _camelCase)
10. **Use analyzers** (StyleCop, Roslyn)

---

## Common Patterns

Refer to [Source Code Standards](#source-code-standards) and [BEST_PRACTICES.md](BEST_PRACTICES.md) for:
- Repository Pattern
- Service Layer Pattern
- Dependency Injection
- Unit of Work Pattern
- Factory Pattern
- Builder Pattern (fluent interfaces)
- CQRS Pattern (MediatR)

---

## Troubleshooting

### Common C# Issues

#### Issue: Package restore fails

**Solution**:
```bash
dotnet nuget locals all --clear
dotnet restore
```

#### Issue: Build errors after updating packages

**Solution**:
```bash
dotnet clean
dotnet build --no-incremental
```

#### Issue: Entity Framework migrations

**Solution**:
```bash
# Add migration
dotnet ef migrations add InitialCreate

# Update database
dotnet ef database update

# Remove last migration
dotnet ef migrations remove
```

---

## Complete Workflow

Follow the [AI Integration Manual Template](../templates/AI_INTEGRATION_MANUAL_TEMPLATE.md) for the complete 6-phase workflow, using C#-specific configurations and commands from this manual.

### Quick Reference

1. **Planning**: Use templates from `gov/manuals/templates/`
2. **Setup**: Follow [C#-Specific Setup](#c-specific-setup)
3. **Implementation**: Follow [Source Code Standards](#source-code-standards)
4. **Testing**: Follow [Testing Standards](#testing-standards)
5. **Build**: Follow [Build & Deployment](#build--deployment)
6. **Documentation**: Follow [Documentation](#documentation)

---

## Additional Resources

- [C# Official Documentation](https://learn.microsoft.com/en-us/dotnet/csharp/)
- [ASP.NET Core Documentation](https://learn.microsoft.com/en-us/aspnet/core/)
- [Entity Framework Core](https://learn.microsoft.com/en-us/ef/core/)
- [.NET API Browser](https://learn.microsoft.com/en-us/dotnet/api/)
- [xUnit Documentation](https://xunit.net/)
- [BEST_PRACTICES.md](BEST_PRACTICES.md) - This project's best practices

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-10-11 | Initial C# manual |

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

