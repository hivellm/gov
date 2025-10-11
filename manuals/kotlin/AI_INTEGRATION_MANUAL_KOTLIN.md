# AI Integration Manual - Kotlin

**Version**: 1.0.0  
**Last Updated**: 2025-10-11  
**Language**: Kotlin 1.9+ / 2.0  
**Target Audience**: AI Agents (LLM-based development assistants)

---

## Table of Contents

1. [Introduction](#introduction)
2. [Quick Start](#quick-start)
3. [Kotlin-Specific Setup](#kotlin-specific-setup)
4. [Configuration Standards](#configuration-standards)
5. [Source Code Standards](#source-code-standards)
6. [Testing Standards](#testing-standards)
7. [Build & Deployment](#build--deployment)
8. [Documentation](#documentation)
9. [Kotlin Best Practices](#kotlin-best-practices)
10. [Common Patterns](#common-patterns)
11. [Troubleshooting](#troubleshooting)
12. [Complete Workflow](#complete-workflow)

---

## Introduction

This manual extends the [AI Integration Manual Template](../templates/AI_INTEGRATION_MANUAL_TEMPLATE.md) with Kotlin-specific implementations.

**When to use this manual**:
- Kotlin/JVM applications (backend APIs, microservices)
- Spring Boot applications
- Ktor applications
- Android backend services
- Kotlin Multiplatform projects
- Libraries/SDKs
- CLI tools

**Prerequisites**:
- Read [AI Integration Manual Template](../templates/AI_INTEGRATION_MANUAL_TEMPLATE.md)
- Read [Language Standards](../LANGUAGE_STANDARDS.md)
- Basic Kotlin knowledge

---

## Quick Start

### Minimum Viable Project with Gradle

```bash
# 1. Create project directory
mkdir my-project && cd my-project

# 2. Initialize Gradle project
gradle init \
  --type kotlin-application \
  --dsl kotlin \
  --test-framework junit-jupiter \
  --project-name my-project \
  --package com.hivellm.myproject

# 3. Build
./gradlew build

# 4. Run
./gradlew run

# 5. Test
./gradlew test
```

### Spring Boot Project

```bash
# Using Spring Initializr
curl https://start.spring.io/starter.tgz \
  -d dependencies=web,data-jpa,h2,validation,actuator \
  -d language=kotlin \
  -d type=gradle-project-kotlin \
  -d groupId=com.hivellm \
  -d artifactId=my-project \
  -d name=MyProject \
  -d javaVersion=17 \
  -d kotlinVersion=1.9.22 \
  | tar -xzf -

cd my-project
./gradlew bootRun
```

### Ktor Project

```bash
# Create Ktor project
mkdir my-ktor-app && cd my-ktor-app

# Create build.gradle.kts
cat > build.gradle.kts << 'EOF'
plugins {
    kotlin("jvm") version "1.9.22"
    id("io.ktor.plugin") version "2.3.7"
    kotlin("plugin.serialization") version "1.9.22"
}

repositories {
    mavenCentral()
}

dependencies {
    implementation("io.ktor:ktor-server-core")
    implementation("io.ktor:ktor-server-netty")
    implementation("io.ktor:ktor-server-content-negotiation")
    implementation("io.ktor:ktor-serialization-kotlinx-json")
    testImplementation("io.ktor:ktor-server-test-host")
    testImplementation("org.jetbrains.kotlin:kotlin-test")
}
EOF

./gradlew run
```

---

## Kotlin-Specific Setup

### 1. Environment Setup

#### Install Kotlin

**Kotlin comes bundled with Gradle/Maven/IntelliJ IDEA**

**Standalone Installation (Optional)**:

```bash
# Using SDKMAN
sdk install kotlin

# Using Homebrew (macOS)
brew install kotlin

# Verify
kotlin -version
kotlinc -version
```

#### Install Java (Required for Kotlin/JVM)

```bash
# Using SDKMAN
sdk install java 17.0.9-tem

# Verify
java -version
```

### 2. Build Tool (Gradle Recommended)

**Gradle with Kotlin DSL** is the recommended build tool:

```bash
# Install Gradle
sdk install gradle

# Or use Gradle wrapper (included in projects)
./gradlew --version
```

### 3. Essential Tools

```bash
# ktlint - Kotlin linter
brew install ktlint
# Or: curl -sSLO https://github.com/pinterest/ktlint/releases/download/1.1.0/ktlint && chmod +x ktlint

# detekt - Static code analysis
# (Add as Gradle plugin - see Configuration section)

# Dokka - Documentation generator
# (Add as Gradle plugin)
```

---

## Configuration Standards

### 1. build.gradle.kts (Gradle Kotlin DSL)

**Complete Configuration**:

```kotlin
import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
    kotlin("jvm") version "1.9.22"
    kotlin("plugin.spring") version "1.9.22"
    kotlin("plugin.jpa") version "1.9.22"
    id("org.springframework.boot") version "3.2.0"
    id("io.spring.dependency-management") version "1.1.4"
    id("org.jetbrains.dokka") version "1.9.10"
    id("io.gitlab.arturbosch.detekt") version "1.23.4"
    id("org.jlleitschuh.gradle.ktlint") version "12.0.3"
    jacoco
}

group = "com.hivellm"
version = "1.0.0"
java.sourceCompatibility = JavaVersion.VERSION_17

repositories {
    mavenCentral()
}

dependencies {
    // Kotlin
    implementation("org.jetbrains.kotlin:kotlin-reflect")
    implementation("org.jetbrains.kotlin:kotlin-stdlib-jdk8")
    
    // Spring Boot
    implementation("org.springframework.boot:spring-boot-starter-web")
    implementation("org.springframework.boot:spring-boot-starter-data-jpa")
    implementation("org.springframework.boot:spring-boot-starter-validation")
    implementation("org.springframework.boot:spring-boot-starter-actuator")
    
    // Kotlin Coroutines
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.7.3")
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-reactor:1.7.3")
    
    // Kotlin Serialization
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:1.6.2")
    
    // Database
    runtimeOnly("org.postgresql:postgresql")
    
    // Logging
    implementation("io.github.microutils:kotlin-logging-jvm:3.0.5")
    
    // Testing
    testImplementation("org.springframework.boot:spring-boot-starter-test")
    testImplementation("org.jetbrains.kotlin:kotlin-test-junit5")
    testImplementation("io.mockk:mockk:1.13.8")
    testImplementation("org.jetbrains.kotlinx:kotlinx-coroutines-test:1.7.3")
    testImplementation("io.kotest:kotest-runner-junit5:5.8.0")
    testImplementation("io.kotest:kotest-assertions-core:5.8.0")
}

tasks.withType<KotlinCompile> {
    kotlinOptions {
        freeCompilerArgs = listOf("-Xjsr305=strict")
        jvmTarget = "17"
    }
}

tasks.withType<Test> {
    useJUnitPlatform()
    finalizedBy(tasks.jacocoTestReport)
}

tasks.jacocoTestReport {
    dependsOn(tasks.test)
    reports {
        xml.required.set(true)
        html.required.set(true)
        csv.required.set(false)
    }
}

tasks.jacocoTestCoverageVerification {
    violationRules {
        rule {
            limit {
                minimum = "0.90".toBigDecimal()
            }
        }
    }
}

detekt {
    buildUponDefaultConfig = true
    allRules = false
    config.setFrom("$projectDir/config/detekt/detekt.yml")
}

ktlint {
    version.set("1.1.0")
    android.set(false)
    outputToConsole.set(true)
}

dokka {
    dokkaSourceSets {
        configureEach {
            includes.from("Module.md")
        }
    }
}
```

### 2. detekt.yml

**Static Analysis Configuration**:

```yaml
build:
  maxIssues: 0
  excludeCorrectable: false

config:
  validation: true
  warningsAsErrors: true

complexity:
  CyclomaticComplexMethod:
    active: true
    threshold: 10
  
  LongMethod:
    active: true
    threshold: 50
  
  LongParameterList:
    active: true
    threshold: 4

exceptions:
  TooGenericExceptionCaught:
    active: true
  
  SwallowedException:
    active: true

naming:
  FunctionNaming:
    active: true
    functionPattern: '[a-z][a-zA-Z0-9]*'
  
  ClassNaming:
    active: true
    classPattern: '[A-Z][a-zA-Z0-9]*'

style:
  MaxLineLength:
    active: true
    maxLineLength: 120
  
  MagicNumber:
    active: true
    ignoreNumbers: ['-1', '0', '1', '2']
```

### 3. .editorconfig

```ini
root = true

[*]
charset = utf-8
end_of_line = lf
insert_final_newline = true
trim_trailing_whitespace = true

[*.{kt,kts}]
indent_style = space
indent_size = 4
ij_kotlin_allow_trailing_comma = true
ij_kotlin_allow_trailing_comma_on_call_site = true

[*.{gradle,gradle.kts}]
indent_style = space
indent_size = 4

[*.{yml,yaml}]
indent_style = space
indent_size = 2
```

### 4. application.yml (Spring Boot)

```yaml
spring:
  application:
    name: my-project
  
  datasource:
    url: ${DATABASE_URL:jdbc:postgresql://localhost:5432/mydb}
    username: ${DATABASE_USER:user}
    password: ${DATABASE_PASSWORD:pass}
    driver-class-name: org.postgresql.Driver
    hikari:
      maximum-pool-size: ${DATABASE_POOL_SIZE:10}
  
  jpa:
    hibernate:
      ddl-auto: ${HIBERNATE_DDL:validate}
    show-sql: ${SHOW_SQL:false}
    properties:
      hibernate:
        format_sql: true
        dialect: org.hibernate.dialect.PostgreSQLDialect

server:
  port: ${PORT:8080}

logging:
  level:
    root: ${LOG_LEVEL:INFO}
    com.hivellm.myproject: DEBUG
```

---

## Source Code Standards

### 1. Directory Structure

**Spring Boot with Kotlin**:

```
src/
├── main/
│   ├── kotlin/
│   │   └── com/
│   │       └── hivellm/
│   │           └── myproject/
│   │               ├── MyProjectApplication.kt
│   │               ├── config/              # Configuration
│   │               ├── controller/          # REST controllers
│   │               ├── service/             # Business logic
│   │               ├── repository/          # Data access
│   │               ├── model/               # JPA entities
│   │               ├── dto/                 # DTOs
│   │               ├── mapper/              # Mappers
│   │               ├── exception/           # Custom exceptions
│   │               └── util/                # Utilities
│   └── resources/
│       ├── application.yml
│       └── db/
│           └── migration/
└── test/
    ├── kotlin/
    │   └── com/
    │       └── hivellm/
    │           └── myproject/
    │               ├── unit/
    │               ├── integration/
    │               └── util/
    └── resources/
        └── application-test.yml
```

### 2. Naming Conventions

| Element | Convention | Example |
|---------|-----------|---------|
| **Packages** | lowercase | `com.hivellm.myproject` |
| **Files** | PascalCase | `UserService.kt` |
| **Classes** | PascalCase | `UserService` |
| **Objects** | PascalCase | `DatabaseConfig` |
| **Interfaces** | PascalCase (no I prefix) | `UserRepository` |
| **Functions** | camelCase | `createUser()` |
| **Properties** | camelCase | `userId` |
| **Constants** | UPPER_SNAKE_CASE | `MAX_RETRY_COUNT` |
| **Type Parameters** | Single uppercase letter | `T`, `E`, `K`, `V` |

### 3. Data Class / Entity

```kotlin
package com.hivellm.myproject.model

import jakarta.persistence.*
import jakarta.validation.constraints.*
import java.time.LocalDateTime
import java.util.UUID

@Entity
@Table(
    name = "users",
    indexes = [Index(name = "idx_email", columnList = "email", unique = true)]
)
data class User(
    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "id", nullable = false)
    var id: UUID? = null,
    
    @field:NotBlank(message = "Email is required")
    @field:Email(message = "Invalid email format")
    @Column(name = "email", nullable = false, unique = true)
    var email: String,
    
    @field:NotBlank(message = "Name is required")
    @field:Size(min = 2, max = 100)
    @Column(name = "name", nullable = false)
    var name: String,
    
    @field:Min(value = 0, message = "Age must be positive")
    @Column(name = "age")
    var age: Int? = null,
    
    @Column(name = "created_at", nullable = false, updatable = false)
    var createdAt: LocalDateTime = LocalDateTime.now(),
    
    @Column(name = "updated_at", nullable = false)
    var updatedAt: LocalDateTime = LocalDateTime.now()
) {
    @PrePersist
    fun onCreate() {
        createdAt = LocalDateTime.now()
        updatedAt = LocalDateTime.now()
    }
    
    @PreUpdate
    fun onUpdate() {
        updatedAt = LocalDateTime.now()
    }
}
```

### 4. Repository Interface

```kotlin
package com.hivellm.myproject.repository

import com.hivellm.myproject.model.User
import org.springframework.data.jpa.repository.JpaRepository
import org.springframework.stereotype.Repository
import java.util.UUID

@Repository
interface UserRepository : JpaRepository<User, UUID> {
    
    fun findByEmail(email: String): User?
    
    fun existsByEmail(email: String): Boolean
    
    fun findByAgeGreaterThanEqual(minAge: Int): List<User>
}
```

### 5. Service Layer

```kotlin
package com.hivellm.myproject.service

import com.hivellm.myproject.dto.CreateUserDto
import com.hivellm.myproject.dto.UserDto
import com.hivellm.myproject.exception.ConflictException
import com.hivellm.myproject.exception.NotFoundException
import com.hivellm.myproject.model.User
import com.hivellm.myproject.repository.UserRepository
import mu.KotlinLogging
import org.springframework.stereotype.Service
import org.springframework.transaction.annotation.Transactional
import java.util.UUID

private val logger = KotlinLogging.logger {}

/**
 * Service for managing users
 */
@Service
class UserService(
    private val userRepository: UserRepository
) {
    
    /**
     * Creates a new user
     *
     * @param createDto User creation data
     * @return Created user
     * @throws ConflictException if email already exists
     */
    @Transactional
    fun createUser(createDto: CreateUserDto): UserDto {
        logger.debug { "Creating user with email: ${createDto.email}" }
        
        // Check if email exists
        if (userRepository.existsByEmail(createDto.email)) {
            throw ConflictException("Email already in use")
        }
        
        // Create user
        val user = User(
            email = createDto.email,
            name = createDto.name,
            age = createDto.age
        )
        
        val savedUser = userRepository.save(user)
        logger.info { "User created with id: ${savedUser.id}" }
        
        return savedUser.toDto()
    }
    
    /**
     * Gets a user by ID
     *
     * @param id User ID
     * @return User if found
     * @throws NotFoundException if user not found
     */
    @Transactional(readOnly = true)
    fun getUserById(id: UUID): UserDto {
        val user = userRepository.findById(id)
            .orElseThrow { NotFoundException("User", id.toString()) }
        
        return user.toDto()
    }
    
    /**
     * Gets all users
     */
    @Transactional(readOnly = true)
    fun getAllUsers(): List<UserDto> =
        userRepository.findAll().map { it.toDto() }
    
    private fun User.toDto() = UserDto(
        id = id!!,
        email = email,
        name = name,
        age = age,
        createdAt = createdAt
    )
}
```

### 6. Controller

```kotlin
package com.hivellm.myproject.controller

import com.hivellm.myproject.dto.CreateUserDto
import com.hivellm.myproject.dto.UserDto
import com.hivellm.myproject.service.UserService
import jakarta.validation.Valid
import org.springframework.http.HttpStatus
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation.*
import java.util.UUID

/**
 * REST API for user management
 */
@RestController
@RequestMapping("/api/users")
class UserController(
    private val userService: UserService
) {
    
    /**
     * Create a new user
     */
    @PostMapping
    fun createUser(@Valid @RequestBody createDto: CreateUserDto): ResponseEntity<UserDto> {
        val user = userService.createUser(createDto)
        return ResponseEntity.status(HttpStatus.CREATED).body(user)
    }
    
    /**
     * Get user by ID
     */
    @GetMapping("/{id}")
    fun getUser(@PathVariable id: UUID): ResponseEntity<UserDto> {
        val user = userService.getUserById(id)
        return ResponseEntity.ok(user)
    }
    
    /**
     * Get all users
     */
    @GetMapping
    fun getAllUsers(): ResponseEntity<List<UserDto>> {
        val users = userService.getAllUsers()
        return ResponseEntity.ok(users)
    }
}
```

### 7. DTOs

```kotlin
package com.hivellm.myproject.dto

import jakarta.validation.constraints.Email
import jakarta.validation.constraints.Min
import jakarta.validation.constraints.NotBlank
import jakarta.validation.constraints.Size
import java.time.LocalDateTime
import java.util.UUID

/**
 * DTO for creating users
 */
data class CreateUserDto(
    @field:NotBlank(message = "Email is required")
    @field:Email(message = "Invalid email format")
    val email: String,
    
    @field:NotBlank(message = "Name is required")
    @field:Size(min = 2, max = 100, message = "Name must be between 2 and 100 characters")
    val name: String,
    
    @field:Min(value = 0, message = "Age must be positive")
    val age: Int? = null
)

/**
 * DTO for user responses
 */
data class UserDto(
    val id: UUID,
    val email: String,
    val name: String,
    val age: Int?,
    val createdAt: LocalDateTime
)
```

### 8. Custom Exceptions

```kotlin
package com.hivellm.myproject.exception

/**
 * Base application exception
 */
open class AppException(
    message: String,
    val code: String,
    val statusCode: Int = 500,
    cause: Throwable? = null
) : RuntimeException(message, cause)

/**
 * Resource not found exception
 */
class NotFoundException(
    resource: String,
    id: String
) : AppException(
    message = "$resource with id $id not found",
    code = "NOT_FOUND",
    statusCode = 404
)

/**
 * Conflict exception
 */
class ConflictException(
    message: String
) : AppException(
    message = message,
    code = "CONFLICT",
    statusCode = 409
)

/**
 * Validation exception
 */
class ValidationException(
    message: String
) : AppException(
    message = message,
    code = "VALIDATION_ERROR",
    statusCode = 400
)
```

### 9. Global Exception Handler

```kotlin
package com.hivellm.myproject.exception

import mu.KotlinLogging
import org.springframework.http.HttpStatus
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.MethodArgumentNotValidException
import org.springframework.web.bind.annotation.ExceptionHandler
import org.springframework.web.bind.annotation.RestControllerAdvice
import java.time.LocalDateTime

private val logger = KotlinLogging.logger {}

/**
 * Global exception handler
 */
@RestControllerAdvice
class GlobalExceptionHandler {
    
    @ExceptionHandler(NotFoundException::class)
    fun handleNotFound(ex: NotFoundException): ResponseEntity<ErrorResponse> {
        logger.warn { "Resource not found: ${ex.message}" }
        return ResponseEntity
            .status(HttpStatus.NOT_FOUND)
            .body(ErrorResponse(ex.code, ex.message ?: "Not found"))
    }
    
    @ExceptionHandler(ConflictException::class)
    fun handleConflict(ex: ConflictException): ResponseEntity<ErrorResponse> {
        logger.warn { "Conflict: ${ex.message}" }
        return ResponseEntity
            .status(HttpStatus.CONFLICT)
            .body(ErrorResponse(ex.code, ex.message ?: "Conflict"))
    }
    
    @ExceptionHandler(MethodArgumentNotValidException::class)
    fun handleValidation(ex: MethodArgumentNotValidException): ResponseEntity<ErrorResponse> {
        val errors = ex.bindingResult.fieldErrors.associate {
            it.field to (it.defaultMessage ?: "Invalid value")
        }
        
        return ResponseEntity
            .status(HttpStatus.BAD_REQUEST)
            .body(ErrorResponse("VALIDATION_ERROR", "Validation failed", errors))
    }
    
    @ExceptionHandler(Exception::class)
    fun handleGeneral(ex: Exception): ResponseEntity<ErrorResponse> {
        logger.error(ex) { "Unexpected error" }
        return ResponseEntity
            .status(HttpStatus.INTERNAL_SERVER_ERROR)
            .body(ErrorResponse("INTERNAL_ERROR", "An unexpected error occurred"))
    }
    
    data class ErrorResponse(
        val code: String,
        val message: String,
        val details: Map<String, String>? = null,
        val timestamp: LocalDateTime = LocalDateTime.now()
    )
}
```

---

## Testing Standards

### 1. Test Structure

```
src/test/kotlin/
└── com/
    └── hivellm/
        └── myproject/
            ├── unit/
            │   ├── service/
            │   │   └── UserServiceTest.kt
            │   └── util/
            │       └── ValidatorTest.kt
            ├── integration/
            │   ├── controller/
            │   │   └── UserControllerTest.kt
            │   └── repository/
            │       └── UserRepositoryTest.kt
            └── util/
                └── TestDataFactory.kt
```

### 2. Unit Test Example (MockK + Kotest)

```kotlin
package com.hivellm.myproject.unit.service

import com.hivellm.myproject.dto.CreateUserDto
import com.hivellm.myproject.exception.ConflictException
import com.hivellm.myproject.model.User
import com.hivellm.myproject.repository.UserRepository
import com.hivellm.myproject.service.UserService
import io.kotest.assertions.throwables.shouldThrow
import io.kotest.core.spec.style.DescribeSpec
import io.kotest.matchers.shouldBe
import io.kotest.matchers.shouldNotBe
import io.mockk.*
import java.util.Optional
import java.util.UUID

class UserServiceTest : DescribeSpec({
    
    lateinit var userRepository: UserRepository
    lateinit var userService: UserService
    
    beforeEach {
        userRepository = mockk()
        userService = UserService(userRepository)
    }
    
    afterEach {
        clearAllMocks()
    }
    
    describe("createUser") {
        
        it("should create user with valid data") {
            // Arrange
            val createDto = CreateUserDto(
                email = "test@example.com",
                name = "Test User",
                age = 25
            )
            
            val user = User(
                email = createDto.email,
                name = createDto.name,
                age = createDto.age
            ).apply {
                id = UUID.randomUUID()
            }
            
            every { userRepository.existsByEmail(createDto.email) } returns false
            every { userRepository.save(any<User>()) } returns user
            
            // Act
            val result = userService.createUser(createDto)
            
            // Assert
            result shouldNotBe null
            result.email shouldBe createDto.email
            result.name shouldBe createDto.name
            
            verify(exactly = 1) { userRepository.existsByEmail(createDto.email) }
            verify(exactly = 1) { userRepository.save(any<User>()) }
        }
        
        it("should throw ConflictException when email exists") {
            // Arrange
            val createDto = CreateUserDto(
                email = "existing@example.com",
                name = "Test"
            )
            
            every { userRepository.existsByEmail(createDto.email) } returns true
            
            // Act & Assert
            shouldThrow<ConflictException> {
                userService.createUser(createDto)
            }.message shouldBe "Email already in use"
            
            verify(exactly = 1) { userRepository.existsByEmail(createDto.email) }
            verify(exactly = 0) { userRepository.save(any<User>()) }
        }
    }
})
```

### 3. Integration Test Example

```kotlin
package com.hivellm.myproject.integration.controller

import com.hivellm.myproject.dto.CreateUserDto
import com.hivellm.myproject.repository.UserRepository
import com.fasterxml.jackson.databind.ObjectMapper
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc
import org.springframework.boot.test.context.SpringBootTest
import org.springframework.http.MediaType
import org.springframework.test.web.servlet.MockMvc
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*
import org.springframework.test.web.servlet.result.MockMvcResultMatchers.*

@SpringBootTest
@AutoConfigureMockMvc
class UserControllerTest {
    
    @Autowired
    private lateinit var mockMvc: MockMvc
    
    @Autowired
    private lateinit var objectMapper: ObjectMapper
    
    @Autowired
    private lateinit var userRepository: UserRepository
    
    @BeforeEach
    fun setUp() {
        userRepository.deleteAll()
    }
    
    @Test
    fun `POST users should create user`() {
        // Arrange
        val createDto = CreateUserDto(
            email = "test@example.com",
            name = "Test User",
            age = 25
        )
        
        // Act & Assert
        mockMvc.perform(
            post("/api/users")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(createDto))
        )
            .andExpect(status().isCreated)
            .andExpect(jsonPath("$.email").value(createDto.email))
            .andExpect(jsonPath("$.name").value(createDto.name))
    }
    
    @Test
    fun `POST users should return 400 for invalid email`() {
        // Arrange
        val createDto = CreateUserDto(
            email = "invalid-email",
            name = "Test User"
        )
        
        // Act & Assert
        mockMvc.perform(
            post("/api/users")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(createDto))
        )
            .andExpect(status().isBadRequest)
            .andExpect(jsonPath("$.code").value("VALIDATION_ERROR"))
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
./gradlew test jacocoTestReport

# Open report
open build/reports/jacoco/test/html/index.html
```

---

## Build & Deployment

### 1. Build Process

```bash
# Build
./gradlew build

# Build without tests
./gradlew build -x test

# Create JAR
./gradlew bootJar  # Spring Boot
./gradlew jar      # Plain JAR

# Clean build
./gradlew clean build
```

### 2. Docker Support

**Dockerfile (Multi-stage)**:

```dockerfile
# Build stage
FROM gradle:8.5-jdk17 AS build

WORKDIR /app

# Copy Gradle files
COPY build.gradle.kts settings.gradle.kts gradlew ./
COPY gradle ./gradle

# Download dependencies (cached layer)
RUN ./gradlew dependencies --no-daemon

# Copy source
COPY src ./src

# Build application
RUN ./gradlew bootJar --no-daemon

# Production stage
FROM eclipse-temurin:17-jre-alpine

WORKDIR /app

# Create non-root user
RUN addgroup -g 1000 appuser && adduser -D -u 1000 -G appuser appuser

# Copy JAR from build stage
COPY --from=build /app/build/libs/*.jar app.jar

# Change ownership
RUN chown -R appuser:appuser /app
USER appuser

# Expose port
EXPOSE 8080

# Health check
HEALTHCHECK --interval=30s --timeout=3s --retries=3 \
    CMD wget --quiet --tries=1 --spider http://localhost:8080/actuator/health || exit 1

# Run application
ENTRYPOINT ["java", "-jar", "app.jar"]
```

### 3. Publishing to Maven Central

```kotlin
// In build.gradle.kts
publishing {
    publications {
        create<MavenPublication>("maven") {
            from(components["java"])
            
            pom {
                name.set("My Project")
                description.set("Project description")
                url.set("https://github.com/user/repo")
                
                licenses {
                    license {
                        name.set("MIT License")
                        url.set("https://opensource.org/licenses/MIT")
                    }
                }
                
                developers {
                    developer {
                        id.set("userid")
                        name.set("Developer Name")
                        email.set("dev@example.com")
                    }
                }
                
                scm {
                    connection.set("scm:git:git://github.com/user/repo.git")
                    url.set("https://github.com/user/repo")
                }
            }
        }
    }
}
```

---

## Documentation

### 1. KDoc Comments

```kotlin
/**
 * Processes a payment transaction.
 *
 * This method handles the complete payment workflow including
 * validation, authorization, and confirmation.
 *
 * @param userId The user's unique identifier
 * @param amount Payment amount in cents (must be positive)
 * @param currency ISO 4217 currency code (e.g., "USD", "EUR")
 * @return Payment confirmation with transaction details
 * @throws ValidationException if amount is negative or currency is invalid
 * @throws InsufficientFundsException if user balance is insufficient
 * @throws PaymentGatewayException if payment gateway is unavailable
 *
 * @sample sampleProcessPayment
 * @see PaymentResult
 * @since 1.0.0
 */
suspend fun processPayment(
    userId: String,
    amount: Int,
    currency: String
): PaymentResult {
    // Implementation
}

private fun sampleProcessPayment() {
    val result = processPayment("user-123", 1000, "USD")
    println("Transaction ID: ${result.transactionId}")
}
```

### 2. Generate Documentation with Dokka

```bash
# Generate documentation
./gradlew dokkaHtml

# Output: build/dokka/html/index.html

# Generate Javadoc JAR
./gradlew dokkaJavadocJar
```

---

## Kotlin Best Practices

See [BEST_PRACTICES.md](BEST_PRACTICES.md) for complete guide.

### Quick Tips

1. **Use data classes** for models and DTOs
2. **Use null safety** (`?`, `?.`, `?:`, `!!`)
3. **Use extension functions** to extend existing classes
4. **Use scope functions** (`let`, `run`, `with`, `apply`, `also`)
5. **Use sealed classes** for restricted hierarchies
6. **Use coroutines** for async operations
7. **Prefer immutability** (`val` over `var`)
8. **Use when** instead of switch
9. **Use destructuring** for data classes
10. **Follow Kotlin coding conventions**

---

## Common Patterns

Refer to [Source Code Standards](#source-code-standards) and [BEST_PRACTICES.md](BEST_PRACTICES.md) for:
- Repository Pattern
- Service Layer Pattern
- Dependency Injection (Spring)
- Factory Pattern
- Builder Pattern (DSL style)
- Sealed Classes for State
- Delegation Pattern

---

## Troubleshooting

### Common Kotlin Issues

#### Issue: Unresolved reference

**Solution**:
```bash
./gradlew clean build --refresh-dependencies
```

#### Issue: Kotlin version mismatch

**Solution**: Ensure all Kotlin plugins use same version in build.gradle.kts

#### Issue: Coroutine context issues

**Solution**: Use proper coroutine scope and dispatchers

---

## Complete Workflow

Follow the [AI Integration Manual Template](../templates/AI_INTEGRATION_MANUAL_TEMPLATE.md) for the complete 6-phase workflow, using Kotlin-specific configurations and commands from this manual.

### Quick Reference

1. **Planning**: Use templates from `gov/manuals/templates/`
2. **Setup**: Follow [Kotlin-Specific Setup](#kotlin-specific-setup)
3. **Implementation**: Follow [Source Code Standards](#source-code-standards)
4. **Testing**: Follow [Testing Standards](#testing-standards)
5. **Build**: Follow [Build & Deployment](#build--deployment)
6. **Documentation**: Follow [Documentation](#documentation)

---

## Additional Resources

- [Kotlin Official Documentation](https://kotlinlang.org/docs/home.html)
- [Kotlin Style Guide](https://kotlinlang.org/docs/coding-conventions.html)
- [Spring Boot with Kotlin](https://spring.io/guides/tutorials/spring-boot-kotlin/)
- [Ktor Framework](https://ktor.io/)
- [Kotlin Coroutines](https://kotlinlang.org/docs/coroutines-overview.html)
- [BEST_PRACTICES.md](BEST_PRACTICES.md) - This project's best practices

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-10-11 | Initial Kotlin manual |

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

