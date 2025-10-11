# AI Integration Manual - Java

**Version**: 1.0.0  
**Last Updated**: 2025-10-11  
**Language**: Java 17+ / 21 LTS  
**Target Audience**: AI Agents (LLM-based development assistants)

---

## Table of Contents

1. [Introduction](#introduction)
2. [Quick Start](#quick-start)
3. [Java-Specific Setup](#java-specific-setup)
4. [Configuration Standards](#configuration-standards)
5. [Source Code Standards](#source-code-standards)
6. [Testing Standards](#testing-standards)
7. [Build & Deployment](#build--deployment)
8. [Documentation](#documentation)
9. [Java Best Practices](#java-best-practices)
10. [Common Patterns](#common-patterns)
11. [Troubleshooting](#troubleshooting)
12. [Complete Workflow](#complete-workflow)

---

## Introduction

This manual extends the [AI Integration Manual Template](../templates/AI_INTEGRATION_MANUAL_TEMPLATE.md) with Java-specific implementations.

**When to use this manual**:
- Java applications (enterprise, microservices)
- Spring Boot applications
- REST APIs
- Android development (backend)
- Libraries/SDKs
- CLI tools

**Prerequisites**:
- Read [AI Integration Manual Template](../templates/AI_INTEGRATION_MANUAL_TEMPLATE.md)
- Read [Language Standards](../LANGUAGE_STANDARDS.md)
- Basic Java knowledge

---

## Quick Start

### Minimum Viable Project Setup with Maven

```bash
# 1. Create project using Maven archetype
mvn archetype:generate \
  -DgroupId=com.hivellm.myproject \
  -DartifactId=my-project \
  -DarchetypeArtifactId=maven-archetype-quickstart \
  -DarchetypeVersion=1.4 \
  -DinteractiveMode=false

# 2. Navigate to project
cd my-project

# 3. Update pom.xml for Java 17+
# (see Configuration Standards section)

# 4. Create source structure
mkdir -p src/main/java/com/hivellm/myproject/{core,api,service,repository,model,util,config}
mkdir -p src/test/java/com/hivellm/myproject/{unit,integration}
mkdir -p src/main/resources
mkdir -p src/test/resources

# 5. Build project
mvn clean install

# 6. Run tests
mvn test
```

### Quick Start with Gradle

```bash
# 1. Initialize Gradle project
gradle init \
  --type java-application \
  --dsl groovy \
  --test-framework junit-jupiter \
  --project-name my-project \
  --package com.hivellm.myproject

# 2. Navigate to project
cd my-project

# 3. Build
./gradlew build

# 4. Run tests
./gradlew test
```

### Quick Start with Spring Boot

**Recommended for web applications**:

```bash
# Using Spring Initializr CLI
curl https://start.spring.io/starter.tgz \
  -d dependencies=web,data-jpa,h2,validation,actuator \
  -d groupId=com.hivellm \
  -d artifactId=my-project \
  -d name=MyProject \
  -d description="My Spring Boot Project" \
  -d packageName=com.hivellm.myproject \
  -d javaVersion=17 \
  -d bootVersion=3.2.0 \
  | tar -xzf -

cd my-project
./mvnw clean install
```

Or visit https://start.spring.io for interactive setup.

---

## Java-Specific Setup

### 1. Environment Setup

#### Install Java Version Manager

**Using SDKMAN (Recommended)**:

```bash
# Install SDKMAN (Unix/Mac/Windows WSL)
curl -s "https://get.sdkman.io" | bash
source "$HOME/.sdkman/bin/sdkman-init.sh"

# List available Java versions
sdk list java

# Install Java 17 (LTS)
sdk install java 17.0.9-tem

# Install Java 21 (Latest LTS)
sdk install java 21.0.1-tem

# Set default
sdk default java 21.0.1-tem

# Verify installation
java -version
javac -version
```

**Alternative: Manual Installation**:
- **Oracle JDK**: https://www.oracle.com/java/technologies/downloads/
- **OpenJDK**: https://openjdk.org/
- **Amazon Corretto**: https://aws.amazon.com/corretto/
- **Eclipse Temurin**: https://adoptium.net/

#### Create .sdkmanrc file

```bash
echo "java=21.0.1-tem" > .sdkmanrc
```

### 2. Build Tool Selection

**Recommended**: Choose **ONE** build tool for consistency.

| Tool | Speed | Features | Ecosystem | Notes |
|------|-------|----------|-----------|-------|
| **Maven** | Good | Standard | Vast | Industry standard, XML-based |
| **Gradle** | Excellent | Advanced | Growing | Groovy/Kotlin DSL, flexible |

**This manual covers both Maven and Gradle**.

### 3. Essential Dependencies (Maven)

```xml
<dependencies>
    <!-- Spring Boot Starter (for web apps) -->
    <dependency>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-web</artifactId>
        <version>3.2.0</version>
    </dependency>
    
    <!-- Database -->
    <dependency>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-data-jpa</artifactId>
        <version>3.2.0</version>
    </dependency>
    
    <!-- Validation -->
    <dependency>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-validation</artifactId>
        <version>3.2.0</version>
    </dependency>
    
    <!-- Lombok (reduce boilerplate) -->
    <dependency>
        <groupId>org.projectlombok</groupId>
        <artifactId>lombok</artifactId>
        <version>1.18.30</version>
        <scope>provided</scope>
    </dependency>
    
    <!-- Testing -->
    <dependency>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-test</artifactId>
        <version>3.2.0</version>
        <scope>test</scope>
    </dependency>
    
    <dependency>
        <groupId>org.junit.jupiter</groupId>
        <artifactId>junit-jupiter</artifactId>
        <version>5.10.1</version>
        <scope>test</scope>
    </dependency>
    
    <dependency>
        <groupId>org.mockito</groupId>
        <artifactId>mockito-core</artifactId>
        <version>5.8.0</version>
        <scope>test</scope>
    </dependency>
</dependencies>
```

---

## Configuration Standards

### 1. pom.xml (Maven)

**Complete Configuration**:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 
         http://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>

    <groupId>com.hivellm</groupId>
    <artifactId>my-project</artifactId>
    <version>1.0.0</version>
    <packaging>jar</packaging>

    <name>My Project</name>
    <description>Project description</description>
    <url>https://github.com/user/repo</url>

    <licenses>
        <license>
            <name>MIT License</name>
            <url>https://opensource.org/licenses/MIT</url>
        </license>
    </licenses>

    <developers>
        <developer>
            <name>Developer Name</name>
            <email>dev@example.com</email>
        </developer>
    </developers>

    <scm>
        <url>https://github.com/user/repo</url>
        <connection>scm:git:git://github.com/user/repo.git</connection>
        <developerConnection>scm:git:ssh://git@github.com/user/repo.git</developerConnection>
    </scm>

    <properties>
        <java.version>17</java.version>
        <maven.compiler.source>17</maven.compiler.source>
        <maven.compiler.target>17</maven.compiler.target>
        <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
        
        <!-- Dependency Versions -->
        <spring-boot.version>3.2.0</spring-boot.version>
        <lombok.version>1.18.30</lombok.version>
        <junit.version>5.10.1</junit.version>
        <mockito.version>5.8.0</mockito.version>
    </properties>

    <dependencies>
        <!-- Dependencies here -->
    </dependencies>

    <build>
        <plugins>
            <!-- Compiler Plugin -->
            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-compiler-plugin</artifactId>
                <version>3.12.1</version>
                <configuration>
                    <source>${java.version}</source>
                    <target>${java.version}</target>
                    <annotationProcessorPaths>
                        <path>
                            <groupId>org.projectlombok</groupId>
                            <artifactId>lombok</artifactId>
                            <version>${lombok.version}</version>
                        </path>
                    </annotationProcessorPaths>
                </configuration>
            </plugin>

            <!-- Surefire (Unit Tests) -->
            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-surefire-plugin</artifactId>
                <version>3.2.3</version>
            </plugin>

            <!-- Failsafe (Integration Tests) -->
            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-failsafe-plugin</artifactId>
                <version>3.2.3</version>
                <executions>
                    <execution>
                        <goals>
                            <goal>integration-test</goal>
                            <goal>verify</goal>
                        </goals>
                    </execution>
                </executions>
            </plugin>

            <!-- JaCoCo (Code Coverage) -->
            <plugin>
                <groupId>org.jacoco</groupId>
                <artifactId>jacoco-maven-plugin</artifactId>
                <version>0.8.11</version>
                <executions>
                    <execution>
                        <goals>
                            <goal>prepare-agent</goal>
                        </goals>
                    </execution>
                    <execution>
                        <id>report</id>
                        <phase>test</phase>
                        <goals>
                            <goal>report</goal>
                        </goals>
                    </execution>
                    <execution>
                        <id>jacoco-check</id>
                        <goals>
                            <goal>check</goal>
                        </goals>
                        <configuration>
                            <rules>
                                <rule>
                                    <element>PACKAGE</element>
                                    <limits>
                                        <limit>
                                            <counter>LINE</counter>
                                            <value>COVEREDRATIO</value>
                                            <minimum>0.90</minimum>
                                        </limit>
                                    </limits>
                                </rule>
                            </rules>
                        </configuration>
                    </execution>
                </executions>
            </plugin>

            <!-- Checkstyle (Linting) -->
            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-checkstyle-plugin</artifactId>
                <version>3.3.1</version>
                <configuration>
                    <configLocation>checkstyle.xml</configLocation>
                    <consoleOutput>true</consoleOutput>
                    <failsOnError>true</failsOnError>
                </configuration>
                <executions>
                    <execution>
                        <phase>validate</phase>
                        <goals>
                            <goal>check</goal>
                        </goals>
                    </execution>
                </executions>
            </plugin>

            <!-- SpotBugs (Static Analysis) -->
            <plugin>
                <groupId>com.github.spotbugs</groupId>
                <artifactId>spotbugs-maven-plugin</artifactId>
                <version>4.8.2.0</version>
                <configuration>
                    <effort>Max</effort>
                    <threshold>Low</threshold>
                    <failOnError>true</failOnError>
                </configuration>
            </plugin>

            <!-- Javadoc -->
            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-javadoc-plugin</artifactId>
                <version>3.6.3</version>
                <configuration>
                    <show>public</show>
                </configuration>
            </plugin>
        </plugins>
    </build>
</project>
```

### 2. build.gradle (Gradle Alternative)

**Complete Configuration**:

```groovy
plugins {
    id 'java'
    id 'application'
    id 'jacoco'
    id 'checkstyle'
    id 'com.github.spotbugs' version '6.0.4'
    id 'org.springframework.boot' version '3.2.0'
    id 'io.spring.dependency-management' version '1.1.4'
}

group = 'com.hivellm'
version = '1.0.0'
sourceCompatibility = '17'

repositories {
    mavenCentral()
}

dependencies {
    // Spring Boot
    implementation 'org.springframework.boot:spring-boot-starter-web'
    implementation 'org.springframework.boot:spring-boot-starter-data-jpa'
    implementation 'org.springframework.boot:spring-boot-starter-validation'
    
    // Lombok
    compileOnly 'org.projectlombok:lombok:1.18.30'
    annotationProcessor 'org.projectlombok:lombok:1.18.30'
    
    // Testing
    testImplementation 'org.springframework.boot:spring-boot-starter-test'
    testImplementation 'org.junit.jupiter:junit-jupiter:5.10.1'
    testImplementation 'org.mockito:mockito-core:5.8.0'
}

application {
    mainClass = 'com.hivellm.myproject.Application'
}

test {
    useJUnitPlatform()
    finalizedBy jacocoTestReport
}

jacoco {
    toolVersion = '0.8.11'
}

jacocoTestReport {
    dependsOn test
    reports {
        xml.required = true
        html.required = true
        csv.required = false
    }
}

jacocoTestCoverageVerification {
    violationRules {
        rule {
            limit {
                minimum = 0.90
            }
        }
    }
}

checkstyle {
    toolVersion = '10.12.7'
    configFile = file("${rootDir}/checkstyle.xml")
}

spotbugs {
    effort = 'max'
    reportLevel = 'low'
}
```

### 3. Checkstyle Configuration

**checkstyle.xml** (Google Java Style):

```xml
<?xml version="1.0"?>
<!DOCTYPE module PUBLIC
    "-//Checkstyle//DTD Checkstyle Configuration 1.3//EN"
    "https://checkstyle.org/dtds/configuration_1_3.dtd">

<module name="Checker">
    <property name="charset" value="UTF-8"/>
    <property name="severity" value="error"/>
    <property name="fileExtensions" value="java"/>

    <module name="TreeWalker">
        <!-- Naming Conventions -->
        <module name="PackageName">
            <property name="format" value="^[a-z]+(\.[a-z][a-z0-9]*)*$"/>
        </module>
        <module name="TypeName"/>
        <module name="MethodName"/>
        <module name="ConstantName"/>
        <module name="LocalVariableName"/>
        <module name="MemberName"/>
        
        <!-- Imports -->
        <module name="AvoidStarImport"/>
        <module name="UnusedImports"/>
        <module name="RedundantImport"/>
        
        <!-- Whitespace -->
        <module name="WhitespaceAround"/>
        <module name="WhitespaceAfter"/>
        
        <!-- Size Violations -->
        <module name="MethodLength">
            <property name="max" value="50"/>
        </module>
        <module name="ParameterNumber">
            <property name="max" value="4"/>
        </module>
        
        <!-- Complexity -->
        <module name="CyclomaticComplexity">
            <property name="max" value="10"/>
        </module>
        
        <!-- Best Practices -->
        <module name="EqualsHashCode"/>
        <module name="SimplifyBooleanExpression"/>
        <module name="SimplifyBooleanReturn"/>
        <module name="StringLiteralEquality"/>
    </module>
    
    <!-- Line Length -->
    <module name="LineLength">
        <property name="max" value="120"/>
    </module>
</module>
```

### 4. Application Configuration

**application.yml** (Spring Boot):

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
      minimum-idle: 2
  
  jpa:
    hibernate:
      ddl-auto: ${HIBERNATE_DDL:validate}
    show-sql: ${SHOW_SQL:false}
    properties:
      hibernate:
        format_sql: true
        dialect: org.hibernate.dialect.PostgreSQLDialect
  
  cache:
    type: redis
    redis:
      time-to-live: 600000
  
  data:
    redis:
      host: ${REDIS_HOST:localhost}
      port: ${REDIS_PORT:6379}

server:
  port: ${PORT:8080}
  error:
    include-message: always
    include-binding-errors: always

logging:
  level:
    root: ${LOG_LEVEL:INFO}
    com.hivellm.myproject: DEBUG
  pattern:
    console: "%d{yyyy-MM-dd HH:mm:ss} - %msg%n"

management:
  endpoints:
    web:
      exposure:
        include: health,info,metrics
  endpoint:
    health:
      show-details: when-authorized
```

**application-test.yml** (for tests):

```yaml
spring:
  datasource:
    url: jdbc:h2:mem:testdb
    driver-class-name: org.h2.Driver
  jpa:
    hibernate:
      ddl-auto: create-drop
```

---

## Source Code Standards

### 1. Directory Structure

```
src/
├── main/
│   ├── java/
│   │   └── com/
│   │       └── hivellm/
│   │           └── myproject/
│   │               ├── Application.java          # Spring Boot main
│   │               ├── config/                   # Configuration classes
│   │               ├── core/                     # Core business logic
│   │               │   ├── domain/              # Domain entities
│   │               │   ├── usecase/             # Use cases
│   │               │   └── port/                # Interfaces/ports
│   │               ├── infrastructure/           # External concerns
│   │               │   ├── persistence/         # Database implementation
│   │               │   └── messaging/           # Message queue
│   │               ├── api/                      # API layer
│   │               │   ├── controller/          # REST controllers
│   │               │   ├── dto/                 # DTOs
│   │               │   └── mapper/              # DTO mappers
│   │               ├── service/                  # Application services
│   │               ├── repository/               # Data repositories
│   │               ├── model/                    # JPA entities
│   │               ├── exception/                # Custom exceptions
│   │               └── util/                     # Utilities
│   └── resources/
│       ├── application.yml
│       ├── application-dev.yml
│       ├── application-prod.yml
│       └── db/
│           └── migration/                        # Database migrations
└── test/
    ├── java/
    │   └── com/
    │       └── hivellm/
    │           └── myproject/
    │               ├── unit/                     # Unit tests
    │               ├── integration/              # Integration tests
    │               └── util/                     # Test utilities
    └── resources/
        └── application-test.yml
```

### 2. Naming Conventions

| Element | Convention | Example |
|---------|-----------|---------|
| **Packages** | lowercase | `com.hivellm.myproject` |
| **Classes** | PascalCase | `UserService` |
| **Interfaces** | PascalCase (no I prefix) | `UserRepository` |
| **Methods** | camelCase | `createUser()` |
| **Variables** | camelCase | `userId` |
| **Constants** | UPPER_SNAKE_CASE | `MAX_RETRY_COUNT` |
| **Enum** | PascalCase | `UserRole` |
| **Enum Constants** | UPPER_SNAKE_CASE | `UserRole.ADMIN` |

### 3. Code Organization Patterns

#### Entity (JPA)

```java
package com.hivellm.myproject.model;

import jakarta.persistence.*;
import jakarta.validation.constraints.*;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;
import java.time.LocalDateTime;

@Entity
@Table(name = "users", indexes = {
    @Index(name = "idx_email", columnList = "email", unique = true)
})
@Data
@NoArgsConstructor
@AllArgsConstructor
public class User {
    
    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private String id;
    
    @NotBlank(message = "Email is required")
    @Email(message = "Invalid email format")
    @Column(nullable = false, unique = true)
    private String email;
    
    @NotBlank(message = "Name is required")
    @Size(min = 2, max = 100)
    @Column(nullable = false)
    private String name;
    
    @Min(value = 0, message = "Age must be positive")
    private Integer age;
    
    @Column(name = "created_at", nullable = false, updatable = false)
    private LocalDateTime createdAt;
    
    @Column(name = "updated_at", nullable = false)
    private LocalDateTime updatedAt;
    
    @PrePersist
    protected void onCreate() {
        createdAt = LocalDateTime.now();
        updatedAt = LocalDateTime.now();
    }
    
    @PreUpdate
    protected void onUpdate() {
        updatedAt = LocalDateTime.now();
    }
}
```

#### Repository Interface

```java
package com.hivellm.myproject.repository;

import com.hivellm.myproject.model.User;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;
import java.util.Optional;

@Repository
public interface UserRepository extends JpaRepository<User, String> {
    
    Optional<User> findByEmail(String email);
    
    boolean existsByEmail(String email);
    
    @Query("SELECT u FROM User u WHERE u.age >= :minAge")
    List<User> findAdults(@Param("minAge") int minAge);
}
```

#### Service Layer

```java
package com.hivellm.myproject.service;

import com.hivellm.myproject.api.dto.CreateUserDto;
import com.hivellm.myproject.api.dto.UserDto;
import com.hivellm.myproject.exception.ConflictException;
import com.hivellm.myproject.exception.NotFoundException;
import com.hivellm.myproject.model.User;
import com.hivellm.myproject.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * Service for managing users
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class UserService {
    
    private final UserRepository userRepository;
    
    /**
     * Creates a new user
     *
     * @param createDto User creation data
     * @return Created user
     * @throws ConflictException if email already exists
     */
    @Transactional
    public UserDto createUser(CreateUserDto createDto) {
        log.debug("Creating user with email: {}", createDto.getEmail());
        
        // Check if email exists
        if (userRepository.existsByEmail(createDto.getEmail())) {
            throw new ConflictException("Email already in use");
        }
        
        // Create and save user
        User user = new User();
        user.setEmail(createDto.getEmail());
        user.setName(createDto.getName());
        user.setAge(createDto.getAge());
        
        user = userRepository.save(user);
        log.info("User created with id: {}", user.getId());
        
        return mapToDto(user);
    }
    
    /**
     * Gets a user by ID
     *
     * @param id User ID
     * @return User if found
     * @throws NotFoundException if user not found
     */
    @Transactional(readOnly = true)
    public UserDto getUserById(String id) {
        User user = userRepository.findById(id)
            .orElseThrow(() -> new NotFoundException("User", id));
        return mapToDto(user);
    }
    
    private UserDto mapToDto(User user) {
        return UserDto.builder()
            .id(user.getId())
            .email(user.getEmail())
            .name(user.getName())
            .age(user.getAge())
            .createdAt(user.getCreatedAt())
            .build();
    }
}
```

#### REST Controller

```java
package com.hivellm.myproject.api.controller;

import com.hivellm.myproject.api.dto.CreateUserDto;
import com.hivellm.myproject.api.dto.UserDto;
import com.hivellm.myproject.service.UserService;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

/**
 * REST API for user management
 */
@RestController
@RequestMapping("/api/users")
@RequiredArgsConstructor
public class UserController {
    
    private final UserService userService;
    
    /**
     * Create a new user
     *
     * @param createDto User creation data
     * @return Created user with HTTP 201
     */
    @PostMapping
    public ResponseEntity<UserDto> createUser(@Valid @RequestBody CreateUserDto createDto) {
        UserDto user = userService.createUser(createDto);
        return ResponseEntity.status(HttpStatus.CREATED).body(user);
    }
    
    /**
     * Get user by ID
     *
     * @param id User ID
     * @return User with HTTP 200
     */
    @GetMapping("/{id}")
    public ResponseEntity<UserDto> getUser(@PathVariable String id) {
        UserDto user = userService.getUserById(id);
        return ResponseEntity.ok(user);
    }
    
    /**
     * Get all users
     *
     * @return List of users with HTTP 200
     */
    @GetMapping
    public ResponseEntity<List<UserDto>> getAllUsers() {
        List<UserDto> users = userService.getAllUsers();
        return ResponseEntity.ok(users);
    }
}
```

### 4. DTO with Lombok

```java
package com.hivellm.myproject.api.dto;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Min;
import lombok.Data;
import lombok.Builder;

@Data
@Builder
public class CreateUserDto {
    
    @NotBlank(message = "Email is required")
    @Email(message = "Invalid email format")
    private String email;
    
    @NotBlank(message = "Name is required")
    private String name;
    
    @Min(value = 0, message = "Age must be positive")
    private Integer age;
}
```

### 5. Custom Exceptions

```java
package com.hivellm.myproject.exception;

public class AppException extends RuntimeException {
    private final String code;
    private final int statusCode;
    
    public AppException(String message, String code, int statusCode) {
        super(message);
        this.code = code;
        this.statusCode = statusCode;
    }
    
    public String getCode() {
        return code;
    }
    
    public int getStatusCode() {
        return statusCode;
    }
}

public class NotFoundException extends AppException {
    public NotFoundException(String resource, String id) {
        super(
            String.format("%s with id %s not found", resource, id),
            "NOT_FOUND",
            404
        );
    }
}

public class ConflictException extends AppException {
    public ConflictException(String message) {
        super(message, "CONFLICT", 409);
    }
}
```

### 6. Global Exception Handler

```java
package com.hivellm.myproject.api.exception;

import com.hivellm.myproject.exception.AppException;
import com.hivellm.myproject.exception.NotFoundException;
import com.hivellm.myproject.exception.ConflictException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;

@RestControllerAdvice
@Slf4j
public class GlobalExceptionHandler {
    
    @ExceptionHandler(NotFoundException.class)
    public ResponseEntity<ErrorResponse> handleNotFound(NotFoundException ex) {
        log.warn("Resource not found: {}", ex.getMessage());
        return ResponseEntity
            .status(HttpStatus.NOT_FOUND)
            .body(new ErrorResponse(ex.getCode(), ex.getMessage()));
    }
    
    @ExceptionHandler(ConflictException.class)
    public ResponseEntity<ErrorResponse> handleConflict(ConflictException ex) {
        log.warn("Conflict: {}", ex.getMessage());
        return ResponseEntity
            .status(HttpStatus.CONFLICT)
            .body(new ErrorResponse(ex.getCode(), ex.getMessage()));
    }
    
    @ExceptionHandler(MethodArgumentNotValidException.class)
    public ResponseEntity<ErrorResponse> handleValidation(
        MethodArgumentNotValidException ex
    ) {
        Map<String, String> errors = new HashMap<>();
        ex.getBindingResult().getFieldErrors().forEach(error ->
            errors.put(error.getField(), error.getDefaultMessage())
        );
        
        return ResponseEntity
            .status(HttpStatus.BAD_REQUEST)
            .body(new ErrorResponse("VALIDATION_ERROR", "Validation failed", errors));
    }
    
    @ExceptionHandler(Exception.class)
    public ResponseEntity<ErrorResponse> handleGeneral(Exception ex) {
        log.error("Unexpected error", ex);
        return ResponseEntity
            .status(HttpStatus.INTERNAL_SERVER_ERROR)
            .body(new ErrorResponse("INTERNAL_ERROR", "An unexpected error occurred"));
    }
    
    @lombok.Data
    @lombok.AllArgsConstructor
    @lombok.NoArgsConstructor
    public static class ErrorResponse {
        private String code;
        private String message;
        private Map<String, String> details;
        private LocalDateTime timestamp = LocalDateTime.now();
        
        public ErrorResponse(String code, String message) {
            this.code = code;
            this.message = message;
        }
    }
}
```

---

## Testing Standards

### 1. Test Structure

```
src/test/
├── java/
│   └── com/
│       └── hivellm/
│           └── myproject/
│               ├── unit/                      # Unit tests
│               │   ├── service/
│               │   │   └── UserServiceTest.java
│               │   └── util/
│               │       └── ValidatorTest.java
│               ├── integration/               # Integration tests
│               │   ├── api/
│               │   │   └── UserControllerIT.java
│               │   └── repository/
│               │       └── UserRepositoryIT.java
│               └── util/                      # Test utilities
│                   ├── TestDataFactory.java
│                   └── TestConfig.java
└── resources/
    ├── application-test.yml
    └── test-data/
```

### 2. Unit Test Example (JUnit 5)

```java
package com.hivellm.myproject.unit.service;

import com.hivellm.myproject.api.dto.CreateUserDto;
import com.hivellm.myproject.exception.ConflictException;
import com.hivellm.myproject.model.User;
import com.hivellm.myproject.repository.UserRepository;
import com.hivellm.myproject.service.UserService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@DisplayName("UserService Tests")
class UserServiceTest {
    
    @Mock
    private UserRepository userRepository;
    
    @InjectMocks
    private UserService userService;
    
    private CreateUserDto createUserDto;
    private User user;
    
    @BeforeEach
    void setUp() {
        createUserDto = CreateUserDto.builder()
            .email("test@example.com")
            .name("Test User")
            .age(25)
            .build();
        
        user = new User();
        user.setId("123");
        user.setEmail(createUserDto.getEmail());
        user.setName(createUserDto.getName());
        user.setAge(createUserDto.getAge());
    }
    
    @Test
    @DisplayName("Should create user successfully")
    void testCreateUser_Success() {
        // Arrange
        when(userRepository.existsByEmail(createUserDto.getEmail())).thenReturn(false);
        when(userRepository.save(any(User.class))).thenReturn(user);
        
        // Act
        UserDto result = userService.createUser(createUserDto);
        
        // Assert
        assertNotNull(result);
        assertEquals(user.getEmail(), result.getEmail());
        assertEquals(user.getName(), result.getName());
        
        verify(userRepository).existsByEmail(createUserDto.getEmail());
        verify(userRepository).save(any(User.class));
    }
    
    @Test
    @DisplayName("Should throw ConflictException when email exists")
    void testCreateUser_EmailExists() {
        // Arrange
        when(userRepository.existsByEmail(createUserDto.getEmail())).thenReturn(true);
        
        // Act & Assert
        assertThrows(ConflictException.class, () -> {
            userService.createUser(createUserDto);
        });
        
        verify(userRepository).existsByEmail(createUserDto.getEmail());
        verify(userRepository, never()).save(any(User.class));
    }
}
```

### 3. Integration Test Example

```java
package com.hivellm.myproject.integration.api;

import com.hivellm.myproject.api.dto.CreateUserDto;
import com.hivellm.myproject.model.User;
import com.hivellm.myproject.repository.UserRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import com.fasterxml.jackson.databind.ObjectMapper;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;
import static org.hamcrest.Matchers.*;

@SpringBootTest
@AutoConfigureMockMvc
@DisplayName("User API Integration Tests")
class UserControllerIT {
    
    @Autowired
    private MockMvc mockMvc;
    
    @Autowired
    private ObjectMapper objectMapper;
    
    @Autowired
    private UserRepository userRepository;
    
    @BeforeEach
    void setUp() {
        userRepository.deleteAll();
    }
    
    @Test
    @DisplayName("POST /api/users should create user")
    void testCreateUser() throws Exception {
        // Arrange
        CreateUserDto createDto = CreateUserDto.builder()
            .email("test@example.com")
            .name("Test User")
            .age(25)
            .build();
        
        // Act & Assert
        mockMvc.perform(post("/api/users")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(createDto)))
            .andExpect(status().isCreated())
            .andExpect(jsonPath("$.id").exists())
            .andExpect(jsonPath("$.email").value(createDto.getEmail()))
            .andExpect(jsonPath("$.name").value(createDto.getName()));
    }
    
    @Test
    @DisplayName("POST /api/users should return 400 for invalid email")
    void testCreateUser_InvalidEmail() throws Exception {
        // Arrange
        CreateUserDto createDto = CreateUserDto.builder()
            .email("invalid-email")
            .name("Test User")
            .age(25)
            .build();
        
        // Act & Assert
        mockMvc.perform(post("/api/users")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(createDto)))
            .andExpect(status().isBadRequest())
            .andExpect(jsonPath("$.code").value("VALIDATION_ERROR"));
    }
}
```

### 4. Coverage Requirements

- **Overall**: > 90%
- **Unit Tests**: > 95%
- **Integration Tests**: > 85%

**Check coverage**:

```bash
# Maven
mvn clean test jacoco:report
open target/site/jacoco/index.html

# Gradle
./gradlew test jacocoTestReport
open build/reports/jacoco/test/html/index.html
```

---

## Build & Deployment

### 1. Build Process

**Maven**:
```bash
# Clean build
mvn clean install

# Skip tests
mvn clean install -DskipTests

# Build without tests
mvn clean package -DskipTests

# Output: target/my-project-1.0.0.jar
```

**Gradle**:
```bash
# Clean build
./gradlew clean build

# Skip tests
./gradlew build -x test

# Output: build/libs/my-project-1.0.0.jar
```

### 2. Docker Support

**Dockerfile (Multi-stage)**:

```dockerfile
# Build stage
FROM maven:3.9-eclipse-temurin-17 AS build
WORKDIR /app
COPY pom.xml .
COPY src ./src
RUN mvn clean package -DskipTests

# Production stage
FROM eclipse-temurin:17-jre-alpine
WORKDIR /app

# Create non-root user
RUN addgroup -g 1000 appuser && adduser -D -u 1000 -G appuser appuser

# Copy jar from build stage
COPY --from=build /app/target/*.jar app.jar

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

### 3. Maven Deployment to Central

```bash
# Configure settings.xml with credentials

# Deploy snapshot
mvn clean deploy

# Release (requires release plugin)
mvn release:prepare
mvn release:perform
```

---

## Documentation

### 1. Javadoc

**Standard Format**:

```java
/**
 * Processes a payment transaction.
 *
 * <p>This method handles the complete payment workflow including
 * validation, authorization, and confirmation.</p>
 *
 * @param userId the user's unique identifier
 * @param amount payment amount in cents (must be positive)
 * @param currency ISO 4217 currency code (e.g., "USD", "EUR")
 * @return payment confirmation with transaction details
 * @throws ValidationException if amount is negative or currency is invalid
 * @throws InsufficientFundsException if user balance is insufficient
 * @throws PaymentGatewayException if payment gateway is unavailable
 * @see PaymentResult
 * @since 1.0.0
 */
public PaymentResult processPayment(
    String userId,
    int amount,
    String currency
) throws ValidationException, InsufficientFundsException {
    // Implementation
}
```

### 2. Generate Javadoc

**Maven**:
```bash
mvn javadoc:javadoc
# Output: target/site/apidocs/
```

**Gradle**:
```bash
./gradlew javadoc
# Output: build/docs/javadoc/
```

### 3. Javadoc Plugin Configuration

**pom.xml**:
```xml
<plugin>
    <groupId>org.apache.maven.plugins</groupId>
    <artifactId>maven-javadoc-plugin</artifactId>
    <version>3.6.3</version>
    <configuration>
        <show>public</show>
        <doctitle>My Project API Documentation</doctitle>
        <windowtitle>My Project API</windowtitle>
        <header>My Project ${project.version}</header>
        <bottom>Copyright © 2025. All rights reserved.</bottom>
        <links>
            <link>https://docs.oracle.com/en/java/javase/17/docs/api/</link>
        </links>
    </configuration>
</plugin>
```

---

## Java Best Practices

See [BEST_PRACTICES.md](BEST_PRACTICES.md) for complete guide.

### Quick Tips

1. **Use Java 17+ features** (records, sealed classes, pattern matching)
2. **Follow Google Java Style Guide**
3. **Use Optional** for nullable returns
4. **Prefer immutability** (final fields, unmodifiable collections)
5. **Use try-with-resources** for auto-closeable
6. **Use Streams API** for collections
7. **Avoid null** - use Optional
8. **Use Lombok** to reduce boilerplate
9. **Implement equals() and hashCode()** properly
10. **Use dependency injection** (Spring @Autowired, constructor injection)

---

## Common Patterns

Refer to [Source Code Standards](#source-code-standards) and [BEST_PRACTICES.md](BEST_PRACTICES.md) for:
- Repository Pattern
- Service Layer Pattern
- Dependency Injection (Spring)
- Builder Pattern
- Factory Pattern
- Singleton Pattern (Spring @Bean)
- DTO Pattern

---

## Troubleshooting

### Common Java Issues

#### Issue: ClassNotFoundException / NoClassDefFoundError

**Solution**:
```bash
# Maven: Download dependencies
mvn dependency:resolve

# Gradle: Refresh dependencies
./gradlew --refresh-dependencies
```

#### Issue: OutOfMemoryError

**Solution**:
```bash
# Increase heap size
export MAVEN_OPTS="-Xmx2048m"
mvn clean install

# Or in pom.xml
<configuration>
    <argLine>-Xmx2048m</argLine>
</configuration>
```

#### Issue: Tests fail with Spring context errors

**Solution**:
```java
// Use @SpringBootTest for integration tests
@SpringBootTest
@AutoConfigureMockMvc
class MyIntegrationTest {
    // Tests
}

// Use @WebMvcTest for controller tests
@WebMvcTest(UserController.class)
class UserControllerTest {
    // Tests
}
```

---

## Complete Workflow

Follow the [AI Integration Manual Template](../templates/AI_INTEGRATION_MANUAL_TEMPLATE.md) for the complete 6-phase workflow, using Java-specific configurations and commands from this manual.

### Quick Reference

1. **Planning**: Use templates from `gov/manuals/templates/`
2. **Setup**: Follow [Java-Specific Setup](#java-specific-setup)
3. **Implementation**: Follow [Source Code Standards](#source-code-standards)
4. **Testing**: Follow [Testing Standards](#testing-standards)
5. **Build**: Follow [Build & Deployment](#build--deployment)
6. **Documentation**: Follow [Documentation](#documentation)

---

## Additional Resources

- [Java Official Documentation](https://docs.oracle.com/en/java/)
- [Spring Boot Documentation](https://spring.io/projects/spring-boot)
- [Google Java Style Guide](https://google.github.io/styleguide/javaguide.html)
- [Effective Java (Book)](https://www.oreilly.com/library/view/effective-java/9780134686097/)
- [JUnit 5 Documentation](https://junit.org/junit5/docs/current/user-guide/)
- [BEST_PRACTICES.md](BEST_PRACTICES.md) - This project's best practices

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-10-11 | Initial Java manual |

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

