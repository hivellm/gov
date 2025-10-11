# Java Best Practices

**Version**: 1.0.0  
**Last Updated**: 2025-10-11  
**Target Audience**: AI Agents developing Java projects

---

## Table of Contents

1. [Java Idioms](#java-idioms)
2. [Anti-Patterns](#anti-patterns)
3. [Performance Optimization](#performance-optimization)
4. [Security Best Practices](#security-best-practices)
5. [Error Handling](#error-handling)
6. [Concurrency Patterns](#concurrency-patterns)
7. [Modern Java Features](#modern-java-features)
8. [Code Organization](#code-organization)
9. [Testing Best Practices](#testing-best-practices)
10. [Common Gotchas](#common-gotchas)

---

## Java Idioms

### 1. Use Optional for Nullable Returns

```java
// ❌ BAD: Can return null
public User findUser(String id) {
    return database.get(id);  // Might be null
}

// ✅ GOOD: Explicit about possibility of absence
public Optional<User> findUser(String id) {
    return Optional.ofNullable(database.get(id));
}

// Usage
Optional<User> user = findUser("123");
user.ifPresent(u -> System.out.println(u.getName()));
String name = user.map(User::getName).orElse("Unknown");
```

### 2. Use try-with-resources

```java
// ❌ BAD: Manual resource management
BufferedReader reader = null;
try {
    reader = new BufferedReader(new FileReader("file.txt"));
    return reader.readLine();
} finally {
    if (reader != null) {
        reader.close();
    }
}

// ✅ GOOD: Automatic resource management
try (BufferedReader reader = new BufferedReader(new FileReader("file.txt"))) {
    return reader.readLine();
}
```

### 3. Use Streams for Collections

```java
// ❌ BAD: Imperative style
List<String> result = new ArrayList<>();
for (User user : users) {
    if (user.getAge() >= 18) {
        result.add(user.getName());
    }
}

// ✅ GOOD: Functional style with Streams
List<String> result = users.stream()
    .filter(user -> user.getAge() >= 18)
    .map(User::getName)
    .collect(Collectors.toList());
```

### 4. Use Records (Java 14+)

```java
// ❌ BAD: Verbose data class
public class Point {
    private final double x;
    private final double y;
    
    public Point(double x, double y) {
        this.x = x;
        this.y = y;
    }
    
    public double getX() { return x; }
    public double getY() { return y; }
    
    @Override
    public boolean equals(Object o) { /* ... */ }
    
    @Override
    public int hashCode() { /* ... */ }
}

// ✅ GOOD: Concise record
public record Point(double x, double y) {}
```

### 5. Use Enums with Methods

```java
// ✅ GOOD: Enums with behavior
public enum UserRole {
    ADMIN {
        @Override
        public boolean canDelete() { return true; }
    },
    USER {
        @Override
        public boolean canDelete() { return false; }
    };
    
    public abstract boolean canDelete();
}
```

### 6. Use var for Local Variables (Java 10+)

```java
// ❌ BAD: Redundant type declaration
Map<String, List<User>> usersByRole = new HashMap<String, List<User>>();

// ✅ GOOD: Type inference
var usersByRole = new HashMap<String, List<User>>();

// ⚠️ CAUTION: Use only when type is obvious
var users = repository.findAll();  // OK - clear from method name
var data = process(input);  // BAD - unclear what type 'data' is
```

### 7. Use Sealed Classes (Java 17+)

```java
// ✅ GOOD: Restricted hierarchy
public sealed interface Shape permits Circle, Rectangle, Triangle {
    double area();
}

public final class Circle implements Shape {
    private final double radius;
    
    public Circle(double radius) {
        this.radius = radius;
    }
    
    @Override
    public double area() {
        return Math.PI * radius * radius;
    }
}
```

### 8. Use Pattern Matching (Java 17+)

```java
// ❌ BAD: Traditional instanceof with cast
if (obj instanceof String) {
    String str = (String) obj;
    System.out.println(str.length());
}

// ✅ GOOD: Pattern matching
if (obj instanceof String str) {
    System.out.println(str.length());
}
```

### 9. Use Text Blocks (Java 15+)

```java
// ❌ BAD: String concatenation
String json = "{\n" +
              "  \"name\": \"John\",\n" +
              "  \"age\": 30\n" +
              "}";

// ✅ GOOD: Text blocks
String json = """
    {
      "name": "John",
      "age": 30
    }
    """;
```

### 10. Use Factory Methods

```java
// ✅ GOOD: Named constructors
public class User {
    private String id;
    private String name;
    
    private User(String id, String name) {
        this.id = id;
        this.name = name;
    }
    
    public static User createNew(String name) {
        return new User(UUID.randomUUID().toString(), name);
    }
    
    public static User fromDatabase(String id, String name) {
        return new User(id, name);
    }
}
```

---

## Anti-Patterns

### 1. Not Using equals() and hashCode()

```java
// ❌ BAD: Doesn't override equals/hashCode
public class User {
    private String id;
    private String name;
    // Missing equals() and hashCode()
}

// ✅ GOOD: Properly implemented
@Override
public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    User user = (User) o;
    return Objects.equals(id, user.id);
}

@Override
public int hashCode() {
    return Objects.hash(id);
}

// ✅ BETTER: Use Lombok
@Data
public class User {
    private String id;
    private String name;
}

// ✅ BEST: Use Record
public record User(String id, String name) {}
```

### 2. Catching Generic Exception

```java
// ❌ BAD: Too broad
try {
    riskyOperation();
} catch (Exception e) {
    // Catches everything
}

// ✅ GOOD: Specific exceptions
try {
    riskyOperation();
} catch (IOException e) {
    log.error("IO error", e);
} catch (SQLException e) {
    log.error("Database error", e);
    throw new DataAccessException(e);
}
```

### 3. Using == for Object Comparison

```java
// ❌ BAD: Compares references
String str1 = new String("hello");
String str2 = new String("hello");
if (str1 == str2) {  // false
    // Won't execute
}

// ✅ GOOD: Use equals()
if (str1.equals(str2)) {  // true
    // Executes
}

// ✅ BETTER: Use Objects.equals() (null-safe)
if (Objects.equals(str1, str2)) {  // true
    // Executes
}
```

### 4. Not Closing Resources

```java
// ❌ BAD: Resource leak
FileInputStream fis = new FileInputStream("file.txt");
// Forgot to close

// ✅ GOOD: try-with-resources
try (FileInputStream fis = new FileInputStream("file.txt")) {
    // Use resource
}  // Automatically closed
```

### 5. Returning Null from Collections

```java
// ❌ BAD: Caller must check for null
public List<User> getUsers() {
    if (noUsers) {
        return null;
    }
    return users;
}

// ✅ GOOD: Return empty collection
public List<User> getUsers() {
    if (noUsers) {
        return Collections.emptyList();
    }
    return users;
}
```

### 6. Ignoring Exceptions

```java
// ❌ BAD: Silent failure
try {
    riskyOperation();
} catch (Exception e) {
    // Ignored
}

// ✅ GOOD: Log or rethrow
try {
    riskyOperation();
} catch (IOException e) {
    log.error("Operation failed", e);
    throw new RuntimeException("Failed to process", e);
}
```

### 7. String Concatenation in Loops

```java
// ❌ BAD: Creates many String objects
String result = "";
for (String item : items) {
    result += item;
}

// ✅ GOOD: Use StringBuilder
StringBuilder result = new StringBuilder();
for (String item : items) {
    result.append(item);
}
return result.toString();

// ✅ BETTER: Use String.join()
String result = String.join("", items);
```

---

## Performance Optimization

### 1. Use ArrayList vs LinkedList Appropriately

```java
// ✅ GOOD: ArrayList for random access
List<String> list = new ArrayList<>();  // O(1) random access

// ✅ GOOD: LinkedList for frequent insertions/deletions at beginning
List<String> list = new LinkedList<>();  // O(1) add/remove at head
```

### 2. Use HashMap/HashSet for Lookups

```java
// ❌ SLOW: O(n) lookup
List<String> items = Arrays.asList("a", "b", "c");
if (items.contains("c")) {  // O(n)
    // ...
}

// ✅ FAST: O(1) lookup
Set<String> items = Set.of("a", "b", "c");
if (items.contains("c")) {  // O(1)
    // ...
}
```

### 3. Use Lazy Initialization

```java
// ❌ BAD: Eager initialization
public class Service {
    private final HeavyObject heavy = new HeavyObject();  // Always created
}

// ✅ GOOD: Lazy initialization
public class Service {
    private HeavyObject heavy;
    
    private HeavyObject getHeavy() {
        if (heavy == null) {
            heavy = new HeavyObject();
        }
        return heavy;
    }
}

// ✅ BETTER: Use Supplier
public class Service {
    private final Supplier<HeavyObject> heavySupplier = 
        Suppliers.memoize(HeavyObject::new);
    
    private HeavyObject getHeavy() {
        return heavySupplier.get();
    }
}
```

### 4. Use Parallel Streams Carefully

```java
// ✅ GOOD: Parallel for CPU-intensive operations on large datasets
List<Integer> result = largeList.parallelStream()
    .filter(n -> isPrime(n))  // CPU-intensive
    .collect(Collectors.toList());

// ⚠️ CAUTION: Don't use for I/O operations
// Use CompletableFuture instead
```

### 5. Cache Expensive Computations

```java
// ✅ GOOD: Caffeine cache
LoadingCache<String, User> userCache = Caffeine.newBuilder()
    .maximumSize(10_000)
    .expireAfterWrite(10, TimeUnit.MINUTES)
    .build(id -> database.loadUser(id));

User user = userCache.get("user-123");
```

---

## Security Best Practices

### 1. Input Validation with Bean Validation

```java
import jakarta.validation.constraints.*;

public class CreateUserRequest {
    
    @NotBlank(message = "Email is required")
    @Email(message = "Invalid email format")
    private String email;
    
    @NotBlank(message = "Password is required")
    @Size(min = 8, message = "Password must be at least 8 characters")
    private String password;
    
    @Min(value = 0, message = "Age must be positive")
    @Max(value = 150, message = "Age must be realistic")
    private Integer age;
    
    // Getters and setters
}
```

### 2. SQL Injection Prevention

```java
// ❌ BAD: SQL injection vulnerability
public User getUser(String userId) {
    String query = "SELECT * FROM users WHERE id = '" + userId + "'";
    return jdbcTemplate.queryForObject(query, userRowMapper);
}

// ✅ GOOD: Prepared statement
public User getUser(String userId) {
    String query = "SELECT * FROM users WHERE id = ?";
    return jdbcTemplate.queryForObject(query, userRowMapper, userId);
}

// ✅ BETTER: Use JPA
public Optional<User> getUser(String userId) {
    return userRepository.findById(userId);
}
```

### 3. Password Hashing

```java
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;

// ✅ GOOD: Use BCrypt
private final BCryptPasswordEncoder encoder = new BCryptPasswordEncoder();

public void saveUser(String email, String password) {
    String hashedPassword = encoder.encode(password);
    // Save to database
}

public boolean checkPassword(String rawPassword, String hashedPassword) {
    return encoder.matches(rawPassword, hashedPassword);
}
```

### 4. Prevent Path Traversal

```java
import java.nio.file.Path;
import java.nio.file.Paths;

public String readFile(String filename) throws IOException {
    Path basePath = Paths.get("/app/data").toAbsolutePath().normalize();
    Path filePath = basePath.resolve(filename).normalize();
    
    // Ensure file is within base directory
    if (!filePath.startsWith(basePath)) {
        throw new SecurityException("Invalid file path");
    }
    
    return Files.readString(filePath);
}
```

### 5. Use HTTPS and Secure Cookies

```java
// application.yml
server:
  ssl:
    enabled: true
    key-store: classpath:keystore.p12
    key-store-password: ${SSL_PASSWORD}
    key-store-type: PKCS12

spring:
  session:
    cookie:
      secure: true
      http-only: true
      same-site: strict
```

---

## Error Handling

### 1. Custom Exception Hierarchy

```java
public class AppException extends RuntimeException {
    private final String code;
    private final int statusCode;
    
    public AppException(String message, String code, int statusCode) {
        super(message);
        this.code = code;
        this.statusCode = statusCode;
    }
    
    public String getCode() { return code; }
    public int getStatusCode() { return statusCode; }
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
```

### 2. Use try-with-resources for Multiple Resources

```java
// ✅ GOOD: Multiple resources
try (
    Connection conn = dataSource.getConnection();
    PreparedStatement stmt = conn.prepareStatement(query);
    ResultSet rs = stmt.executeQuery()
) {
    // Use resources
}  // All auto-closed in reverse order
```

### 3. Don't Catch Throwable

```java
// ❌ BAD: Catches errors too
try {
    operation();
} catch (Throwable t) {
    // Catches OutOfMemoryError, etc.
}

// ✅ GOOD: Catch Exception
try {
    operation();
} catch (Exception e) {
    log.error("Operation failed", e);
}
```

---

## Concurrency Patterns

### 1. Use CompletableFuture for Async

```java
// ✅ GOOD: Async operations
CompletableFuture<User> userFuture = CompletableFuture.supplyAsync(() -> 
    userRepository.findById("123")
);

CompletableFuture<List<Post>> postsFuture = CompletableFuture.supplyAsync(() -> 
    postRepository.findByUserId("123")
);

// Combine results
CompletableFuture<UserWithPosts> combined = userFuture.thenCombine(
    postsFuture,
    (user, posts) -> new UserWithPosts(user, posts)
);
```

### 2. Use ExecutorService Properly

```java
// ✅ GOOD: Use try-with-resources
try (ExecutorService executor = Executors.newFixedThreadPool(10)) {
    List<Future<Result>> futures = new ArrayList<>();
    
    for (Task task : tasks) {
        futures.add(executor.submit(() -> processTask(task)));
    }
    
    // Get results
    for (Future<Result> future : futures) {
        Result result = future.get();
    }
}  // Executor auto-shutdown
```

### 3. Use @Async in Spring

```java
@Service
public class NotificationService {
    
    @Async
    public CompletableFuture<Void> sendEmail(String to, String subject) {
        // Send email asynchronously
        emailService.send(to, subject);
        return CompletableFuture.completedFuture(null);
    }
}
```

### 4. Thread-Safe Collections

```java
// ❌ BAD: Not thread-safe
Map<String, User> cache = new HashMap<>();

// ✅ GOOD: Thread-safe
Map<String, User> cache = new ConcurrentHashMap<>();

// ✅ GOOD: Synchronized wrapper
List<String> syncList = Collections.synchronizedList(new ArrayList<>());
```

---

## Modern Java Features

### 1. Switch Expressions (Java 14+)

```java
// ❌ BAD: Traditional switch
String result;
switch (day) {
    case MONDAY:
    case FRIDAY:
        result = "Work";
        break;
    case SATURDAY:
    case SUNDAY:
        result = "Rest";
        break;
    default:
        result = "Unknown";
}

// ✅ GOOD: Switch expression
String result = switch (day) {
    case MONDAY, FRIDAY -> "Work";
    case SATURDAY, SUNDAY -> "Rest";
    default -> "Unknown";
};
```

### 2. Record Patterns (Java 19+)

```java
// ✅ GOOD: Destructuring records
public record Point(int x, int y) {}

if (obj instanceof Point(int x, int y)) {
    System.out.println("Point at " + x + ", " + y);
}
```

### 3. Virtual Threads (Java 21+)

```java
// ✅ GOOD: Lightweight threads
try (var executor = Executors.newVirtualThreadPerTaskExecutor()) {
    IntStream.range(0, 10_000).forEach(i -> {
        executor.submit(() -> {
            // Task
        });
    });
}
```

---

## Code Organization

### 1. Follow Single Responsibility Principle

```java
// ❌ BAD: Class does too much
public class UserService {
    public void createUser() { }
    public void sendEmail() { }
    public void logActivity() { }
}

// ✅ GOOD: Separate concerns
public class UserService {
    private final UserRepository repository;
    private final EmailService emailService;
    private final AuditService auditService;
    
    public void createUser(CreateUserDto dto) {
        User user = repository.save(toEntity(dto));
        emailService.sendWelcome(user.getEmail());
        auditService.log("USER_CREATED", user.getId());
    }
}
```

### 2. Constructor Injection (Spring)

```java
// ❌ BAD: Field injection
@Service
public class UserService {
    @Autowired
    private UserRepository repository;
}

// ✅ GOOD: Constructor injection
@Service
@RequiredArgsConstructor
public class UserService {
    private final UserRepository repository;
}
```

### 3. Use Interfaces for Abstraction

```java
// ✅ GOOD: Program to interfaces
public interface UserRepository {
    Optional<User> findById(String id);
    User save(User user);
}

@Repository
public class JpaUserRepository implements UserRepository {
    // Implementation
}
```

---

## Testing Best Practices

### 1. Use @DisplayName

```java
@Test
@DisplayName("Should create user when email is unique")
void testCreateUser_UniqueEmail() {
    // Test
}
```

### 2. Use @ParameterizedTest

```java
@ParameterizedTest
@ValueSource(strings = {"", "  ", "invalid"})
@DisplayName("Should reject invalid emails")
void testValidation_InvalidEmail(String email) {
    assertThrows(ValidationException.class, () -> {
        userService.createUser(email, "Name");
    });
}

@ParameterizedTest
@CsvSource({
    "john@example.com, John, true",
    "invalid-email, John, false",
    "jane@example.com, '', false"
})
void testCreateUser(String email, String name, boolean shouldSucceed) {
    // Test
}
```

### 3. Use Mockito Effectively

```java
@ExtendWith(MockitoExtension.class)
class UserServiceTest {
    
    @Mock
    private UserRepository repository;
    
    @InjectMocks
    private UserService userService;
    
    @Test
    void testCreateUser() {
        // Arrange
        when(repository.existsByEmail(anyString())).thenReturn(false);
        when(repository.save(any(User.class))).thenReturn(user);
        
        // Act
        UserDto result = userService.createUser(createDto);
        
        // Assert
        assertNotNull(result);
        verify(repository).save(any(User.class));
    }
}
```

---

## Common Gotchas

### 1. Integer Division

```java
int result = 5 / 2;        // 2 (integer division)
double result = 5 / 2;     // 2.0 (still integer division!)
double result = 5.0 / 2;   // 2.5 (floating point division)
double result = 5 / 2.0;   // 2.5 (floating point division)
```

### 2. Autoboxing Performance

```java
// ❌ BAD: Autoboxing in loop
Integer sum = 0;
for (int i = 0; i < 1000; i++) {
    sum += i;  // Autoboxing on each iteration
}

// ✅ GOOD: Use primitive
int sum = 0;
for (int i = 0; i < 1000; i++) {
    sum += i;
}
```

### 3. String Comparison

```java
String str1 = "hello";
String str2 = "hello";
str1 == str2;  // true (string pool)

String str3 = new String("hello");
str1 == str3;  // false (different objects)
str1.equals(str3);  // true (content comparison)
```

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-10-11 | Initial Java best practices guide |

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

