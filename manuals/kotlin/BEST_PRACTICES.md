# Kotlin Best Practices Guide

**Version**: 1.0.0  
**Last Updated**: 2025-10-11  
**Target Audience**: AI Agents & Developers

---

## Table of Contents

1. [Kotlin Idioms](#kotlin-idioms)
2. [Anti-Patterns](#anti-patterns)
3. [Performance Optimization](#performance-optimization)
4. [Security Best Practices](#security-best-practices)
5. [Error Handling](#error-handling)
6. [Coroutines & Async](#coroutines--async)
7. [Null Safety](#null-safety)
8. [Collections](#collections)
9. [Common Gotchas](#common-gotchas)

---

## Kotlin Idioms

### 1. Data Classes for DTOs/Models

**✅ Good**:
```kotlin
data class User(
    val id: String,
    val name: String,
    val email: String
)

// Automatic: equals(), hashCode(), toString(), copy(), componentN()
val user = User("1", "John", "john@example.com")
val updatedUser = user.copy(name = "Jane")
```

**❌ Bad**:
```kotlin
class User(
    val id: String,
    val name: String,
    val email: String
) {
    // Manual equals, hashCode, toString...
}
```

### 2. Null Safety

**✅ Good**:
```kotlin
fun processUser(user: User?) {
    // Safe call
    val name = user?.name
    
    // Elvis operator
    val displayName = user?.name ?: "Guest"
    
    // let scope function
    user?.let {
        println("User: ${it.name}")
    }
    
    // require for validation
    requireNotNull(user) { "User cannot be null" }
}
```

**❌ Bad**:
```kotlin
fun processUser(user: User?) {
    val name = user!!.name  // !! can throw NPE
    
    if (user == null) {
        return
    }
    // Verbose null checking
}
```

### 3. Extension Functions

**✅ Good**:
```kotlin
fun String.toSlug(): String =
    this.lowercase()
        .replace(Regex("[^a-z0-9]+"), "-")
        .trim('-')

val slug = "Hello World!".toSlug()  // "hello-world"
```

**❌ Bad**:
```kotlin
object StringUtils {
    fun toSlug(input: String): String = // ...
}

val slug = StringUtils.toSlug("Hello World!")
```

### 4. Scope Functions

**`let`** - Non-null operations:
```kotlin
user?.let { user ->
    println("User ID: ${user.id}")
    logActivity(user)
}
```

**`run`** - Object configuration and computing result:
```kotlin
val result = service.run {
    connect()
    query()
    disconnect()
}
```

**`with`** - Non-extension form of `run`:
```kotlin
val message = with(user) {
    "User $name ($email) created on $createdAt"
}
```

**`apply`** - Object configuration (returns `this`):
```kotlin
val user = User().apply {
    name = "John"
    email = "john@example.com"
}
```

**`also`** - Additional actions (returns `this`):
```kotlin
val user = User().also {
    logger.info("Creating user: ${it.name}")
}
```

### 5. When Expressions

**✅ Good**:
```kotlin
sealed class Result {
    data class Success(val data: String) : Result()
    data class Error(val message: String) : Result()
    data object Loading : Result()
}

fun handleResult(result: Result) = when (result) {
    is Result.Success -> println(result.data)
    is Result.Error -> logger.error(result.message)
    Result.Loading -> showSpinner()
}
```

**❌ Bad**:
```kotlin
if (result is Result.Success) {
    println((result as Result.Success).data)
} else if (result is Result.Error) {
    logger.error((result as Result.Error).message)
} else {
    showSpinner()
}
```

### 6. Destructuring

**✅ Good**:
```kotlin
val (id, name, email) = user
println("$id: $name ($email)")

// In loops
for ((key, value) in map) {
    println("$key = $value")
}

// In lambda parameters
users.map { (id, name) -> "$id: $name" }
```

### 7. Lazy Initialization

**✅ Good**:
```kotlin
class MyClass {
    val expensiveResource: Resource by lazy {
        Resource.initialize()
    }
    
    // Thread-safe by default
    // Use lazy(LazyThreadSafetyMode.NONE) if single-threaded
}
```

### 8. Delegates

**✅ Good**:
```kotlin
class User {
    var name: String by Delegates.observable("") { prop, old, new ->
        println("$old -> $new")
    }
    
    var email: String by Delegates.vetoable("") { prop, old, new ->
        new.contains("@")  // Validate
    }
}
```

### 9. DSL-Style Builders

**✅ Good**:
```kotlin
fun buildUser(init: UserBuilder.() -> Unit): User =
    UserBuilder().apply(init).build()

class UserBuilder {
    var name: String = ""
    var email: String = ""
    
    fun build() = User(name, email)
}

// Usage
val user = buildUser {
    name = "John"
    email = "john@example.com"
}
```

### 10. Prefer `val` over `var`

**✅ Good**:
```kotlin
val users = mutableListOf<User>()
users.add(user)  // List is immutable reference, content is mutable
```

**❌ Bad**:
```kotlin
var users = listOf<User>()
users = users + user  // Creates new list every time
```

---

## Anti-Patterns

### 1. Unnecessary Use of `!!`

**❌ Bad**:
```kotlin
val name = user!!.name!!.uppercase()!!
```

**✅ Good**:
```kotlin
val name = user?.name?.uppercase()
```

### 2. Ignoring Sealed Classes

**❌ Bad**:
```kotlin
enum class Status { SUCCESS, ERROR, LOADING }

fun handle(status: Status) = when (status) {
    Status.SUCCESS -> println("OK")
    Status.ERROR -> println("Failed")
    else -> println("Loading")  // else needed, no compiler exhaustiveness check
}
```

**✅ Good**:
```kotlin
sealed class Status {
    data object Success : Status()
    data object Error : Status()
    data object Loading : Status()
}

fun handle(status: Status) = when (status) {
    Status.Success -> println("OK")
    Status.Error -> println("Failed")
    Status.Loading -> println("Loading")
    // Compiler ensures exhaustiveness
}
```

### 3. Platform Types (Java interop)

**❌ Bad**:
```kotlin
// JavaApi.getUser() returns User! (platform type)
val user = JavaApi.getUser()
val name = user.name  // Can throw NPE if null
```

**✅ Good**:
```kotlin
val user: User? = JavaApi.getUser()
val name = user?.name ?: "Unknown"
```

### 4. Blocking in Coroutines

**❌ Bad**:
```kotlin
suspend fun fetchUser(): User {
    Thread.sleep(1000)  // Blocks thread
    return User()
}
```

**✅ Good**:
```kotlin
suspend fun fetchUser(): User {
    delay(1000)  // Suspends coroutine, doesn't block thread
    return User()
}
```

### 5. Using `var` for Collections

**❌ Bad**:
```kotlin
var users = listOf<User>()
users = users + newUser  // Creates new list
```

**✅ Good**:
```kotlin
val users = mutableListOf<User>()
users.add(newUser)
```

### 6. Unnecessary Type Annotations

**❌ Bad**:
```kotlin
val name: String = "John"
val age: Int = 25
```

**✅ Good**:
```kotlin
val name = "John"
val age = 25
```

---

## Performance Optimization

### 1. Use Sequences for Large Collections

**✅ Good (Lazy evaluation)**:
```kotlin
val result = listOf(1..1_000_000)
    .asSequence()
    .filter { it % 2 == 0 }
    .map { it * 2 }
    .take(10)
    .toList()
```

**❌ Bad (Eager evaluation, multiple intermediate lists)**:
```kotlin
val result = listOf(1..1_000_000)
    .filter { it % 2 == 0 }  // Creates list
    .map { it * 2 }          // Creates another list
    .take(10)
```

### 2. Inline Functions

**✅ Good**:
```kotlin
inline fun <T> measureTime(block: () -> T): Pair<T, Long> {
    val start = System.nanoTime()
    val result = block()
    val time = System.nanoTime() - start
    return result to time
}

// Lambda is inlined, no extra allocation
val (result, time) = measureTime {
    expensiveOperation()
}
```

### 3. Avoid Creating Unnecessary Objects

**❌ Bad**:
```kotlin
fun formatUser(user: User): String =
    StringBuilder()
        .append("User: ")
        .append(user.name)
        .toString()
```

**✅ Good**:
```kotlin
fun formatUser(user: User): String =
    "User: ${user.name}"  // Uses StringBuilder internally
```

### 4. Use Primitive Types When Possible

**✅ Good**:
```kotlin
@JvmInline
value class UserId(val value: String)

// No boxing, compiles to String at runtime
```

**❌ Bad**:
```kotlin
data class UserId(val value: String)  // Wrapper object created
```

### 5. Lazy Initialization for Heavy Objects

**✅ Good**:
```kotlin
class Service {
    private val database by lazy {
        Database.connect()  // Only initialized when accessed
    }
}
```

---

## Security Best Practices

### 1. Input Validation

**✅ Good**:
```kotlin
fun createUser(email: String, password: String) {
    require(email.matches(Regex("^[A-Za-z0-9+_.-]+@(.+)\$"))) {
        "Invalid email format"
    }
    
    require(password.length >= 8) {
        "Password must be at least 8 characters"
    }
    
    require(password.any { it.isUpperCase() }) {
        "Password must contain uppercase letter"
    }
}
```

### 2. SQL Injection Prevention (JPA)

**✅ Good**:
```kotlin
@Query("SELECT u FROM User u WHERE u.email = :email")
fun findByEmail(@Param("email") email: String): User?
```

**❌ Bad**:
```kotlin
// Never do this
@Query(value = "SELECT * FROM users WHERE email = '$email'", nativeQuery = true)
fun findByEmail(email: String): User?
```

### 3. Secrets Management

**✅ Good**:
```kotlin
@Configuration
class DatabaseConfig {
    @Value("\${database.password}")
    private lateinit var password: String
    
    // Never log or expose password
}
```

**❌ Bad**:
```kotlin
val password = "hardcoded-secret"  // ❌
logger.info("Password: $password")  // ❌
```

### 4. Avoid Exposing Internal Implementation

**✅ Good**:
```kotlin
class UserService {
    private val users = mutableListOf<User>()
    
    fun getUsers(): List<User> = users.toList()  // Return copy
}
```

**❌ Bad**:
```kotlin
class UserService {
    val users = mutableListOf<User>()  // External modification possible
}
```

---

## Error Handling

### 1. Result Type Pattern

**✅ Good**:
```kotlin
sealed class Result<out T> {
    data class Success<T>(val data: T) : Result<T>()
    data class Error(val exception: Throwable) : Result<Nothing>()
}

fun fetchUser(id: String): Result<User> =
    try {
        val user = userRepository.findById(id)
        Result.Success(user)
    } catch (e: Exception) {
        Result.Error(e)
    }

// Usage
when (val result = fetchUser("123")) {
    is Result.Success -> println(result.data)
    is Result.Error -> logger.error("Failed", result.exception)
}
```

### 2. Use Arrow-kt Either

**✅ Good**:
```kotlin
import arrow.core.Either
import arrow.core.left
import arrow.core.right

fun divide(a: Int, b: Int): Either<String, Int> =
    if (b == 0) "Division by zero".left()
    else (a / b).right()

// Usage
divide(10, 2).fold(
    { error -> println("Error: $error") },
    { result -> println("Result: $result") }
)
```

### 3. Custom Exceptions

**✅ Good**:
```kotlin
sealed class AppException(message: String) : RuntimeException(message) {
    class NotFound(resource: String, id: String) : 
        AppException("$resource with id $id not found")
    
    class Validation(message: String) : 
        AppException("Validation error: $message")
    
    class Unauthorized(message: String) : 
        AppException("Unauthorized: $message")
}

// Usage
throw AppException.NotFound("User", userId)
```

### 4. Use `runCatching`

**✅ Good**:
```kotlin
val result = runCatching {
    riskyOperation()
}.onSuccess { data ->
    println("Success: $data")
}.onFailure { error ->
    logger.error("Failed", error)
}.getOrDefault(defaultValue)
```

---

## Coroutines & Async

### 1. Structured Concurrency

**✅ Good**:
```kotlin
suspend fun fetchUserData(userId: String): UserData = coroutineScope {
    val user = async { userRepository.findById(userId) }
    val orders = async { orderRepository.findByUserId(userId) }
    val preferences = async { preferenceRepository.findByUserId(userId) }
    
    UserData(
        user = user.await(),
        orders = orders.await(),
        preferences = preferences.await()
    )
}
```

### 2. Use Proper Dispatchers

**✅ Good**:
```kotlin
suspend fun processFile(path: String) = withContext(Dispatchers.IO) {
    File(path).readLines()
}

suspend fun computeHeavy(data: List<Int>) = withContext(Dispatchers.Default) {
    data.map { it * it }.sum()
}

suspend fun updateUI(text: String) = withContext(Dispatchers.Main) {
    textView.text = text
}
```

### 3. Exception Handling in Coroutines

**✅ Good**:
```kotlin
val exceptionHandler = CoroutineExceptionHandler { _, exception ->
    logger.error("Coroutine failed", exception)
}

CoroutineScope(Dispatchers.Default + exceptionHandler).launch {
    riskyOperation()
}
```

### 4. Flow for Streams

**✅ Good**:
```kotlin
fun observeUsers(): Flow<List<User>> = flow {
    while (true) {
        emit(userRepository.findAll())
        delay(1000)
    }
}.flowOn(Dispatchers.IO)

// Usage
viewModelScope.launch {
    observeUsers()
        .catch { e -> logger.error("Error", e) }
        .collect { users ->
            _usersState.value = users
        }
}
```

### 5. Cancellation Support

**✅ Good**:
```kotlin
suspend fun processItems(items: List<Item>) = coroutineScope {
    items.forEach { item ->
        ensureActive()  // Check if coroutine is cancelled
        process(item)
    }
}
```

---

## Null Safety

### 1. Avoid Using `!!`

**❌ Bad**:
```kotlin
val name = user!!.name!!
```

**✅ Good**:
```kotlin
val name = user?.name ?: "Unknown"
```

### 2. Use `requireNotNull` and `checkNotNull`

**✅ Good**:
```kotlin
fun processUser(user: User?) {
    val validUser = requireNotNull(user) { "User cannot be null" }
    // validUser is now non-null
}
```

### 3. Use Safe Cast (`as?`)

**✅ Good**:
```kotlin
val user = value as? User
user?.let {
    processUser(it)
}
```

**❌ Bad**:
```kotlin
if (value is User) {
    val user = value as User
    processUser(user)
}
```

---

## Collections

### 1. Use Appropriate Collection Type

**✅ Good**:
```kotlin
// Read-only
val users: List<User> = listOf(...)

// Mutable
val users: MutableList<User> = mutableListOf()

// Set for uniqueness
val uniqueIds: Set<String> = setOf(...)

// Map for key-value
val userMap: Map<String, User> = mapOf(...)
```

### 2. Collection Operations

**✅ Good**:
```kotlin
// Filter + map
val names = users
    .filter { it.age >= 18 }
    .map { it.name }

// GroupBy
val byAge = users.groupBy { it.age }

// Partition
val (adults, minors) = users.partition { it.age >= 18 }

// Associate
val userMap = users.associateBy { it.id }
```

### 3. Use `sequence` for Large Collections

**✅ Good**:
```kotlin
val result = (1..1_000_000).asSequence()
    .filter { it % 2 == 0 }
    .map { it * 2 }
    .take(100)
    .toList()
```

---

## Common Gotchas

### 1. Mutable vs Immutable Collections

```kotlin
// Immutable reference to mutable list
val list = mutableListOf(1, 2, 3)
list.add(4)  // OK

// Immutable list
val list2 = listOf(1, 2, 3)
// list2.add(4)  // ❌ Compilation error
```

### 2. Companion Object Not Static

```kotlin
class MyClass {
    companion object {
        @JvmStatic  // Needed for true Java static
        fun create() = MyClass()
    }
}
```

### 3. Data Class Copy

```kotlin
val user = User(id = "1", name = "John")
val copied = user.copy(name = "Jane")
// user and copied are different instances
```

### 4. lateinit vs lazy

```kotlin
class MyClass {
    // lateinit: var, non-null, initialized later
    lateinit var config: Config
    
    // lazy: val, initialized on first access
    val database by lazy { Database.connect() }
}
```

### 5. Smart Casts

```kotlin
fun process(value: Any) {
    if (value is String) {
        println(value.length)  // Smart cast to String
    }
}
```

---

## Additional Resources

- [Kotlin Coding Conventions](https://kotlinlang.org/docs/coding-conventions.html)
- [Kotlin Official Style Guide](https://developer.android.com/kotlin/style-guide)
- [Effective Kotlin](https://kt.academy/book/effectivekotlin)
- [Kotlin Coroutines Guide](https://kotlinlang.org/docs/coroutines-guide.html)
- [Arrow-kt Functional Programming](https://arrow-kt.io/)

---

**Version**: 1.0.0  
**Maintained by**: HiveLLM Governance Team  
**Last Updated**: 2025-10-11

