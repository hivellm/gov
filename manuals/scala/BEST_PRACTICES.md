# Scala Best Practices

**Version**: 1.0.0  
**Last Updated**: 2025-10-11  
**Target Audience**: AI Agents developing Scala projects  
**Scala Version**: 2.13.x / 3.x

---

## Table of Contents

1. [Scala Idioms](#scala-idioms)
2. [Anti-Patterns](#anti-patterns)
3. [Performance Optimization](#performance-optimization)
4. [Security Best Practices](#security-best-practices)
5. [Error Handling](#error-handling)
6. [Functional Programming](#functional-programming)
7. [Type System](#type-system)
8. [Code Organization](#code-organization)
9. [Testing Best Practices](#testing-best-practices)
10. [Common Gotchas](#common-gotchas)

---

## Scala Idioms

### 1. Prefer `val` Over `var`

```scala
// ❌ BAD: Mutable variable
var count = 0
count = count + 1

// ✅ GOOD: Immutable value
val count = 0
val newCount = count + 1

// ✅ GOOD: Immutable collections
val numbers = List(1, 2, 3)
val doubled = numbers.map(_ * 2)  // Returns new list
```

### 2. Use Case Classes for Data

```scala
// ❌ BAD: Regular class with boilerplate
class User(val name: String, val email: String) {
  override def equals(obj: Any): Boolean = // ...
  override def hashCode(): Int = // ...
  override def toString: String = // ...
}

// ✅ GOOD: Case class (automatic equals, hashCode, toString, copy)
case class User(name: String, email: String)

// Usage
val user = User("John", "john@example.com")
val updated = user.copy(email = "new@example.com")
```

### 3. Use Pattern Matching

```scala
// ❌ BAD: if-else chains
def describe(x: Any): String = {
  if (x.isInstanceOf[Int]) {
    "It's an integer"
  } else if (x.isInstanceOf[String]) {
    "It's a string"
  } else {
    "Unknown"
  }
}

// ✅ GOOD: Pattern matching
def describe(x: Any): String = x match {
  case _: Int => "It's an integer"
  case _: String => "It's a string"
  case _ => "Unknown"
}

// ✅ BETTER: Sealed trait for exhaustive matching
sealed trait Shape
case class Circle(radius: Double) extends Shape
case class Rectangle(width: Double, height: Double) extends Shape

def area(shape: Shape): Double = shape match {
  case Circle(r) => math.Pi * r * r
  case Rectangle(w, h) => w * h
}  // Compiler ensures all cases covered
```

### 4. Use For-Comprehensions

```scala
// ❌ BAD: Nested flatMaps
Option(user).flatMap { u =>
  Option(u.profile).flatMap { p =>
    Option(p.address).map { a =>
      a.city
    }
  }
}

// ✅ GOOD: For-comprehension
for {
  u <- Option(user)
  p <- Option(u.profile)
  a <- Option(p.address)
} yield a.city

// ✅ GOOD: With filtering
for {
  n <- numbers
  if n % 2 == 0
  if n > 10
} yield n * 2
```

### 5. Use Underscore for Anonymous Functions

```scala
// ❌ VERBOSE: Explicit parameter
numbers.map(x => x * 2)
numbers.filter(x => x > 10)

// ✅ GOOD: Underscore syntax
numbers.map(_ * 2)
numbers.filter(_ > 10)

// ⚠️ LIMITATION: Only works once per underscore
numbers.map(_ + _)  // Won't compile!
numbers.map(x => x + x)  // Correct
```

### 6. Use Apply Methods for Constructors

```scala
// Companion object with apply
case class User(name: String, email: String)

object User {
  def apply(name: String): User = {
    User(name, s"${name.toLowerCase}@example.com")
  }
}

// Usage
val user1 = User("John", "john@example.com")
val user2 = User("Jane")  // Uses apply method
```

### 7. Use Implicit Classes for Extension Methods

```scala
// ✅ GOOD: Extending existing types
implicit class StringOps(s: String) {
  def toTitleCase: String = s.split(" ").map(_.capitalize).mkString(" ")
  def isEmail: Boolean = s.contains("@")
}

// Usage
"hello world".toTitleCase  // "Hello World"
"test@example.com".isEmail  // true

// Scala 3: Extension methods (cleaner syntax)
extension (s: String)
  def toTitleCase: String = s.split(" ").map(_.capitalize).mkString(" ")
  def isEmail: Boolean = s.contains("@")
```

### 8. Use Type Aliases

```scala
// ✅ GOOD: Type aliases for clarity
type UserId = Int
type Email = String
type Result[A] = Either[String, A]

def getUser(id: UserId): Result[User] = {
  if (id > 0) Right(User(id, "John"))
  else Left("Invalid user ID")
}
```

### 9. Use Sealed Traits for ADTs

```scala
// ✅ GOOD: Algebraic Data Types
sealed trait Result[+A]
case class Success[A](value: A) extends Result[A]
case class Failure(error: String) extends Result[Nothing]
case object Pending extends Result[Nothing]

// Exhaustive pattern matching
def handle[A](result: Result[A]): String = result match {
  case Success(value) => s"Got: $value"
  case Failure(error) => s"Error: $error"
  case Pending => "Still processing"
}
```

### 10. Use Context Bounds

```scala
// ❌ VERBOSE: Explicit implicit parameter
def sort[A](list: List[A])(implicit ord: Ordering[A]): List[A] =
  list.sorted(ord)

// ✅ GOOD: Context bound
def sort[A: Ordering](list: List[A]): List[A] =
  list.sorted

// ✅ GOOD: Multiple context bounds
def process[A: Ordering: Numeric](list: List[A]): List[A] =
  list.sorted.map(implicitly[Numeric[A]].plus(_, implicitly[Numeric[A]].one))
```

---

## Anti-Patterns

### 1. Using `null`

```scala
// ❌ BAD: null values
def findUser(id: Int): User = {
  if (id > 0) new User(id, "John")
  else null  // Don't use null!
}

// ✅ GOOD: Option
def findUser(id: Int): Option[User] = {
  if (id > 0) Some(User(id, "John"))
  else None
}
```

### 2. Using `return`

```scala
// ❌ BAD: Explicit return (breaks functional style)
def max(a: Int, b: Int): Int = {
  if (a > b) {
    return a
  } else {
    return b
  }
}

// ✅ GOOD: Implicit return (last expression)
def max(a: Int, b: Int): Int = {
  if (a > b) a
  else b
}

// ✅ BETTER: Single expression
def max(a: Int, b: Int): Int = if (a > b) a else b

// ✅ BEST: Built-in
def max(a: Int, b: Int): Int = math.max(a, b)
```

### 3. Using Mutable Collections

```scala
// ❌ BAD: Mutable collection
import scala.collection.mutable

val list = mutable.ListBuffer(1, 2, 3)
list += 4  // Mutation

// ✅ GOOD: Immutable collection
val list = List(1, 2, 3)
val newList = list :+ 4  // Returns new list
```

### 4. Not Using Sealed Traits

```scala
// ❌ BAD: Non-sealed trait (can be extended anywhere)
trait Status
case object Active extends Status
case object Inactive extends Status

// Another file could add:
// case object Unknown extends Status  // Breaks pattern matching!

// ✅ GOOD: Sealed trait (all implementations in same file)
sealed trait Status
case object Active extends Status
case object Inactive extends Status

// Compiler ensures exhaustive matching
def describe(status: Status): String = status match {
  case Active => "Active"
  case Inactive => "Inactive"
  // No need for wildcard case
}
```

### 5. Overusing Implicits

```scala
// ❌ BAD: Too many implicits (confusing)
implicit val config: Config = ???
implicit val logger: Logger = ???
implicit val database: Database = ???
implicit val cache: Cache = ???

def process(data: String)(implicit a: A, b: B, c: C, d: D, e: E): Result = ???

// ✅ GOOD: Explicit parameters when appropriate
class Service(config: Config, logger: Logger, database: Database)

// ✅ GOOD: Implicits for type classes only
def sort[A: Ordering](list: List[A]): List[A] = list.sorted
```

### 6. Using `asInstanceOf` and `isInstanceOf`

```scala
// ❌ BAD: Type casting
def process(value: Any): String = {
  if (value.isInstanceOf[String]) {
    value.asInstanceOf[String].toUpperCase
  } else {
    "Unknown"
  }
}

// ✅ GOOD: Pattern matching
def process(value: Any): String = value match {
  case s: String => s.toUpperCase
  case _ => "Unknown"
}
```

### 7. Large Classes

```scala
// ❌ BAD: God class
class UserService {
  def create(user: User): User = ???
  def update(user: User): User = ???
  def delete(id: Int): Unit = ???
  def findById(id: Int): Option[User] = ???
  def findAll(): List[User] = ???
  def authenticate(email: String, password: String): Boolean = ???
  def sendEmail(user: User): Unit = ???
  def validateEmail(email: String): Boolean = ???
  def hashPassword(password: String): String = ???
  // ... 50 more methods
}

// ✅ GOOD: Separated concerns
class UserRepository {
  def create(user: User): User = ???
  def update(user: User): User = ???
  def delete(id: Int): Unit = ???
  def findById(id: Int): Option[User] = ???
  def findAll(): List[User] = ???
}

class AuthService {
  def authenticate(email: String, password: String): Boolean = ???
}

class EmailService {
  def sendWelcome(user: User): Unit = ???
}
```

### 8. Not Using Tail Recursion

```scala
// ❌ BAD: Non-tail recursive (stack overflow)
def factorial(n: Int): BigInt = {
  if (n <= 1) 1
  else n * factorial(n - 1)
}

// ✅ GOOD: Tail recursive
import scala.annotation.tailrec

@tailrec
def factorial(n: Int, acc: BigInt = 1): BigInt = {
  if (n <= 1) acc
  else factorial(n - 1, n * acc)
}
```

---

## Performance Optimization

### 1. Use Views for Lazy Evaluation

```scala
// ❌ BAD: Eager evaluation creates intermediate collections
val result = (1 to 1000000)
  .map(_ * 2)       // Creates collection of 1M elements
  .filter(_ > 10)   // Creates another collection
  .take(10)         // Finally takes 10

// ✅ GOOD: Lazy evaluation with view
val result = (1 to 1000000).view
  .map(_ * 2)
  .filter(_ > 10)
  .take(10)
  .toList  // Only computes 10 elements
```

### 2. Use Builder for Collection Construction

```scala
// ❌ BAD: Repeated concatenation
var list = List.empty[Int]
for (i <- 1 to 1000) {
  list = list :+ i  // O(n) for each append
}

// ✅ GOOD: Use builder
import scala.collection.mutable.ListBuffer

val builder = ListBuffer.empty[Int]
for (i <- 1 to 1000) {
  builder += i  // O(1) for each append
}
val list = builder.result()

// ✅ BETTER: Use range
val list = (1 to 1000).toList
```

### 3. Use Parallel Collections Wisely

```scala
// ✅ GOOD: Parallel processing for CPU-bound tasks
val numbers = (1 to 1000000).toList

val result = numbers.par
  .filter(_ % 2 == 0)
  .map(expensiveComputation)
  .sum

// ⚠️ WARNING: Overhead for small collections
val smallList = List(1, 2, 3, 4, 5)
val result = smallList.par.map(_ * 2)  // Overhead > benefit

// ⚠️ WARNING: Side effects in parallel
numbers.par.foreach(println)  // Order not guaranteed
```

### 4. Avoid Repeated Option.get

```scala
// ❌ BAD: Repeated get (can throw)
val opt: Option[User] = findUser(1)
if (opt.isDefined) {
  val user = opt.get  // Unsafe!
  process(user)
}

// ✅ GOOD: Pattern matching
findUser(1) match {
  case Some(user) => process(user)
  case None => handleNotFound()
}

// ✅ GOOD: map/flatMap
findUser(1).map(process)

// ✅ GOOD: foreach
findUser(1).foreach(process)
```

### 5. Use LazyList for Infinite Sequences

```scala
// ✅ GOOD: Lazy infinite sequence
def fibonacci: LazyList[BigInt] = {
  def fib(a: BigInt, b: BigInt): LazyList[BigInt] =
    a #:: fib(b, a + b)
  fib(0, 1)
}

// Take only what you need
val first10 = fibonacci.take(10).toList

// ✅ GOOD: Memoization with LazyList
lazy val primes: LazyList[Int] = {
  def sieve(s: LazyList[Int]): LazyList[Int] =
    s.head #:: sieve(s.tail.filter(_ % s.head != 0))
  sieve(LazyList.from(2))
}
```

### 6. Benchmark Critical Code

```scala
// ✅ GOOD: Use JMH for accurate benchmarks
// build.sbt:
// enablePlugins(JmhPlugin)

import org.openjdk.jmh.annotations._

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
class MyBenchmark {

  @Benchmark
  def listAppend(): List[Int] = {
    var list = List.empty[Int]
    for (i <- 1 to 1000) {
      list = list :+ i
    }
    list
  }

  @Benchmark
  def listBuilder(): List[Int] = {
    val builder = List.newBuilder[Int]
    for (i <- 1 to 1000) {
      builder += i
    }
    builder.result()
  }
}
```

---

## Security Best Practices

### 1. Validate All Input

```scala
// ❌ BAD: No validation
def createUser(email: String, age: Int): User = {
  User(email, age)
}

// ✅ GOOD: Validation with Either
def createUser(email: String, age: Int): Either[String, User] = {
  for {
    validEmail <- validateEmail(email)
    validAge <- validateAge(age)
  } yield User(validEmail, validAge)
}

def validateEmail(email: String): Either[String, String] = {
  if (email.matches("^[\\w-\\.]+@([\\w-]+\\.)+[\\w-]{2,4}$"))
    Right(email)
  else
    Left("Invalid email")
}

def validateAge(age: Int): Either[String, Int] = {
  if (age >= 0 && age <= 150)
    Right(age)
  else
    Left("Age must be between 0 and 150")
}
```

### 2. Use Type-Safe SQL

```scala
// ❌ BAD: String interpolation (SQL injection)
def findUser(email: String) = {
  sql"SELECT * FROM users WHERE email = '${email}'".query[User]
}

// ✅ GOOD: Parameterized queries (Doobie)
import doobie._
import doobie.implicits._

def findUser(email: String): Query0[User] =
  sql"SELECT id, name, email FROM users WHERE email = $email"
    .query[User]

// ✅ GOOD: Type-safe queries (Slick)
import slick.jdbc.PostgresProfile.api._

def findUser(email: String) =
  users.filter(_.email === email).result
```

### 3. Avoid Serialization Vulnerabilities

```scala
// ❌ BAD: Java serialization (security risk)
def serialize(obj: Serializable): Array[Byte] = {
  // Java serialization is vulnerable
}

// ✅ GOOD: JSON serialization
import io.circe.syntax._
import io.circe.generic.auto._

case class User(name: String, email: String)

def serialize(user: User): String = user.asJson.noSpaces
```

### 4. Use Strong Types for Sensitive Data

```scala
// ❌ BAD: Plain String for sensitive data
case class User(name: String, password: String)

// ✅ GOOD: Wrapper types
case class Password(private val value: String) {
  override def toString: String = "***"  // Don't leak in logs
}

case class User(name: String, password: Password)
```

### 5. Sanitize User Input

```scala
// ✅ GOOD: HTML sanitization
import org.jsoup.Jsoup
import org.jsoup.safety.Safelist

def sanitizeHtml(input: String): String = {
  Jsoup.clean(input, Safelist.basic())
}

// ✅ GOOD: SQL escaping (use library)
// Use Slick, Doobie, or similar for automatic escaping
```

---

## Error Handling

### 1. Use Option for Nullable Values

```scala
// ❌ BAD: Null checking
def findUser(id: Int): User = {
  val user = database.find(id)
  if (user == null) {
    throw new RuntimeException("User not found")
  }
  user
}

// ✅ GOOD: Option
def findUser(id: Int): Option[User] = {
  database.find(id)  // Returns Option[User]
}

// Usage
findUser(1) match {
  case Some(user) => process(user)
  case None => handleNotFound()
}
```

### 2. Use Either for Error Details

```scala
// ❌ BAD: Throwing exceptions
def divide(a: Int, b: Int): Double = {
  if (b == 0) throw new ArithmeticException("Division by zero")
  a.toDouble / b
}

// ✅ GOOD: Either for errors
def divide(a: Int, b: Int): Either[String, Double] = {
  if (b == 0) Left("Division by zero")
  else Right(a.toDouble / b)
}

// ✅ BETTER: Custom error types
sealed trait MathError
case object DivisionByZero extends MathError
case class Overflow(value: Long) extends MathError

def divide(a: Int, b: Int): Either[MathError, Double] = {
  if (b == 0) Left(DivisionByZero)
  else Right(a.toDouble / b)
}
```

### 3. Use Try for Exception Handling

```scala
import scala.util.{Try, Success, Failure}

// ✅ GOOD: Try for exception-throwing code
def parseInt(s: String): Try[Int] = Try(s.toInt)

parseInt("42") match {
  case Success(value) => println(s"Parsed: $value")
  case Failure(exception) => println(s"Failed: ${exception.getMessage}")
}

// ✅ GOOD: Recovering from failures
val result = parseInt("abc").recover {
  case _: NumberFormatException => 0
}
```

### 4. Use Validated for Accumulating Errors

```scala
import cats.data.Validated
import cats.data.Validated.{Valid, Invalid}
import cats.implicits._

type ValidationResult[A] = Validated[List[String], A]

def validateName(name: String): ValidationResult[String] =
  if (name.nonEmpty) Valid(name)
  else Invalid(List("Name cannot be empty"))

def validateEmail(email: String): ValidationResult[String] =
  if (email.contains("@")) Valid(email)
  else Invalid(List("Email must contain @"))

def validateAge(age: Int): ValidationResult[Int] =
  if (age >= 0 && age <= 150) Valid(age)
  else Invalid(List("Age must be between 0 and 150"))

// Accumulates all errors
def validateUser(name: String, email: String, age: Int): ValidationResult[User] =
  (validateName(name), validateEmail(email), validateAge(age)).mapN(User)

// Usage
validateUser("", "invalid", -1) match {
  case Valid(user) => println(s"Valid user: $user")
  case Invalid(errors) => println(s"Errors: ${errors.mkString(", ")}")
}
```

### 5. Don't Swallow Exceptions

```scala
// ❌ BAD: Silent failure
try {
  riskyOperation()
} catch {
  case _: Exception => // Silently ignored
}

// ✅ GOOD: Log and handle
import com.typesafe.scalalogging.LazyLogging

class MyService extends LazyLogging {
  def process(): Either[String, Result] = {
    try {
      Right(riskyOperation())
    } catch {
      case e: SpecificException =>
        logger.error(s"Operation failed: ${e.getMessage}", e)
        Left(s"Failed: ${e.getMessage}")
      case e: Exception =>
        logger.error(s"Unexpected error: ${e.getMessage}", e)
        throw e  // Re-throw unexpected exceptions
    }
  }
}
```

---

## Functional Programming

### 1. Use Pure Functions

```scala
// ❌ BAD: Impure function (side effects)
var total = 0
def add(x: Int): Int = {
  total += x  // Side effect!
  total
}

// ✅ GOOD: Pure function
def add(x: Int, current: Int): Int = current + x

// Usage
val total = numbers.foldLeft(0)(add)
```

### 2. Use Higher-Order Functions

```scala
// ✅ GOOD: Functions as parameters
def retry[A](n: Int)(block: => A): A = {
  try {
    block
  } catch {
    case e: Exception if n > 0 =>
      println(s"Retrying... ($n attempts left)")
      retry(n - 1)(block)
  }
}

// Usage
val result = retry(3) {
  fetchDataFromApi()
}

// ✅ GOOD: Returning functions
def multiplier(factor: Int): Int => Int = {
  (x: Int) => x * factor
}

val double = multiplier(2)
val triple = multiplier(3)

double(5)  // 10
triple(5)  // 15
```

### 3. Use Function Composition

```scala
// ✅ GOOD: Compose functions
val addOne: Int => Int = _ + 1
val double: Int => Int = _ * 2
val square: Int => Int = x => x * x

// Compose (right to left)
val addOneAndDouble = addOne andThen double
val doubleAndAddOne = double compose addOne

addOneAndDouble(5)  // (5 + 1) * 2 = 12
doubleAndAddOne(5)  // (5 * 2) + 1 = 11

// ✅ GOOD: Chaining
val process = addOne andThen double andThen square
process(3)  // ((3 + 1) * 2)^2 = 64
```

### 4. Use Monads for Sequencing

```scala
// ✅ GOOD: Option monad
def getUser(id: Int): Option[User] = ???
def getProfile(user: User): Option[Profile] = ???
def getAddress(profile: Profile): Option[Address] = ???

// Monadic composition
val result: Option[String] = for {
  user <- getUser(1)
  profile <- getProfile(user)
  address <- getAddress(profile)
} yield address.city

// ✅ GOOD: Either monad
def validateInput(input: String): Either[Error, Input] = ???
def process(input: Input): Either[Error, Result] = ???
def save(result: Result): Either[Error, Unit] = ???

val program: Either[Error, Unit] = for {
  input <- validateInput(rawInput)
  result <- process(input)
  _ <- save(result)
} yield ()
```

### 5. Use Tail Recursion

```scala
import scala.annotation.tailrec

// ✅ GOOD: Tail recursive list operations
@tailrec
def sum(list: List[Int], acc: Int = 0): Int = list match {
  case Nil => acc
  case head :: tail => sum(tail, acc + head)
}

@tailrec
def reverse[A](list: List[A], acc: List[A] = Nil): List[A] = list match {
  case Nil => acc
  case head :: tail => reverse(tail, head :: acc)
}
```

---

## Type System

### 1. Use Phantom Types

```scala
// ✅ GOOD: Phantom types for compile-time safety
sealed trait State
sealed trait Open extends State
sealed trait Closed extends State

class Door[S <: State] private (isOpen: Boolean) {
  def open(implicit ev: S =:= Closed): Door[Open] = {
    println("Opening door")
    new Door[Open](true)
  }
  
  def close(implicit ev: S =:= Open): Door[Closed] = {
    println("Closing door")
    new Door[Closed](false)
  }
}

object Door {
  def apply(): Door[Closed] = new Door[Closed](false)
}

// Usage
val door = Door()
val openDoor = door.open
val closedDoor = openDoor.close
// door.close  // Compile error: door is already closed
```

### 2. Use Type Classes

```scala
// ✅ GOOD: Type class pattern
trait JsonEncoder[A] {
  def encode(value: A): String
}

object JsonEncoder {
  implicit val intEncoder: JsonEncoder[Int] =
    (value: Int) => value.toString

  implicit val stringEncoder: JsonEncoder[String] =
    (value: String) => s""""$value""""

  implicit def listEncoder[A](implicit encoder: JsonEncoder[A]): JsonEncoder[List[A]] =
    (values: List[A]) => values.map(encoder.encode).mkString("[", ",", "]")
}

def toJson[A](value: A)(implicit encoder: JsonEncoder[A]): String =
  encoder.encode(value)

// Usage
toJson(42)  // "42"
toJson("hello")  // "\"hello\""
toJson(List(1, 2, 3))  // "[1,2,3]"
```

### 3. Use Variance Appropriately

```scala
// ✅ GOOD: Covariance for immutable containers
trait Producer[+A] {
  def produce(): A
}

// Can use Dog where Animal is expected
val dogProducer: Producer[Dog] = ???
val animalProducer: Producer[Animal] = dogProducer  // OK

// ✅ GOOD: Contravariance for consumers
trait Consumer[-A] {
  def consume(a: A): Unit
}

// Can use Animal consumer where Dog consumer is expected
val animalConsumer: Consumer[Animal] = ???
val dogConsumer: Consumer[Dog] = animalConsumer  // OK

// ✅ GOOD: Invariance for mutable containers
class Box[A](var value: A)  // Must be invariant
```

### 4. Use Path-Dependent Types

```scala
// ✅ GOOD: Path-dependent types
trait Database {
  type Entity
  def save(entity: Entity): Unit
  def load(id: Int): Entity
}

object UserDatabase extends Database {
  type Entity = User
  def save(entity: User): Unit = ???
  def load(id: Int): User = ???
}

object OrderDatabase extends Database {
  type Entity = Order
  def save(entity: Order): Unit = ???
  def load(id: Int): Order = ???
}

// Type-safe: can't mix users and orders
val user: UserDatabase.Entity = UserDatabase.load(1)
// val order: OrderDatabase.Entity = UserDatabase.load(1)  // Type error!
```

### 5. Use Self-Types for Dependencies

```scala
// ✅ GOOD: Self-types for dependency declaration
trait UserRepository {
  def findUser(id: Int): Option[User]
}

trait EmailService {
  def sendEmail(email: String, body: String): Unit
}

trait UserService {
  self: UserRepository with EmailService =>
  
  def notifyUser(id: Int, message: String): Unit = {
    findUser(id).foreach { user =>
      sendEmail(user.email, message)
    }
  }
}

// Implementation must mix in dependencies
class UserServiceImpl 
  extends UserService 
  with UserRepository 
  with EmailService {
  
  override def findUser(id: Int): Option[User] = ???
  override def sendEmail(email: String, body: String): Unit = ???
}
```

---

## Code Organization

### 1. Package Objects for Shared Definitions

```scala
// com/company/project/package.scala
package com.company

package object project {
  // Type aliases available throughout package
  type UserId = Int
  type Result[A] = Either[Error, A]
  
  // Common implicits
  implicit class StringOps(s: String) {
    def toUserId: UserId = s.toInt
  }
  
  // Constants
  val MaxRetries = 3
  val Timeout = 5000
}

// Usage in any file in com.company.project
package com.company.project

class UserService {
  def getUser(id: UserId): Result[User] = ???
}
```

### 2. Companion Objects for Factory Methods

```scala
// ✅ GOOD: Companion object pattern
case class User private (id: Int, name: String, email: String)

object User {
  def create(name: String, email: String): Either[String, User] = {
    for {
      validName <- validateName(name)
      validEmail <- validateEmail(email)
      id <- generateId()
    } yield User(id, validName, validEmail)
  }
  
  def fromDatabase(id: Int, name: String, email: String): User = {
    User(id, name, email)
  }
  
  private def validateName(name: String): Either[String, String] = ???
  private def validateEmail(email: String): Either[String, String] = ???
  private def generateId(): Either[String, Int] = ???
}
```

### 3. Organize Implicits

```scala
// ✅ GOOD: Separate implicit definitions
object Implicits {
  // Type class instances
  object instances {
    implicit val userJsonEncoder: JsonEncoder[User] = ???
    implicit val orderJsonEncoder: JsonEncoder[Order] = ???
  }
  
  // Extension methods
  object syntax {
    implicit class UserOps(user: User) {
      def fullName: String = s"${user.firstName} ${user.lastName}"
    }
  }
}

// Controlled imports
import Implicits.instances._
import Implicits.syntax._
```

---

## Testing Best Practices

### 1. Use Property-Based Testing

```scala
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object MathSpecification extends Properties("Math") {
  property("addition is commutative") = forAll { (a: Int, b: Int) =>
    a + b == b + a
  }
  
  property("addition is associative") = forAll { (a: Int, b: Int, c: Int) =>
    (a + b) + c == a + (b + c)
  }
  
  property("zero is identity") = forAll { (a: Int) =>
    a + 0 == a && 0 + a == a
  }
}
```

### 2. Test Behavior, Not Implementation

```scala
// ❌ BAD: Testing implementation details
it should "call repository.save" in {
  val repository = mock[UserRepository]
  val service = new UserService(repository)
  
  service.createUser(validUser)
  
  verify(repository).save(any[User])  // Implementation detail
}

// ✅ GOOD: Testing behavior
it should "create user successfully" in {
  val service = new UserService(repository)
  
  val result = service.createUser(validUser)
  
  result shouldBe a[Success[_]]
  result.get.name shouldBe validUser.name
}
```

### 3. Use Test Fixtures

```scala
trait TestFixtures {
  val validUser = User(1, "John", "john@example.com")
  val invalidUser = User(-1, "", "invalid")
  
  val testConfig = Config(
    database = "test-db",
    timeout = 1000
  )
  
  def createMockRepository(): UserRepository = ???
}

class UserServiceSpec extends AnyFlatSpec with Matchers with TestFixtures {
  it should "validate users" in {
    val service = new UserService(createMockRepository())
    
    service.validate(validUser) shouldBe true
    service.validate(invalidUser) shouldBe false
  }
}
```

---

## Common Gotchas

### 1. Type Inference Limitations

```scala
// ⚠️ GOTCHA: Type inference needs help
val list = List()  // List[Nothing]
list :+ 1  // Compile error!

// ✅ FIX: Provide type
val list: List[Int] = List()
val list = List.empty[Int]
val list = List[Int]()
```

### 2. Null from Java Interop

```scala
// ⚠️ GOTCHA: Java methods can return null
val javaString: String = someJavaMethod()  // Might be null!
println(javaString.length)  // NPE!

// ✅ FIX: Wrap in Option
val safeString: Option[String] = Option(someJavaMethod())
safeString.foreach(s => println(s.length))
```

### 3. Semicolon Inference

```scala
// ⚠️ GOTCHA: Unexpected semicolon insertion
val x = 1
+ 2  // Two statements: val x = 1; +2

// ✅ FIX: Continuation on same line or use parentheses
val x = 1 + 2

val x = 1 +
  2  // OK: operator at end of line

val x = (1
  + 2)  // OK: in parentheses
```

### 4. Implicit Conversion Ambiguity

```scala
// ⚠️ GOTCHA: Multiple implicit conversions
implicit def intToString(i: Int): String = i.toString
implicit def intToDouble(i: Int): Double = i.toDouble

val x: Any = 42  // Ambiguous!

// ✅ FIX: Be explicit
val x: String = 42  // Uses intToString
val x: Double = 42  // Uses intToDouble
```

### 5. Lazy Val Initialization

```scala
// ⚠️ GOTCHA: Lazy val not thread-safe in all cases
lazy val expensive = {
  Thread.sleep(1000)
  42
}

// Multiple threads might initialize it multiple times!

// ✅ FIX: Use @volatile or synchronization if needed
@volatile lazy val expensive = computeValue()
```

### 6. Default Arguments in Overridden Methods

```scala
// ⚠️ GOTCHA: Default arguments not inherited
class Base {
  def method(x: Int = 1): Unit = println(x)
}

class Derived extends Base {
  override def method(x: Int): Unit = println(x * 2)
}

val d: Base = new Derived()
d.method()  // Prints 2, not 1!

// ✅ FIX: Redefine defaults in override
class Derived extends Base {
  override def method(x: Int = 1): Unit = println(x * 2)
}
```

---

## Quick Reference

### Collection Operations Cheat Sheet

```scala
// Transformation
list.map(_ * 2)                   // [1,2,3] => [2,4,6]
list.flatMap(x => List(x, x*2))   // [1,2] => [1,2,2,4]
list.collect { case x if x > 2 => x * 2 }  // Partial function

// Filtering
list.filter(_ > 2)                // [1,2,3,4] => [3,4]
list.filterNot(_ > 2)             // [1,2,3,4] => [1,2]
list.takeWhile(_ < 3)             // [1,2,3,4] => [1,2]
list.dropWhile(_ < 3)             // [1,2,3,4] => [3,4]

// Aggregation
list.reduce(_ + _)                // [1,2,3] => 6
list.fold(0)(_ + _)               // [1,2,3] => 6
list.foldLeft(0)(_ + _)           // Left associative
list.foldRight(0)(_ + _)          // Right associative

// Searching
list.find(_ > 2)                  // [1,2,3] => Some(3)
list.exists(_ > 2)                // [1,2,3] => true
list.forall(_ > 0)                // [1,2,3] => true

// Grouping
list.groupBy(_ % 2)               // Map(0 -> [2,4], 1 -> [1,3])
list.partition(_ % 2 == 0)        // ([2,4], [1,3])

// Sorting
list.sorted                       // [3,1,2] => [1,2,3]
list.sortBy(-_)                   // [1,2,3] => [3,2,1]
list.sortWith(_ > _)              // [1,2,3] => [3,2,1]
```

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-10-11 | Initial Scala best practices guide |

---

## References

- [Scala Documentation](https://docs.scala-lang.org/)
- [Scala Style Guide](https://docs.scala-lang.org/style/)
- [Effective Scala](https://twitter.github.io/effectivescala/)
- [Functional Programming in Scala](https://www.manning.com/books/functional-programming-in-scala)
- [Cats Documentation](https://typelevel.org/cats/)

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

