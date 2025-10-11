# AI Integration Manual - Scala

**Version:** 1.0.0  
**Last Updated:** 2025-10-11  
**Target Audience:** AI Agents (LLM-based development assistants)  
**Language:** Scala (2.13.x / 3.x)

---

## Table of Contents

1. [Introduction](#introduction)
2. [Scala Environment Setup](#scala-environment-setup)
3. [Project Structure](#project-structure)
4. [Build Tool (SBT)](#build-tool-sbt)
5. [Testing Framework](#testing-framework)
6. [Code Style & Formatting](#code-style--formatting)
7. [Documentation (ScalaDoc)](#documentation-scaladoc)
8. [Popular Frameworks](#popular-frameworks)
9. [Implementation Guidelines](#implementation-guidelines)
10. [Error Handling](#error-handling)
11. [Concurrency & Futures](#concurrency--futures)
12. [Performance Optimization](#performance-optimization)
13. [Security Best Practices](#security-best-practices)
14. [Continuous Integration](#continuous-integration)
15. [Library Publishing](#library-publishing)

---

## Introduction

This manual extends the base AI Integration Manual Template with Scala-specific practices, tools, and workflows. It covers both Scala 2.13.x and Scala 3.x, focusing on functional programming, type safety, and JVM interoperability.

### Core Principles for Scala

1. **Functional First**: Prefer immutability and pure functions
2. **Type Safety**: Leverage Scala's powerful type system
3. **Expression-Oriented**: Everything is an expression that returns a value
4. **Composability**: Build complex behavior from simple, composable pieces
5. **Conciseness**: Write expressive, concise code without sacrificing clarity
6. **JVM Interoperability**: Seamlessly integrate with Java libraries
7. **Multi-Paradigm**: Combine FP and OOP when appropriate

---

## Scala Environment Setup

### Required Tools

```bash
# Install SDKMAN (recommended for managing JDK and Scala)
curl -s "https://get.sdkman.io" | bash
source "$HOME/.sdkman/bin/sdkman-init.sh"

# Install Java (required for Scala)
sdk install java 17.0.9-tem
sdk default java 17.0.9-tem

# Install Scala
sdk install scala 2.13.12
# Or Scala 3
sdk install scala 3.3.1

# Install SBT (Scala Build Tool)
sdk install sbt 1.9.7

# Alternative: Coursier (Fast Scala artifact fetcher)
curl -fL https://github.com/coursier/launchers/raw/master/cs-x86_64-pc-linux.gz | gzip -d > cs
chmod +x cs
./cs setup

# Verify installations
java -version
scala -version
sbt --version
```

### IDE Setup

```bash
# IntelliJ IDEA (recommended)
# Download from https://www.jetbrains.com/idea/
# Install Scala plugin

# VS Code with Metals
# Install VS Code
# Install Scala (Metals) extension

# Vim/Neovim with Metals
# Install metals-vim or coc-metals
```

### Version Requirements

- **Minimum Scala Version**: 2.13.0 or 3.0.0
- **Recommended**: Scala 2.13.12 or Scala 3.3.1
- **JDK**: 11+ (JDK 17 or 21 recommended)
- **SBT**: 1.9+

### Environment Configuration

```bash
# .scala-build.sbt for Scala 3
scalaVersion := "3.3.1"

# .scalaversion (for coursier/metals)
echo "3.3.1" > .scalaversion

# Set JVM options
echo '-Xmx2G -Xss2M' > .jvmopts

# SBT global settings
mkdir -p ~/.sbt/1.0/
cat > ~/.sbt/1.0/global.sbt << EOF
ThisBuild / useCoursier := true
ThisBuild / csrCacheDirectory := file(System.getProperty("user.home")) / ".coursier" / "cache"
EOF
```

---

## Project Structure

### Standard Scala Project Layout

```
project-root/
├── .github/
│   └── workflows/
│       ├── ci.yml
│       └── release.yml
├── .gitignore
├── .scalafmt.conf
├── .scalafix.conf
├── .scalaversion
├── build.sbt
├── project/
│   ├── build.properties
│   ├── plugins.sbt
│   └── Dependencies.scala
├── README.md
├── CHANGELOG.md
├── LICENSE
├── docs/
│   ├── ROADMAP.md
│   ├── SPECS.md
│   └── specs/
├── src/
│   ├── main/
│   │   ├── scala/
│   │   │   └── com/
│   │   │       └── company/
│   │   │           └── project/
│   │   │               ├── Main.scala
│   │   │               ├── core/
│   │   │               │   ├── Domain.scala
│   │   │               │   └── Service.scala
│   │   │               └── utils/
│   │   │                   └── Helpers.scala
│   │   ├── resources/
│   │   │   └── application.conf
│   │   └── java/           # Java sources (if any)
│   └── test/
│       ├── scala/
│       │   └── com/
│       │       └── company/
│       │           └── project/
│       │               ├── core/
│       │               │   ├── DomainSpec.scala
│       │               │   └── ServiceSpec.scala
│       │               └── utils/
│       │                   └── HelpersSpec.scala
│       └── resources/
│           └── test.conf
├── target/                 # Build artifacts (gitignored)
└── lib/                    # Unmanaged dependencies (optional)
```

### Multi-Module Project

```
project-root/
├── build.sbt
├── project/
├── core/
│   ├── src/
│   │   ├── main/scala/
│   │   └── test/scala/
├── api/
│   ├── src/
│   │   ├── main/scala/
│   │   └── test/scala/
└── integration/
    ├── src/
    │   ├── main/scala/
    │   └── test/scala/
```

---

## Build Tool (SBT)

### build.sbt

```scala
// Project information
name := "project-name"
organization := "com.company"
version := "1.0.0"

// Scala version
scalaVersion := "2.13.12"
// Or for Scala 3
// scalaVersion := "3.3.1"

// Cross-build versions
crossScalaVersions := Seq("2.13.12", "3.3.1")

// Compiler options
scalacOptions ++= Seq(
  "-encoding", "UTF-8",
  "-feature",
  "-deprecation",
  "-unchecked",
  "-language:postfixOps",
  "-language:implicitConversions",
  "-language:higherKinds",
  "-Xlint",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard"
)

// Scala 3 specific options
scalacOptions ++= {
  if (scalaVersion.value.startsWith("3.")) {
    Seq(
      "-explain",
      "-explain-types",
      "-source:future",
      "-Ykind-projector"
    )
  } else Seq.empty
}

// Dependencies
libraryDependencies ++= Seq(
  // Core
  "org.typelevel" %% "cats-core" % "2.10.0",
  "org.typelevel" %% "cats-effect" % "3.5.2",
  
  // Configuration
  "com.typesafe" % "config" % "1.4.3",
  
  // Logging
  "ch.qos.logback" % "logback-classic" % "1.4.14",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
  
  // Testing
  "org.scalatest" %% "scalatest" % "3.2.17" % Test,
  "org.scalatestplus" %% "scalacheck-1-17" % "3.2.17.0" % Test,
  "org.scalacheck" %% "scalacheck" % "1.17.0" % Test,
  "org.scalamock" %% "scalamock" % "5.2.0" % Test
)

// Test settings
Test / parallelExecution := false
Test / fork := true
Test / testOptions += Tests.Argument("-oDF")

// Coverage settings
coverageMinimumStmtTotal := 90
coverageFailOnMinimum := true
coverageExcludedPackages := "<empty>;.*\\.model\\..*;.*\\.config\\..*"

// Formatting
scalafmtOnCompile := true

// Assembly settings (for fat JAR)
assembly / assemblyJarName := s"${name.value}-${version.value}.jar"
assembly / mainClass := Some("com.company.project.Main")
assembly / assemblyMergeStrategy := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
}

// Publishing settings
publishTo := {
  val nexus = "https://nexus.company.com/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "snapshots")
  else
    Some("releases" at nexus + "releases")
}

credentials += Credentials(Path.userHome / ".sbt" / ".credentials")

// Metadata
licenses := Seq("MIT" -> url("https://opensource.org/licenses/MIT"))
homepage := Some(url("https://github.com/company/project"))
scmInfo := Some(
  ScmInfo(
    url("https://github.com/company/project"),
    "scm:git@github.com:company/project.git"
  )
)
developers := List(
  Developer(
    id = "developer",
    name = "Developer Name",
    email = "dev@company.com",
    url = url("https://company.com")
  )
)
```

### project/plugins.sbt

```scala
// Code formatting
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.2")

// Linting
addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.11.1")

// Coverage
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.0.9")

// Assembly (fat JAR)
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.1.5")

// Publishing
addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.2.1")
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.10.0")

// Release
addSbtPlugin("com.github.sbt" % "sbt-release" % "1.1.0")

// Dependencies
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.6.4")
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.10.0-RC1")
```

### project/build.properties

```properties
sbt.version=1.9.7
```

### project/Dependencies.scala

```scala
import sbt._

object Dependencies {
  object Versions {
    val cats = "2.10.0"
    val catsEffect = "3.5.2"
    val scalaTest = "3.2.17"
    val logback = "1.4.14"
  }

  val cats = Seq(
    "org.typelevel" %% "cats-core" % Versions.cats,
    "org.typelevel" %% "cats-effect" % Versions.catsEffect
  )

  val logging = Seq(
    "ch.qos.logback" % "logback-classic" % Versions.logback,
    "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5"
  )

  val testing = Seq(
    "org.scalatest" %% "scalatest" % Versions.scalaTest % Test,
    "org.scalacheck" %% "scalacheck" % "1.17.0" % Test
  )

  val all: Seq[ModuleID] = cats ++ logging ++ testing
}

// In build.sbt:
// libraryDependencies ++= Dependencies.all
```

### SBT Commands

```bash
# Compile
sbt compile

# Run tests
sbt test

# Run specific test
sbt "testOnly *ServiceSpec"

# Run with coverage
sbt clean coverage test coverageReport

# Format code
sbt scalafmt

# Check formatting
sbt scalafmtCheck

# Run scalafix
sbt scalafix

# Build fat JAR
sbt assembly

# Run application
sbt run

# Continuous compilation
sbt ~compile

# Interactive mode
sbt
> compile
> test
> exit

# Clean build
sbt clean

# Dependency tree
sbt dependencyTree

# Check for updates
sbt dependencyUpdates

# Publish local
sbt publishLocal

# Publish
sbt publish
```

---

## Testing Framework

### ScalaTest Setup

**src/test/scala/com/company/project/core/ServiceSpec.scala**:
```scala
package com.company.project.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

class ServiceSpec extends AnyFlatSpec with Matchers with BeforeAndAfterEach {

  behavior of "Service"

  it should "create instance with default values" in {
    val service = new Service()
    
    service shouldBe a[Service]
    service.isActive shouldBe true
  }

  it should "process valid input correctly" in {
    val service = new Service()
    val input = "test"
    
    val result = service.process(input)
    
    result shouldBe "TEST"
  }

  it should "throw exception for invalid input" in {
    val service = new Service()
    
    an[IllegalArgumentException] should be thrownBy {
      service.process(null)
    }
  }

  it should "handle empty input" in {
    val service = new Service()
    
    val result = service.process("")
    
    result shouldBe empty
  }

  override def beforeEach(): Unit = {
    // Setup before each test
  }

  override def afterEach(): Unit = {
    // Cleanup after each test
  }
}
```

### Property-Based Testing with ScalaCheck

```scala
package com.company.project.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class MathSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {

  behavior of "Math operations"

  it should "satisfy commutativity for addition" in {
    forAll { (a: Int, b: Int) =>
      whenever(a != Int.MaxValue && b != Int.MaxValue) {
        Math.add(a, b) shouldBe Math.add(b, a)
      }
    }
  }

  it should "satisfy associativity for addition" in {
    forAll { (a: Int, b: Int, c: Int) =>
      whenever(a != Int.MaxValue && b != Int.MaxValue && c != Int.MaxValue) {
        Math.add(Math.add(a, b), c) shouldBe Math.add(a, Math.add(b, c))
      }
    }
  }

  it should "have zero as identity element" in {
    forAll { (a: Int) =>
      Math.add(a, 0) shouldBe a
      Math.add(0, a) shouldBe a
    }
  }
}
```

### Async Testing

```scala
package com.company.project.core

import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.concurrent.Future

class AsyncServiceSpec extends AsyncFlatSpec with Matchers {

  behavior of "AsyncService"

  it should "fetch data asynchronously" in {
    val service = new AsyncService()
    
    val futureResult = service.fetchData("key")
    
    futureResult.map { result =>
      result shouldBe "value"
    }
  }

  it should "handle errors gracefully" in {
    val service = new AsyncService()
    
    val futureResult = service.fetchData("invalid")
    
    recoverToSucceededIf[NoSuchElementException] {
      futureResult
    }
  }
}
```

### Test Fixtures

```scala
package com.company.project

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

trait TestFixtures {
  val validUser = User("john", "john@example.com", 30)
  val invalidUser = User("", "", -1)
  
  val testConfig = Config(
    host = "localhost",
    port = 8080,
    timeout = 5000
  )
  
  def createMockDatabase(): Database = {
    new InMemoryDatabase()
  }
}

class UserServiceSpec extends AnyFlatSpec with Matchers with TestFixtures {
  
  behavior of "UserService"
  
  it should "validate user correctly" in {
    val service = new UserService(createMockDatabase())
    
    service.validate(validUser) shouldBe true
    service.validate(invalidUser) shouldBe false
  }
}
```

---

## Code Style & Formatting

### Scalafmt Configuration

**.scalafmt.conf**:
```conf
version = "3.7.17"

runner.dialect = scala213
# For Scala 3:
# runner.dialect = scala3

maxColumn = 120
indent.defnSite = 2
indent.callSite = 2
indent.ctorSite = 2

align.preset = more
align.openParenCallSite = false
align.openParenDefnSite = false

continuationIndent.defnSite = 2
continuationIndent.callSite = 2
continuationIndent.extendSite = 2

danglingParentheses.preset = true

newlines.source = keep
newlines.topLevelStatementBlankLines = [
  {
    blanks = 1
  }
]

rewrite.rules = [
  PreferCurlyFors,
  RedundantBraces,
  RedundantParens,
  SortImports,
  SortModifiers
]

rewrite.sortModifiers.order = [
  "override", "private", "protected", "implicit", "final", "sealed", "abstract", "lazy"
]

spaces.inImportCurlyBraces = false

includeNoParensInSelectChains = false

optIn.breakChainOnFirstMethodDot = true
optIn.configStyleArguments = true

trailingCommas = multiple

project.excludeFilters = [
  "target/"
]
```

### Scalafix Configuration

**.scalafix.conf**:
```conf
rules = [
  DisableSyntax,
  LeakingImplicitClassVal,
  NoAutoTupling,
  NoValInForComprehension,
  OrganizeImports,
  ProcedureSyntax,
  RemoveUnused
]

DisableSyntax.noVars = true
DisableSyntax.noThrows = true
DisableSyntax.noNulls = true
DisableSyntax.noReturns = true
DisableSyntax.noWhileLoops = true
DisableSyntax.noAsInstanceOf = false
DisableSyntax.noIsInstanceOf = false
DisableSyntax.noXml = true
DisableSyntax.noFinalVal = true
DisableSyntax.noFinalize = true

OrganizeImports.groups = [
  "re:javax?\\.",
  "scala.",
  "*",
  "com.company.project."
]

OrganizeImports.removeUnused = true
OrganizeImports.groupedImports = Merge
```

### Running Formatters

```bash
# Format all files
sbt scalafmt

# Format test files
sbt test:scalafmt

# Check formatting without modifying
sbt scalafmtCheck

# Run scalafix
sbt scalafix

# Check scalafix rules without modifying
sbt "scalafix --check"
```

---

## Documentation (ScalaDoc)

### ScalaDoc Standards

```scala
package com.company.project.core

/**
 * Core service for processing data.
 *
 * This service provides the main business logic for data processing,
 * including validation, transformation, and persistence.
 *
 * @example {{{
 * val service = new Service(config)
 * val result = service.process("input")
 * println(result)
 * }}}
 *
 * @constructor Create a new service with the given configuration.
 * @param config the configuration for this service
 * @param logger implicit logger for logging operations
 *
 * @author Your Name
 * @version 1.0.0
 * @since 1.0.0
 */
class Service(config: Config)(implicit logger: Logger) {

  /**
   * Process the given input.
   *
   * This method validates the input, applies transformations,
   * and returns the processed result.
   *
   * @param input the input string to process
   * @return the processed result
   * @throws IllegalArgumentException if input is null or empty
   *
   * @example {{{
   * val result = service.process("test")
   * assert(result == "TEST")
   * }}}
   *
   * @see [[validate]] for input validation
   * @note This method is thread-safe
   */
  def process(input: String): String = {
    require(input != null && input.nonEmpty, "Input must not be null or empty")
    
    logger.debug(s"Processing input: $input")
    
    val validated = validate(input)
    transform(validated)
  }

  /**
   * Validate the input.
   *
   * @param input the input to validate
   * @return the validated input
   * @throws IllegalArgumentException if validation fails
   */
  private def validate(input: String): String = {
    // Validation logic
    input
  }

  /**
   * Transform the validated input.
   *
   * @param input the validated input
   * @return the transformed result
   */
  private def transform(input: String): String = {
    input.toUpperCase
  }
}

/**
 * Companion object for [[Service]].
 *
 * Provides factory methods and utilities for creating Service instances.
 */
object Service {
  
  /**
   * Create a service with default configuration.
   *
   * @param logger implicit logger
   * @return a new Service instance
   */
  def apply()(implicit logger: Logger): Service = {
    new Service(Config.default)
  }

  /**
   * Default timeout in milliseconds.
   */
  val DefaultTimeout: Int = 5000
}
```

### Generating ScalaDoc

**build.sbt**:
```scala
// ScalaDoc settings
Compile / doc / scalacOptions ++= Seq(
  "-groups",
  "-implicits",
  "-diagrams",
  "-doc-title", name.value,
  "-doc-version", version.value,
  "-doc-root-content", baseDirectory.value + "/rootdoc.txt"
)

// Separate task for generating docs
lazy val generateDocs = taskKey[Unit]("Generate ScalaDoc")
generateDocs := {
  (Compile / doc).value
  println(s"Documentation generated in ${(Compile / doc / target).value}")
}
```

```bash
# Generate ScalaDoc
sbt doc

# Generate and open
sbt doc && open target/scala-2.13/api/index.html
```

---

## Popular Frameworks

### Akka HTTP (Web Services)

**build.sbt**:
```scala
val akkaVersion = "2.8.5"
val akkaHttpVersion = "10.5.3"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion,
  "com.typesafe.akka" %% "akka-stream" % akkaVersion,
  "com.typesafe.akka" %% "akka-http" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpVersion
)
```

**Example**:
```scala
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import spray.json.DefaultJsonProtocol._

object WebServer {
  implicit val system = ActorSystem(Behaviors.empty, "my-system")
  implicit val executionContext = system.executionContext

  case class User(name: String, email: String)
  implicit val userFormat = jsonFormat2(User)

  val route =
    path("users") {
      get {
        complete(List(User("John", "john@example.com")))
      }
    } ~
    path("users" / IntNumber) { id =>
      get {
        complete(s"User $id")
      }
    }

  def main(args: Array[String]): Unit = {
    Http().newServerAt("localhost", 8080).bind(route)
    println("Server started at http://localhost:8080")
  }
}
```

### Cats/Cats Effect (Functional Programming)

```scala
import cats._
import cats.effect._
import cats.implicits._

object CatsExample extends IOApp {

  def fetchUser(id: Int): IO[User] = IO {
    // Simulated database call
    Thread.sleep(100)
    User(s"User$id", s"user$id@example.com")
  }

  def fetchOrders(userId: Int): IO[List[Order]] = IO {
    // Simulated API call
    Thread.sleep(100)
    List(Order(1, userId, 100.0), Order(2, userId, 200.0))
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val program = for {
      user <- fetchUser(1)
      orders <- fetchOrders(user.id)
      _ <- IO.println(s"User: ${user.name}")
      _ <- IO.println(s"Orders: ${orders.size}")
    } yield ()

    program.as(ExitCode.Success)
  }
}
```

### Play Framework (Full-Stack Web)

**build.sbt**:
```scala
libraryDependencies ++= Seq(
  guice,
  "com.typesafe.play" %% "play-json" % "2.10.3",
  "org.scalatestplus.play" %% "scalatestplus-play" % "6.0.0" % Test
)
```

### ZIO (Effect System)

```scala
import zio._

object ZIOExample extends ZIOAppDefault {

  case class User(id: Int, name: String)

  def fetchUser(id: Int): Task[User] = ZIO.attempt {
    // Simulated database call
    User(id, s"User$id")
  }

  def processUser(user: User): UIO[Unit] = ZIO.succeed {
    println(s"Processing user: ${user.name}")
  }

  override def run = for {
    user <- fetchUser(1)
    _ <- processUser(user)
  } yield ()
}
```

---

## Implementation Guidelines

### Immutability and Case Classes

```scala
// ✅ GOOD: Immutable case class
case class User(
  id: Int,
  name: String,
  email: String,
  age: Int
)

// Copying with modifications
val user = User(1, "John", "john@example.com", 30)
val updatedUser = user.copy(age = 31)

// ✅ GOOD: Immutable collections
val numbers = List(1, 2, 3)
val doubled = numbers.map(_ * 2)  // Returns new list

// ❌ BAD: Mutable collections
import scala.collection.mutable
val mutableList = mutable.ListBuffer(1, 2, 3)
mutableList += 4  // Mutation!
```

### Pattern Matching

```scala
// ✅ GOOD: Exhaustive pattern matching
sealed trait Result
case class Success(value: String) extends Result
case class Failure(error: String) extends Result
case object Pending extends Result

def handleResult(result: Result): String = result match {
  case Success(value) => s"Got: $value"
  case Failure(error) => s"Error: $error"
  case Pending => "Still pending"
}

// ✅ GOOD: Extractors
val email = "john@example.com"
val EmailPattern = "(.+)@(.+)".r

email match {
  case EmailPattern(user, domain) =>
    println(s"User: $user, Domain: $domain")
  case _ =>
    println("Invalid email")
}

// ✅ GOOD: Guards
def classify(n: Int): String = n match {
  case x if x < 0 => "negative"
  case 0 => "zero"
  case x if x > 0 => "positive"
}
```

### For-Comprehensions

```scala
// ✅ GOOD: For-comprehension for sequential operations
def fetchUserData(userId: Int): Future[UserData] = for {
  user <- userService.fetchUser(userId)
  profile <- profileService.fetchProfile(user.profileId)
  orders <- orderService.fetchOrders(userId)
} yield UserData(user, profile, orders)

// ✅ GOOD: With filtering
def processEvenNumbers(numbers: List[Int]): List[Int] = for {
  n <- numbers
  if n % 2 == 0
  doubled = n * 2
  if doubled > 10
} yield doubled

// Equivalent to:
numbers
  .filter(_ % 2 == 0)
  .map(_ * 2)
  .filter(_ > 10)
```

### Implicits and Type Classes

```scala
// ✅ GOOD: Type class pattern
trait JsonEncoder[A] {
  def encode(value: A): String
}

object JsonEncoder {
  implicit val stringEncoder: JsonEncoder[String] =
    (value: String) => s""""$value""""

  implicit val intEncoder: JsonEncoder[Int] =
    (value: Int) => value.toString

  implicit def listEncoder[A](implicit encoder: JsonEncoder[A]): JsonEncoder[List[A]] =
    (values: List[A]) => values.map(encoder.encode).mkString("[", ",", "]")
}

def toJson[A](value: A)(implicit encoder: JsonEncoder[A]): String =
  encoder.encode(value)

// Usage
toJson("Hello")  // "Hello"
toJson(42)  // 42
toJson(List(1, 2, 3))  // [1,2,3]
```

---

## Error Handling

### Option and Either

```scala
// ✅ GOOD: Option for nullable values
def findUser(id: Int): Option[User] = {
  if (id > 0) Some(User(id, "John"))
  else None
}

// Usage
findUser(1) match {
  case Some(user) => println(user.name)
  case None => println("User not found")
}

// Or with map
findUser(1).map(_.name).getOrElse("Unknown")

// ✅ GOOD: Either for error handling
def divide(a: Int, b: Int): Either[String, Double] = {
  if (b == 0) Left("Division by zero")
  else Right(a.toDouble / b)
}

// Usage
divide(10, 2) match {
  case Right(result) => println(s"Result: $result")
  case Left(error) => println(s"Error: $error")
}

// Or with for-comprehension
val result = for {
  x <- divide(10, 2)
  y <- divide(20, x.toInt)
} yield y
```

### Try for Exception Handling

```scala
import scala.util.{Try, Success, Failure}

// ✅ GOOD: Try for catching exceptions
def parseint(s: String): Try[Int] = Try(s.toInt)

parseInt("42") match {
  case Success(value) => println(s"Parsed: $value")
  case Failure(exception) => println(s"Failed: ${exception.getMessage}")
}

// ✅ GOOD: Recovering from failures
val result = parseInt("abc").recover {
  case _: NumberFormatException => 0
}
```

### Custom Error Types

```scala
// ✅ GOOD: ADT for errors
sealed trait AppError
case class ValidationError(field: String, message: String) extends AppError
case class DatabaseError(cause: Throwable) extends AppError
case class NotFoundError(resource: String, id: String) extends AppError

def createUser(email: String): Either[AppError, User] = {
  if (!email.contains("@")) {
    Left(ValidationError("email", "Invalid email format"))
  } else {
    Right(User(1, email))
  }
}
```

---

## Concurrency & Futures

### Future Basics

```scala
import scala.concurrent.{Future, ExecutionContext}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Success, Failure}

// ✅ GOOD: Async operations with Future
def fetchData(url: String): Future[String] = Future {
  // Simulated HTTP call
  Thread.sleep(1000)
  s"Data from $url"
}

// Handling results
fetchData("https://api.example.com").onComplete {
  case Success(data) => println(s"Got: $data")
  case Failure(exception) => println(s"Failed: ${exception.getMessage}")
}

// ✅ GOOD: Composing Futures
val result: Future[Int] = for {
  data1 <- fetchData("url1")
  data2 <- fetchData("url2")
} yield data1.length + data2.length

// ✅ GOOD: Parallel execution
val future1 = fetchData("url1")
val future2 = fetchData("url2")
val combined = for {
  result1 <- future1
  result2 <- future2
} yield (result1, result2)
```

### Cats Effect IO

```scala
import cats.effect._
import cats.implicits._

// ✅ GOOD: Pure functional effects
def fetchUser(id: Int): IO[User] = IO {
  // Side effect wrapped in IO
  User(id, "John")
}

def processUser(user: User): IO[Unit] = IO {
  println(s"Processing ${user.name}")
}

val program: IO[Unit] = for {
  user <- fetchUser(1)
  _ <- processUser(user)
} yield ()

// Run the program
// program.unsafeRunSync() // In production, use IOApp
```

---

## Performance Optimization

### Lazy Evaluation

```scala
// ✅ GOOD: Lazy val for expensive computations
class DataProcessor {
  lazy val expensiveResource: Resource = {
    println("Initializing resource...")
    new Resource()
  }
  
  def process(): Unit = {
    // Resource only initialized when first accessed
    expensiveResource.use()
  }
}

// ✅ GOOD: Lazy collections
val numbers = (1 to 1000000).view
  .filter(_ % 2 == 0)
  .map(_ * 2)
  .take(10)
  .toList  // Only processes 10 elements
```

### Tail Recursion

```scala
import scala.annotation.tailrec

// ✅ GOOD: Tail recursive
@tailrec
def factorial(n: Int, acc: BigInt = 1): BigInt = {
  if (n <= 1) acc
  else factorial(n - 1, n * acc)
}

// ❌ BAD: Not tail recursive (stack overflow for large n)
def factorialBad(n: Int): BigInt = {
  if (n <= 1) 1
  else n * factorialBad(n - 1)
}
```

### Parallel Collections

```scala
// ✅ GOOD: Parallel processing for large collections
val numbers = (1 to 1000000).toList

val result = numbers.par
  .filter(_ % 2 == 0)
  .map(expensiveComputation)
  .sum
```

---

## Security Best Practices

### Input Validation

```scala
// ✅ GOOD: Validation with Either
def validateEmail(email: String): Either[ValidationError, String] = {
  val emailRegex = """^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$""".r
  
  email match {
    case emailRegex() => Right(email)
    case _ => Left(ValidationError("email", "Invalid email format"))
  }
}

// ✅ GOOD: Using Cats Validated
import cats.data.Validated
import cats.data.Validated.{Valid, Invalid}
import cats.implicits._

case class UserInput(name: String, email: String, age: Int)

def validateName(name: String): Validated[List[String], String] =
  if (name.nonEmpty) Valid(name)
  else Invalid(List("Name cannot be empty"))

def validateEmail(email: String): Validated[List[String], String] =
  if (email.contains("@")) Valid(email)
  else Invalid(List("Invalid email"))

def validateAge(age: Int): Validated[List[String], Int] =
  if (age >= 0 && age <= 150) Valid(age)
  else Invalid(List("Age must be between 0 and 150"))

def validateUserInput(input: UserInput): Validated[List[String], UserInput] =
  (validateName(input.name),
   validateEmail(input.email),
   validateAge(input.age)).mapN(UserInput)
```

### SQL Injection Prevention

```scala
// Using Slick (type-safe queries)
import slick.jdbc.PostgresProfile.api._

def findUser(email: String): Future[Option[User]] = {
  val query = users.filter(_.email === email).result.headOption
  db.run(query)  // Parameters are safely escaped
}

// Using Doobie (functional SQL)
import doobie._
import doobie.implicits._

def findUser(email: String): ConnectionIO[Option[User]] =
  sql"SELECT id, name, email FROM users WHERE email = $email"
    .query[User]
    .option
```

---

## Continuous Integration

### GitHub Actions Workflow

**.github/workflows/ci.yml**:
```yaml
name: Scala CI

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main, develop ]

jobs:
  test:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        scala: ['2.13.12', '3.3.1']
        java: ['11', '17', '21']

    steps:
      - uses: actions/checkout@v3

      - name: Setup JDK
        uses: actions/setup-java@v3
        with:
          distribution: 'temurin'
          java-version: ${{ matrix.java }}
          cache: 'sbt'

      - name: Setup Scala
        uses: olafurpg/setup-scala@v14
        with:
          java-version: ${{ matrix.java }}

      - name: Check formatting
        run: sbt scalafmtCheckAll

      - name: Compile
        run: sbt ++${{ matrix.scala }} compile

      - name: Run tests
        run: sbt ++${{ matrix.scala }} coverage test coverageReport

      - name: Upload coverage
        uses: codecov/codecov-action@v3
        if: matrix.scala == '2.13.12' && matrix.java == '17'
        with:
          files: ./target/scala-2.13/coverage-report/cobertura.xml

      - name: Run scalafix
        run: sbt "scalafix --check"

  publish:
    needs: test
    if: github.ref == 'refs/heads/main'
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v3

      - name: Setup JDK
        uses: actions/setup-java@v3
        with:
          distribution: 'temurin'
          java-version: '17'

      - name: Publish
        run: sbt publish
        env:
          SONATYPE_USERNAME: ${{ secrets.SONATYPE_USERNAME }}
          SONATYPE_PASSWORD: ${{ secrets.SONATYPE_PASSWORD }}
```

---

## Library Publishing

### Publishing to Maven Central

**build.sbt**:
```scala
// Publishing settings
publishMavenStyle := true
publishTo := sonatypePublishToBundle.value

pomIncludeRepository := { _ => false }

licenses := Seq("MIT" -> url("https://opensource.org/licenses/MIT"))

homepage := Some(url("https://github.com/company/project"))

scmInfo := Some(
  ScmInfo(
    url("https://github.com/company/project"),
    "scm:git@github.com:company/project.git"
  )
)

developers := List(
  Developer(
    id = "developer",
    name = "Developer Name",
    email = "dev@company.com",
    url = url("https://company.com")
  )
)
```

```bash
# Setup credentials
mkdir -p ~/.sbt/1.0/
cat > ~/.sbt/1.0/sonatype.sbt << EOF
credentials += Credentials(
  "Sonatype Nexus Repository Manager",
  "oss.sonatype.org",
  "your-username",
  "your-password"
)
EOF

# Publish
sbt publishSigned
sbt sonatypeBundleRelease
```

---

## Quick Reference

### Essential Commands

```bash
# Project setup
sbt new scala/scala-seed.g8

# Compile
sbt compile

# Test
sbt test

# Run
sbt run

# Package
sbt package

# Assembly (fat JAR)
sbt assembly

# Format
sbt scalafmt

# Console (REPL)
sbt console

# Continuous compilation
sbt ~compile

# Coverage
sbt clean coverage test coverageReport

# Dependency updates
sbt dependencyUpdates
```

### Checklist for Implementation

- [ ] Scala 2.13+ or 3.x installed
- [ ] SBT configured
- [ ] ScalaTest or other test framework setup
- [ ] Scalafmt configured
- [ ] Scalafix configured
- [ ] Tests written and passing (>90% coverage)
- [ ] Code formatted
- [ ] ScalaDoc generated
- [ ] CHANGELOG updated
- [ ] Version bumped

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-10-11 | Initial Scala manual creation |

---

## References

- [Scala Documentation](https://docs.scala-lang.org/)
- [Scala Style Guide](https://docs.scala-lang.org/style/)
- [ScalaTest Documentation](https://www.scalatest.org/)
- [Cats Documentation](https://typelevel.org/cats/)
- [SBT Documentation](https://www.scala-sbt.org/1.x/docs/)

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

