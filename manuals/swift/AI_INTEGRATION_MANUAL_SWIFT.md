# AI Integration Manual - Swift

**Version:** 1.0.0  
**Last Updated:** 2025-10-11  
**Target Audience:** AI Agents (LLM-based development assistants)  
**Language:** Swift  
**Swift Version:** >= 5.9  
**Platforms:** iOS 16+, macOS 13+, watchOS 9+, tvOS 16+

---

## Table of Contents

1. [Introduction](#introduction)
2. [Swift-Specific Setup](#swift-specific-setup)
3. [Project Structure](#project-structure)
4. [Phase 1: Planning](#phase-1-planning)
5. [Phase 2: Workspace Configuration](#phase-2-workspace-configuration)
6. [Phase 3: Implementation](#phase-3-implementation)
7. [Phase 4: Testing](#phase-4-testing)
8. [Phase 5: Documentation](#phase-5-documentation)
9. [Phase 6: Package Building & Distribution](#phase-6-package-building--distribution)
10. [Phase 7: Review & Quality Assurance](#phase-7-review--quality-assurance)
11. [Swift Best Practices](#swift-best-practices)
12. [App Store Guidelines](#app-store-guidelines)
13. [Quick Reference](#quick-reference)

---

## Introduction

This manual adapts the standard AI Integration Manual for Swift development, incorporating Swift-specific tools, conventions, and Apple platform best practices. Swift projects in the HiveLLM ecosystem should follow this guide for consistency and quality.

### Core Swift Principles

1. **Type Safety**: Leverage Swift's strong type system
2. **Protocol-Oriented**: Favor protocols over inheritance
3. **Value Types**: Prefer structs and enums over classes
4. **Memory Safety**: Use ARC, avoid retain cycles
5. **Modern Concurrency**: Use async/await and actors
6. **SwiftUI First**: Prefer SwiftUI for new projects
7. **Package Management**: Use Swift Package Manager (SPM)

### Swift Ecosystem Tools

| Tool | Purpose |
|------|---------|
| **Xcode** | Official IDE and build tool |
| **Swift Package Manager** | Dependency management |
| **XCTest** | Testing framework |
| **SwiftLint** | Linting and style enforcement |
| **swift-format** | Code formatting |
| **DocC** | Documentation compiler |
| **Instruments** | Profiling and debugging |
| **xcbeautify** | Xcode build output formatter |
| **fastlane** | Automation for releases |

---

## Swift-Specific Setup

### Step 1: Install Xcode and Command Line Tools

```bash
# Install Xcode from Mac App Store
# Or download from https://developer.apple.com/download/

# Install Command Line Tools
xcode-select --install

# Verify installation
swift --version
xcodebuild -version
```

### Step 2: Install Development Tools

```bash
# Install Homebrew (if not already installed)
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Install SwiftLint
brew install swiftlint

# Install swift-format
brew install swift-format

# Install xcbeautify
brew install xcbeautify

# Install fastlane (for automation)
brew install fastlane

# Verify installations
swiftlint version
swift-format --version
fastlane --version
```

### Step 3: Setup Swift Package Manager

```bash
# Create new package
swift package init --type library
# or
swift package init --type executable

# Update dependencies
swift package update

# Generate Xcode project (optional)
swift package generate-xcodeproj
```

### Step 4: Configure Code Signing

```bash
# List available certificates
security find-identity -v -p codesigning

# For automated builds, use environment variables
export FASTLANE_USER="your.apple.id@example.com"
export FASTLANE_PASSWORD="app-specific-password"
```

---

## Project Structure

### Swift Package Structure

```
PackageName/
├── .github/
│   └── workflows/
│       ├── ci.yml
│       ├── release.yml
│       └── docs.yml
├── .swiftlint.yml              # SwiftLint configuration
├── .swift-format               # swift-format configuration
├── .gitignore
├── Package.swift               # Package manifest
├── README.md
├── CHANGELOG.md
├── LICENSE
├── Sources/
│   └── PackageName/
│       ├── PackageName.swift   # Main module
│       ├── Models/             # Data models
│       ├── Services/           # Business logic
│       ├── Utilities/          # Helper functions
│       └── Extensions/         # Type extensions
├── Tests/
│   └── PackageNameTests/
│       ├── UnitTests/
│       ├── IntegrationTests/
│       └── Resources/          # Test fixtures
├── docs/                       # HiveLLM documentation
│   ├── ROADMAP.md
│   ├── SPECS.md
│   ├── specs/
│   └── reviews/
├── scripts/                    # Build and automation scripts
│   ├── build.sh
│   ├── test.sh
│   ├── lint.sh
│   └── format.sh
└── Documentation/              # DocC documentation
    └── PackageName.docc/
        ├── PackageName.md
        ├── GettingStarted.md
        └── Articles/
```

### iOS App Structure

```
AppName/
├── AppName.xcodeproj/
├── AppName.xcworkspace/        # If using CocoaPods
├── .swiftlint.yml
├── .gitignore
├── Podfile                     # CocoaPods (optional)
├── Package.swift               # SPM dependencies
├── AppName/
│   ├── App/
│   │   ├── AppNameApp.swift   # @main entry point
│   │   └── AppDelegate.swift   # If needed
│   ├── Core/
│   │   ├── Models/
│   │   ├── ViewModels/
│   │   ├── Services/
│   │   └── Utilities/
│   ├── Features/
│   │   ├── Home/
│   │   │   ├── Views/
│   │   │   ├── ViewModels/
│   │   │   └── Models/
│   │   └── ...
│   ├── Resources/
│   │   ├── Assets.xcassets/
│   │   ├── Colors.xcassets/
│   │   ├── Localizable.strings
│   │   └── Info.plist
│   └── Supporting Files/
├── AppNameTests/
│   ├── UnitTests/
│   ├── ViewModelTests/
│   └── Mocks/
├── AppNameUITests/
│   └── UITests/
├── docs/
│   ├── ROADMAP.md
│   ├── SPECS.md
│   └── specs/
└── scripts/
    ├── build.sh
    ├── test.sh
    └── upload.sh
```

---

## Phase 1: Planning

### Step 1.1: Initialize Project

#### For Swift Package:

```bash
# Create package directory
mkdir PackageName && cd PackageName

# Initialize package
swift package init --type library

# Initialize git
git init
git add .
git commit -m "chore: initial package setup"
```

#### For iOS/macOS App:

```bash
# Create Xcode project via Xcode GUI or:
# File > New > Project > Choose template

# Add to git
git init
git add .
git commit -m "chore: initial Xcode project setup"
```

### Step 1.2: Configure Package.swift

Edit `Package.swift`:

```swift
// swift-tools-version: 5.9
import PackageDescription

let package = Package(
    name: "PackageName",
    platforms: [
        .iOS(.v16),
        .macOS(.v13),
        .watchOS(.v9),
        .tvOS(.v16)
    ],
    products: [
        .library(
            name: "PackageName",
            targets: ["PackageName"]
        ),
    ],
    dependencies: [
        // External dependencies
        .package(url: "https://github.com/apple/swift-algorithms", from: "1.0.0"),
        .package(url: "https://github.com/apple/swift-collections", from: "1.0.0"),
    ],
    targets: [
        .target(
            name: "PackageName",
            dependencies: [
                .product(name: "Algorithms", package: "swift-algorithms"),
                .product(name: "Collections", package: "swift-collections"),
            ]
        ),
        .testTarget(
            name: "PackageNameTests",
            dependencies: ["PackageName"]
        ),
    ]
)
```

### Step 1.3: Create ROADMAP.md

Create `docs/ROADMAP.md`:

```markdown
# Package Development Roadmap

**Package:** PackageName  
**Version:** 0.1.0  
**Platform:** iOS 16+, macOS 13+, watchOS 9+, tvOS 16+  
**Last Updated:** 2025-10-11

## Phase 1: Foundation (v0.1.0)

### 1.1 Package Setup
- [x] Initialize Swift Package Manager
- [x] Configure Package.swift
- [x] Setup SwiftLint and swift-format
- [x] Configure CI/CD workflows
- [ ] Create package documentation
- [ ] Setup DocC documentation

### 1.2 Core Protocols
- [ ] 1.2.1 Define base protocols
- [ ] 1.2.2 Implement protocol extensions
- [ ] 1.2.3 Create protocol compositions
- [ ] 1.2.4 Write protocol tests

### 1.3 Value Types
- [ ] 1.3.1 Define core structs
- [ ] 1.3.2 Implement Codable conformance
- [ ] 1.3.3 Add Equatable/Hashable
- [ ] 1.3.4 Write unit tests

## Phase 2: Feature Implementation (v0.2.0)

### 2.1 Business Logic
- [ ] 2.1.1 Implement main algorithms
- [ ] 2.1.2 Add error handling
- [ ] 2.1.3 Performance optimization
- [ ] 2.1.4 Integration tests

### 2.2 Async Operations
- [ ] 2.2.1 Implement async/await APIs
- [ ] 2.2.2 Add Task management
- [ ] 2.2.3 Implement actors for state
- [ ] 2.2.4 Concurrency tests

## Phase 3: Polish & Documentation (v0.3.0)

### 3.1 Documentation
- [ ] 3.1.1 Complete DocC documentation
- [ ] 3.1.2 Add code examples
- [ ] 3.1.3 Create tutorials
- [ ] 3.1.4 Write migration guides

### 3.2 Performance
- [ ] 3.2.1 Profile with Instruments
- [ ] 3.2.2 Optimize critical paths
- [ ] 3.2.3 Memory leak checks
- [ ] 3.2.4 Performance benchmarks

## Phase 4: Release (v1.0.0)

### 4.1 Final Review
- [ ] 4.1.1 Code review by 2+ Swift developers
- [ ] 4.1.2 Address all review comments
- [ ] 4.1.3 Cross-platform testing
- [ ] 4.1.4 Documentation review

### 4.2 Release Preparation
- [ ] 4.2.1 Update version numbers
- [ ] 4.2.2 Update CHANGELOG
- [ ] 4.2.3 Tag release
- [ ] 4.2.4 Publish documentation

## Status Legend
- [ ] Todo
- [~] In Progress  
- [x] Completed
- [!] Blocked

## Dependencies

| Package | Version | Purpose | Status |
|---------|---------|---------|--------|
| swift-algorithms | 1.0+ | Algorithm utilities | Optional |
| swift-collections | 1.0+ | Advanced collections | Optional |

## Last Updated: 2025-10-11
```

### Step 1.4: Generate SPECS.md

Create `docs/SPECS.md`:

```markdown
# Package Specifications

**Package:** PackageName  
**Version:** 0.1.0  
**Platforms:** iOS 16+, macOS 13+, watchOS 9+, tvOS 16+

## Overview

Brief description of the package functionality, problem it solves, and target audience.

## Target Audience

- iOS/macOS developers working with [domain]
- Swift developers building [type of applications]
- Teams needing [specific functionality]

## Features

### Feature 1: [Name]
**Priority**: High  
**Status**: Planning  
**Description**: [2-3 sentence summary]  
**Platforms**: iOS, macOS, watchOS, tvOS

### Feature 2: [Name]
**Priority**: Medium  
**Status**: Planning  
**Description**: [2-3 sentence summary]  
**Platforms**: iOS, macOS

## Technical Stack

- **Language**: Swift 5.9+
- **Minimum Deployment**: iOS 16+, macOS 13+, watchOS 9+, tvOS 16+
- **Package Manager**: Swift Package Manager
- **Testing**: XCTest
- **Documentation**: DocC
- **CI/CD**: GitHub Actions with Xcode Cloud

## Architecture

- **Design Pattern**: Protocol-Oriented Programming
- **Concurrency**: Swift Concurrency (async/await, actors)
- **UI Framework**: SwiftUI (if applicable)
- **State Management**: @Observable, @State, @Binding

## Dependencies

### Required
- Foundation
- [Platform-specific frameworks]

### Optional
- swift-algorithms - Advanced algorithms
- swift-collections - Specialized collections

## API Design Philosophy

- **Protocol-Oriented**: Define behavior through protocols
- **Value Semantics**: Prefer structs over classes
- **Type Safety**: Leverage generics and associated types
- **Async by Default**: Use async/await for I/O operations
- **Sendable**: Ensure thread-safe types

## Performance Targets

- Launch time: < 400ms
- Frame rate: 60 FPS (120 FPS on ProMotion)
- Memory footprint: < [specific limit]
- Battery impact: Minimal

## Compatibility

- Swift: 5.9+
- iOS: 16.0+
- macOS: 13.0+
- watchOS: 9.0+
- tvOS: 16.0+
- Xcode: 15.0+
```

---

## Phase 2: Workspace Configuration

### Step 2.1: Configure SwiftLint

Create `.swiftlint.yml`:

```yaml
# SwiftLint Configuration

# Include all Swift files
included:
  - Sources
  - Tests

# Exclude generated or third-party code
excluded:
  - Pods
  - .build
  - DerivedData

# Enabled rules
opt_in_rules:
  - array_init
  - attributes
  - closure_end_indentation
  - closure_spacing
  - conditional_returns_on_newline
  - empty_count
  - empty_string
  - explicit_init
  - extension_access_modifier
  - fallthrough
  - fatal_error_message
  - file_header
  - first_where
  - force_unwrapping
  - implicitly_unwrapped_optional
  - implicit_return
  - joined_default_parameter
  - let_var_whitespace
  - literal_expression_end_indentation
  - multiline_arguments
  - multiline_function_chains
  - multiline_literal_brackets
  - multiline_parameters
  - multiline_parameters_brackets
  - operator_usage_whitespace
  - overridden_super_call
  - pattern_matching_keywords
  - prefer_self_type_over_type_of_self
  - prohibited_super_call
  - redundant_nil_coalescing
  - redundant_type_annotation
  - single_test_class
  - sorted_first_last
  - sorted_imports
  - strict_fileprivate
  - toggle_bool
  - trailing_closure
  - unavailable_function
  - unneeded_parentheses_in_closure_argument
  - vertical_parameter_alignment_on_call
  - vertical_whitespace_closing_braces
  - vertical_whitespace_opening_braces
  - yoda_condition

# Disabled rules
disabled_rules:
  - todo
  - line_length # Handled by swift-format
  - file_length # May need long files
  - type_body_length

# Rule configurations
identifier_name:
  min_length: 2
  max_length: 50
  excluded:
    - id
    - x
    - y
    - z

type_name:
  min_length: 3
  max_length: 50

function_body_length:
  warning: 50
  error: 100

cyclomatic_complexity:
  warning: 10
  error: 20

nesting:
  type_level: 2
  statement_level: 5

# Custom rules
custom_rules:
  no_print:
    name: "No Print Statements"
    regex: "print\\("
    message: "Use proper logging instead of print()"
    severity: warning
```

### Step 2.2: Configure swift-format

Create `.swift-format`:

```json
{
  "version": 1,
  "lineLength": 100,
  "indentation": {
    "spaces": 2
  },
  "respectsExistingLineBreaks": true,
  "lineBreakBeforeControlFlowKeywords": false,
  "lineBreakBeforeEachArgument": true,
  "lineBreakBeforeEachGenericRequirement": true,
  "prioritizeKeepingFunctionOutputTogether": true,
  "indentConditionalCompilationBlocks": true,
  "lineBreakAroundMultilineExpressionChainComponents": true,
  "rules": {
    "AllPublicDeclarationsHaveDocumentation": true,
    "AlwaysUseLowerCamelCase": true,
    "AmbiguousTrailingClosureOverload": true,
    "BeginDocumentationCommentWithOneLineSummary": true,
    "DoNotUseSemicolons": true,
    "DontRepeatTypeInStaticProperties": true,
    "FileScopedDeclarationPrivacy": true,
    "FullyIndirectEnum": true,
    "GroupNumericLiterals": true,
    "IdentifiersMustBeASCII": true,
    "NeverForceUnwrap": false,
    "NeverUseForceTry": false,
    "NeverUseImplicitlyUnwrappedOptionals": false,
    "NoAccessLevelOnExtensionDeclaration": true,
    "NoBlockComments": true,
    "NoCasesWithOnlyFallthrough": true,
    "NoEmptyTrailingClosureParentheses": true,
    "NoLabelsInCasePatterns": true,
    "NoLeadingUnderscores": false,
    "NoParensAroundConditions": true,
    "NoVoidReturnOnFunctionSignature": true,
    "OneCasePerLine": true,
    "OneVariableDeclarationPerLine": true,
    "OnlyOneTrailingClosureArgument": true,
    "OrderedImports": true,
    "ReturnVoidInsteadOfEmptyTuple": true,
    "UseLetInEveryBoundCaseVariable": true,
    "UseShorthandTypeNames": true,
    "UseSingleLinePropertyGetter": true,
    "UseSynthesizedInitializer": true,
    "UseTripleSlashForDocumentationComments": true,
    "ValidateDocumentationComments": true
  }
}
```

### Step 2.3: Setup GitHub Actions

Create `.github/workflows/ci.yml`:

```yaml
name: CI

on:
  push:
    branches: [main, develop]
  pull_request:
    branches: [main, develop]

jobs:
  lint:
    name: SwiftLint
    runs-on: macos-14
    steps:
      - uses: actions/checkout@v4
      
      - name: SwiftLint
        run: |
          brew install swiftlint
          swiftlint --strict

  format:
    name: swift-format
    runs-on: macos-14
    steps:
      - uses: actions/checkout@v4
      
      - name: Check formatting
        run: |
          brew install swift-format
          swift-format lint --recursive Sources Tests

  test-ios:
    name: Test iOS
    runs-on: macos-14
    steps:
      - uses: actions/checkout@v4
      
      - name: Select Xcode
        run: sudo xcode-select -s /Applications/Xcode_15.2.app
      
      - name: Build and Test
        run: |
          xcodebuild test \
            -scheme PackageName \
            -destination 'platform=iOS Simulator,name=iPhone 15 Pro,OS=17.2' \
            -enableCodeCoverage YES \
            | xcbeautify
      
      - name: Upload coverage
        uses: codecov/codecov-action@v3
        with:
          files: ./coverage.lcov

  test-macos:
    name: Test macOS
    runs-on: macos-14
    steps:
      - uses: actions/checkout@v4
      
      - name: Build and Test
        run: |
          swift test --enable-code-coverage
      
      - name: Generate coverage
        run: |
          xcrun llvm-cov export \
            .build/debug/PackageNamePackageTests.xctest/Contents/MacOS/PackageNamePackageTests \
            -instr-profile .build/debug/codecov/default.profdata \
            -format="lcov" > coverage.lcov

  spm-build:
    name: SPM Build
    strategy:
      matrix:
        os: [macos-14]
        swift: ["5.9", "5.10"]
    runs-on: ${{ matrix.os }}
    steps:
      - uses: actions/checkout@v4
      
      - name: Build
        run: swift build -c release
```

### Step 2.4: Configure .gitignore

Create `.gitignore`:

```
# Xcode
*.xcodeproj/*
!*.xcodeproj/project.pbxproj
!*.xcodeproj/xcshareddata/
!*.xcworkspace/contents.xcworkspacedata
*.xcworkspace/*
!*.xcworkspace/xcshareddata/
xcuserdata/
DerivedData/
.swiftpm/
*.xcuserstate

# Build
.build/
build/
Packages/

# CocoaPods
Pods/
Podfile.lock

# Carthage
Carthage/Build/

# fastlane
fastlane/report.xml
fastlane/Preview.html
fastlane/screenshots/**/*.png
fastlane/test_output

# Code coverage
*.gcda
*.gcno
*.profdata
*.profraw
coverage.lcov

# OS
.DS_Store
.AppleDouble
.LSOverride

# Thumbnails
._*

# Files that might appear in the root
.DocumentRevisions-V100
.fseventsd
.Spotlight-V100
.TemporaryItems
.Trashes
.VolumeIcon.icns
.com.apple.timemachine.donotpresent

# IDEs
.vscode/
.idea/
*.swp
*.swo
*~

# Environment
.env
.env.local
```

---

## Phase 3: Implementation

### Step 3.1: Protocol-Oriented Design

```swift
// Sources/PackageName/Protocols/DataProvider.swift

/// A type that provides data from a source.
public protocol DataProvider {
  /// The type of data provided.
  associatedtype Data
  
  /// Fetches data asynchronously.
  /// - Returns: The fetched data.
  /// - Throws: An error if fetching fails.
  func fetch() async throws -> Data
}

/// A type that can be cached.
public protocol Cacheable {
  /// A unique identifier for caching.
  var cacheKey: String { get }
  
  /// The cache expiration time.
  var cacheExpiration: TimeInterval { get }
}

// Protocol extension with default implementation
extension Cacheable {
  public var cacheExpiration: TimeInterval {
    60 * 60 // 1 hour default
  }
}
```

### Step 3.2: Value Types (Structs)

```swift
// Sources/PackageName/Models/User.swift

/// Represents a user in the system.
public struct User: Sendable {
  // MARK: - Properties
  
  /// The user's unique identifier.
  public let id: UUID
  
  /// The user's display name.
  public var name: String
  
  /// The user's email address.
  public var email: String
  
  /// The date the user was created.
  public let createdAt: Date
  
  // MARK: - Initialization
  
  /// Creates a new user.
  /// - Parameters:
  ///   - id: The unique identifier. Defaults to a new UUID.
  ///   - name: The display name.
  ///   - email: The email address.
  ///   - createdAt: The creation date. Defaults to now.
  public init(
    id: UUID = UUID(),
    name: String,
    email: String,
    createdAt: Date = Date()
  ) {
    self.id = id
    self.name = name
    self.email = email
    self.createdAt = createdAt
  }
}

// MARK: - Codable

extension User: Codable {
  enum CodingKeys: String, CodingKey {
    case id
    case name
    case email
    case createdAt = "created_at"
  }
}

// MARK: - Identifiable

extension User: Identifiable {}

// MARK: - Equatable

extension User: Equatable {
  public static func == (lhs: User, rhs: User) -> Bool {
    lhs.id == rhs.id
  }
}

// MARK: - Hashable

extension User: Hashable {
  public func hash(into hasher: inout Hasher) {
    hasher.combine(id)
  }
}
```

### Step 3.3: Error Handling

```swift
// Sources/PackageName/Errors/NetworkError.swift

/// Errors that can occur during network operations.
public enum NetworkError: Error {
  /// The request was invalid.
  case invalidRequest
  
  /// The response was invalid.
  case invalidResponse
  
  /// The server returned an error.
  case serverError(statusCode: Int)
  
  /// Network connection failed.
  case connectionFailed
  
  /// The request timed out.
  case timeout
  
  /// Decoding the response failed.
  case decodingFailed(Error)
  
  /// An unknown error occurred.
  case unknown(Error)
}

// MARK: - LocalizedError

extension NetworkError: LocalizedError {
  public var errorDescription: String? {
    switch self {
    case .invalidRequest:
      return "The request was invalid."
    case .invalidResponse:
      return "The server returned an invalid response."
    case .serverError(let statusCode):
      return "The server returned an error (status code: \(statusCode))."
    case .connectionFailed:
      return "Network connection failed."
    case .timeout:
      return "The request timed out."
    case .decodingFailed:
      return "Failed to decode the response."
    case .unknown(let error):
      return "An unknown error occurred: \(error.localizedDescription)"
    }
  }
}

// MARK: - CustomStringConvertible

extension NetworkError: CustomStringConvertible {
  public var description: String {
    errorDescription ?? "NetworkError"
  }
}
```

### Step 3.4: Async/Await Services

```swift
// Sources/PackageName/Services/APIClient.swift

import Foundation

/// A client for making API requests.
public actor APIClient {
  // MARK: - Properties
  
  private let session: URLSession
  private let baseURL: URL
  
  // MARK: - Initialization
  
  /// Creates a new API client.
  /// - Parameters:
  ///   - baseURL: The base URL for API requests.
  ///   - session: The URL session to use. Defaults to `.shared`.
  public init(baseURL: URL, session: URLSession = .shared) {
    self.baseURL = baseURL
    self.session = session
  }
  
  // MARK: - Public Methods
  
  /// Performs a GET request.
  /// - Parameter path: The path to append to the base URL.
  /// - Returns: The decoded response.
  /// - Throws: `NetworkError` if the request fails.
  public func get<T: Decodable>(path: String) async throws -> T {
    let url = baseURL.appendingPathComponent(path)
    let request = URLRequest(url: url)
    return try await perform(request: request)
  }
  
  /// Performs a POST request.
  /// - Parameters:
  ///   - path: The path to append to the base URL.
  ///   - body: The request body.
  /// - Returns: The decoded response.
  /// - Throws: `NetworkError` if the request fails.
  public func post<T: Decodable, U: Encodable>(
    path: String,
    body: U
  ) async throws -> T {
    let url = baseURL.appendingPathComponent(path)
    var request = URLRequest(url: url)
    request.httpMethod = "POST"
    request.setValue("application/json", forHTTPHeaderField: "Content-Type")
    
    let encoder = JSONEncoder()
    request.httpBody = try encoder.encode(body)
    
    return try await perform(request: request)
  }
  
  // MARK: - Private Methods
  
  private func perform<T: Decodable>(request: URLRequest) async throws -> T {
    do {
      let (data, response) = try await session.data(for: request)
      
      guard let httpResponse = response as? HTTPURLResponse else {
        throw NetworkError.invalidResponse
      }
      
      guard (200...299).contains(httpResponse.statusCode) else {
        throw NetworkError.serverError(statusCode: httpResponse.statusCode)
      }
      
      let decoder = JSONDecoder()
      return try decoder.decode(T.self, from: data)
    } catch let error as NetworkError {
      throw error
    } catch let error as DecodingError {
      throw NetworkError.decodingFailed(error)
    } catch {
      throw NetworkError.unknown(error)
    }
  }
}
```

### Step 3.5: SwiftUI Integration

```swift
// Sources/PackageName/Views/UserListView.swift

import SwiftUI

/// A view that displays a list of users.
@available(iOS 16.0, macOS 13.0, watchOS 9.0, tvOS 16.0, *)
public struct UserListView: View {
  // MARK: - Properties
  
  @State private var viewModel: UserListViewModel
  
  // MARK: - Initialization
  
  /// Creates a new user list view.
  /// - Parameter viewModel: The view model to use.
  public init(viewModel: UserListViewModel) {
    self._viewModel = State(initialValue: viewModel)
  }
  
  // MARK: - Body
  
  public var body: some View {
    NavigationStack {
      Group {
        switch viewModel.state {
        case .idle:
          Color.clear
        case .loading:
          ProgressView("Loading...")
        case .loaded(let users):
          List(users) { user in
            UserRowView(user: user)
          }
        case .error(let error):
          ErrorView(error: error) {
            Task {
              await viewModel.loadUsers()
            }
          }
        }
      }
      .navigationTitle("Users")
      .task {
        await viewModel.loadUsers()
      }
    }
  }
}

// MARK: - View Model

/// The view model for the user list.
@Observable
public final class UserListViewModel {
  // MARK: - State
  
  public enum State {
    case idle
    case loading
    case loaded([User])
    case error(Error)
  }
  
  // MARK: - Properties
  
  public private(set) var state: State = .idle
  
  private let apiClient: APIClient
  
  // MARK: - Initialization
  
  public init(apiClient: APIClient) {
    self.apiClient = apiClient
  }
  
  // MARK: - Public Methods
  
  @MainActor
  public func loadUsers() async {
    state = .loading
    
    do {
      let users: [User] = try await apiClient.get(path: "/users")
      state = .loaded(users)
    } catch {
      state = .error(error)
    }
  }
}
```

---

## Phase 4: Testing

### Step 4.1: Unit Tests

```swift
// Tests/PackageNameTests/UserTests.swift

import XCTest
@testable import PackageName

final class UserTests: XCTestCase {
  // MARK: - Properties
  
  var sut: User!
  
  // MARK: - Setup & Teardown
  
  override func setUp() {
    super.setUp()
    sut = User(
      id: UUID(),
      name: "John Doe",
      email: "john@example.com"
    )
  }
  
  override func tearDown() {
    sut = nil
    super.tearDown()
  }
  
  // MARK: - Tests
  
  func testUserInitialization() {
    // Given
    let name = "Jane Doe"
    let email = "jane@example.com"
    
    // When
    let user = User(name: name, email: email)
    
    // Then
    XCTAssertEqual(user.name, name)
    XCTAssertEqual(user.email, email)
    XCTAssertNotNil(user.id)
    XCTAssertNotNil(user.createdAt)
  }
  
  func testUserEquality() {
    // Given
    let user1 = User(id: UUID(), name: "John", email: "john@example.com")
    let user2 = User(id: user1.id, name: "Jane", email: "jane@example.com")
    let user3 = User(id: UUID(), name: "John", email: "john@example.com")
    
    // Then
    XCTAssertEqual(user1, user2) // Same ID
    XCTAssertNotEqual(user1, user3) // Different ID
  }
  
  func testUserCodable() throws {
    // Given
    let encoder = JSONEncoder()
    let decoder = JSONDecoder()
    encoder.dateEncodingStrategy = .iso8601
    decoder.dateDecodingStrategy = .iso8601
    
    // When
    let encoded = try encoder.encode(sut)
    let decoded = try decoder.decode(User.self, from: encoded)
    
    // Then
    XCTAssertEqual(decoded.id, sut.id)
    XCTAssertEqual(decoded.name, sut.name)
    XCTAssertEqual(decoded.email, sut.email)
  }
}
```

### Step 4.2: Async Tests

```swift
// Tests/PackageNameTests/APIClientTests.swift

import XCTest
@testable import PackageName

final class APIClientTests: XCTestCase {
  // MARK: - Properties
  
  var sut: APIClient!
  var mockSession: URLSession!
  
  // MARK: - Setup & Teardown
  
  override func setUp() async throws {
    try await super.setUp()
    
    let configuration = URLSessionConfiguration.ephemeral
    configuration.protocolClasses = [MockURLProtocol.self]
    mockSession = URLSession(configuration: configuration)
    
    sut = APIClient(
      baseURL: URL(string: "https://api.example.com")!,
      session: mockSession
    )
  }
  
  override func tearDown() async throws {
    sut = nil
    mockSession = nil
    MockURLProtocol.requestHandler = nil
    try await super.tearDown()
  }
  
  // MARK: - Tests
  
  func testGetRequestSuccess() async throws {
    // Given
    let expectedUser = User(name: "John Doe", email: "john@example.com")
    let expectedData = try JSONEncoder().encode(expectedUser)
    
    MockURLProtocol.requestHandler = { request in
      let response = HTTPURLResponse(
        url: request.url!,
        statusCode: 200,
        httpVersion: nil,
        headerFields: nil
      )!
      return (response, expectedData)
    }
    
    // When
    let user: User = try await sut.get(path: "/users/1")
    
    // Then
    XCTAssertEqual(user.name, expectedUser.name)
    XCTAssertEqual(user.email, expectedUser.email)
  }
  
  func testGetRequestServerError() async {
    // Given
    MockURLProtocol.requestHandler = { request in
      let response = HTTPURLResponse(
        url: request.url!,
        statusCode: 500,
        httpVersion: nil,
        headerFields: nil
      )!
      return (response, Data())
    }
    
    // When/Then
    do {
      let _: User = try await sut.get(path: "/users/1")
      XCTFail("Should throw error")
    } catch let error as NetworkError {
      if case .serverError(let statusCode) = error {
        XCTAssertEqual(statusCode, 500)
      } else {
        XCTFail("Wrong error type")
      }
    } catch {
      XCTFail("Wrong error type")
    }
  }
}

// MARK: - Mock URL Protocol

class MockURLProtocol: URLProtocol {
  static var requestHandler: ((URLRequest) throws -> (HTTPURLResponse, Data))?
  
  override class func canInit(with request: URLRequest) -> Bool {
    true
  }
  
  override class func canonicalRequest(for request: URLRequest) -> URLRequest {
    request
  }
  
  override func startLoading() {
    guard let handler = MockURLProtocol.requestHandler else {
      fatalError("Request handler not set")
    }
    
    do {
      let (response, data) = try handler(request)
      client?.urlProtocol(self, didReceive: response, cacheStoragePolicy: .notAllowed)
      client?.urlProtocol(self, didLoad: data)
      client?.urlProtocolDidFinishLoading(self)
    } catch {
      client?.urlProtocol(self, didFailWithError: error)
    }
  }
  
  override func stopLoading() {}
}
```

### Step 4.3: Running Tests

```bash
# Run all tests
swift test

# Run with coverage
swift test --enable-code-coverage

# Run specific test
swift test --filter UserTests

# Xcode
xcodebuild test \
  -scheme PackageName \
  -destination 'platform=iOS Simulator,name=iPhone 15 Pro' \
  -enableCodeCoverage YES
```

---

## Phase 5: Documentation

### Step 5.1: DocC Documentation

Create `Documentation/PackageName.docc/PackageName.md`:

```markdown
# ``PackageName``

A brief one-line description of your package.

## Overview

A longer description that explains what the package does, why someone
would use it, and what problems it solves.

## Topics

### Essentials

- <doc:GettingStarted>
- ``User``
- ``APIClient``

### Protocols

- ``DataProvider``
- ``Cacheable``

### Error Handling

- ``NetworkError``

### Advanced Topics

- <doc:AdvancedUsage>
- <doc:BestPractices>
```

Create `Documentation/PackageName.docc/GettingStarted.md`:

```markdown
# Getting Started

Learn how to integrate PackageName into your project.

## Installation

### Swift Package Manager

Add PackageName to your `Package.swift`:

```swift
dependencies: [
  .package(url: "https://github.com/username/PackageName", from: "1.0.0")
]
```

Or in Xcode:
1. File > Add Package Dependencies
2. Enter the repository URL
3. Select version

## Basic Usage

### Setting Up

First, import the package:

```swift
import PackageName
```

Create an API client:

```swift
let client = APIClient(baseURL: URL(string: "https://api.example.com")!)
```

### Making Requests

Fetch data asynchronously:

```swift
Task {
  do {
    let user: User = try await client.get(path: "/users/1")
    print(user.name)
  } catch {
    print("Error: \(error)")
  }
}
```

## Next Steps

- Explore ``APIClient`` for more request methods
- Learn about error handling with ``NetworkError``
- Read <doc:AdvancedUsage> for advanced features
```

### Step 5.2: Build Documentation

```bash
# Build DocC documentation
swift package generate-documentation

# Preview documentation
swift package --disable-sandbox preview-documentation --target PackageName

# Export for hosting
swift package generate-documentation \
  --target PackageName \
  --output-path ./docs

# Xcode: Product > Build Documentation
```

---

## Phase 6: Package Building & Distribution

### Step 6.1: Version Management

```swift
// Update Package.swift version comment
// Ensure all version numbers are consistent

// Tag release
git tag -a 1.0.0 -m "Release version 1.0.0"
git push origin 1.0.0
```

### Step 6.2: Build for Release

```bash
# Build in release mode
swift build -c release

# Archive framework (if needed)
xcodebuild archive \
  -scheme PackageName \
  -destination "generic/platform=iOS" \
  -archivePath ./build/PackageName

# Create XCFramework
xcodebuild -create-xcframework \
  -framework ./build/iOS/PackageName.framework \
  -framework ./build/macOS/PackageName.framework \
  -output ./PackageName.xcframework
```

### Step 6.3: Swift Package Index

Ensure compatibility with Swift Package Index:

1. Add `.spi.yml` configuration:

```yaml
version: 1
builder:
  configs:
    - documentation_targets: [PackageName]
      platform: ios
      swift_version: 5.9
    - documentation_targets: [PackageName]
      platform: macos
      swift_version: 5.9
```

2. Add README badge:

```markdown
[![](https://img.shields.io/endpoint?url=https%3A%2F%2Fswiftpackageindex.com%2Fapi%2Fpackages%2Fusername%2FPackageName%2Fbadge%3Ftype%3Dswift-versions)](https://swiftpackageindex.com/username/PackageName)
```

---

## Phase 7: Review & Quality Assurance

### Step 7.1: Pre-Release Checklist

```markdown
# Pre-Release Checklist

**Package:** PackageName  
**Version:** 1.0.0  
**Date:** 2025-10-11

## Code Quality

- [ ] SwiftLint passes with no warnings
- [ ] swift-format passes
- [ ] No force unwraps (or justified)
- [ ] No force try (or justified)
- [ ] Proper error handling throughout
- [ ] No compiler warnings

## Testing

- [ ] All tests pass
- [ ] Test coverage > 90%
- [ ] Async tests included
- [ ] Edge cases covered
- [ ] Performance tests (if applicable)

## Documentation

- [ ] All public APIs documented
- [ ] DocC builds successfully
- [ ] README is comprehensive
- [ ] Code examples work
- [ ] CHANGELOG updated
- [ ] Migration guide (if breaking changes)

## Compatibility

- [ ] Builds on iOS 16+
- [ ] Builds on macOS 13+
- [ ] Builds on watchOS 9+ (if supported)
- [ ] Builds on tvOS 16+ (if supported)
- [ ] Swift 5.9+ compatible
- [ ] Xcode 15+ compatible

## Package

- [ ] Package.swift is correct
- [ ] Dependencies are minimal
- [ ] Version numbers consistent
- [ ] LICENSE file present
- [ ] .gitignore is complete

## Performance

- [ ] No memory leaks (Instruments check)
- [ ] No retain cycles
- [ ] Efficient algorithms used
- [ ] Lazy loading where appropriate

## Security

- [ ] No hardcoded credentials
- [ ] Secure network requests
- [ ] Input validation
- [ ] No sensitive data logged
```

---

## Swift Best Practices

### Naming Conventions

```swift
// Types: PascalCase
struct UserProfile {}
class NetworkManager {}
enum LoadingState {}
protocol DataProvider {}

// Functions and variables: camelCase
func fetchUsers() {}
var userName: String
let maxRetries = 3

// Constants: camelCase (not SCREAMING_SNAKE_CASE in Swift)
let defaultTimeout: TimeInterval = 30

// Private properties: prefix with underscore (optional)
private var _cache: [String: Any]
```

### Optionals

```swift
// Good: Use optional binding
if let user = optionalUser {
  print(user.name)
}

// Good: Guard for early exit
guard let user = optionalUser else {
  return
}

// Good: Nil coalescing
let name = user?.name ?? "Unknown"

// Bad: Force unwrapping (avoid unless guaranteed)
let name = user!.name  // Dangerous!

// Good: Implicitly unwrapped optionals only for IBOutlets
@IBOutlet var label: UILabel!
```

### Error Handling

```swift
// Good: Specific error handling
do {
  let data = try loadData()
  process(data)
} catch let error as NetworkError {
  handleNetworkError(error)
} catch {
  handleUnknownError(error)
}

// Bad: Silent failure
try? loadData()  // Use sparingly
```

### Memory Management

```swift
// Good: Weak references in closures
fetchData { [weak self] result in
  guard let self else { return }
  self.handle(result)
}

// Good: Unowned for guaranteed non-nil
class Child {
  unowned let parent: Parent
  init(parent: Parent) {
    self.parent = parent
  }
}

// Avoid: Strong reference cycles
class Bad {
  var closure: (() -> Void)?
  
  func setup() {
    closure = {
      self.doSomething()  // Cycle!
    }
  }
}
```

---

## App Store Guidelines

### App Review Checklist

- [ ] No private APIs used
- [ ] Proper usage descriptions in Info.plist
- [ ] App privacy policy (if collecting data)
- [ ] Age rating appropriate
- [ ] Screenshots and description accurate
- [ ] Testable account credentials provided
- [ ] Follows Human Interface Guidelines

### Info.plist Privacy Keys

```xml
<key>NSCameraUsageDescription</key>
<string>We need camera access to take photos.</string>

<key>NSPhotoLibraryUsageDescription</key>
<string>We need photo library access to select images.</string>

<key>NSLocationWhenInUseUsageDescription</key>
<string>We need your location to show nearby places.</string>
```

---

## Quick Reference

### Essential Commands

```bash
# Package Management
swift package init --type library
swift package update
swift package resolve
swift build
swift test

# Xcode
xcodebuild -list
xcodebuild -scheme MyScheme build
xcodebuild test -scheme MyScheme

# Code Quality
swiftlint
swiftlint --fix
swift-format lint --recursive Sources
swift-format format --in-place --recursive Sources

# Documentation
swift package generate-documentation
swift package preview-documentation

# Simulator
xcrun simctl list devices
xcrun simctl boot "iPhone 15 Pro"
```

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-10-11 | Initial Swift manual creation |

---

## Resources

- [Swift.org](https://swift.org/)
- [Swift Evolution](https://github.com/apple/swift-evolution)
- [Apple Developer Documentation](https://developer.apple.com/documentation/)
- [Swift Package Manager](https://github.com/apple/swift-package-manager)
- [SwiftLint](https://github.com/realm/SwiftLint)
- [swift-format](https://github.com/apple/swift-format)

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

