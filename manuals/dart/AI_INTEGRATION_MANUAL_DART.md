# AI Integration Manual - Dart

**Version:** 1.0.0  
**Last Updated:** 2025-10-11  
**Target Audience:** AI Agents (LLM-based development assistants)  
**Language:** Dart  
**Dart Version:** >= 3.0  
**Flutter Version:** >= 3.13  
**Platforms:** iOS, Android, Web, Windows, macOS, Linux

---

## Table of Contents

1. [Introduction](#introduction)
2. [Dart-Specific Setup](#dart-specific-setup)
3. [Project Structure](#project-structure)
4. [Phase 1: Planning](#phase-1-planning)
5. [Phase 2: Workspace Configuration](#phase-2-workspace-configuration)
6. [Phase 3: Implementation](#phase-3-implementation)
7. [Phase 4: Testing](#phase-4-testing)
8. [Phase 5: Documentation](#phase-5-documentation)
9. [Phase 6: Package Building & Distribution](#phase-6-package-building--distribution)
10. [Phase 7: Review & Quality Assurance](#phase-7-review--quality-assurance)
11. [Flutter Best Practices](#flutter-best-practices)
12. [Dart Best Practices](#dart-best-practices)
13. [Quick Reference](#quick-reference)

---

## Introduction

This manual adapts the standard AI Integration Manual for Dart development, covering both pure Dart packages and Flutter applications. Dart projects in the HiveLLM ecosystem should follow this guide for consistency and quality.

### Core Dart Principles

1. **Null Safety**: All code must be null-safe (Dart 3.0+)
2. **Strong Typing**: Leverage Dart's type system
3. **Immutability**: Prefer immutable data structures
4. **Async/Await**: Use for asynchronous operations
5. **Streams**: Use for reactive programming
6. **Package Structure**: Follow pub.dev conventions
7. **Cross-Platform**: Support multiple platforms when applicable

### Dart Ecosystem Tools

| Tool | Purpose |
|------|---------|
| **Dart SDK** | Core Dart compiler and runtime |
| **Flutter SDK** | UI framework (if building apps) |
| **pub** | Package manager |
| **dart analyze** | Static analysis |
| **dart format** | Code formatting |
| **dart test** | Testing framework |
| **dartdoc** | Documentation generator |
| **dart fix** | Automated code migration |
| **very_good_cli** | Project templates and tools |

---

## Dart-Specific Setup

### Step 1: Install Dart SDK

```bash
# macOS (Homebrew)
brew tap dart-lang/dart
brew install dart

# Windows (Chocolatey)
choco install dart-sdk

# Linux (apt)
sudo apt-get update
sudo apt-get install apt-transport-https
wget -qO- https://dl-ssl.google.com/linux/linux_signing_key.pub | sudo gpg --dearmor -o /usr/share/keyrings/dart.gpg
echo 'deb [signed-by=/usr/share/keyrings/dart.gpg arch=amd64] https://storage.googleapis.com/download.dartlang.org/linux/debian stable main' | sudo tee /etc/apt/sources.list.d/dart_stable.list
sudo apt-get update
sudo apt-get install dart

# Verify installation
dart --version
```

### Step 2: Install Flutter (if needed)

```bash
# Download Flutter SDK from https://flutter.dev/docs/get-started/install

# macOS/Linux - Add to PATH
export PATH="$PATH:`pwd`/flutter/bin"

# Windows - Add to PATH in System Environment Variables

# Verify installation
flutter --version
flutter doctor

# Accept Android licenses (if targeting Android)
flutter doctor --android-licenses
```

### Step 3: Install Development Tools

```bash
# Install Very Good CLI (optional but recommended)
dart pub global activate very_good_cli

# Install coverage tools
dart pub global activate coverage

# Install build_runner (for code generation)
dart pub global activate build_runner

# Install melos (for monorepo management)
dart pub global activate melos

# Verify installations
very_good --version
```

### Step 4: Configure IDE

#### VS Code

```bash
# Install extensions
code --install-extension Dart-Code.dart-code
code --install-extension Dart-Code.flutter

# Create .vscode/settings.json
{
  "dart.lineLength": 80,
  "dart.enableSdkFormatter": true,
  "dart.flutterSdkPath": "/path/to/flutter",
  "editor.formatOnSave": true,
  "editor.rulers": [80]
}
```

#### IntelliJ/Android Studio

1. Install Dart plugin
2. Install Flutter plugin
3. Configure SDK paths
4. Enable "Format on save"

---

## Project Structure

### Pure Dart Package Structure

```
package_name/
├── .github/
│   └── workflows/
│       ├── dart.yml
│       ├── coverage.yml
│       └── publish.yml
├── .dart_tool/              # Generated (ignored)
├── analysis_options.yaml    # Linter configuration
├── .gitignore
├── pubspec.yaml            # Package manifest
├── pubspec.lock            # Dependency lock file
├── README.md
├── CHANGELOG.md
├── LICENSE
├── lib/
│   ├── package_name.dart   # Main export file
│   ├── src/
│   │   ├── models/
│   │   ├── services/
│   │   ├── utils/
│   │   └── exceptions/
│   └── package_name_web.dart  # Web-specific (if needed)
├── test/
│   ├── src/
│   │   ├── models/
│   │   ├── services/
│   │   └── utils/
│   ├── fixtures/
│   └── package_name_test.dart
├── example/                # Usage examples
│   ├── main.dart
│   └── pubspec.yaml
├── docs/                   # HiveLLM documentation
│   ├── ROADMAP.md
│   ├── SPECS.md
│   └── specs/
├── tool/                   # Build scripts
│   └── build.dart
└── bin/                    # Executables (if CLI tool)
    └── package_name.dart
```

### Flutter App Structure

```
app_name/
├── .github/
│   └── workflows/
│       ├── flutter.yml
│       ├── android.yml
│       └── ios.yml
├── android/                # Android platform code
├── ios/                    # iOS platform code
├── web/                    # Web platform code
├── windows/                # Windows platform code
├── macos/                  # macOS platform code
├── linux/                  # Linux platform code
├── lib/
│   ├── main.dart           # App entry point
│   ├── app.dart            # App widget
│   ├── core/
│   │   ├── theme/
│   │   ├── routing/
│   │   ├── constants/
│   │   └── utils/
│   ├── features/
│   │   ├── home/
│   │   │   ├── domain/
│   │   │   │   ├── entities/
│   │   │   │   ├── repositories/
│   │   │   │   └── usecases/
│   │   │   ├── data/
│   │   │   │   ├── models/
│   │   │   │   ├── datasources/
│   │   │   │   └── repositories/
│   │   │   └── presentation/
│   │   │       ├── pages/
│   │   │       ├── widgets/
│   │   │       └── providers/
│   │   └── ...
│   ├── shared/
│   │   ├── widgets/
│   │   ├── models/
│   │   └── services/
│   └── l10n/               # Localization
├── test/
│   ├── unit/
│   ├── widget/
│   └── integration/
├── test_driver/            # Integration tests
├── assets/
│   ├── images/
│   ├── fonts/
│   └── translations/
├── docs/
│   ├── ROADMAP.md
│   ├── SPECS.md
│   └── specs/
├── analysis_options.yaml
├── pubspec.yaml
└── README.md
```

---

## Phase 1: Planning

### Step 1.1: Initialize Project

#### For Dart Package:

```bash
# Using Very Good CLI (recommended)
very_good create dart_pkg package_name

# Or using dart
dart create -t package package_name

# Or using Flutter
flutter create --template=package package_name

cd package_name
git init
git add .
git commit -m "chore: initial package setup"
```

#### For Flutter App:

```bash
# Using Very Good CLI (recommended)
very_good create flutter_app app_name --org com.example

# Or using Flutter
flutter create app_name --org com.example

cd app_name
git init
git add .
git commit -m "chore: initial Flutter app setup"
```

### Step 1.2: Configure pubspec.yaml

Edit `pubspec.yaml`:

```yaml
name: package_name
description: A brief description of what this package does.
version: 0.1.0
repository: https://github.com/username/package_name
issue_tracker: https://github.com/username/package_name/issues
documentation: https://pub.dev/documentation/package_name/latest/

environment:
  sdk: '>=3.0.0 <4.0.0'

dependencies:
  # Production dependencies
  meta: ^1.10.0
  
  # For async operations
  async: ^2.11.0
  
  # For HTTP requests
  http: ^1.1.0
  
  # For JSON serialization
  json_annotation: ^4.8.1

dev_dependencies:
  # Testing
  test: ^1.24.0
  mocktail: ^1.0.1
  
  # Code generation
  build_runner: ^2.4.6
  json_serializable: ^6.7.1
  
  # Linting
  very_good_analysis: ^5.1.0
  
  # Coverage
  coverage: ^1.6.4

# For Flutter apps, add:
# flutter:
#   uses-material-design: true
#   assets:
#     - assets/images/
```

### Step 1.3: Create ROADMAP.md

Create `docs/ROADMAP.md`:

```markdown
# Package Development Roadmap

**Package:** package_name  
**Version:** 0.1.0  
**Platforms:** All (VM, Web, Flutter)  
**Last Updated:** 2025-10-11

## Phase 1: Foundation (v0.1.0)

### 1.1 Package Setup
- [x] Initialize package structure
- [x] Configure pubspec.yaml
- [x] Setup analysis_options.yaml
- [x] Configure CI/CD workflows
- [ ] Create package documentation
- [ ] Setup dartdoc

### 1.2 Core Models
- [ ] 1.2.1 Define data models with null safety
- [ ] 1.2.2 Implement JSON serialization
- [ ] 1.2.3 Add copyWith methods
- [ ] 1.2.4 Write model tests

### 1.3 Core Services
- [ ] 1.3.1 Implement main service classes
- [ ] 1.3.2 Add error handling
- [ ] 1.3.3 Implement async operations
- [ ] 1.3.4 Write service tests

## Phase 2: Features (v0.2.0)

### 2.1 API Integration
- [ ] 2.1.1 HTTP client implementation
- [ ] 2.1.2 Request/response models
- [ ] 2.1.3 Error handling
- [ ] 2.1.4 Integration tests

### 2.2 State Management (Flutter)
- [ ] 2.2.1 Choose state management solution
- [ ] 2.2.2 Implement providers/blocs/riverpod
- [ ] 2.2.3 Add state tests
- [ ] 2.2.4 Widget tests

## Phase 3: Polish (v0.3.0)

### 3.1 Documentation
- [ ] 3.1.1 Complete API documentation
- [ ] 3.1.2 Write usage examples
- [ ] 3.1.3 Create README with examples
- [ ] 3.1.4 Add inline code documentation

### 3.2 Performance
- [ ] 3.2.1 Profile critical paths
- [ ] 3.2.2 Optimize bottlenecks
- [ ] 3.2.3 Reduce bundle size
- [ ] 3.2.4 Performance benchmarks

## Phase 4: Release (v1.0.0)

### 4.1 Final Review
- [ ] 4.1.1 Code review by 2+ Dart developers
- [ ] 4.1.2 Address all feedback
- [ ] 4.1.3 Cross-platform testing
- [ ] 4.1.4 pub.dev compliance check

### 4.2 Publication
- [ ] 4.2.1 Update version to 1.0.0
- [ ] 4.2.2 Update CHANGELOG
- [ ] 4.2.3 Publish to pub.dev
- [ ] 4.2.4 Announce release

## Status Legend
- [ ] Todo
- [~] In Progress  
- [x] Completed
- [!] Blocked

## Dependencies

| Package | Version | Purpose | Status |
|---------|---------|---------|--------|
| http | ^1.1.0 | HTTP requests | Required |
| json_annotation | ^4.8.1 | JSON serialization | Required |

## Last Updated: 2025-10-11
```

### Step 1.4: Generate SPECS.md

Create `docs/SPECS.md`:

```markdown
# Package Specifications

**Package:** package_name  
**Version:** 0.1.0  
**Platforms:** All Dart platforms

## Overview

Brief description of the package, its purpose, and target use cases.

## Target Audience

- Dart/Flutter developers working with [domain]
- Developers building [type of applications]
- Teams needing [specific functionality]

## Features

### Feature 1: [Name]
**Priority**: High  
**Status**: Planning  
**Description**: [2-3 sentence summary]  
**Platforms**: VM, Web, Flutter

### Feature 2: [Name]
**Priority**: Medium  
**Status**: Planning  
**Description**: [2-3 sentence summary]  
**Platforms**: Flutter only

## Technical Stack

- **Language**: Dart 3.0+
- **Flutter**: 3.13+ (if applicable)
- **Platforms**: iOS, Android, Web, Windows, macOS, Linux
- **Package Manager**: pub
- **Testing**: test package
- **Documentation**: dartdoc

## Architecture

- **Pattern**: Clean Architecture / Feature-First
- **State Management**: Provider / Riverpod / BLoC (Flutter)
- **Dependency Injection**: get_it / riverpod
- **Networking**: http / dio
- **Local Storage**: shared_preferences / hive / drift

## Dependencies

### Core
- dart:core, dart:async, dart:convert

### External
- http (>= 1.1.0) - HTTP requests
- json_annotation (>= 4.8.1) - JSON serialization

### Dev Dependencies
- test - Testing framework
- mocktail - Mocking
- build_runner - Code generation

## API Design Philosophy

- **Null Safety**: All APIs must be null-safe
- **Immutability**: Prefer immutable data structures
- **Async by Default**: Use Future/Stream for async operations
- **Documentation**: Document all public APIs
- **Examples**: Provide usage examples

## Performance Targets

- Package size: < 100KB
- Cold start: < 100ms
- Hot reload: < 500ms (Flutter)
- Memory usage: < 50MB baseline

## Compatibility

- Dart: >= 3.0.0 < 4.0.0
- Flutter: >= 3.13.0 (if applicable)
- Platforms: All supported Dart platforms
```

---

## Phase 2: Workspace Configuration

### Step 2.1: Configure analysis_options.yaml

Create `analysis_options.yaml`:

```yaml
# Analysis options for Dart
include: package:very_good_analysis/analysis_options.yaml

analyzer:
  exclude:
    - "**/*.g.dart"
    - "**/*.freezed.dart"
    - "build/**"
    - "lib/generated/**"
  
  language:
    strict-casts: true
    strict-inference: true
    strict-raw-types: true
  
  errors:
    # Treat all lints as errors
    invalid_annotation_target: ignore
    
  strong-mode:
    implicit-casts: false
    implicit-dynamic: false

linter:
  rules:
    # Style rules
    - always_declare_return_types
    - always_put_control_body_on_new_line
    - always_put_required_named_parameters_first
    - always_require_non_null_named_parameters
    - annotate_overrides
    - avoid_annotating_with_dynamic
    - avoid_bool_literals_in_conditional_expressions
    - avoid_catches_without_on_clauses
    - avoid_catching_errors
    - avoid_classes_with_only_static_members
    - avoid_double_and_int_checks
    - avoid_dynamic_calls
    - avoid_empty_else
    - avoid_equals_and_hash_code_on_mutable_classes
    - avoid_escaping_inner_quotes
    - avoid_field_initializers_in_const_classes
    - avoid_function_literals_in_foreach_calls
    - avoid_implementing_value_types
    - avoid_init_to_null
    - avoid_js_rounded_ints
    - avoid_multiple_declarations_per_line
    - avoid_null_checks_in_equality_operators
    - avoid_positional_boolean_parameters
    - avoid_print
    - avoid_private_typedef_functions
    - avoid_redundant_argument_values
    - avoid_relative_lib_imports
    - avoid_renaming_method_parameters
    - avoid_return_types_on_setters
    - avoid_returning_null_for_void
    - avoid_returning_this
    - avoid_setters_without_getters
    - avoid_shadowing_type_parameters
    - avoid_single_cascade_in_expression_statements
    - avoid_slow_async_io
    - avoid_type_to_string
    - avoid_types_as_parameter_names
    - avoid_types_on_closure_parameters
    - avoid_unnecessary_containers
    - avoid_unused_constructor_parameters
    - avoid_void_async
    - avoid_web_libraries_in_flutter
    - await_only_futures
    - camel_case_extensions
    - camel_case_types
    - cancel_subscriptions
    - cascade_invocations
    - cast_nullable_to_non_nullable
    - close_sinks
    - combinators_ordering
    - comment_references
    - conditional_uri_does_not_exist
    - constant_identifier_names
    - control_flow_in_finally
    - curly_braces_in_flow_control_structures
    - dangling_library_doc_comments
    - depend_on_referenced_packages
    - deprecated_consistency
    - directives_ordering
    - discarded_futures
    - do_not_use_environment
    - empty_catches
    - empty_constructor_bodies
    - empty_statements
    - eol_at_end_of_file
    - exhaustive_cases
    - file_names
    - flutter_style_todos
    - hash_and_equals
    - implementation_imports
    - implicit_call_tearoffs
    - implicit_reopen
    - invalid_case_patterns
    - join_return_with_assignment
    - leading_newlines_in_multiline_strings
    - library_annotations
    - library_names
    - library_prefixes
    - library_private_types_in_public_api
    - lines_longer_than_80_chars
    - literal_only_boolean_expressions
    - matching_super_parameters
    - missing_whitespace_between_adjacent_strings
    - no_adjacent_strings_in_list
    - no_default_cases
    - no_duplicate_case_values
    - no_leading_underscores_for_library_prefixes
    - no_leading_underscores_for_local_identifiers
    - no_literal_bool_comparisons
    - no_logic_in_create_state
    - no_runtimeType_toString
    - non_constant_identifier_names
    - noop_primitive_operations
    - null_check_on_nullable_type_parameter
    - null_closures
    - omit_local_variable_types
    - one_member_abstracts
    - only_throw_errors
    - overridden_fields
    - package_api_docs
    - package_names
    - package_prefixed_library_names
    - parameter_assignments
    - prefer_adjacent_string_concatenation
    - prefer_asserts_in_initializer_lists
    - prefer_asserts_with_message
    - prefer_collection_literals
    - prefer_conditional_assignment
    - prefer_const_constructors
    - prefer_const_constructors_in_immutables
    - prefer_const_declarations
    - prefer_const_literals_to_create_immutables
    - prefer_constructors_over_static_methods
    - prefer_contains
    - prefer_equal_for_default_values
    - prefer_expression_function_bodies
    - prefer_final_fields
    - prefer_final_in_for_each
    - prefer_final_locals
    - prefer_for_elements_to_map_fromIterable
    - prefer_foreach
    - prefer_function_declarations_over_variables
    - prefer_generic_function_type_aliases
    - prefer_if_elements_to_conditional_expressions
    - prefer_if_null_operators
    - prefer_initializing_formals
    - prefer_inlined_adds
    - prefer_int_literals
    - prefer_interpolation_to_compose_strings
    - prefer_is_empty
    - prefer_is_not_empty
    - prefer_is_not_operator
    - prefer_iterable_whereType
    - prefer_mixin
    - prefer_null_aware_method_calls
    - prefer_null_aware_operators
    - prefer_relative_imports
    - prefer_single_quotes
    - prefer_spread_collections
    - prefer_typing_uninitialized_variables
    - prefer_void_to_null
    - provide_deprecation_message
    - public_member_api_docs
    - recursive_getters
    - require_trailing_commas
    - secure_pubspec_urls
    - sized_box_for_whitespace
    - sized_box_shrink_expand
    - slash_for_doc_comments
    - sort_child_properties_last
    - sort_constructors_first
    - sort_pub_dependencies
    - sort_unnamed_constructors_first
    - test_types_in_equals
    - throw_in_finally
    - tighten_type_of_initializing_formals
    - type_annotate_public_apis
    - type_init_formals
    - type_literal_in_constant_pattern
    - unawaited_futures
    - unnecessary_await_in_return
    - unnecessary_brace_in_string_interps
    - unnecessary_const
    - unnecessary_constructor_name
    - unnecessary_getters_setters
    - unnecessary_lambdas
    - unnecessary_late
    - unnecessary_library_directive
    - unnecessary_new
    - unnecessary_null_aware_assignments
    - unnecessary_null_aware_operator_on_extension_on_nullable
    - unnecessary_null_checks
    - unnecessary_null_in_if_null_operators
    - unnecessary_nullable_for_final_variable_declarations
    - unnecessary_overrides
    - unnecessary_parenthesis
    - unnecessary_raw_strings
    - unnecessary_statements
    - unnecessary_string_escapes
    - unnecessary_string_interpolations
    - unnecessary_this
    - unnecessary_to_list_in_spreads
    - unrelated_type_equality_checks
    - use_build_context_synchronously
    - use_colored_box
    - use_decorated_box
    - use_enums
    - use_full_hex_values_for_flutter_colors
    - use_function_type_syntax_for_parameters
    - use_if_null_to_convert_nulls_to_bools
    - use_is_even_rather_than_modulo
    - use_key_in_widget_constructors
    - use_late_for_private_fields_and_variables
    - use_named_constants
    - use_raw_strings
    - use_rethrow_when_possible
    - use_setters_to_change_properties
    - use_string_buffers
    - use_string_in_part_of_directives
    - use_super_parameters
    - use_test_throws_matchers
    - use_to_and_as_if_applicable
    - valid_regexps
    - void_checks
```

### Step 2.2: Setup GitHub Actions

Create `.github/workflows/dart.yml`:

```yaml
name: Dart CI

on:
  push:
    branches: [main, develop]
  pull_request:
    branches: [main, develop]

jobs:
  analyze:
    name: Analyze
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - uses: dart-lang/setup-dart@v1
        with:
          sdk: stable
      
      - name: Install dependencies
        run: dart pub get
      
      - name: Verify formatting
        run: dart format --output=none --set-exit-if-changed .
      
      - name: Analyze project
        run: dart analyze --fatal-infos

  test:
    name: Test
    runs-on: ubuntu-latest
    strategy:
      matrix:
        sdk: [stable, beta]
    steps:
      - uses: actions/checkout@v4
      
      - uses: dart-lang/setup-dart@v1
        with:
          sdk: ${{ matrix.sdk }}
      
      - name: Install dependencies
        run: dart pub get
      
      - name: Run tests
        run: dart test --coverage=coverage
      
      - name: Generate coverage
        run: |
          dart pub global activate coverage
          dart pub global run coverage:format_coverage \
            --lcov \
            --in=coverage \
            --out=coverage/lcov.info \
            --packages=.dart_tool/package_config.json \
            --report-on=lib
      
      - name: Upload coverage
        uses: codecov/codecov-action@v3
        with:
          files: coverage/lcov.info

  pana:
    name: Package Analysis
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - uses: dart-lang/setup-dart@v1
      
      - name: Install dependencies
        run: dart pub get
      
      - name: Install pana
        run: dart pub global activate pana
      
      - name: Run pana
        run: dart pub global run pana --no-warning
```

---

## Phase 3: Implementation

### Step 3.1: Implementing Models

```dart
// lib/src/models/user.dart

import 'package:json_annotation/json_annotation.dart';
import 'package:meta/meta.dart';

part 'user.g.dart';

/// Represents a user in the system.
@immutable
@JsonSerializable()
class User {
  /// Creates a new [User].
  const User({
    required this.id,
    required this.name,
    required this.email,
    this.avatarUrl,
    required this.createdAt,
  });

  /// Creates a [User] from JSON.
  factory User.fromJson(Map<String, dynamic> json) => _$UserFromJson(json);

  /// The user's unique identifier.
  final String id;

  /// The user's display name.
  final String name;

  /// The user's email address.
  final String email;

  /// The URL of the user's avatar image.
  final String? avatarUrl;

  /// The date and time when the user was created.
  @JsonKey(name: 'created_at')
  final DateTime createdAt;

  /// Converts this [User] to JSON.
  Map<String, dynamic> toJson() => _$UserToJson(this);

  /// Creates a copy of this [User] with the given fields replaced.
  User copyWith({
    String? id,
    String? name,
    String? email,
    String? avatarUrl,
    DateTime? createdAt,
  }) {
    return User(
      id: id ?? this.id,
      name: name ?? this.name,
      email: email ?? this.email,
      avatarUrl: avatarUrl ?? this.avatarUrl,
      createdAt: createdAt ?? this.createdAt,
    );
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is User &&
          runtimeType == other.runtimeType &&
          id == other.id &&
          name == other.name &&
          email == other.email &&
          avatarUrl == other.avatarUrl &&
          createdAt == other.createdAt;

  @override
  int get hashCode =>
      id.hashCode ^
      name.hashCode ^
      email.hashCode ^
      avatarUrl.hashCode ^
      createdAt.hashCode;

  @override
  String toString() {
    return 'User(id: $id, name: $name, email: $email, '
        'avatarUrl: $avatarUrl, createdAt: $createdAt)';
  }
}

// Generate code with: dart run build_runner build
```

### Step 3.2: Implementing Services

```dart
// lib/src/services/user_service.dart

import 'dart:convert';

import 'package:http/http.dart' as http;
import 'package:meta/meta.dart';

import '../models/user.dart';
import '../exceptions/api_exception.dart';

/// A service for managing users.
@immutable
class UserService {
  /// Creates a new [UserService].
  const UserService({
    required this.baseUrl,
    http.Client? httpClient,
  }) : _httpClient = httpClient;

  /// The base URL for the API.
  final String baseUrl;

  final http.Client? _httpClient;

  http.Client get _client => _httpClient ?? http.Client();

  /// Fetches a user by ID.
  ///
  /// Throws an [ApiException] if the request fails.
  Future<User> getUser(String id) async {
    final uri = Uri.parse('$baseUrl/users/$id');

    try {
      final response = await _client.get(uri);

      if (response.statusCode == 200) {
        final json = jsonDecode(response.body) as Map<String, dynamic>;
        return User.fromJson(json);
      } else if (response.statusCode == 404) {
        throw ApiException.notFound('User not found');
      } else {
        throw ApiException.server(
          'Server error: ${response.statusCode}',
          statusCode: response.statusCode,
        );
      }
    } on http.ClientException catch (e) {
      throw ApiException.network('Network error: ${e.message}');
    } on FormatException {
      throw ApiException.parse('Failed to parse response');
    }
  }

  /// Fetches all users.
  ///
  /// Returns a list of [User] objects.
  Future<List<User>> getUsers() async {
    final uri = Uri.parse('$baseUrl/users');

    try {
      final response = await _client.get(uri);

      if (response.statusCode == 200) {
        final List<dynamic> json = jsonDecode(response.body) as List;
        return json
            .map((e) => User.fromJson(e as Map<String, dynamic>))
            .toList();
      } else {
        throw ApiException.server(
          'Server error: ${response.statusCode}',
          statusCode: response.statusCode,
        );
      }
    } on http.ClientException catch (e) {
      throw ApiException.network('Network error: ${e.message}');
    } on FormatException {
      throw ApiException.parse('Failed to parse response');
    }
  }

  /// Creates a new user.
  ///
  /// Returns the created [User].
  Future<User> createUser({
    required String name,
    required String email,
  }) async {
    final uri = Uri.parse('$baseUrl/users');

    final body = jsonEncode({
      'name': name,
      'email': email,
    });

    try {
      final response = await _client.post(
        uri,
        headers: {'Content-Type': 'application/json'},
        body: body,
      );

      if (response.statusCode == 201) {
        final json = jsonDecode(response.body) as Map<String, dynamic>;
        return User.fromJson(json);
      } else {
        throw ApiException.server(
          'Server error: ${response.statusCode}',
          statusCode: response.statusCode,
        );
      }
    } on http.ClientException catch (e) {
      throw ApiException.network('Network error: ${e.message}');
    } on FormatException {
      throw ApiException.parse('Failed to parse response');
    }
  }

  /// Disposes resources used by this service.
  void dispose() {
    _httpClient?.close();
  }
}
```

### Step 3.3: Exception Handling

```dart
// lib/src/exceptions/api_exception.dart

/// An exception thrown when an API request fails.
class ApiException implements Exception {
  /// Creates an [ApiException].
  const ApiException({
    required this.message,
    this.statusCode,
    this.cause,
  });

  /// Creates an [ApiException] for network errors.
  const ApiException.network(this.message)
      : statusCode = null,
        cause = null;

  /// Creates an [ApiException] for server errors.
  const ApiException.server(this.message, {required this.statusCode})
      : cause = null;

  /// Creates an [ApiException] for parsing errors.
  const ApiException.parse(this.message)
      : statusCode = null,
        cause = null;

  /// Creates an [ApiException] for not found errors.
  const ApiException.notFound(this.message)
      : statusCode = 404,
        cause = null;

  /// The error message.
  final String message;

  /// The HTTP status code, if applicable.
  final int? statusCode;

  /// The underlying cause of the exception.
  final Object? cause;

  @override
  String toString() {
    final buffer = StringBuffer('ApiException: $message');
    if (statusCode != null) {
      buffer.write(' (status code: $statusCode)');
    }
    if (cause != null) {
      buffer.write('\nCaused by: $cause');
    }
    return buffer.toString();
  }
}
```

### Step 3.4: Streams and Async

```dart
// lib/src/services/realtime_service.dart

import 'dart:async';

import '../models/user.dart';

/// A service for real-time user updates.
class RealtimeService {
  /// Creates a new [RealtimeService].
  RealtimeService() : _controller = StreamController<User>.broadcast();

  final StreamController<User> _controller;

  /// A stream of user updates.
  Stream<User> get userUpdates => _controller.stream;

  /// Simulates receiving a user update.
  void _onUserUpdate(User user) {
    if (!_controller.isClosed) {
      _controller.add(user);
    }
  }

  /// Disposes resources used by this service.
  Future<void> dispose() async {
    await _controller.close();
  }
}

// Usage with StreamBuilder in Flutter:
// StreamBuilder<User>(
//   stream: realtimeService.userUpdates,
//   builder: (context, snapshot) {
//     if (snapshot.hasData) {
//       return UserWidget(user: snapshot.data!);
//     }
//     return LoadingWidget();
//   },
// )
```

---

## Phase 4: Testing

### Step 4.1: Unit Tests

```dart
// test/src/models/user_test.dart

import 'package:package_name/src/models/user.dart';
import 'package:test/test.dart';

void main() {
  group('User', () {
    test('can be instantiated', () {
      final user = User(
        id: '1',
        name: 'John Doe',
        email: 'john@example.com',
        createdAt: DateTime(2025),
      );

      expect(user.id, '1');
      expect(user.name, 'John Doe');
      expect(user.email, 'john@example.com');
      expect(user.avatarUrl, isNull);
      expect(user.createdAt, DateTime(2025));
    });

    test('fromJson creates correct User', () {
      final json = {
        'id': '1',
        'name': 'John Doe',
        'email': 'john@example.com',
        'avatar_url': 'https://example.com/avatar.jpg',
        'created_at': '2025-01-01T00:00:00.000Z',
      };

      final user = User.fromJson(json);

      expect(user.id, '1');
      expect(user.name, 'John Doe');
      expect(user.email, 'john@example.com');
      expect(user.avatarUrl, 'https://example.com/avatar.jpg');
    });

    test('toJson creates correct JSON', () {
      final user = User(
        id: '1',
        name: 'John Doe',
        email: 'john@example.com',
        createdAt: DateTime(2025),
      );

      final json = user.toJson();

      expect(json['id'], '1');
      expect(json['name'], 'John Doe');
      expect(json['email'], 'john@example.com');
    });

    test('copyWith creates correct copy', () {
      final user = User(
        id: '1',
        name: 'John Doe',
        email: 'john@example.com',
        createdAt: DateTime(2025),
      );

      final copy = user.copyWith(name: 'Jane Doe');

      expect(copy.id, user.id);
      expect(copy.name, 'Jane Doe');
      expect(copy.email, user.email);
    });

    test('equality works correctly', () {
      final user1 = User(
        id: '1',
        name: 'John Doe',
        email: 'john@example.com',
        createdAt: DateTime(2025),
      );

      final user2 = User(
        id: '1',
        name: 'John Doe',
        email: 'john@example.com',
        createdAt: DateTime(2025),
      );

      final user3 = User(
        id: '2',
        name: 'Jane Doe',
        email: 'jane@example.com',
        createdAt: DateTime(2025),
      );

      expect(user1, equals(user2));
      expect(user1, isNot(equals(user3)));
    });
  });
}
```

### Step 4.2: Service Tests with Mocks

```dart
// test/src/services/user_service_test.dart

import 'package:http/http.dart' as http;
import 'package:mocktail/mocktail.dart';
import 'package:package_name/src/exceptions/api_exception.dart';
import 'package:package_name/src/models/user.dart';
import 'package:package_name/src/services/user_service.dart';
import 'package:test/test.dart';

class MockHttpClient extends Mock implements http.Client {}

void main() {
  group('UserService', () {
    late http.Client httpClient;
    late UserService userService;

    setUp(() {
      httpClient = MockHttpClient();
      userService = UserService(
        baseUrl: 'https://api.example.com',
        httpClient: httpClient,
      );
    });

    setUpAll(() {
      registerFallbackValue(Uri());
    });

    group('getUser', () {
      test('returns User on successful response', () async {
        // Arrange
        final json = '''
        {
          "id": "1",
          "name": "John Doe",
          "email": "john@example.com",
          "created_at": "2025-01-01T00:00:00.000Z"
        }
        ''';

        when(() => httpClient.get(any())).thenAnswer(
          (_) async => http.Response(json, 200),
        );

        // Act
        final user = await userService.getUser('1');

        // Assert
        expect(user.id, '1');
        expect(user.name, 'John Doe');
        verify(() => httpClient.get(any())).called(1);
      });

      test('throws ApiException.notFound when user not found', () async {
        // Arrange
        when(() => httpClient.get(any())).thenAnswer(
          (_) async => http.Response('Not found', 404),
        );

        // Act & Assert
        expect(
          () => userService.getUser('1'),
          throwsA(isA<ApiException>()),
        );
      });

      test('throws ApiException.server on server error', () async {
        // Arrange
        when(() => httpClient.get(any())).thenAnswer(
          (_) async => http.Response('Server error', 500),
        );

        // Act & Assert
        expect(
          () => userService.getUser('1'),
          throwsA(
            isA<ApiException>().having(
              (e) => e.statusCode,
              'statusCode',
              500,
            ),
          ),
        );
      });

      test('throws ApiException.network on network error', () async {
        // Arrange
        when(() => httpClient.get(any())).thenThrow(
          http.ClientException('Network error'),
        );

        // Act & Assert
        expect(
          () => userService.getUser('1'),
          throwsA(isA<ApiException>()),
        );
      });
    });
  });
}
```

### Step 4.3: Flutter Widget Tests

```dart
// test/widgets/user_card_test.dart

import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:package_name/src/models/user.dart';
import 'package:package_name/src/widgets/user_card.dart';

void main() {
  group('UserCard', () {
    testWidgets('displays user information', (tester) async {
      // Arrange
      final user = User(
        id: '1',
        name: 'John Doe',
        email: 'john@example.com',
        createdAt: DateTime(2025),
      );

      // Act
      await tester.pumpWidget(
        MaterialApp(
          home: Scaffold(
            body: UserCard(user: user),
          ),
        ),
      );

      // Assert
      expect(find.text('John Doe'), findsOneWidget);
      expect(find.text('john@example.com'), findsOneWidget);
    });

    testWidgets('displays avatar when available', (tester) async {
      // Arrange
      final user = User(
        id: '1',
        name: 'John Doe',
        email: 'john@example.com',
        avatarUrl: 'https://example.com/avatar.jpg',
        createdAt: DateTime(2025),
      );

      // Act
      await tester.pumpWidget(
        MaterialApp(
          home: Scaffold(
            body: UserCard(user: user),
          ),
        ),
      );

      // Assert
      expect(find.byType(CircleAvatar), findsOneWidget);
    });

    testWidgets('calls onTap when tapped', (tester) async {
      // Arrange
      var tapped = false;
      final user = User(
        id: '1',
        name: 'John Doe',
        email: 'john@example.com',
        createdAt: DateTime(2025),
      );

      // Act
      await tester.pumpWidget(
        MaterialApp(
          home: Scaffold(
            body: UserCard(
              user: user,
              onTap: () => tapped = true,
            ),
          ),
        ),
      );

      await tester.tap(find.byType(UserCard));

      // Assert
      expect(tapped, isTrue);
    });
  });
}
```

### Step 4.4: Running Tests

```bash
# Run all tests
dart test

# Run specific test file
dart test test/src/models/user_test.dart

# Run with coverage
dart test --coverage=coverage

# Generate coverage report
dart pub global run coverage:format_coverage \
  --lcov \
  --in=coverage \
  --out=coverage/lcov.info \
  --packages=.dart_tool/package_config.json \
  --report-on=lib

# View coverage in browser (requires lcov)
genhtml coverage/lcov.info -o coverage/html
open coverage/html/index.html
```

---

## Phase 5: Documentation

### Step 5.1: Inline Documentation

```dart
/// A library for managing users.
///
/// This library provides models and services for user management,
/// including CRUD operations and real-time updates.
///
/// Example:
/// ```dart
/// final service = UserService(baseUrl: 'https://api.example.com');
/// final user = await service.getUser('123');
/// print(user.name);
/// ```
library package_name;

export 'src/models/user.dart';
export 'src/services/user_service.dart';
export 'src/exceptions/api_exception.dart';
```

### Step 5.2: Generate Documentation

```bash
# Generate dartdoc documentation
dart doc

# Output is in doc/api/

# Preview locally
python3 -m http.server 8000 -d doc/api
# Open http://localhost:8000
```

### Step 5.3: README Example

Create comprehensive README:

```markdown
# package_name

[![Dart](https://github.com/username/package_name/workflows/Dart/badge.svg)](https://github.com/username/package_name/actions)
[![pub package](https://img.shields.io/pub/v/package_name.svg)](https://pub.dev/packages/package_name)
[![style: very good analysis](https://img.shields.io/badge/style-very_good_analysis-B22C89.svg)](https://pub.dev/packages/very_good_analysis)

A brief description of your package.

## Features

- ✨ Feature 1
- ✨ Feature 2
- ✨ Feature 3

## Installation

Add this package to your `pubspec.yaml`:

```yaml
dependencies:
  package_name: ^1.0.0
```

Then run:

```bash
dart pub get
```

## Usage

```dart
import 'package:package_name/package_name.dart';

void main() async {
  final service = UserService(baseUrl: 'https://api.example.com');
  
  try {
    final user = await service.getUser('123');
    print('User: ${user.name}');
  } on ApiException catch (e) {
    print('Error: ${e.message}');
  }
}
```

## Documentation

For detailed documentation, see [pub.dev/documentation](https://pub.dev/documentation/package_name/latest/).

## Contributing

Contributions are welcome! Please read our [contributing guidelines](CONTRIBUTING.md).

## License

MIT License - see [LICENSE](LICENSE) for details.
```

---

## Phase 6: Package Building & Distribution

### Step 6.1: Prepare for Publishing

```bash
# Validate package
dart pub publish --dry-run

# Check package score
dart pub global activate pana
dart pub global run pana

# Ensure proper version
# Update version in pubspec.yaml following semantic versioning
```

### Step 6.2: Publish to pub.dev

```bash
# First time: authenticate
dart pub login

# Publish package
dart pub publish

# Package will be available at:
# https://pub.dev/packages/package_name
```

### Step 6.3: Version Management

```yaml
# pubspec.yaml
version: 1.0.0+1
# Format: MAJOR.MINOR.PATCH+BUILD

# Breaking changes: 1.0.0 -> 2.0.0
# New features: 1.0.0 -> 1.1.0
# Bug fixes: 1.0.0 -> 1.0.1
```

---

## Phase 7: Review & Quality Assurance

### Pre-Publication Checklist

```markdown
# Pre-Publication Checklist

**Package:** package_name  
**Version:** 1.0.0  
**Date:** 2025-10-11

## Code Quality

- [ ] dart analyze passes with no issues
- [ ] dart format applied
- [ ] No TODO comments in production code
- [ ] Null safety enabled
- [ ] Strong mode enabled

## Testing

- [ ] All tests pass
- [ ] Test coverage > 90%
- [ ] Widget tests (if Flutter package)
- [ ] Integration tests (if applicable)
- [ ] Golden tests (if UI package)

## Documentation

- [ ] All public APIs documented
- [ ] README is comprehensive
- [ ] CHANGELOG updated
- [ ] Example code works
- [ ] dartdoc builds without warnings

## Package Configuration

- [ ] pubspec.yaml is correct
- [ ] version follows semver
- [ ] repository field set
- [ ] issue_tracker field set
- [ ] homepage/documentation set
- [ ] LICENSE file present

## pub.dev Score

- [ ] dart pub publish --dry-run passes
- [ ] pana score > 130
- [ ] No suggestions or errors
- [ ] Example included
- [ ] Platform support declared

## Compatibility

- [ ] Dart SDK constraints correct
- [ ] Flutter SDK constraints (if applicable)
- [ ] Platform compatibility declared
- [ ] Works on all declared platforms
```

---

## Flutter Best Practices

### Widget Composition

```dart
// Good: Small, focused widgets
class UserAvatar extends StatelessWidget {
  const UserAvatar({
    required this.imageUrl,
    this.size = 40,
    super.key,
  });

  final String? imageUrl;
  final double size;

  @override
  Widget build(BuildContext context) {
    return CircleAvatar(
      radius: size / 2,
      backgroundImage: imageUrl != null
          ? NetworkImage(imageUrl!)
          : null,
      child: imageUrl == null
          ? Icon(Icons.person, size: size * 0.6)
          : null,
    );
  }
}
```

### State Management (Provider Example)

```dart
// Using Provider
class UserProvider extends ChangeNotifier {
  User? _user;
  bool _isLoading = false;
  String? _error;

  User? get user => _user;
  bool get isLoading => _isLoading;
  String? get error => _error;

  Future<void> loadUser(String id) async {
    _isLoading = true;
    _error = null;
    notifyListeners();

    try {
      _user = await userService.getUser(id);
      _error = null;
    } catch (e) {
      _error = e.toString();
    } finally {
      _isLoading = false;
      notifyListeners();
    }
  }
}

// Usage
Consumer<UserProvider>(
  builder: (context, provider, child) {
    if (provider.isLoading) {
      return const CircularProgressIndicator();
    }
    
    if (provider.error != null) {
      return Text('Error: ${provider.error}');
    }
    
    return UserCard(user: provider.user!);
  },
)
```

---

## Dart Best Practices

See `BEST_PRACTICES.md` for complete guidelines.

---

## Quick Reference

### Essential Commands

```bash
# Project creation
dart create -t package my_package
flutter create my_app

# Dependencies
dart pub get
dart pub upgrade
dart pub outdated

# Code generation
dart run build_runner build
dart run build_runner watch

# Formatting and analysis
dart format .
dart analyze
dart fix --apply

# Testing
dart test
dart test --coverage=coverage

# Documentation
dart doc

# Publishing
dart pub publish --dry-run
dart pub publish
```

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-10-11 | Initial Dart manual creation |

---

## Resources

- [Dart Language Tour](https://dart.dev/guides/language/language-tour)
- [Effective Dart](https://dart.dev/guides/language/effective-dart)
- [Flutter Documentation](https://flutter.dev/docs)
- [pub.dev](https://pub.dev/)
- [Dart API Reference](https://api.dart.dev/)

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

