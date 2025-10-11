# Dart Best Practices Guide

**Version:** 1.0.0  
**Last Updated:** 2025-10-11  
**Target Audience:** AI Agents developing Dart applications  
**Compliance:** Effective Dart, Flutter Style Guide

---

## Table of Contents

1. [Code Style](#code-style)
2. [Naming Conventions](#naming-conventions)
3. [Null Safety](#null-safety)
4. [Type System](#type-system)
5. [Collections](#collections)
6. [Functions](#functions)
7. [Classes and Objects](#classes-and-objects)
8. [Asynchronous Programming](#asynchronous-programming)
9. [Error Handling](#error-handling)
10. [Flutter Specific](#flutter-specific)
11. [Performance](#performance)
12. [Security](#security)
13. [Testing](#testing)
14. [Anti-Patterns](#anti-patterns)

---

## Code Style

### File Organization

```dart
// 1. License header (if applicable)
// Copyright (c) 2025...

// 2. Library documentation
/// A library for user management.
library user_management;

// 3. Imports - sorted alphabetically
// Dart SDK imports
import 'dart:async';
import 'dart:convert';

// Package imports
import 'package:http/http.dart' as http;
import 'package:meta/meta.dart';

// Relative imports
import '../models/user.dart';
import '../services/api_client.dart';

// 4. Parts (if used)
part 'user_service.g.dart';

// 5. Code
class UserService {
  // Implementation
}
```

### Formatting

```dart
// Good: Use dart format
class User {
  const User({
    required this.id,
    required this.name,
    this.email,
  });

  final String id;
  final String name;
  final String? email;
}

// Good: Trailing commas
Widget build(BuildContext context) {
  return Column(
    children: [
      Text('Hello'),
      Text('World'),
    ], // Trailing comma helps formatting
  );
}

// Good: Line length 80 characters
final message = 
    'This is a very long message that needs to be split '
    'across multiple lines for readability.';
```

---

## Naming Conventions

### Classes and Types

```dart
// PascalCase for classes, enums, typedefs, type parameters
class UserProfile {}
enum UserRole { admin, user, guest }
typedef Predicate<T> = bool Function(T value);

// Good: Descriptive names
class HttpClient {}
class ApiException {}

// Bad: Abbreviations
class UsrProf {}  // Too abbreviated
class HTTPClient {}  // Wrong casing
```

### Libraries and Files

```dart
// snake_case for library names and file names
library user_management;

// Files:
// user_service.dart
// api_client.dart
// user_profile.dart

// Bad:
// UserService.dart
// apiClient.dart
// user-profile.dart
```

### Variables and Methods

```dart
// camelCase for variables, functions, parameters
var userName = 'John';
void fetchUser() {}
void processData({required String userId}) {}

// Good: Boolean names read like predicates
bool isLoading = false;
bool hasData = true;
bool canEdit = false;

// Bad: Boolean names
bool loading = false;  // Unclear
bool data = true;  // Not a predicate
```

### Constants

```dart
// lowerCamelCase for constants (Dart style, not SCREAMING_SNAKE_CASE)
const defaultTimeout = Duration(seconds: 30);
const maxRetries = 3;
const apiBaseUrl = 'https://api.example.com';

// Exception: Enum values use lowerCamelCase
enum HttpMethod {
  get,
  post,
  put,
  delete,
}
```

---

## Null Safety

### Non-nullable by Default

```dart
// Good: Non-nullable by default
String name = 'John';
int age = 30;
User user = User(id: '1', name: 'John');

// Good: Explicit nullable when needed
String? nickname;
int? optionalAge;
User? maybeUser;

// Bad: Unnecessary nullable
String? name = 'John';  // Should be non-nullable
```

### Null-Aware Operators

```dart
// Good: Null-aware access
final name = user?.name;

// Good: Null-aware assignment
String? name;
name ??= 'Unknown';

// Good: Null-coalescing
final displayName = user?.name ?? 'Anonymous';

// Good: Null-aware method calls
user?.save();
```

### Late Variables

```dart
// Good: Late for lazy initialization
class DatabaseManager {
  late Database _db;
  
  Future<void> init() async {
    _db = await openDatabase('app.db');
  }
  
  void query() {
    _db.query('SELECT * FROM users');  // Safe after init
  }
}

// Good: Late final for computed values
class Config {
  late final String computedValue = _computeValue();
  
  String _computeValue() {
    // Expensive computation
    return 'result';
  }
}

// Bad: Overusing late
late String name;  // Better: String? name
```

### Required Parameters

```dart
// Good: Required named parameters
class User {
  const User({
    required this.id,
    required this.name,
    this.email,  // Optional
  });

  final String id;
  final String name;
  final String? email;
}

// Good: Required in methods
void updateUser({
  required String id,
  required String name,
  String? email,
}) {
  // Implementation
}
```

---

## Type System

### Type Annotations

```dart
// Good: Annotate public APIs
class UserService {
  Future<User> getUser(String id) async {
    // Implementation
  }
  
  List<User> getUsers() {
    // Implementation
  }
}

// Good: Infer local variables
void process() {
  var count = 0;  // Type inferred as int
  var users = <User>[];  // Type inferred as List<User>
  var name = getName();  // Type inferred from return type
}

// Bad: Redundant annotations
final int count = 0;  // Obvious from literal
final List<User> users = <User>[];  // Redundant
```

### Generics

```dart
// Good: Use generics for reusable code
class Result<T> {
  const Result.success(this.data) : error = null;
  const Result.failure(this.error) : data = null;
  
  final T? data;
  final String? error;
}

// Good: Bounded generics
T? findMax<T extends Comparable<T>>(List<T> items) {
  if (items.isEmpty) return null;
  return items.reduce((a, b) => a.compareTo(b) > 0 ? a : b);
}

// Good: Generic constraints
class Repository<T extends Entity> {
  Future<T> find(String id);
  Future<List<T>> findAll();
}
```

### Avoid Dynamic

```dart
// Bad: Using dynamic
dynamic processData(dynamic input) {
  return input;
}

// Good: Use specific types
Object processData(Object input) {
  return input;
}

// Better: Use generics
T processData<T>(T input) {
  return input;
}
```

---

## Collections

### Literals

```dart
// Good: Use collection literals
final list = <String>[];
final map = <String, int>{};
final set = <String>{};

// Bad: Constructors
final list = List<String>();  // Avoid
final map = Map<String, int>();  // Avoid
final set = Set<String>();  // Avoid
```

### Collection Operations

```dart
// Good: Use collection methods
final users = await fetchUsers();

final activeUsers = users.where((u) => u.isActive);
final names = users.map((u) => u.name);
final hasAdmin = users.any((u) => u.role == 'admin');
final allActive = users.every((u) => u.isActive);

// Good: Cascades for chaining
final users = <User>[]
  ..add(user1)
  ..add(user2)
  ..sort((a, b) => a.name.compareTo(b.name));

// Good: Spread operator
final allUsers = [
  ...activeUsers,
  ...inactiveUsers,
];

// Good: Collection if/for
final users = [
  if (includeAdmin) adminUser,
  for (var user in regularUsers) user,
];
```

### Immutable Collections

```dart
// Good: Use const for immutable collections
const supportedFormats = ['jpg', 'png', 'gif'];
const defaultConfig = {
  'timeout': 30,
  'retries': 3,
};

// Good: Use List.unmodifiable for runtime immutability
class UserGroup {
  UserGroup(List<User> users)
      : _users = List.unmodifiable(users);
  
  final List<User> _users;
  
  List<User> get users => _users;
}
```

---

## Functions

### Function Syntax

```dart
// Good: Expression body for simple functions
int add(int a, int b) => a + b;
String getName() => 'John';

// Good: Block body for complex functions
int calculate(int a, int b) {
  final sum = a + b;
  final product = a * b;
  return sum * product;
}

// Good: Named parameters
void createUser({
  required String name,
  required String email,
  int age = 18,
  String? nickname,
}) {
  // Implementation
}
```

### Higher-Order Functions

```dart
// Good: Functions as parameters
void processUsers(
  List<User> users,
  bool Function(User) predicate,
) {
  for (final user in users) {
    if (predicate(user)) {
      print(user.name);
    }
  }
}

// Usage
processUsers(users, (user) => user.isActive);

// Good: Returning functions
bool Function(User) createAgePredicate(int minAge) {
  return (user) => user.age >= minAge;
}

// Usage
final adults = users.where(createAgePredicate(18));
```

### Extension Methods

```dart
// Good: Extension methods for adding functionality
extension StringExtensions on String {
  bool get isEmail {
    final emailRegex = RegExp(r'^[\w-\.]+@([\w-]+\.)+[\w-]{2,4}$');
    return emailRegex.hasMatch(this);
  }
  
  String capitalize() {
    if (isEmpty) return this;
    return '${this[0].toUpperCase()}${substring(1)}';
  }
}

// Usage
final email = 'john@example.com';
if (email.isEmail) {
  print('Valid email');
}

final name = 'john'.capitalize();  // 'John'
```

---

## Classes and Objects

### Constructors

```dart
// Good: Const constructors for immutable classes
class Point {
  const Point(this.x, this.y);
  
  final double x;
  final double y;
}

// Good: Named constructors
class User {
  const User({
    required this.id,
    required this.name,
  });
  
  const User.guest()
      : id = 'guest',
        name = 'Guest User';
  
  factory User.fromJson(Map<String, dynamic> json) {
    return User(
      id: json['id'] as String,
      name: json['name'] as String,
    );
  }
  
  final String id;
  final String name;
}
```

### Getters and Setters

```dart
// Good: Use getters for computed properties
class Rectangle {
  const Rectangle(this.width, this.height);
  
  final double width;
  final double height;
  
  double get area => width * height;
  double get perimeter => 2 * (width + height);
}

// Good: Use setters sparingly (prefer immutability)
class Counter {
  Counter(this._count);
  
  int _count;
  
  int get count => _count;
  
  set count(int value) {
    if (value < 0) {
      throw ArgumentError('Count must be non-negative');
    }
    _count = value;
  }
}
```

### Immutability

```dart
// Good: Immutable classes
@immutable
class User {
  const User({
    required this.id,
    required this.name,
    this.email,
  });
  
  final String id;
  final String name;
  final String? email;
  
  // copyWith for creating modified copies
  User copyWith({
    String? id,
    String? name,
    String? email,
  }) {
    return User(
      id: id ?? this.id,
      name: name ?? this.name,
      email: email ?? this.email,
    );
  }
  
  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is User &&
          runtimeType == other.runtimeType &&
          id == other.id &&
          name == other.name &&
          email == other.email;
  
  @override
  int get hashCode => Object.hash(id, name, email);
}
```

### Mixins

```dart
// Good: Use mixins for shared behavior
mixin LoggerMixin {
  void log(String message) {
    print('[${DateTime.now()}] $message');
  }
}

mixin ValidationMixin {
  bool validate(String input) {
    return input.isNotEmpty;
  }
}

class UserService with LoggerMixin, ValidationMixin {
  void createUser(String name) {
    if (!validate(name)) {
      log('Invalid name');
      return;
    }
    log('Creating user: $name');
    // Create user
  }
}
```

---

## Asynchronous Programming

### Async/Await

```dart
// Good: Use async/await
Future<User> fetchUser(String id) async {
  final response = await http.get(Uri.parse('$baseUrl/users/$id'));
  if (response.statusCode == 200) {
    return User.fromJson(jsonDecode(response.body));
  }
  throw Exception('Failed to fetch user');
}

// Good: Handle errors
Future<User?> fetchUserSafe(String id) async {
  try {
    return await fetchUser(id);
  } catch (e) {
    print('Error: $e');
    return null;
  }
}

// Good: Parallel async operations
Future<void> loadData() async {
  final results = await Future.wait([
    fetchUsers(),
    fetchPosts(),
    fetchComments(),
  ]);
  
  final users = results[0] as List<User>;
  final posts = results[1] as List<Post>;
  final comments = results[2] as List<Comment>;
}
```

### Streams

```dart
// Good: Use streams for multiple values
Stream<int> countStream() async* {
  for (var i = 0; i < 10; i++) {
    await Future.delayed(Duration(seconds: 1));
    yield i;
  }
}

// Good: Transform streams
Stream<String> getUserNames() {
  return userStream.map((user) => user.name);
}

// Good: Listen to streams
void listenToUsers() {
  userStream.listen(
    (user) {
      print('User: ${user.name}');
    },
    onError: (error) {
      print('Error: $error');
    },
    onDone: () {
      print('Stream closed');
    },
  );
}

// Good: StreamController for custom streams
class UserRepository {
  final _controller = StreamController<User>.broadcast();
  
  Stream<User> get userUpdates => _controller.stream;
  
  void updateUser(User user) {
    _controller.add(user);
  }
  
  void dispose() {
    _controller.close();
  }
}
```

### FutureOr

```dart
// Good: FutureOr for flexible return types
FutureOr<User> getUser(String id, {bool useCache = false}) {
  if (useCache && _cache.containsKey(id)) {
    return _cache[id]!;  // Synchronous
  }
  return _fetchUser(id);  // Asynchronous
}
```

---

## Error Handling

### Exceptions

```dart
// Good: Custom exceptions
class ApiException implements Exception {
  const ApiException(this.message, {this.statusCode});
  
  final String message;
  final int? statusCode;
  
  @override
  String toString() => 'ApiException: $message${statusCode != null ? ' ($statusCode)' : ''}';
}

// Good: Throw appropriate exceptions
Future<User> fetchUser(String id) async {
  if (id.isEmpty) {
    throw ArgumentError('User ID cannot be empty');
  }
  
  final response = await http.get(Uri.parse('$baseUrl/users/$id'));
  
  if (response.statusCode == 404) {
    throw ApiException('User not found', statusCode: 404);
  }
  
  if (response.statusCode != 200) {
    throw ApiException(
      'Server error',
      statusCode: response.statusCode,
    );
  }
  
  try {
    return User.fromJson(jsonDecode(response.body));
  } on FormatException {
    throw ApiException('Invalid response format');
  }
}
```

### Try-Catch

```dart
// Good: Specific catch blocks
Future<void> processUser(String id) async {
  try {
    final user = await fetchUser(id);
    await saveUser(user);
  } on ApiException catch (e) {
    print('API error: ${e.message}');
  } on FormatException catch (e) {
    print('Format error: $e');
  } catch (e) {
    print('Unknown error: $e');
  } finally {
    // Cleanup
  }
}

// Good: Rethrow when needed
Future<User> fetchUserWithRetry(String id) async {
  try {
    return await fetchUser(id);
  } catch (e) {
    print('First attempt failed, retrying...');
    rethrow;  // Preserve stack trace
  }
}
```

---

## Flutter Specific

### Widget Composition

```dart
// Good: Extract widgets
class UserProfile extends StatelessWidget {
  const UserProfile({required this.user, super.key});
  
  final User user;
  
  @override
  Widget build(BuildContext context) {
    return Column(
      children: [
        _UserHeader(user: user),
        _UserDetails(user: user),
        _UserActions(user: user),
      ],
    );
  }
}

class _UserHeader extends StatelessWidget {
  const _UserHeader({required this.user});
  
  final User user;
  
  @override
  Widget build(BuildContext context) {
    return Row(
      children: [
        CircleAvatar(
          backgroundImage: NetworkImage(user.avatarUrl),
        ),
        const SizedBox(width: 16),
        Text(
          user.name,
          style: Theme.of(context).textTheme.headlineSmall,
        ),
      ],
    );
  }
}
```

### State Management

```dart
// Good: Use const constructors
class MyWidget extends StatelessWidget {
  const MyWidget({super.key});
  
  @override
  Widget build(BuildContext context) {
    return const Text('Hello');
  }
}

// Good: Keys for dynamic lists
ListView.builder(
  itemCount: items.length,
  itemBuilder: (context, index) {
    return ListTile(
      key: ValueKey(items[index].id),
      title: Text(items[index].name),
    );
  },
)

// Good: Dispose resources
class MyWidget extends StatefulWidget {
  const MyWidget({super.key});
  
  @override
  State<MyWidget> createState() => _MyWidgetState();
}

class _MyWidgetState extends State<MyWidget> {
  late final TextEditingController _controller;
  
  @override
  void initState() {
    super.initState();
    _controller = TextEditingController();
  }
  
  @override
  void dispose() {
    _controller.dispose();
    super.dispose();
  }
  
  @override
  Widget build(BuildContext context) {
    return TextField(controller: _controller);
  }
}
```

---

## Performance

### Avoid Unnecessary Rebuilds

```dart
// Good: Use const constructors
Widget build(BuildContext context) {
  return const Column(
    children: [
      Text('Static text'),
      Icon(Icons.star),
    ],
  );
}

// Good: Extract unchanging widgets
class StaticWidget extends StatelessWidget {
  const StaticWidget({super.key});
  
  @override
  Widget build(BuildContext context) {
    return const Text('I never change');
  }
}
```

### Lazy Loading

```dart
// Good: Lazy initialization
class ExpensiveService {
  ExpensiveService._();
  
  static ExpensiveService? _instance;
  
  static ExpensiveService get instance {
    return _instance ??= ExpensiveService._();
  }
}

// Good: ListView.builder for long lists
ListView.builder(
  itemCount: 1000,
  itemBuilder: (context, index) {
    return ListTile(title: Text('Item $index'));
  },
)
```

---

## Security

### Sensitive Data

```dart
// Good: Don't log sensitive data
void login(String email, String password) {
  print('Login attempt for: $email');  // OK
  // Never: print('Password: $password');
  
  // Process login
}

// Good: Use secure storage
import 'package:flutter_secure_storage/flutter_secure_storage.dart';

const storage = FlutterSecureStorage();

Future<void> saveToken(String token) async {
  await storage.write(key: 'auth_token', value: token);
}
```

---

## Testing

### Unit Tests

```dart
void main() {
  group('User', () {
    test('fromJson creates correct User', () {
      final json = {
        'id': '1',
        'name': 'John',
        'email': 'john@example.com',
      };
      
      final user = User.fromJson(json);
      
      expect(user.id, '1');
      expect(user.name, 'John');
      expect(user.email, 'john@example.com');
    });
    
    test('copyWith creates modified copy', () {
      final user = User(id: '1', name: 'John');
      final updated = user.copyWith(name: 'Jane');
      
      expect(updated.id, user.id);
      expect(updated.name, 'Jane');
    });
  });
}
```

---

## Anti-Patterns

### Avoid

```dart
// 1. Var without type when unclear
var data = getData();  // What type is this?

// 2. Dynamic when type is known
dynamic user = User(id: '1', name: 'John');

// 3. Ignoring nullable
String name = user.name!;  // Can crash!

// 4. Not using const
Widget build(BuildContext context) {
  return Column(
    children: [
      Text('Hello'),  // Should be const
    ],
  );
}

// 5. Catching without type
try {
  // ...
} catch (e) {  // Too broad
  // ...
}

// 6. Not disposing resources
class _MyState extends State<MyWidget> {
  final controller = TextEditingController();
  // Missing dispose()!
}

// 7. Using print in production
print('Debug: $value');  // Use proper logging

// 8. Mutating parameters
void process(List<int> items) {
  items.add(42);  // Don't mutate parameters!
}

// 9. Not using final
String name = 'John';  // Should be final if never changes

// 10. Over-using inheritance
class UserService extends BaseService {}  // Prefer composition
```

---

## Checklist

### Before Committing

- [ ] dart format applied
- [ ] dart analyze passes
- [ ] dart fix --apply run
- [ ] All tests pass
- [ ] No TODO comments
- [ ] Documentation updated

### Before Publishing

- [ ] dart pub publish --dry-run passes
- [ ] pana score > 130
- [ ] README is comprehensive
- [ ] CHANGELOG updated
- [ ] Version incremented
- [ ] Example code works

---

## Resources

- [Effective Dart](https://dart.dev/guides/language/effective-dart)
- [Dart Language Tour](https://dart.dev/guides/language/language-tour)
- [Flutter Style Guide](https://github.com/flutter/flutter/wiki/Style-guide-for-Flutter-repo)
- [pub.dev Publishing Guide](https://dart.dev/tools/pub/publishing)

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

