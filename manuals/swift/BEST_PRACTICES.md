# Swift Best Practices Guide

**Version:** 1.0.0  
**Last Updated:** 2025-10-11  
**Target Audience:** AI Agents developing Swift applications  
**Compliance:** Swift API Design Guidelines, Apple Human Interface Guidelines

---

## Table of Contents

1. [Code Style](#code-style)
2. [Naming Conventions](#naming-conventions)
3. [Type System](#type-system)
4. [Optionals](#optionals)
5. [Error Handling](#error-handling)
6. [Memory Management](#memory-management)
7. [Concurrency](#concurrency)
8. [Protocol-Oriented Programming](#protocol-oriented-programming)
9. [SwiftUI Best Practices](#swiftui-best-practices)
10. [Performance](#performance)
11. [Security](#security)
12. [Testing](#testing)
13. [Anti-Patterns](#anti-patterns)

---

## Code Style

### File Organization

```swift
// 1. Import statements
import Foundation
import Combine

// 2. Type definition
public struct UserService {
  // 3. Type properties
  static let shared = UserService()
  
  // 4. Instance properties
  private let networkClient: NetworkClient
  
  // 5. Initialization
  init(networkClient: NetworkClient = .shared) {
    self.networkClient = networkClient
  }
  
  // 6. Public methods
  public func fetchUser(id: UUID) async throws -> User {
    // Implementation
  }
  
  // 7. Private methods
  private func validateUser(_ user: User) -> Bool {
    // Implementation
  }
}

// 8. Extensions
extension UserService: Sendable {}
```

### Indentation and Spacing

```swift
// Good: Consistent indentation (2 spaces)
func calculate(
  x: Int,
  y: Int,
  operation: Operation
) -> Int {
  switch operation {
  case .add:
    return x + y
  case .subtract:
    return x - y
  }
}

// Good: Spacing around operators
let result = (x + y) * z

// Good: No space before, space after colon
var name: String
func greet(person: String) -> String

// Bad: Inconsistent spacing
let result=(x+y)*z
var name :String
func greet( person:String )->String
```

### Line Length

```swift
// Maximum 100 characters (configurable)

// Good: Break long lines
let user = User(
  id: UUID(),
  name: "John Doe",
  email: "john@example.com",
  isActive: true
)

// Good: Break function signatures
func processUserData(
  users: [User],
  filter: (User) -> Bool,
  transform: (User) -> ProcessedUser
) -> [ProcessedUser] {
  users.filter(filter).map(transform)
}
```

---

## Naming Conventions

### Types

```swift
// PascalCase for types
struct UserProfile {}
class NetworkManager {}
enum LoadingState {}
protocol DataProvider {}
typealias UserID = UUID

// Protocols for capabilities
protocol Drawable {}
protocol Codable {}  // Can be encoded/decoded

// Protocols for types
protocol Collection {}  // Is a collection
```

### Functions and Variables

```swift
// camelCase for functions and variables
func fetchUsers() async throws -> [User]
var isLoading: Bool
let maxRetryCount = 3

// Boolean names should read as assertions
var isEmpty: Bool
var hasData: Bool
var canEdit: Bool
var shouldRefresh: Bool

// Collections: plural names
var users: [User]
var items: [Item]

// Single values: singular names
var user: User
var item: Item
```

### Constants

```swift
// Swift uses camelCase for constants (not SCREAMING_SNAKE_CASE)
let defaultTimeout: TimeInterval = 30
let maxRetries = 3
let apiBaseURL = URL(string: "https://api.example.com")!

// Type-level constants
extension Configuration {
  static let production = Configuration(baseURL: "https://api.example.com")
  static let development = Configuration(baseURL: "https://dev.api.example.com")
}
```

### Enums

```swift
// Enum cases: camelCase
enum LoadingState {
  case idle
  case loading
  case loaded(Data)
  case failed(Error)
}

// Associated values: labeled
enum Result<Success, Failure: Error> {
  case success(Success)
  case failure(Failure)
}

// Use dot syntax when type is known
let state: LoadingState = .loading
```

---

## Type System

### Value Types (Structs)

```swift
// Prefer structs for data models
struct User {
  let id: UUID
  var name: String
  var email: String
  var createdAt: Date
}

// Structs are copied
var user1 = User(id: UUID(), name: "John", email: "john@example.com", createdAt: Date())
var user2 = user1
user2.name = "Jane"
// user1.name is still "John"
```

### Reference Types (Classes)

```swift
// Use classes when you need:
// - Reference semantics
// - Inheritance
// - Deinitializers
class ViewModel: ObservableObject {
  @Published var users: [User] = []
  
  deinit {
    // Cleanup
  }
}

// Classes are shared
let viewModel1 = ViewModel()
let viewModel2 = viewModel1
viewModel2.users = []
// viewModel1.users is also []
```

### When to Use Each

```swift
// Struct: Value semantics, no inheritance needed
struct Point {
  var x: Double
  var y: Double
}

// Class: Reference semantics, inheritance, deinit
class NetworkManager {
  private var task: URLSessionTask?
  
  deinit {
    task?.cancel()
  }
}

// Enum: Fixed set of cases
enum NetworkError: Error {
  case notFound
  case serverError
  case timeout
}

// Actor: Thread-safe reference type
actor DataCache {
  private var cache: [String: Data] = [:]
  
  func get(key: String) -> Data? {
    cache[key]
  }
}
```

### Generics

```swift
// Generic functions
func swap<T>(_ a: inout T, _ b: inout T) {
  let temp = a
  a = b
  b = temp
}

// Generic types
struct Queue<Element> {
  private var elements: [Element] = []
  
  mutating func enqueue(_ element: Element) {
    elements.append(element)
  }
  
  mutating func dequeue() -> Element? {
    elements.isEmpty ? nil : elements.removeFirst()
  }
}

// Constrained generics
func findIndex<T: Equatable>(of value: T, in array: [T]) -> Int? {
  array.firstIndex(of: value)
}
```

---

## Optionals

### Optional Binding

```swift
// Good: if let for single optional
if let user = optionalUser {
  print(user.name)
}

// Good: guard let for early exit
func process(user: User?) {
  guard let user else {
    return
  }
  // Use user
}

// Good: Multiple bindings
if let user, let account = user.account, account.isActive {
  // All conditions met
}

// Bad: Pyramids of doom
if let user = optionalUser {
  if let account = user.account {
    if account.isActive {
      // Deep nesting
    }
  }
}
```

### Optional Chaining

```swift
// Good: Optional chaining
let city = user?.address?.city

// Good: With default value
let city = user?.address?.city ?? "Unknown"

// Good: Optional map
let uppercasedName = user?.name.uppercased()
```

### Force Unwrapping

```swift
// Avoid force unwrapping
let name = user!.name  // Crashes if nil!

// Only when guaranteed non-nil
let url = URL(string: "https://example.com")!  // Known valid URL

// Better: Handle the optional
guard let url = URL(string: urlString) else {
  throw URLError(.badURL)
}
```

### Implicitly Unwrapped Optionals

```swift
// Only use for:
// 1. IBOutlets
@IBOutlet var label: UILabel!

// 2. Late initialization (rare)
class ViewController: UIViewController {
  var viewModel: ViewModel!
  
  override func viewDidLoad() {
    super.viewDidLoad()
    viewModel = ViewModel()  // Set before use
  }
}

// Avoid: General use
var name: String!  // Don't do this
```

---

## Error Handling

### Defining Errors

```swift
// Good: Conform to Error protocol
enum NetworkError: Error {
  case invalidURL
  case notFound
  case serverError(statusCode: Int)
  case decodingFailed(Error)
}

// Good: LocalizedError for user-facing messages
extension NetworkError: LocalizedError {
  var errorDescription: String? {
    switch self {
    case .invalidURL:
      return "The URL is invalid."
    case .notFound:
      return "The requested resource was not found."
    case .serverError(let code):
      return "Server error with status code \(code)."
    case .decodingFailed:
      return "Failed to decode the response."
    }
  }
}
```

### Throwing Errors

```swift
// Good: Throw specific errors
func fetchUser(id: UUID) async throws -> User {
  guard let url = URL(string: "https://api.example.com/users/\(id)") else {
    throw NetworkError.invalidURL
  }
  
  let (data, response) = try await URLSession.shared.data(from: url)
  
  guard let httpResponse = response as? HTTPURLResponse,
        (200...299).contains(httpResponse.statusCode) else {
    throw NetworkError.notFound
  }
  
  do {
    return try JSONDecoder().decode(User.self, from: data)
  } catch {
    throw NetworkError.decodingFailed(error)
  }
}
```

### Handling Errors

```swift
// Good: Handle different error types
do {
  let user = try await fetchUser(id: userID)
  display(user)
} catch let error as NetworkError {
  handle(networkError: error)
} catch {
  handle(unknownError: error)
}

// Good: try? when failure doesn't matter
let user = try? await fetchUser(id: userID)

// Good: try! only when guaranteed to succeed
let regex = try! NSRegularExpression(pattern: "[A-Z]+")

// Bad: Silent failures (use sparingly)
_ = try? dangerousOperation()  // Ignores error
```

### Result Type

```swift
// Good: Use Result for async completion handlers
func fetchUser(
  id: UUID,
  completion: @escaping (Result<User, Error>) -> Void
) {
  // Implementation
}

// Usage
fetchUser(id: userID) { result in
  switch result {
  case .success(let user):
    print(user.name)
  case .failure(let error):
    print("Error: \(error)")
  }
}

// Better: Use async/await instead
func fetchUser(id: UUID) async throws -> User {
  // Implementation
}
```

---

## Memory Management

### Automatic Reference Counting (ARC)

```swift
// Swift uses ARC for memory management
class Person {
  let name: String
  var apartment: Apartment?
  
  init(name: String) {
    self.name = name
  }
  
  deinit {
    print("\(name) is being deinitialized")
  }
}

// Reference is released when out of scope
do {
  let person = Person(name: "John")
  // person is used here
}
// person.deinit is called here
```

### Retain Cycles

```swift
// Problem: Strong reference cycle
class Person {
  var apartment: Apartment?
}

class Apartment {
  var tenant: Person?  // Strong reference
}

let person = Person()
let apartment = Apartment()
person.apartment = apartment
apartment.tenant = person  // Cycle! Neither will be deallocated

// Solution 1: Weak reference
class Apartment {
  weak var tenant: Person?  // Weak reference
}

// Solution 2: Unowned reference
class CreditCard {
  unowned let customer: Customer  // Always has a customer
  init(customer: Customer) {
    self.customer = customer
  }
}
```

### Closures and Capture Lists

```swift
// Problem: Closure captures self strongly
class ViewController: UIViewController {
  var completion: (() -> Void)?
  
  func setupBad() {
    completion = {
      self.view.backgroundColor = .red  // Strong capture
    }
  }
  
  // Solution 1: Weak self
  func setupGood() {
    completion = { [weak self] in
      guard let self else { return }
      self.view.backgroundColor = .red
    }
  }
  
  // Solution 2: Unowned self (if guaranteed non-nil)
  func setupAlternative() {
    completion = { [unowned self] in
      self.view.backgroundColor = .red
    }
  }
}

// Multiple captures
fetchData { [weak self, weak delegate] result in
  guard let self, let delegate else { return }
  // Use self and delegate
}
```

---

## Concurrency

### Async/Await

```swift
// Good: Async functions
func fetchUser(id: UUID) async throws -> User {
  let url = URL(string: "https://api.example.com/users/\(id)")!
  let (data, _) = try await URLSession.shared.data(from: url)
  return try JSONDecoder().decode(User.self, from: data)
}

// Good: Calling async functions
Task {
  do {
    let user = try await fetchUser(id: userID)
    print(user.name)
  } catch {
    print("Error: \(error)")
  }
}

// Good: Multiple async operations in parallel
async let user = fetchUser(id: userID)
async let posts = fetchPosts(userID: userID)
async let comments = fetchComments(userID: userID)

let (fetchedUser, fetchedPosts, fetchedComments) = await (
  try user,
  try posts,
  try comments
)
```

### Actors

```swift
// Good: Actor for thread-safe state
actor DataCache {
  private var cache: [String: Data] = [:]
  private var timestamps: [String: Date] = [:]
  
  func get(key: String) -> Data? {
    guard let timestamp = timestamps[key],
          Date().timeIntervalSince(timestamp) < 3600 else {
      return nil
    }
    return cache[key]
  }
  
  func set(key: String, value: Data) {
    cache[key] = value
    timestamps[key] = Date()
  }
  
  func clear() {
    cache.removeAll()
    timestamps.removeAll()
  }
}

// Usage
let cache = DataCache()

Task {
  await cache.set(key: "user", value: userData)
  if let data = await cache.get(key: "user") {
    // Use data
  }
}
```

### Task Groups

```swift
// Good: Process multiple items concurrently
func fetchAllUsers(ids: [UUID]) async throws -> [User] {
  try await withThrowingTaskGroup(of: User.self) { group in
    for id in ids {
      group.addTask {
        try await fetchUser(id: id)
      }
    }
    
    var users: [User] = []
    for try await user in group {
      users.append(user)
    }
    return users
  }
}
```

### MainActor

```swift
// Good: UI updates on main thread
@MainActor
class ViewModel: ObservableObject {
  @Published var users: [User] = []
  
  func loadUsers() async {
    // Already on main thread
    users = try? await fetchUsers()
  }
}

// Good: Explicit main actor
func updateUI() async {
  await MainActor.run {
    // Update UI
  }
}
```

---

## Protocol-Oriented Programming

### Protocol Design

```swift
// Good: Single responsibility protocols
protocol Identifiable {
  associatedtype ID: Hashable
  var id: ID { get }
}

protocol Fetchable {
  associatedtype Resource
  func fetch() async throws -> Resource
}

// Good: Protocol composition
typealias Entity = Identifiable & Codable & Hashable

struct User: Entity {
  let id: UUID
  var name: String
}
```

### Protocol Extensions

```swift
// Good: Default implementations
protocol Drawable {
  func draw()
  func drawWithColor(_ color: UIColor)
}

extension Drawable {
  // Default implementation
  func drawWithColor(_ color: UIColor) {
    // Set color
    draw()
  }
}

// Good: Constrained extensions
extension Collection where Element: Equatable {
  func allEqual() -> Bool {
    guard let first else { return true }
    return allSatisfy { $0 == first }
  }
}
```

### Protocol Inheritance

```swift
// Good: Build on existing protocols
protocol Persistable: Codable {
  func save() async throws
  func delete() async throws
}

protocol SyncablePersistable: Persistable {
  func sync() async throws
}
```

---

## SwiftUI Best Practices

### View Composition

```swift
// Good: Small, focused views
struct UserRowView: View {
  let user: User
  
  var body: some View {
    HStack {
      AvatarView(url: user.avatarURL)
      VStack(alignment: .leading) {
        Text(user.name)
          .font(.headline)
        Text(user.email)
          .font(.caption)
          .foregroundStyle(.secondary)
      }
    }
  }
}

// Good: Extract subviews
struct ProfileView: View {
  let user: User
  
  var body: some View {
    VStack {
      headerView
      detailsView
      actionsView
    }
  }
  
  private var headerView: some View {
    // Header implementation
  }
  
  private var detailsView: some View {
    // Details implementation
  }
  
  private var actionsView: some View {
    // Actions implementation
  }
}
```

### State Management

```swift
// Good: @State for view-local state
struct CounterView: View {
  @State private var count = 0
  
  var body: some View {
    VStack {
      Text("Count: \(count)")
      Button("Increment") {
        count += 1
      }
    }
  }
}

// Good: @Binding for two-way binding
struct ToggleView: View {
  @Binding var isOn: Bool
  
  var body: some View {
    Toggle("Setting", isOn: $isOn)
  }
}

// Good: @ObservableObject for shared state
@Observable
class ViewModel {
  var users: [User] = []
  var isLoading = false
  
  func loadUsers() async {
    isLoading = true
    defer { isLoading = false }
    users = try? await fetchUsers()
  }
}

struct ContentView: View {
  @State private var viewModel = ViewModel()
  
  var body: some View {
    List(viewModel.users) { user in
      Text(user.name)
    }
    .task {
      await viewModel.loadUsers()
    }
  }
}
```

### View Modifiers

```swift
// Good: Extract custom modifiers
struct CardModifier: ViewModifier {
  func body(content: Content) -> some View {
    content
      .padding()
      .background(Color.white)
      .cornerRadius(10)
      .shadow(radius: 5)
  }
}

extension View {
  func cardStyle() -> some View {
    modifier(CardModifier())
  }
}

// Usage
Text("Hello")
  .cardStyle()
```

---

## Performance

### Collections

```swift
// Good: Choose appropriate collection
let users: [User] = []  // Ordered, duplicates allowed
let uniqueIDs: Set<UUID> = []  // Unordered, unique
let usersByID: [UUID: User] = [:]  // Key-value lookup

// Good: Use lazy for large collections
let processedUsers = users
  .lazy
  .filter { $0.isActive }
  .map { process($0) }
  .prefix(10)  // Only processes 10 items

// Bad: Unnecessary array operations
let result = users
  .map { $0 }  // Identity map, useless
  .filter { $0.isActive }
  .map { $0.name }
```

### String Performance

```swift
// Good: Use string interpolation
let message = "Hello, \(name)!"

// Bad: String concatenation in loops
var result = ""
for item in items {
  result += item.description  // Slow!
}

// Good: Use joined()
let result = items.map(\.description).joined()
```

### Memory Optimization

```swift
// Good: Use value types
struct Point {  // Allocated on stack
  var x: Double
  var y: Double
}

// Good: Lazy properties
class DataManager {
  lazy var expensiveComputation: [Int] = {
    // Only computed when accessed
    (0...10000).map { $0 * $0 }
  }()
}

// Good: Weak references for large objects
class ImageCache {
  private var cache: NSCache<NSString, UIImage> = {
    let cache = NSCache<NSString, UIImage>()
    cache.countLimit = 100
    return cache
  }()
}
```

---

## Security

### Data Protection

```swift
// Good: Keychain for sensitive data
import Security

class KeychainManager {
  func save(password: String, for account: String) throws {
    let data = password.data(using: .utf8)!
    
    let query: [String: Any] = [
      kSecClass as String: kSecClassGenericPassword,
      kSecAttrAccount as String: account,
      kSecValueData as String: data,
      kSecAttrAccessible as String: kSecAttrAccessibleWhenUnlocked
    ]
    
    SecItemDelete(query as CFDictionary)
    let status = SecItemAdd(query as CFDictionary, nil)
    
    guard status == errSecSuccess else {
      throw KeychainError.saveFailed(status)
    }
  }
}

// Bad: UserDefaults for sensitive data
UserDefaults.standard.set(password, forKey: "password")  // Don't!
```

### Network Security

```swift
// Good: Use HTTPS
let url = URL(string: "https://api.example.com")!

// Good: Certificate pinning
class NetworkManager: NSObject, URLSessionDelegate {
  func urlSession(
    _ session: URLSession,
    didReceive challenge: URLAuthenticationChallenge,
    completionHandler: @escaping (URLSession.AuthChallengeDisposition, URLCredential?) -> Void
  ) {
    // Implement certificate pinning
  }
}

// Good: Validate server trust
```

### Input Validation

```swift
// Good: Validate user input
func validateEmail(_ email: String) -> Bool {
  let emailRegex = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,64}"
  let predicate = NSPredicate(format: "SELF MATCHES %@", emailRegex)
  return predicate.evaluate(with: email)
}

// Good: Sanitize input
func sanitize(_ input: String) -> String {
  input
    .trimmingCharacters(in: .whitespacesAndNewlines)
    .replacingOccurrences(of: "<script>", with: "")
}
```

---

## Testing

### Unit Tests

```swift
import XCTest
@testable import MyApp

final class UserServiceTests: XCTestCase {
  var sut: UserService!
  var mockClient: MockNetworkClient!
  
  override func setUp() {
    super.setUp()
    mockClient = MockNetworkClient()
    sut = UserService(networkClient: mockClient)
  }
  
  override func tearDown() {
    sut = nil
    mockClient = nil
    super.tearDown()
  }
  
  func testFetchUserSuccess() async throws {
    // Given
    let expectedUser = User(name: "John", email: "john@example.com")
    mockClient.mockResult = .success(expectedUser)
    
    // When
    let user = try await sut.fetchUser(id: UUID())
    
    // Then
    XCTAssertEqual(user.name, expectedUser.name)
    XCTAssertEqual(user.email, expectedUser.email)
  }
  
  func testFetchUserFailure() async {
    // Given
    mockClient.mockResult = .failure(NetworkError.notFound)
    
    // When/Then
    do {
      _ = try await sut.fetchUser(id: UUID())
      XCTFail("Should throw error")
    } catch {
      XCTAssertTrue(error is NetworkError)
    }
  }
}
```

### SwiftUI Preview Tests

```swift
// Good: Provide multiple preview variants
struct UserRowView_Previews: PreviewProvider {
  static var previews: some View {
    Group {
      UserRowView(user: .preview)
      UserRowView(user: .previewLongName)
      UserRowView(user: .previewNoAvatar)
    }
    .previewLayout(.sizeThatFits)
  }
}

extension User {
  static var preview: User {
    User(name: "John Doe", email: "john@example.com")
  }
  
  static var previewLongName: User {
    User(
      name: "John Jacob Jingleheimer Schmidt",
      email: "jjjs@example.com"
    )
  }
}
```

---

## Anti-Patterns

### Avoid

```swift
// 1. Force unwrapping
let name = user!.name  // Crashes if nil

// 2. Force try
let data = try! JSONDecoder().decode(User.self, from: jsonData)

// 3. Implicitly unwrapped optionals (except IBOutlets)
var name: String!

// 4. Massive view controllers
class ViewController: UIViewController {
  // 1000+ lines of code
}

// 5. Stringly-typed code
let key = "userDefaultsKey"  // Use enum instead

// 6. Unnecessary optional chaining
let count = array?.count ?? 0  // array is not optional

// 7. Nested optionals
var data: Data??  // Don't do this

// 8. God objects
class AppManager {
  // Does everything
}

// 9. Ignoring errors
_ = try? dangerousOperation()

// 10. Blocking main thread
DispatchQueue.main.sync {
  // Heavy computation
}
```

---

## Checklist

### Before Committing

- [ ] SwiftLint passes
- [ ] swift-format applied
- [ ] No compiler warnings
- [ ] No force unwraps (or justified)
- [ ] No force try (or justified)
- [ ] Tests pass
- [ ] Documentation updated

### Before Release

- [ ] All tests pass
- [ ] Test coverage > 90%
- [ ] No memory leaks (Instruments check)
- [ ] Performance acceptable
- [ ] Documentation complete
- [ ] CHANGELOG updated
- [ ] Version number updated

---

## Resources

- [Swift API Design Guidelines](https://www.swift.org/documentation/api-design-guidelines/)
- [Swift Programming Language](https://docs.swift.org/swift-book/)
- [SwiftLint Rules](https://realm.github.io/SwiftLint/rule-directory.html)
- [Apple Human Interface Guidelines](https://developer.apple.com/design/human-interface-guidelines/)

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

