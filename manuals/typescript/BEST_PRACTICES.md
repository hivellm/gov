# TypeScript Best Practices

**Version**: 1.0.0  
**Last Updated**: 2025-10-11  
**Target Audience**: AI Agents developing TypeScript projects

---

## Table of Contents

1. [TypeScript Idioms](#typescript-idioms)
2. [Anti-Patterns](#anti-patterns)
3. [Performance Optimization](#performance-optimization)
4. [Security Best Practices](#security-best-practices)
5. [Error Handling](#error-handling)
6. [Async/Await Patterns](#asyncawait-patterns)
7. [Type Safety](#type-safety)
8. [Code Organization](#code-organization)
9. [Testing Best Practices](#testing-best-practices)
10. [Common Gotchas](#common-gotchas)

---

## TypeScript Idioms

### 1. Prefer `const` over `let`, never `var`

```typescript
// ❌ BAD
var name = 'John';
let age = 30;  // If it won't be reassigned

// ✅ GOOD
const name = 'John';
const age = 30;
let count = 0;  // Only use let when value will change
```

### 2. Use Type Inference

```typescript
// ❌ BAD: Redundant type annotation
const name: string = 'John';
const age: number = 30;
const isActive: boolean = true;

// ✅ GOOD: Let TypeScript infer
const name = 'John';  // inferred as string
const age = 30;  // inferred as number
const isActive = true;  // inferred as boolean
```

### 3. Explicit Return Types for Functions

```typescript
// ❌ BAD: No return type
function getUser(id: string) {
  return database.findUser(id);
}

// ✅ GOOD: Explicit return type
function getUser(id: string): Promise<User | null> {
  return database.findUser(id);
}
```

### 4. Use Optional Chaining and Nullish Coalescing

```typescript
// ❌ BAD
const userName = user && user.profile && user.profile.name;
const port = config.port !== null && config.port !== undefined ? config.port : 3000;

// ✅ GOOD
const userName = user?.profile?.name;
const port = config.port ?? 3000;
```

### 5. Use Template Literal Types

```typescript
// ✅ GOOD: Type-safe string patterns
type HTTPMethod = 'GET' | 'POST' | 'PUT' | 'DELETE';
type Endpoint = `/api/${string}`;

function request(method: HTTPMethod, url: Endpoint): Promise<Response> {
  // TypeScript ensures url starts with /api/
}
```

### 6. Prefer Interface over Type for Objects

```typescript
// ❌ BAD: Type alias for object shape
type User = {
  id: string;
  name: string;
};

// ✅ GOOD: Interface for object shape
interface User {
  id: string;
  name: string;
}

// ✅ GOOD: Type alias for unions
type Status = 'pending' | 'approved' | 'rejected';
```

### 7. Use `readonly` for Immutable Data

```typescript
// ❌ BAD: Mutable data
interface Config {
  apiKey: string;
  timeout: number;
}

// ✅ GOOD: Immutable configuration
interface Config {
  readonly apiKey: string;
  readonly timeout: number;
}

// ✅ GOOD: Readonly arrays
const numbers: readonly number[] = [1, 2, 3];
// numbers.push(4); // Error: push does not exist on readonly array
```

### 8. Use Discriminated Unions

```typescript
// ✅ GOOD: Type-safe state management
interface LoadingState {
  status: 'loading';
}

interface SuccessState {
  status: 'success';
  data: User;
}

interface ErrorState {
  status: 'error';
  error: string;
}

type State = LoadingState | SuccessState | ErrorState;

function handleState(state: State): void {
  switch (state.status) {
    case 'loading':
      // TypeScript knows: no data or error properties
      break;
    case 'success':
      // TypeScript knows: state.data exists
      console.log(state.data);
      break;
    case 'error':
      // TypeScript knows: state.error exists
      console.error(state.error);
      break;
  }
}
```

### 9. Use `unknown` Instead of `any`

```typescript
// ❌ BAD: any bypasses type checking
function processData(data: any): void {
  console.log(data.value);  // No error, but might crash at runtime
}

// ✅ GOOD: unknown requires type checking
function processData(data: unknown): void {
  if (typeof data === 'object' && data !== null && 'value' in data) {
    console.log((data as { value: string }).value);
  }
}
```

### 10. Use Enums Sparingly

```typescript
// ❌ BAD: Enum adds runtime code
enum Status {
  Pending = 'pending',
  Approved = 'approved',
  Rejected = 'rejected',
}

// ✅ BETTER: Union type (no runtime cost)
type Status = 'pending' | 'approved' | 'rejected';

// ✅ ACCEPTABLE: const enum (no runtime code)
const enum Status {
  Pending = 'pending',
  Approved = 'approved',
  Rejected = 'rejected',
}
```

---

## Anti-Patterns

### 1. Using `any`

```typescript
// ❌ BAD: Defeats the purpose of TypeScript
function processData(data: any): any {
  return data.value;
}

// ✅ GOOD: Proper typing
function processData<T extends { value: string }>(data: T): string {
  return data.value;
}
```

### 2. Type Assertions Without Validation

```typescript
// ❌ BAD: Unsafe type assertion
const user = JSON.parse(jsonString) as User;

// ✅ GOOD: Validate before using
function parseUser(jsonString: string): User {
  const parsed: unknown = JSON.parse(jsonString);
  
  if (!isUser(parsed)) {
    throw new Error('Invalid user data');
  }
  
  return parsed;
}

function isUser(value: unknown): value is User {
  return (
    typeof value === 'object' &&
    value !== null &&
    'id' in value &&
    'name' in value
  );
}
```

### 3. Mutating Function Parameters

```typescript
// ❌ BAD: Mutates input
function addItem(array: string[], item: string): void {
  array.push(item);
}

// ✅ GOOD: Returns new array
function addItem(array: readonly string[], item: string): string[] {
  return [...array, item];
}
```

### 4. Large Functions

```typescript
// ❌ BAD: 100+ lines function
function processOrder(order: Order): void {
  // validate
  // calculate
  // save
  // notify
  // log
  // ... 100+ lines
}

// ✅ GOOD: Small, focused functions
function processOrder(order: Order): void {
  validateOrder(order);
  const total = calculateTotal(order);
  saveOrder(order, total);
  notifyCustomer(order);
  logOrderProcessed(order.id);
}
```

### 5. Ignoring Promises

```typescript
// ❌ BAD: Fire and forget
function handleClick(): void {
  saveData(data);  // Promise ignored
}

// ✅ GOOD: Handle promise
async function handleClick(): Promise<void> {
  await saveData(data);
}

// ✅ ACCEPTABLE: Explicit void
function handleClick(): void {
  void saveData(data);  // Explicitly ignored
}
```

### 6. Overusing Classes

```typescript
// ❌ BAD: Unnecessary class
class MathUtils {
  static add(a: number, b: number): number {
    return a + b;
  }
}

// ✅ GOOD: Simple functions
export function add(a: number, b: number): number {
  return a + b;
}
```

### 7. Not Using Strict Mode

```typescript
// ❌ BAD tsconfig.json
{
  "compilerOptions": {
    "strict": false
  }
}

// ✅ GOOD tsconfig.json
{
  "compilerOptions": {
    "strict": true,
    "noUnusedLocals": true,
    "noUnusedParameters": true
  }
}
```

---

## Performance Optimization

### 1. Avoid O(n²) Complexity

```typescript
// ❌ BAD: O(n²)
function findDuplicates(arr: number[]): number[] {
  return arr.filter((item, index) => arr.indexOf(item) !== index);
}

// ✅ GOOD: O(n)
function findDuplicates(arr: number[]): number[] {
  const seen = new Set<number>();
  const duplicates = new Set<number>();
  
  for (const item of arr) {
    if (seen.has(item)) {
      duplicates.add(item);
    } else {
      seen.add(item);
    }
  }
  
  return Array.from(duplicates);
}
```

### 2. Use Map/Set for Lookups

```typescript
// ❌ BAD: O(n) lookup
const users = [{ id: '1', name: 'John' }, { id: '2', name: 'Jane' }];
const user = users.find(u => u.id === '1');  // O(n)

// ✅ GOOD: O(1) lookup
const usersMap = new Map(users.map(u => [u.id, u]));
const user = usersMap.get('1');  // O(1)
```

### 3. Lazy Loading

```typescript
// ❌ BAD: Load everything upfront
import { hugeLibrary } from 'huge-library';

function processData(data: Data): void {
  if (data.needsProcessing) {
    hugeLibrary.process(data);
  }
}

// ✅ GOOD: Dynamic import
async function processData(data: Data): Promise<void> {
  if (data.needsProcessing) {
    const { hugeLibrary } = await import('huge-library');
    hugeLibrary.process(data);
  }
}
```

### 4. Memoization

```typescript
// ❌ BAD: Recalculate every time
function fibonacci(n: number): number {
  if (n <= 1) return n;
  return fibonacci(n - 1) + fibonacci(n - 2);
}

// ✅ GOOD: Memoized
const fibonacciMemo = new Map<number, number>();

function fibonacci(n: number): number {
  if (n <= 1) return n;
  
  if (fibonacciMemo.has(n)) {
    return fibonacciMemo.get(n)!;
  }
  
  const result = fibonacci(n - 1) + fibonacci(n - 2);
  fibonacciMemo.set(n, result);
  return result;
}
```

### 5. Avoid Unnecessary Re-renders (React)

```typescript
// ❌ BAD: New object every render
function Component(): JSX.Element {
  const config = { timeout: 1000 };  // New object every time
  return <Child config={config} />;
}

// ✅ GOOD: Stable reference
const CONFIG = { timeout: 1000 };

function Component(): JSX.Element {
  return <Child config={CONFIG} />;
}

// ✅ GOOD: useMemo for computed values
function Component(): JSX.Element {
  const config = useMemo(() => ({ timeout: 1000 }), []);
  return <Child config={config} />;
}
```

### 6. Batch Operations

```typescript
// ❌ BAD: Multiple database calls
async function updateUsers(userIds: string[]): Promise<void> {
  for (const id of userIds) {
    await database.updateUser(id, { active: true });
  }
}

// ✅ GOOD: Single batch operation
async function updateUsers(userIds: string[]): Promise<void> {
  await database.updateMany(
    { id: { in: userIds } },
    { active: true }
  );
}
```

---

## Security Best Practices

### 1. Input Validation

```typescript
// ❌ BAD: No validation
function createUser(email: string, password: string): Promise<User> {
  return database.createUser({ email, password });
}

// ✅ GOOD: Validate input
import { z } from 'zod';

const createUserSchema = z.object({
  email: z.string().email(),
  password: z.string().min(8),
});

function createUser(email: string, password: string): Promise<User> {
  const validated = createUserSchema.parse({ email, password });
  return database.createUser(validated);
}
```

### 2. SQL Injection Prevention

```typescript
// ❌ BAD: String concatenation
function getUser(id: string): Promise<User> {
  return database.query(`SELECT * FROM users WHERE id = '${id}'`);
}

// ✅ GOOD: Parameterized queries
function getUser(id: string): Promise<User> {
  return database.query('SELECT * FROM users WHERE id = $1', [id]);
}

// ✅ BETTER: Use ORM
function getUser(id: string): Promise<User> {
  return prisma.user.findUnique({ where: { id } });
}
```

### 3. XSS Prevention

```typescript
// ❌ BAD: Directly insert user content
function renderComment(comment: string): string {
  return `<div>${comment}</div>`;
}

// ✅ GOOD: Sanitize user input
import DOMPurify from 'isomorphic-dompurify';

function renderComment(comment: string): string {
  const sanitized = DOMPurify.sanitize(comment);
  return `<div>${sanitized}</div>`;
}
```

### 4. Secret Management

```typescript
// ❌ BAD: Hardcoded secrets
const API_KEY = 'sk-1234567890abcdef';

// ✅ GOOD: Environment variables
import { env } from './config/env';

const API_KEY = env.API_KEY;
```

### 5. Authentication Token Handling

```typescript
// ❌ BAD: Store in localStorage (vulnerable to XSS)
localStorage.setItem('token', authToken);

// ✅ GOOD: HTTP-only cookies
res.cookie('token', authToken, {
  httpOnly: true,
  secure: true,
  sameSite: 'strict',
  maxAge: 3600000,
});
```

### 6. Rate Limiting

```typescript
// ✅ GOOD: Implement rate limiting
import rateLimit from 'express-rate-limit';

const limiter = rateLimit({
  windowMs: 15 * 60 * 1000,  // 15 minutes
  max: 100,  // Limit each IP to 100 requests per windowMs
  message: 'Too many requests from this IP',
});

app.use('/api/', limiter);
```

---

## Error Handling

### 1. Custom Error Classes

```typescript
// ✅ GOOD: Structured error hierarchy
export class AppError extends Error {
  constructor(
    message: string,
    public readonly code: string,
    public readonly statusCode: number = 500
  ) {
    super(message);
    this.name = this.constructor.name;
    Error.captureStackTrace(this, this.constructor);
  }
}

export class ValidationError extends AppError {
  constructor(message: string) {
    super(message, 'VALIDATION_ERROR', 400);
  }
}
```

### 2. Error Boundaries

```typescript
// ✅ GOOD: Catch and handle errors properly
async function processData(data: unknown): Promise<Result> {
  try {
    const validated = validateData(data);
    const processed = await transform(validated);
    return { success: true, data: processed };
  } catch (error) {
    if (error instanceof ValidationError) {
      return { success: false, error: error.message };
    }
    
    logger.error('Unexpected error', { error });
    return { success: false, error: 'Internal server error' };
  }
}
```

### 3. Never Swallow Errors

```typescript
// ❌ BAD: Silent failure
try {
  await riskyOperation();
} catch {
  // Error ignored
}

// ✅ GOOD: Log or handle
try {
  await riskyOperation();
} catch (error) {
  logger.error('Risky operation failed', { error });
  throw error;  // Re-throw if can't handle
}
```

---

## Async/Await Patterns

### 1. Always Use async/await

```typescript
// ❌ BAD: Promise chains
function getUser(id: string): Promise<User> {
  return database.getUser(id)
    .then(user => {
      return enrichUserData(user);
    })
    .then(enrichedUser => {
      return validateUser(enrichedUser);
    });
}

// ✅ GOOD: async/await
async function getUser(id: string): Promise<User> {
  const user = await database.getUser(id);
  const enrichedUser = await enrichUserData(user);
  return validateUser(enrichedUser);
}
```

### 2. Parallel Execution

```typescript
// ❌ BAD: Sequential when not needed
async function getData(): Promise<[User, Posts]> {
  const user = await getUser();
  const posts = await getPosts();
  return [user, posts];
}

// ✅ GOOD: Parallel execution
async function getData(): Promise<[User, Posts]> {
  const [user, posts] = await Promise.all([
    getUser(),
    getPosts(),
  ]);
  return [user, posts];
}
```

### 3. Error Handling in Parallel

```typescript
// ✅ GOOD: Handle errors properly
async function fetchMultiple(ids: string[]): Promise<Result[]> {
  const results = await Promise.allSettled(
    ids.map(id => fetchData(id))
  );
  
  return results.map((result, index) => {
    if (result.status === 'fulfilled') {
      return { success: true, data: result.value };
    } else {
      logger.error(`Failed to fetch ${ids[index]}`, { error: result.reason });
      return { success: false, error: result.reason };
    }
  });
}
```

---

## Type Safety

### 1. Type Guards

```typescript
// ✅ GOOD: Type-safe runtime checks
function isString(value: unknown): value is string {
  return typeof value === 'string';
}

function isUser(value: unknown): value is User {
  return (
    typeof value === 'object' &&
    value !== null &&
    'id' in value &&
    'email' in value
  );
}

function processValue(value: unknown): void {
  if (isString(value)) {
    console.log(value.toUpperCase());  // TypeScript knows it's string
  }
}
```

### 2. Branded Types

```typescript
// ✅ GOOD: Prevent accidental mixing of IDs
type Brand<K, T> = K & { __brand: T };

type UserId = Brand<string, 'UserId'>;
type PostId = Brand<string, 'PostId'>;

function getUser(id: UserId): Promise<User> { /* ... */ }
function getPost(id: PostId): Promise<Post> { /* ... */ }

const userId = 'user-123' as UserId;
const postId = 'post-456' as PostId;

getUser(userId);  // ✅ OK
getUser(postId);  // ❌ Error: PostId is not assignable to UserId
```

### 3. Exhaustiveness Checking

```typescript
// ✅ GOOD: Ensure all cases are handled
type Shape = Circle | Square | Triangle;

function getArea(shape: Shape): number {
  switch (shape.type) {
    case 'circle':
      return Math.PI * shape.radius ** 2;
    case 'square':
      return shape.side ** 2;
    case 'triangle':
      return (shape.base * shape.height) / 2;
    default:
      // Exhaustiveness check
      const _exhaustive: never = shape;
      throw new Error(`Unhandled shape: ${_exhaustive}`);
  }
}
```

---

## Code Organization

### 1. Single Responsibility Principle

```typescript
// ❌ BAD: Class does too much
class UserService {
  async createUser(data: CreateUserDto): Promise<User> { }
  async sendWelcomeEmail(user: User): Promise<void> { }
  async logUserCreation(user: User): Promise<void> { }
}

// ✅ GOOD: Separate concerns
class UserService {
  constructor(
    private emailService: EmailService,
    private logger: Logger
  ) {}
  
  async createUser(data: CreateUserDto): Promise<User> {
    const user = await this.repository.create(data);
    await this.emailService.sendWelcome(user);
    this.logger.info('User created', { userId: user.id });
    return user;
  }
}
```

### 2. Dependency Injection

```typescript
// ❌ BAD: Hard dependencies
class UserService {
  private repository = new UserRepository();
  
  async getUser(id: string): Promise<User> {
    return this.repository.findById(id);
  }
}

// ✅ GOOD: Injected dependencies
class UserService {
  constructor(
    private readonly repository: UserRepository
  ) {}
  
  async getUser(id: string): Promise<User> {
    return this.repository.findById(id);
  }
}
```

---

## Testing Best Practices

### 1. Test Structure (AAA Pattern)

```typescript
// ✅ GOOD: Clear test structure
describe('UserService', () => {
  describe('createUser', () => {
    it('should create a user with valid data', async () => {
      // Arrange
      const input = { email: 'test@example.com', name: 'Test' };
      const expected = { id: '123', ...input };
      mockRepository.create.mockResolvedValue(expected);
      
      // Act
      const result = await userService.createUser(input);
      
      // Assert
      expect(result).toEqual(expected);
      expect(mockRepository.create).toHaveBeenCalledWith(input);
    });
  });
});
```

### 2. Test Isolation

```typescript
// ✅ GOOD: Each test is independent
describe('UserService', () => {
  let userService: UserService;
  let mockRepository: MockUserRepository;
  
  beforeEach(() => {
    mockRepository = createMockUserRepository();
    userService = new UserService(mockRepository);
  });
  
  // Each test starts with fresh mocks
});
```

### 3. Don't Test Implementation Details

```typescript
// ❌ BAD: Testing implementation
it('should call validateEmail method', () => {
  const spy = vi.spyOn(userService, 'validateEmail');
  userService.createUser(data);
  expect(spy).toHaveBeenCalled();
});

// ✅ GOOD: Testing behavior
it('should throw error for invalid email', async () => {
  await expect(
    userService.createUser({ email: 'invalid', name: 'Test' })
  ).rejects.toThrow('Invalid email');
});
```

---

## Common Gotchas

### 1. Falsy Values

```typescript
// ❌ BAD: 0 and '' are falsy
const count = 0;
if (count) {
  // Won't execute
}

// ✅ GOOD: Explicit checks
if (count !== undefined && count !== null) {
  // Will execute
}

// ✅ GOOD: Use nullish coalescing
const value = count ?? 10;  // Uses count even if it's 0
```

### 2. Array Methods Return New Arrays

```typescript
// ⚠️ IMPORTANT: These return NEW arrays
const doubled = numbers.map(n => n * 2);  // New array
const evens = numbers.filter(n => n % 2 === 0);  // New array

// ⚠️ IMPORTANT: These mutate the array
numbers.push(5);    // Mutates
numbers.sort();     // Mutates
numbers.reverse();  // Mutates
```

### 3. `this` Binding

```typescript
// ❌ BAD: Lost this binding
class Counter {
  count = 0;
  
  increment() {
    this.count++;
  }
}

const counter = new Counter();
const increment = counter.increment;
increment();  // Error: this is undefined

// ✅ GOOD: Arrow function
class Counter {
  count = 0;
  
  increment = () => {
    this.count++;
  };
}

// ✅ GOOD: Bind in constructor
class Counter {
  count = 0;
  
  constructor() {
    this.increment = this.increment.bind(this);
  }
  
  increment() {
    this.count++;
  }
}
```

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-10-11 | Initial best practices guide |

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

