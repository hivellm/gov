# JavaScript Best Practices Guide

**Version**: 1.0.0  
**Last Updated**: 2025-10-11  
**Target Audience**: AI Agents & Developers

---

## Table of Contents

1. [JavaScript Idioms](#javascript-idioms)
2. [Anti-Patterns](#anti-patterns)
3. [Performance Optimization](#performance-optimization)
4. [Security Best Practices](#security-best-practices)
5. [Error Handling](#error-handling)
6. [Async Patterns](#async-patterns)
7. [Modern ES6+ Features](#modern-es6-features)
8. [Common Gotchas](#common-gotchas)

---

## JavaScript Idioms

### 1. Use Modern Variable Declarations

**✅ Good**:
```javascript
const MAX_CONNECTIONS = 100;  // Constants
let count = 0;                // Mutable variables

// Block scoped
if (true) {
  const temp = 'test';
  // temp only exists here
}
```

**❌ Bad**:
```javascript
var MAX_CONNECTIONS = 100;    // Function scoped, can be redeclared
var count = 0;
```

### 2. Arrow Functions

**✅ Good**:
```javascript
// Concise syntax
const numbers = [1, 2, 3, 4, 5];
const doubled = numbers.map(n => n * 2);

// Multiple parameters
const add = (a, b) => a + b;

// Multiple statements
const processUser = user => {
  const normalized = user.toLowerCase();
  return normalized.trim();
};

// No `this` binding
class Timer {
  constructor() {
    this.seconds = 0;
    setInterval(() => {
      this.seconds++;  // `this` refers to Timer instance
    }, 1000);
  }
}
```

**❌ Bad**:
```javascript
const doubled = numbers.map(function(n) {
  return n * 2;
});

// `this` binding issues
class Timer {
  constructor() {
    this.seconds = 0;
    setInterval(function() {
      this.seconds++;  // `this` is undefined or global
    }, 1000);
  }
}
```

### 3. Template Literals

**✅ Good**:
```javascript
const name = 'John';
const age = 30;

// String interpolation
const message = `Hello, ${name}! You are ${age} years old.`;

// Multi-line strings
const html = `
  <div>
    <h1>${name}</h1>
    <p>Age: ${age}</p>
  </div>
`;

// Expression evaluation
const status = `User is ${age >= 18 ? 'adult' : 'minor'}`;
```

**❌ Bad**:
```javascript
const message = 'Hello, ' + name + '! You are ' + age + ' years old.';

const html = '<div>\n' +
  '  <h1>' + name + '</h1>\n' +
  '  <p>Age: ' + age + '</p>\n' +
  '</div>';
```

### 4. Destructuring

**✅ Good**:
```javascript
// Object destructuring
const user = { id: 1, name: 'John', email: 'john@example.com' };
const { id, name, email } = user;

// With defaults
const { age = 18 } = user;

// Nested destructuring
const { address: { city } } = user;

// Array destructuring
const [first, second, ...rest] = [1, 2, 3, 4, 5];

// Function parameters
function greet({ name, age }) {
  console.log(`Hello ${name}, age ${age}`);
}
```

**❌ Bad**:
```javascript
const id = user.id;
const name = user.name;
const email = user.email;

const first = array[0];
const second = array[1];
```

### 5. Spread and Rest Operators

**✅ Good**:
```javascript
// Spread (expand)
const arr1 = [1, 2, 3];
const arr2 = [4, 5, 6];
const combined = [...arr1, ...arr2];

const obj1 = { a: 1, b: 2 };
const obj2 = { c: 3 };
const merged = { ...obj1, ...obj2 };

// Rest (collect)
function sum(...numbers) {
  return numbers.reduce((acc, n) => acc + n, 0);
}

const { a, ...rest } = { a: 1, b: 2, c: 3 };
```

**❌ Bad**:
```javascript
const combined = arr1.concat(arr2);

const merged = Object.assign({}, obj1, obj2);

function sum() {
  const numbers = Array.prototype.slice.call(arguments);
  return numbers.reduce((acc, n) => acc + n, 0);
}
```

### 6. Object Shorthand

**✅ Good**:
```javascript
const name = 'John';
const age = 30;

// Property shorthand
const user = { name, age };

// Method shorthand
const utils = {
  getName() {
    return this.name;
  },
  setName(name) {
    this.name = name;
  },
};
```

**❌ Bad**:
```javascript
const user = { name: name, age: age };

const utils = {
  getName: function() {
    return this.name;
  },
};
```

### 7. Default Parameters

**✅ Good**:
```javascript
function greet(name = 'Guest', greeting = 'Hello') {
  return `${greeting}, ${name}!`;
}

// With destructuring
function createUser({ name, email, role = 'user' } = {}) {
  return { name, email, role };
}
```

**❌ Bad**:
```javascript
function greet(name, greeting) {
  name = name || 'Guest';
  greeting = greeting || 'Hello';
  return greeting + ', ' + name + '!';
}
```

### 8. Optional Chaining

**✅ Good**:
```javascript
const user = { address: { city: 'NYC' } };

// Safe property access
const city = user?.address?.city;
const zipCode = user?.address?.zipCode;  // undefined, no error

// With arrays
const firstUser = users?.[0];

// With functions
const result = obj.someMethod?.();
```

**❌ Bad**:
```javascript
const city = user && user.address && user.address.city;
const zipCode = user && user.address && user.address.zipCode;
```

### 9. Nullish Coalescing

**✅ Good**:
```javascript
// Only null or undefined triggers default
const value = someValue ?? 'default';

// vs OR operator
const count = 0;
const displayCount = count ?? 10;     // 0 (correct)
const wrongCount = count || 10;       // 10 (wrong, 0 is falsy)
```

### 10. Array Methods

**✅ Good**:
```javascript
const numbers = [1, 2, 3, 4, 5];

// Map
const doubled = numbers.map(n => n * 2);

// Filter
const evens = numbers.filter(n => n % 2 === 0);

// Reduce
const sum = numbers.reduce((acc, n) => acc + n, 0);

// Find
const found = numbers.find(n => n > 3);

// Some/Every
const hasEven = numbers.some(n => n % 2 === 0);
const allPositive = numbers.every(n => n > 0);

// Chaining
const result = numbers
  .filter(n => n % 2 === 0)
  .map(n => n * 2)
  .reduce((acc, n) => acc + n, 0);
```

**❌ Bad**:
```javascript
const doubled = [];
for (let i = 0; i < numbers.length; i++) {
  doubled.push(numbers[i] * 2);
}
```

---

## Anti-Patterns

### 1. Callback Hell

**❌ Bad**:
```javascript
getData(function(a) {
  getMoreData(a, function(b) {
    getMoreData(b, function(c) {
      getMoreData(c, function(d) {
        // Nested callbacks
      });
    });
  });
});
```

**✅ Good**:
```javascript
async function fetchData() {
  const a = await getData();
  const b = await getMoreData(a);
  const c = await getMoreData(b);
  const d = await getMoreData(c);
  return d;
}
```

### 2. Mutating Parameters

**❌ Bad**:
```javascript
function addItem(array, item) {
  array.push(item);  // Mutates input
  return array;
}
```

**✅ Good**:
```javascript
function addItem(array, item) {
  return [...array, item];  // Returns new array
}
```

### 3. Using `==` Instead of `===`

**❌ Bad**:
```javascript
if (value == 0) { }       // Type coercion
if (value == null) { }
```

**✅ Good**:
```javascript
if (value === 0) { }      // Strict equality
if (value === null || value === undefined) { }
// Or
if (value == null) { }    // Only acceptable use: null OR undefined check
```

### 4. Global Variables

**❌ Bad**:
```javascript
var userData = {};  // Global variable

function setUser(user) {
  userData = user;
}
```

**✅ Good**:
```javascript
// Use modules and closures
const createUserManager = () => {
  let userData = {};
  
  return {
    setUser(user) {
      userData = user;
    },
    getUser() {
      return userData;
    },
  };
};

const userManager = createUserManager();
```

### 5. Not Handling Promise Rejections

**❌ Bad**:
```javascript
fetch('/api/data')
  .then(res => res.json())
  .then(data => console.log(data));
  // No error handling!
```

**✅ Good**:
```javascript
try {
  const res = await fetch('/api/data');
  const data = await res.json();
  console.log(data);
} catch (error) {
  console.error('Failed to fetch data:', error);
}
```

### 6. Blocking the Event Loop

**❌ Bad**:
```javascript
// Synchronous operation blocks everything
function heavySync() {
  let result = 0;
  for (let i = 0; i < 1000000000; i++) {
    result += i;
  }
  return result;
}
```

**✅ Good**:
```javascript
// Use async for heavy operations
async function heavyAsync() {
  return new Promise(resolve => {
    setImmediate(() => {
      let result = 0;
      for (let i = 0; i < 1000000000; i++) {
        result += i;
      }
      resolve(result);
    });
  });
}

// Or use worker threads for CPU-intensive tasks
```

---

## Performance Optimization

### 1. Avoid Memory Leaks

**✅ Good**:
```javascript
class EventManager {
  constructor() {
    this.listeners = new Map();
  }

  addEventListener(element, event, handler) {
    element.addEventListener(event, handler);
    
    if (!this.listeners.has(element)) {
      this.listeners.set(element, []);
    }
    this.listeners.get(element).push({ event, handler });
  }

  cleanup() {
    // Remove all listeners
    for (const [element, listeners] of this.listeners) {
      listeners.forEach(({ event, handler }) => {
        element.removeEventListener(event, handler);
      });
    }
    this.listeners.clear();
  }
}
```

### 2. Use Memoization

**✅ Good**:
```javascript
const memoize = fn => {
  const cache = new Map();
  
  return (...args) => {
    const key = JSON.stringify(args);
    
    if (cache.has(key)) {
      return cache.get(key);
    }
    
    const result = fn(...args);
    cache.set(key, result);
    return result;
  };
};

const expensiveOperation = memoize((n) => {
  // Heavy computation
  return n * n;
});
```

### 3. Debounce and Throttle

**✅ Good**:
```javascript
// Debounce: Wait until user stops typing
const debounce = (fn, delay) => {
  let timeoutId;
  
  return (...args) => {
    clearTimeout(timeoutId);
    timeoutId = setTimeout(() => fn(...args), delay);
  };
};

const handleSearch = debounce(query => {
  console.log('Searching for:', query);
}, 300);

// Throttle: Limit execution rate
const throttle = (fn, limit) => {
  let inThrottle;
  
  return (...args) => {
    if (!inThrottle) {
      fn(...args);
      inThrottle = true;
      setTimeout(() => inThrottle = false, limit);
    }
  };
};

const handleScroll = throttle(() => {
  console.log('Scrolling...');
}, 100);
```

### 4. Use Object Pools

**✅ Good**:
```javascript
class ObjectPool {
  constructor(createFn, resetFn) {
    this.createFn = createFn;
    this.resetFn = resetFn;
    this.pool = [];
  }

  acquire() {
    return this.pool.length > 0 
      ? this.pool.pop() 
      : this.createFn();
  }

  release(obj) {
    this.resetFn(obj);
    this.pool.push(obj);
  }
}
```

---

## Security Best Practices

### 1. Input Validation

**✅ Good**:
```javascript
const validateEmail = email => {
  const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
  return emailRegex.test(email);
};

const sanitizeInput = input => {
  return input.replace(/[<>]/g, '');
};

app.post('/api/users', (req, res) => {
  const { email, name } = req.body;
  
  if (!validateEmail(email)) {
    return res.status(400).json({ error: 'Invalid email' });
  }
  
  const safeName = sanitizeInput(name);
  // Process...
});
```

### 2. Prevent SQL Injection

**✅ Good**:
```javascript
// Use parameterized queries
const getUserById = async (id) => {
  const query = 'SELECT * FROM users WHERE id = $1';
  const result = await db.query(query, [id]);
  return result.rows[0];
};
```

**❌ Bad**:
```javascript
// String concatenation (vulnerable!)
const getUserById = async (id) => {
  const query = `SELECT * FROM users WHERE id = ${id}`;
  const result = await db.query(query);
  return result.rows[0];
};
```

### 3. Secure Password Handling

**✅ Good**:
```javascript
import bcrypt from 'bcrypt';

// Hash password
const hashPassword = async password => {
  const saltRounds = 10;
  return bcrypt.hash(password, saltRounds);
};

// Verify password
const verifyPassword = async (password, hash) => {
  return bcrypt.compare(password, hash);
};
```

### 4. Use Environment Variables

**✅ Good**:
```javascript
import dotenv from 'dotenv';
dotenv.config();

const config = {
  port: process.env.PORT || 3000,
  dbUrl: process.env.DATABASE_URL,
  jwtSecret: process.env.JWT_SECRET,
};

// Never log secrets
console.log('Port:', config.port);
// console.log('Secret:', config.jwtSecret);  ❌ Never!
```

### 5. Rate Limiting

**✅ Good**:
```javascript
import rateLimit from 'express-rate-limit';

const limiter = rateLimit({
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: 100, // limit each IP to 100 requests per windowMs
  message: 'Too many requests from this IP',
});

app.use('/api/', limiter);
```

---

## Error Handling

### 1. Use Try-Catch with Async/Await

**✅ Good**:
```javascript
async function fetchUserData(userId) {
  try {
    const response = await fetch(`/api/users/${userId}`);
    
    if (!response.ok) {
      throw new Error(`HTTP error! status: ${response.status}`);
    }
    
    const data = await response.json();
    return data;
  } catch (error) {
    console.error('Failed to fetch user:', error);
    throw error;  // Re-throw or handle
  }
}
```

### 2. Custom Error Classes

**✅ Good**:
```javascript
class ValidationError extends Error {
  constructor(message) {
    super(message);
    this.name = 'ValidationError';
    this.statusCode = 400;
  }
}

class NotFoundError extends Error {
  constructor(resource, id) {
    super(`${resource} with id ${id} not found`);
    this.name = 'NotFoundError';
    this.statusCode = 404;
  }
}

// Usage
const getUser = async id => {
  const user = await findUser(id);
  
  if (!user) {
    throw new NotFoundError('User', id);
  }
  
  return user;
};
```

### 3. Error Boundaries in Express

**✅ Good**:
```javascript
// Async error wrapper
const asyncHandler = fn => (req, res, next) => {
  Promise.resolve(fn(req, res, next)).catch(next);
};

// Route
app.get('/api/users/:id', asyncHandler(async (req, res) => {
  const user = await getUser(req.params.id);
  res.json(user);
}));

// Global error handler
app.use((err, req, res, next) => {
  console.error(err);
  
  res.status(err.statusCode || 500).json({
    error: {
      message: err.message,
      ...(process.env.NODE_ENV === 'development' && { stack: err.stack }),
    },
  });
});
```

---

## Async Patterns

### 1. Async/Await

**✅ Good**:
```javascript
// Sequential
async function processSequential() {
  const user = await fetchUser();
  const orders = await fetchOrders(user.id);
  const total = await calculateTotal(orders);
  return total;
}

// Parallel
async function processParallel() {
  const [users, products, orders] = await Promise.all([
    fetchUsers(),
    fetchProducts(),
    fetchOrders(),
  ]);
  
  return { users, products, orders };
}

// Error handling
async function processWithErrors() {
  try {
    const results = await Promise.allSettled([
      fetchUsers(),
      fetchProducts(),
      fetchOrders(),
    ]);
    
    return results.filter(r => r.status === 'fulfilled');
  } catch (error) {
    console.error('Error:', error);
  }
}
```

### 2. Promise Patterns

**✅ Good**:
```javascript
// Promise.all - All must succeed
const results = await Promise.all([
  fetch('/api/users'),
  fetch('/api/products'),
]);

// Promise.allSettled - Get all results (success or failure)
const results = await Promise.allSettled([
  fetch('/api/users'),
  fetch('/api/products'),
]);

// Promise.race - First to complete
const fastest = await Promise.race([
  fetch('/api/server1'),
  fetch('/api/server2'),
]);

// Promise.any - First to succeed
const first = await Promise.any([
  fetch('/api/server1'),
  fetch('/api/server2'),
]);
```

---

## Modern ES6+ Features

### 1. Classes

**✅ Good**:
```javascript
class User {
  #privateField;  // Private field
  
  constructor(name, email) {
    this.name = name;
    this.email = email;
    this.#privateField = 'private';
  }
  
  // Getter
  get displayName() {
    return this.name.toUpperCase();
  }
  
  // Setter
  set displayName(value) {
    this.name = value;
  }
  
  // Method
  greet() {
    return `Hello, ${this.name}!`;
  }
  
  // Static method
  static create(data) {
    return new User(data.name, data.email);
  }
}
```

### 2. Modules

**✅ Good**:
```javascript
// user.js
export class User { }
export const createUser = () => { };
export default User;

// main.js
import User, { createUser } from './user.js';
import * as userModule from './user.js';
```

---

## Common Gotchas

### 1. `this` Binding

```javascript
const obj = {
  name: 'Test',
  regularFunction() {
    console.log(this.name);  // 'Test'
  },
  arrowFunction: () => {
    console.log(this.name);  // undefined (lexical this)
  },
};
```

### 2. Truthy/Falsy Values

```javascript
// Falsy: false, 0, '', null, undefined, NaN
if (0) { }           // false
if ('') { }          // false
if (null) { }        // false
if (undefined) { }   // false

// Truthy: everything else
if ([]) { }          // true (empty array is truthy!)
if ({}) { }          // true (empty object is truthy!)
```

### 3. Type Coercion

```javascript
'5' + 3   // '53' (string concatenation)
'5' - 3   // 2 (number subtraction)
'5' == 5  // true (loose equality)
'5' === 5 // false (strict equality)
```

---

## Additional Resources

- [MDN JavaScript Guide](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide)
- [JavaScript.info](https://javascript.info/)
- [You Don't Know JS](https://github.com/getify/You-Dont-Know-JS)
- [Clean Code JavaScript](https://github.com/ryanmcdermott/clean-code-javascript)
- [Airbnb JavaScript Style Guide](https://github.com/airbnb/javascript)

---

**Version**: 1.0.0  
**Maintained by**: HiveLLM Governance Team  
**Last Updated**: 2025-10-11

