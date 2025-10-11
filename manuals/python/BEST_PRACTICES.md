# Python Best Practices

**Version**: 1.0.0  
**Last Updated**: 2025-10-11  
**Target Audience**: AI Agents developing Python projects

---

## Table of Contents

1. [Python Idioms](#python-idioms)
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

## Python Idioms

### 1. Use List/Dict/Set Comprehensions

```python
# ❌ BAD
numbers = []
for i in range(10):
    if i % 2 == 0:
        numbers.append(i)

# ✅ GOOD
numbers = [i for i in range(10) if i % 2 == 0]
```

### 2. Use Context Managers

```python
# ❌ BAD
f = open('file.txt', 'r')
content = f.read()
f.close()

# ✅ GOOD
with open('file.txt', 'r') as f:
    content = f.read()
# File automatically closed
```

### 3. Use f-strings for Formatting

```python
# ❌ BAD
message = "Hello, %s!" % name
message = "Hello, {}!".format(name)
message = "Hello, " + name + "!"

# ✅ GOOD
message = f"Hello, {name}!"
result = f"Result: {calculate() * 100:.2f}%"
```

### 4. Use Pathlib Instead of os.path

```python
# ❌ BAD
import os
path = os.path.join(os.path.dirname(__file__), 'data', 'file.txt')
if os.path.exists(path):
    with open(path) as f:
        data = f.read()

# ✅ GOOD
from pathlib import Path
path = Path(__file__).parent / 'data' / 'file.txt'
if path.exists():
    data = path.read_text()
```

### 5. Use Generators for Large Datasets

```python
# ❌ BAD: Loads everything in memory
def get_all_users():
    users = database.query_all()  # Loads 1M users
    return users

# ✅ GOOD: Generator for memory efficiency
def get_all_users():
    for batch in database.query_batches(size=1000):
        yield from batch
```

### 6. Use dataclasses or Pydantic

```python
# ❌ BAD
class User:
    def __init__(self, id, name, email):
        self.id = id
        self.name = name
        self.email = email

# ✅ GOOD: dataclass
from dataclasses import dataclass

@dataclass
class User:
    id: str
    name: str
    email: str

# ✅ BETTER: Pydantic (with validation)
from pydantic import BaseModel, EmailStr

class User(BaseModel):
    id: str
    name: str
    email: EmailStr
```

### 7. Use Enums for Constants

```python
# ❌ BAD
STATUS_PENDING = 'pending'
STATUS_APPROVED = 'approved'
STATUS_REJECTED = 'rejected'

# ✅ GOOD
from enum import Enum

class Status(str, Enum):
    PENDING = 'pending'
    APPROVED = 'approved'
    REJECTED = 'rejected'
```

### 8. Use Type Hints

```python
# ❌ BAD
def process_data(data):
    return data.upper()

# ✅ GOOD
def process_data(data: str) -> str:
    return data.upper()

# ✅ BETTER: With modern syntax (Python 3.10+)
def get_user(user_id: str) -> User | None:
    return database.find(user_id)
```

### 9. Use `__slots__` for Performance-Critical Classes

```python
# ✅ GOOD: Reduces memory usage
class Point:
    __slots__ = ('x', 'y')
    
    def __init__(self, x: float, y: float) -> None:
        self.x = x
        self.y = y
```

### 10. Use `itertools` for Efficient Iteration

```python
from itertools import islice, chain, groupby

# Efficient chunking
def chunks(iterable, size):
    it = iter(iterable)
    return iter(lambda: list(islice(it, size)), [])

# Chain multiple iterables
combined = chain(list1, list2, list3)

# Group by key
from operator import itemgetter
data = [{'key': 'a', 'val': 1}, {'key': 'a', 'val': 2}]
for key, group in groupby(data, key=itemgetter('key')):
    print(key, list(group))
```

---

## Anti-Patterns

### 1. Mutable Default Arguments

```python
# ❌ BAD: Mutable default is shared across calls
def add_item(item, items=[]):
    items.append(item)
    return items

# ✅ GOOD
def add_item(item, items=None):
    if items is None:
        items = []
    items.append(item)
    return items
```

### 2. Bare `except` Clauses

```python
# ❌ BAD: Catches everything, including KeyboardInterrupt
try:
    risky_operation()
except:
    pass

# ✅ GOOD: Specific exceptions
try:
    risky_operation()
except ValueError as e:
    logger.error(f"Invalid value: {e}")
except Exception as e:
    logger.error(f"Unexpected error: {e}")
    raise
```

### 3. Not Using `with` for Resources

```python
# ❌ BAD
file = open('data.txt')
data = file.read()
file.close()

# ✅ GOOD
with open('data.txt') as file:
    data = file.read()
```

### 4. Using `eval()` or `exec()`

```python
# ❌ BAD: Security risk
result = eval(user_input)

# ✅ GOOD: Use safer alternatives
import json
result = json.loads(user_input)

# Or use ast.literal_eval for Python literals
from ast import literal_eval
result = literal_eval(user_input)
```

### 5. String Concatenation in Loops

```python
# ❌ BAD: Inefficient
result = ""
for item in items:
    result += str(item)

# ✅ GOOD: Join
result = "".join(str(item) for item in items)
```

### 6. Not Using `is` for None Checks

```python
# ❌ BAD
if value == None:
    pass

# ✅ GOOD
if value is None:
    pass
```

### 7. Catching Exception Too Broadly

```python
# ❌ BAD
try:
    process_data()
except Exception:
    pass  # Silently fails

# ✅ GOOD
try:
    process_data()
except ValidationError as e:
    logger.warning(f"Validation failed: {e}")
    return None
except Exception as e:
    logger.error(f"Unexpected error: {e}")
    raise
```

---

## Performance Optimization

### 1. Use Built-in Functions

```python
# ❌ SLOW
total = 0
for num in numbers:
    total += num

# ✅ FAST: Built-in sum()
total = sum(numbers)
```

### 2. Use Sets for Membership Testing

```python
# ❌ SLOW: O(n)
items = ['a', 'b', 'c', 'd']
if 'c' in items:
    pass

# ✅ FAST: O(1)
items = {'a', 'b', 'c', 'd'}
if 'c' in items:
    pass
```

### 3. Use `__slots__` for Many Objects

```python
# Regular class: ~56 bytes per instance
class Regular:
    def __init__(self, x, y):
        self.x = x
        self.y = y

# With __slots__: ~40 bytes per instance
class Optimized:
    __slots__ = ('x', 'y')
    
    def __init__(self, x, y):
        self.x = x
        self.y = y
```

### 4. Use `lru_cache` for Expensive Functions

```python
from functools import lru_cache

@lru_cache(maxsize=128)
def fibonacci(n: int) -> int:
    if n < 2:
        return n
    return fibonacci(n-1) + fibonacci(n-2)
```

### 5. Avoid Global Variables

```python
# ❌ BAD
counter = 0

def increment():
    global counter
    counter += 1

# ✅ GOOD: Use class or function closure
class Counter:
    def __init__(self):
        self._value = 0
    
    def increment(self):
        self._value += 1
```

### 6. Use Generators for Large Data

```python
# ❌ BAD: Loads all in memory
def read_large_file(path):
    return [line for line in open(path)]

# ✅ GOOD: Generator
def read_large_file(path):
    with open(path) as f:
        for line in f:
            yield line.strip()
```

---

## Security Best Practices

### 1. Input Validation with Pydantic

```python
from pydantic import BaseModel, EmailStr, validator

class CreateUserRequest(BaseModel):
    email: EmailStr
    password: str
    age: int
    
    @validator('password')
    def validate_password(cls, v):
        if len(v) < 8:
            raise ValueError('Password must be at least 8 characters')
        return v
    
    @validator('age')
    def validate_age(cls, v):
        if v < 0 or v > 150:
            raise ValueError('Invalid age')
        return v
```

### 2. SQL Injection Prevention

```python
# ❌ BAD: SQL injection vulnerability
def get_user(user_id):
    query = f"SELECT * FROM users WHERE id = '{user_id}'"
    return database.execute(query)

# ✅ GOOD: Parameterized queries
def get_user(user_id: str) -> User | None:
    query = "SELECT * FROM users WHERE id = %s"
    return database.execute(query, (user_id,))

# ✅ BETTER: Use ORM
def get_user(user_id: str) -> User | None:
    return session.query(User).filter_by(id=user_id).first()
```

### 3. Secrets Management

```python
# ❌ BAD: Hardcoded secrets
API_KEY = 'sk-1234567890abcdef'

# ✅ GOOD: Environment variables
from os import getenv
API_KEY = getenv('API_KEY')

# ✅ BETTER: Validated settings
from pydantic_settings import BaseSettings

class Settings(BaseSettings):
    api_key: str
    
    class Config:
        env_file = '.env'

settings = Settings()
```

### 4. Path Traversal Prevention

```python
from pathlib import Path

def read_file(filename: str) -> str:
    base_dir = Path('/app/data')
    file_path = (base_dir / filename).resolve()
    
    # Ensure path is within base_dir
    if not file_path.is_relative_to(base_dir):
        raise ValueError("Invalid file path")
    
    return file_path.read_text()
```

### 5. Rate Limiting

```python
from slowapi import Limiter
from slowapi.util import get_remote_address

limiter = Limiter(key_func=get_remote_address)

@app.post("/api/users")
@limiter.limit("5/minute")
async def create_user(request: Request, data: CreateUserRequest):
    pass
```

---

## Error Handling

### 1. Custom Exception Hierarchy

```python
class AppError(Exception):
    """Base application error"""
    
    def __init__(
        self,
        message: str,
        code: str,
        status_code: int = 500
    ) -> None:
        super().__init__(message)
        self.code = code
        self.status_code = status_code


class ValidationError(AppError):
    """Validation error"""
    
    def __init__(self, message: str) -> None:
        super().__init__(message, "VALIDATION_ERROR", 400)


class NotFoundError(AppError):
    """Resource not found"""
    
    def __init__(self, resource: str, resource_id: str) -> None:
        super().__init__(
            f"{resource} with id {resource_id} not found",
            "NOT_FOUND",
            404
        )
```

### 2. Exception Handling Best Practices

```python
# ✅ GOOD: Specific exception handling
try:
    result = risky_operation()
except ValidationError as e:
    logger.warning(f"Validation failed: {e}")
    return None
except ConnectionError as e:
    logger.error(f"Connection failed: {e}")
    raise
except Exception as e:
    logger.error(f"Unexpected error: {e}", exc_info=True)
    raise
```

### 3. Context Manager for Cleanup

```python
from contextlib import contextmanager

@contextmanager
def database_transaction():
    """Transaction context manager"""
    session = Session()
    try:
        yield session
        session.commit()
    except Exception:
        session.rollback()
        raise
    finally:
        session.close()

# Usage
with database_transaction() as session:
    session.add(user)
```

---

## Async/Await Patterns

### 1. Use async/await for I/O Operations

```python
# ✅ GOOD: Async for I/O
async def fetch_user(user_id: str) -> User:
    async with httpx.AsyncClient() as client:
        response = await client.get(f"/users/{user_id}")
        return User(**response.json())
```

### 2. Parallel Execution with asyncio.gather

```python
# ❌ BAD: Sequential
async def get_data():
    user = await fetch_user()
    posts = await fetch_posts()
    return user, posts

# ✅ GOOD: Parallel
import asyncio

async def get_data():
    user, posts = await asyncio.gather(
        fetch_user(),
        fetch_posts()
    )
    return user, posts
```

### 3. Async Context Managers

```python
from contextlib import asynccontextmanager

@asynccontextmanager
async def lifespan(app):
    """Application lifespan"""
    # Startup
    await database.connect()
    yield
    # Shutdown
    await database.disconnect()

# FastAPI usage
app = FastAPI(lifespan=lifespan)
```

---

## Type Safety

### 1. Use Type Hints Everywhere

```python
from typing import List, Dict, Optional, Union
from collections.abc import Sequence

def process_users(
    users: Sequence[User],
    filters: Dict[str, str] | None = None
) -> List[User]:
    """Process users with optional filters"""
    pass
```

### 2. Use TypedDict for Dictionaries

```python
from typing import TypedDict

class UserDict(TypedDict):
    id: str
    name: str
    email: str
    age: int

def create_user(data: UserDict) -> User:
    pass
```

### 3. Use Protocol for Structural Typing

```python
from typing import Protocol

class Drawable(Protocol):
    """Anything that can be drawn"""
    
    def draw(self) -> None:
        ...

def render(obj: Drawable) -> None:
    obj.draw()  # Type-safe
```

### 4. Use NewType for Type Safety

```python
from typing import NewType

UserId = NewType('UserId', str)
Email = NewType('Email', str)

def get_user(user_id: UserId) -> User:
    pass

def send_email(email: Email, subject: str) -> None:
    pass

# Type-safe usage
user_id = UserId('user-123')
email = Email('user@example.com')

get_user(user_id)  # ✅ OK
get_user(email)    # ❌ Type error
```

---

## Code Organization

### 1. Follow PEP 8

- Line length: 100 characters (or 88 for Black)
- Indentation: 4 spaces
- Blank lines: 2 between top-level definitions
- Import order: stdlib → third-party → local

### 2. Import Organization

```python
# Standard library
import os
import sys
from pathlib import Path
from typing import List, Dict

# Third-party
import fastapi
import pydantic
from sqlalchemy import create_engine

# Local
from src.core.entities import User
from src.services.user_service import UserService
from src.utils.exceptions import NotFoundError
```

### 3. Module Structure

```python
"""Module docstring describing the module.

This module provides functionality for X, Y, and Z.
"""

from typing import Optional

# Constants
MAX_RETRIES = 3
DEFAULT_TIMEOUT = 30

# Type definitions
UserId = str

# Functions
def public_function() -> None:
    """Public function"""
    pass

def _private_function() -> None:
    """Private function"""
    pass

# Classes
class PublicClass:
    """Public class"""
    pass
```

---

## Testing Best Practices

### 1. Use pytest Fixtures

```python
import pytest
from src.services.user_service import UserService

@pytest.fixture
def user_service():
    """Create UserService instance"""
    return UserService()

@pytest.fixture
def sample_user():
    """Create sample user data"""
    return {
        "email": "test@example.com",
        "name": "Test User"
    }

def test_create_user(user_service, sample_user):
    """Test user creation"""
    result = user_service.create(sample_user)
    assert result.email == sample_user["email"]
```

### 2. Use Parametrize for Multiple Test Cases

```python
import pytest

@pytest.mark.parametrize("input,expected", [
    ("hello", "HELLO"),
    ("world", "WORLD"),
    ("", ""),
])
def test_uppercase(input, expected):
    assert input.upper() == expected
```

### 3. Use Mocks Appropriately

```python
from unittest.mock import Mock, patch

def test_api_call():
    """Test API call with mock"""
    with patch('requests.get') as mock_get:
        mock_get.return_value.json.return_value = {"data": "value"}
        result = fetch_data()
        assert result == {"data": "value"}
```

---

## Common Gotchas

### 1. Mutable Default Arguments

See [Anti-Patterns](#anti-patterns) section.

### 2. Late Binding Closures

```python
# ❌ BAD: All functions reference same i
funcs = [lambda: i for i in range(5)]
[f() for f in funcs]  # [4, 4, 4, 4, 4]

# ✅ GOOD: Capture current value
funcs = [lambda i=i: i for i in range(5)]
[f() for f in funcs]  # [0, 1, 2, 3, 4]
```

### 3. Dictionary Iteration Changes

```python
# ❌ BAD: Can't modify dict during iteration
d = {'a': 1, 'b': 2}
for key in d:
    if key == 'a':
        del d[key]  # RuntimeError

# ✅ GOOD: Iterate over copy
for key in list(d.keys()):
    if key == 'a':
        del d[key]
```

### 4. Integer Division

```python
# Python 3
result = 5 / 2   # 2.5 (float division)
result = 5 // 2  # 2 (integer division)
```

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-10-11 | Initial Python best practices guide |

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

