# AI Integration Manual - Python

**Version**: 1.0.0  
**Last Updated**: 2025-10-11  
**Language**: Python 3.12+  
**Target Audience**: AI Agents (LLM-based development assistants)

---

## Table of Contents

1. [Introduction](#introduction)
2. [Quick Start](#quick-start)
3. [Python-Specific Setup](#python-specific-setup)
4. [Configuration Standards](#configuration-standards)
5. [Source Code Standards](#source-code-standards)
6. [Testing Standards](#testing-standards)
7. [Build & Deployment](#build--deployment)
8. [Documentation](#documentation)
9. [Python Best Practices](#python-best-practices)
10. [Common Patterns](#common-patterns)
11. [Troubleshooting](#troubleshooting)
12. [Complete Workflow](#complete-workflow)

---

## Introduction

This manual extends the [AI Integration Manual Template](../templates/AI_INTEGRATION_MANUAL_TEMPLATE.md) with Python-specific implementations.

**When to use this manual**:
- Python applications (web, CLI, data science, ML)
- Backend APIs (FastAPI, Flask, Django)
- Data processing and analysis
- Machine learning projects
- Automation scripts
- Libraries/SDKs

**Prerequisites**:
- Read [AI Integration Manual Template](../templates/AI_INTEGRATION_MANUAL_TEMPLATE.md)
- Read [Language Standards](../LANGUAGE_STANDARDS.md)
- Basic Python knowledge

---

## Quick Start

### Minimum Viable Project Setup

```bash
# 1. Create project directory
mkdir my-project && cd my-project

# 2. Create virtual environment
python -m venv venv

# 3. Activate virtual environment
# Windows
venv\Scripts\activate
# Linux/Mac
source venv/bin/activate

# 4. Create project structure
mkdir -p src tests docs

# 5. Create pyproject.toml
cat > pyproject.toml << EOF
[project]
name = "my-project"
version = "0.1.0"
description = "My Python Project"
requires-python = ">=3.12"
dependencies = []

[project.optional-dependencies]
dev = [
    "pytest>=8.0.0",
    "pytest-cov>=4.1.0",
    "ruff>=0.1.0",
    "black>=24.0.0",
    "mypy>=1.8.0",
]
EOF

# 6. Install dev dependencies
pip install -e ".[dev]"

# 7. Create basic structure
touch src/__init__.py
touch tests/__init__.py
```

---

## Python-Specific Setup

### 1. Environment Setup

#### Install Python Version Manager

**Using pyenv (Recommended)**:

```bash
# Install pyenv (Unix/Mac)
curl https://pyenv.run | bash

# Install pyenv (Windows)
# Use: https://github.com/pyenv-win/pyenv-win

# Install Python 3.12
pyenv install 3.12
pyenv global 3.12

# Verify installation
python --version  # Should show Python 3.12.x
pip --version
```

#### Create .python-version file

```bash
echo "3.12" > .python-version
```

### 2. Package Manager Selection

**Recommended**: Use **ONE** package manager for consistency.

| Manager | Speed | Lock File | Notes |
|---------|-------|-----------|-------|
| **pip** | Good | `requirements.txt` | Standard, universal |
| **poetry** | Good | `poetry.lock` | Dependency resolver, packaging |
| **pipenv** | Moderate | `Pipfile.lock` | virtualenv + pip combined |
| **uv** | Excellent | `uv.lock` | Fastest (Rust-based) |

**This manual uses pip + pyproject.toml**, but commands are adaptable:

```bash
pip install package    →  poetry add package    →  pipenv install package
pip install -e ".[dev]" →  poetry install       →  pipenv install --dev
```

### 3. Essential Dependencies

#### Production Dependencies

```bash
# Web Framework (choose one)
pip install fastapi      # Modern, fast, async
pip install flask        # Lightweight, simple
pip install django       # Full-featured

# Database
pip install sqlalchemy   # ORM
pip install psycopg2-binary  # PostgreSQL
pip install pymongo      # MongoDB

# Utilities
pip install pydantic     # Data validation
pip install python-dotenv  # Environment variables
pip install structlog    # Structured logging
```

#### Development Dependencies

```bash
# Testing
pip install pytest pytest-cov pytest-asyncio pytest-mock

# Linting and Formatting
pip install ruff         # Fast linter (Rust-based)
pip install black        # Code formatter
pip install isort        # Import sorting

# Type Checking
pip install mypy         # Static type checker
pip install types-*      # Type stubs (as needed)

# Development
pip install ipython      # Better REPL
pip install ipdb         # Debugger
```

---

## Configuration Standards

### 1. pyproject.toml

**Mandatory Configuration**:

```toml
[project]
name = "my-project"
version = "1.0.0"
description = "Project description"
readme = "README.md"
requires-python = ">=3.12"
license = {text = "MIT"}
authors = [
    {name = "Author Name", email = "author@example.com"}
]
keywords = ["python", "api", "fastapi"]
classifiers = [
    "Development Status :: 4 - Beta",
    "Intended Audience :: Developers",
    "License :: OSI Approved :: MIT License",
    "Programming Language :: Python :: 3",
    "Programming Language :: Python :: 3.12",
]

dependencies = [
    "fastapi>=0.109.0",
    "uvicorn[standard]>=0.27.0",
    "pydantic>=2.5.0",
    "sqlalchemy>=2.0.0",
]

[project.optional-dependencies]
dev = [
    "pytest>=8.0.0",
    "pytest-cov>=4.1.0",
    "pytest-asyncio>=0.23.0",
    "pytest-mock>=3.12.0",
    "ruff>=0.1.0",
    "black>=24.0.0",
    "isort>=5.13.0",
    "mypy>=1.8.0",
    "ipython>=8.20.0",
]

[project.urls]
Homepage = "https://github.com/user/repo"
Documentation = "https://docs.example.com"
Repository = "https://github.com/user/repo"
Issues = "https://github.com/user/repo/issues"

[project.scripts]
my-project = "my_project.cli:main"

[build-system]
requires = ["setuptools>=68.0.0", "wheel"]
build-backend = "setuptools.build_meta"

[tool.setuptools.packages.find]
where = ["src"]

[tool.pytest.ini_options]
minversion = "8.0"
testpaths = ["tests"]
python_files = ["test_*.py", "*_test.py"]
python_classes = ["Test*"]
python_functions = ["test_*"]
addopts = [
    "--cov=src",
    "--cov-report=term-missing",
    "--cov-report=html",
    "--cov-report=xml",
    "--cov-fail-under=90",
    "--strict-markers",
    "--strict-config",
    "--verbose",
]

[tool.coverage.run]
source = ["src"]
omit = ["*/tests/*", "*/test_*.py"]

[tool.coverage.report]
exclude_lines = [
    "pragma: no cover",
    "def __repr__",
    "raise AssertionError",
    "raise NotImplementedError",
    "if __name__ == .__main__.:",
    "if TYPE_CHECKING:",
    "@abstractmethod",
]

[tool.ruff]
line-length = 100
target-version = "py312"
src = ["src", "tests"]

[tool.ruff.lint]
select = [
    "E",      # pycodestyle errors
    "W",      # pycodestyle warnings
    "F",      # pyflakes
    "I",      # isort
    "B",      # flake8-bugbear
    "C4",     # flake8-comprehensions
    "UP",     # pyupgrade
    "ARG",    # flake8-unused-arguments
    "SIM",    # flake8-simplify
]
ignore = []

[tool.ruff.lint.per-file-ignores]
"__init__.py" = ["F401"]  # Allow unused imports in __init__.py
"tests/**/*.py" = ["ARG"]  # Allow unused arguments in tests

[tool.black]
line-length = 100
target-version = ["py312"]
include = '\.pyi?$'
exclude = '''
/(
    \.git
  | \.venv
  | venv
  | build
  | dist
)/
'''

[tool.isort]
profile = "black"
line_length = 100
skip_gitignore = true

[tool.mypy]
python_version = "3.12"
strict = true
warn_return_any = true
warn_unused_configs = true
disallow_untyped_defs = true
disallow_any_generics = true
disallow_subclassing_any = true
disallow_incomplete_defs = true
check_untyped_defs = true
no_implicit_optional = true
warn_redundant_casts = true
warn_unused_ignores = true
warn_no_return = true
warn_unreachable = true
strict_equality = true

[[tool.mypy.overrides]]
module = "tests.*"
disallow_untyped_defs = false
```

### 2. requirements.txt (Alternative to pyproject.toml)

If using `requirements.txt` instead:

```txt
# requirements.txt (production)
fastapi==0.109.0
uvicorn[standard]==0.27.0
pydantic==2.5.0
sqlalchemy==2.0.0
python-dotenv==1.0.0
```

```txt
# requirements-dev.txt
-r requirements.txt
pytest==8.0.0
pytest-cov==4.1.0
pytest-asyncio==0.23.0
ruff==0.1.0
black==24.0.0
mypy==1.8.0
```

**Install**:
```bash
pip install -r requirements.txt
pip install -r requirements-dev.txt
```

### 3. Environment Configuration

**.env.example**:

```env
# Application
ENVIRONMENT=development
DEBUG=true
LOG_LEVEL=DEBUG

# Server
HOST=0.0.0.0
PORT=8000

# Database
DATABASE_URL=postgresql://user:pass@localhost:5432/dbname
DATABASE_POOL_SIZE=10
DATABASE_POOL_MAX_OVERFLOW=20

# Redis
REDIS_URL=redis://localhost:6379/0

# External APIs
API_KEY=your-api-key-here
API_SECRET=your-api-secret-here

# Task Queue
TASK_QUEUE_URL=http://localhost:8080
TASK_QUEUE_TOKEN=your-token-here

# Vectorizer
VECTORIZER_URL=http://localhost:9000
VECTORIZER_TOKEN=your-token-here
```

**Load environment variables** (src/config.py):

```python
from pydantic_settings import BaseSettings
from functools import lru_cache

class Settings(BaseSettings):
    """Application settings"""
    
    environment: str = "development"
    debug: bool = False
    log_level: str = "INFO"
    host: str = "0.0.0.0"
    port: int = 8000
    database_url: str
    redis_url: str | None = None
    
    class Config:
        env_file = ".env"
        env_file_encoding = "utf-8"
        case_sensitive = False

@lru_cache
def get_settings() -> Settings:
    return Settings()
```

### 4. Editor Configuration

**.editorconfig**:

```ini
root = true

[*]
charset = utf-8
end_of_line = lf
insert_final_newline = true
trim_trailing_whitespace = true

[*.py]
indent_style = space
indent_size = 4

[*.{yml,yaml,json}]
indent_style = space
indent_size = 2

[*.md]
trim_trailing_whitespace = false
```

---

## Source Code Standards

### 1. Directory Structure

```
src/
├── __init__.py
├── main.py                # Application entry point
├── config.py              # Configuration
├── core/                  # Core business logic
│   ├── __init__.py
│   ├── entities/         # Domain entities
│   ├── use_cases/        # Use case implementations
│   └── interfaces/       # Abstract interfaces
├── infrastructure/        # External concerns
│   ├── __init__.py
│   ├── database/         # Database implementation
│   ├── http/             # HTTP server
│   └── queue/            # Message queue
├── api/                   # API layer
│   ├── __init__.py
│   ├── routes/           # Route definitions
│   ├── dependencies.py   # FastAPI dependencies
│   └── schemas.py        # Pydantic schemas
├── services/             # Application services
│   └── __init__.py
├── repositories/         # Data access layer
│   └── __init__.py
├── models/              # Data models/DTOs
│   └── __init__.py
└── utils/               # Utility functions
    └── __init__.py
```

### 2. Naming Conventions

| Element | Convention | Example |
|---------|-----------|---------|
| **Files** | snake_case | `user_service.py` |
| **Modules** | snake_case | `my_module` |
| **Classes** | PascalCase | `UserService` |
| **Functions** | snake_case | `create_user()` |
| **Variables** | snake_case | `user_id` |
| **Constants** | UPPER_SNAKE_CASE | `MAX_RETRY_COUNT` |
| **Private** | _leading_underscore | `_internal_method()` |

### 3. Type Hints

**Always use type hints** (PEP 484):

```python
from typing import Optional, List, Dict, Any
from datetime import datetime

def get_user(user_id: str) -> Optional[User]:
    """Get user by ID"""
    pass

async def create_user(email: str, name: str) -> User:
    """Create a new user"""
    pass

def process_items(items: List[Dict[str, Any]]) -> List[str]:
    """Process a list of items"""
    pass
```

### 4. Docstrings

**Use Google-style docstrings** (PEP 257):

```python
def process_payment(user_id: str, amount: int, currency: str) -> PaymentResult:
    """Process a user payment.

    Args:
        user_id: The user's unique identifier
        amount: Payment amount in cents (must be positive)
        currency: Three-letter currency code (e.g., 'USD', 'EUR')

    Returns:
        Payment confirmation with transaction details

    Raises:
        ValidationError: If amount is negative or currency is invalid
        InsufficientFundsError: If user balance is insufficient
        PaymentGatewayError: If payment gateway is unavailable

    Examples:
        >>> result = process_payment('user-123', 1000, 'USD')
        >>> print(result.transaction_id)
        'txn-abc123'
    """
    pass
```

### 5. Error Handling

**Custom Exception Classes**:

```python
# utils/exceptions.py
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
        self.message = message


class ValidationError(AppError):
    """Validation error"""
    
    def __init__(self, message: str) -> None:
        super().__init__(message, "VALIDATION_ERROR", 400)


class NotFoundError(AppError):
    """Resource not found error"""
    
    def __init__(self, resource: str, resource_id: str) -> None:
        message = f"{resource} with id {resource_id} not found"
        super().__init__(message, "NOT_FOUND", 404)


class UnauthorizedError(AppError):
    """Unauthorized access error"""
    
    def __init__(self, message: str = "Unauthorized") -> None:
        super().__init__(message, "UNAUTHORIZED", 401)
```

---

## Testing Standards

### 1. Test Structure

```
tests/
├── __init__.py
├── unit/                          # Unit tests
│   ├── __init__.py
│   ├── test_services.py
│   ├── test_utils.py
│   └── test_use_cases.py
├── integration/                   # Integration tests
│   ├── __init__.py
│   ├── test_api.py
│   └── test_repository.py
├── e2e/                          # End-to-end tests
│   ├── __init__.py
│   └── test_user_flows.py
├── fixtures/                      # Test data
│   ├── __init__.py
│   └── users.py
├── conftest.py                   # Pytest configuration & fixtures
└── helpers.py                    # Test utilities
```

### 2. Pytest Configuration

**In pyproject.toml** (see above) or **pytest.ini**:

```ini
[pytest]
minversion = 8.0
testpaths = tests
python_files = test_*.py *_test.py
python_classes = Test*
python_functions = test_*
addopts =
    --cov=src
    --cov-report=term-missing
    --cov-report=html
    --cov-report=xml
    --cov-fail-under=90
    --strict-markers
    --strict-config
    --verbose
markers =
    slow: marks tests as slow
    integration: marks tests as integration tests
    e2e: marks tests as end-to-end tests
```

### 3. Test Example Structure

```python
# tests/unit/test_user_service.py
import pytest
from unittest.mock import Mock, AsyncMock
from src.services.user_service import UserService
from src.utils.exceptions import ValidationError

class TestUserService:
    """Test suite for UserService"""
    
    @pytest.fixture
    def mock_repository(self):
        """Create mock repository"""
        return Mock()
    
    @pytest.fixture
    def user_service(self, mock_repository):
        """Create UserService with mocked dependencies"""
        return UserService(repository=mock_repository)
    
    def test_create_user_success(self, user_service, mock_repository):
        """Test successful user creation"""
        # Arrange
        user_data = {"email": "test@example.com", "name": "Test User"}
        expected = {"id": "123", **user_data}
        mock_repository.create.return_value = expected
        
        # Act
        result = user_service.create_user(user_data)
        
        # Assert
        assert result == expected
        mock_repository.create.assert_called_once_with(user_data)
    
    def test_create_user_invalid_email(self, user_service):
        """Test user creation with invalid email"""
        # Arrange
        user_data = {"email": "invalid", "name": "Test"}
        
        # Act & Assert
        with pytest.raises(ValidationError, match="Invalid email"):
            user_service.create_user(user_data)
```

### 4. Coverage Requirements

- **Overall**: > 90%
- **Unit Tests**: > 95%
- **Integration Tests**: > 85%
- **Critical Paths**: 100%

**Check coverage**:

```bash
pytest --cov --cov-report=html
open htmlcov/index.html
```

---

## Build & Deployment

### 1. Build Process

```bash
# Install build tools
pip install build

# Build distribution
python -m build

# Output
dist/
├── my_project-1.0.0-py3-none-any.whl
└── my_project-1.0.0.tar.gz
```

### 2. Production Optimization

**Create .dockerignore**:

```
__pycache__
*.pyc
*.pyo
*.pyd
.Python
venv/
.venv/
.pytest_cache/
.coverage
htmlcov/
dist/
build/
*.egg-info/
.git/
.env
*.log
```

**Dockerfile**:

```dockerfile
# Build stage
FROM python:3.12-slim as builder

WORKDIR /app

# Install build dependencies
RUN pip install --no-cache-dir build

# Copy source
COPY pyproject.toml README.md ./
COPY src/ ./src/

# Build wheel
RUN python -m build

# Production stage
FROM python:3.12-slim

WORKDIR /app

# Copy wheel from builder
COPY --from=builder /app/dist/*.whl ./

# Install wheel
RUN pip install --no-cache-dir *.whl && rm *.whl

# Create non-root user
RUN useradd -m -u 1000 appuser && chown -R appuser:appuser /app
USER appuser

# Expose port
EXPOSE 8000

# Run application
CMD ["uvicorn", "my_project.main:app", "--host", "0.0.0.0", "--port", "8000"]
```

### 3. Package Publishing (PyPI)

```bash
# Install twine
pip install twine

# Build package
python -m build

# Check package
twine check dist/*

# Upload to TestPyPI (test first)
twine upload --repository testpypi dist/*

# Upload to PyPI
twine upload dist/*
```

**Before publishing checklist**:
- [ ] All tests passing
- [ ] Coverage > 90%
- [ ] No linter errors
- [ ] Documentation complete
- [ ] CHANGELOG updated
- [ ] Version bumped
- [ ] Git tag created

---

## Documentation

### 1. Docstrings (Google Style)

```python
def function_name(param1: str, param2: int) -> bool:
    """Short description (one line).

    Longer description if needed. Can span multiple lines.
    Explain what the function does in detail.

    Args:
        param1: Description of param1
        param2: Description of param2

    Returns:
        Description of return value

    Raises:
        ValueError: If param2 is negative
        TypeError: If param1 is not a string

    Examples:
        >>> function_name("hello", 42)
        True
    """
    pass
```

### 2. Sphinx Documentation

**Install Sphinx**:

```bash
pip install sphinx sphinx-rtd-theme sphinx-autodoc-typehints
```

**docs/conf.py**:

```python
import os
import sys
sys.path.insert(0, os.path.abspath('../src'))

project = 'My Project'
copyright = '2025, Author Name'
author = 'Author Name'
version = '1.0.0'

extensions = [
    'sphinx.ext.autodoc',
    'sphinx.ext.napoleon',
    'sphinx.ext.viewcode',
    'sphinx.ext.intersphinx',
    'sphinx_autodoc_typehints',
]

templates_path = ['_templates']
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']

html_theme = 'sphinx_rtd_theme'
html_static_path = ['_static']

autodoc_typehints = 'description'
napoleon_google_docstring = True
napoleon_numpy_docstring = False
```

**Generate documentation**:

```bash
cd docs
sphinx-apidoc -o . ../src
make html
```

### 3. Alternative: pdoc

```bash
# Install pdoc
pip install pdoc

# Generate documentation
pdoc --html --output-dir docs/api src/

# Serve documentation
pdoc --http :8080 src/
```

---

## Python Best Practices

See [BEST_PRACTICES.md](BEST_PRACTICES.md) for complete guide.

### Quick Tips

1. **Use type hints everywhere** (PEP 484, 585, 604)
2. **Follow PEP 8** style guide
3. **Use dataclasses** or Pydantic for data structures
4. **Prefer `pathlib`** over `os.path`
5. **Use context managers** (`with` statement)
6. **Use list/dict comprehensions** appropriately
7. **Prefer `f-strings`** for formatting
8. **Use `async/await`** for I/O operations
9. **Import order**: stdlib → third-party → local
10. **One import per line**

---

## Common Patterns

Refer to [Source Code Standards](#source-code-standards) and [BEST_PRACTICES.md](BEST_PRACTICES.md) for:
- Repository Pattern
- Service Layer Pattern
- Dependency Injection
- Factory Pattern
- Singleton Pattern
- Context Managers
- Async patterns

---

## Troubleshooting

### Common Python Issues

#### Issue: ImportError / ModuleNotFoundError

**Solution**:
```bash
# Ensure package is installed
pip install package-name

# Verify in correct virtual environment
which python  # Should point to venv
```

#### Issue: Type checking errors (mypy)

**Solution**:
```bash
# Install type stubs
pip install types-package-name

# Or ignore specific errors
# type: ignore[error-code]
```

#### Issue: Tests fail with import errors

**Solution**:
```bash
# Install package in editable mode
pip install -e .

# Or add src to PYTHONPATH
export PYTHONPATH=$PYTHONPATH:$(pwd)/src
```

---

## Complete Workflow

Follow the [AI Integration Manual Template](../templates/AI_INTEGRATION_MANUAL_TEMPLATE.md) for the complete 6-phase workflow, using Python-specific configurations and commands from this manual.

### Quick Reference

1. **Planning**: Use templates from `gov/manuals/templates/`
2. **Setup**: Follow [Python-Specific Setup](#python-specific-setup)
3. **Implementation**: Follow [Source Code Standards](#source-code-standards)
4. **Testing**: Follow [Testing Standards](#testing-standards)
5. **Build**: Follow [Build & Deployment](#build--deployment)
6. **Documentation**: Follow [Documentation](#documentation)

---

## Additional Resources

- [Python Official Documentation](https://docs.python.org/3/)
- [PEP 8 - Style Guide](https://peps.python.org/pep-0008/)
- [FastAPI Documentation](https://fastapi.tiangolo.com/)
- [pytest Documentation](https://docs.pytest.org/)
- [Pydantic Documentation](https://docs.pydantic.dev/)
- [BEST_PRACTICES.md](BEST_PRACTICES.md) - This project's best practices

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-10-11 | Initial Python manual |

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

