# AI Integration Manual - Common Lisp

**Version:** 1.0.0  
**Last Updated:** 2025-10-11  
**Target Audience:** AI Agents (LLM-based development assistants)  
**Language:** Common Lisp  
**Implementation:** SBCL (Steel Bank Common Lisp) 2.3+  
**Also Covers:** Other implementations (CCL, CLISP, ECL, Allegro CL)

---

## Table of Contents

1. [Introduction](#introduction)
2. [Lisp-Specific Setup](#lisp-specific-setup)
3. [Project Structure](#project-structure)
4. [Phase 1: Planning](#phase-1-planning)
5. [Phase 2: Workspace Configuration](#phase-2-workspace-configuration)
6. [Phase 3: Implementation](#phase-3-implementation)
7. [Phase 4: Testing](#phase-4-testing)
8. [Phase 5: Documentation](#phase-5-documentation)
9. [Phase 6: Package Building & Distribution](#phase-6-package-building--distribution)
10. [Phase 7: Review & Quality Assurance](#phase-7-review--quality-assurance)
11. [Common Lisp Best Practices](#common-lisp-best-practices)
12. [Macros and Metaprogramming](#macros-and-metaprogramming)
13. [CLOS (Common Lisp Object System)](#clos-common-lisp-object-system)
14. [Quick Reference](#quick-reference)

---

## Introduction

This manual adapts the standard AI Integration Manual for Common Lisp development. Common Lisp is a powerful, dynamically-typed language with exceptional metaprogramming capabilities through its macro system. Lisp projects in the HiveLLM ecosystem should follow this guide for consistency and quality.

### Core Common Lisp Principles

1. **Interactive Development**: Use REPL for rapid prototyping
2. **Macros**: Leverage macros for DSLs and code generation
3. **Condition System**: Use restart-based error handling
4. **CLOS**: Object-oriented programming with multiple dispatch
5. **Generic Functions**: Polymorphism through generic functions
6. **Symbols and Packages**: Proper namespace management
7. **ASDF**: Standard build system for Lisp projects

### Common Lisp Ecosystem Tools

| Tool | Purpose |
|------|---------|
| **SBCL** | High-performance Common Lisp implementation |
| **Quicklisp** | Package manager |
| **ASDF** | Build system (Another System Definition Facility) |
| **FiveAM** | Testing framework |
| **Prove** | Alternative testing framework |
| **SLY/SLIME** | Emacs integration for development |
| **Roswell** | Implementation manager and scripting |
| **Serapeum** | Utility library collection |

---

## Lisp-Specific Setup

### Step 1: Install SBCL

```bash
# macOS (Homebrew)
brew install sbcl

# Ubuntu/Debian
sudo apt-get update
sudo apt-get install sbcl

# Arch Linux
sudo pacman -S sbcl

# Windows (via Scoop)
scoop install sbcl

# Or download from http://www.sbcl.org/platform-table.html

# Verify installation
sbcl --version
```

### Step 2: Install Quicklisp

```bash
# Download Quicklisp installer
curl -O https://beta.quicklisp.org/quicklisp.lisp

# Install Quicklisp
sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --quit

# Add to SBCL init file
sbcl --eval '(ql:add-to-init-file)' --quit

# Verify installation
sbcl --eval '(ql:quickload :alexandria)' --quit
```

### Step 3: Install Roswell (Optional)

```bash
# macOS
brew install roswell

# Linux
curl -L https://raw.githubusercontent.com/roswell/roswell/master/scripts/install-for-ci.sh | sh

# Initialize Roswell
ros setup

# Install dependencies
ros install qlot  # Project-local Quicklisp
```

### Step 4: Setup Development Environment

#### Emacs with SLIME/SLY

```bash
# Install Emacs
# macOS
brew install --cask emacs

# Ubuntu
sudo apt-get install emacs

# Add to ~/.emacs or ~/.emacs.d/init.el
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

# Or use SLY (modern alternative)
# M-x package-install RET sly RET
```

#### VS Code (Alternative)

```bash
# Install Alive extension
code --install-extension rheller.alive

# Configure in settings.json
{
  "alive.lsp.startCommand": ["sbcl", "--load", "quicklisp/setup.lisp"]
}
```

---

## Project Structure

### Standard Common Lisp Project

```
project-name/
├── .github/
│   └── workflows/
│       ├── ci.yml
│       └── test.yml
├── .gitignore
├── README.md
├── LICENSE
├── CHANGELOG.md
├── project-name.asd          # ASDF system definition
├── package.lisp              # Package definitions
├── src/
│   ├── main.lisp            # Main entry point
│   ├── utils.lisp           # Utility functions
│   ├── models/
│   │   └── user.lisp
│   ├── services/
│   │   └── api.lisp
│   └── conditions.lisp      # Error conditions
├── tests/
│   ├── test-suite.lisp      # Test suite definition
│   ├── test-utils.lisp
│   └── test-models.lisp
├── docs/                     # HiveLLM documentation
│   ├── ROADMAP.md
│   ├── SPECS.md
│   └── specs/
├── examples/
│   └── example.lisp
└── scripts/
    ├── build.sh
    └── run-tests.sh
```

### Web Application Structure

```
web-app/
├── web-app.asd
├── package.lisp
├── src/
│   ├── app.lisp             # Application setup
│   ├── config.lisp          # Configuration
│   ├── routes.lisp          # Route definitions
│   ├── controllers/
│   ├── models/
│   ├── views/
│   │   └── templates/
│   └── middleware/
├── static/
│   ├── css/
│   ├── js/
│   └── images/
├── tests/
└── config/
    ├── development.lisp
    └── production.lisp
```

---

## Phase 1: Planning

### Step 1.1: Create ASDF System Definition

Create `project-name.asd`:

```lisp
;;;; project-name.asd

(asdf:defsystem #:project-name
  :description "Brief description of the project"
  :author "Your Name <your.email@example.com>"
  :license  "MIT"
  :version "0.1.0"
  :homepage "https://github.com/username/project-name"
  :bug-tracker "https://github.com/username/project-name/issues"
  :source-control (:git "https://github.com/username/project-name.git")
  :serial t
  :depends-on (#:alexandria        ; Utility library
               #:serapeum           ; More utilities
               #:str                ; String manipulation
               #:local-time         ; Date/time handling
               #:dexador            ; HTTP client
               #:jonathan           ; JSON parser
               #:log4cl)            ; Logging
  :components ((:file "package")
               (:module "src"
                :components
                ((:file "conditions")
                 (:file "utils")
                 (:module "models"
                  :components
                  ((:file "user")))
                 (:module "services"
                  :components
                  ((:file "api")))
                 (:file "main"))))
  :in-order-to ((test-op (test-op #:project-name/tests))))

(asdf:defsystem #:project-name/tests
  :description "Test suite for project-name"
  :author "Your Name <your.email@example.com>"
  :license "MIT"
  :depends-on (#:project-name
               #:fiveam
               #:mockingbird)
  :components ((:module "tests"
                :components
                ((:file "test-suite")
                 (:file "test-utils")
                 (:file "test-models"))))
  :perform (test-op (o c) (symbol-call :fiveam '#:run! :project-name-suite)))
```

### Step 1.2: Define Packages

Create `package.lisp`:

```lisp
;;;; package.lisp

(defpackage #:project-name
  (:use #:cl #:alexandria #:serapeum)
  (:export
   ;; Main API
   #:start
   #:stop
   
   ;; User model
   #:user
   #:user-id
   #:user-name
   #:user-email
   #:make-user
   
   ;; Services
   #:fetch-user
   #:create-user
   
   ;; Conditions
   #:api-error
   #:not-found-error
   #:validation-error))

(defpackage #:project-name.tests
  (:use #:cl #:fiveam #:project-name)
  (:export #:run-tests))
```

### Step 1.3: Create ROADMAP.md

Create `docs/ROADMAP.md`:

```markdown
# Project Roadmap

**Project:** project-name  
**Version:** 0.1.0  
**Language:** Common Lisp (SBCL)  
**Last Updated:** 2025-10-11

## Phase 1: Foundation (v0.1.0)

### 1.1 Project Setup
- [x] Initialize ASDF system
- [x] Configure package definitions
- [x] Setup Quicklisp dependencies
- [x] Configure CI/CD workflows
- [ ] Setup documentation
- [ ] Create examples

### 1.2 Core Data Structures
- [ ] 1.2.1 Define CLOS classes
- [ ] 1.2.2 Implement constructors
- [ ] 1.2.3 Add validation
- [ ] 1.2.4 Write unit tests

### 1.3 Core Functions
- [ ] 1.3.1 Implement generic functions
- [ ] 1.3.2 Add specialized methods
- [ ] 1.3.3 Error handling with conditions
- [ ] 1.3.4 Write comprehensive tests

## Phase 2: Features (v0.2.0)

### 2.1 API Integration
- [ ] 2.1.1 HTTP client implementation
- [ ] 2.1.2 Request/response handling
- [ ] 2.1.3 JSON serialization
- [ ] 2.1.4 Integration tests

### 2.2 Macros and DSLs
- [ ] 2.2.1 Define utility macros
- [ ] 2.2.2 Create domain-specific macros
- [ ] 2.2.3 Macro expansion tests
- [ ] 2.2.4 Documentation

## Phase 3: Polish (v0.3.0)

### 3.1 Documentation
- [ ] 3.1.1 Complete docstrings
- [ ] 3.1.2 Write user guide
- [ ] 3.1.3 Create tutorials
- [ ] 3.1.4 API reference

### 3.2 Performance
- [ ] 3.2.1 Profile with sb-sprof
- [ ] 3.2.2 Optimize bottlenecks
- [ ] 3.2.3 Type declarations
- [ ] 3.2.4 Benchmarks

## Phase 4: Release (v1.0.0)

### 4.1 Final Review
- [ ] 4.1.1 Code review by 2+ Lispers
- [ ] 4.1.2 Address feedback
- [ ] 4.1.3 Multi-implementation testing
- [ ] 4.1.4 Quicklisp submission

### 4.2 Publication
- [ ] 4.2.1 Update version to 1.0.0
- [ ] 4.2.2 Update CHANGELOG
- [ ] 4.2.3 Create release tag
- [ ] 4.2.4 Submit to Quicklisp

## Status Legend
- [ ] Todo
- [~] In Progress  
- [x] Completed
- [!] Blocked

## Dependencies

| Library | Purpose | Status |
|---------|---------|--------|
| alexandria | Utilities | Required |
| serapeum | More utilities | Required |
| dexador | HTTP client | Optional |
| jonathan | JSON | Optional |

## Last Updated: 2025-10-11
```

---

## Phase 2: Workspace Configuration

### Step 2.1: Configure .gitignore

Create `.gitignore`:

```
# FASL files (compiled Lisp)
*.fasl
*.cfsl
*.lx32fsl
*.lx64fsl
*.ufsl
*.dfsl

# ASDF
.fasls/

# Quicklisp
quicklisp/
.qlot/
*.qlot

# Editor files
*.swp
*~
.#*
\#*#
*.elc

# OS files
.DS_Store
Thumbs.db

# Build artifacts
build/
dist/

# Coverage
coverage/
*.coverage

# IDE
.vscode/
.idea/

# Logs
*.log
```

### Step 2.2: Setup CI/CD

Create `.github/workflows/ci.yml`:

```yaml
name: CI

on:
  push:
    branches: [main, develop]
  pull_request:
    branches: [main, develop]

jobs:
  test:
    name: Test on ${{ matrix.lisp }} ${{ matrix.os }}
    runs-on: ${{ matrix.os }}
    strategy:
      matrix:
        lisp: [sbcl-bin, ccl-bin]
        os: [ubuntu-latest, macos-latest]
    
    steps:
      - uses: actions/checkout@v4
      
      - name: Setup Lisp
        uses: 40ants/setup-lisp@v2
        with:
          implementation: ${{ matrix.lisp }}
      
      - name: Install Quicklisp
        run: |
          curl -O https://beta.quicklisp.org/quicklisp.lisp
          sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --quit
      
      - name: Install dependencies
        run: |
          sbcl --eval '(ql:quickload :project-name)' --quit
      
      - name: Run tests
        run: |
          sbcl --eval '(ql:quickload :project-name/tests)' \
               --eval '(asdf:test-system :project-name)' \
               --quit

  lint:
    name: Lint
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - name: Setup SBCL
        uses: 40ants/setup-lisp@v2
        with:
          implementation: sbcl-bin
      
      - name: Check formatting
        run: |
          # Add lisp linter if available
          echo "Linting Lisp code..."
```

---

## Phase 3: Implementation

### Step 3.1: Define Conditions (Error Handling)

```lisp
;;;; src/conditions.lisp

(in-package #:project-name)

;;; Base condition
(define-condition api-error (error)
  ((message :initarg :message
            :reader error-message
            :documentation "The error message."))
  (:documentation "Base class for all API errors.")
  (:report (lambda (condition stream)
             (format stream "API Error: ~A" (error-message condition)))))

;;; Specific conditions
(define-condition not-found-error (api-error)
  ((resource :initarg :resource
             :reader error-resource
             :documentation "The resource that was not found."))
  (:report (lambda (condition stream)
             (format stream "Not found: ~A - ~A"
                     (error-resource condition)
                     (error-message condition)))))

(define-condition validation-error (api-error)
  ((field :initarg :field
          :reader error-field
          :documentation "The field that failed validation."))
  (:report (lambda (condition stream)
             (format stream "Validation failed for ~A: ~A"
                     (error-field condition)
                     (error-message condition)))))

;;; Restart definitions
(defun invoke-retry-restart ()
  "Invoke the RETRY restart if available."
  (let ((restart (find-restart 'retry)))
    (when restart
      (invoke-restart restart))))

(defun invoke-use-value-restart (value)
  "Invoke the USE-VALUE restart with VALUE."
  (let ((restart (find-restart 'use-value)))
    (when restart
      (invoke-restart restart value))))
```

### Step 3.2: Implement CLOS Classes

```lisp
;;;; src/models/user.lisp

(in-package #:project-name)

;;; User class
(defclass user ()
  ((id :initarg :id
       :accessor user-id
       :type string
       :documentation "The user's unique identifier.")
   (name :initarg :name
         :accessor user-name
         :type string
         :documentation "The user's display name.")
   (email :initarg :email
          :accessor user-email
          :type (or string null)
          :initform nil
          :documentation "The user's email address.")
   (created-at :initarg :created-at
               :accessor user-created-at
               :type local-time:timestamp
               :initform (local-time:now)
               :documentation "When the user was created."))
  (:documentation "Represents a user in the system."))

;;; Constructor
(defun make-user (&key id name email)
  "Create a new USER instance."
  (check-type id string)
  (check-type name string)
  (check-type email (or string null))
  (make-instance 'user
                 :id id
                 :name name
                 :email email))

;;; Print object
(defmethod print-object ((user user) stream)
  (print-unreadable-object (user stream :type t :identity t)
    (format stream "~A (~A)" (user-name user) (user-id user))))

;;; Validation
(defgeneric validate-user (user)
  (:documentation "Validate a USER object."))

(defmethod validate-user ((user user))
  "Validate USER fields."
  (unless (and (user-id user) (plusp (length (user-id user))))
    (error 'validation-error
           :field 'id
           :message "User ID cannot be empty"))
  (unless (and (user-name user) (plusp (length (user-name user))))
    (error 'validation-error
           :field 'name
           :message "User name cannot be empty"))
  (when (user-email user)
    (unless (str:containsp "@" (user-email user))
      (error 'validation-error
             :field 'email
             :message "Invalid email format")))
  t)

;;; JSON serialization
(defun user-to-json (user)
  "Convert USER to JSON-compatible plist."
  (list :id (user-id user)
        :name (user-name user)
        :email (user-email user)
        :created-at (local-time:format-timestring
                     nil
                     (user-created-at user)
                     :format local-time:+iso-8601-format+)))

(defun json-to-user (json)
  "Create USER from JSON-compatible plist."
  (make-user :id (getf json :id)
             :name (getf json :name)
             :email (getf json :email)))

;;; Equality
(defmethod equal-user ((user1 user) (user2 user))
  "Check if two users are equal (same ID)."
  (string= (user-id user1) (user-id user2)))
```

### Step 3.3: Implement Services

```lisp
;;;; src/services/api.lisp

(in-package #:project-name)

(defparameter *base-url* "https://api.example.com"
  "The base URL for API requests.")

;;; Generic function for fetching
(defgeneric fetch-resource (resource-type id)
  (:documentation "Fetch a resource by type and ID."))

(defmethod fetch-resource ((resource-type (eql :user)) id)
  "Fetch a user by ID."
  (check-type id string)
  (let ((url (format nil "~A/users/~A" *base-url* id)))
    (handler-case
        (let* ((response (dex:get url))
               (json (jonathan:parse response :as :plist)))
          (json-to-user json))
      (dex:http-request-not-found ()
        (error 'not-found-error
               :resource (format nil "User ~A" id)
               :message "User not found"))
      (dex:http-request-failed (e)
        (error 'api-error
               :message (format nil "HTTP request failed: ~A" e))))))

;;; Wrapper function
(defun fetch-user (id &key (retry t))
  "Fetch a user by ID with optional retry on error."
  (restart-case
      (fetch-resource :user id)
    (retry ()
      :report "Retry fetching the user"
      (fetch-user id :retry nil))
    (use-value (value)
      :report "Use a different value"
      :interactive (lambda ()
                     (format t "Enter user ID: ")
                     (list (read-line)))
      (fetch-user value :retry retry))
    (return-nil ()
      :report "Return NIL"
      nil)))

;;; Creating users
(defun create-user (name email)
  "Create a new user."
  (check-type name string)
  (check-type email (or string null))
  (let* ((url (format nil "~A/users" *base-url*))
         (body (jonathan:to-json (list :name name :email email)))
         (response (dex:post url
                             :headers '(("Content-Type" . "application/json"))
                             :content body))
         (json (jonathan:parse response :as :plist)))
    (json-to-user json)))

;;; Async operations with bordeaux-threads
(defun fetch-user-async (id callback)
  "Fetch a user asynchronously and call CALLBACK with the result."
  (bt:make-thread
   (lambda ()
     (handler-case
         (let ((user (fetch-user id)))
           (funcall callback user nil))
       (error (e)
         (funcall callback nil e))))
   :name (format nil "Fetch user ~A" id)))
```

### Step 3.4: Macros

```lisp
;;;; src/utils.lisp

(in-package #:project-name)

;;; Utility macros
(defmacro with-retries ((max-retries &key (delay 1)) &body body)
  "Execute BODY with up to MAX-RETRIES attempts on error."
  (let ((attempt (gensym "ATTEMPT"))
        (result (gensym "RESULT"))
        (done (gensym "DONE")))
    `(let ((,attempt 0)
           (,result nil)
           (,done nil))
       (loop until (or ,done (>= ,attempt ,max-retries))
             do (handler-case
                    (progn
                      (setf ,result (progn ,@body))
                      (setf ,done t))
                  (error (e)
                    (incf ,attempt)
                    (unless (>= ,attempt ,max-retries)
                      (format t "Attempt ~A failed: ~A. Retrying...~%" ,attempt e)
                      (sleep ,delay)))))
       ,result)))

(defmacro define-cached-function (name lambda-list &body body)
  "Define a function with memoization."
  (let ((cache (gensym "CACHE")))
    `(let ((,cache (make-hash-table :test 'equal)))
       (defun ,name ,lambda-list
         (let ((key (list ,@(remove '&key (remove '&optional lambda-list)))))
           (or (gethash key ,cache)
               (setf (gethash key ,cache)
                     (progn ,@body))))))))

(defmacro defun-inline (name lambda-list &body body)
  "Define an inline function."
  `(progn
     (declaim (inline ,name))
     (defun ,name ,lambda-list
       ,@body)))

;;; DSL example
(defmacro define-routes (&body routes)
  "Define HTTP routes."
  `(progn
     ,@(loop for (method path handler) in routes
             collect `(register-route ',method ,path #',handler))))
```

---

## Phase 4: Testing

### Step 4.1: Test Suite Setup

```lisp
;;;; tests/test-suite.lisp

(in-package #:project-name.tests)

(def-suite project-name-suite
  :description "Main test suite for project-name")

(in-suite project-name-suite)

;;; Test fixtures
(defixture user-fixture ()
  (let ((user (make-user :id "1"
                         :name "John Doe"
                         :email "john@example.com")))
    (&body)))

;;; Helper functions
(defun run-tests ()
  "Run all tests."
  (run! 'project-name-suite))
```

### Step 4.2: Unit Tests

```lisp
;;;; tests/test-models.lisp

(in-package #:project-name.tests)

(in-suite project-name-suite)

(test user-creation
  "Test user creation."
  (let ((user (make-user :id "1" :name "John" :email "john@example.com")))
    (is (string= "1" (user-id user)))
    (is (string= "John" (user-name user)))
    (is (string= "john@example.com" (user-email user)))))

(test user-validation
  "Test user validation."
  (with-fixture user-fixture ()
    (is (validate-user user)))
  
  ;; Invalid user
  (signals validation-error
    (validate-user (make-user :id "" :name "John" :email nil))))

(test user-json-conversion
  "Test JSON serialization and deserialization."
  (with-fixture user-fixture ()
    (let* ((json (user-to-json user))
           (restored (json-to-user json)))
      (is (string= (user-id user) (user-id restored)))
      (is (string= (user-name user) (user-name restored)))
      (is (string= (user-email user) (user-email restored))))))

(test user-equality
  "Test user equality."
  (let ((user1 (make-user :id "1" :name "John" :email nil))
        (user2 (make-user :id "1" :name "Jane" :email nil))
        (user3 (make-user :id "2" :name "John" :email nil)))
    (is (equal-user user1 user2))
    (is (not (equal-user user1 user3)))))
```

### Step 4.3: Mock Testing

```lisp
;;;; tests/test-api.lisp

(in-package #:project-name.tests)

(in-suite project-name-suite)

(test fetch-user-success
  "Test successful user fetch."
  ;; Mock HTTP response
  (mockingbird:with-dynamic-stubs
      ((dex:get (lambda (url)
                  (jonathan:to-json
                   (list :id "1"
                         :name "John Doe"
                         :email "john@example.com")))))
    (let ((user (fetch-user "1")))
      (is (string= "1" (user-id user)))
      (is (string= "John Doe" (user-name user))))))

(test fetch-user-not-found
  "Test user not found error."
  (mockingbird:with-dynamic-stubs
      ((dex:get (lambda (url)
                  (error 'dex:http-request-not-found))))
    (signals not-found-error
      (fetch-user "999"))))
```

### Step 4.4: Running Tests

```bash
# Run tests from REPL
sbcl --eval '(ql:quickload :project-name/tests)' \
     --eval '(in-package :project-name.tests)' \
     --eval '(run-tests)' \
     --quit

# Run with ASDF
sbcl --eval '(asdf:test-system :project-name)' --quit

# With coverage (requires sb-cover)
sbcl --eval '(require :sb-cover)' \
     --eval '(declaim (optimize sb-cover:store-coverage-data))' \
     --eval '(ql:quickload :project-name/tests)' \
     --eval '(asdf:test-system :project-name)' \
     --eval '(sb-cover:report "coverage/")' \
     --quit
```

---

## Phase 5: Documentation

### Step 5.1: Docstrings

```lisp
;;; Good documentation example
(defun fetch-user (id &key (retry t))
  "Fetch a user by ID from the API.

Arguments:
  ID - A string representing the user's unique identifier.

Keyword Arguments:
  RETRY - Boolean. If T, retry on error. Default is T.

Returns:
  A USER instance if successful, NIL if using return-nil restart.

Signals:
  NOT-FOUND-ERROR - If the user doesn't exist.
  API-ERROR - If the HTTP request fails.

Restarts:
  RETRY - Retry the operation.
  USE-VALUE - Use a different user ID.
  RETURN-NIL - Return NIL instead of signaling an error.

Examples:
  (fetch-user \"123\")
  => #<USER John Doe (123)>

  (handler-bind ((not-found-error
                  (lambda (c)
                    (invoke-restart 'return-nil))))
    (fetch-user \"999\"))
  => NIL"
  (restart-case
      (fetch-resource :user id)
    (retry ()
      :report "Retry fetching the user"
      (fetch-user id :retry nil))
    (use-value (value)
      :report "Use a different value"
      :interactive (lambda ()
                     (format t "Enter user ID: ")
                     (list (read-line)))
      (fetch-user value :retry retry))
    (return-nil ()
      :report "Return NIL"
      nil)))
```

### Step 5.2: README Example

```markdown
# project-name

A Common Lisp project for [description].

## Features

- ✨ Feature 1
- ✨ Feature 2
- ✨ Feature 3

## Installation

### Via Quicklisp

```lisp
(ql:quickload :project-name)
```

### Manual

```bash
git clone https://github.com/username/project-name.git
cd project-name
sbcl --eval '(push #P"./" asdf:*central-registry*)' \
     --eval '(ql:quickload :project-name)' \
     --quit
```

## Usage

```lisp
(in-package :project-name)

;; Fetch a user
(let ((user (fetch-user "123")))
  (format t "User: ~A~%" (user-name user)))

;; Handle errors with restarts
(handler-bind ((not-found-error
                (lambda (c)
                  (format t "Error: ~A~%" c)
                  (invoke-restart 'return-nil))))
  (fetch-user "999"))
```

## API Reference

See [API documentation](docs/api.md).

## Testing

```bash
sbcl --eval '(asdf:test-system :project-name)' --quit
```

## License

MIT License
```

---

## Phase 6: Package Building & Distribution

### Step 6.1: Version Management

Update version in `.asd` file:

```lisp
(asdf:defsystem #:project-name
  :version "1.0.0"
  ;; ...
  )
```

### Step 6.2: Create Release

```bash
# Tag release
git tag -a v1.0.0 -m "Release version 1.0.0"
git push origin v1.0.0

# Build binary (optional)
sbcl --eval '(ql:quickload :project-name)' \
     --eval '(sb-ext:save-lisp-and-die "project-name" 
                                        :toplevel #'"'"'project-name:main
                                        :executable t)' \
     --quit
```

### Step 6.3: Submit to Quicklisp

```bash
# Ensure project is on GitHub/GitLab
# Create release
# Submit project to Quicklisp via:
# https://github.com/quicklisp/quicklisp-projects/issues

# Follow Quicklisp submission guidelines:
# 1. Public repository
# 2. ASDF system definition
# 3. License file
# 4. Tests
```

---

## Phase 7: Review & Quality Assurance

### Pre-Release Checklist

```markdown
# Pre-Release Checklist

**Project:** project-name  
**Version:** 1.0.0  
**Date:** 2025-10-11

## Code Quality

- [ ] No compiler warnings
- [ ] No undefined functions
- [ ] No unused variables
- [ ] Proper package exports
- [ ] Type declarations for performance-critical code

## Testing

- [ ] All tests pass
- [ ] Test coverage > 80%
- [ ] Tests on multiple implementations (SBCL, CCL)
- [ ] Edge cases covered
- [ ] Error conditions tested

## Documentation

- [ ] All exported symbols documented
- [ ] Docstrings follow conventions
- [ ] README is comprehensive
- [ ] Examples work
- [ ] CHANGELOG updated
- [ ] API reference complete

## ASDF

- [ ] System definition is correct
- [ ] Dependencies are minimal
- [ ] Version number updated
- [ ] Loads without errors
- [ ] (asdf:test-system) works

## Compatibility

- [ ] Works on SBCL
- [ ] Works on CCL (if targeting)
- [ ] Works on other implementations (if targeting)
- [ ] No implementation-specific code (or properly conditionalized)

## Performance

- [ ] No obvious performance issues
- [ ] Type declarations where needed
- [ ] Profiled with sb-sprof
- [ ] No memory leaks
```

---

## Common Lisp Best Practices

### Naming Conventions

```lisp
;; Functions and variables: lowercase with hyphens
(defun fetch-user (user-id) ...)
(defvar *global-config* ...)  ; Global vars with earmuffs

;; Constants: +constant-name+
(defconstant +max-retries+ 3)

;; Classes: lowercase with hyphens
(defclass user-profile () ...)

;; Predicates: end with -p or -p
(defun active-p (user) ...)
(defun user-admin-p (user) ...)
```

### Package Usage

```lisp
;; Good: Explicit package reference
(defun process-data ()
  (let ((cleaned (str:trim data)))
    (alexandria:when-let ((result (parse cleaned)))
      result)))

;; Good: Import specific symbols
(defpackage #:my-package
  (:use #:cl)
  (:import-from #:alexandria
                #:when-let
                #:if-let))

;; Avoid: Promiscuous USE
;; (use-package :everything)  ; Don't do this
```

---

## Macros and Metaprogramming

### Writing Macros

```lisp
;;; Good: Hygienic macro
(defmacro with-timing (&body body)
  "Execute BODY and print execution time."
  (let ((start (gensym "START"))
        (result (gensym "RESULT"))
        (duration (gensym "DURATION")))
    `(let* ((,start (get-internal-real-time))
            (,result (progn ,@body))
            (,duration (/ (- (get-internal-real-time) ,start)
                         internal-time-units-per-second)))
       (format t "Execution time: ~,3F seconds~%" ,duration)
       ,result)))

;;; Good: Once-only for arguments
(defmacro square (x)
  (alexandria:once-only (x)
    `(* ,x ,x)))

;; Usage: (square (expensive-computation))
;; Expands correctly without double evaluation
```

---

## CLOS (Common Lisp Object System)

### Multiple Dispatch

```lisp
;;; Generic function with multiple methods
(defgeneric process (input output-format)
  (:documentation "Process INPUT to OUTPUT-FORMAT."))

(defmethod process ((input string) (format (eql :json)))
  (jonathan:to-json (list :text input)))

(defmethod process ((input user) (format (eql :json)))
  (user-to-json input))

(defmethod process ((input list) (format (eql :csv)))
  (str:join "," (mapcar #'prin1-to-string input)))

;;; :before, :after, :around methods
(defmethod process :before (input format)
  (log:info "Processing ~A to ~A" input format))

(defmethod process :after (input format)
  (log:info "Finished processing"))

(defmethod process :around ((input string) format)
  (handler-case (call-next-method)
    (error (e)
      (log:error "Error processing: ~A" e)
      nil)))
```

---

## Quick Reference

### Essential Commands

```bash
# REPL
sbcl
rlwrap sbcl  # With readline support

# Load system
(ql:quickload :project-name)

# Test system
(asdf:test-system :project-name)

# Compile and load
(asdf:load-system :project-name)

# Update Quicklisp
(ql:update-all-dists)

# Build executable
(sb-ext:save-lisp-and-die "executable"
  :toplevel #'main
  :executable t)
```

### REPL Commands

```lisp
;; Help
(describe 'function-name)
(documentation 'function-name 'function)

;; Inspection
(inspect object)
(trace function-name)
(untrace function-name)

;; Profiling
(require :sb-sprof)
(sb-sprof:with-profiling (:report :flat)
  (expensive-function))

;; Debugging
(break)  ; Set breakpoint
:continue  ; Continue execution
:step  ; Step through code
```

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-10-11 | Initial Lisp manual creation |

---

## Resources

- [Common Lisp HyperSpec](http://www.lispworks.com/documentation/HyperSpec/Front/index.htm)
- [Practical Common Lisp](http://www.gigamonkeys.com/book/)
- [SBCL Manual](http://www.sbcl.org/manual/index.html)
- [Quicklisp](https://www.quicklisp.org/)
- [Awesome Common Lisp](https://github.com/CodyReichert/awesome-cl)

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

