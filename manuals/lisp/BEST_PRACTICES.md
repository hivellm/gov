# Common Lisp Best Practices Guide

**Version:** 1.0.0  
**Last Updated:** 2025-10-11  
**Target Audience:** AI Agents developing Common Lisp applications  
**Compliance:** Common Lisp HyperSpec, Community Standards

---

## Table of Contents

1. [Code Style](#code-style)
2. [Naming Conventions](#naming-conventions)
3. [Package Management](#package-management)
4. [Functions](#functions)
5. [Macros](#macros)
6. [CLOS (Object System)](#clos-object-system)
7. [Condition System](#condition-system)
8. [Performance](#performance)
9. [Concurrency](#concurrency)
10. [Testing](#testing)
11. [Documentation](#documentation)
12. [Anti-Patterns](#anti-patterns)

---

## Code Style

### Indentation

```lisp
;; Good: Proper indentation
(defun calculate-total (items)
  (reduce #'+
          (mapcar (lambda (item)
                    (* (item-price item)
                       (item-quantity item)))
                  items)))

;; Good: Align related expressions
(let ((x 10)
      (y 20)
      (z 30))
  (+ x y z))

;; Good: Break long forms
(defun process-user-data (name
                           email
                           phone
                           address
                           &key
                           (active t)
                           (verified nil))
  ;; Implementation
  )
```

### Parentheses

```lisp
;; Good: Closing parens on same line
(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (1- n)))))

;; Bad: Closing parens on separate lines
(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (1- n)))
  )
)
```

### Line Length

```lisp
;; Prefer lines under 80-100 characters

;; Good: Break long strings
(format nil "This is a very long message that needs to be ~
             split across multiple lines for readability.")

;; Good: Break long forms
(some-long-function-name
 argument1
 argument2
 argument3
 :keyword1 value1
 :keyword2 value2)
```

---

## Naming Conventions

### Variables and Functions

```lisp
;; Variables and functions: lowercase with hyphens
(defvar my-variable 42)
(defun calculate-sum (a b) (+ a b))

;; Global variables: *earmuffs*
(defvar *default-timeout* 30)
(defparameter *debug-mode* nil)

;; Constants: +plus-signs+
(defconstant +pi+ 3.14159)
(defconstant +max-connections+ 100)

;; Good: Descriptive names
(defun fetch-user-by-id (id) ...)
(defvar *database-connection* ...)

;; Bad: Abbreviated or unclear
(defun get-usr (i) ...)
(defvar *db-conn* ...)
```

### Predicates

```lisp
;; Predicates: end with -p (or P for single word)
(defun empty-p (list)
  (null list))

(defun numberp (x)
  (typep x 'number))

(defun user-admin-p (user)
  (member :admin (user-roles user)))

;; Type predicates from CL: numberp, stringp, listp, etc.
```

### Classes and Slots

```lisp
;; Classes: lowercase with hyphens
(defclass user-account ()
  ((id :initarg :id
       :accessor account-id)
   (username :initarg :username
             :accessor account-username)
   (email :initarg :email
          :accessor account-email)))

;; Accessors match slot names or use class prefix
(defclass user ()
  ((name :accessor user-name)
   (email :accessor user-email)))
```

---

## Package Management

### Package Definition

```lisp
;; Good: Clear package definition
(defpackage #:my-app
  (:use #:cl #:alexandria #:serapeum)
  (:import-from #:local-time
                #:now
                #:format-timestring)
  (:export
   ;; Main API
   #:start-app
   #:stop-app
   
   ;; User functions
   #:create-user
   #:find-user
   #:delete-user
   
   ;; Classes
   #:user
   #:user-name
   #:user-email))

;; Use package at file start
(in-package #:my-app)
```

### Package Usage

```lisp
;; Good: Explicit package prefix
(defun process-data (data)
  (let ((trimmed (str:trim data)))
    (alexandria:when-let ((parsed (parse trimmed)))
      parsed)))

;; Good: Import specific symbols
(defpackage #:my-app
  (:use #:cl)
  (:import-from #:alexandria
                #:when-let
                #:if-let
                #:hash-table-alist))

;; Avoid: Too many USE packages
;; (use-package :everything)  ; Package pollution
```

### Export Policy

```lisp
;; Good: Export only public API
(defpackage #:my-library
  (:export
   ;; Public functions
   #:public-function
   #:another-public-function
   
   ;; Public classes
   #:my-class
   #:my-class-slot))

;; Internal: Don't export
;; (internal-helper-function ...)
```

---

## Functions

### Function Definition

```lisp
;; Good: Clear signature
(defun calculate-discount (price discount-rate &key (tax-rate 0.1))
  "Calculate final price after discount and tax.
  
  Arguments:
    PRICE - Original price (number)
    DISCOUNT-RATE - Discount as decimal (0.0-1.0)
  
  Keyword Arguments:
    TAX-RATE - Tax rate as decimal (default 0.1)
  
  Returns:
    Final price after discount and tax"
  (let ((discounted (* price (- 1 discount-rate))))
    (* discounted (+ 1 tax-rate))))

;; Good: Type checking
(defun process-numbers (numbers)
  (check-type numbers list)
  (assert (every #'numberp numbers))
  (reduce #'+ numbers))
```

### Lambda Functions

```lisp
;; Good: Lambda for simple operations
(mapcar (lambda (x) (* x x)) numbers)

;; Good: Named function for complex operations
(defun square (x) (* x x))
(mapcar #'square numbers)

;; Good: Currying with closures
(defun make-multiplier (factor)
  (lambda (x) (* x factor)))

(let ((double (make-multiplier 2)))
  (funcall double 5))  ; => 10
```

### Return Values

```lisp
;; Good: Explicit return
(defun find-user (id)
  (let ((user (query-database id)))
    (if user
        (values user t)      ; Return user and success flag
        (values nil nil))))  ; Return nil and failure flag

;; Good: Multiple values
(defun parse-name (full-name)
  (let ((parts (str:split " " full-name)))
    (values (first parts)   ; first-name
            (second parts)   ; last-name
            (cddr parts))))  ; middle-names

;; Usage
(multiple-value-bind (first last middle)
    (parse-name "John Robert Doe")
  (format t "~A ~A" first last))
```

---

## Macros

### Writing Macros

```lisp
;; Good: Use GENSYM to avoid variable capture
(defmacro with-timing (&body body)
  (let ((start (gensym "START"))
        (result (gensym "RESULT")))
    `(let* ((,start (get-internal-real-time))
            (,result (progn ,@body)))
       (format t "Time: ~,3F seconds~%"
               (/ (- (get-internal-real-time) ,start)
                  internal-time-units-per-second))
       ,result)))

;; Good: Use ONCE-ONLY for evaluated arguments
(defmacro square (x)
  (alexandria:once-only (x)
    `(* ,x ,x)))

;; Without once-only, (square (expensive-calc)) evaluates twice!

;; Good: Macro hygiene
(defmacro with-resource ((var resource) &body body)
  `(let ((,var ,resource))
     (unwind-protect
          (progn ,@body)
       (close-resource ,var))))
```

### When to Use Macros

```lisp
;; Good: Use macros for:
;; 1. Control flow
(defmacro when-let ((var expression) &body body)
  `(let ((,var ,expression))
     (when ,var
       ,@body)))

;; 2. Code transformation
(defmacro define-enum (name &rest values)
  `(progn
     ,@(loop for value in values
             for i from 0
             collect `(defconstant ,(intern (symbol-name value)) ,i))))

;; 3. DSLs
(defmacro defrule (name pattern &body body)
  `(setf (gethash ',name *rules*)
         (make-rule :pattern ',pattern :body ',body)))

;; Bad: Don't use macros for simple functions
;; (defmacro add (a b) `(+ ,a ,b))  ; Just use a function!
```

### Macro Expansion

```lisp
;; Always test macro expansion
(macroexpand-1 '(with-timing (expensive-operation)))

;; Use macroexpand for nested macros
(macroexpand '(when-let ((x (get-value)))
                (process x)))
```

---

## CLOS (Object System)

### Class Definition

```lisp
;; Good: Well-documented class
(defclass user ()
  ((id :initarg :id
       :reader user-id
       :type string
       :documentation "Unique user identifier")
   (name :initarg :name
         :accessor user-name
         :type string
         :documentation "User's full name")
   (email :initarg :email
          :accessor user-email
          :type (or string null)
          :initform nil
          :documentation "User's email address")
   (created-at :initarg :created-at
               :reader user-created-at
               :initform (get-universal-time)
               :documentation "Creation timestamp"))
  (:documentation "Represents a user in the system"))

;; Good: Use :reader for read-only, :accessor for read-write
;; Bad: Always using :accessor even for immutable slots
```

### Initialization

```lisp
;; Good: Initialize-instance method
(defmethod initialize-instance :after ((user user) &key)
  (unless (user-email user)
    (setf (user-email user)
          (format nil "~A@example.com" (user-id user)))))

;; Good: Shared-initialize for complex initialization
(defmethod shared-initialize :after ((user user) slot-names &key)
  (when (or (eq slot-names t)
            (member 'email slot-names))
    (validate-email (user-email user))))
```

### Generic Functions

```lisp
;; Good: Generic function with multiple methods
(defgeneric save (object database)
  (:documentation "Save OBJECT to DATABASE"))

(defmethod save ((user user) (db sql-database))
  (execute-sql db
               "INSERT INTO users VALUES (?, ?, ?)"
               (user-id user)
               (user-name user)
               (user-email user)))

(defmethod save ((user user) (db nosql-database))
  (put-document db
                (user-id user)
                (user-to-json user)))

;; Good: Specialized methods
(defmethod save :before (object database)
  (log:info "Saving ~A to ~A" object database))

(defmethod save :after (object database)
  (log:info "Saved successfully"))
```

### Multiple Inheritance

```lisp
;; Good: Mixin classes for shared behavior
(defclass timestamped-mixin ()
  ((created-at :reader created-at
               :initform (get-universal-time))
   (updated-at :accessor updated-at
               :initform (get-universal-time))))

(defclass audited-mixin ()
  ((created-by :initarg :created-by
               :reader created-by)
   (updated-by :accessor updated-by)))

(defclass user (timestamped-mixin audited-mixin)
  ((name :initarg :name
         :accessor user-name)))

;; Method combination
(defmethod update :before ((obj timestamped-mixin))
  (setf (updated-at obj) (get-universal-time)))
```

---

## Condition System

### Defining Conditions

```lisp
;; Good: Hierarchy of conditions
(define-condition app-error (error)
  ((message :initarg :message
            :reader error-message))
  (:documentation "Base application error")
  (:report (lambda (condition stream)
             (format stream "Application error: ~A"
                     (error-message condition)))))

(define-condition validation-error (app-error)
  ((field :initarg :field
          :reader error-field))
  (:report (lambda (condition stream)
             (format stream "Validation failed for ~A: ~A"
                     (error-field condition)
                     (error-message condition)))))

(define-condition not-found-error (app-error)
  ((resource :initarg :resource
             :reader error-resource))
  (:report (lambda (condition stream)
             (format stream "~A not found: ~A"
                     (error-resource condition)
                     (error-message condition)))))
```

### Restarts

```lisp
;; Good: Provide restarts
(defun fetch-data (url &key (retries 3))
  (restart-case
      (http-get url)
    (retry ()
      :report "Retry the request"
      (if (> retries 0)
          (fetch-data url :retries (1- retries))
          (error "Max retries exceeded")))
    (use-cached-value (value)
      :report "Use cached value"
      :interactive (lambda ()
                     (format t "Enter cached value: ")
                     (list (read)))
      value)
    (return-nil ()
      :report "Return NIL"
      nil)))

;; Good: Handler-bind for non-local exits
(defun process-all-users (users)
  (handler-bind ((validation-error
                  (lambda (c)
                    (log:warn "Validation error: ~A" c)
                    (invoke-restart 'skip-user))))
    (dolist (user users)
      (restart-case
          (validate-and-save user)
        (skip-user ()
          :report "Skip this user and continue"
          nil)))))
```

### Error Handling

```lisp
;; Good: Specific error handling
(handler-case
    (parse-json string)
  (json-parse-error (e)
    (log:error "JSON parse error: ~A" e)
    nil)
  (error (e)
    (log:error "Unexpected error: ~A" e)
    (signal e)))

;; Good: Handler-bind for accessing the stack
(handler-bind ((warning #'muffle-warning))
  (potentially-noisy-function))

;; Good: Unwind-protect for cleanup
(let ((file (open "data.txt")))
  (unwind-protect
       (process-file file)
    (close file)))
```

---

## Performance

### Type Declarations

```lisp
;; Good: Declare types for performance
(defun fast-sum (numbers)
  (declare (type list numbers)
           (optimize (speed 3) (safety 0)))
  (let ((sum 0))
    (declare (type fixnum sum))
    (dolist (n numbers sum)
      (declare (type fixnum n))
      (incf sum n))))

;; Good: FTYPE declarations
(declaim (ftype (function (fixnum fixnum) fixnum) add-fixnums))
(defun add-fixnums (a b)
  (declare (type fixnum a b))
  (+ a b))

;; Good: Inline for small, frequently called functions
(declaim (inline square))
(defun square (x)
  (declare (type number x))
  (* x x))
```

### Optimization

```lisp
;; Good: Profile before optimizing
(require :sb-sprof)
(sb-sprof:with-profiling (:report :flat)
  (expensive-function))

;; Good: Avoid consing in hot loops
(defun sum-array (array)
  (declare (type (simple-array fixnum) array)
           (optimize speed))
  (let ((sum 0))
    (declare (type fixnum sum))
    (loop for i from 0 below (length array)
          do (incf sum (aref array i)))
    sum))

;; Bad: Unnecessary allocation
(defun bad-sum (list)
  (reduce #'+ (mapcar #'identity list)))  ; Extra copy!

;; Good: Direct iteration
(defun good-sum (list)
  (reduce #'+ list))
```

### Memory Management

```lisp
;; Good: Reuse objects
(let ((buffer (make-array 1024 :element-type '(unsigned-byte 8))))
  (dotimes (i 100)
    (fill buffer 0)
    (process-buffer buffer)))

;; Good: Use appropriate data structures
(defvar *cache* (make-hash-table :test 'equal :size 1000))

;; Avoid: Growing lists in loops
;; Bad
(let ((result nil))
  (dotimes (i 1000)
    (push i result))  ; Lots of consing
  (nreverse result))

;; Good: Pre-allocate
(let ((result (make-array 1000)))
  (dotimes (i 1000)
    (setf (aref result i) i))
  result)
```

---

## Concurrency

### Bordeaux Threads

```lisp
;; Good: Thread creation
(defun run-in-background (function)
  (bt:make-thread function
                  :name "Background worker"))

;; Good: Locks
(defvar *counter-lock* (bt:make-lock "Counter lock"))
(defvar *counter* 0)

(defun increment-counter ()
  (bt:with-lock-held (*counter-lock*)
    (incf *counter*)))

;; Good: Condition variables
(defvar *queue-lock* (bt:make-lock))
(defvar *queue-cv* (bt:make-condition-variable))
(defvar *queue* nil)

(defun enqueue (item)
  (bt:with-lock-held (*queue-lock*)
    (push item *queue*)
    (bt:condition-notify *queue-cv*)))

(defun dequeue ()
  (bt:with-lock-held (*queue-lock*)
    (loop while (null *queue*)
          do (bt:condition-wait *queue-cv* *queue-lock*))
    (pop *queue*)))
```

### Lparallel

```lisp
;; Good: Parallel map
(require :lparallel)
(setf lparallel:*kernel* (lparallel:make-kernel 4))

(defun parallel-process (items)
  (lparallel:pmapcar #'expensive-function items))

;; Good: Futures
(defun fetch-data-async ()
  (lparallel:future
    (fetch-from-api)))

(defun process-data ()
  (let ((future (fetch-data-async)))
    ;; Do other work
    (process-other-things)
    ;; Wait for result
    (let ((data (lparallel:force future)))
      (process data))))
```

---

## Testing

### Test Organization

```lisp
;; Good: Organized test suite
(in-package :my-app.tests)

(def-suite my-app-suite
  :description "Main test suite")

(in-suite my-app-suite)

(def-suite model-tests :in my-app-suite)
(def-suite service-tests :in my-app-suite)

;; Test with fixtures
(def-fixture user-fixture ()
  (let ((user (make-user :id "1" :name "Test")))
    (&body)))

(test user-creation
  "Test user creation and validation"
  (with-fixture user-fixture ()
    (is (string= "1" (user-id user)))
    (is (string= "Test" (user-name user)))))
```

### Assertions

```lisp
;; Good: Descriptive test names and assertions
(test arithmetic
  "Test basic arithmetic operations"
  (is (= 4 (+ 2 2)))
  (is (= 0 (- 5 5)))
  (is (= 6 (* 2 3)))
  (is (= 2 (/ 10 5))))

(test error-conditions
  "Test that errors are signaled correctly"
  (signals division-by-zero
    (/ 1 0))
  (signals validation-error
    (create-user :name "" :email nil)))

(test optional-behavior
  "Test optional or implementation-specific behavior"
  (skip "Not implemented yet")
  ;; Or
  (is-true (some-condition)))
```

---

## Documentation

### Docstrings

```lisp
;; Good: Comprehensive docstrings
(defun fetch-user (id &key (timeout 30) (retries 3))
  "Fetch a user by ID from the API.

Arguments:
  ID - String. The user's unique identifier.

Keyword Arguments:
  TIMEOUT - Integer. Request timeout in seconds (default: 30)
  RETRIES - Integer. Number of retry attempts (default: 3)

Returns:
  A USER instance if successful, NIL otherwise.

Signals:
  NOT-FOUND-ERROR - If the user doesn't exist
  API-ERROR - If the request fails after retries

Examples:
  (fetch-user \"123\")
  => #<USER John Doe>
  
  (fetch-user \"123\" :timeout 60 :retries 5)
  => #<USER John Doe>"
  (declare (type string id)
           (type (integer 1) timeout retries))
  ;; Implementation
  )

;; Good: Class documentation
(defclass user ()
  ((id :initarg :id
       :reader user-id
       :type string
       :documentation "The user's unique identifier")
   (name :initarg :name
         :accessor user-name
         :type string
         :documentation "The user's display name"))
  (:documentation "Represents a user in the system.
  
  Slots:
    ID - Unique identifier (read-only)
    NAME - Display name (read-write)
    
  Example:
    (make-instance 'user :id \"1\" :name \"John\")"))
```

---

## Anti-Patterns

### Avoid

```lisp
;; 1. Using SETQ instead of SETF
(setq x 10)  ; Bad - old style
(setf x 10)  ; Good

;; 2. Using PROGN unnecessarily
(progn (do-something))  ; Bad - single form
(do-something)  ; Good

;; 3. Not using type declarations in performance-critical code
(defun slow-sum (numbers)
  (reduce #'+ numbers))  ; No type info

;; 4. Catching too broadly
(handler-case (risky-operation)
  (error () nil))  ; Bad - catches everything

;; 5. Not providing restarts
(defun read-config ()
  (unless (probe-file "config.lisp")
    (error "Config not found")))  ; Bad - no way to recover

;; 6. Macro where function would work
(defmacro add (a b)
  `(+ ,a ,b))  ; Bad - just use a function!

;; 7. Modifying quoted lists
(defun bad-append (x)
  (nconc '(1 2 3) (list x)))  ; Bad - modifies literal!

;; 8. Using global variables excessively
(defvar *x* 1)
(defvar *y* 2)
(defvar *z* 3)  ; Bad - too many globals

;; 9. Not using with-* macros
(let ((stream (open "file.txt")))
  (read stream)
  (close stream))  ; Bad - no error handling

(with-open-file (stream "file.txt")
  (read stream))  ; Good

;; 10. Returning from catch/throw instead of using values
(catch 'tag
  (throw 'tag 42))  ; Bad - non-local exit
```

---

## Checklist

### Before Committing

- [ ] No compiler warnings
- [ ] No style-warnings
- [ ] All tests pass
- [ ] Code formatted properly
- [ ] Docstrings complete
- [ ] Package exports updated

### Before Releasing

- [ ] Version number updated
- [ ] CHANGELOG updated
- [ ] README accurate
- [ ] Tests pass on target implementations
- [ ] Documentation generated
- [ ] Examples work

---

## Resources

- [Common Lisp HyperSpec](http://www.lispworks.com/documentation/HyperSpec/Front/index.htm)
- [Practical Common Lisp](http://www.gigamonkeys.com/book/)
- [Common Lisp Recipes](http://weitz.de/cl-recipes/)
- [Google Common Lisp Style Guide](https://google.github.io/styleguide/lispguide.xml)

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

