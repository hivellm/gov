# SQL Best Practices Guide

**Version**: 1.0.0  
**Last Updated**: 2025-10-11  
**Target Audience**: AI Agents & Developers

---

## Table of Contents

1. [SQL Idioms](#sql-idioms)
2. [Anti-Patterns](#anti-patterns)
3. [Schema Design](#schema-design)
4. [Query Optimization](#query-optimization)
5. [Security Best Practices](#security-best-practices)
6. [Indexing Strategies](#indexing-strategies)
7. [Transaction Management](#transaction-management)
8. [Common Patterns](#common-patterns)
9. [Performance Tips](#performance-tips)
10. [Common Gotchas](#common-gotchas)

---

## SQL Idioms

### 1. Use Explicit Column Names

**✅ Good**:
```sql
SELECT 
    u.id,
    u.username,
    u.email,
    u.created_at
FROM users u;
```

**❌ Bad**:
```sql
SELECT * FROM users;  -- Fragile, performance issues
```

### 2. Use Table Aliases

**✅ Good**:
```sql
SELECT 
    u.username,
    p.title,
    p.created_at
FROM users u
INNER JOIN posts p ON u.id = p.user_id;
```

**❌ Bad**:
```sql
SELECT 
    users.username,
    posts.title,
    posts.created_at
FROM users
INNER JOIN posts ON users.id = posts.user_id;
```

### 3. Use EXISTS Instead of IN for Subqueries

**✅ Good**:
```sql
SELECT u.username
FROM users u
WHERE EXISTS (
    SELECT 1
    FROM posts p
    WHERE p.user_id = u.id
        AND p.status = 'published'
);
```

**❌ Bad**:
```sql
SELECT username
FROM users
WHERE id IN (
    SELECT user_id
    FROM posts
    WHERE status = 'published'
);  -- Slower for large datasets
```

### 4. Use CASE for Conditional Logic

**✅ Good**:
```sql
SELECT 
    username,
    CASE 
        WHEN post_count = 0 THEN 'New User'
        WHEN post_count BETWEEN 1 AND 10 THEN 'Regular User'
        WHEN post_count > 10 THEN 'Power User'
        ELSE 'Unknown'
    END AS user_type
FROM user_stats;
```

### 5. Use CTEs for Readability

**✅ Good**:
```sql
WITH active_users AS (
    SELECT id, username, email
    FROM users
    WHERE status = 'active'
        AND deleted_at IS NULL
),
user_posts AS (
    SELECT 
        user_id,
        COUNT(*) AS post_count
    FROM posts
    WHERE status = 'published'
    GROUP BY user_id
)
SELECT 
    au.username,
    au.email,
    COALESCE(up.post_count, 0) AS total_posts
FROM active_users au
LEFT JOIN user_posts up ON au.id = up.user_id
ORDER BY up.post_count DESC NULLS LAST;
```

**❌ Bad**:
```sql
-- Nested subqueries are harder to read
SELECT 
    u.username,
    u.email,
    COALESCE(
        (SELECT COUNT(*) 
         FROM posts p 
         WHERE p.user_id = u.id 
           AND p.status = 'published'), 
        0
    ) AS total_posts
FROM users u
WHERE u.status = 'active'
    AND u.deleted_at IS NULL;
```

---

## Anti-Patterns

### 1. Using DISTINCT as a Band-Aid

**❌ Bad**:
```sql
-- Using DISTINCT to hide duplicate logic issues
SELECT DISTINCT 
    u.username,
    p.title
FROM users u
JOIN posts p ON u.id = p.user_id;
-- Figure out WHY you have duplicates first!
```

**✅ Good**:
```sql
-- Fix the root cause
SELECT 
    u.username,
    p.title
FROM users u
INNER JOIN posts p ON u.id = p.user_id
WHERE p.deleted_at IS NULL;
```

### 2. Boolean Flags Instead of Enum

**❌ Bad**:
```sql
CREATE TABLE orders (
    id BIGSERIAL PRIMARY KEY,
    is_pending BOOLEAN,
    is_processing BOOLEAN,
    is_completed BOOLEAN,
    is_cancelled BOOLEAN
);
-- Can have invalid states: all false or multiple true
```

**✅ Good**:
```sql
CREATE TABLE orders (
    id BIGSERIAL PRIMARY KEY,
    status VARCHAR(20) NOT NULL DEFAULT 'pending'
        CHECK (status IN ('pending', 'processing', 'completed', 'cancelled'))
);
```

### 3. Entity-Attribute-Value (EAV) Pattern

**❌ Bad**:
```sql
-- Generic key-value table
CREATE TABLE entity_attributes (
    entity_id INT,
    attribute_name VARCHAR(100),
    attribute_value TEXT
);
-- Hard to query, no type safety, no constraints
```

**✅ Good**:
```sql
-- Proper schema with typed columns
CREATE TABLE products (
    id BIGSERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    price DECIMAL(10,2) NOT NULL,
    weight DECIMAL(8,2),
    color VARCHAR(50)
);
```

### 4. Storing Delimited Lists in Columns

**❌ Bad**:
```sql
CREATE TABLE posts (
    id BIGSERIAL PRIMARY KEY,
    title VARCHAR(255),
    tags VARCHAR(500)  -- 'tag1,tag2,tag3'
);
-- Can't search efficiently, violates 1NF
```

**✅ Good**:
```sql
CREATE TABLE posts (
    id BIGSERIAL PRIMARY KEY,
    title VARCHAR(255)
);

CREATE TABLE tags (
    id BIGSERIAL PRIMARY KEY,
    name VARCHAR(50) UNIQUE NOT NULL
);

CREATE TABLE post_tags (
    post_id BIGINT REFERENCES posts(id),
    tag_id BIGINT REFERENCES tags(id),
    PRIMARY KEY (post_id, tag_id)
);
```

### 5. No Indexes on Foreign Keys

**❌ Bad**:
```sql
CREATE TABLE posts (
    id BIGSERIAL PRIMARY KEY,
    user_id BIGINT NOT NULL REFERENCES users(id)
    -- No index on user_id!
);
```

**✅ Good**:
```sql
CREATE TABLE posts (
    id BIGSERIAL PRIMARY KEY,
    user_id BIGINT NOT NULL REFERENCES users(id)
);

CREATE INDEX idx_posts_user_id ON posts(user_id);
```

---

## Schema Design

### 1. Normalization (Up to 3NF)

**✅ Good - 3rd Normal Form**:
```sql
-- Users table
CREATE TABLE users (
    id BIGSERIAL PRIMARY KEY,
    username VARCHAR(50) NOT NULL UNIQUE,
    email VARCHAR(255) NOT NULL UNIQUE
);

-- Posts table (no redundant user data)
CREATE TABLE posts (
    id BIGSERIAL PRIMARY KEY,
    user_id BIGINT NOT NULL REFERENCES users(id),
    title VARCHAR(255) NOT NULL,
    content TEXT NOT NULL
);
```

**❌ Bad - Denormalized**:
```sql
CREATE TABLE posts (
    id BIGSERIAL PRIMARY KEY,
    user_id BIGINT,
    username VARCHAR(50),     -- Redundant!
    user_email VARCHAR(255),  -- Redundant!
    title VARCHAR(255),
    content TEXT
);
```

### 2. Use Appropriate Data Types

**✅ Good**:
```sql
CREATE TABLE products (
    id BIGSERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    price DECIMAL(10,2) NOT NULL,        -- Money: DECIMAL
    stock_quantity INT NOT NULL,         -- Integer: INT
    is_available BOOLEAN DEFAULT TRUE,   -- Boolean: BOOLEAN
    created_at TIMESTAMP NOT NULL,       -- Timestamp: TIMESTAMP
    metadata JSONB                       -- JSON: JSONB (PostgreSQL)
);
```

**❌ Bad**:
```sql
CREATE TABLE products (
    id VARCHAR(50),              -- ID as string?
    name TEXT,                   -- TEXT for short strings?
    price VARCHAR(20),           -- Price as string?
    stock_quantity VARCHAR(10),  -- Numbers as strings?
    is_available VARCHAR(5),     -- Boolean as string?
    created_at VARCHAR(30)       -- Date as string?
);
```

### 3. Use Constraints

**✅ Good**:
```sql
CREATE TABLE orders (
    id BIGSERIAL PRIMARY KEY,
    user_id BIGINT NOT NULL REFERENCES users(id),
    total_amount DECIMAL(10,2) NOT NULL CHECK (total_amount >= 0),
    status VARCHAR(20) NOT NULL DEFAULT 'pending'
        CHECK (status IN ('pending', 'processing', 'completed', 'cancelled')),
    order_date TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    
    CONSTRAINT chk_completed_total CHECK (
        status != 'completed' OR total_amount > 0
    )
);
```

### 4. Soft Delete Pattern

**✅ Good**:
```sql
CREATE TABLE users (
    id BIGSERIAL PRIMARY KEY,
    username VARCHAR(50) NOT NULL,
    email VARCHAR(255) NOT NULL,
    deleted_at TIMESTAMP,
    
    -- Unique only for non-deleted records
    CONSTRAINT uq_users_email UNIQUE (email)
        WHERE deleted_at IS NULL
);

-- Partial index for active users
CREATE INDEX idx_users_active 
ON users(id) 
WHERE deleted_at IS NULL;
```

---

## Query Optimization

### 1. Use EXPLAIN ANALYZE

**✅ Good**:
```sql
-- Always analyze slow queries
EXPLAIN ANALYZE
SELECT 
    u.username,
    COUNT(p.id) AS post_count
FROM users u
LEFT JOIN posts p ON u.id = p.user_id
WHERE u.status = 'active'
GROUP BY u.id, u.username
HAVING COUNT(p.id) > 5;
```

### 2. Avoid SELECT * in Production

**✅ Good**:
```sql
-- Select only needed columns
SELECT id, username, email
FROM users
WHERE status = 'active'
LIMIT 100;
```

**❌ Bad**:
```sql
SELECT * FROM users;  -- Fetches unnecessary data
```

### 3. Use Indexes for WHERE Clauses

**✅ Good**:
```sql
-- Index on frequently filtered column
CREATE INDEX idx_posts_status ON posts(status);

-- Query uses index
SELECT id, title
FROM posts
WHERE status = 'published';  -- Uses idx_posts_status
```

### 4. Avoid Functions on Indexed Columns

**❌ Bad**:
```sql
-- Function prevents index usage
SELECT id, title
FROM posts
WHERE YEAR(created_at) = 2025;  -- Full table scan!
```

**✅ Good**:
```sql
-- Range query uses index
SELECT id, title
FROM posts
WHERE created_at >= '2025-01-01'
  AND created_at < '2026-01-01';  -- Uses index
```

### 5. Use LIMIT for Large Result Sets

**✅ Good**:
```sql
SELECT 
    id,
    username,
    email
FROM users
WHERE status = 'active'
ORDER BY created_at DESC
LIMIT 100 OFFSET 0;  -- Pagination
```

---

## Security Best Practices

### 1. Never Concatenate SQL (SQL Injection)

**❌ Bad - VULNERABLE**:
```sql
-- DO NOT DO THIS!
query = "SELECT * FROM users WHERE username = '" + username + "'";
-- User input: ' OR '1'='1
-- Resulting query: SELECT * FROM users WHERE username = '' OR '1'='1'
```

**✅ Good - Use Parameterized Queries**:
```javascript
// Node.js with pg
const query = 'SELECT * FROM users WHERE username = $1';
const result = await client.query(query, [username]);

// Python with psycopg2
cursor.execute("SELECT * FROM users WHERE username = %s", (username,))

// Java with PreparedStatement
PreparedStatement stmt = conn.prepareStatement(
    "SELECT * FROM users WHERE username = ?"
);
stmt.setString(1, username);
```

### 2. Principle of Least Privilege

**✅ Good**:
```sql
-- Create application user with limited permissions
CREATE USER app_user WITH PASSWORD 'secure_password';

-- Grant only necessary permissions
GRANT CONNECT ON DATABASE mydb TO app_user;
GRANT USAGE ON SCHEMA public TO app_user;
GRANT SELECT, INSERT, UPDATE ON TABLE users TO app_user;
GRANT SELECT ON TABLE posts TO app_user;

-- No DELETE or DROP permissions!
```

### 3. Encrypt Sensitive Data

**✅ Good**:
```sql
-- PostgreSQL pgcrypto extension
CREATE EXTENSION IF NOT EXISTS pgcrypto;

-- Store encrypted data
INSERT INTO sensitive_data (user_id, ssn)
VALUES (
    1,
    pgp_sym_encrypt('123-45-6789', 'encryption_key')
);

-- Decrypt when needed
SELECT 
    user_id,
    pgp_sym_decrypt(ssn, 'encryption_key') AS ssn
FROM sensitive_data;
```

### 4. Use Row-Level Security (PostgreSQL)

**✅ Good**:
```sql
-- Enable row-level security
ALTER TABLE posts ENABLE ROW LEVEL SECURITY;

-- Policy: users can only see their own posts
CREATE POLICY user_posts_policy ON posts
    FOR SELECT
    USING (user_id = current_setting('app.current_user_id')::BIGINT);
```

---

## Indexing Strategies

### 1. Index Foreign Keys

**✅ Good**:
```sql
CREATE INDEX idx_posts_user_id ON posts(user_id);
CREATE INDEX idx_comments_post_id ON comments(post_id);
```

### 2. Composite Indexes

**✅ Good**:
```sql
-- Index for WHERE status = ? AND user_id = ?
CREATE INDEX idx_posts_status_user ON posts(status, user_id);

-- Most selective column first
CREATE INDEX idx_orders_date_status ON orders(order_date, status);
```

### 3. Partial Indexes

**✅ Good**:
```sql
-- Index only active records
CREATE INDEX idx_users_active 
ON users(id, username) 
WHERE status = 'active' AND deleted_at IS NULL;

-- Smaller index, faster queries for active users
```

### 4. Covering Indexes

**✅ Good (PostgreSQL)**:
```sql
-- Include additional columns
CREATE INDEX idx_users_email_include 
ON users(email) 
INCLUDE (username, created_at);

-- Query satisfied by index alone (index-only scan)
SELECT username, created_at
FROM users
WHERE email = 'user@example.com';
```

---

## Transaction Management

### 1. Use Transactions for Data Integrity

**✅ Good**:
```sql
BEGIN;

-- Deduct from source account
UPDATE accounts
SET balance = balance - 100
WHERE id = 1;

-- Add to destination account
UPDATE accounts
SET balance = balance + 100
WHERE id = 2;

-- Both succeed or both fail
COMMIT;
```

### 2. Set Appropriate Isolation Levels

**✅ Good**:
```sql
-- Prevent dirty reads and non-repeatable reads
BEGIN TRANSACTION ISOLATION LEVEL REPEATABLE READ;

SELECT balance FROM accounts WHERE id = 1;
-- Other transactions can't modify this row until commit

UPDATE accounts SET balance = balance - 100 WHERE id = 1;

COMMIT;
```

### 3. Handle Deadlocks

**✅ Good**:
```sql
-- Always lock tables in the same order
BEGIN;

-- Lock in consistent order: user table first, then posts
SELECT * FROM users WHERE id = 1 FOR UPDATE;
SELECT * FROM posts WHERE user_id = 1 FOR UPDATE;

-- Perform updates

COMMIT;
```

---

## Common Patterns

### 1. Audit Trail Pattern

**✅ Good**:
```sql
-- Audit table
CREATE TABLE audit_log (
    id BIGSERIAL PRIMARY KEY,
    table_name VARCHAR(100) NOT NULL,
    record_id BIGINT NOT NULL,
    action VARCHAR(20) NOT NULL,
    old_values JSONB,
    new_values JSONB,
    user_id BIGINT,
    changed_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- Trigger function
CREATE OR REPLACE FUNCTION audit_trigger_func()
RETURNS TRIGGER AS $$
BEGIN
    IF TG_OP = 'UPDATE' THEN
        INSERT INTO audit_log (
            table_name, record_id, action, 
            old_values, new_values, user_id
        ) VALUES (
            TG_TABLE_NAME,
            NEW.id,
            'UPDATE',
            row_to_json(OLD),
            row_to_json(NEW),
            current_setting('app.current_user_id', true)::BIGINT
        );
    ELSIF TG_OP = 'DELETE' THEN
        INSERT INTO audit_log (
            table_name, record_id, action, 
            old_values, user_id
        ) VALUES (
            TG_TABLE_NAME,
            OLD.id,
            'DELETE',
            row_to_json(OLD),
            current_setting('app.current_user_id', true)::BIGINT
        );
    END IF;
    RETURN NULL;
END;
$$ LANGUAGE plpgsql;

-- Attach to tables
CREATE TRIGGER audit_users
AFTER UPDATE OR DELETE ON users
FOR EACH ROW EXECUTE FUNCTION audit_trigger_func();
```

### 2. Optimistic Locking

**✅ Good**:
```sql
CREATE TABLE products (
    id BIGSERIAL PRIMARY KEY,
    name VARCHAR(255),
    price DECIMAL(10,2),
    version INT NOT NULL DEFAULT 1
);

-- Update with version check
UPDATE products
SET 
    price = 29.99,
    version = version + 1
WHERE id = 1
  AND version = 5;  -- Only update if version matches

-- Check affected rows
-- If 0, someone else updated it first (conflict)
```

### 3. Pagination (Efficient)

**✅ Good - Keyset Pagination**:
```sql
-- First page
SELECT id, username, created_at
FROM users
WHERE deleted_at IS NULL
ORDER BY created_at DESC, id DESC
LIMIT 20;

-- Next page (using last record's values)
SELECT id, username, created_at
FROM users
WHERE deleted_at IS NULL
  AND (created_at, id) < ('2025-01-15 10:30:00', 12345)
ORDER BY created_at DESC, id DESC
LIMIT 20;
```

**❌ Bad - OFFSET Pagination**:
```sql
-- Gets slower as offset increases
SELECT id, username
FROM users
ORDER BY created_at DESC
LIMIT 20 OFFSET 100000;  -- Scans 100,000 rows!
```

---

## Performance Tips

### 1. Batch Inserts

**✅ Good**:
```sql
-- Batch insert (single transaction)
INSERT INTO users (username, email) VALUES
    ('user1', 'user1@example.com'),
    ('user2', 'user2@example.com'),
    ('user3', 'user3@example.com');
-- Much faster than individual INSERTs
```

### 2. Use VACUUM Regularly

```sql
-- PostgreSQL maintenance
VACUUM ANALYZE users;

-- Or let autovacuum handle it
-- (enabled by default in postgresql.conf)
```

### 3. Monitor Query Performance

```sql
-- PostgreSQL: Enable pg_stat_statements
CREATE EXTENSION pg_stat_statements;

-- View slow queries
SELECT 
    query,
    calls,
    total_exec_time,
    mean_exec_time,
    max_exec_time
FROM pg_stat_statements
ORDER BY mean_exec_time DESC
LIMIT 10;
```

---

## Common Gotchas

### 1. NULL Comparisons

```sql
-- NULL != NULL
SELECT * FROM users WHERE email != 'test@example.com';
-- Does NOT return rows where email IS NULL

-- Use IS NULL / IS NOT NULL
SELECT * FROM users WHERE email IS NOT NULL;
```

### 2. CHAR vs VARCHAR

```sql
-- CHAR pads with spaces (usually unnecessary)
CREATE TABLE test (
    code CHAR(5)  -- 'ABC' stored as 'ABC  '
);

-- Use VARCHAR instead
CREATE TABLE test (
    code VARCHAR(5)  -- 'ABC' stored as 'ABC'
);
```

### 3. Date/Time Comparisons

```sql
-- Might miss records
SELECT * FROM orders
WHERE order_date = '2025-01-15';

-- Better: use range
SELECT * FROM orders
WHERE order_date >= '2025-01-15'
  AND order_date < '2025-01-16';
```

---

## Additional Resources

- [PostgreSQL Performance Optimization](https://wiki.postgresql.org/wiki/Performance_Optimization)
- [MySQL Optimization Guide](https://dev.mysql.com/doc/refman/8.0/en/optimization.html)
- [SQL Antipatterns Book](https://pragprog.com/titles/bksqla/sql-antipatterns/)
- [Use The Index, Luke](https://use-the-index-luke.com/)
- [SQL Style Guide](https://www.sqlstyle.guide/)

---

**Version**: 1.0.0  
**Maintained by**: HiveLLM Governance Team  
**Last Updated**: 2025-10-11

