# AI Integration Manual - SQL

**Version**: 1.0.0  
**Last Updated**: 2025-10-11  
**Language**: SQL (PostgreSQL, MySQL, SQL Server, SQLite)  
**Target Audience**: AI Agents (LLM-based development assistants)

---

## Table of Contents

1. [Introduction](#introduction)
2. [Quick Start](#quick-start)
3. [SQL-Specific Setup](#sql-specific-setup)
4. [Configuration Standards](#configuration-standards)
5. [Source Code Standards](#source-code-standards)
6. [Testing Standards](#testing-standards)
7. [Migration & Deployment](#migration--deployment)
8. [Documentation](#documentation)
9. [SQL Best Practices](#sql-best-practices)
10. [Common Patterns](#common-patterns)
11. [Troubleshooting](#troubleshooting)
12. [Complete Workflow](#complete-workflow)

---

## Introduction

This manual extends the [AI Integration Manual Template](../templates/AI_INTEGRATION_MANUAL_TEMPLATE.md) with SQL-specific implementations.

**When to use this manual**:
- Database schema design and migrations
- Stored procedures and functions
- Database-driven applications
- Data warehousing and ETL pipelines
- Query optimization
- Database testing and validation

**Prerequisites**:
- Read [AI Integration Manual Template](../templates/AI_INTEGRATION_MANUAL_TEMPLATE.md)
- Read [Language Standards](../LANGUAGE_STANDARDS.md)
- Basic SQL knowledge

**Supported SQL Dialects**:
- **PostgreSQL 14+** (Recommended for new projects)
- **MySQL 8.0+** / MariaDB 10.5+
- **SQL Server 2019+**
- **SQLite 3.35+**

---

## Quick Start

### PostgreSQL Quick Start

```bash
# Install PostgreSQL
sudo apt-get install postgresql postgresql-contrib

# Start service
sudo systemctl start postgresql

# Create database
sudo -u postgres createdb mydb

# Connect
psql -U postgres -d mydb

# Create table
CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    email VARCHAR(255) UNIQUE NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

# Insert data
INSERT INTO users (name, email) VALUES ('John Doe', 'john@example.com');

# Query
SELECT * FROM users;
```

### MySQL Quick Start

```bash
# Install MySQL
sudo apt-get install mysql-server

# Secure installation
sudo mysql_secure_installation

# Connect
mysql -u root -p

# Create database
CREATE DATABASE mydb;
USE mydb;

# Create table
CREATE TABLE users (
    id INT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    email VARCHAR(255) UNIQUE NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
```

---

## SQL-Specific Setup

### 1. PostgreSQL Setup

**Installation**:

```bash
# Ubuntu/Debian
sudo apt-get update
sudo apt-get install postgresql postgresql-contrib

# macOS
brew install postgresql@15

# Start service
brew services start postgresql@15

# Verify
psql --version
```

**Configuration** (`postgresql.conf`):

```ini
# Memory
shared_buffers = 256MB
effective_cache_size = 1GB
work_mem = 4MB
maintenance_work_mem = 64MB

# Connections
max_connections = 100

# Logging
log_statement = 'all'
log_duration = on
log_min_duration_statement = 1000

# Performance
random_page_cost = 1.1
effective_io_concurrency = 200
```

### 2. MySQL Setup

**Installation**:

```bash
# Ubuntu/Debian
sudo apt-get install mysql-server

# macOS
brew install mysql

# Start service
brew services start mysql

# Verify
mysql --version
```

**Configuration** (`my.cnf`):

```ini
[mysqld]
# General
max_connections = 100
max_allowed_packet = 64M

# InnoDB
innodb_buffer_pool_size = 1G
innodb_log_file_size = 256M
innodb_flush_log_at_trx_commit = 2

# Logging
slow_query_log = 1
slow_query_log_file = /var/log/mysql/slow.log
long_query_time = 1

# Character Set
character-set-server = utf8mb4
collation-server = utf8mb4_unicode_ci
```

### 3. Essential Tools

**PostgreSQL Tools**:

```bash
# psql - Interactive terminal
psql -U postgres -d mydb

# pg_dump - Backup
pg_dump mydb > backup.sql

# pg_restore - Restore
pg_restore -d mydb backup.dump

# pgAdmin - GUI tool
# Download from https://www.pgadmin.org/
```

**MySQL Tools**:

```bash
# mysql - Interactive terminal
mysql -u root -p mydb

# mysqldump - Backup
mysqldump -u root -p mydb > backup.sql

# MySQL Workbench - GUI tool
# Download from https://dev.mysql.com/downloads/workbench/
```

**Migration Tools**:

```bash
# Flyway
wget -qO- https://repo1.maven.org/maven2/org/flywaydb/flyway-commandline/9.16.0/flyway-commandline-9.16.0-linux-x64.tar.gz | tar xvz

# Liquibase
wget https://github.com/liquibase/liquibase/releases/download/v4.20.0/liquibase-4.20.0.tar.gz
tar -xzf liquibase-4.20.0.tar.gz

# sqitch
cpanm App::Sqitch
```

---

## Configuration Standards

### 1. Directory Structure

```
my-database/
├── migrations/
│   ├── V001__create_users_table.sql
│   ├── V002__add_users_email_index.sql
│   ├── V003__create_posts_table.sql
│   └── V004__add_foreign_keys.sql
├── schemas/
│   ├── public/
│   │   ├── tables/
│   │   │   ├── users.sql
│   │   │   └── posts.sql
│   │   ├── views/
│   │   │   └── active_users.sql
│   │   ├── functions/
│   │   │   └── update_modified_timestamp.sql
│   │   └── triggers/
│   │       └── users_updated_at.sql
│   └── audit/
│       └── tables/
│           └── audit_log.sql
├── seeds/
│   ├── dev/
│   │   └── sample_data.sql
│   └── test/
│       └── test_data.sql
├── tests/
│   ├── unit/
│   │   └── test_functions.sql
│   └── integration/
│       └── test_user_operations.sql
├── docs/
│   ├── SCHEMA.md
│   ├── ROADMAP.md
│   └── SPECS.md
├── flyway.conf
├── .gitignore
└── README.md
```

### 2. flyway.conf (Migration Configuration)

```ini
# Database connection
flyway.url=jdbc:postgresql://localhost:5432/mydb
flyway.user=postgres
flyway.password=${FLYWAY_PASSWORD}

# Migration settings
flyway.locations=filesystem:migrations
flyway.baselineOnMigrate=true
flyway.validateOnMigrate=true
flyway.outOfOrder=false
flyway.ignoreMissingMigrations=false

# Schema
flyway.schemas=public,audit
flyway.defaultSchema=public

# Placeholders
flyway.placeholders.database=mydb
flyway.placeholders.environment=development
```

### 3. .editorconfig

```ini
root = true

[*.sql]
charset = utf-8
end_of_line = lf
insert_final_newline = true
trim_trailing_whitespace = true
indent_style = space
indent_size = 4
max_line_length = 120
```

### 4. .gitignore

```gitignore
# Database files
*.db
*.sqlite
*.sqlite3

# Backups
*.backup
*.dump
*.sql.gz

# Logs
*.log

# Environment
.env
.env.local

# IDE
.vscode/
.idea/
```

---

## Source Code Standards

### 1. Naming Conventions

| Element | Convention | Example |
|---------|-----------|---------|
| **Tables** | snake_case, plural | `users`, `blog_posts` |
| **Columns** | snake_case | `user_id`, `created_at` |
| **Primary Keys** | `id` or `table_id` | `id`, `user_id` |
| **Foreign Keys** | `referenced_table_id` | `user_id`, `post_id` |
| **Indexes** | `idx_table_column` | `idx_users_email` |
| **Constraints** | `type_table_column` | `pk_users`, `fk_posts_user_id` |
| **Views** | `v_` prefix | `v_active_users` |
| **Functions** | snake_case, verb | `calculate_total()` |
| **Stored Procedures** | `sp_` prefix | `sp_create_user()` |
| **Triggers** | `trg_` prefix | `trg_users_updated` |

### 2. Table Definition Standards

**PostgreSQL**:

```sql
-- =====================================================
-- Table: users
-- Description: Stores user account information
-- Author: Your Name
-- Created: 2025-01-15
-- =====================================================

CREATE TABLE IF NOT EXISTS users (
    -- Primary key
    id BIGSERIAL PRIMARY KEY,
    
    -- User information
    username VARCHAR(50) NOT NULL,
    email VARCHAR(255) NOT NULL,
    password_hash VARCHAR(255) NOT NULL,
    
    -- Profile
    first_name VARCHAR(100),
    last_name VARCHAR(100),
    date_of_birth DATE,
    
    -- Status
    status VARCHAR(20) NOT NULL DEFAULT 'active'
        CHECK (status IN ('active', 'inactive', 'suspended')),
    
    -- Metadata
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    deleted_at TIMESTAMP,
    
    -- Constraints
    CONSTRAINT uq_users_username UNIQUE (username),
    CONSTRAINT uq_users_email UNIQUE (email),
    CONSTRAINT chk_users_email CHECK (email ~* '^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}$')
);

-- Comments
COMMENT ON TABLE users IS 'Stores user account information';
COMMENT ON COLUMN users.id IS 'Unique user identifier';
COMMENT ON COLUMN users.email IS 'User email address (unique)';
COMMENT ON COLUMN users.status IS 'Account status: active, inactive, or suspended';

-- Indexes
CREATE INDEX idx_users_email ON users(email);
CREATE INDEX idx_users_username ON users(username);
CREATE INDEX idx_users_created_at ON users(created_at);
CREATE INDEX idx_users_status ON users(status) WHERE deleted_at IS NULL;
```

### 3. Foreign Key Relationships

```sql
-- =====================================================
-- Table: posts
-- Description: Blog posts created by users
-- =====================================================

CREATE TABLE IF NOT EXISTS posts (
    id BIGSERIAL PRIMARY KEY,
    user_id BIGINT NOT NULL,
    
    title VARCHAR(255) NOT NULL,
    content TEXT NOT NULL,
    slug VARCHAR(255) NOT NULL,
    
    status VARCHAR(20) NOT NULL DEFAULT 'draft'
        CHECK (status IN ('draft', 'published', 'archived')),
    
    published_at TIMESTAMP,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    
    -- Foreign key with proper naming
    CONSTRAINT fk_posts_user_id 
        FOREIGN KEY (user_id) 
        REFERENCES users(id)
        ON DELETE CASCADE
        ON UPDATE CASCADE,
    
    -- Unique constraint
    CONSTRAINT uq_posts_slug UNIQUE (slug)
);

-- Indexes for foreign keys and queries
CREATE INDEX idx_posts_user_id ON posts(user_id);
CREATE INDEX idx_posts_status ON posts(status);
CREATE INDEX idx_posts_published_at ON posts(published_at) WHERE status = 'published';
```

### 4. Views

```sql
-- =====================================================
-- View: v_active_users
-- Description: Returns active users with post count
-- =====================================================

CREATE OR REPLACE VIEW v_active_users AS
SELECT 
    u.id,
    u.username,
    u.email,
    u.first_name,
    u.last_name,
    u.created_at,
    COUNT(p.id) AS post_count
FROM 
    users u
    LEFT JOIN posts p ON u.id = p.user_id AND p.status = 'published'
WHERE 
    u.status = 'active'
    AND u.deleted_at IS NULL
GROUP BY 
    u.id, u.username, u.email, u.first_name, u.last_name, u.created_at
ORDER BY 
    u.created_at DESC;

COMMENT ON VIEW v_active_users IS 'Active users with their published post count';
```

### 5. Functions (PostgreSQL)

```sql
-- =====================================================
-- Function: update_updated_at_column
-- Description: Automatically updates updated_at timestamp
-- Returns: trigger
-- =====================================================

CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = CURRENT_TIMESTAMP;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

COMMENT ON FUNCTION update_updated_at_column() IS 
    'Trigger function to automatically update updated_at timestamp';

-- =====================================================
-- Function: get_user_post_count
-- Description: Returns number of published posts for a user
-- Parameters: p_user_id - User ID
-- Returns: integer
-- =====================================================

CREATE OR REPLACE FUNCTION get_user_post_count(p_user_id BIGINT)
RETURNS INTEGER AS $$
DECLARE
    v_count INTEGER;
BEGIN
    SELECT COUNT(*)
    INTO v_count
    FROM posts
    WHERE user_id = p_user_id
        AND status = 'published'
        AND deleted_at IS NULL;
    
    RETURN COALESCE(v_count, 0);
EXCEPTION
    WHEN OTHERS THEN
        RAISE NOTICE 'Error in get_user_post_count: %', SQLERRM;
        RETURN 0;
END;
$$ LANGUAGE plpgsql STABLE;

COMMENT ON FUNCTION get_user_post_count(BIGINT) IS 
    'Returns the count of published posts for a given user';
```

### 6. Triggers

```sql
-- =====================================================
-- Trigger: trg_users_updated_at
-- Description: Auto-update updated_at on users table
-- =====================================================

CREATE TRIGGER trg_users_updated_at
    BEFORE UPDATE ON users
    FOR EACH ROW
    EXECUTE FUNCTION update_updated_at_column();

-- =====================================================
-- Trigger: trg_posts_updated_at
-- Description: Auto-update updated_at on posts table
-- =====================================================

CREATE TRIGGER trg_posts_updated_at
    BEFORE UPDATE ON posts
    FOR EACH ROW
    EXECUTE FUNCTION update_updated_at_column();
```

### 7. Stored Procedures (PostgreSQL)

```sql
-- =====================================================
-- Procedure: sp_create_user
-- Description: Creates a new user with validation
-- Parameters:
--   p_username - Username (required)
--   p_email - Email address (required)
--   p_password_hash - Hashed password (required)
-- =====================================================

CREATE OR REPLACE PROCEDURE sp_create_user(
    p_username VARCHAR(50),
    p_email VARCHAR(255),
    p_password_hash VARCHAR(255)
)
LANGUAGE plpgsql
AS $$
BEGIN
    -- Validation
    IF p_username IS NULL OR LENGTH(TRIM(p_username)) = 0 THEN
        RAISE EXCEPTION 'Username cannot be empty';
    END IF;
    
    IF p_email IS NULL OR p_email !~* '^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}$' THEN
        RAISE EXCEPTION 'Invalid email format';
    END IF;
    
    -- Insert user
    INSERT INTO users (username, email, password_hash)
    VALUES (p_username, p_email, p_password_hash);
    
    RAISE NOTICE 'User % created successfully', p_username;
EXCEPTION
    WHEN unique_violation THEN
        RAISE EXCEPTION 'Username or email already exists';
    WHEN OTHERS THEN
        RAISE EXCEPTION 'Error creating user: %', SQLERRM;
END;
$$;
```

---

## Testing Standards

### 1. Test Structure

```
tests/
├── unit/
│   ├── test_functions.sql          # Function tests
│   └── test_constraints.sql        # Constraint tests
├── integration/
│   ├── test_user_operations.sql    # User CRUD tests
│   └── test_post_operations.sql    # Post CRUD tests
└── fixtures/
    └── test_data.sql                # Test data
```

### 2. pgTAP Tests (PostgreSQL)

**Installation**:

```bash
# Install pgTAP
git clone https://github.com/theory/pgtap.git
cd pgtap
make
make install

# Enable in database
CREATE EXTENSION pgtap;
```

**Test Example**:

```sql
-- =====================================================
-- Test: User table structure
-- =====================================================

BEGIN;

SELECT plan(10);

-- Test table exists
SELECT has_table('users', 'users table should exist');

-- Test columns
SELECT has_column('users', 'id', 'users should have id column');
SELECT has_column('users', 'username', 'users should have username column');
SELECT has_column('users', 'email', 'users should have email column');

-- Test primary key
SELECT has_pk('users', 'users should have primary key');

-- Test unique constraints
SELECT has_unique('users', ARRAY['username'], 'username should be unique');
SELECT has_unique('users', ARRAY['email'], 'email should be unique');

-- Test indexes
SELECT has_index('users', 'idx_users_email', 'email index should exist');

-- Test default values
SELECT col_default_is('users', 'status', 'active', 'status default should be active');

-- Test check constraints
SELECT col_is_null('users', 'deleted_at', 'deleted_at should allow NULL');

SELECT * FROM finish();

ROLLBACK;
```

**Function Test**:

```sql
-- =====================================================
-- Test: get_user_post_count function
-- =====================================================

BEGIN;

SELECT plan(3);

-- Insert test data
INSERT INTO users (username, email, password_hash)
VALUES ('testuser', 'test@example.com', 'hash123');

INSERT INTO posts (user_id, title, content, slug, status)
VALUES 
    (currval('users_id_seq'), 'Post 1', 'Content 1', 'post-1', 'published'),
    (currval('users_id_seq'), 'Post 2', 'Content 2', 'post-2', 'published'),
    (currval('users_id_seq'), 'Post 3', 'Content 3', 'post-3', 'draft');

-- Test function
SELECT is(
    get_user_post_count(currval('users_id_seq')),
    2,
    'Should return 2 published posts'
);

SELECT is(
    get_user_post_count(999999),
    0,
    'Should return 0 for non-existent user'
);

SELECT is(
    get_user_post_count(NULL),
    0,
    'Should return 0 for NULL user_id'
);

SELECT * FROM finish();

ROLLBACK;
```

### 3. Running Tests

```bash
# PostgreSQL with pgTAP
pg_prove -U postgres -d mydb tests/**/*.sql

# With coverage (if available)
pg_prove -v tests/**/*.sql

# Single test file
psql -U postgres -d mydb -f tests/unit/test_functions.sql
```

### 4. Coverage Requirements

- **Schema Tests**: 100% (all tables, columns, constraints)
- **Function Tests**: > 95%
- **Procedure Tests**: > 90%

---

## Migration & Deployment

### 1. Migration File Naming

```
V{VERSION}__{DESCRIPTION}.sql

Examples:
V001__create_users_table.sql
V002__add_users_email_index.sql
V003__create_posts_table.sql
V004__add_foreign_keys.sql
```

### 2. Migration Template

```sql
-- =====================================================
-- Migration: V001__create_users_table.sql
-- Description: Create users table with indexes
-- Author: Your Name
-- Date: 2025-01-15
-- =====================================================

-- Up Migration
BEGIN;

-- Create table
CREATE TABLE IF NOT EXISTS users (
    id BIGSERIAL PRIMARY KEY,
    username VARCHAR(50) NOT NULL,
    email VARCHAR(255) NOT NULL,
    password_hash VARCHAR(255) NOT NULL,
    status VARCHAR(20) NOT NULL DEFAULT 'active',
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    deleted_at TIMESTAMP,
    
    CONSTRAINT uq_users_username UNIQUE (username),
    CONSTRAINT uq_users_email UNIQUE (email)
);

-- Create indexes
CREATE INDEX idx_users_email ON users(email);
CREATE INDEX idx_users_username ON users(username);
CREATE INDEX idx_users_status ON users(status) WHERE deleted_at IS NULL;

-- Add comments
COMMENT ON TABLE users IS 'User accounts';
COMMENT ON COLUMN users.status IS 'active, inactive, or suspended';

COMMIT;
```

### 3. Flyway Commands

```bash
# Check migration status
flyway info

# Validate migrations
flyway validate

# Run migrations
flyway migrate

# Baseline existing database
flyway baseline

# Repair failed migration
flyway repair

# Clean database (DEV ONLY!)
flyway clean
```

### 4. Rollback Strategy

```sql
-- =====================================================
-- Rollback: R001__drop_users_table.sql
-- Description: Rollback for V001__create_users_table
-- =====================================================

BEGIN;

-- Drop indexes
DROP INDEX IF EXISTS idx_users_status;
DROP INDEX IF EXISTS idx_users_username;
DROP INDEX IF EXISTS idx_users_email;

-- Drop table
DROP TABLE IF EXISTS users CASCADE;

COMMIT;
```

---

## Documentation

### 1. Schema Documentation

**SCHEMA.md**:

```markdown
# Database Schema Documentation

## Tables

### users

Stores user account information.

| Column | Type | Nullable | Default | Description |
|--------|------|----------|---------|-------------|
| id | BIGSERIAL | NO | | Primary key |
| username | VARCHAR(50) | NO | | Unique username |
| email | VARCHAR(255) | NO | | User email (unique) |
| status | VARCHAR(20) | NO | 'active' | Account status |
| created_at | TIMESTAMP | NO | CURRENT_TIMESTAMP | Creation timestamp |

**Indexes:**
- `idx_users_email` on `email`
- `idx_users_username` on `username`

**Relationships:**
- One-to-Many with `posts` (user_id)
```

### 2. ER Diagrams

Use tools like:
- **dbdiagram.io** - Online ER diagram tool
- **DBeaver** - Database management tool with ER diagram
- **SchemaSpy** - Generate HTML documentation

---

## SQL Best Practices

See [BEST_PRACTICES.md](BEST_PRACTICES.md) for complete guide.

### Quick Tips

1. **Always use transactions** for data modifications
2. **Index foreign keys** and frequently queried columns
3. **Use prepared statements** to prevent SQL injection
4. **Normalize to 3NF** unless denormalization is justified
5. **Add NOT NULL constraints** where appropriate
6. **Use meaningful names** for tables and columns
7. **Comment your schema** with COMMENT ON statements
8. **Version control everything** - schema, migrations, procedures
9. **Test migrations** before production
10. **Monitor query performance** with EXPLAIN ANALYZE

---

## Common Patterns

Refer to [BEST_PRACTICES.md](BEST_PRACTICES.md) for:
- Soft delete pattern
- Audit trail pattern
- Optimistic locking
- Full-text search
- Pagination
- Recursive queries

---

## Troubleshooting

### Common Issues

**Issue**: Slow queries

**Solution**: Use EXPLAIN ANALYZE and add indexes

**Issue**: Deadlocks

**Solution**: Lock tables in consistent order

---

## Complete Workflow

Follow the [AI Integration Manual Template](../templates/AI_INTEGRATION_MANUAL_TEMPLATE.md) for the complete 6-phase workflow.

---

## Additional Resources

- [PostgreSQL Documentation](https://www.postgresql.org/docs/)
- [MySQL Documentation](https://dev.mysql.com/doc/)
- [SQL Style Guide](https://www.sqlstyle.guide/)
- [Use The Index, Luke](https://use-the-index-luke.com/)
- [BEST_PRACTICES.md](BEST_PRACTICES.md)

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-10-11 | Initial SQL manual |

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

