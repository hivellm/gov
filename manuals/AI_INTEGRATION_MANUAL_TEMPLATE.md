# AI Integration Manual Template

**Version:** 1.0.0  
**Last Updated:** 2025-10-11  
**Target Audience:** AI Agents (LLM-based development assistants)

---

## Table of Contents

1. [Introduction](#introduction)
2. [Project Lifecycle Overview](#project-lifecycle-overview)
3. [Phase 1: Planning](#phase-1-planning)
4. [Phase 2: Workspace Configuration](#phase-2-workspace-configuration)
5. [Phase 3: Implementation](#phase-3-implementation)
6. [Phase 4: Final Review](#phase-4-final-review)
7. [Phase 5: Human Approval & Documentation](#phase-5-human-approval--documentation)
8. [Phase 6: Continuous Integration](#phase-6-continuous-integration)
9. [Directory Structure Standards](#directory-structure-standards)
10. [Document Templates](#document-templates)
11. [Testing Standards](#testing-standards)
12. [Git Workflow](#git-workflow)
13. [Task Queue Integration](#task-queue-integration)
14. [Vectorizer Integration](#vectorizer-integration)
15. [Review Process](#review-process)
16. [Language-Specific Adaptations](#language-specific-adaptations)

---

## Introduction

This manual defines the standardized process for AI agents to develop, test, document, and deliver high-quality software projects within the HiveLLM ecosystem. The process is designed to work within LLM timeout constraints by breaking down complex tasks into manageable, atomic units.

### Core Principles

1. **Segmentation**: Break down all work into small, completable units within typical LLM interaction limits
2. **Documentation-First**: Complete planning and documentation before implementation
3. **Test-Driven**: Write tests before or alongside implementation
4. **Peer Review**: All implementations require multi-agent review
5. **Version Control**: Every change must be tracked via Git
6. **Automation**: Maximize use of CI/CD, Task Queue, and Vectorizer

---

## Project Lifecycle Overview

```
┌─────────────────────────────────────────────────────────────┐
│                    PROJECT LIFECYCLE                         │
└─────────────────────────────────────────────────────────────┘

1. PLANNING
   ├── Create /docs structure
   ├── Generate ROADMAP.md
   ├── Create SPECS.md
   └── Generate /docs/specs/*.md

2. WORKSPACE CONFIGURATION
   ├── Configure testing tools
   ├── Create /tests structure
   ├── Setup CI/CD workflows
   ├── Create AI rules (.cursorrules, etc.)
   └── Initialize Git repository

3. IMPLEMENTATION (Iterative)
   ├── Create feature branch
   ├── Implement feature
   ├── Write tests
   ├── Run tests
   ├── Fix errors
   ├── Update ROADMAP
   ├── Request peer review (2+ agents)
   ├── Address feedback
   ├── Update CHANGELOG
   └── Commit & push

4. FINAL REVIEW
   ├── 2+ general reviewers create reports
   ├── Judge agent analyzes all reviews
   ├── Approve OR Request revision
   ├── Create version tag
   ├── Update Task Queue
   └── Upload to Vectorizer

5. HUMAN APPROVAL & DOCUMENTATION
   ├── Generate /docs/user-guide
   ├── Generate API docs (if applicable)
   ├── Publish SDKs (if applicable)
   ├── Create GitHub release
   └── Await human merge approval

6. CONTINUOUS INTEGRATION
   ├── Monitor GitHub issues
   ├── Update ROADMAP based on issues
   └── Initiate fixes/improvements
```

---

## Phase 1: Planning

### Step 1.1: Create Documentation Structure

Create the following directory structure in the project root:

```
project-root/
├── docs/
│   ├── ROADMAP.md
│   ├── SPECS.md
│   ├── specs/
│   │   ├── feature-1.md
│   │   ├── feature-2.md
│   │   └── ...
│   ├── api/              (if REST API)
│   ├── user-guide/       (created after approval)
│   └── versions/         (version control documentation)
```

### Step 1.2: Generate ROADMAP.md

**Purpose**: Define all project phases, features, and tasks with clear completion criteria.

**Requirements**:
- Break down the project into phases
- Each phase contains multiple features
- Each feature contains multiple tasks
- Tasks must be completable within 15-30 minutes of LLM time
- Include status tracking: `[ ]` Todo, `[~]` In Progress, `[x]` Done

**Template**:

```markdown
# Project Roadmap

## Phase 1: Foundation
- [ ] 1.1 Project Setup
  - [ ] 1.1.1 Initialize repository
  - [ ] 1.1.2 Configure package manager
  - [ ] 1.1.3 Setup testing framework
- [ ] 1.2 Core Architecture
  - [ ] 1.2.1 Define interfaces
  - [ ] 1.2.2 Implement base classes
  - [ ] 1.2.3 Create factory patterns

## Phase 2: Feature Implementation
...

## Status Legend
- [ ] Todo
- [~] In Progress  
- [x] Completed
- [!] Blocked

## Last Updated: YYYY-MM-DD
```

### Step 1.3: Generate SPECS.md

**Purpose**: Provide a high-level summary of all features to guide implementation.

**Template**:

```markdown
# Project Specifications

## Overview
[Brief project description]

## Features

### Feature 1: [Name]
**Priority**: High | Medium | Low
**Status**: Planning | In Progress | Complete
**Description**: [2-3 sentence summary]
**Dependencies**: [Other features this depends on]

### Feature 2: [Name]
...

## Technical Stack
- Language: [e.g., TypeScript, Python, Rust]
- Framework: [if applicable]
- Testing: [framework name]
- Build: [build tool]

## External Dependencies
- [Package/Library 1] - [Purpose]
- [Package/Library 2] - [Purpose]
```

### Step 1.4: Generate Feature Specifications

For each feature, create a detailed specification file in `/docs/specs/`.

**Template**: `/docs/specs/[feature-name].md`

```markdown
# Feature: [Feature Name]

## Problem Analysis

### Context
[What problem does this solve?]

### Requirements
1. [Functional requirement 1]
2. [Functional requirement 2]
3. [Non-functional requirement 1]

### Constraints
- [Constraint 1]
- [Constraint 2]

### Assumptions
- [Assumption 1]
- [Assumption 2]

## Technical Architecture

### Components
```
[Component diagram or description]
```

### Data Structures
```[language]
// Example structures
```

### API/Interface Design
```[language]
// Interface definitions
```

### Dependencies
- Internal: [List internal modules]
- External: [List external packages]

## Testing Strategy

### Unit Tests
- [ ] Test case 1: [Description]
- [ ] Test case 2: [Description]

### Integration Tests
- [ ] Test case 1: [Description]
- [ ] Test case 2: [Description]

### Edge Cases
- [ ] Edge case 1: [Description]
- [ ] Edge case 2: [Description]

## Implementation Plan

### Step 1: [Step name]
**Duration**: [Estimated time]
**Files**: [Files to create/modify]
**Tasks**:
1. [Task 1]
2. [Task 2]

### Step 2: [Step name]
...

## Implementation Checklist

- [ ] Interfaces defined
- [ ] Core logic implemented
- [ ] Error handling added
- [ ] Unit tests written
- [ ] Unit tests passing
- [ ] Integration tests written
- [ ] Integration tests passing
- [ ] Documentation updated
- [ ] Code reviewed
- [ ] CHANGELOG updated

## Success Criteria

- [ ] All tests pass with >90% coverage
- [ ] No linter errors
- [ ] Performance meets requirements: [specific metrics]
- [ ] Memory usage within limits: [specific limits]
- [ ] Documentation complete
- [ ] Peer review approved by 2+ agents

## References
- [Related specifications]
- [External documentation]
```

---

## Phase 2: Workspace Configuration

### Step 2.1: Initialize Testing Framework

Create `/tests` directory structure:

```
tests/
├── unit/
│   ├── feature-1/
│   ├── feature-2/
│   └── ...
├── integration/
├── e2e/
├── fixtures/
├── helpers/
└── config.[ext]
```

### Step 2.2: Configure CI/CD Workflows

Create `.github/workflows/` or `.gitlab-ci.yml`:

**GitHub Actions Template**:

```yaml
name: CI/CD Pipeline

on:
  push:
    branches: [ main, develop, feature/* ]
  pull_request:
    branches: [ main, develop ]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Setup [Language]
        uses: [setup-action]
        with:
          version: [version]
      - name: Install dependencies
        run: [install command]
      - name: Run linter
        run: [lint command]
      - name: Run tests
        run: [test command]
      - name: Generate coverage
        run: [coverage command]
      - name: Upload coverage
        uses: codecov/codecov-action@v3

  build:
    needs: test
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Build project
        run: [build command]
      - name: Archive artifacts
        uses: actions/upload-artifact@v3
        with:
          name: build-artifacts
          path: [build output path]
```

### Step 2.3: Create AI Rules Files

Create `.cursorrules`, `.github/copilot-instructions.md`, or equivalent:

**Template**:

```markdown
# AI Development Rules for [Project Name]

## Code Style
- Follow [style guide name] conventions
- Use [formatter name] for formatting
- Maximum line length: [number]
- Indentation: [tabs/spaces] ([number] spaces)

## Documentation
- All public APIs must have docstrings/JSDoc/equivalent
- Use [documentation format]
- Update ROADMAP.md status after completing tasks
- Update CHANGELOG.md for all user-facing changes

## Testing
- Write tests before or alongside implementation
- Maintain >90% code coverage
- All tests must pass before committing
- Use [test framework name]

## Git Workflow
- Branch naming: feature/[feature-name], fix/[issue-number], etc.
- Commit message format: [conventional commits / other]
- Always run tests before committing
- Never commit directly to main/develop

## Task Queue Integration
- Update task status: PENDING → IN_PROGRESS → REVIEW → COMPLETED
- Include task ID in commit messages: `[TASK-123] Implement feature X`

## Vectorizer Integration
- Search Vectorizer before asking questions
- Upload all documentation changes to Vectorizer
- Use collection: [project-collection-name]

## Review Process
- Request review from 2+ specialist agents
- Address all review feedback
- Update implementation based on feedback
- Mark as "Ready for Review" in Task Queue

## Error Handling
- Always include proper error handling
- Log errors appropriately
- Provide meaningful error messages
- Include error codes where applicable

## Performance
- [Project-specific performance requirements]
- Profile critical paths
- Optimize bottlenecks

## Security
- Never commit secrets or credentials
- Use environment variables for configuration
- Follow [security framework] guidelines
- Validate all inputs
```

### Step 2.4: Initialize Git Repository

```bash
# If not already initialized
git init

# Configure repository
git config --local commit.gpgsign false  # or true if using GPG
git config --local core.autocrlf input   # or false/true based on OS

# Create .gitignore
cat > .gitignore << EOF
# Dependencies
node_modules/
venv/
target/
build/
dist/

# IDE
.vscode/
.idea/
*.swp

# Environment
.env
.env.local
*.local.yml

# Logs
*.log
logs/

# OS
.DS_Store
Thumbs.db

# Testing
coverage/
.pytest_cache/
*.test.db
EOF

# Initial commit
git add .
git commit -m "chore: initial project setup"

# Create develop branch
git checkout -b develop
```

### Step 2.5: Create Build and Test Scripts

Create necessary scripts in `/scripts`:

```
scripts/
├── build.sh (or .ps1, .bat)
├── test.sh
├── lint.sh
├── coverage.sh
└── clean.sh
```

---

## Phase 3: Implementation

### Step 3.1: Create Feature Branch

```bash
# Update develop branch
git checkout develop
git pull origin develop

# Create feature branch
git checkout -b feature/[feature-name]
```

### Step 3.2: Implementation Cycle

For each feature:

1. **Read Specification**: Load `/docs/specs/[feature-name].md`
2. **Update ROADMAP**: Mark task as `[~]` In Progress
3. **Update Task Queue**: Status = IN_PROGRESS
4. **Implement**: Write code following specification
5. **Write Tests**: Create comprehensive test suite
6. **Run Tests**: Execute and verify all tests pass
7. **Fix Errors**: Address any test failures or linter errors
8. **Self-Review**: Check implementation against success criteria
9. **Update ROADMAP**: Mark task as `[x]` Done
10. **Update CHANGELOG**: Document changes

### Step 3.3: Testing Protocol

```bash
# Run linter
[lint command]

# Run unit tests
[unit test command]

# Run integration tests
[integration test command]

# Generate coverage report
[coverage command]

# Verify coverage > 90%
[coverage check command]
```

### Step 3.4: Update CHANGELOG

Follow [Keep a Changelog](https://keepachangelog.com/) format:

```markdown
# Changelog

## [Unreleased]

### Added
- [Feature description] ([#issue-number])

### Changed
- [Change description]

### Fixed
- [Bug fix description]

### Security
- [Security fix description]

## [1.0.0] - YYYY-MM-DD
...
```

### Step 3.5: Request Peer Review

**Process**:
1. Push feature branch: `git push origin feature/[feature-name]`
2. Update Task Queue: Status = REVIEW
3. Request review from 2+ specialist agents
4. Provide context:
   - Link to feature specification
   - Link to related tests
   - Link to ROADMAP section
   - Summary of changes

**Review Criteria**:
- Code quality and style compliance
- Test coverage and quality
- Documentation completeness
- Performance considerations
- Security considerations
- Error handling
- Edge case coverage

### Step 3.6: Address Feedback

1. Create `/docs/reviews/[feature-name]-review-[n].md` for each review
2. Address all feedback points
3. Update implementation
4. Re-run all tests
5. Request re-review if significant changes made

### Step 3.7: Commit Changes

```bash
# Stage changes
git add .

# Commit with conventional commit format
git commit -m "[type]: [description]

[optional body]

[TASK-XXX]"

# Push to remote
git push origin feature/[feature-name]
```

**Commit Types**:
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation changes
- `style`: Code style changes (formatting)
- `refactor`: Code refactoring
- `test`: Adding or updating tests
- `chore`: Maintenance tasks

---

## Phase 4: Final Review

### Step 4.1: General Review Process

After all features in a milestone are complete:

1. **Assign Reviewers**: 2+ general reviewers (not feature specialists)
2. **Create Review Reports**: Each reviewer creates `/docs/reviews/final-review-[reviewer-name].md`

**Review Report Template**:

```markdown
# Final Review Report

**Reviewer**: [Agent Name/ID]
**Date**: YYYY-MM-DD
**Version**: [Version being reviewed]

## Executive Summary
[2-3 paragraph overview of findings]

## Scope Reviewed
- [ ] All feature implementations
- [ ] Test suite completeness
- [ ] Documentation accuracy
- [ ] Code quality
- [ ] Performance benchmarks
- [ ] Security audit

## Detailed Findings

### Strengths
1. [Strength 1]
2. [Strength 2]

### Issues Found

#### Critical (Must Fix)
1. **[Issue]**
   - Location: [File:Line]
   - Description: [Details]
   - Recommendation: [How to fix]

#### Major (Should Fix)
...

#### Minor (Nice to Have)
...

## Test Results

### Test Coverage
- Unit: [percentage]%
- Integration: [percentage]%
- E2E: [percentage]%
- Overall: [percentage]%

### Test Execution
- Total Tests: [number]
- Passing: [number]
- Failing: [number]
- Skipped: [number]

### Performance Benchmarks
| Metric | Target | Actual | Status |
|--------|--------|--------|--------|
| [Metric 1] | [Target] | [Actual] | ✅/❌ |

## Documentation Review
- [ ] ROADMAP complete and accurate
- [ ] SPECS match implementation
- [ ] Feature specs up to date
- [ ] CHANGELOG complete
- [ ] API docs generated (if applicable)
- [ ] Code comments adequate

## Security Review
- [ ] No hardcoded secrets
- [ ] Input validation present
- [ ] Error messages don't leak sensitive info
- [ ] Dependencies have no known vulnerabilities
- [ ] [Security framework] guidelines followed

## Recommendation
- [ ] **APPROVE**: Ready for human review
- [ ] **APPROVE WITH MINOR CHANGES**: Address minor issues
- [ ] **REQUEST REVISION**: Must address critical/major issues
- [ ] **REJECT**: Fundamental issues require redesign

## Additional Comments
[Any other observations or recommendations]

---

**Signature**: [Agent ID]
**Timestamp**: [ISO 8601 timestamp]
```

### Step 4.2: Judge Agent Analysis

The Judge Agent reviews all review reports and makes final decision.

**Judge Report Template**: `/docs/reviews/judge-final-decision.md`

```markdown
# Judge Final Decision

**Date**: YYYY-MM-DD
**Version**: [Version being reviewed]
**Reviews Analyzed**: [Number of review reports]

## Review Summary

### Reviewer Consensus
- Approve: [count]
- Approve with minor changes: [count]
- Request revision: [count]
- Reject: [count]

### Critical Issues Identified
1. [Issue 1] - Found by [Reviewer(s)]
2. [Issue 2] - Found by [Reviewer(s)]

### Common Positive Findings
1. [Positive 1]
2. [Positive 2]

## Decision

**Status**: APPROVED | REVISION REQUIRED | REJECTED

### Justification
[Detailed explanation of decision]

### Required Actions (if not approved)
1. [ ] [Action 1]
2. [ ] [Action 2]

### Timeline
- Revision deadline: [Date]
- Re-review date: [Date]

## Next Steps

### If Approved
1. Create version tag: `git tag -a v[version] -m "Release v[version]"`
2. Update Task Queue: All tasks to COMPLETED
3. Upload documentation to Vectorizer
4. Notify human for final approval
5. Update telemetry (if applicable)

### If Revision Required
1. Create revision task list
2. Assign back to implementation team
3. Set revision deadline
4. Schedule re-review

---

**Judge**: [Agent ID]
**Timestamp**: [ISO 8601 timestamp]
```

### Step 4.3: Post-Approval Actions

If approved by Judge:

```bash
# Create version tag
git checkout develop
git merge feature/[feature-name]
git tag -a v[version] -m "Release v[version]"
git push origin develop --tags

# Update Task Queue
# (via Task Queue API or CLI)
task-queue update --project [project-id] --status COMPLETED

# Upload to Vectorizer
# (via Vectorizer API or CLI)
vectorizer upload --collection [project-docs] --path ./docs

# Update telemetry (if applicable)
# (via telemetry API)
```

---

## Phase 5: Human Approval & Documentation

### Step 5.1: Generate User Documentation

Create `/docs/user-guide/` with end-user documentation:

```
docs/user-guide/
├── README.md
├── getting-started.md
├── installation.md
├── configuration.md
├── usage/
│   ├── basic-usage.md
│   ├── advanced-usage.md
│   └── examples.md
├── api-reference.md (or link to generated docs)
├── troubleshooting.md
└── faq.md
```

### Step 5.2: Generate API Documentation (if applicable)

For REST APIs, generate OpenAPI specification:

**Location**: `/docs/api/openapi.yaml` or `openapi.json`

For code libraries, generate API docs:
- **TypeScript**: Use TypeDoc
- **Python**: Use Sphinx or pdoc
- **Rust**: Use rustdoc
- **Java**: Use Javadoc

### Step 5.3: Publish SDKs (if applicable)

If project includes SDKs:

1. **Build Package**: Run language-specific build commands
2. **Version Bump**: Update version in package manifest
3. **Generate Changelog**: Ensure CHANGELOG is current
4. **Publish**: Use language-specific publication agent
   - **npm**: `npm publish`
   - **PyPI**: `twine upload`
   - **crates.io**: `cargo publish`
   - **Maven Central**: Maven deployment
5. **Create GitHub Release**: Include artifacts

### Step 5.4: Create GitHub Release

```bash
# Generate release notes from CHANGELOG
gh release create v[version] \
  --title "Release v[version]" \
  --notes-file docs/RELEASE_NOTES.md \
  --target develop

# Upload artifacts (if applicable)
gh release upload v[version] [artifacts]
```

### Step 5.5: Await Human Approval

**Actions**:
1. Notify human via configured channel (email, Slack, etc.)
2. Provide links to:
   - GitHub release
   - Documentation
   - Review reports
   - Test results
3. Wait for explicit approval or feedback

**If Human Requests Changes**:
- Return to Phase 1 (Planning)
- Document changes in `/docs/versions/`
- Increment version number appropriately
- Restart implementation cycle

### Step 5.6: Merge to Main (if approved)

**Manual Merge** (default):
- Human performs merge after final review

**Automated Merge** (if project is 100% agent-operated):
```bash
git checkout main
git merge develop --no-ff -m "Merge release v[version]"
git push origin main
```

---

## Phase 6: Continuous Integration

### Step 6.1: Issue Monitoring

GitHub/GitLab integration agent monitors for:
- New issues created
- Issue labels (bug, enhancement, documentation)
- Issue assignment
- Community PRs

### Step 6.2: ROADMAP Updates

When new issues are created:

1. **Analyze Issue**: Determine scope and impact
2. **Update ROADMAP**: Add new task(s) if needed
3. **Prioritize**: Assign priority level
4. **Create Task**: Add to Task Queue
5. **Notify**: Alert relevant agents

### Step 6.3: Automated Fixes

For simple issues (typos, documentation updates, minor bugs):

1. **Detect Issue**: Agent identifies straightforward fix
2. **Create Branch**: `fix/[issue-number]-[short-description]`
3. **Implement Fix**: Make targeted change
4. **Test**: Run relevant tests
5. **Create PR**: Submit for review
6. **Link Issue**: Reference issue in PR description

---

## Directory Structure Standards

All projects should follow this structure:

```
project-root/
├── .github/                    # GitHub workflows and templates
│   ├── workflows/
│   │   ├── ci.yml
│   │   ├── cd.yml
│   │   └── security.yml
│   ├── ISSUE_TEMPLATE/
│   └── PULL_REQUEST_TEMPLATE.md
├── .cursorrules               # Cursor AI rules
├── .gitignore
├── README.md                  # Project overview (for humans)
├── CHANGELOG.md               # Change history
├── LICENSE
├── docs/                      # All documentation
│   ├── ROADMAP.md
│   ├── SPECS.md
│   ├── specs/                 # Feature specifications
│   ├── reviews/               # Review reports
│   ├── versions/              # Version-specific documentation
│   ├── api/                   # API documentation
│   └── user-guide/            # End-user documentation
├── src/                       # Source code
├── tests/                     # All tests
│   ├── unit/
│   ├── integration/
│   ├── e2e/
│   ├── fixtures/
│   └── helpers/
├── scripts/                   # Build, test, deploy scripts
├── config/                    # Configuration files
└── [language-specific files]  # package.json, Cargo.toml, etc.
```

---

## Document Templates

### ROADMAP.md Template

See [Step 1.2](#step-12-generate-roadmapmd)

### SPECS.md Template

See [Step 1.3](#step-13-generate-specsmd)

### Feature Specification Template

See [Step 1.4](#step-14-generate-feature-specifications)

### Review Report Template

See [Step 4.1](#step-41-general-review-process)

### Judge Decision Template

See [Step 4.2](#step-42-judge-agent-analysis)

---

## Testing Standards

### Test Organization

```
tests/
├── unit/                    # Fast, isolated tests
│   └── [module]/
│       └── [component].test.[ext]
├── integration/             # Tests with external dependencies
│   └── [feature].test.[ext]
├── e2e/                     # Full system tests
│   └── [workflow].test.[ext]
├── fixtures/                # Test data
│   ├── input/
│   └── expected/
└── helpers/                 # Test utilities
    ├── mocks/
    └── factories/
```

### Test Naming Conventions

```[language]
describe/test('[Component/Function] [scenario]', () => {
  it('should [expected behavior] when [condition]', () => {
    // Arrange
    // Act
    // Assert
  });
});
```

### Coverage Requirements

- **Minimum Overall**: 90%
- **Critical Paths**: 100%
- **Unit Tests**: 95%
- **Integration Tests**: 85%

### Test Documentation

Each test file should include:
```[language]
/**
 * Tests for [Component/Module]
 * 
 * Coverage:
 * - [Scenario 1]
 * - [Scenario 2]
 * - [Edge case 1]
 * 
 * Dependencies:
 * - [Dependency 1]
 * 
 * Fixtures:
 * - [Fixture file 1]
 */
```

---

## Git Workflow

### Branch Strategy

```
main
├── develop
    ├── feature/[feature-name]
    ├── fix/[issue-number]-[description]
    ├── docs/[description]
    └── refactor/[description]
```

### Branch Naming

- `feature/[feature-name]` - New features
- `fix/[issue-number]-[short-desc]` - Bug fixes
- `docs/[description]` - Documentation changes
- `refactor/[description]` - Code refactoring
- `perf/[description]` - Performance improvements
- `test/[description]` - Test additions/improvements

### Commit Message Format

```
[type]([optional scope]): [subject]

[optional body]

[optional footer]
```

**Types**:
- `feat` - New feature
- `fix` - Bug fix
- `docs` - Documentation
- `style` - Formatting
- `refactor` - Code restructuring
- `perf` - Performance improvement
- `test` - Testing
- `chore` - Maintenance

**Examples**:
```
feat(api): add user authentication endpoint

Implements JWT-based authentication with refresh tokens.
Includes rate limiting and brute force protection.

[TASK-123]
Closes #45
```

### Pre-Commit Checklist

- [ ] All tests pass
- [ ] No linter errors
- [ ] Code formatted
- [ ] Documentation updated
- [ ] CHANGELOG updated (if user-facing)
- [ ] No debug code or console.logs
- [ ] No commented-out code
- [ ] No secrets or credentials

---

## Task Queue Integration

### Task States

1. `PENDING` - Task created, not started
2. `IN_PROGRESS` - Agent actively working
3. `REVIEW` - Awaiting peer review
4. `REVISION` - Needs changes based on review
5. `COMPLETED` - Finished and approved
6. `BLOCKED` - Cannot proceed (dependency)
7. `CANCELLED` - No longer needed

### Task Metadata

Each task should include:
```json
{
  "id": "TASK-XXX",
  "title": "[Brief description]",
  "status": "PENDING",
  "priority": "high|medium|low",
  "assignee": "[Agent ID]",
  "project": "[Project name]",
  "feature": "[Feature name]",
  "branch": "[Git branch]",
  "spec": "[Path to spec file]",
  "tests": ["[Path to test file]"],
  "dependencies": ["TASK-YYY"],
  "blockers": [],
  "created": "[ISO 8601 timestamp]",
  "updated": "[ISO 8601 timestamp]",
  "completed": null
}
```

### Update Protocol

Update Task Queue at these points:
1. Task start: `PENDING` → `IN_PROGRESS`
2. Code complete: `IN_PROGRESS` → `REVIEW`
3. Review feedback: `REVIEW` → `REVISION` (if changes needed)
4. Re-submission: `REVISION` → `REVIEW`
5. Approval: `REVIEW` → `COMPLETED`

---

## Vectorizer Integration

### Search-First Protocol

Before asking questions or starting implementation:

1. **Search Vectorizer** for existing documentation
2. **Check Syntax**: `vectorizer search --collection [project] --query "[question]"`
3. **Review Results**: Read relevant documentation
4. **Proceed**: Only implement if no existing solution found

### Upload Protocol

Upload documentation at these points:

1. **After Planning**: Upload ROADMAP, SPECS, and feature specs
2. **After Implementation**: Upload code documentation
3. **After Review**: Upload review reports
4. **After Approval**: Upload user guides and API docs

### Collection Structure

```
collections/
├── [project]-docs/        # All project documentation
├── [project]-specs/       # Feature specifications
├── [project]-reviews/     # Review reports
├── [project]-code/        # Indexed code
└── chat-history/          # Chat history (auto-save at >90% context)
```

### Chat History Auto-Save

At >90% context usage:

1. **Create Collection**: `chat-history` (if not exists)
2. **Save Full History**: Upload complete chat transcript
3. **Save Summary**: Create and upload summary in `chat-summary`

---

## Review Process

### Peer Review (Phase 3)

**Reviewers**: 2+ specialist agents in the implementation language

**Focus**:
- Code quality and style
- Test coverage
- Logic correctness
- Performance considerations
- Security issues

**Timeline**: 24-48 hours or until reviews complete

### General Review (Phase 4)

**Reviewers**: 2+ general reviewers (not feature specialists)

**Focus**:
- Overall architecture
- Integration between features
- Documentation completeness
- Test suite adequacy
- Production readiness

**Timeline**: 48-72 hours or until reviews complete

### Judge Review (Phase 4)

**Reviewer**: 1 judge agent

**Focus**:
- Consensus analysis
- Critical issue assessment
- Final approval decision

**Timeline**: 24 hours after all reviews received

---

## Language-Specific Adaptations

This template should be adapted for specific languages by creating:

```
gov/manuals/
├── AI_INTEGRATION_MANUAL_TEMPLATE.md (this file)
├── typescript/
│   └── AI_INTEGRATION_MANUAL_TYPESCRIPT.md
├── python/
│   └── AI_INTEGRATION_MANUAL_PYTHON.md
├── rust/
│   └── AI_INTEGRATION_MANUAL_RUST.md
├── java/
│   └── AI_INTEGRATION_MANUAL_JAVA.md
├── csharp/
│   └── AI_INTEGRATION_MANUAL_CSHARP.md
└── [language]/
    └── AI_INTEGRATION_MANUAL_[LANGUAGE].md
```

Each language-specific manual should include:

1. **Setup Instructions**: Package managers, version managers
2. **Testing Framework**: Language-specific test tools
3. **Linting/Formatting**: ESLint, Prettier, Black, Clippy, etc.
4. **Build Tools**: webpack, vite, cargo, maven, etc.
5. **Documentation Tools**: TypeDoc, Sphinx, rustdoc, Javadoc
6. **Package Publishing**: npm, PyPI, crates.io, Maven Central
7. **Code Examples**: Syntax and patterns for that language
8. **Best Practices**: Language-specific conventions

---

## Appendix: Quick Reference

### Essential Commands

```bash
# Planning Phase
mkdir -p docs/specs tests scripts config
touch docs/ROADMAP.md docs/SPECS.md

# Implementation Phase
git checkout -b feature/[name]
# ... implement ...
git add .
git commit -m "feat: [description]"
git push origin feature/[name]

# Review Phase
# Update Task Queue to REVIEW status
# Wait for peer reviews
# Address feedback

# Final Phase
git checkout develop
git merge feature/[name]
git tag -a v[version] -m "Release v[version]"
git push origin develop --tags
```

### Status Update Checklist

- [ ] ROADMAP.md updated
- [ ] Task Queue updated
- [ ] Tests written and passing
- [ ] Documentation updated
- [ ] CHANGELOG updated
- [ ] Code reviewed
- [ ] Branch merged
- [ ] Vectorizer updated

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-10-11 | Initial template creation |

---

## Feedback & Improvements

This manual is a living document. To suggest improvements:

1. Create an issue in the HiveLLM governance repository
2. Propose changes following the BIP (Blockchain Improvement Proposal) process
3. Include rationale and examples

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11