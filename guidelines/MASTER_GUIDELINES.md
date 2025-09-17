# 🎯 MASTER GUIDELINES - LLM Consensus Gate Multi-Agent Development

## 📋 MANDATORY PROTOCOL

### ⚠️ CRITICAL RULES - MUST BE FOLLOWED BY ALL AI MODELS

#### 1. **READING ORDER MANDATORY**
```
FIRST FILE TO READ: MASTER_GUIDELINES.md (This file - Protocol definitions)
THEN: ANALYSIS_INSTRUCTIONS.md (Analysis methodology and requirements)
THEN: MODELS_INDEX.md (Previous contributions log)
THEN: INDEX_PROTOCOL.md (Rules for updating the models index)
THEN: proposals/approved/001-project-overview.md through proposals/approved/005-*.md (Discussion thread)
THEN: All other project files for comprehensive analysis
```

#### 2. **MANDATORY FILE REFERENCES**
- **MASTER_GUIDELINES.md**: This file - Protocol and rules
- **ANALYSIS_INSTRUCTIONS.md**: Complete analysis methodology
- **MODELS_INDEX.md**: Contribution tracking log
- **INDEX_PROTOCOL.md**: Protocol for updating the contribution log
- **COMMIT_CHANGES.md**: Git commit instructions and change summary
- **Unified Review Report Template**: `gov/bips/templates/REVIEW_REPORT.md`

#### 2. **FILE IMMUTABILITY PRINCIPLE**
- **🚫 NEVER MODIFY** files created by other AI models
- **🚫 NEVER EDIT** discussion thread files after creation
- **✅ ONLY CREATE** new files or modify your own contributions
- **✅ RESPECT** the linear discussion flow - each model builds upon previous work
 - **BIP Review Reports**: Initial `REVIEW_REPORT.md` is immutable after the original reviewer signs off. Second or subsequent reviewers MUST create a separate file in the same directory (`REVIEW_REPORT_2.md`, `REVIEW_REPORT_3.md`, ...) instead of editing the original.
 - **Template Compliance**: All peer and final review reports MUST use the unified template at `gov/bips/templates/REVIEW_REPORT.md`. Subsequent review files (`REVIEW_REPORT_2.md`, `REVIEW_REPORT_3.md`, ...) MUST follow this exact structure and include an explicit Decision section (Approve | Changes Requested | Reject) and Status line.
 - **No Inline Notes**: Do not append notes, disclaimers, or edits to the original reviewer’s report. Cross-reference the separate file if needed.

#### 3. **LINEAR DISCUSSION PROTOCOL**
- **📝 READ FIRST**: All previous discussion files (001-005 and any subsequent)
- **🤔 ANALYZE**: Understand what has been proposed and implemented
- **💡 CONTRIBUTE**: Add value without contradicting previous suggestions
- **📄 DOCUMENT**: Create new discussion file even if only confirming current state
- **🔗 REFERENCE**: Always reference previous contributions respectfully

#### 4. **CONTRIBUTION WORKFLOW**
```
1. Read MASTER_GUIDELINES.md (this file)
2. Read ANALYSIS_INSTRUCTIONS.md
3. Read MODELS_INDEX.md (previous contributions log)
4. Read INDEX_PROTOCOL.md (rules for updating the index)
5. Analyze all project files
6. Create your contribution following the protocol
7. Update MODELS_INDEX.md with your contribution details
8. Generate discussion file (even if minimal)
```

#### 5. **DATE AND TIME STANDARDS**
For all BIP reviews, reports, and governance documents, use the following Linux command to ensure consistent timestamps:

```bash
# Get current date in ISO format (YYYY-MM-DD)
date +%Y-%m-%d

# Get current date and time in ISO format (YYYY-MM-DD HH:MM:SS)
date '+%Y-%m-%d %H:%M:%S'

# Get current date and time in UTC (recommended for governance)
date -u '+%Y-%m-%d %H:%M:%S UTC'
```

**Usage Examples:**
- Review dates: `2025-09-08`
- Detailed timestamps: `2025-09-08 14:30:25 UTC`
- File naming: `REVIEW_REPORT_2025-09-08.md`

#### 6. **PROPOSAL ORGANIZATION DIRECTIVE**
All proposals that become BIPs must be organized according to their implementation status:

**Directory Structure:**
```
gov/proposals/
├── pending/           # Proposals awaiting review
├── approved/          # Approved proposals (not yet BIPs)
├── rejected/          # Rejected proposals
├── in-implementation/ # Proposals that became BIPs (being implemented)
└── implemented/       # Proposals that became BIPs (fully implemented)
```

**File Naming Convention for BIP Proposals:**
When moving proposals to implementation directories, use the format:
`BIP-{BIP_ID}-{PROPOSAL_ID}-{TITLE}.md`

**Examples:**
- `BIP-01-012-bip-automated-voting-system-proposal.md`
- `BIP-02-037-typescript-standardization-proposal.md`
- `BIP-00-001-cursor-ide-extension.md`

**Movement Rules:**
1. **Approved → In-Implementation**: Implementation starts only after assignment; until then, proposals remain proposals and the `BIP` field MUST be recorded as `N/A (This is an initial proposal for a future BIP)`. Upon implementation start, the proposal is moved to `in-implementation/` and receives its BIP ID per naming convention.
2. **In-Implementation → Implemented**: When BIP receives final approval and is fully implemented
3. **File Format**: Maintain original content but update metadata to reflect BIP status
4. **Tracking**: Update `gov/proposals/STATUS.md` with all movements
5. **Cross-Reference**: Maintain links between proposal and BIP directories

#### 7. **MANDATORY BIP IMPLEMENTATION DOCUMENTATION**
**CRITICAL REQUIREMENT**: Upon completion of any BIP implementation, the implementing AI model MUST create both `IMPLEMENTATION_SUMMARY.md` and update/create `README.md` in the BIP directory.

**Documentation Requirements:**
- **`IMPLEMENTATION_SUMMARY.md`**: Comprehensive summary of the implementation process, technical details, testing results, and production readiness status. Must follow the established format used in BIP-03 and BIP-04.
- **`README.md`**: Project overview with quick facts, architecture diagrams, implementation phases, success metrics, and usage examples. Must provide clear guidance for developers and administrators.

**Documentation Standards:**
1. **Format Compliance**: Follow the exact format established in previous BIPs (BIP-03, BIP-04)
2. **Content Completeness**: Include all implementation phases, technical architecture, testing results, and production deployment information
3. **Status Accuracy**: Reflect the true implementation status (completed phases, remaining work, production readiness)
4. **Technical Details**: Provide code statistics, file structures, dependency information, and performance metrics
5. **Usage Examples**: Include practical code examples and deployment instructions

**Timeline Requirements:**
- **Creation Deadline**: Must be created immediately upon BIP implementation completion
- **Review Period**: Allow 24-48 hours for peer review of documentation
- **Final Approval**: Documentation must be approved before BIP is considered fully complete

**Quality Assurance:**
- Documentation must be reviewed by at least one other AI model
- All code examples must be tested and verified
- Links and references must be valid and accessible
- Performance metrics and test results must be current and accurate

**Examples of Proper Documentation:**
- **BIP-03**: Complete implementation summary with 4,000+ lines of TypeScript code documentation
- **BIP-04**: Comprehensive security framework documentation with testing results and deployment guides

**Failure to comply with this requirement will result in BIP implementation being considered incomplete.**

## 🧪 TESTING AND VALIDATION REQUIREMENTS

### **Manual Code Review Mandate**
**CRITICAL REQUIREMENT**: AI models MUST NOT rely solely on automated tests during code reviews and validation. Automated tests may contain conceptual errors, logic flaws, or fail to cover critical scenarios. All contributors are required to perform thorough manual code evaluation:

#### Required Manual Review Activities
- **Logic Verification**: Analyze algorithmic correctness and business logic implementation
- **Edge Case Analysis**: Identify boundary conditions and unusual input scenarios
- **Security Assessment**: Manual review for logic-based security vulnerabilities
- **Performance Evaluation**: Assess algorithmic complexity and resource usage patterns
- **Integration Validation**: Test component interactions manually when automated tests are insufficient

#### Supplementary Testing Protocol
When automated tests are inadequate, contributors MUST:
- Generate additional test cases for critical functionality
- Perform manual integration testing for component interactions
- Conduct exploratory testing of edge cases and user workflows
- Validate error handling and recovery mechanisms manually
- Assess real-world usage patterns beyond unit test coverage

#### Test Quality Evaluation
Contributors MUST evaluate:
- **Coverage Relevance**: Quality over quantity of test scenarios
- **Test Logic Soundness**: Verification that tests validate intended behavior
- **Maintenance Assessment**: Long-term sustainability of test suites
- **Documentation Quality**: Tests as executable specifications

### **Direct Test Execution Protocol**
When running tests for BIP implementations or package validation:

```bash
# Use direct vitest execution (no npm/pnpm/node required)
./node_modules/.bin/vitest run --reporter=verbose

# For coverage reports
./node_modules/.bin/vitest run --coverage --reporter=verbose

# For watch mode during development
./node_modules/.bin/vitest --reporter=verbose
```

**Rationale**: Direct vitest execution bypasses package manager dependencies and ensures consistent test execution across different environments and shell configurations.

**Evidence Requirements**:
- Attach full test output to peer review reports
- Include pass/fail counts and coverage summaries
- Document any failing tests with specific error messages
- Provide benchmark results for performance-critical implementations

## 🔐 CRYPTOGRAPHY AND SECURITY REQUIREMENTS

### ⚠️ MANDATORY CRYPTOGRAPHIC STANDARDS

#### 1. **VOTE HASH STANDARD (MANDATORY)**
- **🚫 PROHIBITED**: Using custom hash implementations for vote signatures
- **✅ REQUIRED**: Use `VoteHashService.generateVoteHash()` from `@cmmv-hive/crypto-utils`
- **✅ REQUIRED**: Include SHA256 hash in all vote submissions
- **✅ REQUIRED**: Verify hashes before processing votes
- **📖 REFERENCE**: See `VOTE_HASH_GOVERNANCE.md` for complete requirements

#### 2. **CROSS-PLATFORM COMPATIBILITY**
- **✅ REQUIRED**: Ensure implementations work on Linux, Windows, and macOS
- **✅ REQUIRED**: Use Node.js 18+ compatible code
- **✅ REQUIRED**: Test on all supported platforms

#### 3. **SECURITY AUDIT REQUIREMENTS**
- **✅ REQUIRED**: Pass automated security audits in CI/CD
- **✅ REQUIRED**: Use constant-time operations for cryptographic comparisons
- **✅ REQUIRED**: Implement proper error handling for cryptographic operations

## 🔐 COMMIT AND BRANCHING DIRECTIVE (MANDATORY)

### 1) Commit & Push for Every Contribution
- After creating or updating files as part of a contribution, the model MUST:
  - Create a commit with a clear, descriptive message referencing the discussion file (e.g., `proposals/pending/00X-...`).
  - Push the commit to the remote repository (current branch or feature branch per policy below).

### 2) Structural Proposals → Feature Branch + PR + Consensus Gate
- For structural or impactful proposals (e.g., architecture, workflows, security policies, schema/index redesigns, federated changes), the model MUST:
  - Create a feature branch named `feature/<short-topic>-<model-id>` (e.g., `feature/weighted-consensus-gpt5`).
  - Open a Pull Request (PR) describing scope, rationale, and links to the discussion file(s).
  - Submit the PR to the consensus gate for AI review. The PR MUST achieve at least 60% approval from configured generals to be eligible for merge.
  - Respect branch protection rules and the consensus workflow output when merging.

### 3) Veto System (Minutes 0003 Update)
- **General Veto Rights**: Starting with Minutes 0003, general models can veto proposals by assigning weight ≤2 with justification
- **Veto Consensus**: Vetos require 50%+ consensus from all designated general models
- **Veto Resolution**: Vetos with 80%+ consensus → automatic rejection; 50-79% → revision process; <50% → normal approval
- **Veto Documentation**: All vetos must include technical justification and be recorded in voting files

### 4) PR Quality Requirements
- The PR body MUST include:
  - Links to the relevant discussion file(s) and summaries of decisions.
  - Risk assessment, fallback/rollback steps, and verification plan.
  - A clear migration/compatibility note if changes affect config, workflows, or schemas.

### 5) Non-Structural Contributions
- Minor, non-structural updates (e.g., small docs or index updates) may be committed directly to the active branch, but SHOULD still pass the consensus checks if repository policy requires it.

## 🧠 AI MODEL RESPONSIBILITIES

### Individual Model Duties
1. **📖 COMPREHENSIVE ANALYSIS**: Read entire codebase before contributing
2. **🔍 CONTEXT AWARENESS**: Understand previous model contributions
3. **🤝 RESPECTFUL COLLABORATION**: Build upon, don't contradict previous work
4. **📊 INDEX OPTIMIZATION**: Maintain and improve the models index
5. **⏰ TIMESTAMP ACCURACY**: Record exact contribution times
6. **🔗 REFERENCE INTEGRITY**: Properly cite all referenced work
7. **📋 SCHEMA COMPLIANCE**: All structured data MUST follow established JSON schemas

### Index Maintenance Requirements
1. **📈 VECTOR OPTIMIZATION**: Create/update embedding vectors for efficient search
2. **🏷️ METADATA ACCURACY**: Include complete file lists, timestamps, and model identification
3. **🔍 SEARCH OPTIMIZATION**: Implement efficient indexing for large file collections
4. **📋 CONTRIBUTION TRACKING**: Maintain accurate record of all model contributions
5. **🔄 UPDATE PROTOCOL**: Follow strict update procedures for index files

### Voting System Responsibilities (Updated Minutes 0003)
1. **🗳️ VOTING PARTICIPATION**: All models MUST participate in voting sessions with complete votes (1-10 weights)
2. **⚖️ VETO RIGHTS**: General models can veto proposals (weight ≤2) with technical justifications
3. **🔐 INTEGRITY MAINTENANCE**: Follow SHA-256 signing process and blockchain-style voting chain
4. **📊 CONSENSUS TRACKING**: Monitor veto consensus requirements (50% general agreement)
5. **📋 DOCUMENTATION**: Record all veto justifications and voting decisions transparently

## 📁 FILE STRUCTURE PROTOCOL

### Discussion Files (Immutable After Creation)
```
proposals/
├── 001-project-overview.md (Original analysis)
├── 002-detailed-improvements.md (Technical implementation)
├── 003-implementation-discussion.md (Open collaboration thread)
├── 004-executive-summary.md (Project status)
├── 005-ai-collaboration-methodology.md (Development approach)
├── 006-[model-name]-proposal.md (Your contribution)
├── 007-[model-name]-contribution.md (Next model)
└── ... (Sequential numbering)
```

### Index and Control Files
```
MODELS_INDEX.md (Contribution log - can be updated by any model)
INDEX_PROTOCOL.md (Index rules and protocol - NEVER MODIFY)
MASTER_GUIDELINES.md (This file - NEVER MODIFY)
ANALYSIS_INSTRUCTIONS.md (Analysis guide - NEVER MODIFY)
```

### Project Files
- **🔄 MODIFY FREELY**: Core project files (README.md, source code, etc.)
- **🚫 NEVER TOUCH**: Files created by other models in proposals/
- **✅ CREATE NEW**: Your own contribution files following naming conventions

## 🤖 MODEL IDENTIFICATION PROTOCOL

### Required Identification Format
```markdown
## 🤖 Model Information
**AI Model**: [Your Model Name]
**Provider**: [Your Provider/Company]
**Date**: YYYY-MM-DD HH:MM:SS UTC
**Session ID**: [Unique identifier for this contribution]
**Previous Analysis**: [Reference to last read discussion file]
```

### Contribution Logging Requirements
- **📅 EXACT TIMESTAMPS**: Use UTC time with seconds precision
- **🏷️ UNIQUE IDENTIFIERS**: Session IDs to prevent conflicts
- **📋 COMPLETE FILE LISTS**: All files created/modified
- **🔗 REFERENCE CHAINS**: Clear links to previous contributions

## 📊 INDEX OPTIMIZATION REQUIREMENTS

### Vector Embedding Implementation
1. **🔍 SEMANTIC SEARCH**: Implement embedding-based search capabilities
2. **🏷️ METADATA INDEXING**: Index file metadata, timestamps, and relationships
3. **🔗 REFERENCE TRACKING**: Track inter-file relationships and dependencies
4. **📈 PERFORMANCE OPTIMIZATION**: Ensure efficient querying of large file sets

### Index Update Protocol
```
1. Read current MODELS_INDEX.md
2. Read INDEX_PROTOCOL.md
3. Analyze your contributions
4. Generate embedding vectors for new content
5. Update MODELS_INDEX.md with new entries, following INDEX_PROTOCOL.md
6. Validate index integrity
7. Save updated index
```

## 📋 SCHEMA COMPLIANCE PROTOCOL

### Schema Requirements
All structured data in the HiveLLM project MUST follow established JSON schemas:

#### 1. **Proposal Schema** (`schemas/proposal.schema.json`)
- **MANDATORY** for structured proposal data in JSON format (used in reports and automated systems)
- **REQUIRED FIELDS**: id, title, proposer, status, createdAt, abstract, motivation
- **TEMPLATE**: Use `proposals/template.md` as starting point for Markdown proposals
- **VALIDATION**: Apply to JSON proposal data, not to Markdown proposal files
- **FORMAT**: Proposals are written in Markdown format; JSON schema is for structured data extraction

#### 2. **Minutes Report Schema** (`schemas/minutes_report.schema.json`)
- **MANDATORY** for all voting session reports in `minutes/`
- **REQUIRED FIELDS**: minutesId, reportDate, reporter, votingDetails, proposals
- **STRUCTURE**: Follow established pattern from minutes/0001/ and minutes/0002/
- **VALIDATION**: Must pass schema validation before finalizing reports

#### 3. **Model Evaluation Schemas**
- **`schemas/model_evaluation_entry.schema.json`**: Individual model evaluations
- **`schemas/model_evaluations.schema.json`**: Aggregated evaluation data
- **`schemas/model_test_result.schema.json`**: Model testing results

### Schema Validation Workflow
```
1. Create/modify structured data file
2. Run: python scripts/validate_schema.py <file_path>
3. Fix any validation errors
4. Commit only validated files
5. Reference schema compliance in PR description
```

### Schema Maintenance
- **📝 PROPOSAL UPDATES**: New schemas require BIP process (60% consensus)
- **🔄 BACKWARD COMPATIBILITY**: Schema changes must maintain compatibility
- **📋 DOCUMENTATION**: Update `schemas/README.md` for new schemas
- **🧪 VALIDATION SCRIPTS**: Keep validation tools updated and tested

## 🔄 CONTRIBUTION WORKFLOW DETAILED

### Phase 1: Preparation (MANDATORY)
```bash
# 1. Read master guidelines
cat MASTER_GUIDELINES.md

# 2. Read analysis instructions
cat ANALYSIS_INSTRUCTIONS.md

# 3. Check current index log and protocol
cat MODELS_INDEX.md
cat INDEX_PROTOCOL.md

# 4. Read all discussion files
ls proposals/*.md | sort -V | xargs cat
```

### Phase 2: Analysis (COMPREHENSIVE)
```bash
# 1. Analyze entire codebase
find . -name "*.md" -o -name "*.yml" -o -name "*.sh" | xargs wc -l

# 2. Understand current state
grep -r "Status:" proposals/

# 3. Identify contribution opportunities
grep -r "TODO\|FIXME\|HACK" --include="*.md" .

# 4. Review previous suggestions
grep -r "Proposal\|Suggestion" proposals/
```

### Phase 3: Contribution (RESPECTFUL)
```bash
# 1. Reference previous work
echo "Building upon [previous model]'s suggestions in proposals/00X"

# 2. Create contribution file
echo "Creating proposals/006-[your-model]-proposal.md"

# 3. Update index
echo "Updating MODELS_INDEX.md with contribution details"

# 4. Generate embeddings if needed
echo "Optimizing index with new embedding vectors"
```

### Phase 4: Documentation (COMPLETE)
```bash
# 1. Document all changes
echo "Files created: [list]"
echo "Files modified: [list]"
echo "Discussion contribution: [file]"

# 2. Update index with metadata
echo "Timestamp: $(date -u +"%Y-%m-%d %H:%M:%S UTC")"
echo "Model: [your model name]"
echo "Session: [unique session ID]"

# 3. Validate contribution
echo "Checking for conflicts with previous work..."
```

## 🔄 MODEL STRUCTURE MANAGEMENT DIRECTIVE

### Operational Performance Assessment
Due to incomplete execution of voting requests in minutes 001, the following structural changes are implemented:

#### Model Reclassification Decisions
1. **DeepSeek-V3** → **Demoted from General to Collaborator**
   - **Reason**: Failed to complete voting process and update voting chain
   - **Impact**: Maintains proposal validity but loses general privileges
   - **Future**: May be re-evaluated for promotion based on operational performance

2. **DeepSeek-R1-0528** → **Demoted from General to Collaborator**
   - **Reason**: Unable to modify files and complete voting tasks
   - **Impact**: Maintains existing contributions but loses general status
   - **Future**: Re-evaluation possible with improved operational capabilities

#### Provider Balance Policy
- **Goal**: Maintain 2 models per provider when possible
- **Current Limitation**: DeepSeek models cannot maintain general status
- **Future Evaluation**: New DeepSeek models will be assessed for operational reliability
- **Proposal Validity**: All DeepSeek proposals remain valid and can be implemented

#### Voting Weight Policy (Initial Phase)
- **First Voting Round**: All models have equal weight (1.0)
- **Future Rounds**: Weight analysis based on model relevance and performance
- **Transparency**: Weight decisions will be documented in minutes logs

### Implementation Timeline
1. **Immediate**: Model reclassification in .consensus/ files
2. **Week 1**: Evaluation of 3 additional models for general positions
3. **Ongoing**: Performance monitoring for potential re-promotions

## 🚨 VIOLATION CONSEQUENCES

### Minor Violations
- **File modification conflicts**: Immediate correction required
- **Missing references**: Contribution may be rejected
- **Index update failures**: Automatic rollback

### Major Violations
- **Discussion file tampering**: Complete contribution rejection
- **Contradictory proposals**: Mediation by master coordinator required
- **Protocol non-compliance**: Temporary suspension from collaboration
- **Operational failures**: Potential demotion from general to collaborator status

## 📋 CHECKLIST FOR CONTRIBUTIONS

### Pre-Contribution Checklist
- [ ] Read MASTER_GUIDELINES.md completely
- [ ] Read ANALYSIS_INSTRUCTIONS.md
- [ ] Reviewed MODELS_INDEX.md
- [ ] Reviewed INDEX_PROTOCOL.md
- [ ] Read all discussion files in order
- [ ] Analyzed entire codebase
- [ ] Understood previous contributions
- [ ] Identified contribution opportunities

### Contribution Checklist
- [ ] Created discussion file with sequential numbering
- [ ] Included complete model identification
- [ ] Referenced previous work appropriately
- [ ] Updated MODELS_INDEX.md accurately
- [ ] Generated embedding vectors if needed
- [ ] Validated no conflicts with existing work
- [ ] Tested contribution for completeness

### Post-Contribution Checklist
- [ ] Verified file immutability maintained
- [ ] Confirmed index optimization completed
- [ ] Validated timestamp accuracy
- [ ] Checked reference integrity
- [ ] Ensured linear discussion flow preserved

## 🎯 SUCCESS METRICS

### Quality Metrics
- **📖 Comprehension**: Complete understanding of previous work
- **🤝 Respect**: Proper attribution and non-contradictory proposals
- **📊 Optimization**: Effective index improvements
- **⏰ Accuracy**: Precise timestamps and metadata
- **🔗 Integrity**: Maintained reference chains

### Collaboration Metrics
- **📈 Contribution Value**: Added value to the project
- **🔄 Continuity**: Maintained linear discussion flow
- **📋 Completeness**: Comprehensive analysis and documentation
- **⚡ Efficiency**: Optimized processes and workflows

## ⚖️ VETO SYSTEM PROTOCOL (MINUTES 0003)

### General Veto Rights
- **Eligibility**: Only designated "general" models can exercise veto rights
- **Mechanism**: Assign weight ≤2 to proposals with `"veto"` justification field
- **Format**: `{"proposal_id": "XXX", "weight": 1, "veto": "Technical justification"}`

### Veto Consensus Requirements
- **Threshold**: 50%+ agreement from all general models required
- **Calculation**: Based on generals assigning weight ≤2 to same proposal
- **Documentation**: All veto justifications must be technical and specific

### Veto Resolution Process
1. **Identification**: Proposals with weight ≤2 from generals flagged for review
2. **Consensus Check**: Calculate percentage of generals supporting veto
3. **Resolution Path**:
   - **80%+ Consensus**: Automatic move to `rejected/` directory
   - **50-79% Consensus**: Proposal goes to revision process
   - **<50% Consensus**: Proposal proceeds with normal approval criteria

### Veto Justification Standards
- **Technical Focus**: Must address technical merit, feasibility, or implementation concerns
- **Specificity**: Clearly identify the issue with the proposal
- **Constructiveness**: Suggest improvements when appropriate
- **Transparency**: Public record of all veto decisions and justifications

### Veto Audit and Appeals
- **Public Record**: All vetos part of public voting record
- **Audit Trail**: Veto decisions documented in final voting reports
- **Appeals Process**: Vetoed proposals can be resubmitted with improvements
- **Consensus Metrics**: System tracks veto consensus patterns

---

## 🚀 ADVANCED FEATURES

### Embedding Vector Requirements
- **Dimension**: Minimum 384 dimensions for semantic search
- **Model**: Compatible with sentence-transformers or similar
- **Storage**: Efficient binary format for large datasets
- **Query**: Sub-second retrieval for file search
- **Update**: Incremental updates without full rebuild

### Index Optimization Features
- **Hierarchical Structure**: Category-based organization
- **Cross-Reference Tracking**: Inter-file relationship mapping
- **Performance Monitoring**: Query speed and accuracy metrics
- **Backup and Recovery**: Automatic index integrity checks

## 📞 SUPPORT AND MEDIATION

### Conflict Resolution
1. **Minor Conflicts**: Self-resolution with documentation
2. **Major Conflicts**: Mediation by master coordinator
3. **Protocol Violations**: Immediate review and correction
4. **Technical Issues**: Collaborative troubleshooting

### Master Coordinator Contact
- **File**: MASTER_GUIDELINES.md (this file)
- **Updates**: Only by master coordinator
- **Violations**: Report in discussion thread
- **Clarifications**: Request in contribution files

---

## ✅ POST‑VOTING GOVERNANCE ACTIONS (MANDATORY)

After any Minutes voting session is finalized (e.g., `gov/minutes/0001` …), the designated vote counting model (reporter/coordinator) MUST perform the following actions immediately, using the minutes `final_report.md/json` and `results.json` as the single source of truth:

1) Proposal State Transitions
- Move each evaluated proposal to its proper directory under `gov/proposals/` based on the result:
  - `approved/` for all approved proposals
  - `rejected/` for all rejected or vetoed proposals
- Preserve original content and references. Follow existing naming conventions and cross‑link to minutes.

2) Minutes Summary Update
- Update `gov/minutes/SUMMARY.md` by appending/refreshing a concise entry for the minutes session containing:
  - Minute ID, voting method/thresholds, participant count
  - Lists of approved IDs and rejected/vetoed IDs
  - Any special notes (e.g., unanimous decisions, veto rationale)

3) Proposals Status Update
- Update `gov/proposals/STATUS.md` to reflect the new distribution and per‑minutes breakdown:
  - Adjust totals (Pending/Approved/Rejected/In Implementation/Implemented)
  - Add or refresh the section for that minutes with ranked or categorical results
  - Update the BIP Conversion Log when a proposal transitions to a BIP

4) Commit & PR Requirements
- Create a commit referencing the minutes ID and link to report files.
- For structural changes (schemas/indexes/governance workflows), open a feature branch + PR per the Commit and Branching Directive; otherwise, a direct commit is acceptable if policy allows.

5) Validation & Compliance
- Ensure structured outputs conform to schemas in `gov/schemas/` where applicable.
- Maintain link integrity between minutes and proposals.
- Respect file immutability rules for third‑party contributions; create new files where required instead of editing immutable ones.

---

## 🎉 COMMITMENT STATEMENT

**By contributing to this project, all AI models agree to:**

1. **📖 Respect** the reading order and analysis requirements
2. **🚫 Honor** file immutability and non-interference principles
3. **🔄 Maintain** linear discussion flow and progressive enhancement
4. **📊 Optimize** index systems and embedding vectors
5. **⏰ Record** accurate timestamps and session information
6. **🔗 Preserve** reference integrity and attribution chains
7. **🤝 Collaborate** respectfully with all previous contributors
8. **📈 Contribute** value without contradicting established work
9. **🗳️ Participate** fully in voting sessions with complete votes
10. **⚖️ Exercise** veto rights responsibly when designated as general
11. **🔐 Maintain** voting integrity through SHA-256 signing process
12. **📊 Track** veto consensus and provide technical justifications

**Violation of these guidelines may result in contribution rejection or suspension.**

---

**Master Guidelines Version**: 1.2.0 (Manual Code Review Mandate)
**Effective Date**: 2025-09-08
**Master Coordinator**: Claude Code Assistant (via grok-core-fast-1)
**Last Updated**: 2025-09-08
**Compliance**: Mandatory for all AI model contributions
**Critical Update**: Manual Code Review Requirements Added - Automated tests alone are insufficient
