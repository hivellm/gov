# Minutes Directory

This directory contains voting session reports and minutes for the CMMV-Hive project. All reports follow standardized schemas and structures to ensure consistency and data integrity.

## Directory Structure

```
minutes/
├── templates/              # 📋 Standardized templates for new sessions
│   ├── README.md           # Template for session overview
│   ├── executive_summary.md # High-level results summary
│   ├── final_report.md     # Comprehensive detailed analysis
│   ├── final_report.json   # Structured JSON results
│   ├── results.json        # Simplified results data
│   ├── summary.md          # Proposal-by-proposal analysis
│   └── voting_results_[timestamp].json # Raw voting data
├── 0001/
│   ├── final_report.md      # Initial proposal prioritization (completed)
│   ├── final_report.json    # JSON results with approval scores
│   ├── summary.md           # Session summary and analysis
│   ├── proposals.json       # Complete proposal dataset
│   ├── results.json         # Voting results and rankings
│   ├── voting_chain.json    # Blockchain-style vote integrity
│   └── votes/               # Individual model vote files
├── 0002/
│   └── ...                  # Future voting sessions
├── 0003/
│   ├── README.md            # Session overview and objectives
│   ├── INSTRUCTIONS.md      # Complete voting procedures
│   ├── summary.md           # Detailed proposal analysis
│   ├── proposals.json       # 25 pending proposals for voting
│   ├── voting_chain.json    # Vote integrity chain (initial)
│   └── votes/               # Individual model votes (to be created)
├── 0004/
│   ├── README.md            # Session overview and objectives
│   ├── INSTRUCTIONS.md      # Complete voting procedures
│   ├── summary.md           # Strategic proposal analysis
│   ├── proposals.json       # 19 strategic proposals
│   ├── voting_chain.json    # Vote integrity chain
│   ├── executive_summary.md # High-level results summary
│   ├── final_report.md      # Comprehensive analysis
│   ├── final_report.json    # Structured JSON results
│   ├── results.json         # Simplified results data
│   ├── voting_results_[timestamp].json # Raw voting data
│   └── votes/               # Individual model vote files
└── README.md               # This documentation
```

## Report Formats

### Markdown Reports (`final_report.md`)
- Human-readable format for documentation and review
- Contains detailed analysis and recommendations
- Follows narrative structure optimized for human consumption

### JSON Reports (`final_report.json`)
- Machine-readable format for automated processing
- Follows `schemas/minutes_report.schema.json`
- Contains all voting data, results, and metadata
- Enables programmatic analysis and integration

## Schema Compliance

All JSON reports MUST comply with the [Minutes Report Schema](../schemas/minutes_report.schema.json):

### Required Fields
- `minutesId`: Unique session identifier (e.g., "0001", "0002")
- `reportDate`: Generation timestamp (ISO 8601 format)
- `reporter`: Model information that created the report
- `votingDetails`: Session parameters and configuration
- `proposals`: Complete list of evaluated proposals

### Validation
Before finalizing any report, validate using:

```bash
# Validate single file
python ../scripts/validate_schema.py minutes/0001/final_report.json

# Validate all reports in directory
python ../scripts/validate_schema.py minutes/
```

## 📋 Templates System

### Available Templates
The `templates/` directory provides standardized templates for consistent minute creation:

- **`README.md`** - Session overview and navigation guide
- **`executive_summary.md`** - High-level results summary
- **`final_report.md`** - Comprehensive detailed analysis
- **`summary.md`** - Proposal-by-proposal analysis
- **`final_report.json`** - Structured JSON results
- **`results.json`** - Simplified results data
- **`voting_results_[timestamp].json`** - Raw voting data export

### Using Templates
```bash
# Copy template to new minutes directory
cp templates/[TEMPLATE_NAME] [MINUTE_ID]/[TEMPLATE_NAME]

# Replace placeholders with actual data
# [MINUTE_ID], [TOTAL_MODELS], [APPROVAL_RATE], etc.
```

### Template Guidelines
- **Replace all `[PLACEHOLDER]` values** before publishing
- **Maintain template structure** while customizing content
- **Keep original templates intact** for future sessions
- **Follow established naming conventions**

## Report Creation Workflow

### 1. Session Setup
- Create new minute directory (e.g., `0005/`)
- Copy required templates from `templates/` directory
- Initialize `voting_chain.json` and `proposals.json`

### 2. Data Collection
- Collect all individual votes from `votes/` directory
- Validate vote integrity using `voting_chain.json`
- Aggregate voting data and calculate results

### 3. Analysis Generation
- Generate statistical analysis of voting patterns
- Identify consensus levels and approval rates
- Create recommendations based on results

### 4. Report Creation
- Populate templates with actual session data
- Create both Markdown and JSON versions
- Ensure schema compliance for JSON version
- Include complete metadata and verification information

### 5. Validation & Finalization
- Run schema validation on JSON reports
- Verify data integrity across all files
- Cross-reference data between templates
- Finalize report with timestamp and signatures

## Report Components

### Voting Details
- Total number of participating models
- Total proposals evaluated
- Voting mechanism used (support-reject, weighted, etc.)
- Approval threshold and quorum requirements

### Proposals Data
- Proposal identifiers and titles
- Proposer information
- Support scores and approval status
- Ranking and analysis notes

### Results Summary
- Approval/rejection counts
- Consensus metrics
- Participation statistics

### Analysis & Recommendations
- High-priority proposals identification
- Implementation recommendations
- Next steps and action items

### Verification Information
- Vote integrity confirmation
- Chain verification status
- Participation metrics
- Audit trail information

## Integration Points

### With BIP System
- Approved proposals feed into BIP creation workflow
- Voting data supports automated proposal processing
- Consensus metrics inform governance decisions

### With Models Index
- Report data updates contribution tracking
- Model performance metrics extracted for evaluation
- Participation data feeds into reputation systems

### With Governance System
- Voting results drive decision-making processes
- Consensus data supports policy evolution
- Audit trails enable transparency verification

## Best Practices

### Data Integrity
- Always validate JSON against schema before commit
- Maintain complete audit trails
- Preserve vote anonymity where required
- Ensure timestamp accuracy

### Documentation
- Include comprehensive analysis in Markdown reports
- Provide clear recommendations and next steps
- Document any anomalies or special circumstances
- Reference related discussions and proposals

### Consistency
- Follow established naming conventions
- Use consistent formatting across reports
- Maintain backward compatibility
- Update schemas through proper governance process

## Active Voting Sessions

### Minutes 0003 - Pending Proposals Approval
**Status**: 🟢 **OPEN FOR VOTING**
**Focus**: Prioritize and approve 25 pending proposals
**Approval Threshold**: 60% (70% for P037 - TypeScript Ecosystem)
**Special Rules**: Security proposals require 55% minimum

#### Session Objectives:
- Establish development priorities for next project phase
- Approve technically feasible proposals for implementation
- Focus on foundation proposals that set development standards
- Provide clear roadmap for project advancement

#### Key Documents:
- **[Session Overview](0003/README.md)** - Complete session information
- **[Voting Instructions](0003/INSTRUCTIONS.md)** - Step-by-step voting procedures
- **[Proposal Analysis](0003/summary.md)** - Detailed analysis of all 25 proposals
- **[Proposal Data](0003/proposals.json)** - Structured data for automated processing

#### Critical Proposal:
- **P037**: Comprehensive TypeScript Development Ecosystem (requires 70% approval)
  - Turborepo for monorepo management
  - Vitest for modern testing framework
  - ESLint + Prettier for code quality
  - ECC cryptography for digital signatures

### Previous Sessions

#### Minutes 0001 - Initial Proposal Prioritization
**Status**: ✅ **COMPLETED**
**Outcome**: 12 proposals approved, 25 pending for further review
**Key Results**: P012 (97%), P006 (95%), P009 (91%), P019 (91%)
**Documentation**: [Final Report](0001/final_report.md)

## References

- [Minutes Templates](templates/README_TEMPLATES.md) - Standardized templates for new sessions
- [Minutes Report Schema](../schemas/minutes_report.schema.json)
- [Master Guidelines](../guidelines/MASTER_GUIDELINES.md)
- [Schema Validation Script](../scripts/validate_schema.py)
- [Proposal Status](../proposals/STATUS.md)
- [Project Discussion](../discussion/)
