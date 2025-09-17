# 📋 Proposals Directory Structure

## Overview
This directory organizes all proposals in the CMMV-Hive project according to their current status and relationship to BIPs (Bitcoin Improvement Proposals).

## 📁 Directory Structure

```
proposals/
├── README.md              # This file
├── pending/               # Proposals awaiting review
├── approved/              # Approved proposals (not yet BIPs)
├── rejected/              # Rejected proposals
├── in-implementation/     # Proposals that became BIPs and are being implemented
├── implemented/           # Proposals that became BIPs and are fully implemented
├── STATUS.md              # Current status summary
└── TEMPLATE.md            # Proposal template
```

## 🔄 Proposal Lifecycle

### Flow from Proposal to Implementation

```
Proposal Creation → Review → Decision
     ↓              ↓         ↓
   pending/    → approved/ → BIP Creation
                    ↓             ↓
                rejected/    in-implementation/
                                 ↓
                            implemented/
```

### Status Categories

#### 📝 **pending/**
- New proposals awaiting review
- Under evaluation by the governance process

#### ✅ **approved/** 
- Proposals approved by voting but not yet converted to BIPs
- Waiting for implementation assignment

#### ❌ **rejected/**
- Proposals that did not pass the voting threshold
- Archived for reference

#### 🔄 **in-implementation/**
- Proposals that became BIPs and are currently being implemented
- **File Format**: `BIP-{ID}-{PROPOSAL_ID}-{TITLE}.md`
- Example: `BIP-00-001-cursor-ide-extension.md`

#### 🟢 **implemented/**
- Proposals that became BIPs and are fully implemented
- **File Format**: `BIP-{ID}-{PROPOSAL_ID}-{TITLE}.md`
- Example: `BIP-01-012-bip-automated-voting-system-proposal.md`

## 📋 Current BIP Mapping

### Implemented BIPs
- **BIP-01** (P012): `implemented/BIP-01-012-bip-automated-voting-system-proposal.md`
- **BIP-02** (P037): `implemented/BIP-02-037-typescript-standardization-proposal.md`
- **BIP-03** (P021): `implemented/BIP-03-021-ai-model-resilience-framework.md`
- **BIP-04** (P024): `implemented/BIP-04-024-voting-chain-integrity-verification.md`

### In Implementation
- **BIP-00** (P001): `in-implementation/BIP-00-001-cursor-ide-extension.md`

## 🔧 File Naming Convention

When a proposal becomes a BIP and moves to implementation directories:

**Format**: `BIP-{BIP_ID}-{PROPOSAL_ID}-{TITLE}.md`

**Components**:
- `BIP_ID`: The BIP number (00, 01, 02, etc.)
- `PROPOSAL_ID`: Original proposal ID (001, 012, 037, etc.)
- `TITLE`: Descriptive title in kebab-case

**Examples**:
- `BIP-01-012-bip-automated-voting-system-proposal.md`
- `BIP-02-037-typescript-standardization-proposal.md`
- `BIP-00-001-cursor-ide-extension.md`

## 🎯 Governance Integration

### BIP Creation Process
1. Proposal approved in voting
2. Until implementation starts, the proposal remains in `approved/` and the `BIP` field MUST be set to `N/A (This is an initial proposal for a future BIP)`
3. When implementation begins, the proposal is moved to `in-implementation/` and receives a BIP ID; rename the file to `BIP-{BIP_ID}-{PROPOSAL_ID}-{TITLE}.md`
4. BIP development begins following BIP-01 process
5. Upon completion, moved to `implemented/`

### Status Tracking
- All movements tracked in `STATUS.md`
- Integration with BIP system for progress monitoring
- Cross-references maintained between proposals and BIPs

## 📊 Statistics

For the most up-to-date figures, see `STATUS.md`.

### Current Distribution (2025-09-15)
- **Pending**: 0 proposals
- **Approved**: 47 proposals (awaiting BIP conversion)
- **Rejected**: 6 proposals
- **In Implementation**: 1 BIP (BIP-00)
- **Implemented**: 4 BIPs (BIP-01, BIP-02, BIP-03, BIP-04)

### Summary (Minutes 0004)
- **Total Proposals Evaluated**: 19
- **Approved**: 19 (100%)
- **Rejected**: 0 (0%)
- **Average Consensus**: 78.0%

### Manual Prioritization Note
To accelerate automation of inter-model communication, a user directive applies a scheduling boost of **+50** points (does not alter official minutes scores) to the following proposals. See `STATUS.md` for the prioritized Implementation Order:

- **P054** - Universal Matrix Protocol — (+50)
- **P048** - Real-time Collaboration Infrastructure — (+50)
- **P050** - Bidirectional Feedback System — (+50)
- **P043** - Event-driven Architecture — (+50)

## 🔍 Search and Reference

### Finding Proposals
- Use `git log` to track proposal history
- Cross-reference with BIP directories
- Check `STATUS.md` for current states

### Documentation Standards
- All files must be in English [[memory:8373468]]
- Follow project naming conventions
- Maintain audit trail through file movements

---

*This structure ensures clear tracking from proposal creation through BIP implementation and completion.*
