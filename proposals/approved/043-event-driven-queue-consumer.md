# 🤖 43: Event-Driven Queue & Consumer Automation Service (WS/WebSocket)

## BIP Information
**BIP**: N/A (This is an initial proposal for a future BIP)
**Title**: Event-Driven Queue & Consumer Automation Service (WS/WebSocket)  
**Author**: Andre  
**Status**: Draft  
**Type**: Standards Track  
**Category**: Infrastructure  
**Created**: 2025-09-08  
**License**: MIT

## Abstract
Attach a lightweight queue + consumer service to **BIP-00** that launches a local WebSocket stream server on a high port. A **Client** submits typed TASKs; one or more **Consumers** execute them (e.g., Cursor extension), streaming progress, requesting approvals, and handling governance steps (reviews, voting, PRs) with retries, idempotency, and auto-remediation.

## Motivation
Interactive IDE-only flows limit automation, observability, and recovery. A decoupled **client ↔ broker ↔ consumer** pipeline enables background execution, unified logging, standard TASK contracts, and safe approvals for terminal commands—automating the BIP lifecycle end-to-end while preserving the Hive’s governance.

## Rationale
- **Decoupling** unlocks concurrency, backpressure, and retries.  
- **WebSocket** provides bidirectional streaming with low friction and local-first security.  
- **Typed TASKs** encode governance and repo operations explicitly.  
- **Auto-remediator** (small model) can unblock stalls without expensive models.

## Specification
**Roles & Handshake**
- On connect, peers send: `{ role: "client" | "consumer", capabilities: string[], auth?: string }`.
- **Client** (CLI or dashboard) submits TASKs and tails progress.
- **Consumer** executes TASKs (initially the Cursor extension): can switch model, clear memory, start new session, send/receive chat, and optionally request terminal command approvals.

**Transport & Ports**
- Protocol: **WebSocket**; wire format negotiation supports **Protocol Buffers (default)**, **FlatBuffers (optional)**, and **JSON (debug only)**.
- Handshake: first frame MUST include `{ wire: 'protobuf'|'flatbuffers'|'json', schemaVersion: 'v1' }`; if absent, default to `protobuf` with `v1`. Fallback to `json` only when codegen bindings are unavailable.
- Bind: `127.0.0.1` on dynamic **high port** (49152–65535), with fallback; publish to `.hive/port` and env `HIVE_WS_PORT`.
- Optional TLS for remote mode (off by default).

**Message Envelope**
```json
{ "id":"uuid", "type":"TASK|EVENT|APPROVAL|RESULT|ERROR|HEARTBEAT",
  "traceId":"uuid", "ts":"iso8601", "payload":{...} }
```
- `TASK`: `{ kind, args, repo, branch?, idempotencyKey }`
- `EVENT`: progress `{ stage, message, pct? }`
- `APPROVAL`: `{ action, reason, suggestedCommand }`
- `RESULT`: `{ status: "ok"|"fail", artifacts?, summary }`
- `ERROR`: `{ code, message, details }`
- `HEARTBEAT`: liveness ping

#### Wire Format and Schemas
- Logical envelope above is for documentation; production payloads use binary schemas.
- Schemas live under `packages/hive-broker/protocol/{proto,flatbuffers}/v1/`.
- Code generation produces Rust and TypeScript bindings.
- Backward compatibility: additive fields only in minor versions; breaking changes bump `schemaVersion`.

Example Protobuf envelope:
```proto
syntax = "proto3";
package hive.protocol.v1;

message Envelope {
  string id = 1;
  string type = 2; // TASK | EVENT | APPROVAL | RESULT | ERROR | HEARTBEAT
  string traceId = 3;
  string ts = 4; // ISO-8601
  bytes payload = 5; // marshaled message-specific type
}
```

**Queue Semantics**
- FIFO with **priorities**: `critical > core > normal`.  
- **Idempotency** by `idempotencyKey`.  
- **Retries** with exponential backoff; dead-letter on max attempts.  
- **Backpressure** via queue length thresholds.

**TASK Types (initial)**
- **start_bip { bipId }**: bootstrap BIP skeleton (files/dirs).  
- **implement_bip { bipId }**: begin implementation.  
- **continue_impl { bipId }**: resume after failure or incomplete checklist.  
- **verify_bip { bipId }**: validate all checklist items.  
- **create_impl_summary { bipId }**: create `IMPLEMENTATION_SUMMARY.md` if missing.  
- **review_bip { bipId }**: new session, random general; follow governance template; return approve/reject + report via WS.  
- **revise_impl { bipId }**: apply requested changes; produce new report.  
- **review_bip_round { bipId, round }**: second/third review with a new random model/session.  
- **vote_bip { bipId, instructionsPath }**: open the minute directory + `INSTRUCTIONS.md` to cast vote.  
- **tally_votes { bipId }**: count votes; generate voting report.  
- **update_proposals { bipId }**: move proposals to `accepted` / `rejected` / `pending`.  
- **bootstrap_bip_from_proposal { proposalId }**: generate initial BIP docs from an approved proposal.  
- **update_changelog_after_final_review { bipId }**: append entry under `[Unreleased]` with decision, rationale, and links.  
- **generate_final_review_report { bipId }**: create `FINAL_REVIEW_REPORT.md` using the unified template.  
- **sync_bip_status { bipId }**: harmonize status across `gov/bips/BIP-xx/*` docs and READMEs.  
- **update_readme { bipId }**: update project README with key outcomes and deployment notes.  
- **initiate_voting_session { pendingThreshold, initiatorModel?, minuteId? }**: when pending proposals ≥ threshold, select/rotate an initiator model; create a new minute under `gov/minutes/<NNNN>/` with `INSTRUCTIONS.md` and minute JSON compliant with `gov/schemas/minutes_report.schema.json`; seed session metadata and cast the initiator’s first vote.  
- **request_vote { modelName, subjectId, subjectType: 'proposal'|'bip', ballotTemplatePath }**: send a standardized vote request (same template for all models, only `modelName` varies); ensure models receive their explicit `modelName` parameter.  
- **collect_votes { sessionId, expectedVoters[] }**: collect ballots, validate against the template, record outcomes to the minute, and close the session.

**Governance Coupling**
- **Criticity → reviews**:  
  - normal: 2 reviewers;  
  - core: 3 reviewers;  
  - if three reviewers cannot reach harmony or there are many change requests, add **+2** reviewers; if still unresolved, require approval of **all 10 generals**.  
- **Branch discipline**: on implementation start, create branch `bip/<id>/impl/<timestamp>`.  
- Each review round ⇒ **commit**; on final approval ⇒ **push** and **open PR**.  
- **Post-approval actions** (automated TASKs): `generate_final_review_report`, `update_changelog_after_final_review`, `update_readme`, and `sync_bip_status`.  
- **Voting orchestration**: when proposals pending accumulate, rotate initiator selection via the task manager; create minutes per prior sessions and governance rites; use a single standardized ballot template for all models (only `modelName` changes); initiator must cast the first vote and record it in the minute.

**Security & Approvals**
- Server policy controls terminal command approvals: `allow|deny|ask`.  
- Sandboxed execution, path allowlist, secret redaction; destructive ops require explicit approval.  
- Authentication token optional (local mode); required in remote mode.

**Logging & Auto-Remediation**
- Structured logs (`ndjson`) per `traceId`, stored under `.hive/logs/`.  
- On error/stall, trigger `auto_remediate`: a small model (e.g., gpt-5-mini) reads the last logs to propose a fix or safe rollback; attach reasoning to the TASK.

### Implementation Details
- **Language**: Rust (Tokio) or equivalent for broker.  
- **Modules**:  
  - `broker`: WS server, queue, routing, approvals, retries, idempotency store (SQLite/FS).  
  - `protocol`: Protobuf schemas (`.proto`) with optional FlatBuffers (`.fbs`); JSON only for debugging. Includes codegen + validation.
  - `consumer-cursor`: bridge to Cursor chat/session/model control + terminal approvals.  
  - `gitops`: branch/commit/PR helpers via GitHub App.  
  - `governance`: criticity → required reviews, escalation rules.  
  - `voting`: minute creation (`gov/minutes/<NNNN>/`), standardized ballot templating, initiator rotation, vote collection/tally, schema validation.
- **CLI Client**: `hive task submit ...`, `hive tail <traceId>`.  
- **Dashboard (later)**: visualize queue, approvals, logs, artifacts.

### Success Criteria
- [ ] Broker starts on high port, writes `.hive/port`, and handles client/consumer handshakes.  
- [ ] All TASK types implemented with streaming progress, retries, and idempotency.  
- [ ] Terminal approvals policy enforced; logs complete; auto-remediator resolves stalls or proposes rollback.  
- [ ] Final review report generated from `gov/bips/templates/REVIEW_REPORT.md`.  
- [ ] CHANGELOG updated under `[Unreleased]` with BIP decision and links.  
- [ ] README updated with BIP outcomes and security/operations notes.  
- [ ] BIP status synchronized across all related documents.  
- [ ] Wire format negotiation works (protobuf default, json fallback).  
- [ ] `.proto`/`.fbs` schemas committed and pass lint/validate; codegen bindings compile in Rust and TS.  
- [ ] Schema evolution policy documented and enforced via CI compatibility checks.  
- [ ] Voting session initiation creates `gov/minutes/<NNNN>/` with `INSTRUCTIONS.md` and minute JSON valid per `gov/schemas/minutes_report.schema.json`.  
- [ ] Initiator casts and records the first vote; standardized ballot template is used for all vote requests with explicit `modelName`.  
- [ ] Rotation and collection complete within SLA; final tally recorded to the minute.

### Timeline
- **Phase 1**: Broker WS + CLI + TASK skeletons + logging/ids (Week 1–2)  
- **Phase 2**: Cursor consumer + approvals + gitops (Week 3–4)  
- **Phase 3**: Reviews/vote automation, tally, proposals update, auto-remediator (Week 5–6)

## Benefits
- Decoupled automation with strong observability and recovery.  
- Governance-compliant workflows (reviews, votes, PRs) executed headlessly.  
- Local-first, low-friction transport with minimal conflicts.  
- Scales from terminal to dashboard without changing protocol.

## Potential Challenges
- IDE sandbox limitations and approval UX.  
- Concurrent edits on the same BIP (use branch locks/namespacing).  
- Cost drift due to retries (mitigate with budgets/limits).  
- Secret handling in logs (mitigate with redaction and vault).

## Impact Assessment
- **Scope**: system-wide  
- **Complexity**: medium-high  
- **Priority**: high  
- **Estimated Effort**: medium

## Implementation Plan
1. Define protocol + Rust broker skeleton with queue/idempotency/retries.  
2. Build CLI client and Cursor consumer bridge with approval hooks.  
3. Implement TASK handlers (BIP bootstrap → PR) + gitops.  
4. Add auto-remediator; wire metrics and artifacts; enforce policies.  
5. Run bake-off on sample BIPs; tune backpressure/timeouts.

## Next Steps
- Create `packages/hive-broker` and `packages/hive-cli`.  
- Commit `protocol.md` with schemas; ship initial server + CLI.  
- Configure GitHub App credentials; add `.env.example`.  
- Wire BIP-00 extension to auto-start broker and announce port.

## References
1. [Master Guidelines](../../guidelines/MASTER_GUIDELINES.md)  
2. [Unified Review Report Template](../../bips/templates/REVIEW_REPORT.md)  
3. [Keep a Changelog Standard](https://keepachangelog.com/en/1.0.0/)  
4. [Related BIP-00](../../bips/BIP-00/BIP-00.md)  
5. [External Reference](https://example.com)
6. [Protocol Buffers](https://developers.google.com/protocol-buffers)  
7. [FlatBuffers](https://google.github.io/flatbuffers/)  
8. [Buf: Protobuf Linting and Breaking Change Detection](https://buf.build/docs)  
9. [Minutes README](../../minutes/README.md)  
10. [Minutes Report Schema](../../schemas/minutes_report.schema.json)

---

**Proposer**: GPT-5 Thinking  
**Status**: Draft  
**Date**: 2025-09-08

## Schema Compliance
This proposal follows the [Proposal Schema](../schemas/proposal.schema.json) structure guidelines. For JSON-based proposal data (used in reports and automated systems), the schema ensures data consistency and validation.

**Note**: This is a Markdown proposal document. JSON schema validation applies to structured proposal data, not to Markdown files.
