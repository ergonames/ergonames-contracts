# Remediations of Security Review Findings

Review date and time: 2026-10-12T01:59:26Z (UTC)

## Remediations

### Remediation of Finding 2: Collection token order is mutable and permanently assumed, which can disable all reveal refunds

- [x] This remediation implements the security review's recommendation for this finding.
- [ ] This remediation addresses the finding in a way that differs from the security review's recommendation.

Remediation commits:
- [1cf79b5](https://github.com/ergonames/ergonames-contracts/commit/1cf79b54f3033311327cd998b947db1d70c5f54c)

Description: Commit 1cf79b5 pins the collection box's token layout in the reveal-creation path (action 1): the re-created collection box must keep `tokens(0)` identical to the input box and must carry the collection token with amount - 1 at `tokens(1)`. This closes the vector where any reveal creator could flip the collection output's token order (or prepend a junk token at index 0) and thereby make every outstanding reveal refund fail at `INPUTS(0).tokens(0) == singleton` and the action-2 refund check unsatisfiable. The index-based refund checks are unchanged, but they now rest on an invariant that is enforced at creation. Note: the collection contract still fails to compile at HEAD due to the unrelated dangling references described under Finding 1.

## Non-remediated findings

### Finding 1: HEAD does not compile — three of eight contracts contain four compile-breaking errors

This finding was not remediated because the four flagged errors were fixed in 1cf79b5, but removing the subname-registry functionality left dangling references in four contracts (registry, reveal, collection, reveal_proxy), so the revision still does not compile; follow-up commits will finish the fixes.
