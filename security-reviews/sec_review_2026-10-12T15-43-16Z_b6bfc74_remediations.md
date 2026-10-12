# Remediations of Security Review Findings

Review date and time: 2026-10-12T15:43:16Z (UTC)

## Remediations

### Remediation of Finding 1: The R9 commitment is computed over an ambiguous concatenation, so it does not uniquely fix the reveal box

- [x] This remediation implements the security review's recommendation for this finding.
- [ ] This remediation addresses the finding in a way that differs from the security review's recommendation.

Remediation commits:
- [38cd8ad](https://github.com/ergonames/ergonames-contracts/commit/38cd8ad47dafcd896c5f74d13aabd849c4f7fcde)

Description: Commit 38cd8ad adds length framing to the proxy's R9 commitment preimage — every secret element is prefixed with its 8-byte length (`longToByteArray(secret.size.toLong) ++ secret`), and the element counts (`secrets.size` before the secret bytes, `fees.size` before the fee bytes) are included — so the commitment now uniquely determines the tuple structure and any re-cut of the same byte stream hashes differently. Verified: the previously colliding re-cut variant now produces a different `blake2b256`. The commit also adds explicit element-size checks to the collection's reveal validation (`secrets(0).size > 0`; `secrets(1)`, `secrets(2)`, `secrets(3)` each `== 32`) on top of the `secrets.size == 4` shape assertion, and all 8 contracts compile with `@fleet-sdk/compiler` 0.12.0.

### Remediation of Finding 2: Action 1 does not reject a proxy box holding two or more tokens; those tokens are unconstrained and cannot be returned

- [x] This remediation implements the security review's recommendation for this finding.
- [ ] This remediation addresses the finding in a way that differs from the security review's recommendation.

Remediation commits:
- [f30ae6f](https://github.com/ergonames/ergonames-contracts/commit/f30ae6fbb8e548a4dc20ef54da5b08023dbd6623)

Description: Commit f30ae6f adds `(SELF.tokens.size == 0)` to the action-1 branch, so a proxy box holding any tokens cannot execute a reveal; it can only use the refund path, which returns its tokens. This replaces the payment-token branch that had been removed in the preceding commit (c757ddf) with a hard rejection of token-bearing proxies.

## Non-remediated findings

None.
