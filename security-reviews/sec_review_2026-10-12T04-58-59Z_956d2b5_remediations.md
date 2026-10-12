# Remediations of Security Review Findings

Review date and time: 2026-10-12T04:58:59Z (UTC)

## Remediations

### Remediation of Finding 1: HEAD still does not compile — three of eight contracts fail

- [x] This remediation implements the security review's recommendation for this finding.
- [ ] This remediation addresses the finding in a way that differs from the security review's recommendation.

Remediation commits:
- [10103bd](https://github.com/ergonames/ergonames-contracts/commit/10103bd6f527c3d63bbdf85d020c19b19816b15d)

Description: Commit 10103bd fixes all three compile errors and cleans up the dead check: the reveal contract's issuance-box binding was renamed to `ergoNameIssuanceBoxOut` (declaration and uses made consistent) and the unused, unreferenced `validValueConservation` definition was deleted; the collection refund path's `ergonameCollectionTokenAmount` was renamed to `collectionTokenAmount`; and the missing comma was added in the reveal proxy while keeping the `revealBoxOut.tokens.size == 1` guard. All 8 contracts compile with `@fleet-sdk/compiler` 0.12.0 after this commit. Note: the recommendation's request to re-add a genesis compile check to CI was not part of this commit.

### Remediation of Finding 2: The name-token supply cap still requires two units, but the second unit's destination is no longer constrained

- [x] This remediation implements the security review's recommendation for this finding.
- [ ] This remediation addresses the finding in a way that differs from the security review's recommendation.

Remediation commits:
- [10103bd](https://github.com/ergonames/ergonames-contracts/commit/10103bd6f527c3d63bbdf85d020c19b19816b15d)

Description: Commit 10103bd changes `validTokenLimit`'s total-supply requirement from `outputs_sum == 2L` to `outputs_sum == 1L`, so the mint emits a single unit of the name token in the issuance box — exactly the cap the review recommended for the subname-less launch.

### Remediation of Finding 3: The issuance box is no longer bound to the user's pre-commitment, so the mint executor controls the artwork metadata

- [x] This remediation implements the security review's recommendation for this finding.
- [ ] This remediation addresses the finding in a way that differs from the security review's recommendation.

Remediation commits:
- [10103bd](https://github.com/ergonames/ergonames-contracts/commit/10103bd6f527c3d63bbdf85d020c19b19816b15d)

Description: Commit 10103bd takes the fallback option offered in the review's recommendation: the reveal data gains a fourth element, `ImageDataHash` (R9 shape comment updated accordingly), and the registry's `validErgoNameMint` now checks `blake2b256(issuanceBoxOut.R8 ++ issuanceBoxOut.R9) == imageDataHash`. This pins the artwork registers — the 32-byte picture hash and the image data URI — to the user's committed hash, rather than restoring the hash over the whole issuance box as the primary recommendation suggested.

## Non-remediated findings

None.
