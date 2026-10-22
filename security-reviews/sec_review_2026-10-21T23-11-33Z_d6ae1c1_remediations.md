# Remediations of Security Review Findings

Review date and time: 2026-10-21T23:11:33Z (UTC)

## Remediations

### Remediation of Finding 1: Update transaction can rewrite the target contract's state registers

- [ ] This remediation implements the security review's recommendation for this finding.
- [x] This remediation addresses the finding in a way that differs from the security review's recommendation.

Remediation commits:
- [4d02bf8](https://github.com/ergonames/ergonames-contracts/commit/4d02bf80193031f4b23d35ff305f3c8765526fd8)

Description: Commit 4d02bf8 replaces the single generic update contract with box-specific collection and registry update contracts. The registry update contract now requires `contractToUpdateBoxIn` and `contractToUpdateBoxOut` to match on value, tokens, R4 (the AVL tree), R5 (the state pair), and R6 (the age thresholds); the collection update contract has no registers to pin. R7 (the price map) is deliberately left mutable so prices can be changed in future updates, and the author accepts that R7 remains unbound to the vote commitment. This differs from the review's recommendation to pin R4–R7 or commit the full new box to the vote; the residual front-running risk on R7 is accepted.

### Remediation of Finding 2: The update contract does not compile

- [x] This remediation implements the security review's recommendation for this finding.
- [ ] This remediation addresses the finding in a way that differs from the security review's recommendation.

Remediation commits:
- [4d02bf8](https://github.com/ergonames/ergonames-contracts/commit/4d02bf80193031f4b23d35ff305f3c8765526fd8)
- [e721c32](https://github.com/ergonames/ergonames-contracts/commit/e721c32e24db595c9c706d5d2c54e1ce7f7fa9c3)

Description: Commit 4d02bf8 renamed `isValidBallot` to `isValidVote` and `ballotBoxes` to `voteBoxes` while splitting the update contract, but the new registry update and vote contracts introduced two compile errors. Commit e721c32 adds the missing `.get` on `contractToUpdateBoxOut.R4[AvlTree]` and splits the vote contract's `||` into the `isCollectionUpdate` and `isRegistryUpdate` booleans. With the `$` constants stubbed, `ergoname_v1_update_collection.es`, `ergoname_v1_update_registry.es`, and `ergoname_v1_vote.es` now compile with `@fleet-sdk/compiler` 0.12.0. No CI compile check or hash regeneration was observed.

## Non-remediated findings

None.

## Comments

Author's note on the new dual-NFT ballot path: the same vote contract now accepts either the collection or the registry update NFT. The author considers cross-update vote clearing a non-issue because a vote box's R6 must carry the hash of the contract being updated, so a vote for a different update cannot satisfy the update contract.
