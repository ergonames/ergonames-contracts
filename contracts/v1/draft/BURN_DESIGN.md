# DRAFT — Burn ErgoName (registry delete) branch

**Status:** proposal for the genesis bundle. Not deployed; not on the current chain.
**For review by:** Luca (registry author).
**Goal:** let a name's owner permanently delete it — remove its entry from the
registry AVL tree **and** burn the name NFT, freeing the name to be registered again.

The current `registry.es` has only a mint (insert) branch + the multisig escape
hatch — no removal path. This adds an owner-authorised **Burn ErgoName** branch,
mirroring the proven subname **self-burn (action 2)** in `subname_registry.es`.

Files:
- `ergonames_v1_registry_burn_branch.es` — faithful, compilable repro of the branch.
- `burn_satisfiability_test.mjs` — proves it compiles + is type-consistent (no H4-class dead-branch).

---

## Transaction shape

```
Burn ErgoName
  Inputs:   INPUTS(0) = Registry (SELF)
            INPUTS(1) = ErgoName NFT box (owner-signed P2PK)
  Outputs:  OUTPUTS(0) = Registry'  (recreated, name removed from the AVL)
            OUTPUTS(1) = owner refund (NFT box value − fee, owner's concern)
            OUTPUTS(2) = miner fee
            → the ErgoName token appears in NO output (burned)
  Context:  getVar(0)=ErgoNameHash, getVar(1)=LookUpProof,
            getVar(2)=RemoveProof,  getVar(3)=burn action byte (== 1)
```

## What the branch enforces (see the `.es`)
- **`validErgoName`** — `previousRegistry.get(ergoNameHash, lookupProof)` equals the
  token id held in `INPUTS(1)`. Binds the removed name to the burned token, so you
  can't remove name B while burning token A.
- **`validRemoval`** — `previousRegistry.remove(ergoNameHash, removeProof)` digest ==
  `OUTPUTS(0).R4` digest. The AVL entry is gone.
- **`validBurn`** — the token id is absent from every output. Destroyed, not moved.
- **`validSelfRecreation`** — registry recreated unchanged except its (smaller) tree;
  the cumulative state counter (R5), age threshold (R6), and price map (R7) are preserved.

**Authorisation is implicit and sufficient:** spending `INPUTS(1)` requires the
owner's signature, so only the holder can burn. No `&& userPK` is needed in the
script (same model as the subname self-burn).

## Two integration points in `registry.es`

**(a)** Alongside `isMintShaped`, add a burn gate. The `if/else` extraction is
deliberate — `allOf` does NOT short-circuit, so a bare `getVar[Byte](3).get` would
throw on non-burn txs and break the escape hatch (an H4-class trap):

```scala
val burnAction: Int = if (getVar[Byte](3).isDefined) getVar[Byte](3).get.toInt else -1
val burnRequested: Boolean = allOf(Coll(
    !isMintShaped,
    (burnAction == 1),
    getVar[Coll[Byte]](0).isDefined,   // ErgoNameHash
    getVar[Coll[Byte]](1).isDefined,   // LookUpProof
    getVar[Coll[Byte]](2).isDefined    // RemoveProof
))
```

**(b)** Insert a branch between the mint `if` and the final `else`:

```scala
if (isMintShaped) {
    ... existing mint ...
    sigmaProp(validMintErgoNameTx) || $ergonameMultiSigSigmaProp   // (H3 fix removes the || here, separately)
} else if (burnRequested) {
    ... validBurnErgoNameTx (see the .es) ...
    sigmaProp(validBurnErgoNameTx)        // H3-correct: NO || multisig
} else {
    $ergonameMultiSigSigmaProp            // migration escape hatch, unchanged
}
```

Gating rationale: a mint is `isMintShaped` (≠ burn); a migration sets no burn vars
(falls to `else`); only a deliberately burn-shaped tx hits the branch, and it has
**no `|| multisig`** so a governance key can't satisfy it without `validBurn` (the
H3 lesson). A malformed burn simply fails — the operator never sets the burn vars
for a migration, so the multisig path stays reachable for everything else.

## Genesis prerequisite — AVL config (interacts with H2)
The registry AVL tree must be created at genesis with **`removeAllowed = true`**
(today it is insert-only). H2's fix pins the *full* tree config (flags + keyLength)
on every recreation — so the burn branch and H2 must be designed together: the
pinned config now includes `insertAllowed = true, removeAllowed = true`.

## Off-chain follow-ups (after the branch is approved)
1. **Bot** — generate the `lookupProof` (tokenId for the name) + `removeProof`
   (AVL delete) for the burn tx, exactly as it generates the insert proof at
   register. New endpoint, e.g. `POST /burn-prepare {name, userAddress}`.
2. **Frontend** — a "Burn / delete this name" control on the name's Records card,
   behind a hard, explicit confirmation: **irreversible, and the name becomes free
   for anyone to register again.**
3. **Indexer** — on detecting the burn (registry tx that removes the key), drop the
   `registrations` row so resolution returns "available" cleanly (not a tombstone).

## Notes / risks
- **Irreversible + releases the name.** A burned name is immediately re-registrable
  by anyone. That's the owner's deliberate choice; the UI must say so unmistakably.
- Compatible with "lifetime ownership" — the owner is *voluntarily* ending ownership.
- **Surface:** this adds one branch to the most security-critical contract right as
  we're keeping the launch set lean. It's far simpler than subnames (self-contained,
  owner-authorised, fails closed, proven template), so the added risk is modest — but
  it's a launch-scope call: include at genesis, or hold for the first post-launch genesis.
- **Testing:** the satisfiability test guards the dead-branch trap. A full sigmastate
  property test — construct a real burn tx with a populated AVL tree + valid
  remove/lookup proofs and assert it validates (and that a wrong-token / wrong-owner
  burn fails) — belongs in the genesis property-test pass.
