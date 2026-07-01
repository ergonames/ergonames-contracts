// Genesis property/satisfiability check (static tier).
//
// Two guards, per contract, that DON'T need the AVL prover or the live
// interpreter (those authoritative tests need the Scala/AppKit toolchain — see
// GENESIS_PROPERTY_TESTS.md):
//   [1] compiles to a valid ErgoTree (stubs the injected $ constants), and
//   [2] register-type consistency — no box register is read at two incompatible
//       types in the source, the exact H4 dead-branch trap (a register holds one
//       typed constant, so a second-typed .get necessarily reverts).
//
// The active launch contracts must be clean. subname_registry is EXPECTED to
// flag (its mint branch is the known-dead H4 code, shipped DISABLED at genesis).
import { readFileSync } from "node:fs";
import { compile } from "@fleet-sdk/compiler";

const H32 = 'fromBase16("0000000000000000000000000000000000000000000000000000000000000000")';
const STUBS = {
  $revealContractBytesHash: H32, $subNameContractBytesHash: H32, $ergoNameFeeContractBytesHash: H32,
  $commitContractBytesHash: H32, $configSingletonTokenId: H32, $sigUsdOracleSingletonTokenId: H32,
  $ergoNameCollectionSingletonTokenId: H32, $ergoNameCollectionTokenId: H32,
  $ergonameCollectionSingletonTokenId: H32, $ergonameCollectionTokenId: H32,
  $revealErgoTreeBytesHash: H32, $revealProxyErgoTreeBytesHash: H32, $registrySingletonTokenId: H32,
  $maxCommitBoxAge: "30", $recipientPropBytes: `Coll(${H32}, ${H32})`, $recipientShares: "Coll(500L, 500L)",
};
const stub = (s) => Object.entries(STUBS).reduce((o, [k, v]) => o.replaceAll(k, v), s);

// Active launch set must be fully clean; subname_registry ships disabled (dead
// H4 mint branch retained, multisig-migratable) so its conflict is expected.
const ACTIVE = ["reveal", "fee_split", "registry", "collection", "commit", "reveal_proxy", "config"];
const DISABLED = ["subname_registry"];

let hardFail = 0;
for (const f of [...ACTIVE, ...DISABLED]) {
  const src = readFileSync(new URL(`../ergonames_v1_${f}.es`, import.meta.url), "utf8");
  const disabled = DISABLED.includes(f);

  // [1] compile
  let compiled = true, bytes = 0;
  try { bytes = compile(stub(src), { version: 1 }).toHex().length / 2; }
  catch (e) { compiled = false; }

  // [2] register-type consistency (H4 guard). Group by RECEIVER + register
  // (SELF.R4, sigUsdOracleBoxIn.R4, …) so reading the SAME box's register at two
  // types (the H4 trap) is flagged, WITHOUT false-positiving legit multi-box
  // reads (oracle R4=Long vs registry R4=AvlTree are different boxes).
  const byReg = {};
  for (const m of src.matchAll(/([A-Za-z_]\w*(?:\(\d+\))?)\.(R[4-9])\[(.+?)\]\.get/g)) {
    const key = `${m[1]}.${m[2]}`;
    const type = m[3].replace(/\s+/g, " ").trim();
    (byReg[key] ??= new Set()).add(type);
  }
  const conflicts = Object.entries(byReg).filter(([, t]) => t.size > 1);

  const ok = compiled && conflicts.length === 0;
  const tag = disabled ? (ok ? "OK (disabled)" : "EXPECTED-CONFLICT (disabled)") : (ok ? "OK" : "FAIL");
  console.log(`${f.padEnd(18)} compile:${compiled ? "✓" : "✗"} types:${conflicts.length === 0 ? "✓" : "✗"}  ${bytes ? bytes + "b" : ""}  ${tag}`);
  for (const [key, types] of conflicts) console.log(`    ${key} read at: ${[...types].join("  |  ")}`);
  if (!disabled && !ok) hardFail++;
}

console.log(hardFail ? `\n${hardFail} ACTIVE contract(s) failed` : "\nactive launch set: compiles + H4-type-consistent ✓");
process.exit(hardFail ? 1 : 0);
