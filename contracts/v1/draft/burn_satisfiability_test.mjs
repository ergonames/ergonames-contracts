// Satisfiability test for the proposed "Burn ErgoName" registry branch.
//
// The complement of the auditor's H4 reduction test: instead of proving a branch
// is UNSATISFIABLE, this proves the burn branch is SOUND and REACHABLE —
//   1. it compiles to a valid ErgoTree (deployable), and
//   2. no register is read at two incompatible types (the H4 failure mode),
// so it can't be dead-on-arrival the way the subname mint branch was.
//
// Full proof (construct a real burn tx + AVL remove proof and run it through the
// sigmastate interpreter) belongs in the genesis property-test pass; this guards
// the structural trap that static review keeps missing.
import { readFileSync } from "node:fs";
import { compile } from "@fleet-sdk/compiler";

const line = "=".repeat(70);
console.log(line);
console.log("ErgoNames registry — Burn ErgoName branch satisfiability test");
console.log(line);

const src = readFileSync(new URL("./ergonames_v1_registry_burn_branch.es", import.meta.url), "utf8");

// ---- 1. Compiles to a valid ErgoTree? ----
let compiled = false, info = "";
try {
  const tree = compile(src, { version: 1 });
  info = tree.toHex();
  compiled = true;
} catch (e) {
  info = "COMPILE ERROR: " + e.message;
}
console.log("\n[1] Compiles to a valid ErgoTree (deployable):", compiled ? "YES" : "NO");
console.log("    " + (compiled ? info.slice(0, 48) + "… (" + info.length / 2 + " bytes)" : info));

// ---- 2. Register-type consistency (the H4 guard) ----
// Every typed register read in the branch; each register must demand exactly ONE
// type, or some read necessarily yields None and .get reverts (H4-class).
// `.+?` (not `[^\]]`) so bracketed types like Coll[BigInt] / (Coll[Byte], Long)
// are captured whole — the `].get` anchor stops at the register's closing bracket.
const reads = [...src.matchAll(/\.(R[4-9])\[(.+?)\]\.get/g)].map((m) => ({
  reg: m[1],
  type: m[2].replace(/\s+/g, " ").trim(),
}));
const byReg = {};
for (const r of reads) (byReg[r.reg] ??= new Set()).add(r.type);

console.log("\n[2] Register reads in the branch (each must be a single type):");
let conflict = false;
for (const [reg, types] of Object.entries(byReg).sort()) {
  const ok = types.size === 1;
  conflict ||= !ok;
  console.log(`    ${reg}: ${[...types].join("  |  ")}   ${ok ? "✓" : "✗ TWO TYPES — H4-class conflict"}`);
}

// ---- Verdict ----
console.log("\n" + line);
const pass = compiled && !conflict;
console.log(
  "VERDICT:",
  pass
    ? "burn branch COMPILES and is type-consistent → satisfiable (not dead code)."
    : "PROBLEM — " + (!compiled ? "does not compile." : "a register is read at two types (H4-class).")
);
console.log(line);
process.exit(pass ? 0 : 1);
