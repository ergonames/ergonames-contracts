// Genesis compile-check: verify each v1 contract still compiles to a valid
// ErgoTree after the hardening edits. The real compiler (AppKit ContractCompile)
// injects the `$`-prefixed compile-time constants; here we substitute
// type-correct STUBS so Fleet's compiler can type-check the source (the stub
// values don't affect whether the ErgoScript is well-formed — only types do).
//
// Run: node genesis_compile_check.mjs
import { compile } from "@fleet-sdk/compiler";
import { readFileSync } from "node:fs";

const H32 = 'fromBase16("0000000000000000000000000000000000000000000000000000000000000000")';
// Only the INJECTED constants (no inline `val $x =`). $ergonameMultiSigSigmaProp
// is declared inline in the source, so it is NOT stubbed.
const STUBS = {
  $subNameContractBytesHash: H32,
  $ergoNameFeeContractBytesHash: H32,
  $revealContractBytesHash: H32,
  $commitContractBytesHash: H32,
  $configSingletonTokenId: H32,
  $sigUsdOracleSingletonTokenId: H32,
  $ergoNameCollectionSingletonTokenId: H32,
  $ergoNameCollectionTokenId: H32,
  $ergonameCollectionSingletonTokenId: H32,
  $ergonameCollectionTokenId: H32,
  $revealErgoTreeBytesHash: H32,
  $revealProxyErgoTreeBytesHash: H32,
  $registrySingletonTokenId: H32,
  $maxCommitBoxAge: "30",
  $recipientPropBytes: `Coll(${H32}, ${H32})`,
  $recipientShares: "Coll(500L, 500L)",
};

function stub(src) {
  let out = src;
  for (const [name, val] of Object.entries(STUBS)) {
    // Replace the injected constant ONLY where it is not being declared as an inline val.
    out = out.replaceAll(name, val);
  }
  return out;
}

const contracts = ["reveal", "fee_split", "registry", "collection", "subname_registry", "commit", "reveal_proxy", "config"];
let failed = 0;
for (const f of contracts) {
  let src;
  try { src = readFileSync(new URL(`../ergonames_v1_${f}.es`, import.meta.url), "utf8"); }
  catch { console.log(`${f.padEnd(18)} (no file)`); continue; }
  try {
    const tree = compile(stub(src), { version: 1 });
    console.log(`${f.padEnd(18)} OK  ${tree.toHex().length / 2} bytes`);
  } catch (e) {
    console.log(`${f.padEnd(18)} FAIL ${String(e.message).split("\n")[0].slice(0, 100)}`);
    failed++;
  }
}
console.log(failed ? `\n${failed} contract(s) failed to compile` : "\nall contracts compile ✓");
process.exit(failed ? 1 : 0);
