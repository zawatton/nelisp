// Proof 1 — STANDARDIZED wasm EH: try_table (0x1f) + throw (0x08) + Tag(13).
// KEY V8 FINDING: on Node v24.14.1 these opcodes need the runtime flag
//   --experimental-wasm-exnref
// (legacy try/catch works flag-free; see proof-02).  Run this file WITH:
//   node --experimental-wasm-exnref tools/wasm-proofs/proof-01-eh-trytable.mjs
// Establishes: tag section placement, try_table block-type, catch-clause
// encoding (0x00 tag label), and the catch-label branch depth (=1: depth 0 is
// the try_table's own frame, depth 1 is the enclosing block handler target).
import { section, functype, module, op, uleb, vec, CATCH, I64, funcBody, validateAndRun } from './wasm-build.mjs';

function nameExport(str, idx) {
  const b = [...Buffer.from(str, 'utf8')];
  return [...uleb(b.length), ...b, 0x00, ...uleb(idx)];
}
const typeSec = section(1, vec([functype([], [I64]), functype([I64], [])]));
const funcSec = section(3, vec([uleb(0)]));
const tagSec = section(13, vec([[0x00, ...uleb(1)]]));
const expSec = section(7, [...uleb(1), ...nameExport('run', 0)]);

function bodyFor(label) {
  const expr = [
    ...op.block(I64),                        // $h (result i64)
    ...op.try_table(I64, [CATCH(0, label)]), // try_table (result i64) (catch tag0 -> label)
    ...op.i64Const(42),
    ...op.throw_(0),
    ...op.end,                               // end try_table
    ...op.end,                               // end $h  -> payload i64 is result
    ...op.end,                               // end func
  ];
  return funcBody([], expr);
}

async function run(tagPlacement, label) {
  const codeSec = section(10, vec([bodyFor(label)]));
  let secs;
  if (tagPlacement === 'after-func') secs = [typeSec, funcSec, tagSec, expSec, codeSec];
  else if (tagPlacement === 'after-code') secs = [typeSec, funcSec, expSec, codeSec, tagSec];
  const r = await validateAndRun(module(secs), 'run', []);
  return { tagPlacement, label, valid: r.valid, result: r.result?.toString?.(), error: r.error?.message };
}
for (const placement of ['after-func', 'after-code']) {
  for (const label of [0, 1]) {
    console.log(JSON.stringify(await run(placement, label)));
  }
}
