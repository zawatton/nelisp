// Runs every Doc 164 P3 ground-truth proof and prints a pass/fail digest.
// All P3 proofs run FLAG-FREE on Node v24.14.1 (unlike proof-01's try_table,
// nothing here needs --experimental-wasm-exnref).  Mirrors run-all.mjs.
import { spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import path from 'node:path';

const here = path.dirname(fileURLToPath(import.meta.url));
const proofs = [
  'p3-01-data-segments.mjs',
  'p3-02-memory-grow.mjs',
  'p3-03-mutable-global.mjs',
  'p3-04-module-size.mjs',
  'p3-05-bump-alloc.mjs',
  'p3-06-baked-boot.mjs',
];
let allOk = true;
for (const file of proofs) {
  const r = spawnSync(process.execPath, [path.join(here, file)], { encoding: 'utf8' });
  const out = (r.stdout || '').trim();
  const ok = r.status === 0 && /result:\s*OK/.test(out) && !/FAIL|undefined|NaN|error:/i.test(out);
  allOk = allOk && ok;
  console.log(`\n### ${file}  -> ${ok ? 'OK' : 'CHECK'}`);
  console.log(out);
  if (r.stderr && !/ExperimentalWarning/.test(r.stderr)) process.stderr.write(r.stderr);
}
console.log(`\n=== ${allOk ? 'ALL P3 PROOFS OK' : 'SOME P3 PROOFS NEED REVIEW'} ===`);
process.exit(allOk ? 0 : 1);
