// Runs every Doc 164 P4 ground-truth proof and prints a pass/fail digest.
// All P4 proofs run FLAG-FREE on Node v24.14.1 (no --experimental flags).
// Mirrors p3-run-all.mjs.  These prove the P4a browser-pipeline skeleton
// (frame_out ring buffer, key edges, now_ms timing, the full step() loop, and
// the on-disk Pages bundle) in pure hand-wasm + JS, BEFORE any elisp codegen.
import { spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import path from 'node:path';

const here = path.dirname(fileURLToPath(import.meta.url));
const proofs = [
  'p4-01-frameout-ringbuffer.mjs',
  'p4-02-frameout-per-op.mjs',
  'p4-03-key-input.mjs',
  'p4-04-now-ms.mjs',
  'p4-05-skeleton-loop.mjs',
  'p4-06-www-bundle.mjs',
];
let allOk = true;
for (const file of proofs) {
  const r = spawnSync(process.execPath, [path.join(here, file)], { encoding: 'utf8' });
  const out = (r.stdout || '').trim();
  const ok = r.status === 0 && /result:\s*OK/.test(out) && !/FAIL|NaN|error:/i.test(out);
  allOk = allOk && ok;
  console.log(`\n### ${file}  -> ${ok ? 'OK' : 'CHECK'}`);
  console.log(out);
  if (r.stderr && !/ExperimentalWarning/.test(r.stderr)) process.stderr.write(r.stderr);
}
console.log(`\n=== ${allOk ? 'ALL P4 PROOFS OK' : 'SOME P4 PROOFS NEED REVIEW'} ===`);
process.exit(allOk ? 0 : 1);
