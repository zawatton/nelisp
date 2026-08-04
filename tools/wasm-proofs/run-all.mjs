// Runs every Doc 164 P2 ground-truth proof and prints a pass/fail digest.
// The try_table proof (proof-01) needs `--experimental-wasm-exnref`; this
// runner spawns it with that flag.  All others run flag-free.
import { spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import path from 'node:path';

const here = path.dirname(fileURLToPath(import.meta.url));
const proofs = [
  ['proof-01-eh-trytable.mjs', ['--experimental-wasm-exnref']],
  ['proof-01b-tag-order.mjs', []],
  ['proof-02-eh-legacy.mjs', []],
  ['proof-03-unwind.mjs', []],
  ['proof-04-import-env.mjs', []],
  ['proof-05-wasi-fdwrite.mjs', []],
  ['proof-06-wasi-clock.mjs', []],
  ['proof-07-wasi-file.mjs', []],
  ['proof-08-wasi-start-exit.mjs', []],
  ['proof-09-wasi-random.mjs', []],
];
let allOk = true;
for (const [file, flags] of proofs) {
  const r = spawnSync(process.execPath, [...flags, path.join(here, file)], { encoding: 'utf8' });
  const out = (r.stdout || '').trim();
  const ok = r.status === 0 && !/undefined|NaN|error:/i.test(out);
  allOk = allOk && ok;
  console.log(`\n### ${file} ${flags.join(' ')}  -> ${ok ? 'OK' : 'CHECK'}`);
  console.log(out);
  if (r.stderr && !/ExperimentalWarning/.test(r.stderr)) process.stderr.write(r.stderr);
}
console.log(`\n=== ${allOk ? 'ALL PROOFS OK' : 'SOME PROOFS NEED REVIEW'} ===`);
process.exit(allOk ? 0 : 1);
