// Proof 4-06 — the proof-fixture browser bundle is self-consistent and the
// ON-DISK dtw.wasm runs.
// (1) rebuild dtw.wasm, write it, re-read the bytes FROM DISK (what GitHub Pages
//     serves), validate + instantiate them, drive init()+step() with stub imports
//     and confirm frame_out fired with a full frame -> the served binary is live.
// (2) assert index.html <-> dtw.js <-> dtw.wasm wiring + the import/export contract
//     names match the ABI (frame_out/key_state/now_ms ; memory/init/step).
// This covers the proof fixture under tools/wasm-proofs/p4-www only.  The
// deployable P4c bundle now lives under site/dtw and is built/smoked separately.
import { readFileSync, writeFileSync, existsSync, statSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import path from 'node:path';
import { buildDtwWasm } from './p4-www/build-dtw-wasm.mjs';
import { readRecords, OP } from './p4-helpers.mjs';

const wdir = path.join(path.dirname(fileURLToPath(import.meta.url)), 'p4-www');
const wasmPath = path.join(wdir, 'dtw.wasm');
writeFileSync(wasmPath, buildDtwWasm());              // regenerate the served artifact

const diskBytes = readFileSync(wasmPath);             // exactly what Pages ships
console.log('dtw.wasm on disk:', diskBytes.length, 'bytes; validate:', WebAssembly.validate(diskBytes));

let framed = 0, lastFrame = [], mem;
const imports = { env: { key_state: () => 0, now_ms: () => 0, frame_out: (p, n) => { framed++; lastFrame = readRecords(mem, p, n); } } };
const { instance } = await WebAssembly.instantiate(diskBytes, imports);
mem = instance.exports.memory;
const hasExports = ['memory', 'init', 'step'].every((n) => n in instance.exports);
instance.exports.init();
instance.exports.step();
const ops = lastFrame.map((r) => r.name);
console.log('exports present [memory,init,step]:', hasExports);
console.log('served step() frame ops:', ops.join(','));
const frameOk = ops.includes('FILL_RECT') && ops.filter((o) => o === 'DRAW_IMAGE').length >= 2 && ops.includes('DRAW_TEXT') && ops.includes('PRESENT');

// bundle wiring
const html = readFileSync(path.join(wdir, 'index.html'), 'utf8');
const js = readFileSync(path.join(wdir, 'dtw.js'), 'utf8');
const wiring = {
  'index.html loads dtw.js': /src=["']dtw\.js["']/.test(html),
  'index.html has #screen canvas': /id=["']screen["']/.test(html),
  'dtw.js fetches dtw.wasm': /fetch\(['"]dtw\.wasm['"]\)/.test(js),
  'dtw.js supplies frame_out': /frame_out\s*:/.test(js),
  'dtw.js supplies key_state': /key_state\s*:/.test(js),
  'dtw.js supplies now_ms': /now_ms\s*:/.test(js),
  'dtw.js calls init+step': /\.init\(\)/.test(js) && /\.step\(\)/.test(js),
};
for (const [k, v] of Object.entries(wiring)) console.log(`  ${v ? 'ok ' : 'NO '} ${k}`);

console.log('bundle files:', ['index.html', 'dtw.js', 'dtw.wasm'].map((f) => `${f}=${statSync(path.join(wdir, f)).size}B`).join('  '));
const ok = WebAssembly.validate(diskBytes) && hasExports && framed === 2 && frameOk && Object.values(wiring).every(Boolean) && existsSync(wasmPath);
console.log('result:', ok ? 'OK' : 'FAIL');
process.exit(ok ? 0 : 1);
