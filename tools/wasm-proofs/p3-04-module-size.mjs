// Proof p3-04 — module-size sanity for the FULL-corpus feasibility question.
// If the P3 gate ever bakes the whole standalone reader+evaluator+stdlib, the
// module gains thousands of functions and a multi-MB Data segment.  This proof
// synthesises (in JS, NOT via the Elisp encoder) a module with N functions and
// a ~M-MB active Data segment, and confirms Node v24.14.1 / V8 validate +
// instantiate it.  V8's documented ceilings are far higher (functions up to
// 1,000,000; a single module up to ~2 GB via the JS API), so this establishes
// that neither the function count nor the data segment is the P3 blocker.
import { uleb, sleb, section, vec, module, op, functype, funcBody, I64 } from './wasm-build.mjs';
import { memSec, dataSec, exportSec, KIND, I32 } from './p3-helpers.mjs';

const N_FUNCS = Number(process.env.P3_NFUNCS || 20000);
const DATA_MB = Number(process.env.P3_DATA_MB || 4);

const t0 = Date.now();
// One shared type ()->(i64); every function returns its own index constant.
const typeSec = section(1, vec([functype([], [I64])]));
const funcDecls = [];
const bodies = [];
for (let i = 0; i < N_FUNCS; i += 1) {
  funcDecls.push([0]);
  bodies.push(funcBody([], [...op.i64Const(i), ...op.end]));
}
const funcSec = section(3, vec(funcDecls));
const codeSec = section(10, vec(bodies));

const dataBytes = new Array(DATA_MB * 1024 * 1024).fill(0x5a);
const mem = memSec(Math.ceil((0x1000 + dataBytes.length) / 65536));
const data = dataSec([{ addr: 0x1000, bytes: dataBytes }]);

// Export just the first + last function so we can call across the whole range.
const exports = exportSec([
  { name: 'memory', kind: KIND.mem, index: 0 },
  { name: 'first', kind: KIND.func, index: 0 },
  { name: 'last', kind: KIND.func, index: N_FUNCS - 1 },
]);

const bytes = module([typeSec, funcSec, mem, exports, codeSec, data]);
const built = Date.now();
console.log(`built module: funcs=${N_FUNCS} data=${DATA_MB}MB size=${(bytes.length / 1048576).toFixed(2)}MB in ${built - t0}ms`);
const valid = WebAssembly.validate(bytes);
console.log('validate:', valid);
const { instance } = await WebAssembly.instantiate(bytes, {});
const first = instance.exports.first();
const last = instance.exports.last();
console.log(`instantiate ok; first()=${first} last()=${last} (expect 0, ${N_FUNCS - 1}) in ${Date.now() - built}ms`);
const ok = valid && first === 0n && last === BigInt(N_FUNCS - 1);
console.log('result:', ok ? 'OK' : 'FAIL');
if (!ok) process.exit(1);
