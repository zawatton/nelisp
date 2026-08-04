// Proof p3-02 — memory.grow / memory.size + Memory min/max limits.
//   * memory.grow (0x40 0x00) returns the PREVIOUS size in pages, or -1 on
//     failure (declared max exceeded).
//   * memory.size (0x3f 0x00) returns the current page count.
//   * a store/load into the freshly grown region succeeds (the new pages are
//     zero-initialised and addressable).
// This backs the Doc 164 §4.1 allocator's "memory.grow if past current size".
import { functype, module, op, vec, section, funcBody, I64 } from './wasm-build.mjs';
import { memSec, exportSec, xop, KIND, I32 } from './p3-helpers.mjs';

// Types: 0:(i32)->(i32)  1:()->(i32)  2:(i32,i64)->()  3:(i32)->(i64)
const typeSec = section(1, vec([
  functype([I32], [I32]),
  functype([], [I32]),
  functype([I32, I64], []),
  functype([I32], [I64]),
]));
const funcSec = section(3, vec([[0], [1], [2], [3]]));  // grow,size,store,load
const mem = memSec(1, 4);                                // min 1, MAX 4 pages

const grow = funcBody([], [...op.localGet(0), ...xop.memGrow, ...op.end]);
const size = funcBody([], [...xop.memSize, ...op.end]);
const store = funcBody([], [...op.localGet(0), ...op.localGet(1), ...op.i64Store(3, 0), ...op.end]);
const load = funcBody([], [...op.localGet(0), ...op.i64Load(3, 0), ...op.end]);
const codeSec = section(10, vec([grow, size, store, load]));

const exports = exportSec([
  { name: 'memory', kind: KIND.mem, index: 0 },
  { name: 'grow', kind: KIND.func, index: 0 },
  { name: 'size', kind: KIND.func, index: 1 },
  { name: 'store', kind: KIND.func, index: 2 },
  { name: 'load', kind: KIND.func, index: 3 },
]);

const bytes = module([typeSec, funcSec, mem, exports, codeSec]);
console.log('validate:', WebAssembly.validate(bytes));
const { instance } = await WebAssembly.instantiate(bytes, {});
const e = instance.exports;

const size0 = e.size();
const prev = e.grow(2);            // 1 -> 3 pages, returns previous size 1
const size1 = e.size();
// 2.5 pages in = 0x28000 (163840) — only addressable AFTER the grow.
const ADDR = 0x28000;
e.store(ADDR, 0x0102030405060708n);
const back = e.load(ADDR);
const fail = e.grow(10);           // 3 + 10 = 13 > max 4  -> -1

console.log(`size0=${size0} grow(2)=${prev} size1=${size1}`);
console.log(`grown-store/load @${ADDR.toString(16)} = ${back.toString(16)} (expect 102030405060708)`);
console.log(`grow(10) past max = ${fail} (expect -1)`);
const ok = size0 === 1 && prev === 1 && size1 === 3 &&
  back === 0x0102030405060708n && fail === -1;
console.log('result:', ok ? 'OK' : 'FAIL');
if (!ok) process.exit(1);
