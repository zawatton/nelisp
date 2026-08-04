// Proof p3-03 — a MUTABLE i32 Global (id 6, the `$heap_ptr` bump pointer),
// updated from wasm code and read back from JS via the exported global.
//   * Global section byte layout: valtype 0x7f, mut 0x01, init-expr i32.const.
//   * global.get / global.set (0x23 / 0x24) mutate it.
//   * exporting the global (kind 0x03) lets the host observe heap_ptr, which is
//     how the P3 driver can assert the allocator advanced (Doc 164 §4.1).
import { functype, module, op, vec, section, funcBody } from './wasm-build.mjs';
import { memSec, globalSec, exportSec, i32InitExpr, xop, KIND, I32 } from './p3-helpers.mjs';

const HEAP_START = 66000;
const typeSec = section(1, vec([functype([I32], [I32])]));   // bump(n)->old
const funcSec = section(3, vec([[0]]));
const mem = memSec(2);                                        // 2 pages so 66000 is valid
// global 0 = mutable i32 heap_ptr = HEAP_START
const globals = globalSec([{ type: I32, mut: true, init: i32InitExpr(HEAP_START) }]);

// bump(n): old = heap_ptr; heap_ptr = old + n; return old
const bump = funcBody([[1, I32]], [
  ...op.globalGet(0), ...op.localSet(1),          // old = heap_ptr
  ...op.globalGet(0), ...op.localGet(0), ...xop.i32Add, ...op.globalSet(0), // heap_ptr += n
  ...op.localGet(1), ...op.end,                   // return old
]);
const codeSec = section(10, vec([bump]));
const exports = exportSec([
  { name: 'memory', kind: KIND.mem, index: 0 },
  { name: 'heap_ptr', kind: KIND.global, index: 0 },
  { name: 'bump', kind: KIND.func, index: 0 },
]);

const bytes = module([typeSec, funcSec, mem, globals, exports, codeSec]);
console.log('validate:', WebAssembly.validate(bytes));
const { instance } = await WebAssembly.instantiate(bytes, {});
const e = instance.exports;

const p0 = e.bump(16);            // 66000, heap_ptr -> 66016
const p1 = e.bump(32);            // 66016, heap_ptr -> 66048
const observed = e.heap_ptr.value;
console.log(`bump(16)=${p0} bump(32)=${p1} heap_ptr=${observed} (expect 66000,66016,66048)`);
const ok = p0 === 66000 && p1 === 66016 && observed === 66048;
console.log('result:', ok ? 'OK' : 'FAIL');
if (!ok) process.exit(1);
