// Proof p3-05 — the P3a allocator contract end-to-end: an inline bump
// allocator on the $heap_ptr global that (a) aligns up, (b) calls memory.grow
// when the bump crosses the current memory size, and (c) returns a pointer that
// is then WRITTEN and READ back through linear memory.  This is the exact shape
// `alloc-bytes` (Doc 164 §4.1) must lower to, proven before codex emits it.
//
// heap_ptr starts near the end of page 1 so the first allocation FORCES a grow
// and the store/load lands in the freshly grown page — proving alloc + grow +
// roundtrip together.
import { functype, module, op, vec, section, funcBody, I64 } from './wasm-build.mjs';
import { memSec, globalSec, exportSec, i32InitExpr, xop, KIND, I32 } from './p3-helpers.mjs';

const HEAP_START = 65530;                       // near end of page 1 (65536)
const typeSec = section(1, vec([
  functype([I32, I32], [I32]),                  // 0: alloc(size,align)->ptr
  functype([], [I64]),                          // 1: boot()->i64
]));
const funcSec = section(3, vec([[0], [1]]));
const mem = memSec(1, 8);                        // min 1, max 8 (grow is allowed)
const globals = globalSec([{ type: I32, mut: true, init: i32InitExpr(HEAP_START) }]);

// alloc(size, align): ptr = (heap_ptr + align-1) & -align ; heap_ptr = ptr+size
//   ; if heap_ptr > memory.size<<16 : memory.grow(1) ; return ptr
// locals: 2 = ptr (i32)
const alloc = funcBody([[1, I32]], [
  ...op.globalGet(0), ...op.localGet(1), ...xop.i32Add,      // hp + align
  ...op.i32Const(1), ...xop.i32Sub,                          // + align - 1
  ...op.i32Const(0), ...op.localGet(1), ...xop.i32Sub,       // -align  (== ~(align-1))
  ...xop.i32And, ...op.localTee(2),                          // ptr (saved)
  ...op.localGet(0), ...xop.i32Add, ...op.globalSet(0),      // heap_ptr = ptr + size
  // grow if heap_ptr > memory.size<<16
  ...op.globalGet(0), ...xop.memSize, ...op.i32Const(16), ...xop.i32Shl, ...xop.i32GtU,
  ...op.if_(),
  ...op.i32Const(1), ...xop.memGrow, ...op.drop,
  ...op.end,
  ...op.localGet(2), ...op.end,                              // return ptr
]);

// boot(): p = alloc(16,8) ; mem64[p] = 0x1122334455667788 ; return mem64[p]
// locals: 0 = p (i32)
const boot = funcBody([[1, I32]], [
  ...op.i32Const(16), ...op.i32Const(8), ...op.call(0), ...op.localSet(0),
  ...op.localGet(0), ...op.i64Const(0x1122334455667788n), ...op.i64Store(3, 0),
  ...op.localGet(0), ...op.i64Load(3, 0), ...op.end,
]);
const codeSec = section(10, vec([alloc, boot]));
const exports = exportSec([
  { name: 'memory', kind: KIND.mem, index: 0 },
  { name: 'heap_ptr', kind: KIND.global, index: 0 },
  { name: 'boot', kind: KIND.func, index: 1 },
]);

const bytes = module([typeSec, funcSec, mem, globals, exports, codeSec]);
console.log('validate:', WebAssembly.validate(bytes));
const { instance } = await WebAssembly.instantiate(bytes, {});
const v = instance.exports.boot();
const hp = instance.exports.heap_ptr.value;
console.log(`boot()=${v.toString(16)} (expect 1122334455667788)`);
console.log(`heap_ptr advanced ${HEAP_START} -> ${hp} (allocation happened + grew past page 1)`);
const ok = v === 0x1122334455667788n && hp > HEAP_START && hp >= 65536;
console.log('result:', ok ? 'OK' : 'FAIL');
if (!ok) process.exit(1);
