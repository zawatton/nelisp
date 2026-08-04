// Proof p3-06 — the P3b/P3c capstone: a baked "frozen heap" in a Data segment,
// a reduced tree-walk evaluator that boots from it, and an exported entry that
// returns 3 for the baked form (+ 1 2).  This is the mechanism behind the Doc
// 164 §6 P3 exit gate, proven end-to-end before codex writes any elisp.
//
// It exercises, together:
//   * Data-segment copy of a baked object graph (no re-parse, no reader).
//   * POINTER RELOCATION: the root node stores an ABSOLUTE linear-memory offset
//     to its args cell (args_ptr = B+16), computed here at "emit" time — the
//     wasm analog of the ELF `abs64` reloc the system linker would fill.  wasm
//     linear memory is based at 0 and never slides, so the baked offset is final
//     (no runtime relocation pass).
//   * Boot-time allocation via the $heap_ptr bump allocator (Mode 1: the result
//     cell is materialised in the heap), so the allocator is genuinely on the
//     boot path even though fixnums are immediate.
//   * An exported `_start` returning the i64 result 3.
import { functype, module, op, vec, section, funcBody, I64 } from './wasm-build.mjs';
import { memSec, globalSec, exportSec, i32InitExpr, dataSec, xop, le32, le64, KIND, I32 } from './p3-helpers.mjs';

const B = 4096;               // Data-segment base (page 1); heap lives in page 2
const ARGS = B + 16;          // args cell address (the baked relocation target)
const HEAP_START = 65536;     // page 2 start

// Baked frozen heap for (+ 1 2):
//   B+0  : i32 optag = 1 ("add")      B+4  : i32 pad = 0
//   B+8  : i64 args_ptr = ARGS        <-- ABSOLUTE offset (relocation)
//   B+16 : i64 operand0 = 1           B+24 : i64 operand1 = 2
const heapImage = [
  ...le32(1), ...le32(0),
  ...le64(ARGS),
  ...le64(1), ...le64(2),
];

const typeSec = section(1, vec([functype([], [I64])]));   // _start()->i64
const funcSec = section(3, vec([[0]]));
const mem = memSec(2, 8);
const globals = globalSec([{ type: I32, mut: true, init: i32InitExpr(HEAP_START) }]);
const data = dataSec([{ addr: B, bytes: heapImage }]);

// _start(): tree-walk eval of the baked node at B.
//   locals: 0 = aptr(i32)  1 = resultptr(i32)
const start = funcBody([[2, I32]], [
  // dispatch on optag
  ...op.i32Const(B), ...op.i32Load(2, 0), ...op.i32Const(1), ...xop.i32Eq,
  ...op.if_(I64),
  // aptr = wrap(i64.load[B+8])   (follow the relocated absolute pointer)
  ...op.i32Const(B + 8), ...op.i64Load(3, 0), ...op.i32WrapI64, ...op.localSet(0),
  // resultptr = align8(heap_ptr) ; heap_ptr = resultptr + 8   (bump allocator)
  ...op.globalGet(0), ...op.i32Const(7), ...xop.i32Add, ...op.i32Const(-8), ...xop.i32And,
  ...op.localTee(1),
  ...op.i32Const(8), ...xop.i32Add, ...op.globalSet(0),
  // mem64[resultptr] = operand0 + operand1
  ...op.localGet(1),
  ...op.localGet(0), ...op.i64Load(3, 0),
  ...op.localGet(0), ...op.i64Load(3, 8),
  ...op.i64Add,
  ...op.i64Store(3, 0),
  // return mem64[resultptr]
  ...op.localGet(1), ...op.i64Load(3, 0),
  ...op.else_,
  ...op.i64Const(-1),
  ...op.end,
  ...op.end,
]);
const codeSec = section(10, vec([start]));
const exports = exportSec([
  { name: 'memory', kind: KIND.mem, index: 0 },
  { name: 'heap_ptr', kind: KIND.global, index: 0 },
  { name: '_start', kind: KIND.func, index: 0 },
]);

const bytes = module([typeSec, funcSec, mem, globals, exports, codeSec, data]);
console.log('validate:', WebAssembly.validate(bytes));
const { instance } = await WebAssembly.instantiate(bytes, {});
const v = instance.exports._start();
const hp = instance.exports.heap_ptr.value;
console.log(`_start() eval (+ 1 2) = ${v} (expect 3)`);
console.log(`heap_ptr ${HEAP_START} -> ${hp} (allocator ran during boot)`);
const ok = v === 3n && hp === HEAP_START + 8;
console.log('result:', ok ? 'OK' : 'FAIL');
if (!ok) process.exit(1);
