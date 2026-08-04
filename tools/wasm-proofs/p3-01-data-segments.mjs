// Proof p3-01 — active Data segments (id 11) initialize linear memory at
// instantiate, with MULTIPLE segments at distinct offsets.  This is the
// mechanism Doc 164 §4.2 relies on to copy the baked frozen-heap image into
// memory "for free": no memory.init, no start code — the engine copies the
// segment bytes during instantiation.  Proves segment offset encoding and that
// disjoint segments coexist.
import { functype, module, op, vec, section, funcBody, I64 } from './wasm-build.mjs';
import { memSec, dataSec, exportSec, KIND, le64, I32 } from './p3-helpers.mjs';

// Type 0: (i32) -> (i64)  — a memory reader keyed by address.
const typeSec = section(1, vec([functype([I32], [I64])]));
const funcSec = section(3, vec([[0]]));               // one func, type 0
const mem = memSec(1);                                 // min 1 page, no max

// Two disjoint active segments.
const SEG_A = 100;                                     // low
const SEG_B = 0x1000;                                  // 4096, page-aligned
const data = dataSec([
  { addr: SEG_A, bytes: le64(0x1122334455667788n) },
  { addr: SEG_B, bytes: le64(0x00000000deadbeefn) },
]);

// read64(addr) = i64.load [addr]
const body = funcBody([], [...op.localGet(0), ...op.i64Load(3, 0), ...op.end]);
const codeSec = section(10, vec([body]));
const exports = exportSec([
  { name: 'memory', kind: KIND.mem, index: 0 },
  { name: 'read64', kind: KIND.func, index: 0 },
]);

const bytes = module([typeSec, funcSec, mem, exports, codeSec, data]);
console.log('validate:', WebAssembly.validate(bytes));
const { instance } = await WebAssembly.instantiate(bytes, {});
const a = instance.exports.read64(SEG_A);
const b = instance.exports.read64(SEG_B);
console.log('seg A @100  =', a.toString(16), '(expect 1122334455667788)');
console.log('seg B @4096 =', b.toString(16), '(expect deadbeef)');
const ok = a === 0x1122334455667788n && b === 0xdeadbeefn;
console.log('result:', ok ? 'OK' : 'FAIL');
if (!ok) process.exit(1);
