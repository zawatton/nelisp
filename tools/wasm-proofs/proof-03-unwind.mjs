// Proof 3 — unwind-protect lowering via LEGACY EH: cleanup runs on BOTH the
// normal-exit and the non-local-exit (throw) paths.  Uses catch_all (0x19) +
// rethrow (0x09) for the exceptional path and a DUPLICATED cleanup on the
// normal-exit path (mirrors the native desugar which also emits cleanup on
// both).  A mutable i64 global $counter is bumped by cleanup; we assert it is
// exactly 1 in both modes.  Also exercises Tag(13) placed before Global(6).
import { section, functype, module, op, uleb, vec, I64, funcBody } from './wasm-build.mjs';

function nameExport(str, kind, idx) {
  const b = [...Buffer.from(str, 'utf8')];
  return [...uleb(b.length), ...b, kind, ...uleb(idx)];
}
// Types: 0 = [i64]->[i64] (run: mode->result) ; 1 = []->[] (tag, empty payload)
const typeSec = section(1, vec([functype([I64], [I64]), functype([], [])]));
const funcSec = section(3, vec([uleb(0)]));
const tagSec = section(13, vec([[0x00, ...uleb(1)]]));
// Global 0: mutable i64 counter, init 0
const globalSec = section(6, vec([[I64, 0x01, ...op.i64Const(0), ...op.end]]));
const expSec = section(7, [...uleb(2), ...nameExport('run', 0x00, 0), ...nameExport('counter', 0x03, 0)]);

const CLEANUP = [
  ...op.globalGet(0), ...op.i64Const(1), ...op.i64Add, ...op.globalSet(0),
];
const expr = [
  0x06, I64,                       // try (result i64)  [outer observer]
    0x06, I64,                     // try (result i64)  [unwind-protect]
      // BODY: mode==0 ? 7 : throw
      ...op.localGet(0), ...op.i64Eqz,
      ...op.if_(I64),
        ...op.i64Const(7),
      ...op.else_,
        ...op.throw_(0),           // throw unwind() -- empty payload
      ...op.end,
    0x19,                          // catch_all
      ...CLEANUP,
      0x09, ...uleb(0),            // rethrow 0
    ...op.end,                     // end unwind-protect try
    ...CLEANUP,                    // normal-exit cleanup (duplicated)
  0x19,                            // catch_all [outer]
    ...op.i64Const(-1),            // observed non-local exit
  ...op.end,                       // end outer try
  ...op.end,                       // end func
];
const codeSec = section(10, vec([funcBody([], expr)]));
const bytes = module([typeSec, funcSec, tagSec, globalSec, expSec, codeSec]);
console.log('validate:', WebAssembly.validate(bytes));

async function once(mode) {
  const { instance } = await WebAssembly.instantiate(bytes, {});
  const r = instance.exports.run(BigInt(mode));
  return { mode, result: r.toString(), counter: instance.exports.counter.value.toString() };
}
console.log('normal :', JSON.stringify(await once(0)), '(expect result=7 counter=1)');
console.log('throw  :', JSON.stringify(await once(1)), '(expect result=-1 counter=1)');
