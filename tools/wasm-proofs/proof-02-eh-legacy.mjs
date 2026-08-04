// Proof 2 — LEGACY EH (flag-free on Node 24.14.1) full mechanics:
//   * tag type [i64 i64]->[] carrying (tag_handle, value),
//   * throw (0x08), try (0x06) bt, catch (0x07) tag, catch_all (0x19), end,
//   * nested catch with tag-match + RETHROW to the enclosing handler
//     (proves throw_ref/exnref NOT needed: re-throw a fresh (tag,value)),
//   * catch/throw parity model: (catch 'outer (catch 'inner (throw T V))).
import { section, functype, module, op, uleb, vec, I64, funcBody } from './wasm-build.mjs';

const INNER = 11, OUTER = 22;
function nameExport(str, idx) {
  const b = [...Buffer.from(str, 'utf8')];
  return [...uleb(b.length), ...b, 0x00, ...uleb(idx)];
}

// Types: 0 = [i64 i64]->[i64] (run: throwTag, throwVal -> result)
//        1 = [i64 i64]->[]    (the unwind tag)
const typeSec = section(1, vec([functype([I64, I64], [I64]), functype([I64, I64], [])]));
const funcSec = section(3, vec([uleb(0)]));
const tagSec = section(13, vec([[0x00, ...uleb(1)]]));
const expSec = section(7, [...uleb(1), ...nameExport('run', 0)]);

// params: 0=throwTag 1=throwVal ; locals: 2=tag 3=val
const TAG = 2, VAL = 3;
const handlerMatch = (matchConst) => [
  // stack on catch entry: <tag> <val>
  ...op.localSet(VAL),
  ...op.localSet(TAG),
  ...op.localGet(TAG),
  ...op.i64Const(matchConst),
  ...op.i64Eq,
  ...op.if_(I64),
    ...op.localGet(VAL),          // matched: yield value
  ...op.else_,
    ...op.localGet(TAG),          // mismatch: rethrow (tag,val)
    ...op.localGet(VAL),
    ...op.throw_(0),
  ...op.end,
];

const expr = [
  0x06, I64,                       // try (result i64)  [outer]
    0x06, I64,                     // try (result i64)  [inner]
      ...op.localGet(0),           // throwTag
      ...op.localGet(1),           // throwVal
      ...op.throw_(0),             // throw unwind(tag,val)
    0x07, ...uleb(0),              // catch unwind  [inner handler]
      ...handlerMatch(INNER),
    ...op.end,                     // end inner try
  0x07, ...uleb(0),                // catch unwind  [outer handler]
    ...handlerMatch(OUTER),
  ...op.end,                       // end outer try
  ...op.end,                       // end func
];
const codeSec = section(10, vec([funcBody([[2, I64]], expr)]));
const bytes = module([typeSec, funcSec, tagSec, expSec, codeSec]);

console.log('validate:', WebAssembly.validate(bytes));
const { instance } = await WebAssembly.instantiate(bytes, {});
const run = instance.exports.run;
// throw to inner -> inner catches, returns value 42
console.log('throw INNER 42 ->', run(BigInt(INNER), 42n).toString(), '(expect 42)');
// throw to outer -> inner rethrows, outer catches, returns 99
console.log('throw OUTER 99 ->', run(BigInt(OUTER), 99n).toString(), '(expect 99)');
