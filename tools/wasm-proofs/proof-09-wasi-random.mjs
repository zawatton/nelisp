// Proof 9 — node:wasi random_get: fills a fixed buffer with entropy.
// random_get(buf:i32, len:i32)->errno:i32.  Reactor module; getRand() fills 8
// bytes at a fixed address and returns the loaded i64.  Asserts errno==0 and a
// nonzero draw (determinism note: P2 tests wanting a fixed value inject a
// stub random_get from the driver).
import { WASI } from 'node:wasi';
import { section, functype, module, op, uleb, vec, I32, I64, funcBody } from './wasm-build.mjs';

const BUF = 1024, ERRNO = 1040;
function imp(mod, field, idx) {
  const m = [...Buffer.from(mod, 'utf8')], f = [...Buffer.from(field, 'utf8')];
  return [...uleb(m.length), ...m, ...uleb(f.length), ...f, 0x00, ...uleb(idx)];
}
function nameExport(str, kind, idx) {
  const b = [...Buffer.from(str, 'utf8')];
  return [...uleb(b.length), ...b, kind, ...uleb(idx)];
}
// types: 0=random_get[i32,i32]->[i32]; 1=getRand[]->[i64]; 2=init[]->[]
const typeSec = section(1, vec([functype([I32, I32], [I32]), functype([], [I64]), functype([], [])]));
const importSec = section(2, vec([imp('wasi_snapshot_preview1', 'random_get', 0)]));
const funcSec = section(3, vec([uleb(1), uleb(2)]));
const memSec = section(5, [...uleb(1), 0x00, ...uleb(1)]);
const expSec = section(7, [...uleb(3),
  ...nameExport('memory', 0x02, 0),
  ...nameExport('_initialize', 0x00, 2),
  ...nameExport('getRand', 0x00, 1),
]);
const getRand = funcBody([[1, I32]], [
  ...op.i32Const(BUF), ...op.i32Const(8), ...op.call(0), ...op.localSet(0),
  ...op.i32Const(ERRNO), ...op.localGet(0), ...op.i32Store(),
  ...op.i32Const(BUF), ...op.i64Load(),
  ...op.end,
]);
const initFn = funcBody([], [...op.end]);
const codeSec = section(10, vec([getRand, initFn]));
const bytes = module([typeSec, importSec, funcSec, memSec, expSec, codeSec]);

console.log('validate:', WebAssembly.validate(bytes));
const wasi = new WASI({ version: 'preview1', args: [], env: {}, preopens: {} });
const { instance } = await WebAssembly.instantiate(bytes, wasi.getImportObject());
wasi.initialize(instance);
const draw = instance.exports.getRand();
const errno = new DataView(instance.exports.memory.buffer).getUint32(ERRNO, true);
console.log(`random_get errno=${errno} draw=${draw} nonzero=${draw !== 0n} (expect errno=0 nonzero=true)`);
