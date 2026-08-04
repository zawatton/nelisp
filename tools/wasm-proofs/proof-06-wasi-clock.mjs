// Proof 6 — node:wasi clock_time_get.  Reactor module importing
// wasi_snapshot_preview1.clock_time_get(clock_id:i32, precision:i64,
// ts_ptr:i32)->errno:i32.  getClock() calls it (CLOCK_REALTIME=0), stashes the
// errno at a fixed address, and returns the u64 nanosecond timestamp loaded
// from a fixed address.  Asserts errno==0 and timestamp!=0 (determinism note:
// P2 tests that need a stable value inject a fixed clock from the JS driver).
import { WASI } from 'node:wasi';
import { section, functype, module, op, uleb, vec, I32, I64, funcBody } from './wasm-build.mjs';

const TS_ADDR = 1024, ERRNO_ADDR = 1040;
function importEntry(mod, field, kind, idx) {
  const m = [...Buffer.from(mod, 'utf8')], f = [...Buffer.from(field, 'utf8')];
  return [...uleb(m.length), ...m, ...uleb(f.length), ...f, kind, ...uleb(idx)];
}
function nameExport(str, kind, idx) {
  const b = [...Buffer.from(str, 'utf8')];
  return [...uleb(b.length), ...b, kind, ...uleb(idx)];
}
// Types: 0=clock_time_get [i32,i64,i32]->[i32]; 1=getClock []->[i64]; 2=init []->[]
const typeSec = section(1, vec([
  functype([I32, I64, I32], [I32]),
  functype([], [I64]),
  functype([], []),
]));
const importSec = section(2, vec([importEntry('wasi_snapshot_preview1', 'clock_time_get', 0x00, 0)]));
const funcSec = section(3, vec([uleb(1), uleb(2)]));
const memSec = section(5, [...uleb(1), 0x00, ...uleb(1)]);
const expSec = section(7, [...uleb(3),
  ...nameExport('memory', 0x02, 0),
  ...nameExport('_initialize', 0x00, 2),
  ...nameExport('getClock', 0x00, 1),
]);
const getClock = funcBody([[1, I32]], [ // local0: i32 errno scratch
  ...op.i32Const(0),         // CLOCK_REALTIME
  ...op.i64Const(0),         // precision
  ...op.i32Const(TS_ADDR),
  ...op.call(0),             // -> errno
  ...op.localSet(0),
  ...op.i32Const(ERRNO_ADDR), ...op.localGet(0), ...op.i32Store(),
  ...op.i32Const(TS_ADDR), ...op.i64Load(),
  ...op.end,
]);
const initFn = funcBody([], [...op.end]);
const codeSec = section(10, vec([getClock, initFn]));
const bytes = module([typeSec, importSec, funcSec, memSec, expSec, codeSec]);

console.log('validate:', WebAssembly.validate(bytes));
const wasi = new WASI({ version: 'preview1', args: [], env: {}, preopens: {} });
const { instance } = await WebAssembly.instantiate(bytes, wasi.getImportObject());
wasi.initialize(instance);
const ts = instance.exports.getClock();
const errno = new DataView(instance.exports.memory.buffer).getUint32(ERRNO_ADDR, true);
console.log(`clock_time_get errno=${errno} ts=${ts} nonzero=${ts !== 0n} (expect errno=0 nonzero=true)`);
