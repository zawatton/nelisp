// Proof 5 — node:wasi fd_write.  Hand-built REACTOR module importing
// wasi_snapshot_preview1.fd_write, writing "hi\n" to stdout (fd 1) through an
// iovec at a FIXED linear-memory address (no allocator), baked via active Data
// segments.  Proves: import of a WASI symbol, the fd_write ABI
// (fd, iovs_ptr, iovs_cnt, nwritten_ptr)->errno, a fixed iov/scratch layout,
// and the reactor (_initialize) instantiation path.  Asserts errno==0,
// nwritten==3, and emits "hi\n" on stdout.
import { WASI } from 'node:wasi';
import { section, functype, module, op, uleb, vec, I32, funcBody } from './wasm-build.mjs';

const STR_ADDR = 1024;      // "hi\n"
const IOV_ADDR = 1032;      // struct iovec { i32 buf; i32 len; }
const NWRITTEN_ADDR = 1040; // fd_write writes the byte count here

function importEntry(mod, field, kind, idx) {
  const m = [...Buffer.from(mod, 'utf8')], f = [...Buffer.from(field, 'utf8')];
  return [...uleb(m.length), ...m, ...uleb(f.length), ...f, kind, ...uleb(idx)];
}
function nameExport(str, kind, idx) {
  const b = [...Buffer.from(str, 'utf8')];
  return [...uleb(b.length), ...b, kind, ...uleb(idx)];
}
function activeData(addr, bytes) {
  return [0x00, ...op.i32Const(addr), ...op.end, ...uleb(bytes.length), ...bytes];
}
function le32(n) { return [n & 0xff, (n >>> 8) & 0xff, (n >>> 16) & 0xff, (n >>> 24) & 0xff]; }

// Types: 0=fd_write [i32*4]->[i32] ; 1=writeHi []->[i32] ; 2=_initialize []->[]
const typeSec = section(1, vec([
  functype([I32, I32, I32, I32], [I32]),
  functype([], [I32]),
  functype([], []),
]));
const importSec = section(2, vec([importEntry('wasi_snapshot_preview1', 'fd_write', 0x00, 0)]));
// defined funcs: writeHi(idx1,type1), _initialize(idx2,type2)
const funcSec = section(3, vec([uleb(1), uleb(2)]));
const memSec = section(5, [...uleb(1), 0x00, ...uleb(1)]); // 1 memory, min 1 page
const expSec = section(7, [...uleb(3),
  ...nameExport('memory', 0x02, 0),
  ...nameExport('_initialize', 0x00, 2),
  ...nameExport('writeHi', 0x00, 1),
]);
const writeHi = funcBody([], [
  ...op.i32Const(1),            // fd = stdout
  ...op.i32Const(IOV_ADDR),     // iovs
  ...op.i32Const(1),            // iovs_len
  ...op.i32Const(NWRITTEN_ADDR),
  ...op.call(0),                // fd_write -> errno
  ...op.end,
]);
const initFn = funcBody([], [...op.end]);
const codeSec = section(10, vec([writeHi, initFn]));
const dataSec = section(11, vec([
  activeData(STR_ADDR, [...Buffer.from('hi\n', 'utf8')]),
  activeData(IOV_ADDR, [...le32(STR_ADDR), ...le32(3)]),
]));
const bytes = module([typeSec, importSec, funcSec, memSec, expSec, codeSec, dataSec]);

console.log('validate:', WebAssembly.validate(bytes));
const wasi = new WASI({ version: 'preview1', args: ['hello'], env: {}, preopens: {} });
const { instance } = await WebAssembly.instantiate(bytes, wasi.getImportObject());
wasi.initialize(instance);
const errno = instance.exports.writeHi();
const mem = new DataView(instance.exports.memory.buffer);
const nwritten = mem.getUint32(NWRITTEN_ADDR, true);
console.log(`fd_write errno=${errno} nwritten=${nwritten} (expect errno=0 nwritten=3)`);
